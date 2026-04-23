#include "ParticleScene.h"

#include "AssetPath.h"
#include "Dx12Helpers.h"
#include "ObjLoader.h"

#include <algorithm>
#include <cstring>
#include <d3dcompiler.h>
#include <stdexcept>
#include <vector>

using namespace DirectX;
using Microsoft::WRL::ComPtr;

namespace
{
    constexpr uint32_t kThreadGroupSize = 256;
    constexpr uint32_t kDispatchGroups = ParticleScene::MaxParticles / kThreadGroupSize;
    constexpr float kMaxDeltaTime = 1.f / 30.f;
    constexpr float kEmissionRate = 64.f;
    constexpr uint32_t kMaxEmitPerFrame = 24;
    constexpr float kTargetBunnyHeight = 31.f;
    constexpr float kFloorHalfExtent = 70.f;
    constexpr float kEmitterHeightFactor = 0.85f;
    constexpr float kEmitterHeightOffset = 0.9f;
    constexpr float kParticleSpawnRadius = 1.f;
    constexpr float kParticleInitialRise = 4.8f;
    constexpr float kParticleVelocityJitter = 4.0f;
    constexpr float kParticleGravity = -7.8f;
    constexpr float kParticleBaseSize = 0.26f;
    constexpr float kFloorCheckerTileSize = 14.f;
    const XMFLOAT4 kSunsetLightDirection{-0.28f, -1.f, -0.42f, 0.f};
    const XMFLOAT4 kSunsetLightColor{0.98f, 0.70f, 0.46f, 1.f};
    const XMFLOAT4 kSunsetAmbientColor{0.14f, 0.17f, 0.27f, 1.f};
    constexpr float kSunsetClearColor[4] = {0.18f, 0.22f, 0.33f, 1.f};
    static_assert(
        ParticleScene::MaxParticles % kThreadGroupSize == 0,
        "ParticleScene::MaxParticles must be divisible by the compute thread group size.");

    enum DescriptorIndex : uint32_t
    {
        DescriptorParticle0Srv = 0,
        DescriptorParticle1Srv = 1,
        DescriptorLiveCountSrv = 2,
        DescriptorParticle0Uav = 3,
        DescriptorParticle1Uav = 4,
        DescriptorCount = 5,
    };

    enum GraphicsRootParam : uint32_t
    {
        GraphicsRootSceneCb = 0,
        GraphicsRootDrawConstants = 1,
        GraphicsRootParticlesSrv = 2,
        GraphicsRootCountSrv = 3,
    };

    enum ComputeRootParam : uint32_t
    {
        ComputeRootConstants = 0,
        ComputeRootCountSrv = 1,
        ComputeRootCurrentParticlesUav = 2,
        ComputeRootNextParticlesUav = 3,
    };

    constexpr D3D12_INPUT_ELEMENT_DESC kMeshLayout[] = {
        {"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"NORMAL", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 24, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"TANGENT", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 32, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
    };

    D3D12_RESOURCE_DESC MakeBufferDesc(UINT64 size, D3D12_RESOURCE_FLAGS flags = D3D12_RESOURCE_FLAG_NONE)
    {
        D3D12_RESOURCE_DESC desc = dx12::BufferDesc(size);
        desc.Flags = flags;
        return desc;
    }

    D3D12_CPU_DESCRIPTOR_HANDLE OffsetCpuHandle(
        D3D12_CPU_DESCRIPTOR_HANDLE base,
        uint32_t stride,
        uint32_t index)
    {
        D3D12_CPU_DESCRIPTOR_HANDLE handle = base;
        handle.ptr += static_cast<SIZE_T>(stride) * index;
        return handle;
    }

    D3D12_GPU_DESCRIPTOR_HANDLE OffsetGpuHandle(
        D3D12_GPU_DESCRIPTOR_HANDLE base,
        uint32_t stride,
        uint32_t index)
    {
        D3D12_GPU_DESCRIPTOR_HANDLE handle = base;
        handle.ptr += static_cast<UINT64>(stride) * index;
        return handle;
    }

    void TransitionResource(
        ID3D12GraphicsCommandList* cmdList,
        ID3D12Resource* resource,
        D3D12_RESOURCE_STATES& currentState,
        D3D12_RESOURCE_STATES newState)
    {
        if (!resource || currentState == newState)
            return;

        D3D12_RESOURCE_BARRIER barrier{};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Transition.pResource = resource;
        barrier.Transition.StateBefore = currentState;
        barrier.Transition.StateAfter = newState;
        barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &barrier);
        currentState = newState;
    }

    void UavBarrier(ID3D12GraphicsCommandList* cmdList, ID3D12Resource* resource)
    {
        if (!resource)
            return;

        D3D12_RESOURCE_BARRIER barrier{};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_UAV;
        barrier.UAV.pResource = resource;
        cmdList->ResourceBarrier(1, &barrier);
    }

    float ClampDeltaTime(float dt)
    {
        return std::clamp(dt, 0.f, kMaxDeltaTime);
    }

    bool CreateRootSignature(
        ID3D12Device* device,
        const D3D12_ROOT_SIGNATURE_DESC& desc,
        ComPtr<ID3D12RootSignature>& rootSignature)
    {
        ComPtr<ID3DBlob> blob;
        ComPtr<ID3DBlob> errors;
        HRESULT hr = D3D12SerializeRootSignature(
            &desc,
            D3D_ROOT_SIGNATURE_VERSION_1,
            blob.GetAddressOf(),
            errors.GetAddressOf());
        if (FAILED(hr))
        {
            if (errors)
                throw std::runtime_error(static_cast<const char*>(errors->GetBufferPointer()));
            return false;
        }

        return SUCCEEDED(device->CreateRootSignature(
            0,
            blob->GetBufferPointer(),
            blob->GetBufferSize(),
            IID_PPV_ARGS(&rootSignature)));
    }

    MeshData BuildFloorMesh()
    {
        MeshData mesh;
        mesh.Vertices = {
            {{-kFloorHalfExtent, 0.f, -kFloorHalfExtent}, {0.f, 1.f, 0.f}, {0.f, 1.f}, {1.f, 0.f, 0.f}},
            {{-kFloorHalfExtent, 0.f,  kFloorHalfExtent}, {0.f, 1.f, 0.f}, {0.f, 0.f}, {1.f, 0.f, 0.f}},
            {{ kFloorHalfExtent, 0.f,  kFloorHalfExtent}, {0.f, 1.f, 0.f}, {1.f, 0.f}, {1.f, 0.f, 0.f}},
            {{ kFloorHalfExtent, 0.f, -kFloorHalfExtent}, {0.f, 1.f, 0.f}, {1.f, 1.f}, {1.f, 0.f, 0.f}},
        };
        mesh.Indices = {0, 1, 2, 0, 2, 3};

        SubMesh subMesh;
        subMesh.IndexStart = 0;
        subMesh.IndexCount = static_cast<uint32_t>(mesh.Indices.size());
        subMesh.Material.Kd = {0.22f, 0.08f, 0.11f};
        mesh.SubMeshes.push_back(subMesh);

        mesh.BoundsMin = {-kFloorHalfExtent, 0.f, -kFloorHalfExtent};
        mesh.BoundsMax = {kFloorHalfExtent, 0.f, kFloorHalfExtent};
        return mesh;
    }

    XMFLOAT4 MakeBaseColor(const XMFLOAT3& color, const XMFLOAT4& fallback = XMFLOAT4{1.f, 1.f, 1.f, 1.f})
    {
        const float luminance = color.x + color.y + color.z;
        if (luminance <= 0.0001f)
            return fallback;

        return {
            std::clamp(color.x, 0.02f, 1.f),
            std::clamp(color.y, 0.02f, 1.f),
            std::clamp(color.z, 0.02f, 1.f),
            1.f};
    }
}

bool ParticleScene::Initialize(ID3D12Device* device, ID3D12CommandQueue* cmdQueue,
                               DXGI_FORMAT backBufferFmt, uint32_t width, uint32_t height)
{
    m_currentBufferIndex = 0;
    m_time = 0.f;
    m_emitAccumulator = 0.f;
    XMStoreFloat4x4(&m_bunnyWorld, XMMatrixIdentity());
    XMStoreFloat4x4(&m_floorWorld, XMMatrixIdentity());

    ComPtr<ID3D12CommandAllocator> uploadAlloc;
    ComPtr<ID3D12GraphicsCommandList> uploadList;
    dx12::ThrowIfFailed(
        device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&uploadAlloc)),
        "particle upload allocator");
    dx12::ThrowIfFailed(
        device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, uploadAlloc.Get(), nullptr, IID_PPV_ARGS(&uploadList)),
        "particle upload list");

    std::vector<ComPtr<ID3D12Resource>> uploads;

    if (!BuildDescriptorHeap(device)) return false;
    if (!BuildParticleBuffers(device, uploadList.Get())) return false;
    if (!BuildGeometry(device, uploadList.Get(), uploads)) return false;
    if (!BuildConstantBuffer(device)) return false;
    if (!BuildRootSignatures(device)) return false;
    if (!BuildShaders()) return false;
    if (!BuildPipelineStates(device, backBufferFmt)) return false;
    if (!BuildDepthBuffer(device, width, height)) return false;

    dx12::ExecuteAndWait(device, cmdQueue, uploadList.Get());
    return true;
}

void ParticleScene::Shutdown()
{
    if (m_mappedSceneCB)
    {
        m_sceneCB->Unmap(0, nullptr);
        m_mappedSceneCB = nullptr;
    }

    m_dsvHeap.Reset();
    m_depthBuffer.Reset();
    m_zeroUpload.Reset();
    m_sceneCB.Reset();
    m_liveCountBuffer.Reset();
    for (BufferWithCounter& buffer : m_particleBuffers)
    {
        buffer.Buffer.Reset();
        buffer.Counter.Reset();
    }

    auto resetMesh = [](MeshGpu& mesh) {
        mesh.VertexBuffer.Reset();
        mesh.IndexBuffer.Reset();
        mesh.Vbv = {};
        mesh.Ibv = {};
        mesh.Draws.clear();
    };
    resetMesh(m_bunnyMesh);
    resetMesh(m_floorMesh);

    m_descriptorHeap.Reset();
    m_meshVs.Reset();
    m_meshPs.Reset();
    m_particleVs.Reset();
    m_particleGs.Reset();
    m_particlePs.Reset();
    m_updateCs.Reset();
    m_meshPso.Reset();
    m_particlePso.Reset();
    m_updatePso.Reset();
    m_graphicsRootSig.Reset();
    m_computeRootSig.Reset();
}

void ParticleScene::OnResize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!width || !height)
        return;

    m_depthBuffer.Reset();
    BuildDepthBuffer(device, width, height);
}

void ParticleScene::RecordCommands(ID3D12GraphicsCommandList* cmdList,
                                   const XMFLOAT4X4& view,
                                   const XMFLOAT4X4& proj,
                                   D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                                   D3D12_VIEWPORT viewport,
                                   D3D12_RECT scissorRect,
                                   float dt)
{
    UpdateSceneConstants(view, proj);
    UpdateParticles(cmdList, dt);
    RenderScene(cmdList, backBufferRtv, viewport, scissorRect);
}

bool ParticleScene::BuildShaders()
{
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif

    std::wstring path = ToWide(ResolveAsset("Shaders/ParticleScene.hlsl"));
    ComPtr<ID3DBlob> errors;

    auto compile = [&](const char* entry, const char* target, ComPtr<ID3DBlob>& blob) -> bool {
        errors.Reset();
        HRESULT hr = D3DCompileFromFile(
            path.c_str(),
            nullptr,
            D3D_COMPILE_STANDARD_FILE_INCLUDE,
            entry,
            target,
            flags,
            0,
            blob.GetAddressOf(),
            errors.GetAddressOf());
        if (FAILED(hr))
        {
            if (errors)
                throw std::runtime_error(static_cast<const char*>(errors->GetBufferPointer()));
            return false;
        }
        return true;
    };

    return compile("MeshVS", "vs_5_0", m_meshVs)
        && compile("MeshPS", "ps_5_0", m_meshPs)
        && compile("ParticleVS", "vs_5_0", m_particleVs)
        && compile("ParticleGS", "gs_5_0", m_particleGs)
        && compile("ParticlePS", "ps_5_0", m_particlePs)
        && compile("UpdateParticlesCS", "cs_5_0", m_updateCs);
}

bool ParticleScene::BuildDescriptorHeap(ID3D12Device* device)
{
    m_descriptorStride = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

    D3D12_DESCRIPTOR_HEAP_DESC desc{};
    desc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    desc.NumDescriptors = DescriptorCount;
    desc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    return SUCCEEDED(device->CreateDescriptorHeap(&desc, IID_PPV_ARGS(&m_descriptorHeap)));
}

bool ParticleScene::BuildParticleBuffers(ID3D12Device* device, ID3D12GraphicsCommandList* uploadList)
{
    const auto defaultHeap = dx12::HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
    const auto uploadHeap = dx12::HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
    const D3D12_CPU_DESCRIPTOR_HANDLE cpuBase = m_descriptorHeap->GetCPUDescriptorHandleForHeapStart();
    const D3D12_GPU_DESCRIPTOR_HANDLE gpuBase = m_descriptorHeap->GetGPUDescriptorHandleForHeapStart();

    const D3D12_RESOURCE_DESC zeroDesc = MakeBufferDesc(sizeof(uint32_t));
    if (FAILED(device->CreateCommittedResource(
            &uploadHeap,
            D3D12_HEAP_FLAG_NONE,
            &zeroDesc,
            D3D12_RESOURCE_STATE_GENERIC_READ,
            nullptr,
            IID_PPV_ARGS(&m_zeroUpload))))
    {
        return false;
    }

    void* mapped = nullptr;
    D3D12_RANGE readRange{0, 0};
    dx12::ThrowIfFailed(m_zeroUpload->Map(0, &readRange, &mapped), "Map particle zero upload");
    std::memset(mapped, 0, sizeof(uint32_t));
    m_zeroUpload->Unmap(0, nullptr);

    D3D12_SHADER_RESOURCE_VIEW_DESC particleSrv{};
    particleSrv.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    particleSrv.ViewDimension = D3D12_SRV_DIMENSION_BUFFER;
    particleSrv.Format = DXGI_FORMAT_UNKNOWN;
    particleSrv.Buffer.NumElements = MaxParticles;
    particleSrv.Buffer.StructureByteStride = sizeof(ParticleData);

    D3D12_UNORDERED_ACCESS_VIEW_DESC particleUav{};
    particleUav.ViewDimension = D3D12_UAV_DIMENSION_BUFFER;
    particleUav.Format = DXGI_FORMAT_UNKNOWN;
    particleUav.Buffer.NumElements = MaxParticles;
    particleUav.Buffer.StructureByteStride = sizeof(ParticleData);
    particleUav.Buffer.CounterOffsetInBytes = 0;

    auto createParticleBuffer = [&](BufferWithCounter& buffer, uint32_t srvIndex, uint32_t uavIndex) -> bool {
        const D3D12_RESOURCE_DESC particleDesc = MakeBufferDesc(
            static_cast<UINT64>(sizeof(ParticleData)) * MaxParticles,
            D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS);
        if (FAILED(device->CreateCommittedResource(
                &defaultHeap,
                D3D12_HEAP_FLAG_NONE,
                &particleDesc,
                D3D12_RESOURCE_STATE_UNORDERED_ACCESS,
                nullptr,
                IID_PPV_ARGS(&buffer.Buffer))))
        {
            return false;
        }

        const D3D12_RESOURCE_DESC counterDesc = MakeBufferDesc(
            D3D12_UAV_COUNTER_PLACEMENT_ALIGNMENT,
            D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS);
        if (FAILED(device->CreateCommittedResource(
                &defaultHeap,
                D3D12_HEAP_FLAG_NONE,
                &counterDesc,
                D3D12_RESOURCE_STATE_COPY_DEST,
                nullptr,
                IID_PPV_ARGS(&buffer.Counter))))
        {
            return false;
        }

        buffer.SrvGpu = OffsetGpuHandle(gpuBase, m_descriptorStride, srvIndex);
        buffer.UavGpu = OffsetGpuHandle(gpuBase, m_descriptorStride, uavIndex);
        buffer.BufferState = D3D12_RESOURCE_STATE_UNORDERED_ACCESS;
        buffer.CounterState = D3D12_RESOURCE_STATE_COPY_DEST;

        const D3D12_CPU_DESCRIPTOR_HANDLE srvCpu = OffsetCpuHandle(cpuBase, m_descriptorStride, srvIndex);
        const D3D12_CPU_DESCRIPTOR_HANDLE uavCpu = OffsetCpuHandle(cpuBase, m_descriptorStride, uavIndex);
        device->CreateShaderResourceView(buffer.Buffer.Get(), &particleSrv, srvCpu);
        device->CreateUnorderedAccessView(buffer.Buffer.Get(), buffer.Counter.Get(), &particleUav, uavCpu);

        ResetCounter(uploadList, buffer);
        return true;
    };

    if (!createParticleBuffer(m_particleBuffers[0], DescriptorParticle0Srv, DescriptorParticle0Uav))
        return false;
    if (!createParticleBuffer(m_particleBuffers[1], DescriptorParticle1Srv, DescriptorParticle1Uav))
        return false;

    const D3D12_RESOURCE_DESC liveCountDesc = MakeBufferDesc(sizeof(uint32_t));
    if (FAILED(device->CreateCommittedResource(
            &defaultHeap,
            D3D12_HEAP_FLAG_NONE,
            &liveCountDesc,
            D3D12_RESOURCE_STATE_COPY_DEST,
            nullptr,
            IID_PPV_ARGS(&m_liveCountBuffer))))
    {
        return false;
    }
    m_liveCountState = D3D12_RESOURCE_STATE_COPY_DEST;
    m_liveCountSrvGpu = OffsetGpuHandle(gpuBase, m_descriptorStride, DescriptorLiveCountSrv);

    D3D12_SHADER_RESOURCE_VIEW_DESC countSrv{};
    countSrv.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    countSrv.ViewDimension = D3D12_SRV_DIMENSION_BUFFER;
    countSrv.Format = DXGI_FORMAT_UNKNOWN;
    countSrv.Buffer.NumElements = 1;
    countSrv.Buffer.StructureByteStride = sizeof(uint32_t);
    const D3D12_CPU_DESCRIPTOR_HANDLE liveCountSrvCpu =
        OffsetCpuHandle(cpuBase, m_descriptorStride, DescriptorLiveCountSrv);
    device->CreateShaderResourceView(m_liveCountBuffer.Get(), &countSrv, liveCountSrvCpu);

    uploadList->CopyBufferRegion(m_liveCountBuffer.Get(), 0, m_zeroUpload.Get(), 0, sizeof(uint32_t));
    TransitionResource(
        uploadList,
        m_liveCountBuffer.Get(),
        m_liveCountState,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
    return true;
}

bool ParticleScene::BuildGeometry(ID3D12Device* device,
                                  ID3D12GraphicsCommandList* uploadList,
                                  std::vector<ComPtr<ID3D12Resource>>& uploads)
{
    MeshData bunnyMesh;
    if (!LoadObj(ResolveAsset("Meshes/zaya/Zaya.obj"), bunnyMesh))
        return false;

    const float meshHeight = std::max(bunnyMesh.BoundsMax.y - bunnyMesh.BoundsMin.y, 0.001f);
    const float scale = kTargetBunnyHeight / meshHeight;
    const float centerX = 0.5f * (bunnyMesh.BoundsMin.x + bunnyMesh.BoundsMax.x);
    const float centerZ = 0.5f * (bunnyMesh.BoundsMin.z + bunnyMesh.BoundsMax.z);

    XMStoreFloat4x4(
        &m_bunnyWorld,
        XMMatrixScaling(scale, scale, scale)
            * XMMatrixTranslation(-centerX * scale, -bunnyMesh.BoundsMin.y * scale - 0.1f, -centerZ * scale));
    XMStoreFloat4x4(&m_floorWorld, XMMatrixIdentity());

    const float bunnyHeight = meshHeight * scale;
    m_emitterPosition = {0.f, bunnyHeight * kEmitterHeightFactor + kEmitterHeightOffset, 0.f};

    auto uploadMesh = [&](const MeshData& meshData, MeshGpu& gpu, const XMFLOAT4& fallbackColor) {
        const UINT64 vbSize = static_cast<UINT64>(meshData.Vertices.size()) * sizeof(MeshVertex);
        const UINT64 ibSize = static_cast<UINT64>(meshData.Indices.size()) * sizeof(uint32_t);

        ComPtr<ID3D12Resource> vbUpload;
        ComPtr<ID3D12Resource> ibUpload;
        gpu.VertexBuffer = dx12::CreateDefaultBuffer(device, uploadList, meshData.Vertices.data(), vbSize, vbUpload);
        gpu.IndexBuffer = dx12::CreateDefaultBuffer(device, uploadList, meshData.Indices.data(), ibSize, ibUpload);
        uploads.push_back(vbUpload);
        uploads.push_back(ibUpload);

        gpu.Vbv = {gpu.VertexBuffer->GetGPUVirtualAddress(), static_cast<UINT>(vbSize), sizeof(MeshVertex)};
        gpu.Ibv = {gpu.IndexBuffer->GetGPUVirtualAddress(), static_cast<UINT>(ibSize), DXGI_FORMAT_R32_UINT};
        gpu.Draws.clear();

        if (meshData.SubMeshes.empty())
        {
            MeshDraw draw;
            draw.IndexStart = 0;
            draw.IndexCount = static_cast<uint32_t>(meshData.Indices.size());
            draw.BaseColor = fallbackColor;
            gpu.Draws.push_back(draw);
            return;
        }

        gpu.Draws.reserve(meshData.SubMeshes.size());
        for (const SubMesh& subMesh : meshData.SubMeshes)
        {
            MeshDraw draw;
            draw.IndexStart = subMesh.IndexStart;
            draw.IndexCount = subMesh.IndexCount;
            draw.BaseColor = MakeBaseColor(subMesh.Material.Kd, fallbackColor);
            gpu.Draws.push_back(draw);
        }
    };

    uploadMesh(bunnyMesh, m_bunnyMesh, {0.82f, 0.80f, 0.75f, 1.f});
    uploadMesh(BuildFloorMesh(), m_floorMesh, {0.22f, 0.08f, 0.11f, 1.f});
    return true;
}

bool ParticleScene::BuildConstantBuffer(ID3D12Device* device)
{
    const auto uploadHeap = dx12::HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
    const D3D12_RESOURCE_DESC desc = dx12::BufferDesc(dx12::AlignConstantBufferSize(sizeof(SceneConstants)));
    if (FAILED(device->CreateCommittedResource(
            &uploadHeap,
            D3D12_HEAP_FLAG_NONE,
            &desc,
            D3D12_RESOURCE_STATE_GENERIC_READ,
            nullptr,
            IID_PPV_ARGS(&m_sceneCB))))
    {
        return false;
    }

    D3D12_RANGE readRange{0, 0};
    return SUCCEEDED(m_sceneCB->Map(0, &readRange, reinterpret_cast<void**>(&m_mappedSceneCB)));
}

bool ParticleScene::BuildRootSignatures(ID3D12Device* device)
{
    D3D12_DESCRIPTOR_RANGE particleRange{};
    particleRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    particleRange.NumDescriptors = 1;
    particleRange.BaseShaderRegister = 0;
    particleRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_DESCRIPTOR_RANGE countRange{};
    countRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    countRange.NumDescriptors = 1;
    countRange.BaseShaderRegister = 1;
    countRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_ROOT_PARAMETER graphicsParams[4]{};
    graphicsParams[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    graphicsParams[0].Descriptor.ShaderRegister = 0;
    graphicsParams[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    graphicsParams[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    graphicsParams[1].Constants.ShaderRegister = 1;
    graphicsParams[1].Constants.Num32BitValues = sizeof(DrawConstants) / 4u;
    graphicsParams[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    graphicsParams[2].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    graphicsParams[2].DescriptorTable.NumDescriptorRanges = 1;
    graphicsParams[2].DescriptorTable.pDescriptorRanges = &particleRange;
    graphicsParams[2].ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX;

    graphicsParams[3].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    graphicsParams[3].DescriptorTable.NumDescriptorRanges = 1;
    graphicsParams[3].DescriptorTable.pDescriptorRanges = &countRange;
    graphicsParams[3].ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX;

    D3D12_ROOT_SIGNATURE_DESC graphicsDesc{};
    graphicsDesc.NumParameters = static_cast<UINT>(_countof(graphicsParams));
    graphicsDesc.pParameters = graphicsParams;
    graphicsDesc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    if (!CreateRootSignature(device, graphicsDesc, m_graphicsRootSig))
        return false;

    D3D12_DESCRIPTOR_RANGE computeCountRange{};
    computeCountRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    computeCountRange.NumDescriptors = 1;
    computeCountRange.BaseShaderRegister = 1;
    computeCountRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_DESCRIPTOR_RANGE currentUavRange{};
    currentUavRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_UAV;
    currentUavRange.NumDescriptors = 1;
    currentUavRange.BaseShaderRegister = 0;
    currentUavRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_DESCRIPTOR_RANGE nextUavRange{};
    nextUavRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_UAV;
    nextUavRange.NumDescriptors = 1;
    nextUavRange.BaseShaderRegister = 1;
    nextUavRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_ROOT_PARAMETER computeParams[4]{};
    computeParams[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    computeParams[0].Constants.ShaderRegister = 2;
    computeParams[0].Constants.Num32BitValues = sizeof(UpdateConstants) / 4u;
    computeParams[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    computeParams[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    computeParams[1].DescriptorTable.NumDescriptorRanges = 1;
    computeParams[1].DescriptorTable.pDescriptorRanges = &computeCountRange;
    computeParams[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    computeParams[2].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    computeParams[2].DescriptorTable.NumDescriptorRanges = 1;
    computeParams[2].DescriptorTable.pDescriptorRanges = &currentUavRange;
    computeParams[2].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    computeParams[3].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    computeParams[3].DescriptorTable.NumDescriptorRanges = 1;
    computeParams[3].DescriptorTable.pDescriptorRanges = &nextUavRange;
    computeParams[3].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_ROOT_SIGNATURE_DESC computeDesc{};
    computeDesc.NumParameters = static_cast<UINT>(_countof(computeParams));
    computeDesc.pParameters = computeParams;
    return CreateRootSignature(device, computeDesc, m_computeRootSig);
}

bool ParticleScene::BuildPipelineStates(ID3D12Device* device, DXGI_FORMAT backBufferFmt)
{
    D3D12_BLEND_DESC blend{};
    blend.RenderTarget[0].RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;

    D3D12_DEPTH_STENCIL_DESC depth{};
    depth.DepthEnable = TRUE;
    depth.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    depth.DepthFunc = D3D12_COMPARISON_FUNC_LESS;

    D3D12_RASTERIZER_DESC meshRaster{};
    meshRaster.FillMode = D3D12_FILL_MODE_SOLID;
    meshRaster.CullMode = D3D12_CULL_MODE_NONE;
    meshRaster.DepthClipEnable = TRUE;

    D3D12_RASTERIZER_DESC particleRaster{};
    particleRaster.FillMode = D3D12_FILL_MODE_SOLID;
    particleRaster.CullMode = D3D12_CULL_MODE_NONE;
    particleRaster.DepthClipEnable = TRUE;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC meshDesc{};
    meshDesc.pRootSignature = m_graphicsRootSig.Get();
    meshDesc.VS = {m_meshVs->GetBufferPointer(), m_meshVs->GetBufferSize()};
    meshDesc.PS = {m_meshPs->GetBufferPointer(), m_meshPs->GetBufferSize()};
    meshDesc.InputLayout = {kMeshLayout, static_cast<UINT>(_countof(kMeshLayout))};
    meshDesc.BlendState = blend;
    meshDesc.SampleMask = UINT_MAX;
    meshDesc.RasterizerState = meshRaster;
    meshDesc.DepthStencilState = depth;
    meshDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    meshDesc.NumRenderTargets = 1;
    meshDesc.RTVFormats[0] = backBufferFmt;
    meshDesc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
    meshDesc.SampleDesc.Count = 1;
    if (FAILED(device->CreateGraphicsPipelineState(&meshDesc, IID_PPV_ARGS(&m_meshPso))))
        return false;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC particleDesc{};
    particleDesc.pRootSignature = m_graphicsRootSig.Get();
    particleDesc.VS = {m_particleVs->GetBufferPointer(), m_particleVs->GetBufferSize()};
    particleDesc.GS = {m_particleGs->GetBufferPointer(), m_particleGs->GetBufferSize()};
    particleDesc.PS = {m_particlePs->GetBufferPointer(), m_particlePs->GetBufferSize()};
    particleDesc.BlendState = blend;
    particleDesc.SampleMask = UINT_MAX;
    particleDesc.RasterizerState = particleRaster;
    particleDesc.DepthStencilState = depth;
    particleDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT;
    particleDesc.NumRenderTargets = 1;
    particleDesc.RTVFormats[0] = backBufferFmt;
    particleDesc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
    particleDesc.SampleDesc.Count = 1;
    if (FAILED(device->CreateGraphicsPipelineState(&particleDesc, IID_PPV_ARGS(&m_particlePso))))
        return false;

    D3D12_COMPUTE_PIPELINE_STATE_DESC computeDesc{};
    computeDesc.pRootSignature = m_computeRootSig.Get();
    computeDesc.CS = {m_updateCs->GetBufferPointer(), m_updateCs->GetBufferSize()};
    return SUCCEEDED(device->CreateComputePipelineState(&computeDesc, IID_PPV_ARGS(&m_updatePso)));
}

bool ParticleScene::BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!m_dsvHeap)
    {
        D3D12_DESCRIPTOR_HEAP_DESC heapDesc{};
        heapDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
        heapDesc.NumDescriptors = 1;
        if (FAILED(device->CreateDescriptorHeap(&heapDesc, IID_PPV_ARGS(&m_dsvHeap))))
            return false;
    }

    const auto defaultHeap = dx12::HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
    D3D12_CLEAR_VALUE clearValue{};
    clearValue.Format = DXGI_FORMAT_D32_FLOAT;
    clearValue.DepthStencil.Depth = 1.f;

    const D3D12_RESOURCE_DESC depthDesc = dx12::Texture2DDesc(
        width,
        height,
        DXGI_FORMAT_D32_FLOAT,
        D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);
    if (FAILED(device->CreateCommittedResource(
            &defaultHeap,
            D3D12_HEAP_FLAG_NONE,
            &depthDesc,
            D3D12_RESOURCE_STATE_DEPTH_WRITE,
            &clearValue,
            IID_PPV_ARGS(&m_depthBuffer))))
    {
        return false;
    }

    device->CreateDepthStencilView(m_depthBuffer.Get(), nullptr, m_dsvHeap->GetCPUDescriptorHandleForHeapStart());
    return true;
}

void ParticleScene::UpdateSceneConstants(const XMFLOAT4X4& view, const XMFLOAT4X4& proj)
{
    const XMMATRIX viewMatrix = XMLoadFloat4x4(&view);
    const XMMATRIX projMatrix = XMLoadFloat4x4(&proj);
    const XMMATRIX viewProj = viewMatrix * projMatrix;
    const XMMATRIX invView = XMMatrixInverse(nullptr, viewMatrix);

    const XMVECTOR right = XMVector3Normalize(
        XMVector3TransformNormal(XMVectorSet(1.f, 0.f, 0.f, 0.f), invView));
    const XMVECTOR up = XMVector3Normalize(
        XMVector3TransformNormal(XMVectorSet(0.f, 1.f, 0.f, 0.f), invView));
    const XMVECTOR forward = XMVector3Normalize(
        XMVector3TransformNormal(XMVectorSet(0.f, 0.f, 1.f, 0.f), invView));

    SceneConstants cb{};
    XMStoreFloat4x4(&cb.ViewProj, viewProj);
    XMStoreFloat4(&cb.CameraRight, right);
    XMStoreFloat4(&cb.CameraUp, up);
    XMStoreFloat4(&cb.CameraFacing, XMVectorNegate(forward));
    cb.LightDirection = kSunsetLightDirection;
    cb.LightColor = kSunsetLightColor;
    cb.AmbientColor = kSunsetAmbientColor;
    std::memcpy(m_mappedSceneCB, &cb, sizeof(cb));
}

ParticleScene::UpdateConstants ParticleScene::BuildUpdateConstants(float dt, uint32_t emitCount) const
{
    UpdateConstants constants{};
    constants.DeltaTime = dt;
    constants.TotalTime = m_time;
    constants.EmitCount = emitCount;
    constants.MaxParticles = MaxParticles;
    constants.EmitterPosition = m_emitterPosition;
    constants.SpawnRadius = kParticleSpawnRadius;
    constants.InitialVelocity = {0.f, kParticleInitialRise, 0.f};
    constants.VelocityJitter = kParticleVelocityJitter;
    constants.Gravity = {0.f, kParticleGravity, 0.f};
    constants.BaseSize = kParticleBaseSize;
    return constants;
}

void ParticleScene::ResetCounter(ID3D12GraphicsCommandList* cmdList, BufferWithCounter& buffer)
{
    TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_COPY_DEST);
    cmdList->CopyBufferRegion(buffer.Counter.Get(), 0, m_zeroUpload.Get(), 0, sizeof(uint32_t));
    TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
}

void ParticleScene::CopyLiveCount(ID3D12GraphicsCommandList* cmdList, BufferWithCounter& buffer)
{
    TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_COPY_SOURCE);
    TransitionResource(
        cmdList,
        m_liveCountBuffer.Get(),
        m_liveCountState,
        D3D12_RESOURCE_STATE_COPY_DEST);
    cmdList->CopyBufferRegion(m_liveCountBuffer.Get(), 0, buffer.Counter.Get(), 0, sizeof(uint32_t));
    TransitionResource(
        cmdList,
        m_liveCountBuffer.Get(),
        m_liveCountState,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
    TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
}

void ParticleScene::UpdateParticles(ID3D12GraphicsCommandList* cmdList, float dt)
{
    const float simDt = ClampDeltaTime(dt);
    m_time += simDt;
    m_emitAccumulator = std::min(m_emitAccumulator + simDt * kEmissionRate, static_cast<float>(kMaxEmitPerFrame));
    const uint32_t emitCount = std::min(static_cast<uint32_t>(m_emitAccumulator), kMaxEmitPerFrame);
    m_emitAccumulator -= static_cast<float>(emitCount);

    BufferWithCounter& current = m_particleBuffers[m_currentBufferIndex];
    BufferWithCounter& next = m_particleBuffers[1u - m_currentBufferIndex];

    TransitionResource(cmdList, current.Buffer.Get(), current.BufferState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
    TransitionResource(cmdList, next.Buffer.Get(), next.BufferState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
    TransitionResource(cmdList, current.Counter.Get(), current.CounterState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
    TransitionResource(cmdList, next.Counter.Get(), next.CounterState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);

    ResetCounter(cmdList, next);
    CopyLiveCount(cmdList, current);

    const UpdateConstants constants = BuildUpdateConstants(simDt, emitCount);
    ID3D12DescriptorHeap* heaps[] = {m_descriptorHeap.Get()};
    cmdList->SetDescriptorHeaps(1, heaps);
    cmdList->SetComputeRootSignature(m_computeRootSig.Get());
    cmdList->SetComputeRoot32BitConstants(
        ComputeRootConstants,
        sizeof(UpdateConstants) / 4u,
        &constants,
        0);
    cmdList->SetComputeRootDescriptorTable(ComputeRootCountSrv, m_liveCountSrvGpu);
    cmdList->SetComputeRootDescriptorTable(ComputeRootCurrentParticlesUav, current.UavGpu);
    cmdList->SetComputeRootDescriptorTable(ComputeRootNextParticlesUav, next.UavGpu);
    cmdList->SetPipelineState(m_updatePso.Get());
    cmdList->Dispatch(kDispatchGroups, 1, 1);
    UavBarrier(cmdList, current.Buffer.Get());
    UavBarrier(cmdList, next.Buffer.Get());

    m_currentBufferIndex = 1u - m_currentBufferIndex;
    BufferWithCounter& renderBuffer = m_particleBuffers[m_currentBufferIndex];
    CopyLiveCount(cmdList, renderBuffer);
    TransitionResource(
        cmdList,
        renderBuffer.Buffer.Get(),
        renderBuffer.BufferState,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
}

void ParticleScene::DrawMesh(ID3D12GraphicsCommandList* cmdList,
                             const MeshGpu& mesh,
                             const XMFLOAT4X4& world,
                             bool isFloor)
{
    cmdList->IASetVertexBuffers(0, 1, &mesh.Vbv);
    cmdList->IASetIndexBuffer(&mesh.Ibv);

    for (const MeshDraw& draw : mesh.Draws)
    {
        DrawConstants constants{};
        constants.World = world;
        constants.BaseColor = draw.BaseColor;
        constants.CheckerTileSize = isFloor ? kFloorCheckerTileSize : 0.f;
        constants.IsFloor = isFloor ? 1.f : 0.f;
        cmdList->SetGraphicsRoot32BitConstants(
            GraphicsRootDrawConstants,
            sizeof(DrawConstants) / 4u,
            &constants,
            0);
        cmdList->DrawIndexedInstanced(draw.IndexCount, 1, draw.IndexStart, 0, 0);
    }
}

void ParticleScene::RenderScene(ID3D12GraphicsCommandList* cmdList,
                                D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                                D3D12_VIEWPORT viewport,
                                D3D12_RECT scissorRect)
{
    const D3D12_CPU_DESCRIPTOR_HANDLE dsv = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();

    cmdList->OMSetRenderTargets(1, &backBufferRtv, FALSE, &dsv);
    cmdList->ClearRenderTargetView(backBufferRtv, kSunsetClearColor, 0, nullptr);
    cmdList->ClearDepthStencilView(dsv, D3D12_CLEAR_FLAG_DEPTH, 1.f, 0, 0, nullptr);
    cmdList->RSSetViewports(1, &viewport);
    cmdList->RSSetScissorRects(1, &scissorRect);
    cmdList->SetGraphicsRootSignature(m_graphicsRootSig.Get());
    cmdList->SetGraphicsRootConstantBufferView(GraphicsRootSceneCb, m_sceneCB->GetGPUVirtualAddress());

    cmdList->SetPipelineState(m_meshPso.Get());
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    DrawMesh(cmdList, m_floorMesh, m_floorWorld, true);
    DrawMesh(cmdList, m_bunnyMesh, m_bunnyWorld, false);

    ID3D12DescriptorHeap* heaps[] = {m_descriptorHeap.Get()};
    cmdList->SetDescriptorHeaps(1, heaps);
    cmdList->SetPipelineState(m_particlePso.Get());
    cmdList->SetGraphicsRootConstantBufferView(GraphicsRootSceneCb, m_sceneCB->GetGPUVirtualAddress());
    cmdList->SetGraphicsRootDescriptorTable(
        GraphicsRootParticlesSrv,
        m_particleBuffers[m_currentBufferIndex].SrvGpu);
    cmdList->SetGraphicsRootDescriptorTable(
        GraphicsRootCountSrv,
        m_liveCountSrvGpu);
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_POINTLIST);
    cmdList->DrawInstanced(MaxParticles, 1, 0, 0);
}
