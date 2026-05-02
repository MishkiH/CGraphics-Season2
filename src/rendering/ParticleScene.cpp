#include "ParticleScene.h"

#include "AssetPath.h"
#include "Dx12Helpers.h"
#include "ImageLoader.h"
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
    constexpr float kPrisonHalfExtent = 31.f;
    constexpr float kPrisonHeight = 42.f;
    constexpr float kPrisonSpawnHeight = 60.f;
    constexpr float kPrisonFallSpeed = 60.f;
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
        DescriptorShadowMapSrv = 5,
        DescriptorPrisonTextureSrv = 6,
        DescriptorCount = 7,
    };

    enum GraphicsRootParam : uint32_t
    {
        GraphicsRootSceneCb = 0,
        GraphicsRootDrawConstants = 1,
        GraphicsRootParticlesSrv = 2,
        GraphicsRootCountSrv = 3,
        GraphicsRootShadowMapSrv = 4,
        GraphicsRootPrisonTextureSrv = 5,
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

    float ClampDeltaTime(float dt)
    {
        return std::clamp(dt, 0.f, kMaxDeltaTime);
    }

    D3D12_GRAPHICS_PIPELINE_STATE_DESC MeshPipelineDesc(
        ID3D12RootSignature* rootSignature,
        const D3D12_BLEND_DESC& blend,
        const D3D12_DEPTH_STENCIL_DESC& depth,
        const D3D12_RASTERIZER_DESC& raster,
        D3D12_PRIMITIVE_TOPOLOGY_TYPE topology,
        DXGI_FORMAT dsvFormat)
    {
        D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};
        desc.pRootSignature = rootSignature;
        desc.InputLayout = {kMeshLayout, static_cast<UINT>(_countof(kMeshLayout))};
        desc.BlendState = blend;
        desc.SampleMask = UINT_MAX;
        desc.RasterizerState = raster;
        desc.DepthStencilState = depth;
        desc.PrimitiveTopologyType = topology;
        desc.DSVFormat = dsvFormat;
        desc.SampleDesc.Count = 1;
        return desc;
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

    MeshData BuildPrisonMesh()
    {
        MeshData mesh;
        mesh.Vertices.reserve(20);
        mesh.Indices.reserve(30);

        auto addQuad = [&](XMFLOAT3 bottomLeft,
                           XMFLOAT3 topLeft,
                           XMFLOAT3 bottomRight,
                           XMFLOAT3 topRight,
                           XMFLOAT3 normal,
                           XMFLOAT3 tangent) {
            const uint32_t base = static_cast<uint32_t>(mesh.Vertices.size());
            mesh.Vertices.push_back({bottomLeft, normal, {0.f, 1.f}, tangent});
            mesh.Vertices.push_back({topLeft, normal, {0.f, 0.f}, tangent});
            mesh.Vertices.push_back({bottomRight, normal, {1.f, 1.f}, tangent});
            mesh.Vertices.push_back({topRight, normal, {1.f, 0.f}, tangent});
            mesh.Indices.insert(mesh.Indices.end(), {base, base + 1, base + 2, base + 2, base + 1, base + 3});
        };

        const float e = kPrisonHalfExtent;
        const float h = kPrisonHeight;

        addQuad({-e, 0.f, -e}, {-e, h, -e}, { e, 0.f, -e}, { e, h, -e}, {0.f, 0.f, -1.f}, {1.f, 0.f, 0.f});
        addQuad({ e, 0.f,  e}, { e, h,  e}, {-e, 0.f,  e}, {-e, h,  e}, {0.f, 0.f,  1.f}, {-1.f, 0.f, 0.f});
        addQuad({-e, 0.f,  e}, {-e, h,  e}, {-e, 0.f, -e}, {-e, h, -e}, {-1.f, 0.f, 0.f}, {0.f, 0.f, -1.f});
        addQuad({ e, 0.f, -e}, { e, h, -e}, { e, 0.f,  e}, { e, h,  e}, { 1.f, 0.f, 0.f}, {0.f, 0.f,  1.f});
        addQuad({-e, h,  e}, {-e, h, -e}, { e, h,  e}, { e, h, -e}, {0.f, 1.f, 0.f}, {1.f, 0.f, 0.f});

        SubMesh subMesh;
        subMesh.IndexStart = 0;
        subMesh.IndexCount = static_cast<uint32_t>(mesh.Indices.size());
        subMesh.Material.Kd = {0.85f, 0.85f, 0.85f};
        mesh.SubMeshes.push_back(subMesh);

        mesh.BoundsMin = {-kPrisonHalfExtent, 0.f, -kPrisonHalfExtent};
        mesh.BoundsMax = {kPrisonHalfExtent, kPrisonHeight, kPrisonHalfExtent};
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
    m_prisonFallY = 0.f;
    m_prisonVisible = false;

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
    if (!InitializeShadows(device)) return false;
    m_shadowMapSrvGpu = CreateShadowSrvInHeap(
        device,
        m_descriptorHeap.Get(),
        m_descriptorStride,
        DescriptorShadowMapSrv);

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
    ShutdownShadows();
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
    resetMesh(m_prisonMesh);
    m_prisonTexture.Reset();

    m_descriptorHeap.Reset();
    m_meshVs.Reset();
    m_meshPs.Reset();
    m_shadowVs.Reset();
    m_shadowPs.Reset();
    m_particleVs.Reset();
    m_particleGs.Reset();
    m_particlePs.Reset();
    m_updateCs.Reset();
    m_meshPso.Reset();
    m_particlePso.Reset();
    m_shadowPso.Reset();
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

bool ParticleScene::DropPrisonCage()
{
    if (m_prisonVisible)
        return false;

    m_prisonVisible = true;
    m_prisonFallY = kPrisonSpawnHeight;
    return true;
}

void ParticleScene::RecordCommands(ID3D12GraphicsCommandList* cmdList,
                                   const XMFLOAT4X4& view,
                                   const XMFLOAT4X4& proj,
                                   D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                                   D3D12_VIEWPORT viewport,
                                   D3D12_RECT scissorRect,
                                   float dt)
{
    UpdatePrisonCage(dt);
    UpdateSceneConstants(view, proj);
    UpdateParticles(cmdList, dt);
    RenderShadowMaps(cmdList);
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
        && compile("ShadowVS", "vs_5_0", m_shadowVs)
        && compile("ShadowPS", "ps_5_0", m_shadowPs)
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

        buffer.SrvGpu = dx12::OffsetGpuHandle(gpuBase, m_descriptorStride, srvIndex);
        buffer.UavGpu = dx12::OffsetGpuHandle(gpuBase, m_descriptorStride, uavIndex);
        buffer.BufferState = D3D12_RESOURCE_STATE_UNORDERED_ACCESS;
        buffer.CounterState = D3D12_RESOURCE_STATE_COPY_DEST;

        const D3D12_CPU_DESCRIPTOR_HANDLE srvCpu = dx12::OffsetCpuHandle(cpuBase, m_descriptorStride, srvIndex);
        const D3D12_CPU_DESCRIPTOR_HANDLE uavCpu = dx12::OffsetCpuHandle(cpuBase, m_descriptorStride, uavIndex);
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
    m_liveCountSrvGpu = dx12::OffsetGpuHandle(gpuBase, m_descriptorStride, DescriptorLiveCountSrv);

    D3D12_SHADER_RESOURCE_VIEW_DESC countSrv{};
    countSrv.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    countSrv.ViewDimension = D3D12_SRV_DIMENSION_BUFFER;
    countSrv.Format = DXGI_FORMAT_UNKNOWN;
    countSrv.Buffer.NumElements = 1;
    countSrv.Buffer.StructureByteStride = sizeof(uint32_t);
    const D3D12_CPU_DESCRIPTOR_HANDLE liveCountSrvCpu =
        dx12::OffsetCpuHandle(cpuBase, m_descriptorStride, DescriptorLiveCountSrv);
    device->CreateShaderResourceView(m_liveCountBuffer.Get(), &countSrv, liveCountSrvCpu);

    uploadList->CopyBufferRegion(m_liveCountBuffer.Get(), 0, m_zeroUpload.Get(), 0, sizeof(uint32_t));
    dx12::TransitionResource(
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

    auto uploadMesh = [&](const MeshData& meshData, MeshGpu& gpu, const XMFLOAT4& fallbackColor, bool alphaCutout = false) {
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
            draw.AlphaCutout = alphaCutout;
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
            draw.AlphaCutout = alphaCutout;
            gpu.Draws.push_back(draw);
        }
    };

    uploadMesh(bunnyMesh, m_bunnyMesh, {0.82f, 0.80f, 0.75f, 1.f});
    uploadMesh(BuildFloorMesh(), m_floorMesh, {0.22f, 0.08f, 0.11f, 1.f});
    uploadMesh(BuildPrisonMesh(), m_prisonMesh, {0.85f, 0.85f, 0.85f, 1.f}, true);

    Image prisonImage;
    if (!LoadImage(ResolveAsset("Meshes/zaya/PrisonTexture.png"), prisonImage))
        return false;

    m_prisonTexture = dx12::CreateTexture2D(
        device,
        prisonImage.Width,
        prisonImage.Height,
        DXGI_FORMAT_B8G8R8A8_UNORM,
        D3D12_RESOURCE_STATE_COPY_DEST);
    dx12::UploadTexture2D(device, uploadList, m_prisonTexture.Get(), prisonImage, uploads);

    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc{};
    srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    srvDesc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    srvDesc.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    srvDesc.Texture2D.MipLevels = 1;
    const D3D12_CPU_DESCRIPTOR_HANDLE srvCpu = dx12::OffsetCpuHandle(
        m_descriptorHeap->GetCPUDescriptorHandleForHeapStart(),
        m_descriptorStride,
        DescriptorPrisonTextureSrv);
    device->CreateShaderResourceView(m_prisonTexture.Get(), &srvDesc, srvCpu);
    m_prisonTextureSrvGpu = dx12::OffsetGpuHandle(
        m_descriptorHeap->GetGPUDescriptorHandleForHeapStart(),
        m_descriptorStride,
        DescriptorPrisonTextureSrv);
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
    D3D12_DESCRIPTOR_RANGE particleRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0);
    D3D12_DESCRIPTOR_RANGE countRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 1);
    D3D12_DESCRIPTOR_RANGE shadowRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 2);
    D3D12_DESCRIPTOR_RANGE prisonTextureRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 3);

    D3D12_ROOT_PARAMETER graphicsParams[6]{};
    dx12::SetRootCbv(graphicsParams[0], 0);
    dx12::SetRootConstants(graphicsParams[1], 1, sizeof(DrawConstants) / 4u);
    dx12::SetRootTable(graphicsParams[2], particleRange, D3D12_SHADER_VISIBILITY_VERTEX);
    dx12::SetRootTable(graphicsParams[3], countRange, D3D12_SHADER_VISIBILITY_VERTEX);
    dx12::SetRootTable(graphicsParams[4], shadowRange, D3D12_SHADER_VISIBILITY_PIXEL);
    dx12::SetRootTable(graphicsParams[5], prisonTextureRange, D3D12_SHADER_VISIBILITY_PIXEL);

    D3D12_STATIC_SAMPLER_DESC samplers[] = {
        dx12::ShadowComparisonSampler(0),
        dx12::StaticSampler(
            1,
            D3D12_FILTER_MIN_MAG_MIP_LINEAR,
            D3D12_TEXTURE_ADDRESS_MODE_CLAMP,
            D3D12_SHADER_VISIBILITY_PIXEL),
    };
    D3D12_ROOT_SIGNATURE_DESC graphicsDesc{};
    graphicsDesc.NumParameters = static_cast<UINT>(_countof(graphicsParams));
    graphicsDesc.pParameters = graphicsParams;
    graphicsDesc.NumStaticSamplers = static_cast<UINT>(_countof(samplers));
    graphicsDesc.pStaticSamplers = samplers;
    graphicsDesc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    if (!dx12::CreateRootSignature(device, graphicsDesc, m_graphicsRootSig))
        return false;

    D3D12_DESCRIPTOR_RANGE computeCountRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 1);
    D3D12_DESCRIPTOR_RANGE currentUavRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_UAV, 1, 0);
    D3D12_DESCRIPTOR_RANGE nextUavRange = dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_UAV, 1, 1);

    D3D12_ROOT_PARAMETER computeParams[4]{};
    dx12::SetRootConstants(computeParams[0], 2, sizeof(UpdateConstants) / 4u);
    dx12::SetRootTable(computeParams[1], computeCountRange, D3D12_SHADER_VISIBILITY_ALL);
    dx12::SetRootTable(computeParams[2], currentUavRange, D3D12_SHADER_VISIBILITY_ALL);
    dx12::SetRootTable(computeParams[3], nextUavRange, D3D12_SHADER_VISIBILITY_ALL);

    D3D12_ROOT_SIGNATURE_DESC computeDesc{};
    computeDesc.NumParameters = static_cast<UINT>(_countof(computeParams));
    computeDesc.pParameters = computeParams;
    return dx12::CreateRootSignature(device, computeDesc, m_computeRootSig);
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

    D3D12_RASTERIZER_DESC shadowRaster = meshRaster;
    shadowRaster.DepthBias = 2500;
    shadowRaster.SlopeScaledDepthBias = 2.f;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC meshDesc = MeshPipelineDesc(
        m_graphicsRootSig.Get(),
        blend,
        depth,
        meshRaster,
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
        DXGI_FORMAT_D32_FLOAT);
    meshDesc.VS = dx12::ShaderBytecode(m_meshVs.Get());
    meshDesc.PS = dx12::ShaderBytecode(m_meshPs.Get());
    meshDesc.NumRenderTargets = 1;
    meshDesc.RTVFormats[0] = backBufferFmt;
    if (FAILED(device->CreateGraphicsPipelineState(&meshDesc, IID_PPV_ARGS(&m_meshPso))))
        return false;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC shadowDesc = MeshPipelineDesc(
        m_graphicsRootSig.Get(),
        blend,
        depth,
        shadowRaster,
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
        DXGI_FORMAT_D32_FLOAT);
    shadowDesc.VS = dx12::ShaderBytecode(m_shadowVs.Get());
    shadowDesc.PS = dx12::ShaderBytecode(m_shadowPs.Get());
    shadowDesc.NumRenderTargets = 0;
    if (FAILED(device->CreateGraphicsPipelineState(&shadowDesc, IID_PPV_ARGS(&m_shadowPso))))
        return false;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC particleDesc = MeshPipelineDesc(
        m_graphicsRootSig.Get(),
        blend,
        depth,
        meshRaster,
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT,
        DXGI_FORMAT_D32_FLOAT);
    particleDesc.InputLayout = {};
    particleDesc.VS = dx12::ShaderBytecode(m_particleVs.Get());
    particleDesc.GS = dx12::ShaderBytecode(m_particleGs.Get());
    particleDesc.PS = dx12::ShaderBytecode(m_particlePs.Get());
    particleDesc.NumRenderTargets = 1;
    particleDesc.RTVFormats[0] = backBufferFmt;
    if (FAILED(device->CreateGraphicsPipelineState(&particleDesc, IID_PPV_ARGS(&m_particlePso))))
        return false;

    D3D12_COMPUTE_PIPELINE_STATE_DESC computeDesc{};
    computeDesc.pRootSignature = m_computeRootSig.Get();
    computeDesc.CS = dx12::ShaderBytecode(m_updateCs.Get());
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
    UpdateShadows(view, proj, kSunsetLightDirection);

    const XMVECTOR right = XMVector3Normalize(
        XMVector3TransformNormal(XMVectorSet(1.f, 0.f, 0.f, 0.f), invView));
    const XMVECTOR up = XMVector3Normalize(
        XMVector3TransformNormal(XMVectorSet(0.f, 1.f, 0.f, 0.f), invView));
    const XMVECTOR forward = XMVector3Normalize(
        XMVector3TransformNormal(XMVectorSet(0.f, 0.f, 1.f, 0.f), invView));

    SceneConstants cb{};
    XMStoreFloat4x4(&cb.ViewProj, viewProj);
    cb.View = view;
    CopyShadowConstants(cb);
    XMStoreFloat4(&cb.CameraRight, right);
    XMStoreFloat4(&cb.CameraUp, up);
    XMStoreFloat4(&cb.CameraFacing, XMVectorNegate(forward));
    cb.LightDirection = kSunsetLightDirection;
    cb.LightColor = kSunsetLightColor;
    cb.AmbientColor = kSunsetAmbientColor;
    std::memcpy(m_mappedSceneCB, &cb, sizeof(cb));
}

void ParticleScene::UpdatePrisonCage(float dt)
{
    if (!m_prisonVisible || m_prisonFallY <= 0.f)
        return;

    m_prisonFallY = std::max(0.f, m_prisonFallY - kPrisonFallSpeed * ClampDeltaTime(dt));
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
    dx12::TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_COPY_DEST);
    cmdList->CopyBufferRegion(buffer.Counter.Get(), 0, m_zeroUpload.Get(), 0, sizeof(uint32_t));
    dx12::TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
}

void ParticleScene::CopyLiveCount(ID3D12GraphicsCommandList* cmdList, BufferWithCounter& buffer)
{
    dx12::TransitionResource(
        cmdList,
        buffer.Counter.Get(),
        buffer.CounterState,
        D3D12_RESOURCE_STATE_COPY_SOURCE);
    dx12::TransitionResource(
        cmdList,
        m_liveCountBuffer.Get(),
        m_liveCountState,
        D3D12_RESOURCE_STATE_COPY_DEST);
    cmdList->CopyBufferRegion(m_liveCountBuffer.Get(), 0, buffer.Counter.Get(), 0, sizeof(uint32_t));
    dx12::TransitionResource(
        cmdList,
        m_liveCountBuffer.Get(),
        m_liveCountState,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
    dx12::TransitionResource(
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

    dx12::TransitionResource(cmdList, current.Buffer.Get(), current.BufferState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
    dx12::TransitionResource(cmdList, next.Buffer.Get(), next.BufferState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
    dx12::TransitionResource(cmdList, current.Counter.Get(), current.CounterState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
    dx12::TransitionResource(cmdList, next.Counter.Get(), next.CounterState, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);

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
    dx12::UavBarrier(cmdList, current.Buffer.Get());
    dx12::UavBarrier(cmdList, next.Buffer.Get());

    m_currentBufferIndex = 1u - m_currentBufferIndex;
    BufferWithCounter& renderBuffer = m_particleBuffers[m_currentBufferIndex];
    CopyLiveCount(cmdList, renderBuffer);
    dx12::TransitionResource(
        cmdList,
        renderBuffer.Buffer.Get(),
        renderBuffer.BufferState,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE);
}

void ParticleScene::DrawMesh(ID3D12GraphicsCommandList* cmdList,
                             const MeshGpu& mesh,
                             const XMFLOAT4X4& world,
                             bool isFloor,
                             uint32_t shadowCascadeIndex)
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
        constants.ShadowCascadeIndex = static_cast<float>(shadowCascadeIndex);
        constants.AlphaCutout = draw.AlphaCutout ? 1.f : 0.f;
        cmdList->SetGraphicsRoot32BitConstants(
            GraphicsRootDrawConstants,
            sizeof(DrawConstants) / 4u,
            &constants,
            0);
        cmdList->DrawIndexedInstanced(draw.IndexCount, 1, draw.IndexStart, 0, 0);
    }
}

void ParticleScene::DrawSceneMeshes(ID3D12GraphicsCommandList* cmdList, uint32_t shadowCascadeIndex)
{
    DrawMesh(cmdList, m_floorMesh, m_floorWorld, true, shadowCascadeIndex);
    DrawMesh(cmdList, m_bunnyMesh, m_bunnyWorld, false, shadowCascadeIndex);
    if (m_prisonVisible)
    {
        XMFLOAT4X4 prisonWorld{};
        XMStoreFloat4x4(&prisonWorld, XMMatrixTranslation(0.f, m_prisonFallY, 0.f));
        DrawMesh(cmdList, m_prisonMesh, prisonWorld, false, shadowCascadeIndex);
    }
}

void ParticleScene::RenderShadowMaps(ID3D12GraphicsCommandList* cmdList)
{
    RecordShadowPass(
        cmdList,
        [&]() {
            ID3D12DescriptorHeap* heaps[] = {m_descriptorHeap.Get()};
            cmdList->SetDescriptorHeaps(1, heaps);
            cmdList->SetGraphicsRootSignature(m_graphicsRootSig.Get());
            cmdList->SetGraphicsRootConstantBufferView(GraphicsRootSceneCb, m_sceneCB->GetGPUVirtualAddress());
            cmdList->SetGraphicsRootDescriptorTable(GraphicsRootPrisonTextureSrv, m_prisonTextureSrvGpu);
            cmdList->SetPipelineState(m_shadowPso.Get());
            cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        },
        [&](uint32_t cascade) {
            DrawSceneMeshes(cmdList, cascade);
        });
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

    ID3D12DescriptorHeap* heaps[] = {m_descriptorHeap.Get()};
    cmdList->SetDescriptorHeaps(1, heaps);
    cmdList->SetGraphicsRootDescriptorTable(GraphicsRootShadowMapSrv, m_shadowMapSrvGpu);
    cmdList->SetGraphicsRootDescriptorTable(GraphicsRootPrisonTextureSrv, m_prisonTextureSrvGpu);

    cmdList->SetPipelineState(m_meshPso.Get());
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    DrawSceneMeshes(cmdList);

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
