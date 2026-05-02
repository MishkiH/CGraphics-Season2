#include "ScatterScene.h"
#include "ImageLoader.h"
#include "AssetPath.h"
#include "Dx12Helpers.h"
#include <cstring>
#include <cmath>

using Microsoft::WRL::ComPtr;
using namespace DirectX;

namespace
{
    constexpr float kMotionSpeed = 1.5f;
    const XMFLOAT4 kScatterLightDirection{-0.5f, -1.f, -0.4f, 0.f};

    float SampleAnimationTime(float animationTime, float distanceSq)
    {
        if (distanceSq < 900.f * 900.f)
            return animationTime;

        const float step = distanceSq < 1500.f * 1500.f
            ? 0.10f
            : distanceSq < 2000.f * 2000.f
                ? 0.22f
                : 0.45f;

        return std::floor(animationTime / step) * step;
    }

    XMFLOAT4X4 BuildAnimatedWorld(const SceneInstance& instance, uint32_t instanceIndex,
                                  const XMFLOAT3& eyePos, float animationTime)
    {
        const float dx = instance.World._41 - eyePos.x;
        const float dy = instance.World._42 - eyePos.y;
        const float dz = instance.World._43 - eyePos.z;
        const float sampledTime = SampleAnimationTime(animationTime, dx * dx + dy * dy + dz * dz);
        const float phase = sampledTime * kMotionSpeed - instance.MotionPhaseOffset
            + static_cast<float>(instanceIndex % SceneObjectManager::MeshCount) * 0.18f;
        const float offsetX = std::sin(phase) * SceneObjectManager::MotionAmplitude;

        XMFLOAT4X4 animatedWorld{};
        XMStoreFloat4x4(
            &animatedWorld,
            XMLoadFloat4x4(&instance.World) * XMMatrixTranslation(offsetX, 0.f, 0.f));
        return animatedWorld;
    }
}

bool ScatterScene::Initialize(ID3D12Device* device, ID3D12CommandQueue* cmdQueue,
                               DXGI_FORMAT backBufferFmt, uint32_t width, uint32_t height,
                               const std::string& mesh0Path, const std::string& mesh1Path)
{
    if (!m_scene.Initialize(mesh0Path, mesh1Path)) return false;
    m_scene.BuildOctree();
    if (!BuildShaders(device)) return false;
    if (!BuildRootSignature(device)) return false;
    if (!BuildPSO(device, backBufferFmt)) return false;
    if (!BuildMeshGpu(device, cmdQueue)) return false;
    if (!InitializeShadows(device)) return false;
    BuildShadowDescriptors(device);
    if (!BuildDepthBuffer(device, width, height)) return false;
    if (!BuildSceneCB(device)) return false;
    return true;
}

void ScatterScene::Shutdown()
{
    if (m_mappedSceneCB) { m_sceneCB->Unmap(0, nullptr); m_mappedSceneCB = nullptr; }
    ShutdownShadows();
}

void ScatterScene::OnResize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    m_depthBuffer.Reset();
    BuildDepthBuffer(device, width, height);
}

void ScatterScene::RecordCommands(ID3D12GraphicsCommandList* cmdList,
                                   const XMFLOAT4X4& view, const XMFLOAT4X4& proj,
                                   const XMFLOAT3& eyePos,
                                   D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                                   D3D12_VIEWPORT viewport, D3D12_RECT scissorRect,
                                   float dt)
{
    m_animationTime += dt;
    UpdateSceneConstants(view, proj, eyePos);
    XMFLOAT4X4 viewProj;
    XMStoreFloat4x4(&viewProj, XMLoadFloat4x4(&view) * XMLoadFloat4x4(&proj));
    GatherVisibleInstances(viewProj);
    RenderShadowMaps(cmdList, eyePos);

    D3D12_CPU_DESCRIPTOR_HANDLE dsv = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
    cmdList->OMSetRenderTargets(1, &backBufferRtv, FALSE, &dsv);
    cmdList->ClearDepthStencilView(dsv, D3D12_CLEAR_FLAG_DEPTH, 1.f, 0, 0, nullptr);
    cmdList->RSSetViewports(1, &viewport);
    cmdList->RSSetScissorRects(1, &scissorRect);
    cmdList->SetPipelineState(m_pso.Get());
    cmdList->SetGraphicsRootSignature(m_rootSig.Get());
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cmdList->SetGraphicsRootConstantBufferView(0, m_sceneCB->GetGPUVirtualAddress());
    DrawVisibleInstances(cmdList, eyePos);
}

void ScatterScene::UpdateSceneConstants(const XMFLOAT4X4& view, const XMFLOAT4X4& proj, const XMFLOAT3& eyePos)
{
    UpdateShadows(view, proj, kScatterLightDirection);

    SceneCBData cbData{};
    XMStoreFloat4x4(&cbData.ViewProj, XMLoadFloat4x4(&view) * XMLoadFloat4x4(&proj));
    cbData.View = view;
    const ShadowConstants& shadowConstants = GetShadowConstants();
    for (uint32_t cascade = 0; cascade < ShadowCascadeCount; ++cascade)
        cbData.LightViewProj[cascade] = shadowConstants.LightViewProj[cascade];
    cbData.CascadeFar = shadowConstants.CascadeFar;
    cbData.ShadowParams = shadowConstants.ShadowParams;
    cbData.EyePos = {eyePos.x, eyePos.y, eyePos.z, 1.f};
    std::memcpy(m_mappedSceneCB, &cbData, sizeof(cbData));
}

void ScatterScene::GatherVisibleInstances(const XMFLOAT4X4& viewProj)
{
    m_scene.GetVisibleIndices(viewProj, m_useFrustum, m_useOctree, m_visibleScratch);
    m_lastVisible = static_cast<uint32_t>(m_visibleScratch.size());

    for (auto& bucket : m_visibleByMesh)
    {
        bucket.clear();
        bucket.reserve(m_visibleScratch.size() / SceneObjectManager::MeshCount + 1);
    }

    const auto& instances = m_scene.GetInstances();
    for (uint32_t instanceIndex : m_visibleScratch)
        m_visibleByMesh[instances[instanceIndex].MeshIndex].push_back(instanceIndex);
}

void ScatterScene::DrawVisibleInstances(ID3D12GraphicsCommandList* cmdList, const XMFLOAT3& eyePos)
{
    const auto& instances = m_scene.GetInstances();

    for (uint32_t meshIndex = 0; meshIndex < SceneObjectManager::MeshCount; ++meshIndex)
    {
        const std::vector<uint32_t>& visibleInstances = m_visibleByMesh[meshIndex];
        if (visibleInstances.empty())
            continue;

        const MeshGpu& gpu = m_meshes[meshIndex];
        const MeshData& mesh = m_scene.GetMesh(meshIndex);
        ID3D12DescriptorHeap* heaps[] = {gpu.SrvHeap.Get()};
        cmdList->SetDescriptorHeaps(1, heaps);
        cmdList->SetGraphicsRootDescriptorTable(3, gpu.ShadowSrvGpu);
        cmdList->IASetVertexBuffers(0, 1, &gpu.VBV);
        cmdList->IASetIndexBuffer(&gpu.IBV);

        for (uint32_t instanceIndex : visibleInstances)
        {
            const SceneInstance& instance = instances[instanceIndex];
            const XMFLOAT4X4 animatedWorld = BuildAnimatedWorld(instance, instanceIndex, eyePos, m_animationTime);
            cmdList->SetGraphicsRoot32BitConstants(1, 16, &animatedWorld, 0);

            for (const SubMesh& subMesh : mesh.SubMeshes)
            {
                D3D12_GPU_DESCRIPTOR_HANDLE srv = gpu.SrvHeap->GetGPUDescriptorHandleForHeapStart();
                srv.ptr += static_cast<UINT64>(subMesh.DiffuseTexIndex) * gpu.SrvStride;
                cmdList->SetGraphicsRootDescriptorTable(2, srv);
                cmdList->DrawIndexedInstanced(subMesh.IndexCount, 1, subMesh.IndexStart, 0, 0);
            }
        }
    }
}

void ScatterScene::DrawShadowCasters(ID3D12GraphicsCommandList* cmdList, const XMFLOAT3& eyePos, uint32_t cascadeIndex)
{
    const auto& instances = m_scene.GetInstances();

    for (uint32_t meshIndex = 0; meshIndex < SceneObjectManager::MeshCount; ++meshIndex)
    {
        const std::vector<uint32_t>& visibleInstances = m_visibleByMesh[meshIndex];
        if (visibleInstances.empty())
            continue;

        const MeshGpu& gpu = m_meshes[meshIndex];
        const MeshData& mesh = m_scene.GetMesh(meshIndex);
        cmdList->IASetVertexBuffers(0, 1, &gpu.VBV);
        cmdList->IASetIndexBuffer(&gpu.IBV);

        for (uint32_t instanceIndex : visibleInstances)
        {
            const SceneInstance& instance = instances[instanceIndex];
            const XMFLOAT4X4 animatedWorld = BuildAnimatedWorld(instance, instanceIndex, eyePos, m_animationTime);
            cmdList->SetGraphicsRoot32BitConstants(1, 16, &animatedWorld, 0);
            cmdList->SetGraphicsRoot32BitConstants(1, 1, &cascadeIndex, 16);

            for (const SubMesh& subMesh : mesh.SubMeshes)
                cmdList->DrawIndexedInstanced(subMesh.IndexCount, 1, subMesh.IndexStart, 0, 0);
        }
    }
}

void ScatterScene::RenderShadowMaps(ID3D12GraphicsCommandList* cmdList, const XMFLOAT3& eyePos)
{
    RecordShadowPass(
        cmdList,
        [&]() {
            cmdList->SetGraphicsRootSignature(m_rootSig.Get());
            cmdList->SetGraphicsRootConstantBufferView(0, m_sceneCB->GetGPUVirtualAddress());
            cmdList->SetPipelineState(m_shadowPso.Get());
            cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        },
        [&](uint32_t cascade) {
            DrawShadowCasters(cmdList, eyePos, cascade);
        });
}

bool ScatterScene::BuildShaders(ID3D12Device*)
{
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif
    std::wstring path = ToWide(ResolveAsset("Shaders/ScatterScene.hlsl"));
    ComPtr<ID3DBlob> errors;

    auto compile = [&](const char* entry, const char* target, ComPtr<ID3DBlob>& blob) -> bool {
        errors.Reset();
        HRESULT hr = D3DCompileFromFile(path.c_str(), nullptr, D3D_COMPILE_STANDARD_FILE_INCLUDE, entry, target, flags, 0, &blob, &errors);
        if (FAILED(hr)) { if (errors) throw std::runtime_error((const char*)errors->GetBufferPointer()); return false; }
        return true;
    };
    return compile("VS", "vs_5_0", m_vs)
        && compile("PS", "ps_5_0", m_ps)
        && compile("ShadowVS", "vs_5_0", m_shadowVs);
}

bool ScatterScene::BuildRootSignature(ID3D12Device* device)
{
    D3D12_DESCRIPTOR_RANGE texRange{};
    texRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    texRange.NumDescriptors = 1;
    texRange.BaseShaderRegister = 0;
    texRange.RegisterSpace = 0;
    texRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_DESCRIPTOR_RANGE shadowRange{};
    shadowRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    shadowRange.NumDescriptors = 1;
    shadowRange.BaseShaderRegister = 1;
    shadowRange.RegisterSpace = 0;
    shadowRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_ROOT_PARAMETER params[4]{};
    params[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    params[0].Descriptor.ShaderRegister = 0;
    params[0].Descriptor.RegisterSpace = 0;
    params[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    params[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    params[1].Constants.ShaderRegister = 1;
    params[1].Constants.RegisterSpace = 0;
    params[1].Constants.Num32BitValues = 17;
    params[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX;

    params[2].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[2].DescriptorTable.NumDescriptorRanges = 1;
    params[2].DescriptorTable.pDescriptorRanges = &texRange;
    params[2].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[3].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[3].DescriptorTable.NumDescriptorRanges = 1;
    params[3].DescriptorTable.pDescriptorRanges = &shadowRange;
    params[3].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_STATIC_SAMPLER_DESC samplers[2]{};
    samplers[0].Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    samplers[0].AddressU = samplers[0].AddressV = samplers[0].AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    samplers[0].ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    samplers[0].MaxLOD = D3D12_FLOAT32_MAX;
    samplers[0].ShaderRegister = 0;
    samplers[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    samplers[1].Filter = D3D12_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT;
    samplers[1].AddressU = samplers[1].AddressV = samplers[1].AddressW = D3D12_TEXTURE_ADDRESS_MODE_BORDER;
    samplers[1].ComparisonFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
    samplers[1].BorderColor = D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE;
    samplers[1].MaxLOD = D3D12_FLOAT32_MAX;
    samplers[1].ShaderRegister = 1;
    samplers[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_ROOT_SIGNATURE_DESC desc{};
    desc.NumParameters = static_cast<UINT>(_countof(params)); desc.pParameters = params;
    desc.NumStaticSamplers = static_cast<UINT>(_countof(samplers)); desc.pStaticSamplers = samplers;
    desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> blob, err;
    if (FAILED(D3D12SerializeRootSignature(&desc, D3D_ROOT_SIGNATURE_VERSION_1, &blob, &err))) return false;
    return SUCCEEDED(device->CreateRootSignature(0, blob->GetBufferPointer(), blob->GetBufferSize(), IID_PPV_ARGS(&m_rootSig)));
}

bool ScatterScene::BuildPSO(ID3D12Device* device, DXGI_FORMAT backBufferFmt)
{
    D3D12_INPUT_ELEMENT_DESC layout[] = {
        {"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"NORMAL", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 24, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"TANGENT", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 32, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
    };

    D3D12_RASTERIZER_DESC raster{D3D12_FILL_MODE_SOLID, D3D12_CULL_MODE_BACK};
    raster.DepthClipEnable = TRUE;
    D3D12_BLEND_DESC blend{};
    blend.RenderTarget[0].RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
    D3D12_DEPTH_STENCIL_DESC depth{};
    depth.DepthEnable = TRUE; depth.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    depth.DepthFunc = D3D12_COMPARISON_FUNC_LESS;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};
    desc.pRootSignature = m_rootSig.Get();
    desc.VS = {m_vs->GetBufferPointer(), m_vs->GetBufferSize()};
    desc.PS = {m_ps->GetBufferPointer(), m_ps->GetBufferSize()};
    desc.InputLayout = {layout, 4};
    desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    desc.NumRenderTargets = 1; desc.RTVFormats[0] = backBufferFmt;
    desc.DSVFormat = DXGI_FORMAT_D32_FLOAT; desc.SampleDesc.Count = 1;
    desc.RasterizerState = raster; desc.BlendState = blend;
    desc.DepthStencilState = depth; desc.SampleMask = UINT_MAX;
    if (FAILED(device->CreateGraphicsPipelineState(&desc, IID_PPV_ARGS(&m_pso))))
        return false;

    D3D12_RASTERIZER_DESC shadowRaster = raster;
    shadowRaster.DepthBias = 2500;
    shadowRaster.SlopeScaledDepthBias = 2.f;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC shadowDesc{};
    shadowDesc.pRootSignature = m_rootSig.Get();
    shadowDesc.VS = {m_shadowVs->GetBufferPointer(), m_shadowVs->GetBufferSize()};
    shadowDesc.InputLayout = {layout, 4};
    shadowDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    shadowDesc.NumRenderTargets = 0;
    shadowDesc.DSVFormat = GetShadowDsvFormat();
    shadowDesc.SampleDesc.Count = 1;
    shadowDesc.RasterizerState = shadowRaster;
    shadowDesc.BlendState = blend;
    shadowDesc.DepthStencilState = depth;
    shadowDesc.SampleMask = UINT_MAX;
    return SUCCEEDED(device->CreateGraphicsPipelineState(&shadowDesc, IID_PPV_ARGS(&m_shadowPso)));
}

bool ScatterScene::BuildMeshGpu(ID3D12Device* device, ID3D12CommandQueue* cmdQueue)
{
    ComPtr<ID3D12CommandAllocator> alloc;
    ComPtr<ID3D12GraphicsCommandList> list;
    dx12::ThrowIfFailed(device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&alloc)), "scatter alloc");
    dx12::ThrowIfFailed(device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, alloc.Get(), nullptr, IID_PPV_ARGS(&list)), "scatter list");

    std::vector<ComPtr<ID3D12Resource>> uploads;
    for (uint32_t m = 0; m < SceneObjectManager::MeshCount; ++m)
        UploadMesh(device, list.Get(), m_meshes[m], m_scene.GetMesh(m), uploads);

    dx12::ExecuteAndWait(device, cmdQueue, list.Get());
    return true;
}

void ScatterScene::UploadMesh(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                               MeshGpu& gpu, const MeshData& mesh,
                               std::vector<ComPtr<ID3D12Resource>>& uploads)
{
    uint64_t vbSz = mesh.Vertices.size() * sizeof(MeshVertex);
    uint64_t ibSz = mesh.Indices.size() * sizeof(uint32_t);

    ComPtr<ID3D12Resource> vbUp, ibUp;
    gpu.VertexBuffer = dx12::CreateDefaultBuffer(device, cmdList, mesh.Vertices.data(), vbSz, vbUp);
    gpu.IndexBuffer = dx12::CreateDefaultBuffer(device, cmdList, mesh.Indices.data(), ibSz, ibUp);
    uploads.push_back(vbUp); uploads.push_back(ibUp);
    gpu.VBV = {gpu.VertexBuffer->GetGPUVirtualAddress(), (UINT)vbSz, sizeof(MeshVertex)};
    gpu.IBV = {gpu.IndexBuffer->GetGPUVirtualAddress(), (UINT)ibSz, DXGI_FORMAT_R32_UINT};

    uint32_t texCount = (uint32_t)mesh.DiffusePaths.size();
    D3D12_DESCRIPTOR_HEAP_DESC hd{};
    hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    hd.NumDescriptors = texCount + 1u;
    hd.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    dx12::ThrowIfFailed(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&gpu.SrvHeap)), "scatter SRV heap");
    gpu.SrvStride = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    gpu.ShadowSrvIndex = texCount;
    gpu.Textures.resize(texCount);

    auto uploadTex = [&](uint32_t slot, const Image& img) {
        gpu.Textures[slot] = dx12::CreateTexture2D(
            device,
            img.Width,
            img.Height,
            DXGI_FORMAT_B8G8R8A8_UNORM,
            D3D12_RESOURCE_STATE_COPY_DEST);
        dx12::UploadTexture2D(device, cmdList, gpu.Textures[slot].Get(), img, uploads);
    };

    uploadTex(0, Image{1, 1, {255, 255, 255, 255}});
    for (uint32_t i = 1; i < texCount; ++i)
    {
        Image img;
        if (LoadImage(mesh.DiffusePaths[i], img)) uploadTex(i, img);
        else gpu.Textures[i] = gpu.Textures[0];
    }

    D3D12_SHADER_RESOURCE_VIEW_DESC sd{};
    sd.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    sd.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    sd.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D; sd.Texture2D.MipLevels = 1;
    D3D12_CPU_DESCRIPTOR_HANDLE h = gpu.SrvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < texCount; ++i)
    {
        device->CreateShaderResourceView(gpu.Textures[i].Get(), &sd, h);
        h.ptr += gpu.SrvStride;
    }
}

void ScatterScene::BuildShadowDescriptors(ID3D12Device* device)
{
    for (MeshGpu& gpu : m_meshes)
    {
        D3D12_CPU_DESCRIPTOR_HANDLE cpuHandle = gpu.SrvHeap->GetCPUDescriptorHandleForHeapStart();
        cpuHandle.ptr += static_cast<SIZE_T>(gpu.ShadowSrvIndex) * gpu.SrvStride;
        CreateShadowSrv(device, cpuHandle);

        gpu.ShadowSrvGpu = gpu.SrvHeap->GetGPUDescriptorHandleForHeapStart();
        gpu.ShadowSrvGpu.ptr += static_cast<UINT64>(gpu.ShadowSrvIndex) * gpu.SrvStride;
    }
}

bool ScatterScene::BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!m_dsvHeap)
    {
        D3D12_DESCRIPTOR_HEAP_DESC hd{};
        hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
        hd.NumDescriptors = 1;
        if (FAILED(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_dsvHeap)))) return false;
    }
    auto hp = dx12::HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
    D3D12_RESOURCE_DESC rd = dx12::Texture2DDesc(width, height, DXGI_FORMAT_D32_FLOAT, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);
    D3D12_CLEAR_VALUE cv{DXGI_FORMAT_D32_FLOAT}; cv.DepthStencil.Depth = 1.f;
    if (FAILED(device->CreateCommittedResource(&hp, D3D12_HEAP_FLAG_NONE, &rd,
            D3D12_RESOURCE_STATE_DEPTH_WRITE, &cv, IID_PPV_ARGS(&m_depthBuffer)))) return false;
    device->CreateDepthStencilView(m_depthBuffer.Get(), nullptr, m_dsvHeap->GetCPUDescriptorHandleForHeapStart());
    return true;
}

bool ScatterScene::BuildSceneCB(ID3D12Device* device)
{
    auto hp = dx12::HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
    D3D12_RESOURCE_DESC rd = dx12::BufferDesc(dx12::AlignConstantBufferSize(sizeof(SceneCBData)));
    if (FAILED(device->CreateCommittedResource(&hp, D3D12_HEAP_FLAG_NONE, &rd,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&m_sceneCB)))) return false;
    D3D12_RANGE rr{0, 0};
    m_sceneCB->Map(0, &rr, reinterpret_cast<void**>(&m_mappedSceneCB));
    return true;
}
