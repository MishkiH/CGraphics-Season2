#include "DeferredScene.h"
#include "GBuffer.h"
#include "ObjLoader.h"
#include "ImageLoader.h"
#include "AssetPath.h"
#include "Dx12Helpers.h"
#include <cstring>
#include <cmath>
#include <algorithm>

using namespace DirectX;
using Microsoft::WRL::ComPtr;

namespace
{
    XMFLOAT3 NormalizeOrFallback(const XMFLOAT3& v, const XMFLOAT3& fallback)
    {
        const XMVECTOR vec = XMLoadFloat3(&v);
        if (XMVectorGetX(XMVector3LengthSq(vec)) < 1e-6f)
            return fallback;

        XMFLOAT3 out{};
        XMStoreFloat3(&out, XMVector3Normalize(vec));
        return out;
    }
}

bool DeferredScene::Initialize(ID3D12Device* device, ID3D12CommandQueue* cmdQueue,
                                DXGI_FORMAT backBufferFmt, uint32_t width, uint32_t height,
                                const SceneOptions& options)
{
    m_options = options;
    if (m_options.MeshPath.empty())
        return false;
    if (m_options.Lights.empty())
    {
        SceneLight fallbackLight{};
        fallbackLight.Direction = {0.4f, -1.f, 0.3f};
        fallbackLight.Intensity = 1.8f;
        m_options.Lights.push_back(fallbackLight);
    }

    XMStoreFloat4x4(&m_view, XMMatrixIdentity());
    XMStoreFloat4x4(&m_proj, XMMatrixIdentity());

    ComPtr<ID3D12CommandAllocator> uploadAlloc;
    ComPtr<ID3D12GraphicsCommandList> uploadList;
    dx12::ThrowIfFailed(device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&uploadAlloc)), "deferred upload alloc");
    dx12::ThrowIfFailed(device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, uploadAlloc.Get(), nullptr, IID_PPV_ARGS(&uploadList)), "deferred upload list");

    std::vector<ComPtr<ID3D12Resource>> uploads;

    if (!BuildShaders(device)) return false;
    if (!BuildRootSignature(device)) return false;
    if (!BuildSceneGeometry(device, uploadList.Get(), uploads)) return false;
    if (m_options.EnableWater && !BuildWaterGeometry(device, uploadList.Get(), uploads)) return false;
    if (!BuildConstantBuffers(device)) return false;

    m_gBuffer = std::make_unique<GBuffer>();
    m_gBuffer->Initialize(device, width, height);

    if (!BuildPSOs(device, backBufferFmt)) return false;

    dx12::ExecuteAndWait(device, cmdQueue, uploadList.Get());
    return true;
}

void DeferredScene::Shutdown()
{
    if (m_mappedPassCB) { m_passCB->Unmap(0, nullptr); m_mappedPassCB = nullptr; }
    if (m_mappedLightCB) { m_lightCB->Unmap(0, nullptr); m_mappedLightCB = nullptr; }
    if (m_gBuffer) { m_gBuffer->Shutdown(); m_gBuffer.reset(); }
}

void DeferredScene::OnResize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (m_gBuffer) m_gBuffer->Resize(device, width, height);
}

void DeferredScene::SetCamera(const XMFLOAT4X4& view, const XMFLOAT4X4& proj, const XMFLOAT3& eye)
{
    m_view = view; m_proj = proj; m_eye = eye;
}

void DeferredScene::RecordCommands(ID3D12GraphicsCommandList* cmdList,
                                    D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                                    D3D12_VIEWPORT vp, D3D12_RECT sr, float dt)
{
    UpdateLightConstants(dt);
    UpdatePassConstants((uint32_t)vp.Width, (uint32_t)vp.Height);

    cmdList->RSSetViewports(1, &vp);
    cmdList->RSSetScissorRects(1, &sr);

    // Geometry pass (GBuffer)
    m_gBuffer->TransitionToWrite(cmdList);
    m_gBuffer->BindForGeometryPass(cmdList);

    cmdList->SetGraphicsRootSignature(m_rootSig.Get());
    cmdList->SetPipelineState(m_geometryPSO.Get());
    cmdList->SetGraphicsRootConstantBufferView(0, m_passCB->GetGPUVirtualAddress());

    ID3D12DescriptorHeap* texHeaps[] = {m_textureHeap.Get()};
    cmdList->SetDescriptorHeaps(1, texHeaps);
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST);
    cmdList->IASetVertexBuffers(0, 1, &m_vbv);
    cmdList->IASetIndexBuffer(&m_ibv);

    auto heapBase = m_textureHeap->GetGPUDescriptorHandleForHeapStart();
    auto srvAt = [&](uint32_t slot) {
        D3D12_GPU_DESCRIPTOR_HANDLE h = heapBase;
        h.ptr += (UINT64)slot * m_srvStride;
        return h;
    };

    for (const DrawItem& item : m_drawItems)
    {
        cmdList->SetGraphicsRootDescriptorTable(1, srvAt(item.TextureIndex));
        cmdList->SetGraphicsRootDescriptorTable(5, srvAt(item.NormalTextureIndex));
        cmdList->SetGraphicsRootDescriptorTable(6, srvAt(item.DisplacementTextureIndex));
        cmdList->SetGraphicsRoot32BitConstants(2, 8, &item.Material, 0);
        cmdList->DrawIndexedInstanced(item.IndexCount, 1, item.StartIndexLocation, 0, 0);
    }

    // Lighting pass (back buffer)
    m_gBuffer->TransitionToRead(cmdList);
    m_gBuffer->TransitionDepthToRead(cmdList);

    const float black[4] = {};
    cmdList->OMSetRenderTargets(1, &backBufferRtv, TRUE, nullptr);
    cmdList->ClearRenderTargetView(backBufferRtv, black, 0, nullptr);

    cmdList->SetPipelineState(m_lightingPSO.Get());
    cmdList->SetGraphicsRootSignature(m_rootSig.Get());
    cmdList->SetGraphicsRootConstantBufferView(0, m_passCB->GetGPUVirtualAddress());
    cmdList->SetGraphicsRootConstantBufferView(3, m_lightCB->GetGPUVirtualAddress());

    ID3D12DescriptorHeap* gbHeaps[] = {m_gBuffer->GetSrvHeap()};
    cmdList->SetDescriptorHeaps(1, gbHeaps);
    cmdList->SetGraphicsRootDescriptorTable(4, m_gBuffer->GetSrvTable());
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cmdList->DrawInstanced(3, 1, 0, 0);

    if (m_options.EnableWater && m_waterIndexCount > 0)
    {
        D3D12_CPU_DESCRIPTOR_HANDLE dsvRO = m_gBuffer->GetDsvReadOnly();
        cmdList->OMSetRenderTargets(1, &backBufferRtv, FALSE, &dsvRO);
        cmdList->SetPipelineState(m_waterPSO.Get());
        cmdList->SetGraphicsRootSignature(m_rootSig.Get());
        cmdList->SetGraphicsRootConstantBufferView(0, m_passCB->GetGPUVirtualAddress());

        ID3D12DescriptorHeap* wHeaps[] = {m_textureHeap.Get()};
        cmdList->SetDescriptorHeaps(1, wHeaps);
        auto base = m_textureHeap->GetGPUDescriptorHandleForHeapStart();
        cmdList->SetGraphicsRootDescriptorTable(1, base);
        cmdList->SetGraphicsRootDescriptorTable(5, base);
        cmdList->SetGraphicsRootDescriptorTable(6, base);

        const float dummyMat[8] = {1.f, 1.f, 1.f, 1.f, 0.18f, 32.f, 0.f, 0.f};
        cmdList->SetGraphicsRoot32BitConstants(2, 8, dummyMat, 0);
        cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST);
        cmdList->IASetVertexBuffers(0, 1, &m_waterVBV);
        cmdList->IASetIndexBuffer(&m_waterIBV);
        cmdList->DrawIndexedInstanced(m_waterIndexCount, 1, 0, 0, 0);
    }

    m_gBuffer->TransitionDepthToWrite(cmdList);
}

bool DeferredScene::BuildShaders(ID3D12Device*)
{
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif
    ComPtr<ID3DBlob> errors;
    std::wstring sp = ToWide(ResolveAsset("Shaders/DeferredScene.hlsl"));

    auto compile = [&](const char* entry, const char* target, ComPtr<ID3DBlob>& blob) -> bool {
        errors.Reset();
        HRESULT hr = D3DCompileFromFile(sp.c_str(), nullptr, D3D_COMPILE_STANDARD_FILE_INCLUDE,
                                        entry, target, flags, 0, &blob, &errors);
        if (FAILED(hr)) { if (errors) throw std::runtime_error((const char*)errors->GetBufferPointer()); dx12::ThrowIfFailed(hr, entry); }
        return true;
    };

    return compile("GeometryVS", "vs_5_0", m_geometryVS)
        && compile("GeometryHS", "hs_5_0", m_hullShader)
        && compile("GeometryDS", "ds_5_0", m_domainShader)
        && compile("GeometryPS", "ps_5_0", m_geometryPS)
        && compile("LightingVS", "vs_5_0", m_lightingVS)
        && compile("LightingPS", "ps_5_0", m_lightingPS)
        && compile("WaterVS", "vs_5_0", m_waterVS)
        && compile("WaterHS", "hs_5_0", m_waterHS)
        && compile("WaterDS", "ds_5_0", m_waterDS)
        && compile("WaterPS", "ps_5_0", m_waterPS)
        && [&]() {
            m_inputLayout[0] = {"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
            m_inputLayout[1] = {"NORMAL", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
            m_inputLayout[2] = {"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 24, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
            m_inputLayout[3] = {"TANGENT", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 32, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
            return true;
        }();
}

bool DeferredScene::BuildRootSignature(ID3D12Device* device)
{
    D3D12_DESCRIPTOR_RANGE texRange{D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0, 0, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND};
    D3D12_DESCRIPTOR_RANGE gbRange{D3D12_DESCRIPTOR_RANGE_TYPE_SRV, GBuffer::SrvCount, 3, 0, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND};
    D3D12_DESCRIPTOR_RANGE normRange{D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 1, 0, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND};
    D3D12_DESCRIPTOR_RANGE dispRange{D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 2, 0, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND};

    D3D12_ROOT_PARAMETER params[7]{};

    params[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    params[0].Descriptor.ShaderRegister = 0;
    params[0].Descriptor.RegisterSpace = 0;
    params[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    params[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[1].DescriptorTable.NumDescriptorRanges = 1;
    params[1].DescriptorTable.pDescriptorRanges = &texRange;
    params[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[2].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    params[2].Constants.ShaderRegister = 2;
    params[2].Constants.RegisterSpace = 0;
    params[2].Constants.Num32BitValues = 8;
    params[2].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[3].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    params[3].Descriptor.ShaderRegister = 1;
    params[3].Descriptor.RegisterSpace = 0;
    params[3].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[4].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[4].DescriptorTable.NumDescriptorRanges = 1;
    params[4].DescriptorTable.pDescriptorRanges = &gbRange;
    params[4].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[5].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[5].DescriptorTable.NumDescriptorRanges = 1;
    params[5].DescriptorTable.pDescriptorRanges = &normRange;
    params[5].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[6].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[6].DescriptorTable.NumDescriptorRanges = 1;
    params[6].DescriptorTable.pDescriptorRanges = &dispRange;
    params[6].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_STATIC_SAMPLER_DESC sampler{};
    sampler.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    sampler.AddressU = sampler.AddressV = sampler.AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    sampler.MaxAnisotropy = 1; sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    sampler.MaxLOD = D3D12_FLOAT32_MAX; sampler.ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_ROOT_SIGNATURE_DESC desc{};
    desc.NumParameters = 7; desc.pParameters = params;
    desc.NumStaticSamplers = 1; desc.pStaticSamplers = &sampler;
    desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> blob, err;
    HRESULT hr = D3D12SerializeRootSignature(&desc, D3D_ROOT_SIGNATURE_VERSION_1, &blob, &err);
    if (FAILED(hr)) { if (err) throw std::runtime_error((const char*)err->GetBufferPointer()); return false; }
    return SUCCEEDED(device->CreateRootSignature(0, blob->GetBufferPointer(), blob->GetBufferSize(), IID_PPV_ARGS(&m_rootSig)));
}

bool DeferredScene::BuildPSOs(ID3D12Device* device, DXGI_FORMAT backBufferFmt)
{
    D3D12_RASTERIZER_DESC raster{};
    raster.FillMode = D3D12_FILL_MODE_SOLID; raster.CullMode = D3D12_CULL_MODE_NONE;
    raster.FrontCounterClockwise = TRUE; raster.DepthClipEnable = TRUE;

    const D3D12_RENDER_TARGET_BLEND_DESC noBlend = {
        FALSE, FALSE,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_LOGIC_OP_NOOP, D3D12_COLOR_WRITE_ENABLE_ALL
    };
    D3D12_BLEND_DESC blendOff{};
    for (auto& rt : blendOff.RenderTarget) rt = noBlend;

    D3D12_DEPTH_STENCIL_DESC geoDepth{};
    geoDepth.DepthEnable = TRUE; geoDepth.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    geoDepth.DepthFunc = D3D12_COMPARISON_FUNC_LESS;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC geoPso{};
    geoPso.pRootSignature = m_rootSig.Get();
    geoPso.VS = {m_geometryVS->GetBufferPointer(), m_geometryVS->GetBufferSize()};
    geoPso.HS = {m_hullShader->GetBufferPointer(), m_hullShader->GetBufferSize()};
    geoPso.DS = {m_domainShader->GetBufferPointer(), m_domainShader->GetBufferSize()};
    geoPso.PS = {m_geometryPS->GetBufferPointer(), m_geometryPS->GetBufferSize()};
    geoPso.BlendState = blendOff; geoPso.SampleMask = UINT_MAX;
    geoPso.RasterizerState = raster; geoPso.DepthStencilState = geoDepth;
    geoPso.InputLayout = {m_inputLayout, 4};
    geoPso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;
    geoPso.NumRenderTargets = GBuffer::TargetCount;
    geoPso.RTVFormats[0] = m_gBuffer->GetAlbedoSpecFormat();
    geoPso.RTVFormats[1] = m_gBuffer->GetNormalFormat();
    geoPso.DSVFormat = m_gBuffer->GetDepthStencilFormat();
    geoPso.SampleDesc = {1, 0};
    dx12::ThrowIfFailed(device->CreateGraphicsPipelineState(&geoPso, IID_PPV_ARGS(&m_geometryPSO)), "Geometry PSO");

    D3D12_DEPTH_STENCIL_DESC noDepth{};
    D3D12_GRAPHICS_PIPELINE_STATE_DESC litPso{};
    litPso.pRootSignature = m_rootSig.Get();
    litPso.VS = {m_lightingVS->GetBufferPointer(), m_lightingVS->GetBufferSize()};
    litPso.PS = {m_lightingPS->GetBufferPointer(), m_lightingPS->GetBufferSize()};
    litPso.BlendState = blendOff; litPso.SampleMask = UINT_MAX;
    litPso.RasterizerState = raster; litPso.DepthStencilState = noDepth;
    litPso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    litPso.NumRenderTargets = 1; litPso.RTVFormats[0] = backBufferFmt;
    litPso.DSVFormat = DXGI_FORMAT_UNKNOWN; litPso.SampleDesc = {1, 0};
    dx12::ThrowIfFailed(device->CreateGraphicsPipelineState(&litPso, IID_PPV_ARGS(&m_lightingPSO)), "Lighting PSO");

    D3D12_RENDER_TARGET_BLEND_DESC waterBlend{};
    waterBlend.BlendEnable = TRUE;
    waterBlend.SrcBlend = D3D12_BLEND_SRC_ALPHA; waterBlend.DestBlend = D3D12_BLEND_INV_SRC_ALPHA; waterBlend.BlendOp = D3D12_BLEND_OP_ADD;
    waterBlend.SrcBlendAlpha = D3D12_BLEND_ONE; waterBlend.DestBlendAlpha = D3D12_BLEND_ZERO; waterBlend.BlendOpAlpha = D3D12_BLEND_OP_ADD;
    waterBlend.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
    D3D12_BLEND_DESC wBlend{}; wBlend.RenderTarget[0] = waterBlend;

    D3D12_DEPTH_STENCIL_DESC wDepth{};
    wDepth.DepthEnable = TRUE; wDepth.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ZERO;
    wDepth.DepthFunc = D3D12_COMPARISON_FUNC_LESS;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC waterPso{};
    waterPso.pRootSignature = m_rootSig.Get();
    waterPso.VS = {m_waterVS->GetBufferPointer(), m_waterVS->GetBufferSize()};
    waterPso.HS = {m_waterHS->GetBufferPointer(), m_waterHS->GetBufferSize()};
    waterPso.DS = {m_waterDS->GetBufferPointer(), m_waterDS->GetBufferSize()};
    waterPso.PS = {m_waterPS->GetBufferPointer(), m_waterPS->GetBufferSize()};
    waterPso.BlendState = wBlend; waterPso.SampleMask = UINT_MAX;
    waterPso.RasterizerState = raster; waterPso.DepthStencilState = wDepth;
    waterPso.InputLayout = {m_inputLayout, 4};
    waterPso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;
    waterPso.NumRenderTargets = 1; waterPso.RTVFormats[0] = backBufferFmt;
    waterPso.DSVFormat = m_gBuffer->GetDepthStencilFormat(); waterPso.SampleDesc = {1, 0};
    dx12::ThrowIfFailed(device->CreateGraphicsPipelineState(&waterPso, IID_PPV_ARGS(&m_waterPSO)), "Water PSO");
    return true;
}

bool DeferredScene::BuildSceneGeometry(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                                        std::vector<ComPtr<ID3D12Resource>>& uploads)
{
    MeshData model;
    if (!LoadObj(m_options.MeshPath, model))
        throw std::runtime_error("Failed to load deferred scene mesh");

    const uint32_t nD = (uint32_t)model.DiffusePaths.size();
    const uint32_t nN = (uint32_t)model.NormalPaths.size();
    const uint32_t nDisp = (uint32_t)model.DisplacementPaths.size();
    const uint32_t total = nD + nN + nDisp;

    m_srvStride = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

    D3D12_DESCRIPTOR_HEAP_DESC hd{};
    hd.NumDescriptors = total; hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    hd.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    dx12::ThrowIfFailed(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_textureHeap)), "tex heap");
    m_textures.resize(total);

    auto createTex = [&](uint32_t slot, uint32_t w, uint32_t h) {
        m_textures[slot] = dx12::CreateTexture2D(
            device,
            w,
            h,
            DXGI_FORMAT_B8G8R8A8_UNORM,
            D3D12_RESOURCE_STATE_COPY_DEST);
    };

    auto loadGroup = [](const std::vector<std::string>& paths) {
        std::vector<Image> imgs(paths.size()); std::vector<bool> ok(paths.size(), false);
        for (size_t i = 1; i < paths.size(); ++i) ok[i] = LoadImage(paths[i], imgs[i]);
        return std::make_pair(imgs, ok);
    };

    auto [diffImgs, diffOk] = loadGroup(model.DiffusePaths);
    auto [normImgs, normOk] = loadGroup(model.NormalPaths);
    auto [dispImgs, dispOk] = loadGroup(model.DisplacementPaths);

    const Image white{1, 1, {255, 255, 255, 255}};
    const Image flatN{1, 1, {128, 128, 255, 255}};
    const Image flatD{1, 1, {128, 128, 128, 255}};

    auto uploadGroup = [&](uint32_t base, uint32_t count, const Image& fallback,
                           const std::vector<Image>& imgs, const std::vector<bool>& ok) {
        createTex(base, fallback.Width, fallback.Height);
        dx12::UploadTexture2D(device, cmdList, m_textures[base].Get(), fallback, uploads);
        for (uint32_t i = 1; i < count; ++i)
        {
            if (ok[i]) { createTex(base + i, imgs[i].Width, imgs[i].Height); dx12::UploadTexture2D(device, cmdList, m_textures[base + i].Get(), imgs[i], uploads); }
            else m_textures[base + i] = m_textures[base];
        }
    };

    uploadGroup(0, nD, white, diffImgs, diffOk);
    uploadGroup(nD, nN, flatN, normImgs, normOk);
    uploadGroup(nD + nN, nDisp, flatD, dispImgs, dispOk);

    D3D12_SHADER_RESOURCE_VIEW_DESC sd{};
    sd.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    sd.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    sd.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D; sd.Texture2D.MipLevels = 1;
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_textureHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < total; ++i)
    {
        device->CreateShaderResourceView(m_textures[i].Get(), &sd, h);
        h.ptr += m_srvStride;
    }

    m_drawItems.clear();
    for (const SubMesh& sm : model.SubMeshes)
    {
        DrawItem item;
        item.IndexCount = sm.IndexCount;
        item.StartIndexLocation = sm.IndexStart;
        item.TextureIndex = sm.DiffuseTexIndex;
        item.NormalTextureIndex = nD + sm.NormalTexIndex;
        item.DisplacementTextureIndex = nD + nN + sm.DisplacementTexIndex;

        float ksAvg = (sm.Material.Ks.x + sm.Material.Ks.y + sm.Material.Ks.z) / 3.f;
        item.Material.BaseColor = {sm.Material.Kd.x, sm.Material.Kd.y, sm.Material.Kd.z, 1.f};
        item.Material.SurfaceParams = {std::max(0.04f, ksAvg), std::clamp(sm.Material.Ns, 8.f, 128.f), 0.f, 0.f};
        m_drawItems.push_back(item);
    }

    const UINT64 vbSz = model.Vertices.size() * sizeof(MeshVertex);
    const UINT64 ibSz = model.Indices.size() * sizeof(uint32_t);
    ComPtr<ID3D12Resource> vbUp, ibUp;
    m_vertexBuffer = dx12::CreateDefaultBuffer(device, cmdList, model.Vertices.data(), vbSz, vbUp);
    m_indexBuffer = dx12::CreateDefaultBuffer(device, cmdList, model.Indices.data(), ibSz, ibUp);
    uploads.push_back(vbUp); uploads.push_back(ibUp);

    m_vbv = {m_vertexBuffer->GetGPUVirtualAddress(), (UINT)vbSz, sizeof(MeshVertex)};
    m_ibv = {m_indexBuffer->GetGPUVirtualAddress(), (UINT)ibSz, DXGI_FORMAT_R32_UINT};
    return true;
}

bool DeferredScene::BuildWaterGeometry(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                                        std::vector<ComPtr<ID3D12Resource>>& uploads)
{
    const float half = 30.f, y = 1.5f;
    const MeshVertex verts[4] = {
        {{-half, y, -half}, {0,1,0}, {0,0}, {1,0,0}},
        {{ half, y, -half}, {0,1,0}, {1,0}, {1,0,0}},
        {{-half, y,  half}, {0,1,0}, {0,1}, {1,0,0}},
        {{ half, y,  half}, {0,1,0}, {1,1}, {1,0,0}},
    };
    const uint32_t indices[6] = {0,1,2, 1,3,2};
    m_waterIndexCount = 6;

    ComPtr<ID3D12Resource> vbUp, ibUp;
    m_waterVertexBuffer = dx12::CreateDefaultBuffer(device, cmdList, verts, sizeof(verts), vbUp);
    m_waterIndexBuffer = dx12::CreateDefaultBuffer(device, cmdList, indices, sizeof(indices), ibUp);
    uploads.push_back(vbUp); uploads.push_back(ibUp);

    m_waterVBV = {m_waterVertexBuffer->GetGPUVirtualAddress(), sizeof(verts), sizeof(MeshVertex)};
    m_waterIBV = {m_waterIndexBuffer->GetGPUVirtualAddress(), sizeof(indices), DXGI_FORMAT_R32_UINT};
    return true;
}

bool DeferredScene::BuildConstantBuffers(ID3D12Device* device)
{
    auto upHP = dx12::HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
    D3D12_RANGE rr{0, 0};

    auto mkCB = [&](uint32_t sz, ComPtr<ID3D12Resource>& buf, uint8_t*& ptr) -> bool {
        auto d = dx12::BufferDesc(dx12::AlignConstantBufferSize(sz));
        dx12::ThrowIfFailed(device->CreateCommittedResource(&upHP, D3D12_HEAP_FLAG_NONE, &d,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&buf)), "CB");
        return SUCCEEDED(buf->Map(0, &rr, reinterpret_cast<void**>(&ptr)));
    };

    return mkCB(sizeof(PassConstants), m_passCB, m_mappedPassCB)
        && mkCB(sizeof(LightConstants), m_lightCB, m_mappedLightCB);
}

void DeferredScene::UpdatePassConstants(uint32_t width, uint32_t height)
{
    if (!m_mappedPassCB) return;

    XMMATRIX vp = XMLoadFloat4x4(&m_view) * XMLoadFloat4x4(&m_proj);
    XMMATRIX ivp = XMMatrixInverse(nullptr, vp);

    PassConstants cb{};
    XMStoreFloat4x4(&cb.World, XMMatrixTranspose(XMMatrixIdentity()));
    XMStoreFloat4x4(&cb.ViewProj, XMMatrixTranspose(vp));
    XMStoreFloat4x4(&cb.InvViewProj, XMMatrixTranspose(ivp));
    cb.EyePosW = {m_eye.x, m_eye.y, m_eye.z, 1.f};
    cb.RenderTargetSize = {(float)width, (float)height, 1.f / width, 1.f / height};
    cb.TessParams = {1.f, 6.f, 0.5f, 15.f};
    cb.DispParams = {0.3f, 0.f, (float)EffectiveRenderMode(), m_time};
    const float uvOffsetX = std::fmod(m_time * m_options.UvScrollRate.x, 1.f);
    const float uvOffsetY = std::fmod(m_time * m_options.UvScrollRate.y, 1.f);
    cb.UvOffsetTiling = {uvOffsetX, uvOffsetY, m_options.UvTiling.x, m_options.UvTiling.y};
    std::memcpy(m_mappedPassCB, &cb, sizeof(cb));
}

void DeferredScene::UpdateLightConstants(float dt)
{
    if (!m_mappedLightCB) return;
    m_time += dt;

    LightConstants cb{};
    cb.AmbientColor = {m_options.AmbientColor.x, m_options.AmbientColor.y, m_options.AmbientColor.z, 1.f};

    uint32_t lightCount = 0;
    for (const SceneLight& light : m_options.Lights)
    {
        if (lightCount >= MaxLights) break;

        GpuLight& gpu = cb.Lights[lightCount++];
        gpu.ColorIntensity = {light.Color.x, light.Color.y, light.Color.z, light.Intensity};

        switch (light.LightType)
        {
        case SceneLight::Type::Directional:
        {
            const XMFLOAT3 dir = NormalizeOrFallback(light.Direction, XMFLOAT3{0.f, -1.f, 0.f});
            gpu.DirectionSpot = {dir.x, dir.y, dir.z, 0.f};
            gpu.Params = {0.f, 0.f, 0.f, 0.f};
            break;
        }
        case SceneLight::Type::Point:
            gpu.PositionRange = {light.Position.x, light.Position.y, light.Position.z, light.Range};
            gpu.Params = {1.f, 0.f, 0.f, 0.f};
            break;
        case SceneLight::Type::Spot:
        {
            const XMFLOAT3 dir = NormalizeOrFallback(light.Direction, XMFLOAT3{0.f, -1.f, 0.f});
            const float innerAngle = XMConvertToRadians(light.InnerConeDegrees);
            const float outerAngle = XMConvertToRadians(light.OuterConeDegrees);
            gpu.PositionRange = {light.Position.x, light.Position.y, light.Position.z, light.Range};
            gpu.DirectionSpot = {dir.x, dir.y, dir.z, std::cos(outerAngle)};
            gpu.Params = {2.f, std::cos(innerAngle), 0.f, 0.f};
            break;
        }
        }
    }

    cb.LightCount = {(float)lightCount, 0.f, 0.f, 0.f};
    std::memcpy(m_mappedLightCB, &cb, sizeof(cb));
}

int DeferredScene::EffectiveRenderMode() const
{
    int mode = m_renderMode;
    if (!m_options.EnableNormalMapping) mode &= ~1;
    if (!m_options.EnableDisplacement) mode &= ~2;
    return mode;
}
