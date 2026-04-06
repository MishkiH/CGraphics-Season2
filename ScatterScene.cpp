#include "ScatterScene.h"
#include <stdexcept>
#include <cstring>
#include <algorithm>
#include <cctype>
#include <fstream>
#include <wincodec.h>
#include <objbase.h>

using Microsoft::WRL::ComPtr;
using namespace DirectX;

static const char* kScatterShaders = R"hlsl(
cbuffer SceneCB : register(b0)
{
    row_major float4x4 gViewProj;
    float4             gEyePos;
};

cbuffer ObjectCB : register(b1)
{
    row_major float4x4 gWorld;
};

Texture2D    gDiffuse : register(t0);
SamplerState gSampler : register(s0);

struct VSIn
{
    float3 Pos     : POSITION;
    float3 Normal  : NORMAL;
    float2 TexC    : TEXCOORD;
    float3 Tangent : TANGENT;
};

struct VSOut
{
    float4 PosH    : SV_POSITION;
    float3 PosW    : POSITION;
    float3 NormalW : NORMAL;
    float2 TexC    : TEXCOORD;
};

VSOut VS(VSIn vin)
{
    VSOut vout;
    float4 posW = mul(float4(vin.Pos, 1.0), gWorld);
    vout.PosH = mul(posW, gViewProj);
    vout.PosW = posW.xyz;
    vout.NormalW = normalize(mul(vin.Normal, (float3x3)gWorld));
    vout.TexC = vin.TexC;
    return vout;
}

float4 PS(VSOut pin) : SV_Target
{
    float3 albedo = gDiffuse.Sample(gSampler, pin.TexC).rgb;

    float3 N = normalize(pin.NormalW);
    float3 L = normalize(float3(0.5, 1.0, 0.4));
    float3 V = normalize(gEyePos.xyz - pin.PosW);
    float3 H = normalize(L + V);

    float diff = saturate(dot(N, L));
    float spec = pow(saturate(dot(N, H)), 32.0) * 0.15;

    float3 color = albedo * (0.15 + diff * 0.85) + spec;
    return float4(color, 1.0);
}
)hlsl";

namespace
{
    void ThrowIfFailed(HRESULT hr, const char* msg)
    {
        if (FAILED(hr))
        {
            char buf[256];
            std::snprintf(buf, sizeof(buf), "%s (hr=0x%08X)", msg, (unsigned)hr);
            throw std::runtime_error(buf);
        }
    }

    D3D12_HEAP_PROPERTIES HeapProps(D3D12_HEAP_TYPE type)
    {
        D3D12_HEAP_PROPERTIES p{};
        p.Type = type;
        p.CPUPageProperty = D3D12_CPU_PAGE_PROPERTY_UNKNOWN;
        p.MemoryPoolPreference = D3D12_MEMORY_POOL_UNKNOWN;
        p.CreationNodeMask = 1;
        p.VisibleNodeMask = 1;
        return p;
    }
}

bool ScatterScene::LoadTga(const std::string& path, Image& out)
{
    std::ifstream f(path, std::ios::binary);
    if (!f) return false;

    uint8_t hdr[18]{};
    f.read(reinterpret_cast<char*>(hdr), 18);
    if (!f) return false;

    const uint8_t  idLen = hdr[0];
    const uint8_t  cmType = hdr[1];
    const uint8_t  imgType = hdr[2];
    const uint16_t w = hdr[12] | (hdr[13] << 8);
    const uint16_t h = hdr[14] | (hdr[15] << 8);
    const uint8_t  bpp = hdr[16];
    const uint8_t  desc = hdr[17];

    if (cmType || !w || !h) return false;
    if (bpp != 24 && bpp != 32) return false;
    if (imgType != 2 && imgType != 10) return false;
    if (idLen) f.seekg(idLen, std::ios::cur);

    const uint32_t Bpp = bpp / 8;
    const uint32_t px = w * h;
    out.width = w; out.height = h;
    out.bgra.assign(px * 4u, 255u);

    auto write = [&](uint32_t idx, const uint8_t* p)
    {
        uint8_t* o = out.bgra.data() + idx * 4u;
        o[0] = p[0]; o[1] = p[1]; o[2] = p[2];
        o[3] = (Bpp == 4) ? p[3] : 255u;
    };

    std::vector<uint8_t> tmp(Bpp);
    if (imgType == 2)
    {
        std::vector<uint8_t> raw(px * Bpp);
        f.read(reinterpret_cast<char*>(raw.data()), raw.size());
        if (!f) return false;
        for (uint32_t i = 0; i < px; ++i) write(i, &raw[i * Bpp]);
    }
    else
    {
        for (uint32_t i = 0; i < px;)
        {
            uint8_t pkt = 0;
            f.read(reinterpret_cast<char*>(&pkt), 1);
            if (!f) return false;
            const uint32_t cnt = (pkt & 0x7Fu) + 1u;
            if (pkt & 0x80u)
            {
                f.read(reinterpret_cast<char*>(tmp.data()), Bpp);
                if (!f) return false;
                for (uint32_t k = 0; k < cnt && i < px; ++k, ++i) write(i, tmp.data());
            }
            else
            {
                for (uint32_t k = 0; k < cnt && i < px; ++k, ++i)
                {
                    f.read(reinterpret_cast<char*>(tmp.data()), Bpp);
                    if (!f) return false;
                    write(i, tmp.data());
                }
            }
        }
    }

    if (!(desc & 0x20u))
    {
        const uint32_t row = w * 4u;
        std::vector<uint8_t> tmp2(row);
        for (uint32_t y = 0; y < h / 2; ++y)
        {
            uint8_t* top = out.bgra.data() + y * row;
            uint8_t* bot = out.bgra.data() + (h - 1 - y) * row;
            std::memcpy(tmp2.data(), top, row);
            std::memcpy(top, bot, row);
            std::memcpy(bot, tmp2.data(), row);
        }
    }
    return true;
}

bool ScatterScene::LoadWic(const std::string& path, Image& out)
{
    static bool comInit = false;
    if (!comInit) { CoInitializeEx(nullptr, COINIT_MULTITHREADED); comInit = true; }

    std::wstring wpath(path.begin(), path.end());

    ComPtr<IWICImagingFactory> factory;
    if (FAILED(CoCreateInstance(CLSID_WICImagingFactory2, nullptr, CLSCTX_INPROC_SERVER,
                                IID_PPV_ARGS(&factory))))
        if (FAILED(CoCreateInstance(CLSID_WICImagingFactory, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(&factory))))
            return false;

    ComPtr<IWICBitmapDecoder> decoder;
    if (FAILED(factory->CreateDecoderFromFilename(wpath.c_str(), nullptr, GENERIC_READ,
                WICDecodeMetadataCacheOnDemand, &decoder))) return false;

    ComPtr<IWICBitmapFrameDecode> frame;
    if (FAILED(decoder->GetFrame(0, &frame))) return false;

    UINT w = 0, h = 0;
    frame->GetSize(&w, &h);
    if (!w || !h) return false;

    ComPtr<IWICFormatConverter> conv;
    if (FAILED(factory->CreateFormatConverter(&conv))) return false;
    if (FAILED(conv->Initialize(frame.Get(), GUID_WICPixelFormat32bppBGRA,
                WICBitmapDitherTypeNone, nullptr, 0.f, WICBitmapPaletteTypeCustom))) return false;

    out.width = w; out.height = h;
    out.bgra.resize(w * h * 4u);
    return SUCCEEDED(conv->CopyPixels(nullptr, w * 4u, (UINT)out.bgra.size(), out.bgra.data()));
}

bool ScatterScene::LoadImage(const std::string& path, Image& out)
{
    if (path.size() < 4) return false;
    std::string ext = path.substr(path.size() - 4);
    std::transform(ext.begin(), ext.end(), ext.begin(),
                   [](unsigned char c){ return (char)std::tolower(c); });
    return ext == ".tga" ? LoadTga(path, out) : LoadWic(path, out);
}

bool ScatterScene::Initialize(ID3D12Device*       device,
                               ID3D12CommandQueue* cmdQueue,
                               DXGI_FORMAT         backBufferFmt,
                               uint32_t            width,
                               uint32_t            height,
                               const std::string&  shrekPath,
                               const std::string&  donkeyPath)
{
    if (!m_scene.Initialize(shrekPath, donkeyPath))
        return false;

    m_scene.BuildOctree();

    if (!BuildShaders())                    return false;
    if (!BuildRootSignature(device))        return false;
    if (!BuildPSO(device, backBufferFmt))   return false;
    if (!BuildMeshBuffers(device, cmdQueue)) return false;
    if (!BuildDepthBuffer(device, width, height)) return false;
    if (!BuildSceneCB(device))              return false;

    return true;
}

void ScatterScene::Shutdown()
{
    if (m_mappedSceneCB)
    {
        m_sceneCB->Unmap(0, nullptr);
        m_mappedSceneCB = nullptr;
    }
}

void ScatterScene::OnResize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    m_depthBuffer.Reset();
    BuildDepthBuffer(device, width, height);
}

void ScatterScene::Draw(ID3D12GraphicsCommandList*  cmdList,
                         const XMFLOAT4X4&           viewProj,
                         const XMFLOAT3&             eyePos,
                         D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                         D3D12_VIEWPORT              viewport,
                         D3D12_RECT                  scissorRect)
{
    SceneCBData cbData{};
    cbData.ViewProj = viewProj;
    cbData.EyePos = {eyePos.x, eyePos.y, eyePos.z, 1.f};
    std::memcpy(m_mappedSceneCB, &cbData, sizeof(cbData));

    std::vector<uint32_t> visible;
    m_scene.GetVisibleIndices(viewProj, m_useFrustum, m_useOctree, visible);
    m_lastVisible = (uint32_t)visible.size();

    D3D12_CPU_DESCRIPTOR_HANDLE dsv = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
    cmdList->OMSetRenderTargets(1, &backBufferRtv, FALSE, &dsv);
    cmdList->ClearDepthStencilView(dsv, D3D12_CLEAR_FLAG_DEPTH, 1.f, 0, 0, nullptr);

    cmdList->RSSetViewports(1, &viewport);
    cmdList->RSSetScissorRects(1, &scissorRect);
    cmdList->SetPipelineState(m_pso.Get());
    cmdList->SetGraphicsRootSignature(m_rootSig.Get());
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cmdList->SetGraphicsRootConstantBufferView(0, m_sceneCB->GetGPUVirtualAddress());

    const auto& instances = m_scene.GetInstances();
    uint32_t    prevMesh = UINT32_MAX;

    for (uint32_t idx : visible)
    {
        const SceneInstance& inst = instances[idx];
        const uint32_t mi = inst.MeshIndex;
        const MeshGpu& gpu = m_meshes[mi];
        const MeshData& mesh = m_scene.GetMesh(mi);

        if (mi != prevMesh)
        {
            cmdList->IASetVertexBuffers(0, 1, &gpu.VBV);
            cmdList->IASetIndexBuffer(&gpu.IBV);

            ID3D12DescriptorHeap* heaps[] = {gpu.SrvHeap.Get()};
            cmdList->SetDescriptorHeaps(1, heaps);

            prevMesh = mi;
        }

        cmdList->SetGraphicsRoot32BitConstants(1, 16, &inst.World, 0);

        for (const SubMesh& sm : mesh.SubMeshes)
        {
            D3D12_GPU_DESCRIPTOR_HANDLE srv =
                gpu.SrvHeap->GetGPUDescriptorHandleForHeapStart();
            srv.ptr += (UINT64)sm.DiffuseTextureIndex * gpu.SrvDescriptorSize;

            cmdList->SetGraphicsRootDescriptorTable(2, srv);
            cmdList->DrawIndexedInstanced(sm.IndexCount, 1, sm.IndexStart, 0, 0);
        }
    }
}

bool ScatterScene::BuildShaders()
{
    ComPtr<ID3DBlob> errors;
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif
    const size_t srcLen = strlen(kScatterShaders);

    if (FAILED(D3DCompile(kScatterShaders, srcLen, nullptr, nullptr, nullptr,
                           "VS", "vs_5_0", flags, 0, &m_vs, &errors)))
        return false;

    return SUCCEEDED(D3DCompile(kScatterShaders, srcLen, nullptr, nullptr, nullptr,
                                "PS", "ps_5_0", flags, 0, &m_ps, &errors));
}

bool ScatterScene::BuildRootSignature(ID3D12Device* device)
{
    D3D12_DESCRIPTOR_RANGE texRange{};
    texRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    texRange.NumDescriptors = 1;
    texRange.BaseShaderRegister = 0;
    texRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_ROOT_PARAMETER params[3]{};

    params[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    params[0].Descriptor.ShaderRegister = 0;
    params[0].Descriptor.RegisterSpace = 0;
    params[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    params[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    params[1].Constants.ShaderRegister = 1;
    params[1].Constants.RegisterSpace = 0;
    params[1].Constants.Num32BitValues = 16;
    params[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX;

    params[2].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[2].DescriptorTable.NumDescriptorRanges = 1;
    params[2].DescriptorTable.pDescriptorRanges = &texRange;
    params[2].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_STATIC_SAMPLER_DESC sampler{};
    sampler.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    sampler.AddressU =
    sampler.AddressV =
    sampler.AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    sampler.MaxAnisotropy = 1;
    sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    sampler.MaxLOD = D3D12_FLOAT32_MAX;
    sampler.ShaderRegister = 0;
    sampler.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_ROOT_SIGNATURE_DESC desc{};
    desc.NumParameters = 3;
    desc.pParameters = params;
    desc.NumStaticSamplers = 1;
    desc.pStaticSamplers = &sampler;
    desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> blob, error;
    if (FAILED(D3D12SerializeRootSignature(&desc, D3D_ROOT_SIGNATURE_VERSION_1,
                                           &blob, &error)))
        return false;

    return SUCCEEDED(device->CreateRootSignature(0, blob->GetBufferPointer(),
                                                 blob->GetBufferSize(),
                                                 IID_PPV_ARGS(&m_rootSig)));
}

bool ScatterScene::BuildPSO(ID3D12Device* device, DXGI_FORMAT backBufferFmt)
{
    D3D12_INPUT_ELEMENT_DESC layout[] = {
        {"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0,  0, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"NORMAL",   0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT,    0, 24, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
        {"TANGENT",  0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 32, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
    };

    D3D12_RASTERIZER_DESC raster{};
    raster.FillMode = D3D12_FILL_MODE_SOLID;
    raster.CullMode = D3D12_CULL_MODE_BACK;
    raster.DepthClipEnable = TRUE;

    D3D12_BLEND_DESC blend{};
    blend.RenderTarget[0].RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;

    D3D12_DEPTH_STENCIL_DESC depth{};
    depth.DepthEnable = TRUE;
    depth.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    depth.DepthFunc = D3D12_COMPARISON_FUNC_LESS;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};
    desc.pRootSignature = m_rootSig.Get();
    desc.VS = {m_vs->GetBufferPointer(), m_vs->GetBufferSize()};
    desc.PS = {m_ps->GetBufferPointer(), m_ps->GetBufferSize()};
    desc.InputLayout = {layout, _countof(layout)};
    desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    desc.NumRenderTargets = 1;
    desc.RTVFormats[0] = backBufferFmt;
    desc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
    desc.SampleDesc.Count = 1;
    desc.RasterizerState = raster;
    desc.BlendState = blend;
    desc.DepthStencilState = depth;
    desc.SampleMask = UINT_MAX;

    return SUCCEEDED(device->CreateGraphicsPipelineState(&desc, IID_PPV_ARGS(&m_pso)));
}

bool ScatterScene::BuildMeshBuffers(ID3D12Device*       device,
                                     ID3D12CommandQueue* cmdQueue)
{
    ComPtr<ID3D12CommandAllocator>    alloc;
    ComPtr<ID3D12GraphicsCommandList> list;
    ComPtr<ID3D12Fence>               fence;

    ThrowIfFailed(device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT,
                    IID_PPV_ARGS(&alloc)), "scatter upload alloc");
    ThrowIfFailed(device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
                    alloc.Get(), nullptr, IID_PPV_ARGS(&list)), "scatter upload list");
    ThrowIfFailed(device->CreateFence(0, D3D12_FENCE_FLAG_NONE,
                    IID_PPV_ARGS(&fence)), "scatter fence");

    std::vector<ComPtr<ID3D12Resource>> uploads;

    for (uint32_t m = 0; m < SceneObjectManager::MeshCount; ++m)
        UploadMeshGpu(device, list.Get(), m_meshes[m], m_scene.GetMesh(m), uploads);

    ThrowIfFailed(list->Close(), "scatter upload close");
    ID3D12CommandList* lists[] = {list.Get()};
    cmdQueue->ExecuteCommandLists(1, lists);
    ThrowIfFailed(cmdQueue->Signal(fence.Get(), 1), "scatter signal");
    HANDLE evt = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    fence->SetEventOnCompletion(1, evt);
    WaitForSingleObject(evt, INFINITE);
    CloseHandle(evt);

    return true;
}

void ScatterScene::UploadMeshGpu(ID3D12Device*                             device,
                                  ID3D12GraphicsCommandList*                cmdList,
                                  MeshGpu&                                  gpu,
                                  const MeshData&                           mesh,
                                  std::vector<ComPtr<ID3D12Resource>>&      uploads)
{
    const uint64_t vbSize = mesh.Vertices.size() * sizeof(MeshVertex);
    const uint64_t ibSize = mesh.Indices.size()  * sizeof(uint32_t);

    ComPtr<ID3D12Resource> vbUp, ibUp;
    gpu.VertexBuffer = CreateGpuBuffer(device, cmdList, mesh.Vertices.data(), vbSize, vbUp);
    gpu.IndexBuffer = CreateGpuBuffer(device, cmdList, mesh.Indices.data(),  ibSize, ibUp);
    uploads.push_back(vbUp);
    uploads.push_back(ibUp);

    gpu.VBV = {gpu.VertexBuffer->GetGPUVirtualAddress(), (UINT)vbSize, sizeof(MeshVertex)};
    gpu.IBV = {gpu.IndexBuffer->GetGPUVirtualAddress(),  (UINT)ibSize, DXGI_FORMAT_R32_UINT};

    const uint32_t texCount = (uint32_t)mesh.TexturePaths.size();

    D3D12_DESCRIPTOR_HEAP_DESC hd{};
    hd.NumDescriptors = texCount;
    hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    hd.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    ThrowIfFailed(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&gpu.SrvHeap)),
                  "scatter SRV heap");

    gpu.SrvDescriptorSize = device->GetDescriptorHandleIncrementSize(
        D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    gpu.Textures.resize(texCount);

    auto defaultHP = HeapProps(D3D12_HEAP_TYPE_DEFAULT);
    auto uploadHP = HeapProps(D3D12_HEAP_TYPE_UPLOAD);

    auto UploadTex = [&](uint32_t slot, const Image& img)
    {
        D3D12_RESOURCE_DESC td{};
        td.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        td.Width = img.width;
        td.Height = img.height;
        td.DepthOrArraySize = 1;
        td.MipLevels = 1;
        td.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        td.SampleDesc.Count = 1;
        td.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;

        ThrowIfFailed(
            device->CreateCommittedResource(&defaultHP, D3D12_HEAP_FLAG_NONE, &td,
                D3D12_RESOURCE_STATE_COPY_DEST, nullptr,
                IID_PPV_ARGS(&gpu.Textures[slot])),
            "scatter tex");

        D3D12_PLACED_SUBRESOURCE_FOOTPRINT fp{};
        UINT64 totalBytes = 0;
        device->GetCopyableFootprints(&td, 0, 1, 0, &fp, nullptr, nullptr, &totalBytes);

        D3D12_RESOURCE_DESC bd{};
        bd.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
        bd.Width = totalBytes;
        bd.Height = 1;
        bd.DepthOrArraySize = 1;
        bd.MipLevels = 1;
        bd.SampleDesc.Count = 1;
        bd.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

        ComPtr<ID3D12Resource> up;
        ThrowIfFailed(
            device->CreateCommittedResource(&uploadHP, D3D12_HEAP_FLAG_NONE, &bd,
                D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&up)),
            "scatter tex upload");

        void* mapped = nullptr;
        D3D12_RANGE rr{0, 0};
        up->Map(0, &rr, &mapped);
        const uint32_t srcPitch = img.width * 4u;
        const uint32_t dstPitch = fp.Footprint.RowPitch;
        for (uint32_t y = 0; y < img.height; ++y)
            std::memcpy((uint8_t*)mapped + y * dstPitch,
                        img.bgra.data() + y * srcPitch, srcPitch);
        up->Unmap(0, nullptr);

        D3D12_TEXTURE_COPY_LOCATION dst{gpu.Textures[slot].Get(),
                                        D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX};
        dst.SubresourceIndex = 0;
        D3D12_TEXTURE_COPY_LOCATION src{up.Get(),
                                        D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT};
        src.PlacedFootprint = fp;
        cmdList->CopyTextureRegion(&dst, 0, 0, 0, &src, nullptr);

        D3D12_RESOURCE_BARRIER b{};
        b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        b.Transition.pResource = gpu.Textures[slot].Get();
        b.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
        b.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
        b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &b);

        uploads.push_back(up);
    };

    {
        Image white{1, 1, {255, 255, 255, 255}};
        UploadTex(0, white);
    }
    for (uint32_t i = 1; i < texCount; ++i)
    {
        Image img;
        if (LoadImage(mesh.TexturePaths[i], img))
            UploadTex(i, img);
        else
            gpu.Textures[i] = gpu.Textures[0];
    }

    D3D12_CPU_DESCRIPTOR_HANDLE srv = gpu.SrvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < texCount; ++i)
    {
        D3D12_SHADER_RESOURCE_VIEW_DESC sd{};
        sd.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
        sd.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        sd.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
        sd.Texture2D.MipLevels = 1;
        device->CreateShaderResourceView(gpu.Textures[i].Get(), &sd, srv);
        srv.ptr += gpu.SrvDescriptorSize;
    }
}

bool ScatterScene::BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!m_dsvHeap)
    {
        D3D12_DESCRIPTOR_HEAP_DESC hd{};
        hd.NumDescriptors = 1;
        hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
        if (FAILED(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_dsvHeap))))
            return false;
    }

    D3D12_HEAP_PROPERTIES hp = HeapProps(D3D12_HEAP_TYPE_DEFAULT);

    D3D12_RESOURCE_DESC rd{};
    rd.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    rd.Width = width;
    rd.Height = height;
    rd.DepthOrArraySize = 1;
    rd.MipLevels = 1;
    rd.Format = DXGI_FORMAT_D32_FLOAT;
    rd.SampleDesc.Count = 1;
    rd.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;

    D3D12_CLEAR_VALUE cv{};
    cv.Format = DXGI_FORMAT_D32_FLOAT;
    cv.DepthStencil.Depth = 1.f;

    if (FAILED(device->CreateCommittedResource(&hp, D3D12_HEAP_FLAG_NONE, &rd,
                D3D12_RESOURCE_STATE_DEPTH_WRITE, &cv, IID_PPV_ARGS(&m_depthBuffer))))
        return false;

    device->CreateDepthStencilView(m_depthBuffer.Get(), nullptr,
        m_dsvHeap->GetCPUDescriptorHandleForHeapStart());
    return true;
}

bool ScatterScene::BuildSceneCB(ID3D12Device* device)
{
    D3D12_HEAP_PROPERTIES hp = HeapProps(D3D12_HEAP_TYPE_UPLOAD);

    D3D12_RESOURCE_DESC rd{};
    rd.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    rd.Width = 256;
    rd.Height = 1;
    rd.DepthOrArraySize = 1;
    rd.MipLevels = 1;
    rd.SampleDesc.Count = 1;
    rd.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    if (FAILED(device->CreateCommittedResource(&hp, D3D12_HEAP_FLAG_NONE, &rd,
                D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&m_sceneCB))))
        return false;

    D3D12_RANGE readRange{0, 0};
    m_sceneCB->Map(0, &readRange, reinterpret_cast<void**>(&m_mappedSceneCB));
    return true;
}

ComPtr<ID3D12Resource> ScatterScene::CreateGpuBuffer(
    ID3D12Device*               device,
    ID3D12GraphicsCommandList*  cmdList,
    const void*                 data,
    uint64_t                    byteSize,
    ComPtr<ID3D12Resource>&     uploadBuffer)
{
    auto defaultHP = HeapProps(D3D12_HEAP_TYPE_DEFAULT);
    auto uploadHP = HeapProps(D3D12_HEAP_TYPE_UPLOAD);

    D3D12_RESOURCE_DESC bd{};
    bd.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    bd.Width = byteSize;
    bd.Height = 1;
    bd.DepthOrArraySize = 1;
    bd.MipLevels = 1;
    bd.SampleDesc.Count = 1;
    bd.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    ComPtr<ID3D12Resource> gpuBuf;
    device->CreateCommittedResource(&defaultHP, D3D12_HEAP_FLAG_NONE, &bd,
        D3D12_RESOURCE_STATE_COPY_DEST, nullptr, IID_PPV_ARGS(&gpuBuf));
    device->CreateCommittedResource(&uploadHP, D3D12_HEAP_FLAG_NONE, &bd,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&uploadBuffer));

    void* mapped = nullptr;
    D3D12_RANGE rr{0, 0};
    uploadBuffer->Map(0, &rr, &mapped);
    std::memcpy(mapped, data, byteSize);
    uploadBuffer->Unmap(0, nullptr);

    cmdList->CopyBufferRegion(gpuBuf.Get(), 0, uploadBuffer.Get(), 0, byteSize);

    D3D12_RESOURCE_BARRIER barrier{};
    barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    barrier.Transition.pResource = gpuBuf.Get();
    barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
    barrier.Transition.StateAfter = D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER
                                   | D3D12_RESOURCE_STATE_INDEX_BUFFER;
    barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    cmdList->ResourceBarrier(1, &barrier);

    return gpuBuf;
}
