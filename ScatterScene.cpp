#include "ScatterScene.h"
#include "ImageLoader.h"
#include "AssetPath.h"
#include <stdexcept>
#include <cstring>

using Microsoft::WRL::ComPtr;
using namespace DirectX;

namespace
{
    void ThrowIfFailed(HRESULT hr, const char* msg)
    {
        if (FAILED(hr)) { char buf[256]; std::snprintf(buf, sizeof buf, "%s (hr=0x%08X)", msg, (unsigned)hr); throw std::runtime_error(buf); }
    }

    D3D12_HEAP_PROPERTIES HeapProps(D3D12_HEAP_TYPE type)
    {
        D3D12_HEAP_PROPERTIES p{};
        p.Type = type; p.CPUPageProperty = D3D12_CPU_PAGE_PROPERTY_UNKNOWN;
        p.MemoryPoolPreference = D3D12_MEMORY_POOL_UNKNOWN;
        p.CreationNodeMask = p.VisibleNodeMask = 1;
        return p;
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
    if (!BuildDepthBuffer(device, width, height)) return false;
    if (!BuildSceneCB(device)) return false;
    return true;
}

void ScatterScene::Shutdown()
{
    if (m_mappedSceneCB) { m_sceneCB->Unmap(0, nullptr); m_mappedSceneCB = nullptr; }
}

void ScatterScene::OnResize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    m_depthBuffer.Reset();
    BuildDepthBuffer(device, width, height);
}

void ScatterScene::RecordCommands(ID3D12GraphicsCommandList* cmdList,
                                   const XMFLOAT4X4& viewProj, const XMFLOAT3& eyePos,
                                   D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                                   D3D12_VIEWPORT viewport, D3D12_RECT scissorRect)
{
    SceneCBData cbData{viewProj, {eyePos.x, eyePos.y, eyePos.z, 1.f}};
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
    uint32_t prevMesh = UINT32_MAX;

    for (uint32_t idx : visible)
    {
        const SceneInstance& inst = instances[idx];
        uint32_t mi = inst.MeshIndex;
        const MeshGpu& gpu = m_meshes[mi];
        const MeshData& mesh = m_scene.GetMesh(mi);

        if (mi != prevMesh)
        {
            ID3D12DescriptorHeap* heaps[] = {gpu.SrvHeap.Get()};
            cmdList->SetDescriptorHeaps(1, heaps);
            cmdList->IASetVertexBuffers(0, 1, &gpu.VBV);
            cmdList->IASetIndexBuffer(&gpu.IBV);
            prevMesh = mi;
        }

        cmdList->SetGraphicsRoot32BitConstants(1, 16, &inst.World, 0);

        for (const SubMesh& sm : mesh.SubMeshes)
        {
            D3D12_GPU_DESCRIPTOR_HANDLE srv = gpu.SrvHeap->GetGPUDescriptorHandleForHeapStart();
            srv.ptr += (UINT64)sm.DiffuseTexIndex * gpu.SrvStride;
            cmdList->SetGraphicsRootDescriptorTable(2, srv);
            cmdList->DrawIndexedInstanced(sm.IndexCount, 1, sm.IndexStart, 0, 0);
        }
    }
}

bool ScatterScene::BuildShaders(ID3D12Device*)
{
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif
    std::wstring path = ToWide(ResolveAsset("ScatterShaders.hlsl"));
    ComPtr<ID3DBlob> errors;

    auto compile = [&](const char* entry, const char* target, ComPtr<ID3DBlob>& blob) -> bool {
        errors.Reset();
        HRESULT hr = D3DCompileFromFile(path.c_str(), nullptr, nullptr, entry, target, flags, 0, &blob, &errors);
        if (FAILED(hr)) { if (errors) throw std::runtime_error((const char*)errors->GetBufferPointer()); return false; }
        return true;
    };
    return compile("VS", "vs_5_0", m_vs) && compile("PS", "ps_5_0", m_ps);
}

bool ScatterScene::BuildRootSignature(ID3D12Device* device)
{
    D3D12_DESCRIPTOR_RANGE texRange{};
    texRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    texRange.NumDescriptors = 1;
    texRange.BaseShaderRegister = 0;
    texRange.RegisterSpace = 0;
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
    sampler.AddressU = sampler.AddressV = sampler.AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    sampler.MaxAnisotropy = 1; sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    sampler.MaxLOD = D3D12_FLOAT32_MAX; sampler.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_ROOT_SIGNATURE_DESC desc{};
    desc.NumParameters = 3; desc.pParameters = params;
    desc.NumStaticSamplers = 1; desc.pStaticSamplers = &sampler;
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
    return SUCCEEDED(device->CreateGraphicsPipelineState(&desc, IID_PPV_ARGS(&m_pso)));
}

bool ScatterScene::BuildMeshGpu(ID3D12Device* device, ID3D12CommandQueue* cmdQueue)
{
    ComPtr<ID3D12CommandAllocator> alloc;
    ComPtr<ID3D12GraphicsCommandList> list;
    ComPtr<ID3D12Fence> fence;
    ThrowIfFailed(device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&alloc)), "scatter alloc");
    ThrowIfFailed(device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, alloc.Get(), nullptr, IID_PPV_ARGS(&list)), "scatter list");
    ThrowIfFailed(device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&fence)), "scatter fence");

    std::vector<ComPtr<ID3D12Resource>> uploads;
    for (uint32_t m = 0; m < SceneObjectManager::MeshCount; ++m)
        UploadMesh(device, list.Get(), m_meshes[m], m_scene.GetMesh(m), uploads);

    ThrowIfFailed(list->Close(), "scatter upload close");
    ID3D12CommandList* ls[] = {list.Get()};
    cmdQueue->ExecuteCommandLists(1, ls);
    cmdQueue->Signal(fence.Get(), 1);
    HANDLE evt = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    fence->SetEventOnCompletion(1, evt);
    WaitForSingleObject(evt, INFINITE);
    CloseHandle(evt);
    return true;
}

void ScatterScene::UploadMesh(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                               MeshGpu& gpu, const MeshData& mesh,
                               std::vector<ComPtr<ID3D12Resource>>& uploads)
{
    uint64_t vbSz = mesh.Vertices.size() * sizeof(MeshVertex);
    uint64_t ibSz = mesh.Indices.size() * sizeof(uint32_t);

    ComPtr<ID3D12Resource> vbUp, ibUp;
    gpu.VertexBuffer = CreateGpuBuffer(device, cmdList, mesh.Vertices.data(), vbSz, vbUp);
    gpu.IndexBuffer = CreateGpuBuffer(device, cmdList, mesh.Indices.data(), ibSz, ibUp);
    uploads.push_back(vbUp); uploads.push_back(ibUp);
    gpu.VBV = {gpu.VertexBuffer->GetGPUVirtualAddress(), (UINT)vbSz, sizeof(MeshVertex)};
    gpu.IBV = {gpu.IndexBuffer->GetGPUVirtualAddress(), (UINT)ibSz, DXGI_FORMAT_R32_UINT};

    uint32_t texCount = (uint32_t)mesh.DiffusePaths.size();
    D3D12_DESCRIPTOR_HEAP_DESC hd{};
    hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    hd.NumDescriptors = texCount;
    hd.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    ThrowIfFailed(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&gpu.SrvHeap)), "scatter SRV heap");
    gpu.SrvStride = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    gpu.Textures.resize(texCount);

    auto defHP = HeapProps(D3D12_HEAP_TYPE_DEFAULT);
    auto upHP = HeapProps(D3D12_HEAP_TYPE_UPLOAD);

    auto uploadTex = [&](uint32_t slot, const Image& img) {
        D3D12_RESOURCE_DESC td{};
        td.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        td.Width = img.Width; td.Height = img.Height;
        td.DepthOrArraySize = td.MipLevels = 1;
        td.Format = DXGI_FORMAT_B8G8R8A8_UNORM; td.SampleDesc.Count = 1;

        ThrowIfFailed(device->CreateCommittedResource(&defHP, D3D12_HEAP_FLAG_NONE, &td,
            D3D12_RESOURCE_STATE_COPY_DEST, nullptr, IID_PPV_ARGS(&gpu.Textures[slot])), "scatter tex");

        D3D12_PLACED_SUBRESOURCE_FOOTPRINT fp{}; UINT64 totalBytes = 0;
        device->GetCopyableFootprints(&td, 0, 1, 0, &fp, nullptr, nullptr, &totalBytes);

        D3D12_RESOURCE_DESC bd{D3D12_RESOURCE_DIMENSION_BUFFER, 0, totalBytes, 1, 1, 1, DXGI_FORMAT_UNKNOWN, {1,0}, D3D12_TEXTURE_LAYOUT_ROW_MAJOR};
        ComPtr<ID3D12Resource> up;
        ThrowIfFailed(device->CreateCommittedResource(&upHP, D3D12_HEAP_FLAG_NONE, &bd,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&up)), "scatter tex up");

        void* mapped = nullptr; D3D12_RANGE rr{0, 0}; up->Map(0, &rr, &mapped);
        for (uint32_t y = 0; y < img.Height; ++y)
            std::memcpy((uint8_t*)mapped + y * fp.Footprint.RowPitch, img.BGRA.data() + y * img.Width * 4u, img.Width * 4u);
        up->Unmap(0, nullptr);

        D3D12_TEXTURE_COPY_LOCATION dst{gpu.Textures[slot].Get(), D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX};
        D3D12_TEXTURE_COPY_LOCATION src{up.Get(), D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT};
        src.PlacedFootprint = fp;
        cmdList->CopyTextureRegion(&dst, 0, 0, 0, &src, nullptr);

        D3D12_RESOURCE_BARRIER b{};
        b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        b.Transition.pResource = gpu.Textures[slot].Get();
        b.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
        b.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
        b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &b);
        uploads.push_back(std::move(up));
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

bool ScatterScene::BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!m_dsvHeap)
    {
        D3D12_DESCRIPTOR_HEAP_DESC hd{};
        hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
        hd.NumDescriptors = 1;
        if (FAILED(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_dsvHeap)))) return false;
    }
    auto hp = HeapProps(D3D12_HEAP_TYPE_DEFAULT);
    D3D12_RESOURCE_DESC rd{};
    rd.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D; rd.Width = width; rd.Height = height;
    rd.DepthOrArraySize = rd.MipLevels = 1; rd.Format = DXGI_FORMAT_D32_FLOAT;
    rd.SampleDesc.Count = 1; rd.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;
    D3D12_CLEAR_VALUE cv{DXGI_FORMAT_D32_FLOAT}; cv.DepthStencil.Depth = 1.f;
    if (FAILED(device->CreateCommittedResource(&hp, D3D12_HEAP_FLAG_NONE, &rd,
            D3D12_RESOURCE_STATE_DEPTH_WRITE, &cv, IID_PPV_ARGS(&m_depthBuffer)))) return false;
    device->CreateDepthStencilView(m_depthBuffer.Get(), nullptr, m_dsvHeap->GetCPUDescriptorHandleForHeapStart());
    return true;
}

bool ScatterScene::BuildSceneCB(ID3D12Device* device)
{
    auto hp = HeapProps(D3D12_HEAP_TYPE_UPLOAD);
    D3D12_RESOURCE_DESC rd{};
    rd.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; rd.Width = 256;
    rd.Height = rd.DepthOrArraySize = rd.MipLevels = 1;
    rd.SampleDesc.Count = 1; rd.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    if (FAILED(device->CreateCommittedResource(&hp, D3D12_HEAP_FLAG_NONE, &rd,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&m_sceneCB)))) return false;
    D3D12_RANGE rr{0, 0};
    m_sceneCB->Map(0, &rr, reinterpret_cast<void**>(&m_mappedSceneCB));
    return true;
}

ComPtr<ID3D12Resource> ScatterScene::CreateGpuBuffer(
    ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
    const void* data, uint64_t size, ComPtr<ID3D12Resource>& upload)
{
    auto defHP = HeapProps(D3D12_HEAP_TYPE_DEFAULT);
    auto upHP = HeapProps(D3D12_HEAP_TYPE_UPLOAD);
    D3D12_RESOURCE_DESC bd{D3D12_RESOURCE_DIMENSION_BUFFER, 0, size, 1, 1, 1, DXGI_FORMAT_UNKNOWN, {1,0}, D3D12_TEXTURE_LAYOUT_ROW_MAJOR};

    ComPtr<ID3D12Resource> gpuBuf;
    device->CreateCommittedResource(&defHP, D3D12_HEAP_FLAG_NONE, &bd, D3D12_RESOURCE_STATE_COPY_DEST, nullptr, IID_PPV_ARGS(&gpuBuf));
    device->CreateCommittedResource(&upHP, D3D12_HEAP_FLAG_NONE, &bd, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&upload));

    void* mapped = nullptr; D3D12_RANGE rr{0, 0};
    upload->Map(0, &rr, &mapped); std::memcpy(mapped, data, size); upload->Unmap(0, nullptr);
    cmdList->CopyBufferRegion(gpuBuf.Get(), 0, upload.Get(), 0, size);

    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = gpuBuf.Get();
    b.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
    b.Transition.StateAfter = D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER | D3D12_RESOURCE_STATE_INDEX_BUFFER;
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    cmdList->ResourceBarrier(1, &b);
    return gpuBuf;
}
