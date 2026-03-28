#include "GBuffer.h"
#include <stdexcept>
#include <cstdio>

using Microsoft::WRL::ComPtr;

namespace
{
    void ThrowIfFailed(HRESULT hr, const char* what)
    {
        if (FAILED(hr))
        {
            char buf[256];
            std::snprintf(buf, sizeof(buf), "%s (hr=0x%08X)", what, static_cast<unsigned>(hr));
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

    D3D12_RESOURCE_DESC Tex2DDesc(uint32_t w, uint32_t h, DXGI_FORMAT fmt, D3D12_RESOURCE_FLAGS flags)
    {
        D3D12_RESOURCE_DESC d{};
        d.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        d.Width = w;
        d.Height = h;
        d.DepthOrArraySize = 1;
        d.MipLevels = 1;
        d.Format = fmt;
        d.SampleDesc.Count = 1;
        d.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
        d.Flags = flags;
        return d;
    }
}

bool GBuffer::Initialize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    m_width = width;
    m_height = height;
    m_rtvDescriptorSize = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    m_dsvDescriptorSize = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_DSV);

    D3D12_DESCRIPTOR_HEAP_DESC rtvDesc{};
    rtvDesc.NumDescriptors = TargetCount;
    rtvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
    ThrowIfFailed(device->CreateDescriptorHeap(&rtvDesc, IID_PPV_ARGS(&m_rtvHeap)), "GBuffer RTV heap");

    D3D12_DESCRIPTOR_HEAP_DESC srvDesc{};
    srvDesc.NumDescriptors = SrvCount;
    srvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    srvDesc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    ThrowIfFailed(device->CreateDescriptorHeap(&srvDesc, IID_PPV_ARGS(&m_srvHeap)), "GBuffer SRV heap");

    D3D12_DESCRIPTOR_HEAP_DESC dsvDesc{};
    dsvDesc.NumDescriptors = 2;
    dsvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
    ThrowIfFailed(device->CreateDescriptorHeap(&dsvDesc, IID_PPV_ARGS(&m_dsvHeap)), "GBuffer DSV heap");

    CreateResources(device);
    return true;
}

void GBuffer::Shutdown()
{
    ReleaseResources();
    m_dsvHeap.Reset();
    m_srvHeap.Reset();
    m_rtvHeap.Reset();
    m_width = 0;
    m_height = 0;
    m_isWriteState = false;
    m_isDepthWriteState = true;
}

void GBuffer::Resize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!width || !height) return;
    m_width = width;
    m_height = height;
    ReleaseResources();
    CreateResources(device);
}

void GBuffer::TransitionToWrite(ID3D12GraphicsCommandList* cmdList)
{
    if (m_isWriteState) return;

    D3D12_RESOURCE_BARRIER barriers[TargetCount]{};
    for (uint32_t i = 0; i < TargetCount; ++i)
    {
        barriers[i].Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barriers[i].Transition.pResource = m_targets[i].Get();
        barriers[i].Transition.StateBefore = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
        barriers[i].Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
        barriers[i].Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    }
    cmdList->ResourceBarrier(TargetCount, barriers);
    m_isWriteState = true;
}

void GBuffer::TransitionToRead(ID3D12GraphicsCommandList* cmdList)
{
    if (!m_isWriteState) return;

    D3D12_RESOURCE_BARRIER barriers[TargetCount]{};
    for (uint32_t i = 0; i < TargetCount; ++i)
    {
        barriers[i].Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barriers[i].Transition.pResource = m_targets[i].Get();
        barriers[i].Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
        barriers[i].Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
        barriers[i].Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    }
    cmdList->ResourceBarrier(TargetCount, barriers);
    m_isWriteState = false;
}

void GBuffer::TransitionDepthToRead(ID3D12GraphicsCommandList* cmdList)
{
    if (!m_isDepthWriteState) return;

    constexpr D3D12_RESOURCE_STATES kReadState =
        D3D12_RESOURCE_STATE_DEPTH_READ | D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;

    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = m_depthStencil.Get();
    b.Transition.StateBefore = D3D12_RESOURCE_STATE_DEPTH_WRITE;
    b.Transition.StateAfter = kReadState;
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    cmdList->ResourceBarrier(1, &b);
    m_isDepthWriteState = false;
}

void GBuffer::TransitionDepthToWrite(ID3D12GraphicsCommandList* cmdList)
{
    if (m_isDepthWriteState) return;

    constexpr D3D12_RESOURCE_STATES kReadState =
        D3D12_RESOURCE_STATE_DEPTH_READ | D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;

    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = m_depthStencil.Get();
    b.Transition.StateBefore = kReadState;
    b.Transition.StateAfter = D3D12_RESOURCE_STATE_DEPTH_WRITE;
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    cmdList->ResourceBarrier(1, &b);
    m_isDepthWriteState = true;
}

void GBuffer::BindForGeometryPass(ID3D12GraphicsCommandList* cmdList)
{
    D3D12_CPU_DESCRIPTOR_HANDLE rtvs[TargetCount]{};
    D3D12_CPU_DESCRIPTOR_HANDLE rtv = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < TargetCount; ++i)
    {
        rtvs[i] = rtv;
        rtv.ptr += m_rtvDescriptorSize;
    }

    const float clearAlbedo[4] = {0.f, 0.f, 0.f, 0.f};
    const float clearNormal[4] = {0.f, 0.f, 1.f, 0.f};

    D3D12_CPU_DESCRIPTOR_HANDLE dsv = GetDsv();
    cmdList->OMSetRenderTargets(TargetCount, rtvs, FALSE, &dsv);
    cmdList->ClearRenderTargetView(rtvs[0], clearAlbedo, 0, nullptr);
    cmdList->ClearRenderTargetView(rtvs[1], clearNormal, 0, nullptr);
    cmdList->ClearDepthStencilView(dsv, D3D12_CLEAR_FLAG_DEPTH, 1.f, 0, 0, nullptr);
}

void GBuffer::CreateResources(ID3D12Device* device)
{
    auto heap = HeapProps(D3D12_HEAP_TYPE_DEFAULT);

    const DXGI_FORMAT colorFmts[TargetCount] = {GetAlbedoSpecFormat(), GetNormalFormat()};

    D3D12_CLEAR_VALUE clears[TargetCount]{};
    clears[0].Format = colorFmts[0];
    clears[1].Format = colorFmts[1];
    clears[1].Color[2] = 1.f;

    for (uint32_t i = 0; i < TargetCount; ++i)
    {
        auto desc = Tex2DDesc(m_width, m_height, colorFmts[i], D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET);
        ThrowIfFailed(
            device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &desc,
                D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, &clears[i],
                IID_PPV_ARGS(&m_targets[i])),
            "GBuffer color target");
    }

    D3D12_CLEAR_VALUE depthClear{};
    depthClear.Format = GetDepthStencilFormat();
    depthClear.DepthStencil.Depth = 1.f;

    auto depthDesc = Tex2DDesc(m_width, m_height, GetDepthStencilFormat(), D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);
    ThrowIfFailed(
        device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &depthDesc,
            D3D12_RESOURCE_STATE_DEPTH_WRITE, &depthClear,
            IID_PPV_ARGS(&m_depthStencil)),
        "GBuffer depth");

    D3D12_CPU_DESCRIPTOR_HANDLE rtvHandle = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < TargetCount; ++i)
    {
        device->CreateRenderTargetView(m_targets[i].Get(), nullptr, rtvHandle);
        rtvHandle.ptr += m_rtvDescriptorSize;
    }

    D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc{};
    dsvDesc.Format = GetDepthStencilFormat();
    dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
    dsvDesc.Flags = D3D12_DSV_FLAG_NONE;
    device->CreateDepthStencilView(m_depthStencil.Get(), &dsvDesc,
        m_dsvHeap->GetCPUDescriptorHandleForHeapStart());

    dsvDesc.Flags = D3D12_DSV_FLAG_READ_ONLY_DEPTH;
    D3D12_CPU_DESCRIPTOR_HANDLE dsvRO = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
    dsvRO.ptr += m_dsvDescriptorSize;
    device->CreateDepthStencilView(m_depthStencil.Get(), &dsvDesc, dsvRO);

    const uint32_t srvStride = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    D3D12_CPU_DESCRIPTOR_HANDLE srvHandle = m_srvHeap->GetCPUDescriptorHandleForHeapStart();

    for (uint32_t i = 0; i < TargetCount; ++i)
    {
        D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc{};
        srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
        srvDesc.Format = colorFmts[i];
        srvDesc.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
        srvDesc.Texture2D.MipLevels = 1;
        device->CreateShaderResourceView(m_targets[i].Get(), &srvDesc, srvHandle);
        srvHandle.ptr += srvStride;
    }

    D3D12_SHADER_RESOURCE_VIEW_DESC depthSrv{};
    depthSrv.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    depthSrv.Format = DXGI_FORMAT_R32_FLOAT;
    depthSrv.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    depthSrv.Texture2D.MipLevels = 1;
    device->CreateShaderResourceView(m_depthStencil.Get(), &depthSrv, srvHandle);

    m_isWriteState = false;
    m_isDepthWriteState = true;
}

void GBuffer::ReleaseResources()
{
    for (auto& t : m_targets) t.Reset();
    m_depthStencil.Reset();
}
