#include "SceneRenderTarget.h"

#include "Dx12Helpers.h"

bool SceneRenderTarget::Initialize(
    ID3D12Device* device,
    uint32_t width,
    uint32_t height,
    DXGI_FORMAT format)
{
    m_width = width;
    m_height = height;
    m_format = format;

    D3D12_DESCRIPTOR_HEAP_DESC rtvDesc{};
    rtvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
    rtvDesc.NumDescriptors = 1;
    dx12::ThrowIfFailed(device->CreateDescriptorHeap(&rtvDesc, IID_PPV_ARGS(&m_rtvHeap)), "SceneColor RTV heap");

    D3D12_DESCRIPTOR_HEAP_DESC srvDesc{};
    srvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    srvDesc.NumDescriptors = 1;
    srvDesc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    dx12::ThrowIfFailed(device->CreateDescriptorHeap(&srvDesc, IID_PPV_ARGS(&m_srvHeap)), "SceneColor SRV heap");

    CreateResources(device);
    return true;
}

void SceneRenderTarget::Shutdown()
{
    ReleaseResources();
    m_srvHeap.Reset();
    m_rtvHeap.Reset();
    m_width = 0;
    m_height = 0;
    m_format = DXGI_FORMAT_UNKNOWN;
    m_state = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
}

void SceneRenderTarget::Resize(ID3D12Device* device, uint32_t width, uint32_t height)
{
    if (!width || !height)
        return;

    m_width = width;
    m_height = height;
    ReleaseResources();
    CreateResources(device);
}

void SceneRenderTarget::TransitionToRenderTarget(ID3D12GraphicsCommandList* cmdList)
{
    Transition(cmdList, D3D12_RESOURCE_STATE_RENDER_TARGET);
}

void SceneRenderTarget::TransitionToPixelShaderResource(ID3D12GraphicsCommandList* cmdList)
{
    Transition(cmdList, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);
}

D3D12_CPU_DESCRIPTOR_HANDLE SceneRenderTarget::GetRtv() const
{
    return m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
}

D3D12_GPU_DESCRIPTOR_HANDLE SceneRenderTarget::GetSrv() const
{
    return m_srvHeap->GetGPUDescriptorHandleForHeapStart();
}

void SceneRenderTarget::CreateResources(ID3D12Device* device)
{
    D3D12_CLEAR_VALUE clear{};
    clear.Format = m_format;
    clear.Color[3] = 1.f;

    auto heap = dx12::HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
    auto desc = dx12::Texture2DDesc(
        m_width,
        m_height,
        m_format,
        D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET);

    dx12::ThrowIfFailed(
        device->CreateCommittedResource(
            &heap,
            D3D12_HEAP_FLAG_NONE,
            &desc,
            D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE,
            &clear,
            IID_PPV_ARGS(&m_texture)),
        "SceneColor texture");

    device->CreateRenderTargetView(m_texture.Get(), nullptr, m_rtvHeap->GetCPUDescriptorHandleForHeapStart());

    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc{};
    srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    srvDesc.Format = m_format;
    srvDesc.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    srvDesc.Texture2D.MipLevels = 1;
    device->CreateShaderResourceView(m_texture.Get(), &srvDesc, m_srvHeap->GetCPUDescriptorHandleForHeapStart());

    m_state = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
}

void SceneRenderTarget::ReleaseResources()
{
    m_texture.Reset();
}

void SceneRenderTarget::Transition(ID3D12GraphicsCommandList* cmdList, D3D12_RESOURCE_STATES newState)
{
    dx12::TransitionResource(cmdList, m_texture.Get(), m_state, newState);
}
