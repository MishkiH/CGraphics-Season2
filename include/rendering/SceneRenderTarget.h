#pragma once

#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <cstdint>

class SceneRenderTarget
{
public:
    bool Initialize(ID3D12Device* device, uint32_t width, uint32_t height, DXGI_FORMAT format);
    void Shutdown();
    void Resize(ID3D12Device* device, uint32_t width, uint32_t height);

    void TransitionToRenderTarget(ID3D12GraphicsCommandList* cmdList);
    void TransitionToPixelShaderResource(ID3D12GraphicsCommandList* cmdList);

    D3D12_CPU_DESCRIPTOR_HANDLE GetRtv() const;
    D3D12_GPU_DESCRIPTOR_HANDLE GetSrv() const;
    ID3D12DescriptorHeap* GetSrvHeap() const { return m_srvHeap.Get(); }
    DXGI_FORMAT GetFormat() const { return m_format; }

private:
    void CreateResources(ID3D12Device* device);
    void ReleaseResources();
    void Transition(ID3D12GraphicsCommandList* cmdList, D3D12_RESOURCE_STATES newState);

    uint32_t m_width = 0;
    uint32_t m_height = 0;
    DXGI_FORMAT m_format = DXGI_FORMAT_UNKNOWN;
    D3D12_RESOURCE_STATES m_state = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_texture;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_rtvHeap;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_srvHeap;
};
