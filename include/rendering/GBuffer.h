#pragma once

#include <wrl.h>
#include <d3d12.h>
#include <cstdint>

class GBuffer
{
public:
    static constexpr uint32_t TargetCount = 2;
    static constexpr uint32_t SrvCount = TargetCount + 1;

    bool Initialize(ID3D12Device* device, uint32_t width, uint32_t height);
    void Shutdown();
    void Resize(ID3D12Device* device, uint32_t width, uint32_t height);

    void TransitionToWrite(ID3D12GraphicsCommandList* cmdList);
    void TransitionToRead(ID3D12GraphicsCommandList* cmdList);
    void TransitionDepthToRead(ID3D12GraphicsCommandList* cmdList);
    void TransitionDepthToWrite(ID3D12GraphicsCommandList* cmdList);
    void BindForGeometryPass(ID3D12GraphicsCommandList* cmdList);

    ID3D12DescriptorHeap* GetSrvHeap() const { return m_srvHeap.Get(); }
    D3D12_GPU_DESCRIPTOR_HANDLE GetSrvTable() const { return m_srvHeap->GetGPUDescriptorHandleForHeapStart(); }
    D3D12_CPU_DESCRIPTOR_HANDLE GetDsv() const { return m_dsvHeap->GetCPUDescriptorHandleForHeapStart(); }
    D3D12_CPU_DESCRIPTOR_HANDLE GetDsvReadOnly() const
    {
        D3D12_CPU_DESCRIPTOR_HANDLE h = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
        h.ptr += m_dsvDescriptorSize;
        return h;
    }

    DXGI_FORMAT GetAlbedoSpecFormat() const { return DXGI_FORMAT_R8G8B8A8_UNORM; }
    DXGI_FORMAT GetNormalFormat() const { return DXGI_FORMAT_R16G16B16A16_FLOAT; }
    DXGI_FORMAT GetDepthResourceFormat() const { return DXGI_FORMAT_R32_TYPELESS; }
    DXGI_FORMAT GetDepthStencilFormat() const { return DXGI_FORMAT_D32_FLOAT; }
    DXGI_FORMAT GetDepthSrvFormat() const { return DXGI_FORMAT_R32_FLOAT; }

private:
    void CreateResources(ID3D12Device* device);
    void ReleaseResources();

    uint32_t m_width = 0;
    uint32_t m_height = 0;
    uint32_t m_rtvDescriptorSize = 0;
    uint32_t m_dsvDescriptorSize = 0;
    bool m_isWriteState = false;
    bool m_isDepthWriteState = true;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_targets[TargetCount];
    Microsoft::WRL::ComPtr<ID3D12Resource> m_depthStencil;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_rtvHeap;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_srvHeap;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_dsvHeap;
};
