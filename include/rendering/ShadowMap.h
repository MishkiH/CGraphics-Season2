#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <wrl.h>
#include <d3d12.h>
#include <DirectXMath.h>
#include <cstdint>

// shared DX12 resource and math for cascaded shadow maps
// scenes draw their own shadow casters becaus different root signatures
class CascadedShadowMap
{
public:
    static constexpr uint32_t CascadeCount = 4;
    static constexpr uint32_t ShadowMapSize = 2048;

    struct Constants
    {
        DirectX::XMFLOAT4X4 LightViewProj[CascadeCount]{};
        DirectX::XMFLOAT4 CascadeFar{};
        DirectX::XMFLOAT4 ShadowParams{};
    };

    bool Initialize(ID3D12Device* device);
    void Shutdown();

    void Update(const DirectX::XMFLOAT4X4& view,
                const DirectX::XMFLOAT4X4& proj,
                const DirectX::XMFLOAT4& lightDirection);

    void TransitionToWrite(ID3D12GraphicsCommandList* cmdList);
    void TransitionToRead(ID3D12GraphicsCommandList* cmdList);

    void CreateSrv(ID3D12Device* device, D3D12_CPU_DESCRIPTOR_HANDLE destination) const;

    template <typename SetupPass, typename DrawCascade>
    void Record(ID3D12GraphicsCommandList* cmdList, SetupPass setupPass, DrawCascade drawCascade)
    {
        TransitionToWrite(cmdList);

        const D3D12_VIEWPORT viewport = GetViewport();
        const D3D12_RECT scissorRect = GetScissorRect();
        cmdList->RSSetViewports(1, &viewport);
        cmdList->RSSetScissorRects(1, &scissorRect);
        setupPass();

        for (uint32_t cascade = 0; cascade < CascadeCount; ++cascade)
        {
            const D3D12_CPU_DESCRIPTOR_HANDLE dsv = GetDsv(cascade);
            cmdList->ClearDepthStencilView(dsv, D3D12_CLEAR_FLAG_DEPTH, 1.f, 0, 0, nullptr);
            cmdList->OMSetRenderTargets(0, nullptr, FALSE, &dsv);
            drawCascade(cascade);
        }

        TransitionToRead(cmdList);
    }

    D3D12_CPU_DESCRIPTOR_HANDLE GetDsv(uint32_t cascadeIndex) const;
    D3D12_VIEWPORT GetViewport() const;
    D3D12_RECT GetScissorRect() const;

    const Constants& GetConstants() const { return m_constants; }
    DXGI_FORMAT GetDsvFormat() const { return DXGI_FORMAT_D32_FLOAT; }

private:
    Microsoft::WRL::ComPtr<ID3D12Resource> m_shadowMap;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_dsvHeap;
    uint32_t m_dsvStride = 0;
    D3D12_CPU_DESCRIPTOR_HANDLE m_dsvs[CascadeCount]{};
    D3D12_RESOURCE_STATES m_state = D3D12_RESOURCE_STATE_COMMON;
    Constants m_constants{};
};
