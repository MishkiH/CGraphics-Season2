#pragma once

#include "Dx12Helpers.h"
#include "ShadowMap.h"

// Opt-in base for scenes that use cascaded shadows
// It keeps common shadow ownership out of concrete scene classes
class ShadowedScene
{
protected:
    static constexpr uint32_t ShadowCascadeCount = CascadedShadowMap::CascadeCount;
    using ShadowConstants = CascadedShadowMap::Constants;

    bool InitializeShadows(ID3D12Device* device) { return m_shadowMap.Initialize(device); }
    void ShutdownShadows() { m_shadowMap.Shutdown(); }

    void UpdateShadows(const DirectX::XMFLOAT4X4& view,
                       const DirectX::XMFLOAT4X4& proj,
                       const DirectX::XMFLOAT4& lightDirection)
    {
        m_shadowMap.Update(view, proj, lightDirection);
    }

    D3D12_GPU_DESCRIPTOR_HANDLE CreateShadowSrvInHeap(
        ID3D12Device* device,
        ID3D12DescriptorHeap* heap,
        uint32_t descriptorStride,
        uint32_t descriptorIndex) const
    {
        D3D12_CPU_DESCRIPTOR_HANDLE cpuHandle = dx12::OffsetCpuHandle(
            heap->GetCPUDescriptorHandleForHeapStart(),
            descriptorStride,
            descriptorIndex);
        m_shadowMap.CreateSrv(device, cpuHandle);

        return dx12::OffsetGpuHandle(
            heap->GetGPUDescriptorHandleForHeapStart(),
            descriptorStride,
            descriptorIndex);
    }

    const ShadowConstants& GetShadowConstants() const { return m_shadowMap.GetConstants(); }
    DXGI_FORMAT GetShadowDsvFormat() const { return m_shadowMap.GetDsvFormat(); }

    template <typename Destination>
    void CopyShadowConstants(Destination& destination) const
    {
        const ShadowConstants& source = GetShadowConstants();
        for (uint32_t cascade = 0; cascade < ShadowCascadeCount; ++cascade)
            destination.LightViewProj[cascade] = source.LightViewProj[cascade];
        destination.CascadeFar = source.CascadeFar;
        destination.ShadowParams = source.ShadowParams;
    }

    template <typename Destination>
    void CopyTransposedShadowConstants(Destination& destination) const
    {
        const ShadowConstants& source = GetShadowConstants();
        for (uint32_t cascade = 0; cascade < ShadowCascadeCount; ++cascade)
        {
            DirectX::XMStoreFloat4x4(
                &destination.LightViewProj[cascade],
                DirectX::XMMatrixTranspose(DirectX::XMLoadFloat4x4(&source.LightViewProj[cascade])));
        }
        destination.CascadeFar = source.CascadeFar;
        destination.ShadowParams = source.ShadowParams;
    }

    template <typename SetupPass, typename DrawCascade>
    void RecordShadowPass(ID3D12GraphicsCommandList* cmdList, SetupPass setupPass, DrawCascade drawCascade)
    {
        m_shadowMap.Record(cmdList, setupPass, drawCascade);
    }

private:
    CascadedShadowMap m_shadowMap;
};
