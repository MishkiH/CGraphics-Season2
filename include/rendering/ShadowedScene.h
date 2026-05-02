#pragma once

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

    void CreateShadowSrv(ID3D12Device* device, D3D12_CPU_DESCRIPTOR_HANDLE destination) const
    {
        m_shadowMap.CreateSrv(device, destination);
    }

    const ShadowConstants& GetShadowConstants() const { return m_shadowMap.GetConstants(); }
    DXGI_FORMAT GetShadowDsvFormat() const { return m_shadowMap.GetDsvFormat(); }

    template <typename SetupPass, typename DrawCascade>
    void RecordShadowPass(ID3D12GraphicsCommandList* cmdList, SetupPass setupPass, DrawCascade drawCascade)
    {
        m_shadowMap.Record(cmdList, setupPass, drawCascade);
    }

private:
    CascadedShadowMap m_shadowMap;
};

