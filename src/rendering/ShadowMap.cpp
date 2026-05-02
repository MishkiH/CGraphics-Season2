#include "ShadowMap.h"

#include "Dx12Helpers.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <limits>

using namespace DirectX;

namespace
{
    constexpr float kCascadeSplitLambda = 0.55f;
    constexpr float kShadowCasterDepthPadding = 95.f;
    constexpr float kShadowDepthBias = 0.0017f;

    void TransitionResource(
        ID3D12GraphicsCommandList* cmdList,
        ID3D12Resource* resource,
        D3D12_RESOURCE_STATES& currentState,
        D3D12_RESOURCE_STATES newState)
    {
        if (!resource || currentState == newState)
            return;

        D3D12_RESOURCE_BARRIER barrier{};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Transition.pResource = resource;
        barrier.Transition.StateBefore = currentState;
        barrier.Transition.StateAfter = newState;
        barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &barrier);
        currentState = newState;
    }

    void ExtractPerspectiveClipRange(const XMFLOAT4X4& proj, float& nearClip, float& farClip)
    {
        nearClip = 0.05f;
        farClip = 520.f;

        if (std::abs(proj._33) <= 1e-6f || std::abs(1.f - proj._33) <= 1e-6f)
            return;

        const float nearFromProj = -proj._43 / proj._33;
        const float farFromProj = proj._43 / (1.f - proj._33);
        if (nearFromProj > 0.f && farFromProj > nearFromProj)
        {
            nearClip = nearFromProj;
            farClip = farFromProj;
        }
    }

    std::array<XMFLOAT3, 8> BuildFrustumCorners(
        const XMFLOAT4X4& proj,
        const XMMATRIX& invView,
        float nearDepth,
        float farDepth)
    {
        const float tanHalfFovX = 1.f / proj._11;
        const float tanHalfFovY = 1.f / proj._22;
        const float depths[2] = {nearDepth, farDepth};

        std::array<XMFLOAT3, 8> corners{};
        uint32_t index = 0;
        for (float depth : depths)
        {
            const float x = depth * tanHalfFovX;
            const float y = depth * tanHalfFovY;
            const XMVECTOR viewCorners[4] = {
                XMVectorSet(-x, -y, depth, 1.f),
                XMVectorSet(-x,  y, depth, 1.f),
                XMVectorSet( x,  y, depth, 1.f),
                XMVectorSet( x, -y, depth, 1.f),
            };

            for (const XMVECTOR& corner : viewCorners)
                XMStoreFloat3(&corners[index++], XMVector3TransformCoord(corner, invView));
        }

        return corners;
    }

    XMMATRIX BuildCascadeLightViewProj(
        const std::array<XMFLOAT3, 8>& corners,
        const XMFLOAT4& lightDirection)
    {
        XMVECTOR center = XMVectorZero();
        for (const XMFLOAT3& corner : corners)
            center = XMVectorAdd(center, XMLoadFloat3(&corner));
        center = XMVectorScale(center, 1.f / static_cast<float>(corners.size()));

        float radiusSq = 0.f;
        for (const XMFLOAT3& corner : corners)
        {
            const XMVECTOR delta = XMVectorSubtract(XMLoadFloat3(&corner), center);
            radiusSq = std::max(radiusSq, XMVectorGetX(XMVector3LengthSq(delta)));
        }

        const float radius = std::sqrt(radiusSq);
        const XMVECTOR lightDir = XMVector3Normalize(
            XMVectorSet(lightDirection.x, lightDirection.y, lightDirection.z, 0.f));
        const XMVECTOR upCandidate =
            std::abs(XMVectorGetX(XMVector3Dot(lightDir, XMVectorSet(0.f, 1.f, 0.f, 0.f)))) > 0.95f
                ? XMVectorSet(0.f, 0.f, 1.f, 0.f)
                : XMVectorSet(0.f, 1.f, 0.f, 0.f);

        const XMVECTOR eye = XMVectorSubtract(
            center,
            XMVectorScale(lightDir, radius + kShadowCasterDepthPadding));
        const XMMATRIX lightView = XMMatrixLookToLH(eye, lightDir, upCandidate);

        XMFLOAT3 minPt{
            std::numeric_limits<float>::max(),
            std::numeric_limits<float>::max(),
            std::numeric_limits<float>::max()};
        XMFLOAT3 maxPt{
            -std::numeric_limits<float>::max(),
            -std::numeric_limits<float>::max(),
            -std::numeric_limits<float>::max()};

        for (const XMFLOAT3& corner : corners)
        {
            XMFLOAT3 lightPt{};
            XMStoreFloat3(&lightPt, XMVector3TransformCoord(XMLoadFloat3(&corner), lightView));
            minPt.x = std::min(minPt.x, lightPt.x);
            minPt.y = std::min(minPt.y, lightPt.y);
            minPt.z = std::min(minPt.z, lightPt.z);
            maxPt.x = std::max(maxPt.x, lightPt.x);
            maxPt.y = std::max(maxPt.y, lightPt.y);
            maxPt.z = std::max(maxPt.z, lightPt.z);
        }

        const auto widenIfTiny = [](float& minValue, float& maxValue) {
            if (maxValue - minValue >= 1.f)
                return;
            const float centerValue = 0.5f * (minValue + maxValue);
            minValue = centerValue - 0.5f;
            maxValue = centerValue + 0.5f;
        };

        widenIfTiny(minPt.x, maxPt.x);
        widenIfTiny(minPt.y, maxPt.y);
        minPt.z = std::max(0.f, minPt.z - kShadowCasterDepthPadding);
        maxPt.z = std::max(minPt.z + 1.f, maxPt.z + kShadowCasterDepthPadding);

        return lightView * XMMatrixOrthographicOffCenterLH(
            minPt.x,
            maxPt.x,
            minPt.y,
            maxPt.y,
            minPt.z,
            maxPt.z);
    }
}

bool CascadedShadowMap::Initialize(ID3D12Device* device)
{
    m_dsvStride = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_DSV);

    D3D12_DESCRIPTOR_HEAP_DESC heapDesc{};
    heapDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
    heapDesc.NumDescriptors = CascadeCount;
    if (FAILED(device->CreateDescriptorHeap(&heapDesc, IID_PPV_ARGS(&m_dsvHeap))))
        return false;

    const auto defaultHeap = dx12::HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
    D3D12_CLEAR_VALUE clearValue{};
    clearValue.Format = DXGI_FORMAT_D32_FLOAT;
    clearValue.DepthStencil.Depth = 1.f;

    D3D12_RESOURCE_DESC shadowDesc{};
    shadowDesc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    shadowDesc.Width = ShadowMapSize;
    shadowDesc.Height = ShadowMapSize;
    shadowDesc.DepthOrArraySize = static_cast<UINT16>(CascadeCount);
    shadowDesc.MipLevels = 1;
    shadowDesc.Format = DXGI_FORMAT_R32_TYPELESS;
    shadowDesc.SampleDesc.Count = 1;
    shadowDesc.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
    shadowDesc.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;

    if (FAILED(device->CreateCommittedResource(
            &defaultHeap,
            D3D12_HEAP_FLAG_NONE,
            &shadowDesc,
            D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE,
            &clearValue,
            IID_PPV_ARGS(&m_shadowMap))))
    {
        return false;
    }
    m_state = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;

    const D3D12_CPU_DESCRIPTOR_HANDLE dsvBase = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t cascade = 0; cascade < CascadeCount; ++cascade)
    {
        D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc{};
        dsvDesc.Format = DXGI_FORMAT_D32_FLOAT;
        dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2DARRAY;
        dsvDesc.Texture2DArray.MipSlice = 0;
        dsvDesc.Texture2DArray.FirstArraySlice = cascade;
        dsvDesc.Texture2DArray.ArraySize = 1;

        m_dsvs[cascade] = dsvBase;
        m_dsvs[cascade].ptr += static_cast<SIZE_T>(cascade) * m_dsvStride;
        device->CreateDepthStencilView(m_shadowMap.Get(), &dsvDesc, m_dsvs[cascade]);
    }

    return true;
}

void CascadedShadowMap::Shutdown()
{
    m_shadowMap.Reset();
    m_dsvHeap.Reset();
    m_state = D3D12_RESOURCE_STATE_COMMON;
}

void CascadedShadowMap::Update(
    const XMFLOAT4X4& view,
    const XMFLOAT4X4& proj,
    const XMFLOAT4& lightDirection)
{
    float nearClip = 0.f;
    float farClip = 0.f;
    ExtractPerspectiveClipRange(proj, nearClip, farClip);

    const XMMATRIX invView = XMMatrixInverse(nullptr, XMLoadFloat4x4(&view));
    float cascadeNear = nearClip;
    for (uint32_t cascade = 0; cascade < CascadeCount; ++cascade)
    {
        const float p = static_cast<float>(cascade + 1u) / static_cast<float>(CascadeCount);
        const float logSplit = nearClip * std::pow(farClip / nearClip, p);
        const float uniformSplit = nearClip + (farClip - nearClip) * p;
        float cascadeFarDepth = kCascadeSplitLambda * logSplit
            + (1.f - kCascadeSplitLambda) * uniformSplit;

        if (cascade + 1u == CascadeCount)
            cascadeFarDepth = farClip;

        const auto corners = BuildFrustumCorners(proj, invView, cascadeNear, cascadeFarDepth);
        XMStoreFloat4x4(
            &m_constants.LightViewProj[cascade],
            BuildCascadeLightViewProj(corners, lightDirection));
        (&m_constants.CascadeFar.x)[cascade] = cascadeFarDepth;
        cascadeNear = cascadeFarDepth;
    }

    m_constants.ShadowParams = {
        1.f / static_cast<float>(ShadowMapSize),
        1.f / static_cast<float>(ShadowMapSize),
        kShadowDepthBias,
        0.f};
}

void CascadedShadowMap::TransitionToWrite(ID3D12GraphicsCommandList* cmdList)
{
    TransitionResource(cmdList, m_shadowMap.Get(), m_state, D3D12_RESOURCE_STATE_DEPTH_WRITE);
}

void CascadedShadowMap::TransitionToRead(ID3D12GraphicsCommandList* cmdList)
{
    TransitionResource(cmdList, m_shadowMap.Get(), m_state, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);
}

void CascadedShadowMap::CreateSrv(ID3D12Device* device, D3D12_CPU_DESCRIPTOR_HANDLE destination) const
{
    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc{};
    srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    srvDesc.Format = DXGI_FORMAT_R32_FLOAT;
    srvDesc.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2DARRAY;
    srvDesc.Texture2DArray.MostDetailedMip = 0;
    srvDesc.Texture2DArray.MipLevels = 1;
    srvDesc.Texture2DArray.FirstArraySlice = 0;
    srvDesc.Texture2DArray.ArraySize = CascadeCount;
    device->CreateShaderResourceView(m_shadowMap.Get(), &srvDesc, destination);
}

D3D12_CPU_DESCRIPTOR_HANDLE CascadedShadowMap::GetDsv(uint32_t cascadeIndex) const
{
    return m_dsvs[std::min(cascadeIndex, CascadeCount - 1u)];
}

D3D12_VIEWPORT CascadedShadowMap::GetViewport() const
{
    return {0.f, 0.f, static_cast<float>(ShadowMapSize), static_cast<float>(ShadowMapSize), 0.f, 1.f};
}

D3D12_RECT CascadedShadowMap::GetScissorRect() const
{
    return {0, 0, static_cast<LONG>(ShadowMapSize), static_cast<LONG>(ShadowMapSize)};
}

