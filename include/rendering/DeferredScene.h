#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>
#include "GBuffer.h"

class DeferredScene
{
public:
    enum RenderFeature : int
    {
        RenderFeatureNone = 0,
        RenderFeatureNormalMapping = 1 << 0,
        RenderFeatureDisplacement = 1 << 1,
    };

    struct SceneLight
    {
        enum class Type
        {
            Directional,
            Point,
            Spot,
        };

        Type LightType = Type::Directional;
        DirectX::XMFLOAT3 Position{0.f, 0.f, 0.f};
        float Range = 1.f;
        DirectX::XMFLOAT3 Direction{0.f, -1.f, 0.f};
        float InnerConeDegrees = 15.f;
        DirectX::XMFLOAT3 Color{1.f, 1.f, 1.f};
        float Intensity = 1.f;
        float OuterConeDegrees = 25.f;
    };

    struct SceneOptions
    {
        std::string MeshPath;
        bool EnableWater = false;
        bool UseTessellation = true;
        bool EnableNormalMapping = true;
        bool EnableDisplacement = true;
        float SceneScale = 1.f;
        DirectX::XMFLOAT3 SceneOffset{0.f, 0.f, 0.f};
        DirectX::XMFLOAT3 AmbientColor{0.05f, 0.05f, 0.06f};
        DirectX::XMFLOAT2 UvTiling{1.f, 1.f};
        DirectX::XMFLOAT2 UvScrollRate{0.f, 0.f};
        std::vector<SceneLight> Lights;
    };

    bool Initialize(ID3D12Device* device, ID3D12CommandQueue* cmdQueue,
                    DXGI_FORMAT backBufferFmt, uint32_t width, uint32_t height,
                    const SceneOptions& options);

    void Shutdown();
    void OnResize(ID3D12Device* device, uint32_t width, uint32_t height);

    void SetCamera(const DirectX::XMFLOAT4X4& view,
                   const DirectX::XMFLOAT4X4& proj,
                   const DirectX::XMFLOAT3& eye);

    void SetRenderMode(int mode) { m_renderMode = mode; }
    void SetUvEffectsEnabled(bool enabled) { m_uvEffectsEnabled = enabled; }
    bool UvEffectsEnabled() const { return m_uvEffectsEnabled; }
    bool UsesTessellation() const { return m_options.UseTessellation; }

    void RecordCommands(ID3D12GraphicsCommandList* cmdList,
                        D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                        D3D12_VIEWPORT viewport,
                        D3D12_RECT scissorRect,
                        float dt);

private:
    struct alignas(16) PassConstants
    {
        DirectX::XMFLOAT4X4 World{};
        DirectX::XMFLOAT4X4 ViewProj{};
        DirectX::XMFLOAT4X4 InvViewProj{};
        DirectX::XMFLOAT4 EyePosW{0.f, 0.f, 0.f, 1.f};
        DirectX::XMFLOAT4 RenderTargetSize{1.f, 1.f, 1.f, 1.f};
        DirectX::XMFLOAT4 TessParams{1.f, 6.f, 0.5f, 15.f};
        DirectX::XMFLOAT4 DispParams{0.02f, -0.01f, 0.f, 0.f};
        DirectX::XMFLOAT4 UvOffsetTiling{0.f, 0.f, 1.f, 1.f};
    };

    struct alignas(16) GpuLight
    {
        DirectX::XMFLOAT4 PositionRange{};
        DirectX::XMFLOAT4 DirectionSpot{};
        DirectX::XMFLOAT4 ColorIntensity{};
        DirectX::XMFLOAT4 Params{};
    };

    static constexpr uint32_t MaxLights = 128;

    struct alignas(16) LightConstants
    {
        DirectX::XMFLOAT4 AmbientColor{0.05f, 0.05f, 0.06f, 1.f};
        DirectX::XMFLOAT4 LightCount{0.f, 0.f, 0.f, 0.f};
        GpuLight Lights[MaxLights]{};
    };

    struct MaterialConstants
    {
        DirectX::XMFLOAT4 BaseColor{1.f, 1.f, 1.f, 1.f};
        DirectX::XMFLOAT4 SurfaceParams{0.18f, 32.f, 0.f, 0.f};
    };

    struct DrawItem
    {
        uint32_t IndexCount = 0;
        uint32_t StartIndexLocation = 0;
        uint32_t TextureIndex = 0;
        uint32_t NormalTextureIndex = 0;
        uint32_t DisplacementTextureIndex = 0;
        MaterialConstants Material;
    };

    bool BuildShaders(ID3D12Device* device);
    bool BuildRootSignature(ID3D12Device* device);
    bool BuildPSOs(ID3D12Device* device, DXGI_FORMAT backBufferFmt);
    bool BuildSceneGeometry(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                            std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>>& uploads);
    bool BuildWaterGeometry(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                            std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>>& uploads);
    bool BuildConstantBuffers(ID3D12Device* device);

    void UpdatePassConstants(uint32_t width, uint32_t height);
    void UpdateLightConstants(float dt);
    int EffectiveRenderMode() const;

    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_rootSig;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_geometryPSO;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_lightingPSO;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_waterPSO;

    Microsoft::WRL::ComPtr<ID3DBlob> m_geometryVS, m_geometryFlatVS, m_geometryPS;
    Microsoft::WRL::ComPtr<ID3DBlob> m_hullShader, m_domainShader;
    Microsoft::WRL::ComPtr<ID3DBlob> m_lightingVS, m_lightingPS;
    Microsoft::WRL::ComPtr<ID3DBlob> m_waterVS, m_waterHS, m_waterDS, m_waterPS;

    D3D12_INPUT_ELEMENT_DESC m_inputLayout[4]{};

    Microsoft::WRL::ComPtr<ID3D12Resource> m_vertexBuffer, m_indexBuffer;
    D3D12_VERTEX_BUFFER_VIEW m_vbv{};
    D3D12_INDEX_BUFFER_VIEW m_ibv{};

    Microsoft::WRL::ComPtr<ID3D12Resource> m_waterVertexBuffer, m_waterIndexBuffer;
    D3D12_VERTEX_BUFFER_VIEW m_waterVBV{};
    D3D12_INDEX_BUFFER_VIEW m_waterIBV{};
    uint32_t m_waterIndexCount = 0;

    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_textureHeap;
    std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>> m_textures;
    uint32_t m_srvStride = 0;

    std::vector<DrawItem> m_drawItems;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_passCB, m_lightCB;
    uint8_t* m_mappedPassCB = nullptr;
    uint8_t* m_mappedLightCB = nullptr;

    std::unique_ptr<GBuffer> m_gBuffer;

    DirectX::XMFLOAT4X4 m_view{};
    DirectX::XMFLOAT4X4 m_proj{};
    DirectX::XMFLOAT3 m_eye{};
    SceneOptions m_options;
    float m_time = 0.f;
    int m_renderMode = RenderFeatureNormalMapping | RenderFeatureDisplacement;
    bool m_uvEffectsEnabled = false;
};
