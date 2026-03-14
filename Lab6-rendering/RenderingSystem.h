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

class GBuffer;

class RenderingSystem
{
public:
    RenderingSystem();
    ~RenderingSystem();
    struct Vertex
    {
        DirectX::XMFLOAT3 Pos;
        DirectX::XMFLOAT3 Normal;
        DirectX::XMFLOAT2 TexC;
    };

    bool Initialize(HWND hwnd, uint32_t width, uint32_t height);
    void Shutdown();

    void OnResize(uint32_t width, uint32_t height);
    void Draw(float dt);
    void SetCamera(const DirectX::XMFLOAT3& eyePos, float yaw, float pitch);

private:
    // -------------------------------------------------------------------------
    // N-body simulation
    // -------------------------------------------------------------------------
    static constexpr uint32_t NBodyCount = 1000;

    // CPU-side particle state
    struct NBodyParticle
    {
        DirectX::XMFLOAT3 Position;
        float             Mass;
        DirectX::XMFLOAT3 Velocity;
        float             _pad;
        DirectX::XMFLOAT3 Color;   // linear RGB, 0-1
        float             _pad2;
    };

    struct alignas(16) BulbInstance
    {
        DirectX::XMFLOAT3 Position;
        float             Radius;
        DirectX::XMFLOAT3 Color;
        float             Intensity;
    };

    void InitNBody();
    void UpdateNBody(float dt);
    bool BuildBulbPipeline();

    struct MaterialConstants
    {
        DirectX::XMFLOAT4 BaseColor{ 1.f, 1.f, 1.f, 1.f };
        DirectX::XMFLOAT4 SurfaceParams{ 0.18f, 32.f, 0.f, 0.f };
    };

    struct DrawItem
    {
        uint32_t          IndexCount = 0;
        uint32_t          StartIndexLocation = 0;
        uint32_t          TextureIndex = 0;
        MaterialConstants Material;
    };

    struct alignas(16) PassConstants
    {
        DirectX::XMFLOAT4X4 World{};
        DirectX::XMFLOAT4X4 ViewProj{};
        DirectX::XMFLOAT4X4 InvViewProj{};
        DirectX::XMFLOAT4   EyePosW{ 0.f, 0.f, 0.f, 1.f };
        DirectX::XMFLOAT4   RenderTargetSize{ 1.f, 1.f, 1.f, 1.f };
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
        DirectX::XMFLOAT4 AmbientColor{ 0.05f, 0.05f, 0.06f, 1.f };
        DirectX::XMFLOAT4 LightCount{ 0.f, 0.f, 0.f, 0.f };
        GpuLight          Lights[MaxLights]{};
    };

private:
    bool CreateDevice();
    bool CreateCommandObjects();
    bool CreateSwapChain();
    bool CreateBackBufferHeap();
    bool CreateBackBufferRTVs();

    bool BuildShaders();
    bool BuildRootSignature();
    bool BuildPSOs();
    bool BuildGeometry();
    bool BuildFrameResources();

    void UpdatePassConstants();
    void UpdateLightConstants(float dt);
    void CreateSceneLights();

    void FlushCommandQueue();

    D3D12_CPU_DESCRIPTOR_HANDLE CurrentBackBufferRTV() const;
    ID3D12Resource*             CurrentBackBuffer()    const;

private:
    static constexpr uint32_t SwapChainBufferCount = 2;

    bool m_initialized = false;
    HWND m_hwnd = nullptr;
    uint32_t m_width = 0;
    uint32_t m_height = 0;

    Microsoft::WRL::ComPtr<IDXGIFactory4> m_factory;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    Microsoft::WRL::ComPtr<ID3D12CommandQueue> m_commandQueue;
    Microsoft::WRL::ComPtr<ID3D12CommandAllocator> m_commandAllocator;
    Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> m_commandList;

    Microsoft::WRL::ComPtr<ID3D12Fence> m_fence;
    uint64_t m_fenceValue = 0;
    HANDLE m_fenceEvent = nullptr;

    Microsoft::WRL::ComPtr<IDXGISwapChain> m_swapChain;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_backBuffers[SwapChainBufferCount];
    uint32_t m_backBufferIndex = 0;

    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_backBufferRtvHeap;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_textureHeap;

    uint32_t m_rtvDescriptorSize = 0;
    uint32_t m_srvDescriptorSize = 0;

    D3D12_VIEWPORT m_viewport{};
    D3D12_RECT m_scissorRect{};

    std::unique_ptr<GBuffer> m_gBuffer;

    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_rootSignature;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_geometryPSO;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_lightingPSO;

    Microsoft::WRL::ComPtr<ID3DBlob> m_geometryVS;
    Microsoft::WRL::ComPtr<ID3DBlob> m_geometryPS;
    Microsoft::WRL::ComPtr<ID3DBlob> m_lightingVS;
    Microsoft::WRL::ComPtr<ID3DBlob> m_lightingPS;

    D3D12_INPUT_ELEMENT_DESC m_inputLayout[3]{};

    Microsoft::WRL::ComPtr<ID3D12Resource> m_vertexBuffer;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_indexBuffer;
    D3D12_VERTEX_BUFFER_VIEW m_vertexBufferView{};
    D3D12_INDEX_BUFFER_VIEW m_indexBufferView{};

    std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>> m_textures;
    std::vector<DrawItem> m_drawItems;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_passConstantBuffer;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_lightConstantBuffer;
    uint8_t* m_mappedPassConstants = nullptr;
    uint8_t* m_mappedLightConstants = nullptr;

    std::vector<GpuLight> m_sceneLights;

    DirectX::XMFLOAT4X4 m_world{};
    DirectX::XMFLOAT4X4 m_view{};
    DirectX::XMFLOAT4X4 m_proj{};
    DirectX::XMFLOAT3 m_eyePos{ -5.f, 20.f, -5.f };

    float m_time = 0.f;

    std::vector<NBodyParticle> m_particles;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_bulbInstanceBuffer;
    uint8_t* m_mappedBulbInstances = nullptr;

    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_bulbRootSignature;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_bulbPSO;
    Microsoft::WRL::ComPtr<ID3DBlob> m_bulbVS;
    Microsoft::WRL::ComPtr<ID3DBlob> m_bulbPS;

    std::vector<uint32_t> m_lightIndices;

};
