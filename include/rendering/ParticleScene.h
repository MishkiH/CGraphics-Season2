#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <wrl.h>
#include <d3dcommon.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <DirectXMath.h>
#include <cstdint>
#include <vector>

class ParticleScene
{
public:
    static constexpr uint32_t MaxParticles = 4096;

    bool Initialize(ID3D12Device* device, ID3D12CommandQueue* cmdQueue,
                    DXGI_FORMAT backBufferFmt, uint32_t width, uint32_t height);

    void Shutdown();
    void OnResize(ID3D12Device* device, uint32_t width, uint32_t height);

    void RecordCommands(ID3D12GraphicsCommandList* cmdList,
                        const DirectX::XMFLOAT4X4& view,
                        const DirectX::XMFLOAT4X4& proj,
                        D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                        D3D12_VIEWPORT viewport,
                        D3D12_RECT scissorRect,
                        float dt);

private:
    struct alignas(16) ParticleData
    {
        DirectX::XMFLOAT3 Position{0.f, 0.f, 0.f};
        float Age = 0.f;
        DirectX::XMFLOAT3 Velocity{0.f, 0.f, 0.f};
        float Life = 0.f;
        DirectX::XMFLOAT4 Color{1.f, 1.f, 1.f, 1.f};
        float Size = 1.f;
        DirectX::XMFLOAT3 Padding{0.f, 0.f, 0.f};
    };

    struct alignas(256) SceneConstants
    {
        DirectX::XMFLOAT4X4 ViewProj{};
        DirectX::XMFLOAT4 CameraRight{1.f, 0.f, 0.f, 0.f};
        DirectX::XMFLOAT4 CameraUp{0.f, 1.f, 0.f, 0.f};
        DirectX::XMFLOAT4 CameraFacing{0.f, 0.f, -1.f, 0.f};
        DirectX::XMFLOAT4 LightDirection{0.42f, -1.f, 0.32f, 0.f};
        DirectX::XMFLOAT4 LightColor{0.96f, 0.94f, 0.90f, 1.f};
        DirectX::XMFLOAT4 AmbientColor{0.38f, 0.40f, 0.44f, 1.f};
    };

    struct alignas(16) DrawConstants
    {
        DirectX::XMFLOAT4X4 World{};
        DirectX::XMFLOAT4 BaseColor{1.f, 1.f, 1.f, 1.f};
    };

    struct alignas(16) UpdateConstants
    {
        float DeltaTime = 0.f;
        float TotalTime = 0.f;
        uint32_t EmitCount = 0u;
        uint32_t MaxParticles = 0u;

        DirectX::XMFLOAT3 EmitterPosition{0.f, 0.f, 0.f};
        float SpawnRadius = 0.18f;

        DirectX::XMFLOAT3 InitialVelocity{0.f, 2.8f, 0.f};
        float VelocityJitter = 1.45f;

        DirectX::XMFLOAT3 Gravity{0.f, -7.2f, 0.f};
        float BaseSize = 0.08f;
    };

    struct BufferWithCounter
    {
        Microsoft::WRL::ComPtr<ID3D12Resource> Buffer;
        Microsoft::WRL::ComPtr<ID3D12Resource> Counter;
        D3D12_GPU_DESCRIPTOR_HANDLE SrvGpu{};
        D3D12_GPU_DESCRIPTOR_HANDLE UavGpu{};
        D3D12_RESOURCE_STATES BufferState = D3D12_RESOURCE_STATE_COMMON;
        D3D12_RESOURCE_STATES CounterState = D3D12_RESOURCE_STATE_COMMON;
    };

    struct MeshDraw
    {
        uint32_t IndexStart = 0;
        uint32_t IndexCount = 0;
        DirectX::XMFLOAT4 BaseColor{1.f, 1.f, 1.f, 1.f};
    };

    struct MeshGpu
    {
        Microsoft::WRL::ComPtr<ID3D12Resource> VertexBuffer;
        Microsoft::WRL::ComPtr<ID3D12Resource> IndexBuffer;
        D3D12_VERTEX_BUFFER_VIEW Vbv{};
        D3D12_INDEX_BUFFER_VIEW Ibv{};
        std::vector<MeshDraw> Draws;
    };

    bool BuildShaders();
    bool BuildDescriptorHeap(ID3D12Device* device);
    bool BuildParticleBuffers(ID3D12Device* device, ID3D12GraphicsCommandList* uploadList);
    bool BuildGeometry(ID3D12Device* device,
                       ID3D12GraphicsCommandList* uploadList,
                       std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>>& uploads);
    bool BuildConstantBuffer(ID3D12Device* device);
    bool BuildRootSignatures(ID3D12Device* device);
    bool BuildPipelineStates(ID3D12Device* device, DXGI_FORMAT backBufferFmt);
    bool BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height);

    void UpdateSceneConstants(const DirectX::XMFLOAT4X4& view, const DirectX::XMFLOAT4X4& proj);
    DrawConstants BuildDrawConstants(const DirectX::XMFLOAT4X4& world,
                                     const DirectX::XMFLOAT4& baseColor) const;
    UpdateConstants BuildUpdateConstants(float dt, uint32_t emitCount) const;

    void ResetCounter(ID3D12GraphicsCommandList* cmdList, BufferWithCounter& buffer);
    void CopyLiveCount(ID3D12GraphicsCommandList* cmdList, BufferWithCounter& buffer);
    void UpdateParticles(ID3D12GraphicsCommandList* cmdList, float dt);
    void DrawMesh(ID3D12GraphicsCommandList* cmdList,
                  const MeshGpu& mesh,
                  const DirectX::XMFLOAT4X4& world);
    void RenderScene(ID3D12GraphicsCommandList* cmdList,
                     D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                     D3D12_VIEWPORT viewport,
                     D3D12_RECT scissorRect);

    uint32_t m_descriptorStride = 0;
    uint32_t m_currentBufferIndex = 0;
    float m_time = 0.f;
    float m_emitAccumulator = 0.f;
    float m_bunnyHeight = 2.35f;
    DirectX::XMFLOAT3 m_emitterPosition{0.f, 1.65f, 0.f};
    DirectX::XMFLOAT4X4 m_bunnyWorld{};
    DirectX::XMFLOAT4X4 m_floorWorld{};

    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_graphicsRootSig;
    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_computeRootSig;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_meshPso;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_particlePso;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_updatePso;

    Microsoft::WRL::ComPtr<ID3DBlob> m_meshVs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_meshPs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_particleVs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_particleGs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_particlePs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_updateCs;

    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_descriptorHeap;
    BufferWithCounter m_particleBuffers[2];
    Microsoft::WRL::ComPtr<ID3D12Resource> m_liveCountBuffer;
    D3D12_GPU_DESCRIPTOR_HANDLE m_liveCountSrvGpu{};
    D3D12_RESOURCE_STATES m_liveCountState = D3D12_RESOURCE_STATE_COMMON;

    MeshGpu m_bunnyMesh;
    MeshGpu m_floorMesh;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_sceneCB;
    uint8_t* m_mappedSceneCB = nullptr;

    Microsoft::WRL::ComPtr<ID3D12Resource> m_zeroUpload;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_depthBuffer;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_dsvHeap;
};
