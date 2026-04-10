#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <DirectXMath.h>
#include <cstdint>
#include <memory>

class DeferredScene;
class ScatterScene;

// Owns device, swap chain, and back buffers. Delegates rendering to DeferredScene or ScatterScene.
class RenderingSystem
{
public:
    RenderingSystem();
    ~RenderingSystem();

    bool Initialize(HWND hwnd, uint32_t width, uint32_t height);
    void Shutdown();
    void OnResize(uint32_t width, uint32_t height);
    void Draw(float dt);
    void SetCamera(const DirectX::XMFLOAT3& eye, float yaw, float pitch);

    // 0 = deferred (hand + water), 1 = scatter (300 objects)
    void SetSceneMode(int mode) { m_sceneMode = mode; }
    int GetSceneMode() const { return m_sceneMode; }

    void SetRenderMode(int mode);

    void ToggleFrustumCulling();
    void ToggleOctreeCulling();
    bool FrustumCullingEnabled() const;
    bool OctreeCullingEnabled() const;
    uint32_t ScatterVisibleCount() const;

private:
    void BeginFrame();
    void EndFrame();
    bool CreateDevice();
    bool CreateSwapChain();
    bool CreateBackBufferRTVs();
    void FlushGpu();

    D3D12_CPU_DESCRIPTOR_HANDLE CurrentBackBufferRTV() const;
    Microsoft::WRL::ComPtr<ID3D12Resource>& CurrentBackBuffer();

    static constexpr uint32_t SwapChainBufferCount = 2;

    bool m_initialized = false;
    HWND m_hwnd = nullptr;
    uint32_t m_width = 0;
    uint32_t m_height = 0;
    int m_sceneMode = 0;

    Microsoft::WRL::ComPtr<IDXGIFactory4> m_factory;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    Microsoft::WRL::ComPtr<ID3D12CommandQueue> m_cmdQueue;
    Microsoft::WRL::ComPtr<ID3D12CommandAllocator> m_cmdAlloc;
    Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> m_cmdList;
    Microsoft::WRL::ComPtr<ID3D12Fence> m_fence;
    uint64_t m_fenceValue = 0;
    HANDLE m_fenceEvent = nullptr;

    Microsoft::WRL::ComPtr<IDXGISwapChain> m_swapChain;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_backBuffers[SwapChainBufferCount];
    uint32_t m_backBufferIndex = 0;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_rtvHeap;
    uint32_t m_rtvStride = 0;

    D3D12_VIEWPORT m_viewport{};
    D3D12_RECT m_scissorRect{};

    DirectX::XMFLOAT4X4 m_view{};
    DirectX::XMFLOAT4X4 m_proj{};
    DirectX::XMFLOAT3 m_eye{};

    std::unique_ptr<DeferredScene> m_deferredScene;
    std::unique_ptr<ScatterScene> m_scatterScene;
};
