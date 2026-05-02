#include "RenderingSystem.h"
#include "DeferredScene.h"
#include "ParticleScene.h"
#include "ScatterScene.h"
#include "AssetPath.h"
#include "Dx12Helpers.h"
#include "SceneProfiles.h"

using namespace DirectX;
using Microsoft::WRL::ComPtr;

#pragma comment(lib, "d3d12.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "dxguid.lib")
#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "windowscodecs.lib")
#pragma comment(lib, "ole32.lib")

namespace
{
    constexpr DXGI_FORMAT kBackBufferFormat = DXGI_FORMAT_R8G8B8A8_UNORM;
    constexpr D3D_FEATURE_LEVEL kMinFeatureLevel = D3D_FEATURE_LEVEL_12_0;

    bool IsHardwareAdapter(IDXGIAdapter1* adapter)
    {
        DXGI_ADAPTER_DESC1 desc{};
        if (FAILED(adapter->GetDesc1(&desc)))
            return false;

        return (desc.Flags & DXGI_ADAPTER_FLAG_SOFTWARE) == 0;
    }

    bool SupportsD3D12(IDXGIAdapter1* adapter)
    {
        return SUCCEEDED(D3D12CreateDevice(adapter, kMinFeatureLevel, __uuidof(ID3D12Device), nullptr));
    }

    ComPtr<IDXGIAdapter1> PickHighPerformanceAdapter(IDXGIFactory4* factory)
    {
        ComPtr<IDXGIFactory6> factory6;
        if (SUCCEEDED(factory->QueryInterface(IID_PPV_ARGS(&factory6))))
        {
            for (UINT i = 0;; ++i)
            {
                ComPtr<IDXGIAdapter1> adapter;
                const HRESULT hr = factory6->EnumAdapterByGpuPreference(
                    i,
                    DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE,
                    IID_PPV_ARGS(&adapter));
                if (hr == DXGI_ERROR_NOT_FOUND)
                    break;

                if (SUCCEEDED(hr) && IsHardwareAdapter(adapter.Get()) && SupportsD3D12(adapter.Get()))
                    return adapter;
            }
        }

        for (UINT i = 0;; ++i)
        {
            ComPtr<IDXGIAdapter1> adapter;
            const HRESULT hr = factory->EnumAdapters1(i, adapter.GetAddressOf());
            if (hr == DXGI_ERROR_NOT_FOUND)
                break;

            if (SUCCEEDED(hr) && IsHardwareAdapter(adapter.Get()) && SupportsD3D12(adapter.Get()))
                return adapter;
        }

        return {};
    }

    int BuildRenderMode(bool useNormalMapping, bool useDisplacement)
    {
        int mode = DeferredScene::RenderFeatureNone;
        if (useNormalMapping) mode |= DeferredScene::RenderFeatureNormalMapping;
        if (useDisplacement) mode |= DeferredScene::RenderFeatureDisplacement;
        return mode;
    }

    bool InitializeDeferredScene(
        ID3D12Device* device,
        ID3D12CommandQueue* cmdQueue,
        uint32_t width,
        uint32_t height,
        std::unique_ptr<DeferredScene>& scene,
        const DeferredScene::SceneOptions& options)
    {
        scene = std::make_unique<DeferredScene>();
        return scene->Initialize(device, cmdQueue, kBackBufferFormat, width, height, options);
    }
}

RenderingSystem::RenderingSystem() = default;

RenderingSystem::~RenderingSystem()
{
    Shutdown();
}

bool RenderingSystem::Initialize(HWND hwnd, uint32_t width, uint32_t height)
{
    m_hwnd = hwnd; m_width = width; m_height = height;

#if defined(_DEBUG)
    ComPtr<ID3D12Debug> debug;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug)))) debug->EnableDebugLayer();
#endif

    dx12::ThrowIfFailed(CreateDXGIFactory1(IID_PPV_ARGS(&m_factory)), "CreateDXGIFactory1");
    if (!CreateDevice()) return false;
    if (!CreateSwapChain()) return false;
    if (!CreateBackBufferRTVs()) return false;

    dx12::ThrowIfFailed(m_device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_fence)), "CreateFence");
    m_fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    if (!m_fenceEvent) throw std::runtime_error("CreateEvent failed");

    m_viewport = {0.f, 0.f, (float)width, (float)height, 0.f, 1.f};
    m_scissorRect = {0, 0, (LONG)width, (LONG)height};

    const XMFLOAT3 defaultEye{-5.f, 20.f, -5.f};
    SetCamera(defaultEye, 0.f, 0.f);
    UpdateProjectionMatrix();

    if (!InitializeDeferredScene(
            m_device.Get(),
            m_cmdQueue.Get(),
            width,
            height,
            m_handScene,
            scene_profiles::MakeHandSceneOptions()))
        return false;

    if (!InitializeDeferredScene(
            m_device.Get(),
            m_cmdQueue.Get(),
            width,
            height,
            m_sponzaScene,
            scene_profiles::MakeSponzaSceneOptions()))
        return false;

    m_scatterScene = std::make_unique<ScatterScene>();
    if (!m_scatterScene->Initialize(m_device.Get(), m_cmdQueue.Get(), kBackBufferFormat, width, height,
                     ResolveAsset("Meshes/shrek/shrek.obj"), ResolveAsset("Meshes/donkey/Donkey.obj")))
        return false;

    m_particleScene = std::make_unique<ParticleScene>();
    if (!m_particleScene->Initialize(m_device.Get(), m_cmdQueue.Get(), kBackBufferFormat, width, height))
        return false;

    m_initialized = true;
    return true;
}

void RenderingSystem::Shutdown()
{
    if (!m_device)
        return;

    if (m_initialized)
        FlushGpu();

    if (m_handScene) { m_handScene->Shutdown(); m_handScene.reset(); }
    if (m_sponzaScene) { m_sponzaScene->Shutdown(); m_sponzaScene.reset(); }
    if (m_scatterScene) { m_scatterScene->Shutdown(); m_scatterScene.reset(); }
    if (m_particleScene) { m_particleScene->Shutdown(); m_particleScene.reset(); }
    if (m_fenceEvent) { CloseHandle(m_fenceEvent); m_fenceEvent = nullptr; }
    m_cmdList.Reset();
    m_cmdAlloc.Reset();
    m_cmdQueue.Reset();
    m_fence.Reset();
    m_swapChain.Reset();
    m_rtvHeap.Reset();
    for (auto& buffer : m_backBuffers) buffer.Reset();
    for (auto& state : m_backBufferStates) state = D3D12_RESOURCE_STATE_COMMON;
    m_device.Reset();
    m_factory.Reset();
    m_initialized = false;
}

void RenderingSystem::Draw(float dt)
{
    if (!m_initialized) return;

    SyncDeferredSceneCameras();

    BeginFrame();

    if (m_sceneMode == ScatterSceneMode && m_scatterScene)
    {
        m_scatterScene->RecordCommands(m_cmdList.Get(), m_view, m_proj, m_eye,
                                        CurrentBackBufferRTV(), m_viewport, m_scissorRect, dt);
    }
    else if (m_sceneMode == ParticleSceneMode && m_particleScene)
    {
        m_particleScene->RecordCommands(
            m_cmdList.Get(),
            m_view,
            m_proj,
            CurrentBackBufferRTV(),
            m_viewport,
            m_scissorRect,
            dt);
    }
    else if (m_sceneMode == SponzaSceneMode && m_sponzaScene)
    {
        m_sponzaScene->RecordCommands(m_cmdList.Get(), CurrentBackBufferRTV(),
                                       m_viewport, m_scissorRect, dt);
    }
    else
    {
        if (m_handScene)
            m_handScene->RecordCommands(m_cmdList.Get(), CurrentBackBufferRTV(),
                                         m_viewport, m_scissorRect, dt);
    }

    EndFrame();
}

void RenderingSystem::OnResize(uint32_t width, uint32_t height)
{
    if (!m_initialized || !width || !height) return;
    m_width = width; m_height = height;

    FlushGpu();
    for (auto& b : m_backBuffers) b.Reset();

    dx12::ThrowIfFailed(m_swapChain->ResizeBuffers(SwapChainBufferCount, width, height,
                    kBackBufferFormat, 0), "ResizeBuffers");
    m_backBufferIndex = 0;
    CreateBackBufferRTVs();

    m_viewport = {0.f, 0.f, (float)width, (float)height, 0.f, 1.f};
    m_scissorRect = {0, 0, (LONG)width, (LONG)height};
    UpdateProjectionMatrix();

    if (m_handScene) m_handScene->OnResize(m_device.Get(), width, height);
    if (m_sponzaScene) m_sponzaScene->OnResize(m_device.Get(), width, height);
    if (m_scatterScene) m_scatterScene->OnResize(m_device.Get(), width, height);
    if (m_particleScene) m_particleScene->OnResize(m_device.Get(), width, height);
}

void RenderingSystem::SetCamera(const XMFLOAT3& eye, float yaw, float pitch)
{
    m_eye = eye;
    float sy = sinf(yaw), cy = cosf(yaw), sp = sinf(pitch), cp = cosf(pitch);
    XMVECTOR fwd = XMVector3Normalize(XMVectorSet(sy * cp, sp, cy * cp, 0.f));
    XMStoreFloat4x4(&m_view, XMMatrixLookToLH(
        XMVectorSet(eye.x, eye.y, eye.z, 1.f), fwd, XMVectorSet(0.f, 1.f, 0.f, 0.f)));
}

void RenderingSystem::SetProjectionClipRange(float nearClip, float farClip)
{
    if (nearClip <= 0.f || farClip <= nearClip)
        return;

    m_nearClip = nearClip;
    m_farClip = farClip;
    UpdateProjectionMatrix();
}

void RenderingSystem::SetHandFeatures(bool useNormalMapping, bool useDisplacement)
{
    if (DeferredScene* scene = GetDeferredScene(HandSceneMode))
        scene->SetRenderMode(BuildRenderMode(useNormalMapping, useDisplacement));
}

void RenderingSystem::SetSponzaFeatures(bool useNormalMapping)
{
    if (DeferredScene* scene = GetDeferredScene(SponzaSceneMode))
        scene->SetRenderMode(BuildRenderMode(useNormalMapping, false));
}

void RenderingSystem::SetSponzaUvEffectsEnabled(bool enabled)
{
    if (DeferredScene* scene = GetDeferredScene(SponzaSceneMode))
        scene->SetUvEffectsEnabled(enabled);
}

bool RenderingSystem::SponzaUvEffectsEnabled() const
{
    const DeferredScene* scene = GetDeferredScene(SponzaSceneMode);
    return scene ? scene->UvEffectsEnabled() : false;
}

void RenderingSystem::SetSceneMode(int mode)
{
    if (mode < 0) mode = 0;
    m_sceneMode = mode % SceneModeCount;
}

void RenderingSystem::ToggleFrustumCulling()
{
    if (m_scatterScene) m_scatterScene->SetFrustumCulling(!m_scatterScene->FrustumCullingEnabled());
}

void RenderingSystem::ToggleOctreeCulling()
{
    if (m_scatterScene) m_scatterScene->SetOctreeCulling(!m_scatterScene->OctreeCullingEnabled());
}

bool RenderingSystem::FrustumCullingEnabled() const
{
    return m_scatterScene ? m_scatterScene->FrustumCullingEnabled() : false;
}

bool RenderingSystem::OctreeCullingEnabled() const
{
    return m_scatterScene ? m_scatterScene->OctreeCullingEnabled() : false;
}

uint32_t RenderingSystem::ScatterVisibleCount() const
{
    return m_scatterScene ? m_scatterScene->LastVisibleCount() : 0u;
}

bool RenderingSystem::DropParticleSceneCage()
{
    return m_particleScene && m_particleScene->DropPrisonCage();
}

bool RenderingSystem::ParticleSceneCageVisible() const
{
    return m_particleScene && m_particleScene->IsPrisonCageVisible();
}

void RenderingSystem::SyncDeferredSceneCameras()
{
    if (m_handScene) m_handScene->SetCamera(m_view, m_proj, m_eye);
    if (m_sponzaScene) m_sponzaScene->SetCamera(m_view, m_proj, m_eye);
}

void RenderingSystem::UpdateProjectionMatrix()
{
    if (!m_width || !m_height)
        return;

    XMStoreFloat4x4(
        &m_proj,
        XMMatrixPerspectiveFovLH(XM_PI * 0.25f, (float)m_width / m_height, m_nearClip, m_farClip));
}

void RenderingSystem::BeginFrame()
{
    m_backBufferIndex = m_swapChain->GetCurrentBackBufferIndex();
    dx12::ThrowIfFailed(m_cmdAlloc->Reset(), "Reset allocator");
    dx12::ThrowIfFailed(m_cmdList->Reset(m_cmdAlloc.Get(), nullptr), "Reset list");

    TransitionCurrentBackBuffer(D3D12_RESOURCE_STATE_RENDER_TARGET);
}

void RenderingSystem::EndFrame()
{
    TransitionCurrentBackBuffer(D3D12_RESOURCE_STATE_PRESENT);

    dx12::ThrowIfFailed(m_cmdList->Close(), "Close command list");
    ID3D12CommandList* lists[] = {m_cmdList.Get()};
    m_cmdQueue->ExecuteCommandLists(1, lists);
    dx12::ThrowIfFailed(m_swapChain->Present(0, 0), "Present");
    m_backBufferIndex = m_swapChain->GetCurrentBackBufferIndex();
    FlushGpu();
}

void RenderingSystem::TransitionCurrentBackBuffer(D3D12_RESOURCE_STATES newState)
{
    D3D12_RESOURCE_STATES& currentState = m_backBufferStates[m_backBufferIndex];
    if (currentState == newState)
        return;

    D3D12_RESOURCE_BARRIER barrier{};
    barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    barrier.Transition.pResource = CurrentBackBuffer().Get();
    barrier.Transition.StateBefore = currentState;
    barrier.Transition.StateAfter = newState;
    barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    m_cmdList->ResourceBarrier(1, &barrier);
    currentState = newState;
}

void RenderingSystem::FlushGpu()
{
    const uint64_t val = ++m_fenceValue;
    dx12::ThrowIfFailed(m_cmdQueue->Signal(m_fence.Get(), val), "Signal");
    if (m_fence->GetCompletedValue() < val)
    {
        dx12::ThrowIfFailed(m_fence->SetEventOnCompletion(val, m_fenceEvent), "SetEventOnCompletion");
        WaitForSingleObject(m_fenceEvent, INFINITE);
    }
}

bool RenderingSystem::CreateDevice()
{
    ComPtr<IDXGIAdapter1> adapter = PickHighPerformanceAdapter(m_factory.Get());
    HRESULT hr = adapter
        ? D3D12CreateDevice(adapter.Get(), kMinFeatureLevel, IID_PPV_ARGS(&m_device))
        : D3D12CreateDevice(nullptr, kMinFeatureLevel, IID_PPV_ARGS(&m_device));
    if (FAILED(hr))
    {
        ComPtr<IDXGIAdapter> warp;
        dx12::ThrowIfFailed(m_factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
        dx12::ThrowIfFailed(D3D12CreateDevice(warp.Get(), kMinFeatureLevel, IID_PPV_ARGS(&m_device)), "D3D12CreateDevice (WARP)");
    }
    D3D12_COMMAND_QUEUE_DESC qd{D3D12_COMMAND_LIST_TYPE_DIRECT};
    dx12::ThrowIfFailed(m_device->CreateCommandQueue(&qd, IID_PPV_ARGS(&m_cmdQueue)), "CreateCommandQueue");
    dx12::ThrowIfFailed(m_device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&m_cmdAlloc)), "CreateCommandAllocator");
    dx12::ThrowIfFailed(m_device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, m_cmdAlloc.Get(), nullptr, IID_PPV_ARGS(&m_cmdList)), "CreateCommandList");
    dx12::ThrowIfFailed(m_cmdList->Close(), "Initial cmdList close");
    return true;
}

bool RenderingSystem::CreateSwapChain()
{
    DXGI_SWAP_CHAIN_DESC1 desc{};
    desc.Width = m_width;
    desc.Height = m_height;
    desc.Format = kBackBufferFormat;
    desc.SampleDesc.Count = 1;
    desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    desc.BufferCount = SwapChainBufferCount;
    desc.Scaling = DXGI_SCALING_STRETCH;
    desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
    desc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;

    ComPtr<IDXGISwapChain1> swapChain;
    dx12::ThrowIfFailed(
        m_factory->CreateSwapChainForHwnd(
            m_cmdQueue.Get(),
            m_hwnd,
            &desc,
            nullptr,
            nullptr,
            swapChain.GetAddressOf()),
        "CreateSwapChainForHwnd");
    dx12::ThrowIfFailed(m_factory->MakeWindowAssociation(m_hwnd, DXGI_MWA_NO_ALT_ENTER), "MakeWindowAssociation");
    dx12::ThrowIfFailed(swapChain.As(&m_swapChain), "Query IDXGISwapChain3");
    m_backBufferIndex = m_swapChain->GetCurrentBackBufferIndex();
    return true;
}

bool RenderingSystem::CreateBackBufferRTVs()
{
    m_rtvStride = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    if (!m_rtvHeap)
    {
        D3D12_DESCRIPTOR_HEAP_DESC hd{};
        hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
        hd.NumDescriptors = SwapChainBufferCount;
        dx12::ThrowIfFailed(m_device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_rtvHeap)), "RTV heap");
    }
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < SwapChainBufferCount; ++i)
    {
        dx12::ThrowIfFailed(m_swapChain->GetBuffer(i, IID_PPV_ARGS(&m_backBuffers[i])), "GetBuffer");
        m_device->CreateRenderTargetView(m_backBuffers[i].Get(), nullptr, h);
        h.ptr += m_rtvStride;
        m_backBufferStates[i] = D3D12_RESOURCE_STATE_PRESENT;
    }
    m_backBufferIndex = m_swapChain->GetCurrentBackBufferIndex();
    return true;
}

D3D12_CPU_DESCRIPTOR_HANDLE RenderingSystem::CurrentBackBufferRTV() const
{
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    h.ptr += (SIZE_T)m_backBufferIndex * m_rtvStride;
    return h;
}

ComPtr<ID3D12Resource>& RenderingSystem::CurrentBackBuffer()
{
    return m_backBuffers[m_backBufferIndex];
}

DeferredScene* RenderingSystem::GetDeferredScene(SceneMode mode)
{
    switch (mode)
    {
    case HandSceneMode:
        return m_handScene.get();
    case SponzaSceneMode:
        return m_sponzaScene.get();
    default:
        return nullptr;
    }
}

const DeferredScene* RenderingSystem::GetDeferredScene(SceneMode mode) const
{
    switch (mode)
    {
    case HandSceneMode:
        return m_handScene.get();
    case SponzaSceneMode:
        return m_sponzaScene.get();
    default:
        return nullptr;
    }
}
