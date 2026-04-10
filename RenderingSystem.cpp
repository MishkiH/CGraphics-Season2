#include "RenderingSystem.h"
#include "DeferredScene.h"
#include "ScatterScene.h"
#include "AssetPath.h"
#include <stdexcept>

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
    void ThrowIfFailed(HRESULT hr, const char* msg)
    {
        if (FAILED(hr))
        {
            char buf[256];
            std::snprintf(buf, sizeof buf, "%s (hr=0x%08X)", msg, (unsigned)hr);
            throw std::runtime_error(buf);
        }
    }
}

RenderingSystem::RenderingSystem() = default;
RenderingSystem::~RenderingSystem() = default;

bool RenderingSystem::Initialize(HWND hwnd, uint32_t width, uint32_t height)
{
    m_hwnd = hwnd; m_width = width; m_height = height;

#if defined(_DEBUG)
    ComPtr<ID3D12Debug> debug;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug)))) debug->EnableDebugLayer();
#endif

    ThrowIfFailed(CreateDXGIFactory1(IID_PPV_ARGS(&m_factory)), "CreateDXGIFactory1");
    if (!CreateDevice()) return false;
    if (!CreateSwapChain()) return false;
    if (!CreateBackBufferRTVs()) return false;

    ThrowIfFailed(m_device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_fence)), "CreateFence");
    m_fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    if (!m_fenceEvent) throw std::runtime_error("CreateEvent failed");

    m_viewport = {0.f, 0.f, (float)width, (float)height, 0.f, 1.f};
    m_scissorRect = {0, 0, (LONG)width, (LONG)height};

    const XMFLOAT3 defaultEye{-5.f, 20.f, -5.f};
    SetCamera(defaultEye, 0.f, 0.f);
    XMStoreFloat4x4(&m_proj, XMMatrixPerspectiveFovLH(XM_PI * 0.25f, (float)width / height, 0.05f, 1000.f));

    m_deferredScene = std::make_unique<DeferredScene>();
    if (!m_deferredScene->Initialize(m_device.Get(), m_cmdQueue.Get(), DXGI_FORMAT_R8G8B8A8_UNORM, width, height))
        return false;

    m_scatterScene = std::make_unique<ScatterScene>();
    if (!m_scatterScene->Initialize(m_device.Get(), m_cmdQueue.Get(), DXGI_FORMAT_R8G8B8A8_UNORM, width, height,
                     ResolveAsset("Meshes/shrek/shrek.obj"), ResolveAsset("Meshes/donkey/Donkey.obj")))
    m_scatterScene.reset();

    m_initialized = true;
    return true;
}

void RenderingSystem::Shutdown()
{
    FlushGpu();
    if (m_deferredScene) { m_deferredScene->Shutdown(); m_deferredScene.reset(); }
    if (m_scatterScene) { m_scatterScene->Shutdown(); m_scatterScene.reset(); }
    if (m_fenceEvent) { CloseHandle(m_fenceEvent); m_fenceEvent = nullptr; }
}

void RenderingSystem::Draw(float dt)
{
    if (!m_initialized) return;

    m_deferredScene->SetCamera(m_view, m_proj, m_eye);

    BeginFrame();

    if (m_sceneMode == 1 && m_scatterScene)
    {
        const float clearColor[4] = {0.08f, 0.10f, 0.13f, 1.f};
        m_cmdList->ClearRenderTargetView(CurrentBackBufferRTV(), clearColor, 0, nullptr);

        XMFLOAT4X4 viewProj;
        XMStoreFloat4x4(&viewProj, XMLoadFloat4x4(&m_view) * XMLoadFloat4x4(&m_proj));
        m_scatterScene->RecordCommands(m_cmdList.Get(), viewProj, m_eye,
                                        CurrentBackBufferRTV(), m_viewport, m_scissorRect);
    }
    else
    {
        m_deferredScene->RecordCommands(m_cmdList.Get(), CurrentBackBufferRTV(),
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

    ThrowIfFailed(m_swapChain->ResizeBuffers(SwapChainBufferCount, width, height,
                    DXGI_FORMAT_R8G8B8A8_UNORM, 0), "ResizeBuffers");
    m_backBufferIndex = 0;
    CreateBackBufferRTVs();

    m_viewport = {0.f, 0.f, (float)width, (float)height, 0.f, 1.f};
    m_scissorRect = {0, 0, (LONG)width, (LONG)height};
    XMStoreFloat4x4(&m_proj, XMMatrixPerspectiveFovLH(XM_PI * 0.25f, (float)width / height, 0.05f, 1000.f));

    if (m_deferredScene) m_deferredScene->OnResize(m_device.Get(), width, height);
    if (m_scatterScene) m_scatterScene->OnResize(m_device.Get(), width, height);
}

void RenderingSystem::SetCamera(const XMFLOAT3& eye, float yaw, float pitch)
{
    m_eye = eye;
    float sy = sinf(yaw), cy = cosf(yaw), sp = sinf(pitch), cp = cosf(pitch);
    XMVECTOR fwd = XMVector3Normalize(XMVectorSet(sy * cp, sp, cy * cp, 0.f));
    XMStoreFloat4x4(&m_view, XMMatrixLookToLH(
        XMVectorSet(eye.x, eye.y, eye.z, 1.f), fwd, XMVectorSet(0.f, 1.f, 0.f, 0.f)));
}

void RenderingSystem::SetRenderMode(int mode)
{
    if (m_deferredScene) m_deferredScene->SetRenderMode(mode);
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

void RenderingSystem::BeginFrame()
{
    ThrowIfFailed(m_cmdAlloc->Reset(), "Reset allocator");
    ThrowIfFailed(m_cmdList->Reset(m_cmdAlloc.Get(), nullptr), "Reset list");

    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = CurrentBackBuffer().Get();
    b.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
    b.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    m_cmdList->ResourceBarrier(1, &b);
}

void RenderingSystem::EndFrame()
{
    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = CurrentBackBuffer().Get();
    b.Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
    b.Transition.StateAfter = D3D12_RESOURCE_STATE_PRESENT;
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    m_cmdList->ResourceBarrier(1, &b);

    ThrowIfFailed(m_cmdList->Close(), "Close command list");
    ID3D12CommandList* lists[] = {m_cmdList.Get()};
    m_cmdQueue->ExecuteCommandLists(1, lists);
    ThrowIfFailed(m_swapChain->Present(0, 0), "Present");
    m_backBufferIndex = (m_backBufferIndex + 1) % SwapChainBufferCount;
    FlushGpu();
}

void RenderingSystem::FlushGpu()
{
    const uint64_t val = ++m_fenceValue;
    ThrowIfFailed(m_cmdQueue->Signal(m_fence.Get(), val), "Signal");
    if (m_fence->GetCompletedValue() < val)
    {
        ThrowIfFailed(m_fence->SetEventOnCompletion(val, m_fenceEvent), "SetEventOnCompletion");
        WaitForSingleObject(m_fenceEvent, INFINITE);
    }
}

bool RenderingSystem::CreateDevice()
{
    HRESULT hr = D3D12CreateDevice(nullptr, D3D_FEATURE_LEVEL_12_0, IID_PPV_ARGS(&m_device));
    if (FAILED(hr))
    {
        ComPtr<IDXGIAdapter> warp;
        ThrowIfFailed(m_factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
        ThrowIfFailed(D3D12CreateDevice(warp.Get(), D3D_FEATURE_LEVEL_12_0, IID_PPV_ARGS(&m_device)), "D3D12CreateDevice (WARP)");
    }
    D3D12_COMMAND_QUEUE_DESC qd{D3D12_COMMAND_LIST_TYPE_DIRECT};
    ThrowIfFailed(m_device->CreateCommandQueue(&qd, IID_PPV_ARGS(&m_cmdQueue)), "CreateCommandQueue");
    ThrowIfFailed(m_device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&m_cmdAlloc)), "CreateCommandAllocator");
    ThrowIfFailed(m_device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, m_cmdAlloc.Get(), nullptr, IID_PPV_ARGS(&m_cmdList)), "CreateCommandList");
    ThrowIfFailed(m_cmdList->Close(), "Initial cmdList close");
    return true;
}

bool RenderingSystem::CreateSwapChain()
{
    DXGI_SWAP_CHAIN_DESC sd{};
    sd.BufferCount = SwapChainBufferCount;
    sd.BufferDesc.Width = m_width; sd.BufferDesc.Height = m_height;
    sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM; sd.BufferDesc.RefreshRate = {60, 1};
    sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    sd.OutputWindow = m_hwnd; sd.SampleDesc = {1, 0};
    sd.Windowed = TRUE; sd.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
    ThrowIfFailed(m_factory->CreateSwapChain(m_cmdQueue.Get(), &sd, m_swapChain.GetAddressOf()), "CreateSwapChain");
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
        ThrowIfFailed(m_device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_rtvHeap)), "RTV heap");
    }
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < SwapChainBufferCount; ++i)
    {
        ThrowIfFailed(m_swapChain->GetBuffer(i, IID_PPV_ARGS(&m_backBuffers[i])), "GetBuffer");
        m_device->CreateRenderTargetView(m_backBuffers[i].Get(), nullptr, h);
        h.ptr += m_rtvStride;
    }
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
