#pragma once

#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include <cstdint>

class PostProcessPass
{
public:
    enum class EffectMode : int
    {
        Nothing = 0,
        Halftoning = 1,
        Outliner = 2,
        HalftoningOutliner = 3,
    };

    enum class ColorMode : int
    {
        Nothing = 0,
        Hdr = 1,
        Gamma = 2,
        HdrGamma = 3,
    };

    struct Settings
    {
        EffectMode Mode = EffectMode::Nothing;
        ColorMode Color = ColorMode::HdrGamma;
    };

    bool Initialize(ID3D12Device* device, DXGI_FORMAT outputFormat, uint32_t width, uint32_t height);
    void Shutdown();
    void OnResize(uint32_t width, uint32_t height);

    void RecordCommands(ID3D12GraphicsCommandList* cmdList,
                        D3D12_CPU_DESCRIPTOR_HANDLE outputRtv,
                        D3D12_VIEWPORT viewport,
                        D3D12_RECT scissorRect,
                        ID3D12DescriptorHeap* sceneColorSrvHeap,
                        D3D12_GPU_DESCRIPTOR_HANDLE sceneColorSrv,
                        const Settings& settings);

    static const wchar_t* EffectModeName(EffectMode mode);
    static const wchar_t* ColorModeName(ColorMode mode);

private:
    struct alignas(16) Constants
    {
        DirectX::XMFLOAT4 RenderTargetSize{1.f, 1.f, 1.f, 1.f};
        DirectX::XMFLOAT4 HalftoneParams{72.f, 0.70f, 1.f, 0.f};
        DirectX::XMFLOAT4 OutlineParams{0.12f, 3.75f, 0.f, 3.f};
        DirectX::XMFLOAT4 OutlineColor{0.f, 0.f, 0.f, 1.f};
        DirectX::XMFLOAT4 ColorParams{0.98f, 1.95f, 1.22f, 1.10f};
    };

    bool BuildRootSignature(ID3D12Device* device);
    bool BuildShaders();
    bool BuildPipelineState(ID3D12Device* device, DXGI_FORMAT outputFormat);
    bool BuildConstantBuffer(ID3D12Device* device);
    void UpdateConstants(const Settings& settings);

    uint32_t m_width = 1;
    uint32_t m_height = 1;

    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_rootSig;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_pso;
    Microsoft::WRL::ComPtr<ID3DBlob> m_fullscreenVs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_postProcessPs;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_constants;
    uint8_t* m_mappedConstants = nullptr;
};
