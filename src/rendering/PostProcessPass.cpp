#include "PostProcessPass.h"

#include "AssetPath.h"
#include "Dx12Helpers.h"

#include <cstring>
#include <stdexcept>

using Microsoft::WRL::ComPtr;

namespace
{
    enum RootParam : uint32_t
    {
        RootConstants = 0,
        RootSceneColor = 1,
    };
}

bool PostProcessPass::Initialize(
    ID3D12Device* device,
    DXGI_FORMAT outputFormat,
    uint32_t width,
    uint32_t height)
{
    m_width = width;
    m_height = height;

    if (!BuildRootSignature(device)) return false;
    if (!BuildShaders()) return false;
    if (!BuildPipelineState(device, outputFormat)) return false;
    if (!BuildConstantBuffer(device)) return false;
    return true;
}

void PostProcessPass::Shutdown()
{
    if (m_mappedConstants)
    {
        m_constants->Unmap(0, nullptr);
        m_mappedConstants = nullptr;
    }

    m_constants.Reset();
    m_postProcessPs.Reset();
    m_fullscreenVs.Reset();
    m_pso.Reset();
    m_rootSig.Reset();
}

void PostProcessPass::OnResize(uint32_t width, uint32_t height)
{
    if (!width || !height)
        return;

    m_width = width;
    m_height = height;
}

void PostProcessPass::RecordCommands(
    ID3D12GraphicsCommandList* cmdList,
    D3D12_CPU_DESCRIPTOR_HANDLE outputRtv,
    D3D12_VIEWPORT viewport,
    D3D12_RECT scissorRect,
    ID3D12DescriptorHeap* sceneColorSrvHeap,
    D3D12_GPU_DESCRIPTOR_HANDLE sceneColorSrv,
    const Settings& settings)
{
    UpdateConstants(settings);

    cmdList->RSSetViewports(1, &viewport);
    cmdList->RSSetScissorRects(1, &scissorRect);
    cmdList->OMSetRenderTargets(1, &outputRtv, TRUE, nullptr);
    cmdList->SetGraphicsRootSignature(m_rootSig.Get());
    cmdList->SetPipelineState(m_pso.Get());
    cmdList->SetGraphicsRootConstantBufferView(RootConstants, m_constants->GetGPUVirtualAddress());

    ID3D12DescriptorHeap* heaps[] = {sceneColorSrvHeap};
    cmdList->SetDescriptorHeaps(1, heaps);
    cmdList->SetGraphicsRootDescriptorTable(RootSceneColor, sceneColorSrv);
    cmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cmdList->DrawInstanced(3, 1, 0, 0);
}

const wchar_t* PostProcessPass::EffectModeName(EffectMode mode)
{
    switch (mode)
    {
    case EffectMode::Halftoning:
        return L"Halftoning";
    case EffectMode::Outliner:
        return L"Outliner";
    case EffectMode::HalftoningOutliner:
        return L"Halftoning+Outliner";
    case EffectMode::Nothing:
    default:
        return L"Nothing";
    }
}

const wchar_t* PostProcessPass::ColorModeName(ColorMode mode)
{
    switch (mode)
    {
    case ColorMode::Hdr:
        return L"HDR";
    case ColorMode::Gamma:
        return L"Gamma";
    case ColorMode::HdrGamma:
        return L"HDR+Gamma";
    case ColorMode::Nothing:
    default:
        return L"Nothing";
    }
}

bool PostProcessPass::BuildRootSignature(ID3D12Device* device)
{
    D3D12_DESCRIPTOR_RANGE sceneColorRange =
        dx12::DescriptorRange(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0);

    D3D12_ROOT_PARAMETER params[2]{};
    dx12::SetRootCbv(params[RootConstants], 0, D3D12_SHADER_VISIBILITY_PIXEL);
    dx12::SetRootTable(params[RootSceneColor], sceneColorRange, D3D12_SHADER_VISIBILITY_PIXEL);

    D3D12_STATIC_SAMPLER_DESC samplers[2]{
        dx12::StaticSampler(
            0,
            D3D12_FILTER_MIN_MAG_MIP_LINEAR,
            D3D12_TEXTURE_ADDRESS_MODE_CLAMP,
            D3D12_SHADER_VISIBILITY_PIXEL),
        dx12::StaticSampler(
            1,
            D3D12_FILTER_MIN_MAG_MIP_POINT,
            D3D12_TEXTURE_ADDRESS_MODE_CLAMP,
            D3D12_SHADER_VISIBILITY_PIXEL),
    };

    D3D12_ROOT_SIGNATURE_DESC desc{};
    desc.NumParameters = static_cast<UINT>(_countof(params));
    desc.pParameters = params;
    desc.NumStaticSamplers = static_cast<UINT>(_countof(samplers));
    desc.pStaticSamplers = samplers;
    desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    return dx12::CreateRootSignature(device, desc, m_rootSig);
}

bool PostProcessPass::BuildShaders()
{
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif

    const std::wstring path = ToWide(ResolveAsset("Shaders/PostProcessComposite.hlsl"));
    ComPtr<ID3DBlob> errors;

    auto compile = [&](const char* entry, const char* target, ComPtr<ID3DBlob>& blob) -> bool {
        errors.Reset();
        const HRESULT hr = D3DCompileFromFile(
            path.c_str(),
            nullptr,
            D3D_COMPILE_STANDARD_FILE_INCLUDE,
            entry,
            target,
            flags,
            0,
            blob.GetAddressOf(),
            errors.GetAddressOf());
        if (FAILED(hr))
        {
            if (errors)
                throw std::runtime_error(static_cast<const char*>(errors->GetBufferPointer()));
            dx12::ThrowIfFailed(hr, entry);
        }
        return true;
    };

    return compile("FullscreenVS", "vs_5_0", m_fullscreenVs)
        && compile("PostProcessPS", "ps_5_0", m_postProcessPs);
}

bool PostProcessPass::BuildPipelineState(ID3D12Device* device, DXGI_FORMAT outputFormat)
{
    D3D12_RENDER_TARGET_BLEND_DESC targetBlend{};
    targetBlend.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;

    D3D12_BLEND_DESC blend{};
    blend.RenderTarget[0] = targetBlend;

    D3D12_RASTERIZER_DESC raster{};
    raster.FillMode = D3D12_FILL_MODE_SOLID;
    raster.CullMode = D3D12_CULL_MODE_NONE;
    raster.DepthClipEnable = TRUE;

    D3D12_DEPTH_STENCIL_DESC depth{};

    D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};
    desc.pRootSignature = m_rootSig.Get();
    desc.VS = dx12::ShaderBytecode(m_fullscreenVs.Get());
    desc.PS = dx12::ShaderBytecode(m_postProcessPs.Get());
    desc.BlendState = blend;
    desc.SampleMask = UINT_MAX;
    desc.RasterizerState = raster;
    desc.DepthStencilState = depth;
    desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    desc.NumRenderTargets = 1;
    desc.RTVFormats[0] = outputFormat;
    desc.SampleDesc.Count = 1;

    return SUCCEEDED(device->CreateGraphicsPipelineState(&desc, IID_PPV_ARGS(&m_pso)));
}

bool PostProcessPass::BuildConstantBuffer(ID3D12Device* device)
{
    const auto uploadHeap = dx12::HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
    const auto desc = dx12::BufferDesc(dx12::AlignConstantBufferSize(sizeof(Constants)));

    dx12::ThrowIfFailed(
        device->CreateCommittedResource(
            &uploadHeap,
            D3D12_HEAP_FLAG_NONE,
            &desc,
            D3D12_RESOURCE_STATE_GENERIC_READ,
            nullptr,
            IID_PPV_ARGS(&m_constants)),
        "PostProcess constants");

    D3D12_RANGE readRange{0, 0};
    return SUCCEEDED(m_constants->Map(0, &readRange, reinterpret_cast<void**>(&m_mappedConstants)));
}

void PostProcessPass::UpdateConstants(const Settings& settings)
{
    if (!m_mappedConstants)
        return;

    Constants constants{};
    constants.RenderTargetSize = {
        static_cast<float>(m_width),
        static_cast<float>(m_height),
        1.f / static_cast<float>(m_width),
        1.f / static_cast<float>(m_height)};
    constants.HalftoneParams = {72.f, 0.70f, 1.f, 0.f};
    constants.OutlineParams = {
        0.11f,
        2.15f,
        static_cast<float>(static_cast<int>(settings.Mode)),
        static_cast<float>(static_cast<int>(settings.Color))};
    constants.OutlineColor = {0.f, 0.f, 0.f, 1.f};
    constants.ColorParams = {0.75f, 2.13f, 1.04f, 1.04f}; // expo, gamma, satur, contr
    std::memcpy(m_mappedConstants, &constants, sizeof(constants));
}
