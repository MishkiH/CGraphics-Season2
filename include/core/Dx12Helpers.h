#pragma once

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <stdexcept>
#include <vector>
#include <wrl.h>
#include <d3d12.h>
#include <d3dcommon.h>

#include "ImageLoader.h"

namespace dx12
{
    inline void ThrowIfFailed(HRESULT hr, const char* what)
    {
        if (FAILED(hr))
        {
            char buffer[256];
            std::snprintf(buffer, sizeof(buffer), "%s (hr=0x%08X)", what, static_cast<unsigned>(hr));
            throw std::runtime_error(buffer);
        }
    }

    inline uint32_t AlignConstantBufferSize(uint32_t size)
    {
        return (size + 255u) & ~255u;
    }

    inline D3D12_HEAP_PROPERTIES HeapProperties(D3D12_HEAP_TYPE type)
    {
        D3D12_HEAP_PROPERTIES props{};
        props.Type = type;
        props.CPUPageProperty = D3D12_CPU_PAGE_PROPERTY_UNKNOWN;
        props.MemoryPoolPreference = D3D12_MEMORY_POOL_UNKNOWN;
        props.CreationNodeMask = 1;
        props.VisibleNodeMask = 1;
        return props;
    }

    inline D3D12_RESOURCE_DESC BufferDesc(UINT64 size)
    {
        D3D12_RESOURCE_DESC desc{};
        desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
        desc.Width = size;
        desc.Height = 1;
        desc.DepthOrArraySize = 1;
        desc.MipLevels = 1;
        desc.SampleDesc.Count = 1;
        desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        return desc;
    }

    inline D3D12_RESOURCE_DESC Texture2DDesc(
        uint32_t width,
        uint32_t height,
        DXGI_FORMAT format,
        D3D12_RESOURCE_FLAGS flags = D3D12_RESOURCE_FLAG_NONE)
    {
        D3D12_RESOURCE_DESC desc{};
        desc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        desc.Width = width;
        desc.Height = height;
        desc.DepthOrArraySize = 1;
        desc.MipLevels = 1;
        desc.Format = format;
        desc.SampleDesc.Count = 1;
        desc.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
        desc.Flags = flags;
        return desc;
    }

    inline D3D12_CPU_DESCRIPTOR_HANDLE OffsetCpuHandle(
        D3D12_CPU_DESCRIPTOR_HANDLE base,
        uint32_t stride,
        uint32_t index)
    {
        base.ptr += static_cast<SIZE_T>(stride) * index;
        return base;
    }

    inline D3D12_GPU_DESCRIPTOR_HANDLE OffsetGpuHandle(
        D3D12_GPU_DESCRIPTOR_HANDLE base,
        uint32_t stride,
        uint32_t index)
    {
        base.ptr += static_cast<UINT64>(stride) * index;
        return base;
    }

    inline D3D12_SHADER_BYTECODE ShaderBytecode(ID3DBlob* blob)
    {
        return {blob->GetBufferPointer(), blob->GetBufferSize()};
    }

    inline D3D12_DESCRIPTOR_RANGE DescriptorRange(
        D3D12_DESCRIPTOR_RANGE_TYPE type,
        UINT count,
        UINT shaderRegister,
        UINT registerSpace = 0)
    {
        return {type, count, shaderRegister, registerSpace, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND};
    }

    inline void SetRootCbv(
        D3D12_ROOT_PARAMETER& param,
        UINT shaderRegister,
        D3D12_SHADER_VISIBILITY visibility = D3D12_SHADER_VISIBILITY_ALL)
    {
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
        param.Descriptor.ShaderRegister = shaderRegister;
        param.ShaderVisibility = visibility;
    }

    inline void SetRootConstants(
        D3D12_ROOT_PARAMETER& param,
        UINT shaderRegister,
        UINT valueCount,
        D3D12_SHADER_VISIBILITY visibility = D3D12_SHADER_VISIBILITY_ALL)
    {
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        param.Constants.ShaderRegister = shaderRegister;
        param.Constants.Num32BitValues = valueCount;
        param.ShaderVisibility = visibility;
    }

    inline void SetRootTable(
        D3D12_ROOT_PARAMETER& param,
        const D3D12_DESCRIPTOR_RANGE& range,
        D3D12_SHADER_VISIBILITY visibility)
    {
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
        param.DescriptorTable.NumDescriptorRanges = 1;
        param.DescriptorTable.pDescriptorRanges = &range;
        param.ShaderVisibility = visibility;
    }

    inline D3D12_STATIC_SAMPLER_DESC StaticSampler(
        UINT shaderRegister,
        D3D12_FILTER filter,
        D3D12_TEXTURE_ADDRESS_MODE addressMode,
        D3D12_SHADER_VISIBILITY visibility)
    {
        D3D12_STATIC_SAMPLER_DESC sampler{};
        sampler.Filter = filter;
        sampler.AddressU = sampler.AddressV = sampler.AddressW = addressMode;
        sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
        sampler.MaxLOD = D3D12_FLOAT32_MAX;
        sampler.ShaderRegister = shaderRegister;
        sampler.ShaderVisibility = visibility;
        return sampler;
    }

    inline D3D12_STATIC_SAMPLER_DESC ShadowComparisonSampler(UINT shaderRegister)
    {
        D3D12_STATIC_SAMPLER_DESC sampler = StaticSampler(
            shaderRegister,
            D3D12_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT,
            D3D12_TEXTURE_ADDRESS_MODE_BORDER,
            D3D12_SHADER_VISIBILITY_PIXEL);
        sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
        sampler.BorderColor = D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE;
        return sampler;
    }

    inline bool CreateRootSignature(
        ID3D12Device* device,
        const D3D12_ROOT_SIGNATURE_DESC& desc,
        Microsoft::WRL::ComPtr<ID3D12RootSignature>& rootSignature)
    {
        Microsoft::WRL::ComPtr<ID3DBlob> blob;
        Microsoft::WRL::ComPtr<ID3DBlob> errors;
        const HRESULT hr = D3D12SerializeRootSignature(
            &desc,
            D3D_ROOT_SIGNATURE_VERSION_1,
            blob.GetAddressOf(),
            errors.GetAddressOf());
        if (FAILED(hr))
        {
            if (errors)
                throw std::runtime_error(static_cast<const char*>(errors->GetBufferPointer()));
            return false;
        }

        return SUCCEEDED(device->CreateRootSignature(
            0,
            blob->GetBufferPointer(),
            blob->GetBufferSize(),
            IID_PPV_ARGS(&rootSignature)));
    }

    inline void TransitionResource(
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

    inline void UavBarrier(ID3D12GraphicsCommandList* cmdList, ID3D12Resource* resource)
    {
        if (!resource)
            return;

        D3D12_RESOURCE_BARRIER barrier{};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_UAV;
        barrier.UAV.pResource = resource;
        cmdList->ResourceBarrier(1, &barrier);
    }

    inline Microsoft::WRL::ComPtr<ID3D12Resource> CreateDefaultBuffer(
        ID3D12Device* device,
        ID3D12GraphicsCommandList* cmdList,
        const void* data,
        UINT64 size,
        Microsoft::WRL::ComPtr<ID3D12Resource>& uploadBuffer,
        D3D12_RESOURCE_STATES finalState =
            D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER | D3D12_RESOURCE_STATE_INDEX_BUFFER)
    {
        using Microsoft::WRL::ComPtr;

        const auto defaultHeap = HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
        const auto uploadHeap = HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
        const auto bufferDesc = BufferDesc(size);

        ComPtr<ID3D12Resource> gpuBuffer;
        ThrowIfFailed(
            device->CreateCommittedResource(
                &defaultHeap,
                D3D12_HEAP_FLAG_NONE,
                &bufferDesc,
                D3D12_RESOURCE_STATE_COPY_DEST,
                nullptr,
                IID_PPV_ARGS(&gpuBuffer)),
            "Create default buffer");

        ThrowIfFailed(
            device->CreateCommittedResource(
                &uploadHeap,
                D3D12_HEAP_FLAG_NONE,
                &bufferDesc,
                D3D12_RESOURCE_STATE_GENERIC_READ,
                nullptr,
                IID_PPV_ARGS(&uploadBuffer)),
            "Create upload buffer");

        void* mapped = nullptr;
        D3D12_RANGE readRange{0, 0};
        ThrowIfFailed(uploadBuffer->Map(0, &readRange, &mapped), "Map upload buffer");
        std::memcpy(mapped, data, static_cast<size_t>(size));
        uploadBuffer->Unmap(0, nullptr);

        cmdList->CopyBufferRegion(gpuBuffer.Get(), 0, uploadBuffer.Get(), 0, size);

        D3D12_RESOURCE_BARRIER barrier{};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Transition.pResource = gpuBuffer.Get();
        barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
        barrier.Transition.StateAfter = finalState;
        barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &barrier);

        return gpuBuffer;
    }

    inline Microsoft::WRL::ComPtr<ID3D12Resource> CreateTexture2D(
        ID3D12Device* device,
        uint32_t width,
        uint32_t height,
        DXGI_FORMAT format,
        D3D12_RESOURCE_STATES initialState,
        D3D12_RESOURCE_FLAGS flags = D3D12_RESOURCE_FLAG_NONE,
        const D3D12_CLEAR_VALUE* clearValue = nullptr)
    {
        using Microsoft::WRL::ComPtr;

        ComPtr<ID3D12Resource> texture;
        const auto defaultHeap = HeapProperties(D3D12_HEAP_TYPE_DEFAULT);
        const auto desc = Texture2DDesc(width, height, format, flags);
        ThrowIfFailed(
            device->CreateCommittedResource(
                &defaultHeap,
                D3D12_HEAP_FLAG_NONE,
                &desc,
                initialState,
                clearValue,
                IID_PPV_ARGS(&texture)),
            "Create texture");
        return texture;
    }

    inline void UploadTexture2D(
        ID3D12Device* device,
        ID3D12GraphicsCommandList* cmdList,
        ID3D12Resource* texture,
        const Image& image,
        std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>>& uploads,
        DXGI_FORMAT uploadFormat = DXGI_FORMAT_B8G8R8A8_UNORM)
    {
        using Microsoft::WRL::ComPtr;

        const auto uploadHeap = HeapProperties(D3D12_HEAP_TYPE_UPLOAD);
        const auto textureDesc = Texture2DDesc(image.Width, image.Height, uploadFormat);

        D3D12_PLACED_SUBRESOURCE_FOOTPRINT footprint{};
        UINT64 totalBytes = 0;
        device->GetCopyableFootprints(&textureDesc, 0, 1, 0, &footprint, nullptr, nullptr, &totalBytes);

        ComPtr<ID3D12Resource> uploadBuffer;
        const auto uploadDesc = BufferDesc(totalBytes);
        ThrowIfFailed(
            device->CreateCommittedResource(
                &uploadHeap,
                D3D12_HEAP_FLAG_NONE,
                &uploadDesc,
                D3D12_RESOURCE_STATE_GENERIC_READ,
                nullptr,
                IID_PPV_ARGS(&uploadBuffer)),
            "Create texture upload buffer");

        void* mapped = nullptr;
        D3D12_RANGE readRange{0, 0};
        ThrowIfFailed(uploadBuffer->Map(0, &readRange, &mapped), "Map texture upload buffer");
        for (uint32_t y = 0; y < image.Height; ++y)
        {
            std::memcpy(
                static_cast<uint8_t*>(mapped) + y * footprint.Footprint.RowPitch,
                image.BGRA.data() + static_cast<size_t>(y) * image.Width * 4u,
                static_cast<size_t>(image.Width) * 4u);
        }
        uploadBuffer->Unmap(0, nullptr);

        D3D12_TEXTURE_COPY_LOCATION dst{};
        dst.pResource = texture;
        dst.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
        dst.SubresourceIndex = 0;

        D3D12_TEXTURE_COPY_LOCATION src{};
        src.pResource = uploadBuffer.Get();
        src.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
        src.PlacedFootprint = footprint;

        cmdList->CopyTextureRegion(&dst, 0, 0, 0, &src, nullptr);

        D3D12_RESOURCE_BARRIER barrier{};
        barrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        barrier.Transition.pResource = texture;
        barrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
        barrier.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
        barrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &barrier);

        uploads.push_back(std::move(uploadBuffer));
    }

    inline void ExecuteAndWait(
        ID3D12Device* device,
        ID3D12CommandQueue* queue,
        ID3D12GraphicsCommandList* cmdList)
    {
        using Microsoft::WRL::ComPtr;

        ThrowIfFailed(cmdList->Close(), "Close command list");
        ID3D12CommandList* lists[] = {cmdList};
        queue->ExecuteCommandLists(1, lists);

        ComPtr<ID3D12Fence> fence;
        ThrowIfFailed(device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&fence)), "Create fence");
        ThrowIfFailed(queue->Signal(fence.Get(), 1), "Signal fence");

        HANDLE eventHandle = CreateEvent(nullptr, FALSE, FALSE, nullptr);
        if (!eventHandle)
            throw std::runtime_error("CreateEvent failed");

        ThrowIfFailed(fence->SetEventOnCompletion(1, eventHandle), "SetEventOnCompletion");
        WaitForSingleObject(eventHandle, INFINITE);
        CloseHandle(eventHandle);
    }
}
