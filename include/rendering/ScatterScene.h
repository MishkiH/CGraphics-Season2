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
#include <string>
#include <vector>

#include "SceneObjectManager.h"

class ScatterScene
{
public:
    bool Initialize(ID3D12Device* device, ID3D12CommandQueue* cmdQueue,
                    DXGI_FORMAT backBufferFmt, uint32_t width, uint32_t height,
                    const std::string& mesh0Path, const std::string& mesh1Path);

    void Shutdown();
    void OnResize(ID3D12Device* device, uint32_t width, uint32_t height);

    void RecordCommands(ID3D12GraphicsCommandList* cmdList,
                        const DirectX::XMFLOAT4X4& viewProj,
                        const DirectX::XMFLOAT3& eyePos,
                        D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
                        D3D12_VIEWPORT viewport,
                        D3D12_RECT scissorRect);

    void SetFrustumCulling(bool v) { m_useFrustum = v; }
    void SetOctreeCulling(bool v) { m_useOctree = v; }
    bool FrustumCullingEnabled() const { return m_useFrustum; }
    bool OctreeCullingEnabled() const { return m_useOctree; }
    uint32_t LastVisibleCount() const { return m_lastVisible; }

private:
    struct alignas(256) SceneCBData
    {
        DirectX::XMFLOAT4X4 ViewProj;
        DirectX::XMFLOAT4 EyePos;
    };

    struct MeshGpu
    {
        Microsoft::WRL::ComPtr<ID3D12Resource> VertexBuffer;
        Microsoft::WRL::ComPtr<ID3D12Resource> IndexBuffer;
        D3D12_VERTEX_BUFFER_VIEW VBV{};
        D3D12_INDEX_BUFFER_VIEW IBV{};
        Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> SrvHeap;
        std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>> Textures;
        uint32_t SrvStride = 0;
    };

    bool BuildShaders(ID3D12Device* device);
    bool BuildRootSignature(ID3D12Device* device);
    bool BuildPSO(ID3D12Device* device, DXGI_FORMAT backBufferFmt);
    bool BuildMeshGpu(ID3D12Device* device, ID3D12CommandQueue* cmdQueue);
    bool BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height);
    bool BuildSceneCB(ID3D12Device* device);
    void UpdateSceneConstants(const DirectX::XMFLOAT4X4& viewProj, const DirectX::XMFLOAT3& eyePos);
    void GatherVisibleInstances(const DirectX::XMFLOAT4X4& viewProj);
    void DrawVisibleInstances(ID3D12GraphicsCommandList* cmdList);

    void UploadMesh(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                    MeshGpu& gpu, const MeshData& mesh,
                    std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>>& uploads);

    static Microsoft::WRL::ComPtr<ID3D12Resource> CreateGpuBuffer(
        ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
        const void* data, uint64_t size,
        Microsoft::WRL::ComPtr<ID3D12Resource>& upload);

    SceneObjectManager m_scene;
    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_rootSig;
    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_pso;
    Microsoft::WRL::ComPtr<ID3DBlob> m_vs;
    Microsoft::WRL::ComPtr<ID3DBlob> m_ps;
    MeshGpu m_meshes[SceneObjectManager::MeshCount];
    Microsoft::WRL::ComPtr<ID3D12Resource> m_sceneCB;
    uint8_t* m_mappedSceneCB = nullptr;
    Microsoft::WRL::ComPtr<ID3D12Resource> m_depthBuffer;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_dsvHeap;
    std::vector<uint32_t> m_visibleScratch;
    std::vector<uint32_t> m_visibleByMesh[SceneObjectManager::MeshCount];
    bool m_useFrustum = true;
    bool m_useOctree = true;
    uint32_t m_lastVisible = 0;
};
