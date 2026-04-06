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
    bool Initialize(ID3D12Device*       device,
                    ID3D12CommandQueue* cmdQueue,
                    DXGI_FORMAT         backBufferFmt,
                    uint32_t            width,
                    uint32_t            height,
                    const std::string&  shrekPath,
                    const std::string&  donkeyPath);

    void Shutdown();
    void OnResize(ID3D12Device* device, uint32_t width, uint32_t height);

    void Draw(ID3D12GraphicsCommandList*  cmdList,
              const DirectX::XMFLOAT4X4& viewProj,
              const DirectX::XMFLOAT3&   eyePos,
              D3D12_CPU_DESCRIPTOR_HANDLE backBufferRtv,
              D3D12_VIEWPORT              viewport,
              D3D12_RECT                  scissorRect);

    void     SetFrustumCulling(bool v)  { m_useFrustum = v; }
    void     SetOctreeCulling(bool v)   { m_useOctree = v; }
    bool     FrustumCullingEnabled() const { return m_useFrustum; }
    bool     OctreeCullingEnabled()  const { return m_useOctree;  }
    uint32_t LastVisibleCount()      const { return m_lastVisible; }

private:
    // CPU-side image data used only during upload
    struct Image
    {
        uint32_t             width = 0;
        uint32_t             height = 0;
        std::vector<uint8_t> bgra;
    };

    struct alignas(256) SceneCBData
    {
        DirectX::XMFLOAT4X4 ViewProj;
        DirectX::XMFLOAT4   EyePos;
    };

    // GPU resources for one mesh (geometry + its textures in a single SRV heap)
    struct MeshGpu
    {
        Microsoft::WRL::ComPtr<ID3D12Resource>       VertexBuffer;
        Microsoft::WRL::ComPtr<ID3D12Resource>       IndexBuffer;
        D3D12_VERTEX_BUFFER_VIEW                     VBV{};
        D3D12_INDEX_BUFFER_VIEW                      IBV{};

        // SRV heap: [0] = white fallback, [1..N] = actual diffuse textures
        Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> SrvHeap;
        std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>> Textures;
        uint32_t SrvDescriptorSize = 0;
    };

    bool BuildShaders();
    bool BuildRootSignature(ID3D12Device* device);
    bool BuildPSO(ID3D12Device* device, DXGI_FORMAT backBufferFmt);
    bool BuildMeshBuffers(ID3D12Device* device, ID3D12CommandQueue* cmdQueue);
    bool BuildDepthBuffer(ID3D12Device* device, uint32_t width, uint32_t height);
    bool BuildSceneCB(ID3D12Device* device);

    void UploadMeshGpu(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                       MeshGpu& gpu, const MeshData& mesh,
                       std::vector<Microsoft::WRL::ComPtr<ID3D12Resource>>& uploadKeepAlive);

    static bool LoadImage(const std::string& path, Image& out);
    static bool LoadTga(const std::string& path, Image& out);
    static bool LoadWic(const std::string& path, Image& out);

    static Microsoft::WRL::ComPtr<ID3D12Resource> CreateGpuBuffer(
        ID3D12Device*                            device,
        ID3D12GraphicsCommandList*               cmdList,
        const void*                              data,
        uint64_t                                 byteSize,
        Microsoft::WRL::ComPtr<ID3D12Resource>&  uploadBuffer);

    SceneObjectManager m_scene;

    Microsoft::WRL::ComPtr<ID3D12RootSignature>  m_rootSig;
    Microsoft::WRL::ComPtr<ID3D12PipelineState>  m_pso;
    Microsoft::WRL::ComPtr<ID3DBlob>             m_vs;
    Microsoft::WRL::ComPtr<ID3DBlob>             m_ps;

    MeshGpu m_meshes[SceneObjectManager::MeshCount];

    Microsoft::WRL::ComPtr<ID3D12Resource>       m_sceneCB;
    uint8_t*                                     m_mappedSceneCB = nullptr;

    Microsoft::WRL::ComPtr<ID3D12Resource>       m_depthBuffer;
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_dsvHeap;

    bool     m_useFrustum = true;
    bool     m_useOctree = true;
    uint32_t m_lastVisible = 0;
};
