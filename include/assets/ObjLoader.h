#pragma once
#include <cstdint>
#include <vector>
#include <string>
#include <DirectXMath.h>

struct MeshVertex
{
    DirectX::XMFLOAT3 Pos;
    DirectX::XMFLOAT3 Normal;
    DirectX::XMFLOAT2 TexC;
    DirectX::XMFLOAT3 Tangent;
};

struct SubMeshMaterial
{
    DirectX::XMFLOAT3 Kd{1.f, 1.f, 1.f};
    DirectX::XMFLOAT3 Ks{0.18f, 0.18f, 0.18f};
    float Ns{32.f};
};

struct SubMesh
{
    uint32_t IndexStart = 0;
    uint32_t IndexCount = 0;
    uint32_t DiffuseTexIndex = 0;
    uint32_t NormalTexIndex = 0;
    uint32_t DisplacementTexIndex = 0;
    SubMeshMaterial Material;
};

struct MeshData
{
    std::vector<MeshVertex> Vertices;
    std::vector<uint32_t> Indices;
    std::vector<SubMesh> SubMeshes;

    std::vector<std::string> DiffusePaths;
    std::vector<std::string> NormalPaths;
    std::vector<std::string> DisplacementPaths;

    DirectX::XMFLOAT3 BoundsMin{1e9f, 1e9f, 1e9f};
    DirectX::XMFLOAT3 BoundsMax{-1e9f, -1e9f, -1e9f};
};

bool LoadObj(const std::string& path, MeshData& out);
