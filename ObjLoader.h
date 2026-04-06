#pragma once
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

struct SubMesh
{
    uint32_t IndexStart = 0;
    uint32_t IndexCount = 0;
    uint32_t DiffuseTextureIndex = 0;
};

struct MeshData
{
    std::vector<MeshVertex> Vertices;
    std::vector<uint32_t> Indices;
    std::vector<SubMesh> SubMeshes;
    std::vector<std::string> TexturePaths;

    DirectX::XMFLOAT3 BoundsMin{ 1e9f,  1e9f,  1e9f};
    DirectX::XMFLOAT3 BoundsMax{-1e9f, -1e9f, -1e9f};
};

bool LoadObj(const std::string& path, MeshData& out);
