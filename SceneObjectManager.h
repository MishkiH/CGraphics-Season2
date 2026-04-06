#pragma once
#include "FrustumCuller.h"
#include "Octree.h"
#include "ObjLoader.h"
#include <vector>
#include <string>
#include <cstdint>

struct SceneInstance
{
    DirectX::XMFLOAT4X4 World;
    AABB                 WorldBounds;
    uint32_t             MeshIndex;
};

class SceneObjectManager
{
public:
    static constexpr uint32_t InstanceCount = 300;
    static constexpr uint32_t MeshCount     = 2;

    bool Initialize(const std::string& shrekPath, const std::string& donkeyPath);
    void BuildOctree(int maxDepth = 5, int minPerLeaf = 8);

    void GetVisibleIndices(const DirectX::XMFLOAT4X4& viewProj,
                           bool useFrustum, bool useOctree,
                           std::vector<uint32_t>& out) const;

    const MeshData&                    GetMesh(uint32_t idx) const { return m_meshes[idx]; }
    const std::vector<SceneInstance>&  GetInstances()        const { return m_instances; }

private:
    void PlaceInstances();
    AABB ComputeSceneBounds() const;

    MeshData                   m_meshes[MeshCount];
    std::vector<SceneInstance> m_instances;
    std::vector<AABB>          m_worldBounds;
    Octree                     m_octree;
    bool                       m_octreeBuilt = false;
};
