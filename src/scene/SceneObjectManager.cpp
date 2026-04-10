#include "SceneObjectManager.h"
#include <random>
#include <algorithm>

using namespace DirectX;

namespace
{
    constexpr uint32_t kPlacementColumns = 20;
    constexpr float kMinPlacementSpacing = 24.f;
    constexpr float kPlacementSpacingMultiplier = 2.2f;
    constexpr float kPlacementJitterRatio = 0.18f;
}

bool SceneObjectManager::Initialize(const std::string& mesh0Path, const std::string& mesh1Path)
{
    if (!LoadObj(mesh0Path, m_meshes[0])) return false;
    if (!LoadObj(mesh1Path, m_meshes[1])) return false;
    PlaceInstances();
    return true;
}

void SceneObjectManager::PlaceInstances()
{
    m_instances.clear();
    m_worldBounds.clear();
    m_instances.reserve(InstanceCount);
    m_worldBounds.reserve(InstanceCount);

    std::mt19937 rng(42);
    std::uniform_real_distribution<float> rotDist(0.f, XM_2PI);
    std::uniform_real_distribution<float> scaleDist(0.85f, 1.25f);
    const float spacing = ComputePlacementSpacing();
    const uint32_t columns = kPlacementColumns;
    const uint32_t rows = (InstanceCount + columns - 1) / columns;
    const float originX = -0.5f * (columns - 1) * spacing;
    const float originZ = -0.5f * (rows - 1) * spacing;
    const float jitter = spacing * kPlacementJitterRatio;
    std::uniform_real_distribution<float> jitterDist(-jitter, jitter);

    for (uint32_t i = 0; i < InstanceCount; ++i)
    {
        const uint32_t meshIdx = PickMeshIndex(rng);
        const uint32_t column = i % columns;
        const uint32_t row = i / columns;
        const float x = originX + column * spacing + jitterDist(rng);
        const float z = originZ + row * spacing + jitterDist(rng);
        const float scale = scaleDist(rng);

        XMMATRIX world = XMMatrixScaling(scale, scale, scale)
                       * XMMatrixRotationY(rotDist(rng))
                       * XMMatrixTranslation(x, 0.f, z);

        SceneInstance inst;
        inst.MeshIndex = meshIdx;
        XMStoreFloat4x4(&inst.World, world);
        inst.WorldBounds = TransformAABB({m_meshes[meshIdx].BoundsMin, m_meshes[meshIdx].BoundsMax}, inst.World);
        m_instances.push_back(inst);
        m_worldBounds.push_back(inst.WorldBounds);
    }
}

AABB SceneObjectManager::ComputeSceneBounds() const
{
    AABB scene;
    for (const auto& b : m_worldBounds)
    {
        scene.Min.x = std::min(scene.Min.x, b.Min.x); scene.Min.y = std::min(scene.Min.y, b.Min.y); scene.Min.z = std::min(scene.Min.z, b.Min.z);
        scene.Max.x = std::max(scene.Max.x, b.Max.x); scene.Max.y = std::max(scene.Max.y, b.Max.y); scene.Max.z = std::max(scene.Max.z, b.Max.z);
    }
    scene.Min.x -= 1.f; scene.Min.y -= 1.f; scene.Min.z -= 1.f;
    scene.Max.x += 1.f; scene.Max.y += 1.f; scene.Max.z += 1.f;
    return scene;
}

float SceneObjectManager::ComputePlacementSpacing() const
{
    const auto spanXZ = [](const MeshData& mesh) {
        const float sizeX = mesh.BoundsMax.x - mesh.BoundsMin.x;
        const float sizeZ = mesh.BoundsMax.z - mesh.BoundsMin.z;
        return std::max(sizeX, sizeZ);
    };

    const float maxSpan = std::max(spanXZ(m_meshes[0]), spanXZ(m_meshes[1]));
    return std::max(kMinPlacementSpacing, maxSpan * kPlacementSpacingMultiplier);
}

uint32_t SceneObjectManager::PickMeshIndex(std::mt19937& rng) const
{
    std::uniform_int_distribution<uint32_t> meshDist(0, MeshCount - 1);
    return meshDist(rng);
}

void SceneObjectManager::BuildOctree(int maxDepth, int minPerLeaf)
{
    m_octree.Build(m_worldBounds, ComputeSceneBounds(), maxDepth, minPerLeaf);
    m_octreeBuilt = true;
}

void SceneObjectManager::GetVisibleIndices(const XMFLOAT4X4& viewProj,
                                            bool useFrustum, bool useOctree,
                                            std::vector<uint32_t>& out) const
{
    out.clear();

    if (!useFrustum)
    {
        out.resize(InstanceCount);
        for (uint32_t i = 0; i < InstanceCount; ++i) out[i] = i;
        return;
    }

    Frustum frustum = Frustum::FromViewProj(viewProj);

    if (useOctree && m_octreeBuilt)
        m_octree.QueryVisible(frustum, out);
    else
        for (uint32_t i = 0; i < InstanceCount; ++i)
            if (frustum.Intersects(m_instances[i].WorldBounds)) out.push_back(i);
}
