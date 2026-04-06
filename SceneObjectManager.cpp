#include "SceneObjectManager.h"
#include <random>
#include <algorithm>
#include <cmath>

using namespace DirectX;

bool SceneObjectManager::Initialize(const std::string& shrekPath,
                                     const std::string& donkeyPath)
{
    if (!LoadObj(shrekPath,  m_meshes[0])) return false;
    if (!LoadObj(donkeyPath, m_meshes[1])) return false;
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
    std::uniform_real_distribution<float> posDist(-80.f, 80.f);
    std::uniform_real_distribution<float> rotDist(0.f, XM_2PI);
    std::uniform_real_distribution<float> scaleDist(0.8f, 1.4f);

    for (uint32_t i = 0; i < InstanceCount; ++i)
    {
        const uint32_t meshIdx = (i < InstanceCount / 2) ? 0u : 1u;

        const float tx = posDist(rng);
        const float tz = posDist(rng);
        const float ry = rotDist(rng);
        const float s = scaleDist(rng);

        const XMMATRIX world = XMMatrixScaling(s, s, s)
                             * XMMatrixRotationY(ry)
                             * XMMatrixTranslation(tx, 0.f, tz);

        SceneInstance inst;
        inst.MeshIndex = meshIdx;
        XMStoreFloat4x4(&inst.World, world);

        const AABB localBounds{m_meshes[meshIdx].BoundsMin, m_meshes[meshIdx].BoundsMax};
        inst.WorldBounds = TransformAABB(localBounds, inst.World);

        m_instances.push_back(inst);
        m_worldBounds.push_back(inst.WorldBounds);
    }
}

AABB SceneObjectManager::ComputeSceneBounds() const
{
    AABB scene;
    for (const auto& b : m_worldBounds)
    {
        scene.Min.x = std::min(scene.Min.x, b.Min.x);
        scene.Min.y = std::min(scene.Min.y, b.Min.y);
        scene.Min.z = std::min(scene.Min.z, b.Min.z);
        scene.Max.x = std::max(scene.Max.x, b.Max.x);
        scene.Max.y = std::max(scene.Max.y, b.Max.y);
        scene.Max.z = std::max(scene.Max.z, b.Max.z);
    }
    scene.Min.x -= 1.f; scene.Min.y -= 1.f; scene.Min.z -= 1.f;
    scene.Max.x += 1.f; scene.Max.y += 1.f; scene.Max.z += 1.f;
    return scene;
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
        for (uint32_t i = 0; i < InstanceCount; ++i)
            out[i] = i;
        return;
    }

    const Frustum frustum = Frustum::FromViewProj(viewProj);

    if (useOctree && m_octreeBuilt)
    {
        m_octree.QueryVisible(frustum, out);
    }
    else
    {
        for (uint32_t i = 0; i < InstanceCount; ++i)
        {
            if (frustum.Intersects(m_instances[i].WorldBounds))
                out.push_back(i);
        }
    }
}
