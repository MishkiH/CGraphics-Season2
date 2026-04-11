#include "Octree.h"
#include <array>

namespace
{
    bool ContainsAabb(const AABB& outer, const AABB& inner)
    {
        return inner.Min.x >= outer.Min.x && inner.Max.x <= outer.Max.x
            && inner.Min.y >= outer.Min.y && inner.Max.y <= outer.Max.y
            && inner.Min.z >= outer.Min.z && inner.Max.z <= outer.Max.z;
    }

    std::array<AABB, 8> SplitBounds(const AABB& bounds)
    {
        const float midX = (bounds.Min.x + bounds.Max.x) * 0.5f;
        const float midY = (bounds.Min.y + bounds.Max.y) * 0.5f;
        const float midZ = (bounds.Min.z + bounds.Max.z) * 0.5f;

        return {{
            {{bounds.Min.x, bounds.Min.y, bounds.Min.z}, {midX, midY, midZ}},
            {{midX, bounds.Min.y, bounds.Min.z}, {bounds.Max.x, midY, midZ}},
            {{bounds.Min.x, midY, bounds.Min.z}, {midX, bounds.Max.y, midZ}},
            {{midX, midY, bounds.Min.z}, {bounds.Max.x, bounds.Max.y, midZ}},
            {{bounds.Min.x, bounds.Min.y, midZ}, {midX, midY, bounds.Max.z}},
            {{midX, bounds.Min.y, midZ}, {bounds.Max.x, midY, bounds.Max.z}},
            {{bounds.Min.x, midY, midZ}, {midX, bounds.Max.y, bounds.Max.z}},
            {{midX, midY, midZ}, {bounds.Max.x, bounds.Max.y, bounds.Max.z}},
        }};
    }

    int FindContainingChild(const std::array<AABB, 8>& childBounds, const AABB& objectBounds)
    {
        for (int childIndex = 0; childIndex < static_cast<int>(childBounds.size()); ++childIndex)
        {
            if (ContainsAabb(childBounds[childIndex], objectBounds))
                return childIndex;
        }
        return -1;
    }
}

bool Octree::Node::IsLeaf() const
{
    for (int child : Children)
    {
        if (child != -1)
            return false;
    }
    return true;
}

void Octree::Build(const std::vector<AABB>& worldBounds, const AABB& sceneBounds,
                   int maxDepth, int minPerLeaf)
{
    m_nodes.clear();
    m_objectBounds = worldBounds;
    if (worldBounds.empty()) return;

    Node root;
    root.Bounds = sceneBounds;
    root.Objects.resize(worldBounds.size());
    for (uint32_t i = 0; i < static_cast<uint32_t>(worldBounds.size()); ++i)
        root.Objects[i] = i;

    m_nodes.push_back(std::move(root));
    Subdivide(0, worldBounds, 0, maxDepth, minPerLeaf);
}

void Octree::Subdivide(int idx, const std::vector<AABB>& bounds,
                       int depth, int maxDepth, int minPerLeaf)
{
    if (depth >= maxDepth) return;
    if ((int)m_nodes[idx].Objects.size() <= minPerLeaf) return;

    const std::array<AABB, 8> childBounds = SplitBounds(m_nodes[idx].Bounds);
    std::array<std::vector<uint32_t>, 8> childObjects;
    std::vector<uint32_t> retainedObjects;
    retainedObjects.reserve(m_nodes[idx].Objects.size());

    for (uint32_t objectIndex : m_nodes[idx].Objects)
    {
        const int childIndex = FindContainingChild(childBounds, bounds[objectIndex]);
        if (childIndex == -1)
        {
            retainedObjects.push_back(objectIndex);
            continue;
        }

        childObjects[childIndex].push_back(objectIndex);
    }

    m_nodes[idx].Objects = std::move(retainedObjects);
    m_nodes.reserve(m_nodes.size() + 8);

    for (int childIndex = 0; childIndex < static_cast<int>(childObjects.size()); ++childIndex)
    {
        if (childObjects[childIndex].empty())
            continue;

        Node child;
        child.Bounds = childBounds[childIndex];
        child.Objects = std::move(childObjects[childIndex]);

        m_nodes[idx].Children[childIndex] = static_cast<int>(m_nodes.size());
        m_nodes.push_back(std::move(child));
    }

    const std::array<int, 8> childIndices = m_nodes[idx].Children;
    for (int childIndex : childIndices)
    {
        if (childIndex != -1)
            Subdivide(childIndex, bounds, depth + 1, maxDepth, minPerLeaf);
    }
}

void Octree::QueryVisible(const Frustum& frustum, std::vector<uint32_t>& out) const
{
    out.clear();
    if (m_nodes.empty()) return;
    QueryNode(0, frustum, out);
}

void Octree::QueryNode(int idx, const Frustum& frustum,
                       std::vector<uint32_t>& out) const
{
    const Node& node = m_nodes[idx];
    if (!frustum.Intersects(node.Bounds)) return;

    for (uint32_t objectIndex : node.Objects)
        if (frustum.Intersects(m_objectBounds[objectIndex]))
            out.push_back(objectIndex);

    if (node.IsLeaf())
        return;

    for (int childIndex : node.Children)
    {
        if (childIndex != -1)
            QueryNode(childIndex, frustum, out);
    }
}
