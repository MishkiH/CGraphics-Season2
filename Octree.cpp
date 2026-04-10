#include "Octree.h"
#include <algorithm>

void Octree::Build(const std::vector<AABB>& worldBounds, const AABB& sceneBounds,
                   int maxDepth, int minPerLeaf)
{
    m_nodes.clear();
    m_objectCount = (uint32_t)worldBounds.size();
    if (m_objectCount == 0) return;

    Node root;
    root.Bounds = sceneBounds;
    root.Objects.resize(m_objectCount);
    for (uint32_t i = 0; i < m_objectCount; ++i) root.Objects[i] = i;

    m_nodes.push_back(std::move(root));
    Subdivide(0, worldBounds, 0, maxDepth, minPerLeaf);
}

void Octree::Subdivide(int idx, const std::vector<AABB>& bounds,
                        int depth, int maxDepth, int minPerLeaf)
{
    if (depth >= maxDepth) return;
    if ((int)m_nodes[idx].Objects.size() <= minPerLeaf) return;

    AABB nb = m_nodes[idx].Bounds;
    std::vector<uint32_t> objects = std::move(m_nodes[idx].Objects);

    float mx = (nb.Min.x + nb.Max.x) * 0.5f;
    float my = (nb.Min.y + nb.Max.y) * 0.5f;
    float mz = (nb.Min.z + nb.Max.z) * 0.5f;

    const AABB childBounds[8] = {
        {{nb.Min.x, nb.Min.y, nb.Min.z}, {mx, my, mz}},
        {{mx, nb.Min.y, nb.Min.z}, {nb.Max.x, my, mz}},
        {{nb.Min.x, my, nb.Min.z}, {mx, nb.Max.y, mz}},
        {{mx, my, nb.Min.z}, {nb.Max.x, nb.Max.y, mz}},
        {{nb.Min.x, nb.Min.y, mz}, {mx, my, nb.Max.z}},
        {{mx, nb.Min.y, mz}, {nb.Max.x, my, nb.Max.z}},
        {{nb.Min.x, my, mz}, {mx, nb.Max.y, nb.Max.z}},
        {{mx, my, mz}, {nb.Max.x, nb.Max.y, nb.Max.z}},
    };

    m_nodes.reserve(m_nodes.size() + 8);

    int childIdx[8];
    for (int c = 0; c < 8; ++c)
    {
        Node child;
        child.Bounds = childBounds[c];
        for (uint32_t oi : objects)
        {
            const AABB& ob = bounds[oi];
            if (ob.Max.x >= child.Bounds.Min.x && ob.Min.x <= child.Bounds.Max.x &&
                ob.Max.y >= child.Bounds.Min.y && ob.Min.y <= child.Bounds.Max.y &&
                ob.Max.z >= child.Bounds.Min.z && ob.Min.z <= child.Bounds.Max.z)
                child.Objects.push_back(oi);
        }
        childIdx[c] = (int)m_nodes.size();
        m_nodes.push_back(std::move(child));
    }

    for (int c = 0; c < 8; ++c) m_nodes[idx].Children[c] = childIdx[c];
    for (int c = 0; c < 8; ++c) Subdivide(childIdx[c], bounds, depth + 1, maxDepth, minPerLeaf);
}

void Octree::QueryVisible(const Frustum& frustum, std::vector<uint32_t>& out) const
{
    if (m_nodes.empty()) return;
    std::vector<bool> visited(m_objectCount, false);
    QueryNode(0, frustum, out, visited);
}

void Octree::QueryNode(int idx, const Frustum& frustum,
                        std::vector<uint32_t>& out, std::vector<bool>& visited) const
{
    const Node& node = m_nodes[idx];
    if (!frustum.Intersects(node.Bounds)) return;

    if (node.IsLeaf())
    {
        for (uint32_t oi : node.Objects)
            if (!visited[oi]) { visited[oi] = true; out.push_back(oi); }
        return;
    }

    for (int c = 0; c < 8; ++c)
        if (node.Children[c] != -1) QueryNode(node.Children[c], frustum, out, visited);
}
