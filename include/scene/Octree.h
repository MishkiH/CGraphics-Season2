#pragma once
#include "FrustumCuller.h"
#include <vector>
#include <cstdint>

class Octree
{
public:
    void Build(const std::vector<AABB>& worldBounds, const AABB& sceneBounds,
               int maxDepth = 5, int minPerLeaf = 8);

    void QueryVisible(const Frustum& frustum, std::vector<uint32_t>& out) const;

private:
    struct Node
    {
        AABB Bounds;
        int Children[8]{-1,-1,-1,-1,-1,-1,-1,-1};
        std::vector<uint32_t> Objects; // Objects owned by this node. Straddling bounds stay here.

        bool IsLeaf() const;
    };

    void Subdivide(int nodeIdx, const std::vector<AABB>& bounds, int depth, int maxDepth, int minPerLeaf);
    void QueryNode(int nodeIdx, const Frustum& frustum, std::vector<uint32_t>& out) const;

    std::vector<Node> m_nodes;
    std::vector<AABB> m_objectBounds;
};
