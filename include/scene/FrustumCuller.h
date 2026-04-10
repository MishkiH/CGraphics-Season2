#pragma once
#include <DirectXMath.h>
#include <array>

struct AABB
{
    DirectX::XMFLOAT3 Min{1e9f, 1e9f, 1e9f};
    DirectX::XMFLOAT3 Max{-1e9f, -1e9f, -1e9f};

    bool IsValid() const { return Min.x <= Max.x; }
};

AABB TransformAABB(const AABB& local, const DirectX::XMFLOAT4X4& world);

struct Frustum
{
    std::array<DirectX::XMFLOAT4, 6> Planes; // dot(xyz, point) + w >= 0 means inside

    static Frustum FromViewProj(const DirectX::XMFLOAT4X4& vp);
    bool Intersects(const AABB& aabb) const;
};
