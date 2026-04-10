#include "FrustumCuller.h"
#include <algorithm>

using namespace DirectX;

AABB TransformAABB(const AABB& local, const XMFLOAT4X4& world)
{
    const XMVECTOR corners[8] = {
        XMVectorSet(local.Min.x, local.Min.y, local.Min.z, 1.f),
        XMVectorSet(local.Max.x, local.Min.y, local.Min.z, 1.f),
        XMVectorSet(local.Min.x, local.Max.y, local.Min.z, 1.f),
        XMVectorSet(local.Max.x, local.Max.y, local.Min.z, 1.f),
        XMVectorSet(local.Min.x, local.Min.y, local.Max.z, 1.f),
        XMVectorSet(local.Max.x, local.Min.y, local.Max.z, 1.f),
        XMVectorSet(local.Min.x, local.Max.y, local.Max.z, 1.f),
        XMVectorSet(local.Max.x, local.Max.y, local.Max.z, 1.f),
    };
    XMMATRIX W = XMLoadFloat4x4(&world);
    AABB result;
    for (const auto& c : corners)
    {
        XMFLOAT4 t;
        XMStoreFloat4(&t, XMVector4Transform(c, W));
        result.Min.x = std::min(result.Min.x, t.x); result.Min.y = std::min(result.Min.y, t.y); result.Min.z = std::min(result.Min.z, t.z);
        result.Max.x = std::max(result.Max.x, t.x); result.Max.y = std::max(result.Max.y, t.y); result.Max.z = std::max(result.Max.z, t.z);
    }
    return result;
}

Frustum Frustum::FromViewProj(const XMFLOAT4X4& vp)
{
    Frustum f;
    f.Planes[0] = {vp._11 + vp._14, vp._21 + vp._24, vp._31 + vp._34, vp._41 + vp._44};
    f.Planes[1] = {vp._14 - vp._11, vp._24 - vp._21, vp._34 - vp._31, vp._44 - vp._41};
    f.Planes[2] = {vp._12 + vp._14, vp._22 + vp._24, vp._32 + vp._34, vp._42 + vp._44};
    f.Planes[3] = {vp._14 - vp._12, vp._24 - vp._22, vp._34 - vp._32, vp._44 - vp._42};
    f.Planes[4] = {vp._13, vp._23, vp._33, vp._43};
    f.Planes[5] = {vp._14 - vp._13, vp._24 - vp._23, vp._34 - vp._33, vp._44 - vp._43};
    return f;
}

bool Frustum::Intersects(const AABB& aabb) const
{
    for (const auto& p : Planes)
    {
        float px = (p.x >= 0.f) ? aabb.Max.x : aabb.Min.x;
        float py = (p.y >= 0.f) ? aabb.Max.y : aabb.Min.y;
        float pz = (p.z >= 0.f) ? aabb.Max.z : aabb.Min.z;
        if (p.x * px + p.y * py + p.z * pz + p.w < 0.f) return false;
    }
    return true;
}
