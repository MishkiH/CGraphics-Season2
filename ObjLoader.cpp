#include "ObjLoader.h"
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <algorithm>
#include <cctype>
#include <cmath>

using namespace DirectX;

namespace
{
    std::string DirName(const std::string& path)
    {
        const size_t pos = path.find_last_of("\\/");
        return pos == std::string::npos ? std::string() : path.substr(0, pos + 1);
    }

    std::string JoinPath(const std::string& dir, const std::string& file)
    {
        if (dir.empty()) return file;
        if (dir.back() == '/' || dir.back() == '\\') return dir + file;
        return dir + "/" + file;
    }

    std::string Trim(const std::string& s)
    {
        const size_t b = s.find_first_not_of(" \t\r\n");
        const size_t e = s.find_last_not_of(" \t\r\n");
        return b == std::string::npos ? std::string() : s.substr(b, e - b + 1);
    }

    std::string LastToken(std::istringstream& ss)
    {
        std::string tok, last;
        while (ss >> tok) last = tok;
        return last;
    }

    std::unordered_map<std::string, std::string> LoadMtl(const std::string& mtlPath,
                                                          const std::string& baseDir)
    {
        std::unordered_map<std::string, std::string> result;
        std::ifstream f(mtlPath);
        if (!f) return result;

        std::string line, curMat;
        while (std::getline(f, line))
        {
            if (line.empty() || line[0] == '#') continue;
            std::istringstream ss(line);
            std::string cmd;
            ss >> cmd;

            if (cmd == "newmtl")
                ss >> curMat;
            else if ((cmd == "map_Kd") && !curMat.empty())
            {
                std::string tex = LastToken(ss);
                if (!tex.empty())
                    result[curMat] = JoinPath(baseDir, tex);
            }
        }
        return result;
    }

    struct FaceIdx { int p = -1, t = -1, n = -1; };

    FaceIdx ParseToken(const std::string& tok)
    {
        FaceIdx idx;
        const size_t s1 = tok.find('/');
        if (s1 == std::string::npos)
        {
            idx.p = std::stoi(tok) - 1;
            return idx;
        }
        if (s1 > 0) idx.p = std::stoi(tok.substr(0, s1)) - 1;
        const size_t s2 = tok.find('/', s1 + 1);
        if (s2 == std::string::npos)
        {
            if (s1 + 1 < tok.size()) idx.t = std::stoi(tok.substr(s1 + 1)) - 1;
            return idx;
        }
        if (s2 > s1 + 1) idx.t = std::stoi(tok.substr(s1 + 1, s2 - s1 - 1)) - 1;
        if (s2 + 1 < tok.size()) idx.n = std::stoi(tok.substr(s2 + 1)) - 1;
        return idx;
    }

    struct FaceIdxHash
    {
        size_t operator()(const FaceIdx& i) const noexcept
        {
            return (size_t)i.p * 73856093u ^ (size_t)i.t * 19349663u ^ (size_t)i.n * 83492791u;
        }
    };

    struct FaceIdxEq
    {
        bool operator()(const FaceIdx& a, const FaceIdx& b) const noexcept
        {
            return a.p == b.p && a.t == b.t && a.n == b.n;
        }
    };

    void ComputeTangents(MeshData& mesh)
    {
        std::vector<XMFLOAT3> acc(mesh.Vertices.size(), {0,0,0});

        for (size_t i = 0; i + 2 < mesh.Indices.size(); i += 3)
        {
            auto& v0 = mesh.Vertices[mesh.Indices[i]];
            auto& v1 = mesh.Vertices[mesh.Indices[i + 1]];
            auto& v2 = mesh.Vertices[mesh.Indices[i + 2]];

            const XMVECTOR e1 = XMLoadFloat3(&v1.Pos) - XMLoadFloat3(&v0.Pos);
            const XMVECTOR e2 = XMLoadFloat3(&v2.Pos) - XMLoadFloat3(&v0.Pos);
            const float du1 = v1.TexC.x - v0.TexC.x, dv1 = v1.TexC.y - v0.TexC.y;
            const float du2 = v2.TexC.x - v0.TexC.x, dv2 = v2.TexC.y - v0.TexC.y;
            const float det = du1 * dv2 - du2 * dv1;
            if (fabsf(det) < 1e-7f) continue;

            XMFLOAT3 T;
            XMStoreFloat3(&T, (e1 * dv2 - e2 * dv1) * (1.f / det));
            for (int j = 0; j < 3; ++j)
            {
                auto& a = acc[mesh.Indices[i + j]];
                a.x += T.x; a.y += T.y; a.z += T.z;
            }
        }

        for (size_t i = 0; i < mesh.Vertices.size(); ++i)
        {
            const XMVECTOR N = XMLoadFloat3(&mesh.Vertices[i].Normal);
            XMVECTOR T = XMLoadFloat3(&acc[i]);
            T = T - N * XMVector3Dot(T, N);
            if (XMVectorGetX(XMVector3LengthSq(T)) < 1e-10f)
                T = XMVectorSet(1, 0, 0, 0);
            XMStoreFloat3(&mesh.Vertices[i].Tangent, XMVector3Normalize(T));
        }
    }
}

bool LoadObj(const std::string& path, MeshData& out)
{
    std::ifstream f(path);
    if (!f) return false;

    const std::string baseDir = DirName(path);

    std::vector<XMFLOAT3> positions, normals;
    std::vector<XMFLOAT2> texcoords;
    positions.reserve(100000);
    normals.reserve(100000);
    texcoords.reserve(100000);

    std::unordered_map<FaceIdx, uint32_t, FaceIdxHash, FaceIdxEq> vertexMap;

    std::unordered_map<std::string, std::string> matDiffuse;
    std::unordered_map<std::string, uint32_t>    texIndex;

    out.TexturePaths.push_back("");

    std::string    curMat;
    uint32_t       curGroupStart = 0;
    bool           groupOpen     = false;

    auto FlushGroup = [&]()
    {
        if (!groupOpen) return;
        const uint32_t end = (uint32_t)out.Indices.size();
        if (end > curGroupStart)
        {
            SubMesh sm;
            sm.IndexStart  = curGroupStart;
            sm.IndexCount  = end - curGroupStart;

            auto it = matDiffuse.find(curMat);
            if (it != matDiffuse.end() && !it->second.empty())
            {
                auto [ti, inserted] = texIndex.emplace(it->second, (uint32_t)out.TexturePaths.size());
                if (inserted) out.TexturePaths.push_back(it->second);
                sm.DiffuseTextureIndex = ti->second;
            }
            out.SubMeshes.push_back(sm);
        }
        groupOpen     = false;
        curGroupStart = (uint32_t)out.Indices.size();
    };

    auto OpenGroup = [&](const std::string& mat)
    {
        FlushGroup();
        curMat    = mat;
        groupOpen = true;
        curGroupStart = (uint32_t)out.Indices.size();
    };

    std::string line;
    while (std::getline(f, line))
    {
        if (line.empty() || line[0] == '#') continue;

        if (line.rfind("mtllib ", 0) == 0)
        {
            std::istringstream ss(line);
            std::string cmd, tok;
            ss >> cmd;
            while (ss >> tok)
            {
                const std::string mtlPath = JoinPath(baseDir, tok);
                auto m = LoadMtl(mtlPath, baseDir);
                matDiffuse.insert(m.begin(), m.end());
            }
            continue;
        }

        if (line.rfind("usemtl ", 0) == 0)
        {
            std::istringstream ss(line);
            std::string cmd, mat;
            ss >> cmd;
            std::getline(ss, mat);
            mat = Trim(mat);
            if (!mat.empty()) OpenGroup(mat);
            continue;
        }

        std::istringstream ss(line);
        std::string tag;
        ss >> tag;

        if (tag == "v")
        {
            XMFLOAT3 p;
            ss >> p.x >> p.y >> p.z;
            positions.push_back(p);
        }
        else if (tag == "vn")
        {
            XMFLOAT3 n;
            ss >> n.x >> n.y >> n.z;
            normals.push_back(n);
        }
        else if (tag == "vt")
        {
            XMFLOAT2 t;
            ss >> t.x >> t.y;
            t.y = 1.f - t.y;
            texcoords.push_back(t);
        }
        else if (tag == "f")
        {
            if (!groupOpen) OpenGroup(curMat);

            std::vector<uint32_t> faceVerts;
            std::string tok;
            while (ss >> tok)
            {
                const FaceIdx fi = ParseToken(tok);

                auto [it, inserted] = vertexMap.emplace(fi, (uint32_t)out.Vertices.size());
                if (inserted)
                {
                    MeshVertex v{};
                    if (fi.p >= 0 && fi.p < (int)positions.size())
                    {
                        v.Pos = positions[fi.p];
                        out.BoundsMin.x = std::min(out.BoundsMin.x, v.Pos.x);
                        out.BoundsMin.y = std::min(out.BoundsMin.y, v.Pos.y);
                        out.BoundsMin.z = std::min(out.BoundsMin.z, v.Pos.z);
                        out.BoundsMax.x = std::max(out.BoundsMax.x, v.Pos.x);
                        out.BoundsMax.y = std::max(out.BoundsMax.y, v.Pos.y);
                        out.BoundsMax.z = std::max(out.BoundsMax.z, v.Pos.z);
                    }
                    if (fi.t >= 0 && fi.t < (int)texcoords.size()) v.TexC   = texcoords[fi.t];
                    if (fi.n >= 0 && fi.n < (int)normals.size())   v.Normal = normals[fi.n];
                    out.Vertices.push_back(v);
                }
                faceVerts.push_back(it->second);
            }

            for (size_t i = 1; i + 1 < faceVerts.size(); ++i)
            {
                out.Indices.push_back(faceVerts[0]);
                out.Indices.push_back(faceVerts[i]);
                out.Indices.push_back(faceVerts[i + 1]);
            }
        }
    }

    FlushGroup();

    if (out.Vertices.empty()) return false;

    if (out.SubMeshes.empty())
    {
        SubMesh sm;
        sm.IndexStart  = 0;
        sm.IndexCount  = (uint32_t)out.Indices.size();
        sm.DiffuseTextureIndex = 0;
        out.SubMeshes.push_back(sm);
    }

    ComputeTangents(out);
    return true;
}
