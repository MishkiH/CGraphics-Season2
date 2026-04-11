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
        size_t pos = path.find_last_of("\\/");
        return pos == std::string::npos ? std::string() : path.substr(0, pos + 1);
    }

    std::string JoinPath(const std::string& dir, const std::string& file)
    {
        if (dir.empty()) return file;
        return (dir.back() == '/' || dir.back() == '\\') ? dir + file : dir + "/" + file;
    }

    std::string Trim(const std::string& s)
    {
        size_t b = s.find_first_not_of(" \t\r\n");
        size_t e = s.find_last_not_of(" \t\r\n");
        return b == std::string::npos ? std::string() : s.substr(b, e - b + 1);
    }

    std::string LastToken(std::istringstream& ss)
    {
        std::string tok, last;
        while (ss >> tok) last = tok;
        return last;
    }

    std::string ToLowerAscii(std::string value)
    {
        std::transform(
            value.begin(),
            value.end(),
            value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }

    bool LooksLikeNormalMap(const std::string& path)
    {
        const std::string lower = ToLowerAscii(path);
        return lower.find("ddn") != std::string::npos
            || lower.find("_n.") != std::string::npos
            || lower.find("_normal") != std::string::npos
            || lower.find("normal") != std::string::npos
            || lower.find("norm") != std::string::npos;
    }

    struct MtlEntry
    {
        std::string DiffusePath;
        std::string NormalPath;
        std::string DisplacementPath;
        SubMeshMaterial Material;
    };

    void AssignAuxTexturePath(MtlEntry& entry, const std::string& path, bool fromDispTag)
    {
        if (path.empty()) return;

        if (fromDispTag)
        {
            if (LooksLikeNormalMap(path) && entry.NormalPath.empty())
                entry.NormalPath = path;
            else
                entry.DisplacementPath = path;
            return;
        }

        entry.NormalPath = path;
    }

    std::unordered_map<std::string, MtlEntry> LoadMtl(const std::string& mtlPath,
                                                        const std::string& baseDir)
    {
        std::unordered_map<std::string, MtlEntry> result;
        std::ifstream f(mtlPath);
        if (!f) return result;

        std::string line, cur;
        while (std::getline(f, line))
        {
            if (line.empty() || line[0] == '#') continue;
            std::istringstream ss(line);
            std::string cmd;
            ss >> cmd;
            cmd = ToLowerAscii(cmd);

            if (cmd == "newmtl") { ss >> cur; }
            else if (cmd == "kd" && !cur.empty()) { auto& m = result[cur].Material; ss >> m.Kd.x >> m.Kd.y >> m.Kd.z; }
            else if (cmd == "ks" && !cur.empty()) { auto& m = result[cur].Material; ss >> m.Ks.x >> m.Ks.y >> m.Ks.z; }
            else if (cmd == "ns" && !cur.empty()) { ss >> result[cur].Material.Ns; }
            else if (cmd == "map_kd" && !cur.empty()) { std::string t = LastToken(ss); if (!t.empty()) result[cur].DiffusePath = JoinPath(baseDir, t); }
            else if ((cmd == "map_bump" || cmd == "bump" || cmd == "norm" || cmd == "map_kn") && !cur.empty())
            {
                std::string t = LastToken(ss);
                if (!t.empty()) AssignAuxTexturePath(result[cur], JoinPath(baseDir, t), false);
            }
            else if ((cmd == "disp" || cmd == "map_disp") && !cur.empty())
            {
                std::string t = LastToken(ss);
                if (!t.empty()) AssignAuxTexturePath(result[cur], JoinPath(baseDir, t), true);
            }
        }
        return result;
    }

    struct FaceIdx { int p = -1, t = -1, n = -1; };

    FaceIdx ParseFaceToken(const std::string& tok)
    {
        FaceIdx fi;
        size_t s1 = tok.find('/');
        if (s1 == std::string::npos) { fi.p = std::stoi(tok) - 1; return fi; }
        if (s1 > 0) fi.p = std::stoi(tok.substr(0, s1)) - 1;
        size_t s2 = tok.find('/', s1 + 1);
        if (s2 == std::string::npos) { if (s1 + 1 < tok.size()) fi.t = std::stoi(tok.substr(s1 + 1)) - 1; return fi; }
        if (s2 > s1 + 1) fi.t = std::stoi(tok.substr(s1 + 1, s2 - s1 - 1)) - 1;
        if (s2 + 1 < tok.size()) fi.n = std::stoi(tok.substr(s2 + 1)) - 1;
        return fi;
    }

    struct FaceIdxHash {
        size_t operator()(const FaceIdx& i) const noexcept {
            return (size_t)i.p * 73856093u ^ (size_t)i.t * 19349663u ^ (size_t)i.n * 83492791u;
        }
    };
    struct FaceIdxEq {
        bool operator()(const FaceIdx& a, const FaceIdx& b) const noexcept {
            return a.p == b.p && a.t == b.t && a.n == b.n;
        }
    };

    void ComputeTangents(MeshData& mesh)
    {
        std::vector<XMFLOAT3> acc(mesh.Vertices.size(), {0.f, 0.f, 0.f});

        for (size_t i = 0; i + 2 < mesh.Indices.size(); i += 3)
        {
            const MeshVertex& v0 = mesh.Vertices[mesh.Indices[i]];
            const MeshVertex& v1 = mesh.Vertices[mesh.Indices[i + 1]];
            const MeshVertex& v2 = mesh.Vertices[mesh.Indices[i + 2]];

            XMVECTOR e1 = XMLoadFloat3(&v1.Pos) - XMLoadFloat3(&v0.Pos);
            XMVECTOR e2 = XMLoadFloat3(&v2.Pos) - XMLoadFloat3(&v0.Pos);
            float du1 = v1.TexC.x - v0.TexC.x, dv1 = v1.TexC.y - v0.TexC.y;
            float du2 = v2.TexC.x - v0.TexC.x, dv2 = v2.TexC.y - v0.TexC.y;
            float det = du1 * dv2 - du2 * dv1;
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
            XMVECTOR N = XMLoadFloat3(&mesh.Vertices[i].Normal);
            XMVECTOR T = XMLoadFloat3(&acc[i]);
            T = T - N * XMVector3Dot(T, N); // Gram-Schmidt
            if (XMVectorGetX(XMVector3LengthSq(T)) < 1e-10f) T = XMVectorSet(1.f, 0.f, 0.f, 0.f);
            XMStoreFloat3(&mesh.Vertices[i].Tangent, XMVector3Normalize(T));
        }
    }
}

bool LoadObj(const std::string& path, MeshData& out)
{
    std::ifstream f(path);
    if (!f) return false;
    out = MeshData{};

    const std::string baseDir = DirName(path);
    std::vector<XMFLOAT3> positions, normals;
    std::vector<XMFLOAT2> texcoords;
    positions.reserve(100000); normals.reserve(100000); texcoords.reserve(100000);

    std::unordered_map<FaceIdx, uint32_t, FaceIdxHash, FaceIdxEq> vertexCache;
    std::unordered_map<std::string, MtlEntry> materials;
    std::unordered_map<std::string, uint32_t> diffIdx, normIdx, dispIdx;

    out.DiffusePaths = {""};
    out.NormalPaths = {""};
    out.DisplacementPaths = {""};

    std::string curMat;
    uint32_t groupStart = 0;
    bool groupOpen = false;

    auto FlushGroup = [&]() {
        if (!groupOpen) return;
        uint32_t end = (uint32_t)out.Indices.size();
        if (end <= groupStart) { groupOpen = false; return; }

        SubMesh sm;
        sm.IndexStart = groupStart;
        sm.IndexCount = end - groupStart;

        auto mit = materials.find(curMat);
        if (mit != materials.end())
        {
            const MtlEntry& e = mit->second;
            sm.Material = e.Material;

            auto addPath = [](const std::string& p,
                              std::unordered_map<std::string, uint32_t>& idx,
                              std::vector<std::string>& paths) -> uint32_t {
                if (p.empty()) return 0;
                auto [it, ins] = idx.emplace(p, (uint32_t)paths.size());
                if (ins) paths.push_back(p);
                return it->second;
            };
            sm.DiffuseTexIndex = addPath(e.DiffusePath, diffIdx, out.DiffusePaths);
            sm.NormalTexIndex = addPath(e.NormalPath, normIdx, out.NormalPaths);
            sm.DisplacementTexIndex = addPath(e.DisplacementPath, dispIdx, out.DisplacementPaths);
        }

        out.SubMeshes.push_back(sm);
        groupOpen = false;
        groupStart = (uint32_t)out.Indices.size();
    };

    auto OpenGroup = [&](const std::string& mat) {
        FlushGroup();
        curMat = mat;
        groupOpen = true;
        groupStart = (uint32_t)out.Indices.size();
    };

    std::string line;
    while (std::getline(f, line))
    {
        if (line.empty() || line[0] == '#') continue;

        if (line.rfind("mtllib ", 0) == 0)
        {
            std::istringstream ss(line); std::string cmd, tok; ss >> cmd;
            while (ss >> tok) { auto m = LoadMtl(JoinPath(baseDir, tok), baseDir); materials.insert(m.begin(), m.end()); }
            continue;
        }
        if (line.rfind("usemtl ", 0) == 0)
        {
            std::istringstream ss(line); std::string cmd, mat; ss >> cmd; std::getline(ss, mat);
            mat = Trim(mat); if (!mat.empty()) OpenGroup(mat);
            continue;
        }

        std::istringstream ss(line);
        std::string tag; ss >> tag;

        if (tag == "v") { XMFLOAT3 p; ss >> p.x >> p.y >> p.z; positions.push_back(p); }
        else if (tag == "vn") { XMFLOAT3 n; ss >> n.x >> n.y >> n.z; normals.push_back(n); }
        else if (tag == "vt") { XMFLOAT2 t; ss >> t.x >> t.y; t.y = 1.f - t.y; texcoords.push_back(t); }
        else if (tag == "f")
        {
            if (!groupOpen) OpenGroup(curMat);
            std::vector<uint32_t> faceVerts;
            std::string tok;
            while (ss >> tok)
            {
                FaceIdx fi = ParseFaceToken(tok);
                auto [it, inserted] = vertexCache.emplace(fi, (uint32_t)out.Vertices.size());
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
                    if (fi.t >= 0 && fi.t < (int)texcoords.size()) v.TexC = texcoords[fi.t];
                    if (fi.n >= 0 && fi.n < (int)normals.size()) v.Normal = normals[fi.n];
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
        SubMesh sm; sm.IndexStart = 0; sm.IndexCount = (uint32_t)out.Indices.size();
        out.SubMeshes.push_back(sm);
    }

    ComputeTangents(out);
    return true;
}
