#include "RenderingSystem.h"
#include "GBuffer.h"
#include <stdexcept>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <algorithm>
#include <cctype>
#include <wincodec.h>
#include <objbase.h>

RenderingSystem::RenderingSystem() = default;
RenderingSystem::~RenderingSystem() = default;

using namespace DirectX;
using Microsoft::WRL::ComPtr;

namespace
{
    void ThrowIfFailed(HRESULT hr, const char* what)
    {
        if (FAILED(hr))
        {
            char buf[256];
            std::snprintf(buf, sizeof(buf), "%s (hr=0x%08X)", what, static_cast<unsigned>(hr));
            throw std::runtime_error(buf);
        }
    }

    uint32_t AlignCbSize(uint32_t size) { return (size + 255u) & ~255u; }

    D3D12_HEAP_PROPERTIES HeapProps(D3D12_HEAP_TYPE type)
    {
        D3D12_HEAP_PROPERTIES p{};
        p.Type = type;
        p.CPUPageProperty = D3D12_CPU_PAGE_PROPERTY_UNKNOWN;
        p.MemoryPoolPreference = D3D12_MEMORY_POOL_UNKNOWN;
        p.CreationNodeMask = 1;
        p.VisibleNodeMask = 1;
        return p;
    }

    D3D12_RESOURCE_DESC BufferDesc(UINT64 size)
    {
        D3D12_RESOURCE_DESC d{};
        d.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
        d.Width = size;
        d.Height = 1;
        d.DepthOrArraySize = 1;
        d.MipLevels = 1;
        d.Format = DXGI_FORMAT_UNKNOWN;
        d.SampleDesc.Count = 1;
        d.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        return d;
    }

    D3D12_RESOURCE_DESC Tex2DDesc(uint32_t w, uint32_t h, DXGI_FORMAT fmt)
    {
        D3D12_RESOURCE_DESC d{};
        d.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        d.Width = w;
        d.Height = h;
        d.DepthOrArraySize = 1;
        d.MipLevels = 1;
        d.Format = fmt;
        d.SampleDesc.Count = 1;
        d.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
        return d;
    }

    std::string DirName(const std::string& path)
    {
        const size_t pos = path.find_last_of("\\/");
        return pos == std::string::npos ? std::string() : path.substr(0, pos + 1);
    }

    std::string Join(const std::string& a, const std::string& b)
    {
        if (a.empty()) return b;
        if (b.empty()) return a;
        if (a.back() == '/' || a.back() == '\\') return a + b;
        return a + "/" + b;
    }

    bool FileExists(const std::string& path)
    {
        DWORD attr = GetFileAttributesA(path.c_str());
        return attr != INVALID_FILE_ATTRIBUTES && !(attr & FILE_ATTRIBUTE_DIRECTORY);
    }

    std::string ExeDir()
    {
        char buf[MAX_PATH]{};
        DWORD n = GetModuleFileNameA(nullptr, buf, MAX_PATH);
        return (n > 0 && n < MAX_PATH) ? DirName(std::string(buf)) : std::string();
    }

    std::string ResolveAsset(const std::string& name)
    {
        const std::string exe = ExeDir();
        const std::string candidates[] = {
            name,
            Join("assets", name),
            Join(exe, name),
            Join(Join(exe, "assets"), name),
            Join("..", name),
            Join("../..", name)
        };
        for (const auto& c : candidates)
            if (FileExists(c)) return c;
        return name;
    }

    std::wstring ToWide(const std::string& s) { return std::wstring(s.begin(), s.end()); }

    std::string Trim(std::string s)
    {
        const size_t b = s.find_first_not_of(" \t\r\n");
        const size_t e = s.find_last_not_of(" \t\r\n");
        return b == std::string::npos ? std::string() : s.substr(b, e - b + 1);
    }

    struct Image { uint32_t width = 0, height = 0; std::vector<uint8_t> bgra; };

    bool LoadTga(const std::string& path, Image& out)
    {
        std::ifstream f(path, std::ios::binary);
        if (!f) return false;

        uint8_t header[18]{};
        f.read(reinterpret_cast<char*>(header), sizeof(header));
        if (!f) return false;

        const uint8_t idLen = header[0];
        const uint8_t cmType = header[1];
        const uint8_t imgType= header[2];
        const uint16_t w = header[12] | (header[13] << 8);
        const uint16_t h = header[14] | (header[15] << 8);
        const uint8_t bpp = header[16];
        const uint8_t desc = header[17];

        if (cmType || !w || !h) return false;
        if (bpp != 24 && bpp != 32) return false;
        if (imgType != 2 && imgType != 10) return false;
        if (idLen) f.seekg(idLen, std::ios::cur);

        const uint32_t Bpp = bpp / 8;
        const uint32_t px = w * h;
        out.width = w; out.height = h;
        out.bgra.assign(px * 4u, 255u);

        auto write = [&](uint32_t idx, const uint8_t* p)
        {
            size_t o = idx * 4u;
            out.bgra[o] = p[0];
            out.bgra[o+1] = p[1];
            out.bgra[o+2] = p[2];
            out.bgra[o+3] = Bpp == 4 ? p[3] : 255u;
        };

        std::vector<uint8_t> tmp(Bpp);
        if (imgType == 2)
        {
            std::vector<uint8_t> raw(px * Bpp);
            f.read(reinterpret_cast<char*>(raw.data()), raw.size());
            if (!f) return false;
            for (uint32_t i = 0; i < px; ++i) write(i, &raw[i * Bpp]);
        }
        else
        {
            for (uint32_t i = 0; i < px;)
            {
                uint8_t pkt = 0;
                f.read(reinterpret_cast<char*>(&pkt), 1);
                if (!f) return false;
                const uint32_t cnt = (pkt & 0x7Fu) + 1u;
                if (pkt & 0x80u)
                {
                    f.read(reinterpret_cast<char*>(tmp.data()), Bpp);
                    if (!f) return false;
                    for (uint32_t k = 0; k < cnt && i < px; ++k, ++i) write(i, tmp.data());
                }
                else
                {
                    for (uint32_t k = 0; k < cnt && i < px; ++k, ++i)
                    {
                        f.read(reinterpret_cast<char*>(tmp.data()), Bpp);
                        if (!f) return false;
                        write(i, tmp.data());
                    }
                }
            }
        }

        if (!(desc & 0x20u))
        {
            const uint32_t row = w * 4u;
            std::vector<uint8_t> tmp2(row);
            for (uint32_t y = 0; y < h / 2; ++y)
            {
                uint8_t* top = out.bgra.data() + y * row;
                uint8_t* bot = out.bgra.data() + (h - 1 - y) * row;
                std::memcpy(tmp2.data(), top, row);
                std::memcpy(top, bot, row);
                std::memcpy(bot, tmp2.data(), row);
            }
        }
        return true;
    }

    bool LoadWic(const std::string& path, Image& out)
    {
        static bool comInit = false;
        if (!comInit) { CoInitializeEx(nullptr, COINIT_MULTITHREADED); comInit = true; }

        ComPtr<IWICImagingFactory> factory;
        if (FAILED(CoCreateInstance(CLSID_WICImagingFactory2, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&factory))))
            if (FAILED(CoCreateInstance(CLSID_WICImagingFactory, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&factory))))
                return false;

        ComPtr<IWICBitmapDecoder> decoder;
        if (FAILED(factory->CreateDecoderFromFilename(ToWide(path).c_str(), nullptr, GENERIC_READ,
            WICDecodeMetadataCacheOnDemand, &decoder))) return false;

        ComPtr<IWICBitmapFrameDecode> frame;
        if (FAILED(decoder->GetFrame(0, &frame))) return false;

        UINT w = 0, h = 0;
        frame->GetSize(&w, &h);
        if (!w || !h) return false;

        ComPtr<IWICFormatConverter> conv;
        if (FAILED(factory->CreateFormatConverter(&conv))) return false;
        if (FAILED(conv->Initialize(frame.Get(), GUID_WICPixelFormat32bppBGRA,
            WICBitmapDitherTypeNone, nullptr, 0.f, WICBitmapPaletteTypeCustom))) return false;

        out.width = w; out.height = h;
        out.bgra.resize(w * h * 4u);
        return SUCCEEDED(conv->CopyPixels(nullptr, w * 4u, (UINT)out.bgra.size(), out.bgra.data()));
    }

    bool LoadImage(const std::string& path, Image& out)
    {
        std::string ext = path.size() > 4 ? path.substr(path.size() - 4) : "";
        std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c){ return (char)std::tolower(c); });
        return ext == ".tga" ? LoadTga(path, out) : LoadWic(path, out);
    }

    struct MtlData
    {
        std::string diffusePath;
        std::string normalPath;
        std::string displacementPath;
        XMFLOAT3 kd{1,1,1};
        XMFLOAT3 ks{0.18f,0.18f,0.18f};
        float ns = 32.f;
    };

    std::unordered_map<std::string, MtlData> LoadMtl(const std::string& mtlPath)
    {
        std::unordered_map<std::string, MtlData> mats;
        std::ifstream f(mtlPath);
        if (!f) return mats;
        const std::string base = DirName(mtlPath);
        std::string line, cur;

        auto lastToken = [](std::istringstream& ss) -> std::string
        {
            std::string tok, last;
            while (ss >> tok) last = tok;
            return last;
        };

        while (std::getline(f, line))
        {
            if (line.empty() || line[0] == '#') continue;
            std::istringstream ss(line);
            std::string cmd; ss >> cmd;
            if (cmd == "newmtl") { ss >> cur; }
            else if (cmd == "Kd" && !cur.empty()) ss >> mats[cur].kd.x >> mats[cur].kd.y >> mats[cur].kd.z;
            else if (cmd == "Ks" && !cur.empty()) ss >> mats[cur].ks.x >> mats[cur].ks.y >> mats[cur].ks.z;
            else if (cmd == "Ns" && !cur.empty()) ss >> mats[cur].ns;
            else if (cmd == "map_Kd" && !cur.empty())
            {
                std::string t = lastToken(ss);
                if (!t.empty()) mats[cur].diffusePath = Join(base, t);
            }
            else if ((cmd == "map_bump" || cmd == "bump" || cmd == "norm") && !cur.empty())
            {
                std::string t = lastToken(ss);
                if (!t.empty()) mats[cur].normalPath = Join(base, t);
            }
            else if (cmd == "disp" && !cur.empty())
            {
                std::string t = lastToken(ss);
                if (!t.empty()) mats[cur].displacementPath = Join(base, t);
            }
        }
        return mats;
    }

    struct ObjKey { int p=-1, t=-1, n=-1; bool operator==(const ObjKey& o) const { return p==o.p&&t==o.t&&n==o.n; } };
    struct ObjKeyHash { size_t operator()(const ObjKey& k) const noexcept { return (size_t)k.p*73856093u^(size_t)k.t*19349663u^(size_t)k.n*83492791u; } };

    int FixIdx(int v, int sz) { return v>0?v-1:v<0?sz+v:-1; }

    void ParseFace(const std::string& tok, int& p, int& t, int& n)
    {
        p = t = n = 0;
        size_t s1 = tok.find('/');
        if (s1 == std::string::npos) { p = std::stoi(tok); return; }
        if (s1 > 0) p = std::stoi(tok.substr(0, s1));
        size_t s2 = tok.find('/', s1 + 1);
        if (s2 == std::string::npos) { if (s1+1 < tok.size()) t = std::stoi(tok.substr(s1+1)); return; }
        if (s2 > s1+1) t = std::stoi(tok.substr(s1+1, s2-s1-1));
        if (s2+1 < tok.size()) n = std::stoi(tok.substr(s2+1));
    }

    struct ObjGroup { uint32_t start=0, count=0; std::string material; };
    struct ObjMesh
    {
        std::vector<RenderingSystem::Vertex> vertices;
        std::vector<uint32_t> indices;
        std::vector<ObjGroup> groups;
        std::unordered_map<std::string, MtlData> materials;
    };

    bool LoadObj(const std::string& path, ObjMesh& out)
    {
        std::ifstream f(path);
        if (!f) return false;
        const std::string base = DirName(path);
        std::vector<XMFLOAT3> pos, nrm;
        std::vector<XMFLOAT2> uv;
        pos.reserve(200000); nrm.reserve(200000); uv.reserve(200000);

        std::unordered_map<ObjKey, uint32_t, ObjKeyHash> vmap;
        std::vector<std::string> mtllibs;
        std::string curMat, line;

        auto switchMat = [&](const std::string& m)
        {
            if (!out.groups.empty() && curMat != m)
                out.groups.back().count = (uint32_t)out.indices.size() - out.groups.back().start;
            if (out.groups.empty() || curMat != m)
            {
                curMat = m;
                out.groups.push_back({(uint32_t)out.indices.size(), 0u, curMat});
            }
        };

        while (std::getline(f, line))
        {
            if (line.empty() || line[0] == '#') continue;
            if (line.rfind("mtllib ", 0) == 0)
            {
                std::istringstream ss(line); std::string cmd, rest; ss >> cmd; std::getline(ss, rest);
                std::istringstream ns(Trim(rest)); std::string n;
                while (ns >> n) mtllibs.push_back(Join(base, n));
                continue;
            }
            if (line.rfind("usemtl ", 0) == 0)
            {
                std::istringstream ss(line); std::string cmd, rest; ss >> cmd; std::getline(ss, rest);
                rest = Trim(rest); if (!rest.empty()) switchMat(rest);
                continue;
            }
            std::istringstream ss(line); std::string tag; ss >> tag;
            if (tag == "v") { XMFLOAT3 v{}; ss>>v.x>>v.y>>v.z; pos.push_back(v); }
            else if (tag == "vn") { XMFLOAT3 v{}; ss>>v.x>>v.y>>v.z; nrm.push_back(v); }
            else if (tag == "vt") { XMFLOAT2 v{}; ss>>v.x>>v.y; v.y=1.f-v.y; uv.push_back(v); }
            else if (tag == "f")
            {
                if (out.groups.empty())
                    out.groups.push_back({(uint32_t)out.indices.size(), 0u, curMat});
                std::vector<uint32_t> face; face.reserve(8);
                std::string tok;
                while (ss >> tok)
                {
                    int p=0, t=0, n=0;
                    ParseFace(tok, p, t, n);
                    p = FixIdx(p, (int)pos.size());
                    t = FixIdx(t, (int)uv.size());
                    n = FixIdx(n, (int)nrm.size());
                    if (p < 0) continue;
                    const ObjKey key{p, t, n};
                    auto it = vmap.find(key);
                    if (it == vmap.end())
                    {
                        RenderingSystem::Vertex vtx{};
                        vtx.Pos = pos[p];
                        vtx.Normal = n >= 0 ? nrm[n] : XMFLOAT3(0,1,0);
                        vtx.TexC = t >= 0 ? uv[t] : XMFLOAT2(0,0);
                        uint32_t idx = (uint32_t)out.vertices.size();
                        out.vertices.push_back(vtx);
                        vmap.emplace(key, idx);
                        face.push_back(idx);
                    }
                    else face.push_back(it->second);
                }
                for (size_t i = 1; i+1 < face.size(); ++i)
                {
                    out.indices.push_back(face[0]);
                    out.indices.push_back(face[i]);
                    out.indices.push_back(face[i+1]);
                }
            }
        }
        if (!out.groups.empty())
            out.groups.back().count = (uint32_t)out.indices.size() - out.groups.back().start;
        for (const auto& lib : mtllibs)
        {
            auto m = LoadMtl(lib);
            out.materials.insert(m.begin(), m.end());
        }
        if (out.vertices.empty() || out.indices.empty()) return false;

        std::vector<XMFLOAT3> tangentAcc(out.vertices.size(), XMFLOAT3(0,0,0));
        for (size_t i = 0; i + 2 < out.indices.size(); i += 3)
        {
            auto& v0 = out.vertices[out.indices[i]];
            auto& v1 = out.vertices[out.indices[i+1]];
            auto& v2 = out.vertices[out.indices[i+2]];

            XMVECTOR e1 = XMLoadFloat3(&v1.Pos) - XMLoadFloat3(&v0.Pos);
            XMVECTOR e2 = XMLoadFloat3(&v2.Pos) - XMLoadFloat3(&v0.Pos);
            float du1 = v1.TexC.x - v0.TexC.x, dv1 = v1.TexC.y - v0.TexC.y;
            float du2 = v2.TexC.x - v0.TexC.x, dv2 = v2.TexC.y - v0.TexC.y;
            float det = du1 * dv2 - du2 * dv1;
            if (fabsf(det) < 1e-7f) continue;
            XMVECTOR T = (e1 * dv2 - e2 * dv1) * (1.f / det);
            XMFLOAT3 tf; XMStoreFloat3(&tf, T);
            for (int j = 0; j < 3; ++j)
            {
                auto& acc = tangentAcc[out.indices[i+j]];
                acc.x += tf.x; acc.y += tf.y; acc.z += tf.z;
            }
        }
        for (size_t i = 0; i < out.vertices.size(); ++i)
        {
            XMVECTOR N = XMLoadFloat3(&out.vertices[i].Normal);
            XMVECTOR T = XMLoadFloat3(&tangentAcc[i]);
            // Gram-Schmidt orthogonalization against normal
            T = T - N * XMVector3Dot(T, N);
            if (XMVector3LengthSq(T).m128_f32[0] < 1e-10f)
                T = XMVectorSet(1, 0, 0, 0);
            T = XMVector3Normalize(T);
            XMStoreFloat3(&out.vertices[i].Tangent, T);
        }
        return true;
    }

    void UploadTexture(ID3D12Device* device, ID3D12GraphicsCommandList* cmdList,
                       ID3D12Resource* tex, const Image& img,
                       std::vector<ComPtr<ID3D12Resource>>& uploads)
    {
        auto uploadHeap = HeapProps(D3D12_HEAP_TYPE_UPLOAD);
        auto texDesc = Tex2DDesc(img.width, img.height, DXGI_FORMAT_B8G8R8A8_UNORM);

        D3D12_PLACED_SUBRESOURCE_FOOTPRINT fp{};
        UINT64 totalBytes = 0;
        device->GetCopyableFootprints(&texDesc, 0, 1, 0, &fp, nullptr, nullptr, &totalBytes);

        ComPtr<ID3D12Resource> upload;
        auto uploadDesc = BufferDesc(totalBytes);
        ThrowIfFailed(
            device->CreateCommittedResource(&uploadHeap, D3D12_HEAP_FLAG_NONE, &uploadDesc,
                D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&upload)),
            "texture upload buf");

        void* mapped = nullptr;
        D3D12_RANGE rr{0, 0};
        ThrowIfFailed(upload->Map(0, &rr, &mapped), "map tex upload");

        const uint32_t srcPitch = img.width * 4u;
        const uint32_t dstPitch = fp.Footprint.RowPitch;
        for (uint32_t y = 0; y < img.height; ++y)
            std::memcpy((uint8_t*)mapped + y * dstPitch, img.bgra.data() + y * srcPitch, srcPitch);
        upload->Unmap(0, nullptr);

        D3D12_TEXTURE_COPY_LOCATION dst{tex, D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX};
        dst.SubresourceIndex = 0;
        D3D12_TEXTURE_COPY_LOCATION src{upload.Get(), D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT};
        src.PlacedFootprint = fp;
        cmdList->CopyTextureRegion(&dst, 0, 0, 0, &src, nullptr);

        D3D12_RESOURCE_BARRIER b{};
        b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        b.Transition.pResource = tex;
        b.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
        b.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
        b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        cmdList->ResourceBarrier(1, &b);
        uploads.push_back(upload);
    }
}

bool RenderingSystem::Initialize(HWND hwnd, uint32_t width, uint32_t height)
{
    m_hwnd = hwnd; m_width = width; m_height = height;

#if defined(_DEBUG)
    ComPtr<ID3D12Debug> debug;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug)))) debug->EnableDebugLayer();
#endif

    ThrowIfFailed(CreateDXGIFactory1(IID_PPV_ARGS(&m_factory)), "CreateDXGIFactory1");
    CreateDevice();
    CreateCommandObjects();
    ThrowIfFailed(m_device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_fence)), "CreateFence");
    m_fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    if (!m_fenceEvent) throw std::runtime_error("CreateEvent failed");

    CreateSwapChain();
    m_rtvDescriptorSize = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    m_srvDescriptorSize = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);
    CreateBackBufferHeap();
    CreateBackBufferRTVs();

    m_viewport = {0.f, 0.f, (float)m_width, (float)m_height, 0.f, 1.f};
    m_scissorRect = {0, 0, (LONG)m_width, (LONG)m_height};

    XMStoreFloat4x4(&m_world, XMMatrixScaling(1.f, 1.f, 1.f));
    SetCamera(m_eyePos, 20.f, 0.f);
    XMStoreFloat4x4(&m_proj, XMMatrixPerspectiveFovLH(0.25f * XM_PI,
        m_height > 0 ? (float)m_width / m_height : 1.f, 0.05f, 1000.f));

    BuildShaders();
    BuildRootSignature();
    BuildGeometry();
    BuildFrameResources();

    m_gBuffer = std::make_unique<GBuffer>();
    m_gBuffer->Initialize(m_device.Get(), m_width, m_height);

    UpdatePassConstants();
    UpdateLightConstants(0.f);
    BuildPSOs();
    m_initialized = true;
    return true;
}

void RenderingSystem::Shutdown()
{
    if (m_commandQueue) FlushCommandQueue();

    auto unmap = [](ComPtr<ID3D12Resource>& buf, uint8_t*& ptr)
    { if (buf && ptr) { buf->Unmap(0, nullptr); ptr = nullptr; } };

    unmap(m_passConstantBuffer, m_mappedPassConstants);
    unmap(m_lightConstantBuffer, m_mappedLightConstants);

    if (m_gBuffer) { m_gBuffer->Shutdown(); m_gBuffer.reset(); }
    if (m_fenceEvent) { CloseHandle(m_fenceEvent); m_fenceEvent = nullptr; }
}

void RenderingSystem::OnResize(uint32_t width, uint32_t height)
{
    if (!m_initialized || !width || !height) return;
    m_width = width; m_height = height;
    FlushCommandQueue();
    for (auto& b : m_backBuffers) b.Reset();
    ThrowIfFailed(m_swapChain->ResizeBuffers(SwapChainBufferCount, m_width, m_height, DXGI_FORMAT_R8G8B8A8_UNORM, 0), "ResizeBuffers");
    m_backBufferIndex = 0;
    CreateBackBufferRTVs();
    if (m_gBuffer) m_gBuffer->Resize(m_device.Get(), m_width, m_height);
    m_viewport = {0.f, 0.f, (float)m_width, (float)m_height, 0.f, 1.f};
    m_scissorRect = {0, 0, (LONG)m_width, (LONG)m_height};
    XMStoreFloat4x4(&m_proj, XMMatrixPerspectiveFovLH(0.25f * XM_PI, (float)m_width / m_height, 0.05f, 1000.f));
    UpdatePassConstants();
}

void RenderingSystem::Draw(float dt)
{
    if (!m_initialized) return;

    UpdatePassConstants();
    UpdateLightConstants(dt);

    ThrowIfFailed(m_commandAllocator->Reset(), "Reset allocator");
    ThrowIfFailed(m_commandList->Reset(m_commandAllocator.Get(), nullptr), "Reset list");

    m_commandList->RSSetViewports(1, &m_viewport);
    m_commandList->RSSetScissorRects(1, &m_scissorRect);

    m_gBuffer->TransitionToWrite(m_commandList.Get());
    m_gBuffer->BindForGeometryPass(m_commandList.Get());

    m_commandList->SetGraphicsRootSignature(m_rootSignature.Get());
    m_commandList->SetPipelineState(m_geometryPSO.Get());
    m_commandList->SetGraphicsRootConstantBufferView(0, m_passConstantBuffer->GetGPUVirtualAddress());

    ID3D12DescriptorHeap* heaps[] = {m_textureHeap.Get()};
    m_commandList->SetDescriptorHeaps(1, heaps);

    m_commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST);
    m_commandList->IASetVertexBuffers(0, 1, &m_vertexBufferView);
    m_commandList->IASetIndexBuffer(&m_indexBufferView);

    const auto texBase = m_textureHeap->GetGPUDescriptorHandleForHeapStart();
    for (const DrawItem& item : m_drawItems)
    {
        D3D12_GPU_DESCRIPTOR_HANDLE dh = texBase;
        dh.ptr += (UINT64)item.TextureIndex * m_srvDescriptorSize;
        m_commandList->SetGraphicsRootDescriptorTable(1, dh);

        D3D12_GPU_DESCRIPTOR_HANDLE nh = texBase;
        nh.ptr += (UINT64)item.NormalTextureIndex * m_srvDescriptorSize;
        m_commandList->SetGraphicsRootDescriptorTable(5, nh);

        D3D12_GPU_DESCRIPTOR_HANDLE ph = texBase;
        ph.ptr += (UINT64)item.DisplacementTextureIndex * m_srvDescriptorSize;
        m_commandList->SetGraphicsRootDescriptorTable(6, ph);

        m_commandList->SetGraphicsRoot32BitConstants(2, 8, &item.Material, 0);
        m_commandList->DrawIndexedInstanced(item.IndexCount, 1, item.StartIndexLocation, 0, 0);
    }

    m_gBuffer->TransitionToRead(m_commandList.Get());
    m_gBuffer->TransitionDepthToRead(m_commandList.Get());

    D3D12_RESOURCE_BARRIER toRT{};
    toRT.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    toRT.Transition.pResource = CurrentBackBuffer();
    toRT.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
    toRT.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
    toRT.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    m_commandList->ResourceBarrier(1, &toRT);

    const auto bbRtv = CurrentBackBufferRTV();
    const float clearColor[4] = {0.f, 0.f, 0.f, 1.f};
    m_commandList->OMSetRenderTargets(1, &bbRtv, TRUE, nullptr);
    m_commandList->ClearRenderTargetView(bbRtv, clearColor, 0, nullptr);

    m_commandList->SetPipelineState(m_lightingPSO.Get());
    m_commandList->SetGraphicsRootSignature(m_rootSignature.Get());
    m_commandList->SetGraphicsRootConstantBufferView(0, m_passConstantBuffer->GetGPUVirtualAddress());
    m_commandList->SetGraphicsRootConstantBufferView(3, m_lightConstantBuffer->GetGPUVirtualAddress());

    ID3D12DescriptorHeap* gbHeaps[] = {m_gBuffer->GetSrvHeap()};
    m_commandList->SetDescriptorHeaps(1, gbHeaps);
    m_commandList->SetGraphicsRootDescriptorTable(4, m_gBuffer->GetSrvTable());
    m_commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    m_commandList->DrawInstanced(3, 1, 0, 0);

    m_gBuffer->TransitionDepthToWrite(m_commandList.Get());

    D3D12_RESOURCE_BARRIER toPresent = toRT;
    toPresent.Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
    toPresent.Transition.StateAfter = D3D12_RESOURCE_STATE_PRESENT;
    m_commandList->ResourceBarrier(1, &toPresent);

    ThrowIfFailed(m_commandList->Close(), "Close list");
    ID3D12CommandList* lists[] = {m_commandList.Get()};
    m_commandQueue->ExecuteCommandLists(1, lists);
    ThrowIfFailed(m_swapChain->Present(1, 0), "Present");
    m_backBufferIndex = (m_backBufferIndex + 1) % SwapChainBufferCount;
    FlushCommandQueue();
}

void RenderingSystem::SetCamera(const XMFLOAT3& eyePos, float yaw, float pitch)
{
    m_eyePos = eyePos;
    const float sy = sinf(yaw), cy = cosf(yaw), sp = sinf(pitch), cp = cosf(pitch);
    const XMVECTOR fwd = XMVector3Normalize(XMVectorSet(sy * cp, sp, cy * cp, 0.f));
    XMStoreFloat4x4(&m_view, XMMatrixLookToLH(
        XMVectorSet(eyePos.x, eyePos.y, eyePos.z, 1.f), fwd, XMVectorSet(0, 1, 0, 0)));
}

bool RenderingSystem::CreateDevice()
{
    HRESULT hr = D3D12CreateDevice(nullptr, D3D_FEATURE_LEVEL_12_0, IID_PPV_ARGS(&m_device));
    if (FAILED(hr))
    {
        ComPtr<IDXGIAdapter> warp;
        ThrowIfFailed(m_factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
        ThrowIfFailed(D3D12CreateDevice(warp.Get(), D3D_FEATURE_LEVEL_12_0, IID_PPV_ARGS(&m_device)), "WARP device");
    }
    return true;
}

bool RenderingSystem::CreateCommandObjects()
{
    D3D12_COMMAND_QUEUE_DESC qd{};
    qd.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
    ThrowIfFailed(m_device->CreateCommandQueue(&qd, IID_PPV_ARGS(&m_commandQueue)), "CreateCommandQueue");
    ThrowIfFailed(m_device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&m_commandAllocator)), "CreateCommandAllocator");
    ThrowIfFailed(m_device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, m_commandAllocator.Get(), nullptr, IID_PPV_ARGS(&m_commandList)), "CreateCommandList");
    ThrowIfFailed(m_commandList->Close(), "Initial close");
    return true;
}

bool RenderingSystem::CreateSwapChain()
{
    DXGI_SWAP_CHAIN_DESC d{};
    d.BufferCount = SwapChainBufferCount;
    d.BufferDesc.Width = m_width; d.BufferDesc.Height = m_height;
    d.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    d.BufferDesc.RefreshRate = {60, 1};
    d.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    d.OutputWindow = m_hwnd;
    d.SampleDesc = {1, 0};
    d.Windowed = TRUE;
    d.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
    ThrowIfFailed(m_factory->CreateSwapChain(m_commandQueue.Get(), &d, m_swapChain.GetAddressOf()), "CreateSwapChain");
    return true;
}

bool RenderingSystem::CreateBackBufferHeap()
{
    D3D12_DESCRIPTOR_HEAP_DESC d{};
    d.NumDescriptors = SwapChainBufferCount;
    d.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
    ThrowIfFailed(m_device->CreateDescriptorHeap(&d, IID_PPV_ARGS(&m_backBufferRtvHeap)), "BB RTV heap");
    return true;
}

bool RenderingSystem::CreateBackBufferRTVs()
{
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_backBufferRtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (uint32_t i = 0; i < SwapChainBufferCount; ++i)
    {
        ThrowIfFailed(m_swapChain->GetBuffer(i, IID_PPV_ARGS(&m_backBuffers[i])), "GetBuffer");
        m_device->CreateRenderTargetView(m_backBuffers[i].Get(), nullptr, h);
        h.ptr += m_rtvDescriptorSize;
    }
    return true;
}

bool RenderingSystem::BuildShaders()
{
    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif
    ComPtr<ID3DBlob> errors;
    const std::wstring sp = ToWide(ResolveAsset("Shaders.hlsl"));

    auto compile = [&](const char* entry, const char* target, ComPtr<ID3DBlob>& blob)
    {
        errors.Reset();
        HRESULT hr = D3DCompileFromFile(sp.c_str(), nullptr, D3D_COMPILE_STANDARD_FILE_INCLUDE,
            entry, target, flags, 0, &blob, &errors);
        if (FAILED(hr))
        {
            if (errors) throw std::runtime_error((const char*)errors->GetBufferPointer());
            ThrowIfFailed(hr, entry);
        }
    };

    compile("GeometryVS", "vs_5_0", m_geometryVS);
    compile("GeometryHS", "hs_5_0", m_hullShader);
    compile("GeometryDS", "ds_5_0", m_domainShader);
    compile("GeometryPS", "ps_5_0", m_geometryPS);
    compile("LightingVS", "vs_5_0", m_lightingVS);
    compile("LightingPS", "ps_5_0", m_lightingPS);

    m_inputLayout[0] = {"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
    m_inputLayout[1] = {"NORMAL",   0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
    m_inputLayout[2] = {"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT,    0, 24, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
    m_inputLayout[3] = {"TANGENT",  0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 32, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0};
    return true;
}

bool RenderingSystem::BuildRootSignature()
{
    // t0: diffuse (geometry PS)
    D3D12_DESCRIPTOR_RANGE texRange{};
    texRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    texRange.NumDescriptors = 1;
    texRange.BaseShaderRegister = 0;
    texRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    // t3-t5: GBuffer SRVs (lighting PS)
    D3D12_DESCRIPTOR_RANGE gbRange{};
    gbRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    gbRange.NumDescriptors = GBuffer::SrvCount;
    gbRange.BaseShaderRegister = 3;
    gbRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    // t1: normal map (geometry PS)
    D3D12_DESCRIPTOR_RANGE normRange{};
    normRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    normRange.NumDescriptors = 1;
    normRange.BaseShaderRegister = 1;
    normRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    // t2: displacement map (geometry DS + PS)
    D3D12_DESCRIPTOR_RANGE dispRange{};
    dispRange.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    dispRange.NumDescriptors = 1;
    dispRange.BaseShaderRegister = 2;
    dispRange.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_ROOT_PARAMETER params[7]{};

    params[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    params[0].Descriptor.ShaderRegister = 0;
    params[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    params[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[1].DescriptorTable = {1, &texRange};
    params[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[2].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    params[2].Constants = {2, 0, 8};
    params[2].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[3].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    params[3].Descriptor.ShaderRegister = 1;
    params[3].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[4].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[4].DescriptorTable = {1, &gbRange};
    params[4].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[5].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[5].DescriptorTable = {1, &normRange};
    params[5].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    params[6].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    params[6].DescriptorTable = {1, &dispRange};
    params[6].ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_STATIC_SAMPLER_DESC sampler{};
    sampler.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    sampler.AddressU = sampler.AddressV = sampler.AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    sampler.MaxAnisotropy = 1;
    sampler.ComparisonFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    sampler.BorderColor = D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE;
    sampler.MaxLOD = D3D12_FLOAT32_MAX;
    sampler.ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_ROOT_SIGNATURE_DESC desc{};
    desc.NumParameters = (UINT)_countof(params);
    desc.pParameters = params;
    desc.NumStaticSamplers = 1;
    desc.pStaticSamplers = &sampler;
    desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> serialized, errors;
    HRESULT hr = D3D12SerializeRootSignature(&desc, D3D_ROOT_SIGNATURE_VERSION_1, &serialized, &errors);
    if (FAILED(hr))
    {
        if (errors) throw std::runtime_error((const char*)errors->GetBufferPointer());
        ThrowIfFailed(hr, "SerializeRS");
    }
    ThrowIfFailed(m_device->CreateRootSignature(0, serialized->GetBufferPointer(),
        serialized->GetBufferSize(), IID_PPV_ARGS(&m_rootSignature)), "CreateRS");
    return true;
}

bool RenderingSystem::BuildPSOs()
{
    D3D12_RASTERIZER_DESC raster{};
    raster.FillMode = D3D12_FILL_MODE_SOLID;
    raster.CullMode = D3D12_CULL_MODE_NONE;
    raster.FrontCounterClockwise = TRUE;
    raster.DepthClipEnable = TRUE;
    raster.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF;

    const D3D12_RENDER_TARGET_BLEND_DESC noBlend = {
        FALSE, FALSE,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_LOGIC_OP_NOOP, D3D12_COLOR_WRITE_ENABLE_ALL
    };
    D3D12_BLEND_DESC blendOff{};
    for (auto& rt : blendOff.RenderTarget) rt = noBlend;

    D3D12_DEPTH_STENCIL_DESC geoDS{};
    geoDS.DepthEnable = TRUE;
    geoDS.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    geoDS.DepthFunc = D3D12_COMPARISON_FUNC_LESS;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC geoPso{};
    geoPso.pRootSignature = m_rootSignature.Get();
    geoPso.VS = {m_geometryVS->GetBufferPointer(), m_geometryVS->GetBufferSize()};
    geoPso.HS = {m_hullShader->GetBufferPointer(), m_hullShader->GetBufferSize()};
    geoPso.DS = {m_domainShader->GetBufferPointer(), m_domainShader->GetBufferSize()};
    geoPso.PS = {m_geometryPS->GetBufferPointer(), m_geometryPS->GetBufferSize()};
    geoPso.BlendState = blendOff;
    geoPso.SampleMask = UINT_MAX;
    geoPso.RasterizerState = raster;
    geoPso.DepthStencilState = geoDS;
    geoPso.InputLayout = {m_inputLayout, (UINT)_countof(m_inputLayout)};
    geoPso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;
    geoPso.NumRenderTargets = GBuffer::TargetCount;
    geoPso.RTVFormats[0] = m_gBuffer->GetAlbedoSpecFormat();
    geoPso.RTVFormats[1] = m_gBuffer->GetNormalFormat();
    geoPso.DSVFormat = m_gBuffer->GetDepthStencilFormat();
    geoPso.SampleDesc = {1, 0};
    ThrowIfFailed(m_device->CreateGraphicsPipelineState(&geoPso, IID_PPV_ARGS(&m_geometryPSO)), "Geometry PSO");

    D3D12_DEPTH_STENCIL_DESC litDS{};
    litDS.DepthEnable = FALSE;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC litPso{};
    litPso.pRootSignature = m_rootSignature.Get();
    litPso.VS = {m_lightingVS->GetBufferPointer(), m_lightingVS->GetBufferSize()};
    litPso.PS = {m_lightingPS->GetBufferPointer(), m_lightingPS->GetBufferSize()};
    litPso.BlendState = blendOff;
    litPso.SampleMask = UINT_MAX;
    litPso.RasterizerState = raster;
    litPso.DepthStencilState = litDS;
    litPso.InputLayout = {nullptr, 0};
    litPso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    litPso.NumRenderTargets = 1;
    litPso.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    litPso.DSVFormat = DXGI_FORMAT_UNKNOWN;
    litPso.SampleDesc = {1, 0};
    ThrowIfFailed(m_device->CreateGraphicsPipelineState(&litPso, IID_PPV_ARGS(&m_lightingPSO)), "Lighting PSO");
    return true;
}

bool RenderingSystem::BuildGeometry()
{
    ObjMesh model{};
    if (!LoadObj(ResolveAsset("038F_05SET_04SHOT.obj"), model))
        throw std::runtime_error("Failed to load .obj");


    std::unordered_map<std::string, uint32_t> diffIdx, normIdx, dispIdx;
    std::vector<std::string> diffPaths, normPaths, dispPaths;

    auto getIdx = [](const std::string& path,
                     std::unordered_map<std::string, uint32_t>& map,
                     std::vector<std::string>& paths) -> uint32_t
    {
        if (path.empty()) return 0;
        auto [it, ins] = map.emplace(path, (uint32_t)paths.size() + 1u);
        if (ins) paths.push_back(path);
        return it->second;
    };

    m_drawItems.clear();
    m_drawItems.reserve(model.groups.size());
    for (const ObjGroup& g : model.groups)
    {
        DrawItem item{};
        item.StartIndexLocation = g.start;
        item.IndexCount = g.count;
        auto it = model.materials.find(g.material);
        if (it != model.materials.end())
        {
            const MtlData& m = it->second;
            item.TextureIndex = getIdx(m.diffusePath, diffIdx, diffPaths);
            item.NormalTextureIndex = getIdx(m.normalPath, normIdx, normPaths);
            item.DisplacementTextureIndex = getIdx(m.displacementPath, dispIdx, dispPaths);
            item.Material.BaseColor = XMFLOAT4(m.kd.x, m.kd.y, m.kd.z, 1.f);
            float ksAvg = (m.ks.x + m.ks.y + m.ks.z) / 3.f;
            item.Material.SurfaceParams.x = std::max(0.04f, ksAvg);
            item.Material.SurfaceParams.y = std::max(8.f, std::min(m.ns, 128.f));
        }
        m_drawItems.push_back(item);
    }

    const uint32_t nD = (uint32_t)diffPaths.size();
    const uint32_t nN = (uint32_t)normPaths.size();
    const uint32_t normBase = nD + 1u;
    const uint32_t dispBase = nD + 1u + nN + 1u;

    for (auto& item : m_drawItems)
    {
        item.NormalTextureIndex += normBase;
        item.DisplacementTextureIndex += dispBase;
    }

    auto loadGroup = [](const std::vector<std::string>& paths) {
        std::vector<Image> imgs(paths.size());
        std::vector<bool> ok(paths.size(), false);
        for (size_t i = 0; i < paths.size(); ++i) ok[i] = LoadImage(paths[i], imgs[i]);
        return std::make_pair(imgs, ok);
    };

    auto [diffImgs, diffOk] = loadGroup(diffPaths);
    auto [normImgs, normOk] = loadGroup(normPaths);
    auto [dispImgs, dispOk] = loadGroup(dispPaths);

    const uint32_t totalDesc = (nD + 1u) + (nN + 1u) + ((uint32_t)dispPaths.size() + 1u);

    auto defHeap = HeapProps(D3D12_HEAP_TYPE_DEFAULT);
    auto upHeap = HeapProps(D3D12_HEAP_TYPE_UPLOAD);

    auto mkVB = [&](UINT64 sz, ComPtr<ID3D12Resource>& r)
    {
        auto d = BufferDesc(sz);
        ThrowIfFailed(m_device->CreateCommittedResource(&defHeap, D3D12_HEAP_FLAG_NONE, &d,
            D3D12_RESOURCE_STATE_COPY_DEST, nullptr, IID_PPV_ARGS(&r)), "default buf");
    };
    auto mkUpload = [&](UINT64 sz, const void* data) -> ComPtr<ID3D12Resource>
    {
        ComPtr<ID3D12Resource> r;
        auto d = BufferDesc(sz);
        ThrowIfFailed(m_device->CreateCommittedResource(&upHeap, D3D12_HEAP_FLAG_NONE, &d,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&r)), "upload buf");
        void* mapped = nullptr; D3D12_RANGE rr{0,0};
        ThrowIfFailed(r->Map(0, &rr, &mapped), "map upload");
        std::memcpy(mapped, data, sz);
        r->Unmap(0, nullptr);
        return r;
    };

    const UINT64 vbSize = model.vertices.size() * sizeof(Vertex);
    const UINT64 ibSize = model.indices.size()  * sizeof(uint32_t);
    mkVB(vbSize, m_vertexBuffer);
    mkVB(ibSize, m_indexBuffer);
    auto vbUp = mkUpload(vbSize, model.vertices.data());
    auto ibUp = mkUpload(ibSize, model.indices.data());

    auto mkTex = [&](uint32_t w, uint32_t h) -> ComPtr<ID3D12Resource>
    {
        ComPtr<ID3D12Resource> t;
        auto d = Tex2DDesc(w, h, DXGI_FORMAT_B8G8R8A8_UNORM);
        ThrowIfFailed(m_device->CreateCommittedResource(&defHeap, D3D12_HEAP_FLAG_NONE,
            &d, D3D12_RESOURCE_STATE_COPY_DEST, nullptr, IID_PPV_ARGS(&t)), "tex");
        return t;
    };

    m_diffuseTextures.clear(); m_diffuseTextures.resize(nD + 1u);
    m_normalTextures.clear();  m_normalTextures.resize(nN + 1u);
    m_dispTextures.clear();    m_dispTextures.resize(dispPaths.size() + 1u);

    m_diffuseTextures[0] = mkTex(1, 1);
    for (uint32_t i = 0; i < nD; ++i)
        m_diffuseTextures[i+1] = diffOk[i] ? mkTex(diffImgs[i].width, diffImgs[i].height) : m_diffuseTextures[0];

    m_normalTextures[0] = mkTex(1, 1);
    for (uint32_t i = 0; i < nN; ++i)
        m_normalTextures[i+1] = normOk[i] ? mkTex(normImgs[i].width, normImgs[i].height) : m_normalTextures[0];

    m_dispTextures[0] = mkTex(1, 1);
    for (size_t i = 0; i < dispPaths.size(); ++i)
        m_dispTextures[i+1] = dispOk[i] ? mkTex(dispImgs[i].width, dispImgs[i].height) : m_dispTextures[0];


    ThrowIfFailed(m_commandAllocator->Reset(), "Reset alloc upload");
    ThrowIfFailed(m_commandList->Reset(m_commandAllocator.Get(), nullptr), "Reset list upload");

    m_commandList->CopyBufferRegion(m_vertexBuffer.Get(), 0, vbUp.Get(), 0, vbSize);
    m_commandList->CopyBufferRegion(m_indexBuffer.Get(), 0, ibUp.Get(), 0, ibSize);

    D3D12_RESOURCE_BARRIER bb[2]{};
    bb[0].Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    bb[0].Transition.pResource = m_vertexBuffer.Get();
    bb[0].Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
    bb[0].Transition.StateAfter = D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER;
    bb[0].Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    bb[1] = bb[0];
    bb[1].Transition.pResource = m_indexBuffer.Get();
    bb[1].Transition.StateAfter = D3D12_RESOURCE_STATE_INDEX_BUFFER;
    m_commandList->ResourceBarrier(2, bb);

    std::vector<ComPtr<ID3D12Resource>> uploads;

    Image white{1, 1, {255, 255, 255, 255}};
    Image flatNorm{1, 1, {255, 128, 128, 255}};
    Image flatDisp{1, 1, {128, 128, 128, 255}};

    UploadTexture(m_device.Get(), m_commandList.Get(), m_diffuseTextures[0].Get(), white, uploads);
    for (uint32_t i = 0; i < nD; ++i)
        if (diffOk[i])
            UploadTexture(m_device.Get(), m_commandList.Get(), m_diffuseTextures[i+1].Get(), diffImgs[i], uploads);

    UploadTexture(m_device.Get(), m_commandList.Get(), m_normalTextures[0].Get(), flatNorm, uploads);
    for (uint32_t i = 0; i < nN; ++i)
        if (normOk[i])
            UploadTexture(m_device.Get(), m_commandList.Get(), m_normalTextures[i+1].Get(), normImgs[i], uploads);

    UploadTexture(m_device.Get(), m_commandList.Get(), m_dispTextures[0].Get(), flatDisp, uploads);
    for (size_t i = 0; i < dispPaths.size(); ++i)
        if (dispOk[i])
            UploadTexture(m_device.Get(), m_commandList.Get(), m_dispTextures[i+1].Get(), dispImgs[i], uploads);

    ThrowIfFailed(m_commandList->Close(), "Close upload list");
    ID3D12CommandList* ls[] = {m_commandList.Get()};
    m_commandQueue->ExecuteCommandLists(1, ls);
    FlushCommandQueue();

    m_vertexBufferView = {m_vertexBuffer->GetGPUVirtualAddress(), (UINT)vbSize, sizeof(Vertex)};
    m_indexBufferView = {m_indexBuffer->GetGPUVirtualAddress(),  (UINT)ibSize, DXGI_FORMAT_R32_UINT};

    D3D12_DESCRIPTOR_HEAP_DESC hd{};
    hd.NumDescriptors = totalDesc;
    hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    hd.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    ThrowIfFailed(m_device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&m_textureHeap)), "tex heap");

    auto makeSrv = [&](ID3D12Resource* res, D3D12_CPU_DESCRIPTOR_HANDLE& handle)
    {
        D3D12_SHADER_RESOURCE_VIEW_DESC sd{};
        sd.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
        sd.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        sd.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
        sd.Texture2D.MipLevels = 1;
        m_device->CreateShaderResourceView(res, &sd, handle);
        handle.ptr += m_srvDescriptorSize;
    };

    D3D12_CPU_DESCRIPTOR_HANDLE sh = m_textureHeap->GetCPUDescriptorHandleForHeapStart();
    for (const auto& t : m_diffuseTextures) makeSrv(t.Get(), sh);
    for (const auto& t : m_normalTextures) makeSrv(t.Get(), sh);
    for (const auto& t : m_dispTextures) makeSrv(t.Get(), sh);

    return true;
}

bool RenderingSystem::BuildFrameResources()
{
    auto upHeap = HeapProps(D3D12_HEAP_TYPE_UPLOAD);
    auto mkCb = [&](uint32_t sz, ComPtr<ID3D12Resource>& buf, uint8_t*& ptr)
    {
        auto d = BufferDesc(AlignCbSize(sz));
        ThrowIfFailed(m_device->CreateCommittedResource(&upHeap, D3D12_HEAP_FLAG_NONE,
            &d, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr,
            IID_PPV_ARGS(&buf)), "const buf");
        D3D12_RANGE rr{0,0};
        ThrowIfFailed(buf->Map(0, &rr, reinterpret_cast<void**>(&ptr)), "map const buf");
    };
    mkCb(sizeof(PassConstants),  m_passConstantBuffer,  m_mappedPassConstants);
    mkCb(sizeof(LightConstants), m_lightConstantBuffer, m_mappedLightConstants);
    return true;
}

void RenderingSystem::UpdatePassConstants()
{
    if (!m_mappedPassConstants) return;

    const XMMATRIX world = XMLoadFloat4x4(&m_world);
    const XMMATRIX vp = XMLoadFloat4x4(&m_view)*XMLoadFloat4x4(&m_proj);
    const XMMATRIX ivp = XMMatrixInverse(nullptr, vp);

    PassConstants cb{};
    XMStoreFloat4x4(&cb.World, XMMatrixTranspose(world));
    XMStoreFloat4x4(&cb.ViewProj, XMMatrixTranspose(vp));
    XMStoreFloat4x4(&cb.InvViewProj, XMMatrixTranspose(ivp));
    cb.EyePosW = XMFLOAT4(m_eyePos.x, m_eyePos.y, m_eyePos.z, 1.f);
    cb.RenderTargetSize = XMFLOAT4((float)m_width, (float)m_height, 1.f / m_width, 1.f / m_height);

    cb.TessParams = XMFLOAT4(1.f, 6.f, 0.5f, 15.f);
    cb.DispParams = XMFLOAT4(0.3f, 0.0f, (float)m_renderMode, 0.f);
    std::memcpy(m_mappedPassConstants, &cb, sizeof(cb));
}

void RenderingSystem::UpdateLightConstants(float dt)
{
    if (!m_mappedLightConstants) return;
    m_time += dt;

    LightConstants cb{};
    cb.AmbientColor = XMFLOAT4(0.3f, 0.3f, 0.3f, 1.f);

    uint32_t idx = 0;

    GpuLight& dir = cb.Lights[idx++];
    const XMFLOAT3 rawDir(0.4f, -1.f, 0.3f);
    float len = sqrtf(rawDir.x*rawDir.x + rawDir.y*rawDir.y + rawDir.z*rawDir.z);
    dir.DirectionSpot = XMFLOAT4(rawDir.x/len, rawDir.y/len, rawDir.z/len, 0.f);
    dir.ColorIntensity = XMFLOAT4(1.0f, 0.98f, 0.9f, 1.8f);

    cb.LightCount = XMFLOAT4((float)idx, 0.f, 0.f, 0.f);
    std::memcpy(m_mappedLightConstants, &cb, sizeof(cb));
}

void RenderingSystem::FlushCommandQueue()
{
    const uint64_t val = ++m_fenceValue;
    ThrowIfFailed(m_commandQueue->Signal(m_fence.Get(), val), "Signal");
    if (m_fence->GetCompletedValue() < val)
    {
        ThrowIfFailed(m_fence->SetEventOnCompletion(val, m_fenceEvent), "SetEvent");
        WaitForSingleObject(m_fenceEvent, INFINITE);
    }
}

D3D12_CPU_DESCRIPTOR_HANDLE RenderingSystem::CurrentBackBufferRTV() const
{
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_backBufferRtvHeap->GetCPUDescriptorHandleForHeapStart();
    h.ptr += (SIZE_T)m_backBufferIndex * m_rtvDescriptorSize;
    return h;
}

ID3D12Resource* RenderingSystem::CurrentBackBuffer() const
{
    return m_backBuffers[m_backBufferIndex].Get();
}
