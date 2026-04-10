#include "ImageLoader.h"
#include "AssetPath.h"
#include <fstream>
#include <algorithm>
#include <cctype>
#include <cstring>
#include <wincodec.h>
#include <objbase.h>
#include <wrl.h>

using Microsoft::WRL::ComPtr;

namespace
{
    bool LoadTga(const std::string& path, Image& out)
    {
        std::ifstream f(path, std::ios::binary);
        if (!f) return false;

        uint8_t hdr[18]{};
        f.read(reinterpret_cast<char*>(hdr), 18);
        if (!f) return false;

        const uint8_t idLen = hdr[0];
        const uint8_t cmType = hdr[1];
        const uint8_t imgType = hdr[2];
        const uint16_t w = hdr[12] | (hdr[13] << 8);
        const uint16_t h = hdr[14] | (hdr[15] << 8);
        const uint8_t bpp = hdr[16];
        const uint8_t desc = hdr[17];

        if (cmType || !w || !h) return false;
        if (bpp != 24 && bpp != 32) return false;
        if (imgType != 2 && imgType != 10) return false;
        if (idLen) f.seekg(idLen, std::ios::cur);

        const uint32_t Bpp = bpp / 8u;
        const uint32_t px = w * h;
        out.Width = w; out.Height = h;
        out.BGRA.assign(px * 4u, 255u);

        auto write = [&](uint32_t i, const uint8_t* p) {
            uint8_t* dst = out.BGRA.data() + i * 4u;
            dst[0] = p[0]; dst[1] = p[1]; dst[2] = p[2];
            dst[3] = (Bpp == 4) ? p[3] : 255u;
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
            const uint32_t rowBytes = w * 4u;
            std::vector<uint8_t> row(rowBytes);
            for (uint32_t y = 0; y < h / 2; ++y)
            {
                uint8_t* top = out.BGRA.data() + y * rowBytes;
                uint8_t* bot = out.BGRA.data() + (h - 1 - y) * rowBytes;
                std::memcpy(row.data(), top, rowBytes);
                std::memcpy(top, bot, rowBytes);
                std::memcpy(bot, row.data(), rowBytes);
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

        out.Width = w; out.Height = h;
        out.BGRA.resize(w * h * 4u);
        return SUCCEEDED(conv->CopyPixels(nullptr, w * 4u, (UINT)out.BGRA.size(), out.BGRA.data()));
    }
}

bool LoadImage(const std::string& path, Image& out)
{
    if (path.size() < 4) return false;
    std::string ext = path.substr(path.size() - 4);
    std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c){ return (char)std::tolower(c); });
    return ext == ".tga" ? LoadTga(path, out) : LoadWic(path, out);
}
