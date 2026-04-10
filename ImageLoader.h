#pragma once
#include <vector>
#include <string>
#include <cstdint>

struct Image
{
    uint32_t Width = 0;
    uint32_t Height = 0;
    std::vector<uint8_t> BGRA; // Width*Height*4 bytes
};

bool LoadImage(const std::string& path, Image& out);
