#pragma once
#include <string>
#include <windows.h>

inline std::string ResolveAsset(const std::string& name)
{
    auto dirName = [](const std::string& path) {
        size_t pos = path.find_last_of("\\/");
        return pos == std::string::npos ? std::string() : path.substr(0, pos + 1);
    };
    auto join = [](const std::string& a, const std::string& b) {
        if (a.empty()) return b;
        return (a.back() == '/' || a.back() == '\\') ? a + b : a + "/" + b;
    };
    auto exists = [](const std::string& p) {
        DWORD a = GetFileAttributesA(p.c_str());
        return a != INVALID_FILE_ATTRIBUTES && !(a & FILE_ATTRIBUTE_DIRECTORY);
    };

    char buf[MAX_PATH]{};
    DWORD n = GetModuleFileNameA(nullptr, buf, MAX_PATH);
    std::string exe = (n > 0 && n < MAX_PATH) ? dirName(std::string(buf)) : std::string();

    for (const auto& c : {name, join("assets", name), join(exe, name),
                          join(join(exe, "assets"), name), join("..", name), join("../..", name)})
        if (exists(c)) return c;
    return name;
}

inline std::wstring ToWide(const std::string& s)
{
    return std::wstring(s.begin(), s.end());
}
