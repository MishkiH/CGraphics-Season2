#pragma once

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <cstdint>
#include <DirectXMath.h>
#include "RenderingSystem.h"

class Window;
class Input;

class App
{
public:
    bool Initialize(HINSTANCE hInstance, int nCmdShow);
    int Run();
    LRESULT HandleWindowMessage(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);

private:
    void Update(float dt);
    void ApplyRenderMode();

    Window* m_window = nullptr;
    Input* m_input = nullptr;
    RenderingSystem* m_renderer = nullptr;

    bool m_exitRequested = false;
    uint64_t m_prevTick = 0;
    double m_secondsPerTick = 0.0;

    float m_camYaw = 1.f;
    float m_camPitch = 0.f;
    DirectX::XMFLOAT3 m_camPos{ -7.f, 4.f, -7.f };

    POINT m_savedCursorPos{ 0, 0 };
    bool m_justEnteredRmbLook = false;

    bool m_useNormal = true;
    bool m_useDisp = true;
    bool m_nKeyWasDown = false;
    bool m_mKeyWasDown = false;
};
