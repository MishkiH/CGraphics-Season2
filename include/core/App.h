#pragma once

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <DirectXMath.h>
#include <cstdint>
#include <memory>

class Window;
class Input;
class RenderingSystem;

class App
{
public:
    App();
    ~App();

    bool Initialize(HINSTANCE hInstance, int nCmdShow);
    int Run();
    LRESULT HandleWindowMessage(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);

private:
    void Update(float dt);
    void UpdateWindowTitle();
    void HandleSceneHotkeys();
    void HandleSceneFeatureHotkeys();
    void UpdateCameraController(float dt);
    void ApplyHandRenderMode();
    void ApplySponzaRenderMode();
    void ApplySponzaUvEffects();
    void ApplySceneCameraPreset(int sceneMode);

    std::unique_ptr<Window> m_window;
    std::unique_ptr<Input> m_input;
    std::unique_ptr<RenderingSystem> m_renderer;

    bool m_exitRequested = false;
    uint64_t m_prevTick = 0;
    double m_secondsPerTick = 0.0;

    float m_camYaw = 1.f;
    float m_camPitch = 0.f;
    DirectX::XMFLOAT3 m_camPos{-24.f, 8.f, -24.f};
    float m_camMoveSpeed = 8.f;
    POINT m_savedCursor{0, 0};
    bool m_rmbLookActive = false;

    bool m_handUseNormal = true;
    bool m_handUseDisp = true;
    bool m_sponzaUseNormal = true;
    bool m_sponzaUvEffects = true;

    bool m_prevTab = false;
    bool m_prevN = false;
    bool m_prevM = false;
    bool m_prevT = false;
    bool m_prevF = false;
    bool m_prevO = false;
};
