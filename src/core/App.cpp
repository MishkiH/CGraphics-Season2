#include "App.h"
#include "Window.h"
#include "Input.h"
#include "RenderingSystem.h"
#include "SceneProfiles.h"
#include <windows.h>
#include <windowsx.h>
#include <algorithm>
#include <cmath>
#include <DirectXMath.h>

using namespace DirectX;

namespace
{
    uint64_t Qpc() { LARGE_INTEGER t{}; QueryPerformanceCounter(&t); return (uint64_t)t.QuadPart; }
    double Qpf() { LARGE_INTEGER f{}; QueryPerformanceFrequency(&f); return (double)f.QuadPart; }

    bool JustPressed(bool current, bool& prev)
    {
        bool fired = current && !prev;
        prev = current;
        return fired;
    }
}

App::App() = default;
App::~App() = default;

bool App::Initialize(HINSTANCE hInstance, int nCmdShow)
{
    m_window = std::make_unique<Window>();
    m_input = std::make_unique<Input>();
    m_input->Reset();

    if (!m_window->Create(this, hInstance, nCmdShow, 1920, 1200, L"Mini Renderer")) return false;

    m_secondsPerTick = 1.0 / Qpf();
    m_prevTick = Qpc();

    m_renderer = std::make_unique<RenderingSystem>();

    RECT rc{};
    GetClientRect(m_window->GetHwnd(), &rc);
    if (!m_renderer->Initialize(m_window->GetHwnd(),
                                 (uint32_t)(rc.right - rc.left),
                                 (uint32_t)(rc.bottom - rc.top)))
        return false;

    ApplySceneCameraPreset(RenderingSystem::ScatterSceneMode);
    ApplyHandRenderMode();
    ApplySponzaRenderMode();
    ApplySponzaUvEffects();
    UpdateWindowTitle();
    return true;
}

int App::Run()
{
    MSG msg{};
    while (!m_exitRequested)
    {
        while (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE))
        {
            if (msg.message == WM_QUIT) { m_exitRequested = true; break; }
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        uint64_t now = Qpc();
        float dt = (float)((now - m_prevTick) * m_secondsPerTick);
        m_prevTick = now;
        Update(dt);
        if (m_renderer) m_renderer->Draw(dt);
    }
    return 0;
}

void App::Update(float dt)
{
    if (!m_input || !m_renderer || !m_window) return;
    if (m_input->IsKeyDown(VK_ESCAPE)) { m_exitRequested = true; return; }

    HandleSceneHotkeys();
    HandleSceneFeatureHotkeys();
    UpdateCameraController(dt);
    m_renderer->SetCamera(m_camPos, m_camYaw, m_camPitch);

    if (m_renderer->GetSceneMode() == RenderingSystem::ScatterSceneMode) UpdateWindowTitle();
}

void App::HandleSceneHotkeys()
{
    if (JustPressed(m_input->IsKeyDown(VK_TAB), m_prevTab))
    {
        m_renderer->SetSceneMode((m_renderer->GetSceneMode() + 1) % RenderingSystem::SceneModeCount);
        ApplySceneCameraPreset(m_renderer->GetSceneMode());
        UpdateWindowTitle();
    }
}

void App::HandleSceneFeatureHotkeys()
{
    const int sceneMode = m_renderer->GetSceneMode();
    const bool nDown = m_input->IsKeyDown('N');
    const bool mDown = m_input->IsKeyDown('M');
    const bool tDown = m_input->IsKeyDown('T');
    const bool fDown = m_input->IsKeyDown('F');
    const bool oDown = m_input->IsKeyDown('O');

    if (sceneMode == RenderingSystem::ScatterSceneMode)
    {
        m_prevN = nDown;
        m_prevM = mDown;
        m_prevT = tDown;
        if (JustPressed(fDown, m_prevF)) { m_renderer->ToggleFrustumCulling(); UpdateWindowTitle(); }
        if (JustPressed(oDown, m_prevO)) { m_renderer->ToggleOctreeCulling(); UpdateWindowTitle(); }
        return;
    }

    m_prevF = fDown;
    m_prevO = oDown;

    if (JustPressed(nDown, m_prevN))
    {
        if (sceneMode == RenderingSystem::HandSceneMode)
        {
            m_handUseNormal = !m_handUseNormal;
            ApplyHandRenderMode();
        }
        else
        {
            m_sponzaUseNormal = !m_sponzaUseNormal;
            ApplySponzaRenderMode();
        }
    }

    if (sceneMode == RenderingSystem::HandSceneMode)
    {
        if (JustPressed(mDown, m_prevM))
        {
            m_handUseDisp = !m_handUseDisp;
            ApplyHandRenderMode();
        }
        m_prevT = tDown;
        return;
    }

    m_prevM = mDown;
    if (JustPressed(tDown, m_prevT))
    {
        m_sponzaUvEffects = !m_sponzaUvEffects;
        ApplySponzaUvEffects();
    }
}

void App::UpdateCameraController(float dt)
{
    if (m_input->IsKeyDown(VK_RBUTTON))
    {
        HWND hwnd = m_window->GetHwnd();
        RECT rc{}; GetClientRect(hwnd, &rc);
        POINT center{(rc.right - rc.left) / 2, (rc.bottom - rc.top) / 2};
        POINT cs = center; ClientToScreen(hwnd, &cs);

        if (!m_rmbLookActive)
        {
            SetCursorPos(cs.x, cs.y);
            m_rmbLookActive = true;
        }
        else
        {
            POINT cur{}; GetCursorPos(&cur);
            m_camYaw += (cur.x - cs.x) * 0.005f;
            m_camPitch -= (cur.y - cs.y) * 0.005f;
            m_camPitch = std::clamp(m_camPitch, -(XM_PIDIV2 - 0.01f), XM_PIDIV2 - 0.01f);
            SetCursorPos(cs.x, cs.y);
        }
    }
    else
    {
        m_rmbLookActive = false;
    }

    const float speed = m_camMoveSpeed * (m_input->IsKeyDown(VK_SHIFT) ? 2.75f : 1.f);
    XMVECTOR fwd = XMVector3Normalize(XMVectorSet(sinf(m_camYaw), 0.f, cosf(m_camYaw), 0.f));
    XMVECTOR right = XMVector3Normalize(XMVector3Cross(XMVectorSet(0.f, 1.f, 0.f, 0.f), fwd));
    XMVECTOR move = XMVectorZero();

    if (m_input->IsKeyDown('W')) move = XMVectorAdd(move, fwd);
    if (m_input->IsKeyDown('S')) move = XMVectorSubtract(move, fwd);
    if (m_input->IsKeyDown('A')) move = XMVectorSubtract(move, right);
    if (m_input->IsKeyDown('D')) move = XMVectorAdd(move, right);
    if (m_input->IsKeyDown('E')) move = XMVectorAdd(move, XMVectorSet(0.f, 1.f, 0.f, 0.f));
    if (m_input->IsKeyDown('Q')) move = XMVectorSubtract(move, XMVectorSet(0.f, 1.f, 0.f, 0.f));

    if (!XMVector3Equal(move, XMVectorZero()))
    {
        XMVECTOR pos = XMLoadFloat3(&m_camPos);
        pos = XMVectorAdd(pos, XMVectorScale(XMVector3Normalize(move), speed * dt));
        XMStoreFloat3(&m_camPos, pos);
    }
}

void App::ApplyHandRenderMode()
{
    if (!m_renderer) return;
    m_renderer->SetHandFeatures(m_handUseNormal, m_handUseDisp);
    UpdateWindowTitle();
}

void App::ApplySponzaRenderMode()
{
    if (!m_renderer) return;
    m_renderer->SetSponzaFeatures(m_sponzaUseNormal);
    UpdateWindowTitle();
}

void App::ApplySponzaUvEffects()
{
    if (!m_renderer) return;
    m_renderer->SetSponzaUvEffectsEnabled(m_sponzaUvEffects);
    UpdateWindowTitle();
}

void App::ApplySceneCameraPreset(int sceneMode)
{
    const scene_profiles::CameraPreset preset = scene_profiles::GetCameraPresetForScene(sceneMode);
    m_camPos = preset.Position;
    m_camYaw = preset.Yaw;
    m_camPitch = preset.Pitch;
    m_camMoveSpeed = preset.MoveSpeed;
    if (m_renderer)
    {
        m_renderer->SetProjectionClipRange(preset.NearClip, preset.FarClip);
        m_renderer->SetCamera(m_camPos, m_camYaw, m_camPitch);
    }
}

void App::UpdateWindowTitle()
{
    if (!m_window || !m_renderer) return;
    wchar_t buf[256];
    if (m_renderer->GetSceneMode() == RenderingSystem::ScatterSceneMode)
    {
        swprintf_s(buf, L"Lab-8  |  Scatter 1300  |  [F] Frustum: %-3s  [O] Octree: %-3s  |  Visible: %u/1300  |  [Tab] Hand scene",
            m_renderer->FrustumCullingEnabled() ? L"ON" : L"OFF",
            m_renderer->OctreeCullingEnabled() ? L"ON" : L"OFF",
            m_renderer->ScatterVisibleCount());
    }
    else if (m_renderer->GetSceneMode() == RenderingSystem::SponzaSceneMode)
    {
        swprintf_s(
            buf,
            L"Lab-5/6  |  Sponza Deferred  |  [N] Normals: %-3s  [T] UV FX: %-3s  |  Dir + Point + Spot  |  [Tab] Scatter scene",
            m_sponzaUseNormal ? L"ON" : L"OFF",
            m_renderer->SponzaUvEffectsEnabled() ? L"ON" : L"OFF");
    }
    else
    {
        swprintf_s(buf, L"Lab-7  |  Hand + Water  |  Tessellation active  |  [N] Normals: %-3s  [M] Displacement: %-3s  |  [Tab] Sponza scene",
            m_handUseNormal ? L"ON" : L"OFF",
            m_handUseDisp ? L"ON" : L"OFF");
    }
    SetWindowTextW(m_window->GetHwnd(), buf);
}

LRESULT App::HandleWindowMessage(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
    switch (msg)
    {
    case WM_CLOSE:
    case WM_DESTROY:
        m_exitRequested = true;
        PostQuitMessage(0);
        return 0;

    case WM_KEYDOWN: if (m_input) m_input->OnKeyDown((uint32_t)wparam); return 0;
    case WM_KEYUP: if (m_input) m_input->OnKeyUp((uint32_t)wparam); return 0;
    case WM_MOUSEMOVE: if (m_input) m_input->OnMouseMove(GET_X_LPARAM(lparam), GET_Y_LPARAM(lparam)); return 0;

    case WM_RBUTTONDOWN:
        if (m_input) m_input->OnKeyDown(VK_RBUTTON);
        GetCursorPos(&m_savedCursor);
        ShowCursor(FALSE);
        SetCapture(hwnd);
        return 0;

    case WM_RBUTTONUP:
        if (m_input) m_input->OnKeyUp(VK_RBUTTON);
        m_rmbLookActive = false;
        SetCursorPos(m_savedCursor.x, m_savedCursor.y);
        ShowCursor(TRUE);
        ReleaseCapture();
        return 0;

    case WM_SIZE:
    {
        uint32_t w = LOWORD(lparam), h = HIWORD(lparam);
        if (w && h && m_renderer) m_renderer->OnResize(w, h);
        return 0;
    }
    default: return DefWindowProcW(hwnd, msg, wparam, lparam);
    }
}
