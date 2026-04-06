#include "App.h"
#include "Window.h"
#include "Input.h"
#include <exception>

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <windowsx.h>
#include <algorithm>
#include <cmath>
#include <DirectXMath.h>

#pragma comment(lib, "d3d12.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "dxguid.lib")
#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "windowscodecs.lib")
#pragma comment(lib, "ole32.lib")

static uint64_t GetQpc() { LARGE_INTEGER t{}; QueryPerformanceCounter(&t);  return (uint64_t)t.QuadPart; }
static double   GetQpf() { LARGE_INTEGER f{}; QueryPerformanceFrequency(&f); return (double)f.QuadPart; }

// ---------------------------------------------------------------------------

void App::ApplyRenderMode()
{
    const int mode = (m_useNormal ? 1 : 0) + (m_useDisp ? 2 : 0);
    m_renderer->SetRenderMode(mode);
    UpdateWindowTitle();
}

void App::UpdateWindowTitle()
{
    if (!m_renderer || !m_window) return;

    wchar_t title[256];

    if (m_renderer->GetSceneMode() == 1)
    {
        const bool fc = m_renderer->FrustumCullingEnabled();
        const bool oc = m_renderer->OctreeCullingEnabled();
        const uint32_t vis = m_renderer->ScatterVisibleCount();
        swprintf_s(title,
            L"Lab-7  [Scatter 300]  |  "
            L"[F] Frustum: %-3s  "
            L"[O] Octree: %-3s  "
            L"| Visible: %u/300  |  "
            L"[Tab] -> Hand scene",
            fc ? L"ON" : L"OFF",
            oc ? L"ON" : L"OFF",
            vis);
    }
    else
    {
        swprintf_s(title,
            L"Lab-7  [Hand+Water]  |  "
            L"[N] Normal: %-3s  "
            L"[M] Displacement: %-3s  |  "
            L"[Tab] -> Scatter scene",
            m_useNormal ? L"ON" : L"OFF",
            m_useDisp   ? L"ON" : L"OFF");
    }

    SetWindowTextW(m_window->GetHwnd(), title);
}

// ---------------------------------------------------------------------------

bool App::Initialize(HINSTANCE hInstance, int nCmdShow)
{
    m_window = new Window();
    m_input  = new Input();
    m_input->Reset();

    if (!m_window->Create(this, hInstance, nCmdShow, 1280, 720, L"Lab-7"))
        return false;

    m_secondsPerTick = 1.0 / GetQpf();
    m_prevTick       = GetQpc();

    m_renderer = new RenderingSystem();

    RECT rc{};
    GetClientRect(m_window->GetHwnd(), &rc);
    if (!m_renderer->Initialize(m_window->GetHwnd(),
        (uint32_t)(rc.right - rc.left), (uint32_t)(rc.bottom - rc.top)))
        return false;

    ApplyRenderMode();
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
        const uint64_t now = GetQpc();
        const float    dt  = (float)((now - m_prevTick) * m_secondsPerTick);
        m_prevTick = now;

        Update(dt);
        if (m_renderer) m_renderer->Draw(dt);
    }
    return 0;
}

void App::Update(float dt)
{
    using namespace DirectX;

    if (m_input && m_input->IsKeyDown(VK_ESCAPE))
        m_exitRequested = true;

    if (!m_input || !m_renderer || !m_window) return;

    // ------------------------------------------------------------------
    // Tab — switch between deferred (hand+water) and scatter (300 objs)
    // ------------------------------------------------------------------
    const bool tabDown = m_input->IsKeyDown(VK_TAB);
    if (tabDown && !m_tabWasDown)
    {
        const int newMode = 1 - m_renderer->GetSceneMode();
        m_renderer->SetSceneMode(newMode);
        UpdateWindowTitle();
    }
    m_tabWasDown = tabDown;

    // ------------------------------------------------------------------
    // Deferred scene: N = normal map, M = displacement
    // ------------------------------------------------------------------
    if (m_renderer->GetSceneMode() == 0)
    {
        const bool nDown = m_input->IsKeyDown('N');
        if (nDown && !m_nKeyWasDown)
        {
            m_useNormal = !m_useNormal;
            ApplyRenderMode();
        }
        m_nKeyWasDown = nDown;

        const bool mDown = m_input->IsKeyDown('M');
        if (mDown && !m_mKeyWasDown)
        {
            m_useDisp = !m_useDisp;
            ApplyRenderMode();
        }
        m_mKeyWasDown = mDown;
    }

    // ------------------------------------------------------------------
    // Scatter scene: F = frustum culling, O = octree culling
    // ------------------------------------------------------------------
    if (m_renderer->GetSceneMode() == 1)
    {
        const bool fDown = m_input->IsKeyDown('F');
        if (fDown && !m_fKeyWasDown)
        {
            m_renderer->ToggleFrustumCulling();
            UpdateWindowTitle();
        }
        m_fKeyWasDown = fDown;

        const bool oDown = m_input->IsKeyDown('O');
        if (oDown && !m_oKeyWasDown)
        {
            m_renderer->ToggleOctreeCulling();
            UpdateWindowTitle();
        }
        m_oKeyWasDown = oDown;
    }

    // ------------------------------------------------------------------
    // Camera — right mouse button look, WASD + Q/E
    // ------------------------------------------------------------------
    if (m_input->IsKeyDown(VK_RBUTTON))
    {
        HWND  hwnd = m_window->GetHwnd();
        RECT  rc{};  GetClientRect(hwnd, &rc);
        POINT center{ (rc.right - rc.left) / 2, (rc.bottom - rc.top) / 2 };
        POINT centerScreen = center;
        ClientToScreen(hwnd, &centerScreen);

        if (m_justEnteredRmbLook)
        {
            SetCursorPos(centerScreen.x, centerScreen.y);
            m_justEnteredRmbLook = false;
            return;
        }

        POINT cur{}; GetCursorPos(&cur);
        m_camYaw   += (cur.x - centerScreen.x) * 0.005f;
        m_camPitch -= (cur.y - centerScreen.y) * 0.005f;
        m_camPitch  = std::clamp(m_camPitch, -(XM_PIDIV2 - 0.1f), XM_PIDIV2 - 0.1f);
        SetCursorPos(centerScreen.x, centerScreen.y);
    }

    const float speed = m_input->IsKeyDown(VK_SHIFT) ? 20.f : 8.f;

    const XMVECTOR fwd   = XMVector3Normalize(XMVectorSet(sinf(m_camYaw), 0, cosf(m_camYaw), 0));
    const XMVECTOR right = XMVector3Normalize(XMVector3Cross(XMVectorSet(0, 1, 0, 0), fwd));
    const XMVECTOR up    = XMVectorSet(0, 1, 0, 0);
    XMVECTOR move        = XMVectorZero();

    if (m_input->IsKeyDown('W')) move = XMVectorAdd(move, fwd);
    if (m_input->IsKeyDown('A')) move = XMVectorSubtract(move, right);
    if (m_input->IsKeyDown('S')) move = XMVectorSubtract(move, fwd);
    if (m_input->IsKeyDown('D')) move = XMVectorAdd(move, right);
    if (m_input->IsKeyDown('E')) move = XMVectorAdd(move, up);
    if (m_input->IsKeyDown('Q')) move = XMVectorSubtract(move, up);

    if (!XMVector3Equal(move, XMVectorZero()))
    {
        XMVECTOR pos = XMLoadFloat3(&m_camPos);
        pos = XMVectorAdd(pos, XMVectorScale(XMVector3Normalize(move), speed * dt));
        XMStoreFloat3(&m_camPos, pos);
    }

    m_renderer->SetCamera(m_camPos, m_camYaw, m_camPitch);

    // Update title every frame only for scatter scene to show live visible count
    if (m_renderer->GetSceneMode() == 1)
        UpdateWindowTitle();
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
    case WM_KEYUP:   if (m_input) m_input->OnKeyUp((uint32_t)wparam);   return 0;

    case WM_RBUTTONDOWN:
        if (m_input) m_input->OnKeyDown(VK_RBUTTON);
        m_justEnteredRmbLook = true;
        GetCursorPos(&m_savedCursorPos);
        ShowCursor(FALSE);
        SetCapture(hwnd);
        return 0;

    case WM_RBUTTONUP:
        if (m_input) m_input->OnKeyUp(VK_RBUTTON);
        SetCursorPos(m_savedCursorPos.x, m_savedCursorPos.y);
        ShowCursor(TRUE);
        ReleaseCapture();
        return 0;

    case WM_MOUSEMOVE:
        if (m_input) m_input->OnMouseMove(GET_X_LPARAM(lparam), GET_Y_LPARAM(lparam));
        return 0;

    case WM_SIZE:
    {
        const uint32_t w = LOWORD(lparam), h = HIWORD(lparam);
        if (w && h && m_renderer) m_renderer->OnResize(w, h);
        return 0;
    }
    default: return DefWindowProc(hwnd, msg, wparam, lparam);
    }
}
