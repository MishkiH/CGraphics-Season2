#include "App.h"
#include <windows.h>

extern "C"
{
    __declspec(dllexport) unsigned long NvOptimusEnablement = 0x00000001;
    __declspec(dllexport) int AmdPowerXpressRequestHighPerformance = 1;
}

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE, PWSTR, int nCmdShow)
{
    App app;

    if (!app.Initialize(hInstance, nCmdShow))
        return 0;

    return app.Run();
}
