cmake_minimum_required(VERSION 3.20)
project(dx12_optimization_winapi LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(dx12_optimization
    App.cpp
    DeferredScene.cpp
    FrustumCuller.cpp
    GBuffer.cpp
    ImageLoader.cpp
    Input.cpp
    main.cpp
    ObjLoader.cpp
    Octree.cpp
    RenderingSystem.cpp
    ScatterScene.cpp
    SceneObjectManager.cpp
    Window.cpp
)

target_compile_definitions(dx12_optimization PRIVATE UNICODE _UNICODE WIN32_LEAN_AND_MEAN NOMINMAX)

set_target_properties(dx12_optimization PROPERTIES WIN32_EXECUTABLE TRUE)

target_link_libraries(dx12_optimization PRIVATE
    user32 gdi32
    dxgi d3d12
)