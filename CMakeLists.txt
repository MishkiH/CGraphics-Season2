cmake_minimum_required(VERSION 3.20)
project(dx12_optimization_winapi LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(dx12_optimization
    src/core/App.cpp
    src/core/Input.cpp
    src/core/main.cpp
    src/core/Window.cpp
    src/rendering/DeferredScene.cpp
    src/rendering/GBuffer.cpp
    src/rendering/ParticleScene.cpp
    src/rendering/RenderingSystem.cpp
    src/rendering/ScatterScene.cpp
    src/assets/ImageLoader.cpp
    src/assets/ObjLoader.cpp
    src/scene/FrustumCuller.cpp
    src/scene/Octree.cpp
    src/scene/SceneObjectManager.cpp
)

target_compile_definitions(dx12_optimization PRIVATE UNICODE _UNICODE WIN32_LEAN_AND_MEAN NOMINMAX)
target_include_directories(dx12_optimization PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include/core
    ${CMAKE_CURRENT_SOURCE_DIR}/include/rendering
    ${CMAKE_CURRENT_SOURCE_DIR}/include/assets
    ${CMAKE_CURRENT_SOURCE_DIR}/include/scene
    ${CMAKE_CURRENT_SOURCE_DIR}/include/third_party
)

set_target_properties(dx12_optimization PROPERTIES WIN32_EXECUTABLE TRUE)

target_link_libraries(dx12_optimization PRIVATE
    user32 gdi32
    dxgi d3d12
    d3dcompiler dxguid windowscodecs ole32
)
