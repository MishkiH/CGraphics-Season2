cmake_minimum_required(VERSION 3.20)
project(dx12_textures_winapi LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(dx12_textures
    main.cpp
    App.cpp
    Window.cpp
    Input.cpp
    D3D12Context.cpp
)

target_compile_definitions(dx12_textures PRIVATE UNICODE _UNICODE WIN32_LEAN_AND_MEAN NOMINMAX)

set_target_properties(dx12_textures PROPERTIES WIN32_EXECUTABLE TRUE)

target_link_libraries(dx12_textures PRIVATE
    user32 gdi32
    dxgi d3d12
)