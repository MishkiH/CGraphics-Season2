# Mini Renderer Labs 4-8

Единая учебная кодовая база на C++ / DirectX 12, в которой собраны лабораторные 4, 5, 6, 7 и 8.

## Что есть сейчас

- `Hand + Water` — deferred-сцена с normal mapping, displacement mapping, tessellation и water pass.
- `Sponza Deferred` — deferred-сцена для lab 6 с несколькими типами света: directional, point, spot.
- `Scatter 300` — сцена для lab 8 с большим числом объектов, frustum culling и octree.

Сцены переключаются по `Tab` циклически:

1. `Hand + Water`
2. `Sponza Deferred`
3. `Scatter 300`

## Управление

- `Tab` — следующая сцена
- `N` — вкл/выкл normal mapping в deferred-сценах
- `M` — вкл/выкл displacement mapping в `Hand + Water`
- `F` — вкл/выкл frustum culling в `Scatter 300`
- `O` — вкл/выкл octree-ускорение в `Scatter 300`
- `ПКМ + мышь` — обзор камерой
- `WASD`, `Q`, `E`, `Shift` — перемещение
- `Esc` — выход

## Структура

```text
include/
  assets/        ImageLoader, ObjLoader
  core/          App, Window, Input, AssetPath, Dx12Helpers
  rendering/     RenderingSystem, DeferredScene, ScatterScene, GBuffer
  scene/         FrustumCuller, Octree, SceneObjectManager
  third_party/   DirectXMath.h

src/
  assets/
  core/
  rendering/
  scene/

Shaders/
  DeferredScene.hlsl
  ScatterScene.hlsl

Meshes/
  hand/
  sponza/
  shrek/
  donkey/
```

## Краткая карта подсистем

- `RenderingSystem` владеет DX12 device / swapchain / back buffers и делегирует кадр активной сцене.
- `DeferredScene` использует `GBuffer`, geometry pass, lighting pass и опциональный water pass.
- `ScatterScene` рисует 300 инстансов и использует `SceneObjectManager`, `FrustumCuller`, `Octree`.
- `ObjLoader` читает `.obj + .mtl`, материалы и сопутствующие текстуры.
- `Dx12Helpers` содержит общие DX12 helper-функции, чтобы не дублировать upload / buffer / texture boilerplate.

## Ассеты

Шейдеры и меши ищутся через `ResolveAsset()` по нескольким относительным путям, поэтому проект остаётся устойчивым к запуску из `build/` или из корня репозитория.
