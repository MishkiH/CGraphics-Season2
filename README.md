# Mini Renderer Labs 4-8

Единая учебная кодовая база на C++ / DirectX 12, в которой собраны лабораторные 4, 5, 6, 7 и 8 без разбиения на отдельные копии проекта.

## Что есть сейчас

- `Hand + Water` — deferred-сцена с normal mapping, displacement mapping, tessellation и water pass.
- `Sponza Deferred` — deferred-сцена для lab 5/6 с directional, point и spot light, а также UV tiling и UV animation.
- `Scatter 300` — сцена для lab 8 с большим числом объектов, frustum culling и octree.

Сцены переключаются по `Tab` циклически:

1. `Hand + Water`
2. `Sponza Deferred`
3. `Scatter 300`

## Управление

- `Tab` — следующая сцена
- `N` — вкл/выкл normal mapping в deferred-сценах
- `M` — вкл/выкл displacement mapping в `Hand + Water`
- `T` — вкл/выкл UV tiling + UV animation в `Sponza Deferred`
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
  rendering/     RenderingSystem, DeferredScene, ScatterScene, GBuffer, SceneProfiles
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

## Карта подсистем

- `RenderingSystem` владеет DX12 device, swap chain и back buffers, а отрисовку делегирует активной сцене.
- `DeferredScene` реализует geometry pass, lighting pass, G-buffer и опциональный water pass.
- `ScatterScene` рисует 300 инстансов и использует `SceneObjectManager`, `Frustum`, `Octree`.
- `ObjLoader` читает `.obj + .mtl`, материалы и связанные diffuse/normal/displacement карты.
- `Dx12Helpers` собирает общие helper-функции для upload, буферов, текстур и синхронизации.
- `SceneProfiles` хранит пресеты сцен, света и стартовых камер в одном месте.

## Соответствие лабораторным

- Lab 4: есть компиляция и загрузка шейдеров, constant buffer, root signature, PSO и освещение по Фонгу в deferred lighting pass.
- Lab 5: есть загрузка текстур, OBJ/MTL pipeline, материалы, UV tiling и UV scroll-анимация через `gUvOffsetTiling` в сцене Sponza.
- Lab 6: есть `RenderingSystem`, `GBuffer`, deferred rendering и сцена Sponza с directional, point и spot light.
- Lab 7: есть normal/displacement texture, tessellation, displacement по texture и зависимость tessellation level от расстояния до камеры.
- Lab 8: есть сцена на 300 объектов, frustum culling, переключение culling, octree и переключение octree-ускорения отдельно.

## Что важно про оптимизацию

- В lab 8 реализована именно та оптимизация, которую обычно требуют в ТЗ: CPU-side frustum culling плюс пространственное разбиение сцены через octree.
- `F` полностью отключает frustum culling.
- `O` включает или выключает octree как ускоритель для того же frustum culling.
- Это не occlusion culling и не GPU-driven culling, но для формулировки лабораторной этого достаточно.

## Ассеты

Шейдеры и меши ищутся через `ResolveAsset()` по нескольким относительным путям, поэтому проект остаётся устойчивым к запуску из корня репозитория или из каталога сборки.

## Ограничения текущего прохода

- Аудит и рефакторинг выполнялись по коду, без обязательного compile/run.
- Проект остаётся одной общей кодовой базой, а не набором отдельных проектов под каждую лабораторную.
