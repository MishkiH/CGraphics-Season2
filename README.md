# Lab-7 — DX12 Deferred Renderer + Frustum Culling + Octree

## Быстрый старт

1. Скопировать все файлы из этой папки в папку проекта (перезаписать старые).
2. Добавить в VS-проект новые `.cpp` файлы (Add → Existing Item):
   `ImageLoader.cpp`, `ObjLoader.cpp`, `FrustumCuller.cpp`, `Octree.cpp`,
   `SceneObjectManager.cpp`, `DeferredScene.cpp`, `ScatterScene.cpp`.
3. Создать папку `Meshes/` рядом с `.exe`, положить туда `shrek.obj` / `donkey.obj`
   (включая их папку `textures/`).
4. Собрать и запустить — никаких новых внешних зависимостей нет.

---

## Управление

| Клавиша | Действие |
|---------|----------|
| **Tab** | Переключить сцену: Hand+Water ↔ Scatter 300 |
| **N** | (Hand) Normal mapping вкл/выкл |
| **M** | (Hand) Displacement mapping вкл/выкл |
| **F** | (Scatter) Frustum Culling вкл/выкл |
| **O** | (Scatter) Octree ускорение вкл/выкл |
| **ПКМ + мышь** | Look-around камеры |
| **WASD** | Движение горизонтальное |
| **Q / E** | Вниз / вверх |
| **Shift** | Ускорение ×2.5 |
| **Esc** | Выход |

Текущее состояние и количество видимых объектов отображается в заголовке окна.

---

## Структура проекта

```
├── main.cpp                    — точка входа, создаёт App и запускает цикл
│
├── App.h / App.cpp             — прикладной слой
│                                 владеет Window, Input, RenderingSystem
│                                 обрабатывает ввод → команды рендереру
│
├── Window.h / Window.cpp       — обёртка над Win32 HWND и WNDCLASS
├── Input.h  / Input.cpp        — простой опросник клавиш/мыши (массив bool[256])
│
├── RenderingSystem.h / .cpp    — оркестратор рендеринга
│                                 владеет: Device, CommandQueue, SwapChain, BackBuffers
│                                 делегирует кадр DeferredScene или ScatterScene
│                                 не содержит логики рисования
│
│   ╔══════════════════════════════════════════════════════════╗
│   ║  СЦЕНА 0 — Deferred (рука + вода)                       ║
│   ╠══════════════════════════════════════════════════════════╣
│   ║  DeferredScene.h / .cpp                                  ║
│   ║    Geometry pass   → GBuffer (albedo+spec, normal+shin)  ║
│   ║    Lighting pass   → fullscreen quad, deferred Blinn-Ph. ║
│   ║    Water pass      → tessellated mesh, alpha blend        ║
│   ║    Шейдеры читаются из Shaders.hlsl                      ║
│   ║    Меш: Meshes/hand/handd.obj + MTL + текстуры           ║
│   ╚══════════════════════════════════════════════════════════╝
│
│   ╔══════════════════════════════════════════════════════════╗
│   ║  СЦЕНА 1 — Scatter (300 объектов)                        ║
│   ╠══════════════════════════════════════════════════════════╣
│   ║  ScatterScene.h / .cpp                                   ║
│   ║    Forward Blinn-Phong, diffuse texture per submesh       ║
│   ║    Три режима culling: OFF / Frustum / Frustum + Octree  ║
│   ║    Шейдеры читаются из ScatterShaders.hlsl               ║
│   ║    Меши: Meshes/shrek.obj, Meshes/donkey.obj             ║
│   ╚══════════════════════════════════════════════════════════╝
│
├── GBuffer.h / .cpp            — два render target (albedo+spec / normal+shin)
│                                 + depth stencil; управление барьерами ресурсов
│
├── Shaders.hlsl                — все шейдеры deferred сцены в одном файле
│   GeometryVS/HS/DS/PS         → tessellated geometry pass
│   LightingVS/PS               → fullscreen deferred lighting quad
│   WaterVS/HS/DS/PS            → tessellated animated water
│
├── ScatterShaders.hlsl         — шейдеры scatter сцены (VS + PS)
│
├── ObjLoader.h / .cpp          — загрузка .obj + .mtl
│   MeshVertex                  → {Pos, Normal, TexC, Tangent}
│   SubMesh                     → {IndexStart, IndexCount, tex indices, material}
│   MeshData                    → вершины, индексы, субмеши, пути к текстурам, AABB
│   ComputeTangents()           → Gram-Schmidt tangent basis
│
├── ImageLoader.h / .cpp        — загрузка TGA (встроенный) и всех WIC форматов
│   LoadImage(path, Image&)     → Image{Width, Height, BGRA[]}
│
├── AssetPath.h                 — инлайн-утилиты поиска ассета по нескольким путям
│   ResolveAsset(name)          → абсолютный путь к файлу
│   ToWide(string)              → wstring
│
├── FrustumCuller.h / .cpp      — AABB + 6-plane frustum
│   AABB                        → {Min, Max}
│   TransformAABB()             → трансформация AABB через world matrix
│   Frustum::FromViewProj()     → Gribb-Hartmann row-major plane extraction
│   Frustum::Intersects(AABB)   → positive-vertex тест
│
├── Octree.h / .cpp             — пространственное дерево
│   Build(AABBs, sceneBounds)   → рекурсивное разбиение (depth=5, minPerLeaf=8)
│   QueryVisible(frustum, out)  → обход дерева с отсечением по фрустуму
│
└── SceneObjectManager.h / .cpp — 300 инстансов (150 shrek + 150 donkey)
    Initialize()                → LoadObj × 2, PlaceInstances()
    PlaceInstances()            → random pos/rot/scale, фиксированный seed=42
    BuildOctree()               → строит Octree по world-AABB инстансов
    GetVisibleIndices()         → возвращает видимые индексы (3 режима)
```

---

## Архитектура culling

```
GetVisibleIndices(viewProj, useFrustum, useOctree)
    │
    ├─ useFrustum = false
    │       └─ все 300 индексов
    │
    ├─ useFrustum = true, useOctree = false
    │       └─ линейный перебор × 300
    │              Frustum::Intersects(instance.WorldBounds)
    │
    └─ useFrustum = true, useOctree = true
            └─ Octree::QueryVisible(frustum)
                   └─ рекурсивно по дереву:
                      если AABB узла вне frustum → отсекаем поддерево целиком
                      если лист → проверяем каждый объект
```

---

## Размещение ассетов

```
<exe_dir>/
├── Shaders.hlsl
├── ScatterShaders.hlsl
└── Meshes/
    ├── hand/
    │   ├── handd.obj
    │   ├── handd.mtl
    │   └── textures/  (diffuse, normal, displacement)
    ├── shrek/
    │   ├── shrek.obj
    │   ├── shrek.mtl
    │   └── textures/
    └── donkey/
        ├── donkey.obj
        ├── donkey.mtl
        └── textures/
```

`ResolveAsset()` ищет файлы в следующем порядке:
1. Путь как есть (относительно рабочей директории)
2. `assets/<name>`
3. `<exe_dir>/<name>`
4. `<exe_dir>/assets/<name>`
5. `../<name>`
6. `../../<name>`
