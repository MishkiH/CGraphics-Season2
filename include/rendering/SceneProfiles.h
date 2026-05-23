#pragma once

#include <DirectXMath.h>

#include "DeferredScene.h"

namespace scene_profiles
{
    struct CameraPreset
    {
        DirectX::XMFLOAT3 Position{0.f, 0.f, 0.f};
        float Yaw = 0.f;
        float Pitch = 0.f;
        float MoveSpeed = 8.f;
        float NearClip = 0.05f;
        float FarClip = 1000.f;
    };

    inline DeferredScene::SceneLight MakeDirectionalLight(
        const DirectX::XMFLOAT3& direction,
        const DirectX::XMFLOAT3& color,
        float intensity)
    {
        DeferredScene::SceneLight light{};
        light.LightType = DeferredScene::SceneLight::Type::Directional;
        light.Direction = direction;
        light.Color = color;
        light.Intensity = intensity;
        return light;
    }

    inline DeferredScene::SceneLight MakePointLight(
        const DirectX::XMFLOAT3& position,
        const DirectX::XMFLOAT3& color,
        float intensity,
        float range)
    {
        DeferredScene::SceneLight light{};
        light.LightType = DeferredScene::SceneLight::Type::Point;
        light.Position = position;
        light.Color = color;
        light.Intensity = intensity;
        light.Range = range;
        return light;
    }

    inline DeferredScene::SceneLight MakeSpotLight(
        const DirectX::XMFLOAT3& position,
        const DirectX::XMFLOAT3& direction,
        const DirectX::XMFLOAT3& color,
        float intensity,
        float range,
        float innerAngleDeg,
        float outerAngleDeg)
    {
        DeferredScene::SceneLight light{};
        light.LightType = DeferredScene::SceneLight::Type::Spot;
        light.Position = position;
        light.Direction = direction;
        light.Color = color;
        light.Intensity = intensity;
        light.Range = range;
        light.InnerConeDegrees = innerAngleDeg;
        light.OuterConeDegrees = outerAngleDeg;
        return light;
    }

    inline DeferredScene::SceneOptions MakeHandSceneOptions()
    {
        DeferredScene::SceneOptions options;
        options.MeshPath = "Meshes/hand/handd.obj";
        options.EnableWater = true;
        options.UseTessellation = true;
        options.EnableNormalMapping = true;
        options.EnableDisplacement = true;
        options.SceneScale = 1.f;
        options.AmbientColor = {0.3f, 0.3f, 0.3f};
        options.SkyTopColor = {0.02f, 0.05f, 0.12f};
        options.ClearColor = {0.005f, 0.02f, 0.06f};
        options.Lights.push_back(MakeDirectionalLight(
            DirectX::XMFLOAT3{0.4f, -1.f, 0.3f},
            DirectX::XMFLOAT3{1.f, 0.98f, 0.9f},
            1.8f));
        return options;
    }

    inline DeferredScene::SceneOptions MakeSponzaSceneOptions()
    {
        DeferredScene::SceneOptions options;
        options.MeshPath = "Meshes/sponza/sponza.obj";
        options.EnableWater = false;
        options.UseTessellation = false;
        options.EnableNormalMapping = true;
        options.EnableDisplacement = false;
        options.SceneScale = 0.008f;
        options.AmbientColor = {0.12f, 0.11f, 0.10f};
        options.SkyTopColor = {0.08f, 0.16f, 0.34f};
        options.ClearColor = {0.52f, 0.28f, 0.12f};
        options.UvTiling = {1.55f, 1.55f};
        options.UvScrollRate = {0.03f, 0.012f};
        options.Lights.push_back(MakeDirectionalLight(
            DirectX::XMFLOAT3{0.32f, -1.f, 0.18f}, // direction
            DirectX::XMFLOAT3{0.92f, 0.52f, 0.30f},// color
            0.95f));                               // intensity
        options.Lights.push_back(MakePointLight(
            DirectX::XMFLOAT3{8.5f, 2.3f, -0.4f},  // position
            DirectX::XMFLOAT3{1.0f, 0.42f, 0.08f}, // color
            3.5f,                                  // intensity
            4.f));                                 // range
        options.Lights.push_back(MakeSpotLight(
            DirectX::XMFLOAT3{-7.8f, 13.f, -1.4f}, // position
            DirectX::XMFLOAT3{0.16f, -1.f, 0.03f}, // direction
            DirectX::XMFLOAT3{0.74f, 0.28f, 1.0f}, // color
            6.1f,                                  // intensity
            15.5f,                                 // range
            5.5f,                                  // inner cone angle
            8.5f));                                // outer cone angle
        return options;
    }

    inline CameraPreset GetCameraPresetForScene(int sceneMode)
    {
        switch (sceneMode)
        {
        case 0:
            return {{19.1f, 18.f, 40.3f}, -2.70f, -0.20f, 22.f, 0.05f, 520.f};
        case 1:
            return {{-340.f, 62.f, -340.f}, 0.82f, -0.11f, 42.f, 0.05f, 2600.f};
        case 2:
            return {{-24.f, 8.f, -24.f}, 1.f, 0.f, 8.f, 0.05f, 1000.f};
        case 3:
            return {{-6.5f, 1.7f, -3.5f}, 0.92f, -0.05f, 7.f, 0.05f, 900.f};
        default:
            return {{-9.2f, 4.7f, -12.6f}, 0.63f, -0.17f, 10.f, 0.05f, 220.f};
        }
    }
}
