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
        options.AmbientColor = {0.025f, 0.025f, 0.03f};
        options.UvTiling = {1.55f, 1.55f};
        options.UvScrollRate = {0.03f, 0.012f};

        // Broad cool fill that keeps the atrium readable without washing out local lights.
        options.Lights.push_back(MakeDirectionalLight(
            DirectX::XMFLOAT3{0.4f, -1.f, 0.3f},
            DirectX::XMFLOAT3{0.58f, 0.62f, 1.0f},
            1.1f));

        // Warm point light near the right gallery to make the local falloff obvious.
        options.Lights.push_back(MakePointLight(
            DirectX::XMFLOAT3{8.5f, 2.3f, -0.4f},
            DirectX::XMFLOAT3{1.0f, 0.18f, 0.12f},
            4.2f,
            4.8f));

        // Focused green spotlight from the upper left wing for a clearly visible cone.
        options.Lights.push_back(MakeSpotLight(
            DirectX::XMFLOAT3{-7.5f, 12.8f, -0.3f},
            DirectX::XMFLOAT3{0.55f, -1.f, 0.05f},
            DirectX::XMFLOAT3{0.18f, 1.0f, 0.25f},
            5.3f,
            10.5f,
            9.f,
            22.f));
        return options;
    }

    inline CameraPreset GetCameraPresetForScene(int sceneMode)
    {
        switch (sceneMode)
        {
        case 1:
            return {{-6.5f, 1.7f, -3.5f}, 0.92f, -0.05f, 7.f, 0.05f, 900.f};
        case 2:
            return {{-340.f, 62.f, -340.f}, 0.82f, -0.11f, 42.f, 0.05f, 2600.f};
        default:
            return {{-24.f, 8.f, -24.f}, 1.f, 0.f, 8.f, 0.05f, 1000.f};
        }
    }
}
