#ifndef SHADOW_COMMON_HLSL
#define SHADOW_COMMON_HLSL

uint SelectShadowCascade(float viewDepth)
{
    if (viewDepth <= gCascadeFar.x) return 0u;
    if (viewDepth <= gCascadeFar.y) return 1u;
    if (viewDepth <= gCascadeFar.z) return 2u;
    return 3u;
}

float SampleCsmShadowPcf(float3 worldPos, float viewDepth, float3 normalW, float3 lightToSurfaceDirection)
{
    if (viewDepth > gCascadeFar.w)
        return 1.0;

    const uint cascadeIndex = SelectShadowCascade(viewDepth);
    const float4 lightClip = mul(float4(worldPos, 1.0), gLightViewProj[cascadeIndex]);
    if (lightClip.w <= 0.0)
        return 1.0;

    const float3 proj = lightClip.xyz / lightClip.w;
    const float2 uv = float2(proj.x * 0.5 + 0.5, -proj.y * 0.5 + 0.5);
    if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0 || proj.z < 0.0 || proj.z > 1.0)
        return 1.0;

    const float slopeScale = lerp(
        2.0,
        0.65,
        saturate(dot(normalize(normalW), normalize(lightToSurfaceDirection))));
    const float compareDepth = proj.z - gShadowParams.z * slopeScale;
    const float2 texelSize = gShadowParams.xy;

    // 3x3 PCF
    float visibility = 0.0;
    [unroll]
    for (int y = -1; y <= 1; ++y)
    {
        [unroll]
        for (int x = -1; x <= 1; ++x)
        {
            visibility += gShadowMaps.SampleCmpLevelZero(
                gShadowSampler,
                float3(uv + float2(x, y) * texelSize, cascadeIndex),
                compareDepth);
        }
    }

    return visibility / 9.0;
}

#endif

