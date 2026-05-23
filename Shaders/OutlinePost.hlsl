float SceneLuma(float2 uv)
{
    const float3 color = gSceneColor.SampleLevel(gPointSampler, saturate(uv), 0).rgb;
    return AnalysisLuma(color);
}

float EdgeDiff(float2 uv, float2 texel, out float2 gradient)
{
    const float center = SceneLuma(uv);
    const float left = SceneLuma(uv + float2(-texel.x, 0.0));
    const float right = SceneLuma(uv + float2(texel.x, 0.0));
    const float up = SceneLuma(uv + float2(0.0, -texel.y));
    const float down = SceneLuma(uv + float2(0.0, texel.y));

    gradient = float2(right - left, down - up);

    float diff = 0.0;
    diff += abs(center - left);
    diff += abs(center - right);
    diff += abs(center - up);
    diff += abs(center - down);
    return diff;
}

float ComputeLumaEdge(float2 uv, out float2 gradient)
{
    const float threshold = max(gOutlineParams.x, 0.0001);
    const float thickness = max(gOutlineParams.y, 1.0);
    const float2 texel = gRenderTargetSize.zw * thickness;
    const float diff = EdgeDiff(uv, texel, gradient);

    return smoothstep(threshold * 0.90, threshold * 1.35, diff);
}

float ComputeLumaEdge(float2 uv)
{
    float2 gradient = 0.0;
    return ComputeLumaEdge(uv, gradient);
}

float3 HueToRgb(float hue)
{
    const float3 offsets = float3(0.0, 0.6666667, 0.3333333);
    return saturate(abs(frac(hue + offsets) * 6.0 - 3.0) - 1.0);
}

float3 NeonColor(float2 uv, float2 gradient)
{
    const float angleHue = atan2(gradient.y, gradient.x) * 0.15915494;
    const float screenHue = uv.x * 0.36 + uv.y * 0.24;
    const float hue = frac(angleHue + screenHue);

    float3 color = HueToRgb(hue);
    color = lerp(color, float3(0.16, 0.85, 1.0), 0.12);
    return color;
}

float3 ApplyOutline(float2 uv, float3 sceneColor)
{
    float2 gradient = 0.0;
    const float edge = ComputeLumaEdge(uv, gradient);
    const float thickness = max(gOutlineParams.y, 1.0);
    const float2 glowTexel = gRenderTargetSize.zw * thickness * 3.0;

    float glow = edge * 0.75;
    glow += ComputeLumaEdge(uv + float2(-glowTexel.x, 0.0)) * 0.30;
    glow += ComputeLumaEdge(uv + float2( glowTexel.x, 0.0)) * 0.30;
    glow += ComputeLumaEdge(uv + float2(0.0, -glowTexel.y)) * 0.30;
    glow += ComputeLumaEdge(uv + float2(0.0,  glowTexel.y)) * 0.30;
    glow += ComputeLumaEdge(uv + float2(-glowTexel.x, -glowTexel.y)) * 0.16;
    glow += ComputeLumaEdge(uv + float2( glowTexel.x, -glowTexel.y)) * 0.16;
    glow += ComputeLumaEdge(uv + float2(-glowTexel.x,  glowTexel.y)) * 0.16;
    glow += ComputeLumaEdge(uv + float2( glowTexel.x,  glowTexel.y)) * 0.16;
    glow = saturate(glow * 0.34);

    const float strength = saturate(gOutlineColor.a);
    const float3 neon = NeonColor(uv, gradient);
    float3 color = sceneColor + neon * glow * strength * 0.42;
    color = lerp(color, neon * 1.18, edge * strength * 0.72);
    return color;
}
