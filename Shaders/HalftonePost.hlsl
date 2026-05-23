float3 ApplyHalftone(float2 uv, float3 sceneColor)
{
    const float gridSize = max(gHalftoneParams.x, 1.0);
    const float maxRadius = saturate(gHalftoneParams.y);
    const float strength = saturate(gHalftoneParams.z);
    const bool monochromeMode = gHalftoneParams.w > 0.5;

    const float aspect = gRenderTargetSize.x * gRenderTargetSize.w;
    const float2 grid = float2(gridSize * aspect, gridSize);

    const float2 cell = floor(uv * grid);
    const float2 centerUv = (cell + 0.5) / grid;
    const float3 cellColor = gSceneColor.SampleLevel(gLinearSampler, centerUv, 0).rgb;
    const float luma = saturate(AnalysisLuma(cellColor));

    const float2 cellUv = frac(uv * grid);
    const float dist = length(cellUv - 0.5);
    const float radiusLuma = saturate(pow(luma, 0.48) * 1.18 + 0.12);
    const float radius = maxRadius * radiusLuma;
    const float aa = max(fwidth(dist), 0.0001);
    const float dotMask = 1.0 - smoothstep(radius - aa, radius + aa, dist);

    const float3 dotColor = monochromeMode ? float3(luma, luma, luma) : cellColor;
    const float3 halftoneColor = dotColor * dotMask;
    return lerp(sceneColor, halftoneColor, strength);
}
