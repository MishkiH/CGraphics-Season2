#include "PostProcessCommon.hlsl"
#include "HalftonePost.hlsl"
#include "OutlinePost.hlsl"

float4 PostProcessPS(FullscreenVSOut pin) : SV_Target
{
    float3 color = gSceneColor.SampleLevel(gLinearSampler, pin.Uv, 0).rgb;
    const int mode = (int)gOutlineParams.z;

    if (mode == 1 || mode == 3)
        color = ApplyHalftone(pin.Uv, color);

    if (mode == 2 || mode == 3)
        color = ApplyOutline(pin.Uv, color);

    return float4(ToDisplayColor(color), 1.0);
}
