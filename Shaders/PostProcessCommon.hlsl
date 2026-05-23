cbuffer PostProcessCB : register(b0)
{
    float4 gRenderTargetSize;
    float4 gHalftoneParams;
    float4 gOutlineParams;
    float4 gOutlineColor;
    float4 gColorParams;
};

Texture2D gSceneColor : register(t0);
SamplerState gLinearSampler : register(s0);
SamplerState gPointSampler : register(s1);

static const float3 kLumaWeights = float3(0.2126f, 0.7152f, 0.0722f);

struct FullscreenVSOut
{
    float4 PosH : SV_POSITION;
    float2 Uv : TEXCOORD0;
};

FullscreenVSOut FullscreenVS(uint vertexId : SV_VertexID)
{
    FullscreenVSOut vout;
    vout.Uv = float2((vertexId << 1) & 2, vertexId & 2);
    vout.PosH = float4(vout.Uv * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);
    return vout;
}

float3 ToneMapACES(float3 color)
{
    color = max(color, 0.0f) * gColorParams.x;
    const float a = 2.51f;
    const float b = 0.03f;
    const float c = 2.43f;
    const float d = 0.59f;
    const float e = 0.14f;
    return saturate((color * (a * color + b)) / (color * (c * color + d) + e));
}

float3 LinearToSrgb(float3 color)
{
    return pow(saturate(color), 1.0f / max(gColorParams.y, 0.0001f));
}

float3 ApplySimpleGrade(float3 color)
{
    color = color + (1.0f - color) * 0.012f;
    const float luma = dot(color, kLumaWeights);
    color = lerp(float3(luma, luma, luma), color, gColorParams.z);
    color = (color - 0.5f) * gColorParams.w + 0.5f;
    return saturate(color);
}

float AnalysisLuma(float3 hdrColor)
{
    return dot(ToneMapACES(hdrColor), kLumaWeights);
}

float3 ToDisplayColor(float3 sceneColor)
{
    const int colorMode = (int)gOutlineParams.w;
    const float3 linearColor = max(sceneColor, 0.0f);

    if (colorMode == 1)
        return ToneMapACES(linearColor);

    if (colorMode == 2)
        return LinearToSrgb(linearColor);

    if (colorMode == 3)
    {
        float3 color = ToneMapACES(linearColor);
        color = ApplySimpleGrade(color);
        return LinearToSrgb(color);
    }

    return saturate(linearColor);
}
