cbuffer SceneCB : register(b0)
{
    row_major float4x4 gViewProj;
    row_major float4x4 gView;
    row_major float4x4 gLightViewProj[4];
    float4             gCascadeFar;
    float4             gShadowParams;
    float4             gEyePos;
};

cbuffer ObjectCB : register(b1)
{
    row_major float4x4 gWorld;
    uint               gShadowCascadeIndex;
};

Texture2D    gDiffuse : register(t0);
Texture2DArray gShadowMaps : register(t1);
SamplerState gSampler : register(s0);
SamplerComparisonState gShadowSampler : register(s1);

#include "ShadowCommon.hlsl"

struct VSIn
{
    float3 Pos     : POSITION;
    float3 Normal  : NORMAL;
    float2 TexC    : TEXCOORD;
    float3 Tangent : TANGENT;
};

struct VSOut
{
    float4 PosH    : SV_POSITION;
    float3 PosW    : POSITION;
    float3 PosV    : TEXCOORD1;
    float3 NormalW : NORMAL;
    float2 TexC    : TEXCOORD;
};

VSOut VS(VSIn vin)
{
    VSOut vout;
    float4 posW  = mul(float4(vin.Pos, 1.0), gWorld);
    vout.PosH    = mul(posW, gViewProj);
    vout.PosW    = posW.xyz;
    vout.PosV    = mul(posW, gView).xyz;
    vout.NormalW = normalize(mul(vin.Normal, (float3x3)gWorld));
    vout.TexC    = vin.TexC;
    return vout;
}

float4 ShadowVS(VSIn vin) : SV_POSITION
{
    const float4 posW = mul(float4(vin.Pos, 1.0), gWorld);
    return mul(posW, gLightViewProj[gShadowCascadeIndex]);
}

float4 PS(VSOut pin) : SV_Target
{
    float3 albedo = gDiffuse.Sample(gSampler, pin.TexC).rgb;

    float3 N = normalize(pin.NormalW);
    float3 L = normalize(float3(0.5, 1.0, 0.4));
    float3 V = normalize(gEyePos.xyz - pin.PosW);
    float3 H = normalize(L + V);

    float diff = saturate(dot(N, L));
    float spec = pow(saturate(dot(N, H)), 32.0) * 0.15;
    float shadow = SampleCsmShadowPcf(pin.PosW, max(pin.PosV.z, 0.0), N, L);

    return float4(albedo * (0.15 + diff * 0.85 * shadow) + spec * shadow, 1.0);
}
