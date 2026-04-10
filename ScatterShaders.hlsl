cbuffer SceneCB : register(b0)
{
    row_major float4x4 gViewProj;
    float4             gEyePos;
};

cbuffer ObjectCB : register(b1)
{
    row_major float4x4 gWorld;
};

Texture2D    gDiffuse : register(t0);
SamplerState gSampler : register(s0);

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
    float3 NormalW : NORMAL;
    float2 TexC    : TEXCOORD;
};

VSOut VS(VSIn vin)
{
    VSOut vout;
    float4 posW  = mul(float4(vin.Pos, 1.0), gWorld);
    vout.PosH    = mul(posW, gViewProj);
    vout.PosW    = posW.xyz;
    vout.NormalW = normalize(mul(vin.Normal, (float3x3)gWorld));
    vout.TexC    = vin.TexC;
    return vout;
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

    return float4(albedo * (0.15 + diff * 0.85) + spec, 1.0);
}
