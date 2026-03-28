cbuffer PassCB : register(b0)
{
    float4x4 gWorld;
    float4x4 gViewProj;
    float4x4 gInvViewProj;
    float4 gEyePosW;
    float4 gRTSize;
    float4 gTessParams;
    float4 gDispParams;
};

cbuffer MaterialCB : register(b2)
{
    float4 gBaseColor;
    float4 gSurfaceParams;
};

Texture2D gDiffuseMap : register(t0);
Texture2D gNormalMap : register(t1);
Texture2D gDisplacementMap : register(t2);
SamplerState gSampler : register(s0);

struct VSIn
{
    float3 PosL : POSITION;
    float3 NormalL : NORMAL;
    float2 TexC : TEXCOORD;
    float3 TangentL : TANGENT;
};

struct VsHsOut
{
    float3 PosW : POSITION;
    float3 NormalW : NORMAL;
    float3 TangentW : TANGENT;
    float2 TexC : TEXCOORD;
};

VsHsOut GeometryVS(VSIn vin)
{
    VsHsOut vout;
    float3x3 w3 = (float3x3)gWorld;
    vout.PosW = mul(float4(vin.PosL, 1.0), gWorld).xyz;
    vout.NormalW = mul(vin.NormalL, w3);
    vout.TangentW= mul(vin.TangentL, w3);
    vout.TexC = vin.TexC;
    return vout;
}

struct HsConstData
{
    float Edges[3] : SV_TessFactor;
    float Inside : SV_InsideTessFactor;
};

struct HsCpOut
{
    float3 PosW : POSITION;
    float3 NormalW : NORMAL;
    float3 TangentW : TANGENT;
    float2 TexC : TEXCOORD;
};

float CalcEdgeTess(float3 p0, float3 p1)
{
    float dist = distance(0.5 * (p0 + p1), gEyePosW.xyz);
    float t = saturate((dist - gTessParams.z) / max(gTessParams.w - gTessParams.z, 1e-4));
    return lerp(gTessParams.y, gTessParams.x, t);
}

HsConstData HsPatchConst(InputPatch<VsHsOut, 3> p, uint pid : SV_PrimitiveID)
{
    HsConstData d;
    d.Edges[0] = CalcEdgeTess(p[1].PosW, p[2].PosW);
    d.Edges[1] = CalcEdgeTess(p[2].PosW, p[0].PosW);
    d.Edges[2] = CalcEdgeTess(p[0].PosW, p[1].PosW);
    d.Inside = (d.Edges[0] + d.Edges[1] + d.Edges[2]) / 3.0;
    return d;
}

[domain("tri")]
[partitioning("fractional_odd")]
[outputtopology("triangle_cw")]
[outputcontrolpoints(3)]
[patchconstantfunc("HsPatchConst")]
[maxtessfactor(8.0)]
HsCpOut GeometryHS(InputPatch<VsHsOut, 3> p, uint cpId : SV_OutputControlPointID)
{
    HsCpOut cp;
    cp.PosW = p[cpId].PosW;
    cp.NormalW = p[cpId].NormalW;
    cp.TangentW= p[cpId].TangentW;
    cp.TexC = p[cpId].TexC;
    return cp;
}

struct DsOut
{
    float4 PosH : SV_POSITION;
    float3 PosW : POSITION;
    float3 NormalW : NORMAL;
    float3 TangentW : TANGENT;
    float2 TexC : TEXCOORD;
};

[domain("tri")]
DsOut GeometryDS(
    HsConstData hsd,
    float3 bary : SV_DomainLocation,
    const OutputPatch<HsCpOut, 3> patch)
{
    DsOut dout;

    float3 posW = bary.x*patch[0].PosW + bary.y*patch[1].PosW + bary.z*patch[2].PosW;
    float3 normalW = bary.x*patch[0].NormalW + bary.y*patch[1].NormalW + bary.z*patch[2].NormalW;
    float3 tangentW= bary.x*patch[0].TangentW+ bary.y*patch[1].TangentW+ bary.z*patch[2].TangentW;
    float2 texC = bary.x*patch[0].TexC   + bary.y*patch[1].TexC   + bary.z*patch[2].TexC;

    normalW = normalize(normalW);
    tangentW = normalize(tangentW);

    int mode = (int)gDispParams.z;
    if (mode == 2 || mode == 3)
    {
        float disp = gDisplacementMap.SampleLevel(gSampler, texC, 0).r;
        disp = max(0.0, disp * gDispParams.x + gDispParams.y);
        posW += normalW * disp;
    }

    dout.PosH = mul(float4(posW, 1.0), gViewProj);
    dout.PosW = posW;
    dout.NormalW = normalW;
    dout.TangentW= tangentW;
    dout.TexC = texC;
    return dout;
}

struct GBufferOut
{
    float4 AlbedoSpec : SV_Target0;
    float4 Normal : SV_Target1;
};

GBufferOut GeometryPS(DsOut pin)
{
    GBufferOut gout;

    float3 albedo = gDiffuseMap.Sample(gSampler, pin.TexC).rgb * gBaseColor.rgb;
    gout.AlbedoSpec = float4(albedo, gSurfaceParams.x);

    int mode = (int)gDispParams.z;

    float3 finalNormal;
    if (mode == 1 || mode == 3)
    {
        float3 N = normalize(pin.NormalW);
        float3 T = normalize(pin.TangentW - dot(pin.TangentW, N) * N);
        float3 B = cross(N, T);
        float3x3 TBN = float3x3(T, B, N);
        float3 normalTS = gNormalMap.Sample(gSampler, pin.TexC).rgb * 2.0 - 1.0;
        finalNormal = normalize(mul(normalTS, TBN));
    }
    else
    {
        finalNormal = normalize(pin.NormalW);
    }

    gout.Normal = float4(finalNormal, gSurfaceParams.y);
    return gout;
}

struct GpuLight
{
    float4 PositionRange;
    float4 DirectionSpot;
    float4 ColorIntensity;
    float4 Params;
};

#define MAX_LIGHTS 128

cbuffer LightCB : register(b1)
{
    float4 gAmbientColor;
    float4 gLightCount;
    GpuLight gLights[MAX_LIGHTS];
};

Texture2D gAlbedoSpecTex : register(t3);
Texture2D gNormalTex : register(t4);
Texture2D gHwDepthTex : register(t5);

struct QuadVSOut
{
    float4 PosH : SV_POSITION;
    float2 TexC : TEXCOORD;
};

QuadVSOut LightingVS(uint id : SV_VertexID)
{
    QuadVSOut vout;
    vout.TexC = float2((id << 1) & 2, id & 2);
    vout.PosH = float4(vout.TexC * float2(2.0, -2.0) + float2(-1.0, 1.0), 0.0, 1.0);
    return vout;
}

float3 ReconstructWorldPos(float2 uv, float ndcDepth)
{
    float4 clipPos = float4(uv * float2(2.0, -2.0) + float2(-1.0, 1.0), ndcDepth, 1.0);
    float4 worldPos = mul(clipPos, gInvViewProj);
    return worldPos.xyz / worldPos.w;
}

float4 LightingPS(QuadVSOut pin) : SV_TARGET
{
    int3 coords = int3((int2)pin.PosH.xy, 0);

    float4 albedoSpec = gAlbedoSpecTex.Load(coords);
    float3 albedo = albedoSpec.rgb;
    float  specInt = albedoSpec.a;
    float4 normalSample = gNormalTex.Load(coords);
    float3 N = normalize(normalSample.xyz);
    float  shininess = max(normalSample.a, 1.0);
    float  ndcDepth = gHwDepthTex.Load(coords).r;

    if (ndcDepth >= 1.0)
        return float4(0.0, 0.0, 0.0, 1.0);

    float2 uv = pin.PosH.xy * gRTSize.zw;
    float3 posW = ReconstructWorldPos(uv, ndcDepth);
    float3 V = normalize(gEyePosW.xyz - posW);

    float3 finalColor = gAmbientColor.rgb * albedo;

    int lightCount = (int)gLightCount.x;
    for (int i = 0; i < lightCount; ++i)
    {
        GpuLight light = gLights[i];
        float type = light.Params.x;
        float3 lColor = light.ColorIntensity.rgb;
        float intensity = light.ColorIntensity.a;
        float3 L;
        float attenuation = 1.0;

        if (type < 0.5)
        {
            L = normalize(-light.DirectionSpot.xyz);
        }
        else if (type < 1.5)
        {
            float3 toLight = light.PositionRange.xyz - posW;
            float dist = length(toLight);
            float range = light.PositionRange.w;
            if (dist >= range) continue;
            L = toLight / dist;
            float t = dist / range;
            attenuation = saturate(1.0 - t * t);
        }
        else
        {
            float3 toLight = light.PositionRange.xyz - posW;
            float dist = length(toLight);
            float range = light.PositionRange.w;
            if (dist >= range) continue;
            L = toLight / dist;
            float cosOuter = light.DirectionSpot.w;
            float cosInner = light.Params.y;
            float cosAngle = dot(-L, normalize(light.DirectionSpot.xyz));
            if (cosAngle <= cosOuter) continue;
            float spotFactor = saturate((cosAngle - cosOuter) / max(cosInner - cosOuter, 1e-4));
            float t = dist / range;
            attenuation = saturate(1.0 - t * t) * spotFactor;
        }

        float  NdotL = max(dot(N, L), 0.0);
        float3 H = normalize(L + V);
        float  spec = specInt * pow(max(dot(N, H), 0.0), shininess);
        finalColor  += (albedo * NdotL + spec) * lColor * intensity * attenuation;
    }

    return float4(finalColor, 1.0);
}
