cbuffer PassCB : register(b0)
{
    float4x4 gWorld;
    float4x4 gViewProj;
    float4x4 gInvViewProj;
    float4   gEyePosW;
    float4   gRTSize;
};

cbuffer MaterialCB : register(b2)
{
    float4 gBaseColor;
    float4 gSurfaceParams;
};

Texture2D gDiffuseMap : register(t0);
SamplerState gSampler : register(s0);

struct VSIn
{
    float3 PosL : POSITION;
    float3 NormalL : NORMAL;
    float2 TexC : TEXCOORD;
};

struct GeoVSOut
{
    float4 PosH : SV_POSITION;
    float3 PosW : POSITION;
    float3 NormalW : NORMAL;
    float2 TexC : TEXCOORD;
};

GeoVSOut GeometryVS(VSIn vin)
{
    GeoVSOut vout;
    float4 posW = mul(float4(vin.PosL, 1.f), gWorld);
    vout.PosW = posW.xyz;
    vout.NormalW = mul(vin.NormalL, (float3x3)gWorld);
    vout.PosH = mul(posW, gViewProj);
    vout.TexC = vin.TexC;
    return vout;
}

struct GBufferOut
{
    float4 AlbedoSpec : SV_Target0;
    float4 Normal : SV_Target1;
    float Depth : SV_Target2;
};

GBufferOut GeometryPS(GeoVSOut pin)
{
    GBufferOut gout;

    float3 albedo = gDiffuseMap.Sample(gSampler, pin.TexC).rgb * gBaseColor.rgb;
    float specInt = gSurfaceParams.x;
    float shiny = gSurfaceParams.y;

    gout.AlbedoSpec = float4(albedo, specInt);
    gout.Normal = float4(normalize(pin.NormalW), shiny);
    gout.Depth = pin.PosH.z;
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
    float4   gAmbientColor;
    float4   gLightCount;
    GpuLight gLights[MAX_LIGHTS];
};


Texture2D gAlbedoSpecTex : register(t1);
Texture2D gNormalTex : register(t2);
Texture2D gDepthTex : register(t3);


struct QuadVSOut
{
    float4 PosH : SV_POSITION;
    float2 TexC : TEXCOORD;
};

QuadVSOut LightingVS(uint id : SV_VertexID)
{
    QuadVSOut vout;
    vout.TexC = float2((id << 1) & 2, id & 2);
    vout.PosH = float4(vout.TexC * float2(2.f, -2.f) + float2(-1.f, 1.f), 0.f, 1.f);
    return vout;
}

float3 ReconstructWorldPos(float2 uv, float ndcDepth)
{
    float4 clipPos = float4(uv * float2(2.f, -2.f) + float2(-1.f, 1.f),
                            ndcDepth, 1.f);
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
    float  shininess = normalSample.a;
    shininess = max(shininess, 1.f);

    float  ndcDepth = gDepthTex.Load(coords).r;


    if (ndcDepth >= 1.f)
        return float4(0.f, 0.f, 0.f, 1.f);

    float2 uv = pin.PosH.xy * gRTSize.zw;
    float3 posW = ReconstructWorldPos(uv, ndcDepth);
    float3 V = normalize(gEyePosW.xyz - posW);

    float3 finalColor = gAmbientColor.rgb * albedo;

    int lightCount = (int)gLightCount.x;
    for (int i = 0; i < lightCount; ++i)
    {
        GpuLight light = gLights[i];
        float type = light.Params.x;
        float3 lightColor = light.ColorIntensity.rgb;
        float intensity = light.ColorIntensity.a;
        float3 L;
        float attenuation = 1.f;

        if (type < 0.5f)
        {
            L = normalize(-light.DirectionSpot.xyz);
        }
        else if (type < 1.5f)
        {
            float3 toLight = light.PositionRange.xyz - posW;
            float dist = length(toLight);
            float range = light.PositionRange.w;
            if (dist >= range) continue;
            L = toLight / dist;
            float t = dist / range;
            attenuation = saturate(1.f - t * t);
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

            float denom = max(cosInner - cosOuter, 1e-4f);
            float spotFactor = saturate((cosAngle - cosOuter) / denom);
            float t = dist / range;
            attenuation = saturate(1.f - t * t) * spotFactor;
        }

        float NdotL = max(dot(N, L), 0.f);
        float3 H = normalize(L + V);
        float NdotH = max(dot(N, H), 0.f);
        float spec = specInt * pow(NdotH, shininess);

        finalColor += (albedo * NdotL + spec) * lightColor * intensity * attenuation;
    }

    return float4(finalColor, 1.f);
}

// =============================================================================
//  Bulb billboard pass  (forward additive, instanced triangle strip)
// =============================================================================
struct BulbInstance
{
    float3 Position;
    float  Radius;
    float3 Color;
    float  Intensity;
};
StructuredBuffer<BulbInstance> gBulbs : register(t0);

struct BulbVSOut
{
    float4 PosH  : SV_Position;
    float3 Color : COLOR;
    float2 UV    : TEXCOORD0;
};

static const float2 kCorners[4] =
{
    float2(-1.f, +1.f),
    float2(+1.f, +1.f),
    float2(-1.f, -1.f),
    float2(+1.f, -1.f)
};

BulbVSOut BulbVS(uint vid : SV_VertexID, uint iid : SV_InstanceID)
{
    BulbInstance inst = gBulbs[iid];

    float4 clipCenter = mul(float4(inst.Position, 1.f), gViewProj);

    BulbVSOut output;
    if (clipCenter.w <= 0.001f)
    {
        output.PosH  = float4(2.f, 2.f, 2.f, 1.f);
        output.Color = float3(0.f, 0.f, 0.f);
        output.UV    = float2(0.f, 0.f);
        return output;
    }

    float2 corner = kCorners[vid];
    float  scale  = inst.Radius / clipCenter.w;
    clipCenter.x += corner.x * scale * gRTSize.y * gRTSize.z; // aspect correction
    clipCenter.y += corner.y * scale;

    output.PosH  = clipCenter;
    output.Color = inst.Color * inst.Intensity;
    output.UV    = corner;
    return output;
}

float4 BulbPS(BulbVSOut input) : SV_Target
{
    float d2 = dot(input.UV, input.UV);
    if (d2 > 1.f) discard;

    float core = exp(-d2 * 5.f);
    float halo = exp(-d2 * 1.8f) * 0.35f;
    float glow = saturate(core + halo);

    return float4(input.Color * glow, glow);
}
