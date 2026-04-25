cbuffer SceneCB : register(b0)
{
    row_major float4x4 gViewProj;
    float4 gCameraRight;
    float4 gCameraUp;
    float4 gCameraFacing;
    float4 gLightDirection;
    float4 gLightColor;
    float4 gAmbientColor;
};

cbuffer DrawCB : register(b1)
{
    row_major float4x4 gWorld;
    float4 gBaseColor;
    float gCheckerTileSize;
    float gIsFloor;
    float2 gDrawPadding;
};

cbuffer UpdateCB : register(b2)
{
    float gDeltaTime;
    float gTotalTime;
    uint gEmitCount;
    uint gMaxParticles;

    float3 gEmitterPosition;
    float gSpawnRadius;

    float3 gInitialVelocity;
    float gVelocityJitter;

    float3 gGravity;
    float gBaseSize;
};

struct MeshVSIn
{
    float3 Pos     : POSITION;
    float3 Normal  : NORMAL;
    float2 TexC    : TEXCOORD;
    float3 Tangent : TANGENT;
};

struct MeshVSOut
{
    float4 PosH    : SV_POSITION;
    float3 PosW    : TEXCOORD0;
    float3 NormalW : NORMAL;
};

MeshVSOut MeshVS(MeshVSIn vin)
{
    MeshVSOut vout;
    float4 posW = mul(float4(vin.Pos, 1.0), gWorld);
    vout.PosH = mul(posW, gViewProj);
    vout.PosW = posW.xyz;
    vout.NormalW = normalize(mul(vin.Normal, (float3x3)gWorld));
    return vout;
}

float4 MeshPS(MeshVSOut pin) : SV_Target
{
    const float3 N = normalize(pin.NormalW);
    const float3 L = normalize(-gLightDirection.xyz);
    const float diffuse = saturate(dot(N, L));
    const float skyBounce = saturate(N.y * 0.5 + 0.5);

    float3 baseColor = gBaseColor.rgb;
    if (gIsFloor > 0.5)
    {
        const int2 tileCoord = int2(floor(pin.PosW.xz / max(gCheckerTileSize, 0.001)));
        const bool isEvenTile = (((tileCoord.x + tileCoord.y) & 1) == 0);
        const float3 darkLettuce = float3(0.34, 0.46, 0.16);
        const float3 darkBrown = float3(0.40, 0.27, 0.16);
        baseColor = isEvenTile ? darkLettuce : darkBrown;
    }

    float3 litColor = baseColor * (gAmbientColor.rgb + gLightColor.rgb * diffuse);
    litColor += baseColor * (0.08 * skyBounce);
    return float4(saturate(litColor), 1.0);
}

struct Particle
{
    float3 Position;
    float Age;
    float3 Velocity;
    float Life;
    float4 Color;
    float Size;
    float3 Padding;
};

StructuredBuffer<Particle> gParticles : register(t0);
StructuredBuffer<uint> gLiveCount : register(t1);

ConsumeStructuredBuffer<Particle> gCurrentParticles : register(u0);
AppendStructuredBuffer<Particle> gNextParticles : register(u1);

static const float kGroundY = 0.0;
static const float kBounceVelocityScale = 0.34;
static const float kGroundFriction = 0.78;
static const float kSimulationTimeScale = 3.0;
static const float kParticleLifeMin = 4.8;
static const float kParticleLifeMax = 6.2;
static const float kSpawnHeightJitter = 0.15;
static const float kHorizontalImpulseMin = 1.8;
static const float kHorizontalImpulseMax = 3.8;
static const float kVerticalImpulseMin = 0.35;
static const float kVerticalImpulseMax = 1.3;

uint Hash(uint value)
{
    value ^= value >> 16;
    value *= 0x7feb352du;
    value ^= value >> 15;
    value *= 0x846ca68bu;
    value ^= value >> 16;
    return value;
}

float HashFloat(uint value)
{
    return (Hash(value) & 0x00FFFFFFu) / 16777216.0;
}

struct ParticleVsOut
{
    float3 Center   : POSITION;
    float3 Velocity : TEXCOORD1;
    float2 Size     : TEXCOORD2;
    float4 Color    : COLOR0;
    float Alive     : TEXCOORD0;
};

ParticleVsOut ParticleVS(uint vertexId : SV_VertexID)
{
    ParticleVsOut vout = (ParticleVsOut)0;
    const uint liveCount = gLiveCount[0];
    if (vertexId >= liveCount)
        return vout;

    const Particle particle = gParticles[vertexId];
    const float lifeT = saturate(particle.Age / max(particle.Life, 0.001));
    const float speed = length(particle.Velocity);

    vout.Center = particle.Position;
    vout.Velocity = particle.Velocity;
    vout.Size = float2(
        particle.Size * lerp(1.0, 0.45, lifeT),
        particle.Size * (2.6 + speed * 0.22) * lerp(1.0, 0.7, lifeT));
    vout.Color = float4(particle.Color.rgb * lerp(1.0, 0.82, lifeT), 1.0);
    vout.Alive = 1.0;
    return vout;
}

struct ParticleGsOut
{
    float4 PosH : SV_POSITION;
    float2 Uv : TEXCOORD0;
    float4 Color : COLOR0;
};

[maxvertexcount(4)]
void ParticleGS(point ParticleVsOut input[1], inout TriangleStream<ParticleGsOut> triStream)
{
    if (input[0].Alive < 0.5 || input[0].Size.x <= 0.0 || input[0].Size.y <= 0.0)
        return;

    const float3 center = input[0].Center;
    float3 sparkAxis = input[0].Velocity;
    sparkAxis -= gCameraFacing.xyz * dot(sparkAxis, gCameraFacing.xyz);
    if (dot(sparkAxis, sparkAxis) < 1e-5)
        sparkAxis = gCameraUp.xyz;
    sparkAxis = normalize(sparkAxis);

    float3 side = cross(gCameraFacing.xyz, sparkAxis);
    if (dot(side, side) < 1e-5)
        side = gCameraRight.xyz;
    side = normalize(side);

    const float halfWidth = input[0].Size.x * 0.5;
    const float halfLength = input[0].Size.y * 0.5;

    const float3 positions[4] =
    {
        center - side * halfWidth - sparkAxis * halfLength,
        center - side * halfWidth + sparkAxis * halfLength,
        center + side * halfWidth - sparkAxis * halfLength,
        center + side * halfWidth + sparkAxis * halfLength
    };

    const float2 uvs[4] =
    {
        float2(0.0, 0.0),
        float2(0.0, 1.0),
        float2(1.0, 0.0),
        float2(1.0, 1.0)
    };

    [unroll]
    for (uint i = 0; i < 4; ++i)
    {
        ParticleGsOut gout;
        gout.PosH = mul(float4(positions[i], 1.0), gViewProj);
        gout.Uv = uvs[i];
        gout.Color = input[0].Color;
        triStream.Append(gout);
    }

    triStream.RestartStrip();
}

float4 ParticlePS(ParticleGsOut pin) : SV_Target
{
    const float localX = pin.Uv.x * 2.0 - 1.0;
    const float halfWidth = lerp(0.92, 0.12, pin.Uv.y);
    clip(halfWidth - abs(localX));
    clip(pin.Uv.y - 0.02);

    const float brightness = 0.70 + 0.30 * pin.Uv.y;
    float3 litColor = pin.Color.rgb * brightness;
    litColor *= gAmbientColor.rgb + gLightColor.rgb * 0.75;
    return float4(saturate(litColor), 1.0);
}

[numthreads(256, 1, 1)]
void UpdateParticlesCS(uint dispatchId : SV_DispatchThreadID)
{
    const float simDt = gDeltaTime * kSimulationTimeScale;
    const uint liveCount = gLiveCount[0];
    const uint freeSlots = (liveCount < gMaxParticles) ? (gMaxParticles - liveCount) : 0u;
    const uint spawnCount = min(gEmitCount, freeSlots);

    if (dispatchId < liveCount)
    {
        Particle particle = gCurrentParticles.Consume();
        particle.Age += simDt;

        if (particle.Age < particle.Life)
        {
            particle.Velocity += gGravity * simDt;
            particle.Position += particle.Velocity * simDt;
            bool keepParticle = true;

            if (particle.Position.y <= kGroundY)
            {
                if (particle.Padding.x < 0.5 && particle.Velocity.y < 0.0)
                {
                    particle.Position.y = kGroundY;
                    particle.Velocity.x *= kGroundFriction;
                    particle.Velocity.z *= kGroundFriction;
                    particle.Velocity.y = abs(particle.Velocity.y) * kBounceVelocityScale;
                    particle.Padding.x = 1.0;
                }
                else
                {
                    keepParticle = false;
                }
            }

            if (keepParticle)
                gNextParticles.Append(particle);
        }
    }

    if (dispatchId >= spawnCount)
        return;

    const uint seed = Hash(dispatchId + asuint(gTotalTime * 4096.0));
    const float rnd0 = HashFloat(seed);
    const float rnd1 = HashFloat(seed ^ 0x9E3779B9u);
    const float rnd2 = HashFloat(seed ^ 0x85EBCA6Bu);

    const float angle = rnd0 * 6.28318530718;
    const float radius = sqrt(rnd1) * gSpawnRadius;
    const float3 radial = float3(cos(angle), 0.0, sin(angle));

    Particle particle;
    particle.Position = gEmitterPosition + radial * radius + float3(0.0, rnd2 * kSpawnHeightJitter, 0.0);
    particle.Age = 0.0;
    particle.Velocity = gInitialVelocity
        + radial * lerp(kHorizontalImpulseMin, kHorizontalImpulseMax, rnd2) * gVelocityJitter
        + float3(0.0, lerp(kVerticalImpulseMin, kVerticalImpulseMax, rnd1) * gVelocityJitter, 0.0);
    particle.Life = lerp(kParticleLifeMin, kParticleLifeMax, rnd1);
    particle.Color = float4(
        lerp(float3(0.74, 0.70, 0.62), float3(0.98, 0.93, 0.82), rnd2),
        1.0);
    particle.Size = gBaseSize * lerp(0.85, 1.20, rnd0);
    particle.Padding = float3(0.0, 0.0, 0.0);

    gNextParticles.Append(particle);
}
