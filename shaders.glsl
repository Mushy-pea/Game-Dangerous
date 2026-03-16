// Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
// If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

// Vertex shader (program 0, two light map plus up to four programmable point lighted object, no texture)
#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 colour;
layout(location = 2) in vec3 normal;

out vec3 modelInWorldPosition;
out vec4 diffColour;
flat out vec3 vertNormal;
flat out vec4 lightIntensities[6];
flat out vec3 worldLightPositions[6];

uniform mat4 mod_to_world;
uniform mat4 world_to_mod;
uniform mat4 world_to_clip;
uniform vec3 lmap_pos0[100];
uniform vec3 lmap_pos1[100];
uniform vec3 lmap_int0[100];
uniform vec3 lmap_int1[100];
uniform vec4 mobileLightIntensities[4];
uniform vec3 mobileLightPositions[4];

void main() {
vec4 worldPos = mod_to_world * position;
modelInWorldPosition = worldPos.xyz;
diffColour = colour;
vertNormal = normal;

int look_up = int(worldPos.x / 10) + int(worldPos.y / 10) * 10;
vec4 staticLightIntensity0 = vec4(lmap_int0[look_up], 1);
vec4 staticLightIntensity1 = vec4(lmap_int1[look_up], 1);
vec3 staticLightPosition0 = lmap_pos0[look_up];
vec3 staticLightPosition1 = lmap_pos1[look_up];
lightIntensities[0] = staticLightIntensity0;
lightIntensities[1] = staticLightIntensity1;
lightIntensities[2] = mobileLightIntensities[0];
lightIntensities[3] = mobileLightIntensities[1];
lightIntensities[4] = mobileLightIntensities[2];
lightIntensities[5] = mobileLightIntensities[3];
worldLightPositions[0] = staticLightPosition0;
worldLightPositions[1] = staticLightPosition1;
worldLightPositions[2] = mobileLightPositions[0];
worldLightPositions[3] = mobileLightPositions[1];
worldLightPositions[4] = mobileLightPositions[2];
worldLightPositions[5] = mobileLightPositions[3];

gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 0, two light map plus up to four programmable point lighted object, no texture)
#version 330

in vec3 modelInWorldPosition;
in vec4 diffColour;
flat in vec3 vertNormal;
flat in vec4 lightIntensities[6];
flat in vec3 worldLightPositions[6];

out vec4 outputColour;

uniform float lmap_t0[240];
uniform float lmap_t1[240];
uniform int t;
uniform int numLights;

void main() {
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);
float distanceSqr;
vec3 lightDifference; vec3 lightDir;
float cosAngleIncidence[6];
float attenuation[6];

for (int n = 0; n < numLights; n++)
{
  lightDifference = modelInWorldPosition - worldLightPositions[n];
  distanceSqr = dot(lightDifference, lightDifference);
  lightDir = lightDifference * inversesqrt(distanceSqr);
  attenuation[n] = 1 / distanceSqr;
  cosAngleIncidence[n] = clamp(0.15, 1, dot(vertNormal, lightDir));
}

vec4 totalLight = (attenuation[0] * lmap_t0[t] * cosAngleIncidence[0] * lightIntensities[0] * diffColour) + (attenuation[1] * lmap_t1[t] * cosAngleIncidence[1] * lightIntensities[1] * diffColour);

for (int n = 2; n < numLights; n++)
{
  totalLight = totalLight + (attenuation[n] * cosAngleIncidence[n] * lightIntensities[n] * diffColour);
}

outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 1, two light map plus up to four programmable point lighted object, with texture)
#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoord;
layout(location = 2) in vec3 normal;

out vec3 modelInWorldPosition;
out vec2 tex_coord;
flat out vec3 vertNormal;
flat out vec4 lightIntensities[6];
flat out vec3 worldLightPositions[6];

uniform mat4 mod_to_world;
uniform mat4 world_to_clip;
uniform vec3 lmap_pos0[100];
uniform vec3 lmap_pos1[100];
uniform vec3 lmap_int0[100];
uniform vec3 lmap_int1[100];
uniform vec4 mobileLightIntensities[4];
uniform vec3 mobileLightPositions[4];

void main() {
vec4 worldPos = mod_to_world * position;
modelInWorldPosition = worldPos.xyz;
tex_coord = texCoord;
vertNormal = normal;

int look_up = int(worldPos.x / 10) + int(worldPos.y / 10) * 10;
vec4 staticLightIntensity0 = vec4(lmap_int0[look_up], 1);
vec4 staticLightIntensity1 = vec4(lmap_int1[look_up], 1);
vec3 staticLightPosition0 = lmap_pos0[look_up];
vec3 staticLightPosition1 = lmap_pos1[look_up];
lightIntensities[0] = staticLightIntensity0;
lightIntensities[1] = staticLightIntensity1;
lightIntensities[2] = mobileLightIntensities[0];
lightIntensities[3] = mobileLightIntensities[1];
lightIntensities[4] = mobileLightIntensities[2];
lightIntensities[5] = mobileLightIntensities[3];
worldLightPositions[0] = staticLightPosition0;
worldLightPositions[1] = staticLightPosition1;
worldLightPositions[2] = mobileLightPositions[0];
worldLightPositions[3] = mobileLightPositions[1];
worldLightPositions[4] = mobileLightPositions[2];
worldLightPositions[5] = mobileLightPositions[3];

gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 1, two light map plus up to four programmable point lighted object, with texture)
#version 330

in vec3 modelInWorldPosition;
in vec2 tex_coord;
flat in vec3 vertNormal;
flat in vec4 lightIntensities[6];
flat in vec3 worldLightPositions[6];

out vec4 outputColour;

uniform sampler2D tex_unit0;
uniform float lmap_t0[240];
uniform float lmap_t1[240];
uniform int t;
uniform int numLights;

void main() {
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);
float distanceSqr;
vec3 lightDifference; vec3 lightDir;
float cosAngleIncidence[6];
float attenuation[6];

for (int n = 0; n < numLights; n++)
{
  lightDifference = modelInWorldPosition - worldLightPositions[n];
  distanceSqr = dot(lightDifference, lightDifference);
  lightDir = lightDifference * inversesqrt(distanceSqr);
  attenuation[n] = 1 / distanceSqr;
  cosAngleIncidence[n] = clamp(0.15, 1, dot(vertNormal, lightDir));
}

vec4 diffColour = texture(tex_unit0, tex_coord);
vec4 totalLight = (attenuation[0] * lmap_t0[t] * cosAngleIncidence[0] * lightIntensities[0] * diffColour) + (attenuation[1] * lmap_t1[t] * cosAngleIncidence[1] * lightIntensities[1] * diffColour);

for (int n = 2; n < numLights; n++)
{
  totalLight = totalLight + (attenuation[n] * cosAngleIncidence[n] * lightIntensities[n] * diffColour);
}

outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 2, player torch plus up to four programmable point lighted object, no texture)
#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 colour;
layout(location = 2) in vec3 normal;

out vec3 modelInWorldPosition;
out vec4 diffColour;
flat out vec3 vertNormal;

uniform mat4 mod_to_world;
uniform mat4 world_to_mod;
uniform mat4 world_to_clip;

void main() {
vec4 worldPos = mod_to_world * position;
modelInWorldPosition = worldPos.xyz;
diffColour = colour;
vertNormal = normal;
gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 2, player torch plus up to four programmable point lighted object, no texture)
#version 330

in vec3 modelInWorldPosition;
in vec4 diffColour;
flat in vec3 vertNormal;

out vec4 outputColour;

uniform vec4 mobileLightIntensities[5];
uniform vec3 mobileLightPositions[5];
uniform int timer;
uniform int numLights;

void main() {
float adjust;
float distanceSqr;
vec3 lightDifference;
vec3 lightDir;
float cosAngleIncidence[5];
float attenuation[5];
if (timer > 0)
  adjust = 1;
else
  adjust = 0;
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);

for (int n = 0; n < numLights; n++)
{
  lightDifference = modelInWorldPosition - mobileLightPositions[n];
  distanceSqr = dot(lightDifference, lightDifference);
  lightDir = lightDifference * inversesqrt(distanceSqr);
  attenuation[n] = 1 / distanceSqr;
  cosAngleIncidence[n] = clamp(0.15, 1, dot(vertNormal, lightDir));
}

vec4 totalLight = attenuation[0] * adjust * cosAngleIncidence[0] * mobileLightIntensities[0] * diffColour;

for (int n = 1; n < numLights; n++)
{
  totalLight = totalLight + (attenuation[n] * cosAngleIncidence[n] * mobileLightIntensities[n] * diffColour);
}

outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 3, player torch plus up to four programmable point lighted object, with texture)
#version 420

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoord;
layout(location = 2) in vec3 normal;
layout(binding = 3) uniform WallShadowMap0
{
  bvec4 wallShadow0[2500];
};
layout(binding = 4) uniform WallShadowMap1
{
  bvec4 wallShadow1[2500];
};
layout(binding = 5) uniform WallShadowMap2
{
  bvec4 wallShadow2[2500];
};
layout(binding = 6) uniform FloorShadowMap0
{
  bool floorShadow0[2500];
};
layout(binding = 7) uniform FloorShadowMap1
{
  bool floorShadow1[2500];
};
layout(binding = 8) uniform FloorShadowMap2
{
  bool floorShadow2[2500];
};

out vec3 modelInWorldPosition;
out vec2 tex_coord;
flat out vec3 vertNormal;
out float shadowScaling[5];

uniform mat4 mod_to_world;
uniform mat4 world_to_clip;
uniform vec3 mobileLightPositions[5];
uniform int numLights;

void intersectSequence(const vec3 lightPos, const vec3 lightDifference, inout float intersection[75]) {
  const float distanceSqr = dot(lightDifference, lightDifference);
  const vec3 lightDir = lightDifference * inversesqrt(distanceSqr);
  float lightDirX; float lightDirY; float lightDirZ;
  if (lightDir.x == 0.0) {
    lightDirX = 0.000001;
  }
  else {
    lightDirX = lightDir.x;
  }
  if (lightDir.y == 0.0) {
    lightDirY = 0.000001;
  }
  else {
    lightDirY = lightDir.y;
  }
  if (lightDir.z == 0.0) {
    lightDirZ = 0.000001;
  }
  else {
    lightDirZ = lightDir.z;
  }
  const float lightDirSignX = sign(lightDirX);
  const float lightDirSignY = sign(lightDirY);
  const float lightDirSignZ = sign(lightDirZ);
  const float lightDirSignXC = clamp(lightDirSignX, 0.0, 1.0);
  const float lightDirSignYC = clamp(lightDirSignY, 0.0, 1.0);
  const float lightDirSignZC = clamp(lightDirSignZ, 0.0, 1.0);
  intersection[0] = (lightDirSignXC - lightDirSignX * mod(lightPos.x, 1.0)) / lightDirX;
  intersection[1] = (lightDirSignYC - lightDirSignY * mod(lightPos.y, 1.0)) / lightDirY;
  intersection[2] = (lightDirSignZC - lightDirSignZ * mod(lightPos.z, 1.0)) / lightDirZ;

  for (int n = 3; n < 75; n = n + 3) {
    intersection[n] = intersection[n - 3] + 1.0 / lightDirX;
    intersection[n + 1] = intersection[n - 2] + 1.0 / lightDirY;
    intersection[n + 2] = intersection[n - 1] + 1.0 / lightDirZ;
  }
}

void main() {
vec4 worldPos = mod_to_world * position;
modelInWorldPosition = worldPos.xyz;
tex_coord = texCoord;
vertNormal = normal;
float shadowEffect[5] = float[5](1, 1, 1, 1, 1);
const int vertexU = int(clamp(modelInWorldPosition.x, 0.0, 49.9));
const int vertexV = int(clamp(modelInWorldPosition.y, 0.0, 49.9));

for (int m = 0; m < numLights; m++) {
  const vec3 lightDifference = modelInWorldPosition - mobileLightPositions[m];
  float intersection[75];
  intersectSequence(mobileLightPositions[m], lightDifference, intersection);
  int u = int(clamp(mobileLightPositions[m].x, 0.0, 49.9));
  int v = int(clamp(mobileLightPositions[m].y, 0.0, 49.9));
  int w = int(clamp(mobileLightPositions[m].z, 0.0, 2.9));
  for (int n = 0; n < 75; n = n + 3) {
    bool u1Sample;
    bool u2Sample;
    bool v1Sample;
    bool v2Sample;
    if (u == vertexU && v == vertexV) {
      break;
    }
    if (w == 0) {
      u1Sample = wallShadow0[u * 50 + v].x;
      u2Sample = wallShadow0[u * 50 + v].y;
      v1Sample = wallShadow0[u * 50 + v].z;
      v2Sample = wallShadow0[u * 50 + v].w;
    }
    else if (w == 1) {
      u1Sample = wallShadow1[u * 50 + v].x;
      u2Sample = wallShadow1[u * 50 + v].y;
      v1Sample = wallShadow1[u * 50 + v].z;
      v2Sample = wallShadow1[u * 50 + v].w;
    }
    else {
      u1Sample = wallShadow2[u * 50 + v].x;
      u2Sample = wallShadow2[u * 50 + v].y;
      v1Sample = wallShadow2[u * 50 + v].z;
      v2Sample = wallShadow2[u * 50 + v].w;
    }

    if (intersection[n] < 0) {
      if (u1Sample) {
        shadowEffect[m] = 0.0;
        break;
      }
      else {
        u--;
      }
    }
    else {
      if (u2Sample) {
        shadowEffect[m] = 0.0;
        break;
      }
      else {
        u++;
      }
    }

    if (intersection[n + 1] < 0) {
      if (v1Sample) {
        shadowEffect[m] = 0.0;
        break;
      }
      else {
        v--;
      }
    }
    else {
      if (v2Sample) {
        shadowEffect[m] = 0.0;
        break;
      }
      else {
        v++;
      }
    }
  }
}

shadowScaling[0] = shadowEffect[0];
shadowScaling[1] = shadowEffect[1];
shadowScaling[2] = shadowEffect[2];
shadowScaling[3] = shadowEffect[3];
shadowScaling[4] = shadowEffect[4];
gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 3, player torch plus up to four programmable point lighted object, with texture)
#version 420

in vec3 modelInWorldPosition;
in vec2 tex_coord;
flat in vec3 vertNormal;
in float shadowScaling[5];

out vec4 outputColour;

uniform sampler2D tex_unit0;
uniform vec4 mobileLightIntensities[5];
uniform vec3 mobileLightPositions[5];
uniform int timer;
uniform int numLights;

void main() {
float adjust; float distanceSqr;
vec3 lightDifference; vec3 lightDir;
float cosAngleIncidence[5]; float attenuation[5] = float[5](0, 0, 0, 0, 0);
if (timer > 0)
  adjust = 1;
else
  adjust = 0;
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);

for (int n = 0; n < numLights; n++)
{
    lightDifference = modelInWorldPosition - mobileLightPositions[n];
    distanceSqr = dot(lightDifference, lightDifference);
    lightDir = lightDifference * inversesqrt(distanceSqr);
    attenuation[n] = 1 / distanceSqr;
    cosAngleIncidence[n] = clamp(dot(vertNormal, lightDir), 0.15, 1.0);
}

vec4 diffColour = texture(tex_unit0, tex_coord);
vec4 totalLight = adjust * attenuation[0] * shadowScaling[0] * cosAngleIncidence[0] * mobileLightIntensities[0] * diffColour;

for (int n = 1; n < numLights; n++)
{
  totalLight = totalLight + (attenuation[n] * shadowScaling[n] * cosAngleIncidence[n] * mobileLightIntensities[n] * diffColour);
}

outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 4, message tile)
#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoord;
layout(location = 2) in vec3 normal;
out vec2 tex_coord;

uniform mat4 tt_matrix;

void main() {
tex_coord = texCoord;
gl_Position = tt_matrix * position;
}

// Fragment shader (program 4, message tile)
#version 330

in vec2 tex_coord;
out vec4 outputColour;

uniform sampler2D tex_unit0;
uniform int mode;

void main() {
vec4 tex_colour = texture(tex_unit0, tex_coord);
vec4 highlight = vec4 (0, 0.75, 0, 1);
if (mode == 0)
  outputColour = tex_colour;
else
  outputColour = mix (highlight, tex_colour, 0.5);
}

// Vertex shader (program 5, two point light map lighted character model)
#version 330
layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoord;
layout(location = 2) in vec3 normal;

out vec3 modelPosition;
out vec2 tex_coord;
out vec3 vertNormal;
flat out vec3 modLightPos0;
flat out vec3 modLightPos1;
flat out vec4 intensity0;
flat out vec4 intensity1;

uniform mat4 mod_to_world;
uniform mat4 world_to_clip;
uniform mat4 world_to_mod;
uniform mat4 normal_transf;
uniform vec3 lmap_pos0[100];
uniform vec3 lmap_pos1[100];
uniform vec3 lmap_int0[100];
uniform vec3 lmap_int1[100];

void main() {
modelPosition = position.xyz;
tex_coord = texCoord;
vertNormal = (normal_transf * vec4(normal, 1)).xyz;
vec4 worldPos = mod_to_world * position;
int look_up = int(worldPos.x / 10) + int(worldPos.y / 10) * 10;
vec4 worldLightPos0 = vec4(lmap_pos0[look_up], 1);
vec4 worldLightPos1 = vec4(lmap_pos1[look_up], 1);
intensity0 = vec4(lmap_int0[look_up], 1);
intensity1 = vec4(lmap_int1[look_up], 1);
modLightPos0 = (world_to_mod * worldLightPos0).xyz;
modLightPos1 = (world_to_mod * worldLightPos1).xyz;

gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 5, two point light map lighted character model)
#version 330
in vec3 modelPosition;
in vec2 tex_coord;
in vec3 vertNormal;
flat in vec3 modLightPos0;
flat in vec3 modLightPos1;
flat in vec4 intensity0;
flat in vec4 intensity1;

out vec4 outputColour;

uniform sampler2D tex_unit0;
uniform float lmap_t0[240];
uniform float lmap_t1[240];
uniform int t;

float lightAttenuation (in vec3 fragPos, in vec3 lightPos, out vec3 lightDir) {
vec3 lightDifference = fragPos - lightPos;
float distanceSqr = dot(lightDifference, lightDifference);
lightDir = lightDifference * inversesqrt(distanceSqr);
return (1 / distanceSqr);
}

void main() {
vec3 lightDir0; vec3 lightDir1;
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);
float attenuation0 = lightAttenuation(modelPosition, modLightPos0, lightDir0);
float attenuation1 = lightAttenuation(modelPosition, modLightPos1, lightDir1);
float cosAngIncidence0 = dot(vertNormal, lightDir0);
float cosAngIncidence1 = dot(vertNormal, lightDir1);
cosAngIncidence0 = clamp(cosAngIncidence0, 0, 1);
cosAngIncidence1 = clamp(cosAngIncidence1, 0, 1);
vec4 diffColour = texture(tex_unit0, tex_coord);
float adjust0 = lmap_t0[t];
float adjust1 = lmap_t1[t];
vec4 totalLight = (attenuation0 * adjust0 * cosAngIncidence0 * intensity0 * diffColour) + (attenuation1 * adjust1 * cosAngIncidence1 * intensity1 * diffColour) + (vec4(0.1, 0.1, 0.1, 1) * diffColour);
outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 6, character model in dark area)
#version 330
layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texCoord;

out vec2 tex_coord;

uniform mat4 mod_to_world;
uniform mat4 world_to_clip;

void main() {
tex_coord = texCoord;
vec4 worldPos = mod_to_world * position;
gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 6, character model in dark area)
#version 330
in vec2 tex_coord;

out vec4 outputColour;

uniform sampler2D tex_unit0;

void main() {
outputColour = texture(tex_unit0, tex_coord);
}
