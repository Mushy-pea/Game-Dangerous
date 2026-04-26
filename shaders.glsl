// Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
// If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

// Vertex shader (program 0, two light map plus up to four programmable point lighted object, no texture)
#version 420

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
#version 420

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
  cosAngleIncidence[n] = dot(vertNormal, lightDir);
}

vec4 totalLight = (attenuation[0] * lmap_t0[t] * cosAngleIncidence[0] * lightIntensities[0] * diffColour) + (attenuation[1] * lmap_t1[t] * cosAngleIncidence[1] * lightIntensities[1] * diffColour);

for (int n = 2; n < numLights; n++)
{
  totalLight = totalLight + (attenuation[n] * cosAngleIncidence[n] * lightIntensities[n] * diffColour);
}

outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 1, two light map plus up to four programmable point lighted object, with texture)
#version 420

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
#version 420

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
  cosAngleIncidence[n] = dot(vertNormal, lightDir);
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
#version 420

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
#version 420

in vec3 modelInWorldPosition;
in vec4 diffColour;
flat in vec3 vertNormal;

out vec4 outputColour;

uniform vec4 mobileLightIntensities[8];
uniform vec3 mobileLightPositions[8];
uniform int numLights;

void main() {
float distanceSqr;
vec3 lightDifference;
vec3 lightDir;
float cosAngleIncidence[8];
float attenuation[8];
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);
vec4 totalLight = vec4(0.0, 0.0, 0.0, 0.0);

for (int n = 0; n < numLights; n++)
{
  lightDifference = modelInWorldPosition - mobileLightPositions[n];
  distanceSqr = dot(lightDifference, lightDifference);
  lightDir = lightDifference * inversesqrt(distanceSqr);
  attenuation[n] = 1 / distanceSqr;
  cosAngleIncidence[n] = dot(vertNormal, lightDir);
  totalLight = totalLight + attenuation[n] * cosAngleIncidence[n] * mobileLightIntensities[n] * diffColour;
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
  bvec4 wallShadow0[3600];
};
layout(binding = 4) uniform WallShadowMap1
{
  bvec4 wallShadow1[3600];
};
layout(binding = 5) uniform WallShadowMap2
{
  bvec4 wallShadow2[3600];
};
layout(binding = 6) uniform FloorShadowMap0
{
  bool floorShadow0[3600];
};
layout(binding = 7) uniform FloorShadowMap1
{
  bool floorShadow1[3600];
};
layout(binding = 8) uniform FloorShadowMap2
{
  bool floorShadow2[3600];
};

out vec3 modelInWorldPosition;
out vec2 tex_coord;
flat out vec3 vertNormal;
out float shadowScaling[8];

uniform mat4 mod_to_world;
uniform mat4 world_to_clip;
uniform vec3 mobileLightPositions[8];
uniform int numLights;

void intersectSequence(const vec3 lightPos, const vec3 lightDifference, inout float intersection[75]) {
  const float distanceSqr = dot(lightDifference, lightDifference);
  const vec3 lightDir = lightDifference * inversesqrt(distanceSqr);
  const float lightDirX = (lightDir.x == 0.0) ? 0.000001 : lightDir.x;
  const float lightDirY = (lightDir.y == 0.0) ? 0.000001 : lightDir.y;
  const float lightDirZ = (lightDir.z == 0.0) ? 0.000001 : lightDir.z;
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
shadowScaling[0] = 1; shadowScaling[1] = 1; shadowScaling[2] = 1; shadowScaling[3] = 1;
shadowScaling[4] = 1; shadowScaling[5] = 1; shadowScaling[6] = 1; shadowScaling[7] = 1;
const int vertexU = int(clamp(modelInWorldPosition.x, 0.0, 49.9));
const int vertexV = int(clamp(modelInWorldPosition.y, 0.0, 49.9));
const int vertexW = int(clamp(modelInWorldPosition.z + 0.5, 0.0, 2.9));

for (int m = 0; m < numLights; m++) {
  const vec3 lightDifference = modelInWorldPosition - mobileLightPositions[m];
  float intersection[75];
  intersectSequence(mobileLightPositions[m], lightDifference, intersection);
  int u = int(clamp(mobileLightPositions[m].x, 0.0, 49.9));
  int v = int(clamp(mobileLightPositions[m].y, 0.0, 49.9));
  int w = int(clamp(mobileLightPositions[m].z, 0.0, 2.9));
  int i = 0;
  int j = 1;
  int k = 2;
  for (int n = 0; n < 75; n = n + 3) {
    bool u1Sample;
    bool u2Sample;
    bool v1Sample;
    bool v2Sample;
    if (u == vertexU && v == vertexV && w == vertexW) {
      break;
    }
    if (w == 0) {
      u1Sample = wallShadow0[u * 60 + v].x;
      u2Sample = wallShadow0[u * 60 + v].y;
      v1Sample = wallShadow0[u * 60 + v].z;
      v2Sample = wallShadow0[u * 60 + v].w;
    }
    else if (w == 1) {
      u1Sample = wallShadow1[u * 60 + v].x;
      u2Sample = wallShadow1[u * 60 + v].y;
      v1Sample = wallShadow1[u * 60 + v].z;
      v2Sample = wallShadow1[u * 60 + v].w;
    }
    else {
      u1Sample = wallShadow2[u * 60 + v].x;
      u2Sample = wallShadow2[u * 60 + v].y;
      v1Sample = wallShadow2[u * 60 + v].z;
      v2Sample = wallShadow2[u * 60 + v].w;
    }
    const bool fSample0 = floorShadow0[u * 60 + v];
    const bool fSample1 = floorShadow1[u * 60 + v];
    const bool fSample2 = floorShadow2[u * 60 + v];
    const float floorShadowScaling = 0.1;
    
    if (abs(intersection[k]) < abs(intersection[i]) && abs(intersection[k]) < abs(intersection[j])) {
      if (intersection[k] < 0 && w == 0) {
        if (fSample0) {
          shadowScaling[m] = floorShadowScaling;
          break;
        }
        else {
          k = k + 3;
          continue;
        }
      }
      else if (intersection[k] < 0 && w == 1) {
        if (fSample1) {
          shadowScaling[m] = floorShadowScaling;
          break;
        }
        else {
          w--;
          k = k + 3;
          continue;
        }
      }
      else if (intersection[k] < 0 && w == 2) {
        if (fSample2) {
          shadowScaling[m] = floorShadowScaling;
          break;
        }
        else {
          w--;
          k = k + 3;
          continue;
        }
      }
      else if (intersection[k] > 0 && w == 0) {
        if (fSample1) {
          shadowScaling[m] = floorShadowScaling;
          break;
        }
        else {
          w++;
          k = k + 3;
          continue;
        }
      }
      else if (intersection[k] > 0 && w == 1) {
        if (fSample2) {
          shadowScaling[m] = floorShadowScaling;
          break;
        }
        else {
          w++;
          k = k + 3;
          continue;
        }
      }
      else {
        k = k + 3;
        continue;
      }
    }

    if (abs(intersection[i]) < abs(intersection[j])) {
      if (intersection[i] < 0) {
        if (u1Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          u--;
          i = i + 3;
        }
      }
      else {
        if (u2Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          u++;
          i = i + 3;
        }
      }
    }
    else if (abs(intersection[i]) > abs(intersection[j])) {
      if (intersection[j] < 0) {
        if (v1Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          v--;
          j = j + 3;
        }
      }
      else {
        if (v2Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          v++;
          j = j + 3;
        }
      }
    }
    else {
      if (intersection[i] < 0 && intersection[j] < 0) {
        if (u1Sample || v1Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          u--;
          v--;
          i = i + 3;
          j = j + 3;
        }
      }
      else if (intersection[i] < 0 && intersection[j] > 0) {
        if (u1Sample || v2Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          u--;
          v++;
          i = i + 3;
          j = j + 3;
        }
      }
      else if (intersection[i] > 0 && intersection[j] < 0) {
        if (u2Sample || v1Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          u++;
          v--;
          i = i + 3;
          j = j + 3;
        }
      }
      else {
        if (u2Sample || v2Sample) {
          shadowScaling[m] = 0.0;
          break;
        }
        else {
          u++;
          v++;
          i = i + 3;
          j = j + 3;
        }
      }
    }
  }
}

gl_Position = world_to_clip * worldPos;
}

// Fragment shader (program 3, player torch plus up to four programmable point lighted object, with texture)
#version 420

in vec3 modelInWorldPosition;
in vec2 tex_coord;
flat in vec3 vertNormal;
in float shadowScaling[8];

out vec4 outputColour;

uniform sampler2D tex_unit0;
uniform vec4 mobileLightIntensities[8];
uniform vec3 mobileLightPositions[8];
uniform int numLights;

void main() {
float distanceSqr;
vec3 lightDifference;
vec3 lightDir;
float cosAngleIncidence[8];
float attenuation[8];
float g = 0.4545455;
vec4 gamma = vec4(g, g, g, 1);
vec4 diffColour = texture(tex_unit0, tex_coord);
vec4 totalLight = vec4(0.0, 0.0, 0.0, 0.0);

for (int n = 0; n < numLights; n++)
{
  lightDifference = modelInWorldPosition - mobileLightPositions[n];
  distanceSqr = dot(lightDifference, lightDifference);
  lightDir = lightDifference * inversesqrt(distanceSqr);
  attenuation[n] = 1 / distanceSqr;
  cosAngleIncidence[n] = dot(vertNormal, lightDir);
  totalLight = totalLight + max(attenuation[n] * shadowScaling[n] * cosAngleIncidence[n], 0.00005) * mobileLightIntensities[n] * diffColour;
}

outputColour = pow(totalLight, gamma);
}

// Vertex shader (program 4, message tile)
#version 420

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
#version 420

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
#version 420
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
#version 420
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
#version 420
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
#version 420
in vec2 tex_coord;

out vec4 outputColour;

uniform sampler2D tex_unit0;

void main() {
outputColour = texture(tex_unit0, tex_coord);
}
