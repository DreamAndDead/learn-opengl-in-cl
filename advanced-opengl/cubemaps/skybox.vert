#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTex;

out vec3 texCoord;

uniform mat4 proj;
uniform mat4 view;
uniform mat4 model;

void main()
{
  texCoord = aPos;
  vec4 pos = proj * view * model * vec4(aPos, 1.0);
  gl_Position = pos.xyww;
}
