#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

uniform float PointSize;

out VS_OUT {
  vec3 color;
} vs_out;

void main()
{
  gl_Position = vec4(aPos, 1.0);
  gl_PointSize = PointSize;
  vs_out.color = aColor;
}
