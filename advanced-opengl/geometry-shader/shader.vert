#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTex;
layout (location = 3) in vec3 aTangent;
layout (location = 4) in vec3 aBitangent;

layout (std140) uniform Matrices
{
  mat4 proj;
  mat4 view;
};
uniform mat4 model;

out VS_OUT {
  vec3 norm;
  vec3 FragPos;
  vec2 TexCoord;
} vs_out;

void main()
{
  gl_Position = proj * view * model * vec4(aPos, 1.0);
  vs_out.FragPos = vec3(model * vec4(aPos, 1.0));
  // FIXME mat3(transpose(inverse(model))) * aNormal
  vs_out.norm = vec3(model * vec4(aNormal, 0.0));
  vs_out.TexCoord = aTex;
}
