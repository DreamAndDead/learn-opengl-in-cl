#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTex;
layout (location = 3) in vec3 aTangent;
layout (location = 4) in vec3 aBitangent;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

out vec3 norm;
out vec3 FragPos;
out vec2 TexCoord;

void main()
{
  gl_Position = proj * view * model * vec4(aPos, 1.0);
  FragPos = vec3(model * vec4(aPos, 1.0));
  // FIXME mat3(transpose(inverse(model))) * aNormal
  norm = vec3(model * vec4(aNormal, 0.0));
  TexCoord = aTex;
}
