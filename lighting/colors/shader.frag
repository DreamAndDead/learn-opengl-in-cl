#version 330 core

in vec2 TexCoord;

uniform sampler2D Texture1;
uniform sampler2D Texture2;

uniform vec3 lightColor;
uniform vec3 objectColor;

out vec4 FragColor;

void main()
{
  FragColor = vec4(lightColor * objectColor, 1.0);
}
