#version 330 core

in vec3 ourColor;
in vec2 TexCoord;

uniform sampler2D Texture1;
uniform sampler2D Texture2;

out vec4 FragColor;

void main()
{
  FragColor = mix(texture(Texture1, TexCoord), texture(Texture2, TexCoord), 0.2);
}
