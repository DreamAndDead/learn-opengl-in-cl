#version 330 core

in VS_OUT {
  vec3 norm;
  vec3 FragPos;
  vec2 TexCoord;
} fs_in;

struct Material {
  sampler2D diffuse1;
  sampler2D diffuse2;
  sampler2D diffuse3;
  sampler2D diffuse4;

  sampler2D specular1;
  sampler2D specular2;
  sampler2D specular3;
  sampler2D specular4;

  float shininess;
};

struct DirectionalLight {
  vec3 direction;

  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};

struct PointLight {
  vec3 position;

  vec3 ambient;
  vec3 diffuse;
  vec3 specular;

  float constant;
  float linear;
  float quadratic;
};

struct SpotLight {
  vec3 position;
  vec3 direction;
  float cutoff;
  float outerCutoff;

  vec3 ambient;
  vec3 diffuse;
  vec3 specular;

  float constant;
  float linear;
  float quadratic;
};

uniform vec3 viewPos;
uniform samplerCube skybox;

uniform DirectionalLight dirLight;
#define NR_POINT_LIGHTS 4
uniform PointLight pointLights[NR_POINT_LIGHTS];
uniform SpotLight spotLight;

uniform Material material;

out vec4 FragColor;

vec4 NoLight()
{
  return texture(material.diffuse1, fs_in.TexCoord);
}

void main()
{
  // if (gl_FragCoord.x < 480)
  //   FragColor = NoLight();
  // else
  //   FragColor = vec4(0.0, 1.0, 0.0, 1.0);

  if (gl_FrontFacing)
    FragColor = NoLight();
  else
    FragColor = vec4(0.0, 1.0, 0.0, 1.0);
    
}
