#version 330 core

in vec3 norm;
in vec3 FragPos;
in vec2 TexCoord;

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

uniform DirectionalLight dirLight;
#define NR_POINT_LIGHTS 4
uniform PointLight pointLights[NR_POINT_LIGHTS];
uniform SpotLight spotLight;

uniform Material material;

out vec4 FragColor;

vec3 CalcPointLight(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir)
{
  vec3 lightDir = normalize(light.position - fragPos);
  float distance = length(light.position - fragPos);
  
  vec3 reflectDir = reflect(-lightDir, normal);

  // ambient
  vec3 ambient = light.ambient * vec3(texture(material.diffuse1, TexCoord));

  // diffuse
  float diff = max(dot(normal, lightDir), 0.0);
  vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse1, TexCoord));

  // specular
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
  vec3 specular = light.specular * spec * vec3(texture(material.specular1, TexCoord));

  float attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic * distance * distance);
  return attenuation * (ambient + diffuse + specular);
}

vec3 CalcDirLight(DirectionalLight light, vec3 normal, vec3 viewDir)
{
  vec3 lightDir = normalize(-light.direction);
  vec3 reflectDir = reflect(-lightDir, normal);
  
  // ambient
  vec3 ambient = light.ambient * vec3(texture(material.diffuse1, TexCoord));

  // diffuse
  float diff = max(dot(normal, lightDir), 0.0);
  vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse1, TexCoord));

  // specular
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
  vec3 specular = light.specular * spec * vec3(texture(material.specular1, TexCoord));

  return (ambient + diffuse + specular);
}

vec3 CalcSpotLight(SpotLight light, vec3 normal, vec3 fragPos, vec3 viewDir)
{
  vec3 lightDir = normalize(light.position - fragPos);
  float distance = length(light.position - fragPos);
  float attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic * distance * distance);
  
  float theta = dot(lightDir, normalize(-light.direction));
  float epsilon = light.cutoff - light.outerCutoff;
  float intensity = clamp((theta - light.outerCutoff) / epsilon, 0.0, 1.0);
  
  // ambient
  vec3 ambient = light.ambient * vec3(texture(material.diffuse1, TexCoord));

  // diffuse
  float diff = max(dot(normal, lightDir), 0.0);
  vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse1, TexCoord));

  // specular
  vec3 reflectDir = reflect(-lightDir, normal);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
  vec3 specular = light.specular * spec * vec3(texture(material.specular1, TexCoord));

  return attenuation * (ambient + intensity * (diffuse + specular));
}

vec3 NoLight()
{
  vec3 result = vec3(texture(material.diffuse1, TexCoord));
  return result;
}

void main()
{
  vec3 n = normalize(norm);
  vec3 viewDir = normalize(viewPos - FragPos);

  vec3 result = NoLight();
  // result = CalcDirLight(dirLight, n, viewDir);
  // result = CalcPointLight(pointLights[0], n, FragPos, viewDir);
  // result = CalcSpotLight(spotLight, n, FragPos, viewDir);

  FragColor = vec4(result, 1.0);
}
