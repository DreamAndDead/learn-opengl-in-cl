#version 330 core

in vec3 norm;
in vec3 FragPos;
in vec2 TexCoord;

struct Material {
  sampler2D diffuse;
  sampler2D specular;
  float shininess;
};

struct Light {
  vec3 position;

  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};


uniform vec3 lightPos;
uniform vec3 viewPos;
uniform Light light;

uniform Material material;

out vec4 FragColor;

void main()
{
  // ambient
  vec3 ambient = light.ambient * vec3(texture(material.diffuse, TexCoord));

  // diffuse
  vec3 n = normalize(norm);
  vec3 lightDir = normalize(lightPos - FragPos);
  float diff = max(dot(n, lightDir), 0.0);
  vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, TexCoord));

  // specular
  vec3 viewDir = normalize(viewPos - FragPos);
  vec3 reflectDir = reflect(-lightDir, n);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
  vec3 specular = light.specular * spec * vec3(texture(material.specular, TexCoord));

  vec3 result = ambient + diffuse + specular;

  FragColor = vec4(result, 1.0);
}
