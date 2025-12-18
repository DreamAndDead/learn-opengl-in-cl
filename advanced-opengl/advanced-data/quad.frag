#version 330 core

in vec2 texCoord;

out vec4 FragColor;

uniform sampler2D framebuffer;

vec3 inversion()
{
  return vec3(1.0 - texture(framebuffer, texCoord));
}

vec3 grayscale()
{
  vec4 color = texture(framebuffer, texCoord);
  float avg = (color.r + color.g + color.b) / 3.0;
  return vec3(avg);
}

vec3 grayscale_humaneye()
{
  vec4 color = texture(framebuffer, texCoord);
  float avg = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;
  return vec3(avg);
}



vec3 kernel(float kernel[9])
{
  const float offset = 1.0 / 300.0;  
  vec2 offsets[9] = vec2[](
                           vec2(-offset,  offset), // top-left
                           vec2( 0.0f,    offset), // top-center
                           vec2( offset,  offset), // top-right
                           vec2(-offset,  0.0f),   // center-left
                           vec2( 0.0f,    0.0f),   // center-center
                           vec2( offset,  0.0f),   // center-right
                           vec2(-offset, -offset), // bottom-left
                           vec2( 0.0f,   -offset), // bottom-center
                           vec2( offset, -offset)  // bottom-right    
                           );
    
  vec3 sampleTex[9];
  for(int i = 0; i < 9; i++)
    {
      sampleTex[i] = vec3(texture(framebuffer, texCoord.st + offsets[i]));
    }
  vec3 col = vec3(0.0);
  for(int i = 0; i < 9; i++)
    col += sampleTex[i] * kernel[i];

  return col;
}

void main()
{
  FragColor = vec4(inversion(), 1.0);
  FragColor = vec4(grayscale(), 1.0);
  FragColor = vec4(grayscale_humaneye(), 1.0);

  float sharpen[9] = float[](
                            -1, -1, -1,
                            -1,  9, -1,
                            -1, -1, -1
                            );
  FragColor = vec4(kernel(sharpen), 1.0);

  float blur[9] = float[](
                          1, 2, 1,
                          2, 4, 2,
                          1, 2, 1
                          );
  for (int i = 0; i < 9; i++)
    {
      blur[i] /= 16;
    }
  FragColor = vec4(kernel(blur), 1.0);

  float edge[9] = float[](
                          1, 1, 1,
                          1, -8, 1,
                          1, 1, 1
                          );
  FragColor = vec4(kernel(edge), 1.0);

  FragColor = vec4(vec3(texture(framebuffer, texCoord)), 1.0); 
}
