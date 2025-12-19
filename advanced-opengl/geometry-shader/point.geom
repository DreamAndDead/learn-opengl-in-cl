#version 330 core

/* horizental line
layout (points) in;
layout (line_strip, max_vertices = 2) out;

void main() {
  gl_Position = gl_in[0].gl_Position + vec4(-0.1, 0.0, 0.0, 0.0);
  EmitVertex();

  gl_Position = gl_in[0].gl_Position + vec4( 0.1, 0.0, 0.0, 0.0);
  EmitVertex();

  EndPrimitive();
}
*/

/* pass through
layout (points) in;
layout (points, max_vertices = 1) out;

void main() {
  gl_Position = gl_in[0].gl_Position;
  EmitVertex();
  EndPrimitive();
}
 */

in VS_OUT {
  vec3 color;
} gs_in[];

out GS_OUT {
  vec3 color;
} gs_out;

layout (points) in;
layout (triangle_strip, max_vertices = 5) out;

void build_house(vec4 position) {
  gs_out.color = gs_in[0].color;
  gl_Position = position + vec4(-0.2, -0.2, 0.0, 0.0);
  EmitVertex();
  gl_Position = position + vec4( 0.2, -0.2, 0.0, 0.0);
  EmitVertex();
  gl_Position = position + vec4(-0.2,  0.2, 0.0, 0.0);
  EmitVertex();
  gl_Position = position + vec4( 0.2,  0.2, 0.0, 0.0);
  EmitVertex();
  gs_out.color = vec3(1.0, 1.0, 1.0);
  gl_Position = position + vec4( 0. ,  0.4, 0.0, 0.0);
  EmitVertex();
  EndPrimitive();
}

void main() {
  build_house(gl_in[0].gl_Position);
}
