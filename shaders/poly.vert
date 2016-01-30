#version 150
in vec3 vertexPos;
in vec3 vertexNormal;
in vec3 vertexColour;
uniform mat4 modelViewProj;
out vec3 pointNormal;
out vec3 diffuse;
out vec4 eye;

void main() {
  gl_Position = modelViewProj * vec4(vertexPos,1);
  eye = -(modelViewProj * vec4(vertexPos,1));
  pointNormal = vertexNormal;
  diffuse = vertexColour;
}
