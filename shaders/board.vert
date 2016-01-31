#version 150

uniform mat4 modelViewProj;

in vec3 vertexPos;
in vec3 vertexNormal;
in vec3 vertexColour;

out vec3 pointNormal;
out vec3 diffuse;

void main() {
    gl_Position = modelViewProj * vec4(vertexPos,1);
    pointNormal = vertexNormal;
    diffuse = vertexColour;
}
