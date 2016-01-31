#version 150

uniform mat4 modelViewProj;

in vec3 vertexPos;

void main() {
    gl_Position = modelViewProj * vec4(vertexPos, 1.0);
}
