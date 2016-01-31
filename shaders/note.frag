#version 150

uniform vec3 colour;

out vec4 fragColour;

void main() {
    fragColour = vec4(colour * 0.5 + 0.5, 1.0);
}
