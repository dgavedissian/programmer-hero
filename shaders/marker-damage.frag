#version 150

uniform vec4 colour;

out vec4 fragColour;

void main() {
    fragColour = vec4(colour * 0.6);
}
