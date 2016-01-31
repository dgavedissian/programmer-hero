#version 150

uniform vec3 lightDir;
uniform vec3 colour;

in vec3 pointNormal;

out vec4 fragColour;

void main() {
    float intensity = dot(pointNormal, lightDir);
    fragColour.rgb = vec3(colour * intensity);
    fragColour.a = 1.0;
}
