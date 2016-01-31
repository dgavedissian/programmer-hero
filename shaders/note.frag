#version 150

uniform vec3 lightDir;

in vec3 pointNormal;

out vec4 fragColour;

void main() {
    float intensity = dot(pointNormal, lightDir);
    fragColour.rgb = vec3(intensity);
    fragColour.a = 1.0;
}
