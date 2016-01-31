#version 150

uniform vec3 lightDir;

in vec3 pointNormal;
in vec3 diffuse;

out vec4 fragColour;

void main() {
    float intensity = dot(pointNormal, lightDir);
    fragColour.rgb = diffuse * intensity;
    fragColour.a = 1.0;
}
