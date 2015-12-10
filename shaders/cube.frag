#version 330 core

uniform vec3 uCamera;
uniform vec4 uDiffuse;
uniform vec3 uCubeHit;

in      vec3 vPosition;
in      vec3 vNormal;

out     vec4 fragColor;

const   vec3 lightColor = vec3(1);
const   float ambient = 0.2;

void main() {

    vec3 lightPosition = uCamera;
    
    //calculate normal in world coordinates
    vec3 normal = normalize(vNormal);

    //calculate the location of this fragment in world coordinates
    vec3 surfacePos = vPosition;
    
    // vec4 surfaceColor = texture(materialTex, fragTexCoord);
    vec4 surfaceColor = uDiffuse;
    vec3 surfaceToLight = normalize(lightPosition - surfacePos);

    // Calculate final color of the pixel, based on:
    // 1. The angle of incidence: diffuseCoefficient
    // 2. The color/intensities of the light: lightColor
    // 3. The diffuse color: surfaceColor

    float diffuseCoefficient = max(ambient, dot(normal, surfaceToLight));
    vec3 diffuseLit = diffuseCoefficient * surfaceColor.rgb * lightColor;


    fragColor = vec4(diffuseLit, uDiffuse.a);

    float hitDist = distance(vPosition, uCubeHit);
    fragColor.r += hitDist;
}
