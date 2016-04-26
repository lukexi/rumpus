#version 330 core

uniform mat4 uProjectionView;
uniform mat4 uModel;
uniform vec4 uColor;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;
out     vec4 vDiffuse;

void main() {

    // Apply all matrix transformations to vert
    gl_Position = uProjectionView * uModel * vec4(aPosition, 1.0);
    
    // Pass some variables to the fragment shader
    vPosition = vec3(uModel * vec4(aPosition, 1.0));
    vNormal   = vec3(uModel * vec4(aNormal, 0.0));
    vUV       = aUV;
    vDiffuse  = uColor;
}
