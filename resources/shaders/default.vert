#version 330 core

uniform mat4 uProjectionView;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

in      mat4 aInstanceTransform;
in      vec4 aInstanceColor;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;
out     vec4 vDiffuse;

void main() {
    // NOTE: we pass our matrices untransposed in Haskell to distribute more work to the GPU
    
    mat4 model = transpose(aInstanceTransform);
    // Apply all matrix transformations to vert
    gl_Position = uProjectionView * model * vec4(aPosition, 1.0);
    
    // Pass some variables to the fragment shader
    vPosition = vec3(model * vec4(aPosition, 1.0));
    vNormal   = vec3(model * vec4(aNormal, 0.0));
    vUV       = aUV;
    vDiffuse  = aInstanceColor;
}
