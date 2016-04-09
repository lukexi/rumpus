#version 330 core

struct Character {
  vec4 posCoords[4];
};

layout (std140) uniform charactersBlock {
  Character characters[500];
};

uniform mat4 uProjectionView;
uniform mat4 uModel;
uniform float uTime;

in int  aInstanceGlyphIndex;
in vec4 aInstanceCharacterOffset;

out vec2 vTexCoord;
out vec3 vColor;

// via http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

mat4 scaleMatrix(vec3 s) { 
    return transpose(mat4(
        s.x, 0.0, 0.0, 0.0,
        0.0, s.y, 0.0, 0.0,
        0.0, 0.0, s.z, 0.0,
        0.0, 0.0, 0.0, 1.0
    ));
}

mat4 translateMatrix(vec3 t) { 
    return transpose(mat4(
        1.0, 0.0, 0.0, t.x,
        0.0, 1.0, 0.0, t.y,
        0.0, 0.0, 1.0, t.z,
        0.0, 0.0, 0.0, 1.0
    ));
}

void main() {
    mat4 mvp = uProjectionView * uModel; 

    Character character = characters[aInstanceGlyphIndex];
    
    vec4 posCoords = character.posCoords[gl_VertexID];
    vec3 position = vec3(posCoords.xy,0.0) + aInstanceCharacterOffset.xyz;
    vec2 texCoord = posCoords.zw;
    
    vTexCoord = texCoord;

    vColor = vec3(aInstanceCharacterOffset.w,1.0,1.0);
    gl_Position = mvp * vec4(position.xyz, 1.0);

    // Scaling & color effects for the cursor
    if (aInstanceCharacterOffset.w < 0.0) {
        float scaleTime = sin(uTime * 5) / 2 + 1;
        float hueTime   = sin(uTime * 10) / 2 + 1;
        vColor = hsv2rgb(vec3(hueTime,0.5,0.8));

        vec3 scale = vec3(scaleTime + 2);

        vec3 posCoordsOffset = vec3(posCoords.xy, 0.0);
        
        posCoordsOffset.y -= posCoordsOffset.y/2;
        posCoordsOffset.x -= posCoordsOffset.x/2;
        
        vec3 positionScaled = vec3(posCoordsOffset * scale) 
                                + aInstanceCharacterOffset.xyz;
        
        gl_Position = mvp * vec4(positionScaled, 1.0);
    }
}

