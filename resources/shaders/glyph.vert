#version 330 core

struct Character {
  vec4 posCoords[4];
};

layout (std140) uniform charactersBlock {
  Character characters[500];
};

uniform mat4 uMVP;

in int  aInstanceGlyphIndex;
in vec2 aInstanceCharacterOffset;

out vec2 vTexCoord;
out vec4 vColor;

void main() {

    // Character character = characters[gl_InstanceID];
    Character character = characters[aInstanceGlyphIndex];
    // Character character = characters[55];
    vec4 posCoords = character.posCoords[gl_VertexID];
    vec2 position = posCoords.xy + aInstanceCharacterOffset;
    vec2 texCoord = posCoords.zw;
    vTexCoord = texCoord;

    gl_Position = uMVP * vec4(position.x, position.y, 0.0, 1.0);
}
