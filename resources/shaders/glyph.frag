#version 330 core

uniform sampler2D uTexture;
uniform vec3 uColor;

in vec2 vTexCoord;
in vec4 vColor;

out vec4 color;

void main() {

  float alpha = texture(uTexture, vTexCoord).r;
  color = vec4(uColor, alpha);
  
  // color = vec4(1,1,1,1);
  // color = vColor;
}
