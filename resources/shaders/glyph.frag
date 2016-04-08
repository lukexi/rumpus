#version 330 core

uniform sampler2D uTexture;

in vec2 vTexCoord;
in vec3 vColor;

out vec4 color;

void main() {

  float alpha = texture(uTexture, vTexCoord).r;
  color = vec4(vColor, alpha);
  
  // color = vec4(1,1,1,1);
}
