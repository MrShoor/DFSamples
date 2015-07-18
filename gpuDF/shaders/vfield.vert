#version 330

uniform vec2 uTexSize;

layout(location=0) in vec2 aPos;

out vec2 TexCoord;

void main(){
  TexCoord = (aPos+1.0)*0.5;
  TexCoord *= uTexSize;
  gl_Position.xy = aPos;
  gl_Position.zw = vec2(0.0, 1.0);
};