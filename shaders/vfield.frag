#version 330
#extension GL_ARB_texture_rectangle : enable

uniform sampler2DRect uTex1;
uniform float uDistance;

in vec2 TexCoord;

layout(location=0) out vec4 FragColor;

void main(){

  if (texture2DRect(uTex1, TexCoord).r>0.5)
  {
    FragColor = vec4(0.0);
    return;
  }

  for (int i=1;i<=uDistance;i++)
  {
    if (texture2DRect(uTex1, TexCoord + vec2(0.0, i)).r>0.5)
    {
        FragColor = vec4(i/uDistance);
        return;
    }
    if (texture2DRect(uTex1, TexCoord - vec2(0.0, i)).r>0.5)
    {
        FragColor = vec4(i/uDistance);
        return;
    }
  }
  FragColor = vec4(1.0);
  
};
