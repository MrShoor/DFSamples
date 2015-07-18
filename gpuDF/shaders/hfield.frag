#version 330
#extension GL_ARB_texture_rectangle : enable
 
uniform sampler2DRect uTex1; 
uniform float uDistance;
uniform float uMod16;
 
in vec2 TexCoord; 
 
layout(location=0) out vec4 FragColor;

float CalcC(float H, float V)
{
    return (sqrt(H*H+V*V));
}

float CalcDistance(){
  float dist = CalcC(0.0, texture2DRect(uTex1, TexCoord).r);
  for (int i=1;i<=uDistance;i++)
  {
    float H = i/uDistance;
    dist = min(dist, CalcC(H, texture2DRect(uTex1, TexCoord + vec2(i, 0.0)).r));
    dist = min(dist, CalcC(H, texture2DRect(uTex1, TexCoord - vec2(i, 0.0)).r));
  }
  return dist;
}

void main(){ 
  float dist = CalcDistance();
  if ((uMod16!=0.0)&&(dist<1.0))
  { 
    dist = fract(dist*uDistance/15.999);
  } 
  FragColor = vec4(dist); 
  FragColor.w = 1.0; 
};  
