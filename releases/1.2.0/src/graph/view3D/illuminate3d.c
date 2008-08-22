/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

      - Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      - Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.

      - Neither the name of The Numerical Algorithms Group Ltd. nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#define _ILLUMINATE3D_C
#include "openaxiom-c-macros.h"


#include <math.h>

#include "header.h"
#include "draw.h"

#include "all_3d.H1"

/***********************
 * void phong(pt,N)    *
 *                     *
 * a general routine   *
 * for determining the *
 * intensity values at *
 * a particular point  *
 * using the Phong     *
 * illumination model  *
 * with Phong shading  *
 ***********************/

float 
phong(triple pt,float N[3])
{
  float     dotLN, dotHN, H[3], E[3], P[3], NM[3], L[3];
  float     color, diffuse, specular;

  diffuse = 0.0;  specular = 0.0;

  /* renormalize average normal vector for the point */
  normalizeVector(N);
  /* temporary norm, in case the sign is switched */
  NM[0] = N[0];  NM[1] = N[1];  NM[2] = N[2];

  P[0] = pt.x;  P[1] = pt.y;  P[2] = pt.z;
  normalizeVector(P);

  /* vector from infinite light source */
  L[0] = viewport->lightVector[0];
  L[1] = viewport->lightVector[1];
  L[2] = viewport->lightVector[2];
  normalizeVector(L);

  /* vector from point to observers eye */
  normalizeVector(eyePoint);
  E[0] = 8.0*eyePoint[0] - P[0];
  E[1] = 8.0*eyePoint[1] - P[1];
  E[2] = 8.0*eyePoint[2] - P[2];
  normalizeVector(E);

  /*  light returned even if normal faces away from light source */
  dotLN = L[0]*NM[0] + L[1]*NM[1] + L[2]*NM[2];
  if (dotLN < 0.0) dotLN = -dotLN;
  diffuse = dotLN*lightIntensity;
  /* calculate specular highlight if surface faces light source */
  if (dotLN > 0.0) {
    H[0] = E[0] + L[0];
    H[1] = E[1] + L[1]; 
    H[2] = E[2] + L[2];
    normalizeVector(H);
    dotHN = NM[0]*H[0]+NM[1]*H[1]+NM[2]*H[2];
    if (dotHN < 0.0) dotHN = -dotHN;
    specular = pow((double)dotHN,coeff)*lightIntensity;
  } 

  /* return intensity value from 0.0 to 1.0 */
  color = Camb + diffuse*Cdiff + specular*Cspec;

  if (color > 1.0) color = 1.0;
  if (color < 0.0) color = 0.0;

  return(color);
}

int 
hueValue(float val)
{
  int hue;
        
  hue = floor(absolute(val) * viewport->numberOfHues) + viewport->hueOffset;
  if (hue > 26) hue = 26;

  return hue;
}

int 
getHue(float val)
{
  int hue;
        
  hue = hueValue(val);
  if (hue < 11)
    hue *= 6;
  else
    if (hue > 10 && hue < 16)
      hue = hue*20 - 140;
    else 
      hue = hue*12 - 12;

  return hue;
}

/**** Conversion functions for different color models ****/

float 
Value(float n1, float n2, float hue)
{
  float v;

  if (hue > 360.0) hue -= 360.0;
  if (hue < 0.0) hue += 360.0;
  if (hue < 60.0) {
    v = n1 + (n2-n1)*hue/60.0;
  } else {
    if (hue < 180.0) { 
      v = n2; 
    } else {
      if (hue < 240.0) {
        v = n1 + (n2-n1)*(240.0-hue)/60.0;
      } else {
        v = n1;
      }
    }
  }
  return(v);
}


RGB
hlsTOrgb(float h,float l,float s)
{
  RGB rgb;
  float m1, m2;

  if (l <= 0.5) { 
    m2 = l*(1.0+s); 
  }
  else {
    m2 = l+s-l*s;
  }
  m1 = 2.0*l-m2;
  rgb.r = Value(m1,m2,h+120.0);
  rgb.g = Value(m1,m2,h);
  rgb.b = Value(m1,m2,h-120.0);

  return(rgb);
}






