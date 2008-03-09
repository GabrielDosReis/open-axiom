/*
  Copyright (C) 1991-2002, The Numerical ALgorithms Group Ltd.
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

      - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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

#define _TRANSFORM3D_C
#include "axiom-c-macros.h"


#include "header.h"

#include "all_3d.H1"

void 
#ifdef _NO_PROTO
matrixMultiply4x4(xxA,xxB,array)   
     float xxA[4][4], xxB[4][4], array[4][4];
#else
matrixMultiply4x4(float xxA[4][4],float xxB[4][4],float array[4][4])   
#endif
{
  array[0][0] = xxA[0][0]*xxB[0][0] + xxA[0][1]*xxB[1][0] + 
                xxA[0][2]*xxB[2][0] + xxA[0][3]*xxB[3][0]; 
  array[1][0] = xxA[1][0]*xxB[0][0] + xxA[1][1]*xxB[1][0] +
                xxA[1][2]*xxB[2][0] + xxA[1][3]*xxB[3][0]; 
  array[2][0] = xxA[2][0]*xxB[0][0] + xxA[2][1]*xxB[1][0] +
                xxA[2][2]*xxB[2][0] + xxA[2][3]*xxB[3][0]; 
  array[3][0] = xxA[3][0]*xxB[0][0] + xxA[3][1]*xxB[1][0] +
                xxA[3][2]*xxB[2][0] + xxA[3][3]*xxB[3][0]; 
  array[0][1] = xxA[0][0]*xxB[0][1] + xxA[0][1]*xxB[1][1] +
                xxA[0][2]*xxB[2][1] + xxA[0][3]*xxB[3][1]; 
  array[1][1] = xxA[1][0]*xxB[0][1] + xxA[1][1]*xxB[1][1] +
                xxA[1][2]*xxB[2][1] + xxA[1][3]*xxB[3][1]; 
  array[2][1] = xxA[2][0]*xxB[0][1] + xxA[2][1]*xxB[1][1] +
                xxA[2][2]*xxB[2][1] + xxA[2][3]*xxB[3][1]; 
  array[3][1] = xxA[3][0]*xxB[0][1] + xxA[3][1]*xxB[1][1] +
                xxA[3][2]*xxB[2][1] + xxA[3][3]*xxB[3][1];  
  array[0][2] = xxA[0][0]*xxB[0][2] + xxA[0][1]*xxB[1][2] +
                xxA[0][2]*xxB[2][2] + xxA[0][3]*xxB[3][2]; 
  array[1][2] = xxA[1][0]*xxB[0][2] + xxA[1][1]*xxB[1][2] +
                xxA[1][2]*xxB[2][2] + xxA[1][3]*xxB[3][2]; 
  array[2][2] = xxA[2][0]*xxB[0][2] + xxA[2][1]*xxB[1][2] +
                xxA[2][2]*xxB[2][2] + xxA[2][3]*xxB[3][2]; 
  array[3][2] = xxA[3][0]*xxB[0][2] + xxA[3][1]*xxB[1][2] +
                xxA[3][2]*xxB[2][2] + xxA[3][3]*xxB[3][2]; 
  array[0][3] = xxA[0][0]*xxB[0][3] + xxA[0][1]*xxB[1][3] +
                xxA[0][2]*xxB[2][3] + xxA[0][3]*xxB[3][3]; 
  array[1][3] = xxA[1][0]*xxB[0][3] + xxA[1][1]*xxB[1][3] +
                xxA[1][2]*xxB[2][3] + xxA[1][3]*xxB[3][3]; 
  array[2][3] = xxA[2][0]*xxB[0][3] + xxA[2][1]*xxB[1][3] +
                xxA[2][2]*xxB[2][3] + xxA[2][3]*xxB[3][3]; 
  array[3][3] = xxA[3][0]*xxB[0][3] + xxA[3][1]*xxB[1][3] +
                xxA[3][2]*xxB[2][3] + xxA[3][3]*xxB[3][3]; 
}


void 
#ifdef _NO_PROTO
vectorMatrix4(xxD,xxE,xxF)   
     float xxD[4], xxE[4][4], xxF[4];
#else
vectorMatrix4(float xxD[4],float xxE[4][4],float xxF[4])   
#endif
{
  xxF[0]= xxD[0]*xxE[0][0] + xxD[1]*xxE[1][0] + xxD[2]*xxE[2][0] + xxD[3]*xxE[3][0]; 
  xxF[1]= xxD[0]*xxE[0][1] + xxD[1]*xxE[1][1] + xxD[2]*xxE[2][1] + xxD[3]*xxE[3][1]; 
  xxF[2]= xxD[0]*xxE[0][2] + xxD[1]*xxE[1][2] + xxD[2]*xxE[2][2] + xxD[3]*xxE[3][2]; 
  xxF[3]= xxD[0]*xxE[0][3] + xxD[1]*xxE[1][3] + xxD[2]*xxE[2][3] + xxD[3]*xxE[3][3]; 
}


void 
#ifdef _NO_PROTO
ROTATE(xxR)  
     float xxR[4][4];
#else
ROTATE(float xxR[4][4])  
#endif
{
  xxR[0][0]= -(cosTheta); xxR[0][1]= -(-sinTheta*cosPhi); xxR[0][2]= -(sinTheta*sinPhi);  xxR[0][3]= 0.0; 
  xxR[1][0]= -(sinTheta); xxR[1][1]= -(cosTheta*cosPhi);  xxR[1][2]= -(-cosTheta*sinPhi); xxR[1][3]= 0.0; 
  xxR[2][0]= 0.0;         xxR[2][1]= -(sinPhi);           xxR[2][2]= -(cosPhi);           xxR[2][3]= 0.0; 
  xxR[3][0]= 0.0;         xxR[3][1]= 0.0;                 xxR[3][2]= 0.0;                 xxR[3][3]= -(1.0);
}

void 
#ifdef _NO_PROTO
ROTATE1(xxR)  
     float xxR[4][4];
#else
ROTATE1(float xxR[4][4])  
#endif
{
  xxR[0][0]= (cosTheta); xxR[0][1]= (-sinTheta*cosPhi); xxR[0][2]= (sinTheta*sinPhi);  xxR[0][3]= 0.0; 
  xxR[1][0]= (sinTheta); xxR[1][1]= (cosTheta*cosPhi);  xxR[1][2]= (-cosTheta*sinPhi); xxR[1][3]= 0.0; 
  xxR[2][0]= 0.0;        xxR[2][1]= (sinPhi);           xxR[2][2]= (cosPhi);           xxR[2][3]= 0.0; 
  xxR[3][0]= 0.0;        xxR[3][1]= 0.0;                xxR[3][2]= 0.0;                xxR[3][3]= (1.0);
}


void 
#ifdef _NO_PROTO
SCALE(x,y,z,xxS)  
     float x, y, z, xxS[4][4];
#else
SCALE(float x,float y,float z,float xxS[4][4])  
#endif
{
  xxS[0][0] = x;   xxS[0][1] = 0.0; xxS[0][2] = 0.0; xxS[0][3] = 0.0; 
  xxS[1][0] = 0.0; xxS[1][1] = y;   xxS[1][2] = 0.0; xxS[1][3] = 0.0; 
  xxS[2][0] = 0.0; xxS[2][1] = 0.0; xxS[2][2] = z;   xxS[2][3] = 0.0; 
  xxS[3][0] = 0.0; xxS[3][1] = 0.0; xxS[3][2] = 0.0; xxS[3][3] = 1.0;
}


void 
#ifdef _NO_PROTO
TRANSLATE(x,y,z,xxT)   
     float x, y, z, xxT[4][4];
#else
TRANSLATE(float x,float y,float z,float xxT[4][4])   
#endif
{
  xxT[0][0] = 1.0; xxT[0][1] = 0.0; xxT[0][2] = 0.0;  xxT[0][3] = 0.0; 
  xxT[1][0] = 0.0; xxT[1][1] = 1.0; xxT[1][2] = 0.0;  xxT[1][3] = 0.0; 
  xxT[2][0] = 0.0; xxT[2][1] = 0.0; xxT[2][2] = -1.0; xxT[2][3] = 0.0; 
  xxT[3][0] = x;   xxT[3][1] = y;   xxT[3][2] = z;    xxT[3][3] = 1.0;
}
