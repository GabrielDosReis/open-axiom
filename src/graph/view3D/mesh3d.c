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

#define _MESH3D_C
#include "axiom-c-macros.h"

#include <math.h>

#include "header.h"
#include "all_3d.H1"

/***************************************************************************
 *** void getMeshNormal(x0,y0,z0,x1,y1,z1,x2,y2,z2,zMin,zRange,Normal);  ***
 ***************************************************************************/

void 
#ifdef _NO_PROTO
getMeshNormal(x0,y0,z0,x1,y1,z1,x2,y2,z2,zMin,zRange,Normal)
  float x0,y0,z0,x1,y1,z1,x2,y2,z2,zMin,zRange,Normal[3]; 
#else
getMeshNormal(float x0,float y0,float z0,float x1,float y1,float z1,
              float x2,float y2,float z2,float zMin,float zRange,float Normal[3])
#endif
{
  float Ax,Ay,Az,Bx,By,Bz,
        UnitFactor;

  Ax = x0-x1;  Ay = y0-y1;  Az = z0-z1;
  Bx = x2-x1;  By = y2-y1;  Bz = z2-z1;

  /* compute cross product */

  Normal[0] = (Ay*Bz - Az*By);
  Normal[1] = (Az*Bx - Ax*Bz);
  Normal[2] = (Ax*By - Ay*Bx);

  /* normalize normal vector */

  UnitFactor = sqrt(Normal[0]*Normal[0] +
                    Normal[1]*Normal[1] +
                    Normal[2]*Normal[2]);
  if (UnitFactor > 0.0) {
    Normal[0] /= UnitFactor;
    Normal[1] /= UnitFactor;
    Normal[2] /= UnitFactor;
  } else {
    Normal[0] = 0.0;
    Normal[1] = 0.0;
    Normal[2] = 0.0;
  }

} /* getMeshNormal() */


/***********************************
 ****  void normalizeVector(v)  ****
 ***********************************/

void 
#ifdef _NO_PROTO
normalizeVector(v)
  float *v;
#else
normalizeVector(float *v)
#endif
{
   /* v should be a triple (ignoring the rest of the array if necessary) */

  float UnitFactor;

  UnitFactor = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
  if (UnitFactor != 0.0) {
    v[0] /= UnitFactor;
    v[1] /= UnitFactor;
    v[2] /= UnitFactor;
  } else {
    v[0] = v[1] = v[2] = 0.0;
  }

} /* normalizeVector() */


/************************************
 ****  void dotProduct(a,b,size) ****
 ************************************/

float 
#ifdef _NO_PROTO
dotProduct(a,b,size)
  float *a,*b;
  int size;
#else
dotProduct(float * a,float *b,int size)
#endif
{
  int i;
  float f=0;

  for (i=0; i<size; i++)
    f += (a[i]*b[i]);
  return(f);

} /* dotProduct() */

