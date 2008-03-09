/*
  Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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

#define _POT3D_C
#include "axiom-c-macros.h"

#include "header.h"
#include "all_3d.H1"

/****************************/
/***  void getPotValue()  ***/
/****************************/

mouseCoord 
#ifdef _NO_PROTO
getPotValue(eX,eY,xH,yH)
     short eX,eY,xH,yH;
#else
getPotValue(short eX,short eY,short xH,short yH)
#endif
{

  mouseCoord whereMouse;
  float x,y;

  x = (float)eX/xH - 1;
  y = -((float)eY/yH -1);

  /* make non-linear potentiometer */
  whereMouse.x = x*x*x; 
  whereMouse.y = y*y*y; 
  if (whereMouse.x > 1.0)  whereMouse.x = 1.0;
  if (whereMouse.y > 1.0)  whereMouse.y = 1.0;
  if (whereMouse.x < -1.0) whereMouse.x = -1.0;
  if (whereMouse.y < -1.0) whereMouse.y = -1.0;

  return(whereMouse);

} /* getPotValue() */



/**********************************/
/***  void getLinearPotValue()  ***/
/**********************************/

mouseCoord 
#ifdef _NO_PROTO
getLinearPotValue(eX,eY,xH,yH)
     short eX,eY,xH,yH;
#else
getLinearPotValue(short eX,short eY,short xH,short yH)
#endif
{

  mouseCoord whereMouse;

  whereMouse.x = (float)eX/xH - 1;
  whereMouse.y = -((float)eY/yH -1);

  return(whereMouse);

} /* getLinearPotValue() */


