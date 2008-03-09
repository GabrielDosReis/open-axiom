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

#define _LIGHTBUT3D_C
#include "axiom-c-macros.h"

#include "header.h"
#include "cpanel.h"

#include "all_3d.H1"
/*****************************************************
 * int initLightButtons (lightButtons)               *
 *                                                   *
 * Creates the fields for each button window in the  *
 * three dimensional lighting subpanel, and returns  *
 * the number of buttons created.                    *
 *                                                   *
 *****************************************************/

int 
#ifdef _NO_PROTO
initLightButtons (lightButtons)
        buttonStruct *lightButtons;
#else
initLightButtons (buttonStruct *lightButtons)
#endif
{
  int ii;
  int num = 0;

  /* Not functional -- can be used to allow light window as a potentiometer */
  ii = lightMove;
  lightButtons[ii].buttonX      = 63;
  lightButtons[ii].buttonY      = 30;  
  lightButtons[ii].buttonWidth  = 171;
  lightButtons[ii].buttonHeight = 171;
  lightButtons[ii].buttonKey    = ii;
  lightButtons[ii].pot          = yes;          /* a potentiometer */
  lightButtons[ii].mask         = potMASK;
  lightButtons[ii].textColor    = 163;
  lightButtons[ii].xHalf        = lightButtons[ii].buttonWidth/2;
  lightButtons[ii].yHalf        = lightButtons[ii].buttonHeight/2;
  ++num;

  /* Change x and y coordinate of light source */
  ii = lightMoveXY;
  lightButtons[ii].buttonX      = 20;
  lightButtons[ii].buttonY      = 247;  
  lightButtons[ii].buttonWidth  = 110;
  lightButtons[ii].buttonHeight = 110;
  lightButtons[ii].buttonKey    = ii;
  lightButtons[ii].pot          = yes;         /* a potentiometer */
  lightButtons[ii].mask         = potMASK;
  lightButtons[ii].textColor    = 133;
  lightButtons[ii].xHalf        = lightButtons[ii].buttonWidth/2;
  lightButtons[ii].yHalf        = lightButtons[ii].buttonHeight/2;
  ++num;
  
  /* Change z coordinate of light source */
  ii = lightMoveZ;
  lightButtons[ii].buttonX      = 149;
  lightButtons[ii].buttonY      = 247; 
  lightButtons[ii].buttonWidth  = 58;
  lightButtons[ii].buttonHeight = 110;
  lightButtons[ii].buttonKey    = ii;
  lightButtons[ii].pot          = yes;        /* a potentiometer */
  lightButtons[ii].mask         = potMASK;
  lightButtons[ii].textColor    = 165;
  lightButtons[ii].xHalf        = lightButtons[ii].buttonWidth/2;
  lightButtons[ii].yHalf        = lightButtons[ii].buttonHeight/2;
  ++num;
  
  /* Change intensity of light source */
  ii = lightTranslucent;
  lightButtons[ii].buttonX      = 250;
  lightButtons[ii].buttonY      = 247; 
  lightButtons[ii].buttonWidth  = 34;
  lightButtons[ii].buttonHeight = 110;
  lightButtons[ii].buttonKey    = ii;
  lightButtons[ii].pot          = yes;          /* a potentiometer */
  lightButtons[ii].mask         = potMASK;
  lightButtons[ii].textColor    = 37;
  lightButtons[ii].xHalf        = lightButtons[ii].buttonWidth/2;
  lightButtons[ii].yHalf        = lightButtons[ii].buttonHeight/2;
  ++num;

  /* Leave lighting window without updating changes made. */
  ii = lightAbort;
  lightButtons[ii].buttonX      = 36;
  lightButtons[ii].buttonY      = 370;
  lightButtons[ii].buttonWidth  = 110;
  lightButtons[ii].buttonHeight = 24;
  lightButtons[ii].buttonKey    = ii;
  lightButtons[ii].pot          = no;
  lightButtons[ii].mask         = buttonMASK;
  lightButtons[ii].text         = "Abort";
  lightButtons[ii].textColor    = 52;
  lightButtons[ii].xHalf        = lightButtons[ii].buttonWidth/2;
  lightButtons[ii].yHalf        = lightButtons[ii].buttonHeight/2;
  ++num;

  /* Leave lighting window and update changes made. */
  ii = lightReturn;
  lightButtons[ii].buttonX      = 154;
  lightButtons[ii].buttonY      = 370;
  lightButtons[ii].buttonWidth  = 110;
  lightButtons[ii].buttonHeight = 24;
  lightButtons[ii].buttonKey    = ii;
  lightButtons[ii].pot          = no;
  lightButtons[ii].mask         = buttonMASK;
  lightButtons[ii].text         = "Return";
  lightButtons[ii].textColor    = 28;
  lightButtons[ii].xHalf        = lightButtons[ii].buttonWidth/2;
  lightButtons[ii].yHalf        = lightButtons[ii].buttonHeight/2;
  ++num;

  return(num);
}


