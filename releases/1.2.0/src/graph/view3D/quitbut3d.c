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

#define _QUITBUT3D_C
#include "openaxiom-c-macros.h"

#include "header.h"
#include "cpanel.h"

#include "all_3d.H1"



int 
initQuitButtons (buttonStruct *quitButtons)
{
  int ii;
  int num = 0;

  ii = quitAbort;
  quitButtons[ii].buttonX      = 5;
  quitButtons[ii].buttonY      = 41;
  quitButtons[ii].buttonWidth  = 53;
  quitButtons[ii].buttonHeight = 25;
  quitButtons[ii].buttonKey    = ii;
  quitButtons[ii].pot          = no;
  quitButtons[ii].mask         = buttonMASK;
  quitButtons[ii].text         = "No";
  quitButtons[ii].textColor    = 6;
  quitButtons[ii].xHalf        = quitButtons[ii].buttonWidth/2;
  quitButtons[ii].yHalf        = quitButtons[ii].buttonHeight/2;
  ++num;

  ii = quitReturn;
  quitButtons[ii].buttonX      = 5;
  quitButtons[ii].buttonY      = 75;
  quitButtons[ii].buttonWidth  = 53;
  quitButtons[ii].buttonHeight = 25;
  quitButtons[ii].buttonKey    = ii;
  quitButtons[ii].pot          = no;
  quitButtons[ii].mask         = buttonMASK;
  quitButtons[ii].text         = "Yes";
  quitButtons[ii].textColor    = onColor;
  quitButtons[ii].xHalf        = quitButtons[ii].buttonWidth/2;
  quitButtons[ii].yHalf        = quitButtons[ii].buttonHeight/2;
  ++num;

  return(num);
}


