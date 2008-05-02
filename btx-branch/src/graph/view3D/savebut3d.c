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

#define _SAVEBUT3D_C
#include "openaxiom-c-macros.h"

#include "header.h"
#include "cpanel.h"

#include "all_3d.H1"
int 
initSaveButtons (buttonStruct *saveButtons)
{
  int ii;
  int num = 0;

  ii = saveExit;
  saveButtons[ii].buttonX      = 5;
  saveButtons[ii].buttonY      = 7;
  saveButtons[ii].buttonWidth  = 53;
  saveButtons[ii].buttonHeight = 25;
  saveButtons[ii].buttonKey    = ii;
  saveButtons[ii].pot          = no;
  saveButtons[ii].mask         = buttonMASK;
  saveButtons[ii].text         = "Cancel";
  saveButtons[ii].textColor    = 6;
  saveButtons[ii].xHalf        = saveButtons[ii].buttonWidth/2;
  saveButtons[ii].yHalf        = saveButtons[ii].buttonHeight/2;
  ++num;

  ii = pixmap;
  saveButtons[ii].buttonX      = 5;
  saveButtons[ii].buttonY      = 41;
  saveButtons[ii].buttonWidth  = 53;
  saveButtons[ii].buttonHeight = 25;
  saveButtons[ii].buttonKey    = ii;
  saveButtons[ii].pot          = no;
  saveButtons[ii].mask         = buttonMASK;
  saveButtons[ii].text         = "Pixmap";
  saveButtons[ii].textColor    = 28;
  saveButtons[ii].xHalf        = saveButtons[ii].buttonWidth/2;
  saveButtons[ii].yHalf        = saveButtons[ii].buttonHeight/2;
  ++num;

  ii = ps;
  saveButtons[ii].buttonX              = 5;
  saveButtons[ii].buttonY              = 75;
  saveButtons[ii].buttonWidth          = 53;
  saveButtons[ii].buttonHeight         = 25;
  saveButtons[ii].buttonKey            = ii;
  saveButtons[ii].pot                  = no;
  saveButtons[ii].mask                 = buttonMASK;
  saveButtons[ii].text                 = "PS";
  saveButtons[ii].textColor            = 149;
  saveButtons[ii].xHalf                = saveButtons[ii].buttonWidth/2;
  saveButtons[ii].yHalf                = saveButtons[ii].buttonHeight/2;

  return(num);
}


