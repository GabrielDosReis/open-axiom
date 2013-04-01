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

#define _CLOSEVIEW3D_C
#include "openaxiom-c-macros.h"

#include <stdlib.h>
#include "header.h"

#include "util.H1"
#include "Gfun.H1"
#include "all_3d.H1"



/*****************************************************
 * int closeViewport()                               *
 *                                                   *
 * This closes all of the windows created for the    *
 * control panel window and the viewport window of   *
 * the current graph being displayed.                *
 * It does not currently return a specified value.   * 
 *                                                   *
 *****************************************************/

void
closeViewport (void)
{
  int i;

  /* First, unlink viewport from global list of viewports */
  if (viewport->prevViewport) {    /* if there is a viewport before it */
    (viewport->prevViewport)->nextViewport = viewport->nextViewport;
  } else {                         /* this is the first viewport */
    viewport = viewport->nextViewport;
  }
  if (viewport->nextViewport) {    /* if there is a viewport following it */
    (viewport->nextViewport)->prevViewport = viewport->prevViewport;
  }

  /* Free up the control panel button windows */
  for (i=0; i<maxButtons3D; i++) {
    XDeleteAssoc(dsply,table,(control->buttonQueue[i]).self);
  }
  
  /* Free up the control panel window */
  XDestroyWindow(dsply,control->controlWindow);
  free(control);
  
  /* Free up the viewport window */

  XDestroyWindow(dsply,viewport->viewWindow);
  XDestroyWindow(dsply,viewport->titleWindow);
  free(viewport);
  
  XFlush(dsply);
  
} /* closeViewport() */

