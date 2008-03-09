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

#define _STUFF2D_C
#include "axiom-c-macros.h"

#include <unistd.h>
#include <stdlib.h>

#include "header2.h"

#include "all_2d.H1"
#include "Gfun.H1"
#include "util.H1"


/**************************/
/***  float absolute(x) ***/
/**************************/

float 
#ifdef _NO_PROTO
absolute(x)
  float x;
#else
absolute(float x)
#endif
{
  if (x<0.0) {
    return(-x);
  } else {
    return(x);
  }
}



/************************/
/***  void goodbye()  ***/
/************************/

void 
#ifdef _NO_PROTO
goodbye(sig)
int sig;
#else
goodbye(int sig)
#endif
{
  int Command,i;

#ifdef DEBUG
  fprintf(stderr,"view2D: Tidying up and exiting\n");
#endif
  PSClose(); /* free PS file and data structure space */

  XFreeGC(dsply,globalGC1);
  XFreeGC(dsply,globalGC2);
  XFreeGC(dsply,globGC);
  XFreeGC(dsply,trashGC);
  XFreeGC(dsply,anotherGC);
  XFreeGC(dsply,controlMessageGC);
  XFreeGC(dsply,graphGC);
  XFreeGC(dsply,unitGC);

  XFreeFont(dsply,globalFont);
  XFreeFont(dsply,buttonFont);
  XFreeFont(dsply,headerFont);
  XFreeFont(dsply,titleFont);
  XFreeFont(dsply,graphFont);
  XFreeFont(dsply,unitFont);

  XFreeColormap(dsply,colorMap);

  /** send off the current graphs to viewport manager **/

  Command = viewportClosing;
  check(write(Socket,&Command,intSize));

  for (i=0; i<maxGraphs;i++) {
    check(write(Socket,&graphArray[i].key,intSize));
  }
  close(Socket);
  XCloseDisplay(dsply);
  exit(0);

}






