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

#define _SSELECT_C
#include "axiom-c-macros.h"

#include <stdio.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/wait.h>

#include "viewman.h"
#include "bsdsignal.h"

#include "sockio.h"
#include "cleanup.H1"
#include "sselect.H1"

/*******************************************
 * int superSelect(n, rd, wr, ex, timeout) *
 *                                         *
 * superselect! if select returns a -1     *
 * due to an interrupt (EINTR), this       *
 * routine checks to see if it's a         *
 * child viewport that has closed.         *
 * Expected global variables:              *
 *   int  checkClosedChild                 *
 *******************************************/
 
int 
superSelect(int n, int *rd, int *wr, int *ex, char *timeout)
{
  
  int waiting;
  viewManager *viewport;
  int ret_val;
  
  ret_val = select(n, (void *)rd, (void *)wr, (void *)ex, (void *)timeout);
  while (ret_val == -1 && errno == EINTR) {
    /* checkClosedChild gets set by the SIGCHLD handler */
    if (checkClosedChild) {
      while ((waiting = wait(0)) == -1 );
      viewport = viewports;
      while ((viewport) && (viewport->PID != waiting))
        viewport = viewport->nextViewport;
      if (viewport) {
        /* we shouldn't really be doing this since child is dead */
        /* rmViewMgr(viewport); */
        /* flush(spadSock); */
        /* send_int(spadSock,1);   acknowledge to spad */
        checkClosedChild = no;
        bsdSignal(OPENAXIOM_SIGCHLD,endChild,DontRestartSystemCalls);
      }
    }
    ret_val = select(n, (void *)rd, (void *)wr, (void *)ex, (void *)timeout);
  }
  return ret_val;
}
