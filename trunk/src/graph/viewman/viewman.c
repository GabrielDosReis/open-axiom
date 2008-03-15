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

#define _VIEWMAN_C
#include "axiom-c-macros.h"

#include <unistd.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#ifdef SGIplatform
#include <bstring.h>
#endif

#include "viewman.h"
#include "mode.h"
#include "actions.h"
#include "viewCommand.h"
#include "bsdsignal.h"


#include "util.H1"
#include "sockio.h"
#include "fun2D.H1"
#include "fun3D.H1"
#include "makeGraph.H1"
#include "readView.H1"
#include "cleanup.H1"
#include "sselect.H1"

/************* global variables **************/

Display *dsply;
Window  root;
XEvent  viewmanEvent;
viewManager *viewports,
  *slot,
  *stepSlot;
Sock        *spadSock;
int         viewType, 
  viewCommand,
  acknow,
  graphKey = 1,
  defDsply,
  currentGraph,
  picked = no,
  viewOkay  = 0,
  viewError = -1,
  checkClosedChild = no,
  foundBrokenPipe = no;
fd_set      filedes;
graphStruct *graphList;
graphStateStruct currentGraphState;
char        *s1,
  propertyBuffer[256];/* XProperty buffer */


int
#ifdef _NO_PROTO
main ()
#else
main (void)
#endif
{

  graphStruct *aGraph;
  int keepLooking,code;
  
  bsdSignal(SIGPIPE,brokenPipe,DontRestartSystemCalls);
#if defined(BSDplatform) || defined (MACOSXplatform)
  bsdSignal(SIGCHLD,endChild,RestartSystemCalls);
#else
  bsdSignal(SIGCLD,endChild,RestartSystemCalls);
#endif
  bsdSignal(SIGTERM,goodbye,DontRestartSystemCalls);
  
  /* Connect up to AXIOM server */
  spadSock = connect_to_local_server(SpadServer,ViewportServer,Forever);
  if (spadSock == NULL) {
    fprintf(stderr,"The viewport manager couldn't connect to AXIOM\n");
    exit(-1);
  }
#ifdef DEBUG
  else
    fprintf(stderr,"viewman: Connected to AXIOM\n");
#endif
  
  /******** initialize ********/
  viewports = 0;
  graphList = 0;
  
  /******** getting stuff from spad and viewports ********
  *********   the viewports have priority over    ****
  ***   AXIOM.                              ***/
  while (1) {
    FD_ZERO(&filedes); /* zero out file descriptor */
    FD_SET(spadSock->socket,&filedes);
    slot = viewports;
    while (slot) {
      FD_SET(slot->viewIn,&filedes);
      slot = slot->nextViewport;
    }
    
#ifdef DEBUG
    fprintf(stderr,"Selection for filedes of %x \n",filedes);
#endif
    code = check(superSelect(FD_SETSIZE,(void *) &filedes,0,0,0));
    for (;code<=0;) 
      code = check(superSelect(FD_SETSIZE,(void *)&filedes,0,0,0));
    
    slot = viewports;
    keepLooking = 1;
    while (keepLooking && slot) {
      if (FD_ISSET(slot->viewIn,&filedes)) {
        keepLooking = 0;
#ifdef DEBUG
        fprintf(stderr,"Reading child viewport...\n");
#endif
        readViewport(slot,&viewCommand,intSize);
        
        switch (viewCommand) {
          
        case pick2D:
#ifdef DEBUG
          fprintf(stderr,"viewman: Doing 2D pick\n");
#endif
          picked = yes;
          
          readViewport(slot,&currentGraph,intSize); /* get the graph to pick */
          readViewport(slot,&currentGraphState,sizeof(graphStateStruct));
          break;
          
        case drop2D:
#ifdef DEBUG
          fprintf(stderr,"viewman: Doing 2D drop\n");
#endif
          if (picked) {
            write(slot->viewOut,&viewOkay,intSize);
            write(slot->viewOut,&currentGraph,intSize);
            sendGraphToView2D(0,currentGraph,slot,&currentGraphState);
          } else {
            write(slot->viewOut,&viewError,intSize);
            fprintf(stderr,"The viewport manager cannot drop a graph because nothing has been picked yet.\n");
          }
          break;
          
        case viewportClosing:
#ifdef DEBUG
          fprintf(stderr,"viewman: closing viewport\n");
#endif
          closeChildViewport(slot);
          break;

        };  /* switch */
        
      };  /* if reading slot->viewIn */
      stepSlot = slot;
      slot = slot->nextViewport;
    };  /* while */
    
    if (keepLooking) {   /* if  1 => slots not read, read from spad */
#ifdef DEBUG
      fprintf(stderr,"viewman: still looking\n");
#endif
      viewType = get_int(spadSock);
      if (viewType == -1) goodbye(-1);
      viewCommand = get_int(spadSock);
      
      switch (viewType) {
        
      case view3DType:
#ifdef DEBUG
        fprintf(stderr,"viewman: making 3D viewport\n");
#endif
        if (viewCommand == makeViewport) 
          forkView3D(view3DType);
        else 
          funView3D(viewCommand);
        
        break;
        
      case viewTubeType:
#ifdef DEBUG
        fprintf(stderr,"viewman: viewing a tube\n");
#endif
        if (viewCommand == makeViewport) 
          forkView3D(viewTubeType);
        else 
          funView3D(viewCommand);
        
        break;
        
      case viewGraphType:
#ifdef DEBUG
        fprintf(stderr,"viewman: making a graph\n");
#endif
        if (viewCommand == makeGraph) {
          aGraph            = makeGraphFromSpadData();
          aGraph->nextGraph = graphList;
          graphList         = aGraph;
        }
        break;
        
      case view2DType:
#ifdef DEBUG
        fprintf(stderr,"viewman: forking 2D\n");
#endif
        if (viewCommand == makeViewport) {
          forkView2D();
        } else {
          funView2D(viewCommand);
        }
        break;
        
      }   /* switch on viewType */
    }   /* if (keepLooking) */ 
  }   /* while (1) */
}



