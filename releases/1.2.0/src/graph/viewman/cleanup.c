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

#define _CLEANUP_C
#include "openaxiom-c-macros.h"

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <signal.h>
#include <sys/wait.h>

#include "viewman.h"
#include "actions.h"

#include "util.H1"
#include "cleanup.H1"
#include "makeGraph.H1"
#include "readView.H1"

void
brokenPipe(int sig)
{
  fprintf(stderr,
          "The viewport manager tried to write to a non-existing pipe.\n");
}


void
endChild(int sig)
{

  checkClosedChild = yes;
}


/****************************
 * void rmViewMgr(slotPtr)  *
 *                          *
 * given a pointer to a     *
 * viewManager, this        *
 * procedure removes it     *
 * from the viewport list   *
*****************************/

void 
rmViewMgr(viewManager *slotPtr)
{
  
  int i,throwAway,code;
  viewManager *somePort, *someOtherPort;
  graphStruct *someGraph,*someOtherGraph;   /* used in discarding graphs */
  viewsWithThisGraph *someView,*someOtherView;
  
  for (somePort=someOtherPort=viewports;
       (somePort != 0) && (somePort != slotPtr);
       somePort=(someOtherPort=somePort)->nextViewport)
    {}
  assert ((somePort == 0) ||
          (somePort == viewports) ||
          (somePort == someOtherPort->nextViewport));

  if (somePort) {
    if (somePort == viewports) viewports=viewports->nextViewport;
    else someOtherPort->nextViewport = somePort->nextViewport;
  }
  /*** if view2D, then clean up after the graphs as well ***/
  if (slotPtr->viewType == view2DType) {
    for (i=0; i<maxGraphs; i++) {
      code=readViewport(slotPtr,&throwAway,intSize);  /* get the graph to discard */
      if (code == -1) break; /* read failure - give up */
      if (throwAway) {                           /* zero means no graph */

        for (someGraph = someOtherGraph = graphList;
             (someGraph != 0) && (someGraph->key != throwAway);
             someGraph=(someOtherGraph=someGraph)->nextGraph)
          {
          }
        /* someGraph is 0 if not found */
        /* someGraph == graphList if found at first */
        /* otherwise someGraph == someOtherGraph->nextGraph */
        assert( (someGraph == 0) ||
                (someGraph == graphList) ||
                (someGraph == someOtherGraph->nextGraph));

        if (someGraph) {              /* if found (should always be true) */

          for(someView=someOtherView=someGraph->views;
              (someView !=0 ) && (someView->viewGr != slotPtr);
              someView=(someOtherView=someView)->nextViewthing)
            {
            }
          /* similarly */
          assert( (someView == 0) ||
                  (someView == someGraph->views) ||
                  (someView == someOtherView->nextViewthing));

          if (someView) {     /* if found (should always be true) */
            if (someView == someGraph->views) 
              /* first */
              someGraph->views = someGraph->views->nextViewthing;
            else
              someOtherView->nextViewthing = someView->nextViewthing;
            free(someView);                    /* remove this viewport 
                                                  from list */
          }
          /* if now nothing is pointing  to this graph , remove the graph from the list*/
          if (someGraph->views == 0) {
            if (someGraph == graphList)
              graphList = graphList->nextGraph;
            else
              someOtherGraph->nextGraph = someGraph->nextGraph;
            discardGraph(someGraph);           /* free the graph */
          }
        }  /* if someGraph */
      } /* if throwAway */
    } /* for i */
  } /* if type is view2D */
  close(slotPtr->viewIn);
  close(slotPtr->viewOut);
  free(slotPtr);
} /* rmViewMgr() */


/***********************************
 * int closeChildViewport(slotPtr) *
 *                                 *
 * given a pointer to a viewport   *
 * structure (viewManager) this    *
 * procedure first waits for the   *
 * actual process to die and then  *
 * removes it from the list of     *
 * viewports via rmViewMgr().      *
 ***********************************/

void  
closeChildViewport(viewManager *slotPtr)
{
  
  int status;
  rmViewMgr(slotPtr);
  wait(&status); 
  
}  /* closeChildViewport */


/*********************
 * int goodbye()     *
 *                   *
 * kill all children *
 * (how mean) and    *
 * then kill self.   *
 *********************/

void 
goodbye(int sig)
{

  viewManager *v;

  v = viewports;
  while (v) {
    kill(v->PID,SIGTERM);
    while (wait(NULL) == -1);
    v = v->nextViewport;
  }
  exit(0);

}




