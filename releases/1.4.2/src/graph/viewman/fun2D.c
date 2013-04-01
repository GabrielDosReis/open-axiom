/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2013, Gabriel Dos Reis.
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

#define _FUN2D_C

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>


#include "open-axiom.h"
#include "viewman.h"
#include "actions.h"

#include "util.H1"
#include "sockio.h"
#include "fun2D.H1"
#include "readView.H1"
#include "make2D.H1"
#include "cfuns.h"

using namespace OpenAxiom;

#define writeEach

void 
funView2D(int viewCommand)
{

  int viewPID;
  float f1,f2;
  int i1,i2,i3;
  viewManager *viewport;

  viewPID = get_int(spadSock);


  viewport = viewports;
  while ((viewport) && (viewport->PID != viewPID)) {
    viewport = viewport->nextViewport;
  }
  
  if (viewport) {
    send_int(spadSock,1);  /* acknowledge to spad */
    write(viewport->viewOut,&viewCommand,intSize);


    switch (viewCommand) {

    case putGraph:
      i1 = get_int(spadSock);  /* graph key */
      i2 = get_int(spadSock);  /* viewport slot 1..9 */
      i2--; /* 0..8*/
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,&i2,intSize);
      i3 = 1; /* continue*/
      write(viewport->viewOut,&i3,intSize);      
      sendGraphToView2D(0,i1,viewport,&currentGraphState);
     
      break;

    case translate2D:
      i1 = get_int(spadSock);   /* graph index */
      f1 = get_float(spadSock); /* translate in the x direction */
      f2 = get_float(spadSock); /* translate in the y direction */
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,&f1,floatSize);
      write(viewport->viewOut,&f2,floatSize);
      break;

    case scale2D:
      i1 = get_int(spadSock);   /* graph index */
      f1 = get_float(spadSock); /* scale in the x direction */
      f2 = get_float(spadSock); /* scale in the y direction */
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,&f1,floatSize);
      write(viewport->viewOut,&f2,floatSize);
      break;

    case hideControl2D:
      i1 = get_int(spadSock);
      write(viewport->viewOut,&i1,intSize);
      break;

    case axesOnOff2D:
    case unitsOnOff2D:
    case connectOnOff:
    case pointsOnOff:
    case spline2D:
    case showing2D:
      i1 = get_int(spadSock);   /* graph index */
      i2 = get_int(spadSock);   /* axes status */
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,&i2,intSize);
      break;

    case moveViewport:
    case resizeViewport:
      i1 = get_int(spadSock);
      i2 = get_int(spadSock);
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,&i2,intSize);
      break;

    case changeTitle:
      s1 = get_string(spadSock);
      i1 = strlen(s1);
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,s1,i1);
      break;

    case writeView:
      s1 = get_string(spadSock);
      i1 = strlen(s1);
      write(viewport->viewOut,&i1,intSize);
      write(viewport->viewOut,s1,i1);
        /* write out the types of things to be written */
      i2 = get_int(spadSock);
      write(viewport->viewOut,&i2,intSize);
      while (i2) {
        i2 = get_int(spadSock);
        write(viewport->viewOut,&i2,intSize);
      }
      break;

    case spadPressedAButton:
      i1 = get_int(spadSock);
      write(viewport->viewOut,&i1,intSize);
      break;

    }  /* switch */
         /*** get acknowledge from viewport */
    readViewport(viewport,&acknow,intSize);
    send_int(spadSock,1);  /* acknowledge to spad */
  } else {  
    send_int(spadSock,-1);  /* send error value in acknowledge to spad */
  }

}

void 
forkView2D(void)
{

  viewManager      *viewport;
  int              childPID, code;
  int              i;
  view2DStruct     doView2D;
  graphStateStruct doGraphStateArray[maxGraphs];
  int              there;
  int  pipe0[2], pipe1[2];

  char envAXIOM[100],runView[100];

#ifdef DEBUG
  fprintf(stderr,"fun2D:Pipe calls for 2D\n");
#endif
  check(pipe(pipe0));
  check(pipe(pipe1));

#ifdef DEBUG
  fprintf(stderr,"Fork routine for 2D\n");
#endif
  childPID = check(fork());

  switch(childPID) {

  case -1:
    fprintf(stderr,
            "The viewport manager cannot open a viewport window.\nTry closing some viewports.\n");
    return;

  case 0:
    /*****************************
     *       child process       *
     *****************************/
        /* map pipes from viewport manager to standard input and output */
#ifdef DEBUG
    fprintf(stderr,"Mapping pipes to standard I/O in 2D\n");
#endif
    check(dup2(pipe0[0],0));
    check(dup2(pipe1[1],1));
    close(pipe0[0]);
    close(pipe0[1]);
    close(pipe1[0]);
    close(pipe1[1]);

#ifdef DEBUG
    fprintf(stderr,"Executing TwoDimensionalViewport process\n");
#endif
    sprintf(envAXIOM,"%s",oa_getenv("AXIOM"));
    sprintf(runView,"%s%s",envAXIOM,"/lib/view2D");
    check(execl(runView,runView, (char*) NULL));
    fprintf(stderr,"The viewport manager could not execute view2D.\nCheck that view2D is on your PATH.\n");
    exit(-1);

  default:
    /******************************
     *       parent process       *
     ******************************/
    if (!(viewport = (viewManager *)malloc(sizeof(viewManager)))) {
      fprintf(stderr,"The viewport manager ran out of memory trying to create a new viewport window (viewManager).\n");
      return;
    }
    viewport->viewType = view2DType;
    viewport->PID = childPID;

         /* set up pipes to child process */
    close(pipe0[0]);
    close(pipe1[1]);
    viewport->viewIn  = pipe1[0];
    viewport->viewOut = pipe0[1];

         /* add new viewport to global list */
    viewport->nextViewport = viewports;
    viewports = viewport;

    if (viewport->viewIn <0) {
      fprintf(stderr,
 "viewman could not create connection to a 2D viewport window. Try again.\n");
      return;
    } else {

      code = readViewport(viewport,&acknow,intSize);

      if (code < 0) {
        fprintf(stderr,
         "viewman could not read from a 2D viewport window\ncode=%d\nack=%d\n",
         code,acknow);
        return;
      }
    } 

    makeView2DFromSpadData(&doView2D,doGraphStateArray);

      /* tell the child that mother is a viewport manager */
    i = no;
    write(viewport->viewOut,&i,sizeof(int));

    write(viewport->viewOut,&doView2D,sizeof(view2DStruct));
    i = strlen(doView2D.title)+1;
    write(viewport->viewOut,&i,intSize); /* send length of the title child */
    write(viewport->viewOut,doView2D.title,i);  /* send title to the child */
    for (i=0; i<maxGraphs; i++) {
      there = doView2D.graphKeyArray[i];
      write(viewport->viewOut,&there,intSize);
      sendGraphToView2D(i,there,viewport,doGraphStateArray);
    };  /* for i in graphs */
    
         /*** get acknowledge from viewport */

    code = readViewport(viewport,&(viewport->viewWindow),sizeof(Window));
    openaxiom_sleep(1);  /* wait a second...*/
    send_int(spadSock,viewport->PID);  /* acknowledge to spad */

  }   /* switch */

}    /* forkView2D() */



void
sendGraphToView2D(int i,int there,viewManager *viewport,graphStateStruct *doGraphStateArray)
{

  graphStruct      *gPtr;
  pointListStruct  *llPtr;
  pointStruct      *p;
  viewsWithThisGraph *oneView;
  int j,k;

  if (there) {
    gPtr = graphList;
    while ( gPtr != NULL  &&   gPtr->key != there)    /** find the right graph (same key) in graph list **/
      gPtr = gPtr->nextGraph;
    if ((gPtr==NULL) ||(gPtr->key != there) ){
      fprintf(stderr,
              "The viewport manager cannot find the requested graph and will quit and restart.\n");
      exit(-1);
    } 
    

/*** Before sending off the data, insert a pointer to viewport from graph ***/
  if (!(oneView = (viewsWithThisGraph *)malloc(sizeof(viewsWithThisGraph)))) {
    fprintf(stderr,"The viewport manager ran out of memory trying to create a new graph (viewsWithThisGraph).\n");
    return;
  }
  oneView->viewGr          = viewport;
  oneView->nextViewthing   = gPtr->views;
  gPtr->views              = oneView;

#ifdef writeEach    
    write(viewport->viewOut,&(gPtr->xmin),floatSize);
    write(viewport->viewOut,&(gPtr->xmax),floatSize);
    write(viewport->viewOut,&(gPtr->ymin),floatSize);
    write(viewport->viewOut,&(gPtr->ymax),floatSize);
    write(viewport->viewOut,&(gPtr->xNorm),floatSize);
    write(viewport->viewOut,&(gPtr->yNorm),floatSize);
    write(viewport->viewOut,&(gPtr->spadUnitX),floatSize);
    write(viewport->viewOut,&(gPtr->spadUnitY),floatSize);
    write(viewport->viewOut,&(gPtr->unitX),floatSize);
    write(viewport->viewOut,&(gPtr->unitY),floatSize);
    write(viewport->viewOut,&(gPtr->originX),floatSize);
    write(viewport->viewOut,&(gPtr->originY),floatSize);
    write(viewport->viewOut,&(gPtr->numberOfLists),intSize);
#else
    write(viewport->viewOut,gPtr,sizeof(graphStruct));
#endif
    
    llPtr = gPtr->listOfListsOfPoints;
    for (j=0; j<(gPtr->numberOfLists); j++) {
      write(viewport->viewOut,&(llPtr->numberOfPoints),intSize);
      p = llPtr->listOfPoints;
      for (k=0; k<(llPtr->numberOfPoints); k++) {
        write(viewport->viewOut,&(p->x),floatSize);
        write(viewport->viewOut,&(p->y),floatSize);
        write(viewport->viewOut,&(p->hue),floatSize);
        write(viewport->viewOut,&(p->shade),floatSize);
        p++;
      }    /* for k in list of points */
      write(viewport->viewOut,&(llPtr->pointColor),intSize);
      write(viewport->viewOut,&(llPtr->lineColor),intSize);
      write(viewport->viewOut,&(llPtr->pointSize),intSize);
      llPtr++;
    }   /* for j in list of lists of points */
    
    /* a graph state is defined for a graph if graph is there */
    write(viewport->viewOut,&(doGraphStateArray[i].scaleX),floatSize);    
    write(viewport->viewOut,&(doGraphStateArray[i].scaleY),floatSize);
    write(viewport->viewOut,&(doGraphStateArray[i].deltaX),floatSize);
    write(viewport->viewOut,&(doGraphStateArray[i].deltaY),floatSize);
    write(viewport->viewOut,&(doGraphStateArray[i].pointsOn),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].connectOn),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].splineOn),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].axesOn),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].axesColor),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].unitsOn),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].unitsColor),intSize);
    write(viewport->viewOut,&(doGraphStateArray[i].showing),intSize);

  }   /* if graph is there */

}

