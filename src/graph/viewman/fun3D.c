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

#define _FUN3D_C

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include "open-axiom.h"
#include "viewman.h"
#include "mode.h"
#include "actions.h"
  
#include "util.H1"
#include "sockio.h"
#include "fun3D.H1"
#include "make3D.H1"
#include "readView.H1"

void 
funView3D(int viewCommand)
{

  int code;
  int viewPID;
  float f1,f2,f3,f4;
  int i1,i2;
  viewManager *viewport;

  viewPID = get_int(spadSock);

  viewport = viewports;
  while ((viewport) && (viewport->PID != viewPID)) 
    viewport = viewport->nextViewport;

  if (viewport) {
    send_int(spadSock,1);  /* acknowledge to spad */

    viewmanEvent.xclient.window = viewport->viewWindow;

    code = write(viewport->viewOut,&viewCommand,intSize);
    switch (viewCommand) {
    case rotate:
      f1 = get_float(spadSock);
      f2 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      code = write(viewport->viewOut,&f2,floatSize);
      break;

    case zoom:
      f1 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      break;

    case zoomx:
      f1 = get_float(spadSock);
      f2 = get_float(spadSock);
      f3 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      code = write(viewport->viewOut,&f2,floatSize);
      code = write(viewport->viewOut,&f3,floatSize);
      break;

    case translate:
      f1 = get_float(spadSock);
      f2 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      code = write(viewport->viewOut,&f2,floatSize);
      break;

    case modifyPOINT:
      i1 = get_int(spadSock);
      f1 = get_float(spadSock);
      f2 = get_float(spadSock);
      f3 = get_float(spadSock);
      f4 = get_float(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      code = write(viewport->viewOut,&f1,floatSize);
      code = write(viewport->viewOut,&f2,floatSize);
      code = write(viewport->viewOut,&f3,floatSize);
      code = write(viewport->viewOut,&f4,floatSize);
      break;

    case hideControl:
      i1 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      break;

    case axesOnOff:
    case perspectiveOnOff:
    case region3D:
    case clipRegionOnOff:
    case clipSurfaceOnOff:
      i1 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      break;

    case eyeDistanceData:
    case hitherPlaneData:
      f1 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      break;

    case colorDef:
      i1 = get_int(spadSock);
      i2 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      code = write(viewport->viewOut,&i2,intSize);
      break;

    case moveViewport:
      i1 = get_int(spadSock);
      i2 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      code = write(viewport->viewOut,&i2,intSize);
      break;

    case resizeViewport:
      i1 = get_int(spadSock);
      i2 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      code = write(viewport->viewOut,&i2,intSize);
      break;

    case transparent:
    case opaqueMesh:
    case render:
      break;

    case lightDef:
      f1 = get_float(spadSock);
      f2 = get_float(spadSock);
      f3 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      code = write(viewport->viewOut,&f2,floatSize);
      code = write(viewport->viewOut,&f3,floatSize);
      break;

    case translucenceDef:
      f1 = get_float(spadSock);
      code = write(viewport->viewOut,&f1,floatSize);
      break;


    case changeTitle:
      s1 = get_string(spadSock);
      i1 = strlen(s1);
      code = write(viewport->viewOut,&i1,intSize);
      code = write(viewport->viewOut,s1,i1);
      break;

    case writeView:
      s1 = get_string(spadSock);
      i1 = strlen(s1);
      code = write(viewport->viewOut,&i1,intSize);
      code = write(viewport->viewOut,s1,i1);
      /* write out the types of things to be written */
      i2 = get_int(spadSock);
      code = write(viewport->viewOut,&i2,intSize);
      while (i2) {
        i2 = get_int(spadSock);
        code = write(viewport->viewOut,&i2,intSize);
      }
      break;

    case diagOnOff:
      i1 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      break;

    case outlineOnOff:
      i1 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      break;

    case spadPressedAButton:
      i1 = get_int(spadSock);
      code = write(viewport->viewOut,&i1,intSize);
      break;
    }  /* switch */
    /*** get acknowledge from viewport */

    code = readViewport(viewport,&acknow,intSize);
    send_int(spadSock,1);  /* acknowledge to spad */
  } else {  /* if (viewport) */
    send_int(spadSock,-1);  /* send error value in acknowledge to spad */
  }

}
void
forkView3D(int typeOfViewport)
{

  viewManager *viewport;
  int         childPID, code;
  int         i;

  view3DStruct doView3D;
  int  pipe0[2],pipe1[2];
  int *anIndex;

  char envAXIOM[100],runView[100];
  int j,k;
  LLPoint *anLLPoint;
  LPoint *anLPoint;

#ifdef DEBUG
  fprintf(stderr,"Pipe calls for 3D\n");
#endif
  check(pipe(pipe0));
  check(pipe(pipe1));

#ifdef DEBUG
  fprintf(stderr,"Fork routine for 3D\n");
#endif
  switch(childPID = check(fork())) {

  case -1:
    printf("Cannot create a new process - you probably have too many things running already.\n");
    return;

  case 0:
    /*****************************
     *       child process       *
     *****************************/
        /* map pipes from viewport manager to standard input and output */
#ifdef DEBUG
    fprintf(stderr,"Mapping pipes to standard I/O in 3D\n");
#endif
    check(dup2(pipe0[0],0));
    check(dup2(pipe1[1],1));
    close(pipe0[0]);
    close(pipe0[1]);
    close(pipe1[0]);
    close(pipe1[1]);

#ifdef DEBUG
    fprintf(stderr,"Executing ThreeDimensionalViewport process\n");
#endif
    sprintf(envAXIOM,"%s",getenv("AXIOM"));
    sprintf(runView,"%s%s",envAXIOM,"/lib/view3D");
    check(execl(runView,runView,NULL));
    fprintf(stderr,"The viewport manager could not execute view3D.\nCheck that view3D is on your PATH.\n");
    exit(-1);

  default:
    /******************************
     *       parent process       *
     ******************************/
    if (!(viewport = (viewManager *)malloc(sizeof(viewManager)))) {
      printf("Ran out of memory trying to create a new viewport process.\n");
      return;
    }
    viewport->viewType = typeOfViewport;
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
              "The viewport manager could not create connection to a 3D viewport window. Try again.\n");
      return;
    } else {

      code = readViewport(viewport,&acknow,intSize);

      if (code < 0) {
        fprintf(stderr,
              "The viewport manager could not read from a 3D viewport window\ncode=%d\nack=%d\n",code,acknow);
        return;
      }
    } 

    makeView3DFromSpadData(&doView3D,typeOfViewport);

      /* tell the child that parent is a viewport manager */
    i = no;
    write(viewport->viewOut,&i,sizeof(int));

    write(viewport->viewOut,&doView3D,sizeof(view3DStruct));

    i = strlen(doView3D.title)+1;
    write(viewport->viewOut,&i,intSize);        /* tell the length of the title to child */
    write(viewport->viewOut,doView3D.title,i);  /* tell the title to the child */
    write(viewport->viewOut,&(doView3D.lightVec[0]),floatSize);
    write(viewport->viewOut,&(doView3D.lightVec[1]),floatSize);
    write(viewport->viewOut,&(doView3D.lightVec[2]),floatSize);

    /* send generalized 3D components */
    write(viewport->viewOut,&(doView3D.numOfPoints),intSize);
    for (i=0; i<doView3D.numOfPoints; i++) {
      write(viewport->viewOut,&(refPt(doView3D,i)->x),floatSize);
      write(viewport->viewOut,&(refPt(doView3D,i)->y),floatSize);
      write(viewport->viewOut,&(refPt(doView3D,i)->z),floatSize);
      write(viewport->viewOut,&(refPt(doView3D,i)->c),floatSize);
    }
    write(viewport->viewOut,&(doView3D.lllp.numOfComponents),intSize);
    anLLPoint = doView3D.lllp.llp;
    for (i=0; i<doView3D.lllp.numOfComponents; i++,anLLPoint++) {
      write(viewport->viewOut,&(anLLPoint->prop.closed),intSize);
      write(viewport->viewOut,&(anLLPoint->prop.solid),intSize);
      write(viewport->viewOut,&(anLLPoint->numOfLists),intSize);
      anLPoint = anLLPoint->lp;
      for (j=0; j<anLLPoint->numOfLists; j++,anLPoint++) {
        write(viewport->viewOut,&(anLPoint->prop.closed),intSize);
        write(viewport->viewOut,&(anLPoint->prop.solid),intSize);
        write(viewport->viewOut,&(anLPoint->numOfPoints),intSize);
        anIndex = anLPoint->indices;
        for (k=0; k<anLPoint->numOfPoints; k++,anIndex++)
          write(viewport->viewOut,anIndex,intSize);
      } /* for LPoints in LLPoints (j) */
    } /* for LLPoints in LLLPoints (i) */
    
         /*** get acknowledge from viewport */
    code = readViewport(viewport,&(viewport->viewWindow),sizeof(Window)); 
    openaxiom_sleep(1);  /* wait a second...*/
    send_int(spadSock,viewport->PID);  /* acknowledge to spad */

  }   /* switch */

}    /* forkView3D() */


