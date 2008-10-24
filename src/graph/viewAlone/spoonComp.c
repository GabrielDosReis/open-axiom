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

#define _SPOONCOMP_C

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "open-axiom.h"
#include "viewAlone.h"
#include "mode.h"

#include "util.H1"
#include "all_alone.H1"



/* This file forks a child process and exits the parent. It 
   has the same general form as ../viewman/funView3D() and so 
   changes there may require similar changes here. */

void 
spoonView3D(int type)
{
  
  int  i,j,k,code,pipe0[2],pipe1[2];
  char envAXIOM[100],runView[100];
  LLPoint *anLLPoint;
  LPoint *anLPoint;
  int *anIndex;
  
  sprintf(errorStr,"%s","creating pipes");
  check(pipe(pipe0));
  check(pipe(pipe1));
  switch(fork()) {
  case -1:
    fprintf(stderr,"can't create a child process\n");
    fprintf(stderr,"you may have too many processes running\n");
    exit(-1);
  case 0:
    /*  Child  */
    sprintf(errorStr,"%s",
            "(viewAlone) mapping of pipes to standard I/O for view3D");
    check(dup2(pipe0[0],0));     
    check(dup2(pipe1[1],1));
    close(pipe0[0]);
    close(pipe0[1]);
    close(pipe1[0]);
    close(pipe1[1]);
    
    sprintf(errorStr,"%s",
            "(viewAlone) execution of the ThreeDimensionalViewport process");
    sprintf(envAXIOM,"%s",oa_getenv("AXIOM"));
    sprintf(runView,"%s%s",envAXIOM,"/lib/view3D");
    check(execl(runView,runView,NULL));
    fprintf(stderr,"Could not execute view3D!\n");
    exit(-1);
  default:
    /*  Parent  */
    
    viewP.viewType = type;
    
    /* set up pipes to child process */
    close(pipe0[0]);
    close(pipe1[1]);
    viewP.viewIn  = pipe1[0];
    viewP.viewOut = pipe0[1];
    
    if (viewP.viewIn <0) {
      fprintf(stderr,
              "can't set up pipes to viewport process. Try again.\n");
      return;
    } else {
      code = read(viewP.viewIn,&ack,intSize);
      if (code < 0) {
        fprintf(stderr,"can't read from viewport process pipe. Try again.\n");
        return;
      }
    } 
    
    makeView3DFromFileData(type);
    /* tell child it is to be a stand alone program */
    i = yes;
    fprintf(stderr,"  Transmitting data to viewport...\n");
    write(viewP.viewOut,&i,intSize);
    write(viewP.viewOut,&doView3D,sizeof(view3DStruct));
    i = strlen(doView3D.title)+1;
    write(viewP.viewOut,&i,intSize);  /* tell the length of 
                                         the title to child */
    write(viewP.viewOut,doView3D.title,i);  /* tell the title 
                                               to the child, child */
    write(viewP.viewOut,&(doView3D.lightVec[0]),floatSize);
    write(viewP.viewOut,&(doView3D.lightVec[1]),floatSize);
    write(viewP.viewOut,&(doView3D.lightVec[2]),floatSize);
    
    write(viewP.viewOut,&(doView3D.numOfPoints),intSize);
    for (i=0; i<doView3D.numOfPoints; i++) {
      write(viewP.viewOut,&(refPt(doView3D,i)->x),floatSize);
      write(viewP.viewOut,&(refPt(doView3D,i)->y),floatSize);
      write(viewP.viewOut,&(refPt(doView3D,i)->z),floatSize);
      write(viewP.viewOut,&(refPt(doView3D,i)->c),floatSize);
    }
    
    /* send generalized 3D components */
    write(viewP.viewOut,&(doView3D.lllp.numOfComponents),intSize);
    anLLPoint = doView3D.lllp.llp;
    for (i=0; i<doView3D.lllp.numOfComponents; i++,anLLPoint++) {
      write(viewP.viewOut,&(anLLPoint->prop.closed),intSize);
      write(viewP.viewOut,&(anLLPoint->prop.solid),intSize);
      write(viewP.viewOut,&(anLLPoint->numOfLists),intSize);
      anLPoint = anLLPoint->lp;
      for (j=0; j<anLLPoint->numOfLists; j++,anLPoint++) {
        write(viewP.viewOut,&(anLPoint->prop.closed),intSize);
        write(viewP.viewOut,&(anLPoint->prop.solid),intSize);
        write(viewP.viewOut,&(anLPoint->numOfPoints),intSize);
        anIndex = anLPoint->indices;
        for (k=0; k<anLPoint->numOfPoints; k++,anIndex++)
          write(viewP.viewOut,anIndex,intSize);
      } /* for LPoints in LLPoints (j) */
    } /* for LLPoints in LLLPoints (i) */
    fprintf(stderr,"    Done.\n");
    
    /*** get acknowledge from viewport */
    code = read(viewP.viewIn,&(viewP.viewWindow),sizeof(Window));
    openaxiom_sleep(1);  /* wait a second...*/
    exit(0); 
    
  }   /* switch */
  
}    /* spoonView3D() */


void
makeView3DFromFileData(int type)
{
  
  int i,j,k;
  char title[256];
  LLPoint *anLLPoint;
  LPoint *anLPoint;
  viewTriple *aPoint;
  int *anIndex;

  /* fscanf(doView3D,""); */
  /* read in the view3DStruct stuff */
  /* &view3DType already read */
  doView3D.typeOf3D = type;
  fscanf(viewFile,"%f %f %f %f %f %f\n",
         &(doView3D.xmin),
         &(doView3D.xmax),
         &(doView3D.ymin),
         &(doView3D.ymax),
         &(doView3D.zmin),
         &(doView3D.zmax));

  fgets(title,256,viewFile);
  if (!(doView3D.title = (char *)malloc((strlen(title)+1) * 
                                        sizeof(char)))) {
    fprintf(stderr,"Ran out of memory (malloc) trying to get the title.\n");
    exit(-1);
  }
  sprintf(doView3D.title,"%s",title);
  /* put in a null terminator over the newline that the fgets reads */
  doView3D.title[strlen(doView3D.title)-1] = '\0'; 
  
  fscanf(viewFile,"%f %f %f %f %f %f %f %f\n",
         &(doView3D.deltaX),
         &(doView3D.deltaY),
         &(doView3D.scale),
         &(doView3D.scaleX),
         &(doView3D.scaleY),
         &(doView3D.scaleZ),
         &(doView3D.theta),
         &(doView3D.phi));
  
  fscanf(viewFile,"%d %d %d %d\n",
         &(doView3D.vX),
         &(doView3D.vY),
         &(doView3D.vW),
         &(doView3D.vH));
  fscanf(viewFile,"%d %d %d %d %d %d %d\n",
         &(doView3D.showCP),
         &(doView3D.style),
         &(doView3D.AxesOn),
         &(doView3D.hueOff),
         &(doView3D.numOfHues),
         &(doView3D.diagonals),
         &(doView3D.outlineRenderOn));
  fscanf(viewFile,"%f %f %f %f\n",
         &(doView3D.lightVec[0]),
         &(doView3D.lightVec[1]),
         &(doView3D.lightVec[2]),
         &(doView3D.translucency));
  fscanf(viewFile,"%d %f\n",
         &(doView3D.perspective),
         &(doView3D.eyeDistance));
  
  /* get generalized 3D components */
  
  fscanf(viewFile,"%d\n",
         &(doView3D.numOfPoints));
  aPoint = doView3D.points = (viewTriple *)malloc(doView3D.numOfPoints*
                                                  sizeof(viewTriple));
  for (i=0; i<doView3D.numOfPoints; i++, aPoint++)
    fscanf(viewFile,"%g %g %g %g\n",
           &(aPoint->x),
           &(aPoint->y),
           &(aPoint->z),
           &(aPoint->c));
  fscanf(viewFile,"%d\n",
         &(doView3D.lllp.numOfComponents));
  anLLPoint = doView3D.lllp.llp = 
    (LLPoint *)malloc(doView3D.lllp.numOfComponents*sizeof(LLPoint));
  for (i=0; i<doView3D.lllp.numOfComponents; i++,anLLPoint++) {
    fscanf(viewFile,"%d %d\n",
           &(anLLPoint->prop.closed),
           &(anLLPoint->prop.solid));
    fscanf(viewFile,"%d\n",
           &(anLLPoint->numOfLists));
    anLPoint = anLLPoint->lp = 
      (LPoint *)malloc(anLLPoint->numOfLists*sizeof(LPoint));
    for (j=0; j<anLLPoint->numOfLists; j++,anLPoint++) {
      fscanf(viewFile,"%d %d\n",
             &(anLPoint->prop.closed),
             &(anLPoint->prop.solid));
      fscanf(viewFile,"%d\n",
             &(anLPoint->numOfPoints));
      anIndex = anLPoint->indices =
        (int *)malloc(anLPoint->numOfPoints*sizeof(int));
      for (k=0; k<anLPoint->numOfPoints; k++,anIndex++) {
        fscanf(viewFile,"%dn",anIndex);
      } /* for points in LPoints (k) */
    } /* for LPoints in LLPoints (j) */
  } /* for LLPoints in LLLPoints (i) */
  
  fclose(viewFile);
  doView3D.scaleDown = no ;
}

