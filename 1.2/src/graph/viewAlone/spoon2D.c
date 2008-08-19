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

#define _SPOON2D_C
#include "openaxiom-c-macros.h"

#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "viewAlone.h"


#include "util.H1"
#include "all_alone.H1"

/* #define huhDEBUG */

/* #define spoonDEBUG  */

void 
spoonView2D(void)
{

  int  i,code,pipe0[2],pipe1[2],there;
  char envAXIOM[100],runView[100];

  sprintf(errorStr,"%s","creating pipes");
  check(pipe(pipe0));
  check(pipe(pipe1));
  switch(fork()) {

  case -1:
    fprintf(stderr,
            "Cannot create a new process - probably have too many things running already.\n");
    exit(-1);

  case 0:
    /************
     *   Child  *
     ************/
    printf("(spoon2D child) mapping of pipes to standard I/O for view2D\n");
    sprintf(errorStr,"%s","(viewAlone) mapping of pipes to standard I/O for view2D");
    check(dup2(pipe0[0],0));     
    check(dup2(pipe1[1],1));
    close(pipe0[0]);
    close(pipe0[1]);
    close(pipe1[0]);
    close(pipe1[1]);
    printf("(spoon2D child) start the TwoDimensionalViewport process\n");
    sprintf(errorStr,"%s","(viewAlone) execution of the TwoDimensionalViewport process");
    sprintf(envAXIOM,"%s",getenv("AXIOM"));
    sprintf(runView,"%s%s",envAXIOM,"/lib/view2D");
    check(execl(runView,runView,NULL));
    fprintf(stderr,"Could not execute view2D! Check that view2D is on your path variable.\n");
    exit(-1);

  default:
    /*************
     *   Parent  *
     *************/

    viewP.viewType = view2DType;
    
    /* set up pipes to child process */
    close(pipe0[0]);
    close(pipe1[1]);
    viewP.viewIn  = pipe1[0];
    viewP.viewOut = pipe0[1];
    printf("(spoon2D parent) pipes created\n");
    if (viewP.viewIn <0) {
      fprintf(stderr,"  Could not connect from Viewport manager to viewport process. Try again.\n");
      return;
    } else {
      code = read(viewP.viewIn,&ack,intSize);
      if (code < 0) {
        fprintf(stderr,"  Could not connect from Viewport manager to viewport process. Try again.\n");
        return;
      }
    } 
    printf("(spoon2D parent) making View2D data\n");
    makeView2DFromFileData(&doView2D);

    /* tell child it is to be a stand alone program */
    i = yes;
    fprintf(stderr,"  Transmitting data to viewport...\n");
    write(viewP.viewOut,&i,intSize);

    write(viewP.viewOut,&doView2D,sizeof(view2DStruct));
    i = strlen(doView2D.title)+1;
    write(viewP.viewOut,&i,intSize);        /* tell the length of the title to child */
    write(viewP.viewOut,doView2D.title,i);  /* tell the title to the child, child */
    for (i=0; i<maxGraphs; i++) {
      there = graphArray[i].key;
      write(viewP.viewOut,&there,intSize);
      sendGraphToView2D(i,there,&viewP);
    };  /* for i in graphs */

    fprintf(stderr,"  Done.\n");
    
    /*** get acknowledge from viewport */

    code = read(viewP.viewIn,&(viewP.viewWindow),sizeof(Window));
    sleep(1);  /* wait a second...*/
    exit(0); 

  }   /* switch */

}    /* forkView2D() */



/*void sendGraphToView2D(i,there,viewP) */
void 
sendGraphToView2D(int i,int there,viewManager *viewP)
{
  graphStruct      *gPtr;
  pointListStruct  *llPtr;
  pointStruct      *p;
  int j,k;
  printf("(spoon2D sendGraphToView2D) i=%d there=%d\n",i,there);
  if (there) {

    gPtr = &(graphArray[i]);
    printf("(spoon2D sendGraphToView2D) graph %d is there\n",i);
    write(viewP->viewOut,&(gPtr->xmin),floatSize);
    write(viewP->viewOut,&(gPtr->xmax),floatSize);
    write(viewP->viewOut,&(gPtr->ymin),floatSize);
    write(viewP->viewOut,&(gPtr->ymax),floatSize);
    write(viewP->viewOut,&(gPtr->xNorm),floatSize);
    write(viewP->viewOut,&(gPtr->yNorm),floatSize);
    write(viewP->viewOut,&(gPtr->spadUnitX),floatSize);
    write(viewP->viewOut,&(gPtr->spadUnitY),floatSize);
    write(viewP->viewOut,&(gPtr->unitX),floatSize);
    write(viewP->viewOut,&(gPtr->unitY),floatSize);
    write(viewP->viewOut,&(gPtr->originX),floatSize);
    write(viewP->viewOut,&(gPtr->originY),floatSize);
    write(viewP->viewOut,&(gPtr->numberOfLists),intSize);
    
    llPtr = gPtr->listOfListsOfPoints;
    for (j=0; j<(gPtr->numberOfLists); j++) {
      write(viewP->viewOut,&(llPtr->numberOfPoints),intSize);
      p = llPtr->listOfPoints;
      for (k=0; k<(llPtr->numberOfPoints); k++) {
        write(viewP->viewOut,&(p->x),floatSize);
        write(viewP->viewOut,&(p->y),floatSize);
        write(viewP->viewOut,&(p->hue),floatSize);
        write(viewP->viewOut,&(p->shade),floatSize);
        p++;
      }    /* for k in list of points */
      write(viewP->viewOut,&(llPtr->pointColor),intSize);
      write(viewP->viewOut,&(llPtr->lineColor),intSize);
      write(viewP->viewOut,&(llPtr->pointSize),intSize);
      llPtr++;
    }   /* for j in list of lists of points */
    
    /* a state is defined for a graph if it is there */
    write(viewP->viewOut,&(graphStateArray[i].scaleX),floatSize);
    write(viewP->viewOut,&(graphStateArray[i].scaleY),floatSize);
    write(viewP->viewOut,&(graphStateArray[i].deltaX),floatSize);
    write(viewP->viewOut,&(graphStateArray[i].deltaY),floatSize);
    write(viewP->viewOut,&(graphStateArray[i].pointsOn),intSize);
    write(viewP->viewOut,&(graphStateArray[i].connectOn),intSize);
    write(viewP->viewOut,&(graphStateArray[i].splineOn),intSize);
    write(viewP->viewOut,&(graphStateArray[i].axesOn),intSize);
    write(viewP->viewOut,&(graphStateArray[i].axesColor),intSize);
    write(viewP->viewOut,&(graphStateArray[i].unitsOn),intSize);
    write(viewP->viewOut,&(graphStateArray[i].unitsColor),intSize);
    write(viewP->viewOut,&(graphStateArray[i].showing),intSize);
    
  }   /* if graph is there */

}


void  
makeView2DFromFileData(view2DStruct *doView2D)
{
  
  int i,j,k;
  char title[256];
  FILE *graphFile;
  char graphFilename[256];
  pointListStruct *aList;
  pointStruct     *aPoint;
  printf("(spoon2D makeView2DFromFileData)\n");  
  fgets(title,256,viewFile);
  printf("(spoon2D) title=%s\n",title);  
  if (!(doView2D->title = 
        (char *)malloc((strlen(title)+1) * sizeof(char)))) {
    fprintf(stderr,
            "Ran out of memory (malloc) trying to get the title.\n");
    exit(-1);
  }
  sprintf(doView2D->title,"%s",title);
  /* put in a null terminator over the newline that the fgets reads */
  doView2D->title[strlen(doView2D->title)-1] = '\0'; 
  fscanf(viewFile,"%d %d %d %d\n",
         &(doView2D->vX),
         &(doView2D->vY),
         &(doView2D->vW),
         &(doView2D->vH));
  printf("(spoon2D) X=%d Y=%d W=%d H=%d \n",
           doView2D->vX,doView2D->vY,doView2D->vW,doView2D->vH);
  for (i=0; i<maxGraphs; i++) {
    fscanf(viewFile,"%d\n",
           &(graphArray[i].key));
    printf("(spoon2D) i=%d key=%d\n",
           i,graphArray[i].key);  
    fscanf(viewFile,"%g %g\n",
           &(graphStateArray[i].scaleX),
           &(graphStateArray[i].scaleY));
    printf("(spoon2D) scaleX=%g scaleY=%g\n",
             graphStateArray[i].scaleX,graphStateArray[i].scaleY);
    fscanf(viewFile,"%g %g\n",
           &(graphStateArray[i].deltaX),
           &(graphStateArray[i].deltaY));
    printf("(spoon2D) deltaX=%g deltaY=%g\n",
             graphStateArray[i].deltaX,graphStateArray[i].deltaY);
    fscanf(viewFile,"%g %g\n",
           &(graphStateArray[i].centerX),
           &(graphStateArray[i].centerY));
    printf("(spoon2D) centerX=%g centerY=%g\n",
             graphStateArray[i].centerX,graphStateArray[i].centerY);
    fscanf(viewFile,"%d %d %d %d %d %d %d\n",
           &(graphStateArray[i].pointsOn),
           &(graphStateArray[i].connectOn),
           &(graphStateArray[i].splineOn),
           &(graphStateArray[i].axesOn),
           &(graphStateArray[i].axesColor),
           &(graphStateArray[i].unitsOn),
           &(graphStateArray[i].unitsColor));
    printf("(spoon2D) pointsOn=%d connectOn=%d splineOn=%d axesOn=%d axesColor=%d unitsOn=%d unitsColor=%d\n",
           graphStateArray[i].pointsOn,graphStateArray[i].connectOn,
           graphStateArray[i].splineOn,graphStateArray[i].axesOn,
           graphStateArray[i].axesColor,graphStateArray[i].unitsOn,
           graphStateArray[i].unitsColor);
    fscanf(viewFile,"%d %d\n",
           &(graphStateArray[i].showing),
           &(graphStateArray[i].selected));
    printf("(spoon2D) showing=%d selected=%d\n",
            graphStateArray[i].showing,graphStateArray[i].selected);
  }
  fclose(viewFile);
  for (i=0; i<maxGraphs; i++) {
    if (graphArray[i].key) {
      /** OPEN FILE FOR GRAPHS **/
      sprintf(graphFilename,"%s%s%d",pathname,"/graph",i);
      if ((graphFile = fopen(graphFilename,"r")) == NULL) {
        fprintf(stderr,"   Error: Cannot find the file %s\n",graphFilename);
        perror("fopen");
        return;
      } else {
        printf("(spoon2d) \n\nGRAPH%i\n",i);
        fscanf(graphFile,"%g %g %g %g\n",
               &(graphArray[i].xmin),
               &(graphArray[i].ymin),
               &(graphArray[i].xmax),
               &(graphArray[i].ymax));
        printf("(spoon2d) xmin=%g ymin=%g xmax=%g ymax=%g\n",
                graphArray[i].xmin,graphArray[i].ymin,
                graphArray[i].xmax,graphArray[i].ymax);
        fscanf(graphFile,"%g %g\n",
               &(graphArray[i].xNorm),
               &(graphArray[i].yNorm));
        printf("(spoon2d) xNorm=%g yNorm=%g\n",
               graphArray[i].xNorm,graphArray[i].yNorm);
        fscanf(graphFile,"%g %g\n",
               &(graphArray[i].originX),
               &(graphArray[i].originY));
        printf("(spoon2d) originX=%g originY=%g\n",
               graphArray[i].originX,graphArray[i].originY);
        fscanf(graphFile,"%g %g\n",
               &(graphArray[i].spadUnitX),
               &(graphArray[i].spadUnitY));
        printf("(spoon2d) spadUnitX=%g spadUnitY=%g\n",
               graphArray[i].spadUnitX,graphArray[i].spadUnitY);
        fscanf(graphFile,"%g %g\n",
               &(graphArray[i].unitX),
               &(graphArray[i].unitY));
        printf("(spoon2d) unitX=%g unitY=%g\n",
               graphArray[i].unitX,graphArray[i].unitY);
        fscanf(graphFile,"%d\n",
               &(graphArray[i].numberOfLists));
        printf("(spoon2d) numberOfLists=%d\n",
                graphArray[i].numberOfLists);
        if (!(aList =
              (pointListStruct *)malloc(graphArray[i].numberOfLists * 
                                        sizeof(pointListStruct)))) {
          fprintf(stderr,"viewAlone: Fatal Error>> Out of memory trying to receive a graph.\n");
          exit(-1);
        }
        graphArray[i].listOfListsOfPoints = aList;
        for (j=0;
             j<graphArray[i].numberOfLists;
             j++, aList++) {
          printf("(spoon2d) list number %d\n",j);
          fscanf(graphFile,"%d\n",&(aList->numberOfPoints));
          printf("(spoon2d) number of points %d\n",
                  aList->numberOfPoints);
          fscanf(graphFile,"%d %d %d\n",
                 &(aList->pointColor),
                 &(aList->lineColor),
                 &(aList->pointSize));
          printf("(spoon2d) pointColor=%d lineColor=%d pointSize=%d\n",
                 aList->pointColor,aList->lineColor,aList->pointSize);
          if (!(aPoint = (pointStruct *)malloc(aList->numberOfPoints * 
                                               sizeof(pointStruct)))) {
            fprintf(stderr,"viewAlone: Fatal Error>> Out of memory trying to receive a graph.\n");
            exit(-1);
          }
          aList->listOfPoints = aPoint;   /** point to current point list **/
          for (k=0;
               k<aList->numberOfPoints; 
               k++,aPoint++) 
          {  fscanf(graphFile,"%g %g %g %g\n",
                   &(aPoint->x),
                   &(aPoint->y),
                   &(aPoint->hue),
                   &(aPoint->shade));
            printf("(spoon2d)k=%d x=%g y=%g hue=%g shade=%g\n",
                   k,aPoint->x,aPoint->y,aPoint->hue,aPoint->shade);
          }
        } /* for j, aList */            
        fclose(graphFile);
      } /* else, opened up a file */
    } /* if graph.key */
  } /* for i */
} /* makeView2DFromFileData */
