/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All right reserved.

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

#define _GRAPH2D_C
#include "axiom-c-macros.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "header2.h"

#include "all_2d.H1"
#include "util.H1"


void 
getGraphFromViewman(int i)
{
 /** This routine should be called right after a read of the graph key 
     was made from the viewport manager (or defined in some other way). **/

  int j,k,xPointsNeeded;
  pointListStruct *llPtr;
  pointStruct     *p;

  readViewman(&(graphArray[i].xmin),floatSize);
  readViewman(&(graphArray[i].xmax),floatSize);
  readViewman(&(graphArray[i].ymin),floatSize);
  readViewman(&(graphArray[i].ymax),floatSize);
  readViewman(&(graphArray[i].xNorm),floatSize);
  readViewman(&(graphArray[i].yNorm),floatSize);
  readViewman(&(graphArray[i].spadUnitX),floatSize);
  readViewman(&(graphArray[i].spadUnitY),floatSize);
  readViewman(&(graphArray[i].unitX),floatSize);
  readViewman(&(graphArray[i].unitY),floatSize);
  readViewman(&(graphArray[i].originX),floatSize);
  readViewman(&(graphArray[i].originY),floatSize);
  readViewman(&(graphArray[i].numberOfLists),intSize);
  
  if (!(llPtr = (pointListStruct *)malloc(graphArray[i].numberOfLists * sizeof(pointListStruct)))) {
    fprintf(stderr,"VIEW2D: Fatal Error>> Ran out of memory trying to receive a graph.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  graphArray[i].listOfListsOfPoints = llPtr;
  
  xPointsNeeded = 0;
  for (j=0; j<graphArray[i].numberOfLists; j++) {
    readViewman(&(llPtr->numberOfPoints),intSize);
    if (!(p = (pointStruct *)malloc(llPtr->numberOfPoints * sizeof(pointStruct)))) {
      fprintf(stderr,"VIEW2D: (pointStruct) ran out of memory trying to create a new graph.\n");
      exitWithAck(RootWindow(dsply,scrn),Window,-1);
    }
    llPtr->listOfPoints = p;             /** point to current point list **/
    for (k=0; k<llPtr->numberOfPoints; k++) {
      readViewman(&(p->x),floatSize);
      readViewman(&(p->y),floatSize);
      readViewman(&(p->hue),floatSize);
      readViewman(&(p->shade),floatSize);
      p++;
    }   /* for k in list of points */
    readViewman(&(llPtr->pointColor),intSize);
    readViewman(&(llPtr->lineColor),intSize);
    readViewman(&(llPtr->pointSize),intSize);
    
    xPointsNeeded += llPtr->numberOfPoints;
    llPtr++;
  }   /* for j in list of lists of points */
  
  /* read in graph state for the existing graph (override default values) */
  readViewman(&(graphStateArray[i].scaleX),floatSize);
  readViewman(&(graphStateArray[i].scaleY),floatSize);
  readViewman(&(graphStateArray[i].deltaX),floatSize);
  readViewman(&(graphStateArray[i].deltaY),floatSize);
  readViewman(&(graphStateArray[i].pointsOn),intSize);
  readViewman(&(graphStateArray[i].connectOn),intSize);
  readViewman(&(graphStateArray[i].splineOn),intSize);
  readViewman(&(graphStateArray[i].axesOn),intSize);
  readViewman(&(graphStateArray[i].axesColor),intSize);
  readViewman(&(graphStateArray[i].unitsOn),intSize);
  readViewman(&(graphStateArray[i].unitsColor),intSize);
  readViewman(&(graphStateArray[i].showing),intSize);
  graphStateArray[i].selected = yes;
  graphStateBackupArray[i] = graphStateArray[i];

  graphStateArray[i].deltaX = graphStateArray[0].deltaX;
  graphStateArray[i].deltaY = graphStateArray[0].deltaY;
  graphStateArray[i].scaleX = graphStateArray[0].scaleX;
  graphStateArray[i].scaleY = graphStateArray[0].scaleY;

  /* allocate memory for xPoints (used in drawViewport) */
  if (!(xPointsArray[i].xPoint = (XPoint *)malloc(xPointsNeeded * sizeof(XPoint)))) {
    fprintf(stderr,"VIEW2D: (XPoint) Ran out of memory (malloc) trying to create a new graph.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  if (!(xPointsArray[i].x10Point = (Vertex *)malloc(xPointsNeeded * sizeof(Vertex)))) {
    fprintf(stderr,
            "VIEW2D: (X10Point) Ran out of memory (malloc) trying to create a new graph.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  if (!(xPointsArray[i].arc = (XArc *)malloc(xPointsNeeded * sizeof(XArc)))) {
    fprintf(stderr,"VIEW2D: (XArc) Ran out of memory (malloc) trying to create a new graph.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }

}   /* getGraphFromViewman */



void 
freeGraph(int i)
{
  int j;
  pointListStruct *llPtr;

  if (graphArray[i].key) {
    graphArray[i].key = 0;   /* 0 means no graph */
    for (j=0,llPtr=graphArray[i].listOfListsOfPoints; 
         j<graphArray[i].numberOfLists; j++,llPtr++) 
      free(llPtr->listOfPoints);
    free(llPtr);
    free(xPointsArray[i].xPoint);
  } else {
  }

}
