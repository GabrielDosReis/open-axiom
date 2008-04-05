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

#define _MAKE3D_C
#include "axiom-c-macros.h"

#include <stdlib.h>

#include "viewman.h"
#include "mode.h"

#include "sockio.h"
#include "make3D.H1"

void 
makeView3DFromSpadData(view3DStruct *viewdata,int typeOfViewport)
{

  int i,j,k;
  LLPoint *anLLPoint;
  LPoint *anLPoint;
  int *anIndex;
  int firstPieceOfData = yes;
  int constantColor;
  double cMin = 0;
  double cMax = 0;
  double cNorm = 0;

  viewdata->typeOf3D = typeOfViewport;

  viewdata->title = get_string(spadSock);

  viewdata->deltaX = get_float(spadSock);
  viewdata->deltaY = get_float(spadSock);
  viewdata->scale  = get_float(spadSock);
  viewdata->scaleX = get_float(spadSock);
  viewdata->scaleY = get_float(spadSock);
  viewdata->scaleZ = get_float(spadSock);
  viewdata->theta  = get_float(spadSock);
  viewdata->phi    = get_float(spadSock);

  viewdata->vX = get_int(spadSock);
  viewdata->vY = get_int(spadSock);
  viewdata->vW = get_int(spadSock);
  viewdata->vH = get_int(spadSock);
  
  viewdata->showCP    = get_int(spadSock);
  viewdata->style     = get_int(spadSock);
  viewdata->AxesOn    = get_int(spadSock);
  viewdata->diagonals = get_int(spadSock);
  viewdata->outlineRenderOn = get_int(spadSock);
  viewdata->box = get_int(spadSock);
  viewdata->clipbox = get_int(spadSock);
  viewdata->clipStuff = get_int(spadSock);

  viewdata->hueOff    = get_int(spadSock);
  viewdata->numOfHues = get_int(spadSock);

  viewdata->lightVec[0]  = get_float(spadSock);
  viewdata->lightVec[1]  = get_float(spadSock);
  viewdata->lightVec[2]  = get_float(spadSock);
  viewdata->translucency = get_float(spadSock);

  viewdata->perspective = get_int(spadSock);
  viewdata->eyeDistance = get_float(spadSock);

  viewdata->numOfPoints = get_int(spadSock);
  viewdata->points = (viewTriple *)malloc(viewdata->numOfPoints * sizeof(viewTriple));
  for (i=0; i<viewdata->numOfPoints; i++) {
    refPt(*viewdata,i)->x = get_float(spadSock);
    refPt(*viewdata,i)->y = get_float(spadSock);
    refPt(*viewdata,i)->z = get_float(spadSock);
    refPt(*viewdata,i)->c = get_float(spadSock);
          /* set min/max values */
    if (firstPieceOfData) {
      firstPieceOfData = no;
      viewdata->xmin = viewdata->xmax = refPt(*viewdata,i)->x;
      viewdata->ymin = viewdata->ymax = refPt(*viewdata,i)->y;
      viewdata->zmin = viewdata->zmax = refPt(*viewdata,i)->z;
      cMin = cMax = refPt(*viewdata,i)->c;
    } else {
      if (refPt(*viewdata,i)->x < viewdata->xmin) viewdata->xmin = refPt(*viewdata,i)->x;
      else if (refPt(*viewdata,i)->x > viewdata->xmax) viewdata->xmax = refPt(*viewdata,i)->x;
      if (refPt(*viewdata,i)->y < viewdata->ymin) viewdata->ymin = refPt(*viewdata,i)->y;
      else if (refPt(*viewdata,i)->y > viewdata->ymax) viewdata->ymax = refPt(*viewdata,i)->y;
      if (refPt(*viewdata,i)->z < viewdata->zmin) viewdata->zmin = refPt(*viewdata,i)->z;
      else if (refPt(*viewdata,i)->z > viewdata->zmax) viewdata->zmax = refPt(*viewdata,i)->z;
      if (refPt(*viewdata,i)->c < cMin) cMin = refPt(*viewdata,i)->c;
      else if (refPt(*viewdata,i)->c > cMax) cMax = refPt(*viewdata,i)->c;
    } /* if (firstPieceOfData) else */
  } /* for i (point data) */

  viewdata->lllp.numOfComponents = get_int(spadSock);
  anLLPoint = viewdata->lllp.llp =
    (LLPoint *)malloc(viewdata->lllp.numOfComponents*sizeof(LLPoint));
  for (i=0; i<viewdata->lllp.numOfComponents; i++,anLLPoint++) {
    anLLPoint->prop.closed = get_int(spadSock);
    anLLPoint->prop.solid = get_int(spadSock);
    anLLPoint->numOfLists = get_int(spadSock);
    anLPoint = anLLPoint->lp =
      (LPoint *)malloc(anLLPoint->numOfLists*sizeof(LPoint));
    for (j=0; j<anLLPoint->numOfLists; j++,anLPoint++) {
      anLPoint->prop.closed = get_int(spadSock);
      anLPoint->prop.solid = get_int(spadSock);
      anLPoint->numOfPoints = get_int(spadSock);
      anIndex = anLPoint->indices =
        (int *)malloc(anLPoint->numOfPoints*sizeof(int));
      for (k=0; k<anLPoint->numOfPoints; k++,anIndex++)
        *anIndex = get_int(spadSock);
    } /* for LPoints in LLPoints (j) */
  } /* for LLPoints in LLLPoints (i) */

      /* now normalize the colors */
  cNorm = cMax - cMin;
         /*** new fields - cmin, cmax ***/
  viewdata->cmin = cMin;
  viewdata->cmax = cMax;

  constantColor = (cNorm < 0.0001);
  for (i=0; i<viewdata->numOfPoints; i++)
    if (constantColor) refPt(*viewdata,i)->c = 0.5;
    else refPt(*viewdata,i)->c = (refPt(*viewdata,i)->c - cMin)/cNorm;

  viewdata->scaleDown = yes; 

}

