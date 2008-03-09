/*
  Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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

#define _SURFACE3D_C
#include "axiom-c-macros.h"

#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "header.h"
#include "draw.h"
#include "mode.h"   /* for #define components */
#include "sockio.h"
#include "com.h"

#include "XSpadFill.H1"
#include "XShade.H1"
#include "util.H1"
#include "Gfun.H1"
#include "all_3d.H1"


  /**** useful defines ****/

#define precisionFactor 1024

/* depthChecker turns on the extensive depth checking mechanisms
   for the depth sort algorithm. Without it, the hidden surface
   removal is just a sort by z which works remarkably well, but,
   is insufficient and, at times, may end up being incorrect */
#define depthChecker


pointInfo ptIA, ptIB, ptIC; /* global to this file */

/************************************
 * void drawLineComponent(p,dFlag)  *
 ************************************/

void
#ifdef _NO_PROTO
drawLineComponent (p, dFlag)
     poly *p;
     int  dFlag;
#else
drawLineComponent (poly * p, int dFlag)
#endif 
{
  int i, hue;
  int *anIndex;
  RGB col_rgb;

  /* If the polygon is clipped against the hither plane (clipPz) then
     it is not drawn...or...if the clipStuff is set to true, and
     the polygon is clipped against the user defined clip volume, it
     is also not drawn. */
  if (!((p->partialClipPz) || (viewData.clipStuff && (p->partialClip)))) {
    /* This routine will eventually only be skipped if
       p->totalClip is true and another routine would
       handle the partialClip. this routine would handle
       only those polygons without any clipped points */
    for (i=0, anIndex=p->indexPtr; i<p->numpts; i++,anIndex++) {
      quadMesh[i].x = refPt3D(viewData,*anIndex)->px;
      quadMesh[i].y = refPt3D(viewData,*anIndex)->py;
    }

    if (dFlag==Xoption) {
      if (mono || viewport->monoOn)
        GSetForeground(opaqueGC, (float)foregroundColor, dFlag);
      else {
        hue = hueValue(p->color);
        GSetForeground(opaqueGC, (float)XSolidColor(hue,2), dFlag);
      }
    } else
      GSetForeground(opaqueGC, psBlack, dFlag);

    if (dFlag==PSoption && !mono && !viewport->monoOn) {
      hue = getHue(p->color);
      col_rgb = hlsTOrgb((float)hue,0.5,0.8);
      PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,quadMesh,p->numpts);
    } else {
      GDrawLines(opaqueGC, viewport->viewWindow, quadMesh, p->numpts,
                 CoordModeOrigin, dFlag);
    }
    if (dFlag == Xoption)
      XMapWindow(dsply, viewport->viewWindow);
  }
}



/**************************************************
 * void drawOpaquePolygon(p,aGC,anotherGC)        *
 **************************************************/

void
#ifdef _NO_PROTO
drawOpaquePolygon (p,aGC,anotherGC,dFlag)
     poly *p;
     GC aGC,anotherGC;
     int dFlag;
#else
drawOpaquePolygon (poly *p,GC aGC,GC anotherGC,int dFlag)
#endif 
{

  int *anIndex, i, hue, isNaN = 0;
  RGB col_rgb;

  if (mono || viewport->monoOn) {
    GSetForeground(anotherGC, (float)foregroundColor, dFlag);
  } else {
    hue = hueValue(p->color);
    GSetForeground(anotherGC, (float)XSolidColor(hue,2), dFlag);
  }

  /* If the polygon is clipped against the hither plane (clipPz) then
     it is not drawn, or if the clipStuff is set to true, and
     the polygon is clipped against the user defined clip volume, it
     is also not drawn. */

  if (!((p->partialClipPz) || (viewData.clipStuff && (p->partialClip)))) {

    /* This routine should eventually only be skipped if
       p->totalClip is true and another routine would
       handle the partialClip. This routine would handle
       only those polygons without any clipped points. */

    for (i=0, anIndex=p->indexPtr; i<p->numpts; i++,anIndex++) {
      quadMesh[i].x = refPt3D(viewData,*anIndex)->px;
      quadMesh[i].y = refPt3D(viewData,*anIndex)->py;
      if (eqNANQ(quadMesh[i].x) || eqNANQ(quadMesh[i].y)) isNaN = 1;
    }

    quadMesh[i].x =refPt3D(viewData,*(p->indexPtr))->px;
    quadMesh[i].y =refPt3D(viewData,*(p->indexPtr))->py;
    if (eqNANQ(quadMesh[i].x) || eqNANQ(quadMesh[i].y)) isNaN = 1;

    if (dFlag==PSoption && !mono && !viewport->monoOn && !isNaN) {
      GSetForeground(GC9991, (float)backgroundColor, PSoption);
      PSFillPolygon(GC9991, quadMesh, p->numpts+1);
      hue = getHue(p->color);
      col_rgb = hlsTOrgb((float)hue,0.5,0.8);
      if (viewport->diagonals)
        PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,quadMesh,p->numpts+1);
      else
        PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,quadMesh,p->numpts);
    } else {
      if (mono || viewport->monoOn) {
        GSetForeground(anotherGC, (float)foregroundColor, dFlag);
      } else {
        hue = hueValue(p->color);
        GSetForeground(anotherGC, (float)XSolidColor(hue,2), dFlag);
      }
      GSetForeground(aGC,(float)backgroundColor,dFlag);
      if (!isNaN) {
        XFillPolygon(dsply, viewport->viewWindow, aGC, quadMesh, p->numpts,
                     Convex,CoordModeOrigin);
        if (viewport->diagonals)
          GDrawLines(anotherGC,viewport->viewWindow,quadMesh,p->numpts+1,
                     CoordModeOrigin, dFlag);
        else
          GDrawLines(anotherGC,viewport->viewWindow,quadMesh,p->numpts,
                     CoordModeOrigin, dFlag);
      }
    }
    if (dFlag == Xoption) XMapWindow(dsply,viewport->viewWindow);
  }   /* if not totally clipped */
}



/*************************************
 * poly *copyPolygons(polygonList)   *
 *                                   *
 * copies the given list of polygons *
 * into a newly allocated list       *
 *************************************/

poly *
#ifdef _NO_PROTO
copyPolygons (polygonList)
     poly *polygonList;
#else
copyPolygons (poly *polygonList)
#endif 
{

  int i;
  poly *aPoly,*retval,*prev;

  prev = retval = aPoly = (poly *)saymem("surface.c",1,sizeof(poly));
  aPoly->indexPtr = (int *)saymem("surface.c",
                                  polygonList->numpts,sizeof(int));
  aPoly->num = polygonList->num;
  aPoly->sortNum = polygonList->sortNum;
  aPoly->split = polygonList->split;
  aPoly->numpts = polygonList->numpts;
  for (i=0; i<aPoly->numpts; i++)
    *((aPoly->indexPtr) + i) = *((polygonList->indexPtr) + i);
  aPoly->N[0] = polygonList->N[0];
  aPoly->N[1] = polygonList->N[1];
  aPoly->N[2] = polygonList->N[2];
  aPoly->planeConst = polygonList->planeConst;
  aPoly->color = polygonList->color;
  aPoly->moved = no;
  aPoly->pxmin = polygonList->pxmin;
  aPoly->pxmax = polygonList->pxmax;
  aPoly->pymin = polygonList->pymin;
  aPoly->pymax = polygonList->pymax;
  aPoly->pzmin = polygonList->pzmin;
  aPoly->pzmax = polygonList->pzmax;
  aPoly->xmin = polygonList->xmin;
  aPoly->xmax = polygonList->xmax;
  aPoly->ymin = polygonList->ymin;
  aPoly->ymax = polygonList->ymax;
  aPoly->zmin = polygonList->zmin;
  aPoly->zmax = polygonList->zmax;
  aPoly->normalFacingOut = polygonList->normalFacingOut;
  aPoly->primitiveType = polygonList->primitiveType;
  for (polygonList = polygonList->next;
       polygonList != NIL(poly);
       polygonList = polygonList->next) {
    prev->next = aPoly = (poly *)saymem("surface.c",1,sizeof(poly));
    aPoly->indexPtr = (int *)saymem("surface.c",
                                    polygonList->numpts,sizeof(int));
    aPoly->num = polygonList->num;
    aPoly->sortNum = polygonList->sortNum;
    aPoly->numpts = polygonList->numpts;
    aPoly->split = polygonList->split;
    for (i=0; i<aPoly->numpts; i++)
      *((aPoly->indexPtr) + i) = *((polygonList->indexPtr) + i);
    aPoly->N[0] = polygonList->N[0];
    aPoly->N[1] = polygonList->N[1];
    aPoly->N[2] = polygonList->N[2];
    aPoly->planeConst = polygonList->planeConst;
    aPoly->color = polygonList->color;
    aPoly->moved = no;
    aPoly->pxmin = polygonList->pxmin;
    aPoly->pxmax = polygonList->pxmax;
    aPoly->pymin = polygonList->pymin;
    aPoly->pymax = polygonList->pymax;
    aPoly->pzmin = polygonList->pzmin;
    aPoly->pzmax = polygonList->pzmax;
    aPoly->xmin = polygonList->xmin;
    aPoly->xmax = polygonList->xmax;
    aPoly->ymin = polygonList->ymin;
    aPoly->ymax = polygonList->ymax;
    aPoly->zmin = polygonList->zmin;
    aPoly->zmax = polygonList->zmax;
    aPoly->normalFacingOut = polygonList->normalFacingOut;
    aPoly->primitiveType = polygonList->primitiveType;
    prev = aPoly;
  }
  aPoly->next = 0;
  return(retval);
}


/******************************
 * void minMaxPolygons(aPoly) *
 *                            *
 * sets up the xmin,          *
 * etc, for each polygon      *
 * for sorting and            *
 * extent checking.           *
 ******************************/

void
#ifdef _NO_PROTO
minMaxPolygons (aPoly)
     poly *aPoly;
#else
minMaxPolygons (poly *aPoly)
#endif 
{

  int *anIndex;
  int i;

  for (; aPoly != NIL(poly); aPoly = aPoly->next) {
    anIndex = aPoly->indexPtr;
    aPoly->pxmin = aPoly->pxmax = refPt3D(viewData,*anIndex)->px;
    aPoly->pymin = aPoly->pymax = refPt3D(viewData,*anIndex)->py;
    aPoly->pzmin = aPoly->pzmax = refPt3D(viewData,*anIndex)->pz;
    aPoly->xmin = aPoly->xmax = refPt3D(viewData,*anIndex)->x;
    aPoly->ymin = aPoly->ymax = refPt3D(viewData,*anIndex)->y;
    aPoly->zmin = aPoly->zmax = refPt3D(viewData,*anIndex)->z;
    for (i=1,anIndex++; i<aPoly->numpts; i++,anIndex++) {
      if (refPt3D(viewData,*anIndex)->px < aPoly->pxmin)
        aPoly->pxmin = refPt3D(viewData,*anIndex)->px;
      else if (refPt3D(viewData,*anIndex)->px > aPoly->pxmax)
        aPoly->pxmax = refPt3D(viewData,*anIndex)->px;
      if (refPt3D(viewData,*anIndex)->py < aPoly->pymin)
        aPoly->pymin = refPt3D(viewData,*anIndex)->py;
      else if (refPt3D(viewData,*anIndex)->py > aPoly->pymax)
        aPoly->pymax = refPt3D(viewData,*anIndex)->py;
      if (refPt3D(viewData,*anIndex)->pz < aPoly->pzmin)
        aPoly->pzmin = refPt3D(viewData,*anIndex)->pz;
      else if (refPt3D(viewData,*anIndex)->pz > aPoly->pzmax)
        aPoly->pzmax = refPt3D(viewData,*anIndex)->pz;

      if (refPt3D(viewData,*anIndex)->x < aPoly->xmin)
        aPoly->xmin = refPt3D(viewData,*anIndex)->x;
      else if (refPt3D(viewData,*anIndex)->x > aPoly->xmax)
        aPoly->xmax = refPt3D(viewData,*anIndex)->x;
      if (refPt3D(viewData,*anIndex)->y < aPoly->ymin)
        aPoly->ymin = refPt3D(viewData,*anIndex)->y;
      else if (refPt3D(viewData,*anIndex)->y > aPoly->ymax)
        aPoly->ymax = refPt3D(viewData,*anIndex)->y;
      if (refPt3D(viewData,*anIndex)->z < aPoly->zmin)
        aPoly->zmin = refPt3D(viewData,*anIndex)->z;
      else if (refPt3D(viewData,*anIndex)->z > aPoly->zmax)
        aPoly->zmax = refPt3D(viewData,*anIndex)->z;
    }
  }
}  /* minMaxPolygons */



/***********************************
 * int polyCompare (p1,p2) *
 *                                 *
 * returns -1 if p1 < p2           *
 *          0 if p1 = p2           *
 *          1 if p1 > p2           *
 * note that this is the reverse   *
 * of what the msort requested.    *
 * this is so that the list will   *
 * be sorted from max to min.      *
 ***********************************/

int
#ifdef _NO_PROTO
polyCompare (p1,p2)
     poly *p1,*p2;
#else
polyCompare (poly *p1,poly *p2)
#endif 
{

  if (p1->pzmax > p2->pzmax) return(-1);
  else if (p1->pzmax < p2->pzmax) return(1);
  else return(0);
}

/***********************
 * void calcEyePoint() *
 *                     *
 * sets the global     *
 * variable eyePoint[] *
 * to where the viewer *
 * is pointed towards  *
 ***********************/

void
#ifdef _NO_PROTO
calcEyePoint ()
#else
calcEyePoint (void)
#endif 
{

  eyePoint[0] = sinPhi * (sinTheta);
  eyePoint[1] = sinPhi * (-cosTheta);
  eyePoint[2] = cosPhi;

}



/*
 void drawPolygons()
 A general routine for displaying a list of polygons 
 with the proper hidden surfaces removed. Assumes the 
 list of polygons is in viewData.polygons. Needs a 
 routine to split intersecting polygons in object space.               *
 */


/**************************************
 * void drawRenderedPolygon(p,dFlag)  *
 *                                    *
 * calculate the color for the        *
 * polygon p and draw it              *
 **************************************/

void
#ifdef _NO_PROTO
drawRenderedPolygon (p,dFlag)
     poly *p;
     int dFlag;
#else
drawRenderedPolygon (poly *p,int dFlag)
#endif 
{

  int        i,hue,shade, isNaN = 0;
  float      whichSide,H[3],P[3],LN,HN,diff,spec,tempLight,lumens,E[3],N[3];
  int        *anIndex, *indx;
  RGB        col_rgb;

  if (!((p->partialClipPz) || (viewData.clipStuff && (p->partialClip)))) {
      /* This routine should eventually only be skipped if
         p->totalClip is true and another routine would
         handle the partialClip. This routine would handle
         only those polygons without any clipped points. */

    for (i=0, anIndex=p->indexPtr; i<p->numpts; i++,anIndex++) {
      quadMesh[i].x = refPt3D(viewData,*anIndex)->px;
      quadMesh[i].y = refPt3D(viewData,*anIndex)->py;
      if (eqNANQ(quadMesh[i].x) || eqNANQ(quadMesh[i].y)) isNaN = 1;
    }
    quadMesh[i].x = refPt3D(viewData,*(p->indexPtr))->px;
    quadMesh[i].y = refPt3D(viewData,*(p->indexPtr))->py;
    if (eqNANQ(quadMesh[i].x) || eqNANQ(quadMesh[i].y)) isNaN = 1;

    if (!isNaN) {
      /* calculate polygon illumination */
      indx = p->indexPtr;
      P[0] = (refPt3D(viewData,*(indx))->wx +
              refPt3D(viewData,*(indx+1))->wx +
              refPt3D(viewData,*(indx+2))->wx);
      P[1] = (refPt3D(viewData,*(indx))->wy +
              refPt3D(viewData,*(indx+1))->wy +
              refPt3D(viewData,*(indx+2))->wy);
      P[2] = (refPt3D(viewData,*(indx))->wz +
              refPt3D(viewData,*(indx+1))->wz +
              refPt3D(viewData,*(indx+2))->wz);
      normalizeVector(P);

      N[0] = p->N[0];  N[1] = p->N[1];  N[2] = p->N[2];
      normalizeVector(eyePoint);
      E[0] = 4.0*eyePoint[0] - P[0];
      E[1] = 4.0*eyePoint[1] - P[1];
      E[2] = 4.0*eyePoint[2] - P[2];
      normalizeVector(E);
      diff = 0.0;        spec = 0.0;
      LN = N[0]*viewport->lightVector[0] +
           N[1]*viewport->lightVector[1] +
           N[2]*viewport->lightVector[2];
      if (LN < 0.0) LN = -LN;
      diff = LN*Cdiff;

      if (LN > 0.0) {
        H[0] = E[0] + viewport->lightVector[0];
        H[1] = E[1] + viewport->lightVector[1];
        H[2] = E[2] + viewport->lightVector[2];
        normalizeVector(H);
        HN = dotProduct(N,H,3);
        if (HN < 0.0) HN = -HN;
        spec = pow((double)absolute(HN),coeff);
        if (spec > 1.0) spec = 1.0;
      }

      lumens = ((Camb + 0.15) + diff + spec*Cspec);
      if (lumens > 1.0) lumens = 1.0;
      if (lumens < 0.0) lumens = 0.0;
      if (dFlag==PSoption && !mono && !viewport->monoOn) {
        hue = getHue(p->color);
        col_rgb = hlsTOrgb((float)hue,lumens,0.8);
        /* NTSC color to grey =  .299 red + .587 green + .114 blue */
        maxGreyShade = (int) psShadeMax;
        whichSide = (.299*col_rgb.r + .587*col_rgb.g + .114*col_rgb.b) *
                    (maxGreyShade-1);
      }
      else {
        if (mono || viewport->monoOn) {
          hue = getHue(p->color);
          col_rgb = hlsTOrgb((float)hue,lumens,0.8);
          whichSide = (.299*col_rgb.r + .587*col_rgb.g + .114*col_rgb.b) *
                      (maxGreyShade-1);
        } else
          whichSide = lumens*(totalShades-1);
      }

      tempLight = lightIntensity;
      if (lightIntensity < Camb) lightIntensity = Camb;

      shade = floor(lightIntensity * absolute(whichSide));
      lightIntensity = tempLight;

      if (shade < totalShades) {
        /* shade < totalShades is (temporarily) necessary here
           because, currently, parameterizations for things like
           the sphere would produce triangular shaped polygons
           close to the poles which get triangularized leaving a
           triangle with coincidental points. the normal for this
           would be undefined (since coincidental points would create
           a zero vector) and the shade would be large, hence,
           the conditional.  */

        if (mono || viewport->monoOn) {
          if (dFlag == Xoption) {
            XChangeShade(dsply,maxGreyShade-shade-1);
            XShadePolygon(dsply,viewport->viewWindow,quadMesh,p->numpts+1,
                          Convex,CoordModeOrigin);
          }
          else if (dFlag == PSoption) { /* renderGC has number 9991 
                                     (see main.c, header.h) */
            GSetForeground(GC9991,
                           1.0-(float)(maxGreyShade-shade-1)*psShadeMul,PSoption);
            PSFillPolygon(GC9991, quadMesh, p->numpts+1);
          }
        } else { /* not mono */
          if (dFlag == Xoption) {
            hue = hueValue(p->color);
            XSpadFillPolygon(dsply, viewport->viewWindow, quadMesh,
                             p->numpts+1, Convex,CoordModeOrigin, hue, shade);
          }
          else if (dFlag == PSoption)  /* draws it out in monochrome  */
            PSColorPolygon(col_rgb.r,col_rgb.g,col_rgb.b,quadMesh,p->numpts+1);
        }        /* if mono-else */

        if (viewData.outlineRenderOn) {
          if (viewport->diagonals) {
            if (dFlag == PSoption) {
              GSetForeground(renderGC,psBlack, dFlag);
              GDrawLines(renderGC,viewport->viewWindow,quadMesh,p->numpts+1,
                         CoordModeOrigin,dFlag);
            } else
              GDrawLines(renderGC,viewport->viewWindow,quadMesh,p->numpts+1,
                         CoordModeOrigin,dFlag);
          } else {
            if (dFlag == PSoption) {
              GSetForeground(renderGC,psBlack,PSoption);
              GDrawLines(renderGC,viewport->viewWindow,quadMesh,p->numpts,
                         CoordModeOrigin,PSoption);
            } else
              GDrawLines(renderGC,viewport->viewWindow,quadMesh,p->numpts,
                         CoordModeOrigin,dFlag);
          }
        }
      }
    } /* if not NaN */
    if (dFlag == Xoption) XMapWindow(dsply,viewport->viewWindow);
  }  /* if not clipped */

} /* drawRenderedPolygon */


void 
#ifdef _NO_PROTO
freePointResevoir()
#else
freePointResevoir(void)
#endif
{

  viewTriple *v;

  while (splitPoints != NIL(viewTriple)) {
    v = splitPoints;
    splitPoints = splitPoints->next;
    free(v);
  }

} /* freePointResevoir */

/***********************************
 * void freeListOfPolygons(pList); *
 *                                 *
 * frees up a list of polygons.    *
 ***********************************/

void
#ifdef _NO_PROTO
freeListOfPolygons (pList)
poly *pList;
#else
freeListOfPolygons (poly *pList)
#endif 
{

  poly *nextP;

  for (; pList != NIL(poly); pList=nextP) {
    nextP=pList->next;
    free(pList->indexPtr);
    free(pList);
  }
} /* freeListOfPolygons() */



void
#ifdef _NO_PROTO
drawPolygons(dFlag)
     int dFlag;
#else
drawPolygons(int dFlag)
#endif 
{

  poly *p,*head;
  poly *tempQuick=NULL;
  int quickFirst=yes;

  if (recalc) {

    /* To get around multiple X Expose events the server tends
       to send upon startup, leave negation of firstTime to the end. */
    rotated = no;
    zoomed = no;
    translated = no;
    switchedPerspective = no;
    changedEyeDistance = no;
    redoSmooth = yes;

    if (keepDrawingViewport()) {
      if (!firstTime) {
        strcpy(control->message,"          Creating Polygons         ");
        writeControlMessage();
        freeListOfPolygons(quickList);
        freePointResevoir();
      }
      strcpy(control->message,"         Collecting Polygons        ");
      writeControlMessage();
      quickList = copyPolygons(viewData.polygons);

      if (keepDrawingViewport()) {
        /* to get normal facing outside info    */
        strcpy(control->message,"         Projecting Polygons        ");
        writeControlMessage();
        projectAllPolys(quickList);
        if (keepDrawingViewport()) {
          strcpy(control->message,"       Setting Extreme Values       ");
          writeControlMessage();
          minMaxPolygons(quickList);
          if (keepDrawingViewport()) {
            strcpy(control->message,"          Sorting Polygons          ");
            writeControlMessage();
            quickList = msort(quickList,0,viewData.numPolygons,polyCompare);
            if (keepDrawingViewport()) {
              calcEyePoint();
              head = p = quickList;

              clearControlMessage();
              strcpy(control->message,viewport->title);
              writeControlMessage();

              if (viewData.scaleDown) {
                if (keepDrawingViewport()) {
                  for (p=quickList;
                       keepDrawingViewport() && (p != NIL(poly));
                       p=p->next) {
                    switch (p->primitiveType) {
                    case pointComponent:
                      if (dFlag==Xoption) {
                        if (mono || viewport->monoOn)
                          GSetForeground(componentGC,
                                         (float)foregroundColor, dFlag);
                        else
                          GSetForeground(componentGC,
                                         (float)meshOutline, dFlag);
                      } else {
                        GSetForeground(componentGC, psBlack, dFlag);
                        GFillArc(componentGC, viewport->viewWindow,
                                 (int)refPt3D(viewData,*(p->indexPtr))->px,
                                 (int)refPt3D(viewData,*(p->indexPtr))->py,
                                 viewData.pointSize,viewData.pointSize,0,
                                 360*64, dFlag);
                      }
                      break;
                    case lineComponent:
                      drawLineComponent(p,dFlag);
                      break;
                    default:
                      if (viewData.style == opaqueMesh) {
                        GSetForeground(globGC,(float)backgroundColor,dFlag);
                        drawOpaquePolygon(p,globGC,opaqueGC,dFlag);
                      } else {
                        drawRenderedPolygon(p,dFlag);
                      }
                    } /* switch */
                  }
                }
              }

              if (!quickFirst) {
                /* append the rest of the polygons onto the list */
                tempQuick->next = head;
                /* but do not continue the drawing... */
                if (head != NIL(poly)) head->doNotStopDraw = no;
              }  /* if !quickFirst */
              finishedList = (p==NIL(poly));
            }      /*      for various      */
          }        /*         steps         */
        }          /*           of          */
      }    /* keepDrawingViewport() */
    }              /*          ***          */
    /* May want to have a flag somewhere to stop the drawing yet
       continue the freeing */
    if (firstTime) firstTime = no;
  } else {  /* if recalc else if not recalc just draw stuff in list */
    if (keepDrawingViewport()) {
      for (p=quickList;
           keepDrawingViewport() && p != NIL(poly) &&
             (viewData.scaleDown || p->doNotStopDraw);  p=p->next) {
        projectAPoly(p);
        switch (p->primitiveType) {
        case pointComponent:
          if (dFlag==Xoption) {
            if (mono || viewport->monoOn)
              GSetForeground(componentGC,(float)foregroundColor, dFlag);
            else
              GSetForeground(componentGC,(float)meshOutline, dFlag);
          } else
            GSetForeground(componentGC,psBlack, dFlag);
          GFillArc(componentGC, viewport->viewWindow,
                   (int)refPt3D(viewData,*(p->indexPtr))->px,
                   (int)refPt3D(viewData,*(p->indexPtr))->py,
                   viewData.pointSize,viewData.pointSize,0,360*64,dFlag);
          break;
        case lineComponent:
          drawLineComponent(p,dFlag);
          break;
        default:
          if (viewData.style == opaqueMesh) {
            GSetForeground(globGC,(float)backgroundColor,dFlag);
            drawOpaquePolygon(p,globGC,opaqueGC,dFlag);
          } else
            drawRenderedPolygon(p,dFlag);
        } /* switch */
      }
    }
  }
} /* drawPolygons */







/**************************
 * int lessThan(x,y)      *
 * int greaterThan(x,y)   *
 * int equal(x,y)         *
 *                        *
 * Compares two floating  *
 * point numbers for      *
 * precision of up to one *
 * place in a thousand.   *
 * returns                *
 *    1 if true           *
 *    o otherwise         *
 **************************/

int
#ifdef _NO_PROTO
lessThan (x,y)
     float x,y;
#else
lessThan (float x,float y)
#endif 
{
  int xI,yI;

  xI = x*precisionFactor;
  yI = y*precisionFactor;
  return(xI<yI);
}

int
#ifdef _NO_PROTO
greaterThan (x,y)
     float x,y;
#else
greaterThan (float x,float y)
#endif 
{
  int xI,yI;

  xI = x*precisionFactor;
  yI = y*precisionFactor;
  return(xI>yI);
}

int
#ifdef _NO_PROTO
isNaN (v)
     float v;
#else
isNaN (float v)
#endif 
{
  return (v != v);
}


int
#ifdef _NO_PROTO
isNaNPoint (x,y,z)
     float x,y,z;
#else
isNaNPoint (float x,float y,float z)
#endif 
{
  return (isNaN(x) || isNaN(y) || isNaN(z));
}

int
#ifdef _NO_PROTO
equal (x,y)
     float x,y;
#else
equal (float x,float y)
#endif 
{
  int xI,yI;

  xI = x*precisionFactor;
  yI = y*precisionFactor;
  return(xI==yI);
}

