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

#define _COMPONENT3D_C
#include "axiom-c-macros.h"

#include "header.h"
#include "draw.h"

#include "Gfun.H1"
#include "util.H1"
#include "XSpadFill.H1"

#include "all_3d.H1"

#define axisLength      1.0   /* use 100.0, if data is not to be normalized */

#define samePoint(a,b) ((refPt3D(viewData,a)->x == refPt3D(viewData,b)->x) &&\
                        (refPt3D(viewData,a)->y == refPt3D(viewData,b)->y) &&\
                        (refPt3D(viewData,a)->z == refPt3D(viewData,b)->z))
#define MAX_POINT       1000.0
#define MIN_POINT       -1000.0


void 
#ifdef _NO_PROTO
scaleComponents ()
#else
scaleComponents (void)
#endif
{
  
  double xRange,yRange,zRange;
  int i;
  viewTriple *aPoint;
  
  /* Temporary range limits until the three dimensional clipping
     package is fully functional */
  
  if (viewData.xmin < MIN_POINT) viewData.xmin = MIN_POINT;
  if (viewData.xmax > MAX_POINT) viewData.xmax = MAX_POINT;
  if (viewData.ymin < MIN_POINT) viewData.ymin = MIN_POINT;
  if (viewData.ymax > MAX_POINT) viewData.ymax = MAX_POINT;
  if (viewData.zmin < MIN_POINT) viewData.zmin = MIN_POINT;
  if (viewData.zmax > MAX_POINT) viewData.zmax = MAX_POINT;
  
  xRange = viewData.xmax - viewData.xmin;
  yRange = viewData.ymax - viewData.ymin;
  zRange = viewData.zmax - viewData.zmin;
  
  /* We scale down, normalize the data, if it is coming from AXIOM
     (handled by viewman).  If the data is coming from a file (handled by
     viewAlone) then it should already been scaled down. 
     */
  
  /* Find the coordinate axis with the larges range of data and scale
     the others relative to it. 
     */
  /* compare x and y ranges */
  if (xRange > yRange) {
    if (xRange > zRange) {
      if (absolute(viewData.xmax) >= absolute(viewData.xmin))
        viewData.scaleToView = axisLength/(absolute(viewData.xmax));
      else
        viewData.scaleToView = axisLength/(absolute(viewData.xmin));
    } else {
      if (absolute(viewData.zmax) >= absolute(viewData.zmin))
        viewData.scaleToView = axisLength/(absolute(viewData.zmax));
      else
        viewData.scaleToView = axisLength/(absolute(viewData.zmin));
    }
  } else {
    if (yRange > zRange) {
      if (absolute(viewData.ymax) >= absolute(viewData.ymin))
        viewData.scaleToView = axisLength/(absolute(viewData.ymax));
      else
        viewData.scaleToView = axisLength/(absolute(viewData.ymin));
    } else {
      if (absolute(viewData.zmax) >= absolute(viewData.zmin))
        viewData.scaleToView = axisLength/(absolute(viewData.zmax));
      else
        viewData.scaleToView = axisLength/(absolute(viewData.zmin));
    }
  }
  
  /* We now normalize all the points in this program. The information
     needed to link the normalized set of points back to the real object
     space scale created in AXIOM is held in viewData.scaleToView. */
  viewData.xmin *= viewData.scaleToView;
  viewData.xmax *= viewData.scaleToView;
  viewData.ymin *= viewData.scaleToView;
  viewData.ymax *= viewData.scaleToView;
  viewData.zmin *= viewData.scaleToView;
  viewData.zmax *= viewData.scaleToView;
  viewData.clipXmin = viewData.xmin;
  viewData.clipXmax = viewData.xmax;
  viewData.clipYmin = viewData.ymin;
  viewData.clipYmax = viewData.ymax;
  viewData.clipZmin = viewData.zmin;
  viewData.clipZmax = viewData.zmax;
  
  for (i=0, aPoint=viewData.points; i<viewData.numOfPoints; i++,aPoint++) {
    aPoint->x *= viewData.scaleToView;
    aPoint->y *= viewData.scaleToView;
    aPoint->z *= viewData.scaleToView;
  }
  
} /* scaleComponents() */


/*
 void makeTriangle(a,b,c)   
 Given three indices to three points, a triangular polygon is created 
 and inserted into the polygon list of viewData. If two or more of the 
 points are coincidental, no polygon is created since that would be a 
 degenerate (collapsed) polygon.
 */

void
#ifdef _NO_PROTO
makeTriangle (a, b, c)
     int a,b,c;
#else
makeTriangle (int a, int b, int c)
#endif
{
  poly *aPoly;
  
  if (!(samePoint(a,b) || samePoint(b,c) || samePoint(c,a))) {
    /* create triangle only if the three vertex points are distinct */
    aPoly = (poly *)saymem("component.c",1,sizeof(poly));
    aPoly->num = aPoly->sortNum = viewData.numPolygons++;
    aPoly->split = aPoly->moved = no;
    aPoly->numpts = 3;
    aPoly->primitiveType = polygonComponent;
    aPoly->indexPtr = (int *)saymem("component.c",3,sizeof(int));
    *(aPoly->indexPtr)     = a;
    *(aPoly->indexPtr + 1) = b;
    *(aPoly->indexPtr + 2) = c;
    aPoly->doNotStopDraw = yes;
    aPoly->next = viewData.polygons;
    viewData.polygons = aPoly;
  } /* if all points are unique */
  
} /* makeTriangle() */




/*
  void triangulate()
 
  Only if there is more than one list do we triangulate; a single list 
  is used for either a space curve or simply a point.  Actually, in that 
  case, we now make "flat" *polygons, flagged by the primitiveType field 
  (pointComponent, etc. in tube.h).  We need to examine two lists at a time
  (and if the structure is closed, the last and first as well). For every 
  three points in the two lists, alternating between one in one and two in
  the other, we construct triangles. If one list is shorter, then its last 
  point becomes the vertex for the remaining pairs of points from the other
  list.  It turns out that any distribution of points in the two lists 
  (preserving cyclic order) will produce the same desired polygon.
  */

void
#ifdef _NO_PROTO
triangulate ()
#else
triangulate (void)
#endif
{
  
  int u,l;
  int uBound,lBound;
  int i,j,k;
  LLPoint *anLLPoint;
  LPoint *list1,*list2;
  poly *aPoly;
  
  anLLPoint = viewData.lllp.llp;
  for (i=0; i<viewData.lllp.numOfComponents; i++,anLLPoint++) {
    if (anLLPoint->numOfLists > 1) {
      list2 = anLLPoint->lp;
      for (j=1; j<anLLPoint->numOfLists; j++) {
        list1 = list2;
        list2 = list1 + 1;
        u = l = 0;
        uBound = u+1 < list1->numOfPoints;
        lBound = l+1 < list2->numOfPoints;
        while (uBound || lBound) {
          if (uBound) {
            makeTriangle(*(list1->indices + u + 1),
                         *(list1->indices + u), *(list2->indices + l));
            u++;
            uBound = u+1 < list1->numOfPoints;
          }
          if (lBound) {
            makeTriangle(*(list2->indices + l),
                         *(list2->indices + l + 1), *(list1->indices + u));
            l++;
            lBound = l+1 < list2->numOfPoints;
          }
        } /* while (uBound || lBound) */
      } /* for j<anLLPoint->numOfLists */
    } /* if anLLPoint->numOfLists > 1 */ 
    else {
      /* if anLLPoint->numOfLists <= 1...assume this means =1 */
      /* Flat polygons are to be drawn when hidden 
         surface algorithm is used.*/
      if (anLLPoint->numOfLists == 1) {
        if (anLLPoint->lp->numOfPoints == 1) {
          /* this graph is a single point */
          aPoly = (poly *)saymem("component.c",1,sizeof(poly));
          aPoly->num = aPoly->sortNum = viewData.numPolygons++;
          aPoly->split = aPoly->moved = no;
          aPoly->primitiveType = pointComponent;
          aPoly->numpts = 1;
          aPoly->indexPtr = (int *)saymem("component.c",1,intSize);
          *(aPoly->indexPtr) = *(anLLPoint->lp->indices);
          aPoly->doNotStopDraw = yes;
          aPoly->next = viewData.polygons;
          viewData.polygons = aPoly;
        } else {
          /* this graph is a curve */
          for (k=0; k<anLLPoint->lp->numOfPoints-1; k++) {
            aPoly = (poly *)saymem("component.c",1,sizeof(poly));
            aPoly->num = aPoly->sortNum = viewData.numPolygons++;
            aPoly->split = aPoly->moved = no;
            aPoly->primitiveType = lineComponent; /* curveComponent */
            aPoly->numpts = 2;
            aPoly->indexPtr =
              (int *)saymem("component.c",2,sizeof(int));
            *(aPoly->indexPtr)   = *(anLLPoint->lp->indices + k);
            *(aPoly->indexPtr+1) = *(anLLPoint->lp->indices + k + 1);
            aPoly->doNotStopDraw = yes;
            aPoly->next = viewData.polygons;
            viewData.polygons = aPoly;
          }  /* for k */
          if (anLLPoint->lp->prop.closed) {
            aPoly = (poly *)saymem("component.c",1,sizeof(poly));
            aPoly->num = aPoly->sortNum = viewData.numPolygons++;
            aPoly->split = aPoly->moved = no;
            aPoly->primitiveType = lineComponent; /* curveComponent */
            aPoly->numpts = 2;
            aPoly->indexPtr =
              (int *)saymem("component.c",2,sizeof(int));
            *(aPoly->indexPtr)   = *(anLLPoint->lp->indices + k);
            *(aPoly->indexPtr+1) = *(anLLPoint->lp->indices);
            aPoly->doNotStopDraw = yes;
            aPoly->next = viewData.polygons;
            viewData.polygons = aPoly;
          } /* if list of points is closed */
        }  /* else */
      } /* point, line, polygon, surface components are taken care of above */
    } /* else anLLPoint->numOfLists <= 1 */
  } /* for LLPoints in LLLPoints (i) */
  
} /* triangulate */



void
#ifdef _NO_PROTO
readComponentsFromViewman ()
#else
readComponentsFromViewman (void)
#endif
{
  int i,j,k;
  LLPoint *anLLPoint;
  LPoint *anLPoint;
  viewTriple *aPoint;
  /* maxLength holds the max(llp,lp) figure regarding how large to
     make the array of XPoints, i.e. quadMesh, for use in calling XDraw(). */
  int maxLength=0;
  
  int *anIndex;
  
  readViewman(&(viewData.numOfPoints),intSize);
  aPoint = viewData.points =
    (viewTriple *)saymem("component.c",viewData.numOfPoints,
                         sizeof(viewTriple));
  for (i=0; i<viewData.numOfPoints; i++, aPoint++) {
    readViewman(&(aPoint->x),floatSize);
    readViewman(&(aPoint->y),floatSize);
    readViewman(&(aPoint->z),floatSize);
    readViewman(&(aPoint->c),floatSize);
#ifdef NANQ_DEBUG
    if (!(aPoint->z < 0) && !(aPoint->z > 0) && !(aPoint->z == 0))
      fprintf(stderr,"%g\n", aPoint->z);
#endif
  }
  
  readViewman(&(viewData.lllp.numOfComponents),intSize);
  anLLPoint = viewData.lllp.llp =
    (LLPoint *)saymem("component.c, i",viewData.lllp.numOfComponents,
                      sizeof(LLPoint));
  for (i=0; i<viewData.lllp.numOfComponents; i++,anLLPoint++) {
    readViewman(&(anLLPoint->prop.closed),intSize);
    readViewman(&(anLLPoint->prop.solid),intSize);
    readViewman(&(anLLPoint->numOfLists),intSize);
    anLPoint = anLLPoint->lp =
      (LPoint *)saymem("component.c, ii",anLLPoint->numOfLists,
                       sizeof(LPoint));
    for (j=0; j<anLLPoint->numOfLists; j++,anLPoint++) {
      if (anLLPoint->numOfLists > maxLength)
        maxLength = anLLPoint->numOfLists;
      readViewman(&(anLPoint->prop.closed),intSize);
      readViewman(&(anLPoint->prop.solid),intSize);
      readViewman(&(anLPoint->numOfPoints),intSize);
      anIndex = anLPoint->indices =
        (int *)saymem("component.c, index",anLPoint->numOfPoints,intSize);
      if (anLPoint->numOfPoints > maxLength)
        maxLength = anLPoint->numOfPoints;
      for (k=0; k<anLPoint->numOfPoints; k++,anIndex++) {
        readViewman(anIndex,intSize);
        /* AXIOM arrays are one based, C arrays are zero based */
        if (!viewAloned) (*anIndex)--;
      }
    } /* for LPoints in LLPoints (j) */
  } /* for LLPoints in LLLPoints (i) */
  
  quadMesh = (XPoint *)saymem("component.c",maxLength+2,sizeof(XPoint));
  
} /* readComponentsFromViewman() */



/*
  void calcNormData()       *
  Calculates the surface normals for the polygons that make up the tube.
  Also finds the fourth coefficient to the plane equation:
  Ax + By + Cz + D = 0  
  A,B, and C are in the normal N[3] and D is the planeConst.
  Figures out the color as well (from the average of the points) and 
  resets the moved flag    
  */

void
#ifdef _NO_PROTO
calcNormData ()
#else
calcNormData (void)
#endif
{
  
  poly *aPoly;
  int *index;
  
  for (aPoly = viewData.polygons; aPoly != NIL(poly); aPoly = aPoly->next) {
    index = aPoly->indexPtr;
    switch (aPoly->primitiveType) {
    case pointComponent:
    case lineComponent:
      aPoly->moved = 0;
      aPoly->color = refPt3D(viewData,*index)->c;
      break;
    default:
      /*
        The following line takes 3 consecutive points and asks
        for the normal vector defined by them. This assumes that
        these do not contain co-linear points. For some reason,
        co-linear points are allowed, this needs to be changed.
        */
      getMeshNormal(refPt3D(viewData,*index)->x,
                    refPt3D(viewData,*index)->y,
                    refPt3D(viewData,*index)->z,
                    refPt3D(viewData,*(index+1))->x,
                    refPt3D(viewData,*(index+1))->y,
                    refPt3D(viewData,*(index+1))->z,
                    refPt3D(viewData,*(index+2))->x,
                    refPt3D(viewData,*(index+2))->y,
                    refPt3D(viewData,*(index+2))->z, 0.0, 1.0, aPoly->N);
      
      /* calculate the constant term, D,  for the plane equation */
      aPoly->planeConst =
        -(aPoly->N[0] * refPt3D(viewData,*index)->x +
          aPoly->N[1] * refPt3D(viewData,*index)->y +
          aPoly->N[2] * refPt3D(viewData,*index)->z);
      aPoly->moved = 0;
      aPoly->color = (refPt3D(viewData,*index)->c +
                      (refPt3D(viewData,*(index+1)))->c +
                      (refPt3D(viewData,*(index+2)))->c) / 3.0;
      break;
    } /* switch */
  }
  
}  /* calcNormData() */



/*
  viewPoints *make3DComponents()

  Read in all the 3D data from the viewport manager and construct the 
  model of it. The model is based upon a list of lists of lists of points. 
  Each top level list makes a component in 3-space. The interpretation 
  really begins at the level below that, where the list of lists of 
  points is. For 3D explicit equations of two variables, the closed 
  boolean for this level is False and the closed boolean for each sublist 
  is False as well. For 3D parameterized curves of one variable, the 
  closed boolean for this level is defined by the user from AXIOM , 
  (which defaults to False) and the closed boolean for each sublist is True.
  */

viewPoints *
#ifdef _NO_PROTO
make3DComponents ()
#else
make3DComponents (void)
#endif
{
  viewPoints *graphData;

  readComponentsFromViewman();

  /* The initial boundaries for the clipping region are set to those
     of the boundaries of the data region. */
  viewData.clipXmin = viewData.xmin;  viewData.clipXmax = viewData.xmax;
  viewData.clipYmin = viewData.ymin;  viewData.clipYmax = viewData.ymax;
  viewData.clipZmin = viewData.zmin;  viewData.clipZmax = viewData.zmax;
  
  /* normalize the data coordinates */
  if (viewData.scaleDown) scaleComponents();
  viewData.numPolygons = 0;
  /* initially the list of polygons is empty */
  viewData.polygons = NIL(poly);
  /* create the polygons; (sets viewData.polygons and viewData.numPolygons) */
  triangulate();
  /* calculate the plane equations for all the polygons */
  calcNormData();
  
  graphData = makeViewport();
  
  imageX = XCreateImage(/* display */        dsply,
                        /* visual */         DefaultVisual(dsply,scrn),
                        /* depth */          DefaultDepth(dsply,scrn),
                        /* format */         ZPixmap,
                        /* offset */         0,
                        /* data */           NULL,
                        /* width */          vwInfo.width,
                        /* height */         1,
                        /* bitmap_pad */     32,
                        /* bytes_per_line */ 0);
  imageX->data = NIL(char);
  
  /* windowing displaying */
  writeTitle();
  postMakeViewport();
  drawViewport(Xoption);
  firstTime = yes;
  XMapWindow(dsply, graphData->viewWindow);
  XMapWindow(dsply, graphData->titleWindow);
  XFlush(dsply);
  
  return(graphData);
  
} /* make3DComponents */





void
#ifdef _NO_PROTO
draw3DComponents (dFlag)
     int dFlag;
#else
draw3DComponents (int dFlag)
#endif
{
  
  int        i, j, k, hue, x1, y1, x2, y2;
  LLPoint    *anLLPoint;
  LPoint     *anLPoint;
  int        *anIndex;
  int        componentType;  /* what the component is to be interpreted as */
  int        clip_a,clip_i;  /* for use in wire mesh mode clipping */
  XEvent     peekEvent;
  viewTriple *aLPt;
  XPoint     line[2];
  RGB        col_rgb;
  
  calcEyePoint();
  while ((XPending(dsply) > 0) && (scanline > 0))
    XNextEvent(dsply,&peekEvent);
  switch (viewData.style) {
    
  case transparent:
    GSetLineAttributes(componentGC,0,LineSolid,CapButt,JoinMiter,dFlag);
    if (dFlag==Xoption) {
      if (mono || viewport->monoOn)
        GSetForeground(componentGC, (float)foregroundColor, dFlag);
      else
        GSetForeground(componentGC, (float) meshOutline, dFlag);
    } else {
      GSetForeground(componentGC, psBlack, dFlag);
    }
    /* no need to check "keep drawing" for ps */
    if (dFlag == Xoption) drawMore = keepDrawingViewport();
    
    /*
      This is where we interpret the list of lists of lists of points struct.
      We want to extract the following forms of data:
      - individual points (drawn as filled points)
      - lines (space curves)
      - defined polygon primitives
      - surfaces
      the last one is the one that will replace the function of 2 variables,
      tubes as well as 3D parameterized functions of 2 variables.
      Since there could be many other ways of constructing L L L Pts - much
      more than could be usefully interpreted - any other formats are
      currently not allowed. When they are, this comment should be updated
      appropriately.

      ************************************************************************
      
      Traverse each component.
      We decide here, before we begin traversing the
      component what we want to interpret it as.
      Here's the convention used to figure that out:
      - points: #anLLPoint->numOfLists was 1
      #anLPoint->numOfPoints is 1
      - lines:  #anLLPoint->numOfLists was 1
      #anLPoint->numOfPoints > 1
      - polygons: #anLLPoint->numOfLists was 2
      #anLPoint->numOfPoints is 1
      - surface:        #anLLPoint->numOfLists was some m>1
      #anLPoint->numOfPoints all point lists are the same.
      
      */

    anLLPoint = viewData.lllp.llp;
    for (i=0; i<viewData.lllp.numOfComponents; i++,anLLPoint++) {
      /* initially, component type is unknown */
      componentType = stillDontKnow;
      if (anLLPoint->numOfLists == 1) {
        if (anLLPoint->lp->numOfPoints == 1) componentType = pointComponent;
        else componentType = lineComponent;
      } else if (anLLPoint->numOfLists == 2) {
        if ((anLLPoint->lp->numOfPoints == 1) &&
            ((anLLPoint->lp+1)->numOfPoints > 2))
          componentType = polygonComponent;
      }
      /* Check for corrupt data and NaN data is made in AXIOM . */
      if (componentType == stillDontKnow)
        componentType = surfaceComponent;
      
      anLPoint = anLLPoint->lp;
      
      switch (componentType) {
        
      case pointComponent:
        /* anLLPoint->numOfLists == anLLPoint->lp->numOfPoints == 1 here */
        aLPt = refPt3D(viewData,*(anLPoint->indices));
        project(aLPt,quadMesh,0);
        if (dFlag==Xoption) {
          if (mono || viewport->monoOn)
            GSetForeground(componentGC, (float)foregroundColor, dFlag);
          else {
            hue = hueValue(aLPt->c);
            GSetForeground(componentGC, (float)XSolidColor(hue,2), dFlag);
          }
        } else GSetForeground(componentGC, psBlack, dFlag);
        GFillArc(componentGC,viewport->viewWindow,quadMesh->x,quadMesh->y,
                 viewData.pointSize,viewData.pointSize,0,360*64,dFlag);
        break;
        
      case lineComponent:
        /* anLLPoint->numOfLists == 1 here */
        anIndex = anLPoint->indices;
        aLPt = refPt3D(viewData,*anIndex);
        project(aLPt,quadMesh,0);
        x1 = quadMesh[0].x;  y1 = quadMesh[0].y;  anIndex++;
        for (k=1; k<anLPoint->numOfPoints; k++,anIndex++) {
          aLPt = refPt3D(viewData,*anIndex);
          project(aLPt,quadMesh,k);
          x2 = quadMesh[k].x; y2 = quadMesh[k].y;
          if (dFlag==Xoption) {
            if (mono || viewport->monoOn)
              GSetForeground(componentGC, (float)foregroundColor, dFlag);
            else {
              hue = hueValue(aLPt->c);
              GSetForeground(componentGC, (float)XSolidColor(hue,2), dFlag);
            }
            if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
              GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
          } else {
            if (dFlag==PSoption && !mono && !viewport->monoOn) {
              hue = getHue(aLPt->c);
              col_rgb = hlsTOrgb((float)hue,0.5,0.8);
              line[0].x = x1;  line[0].y = y1;
              line[1].x = x2;  line[1].y = y2;
              PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,line,2);
            } else {
              if (foregroundColor == white)
                GSetForeground(componentGC, 0.0, dFlag);
              else
                GSetForeground(componentGC, psBlack, dFlag);
              if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
                GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
            }
          }
          x1 = x2; y1 = y2;
        } /* for points in LPoints (k) */
        if (anLPoint->prop.closed) {
          project(refPt3D(viewData,*(anLPoint->indices)),quadMesh,
                  anLPoint->numOfPoints);
          x2 = quadMesh[anLPoint->numOfPoints].x;
          y2 = quadMesh[anLPoint->numOfPoints].y;
          if (dFlag==Xoption) {
            if (mono || viewport->monoOn)
              GSetForeground(componentGC, (float)foregroundColor, dFlag);
            else {
              hue = hueValue(aLPt->c);
              GSetForeground(componentGC, (float)XSolidColor(hue,2), dFlag);
            }
            if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
              GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
          } 
          else {
            if (dFlag==PSoption && !mono && !viewport->monoOn) {
              hue = getHue(aLPt->c);
              col_rgb = hlsTOrgb((float)hue,0.5,0.8);
              line[0].x = x1;  line[0].y = y1;
              line[1].x = x2;  line[1].y = y2;
              PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,line,2);
            } 
            else {
              if (foregroundColor == white)
                GSetForeground(componentGC, 0.0, dFlag);
              else
                GSetForeground(componentGC, psBlack, dFlag);
              if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
                GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
            }
          }
        }
        break;
        
      case polygonComponent:
        /* first pt of polygon is a single list */
        project(refPt3D(viewData,*(anLPoint->indices)),quadMesh,0);
        /* remaining points in the 2nd list (always of size 2 or greater) */
        x1 = quadMesh[0].x;  y1 = quadMesh[0].y;
        anLPoint = anLLPoint->lp + 1;
        anIndex = anLPoint->indices;
        for (k=1; k<=anLPoint->numOfPoints; k++,anIndex++) {
          aLPt = refPt3D(viewData,*anIndex);
          project(aLPt,quadMesh,k);
          x2 = quadMesh[k].x;  y2 = quadMesh[k].y;
          if (dFlag==Xoption) {
            if (mono || viewport->monoOn)
              GSetForeground(componentGC, (float)foregroundColor, dFlag);
            else {
              hue = hueValue(aLPt->c);
              GSetForeground(componentGC, (float)XSolidColor(hue,2), dFlag);
            }
            if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
              GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
          } 
          else {
            if (dFlag==PSoption && !mono && !viewport->monoOn) {
              hue = getHue(aLPt->c);
              col_rgb = hlsTOrgb((float)hue,0.5,0.8);
              line[0].x = x1;  line[0].y = y1;
              line[1].x = x2;  line[1].y = y2;
              PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,line,2);
            } 
            else {
              if (foregroundColor == white)
                GSetForeground(componentGC, 0.0, dFlag);
              else
                GSetForeground(componentGC, psBlack, dFlag);
              if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
                GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
            }
          }
          x1 = x2; y1 = y2;
        } /* for points in LPoints (k) */
        project(refPt3D(viewData,*(anLLPoint->lp->indices)),quadMesh,k);
        x2 = quadMesh[k].x;  y2 = quadMesh[k].y;
        if (dFlag==Xoption) {
          if (mono || viewport->monoOn)
            GSetForeground(componentGC, (float)foregroundColor, dFlag);
          else {
            hue = hueValue(refPt3D(viewData,*anIndex)->c);
            GSetForeground(componentGC, (float)XSolidColor(hue,2), dFlag);
          }
          if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
            GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
        } else {
          if (dFlag==PSoption && !mono && !viewport->monoOn) {
            hue = getHue(refPt3D(viewData,*anIndex)->c);
            col_rgb = hlsTOrgb((float)hue,0.5,0.8);
            line[0].x = x1;  line[0].y = y1;
            line[1].x = x2;  line[1].y = y2;
            PSDrawColor(col_rgb.r,col_rgb.g,col_rgb.b,line,2);
          } 
          else {
            if (foregroundColor == white)
              GSetForeground(componentGC, 0.0, dFlag);
            else
              GSetForeground(componentGC, psBlack, dFlag);
            if (!eqNANQ(x1) && !eqNANQ(y1) && !eqNANQ(x2) && !eqNANQ(y2))
              GDrawLine(componentGC,viewport->viewWindow,x1,y1,x2,y2,dFlag);
          }
        }
        /* close a polygon */
        break;
        
      case surfaceComponent:
        if (dFlag==Xoption) {
          if (mono || viewport->monoOn)
            GSetForeground(componentGC, (float)foregroundColor, dFlag);
          else
            GSetForeground(componentGC, (float) meshOutline, dFlag);
        }
        else {
          GSetForeground(componentGC, psBlack, dFlag);
        }
        
        /* traverse down one direction first (all points 
           in a list at a time) */
        for (j=0; drawMore && j<anLLPoint->numOfLists; j++,anLPoint++) {
          anIndex = anLPoint->indices;
          clip_a = 0;
          for (k=0, clip_i=0;
               drawMore && k<anLPoint->numOfPoints;
               k++, anIndex++, clip_i++) {
            aLPt = refPt3D(viewData,*anIndex);
            project(aLPt,quadMesh,k);
            
            if (behindClipPlane(aLPt->pz) || 
                (viewData.clipStuff && 
                 outsideClippedBoundary(aLPt->x, aLPt->y, aLPt->z))) {
              if (clip_i - clip_a > 1) {
                GDrawLines(componentGC,viewport->viewWindow,(quadMesh+clip_a),
                           clip_i-clip_a, CoordModeOrigin, dFlag );
              }
              clip_a = clip_i + 1;
            }
            
            drawMore = keepDrawingViewport();
          } /* for points in LPoints (k) */
          if (drawMore) {
            /* if drawMore is true, then the above loop terminated with
               clip_i incremented properly */
            if (anLPoint->prop.closed) {
              /* If closed, then do the first point again - no need to project
                 just copy over from the first one */
              aLPt = refPt3D(viewData,*(anLPoint->indices));
              project(aLPt,quadMesh, anLPoint->numOfPoints);
              if (behindClipPlane(aLPt->pz) || 
                  (viewData.clipStuff &&
                   outsideClippedBoundary(aLPt->x, aLPt->y, aLPt->z))) {
                if (clip_i - clip_a > 1) {
                  GDrawLines(componentGC, viewport->viewWindow,
                             (quadMesh+clip_a), clip_i-clip_a,
                             CoordModeOrigin, dFlag);
                }
                clip_a = clip_i + 1;
              }
              clip_i++;
            } /* closed */
            if (clip_i - clip_a > 1) {
              GDrawLines(componentGC, viewport->viewWindow, (quadMesh+clip_a),
                         clip_i-clip_a, CoordModeOrigin, dFlag);
            }
          } /* drawMore */
        } /* for LPoints in LLPoints (j) */
        
        /* now traverse down the list in the other direction
           (one point from each list at a time) */
        for (j=0; drawMore && j<anLLPoint->lp->numOfPoints; j++) {
          clip_a = 0;
          for (k=0, clip_i=0;
               drawMore && k<anLLPoint->numOfLists;
               k++, clip_i++) {
            aLPt = refPt3D(viewData,*((anLLPoint->lp + k)->indices + j));
            project(aLPt, quadMesh,k);
            
            if (behindClipPlane(aLPt->pz) || 
                (viewData.clipStuff &&
                 outsideClippedBoundary(aLPt->x, aLPt->y, aLPt->z))) {
              if (clip_i - clip_a > 1) {
                GDrawLines(componentGC,viewport->viewWindow,quadMesh+clip_a,
                           clip_i-clip_a, CoordModeOrigin, dFlag );
              }
              clip_a = clip_i + 1;
            }
            drawMore = keepDrawingViewport();
          } /* for points in LPoints (k) */
          
          if (drawMore) {
            /* if drawMore is true, then the above loop terminated with
               clip_i incremented properly */
            if (anLLPoint->prop.closed) {
              /* if closed, do the first point again - no need to project
                 just copy over from the first one */
              aLPt = refPt3D(viewData,*((anLLPoint->lp + 0)->indices + j));
              project(aLPt, quadMesh, anLLPoint->numOfLists);
              if (behindClipPlane(aLPt->pz) || 
                  (viewData.clipStuff &&
                   outsideClippedBoundary(aLPt->x, aLPt->y, aLPt->z))) {
                if (clip_i - clip_a > 1) {
                  GDrawLines(componentGC, viewport->viewWindow,
                             quadMesh + clip_a, clip_i - clip_a,
                             CoordModeOrigin, dFlag);
                }
                clip_a = clip_i + 1;
              }
              clip_i++;
            } /* closed */
            if (clip_i - clip_a > 1) {
              GDrawLines(componentGC, viewport->viewWindow, quadMesh+clip_a,
                         clip_i-clip_a, CoordModeOrigin, dFlag);
            }
          } /* drawMore */
        } /* for a point in each LPoint (j) */
        break;
      } /* switch componentType */
    } /* for LLPoints in LLLPoints (i) */
    break;
    
  case opaqueMesh:
    if (dFlag==Xoption) {
      GSetForeground(globGC, (float)opaqueForeground, dFlag);
      GSetForeground(opaqueGC, (float)opaqueOutline, dFlag);
    }
    else {
      GSetForeground(globGC, psBlack, dFlag);
      GSetForeground(opaqueGC, psBlack, dFlag);
    }
    GSetLineAttributes(opaqueGC,0,LineSolid,CapButt,JoinRound,dFlag);
    drawPolygons(dFlag);
    break;
    
  case render:
    if (viewData.outlineRenderOn) {
      GSetLineAttributes(renderGC,0,LineSolid,CapButt,JoinRound,dFlag);
      if (dFlag==Xoption) GSetForeground(renderGC,(float)black, dFlag);
      else GSetForeground(renderGC,psBlack, dFlag );
    }
    drawPolygons(dFlag);
    break;
    
  case smooth:
    drawPhong(dFlag);
    break;
    
  } /* switch on style */
  
} /* draw3DComponents() */

