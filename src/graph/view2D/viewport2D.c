/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2024, Gabriel Dos Reis.
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

#define _VIEWPORT2D_C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <limits.h>

#include "open-axiom.h"

#define NotPoint        (SHRT_MAX)
#define eqNANQ(x)       (x == NotPoint)

#include "header2.h"

#include "all_2d.H1"
#include "Gfun.H1"
#include "util.H1"
#include "XSpadFill.h"

#include "spadBitmap.bitmap"
#include "spadMask.mask"

using namespace OpenAxiom;

#define rint(z) ((int)(z))

Atom    wm_delete_window;               


/*************************** 
 ***  void writeTitle()  *** 
 ***************************/

void 
writeTitle(void)
{

  int strlength;
  XWindowAttributes attribInfo;
  
  XGetWindowAttributes(dsply,viewport->titleWindow,&attribInfo);
  if (mono) GSetForeground(anotherGC,(float)foregroundColor,Xoption);
  else GSetForeground(anotherGC,(float)titleColor,Xoption);
  XClearWindow(dsply,viewport->titleWindow); /* it's behind the viewWindow */
  strlength = strlen(viewport->title);
  GDrawImageString(anotherGC,viewport->titleWindow,
              centerX(anotherGC,viewport->title,strlength,attribInfo.width),
              15,viewport->title,strlength,Xoption);

}


/********************************/
/***  void drawTheViewport()  ***/
/********************************/

void 
drawTheViewport(int dFlag)
{

  Window            vw;
  XWindowAttributes vwInfo;
  pointListStruct   *aList;
  pointStruct       *aPoint;
  XPoint            *anXPoint,*tempXpt;
  Vertex            *anX10Point;
  float             jj,diffX, diffY, tickStart,oneTickUnit;
  int               i,j,k,ii,halfSize;
  int               charlength,strlength,halflength;
  int               ptX,ptY,ptX1,ptY1,clipped, clipped1;
  int               xAxis,yAxis,dummyInt, ascent, descent;
  int               unitWidth,boxX,boxY,boxW,boxH;
  char              aunit[20];
  XCharStruct       overall;

  drawMore = yes;
  vw = viewport->viewWindow;
  XGetWindowAttributes(dsply,vw,&vwInfo);
  aspectR = (float)vwInfo.width/(float)vwInfo.height;

  XTextExtents(unitFont,"o",1,&dummyInt,&ascent,&descent,&overall);

  /* Calculate various factors for use in projection. */
  /* Scale the plot, so that the scaling between the axes remains 
     constant and fits within the smaller of the two dimensions. */

  charlength = overall.width;

  if (dFlag==Xoption) XClearWindow(dsply,vw);

  for (i=0; i<maxGraphs; i++) {

    if ((graphArray[i].key) && (graphStateArray[i].showing)) {

      /* Scale y coordinate dimensions relative to viewport aspect ratio. */
     
      graphArray[i].yNorm = 1.0/((graphArray[i].ymax-graphArray[i].ymin) *
                                 aspectR);
      graphArray[i].originY = -graphArray[i].ymin*graphArray[i].yNorm 
        - 0.5/aspectR;
      graphArray[i].unitY = graphArray[i].spadUnitY*graphArray[i].yNorm;

      xAxis = rint(vwInfo.width *
                   ((graphArray[0].originX - graphStateArray[0].centerX) *
                    graphStateArray[0].scaleX + 0.5));
      yAxis= rint(vwInfo.height * aspectR *
                  (1 - ((graphArray[0].originY*aspectR - 
                         graphStateArray[0].centerY) *
                        graphStateArray[0].scaleY + 0.5*aspectR )));

      if (graphStateArray[i].axesOn) {
        if (dFlag==Xoption) /* do only for X, ps uses default of black */
          GSetForeground(globalGC1,
                         (float)monoColor(graphStateArray[i].axesColor),
                         dFlag);
                         
        if ((yAxis >=0) && (yAxis <= vwInfo.height))
          GDrawLine(globalGC1,vw,
                    0,yAxis,
                    vwInfo.width,yAxis,
                    dFlag);
        if ((xAxis >=0) && (xAxis <= vwInfo.width))
          GDrawLine(globalGC1,vw,
                    xAxis,0,
                    xAxis,vwInfo.height,
                    dFlag);
      }
        

      tempXpt   = anXPoint   = xPointsArray[i].xPoint;
      anX10Point = xPointsArray[i].x10Point;

      for (j=0,aList=graphArray[i].listOfListsOfPoints;
           (j<graphArray[i].numberOfLists);
           j++, aList++) {

        for (k=0,aPoint=aList->listOfPoints;
             (k<aList->numberOfPoints); 
             k++,aPoint++) {

          if (graphStateArray[i].scaleX > 99.0)
            graphStateArray[i].scaleX = 99.0;
          if (graphStateArray[i].scaleY > 99.0)
            graphStateArray[0].scaleY = 99.0;
          if (i > 0) {
            if (isNaN(aPoint->x)) {
              anXPoint->x = anX10Point->x = NotPoint;
            }
            else {
              diffX = graphArray[i].xmax-graphArray[i].xmin;
              anXPoint->x = anX10Point->x = vwInfo.width *
                ((aPoint->x * diffX/(graphArray[0].xmax-graphArray[0].xmin)
                  + (graphArray[0].originX - graphArray[i].originX*diffX /
                     (graphArray[0].xmax-graphArray[0].xmin))
                  - graphStateArray[0].centerX)*graphStateArray[i].scaleX+0.5);
            }
            if (isNaN(aPoint->y)) {
              anXPoint->y = anX10Point->y = NotPoint;
            }
            else {
              diffY = graphArray[i].ymax-graphArray[i].ymin;
              anXPoint->y = anX10Point->y = vwInfo.height * aspectR *
                (1 - ((aPoint->y * diffY/(graphArray[0].ymax-graphArray[0].ymin)
                       + (graphArray[0].originY - graphArray[i].originY* diffY/
                          (graphArray[0].ymax-graphArray[0].ymin))*aspectR
                       - graphStateArray[0].centerY) *
                      graphStateArray[i].scaleY + 0.5*aspectR));
            }
          } else {
            if (isNaN(aPoint->x)) {
              anXPoint->x = anX10Point->x = NotPoint;
            }
            else {
              anXPoint->x = anX10Point->x = vwInfo.width *
                ((aPoint->x - graphStateArray[i].centerX) * 
                 graphStateArray[i].scaleX + 0.5);
            }
            if (isNaN(aPoint->y)) {
              anXPoint->y = anX10Point->y = NotPoint;
            }
            else {
              anXPoint->y = anX10Point->y = vwInfo.height * aspectR *
                (1 - ((aPoint->y - graphStateArray[i].centerY) * 
                      graphStateArray[i].scaleY + 0.5*aspectR));
            }
          }

          /* first or last point */ 
          if (k == 0 || k == (aList->numberOfPoints - 1)) {
            anX10Point->flags = 0;
          } else {
            anX10Point->flags = VertexCurved;
          }

          anXPoint++;
          anX10Point++;
        }      /* for aPoint in pointList */
          
        aPoint--; /* make it legal, the last one*/
        if (graphStateArray[i].connectOn || graphStateArray[i].pointsOn) {
          halfSize = aList->pointSize/2;
          ptX = tempXpt->x;
          ptY = tempXpt->y;
          clipped = ptX > vwInfo.x && ptX < vwInfo.width &&
            ptY > 0 && ptY < vwInfo.height;
          if (graphStateArray[i].pointsOn) {
            if (dFlag==Xoption) {
              if (mono) {
                GSetForeground(globalGC1,
                               (float)monoColor((int)(aPoint->hue)),
                               dFlag);
              } else {
                GSetForeground(globalGC1,
                               (float)XSolidColor((int)(aPoint->hue),
                                                  (int)(aPoint->shade)),
                               dFlag);
              }
            }
            if (clipped && !eqNANQ(ptX) && !eqNANQ(ptY))
              GFillArc(globalGC1,vw,ptX-halfSize,
                       ptY-halfSize,aList->pointSize,aList->pointSize,
                       0,360*64, dFlag);
                      
          } /* if points on */
          for (ii=0, aPoint=aList->listOfPoints; 
               ii<aList->numberOfPoints;
               ++ii, ++tempXpt, ++aPoint) {
            ptX1 = tempXpt->x;
            ptY1 = tempXpt->y;
            clipped1 = ptX1 > vwInfo.x && ptX1 < vwInfo.width &&
              ptY1 > 0 && ptY1 < vwInfo.height;
            if (graphStateArray[i].connectOn) {
              if (dFlag==Xoption) {
                if (mono) {
                  GSetForeground(globalGC1,
                         (float)monoColor((int)(aList->lineColor-1)/5),
                         dFlag);
                } else {
                  GSetForeground(globalGC1,
                         (float)XSolidColor((int)(aList->lineColor-1)/5,
                                            (int)((aList->lineColor-1)%5)/2),
                                 dFlag);
                }
              } /* if X */
              if ((clipped || clipped1) && !eqNANQ(ptX) && !eqNANQ(ptY) &&
                  !eqNANQ(ptX1) && !eqNANQ(ptY1))
                GDrawLine(globalGC1,vw,
                          ptX,ptY,ptX1,ptY1,
                          dFlag);
            } /* if lines on */
            if (graphStateArray[i].pointsOn) {
              if (dFlag==Xoption) {
                if (mono) {
                  GSetForeground(globalGC1,
                                 (float)monoColor((int)(aPoint->hue)),
                                 dFlag);
                } else {
                  GSetForeground(globalGC1,
                                 (float)XSolidColor((int)(aPoint->hue),
                                                    (int)(aPoint->shade)),
                                 dFlag);
                }
              }
              if (clipped1 && !eqNANQ(ptX1) && !eqNANQ(ptY1))
                GFillArc(globalGC1,vw,ptX1-halfSize,
                         ptY1-halfSize,aList->pointSize,aList->pointSize,
                         0,360*64, dFlag);
            } /* if points on */
            ptX = ptX1;  ptY = ptY1;  clipped = clipped1;
          } /* for all points */
        } /* if points or lines on */

        if (graphStateArray[i].splineOn) {   /* need spline color as well */
          if (dFlag==Xoption)  /* do only for X, ps uses default of black */
            GSetForeground(globalGC1,
                           (float)monoColor(148),
                           dFlag);
          boxX = vwInfo.width *
            ((-0.5 - graphStateArray[i].centerX)*
             graphStateArray[i].scaleX + 0.5);
          boxY = vwInfo.height * aspectR *
            (1 - ((0.5 - graphStateArray[i].centerY)*
                  graphStateArray[i].scaleY + 0.5*aspectR));

          boxW = graphStateArray[i].scaleX * vwInfo.width + 1;
          boxH = graphStateArray[i].scaleY * vwInfo.height * aspectR + 1;

          GDrawRectangle(globalGC1,vw,
                         boxX,boxY,boxW,boxH,
                         dFlag);
        }

        tempXpt = anXPoint;
      }     /* for a aList in listofListsOfPoints */
      if (graphStateArray[i].unitsOn) {
        /* do only for X, ps uses default of black */
        if (dFlag==Xoption)
          GSetForeground(unitGC,
                         (float)monoColor(graphStateArray[i].unitsColor),
                         dFlag);
 

        tickStart   = calcUnitX(0);
        oneTickUnit = calcUnitX(1) - tickStart;

        /* ticks along the positive X axis */

        unitWidth = 5*overall.width;            /* limit on acceptable separation : 5 chars */
        k = floor(unitWidth/oneTickUnit) +1;    /* get skipping integer */
        for (ii=0, jj = tickStart;
             jj < vwInfo.width;
             ii=ii+k,jj =jj+k* oneTickUnit) {
          if (jj >= 0) {

            /* ticks stuck to viewport*/
            GDrawLine(unitGC,vw,
                      (int)rint(jj),vwInfo.height-8,(int)rint(jj),vwInfo.height-4,
                      dFlag);

            sprintf(aunit,"%0.3g",ii*graphArray[0].spadUnitX);
            strlength=strlen(aunit);
            halflength=XTextWidth(unitFont,aunit,strlength)/2;

            if (dFlag == Xoption) GDrawImageString(unitGC,
                                             vw,
                                             (int)rint(jj) - halflength,
                                             vwInfo.height -8 -descent, 
                                             aunit,
                                             strlength, 
                                             dFlag);
            if (dFlag == PSoption) GDrawImageString(unitGC,
                                              vw,
                                              (int)rint(jj) -(strlength*3) ,
                                              vwInfo.height -14, 
                                              aunit,
                                              strlength, 
                                              dFlag); 
            /* these are "eyeball" parameters for the given PS font */
              
          }
            
        }
        /* ticks along the negative X axis */
        for (ii=-k,jj=tickStart - k*oneTickUnit;
             jj > 0;
             ii=ii-k,jj = jj-k*oneTickUnit) {
          if (jj <= vwInfo.width) {

            /* ticks stuck to viewport*/
            GDrawLine(unitGC,vw,
                      (int)rint(jj),vwInfo.height-8,(int)rint(jj),vwInfo.height-4,
                      dFlag);

            sprintf(aunit,"%0.3g",ii*graphArray[0].spadUnitX);
            strlength=strlen(aunit);
            halflength=XTextWidth(unitFont,aunit,strlength)/2;

            if (dFlag == Xoption) GDrawImageString(unitGC,
                                             vw,
                                             (int)rint(jj) - halflength,
                                             vwInfo.height -8 -descent, 
                                             aunit,
                                             strlength, 
                                             dFlag);
            if (dFlag == PSoption) GDrawImageString(unitGC,
                                              vw,
                                              (int)rint(jj) -(strlength*3) ,
                                              vwInfo.height -14, 
                                              aunit,
                                              strlength, 
                                              dFlag);
            /* these are "eyeball" parameters for the given PS font */
          }
        }
      
        tickStart = calcUnitY(0);
        oneTickUnit = calcUnitY(1) - tickStart;
     
        /* ticks along the positive Y axis */
        unitWidth = 2*(ascent+descent);                 /* limit of acceptable separation */
        k = floor(unitWidth/fabs(oneTickUnit)) +1;  /* get skipping integer */
        for (ii=0,jj = tickStart;
             jj > 0;
             ii=ii+k,jj =jj+k*oneTickUnit ) {
          if  (jj < vwInfo.height) {
 
            /* ticks stuck to viewport*/
            /* on the right */
            /*
              GDrawLine(unitGC,vw,
              vwInfo.width-6,(int)rint(jj),
              vwInfo.width-2,(int)rint(jj),dFlag);
              */
            /* on the left */
            GDrawLine(unitGC,vw,
                      2,(int)rint(jj),
                      6,(int)rint(jj),
                      dFlag);
            sprintf(aunit,"%0.3g",ii*graphArray[0].spadUnitY);
            strlength=strlen(aunit);
            XTextExtents(unitFont,aunit,strlength,&dummyInt,
                         &ascent,&descent,&overall);
            halflength=overall.width;           /* let's reuse that variable */

            if(dFlag == Xoption){
              /* on the right */
              /*
                GDrawImageString(unitGC, vw,
                vwInfo.width-halflength -6-descent,
                (int)rint(jj)+ascent/2 , 
                aunit, strlength, dFlag);
                */
              /* on the left */
              GDrawImageString(unitGC, vw,
                               8 + charlength/2,
                               (int)rint(jj)+ascent/2 , 
                               aunit, strlength, dFlag);
            }
            if(dFlag == PSoption){
              /* on the right */
              /*
                GDrawImageString(unitGC, vw,
                vwInfo.width - 6 - (strlength*6),
                (int)rint(jj)+4, 
                aunit, strlength, dFlag);
                */
              /* on the left */
              GDrawImageString(unitGC, vw,
                               8,(int)rint(jj)+4, 
                               aunit, strlength, dFlag);
              /* these are "eyeball" parameters for the given PS font */
            }
          }
        }

        /* ticks along the negative Y axis */
    
        for (ii=(-k),jj = tickStart - k*oneTickUnit;
             jj < vwInfo.height;
             ii=ii-k,jj =jj-k*oneTickUnit) {
          if (jj > 0) {

            /* ticks stuck to viewport*/
            /* on the right */
            /*
              GDrawLine(unitGC,vw,
              vwInfo.width-6,(int)rint(jj),
              vwInfo.width-2,(int)rint(jj),
              dFlag);
              */
            /* on the left */
            GDrawLine(unitGC,vw,
                      2,(int)rint(jj),
                      6,(int)rint(jj),
                      dFlag);

            sprintf(aunit,"%0.3g",ii*graphArray[0].spadUnitY);
            strlength=strlen(aunit);
            XTextExtents(unitFont,aunit,strlength,&dummyInt,
                         &ascent,&descent,&overall);
            halflength=overall.width;           /* let's reuse that variable */

            if(dFlag == Xoption){
              /* on the right */
              /*
                GDrawImageString(unitGC, vw,
                vwInfo.width-halflength -6-descent,
                (int)rint(jj)+ascent/2 , 
                aunit, strlength, dFlag);
                */
              /* on the left */
              GDrawImageString(unitGC, vw,
                               8 + charlength/2,
                               (int)rint(jj)+ascent/2 , 
                               aunit, strlength, dFlag);
            }
            if(dFlag == PSoption){
              /* on the right */
              /*
                GDrawImageString(unitGC, vw,
                vwInfo.width -6 -(strlength*6),
                (int)rint(jj)+4 , 
                aunit, strlength, dFlag);
                */
              /* on the left */
              GDrawImageString(unitGC, vw,
                               8,
                               (int)rint(jj)+4 , 
                               aunit, strlength, dFlag);
              /* these are "eyeball" parameters for the given PS font */
            }
          }
        }
      
      }  /* if unitsOn */
    }    /* if graph i exists and is showing */
  }   /* for i in graphs */


  if (dFlag==Xoption) {
    if (!followMouse) {
      /* no need to do this while autorepeating */
      makeMessageFromData(queriedGraph);
      writeControlMessage();  
    }
    XFlush(dsply);
  }

}           /* drawViewport() */



/************************************
 ***  viewPoints *makeViewport()  ***
 ************************************/

viewPoints *
makeViewport(const char* title,int vX,int vY,int vW,int vH,int showCP)
{
  Pixmap               spadbits,spadmask;
  XSetWindowAttributes viewAttrib;
  XSizeHints           titleSizeHints,viewSizeHints;
  Window               viewTitleWindow,viewGraphWindow;
  XColor               foreColor, backColor;

#ifdef DEBUG
  fprintf(stderr,"view2D: About to make a viewport\n");
#endif

  /* Create a viewport */
  if (!(viewport = (viewPoints *)malloc(sizeof(viewPoints)))) {
    fprintf(stderr,"Ran out of memory (malloc) trying to create a viewport.\n");
    openaxiom_sleep(5);
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }

#ifdef DEBUG
  fprintf(stderr,"view2D: Made a viewport\n");
#endif

  strcpy(viewport->title,title); 

  viewport->closing      = no;
  viewport->allowDraw    = yes;   /* just draw axes the first time around */
  viewport->axesOn   = axesON;
  viewport->unitsOn  = unitsON;
  viewport->pointsOn = pointsON;
  viewport->linesOn  = connectON;
  viewport->splineOn = splineON;

  /**** Make the windows for the viewport ****/
  spadbits = XCreateBitmapFromData(dsply,rtWindow,
                                   (const char*) spadBitmap_bits,
                                   spadBitmap_width,spadBitmap_height);
  spadmask = XCreateBitmapFromData(dsply,rtWindow,
                                   (const char*) spadMask_bits,
                                   spadMask_width,spadMask_height);
  viewAttrib.background_pixel = backgroundColor;
  viewAttrib.border_pixel = foregroundColor;
  viewAttrib.override_redirect = overrideManager; 
  viewAttrib.colormap = colorMap;

  foreColor.pixel = foregroundColor;
  backColor.pixel = backgroundColor;
  XQueryColor(dsply,colorMap,&foreColor);
  XQueryColor(dsply,colorMap,&backColor);
  viewAttrib.cursor = XCreatePixmapCursor(dsply,spadbits,spadmask,
                  &foreColor,&backColor,spadBitmap_x_hot,spadBitmap_y_hot);

  viewAttrib.event_mask = titleMASK;
  if (vW) {
    titleSizeHints.flags  = PPosition | PSize;
    titleSizeHints.x      = vX;
    titleSizeHints.y      = vY;
    titleSizeHints.width  = vW;
    titleSizeHints.height = vH;
  } else {
    titleSizeHints.flags  = PSize;
    titleSizeHints.width  = viewWidth;
    titleSizeHints.height = viewHeight;
  }

  viewTitleWindow = XCreateWindow(dsply,rtWindow,vX,vY,vW,vH,
                                  viewBorderWidth,
                                  CopyFromParent,InputOutput,CopyFromParent,
                                  viewportTitleCreateMASK,&viewAttrib);

  wm_delete_window = XInternAtom(dsply, "WM_DELETE_WINDOW", False);
  (void) XSetWMProtocols(dsply, viewTitleWindow, &wm_delete_window, 1);

  XSetNormalHints(dsply,viewTitleWindow,&titleSizeHints); 
  XSetStandardProperties(dsply,viewTitleWindow,"OpenAxiom 2D",viewport->title,
                           None,NULL,0,&titleSizeHints);

  viewport->titleWindow = viewTitleWindow;
  viewAttrib.event_mask = viewportMASK; 
  viewSizeHints.flags   = PPosition | PSize;
  viewSizeHints.x       = -viewBorderWidth;
  viewSizeHints.y       = titleHeight;
  viewSizeHints.width   = titleSizeHints.width;
  viewSizeHints.height  = titleSizeHints.height - 
                          (titleHeight + appendixHeight);
  viewGraphWindow = XCreateWindow(dsply,viewTitleWindow,
                                  viewSizeHints.x,viewSizeHints.y,
                                  viewSizeHints.width,viewSizeHints.height,
                                  viewBorderWidth,
                                  CopyFromParent,InputOutput,CopyFromParent,
                                  viewportCreateMASK,&viewAttrib);
  XSetNormalHints(dsply,viewGraphWindow,&viewSizeHints);
  XSetStandardProperties(dsply,viewGraphWindow,"2D Viewport","2D Viewport",
                         None,NULL,0,&viewSizeHints); 

  viewport->viewWindow = viewGraphWindow;

  /*Make the control panel for the viewport. */
  viewport->controlPanel = makeControlPanel();        
  if ((viewport->haveControl = showCP)) putControlPanelSomewhere(anywhere);

  XSync(dsply,False); 
  return(viewport);

}


/*********************************************
 *****  viewPoints *makeView2D(viewdata)  ****
 *********************************************/


viewPoints *
makeView2D(view2DStruct *viewdata)
{
  viewPoints *vPoints;  

  vPoints = makeViewport(viewdata->title, viewdata->vX,viewdata->vY,
                         viewdata->vW,viewdata->vH,viewdata->showCP);

  vPoints->allowDraw = yes;   /* draw everything from now on */

  if (viewdata->showCP) clearControlMessage();

  writeTitle(); 

  XMapWindow(dsply,vPoints->viewWindow);
  XMapWindow(dsply,vPoints->titleWindow);
  XSync(dsply,0);

  drawViewport(Xoption);      /* draw viewport with X routines (as opposed to PS) */
  return(vPoints);

}    /* makeView2D */

