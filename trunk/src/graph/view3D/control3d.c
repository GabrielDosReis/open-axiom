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

#define _CONTROL3D_C
#include "axiom-c-macros.h"

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "mouse11.bitmap"
#include "mouse11.mask"

#include "header.h"
#include "cpanel.h"

#include "util.H1"
#include "XShade.H1"
#include "XSpadFill.H1"
#include "Gfun.H1"
#include "all_3d.H1"

/* Defines the pixmap for the arrow displayed in the scale window */
#define zoomArrowN 11
static XPoint zoomArrow[zoomArrowN] = {
  {29,14},{38,23},{33,23},
  {40,45},{53,45},
  {29,69},
  {5,45},{18,45},
  {25,23},{20,23},{29,14} };

/* Defines the pixmap for the arrows displayed in the translate window */
#define translateArrowN 25
static XPoint translateArrow[translateArrowN] = {
  {55,2},{60,10},{58,10},{58,37},
  {85,37},{85,35},{93,40},{85,45},{85,43},{58,43},
  {58,70},{60,70},{55,78},{50,70},{52,70},{52,43},
  {25,43},{25,45},{17,40},{25,35},{25,37},{52,37},
  {52,10},{50,10},{55,2} };

static int rotateX, rotateY, rotateR;

/*
  void drawColorMap () 
  */

void
drawColorMap (void)
{
  
  controlPanelStruct *cp;
  int i,shadeWidth;
  
  /* Draw the color map window */
  
  cp = viewport->controlPanel;
  
  XClearArea(dsply,cp->controlWindow,5,colormapY,colormapW,colormapH,False);
  
  /* if window is grayscale, show the grayscale colormap */
  if (mono || (viewport->monoOn)) {
    shadeWidth = 230/maxGreyShade;
    for (i=0; i<maxGreyShade; i++) {
      XChangeShade(dsply, i);
      XShadeRectangle(dsply,cp->controlWindow,
                      colormapX + colorOffsetX + i*shadeWidth,
                      colormapY + colorOffsetY - 10, shadeWidth, 40);
    }
  } else {
    GDrawString(globalGC2,cp->controlWindow,colorWidth,
                colormapY + 13,"-",1,Xoption);
    GDrawString(globalGC2,cp->controlWindow,30*colorWidth + 40,
                colormapY + 13,"+",1,Xoption);
    GDrawString(globalGC2,cp->controlWindow,colorWidth,
                colormapY + 46,"-",1,Xoption);
    GDrawString(globalGC2,cp->controlWindow,30*colorWidth + 40,
                colormapY + 46,"+",1,Xoption);
    for (i=0; i<totalHues; i++) {
      GSetForeground(anotherGC, (float)XSolidColor(i,2), Xoption);
      GDrawLine(anotherGC,cp->controlWindow,
                colormapX + i*colorWidth + colorOffsetX,
                colormapY + colorOffsetY,
                colormapX + i*colorWidth + colorOffsetX,
                colormapY + colorOffsetY + colorHeight,Xoption);
    }

    if (viewport->hueTop > totalHues-1) viewport->hueTop = totalHues-1;
    if (viewport->hueOffset > totalHues-1) viewport->hueOffset = totalHues-1;

    GSetForeground(globGC, (float)monoColor(7), Xoption);
    /* Bottom (zmin) color indicator */
    GDrawLine(globGC,cp->controlWindow,
              colormapX + viewport->hueOffset * colorWidth + colorOffsetX,
              colormapY + colorOffsetY+colorHeight,
              colormapX + viewport->hueOffset * colorWidth + colorOffsetX,
              colormapY + colorOffsetY+colorHeight+colorPointer,Xoption);

    /* Top (zmax) color indicator */
    GDrawLine(globGC,cp->controlWindow,
              colormapX + viewport->hueTop * colorWidth+colorOffsetX,
              colormapY + colorOffsetY,
              colormapX + viewport->hueTop * colorWidth+colorOffsetX,
              colormapY + colorOffsetY-colorPointer,Xoption);

    /* Connect the bottom and top color indicator bars */
    GSetForeground(globGC, (float)monoColor(0), Xoption);
    GDrawLine(globGC,cp->controlWindow,
              colormapX + viewport->hueOffset * colorWidth + colorOffsetX,
              colormapY + colorOffsetY+colorHeight,
              colormapX + viewport->hueTop * colorWidth+colorOffsetX,
              colormapY + colorOffsetY,Xoption);
  }
  XSync(dsply,0);

}  /* drawColorMap() */


/*******************************
 * void writeControlTitle(w)   *
 *                             *
 * We need the window argument *
 * here because there are      *
 * multiple control panels in  *
 * 3D.                         *
 *******************************/

void
writeControlTitle (Window w)
{
  int strlength;

  s = viewport->title;
  strlength = strlen(s);
  XClearArea(dsply,w,0,0,controlWidth,potA,False);

  GSetForeground(anotherGC,(float)controlTitleColor,Xoption);
  GDrawString(anotherGC,w,centerX(anotherGC,s,strlength,controlWidth),
              15,s,strlength,Xoption);

}  /* writeControlTitle() */


/************************************/
/***  void clearControlMessage()  ***/
/************************************/

void
clearControlMessage (void)
{
  int strlength;

  strcpy(viewport->controlPanel->message,"                                ");
  strlength = strlen(viewport->controlPanel->message);
  GDrawImageString(globalGC1,viewport->controlPanel->controlWindow,
                   centerX(globalGC1,viewport->controlPanel->message,
                           strlength,controlWidth),
                   controlMessageY + globalFont->max_bounds.ascent + 8,
                   viewport->controlPanel->message,strlength,Xoption);

}

/************************************/
/***  void writeControlMessage()  ***/
/************************************/

void
writeControlMessage (void)
{

  int strlength;
  controlPanelStruct *cp;

  cp = viewport->controlPanel;
  strlength = strlen(cp->message);
  XClearArea(dsply,cp->controlWindow,
             0,controlMessageY+ globalFont->max_bounds.ascent + 8,
             0,controlMessageHeight,False);
  GSetForeground(globalGC1, (float)controlMessageColor, Xoption);
  GDrawImageString(globalGC1,cp->controlWindow,
                   centerX(globalGC1,cp->message,strlength,controlWidth),
                   controlMessageY + globalFont->max_bounds.ascent + 8,
                   cp->message,strlength,Xoption);

  XFlush(dsply);

}

/*********************************/
/***  void drawControlPanel()  ***/
/*********************************/

void
drawControlPanel(void )
{

  int                   offShade=14;
  controlPanelStruct    *cp;
  int                   i, strlength;
  char                  *s;

  cp = viewport->controlPanel;

  GSetForeground(trashGC, (float)foregroundColor, Xoption);

  /* Draw border lines to separate the potentiometer, message, colormap and
     button regions of the control panel. */
  GSetLineAttributes(trashGC, 2, LineSolid, CapButt, JoinMiter, Xoption);

  /* Draw a horizontal white line below the potentiometer area. */
  GDrawLine(trashGC, cp->controlWindow, 0, potB-1, controlWidth, potB-1, Xoption);

  /* Draw a horizontal white line above the rendering mode buttons. */
  GDrawLine(trashGC, cp->controlWindow, 0, butA, controlWidth, butA, Xoption);

  /* Draw a horizontal white line above the color mapping area. */
  GDrawLine(trashGC, cp->controlWindow, 0, cmapA, controlWidth, cmapA, Xoption);

  GSetLineAttributes(trashGC, 3, LineSolid, CapButt, JoinMiter, Xoption);
  /* Draw a horizontal white line above the potentiometer area. */
  GDrawLine(trashGC, cp->controlWindow, 0, potA, controlWidth, potA, Xoption);

  /* Set the line width as 1 here because it is used below as well. */
  GSetLineAttributes(trashGC, 1, LineSolid, CapButt, JoinMiter, Xoption);

  /* Draw inner white lines around quit, hide panel, and reset buttons. */
  GDrawLine(trashGC, cp->controlWindow, closeL, butA, closeL, butA+110, Xoption);

  /* Write potentiometer titles on the control panel. */

  writeControlTitle(cp->controlWindow);
  GSetForeground(globGC, (float)controlPotHeaderColor, Xoption);

  s     = "Rotate";
  GDrawString(globGC,cp->controlWindow,35,31+headerHeight,s,strlen(s),Xoption);
  s     = "Translate";
  GDrawString(globGC,cp->controlWindow,202,31+headerHeight,s,strlen(s),Xoption);
  s     = "Scale";
  GDrawString(globGC,cp->controlWindow,126,31+headerHeight,s,strlen(s),Xoption);

  GSetForeground(globGC, (float)controlColorColor, Xoption);

  /* Write labels on regular buttons, draw pixmaps on the potentiometers. */

  GSetForeground(globalGC1, (float)monoColor(buttonColor), Xoption);

  for (i=controlButtonsStart3D; i<(controlButtonsEnd3D); i++) {
    /* special cases depending on initial conditions */

    /* check if axes are set on or off */

    if (((cp->buttonQueue[i]).buttonKey == axesOnOff) &&
          (viewport->axesOn)) {
      (cp->buttonQueue[i]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1, (float)backgroundColor, Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[axesOnOff]).buttonX,
                   (control->buttonQueue[axesOnOff]).buttonY,
                   (control->buttonQueue[axesOnOff]).buttonWidth,
                   (control->buttonQueue[axesOnOff]).buttonHeight);
        GSetForeground(globalGC1, (float)foregroundColor, Xoption);
        GDrawRectangle(globalGC1,control->controlWindow,
                       (control->buttonQueue[axesOnOff]).buttonX,
                       (control->buttonQueue[axesOnOff]).buttonY,
                       (control->buttonQueue[axesOnOff]).buttonWidth,
                       (control->buttonQueue[axesOnOff]).buttonHeight,Xoption);
      }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == axesOnOff) &&
          (!viewport->axesOn)) {
        (cp->buttonQueue[i]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,cp->controlWindow,
                   (cp->buttonQueue[i]).buttonX,
                   (cp->buttonQueue[i]).buttonY,
                   (cp->buttonQueue[i]).buttonWidth,
                   (cp->buttonQueue[i]).buttonHeight);
          s = (control->buttonQueue[axesOnOff]).text;
          strlength = strlen(s);
          GSetForeground(processGC,
            (float)monoColor((control->buttonQueue[axesOnOff]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                           (control->buttonQueue[axesOnOff]).buttonX +
                           centerX(processGC,s,strlength,
                           (control->buttonQueue[axesOnOff]).buttonWidth),
                           (control->buttonQueue[axesOnOff]).buttonY +
                           centerY(processGC,
                           (control->buttonQueue[axesOnOff]).buttonHeight),
                           s,strlength,Xoption);
        } /* if mono */
      }
    } /* if axes */

    /* check if bounding region is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == region3D) &&
          (viewport->regionOn)) {
      (cp->buttonQueue[i]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1, (float)backgroundColor, Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[region3D]).buttonX,
                   (control->buttonQueue[region3D]).buttonY,
                   (control->buttonQueue[region3D]).buttonWidth,
                   (control->buttonQueue[region3D]).buttonHeight);
        GSetForeground(globalGC1, (float)foregroundColor, Xoption);
        GDrawRectangle(globalGC1,control->controlWindow,
                       (control->buttonQueue[region3D]).buttonX,
                       (control->buttonQueue[region3D]).buttonY,
                       (control->buttonQueue[region3D]).buttonWidth,
                       (control->buttonQueue[region3D]).buttonHeight,Xoption);
      }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == region3D) &&
          (!viewport->regionOn)) {
        (cp->buttonQueue[i]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,cp->controlWindow,
                   (cp->buttonQueue[i]).buttonX,
                   (cp->buttonQueue[i]).buttonY,
                   (cp->buttonQueue[i]).buttonWidth,
                   (cp->buttonQueue[i]).buttonHeight);
          s = (control->buttonQueue[region3D]).text;
          strlength = strlen(s);
          GSetForeground(processGC,
            (float)monoColor((control->buttonQueue[region3D]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                           (control->buttonQueue[region3D]).buttonX +
                           centerX(processGC,s,strlength,
                           (control->buttonQueue[region3D]).buttonWidth),
                           (control->buttonQueue[region3D]).buttonY +
                           centerY(processGC,
                           (control->buttonQueue[region3D]).buttonHeight),
                           s,strlength,Xoption);
        } /* if mono */
      }
    } /* if bounding region */

    /* check if black and white is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == bwColor) && (mono)) {
      (cp->buttonQueue[i]).text = " ";
      XChangeShade(dsply,offShade);
      XShadeRectangle(dsply,cp->controlWindow,
                   (cp->buttonQueue[i]).buttonX,
                   (cp->buttonQueue[i]).buttonY,
                   (cp->buttonQueue[i]).buttonWidth,
                   (cp->buttonQueue[i]).buttonHeight);
    } else {
      if (((cp->buttonQueue[i]).buttonKey == bwColor) && viewport->monoOn) {
        (cp->buttonQueue[i]).textColor = onColor;
        s = (control->buttonQueue[bwColor]).text;
        strlength = strlen(s);

        GSetForeground(processGC,
          (float)monoColor((control->buttonQueue[bwColor]).textColor),Xoption);
        GDrawImageString(processGC,control->controlWindow,
                         (control->buttonQueue[bwColor]).buttonX +
                         centerX(processGC,s,strlength,
                                 (control->buttonQueue[bwColor]).buttonWidth),
                         (control->buttonQueue[bwColor]).buttonY +
                         centerY(processGC,
                                 (control->buttonQueue[bwColor]).buttonHeight),
                         s,strlength,Xoption);
      } else {
        if (((cp->buttonQueue[i]).buttonKey == bwColor) && (!viewport->monoOn)) {
          (cp->buttonQueue[i]).textColor = offColor;
          s = (control->buttonQueue[bwColor]).text;
          strlength = strlen(s);

          GSetForeground(processGC,
            (float)monoColor((control->buttonQueue[bwColor]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                           (control->buttonQueue[bwColor]).buttonX +
                           centerX(processGC,s,strlength,
                           (control->buttonQueue[bwColor]).buttonWidth),
                           (control->buttonQueue[bwColor]).buttonY +
                           centerY(processGC,
                           (control->buttonQueue[bwColor]).buttonHeight),
                           s,strlength,Xoption);
        }
      }
    } /* if black and white */

    /* check if object rotation is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == objectr) &&
              (viewport->objectrOn)) {
        (control->buttonQueue[objectr]).textColor = onColor;
        if (mono) {
          GSetForeground(globalGC1, (float)backgroundColor, Xoption);
          XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[objectr]).buttonX,
                 (control->buttonQueue[objectr]).buttonY,
                 (control->buttonQueue[objectr]).buttonWidth,
                 (control->buttonQueue[objectr]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[objectr]).buttonX,
                         (control->buttonQueue[objectr]).buttonY,
                         (control->buttonQueue[objectr]).buttonWidth,
                         (control->buttonQueue[objectr]).buttonHeight,Xoption);
        }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == objectr) &&
              (!viewport->objectrOn)) {
        (control->buttonQueue[objectr]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[objectr]).buttonX,
                 (control->buttonQueue[objectr]).buttonY,
                 (control->buttonQueue[objectr]).buttonWidth,
                 (control->buttonQueue[objectr]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[objectr]).buttonX,
                         (control->buttonQueue[objectr]).buttonY,
                         (control->buttonQueue[objectr]).buttonWidth,
                         (control->buttonQueue[objectr]).buttonHeight,Xoption);
          GSetForeground(processGC,
            (float)monoColor((control->buttonQueue[objectr]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                   (control->buttonQueue[objectr]).buttonX +
                   centerX(processGC,(control->buttonQueue[objectr]).text,
                           strlen((control->buttonQueue[objectr]).text),
                           (control->buttonQueue[objectr]).buttonWidth),
                           (control->buttonQueue[objectr]).buttonY +
                           centerY(processGC,
                           (control->buttonQueue[objectr]).buttonHeight),
                           (control->buttonQueue[objectr]).text,
                           strlen((control->buttonQueue[objectr]).text),Xoption);
        }
      } /* else not object rotation */
    } /* if object rotation */

    /* check if origin rotation is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == originr) &&
              (viewport->originrOn)) {
        (control->buttonQueue[originr]).textColor = onColor;
        if (mono) {
          GSetForeground(globalGC1, (float)backgroundColor, Xoption);
          XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[originr]).buttonX,
                 (control->buttonQueue[originr]).buttonY,
                 (control->buttonQueue[originr]).buttonWidth,
                 (control->buttonQueue[originr]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[originr]).buttonX,
                         (control->buttonQueue[originr]).buttonY,
                         (control->buttonQueue[originr]).buttonWidth,
                         (control->buttonQueue[originr]).buttonHeight,Xoption);
        }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == originr) &&
              (!viewport->originrOn)) {
        (control->buttonQueue[originr]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[originr]).buttonX,
                 (control->buttonQueue[originr]).buttonY,
                 (control->buttonQueue[originr]).buttonWidth,
                 (control->buttonQueue[originr]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[originr]).buttonX,
                         (control->buttonQueue[originr]).buttonY,
                         (control->buttonQueue[originr]).buttonWidth,
                         (control->buttonQueue[originr]).buttonHeight,Xoption);

          GSetForeground(processGC,
                (float)monoColor((control->buttonQueue[originr]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                   (control->buttonQueue[originr]).buttonX +
                   centerX(processGC,(control->buttonQueue[originr]).text,
                           strlen((control->buttonQueue[originr]).text),
                           (control->buttonQueue[originr]).buttonWidth),
                           (control->buttonQueue[originr]).buttonY +
                           centerY(processGC,
                           (control->buttonQueue[originr]).buttonHeight),
                           (control->buttonQueue[originr]).text,
                           strlen((control->buttonQueue[originr]).text),Xoption);
        }
      } /* else not origin rotation */
    } /* if origin rotation */

    /* check if zoom X is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == zoomx) &&
              (viewport->zoomXOn)) {
        (control->buttonQueue[zoomx]).textColor = onColor;
        if (mono) {
          GSetForeground(globalGC1, (float)backgroundColor, Xoption);
          XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[zoomx]).buttonX,
                 (control->buttonQueue[zoomx]).buttonY,
                 (control->buttonQueue[zoomx]).buttonWidth,
                 (control->buttonQueue[zoomx]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[zoomx]).buttonX,
                         (control->buttonQueue[zoomx]).buttonY,
                         (control->buttonQueue[zoomx]).buttonWidth,
                         (control->buttonQueue[zoomx]).buttonHeight,Xoption);
        }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == zoomx) &&
              (!viewport->zoomXOn)) {
        (control->buttonQueue[zoomx]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[zoomx]).buttonX,
                 (control->buttonQueue[zoomx]).buttonY,
                 (control->buttonQueue[zoomx]).buttonWidth,
                 (control->buttonQueue[zoomx]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[zoomx]).buttonX,
                         (control->buttonQueue[zoomx]).buttonY,
                         (control->buttonQueue[zoomx]).buttonWidth,
                         (control->buttonQueue[zoomx]).buttonHeight,Xoption);

          GSetForeground(processGC,
                (float)monoColor((control->buttonQueue[zoomx]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                   (control->buttonQueue[zoomx]).buttonX +
                   centerX(processGC,(control->buttonQueue[zoomx]).text,
                           strlen((control->buttonQueue[zoomx]).text),
                           (control->buttonQueue[zoomx]).buttonWidth),
                           (control->buttonQueue[zoomx]).buttonY +
                           centerY(processGC,
                                   (control->buttonQueue[zoomx]).buttonHeight),
                           (control->buttonQueue[zoomx]).text,
                           strlen((control->buttonQueue[zoomx]).text),Xoption);
        }
      } /* else not zoom X */
    } /* if zoom X */

    /* check if zoom Y is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == zoomy) &&
              (viewport->zoomYOn)) {
        (control->buttonQueue[zoomy]).textColor = onColor;
        if (mono) {
          GSetForeground(globalGC1, (float)backgroundColor, Xoption);
          XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[zoomy]).buttonX,
                 (control->buttonQueue[zoomy]).buttonY,
                 (control->buttonQueue[zoomy]).buttonWidth,
                 (control->buttonQueue[zoomy]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1, control->controlWindow,
                         (control->buttonQueue[zoomy]).buttonX,
                         (control->buttonQueue[zoomy]).buttonY,
                         (control->buttonQueue[zoomy]).buttonWidth,
                         (control->buttonQueue[zoomy]).buttonHeight,Xoption);
        }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == zoomy) &&
              (!viewport->zoomYOn)) {
        (control->buttonQueue[zoomy]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[zoomy]).buttonX,
                 (control->buttonQueue[zoomy]).buttonY,
                 (control->buttonQueue[zoomy]).buttonWidth,
                 (control->buttonQueue[zoomy]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[zoomy]).buttonX,
                         (control->buttonQueue[zoomy]).buttonY,
                         (control->buttonQueue[zoomy]).buttonWidth,
                         (control->buttonQueue[zoomy]).buttonHeight,Xoption);

          GSetForeground(processGC,
                (float)monoColor((control->buttonQueue[zoomy]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                   (control->buttonQueue[zoomy]).buttonX +
                   centerX(processGC,(control->buttonQueue[zoomy]).text,
                           strlen((control->buttonQueue[zoomy]).text),
                           (control->buttonQueue[zoomy]).buttonWidth),
                           (control->buttonQueue[zoomy]).buttonY +
                           centerY(processGC,
                                   (control->buttonQueue[zoomy]).buttonHeight),
                           (control->buttonQueue[zoomy]).text,
                           strlen((control->buttonQueue[zoomy]).text),Xoption);
        }
      } /* else not zoom Y */
    } /* if zoom Y */

    /* check if zoom Z is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == zoomz) &&
              (viewport->zoomZOn)) {
        (control->buttonQueue[zoomz]).textColor = onColor;
        if (mono) {
          GSetForeground(globalGC1, (float)backgroundColor, Xoption);
          XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[zoomz]).buttonX,
                 (control->buttonQueue[zoomz]).buttonY,
                 (control->buttonQueue[zoomz]).buttonWidth,
                 (control->buttonQueue[zoomz]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[zoomz]).buttonX,
                         (control->buttonQueue[zoomz]).buttonY,
                         (control->buttonQueue[zoomz]).buttonWidth,
                         (control->buttonQueue[zoomz]).buttonHeight,Xoption);
        }
    } else {
      if (((cp->buttonQueue[i]).buttonKey == zoomz) &&
              (!viewport->zoomZOn)) {
        (control->buttonQueue[zoomz]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[zoomz]).buttonX,
                 (control->buttonQueue[zoomz]).buttonY,
                 (control->buttonQueue[zoomz]).buttonWidth,
                 (control->buttonQueue[zoomz]).buttonHeight);
          GSetForeground(globalGC1, (float)foregroundColor, Xoption);
          GDrawRectangle(globalGC1,control->controlWindow,
                         (control->buttonQueue[zoomz]).buttonX,
                         (control->buttonQueue[zoomz]).buttonY,
                         (control->buttonQueue[zoomz]).buttonWidth,
                         (control->buttonQueue[zoomz]).buttonHeight,Xoption);

          GSetForeground(processGC,
                (float)monoColor((control->buttonQueue[zoomz]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                   (control->buttonQueue[zoomz]).buttonX +
                   centerX(processGC,(control->buttonQueue[zoomz]).text,
                           strlen((control->buttonQueue[zoomz]).text),
                           (control->buttonQueue[zoomz]).buttonWidth),
                           (control->buttonQueue[zoomz]).buttonY +
                           centerY(processGC,
                                   (control->buttonQueue[zoomz]).buttonHeight),
                           (control->buttonQueue[zoomz]).text,
                           strlen((control->buttonQueue[zoomz]).text),Xoption);
        }
      } /* else not zoom Y */
    } /* if zoom Y */

    /* check if outline is set on or off */

    if (((cp->buttonQueue[i]).buttonKey == outlineOnOff) &&
          (viewData.outlineRenderOn)) {
      (cp->buttonQueue[i]).textColor = onColor;
    } else {
      if (((cp->buttonQueue[i]).buttonKey == outlineOnOff) &&
          !(viewData.outlineRenderOn)) {
        (cp->buttonQueue[i]).textColor = offColor;
        if (mono) {
          XChangeShade(dsply,offShade);
          XShadeRectangle(dsply,cp->controlWindow,
                   (cp->buttonQueue[i]).buttonX,
                   (cp->buttonQueue[i]).buttonY,
                   (cp->buttonQueue[i]).buttonWidth,
                   (cp->buttonQueue[i]).buttonHeight);
          s = (control->buttonQueue[outlineOnOff]).text;
          strlength = strlen(s);

          GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[outlineOnOff]).textColor),Xoption);
          GDrawImageString(processGC,control->controlWindow,
                           (control->buttonQueue[outlineOnOff]).buttonX +
                           centerX(processGC,s,strlength,
                           (control->buttonQueue[outlineOnOff]).buttonWidth),
                           (control->buttonQueue[outlineOnOff]).buttonY +
                           centerY(processGC,
                           (control->buttonQueue[outlineOnOff]).buttonHeight),
                           s,strlength,Xoption);
        } /* if mono */
      } /* outline off */
    } /* outline on */

    /* Draw the button window border */

    GDraw3DButtonOut(globalGC1,cp->controlWindow,
                   (cp->buttonQueue[i]).buttonX, (cp->buttonQueue[i]).buttonY,
                   (cp->buttonQueue[i]).buttonWidth,
                   (cp->buttonQueue[i]).buttonHeight,Xoption);

    GSetForeground(trashGC,
                   (float)monoColor((cp->buttonQueue[i]).textColor), Xoption);
    switch (i) {
    case rotate:
      GDrawArc(trashGC, cp->controlWindow,
               rotateX, rotateY, rotateR, rotateR, 0, 360*64, Xoption);
      break;

    case zoom:
      GDrawLines(trashGC, cp->controlWindow, zoomArrow, zoomArrowN,
                 CoordModeOrigin, Xoption);
      break;

    case translate:
      GDrawLines(trashGC, cp->controlWindow, translateArrow,
                 translateArrowN, CoordModeOrigin, Xoption);
      break;

    default:
      s = (cp->buttonQueue[i]).text;
      strlength = strlen(s);
      GDrawString(trashGC, cp->controlWindow,
                  (cp->buttonQueue[i]).buttonX +
                  centerX(processGC,s,strlength,
                          (cp->buttonQueue[i]).buttonWidth),
                  (cp->buttonQueue[i]).buttonY +
                  centerY(processGC,
                          (cp->buttonQueue[i]).buttonHeight),s,strlen(s),Xoption);
      break;
    };

    if ((cp->buttonQueue[i]).pot) {
      /* draw horizontal and vertical centerlines */

      GDrawLine(globalGC1,cp->controlWindow,
                (cp->buttonQueue[i]).buttonX + (cp->buttonQueue[i]).xHalf,
                (cp->buttonQueue[i]).buttonY,
                (cp->buttonQueue[i]).buttonX + (cp->buttonQueue[i]).xHalf,
                (cp->buttonQueue[i]).buttonY + 2*(cp->buttonQueue[i]).yHalf,Xoption);

      GDrawLine(globalGC1,cp->controlWindow,
                (cp->buttonQueue[i]).buttonX,
                (cp->buttonQueue[i]).buttonY + (cp->buttonQueue[i]).yHalf,
                (cp->buttonQueue[i]).buttonX + 2*(cp->buttonQueue[i]).xHalf,
                (cp->buttonQueue[i]).buttonY + (cp->buttonQueue[i]).yHalf,Xoption);
    }
  }

  /* refresh the latest message */
  clearControlMessage();
  strcpy(control->message,viewport->title);
  writeControlMessage();

 /* Draw the color map window */
  cp = viewport->controlPanel;
  drawColorMap();
  XFlush(dsply);

}  /* drawControlPanel() */


/*****************************
 *  void getControlXY()      *
 *                           *
 * Determines the x and y    *
 * coordinate where the      *
 * control panel is to be    *
 * placed, based upon where  *
 * the mouse button was      *
 * pressed within the graph  *
 * viewport window.          *
 *****************************/

controlXY
getControlXY (int whereDoYouWantPanel)
{

  XWindowAttributes wAttrib;
  controlXY         cXY;
  int               viewX, viewY, viewW, viewH, tmp=1;
  Window            rootW, parentW, *childrenWs, tmpW;
  unsigned int      nChildren;

  tmpW = viewport->titleWindow;
  while(tmp) {
    XQueryTree(dsply,tmpW,&rootW,&parentW,&childrenWs,&nChildren);
    XFree(childrenWs);
    if (parentW == rtWindow) {
      tmp = 0;
    } else {
      tmpW = parentW;
    }
  }
  XGetWindowAttributes(dsply,tmpW,&wAttrib);

  viewX = wAttrib.x;
  viewY = wAttrib.y;
  viewW = wAttrib.width;
  viewH = wAttrib.height;

  if (whereDoYouWantPanel) {
    switch (whereDoYouWantPanel) {
    case 1: /* right */
      cXY.putX = viewX + viewW;
      cXY.putY = viewY;
      break;
    case 2: /* bottom */
      cXY.putX = viewX + (viewW - controlWidth)/2;   /* center it */
      cXY.putY = viewY + viewH;
      break;
    case 3: /* left */
      cXY.putX = viewX - controlWidth  - borderWidth;
      cXY.putY = viewY;
      break;
    case 4: /* top */
      cXY.putX = viewX + (viewW - controlWidth)/2;   /* center it */
      cXY.putY = viewY - controlHeight - borderHeight;
    }
  } else {
    if ((physicalWidth - (viewX + viewW)) >= controlWidth) {
      cXY.putX = viewX + viewW;
      cXY.putY = viewY;
    } else if ((physicalHeight - (viewY + viewH)) >= controlHeight) {
      cXY.putX = viewX + (viewW - controlWidth)/2;   /* center it */
      cXY.putY = viewY + viewH;
    } else if (viewX >= controlWidth) {
      cXY.putX = viewX - controlWidth - borderWidth;
      cXY.putY = viewY;
    } else if (viewY >= controlHeight) {
      cXY.putX = viewX + (viewW - controlWidth)/2;   /* center it */
      cXY.putY = viewY - controlHeight - borderHeight;
    } else {                       /* put inside of viewport */
      cXY.putX = viewX + viewW - controlWidth;
      cXY.putY = viewY + viewH - controlHeight;
    }
  }
  if (cXY.putX < 0) cXY.putX = 0;
  if (cXY.putY < 0) cXY.putY = 0;
  return(cXY);

}



/************************************************/
/***  controlPanelStruct *makeControlPanel()  ***/
/************************************************/

controlPanelStruct *
makeControlPanel (void)
{

  Window                cw;
  int                   i, num;
  controlPanelStruct    *control;
  buttonStruct          *buttons;
  controlXY             cXY;
  XSetWindowAttributes  cwAttrib, controlAttrib;
  XSizeHints            sizehint;
  Pixmap                mousebits, mousemask;
  XColor                foreColor, backColor;

  if (!(control = (controlPanelStruct *)saymem("control.c",1,
                        sizeof(controlPanelStruct)))) {
    fprintf(stderr,"Ran out of memory trying to create control panel.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }

  cXY = getControlXY(0);

  mousebits = XCreateBitmapFromData(dsply,rtWindow, mouseBitmap_bits,
                                    mouseBitmap_width, mouseBitmap_height);
  mousemask = XCreateBitmapFromData(dsply,rtWindow, mouseMask_bits,
                                    mouseMask_width, mouseMask_height);
  cwAttrib.background_pixel     = backgroundColor;
  cwAttrib.border_pixel         = foregroundColor;
  cwAttrib.event_mask           = controlMASK;
  cwAttrib.colormap             = colorMap;
  cwAttrib.override_redirect    = overrideManager;
  foreColor.pixel               = controlCursorForeground;
  XQueryColor(dsply,colorMap,&foreColor);
  backColor.pixel               = controlCursorBackground;
  XQueryColor(dsply,colorMap,&backColor);
  cwAttrib.cursor               = XCreatePixmapCursor(dsply,mousebits,
                                        mousemask, &foreColor,&backColor,
                                        mouseBitmap_x_hot,mouseBitmap_y_hot);
  cw = XCreateWindow(dsply,rtWindow,
                     cXY.putX,cXY.putY,controlWidth,controlHeight,3,
                     CopyFromParent,InputOutput,CopyFromParent,
                     controlCreateMASK,&cwAttrib);

  sizehint.flags  = PPosition | PSize;
  sizehint.x      = cXY.putX;
  sizehint.y      = cXY.putY;
  sizehint.width  = controlWidth;
  sizehint.height = controlHeight;
  /*** the None stands for icon pixmap  ***/
  XSetNormalHints(dsply,cw,&sizehint);
  XSetStandardProperties(dsply,cw,"3D Control Panel","3D Control Panel",
                         None,NULL,0,&sizehint);

  /* Define and assign a mouse cursor */
  control->controlWindow = cw;

  num = initButtons(control->buttonQueue);
  buttons = control->buttonQueue;
  for (i=controlButtonsStart3D; i<(controlButtonsEnd3D); i++) {
    controlAttrib.event_mask = (control->buttonQueue[i]).mask;
    (control->buttonQueue[i]).self = XCreateWindow(dsply,cw,
                                       (control->buttonQueue[i]).buttonX,
                                       (control->buttonQueue[i]).buttonY,
                                       (control->buttonQueue[i]).buttonWidth,
                                       (control->buttonQueue[i]).buttonHeight,
                                       0,0,InputOnly,CopyFromParent,
                                       buttonCreateMASK,&controlAttrib);
    XMakeAssoc(dsply,table,(control->buttonQueue[i]).self,
               &((control->buttonQueue[i]).buttonKey));
    /* use buttonKey and not i because buttonKey has a permanent address */

    XMapWindow(dsply,(control->buttonQueue[i]).self);

  } /* for each button */


  /* Set up the potentiometer pixmaps. */
  for (i=0; i<zoomArrowN; i++) {
    zoomArrow[i].x += buttons[zoom].buttonX;
    zoomArrow[i].y += buttons[zoom].buttonY;
  }
  for (i=0; i<translateArrowN; i++) {
    translateArrow[i].x += buttons[translate].buttonX;
    translateArrow[i].y += buttons[translate].buttonY;
  }

  rotateX = control->buttonQueue[rotate].buttonX+17;
  rotateY = control->buttonQueue[rotate].buttonY+2;
  rotateR = control->buttonQueue[rotate].buttonHeight-4;

  strcpy(control->message,"                                ");

  /* Create the color mapping window */
  controlAttrib.event_mask   = colorMASK;
  control->colormapWindow = XCreateWindow(dsply,cw, colorWidth,colormapY,
                              colormapW,colormapH,0, 0,InputOnly,
                              CopyFromParent, colormapCreateMASK,
                              &controlAttrib);
  XMapWindow(dsply,control->colormapWindow);
  viewport->justMadeControl = yes;

  return(control);

} /* makeControlPanel() */




/******************************************
 *  void putControlPanelSomewhere()       *
 * This routine puts up the control panel *
 * associated with the viewport passed    *
 * in.  It first tries to put it to the   *
 * right of the viewport.  If there isn't *
 * enough room, it tries the bottom and   *
 * so on going clockwise. If the viewport *
 * is too big and there is no room to put *
 * the control panel outside of it, the   *
 * control panel is placed on the bottom  *
 * right hand corner of the viewport.     *
 *****************************************/

void
putControlPanelSomewhere (int whereDoesPanelGo)
{
  controlPanelStruct *control;
  controlXY          whereControl;

  control      = viewport->controlPanel;
  whereControl = getControlXY(whereDoesPanelGo);

  viewport->haveControl = yes;

  XRaiseWindow(dsply,control->controlWindow);
  XMoveWindow(dsply, control->controlWindow,
              whereControl.putX, whereControl.putY);

  drawControlPanel();
  XSync(dsply,0);
  if (viewport->justMadeControl) {
    XMapWindow(dsply,control->controlWindow);
    viewport->justMadeControl = no;
  }
  XMapWindow(dsply,control->controlWindow);
  XFlush(dsply);

}



