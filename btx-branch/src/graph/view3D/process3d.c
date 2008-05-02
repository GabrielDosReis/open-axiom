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

#define _PROCESS3D_C
#include "openaxiom-c-macros.h"

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>


#include "header.h"
#include "cpanel.h"
#include "volume.h"
#include "mode.h"
#include "process.h"
#include "draw.h"
#include "sockio.h"
#include "com.h"


#include "util.H1"
#include "Gfun.H1"
#include "pixmap.H1"
#include "XShade.H1"
#include "XSpadFill.H1"
#include "all_3d.H1"

#define inside(A,B) (((XButtonEvent *)event)->x >= A && \
                     ((XButtonEvent *)event)->x <= B)


void
buttonAction (int bKey)
{

  char *s1, *s2;
  int strL, strL1, strL2, offShade=14;

  /* Button colors which are offColor, RED, are turned off, and those which
     are onColor, GREEN, indicate the mode is in effect. */

  switch (bKey) {

  case hideControl:
    if (viewport->haveControl) {
      viewport->haveControl = no;
      XUnmapWindow(dsply,control->controlWindow);
    }
    break;

  case region3D:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    if (viewport->regionOn) {
      viewport->regionOn = no;
      (control->buttonQueue[region3D]).textColor = offColor;
      viewData.box = 0;
      if (mono) {
        XChangeShade(dsply,offShade);
        XShadeRectangle(dsply,control->controlWindow,
                   (control->buttonQueue[region3D]).buttonX,
                   (control->buttonQueue[region3D]).buttonY,
                   (control->buttonQueue[region3D]).buttonWidth,
                   (control->buttonQueue[region3D]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[region3D]).buttonX,
                       (control->buttonQueue[region3D]).buttonY,
                       (control->buttonQueue[region3D]).buttonWidth,
                       (control->buttonQueue[region3D]).buttonHeight,Xoption);
      }
    } else {  /* inverted color for region off */
      viewport->regionOn = yes;
      viewData.box = 1;
      (control->buttonQueue[region3D]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[region3D]).buttonX,
                   (control->buttonQueue[region3D]).buttonY,
                   (control->buttonQueue[region3D]).buttonWidth,
                   (control->buttonQueue[region3D]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[region3D]).buttonX,
                       (control->buttonQueue[region3D]).buttonY,
                       (control->buttonQueue[region3D]).buttonWidth,
                       (control->buttonQueue[region3D]).buttonHeight,Xoption);
      }
    }

    s = (control->buttonQueue[region3D]).text;
    strL = strlen(s);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[region3D]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[region3D]).buttonX +
                     centerX(processGC,s,strL,
                             (control->buttonQueue[region3D]).buttonWidth),
                     (control->buttonQueue[region3D]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[region3D]).buttonHeight),
                     s,strL,Xoption);
    redoSmooth = yes;
    drawViewport(Xoption);
    break;



  case bwColor:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    if (!mono) {
      if (viewport->monoOn) {
        viewport->monoOn = no;
        if (viewport->hueTop == viewport->hueOffset) redoColor = yes;
        else redoDither = yes;
        (control->buttonQueue[bwColor]).textColor = offColor;
        (control->buttonQueue[bwColor]).text = "BW";
      } else {
        viewport->monoOn = yes;
        maxGreyShade = XInitShades(dsply,scrn);
        if (viewport->hueTop == viewport->hueOffset) redoColor = yes;
        else redoDither = yes;
        (control->buttonQueue[bwColor]).textColor = onColor;
        (control->buttonQueue[bwColor]).text = "BW";
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[bwColor]).buttonX,
                   (control->buttonQueue[bwColor]).buttonY,
                   (control->buttonQueue[bwColor]).buttonWidth,
                   (control->buttonQueue[bwColor]).buttonHeight);
        GSetForeground(globalGC1,(float)monoColor(buttonColor),Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[bwColor]).buttonX,
                       (control->buttonQueue[bwColor]).buttonY,
                       (control->buttonQueue[bwColor]).buttonWidth,
                       (control->buttonQueue[bwColor]).buttonHeight,Xoption);
      }

      s = (control->buttonQueue[bwColor]).text;
      strL = strlen(s);

      GSetForeground(processGC,
             (float)monoColor((control->buttonQueue[bwColor]).textColor),Xoption);
      GDrawImageString(processGC,control->controlWindow,
                       (control->buttonQueue[bwColor]).buttonX +
                       centerX(processGC,s,strL,
                               (control->buttonQueue[bwColor]).buttonWidth),
                       (control->buttonQueue[bwColor]).buttonY +
                       centerY(processGC,
                               (control->buttonQueue[bwColor]).buttonHeight),
                       s,strL,Xoption);
      drawColorMap();
      redoSmooth = yes;
      writeTitle();
      drawViewport(Xoption);
    }
    break;



  case outlineOnOff:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    if (viewData.outlineRenderOn) {
      viewData.outlineRenderOn = 0;
      (control->buttonQueue[outlineOnOff]).textColor = offColor;
      if (mono) {
        XChangeShade(dsply,offShade);
        XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[outlineOnOff]).buttonX,
                 (control->buttonQueue[outlineOnOff]).buttonY,
                 (control->buttonQueue[outlineOnOff]).buttonWidth,
                 (control->buttonQueue[outlineOnOff]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[outlineOnOff]).buttonX,
                       (control->buttonQueue[outlineOnOff]).buttonY,
                       (control->buttonQueue[outlineOnOff]).buttonWidth,
                       (control->buttonQueue[outlineOnOff]).buttonHeight,Xoption);
      }
    } else {
      viewData.outlineRenderOn = 1;
      (control->buttonQueue[outlineOnOff]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[outlineOnOff]).buttonX,
                 (control->buttonQueue[outlineOnOff]).buttonY,
                 (control->buttonQueue[outlineOnOff]).buttonWidth,
                 (control->buttonQueue[outlineOnOff]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[outlineOnOff]).buttonX,
                       (control->buttonQueue[outlineOnOff]).buttonY,
                       (control->buttonQueue[outlineOnOff]).buttonWidth,
                       (control->buttonQueue[outlineOnOff]).buttonHeight,Xoption);
      }
    }
    s = (control->buttonQueue[outlineOnOff]).text;
    strL = strlen(s);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[outlineOnOff]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[outlineOnOff]).buttonX +
                     centerX(processGC,s,strL,
                             (control->buttonQueue[outlineOnOff]).buttonWidth),
                     (control->buttonQueue[outlineOnOff]).buttonY +
                     centerY(processGC,
                     (control->buttonQueue[outlineOnOff]).buttonHeight),
                     s,strL,Xoption);
    if (viewData.style == render) {
      drawViewport(Xoption);
    }
    break;


  case lighting:
    if (saveFlag) {
      doingPanel = CONTROLpanel;
      XUnmapWindow(dsply,saveWindow);
    }
    doingPanel = LIGHTpanel;
    tempLightPointer[0] = viewport->lightVector[0];
    tempLightPointer[1] = viewport->lightVector[1];
    tempLightPointer[2] = viewport->lightVector[2];
    tempLightIntensity = lightIntensity;
    XMapWindow(dsply,lightingWindow);
    break;


  case viewVolume:
    if (saveFlag) {
      doingPanel = CONTROLpanel;
      XUnmapWindow(dsply,saveWindow);
    }
    doingPanel = VOLUMEpanel;
    XMapWindow(dsply,volumeWindow);
    redrawView = yes;
    drawViewport(Xoption);      /* draw it with doingVolume set to yes */
    break;


  case volumeReturn:
    doingPanel = CONTROLpanel;
    redoSmooth = yes;
    redrawView = yes;
    XUnmapWindow(dsply,volumeWindow);
    break;


  case volumeAbort:
    doingPanel = CONTROLpanel;
    redrawView = yes;
    XUnmapWindow(dsply,volumeWindow);
    break;


  case lightReturn:
    doingPanel = CONTROLpanel;
    viewport->lightVector[0] = lightPointer[0] = tempLightPointer[0];
    viewport->lightVector[1] = lightPointer[1] = tempLightPointer[1];
    viewport->lightVector[2] = lightPointer[2] = tempLightPointer[2];
    lightIntensity = tempLightIntensity;
    normalizeVector(viewport->lightVector);
    redrawView = ((viewData.style == render) || (viewData.style == smooth));
    if (movingLight || changedIntensity) redoSmooth = yes;
    XUnmapWindow(dsply,lightingWindow);
    break;


  case lightAbort:
    movingLight = no;  changedIntensity = no;
    doingPanel = CONTROLpanel;
    XUnmapWindow(dsply,lightingWindow);
    break;


  case resetView:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    viewport->axesOn       = yes;
    viewport->regionOn     = no;  viewData.box = 0;
    viewData.outlineRenderOn = 0;
    viewport->monoOn       = no;
    viewport->zoomXOn      = viewport->zoomYOn = viewport->zoomZOn = yes;
    viewport->originrOn    = yes;  viewport->objectrOn    = no;
    viewport->originFlag   = no;
    viewport->xyOn         = viewport->xzOn = viewport->yzOn = no;
    viewport->lightVector[0] = -0.5;
    viewport->lightVector[1] = 0.5;
    viewport->lightVector[2] = 0.5;
    viewport->translucency   = viewData.translucency;
    viewport->deltaX       = viewport->deltaX0;
    viewport->deltaY       = viewport->deltaY0;
    viewport->deltaY       = viewport->deltaZ0;
    viewport->scale        = viewport->scale0;
    viewport->scaleX       = viewport->scaleY = viewport->scaleZ = 1.0;
    if (!equal(viewport->theta,viewport->theta0) || !equal(viewport->phi,viewport->phi0))
      rotated = yes;
    viewport->theta = viewport->axestheta = viewport->theta0 = viewData.theta;
    viewport->phi   = viewport->axesphi   = viewport->phi0   = viewData.phi;
    viewport->thetaObj = 0.0;
    viewport->phiObj   = 0.0;
    redoSmooth = yes;
    drawViewport(Xoption);
    if (viewport->haveControl) drawControlPanel();
    writeTitle();
    break;


  case axesOnOff:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    if (viewport->axesOn) {
      viewport->axesOn = no;
      (control->buttonQueue[axesOnOff]).textColor = offColor;
      if (mono) {
        XChangeShade(dsply,offShade);
        XShadeRectangle(dsply,control->controlWindow,
                   (control->buttonQueue[axesOnOff]).buttonX,
                   (control->buttonQueue[axesOnOff]).buttonY,
                   (control->buttonQueue[axesOnOff]).buttonWidth,
                   (control->buttonQueue[axesOnOff]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[axesOnOff]).buttonX,
                       (control->buttonQueue[axesOnOff]).buttonY,
                       (control->buttonQueue[axesOnOff]).buttonWidth,
                       (control->buttonQueue[axesOnOff]).buttonHeight,Xoption);
      }
    } else {   /* draw invert-color button */
      viewport->axesOn = yes;
      (control->buttonQueue[axesOnOff]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[axesOnOff]).buttonX,
                   (control->buttonQueue[axesOnOff]).buttonY,
                   (control->buttonQueue[axesOnOff]).buttonWidth,
                   (control->buttonQueue[axesOnOff]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[axesOnOff]).buttonX,
                       (control->buttonQueue[axesOnOff]).buttonY,
                       (control->buttonQueue[axesOnOff]).buttonWidth,
                       (control->buttonQueue[axesOnOff]).buttonHeight,Xoption);
      }
    }

    s = (control->buttonQueue[axesOnOff]).text;
    strL = strlen(s);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[axesOnOff]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[axesOnOff]).buttonX +
                     centerX(processGC,s,strL,
                             (control->buttonQueue[axesOnOff]).buttonWidth),
                     (control->buttonQueue[axesOnOff]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[axesOnOff]).buttonHeight),
                     s,strL,Xoption);
    if (viewData.style == smooth) {
      if (multiColorFlag) redoDither = yes;
      else redoColor = yes;
    }
    drawViewport(Xoption);
    break;


  case zoomx:
    if (viewport->zoomXOn) {
      viewport->zoomXOn = no;
      (control->buttonQueue[zoomx]).textColor = offColor;
      if (mono) {
        XChangeShade(dsply,offShade);
        XShadeRectangle(dsply,control->controlWindow,
                   (control->buttonQueue[zoomx]).buttonX,
                   (control->buttonQueue[zoomx]).buttonY,
                   (control->buttonQueue[zoomx]).buttonWidth,
                   (control->buttonQueue[zoomx]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[zoomx]).buttonX,
                       (control->buttonQueue[zoomx]).buttonY,
                       (control->buttonQueue[zoomx]).buttonWidth,
                       (control->buttonQueue[zoomx]).buttonHeight,Xoption);
      }
    } else {
      viewport->zoomXOn = yes;
      (control->buttonQueue[zoomx]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[zoomx]).buttonX,
                   (control->buttonQueue[zoomx]).buttonY,
                   (control->buttonQueue[zoomx]).buttonWidth,
                   (control->buttonQueue[zoomx]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[zoomx]).buttonX,
                       (control->buttonQueue[zoomx]).buttonY,
                       (control->buttonQueue[zoomx]).buttonWidth,
                       (control->buttonQueue[zoomx]).buttonHeight,Xoption);
      }
    }

    s = (control->buttonQueue[zoomx]).text;
    strL = strlen(s);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[zoomx]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[zoomx]).buttonX +
                     centerX(processGC,s,strL,
                             (control->buttonQueue[zoomx]).buttonWidth),
                     (control->buttonQueue[zoomx]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[zoomx]).buttonHeight),
                     s,strL,Xoption);
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    break;


  case zoomy:
    if (viewport->zoomYOn) {
      viewport->zoomYOn = no;
      (control->buttonQueue[zoomy]).textColor = offColor;
      if (mono) {
        XChangeShade(dsply,offShade);
        XShadeRectangle(dsply,control->controlWindow,
                   (control->buttonQueue[zoomy]).buttonX,
                   (control->buttonQueue[zoomy]).buttonY,
                   (control->buttonQueue[zoomy]).buttonWidth,
                   (control->buttonQueue[zoomy]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[zoomy]).buttonX,
                       (control->buttonQueue[zoomy]).buttonY,
                       (control->buttonQueue[zoomy]).buttonWidth,
                       (control->buttonQueue[zoomy]).buttonHeight,Xoption);
      }
    } else {
      viewport->zoomYOn = yes;
      (control->buttonQueue[zoomy]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[zoomy]).buttonX,
                   (control->buttonQueue[zoomy]).buttonY,
                   (control->buttonQueue[zoomy]).buttonWidth,
                   (control->buttonQueue[zoomy]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[zoomy]).buttonX,
                       (control->buttonQueue[zoomy]).buttonY,
                       (control->buttonQueue[zoomy]).buttonWidth,
                       (control->buttonQueue[zoomy]).buttonHeight,Xoption);
      }
    }

    s = (control->buttonQueue[zoomy]).text;
    strL = strlen(s);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[zoomy]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[zoomy]).buttonX +
                     centerX(processGC,s,strL,
                             (control->buttonQueue[zoomy]).buttonWidth),
                     (control->buttonQueue[zoomy]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[zoomy]).buttonHeight),
                     s,strL,Xoption);
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    break;


  case zoomz:
    if (viewport->zoomZOn) {
      viewport->zoomZOn = no;
      (control->buttonQueue[zoomz]).textColor = offColor;
      if (mono) {
        XChangeShade(dsply,offShade);
        XShadeRectangle(dsply,control->controlWindow,
                   (control->buttonQueue[zoomz]).buttonX,
                   (control->buttonQueue[zoomz]).buttonY,
                   (control->buttonQueue[zoomz]).buttonWidth,
                   (control->buttonQueue[zoomz]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[zoomz]).buttonX,
                       (control->buttonQueue[zoomz]).buttonY,
                       (control->buttonQueue[zoomz]).buttonWidth,
                       (control->buttonQueue[zoomz]).buttonHeight,Xoption);
      }
    } else {
      viewport->zoomZOn = yes;
      (control->buttonQueue[zoomz]).textColor = onColor;
      if (mono) {
        GSetForeground(globalGC1,(float)backgroundColor,Xoption);
        XFillRectangle(dsply, control->controlWindow, globalGC1,
                   (control->buttonQueue[zoomz]).buttonX,
                   (control->buttonQueue[zoomz]).buttonY,
                   (control->buttonQueue[zoomz]).buttonWidth,
                   (control->buttonQueue[zoomz]).buttonHeight);
        GSetForeground(globalGC1,(float)foregroundColor,Xoption);
        GDrawRectangle(globalGC1, control->controlWindow,
                       (control->buttonQueue[zoomz]).buttonX,
                       (control->buttonQueue[zoomz]).buttonY,
                       (control->buttonQueue[zoomz]).buttonWidth,
                       (control->buttonQueue[zoomz]).buttonHeight,Xoption);
      }
    }

    s = (control->buttonQueue[zoomz]).text;
    strL = strlen(s);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[zoomz]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[zoomz]).buttonX +
                     centerX(processGC,s,strL,
                             (control->buttonQueue[zoomz]).buttonWidth),
                     (control->buttonQueue[zoomz]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[zoomz]).buttonHeight),
                     s,strL,Xoption);
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    break;


  case originr:
    viewport->originrOn = yes;
    (control->buttonQueue[originr]).textColor = onColor;
    viewport->objectrOn = no;
    (control->buttonQueue[objectr]).textColor = offColor;
    viewport->originFlag = yes;
    if (mono) {
      XChangeShade(dsply,offShade);
      XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[objectr]).buttonX,
                 (control->buttonQueue[objectr]).buttonY,
                 (control->buttonQueue[objectr]).buttonWidth,
                 (control->buttonQueue[objectr]).buttonHeight);
      GSetForeground(globalGC1,(float)foregroundColor,Xoption);
      GDrawRectangle(globalGC1, control->controlWindow,
                     (control->buttonQueue[objectr]).buttonX,
                     (control->buttonQueue[objectr]).buttonY,
                     (control->buttonQueue[objectr]).buttonWidth,
                     (control->buttonQueue[objectr]).buttonHeight,Xoption);
      GSetForeground(globalGC1,(float)backgroundColor,Xoption);
      XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[originr]).buttonX,
                 (control->buttonQueue[originr]).buttonY,
                 (control->buttonQueue[originr]).buttonWidth,
                 (control->buttonQueue[originr]).buttonHeight);
      GSetForeground(globalGC1,(float)foregroundColor,Xoption);
      GDrawRectangle(globalGC1, control->controlWindow,
                     (control->buttonQueue[originr]).buttonX,
                     (control->buttonQueue[originr]).buttonY,
                     (control->buttonQueue[originr]).buttonWidth,
                     (control->buttonQueue[originr]).buttonHeight,Xoption);
    }
    s1 = (control->buttonQueue[objectr]).text;
    strL1 = strlen(s1);
    s2 = (control->buttonQueue[originr]).text;
    strL2 = strlen(s2);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[objectr]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[objectr]).buttonX +
                     centerX(processGC,s1,strL1,
                             (control->buttonQueue[objectr]).buttonWidth),
                     (control->buttonQueue[objectr]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[objectr]).buttonHeight),
                     s1,strL1,Xoption);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[originr]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[originr]).buttonX +
                     centerX(processGC,s2,strL2,
                             (control->buttonQueue[originr]).buttonWidth),
                     (control->buttonQueue[originr]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[originr]).buttonHeight),
                     s2,strL2,Xoption);
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    break;



  case objectr:
    viewport->objectrOn = yes;
    (control->buttonQueue[objectr]).textColor = onColor;
    viewport->originrOn = no;
    (control->buttonQueue[originr]).textColor = offColor;

    viewport->originFlag = no;
    if (mono) {
      XChangeShade(dsply,offShade);
      XShadeRectangle(dsply,control->controlWindow,
                 (control->buttonQueue[originr]).buttonX,
                 (control->buttonQueue[originr]).buttonY,
                 (control->buttonQueue[originr]).buttonWidth,
                 (control->buttonQueue[originr]).buttonHeight);
      GSetForeground(globalGC1,(float)foregroundColor,Xoption);
      GDrawRectangle(globalGC1, control->controlWindow,
                     (control->buttonQueue[originr]).buttonX,
                     (control->buttonQueue[originr]).buttonY,
                     (control->buttonQueue[originr]).buttonWidth,
                     (control->buttonQueue[originr]).buttonHeight,Xoption);
      GSetForeground(globalGC1,(float)backgroundColor,Xoption);
      XFillRectangle(dsply, control->controlWindow, globalGC1,
                 (control->buttonQueue[objectr]).buttonX,
                 (control->buttonQueue[objectr]).buttonY,
                 (control->buttonQueue[objectr]).buttonWidth,
                 (control->buttonQueue[objectr]).buttonHeight);
      GSetForeground(globalGC1,(float)foregroundColor,Xoption);
      GDrawRectangle(globalGC1, control->controlWindow,
                     (control->buttonQueue[objectr]).buttonX,
                     (control->buttonQueue[objectr]).buttonY,
                     (control->buttonQueue[objectr]).buttonWidth,
                     (control->buttonQueue[objectr]).buttonHeight,Xoption);
    }
    s1 = (control->buttonQueue[objectr]).text;
    strL1 = strlen(s1);
    s2 = (control->buttonQueue[originr]).text;
    strL2 = strlen(s2);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[objectr]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[objectr]).buttonX +
                     centerX(processGC,s1,strL1,
                             (control->buttonQueue[objectr]).buttonWidth),
                     (control->buttonQueue[objectr]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[objectr]).buttonHeight),
                     s1,strL1,Xoption);

    GSetForeground(processGC,
           (float)monoColor((control->buttonQueue[originr]).textColor),Xoption);
    GDrawImageString(processGC,control->controlWindow,
                     (control->buttonQueue[originr]).buttonX +
                     centerX(processGC,s2,strL2,
                             (control->buttonQueue[originr]).buttonWidth),
                     (control->buttonQueue[originr]).buttonY +
                     centerY(processGC,
                             (control->buttonQueue[originr]).buttonHeight),
                     s2,strL2,Xoption);
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    break;



  case ps:
    strcpy(control->message,"   Creating postscript file ...   ");
    writeControlMessage();
    if (PSInit(viewport->viewWindow, viewport->titleWindow) == psError) {
      strcpy(control->message,"    Aborted: PSInit error.    ");
      writeControlMessage();
      return;   /* make new tmpnam for new file */
    }

    redoSmooth = yes;
    drawViewport(PSoption);     /* draw picture in PS; create ps script file */

    if (PSCreateFile(viewBorderWidth, viewport->viewWindow,
         viewport->titleWindow, viewport->title) == psError) {
      strcpy(control->message,"    Aborted: PSCreateFile error.    ");
      writeControlMessage();
      return;
    }

    clearControlMessage();
    strcpy(control->message,PSfilename);
    strcat(control->message," in working dir ");
    writeControlMessage();
    break;



  case pixmap:
    strcpy(control->message,"   Creating axiom3D.xpm now ...   ");
    writeControlMessage();
      XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
      write_pixmap_file(dsply,scrn,"axiom3D.xpm",
                        viewport->titleWindow,0,0,vwInfo.width,
                        vwInfo.height+titleHeight);
    clearControlMessage();
    strcpy(control->message,"   axiom3D.xpm in working dir     ");
    writeControlMessage();
    break;



  case transparent:
  case opaqueMesh:
  case render:
  case smooth:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    viewData.style = bKey;
    drawViewport(Xoption);  /* draw picture in viewWindow with X routines */
    break;


  case closeAll:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    doingPanel = QUITpanel;
    viewport->closing = yes;
    XMapWindow(dsply,quitWindow);
    break;


  case quitReturn:
    XUnmapWindow(dsply,quitWindow);
    break;


  case quitAbort:
    doingPanel = CONTROLpanel;
    XUnmapWindow(dsply,quitWindow);
    break;


  case saveit:
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();
    saveFlag = yes;
    doingPanel = SAVEpanel;
    XMapWindow(dsply,saveWindow);
    break;


  case saveExit:
    saveFlag = no;
    doingPanel = CONTROLpanel;
    XUnmapWindow(dsply,saveWindow);
    break;


  case xy:
    viewport->theta = pi;
    viewport->phi   = 0.0;
    viewport->axestheta = pi;
    viewport->axesphi   = 0.0;
    rotated = yes;
    viewport->yzOn = viewport->xzOn = no;
    viewport->xyOn = yes;
    drawViewport(Xoption);
    break;


  case xz:
    viewport->theta = pi;
    viewport->phi   = -pi_half;
    viewport->axestheta = pi;
    viewport->axesphi   = -pi_half;
    rotated = yes;
    viewport->yzOn = viewport->xyOn = no;
    viewport->xzOn = yes;
    drawViewport(Xoption);
    break;


  case yz:
    viewport->theta = pi_half;
    viewport->phi   = -pi_half;
    viewport->axestheta = pi_half;
    viewport->axesphi   = -pi_half;
    rotated = yes;
    viewport->xzOn = viewport->xyOn = no;
    viewport->yzOn = yes;
    drawViewport(Xoption);
    break;


  default:
    fprintf(stderr,"Received a non-functioning button request: %d \n",bKey);
    break;
  } /* switch (action) */

} /* processEvents() */



/************************** X Event Processing *****************************/
void 
processEvents(void)
{

  XEvent                *event, tempEvent;
  Window                whichWindow;
  buttonStruct          *controlButton;
  mouseCoord            mouseXY, linearMouseXY;
  int                   someInt, mouseW4, mouseH4;
  int                   toggleReady =yes;
  int                   checkButton = no;
  int                   first_time = yes;
  int                   changingColor = yes;
  int                   gotEvent = 0, exposeView = no;
  int                   tempTW, tempTH, tempVW, tempVH;
  int                   buttonTablePtr;
  float                 f1, f2;
  int                   px, py, lx, ly;
  unsigned int          lbuttons;
  Window                dummy;
  int                   Xcon,externalControl,len;
  fd_set                rd;

  externalControl = 0;
  Xcon = ConnectionNumber(dsply);

  /** assign lightPointer for light panel **/
  lightPointer[0] = tempLightPointer[0] = viewport->lightVector[0];
  lightPointer[1] = tempLightPointer[1] = viewport->lightVector[1];
  lightPointer[2] = tempLightPointer[2] = viewport->lightVector[2];

  if (!(event = (XEvent *)saymem("process.c",1,sizeof(XEvent)))) {
    fprintf(stderr,"Ran out of memory initializing event processing.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }

  controlButton = 0;

  while(1) {

    /* Store old viewport window size attributes for resizing comparison. */
    XGetWindowAttributes(dsply,viewport->titleWindow,&graphWindowAttrib);
    tempTW = graphWindowAttrib.width;
    tempTH = graphWindowAttrib.height;
    XGetWindowAttributes(dsply,viewport->viewWindow,&graphWindowAttrib);
    tempVW = graphWindowAttrib.width;
    tempVH = graphWindowAttrib.height;

    /* Get the next X event. The check for pending events is so that
       a held down mouse button is interpreted as an event
       even if nothing is pending. */

    len=0;
    while(len<=0) {
      FD_ZERO(&rd);
      if (externalControl==0) FD_SET(0, &rd);
      FD_SET(Xcon,&rd);
      
      if (XEventsQueued(dsply, QueuedAlready)) {
        len=1;
        break;
      }
      if (!followMouse) 
        len=select(FD_SETSIZE,(void *)&rd,0,0,0); 
      else 
        len=1;
    }
    
    if (FD_ISSET(Xcon,&rd)|| 
        XEventsQueued(dsply, QueuedAfterFlush) || 
        followMouse) {
     
      if (followMouse) {
        if (XPending(dsply)) 
          XNextEvent(dsply,event);
        gotEvent++;
      } else {
        XNextEvent(dsply,event);
        gotEvent++;
      }
      
      if (gotToggle || !followMouse) 
        checkButton = no;
      
      if (gotEvent) {
        whichWindow = ((XButtonEvent *)event)->window;
        first_time = no;
        
        switch(((XEvent *)event)->type) {
        case ClientMessage:
          if (event->xclient.data.l[0] == wm_delete_window) {
            goodbye(-1);
          }
          else {
            fprintf(stderr,"Unknown Client Message ...\n");
          }
          break;
        case Expose:
          if (whichWindow == viewport->titleWindow) {
            exposeView = yes;
            followMouse = no;
            XSync(dsply,0);
            /* get rid of redundant exposure events */
            XCheckWindowEvent(dsply,viewport->titleWindow,
                              ExposureMask,&tempEvent);
            writeTitle();
            XGetWindowAttributes(dsply,viewport->titleWindow,
                                 &graphWindowAttrib);
            if ((graphWindowAttrib.width!=tempTW) ||
                ((graphWindowAttrib.height)!=tempTH)) {
              XResizeWindow(dsply,viewport->viewWindow,
                            graphWindowAttrib.width, graphWindowAttrib.height);
              redoSmooth = yes;  /* recompute smooth image pixmap if resized */
            }
          } else if (whichWindow == viewport->viewWindow) {
            exposeView = yes;
            followMouse = no;
            XSync(dsply,0);
            XCheckWindowEvent(dsply,viewport->viewWindow,ExposureMask,
                              &tempEvent);
            XGetWindowAttributes(dsply,viewport->viewWindow,&graphWindowAttrib);
            if ((graphWindowAttrib.width!=tempVW) ||
                ((graphWindowAttrib.height)!=tempVH)) {
              XResizeWindow(dsply,viewport->viewWindow,graphWindowAttrib.width,
                            graphWindowAttrib.height);
              redoSmooth = yes;  /* recompute smooth image pixmap if resized */
            }
            drawViewport(Xoption);
            XMapWindow(dsply,whichWindow);
          } else if (whichWindow == lightingWindow) {
            XGetWindowAttributes(dsply,control->controlWindow,
                                 &graphWindowAttrib);
            /* do not allow resizing of control panel */
            if ((graphWindowAttrib.width!=controlWidth) ||
                (graphWindowAttrib.height!=controlHeight)) {
              XResizeWindow(dsply,control->controlWindow,controlWidth,
                            controlHeight);
            }
            drawLightingPanel();
          } else if (whichWindow == volumeWindow) {
            XGetWindowAttributes(dsply,control->controlWindow,
                                 &graphWindowAttrib);
            /* do not allow resizing of control panel */
            if ((graphWindowAttrib.width!=controlWidth) ||
                (graphWindowAttrib.height!=controlHeight)) {
              XResizeWindow(dsply,control->controlWindow,controlWidth,
                            controlHeight);
            }
            drawVolumePanel();
            if (redrawView) {
              redrawView = no;
              drawViewport(Xoption);
            }
          } else if (whichWindow == quitWindow) {
            XGetWindowAttributes(dsply,control->controlWindow,
                                 &graphWindowAttrib);
            /* do not allow resizing of control panel */
            if ((graphWindowAttrib.width!=controlWidth) ||
                (graphWindowAttrib.height!=controlHeight)) {
              XResizeWindow(dsply,control->controlWindow,controlWidth,
                            controlHeight);
            }
            drawQuitPanel();
          } else if (whichWindow == saveWindow) {
            XGetWindowAttributes(dsply,control->controlWindow,
                                 &graphWindowAttrib);
            /* do not allow resizing of control panel */
            if ((graphWindowAttrib.width!=controlWidth) ||
                (graphWindowAttrib.height!=controlHeight)) {
              XResizeWindow(dsply,control->controlWindow,controlWidth,
                            controlHeight);
            }
            drawSavePanel();
          } else if (whichWindow == control->controlWindow) {
            XGetWindowAttributes(dsply,control->controlWindow,
                                 &graphWindowAttrib);
            /* do not allow resizing of control panel */
            if ((graphWindowAttrib.width != controlWidth) ||
                (graphWindowAttrib.height != controlHeight)) {
              XResizeWindow(dsply,control->controlWindow,
                            controlWidth,controlHeight);
            }
            if (viewport->haveControl) drawControlPanel();
            followMouse = no;
            if (redrawView || exposeView) {
              redrawView = no;
              drawViewport(Xoption);
            }
            exposeView = no;
          } else {
            fprintf(stderr,"Not a valid window.\n");
          }
          
          XFlush(dsply);
          while(XCheckTypedWindowEvent(dsply, whichWindow, Expose, &tempEvent));
          break;
          
          
        case MotionNotify:
          exposeView = no;
          if (followMouse) {
            if (whichWindow == control->colormapWindow) {
              while (XCheckMaskEvent(dsply,ButtonMotionMask,event));
              first_time = checkButton = followMouse = changingColor = yes;
              gotToggle = no;
            } else if (whichWindow != control->controlWindow) {
              if (controlButton->pot) {
                while (XCheckMaskEvent(dsply,ButtonMotionMask,event));
                mouseXY = getPotValue(((XButtonEvent *)event)->x,
                                      ((XButtonEvent *)event)->y,
                                      controlButton->xHalf,
                                      controlButton->yHalf);
                linearMouseXY = getLinearPotValue(((XButtonEvent *)event)->x,
                                                  ((XButtonEvent *)event)->y,
                                                  controlButton->xHalf,
                                                  controlButton->yHalf);
                first_time = checkButton = followMouse = yes;
                gotToggle = no;
              }
            }
          }
          break;
          
        case ButtonRelease:
          exposeView = followMouse = no;
          toggleReady = yes;  gotToggle = yes;
          break;
          
        case LeaveNotify:
          XQueryPointer(dsply,rtWindow,&dummy,&dummy,&px,&py,&lx,&ly,&lbuttons);
          if ( (controlButton) &&
               ((whichWindow == control->colormapWindow) ||
                (controlButton->pot)) &&
               (lbuttons & Button1Mask ||
                lbuttons & Button2Mask ||
                lbuttons & Button3Mask)) {
            followMouse = yes;
            if (whichWindow == control->colormapWindow)
              changingColor = yes;
          }
          else {
            followMouse = no;
            changingColor = no;
          }
          toggleReady = yes;
          checkButton = exposeView = no;
          break;
          
        case ButtonPress:
          exposeView = no;  changingColor = no;
          if (whichWindow == viewport->viewWindow) {
            /* find out where the mouse button is pressed on the viewport,
               this determines where to put the control panel */
            XGetWindowAttributes(dsply,whichWindow,&graphWindowAttrib);
            mouseW4 = graphWindowAttrib.width/4;
            if (((XButtonEvent *)event)->x > (graphWindowAttrib.width-mouseW4))
              someInt = 1;
            else {
              mouseH4 = graphWindowAttrib.height/4;
              if (((XButtonEvent *)event)->y >
                  (graphWindowAttrib.height - mouseH4)) someInt = 2;
              else if (((XButtonEvent *)event)->x < mouseW4) someInt = 3;
              else if (((XButtonEvent *)event)->y < mouseH4) someInt = 4;
              else someInt = 0;
            }
            if (viewport->haveControl) {
              XUnmapWindow(dsply,control->controlWindow);
            }
            putControlPanelSomewhere(someInt);
            writeControlMessage();
            XSync(dsply,0);
          } else if (whichWindow == control->colormapWindow) {
            gotToggle = no;
            first_time = checkButton = followMouse = changingColor = yes;
          } else if (whichWindow != control->controlWindow) {
            /* mouse clicked on one of the buttons */
            if (!controlButton || (controlButton->self != whichWindow)) {
              buttonTablePtr = *((int *)XLookUpAssoc(dsply,table,whichWindow));
              /** lighting buttons have indices greater than 100 **/
              /** all buttons share the same array now **/
              controlButton = &(control->buttonQueue[buttonTablePtr]);
            }
            if (controlButton->pot) {
              /* figure out [x,y] for this button in the range [-1..1,-1..1] */
              mouseXY = getPotValue(((XButtonEvent *)event)->x,
                                    ((XButtonEvent *)event)->y,
                                    controlButton->xHalf,controlButton->yHalf);
              linearMouseXY = getLinearPotValue(((XButtonEvent *)event)->x,
                                                ((XButtonEvent *)event)->y,
                                                controlButton->xHalf,
                                                controlButton->yHalf);
              followMouse = yes;
              gotToggle = no;
            } else {
              followMouse = no;
              gotToggle = yes;   /* auto-repeat of toggle buttons not allowed */
              if (toggleReady) toggleReady = no;
            }
            checkButton = yes;
            first_time  = yes;
          }
          break;
          
        default:
          toggleReady = gotToggle = yes;
          exposeView = changingColor = checkButton = followMouse = no;
          break;
          
        } /* switch */
        gotEvent--;
      }  /* if gotEvent */
      
      /* Allow a pressed mouse button on a potentiometer to poll repeatedly. */
      if (followMouse && !first_time && (followMouse++ > mouseWait)) {
        /* reset for next timing loop */
        followMouse = yes;
        checkButton = yes;
      }
      if (checkButton) {
        if (viewport->closing && (controlButton->buttonKey == quitReturn)) {
          goodbye(-1);
        } else if (changingColor) {
          viewport->closing = no;
          /* moving top color map pointer */
          if (((XButtonEvent *)event)->y < colorOffsetY) {
            if (((XButtonEvent *)event)->x < (colorOffset+colorWidth)) {
              /* decreasing top hue number */
              if (viewport->hueTop > 0) viewport->hueTop--;
            } else if (((XButtonEvent *)event)->x >=
                       (colorOffsetX + totalHues*colorWidth + colorWidth)) {
              if (viewport->hueTop < totalHues) viewport->hueTop++;
            } else {
              viewport->hueTop =
                (((XButtonEvent *)event)->x -
                 colorOffsetX + colorWidth/2 - 13) / colorWidth;
            }
          } else if (((XButtonEvent *)event)->y >
                     (colorOffsetY + colorHeight)) {
            /* moving bottom color map pointer */
            if (((XButtonEvent *)event)->x < (colorOffset+colorWidth)) {
              /* decreasing offset number */
              if (viewport->hueOffset > 0) viewport->hueOffset--;
            } else if (((XButtonEvent *)event)->x >=
                       (colorOffsetX + totalHues*colorWidth + colorWidth)) {
              if (viewport->hueOffset < totalHues) viewport->hueOffset++;
            } else {
              viewport->hueOffset =
                (((XButtonEvent *)event)->x -
                 colorOffsetX + colorWidth/2 - 13) / colorWidth;
            }
          }
          /* color map pointer does not wrap around */
          if (viewport->hueOffset < 0) viewport->hueOffset = 0;
          if (viewport->hueTop < 0) viewport->hueTop = 0;
          if (viewport->hueOffset >= totalHues)
            viewport->hueOffset = totalHues-1;
          if (viewport->hueTop >= totalHues) viewport->hueTop = totalHues-1;
          viewport->numberOfHues = viewport->hueTop - viewport->hueOffset;
          if ((viewport->hueTop == viewport->hueOffset) && !viewport->monoOn) {
            redoColor = yes;
            redoDither = no;
          } else {
            redoColor = no;
            redoDither = yes;
          }
          /* update color map changes on control panel */
          drawColorMap();
        } else {
          viewport->closing = no;
          clearControlMessage();
          /* reset all the things that might affect a recalculation for
             redrawing removing hidden surfaces */
          
          /* determine what type of button has been pressed */
          switch(controlButton->buttonKey) {
            
            /*** Potentiometers ***/
          case rotate:
            if (!((viewport->originrOn) && (viewport->objectrOn))) {
              /* update the amount of rotation around the object center
                 of volume */
              if (viewport->objectrOn) {
                viewport->thetaObj += mouseXY.x * rotateFactor;
                viewport->phiObj  -= mouseXY.y * rotateFactor;
                while (viewport->thetaObj >= two_pi) {
                  viewport->thetaObj -= two_pi;
                }
                while (viewport->thetaObj < 0.0) {
                  viewport->thetaObj += two_pi;
                }
                while (viewport->phiObj > pi) {
                  viewport->phiObj -= two_pi;
                }
                while (viewport->phiObj <= -pi) {
                  viewport->phiObj += two_pi;
                }
              }
              /* update amount of rotation around the world space origin */
              if (viewport->originrOn) {
                viewport->theta += mouseXY.x * rotateFactor;
                viewport->phi   -= mouseXY.y * rotateFactor;
                while (viewport->theta >= two_pi) {
                  viewport->theta -= two_pi;
                }
                while (viewport->theta < 0.0) {
                  viewport->theta += two_pi;
                }
                while (viewport->phi > pi) {
                  viewport->phi -= two_pi;
                }
                while (viewport->phi <= -pi) {
                  viewport->phi += two_pi;
                }
                viewport->axestheta += mouseXY.x * rotateFactor;
                viewport->axesphi   -= mouseXY.y * rotateFactor;
                while (viewport->axestheta >= two_pi) {
                  viewport->axestheta -= two_pi;
                }
                while (viewport->axestheta < 0.0) {
                  viewport->axestheta += two_pi;
                }
                while (viewport->axesphi > pi) {
                  viewport->axesphi -= two_pi;
                }
                while (viewport->axesphi <= -pi) {
                  viewport->axesphi += two_pi;
                }
              }
              rotated = yes;
              viewport->yzOn = viewport->xzOn = viewport->xyOn = no;
              clearControlMessage();
              strcpy(control->message,viewport->title);
              writeControlMessage();
              drawViewport(Xoption);
            }
            break;
            
          case zoom:
            /* if uniform scaling */
            if ((viewport->zoomXOn) &&
                (viewport->zoomYOn) &&
                (viewport->zoomZOn)) {
              viewport->scale *= 1 - mouseXY.y * scaleFactor;
            } else { /* else scale axes independently */
              if (viewport->zoomXOn) viewport->scaleX *= (1 - mouseXY.y);
              if (viewport->zoomYOn) viewport->scaleY *= (1 - mouseXY.y);
              if (viewport->zoomZOn) viewport->scaleZ *= (1 - mouseXY.y);
            }
            if (viewport->scale > maxScale) viewport->scale = maxScale;
            else if (viewport->scale < minScale) viewport->scale = minScale;
            if (viewport->scaleX > maxScale) viewport->scaleX = maxScale;
            else if (viewport->scaleX < minScale) viewport->scaleX = minScale;
            if (viewport->scaleY > maxScale) viewport->scaleY = maxScale;
            else if (viewport->scaleY < minScale) viewport->scaleY = minScale;
            if (viewport->scaleZ > maxScale) viewport->scaleZ = maxScale;
            else if (viewport->scaleZ < minScale) viewport->scaleZ = minScale;
            zoomed = yes;
            clearControlMessage();
            strcpy(control->message,viewport->title);
            writeControlMessage();
            if ((viewport->zoomXOn) ||
                (viewport->zoomYOn) ||
                (viewport->zoomZOn))
              drawViewport(Xoption);
            break;
            
          case translate:
            viewport->deltaX += mouseXY.x * translateFactor;
            viewport->deltaY += mouseXY.y * translateFactor;
            if (viewport->deltaX > maxDeltaX) viewport->deltaX = maxDeltaX;
            else if (viewport->deltaX < -maxDeltaX) viewport->deltaX = -maxDeltaX;
            
            if (viewport->deltaY > maxDeltaY) viewport->deltaY = maxDeltaY;
            else if (viewport->deltaY < -maxDeltaY) viewport->deltaY = -maxDeltaY;
            translated = yes;
            clearControlMessage();
            strcpy(control->message,viewport->title);
            writeControlMessage();
            drawViewport(Xoption);
            break;
            
            /*** Lighting panel ***/
          case lightMoveXY:
            tempLightPointer[0] = linearMouseXY.x;
            tempLightPointer[1] = linearMouseXY.y;
            if (tempLightPointer[0] > 1) tempLightPointer[0] = 1;
            else if (tempLightPointer[0] < -1) tempLightPointer[0] = -1;
            if (tempLightPointer[1] > 1) tempLightPointer[1] = 1;
            else if (tempLightPointer[1] < -1) tempLightPointer[1] = -1;
            movingLight = yes;
            drawLightingAxes();
            break;
            
          case lightMoveZ:
            tempLightPointer[2] = linearMouseXY.y;
            /* linearMouse => no checking necessary */
            if (tempLightPointer[2] > 1) tempLightPointer[2] = 1;
            else if (tempLightPointer[2] < -1) tempLightPointer[2] = -1;
            movingLight = yes;
            drawLightingAxes();
            break;
            
            /* changes the light intensity */
          case lightTranslucent:
            tempLightIntensity = (linearMouseXY.y+1)/2;
            if (tempLightIntensity > 1) tempLightIntensity = 1;
            else if (tempLightIntensity < 0) tempLightIntensity = 0;
            changedIntensity = yes;
            drawLightTransArrow();
            break;
            
            /*** volume panel ***/
          case frustrumBut:
            screenX = ((XButtonEvent *)event)->x;
            if inside(eyeMinX,eyeMaxX) {
              /* object coordinate */
              f2 = mouseXY.x * (maxEyeDistance - minEyeDistance) +
                minEyeDistance;
              if (f2 != viewData.eyeDistance) {
                doingVolume = 2;   /* flag for using screenX */
                changedEyeDistance = yes;
                viewData.eyeDistance = f2;
                drawFrustrum();
                drawViewport(Xoption);
              }
            }
            else if inside(hitherMinX,hitherMaxX) {
              f1 = ((float)hitherMaxX - ((XButtonEvent *)event)->x) /
                (hitherMaxX - hitherMinX);
              /* object coordinate */
              f2 = f1 * (clipPlaneMax - clipPlaneMin) + clipPlaneMin;
              if (f2 != viewData.clipPlane) {
                doingVolume = 3;   /* flag for using screenX */
                viewData.clipPlane = f2;
                drawFrustrum();
                drawViewport(Xoption);
              }
            }
            else {
              doingVolume = 1;  /* check out doingVolume */
              doingPanel = VOLUMEpanel;
            }
            break;
            
          case clipXBut:  /* this is a horizontal button */
            clipValue = linearMouseXY.x * 0.5 + 0.5;   /* normalize to 0..1 */
            if (lessThan(clipValue,0.0)) clipValue = 0.0;
            if (greaterThan(clipValue,1.0)) clipValue = 1.0;
            if (lessThan(linearMouseXY.y,0.0)) {
              if (!equal(xClipMinN,clipValue)) {
                if (greaterThan(xClipMaxN-clipValue,minDistXY))
                  xClipMinN = clipValue;
                else
                  xClipMinN = xClipMaxN - minDistXY;
                viewData.clipXmin = xClipMinN *
                  (viewData.xmax - viewData.xmin) +
                  viewData.xmin;
                drawClipXBut();
                drawClipVolume();
                if (viewData.clipbox)
                  drawViewport(Xoption);
              }
            } else {
              if (!equal(xClipMaxN,clipValue)) {
                if (greaterThan(clipValue-xClipMinN,minDistXY))
                  xClipMaxN = clipValue;
                else
                  xClipMaxN = xClipMinN + minDistXY;
                viewData.clipXmax = xClipMaxN *
                  (viewData.xmax - viewData.xmin) +
                  viewData.xmin;
                drawClipXBut();
                drawClipVolume();
                if (viewData.clipbox)
                  drawViewport(Xoption);
              }
            }
            break;
            
          case clipYBut:  /* this is a vertical button */
            /* normalize to 0..1, bottom up */
            clipValue = 1 - (linearMouseXY.y * 0.5 + 0.5);
            if (lessThan(clipValue,0.0)) clipValue = 0.0;
            if (greaterThan(clipValue,1.0)) clipValue = 1.0;
            if (lessThan(linearMouseXY.x,0.0)) {
              if (!equal(yClipMinN,clipValue)) {
                if (greaterThan(yClipMaxN-clipValue,minDistXY))
                  yClipMinN = clipValue;
                else
                  yClipMinN = yClipMaxN - minDistXY;
                viewData.clipYmin = yClipMinN *
                  (viewData.ymax - viewData.ymin) +
                  viewData.ymin;
                drawClipYBut();
                drawClipVolume();
                if (viewData.clipbox)
                  drawViewport(Xoption);
              }
            } else {
              if (!equal(yClipMaxN,clipValue)) {
                if (greaterThan(clipValue-yClipMinN,minDistXY))
                  yClipMaxN = clipValue;
                else
                  yClipMaxN = yClipMinN + minDistXY;
                viewData.clipYmax = yClipMaxN *
                  (viewData.ymax - viewData.ymin) +
                  viewData.ymin;
                drawClipYBut();
                drawClipVolume();
                if (viewData.clipbox)
                  drawViewport(Xoption);
              }
            }
            break;
            
          case clipZBut:  /* this is a diagonally aligned button! */
            /* f1 is the distance from the center of the button along
               the diagonal line with a slope of -1. If f1 is negative,
               the direction is downward from the center, if f1 is
               positive, the direction is upward from the center.
               Note that there ought to be a constant factor, namely
               cos(45), multiplied by f1 for the correct normalized value;
               however, we exploit this by foreshortening the length of the
               diagonal by that same factor (so instead of normalizing the
               numbers to, the line we normalize the line to the numbers)
               since we need to shorten the line at some point anyway
               (both to match the length of the diagonal side of the box
               and to allow more area for mouse input. */
            
            /* cos(45), etc => 0.4 */
            f1 = (linearMouseXY.x - linearMouseXY.y) * 0.4 + 0.5;
            if (lessThan(f1,0.0)) f1 = 0.0;
            if (greaterThan(f1,1.0)) f1 = 1.0;
            /* note that x<y => moving upward */
            if (lessThan(-linearMouseXY.x,linearMouseXY.y)) {
              if (!equal(zClipMaxN,f1)) {
                if (greaterThan(f1-zClipMinN,minDistZ))
                  zClipMaxN = f1;
                else
                  zClipMaxN = zClipMinN + minDistZ;
                viewData.clipZmax = zClipMaxN *
                  (viewData.zmax - viewData.zmin) +
                  viewData.zmin;
                drawClipZBut();
                drawClipVolume();
                if (viewData.clipbox)
                  drawViewport(Xoption);
              }
            } else {
              if (!equal(zClipMinN,f1)) {
                if (greaterThan(zClipMaxN-f1,minDistZ))
                  zClipMinN = f1;
                else
                  zClipMinN = zClipMaxN - minDistZ;
                viewData.clipZmin = zClipMinN *
                  (viewData.zmax - viewData.zmin) +
                  viewData.zmin;
                drawClipZBut();
                drawClipVolume();
                if (viewData.clipbox)
                  drawViewport(Xoption);
              }
            }   /* if lessThan(x,y) */
            break;
            
          case perspectiveBut:
            if ((viewData.perspective = !viewData.perspective)) {
              switchedPerspective = yes;
              GSetForeground(volumeGC,
                             (float)monoColor((control->buttonQueue[perspectiveBut]).textColor),Xoption);
              GDrawString(volumeGC,volumeWindow,
                          controlButton->buttonX +
                          centerX(volumeGC,"x",1,controlButton->buttonWidth),
                          controlButton->buttonY +
                          centerY(volumeGC,controlButton->buttonHeight),
                          "x",1,Xoption);
            }
            else
              XClearArea(dsply,volumeWindow,
                         controlButton->buttonX+1,
                         controlButton->buttonY+1,
                         controlButton->buttonHeight-2,
                         controlButton->buttonWidth-2,
                         False);
            drawViewport(Xoption);
            break;
            
          case clipRegionBut:
            if ((viewData.clipbox = !viewData.clipbox)) {
              GSetForeground(volumeGC,
                             (float)monoColor((control->buttonQueue[clipRegionBut]).textColor),Xoption);
              GDrawString(volumeGC,volumeWindow,
                          controlButton->buttonX +
                          centerX(volumeGC,"x",1,controlButton->buttonWidth),
                          controlButton->buttonY +
                          centerY(volumeGC,controlButton->buttonHeight),
                          "x",1,Xoption);
            }
            else
              XClearArea(dsply,volumeWindow,
                         controlButton->buttonX+1,
                         controlButton->buttonY+1,
                         controlButton->buttonWidth-2,
                         controlButton->buttonHeight-2,
                         False);
            
            drawViewport(Xoption);
            break;
            
          case clipSurfaceBut:
            if ((viewData.clipStuff = !viewData.clipStuff)) {
              GSetForeground(volumeGC,
                             (float)monoColor((control->buttonQueue[clipSurfaceBut]).textColor),Xoption);
              GDrawString(volumeGC,volumeWindow,
                          controlButton->buttonX +
                          centerX(volumeGC,"x",1,controlButton->buttonWidth),
                          controlButton->buttonY +
                          centerY(volumeGC,controlButton->buttonHeight),
                          "x",1,Xoption);
            }
            else
              XClearArea(dsply,volumeWindow,
                         controlButton->buttonX+1,
                         controlButton->buttonY+1,
                         controlButton->buttonWidth-2,
                         controlButton->buttonHeight-2,
                         False);
            break;
            
          default:
            buttonAction(controlButton->buttonKey);
          } /* switch on buttonKey */
          
        }  /* else - not closing */
      }   /* if checkButton */
    } /* if FD_ISSET(Xcon,.. */
    else if (FD_ISSET(0,&rd)) {
      externalControl = spadAction();
      if (spadDraw && (externalControl==0)) drawViewport(Xoption);
    }
  }       /* for (until closed) */
}         /* processEvents() */



