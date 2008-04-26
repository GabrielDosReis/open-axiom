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

#define _SPADACTION3D_C
#include "axiom-c-macros.h"

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include "header.h"
#include "process.h"
#include "draw.h"

#include "util.H1"
#include "all_3d.H1"

int 
readViewman (void *info,int size)
{
  int m = 0;

  sprintf(errorStr,"%s","read from viewport manager\n");
  m = check(read( 0, info, size));   

  return(m);

}
void 
scalePoint (viewTriple *p)
{

  p->x *= viewData.scaleToView;
  p->y *= viewData.scaleToView;
  p->z *= viewData.scaleToView;

  if (viewData.cmin != viewData.cmax)
    p->c = (p->c - viewData.cmin)/(viewData.cmax-viewData.cmin);
  if (p->c > 1.0) p->c = 1.0;
  else if (p->c < 0) p->c = 0.0;

} /* scalePoint */

  
/********************
 * int spadAction() *
 ********************/

int 
spadAction (void)
{
  int code, viewCommand;
  float f1, f2, f3;
  int i1, i2, i3;


  if (viewAloned==yes) {
      close(0);
      return(-1);
      }
  readViewman(&viewCommand, intSize);

  switch (viewCommand) {

  case rotate:
    readViewman(&f1, floatSize);        
    readViewman(&f2, floatSize);
    viewport->theta = f1;
    viewport->phi   = f2;
    while (viewport->theta >= two_pi) viewport->theta -= two_pi;
    while (viewport->theta < 0.0)     viewport->theta += two_pi;
    while (viewport->phi > pi)        viewport->phi   -= two_pi;
    while (viewport->phi <= -pi)      viewport->phi   += two_pi;
    viewport->axestheta = viewport->theta;
    viewport->axesphi = viewport->phi;
    spadDraw=yes;
    rotated=yes; 
    viewport->yzOn = viewport->xzOn = viewport->xyOn = no;

    break;

  case zoom:
    readViewman(&f1, floatSize);        
    viewport->scale = f1;
    if (viewport->scale > maxScale) viewport->scale = maxScale;
    else if (viewport->scale < minScale) viewport->scale = minScale;          

    spadDraw=yes;
    zoomed = yes; 
    break;

  case zoomx:
    readViewman(&f1, floatSize);        
    readViewman(&f2, floatSize);        
    readViewman(&f3, floatSize);        
    viewport->scaleX = f1;  viewport->scaleY = f2;  viewport->scaleZ = f3;
    if ((viewport->scaleX == 1.0) && 
        (viewport->scaleY == 1.0) && 
        (viewport->scaleZ == 1.0)) {
      viewport->zoomXOn = viewport->zoomYOn = viewport->zoomZOn = yes;
    } else {
      if (viewport->scaleX == 1.0) viewport->zoomXOn = no;
      else {
        if (viewport->scaleX > maxScale) viewport->scaleX = maxScale;
        else if (viewport->scaleX < minScale) viewport->scaleX = minScale;
      }
      if (viewport->scaleY == 1.0) viewport->zoomYOn = no;
      else {
        if (viewport->scaleY > maxScale) viewport->scaleY = maxScale;
        else if (viewport->scaleY < minScale) viewport->scaleY = minScale;
      }
      if (viewport->scaleZ == 1.0) viewport->zoomZOn = no;
      else {
        if (viewport->scaleZ > maxScale) viewport->scaleZ = maxScale;
        else if (viewport->scaleZ < minScale) viewport->scaleZ = minScale;
      }
    }
          
    spadDraw=yes;
    zoomed = yes; 
    break;

  case translate:
    readViewman(&(viewport->deltaX),floatSize);
    readViewman(&(viewport->deltaY),floatSize);
    if (viewport->deltaX > maxDeltaX) viewport->deltaX = maxDeltaX;
    else if (viewport->deltaX < -maxDeltaX) viewport->deltaX = -maxDeltaX; 
    if (viewport->deltaY > maxDeltaY) viewport->deltaY = maxDeltaY;
    else if (viewport->deltaY < -maxDeltaY) viewport->deltaY = -maxDeltaY;
    spadDraw=yes;
    translated = yes; 
    break;

  case modifyPOINT:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&(refPt3D(viewData,i1)->x),floatSize);
    readViewman(&(refPt3D(viewData,i1)->y),floatSize);
    readViewman(&(refPt3D(viewData,i1)->z),floatSize);
    readViewman(&(refPt3D(viewData,i1)->c),floatSize);
    scalePoint(refPt3D(viewData,i1));
    spadDraw=yes;
    break;
    
  case hideControl:
    readViewman(&i1,intSize);
    if (i1) {                         /* show control panel */
      if (viewport->haveControl)
        XUnmapWindow(dsply,control->controlWindow);
      putControlPanelSomewhere(someInt);
    } else {    /* turn off control panel */
      if (viewport->haveControl) {
        viewport->haveControl = no;
        XUnmapWindow(dsply,control->controlWindow);
      }
    }
    break;

  case axesOnOff:
    readViewman(&i1,intSize);
    viewport->axesOn = i1;
    spadDraw=yes;
    if (viewData.style == smooth) {    
      if (multiColorFlag) redoDither = yes; 
      else redoColor = yes; 
    }
    if (viewport->haveControl) drawControlPanel();
    break;

/* Non-uniform scaling is not in OpenAxiom yet. */
/* Neither is object or origin rotation. */

  case perspectiveOnOff:
    readViewman(&i1,intSize);
    viewData.perspective = i1;
    switchedPerspective = yes;
    spadDraw=yes;
    break;

  case region3D:
    readViewman(&i1,intSize);
    viewport->regionOn = i1;
    viewData.box = i1;
    spadDraw=yes;
    if (viewport->haveControl) drawControlPanel();
    redoSmooth = yes; 
    break;

  case clipRegionOnOff:
    readViewman(&i1,intSize);
    viewData.clipbox = i1;
    spadDraw=yes;
    break;

  case clipSurfaceOnOff:
    readViewman(&i1,intSize);
    viewData.clipStuff = i1;
    spadDraw=yes;
    break;

  case eyeDistanceData:
    readViewman(&f1,floatSize);
    viewData.eyeDistance = f1;
    if (viewData.eyeDistance > maxEyeDistance)
      viewData.eyeDistance = maxEyeDistance;
    else if (viewData.eyeDistance < minEyeDistance)
           viewData.eyeDistance = minEyeDistance;
    spadDraw=yes;
    changedEyeDistance = yes; 
    break;

  case hitherPlaneData:
    readViewman(&f1,floatSize);
    viewData.clipPlane = f1;
    spadDraw=yes;
    changedEyeDistance = yes;
    break;

  case colorDef:
    readViewman(&(viewport->hueOffset),intSize);
    readViewman(&(viewport->numberOfHues),intSize);
    /* spadcolors is indexed by 0 */
    viewport->hueOffset --;    
    viewport->numberOfHues --;
    viewport->hueTop = viewport->numberOfHues;
    if (viewport->hueOffset < 0) viewport->hueOffset = 0;
    if (viewport->hueTop < 0) viewport->hueTop = 0;
    if (viewport->hueOffset >= totalHues) 
      viewport->hueOffset = totalHues-1;
    if (viewport->hueTop >= totalHues) viewport->hueTop = totalHues-1;
    viewport->numberOfHues = viewport->hueTop - viewport->hueOffset;
    if ((viewport->hueTop == viewport->hueOffset) && (!viewport->monoOn))
      redoColor = yes;
    else {
      redoColor = no;
      redoDither = yes;
    }
    if (viewport->haveControl) drawColorMap();
    break;

  case closeAll:
    code = check(write(Socket,&ack,intSize));
    goodbye(-1);
    

  case moveViewport:
    readViewman(&i1,intSize);
    readViewman(&i2,intSize);
    XMoveWindow(dsply,viewport->titleWindow,i1,i2);
    XSync(dsply,0);
    break;

 case resizeViewport:
    readViewman(&i1,intSize);
    readViewman(&i2,intSize);
    XResizeWindow(dsply,viewport->titleWindow,i1,i2+titleHeight); 
    XResizeWindow(dsply,viewport->viewWindow,i1,i2);
    spadDraw=yes;
    redoSmooth =yes; 
    break;

  case transparent:
  case opaqueMesh:
  case render:
  case smooth:
    viewData.style = viewCommand;
    spadDraw=yes;
    redoSmooth =yes; 
    break;

  case lightDef:
    readViewman(&(viewport->lightVector[0]),floatSize);
    readViewman(&(viewport->lightVector[1]),floatSize);
    readViewman(&(viewport->lightVector[2]),floatSize);
    normalizeVector(viewport->lightVector);
    movingLight = yes; 
    drawLightingAxes();
    XSync(dsply,0);
    break;

  case translucenceDef:
    readViewman(&backLightIntensity,floatSize);
    tempLightIntensity = backLightIntensity;
    lightIntensity = tempLightIntensity;
    changedIntensity = yes;
    drawLightTransArrow();
    XSync(dsply,0);
    break;

  case changeTitle:
    readViewman(&i1,intSize);
    readViewman(viewport->title,i1);
    viewport->title[i1] = '\0';
    writeTitle();
    switch (doingPanel) {
    case CONTROLpanel:
    case CONTOURpanel:
      writeControlTitle(control->controlWindow);
      break;
    case VOLUMEpanel:
      writeControlTitle(volumeWindow);
      break;
    case LIGHTpanel:
      writeControlTitle(lightingWindow);
      break;
    } /* switch */      
    XFlush(dsply);
    break;

  case writeView:
    readViewman(&i1,intSize);
    readViewman(filename,i1);
    filename[i1] = '\0';
    sprintf(errorStr,"writing of viewport data");
    i3 = 0;
    readViewman(&i2,intSize);
    while (i2) {
      i3 = i3 | (1<<i2);
      readViewman(&i2,intSize);
    }
    if (writeViewport(i3) < 0)
      fprintf(stderr,"          Nothing was written\n");
    break;

  case diagOnOff:
    readViewman(&i1,intSize);
    if (viewData.outlineRenderOn) {
      viewport->diagonals = i1;
      spadDraw=yes;
    } else {
      strcpy(control->message,"  Use this option with Outline  ");
      writeControlMessage();
    }
    break;

  case outlineOnOff:
    readViewman(&i1,intSize);
    if (viewData.style == render) {
      viewData.outlineRenderOn = i1;
      spadDraw=yes;
      if (viewport->haveControl) drawControlPanel();
    } else {
      strcpy(control->message," Use this option in Shaded mode ");
      writeControlMessage();
    }
    break;

  case spadPressedAButton:
    readViewman(&i1,intSize);
    buttonAction(i1);
    break;
  default:
    return(-1);
  } /* switch */


  ack++;
  code = check(write(Socket,&ack,intSize));
  return(0);

}


