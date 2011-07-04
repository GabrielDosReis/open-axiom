/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2011, Gabriel Dos Reis.
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

#define _CONTROL2D_C
#include "openaxiom-c-macros.h"

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "cfuns.h"
#include "header2.h"
#include "buttons2d.H1"
#include "all_2d.H1"
#include "Gfun.H1"
#include "XSpadFill.h"
#include "util.H1"


#include "mouse11.bitmap"
#include "mouse11.mask"

/* Defines the pixmap for the arrow displayed in the scale window */

#define scaleArrowN 11
static XPoint scaleArrow[scaleArrowN] = {
  {55,14},{64,23},{59,23},
  {66,45},{79,45},
  {55,69},
  {31,45},{44,45},
  {51,23},{46,23},{55,14} };

/* Defines the pixmap for the arrows displayed in the translate window */

#define translateArrowN 25
static XPoint translateArrow[translateArrowN] = {
  {55,2},{60,10},{58,10},{58,37},
  {85,37},{85,35},{93,40},{85,45},{85,43},{58,43},
  {58,70},{60,70},{55,78},{50,70},{52,70},{52,43},
  {25,43}, {25,45}, {17,40},  {25,35}, {25,37}, {52,37},
  {52,10},{50,10},{55,2} };

/****************************
 * void writeControlTitle() *
 ****************************/

void 
writeControlTitle(void)
{

  int strlength;

  s = viewport->title;
  strlength = strlen(s);
  XClearArea(dsply,control->controlWindow,0,0,controlWidth,potA,False);
  GSetForeground(anotherGC,(float)controlTitleColor,Xoption);
  GDrawImageString(anotherGC,control->controlWindow,
                   centerX(anotherGC,s,strlength,controlWidth),
                   15,s,strlength,Xoption);

} /* writeControlTitle() */

void 
makeMessageFromData(int whichGraph)
{
  if (viewport->haveControl) {
    if ((graphStateArray[whichGraph].scaleX) > 99.0) {
      strcpy(scaleXReport,"big");
    } else {
      sprintf(scaleXReport,"%4.1f",graphStateArray[whichGraph].scaleX);
    }
    if ((graphStateArray[whichGraph].scaleY) > 99.0) {
      strcpy(scaleYReport,"big");
    } else {
      sprintf(scaleYReport,"%4.1f",graphStateArray[whichGraph].scaleY);
    }
    if ((graphStateArray[whichGraph].centerX) > 999.0) {
      strcpy(deltaXReport,"+big");
    } else if ((graphStateArray[whichGraph].centerX) < -999.0) {
      strcpy(deltaXReport,"-big");
    } else {
      sprintf(deltaXReport,"%4.0f",
              -graphStateArray[whichGraph].centerX /
              graphArray[whichGraph].unitX);
    }
    if ((graphStateArray[whichGraph].centerY) > 999.0) {
      strcpy(deltaYReport,"+big");
    } else if ((graphStateArray[whichGraph].centerY) < -999.0) {
      strcpy(deltaYReport,"-big");
    } else {
      sprintf(deltaYReport,"%4.0f",
              -graphStateArray[whichGraph].centerY /
              graphArray[whichGraph].unitY);
    }
    sprintf(viewport->controlPanel->message,"[%s,%s] >%d< [%s,%s]",
            scaleXReport,scaleYReport,whichGraph+1,deltaXReport,deltaYReport);
  }  /* if haveControl */

} /* makeMessageFromData() */


void 
writeControlMessage(void)
{
  int                strlength;
  controlPanelStruct *cp;
  XWindowAttributes  cwInfo;

  cp = viewport->controlPanel;
  XGetWindowAttributes(dsply,cp->controlWindow,&cwInfo);
  strlength = strlen(cp->message);



  GDrawImageString(controlMessageGC,cp->controlWindow,
                   centerX(globalGC1,cp->message,strlength,controlWidth),
                   controlMessageY + globalFont->max_bounds.ascent - 2,
                   cp->message,strlength,Xoption);
}

/*********************************/
/***  void drawControlPanel()  ***/
/*********************************/

void 
drawControlPanel(void)
{

  controlPanelStruct *cp;
  int i,strlength;
  char *s;

  cp = viewport->controlPanel;
  /* Draw border lines to separate the potentiometer, message, graph select
     and button regions of the control panel. */
  GSetForeground(trashGC,(float)foregroundColor,Xoption);
  GSetLineAttributes(trashGC,3,LineSolid,CapButt,JoinMiter,Xoption);
  GDrawLine(trashGC, cp->controlWindow, 0, potA, controlWidth, potA, Xoption);
  GSetLineAttributes(trashGC,2,LineSolid,CapButt,JoinMiter,Xoption);
  GDrawLine(trashGC, cp->controlWindow, 0, potB, controlWidth, potB, Xoption);
  GDrawLine(trashGC, cp->controlWindow, 0, messageBot,
            controlWidth, messageBot, Xoption);
  GDrawLine(trashGC, cp->controlWindow, 0, 286, controlWidth, 286, Xoption);

  /** put the line width as 1 last because used below as well **/
  GSetLineAttributes(trashGC,1,LineSolid,CapButt,JoinMiter,Xoption);
  GDrawRectangle(trashGC,cp->controlWindow,closeLeft,closeTop,
                 (controlWidth-closeLeft+8),(controlHeight-closeTop+8),Xoption);

  /* Write potentiometer titles on the control panel. */
  writeControlTitle();
  GSetForeground(globGC,(float)controlPotHeaderColor,Xoption);
  s = "Scale";
  strlength = strlen(s);
  GDrawString(globGC,cp->controlWindow,
              centerX(globGC,s,strlength,
                      cp->buttonQueue[scale2D].buttonWidth) +
              cp->buttonQueue[scale2D].buttonX, 31+headerHeight,s,strlength,Xoption);

  s = "Translate";
  strlength = strlen(s);
  GDrawString(globGC,cp->controlWindow,
              centerX(globGC,s,strlength,
                      cp->buttonQueue[translate2D].buttonWidth) +
              cp->buttonQueue[translate2D].buttonX,
              31+headerHeight,s,strlen(s),Xoption);

  GSetForeground(globGC,(float)controlColorColor,Xoption);

  /* Write title of the graph selection window. */
  s = "Graphs";
  strlength = strlen(s);
  GDrawString(globGC,cp->controlWindow,
              centerX(globGC,s,strlength,controlWidth),graphHeaderHeight,
              s,strlength,Xoption);

  /* Write titles on regular buttons and draw pixmaps on potentiometers. */

  for (i=0; i<(maxButtons2D); i++) {
    if ((cp->buttonQueue[i]).pot) {

      GSetForeground(globalGC1,(float)buttonColor,Xoption);
      GDrawRectangle(globalGC1,cp->controlWindow,
                     (cp->buttonQueue[i]).buttonX,
                     (cp->buttonQueue[i]).buttonY,
                     (cp->buttonQueue[i]).buttonWidth,
                     (cp->buttonQueue[i]).buttonHeight,Xoption);

      GSetForeground(trashGC,
                     (float)monoColor((cp->buttonQueue[i]).textColor),Xoption);

      GDrawLine(globalGC1,cp->controlWindow, /* trashGC, */
                (cp->buttonQueue[i]).buttonX + (cp->buttonQueue[i]).xHalf,
                (cp->buttonQueue[i]).buttonY,
                (cp->buttonQueue[i]).buttonX + (cp->buttonQueue[i]).xHalf,
                (cp->buttonQueue[i]).buttonY + 2*(cp->buttonQueue[i]).yHalf,Xoption);
      GDrawLine(globalGC1,cp->controlWindow, /* trashGC, */
                (cp->buttonQueue[i]).buttonX,
                (cp->buttonQueue[i]).buttonY + (cp->buttonQueue[i]).yHalf,
                (cp->buttonQueue[i]).buttonX + 2*(cp->buttonQueue[i]).xHalf,
                (cp->buttonQueue[i]).buttonY + (cp->buttonQueue[i]).yHalf,Xoption);
      switch (i) {
      case scale2D:
        GDrawLines(trashGC,cp->controlWindow,scaleArrow,
                   scaleArrowN,CoordModeOrigin,Xoption);
        break;
      case translate2D:
        GDrawLines(trashGC,cp->controlWindow,translateArrow,
                   translateArrowN,CoordModeOrigin,Xoption);
        break;
      } /* switch i */
    } else if (cp->buttonQueue[i].graphNum) {

      if (mono) {
        if (graphStateArray[i-graphStart].showing) {
          GSetForeground(graphGC,(float)backgroundColor,Xoption);
          GSetBackground(graphGC,(float)foregroundColor,Xoption);
        } else {
          GSetForeground(graphGC,(float)foregroundColor,Xoption);
          GSetBackground(graphGC,(float)backgroundColor,Xoption);
        }
        strlength = strlen((cp->buttonQueue[i]).text);
        GDrawImageString(graphGC,cp->controlWindow,
                         (cp->buttonQueue[i]).buttonX +
                         centerX(graphGC,cp->buttonQueue[i].text,
                                 strlength,(cp->buttonQueue[i]).buttonWidth),
                         (cp->buttonQueue[i]).buttonY +
                         centerY(graphGC,(cp->buttonQueue[i]).buttonHeight),
                         cp->buttonQueue[i].text,strlength,Xoption);
      } else {
        if (graphStateArray[i-graphStart].showing)
          GSetForeground(graphGC,(float)graphBarShowingColor,Xoption);
        else
          GSetForeground(graphGC,(float)graphBarHiddenColor,Xoption);
        strlength = strlen((cp->buttonQueue[i]).text);
        GDrawString(graphGC,cp->controlWindow,
                    (cp->buttonQueue[i]).buttonX +
                    centerX(graphGC,cp->buttonQueue[i].text,
                            strlength,(cp->buttonQueue[i]).buttonWidth),
                    (cp->buttonQueue[i]).buttonY +
                    centerY(graphGC,(cp->buttonQueue[i]).buttonHeight),
                    cp->buttonQueue[i].text,strlength,Xoption);
        }
    } else if (cp->buttonQueue[i].graphSelect) {
      /* The select characters are defined as: "^" for on and "-" for off. */
      if (graphStateArray[i-graphSelectStart].selected) {
        GSetForeground(graphGC,(float)graphBarSelectColor,Xoption);
        strcpy((cp->buttonQueue[i]).text,"^");
      } else {
        GSetForeground(graphGC,(float)graphBarNotSelectColor,Xoption);
        *(cp->buttonQueue[i]).text = '-';
        strcpy((cp->buttonQueue[i]).text,"-");
      }
       GDrawString(graphGC,cp->controlWindow,
                 (cp->buttonQueue[i]).buttonX +
                 centerX(graphGC,cp->buttonQueue[i].text,
                         strlength,(cp->buttonQueue[i]).buttonWidth),
                 (cp->buttonQueue[i]).buttonY +
                 centerY(graphGC,(cp->buttonQueue[i]).buttonHeight),
                 cp->buttonQueue[i].text,strlength,Xoption);
    }
    else {  /* a regular button */
      int isOn = 1;

      switch(i) {
        case pointsOnOff:
          isOn = pointsON = graphStateArray[0].pointsOn;
          if (graphStateArray[0].pointsOn)
            strcpy((cp->buttonQueue[i]).text,"Pts On ");
          else
            strcpy((cp->buttonQueue[i]).text,"Pts Off");
          break;

        case spline2D:
          isOn = splineON = graphStateArray[0].splineOn;
          if (graphStateArray[0].splineOn)
            strcpy((cp->buttonQueue[i]).text, "Box On ");
          else
            strcpy((cp->buttonQueue[i]).text, "Box Off");
          break;

        case connectOnOff:
          isOn = connectON = graphStateArray[0].connectOn;
          if (graphStateArray[0].connectOn)
            strcpy((cp->buttonQueue[i]).text, "Lines On ");
          else
            strcpy((cp->buttonQueue[i]).text, "Lines Off");
          break;

        case axesOnOff2D:
          isOn = axesON = graphStateArray[0].axesOn;
          if (graphStateArray[0].axesOn)
            strcpy((cp->buttonQueue[i]).text , "Axes On ");
          else
            strcpy((cp->buttonQueue[i]).text , "Axes Off");
          break;

        case unitsOnOff2D:
          isOn = unitsON = graphStateArray[0].unitsOn;
          if (graphStateArray[0].unitsOn)
           strcpy( (cp->buttonQueue[i]).text , "Units On ");
          else
           strcpy( (cp->buttonQueue[i]).text , "Units Off");
          break;
        case closeAll2D:
          isOn = 0;

        default:
          break;
      } /* switch i */

      s = (cp->buttonQueue[i]).text;
      strlength = strlen(s);

      GDrawPushButton(dsply, globalGC1, trashGC, processGC, cp->controlWindow,
          (cp->buttonQueue[i]).buttonX, (cp->buttonQueue[i]).buttonY,
          (cp->buttonQueue[i]).buttonWidth, (cp->buttonQueue[i]).buttonHeight,
          isOn, s,buttonColor,
          monoColor((cp->buttonQueue[i]).textColor), Xoption);

    } /* else a regular button */
  } /* for each button */

  /* Refresh the latest message */
  makeMessageFromData(0);
  writeControlMessage();
  XFlush(dsply);

}    /*** drawControlPanel ***/

controlXY 
getControlXY(int whereDoYouWantPanel)
{
  XWindowAttributes wAttr, wAttrib;
  controlXY         cXY;
  int               tmp=1;
  Window            rootW, parentW, *childrenWs, tmpW;
  unsigned int      nChildren;

  tmpW = viewport->titleWindow;
  while(tmp) {
    XQueryTree(dsply,tmpW,&rootW,&parentW,&childrenWs,&nChildren);
    XFree(childrenWs);
    if (parentW == rtWindow) tmp = 0;
    else tmpW = parentW;
  }
  XGetWindowAttributes(dsply,tmpW,&wAttrib);

  XGetWindowAttributes(dsply,viewport->titleWindow,&wAttr);
  if (whereDoYouWantPanel) {
    switch (whereDoYouWantPanel) {
    case 1: /* right */
      cXY.putX = wAttrib.x + wAttrib.width;
      cXY.putY = wAttrib.y;
      break;
    case 2: /* bottom */
      cXY.putX = wAttrib.x + (wAttr.width - controlWidth)/2;  /* center it */
      cXY.putY = wAttrib.y + wAttrib.height;
      break;
    case 3: /* left */
      cXY.putX = wAttrib.x - controlWidth - borderWidth;
      cXY.putY = wAttrib.y;
      break;
    case 4: /* top */
      cXY.putX = wAttrib.x + (wAttr.width - controlWidth)/2;
      cXY.putY = wAttrib.y - controlHeight - borderHeight;
    }
  } else {
    if ((physicalWidth - (wAttrib.x + wAttr.width)) >= controlWidth) {
      cXY.putX = wAttrib.x + wAttrib.width;
      cXY.putY = wAttrib.y;
    } else if ((physicalHeight - (wAttrib.y + wAttr.height)) >=
               controlHeight) {
      cXY.putX = wAttrib.x + (wAttr.width - controlWidth)/2;
      cXY.putY = wAttrib.y + wAttrib.height;
    } else if (wAttrib.x >= controlWidth) {
      cXY.putX = wAttrib.x - controlWidth - borderWidth;
      cXY.putY = wAttrib.y;
    } else if (wAttrib.y >= controlHeight) {
      cXY.putX = wAttrib.x + (wAttr.width - controlWidth)/2;
      cXY.putY = wAttrib.y - controlHeight - borderHeight;
    } else {                       /* put inside of viewport */
      cXY.putX = wAttrib.x + wAttr.width - controlWidth;
      cXY.putY = wAttrib.y + wAttr.height - controlHeight;
    }
  }
  return(cXY);

}

/************************************************/
/***  controlPanelStruct *makeControlPanel()  ***/
/************************************************/

controlPanelStruct *
makeControlPanel(void)
{

  Window cw;
  int                  i,num;
  controlPanelStruct   *control;
  buttonStruct         *buttons;
  controlXY            cXY;
  XSetWindowAttributes cwAttrib, controlAttrib;
  XSizeHints           sizehints;
  Pixmap               mousebits,mousemask;
  XColor               foreColor, backColor;

  if (!(control = (controlPanelStruct *)malloc(sizeof(controlPanelStruct)))) {
    fprintf(stderr,"Ran out of memory trying to create a control panel.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }

  cXY = getControlXY(0);

  /* Define and assign a mouse cursor. */
  mousebits = XCreateBitmapFromData(dsply,rtWindow,mouseBitmap_bits,
                                    mouseBitmap_width,mouseBitmap_height);
  mousemask = XCreateBitmapFromData(dsply,rtWindow,mouseMask_bits,
                                    mouseMask_width,mouseMask_height);
  cwAttrib.background_pixel = backgroundColor; /* controlBackground; */
  cwAttrib.border_pixel = foregroundColor;
  cwAttrib.backing_store = WhenMapped;
  cwAttrib.event_mask = controlMASK;
  cwAttrib.colormap = colorMap;
  cwAttrib.override_redirect = overrideManager;
  foreColor.pixel = controlCursorForeground;
  XQueryColor(dsply,colorMap,&foreColor);
  backColor.pixel = controlCursorBackground;
  XQueryColor(dsply,colorMap,&backColor);
  cwAttrib.cursor = XCreatePixmapCursor(dsply,mousebits,mousemask,
                                        &foreColor,&backColor,
                                        mouseBitmap_x_hot,mouseBitmap_y_hot);

  cw = XCreateWindow(dsply,rtWindow,
                     cXY.putX,cXY.putY,controlWidth,controlHeight,3,
                     CopyFromParent,InputOutput,CopyFromParent,
                     controlCreateMASK,&cwAttrib);

  sizehints.flags  = PPosition | PSize;
  sizehints.x      = cXY.putX;
  sizehints.y      = cXY.putY;
  sizehints.width  = controlWidth;
  sizehints.height = controlHeight;

  /*** the None stands for icon pixmap...change.... ***/

  XSetNormalHints(dsply,cw,&sizehints);
  XSetStandardProperties(dsply,cw,"2D Control Panel","2D Control Panel",
                         None,NULL,0,&sizehints);

  control->controlWindow = cw;
  num = initButtons(control->buttonQueue);
  control->numOfButtons = num;
  buttons = control->buttonQueue;

  for (i=0; i<num; i++) {
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
   /* Use buttonKey instead of i because buttonKey has a permanent address */
    XMapWindow(dsply,(control->buttonQueue[i]).self);
  }

           /* Create message window */
  control->messageWindow = XCreateWindow(dsply,cw,0,controlMessageY,
                                         controlWidth,controlMessageHeight,
                                         0,0,InputOnly,CopyFromParent,
                                         messageCreateMASK,&cwAttrib);
  XMapWindow(dsply,control->messageWindow);

  for (i=0; i<scaleArrowN; i++) {
    scaleArrow[i].x += buttons[scale2D].buttonX;
    scaleArrow[i].y += buttons[scale2D].buttonY;
  }
  for (i=0; i<translateArrowN; i++) {
    translateArrow[i].x += buttons[translate2D].buttonX;
    translateArrow[i].y += buttons[translate2D].buttonY;
  }

  viewport->justMadeControl = yes;
  return(control);

} /* makeControlPanel() */






/*****************************************/
/***  void putControlPanelSomewhere()  ***/
/*****************************************/

/* This routine puts up the control panel associated with the viewport
   passed in.  It first tries to put it to the right of the viewport. If
   there isn't enough room there, it tries the bottom and so on going
   clockwise. If the viewport is too big and there is no room to put the
   control panel outside of it, it placed the control panel in the bottom
   right hand corner of the viewport window. */

void 
putControlPanelSomewhere(int whereDoesPanelGo)
{
  controlPanelStruct *control;
  controlXY          whereControl;

  control = viewport->controlPanel;
  whereControl = getControlXY(whereDoesPanelGo);

  viewport->haveControl = yes;

  XRaiseWindow(dsply,control->controlWindow);
  XMoveWindow(dsply,control->controlWindow,whereControl.putX,
              whereControl.putY);

  drawControlPanel();
  if (viewport->justMadeControl) {
    XMapWindow(dsply,control->controlWindow);
    viewport->justMadeControl = no;
  }
  XMapWindow(dsply,control->controlWindow);
}





/************************************/
/***  void clearControlMessage()  ***/
/************************************/

void 
clearControlMessage(void)
{

  strcpy(viewport->controlPanel->message,"");
  
  XClearArea(dsply,viewport->controlPanel->controlWindow,
             0,controlMessageY-2,controlWidth,controlMessageHeight,False);
}


