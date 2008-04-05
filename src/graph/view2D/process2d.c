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

#define _PROCESS2D_C
#include "axiom-c-macros.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef RIOSplatform
#include <sys/select.h>
#endif

#include "header2.h"

#include "all_2d.H1"
#include "util.H1"
#include "Gfun.H1"
#include "XSpadFill.H1"


static int doit=0;  /* globish variable for picking/dropping/clearing - all sorts of
                2 button sequence events (command & graph #). */


void 
doPick (int i,int bKey)
{
  int vCommand=pick2D;
  
  picking = no;
  /* reset indicator so that we're not in pick/drop/query mode anymore */
  doit = 0;
  if (graphArray[currentGraph].key) {
    check(write(Socket,&vCommand,intSize));
    check(write(Socket,&(graphArray[currentGraph].key),intSize));
    check(write(Socket,&(graphStateArray[currentGraph]),
                sizeof(graphStateStruct)));
    sprintf(control->message,"%s%d","Picked up graph ",currentGraph+1);
  } else
    sprintf(control->message,"%s","This graph is empty!");
  writeControlMessage();
  
  
}


void 
doDrop (int i,int bKey)
{
  int vCommand=drop2D;
  int viewGoAhead;
  
  dropping = no;
  /* reset indicator so that we're not in pick/drop/query mode anymore */
  doit = 0;
  check(write(Socket,&(vCommand),intSize));
  readViewman(&viewGoAhead,intSize);
  if (viewGoAhead < 0) {
    sprintf(control->message,"%s%d","Couldn't drop to graph ",currentGraph+1);
    writeControlMessage();
  } else {
    sprintf(control->message,"%s%d","Dropped onto graph ",currentGraph+1);
    writeControlMessage();
    freeGraph(currentGraph);
    readViewman(&(graphArray[currentGraph].key),intSize);
    getGraphFromViewman(currentGraph);
    /* simulate a button press to turn display number on and select on */
    /* need !yes since it will be inverted */
    graphStateArray[currentGraph].selected = no;
    graphStateArray[currentGraph].showing =
      !(graphStateArray[currentGraph].showing);
    clickedOnGraph(currentGraph,currentGraph+graphStart);
    clickedOnGraphSelect(currentGraph,currentGraph+graphSelectStart);
  }
}

void 
clickedOnGraphSelect (int i,int bKey)
{
  int strlength;
  
  switch (doit) {
  case pick2D:
    currentGraph = i;
    doPick(i,bKey);
    break;
  case drop2D:
    currentGraph = i;
    doDrop(i,bKey);
    break;
  case query2D:
    queriedGraph = i;
    makeMessageFromData(queriedGraph);
    writeControlMessage();
    /* reset indicator so that we're not in pick/drop/query mode anymore */
    doit = 0;
    break;
  default:
    graphStateArray[i].selected = !(graphStateArray[i].selected);
    if (graphStateArray[i].selected) {
      GSetForeground(graphGC,(float)graphBarSelectColor,Xoption);
      strcpy(control->buttonQueue[bKey].text,"^");
      strlength = strlen(control->buttonQueue[bKey].text);
    } else {
      GSetForeground(graphGC,(float)graphBarNotSelectColor,Xoption);
      strcpy(control->buttonQueue[bKey].text,"-");
      strlength = strlen(control->buttonQueue[bKey].text);
    }
    /* just to make sure the background is reset from clickedOnGraph */
    if (mono) {
      GSetForeground(graphGC,(float)foregroundColor,Xoption);
      GSetBackground(graphGC,(float)backgroundColor,Xoption);
    }
    
    GDrawImageString(graphGC,control->controlWindow,
                     control->buttonQueue[bKey].buttonX +
                     centerX(graphGC,
                             control->buttonQueue[bKey].text,strlength,
                             control->buttonQueue[bKey].buttonWidth),
                     control->buttonQueue[bKey].buttonY +
                     centerY(graphGC,
                             control->buttonQueue[bKey].buttonHeight),
                     control->buttonQueue[bKey].text,strlength,Xoption);
    GSetForeground(trashGC,(float)foregroundColor,Xoption);
    GSetLineAttributes(trashGC,2,LineSolid,CapButt,JoinMiter,Xoption);
    GDrawLine(trashGC,control->controlWindow,0,286,controlWidth,286,Xoption);
    break;
  } /* switch doit */
  
}


static void
drawControlPushButton(int isOn, int index)
{
  GDrawPushButton(dsply, processGC, processGC, processGC,
                  control->controlWindow,
                  (control->buttonQueue[index]).buttonX,
                  (control->buttonQueue[index]).buttonY,
                  (control->buttonQueue[index]).buttonWidth,
                  (control->buttonQueue[index]).buttonHeight,
                  isOn,
                  (control->buttonQueue[index]).text,
                  buttonColor,
                  monoColor((control->buttonQueue[index]).textColor), Xoption);
  XSync(dsply,False);
}



void 
buttonAction (int bKey)
{
  int i;
  
  switch (bKey) {
    
  case pick2D:
    if (viewAloned) {
      sprintf(control->message,"%s","Cannot pick without AXIOM!");
      writeControlMessage();
      XSync(dsply,False);
    }
    else {
      doit = pick2D;
      sprintf(control->message,"%s","Click on the graph to pick");
      writeControlMessage();
      XSync(dsply,False);
    }
    break;
    
  case drop2D:
    if (viewAloned) {
      sprintf(control->message,"%s","Cannot drop without AXIOM!");
      writeControlMessage();
      XSync(dsply,False);
    } else {
      doit = drop2D;
      sprintf(control->message,"%s","Click on the graph to drop");
      writeControlMessage();
      XSync(dsply,False);
    }
    break;
    
  case query2D:
    doit = query2D;
    sprintf(control->message,"%s","Click on the graph to query");
    writeControlMessage();
    XSync(dsply,False);
    break;
    
    
  case zoom2Dx:
    if (zoomXON)
      strcpy((control->buttonQueue[zoom2Dx]).text ,
             "X Off");
    else
      strcpy((control->buttonQueue[zoom2Dx]).text , 
             "X On ");
    zoomXON = !zoomXON;
    
    drawControlPushButton(zoomXON, zoom2Dx);
    XSync(dsply,False);
    break;
    
  case zoom2Dy:
    if (zoomYON) strcpy((control->buttonQueue[zoom2Dy]).text,
                        "Y Off");
    else strcpy((control->buttonQueue[zoom2Dy]).text ,
                "Y On ");
    zoomYON = !zoomYON;
    drawControlPushButton(zoomYON, zoom2Dy);
    XSync(dsply,False);
    break;
    
    
  case translate2Dx:
    if (transXON) strcpy((control->buttonQueue[translate2Dx]).text,
                         "X Off");
    else strcpy( (control->buttonQueue[translate2Dx]).text,
                 "X On ");
    transXON = !transXON;
    drawControlPushButton(transXON, translate2Dx);
    XSync(dsply,False);
    break;
    
  case translate2Dy:
    if (transYON) strcpy((control->buttonQueue[translate2Dy]).text,
                         "Y Off");
    else strcpy( (control->buttonQueue[translate2Dy]).text,
                 "Y On");
    transYON = !transYON;
    drawControlPushButton(transYON, translate2Dy);
    XSync(dsply,False);
    break;
    
  case pointsOnOff:
    if (pointsON) strcpy((control->buttonQueue[pointsOnOff]).text,
                         "Pts Off");
    else strcpy( (control->buttonQueue[pointsOnOff]).text,
                 "Pts On ");
    pointsON = !pointsON;
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].pointsOn = pointsON;
    
    drawControlPushButton(pointsON, pointsOnOff);
    drawViewport(Xoption);
    break;
    
  case connectOnOff:
    if (connectON) strcpy((control->buttonQueue[connectOnOff]).text,
                          "Lines Off");
    else strcpy( (control->buttonQueue[connectOnOff]).text,
                 "Lines On ");
    connectON = !connectON;
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].connectOn = connectON;
    
    drawControlPushButton(connectON, connectOnOff);
    drawViewport(Xoption);
    break;
    
  case spline2D:
    if (splineON) strcpy((control->buttonQueue[spline2D]).text,
                         "Box Off");
    else strcpy ((control->buttonQueue[spline2D]).text ,
                 "Box On ");
    splineON = !splineON;
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].splineOn = splineON;
    
    drawControlPushButton(splineON, spline2D);
    drawViewport(Xoption);
    break;
    
  case axesOnOff2D:
    if (axesON)
      strcpy ((control->buttonQueue[axesOnOff2D]).text,
              "Axes Off");
    else
      strcpy ((control->buttonQueue[axesOnOff2D]).text ,
              "Axes On ");
    axesON = !axesON;
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].axesOn = axesON;
    
    drawControlPushButton(axesON, axesOnOff2D);
    drawViewport(Xoption);
    break;
    
  case unitsOnOff2D:
    if (unitsON)
      strcpy( (control->buttonQueue[unitsOnOff2D]).text,
              "Units Off");
    else
      strcpy ((control->buttonQueue[unitsOnOff2D]).text,
              "Units On ");
    unitsON = !unitsON;
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].unitsOn = unitsON;
    
    drawControlPushButton(unitsON, unitsOnOff2D);
    drawViewport(Xoption);
    break;
    
  case ps2D:
    strcpy(control->message,"Creating postscript now ...");
    writeControlMessage();
    drawControlPushButton(1, ps2D);
    
    if (PSInit(viewport->viewWindow, viewport->titleWindow) == psError) {
      strcpy(control->message,"Aborted: PSInit error.");
      writeControlMessage();
      drawControlPushButton(0, ps2D);
      XSync(dsply,False);
      return;   /* make new temp name for new file */
    }
    
    drawViewport(PSoption);     /* draw picture in PS; create ps script file */
    
    if (PSCreateFile(viewBorderWidth, 
                     viewport->viewWindow,
                     viewport->titleWindow, 
                     viewport->title) == psError) {
      strcpy(control->message,"Aborted: PSCreateFile error.");
      writeControlMessage();
      drawControlPushButton(0, ps2D);
      XSync(dsply,False);
      return;
    }
    
    clearControlMessage();
    strcpy(control->message,PSfilename);
    strcat(control->message," in working dir ");
    writeControlMessage();
    drawControlPushButton(0, ps2D);
    XSync(dsply,False);
    break;
    
  case hideControl2D:
    if (viewport->haveControl) {
      viewport->haveControl = no;
      XUnmapWindow(dsply,control->controlWindow);
      XSync(dsply,False);
    }
    break;
    
  case reset2D:
    /* reset view */
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i] = graphStateBackupArray[i];
    
    unitsON = no;
    strcpy( (control->buttonQueue[unitsOnOff2D]).text,s  = "Units Off");
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].unitsOn = no;
    drawControlPushButton(unitsON, unitsOnOff2D);
    
    pointsON = yes;
    strcpy ((control->buttonQueue[pointsOnOff]).text ,"Pts On ");
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].pointsOn = yes;
    drawControlPushButton(pointsON, pointsOnOff);
    
    axesON = yes;
    strcpy ((control->buttonQueue[axesOnOff2D]).text,"Axes On ");
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].axesOn = yes;
    drawControlPushButton(axesON, axesOnOff2D);
    
    connectON = yes;
    strcpy((control->buttonQueue[connectOnOff]).text,"Lines On ");
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].connectOn = yes;
    drawControlPushButton(connectON, connectOnOff);
    
    splineON = no;
    strcpy( (control->buttonQueue[connectOnOff]).text ,"Box Off");
    for (i=0; i<maxGraphs; i++)
      if (graphStateArray[i].showing && graphStateArray[i].selected)
        graphStateArray[i].splineOn = splineON;
    drawControlPushButton(splineON, spline2D);
    drawViewport(Xoption);
    
    break;
    
  case closeAll2D:
    strcpy(control->message,"       Click again to confirm       ");
    writeControlMessage();
    drawControlPushButton(1, closeAll2D);
    XSync(dsply,False);
    viewport->closing = yes;
    break;
    
  case clear2D:
    for (i=0; i<maxGraphs; i++) graphStateArray[i].selected = 1;
    clickedOnGraphSelect(0,graphSelect1);
    clickedOnGraphSelect(1,graphSelect2);
    clickedOnGraphSelect(2,graphSelect3);
    clickedOnGraphSelect(3,graphSelect4);
    clickedOnGraphSelect(4,graphSelect5);
    clickedOnGraphSelect(5,graphSelect6);
    clickedOnGraphSelect(6,graphSelect7);
    clickedOnGraphSelect(7,graphSelect8);
    clickedOnGraphSelect(8,graphSelect9);
    XSync(dsply,False);
    break;
    
  case graph1:
  case graph2:
  case graph3:
  case graph4:
  case graph5:
  case graph6:
  case graph7:
  case graph8:
  case graph9:
    clickedOnGraph(bKey-graphStart,bKey);
    XSync(dsply,False);
    break;
    
  case graphSelect1:
  case graphSelect2:
  case graphSelect3:
  case graphSelect4:
  case graphSelect5:
  case graphSelect6:
  case graphSelect7:
  case graphSelect8:
  case graphSelect9:
    clickedOnGraphSelect(bKey-graphSelectStart,bKey);
    XSync(dsply,False);
    break;
    
  } /* switch (action) */
}

/*********************** X Event Processing ***************************/
void 
processEvents(void)
{
  
  XEvent                  *event,
    tempEvent;
  Window                  whichWindow;
  XWindowAttributes       graphWindowAttrib;
  buttonStruct            *controlButton;
  mouseCoord              mouseXY;
  int                     i,
    someInt,
    mouseW4,
    mouseH4,
    toggleReady, 
    gotToggle = no,
    checkButton = no,
    firstTime = yes,
    gotEvent = 0,
    buttonTablePtr,
    Xcon,
    len,
    externalControl;
  fd_set                  rd;
  
  
  externalControl=0;
  Xcon = ConnectionNumber(dsply);
  
  
  if (!(event = (XEvent *)malloc(sizeof(XEvent)))) {
    fprintf(stderr,"Ran out of memory initializing event processing.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  
  controlButton = control->buttonQueue;
  
  while(1) {
    
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
        len=select(FD_SETSIZE,(void *) &rd,0,0,0); 
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
        firstTime = no;
        
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
            /* get rid of redundant events */
            XCheckWindowEvent(dsply,
                              viewport->titleWindow,
                              ExposureMask,
                              &tempEvent);
            writeTitle();
            XGetWindowAttributes(dsply,
                                 whichWindow,
                                 &graphWindowAttrib);
            XResizeWindow(dsply,
                          viewport->viewWindow,
                          graphWindowAttrib.width,
                          graphWindowAttrib.height-titleHeight);
            XSync(dsply,False);
            break;
          } else if (whichWindow == viewport->viewWindow) {
            XCheckWindowEvent(dsply,
                              viewport->viewWindow,
                              ExposureMask,
                              &tempEvent);
            XGetWindowAttributes(dsply,
                                 viewport->titleWindow,
                                 &graphWindowAttrib);
            XResizeWindow(dsply,
                          viewport->viewWindow,
                          graphWindowAttrib.width,
                          graphWindowAttrib.height-titleHeight);
            drawViewport(Xoption);
            XMapWindow(dsply,whichWindow);
            XSync(dsply,False);
            break;
          } else {      /* it's gotta be the control panel */
            XGetWindowAttributes(dsply,
                                 control->controlWindow,
                                 &graphWindowAttrib);
            /* do not allow resizing of control panel */
            if ((graphWindowAttrib.width != controlWidth) ||
                (graphWindowAttrib.height != controlHeight)) {
              XResizeWindow(dsply,
                            control->controlWindow,
                            controlWidth,
                            controlHeight);
            }
            drawControlPanel();
            XSync(dsply,False);
            break;
          }
          break;
          
        case MotionNotify:
          if (followMouse) {
            while (XCheckMaskEvent(dsply,
                                   ButtonMotionMask,
                                   event));
            mouseXY = getPotValue(((XButtonEvent *)event)->x,
                                  ((XButtonEvent *)event)->y,
                                  controlButton->xHalf,
                                  controlButton->yHalf);
          }
          if (controlButton->pot) {
            gotToggle = no;
            checkButton = yes;
          }
          break;
          
        case ButtonRelease:
          if (followMouse==yes) {
            followMouse = no;
            toggleReady = yes;
            checkButton = no;
            drawViewport(Xoption);
          } else {
            followMouse = no;
            toggleReady = yes;
            checkButton = no;
          }
          break;
          
        case LeaveNotify:
          /*
            We still follow the mouse when we leave the pots.
            */
          /*      
                  followMouse = no;
                  toggleReady = yes; 
                  checkButton = no; 
                  */

          break;
          
        case ButtonPress:
          if (whichWindow == viewport->viewWindow) {
            
            /* mouse clicked on viewport */

            switch (((XButtonEvent *)event)->button) {
            case Button3:

              /* print out (x,y) object-space coordinates in message area */

              XGetWindowAttributes(dsply,whichWindow,&graphWindowAttrib);
              sprintf(viewport->controlPanel->message,
                      "       >%d<: [%6.2f,%6.2f]       ", 
                      queriedGraph+1,
                      projX((((XButtonEvent *)event)->x),
                            graphWindowAttrib.width,queriedGraph),
                      projY((((XButtonEvent *)event)->y),
                            graphWindowAttrib.height,queriedGraph));
              writeControlMessage();
              XFlush(dsply);
              break;
            default:

              /* Find where mouse is on the viewport => where to put the CP */

              XGetWindowAttributes(dsply,
                                   whichWindow,
                                   &graphWindowAttrib);
              mouseW4 = graphWindowAttrib.width/4;
              if (((XButtonEvent *)event)->x >
                  (graphWindowAttrib.width - mouseW4))
                someInt = 1;
              else {
                mouseH4 = graphWindowAttrib.height/4;
                if (((XButtonEvent *)event)->y >
                    (graphWindowAttrib.height - mouseH4)) 
                  someInt = 2;
                else if (((XButtonEvent *)event)->x < mouseW4) 
                  someInt = 3;
                else if (((XButtonEvent *)event)->y < mouseH4) 
                  someInt = 4;
                else someInt = 0;
              }
              if (viewport->haveControl) {
                XUnmapWindow(dsply,control->controlWindow);
              }
              putControlPanelSomewhere(someInt);
              XMapWindow(dsply,control->controlWindow);
              XSync(dsply,False);
              break;
            } /* switch on mouse button */
          } else if (whichWindow == control->colormapWindow) {

            /* mouse clicked on colormap */

            followMouse = yes;
            gotToggle   = no;
            checkButton = yes;
            firstTime   = yes;
          } else if (whichWindow != control->controlWindow) {

            /* mouse clicked on control window (not colormap) */

            if (controlButton->self != whichWindow) {
              buttonTablePtr = *((int *)XLookUpAssoc(dsply,table,whichWindow));
              controlButton = &(control->buttonQueue[buttonTablePtr]);
            }
            
            if (controlButton->pot) {
              /* figure out [x,y] for this button in the range [-1..1,-1..1] */
              mouseXY = getPotValue(((XButtonEvent *)event)->x,
                                    ((XButtonEvent *)event)->y,
                                    controlButton->xHalf,
                                    controlButton->yHalf);
              followMouse = yes;
              gotToggle = no;
            } else {
              followMouse = no;
              gotToggle = yes;  /* auto-repeat on toggle buttons not allowed */
              if (toggleReady) {
                toggleReady = no;
              }
            }
            checkButton = yes;
            firstTime   = yes;
          }
          break;
          
        } /* switch */
        gotEvent--;
      } /* if gotEvent */
      
      
      /* Allow repeat polling when mouse button clicked on a potentiometer. */

      if (followMouse && !firstTime && (followMouse++ > mouseWait)) {
        followMouse = yes;   /* reset for next timing loop */
        checkButton = yes;
      }
      if (checkButton) {
        if (viewport->closing && (controlButton->buttonKey == closeAll2D)) {
          goodbye(-1);
        } else {
          clearControlMessage();
          viewport->closing = no;
          drawControlPushButton(0, closeAll2D);
          if ((doit) &&
              ((controlButton->buttonKey < graphStart) &&
               (controlButton->buttonKey > (graphSelectStart + maxGraphs))))
            doit = 0;
          
          switch(controlButton->buttonKey) {
            
          case translate2D:
            for (i=0; i<maxGraphs; i++) {
              if (graphStateArray[i].showing && graphStateArray[i].selected) {
                if (transXON) {
                  graphStateArray[i].centerX -= mouseXY.x * 0.1;
                  if (graphStateArray[i].centerX > maxDelta)
                    graphStateArray[i].centerX = maxDelta;
                  else if (graphStateArray[i].centerX < -maxDelta)
                    graphStateArray[i].centerX = maxDelta;
                }
                if (transYON) {
                  graphStateArray[i].centerY -= mouseXY.y * 0.1;
                  if (graphStateArray[i].centerY > maxDelta)
                    graphStateArray[i].centerY = maxDelta;
                  else if (graphStateArray[i].centerY < -maxDelta)
                    graphStateArray[i].centerY = maxDelta;
                }
              } /* graph showing or selected */
            }  /* for graphs */
            drawViewport(Xoption);
            break;
            
          case scale2D:
            for (i=0; i<maxGraphs; i++) {
              if (graphStateArray[i].showing && graphStateArray[i].selected) {
                if (zoomXON) {
                  graphStateArray[i].scaleX *= (1 - mouseXY.y * 0.3);
                  if (graphStateArray[i].scaleX > maxScale)
                    graphStateArray[i].scaleX = maxScale;
                  else if (graphStateArray[i].scaleX < minScale)
                    graphStateArray[i].scaleX = minScale;
                }
                if (zoomYON) {
                  graphStateArray[i].scaleY *= (1 - mouseXY.y * 0.3);
                  if (graphStateArray[i].scaleY > maxScale)
                    graphStateArray[i].scaleY = maxScale;
                  else if (graphStateArray[i].scaleY < minScale)
                    graphStateArray[i].scaleY = minScale;
                }
              } /* graph showing or selected */
            }  /* for graphs */
            drawViewport(Xoption);
            break;
            
          default:
            buttonAction(controlButton->buttonKey);
          } /* switch on buttonKey */
        } /* else - not closing */
      } /* if checkButton */
    } /* if FD_ISSET(Xcon.... */
    else if (FD_ISSET(0,&rd)) {
      externalControl=spadAction(); /* returns (-1) if broken ,0 if success */
      if (spadDraw && (externalControl==0)) drawViewport(Xoption);
    }
  } /* while */
} /* processEvents() */






void 
clickedOnGraph (int i,int bKey)
{
  
  switch (doit) {
  case pick2D:
    currentGraph = queriedGraph = i;
    doPick(i,bKey);
    break;
  case drop2D:
    currentGraph = queriedGraph = i;
    doDrop(i,bKey);
    break;
  case query2D:
    queriedGraph = i;
    makeMessageFromData(queriedGraph);
    writeControlMessage();
    /* reset indicator so that we're not in pick/drop/query mode anymore */
    doit = 0;
    break;
  default:
    graphStateArray[i].showing = !(graphStateArray[i].showing);
    if (mono) {
      if (graphStateArray[i].showing) {
        GSetForeground(graphGC,(float)backgroundColor,Xoption);
        GSetBackground(graphGC,(float)foregroundColor,Xoption);
      } else {
        GSetForeground(graphGC,(float)foregroundColor,Xoption);
        GSetBackground(graphGC,(float)backgroundColor,Xoption);
      }
      GDrawImageString(graphGC,
                       control->controlWindow,
                       (control->buttonQueue[bKey]).buttonX +
                       centerX(graphGC,(control->buttonQueue[bKey]).text,1,
                               (control->buttonQueue[bKey]).buttonWidth),
                       (control->buttonQueue[bKey]).buttonY +
                       centerY(graphGC,(control->buttonQueue[bKey]).buttonHeight),
                       (control->buttonQueue[bKey]).text,
                       1,
                       Xoption);
    } else {
      if (graphStateArray[i].showing)
        GSetForeground(graphGC,(float)graphBarShowingColor,Xoption);
      else
        GSetForeground(graphGC,(float)graphBarHiddenColor,Xoption);
      GDrawString(graphGC,
                  control->controlWindow,
                  (control->buttonQueue[bKey]).buttonX +
                  centerX(graphGC,(control->buttonQueue[bKey]).text,1,
                          (control->buttonQueue[bKey]).buttonWidth),
                  (control->buttonQueue[bKey]).buttonY +
                  centerY(graphGC,(control->buttonQueue[bKey]).buttonHeight),
                  (control->buttonQueue[bKey]).text,1,Xoption);
    }
    drawViewport(Xoption);
    break;
  } /* switch doit */
  
}









