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

#define _SPADACTION2D_C
#include "openaxiom-c-macros.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "header2.h"

#include "all_2d.H1"
#include "util.H1"


/******************************
 * int readViewman(info,size) *
 ******************************/

int 
readViewman(void * info,int size)
{
  int mold = 0;

  sprintf(errorStr,"read of %d bytes from viewport manager\n", size);
  mold = check(read(0,info,size));   
  return(mold);

}

/********************
 * int spadAction() *
 ********************/
extern int viewAloned;
int 
spadAction(void)
{
  int viewCommand;
  float f1,f2;
  int i1,i2,i3,viewGoAhead;
  static int ack = 1;

  if (viewAloned==yes) {
      close(0);
      return(-1);
      }
  readViewman(&viewCommand,intSize);

  switch (viewCommand) {

  case hideControl2D:
    readViewman(&i1,intSize);
    if (i1) {                         /* show control panel */
      if (viewport->haveControl) XUnmapWindow(dsply,control->controlWindow);
      putControlPanelSomewhere(someInt);
    } else {    /* turn off control panel */
      if (viewport->haveControl) {
        viewport->haveControl = no;
        XUnmapWindow(dsply,control->controlWindow);
      }
    }
    break;

  case changeTitle:
    readViewman(&i1,intSize);
    readViewman(viewport->title,i1);
    viewport->title[i1] = '\0';
    writeTitle();
    writeControlTitle();
    XFlush(dsply);
    spadDraw=no;
    break;

  case writeView:
    readViewman(&i1,intSize);
    readViewman(filename,i1);
    filename[i1] = '\0';
    strcpy(errorStr,"writing of viewport data");
    i3 = 0;
    readViewman(&i2,intSize);
    while (i2) {
      i3 = i3 | (1<<i2);
      readViewman(&i2,intSize);
    }
    if (writeViewport(i3) < 0)
      fprintf(stderr,"          Nothing was written\n");
    break;

  case closeAll2D:
    check(write(Socket,&ack,intSize));
    goodbye(-1);

  case ps2D:
    readViewman(&i1,intSize);
    buttonAction(viewCommand);
    break;

  case axesOnOff2D:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].axesOn = i2;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case axesColor2D:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].axesColor = i2;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case unitsOnOff2D:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].unitsOn = i2;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case unitsColor2D:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].unitsColor = i2;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case connectOnOff:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].connectOn = i2; 
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case pointsOnOff:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].pointsOn = i2;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case spline2D:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    graphStateArray[i1].splineOn = i2;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;

  case showing2D:
    readViewman(&i1,intSize);
    i1--;
    readViewman(&i2,intSize);
    /* simulate a button press to turn display number on/off */
    graphStateArray[i1].showing = !i2;
    clickedOnGraph(i1,i1+graphStart);
    break;

  case scale2D:
    readViewman(&i1,intSize);
    i1--;   /* passed index is [1..9] but internal representation is [0..8] */
    readViewman(&f1,floatSize);
    readViewman(&f2,floatSize);
    graphStateArray[i1].scaleX = f1;
    graphStateArray[i1].scaleY = f2;
    if (graphStateArray[i1].scaleX > maxScale) 
      graphStateArray[i1].scaleX = maxScale;
    else
      if (graphStateArray[i1].scaleX < minScale) 
        graphStateArray[i1].scaleX = minScale;
    if (graphStateArray[i1].scaleY > maxScale) 
      graphStateArray[i1].scaleY = maxScale;
    else
      if (graphStateArray[i1].scaleY < minScale) 
        graphStateArray[i1].scaleY = minScale;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;   /* scale2D */


  case translate2D:
    readViewman(&i1,intSize);
    i1--;   /* passed index is [1..9] but internal representation is [0..8] */
    readViewman(&f1,floatSize);
    readViewman(&f2,floatSize);
    graphStateArray[i1].centerX = f1;
    graphStateArray[i1].centerY = f2;
    if (graphStateArray[i1].centerX > maxDelta) 
      graphStateArray[i1].centerX = maxDelta;
    else if (graphStateArray[i1].centerX < -maxDelta) 
           graphStateArray[i1].centerX = maxDelta;
    if (graphStateArray[i1].centerY > maxDelta) 
      graphStateArray[i1].centerY = maxDelta;
    else if (graphStateArray[i1].centerY < -maxDelta) 
           graphStateArray[i1].centerY = maxDelta;
    if (graphStateArray[i1].showing) spadDraw=yes;
    break;   /* translate2D */

  case moveViewport:
    readViewman(&i1,intSize);
    readViewman(&i2,intSize);
    XMoveWindow(dsply,viewport->titleWindow,i1,i2);
    XSync(dsply,False);
    break;

 case resizeViewport:
    readViewman(&i1,intSize);
    readViewman(&i2,intSize);
    XResizeWindow(dsply,viewport->titleWindow,i1,i2+titleHeight); 
    XResizeWindow(dsply,viewport->viewWindow,i1,i2);
    spadDraw=yes;
    break;

  case putGraph:
    readViewman(&i1,intSize);           /* key of graph to get */
    readViewman(&i2,intSize);           /* slot to drop graph onto 0..8*/
    readViewman(&viewGoAhead,intSize);
    if (viewGoAhead < 0) {
      sprintf(control->message,"Couldn't put into graph %d", i2+1);
      writeControlMessage();
    } else {
      sprintf(control->message,"Dropped onto graph %d", i2+1);
      writeControlMessage();
      freeGraph(i2);
      graphArray[i2].key = i1;
      getGraphFromViewman(i2);
      /* simulate a button press to turn display number on and select on */
      /* need !yes since it will be inverted */
      graphStateArray[i2].selected = no;  
      graphStateArray[i2].connectOn = yes;  
      graphStateArray[i2].showing = !(graphStateArray[i2].showing); 
      clickedOnGraph(i2,i2+graphStart);
      clickedOnGraphSelect(i2,i2+graphSelectStart);
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
  check(write(Socket,&ack,intSize));
  return(0);

}

