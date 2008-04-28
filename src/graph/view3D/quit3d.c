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

#define _QUIT3D_C
#include "openaxiom-c-macros.h"

#include <string.h>
#include "header.h"
#include "cpanel.h"
#include "volume.h"
#include "../include/purty/volume.bitmap"
#include "../include/purty/volume.mask"

#include "util.H1"
#include "Gfun.H1"
#include "XSpadFill.H1"
#include "all_3d.H1"

#define quitMASK ExposureMask
#define quitCursorForeground monoColor(55)
#define quitCursorBackground monoColor(197)
#define quitTitleColor monoColor(69)
#define quitButtonColor monoColor(195)
#define quitFontHeight (quitFont->max_bounds.ascent+quitFont->max_bounds.descent)


/***************************
 * int makeQuitPanel() *
 ***************************/

int
makeQuitPanel(void)
{

  int i;
  XSetWindowAttributes quitter,QuitterAttrib;
  XSizeHints sizeh;
  Pixmap quitbits, quitmask;
  XColor quitterColor,qColor;

  quitbits = XCreateBitmapFromData(dsply,rtWindow, volumeBitmap_bits,
                                   volumeBitmap_width,volumeBitmap_height);
  quitmask = XCreateBitmapFromData(dsply,rtWindow,volumeMask_bits,
                                   volumeMask_width,volumeMask_height);
  quitter.background_pixel = backgroundColor;
  quitter.border_pixel = foregroundColor;
  quitter.event_mask = quitMASK;
  quitter.colormap = colorMap;
  quitter.override_redirect = overrideManager;
  quitterColor.pixel = quitCursorForeground;
  XQueryColor(dsply,colorMap,&quitterColor);
  qColor.pixel = quitCursorBackground;
  XQueryColor(dsply,colorMap,&qColor);
  quitter.cursor = XCreatePixmapCursor(dsply,quitbits,quitmask,
                                       &quitterColor,&qColor,
                                       volumeBitmap_x_hot,volumeBitmap_y_hot);
  quitWindow = XCreateWindow(dsply,control->controlWindow,
                             controlWidth-quitWidth-2,controlHeight-quitHeight-2,
                             quitWidth-2,quitHeight-2,2,
                             CopyFromParent,InputOutput,CopyFromParent,
                             controlCreateMASK,&quitter);

  sizeh.flags  = USPosition | USSize;
  sizeh.x      = 0;
  sizeh.y      = 0;
  sizeh.width  = quitWidth-2;
  sizeh.height = quitHeight-2;

  XSetNormalHints(dsply,quitWindow,&sizeh);
  XSetStandardProperties(dsply,quitWindow,"Quit Panel","Quit Panel",
                         None,NULL,0,&sizeh);

  /*** do quit buttons ***/
  initQuitButtons(control->buttonQueue);
  for (i=quitButtonsStart; i<(quitButtonsEnd); i++) {
    QuitterAttrib.event_mask = (control->buttonQueue[i]).mask;
    (control->buttonQueue[i]).self =
                XCreateWindow(dsply,quitWindow,
                              (control->buttonQueue[i]).buttonX,
                              (control->buttonQueue[i]).buttonY,
                              (control->buttonQueue[i]).buttonWidth,
                              (control->buttonQueue[i]).buttonHeight,
                              0,0,InputOnly,CopyFromParent,
                              buttonCreateMASK,&QuitterAttrib);
    XMakeAssoc(dsply,table,(control->buttonQueue[i]).self,
               &((control->buttonQueue[i]).buttonKey));
    XMapWindow(dsply,(control->buttonQueue[i]).self);
  }

  return(0);

}  /* makeQuitPanel() */


/****************************
 * void drawQuitPanel() *
 ****************************/

void
drawQuitPanel(void)
{

  char *s;
  int i,strlength;

  s = "Really?";
  strlength = strlen(s);
  GSetForeground(anotherGC,(float)quitTitleColor,Xoption);
  GDrawString(anotherGC,quitWindow,
              centerX(anotherGC,s,strlength,quitWidth),centerY(anotherGC,39),s,strlength,Xoption);

  GSetForeground(anotherGC,(float)quitButtonColor,Xoption);
  for (i=quitButtonsStart; i<(quitButtonsEnd); i++) {
      GDraw3DButtonOut(quitGC,quitWindow,
                     (control->buttonQueue[i]).buttonX,
                     (control->buttonQueue[i]).buttonY,
                     (control->buttonQueue[i]).buttonWidth,
                     (control->buttonQueue[i]).buttonHeight,Xoption);
      s = (control->buttonQueue[i]).text;
      strlength = strlen(s);
      GSetForeground(trashGC,
                     (float)monoColor((control->buttonQueue[i]).textColor),Xoption);
      GDrawString(trashGC,quitWindow,
                  (control->buttonQueue[i]).buttonX +
                  centerX(processGC,s,strlength,
                          (control->buttonQueue[i]).buttonWidth),
                  (control->buttonQueue[i]).buttonY +
                  centerY(processGC,(control->buttonQueue[i]).buttonHeight),
                  s,strlen(s),Xoption);
  }  /* for i in control->buttonQueue */

}  /* drawQuitPanel */
