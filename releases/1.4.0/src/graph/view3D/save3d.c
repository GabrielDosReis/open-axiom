/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2010, Gabriel Dos Reis.
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

#define _SAVE3D_C
#include "openaxiom-c-macros.h"

#include <stdio.h>
#include <string.h>
#include "header.h"
#include "cpanel.h"
#include "volume.h"
#include "../include/purty/volume.bitmap"
#include "../include/purty/volume.mask"

#include "Gfun.H1"
#include "XSpadFill.h"
#include "all_3d.H1"

#define saveMASK ExposureMask
#define saveCursorForeground monoColor(55)
#define saveCursorBackground monoColor(197)
#define saveTitleColor monoColor(70)
#define saveButtonColor monoColor(195)
#define saveFontHeight (saveFont->max_bounds.ascent+saveFont->max_bounds.descent)


/***************************
 * int makeSavePanel() *
 ***************************/

int
makeSavePanel(void)
{

  int i;
  XSetWindowAttributes saver,SaverAttrib;
  XSizeHints sizeh;
  Pixmap savebits, savemask;
  XColor saveColor,sColor;

  savebits = XCreateBitmapFromData(dsply,rtWindow, volumeBitmap_bits,
                                   volumeBitmap_width,volumeBitmap_height);
  savemask = XCreateBitmapFromData(dsply,rtWindow,volumeMask_bits,
                                   volumeMask_width,volumeMask_height);
  saver.background_pixel = backgroundColor;
  saver.border_pixel = foregroundColor;
  saver.event_mask = saveMASK;
  saver.colormap = colorMap;
  saver.override_redirect = overrideManager;
  saveColor.pixel = saveCursorForeground;
  XQueryColor(dsply,colorMap,&saveColor);
  sColor.pixel = saveCursorBackground;
  XQueryColor(dsply,colorMap,&sColor);
  saver.cursor = XCreatePixmapCursor(dsply,savebits,savemask,
                                     &saveColor,&sColor,
                                     volumeBitmap_x_hot,volumeBitmap_y_hot);

  saveWindow = XCreateWindow(dsply,control->controlWindow,
                             controlWidth-saveWidth-2, controlHeight-saveHeight-2,
                             saveWidth-2,saveHeight-2,2,
                             CopyFromParent,InputOutput,CopyFromParent,
                             controlCreateMASK,&saver);

  sizeh.flags  = USPosition | USSize;
  sizeh.x      = 0;
  sizeh.y      = 0;
  sizeh.width  = saveWidth-2;
  sizeh.height = saveHeight-2;

  XSetNormalHints(dsply,saveWindow,&sizeh);
  XSetStandardProperties(dsply,saveWindow,"Save Panel","Save Panel",
                         None,NULL,0,&sizeh);

  /*** Create save buttons ***/
  initSaveButtons(control->buttonQueue);
  for (i=saveButtonsStart; i<(saveButtonsEnd); i++) {
    SaverAttrib.event_mask = (control->buttonQueue[i]).mask;
    (control->buttonQueue[i]).self =
                XCreateWindow(dsply,saveWindow,
                              (control->buttonQueue[i]).buttonX,
                              (control->buttonQueue[i]).buttonY,
                              (control->buttonQueue[i]).buttonWidth,
                              (control->buttonQueue[i]).buttonHeight,
                              0,0,InputOnly,CopyFromParent,
                              buttonCreateMASK,&SaverAttrib);
    XMakeAssoc(dsply,table,(control->buttonQueue[i]).self,
               &((control->buttonQueue[i]).buttonKey));
    XMapWindow(dsply,(control->buttonQueue[i]).self);
  }

  return(0);

}  /* makeSavePanel() */


/****************************
 * void drawSavePanel() *
 ****************************/

void
drawSavePanel(void)
{

  char *s;
  int i,strlength;

  GSetForeground(saveGC,(float)saveButtonColor,Xoption);
  for (i=saveButtonsStart; i<(saveButtonsEnd); i++) {
      GDraw3DButtonOut(saveGC,saveWindow,
                     (control->buttonQueue[i]).buttonX,
                     (control->buttonQueue[i]).buttonY,
                     (control->buttonQueue[i]).buttonWidth,
                     (control->buttonQueue[i]).buttonHeight,Xoption);
      s = (control->buttonQueue[i]).text;
      strlength = strlen(s);
      GSetForeground(trashGC,
                     (float)monoColor((control->buttonQueue[i]).textColor),Xoption);
      GDrawString(trashGC,saveWindow,
                  (control->buttonQueue[i]).buttonX +
                  centerX(processGC,s,strlength,
                          (control->buttonQueue[i]).buttonWidth),
                  (control->buttonQueue[i]).buttonY +
                  centerY(processGC,(control->buttonQueue[i]).buttonHeight),
                  s,strlen(s),Xoption);
  }  /* for i in control->buttonQueue */

}  /* drawSavePanel */
