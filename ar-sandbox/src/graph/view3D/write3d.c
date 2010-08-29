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

#define _WRITE3D_C
#include "openaxiom-c-macros.h"

#include <stdio.h>
#include <stdlib.h>

#include "header.h"
#include "write.h"
#include "mode.h"


#include "pixmap.h"
#include "XShade.H1"
#include "Gfun.H1"

#include "all_3d.H1"

  /* upper limit as to how many kinds of files could be written (numBits-1) */
#define numBits (8*sizeof(int))
#define StellarColors 9

int 
writeViewport (int thingsToWrite)
{

  int               i, j, k, ii, code, *anIndex;
  LLPoint           *anLLPoint;
  LPoint            *anLPoint;
  viewTriple        *aPt;
  XWindowAttributes vwInfo;
  FILE              *viewDataFile;
  char              viewDirName[80], viewDataFilename[80],
                    viewBitmapFilename[80], viewPixmapFilename[80],
                    command[80];

  XGetWindowAttributes(dsply,viewport->titleWindow,&vwInfo);
  sprintf(viewDirName,"%s%s",filename,".VIEW"); 
  sprintf(command,"%s%s%s","rm -r ",viewDirName," >  /dev/null 2>&1");
  code = system(command);
  sprintf(command,"%s%s%s","mkdir ",viewDirName," > /dev/null 2>&1");
  system(command);
  if (0) {
    fprintf(stderr,"   Error: Cannot create %s\n",viewDirName);
    return(-1);
  } else {

            /*** Create the data file ***/
    sprintf(viewDataFilename,"%s%s",viewDirName,"/data");
    if ((viewDataFile = fopen(viewDataFilename,"w")) == NULL) {
      fprintf(stderr,"   Error: Cannot create %s\n",viewDataFilename);
      perror("fopen");
      return(-1);
    } else {
              /*** write out the view3DStruct stuff ***/
      fprintf(viewDataFile,"%d\n",viewData.typeOf3D);
      fprintf(viewDataFile,"%g %g %g %g %g %g\n",
              viewData.xmin,viewData.xmax,viewData.ymin,viewData.ymax,
              viewData.zmin,viewData.zmax);
      fprintf(viewDataFile,"%s\n",viewport->title);
      fprintf(viewDataFile,"%g %g %g %g %g %g %g %g\n",viewport->deltaX,
              viewport->deltaY,viewport->scale,
              viewport->scaleX,viewport->scaleY,viewport->scaleZ,
              viewport->theta,viewport->phi);
      fprintf(viewDataFile,"%d %d %d %d\n",vwInfo.x,vwInfo.y,vwInfo.width,
              vwInfo.height);
      fprintf(viewDataFile,"%d %d %d %d %d %d %d\n",viewport->haveControl,
              viewData.style, viewport->axesOn,
              viewport->hueOffset,viewport->numberOfHues,
              viewport->diagonals, viewData.outlineRenderOn);
      fprintf(viewDataFile,"%g %g %g %g\n",viewport->lightVector[0],
              viewport->lightVector[1], viewport->lightVector[2],
              viewport->translucency);
      fprintf(viewDataFile,"%d %g\n",viewData.perspective,
              viewData.eyeDistance);

      /* write out the generalized 3D components */
      fprintf(viewDataFile,"%d\n",viewData.numOfPoints);
      for (i=0; i<viewData.numOfPoints; i++) {
        aPt = refPt3D(viewData,i);
        fprintf(viewDataFile,"%g %g %g %g\n",aPt->x, aPt->y, aPt->z, aPt->c);
      }
      fprintf(viewDataFile,"%d\n",viewData.lllp.numOfComponents);
      anLLPoint = viewData.lllp.llp;
      for (i=0; i<viewData.lllp.numOfComponents; i++,anLLPoint++) {
        fprintf(viewDataFile,"%d %d\n",anLLPoint->prop.closed,
                anLLPoint->prop.solid);
        fprintf(viewDataFile,"%d\n",anLLPoint->numOfLists);
        anLPoint = anLLPoint->lp;
        for (j=0; j<anLLPoint->numOfLists; j++,anLPoint++) {
          fprintf(viewDataFile,"%d %d\n",anLPoint->prop.closed,
                  anLPoint->prop.solid);
          fprintf(viewDataFile,"%d\n",anLPoint->numOfPoints);
          anIndex = anLPoint->indices;
          for (k=0; k<anLPoint->numOfPoints; k++,anIndex++) {
            fprintf(viewDataFile,"%d\n",*anIndex);
          } /* for points in LPoints (k) */
        } /* for LPoints in LLPoints (j) */
      } /* for LLPoints in LLLPoints (i) */
      fclose(viewDataFile);
    }  /* else was able to open file under the given filename */

           /* write out special files */
    for (ii=1; ii<numBits; ii++) {   /* write.h is one-based */
      if (thingsToWrite & (1<<ii)) {
        switch (ii) {
        case Bitmap:
            /*** Create the pixmap (bitmaps need leaf name) ***/
          sprintf(viewBitmapFilename,"%s%s%s",viewDirName,"/","image.bm");
          XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
          code = XWriteBitmapFile(dsply,viewBitmapFilename,
                                  viewport->titleWindow,vwInfo.width,
                                  vwInfo.height+vwInfo.border_width+20,-1,-1);
          break;

        case Pixmap:
            /*** Create the pixmap (bitmaps need leaf name) ***/
          sprintf(viewPixmapFilename,"%s%s%s",viewDirName,"/","image.xpm");
          XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
          write_pixmap_file(dsply,scrn,viewPixmapFilename,
                                   viewport->titleWindow,0,0,vwInfo.width,
                                   vwInfo.height+titleHeight);
          break;

        case Image:
            /*** Create the image (bitmaps need leaf name) ***/
          writeImage = yes;
          sprintf(viewPixmapFilename,"%s%s%s",viewDirName,"/","image.xpm");
          XResizeWindow(dsply,viewport->titleWindow,300,300+titleHeight);
          XResizeWindow(dsply,viewport->viewWindow,300,300);
          viewport->hueTop = totalHues-1;  viewport->hueOffset = 0;
          viewport->numberOfHues = viewport->hueTop - viewport->hueOffset;
          firstTime = 1;
          if (viewData.style == transparent) {
            viewData.style = render;
            viewData.outlineRenderOn = 1;
          } else {
            if (viewData.style == render) viewData.outlineRenderOn = 1;
          }
          drawViewport(Xoption);
          writeTitle();
          XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
          write_pixmap_file(dsply,scrn,viewPixmapFilename,
                                   viewport->titleWindow,0,0,vwInfo.width,
                                   vwInfo.height+titleHeight);
          viewport->monoOn = 1;
          maxGreyShade = XInitShades(dsply,scrn);
          firstTime = 1;
          drawViewport(Xoption);
          writeTitle();
          sprintf(viewBitmapFilename,"%s%s%s",viewDirName,"/","image.bm");
          code = XWriteBitmapFile(dsply,viewBitmapFilename,
                                  viewport->titleWindow,vwInfo.width,
                                  vwInfo.height+vwInfo.border_width+20,-1,-1);
                                  
          writeImage = no;
          break;

        case Postscript:
            /*** Create postscript output for viewport (in axiom3D.ps) ***/
         sprintf(PSfilename,"%s%s",viewDirName,"/axiom3D.ps");
         if (PSInit(viewport->viewWindow,viewport->titleWindow) == psError)
           return(-1);
         drawViewport(PSoption);  /* write new script file in /tmp */
         if (PSCreateFile(viewBorderWidth,viewport->viewWindow,
                          viewport->titleWindow, viewport->title) == psError)
           return(-1);  /* concat script & proc into axiom3D.ps */
         break;
        } /* switch on ii */
      }  /* if thingsToWrite >> ii */
    }  /* for ii */

    return(0);
  }    /* else create directory okay */

}
