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


#define _GFUN_C

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "Gdraws0.h"
#include "G.h"
#include "hash.h"

#include "Gfun.H1"
#include "cfuns.h"

using namespace OpenAxiom;


/*
 * Given 2 file pointers, this function copies file ifp to file ofp
 */

static void
filecopy(FILE * ifp, FILE * ofp)
{

  int c;

  while ((c = getc(ifp)) != EOF)
    putc(c, ofp);
}


/*
 * PSCreateFile generates the output file by using the order of defined
 * variables; they are used to create the OUTPUT file.  Essentially,
 * PSCreateFile() loop through the index of 0 to psDrawNo and checks if the
 * file/procedure is used.      If so, the file is included into the output
 * file.
 */

int
PSCreateFile(
             int bWidth,            /* border width of picture frame */
             Window vw, Window tw,  /* viewWindow, and titleWindow */
             char *title)           /* title of picture to be drawn in title bar */
{
  FILE *ifp, *ofp, *fp;       /* input, output and temp file pointer */
  int i;                      /* index */

  /* last things to add to the script file */

  fp = fopen(psData[scriptps].filename, "a");
  fprintf(fp, "\n    grestore\t%% restore graphics state\n\n");
  fclose(fp);

#if 0
  /* Make frame drawing optional. */

  Gdraws_drawFrame(bWidth, vw, tw, title);
#endif

  /* put procedures and script together into OUTPUT.ps */

  if ((ofp = fopen(psData[output].filename, "w")) == NULL) {
    fprintf(stderr, "Cannot open %s to write.\n", psData[output].filename);
    return (psError);
  }
  else {
    i = 1;
    while (i < psDrawNo) {  /* loops through each file/procedure */
      if (psData[i].flag) { /* if set, procedure/file is used */
        if ((ifp = fopen(psData[i].filename, "r")) == NULL) {
          if (i == GCdictps) {  /* GC dictionaries */
            fprintf(stderr, "Warning: missing GCdictionary.\n");
          }
          else {
            fprintf(stderr, "Cannot open %s to read.\n",
                    psData[i].filename);
            fclose(ofp);
            return (psError);
          }
        }
        else {
          filecopy(ifp, ofp);
          fclose(ifp);
        }
      }
      i++;
    }
  }

  /* remove script file in tmp */

  unlink(psData[scriptps].filename);

#if 0
  /* remove GCdict file in tmp */
  unlink(psData[GCdictps].filename);
#endif

  return (fclose(ofp));
}



/*
 * This function draws the frame of the picture, which corresponds to the
 * picture frame on the X display.  In addition, it draws the title window as
 * well as the title of the picture.
 */

int
Gdraws_drawFrame(
          int borderW,          /* border width */
          Window viewWindow, Window titleWindow,
          char *title)          /* title of picture */
{
  FILE *fp;
  XWindowAttributes vwInfo, twInfo;

  /* choose 2 and "frameDict" for frame dictionary: can be anything else */

  PSCreateContext((GC)2, "frameDict", borderW, psButtCap, psMiterJoin,
                  psWhite, psBlack);

  fp = fopen(psData[scriptps].filename, "a");

  XGetWindowAttributes(dsply, viewWindow, &vwInfo);

  /* draw title window */

  XGetWindowAttributes(dsply, titleWindow, &twInfo);
  fprintf(fp, "\t%s\t%d\t%d\t%d\t%d\ttitle\n", "frameDict",
          twInfo.height - vwInfo.height, twInfo.width, 0, vwInfo.height);

  /* draw viewport window */

  fprintf(fp, "\t%s\tdrawFrame\n", "frameDict");      /* using Gdraws_setDimension() */

  /* draw title text */

  psData[drawIstrps].flag = yes;
  fprintf(fp, "\t%s\tloadFont\n\t%d\t(%s) stringwidth pop sub 2 div\n",
          "frameDict", twInfo.width, title);
  fprintf(fp, "\t%d\t(%s)\t(%s)\tpsDrawIStr\n", 15, title, "title");

  return (fclose(fp));
}


/* setDimension sets the dimension of the picture */

int
Gdraws_setDimension(
                    Window viewWindow, 
                    Window titleWindow)
{
  FILE *fp;
  XWindowAttributes vwInfo, twInfo;
  float pageWidth, pageHeight, width;

  fp = fopen(psData[scriptps].filename, "w");

  XGetWindowAttributes(dsply, titleWindow, &twInfo);
  XGetWindowAttributes(dsply, viewWindow, &vwInfo);
  pageWidth = 575.0;
  pageHeight = 750.0;

#if 0
  pageWidth = (float) (DisplayWidth(dsply, scrn) / DisplayWidthMM(dsply, scrn));
  pageWidth *= 160.0;
  pageHeight = (float) (DisplayHeight(dsply, scrn) / DisplayHeightMM(dsply, scrn));
  pageHeight *= 210.0;
  fprintf(stderr, "%f, %f\n", pageWidth, pageHeight);
#endif

  fprintf(fp, "\n    gsave\t%% save graphics state for clipping path\n\n");
  if ((vwInfo.height > pageWidth) || (vwInfo.height > pageHeight)) {
    width = (float) vwInfo.width;
    if (vwInfo.height > pageWidth) {
      width = pageWidth / width;
      fprintf(fp, "\t%f\t%f", width, width);
    }
    else {
      if (vwInfo.height > pageHeight)
        fprintf(fp, "\t%f\t%f", width, pageHeight / width);
    }
  }
  else {
    fprintf(fp, "\t%f\t%f", 1.0, 1.0);
  }
  fprintf(fp, "\tscale\n\n");

  fprintf(fp, "\t%d\t%d\t%d\tsetDim\n", twInfo.height - vwInfo.height,
          vwInfo.height, vwInfo.width);

  /* Write a Bounding Box for psfig etc. */

  fprintf(fp, "%%%%BoundingBox: 0 0 %d %d\n", vwInfo.height, vwInfo.width);

  fprintf(fp, "\tmaxX maxY\t0 0\trectangle\tclip\t%% set clip path\n\n");
  return (fclose(fp));
}
/*
 * GDrawImageString draws an image text string 
 */

int
GDrawImageString(
                 GC gc,         /* graphics context */
                 Window wid,    /* window id */
                 int x, int y,
                 char *string,
                 int length, int dFlag)
{
  int s;

  switch (dFlag) {
  case Xoption:
    s = XDrawImageString(dsply, wid, gc, x, y, string, length);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawImageString cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawIstrps].flag = yes;      /* set procedure flag */
      fprintf(fp, "\t%s\t%d\t%d\t(%s)\t(%s)\tpsDrawIStr\n",
              PSfindGC(gc), x, y, string, "window");
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "GdrawImagestring request (%d) not implemented yet.\n", dFlag);
    return (psError);
  }
  return (s);
}

/* Draws an arc; see XDrawArc */

int
GDrawArc(
         GC gc,                 /* graphics context */
         Window wid,            /* window id */
         int x, int y,
         unsigned int wdth, unsigned int hght,
         int ang1, int ang2, int dFlag)
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XDrawArc(dsply, wid, gc, x, y, wdth, hght, ang1, ang2);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawArc cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawarcps].flag = yes;
      fprintf(fp, "\t%s\t%d\t%d\t%d\t%d\t%d\t%d\tpsDrawArc\n",
              PSfindGC(gc), x, y, hght, wdth, ang1 / 64, ang2 / 64);
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "Gdrawarc request (%d) not implemented yet.\n",
            dFlag);
    return (psError);
  }
  return (s);
}
/*
 * GDrawLine draws a line, see XDrawLine 
 */

int
GDrawLine(
          GC gc,                /* graphics context */
          Window wid,           /* window id */
          int x0, int y0, int x1, int y1, int dFlag)
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XDrawLine(dsply, wid, gc, x0, y0, x1, y1);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawLine cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawlineps].flag = yes;      /* sets procedure flag */
      fprintf(fp, "\t%s\t%d\t%d\t%d\t%d\tpsDrawLine\n",
              PSfindGC(gc), x1, y1, x0, y0);
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "Gdrawline request (%d) not implemented yet.\n",
            dFlag);
    return (psError);
  }
  return (s);
}





/*
 * GDrawLines draws lines; see XDrawLines
 */

int
GDrawLines(
           GC gc,               /* graphics context */
           Window wid,          /* window id */
           XPoint * points,     /* points */
           int numberOfPoints, int mode, int dFlag)
                                /* number of points, mode and display flag */
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XDrawLines(dsply, wid, gc, points, numberOfPoints, mode);
    break;
  case PSoption:
    {
      FILE *fp;           /* not dealing with modes yet */
      int i = 0;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawLines cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawlinesps].flag = yes;     /* set procedure flag */
      fprintf(fp, "\t%s\n", PSfindGC(gc));
      i = numberOfPoints - 1;
      while (i > 0)
      { fprintf(fp, "\t%d\t%d\n", points[i].x, points[i].y);
        i = i-1;
      }

      fprintf(fp, "\t%d\t%d\t%d\tpsDrawLines\n",
              numberOfPoints, points[i].x, points[i].y);
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "Gdrawlines request (%d) not implemented yet\n",
            dFlag);
    return (psError);
  }
  return (s);
}

/*
 * GDrawPoint draws a point, see XDrawPoint 
 */

int
GDrawPoint(
           Window wid,          /* window id */
           GC gc,               /* graphics context */
           int x0, int y0, int dFlag)
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XDrawPoint(dsply, wid, gc, x0, y0);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawPoint cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawpointps].flag = yes;     /* sets procedure flag */
      fprintf(fp, "\t%s\t%d\t%d\t%d\t%d\tpsDrawPoint\n",
              PSfindGC(gc), x0, y0, x0 + 1, y0 + 1);
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "Gdrawpoint request (%d) not implemented yet\n",
            dFlag);
    return (psError);
  }
  return (s);
}

/*
 * GDrawRectangle draws a rectangle; see XDrawRectangle
 */

int
GDrawRectangle(
               GC gc,
               Window windowId,
               short int x,short  int y,short  int width,short  int height,
               int dFlag)
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XDrawRectangle(dsply, windowId, gc, x, y, width, height);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawRect cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawrectps].flag = yes;
      fprintf(fp, "\t%s\t%d\t%d\t%d\t%d\tpsDrawRect\n",
              PSfindGC(gc), width, height, x, y);
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "Gdrawrect request (%d) not implemented yet\n",
            dFlag);
    return (psError);
  }
  return (s);
}

/*
 * GDraw3DButtonOut draws a rectangle with 3D shading on rhs and bottom
 */

int
GDraw3DButtonOut(
                 GC gc,
                 Window windowId,
                 short int x,short  int y,short  int width,short  int height,
                 int dFlag)
{
  /* draw regular rectangle */

  int s = GDrawRectangle(gc, windowId, x, y, width - 1, height - 1, dFlag);

  /* add extra line down rhs */

  GDrawLine(gc, windowId,
            x + width, y + 1, x + width, y + height,
            dFlag);

  /* add extra line across bottom */

  GDrawLine(gc, windowId,
            x + 1, y + height, x + width, y + height,
            dFlag);

  return (s);
}

/*
 * GDraw3DButtonIn draws a rectangle with 3D shading on lhs and top
 */

int
GDraw3DButtonIn(
                GC gc,
                Window windowId,
                short int x,short  int y,short  int width,short  int height,
                int dFlag)
{
  /* draw regular rectangle */

  int s = GDrawRectangle(gc, windowId, x + 1, y + 1, width - 1, height - 1, dFlag);

  /* add extra line down lhs */

  GDrawLine(gc, windowId,
            x, y, x, y + height - 1,
            dFlag);

  /* add extra line across top */

  GDrawLine(gc, windowId,
            x, y, x + width - 1, y,
            dFlag);

  return (s);
}

/*
 * GDrawPushButton draws a push button whose appearance depends on "isOn."
 */

int
GDrawPushButton(
                Display * display,
                GC gc1, GC gc2, GC gc3,
                Window windowId,
                short int x,short int y,short  int width,short  int height,
                int isOn, char *text,
                unsigned long buttonColor, unsigned long color,
                int dFlag)
{
  int len = strlen(text);

  if (dFlag == Xoption)
    XClearArea(display, windowId, x, y, width + 1, height + 1, False);

  GSetForeground(gc1, (float) buttonColor, dFlag);

  if (isOn)
    GDraw3DButtonIn(gc1, windowId, x, y, width, height, dFlag);
  else
    GDraw3DButtonOut(gc1, windowId, x, y, width, height, dFlag);

  GSetForeground(gc2, (float) color, dFlag);

  return GDrawString(gc2, windowId,
                     x + (isOn ? 2 : 0) + centerX(gc3, text, len, width),
                     y + (isOn ? 2 : 0) + centerY(gc3, height),
                     text, len, dFlag);
}



/*
 * Draws a string; see XDrawString
 */

int
GDrawString(
            GC gc,              /* graphics context */
            Window wid,         /* window id */
            int x, int y,
            char *string,       /* string to be drawn */
            int length, int dFlag)
{
  int s;

  switch (dFlag) {
  case Xoption:
    s = XDrawString(dsply, wid, gc, x, y, string, length);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GDrawString cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[drawstrps].flag = yes;       /* sets procedure flag */
      fprintf(fp, "\t%s\t(%s)\t%d\t%d\tpsDrawStr\n",
              PSfindGC(gc), string, x, y);

      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "Gdrawstring request (%d) not implemented yet\n",
            dFlag);
    return (psError);
  }
  return (s);
}

/*
 * Draws and fills an arc with foreground color; see XFillArc
 */

int
GFillArc(
         GC gc,                 /* graphics context */
         Window wid,            /* window id */
         int x, int y,
         unsigned int wdth, unsigned int hght,
         int ang1, int ang2, int dFlag)
{
  int s = 0;

  switch (dFlag) {
  case Xoption:                   /* angle: times 64 already */
    XFillArc(dsply, wid, gc, x, y, wdth, hght, ang1, ang2);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GFillArc cannot open %s\n",
                psData[scriptps].filename);
        return (psError);
      }

      psData[fillarcps].flag = yes;       /* sets procedure flag */
      fprintf(fp, "\t%s\t%d %d\t%d %d\t%d %d\t%d %d\tpsFillArc\n",
              PSfindGC(gc), x, y, hght, wdth, ang1 / 64, ang2 / 64,
              x + wdth / 2, y + hght / 2);
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "GFillArc request (%d) not implemented yet\n",
            dFlag);
    return (psError);
  }
  return (s);
}

/*
 * Initializes the path and files to be used.
 */

int
PSGlobalInit(void)
{                               /* This needs to be called only once each
                                 * session. */
  char *tmp;

  /* path-independent global file name */
  psData[GCdictps].flag = yes;
  tmp = tempnam(NULL, "axPS");
  sprintf(psData[GCdictps].filename, "%s", tmp);
  free(tmp);
  psData[setupps].flag = yes;
  psData[scriptps].flag = yes;/* new script file name */
  psData[endps].flag = yes;

  /* path specific file names */

  if ((envAXIOM = oa_getenv("DEVE")) != NULL) {  /* get env var AXIOM */

    psData[headerps].flag = yes;
    sprintf(psData[headerps].filename, "%s%s", envAXIOM, "/Gdraws/PS/header.ps");
    sprintf(psData[drawps].filename, "%s%s", envAXIOM, "/Gdraws/PS/draw.ps");
    sprintf(psData[drawarcps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawarc.ps");
    sprintf(psData[drawfilledps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drwfilled.ps");
    sprintf(psData[drawcolorps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawcolor.ps");
    sprintf(psData[fillpolyps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/fillpoly.ps");
    sprintf(psData[colorpolyps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/colorpoly.ps");
    sprintf(psData[fillwolps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/fillwol.ps");
    sprintf(psData[colorwolps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/colorwol.ps");
    sprintf(psData[drawpointps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawpoint.ps");
    sprintf(psData[drawlineps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawline.ps");
    sprintf(psData[drawlinesps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawlines.ps");
    sprintf(psData[drawrectps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawrect.ps");
    sprintf(psData[drawstrps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawstr.ps");
    sprintf(psData[drawIstrps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/drawIstr.ps");
    sprintf(psData[fillarcps].filename, "%s%s", envAXIOM,
            "/Gdraws/PS/fillarc.ps");
    sprintf(psData[setupps].filename, "%s%s", envAXIOM, "/Gdraws/PS/setup.ps");
    sprintf(psData[endps].filename, "%s%s", envAXIOM, "/Gdraws/PS/end.ps");
  }
  else if ((envAXIOM = oa_getenv("AXIOM")) != NULL) {
    psData[headerps].flag = yes;
    sprintf(psData[headerps].filename, "%s%s", envAXIOM,
            "/lib/graph/header.ps");
    sprintf(psData[drawps].filename, "%s%s", envAXIOM,
            "/lib/graph/draw.ps");
    sprintf(psData[drawarcps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawarc.ps");
    sprintf(psData[drawfilledps].filename, "%s%s", envAXIOM,
            "/lib/graph/drwfilled.ps");
    sprintf(psData[drawcolorps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawcolor.ps");
    sprintf(psData[fillpolyps].filename, "%s%s", envAXIOM,
            "/lib/graph/fillpoly.ps");
    sprintf(psData[colorpolyps].filename, "%s%s", envAXIOM,
            "/lib/graph/colorpoly.ps");
    sprintf(psData[fillwolps].filename, "%s%s", envAXIOM,
            "/lib/graph/fillwol.ps");
    sprintf(psData[colorwolps].filename, "%s%s", envAXIOM,
            "/lib/graph/colorwol.ps");
    sprintf(psData[drawpointps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawpoint.ps");
    sprintf(psData[drawlineps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawline.ps");
    sprintf(psData[drawlinesps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawlines.ps");
    sprintf(psData[drawrectps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawrect.ps");
    sprintf(psData[drawstrps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawstr.ps");
    sprintf(psData[drawIstrps].filename, "%s%s", envAXIOM,
            "/lib/graph/drawIstr.ps");
    sprintf(psData[fillarcps].filename, "%s%s", envAXIOM,
            "/lib/graph/fillarc.ps");
    sprintf(psData[setupps].filename, "%s%s", envAXIOM,
            "/lib/graph/setup.ps");
    sprintf(psData[endps].filename, "%s%s", envAXIOM,
            "/lib/graph/end.ps");
  }
  else {
    fprintf(stderr, " need environment variable AXIOM or DEVE; process aborted\n");
    return (psError);
  }

  return (psInit = yes);
}


/*
 * This needs to be called for every postscript file generated. It
 * initializes the procedure flags.
 */

int
PSInit(Window vw, Window tw)
{
  if (!psInit) {
    /* must have PSGlobalInit() called before this */
    fprintf(stderr, "Error: need initialization for ps data files: call PSGlobalInit().\n");
    return (psError);
  }

  sprintf(psData[output].filename, "%s", PSfilename); /* output file name */

  psData[drawps].flag = no;   /* ps procedures flags */
  psData[drawarcps].flag = no;
  psData[drawfilledps].flag = no;
  psData[drawcolorps].flag = no;
  psData[fillpolyps].flag = no;
  psData[fillwolps].flag = no;
  psData[colorpolyps].flag = no;
  psData[colorwolps].flag = no;
  psData[drawpointps].flag = no;
  psData[drawlineps].flag = no;
  psData[drawlinesps].flag = no;
  psData[drawrectps].flag = no;
  psData[drawstrps].flag = no;
  psData[drawIstrps].flag = no;
  psData[fillarcps].flag = no;

  sprintf(psData[scriptps].filename, "%s", tmpnam(NULL));     /* script file */
  return (Gdraws_setDimension(vw, tw));
}

/*
 * This procedure sets the line attributes; notice that lineWidth is not set
 * for PS, this is because lineWidth of 0, the thinest line on the ps device,
 * is device-dependent, thus, we'll leave it and use default. Also lineStyle
 * is solid in ps by default, thus we don't need to set it. We'll leave out
 * line style here since we really never used anything other than line solid
 * which is the default line style in postscript.
 */

int
PSCreateContext(
                GC gc,       /* graphics context */
                char *C_gc,     /* GC name to be used as postscript variable */
                int lineWidth, int capStyle, int joinStyle,
                float bg, float fg)
{
  FILE *fp;
  GCptr newGC, curGC;
  
  /* get memory for new GC cell */
  
  if (!(newGC = (GCptr) malloc(sizeof(GCstruct)))) {
    fprintf(stderr, "Ran out of memory(malloc) trying to create a ps GC.\n");
    exit(-1);
  }
  
  /* attach newGC to chain */
  
  if (GChead == NULL)
    GChead = newGC;
  else {                      /* attach newGC to end of linked list */
    curGC = GChead;
    while (curGC->next != NULL)
      curGC = curGC->next;
    curGC->next = newGC;
  }
  
  /* fill newGC with information */
  
  newGC->GCint = gc;
  sprintf(newGC->GCchar, "%s", C_gc);
  newGC->next = NULL;
  
  if ((fp = fopen(psData[GCdictps].filename, "a")) == NULL) {
    fprintf(stderr, "PSCreateContext cannot open %s\n",
            psData[GCdictps].filename);
    return (psError);
  }
  
  fprintf(fp, "\t%d\t%d\t%d\n\t%f\t%f\t/%s\tmakeDict\n", joinStyle,
          capStyle, lineWidth, bg, fg, C_gc);
  
  return (fclose(fp));
}

/*
 * Looks into GC linked list with gc (unsigned long) as index to find the
 * character name.
 */

char *
PSfindGC(GC gc)
{
  GCptr curGC;
  
  curGC = GChead;
  while ((curGC != NULL) && (curGC->GCint != gc))
    curGC = curGC->next;
  
  if (curGC == NULL) {
    fprintf(stderr, "PSfindGC cannot find gc: %p.\n",gc);
    return (NULL);
  }
  else
    return (curGC->GCchar);
}

/*
 * Sets foreground color; see XSetForeground
 */

int
GSetForeground(
               GC gc,           /* graphics context */
               float color,     /* foreground color to be set */
               int dFlag)       /* display flag: PS, X,... */
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XSetForeground(dsply, gc, (unsigned long) color);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GSetForeground cannot open %s\n",
                psData[scriptps].filename);
        return (0);
      }
      fprintf(fp, "\t%s\t%f\tsetForeground\n", PSfindGC(gc), color);
      s = fclose(fp);
      break;
    }
  default:
    fprintf(stderr, "GSetForeground request (%d) not implemented yet\n", dFlag);
    return (0);
  }
  return (s);
}

/*
 * Sets background color; see XSetBackground
 */

int
GSetBackground(
               GC gc,           /* graphics context */
               float color,     /* background color to be set */
               int dFlag)       /* display flag: PS, X,... */
{
  int s = 0;

  switch (dFlag) {
  case Xoption:
    XSetBackground(dsply, gc, (unsigned long) color);
    break;
  case PSoption:
    {
      FILE *fp;

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GSetBackground cannot open %s\n",
                psData[scriptps].filename);
        return (0);
      }
      fprintf(fp, "\t%s\t%f\tsetBackground\n", PSfindGC(gc), color);
      s = fclose(fp);
      break;
    }
  default:
    fprintf(stderr, "GSetBackground request (%d) not implemented yet\n", dFlag);
    return (0);
  }
  return (s);
}

/*
 * See XSetLineAttributes.  Notice that we'll not setting line style for
 * postscript.  This is because solid is the ls in ps and in view3D and 2D,
 * we really don't use anything else than solid.
 */

int
GSetLineAttributes(
                   GC gc,
                   int lineWidth, int lineStyle, int capStyle, int joinStyle,
                   int dFlag)
{
  int s = 0;
  
  switch (dFlag) {
  case Xoption:
    XSetLineAttributes(dsply, gc, lineWidth, lineStyle,
                           capStyle, joinStyle);
    break;
  case PSoption:
    {
      FILE *fp;
      int psCap, psJoin;

      switch (capStyle) {
      case 0:           /* CapNotLast is not implemented in ps */
      case 1:
        psCap = psButtCap;
        break;
      case 2:
        psCap = psRoundCap;
        break;
      case 3:
        psCap = psPSqCap;
        break;
      default:
        fprintf(stderr, "cap style: %d unknown, using default.\n", capStyle);
        psCap = psButtCap;
      }

      switch (joinStyle) {
      case 0:
        psJoin = psMiterJoin;
        break;
      case 1:
        psJoin = psRoundJoin;
        break;
      case 2:
        psJoin = psBevelJoin;
        break;
      default:
        fprintf(stderr, "join style: %d unknown, using default.\n", joinStyle);
        psJoin = psMiterJoin;
      }

      /*
       * width of zero is machine-dependent and is not recom- mended,
       * we'll use 1 as the thinest line available if (lineWidth < 1)
       * lineWidth = 1;
       */

      if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
        fprintf(stderr, "GSetLineAttributes cannot open %s\n",
                psData[scriptps].filename);
        return (0);
      }

      fprintf(fp, "\t%d\t%d\t%d\t%s\tsetLineAttributes\n",
              lineWidth, psCap, psJoin, PSfindGC(gc));
      s = fclose(fp);
    }
    break;
  default:
    fprintf(stderr, "GSetLineAttributes request (%d) not implemented yet\n", dFlag);
    return (0);
  }
  return (s);
}

/*
 * This procedure frees the data structure used for GC information, and also
 * unlinks the GC dictionary file.
 */

int
PSClose(void)
{
  if (GChead != NULL) {

    /* free memory used by GC struct */

    GCptr curGC = GChead;

    while (curGC != NULL) {
      GCptr freeGC = curGC;

      curGC = curGC->next;
      free(freeGC);
    }
  }

  /* remove GC dictionary file */

  return (unlink(psData[GCdictps].filename));
}




int 
centerX (GC viewGCx,char * theString,int strlength,int windowWidth)
{
  XFontStruct *fontStruct;
  GContext con;
  int result;


  con=XGContextFromGC(viewGCx);
  fontStruct = XQueryFont(dsply,con);
  if(fontStruct == NULL) return(0);
  result = (windowWidth - XTextWidth(fontStruct,theString,strlength))/2 -
    fontStruct->min_bounds.lbearing;
  XFreeFontInfo(NULL,fontStruct,1); 
  return(result);
}


int 
centerY (GC viewGCy,int windowHeight)
{

  XFontStruct *fontStruct;
  GContext con;
  int result;

  con=XGContextFromGC(viewGCy);
  fontStruct = XQueryFont(dsply,con);
  if (fontStruct == NULL) return(0);
  result = (windowHeight -
            (fontStruct->max_bounds.ascent + fontStruct->max_bounds.descent))/2 +
    fontStruct->max_bounds.ascent;
  XFreeFontInfo(NULL,fontStruct,1);
  return(result);

}
 

/*
 * PSColorPolygon draws and fills a polygon given data in XPoint; see
 * XFillPolygon
 */

int
PSColorPolygon(
               float r, float g, float b,       /* red, green and blue color
                                                 * components */
               XPoint * points,         /* vertices of polygon */
               int numberOfPoints)      /* number of points */
{
  int i = 0;
  FILE *fp;

  if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
    fprintf(stderr, "PSColorPolygon cannot open %s\n",
            psData[scriptps].filename);
    return (psError);
  }

  psData[colorpolyps].flag = yes;     /* sets procedure flag */

  fprintf(fp, "\t%f\t%f\t%f\tsetrgbcolor\n", r, g, b);

  i = numberOfPoints - 1;
  while (i > 0)
  { fprintf(fp, "\t%d\t%d\n", points[i].x, points[i].y);
    i = i-1;
  }

  fprintf(fp, "\t%d\t%d\t%d\tpsColorPoly\n", numberOfPoints, points[i].x, points[i].y);

  return (fclose(fp));
}


/*
 * PSColorwOutline draws and also outlines the colored polygon.
 */

int
PSColorwOutline(
                float r, float g, float b,      /* red, green and blue color
                                                 * components */
                XPoint * points,        /* vertices of polygon */
                int numberOfPoints)     /* number of points */
{
  int i = 0;
  FILE *fp;

  if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
    fprintf(stderr, "PSDrawFOL cannot open %s\n", psData[scriptps].filename);
    return (psError);
  }

  psData[colorwolps].flag = yes;      /* sets procedure flag */

  fprintf(fp, "\t%f\t%f\t%f\tsetrgbcolor\n", r, g, b);

  i = numberOfPoints - 1;
  while (i > 0)
  { fprintf(fp, "\t%d\t%d\n", points[i].x, points[i].y);
    i = i-1;
  }

  fprintf(fp, "\t%d\t%d\t%d\tpsFillwOutline\n",
          numberOfPoints, points[i].x, points[i].y);

  return (fclose(fp));
}
/*
 * This function does what XDraw would do, notice that only a subset of
 * attributes in GC is implemented -- adequate for our purpose now
 */

int
PSDrawColor(
            float r, float g, float b,  /* red, green and blue color
                                         * components */
            XPoint *points,             /* point list */
            int numberOfPoints)         /* vertex count and display flag (X, PS,...) */
{
  int i = 0;
  FILE *fp;

  if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
    fprintf(stderr, "GDraw cannot open %s\n",
            psData[scriptps].filename);
    return (psError);
  }

  psData[drawcolorps].flag = yes;     /* set procedure flag */

  fprintf(fp, "\t%f\t%f\t%f\tsetrgbcolor\n", r, g, b);

  i = numberOfPoints - 1;
  while (i > 0)
  { fprintf(fp, "\t%d\t%d\n", points[i].x, points[i].y);
    i=i-1;
  }

  fprintf(fp, "\t%d\t%d\t%d\tpsDrawColor\n", numberOfPoints, points[i].x, points[i].y);

  return (fclose(fp));
}
/*
 * PSFillPolygon draws and fills a polygon given data in XPoint; see
 * XFillPolygon.
 */

int
PSFillPolygon(
              GC gc,               /* graphics context */
              XPoint * points,      /* vertices of polygon */
              int numberOfPoints)   /* number of points */
{
  int i = 0;
  FILE *fp;

  if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
    fprintf(stderr, "PSFillPolygon cannot open %s\n",
            psData[scriptps].filename);
    return (psError);
  }

  psData[fillpolyps].flag = yes;      /* sets procedure flag */
  fprintf(fp, "\t%s\n", PSfindGC(gc));

  i = numberOfPoints - 1;
  while (i > 0)
  { fprintf(fp, "\t%d\t%d\n", points[i].x, points[i].y);
    i = i-1;
  }

  fprintf(fp, "\t%d\t%d\t%d\tpsFillPoly\n", numberOfPoints, points[i].x, points[i].y);

  return (fclose(fp));
}

/*
 * PSFillwOutline draws and also outlines the filled polygon.
 */

int
PSFillwOutline(
               GC gc,           /* graphics context */
               XPoint * points, /* vertices of polygon */
               int numberOfPoints)
                               /* number of points */
{
  int i = 0;
  FILE *fp;

  if ((fp = fopen(psData[scriptps].filename, "a")) == NULL) {
    fprintf(stderr, "PSDrawFOL cannot open %s\n", psData[scriptps].filename);
    return (psError);
  }

  psData[fillwolps].flag = yes;       /* sets procedure flag */
  fprintf(fp, "\t%s\n", PSfindGC(gc));

  i = numberOfPoints - 1;
  while (i > 0)
  { fprintf(fp, "\t%d\t%d\n", points[i].x, points[i].y);
    i = i-1;
  }

  fprintf(fp, "\t%d\t%d\t%d\tpsFillwOutline\n",
          numberOfPoints, points[i].x, points[i].y);

  return (fclose(fp));
}

static int 
TrivEqual(Window s1,Window s2)
{
  return ( s1 == s2);
}

static int 
TrivHash_code(Window s,int size)
{
  return (s % size);
}


HashTable *
XCreateAssocTable(int size)
{
  HashTable * table;
  table = (HashTable *) malloc(sizeof(HashTable));
  hash_init(table,size,(EqualFunction)TrivEqual,(HashcodeFunction)TrivHash_code);
  return table;
}

void 
XMakeAssoc(Display * dsp, HashTable *table, Window w, int * p)
{
  hash_insert(table,(char *) p, (char *) w);
}

int *
XLookUpAssoc(Display * dsp, HashTable *table,Window w)
{
  return (int *) hash_find(table,(char *)w);
}

void 
XDeleteAssoc(Display * dsp,HashTable * table, Window w)
{
  hash_delete(table,(char *) w);
}





