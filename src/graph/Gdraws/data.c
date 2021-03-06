/*
  Copyright (C) 1991-2002, The Numerical ALgorithms Group Ltd.
  All rights reserved.
  Copyright (C), 2007-2008, Gabriel Dos Reis.
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

/* Data file to test out Gdraw functions */

#include "Gdraws0.h"
#include "../view3D/header.h"
#include "noX10.h"

extern GC gc, gc1;

int
Gdraws_data(int dFlag)
{
    static Vertex vlist[5];
    static int vcount = 5;
    static int x0, y0, x1, y1;
    static const char* str = "This is a text string.";
    static int x, y;
    static int FirstTime = yes;
    static int Rx, Ry, Rwidth, Rheight;
    static int Ix, Iy;
    static const char* Istr = "Image text string.";
    static XPoint points[7];
    static XPoint p1[7];
    static int np1 = 7;
    static int npoints, mode;
    static int ax, ay, angle1, angle2;
    static unsigned int awidth, aheight;
    static Vertex dfvlist[7];
    static int dfvcount = 7;
    static XPoint dfvl1[7];
    static int dfvc = 7;

    if (FirstTime) {

        vlist[0].x = 10;
        vlist[0].y = 10;
        vlist[0].flags = 0;

        vlist[1].x = 250;
        vlist[1].y = 10;
        vlist[1].flags = 0;

        vlist[2].x = 250;
        vlist[2].y = 140;
        vlist[2].flags = 0;

        vlist[3].x = 10;
        vlist[3].y = 140;
        vlist[3].flags = 0;

        vlist[4].x = 10;
        vlist[4].y = 10;
        vlist[4].flags = 0;

        x0 = y0 = 12;
        x1 = 248;
        y1 = 138;

        x = 25;
        y = 50;

        Ix = 20;
        Iy = 180;

        Rx = 160;
        Ry = 40;
        Rwidth = 30;
        Rheight = 80;

        points[0].x = 80;
        points[0].y = 95;
        points[1].x = 30;
        points[1].y = 15;
        points[2].x = 120;
        points[2].y = 70;
        points[3].x = 25;
        points[3].y = 180;
        points[4].x = 43;
        points[4].y = 56;
        points[5].x = 270;
        points[6].y = 0;
        points[6].x = 0;
        points[5].y = 160;

        p1[0].x = 2;
        p1[0].y = 155;
        p1[1].x = 99;
        p1[1].y = 39;
        p1[2].x = 260;
        p1[2].y = 75;
        p1[3].x = 33;
        p1[3].y = 170;
        p1[4].x = 4;
        p1[4].y = 111;
        p1[5].x = 203;
        p1[6].y = 33;
        p1[6].x = 170;
        p1[5].y = 200;

        npoints = 7;
        mode = CoordModeOrigin; /* there's also CoordModePrevious */

        ax = 130;
        ay = 100;
        awidth = 165;
        aheight = 95;
        angle1 = 165;
        angle2 = -330;

        dfvlist[0].x = 300;
        dfvlist[0].y = 0;
        dfvlist[0].flags = 0;

        dfvlist[1].x = 200;
        dfvlist[1].y = 0;
        dfvlist[1].flags = 0;

        dfvlist[2].x = 200;
        dfvlist[2].y = 100;
        dfvlist[2].flags = 0;

        dfvlist[3].x = 100;
        dfvlist[3].y = 100;
        dfvlist[3].flags = 0;

        dfvlist[4].x = 100;
        dfvlist[4].y = 200;
        dfvlist[4].flags = 0;

        dfvlist[5].x = 0;
        dfvlist[5].y = 200;
        dfvlist[5].flags = 0;

        dfvlist[6].x = 300;
        dfvlist[6].y = 0;
        dfvlist[6].flags = 0;

        dfvl1[0].x = 0;
        dfvl1[0].y = 300;
        dfvl1[1].x = 0;
        dfvl1[1].y = 200;
        dfvl1[2].x = 100;
        dfvl1[2].y = 200;
        dfvl1[3].x = 100;
        dfvl1[3].y = 100;
        dfvl1[4].x = 200;
        dfvl1[4].y = 100;
        dfvl1[5].x = 200;
        dfvl1[5].y = 0;
        dfvl1[6].x = 0;
        dfvl1[6].y = 300;

        FirstTime = no;
    }

    if (dFlag == PS) {
        GSetForeground(gc1, 0.3125, PS);
        if (PSFillwOutline(gc1, dfvl1, dfvc) == psError)
            return (psError);
    }

    if (GDrawString(gc, viewport->viewWindow, x, y, str, strlen(str), dFlag)
        == psError)
        return (psError);


    if (GDrawLine(gc, viewport->viewWindow, x0, y0, x1, y1, dFlag) == psError)
        return (psError);

    if (GDrawRectangle(gc, viewport->viewWindow, Rx, Ry, Rwidth, Rheight, dFlag)
        == psError)
        return (psError);

    if (GDrawLines(gc, viewport->viewWindow, points, npoints, mode, dFlag)
        == psError)
        return (psError);

    if (GDrawArc(gc, viewport->viewWindow, ax, ay, awidth, aheight, angle1 * 64,
                 angle2 * 64, dFlag) == psError)
        return (psError);

    GSetForeground(gc1, (float) psBlack, dFlag);
    GSetBackground(gc1, (float) psWhite, dFlag);
    if (GFillArc(gc1, viewport->viewWindow, 20, ay - 20, awidth / 2, aheight / 2,
                 angle1 * 64, angle2 * 64, dFlag) == psError)
        return (psError);


    GSetForeground(gc1, (float) psWhite, dFlag);
    GSetBackground(gc1, (float) psBlack, dFlag);
    if (GDrawImageString(gc1, viewport->viewWindow, Ix, Iy,
                         Istr, strlen(Istr), dFlag) == psError)
        return (psError);

    if (dFlag == PS) {
        GSetForeground(gc1, 0.8, dFlag);
        PSFillPolygon(gc1, p1, np1);
        PSColorPolygon(0.2, 0.4, 0.8, p1, np1);
    }

    return (1);
}
