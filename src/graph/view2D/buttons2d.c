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

#define _BUTTONS2D_C
#include "axiom-c-macros.h"

#include <string.h>

#include "header2.h"
#include "buttons2d.H1"
#include "all_2d.H1"




#define bColor          98
#define graphColor      138

/*****************************************************
 * int initButtons(buttons)                          *
 *                                                   *
 * Creates the fields for each button window in the  *
 * two dimensional control panel, and returns the    *
 * number of buttons created.                        *
 *                                                   *
 *****************************************************/

int 
#ifdef _NO_PROTO
initButtons (buttons)
buttonStruct *buttons;
#else
initButtons (buttonStruct *buttons)
#endif
{
  int ii, num = 0;

/********** Scale(Zoom) and Translate Potentiometer Buttons **********/

  /* Title:  "Scale" */
  ii = scale2D;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 85;
  buttons[ii].buttonWidth  = 110;
  buttons[ii].buttonHeight = 80;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = yes;          /* scale is a potentiometer */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = potMASK;
  buttons[ii].textColor    = 164;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Title:  "Translate" */
  ii = translate2D;
  buttons[ii].buttonX      = 121;
  buttons[ii].buttonY      = 85;
  buttons[ii].buttonWidth  = 110;
  buttons[ii].buttonHeight = 80;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = yes;          /* translate is a potentiometer */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = potMASK;
  buttons[ii].textColor    = 21;           /* line color of translate arrow */
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Scale potentiometer buttons */

  /* Scale along X axis */
  ii = zoom2Dx;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth  = 53;
  buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"X On ");
  buttons[ii].textColor    = bColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;


  /* Scale along Y axis */
  ii = zoom2Dy;
  buttons[ii].buttonX      = 62;
  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth  = 53;
  buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Y On ");
  buttons[ii].textColor    = bColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Translate along X axis */
  ii = translate2Dx;
  buttons[ii].buttonX      = 121;
  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth  = 53;
  buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"X On ");
  buttons[ii].textColor    = bColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;


  /* Translate along Y axis */
  ii = translate2Dy;
  buttons[ii].buttonX      = 179;
  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth  = 53;
  buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Y On ");
  buttons[ii].textColor    = bColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;


  /* Axes Turned On/Off */
  ii = axesOnOff2D;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 292;
  buttons[ii].buttonWidth  = 90;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Axes On ");
  buttons[ii].textColor    = 75;
  buttons[ii].textHue      = 10;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Units Turned On/Off */
  ii = unitsOnOff2D;
  buttons[ii].buttonX      = 100;
  buttons[ii].buttonY      = 292;
  buttons[ii].buttonWidth  = 90;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Units Off");
  buttons[ii].textColor    = 75;
  buttons[ii].textHue      = 10;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Generate a Postscript file */
  ii = ps2D;
  buttons[ii].buttonX      = 195;
  buttons[ii].buttonY      = 292;
  buttons[ii].buttonWidth  = 36;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"PS");
  buttons[ii].textColor    = 35;
  buttons[ii].textHue      = 5;
  buttons[ii].textShade    = 2;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Bounding Rectangle On/Off */
  ii = spline2D;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 329;
  buttons[ii].buttonWidth  = 66;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Box Off");
  buttons[ii].textColor    = 7;
  buttons[ii].textHue      = 26;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Graph points On/Off */
  ii = pointsOnOff;
  buttons[ii].buttonX      = 75;
  buttons[ii].buttonY      = 329;
  buttons[ii].buttonWidth  = 67;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Pts On ");
  buttons[ii].textColor    = 7;
  buttons[ii].textHue      = 26;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Graph lines On/Off */
  ii = connectOnOff;
  buttons[ii].buttonX      = 147;
  buttons[ii].buttonY      = 329;
  buttons[ii].buttonWidth  = 84;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Lines On ");
  buttons[ii].textColor    = 7;
  buttons[ii].textHue      = 26;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Reset View Position Button */
  ii = reset2D;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 364;
  buttons[ii].buttonWidth  = 60;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Reset");
  buttons[ii].textColor    = bColor;
  buttons[ii].textHue      = 5;
  buttons[ii].textShade    = 2;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Hide Control Panel */
  ii = hideControl2D;
  buttons[ii].buttonX      = 70;
  buttons[ii].buttonY      = 364;
  buttons[ii].buttonWidth  = 88;
  buttons[ii].buttonHeight = 30;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Hide Panel");
  buttons[ii].textColor    = bColor;
  buttons[ii].textHue      = 5;
  buttons[ii].textShade    = 2;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Exits from the viewport running */
  ii = closeAll2D;
  buttons[ii].buttonX      = 169;
  buttons[ii].buttonY      = 370;
  buttons[ii].buttonWidth  = 61;
  buttons[ii].buttonHeight = 24;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Quit");
  buttons[ii].textColor    = 13;
  buttons[ii].textHue      = 29;
  buttons[ii].textShade    = 2;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Indicates that the graph from a viewport is to be picked up. */
  ii = pick2D;
  buttons[ii].buttonX      = 190;
  buttons[ii].buttonY      = 217;
  buttons[ii].buttonWidth  = 40;
  buttons[ii].buttonHeight = 24;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Pick");
  buttons[ii].textColor    = 123;
  buttons[ii].textHue      = 19;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Indicates that the graph from a viewport is to be dropped into a slot. */
  ii = drop2D;
  buttons[ii].buttonX      = 190;
  buttons[ii].buttonY      = 245;
  buttons[ii].buttonWidth  = 40;
  buttons[ii].buttonHeight = 24;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Drop");
  buttons[ii].textColor    = 123;
  buttons[ii].textHue      = 19;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Indicates that the status of the graphs being displayed in the viewport
     is to be cleared. */
  ii = clear2D;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 217;
  buttons[ii].buttonWidth  = 49;
  buttons[ii].buttonHeight = 24;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Clear");
  buttons[ii].textColor    = 123;
  buttons[ii].textHue      = 19;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* Asks for the scale and translation information for the specified graph. */
  ii = query2D;
  buttons[ii].buttonX      = 5;
  buttons[ii].buttonY      = 245;
  buttons[ii].buttonWidth  = 49;
  buttons[ii].buttonHeight = 24;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"Query");
  buttons[ii].textColor    = 123;
  buttons[ii].textHue      = 19;
  buttons[ii].textShade    = 3;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  /* These buttons indicate the 9 available slot numbers into which
     a 2D graph can be placed, and the status of the graph, i.e. whether
     it is displayed or not. */

  ii = graph1;
  buttons[ii].buttonX      = graphBarLeft;
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"1");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect1;
  buttons[ii].buttonX      = graphBarLeft;
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"^");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph2;
  buttons[ii].buttonX      = graphBarLeft + (graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"2");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect2;
  buttons[ii].buttonX      = graphBarLeft + (graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph3;
  buttons[ii].buttonX      = graphBarLeft + 2*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"3");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect3;
  buttons[ii].buttonX      = graphBarLeft + 2*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;  /**** blend these three together ***/
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph4;
  buttons[ii].buttonX      = graphBarLeft + 3*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"4");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect4;
  buttons[ii].buttonX      = graphBarLeft + 3*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph5;
  buttons[ii].buttonX      = graphBarLeft + 4*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"5");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect5;
  buttons[ii].buttonX      = graphBarLeft + 4*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph6;
  buttons[ii].buttonX      = graphBarLeft + 5*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"6");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect6;
  buttons[ii].buttonX      = graphBarLeft + 5*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph7;
  buttons[ii].buttonX      = graphBarLeft + 6*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"7");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect7;
  buttons[ii].buttonX      = graphBarLeft + 6*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph8;
  buttons[ii].buttonX      = graphBarLeft + 7*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"8");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect8;
  buttons[ii].buttonX      = graphBarLeft + 7*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"-");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graph9;
  buttons[ii].buttonX      = graphBarLeft + 8*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = yes;
  buttons[ii].graphSelect  = no;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"9");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  ii = graphSelect9;
  buttons[ii].buttonX      = graphBarLeft + 8*(graphBarWidth);
  buttons[ii].buttonY      = graphBarTop + graphBarHeight;
  buttons[ii].buttonWidth  = graphBarWidth;
  buttons[ii].buttonHeight = graphBarHeight-2;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;                /* this is a regular button */
  buttons[ii].graphNum     = no;
  buttons[ii].graphSelect  = yes;
  buttons[ii].mask         = buttonMASK;
  strcpy(buttons[ii].text,"*");
  buttons[ii].textColor    = graphColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;

  return(num);
}
