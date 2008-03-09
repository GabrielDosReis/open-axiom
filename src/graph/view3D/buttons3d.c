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

#define _BUTTONS3D_C
#include "axiom-c-macros.h"

#include "header.h"
#include "cpanel.h"

#include "all_3d.H1"
#define BH      31  /* button window height */
#define PH      80  /* potentiometer window height */
#define XEDGE   5   /* leftmost button starts here */

/*****************************************************
 * int initButtons(buttons)                          *
 *                                                   *
 * Creates the fields for each button window in the  *
 * three dimensional control panel, and returns the  *
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
  
  int PBY = 297; /* panel button Y coordinate at which buttons appear */
  int ii, num = 0;
  
  /* Rotate, Zoom, and Translate Potentiometer Buttons */
  
  /* Title:  "Rotate" */
  ii = rotate;
  buttons[ii].buttonX       = XEDGE;    buttons[ii].buttonY         = 85;
  buttons[ii].buttonWidth   = 110;      buttons[ii].buttonHeight    = PH;
  buttons[ii].buttonKey     = ii;
  buttons[ii].pot           = yes;  /* rotate is a potentiometer */
  buttons[ii].mask          = potMASK;
  buttons[ii].textColor     = 139;  /* line color of rotate dial */
  buttons[ii].xHalf         = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf         = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Title:  "Scale" */
  ii = zoom;
  buttons[ii].buttonX         = 121;  buttons[ii].buttonY           = 85;
  buttons[ii].buttonWidth     = 58;   buttons[ii].buttonHeight      = PH;
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = yes;  /* zoom(scale) is a potentiometer */
  buttons[ii].mask            = potMASK;
  buttons[ii].textColor       = 165;  /* line color of scale arrow */
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  
  /* Title:  "Translate" */
  ii = translate;
  buttons[ii].buttonX         = 185;  buttons[ii].buttonY      = 85;
  buttons[ii].buttonWidth     = 110;  buttons[ii].buttonHeight = PH;
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = yes;  /* translate is a potentiometer */
  buttons[ii].mask            = potMASK;
  buttons[ii].textColor       = 21;   /* line color of translate arrows */
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* All the rest of the buttons are regular, toggle only buttons and
     have the potentiometer variable set to "no". */
  
  /* First Row of Buttons */
  /* The four rendering mode buttons:  
     wireframe, hiddenline solid, hiddenline shaded and smooth shaded **/

  /* Wirefram mesh */
  ii = transparent;
  buttons[ii].buttonX         = XEDGE;   buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth     = 45;      buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no;
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "Wire";
  buttons[ii].textColor       = modeColor;
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Hidden surface mesh */
  ii = opaqueMesh;
  buttons[ii].buttonX         = 55;  buttons[ii].buttonY      = PBY;
  buttons[ii].buttonWidth     = 53;  buttons[ii].buttonHeight = BH;
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no;
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "Solid";
  buttons[ii].textColor       = modeColor;
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Lambertian polygon fill with phong illumination model */
  ii = render;
  buttons[ii].buttonX       = 113;  buttons[ii].buttonY         = PBY;
  buttons[ii].buttonWidth   = 53;   buttons[ii].buttonHeight    = BH;
  buttons[ii].buttonKey     = ii;
  buttons[ii].pot           = no;
  buttons[ii].mask          = buttonMASK;
  buttons[ii].text          = "Shade";
  buttons[ii].textColor     = modeColor;
  buttons[ii].xHalf         = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf         = buttons[ii].buttonHeight/2;
  ++num;  
  
  /* Phong smooth shading and illumination */
  ii = smooth;
  buttons[ii].buttonX       = 171;  buttons[ii].buttonY         = PBY;
  buttons[ii].buttonWidth   = 59;   buttons[ii].buttonHeight    = BH;
  buttons[ii].buttonKey     = ii;
  buttons[ii].pot           = no;
  buttons[ii].mask          = buttonMASK;
  buttons[ii].text          = "Smooth";
  buttons[ii].textColor     = modeColor;
  buttons[ii].xHalf         = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf         = buttons[ii].buttonHeight/2;
  ++num;  
  
  /* Reset View Position Button */
  ii = resetView;
  buttons[ii].buttonX       = 240;    buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth   = 53;   buttons[ii].buttonHeight    = BH;
  buttons[ii].buttonKey     = ii;
  buttons[ii].pot           = no;
  buttons[ii].mask          = buttonMASK;
  buttons[ii].text          = "Reset";
  buttons[ii].textColor     = 149;
  buttons[ii].xHalf         = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf         = buttons[ii].buttonHeight/2;
  ++num;
  
  
  /* Second Row of Buttons */

  /* update y displacement of buttons row */
  PBY=PBY+BH+3;  
  
  /* Bounding Region On/Off */
  ii = region3D;
  buttons[ii].buttonX     = XEDGE;    buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth = 58;       buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey   = ii;
  buttons[ii].pot         = no;
  buttons[ii].mask        = buttonMASK;
  buttons[ii].text        = "Bounds";
  buttons[ii].textColor   = 6;
  buttons[ii].xHalf       = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf       = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Axes Turned On/Off */
  ii = axesOnOff;
  buttons[ii].buttonX     = 68;    buttons[ii].buttonY      = PBY;
  buttons[ii].buttonWidth = 49;    buttons[ii].buttonHeight = BH;
  buttons[ii].buttonKey   = ii;
  buttons[ii].pot         = no;
  buttons[ii].mask        = buttonMASK;
  buttons[ii].text        = "Axes";
  buttons[ii].textColor   = offColor;
  buttons[ii].xHalf       = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf       = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Outline polygons with black lines in render mode */
  ii = outlineOnOff;
  buttons[ii].buttonX     = 122;   buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth = 70;    buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey   = ii;
  buttons[ii].pot         = no;
  buttons[ii].mask        = buttonMASK;
  buttons[ii].text        = "Outline";
  buttons[ii].textColor   = offColor;
  buttons[ii].xHalf       = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf       = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Display as if a 1-bit plane image */
  ii = bwColor;
  buttons[ii].buttonX      = 197; buttons[ii].buttonY      = PBY;
  buttons[ii].buttonWidth  = 33;  buttons[ii].buttonHeight = BH;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].mask         = buttonMASK;
  buttons[ii].text         = "BW";
  buttons[ii].textColor    = offColor;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Hide Control Panel */
  ii = hideControl;
  buttons[ii].buttonX      = 240;   buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth  = 53;    buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].mask         = buttonMASK;
  buttons[ii].text         = "Hide";
  buttons[ii].textColor    = 149;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;
  
  
  /* Third Row of Buttons */

  /* update y displacement of buttons row */
  PBY=PBY+BH+3;  
  
  /* Shows Lighting Control Panel */
  ii = lighting;
  buttons[ii].buttonX     = XEDGE;   buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth = 65;      buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey   = ii;
  buttons[ii].pot         = no;
  buttons[ii].mask        = buttonMASK;
  buttons[ii].text        = "Light";
  buttons[ii].textColor   = 149;
  buttons[ii].xHalf       = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf       = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Shows View Volume Control Panel */
  ii = viewVolume;
  buttons[ii].buttonX     = 75;    buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth = 100;   buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey   = ii;
  buttons[ii].pot         = no;
  buttons[ii].mask        = buttonMASK;
  buttons[ii].text        = "View Volume";
  buttons[ii].textColor   = 149;
  buttons[ii].xHalf       = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf       = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Shows Save Panel */
  ii = saveit;
  buttons[ii].buttonX      = 180;  buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth  = 50;   buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey    = ii;
  buttons[ii].pot          = no;
  buttons[ii].mask         = buttonMASK;
  buttons[ii].text         = "Save";
  buttons[ii].textColor    = 149;
  buttons[ii].xHalf        = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf        = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Exits from the viewport running */
  ii = closeAll;
  buttons[ii].buttonX     = 240;  buttons[ii].buttonY       = PBY;
  buttons[ii].buttonWidth = 53;   buttons[ii].buttonHeight  = BH;
  buttons[ii].buttonKey   = ii;
  buttons[ii].pot         = no;
  buttons[ii].mask        = buttonMASK;
  buttons[ii].text        = "Quit";
  buttons[ii].textColor   = offColor;
  buttons[ii].xHalf       = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf       = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Buttons to control potentiometers */
  /* These buttons appear above the potentiometer windows which they affect. */

  /* Rotate potentiometer buttons */
  
  /* Rotate about the origin indicated by the axes */
  /* Red is off, Green is on */
  ii = originr;
  buttons[ii].buttonX         = XEDGE;  buttons[ii].buttonY           = 55;
  buttons[ii].buttonWidth     = 53;     buttons[ii].buttonHeight    = 25; 
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no;  
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "origin";
  buttons[ii].textColor       = onColor;
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Rotate about the objects center of volume */
  /* Red is off, Green is on */
  ii = objectr;
  buttons[ii].buttonX         = 62;  buttons[ii].buttonY           = 55;
  buttons[ii].buttonWidth     = 53;   buttons[ii].buttonHeight      = 25; 
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no;  
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "object";
  buttons[ii].textColor       = offColor; 
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Scale potentiometer buttons */
  
  /* Scale along X axis:  Red is off, Green is on */
  ii = zoomx;
  buttons[ii].buttonX         = 121;  buttons[ii].buttonY           = 55;
  buttons[ii].buttonWidth     = 17;   buttons[ii].buttonHeight      = 25; 
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no;  
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "x";
  buttons[ii].textColor       = onColor;
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Scale along Y axis:  Red is off, Green is on */
  ii = zoomy;
  buttons[ii].buttonX         = 141;  buttons[ii].buttonY           = 55;
  buttons[ii].buttonWidth     = 17;   buttons[ii].buttonHeight      = 25;
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no;  
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "y";
  buttons[ii].textColor       = onColor;
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Zoom along Z axis:  Red is off, Green is on */
  ii = zoomz;
  buttons[ii].buttonX         = 161;  buttons[ii].buttonY           = 55;
  buttons[ii].buttonWidth     = 17;   buttons[ii].buttonHeight      = 25;
  buttons[ii].buttonKey       = ii;
  buttons[ii].pot             = no; 
  buttons[ii].mask            = buttonMASK;
  buttons[ii].text            = "z";
  buttons[ii].textColor       = onColor;
  buttons[ii].xHalf           = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf           = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Translate potentiometer buttons */
  
  /* Indicates an orthographic projection of the xy-plane,
     translation is in x and y coordinates */ 
  ii = xy;
  buttons[ii].buttonX    = 185;  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth= 34;   buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey  = ii;
  buttons[ii].pot        = no;  
  buttons[ii].mask       = buttonMASK;
  buttons[ii].text       = "xy";
  buttons[ii].textColor  = 35;
  buttons[ii].xHalf      = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf      = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Indicates an orthographic projection of the xz-plane,
     translation is in x and z coordinates */ 
  ii = xz;
  buttons[ii].buttonX    = 223;  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth= 34;   buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey  = ii;
  buttons[ii].pot        = no;  
  buttons[ii].mask       = buttonMASK;
  buttons[ii].text       = "xz";
  buttons[ii].textColor  = 35;
  buttons[ii].xHalf      = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf      = buttons[ii].buttonHeight/2;
  ++num;
  
  /* Indicates an orthographic projection of the yz-plane,
     translation is in y and z coordinates */ 
  ii = yz;
  buttons[ii].buttonX    = 261;  buttons[ii].buttonY      = 55;
  buttons[ii].buttonWidth= 34;   buttons[ii].buttonHeight = 25;
  buttons[ii].buttonKey  = ii;
  buttons[ii].pot        = no;  
  buttons[ii].mask       = buttonMASK;
  buttons[ii].text       = "yz";
  buttons[ii].textColor  = 35;
  buttons[ii].xHalf      = buttons[ii].buttonWidth/2;
  buttons[ii].yHalf      = buttons[ii].buttonHeight/2;
  ++num;
  
  return(num);
  
} /* initButtons() */

