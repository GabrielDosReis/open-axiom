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

#ifndef OPENAXIOM_XSpadFill_INCLUDED
#define OPENAXIOM_XSpadFill_INCLUDED

extern int XInitSpadFill(Display *  , int  , Colormap *  , int *  , int *  , int *  , int * );
extern void XSpadFillSetArcMode(Display *  , int );
extern GC SpadFillGC(Display *  , int  , int  , const char* );
extern unsigned long XSolidColor(int  , int );
extern void XSpadFillRectangle(Display *  , Drawable  , int  , int  , unsigned int  , unsigned int  , int  , int );
extern void XSpadFillRectangles(Display *  , Drawable  , XRectangle *  , int  , int  , int );
extern void XSpadFillPolygon(Display *  , Drawable  , XPoint *  , int  , int  , int  , int  , int );
extern void XSpadFillArc(Display *  , Drawable  , int  , int  , unsigned int  , unsigned int  , int  , int  , int  , int );
extern void XSpadFillArcs(Display *  , Drawable  , XArc *  , int  , int  , int );

#endif /* OPENAXIOM_XSpadFill_INCLUDED */
