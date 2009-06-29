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

#ifndef _HYPER_H_
#define _HYPER_H_ 1

#include "openaxiom-c-macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>


#include "com.h"
#include "token.h"
#include "hash.h"
#include "node.h"

#define boolean unsigned short int

#ifndef TRUE
#define TRUE   ((boolean) 0x1)
#endif
#ifndef FALSE
#define FALSE  ((boolean) 0x0)
#endif

#ifndef HTADD
extern int MenuServerOpened;

/* These are all the colors one can use in HyperDoc. */

extern int  gActiveColor,
            gAxiomColor,
            gBackgroundColor,
            gBfColor,
            gControlBackgroundColor,
            gControlForegroundColor,
            gEmColor,
            gInputBackgroundColor,
            gInputForegroundColor,
            gItColor,
            gRmColor,
            gSlColor,
            gTtColor;

/* These are all the different fonts one can use in HyperDoc. */

extern XFontStruct  *gActiveFont,
                    *gAxiomFont,
                    *gBfFont,
                    *gEmFont,
                    *gInputFont,
                    *gItFont,
                    *gRmFont,
                    *gSlFont,
                    *gTitleFont,
                    *gTtFont;


#endif

#ifndef HTADD
/* From hyper.c */
extern int      gXScreenNumber;
extern Display *gXDisplay;
extern int gSwitch_to_mono;
extern unsigned long * spadColors;
extern int gIsEndOfOutput;
extern HDWindow *gWindow;
extern openaxiom_sio *session_server;
extern openaxiom_sio *spad_socket;
extern HashTable gFileHashTable;
extern HashTable gImageHashTable;           /* A global hash table for images */
extern openaxiom_cursor gNormalCursor; /* The normal mouse cursor. */
extern openaxiom_cursor gActiveCursor; /* The cursor in active regions. */
extern openaxiom_cursor gBusyCursor; /* The clock cursor for when I am busy */
extern int gIsAxiomServer;            /* true iff HyperDoc is acting as an Axiom server */
extern int    gArgc;                  /* original argc from main */
extern char **gArgv;                  /* original argv from main */
/* from input.c */
extern openaxiom_image *picked;
extern int picked_height;
extern int picked_width;
extern openaxiom_image *unpicked;
extern int unpicked_height;
extern int unpicked_width;
/* from display.c */
extern int line_height;
extern int need_scroll_up_button;
extern int scrolling;
extern int need_scroll_down_button;
extern int space_width;
#endif 

/* Here are some of the functions and constants declared and needed in
      htadd.c                                                    ******/

#define NoChar   -9999
#define db_file_name "ht.db"


/* Types of HyperDoc pages */

#define UlUnknownPage    9993 /*I hate this hack, but I have to know whether*/
#define UnknownPage      9994 /*this page has been loaded or not.           */
#define ErrorPage        9995
#define Unixfd           9996
#define SpadGen          9997
#define Normal           9998
#define UnloadedPageType 9999

/* Commands from Axiom */

#define EndOfPage        99
#define SendLine         98
#define StartPage        97          /* A normal HyperDoc page */
#define LinkToPage       96
#define PopUpPage        95          /* A pop-up page          */
#define PopUpNamedPage   94
#define KillPage         93
#define ReplacePage      92
#define ReplaceNamedPage 91
#define SpadError        90

/* Constants declaring size of page stacks */

#define MaxMemoDepth    25              /* max nesting level for memolinks */
#define MaxDownlinkDepth 50             /* max downlink nesting depth */

/* Constants defining the size of various hash tables */

#define PageHashSize     1000
#define FileHashSize     30
#define SessionHashSize  10
#define MacroHashSize    100
#define ImageHashSize    100
#define CondHashSize     100
#define BoxHashSize      20
#define PasteHashSize    100
#define PatchHashSize    100

/* A couple of macros for memo and down links */

#define need_up_button \
  (gWindow->fMemoStackIndex ? gWindow->fDownLinkStackIndex >= \
   gWindow->fDownLinkStackTop[gWindow->fMemoStackIndex-1] \
   : gWindow->fDownLinkStackIndex)

#define need_return_button (gWindow->fMemoStackIndex)

#define need_help_button (gWindow->page->helppage != NULL)

#define max(x,y) ((x) > (y) ? (x) : (y))


#define pick_box(box) fill_box(box->win, box->selected)
#define unpick_box(box) fill_box(box->win, box->unselected)

#define TopLevelHelpPage  "ugHyperPage"
#define NoMoreHelpPage    "NoMoreHelpPage"
#define KeyDefsHelpPage   "ugHyperKeysPage"
#define InputAreaHelpPage "ugHyperInputPage"

/* definitions for connecting to the Axiom server */

#define Connected       0
#define NotConnected    1
#define SpadBusy        2

/* some GUI-dependent stuff */

#define BeepAtTheUser()         /* (XBell(gXDisplay,  5)) */
#define LoudBeepAtTheUser()     /* (XBell(gXDisplay, 50)) */

extern int connect_spad(void);


/***      default fonts      ***/
#define RmFontDefault         "-adobe-courier-medium-r-normal--18-*-*-*-m-*-iso8859-1"
#define TtFontDefault         "-adobe-courier-medium-r-normal--18-*-*-*-m-*-iso8859-1"
#define ActiveFontDefault     "-adobe-courier-bold-r-normal--18-*-*-*-m-*-iso8859-1"
#define AxiomFontDefault      "-adobe-courier-bold-o-normal--18-*-*-*-m-*-iso8859-1"
#define EmphasizeFontDefault  "-adobe-courier-medium-o-normal--18-*-*-*-m-*-iso8859-1"
#define BoldFontDefault       "-adobe-courier-bold-r-normal--18-*-*-*-m-*-iso8859-1"

/* external variables and functions.  See the source file for a description
 of their purposes */

extern HashTable gSessionHashTable;   /* hash table of HD windows */

extern HDWindow *gParentWindow;       /* the parent window. The one that
                                        * appears when you first start HD */

extern HyperLink *quitLink; /** a special link to the protected quit page **/

extern GroupItem   *gTopOfGroupStack;

extern HyperDocPage *gPageBeingParsed;

#endif
