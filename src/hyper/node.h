/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2023, Gabriel Dos Reis.
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

#ifndef OPENAXIOM_NODE
#define OPENAXIOM_NODE

#include "open-axiom.h"
#include "hash.h"

#ifndef X_DISPLAY_MISSING
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#  include <X11/Xos.h>
typedef Window openaxiom_window;
typedef Pixmap openaxiom_pixmap;
typedef XImage openaxiom_image;
typedef GC openaxiom_graphic_context;
typedef XFontStruct openaxiom_font;
typedef Cursor openaxiom_cursor;
#else                            /* X_DISPLAY_MISSING */
typedef OpenAxiom::Handle openaxiom_window;
typedef OpenAxiom::Handle openaxiom_pixmap;
typedef OpenAxiom::Handle openaxiom_image;
typedef OpenAxiom::Handle openaxiom_graphic_context;
typedef OpenAxiom::Handle openaxiom_cursor;
typedef OpenAxiom::Handle openaxiom_font;
#endif /* X_DISPLAY_MISSING */

/* Struct forward declarations */

struct TextNode;
struct InputBox;
struct InputItem;
struct paste_node;
struct RadioBoxes;
struct GroupItem;
struct PasteNode;

#define Noopbutton         6

#define Scrolling 1
#define Header    2
#define Footer    3
#define Title     4

/* structure for a hyper text link */
struct HyperLink {
   OpenAxiom::TokenType type;                   /* Memolink, Spadlink, Downlink etc. */
   openaxiom_window win;       /* X11 window containing active area */
   union {
      TextNode *node;     /* ID of link to which link refers */
      InputBox *box;
      InputItem *string;
      PasteNode *paste;   /* the paste node area */
   } reference;
   int x,y;                      /* relative position inside page */
};


struct IfNode {
   TextNode *cond;                /* the condition nodes*/
   TextNode *thennode;
   TextNode *elsenode;
};

struct ItemStack {
   int indent;
   int item_indent;
   int in_item;
   ItemStack *next;
};

struct PasteNode {
   char *name;
   SourceInputKind where;       /* where should I be parsing from? */
   short int hasbutton;
   short int haspaste;
   GroupItem *group;
   ItemStack *item_stack;
   TextNode *arg_node;
   TextNode *end_node;
   TextNode *begin_node;
   InputItem *paste_item;
};

/* Structure for formatted hypertext */

struct TextNode {
   OpenAxiom::TokenType  type;                  /* type of node (text, link, etc.) */
   int x,y, width, height;       /* relative location on screen */
   int space;                    /* was there space in front of me ? */
   union {
      char *text;                 /* piece of text to display */
      TextNode *node;     /* argument text */
      IfNode *ifnode;
   } data;
   HyperLink *link;              /* link for active text */
   union {
      openaxiom_pixmap pm;      /* pixmap for bit images */
      openaxiom_image *xi;      /* pixmap image */
   } image;
   TextNode *next;       /* next node in formatted text */
};

/** Structure used to store pixmaps and bitmaps **/

struct ImageStruct {
   int width,height;   /** It's width and height **/
   union {
      openaxiom_pixmap pm;
      openaxiom_image *xi;
   } image;
   char *filename;       /** The filename used to reference it **/
};

/* Structure for locating HyperDoc pages in a source file */

struct FilePosition {
   const char* name;       // file name
   long  pos;              // offset from the beginning of the file
   int   line_number;      // the line number
};

/*** The structure needed for storing a macro **/

struct MacroStore {
   short int loaded;
   FilePosition fpos;
   char *name;
   char *macro_string;
   short number_parameters;
};

/** Structure needed for storing a patch **/
struct PatchStore {
   short int loaded;
   FilePosition fpos;
   char *name;
   char *string;
};

/*  Here are the structures needed for doing input to HyperDoc windows. */

struct LineStruct {
   char *buffer;
   int changed;      /* Has the line changed */
   int line_number;
   int buff_pntr;
   int len;
   LineStruct *prev, *next;
};

struct InputItem {
   char *name;                   /* symbol name                **/
   int size;                      /* the length of the window   **/
   int cursor_x;                  /* x-coordinate for the cursor **/
   int entered;                   /* tells me whether I have typed here
                                     before       */
   int num_lines;                 /* number of lines needed to store
                                     buffer                     */
   LineStruct *lines;
   LineStruct *curr_line;         /* the current line on which the cursor */
   openaxiom_window win;
   InputItem  *next;
};


/* structure for storing input boxes **/
struct InputBox {
   char *name;
   ImageStruct *selected, *unselected;
   short int picked;
   InputBox  *next;
   RadioBoxes *rbs;
   openaxiom_window win;
};

struct RadioBoxes {
   char *name;
   InputBox *boxes;
   ImageStruct *selected, *unselected;
   int width, height;
   RadioBoxes *next;
};

/* Structure for spadcommand dependencies hash table entries */
struct SpadcomDepend {
   char *label;                  /* dependency label */
   TextNode *spadcom;            /* spadcommand defining the label */
   short executed;               /* true iff spadcommand has benn executed */
} ;

struct ButtonList {
   int           x0,y0,x1,y1;
   HyperLink             *link;
   openaxiom_window     win;
   ButtonList    *next;
};

/* Stucture for unformatted hyper text page */

struct HyperDocPage {
   OpenAxiom::TokenType type;                   /* Normal, Quitbutton, Upbutton etc.       */
   const char *name;             /* ID of page                              */
   char *filename;               /* The name of the file in which the page
                                    occurs, Null if not                     */
   int scroll_off;             /* The offset in the scrolling region        */
   int bot_scroll_margin;        /* bottom of the scrolling region          */
   int top_scroll_margin;        /* top of the scrolling region             */
   TextNode *title;              /* the title of the page                   */
   TextNode *header;             /* formatted version of page               */
   TextNode *scrolling;          /* Top of scrolling region                 */
   TextNode *footer;             /* top of non-scrolling region at bottom   */
   OpenAxiom::openaxiom_sio *sock;          /* socket connection for spad buffer       */
   HashTable *fLinkHashTable;         /* active link hash table                  */
   ButtonList *s_button_list;    /* active buttons on page                  */
   ButtonList *button_list;      /* active buttons on page                  */
   HashTable *depend_hash;       /* Hash tables of spadcommand dependencies */
   InputItem *input_list;        /* List of input structures                */
   InputItem *current_item;      /* a pntr to the currently active item     */
   HashTable *box_hash;          /* place where all the boxes are stored    */
   RadioBoxes *radio_boxes;      /* a linked list of radio boxes            */
   short page_flags;             /* A list of flags for the page            */
   char *helppage;               /* the name of the helppage                */
};

/* Structure for an unloaded page */

struct UnloadedPage {
   OpenAxiom::TokenType type;                   /* indicator of unloaded page */
   char *name;                   /* name of page */
   FilePosition fpos;            /* where to find the page */
};

/* Structure for a HyperDoc Window */

struct HDWindow {
   openaxiom_window fMainWindow; /* The main text field window.  */
   openaxiom_window fScrollWindow; /* The scrolling area of the window.  */
   openaxiom_window fDisplayedWindow; /* The current window of the above two,*/
                                /*   being filled by display             */
   
   openaxiom_window fScrollUpWindow; /* Window for scrolling up a line. */
   openaxiom_window fScrollDownWindow; /* Window for scrolling down a line. */
   
   openaxiom_window scrollbar; /* the window for scrolling. */
   openaxiom_window scroller; /* the scroller window.  */
   
   openaxiom_window fTitleBarButton1; /* 1st titlebar bitmap button.  */
   openaxiom_window fTitleBarButton2; /* 2nd titlebar bitmap button.  */
   openaxiom_window fTitleBarButton3; /* 3rd titlebar bitmap button.  */
   openaxiom_window fTitleBarButton4; /* 4th titlebar bitmap button.  */

   int fScrollerTopPos;          /* where the top of the scroller is      */
   int fScrollerHeight;          /* the height of the scroller            */
   int fScrollBarHeight;         /* the height for the scrollbar          */

   int scrollwidth;              /* the width of the scrolling area       */
   int scrollheight;             /* the height of the scrolling area      */
   int scrollupy;                /* Current y position of the scroll up   */
                                /*        button                         */
   int scrolldowny;              /* Current y position of the scroll      */
                                 /*        downbutton                     */
   int scrollbary;               /* Current y position of teh scrollbar   */
   int scrollx;                  /* X coordinates for all of the above    */
   int border_width;             /* Width of the border                   */
   HyperDocPage *page;           /* currently displayed page              */
   int width, height;            /* in pixels                             */
   int columns;                  /* Width in characters, only setable     */
                                /*      for form pages                   */
   HyperDocPage **fMemoStack;    /* stack of memo links */
   HyperDocPage **fDownLinkStack;/* stack of down links */

   int *fDownLinkStackTop;       /* stack of down links */
   int fMemoStackIndex;            /* memo stack pointer */
   int fDownLinkStackIndex;        /* downlink stack pointer */

   HashTable *fWindowHashTable;  /* hash table of active subwindows */
   HashTable *fPageHashTable;    /* hash table of HyperDoc pages */
   HashTable *fPasteHashTable;   /* hash table for paste in areas */
   HashTable *fMacroHashTable;   /* hash table of HyperDoc macros */
   HashTable *fCondHashTable;    /* hash table for values         */
   HashTable *fPatchHashTable;   /* hash table for patch locations */

   int fAxiomFrame;              /* Axiom frame number initializing window */
   openaxiom_graphic_context fStandardGC; /* Graphics context for window */
   openaxiom_graphic_context fInputGC; /* Graphics context for the input windows */
   openaxiom_graphic_context fCursorGC; /* Graphics context for the cursors */
   openaxiom_graphic_context fControlGC; /* Graphics context for the buttons */
   openaxiom_cursor fDisplayedCursor; /* The currently displayed cursor */
};

/* Structure for identifying appropriate link hash tables */

struct LinkHashID {
   int code;                     /* code of active area */
   HyperDocPage *page;           /* page for which hash table applies */
};



struct GroupItem {
    int cur_color;
    openaxiom_font *cur_font;
    int center;
    GroupItem *next;
};


struct CondNode {
   char *label;
   char *cond;
};

struct parameter_list_type {
    char          **list;       /** The parameters in string form **/
    short           number;     /** How many parameters are there **/
    parameter_list_type *next;
};

using ParameterList = parameter_list_type*;

/*** Flags for the page ***/

#define NOLINES 0000001          /* Ibid, for the bottom of the page      ***/

/* Here are some of the functions and constants declared and needed in
      htadd.c                                                    ******/

#define NoChar   -9999
#define db_file_name "ht.db"


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

#define openaxiom_max(x,y) ((x) > (y) ? (x) : (y))


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



#endif /* OPENAXIOM_NODE */
