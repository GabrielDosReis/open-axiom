/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2016, Gabriel Dos Reis.
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
#include "pixmap.h"

#define boolean unsigned short int

#ifndef TRUE
#define TRUE   ((boolean) 0x1)
#endif
#ifndef FALSE
#define FALSE  ((boolean) 0x0)
#endif

extern void sigusr2_handler(int sig);
extern void sigcld_handler(int sig);
extern void clean_socket(void);
extern void init_page_structs(HDWindow * w);
extern void push_item_stack(void);
extern void clear_item_stack(void);
extern void pop_item_stack(void);
extern ItemStack * copy_item_stack(void);
extern void free_item_stack(ItemStack * is);
extern ButtonList * alloc_button_list(void);
extern CondNode * alloc_condnode(void);
extern HDWindow * alloc_hd_window(void);
extern IfNode * alloc_ifnode(void);
extern InputBox * alloc_inputbox(void);
extern LineStruct * alloc_inputline(int size);
extern TextNode * alloc_node(void);
extern HyperDocPage * alloc_page(const char * name);
extern PasteNode * alloc_paste_node(char * name);
extern RadioBoxes * alloc_rbs(void);
extern void free_button_list(ButtonList * bl);
extern void free_hd_window(HDWindow * w);
extern void free_input_item(InputItem * sym , short des);
extern void free_input_list(InputItem * il);
extern void free_node(TextNode * node , short des);
extern void free_page(HyperDocPage * page);
extern void free_patch(PatchStore * p);
extern void free_string(char * str);
extern char * resizeBuffer(int size , char * oldBuf , int * oldSize);
extern PatchStore * alloc_patchstore(void);
extern InputItem * return_item(char * name);
extern void fill_box(Window w , ImageStruct * image);
extern void toggle_input_box(HyperLink * link);
extern void toggle_radio_box(HyperLink * link);
extern void change_input_focus(HyperLink * link);
extern void next_input_focus(void);
extern void prev_input_focus(void);
extern int delete_item(char * name);
extern XImage * HTReadBitmapFile(Display * display , int screen , char * filename , int * width , int * height);
extern ImageStruct * insert_image_struct(char * filename);
extern void compute_form_page(HyperDocPage * page);
extern int window_width(int cols);
extern void ht2_input(void );
extern void make_record(void );
extern void verify_record(void );
extern char * strCopy(char * s);
extern void print_paste_line(FILE * pfile , char * str);
extern void get_spad_output(FILE * pfile , char * command , int com_type);
extern void get_graph_output(char*, const char*, int);
extern void add_buffer_to_sym(char * buffer , InputItem * sym);
extern void dialog(XEvent * event , KeySym keysym , char * buffer);
extern void draw_inputsymbol(InputItem * sym);
extern void update_inputsymbol(InputItem * sym);
extern HyperDocPage * issue_server_command(HyperLink * link);
extern HyperDocPage * issue_unixlink(TextNode * node);
extern char * print_to_string(TextNode * command);
extern void issue_spadcommand(HyperDocPage * page , TextNode * command , int immediate , int type);
extern OpenAxiom::openaxiom_sio * accept_menu_connection(OpenAxiom::openaxiom_sio * server_sock);
extern char * print_to_string1(TextNode * command , int * sizeBuf);
extern int issue_serverpaste(TextNode * command);
extern void issue_unixcommand(TextNode * node);
extern int issue_unixpaste(TextNode * node);
extern void service_session_socket(void);
extern void send_lisp_command(char * command);
extern void escape_string(char * s);
extern void unescape_string(char * s);
extern char * print_source_to_string1(TextNode * command , int * sizeBuf);
extern char * print_source_to_string(TextNode * command);
extern void change_cond(char * label , char * newcond);
extern int check_condition(TextNode * node);
extern void insert_cond(char * label , char * cond);


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
extern OpenAxiom::openaxiom_sio *session_server;
extern OpenAxiom::openaxiom_sio *spad_socket;
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


// Kludge between the more correct 'array of unsigned char' incoming data,
// and the unprincipled 'pointer to char' parameters of several X11
// functions below.
template<int N>
inline const char* as_chars(const unsigned char (&ary)[N]) {
   return reinterpret_cast<const char*>(&ary[0]);
}

#endif
