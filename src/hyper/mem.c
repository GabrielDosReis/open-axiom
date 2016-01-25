/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2010, Gabriel Dos Reis.
  All rights reversed.

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

/******************************************************************************
 *
 * mem.c:  HyperDoc Memory Management Routines.
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "debug.h"
#include "halloc.h"
#include "sockio.h"
#include "hyper.h"
#include "group.h"
#include "event.h"
#include "parse.h"

static void free_cond(CondNode * cond);
static void free_depend(SpadcomDepend * sd);
static void free_lines(LineStruct * lines);
static void dont_free(void * link);
static void free_if_non_NULL(void * p);
static void free_input_box(InputBox * box);
static void free_paste(PasteNode * paste , short des);
static void free_pastearea(TextNode * node , short des);
static void free_pastebutton(TextNode * node , short des);
static void free_radio_boxes(RadioBoxes * radio);


extern HashTable init_page_hash;
extern HashTable init_macro_hash;
extern HashTable init_patch_hash;


static void
free_if_non_NULL(void *p)
{
  if (p){
    free(p);
  }
}


/* allocate an HDWindow Structure and initialize it */

HDWindow *
alloc_hd_window()
{
  HDWindow *w = (HDWindow *) halloc(sizeof(HDWindow), "HDWindow");
  /*char haslisp[10];*/

  w->fMemoStack = (HyperDocPage **)
    halloc(MaxMemoDepth * sizeof(HyperDocPage *), "Memo Stack");
  w->fDownLinkStack = (HyperDocPage **)
    halloc(MaxDownlinkDepth * sizeof(HyperDocPage *), "downlink stack");
  w->fDownLinkStackTop =
    (int *) halloc(MaxDownlinkDepth * sizeof(int), "top downlink stack");
  w->fAxiomFrame = 0;
  init_page_structs(w);

  /* Now I initialize the hash tables for the page */
  w->fCondHashTable = (HashTable *) halloc(sizeof(HashTable), "cond hash");
  hash_init(
            w->fCondHashTable, 
            CondHashSize, 
            (EqualFunction) string_equal, 
            (HashcodeFunction) string_hash);

  w->fPasteHashTable = (HashTable *) halloc(sizeof(HashTable), "paste hash");
  hash_init(
            w->fPasteHashTable, 
            PasteHashSize, 
            (EqualFunction) string_equal, 
            (HashcodeFunction) string_hash);
  w->fPageHashTable = hash_copy_table(&init_page_hash);
  w->fPatchHashTable = hash_copy_table(&init_patch_hash);
  w->fMacroHashTable = hash_copy_table(&init_macro_hash);

  gWindow = w;
  /*sprintf(haslisp, "%1d\0", MenuServerOpened);*/
  make_special_pages(w->fPageHashTable);
  w->fDisplayedCursor = 0;
  return w;
}

void
free_hd_window(HDWindow *w)
{
  if (w) {
    free(w->fMemoStack);
    free(w->fDownLinkStack);
    free(w->fDownLinkStackTop);
    /*
      free(w->fWindowHashTable); will be taken care of by freeing 
      free_hash(w->fPageHashTable, free_page); below
      cf free_page
      */
    free_hash(w->fMacroHashTable, (FreeFunction)dont_free); 
    free_hash(w->fPasteHashTable, (FreeFunction)dont_free); 
    free_hash(w->fPatchHashTable, (FreeFunction)dont_free); 

    free_hash(w->fCondHashTable, (FreeFunction)free_cond);
    free_hash(w->fPageHashTable, (FreeFunction)free_page);
    free(w->fPageHashTable);
    free(w->fPatchHashTable);
    free(w->fMacroHashTable);
    XFreeGC(gXDisplay, w->fStandardGC);
    XFreeGC(gXDisplay, w->fInputGC);
    XFreeGC(gXDisplay, w->fCursorGC);
    XFreeGC(gXDisplay, w->fControlGC);
    free(w);
  }
}


/* Allocate an empty text node */

TextNode *
alloc_node()
{
  TextNode *temp_node;

  temp_node = (TextNode *) halloc(sizeof(TextNode), "Text Node");
  temp_node->type = 0;
  temp_node->space = 0;
  temp_node->height = 0;
  temp_node->width = 0;
  temp_node->x = -1;
  temp_node->y = -1;
  temp_node->data.node = NULL;
  temp_node->next = NULL;
  temp_node->link = NULL;
  temp_node->image.pm = 0;
  return temp_node;
}

void
free_node(TextNode *node, short int des)
{

  if (node == NULL)
    return;

  switch (node->type) {
  case openaxiom_Paste_token:
    free_pastearea(node, des);
    free_node(node->next, des);
    break;
  case openaxiom_Pastebutton_token:
    free_pastebutton(node, des);
    free_node(node->next, des);
    break;
  case openaxiom_Ifcond_token:
    free_node(node->data.ifnode->cond, des);
    free_node(node->data.ifnode->thennode, des);
    free_node(node->data.ifnode->elsenode, des);
    break;
  case openaxiom_Dash_token:
  case openaxiom_Lsquarebrace_token:
  case openaxiom_Word_token:
  case openaxiom_WindowId_token:
  case openaxiom_Punctuation_token:
  case openaxiom_Lbrace_token:
  case openaxiom_Rbrace_token:
  case openaxiom_SimpleBox_token:
  case openaxiom_Verbatim_token:
  case openaxiom_Math_token:
  case openaxiom_Spadsrctxt_token:
  case openaxiom_Spadsrc_token:
    free_if_non_NULL(node->data.text);
    free_node(node->next, des);
    break;
  case openaxiom_Inputstring_token:
    if (des)
      delete_item(node->data.text);
    free_if_non_NULL(node->data.text);
    free_node(node->next, des);
    break;
  case openaxiom_It_token:
  case openaxiom_Sl_token:
  case openaxiom_Tt_token:
  case openaxiom_Rm_token:
  case openaxiom_Emphasize_token:
  case openaxiom_Beep_token:
  case openaxiom_BoldFace_token:
  case openaxiom_Par_token:
  case openaxiom_Newline_token:
  case openaxiom_Horizontalline_token:
  case openaxiom_Item_token:
  case openaxiom_Beginscroll_token:
  case openaxiom_Endscroll_token:
  case openaxiom_Group_token:
  case openaxiom_Table_token:
  case openaxiom_Macro_token:
  case openaxiom_Pound_token:
  case openaxiom_Center_token:
  case openaxiom_Box_token:
  case openaxiom_Mbox_token:
  case openaxiom_Tableitem_token:
  case openaxiom_Scrollingnode_token:
  case openaxiom_Headernode_token:
  case openaxiom_Titlenode_token:
  case openaxiom_Footernode_token:
  case openaxiom_Controlbitmap_token:
  case openaxiom_Fi_token:
  case openaxiom_Description_token:
  case openaxiom_Rsquarebrace_token:
  case openaxiom_Endpaste_token:
  case openaxiom_Endpastebutton_token:
    free_node(node->next, des);
    break;
  case openaxiom_Inputbitmap_token:
  case openaxiom_Inputpixmap_token:
    free_if_non_NULL(node->data.text);
    free_node(node->next, des);
    break;
  case openaxiom_Quitbutton_token:
  case openaxiom_Helpbutton_token:
  case openaxiom_Upbutton_token:
  case openaxiom_Returnbutton_token:
    if (des && node->link->win) {
      hash_delete(gWindow->page->fLinkHashTable,(char *) &node->link->win);
      XDestroyWindow(gXDisplay, node->link->win);
    }
    free_if_non_NULL(node->link);
    free_node(node->next, des);
    break;
  case openaxiom_Memolink_token:
  case openaxiom_Downlink_token:
  case openaxiom_Windowlink_token:
  case openaxiom_Link_token:
  case openaxiom_Lisplink_token:
  case openaxiom_Lispwindowlink_token:
  case openaxiom_Spadcall_token:
  case openaxiom_Spadcallquit_token:
  case openaxiom_LispMemoLink_token:
  case openaxiom_Lispcommand_token:
  case openaxiom_Lispcommandquit_token:
  case openaxiom_LispDownLink_token:
  case openaxiom_Unixlink_token:
  case openaxiom_Spadlink_token:
  case openaxiom_Spadmemolink_token:
  case openaxiom_Spaddownlink_token:
  case openaxiom_Unixcommand_token:
  case openaxiom_Spadcommand_token:
  case openaxiom_Spadgraph_token:
    if (des && node->link->win) {
      hash_delete(gWindow->page->fLinkHashTable,(char *) &node->link->win);
      XDestroyWindow(gXDisplay, node->link->win);
    }
    /* TTT don't free the link before freeing nodes off it */
    /*  free_node(node->link->reference.node);*/
    free_if_non_NULL(node->link);
    free_node(node->next, des);
    break;
  case openaxiom_Free_token:
  case openaxiom_Indent_token:
  case openaxiom_Indentrel_token:
  case openaxiom_HSpace_token:
  case openaxiom_Space_token:
  case openaxiom_VSpace_token:
  case openaxiom_Button_token:
  case openaxiom_Bound_token:
  case openaxiom_Tab_token:
    free_node(node->next, des);
    free_node(node->data.node, des);
    break;
  case openaxiom_End_token:
  case openaxiom_Endcenter_token:
  case openaxiom_Endlink_token:
  case openaxiom_Endgroup_token:
  case openaxiom_Endbox_token:
  case openaxiom_Endmbox_token:
  case openaxiom_Endspadcommand_token:
  case openaxiom_Endpix_token:
  case openaxiom_Endmacro_token:
  case openaxiom_Endparameter_token:
  case openaxiom_Endtable_token:
  case openaxiom_Endtableitem_token:
  case openaxiom_Noop_token:
  case openaxiom_Endinputbox_token:
  case openaxiom_Enddescription_token:
  case openaxiom_Endif_token:
  case openaxiom_Endtitems_token:
  case openaxiom_Enditems_token:
  case openaxiom_Endverbatim_token:
  case openaxiom_Endmath_token:
  case openaxiom_Endspadsrc_token:
    free_node(node->next, des);
    break;
  case openaxiom_Endheader_token:
  case openaxiom_Endtitle_token:
  case openaxiom_Endfooter_token:
  case openaxiom_Endscrolling_token:
  case openaxiom_Endarg_token:
    break;
  case openaxiom_Endbutton_token:
  case openaxiom_Beginitems_token:
    free_if_non_NULL(node->data.text);
    free_node(node->next, des);
    break;

  default:

    /*        printf("don't know how to free type %d\n", node->type); */
    return;
  }
  free(node);
}

IfNode *
alloc_ifnode()
{
  IfNode *tempif;

  tempif = (IfNode *) halloc(sizeof(IfNode), "IfNode");
  tempif->thennode = tempif->elsenode = tempif->cond = NULL;
  return tempif;
}

CondNode *
alloc_condnode()
{
  CondNode *temp;

  temp = (CondNode *) halloc(sizeof(CondNode), "Cond Node");
  temp->cond = temp->label = NULL;
  return temp;
}

static void
free_cond(CondNode *cond)
{
  if (cond) {
    free(cond->label);
    if (cond->cond)
      free(cond->cond);
    free(cond);
  }
}

/* Allocate a new HyperDoc page */

HyperDocPage *
alloc_page(const char *name)
{
  HyperDocPage *page;

  page = (HyperDocPage *) halloc(sizeof(HyperDocPage), "HyperDocPage");
  page->name = name;
  page->header = page->scrolling = page->footer = page->title = NULL;
  page->scroll_off = 0;
  page->sock = NULL;
  page->box_hash = page->depend_hash = NULL;
  page->fLinkHashTable = (HashTable *) halloc(sizeof(HashTable), "Page->fLinkHashTable");
  page->input_list = page->current_item = NULL;
  page->page_flags = 0000000;
  page->filename = NULL;
  page->helppage = alloc_string(TopLevelHelpPage);
  page->radio_boxes = NULL;
  page->button_list = NULL;
  page->s_button_list = NULL;
  return page;
}

void
free_page(HyperDocPage *page)
{
  /*
   * This routine now checks for an environment variable NOFREE. If found
   * it returns.
   */

  if (page == NULL)
    return;

  switch (page->type) {
  case UlUnknownPage:
  case UnknownPage:
  case ErrorPage:
  case Unixfd:
  case SpadGen:
  case Normal:

    /*
     * if(page->name) free(page->name); if(page->filename)
     * free(page->filename);
     */
    free_node(page->scrolling, 0);
    free_node(page->header, 0);
    free_node(page->footer, 0);
    free_node(page->title, 0);
    free_button_list(page->s_button_list);
    free_button_list(page->button_list);
/*
     if (page->sock != NULL)
      free(page->sock);
*/
    free_hash(page->depend_hash, (FreeFunction)free_depend);
    /* TTT line below causes freeing of freed memory and freed memory reads
       links should have been freed by the recursive free_node's above (cf.free_node)
       this is apparently because we are called from free_hd_window
       and we had made a call to free w->fWindowHashTable which is made
       to point to the same thing 
       so we do it HERE not THERE
       */
    free_hash(page->fLinkHashTable, (FreeFunction)dont_free);
    free_hash(page->box_hash, (FreeFunction)free_input_box);
    free_input_list(page->input_list);
    free_radio_boxes(page->radio_boxes);
    free(page->helppage);
    free(page);
    break;
  case UnloadedPageType:
    break;
  default:
    /* fprintf(stderr, "Unknown Page type: %d\n", page->type); */
    break;
  }
}

static void
free_paste(PasteNode *paste, short int des)
{
  if (paste) {
    free_group_stack(paste->group);
    free_item_stack(paste->item_stack);
    free_node(paste->arg_node, des);
    free(paste);
  }
}

static void
free_pastebutton(TextNode *node, short int des)
{
  /*
   * if I am freeing from within parse patch, then I have to do some
   * special things first
   */


  /* the following seems to be unused */
  if (gActiveWindow == node->link->win)
    gActiveWindow = -1;



  if (des) {
    PasteNode *paste;
    paste = (PasteNode *) hash_find(gWindow->fPasteHashTable, node->data.text);

    if (!paste->haspaste) {
      /* squash this thing */

      hash_delete(gWindow->fPasteHashTable, (char *)node->data.text);
      free_paste(paste, des);
      hash_delete(gWindow->page->fLinkHashTable,(char *) &node->link->win);

      XDestroyWindow(gXDisplay, node->link->win);
    }
    else
      paste->hasbutton = 0;
  }

  free_if_non_NULL(node->data.text);

}

static void
free_pastearea(TextNode *node, short int des)
{
  if (des) {
    PasteNode *paste;
    paste = (PasteNode *) hash_find(gWindow->fPasteHashTable, node->data.text);
    if (paste) {
      if (!paste->hasbutton) {
        /* squash this thing */
        hash_delete(gWindow->fPasteHashTable, node->data.text);
        free_paste(paste, des);
      }
      else
        paste->haspaste = 0;
    }
  }

  free_if_non_NULL(node->data.text);
}


void
free_string(char *str)
{
  free_if_non_NULL(str);
}

static void
free_depend(SpadcomDepend *sd)
{
  free_if_non_NULL((char *) sd);
}

static void
dont_free(void  *link)
{
  return;
}

#if 0 
----------- NOT USED
static void
free_link(HyperLink *link)
{
  printf("Link type %d\n",link->type);
  free_if_non_NULL((char *) link);
}
----------- NOT USED
#endif

static void
free_lines(LineStruct *lines)
{
  if (lines->prev != NULL)
    lines->prev->next = NULL;
  while (lines != NULL) {
    LineStruct *del;
    del = lines;
    lines = lines->next;
    free(del->buffer);
    free(del);
  }
}

void
free_input_item(InputItem *sym, short int des)
{
  free_if_non_NULL(sym->name);
  free_lines(sym->lines);
  if (des)
    XDestroyWindow(gXDisplay, sym->win);
}

void
free_input_list(InputItem *il)
{
  while (il) {
    InputItem *trash = il;
    il = il->next;
    free_input_item(trash, 0);
    free(trash);
  }
}

static void
free_input_box(InputBox *box)
{
  if (box) {
    free_if_non_NULL(box->name);
    free(box);
  }
}

static void
free_radio_boxes(RadioBoxes *radio)
{
  if (radio) {
    free_radio_boxes(radio->next);
    free_if_non_NULL(radio->name);
    free(radio);
  }
}

#if 0
--------------- NOT USED
static void
free_image(ImageStruct *image)
{
  free(image->image.xi->data);
  free(image->image.xi);
  free(image);
}
--------------- NOT USED
#endif

#if 0
--------------- NOT USED
static void
free_macro(MacroStore *macro)
{
  if (macro) {
    free(macro->name);
    if (macro->macro_string)
      free(macro->macro_string);
    free(macro);
  }
}
--------------- NOT USED
#endif 



LineStruct *
alloc_inputline(int size)
{
  int i;
  LineStruct *line =
    (LineStruct *) halloc(sizeof(LineStruct), "Line Structure");

  line->prev = line->next = NULL;
  line->buffer = (char *) halloc(sizeof(char) * size + 2, "symbol buffer");
  for (i = 0; i < size + 2; i++)
    line->buffer[i] = 0;
  line->buff_pntr = line->len = 0;
  return line;
}

PasteNode *
alloc_paste_node(char *name)
{
  PasteNode *pastenode =
    (PasteNode *) halloc(sizeof(PasteNode), "PasteNode");

  pastenode->group = NULL;
  pastenode->item_stack = NULL;
  pastenode->arg_node = NULL;
  pastenode->end_node = NULL;
  pastenode->name = alloc_string(name);
  pastenode->haspaste = pastenode->hasbutton = 0;
  return pastenode;
}

PatchStore *
alloc_patchstore()
{
  PatchStore *p = (PatchStore *) halloc(sizeof(PatchStore), "PatchStore");

  p->loaded = 0;
  p->string = NULL;
  return p;
}

void
free_patch(PatchStore *p)
{
  if (p) {
    if (p->name)
      free(p->name);
    if (p->fpos.name)
      free(p->fpos.name);
    if (p->string)
      free(p->string);
    free(p);
  }
}

InputBox *
alloc_inputbox()
{
  InputBox *box = (InputBox *) halloc(sizeof(InputBox), "InputBox");

  box->picked = 0;
  box->next = NULL;
  box->rbs = NULL;
  return box;
}

RadioBoxes *
alloc_rbs()
{
  RadioBoxes *newrb = (RadioBoxes *) halloc(sizeof(RadioBoxes), "Radio Boxes");

  newrb->next = NULL;
  newrb->boxes = NULL;
  return newrb;
}

ButtonList *
alloc_button_list()
{
  ButtonList *newbl = (ButtonList *) halloc(sizeof(ButtonList), "Button List");

  newbl->link = NULL;
  newbl->x0 = newbl->y0 = newbl->x1 = newbl->y1 = 0;
  newbl->next = NULL;
  return newbl;
}

void
free_button_list(ButtonList *bl)
{
  while (bl) {
    ButtonList *nbl = bl->next;
    free(bl);
    bl = nbl;
  }
}


/* resizable static buffers */

#define BufferSlop      0

char *
resizeBuffer(int size, char *oldBuf, int *oldSize)
{
  char *newBuf;
  int newSize;

  if (size <= *oldSize)
    return oldBuf;

  newSize = size + BufferSlop;
  newBuf = (char *) halloc(newSize,"Buffer");
  memset(newBuf,'\0',newSize);
  if (oldBuf) {
    memcpy(newBuf, oldBuf, *oldSize);
    free(oldBuf);
  }
  *oldSize = newSize;

  return newBuf;
}
