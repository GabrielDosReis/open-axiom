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

/******************************************************************************
 *
 * show_types.c:  Show the various types of things that can show up in text
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "openaxiom-c-macros.h"
#include "debug.h"
#include "sockio.h"
#include "hyper.h"
#include "display.h"
#include "extent.h"
#include "group.h"

static void show_image(TextNode * node , GC gc);
static void show_input(TextNode * node);
static void show_link(TextNode * node);
static void show_paste(TextNode * node);
static void show_pastebutton(TextNode * node);
static void show_simple_box(TextNode * node);
static void show_spadcommand(TextNode * node);


/*
 * Display the page whose extent has been computed, using the actual size of
 * the window, and y_off to determine clipped areas
 */

void
show_text(TextNode *node, int Ender)
{
    /*int twidth, len;*/
    /*int otext_x, otext_y, t;*/
    /*XFontStruct *old_font;*/
    /*int old_color;*/

    for (; node != NULL; node = node->next) {
        switch (node->type) {
          case 0:
          case openaxiom_Beginitems_token:
          case openaxiom_Begintitems_token:
          case openaxiom_Bound_token:
          case openaxiom_Center_token:
          case openaxiom_Free_token:
          case openaxiom_HSpace_token:
          case openaxiom_Indent_token:
          case openaxiom_Indentrel_token:
          case openaxiom_Item_token:
          case openaxiom_Macro_token:
          case openaxiom_Mbox_token:
          case openaxiom_Newline_token:
          case openaxiom_Noop_token:
          case openaxiom_Par_token:
          case openaxiom_Pound_token:
          case openaxiom_Rbrace_token:
          case openaxiom_Space_token:
          case openaxiom_Tab_token:
          case openaxiom_Table_token:
          case openaxiom_Titem_token:
          case openaxiom_VSpace_token:
            break;

          case openaxiom_Dash_token:
          case openaxiom_Fi_token:
          case openaxiom_Ifcond_token:
            if (visible(node->y, node->height)) {
                if (strlen(node->data.text) > 1) {
                    XDrawLine(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x,
                              node->y + gRegionOffset + y_off
                              - gTopOfGroupStack->cur_font->descent -
                              word_off_height,
                              node->x + node->width,
                         node->y + gRegionOffset + y_off - word_off_height -
                              gTopOfGroupStack->cur_font->descent);
                }
                else {
                    XDrawString(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x, node->y +
                       gRegionOffset - gTopOfGroupStack->cur_font->descent + y_off,
                                node->data.text, 1);
                }
            }
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;

          case openaxiom_Lsquarebrace_token:
          case openaxiom_Math_token:
          case openaxiom_Punctuation_token:
          case openaxiom_Rsquarebrace_token:
          case openaxiom_Spadsrctxt_token:
          case openaxiom_WindowId_token:
          case openaxiom_Word_token:
            if (visible(node->y, node->height))
                XDrawString(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x, node->y +
                       gRegionOffset - gTopOfGroupStack->cur_font->descent + y_off,
                            node->data.text, node->width);
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;

          case openaxiom_Verbatim_token:
            push_group_stack();
            tt_top_group();
            if (visible(node->y, node->height))
                XDrawString(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x, node->y +
                       gRegionOffset - gTopOfGroupStack->cur_font->descent + y_off,
                            node->data.text, node->width);
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            pop_group_stack();
            break;

          case openaxiom_Horizontalline_token:
            if (visible(node->y, node->height)) {
                line_top_group();
                XDrawLine(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, 0,
                          node->y + gRegionOffset + y_off,
                          gWindow->width,
                          node->y + gRegionOffset + y_off);
                pop_group_stack();
            }
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;

          case openaxiom_Box_token:
            if (visible(node->y, node->height))
                XDrawRectangle(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC,
                               node->x,
                             node->y + gRegionOffset + y_off - node->height,
                               node->width,
                               node->height);
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;


          case openaxiom_Downlink_token:
          case openaxiom_Link_token:
          case openaxiom_LispDownLink_token:
          case openaxiom_LispMemoLink_token:
          case openaxiom_Lispcommand_token:
          case openaxiom_Lispcommandquit_token:
          case openaxiom_Lisplink_token:
          case openaxiom_Lispwindowlink_token:
          case openaxiom_Memolink_token:
          case openaxiom_Qspadcall_token:
          case openaxiom_Qspadcallquit_token:
          case openaxiom_Returnbutton_token:
          case openaxiom_Spadcall_token:
          case openaxiom_Spadcallquit_token:
          case openaxiom_Spaddownlink_token:
          case openaxiom_Spadlink_token:
          case openaxiom_Spadmemolink_token:
          case openaxiom_Unixcommand_token:
          case openaxiom_Unixlink_token:
          case openaxiom_Upbutton_token:
          case openaxiom_Windowlink_token:
            if (pix_visible(node->y, node->height))
                show_link(node);
            break;

          case openaxiom_Spadcommand_token:
          case openaxiom_Spadgraph_token:
          case openaxiom_Spadsrc_token:
            show_spadcommand(node);
            break;

          case openaxiom_Pastebutton_token:
            if (visible(node->y, node->height))
                show_pastebutton(node);
            break;

          case openaxiom_Paste_token:
            show_paste(node);
            break;

          case openaxiom_Group_token:
          case openaxiom_Tableitem_token:
            push_group_stack();
            break;

          case openaxiom_Controlbitmap_token:
            show_image(node, gWindow->fControlGC);
            break;

          case openaxiom_Inputbitmap_token:
            show_image(node, gWindow->fStandardGC);
            break;

          case openaxiom_Inputpixmap_token:
            show_image(node, gWindow->fStandardGC);
            break;

          case openaxiom_BoldFace_token:
            bf_top_group();
            break;

          case openaxiom_Emphasize_token:
            if (gTopOfGroupStack->cur_font == gRmFont)
                em_top_group();
            else
                rm_top_group();
            break;

          case openaxiom_It_token:
            em_top_group();
            break;

          case openaxiom_Sl_token:
          case openaxiom_Rm_token:
            rm_top_group();
            break;

          case openaxiom_Tt_token:
            tt_top_group();
            break;

          case openaxiom_Inputstring_token:
            show_input(node);
            break;

          case openaxiom_Radiobox_token:
          case openaxiom_SimpleBox_token:
            show_simple_box(node);
            break;

          case openaxiom_Beep_token:
            LoudBeepAtTheUser();
            break;

          case openaxiom_Description_token:
            bf_top_group();
            break;

          case openaxiom_Endspadsrc_token:
          case openaxiom_Endspadcommand_token:
            gInAxiomCommand = 1;
          case openaxiom_Endtableitem_token:
          case openaxiom_Enddescription_token:
          case openaxiom_Endpastebutton_token:
          case openaxiom_Endlink_token:
          case openaxiom_Endbutton_token:
          case openaxiom_Endgroup_token:
            pop_group_stack();
          case openaxiom_Endverbatim_token:
          case openaxiom_Endmath_token:
          case openaxiom_Endbox_token:
          case openaxiom_Endtable_token:
          case openaxiom_Endmbox_token:
          case openaxiom_Endparameter_token:
          case openaxiom_Endpaste_token:
          case openaxiom_Endinputbox_token:
          case openaxiom_Endcenter_token:
          case openaxiom_Endmacro_token:
          case openaxiom_Endif_token:
          case openaxiom_Endtitems_token:
          case openaxiom_Enditems_token:

            /*
             * Now since I can show specific regions of the text, then at
             * this point I should check to see if I am the end
             */
            if (node->type == Ender)
                return;
            break;
          case openaxiom_Endfooter_token:
          case openaxiom_Endscrolling_token:
          case openaxiom_Endheader_token:
          case openaxiom_Endtitle_token:

            /*
             * regardless of what ender I have, I always terminate showing
             * with one of these
             */
            return;
          default:
            fprintf(stderr, "Show_text: Unknown Node Type %d\n", node->type);
            break;
        }
    }
}

static void
show_link(TextNode *node)
{
    /* XFontStruct *old_font;*/
    XWindowChanges wc;
    /*int twidth, boxwidth, old_color;*/
    int active;

    switch (node->type) {
      case openaxiom_Upbutton_token:
        if (!need_up_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      case openaxiom_Returnbutton_token:
        if (!need_return_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      case openaxiom_Helpbutton_token:
        if (!need_help_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      default:
        active = 1;
        break;
    }

    if (active) {
        ButtonList *bl = alloc_button_list();

        push_active_group();
        wc.x = node->x;
        wc.y = node->y - node->height + y_off + gRegionOffset;
        wc.height = node->height;
        wc.width = node->width - trailing_space(node->next);
        bl->x0 = wc.x;
        bl->y0 = wc.y;
        bl->x1 = bl->x0 + wc.width;
        bl->y1 = bl->y0 + wc.height;
        bl->link = node->link;
        if (!not_in_scroll) {
            bl->y0 += gWindow->page->top_scroll_margin + scroll_top_margin;
            bl->y1 += gWindow->page->top_scroll_margin + scroll_top_margin;
            bl->next = gWindow->page->s_button_list;
            gWindow->page->s_button_list = bl;
        }
        else {
            bl->next = gWindow->page->button_list;
            gWindow->page->button_list = bl;
        }
    }
    else
        rm_top_group();
}

static void
show_paste(TextNode *node)
{
    PasteNode *paste;

    if (!(paste = (PasteNode *) hash_find(gWindow->fPasteHashTable,
        node->data.text)))
            return;

    /*
     * Once I have got this far, then I had better save the current group
     * stack and the item stack
     */
    if (paste->group)
        free_group_stack(paste->group);
    paste->group = (GroupItem *) copy_group_stack();
    if (paste->item_stack)
        free_item_stack(paste->item_stack);
    paste->item_stack = (ItemStack *) copy_item_stack();
}

static void
show_pastebutton(TextNode *node)
{
    /*XFontStruct *old_font;*/
    XWindowChanges wc;
    /*int twidth, boxwidth, old_color;*/
    /*int active;*/

    push_active_group();
    wc.x = node->x;
    wc.y = node->y - node->height + y_off + gRegionOffset;
    wc.height = node->height;
    wc.width = node->width - trailing_space(node->next);
#ifdef DEBUG
    fprintf(stderr, "Configure in  show_link %d %d %d %d\n",
            wc.x, wc.y, wc.width, wc.height);
#endif
    XConfigureWindow(gXDisplay, node->link->win,
        CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, node->link->win);
}

/* display an input string window */

static void
show_input(TextNode *node)
{
    /*XFontStruct *old_font;*/
    XWindowChanges wc;
    /*int twidth, boxwidth, old_color;*/
    /*Window root, child;*/
    /*int root_x, root_y, win_x, win_y, buttons;*/
    InputItem *item;
    char *inpbuffer;

    item = node->link->reference.string;
    inpbuffer = item->curr_line->buffer;

    wc.border_width = 0;
    wc.x = node->x;
    wc.y = node->y + gRegionOffset + y_off - node->height + 2;
    wc.height = node->height - 2;
    wc.width = node->width;
    if (pix_visible(node->y, node->height)) {
        XConfigureWindow(gXDisplay, node->link->win,
                         CWX | CWY | CWHeight | CWWidth | CWBorderWidth,
                         &wc);
        XMapWindow(gXDisplay, node->link->win);
    }
    XFlush(gXDisplay);
    draw_inputsymbol(item);
}

static void
show_simple_box(TextNode *node)
{
    XWindowChanges wc;
    InputBox *box;

    /* first configure the box size properly */
    box = node->link->reference.box;
    wc.x = node->x;
    wc.y = node->y + gRegionOffset + y_off - node->height;
    wc.height = ((box->picked) ?
                 (box->selected->height) : (box->unselected->height));
    wc.width = node->width;
    if (visible(node->y + gTopOfGroupStack->cur_font->ascent, node->height)) {
        XConfigureWindow(gXDisplay, node->link->win, CWX | CWY | CWHeight | CWWidth,
                         &wc);
        XMapWindow(gXDisplay, node->link->win);
        if (box->picked)
            pick_box(box);
        else
            unpick_box(box);
    }
}

/* display a spad command node */

static void
show_spadcommand(TextNode *node)
{
    XWindowChanges wc;

    gInAxiomCommand = 1;

    push_spad_group();
    wc.x = node->x;
    if (node->type == openaxiom_Spadsrc_token)
        wc.y = node->y + gRegionOffset + y_off - 2 * node->height;
    else
        wc.y = node->y + gRegionOffset + y_off - node->height;
    wc.height = node->height;
    wc.width = node->width;
#ifdef DEBUG
    fprintf(stderr, "Spadcommand configured %d x %d -- (%d, %d)\n",
            wc.width, wc.height, wc.x, wc.y);
#endif
    XConfigureWindow(gXDisplay, node->link->win,
        CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, node->link->win);
}


/* display a pixmap */

static void
show_image(TextNode *node, GC gc)
{
    int src_x, src_y, src_width, src_height, dest_x, dest_y, ret_val;

    if (!pix_visible(node->y, node->height))
        return;
    if (node->image.xi == NULL)
        return;

    dest_x = node->x;
    src_x = 0;
    src_y = 0;
    dest_y = node->y + gRegionOffset - node->height + y_off;
    need_scroll_up_button = 1;
    if (node->width > (right_margin - node->x))
        src_width = right_margin - node->x;
    else
        src_width = node->width;

    if (gDisplayRegion != Scrolling) {
        src_y = 0;
        src_height = node->image.xi->height;
    }
    else {
        /* I may have only a partial image */
        if (dest_y < 0) {       /* the top is cut off */
            src_y = -dest_y;
            dest_y = 0;
            src_height = node->image.xi->height - src_y;
        }
        else if (dest_y + node->image.xi->height > gWindow->scrollheight) {
            /* the bottom is cut off */
            src_y = 0;
            src_height = gWindow->scrollheight - dest_y;
        }
        else {                  /* the whole thing is visible */
            src_y = 0;
            src_height = node->image.xi->height;
        }
    }

    ret_val = XPutImage(gXDisplay, gWindow->fDisplayedWindow, gc,
            node->image.xi, src_x, src_y, dest_x, dest_y,
            src_width, src_height);

    switch (ret_val) {
      case BadDrawable:
        fprintf(stderr, "(HyperDoc: show_image) bad drawable\n");
        break;
      case BadGC:
        fprintf(stderr, "(HyperDoc: show_image) bad GC");
        break;
      case BadMatch:
        fprintf(stderr, "(HyperDoc: show_image) bad match");
        break;
      case BadValue:
#ifndef HP9platform
        fprintf(stderr, "(HyperDoc: show_image) bad value");
#endif /* HP complains about this*/
        break;
    }
}
