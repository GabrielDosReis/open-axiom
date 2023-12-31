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

using namespace OpenAxiom;

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
show_text(TextNode *node, TokenType Ender)
{
    /*int twidth, len;*/
    /*int otext_x, otext_y, t;*/
    /*XFontStruct *old_font;*/
    /*int old_color;*/

    for (; node != NULL; node = node->next) {
        switch (node->type) {
          case TokenType{}:             // FIXME: Why this?
          case TokenType::Beginitems:
          case TokenType::Begintitems:
          case TokenType::Bound:
          case TokenType::Center:
          case TokenType::Free:
          case TokenType::HSpace:
          case TokenType::Indent:
          case TokenType::Indentrel:
          case TokenType::Item:
          case TokenType::Macro:
          case TokenType::Mbox:
          case TokenType::Newline:
          case TokenType::Noop:
          case TokenType::Par:
          case TokenType::Pound:
          case TokenType::Rbrace:
          case TokenType::Space:
          case TokenType::Tab:
          case TokenType::Table:
          case TokenType::Titem:
          case TokenType::VSpace:
            break;

          case TokenType::Dash:
          case TokenType::Fi:
          case TokenType::Ifcond:
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

          case TokenType::Lsquarebrace:
          case TokenType::Math:
          case TokenType::Punctuation:
          case TokenType::Rsquarebrace:
          case TokenType::Spadsrctxt:
          case TokenType::WindowId:
          case TokenType::Word:
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

          case TokenType::Verbatim:
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

          case TokenType::Horizontalline:
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

          case TokenType::Box:
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


          case TokenType::Downlink:
          case TokenType::Link:
          case TokenType::LispDownLink:
          case TokenType::LispMemoLink:
          case TokenType::Lispcommand:
          case TokenType::Lispcommandquit:
          case TokenType::Lisplink:
          case TokenType::Lispwindowlink:
          case TokenType::Memolink:
          case TokenType::Qspadcall:
          case TokenType::Qspadcallquit:
          case TokenType::Returnbutton:
          case TokenType::Spadcall:
          case TokenType::Spadcallquit:
          case TokenType::Spaddownlink:
          case TokenType::Spadlink:
          case TokenType::Spadmemolink:
          case TokenType::Unixcommand:
          case TokenType::Unixlink:
          case TokenType::Upbutton:
          case TokenType::Windowlink:
            if (pix_visible(node->y, node->height))
                show_link(node);
            break;

          case TokenType::Spadcommand:
          case TokenType::Spadgraph:
          case TokenType::Spadsrc:
            show_spadcommand(node);
            break;

          case TokenType::Pastebutton:
            if (visible(node->y, node->height))
                show_pastebutton(node);
            break;

          case TokenType::Paste:
            show_paste(node);
            break;

          case TokenType::Group:
          case TokenType::Tableitem:
            push_group_stack();
            break;

          case TokenType::Controlbitmap:
            show_image(node, gWindow->fControlGC);
            break;

          case TokenType::Inputbitmap:
            show_image(node, gWindow->fStandardGC);
            break;

          case TokenType::Inputpixmap:
            show_image(node, gWindow->fStandardGC);
            break;

          case TokenType::BoldFace:
            bf_top_group();
            break;

          case TokenType::Emphasize:
            if (gTopOfGroupStack->cur_font == gRmFont)
                em_top_group();
            else
                rm_top_group();
            break;

          case TokenType::It:
            em_top_group();
            break;

          case TokenType::Sl:
          case TokenType::Rm:
            rm_top_group();
            break;

          case TokenType::Tt:
            tt_top_group();
            break;

          case TokenType::Inputstring:
            show_input(node);
            break;

          case TokenType::Radiobox:
          case TokenType::SimpleBox:
            show_simple_box(node);
            break;

          case TokenType::Beep:
            LoudBeepAtTheUser();
            break;

          case TokenType::Description:
            bf_top_group();
            break;

          case TokenType::Endspadsrc:
          case TokenType::Endspadcommand:
            gInAxiomCommand = 1;
          case TokenType::Endtableitem:
          case TokenType::Enddescription:
          case TokenType::Endpastebutton:
          case TokenType::Endlink:
          case TokenType::Endbutton:
          case TokenType::Endgroup:
            pop_group_stack();
          case TokenType::Endverbatim:
          case TokenType::Endmath:
          case TokenType::Endbox:
          case TokenType::Endtable:
          case TokenType::Endmbox:
          case TokenType::Endparameter:
          case TokenType::Endpaste:
          case TokenType::Endinputbox:
          case TokenType::Endcenter:
          case TokenType::Endmacro:
          case TokenType::Endif:
          case TokenType::Endtitems:
          case TokenType::Enditems:

            /*
             * Now since I can show specific regions of the text, then at
             * this point I should check to see if I am the end
             */
            if (node->type == Ender)
                return;
            break;
          case TokenType::Endfooter:
          case TokenType::Endscrolling:
          case TokenType::Endheader:
          case TokenType::Endtitle:

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
      case TokenType::Upbutton:
        if (!need_up_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      case TokenType::Returnbutton:
        if (!need_return_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      case TokenType::Helpbutton:
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
    if (node->type == TokenType::Spadsrc)
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

    if (gDisplayRegion != HyperRegion::Scrolling) {
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
