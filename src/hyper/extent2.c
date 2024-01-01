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
 * extent2.h:  HyperDoc extent computation routines
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "openaxiom-c-macros.h"
#include "debug.h"
#include "halloc.h"
#include "sockio.h"
#include "extent.h"
#include "group.h"
#include "titlebar.h"
#include "cfuns.h"

using namespace OpenAxiom;

static void center_nodes(TextNode * begin_node , TextNode * end_node);
static int input_string_width(TextNode * node);
static int punctuation_width(TextNode * node);
static int text_height1(TextNode * node , TokenType Ender);
static int verbatim_width(TextNode * node);
static int width_of_dash(TextNode * node);
static int word_width(TextNode * node);
static int x_value(TextNode * node);

static int cur_height = 0;
static int max_x_value = 0;

/*
 * start_newline updates the current header node, and also allocates if needed
 * memory for the next Line Header. It also assigns the first TextNode on the
 * line to the structure, because this is the last time I will be able to do
 * this
 */

void
start_newline(int distance, TextNode * node)
{
    if (gLineNode != NULL) {
        if (gTopOfGroupStack->center)
            center_nodes(gLineNode, node);
        gLineNode = node;
    }
    text_y += distance;
    past_line_height = distance;
    present_line_height = line_height;
    gInLine = 0;
}

/*
 * center_nodes goes through and centers all the text between the two
 * given nodes.
 */

static void
center_nodes(TextNode * begin_node, TextNode * end_node)
{
    int begin_x, end_x, wmid_x, offset, mid_x;
    TextNode *node;

    end_x = text_x;
    begin_x = x_value(begin_node);
    mid_x = (int) (end_x + begin_x) / 2;
    wmid_x = (int) (right_margin + indent) / 2;

    if (mid_x > wmid_x)
        offset = 0;
    else
        offset = wmid_x - mid_x;

    for (node = begin_node; node != end_node; node = node->next)
        if (node->x > 0)
            node->x += offset;
}

static int
punctuation_width(TextNode * node)
{
    int twidth, width = strlen(node->data.text);

    twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text, width);

    /* check to see if there was some space in front */

    if (gInLine && (node->space & FRONTSPACE))
        twidth += inter_word_space;

# if 0
    if (node->space & BACKSPACE) {
        switch (node->data.text[0]) {
            case '.':
            case '?':
            case '!':
                twidth += term_punct_space;
                break;
        }
    }
#endif

    return twidth;
}

static int
input_string_width(TextNode * node)
{
    InputItem *item;
    int t_width;

    /** search the symbol table for the proper entry **/

    item = node->link->reference.string;

    /** Once I have gotten this far, I should just be able to calculate
      the width using the normal font **/

    t_width = (item->size + 1) * gInputFont->max_bounds.width + 10;
    return t_width;

}

static int
word_width(TextNode * node)
{
    int twidth, len = strlen(node->data.text);

    twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text, len);
    if (node->space & FRONTSPACE)
        twidth += inter_word_space;

    return twidth;
}

static int
verbatim_width(TextNode * node)
{
    int twidth, len = strlen(node->data.text);

    twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text, len);
    if (node->space)
        twidth += inter_word_space;

    return twidth;
}

static int
width_of_dash(TextNode * node)
{
    int num_dashes, twidth;

    num_dashes = strlen(node->data.text);
    if (num_dashes > 1)
        twidth = node->width = num_dashes * dash_width;
    else
        twidth = node->width = XTextWidth(gTopOfGroupStack->cur_font,
                                          node->data.text, 1);
    if (node->space)
        twidth += inter_word_space;
    return twidth;
}

/*
 * return the gWindow->width in pixels of the given text node, when
 * displayed
 */

int
text_width(TextNode * node, TokenType Ender)
{
    int twidth = 0;

    for (; node != NULL; node = node->next) {
        if (Ender == TokenType::Endtokens) {
            if (node->type == TokenType::Endtokens)
                return twidth;
        }
        else if (node->type == Ender)
            return twidth;

        switch (node->type) {
          case TokenType::Macro:
          case TokenType::Pound:
            if (node->space && gInLine)
                twidth += inter_word_space;
            break;
          case TokenType::Punctuation:
            twidth += punctuation_width(node);
            break;
          case TokenType::Dash:
            if (gInLine && node->space)
                twidth += inter_word_space;
            twidth += width_of_dash(node);
            break;
          case TokenType::Verbatim:
          case TokenType::Spadsrctxt:
            twidth += verbatim_width(node);
            break;
          case TokenType::Lsquarebrace:
          case TokenType::Rsquarebrace:
          case TokenType::Word:
            twidth += word_width(node);
            break;
          case TokenType::Box:
            twidth += 2 * box_space;
            break;
          case TokenType::Link:
          case TokenType::Downlink:
          case TokenType::Memolink:
          case TokenType::Windowlink:
          case TokenType::LispMemoLink:
          case TokenType::Lispwindowlink:
          case TokenType::Lisplink:
          case TokenType::Unixlink:
          case TokenType::Spadcall:
          case TokenType::Spadcallquit:
          case TokenType::Qspadcall:
          case TokenType::Qspadcallquit:
          case TokenType::LispDownLink:
          case TokenType::Lispcommand:
          case TokenType::Lispcommandquit:
          case TokenType::Spadlink:
          case TokenType::Spaddownlink:
          case TokenType::Spadmemolink:
          case TokenType::Unixcommand:
          case TokenType::Upbutton:
          case TokenType::Returnbutton:
          case TokenType::Description:
            push_active_group();
            break;
          case TokenType::Endbutton:
          case TokenType::Endspadcommand:
          case TokenType::Enddescription:
            pop_group_stack();
            break;
          case TokenType::Endlink:
            pop_group_stack();
            break;
          case TokenType::Inputstring:
            twidth += input_string_width(node);
            break;
          case TokenType::SimpleBox:
          case TokenType::Radiobox:
            twidth += node->width + ((node->space) ? inter_word_space : 0);
            break;
          case TokenType::Spadcommand:
          case TokenType::Spadgraph:
            push_spad_group();
            break;
          case TokenType::VSpace:
            break;
          case TokenType::HSpace:
            twidth +=
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case TokenType::Space:
            twidth += (gTopOfGroupStack->cur_font->max_bounds.width) *
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case TokenType::Tab:
            twidth = (gTopOfGroupStack->cur_font->max_bounds.width) *
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case TokenType::Table:
            twidth = gWindow->width - left_margin - right_margin_space;
            break;
          case TokenType::Tableitem:
          case TokenType::Group:
            twidth += (node->space) ? inter_word_space : 0;
            push_group_stack();
            break;
          case TokenType::BoldFace:
            if (node->space)
                twidth += inter_word_space;
            bf_top_group();
            break;
          case TokenType::Emphasize:
            if (node->space)
                twidth += inter_word_space;
            if (gTopOfGroupStack->cur_font == gRmFont)
                em_top_group();
            else
                rm_top_group();
            break;
          case TokenType::It:
            if (node->space)
                twidth += inter_word_space;
            em_top_group();
            break;
          case TokenType::Rm:
          case TokenType::Sl:
          case TokenType::Tt:
            if (node->space)
                twidth += inter_word_space;
            rm_top_group();
            break;
          case TokenType::Endgroup:
            pop_group_stack();
            break;
          case TokenType::Controlbitmap:
          case TokenType::Inputbitmap:
            if (node->width == -1)
                insert_bitmap_file(node);
            twidth += node->width;
            break;
          case TokenType::Inputpixmap:
            if (node->width == -1)
                insert_pixmap_file(node);
            twidth += node->width;
            break;
          case TokenType::Mbox:
          case TokenType::Indent:
          case TokenType::Endmacro:
          case TokenType::Free:
          case TokenType::Bound:
          case TokenType::Beep:
          case TokenType::Item:
          case TokenType::Titem:
          case TokenType::Beginitems:
          case TokenType::Noop:
          case TokenType::Endinputbox:
          case TokenType::Fi:
          case TokenType::Ifcond:
          case TokenType::Endif:
          case TokenType::Begintitems:
          case TokenType::Enditems:
          case TokenType::Endtitems:
          case TokenType::Endtableitem:
          case TokenType::Endtable:
          case TokenType::Endparameter:
          case TokenType::Endbox:
          case TokenType::Endheader:
          case TokenType::Endfooter:
          case TokenType::Endscrolling:
          case TokenType::Endverbatim:
          case TokenType::Endspadsrc:
            break;
          case TokenType::Newline:
            /* WOw, I guess I should ertunr a really big number */
            twidth += gWindow->width;
            break;
          default:

            /*
             * fprintf(stderr, "Unknown nodetype %d in text_width\n",
             * node->type);
             */
            break;
        }
    }
    return twidth;
}

/*
 * total_width traces through the nodes, until it finds a blank space. It is
 * used by compute_word_extent, and compute_punctuation extent to determine
 * How far we go before we actually see white space.
 */

int
total_width(TextNode * node, TokenType Ender)
{
    int twidth = 0;

    for (; (node != NULL); node = node->next) {
        if (Ender == TokenType::Endtokens) {
            if (node->type >= TokenType::Endtokens)
                return twidth;
        }
        else if (node->type == Ender)
            return twidth;

        /*
         * The first thing we check for is to see if there was space in front
         * of the current node, if so we are done
         */

        if (node->space)
            return twidth;

        /*** Else depending on the node type ***/

        switch (node->type) {
          case TokenType::Noop:
          case TokenType::Endinputbox:
          case TokenType::Pound:
          case TokenType::Ifcond:
          case TokenType::Fi:
          case TokenType::Endif:
            break;
          case TokenType::Rsquarebrace:
          case TokenType::Punctuation:
          case TokenType::Word:
          case TokenType::Dash:
            twidth += XTextWidth(gTopOfGroupStack->cur_font, node->data.text,
                                 strlen(node->data.text));
            break;
          case TokenType::Box:
          case TokenType::Link:
          case TokenType::Downlink:
          case TokenType::Memolink:
          case TokenType::Windowlink:
          case TokenType::LispMemoLink:
          case TokenType::Lispwindowlink:
          case TokenType::Lisplink:
          case TokenType::Unixlink:
          case TokenType::Spadcall:
          case TokenType::Spadcallquit:
          case TokenType::Qspadcall:
          case TokenType::Qspadcallquit:
          case TokenType::LispDownLink:
          case TokenType::Lispcommand:
          case TokenType::Lispcommandquit:
          case TokenType::Spadlink:
          case TokenType::Spaddownlink:
          case TokenType::Spadmemolink:
          case TokenType::Unixcommand:
          case TokenType::Inputstring:
          case TokenType::SimpleBox:
          case TokenType::Radiobox:
          case TokenType::Upbutton:
          case TokenType::Returnbutton:
          case TokenType::Spadcommand:
          case TokenType::Spadgraph:
          case TokenType::VSpace:
          case TokenType::HSpace:
          case TokenType::Space:
          case TokenType::Table:
          case TokenType::Group:
          case TokenType::Controlbitmap:
          case TokenType::Inputbitmap:
          case TokenType::Inputpixmap:
          case TokenType::Free:
          case TokenType::Beep:
          case TokenType::Bound:
          case TokenType::Lsquarebrace:
          case TokenType::BoldFace:
          case TokenType::Emphasize:
          case TokenType::It:
          case TokenType::Rm:
          case TokenType::Sl:
          case TokenType::Tt:
          case TokenType::Newline:
          case TokenType::Verbatim:
          case TokenType::Spadsrctxt:
            return twidth;
          default:
            break;
        }
    }
    return twidth;
}

/*
 * init_extents initialize some text size variables
 */

void
init_extents()
{
    present_line_height = line_height;
    gInLine = 0;
    gInItem = 0;
    gInAxiomCommand = 0;
    item_indent = 0;
    gInDesc = 0;
    indent = left_margin;
    text_x = indent;
    gTopOfGroupStack->cur_font = gRmFont;
    gTopOfGroupStack->cur_color = gRmColor;
    right_margin = gWindow->width - right_margin_space;
    clear_item_stack();
}

/*
 * init_title_extents initialize some title text size variables
 */

void
init_title_extents(HyperDocPage * page)
{
    present_line_height = line_height;
    gInLine = 0;
    gInAxiomCommand = 0;
    item_indent = 0;
    gInDesc = 0;
    indent = left_margin + page->title->x;
    text_x = indent;
    gTopOfGroupStack->cur_font = gRmFont;
    gTopOfGroupStack->cur_color = gRmColor;
    right_margin = gWindow->width - right_margin_space - gWindow->border_width -
        2 * twwidth;
    clear_item_stack();
}

/*
 * init_text initialize some text size variables
 */

void
init_text()
{
    normal_text_height = gRmFont->ascent + gRmFont->descent;
    line_height = gRmFont->ascent + gRmFont->descent + inter_line_space;
    word_off_height = line_height - normal_text_height;
    space_width = gRmFont->max_bounds.width;
}

/*
 * text_height returns the height of a piece of formatted text in pixels
 */

int
text_height(TextNode * node, TokenType Ender)
{
    cur_height = 0;
    return text_height1(node, Ender);
}

/*
 * text_height1 is the recursive part of text_height
 */

static int
text_height1(TextNode * node, TokenType Ender)
{
    for (; node != NULL; node = node->next) {
        if (Ender == TokenType::Endtokens) {
            if (node->type >= TokenType::Endtokens)
                return cur_height;
        }
        else if (node->type == Ender)
            return cur_height;
        switch (node->type) {
          case TokenType::Center:
          case TokenType::Downlink:
          case TokenType::Link:
          case TokenType::Spadcommand:
          case TokenType::Spadgraph:
          case TokenType::Upbutton:
          case TokenType::Returnbutton:
          case TokenType::Windowlink:
          case TokenType::Memolink:
          case TokenType::Lispwindowlink:
          case TokenType::Lisplink:
          case TokenType::Unixlink:
          case TokenType::Spadcall:
          case TokenType::Spadcallquit:
          case TokenType::Qspadcall:
          case TokenType::Qspadcallquit:
          case TokenType::LispDownLink:
          case TokenType::LispMemoLink:
          case TokenType::Lispcommand:
          case TokenType::Lispcommandquit:
          case TokenType::Spadlink:
          case TokenType::Spaddownlink:
          case TokenType::Spadmemolink:
          case TokenType::Unixcommand:
          case TokenType::SimpleBox:
          case TokenType::Radiobox:
          case TokenType::Group:
          case TokenType::Box:
          case TokenType::Controlbitmap:
          case TokenType::Inputbitmap:
          case TokenType::Inputpixmap:
          case TokenType::Horizontalline:
          case TokenType::Punctuation:
          case TokenType::Lsquarebrace:
          case TokenType::Rsquarebrace:
          case TokenType::Word:
          case TokenType::Verbatim:
          case TokenType::Math:
          case TokenType::Spadsrctxt:
          case TokenType::Dash:
          case TokenType::Inputstring:
            cur_height = max(node->y, cur_height);
            break;
          case TokenType::Mbox:
          case TokenType::Macro:
          case TokenType::Pound:
          case TokenType::Emphasize:
          case TokenType::BoldFace:
          case TokenType::It:
          case TokenType::Rm:
          case TokenType::Sl:
          case TokenType::Tt:
          case TokenType::Endparameter:
          case TokenType::Description:
          case TokenType::Enddescription:
          case TokenType::Noop:
          case TokenType::Fi:
          case TokenType::Ifcond:
          case TokenType::Endif:
          case TokenType::Endinputbox:
          case TokenType::Tab:
          case TokenType::Newline:
          case TokenType::Space:
          case TokenType::VSpace:
          case TokenType::HSpace:
          case TokenType::Beginitems:
          case TokenType::Begintitems:
          case TokenType::Endtitems:
          case TokenType::Titem:
          case TokenType::Enditems:
          case TokenType::Endtable:
          case TokenType::Endtableitem:
          case TokenType::Item:
          case TokenType::Par:
          case TokenType::Beep:
          case TokenType::Free:
          case TokenType::Bound:
          case TokenType::Endgroup:
          case TokenType::Endcenter:
          case TokenType::Endbutton:
          case TokenType::Endmacro:
          case TokenType::Tableitem:
          case TokenType::Endlink:
          case TokenType::Endspadcommand:
          case TokenType::Indent:
          case TokenType::Indentrel:
          case TokenType::Endbox:
          case TokenType::Endmbox:
          case TokenType::Table:
          case TokenType::Endverbatim:
          case TokenType::Endmath:
          case TokenType::Spadsrc:
          case TokenType::Endspadsrc:
            break;
          case TokenType::Beginscroll:
          case TokenType::Endscroll:
            break;
          case TokenType::Endscrolling:
            return cur_height;
          default:

            /*
             * fprintf(stderr, "Text_height1: Unknown Node Type %d\n",
             * node->type);
             */
            break;
        }
    }
    return cur_height;
}

/*
 * max_x returns the height of a piece of formatted text in pixels
 */

int
max_x(TextNode * node, TokenType Ender)
{
    max_x_value = 0;
    for (; node != NULL; node = node->next) {
        if (Ender == TokenType::Endtokens) {
            if (node->type >= TokenType::Endtokens)
                return max_x_value;
        }
        else if (node->type == Ender)
            return max_x_value;
        switch (node->type) {
          case TokenType::Lsquarebrace:
          case TokenType::Rsquarebrace:
          case TokenType::Word:
            max_x_value = max(max_x_value, node->x + word_width(node));
            break;
          case TokenType::Verbatim:
          case TokenType::Spadsrctxt:
            max_x_value = max(max_x_value, node->x + verbatim_width(node));
            break;
          case TokenType::Punctuation:
            max_x_value = max(max_x_value, node->x + punctuation_width(node));
            break;
          case TokenType::Dash:
            max_x_value = max(max_x_value, node->x + width_of_dash(node));
            break;
          case TokenType::HSpace:
            max_x_value = max(max_x_value, node->x +
                              (node->data.node != NULL ? atoi(node->data.node->data.text) : 1));
            break;
          case TokenType::Space:
            max_x_value = max(max_x_value, node->x +
                           (gTopOfGroupStack->cur_font->max_bounds.width) *
                              (node->data.node != NULL ? atoi(node->data.node->data.text) : 1));
            break;
          case TokenType::Group:
            push_group_stack();
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
          case TokenType::Rm:
          case TokenType::Sl:
          case TokenType::Tt:
            rm_top_group();
            break;
          case TokenType::Endgroup:
            pop_group_stack();
            break;
          case TokenType::Controlbitmap:
          case TokenType::Inputbitmap:
            if (node->width == -1)
                insert_bitmap_file(node);
            max_x_value = max(max_x_value, node->x + node->width);
            break;
          case TokenType::Inputpixmap:
            if (node->width == -1)
                insert_pixmap_file(node);
            max_x_value = max(max_x_value, node->y + node->width);
            break;
          default:
            break;
        }
    }
    return cur_height;
}

static int
x_value(TextNode * node)
{
    for (; node != NULL; node = node->next) {
        switch (node->type) {
          case TokenType::Controlbitmap:
          case TokenType::Inputbitmap:
          case TokenType::Inputpixmap:
          case TokenType::Lsquarebrace:
          case TokenType::Rsquarebrace:
          case TokenType::Word:
          case TokenType::Verbatim:
          case TokenType::Spadsrctxt:
          case TokenType::Dash:
          case TokenType::Punctuation:
          case TokenType::VSpace:
          case TokenType::HSpace:
          case TokenType::Horizontalline:
          case TokenType::Box:
          case TokenType::Downlink:
          case TokenType::Link:
          case TokenType::Lispwindowlink:
          case TokenType::Lisplink:
          case TokenType::Unixlink:
          case TokenType::Spadcall:
          case TokenType::Spadcallquit:
          case TokenType::Qspadcall:
          case TokenType::Qspadcallquit:
          case TokenType::LispDownLink:
          case TokenType::LispMemoLink:
          case TokenType::Lispcommand:
          case TokenType::Lispcommandquit:
          case TokenType::Spadlink:
          case TokenType::Spaddownlink:
          case TokenType::Spadmemolink:
          case TokenType::Spadcommand:
          case TokenType::Spadgraph:
          case TokenType::Unixcommand:
          case TokenType::Space:
          case TokenType::SimpleBox:
          case TokenType::Radiobox:
            return node->x;
          default:
#ifdef DEBUG
            fprintf(stderr, "X_value did not know x value of type %d\n", node->type);
#endif
            return x_value(node->next);
        }
    }
    return 0;
}

/*
 * trailing_space computes the length of the trailing spaces of a node
 */

int
trailing_space(TextNode * node)
{
    int space = 0;

    for (; node->type < TokenType::Endtokens; node = node->next);
    if (node->type == TokenType::Space)
        space += inter_word_space *
            (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
    return space;
}

/*
 * insert_bitmap_file reads a bitmap file into memory
 */

void
insert_bitmap_file(TextNode * node)
{
    char *filename = node->data.text;
    int bm_width, bm_height;
    XImage *im;
    ImageStruct *image;

    if (*filename == ' ')
        filename++;
    if (node->image.pm == 0) {
        if (
        ((image = (ImageStruct *) hash_find(&gImageHashTable, filename)) == NULL)
            || (oa_getenv("HTCACHE"))) {

            /*
             * read the bitmap if not already in memory or if the environment
             * variable HTCACHE is set (NAG addition).
             */

            im = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                  &bm_width, &bm_height);

            /** now add the image to the gImageHashTable **/
            image = (ImageStruct *) halloc(sizeof(ImageStruct), "ImageStruct");
            image->image.xi = im;
            image->width = image->image.xi->width;
            image->height = image->image.xi->height;
            image->filename = (char *) halloc(sizeof(char) * strlen(filename) +1,"Image Filename");
            /* strcpy(image->filename, filename); */
            sprintf(image->filename, "%s", filename);
            hash_insert(&gImageHashTable, (char *)image, image->filename);
        }
        node->width = image->width;
        node->height = image->height;
        node->image.xi = image->image.xi;
    }
}

/*
 * insert_pixmap_file reads a pixmap file into memory
 */

void
insert_pixmap_file(TextNode * node)
{
    char *filename = node->data.text;
    int bm_width, bm_height, ret_val;
    XImage *xi;
    ImageStruct *image;

    if (*filename == ' ')
        filename++;
    if (node->image.xi == 0) {
        if ((image = (ImageStruct *) hash_find(&gImageHashTable, filename)) == NULL) {
            ret_val = read_pixmap_file(gXDisplay, gXScreenNumber, filename, &xi,
                                       &bm_width, &bm_height);
            switch (ret_val) {
              case(-1):
                gSwitch_to_mono = 1;
                return;
              case BitmapFileInvalid:
                fprintf(stderr, "File %s contains invalid bitmap data\n", filename);
                return;
              case BitmapOpenFailed:
                fprintf(stderr, "couldn't open bitmap file %s\n", filename);
                return;
              case BitmapNoMemory:
                fprintf(stderr, "not enough memory to store bitmap\n");
                return;
            }
            image = (ImageStruct *) halloc(sizeof(ImageStruct), "ImageStruct");
            image->width = bm_width;
            image->height = bm_height;
            image->filename = (char *) halloc(sizeof(char) * strlen(filename) +1,
                                              "insert_pixmap--filename");
            /* strcpy(image->filename, filename); */
            sprintf(image->filename, "%s", filename);
            image->image.xi = xi;
            hash_insert(&gImageHashTable, (char *)image, image->filename);
        }
        node->width = image->width;
        node->height = plh(image->height + inter_line_space);
        node->image.xi = image->image.xi;
    }
}

/*
 * plh calculates the closet value of line_height > height
 */

int
plh(int height)
{
    int rheight = height;

    if (gExtentRegion == HyperRegion::Scrolling) {
        for (rheight = line_height; rheight < height; rheight += line_height)
            ;
    }
    return rheight;
}
