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
 * parse-paste.c: HyperDoc routines for paste-in areas.
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "openaxiom-c-macros.h"
#include "debug.h"
#include "halloc.h"
#include "sockio.h"
#include "parse.h"
#include "hyper.h"
#include "display.h"
#include "group.h"
#include "lex.h"

using namespace OpenAxiom;

static void load_patch(PatchStore * patch);

short int gInPaste;


void
parse_paste()
{
    TextNode *pn = curr_node;
    PasteNode *paste;
    SourceInputKind where;

    if (gParserRegion != HyperRegion::Scrolling) {
        fprintf(stderr, "(HyperDoc) Paste areas are only allowed in the scrolling area:");
        print_page_and_filename();
        jump();
    }
    gInPaste++;

    /* now I need to get the name */
    get_token();
    if (token.type != TokenType::Lbrace) {
        fprintf(stderr, "(HyperDoc) A paste area needs a name:\n");
        print_next_ten_tokens();
        print_page_and_filename();
        jump();
    }
    pn->data.text = alloc_string(get_input_string());
    pn->type = TokenType::Paste;

    /*
     * now see if there is already an entry in the hash_table for this thing,
     * if not create it and put it there.
     */
    paste = (PasteNode *) hash_find(gWindow->fPasteHashTable, pn->data.text);
    if (paste == 0) {
        paste = alloc_paste_node(pn->data.text);
        hash_insert(gWindow->fPasteHashTable, (char *)paste, paste->name);
    }
    else if (paste->haspaste) {
        fprintf(stderr, "(HyperDoc) Tried to redefine paste area %s\n", paste->name);
        print_page_and_filename();
        /* jump(); */
    }
    paste->haspaste = 1;
    paste->paste_item = current_item();
    get_token();
    if (token.type == TokenType::Lsquarebrace) {
        /* user wishes to specify a where to send the command */
        where = get_where();
        if (where == SourceInputKind::Error) {
            paste->where = where;
            fprintf(stderr, "(HyperDoc) \\begin{paste} was expecting [lisp|unix|ht]\n");
            print_next_ten_tokens();
            print_page_and_filename();
            jump();
        }
        else
            paste->where = where;
        get_token();
    }
    else
       paste->where = SourceInputKind::File;

    /* now try to get the command argument or page name */
    if (token.type != TokenType::Lbrace) {
        paste->where = { };
        fprintf(stderr, "(HyperDoc) \\begin{paste} was expecting an argument\n");
        print_next_ten_tokens();
        print_page_and_filename();
        jump();
    }
    paste->arg_node = alloc_node();
    curr_node = paste->arg_node;
    parse_HyperDoc();
    curr_node->type = TokenType::Endarg;

    gWindow->fDisplayedWindow = gWindow->fScrollWindow;

    /* Now try to find the displaying text */
    pn->next = alloc_node();
    curr_node = pn->next;
    parse_HyperDoc();
    curr_node->type = TokenType::Endpaste;
    paste->end_node = curr_node;

    paste->begin_node = pn;
    gInPaste--;
}

void
parse_pastebutton()
{
    PasteNode *paste;
    TextNode *pb;

    /*
     * this routine parse a \pastebutton expression. The syntax is
     * \pastebutton{name}
     */
    pb = curr_node;
    pb->type = TokenType::Pastebutton;

    /* first thing I should do is get the name */
    get_token();
    if (token.type != TokenType::Lbrace) {
        fprintf(stderr, "(HyperDoc) \\pastebutton needs a name\n");
        print_page_and_filename();
        print_next_ten_tokens();
        jump();
    }
    pb->data.text = alloc_string(get_input_string());

    /*
     * now I should see if the paste area has already been parsed, and if not
     * I should create a spot in the hash table for it
     */
    paste = (PasteNode *) hash_find(gWindow->fPasteHashTable, pb->data.text);
    if (paste == 0) {
        paste = alloc_paste_node(pb->data.text);
        hash_insert(gWindow->fPasteHashTable,(char *) paste, paste->name);
    }
    else if (paste->hasbutton) {
        fprintf(stderr, "(HyperDoc) Tried to redefine paste area %s\n", paste->name);
        print_page_and_filename();
        /* jump(); */
    }
    paste->hasbutton = 1;

    /* Now we need to parse the HyperDoc and for the displayed text */

    get_token();
    if (token.type != TokenType::Lbrace) {
        fprintf(stderr, "(HyperDoc) \\pastebutton was expecting a { \n");
        print_page_and_filename();
        print_next_ten_tokens();
        jump();
    }
    pb->next = alloc_node();
    curr_node = pb->next;
    parse_HyperDoc();
    curr_node->type = TokenType::Endpastebutton;

    /* once that is done I need only make the window for this link */
    pb->link = make_paste_window(paste);
}


/*
 * this routine is responsible for parsing a patch from a file. To do this I
 * guess er will init_scanner, then parse, the parsed piece of text
 * will replace the current PasteNode which will be squashed down to
 * nothing, and then discarded.
 */

HyperDocPage *
parse_patch(PasteNode *paste)
{
    TextNode *new_paste;
    TextNode *end_node;
    TextNode *begin_node;
    TextNode *arg_node;
    TextNode *old;
    TextNode *next_node;
    InputItem *paste_item = paste->paste_item;
    SourceInputKind where = paste->where;
    GroupItem *g = paste->group;
    ItemStack *is = paste->item_stack;
    PatchStore *patch;
    char *patch_name;
    int ret_value = 1;

    /* prepare to throw away the current paste node */
    end_node = paste->end_node;
    next_node = end_node->next;
    begin_node = paste->begin_node;
    arg_node = paste->arg_node;
    old = begin_node->next;

    /* now read the new stuff and add it in between all this stuff */

    switch (where) {
    case SourceInputKind::File:
        patch_name = print_to_string(arg_node);
        patch = (PatchStore *) hash_find(gWindow->fPatchHashTable, patch_name);
        if (!patch) {
            fprintf(stderr, "(HyperDoc) Unknown patch name %s\n", patch_name);
            BeepAtTheUser();
            return 0;
        }
        if (!patch->loaded)
            load_patch(patch);
        input_type = SourceInputKind::String;
        input_string = patch->string;
        break;
    case SourceInputKind::SpadSocket:
       input_type = SourceInputKind::SpadSocket;
        ret_value = issue_serverpaste(arg_node);
        if (ret_value < 0) {
            paste->where = where;
            paste->end_node = end_node;
            paste->arg_node = arg_node;
            paste->group = g;
            paste->item_stack = is;
            paste->haspaste = 1;
            return 0;
        }
        break;
    case SourceInputKind::UnixFD:
       input_type = SourceInputKind::UnixFD;
        issue_unixpaste(arg_node);
        break;
      default:
        fprintf(stderr, "(HyperDoc) \\parsebutton error: Unknown where\n");
        exit(-1);
        break;
    }

    paste->where =  { };
    paste->end_node = paste->arg_node = paste->begin_node = 0;
    paste->group = 0;
    paste->item_stack = 0;
    paste->haspaste = 0;
    paste->paste_item = 0;

    try {
        end_node->next = 0;
        free_node(old, 1);

        init_parse_patch(gWindow->page);
        init_paste_item(paste_item);
        get_token();
        if (token.type != TokenType::Patch) {
            fprintf(stderr, "(HyperDoc) Pastebutton %s was expecting a patch\n",
                    paste->name);
            jump();
        }
        if (input_type == SourceInputKind::String) {
            get_token();
            if (token.type != TokenType::Lbrace) {
                token_name(token.type);
                fprintf(stderr, "(HyperDoc) Unexpected %s \n", ebuffer);
                print_page_and_filename();
                jump();
            }

            get_token();
            if (token.type != TokenType::Word) {
                token_name(token.type);
                fprintf(stderr, "(HyperDoc) Unexpected %s \n", ebuffer);
                print_page_and_filename();
                jump();
            }

            get_token();
            if (token.type != TokenType::Rbrace) {
                token_name(token.type);
                fprintf(stderr, "(HyperDoc) Unexpected %s \n", ebuffer);
                print_page_and_filename();
                jump();
            }
        }
        new_paste = alloc_node();
        curr_node = new_paste;
        parse_HyperDoc();

        /* Once I am back, I need only reallign all the text structures */
        curr_node->type = TokenType::Noop;
        curr_node->next = next_node;
        begin_node->next = new_paste;
        begin_node->type = TokenType::Noop;
        free(begin_node->data.text);
        begin_node->data.text = 0;

        gWindow->fDisplayedWindow = gWindow->fScrollWindow;

        repaste_item();

        paste_page(begin_node);
    }
    catch(const HyperError&) {
        fprintf(stderr, "(HyperDoc) Had an error parsing a patch: Goodbye!\n");
        exit(-1);
    }

    /* so now I should just be able to disappear */
    return gWindow->page;
}

static void
load_patch(PatchStore *patch)
{
    long start_fpos;
    int size = 0;
    int limsize;
    char *trace;
    OpenAxiom::IOStateManager save_io_state { };

    cfile = find_fp(patch->fpos);

    init_scanner();

    /** First thing I should do is make sure that the name is correct ***/
    start_fpos = fpos;
    get_expected_token(TokenType::Patch);
    get_expected_token(TokenType::Lbrace);
    get_expected_token(TokenType::Word);
    if (strcmp(token.id, patch->name)) {
        /** WOW, Somehow I had the location of the wrong macro **/
        fprintf(stderr, "(HyperDoc) Expected patch name %s: got instead %s in load_patch\n",
                patch->name, token.id);
        jump();
    }
    get_expected_token(TokenType::Rbrace);

    scan_HyperDoc();
    fseek(cfile, patch->fpos.pos + start_fpos, 0);
    limsize = fpos - start_fpos + 1;
    patch->string = (char *) halloc((limsize + 1) * sizeof(char), "Patch String");
    for (size = 1, trace = patch->string; size < limsize; size++)
        *trace++ = getc(cfile);
    *trace = '\0';
    patch->loaded = 1;
}


