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

#include <stack>
#include <vector>
#include "debug.h"
#include "halloc.h"
#include "sockio.h"
#include "parse.h"
#include "parse-paste.h"
#include "parse-types.h"
#include "lex.h"
#include "hyper.h"
#include "extent.h"
#include "event.h"
#include "display.h"
#include "group.h"
#include "scrollbar.h"
#include "titlebar.h"

using namespace OpenAxiom;

static void end_a_page(void );
static HyperDocPage * format_page(UnloadedPage * ulpage);
static void parse_page(HyperDocPage * page);
static void parse_replacepage(void );
static void start_footer(void );
static void start_scrolling(void );
static void Push_MR(void );
static void Pop_MR(void );
static void parse_title(HyperDocPage * page);
static void parse_header(HyperDocPage * page);
static void init_parse_page(HyperDocPage * page);


TextNode *curr_node;            /* current node being parsed. It is to be the
                                 * next one filled   */
HashTable *gLinkHashTable;           /* the hash table of active link windows   */
TextNode *cur_spadcom;          /* The current OpenAxiom command   */

short int gParserMode;           /* Parser mode flag */
HyperRegion gParserRegion;       /* Parser Region flag scrolling etc */
short int gStringValueOk;        /* is a string or box value ok */
bool gEndedPage;

extern int example_number;             /* sequence example number */


int ret_val;                    /* The return value from get_token */

HyperDocPage *cur_page;

char *replace_page;             /* true if dynamic page is link to static one */


void
reset_connection()
{
    if (spad_socket) {
        FD_CLR(spad_socket->socket, &socket_mask);
        purpose_table[spad_socket->purpose] = NULL;
        close(spad_socket->socket);
        spad_socket->socket = 0;
        spad_socket = NULL;
        input_string = nullptr;
        spad_socket->nbytes_pending = 0;
        connect_spad();
    }
}

/*
 * These routines are used for storing an restoring the parser mode. When
 * I start to parse from string, or from a macro, I need to restore the
 * parser mode and region once done. These routines do that
 *
 */

/** The structure for storing parser mode and region **/
struct ModeRegion {
    short int fParserMode;
    HyperRegion fParserRegion;
};

using ModeRegionStack = std::stack<ModeRegion, std::vector<ModeRegion>>;

static ModeRegionStack mr_stack { };  /** Declaration for the stack  **/

static void Push_MR()
{
    ModeRegion mr { gParserMode, gParserRegion };
    mr_stack.push(mr);
}

static void Pop_MR()
{
    if (mr_stack.empty()) {
        fprintf(stderr, "(HyperDoc) Parser Error: Tried to pop empty MR Stack\n");
        exit(-1);
    }
    else {
        auto mr = mr_stack.top();
        mr_stack.pop();
        gParserMode = mr.fParserMode;
        gParserRegion = mr.fParserRegion;
    }
}

void
load_page(HyperDocPage *page)
{
    if (page->type == TokenType::UnloadedPageType) {
        HyperDocPage *new_page;
        init_scanner();
        new_page = format_page((UnloadedPage *)page);
        gWindow->page = new_page;
        /* free(page); */
        page = new_page;
    }
}

static HyperDocPage *formatpage;

/* Display a HyperDoc page with the given name, parsing it if needed */

void
display_page(HyperDocPage *page)
{
    HyperDocPage *new_page;

    XUnmapSubwindows(gXDisplay, gWindow->fMainWindow);
    XUnmapSubwindows(gXDisplay, gWindow->fScrollWindow);
    XFlush(gXDisplay);
    try {
        if (page->type == TokenType::UnloadedPageType || page->type == TokenType::ErrorPage) {
            /* Gack! (page should be a union!) */
            init_scanner();
            new_page = format_page((UnloadedPage *)page);
            gWindow->page = new_page;
            /* free(page); */
            page = new_page;
        }
        show_page(page);
    }
    catch(const HyperError&) {
        /*
         * since I did not finish formatting the page, let me get rid of what
         * I had
         */
        free_page(formatpage);
        /* Replace the buggy page with what I started with */
        hash_replace(gWindow->fPageHashTable, (char *)page, formatpage->name);
        if (!strcmp(formatpage->name, "ErrorPage")) {
            fprintf(stderr, "(HyperDoc) Oops the error page is buggy\n");
            exit(-1);
        }
        gWindow->page = page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        if (page == NULL) {
            fprintf(stderr, "(HyperDoc) No error page found, exiting\n");
            exit(-1);
        }
        reset_connection();
    }
}


/* Parse a given HyperDoc Page, from the top */

static HyperDocPage *
format_page(UnloadedPage *ulpage)
{
    /*int ret_val;*/
    HyperDocPage *page = alloc_page(ulpage->name);

    /*
     * In case of an error I will have to get at this page so I can free the
     * waisted memory
     */
    formatpage = page;
    page->type = TokenType::Normal;
    hash_replace(gWindow->fPageHashTable, (char *)page, ulpage->name);

    cfile = find_fp(ulpage->fpos);


    page->filename = alloc_string(ulpage->fpos.name);
    parse_page(page);
    return page;
}

/* parse the HyperDoc statements in the given string */

void
parse_from_string(const char *str)
{
    OpenAxiom::IOStateManager save_io_state { };
    last_ch = NoChar;
    last_token = {};
    input_string = str;
    input_type = SourceInputKind::String;
    parse_HyperDoc();
}

static void
parse_title(HyperDocPage *page)
{
    TextNode *node;

    Push_MR();
    gParserRegion = HyperRegion::Title;
    get_expected_token(TokenType::Lbrace);
    node = alloc_node();
    page->title = node;
    node->type = TokenType::Titlenode;
    node->next = alloc_node();
    node = node->next;
    node->type = TokenType::Center;
    node->next = alloc_node();
    curr_node = node->next;
    parse_HyperDoc();
    curr_node->type = TokenType::Endcenter;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    curr_node->type = TokenType::Endtitle;
    curr_node->next = NULL;
    if (gNeedIconName) {
        char *title = print_to_string(page->title);

        XSetIconName(gXDisplay, gWindow->fMainWindow, title);
        gNeedIconName = 0;
    }
    if (token.type != TokenType::Rbrace) {
        fprintf(stderr, "(HyperDoc) Parse title was expecting a closing brace\n");
        print_page_and_filename();
        jump();
    }
    linkTitleBarWindows();
    Pop_MR();
}

static void
parse_header(HyperDocPage *page)
{
    TextNode *node;

    Push_MR();
    gParserRegion = HyperRegion::Header;
    node = alloc_node();
    page->header = node;
    node->type = TokenType::Headernode;
    node->next = alloc_node();
    curr_node = node->next;
    parse_HyperDoc();
}

/*
 * parse a page from the top level
 */

static void
init_parse_page(HyperDocPage *page)
{
    gEndedPage = gInDesc = gStringValueOk = gInIf =
        gInButton = gInOptional = gInVerbatim = gInPaste = gInItems =
        gInSpadsrc = false;
    example_number = 1;
    cur_page = page;
    gParserMode = AllMode;
    /* Now I should set the input list to be null */
    free_input_list(page->input_list);
    page->input_list = page->current_item = NULL;

    init_top_group();
    clear_be_stack();

    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable, 
              LinkHashSize, 
              (EqualFunction) window_equal, 
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;

}

void
init_parse_patch(HyperDocPage *page)
{
    gEndedPage = gInDesc = gStringValueOk = gInIf =
        gInButton = gInOptional = gInVerbatim = gInPaste = gInItems =
        gInSpadsrc = false;
    gParserMode = AllMode;
    gParserRegion = HyperRegion::Scrolling;

    init_top_group();
    clear_be_stack();

    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    gPageBeingParsed = page;
}

#define end_page(t) \
  ((t == TokenType::Page \
    || t == TokenType::NewCommand \
    ||t == TokenType::Endpage) ? 1 : 0)

static void
parse_page(HyperDocPage *page)
{
    init_parse_page(page);

    /* Get the name of the page */

    get_expected_token(TokenType::Page);
    get_expected_token(TokenType::Lbrace);
    get_expected_token(TokenType::Word);
    if (page->name == NULL)
        page->name = alloc_string(token.id);
    get_expected_token(TokenType::Rbrace);
    /* parse the title */
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
    parse_title(page);

    /*
     * Now start parsing the header region
     */
    parse_header(page);
}

/*
 * The general HyperDoc parsing function.  expects to see anything. This
 * function will parse until it sees either: 1) A new page starting 2) An end
 * of file 3) a closing bracket "}"
 */

void
parse_HyperDoc()
{
    TextNode *node = NULL /*, *save_node = NULL, *arg_node = NULL*/ ;

    for(;;) {
        ret_val = get_token();

        if (ret_val == EOF)
            return;

        switch (token.type) {
          case TokenType::Spadsrc:
            parse_spadsrc(curr_node);
            break;
          case TokenType::Helppage:
            parse_help();
            break;
          case TokenType::Endpatch:
          case TokenType::Endpaste:
          case TokenType::Rbrace:
            return;
          case TokenType::Paste:
            parse_paste();
            break;
          case TokenType::Pastebutton:
            parse_pastebutton();
            break;
          case TokenType::Endpage:
          case TokenType::NewCommand:
          case TokenType::Page:
            end_a_page();
            return;
          case TokenType::EndScroll:
            token.type = TokenType::Endscroll;
          case TokenType::Endscroll:
            start_footer();
            break;
          case TokenType::Beginscroll:
            start_scrolling();
            break;
          case TokenType::Thispage:        /* it really is just a word */
            curr_node->type = TokenType::Word;
            curr_node->data.text = alloc_string(gPageBeingParsed->name);
            break;
          case TokenType::Icorrection:
            node->type = TokenType::Noop;
            break;
          case TokenType::Newcond:
            parse_newcond();
            break;
          case TokenType::Setcond:
            parse_setcond();
            break;
          case TokenType::Dollar:
            parse_verbatim(TokenType::Math);
            break;
          case TokenType::Verbatim:
            parse_verbatim(TokenType::Verbatim);
            break;
          case TokenType::Ifcond:
            parse_ifcond();
            break;
          case TokenType::Fi:
            if (gInIf)
                return;
            else {
                curr_node->type = TokenType::Noop;
                /* Oops I had a problem parsing this puppy */
                fprintf(stderr, "(HyperDoc) \\fi found without macthing if?\n");
                throw HyperError{};
            }
          case TokenType::Else:
            if (gInIf)
                return;
            else {
                /* Oops I had a problem parsing this puppy */
                curr_node->type = TokenType::Noop;
                fprintf(stderr, "(HyperDoc) \\else found without macthing if?\n");
                throw HyperError{};
            }
          case TokenType::Macro:
            parse_macro();
            break;
          case TokenType::Env:
            /** In this case, get the environment value, and make it a word **/
            parse_env(curr_node);
            break;
          case TokenType::WindowId:
            curr_node->type = TokenType::WindowId;
            curr_node->space = token.id[-1];
            curr_node->data.text = window_id(gWindow->fMainWindow);
            break;
          case TokenType::Punctuation:
          case TokenType::Word:
          case TokenType::Lsquarebrace:
          case TokenType::Dash:
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            curr_node->data.text = alloc_string(token.id);
            break;
          case TokenType::Pagename:
            {
                char *str;

                curr_node->type = TokenType::Word;
                curr_node->space = 0;
                str = halloc(strlen(cur_page->name) + 1, "parse");
                sprintf(str, "%s", cur_page->name);
                curr_node->data.text = alloc_string(str);
                break;
            }
          case TokenType::Examplenumber:
            {
                char *str;

                curr_node->type = TokenType::Word;
                curr_node->space = 0;
                str = halloc(5, "parse");
                sprintf(str, "%d", example_number);
                curr_node->data.text = alloc_string(str);
                break;
            }
          case TokenType::Rsquarebrace:
            if (gInOptional)
                return;
            else {
                curr_node->type = token.type;
                curr_node->space = token.id[-1];
                curr_node->data.text = alloc_string(token.id);
            }
            break;
          case TokenType::EndTitems:
            token.type = TokenType::Endtitems;
          case TokenType::Endtitems:
            if (gParserMode != AllMode) {
                curr_node->type = TokenType::Noop;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[rep(token.type)]);
                throw HyperError{};
            }
            else {
                curr_node->type = token.type;
                break;
            }
          case TokenType::EndItems:
            token.type = TokenType::Enditems;
          case TokenType::Enditems:
            gInItems = false;
          case TokenType::Horizontalline:
          case TokenType::Par:
          case TokenType::Newline:
          case TokenType::Titem:
            if (gParserMode != AllMode) {
                curr_node->type = TokenType::Noop;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[rep(token.type)]);
                throw HyperError{};
            }
            else {
                curr_node->type = token.type;
                break;
            }
          case TokenType::Begintitems:
          case TokenType::Beginitems:
            if (gParserMode != AllMode) {
                curr_node->type = TokenType::Noop;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[rep(token.type)]);
                throw HyperError{};
            }
            else {
                parse_begin_items();
                break;
            }
          case TokenType::Item:
            parse_item();
            break;
          case TokenType::Mitem:
            parse_mitem();
            break;
          case TokenType::VSpace:
          case TokenType::Tab:
          case TokenType::HSpace:
          case TokenType::Indent:
          case TokenType::Indentrel:
            parse_value1();
            break;
          case TokenType::Space:
            parse_value2();
            break;
          case TokenType::Lbrace:
            curr_node->type = TokenType::Group;
            curr_node->space = token.id[-1];
            push_group_stack();
            node = alloc_node();
            curr_node->next = node;
            curr_node = curr_node->next;
            parse_HyperDoc();
            curr_node->type = TokenType::Endgroup;
            pop_group_stack();
            break;
          case TokenType::Upbutton:
          case TokenType::Returnbutton:
          case TokenType::Link:
          case TokenType::Downlink:
          case TokenType::Memolink:
          case TokenType::Windowlink:
            parse_button();
            break;
          case TokenType::Unixlink:
          case TokenType::LispMemoLink:
          case TokenType::LispDownLink:
          case TokenType::Lisplink:
          case TokenType::Lispcommand:
          case TokenType::Lispcommandquit:
          case TokenType::Spadlink:
          case TokenType::Spaddownlink:
          case TokenType::Spadmemolink:
          case TokenType::Unixcommand:
          case TokenType::Spadcall:
          case TokenType::Spadcallquit:
          case TokenType::Qspadcall:
          case TokenType::Qspadcallquit:
          case TokenType::Lispwindowlink:
            parse_command();
            break;
          case TokenType::Controlbitmap:
          case TokenType::Inputbitmap:
          case TokenType::Inputpixmap:
          case TokenType::Inputimage:
            parse_input_pix();
            break;
          case TokenType::Box:
            parse_box();
            break;
          case TokenType::Mbox:
            parse_mbox();
            break;
          case TokenType::Free:
            parse_free();
            break;
          case TokenType::Center:
            parse_centerline();
            break;
          case TokenType::Bound:
            add_dependencies();
            break;
          case TokenType::Spadcommand:
          case TokenType::Spadgraph:
            parse_spadcommand(curr_node);
            break;
          case TokenType::Table:
            parse_table();
            break;
          case TokenType::Beep:
          case TokenType::Emphasize:
          case TokenType::BoldFace:
          case TokenType::Rm:
          case TokenType::It:
          case TokenType::Tt:
          case TokenType::Sl:
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            break;
          case TokenType::Inputstring:
            parse_inputstring();
            break;
          case TokenType::SimpleBox:
            parse_simplebox();
            break;
          case TokenType::BoxValue:
          case TokenType::StringValue:
            if (!gStringValueOk) {
                strcpy(ebuffer,"(HyperDoc): Unexpected Value Command:");
                strcat(ebuffer, token.id);

                parser_error(ebuffer);
                curr_node->type = TokenType::Noop;
                throw HyperError{};
            }
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            get_expected_token(TokenType::Lbrace);
            get_expected_token(TokenType::Word);
            curr_node->data.text = alloc_string(token.id);
            get_expected_token(TokenType::Rbrace);
            break;
          case TokenType::NoLines:
            gPageBeingParsed->page_flags |= NOLINES;
            break;
          case TokenType::Pound:
            curr_node->type = TokenType::Pound;
            curr_node->space = token.id[-1];
            curr_node->next = alloc_node();
            curr_node = curr_node->next;
            parse_parameters();
            break;
          case TokenType::Radiobox:
            parse_radiobox();
            break;
          case TokenType::Radioboxes:
            parse_radioboxes();
            break;
          case TokenType::Replacepage:
            parse_replacepage();
            break;
          default:
            fprintf(stderr, "(HyperDoc) Keyword not currently supported: %s\n", token.id);
            print_page_and_filename();
            curr_node->type = TokenType::Noop;
            break;
        }
        if (gEndedPage)
            return;
        if (curr_node->type != TokenType::Noop) {
            node = alloc_node();
            curr_node->next = node;
            curr_node = node;
        }
    }
}


/* parse a page from a socket source */

HyperDocPage *
parse_page_from_socket()
{
    HyperDocPage *page = alloc_page((char *) NULL);
    HyperDocPage *hpage;

    init_scanner();
    input_type = SourceInputKind::SpadSocket;
    input_string = nullptr;
    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable, 
              LinkHashSize, 
              (EqualFunction) window_equal, 
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;
    replace_page = NULL;
    try {
        parse_page(page);
        page->type = TokenType::SpadGen;
        page->filename = NULL;
        /* just for kicks, let me add this thing to the hash file */
        hpage = (HyperDocPage *) hash_find(gWindow->fPageHashTable, page->name);
        if (hpage)
            hash_replace(gWindow->fPageHashTable, (char *)page, page->name);
        else {
            hash_insert(gWindow->fPageHashTable, (char *)page, page->name);
        }
    }
    catch(const HyperError&) {
        /* Ooops, somewhere I had an error */
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        reset_connection();
    }
    if (replace_page != NULL) {
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, replace_page);
        if (page == NULL)
            fprintf(stderr, "(HyperDoc) Unknown page: %s\n", replace_page);
    }
    return page;
}

HyperDocPage *
parse_page_from_unixfd()
{
    HyperDocPage *page = alloc_page((char *) NULL);

    init_scanner();
    input_type = SourceInputKind::UnixFD;
    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable, 
              LinkHashSize, 
              (EqualFunction) window_equal, 
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;
    try {
        parse_page(page);
        page->type = TokenType::Unixfd;
        page->filename = NULL;
    }
    catch (const HyperError&) {
        /* Ooops, somewhere I had an error */
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        reset_connection();
    }
    return page;
}

static void
start_scrolling()
{

    /*
     * if I am here than I had a begin scroll. This means I should end the
     * header, and then start parsing the footer
     */

    if (gParserRegion != HyperRegion::Header) {
        curr_node->type = TokenType::Noop;
        fprintf(stderr, "(HyperDoc) Parser Error: Unexpected BeginScrollFound\n");
        throw HyperError{};
    }
    curr_node->type = TokenType::Endheader;
    curr_node->next = NULL;
    Pop_MR();

    Push_MR();
    gParserRegion = HyperRegion::Scrolling;
    gWindow->fDisplayedWindow = gWindow->fScrollWindow;
    curr_node = alloc_node();
    gPageBeingParsed->scrolling = curr_node;
    curr_node->type = TokenType::Scrollingnode;
}

static void
start_footer()
{
    /*
     * This ends the parsing of the scrolling region, and then starts to
     * parse the footer
     */

    if (gParserRegion != HyperRegion::Scrolling) {
        curr_node->type = TokenType::Noop;
        fprintf(stderr, "(HyperDoc) Parser Error: Unexpected Endscroll Found\n");
        print_page_and_filename();
        throw HyperError{};
    }

    curr_node->type = TokenType::Endscrolling;
    curr_node->next = NULL;
    Pop_MR();
    linkScrollBars();

    Push_MR();
    gParserRegion = HyperRegion::Footer;
    curr_node = alloc_node();
    curr_node->type = TokenType::Footernode;
    gPageBeingParsed->footer = curr_node;
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
}

static void
end_a_page()
{
    if (gParserRegion == HyperRegion::Scrolling) {
        fprintf(stderr, "%s\n",
                "(HyperDoc) end_a_page: Unexpected End of Page occurred \
                   inside a \beginscroll");
        print_page_and_filename();
        jump();
    }
    gEndedPage = true;
    if (gParserRegion == HyperRegion::Footer) {
        /* the person had all the regions, I basically just have to leave */
        curr_node->type = TokenType::Endscrolling;
        curr_node->next = NULL;
        Pop_MR();
    }
    else if (gParserRegion == HyperRegion::Header) {
        /* person had a header. So just end it and return */
        curr_node->type = TokenType::Endheader;
        curr_node->next = NULL;
        Pop_MR();
        gPageBeingParsed->scrolling = NULL;
        gPageBeingParsed->footer = NULL;
    }
}

static void
parse_replacepage()
{
    get_expected_token(TokenType::Lbrace);
    get_token();
    replace_page = alloc_string(token.id);
    get_expected_token(TokenType::Rbrace);
}
