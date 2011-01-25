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
short int gParserRegion;         /* Parser Region flag scrolling etc */
short int gStringValueOk;        /* is a string or box value ok */
boolean gEndedPage;

extern int example_number;             /* sequence example number */


int ret_val;                    /* The return value from get_token */

HyperDocPage *cur_page;

char *replace_page;             /* true if dynamic page is link to static one */


void
reset_connection(void)
{
    if (spad_socket) {
        FD_CLR(spad_socket->socket, &socket_mask);
        purpose_table[spad_socket->purpose] = NULL;
        close(spad_socket->socket);
        spad_socket->socket = 0;
        spad_socket = NULL;
        if (input_string)
            input_string[0] = '\0';
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

typedef struct mr_stack {
    /** The structure for storing parser mode and region **/
    short int fParserMode;
    short int fParserRegion;
    struct mr_stack *fNext;
}   MR_Stack;

MR_Stack *top_mr_stack = NULL;  /** Declaration for the stack  **/

static void
Push_MR(void)
{
    MR_Stack *newStackItem = (MR_Stack *) halloc(sizeof(MR_Stack), "Mode Region Stack");

    newStackItem->fParserMode = gParserMode;
    newStackItem->fParserRegion = gParserRegion;
    newStackItem->fNext = top_mr_stack;
    top_mr_stack = newStackItem;
}

static void
Pop_MR(void)
{
    MR_Stack *old = top_mr_stack;

    if (old == NULL) {
        fprintf(stderr, "(HyperDoc) Parser Error: Tried to pop empty MR Stack\n");
        exit(-1);
    }
    else {
        gParserMode = old->fParserMode;
        gParserRegion = old->fParserRegion;
        top_mr_stack = old->fNext;
        free(old);
    }
}

void
load_page(HyperDocPage *page)
{
    if (page->type == UnloadedPageType) {
        HyperDocPage *new_page;
        init_scanner();
        new_page = format_page((UnloadedPage *)page);
        gWindow->page = new_page;
        /* free(page); */
        page = new_page;
    }
}

HyperDocPage *formatpage;

/* Display a HyperDoc page with the given name, parsing it if needed */

void
display_page(HyperDocPage *page)
{
    HyperDocPage *new_page;

    XUnmapSubwindows(gXDisplay, gWindow->fMainWindow);
    XUnmapSubwindows(gXDisplay, gWindow->fScrollWindow);
    XFlush(gXDisplay);

    if (setjmp(jmpbuf)) {

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
    if (page->type == UnloadedPageType || page->type == ErrorPage) {
        /* Gack! (page should be a union!) */
        init_scanner();
        new_page = format_page((UnloadedPage *)page);
        gWindow->page = new_page;
        /* free(page); */
        page = new_page;
    }
    show_page(page);
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
    page->type = Normal;
    hash_replace(gWindow->fPageHashTable, (char *)page, ulpage->name);

    cfile = find_fp(ulpage->fpos);


    page->filename = alloc_string(ulpage->fpos.name);
    parse_page(page);
    return page;
}

/* parse the HyperDoc statements in the given string */

void
parse_from_string(char *str)
{
    save_scanner_state();
    last_ch = NoChar;
    last_token = 0;
    input_string = str;
    input_type = openaxiom_FromString_input;
    parse_HyperDoc();
    restore_scanner_state();
}

static void
parse_title(HyperDocPage *page)
{
    TextNode *node;

    Push_MR();
    gParserRegion = Title;
    get_expected_token(openaxiom_Lbrace_token);
    node = alloc_node();
    page->title = node;
    node->type = openaxiom_Titlenode_token;
    node->next = alloc_node();
    node = node->next;
    node->type = openaxiom_Center_token;
    node->next = alloc_node();
    curr_node = node->next;
    parse_HyperDoc();
    curr_node->type = openaxiom_Endcenter_token;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    curr_node->type = openaxiom_Endtitle_token;
    curr_node->next = NULL;
    if (gNeedIconName) {
        char *title = print_to_string(page->title);

        XSetIconName(gXDisplay, gWindow->fMainWindow, title);
        gNeedIconName = 0;
    }
    if (token.type != openaxiom_Rbrace_token) {
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
    gParserRegion = Header;
    node = alloc_node();
    page->header = node;
    node->type = openaxiom_Headernode_token;
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
        gInSpadsrc = FALSE;
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
        gInSpadsrc = FALSE;
    gParserMode = AllMode;
    gParserRegion = Scrolling;

    init_top_group();
    clear_be_stack();

    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    gPageBeingParsed = page;
}

#define end_page(t) \
  ((t == openaxiom_Page_token \
    || t == openaxiom_NewCommand_token \
    ||t == openaxiom_Endpage_token) ? 1 : 0)

static void
parse_page(HyperDocPage *page)
{
    init_parse_page(page);

    /* Get the name of the page */

    get_expected_token(openaxiom_Page_token);
    get_expected_token(openaxiom_Lbrace_token);
    get_expected_token(openaxiom_Word_token);
    if (page->name == NULL)
        page->name = alloc_string(token.id);
    get_expected_token(openaxiom_Rbrace_token);
    /* parse the title */
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
    parse_title(page);

    /*
     * Now start parsing the header region
     */
    parse_header(page);
}

char *ExpectedBeginScroll =
"Parser Error: Unexpected new page, expecting a begin scroll\n", *ExpectedEndScroll =
"Parser Error: Unexpected new page, expected an end scroll\n";

/*
 * The general HyperDoc parsing function.  expects to see anything. This
 * function will parse until it sees either: 1) A new page starting 2) An end
 * of file 3) a closing bracket "}"
 */

void
parse_HyperDoc(void)
{
    TextNode *node = NULL /*, *save_node = NULL, *arg_node = NULL*/ ;

    for(;;) {
        ret_val = get_token();

        if (ret_val == EOF)
            return;

        switch (token.type) {
          case openaxiom_Spadsrc_token:
            parse_spadsrc(curr_node);
            break;
          case openaxiom_Helppage_token:
            parse_help();
            break;
          case openaxiom_Endpatch_token:
          case openaxiom_Endpaste_token:
          case openaxiom_Rbrace_token:
            return;
          case openaxiom_Paste_token:
            parse_paste();
            break;
          case openaxiom_Pastebutton_token:
            parse_pastebutton();
            break;
          case openaxiom_Endpage_token:
          case openaxiom_NewCommand_token:
          case openaxiom_Page_token:
            end_a_page();
            return;
          case openaxiom_EndScroll_token:
            token.type = openaxiom_Endscroll_token;
          case openaxiom_Endscroll_token:
            start_footer();
            break;
          case openaxiom_Beginscroll_token:
            start_scrolling();
            break;
          case openaxiom_Thispage_token:        /* it really is just a word */
            curr_node->type = openaxiom_Word_token;
            curr_node->data.text = alloc_string(gPageBeingParsed->name);
            break;
          case openaxiom_Icorrection_token:
            node->type = openaxiom_Noop_token;
            break;
          case openaxiom_Newcond_token:
            parse_newcond();
            break;
          case openaxiom_Setcond_token:
            parse_setcond();
            break;
          case openaxiom_Dollar_token:
            parse_verbatim(openaxiom_Math_token);
            break;
          case openaxiom_Verbatim_token:
            parse_verbatim(openaxiom_Verbatim_token);
            break;
          case openaxiom_Ifcond_token:
            parse_ifcond();
            break;
          case openaxiom_Fi_token:
            if (gInIf)
                return;
            else {
                curr_node->type = openaxiom_Noop_token;
                /* Oops I had a problem parsing this puppy */
                fprintf(stderr, "(HyperDoc) \\fi found without macthing if?\n");
                longjmp(jmpbuf, 1);
                fprintf(stderr, "(HyperDoc) Longjmp failed -- Exiting \n");
                exit(-1);
            }
          case openaxiom_Else_token:
            if (gInIf)
                return;
            else {
                /* Oops I had a problem parsing this puppy */
                curr_node->type = openaxiom_Noop_token;
                fprintf(stderr, "(HyperDoc) \\else found without macthing if?\n");
                longjmp(jmpbuf, 1);
                fprintf(stderr, "(HyperDoc) Longjmp failed -- Exiting \n");
                exit(-1);
            }
          case openaxiom_Macro_token:
            parse_macro();
            break;
          case openaxiom_Env_token:
            /** In this case, get the environment value, and make it a word **/
            parse_env(curr_node);
            break;
          case openaxiom_WindowId_token:
            curr_node->type = openaxiom_WindowId_token;
            curr_node->space = token.id[-1];
            curr_node->data.text = window_id(gWindow->fMainWindow);
            break;
          case openaxiom_Punctuation_token:
          case openaxiom_Word_token:
          case openaxiom_Lsquarebrace_token:
          case openaxiom_Dash_token:
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            curr_node->data.text = alloc_string(token.id);
            break;
          case openaxiom_Pagename_token:
            {
                char *str;

                curr_node->type = openaxiom_Word_token;
                curr_node->space = 0;
                str = halloc(strlen(cur_page->name) + 1, "parse");
                sprintf(str, "%s", cur_page->name);
                curr_node->data.text = alloc_string(str);
                break;
            }
          case openaxiom_Examplenumber_token:
            {
                char *str;

                curr_node->type = openaxiom_Word_token;
                curr_node->space = 0;
                str = halloc(5, "parse");
                sprintf(str, "%d", example_number);
                curr_node->data.text = alloc_string(str);
                break;
            }
          case openaxiom_Rsquarebrace_token:
            if (gInOptional)
                return;
            else {
                curr_node->type = token.type;
                curr_node->space = token.id[-1];
                curr_node->data.text = alloc_string(token.id);
            }
            break;
          case openaxiom_EndTitems_token:
            token.type = openaxiom_Endtitems_token;
          case openaxiom_Endtitems_token:
            if (gParserMode != AllMode) {
                curr_node->type = openaxiom_Noop_token;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[token.type]);
                longjmp(jmpbuf, 1);
            }
            else {
                curr_node->type = token.type;
                break;
            }
          case openaxiom_EndItems_token:
            token.type = openaxiom_Enditems_token;
          case openaxiom_Enditems_token:
            gInItems--;
          case openaxiom_Horizontalline_token:
          case openaxiom_Par_token:
          case openaxiom_Newline_token:
          case openaxiom_Titem_token:
            if (gParserMode != AllMode) {
                curr_node->type = openaxiom_Noop_token;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[token.type]);
                longjmp(jmpbuf, 1);
            }
            else {
                curr_node->type = token.type;
                break;
            }
          case openaxiom_Begintitems_token:
          case openaxiom_Beginitems_token:
            if (gParserMode != AllMode) {
                curr_node->type = openaxiom_Noop_token;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[token.type]);
                longjmp(jmpbuf, 1);
            }
            else {
                parse_begin_items();
                break;
            }
          case openaxiom_Item_token:
            parse_item();
            break;
          case openaxiom_Mitem_token:
            parse_mitem();
            break;
          case openaxiom_VSpace_token:
          case openaxiom_Tab_token:
          case openaxiom_HSpace_token:
          case openaxiom_Indent_token:
          case openaxiom_Indentrel_token:
            parse_value1();
            break;
          case openaxiom_Space_token:
            parse_value2();
            break;
          case openaxiom_Lbrace_token:
            curr_node->type = openaxiom_Group_token;
            curr_node->space = token.id[-1];
            push_group_stack();
            node = alloc_node();
            curr_node->next = node;
            curr_node = curr_node->next;
            parse_HyperDoc();
            curr_node->type = openaxiom_Endgroup_token;
            pop_group_stack();
            break;
          case openaxiom_Upbutton_token:
          case openaxiom_Returnbutton_token:
          case openaxiom_Link_token:
          case openaxiom_Downlink_token:
          case openaxiom_Memolink_token:
          case openaxiom_Windowlink_token:
            parse_button();
            break;
          case openaxiom_Unixlink_token:
          case openaxiom_LispMemoLink_token:
          case openaxiom_LispDownLink_token:
          case openaxiom_Lisplink_token:
          case openaxiom_Lispcommand_token:
          case openaxiom_Lispcommandquit_token:
          case openaxiom_Spadlink_token:
          case openaxiom_Spaddownlink_token:
          case openaxiom_Spadmemolink_token:
          case openaxiom_Unixcommand_token:
          case openaxiom_Spadcall_token:
          case openaxiom_Spadcallquit_token:
          case openaxiom_Qspadcall_token:
          case openaxiom_Qspadcallquit_token:
          case openaxiom_Lispwindowlink_token:
            parse_command();
            break;
          case openaxiom_Controlbitmap_token:
          case openaxiom_Inputbitmap_token:
          case openaxiom_Inputpixmap_token:
          case openaxiom_Inputimage_token:
            parse_input_pix();
            break;
          case openaxiom_Box_token:
            parse_box();
            break;
          case openaxiom_Mbox_token:
            parse_mbox();
            break;
          case openaxiom_Free_token:
            parse_free();
            break;
          case openaxiom_Center_token:
            parse_centerline();
            break;
          case openaxiom_Bound_token:
            add_dependencies();
            break;
          case openaxiom_Spadcommand_token:
          case openaxiom_Spadgraph_token:
            parse_spadcommand(curr_node);
            break;
          case openaxiom_Table_token:
            parse_table();
            break;
          case openaxiom_Beep_token:
          case openaxiom_Emphasize_token:
          case openaxiom_BoldFace_token:
          case openaxiom_Rm_token:
          case openaxiom_It_token:
          case openaxiom_Tt_token:
          case openaxiom_Sl_token:
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            break;
          case openaxiom_Inputstring_token:
            parse_inputstring();
            break;
          case openaxiom_SimpleBox_token:
            parse_simplebox();
            break;
          case openaxiom_BoxValue_token:
          case openaxiom_StringValue_token:
            if (!gStringValueOk) {
                strcpy(ebuffer,"(HyperDoc): Unexpected Value Command:");
                strcat(ebuffer, token.id);

                parser_error(ebuffer);
                curr_node->type = openaxiom_Noop_token;
                longjmp(jmpbuf, 1);
            }
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            get_expected_token(openaxiom_Lbrace_token);
            get_expected_token(openaxiom_Word_token);
            curr_node->data.text = alloc_string(token.id);
            get_expected_token(openaxiom_Rbrace_token);
            break;
          case openaxiom_NoLines_token:
            gPageBeingParsed->page_flags |= NOLINES;
            break;
          case openaxiom_Pound_token:
            curr_node->type = openaxiom_Pound_token;
            curr_node->space = token.id[-1];
            curr_node->next = alloc_node();
            curr_node = curr_node->next;
            parse_parameters();
            break;
          case openaxiom_Radiobox_token:
            parse_radiobox();
            break;
          case openaxiom_Radioboxes_token:
            parse_radioboxes();
            break;
          case openaxiom_Replacepage_token:
            parse_replacepage();
            break;
          default:
            fprintf(stderr, "(HyperDoc) Keyword not currently supported: %s\n", token.id);
            print_page_and_filename();
            curr_node->type = openaxiom_Noop_token;
            break;
        }
        if (gEndedPage)
            return;
        if (curr_node->type != openaxiom_Noop_token) {
            node = alloc_node();
            curr_node->next = node;
            curr_node = node;
        }
    }
}


/* parse a page from a socket source */

HyperDocPage *
parse_page_from_socket(void)
{
    HyperDocPage *page = alloc_page((char *) NULL);
    HyperDocPage *hpage;

    init_scanner();
    input_type = openaxiom_FromSpadSocket_input;
    input_string = "";
    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable, 
              LinkHashSize, 
              (EqualFunction) window_equal, 
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;
    replace_page = NULL;
    if (setjmp(jmpbuf)) {
        /* Ooops, somewhere I had an error */
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        reset_connection();
    }
    else {
        parse_page(page);
        page->type = SpadGen;
        page->filename = NULL;
        /* just for kicks, let me add this thing to the hash file */
        hpage = (HyperDocPage *) hash_find(gWindow->fPageHashTable, page->name);
        if (hpage)
            hash_replace(gWindow->fPageHashTable, (char *)page, page->name);
        else {
            hash_insert(gWindow->fPageHashTable, (char *)page, page->name);
        }
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
parse_page_from_unixfd(void)
{
    HyperDocPage *page = alloc_page((char *) NULL);

    init_scanner();
    input_type = openaxiom_FromUnixFD_input;
    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable, 
              LinkHashSize, 
              (EqualFunction) window_equal, 
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;
    if (setjmp(jmpbuf)) {
        /* Ooops, somewhere I had an error */
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        reset_connection();
    }
    else {
        parse_page(page);
        page->type = Unixfd;
        page->filename = NULL;
    }
    return page;

}

static void
start_scrolling(void)
{

    /*
     * if I am here than I had a begin scroll. This means I should end the
     * header, and then start parsing the footer
     */

    if (gParserRegion != Header) {
        curr_node->type = openaxiom_Noop_token;
        fprintf(stderr, "(HyperDoc) Parser Error: Unexpected BeginScrollFound\n");
        longjmp(jmpbuf, 1);
        fprintf(stderr, "(HyperDoc) Longjump failed exiting\n");
    }
    curr_node->type = openaxiom_Endheader_token;
    curr_node->next = NULL;
    Pop_MR();

    Push_MR();
    gParserRegion = Scrolling;
    gWindow->fDisplayedWindow = gWindow->fScrollWindow;
    curr_node = alloc_node();
    gPageBeingParsed->scrolling = curr_node;
    curr_node->type = openaxiom_Scrollingnode_token;
}

static void
start_footer(void)
{
    /*
     * This ends the parsing of the scrolling region, and then starts to
     * parse the footer
     */

    if (gParserRegion != Scrolling) {
        curr_node->type = openaxiom_Noop_token;
        fprintf(stderr, "(HyperDoc) Parser Error: Unexpected Endscroll Found\n");
        print_page_and_filename();
        longjmp(jmpbuf, 1);
        fprintf(stderr, "(HyperDoc) Longjump failed exiting\n");
    }

    curr_node->type = openaxiom_Endscrolling_token;
    curr_node->next = NULL;
    Pop_MR();
    linkScrollBars();

    Push_MR();
    gParserRegion = Footer;
    curr_node = alloc_node();
    curr_node->type = openaxiom_Footernode_token;
    gPageBeingParsed->footer = curr_node;
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
}

static void
end_a_page(void)
{
    if (gParserRegion == Scrolling) {
        fprintf(stderr, "%s\n",
                "(HyperDoc) end_a_page: Unexpected End of Page occurred \
                   inside a \beginscroll");
        print_page_and_filename();
        jump();
    }
    gEndedPage = TRUE;
    if (gParserRegion == Footer) {
        /* the person had all the regions, I basically just have to leave */
        curr_node->type = openaxiom_Endscrolling_token;
        curr_node->next = NULL;
        Pop_MR();
    }
    else if (gParserRegion == Header) {
        /* person had a header. So just end it and return */
        curr_node->type = openaxiom_Endheader_token;
        curr_node->next = NULL;
        Pop_MR();
        gPageBeingParsed->scrolling = NULL;
        gPageBeingParsed->footer = NULL;
    }
}

static void
parse_replacepage(void)
{
    get_expected_token(openaxiom_Lbrace_token);
    get_token();
    replace_page = alloc_string(token.id);
    get_expected_token(openaxiom_Rbrace_token);
}
