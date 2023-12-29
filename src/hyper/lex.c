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

/*
 * Lexical analyzer stuff. Exported functions: 
 * -- parser_init(): 
 *    initialize the parser tables with keywords 
 * -- init_scanner():
 *    initialize scanner for reading a new page 
 * -- get_token():
 *    sets the "token" variable to be the next token in the current input
 *    stream 
 * -- OpenAxiom::IOStateManager:
 *    save the current state of scanner so that the scanner input mode may 
 *    be switched and then undo the saved state
 *
 * Note: The scanner reads from three seperate input locations depending on the
 * value of the variable "input_type".  If this variable is:
 *
 * -- FromFile: it read from the file pointed to by "cfile".
 * -- FromString: It reads from the string "input_string". 
 * -- FromSpadSocket: It reads from the socket pointed to by spad_socket.
 * -- FromFD: It reads from a file descriptor.
 *
 */
#define _LEX_C
#include "openaxiom-c-macros.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stack>
#include <vector>

#include "debug.h"
#include "sockio.h"

#define PARSER 1

#include "halloc.h"
#include "lex.h"
#include "node.h"
#include "cfuns.h"

using namespace OpenAxiom;

static int keyword_type(void );

extern int gTtFontIs850;
extern HDWindow *gWindow;

/** I am implementing a state node stack, this is the structure I store **/

struct IOState {
   int last_ch;
   TokenType last_token;
   int line_number;
   SourceInputKind input_type;
   long fpos, keyword_fpos;
   long page_start_fpos;
   Token token;
   char *input_string;
   FILE *cfile;
   int keyword;
};

using IOStateStack = std::stack<IOState, std::vector<IOState>>;

/** The stack of IOStates. **/
static IOStateStack io_states;

HyperDocPage *gPageBeingParsed;      /* page currently being parsed    */
char ebuffer[128];
short int gInSpadsrc = 0;
short int gInVerbatim;
openaxiom_sio* spad_socket;

/* Parser variables */
long fpos;                      /* Position of pointer in file in characters */
long page_start_fpos;           /* where the current pages fpos started      */
long keyword_fpos;              /* fpos of beginning of most recent keyword */
Token token;                    /* most recently read token */
TokenType last_token;           /* most recently read token for unget_token */
SourceInputKind input_type;                 /* indicates where to read input */
char *input_string;             /* input string read when from_string is true */
int last_ch;                    /* last character read, for unget_char */
int last_command;               /* the last socket command */
int keyword;                    /* the last command was a keyword, or a group */
int cfd;                        /* current file decriptor */
FILE *cfile;                    /* currently active file pointer */
FILE *unixfd;
int line_number;

char sock_buf[1024];            /* buffer for socket input */

#define TokenHashSize   100

static HashTable tokenHashTable;           /* hash table of parser tokens */

const char* token_table[] = {
  "",           /* Dummy token name */
  "word",
  "page",
  "lispcommandquit",
  "bf",
  "link",
  "downlink",
  "beginscroll",
  "spadcommand",
  "nolines",
  "env",
  "par",
  "centerline",
  "begin",
  "beginitems",
  "item",
  "table",
  "fbox",
  "tab",
  "space",
  "indent",
  "horizontalline",
  "newline",
  "enditems",
  "returnbutton",
  "memolink",
  "upbutton",
  "endscroll",
  "thispage",
  "returnto",
  "free",
  "bound",
  "lisplink",
  "unixlink",
  "mbox",
  "inputstring",
  "stringvalue",
  "spadlink",
  "inputbitmap",
  "inputpixmap",
  "unixcommand",
  "em",
  "lispcommand",
  "lispmemolink",
  "lispdownlink",
  "spadcall",
  "spadcallquit",
  "spaddownlink",
  "spadmemolink",
  "qspadcall",
  "qspadcallquit",
  "inputbox",
  "radioboxes",
  "boxvalue",
  "vspace",
  "hspace",
  "newcommand",
  "windowid",
  "beep",
  "quitbutton",
  "begintitems",
  "titem",
  "end",
  "it",
  "sl",
  "tt",
  "rm",
  "ifcond",
  "else",
  "fi",
  "newcond",
  "setcond" ,
  "button",
  "windowlink",
  "haslisp",
  "hasup",
  "hasreturn",
  "hasreturnto",
  "lastwindow",
  "endtitems",
  "lispwindowlink",
  "beginpile",
  "endpile",
  "nextline",
  "pastebutton",
  "color",
  "helppage",
  "patch",
  "radiobox",
  "ifrecond",
  "math",
  "mitem",
  "pagename",
  "examplenumber",
  "replacepage",
  "inputimage",
  "spadgraph",
  "indentrel",
  "controlbitmap"
  };

void
dumpToken(char *caller, Token t)
{ fprintf(stderr,"%s:dumpToken type=%s id=%s\n",
    caller,token_table[OpenAxiom::rep(t.type)], t.id);
}


/* initialize the parser keyword hash table */
void
parser_init()
{
    int i;
    Token *toke;

    /* First I initialize the hash table for the tokens */

    hash_init(
              &tokenHashTable, 
              TokenHashSize, 
              (EqualFunction)string_equal, 
              (HashcodeFunction)string_hash);
    for (i = 2; i <= OpenAxiom::rep(TokenType::NumberUserTokens); i++) {
        toke = (Token *) halloc(sizeof(Token), "Token");
        toke->type = TokenType{i};
        toke->id = token_table[i];
        hash_insert(&tokenHashTable, (char *)toke, toke->id);
    }

}

/* initialize the lexical scanner to read from a file */
void
init_scanner()
{
    keyword = 0;
    last_ch = NoChar;
    last_token = {};
    input_type = SourceInputKind::File;
    fpos = 0;
    keyword_fpos = 0;
    last_command = -1;
    line_number = 1;
}

/*
 * variables to save current state of scanner.  Currently only one level of
 * saving is allowed.  In the future we should allow nested saves
 */

/* save the current state of the scanner */
OpenAxiom::IOStateManager::IOStateManager()
{
    io_states.push({});
    auto new_item = &io_states.top();
    new_item->page_start_fpos = page_start_fpos;
    new_item->fpos = fpos;
    new_item->keyword_fpos = keyword_fpos;
    new_item->line_number = line_number;
    new_item->last_ch = last_ch;
    new_item->last_token = last_token;
    new_item->token = token;
    new_item->input_type = input_type;
    new_item->input_string = input_string;
    new_item->cfile = cfile;
    new_item->keyword = keyword;
}

/* restore the saved scanner state */
OpenAxiom::IOStateManager::~IOStateManager()
{
    if (io_states.empty()) {
        fprintf(stderr, "Restore Scanner State: State empty\n");
        exit(-1);
    }
    auto x = io_states.top();
    io_states.pop();
    page_start_fpos = x.page_start_fpos;
    fpos = x.fpos;
    keyword_fpos = x.keyword_fpos;
    line_number = x.line_number;
    last_ch = x.last_ch;
    last_token = x.last_token;
    token = x.token;
    input_type = x.input_type;
    input_string = x.input_string;
    cfile = x.cfile;
    keyword = x.keyword;
    if (cfile != NULL)
        fseek(cfile, fpos + page_start_fpos, 0);
}

/* return the character to the input stream. */
void
unget_char(int c)
{
    if (c == '\n')
        line_number--;
    last_ch = c;
}

/* return the next character in the input stream */
int get_char()
{
    int c;
    int cmd;

    if (last_ch != NoChar) {
        c = last_ch;
        last_ch = NoChar;
        if (c == '\n')
            line_number++;
        return c;
    }
    switch (input_type) {
    case SourceInputKind::UnixFD:
        c = getc(unixfd);
        if (c == '\n')
            line_number++;
        return c;
    case SourceInputKind::String:
        c = (*input_string ? *input_string++ : EOF);
        if (c == '\n')
            line_number++;
        return c;
    case SourceInputKind::File:
        c = getc(cfile);
        fpos++;
        if (c == '\n')
            line_number++;
        return c;
    case SourceInputKind::SpadSocket:
AGAIN:
        if (*input_string) {
            /* this should never happen for the first character */
            c = *input_string++;
            if (c == '\n')
                line_number++;
            return c;
        }
        if (last_command == EndOfPage)
            return EOF;
        if (spad_socket->nbytes_pending == 0) {
            last_command = cmd = get_int(spad_socket);
            if (cmd == EndOfPage)
                return EOF;
            if (cmd == SpadError)
                throw HyperError{};
        }
        get_string_buf(spad_socket, sock_buf, 1023);
        /* this will be null if this is the last time*/
        input_string = sock_buf;
        goto AGAIN;
      default:
        fprintf(stderr, "Get Char: Unknown type of input: %d\n", input_type);
        return -1;
    }
}


#define special(c) ((c) == '{' || (c) == '}' || (c) == '#' || (c) == '%' || \
                    (c) == '\\'  || (c) == '[' || (c) == ']' || (c) == '_' || \
                    (c) == ' ' || (c) == '$' || (c) == '~' || (c) == '^' ||  \
                    (c) == '&')

#define punctuation(c) ((c)== '`' || (c) == '\'' || (c) == ','  || \
                        (c) == '.' || (c) == '?' || (c) == '"' || \
                        (c)  == ';' || (c) == ':' || (c) == '-')

#define whitespace(c) ((c) == ' ' || (c) == '\t' || (c) == '\n')
#define delim(c) \
  (whitespace(c) || special(c) || punctuation(c))



Token unget_toke;

/* return current token to the input stream */
void
unget_token()
{
    last_token = TokenType::Word;
    unget_toke.type = token.type;
    unget_toke.id = alloc_string(token.id - 1);
}


int
get_token()
{
    int c, ws;
    int nls = 0;
    static int seen_white = 0;
    static char buffer[1024];
    char *buf = buffer;

    if (last_token != TokenType{}) {
        last_token = {};
        token.type = unget_toke.type;
        strcpy(buffer, unget_toke.id);
        free((char*) unget_toke.id);
        token.id = buffer + 1;
        if (OpenAxiom::rep(token.type) == EOF)
            return EOF;
        else
            return 0;
    }
    seen_white = nls = 0;
    do {
        c = get_char();
        ws = whitespace(c);
        if (ws)
            seen_white++;
        if (c == '\n') {
            if (nls) {
                token.type = TokenType::Par;
                return 0;
            }
            else
                nls++;
        }
    } while (ws);

    /* first character of string indicates number of spaces before token */

    if (!keyword)
        *buf++ = seen_white;
    else
        *buf++ = 0;

    keyword = 0;
    if (input_type != SourceInputKind::SpadSocket && c == '%') {
        while ((c = get_char()) != '\n' && c != EOF);
/* trying to fix the comment problem: a comment line forces words on either side together*/
/* try returning the eol */
        unget_char(c);
        return get_token();
    }
    if (input_type == SourceInputKind::File && c == '$') {
        token.type = TokenType::Dollar;
        return 0;
    }
    switch (c) {
      case EOF:
        token.type = TokenType{-1};
        return EOF;
      case '\\':
        keyword_fpos = fpos - 1;
        c = get_char();
        if (!isalpha(c)) {
            *buf++ = c;
            token.type = TokenType::Word;
            *buf = '\0';
            seen_white = 0;
        }
        else {
            do {
                *buf++ = c;
            } while ((c = get_char()) != EOF && isalpha(c));

            unget_char(c);
            *buf = '\0';
            keyword = 1;
            token.id = buffer + 1;
            return (keyword_type());
        }
        break;
      case '{':
        token.type = TokenType::Lbrace;
        break;
      case '}':
        token.type = TokenType::Rbrace;
        break;
      case '[':
        token.type = TokenType::Lsquarebrace;
        *buf++ = c;
        *buf = '\0';
        token.id = buffer + 1;
        break;
      case ']':
        token.type = TokenType::Rsquarebrace;
        *buf++ = c;
        *buf = '\0';
        token.id = buffer + 1;
        break;
      case '#':
        token.type = TokenType::Pound;

        /*
         * if I get a pound then what I do is parse until I get something
         * that is not an integer
         */
        c = get_char();
        while (isdigit(c) && (c != EOF)) {
            *buf++ = c;
            c = get_char();
        }
        unget_char(c);
        *buf = '\0';
        token.id = buffer + 1;
        break;
      case '`':
      case '\'':
      case ',':
      case '.':
      case '!':
      case '?':
      case '"':
      case ':':
      case ';':
        token.type = TokenType::Punctuation;
        *buf++ = c;
        *buf = '\0';
        /** Now I should set the buffer[0] as my flag for whether I had
          white-space in front of me, and whether I had white space
          behind me **/
        if (buffer[0])
            buffer[0] = FRONTSPACE;
        c = get_char();
        if (whitespace(c))
            buffer[0] |= BACKSPACE;
        unget_char(c);
        token.id = buffer + 1;
        break;
      case '-':
        do {
            *buf++ = c;
        } while (((c = get_char()) != EOF) && (c == '-'));
        unget_char(c);
        *buf = '\0';
        token.type = TokenType::Dash;
        token.id = buffer + 1;
        break;
      default:
        do {
            *buf++ = c;
        } while ((c = get_char()) != EOF && !delim(c));
        unget_char(c);
        *buf = '\0';
        token.type = TokenType::Word;
        token.id = buffer + 1;
        break;
    }
/*    dumpToken("get_token",token);*/
    return 0;
}


/*
 * Here are the structures and stuff needed for the begin and end routines.
 * The stack stores the begin types that have been seen and the end
 * pops them off and checks to insure that they are reversed properly.
 */

typedef struct be_struct {
    TokenType type;
    char *id;
    struct be_struct *next;
}   BeStruct;

BeStruct *top_be_stack;

void
jump()
{
    if (gWindow == NULL)
        exit(-1);
    throw HyperError{};
}

void
push_be_stack(TokenType type, const char* id)
{
    BeStruct *be = (BeStruct *) halloc(sizeof(BeStruct), "BeginENd stack");

    if (gWindow != NULL) {
        be->type = type;
        be->next = top_be_stack;
        be->id = alloc_string(id);
        top_be_stack = be;
    }
    return;
}
void
check_and_pop_be_stack(TokenType type, const char* id)
{
    BeStruct *x;

    /*
     * this routine pops the be stack and compares types. If they are
     * the same then I am okay and return a 1. Else I return a two and try to
     * print a meaningful message
     */
    if (gWindow == NULL)
        return;
    if (top_be_stack == NULL) { /* tried to pop when I shouldn't have */
        fprintf(stderr, "Unexpected \\end{%s} \n", token.id);
        print_page_and_filename();
        print_next_ten_tokens();
        jump();
    }
    x = top_be_stack;
    if (x->type == type) {
        top_be_stack = top_be_stack->next;
        free(x->id);
        free(x);
        return;
    }
    /* else I didn't have a match. Lets try to write a sensible message */
    fprintf(stderr, "\\begin{%s} ended with \\end{%s} \n", x->id, id);
    print_page_and_filename();
    print_next_ten_tokens();
    jump();
}

int
clear_be_stack()
{
    BeStruct *x = top_be_stack, *y;

    top_be_stack = NULL;
    while (x != NULL) {
        y = x->next;
        free(x);
        x = y;
    }
    return 1;
}

int
be_type(const char* which)
{
    Token store;

    get_expected_token(TokenType::Lbrace);
    get_expected_token(TokenType::Word);
    switch (token.id[0]) {
      case 't':
        if (!strcmp(token.id, "titems")) {
            token.type = TokenType::Begintitems;
        }
        else {
            return -1;
        }
        break;
      case 'p':
        if (!strcmp(token.id, "page")) {
            token.type = TokenType::Page;
        }
        else if (!strcmp(token.id, "paste")) {
            token.type = TokenType::Paste;
        }
        else if (!strcmp(token.id, "patch")) {
            token.type = TokenType::Patch;
        }
        else {
            return -1;
        }
        break;
      case 'v':         /* possibly a verbatim mode */
        if (!strcmp(token.id, "verbatim")) {
            token.type = TokenType::Verbatim;
        }
        else {
            return -1;
        }
        break;
      case 's':         /* possibly a scroll mode */
        if (!strcmp("scroll", token.id)) {
            token.type = TokenType::Beginscroll;
        }
        else if (!strcmp(token.id, "spadsrc")) {
            token.type = TokenType::Spadsrc;
        }
        else {
            return -1;
        }
        break;
      case 'i':         /* possibly a item */
        if (!strcmp("items", token.id)) {
            token.type = TokenType::Beginitems;
        }
        else {
            return -1;
        }
        break;
      default:
        return -1;
    }
    store.type = token.type;
    /* store.id = alloc_string(token.id); */
    get_expected_token(TokenType::Rbrace);
    token.type = store.type;

    /*
     * strcpy(token.id, store.id); free(store.id);
     */
    return 0;

}
int
begin_type()
{
    /*Token store;*/
    int ret_val;

    /*
     * This routine parses a statement of the form \begin{word}. Once it has
     * read the word it tries to assign it a type. Once that is done it sends
     * the word id, and the type to push_be_stack and then returns the type.
     * For the moment I amnot even going to use a has_table, although in the
     * future this may be needed
     */
    ret_val = be_type("begin");
    if (ret_val == -1) {
        if (gWindow == NULL || gInVerbatim)
            return 1;
        else {
            fprintf(stderr, "Unknown begin type \\begin{%s} \n", token.id);
            print_page_and_filename();
            print_next_ten_tokens();
            jump();
        }
    }
    else {
        if (gWindow != NULL && !gInVerbatim
            && token.type != TokenType::Verbatim
            && token.type != TokenType::Spadsrc) {
            /* Now here I should push the needed info and then get */
            push_be_stack(token.type, token.id);
        }
        return 1;
    }
    return 1;
}

// Return the closing matching end token type of the argument.
static constexpr TokenType matching_end(TokenType t)
{
    return TokenType{OpenAxiom::rep(t) + 3000};
}

int
end_type()
{
    int ret;

    /*
     * This routine gets the end type just as the begin_type routine does,
     * But then it checks to see if recieved the proper end_type. By a clever
     * trick, the proper end type is 3000 + type. When environments this will
     * have to change
     */
    ret = be_type("end");
    if (ret == -1) {
        /* unrecognized end token */
        if (gWindow == NULL || gInVerbatim) {
            return 1;
        }
        else {
            fprintf(stderr, "Unknown begin type \\begin{%s} \n", token.id);
            print_page_and_filename();
            print_next_ten_tokens();
            jump();
        }
    }
    else {
        if (gWindow != NULL && !gInVerbatim) {
            check_and_pop_be_stack(token.type, token.id);
            token.type = matching_end(token.type);
            return 1;
        }
        else {
            if (gWindow != NULL
                && ((gInVerbatim && token.type == TokenType::Verbatim)
                    || (gInSpadsrc && token.type == TokenType::Spadsrc))) {
                check_and_pop_be_stack(token.type, token.id);
                token.type = matching_end(token.type);
                return 1;
            }
            else {
                token.type = matching_end(token.type);
                return 1;
            }
        }
    }
    return 1;
}


void
token_name(TokenType type)
{
    if (type <= TokenType::NumberUserTokens)
        strcpy(ebuffer, token_table[OpenAxiom::rep(type)]);
    else {
        switch (type) {
          case TokenType::Lbrace:
            strcpy(ebuffer, "{");
            break;
          case TokenType::Rbrace:
            strcpy(ebuffer, "}");
            break;
          case TokenType::Macro:
            strcpy(ebuffer, token.id);
            break;
          case TokenType::Group:
            strcpy(ebuffer, "{");
            break;
          case TokenType::Pound:
            strcpy(ebuffer, "#");
            break;
          case TokenType::Lsquarebrace:
            strcpy(ebuffer, "[");
            break;
          case TokenType::Rsquarebrace:
            strcpy(ebuffer, "]");
            break;
          case TokenType::Punctuation:
            strcpy(ebuffer, token.id);
            break;
          case TokenType::Dash:
            strcpy(ebuffer, token.id);
            break;
          case TokenType::Verbatim:
            strcpy(ebuffer, "\\begin{verbatim}");
            break;
          case TokenType::Scroll:
            strcpy(ebuffer, "\\begin{scroll}");
            break;
          case TokenType::Dollar:
            strcpy(ebuffer, "$");
            break;
          case TokenType::Percent:
            strcpy(ebuffer, "%");
            break;
          case TokenType::Carrot:
            strcpy(ebuffer, "^");
            break;
          case TokenType::Underscore:
            strcpy(ebuffer, "_");
            break;
          case TokenType::Tilde:
            strcpy(ebuffer, "~");
            break;
          case TokenType::Cond:
            sprintf(ebuffer, "\\%s", token.id);
            break;
          case TokenType::Icorrection:
            strcpy(ebuffer, "\\/");
            break;
          case TokenType::Paste:
            strcpy(ebuffer, "\\begin{paste}");
            break;
          case TokenType::Patch:
            strcpy(ebuffer, "\\begin{patch}");
            break;
          default:
            sprintf(ebuffer, " %d ", type);
        }
    }
}


/* print out a token value */
void
print_token()
{
    if (token.type == TokenType::Word)
        printf("%s ", token.id);
    else {
        token_name(token.type);
        printf("\\%s ", ebuffer);
    }
    fflush(stdout);
}

void
print_next_ten_tokens()
{
    int i;
    int v;

    fprintf(stderr, "Trying to print the next ten tokens\n");
    for (i = 0; i < 10; i++) {
        v = get_token();
        if (v == EOF)
            break;
        print_token();
    }
    fprintf(stderr, "\n");
}

void
print_page_and_filename()
{
    char obuff[128];

    if (gPageBeingParsed->type == TokenType::Normal) {

        /*
         * Now try to inform the user as close to possible where the error
         * occurred
         */
        sprintf(obuff, "(HyperDoc) While parsing %s on line %d\n\tin the file %s\n",
                gPageBeingParsed->name, line_number,
                gPageBeingParsed->filename);
    }
    else if (gPageBeingParsed->type == TokenType::SpadGen) {
        sprintf(obuff, "While parsing %s from the Spad socket\n",
                gPageBeingParsed->name);
    }
    else if (gPageBeingParsed->type == TokenType::Unixfd) {
        sprintf(obuff, "While parsing %s from a Unixpipe\n",
                gPageBeingParsed->name);
    }
    else {
        /* Unknown page type */
        sprintf(obuff, "While parsing %s\n", gPageBeingParsed->name);
    }
    fprintf(stderr, "%s", obuff);
}


static int
keyword_type()
{
    Token *token_ent;

    /* first check to see if it is a reserved token */
    token_ent = (Token *) hash_find(&tokenHashTable, token.id);
    if (token_ent != NULL) {
        token.type = token_ent->type;

        /*
         * if I am a keyword I also have to check to see if I am a begin or
         * an end
         */
        if (token.type == TokenType::Begin)
            return begin_type();
        if (token.type == TokenType::End)
            return end_type();
        /* next check to see if it is a macro */
    }
    else if (gWindow != NULL) {
        if (hash_find(gWindow->fMacroHashTable, token.id) != NULL)
            token.type = TokenType::Macro;
        else if (gPageBeingParsed->box_hash != NULL &&
                 hash_find(gPageBeingParsed->box_hash, token.id) != NULL)
        {
            token.type = TokenType::Boxcond;
        }
        else if (hash_find(gWindow->fCondHashTable, token.id) != NULL)
            token.type = TokenType::Cond;
        else                    /* We have no idea what we've got */
            token.type = TokenType::Unkeyword;
    }
    else {                      /* We am probably in htadd so just return. It
                                 * is only concerned with pages anyway */
        token.type = TokenType::Unkeyword;
    }
    return 0;
}

/* read a token, and report a syntax error if it has the wrong type */
void
get_expected_token(TokenType type)
{
    get_token();
    if (token.type != type) {
        token_name(type);
        fprintf(stderr, "syntax error: expected a %s\n", ebuffer);
        if (token.type == TokenType{EOF}) {
            print_page_and_filename();
            fprintf(stderr, "Unexpected EOF\n");
        }
        else {
            token_name(token.type);
            fprintf(stderr, "not a %s\n", ebuffer);
            print_page_and_filename();
            print_next_ten_tokens();
        }
        throw HyperError{};
    }
}
