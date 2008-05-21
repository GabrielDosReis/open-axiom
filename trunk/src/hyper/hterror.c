/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All right reserved.

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

#define _HTERROR_C
#define HTERROR

#include "openaxiom-c-macros.h"

#include "debug.h"

#include "sockio.h"
#include "lex.h"
#include "parse.h"

#include "all_hyper_proto.H1"

char ebuffer[128];
jmp_buf jmpbuf;

char *errmess[] =  {
  "place holder",
  "parsing condition node",
  "unrecognized keyword"
};

/*
 * This file is the error handling routine in AXIOM. The main routine is
 * called htperror(): arguments: msg - like perror it accepts an error
 * message to be printed errno - the errno which occurred. This is so an
 * appropriate error message can be printed.
 *
 * The prints out the page name, and then the filename in which the error
 * occurred. If possible it also tries to print out the next ten tokens.
 */

void
jump(void)
{
    if (gWindow == NULL)
        exit(-1);
    longjmp(jmpbuf, 1);
    fprintf(stderr, "(HyperDoc) Long Jump failed, Exiting\n");
    exit(-1);
}

void
print_page_and_filename(void)
{
    char obuff[128];

    if (gPageBeingParsed->type == Normal) {

        /*
         * Now try to inform the user as close to possible where the error
         * occurred
         */
        sprintf(obuff, "(HyperDoc) While parsing %s on line %d\n\tin the file %s\n",
                gPageBeingParsed->name, line_number,
                gPageBeingParsed->filename);
    }
    else if (gPageBeingParsed->type == SpadGen) {
        sprintf(obuff, "While parsing %s from the Spad socket\n",
                gPageBeingParsed->name);
    }
    else if (gPageBeingParsed->type == Unixfd) {
        sprintf(obuff, "While parsing %s from a Unixpipe\n",
                gPageBeingParsed->name);
    }
    else {
        /* Unknown page type */
        sprintf(obuff, "While parsing %s\n", gPageBeingParsed->name);
    }
    fprintf(stderr, "%s", obuff);
}


void
print_next_ten_tokens(void)
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

/* print out a token value */
void
print_token(void)
{
    if (token.type == Word)
        printf("%s ", token.id);
    else {
        token_name(token.type);
        printf("\\%s ", ebuffer);
    }
    fflush(stdout);
}


void
token_name(int type)
{
    if (type <= NumberUserTokens)
        strcpy(ebuffer, token_table[type]);
    else {
        switch (type) {
          case Lbrace:
            strcpy(ebuffer, "{");
            break;
          case Rbrace:
            strcpy(ebuffer, "}");
            break;
          case Macro:
            strcpy(ebuffer, token.id);
            break;
          case Group:
            strcpy(ebuffer, "{");
            break;
          case Pound:
            strcpy(ebuffer, "#");
            break;
          case Lsquarebrace:
            strcpy(ebuffer, "[");
            break;
          case Rsquarebrace:
            strcpy(ebuffer, "]");
            break;
          case Punctuation:
            strcpy(ebuffer, token.id);
            break;
          case Dash:
            strcpy(ebuffer, token.id);
            break;
          case Verbatim:
            strcpy(ebuffer, "\\begin{verbatim}");
            break;
          case Scroll:
            strcpy(ebuffer, "\\begin{scroll}");
            break;
          case Dollar:
            strcpy(ebuffer, "$");
            break;
          case Percent:
            strcpy(ebuffer, "%");
            break;
          case Carrot:
            strcpy(ebuffer, "^");
            break;
          case Underscore:
            strcpy(ebuffer, "_");
            break;
          case Tilde:
            strcpy(ebuffer, "~");
            break;
          case Cond:
            sprintf(ebuffer, "\\%s", token.id);
            break;
          case Icorrection:
            strcpy(ebuffer, "\\/");
            break;
          case Paste:
            strcpy(ebuffer, "\\begin{paste}");
            break;
          case Patch:
            strcpy(ebuffer, "\\begin{patch}");
            break;
          default:
            sprintf(ebuffer, " %d ", type);
        }
        /*return 1;*/
    }
}
void
htperror(char *msg, int erno)
{
    char obuff[256];

    /* The first thing I do is create the error message */

    if (erno <= Numerrors) {
        sprintf(obuff, "%s:%s\n", msg, errmess[errno]);
    }
    else {
        sprintf(obuff, "%s:\n", msg);
        fprintf(stderr, "Unknown error type %d\n", erno);
    }
    fprintf(stderr, "%s", obuff);

    print_page_and_filename();

    print_next_ten_tokens();
}
