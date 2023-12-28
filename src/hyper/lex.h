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

#ifndef _LEX_H_
#define _LEX_H_ 1

#include "token.h"

#define HTCONDNODE 1    /* unrecognized condition node */
#define KEYTYPE    2    /* unrecognized keyword found in lex.c */
#define Numerrors  2

namespace OpenAxiom {
  // Type of IO state stack.  Used to automate save/restore of IO states.
  struct IOStateManager {
    IOStateManager();
    IOStateManager(const IOStateManager&) = delete;
    IOStateManager(IOStateManager&&) = delete;
    ~IOStateManager();
  };
}

extern void get_expected_token(int);
extern void parser_init();
extern void init_scanner();
extern void unget_char(int);
extern int get_char();
extern void unget_token();
extern int get_token();
extern void push_be_stack(int, const char*);
extern void check_and_pop_be_stack(int, const char*);
extern int clear_be_stack();
extern int be_type(const char*);
extern int begin_type();
extern int end_type();
extern void reset_connection();

extern void print_page_and_filename();
extern void jump();
extern void print_token();
extern void token_name(int);
extern void print_next_ten_tokens();

extern long fpos, keyword_fpos;
extern Token token;
extern int last_token, last_ch;
extern SourceInputKind input_type;
extern char *input_string;
extern FILE *cfile;
extern short int gInSpadsrc;
extern short int gInVerbatim;


#endif
