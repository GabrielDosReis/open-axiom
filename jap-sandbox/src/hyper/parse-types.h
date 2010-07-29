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

#ifndef _PARSE_TYPES_H_
#define _PARSE_TYPES_H_ 1

#include "hyper.h"

extern void parse_begin_items(void );
extern void parse_box(void );
extern void parse_button(void );
extern void parse_centerline(void );
extern void parse_command(void );
extern void parse_env(TextNode * node);
extern void parse_free(void );
extern void parse_help(void );
extern void parse_ifcond(void );
extern void parse_input_pix(void );
extern void parse_item(void );
extern void parse_mbox(void );
extern void parse_mitem(void );
extern void parse_newcond(void );
extern void parse_setcond(void );
extern void parse_spadcommand(TextNode * spad_node);
extern void parse_spadsrc(TextNode * spad_node);
extern void parse_table(void );
extern void parse_value1(void );
extern void parse_value2(void );
extern void parse_verbatim(int type);
extern void show_text(TextNode * node , int Ender);

extern boolean gInButton;
extern boolean gInIf;
extern boolean gInItems;
extern boolean gInOptional;


#endif
