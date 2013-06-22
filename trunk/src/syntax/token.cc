// Copyright (C) 2013, Gabriel Dos Reis.
// All rights reserved.
// Written by Gabriel Dos Reis.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of OpenAxiom. nor the names of its contributors
//       may be used to endorse or promote products derived from this
//       software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <open-axiom/token>

namespace OpenAxiom {
   namespace token {
      struct Keyword {
         const char* const key;
         const Value value;
      };

      const Keyword keyword_map[] = {
         { "add", add_tv },
         { "and", and_tv },
         { "assume", assume_tv },
         { "break", break_tv },
         { "by", by_tv },
         { "case", case_tv },
         { "catch", catch_tv },
         { "do", do_tv },
         { "else", else_tv },
         { "exist", exist_tv },
         { "finally", finally_tv },
         { "for", for_tv },
         { "from", from_tv },
         { "function", function_tv },
         { "has", has_tv },
         { "if", if_tv },
         { "import", import_tv },
         { "in", in_tv },
         { "inline", inline_tv },
         { "is", is_tv },
         { "isnt", isnt_tv },
         { "iterate", iterate_tv },
         { "leave", leave_tv },
         { "macro", macro_tv },
         { "mod", mod_tv },
         { "module", module_tv },
         { "namespace", namespace_tv },
         { "of", of_tv },
         { "or", or_tv },
         { "pretend", pretend_tv },
         { "quo", quo_tv },
         { "rem", rem_tv },
         { "repeat", repeat_tv },
         { "return", return_tv },
         { "rule", rule_tv },
         { "structure", structure_tv },
         { "then", then_tv },
         { "throw", throw_tv },
         { "try", try_tv },
         { "until", until_tv },
         { "with", with_tv },
         { "where", where_tv },
         { "while", while_tv }
      };
   }
}
