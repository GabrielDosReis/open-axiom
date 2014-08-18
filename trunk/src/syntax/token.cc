// Copyright (C) 2013-2014, Gabriel Dos Reis.
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
         { "add", Value::Add },
         { "and", Value::And },
         { "assume", Value::Assume },
         { "break", Value::Break },
         { "by", Value::By },
         { "case", Value::Case },
         { "catch", Value::Catch },
         { "do", Value::Do },
         { "else", Value::Else },
         { "exist", Value::Exists },
         { "finally", Value::Finally },
         { "for", Value::For },
         { "from", Value::From },
         { "function", Value::Function },
         { "has", Value::Has },
         { "if", Value::If },
         { "import", Value::Import },
         { "in", Value::In },
         { "inline", Value::Inline },
         { "is", Value::Is },
         { "isnt", Value::Isnt },
         { "iterate", Value::Iterate },
         { "leave", Value::Leave },
         { "macro", Value::Macro },
         { "mod", Value::Mod },
         { "module", Value::Module },
         { "namespace", Value::Namespace },
         { "of", Value::Of },
         { "or", Value::Or },
         { "pretend", Value::Pretend },
         { "quo", Value::Quo },
         { "rem", Value::Rem },
         { "repeat", Value::Repeat },
         { "return", Value::Return },
         { "rule", Value::Rule },
         { "structure", Value::Structure },
         { "then", Value::Then },
         { "throw", Value::Throw },
         { "try", Value::Try },
         { "until", Value::Until },
         { "with", Value::With },
         { "where", Value::Where },
         { "while", Value::While }
      };
   }
}
