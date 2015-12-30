// -*- C++ -*-
// Copyright (C) 2014-2015, Gabriel Dos Reis.
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

// --% Author: Gabriel Dos Reis
// --% Description:

#include <open-axiom/diagnostics>
#include <open-axiom/InputFragment>
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <stack>
#include <iterator>
#include <ctype.h>
#include <open-axiom/SourceInput>

using namespace OpenAxiom;

// 
// -- Decomposing source files into lexical units of information --
// 

static std::ostream&
operator<<(std::ostream& os, const Token& t) {
   os << t.category << '{' << t.start << '-' << t.end << '}';
   return os;
}

using TokenSequence = OpenAxiom::TokenStream<Token>;

// --

namespace {
   struct Parser {
      TokenSequence* tokens;
   };
}

// --

static void
translate_source_file(SourceInput& src, std::ostream& out, const char* path) {
   while (auto f = src.get()) {
      out << "================================================\n";
      out << f;
      try {
         TokenSequence ts { f, OpenAxiom::Language::Boot };
         for (auto& t : ts) {
            out << '\t' << t;
            switch (t.category) {
            case TokenCategory::Junk:
            case TokenCategory::Unclassified:
               out //<< f[t.start.line].sub_string(t.start.column, t.end.column)
                   << " in file " << path
                   << " at line " << t.start.line
                   << ", column " << t.start.column;
               break;
            default:
               break;
            }
            out << '\n';
         }
      }
      catch(const OpenAxiom::EndOfStringUnseen& e) {
         std::cerr << path << ": syntax error: "
                   << "premature end of line before matching quote "
                   << "of string literal on line " << e.line
                   << " at column " << e.column
                   << std::endl;
      }
      catch (const OpenAxiom::MissingExponent& e) {
         std::cerr << path << ": syntax error: "
                   << "missing exponent of floating point constant "
                   << "on line " << e.line
                   << ", column " << e.column
                   << std::endl;
      }
      out << "================================================\n";
   }
   out << std::flush;
}

static void
process_file(const char* path) {
   std::ifstream in { path };
   if (!in) {
      std::cerr << "error: could not open file `" << path << "'"
                << std::endl;
      return;
   }
   SourceInput src { in };
   translate_source_file(src, std::cout, path);
}

int main(int argc, char* argv[]) {
   for (int i = 1; i < argc; ++i) {
      process_file(argv[i]);
   }
}
