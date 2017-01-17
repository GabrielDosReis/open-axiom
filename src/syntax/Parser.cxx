// -*- C++ -*-
// Copyright (C) 2014-2017, Gabriel Dos Reis.
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

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <stack>
#include <iterator>
#include <ctype.h>

#include <open-axiom/diagnostics>
#include <open-axiom/InputFragment>
#include <open-axiom/SourceInput>
#include <open-axiom/token>
#include <open-axiom/Parser>

namespace {
   using namespace OpenAxiom;

   using TokenSequence = TokenStream<Token>;

   struct ParsingContext {
      explicit ParsingContext(TokenSequence& ts)
            : tokens{ ts }, position{ }
      { }

      bool exhausted() const {
         return position >= tokens.size();
      }

      const Token* current_token() const {
         if (exhausted())
            return nullptr;
         return &tokens[position];
      }

      void advance() { ++position; }

   private:
      TokenSequence& tokens;
      TokenSequence::size_type position;
   };

   const Token* next_token(ParsingContext& ctx) {
      while (auto t = ctx.current_token()) {
         switch (t->category) {
         case TokenCategory::Whitespace:
            break;

         case TokenCategory::Comment:
            if (t->value == TokenValue::Wisecrack)
               break;

         default:
            return t;
         }
         ctx.advance();
      }
      return nullptr;
   }

   // Simple wrapper around standard file streams, along with the pathname
   // to the file.
   template<typename T>
   struct FileAs {
      const char* path;
      T stream;
      FileAs(const char* p, std::ios_base::openmode flags)
            : path{ p }, stream{ p, flags }
      {
         if (!stream)
            throw Error::CannotOpenFile{ path };
      }
   };

   using InputFile = FileAs<std::ifstream>;
   using OutputFile = FileAs<std::ofstream>;

   // Helper function for streaming out details of tokens.
   std::ostream& operator<<(std::ostream& os, const Token& t) {
      os << t.category << '{'
         << t.start << '-' << t.end
         << ", " << t.value
         << '}';
      return os;
   }

   // FIXME: This is just a stub to get a native parsing entry point
   //        into the bootsys and interpsys images.
   void transpile_boot_to_lisp(InputFile& in, OutputFile& out) {
      out.stream << "## Input: " << in.path << '\n'
                 << "## Output: " << out.path << '\n';

      SourceInput src { in.stream };
      while (auto f = src.get()) {
         out.stream << "================================================\n";
         out.stream << f;
         try {
            TokenSequence ts { f, Language::Boot };
            for (auto& t : ts) {
               out.stream << '\t' << t;
               switch (t.category) {
               case TokenCategory::Junk:
               case TokenCategory::Unclassified:
                  out.stream //<< f[t.start.line].sub_string(t.start.column, t.end.column)
                     << " in file " << in.path
                     << " at line " << t.start.line
                     << ", column " << t.start.column;
                  break;
               default:
                  break;
               }
               out.stream << '\n';
            }
         }
         catch(const EndOfStringUnseen& e) {
            std::cerr << in.path << ": syntax error: "
                      << "premature end of line before matching quote "
                      << "of string literal on line " << e.line
                      << " at column " << e.column
                      << std::endl;
         }
         catch (const MissingExponent& e) {
            std::cerr << in.path << ": syntax error: "
                      << "missing exponent of floating point constant "
                      << "on line " << e.line
                      << ", column " << e.column
                      << std::endl;
         }
         out.stream << "================================================\n";
      }
      out.stream << std::flush;
   }
}

namespace OpenAxiom {

   int boot_to_lisp(const char* boot_path, const char* lisp_path) try {
      InputFile in { boot_path, std::ios_base::binary };
      OutputFile out { lisp_path, std::ios_base::binary };
      transpile_boot_to_lisp(in, out);
      return 0;
   }
   catch (const Error::CannotOpenFile& e) {
      Diagnostics::StandardStream diagnostics { };
      diagnostics.error() << "error: could not open file `"
                          << e.path << "'\n";
      return -1;
   }
}
