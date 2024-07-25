// Copyright (C) 2013-2024, Gabriel Dos Reis.
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
#include <ostream>
#include <iostream>

namespace OpenAxiom {
   std::ostream& operator<<(std::ostream& os, TokenCategory tc) {
      switch (tc) {
      case TokenCategory::Unclassified: os << "UNCLASSIFIED"; break;
      case TokenCategory::Whitespace: os << "WHITESPACE"; break;
      case TokenCategory::Comment: os << "COMMENT"; break;
      case TokenCategory::Punctuator: os << "PUNCTUATOR"; break;
      case TokenCategory::Operator: os << "OPERATOR"; break;
      case TokenCategory::Integer: os << "INTEGER"; break;
      case TokenCategory::FloatingPoint: os << "FLOATINGPOINT"; break;
      case TokenCategory::String: os << "STRING"; break;
      case TokenCategory::Keyword: os << "KEYWORD"; break;
      case TokenCategory::Identifier: os << "IDENTIFIER"; break;
      case TokenCategory::Formatting: os << "FORMATTING"; break;
      case TokenCategory::Junk: os << "JUNK"; break;
      default: os << "????"; break;
      }
      return os;
   }


   bool separator_or_punctuator(uint8_t c) {
      switch (c) {
      case '.': case '`': case '^': case '&': case '~': case '*':
      case '-': case '+': case ';': case ',': case '@': case '|':
      case '\'': case ':': case '=': case '\\': case '"': case '/':
      case '(': case ')': case '{': case '}': case '[': case ']':
      case '<': case '>': case '#': case ' ':
         return true;
      default:
         return false;
      }
   }

   namespace {
      struct TokenMapEntry {
         const char* const text;
         const TokenCategory category;
         const TokenValue value;
         const Language dialect; //  = Language::Spad
      };
   }

   const TokenMapEntry token_map[] {
#undef OPENAXIOM_DEFINE_TOKEN
#define OPENAXIOM_DEFINE_TOKEN(T, N, C, ...)        \
      { N, TokenCategory::C, TokenValue::T, __VA_ARGS__ },
#include <open-axiom/token-value>
#undef OPENAXIOM_DEFINE_TOKEN      
   };

   TokenClassification classify(std::string_view s)
   {
      for (auto& t : token_map) {
         if (t.text == s)
            return { t.category, t.value };
      }
      return { TokenCategory::Identifier, TokenValue::Unknown };
   }

   std::ostream&
   operator<<(std::ostream& os, TokenValue tv) {
      if (tv < TokenValue::EndOfStream)
         os << token_map[uint8_t(tv)].text;
      else
         os << "%ALIEN";
      return os;
   }

   std::vector<Token> words(const Fragment& f, Language lang)
   {
      std::vector<Token> v { };   
      Tokenizer<Token> lex { f };
      while (auto t = lex.get(lang))
         v.push_back(t);
      return v;
   }
}
