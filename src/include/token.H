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

#ifndef OPENAXIOM_TOKEN_included
#define OPENAXIOM_TOKEN_included

#include <stdint.h>
#include <stack>
#include <iosfwd>
#include <open-axiom/Input>
#include <open-axiom/dialect>

namespace OpenAxiom {
   // Categorization of Boot and Spad tokens.
   enum class TokenCategory : uint8_t {
      Unclassified,             // token of unknown class
      Whitespace,               // sequence of white-space characters
      Comment,                  // a description of an ignorable comment
      Punctuator,               // a punctuator character
      Operator,                 // an operator both symbolic and alphabetic
      Integer,                  // an integer literal
      FloatingPoint,            // a floating-point literal
      String,                   // a string literal
      Keyword,                  // a reserved word both symbolic and alphabetic
      Identifier,               // an identifier
      Formatting,               // a layout formatting token
      Junk,                     // invalid/malformed token
      EOS                       // end-of-token-stream indicator
   };

   std::ostream& operator<<(std::ostream&, TokenCategory);

   // The abstract value associated with a token.
   enum class TokenValue : uint8_t {
#undef OPENAXIOM_DEFINE_TOKEN
#define OPENAXIOM_DEFINE_TOKEN(T, ...)  T,
#include <open-axiom/token-value>
#undef OPENAXIOM_DEFINE_TOKEN
      Artificial,               // Tokens after this are artificial
      Indent,                   // new line indentation, greater than previous
      Unindent,                 // new line indentation, less than previous
      Justify,                  // align indentation with preceding line.

      EndOfStream               // end of token stream
   };

   std::ostream& operator<<(std::ostream&, TokenValue);

   // Given a symbolic or alphabetic token, retrieve its category
   // and associated abstract value.
   struct TokenClassification {
      TokenCategory category;
      TokenValue value;

      explicit operator bool() const {
         return category != TokenCategory::Unclassified;
      }
   };

   TokenClassification classify(const std::string&);

   // Datatypes for locating lines and columns.
   using LineNumber = std::size_t;
   using ColumnIndex = std::size_t;

   // -- Exception types
   struct EndOfStringUnseen {
      LineNumber line;
      ColumnIndex column;
   };

   struct MissingExponent {
      LineNumber line;
      ColumnIndex column;
   };

   // Object of this datatype decompose a program fragment into a
   // token stream.  The tokens are of type indicated by Tok.
   template<typename Frag, typename Tok>
   struct TokenStream {
      TokenStream(Frag& f)
            : frag(f),
              line(),
              idx(frag.front().indent)
      {
         indents.push(idx);
      }

      bool eos() const {
         return line >= frag.size()
            or (line + 1 == frag.size() and idx >= frag.back().size());
      }

      Tok get(Language = Language::Spad);
   private:
      Frag& frag;
      std::size_t line;
      std::size_t idx;
      std::stack<ColumnIndex> indents;

      std::size_t line_length() const { return frag[line].size(); }
      LineNumber next_line_number() const {
         return line + 1 < frag.size()
            ? frag[line + 1].number
            : frag.back().number + 1;
      }
      ColumnIndex next_indentation() const {
         return line + 1 < frag.size() ? frag[line + 1].indent : 0;
      }

      LineNumber line_number() const {
         return line < frag.size()
            ? frag[line].number
            : frag.back().number + 1;
      }

      ColumnIndex column_number() const {
         return line < frag.size() ? idx : 0;
      }

      using Locus = typename Tok::Location;
      Locus current_locus() {
         return { line_number(), column_number() };
      }
   };

   bool separator_or_punctuator(uint8_t);

   template<typename L, typename T>
   static void junk(L& line, ColumnIndex& idx, T& t) {
      while (idx < line.size() and not separator_or_punctuator(line[idx]))
         ++idx;
      t.category = TokenCategory::Junk;
   }

   template<typename L>
   inline void
   skip_whitespace(L& line, ColumnIndex& idx) {
      while (idx < line.size() and isspace(line[idx]))
         ++idx;
   }

   template<typename L, typename T>
   void string(L& line, ColumnIndex& idx, T& t) {
      bool done = false;
      bool escape = false;
      while (idx < line.size() && not done) {
         switch (line[idx++]) {
         case '_': escape = !escape; break;
         case '"': done = !escape;
            // fallthrough
         default: escape = false; break;
         }
      }
      if (not done)
         throw EndOfStringUnseen{ line.number, idx };
      t.category = TokenCategory::String;
   }

   template<typename L>
   void skip_to_end_of_integer(L& line, ColumnIndex& idx) {
      while (idx < line.size() and isdigit(line[idx]))
         ++idx;
   }
   
   template<typename L, typename T>
   void integer(L& line, ColumnIndex& idx, T& t) {
      skip_to_end_of_integer(line, idx);
      t.category = TokenCategory::Integer;
   }

   template<typename L, typename T>
   T& number(L& line, ColumnIndex& idx, T& t) {
      integer(line, idx, t);
      if (idx >= line.size() or line[idx] != '.')
         return t;
      if (++idx >= line.size() or not isdigit(line[idx])) {
         --idx;
         return t;
      }

      t.category = TokenCategory::FloatingPoint;
      skip_to_end_of_integer(line, idx);
      if (idx >= line.size() or (line[idx] != 'e' and line[idx] != 'E'))
         return t;
      if (++idx < line.size() and (line[idx] == '+' or line[idx] == '-'))
         ++idx;
      if (idx >= line.size() or not isdigit(line[idx]))
         throw MissingExponent{ line.number, idx };
      skip_to_end_of_integer(line, idx);
      return t;
   }

   inline bool
   identifier_head(uint8_t c) {
      return isalpha(c) or c == '%' or c == '_';
   }

   inline bool
   identifier_part(uint8_t c) {
      return identifier_head(c) or isdigit(c);
   }

   inline bool
   identifier_suffix(uint8_t c) {
      return c == '!' or c == '?';
   }

   inline bool internal_prefix(uint8_t c) {
      return c == '%' or c == '$';
   }

   template<typename L>
   inline void
   skip_prefix(L& line, ColumnIndex& idx, uint8_t c) {
      while (idx < line.size() and line[idx] == c)
         ++idx;
   }

   template<typename L, typename T>
   T& identifier(L& line, ColumnIndex& idx, T& t, Language dialect) {
      t.category = TokenCategory::Identifier;

      ColumnIndex start = --idx; // idx was ahead by 1.
      if (dialect == Language::Boot and internal_prefix(line[idx]))
         skip_prefix(line, idx, line[idx]);
      bool saw_escape = false;
      while (idx < line.size()) {
         if (not identifier_part(line[idx]) and line[idx - 1] != '_')
            break;
         else if (line[idx] == '_')
            saw_escape = true;
         ++idx;
      }
      while (idx < line.size() and identifier_suffix(line[idx]))
         ++idx;

      if (saw_escape)
         t.category = TokenCategory::Identifier;
      else if (auto info = classify(line.sub_string(start, idx))) {
         t.category = info.category;
         t.value = info.value;
      }
      return t;
   }

   template<typename Frag, typename Tok>
   Tok TokenStream<Frag, Tok>::get(Language dialect) {
      Tok t { };
      t.start = current_locus();
      
      if (eos()) {
         t.category = TokenCategory::EOS;
         t.end = current_locus();
         return t;
      }
      else if (isspace(frag[line][idx])) {
         skip_whitespace(frag[line], idx);
         t.category = TokenCategory::Whitespace;
         t.end = current_locus();
         return t;
      }
      else if (idx == line_length() - 1 and frag[line].back() == '_') {
         ++line;
         idx = frag[line].indent;
      }
      else if (idx == line_length()) {
         auto indent = indents.top();
         auto next_indent = next_indentation();
         t.start = t.end = { next_line_number(), next_indent };
         if (indent < next_indent) {
            indents.push(next_indent);
            ++line;
            idx = next_indent;
            t.category = TokenCategory::Formatting;
            t.value = TokenValue::Indent;
         }
         else if (indent > next_indent) {
            indents.pop();
            t.category = TokenCategory::Formatting;
            t.value = TokenValue::Unindent;
         }
         else {
            ++line;
            idx = next_indent;
            t.category = TokenCategory::Formatting;
            t.value = TokenValue::Justify;
         }
         return t;
      }

      switch (auto c = frag[line][idx++]) {
      case '#':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Pound;
         break;

      case '@':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::At;
         break;

      case '^':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Caret;
         break;

      case '&':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Ampersand;
         break;
         
      case '!':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Exclamation;
         break;
         
      case '\'':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Apostrophe;
         break;
      case ',':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Comma;
         break;
         
      case ';':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Semicolon;
         break;
         
      case '`':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Backquote;
         break;
         
      case '(':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::OpenParen;
         if (idx < line_length() and frag[line][idx] == '|') {
            ++idx;
            t.value = TokenValue::OpenMetaParen;
         }
         break;
         
      case ')':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::CloseParen;
         break;
         
      case '{':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::OpenBrace;
         if (idx < line_length() and frag[line][idx] == '|') {
            ++idx;
            t.value = TokenValue::OpenMetaBrace;
         }
         break;

      case '}':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::CloseBrace;
         break;

      case '[':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::OpenBracket;
         if (idx < line_length() and frag[line][idx] == '|') {
            ++idx;
            t.value = TokenValue::OpenMetaBracket;
         }
         break;
         
      case ']':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::CloseBracket;
         break;
         
      case ':':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Colon; 
         if (idx < line_length())
            switch (frag[line][idx]) {
            case ':': t.value = TokenValue::ColonColon; ++idx; break;
            case '=': t.value = TokenValue::ColonEq; ++idx; break;
            case '-': t.value = TokenValue::ColonDash; ++idx; break;
            default: break;
            }
         break;
         
      case '*':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Star;
         if (idx < line_length() and frag[line][idx] == '*') {
            t.value = TokenValue::StarStar;
            ++idx;
         }
         break;
         
      case '/':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Slash;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '/': t.value = TokenValue::SlashSlash; ++idx; break;
            case '\\': t.value = TokenValue::SlashBackslash; ++idx; break;
            default: break;
            }
         break;

      case '\\':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Backslash;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '\\': t.value = TokenValue::BackslashBackslash; ++idx; break;
            case '/': t.value = TokenValue::BackslashSlash; ++idx; break;
            default: break;
            }
         break;
         
      case '<':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Less;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '-': t.value = TokenValue::LeftArrow; ++idx; break;
            case '<': t.value = TokenValue::OpenChevron; ++idx; break;
            case '=':
               t.value = TokenValue::LessEq;
               if (++idx < line_length() and frag[line][idx] == '>') {
                  t.value = TokenValue::Equiv;
                  ++idx;
               }
               break;
            default: break;
            }
         break;
         
      case '=':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Eq;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '>': t.value = TokenValue::Implies; ++idx; break;
            case '=':
               t.value = TokenValue::EqEq;
               if (++idx < line_length() and frag[line][idx] == '>') {
                  t.value = TokenValue::FatArrow;
                  ++idx;
               }
               break;
            default: break;
            }
         break;
         
      case '~':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Tilde;
         if (idx < line_length() and frag[line][idx] == '=') {
            t.value = TokenValue::TildeEq;
            ++idx;
         }
         break;

      case '>':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Greater;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '=': t.value = TokenValue::GreaterEq; ++idx; break;
            case '>': t.value = TokenValue::CloseChevron; ++idx; break;
            }
         break;

      case '|':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Bar;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case ']': t.value = TokenValue::CloseMetaBracket; ++idx; break;
            case '}': t.value = TokenValue::CloseMetaBrace; ++idx; break;
            case ')': t.value = TokenValue::CloseMetaParen; ++idx; break;
            default: break;
            }
         break;

      case '-':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Minus; 
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '>': t.value = TokenValue::RightArrow; ++idx; break;
            case '-':
               t.category = TokenCategory::Comment;
               t.value = TokenValue::Wisecrack;
               idx = frag[line].size();
               break;
            }
         break;

      case '+':
         t.category = TokenCategory::Operator;
         t.value = TokenValue::Plus;
         if (idx < line_length())
            switch (frag[line][idx]) {
            case '+':
               t.category = TokenCategory::Comment;
               t.value = TokenValue::Commentary;
               idx = frag[line].size();
               break;
            case '-':
               if (idx + 1 < line_length() and frag[line][idx+1] == '>') {
                  t.value = TokenValue::MapsTo;
                  idx += 2;
               }
               break;
            default: break;
            }
         break;

      case '.':
         t.category = TokenCategory::Punctuator;
         t.value = TokenValue::Dot;
         if (idx < line_length() and frag[line][idx] == '.') {
            t.category = TokenCategory::Operator;
            t.value = TokenValue::DotDot;
            ++idx;
         }
         break;

      case '"':
         string(frag[line], idx, t);
         break;

      case '$':
         if (dialect != Language::Boot or idx >= line_length()
             or separator_or_punctuator(frag[line][idx])) {
            t.category = TokenCategory::Operator;
            t.value = TokenValue::Dollar;
         }
         else
            identifier(frag[line], idx, t, dialect);
         break;

      default:
         if (isdigit(c))
            number(frag[line], idx, t);
         else if (identifier_head(c))
            identifier(frag[line], idx, t, dialect);
         else
            junk(frag[line], idx, t);
         break;
      }

      t.end = { frag[line].number, idx };
      return t;
   }
}

#endif  // OPENAXIOM_TOKEN_included
