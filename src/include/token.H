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

   // Cursor into a fragment.
   struct FragmentCursor {
      std::size_t line;         // index of a line in a fragment
      std::size_t column;       // column number at line.
   };

   inline FragmentCursor& operator++(FragmentCursor& p) {
      ++p.column;
      return p;
   }

   inline FragmentCursor operator++(FragmentCursor& p, int) {
      auto q = p;
      ++p;
      return q;
   }

   inline FragmentCursor& operator--(FragmentCursor& p) {
      --p.column;
      return p;
   }

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
   struct Tokenizer {
      Tokenizer(Frag& f)
            : frag(f),
              pos{ 0, frag.front().indent }
      {
         indents.push(pos.column);
      }

      bool eos() const {
         return pos.line >= frag.size()
            or (pos.line + 1 == frag.size() and pos.column >= frag.back().size());
      }

      Tok get(Language = Language::Spad);
   private:
      Frag& frag;
      FragmentCursor pos;
      std::stack<ColumnIndex> indents;

      std::size_t line_length() const { return frag(pos).size(); }
      LineNumber next_line_number() const {
         return pos.line + 1 < frag.size()
            ? frag[pos.line + 1].number
            : frag.back().number + 1;
      }
      ColumnIndex next_indentation() const {
         return pos.line + 1 < frag.size() ? frag[pos.line + 1].indent : 0;
      }

      LineNumber line_number() const {
         return pos.line < frag.size()
            ? frag(pos).number
            : frag.back().number + 1;
      }

      ColumnIndex column_number() const {
         return pos.line < frag.size() ? pos.column : 0;
      }

      using Locus = typename Tok::Location;
      Locus current_locus() {
         return { line_number(), column_number() };
      }
   };

   bool separator_or_punctuator(uint8_t);

   template<typename T>
   inline void comment_token(T& t, TokenValue v) {
      t.category = TokenCategory::Comment;
      t.value = v;
   }

   template<typename T>
   inline void operator_token(T& t, TokenValue v) {
      t.category = TokenCategory::Operator;
      t.value = v;
   }

   template<typename T>
   inline void punctuator_token(T& t, TokenValue v) {
      t.category = TokenCategory::Punctuator;
      t.value = v;
   }

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

   template<typename Frag, typename Tok>
   void string_literal(Frag& frag, FragmentCursor& pos, Tok& t) {
      bool done = false;
      bool escape = false;
      while (frag.covering(pos) && not done) {
         switch (frag(pos)[pos.column++]) {
         case '"': done = !escape;
            // fallthrough
         default: escape = false; break;
         case '_':
            if (pos.column == frag(pos).size()
                and pos.line < frag.size() - 1) {
               ++pos.line;
               pos.column = 0;
            }
            else
               escape = !escape;
            break;
         }
      }
      if (not done)
         throw EndOfStringUnseen{ frag(pos).number, pos.column };
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
   static void
   left_paren_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      punctuator_token(t, TokenValue::OpenParen);
      if (frag.covering(pos) and frag[pos] == '|') {
         ++pos;
         t.value = TokenValue::OpenMetaParen;
      }
   }

   template<typename Frag, typename Tok>
   static void
   left_brace_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      punctuator_token(t, TokenValue::OpenBrace);
      if (frag.covering(pos) and frag[pos] == '|') {
         ++pos;
         t.value = TokenValue::OpenMetaBrace;
      }
   }

   template<typename Frag, typename Tok>
   static void
   left_bracket_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      punctuator_token(t, TokenValue::OpenBracket);
      if (frag.covering(pos) and frag[pos] == '|') {
         ++pos;
         t.value = TokenValue::OpenMetaBracket;
      }
   }

   template<typename Frag, typename Tok>
   static void
   colon_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Colon);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case ':': t.value = TokenValue::ColonColon; ++pos; break;
         case '=': t.value = TokenValue::ColonEq; ++pos; break;
         case '-': t.value = TokenValue::ColonDash; ++pos; break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   star_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Star);
      if (frag.covering(pos) and frag[pos] == '*') {
         t.value = TokenValue::StarStar;
         ++pos;
      }
   }

   template<typename Frag, typename Tok>
   static void
   slash_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Slash);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '/': t.value = TokenValue::SlashSlash; ++pos; break;
         case '\\': t.value = TokenValue::SlashBackslash; ++pos; break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   backslash_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Backslash);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '\\': t.value = TokenValue::BackslashBackslash; ++pos; break;
         case '/': t.value = TokenValue::BackslashSlash; ++pos; break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   less_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Less);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '-': t.value = TokenValue::LeftArrow; ++pos; break;
         case '<': t.value = TokenValue::OpenChevron; ++pos; break;
         case '=':
            t.value = TokenValue::LessEq;
            if (frag.covering(++pos) and frag[pos] == '>') {
               t.value = TokenValue::Equiv;
               ++pos;
            }
            break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   equal_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Eq);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '>': t.value = TokenValue::Implies; ++pos; break;
         case '=':
            t.value = TokenValue::EqEq;
            if (frag.covering(++pos) and frag[pos] == '>') {
               t.value = TokenValue::FatArrow;
               ++pos;
            }
            break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   tilde_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Tilde);
      if (frag.covering(pos) and frag[pos] == '=') {
         t.value = TokenValue::TildeEq;
         ++pos;
      }
   }

   template<typename Frag, typename Tok>
   static void
   greater_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Greater);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '=': t.value = TokenValue::GreaterEq; ++pos; break;
         case '>': t.value = TokenValue::CloseChevron; ++pos; break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   bar_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      punctuator_token(t, TokenValue::Bar);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case ']': t.value = TokenValue::CloseMetaBracket; ++pos; break;
         case '}': t.value = TokenValue::CloseMetaBrace; ++pos; break;
         case ')': t.value = TokenValue::CloseMetaParen; ++pos; break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   minus_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Minus); 
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '>': t.value = TokenValue::RightArrow; ++pos; break;
         case '-':
            comment_token(t, TokenValue::Wisecrack);
            pos.column = frag(pos).size();
            break;
         }
   }


   template<typename Frag, typename Tok>
   static void
   plus_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Plus);
      if (frag.covering(pos))
         switch (frag[pos]) {
         case '+':
            comment_token(t, TokenValue::Commentary);
            pos.column = frag(pos).size();
            break;
         case '-':
            if (pos.column + 1 < frag(pos).size()
                and frag(pos)[pos.column + 1] == '>') {
               t.value = TokenValue::MapsTo;
               pos.column += 2;
            }
            break;
         default: break;
         }
   }

   template<typename Frag, typename Tok>
   static void
   dot_et_al(Frag& frag, FragmentCursor& pos, Tok& t) {
      operator_token(t, TokenValue::Dot);
      if (frag.covering(pos) and frag[pos] == '.') {
         t.value = TokenValue::DotDot;
         ++pos;
      }
   }

   template<typename Frag, typename Tok>
   static void
   dollar_et_al(Frag& frag, FragmentCursor& pos, Tok& t, Language dialect) {
      if (dialect != Language::Boot or not frag.covering(pos)
          or separator_or_punctuator(frag[pos]))
         operator_token(t, TokenValue::Dollar);
      else
         identifier(frag(pos), pos.column, t, dialect);
   }

   template<typename Frag, typename Tok>
   static void
   sharp_et_al(Frag& frag, FragmentCursor& pos, Tok& t, Language dialect) {
      if (dialect != Language::Lisp)
         operator_token(t, TokenValue::Sharp);
      else if (frag.covering(pos))
         switch (frag[pos++]) {
         case '(': punctuator_token(t, TokenValue::SharpLeftParen); break;
         case '\'': operator_token(t, TokenValue::SharpApostrophe); break;
         case ':': operator_token(t, TokenValue::SharpColon); break;
         case '+': punctuator_token(t, TokenValue::SharpPlus); break;
         case '-': punctuator_token(t, TokenValue::SharpMinus); break;
         case '.': operator_token(t, TokenValue::SharpDot); break;
         default: --pos; break;
         }
   }


   template<typename Frag, typename Tok>
   Tok Tokenizer<Frag, Tok>::get(Language dialect) {
      Tok t { };
      t.start = current_locus();
      
      if (eos()) {
         t.category = TokenCategory::EOS;
         t.end = current_locus();
         return t;
      }
      else if (isspace(frag[pos])) {
         skip_whitespace(frag(pos), pos.column);
         t.category = TokenCategory::Whitespace;
         t.end = current_locus();
         return t;
      }
      else if (pos.column == line_length() - 1 and frag(pos).back() == '_') {
         ++pos.line;
         pos.column = frag(pos).indent;
      }
      else if (pos.column == line_length()) {
         auto indent = indents.top();
         auto next_indent = next_indentation();
         t.start = t.end = { next_line_number(), next_indent };
         if (indent < next_indent) {
            indents.push(next_indent);
            ++pos.line;
            pos.column = next_indent;
            t.category = TokenCategory::Formatting;
            t.value = TokenValue::Indent;
         }
         else if (indent > next_indent) {
            indents.pop();
            t.category = TokenCategory::Formatting;
            t.value = TokenValue::Unindent;
         }
         else {
            ++pos.line;
            pos.column = next_indent;
            t.category = TokenCategory::Formatting;
            t.value = TokenValue::Justify;
         }
         return t;
      }

      switch (auto c = frag.advance(pos)) {
      case '#': sharp_et_al(frag, pos, t, dialect); break;
      case '@': operator_token(t, TokenValue::At); break;
      case '^': operator_token(t, TokenValue::Caret); break;
      case '&': punctuator_token(t, TokenValue::Ampersand); break;         
      case '!': punctuator_token(t, TokenValue::Exclamation); break;
      case '\'': punctuator_token(t, TokenValue::Apostrophe); break;
      case ',': punctuator_token(t, TokenValue::Comma); break;
      case ';': punctuator_token(t, TokenValue::Semicolon); break;
      case '`': punctuator_token(t, TokenValue::Backquote); break;
      case '(': left_paren_et_al(frag, pos, t); break;
      case ')': punctuator_token(t, TokenValue::CloseParen); break;
      case '{': left_brace_et_al(frag, pos, t); break;
      case '}': punctuator_token(t, TokenValue::CloseBrace); break;
      case '[': left_bracket_et_al(frag, pos, t); break;
      case ']': punctuator_token(t, TokenValue::CloseBracket); break;
      case ':': colon_et_al(frag, pos, t); break;
      case '*': star_et_al(frag, pos, t); break;
      case '/': slash_et_al(frag, pos, t); break;
      case '\\': backslash_et_al(frag, pos, t); break;
      case '<': less_et_al(frag, pos, t); break;
      case '=': equal_et_al(frag, pos, t); break;
      case '~': tilde_et_al(frag, pos, t); break;
      case '>': greater_et_al(frag, pos, t); break;
      case '|': bar_et_al(frag, pos, t); break;
      case '-': minus_et_al(frag, pos, t); break;
      case '+': plus_et_al(frag, pos, t); break;
      case '.': dot_et_al(frag, pos, t); break;
      case '"': string_literal(frag, pos, t); break;
      case '$': dollar_et_al(frag, pos, t, dialect); break;

      default:
         if (isdigit(c))
            number(frag(pos), pos.column, t);
         else if (identifier_head(c))
            identifier(frag(pos), pos.column, t, dialect);
         else
            junk(frag(pos), pos.column, t);
         break;
      }

      t.end = { frag(pos).number, pos.column };
      return t;
   }

   // -- Token streams.
   template<typename T>
   struct TokenStream : std::vector<T> {
      template<typename Frag>
      explicit TokenStream(Frag& f, Language dialect = Language::Spad) {
         Tokenizer<Frag, T> lex { f };
         while (auto t = lex.get(dialect))
            this->push_back(t);
      }
   };
}

#endif  // OPENAXIOM_TOKEN_included
