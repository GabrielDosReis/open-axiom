// Copyright (C) 2010-2013, Gabriel Dos Reis.
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
//     - Neither the name of The Numerical Algorithms Group Ltd. nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
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

// --% Author: Gabriel Dos Reis.

#include <ctype.h>
#include <string.h>
#include <iostream>
#include <iterator>
#include <open-axiom/sexpr>
#include <open-axiom/FileMapping>
#include <open-axiom/diagnostics>

namespace OpenAxiom {
   namespace Sexpr {
      static void
      invalid_character(Reader::State& s) {
         auto line = std::to_string(s.lineno);
         auto column = std::to_string(s.cur - s.line);
         auto msg = "invalid character on line " + line +
            " and column " + column;
         if (isprint(*s.cur))
            throw Diagnostics::BasicError(msg + ": " + std::string(1, *s.cur));
         throw Diagnostics::BasicError(msg + " with code " + std::to_string(*s.cur));
      }
      
      static void
      syntax_error(const std::string& s) {
         throw Diagnostics::BasicError(s);
      }

      // Return true if character `c' introduces a blank.
      static bool
      is_blank(char c) {
         return c == ' ' or c == '\t' or c == '\v'
            or c == '\n' or c == '\f' or c == '\r';
      }
      
      // Return true if the character `c' introduces a delimiter.
      static bool
      is_delimiter(char c) {
         return is_blank(c)
            or c == '(' or c == ')' or c == '\''
            or c == '`' or c == '#';
      }

      // Move the cursor past all consecutive blank characters, and
      // return true if there are more input characters to consider.
      static bool
      skip_blank(Reader::State& s) {
         for (bool done = false; s.cur < s.end and not done; )
            switch (*s.cur) {
            case '\n':
               ++s.lineno;
               s.line = ++s.cur;
               break;
            case ' ': case '\t': case '\v': case '\r': case '\f':
               ++s.cur;
               break;
            default: done = true; break;
            }
         return s.cur < s.end;
      }

      // Move `cur' to end-of-line marker.
      static void
      skip_to_eol(Reader::State& s) {
         // FIXME: properly handle CR+LF.
         while (s.cur < s.end and *s.cur != '\n')
            ++s.cur;
      }

      // Move `cur' one-past a non-esacaped character `c'.
      // Return true if the character was seen.
      static bool
      skip_to_nonescaped_char(Reader::State& s, char c) {
         for (bool saw_escape = false; s.cur < s.end; ++s.cur)
            if (saw_escape)
               saw_escape = false;
            else if (*s.cur == '\\')
               saw_escape = true;
            else if (*s.cur == c) {
               ++s.cur;
               return true;
            }
         return false;
      }

      // Move the cursor past the closing quote of string literal.
      // Return true if the closing quote was effectively seen.
      static inline bool
      skip_to_quote(Reader::State& s) {
         return skip_to_nonescaped_char(s, '"');
      }

      template<typename Pred>
      static bool
      advance_while(Reader::State& s, Pred p) {
         while (s.cur < s.end and p(*s.cur))
            ++s.cur;
         return s.cur < s.end;
      }

      // Return true if the character `c' be part of a non-absolute
      // identifier.
      static bool
      identifier_part(Byte c) {
         switch (c) {
         case '+': case '-': case '*': case '/': case '%': case '^':
         case '~': case '@': case '$': case '&': case '=':
         case '<': case '>': case '?': case '!': case '_':
         case '[': case ']': case '{': case '}':
            return true;
         default:
            return isalnum(c);
         }
      }

      // -- AtomSyntax --
      AtomSyntax::AtomSyntax(const Lexeme& t) : lex(t) { }

      // -- IntegerSyntax --
      IntegerSyntax::IntegerSyntax(const Lexeme& t) : AtomSyntax(t) { }

      void
      IntegerSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- CharacterSyntax --
      CharacterSyntax::CharacterSyntax(const Lexeme& t) : AtomSyntax(t) { }

      void
      CharacterSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- StringSyntax --
      StringSyntax::StringSyntax(const Lexeme& t) : AtomSyntax(t) { }

      void
      StringSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- SymbolSyntax --
      SymbolSyntax::SymbolSyntax(const Lexeme& t, Kind k)
            : AtomSyntax(t), sort(k)
      { }

      void
      SymbolSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- AnchorSyntax --
      AnchorSyntax::AnchorSyntax(size_t t, const Syntax* s) : tag(t), val(s) { }

      void
      AnchorSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- ReferenceSyntax --
       ReferenceSyntax::ReferenceSyntax(const Lexeme& t, Ordinal n)
             : AtomSyntax(t), pos(n)
      { }

      void
      ReferenceSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- QuoteSyntax --
      QuoteSyntax::QuoteSyntax(const Syntax* s)
            : unary_form<QuoteSyntax>(s)
      { }

      // -- AntiquoteSyntax --
      AntiquoteSyntax::AntiquoteSyntax(const Syntax* s)
            : unary_form<AntiquoteSyntax>(s)
      { }

      // -- Expand --
      Expand::Expand(const Syntax* s) : unary_form<Expand>(s) { }

      // -- Eval --
      Eval::Eval(const Syntax* s) : unary_form<Eval>(s) { }

      // -- Splice --
      Splice::Splice(const Syntax* s) : unary_form<Splice>(s) { }

      // -- Function --
      Function::Function(const Syntax* s) : unary_form<Function>(s) { }

      // -- Include --
      Include::Include(const Syntax* c, const Syntax* s)
            : binary_form<Include>(c, s)
      { }

      // -- Exclude --
      Exclude::Exclude(const Syntax* c, const Syntax* s)
            : binary_form<Exclude>(c, s)
      { }

      // -- ListSyntax --
      ListSyntax::ListSyntax() : dot(false) { }

      ListSyntax::ListSyntax(const base& elts, bool d)
            : base(elts), dot(d)
      { }

      ListSyntax::~ListSyntax() { }

      void
      ListSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -- VectorSyntax --
      VectorSyntax::VectorSyntax() { }

      VectorSyntax::VectorSyntax(const base& elts) : base(elts) { }

      VectorSyntax::~VectorSyntax() { }
      
      void
      VectorSyntax::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ---------------
      // -- Allocator --
      // ---------------
      Allocator::Allocator() { }

      // This destructor is defined here so that it provides
      // a single instantiation point for destructors of all
      // used templates floating around.
      Allocator::~Allocator() { }

      const CharacterSyntax*
      Allocator::make_character(const Lexeme& t) {
         return chars.make(t);
      }

      const IntegerSyntax*
      Allocator::make_integer(const Lexeme& t) {
         return ints.make(t);
      }

      const StringSyntax*
      Allocator::make_string(const Lexeme& t) {
         return strs.make(t);
      }

      const SymbolSyntax*
      Allocator::make_symbol(SymbolSyntax::Kind k, const Lexeme& t) {
         return syms.make(t, k);
      }

      const ReferenceSyntax*
      Allocator::make_reference(size_t i, const Lexeme& t) {
         return refs.make(t, i);
      }

      const AnchorSyntax*
      Allocator::make_anchor(size_t t, const Syntax* s) {
         return ancs.make(t, s);
      }

      const QuoteSyntax*
      Allocator::make_quote(const Syntax* s) {
         return quotes.make(s);
      }

      const AntiquoteSyntax*
      Allocator::make_antiquote(const Syntax* s) {
         return antis.make(s);
      }

      const Expand*
      Allocator::make_expand(const Syntax* s) {
         return exps.make(s);
      }

      const Eval*
      Allocator::make_eval(const Syntax* s) {
         return evls.make(s);
      }

      const Splice*
      Allocator::make_splice(const Syntax* s) {
         return spls.make(s);
      }

      const Function*
      Allocator::make_function(const Syntax* s) {
         return funs.make(s);
      }

      const Include*
      Allocator::make_include(const Syntax* c, const Syntax* s) {
         return incs.make(c, s);
      }

      const Exclude*
      Allocator::make_exclude(const Syntax* c, const Syntax* s) {
         return excs.make(c, s);
      }

      const ListSyntax*
      Allocator::make_list(const std::vector<const Syntax*>& elts, bool dot) {
         if (elts.empty())
            return &empty_list;
         return lists.make(elts, dot);
      }

      const VectorSyntax*
      Allocator::make_vector(const std::vector<const Syntax*>& elts) {
         if (elts.empty())
            return &empty_vector;
         return vectors.make(elts);
      }

      // The sequence of characters in [cur, last) consists
      // entirely of digits.  Return the corresponding natural value.
      static size_t
      natural_value(const Byte* cur, const Byte* last) {
         size_t n = 0;
         for (; cur < last; ++cur)
            // FIXME: check for overflow.
            n = 10 * n + (*cur - '0');
         return n;
      }

      // -- Reader --
      Reader::Reader(const Byte* f, const Byte* l)
            : st{ f, l, f, f, 1, }
      { }

      static const Syntax* read_sexpr(Reader::State&);

      // Parse a string literal
      static const Syntax*
      read_string(Reader::State& s) {
         auto start = s.cur++;
         if (not skip_to_quote(s))
            syntax_error("missing closing quote sign for string literal");
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_string(t);
      }

      // Parse an absolute identifier.
      static const Syntax*
      read_absolute_symbol(Reader::State& s) {
         auto start = ++s.cur;
         if (not skip_to_nonescaped_char(s, '|'))
            syntax_error("missing closing bar sign for an absolute symbol");
         Lexeme t = { { start, s.cur - 1 }, s.lineno };
         return s.alloc.make_symbol(SymbolSyntax::absolute, t);
      }

      // Read an atom starting with digits.
      static const Syntax*
      read_maybe_natural(Reader::State& s) {
         auto start = s.cur;
         advance_while (s, isdigit);
         if (s.cur >= s.end or is_delimiter(*s.cur)) {
            Lexeme t = { { start, s.cur }, s.lineno };
            return s.alloc.make_integer(t);
         }
         advance_while(s, identifier_part);
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_symbol(SymbolSyntax::ordinary, t);
      }

      // Read an identifier.
      static const Syntax*
      read_identifier(Reader::State& s) {
         auto start = s.cur;
         advance_while(s, identifier_part);
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_symbol(SymbolSyntax::ordinary, t);
      }

      // Read an atom starting with a '+' or '-' sign; this
      // should be identifier, or a signed integer.
      static const Syntax*
      read_maybe_signed_number(Reader::State& s) {
         auto start = s.cur++;
         if (s.cur < s.end and isdigit(*s.cur)) {
            advance_while(s, isdigit);
            if (s.cur >= s.end or is_delimiter(*s.cur)) {
               Lexeme t = { { start, s.cur }, s.lineno };
               return s.alloc.make_integer(t);
            }
         }
         advance_while(s, identifier_part);
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_symbol(SymbolSyntax::ordinary, t);
      }

      static const Syntax*
      read_keyword(Reader::State& s) {
         auto start = s.cur++;
         advance_while(s, identifier_part);
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_symbol(SymbolSyntax::keyword, t);
      }

      // Read an atom.
      static const Syntax*
      read_atom(Reader::State& s) {
         switch (*s.cur) {
         case '"': return read_string(s);
         case ':': return read_keyword(s);
         case '-': case '+': return read_maybe_signed_number(s);

         case '0': case '1': case '2': case '3': case '4':
         case '5': case '6': case '7': case '8': case '9':
            return read_maybe_natural(s);

         default:
            if (identifier_part(*s.cur))
               return read_identifier(s);
            invalid_character(s);
            ++s.cur;
            return nullptr;
         }
      }

      // Parse a quote expression.
      static const Syntax*
      read_quote(Reader::State& s) {
         ++s.cur;               // skip the quote character
         auto x = read_sexpr(s);
         if (x == nullptr)
            syntax_error("end of input reached after quote sign");
         return s.alloc.make_quote(x);
      }

      // Parse a backquote expression.
      static const Syntax*
      read_backquote(Reader::State& s) {
         ++s.cur;               // skip the backquote character
         auto x = read_sexpr(s);
         if (x == nullptr)
            syntax_error("end of input reached after backquote sign");
         return s.alloc.make_antiquote(x);
      }

      // We've just seen "#(" indicating the start of a literal
      // vector.  Read the elements and return the corresponding form.
      static const Syntax*
      finish_literal_vector(Reader::State& s) {
         ++s.cur;               // Skip the open paren.
         std::vector<const Syntax*> elts { };
         while (skip_blank(s) and *s.cur != ')') {
            if (auto x = read_sexpr(s))
               elts.push_back(x);
            else
               syntax_error("syntax error while reading vector elements");
         }
         if (s.cur >= s.end)
            syntax_error("unfinished literal vector");
         else
            ++s.cur;
         return s.alloc.make_vector(elts);
      }

      // We've just seen the sharp sign followed by a digit.  We assume
      // we are about to read an anchor or a back reference.
      static const Syntax*
      finish_anchor_or_reference(Reader::State& s) {
         auto start = s.cur;
         advance_while(s, isdigit);
         if (s.cur >= s.end)
            syntax_error("end-of-input after sharp-number sign");
         const Byte c = *s.cur;
         if (c != '#' and c != '=')
            syntax_error("syntax error after sharp-number-equal sign");
         Lexeme t = { { start, s.cur }, s.lineno };
         auto n = natural_value(start, s.cur);
         ++s.cur;
         if (c == '#')
            return s.alloc.make_reference(n, t);
         auto x = read_sexpr(s);
         if (x == nullptr)
            syntax_error("syntax error after sharp-number-equal sign");
         return s.alloc.make_anchor(n, x);
      }

      static const Syntax*
      finish_function(Reader::State& s) {
         ++s.cur;               // skip quote sign.
         auto x = read_sexpr(s);
         if (x == nullptr)
            syntax_error("missing function designator after sharp-quote sign");
         return s.alloc.make_function(x);
      }

      static const Syntax*
      finish_uninterned_symbol(Reader::State& s) {
         ++s.cur;               // skip colon sign.
         auto start = s.cur;
         advance_while(s, identifier_part);
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_symbol(SymbolSyntax::uninterned, t);
      }

      static const Syntax*
      finish_readtime_eval(Reader::State& s) {
         ++s.cur;               // skip dot sign.
         auto x = read_sexpr(s);
         if (x == nullptr)
            syntax_error("parse error after sharp-dot sign");
         return s.alloc.make_eval(x);
      }

      static const Syntax*
      finish_character(Reader::State& s) {
         ++s.cur;               // skip backslash sign
         auto start = s.cur;
         advance_while(s, identifier_part);
         Lexeme t = { { start, s.cur }, s.lineno };
         return s.alloc.make_character(t);
      }

      static const Syntax*
      finish_include(Reader::State& s) {
         ++s.cur;
         auto cond = read_sexpr(s);
         auto form = read_sexpr(s);
         return s.alloc.make_include(cond, form);
      }

      static const Syntax*
      finish_exclude(Reader::State& s) {
         ++s.cur;
         auto cond = read_sexpr(s);
         auto form = read_sexpr(s);
         return s.alloc.make_exclude(cond, form);
      }

      static const Syntax*
      read_sharp_et_al(Reader::State& s) {
         if (++s.cur >= s.end)
            syntax_error("end-of-input reached after sharp sign");
         switch (*s.cur) {
         case '(':  return finish_literal_vector(s);
         case '\'': return finish_function(s);
         case ':': return finish_uninterned_symbol(s);
         case '.': return finish_readtime_eval(s);
         case '\\': return finish_character(s);
         case '+': return finish_include(s);
         case '-': return finish_exclude(s);

         default:
            if (isdigit(*s.cur))
               return finish_anchor_or_reference(s);
            syntax_error("syntax error after sharp-sign");
         }
         return nullptr;
      }

      // We have just seen a dot; read the tail and the closing parenthesis.
      static const Syntax*
      finish_dotted_list(Reader::State& s, std::vector<const Syntax*>& elts) {
         ++s.cur;               // Skip dot sign.
         auto x = read_sexpr(s);
         if (x == nullptr)
            syntax_error("missing expression after dot sign");
         if (not skip_blank(s) or *s.cur != ')')
            syntax_error("missing closing parenthesis");
         ++s.cur;
         elts.push_back(x);
         return s.alloc.make_list(elts, true);
      }

      static const Syntax*
      read_pair(Reader::State& s) {
         ++s.cur;               // skip opening parenthesis
         std::vector<const Syntax*> elts { };
         while (skip_blank(s))
            switch (*s.cur) {
            case '.':
               if (elts.empty())
                  syntax_error("missing expression before dot sign.");
               return finish_dotted_list(s, elts);

            case ')':
               ++s.cur;
               return s.alloc.make_list(elts);

            default:
               if (auto x = read_sexpr(s))
                  elts.push_back(x);
               else
                  syntax_error("unfinished pair expression");
               break;
            }
         syntax_error("end-of-input while looking for closing parenthesis");
         return nullptr;
      }

      static const Syntax*
      read_sexpr(Reader::State& s) {
         while (skip_blank(s))
            switch (*s.cur) {
            case ';': skip_to_eol(s); break;
            case '\'': return read_quote(s);
            case '`': return read_backquote(s);
            case '|': return read_absolute_symbol(s);
            case '#': return read_sharp_et_al(s);
            case '(': return read_pair(s);
            default: return read_atom(s);
            }
         return nullptr;
      }

      const Syntax*
      Reader::read() {
         return read_sexpr(st);
      }

      const Byte*
      Reader::position(Ordinal p) {
         st.cur = st.start + p;
         st.line = st.cur;
         // while (st.line > st.start and st.line[-1] != '\n')
         //    --st.line;
         return st.cur;
      }

   }
}
