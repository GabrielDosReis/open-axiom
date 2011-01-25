// Copyright (C) 2010, Gabriel Dos Reis.
// All rights reserved.
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
#include <iostream>
#include <iterator>
#include <open-axiom/sexpr>

namespace OpenAxiom {
   namespace Sexpr {
      std::ostream&
      operator<<(std::ostream& os, const Token& t) {
         switch (t.type) {
         case Token::dot: os << "DOT"; break;
         case Token::comma: os << "COMMA"; break;
         case Token::open_paren: os << "OPEN_PAREN"; break;
         case Token::close_paren: os << "CLOSE_PAREN"; break;
         case Token::apostrophe: os << "APOSTROPHE"; break;
         case Token::backquote: os << "BACKQUOTE"; break;
         case Token::backslash: os << "BACKSLASH"; break;
         case Token::sharp_open_paren: os << "SHARP_OPEN_PAREN"; break;
         case Token::sharp_apostrophe: os << "SHARP_APOSTROPHE"; break;
         case Token::sharp_colon: os << "SHARP_COLON"; break;
         case Token::integer: os << "INTEGER"; break;
         case Token::character: os << "CHARACTER"; break;
         case Token::string: os << "STRING"; break;
         case Token::identifier: os << "IDENTIFIER"; break;
         case Token::sharp_integer_sharp:
            os << "SHARP_INTEGER_SHARP"; break;
         case Token::sharp_integer_equal:
            os << "SHARP_INTEGER_EQUAL"; break;
         default: os << "UNKNOWN"; break;
         }
         os << '(';
         if (t.lexeme != 0) {
            os << '"';
            std::copy(t.lexeme->begin(), t.lexeme->end(),
                      std::ostream_iterator<char>(os));
            os << '"';
         }
         else
            os << "<missing>";
         return os << ')';
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
            or c == '`' or c == '\\' or c == '#';
      }

      // Move `cur' past all consecutive blank characters, and
      // return the new position.
      static const char* 
      skip_blank(const char*& cur, const char* end) {
         while (cur < end and is_blank(*cur))
            ++cur;
         return cur;
      }

      // Move `cur' until a word boundary is reached.
      static const char*
      skip_to_word_boundary(const char*& cur, const char* end) {
         while (cur < end and not is_delimiter(*cur))
            ++cur;
         return cur;
      }

      // Move `cur' one-past a non-esacaped character `c'.
      // Return true if the character was seen.
      static bool
      skip_to_nonescaped_char(const char*& cur, const char* end, char c) {
         for (; cur < end; ++cur)
            if (cur[0] == c and cur[-1] != '\\') {
               ++cur;
               return true;
            }
         return false;
      }

      // Move `cur' past the closing fence of an absolute identifier.
      // Return true if the closing fence was effectively seen.
      static inline bool
      skip_to_fence(const char*& cur, const char* end) {
         return skip_to_nonescaped_char(cur, end, '|');
      }

      // Move `cur' past the closing quote of string literal.
      // Return true if the closing fence was effectively seen.
      static inline bool
      skip_to_quote(const char*& cur, const char* end) {
         return skip_to_nonescaped_char(cur, end, '"');
      }

      // Return true if the character `c' be part of a non-absolute
      // identifier.
      static bool
      identifier_part(char c) {
         switch (c) {
         case '+': case '-': case '*': case '/': case '%': case '^':
         case '~': case '@': case '$': case '&': case ':': case '=':
         case '<': case '>': case '?': case '!': case '_':
            return true;
         default:
            return isalnum(c);
         }
      }

      // Return true if the character `c' has a special meaning after
      // the sharp character.
      static bool
      special_after_sharp(char c) {
         return c == '(' or c == '\'' or c == ':';
      }

      // Return true if the sequence `[cur, end)' has a prefix that is 
      // an integer followrd by the equal sign or the sharp sign.
      // `cur' is moved along the way.
      static bool
      only_digits_before_equal_or_shap(const char*& cur, const char* end) {
         while (cur < end and isdigit(*cur))
            ++cur;
         return cur < end and (*cur == '#' or *cur == '=');
      }

      // The token `t' was thought to designate an identifier.
      // Reclassify it as an integer if, in fact, its lexeme consists
      // entirely of digits.
      static void
      maybe_reclassify(Token& t) {
         const char* cur = t.lexeme->begin();
         const char* end = t.lexeme->end();
         while (cur < end and isdigit(*cur))
            ++cur;
         if (cur == end)
            t.type = Token::integer;
      }

      const char*
      Lexer::tokenize(const char* cur, const char* end) {
         while (skip_blank(cur, end) < end) {
            Token t = { Token::unknown, 0 };
            switch (*cur) {
            case '.': case ',': case '(': case ')':
            case '\'': case '\\':
               t.type = Token::Type(OPENAXIOM_SEXPR_TOKEN1(*cur));
               t.lexeme = strings.intern(cur, 1);
               ++cur;
               break;

            case '#': {
               const char* start = cur;
               if (cur + 1 < end and special_after_sharp(cur[1])) {
                  t.type = Token::Type(OPENAXIOM_SEXPR_TOKEN2(cur[0], cur[1]));
                  t.lexeme = strings.intern(cur, 2);
                  cur += 2;
               }
               else if (only_digits_before_equal_or_shap(++cur, end)) {
                  t.type = *cur == '#'
                     ? Token::sharp_integer_sharp
                     : Token::sharp_integer_equal;
                  t.lexeme = strings.intern(start, cur - start + 1);
                  ++cur;
               }
               else if (cur + 1 < end and cur[1] == '\\') {
                  start = cur += 2;
                  skip_to_word_boundary(cur, end);
                  t.type = Token::character;
                  t.lexeme = strings.intern(start, cur - start);
               }
               else {
                  skip_to_word_boundary(cur, end);
                  t.lexeme = strings.intern(start, cur - start);
               }
               break;
            }

            case '|': {
               const char* start = cur;
               skip_to_fence(++cur, end);
               t.type = Token::identifier;
               t.lexeme = strings.intern(start, cur - start);
               break;
            }

            case '"': {
               const char* start = cur;
               skip_to_quote(++cur, end);
               t.type = Token::string;
               t.lexeme = strings.intern(start, cur - start);
               break;
            }

            default:
               if (identifier_part(*cur)) {
                  const char* start = cur;
                  skip_to_word_boundary(++cur, end);
                  t.type = Token::identifier;
                  t.lexeme = strings.intern(start, cur - start);
                  maybe_reclassify(t);
               }
               else {
                  const char* start = cur;
                  skip_to_word_boundary(++cur, end);
                  t.lexeme = strings.intern(start, cur - start);
               }
               break;
            }
            tokens.push_back(t);
         }
         return cur;
      }

      // ----------
      // -- Atom --
      // ----------
      Atom::Atom(const Token& t) : tok(t) { }

      void
      Atom::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -------------
      // -- Integer --
      // -------------
      Integer::Integer(const Token& t) : Atom(t) { }

      void
      Integer::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ---------------
      // -- Character --
      // ---------------
      Character::Character(const Token& t) : Atom(t) { }

      void
      Character::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ------------
      // -- String --
      // ------------
      String::String(const Token& t) : Atom(t) { }

      void
      String::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ------------
      // -- Symbol --
      // ------------
      Symbol::Symbol(const Token& t, Kind k) : Atom(t), sort(k) { }

      void
      Symbol::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ------------
      // -- Anchor --
      // ------------
      Anchor::Anchor(size_t t, const Syntax* s) : tag(t), val(s) { }

      void
      Anchor::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ---------------
      // -- Reference --
      // ---------------
      Reference::Reference(const Token& t, size_t v) : Atom(t), pos(v) { }

      void
      Reference::accept(Visitor& v) const {
         v.visit(*this);
      }

      // -----------
      // -- Quote --
      // -----------
      Quote::Quote(const Syntax* s) : form(s) { }

      void
      Quote::accept(Visitor& v) const {
         v.visit(*this);
      }

      // --------------
      // -- Function --
      // --------------
      Function::Function(const Syntax* s) : form(s) { }

      void
      Function::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ----------
      // -- Pair --
      // ----------
      Pair::Pair(const Syntax* f, const Syntax* s) : elts(f, s) { }

      void
      Pair::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ----------
      // -- List --
      // ----------
      List::List() { }

      List::List(const base& elts) : base(elts) { }

      List::~List() { }

      void
      List::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ------------
      // -- Vector --
      // ------------
      Vector::Vector() { }

      Vector::Vector(const base& elts) : base(elts) { }

      Vector::~Vector() { }
      
      void
      Vector::accept(Visitor& v) const {
         v.visit(*this);
      }

      // ---------------------
      // -- Syntax::Visitor --
      // ---------------------

      // implicitly convert a reference to `T' to a reference to `S'.
      template<typename S, typename T>
      inline const S&
      as(const T& t) {
         return t;
      }

      void
      Syntax::Visitor::visit(const Integer& i) {
         visit(as<Atom>(i));
      }

      void
      Syntax::Visitor::visit(const Character& c) {
         visit(as<Atom>(c));
      }

      void
      Syntax::Visitor::visit(const String& s) {
         visit(as<Atom>(s));
      }

      void
      Syntax::Visitor::visit(const Symbol& s) {
         visit(as<Atom>(s));
      }

      void
      Syntax::Visitor::visit(const Reference& r) {
         visit(as<Atom>(r));
      }

      // ---------------
      // -- Allocator --
      // ---------------
      Allocator::Allocator() { }

      // This destructor is defined here so that it provides
      // a single instantiation point for destructors of all
      // used templates floating around.
      Allocator::~Allocator() { }

      const Integer*
      Allocator::make_integer(const Token& t) {
         return ints.allocate(t);
      }

      const String*
      Allocator::make_string(const Token& t) {
         return strs.allocate(t);
      }

      const Symbol*
      Allocator::make_symbol(const Token& t, Symbol::Kind k) {
         return syms.allocate(t, k);
      }

      const Anchor*
      Allocator::make_anchor(size_t t, const Syntax* s) {
         return ancs.allocate(t, s);
      }

      const Reference*
      Allocator::make_reference(const Token& t, size_t i) {
         return refs.allocate(t, i);
      }

      const Quote*
      Allocator::make_quote(const Syntax* s) {
         return quotes.allocate(s);
      }

      const Function*
      Allocator::make_function(const Syntax* s) {
         return funs.allocate(s);
      }

      const Pair*
      Allocator::make_pair(const Syntax* f, const Syntax* s) {
         return pairs.allocate(f, s);
      }

      const List*
      Allocator::make_list(const std::vector<const Syntax*>& elts) {
         if (elts.empty())
            return &empty_list;
         return lists.make(elts);
      }

      const Vector*
      Allocator::make_vector(const std::vector<const Syntax*>& elts) {
         if (elts.empty())
            return &empty_vector;
         return vectors.make(elts);
      }

      // ------------
      // -- Parser --
      // ------------

      // Signal a parse error
      static void
      parse_error(const std::string& s) {
         throw SystemError(s);
      }

      // Signal that an expected syntax object was missing
      static void
      expected_syntax(const std::string& s) {
         parse_error("expected " + s);
      }

      // Signal an abrupt end of input
      static void
      unexpected_end_of_input(const std::string& s) {
         parse_error("unexpected end of input after " + s);
      }

      // Signal a missing closing parenthesis
      static void
      missing_closer_for(const std::string& s) {
         parse_error("missing closing parenthesis for " + s);
      }

      // The sequence of characters in [cur, last) consists
      // entirely of digits.  Return the corresponding natural value.
      static size_t
      natural_value(const char* cur, const char* last) {
         size_t n = 0;
         for (; cur < last; ++cur)
            // FIXME: check for overflow.
            n = 10 * n + (*cur - '0');
         return n;
      }

      // Parse a plain identifier or a Lisp-style keyword identifier.
      const Symbol*
      Parser::parse_symbol(const Token*& cur, const Token* last) {
         Symbol::Kind kind = *cur->lexeme->begin() == ':'
            ? Symbol::keyword
            : Symbol::ordinary;
         return alloc.make_symbol(*cur++, kind);
      }

      const Character*
      Parser::parse_character(const Token*& cur, const Token* last) {
         // NOTE: For the time being, accept only simple characters.
         if (cur->lexeme->size() != 1)
            parse_error("invalid literal character syntax");
         return alloc.make_character(*cur++);
      }

      // Parse an anchor definition of the form #n=<syntax>
      const Anchor*
      Parser::parse_anchor(const Token*& cur, const Token* last) {
         const size_t n = natural_value(cur->lexeme->begin() + 1,
                                        cur->lexeme->end() - 1);
         if (++cur == last)
            unexpected_end_of_input("sharp-integer-equal sign");
         return alloc.make_anchor(n, parse_syntax(cur, last));
      }

      // Parse a reference to an anchor, #n#
      const Reference*
      Parser::parse_reference(const Token*& cur, const Token* last) {
         const size_t n = natural_value(cur->lexeme->begin() + 1,
                                        cur->lexeme->end() - 1);
         return alloc.make_reference(*cur++, n);
      }

      // Parse an uninterned symbol #:<identifier>
      const Symbol*
      Parser::parse_uninterned(const Token*& cur, const Token* last) {
         if (cur == last or cur->type != Token::identifier)
            expected_syntax("symbol after sharp-colon sign");
         // FIXME: check that the identifier is not a keyword.
         return alloc.make_symbol(*cur++, Symbol::uninterned);
      }

      // Parse a function syntax: #'<syntax>
      const Function*
      Parser::parse_function(const Token*& cur, const Token* last) {
         if (cur == last)
            unexpected_end_of_input("sharp-quote sign");
         return alloc.make_function(parse_syntax(cur, last));
      }

      // Parse a quotation
      const Quote*
      Parser::parse_quote(const Token*& cur, const Token* last) {
         if (cur == last)
            unexpected_end_of_input("quote sign");
         return alloc.make_quote(parse_syntax(cur, last));
      }

      // Parse a vector of syntax objects: #(s .. s)
      const Vector*
      Parser::parse_vector(const Token*& cur, const Token* last) {
         std::vector<const Syntax*> elts;
         while (cur < last and cur->type != Token::close_paren)
            elts.push_back(parse_syntax(cur, last));
         if (cur == last)
            missing_closer_for("vector");
         ++cur;
         return alloc.make_vector(elts);
      }

      // Constructs a pair or a list syntax object.
      // This function is hairy for three reasons: (a) it is not known
      // whether we list or a pair until after we have seen the
      // enclosed tokens; (b) a dot is allowed at most once; (c) Lisp-style
      // improper lists are not allowed.
      const Syntax*
      Parser::parse_list_or_pair(const Token*& cur, const Token* last) {
         std::vector<const Syntax*> elts;
         bool saw_dot = false;
         while (cur < last and cur->type != Token::close_paren) {
            if (cur->type == Token::dot) {
               if (elts.size() != 1)
                  parse_error("unexpected dot sign");
               saw_dot = true;
               ++cur;
               continue;
            }
            elts.push_back(parse_syntax(cur, last));
            if (saw_dot && elts.size() == 2)
               break;
         }
         if (cur == last or cur->type != Token::close_paren)
            missing_closer_for(saw_dot ? "pair" : "list");
         ++cur;
         if (saw_dot)
            return alloc.make_pair(elts.front(), elts.back());
         return alloc.make_list(elts);
      }

      Parser::Parser(Allocator& a, std::vector<const Syntax*>& v)
            : alloc(a), syns(v) { }

      const Syntax*
      Parser::parse_syntax(const Token*& cur, const Token* last) {
         switch (cur->type) {
         case Token::integer:
            return alloc.make_integer(*cur++);

         case Token::character:
            return parse_character(cur, last);
            
         case Token::string:
            return alloc.make_string(*cur++);
               
         case Token::identifier:
            return parse_symbol(cur, last);

         case Token::sharp_integer_equal:
            return parse_anchor(cur, last);

         case Token::sharp_integer_sharp:
            return parse_reference(cur, last);

         case Token::sharp_colon:
            return parse_uninterned(++cur, last);

         case Token::sharp_apostrophe:
            return parse_function(++cur, last);

         case Token::sharp_open_paren:
            return parse_vector(++cur, last);

         case Token::apostrophe:
            return parse_quote(++cur, last);

         case Token::open_paren:
            return parse_list_or_pair(++cur, last);

         default:
            parse_error(std::string("parse error before ")
                        + cur->lexeme->begin());
            return 0;           // never executed
         }
      }

      const Token*
      Parser::parse(const Token* cur, const Token* last) {
         while (cur < last) 
            syns.push_back(parse_syntax(cur, last));
         return cur;
      }
   }
}
