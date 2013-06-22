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

#ifndef OPENAXIOM_TOKEN_included
#define OPENAXIOM_TOKEN_included

#include <stdint.h>
#include <open-axiom/Input>

namespace OpenAxiom {
   namespace token {
      // -- Underlying representation of a token class.
      using class_type = uint32_t;

      // -- 8-bit byte data type
      using u8 = uint8_t;

      // -- Number of bits per basic char value
      constexpr auto char_bits = 8;

      // -- Type of literal strings of given number of characters.
      template<int N>
      using text_chunk = const char(&)[N+1];

      // -- Return the token value of certain literal strings.
      constexpr class_type value(text_chunk<0>) { return u8(); }
      constexpr class_type value(text_chunk<1> s) { return u8(s[0]); }
      constexpr class_type value(text_chunk<2> s) {
         return (u8(s[0]) << char_bits) | u8(s[0]);
      }
      constexpr class_type value(text_chunk<3> s) {
         return (u8(s[0]) << (2 * char_bits))
            | (u8(s[1]) << char_bits)
            | u8(s[2]);
      }

      // -- Abstract values of tokens.
      enum Value {
         unknown_tv             = value(""),
         bar_tv                 = value("|"),
         dot_tv                 = value("."),
         dot_dot_tv             = value(".."),
         colon_tv               = value(":"),
         colon_colon_tv         = value("::"),
         colon_dash_tv          = value(":-"),
         colon_eq_tv            = value(":="),
         at_tv                  = value("@"),
         comma_tv               = value(","),
         semicolon_tv           = value(";"),
         star_tv                = value("*"),
         plus_tv                = value("+"),
         minus_tv               = value("-"),
         slash_tv               = value("/"),
         backslash_tv           = value("\\"),
         slash_slash_tv         = value("//"),
         backslash_backslash_tv = value("\\\\"),
         backslash_slash_tv     = value("\\/"),
         slash_backslash_tv     = value("/\\"),
         less_tv                = value("<"),
         less_eq_tv             = value("<="),
         greater_tv             = value(">"),
         greater_eq_tv          = value(">="),
         eq_tv                  = value("="),
         eq_eq_tv               = value("=="),
         tilde_tv               = value("~"),
         tilde_eq_tv            = value("~="),
         caret_tv               = value("^"),
         pound_tv               = value("#"),
         dollar_tv              = value("$"),
         ampersand_tv           = value("&"),
         open_paren_tv          = value("("),
         close_paren_tv         = value(")"),
         open_bracket_tv        = value("["),
         close_bracket_tv       = value("]"),
         open_brace_tv          = value("{"),
         close_brace_tv         = value("}"),
         open_meta_paren_tv     = value("(|"),
         close_meta_paren_tv    = value("|)"),
         open_meta_bracket_tv   = value("[|"),
         close_meta_bracket_tv  = value("|]"),
         open_meta_brace_tv     = value("{|"),
         close_meta_brace_tv    = value("|}"),
         apostrophe_tv          = value("'"),
         backquote_tv           = value("`"),
         star_star_tv           = value("**"),
         implies_tv             = value("=>"),
         right_arrow_tv         = value("->"),
         left_arrow_tv          = value("<-"),
         open_chevron_tv        = value("<<"),
         close_chevron_tv       = value(">>"),
         fat_arrow_tv           = value("==>"),
         equiv_tv               = value("<=>"),
         mapsto_tv              = value("+->"),

         add_tv                 = value("add"),
         and_tv                 = value("and"),
         by_tv                  = value("by"),
         do_tv                  = value("do"),
         for_tv                 = value("for"),
         has_tv                 = value("has"),
         if_tv                  = value("if"),
         in_tv                  = value("in"),
         is_tv                  = value("is"),
         mod_tv                 = value("mod"),
         of_tv                  = value("of"),    // -- Boot only
         or_tv                  = value("or"),
         quo_tv                 = value("quo"),
         rem_tv                 = value("rem"),
         try_tv                 = value("try"),
         last_trigraph_tv       = value("\xff\xff\xff"),

         assume_tv,             // "assume"
         break_tv,              // "break"
         case_tv,               // "case"
         catch_tv,              // "catch"
         cross_tv,              // "cross"
         else_tv,               // "else"
         exist_tv,              // "exist"
         finally_tv,            // "finally"
         from_tv,               // "from"
         forall_tv,             // "forall"
         function_tv,           // "function" -- Boot only
         import_tv,             // "import"
         inline_tv,             // "inline"
         isnt_tv,               // "isnt"
         iterate_tv,            // "iterate"
         leave_tv,              // "leave"
         macro_tv,              // "macro"
         module_tv,             // "module"   -- Boot only
         namespace_tv,          // "namespace" -- Boot only
         pretend_tv,            // "pretend"
         repeat_tv,             // "repeat"
         return_tv,             // "return"
         rule_tv,               // "rule"
         structure_tv,          // "structure" -- Boot only
         then_tv,               // "then"
         throw_tv,              // "throw"
         until_tv,              // "until"
         with_tv,               // "with"
         where_tv,              // "where"
         while_tv,              // "while"

         integer_literal_tv,    // integer literal
         string_literal_tv,     // string literal
         fp_literal_tv,         // floating point literal
         indent_tv,             // new line indentation, greater than previous
         unindent_tv,           // new line indentation, less than previous
         justify_tv,            // align indentation with preceding line.
      };
   }
}

#endif  // OPENAXIOM_TOKEN_included
