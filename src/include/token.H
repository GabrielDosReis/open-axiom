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
      using base_type = uint32_t;

      // -- 8-bit byte data type
      using u8 = uint8_t;

      constexpr base_type value(u8 c) { return c; }
      constexpr base_type value(u8 hi, u8 lo) { return (hi << 8) | lo; }
      constexpr base_type value(u8 hi, u8 mi, u8 lo) {
         return (value(hi, mi) << 8) | lo;
      }

      // -- Type of literal strings of given number of characters.
      template<int N>
      using text_chunk = const char(&)[N+1];

      // -- Return the token value of certain literal strings.
      constexpr base_type value(text_chunk<0>) { return u8(); }
      constexpr base_type value(text_chunk<1> s) {
         return value(s[0]);
      }
      constexpr base_type value(text_chunk<2> s) {
         return value(s[0], s[1]);
      }
      constexpr base_type value(text_chunk<3> s) {
         return value(s[0], s[1], s[2]);
      }

      // -- Abstract values of tokens.
      enum Value : base_type {
         Unknown                = value(""),
         Bar                    = value("|"),
         Dot                    = value("."),
         DotDot                 = value(".."),
         Colon                  = value(":"),
         ColonColon             = value("::"),
         ColonDash              = value(":-"),
         ColonEq                = value(":="),
         At                     = value("@"),
         Comma                  = value(","),
         Semicolon              = value(";"),
         Star                   = value("*"),
         Plus                   = value("+"),
         Minus                  = value("-"),
         Slash                  = value("/"),
         Backslash              = value("\\"),
         SlashSlash             = value("//"),
         BackslashBackslash     = value("\\\\"),
         BackslashSlash         = value("\\/"),
         SlashBackslash         = value("/\\"),
         Less                   = value("<"),
         LessEq                 = value("<="),
         Greater                = value(">"),
         GreaterEq              = value(">="),
         Eq                     = value("="),
         EqEq                   = value("=="),
         Tilde                  = value("~"),
         TildeEq                = value("~="),
         Caret                  = value("^"),
         Pound                  = value("#"),
         Dollar                 = value("$"),
         Ampersand              = value("&"),
         OpenParen              = value("("),
         CloseParen             = value(")"),
         OpenBracket            = value("["),
         CloseBracket           = value("]"),
         OpenBrace              = value("{"),
         CloseBrace             = value("}"),
         OpenMetParen           = value("(|"),
         CloseMetaParen         = value("|)"),
         OpenMetaBracket        = value("[|"),
         CloseMetaBracket       = value("|]"),
         OpenMetaBrace          = value("{|"),
         CloseMetaBrace         = value("|}"),
         Apostrophe             = value("'"),
         Backquote              = value("`"),
         StarStar               = value("**"),
         Implies                = value("=>"),
         RightArrow             = value("->"),
         LeftArrow              = value("<-"),
         OpenChevron            = value("<<"),
         CloseChevron           = value(">>"),
         FatArrow               = value("==>"),
         Equiv                  = value("<=>"),
         MapsTo                 = value("+->"),

         Add                    = value("add"),
         And                    = value("and"),
         By                     = value("by"),
         Do                     = value("do"),
         For                    = value("for"),
         Has                    = value("has"),
         If                     = value("if"),
         In                     = value("in"),
         Is                     = value("is"),
         Mod                    = value("mod"),
         Of                     = value("of"),    // -- Boot only
         Or                     = value("or"),
         Quo                    = value("quo"),
         Rem                    = value("rem"),
         Try                    = value("try"),
         LastTrigraph           = 0xffffff,

         Assume,             // "assume"
         Break,              // "break"
         Case,               // "case"
         Catch,              // "catch"
         Cross,              // "cross"
         Else,               // "else"
         Exists,             // "exists"
         Finally,            // "finally"
         From,               // "from"
         Forall,             // "forall"
         Function,           // "function" -- Boot only
         Import,             // "import"
         Inline,             // "inline"
         Isnt,               // "isnt"
         Iterate,            // "iterate"
         Leave,              // "leave"
         Macro,              // "macro"
         Module,             // "module"   -- Boot only
         Namespace,          // "namespace" -- Boot only
         Pretend,            // "pretend"
         Repeat,             // "repeat"
         Return,             // "return"
         Rule,               // "rule"
         Structure,          // "structure" -- Boot only
         Then,               // "then"
         Throw,              // "throw"
         Until,              // "until"
         With,               // "with"
         Where,              // "where"
         While,              // "while"

         IntegerLiteral,     // integer literal
         StringLiteral,      // string literal
         FPLiteral,          // floating point literal
         Indent,             // new line indentation, greater than previous
         Unindent,           // new line indentation, less than previous
         Justify,            // align indentation with preceding line.
      };
   }
}

#endif  // OPENAXIOM_TOKEN_included
