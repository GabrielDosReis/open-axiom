// -- Copyright (C) 2013-2024, Gabriel Dos Reis.
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
   std::ostream& operator<<(std::ostream& os, TokenCategory tc)
   {
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


   bool separator_or_punctuator(char8_t c)
   {
      switch (c)
      {
      case u8'.': case u8'`': case u8'^': case u8'&': case u8'~': case u8'*':
      case u8'-': case u8'+': case u8';': case u8',': case u8'@': case u8'|':
      case u8'\'': case u8':': case u8'=': case u8'\\': case u8'"': case u8'/':
      case u8'(': case u8')': case u8'{': case u8'}': case u8'[': case u8']':
      case u8'<': case u8'>': case u8'#': case u8' ':
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

   TokenClassification classify(std::string_view s, Language dialect)
   {
      for (auto& t : token_map) {
         if (t.text == s
             and (uint8_t(t.dialect) & uint8_t(dialect)) != 0)
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

   // -- Pile phase - mirrors the algorithm of pile.boot.
   //
   //  Given a flat token vector (with formatting tokens from the
   //  tokenizer), this phase:
   //    1. Strips existing formatting/whitespace/comment tokens.
   //    2. Groups remaining tokens into logical "lines" by their
   //       source line number.
   //    3. Coagulates adjacent lines when:
   //       - the next line starts with `then` or `else`, or
   //       - the current line ends with an infix operator,
   //         comma, or semicolon.
   //    4. Recursively builds a pile tree based on indentation
   //       columns.
   //    5. Inserts Indent/Justify/Unindent tokens to delimit
   //       pile structures.
   namespace {
      // -- A "line" in the pile sense: a contiguous run of content
      // tokens from the same source line (or coagulated from
      // adjacent source lines).
      struct PileLine {
         std::vector<Token> tokens;

         bool empty() const { return tokens.empty(); }

         ColumnIndex column() const {
            return tokens.front().start.column;
         }

         // -- The value of the last non-comment token on this line.
         TokenValue last_value() const {
            for (auto it = tokens.rbegin(); it != tokens.rend(); ++it)
               if (it->category != TokenCategory::Comment)
                  return it->value;
            return TokenValue::EndOfStream;
         }

         // -- The value of the first non-comment token on this line.
         TokenValue first_value() const {
            for (auto& t : tokens)
               if (t.category != TokenCategory::Comment)
                  return t.value;
            return TokenValue::EndOfStream;
         }

         // -- Position of the first token (for synthesizing formatting tokens).
         FragmentCursor first_pos() const {
            return tokens.front().start;
         }

         // -- Position of the last token (for synthesizing formatting tokens).
         FragmentCursor last_pos() const {
            return tokens.back().end;
         }

         // -- True if this line consists entirely of descriptions (++).
         // Description-only lines form their own piles and must not
         // be coagulated with adjacent code lines.  Wisecracks (--)
         // do not have this restriction.
         bool description_only_p() const {
            for (auto& t : tokens)
               if (t.value != TokenValue::Commentary)
                  return false;
            return true;
         }
      };

      // -- True if `v` is an infix operator (the SHOEINF set from pile.boot).
      bool is_infix(TokenValue v) {
         switch (v) {
         case TokenValue::Eq:
         case TokenValue::Star:
         case TokenValue::StarStar:
         case TokenValue::Plus:
         case TokenValue::Minus:
         case TokenValue::Slash:
         case TokenValue::Less:
         case TokenValue::LessEq:
         case TokenValue::Greater:
         case TokenValue::GreaterEq:
         case TokenValue::TildeEq:
         case TokenValue::And:
         case TokenValue::Or:
         case TokenValue::Is:
         case TokenValue::Isnt:
         case TokenValue::Rem:
         case TokenValue::Quo:
            return true;
         default:
            return false;
         }
      }

      // -- True if `v` is a token that triggers coagulation when it
      // ends the current line.
      bool coagulate_trailing(TokenValue v) {
         return is_infix(v)
            or v == TokenValue::Comma
            or v == TokenValue::Semicolon;
      }

      // -- True if `v` is a token that triggers coagulation when it
      // starts the next line.
      bool coagulate_leading(TokenValue v) {
         return v == TokenValue::Then
            or v == TokenValue::Else;
      }

      // -- Make a synthetic formatting token at a given position.
      Token make_formatting(TokenValue v, FragmentCursor pos) {
         Token t { };
         t.start = pos;
         t.end = pos;
         t.category = TokenCategory::Formatting;
         t.value = v;
         return t;
      }

      // -- True if `v` opens a grouping delimiter.
      bool is_open_delimiter(TokenValue v) {
         switch (v) {
         case TokenValue::OpenParen:
         case TokenValue::OpenBracket:
         case TokenValue::OpenBrace:
         case TokenValue::OpenMetaParen:
         case TokenValue::OpenMetaBracket:
         case TokenValue::OpenMetaBrace:
            return true;
         default:
            return false;
         }
      }

      // -- True if `v` closes a grouping delimiter.
      bool is_close_delimiter(TokenValue v) {
         switch (v) {
         case TokenValue::CloseParen:
         case TokenValue::CloseBracket:
         case TokenValue::CloseBrace:
         case TokenValue::CloseMetaParen:
         case TokenValue::CloseMetaBracket:
         case TokenValue::CloseMetaBrace:
            return true;
         default:
            return false;
         }
      }

      // -- Step 1 & 2: strip whitespace and formatting tokens, keep
      //    comments (wisecracks and descriptions attach to definitions),
      //    and group remaining tokens by source line.
      //    Inside balanced grouping delimiters (parens, brackets,
      //    braces), all tokens belong to the same logical line
      //    regardless of source line breaks - mirroring the
      //    tokenizer's group_depth suppression of formatting tokens.
      std::vector<PileLine> group_into_lines(const std::vector<Token>& raw) {
         std::vector<PileLine> lines;
         PileLine current;
         int group_depth = 0;
         for (auto& t : raw) {
            if (t.category == TokenCategory::Whitespace
                or t.category == TokenCategory::Formatting)
               continue;
            // -- Only split on line boundaries when outside delimiters.
            if (group_depth == 0
                and not current.empty()
                and t.start.line != current.tokens.back().start.line) {
               lines.push_back(std::move(current));
               current = PileLine{};
            }
            current.tokens.push_back(t);
            if (is_open_delimiter(t.value))
               ++group_depth;
            else if (is_close_delimiter(t.value) and group_depth > 0)
               --group_depth;
         }
         if (not current.empty())
            lines.push_back(std::move(current));
         return lines;
      }

      // -- Steps 3-5: pile tree building with coagulation.
      //
      //  This faithfully mirrors pile.boot's algorithm:
      //    - pile_tree / eq_pile_tree: find sub-trees by column
      //    - pile_forest: collect a forest of sibling sub-piles
      //    - pile_forests: process head + merge children
      //    - pile_cforest: format a forest (coagulate, then wrap)
      //    - pile_coagulate: merge adjacent sub-piles per SHOEINF
      //
      //  Coagulation happens during forest emission (not before
      //  pile tree building), matching pile.boot's shoePileCforest.
      //  Single children are NOT wrapped with Indent/Unindent,
      //  matching pile.boot's "rest x = nil => first x" check.

      // -- Result of pile_tree / eq_pile_tree / pile_forests.
      struct PileTreeResult {
         bool found;
         std::vector<Token> tree;
         std::size_t next;
      };

      // -- Result of pile_forest.
      struct ForestResult {
         std::vector<std::vector<Token>> children;
         std::size_t next;
      };

      // -- Forward declaration for mutual recursion.
      PileTreeResult pile_forests(const std::vector<PileLine>& lines,
                                  std::size_t pos, int n);

      // -- pile_tree: consume one sub-tree at column > n.
      // Mirrors shoePileTree(n, s) from pile.boot.
      PileTreeResult pile_tree(const std::vector<PileLine>& lines,
                               std::size_t pos, int n) {
         if (pos >= lines.size())
            return { false, {}, pos };
         if (static_cast<int>(lines[pos].column()) <= n)
            return { false, {}, pos };
         return pile_forests(lines, pos,
                             static_cast<int>(lines[pos].column()));
      }

      // -- eq_pile_tree: consume one sub-tree at column == n.
      // Mirrors eqshoePileTree(n, s) from pile.boot.
      PileTreeResult eq_pile_tree(const std::vector<PileLine>& lines,
                                  std::size_t pos, int n) {
         if (pos >= lines.size())
            return { false, {}, pos };
         if (static_cast<int>(lines[pos].column()) != n)
            return { false, {}, pos };
         return pile_forests(lines, pos,
                             static_cast<int>(lines[pos].column()));
      }

      // -- pile_forest: collect a forest of sibling sub-piles.
      // Mirrors shoePileForest + shoePileForest1 from pile.boot.
      ForestResult pile_forest(const std::vector<PileLine>& lines,
                               std::size_t pos, int n) {
         auto first = pile_tree(lines, pos, n);
         if (not first.found)
            return { {}, pos };

         // -- Column of the first child - siblings must match this.
         int child_col = static_cast<int>(lines[pos].column());

         std::vector<std::vector<Token>> forest;
         forest.push_back(std::move(first.tree));
         auto next = first.next;

         // -- Collect remaining siblings at column == child_col.
         while (next < lines.size()) {
            auto sib = eq_pile_tree(lines, next, child_col);
            if (not sib.found)
               break;
            forest.push_back(std::move(sib.tree));
            next = sib.next;
         }

         return { std::move(forest), next };
      }

      // -- Helpers for sub-pile token inspection --

      // -- First non-comment, non-formatting token value.
      TokenValue first_content_value(const std::vector<Token>& tokens) {
         for (auto& t : tokens)
            if (t.category != TokenCategory::Comment
                and t.category != TokenCategory::Formatting)
               return t.value;
         return TokenValue::EndOfStream;
      }

      // -- Last non-comment, non-formatting token value.
      TokenValue last_content_value(const std::vector<Token>& tokens) {
         for (auto it = tokens.rbegin(); it != tokens.rend(); ++it)
            if (it->category != TokenCategory::Comment
                and it->category != TokenCategory::Formatting)
               return it->value;
         return TokenValue::EndOfStream;
      }

      // -- True if a sub-pile consists entirely of descriptions (++).
      bool description_only_pile_p(const std::vector<Token>& pile) {
         for (auto& t : pile)
            if (t.category != TokenCategory::Formatting
                and t.value != TokenValue::Commentary)
               return false;
         return not pile.empty();
      }

      // -- pile_coagulate: merge adjacent sub-piles.
      // Mirrors shoePileCoagulate from pile.boot.
      // Merges when:
      //   - next sub-pile starts with THEN or ELSE, or
      //   - current sub-pile ends with infix / comma / semicolon.
      // Description-only sub-piles are never merged.
      std::vector<std::vector<Token>> pile_coagulate(
            std::vector<std::vector<Token>> forest) {
         if (forest.size() <= 1)
            return forest;
         std::vector<std::vector<Token>> result;
         result.push_back(std::move(forest[0]));
         for (std::size_t i = 1; i < forest.size(); ++i) {
            auto& prev = result.back();
            if (not description_only_pile_p(prev)
                and not description_only_pile_p(forest[i])
                and (coagulate_leading(first_content_value(forest[i]))
                     or coagulate_trailing(last_content_value(prev)))) {
               for (auto& t : forest[i])
                  prev.push_back(std::move(t));
            } else {
               result.push_back(std::move(forest[i]));
            }
         }
         return result;
      }

      // -- separate_piles: insert Justify (BACKSET) between sub-piles.
      // Mirrors shoeSeparatePiles from pile.boot.
      std::vector<Token> separate_piles(
            const std::vector<std::vector<Token>>& piles) {
         std::vector<Token> result;
         for (std::size_t i = 0; i < piles.size(); ++i) {
            if (i > 0) {
               // -- Justify position at last token of preceding pile,
               // matching shoeLastTokPosn in shoeSeparatePiles.
               auto pos = piles[i - 1].back().start;
               result.push_back(make_formatting(TokenValue::Justify, pos));
            }
            for (auto& t : piles[i])
               result.push_back(t);
         }
         return result;
      }

      // -- en_pile: wrap with Indent/Unindent.
      // Mirrors shoeEnPile from pile.boot.
      std::vector<Token> en_pile(std::vector<Token> tokens) {
         if (tokens.empty())
            return {};
         auto indent_pos = tokens.front().start;
         auto unindent_pos = tokens.back().start;
         std::vector<Token> result;
         result.reserve(tokens.size() + 2);
         result.push_back(make_formatting(TokenValue::Indent, indent_pos));
         for (auto& t : tokens)
            result.push_back(std::move(t));
         result.push_back(make_formatting(TokenValue::Unindent, unindent_pos));
         return result;
      }

      // -- pile_cforest: process a forest of sub-piles.
      // Mirrors shoePileCforest from pile.boot:
      //   0 children > empty
      //   1 child > just the child (no wrapping)
      //   2+ children > coagulate, then:
      //     1 after coag > just the result (no wrapping)
      //     2+ after coag > wrap with Indent + Justify + Unindent
      std::vector<Token> pile_cforest(
            std::vector<std::vector<Token>> forest) {
         if (forest.empty())
            return {};
         if (forest.size() == 1)
            return std::move(forest[0]);

         auto coagulated = pile_coagulate(std::move(forest));
         if (coagulated.size() == 1)
            return std::move(coagulated[0]);

         return en_pile(separate_piles(coagulated));
      }

      // -- pile_forests: process head line + merge children.
      // Mirrors shoePileForests from pile.boot:
      //   1. Collect children via pile_forest
      //   2. If no children, return head as-is
      //   3. Process children with pile_cforest (coagulate + wrap)
      //   4. Append processed children to head
      //   5. Loop (in pile.boot this is tail-recursive; here it's
      //      a loop - the second iteration always finds no more
      //      children since pile_forest already consumed them all).
      PileTreeResult pile_forests(const std::vector<PileLine>& lines,
                                  std::size_t pos, int n) {
         // -- Start with head line's tokens.
         std::vector<Token> head(lines[pos].tokens.begin(),
                                 lines[pos].tokens.end());
         auto next = pos + 1;

         for (;;) {
            auto [children, child_next] = pile_forest(lines, next, n);
            if (children.empty())
               return { true, std::move(head), next };

            // -- shoePileCtree: append processed forest to head.
            auto processed = pile_cforest(std::move(children));
            head.insert(head.end(), processed.begin(), processed.end());
            next = child_next;
         }
      }
   } // anonymous namespace

   std::vector<Token> pile(const std::vector<Token>& raw) {
      // -- Step 1-2: strip formatting and group into lines.
      auto lines = group_into_lines(raw);
      if (lines.empty())
         return {};

      // -- Steps 3-5: build pile tree and emit.
      // Coagulation happens inside pile_cforest, not here.
      std::vector<Token> out;
      std::size_t pos = 0;
      while (pos < lines.size()) {
         auto result = pile_tree(lines, pos, -1);
         if (result.found) {
            for (auto& t : result.tree)
               out.push_back(std::move(t));
            pos = result.next;
         } else {
            // -- Safety fallback (column > -1 is always true for
            // real columns, so this shouldn't happen).
            for (auto& t : lines[pos].tokens)
               out.push_back(t);
            ++pos;
         }
      }
      return out;
   }
}
