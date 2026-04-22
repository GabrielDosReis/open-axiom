// -- -*- C++ -*-
// Copyright (C) 2014-2026, Gabriel Dos Reis.
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
//     - Neither the name of OpenAxiom, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
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
// --%   Recursive-descent parser for the Boot language.
// --%
// --%   Boot is the implementation language of OpenAxiom.  It is a
// --%   statically-scoped, mostly-functional language with Lisp-like
// --%   S-expression quoting, ML-like pattern matching (is/isnt),
// --%   significant indentation (the "off-side rule"), and algebraic
// --%   notation for arithmetic and function application.
// --%
// --%   This parser operates on a flat vector of tokens produced by
// --%   Tokenizer<Token> (see <open-axiom/token>).  The tokenizer has
// --%   already resolved physical lines into logical lines within each
// --%   Fragment, and has injected three kinds of formatting tokens at
// --%   line boundaries:
// --%     Indent   - new line is deeper than the current indentation,
// --%     Unindent - new line is shallower,
// --%     Justify  - new line is at the same level.
// --%
// --%   These formatting tokens encode the off-side rule: an Indent
// --%   opens a new "pile" (a vertically-aligned block of items), and
// --%   the matching Unindent closes it.  Justify separates adjacent
// --%   items within the same pile.
// --%
// --%   The parser uses the formatting tokens only at the "pile"
// --%   level of the grammar.  Inside parentheses, brackets, or braces,
// --%   `delimiter_depth > 0` tracks the nesting, and certain constructs
// --%   (like `==` as default-value) are disambiguated by it.
// --%
// --% Grammar (operator precedence, lowest to highest):
// --%
// --%   toplevel      > comma
// --%   comma         > module | import | namespace | tuple_of(where)
// --%   semicolon     > semi_list(comma)
// --%   where         > definition [ 'where' local-defs ]
// --%   definition    > 'macro' name params '==' where
// --%                  | exit [ '==' (where | pile) ]
// --%   exit          > assign [ '=>' where ]
// --%   assign        > statement [ ':=' assign | '+->' assign | '<-' logical ]
// --%   statement     > conditional | loop | try | expression
// --%   expression    > [':'] logical
// --%   logical       > return_expr { 'or' return_expr }
// --%   return_expr   > 'return' assign | 'leave' logical
// --%                  | 'throw' application | 'do' statement
// --%                  | and_expr
// --%   and_expr      > compare { 'and' compare }
// --%   compare       > is_expr [ comp_op is_expr | 'in' is_expr ]
// --%   is_expr       > segment [ 'is' pattern | 'isnt' pattern | 'has' application ]
// --%   segment       > arith [ '..' arith ]
// --%   arith         > minus { ('+' | '-') minus }
// --%   minus         > ['-'] euclid
// --%   euclid        > times { ('quo' | 'rem') times }
// --%   times         > reduce | expt { ('*' | '/') expt }
// --%   reduce        > op '/' (construct | application)
// --%   expt          > typed { '**' typed }
// --%   typed         > application [ '@' typing | '::' typing
// --%                                | ':' typing | '->' typed ]
// --%   typing        > 'forall' vars '.' typing | application [ '->' application ]
// --%   application   > primary { '.' primary | '(' args ')' | juxtaposition }
// --%   primary       > ''' sexp | '(' semicolon ')' | '[' construct ']'
// --%                  | '.' | prefix_op primary | 'leave' primary
// --%                  | 'if' ... | 'case' ... | 'structure' ... | 'function' primary
// --%                  | pile | 'namespace' name | name | literal
// --%
// --% Pattern grammar (used after 'is' and 'isnt'):
// --%   pattern       > pattern_item
// --%   pattern_item  > '=' expr | ':' pattern_item | '[' pattern_list ']'
// --%                  | '.' | ''' sexp | constant
// --%                  | name [ '::' name ] [ ':=' pattern_item ]
// --%   pattern_list  > pattern_item { ',' pattern_item }
// --%
// --% S-expression grammar (used after quote '):
// --%   sexp          > '-' INTEGER | ''' sexp
// --%                  | '(' { sexp } [ '.' sexp ] ')'
// --%                  | atom
// --%   sexp_atom     > identifier | integer | float | string
// --%                  | keyword-as-symbol | operator-as-symbol

#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include <open-axiom/Charset>
#include <open-axiom/diagnostics>
#include <open-axiom/FileMapping>
#include <open-axiom/InputFragment>
#include <open-axiom/Parser>
#include <open-axiom/SyntaxTree>
#include <open-axiom/token>

namespace OpenAxiom::Boot {
    using Syntax::NodeIndex;
    using Syntax::NodeSpan;
    using Syntax::SyntaxForest;
    using Syntax::Kind;

    namespace {

        // -- Operator classification

        // -- Return true if the token value is a prefix operator.
        bool prefix_operator(TokenValue v)
        {
            switch (v)
            {
            case TokenValue::Sharp:
            case TokenValue::Minus:
            case TokenValue::Tilde:
                return true;
            default:
                return false;
            }
        }

        // -- Return true if the token value is an infix operator.
        bool infix_operator(TokenValue v)
        {
            switch (v)
            {
            case TokenValue::Plus:
            case TokenValue::Minus:
            case TokenValue::Star:
            case TokenValue::Slash:
            case TokenValue::StarStar:
            case TokenValue::Quo:
            case TokenValue::Rem:
            case TokenValue::Mod:
            case TokenValue::And:
            case TokenValue::Or:
            case TokenValue::Less:
            case TokenValue::LessEq:
            case TokenValue::Greater:
            case TokenValue::GreaterEq:
            case TokenValue::Eq:
            case TokenValue::TildeEq:
                return true;
            default:
                return false;
            }
        }

        // -- Return true if the token value can head a reduce.
        bool reduce_operator(TokenValue v)
        {
            return infix_operator(v) or v == TokenValue::Sharp;
        }

        // -- Return true if the token is a comparison operator.
        bool comparison_operator(TokenValue v)
        {
            switch (v)
            {
            case TokenValue::Eq:
            case TokenValue::TildeEq:
            case TokenValue::Less:
            case TokenValue::LessEq:
            case TokenValue::Greater:
            case TokenValue::GreaterEq:
                return true;
            default:
                return false;
            }
        }

        // -- Operator predicates for left-associative chains.
        bool comparison_or_in_operator(TokenValue v)
        {
            return comparison_operator(v) or v == TokenValue::In;
        }

        bool exponentiation_operator(TokenValue v)
        {
            return v == TokenValue::StarStar;
        }

        bool multiplicative_operator(TokenValue v)
        {
            return v == TokenValue::Star or v == TokenValue::Slash;
        }

        bool euclidean_operator(TokenValue v)
        {
            return v == TokenValue::Quo or v == TokenValue::Rem;
        }

        bool additive_operator(TokenValue v)
        {
            return v == TokenValue::Plus or v == TokenValue::Minus;
        }

        bool conjunction_operator(TokenValue v)
        {
            return v == TokenValue::And;
        }

        bool disjunction_operator(TokenValue v)
        {
            return v == TokenValue::Or;
        }

        // -- Return true if the token can start an iterator or guard clause.
        bool starts_iterator(TokenValue v)
        {
            return v == TokenValue::For
                or v == TokenValue::While
                or v == TokenValue::Until
                or v == TokenValue::Bar;
        }

        // -- Return true if the token is a structural delimiter that
        //    terminates S-expression lists.  These tokens cannot
        //    appear as atoms in S-expressions.
        bool sexp_structural_delimiter(TokenValue v)
        {
            switch (v)
            {
            case TokenValue::OpenParen:
            case TokenValue::CloseParen:
            case TokenValue::OpenBracket:
            case TokenValue::CloseBracket:
            case TokenValue::OpenBrace:
            case TokenValue::CloseBrace:
            case TokenValue::Comma:
            case TokenValue::Semicolon:
            case TokenValue::Dot:
            case TokenValue::Apostrophe:
            case TokenValue::Backquote:
            case TokenValue::Indent:
            case TokenValue::Unindent:
            case TokenValue::Justify:
            case TokenValue::EndOfStream:
                return true;
            default:
                return false;
            }
        }

        // -- Return true if the token is a Spad structural keyword
        //    that cannot be used as an identifier name.
        bool spad_structural_keyword(TokenValue v)
        {
            switch (v)
            {
            case TokenValue::If:
            case TokenValue::Then:
            case TokenValue::Else:
            case TokenValue::For:
            case TokenValue::While:
            case TokenValue::Until:
            case TokenValue::Repeat:
            case TokenValue::Where:
            case TokenValue::With:
            case TokenValue::Add:
            case TokenValue::Import:
            case TokenValue::Return:
            case TokenValue::From:
            case TokenValue::Macro:
            case TokenValue::Inline:
            case TokenValue::Free:
            case TokenValue::Export:
            case TokenValue::Do:
            case TokenValue::EqEq:
            case TokenValue::ColonEq:
            case TokenValue::Implies:
            case TokenValue::MapsTo:
            case TokenValue::FatArrow:
                return true;
            default:
                return false;
            }
        }

        // -- Return true if the token terminates juxtaposition application.
        //
        //    In Boot, function application by juxtaposition (`f x`)
        //    must not consume tokens that belong to an enclosing
        //    context.  This classifier lists every token value that
        //    ends a juxtaposition chain: infix operators, delimiters,
        //    assignment/definition operators, formatting tokens,
        //    keywords that start new clauses, and type operators.
        bool terminates_juxtaposition(TokenValue v)
        {
            if (infix_operator(v))
                return true;
            switch (v)
            {
            case TokenValue::EndOfStream:
            case TokenValue::Comma:
            case TokenValue::Semicolon:
            case TokenValue::CloseParen:
            case TokenValue::CloseBracket:
            case TokenValue::CloseBrace:
            case TokenValue::CloseMetaParen:
            case TokenValue::CloseMetaBracket:
            case TokenValue::CloseMetaBrace:
            case TokenValue::ColonEq:
            case TokenValue::EqEq:
            case TokenValue::Implies:
            case TokenValue::MapsTo:
            case TokenValue::LeftArrow:
            case TokenValue::Then:
            case TokenValue::Else:
            case TokenValue::Where:
            case TokenValue::Indent:
            case TokenValue::Unindent:
            case TokenValue::Justify:
            case TokenValue::Is:
            case TokenValue::Isnt:
            case TokenValue::Has:
            case TokenValue::By:
            case TokenValue::In:
            case TokenValue::Cross:
            case TokenValue::Repeat:
            case TokenValue::Catch:
            case TokenValue::Finally:
            case TokenValue::With:
            case TokenValue::Colon:
            case TokenValue::DotDot:
            case TokenValue::ColonColon:
            case TokenValue::At:
            case TokenValue::RightArrow:
            case TokenValue::Pretend:
            case TokenValue::Add:
            case TokenValue::FatArrow:
            case TokenValue::Free:
            case TokenValue::Export:
            case TokenValue::From:
                return true;
            default:
                return false;
            }
        }

        // -- ParseError: diagnostic for syntax errors
        struct ParseError : Diagnostics::BasicError {
            LineNumber line;
            ColumnIndex column;
            explicit ParseError(const std::string& msg,
                                LineNumber l = 0, ColumnIndex c = 0)
                : BasicError(msg), line(l), column(c)
            { }
        };

        // -- ParserState

        // -- Central mutable state for the recursive-descent parser.
        //
        // The parser consumes a flat `vector<Token>` using a cursor
        // (`position`).  Parsed sub-expressions are communicated
        // between grammar rules via an explicit `stack` of NodeIndex
        // values: a production pushes its result, and the caller pops
        // it.  This avoids returning NodeIndex from every function
        // (many productions are "bool: did we match?").
        //
        // Key fields:
        //   `delimiter_depth` - nesting depth inside balanced
        //       delimiters (parens, brackets, braces).  Used for
        //       semantic decisions (e.g. `name == value` inside
        //       parens is a default-value, not a definition).
        //       The pile phase never emits formatting tokens inside
        //       balanced delimiters, so this field has no formatting
        //       role.
        //   `forest` - arena allocator for AST nodes (SyntaxForest).
        //
        // Snapshot/restore: several grammar rules need tentative
        // parsing (try one alternative, backtrack on failure).  The
        // Snapshot struct captures the full parser state (position,
        // stack depth, delimiter_depth) so it can be cheaply restored.
        struct ParserState {
            const std::vector<Token>& tokens;
            std::uint32_t position;
            int delimiter_depth;    // nesting depth inside ()[]{}
            std::vector<NodeIndex> stack;
            SyntaxForest& forest;
            Language dialect;

            ParserState(const std::vector<Token>& toks, SyntaxForest& f,
                        Language lang = Language::Boot)
                : tokens(toks), position(0), delimiter_depth(0),
                  forest(f), dialect(lang)
            { }

            bool spad() const { return dialect == Language::Spad; }

            const Token* current() const
            {
                if (position >= tokens.size())
                    return nullptr;
                return &tokens[position];
            }

            void advance()
            {
                if (position < tokens.size())
                    ++position;
            }

            TokenIndex current_index() const
            {
                return TokenIndex(position);
            }

            // -- Peek at the value of the current token, skipping
            //    whitespace and comments.  Formatting tokens
            //    (Indent/Justify/Unindent) are always visible.
            TokenValue peek_value();

            bool match(TokenValue v);

            TokenIndex require(TokenValue v, const std::string& context);

            void push(NodeIndex n) { stack.push_back(n); }

            NodeIndex pop();

            struct Snapshot {
                std::uint32_t position;
                std::size_t stack_depth;
                int delimiter_depth;
            };

            Snapshot save() const
            {
                return { position, stack.size(), delimiter_depth };
            }

            void restore(const Snapshot& s)
            {
                position = s.position;
                stack.resize(s.stack_depth);
                delimiter_depth = s.delimiter_depth;
            }

            NodeSpan make_span(const std::vector<NodeIndex>& nodes);
        };

        // -- RAII guard for delimiter_depth inside balanced delimiters.
        struct DelimiterGuard {
            ParserState& ps;
            explicit DelimiterGuard(ParserState& p) : ps(p) { ++ps.delimiter_depth; }
            ~DelimiterGuard() { --ps.delimiter_depth; }
            DelimiterGuard(const DelimiterGuard&) = delete;
            DelimiterGuard& operator=(const DelimiterGuard&) = delete;
        };

        TokenValue ParserState::peek_value()
        {
            while (auto t = current())
            {
                switch (t->category)
                {
                case TokenCategory::Whitespace:
                case TokenCategory::Comment:
                    advance();
                    continue;
                default:
                    return t->value;
                }
            }
            return TokenValue::EndOfStream;
        }

        bool ParserState::match(TokenValue v)
        {
            if (peek_value() == v)
            {
                advance();
                return true;
            }
            return false;
        }

        TokenIndex ParserState::require(TokenValue v, const std::string& ctx)
        {
            auto idx = current_index();
            if (not match(v))
            {
                auto t = current();
                LineNumber ln = 0;
                ColumnIndex col = 0;
                std::string found_str = "<end>";
                if (t != nullptr)
                {
                    ln = t->start.line;
                    col = t->start.column;
                    std::ostringstream oss;
                    oss << t->value;
                    found_str = oss.str();
                }
                throw ParseError("expected " + ctx
                    + " but found " + found_str
                    + " [at line " + std::to_string(ln)
                    + ", col " + std::to_string(col) + "]", ln, col);
            }
            return idx;
        }

        NodeIndex ParserState::pop()
        {
            if (stack.empty())
                return NodeIndex::none;
            auto n = stack.back();
            stack.pop_back();
            return n;
        }

        NodeSpan ParserState::make_span(const std::vector<NodeIndex>& nodes)
        {
            auto span = forest.allocate_span(
                static_cast<std::uint32_t>(nodes.size()));
            for (std::uint32_t i = 0; i < nodes.size(); ++i)
                forest.child(span, i) = nodes[i];
            return span;
        }

        // -- Forward declarations
        bool parse_comma(ParserState& ps);
        bool parse_where(ParserState& ps);
        bool parse_definition(ParserState& ps);
        bool parse_assign(ParserState& ps);
        bool parse_exit(ParserState& ps);
        bool parse_statement(ParserState& ps);
        bool parse_expression(ParserState& ps);
        bool parse_logical(ParserState& ps);
        bool parse_return_expr(ParserState& ps);
        bool parse_and_expr(ParserState& ps);
        bool parse_compare(ParserState& ps);
        bool parse_is_expr(ParserState& ps);
        bool parse_arith(ParserState& ps);
        bool parse_times(ParserState& ps);
        bool parse_euclid(ParserState& ps);
        bool parse_minus(ParserState& ps);
        bool parse_expt(ParserState& ps);
        bool parse_typed(ParserState& ps);
        void parse_typed_tail(ParserState& ps);
        bool parse_application(ParserState& ps);
        bool parse_primary(ParserState& ps);
        bool parse_name(ParserState& ps);
        bool parse_constant(ParserState& ps);
        bool parse_construct(ParserState& ps);
        static std::vector<NodeIndex> parse_collection_body(
            ParserState& ps, TokenValue close_tok);
        bool parse_conditional(ParserState& ps);
        bool parse_loop(ParserState& ps);
        bool parse_try_expr(ParserState& ps);
        bool parse_variable(ParserState& ps);
        bool parse_typing(ParserState& ps);
        bool parse_reduce(ParserState& ps);
        bool parse_module(ParserState& ps);
        bool parse_import(ParserState& ps);
        bool parse_namespace(ParserState& ps);
        bool parse_case_expr(ParserState& ps);
        bool parse_struct(ParserState& ps);
        bool parse_semicolon(ParserState& ps);
        bool parse_segment(ParserState& ps);
        bool parse_signature_tail(ParserState& ps);
        bool parse_sexp(ParserState& ps);
        bool parse_pattern_item(ParserState& ps);
        bool parse_pattern_list(ParserState& ps);
        bool parse_iterators(ParserState& ps);
        bool parse_definition_item(ParserState& ps);
        bool parse_capsule(ParserState& ps);
        bool parse_with_body(ParserState& ps);

        // -- Combinator helpers

        // -- Build an application node from fn (already popped)
        //    and arg (just parsed and still on stack).
        static void push_apply(ParserState& ps, NodeIndex fn)
        {
            auto arg = ps.pop();
            std::vector<NodeIndex> args = { arg };
            auto span = ps.make_span(args);
            ps.push(ps.forest.make_apply({ fn, span }));
        }

        // -- Parse a comma-separated list.  Wraps in Tuple if > 1.
        //    Used for both actual tuple expressions and parameter lists.
        //    Single elements are returned unwrapped to avoid trivial
        //    one-element tuples polluting the AST.
        // -- Parse a separated list of items.
        //
        //    Parses one or more items separated by `separator`.
        //    If only one item is parsed, it is returned as-is;
        //    otherwise the items are wrapped using `make`.
        //    When `justify` is SkipJustify, a Justify token after
        //    the separator is silently consumed (needed for
        //    semicolons inside brace blocks where line breaks
        //    produce Justify).
        enum class Justify : bool { Keep, Skip };

        template<typename MakeFn>
        bool parse_separated_list(ParserState& ps,
                                  bool (*parser)(ParserState&),
                                  TokenValue separator,
                                  MakeFn make,
                                  Justify justify = Justify::Keep)
        {
            if (not parser(ps))
                return false;
            std::vector<NodeIndex> elts;
            elts.push_back(ps.pop());
            while (ps.match(separator))
            {
                if (justify == Justify::Skip)
                    ps.match(TokenValue::Justify);
                if (not parser(ps))
                    break;
                elts.push_back(ps.pop());
            }
            if (elts.size() == 1)
                ps.push(elts[0]);
            else
            {
                auto span = ps.make_span(elts);
                ps.push((ps.forest.*make)({ span }));
            }
            return true;
        }

        bool parse_tuple_of(ParserState& ps,
                            bool (*parser)(ParserState&))
        {
            return parse_separated_list(ps, parser,
                TokenValue::Comma, &SyntaxForest::make_tuple);
        }

        bool parse_semi_list(ParserState& ps,
                             bool (*parser)(ParserState&))
        {
            return parse_separated_list(ps, parser,
                TokenValue::Semicolon, &SyntaxForest::make_sequence,
                Justify::Skip);
        }

        // -- Parse an indentation-delimited pile (off-side block).
        //
        //    A pile is a sequence of items at the same indentation
        //    level, introduced by an Indent token and terminated by
        //    the matching Unindent.  Individual items within the pile
        //    are separated by Justify tokens (same-level line breaks).
        //
        //    The `parser` argument determines what constitutes one
        //    item; typically `parse_semicolon` for top-level piles
        //    or `parse_definition_item` for where-clause piles.
        //
        //    Boot example:
        //      foo x ==
        //        a := x + 1      <- Indent before 'a', Justify before 'b'
        //        b := a * 2      <- Unindent after 'b' (back to foo's level)
        //        b
        //
        //    If the pile contains a single item, it is returned
        //    unwrapped (no Pile node).

        // -- Skip balanced INDENT/UNINDENT pairs from deeper-indented
        //    comment lines (++ documentation) and match `target` at
        //    the original pile level.
        //
        //    In Spad, ++ comments often appear at deeper indentation
        //    than the signatures they document, creating INDENT/UNINDENT
        //    formatting tokens that sit between pile items.  This
        //    helper skips those balanced pairs and looks for `target`
        //    (either Justify or Unindent) at depth 0.
        static bool skip_nested_comments_and_match(
            ParserState& ps, TokenValue target)
        {
            if (ps.match(target))
                return true;
            auto snap = ps.save();
            int depth = 0;
            for (;;)
            {
                auto v = ps.peek_value();
                if (v == TokenValue::Indent)
                {
                    ps.advance();
                    ++depth;
                    continue;
                }
                if (v == TokenValue::Unindent and depth > 0)
                {
                    ps.advance();
                    --depth;
                    continue;
                }
                if (v == TokenValue::Justify and depth > 0)
                {
                    ps.advance();
                    continue;
                }
                if (v == target and depth == 0)
                {
                    ps.advance();
                    return true;
                }
                break;
            }
            ps.restore(snap);
            return false;
        }

        bool parse_pile(ParserState& ps,
                               bool (*parser)(ParserState&))
        {
            if (not ps.match(TokenValue::Indent))
                return false;
            std::vector<NodeIndex> items;
            if (parser(ps))
                items.push_back(ps.pop());
            while (skip_nested_comments_and_match(ps, TokenValue::Justify))
            {
                if (parser(ps))
                    items.push_back(ps.pop());
            }
            skip_nested_comments_and_match(ps, TokenValue::Unindent);
            if (items.empty())
                return false;
            if (items.size() == 1)
                ps.push(items[0]);
            else
            {
                auto span = ps.make_span(items);
                ps.push(ps.forest.make_pile({ span }));
            }
            return true;
        }

        // -- Helper: check for continuation-line operator.
        //
        //    In Boot, an expression can continue on the next line if
        //    the next line is indented more deeply.  The tokenizer
        //    emits an Indent token for such continuations.  If the
        //    Indent is followed by a binary operator, this function
        //    consumes the Indent and returns true.  Otherwise the
        //    save point is restored and the function returns false.
        static bool skip_continuation_indent(ParserState& ps,
                            bool (*is_op)(TokenValue))
        {
            if (ps.peek_value() != TokenValue::Indent)
                return false;
            auto snap = ps.save();
            ps.advance();   // skip Indent
            if (is_op(ps.peek_value()))
                return true;   // leave Indent consumed
            ps.restore(snap);
            return false;
        }

        // -- Parse a left-associative binary operator chain.
        //
        //    Parses `sub { op sub }` where `op` is any token for
        //    which the `is_op` predicate returns true.  Each pair
        //    is folded into an Infix AST node, left-to-right.
        //
        //    Handles continuation lines: if an Indent token precedes
        //    the operator, it is consumed (and the matching Unindent
        //    is consumed after the RHS).
        //
        //    Handles continuation lines: if an Indent token precedes
        //    the operator, it is consumed (and the matching Unindent
        //    is consumed after the RHS).  When the Unindent cannot
        //    be consumed immediately (e.g. `while a or\n  b repeat
        //    body` where `repeat body` sits between the RHS and the
        //    Unindent), the orphaned Indent is recorded in `scope`
        //    so that the enclosing construct (e.g. parse_loop) can
        //    drain it after the intervening tokens have been parsed.
        //
        //    Used for arithmetic (+, -, *, /, **, quo, rem),
        //    logical connectives (and, or), and comparison.
        bool parse_left_assoc(ParserState& ps,
                              bool (*sub)(ParserState&),
                              bool (*is_op)(TokenValue))
        {
            if (not sub(ps))
                return false;
            for (;;)
            {
                bool continuation = false;
                if (is_op(ps.peek_value()))
                    ;  // operator on same line
                else if (skip_continuation_indent(ps, is_op))
                    continuation = true;
                else
                    break;

                auto op = ps.current_index();
                ps.advance();
                auto lhs = ps.pop();
                if (not sub(ps))
                {
                    ps.push(lhs);
                    break;
                }
                auto rhs = ps.pop();
                ps.push(ps.forest.make_infix({ lhs, op, rhs }));
                if (continuation)
                    ps.match(TokenValue::Unindent);
            }
            return true;
        }

        // -- Parse a right-associative binary operator chain.
        //
        //    a ** b ** c  =>  Infix(a, **, Infix(b, **, c))
        //
        //    After parsing the first sub-expression and seeing an operator
        //    that satisfies is_op, the RHS is parsed by recursing into
        //    parse_right_assoc itself (not into sub), giving right grouping.
        bool parse_right_assoc(ParserState& ps,
                               bool (*sub)(ParserState&),
                               bool (*is_op)(TokenValue))
        {
            if (not sub(ps))
                return false;
            bool continuation = false;
            if (is_op(ps.peek_value()))
                ;
            else if (skip_continuation_indent(ps, is_op))
                continuation = true;
            else
                return true;

            auto op = ps.current_index();
            ps.advance();
            auto lhs = ps.pop();
            if (not parse_right_assoc(ps, sub, is_op))
            {
                ps.push(lhs);
                if (continuation)
                    ps.match(TokenValue::Unindent);
                return true;
            }
            auto rhs = ps.pop();
            ps.push(ps.forest.make_infix({ lhs, op, rhs }));
            if (continuation)
                ps.match(TokenValue::Unindent);
            return true;
        }

        // -- Parse a quoted S-expression: ' sexp.
        //
        //    Shared by parse_primary, parse_sexp, and parse_pattern_item.
        bool parse_quote(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Apostrophe)
                return false;
            auto apos = ps.current_index();
            ps.advance();
            if (not parse_sexp(ps))
                return false;
            auto expr = ps.pop();
            ps.push(ps.forest.make_quote({ apos, expr }));
            return true;
        }

        // -- Parse a keyword-prefixed unary expression.
        //
        //    Pattern: keyword operand > make_X({ kw, operand })
        //    Used for return, leave, throw, while, until, etc.
        //
        //    `Make` is a pointer-to-member for the forest node
        //    constructor, allowing different AST node types to be
        //    built with the same code shape.
        template<typename Syntax>
        bool parse_prefix_kw(ParserState& ps,
                             bool (*sub)(ParserState&),
                             NodeIndex (SyntaxForest::*make)(const Syntax&))
        {
            auto kw = ps.current_index();
            ps.advance();
            if (not sub(ps))
                return false;
            auto val = ps.pop();
            ps.push((ps.forest.*make)({ kw, val }));
            return true;
        }

        // -- Parse a parenthesized expression: '(' content ')'.
        //
        //    The pile phase never emits formatting tokens inside
        //    balanced delimiters, so no special suppression is needed.
        //
        //    If the content parser fails (e.g. empty parens), an
        //    empty tuple node is synthesized rather than failing.
        //    This matches Boot's treatment of `()` as unit/void.
        bool parse_paren(ParserState& ps,
                                bool (*parser)(ParserState&))
        {
            if (not ps.match(TokenValue::OpenParen))
                return false;
            DelimiterGuard delim(ps);
            bool ok = parser(ps);
            if (not ok)
            {
                ps.push(ps.forest.make_tuple({ ps.make_span({}) }));
                ok = true;
            }
            ps.require(TokenValue::CloseParen, "')'");
            return ok;
        }

        // -- Parse a bracketed expression: '[' content ']'.
        //
        //    The pile phase never emits formatting tokens inside
        //    balanced delimiters.  Boot uses brackets for:
        //      - List construction: [1, 2, 3]
        //      - List comprehension: [x*x for x in 1..10]
        //      - Pattern matching: x is [first, :rest]
        //
        //    Note: this is the generic bracket parser used by pattern
        //    matching.  List construction with comprehensions uses
        //    parse_construct() instead, which adds iterator support.
        bool parse_bracket(ParserState& ps,
                                  bool (*parser)(ParserState&))
        {
            auto open_idx = ps.current_index();
            if (not ps.match(TokenValue::OpenBracket))
                return false;
            DelimiterGuard delim(ps);
            std::vector<NodeIndex> elts;
            if (parser(ps))
                elts.push_back(ps.pop());
            auto close_idx = ps.current_index();
            ps.require(TokenValue::CloseBracket, "']'");
            auto span = ps.make_span(elts);
            ps.push(ps.forest.make_bracket({ open_idx, span, close_idx }));
            return true;
        }

        // -- S-expression parser (for quoted expressions: '(...))
        //
        // Boot inherits Lisp-style quoting: the apostrophe ' followed
        // by an S-expression produces a literal data structure.  This
        // is used extensively in Boot for:
        //   - Symbol literals: 'FOO, 'SPADCALL
        //   - Association lists: '((a . 1) (b . 2))
        //   - Code templates: '(PROGN ,x ,y)
        //   - Dispatch tables: '((+ WIDTH sumWidth) ...)
        //
        // The S-expression grammar is separate from the main
        // expression grammar.  Inside an S-expression, almost any
        // token (including keywords and operators) is treated as an
        // atom - only structural delimiters like parentheses and
        // commas retain their role.
        //
        // The parser handles:
        //   - Atoms: identifiers, numbers, strings, keywords, operators
        //   - Nested quotes: ''x, '('a 'b)
        //   - Parenthesized lists: '(a b c)
        //   - Dotted pairs: '(a . b)
        //   - Negative numbers: '(-1)

        // -- Parse any token as an S-expression atom: identifier,
        //    integer, float, string, or keyword used as symbol.
        //    Returns false only for structural delimiters that would
        //    terminate an enclosing S-expression list.
        bool parse_sexp_atom(ParserState& ps)
        {
            auto t = ps.current();
            if (t == nullptr)
                return false;
            switch (t->category)
            {
            case TokenCategory::Identifier:
            case TokenCategory::Integer:
            case TokenCategory::FloatingPoint:
            case TokenCategory::String:
            {
                auto idx = ps.current_index();
                ps.advance();
                ps.push(ps.forest.make_name({ idx }));
                return true;
            }
            case TokenCategory::Keyword:
            case TokenCategory::Operator:
            case TokenCategory::Punctuator:
            {
                // -- Accept keywords and operators as S-expression atoms,
                // but exclude structural delimiters that terminate lists.
                if (sexp_structural_delimiter(t->value))
                    return false;
                auto idx = ps.current_index();
                ps.advance();
                ps.push(ps.forest.make_name({ idx }));
                return true;
            }
            default:
                return false;
            }
        }

        // -- Parse an S-expression: atom, or (sexp*), or 'sexp.
        bool parse_sexp(ParserState& ps)
        {
            auto v = ps.peek_value();
            // -- Negative integer literal: - INTEGER → single atom.
            // The lexer produces two tokens; S-expression semantics
            // require them to be one (e.g. in dotted pairs like
            // '(0 . -1)).  If the minus is not followed by an
            // integer, fall through to parse_sexp_atom which
            // accepts '-' as a plain operator atom.
            if (v == TokenValue::Minus)
            {
                auto snap = ps.save();
                auto op = ps.current_index();
                ps.advance();
                auto t = ps.current();
                if (t != nullptr and t->category == TokenCategory::Integer)
                {
                    auto num = ps.current_index();
                    ps.advance();
                    auto operand = ps.forest.make_literal({ num });
                    ps.push(ps.forest.make_prefix({ op, operand }));
                    return true;
                }
                ps.restore(snap);
            }
            // -- Nested quote.
            if (v == TokenValue::Apostrophe)
                return parse_quote(ps);
            // -- Parenthesized S-expression list.
            if (v == TokenValue::OpenParen)
            {
                ps.advance();
                DelimiterGuard delim(ps);
                std::vector<NodeIndex> items;
                while (parse_sexp(ps))
                    items.push_back(ps.pop());
                // -- Check for dotted pair: (a . b).
                if (ps.match(TokenValue::Dot))
                {
                    if (parse_sexp(ps))
                        items.push_back(ps.pop());
                }
                ps.require(TokenValue::CloseParen, "')' in S-expression");
                auto span = ps.make_span(items);
                ps.push(ps.forest.make_tuple({ span }));
                return true;
            }
            return parse_sexp_atom(ps);
        }

        // -- Atom parsers

        // -- Parse a simple identifier name.
        //
        //    In addition to TokenCategory::Identifier, this also
        //    accepts the keyword `rule` as a name.  The `rule`
        //    keyword is defined in the token vocabulary as a Boot
        //    keyword, but no Boot grammar rule uses it as a keyword;
        //    it appears only as a plain identifier in Boot source.
        //    Accepting it here avoids tokenizer-level changes while
        //    correctly parsing all Boot files.
        bool parse_name(ParserState& ps)
        {
            if (ps.peek_value() == TokenValue::EndOfStream)
                return false;
            auto t = ps.current();
            if (t == nullptr)
                return false;
            // -- Accept identifiers and keywords that are not used in Boot
            // grammar but may appear as names in user code (e.g. 'rule').
            if (t->category == TokenCategory::Identifier
                or t->value == TokenValue::Rule
                or t->value == TokenValue::Break
                or t->value == TokenValue::Dollar)
            {
                auto idx = ps.current_index();
                ps.advance();
                ps.push(ps.forest.make_name({ idx }));
                return true;
            }
            // -- In Spad, many keywords and operators can appear as
            // operation names (e.g. `cross: (%,%) -> %`, `case`,
            // `not`, `quo`, `mod`, etc.).  Accept them as names
            // unless they are structural keywords that the parser
            // relies on for delimiting constructs.
            if (ps.spad()
                and (t->category == TokenCategory::Keyword
                     or t->category == TokenCategory::Operator))
            {
                if (spad_structural_keyword(t->value))
                    return false;
                auto idx = ps.current_index();
                ps.advance();
                ps.push(ps.forest.make_name({ idx }));
                return true;
            }
            return false;
        }

        // -- Parse a literal constant (integer, float, or string).
        bool parse_constant(ParserState& ps)
        {
            auto t = ps.current();
            if (t == nullptr)
                return false;
            switch (t->category)
            {
            case TokenCategory::Integer:
            case TokenCategory::FloatingPoint:
            case TokenCategory::String:
            {
                auto idx = ps.current_index();
                ps.advance();
                ps.push(ps.forest.make_literal({ idx }));
                return true;
            }
            default:
                return false;
            }
        }

        // -- Parse a dot (don't-care placeholder).
        bool parse_dot(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Dot)
                return false;
            auto idx = ps.current_index();
            ps.advance();
            ps.push(ps.forest.make_name({ idx }));
            return true;
        }

        // -- Primary expressions

        // -- Parse a primary expression (highest precedence).
        //
        //    A primary is the tightest-binding syntactic unit.  It
        //    includes:
        //      'sexp         - quoted S-expression (data literal)
        //      (expr)        - parenthesized grouping
        //      [elts]        - bracket construction / list literal
        //      .             - don't-care / wildcard
        //      #expr, -expr, ~expr - prefix operators
        //      leave expr    - early exit (tight binding variant used
        //                      inside comprehensions, e.g.
        //                      or/[... and leave x for ...])
        //      if/case/structure/function - keyword constructs
        //      Indent...     - an indented pile as a primary
        //      namespace nm  - namespace reference
        //      name, literal - identifiers and constants
        //
        //    `leave` appears at two precedence levels in Boot:
        //      1. In parse_return_expr (loose binding): `leave expr`
        //         as a statement, where expr can be a full logical.
        //      2. Here in parse_primary (tight binding): `leave x`
        //         as a primary, where x is a single primary.  This
        //         is used inside `and` chains in comprehension guards.
        bool parse_primary(ParserState& ps)
        {
            
            auto v = ps.peek_value();

            // -- Quoted S-expression: 'sexp.
            if (v == TokenValue::Apostrophe)
                return parse_quote(ps);

            // -- Parenthesized expression.
            if (v == TokenValue::OpenParen)
                return parse_paren(ps, parse_semicolon);

            // -- Bracket construction: [elements].
            if (v == TokenValue::OpenBracket)
                return parse_construct(ps);

            // -- Spad delimited constructions: {}, [||], (||).
            // All share the same structure: open, parse content,
            // close, wrap in a Bracket node.
            if (ps.spad())
            {
                auto delim_close = TokenValue::EndOfStream;
                const char* close_name = nullptr;
                bool use_collection_body = false;
                if (v == TokenValue::OpenBrace)
                {
                    delim_close = TokenValue::CloseBrace;
                    close_name = "'}'";
                    use_collection_body = true;
                }
                else if (v == TokenValue::OpenMetaBracket)
                {
                    delim_close = TokenValue::CloseMetaBracket;
                    close_name = "'|]'";
                }
                else if (v == TokenValue::OpenMetaParen)
                {
                    delim_close = TokenValue::CloseMetaParen;
                    close_name = "'|)'";
                }
                if (close_name != nullptr)
                {
                    auto open = ps.current_index();
                    ps.advance();
                    DelimiterGuard delim(ps);
                    std::vector<NodeIndex> elts;
                    if (use_collection_body)
                        elts = parse_collection_body(ps, delim_close);
                    else if (parse_comma(ps))
                        elts.push_back(ps.pop());
                    auto close = ps.current_index();
                    ps.require(delim_close, close_name);
                    auto span = ps.make_span(elts);
                    ps.push(ps.forest.make_bracket({ open, span, close }));
                    return true;
                }
            }

            // -- Dot (don't-care or field accessor).
            if (v == TokenValue::Dot)
                return parse_dot(ps);

            // -- Prefix operator.
            if (prefix_operator(v))
                return parse_prefix_kw(ps, parse_primary,
                                       &SyntaxForest::make_prefix);

            // -- Leave: prefix unary in expression context (e.g. inside
            // comprehension bodies: or/[... and leave x for ...]).
            if (v == TokenValue::Leave)
                return parse_prefix_kw(ps, parse_primary,
                                       &SyntaxForest::make_leave);

            // -- Keyword-initiated expressions.
            switch (v)
            {
            case TokenValue::If:
                return parse_conditional(ps);
            case TokenValue::Case:
                return parse_case_expr(ps);
            case TokenValue::Structure:
                return parse_struct(ps);
            case TokenValue::Function:
            {
                ps.advance();
                if (not parse_primary(ps))
                    return false;
                return true;
            }
            default:
                break;
            }

            // -- A pile as primary.
            if (v == TokenValue::Indent)
                return parse_pile(ps, parse_semicolon);

            // -- Namespace expression: namespace name.
            if (v == TokenValue::Namespace)
                return parse_namespace(ps);

            // -- Spad: prefix 'with' - category definition.
            if (v == TokenValue::With)
            {
                auto kw = ps.current_index();
                ps.advance();
                if (not parse_with_body(ps))
                    throw ParseError("expected body after 'with'");
                auto body = ps.pop();
                ps.push(ps.forest.make_with({ kw, body }));
                return true;
            }

            // -- Spad: prefix 'add' - domain implementation.
            if (v == TokenValue::Add)
            {
                auto kw = ps.current_index();
                ps.advance();
                if (not parse_capsule(ps))
                    throw ParseError("expected body after 'add'");
                auto body = ps.pop();
                ps.push(ps.forest.make_add({ kw, body }));
                return true;
            }

            // -- Spad: 'free' declaration.
            if (v == TokenValue::Free)
                return parse_prefix_kw(ps, parse_expression,
                                       &SyntaxForest::make_free_decl);

            // -- Spad: 'export' declaration.
            if (v == TokenValue::Export)
                return parse_prefix_kw(ps, parse_expression,
                                       &SyntaxForest::make_export_decl);

            // -- Name or literal.
            if (parse_name(ps))
                return true;
            if (parse_constant(ps))
                return true;

            return false;
        }

        // -- Application

        // -- Parse function application: primary { selector | arg }.
        //
        //    Boot and Spad have three forms of function application:
        //
        //    1. Dot selector: `expr.name` or `expr.` (suffix dot)
        //       Field access or qualified name lookup.
        //
        //    2. Parenthesized arguments: `f(x, y)`
        //       Traditional function call notation.  The content
        //       inside parens is parsed by parse_semicolon, which
        //       means semicolons are valid separators (used in
        //       multi-value expressions like `coreQuit(x > 0 => 1; 0)`).
        //
        //    3. Juxtaposition: `f x`
        //       Argument by adjacency.  The parser must be careful
        //       NOT to grab tokens that belong to an outer context
        //       (infix operators, keywords, formatting tokens, etc.).
        //       The large exclusion list below enumerates all tokens
        //       that terminate juxtaposition.
        //
        //    Dot and parenthesized application are left-associative;
        //    juxtaposition is right-associative:
        //      `f.g(x).h` = (((f.g)(x)).h)
        //      `f x y z`  = f(x(y(z)))
        //
        //    For (2) and (3), the parser uses tentative parsing with
        //    save/restore: if the attempted parse fails, the state
        //    is rewound and the loop breaks.
        bool parse_application(ParserState& ps)
        {
            if (not parse_primary(ps))
                return false;

            for (;;)
            {
                auto v = ps.peek_value();

                // -- Dot selector: expr.name or expr. (suffix dot).
                if (v == TokenValue::Dot)
                {
                    auto dot = ps.current_index();
                    ps.advance();
                    auto base = ps.pop();
                    if (parse_primary(ps))
                    {
                        auto selector = ps.pop();
                        ps.push(ps.forest.make_infix({ base, dot, selector }));
                    }
                    else
                    {
                        ps.push(ps.forest.make_suffix_dot({ base, dot }));
                    }
                    continue;
                }

                // -- Parenthesized argument list: f(args).
                if (v == TokenValue::OpenParen)
                {
                    auto snap = ps.save();
                    auto fn = ps.pop();
                    if (parse_paren(ps, parse_semicolon))
                    {
                        push_apply(ps, fn);
                        continue;
                    }
                    ps.restore(snap);
                    ps.push(fn);
                    break;
                }

                // -- Juxtaposition application: f x y => f(x(y)).
                // Right-associative: collect primaries iteratively
                // then fold right-to-left to avoid deep recursion.
                if (not terminates_juxtaposition(v))
                {
                    auto snap = ps.save();
                    auto fn = ps.pop();

                    // -- Try to parse one primary as the start of the
                    // juxtaposed argument chain.
                    if (not parse_primary(ps))
                    {
                        ps.restore(snap);
                        ps.push(fn);
                        break;
                    }

                    // -- Collect additional juxtaposed primaries
                    // iteratively (each may also have dot/paren
                    // suffixes, so we parse full "applicative"
                    // primaries via the dot/paren loop above by
                    // re-entering the for loop for those cases).
                    // Here we only handle the "more juxtaposed
                    // primaries" part without recursion.
                    std::vector<NodeIndex> chain;
                    chain.push_back(fn);
                    chain.push_back(ps.pop());

                    for (;;)
                    {
                        auto v2 = ps.peek_value();
                        // -- Handle dot and paren for the last
                        // collected primary before checking for
                        // more juxtaposed primaries.
                        if (v2 == TokenValue::Dot)
                        {
                            auto dot = ps.current_index();
                            ps.advance();
                            auto base = chain.back();
                            if (parse_primary(ps))
                            {
                                auto sel = ps.pop();
                                chain.back() = ps.forest.make_infix(
                                    { base, dot, sel });
                            }
                            else
                            {
                                chain.back() = ps.forest.make_suffix_dot(
                                    { base, dot });
                            }
                            continue;
                        }
                        if (v2 == TokenValue::OpenParen)
                        {
                            auto snap2 = ps.save();
                            auto fn2 = chain.back();
                            if (parse_paren(ps, parse_semicolon))
                            {
                                push_apply(ps, fn2);
                                chain.back() = ps.pop();
                                continue;
                            }
                            ps.restore(snap2);
                            break;
                        }
                        if (terminates_juxtaposition(v2))
                            break;
                        auto snap2 = ps.save();
                        if (not parse_primary(ps))
                        {
                            ps.restore(snap2);
                            break;
                        }
                        chain.push_back(ps.pop());
                    }

                    // -- Fold right-to-left: f x y z => f(x(y(z)))
                    // but z is just z, y(z), x(y(z)), f(x(y(z))).
                    auto result = chain.back();
                    for (auto i = chain.size() - 1; i-- > 0; )
                    {
                        ps.push(result);
                        push_apply(ps, chain[i]);
                        result = ps.pop();
                    }
                    ps.push(result);
                    return true;
                }
                break;
            }
            return true;
        }

        // -- Type expressions

        // -- Parse a type annotation (right-hand side of : or @).
        //
        //    This is the grammar for type expressions that appear
        //    after type annotation operators (:, ::, @).  It is
        //    separate from the main expression grammar because type
        //    syntax allows `->` for function types:
        //      (int, int) -> int
        //      forall T . T -> T
        //
        //    The `forall` quantifier binds variables before a dot,
        //    then the body is a recursive call to parse_typing.
        //    The `->` for function types is only parsed here, not
        //    in the main expression chain, because in expression
        //    context `->` has a different role (see parse_typed).
        bool parse_typing(ParserState& ps)
        {
            if (ps.peek_value() == TokenValue::Forall)
            {
                auto kw = ps.current_index();
                ps.advance();
                std::vector<NodeIndex> vars;
                while (parse_variable(ps))
                    vars.push_back(ps.pop());
                auto dot = ps.require(TokenValue::Dot, "'.' in forall");
                if (not parse_typing(ps))
                    throw ParseError("expected type after 'forall ... .'");
                auto body = ps.pop();
                auto span = ps.make_span(vars);
                ps.push(ps.forest.make_forall({ kw, span, dot, body }));
                return true;
            }
            if (not parse_application(ps))
                return false;
            if (ps.peek_value() == TokenValue::RightArrow)
            {
                auto arrow = ps.current_index();
                ps.advance();
                auto source = ps.pop();
                std::vector<NodeIndex> src_list = { source };
                auto src_span = ps.make_span(src_list);
                if (not parse_application(ps))
                    throw ParseError("expected return type after '->'");
                auto target = ps.pop();
                ps.push(ps.forest.make_mapping({ src_span, arrow, target }));
            }
            return true;
        }

        // -- Parse a typed expression: application [@ T | :: T | : T | -> T].
        //
        //    Boot supports four postfix type operators on expressions:
        //
        //      expr @ Type     - domain restriction (restrict)
        //      expr :: Type    - coercion to Type (coerce)
        //      expr : Type     - type signature (annotate)
        //      expr -> Type    - function type in expression context
        //
        //    These all bind tighter than arithmetic but looser than
        //    application.  They left-associate, so `a : T @ S` means
        //    `(a : T) @ S`.
        //
        //    The `->` case is right-associative (parsed via recursive
        //    call to parse_typed) because function types nest on the
        //    right: `A -> B -> C` means `A -> (B -> C)`.
        //
        //    Note the distinction between `->` in parse_typing
        //    (appears after : or @, in a pure type context) and
        //    `->` here (appears in expression context, e.g. in
        //    module export signatures like:
        //      every?: (%Thing -> %Thing, %List %Thing) -> %Thing
        //    The parenthesized part is parsed by parse_typed because
        //    it appears as a regular expression inside parens).
        // -- Helper: parse a postfix type operator.
        //    Pops the LHS from the stack, parses the RHS with `rhs_parser`,
        //    and pushes a ternary AST node built by `make`.
        template<typename MakeFn>
        static void parse_postfix_type_op(
            ParserState& ps,
            bool (*rhs_parser)(ParserState&),
            MakeFn make,
            const char* error_msg)
        {
            auto op = ps.current_index();
            ps.advance();
            auto lhs = ps.pop();
            if (not rhs_parser(ps))
                throw ParseError(error_msg);
            auto rhs = ps.pop();
            ps.push((ps.forest.*make)({ lhs, op, rhs }));
        }

        // -- Apply postfix type operators to an already-parsed operand.
        //
        //    Assumes the operand is already on the stack.  Handles
        //    the same operators as parse_typed: @, ::, :, ->, pretend.
        //    Factored out so that parse_times can call it after a
        //    reduce expression, matching the old Pratt parser's
        //    behaviour where a Nud-produced reduction feeds into the
        //    Led loop for these operators.
        void parse_typed_tail(ParserState& ps)
        {
            for (;;)
            {
                auto v = ps.peek_value();
                if (v == TokenValue::At)
                {
                    parse_postfix_type_op(ps, parse_typing,
                        &SyntaxForest::make_restrict,
                        "expected type after '@'");
                    continue;
                }
                if (v == TokenValue::ColonColon)
                {
                    parse_postfix_type_op(ps, parse_typing,
                        &SyntaxForest::make_coerce,
                        "expected type after '::'");
                    continue;
                }
                if (v == TokenValue::Colon)
                {
                    parse_postfix_type_op(ps, parse_typing,
                        &SyntaxForest::make_signature,
                        "expected type after ':'");
                    continue;
                }
                if (v == TokenValue::RightArrow)
                {
                    parse_postfix_type_op(ps, parse_typed,
                        &SyntaxForest::make_infix,
                        "expected type after '->'");
                    continue;
                }
                if (v == TokenValue::Pretend)
                {
                    parse_postfix_type_op(ps, parse_application,
                        &SyntaxForest::make_pretend,
                        "expected type after 'pretend'");
                    continue;
                }
                break;
            }
        }

        bool parse_typed(ParserState& ps)
        {
            if (not parse_application(ps))
                return false;
            parse_typed_tail(ps);
            return true;
        }

        // -- Try to parse ': type' signature tail.
        //
        //    Called after a name has been parsed, when we see a
        //    colon that might introduce a type annotation.  Pops
        //    the name from the stack, parses the type, and pushes
        //    a Signature node (name : type).
        //
        //    Used in both parameter declarations (parse_variable)
        //    and catch clauses (parse_try_expr).
        bool parse_signature_tail(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Colon)
                return false;
            parse_postfix_type_op(ps, parse_typing,
                &SyntaxForest::make_signature,
                "expected type after ':'");
            return true;
        }

        // -- Arithmetic
        //
        // The arithmetic operators follow standard mathematical
        // precedence (tightest to loosest):
        //   **         - exponentiation (right-associative)
        //   *, /       - multiplication, division
        //   quo, rem   - Euclidean quotient and remainder
        //   unary -    - negation
        //   +, -       - addition, subtraction
        //
        // Additionally, the reduce operator (e.g. +/[...]) is parsed
        // at the same level as *.  A reduce expression like +/[x for ...]
        // means "fold + over the list comprehension".

        // -- Parse exponentiation: typed ** typed ** ... (right-associative).
        bool parse_expt(ParserState& ps)
        {
            return parse_right_assoc(ps, parse_typed, exponentiation_operator);
        }

        // -- Parse reduce: op / collection.
        //
        //    A reduce expression folds a binary operator over a
        //    collection.  The operator comes first, then '/', then
        //    either a bracket construction or an application:
        //      +/[x for x in 1..10]    - sum of 1 to 10
        //      or/[p x for x in l]     - any element satisfies p?
        //      #/args                  - length of args
        //
        //    The operator can be any infix or prefix operator
        //    (checked by reduce_operator).  Parsing is tentative: if
        //    the '/' does not follow the operator, we backtrack.
        bool parse_reduce(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (not reduce_operator(v))
                return false;
            auto snap = ps.save();
            auto op_idx = ps.current_index();
            ps.advance();
            if (ps.peek_value() != TokenValue::Slash)
            {
                ps.restore(snap);
                return false;
            }
            auto slash = ps.current_index();
            ps.advance();
            auto op_node = ps.forest.make_name({ op_idx });
            if (not parse_construct(ps) and not parse_application(ps))
                throw ParseError("expected collection after reduce operator");
            auto body = ps.pop();
            ps.push(ps.forest.make_reduce({ op_node, slash, body }));
            return true;
        }

        // -- Parse multiplication/division: expt { (* | /) expt }.
        //    Also tries reduce (op/collection) first.
        //
        //    In the old Pratt parser, a reduction like `+/[...]` is
        //    a Nud form: after it is parsed, the Led loop handles
        //    postfix operators (`@`, `::`) and binary operators (`*`)
        //    at their normal binding powers.  Here we emulate this:
        //    after a successful reduce, run the typed-postfix loop
        //    (for `@`, `::`, `:`, `pretend`) then continue with the
        //    `*`/`/` chain so that `*/[...] * expr` and
        //    `+/[...]@Type` both work.
        bool parse_times(ParserState& ps)
        {
            if (not parse_reduce(ps))
                return parse_left_assoc(ps, parse_expt, multiplicative_operator);
            // -- Reduce succeeded — apply the same postfix type
            // operators that parse_typed would apply.
            parse_typed_tail(ps);
            // -- Now continue with the * / chain.
            for (;;)
            {
                if (not multiplicative_operator(ps.peek_value()))
                    break;
                auto op = ps.current_index();
                ps.advance();
                auto lhs = ps.pop();
                if (not parse_expt(ps))
                {
                    ps.push(lhs);
                    break;
                }
                auto rhs = ps.pop();
                ps.push(ps.forest.make_infix({ lhs, op, rhs }));
            }
            return true;
        }

        // -- Parse Euclidean division: times { (quo | rem) times }.
        bool parse_euclid(ParserState& ps)
        {
            return parse_left_assoc(ps, parse_times, euclidean_operator);
        }

        // -- Parse unary minus: ['-'] euclid.
        bool parse_minus(ParserState& ps)
        {
            if (ps.peek_value() == TokenValue::Minus)
                return parse_prefix_kw(ps, parse_euclid,
                                       &SyntaxForest::make_prefix);
            return parse_euclid(ps);
        }

        // -- Parse addition/subtraction: minus { (+ | -) minus }.
        bool parse_arith(ParserState& ps)
        {
            return parse_left_assoc(ps, parse_minus, additive_operator);
        }

        // -- Segments
        //
        // A segment is a range expression using `..`:
        //
        //   1..10       - bounded range from 1 to 10
        //   n..         - unbounded range starting at n
        //
        // Segments appear in for-loop iterators (for i in 1..n)
        // and in list comprehensions ([x for x in 0..9]).
        // The bounded form has both endpoints; the unbounded form
        // has only a start.

        // -- Parse: arith [.. [arith]].
        bool parse_segment(ParserState& ps)
        {
            if (not parse_arith(ps))
                return false;
            if (ps.peek_value() == TokenValue::DotDot)
            {
                auto dots = ps.current_index();
                ps.advance();
                auto lo = ps.pop();
                if (parse_arith(ps))
                {
                    auto hi = ps.pop();
                    ps.push(ps.forest.make_bounded_segment({ lo, dots, hi }));
                }
                else
                {
                    ps.push(ps.forest.make_unbounded_segment({ lo, dots }));
                }
            }
            return true;
        }

        // -- Pattern matching
        //
        // Boot has ML-style pattern matching via `is` and `isnt`:
        //   x is [first, :rest]     - destructure a list
        //   x isnt [.,:.] => ...    - guard: x is not a pair
        //   f x is ['SPADCALL, =arg, n, ...] - match a specific shape
        //
        // Patterns differ from expressions:
        //   =expr       - equality test: match if the value equals expr
        //   :rest       - rest binding: bind remaining list elements
        //   name := pat - bind-and-match: bind name while matching pat
        //   name::name  - qualified symbol: e.g. KEYWORD::RELATIVE
        //   [p1, p2, :r] - list destructuring
        //   .           - don't-care (wildcard)
        //   'SYMBOL     - quoted symbol literal
        //   "string"    - string literal match
        //
        // The `is`/`isnt` keywords use nesting suppression to handle
        // patterns that start on the next line:
        //   argl.(sharpPosition-1) is
        //     ['SPADCALL, =sharpArg, n, ...]
        // Here the Indent before '[' would normally start a pile, but
        // inside the nesting-suppressed region it is skipped.

        // -- Parse a pattern item: =expr | :rest | . | name | constant
        //    | [pattern-list].
        bool parse_pattern_item(ParserState& ps)
        {
            auto v = ps.peek_value();
            // -- Equality pattern: =expr.
            if (v == TokenValue::Eq)
            {
                auto eq = ps.current_index();
                ps.advance();
                if (not parse_application(ps) and not parse_constant(ps))
                    return false;
                auto expr = ps.pop();
                ps.push(ps.forest.make_equal_pattern({ eq, expr }));
                return true;
            }
            // -- Colon pattern: :rest.
            if (v == TokenValue::Colon)
            {
                auto colon = ps.current_index();
                ps.advance();
                if (parse_pattern_item(ps))
                {
                    auto rest = ps.pop();
                    // -- Wrap as ColonAppend with empty head.
                    ps.push(ps.forest.make_colon_append(
                        { ps.make_span({}), colon, rest }));
                }
                else
                {
                    // -- Bare colon - treat as don't-care rest.
                    ps.push(ps.forest.make_name({ colon }));
                }
                return true;
            }
            // -- Bracket pattern: [items].
            if (v == TokenValue::OpenBracket)
                return parse_bracket(ps, parse_pattern_list);
            // -- Dot (don't-care).
            if (v == TokenValue::Dot)
                return parse_dot(ps);
            // -- Quoted symbol.
            if (v == TokenValue::Apostrophe)
                return parse_quote(ps);
            // -- String constant in pattern context.
            if (parse_constant(ps))
                return true;
            // -- Name, possibly followed by :: qualifier or := binding.
            if (parse_name(ps))
            {
                // -- Qualified name: PKG::SYM (e.g. KEYWORD::RELATIVE).
                if (ps.peek_value() == TokenValue::ColonColon)
                {
                    auto cc = ps.current_index();
                    ps.advance();
                    auto pkg = ps.pop();
                    if (parse_name(ps))
                    {
                        auto sym = ps.pop();
                        ps.push(ps.forest.make_coerce({ pkg, cc, sym }));
                    }
                    else
                    {
                        ps.push(pkg);
                    }
                }
                // -- Pattern binding: name := pattern (bind and match).
                if (ps.peek_value() == TokenValue::ColonEq)
                {
                    auto ceq = ps.current_index();
                    ps.advance();
                    auto name = ps.pop();
                    if (not parse_pattern_item(ps))
                        return true;
                    auto pat = ps.pop();
                    ps.push(ps.forest.make_assignment({ name, ceq, pat }));
                }
                return true;
            }
            return false;
        }

        // -- Parse a comma-separated pattern list with optional
        //    colon-append at the end: item, item, ..., :rest.
        //
        //    Produces a Tuple node when there are multiple items,
        //    or a single pattern node otherwise.  Used inside
        //    bracketed patterns: [first, second, :rest].
        bool parse_pattern_list(ParserState& ps)
        {
            return parse_tuple_of(ps, parse_pattern_item);
        }

        // -- Parse is/isnt: segment [ (is|isnt) pattern | has expr ].
        //
        //    The `is` and `isnt` operators test whether a value
        //    matches a structural pattern.  They bind tighter than
        //    comparison operators but looser than segment expressions.
        //
        //    `has` tests for trait/category membership:
        //      T has Ring   - does T have the Ring trait?
        bool parse_is_expr(ParserState& ps)
        {
            if (not parse_segment(ps))
                return false;
            auto v = ps.peek_value();
            if (v == TokenValue::Is or v == TokenValue::Isnt)
            {
                auto kw = ps.current_index();
                ps.advance();
                auto expr = ps.pop();
                // -- In Boot, `is` is destructuring: `x is [first, :rest]`.
                // In Spad, `is` also tests multi-word type expressions
                // via juxtaposition (e.g. `S is Polynomial Integer`
                // means `S is Polynomial(Integer)`).
                bool ok = ps.spad()
                    ? parse_application(ps)
                    : parse_pattern_item(ps);
                if (not ok)
                {
                    ps.push(expr);
                    return true;
                }
                auto pat = ps.pop();
                if (v == TokenValue::Is)
                    ps.push(ps.forest.make_is({ expr, kw, pat }));
                else
                    ps.push(ps.forest.make_isnt({ expr, kw, pat }));
            }
            else if (v == TokenValue::Has)
            {
                auto kw = ps.current_index();
                ps.advance();
                auto expr = ps.pop();
                if (not parse_application(ps))
                    return true;
                // -- Handle signature attribute: `T has name: Type`.
                parse_signature_tail(ps);
                auto trait = ps.pop();
                ps.push(ps.forest.make_infix({ expr, kw, trait }));
            }
            return true;
        }

        // -- Comparison and logic
        //
        // Comparison operators (=, ~=, <, <=, >, >=) are non-
        // associative: `a < b < c` is a syntax error; use
        // `a < b and b < c`.  The keyword `in` is also handled
        // here as a comparison (membership test: `x in collection`).
        //
        // Conjunction (`and`) and disjunction (`or`) are the standard
        // logical connectives, with `and` binding tighter.

        // -- Parse comparison: is_expr [ comp_op is_expr ].
        //    Non-associative: at most one operator is consumed.
        bool parse_compare(ParserState& ps)
        {
            return parse_left_assoc(ps, parse_is_expr, comparison_or_in_operator);
        }

        // -- Parse conjunction: compare { and compare }.
        bool parse_and_expr(ParserState& ps)
        {
            return parse_left_assoc(ps, parse_compare, conjunction_operator);
        }

        // -- Control flow expressions
        //
        // Boot has several control-flow keywords that appear at the
        // expression level (between logical operators and conditionals):
        //
        //   return expr   - return from the enclosing function
        //   leave expr    - exit from the enclosing loop (like `break`)
        //   throw expr    - raise an exception
        //   do statement  - execute a statement for its side effects
        //
        // These all parse their operand at a specific precedence level:
        //   - `return` takes a full assignment (loosest in this group)
        //   - `leave` takes a logical expression
        //   - `throw` takes an application (tight binding)
        //   - `do` takes a statement

        // -- Parse return/leave/throw, or fall through to and_expr.
        bool parse_return_expr(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::Return)
                return parse_prefix_kw(ps, parse_assign,
                                       &SyntaxForest::make_return);
            if (v == TokenValue::Leave)
                return parse_prefix_kw(ps, parse_logical,
                                       &SyntaxForest::make_leave);
            if (v == TokenValue::Throw)
                return parse_prefix_kw(ps, parse_application,
                                       &SyntaxForest::make_throw);
            // -- 'in namespace X do body': evaluate body in namespace X.
            //   Matches original bpDo: IN Namespace DO ...
            if (v == TokenValue::In)
            {
                auto in_tok = ps.current_index();
                ps.advance();
                if (not parse_namespace(ps))
                    return false;
                auto ns = ps.pop();
                ps.require(TokenValue::Do, "'do' after 'in namespace'");
                if (not parse_return_expr(ps))
                    return false;
                auto body = ps.pop();
                ps.push(ps.forest.make_coerce({ ns, in_tok, body }));
                return true;
            }
            // -- 'do' keyword: do statement.
            if (v == TokenValue::Do)
            {
                ps.advance();
                return parse_statement(ps);
            }
            return parse_and_expr(ps);
        }

        // -- Parse disjunction: return_expr { or return_expr }.
        bool parse_logical(ParserState& ps)
        {
            return parse_left_assoc(ps, parse_return_expr, disjunction_operator);
        }

        // -- Parse conditional: if cond then consequent [else alternate].
        //
        //    Boot's `if` is an expression (has a value), not a
        //    statement.  Both branches can be multi-line piles.
        //
        //    The pile phase handles coagulation of multi-line
        //    conditions (e.g. `if cond1\n  and cond2 then ...`)
        //    by merging lines that end with infix operators.
        //    Inside delimiters, no formatting tokens are emitted.
        //    For multi-line then/else bodies, the pile phase wraps
        //    siblings with Indent/Justify/Unindent, consumed by
        //    parse_pile.
        bool parse_conditional(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::If)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            if (not parse_where(ps))
                return false;
            auto cond = ps.pop();
            ps.require(TokenValue::Then, "'then'");
            if (not parse_where(ps))
                return false;
            auto consequent = ps.pop();
            auto alternate = NodeIndex::none;
            // -- Check for 'else' which may follow:
            //   (a) a Justify token (else at same column as if),
            //   (b) directly (no intervening formatting), or
            //   (c) an Indent token (when `if` is inline after `==`
            //       and `else` is on a subsequent continuation line).
            {
                auto snap_j = ps.save();
                bool else_indented = false;
                if (ps.match(TokenValue::Justify)
                    and ps.peek_value() == TokenValue::Else)
                {
                    ps.advance();
                    if (parse_where(ps))
                        alternate = ps.pop();
                }
                else
                {
                    ps.restore(snap_j);
                    if (ps.match(TokenValue::Else))
                    {
                        if (parse_where(ps))
                            alternate = ps.pop();
                    }
                    else if (ps.peek_value() == TokenValue::Indent)
                    {
                        auto snap_i = ps.save();
                        ps.advance();
                        if (ps.peek_value() == TokenValue::Else)
                        {
                            else_indented = true;
                            ps.advance();
                            if (parse_where(ps))
                                alternate = ps.pop();
                        }
                        else
                        {
                            ps.restore(snap_i);
                        }
                    }
                }
                if (else_indented)
                    ps.match(TokenValue::Unindent);
            }
            ps.push(ps.forest.make_if({ kw, cond, consequent, alternate }));
            return true;
        }

        // -- Parse exit (guard): assign [ => where ].
        //
        //    A guard expression uses `=>` (fat arrow) to associate
        //    a condition with an action.  In pile context, this is
        //    the primary way to write multi-branch control flow:
        //
        //      condition1 => action1
        //      condition2 => action2
        //      otherwise  => default_action
        //
        //    The left side is the condition (parsed as assignment),
        //    the right side is the consequent body (parsed as where).
        //    The Implies node represents the `=>` relationship.
        //
        //    Guard expressions appear frequently in Boot's `case`
        //    and `cond`-like patterns (piles of alternatives).
        bool parse_exit(ParserState& ps)
        {
            if (not parse_assign(ps))
                return false;

            // -- Spad: infix 'with' - category extension (expr with body).
            if (ps.peek_value() == TokenValue::With)
            {
                auto kw = ps.current_index();
                ps.advance();
                auto lhs = ps.pop();
                if (not parse_with_body(ps))
                    throw ParseError("expected body after 'with'");
                auto body = ps.pop();
                auto w = ps.forest.make_with({ kw, body });
                ps.push(ps.forest.make_infix({ lhs, kw, w }));
                // -- Allow chained: ... with { ... } add { ... }
                if (ps.peek_value() == TokenValue::Add)
                {
                    auto akw = ps.current_index();
                    ps.advance();
                    auto wexpr = ps.pop();
                    if (not parse_capsule(ps))
                        throw ParseError("expected body after 'add'");
                    auto abody = ps.pop();
                    auto a = ps.forest.make_add({ akw, abody });
                    ps.push(ps.forest.make_infix({ wexpr, akw, a }));
                }
                return true;
            }

            // -- Spad: infix 'add' - domain implementation (expr add body).
            if (ps.peek_value() == TokenValue::Add)
            {
                auto kw = ps.current_index();
                ps.advance();
                auto lhs = ps.pop();
                if (not parse_capsule(ps))
                    throw ParseError("expected body after 'add'");
                auto body = ps.pop();
                auto a = ps.forest.make_add({ kw, body });
                ps.push(ps.forest.make_infix({ lhs, kw, a }));
                return true;
            }

            // -- Handle `=>` on the same line or on a continuation line
            // (indented deeper, preceded by an Indent token).
            bool continued = skip_continuation_indent(ps,
                [](TokenValue v) { return v == TokenValue::Implies; });
            if (ps.peek_value() == TokenValue::Implies)
            {
                auto arrow = ps.current_index();
                ps.advance();
                auto cond = ps.pop();
                if (not parse_where(ps))
                    return true;
                auto body = ps.pop();
                ps.push(ps.forest.make_implies({ cond, arrow, body }));
            }
            if (continued)
                ps.match(TokenValue::Unindent);
            return true;
        }

        // -- Loops and iterators
        //
        // Boot and Spad support these iterator and guard forms:
        //
        //   for x in collection         - iterate over elements
        //   for x in lo..hi             - iterate over a range
        //   while condition             - loop while condition holds
        //   until condition             - loop until condition holds
        //   | predicate                 - such-that guard (filter)
        //
        // Iterators can be combined with `cross`:
        //   for x in xs cross for y in ys
        //   - Cartesian product: every (x,y) pair
        //
        // Without `cross`, adjacent iterators run in parallel (zip):
        //   for x in xs for y in ys
        //   - zipped: (x1,y1), (x2,y2), ...
        //
        // Loops are formed by iterators + `repeat` + body:
        //   for x in collection repeat
        //     process x
        //
        // The body can be a pile (multi-line), and `repeat` with
        // no iterators gives an infinite loop:
        //   repeat
        //     line := readLine()
        //     line = nil => leave
        //     process line

        // -- Parse a single iterator or guard clause.
        //
        //    Handles `for var in expr`, `while expr`, `until expr`,
        //    and `| predicate` (such-that guard).
        //    For `for`, nesting is bumped to suppress formatting
        //    tokens inside the iterator clause, because the collection
        //    expression may span multiple lines:
        //
        //      for x in remove(a,
        //                      remove(b, l))
        //
        //    The consumed Indent/Unindent pairs are tracked and
        //    drained after the clause is parsed.
        bool parse_iterator(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::For)
            {
                auto kw = ps.current_index();
                ps.advance();
                // -- The pile phase coagulates multi-line for-in
                // clauses (single child appended directly).
                // Parse formal: variable or dot (don't care).
                if (not parse_variable(ps) and not parse_dot(ps))
                    return false;
                // -- In Spad, `for var: free in` and `for var: local in`
                // declare the loop variable's storage class.
                // Skip the `: free`/`: local` modifier.
                if (ps.peek_value() == TokenValue::Colon)
                {
                    auto snap = ps.save();
                    ps.advance();
                    auto next = ps.peek_value();
                    if (next == TokenValue::Free)
                    {
                        ps.advance();   // consume 'free'
                    }
                    else
                    {
                        ps.restore(snap);
                    }
                }
                auto var = ps.pop();
                ps.require(TokenValue::In, "'in' after for-variable");
                if (not parse_segment(ps))
                    return false;
                auto seq = ps.pop();
                auto step = NodeIndex::none;
                if (ps.match(TokenValue::By))
                {
                    if (parse_arith(ps))
                        step = ps.pop();
                }
                ps.push(ps.forest.make_for_in({ kw, var, seq, step }));
                return true;
            }
            if (v == TokenValue::While)
                return parse_prefix_kw(ps, parse_logical,
                                       &SyntaxForest::make_while);
            if (v == TokenValue::Until)
                return parse_prefix_kw(ps, parse_logical,
                                       &SyntaxForest::make_until);
            if (v == TokenValue::Bar)
                return parse_prefix_kw(ps, parse_where,
                                       &SyntaxForest::make_such_that);
            return false;
        }

        // -- Parse iterators, possibly cross-product.
        //
        //    Multiple iterators may be combined.  Adjacent iterators
        //    without `cross` are zipped; with `cross`, they form a
        //    Cartesian product (represented as a Cross AST node).
        //
        //    Result is a list of iterator nodes, optionally joined
        //    by Cross nodes.  Returns false if no iterator is found.
        bool parse_iterators(ParserState& ps)
        {
            std::vector<NodeIndex> first_group;
            while (parse_iterator(ps))
                first_group.push_back(ps.pop());
            if (first_group.empty())
                return false;
            auto first_span = ps.make_span(first_group);
            auto first_node = ps.forest.make_iterators({ first_span });
            if (ps.peek_value() != TokenValue::Cross)
            {
                ps.push(first_node);
                return true;
            }
            std::vector<NodeIndex> factors;
            factors.push_back(first_node);
            while (ps.match(TokenValue::Cross))
            {
                ps.match(TokenValue::Justify);
                std::vector<NodeIndex> group;
                while (parse_iterator(ps))
                    group.push_back(ps.pop());
                if (not group.empty())
                {
                    auto span = ps.make_span(group);
                    factors.push_back(ps.forest.make_iterators({ span }));
                }
            }
            auto cross_span = ps.make_span(factors);
            ps.push(ps.forest.make_cross({ cross_span }));
            return true;
        }

        // -- Parse a loop: [iterators] repeat body.
        //
        //    A loop consists of optional iterators followed by
        //    `repeat` and a body.  The body is typically a pile
        //    (parsed by parse_pile).  If iterators are present,
        //    they are collected and wrapped with the repeat body
        //    in a Repeat node.
        //
        //    `repeat` alone (no iterators) creates an infinite loop.
        //    Use `leave` or `return` to exit.
        bool parse_loop(ParserState& ps)
        {
            if (ps.peek_value() == TokenValue::Repeat)
            {
                auto kw = ps.current_index();
                ps.advance();
                if (not parse_logical(ps))
                    return false;
                auto body = ps.pop();
                ps.push(ps.forest.make_repeat(
                    { NodeIndex::none, kw, body }));
                return true;
            }
            auto snap = ps.save();
            if (parse_iterators(ps))
            {
                auto kw = ps.current_index();
                bool continued = skip_continuation_indent(ps,
                    [](TokenValue v) { return v == TokenValue::Repeat; });
                ps.match(TokenValue::Repeat);
                auto iters = ps.pop();
                if (parse_where(ps))
                {
                    auto body = ps.pop();
                    if (continued)
                        ps.match(TokenValue::Unindent);
                    ps.push(ps.forest.make_repeat({ iters, kw, body }));
                    return true;
                }
                // -- Iterators without body - backtrack.
                ps.restore(snap);
            }
            return false;
        }

        // -- Exception handling
        //
        // Boot supports structured exception handling:
        //
        //   try
        //     dangerousOperation()
        //   catch(e: SomeException) =>
        //     handleError(e)
        //   catch(e: OtherException) =>
        //     handleOther(e)
        //   finally
        //     cleanup()
        //
        // The `try` body is an assign-level expression.  Each
        // `catch` clause names a variable with a type signature
        // and provides a handler body after `=>`.  An optional
        // `finally` clause runs unconditionally after the body.
        //
        // Between clauses, the parser tolerates stray Justify
        // (newline) and Semicolon tokens from formatting.

        // -- Parse try expression.
        bool parse_try_expr(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Try)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            if (not parse_assign(ps))
                return false;
            auto body = ps.pop();
            std::vector<NodeIndex> handlers;
            while (true)
            {
                ps.match(TokenValue::Justify);
                ps.match(TokenValue::Semicolon);
                if (ps.peek_value() != TokenValue::Catch)
                    break;
                auto catch_kw = ps.current_index();
                ps.advance();
                ps.require(TokenValue::OpenParen, "'(' in catch");
                if (not parse_name(ps))
                    return false;
                parse_signature_tail(ps);
                auto sig = ps.pop();
                ps.require(TokenValue::CloseParen, "')' in catch");
                ps.require(TokenValue::Implies, "'=>' in catch");
                if (not parse_assign(ps))
                    return false;
                auto handler_body = ps.pop();
                handlers.push_back(
                    ps.forest.make_catch({ catch_kw, sig, handler_body }));
            }
            ps.match(TokenValue::Justify);
            ps.match(TokenValue::Semicolon);
            if (ps.peek_value() == TokenValue::Finally)
            {
                auto fin_kw = ps.current_index();
                ps.advance();
                if (parse_assign(ps))
                {
                    auto fin_body = ps.pop();
                    handlers.push_back(
                        ps.forest.make_finally({ fin_kw, fin_body }));
                }
            }
            auto handler_span = ps.make_span(handlers);
            ps.push(ps.forest.make_try({ kw, body, handler_span }));
            return true;
        }

        // -- Constructs (bracket list with optional iterators)
        //
        // Boot's bracket expressions serve dual duty:
        //
        //   1. Plain lists: [1, 2, 3]
        //   2. List comprehensions: [f x for x in collection]
        //
        // A list comprehension is detected when the first comma
        // expression is followed by an iterator keyword (for, while,
        // until) or `repeat`.  The comprehension is represented as
        // a Repeat node inside the Bracket node.
        //
        // Inside brackets, nesting is bumped to suppress formatting
        // tokens, since bracket contents may span multiple lines:
        //
        //   [x for x in
        //        longListName]

        // -- Parse a construction: [ comma [iterators] ].
        //    In Boot, [expr for x in l | pred] is a list comprehension.
        // -- Parse the interior of a collection literal (bracket or
        //    brace), possibly with trailing iterators forming a
        //    comprehension.  Returns a (possibly empty) element list.
        static std::vector<NodeIndex> parse_collection_body(
            ParserState& ps,
            TokenValue close_tok)
        {
            std::vector<NodeIndex> elts;
            if (ps.peek_value() != close_tok)
            {
                if (parse_comma(ps))
                {
                    auto content = ps.pop();
                    if (starts_iterator(ps.peek_value())
                        or ps.peek_value() == TokenValue::Repeat)
                    {
                        ps.match(TokenValue::Repeat);
                        if (parse_iterators(ps))
                        {
                            auto iters = ps.pop();
                            auto collect = ps.forest.make_repeat(
                                { iters, ps.current_index(), content });
                            elts.push_back(collect);
                        }
                        else
                            elts.push_back(content);
                    }
                    else
                        elts.push_back(content);
                }
            }
            return elts;
        }

        bool parse_construct(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::OpenBracket)
                return false;
            auto open = ps.current_index();
            ps.advance();
            DelimiterGuard delim(ps);
            auto elts = parse_collection_body(ps, TokenValue::CloseBracket);
            auto close = ps.current_index();
            ps.require(TokenValue::CloseBracket, "']'");
            auto span = ps.make_span(elts);
            ps.push(ps.forest.make_bracket({ open, span, close }));
            return true;
        }

        // -- Assignment, lambda, expression statements
        //
        // This level of the expression grammar handles three postfix
        // operators that all follow a statement-level expression:
        //
        //   target := value     - assignment (mutable update)
        //   param +-> body      - lambda (anonymous function)
        //   key <- value        - key argument (named parameter)
        //
        // All three are right-associative: the right-hand side is
        // parsed recursively at the same level.  Assignment and
        // lambda are standard; key-arg is Boot-specific syntax for
        // passing named arguments to functions.

        // -- Parse assignment, lambda, or key-arg.
        bool parse_assign(ParserState& ps)
        {
            if (not parse_statement(ps))
                return false;
            auto v = ps.peek_value();
            if (v == TokenValue::ColonEq)
            {
                auto ceq = ps.current_index();
                ps.advance();
                auto target = ps.pop();
                if (not parse_assign(ps))
                    return true;
                auto val = ps.pop();
                ps.push(ps.forest.make_assignment({ target, ceq, val }));
                return true;
            }
            if (v == TokenValue::MapsTo)
            {
                auto arrow = ps.current_index();
                ps.advance();
                auto param = ps.pop();
                std::vector<NodeIndex> params = { param };
                auto span = ps.make_span(params);
                if (not parse_assign(ps))
                    return true;
                auto body = ps.pop();
                ps.push(ps.forest.make_lambda({ span, arrow, body }));
                return true;
            }
            if (v == TokenValue::LeftArrow)
            {
                auto arrow = ps.current_index();
                ps.advance();
                auto key = ps.pop();
                if (not parse_logical(ps))
                    return true;
                auto val = ps.pop();
                ps.push(ps.forest.make_key_arg({ key, arrow, val }));
                return true;
            }
            return true;
        }

        // -- Parse a statement: conditional, loop, try, or expression.
        //
        //    A statement is either a control-flow construct (if, for,
        //    while, until, repeat, try) or a plain expression.  This
        //    is the dispatch point where keyword-led constructs are
        //    separated from expression parsing.
        bool parse_statement(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::If)
                return parse_conditional(ps);
            if (v == TokenValue::For or v == TokenValue::While
                or v == TokenValue::Until or v == TokenValue::Repeat)
                return parse_loop(ps);
            if (v == TokenValue::Try)
                return parse_try_expr(ps);
            return parse_expression(ps);
        }

        // -- Parse an expression: [:] logical.
        //
        //    In Boot, a leading colon creates a COLON application,
        //    used for quoting or type-level references:
        //      :Symbol     - the symbol Symbol itself (not its value)
        //      :Package    - reference to a package by name
        //
        //    Without the colon prefix, an expression is just a
        //    logical-level expression.
        bool parse_expression(ParserState& ps)
        {
            
            if (ps.peek_value() == TokenValue::Colon)
            {
                auto colon = ps.current_index();
                ps.advance();
                if (not parse_logical(ps))
                    return false;
                auto operand = ps.pop();
                ps.push(ps.forest.make_prefix({ colon, operand }));
                return true;
            }
            return parse_logical(ps);
        }

        // -- Variable (parameter) parsing
        //
        // Variables appear in function parameter lists, macro
        // parameter lists, and destructuring patterns.  A variable
        // can be:
        //
        //   name              - simple name
        //   name: Type        - typed parameter
        //   name is pattern   - destructuring bind
        //   name == default   - parameter with default value
        //   (pattern)         - parenthesized pattern
        //   [pattern]         - list destructuring pattern
        //   .                 - don't-care placeholder
        //   "string", 42, etc - constant (matched literally)
        //
        // The `== default` form is only valid inside parenthesized
        // parameter lists (nesting > 0), to distinguish it from
        // `==` as a definition operator at top level.

        // -- Parse a variable: name [: type | is pattern | == default],
        //    or parenthesized/bracketed pattern.
        bool parse_variable(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::OpenParen)
                return parse_paren(ps, parse_comma);
            if (v == TokenValue::OpenBracket)
                return parse_bracket(ps, parse_pattern_list);
            if (v == TokenValue::Dot)
                return parse_dot(ps);
            if (parse_constant(ps))
                return true;
            if (parse_name(ps))
            {
                // -- Optional tail: signature, is-destructuring,
                // or default value.
                v = ps.peek_value();
                if (v == TokenValue::Colon)
                {
                    // -- In Spad, `var: free` is a storage modifier,
                    // not a type annotation.  Skip the signature.
                    auto snap = ps.save();
                    ps.advance();   // skip ':'
                    auto next = ps.peek_value();
                    ps.restore(snap);
                    if (next != TokenValue::Free)
                        parse_signature_tail(ps);
                }
                else if (v == TokenValue::Is)
                {
                    auto kw = ps.current_index();
                    ps.advance();
                    auto name = ps.pop();
                    if (parse_pattern_item(ps))
                    {
                        auto pat = ps.pop();
                        ps.push(ps.forest.make_is({ name, kw, pat }));
                    }
                    else
                    {
                        ps.push(name);
                    }
                }
                else if (v == TokenValue::EqEq and ps.delimiter_depth > 0)
                {
                    // -- Default value: name == expr.  Only valid inside
                    // parenthesized parameter lists, not at top level
                    // where == means a definition body.
                    auto def = ps.current_index();
                    ps.advance();
                    auto name = ps.pop();
                    if (parse_application(ps))
                    {
                        auto val = ps.pop();
                        ps.push(ps.forest.make_default_value(
                            { name, def, val }));
                    }
                    else
                    {
                        ps.push(name);
                    }
                }
                return true;
            }
            return false;
        }

        // -- Definitions
        //
        // Boot has three kinds of definitions:
        //
        //   macro name params == body
        //     - Textual substitution macro.  Parameters are variables.
        //
        //   name params == body
        //     - Function or constant definition (depending on
        //       whether params are present).
        //
        //   namespace N == body
        //     - Named namespace binding.
        //
        // The `==` operator separates the left-hand side (signature)
        // from the right-hand side (body).  The body can be an
        // inline expression or a pile (indented block):
        //
        //   factorial n ==
        //     n = 0 => 1
        //     n * factorial(n - 1)
        //
        // Definitions can be nested inside `where` clauses for
        // local bindings.

        // -- Parse a definition: macro, constant, or function.
        bool parse_definition(ParserState& ps)
        {
            
            auto v = ps.peek_value();

            // -- Macro definition.
            if (v == TokenValue::Macro)
            {
                auto kw = ps.current_index();
                ps.advance();
                if (not parse_name(ps))
                    return false;
                auto name = ps.pop();
                std::vector<NodeIndex> params;
                while (parse_variable(ps))
                    params.push_back(ps.pop());
                auto definer = ps.require(TokenValue::EqEq, "'==' in macro");
                if (not parse_where(ps))
                    return false;
                auto body = ps.pop();
                auto span = ps.make_span(params);
                ps.push(ps.forest.make_macro_def(
                    { kw, name, span, definer, body }));
                return true;
            }

            // -- Namespace directive followed by '=='.
            if (v == TokenValue::Namespace)
            {
                auto snap = ps.save();
                if (parse_namespace(ps))
                {
                    if (ps.peek_value() == TokenValue::EqEq)
                    {
                        auto definer = ps.current_index();
                        ps.advance();
                        auto lhs = ps.pop();
                        if (not parse_where(ps))
                            return false;
                        auto body = ps.pop();
                        ps.push(ps.forest.make_constant_def(
                            { lhs, definer, body }));
                        return true;
                    }
                    // -- Just a namespace directive.
                    return true;
                }
                ps.restore(snap);
            }

            // -- Try to parse exit first (covers most expressions).
            if (not parse_exit(ps))
                return false;

            v = ps.peek_value();

            // -- Handle continuation: INDENT before == or ==>
            // (Spad places == add on a half-indented line).
            bool definition_continued = false;
            if (v == TokenValue::Indent)
            {
                auto snap = ps.save();
                ps.advance();
                v = ps.peek_value();
                if (v == TokenValue::EqEq or v == TokenValue::FatArrow)
                    definition_continued = true;
                else
                {
                    ps.restore(snap);
                    v = ps.peek_value();
                }
            }

            // -- Definition: ... == body.
            if (v == TokenValue::EqEq)
            {
                auto definer = ps.current_index();
                ps.advance();
                auto lhs = ps.pop();
                while (ps.peek_value() == TokenValue::Justify)
                    ps.advance();
                if (not parse_where(ps)
                    and not parse_pile(ps, parse_definition_item))
                    return false;
                auto body = ps.pop();
                ps.push(ps.forest.make_constant_def(
                    { lhs, definer, body }));
                if (definition_continued)
                    ps.match(TokenValue::Unindent);
                return true;
            }

            // -- Spad: ... ==> expansion (macro definition).
            if (v == TokenValue::FatArrow)
            {
                auto definer = ps.current_index();
                ps.advance();
                auto lhs = ps.pop();
                if (not parse_where(ps))
                    return false;
                auto body = ps.pop();
                ps.push(ps.forest.make_macro_def(
                    { definer, lhs, ps.make_span({}), definer, body }));
                if (definition_continued)
                    ps.match(TokenValue::Unindent);
                return true;
            }

            if (definition_continued)
                ps.match(TokenValue::Unindent);
            return true;
        }

        // -- Parse a definition item (for where-clauses and piles).
        //
        //    A definition item is either a simple `name == body`
        //    form (tried first with tentative parsing) or a full
        //    definition.  This two-phase approach allows local
        //    definitions inside piles and where-clauses to use the
        //    simpler syntax without ambiguity.
        bool parse_definition_item(ParserState& ps)
        {
            auto snap = ps.save();
            // -- Try name == body (simple definition).
            if (parse_name(ps))
            {
                if (ps.peek_value() == TokenValue::EqEq)
                {
                    auto definer = ps.current_index();
                    ps.advance();
                    auto name = ps.pop();
                    if (parse_where(ps) or parse_pile(ps, parse_definition_item))
                    {
                        auto body = ps.pop();
                        ps.push(ps.forest.make_constant_def(
                            { name, definer, body }));
                        return true;
                    }
                }
                // -- Name followed by params == body (compound def).
                ps.restore(snap);
            }
            // -- Fall through to full definition parse.
            // Use parse_where so that expression-level where-clauses
            // (e.g. `(x := foo()) where bar() == ...`) are handled.
            return parse_where(ps);
        }

        // -- Where clause
        //
        // The `where` keyword introduces local definitions that
        // scope over the preceding expression:
        //
        //   result where
        //     helper x == x + offset
        //     offset == 42
        //
        // The local definitions can be in a pile (indented block),
        // parenthesized, or a single inline definition.

        // -- Parse: definition [ where local-defs ].
        bool parse_where(ParserState& ps)
        {
            
            if (not parse_definition(ps)
                and not parse_pile(ps, parse_definition_item))
                return false;

            // -- Function application with Indent-pile argument.
            //
            // In Boot, a function name (or call) followed by a
            // deeper-indented pile means the pile is the argument:
            //
            //     bcHt
            //       $saturn => '" {\ttrarrow} "
            //       '" -> "
            //
            // At this point all continuation operators (+, and, =>,
            // etc.) have already been handled by the expression
            // parser chain, so any remaining Indent is safe to
            // consume as the function argument.
            //
            // Guard: only extend expressions, not definitions or
            // where-clauses (which already consumed their bodies).
            if (ps.peek_value() == TokenValue::Indent)
            {
                auto top = ps.stack.back();
                auto k = kind(top);
                if (k != Kind::ConstantDef
                    and k != Kind::FunctionDef
                    and k != Kind::MacroDef
                    and k != Kind::Where
                    and k != Kind::Pile)
                {
                    auto snap2 = ps.save();
                    auto fn = ps.pop();
                    if (parse_pile(ps, parse_semicolon))
                    {
                        push_apply(ps, fn);
                    }
                    else
                    {
                        ps.restore(snap2);
                        ps.push(fn);
                    }
                }
            }

            // -- The `where` keyword may appear on a deeper continuation
            // line (e.g. after a multi-line bracket expression),
            // hidden behind an Indent token.  Tentatively skip it
            // to check for `where`.
            bool skipped_indent = false;
            auto v = ps.peek_value();
            if (v != TokenValue::Where and v == TokenValue::Indent)
            {
                auto snap = ps.save();
                ps.advance();    // skip the Indent token
                v = ps.peek_value();
                if (v != TokenValue::Where)
                {
                    ps.restore(snap);
                    return true;
                }
                skipped_indent = true;
            }

            if (v == TokenValue::Where)
            {
                auto kw = ps.current_index();
                ps.advance();
                auto body = ps.pop();
                std::vector<NodeIndex> defs;
                if (ps.peek_value() == TokenValue::Indent)
                {
                    if (parse_pile(ps, parse_definition_item))
                        defs.push_back(ps.pop());
                }
                else if (parse_paren(ps, parse_definition_item))
                {
                    defs.push_back(ps.pop());
                }
                else if (parse_definition_item(ps))
                {
                    defs.push_back(ps.pop());
                }
                // -- If we skipped an Indent to find `where`, the
                // matching Unindent follows the where-clause content.
                // Consume it so the enclosing pile's Unindent isn't
                // stolen.
                if (skipped_indent)
                    ps.match(TokenValue::Unindent);
                auto span = ps.make_span(defs);
                ps.push(ps.forest.make_where({ body, kw, span }));
            }
            return true;
        }

        // -- Semicolon sequences
        //
        // Semicolons separate statements within a single line or
        // inside parentheses.  parse_semicolon wraps parse_comma
        // in a semicolon-separated list.

        bool parse_semicolon(ParserState& ps)
        {
            return parse_semi_list(ps, parse_comma);
        }

        // -- Module system
        //
        // Boot has a simple module system with three constructs:
        //
        //   module Name(export1, export2, ...) where
        //     definitions...
        //   - Declares a module with named exports and a body.
        //
        //   import Name: Type for LocalAlias
        //   - Imports a name with an explicit type signature,
        //     optionally renaming it with `for`.  The type can
        //     span multiple lines (nesting is suppressed).
        //
        //   import namespace Name
        //   - Imports all exports from a namespace.
        //
        //   namespace Name
        //   - Declares or references a namespace.  `namespace .`
        //     refers to the current namespace.

        // -- Parse module declaration: module Name(exports) where body.
        bool parse_module(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Module)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            if (not parse_name(ps))
                return false;
            auto name = ps.pop();
            NodeSpan exports = { 0, 0 };
            if (ps.peek_value() == TokenValue::OpenParen)
            {
                ps.advance();
                DelimiterGuard delim(ps);
                std::vector<NodeIndex> export_list;
                if (parse_name(ps))
                {
                    export_list.push_back(ps.pop());
                    while (ps.match(TokenValue::Comma))
                    {
                        if (not parse_name(ps))
                            break;
                        export_list.push_back(ps.pop());
                    }
                }
                ps.require(TokenValue::CloseParen, "')' in module exports");
                exports = ps.make_span(export_list);
            }
            NodeSpan interface = { 0, 0 };
            if (ps.match(TokenValue::Where))
            {
                std::vector<NodeIndex> items;
                if (ps.peek_value() == TokenValue::Indent)
                {
                    if (parse_pile(ps, parse_definition))
                        items.push_back(ps.pop());
                }
                else if (parse_definition(ps))
                {
                    items.push_back(ps.pop());
                }
                interface = ps.make_span(items);
            }
            ps.push(ps.forest.make_module({ kw, name, exports, interface }));
            return true;
        }

        // -- Parse import: import name | import name: type [for alias]
        //    | import namespace name.
        //
        //    The full import form with type signature uses nesting
        //    suppression so that multi-line type expressions are
        //    handled correctly.  After parsing, consumed Indent
        //    tokens are matched by draining corresponding Unindents.
        bool parse_import(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Import)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            if (ps.peek_value() == TokenValue::Namespace)
            {
                ps.advance();
                if (not parse_name(ps))
                    return false;
                auto name = ps.pop();
                auto ns = ps.forest.make_namespace({ kw, name });
                ps.push(ps.forest.make_import({ kw, ns }));
                return true;
            }
            // -- In Spad, the imported expression can be a full domain
            // application (e.g. import DistinctDegreeFactorize(F,FP)
            // or import PointPackage DoubleFloat).  Use
            // parse_application so that parameterized domains and
            // juxtaposition-style applications are captured fully.
            // For Boot, parse_application on a bare name degenerates
            // to parse_name, so both dialects are handled correctly.
            if (not parse_application(ps))
                return false;
            auto what = ps.pop();
            if (ps.peek_value() == TokenValue::Colon)
            {
                auto colon = ps.current_index();
                ps.advance();
                NodeIndex alias = NodeIndex::none;
                NodeIndex provenance = NodeIndex::none;
                NodeIndex sig;
                if (not parse_typing(ps))
                    throw ParseError("expected type after ':' in import");
                auto type = ps.pop();
                sig = ps.forest.make_signature({ what, colon, type });
                if (ps.match(TokenValue::For))
                {
                    if (parse_name(ps))
                        alias = ps.pop();
                }
                if (ps.match(TokenValue::From))
                {
                    if (parse_application(ps))
                        provenance = ps.pop();
                }
                ps.push(ps.forest.make_import_signature(
                    { kw, sig, alias, provenance }));
                return true;
            }
            // -- Spad: bare import from Domain (no signature).
            if (ps.peek_value() == TokenValue::From)
            {
                ps.advance();
                if (not parse_application(ps))
                    throw ParseError("expected domain after 'from' in import");
                auto source = ps.pop();
                ps.push(ps.forest.make_import({ kw, source }));
                return true;
            }
            ps.push(ps.forest.make_import({ kw, what }));
            return true;
        }

        // -- Parse namespace: namespace name | namespace .
        //    The dot form refers to the current (enclosing) namespace.
        bool parse_namespace(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Namespace)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            // -- 'namespace .' means the current namespace.
            if (not parse_name(ps) and not parse_dot(ps))
                return false;
            auto name = ps.pop();
            ps.push(ps.forest.make_namespace({ kw, name }));
            return true;
        }

        // -- Structure and case
        //
        // Boot's `structure` keyword defines algebraic data types:
        //
        //   structure Pair ==
        //     Record(first: %Thing, second: %Thing)
        //
        // The body after `==` is typically a pile of variant
        // definitions.
        //
        // The `case` keyword provides multi-way dispatch:
        //
        //   case expr of
        //     pattern1 => action1
        //     pattern2 => action2
        //     otherwise => default
        //
        // The `of` keyword introduces the branches, which are
        // parsed as a pile of guard (exit) expressions.

        // -- Parse an accessor definition: name == (.field)
        bool parse_accessor_def(ParserState& ps)
        {
            if (not parse_name(ps))
                return false;
            auto name = ps.pop();
            auto definer = ps.require(TokenValue::EqEq,
                "'==' in accessor definition");
            if (not parse_where(ps))
                return false;
            auto selector = ps.pop();
            ps.push(ps.forest.make_accessor_def(
                { name, definer, selector }));
            return true;
        }

        // -- Parse structure: structure Name == variants.
        bool parse_struct(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Structure)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            if (not parse_name(ps))
                return false;
            auto name = ps.pop();
            auto definer = ps.require(TokenValue::EqEq, "'==' in structure");
            std::vector<NodeIndex> variants;
            if (ps.peek_value() == TokenValue::Indent)
            {
                if (parse_pile(ps, parse_definition))
                    variants.push_back(ps.pop());
            }
            else if (parse_definition(ps))
            {
                variants.push_back(ps.pop());
            }

            // -- Check for 'with' clause (Record accessors).
            if (ps.peek_value() == TokenValue::With)
            {
                auto with_kw = ps.current_index();
                ps.advance();
                // -- Parse accessor definitions (pile or single).
                std::vector<NodeIndex> fields;
                std::vector<NodeIndex> accessors;
                // -- Move existing variants to fields.
                fields = std::move(variants);
                variants.clear();
                if (ps.peek_value() == TokenValue::Indent)
                {
                    // -- Pile of accessor definitions.
                    if (ps.match(TokenValue::Indent))
                    {
                        if (parse_accessor_def(ps))
                            accessors.push_back(ps.pop());
                        while (ps.match(TokenValue::Justify))
                        {
                            if (parse_accessor_def(ps))
                                accessors.push_back(ps.pop());
                        }
                        ps.match(TokenValue::Unindent);
                    }
                }
                else if (parse_accessor_def(ps))
                {
                    accessors.push_back(ps.pop());
                }
                auto fields_span = ps.make_span(fields);
                auto accessors_span = ps.make_span(accessors);
                auto record = ps.forest.make_record(
                    { with_kw, fields_span, accessors_span });
                variants.push_back(record);
            }

            auto span = ps.make_span(variants);
            ps.push(ps.forest.make_structure({ kw, name, definer, span }));
            return true;
        }

        // -- Parse case expression: case expr [of branches].
        //    Branches are a pile of guard expressions (cond => body).
        //    If `of` is absent, this is a simple case wrapper.
        bool parse_case_expr(ParserState& ps)
        {
            if (ps.peek_value() != TokenValue::Case)
                return false;
            auto kw = ps.current_index();
            ps.advance();
            if (not parse_where(ps))
                return false;
            auto expr = ps.pop();
            // -- 'of' might not be present in all case expressions.
            if (ps.match(TokenValue::Of))
            {
                std::vector<NodeIndex> branches;
                if (ps.peek_value() == TokenValue::Indent)
                {
                    ps.advance();
                    while (ps.peek_value() != TokenValue::Unindent
                           and ps.peek_value() != TokenValue::EndOfStream)
                    {
                        if (parse_exit(ps))
                            branches.push_back(ps.pop());
                        ps.match(TokenValue::Justify);
                    }
                    ps.match(TokenValue::Unindent);
                }
                auto span = ps.make_span(branches);
                ps.push(ps.forest.make_case({ kw, expr, span }));
            }
            else
            {
                // -- Simple case (expression after `case`).
                ps.push(ps.forest.make_case(
                    { kw, expr, ps.make_span({}) }));
            }
            return true;
        }

        // -- Spad: capsule and with-body parsers

        // -- Parse a brace-delimited list of items separated by
        //    semicolons and/or newlines (Justify tokens).
        //    Returns the items; caller wraps in the appropriate node.
        static std::vector<NodeIndex> parse_braced_items(
            ParserState& ps,
            bool (*parser)(ParserState&))
        {
            DelimiterGuard delim(ps);
            std::vector<NodeIndex> items;
            while (ps.peek_value() != TokenValue::CloseBrace
                   and ps.peek_value() != TokenValue::EndOfStream)
            {
                if (parser(ps))
                    items.push_back(ps.pop());
                else
                    break;
                ps.match(TokenValue::Semicolon);
                ps.match(TokenValue::Justify);
            }
            ps.require(TokenValue::CloseBrace, "'}'");
            return items;
        }

        // -- Parse a capsule body: { items } | pile | single expr.
        //
        //    A capsule is the implementation body of a Spad domain,
        //    appearing after 'add'.
        bool parse_capsule(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::OpenBrace)
            {
                ps.advance();
                auto items = parse_braced_items(ps, parse_comma);
                auto span = ps.make_span(items);
                ps.push(ps.forest.make_capsule({ span }));
                return true;
            }
            // -- Pile or single expression.
            if (v == TokenValue::Indent)
            {
                if (not parse_pile(ps, parse_comma))
                    return false;
                auto body = ps.pop();
                auto span = ps.make_span({ body });
                ps.push(ps.forest.make_capsule({ span }));
                return true;
            }
            if (parse_comma(ps))
            {
                auto item = ps.pop();
                auto span = ps.make_span({ item });
                ps.push(ps.forest.make_capsule({ span }));
                return true;
            }
            return false;
        }

        // -- Parse a with-body: { items } | pile | single expr.
        //
        //    The body of a category definition (after 'with').
        bool parse_with_body(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::OpenBrace)
            {
                ps.advance();
                auto items = parse_braced_items(ps, parse_comma);
                if (items.size() == 1)
                    ps.push(items[0]);
                else
                {
                    auto span = ps.make_span(items);
                    ps.push(ps.forest.make_sequence({ span }));
                }
                return true;
            }
            // -- Pile form.
            if (v == TokenValue::Indent)
                return parse_pile(ps, parse_comma);
            // -- Single expression.
            return parse_comma(ps);
        }

        // -- Comma (top-level)
        //
        // Comma is the loosest-binding operator in Boot.  At the
        // top level, a comma separates elements in a tuple or
        // parameter list.  The comma parser also dispatches to
        // module-system constructs (module, import, namespace)
        // because these can appear wherever a comma-expression can.
        //
        // Inside brackets, a variant (parse_bracket_comma) handles
        // colon-prefixed items for list append syntax: [:rest].

        // -- Parse a comma expression, or module/import/namespace.
        bool parse_comma(ParserState& ps)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::Module)
                return parse_module(ps);
            if (v == TokenValue::Import)
                return parse_import(ps);
            if (v == TokenValue::Namespace)
                return parse_namespace(ps);
            return parse_tuple_of(ps, parse_where);
        }

    } // unnamed namespace

    // -- Public API
    //
    // parse_toplevel: Parse one top-level form from the token
    // stream.  Returns NodeIndex::none at end-of-stream.
    //
    // parse_boot: Parse an entire Boot source file.  Returns
    // a vector of top-level AST node indices.  The parser
    // skips stray formatting tokens (Justify, Indent, Unindent,
    // Semicolon) between top-level forms, and reports errors
    // for anything else that fails to parse.

    NodeIndex parse_toplevel(ParserState& ps)
    {
        // -- Skip stray formatting tokens between top-level forms.
        for (;;)
        {
            auto v = ps.peek_value();
            if (v == TokenValue::Justify or v == TokenValue::Indent
                or v == TokenValue::Unindent)
                ps.advance();
            else
                break;
        }
        if (ps.peek_value() == TokenValue::EndOfStream)
            return NodeIndex::none;
        if (not parse_comma(ps))
            return NodeIndex::none;
        return ps.pop();
    }

    std::vector<NodeIndex> parse_boot(
        const std::vector<Token>& tokens,
        SyntaxForest& forest)
    {
        ParserState ps(tokens, forest);
        std::vector<NodeIndex> forms;
        while (true)
        {
            auto node = parse_toplevel(ps);
            if (not Syntax::valid(node))
                break;
            forms.push_back(node);
        }
        return forms;
    }

    std::vector<NodeIndex> parse_spad(
        const std::vector<Token>& tokens,
        SyntaxForest& forest,
        std::size_t* consumed)
    {
        ParserState ps(tokens, forest, Language::Spad);
        std::vector<NodeIndex> forms;
        while (true)
        {
            auto node = parse_toplevel(ps);
            if (not Syntax::valid(node))
                break;
            forms.push_back(node);
        }
        if (consumed != nullptr)
            *consumed = ps.position;
        return forms;
    }
}

// -- Legacy Boot-to-Lisp transpiler stub.
//  Tokenizes a Boot source file and writes a formatted token dump.
namespace {
   using namespace OpenAxiom;

   struct OutputFile {
      const char* path;
      std::ofstream stream;
      OutputFile(const char* p, std::ios_base::openmode flags)
            : path{ p }, stream{ p, flags }
      {
         if (!stream)
            throw Error::CannotOpenFile{ path };
      }
   };

   void format_text(const Fragment& f, const Token& t, std::ostream& os) {
      if (t.start.line == t.end.line) {
         auto& line = f(t.start);
         auto sv = std::u8string_view(line).substr(t.start.column,
                                                   t.end.column - t.start.column);
         os << sv;
      }
      else {
         auto& first_line = f(t.start);
         os << std::u8string_view(first_line).substr(t.start.column);
         for (auto i = t.start.line + 1; i < t.end.line; ++i)
            os << std::u8string_view(f[i]);
         auto& last_line = f[t.end.line];
         os << std::u8string_view(last_line).substr(0, t.end.column);
      }
   }

   void format_token(const Fragment& f, const Token& t, std::ostream& os) {
      os << t.category << '{'
         << t.start << '-' << t.end
         << ", ";
      switch (t.category) {
      case TokenCategory::Integer:
      case TokenCategory::FloatingPoint:
      case TokenCategory::String:
      case TokenCategory::Identifier:
         format_text(f, t, os);
         break;
      default:
         os << t.value;
         break;
      }
      os << '}';
   }

   // -- FIXME: This is just a stub to get a native parsing entry point
   //        into the bootsys and interpsys images.
   void transpile_boot_to_lisp(const char* in_path,
                                Memory::FileMapping& in,
                                OutputFile& out) {
      out.stream << "## Input: " << in_path << '\n'
                 << "## Output: " << out.path << '\n';

      Utf8SourceView source { in.begin(), in.size() };
      for (auto& f : read_source(source.view()))
      {
         out.stream << "================================================\n";
         out.stream << f;
         try {
            for (auto& t : words(f, Language::Boot)) {
               out.stream << '\t';
               format_token(f, t, out.stream);
               switch (t.category) {
               case TokenCategory::Junk:
               case TokenCategory::Unclassified:
                  out.stream
                     << " in file " << in_path
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
            std::cerr << in_path << ": syntax error: "
                      << "premature end of line before matching quote "
                      << "of string literal on line " << e.line
                      << " at column " << e.column
                      << std::endl;
         }
         catch (const MissingExponent& e) {
            std::cerr << in_path << ": syntax error: "
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
      Memory::FileMapping in { boot_path };
      OutputFile out { lisp_path, std::ios_base::binary };
      transpile_boot_to_lisp(boot_path, in, out);
      return 0;
   }
   catch (const Error::CannotOpenFile& e) {
      Diagnostics::StandardStream diagnostics { };
      diagnostics.error() << "error: could not open file `"
                          << e.path << "'\n";
      return -1;
   }
}
