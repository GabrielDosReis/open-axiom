// -- -*- C++ -*-
// Copyright (C) 2026, Gabriel Dos Reis.
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
// --%   CST-to-AST lowering.  Converts a SyntaxForest CST node
// --%   into a Lisp value matching the old Boot translator's %Ast
// --%   representation:
// --%     (CONS '%Tag (LIST arg1 arg2 ...))
// --%   Atoms (names, literals) become symbols, integers, floats,
// --%   or strings directly.

#include <stdexcept>
#include <open-axiom/BootAst>
#include <open-axiom/SyntaxPrint>

namespace OpenAxiom::Boot {

    using namespace Syntax;
    using namespace Lisp;

    // -- Forward declaration.
    Value lower(NodeIndex node, LoweringContext& ctx);

    // -- Extract the source text of a token and return a Lisp symbol.
    // -- Process Boot underscore-escape sequences.
    //    In Boot, `_` is an escape character:
    //    - `_x` (where x is a non-whitespace char) includes x
    //      literally in the identifier.  So `_-` > `-`, `_*` > `*`.
    //    - `_` followed by whitespace (space, tab, newline) is a
    //      line continuation: skip the underscore and all following
    //      whitespace.
    std::u8string unescape_boot_id(std::u8string_view text)
    {
        std::u8string result;
        result.reserve(text.size());
        for (size_t i = 0; i < text.size(); ++i)
        {
            if (text[i] == u8'_' and i + 1 < text.size())
            {
                ++i;
                if (text[i] == u8' ' or text[i] == u8'\t'
                    or text[i] == u8'\n' or text[i] == u8'\r')
                {
                    // -- Line continuation: skip all whitespace.
                    while (i < text.size()
                        and (text[i] == u8' ' or text[i] == u8'\t'
                            or text[i] == u8'\n'
                            or text[i] == u8'\r'))
                        ++i;
                    --i; // will be incremented by the for loop
                }
                else
                {
                    result += text[i];
                }
            }
            else
            {
                result += text[i];
            }
        }
        return result;
    }

    static Value tok_sym(TokenIndex ti, LoweringContext& ctx)
    {
        auto text = Syntax::token_text(ti, ctx.tokens, ctx.fragment);
        // -- Process underscore escapes for identifiers.
        if (text.find(u8'_') != std::u8string_view::npos)
        {
            auto clean = unescape_boot_id(text);
            return ctx.arena.make_symbol(clean);
        }
        return ctx.arena.make_symbol(text);
    }

    // -- Lower a span of children to a Lisp list.
    Value lower_span(NodeSpan span, LoweringContext& ctx)
    {
        Value result = nil;
        for (auto i = span.count; i > 0; --i)
            result = ctx.arena.cons(
                lower(ctx.forest.child(span, i - 1), ctx), result);
        return result;
    }

    // -- Construct a tagged AST node: (CONS tag (LIST args...))
    //    which is (%Tag arg1 arg2 ...).
    static Value tagged(LoweringContext& ctx, std::u8string_view tag,
                    std::initializer_list<Value> args)
    {
        return ctx.arena.cons(ctx.arena.make_symbol(tag),
                              ctx.arena.list(args));
    }

    // -- The main lowering function.
    Value lower(NodeIndex node, LoweringContext& ctx)
    {
        if (not valid(node))
            return nil;

        auto k = kind(node);
        auto idx = index(node);

        auto& forest = ctx.forest;

        switch (k) {

        // -- Atoms: become Lisp values directly.
        case Kind::Literal: {
            auto& n = forest.literal(idx);
            auto text = Syntax::token_text(n.token, ctx.tokens, ctx.fragment);
            // -- Determine if it's an integer, float, or string.
            if (not text.empty() and text[0] == u8'"')
                return ctx.arena.make_string(text.substr(1, text.size() - 2));
            // -- Try integer.
            bool all_digits = true;
            bool has_sign = false;
            for (std::size_t i = 0; i < text.size(); ++i)
            {
                char8_t c = text[i];
                if (i == 0 and (c == u8'+' or c == u8'-'))
                {
                    has_sign = true;
                    continue;
                }
                if (c < u8'0' or c > u8'9')
                {
                    all_digits = false;
                    break;
                }
            }
            if (all_digits and (not has_sign or text.size() > 1))
            {
                try
                {
                    return ctx.arena.make_integer(std::stoll(std::string(as_chars(text))));
                }
                catch (const std::out_of_range&) { }
                catch (const std::invalid_argument&) { }
            }
            // -- Try float.
            bool has_dot = false;
            for (char8_t c : text)
            {
                if (c == u8'.' or c == u8'e' or c == u8'E')
                {
                    has_dot = true;
                    break;
                }
            }
            if (has_dot)
            {
                try
                {
                    return ctx.arena.make_float(std::stod(std::string(as_chars(text))));
                }
                catch (const std::out_of_range&) { }
                catch (const std::invalid_argument&) { }
            }
            // -- Fallback: treat as a symbol.
            return ctx.arena.make_symbol(text);
        }

        case Kind::Name: {
            auto& n = forest.name(idx);
            return tok_sym(n.token, ctx);
        }

        // -- Module system --
        case Kind::QualifiedName: {
            auto& n = forest.qualified_name(idx);
            return tagged(ctx, u8"%QualifiedName",
                { lower(n.qualifier, ctx),
                  lower(n.name, ctx) });
        }

        case Kind::Module: {
            auto& n = forest.module(idx);
            return tagged(ctx, u8"%Module",
                { lower(n.name, ctx),
                  lower_span(n.exports, ctx),
                  lower_span(n.interface, ctx) });
        }

        case Kind::Namespace: {
            auto& n = forest.namespace_node(idx);
            return tagged(ctx, u8"%Namespace",
                { lower(n.name, ctx) });
        }

        case Kind::Import: {
            auto& n = forest.import_node(idx);
            return tagged(ctx, u8"%Import",
                { lower(n.what, ctx) });
        }

        case Kind::ImportSignature: {
            auto& n = forest.import_signature(idx);
            return tagged(ctx, u8"%ImportSignature",
                { lower(n.signature, ctx),
                  lower(n.name, ctx),
                  lower(n.provenance, ctx) });
        }

        // -- Definitions --
        case Kind::ConstantDef: {
            auto& n = forest.constant_def(idx);
            return tagged(ctx, u8"%ConstantDefinition",
                { lower(n.name, ctx),
                  lower(n.body, ctx) });
        }

        case Kind::FunctionDef: {
            auto& n = forest.function_def(idx);
            return tagged(ctx, u8"%Definition",
                { lower(n.name, ctx),
                  lower_span(n.parameters, ctx),
                  lower(n.body, ctx) });
        }

        case Kind::MacroDef: {
            auto& n = forest.macro_def(idx);
            return tagged(ctx, u8"%Macro",
                { lower(n.name, ctx),
                  lower_span(n.parameters, ctx),
                  lower(n.body, ctx) });
        }

        case Kind::TypeAlias: {
            auto& n = forest.type_alias(idx);
            return tagged(ctx, u8"%TypeAlias",
                { lower(n.name, ctx),
                  lower(n.type, ctx) });
        }

        case Kind::Signature: {
            auto& n = forest.signature(idx);
            return tagged(ctx, u8"%Signature",
                { lower(n.name, ctx),
                  lower(n.type, ctx) });
        }

        // -- Expressions --
        case Kind::Apply: {
            auto& n = forest.apply(idx);
            // -- The old AST: %Call(fun, %Sequence args)
            // where args is a list wrapped in (CONS '%Sequence args).
            // Actually, looking at the ast.boot definition:
            //   %Call(%Ast, %Sequence)
            // The second arg is the argument list as a sequence.
            auto fun = lower(n.function, ctx);
            auto args = lower_span(n.arguments, ctx);
            return tagged(ctx, u8"%Call", { fun, args });
        }

        case Kind::InfixExpr: {
            auto& n = forest.infix_expr(idx);
            return tagged(ctx, u8"%InfixExpr",
                { tok_sym(n.op, ctx),
                  lower(n.lhs, ctx),
                  lower(n.rhs, ctx) });
        }

        case Kind::PrefixExpr: {
            auto& n = forest.prefix_expr(idx);
            return tagged(ctx, u8"%PrefixExpr",
                { tok_sym(n.op, ctx),
                  lower(n.operand, ctx) });
        }

        case Kind::SuffixDot: {
            auto& n = forest.suffix_dot(idx);
            return tagged(ctx, u8"%SuffixDot",
                { lower(n.operand, ctx) });
        }

        case Kind::Restrict: {
            auto& n = forest.restrict_node(idx);
            return tagged(ctx, u8"%Restrict",
                { lower(n.expr, ctx),
                  lower(n.type, ctx) });
        }

        case Kind::Coerce: {
            // -- The old AST doesn't have %Coerce.
            // Coerce (::) is parsed as a type restriction in old Boot.
            // We map it to %Restrict for now.
            auto& n = forest.coerce(idx);
            return tagged(ctx, u8"%Restrict",
                { lower(n.expr, ctx),
                  lower(n.type, ctx) });
        }

        case Kind::Quote: {
            auto& n = forest.quote(idx);
            return tagged(ctx, u8"%Quote",
                { lower(n.expr, ctx) });
        }

        case Kind::Bracket: {
            auto& n = forest.bracket(idx);
            return tagged(ctx, u8"%Bracket",
                { lower_span(n.elements, ctx) });
        }

        case Kind::Tuple: {
            auto& n = forest.tuple(idx);
            return tagged(ctx, u8"%Tuple",
                { lower_span(n.elements, ctx) });
        }

        case Kind::Sequence: {
            auto& n = forest.sequence(idx);
            return ctx.arena.cons(
                ctx.arena.make_symbol(u8"APPEND"),
                lower_span(n.statements, ctx));
        }

        case Kind::Pile: {
            auto& n = forest.pile(idx);
            return tagged(ctx, u8"%Pile",
                { lower_span(n.items, ctx) });
        }

        // -- Types --
        case Kind::Mapping: {
            auto& n = forest.mapping(idx);
            return tagged(ctx, u8"%Mapping",
                { lower(n.target, ctx),
                  lower_span(n.sources, ctx) });
        }

        case Kind::Forall: {
            auto& n = forest.forall(idx);
            return tagged(ctx, u8"%Forall",
                { lower_span(n.variables, ctx),
                  lower(n.body, ctx) });
        }

        // -- Control flow --
        case Kind::IfExpr: {
            // -- The old parser doesn't produce %If directly;
            // it produces COND-like structures via bfIf.
            // But at AST level before translation, the parser
            // produces a form that bfIf turns into COND.
            // For our CST>AST, we preserve the conditional structure.
            auto& n = forest.if_expr(idx);
            auto cond = lower(n.condition, ctx);
            auto consq = lower(n.consequent, ctx);
            auto alt = lower(n.alternate, ctx);
            // -- If there's an alternate, produce bfIf(cond,consq,alt)
            // Otherwise, bfIfThenOnly(cond,consq)
            if (not valid(n.alternate))
                return tagged(ctx, u8"%Implies", { cond, consq });
            return ctx.arena.list({
                ctx.arena.make_symbol(u8"IF"), cond, consq, alt });
        }

        case Kind::Return: {
            auto& n = forest.return_node(idx);
            return tagged(ctx, u8"%Return",
                { lower(n.value, ctx) });
        }

        case Kind::Leave: {
            auto& n = forest.leave(idx);
            return tagged(ctx, u8"%Leave",
                { lower(n.value, ctx) });
        }

        case Kind::Implies: {
            auto& n = forest.implies(idx);
            return tagged(ctx, u8"%Implies",
                { lower(n.condition, ctx),
                  lower(n.body, ctx) });
        }

        // -- Loops --
        case Kind::While: {
            auto& n = forest.while_node(idx);
            return tagged(ctx, u8"%While",
                { lower(n.condition, ctx) });
        }

        case Kind::Until: {
            auto& n = forest.until_node(idx);
            return tagged(ctx, u8"%Until",
                { lower(n.condition, ctx) });
        }

        case Kind::ForIn: {
            auto& n = forest.for_in(idx);
            return tagged(ctx, u8"%For",
                { lower(n.variable, ctx),
                  lower(n.sequence, ctx),
                  lower(n.step, ctx) });
        }

        case Kind::SuchThat: {
            auto& n = forest.such_that(idx);
            return tagged(ctx, u8"%SuchThat",
                { lower(n.predicate, ctx) });
        }

        case Kind::Iterators: {
            auto& n = forest.iterators(idx);
            return tagged(ctx, u8"%Iterators",
                { lower_span(n.clauses, ctx) });
        }

        case Kind::Cross: {
            auto& n = forest.cross(idx);
            return tagged(ctx, u8"%Cross",
                { lower_span(n.factors, ctx) });
        }

        case Kind::Repeat: {
            auto& n = forest.repeat(idx);
            return tagged(ctx, u8"%Repeat",
                { lower(n.iterators, ctx),
                  lower(n.body, ctx) });
        }

        // -- Pattern matching --
        case Kind::Is: {
            auto& n = forest.is_test(idx);
            return tagged(ctx, u8"%Is",
                { lower(n.expr, ctx),
                  lower(n.pattern, ctx) });
        }

        case Kind::Isnt: {
            auto& n = forest.isnt_test(idx);
            return tagged(ctx, u8"%Isnt",
                { lower(n.expr, ctx),
                  lower(n.pattern, ctx) });
        }

        case Kind::EqualPattern: {
            auto& n = forest.equal_pattern(idx);
            return tagged(ctx, u8"%EqualPattern",
                { lower(n.expr, ctx) });
        }

        case Kind::ColonAppend: {
            auto& n = forest.colon_append(idx);
            return tagged(ctx, u8"%ColonAppend",
                { lower_span(n.head, ctx),
                  lower(n.rest, ctx) });
        }

        // -- Assignment & binding --
        case Kind::Assignment: {
            auto& n = forest.assignment(idx);
            return tagged(ctx, u8"%Assignment",
                { lower(n.target, ctx),
                  lower(n.value, ctx) });
        }

        case Kind::Lambda: {
            auto& n = forest.lambda(idx);
            return tagged(ctx, u8"%Lambda",
                { lower_span(n.parameters, ctx),
                  lower(n.body, ctx) });
        }

        case Kind::DefaultValue: {
            auto& n = forest.default_value(idx);
            return tagged(ctx, u8"%DefaultValue",
                { lower(n.name, ctx),
                  lower(n.value, ctx) });
        }

        case Kind::KeyArg: {
            auto& n = forest.key_arg(idx);
            return tagged(ctx, u8"%Key",
                { lower(n.key, ctx),
                  lower(n.value, ctx) });
        }

        // -- Segments --
        case Kind::BoundedSegment: {
            auto& n = forest.bounded_segment(idx);
            return tagged(ctx, u8"%BoundedSegment",
                { lower(n.lo, ctx),
                  lower(n.hi, ctx) });
        }

        case Kind::UnboundedSegment: {
            auto& n = forest.unbounded_segment(idx);
            return tagged(ctx, u8"%UnboundedSegment",
                { lower(n.lo, ctx) });
        }

        // -- Reduce --
        case Kind::Reduce: {
            auto& n = forest.reduce(idx);
            return tagged(ctx, u8"%Reduce",
                { lower(n.op, ctx),
                  lower(n.body, ctx) });
        }

        // -- Structural --
        case Kind::Structure: {
            auto& n = forest.structure(idx);
            return tagged(ctx, u8"%Structure",
                { lower(n.name, ctx),
                  lower_span(n.variants, ctx) });
        }

        case Kind::Record: {
            auto& n = forest.record(idx);
            return tagged(ctx, u8"%Record",
                { lower_span(n.fields, ctx),
                  lower_span(n.accessors, ctx) });
        }

        case Kind::AccessorDef: {
            auto& n = forest.accessor_def(idx);
            return tagged(ctx, u8"%AccessorDef",
                { lower(n.name, ctx),
                  lower(n.selector, ctx) });
        }

        case Kind::Case: {
            auto& n = forest.case_node(idx);
            return tagged(ctx, u8"%Case",
                { lower(n.expr, ctx),
                  lower_span(n.branches, ctx) });
        }

        // -- Exception handling --
        case Kind::Throw: {
            auto& n = forest.throw_node(idx);
            return tagged(ctx, u8"%Throw",
                { lower(n.expr, ctx) });
        }

        case Kind::Catch: {
            auto& n = forest.catch_node(idx);
            return tagged(ctx, u8"%Catch",
                { lower(n.signature, ctx),
                  lower(n.body, ctx) });
        }

        case Kind::Finally: {
            auto& n = forest.finally_node(idx);
            return tagged(ctx, u8"%Finally",
                { lower(n.body, ctx) });
        }

        case Kind::Try: {
            auto& n = forest.try_node(idx);
            return tagged(ctx, u8"%Try",
                { lower(n.body, ctx),
                  lower_span(n.handlers, ctx) });
        }

        // -- Scoping --
        case Kind::Where: {
            auto& n = forest.where(idx);
            return tagged(ctx, u8"%Where",
                { lower(n.body, ctx),
                  lower_span(n.definitions, ctx) });
        }

        case Kind::Dynamic: {
            auto& n = forest.dynamic(idx);
            return tagged(ctx, u8"%Dynamic",
                { lower(n.expr, ctx) });
        }

        // -- Embedded Lisp --
        case Kind::LispExpr: {
            auto& n = forest.lisp_expr(idx);
            auto text = Syntax::token_text(n.token, ctx.tokens, ctx.fragment);
            return tagged(ctx, u8"%Lisp",
                { ctx.arena.make_string(text) });
        }

        // -- Fallback --
        case Kind::Unparsed:
            return nil;

        default:
            return nil;
        }
    }
}
