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
// --%   Implementation of SyntaxForest: the arena that owns all syntax
// --%   tree nodes for a Boot translation unit.

#include <open-axiom/SyntaxTree>

namespace OpenAxiom::Syntax {

    // -- Allocate a contiguous span of `count` child slots in the
    // -- shared children array, initialized to NodeIndex::none.
    NodeSpan SyntaxForest::allocate_span(std::uint32_t count)
    {
        auto offset = static_cast<std::uint32_t>(children.size());
        children.resize(children.size() + count, NodeIndex::none);
        return { offset, count };
    }

    // -- Access the i-th child within a span (mutable).
    NodeIndex& SyntaxForest::child(NodeSpan span, std::uint32_t i)
    {
        return children[span.offset + i];
    }

    // -- Access the i-th child within a span (const).
    const NodeIndex& SyntaxForest::child(NodeSpan span, std::uint32_t i) const
    {
        return children[span.offset + i];
    }

    // -- Helper macro: push a node into a per-kind vector and return
    // the corresponding NodeIndex handle.  The `func` argument
    // is the snake_case function suffix, `kind_tag` is the
    // PascalCase Kind enumerator.
#define DEFINE_MAKE_AS(func, kind_tag, vec, SynType)            \
    NodeIndex SyntaxForest::make_##func(const SynType& node) \
    {                                                       \
        auto idx = static_cast<std::uint32_t>(vec.size()); \
        vec.push_back(node);                                \
        return make_node(Kind::kind_tag, idx);        \
    }

    // -- Atoms --
    DEFINE_MAKE_AS(literal, Literal, literals, LiteralSyntax)
    DEFINE_MAKE_AS(name, Name, names, NameSyntax)
    DEFINE_MAKE_AS(qualified_name, QualifiedName, qualified_names, QualifiedNameSyntax)

    // -- Module system --
    DEFINE_MAKE_AS(module, Module, modules, ModuleSyntax)
    DEFINE_MAKE_AS(namespace, Namespace, namespaces, NamespaceSyntax)
    DEFINE_MAKE_AS(import, Import, imports, ImportSyntax)
    DEFINE_MAKE_AS(import_signature, ImportSignature, import_signatures, ImportSignatureSyntax)

    // -- Definitions --
    DEFINE_MAKE_AS(constant_def, ConstantDef, constant_defs, ConstantDefSyntax)
    DEFINE_MAKE_AS(function_def, FunctionDef, function_defs, FunctionDefSyntax)
    DEFINE_MAKE_AS(macro_def, MacroDef, macro_defs, MacroDefSyntax)
    DEFINE_MAKE_AS(type_alias, TypeAlias, type_aliases, TypeAliasSyntax)
    DEFINE_MAKE_AS(signature, Signature, signatures, SignatureSyntax)

    // -- Expressions --
    DEFINE_MAKE_AS(apply, Apply, applications, ApplySyntax)
    DEFINE_MAKE_AS(infix, InfixExpr, infix_exprs, InfixExprSyntax)
    DEFINE_MAKE_AS(prefix, PrefixExpr, prefix_exprs, PrefixExprSyntax)
    DEFINE_MAKE_AS(suffix_dot, SuffixDot, suffix_dots, SuffixDotSyntax)
    DEFINE_MAKE_AS(restrict, Restrict, restricts, RestrictSyntax)
    DEFINE_MAKE_AS(coerce, Coerce, coercions, CoerceSyntax)
    DEFINE_MAKE_AS(quote, Quote, quotes, QuoteSyntax)
    DEFINE_MAKE_AS(bracket, Bracket, brackets, BracketSyntax)
    DEFINE_MAKE_AS(tuple, Tuple, tuples, TupleSyntax)
    DEFINE_MAKE_AS(sequence, Sequence, sequences, SequenceSyntax)
    DEFINE_MAKE_AS(pile, Pile, piles, PileSyntax)

    // -- Types --
    DEFINE_MAKE_AS(mapping, Mapping, mappings, MappingSyntax)
    DEFINE_MAKE_AS(forall, Forall, foralls, ForallSyntax)

    // -- Control flow --
    DEFINE_MAKE_AS(if, IfExpr, if_exprs, IfExprSyntax)
    DEFINE_MAKE_AS(return, Return, returns, ReturnSyntax)
    DEFINE_MAKE_AS(leave, Leave, leaves, LeaveSyntax)
    DEFINE_MAKE_AS(implies, Implies, implications, ImpliesSyntax)

    // -- Loops --
    DEFINE_MAKE_AS(while, While, whiles, WhileSyntax)
    DEFINE_MAKE_AS(until, Until, untils, UntilSyntax)
    DEFINE_MAKE_AS(for_in, ForIn, for_ins, ForInSyntax)
    DEFINE_MAKE_AS(such_that, SuchThat, such_thats, SuchThatSyntax)
    DEFINE_MAKE_AS(iterators, Iterators, iterators_list, IteratorsSyntax)
    DEFINE_MAKE_AS(cross, Cross, crosses, CrossSyntax)
    DEFINE_MAKE_AS(repeat, Repeat, repeats, RepeatSyntax)

    // -- Pattern matching --
    DEFINE_MAKE_AS(is, Is, is_tests, IsSyntax)
    DEFINE_MAKE_AS(isnt, Isnt, isnt_tests, IsntSyntax)
    DEFINE_MAKE_AS(equal_pattern, EqualPattern, equal_patterns, EqualPatternSyntax)
    DEFINE_MAKE_AS(colon_append, ColonAppend, colon_appends, ColonAppendSyntax)

    // -- Assignment & binding --
    DEFINE_MAKE_AS(assignment, Assignment, assignments, AssignmentSyntax)
    DEFINE_MAKE_AS(lambda, Lambda, lambdas, LambdaSyntax)
    DEFINE_MAKE_AS(default_value, DefaultValue, default_values, DefaultValueSyntax)
    DEFINE_MAKE_AS(key_arg, KeyArg, key_args, KeyArgSyntax)

    // -- Segments --
    DEFINE_MAKE_AS(bounded_segment, BoundedSegment, bounded_segments, BoundedSegmentSyntax)
    DEFINE_MAKE_AS(unbounded_segment, UnboundedSegment, unbounded_segments, UnboundedSegmentSyntax)

    // -- Reduce --
    DEFINE_MAKE_AS(reduce, Reduce, reduces, ReduceSyntax)

    // -- Structural --
    DEFINE_MAKE_AS(structure, Structure, structures, StructureSyntax)
    DEFINE_MAKE_AS(record, Record, records, RecordSyntax)
    DEFINE_MAKE_AS(accessor_def, AccessorDef, accessor_defs, AccessorDefSyntax)
    DEFINE_MAKE_AS(case, Case, cases, CaseSyntax)

    // -- Exception handling --
    DEFINE_MAKE_AS(throw, Throw, throws, ThrowSyntax)
    DEFINE_MAKE_AS(catch, Catch, catches, CatchSyntax)
    DEFINE_MAKE_AS(finally, Finally, finallys, FinallySyntax)
    DEFINE_MAKE_AS(try, Try, trys, TrySyntax)

    // -- Scoping --
    DEFINE_MAKE_AS(where, Where, wheres, WhereSyntax)
    DEFINE_MAKE_AS(dynamic, Dynamic, dynamics, DynamicSyntax)

    // -- Spad type system --
    DEFINE_MAKE_AS(with, With, withs, WithSyntax)
    DEFINE_MAKE_AS(add, Add, adds, AddSyntax)
    DEFINE_MAKE_AS(capsule, Capsule, capsules, CapsuleSyntax)
    DEFINE_MAKE_AS(pretend, Pretend, pretends, PretendSyntax)
    DEFINE_MAKE_AS(has, Has, has_tests, HasSyntax)
    DEFINE_MAKE_AS(collect, Collect, collects, CollectSyntax)
    DEFINE_MAKE_AS(join, Join, joins, JoinSyntax)

    // -- Spad declarations --
    DEFINE_MAKE_AS(free_decl, FreeDecl, free_decls, FreeDeclSyntax)
    DEFINE_MAKE_AS(export_decl, ExportDecl, export_decls, ExportDeclSyntax)

    // -- Embedded Lisp --
    DEFINE_MAKE_AS(lisp_expr, LispExpr, lisp_exprs, LispExprSyntax)

    // -- Fallback --
    DEFINE_MAKE_AS(unparsed, Unparsed, unparsed, UnparsedSyntax)

#undef DEFINE_MAKE_AS

    // -- Node descriptor infrastructure

    // -- Return the S-expression tag name for a given Kind.
    //    Returns nullptr for leaf nodes (Literal, Name).
    const char* tag_name(Kind k)
    {
        switch (k) {
        case Kind::Literal:           return nullptr;
        case Kind::Name:              return nullptr;
        case Kind::QualifiedName:     return "QualifiedName";
        case Kind::Module:            return "Module";
        case Kind::Namespace:         return "Namespace";
        case Kind::Import:            return "Import";
        case Kind::ImportSignature:   return "ImportSignature";
        case Kind::ConstantDef:       return "ConstantDefinition";
        case Kind::FunctionDef:       return "Definition";
        case Kind::MacroDef:          return "Macro";
        case Kind::TypeAlias:         return "TypeAlias";
        case Kind::Signature:         return "Signature";
        case Kind::Apply:             return "Call";
        case Kind::InfixExpr:         return "InfixExpr";
        case Kind::PrefixExpr:        return "PrefixExpr";
        case Kind::SuffixDot:         return "SuffixDot";
        case Kind::Restrict:          return "Restrict";
        case Kind::Coerce:            return "Coerce";
        case Kind::Quote:             return "Quote";
        case Kind::Bracket:           return "Bracket";
        case Kind::Tuple:             return "Tuple";
        case Kind::Sequence:          return "Sequence";
        case Kind::Pile:              return "Pile";
        case Kind::Mapping:           return "Mapping";
        case Kind::Forall:            return "Forall";
        case Kind::IfExpr:            return "If";
        case Kind::Return:            return "Return";
        case Kind::Leave:             return "Leave";
        case Kind::Implies:           return "Implies";
        case Kind::While:             return "While";
        case Kind::Until:             return "Until";
        case Kind::ForIn:             return "For";
        case Kind::SuchThat:          return "SuchThat";
        case Kind::Iterators:         return "Iterators";
        case Kind::Cross:             return "Cross";
        case Kind::Repeat:            return "Repeat";
        case Kind::Is:                return "Is";
        case Kind::Isnt:              return "Isnt";
        case Kind::EqualPattern:      return "EqualPattern";
        case Kind::ColonAppend:       return "ColonAppend";
        case Kind::Assignment:        return "Assignment";
        case Kind::Lambda:            return "Lambda";
        case Kind::DefaultValue:      return "DefaultValue";
        case Kind::KeyArg:            return "Key";
        case Kind::BoundedSegment:    return "BoundedSegment";
        case Kind::UnboundedSegment:  return "UnboundedSegment";
        case Kind::Reduce:            return "Reduce";
        case Kind::Structure:         return "Structure";
        case Kind::Record:            return "Record";
        case Kind::AccessorDef:       return "AccessorDef";
        case Kind::Case:              return "Case";
        case Kind::Throw:             return "Throw";
        case Kind::Catch:             return "Catch";
        case Kind::Finally:           return "Finally";
        case Kind::Try:               return "Try";
        case Kind::Where:             return "Where";
        case Kind::Dynamic:           return "Dynamic";
        case Kind::With:              return "With";
        case Kind::Add:               return "Add";
        case Kind::Capsule:           return "Capsule";
        case Kind::Pretend:           return "Pretend";
        case Kind::Has:               return "Has";
        case Kind::Collect:           return "Collect";
        case Kind::Join:              return "Join";
        case Kind::FreeDecl:          return "Free";
        case Kind::ExportDecl:        return "Export";
        case Kind::LispExpr:          return "Lisp";
        case Kind::Unparsed:          return "Unparsed";
        default:                            return "???";
        }
    }

    // -- Describe a node's shape and extract its field values.
    NodeDescriptor describe_node(NodeIndex node,
                                 const SyntaxForest& forest)
    {
        auto k = kind(node);
        auto idx = index(node);
        auto tag = tag_name(k);

        switch (k) {

        // -- Atoms --
        case Kind::Literal: {
            auto& n = forest.literal(idx);
            return { tag, 1, { literal_field(n.token) } };
        }
        case Kind::Name: {
            auto& n = forest.name(idx);
            return { tag, 1, { token_field(n.token) } };
        }
        case Kind::QualifiedName: {
            auto& n = forest.qualified_name(idx);
            return { tag, 2, { child_field(n.qualifier),
                               child_field(n.name) } };
        }

        // -- Module system --
        case Kind::Module: {
            auto& n = forest.module(idx);
            return { tag, 3, { child_field(n.name),
                               wrapped_span_field(n.exports),
                               wrapped_span_field(n.interface) } };
        }
        case Kind::Namespace: {
            auto& n = forest.namespace_node(idx);
            return { tag, 1, { child_field(n.name) } };
        }
        case Kind::Import: {
            auto& n = forest.import_node(idx);
            return { tag, 1, { child_field(n.what) } };
        }
        case Kind::ImportSignature: {
            auto& n = forest.import_signature(idx);
            return { tag, 3, { child_field(n.signature),
                               child_field(n.name),
                               child_field(n.provenance) } };
        }

        // -- Definitions --
        case Kind::ConstantDef: {
            auto& n = forest.constant_def(idx);
            return { tag, 2, { child_field(n.name),
                               child_field(n.body) } };
        }
        case Kind::FunctionDef: {
            auto& n = forest.function_def(idx);
            return { tag, 3, { child_field(n.name),
                               wrapped_span_field(n.parameters),
                               child_field(n.body) } };
        }
        case Kind::MacroDef: {
            auto& n = forest.macro_def(idx);
            return { tag, 3, { child_field(n.name),
                               wrapped_span_field(n.parameters),
                               child_field(n.body) } };
        }
        case Kind::TypeAlias: {
            auto& n = forest.type_alias(idx);
            return { tag, 2, { child_field(n.name),
                               child_field(n.type) } };
        }
        case Kind::Signature: {
            auto& n = forest.signature(idx);
            return { tag, 2, { child_field(n.name),
                               child_field(n.type) } };
        }

        // -- Expressions --
        case Kind::Apply: {
            auto& n = forest.apply(idx);
            return { tag, 2, { child_field(n.function),
                               span_field(n.arguments) } };
        }
        case Kind::InfixExpr: {
            auto& n = forest.infix_expr(idx);
            return { tag, 3, { token_field(n.op),
                               child_field(n.lhs),
                               child_field(n.rhs) } };
        }
        case Kind::PrefixExpr: {
            auto& n = forest.prefix_expr(idx);
            return { tag, 2, { token_field(n.op),
                               child_field(n.operand) } };
        }
        case Kind::SuffixDot: {
            auto& n = forest.suffix_dot(idx);
            return { tag, 1, { child_field(n.operand) } };
        }
        case Kind::Restrict: {
            auto& n = forest.restrict_node(idx);
            return { tag, 2, { child_field(n.expr),
                               child_field(n.type) } };
        }
        case Kind::Coerce: {
            auto& n = forest.coerce(idx);
            return { tag, 2, { child_field(n.expr),
                               child_field(n.type) } };
        }
        case Kind::Quote: {
            auto& n = forest.quote(idx);
            return { tag, 1, { child_field(n.expr) } };
        }
        case Kind::Bracket: {
            auto& n = forest.bracket(idx);
            return { tag, 1, { span_field(n.elements) } };
        }
        case Kind::Tuple: {
            auto& n = forest.tuple(idx);
            return { tag, 1, { span_field(n.elements) } };
        }
        case Kind::Sequence: {
            auto& n = forest.sequence(idx);
            return { tag, 1, { span_field(n.statements) } };
        }
        case Kind::Pile: {
            auto& n = forest.pile(idx);
            return { tag, 1, { span_field(n.items) } };
        }

        // -- Types --
        case Kind::Mapping: {
            auto& n = forest.mapping(idx);
            return { tag, 2, { wrapped_span_field(n.sources),
                               child_field(n.target) } };
        }
        case Kind::Forall: {
            auto& n = forest.forall(idx);
            return { tag, 2, { wrapped_span_field(n.variables),
                               child_field(n.body) } };
        }

        // -- Control flow --
        case Kind::IfExpr: {
            auto& n = forest.if_expr(idx);
            return { tag, 3, { child_field(n.condition),
                               child_field(n.consequent),
                               child_field(n.alternate) } };
        }
        case Kind::Return: {
            auto& n = forest.return_node(idx);
            return { tag, 1, { child_field(n.value) } };
        }
        case Kind::Leave: {
            auto& n = forest.leave(idx);
            return { tag, 1, { child_field(n.value) } };
        }
        case Kind::Implies: {
            auto& n = forest.implies(idx);
            return { tag, 2, { child_field(n.condition),
                               child_field(n.body) } };
        }

        // -- Loops --
        case Kind::While: {
            auto& n = forest.while_node(idx);
            return { tag, 1, { child_field(n.condition) } };
        }
        case Kind::Until: {
            auto& n = forest.until_node(idx);
            return { tag, 1, { child_field(n.condition) } };
        }
        case Kind::ForIn: {
            auto& n = forest.for_in(idx);
            return { tag, 3, { child_field(n.variable),
                               child_field(n.sequence),
                               child_field(n.step) } };
        }
        case Kind::SuchThat: {
            auto& n = forest.such_that(idx);
            return { tag, 1, { child_field(n.predicate) } };
        }
        case Kind::Iterators: {
            auto& n = forest.iterators(idx);
            return { tag, 1, { span_field(n.clauses) } };
        }
        case Kind::Cross: {
            auto& n = forest.cross(idx);
            return { tag, 1, { span_field(n.factors) } };
        }
        case Kind::Repeat: {
            auto& n = forest.repeat(idx);
            return { tag, 2, { child_field(n.iterators),
                               child_field(n.body) } };
        }

        // -- Pattern matching --
        case Kind::Is: {
            auto& n = forest.is_test(idx);
            return { tag, 2, { child_field(n.expr),
                               child_field(n.pattern) } };
        }
        case Kind::Isnt: {
            auto& n = forest.isnt_test(idx);
            return { tag, 2, { child_field(n.expr),
                               child_field(n.pattern) } };
        }
        case Kind::EqualPattern: {
            auto& n = forest.equal_pattern(idx);
            return { tag, 1, { child_field(n.expr) } };
        }
        case Kind::ColonAppend: {
            auto& n = forest.colon_append(idx);
            return { tag, 2, { wrapped_span_field(n.head),
                               child_field(n.rest) } };
        }

        // -- Assignment & binding --
        case Kind::Assignment: {
            auto& n = forest.assignment(idx);
            return { tag, 2, { child_field(n.target),
                               child_field(n.value) } };
        }
        case Kind::Lambda: {
            auto& n = forest.lambda(idx);
            return { tag, 2, { wrapped_span_field(n.parameters),
                               child_field(n.body) } };
        }
        case Kind::DefaultValue: {
            auto& n = forest.default_value(idx);
            return { tag, 2, { child_field(n.name),
                               child_field(n.value) } };
        }
        case Kind::KeyArg: {
            auto& n = forest.key_arg(idx);
            return { tag, 2, { child_field(n.key),
                               child_field(n.value) } };
        }

        // -- Segments --
        case Kind::BoundedSegment: {
            auto& n = forest.bounded_segment(idx);
            return { tag, 2, { child_field(n.lo),
                               child_field(n.hi) } };
        }
        case Kind::UnboundedSegment: {
            auto& n = forest.unbounded_segment(idx);
            return { tag, 1, { child_field(n.lo) } };
        }

        // -- Reduce --
        case Kind::Reduce: {
            auto& n = forest.reduce(idx);
            return { tag, 2, { child_field(n.op),
                               child_field(n.body) } };
        }

        // -- Structural --
        case Kind::Structure: {
            auto& n = forest.structure(idx);
            return { tag, 2, { child_field(n.name),
                               wrapped_span_field(n.variants) } };
        }
        case Kind::Record: {
            auto& n = forest.record(idx);
            return { tag, 2, { wrapped_span_field(n.fields),
                               wrapped_span_field(n.accessors) } };
        }
        case Kind::AccessorDef: {
            auto& n = forest.accessor_def(idx);
            return { tag, 2, { child_field(n.name),
                               child_field(n.selector) } };
        }
        case Kind::Case: {
            auto& n = forest.case_node(idx);
            return { tag, 2, { child_field(n.expr),
                               wrapped_span_field(n.branches) } };
        }

        // -- Exception handling --
        case Kind::Throw: {
            auto& n = forest.throw_node(idx);
            return { tag, 1, { child_field(n.expr) } };
        }
        case Kind::Catch: {
            auto& n = forest.catch_node(idx);
            return { tag, 2, { child_field(n.signature),
                               child_field(n.body) } };
        }
        case Kind::Finally: {
            auto& n = forest.finally_node(idx);
            return { tag, 1, { child_field(n.body) } };
        }
        case Kind::Try: {
            auto& n = forest.try_node(idx);
            return { tag, 2, { child_field(n.body),
                               wrapped_span_field(n.handlers) } };
        }

        // -- Scoping --
        case Kind::Where: {
            auto& n = forest.where(idx);
            return { tag, 2, { child_field(n.body),
                               wrapped_span_field(n.definitions) } };
        }
        case Kind::Dynamic: {
            auto& n = forest.dynamic(idx);
            return { tag, 1, { child_field(n.expr) } };
        }

        // -- Spad type system --
        case Kind::With: {
            auto& n = forest.with_node(idx);
            return { tag, 1, { child_field(n.body) } };
        }
        case Kind::Add: {
            auto& n = forest.add_node(idx);
            return { tag, 1, { child_field(n.body) } };
        }
        case Kind::Capsule: {
            auto& n = forest.capsule(idx);
            return { tag, 1, { span_field(n.items) } };
        }
        case Kind::Pretend: {
            auto& n = forest.pretend_node(idx);
            return { tag, 2, { child_field(n.expr),
                               child_field(n.type) } };
        }
        case Kind::Has: {
            auto& n = forest.has_node(idx);
            return { tag, 2, { child_field(n.expr),
                               child_field(n.type) } };
        }
        case Kind::Collect: {
            auto& n = forest.collect(idx);
            return { tag, 2, { child_field(n.body),
                               child_field(n.iterators) } };
        }
        case Kind::Join: {
            auto& n = forest.join_node(idx);
            return { tag, 1, { wrapped_span_field(n.arguments) } };
        }

        // -- Spad declarations --
        case Kind::FreeDecl: {
            auto& n = forest.free_decl(idx);
            return { tag, 1, { child_field(n.expr) } };
        }
        case Kind::ExportDecl: {
            auto& n = forest.export_decl(idx);
            return { tag, 1, { child_field(n.signature) } };
        }

        // -- Embedded Lisp --
        case Kind::LispExpr: {
            auto& n = forest.lisp_expr(idx);
            return { tag, 1, { token_field(n.token) } };
        }

        // -- Fallback --
        case Kind::Unparsed: {
            auto& n = forest.unparsed_node(idx);
            return { tag, 2, { token_field(n.first),
                               token_field(n.last) } };
        }

        default:
            return { "???", 0, { } };
        }
    }
}
