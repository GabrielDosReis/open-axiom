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
// --%   Implementation of the Lisp value arena and printer.

#include <iostream>
#include <open-axiom/LispValue>
#include <open-axiom/Charset>

namespace OpenAxiom::Lisp {

    using OpenAxiom::operator<<;

    // -- Arena implementation

    std::u8string_view Arena::intern_string(std::u8string_view s)
    {
        owned_strings.emplace_front(s);
        return std::u8string_view(owned_strings.front());
    }

    Arena::Arena()
    {
        T = make_symbol(u8"T");
        quote_sym = make_symbol(u8"QUOTE");
        nil_sym = make_symbol(u8"NIL");
    }

    Value Arena::make_symbol(std::u8string_view name)
    {
        auto it = symbols.find(name);
        if (it != symbols.end())
            return it->second;
        auto interned = intern_string(name);
        auto& v = sym_pool.emplace_front();
        v.name = interned;
        auto result = tag(&v, ValueKind::Symbol);
        symbols[interned] = result;
        return result;
    }

    Value Arena::make_integer(std::int64_t value)
    {
        auto& v = int_pool.emplace_front();
        v.value = value;
        return tag(&v, ValueKind::Integer);
    }

    Value Arena::make_float(double value)
    {
        auto& v = float_pool.emplace_front();
        v.value = value;
        return tag(&v, ValueKind::Float);
    }

    Value Arena::make_string(std::u8string_view text)
    {
        auto interned = intern_string(text);
        auto& v = str_pool.emplace_front();
        v.text = interned;
        return tag(&v, ValueKind::String);
    }

    Value Arena::cons(Value a, Value d)
    {
        auto& v = cons_pool.emplace_front();
        v.car = a;
        v.cdr = d;
        return tag(&v, ValueKind::Cons);
    }

    Value Arena::list(std::initializer_list<Value> elems)
    {
        Value result = nil;
        auto it = elems.end();
        while (it != elems.begin())
        {
            --it;
            result = cons(*it, result);
        }
        return result;
    }

    Value Arena::list(const std::vector<Value>& elems)
    {
        Value result = nil;
        for (auto it = elems.rbegin(); it != elems.rend(); ++it)
            result = cons(*it, result);
        return result;
    }

    std::uint32_t length(Value v)
    {
        std::uint32_t n = 0;
        while (consp(v))
        {
            ++n;
            v = cdr(v);
        }
        return n;
    }

    // -- Printer implementation

    void print(std::ostream& os, Value v)
    {
        if (null(v))
        {
            os << "NIL";
            return;
        }

        switch (kind(v))
        {

        case ValueKind::Symbol: {
            auto name = symbol_name(v);
            // -- Handle keyword symbols (prefixed with ':'). 
            if (name.size() > 1 and name[0] == u8':')
            {
                auto rest = name.substr(1);
                os << ':';
                if (needs_bar_quoting(rest))
                    print_bar_quoted(os, rest);
                else
                    os << rest;
            }
            else if (needs_bar_quoting(name))
                print_bar_quoted(os, name);
            else
                os << name;
            break;
        }

        case ValueKind::Integer:
            os << integer_value(v);
            break;

        case ValueKind::Float:
            os << as<FloatData>(v)->value;
            break;

        case ValueKind::String:
            os << '"' << string_text(v) << '"';
            break;

        case ValueKind::Cons: {
            os << '(';
            auto cur = v;
            bool first = true;
            while (consp(cur))
            {
                if (not first)
                    os << ' ';
                first = false;
                print(os, car(cur));
                cur = cdr(cur);
            }
            if (not null(cur))
            {
                os << " . ";
                print(os, cur);
            }
            os << ')';
            break;
        }

        }
    }

    // -- Condition evaluator for reader directives
    //
    // The lowered Boot condition AST uses tagged lists:
    //   (%Call fun args)         -- function application
    //   (%InfixExpr op lhs rhs)  -- infix operator
    //   (%PrefixExpr op operand) -- prefix operator
    //   (%QualifiedName pkg sym) -- e.g. KEYWORD::GCL -> :GCL
    //
    // The features list is a Lisp-style list of keyword symbols
    // (e.g. (:SBCL :COMMON-LISP ...)).  %hasFeature checks
    // membership via symbol name comparison.

    // -- Resolve a keyword expression to its keyword name.
    //    Handles both (%QualifiedName KEYWORD X) -> ":X"
    //    and names prefixed with "&" (Boot macro for KEYWORD::).
    static std::u8string resolve_keyword(Value v)
    {
        // -- (%QualifiedName KEYWORD X) -> ":X"
        if (is_tagged(v, u8"%QualifiedName"))
        {
            auto pkg = cadr(v);
            auto sym = caddr(v);
            if (symbolp(pkg) and symbol_name(pkg) == u8"KEYWORD"
                and symbolp(sym))
            {
                std::u8string kw = u8":";
                kw += symbol_name(sym);
                return kw;
            }
        }
        // -- A bare symbol like ":GCL" (already keyword-prefixed).
        if (symbolp(v))
        {
            auto name = symbol_name(v);
            if (not name.empty() and name[0] == u8':')
                return std::u8string(name);
        }
        return {};
    }

    // -- Check if a keyword is in the features list.
    static bool has_feature(std::u8string_view kw, Value features)
    {
        auto cur = features;
        while (consp(cur))
        {
            auto item = car(cur);
            if (symbolp(item) and symbol_name(item) == kw)
                return true;
            cur = cdr(cur);
        }
        return false;
    }

    bool eval_condition(Value v, Value features)
    {
        if (null(v))
            return false;

        // -- Non-cons atom: true (non-nil).
        if (not consp(v))
            return true;

        auto tag_v = car(v);
        if (not symbolp(tag_v))
            return true;
        auto name = symbol_name(tag_v);

        // -- (%Call |%hasFeature| (kw-expr))
        if (name == u8"%Call")
        {
            auto fun = cadr(v);
            if (symbolp(fun) and symbol_name(fun) == u8"%hasFeature")
            {
                // -- Third element is the args list.
                auto args = caddr(v);
                // -- args is the result of span_list: a proper list
                // of arguments.  For %hasFeature, the first arg
                // is the keyword expression.
                Value kw_expr = null(args) ? nil
                    : consp(args) ? car(args) : args;
                auto kw = resolve_keyword(kw_expr);
                if (kw.empty())
                    return false;
                return has_feature(kw, features);
            }
            // -- Unknown function: treat as false.
            return false;
        }

        // -- (%InfixExpr |and| lhs rhs)
        if (name == u8"%InfixExpr")
        {
            auto op = cadr(v);
            if (not symbolp(op))
                return true;
            auto opname = symbol_name(op);
            auto lhs = caddr(v);
            auto rhs = car(cddr(cdr(v)));  // fourth element
            if (opname == u8"and")
                return eval_condition(lhs, features)
                    and eval_condition(rhs, features);
            if (opname == u8"or")
                return eval_condition(lhs, features)
                    or eval_condition(rhs, features);
            return true;
        }

        // -- (%PrefixExpr |not| operand)
        if (name == u8"%PrefixExpr")
        {
            auto op = cadr(v);
            if (symbolp(op) and symbol_name(op) == u8"not")
                return not eval_condition(caddr(v), features);
            return true;
        }

        // -- Anything else: non-nil means true.
        return true;
    }
}
