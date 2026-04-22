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
// --%   Boot-to-Lisp translator implementation.

#include <iostream>

#include <open-axiom/BootTranslator>
#include <open-axiom/BootAst>

namespace OpenAxiom::Boot {

    using namespace Lisp;

    // -- Abbreviations

    static Value sym(Arena& a, std::u8string_view s) { return a.make_symbol(s); }
    static Value str(Arena& a, std::u8string_view s) { return a.make_string(s); }

    // -- TranslationUnit

    Value TranslationUnit::fresh_symbol(std::u8string_view prefix, int& counter)
    {
        ++counter;
        auto name = std::u8string(prefix);
        auto suffix = std::to_string(counter);
        name.append(suffix.begin(), suffix.end());
        return arena.make_symbol(name);
    }

    Value TranslationUnit::gensym()
    {
        return fresh_symbol(u8"bfVar#", gensym_counter);
    }

    Value TranslationUnit::let_var()
    {
        return fresh_symbol(u8"LETTMP#", let_var_counter);
    }

    Value TranslationUnit::is_var()
    {
        return fresh_symbol(u8"ISTMP#", is_var_counter);
    }

    // -- List utilities

    // -- Append two lists.
    static Value append_lists(Arena& a, Value xs, Value ys)
    {
        if (null(xs)) return ys;
        return a.cons(car(xs), append_lists(a, cdr(xs), ys));
    }

    // -- Last element of a list.
    static Value last_elem(Value lst)
    {
        if (null(lst)) return nil;
        while (consp(cdr(lst))) lst = cdr(lst);
        return car(lst);
    }

    // -- All elements except the last.
    static Value but_last(Arena& a, Value lst)
    {
        if (null(lst) or null(cdr(lst))) return nil;
        return a.cons(car(lst), but_last(a, cdr(lst)));
    }

    // -- Helper predicates matching the old translator

    // -- Check if a parameter list (raw AST args) contains any
    // string-literal ("double-quoted") parameters, which Boot
    // uses for lazy evaluation.  Such functions get a ,LAM
    // suffix in their DEFUN name.
    // We must not recurse into %Bracket (destructuring patterns)
    // or %Is (type tests) which may contain string constants.
    static bool lazy_param_p(Value p)
    {
        if (stringp(p))
            return true;
        if (not consp(p))
            return false;
        auto tag = car(p);
        if (not symbolp(tag))
        {
            // -- Plain list: check each element.
            for (Value e = p; consp(e); e = cdr(e))
                if (lazy_param_p(car(e)))
                    return true;
            return false;
        }
        auto name = symbol_name(tag);
        // -- %PrefixExpr for rest-args like (: "args")
        if (name == u8"%PrefixExpr")
            return stringp(caddr(p));
        // -- %Tuple for multi-arg like ("functor" "opname")
        if (name == u8"%Tuple")
        {
            for (Value e = cdr(p); consp(e); e = cdr(e))
                if (lazy_param_p(car(e)))
                    return true;
            return false;
        }
        // -- Do not recurse into %Bracket, %Is, etc.
        return false;
    }

    static bool lazy_params_p(Value raw_args)
    {
        for (Value e = raw_args; consp(e); e = cdr(e))
            if (lazy_param_p(car(e)))
                return true;
        return false;
    }

    static bool keywordp(Value x)
    {
        // -- Keywords in Common Lisp start with :
        return symbolp(x) and not symbol_name(x).empty()
            and symbol_name(x)[0] == ':';
    }

    static bool dollar_prefixed(Value x)
    {
        return symbolp(x) and not symbol_name(x).empty()
            and symbol_name(x)[0] == '$';
    }

    // -- Core bf* functions (expression-level translation)

    // -- Forward declarations.
    static Value translate(TranslationUnit& tu, Value ast);
    static Value bf_sequence(TranslationUnit& tu, Value lst);
    static Value bf_mkprogn(Arena& a, Value lst);
    static Value bf_lp(TranslationUnit& tu, Value iters, Value body);
    static Value bf_is1(TranslationUnit& tu, Value lhs, Value rhs);

    // -- Translate each element of a list, return a new list.
    static Value tx_list(TranslationUnit& tu, Value lst)
    {
        std::vector<Value> result;
        for (Value e = lst; consp(e); e = cdr(e))
            result.push_back(translate(tu, car(e)));
        return tu.arena.list(result);
    }

    // -- bfFlatten(op, x): if x is (op ...) return (cdr x), else (list x)
    static Value bf_flatten(Arena& a, Value op, Value x)
    {
        if (consp(x) and symbolp(car(x)) and symbolp(op)
            and symbol_name(car(x)) == symbol_name(op))
            return cdr(x);
        return a.cons(x, nil);
    }

    // -- bfFlattenSeq(l): flatten nested PROGNs
    static Value bf_flatten_seq(Arena& a, Value lst)
    {
        if (null(lst)) return nil;
        auto first = car(lst);
        auto rest = bf_flatten_seq(a, cdr(lst));
        if (consp(first) and is_symbol(car(first), u8"PROGN"))
            return append_lists(a, cdr(first), rest);
        return a.cons(first, rest);
    }

    // -- bfMKPROGN(l): flatten and wrap in PROGN if needed.
    static Value bf_mkprogn(Arena& a, Value lst)
    {
        lst = bf_flatten_seq(a, lst);
        if (null(lst)) return nil;
        if (null(cdr(lst))) return car(lst);
        return a.cons(sym(a, u8"PROGN"), lst);
    }

    // -- Unwrap PROGN: (PROGN e1 e2 ...) > (e1 e2 ...); x > (x).
    static Value unwrap_progn(Arena& a, Value x)
    {
        if (consp(x) and is_symbol(car(x), u8"PROGN"))
            return cdr(x);
        return a.cons(x, nil);
    }

    // -- bfNOT(x): (NOT x) with double-negation elimination.

    static Value bf_not(Arena& a, Value x)
    {
        if (consp(x) and is_symbol(car(x), u8"NOT") and consp(cdr(x))
            and null(cddr(x)))
            return cadr(x);
        if (consp(x) and is_symbol(car(x), u8"NULL") and consp(cdr(x))
            and null(cddr(x)))
            return cadr(x);
        return a.list({ sym(a, u8"NOT"), x });
    }

    // -- Flatten a logical connective list (AND/OR).
    static Value bf_logical(Arena& a, const char8_t* op, Value identity, Value lst)
    {
        if (null(lst)) return identity;
        if (null(cdr(lst))) return car(lst);
        auto op_sym = sym(a, op);
        std::vector<Value> elems;
        for (Value cur = lst; consp(cur); cur = cdr(cur))
        {
            Value flat = bf_flatten(a, op_sym, car(cur));
            for (Value f = flat; consp(f); f = cdr(f))
                elems.push_back(car(f));
        }
        return a.cons(op_sym, a.list(elems));
    }

    // -- bfAND(l): (AND ...) with flattening.
    static Value bf_and(Arena& a, Value lst)
    {
        return bf_logical(a, u8"AND", a.T, lst);
    }

    // -- bfOR(l): (OR ...) with flattening.
    static Value bf_or(Arena& a, Value lst)
    {
        return bf_logical(a, u8"OR", nil, lst);
    }

    // -- Shared equality test logic used by both bf_q and bf_is1.
    //    Dispatches on the type of `r` to choose EQ/EQL/EQUAL/STRING=.
    static Value equality_test(Arena& a, Value l, Value r)
    {
        if (null(r)) return a.list({ sym(a, u8"NULL"), l });
        if (symbolp(r) and symbol_name(r) == u8"T")
            return a.list({ sym(a, u8"EQ"), l, r });
        if (stringp(r))
            return bf_and(a, a.list({
                a.list({ sym(a, u8"STRINGP"), l }),
                a.list({ sym(a, u8"STRING="), l, r }) }));
        if (integerp(r))
            return a.list({ sym(a, u8"EQL"), l, r });
        if (consp(r) and is_symbol(car(r), u8"QUOTE") and consp(cdr(r)))
        {
            auto val = cadr(r);
            if (symbolp(val)) return a.list({ sym(a, u8"EQ"), l, r });
            if (stringp(val))
                return bf_and(a, a.list({
                    a.list({ sym(a, u8"STRINGP"), l }),
                    a.list({ sym(a, u8"STRING="), l, val }) }));
            return a.list({ sym(a, u8"EQUAL"), l, r });
        }
        return a.list({ sym(a, u8"EQUAL"), l, r });
    }

    // -- bfQ(l, r): equality test.
    static Value bf_q(Arena& a, Value l, Value r)
    {
        return equality_test(a, l, r);
    }

    // -- bfLessp(l, r): numeric less-than.
    static Value bf_lessp(Arena& a, Value l, Value r)
    {
        return a.list({ sym(a, u8"<"), l, r });
    }

    // -- bfMember(l, r): membership test.
    static Value bf_member(Arena& a, Value l, Value r)
    {
        return a.list({ sym(a, u8"MEMBER"), l, r });
    }

    // -- bfInfApplication(op, left, right): infix operator translation.
    static Value bf_inf_application(Arena& a, Value op, Value left, Value right)
    {
        auto name = symbolp(op) ? symbol_name(op) : std::u8string_view(u8"");
        if (name == u8"=" or name == u8"EQUAL")
            return bf_q(a, left, right);
        if (name == u8"/=" or name == u8"~=")
            return bf_not(a, bf_q(a, left, right));
        if (name == u8">")
            return bf_lessp(a, right, left);
        if (name == u8"<")
            return bf_lessp(a, left, right);
        if (name == u8"<=")
            return bf_not(a, bf_lessp(a, right, left));
        if (name == u8">=")
            return bf_not(a, bf_lessp(a, left, right));
        if (name == u8"or" or name == u8"OR")
            return bf_or(a, a.list({ left, right }));
        if (name == u8"and" or name == u8"AND")
            return bf_and(a, a.list({ left, right }));
        if (name == u8"in" or name == u8"IN")
            return bf_member(a, left, right);
        return a.list({ op, left, right });
    }

    // -- bfTupleP(x): is x a TUPLE form?
    static bool bf_tuple_p(Value x)
    {
        return consp(x) and is_symbol(car(x), u8"TUPLE");
    }

    // -- bfMakeCons(l): Build a LIST/CONS form from a list of elements.
    static Value bf_make_cons(Arena& a, Value lst)
    {
        if (null(lst)) return nil;
        if (null(cdr(lst)))
        {
            auto x = car(lst);
            // -- Check for :x (colon/rest)
            if (consp(x) and is_symbol(car(x), u8"COLON") and consp(cdr(x)))
                return cadr(x);
            return a.list({ sym(a, u8"LIST"), x });
        }
        auto x = car(lst);
        auto rest = bf_make_cons(a, cdr(lst));
        if (consp(x) and is_symbol(car(x), u8"COLON") and consp(cdr(x)))
            return a.list({ sym(a, u8"APPEND"), cadr(x), rest });
        if (consp(rest) and is_symbol(car(rest), u8"LIST"))
            return a.cons(sym(a, u8"LIST"), a.cons(x, cdr(rest)));
        return a.list({ sym(a, u8"CONS"), x, rest });
    }

    // -- bfConstruct(b): bracket/list construction.
    // bfTupleConstruct: bracket list construction.
    // If no element is a COLON form (splice), produce (LIST a b c).
    // Otherwise, use bfMakeCons for nested CONS with splice handling.
    static Value bf_tuple_construct(Arena& a, Value b)
    {
        Value elems;
        if (bf_tuple_p(b))
            elems = cdr(b);
        else
            elems = a.cons(b, nil);

        // -- Check if any element is a COLON/splice form.
        bool has_splice = false;
        for (Value e = elems; consp(e); e = cdr(e))
        {
            auto elem = car(e);
            if (consp(elem) and is_symbol(car(elem), u8"COLON"))
            {
                has_splice = true;
                break;
            }
        }

        if (has_splice)
            return bf_make_cons(a, elems);

        // -- No splices > (LIST elem1 elem2 ...)
        return a.cons(sym(a, u8"LIST"), elems);
    }

    // -- bfConstruct: used for pattern brackets.
    // Always produces nested CONS.
    static Value bf_construct(Arena& a, Value b)
    {
        Value elems;
        if (bf_tuple_p(b))
            elems = cdr(b);
        else
            elems = a.cons(b, nil);
        return bf_make_cons(a, elems);
    }

    // -- bfLetForm(lhs, rhs): simple let binding.
    static Value bf_let_form(Arena& a, Value lhs, Value rhs)
    {
        return a.list({ sym(a, u8"L%T"), lhs, rhs });
    }

    // -- bfIf(a, b, c): conditional > COND.
    static Value bf_if(Arena& a, Value cond, Value consq, Value alt)
    {
        auto b1 = unwrap_progn(a, consq);

        // -- If alternate is already a COND, merge.
        if (consp(alt) and is_symbol(car(alt), u8"COND"))
            return a.cons(sym(a, u8"COND"),
                          a.cons(a.cons(cond, b1), cdr(alt)));

        auto c1 = unwrap_progn(a, alt);

        return a.list({
            sym(a, u8"COND"),
            a.cons(cond, b1),
            a.cons(a.T, c1) });
    }

    // -- bfIfThenOnly(a, b): one-armed conditional.
    static Value bf_if_then_only(Arena& a, Value cond, Value consq)
    {
        return a.list({ sym(a, u8"COND"),
            a.cons(cond, unwrap_progn(a, consq)) });
    }

    // -- bfExit(a, b): exit/implies > COND with IDENTITY.
    static Value bf_exit(Arena& a, Value cond, Value body)
    {
        return a.list({
            sym(a, u8"COND"),
            a.list({ cond, a.list({ sym(a, u8"IDENTITY"), body }) }) });
    }

    // -- bfSuffixDot(x): (x "DOT")
    static Value bf_suffix_dot(Arena& a, Value x)
    {
        return a.list({ x, str(a, u8"DOT") });
    }

    // -- bfReturnNoName(x): (RETURN x)
    static Value bf_return(Arena& a, Value x)
    {
        return a.list({ sym(a, u8"RETURN"), x });
    }

    // -- bfLeave(x): (%Leave x) - stays until shoeCompTran1.
    static Value bf_leave(Arena& a, Value x)
    {
        return a.list({ sym(a, u8"%Leave"), x });
    }

    // -- bfEqual(x): (EQUAL x) - pattern matching equality.
    static Value bf_equal(Arena& a, Value x)
    {
        return a.list({ sym(a, u8"EQUAL"), x });
    }

    // -- bfSignature(a, b): type signature.
    static Value bf_signature(Arena& a, Value name, Value type)
    {
        if (is_symbol(type, u8"local"))
        {
            // -- compFluid: (%Dynamic name)
            return a.list({ sym(a, u8"%Dynamic"), name });
        }
        return a.list({ sym(a, u8"%Signature"), name, type });
    }

    // -- bfSegment1(lo): (SEGMENT lo NIL)
    static Value bf_segment1(Arena& a, Value lo)
    {
        return a.list({ sym(a, u8"SEGMENT"), lo, nil });
    }

    // -- bfSegment2(lo, hi): (SEGMENT lo hi)
    static Value bf_segment2(Arena& a, Value lo, Value hi)
    {
        return a.list({ sym(a, u8"SEGMENT"), lo, hi });
    }

    // -- bfRestrict(x, t): (THE t x)
    static Value bf_restrict(Arena& a, Value x, Value t)
    {
        return a.list({ sym(a, u8"THE"), t, x });
    }

    // -- bfAssign(tu, l, r): assignment translation.
    static Value bf_assign(TranslationUnit& tu, Value l, Value r)
    {
        if (bf_tuple_p(l))
        {
            // -- Tuple assignment > SETELT form.
            // Simplified: just produce SETF.
            return tu.arena.list({ sym(tu.arena, u8"SETF"), l, r });
        }
        if (consp(l) and is_symbol(car(l), u8"%Place"))
        {
            return tu.arena.list({ sym(tu.arena, u8"SETF"), cdr(l), r });
        }
        // -- Simple assignment > L%T.
        return bf_let_form(tu.arena, l, r);
    }

    // -- Iterator translation

    // -- Iterator format (internal representation matching old Boot):
    //   (vars inits sucs filters exits value)
    // Each field is a list.

    // -- bfWhile(p): iterator that checks condition.
    static Value bf_while(TranslationUnit& tu, Value p)
    {
        auto& a = tu.arena;
        // -- ((NIL NIL NIL NIL (NOT-p) NIL))
        return a.list({
            a.list({
                nil, nil, nil, nil,
                a.cons(bf_not(a, p), nil),
                nil }) });
    }

    // -- bfUntil(p): iterator that checks exit condition.
    static Value bf_until(TranslationUnit& tu, Value p)
    {
        auto& a = tu.arena;
        auto g = tu.gensym();
        return a.list({
            a.list({
                a.cons(g, nil),              // vars
                a.cons(nil, nil),             // inits
                a.cons(a.list({ sym(a, u8"SETQ"), g, p }), nil),  // sucs
                nil,                          // filters
                a.cons(g, nil),               // exits
                nil }) });                    // value
    }

    // -- bfSuchthat(p): filter iterator.
    static Value bf_suchthat(TranslationUnit& tu, Value p)
    {
        auto& a = tu.arena;
        return a.list({
            a.list({ nil, nil, nil, a.cons(p, nil), nil, nil }) });
    }

    // -- bfIN(tu, x, e): list iteration (IN).
    static Value bf_in(TranslationUnit& tu, Value x, Value e)
    {
        auto& a = tu.arena;
        auto g = tu.gensym();
        auto vars = a.cons(g, nil);
        auto inits = a.cons(e, nil);
        auto exit_cond = a.list({ sym(a, u8"NOT"),
                                  a.list({ sym(a, u8"CONSP"), g }) });
        Value suc = a.list({ sym(a, u8"SETQ"), g,
                         a.list({ sym(a, u8"CDR"), g }) });

        if (not is_symbol(x, u8"DOT"))
        {
            vars = a.list({ g, x });
            inits = a.list({ e, nil });
            exit_cond = a.list({
                sym(a, u8"OR"), exit_cond,
                a.list({ sym(a, u8"PROGN"),
                         a.list({ sym(a, u8"SETQ"), x,
                                  a.list({ sym(a, u8"CAR"), g }) }),
                         sym(a, u8"NIL") }) });
        }

        return a.list({
            a.list({
                vars, inits,
                a.cons(suc, nil),
                nil,
                a.cons(exit_cond, nil),
                nil }) });
    }

    // -- bfON(tu, x, e): tail iteration (ON).
    static Value bf_on(TranslationUnit& tu, Value x, Value e)
    {
        auto& a = tu.arena;
        if (is_symbol(x, u8"DOT"))
            x = tu.gensym();

        Value vars = nil;
        Value inits = nil;
        if (not symbolp(e) or not (symbolp(x) and symbol_name(x) == symbol_name(e)))
        {
            vars = a.cons(x, nil);
            inits = a.cons(e, nil);
        }

        auto suc = a.list({ sym(a, u8"SETQ"), x,
                            a.list({ sym(a, u8"CDR"), x }) });
        auto exit_cond = a.list({ sym(a, u8"NOT"),
                                  a.list({ sym(a, u8"CONSP"), x }) });

        return a.list({
            a.list({
                vars, inits,
                a.cons(suc, nil),
                nil,
                a.cons(exit_cond, nil),
                nil }) });
    }

    // -- bfSTEP(tu, id, fst, step, lst): numeric stepping.
    static Value bf_step(TranslationUnit& tu, Value id, Value fst, Value step, Value lst)
    {
        auto& a = tu.arena;
        if (is_symbol(id, u8"DOT"))
            id = tu.gensym();

        auto initvar = a.cons(id, nil);
        auto initval = a.cons(fst, nil);
        Value inc = step;
        Value final_val = lst;

        if (consp(step))
        {
            auto g1 = tu.gensym();
            initvar = a.cons(g1, initvar);
            initval = a.cons(step, initval);
            inc = g1;
        }

        if (consp(lst))
        {
            auto g2 = tu.gensym();
            initvar = a.cons(g2, initvar);
            initval = a.cons(lst, initval);
            final_val = g2;
        }

        Value exits = nil;
        if (not null(lst))
        {
            if (integerp(inc))
            {
                auto pred = (integer_value(inc) < 0)
                    ? sym(a, u8"<") : sym(a, u8">");
                exits = a.cons(a.list({ pred, id, final_val }), nil);
            }
            else
            {
                exits = a.cons(
                    a.list({
                        sym(a, u8"COND"),
                        a.list({ a.list({ sym(a, u8"MINUSP"), inc }),
                                 a.list({ sym(a, u8"<"), id, final_val }) }),
                        a.list({ a.T,
                                 a.list({ sym(a, u8">"), id, final_val }) }) }),
                    nil);
            }
        }

        auto suc = a.list({ sym(a, u8"SETQ"), id,
                            a.list({ sym(a, u8"+"), id, inc }) });

        return a.list({
            a.list({
                initvar, initval,
                a.cons(suc, nil),
                nil,
                exits,
                nil }) });
    }

    // -- bfFor(tu, lhs, u, step): dispatch for-in iteration.
    static Value bf_for(TranslationUnit& tu, Value lhs, Value u, Value step)
    {
        auto& a = tu.arena;
        if (consp(u) and is_symbol(car(u), u8"tails"))
            return bf_on(tu, lhs, cadr(u));
        if (consp(u) and is_symbol(car(u), u8"SEGMENT"))
            return bf_step(tu, lhs, cadr(u), step, caddr(u));
        // -- Default: IN iteration.
        // For structured lhs (destructuring), use bfForTree.
        if (symbolp(lhs))
            return bf_in(tu, lhs, u);
        // -- bfForTree: structured LHS.
        auto g = tu.gensym();
        auto in_iter = bf_in(tu, g, u);
        auto suchthat = bf_suchthat(tu,
            bf_is1(tu, g, lhs));
        return append_lists(a, in_iter, suchthat);
    }

    // -- bfIterators(l): wrap iterator list.
    static Value bf_iterators(Arena& a, Value lst)
    {
        return a.cons(sym(a, u8"ITERATORS"), lst);
    }

    // -- bfCross(l): wrap cross-product iterator list.
    static Value bf_cross(Arena& a, Value lst)
    {
        return a.cons(sym(a, u8"CROSS"), lst);
    }

    // -- bfSep(iterators): separate iterator fields.
    //    Each iterator is ((vars inits sucs filters exits value)).
    //    Returns (all-vars all-inits all-sucs all-filters all-exits all-values).
    static Value bf_sep(Arena& a, Value iters)
    {
        std::vector<Value> all_vars, all_inits, all_sucs, all_filters,
                       all_exits, all_values;

        for (Value it = iters; consp(it); it = cdr(it))
        {
            auto iter = car(it);
            if (not consp(iter)) continue;
            auto entry = car(iter);
            if (not consp(entry)) continue;

            // -- entry = (vars inits sucs filters exits value)
            auto vars = car(entry);
            auto inits = (consp(cdr(entry))) ? cadr(entry) : nil;
            auto sucs = (consp(cddr(entry))) ? caddr(entry) : nil;
            auto filters_v = nil;
            auto exits_v = nil;
            auto value_v = nil;
            if (consp(cddr(entry)))
            {
                auto rest3 = cddr(entry);
                if (consp(cdr(rest3))) filters_v = cadr(rest3);
                if (consp(cddr(rest3))) exits_v = car(cddr(rest3));
                if (consp(cdr(cddr(rest3)))) value_v = cadr(cddr(rest3));
            }

            for (Value v = vars; consp(v); v = cdr(v))
                all_vars.push_back(car(v));
            for (Value v = inits; consp(v); v = cdr(v))
                all_inits.push_back(car(v));
            for (Value v = sucs; consp(v); v = cdr(v))
                all_sucs.push_back(car(v));
            for (Value v = filters_v; consp(v); v = cdr(v))
                all_filters.push_back(car(v));
            for (Value v = exits_v; consp(v); v = cdr(v))
                all_exits.push_back(car(v));
            for (Value v = value_v; consp(v); v = cdr(v))
                all_values.push_back(car(v));
        }

        return a.list({
            a.list(all_vars),
            a.list(all_inits),
            a.list(all_sucs),
            a.list(all_filters),
            a.list(all_exits),
            a.list(all_values) });
    }

    // -- bfLp1(tu, iters, body): build a LOOP form.
    static Value bf_lp1(TranslationUnit& tu, Value iters, Value body)
    {
        auto& a = tu.arena;
        auto sep = bf_sep(a, iters);
        auto vars = car(sep);
        auto inits = cadr(sep);
        auto sucs = caddr(sep);
        auto rest3 = cddr(sep);
        auto filters = (consp(cdr(rest3))) ? cadr(rest3) : nil;
        auto exits = (consp(cddr(rest3))) ? car(cddr(rest3)) : nil;
        auto values = (consp(cdr(cddr(rest3)))) ? cadr(cddr(rest3)) : nil;

        // -- Apply filters to body.
        Value nbody = body;
        if (not null(filters))
        {
            auto combined = append_lists(a, filters, a.cons(body, nil));
            nbody = bf_and(a, combined);
        }

        // -- Value defaults to NIL.
        Value value = null(values) ? nil : car(values);

        // -- Apply exit conditions.
        Value loop_body;
        if (null(exits))
            loop_body = nbody;
        else
            loop_body = bf_if(a,
                bf_or(a, exits),
                a.list({ sym(a, u8"RETURN"), value }),
                nbody);

        // -- Build LOOP form.
        Value loop_form = a.cons(sym(a, u8"LOOP"),
                             a.cons(loop_body, sucs));

        // -- Wrap in LET if there are variables.
        if (not null(vars))
        {
            // -- Build bindings: ((v1 i1) (v2 i2) ...)
            std::vector<Value> bindings;
            Value vp = vars, ip = inits;
            while (consp(vp) and consp(ip))
            {
                bindings.push_back(a.list({ car(vp), car(ip) }));
                vp = cdr(vp);
                ip = cdr(ip);
            }
            loop_form = a.list({
                sym(a, u8"LET"),
                a.list(bindings),
                loop_form });
        }

        return loop_form;
    }

    // -- bfLp(tu, iters, body): dispatch on iterator type.
    static Value bf_lp(TranslationUnit& tu, Value iters, Value body)
    {
        if (consp(iters) and is_symbol(car(iters), u8"ITERATORS"))
            return bf_lp1(tu, cdr(iters), body);
        // -- CROSS: handle crossed iterators.
        // For now, handle as ITERATORS.
        if (consp(iters) and is_symbol(car(iters), u8"CROSS"))
        {
            // -- Cross-product iteration: nested loops.
            // Simplified: just use first factor.
            auto factors = cdr(iters);
            if (null(factors)) return body;
            if (null(cdr(factors)))
            {
                auto factor = car(factors);
                if (consp(factor) and is_symbol(car(factor), u8"ITERATORS"))
                    return bf_lp1(tu, cdr(factor), body);
            }
            // -- Multiple factors: nest them.
            Value result = body;
            for (Value f = factors; consp(f); f = cdr(f))
            {
                auto factor = car(f);
                if (consp(factor) and is_symbol(car(factor), u8"ITERATORS"))
                    result = bf_lp1(tu, cdr(factor), result);
            }
            return result;
        }
        return body;
    }

    // -- bfMakeCollectInsn: build the collect loop body.
    static Value bf_make_collect_insn(Arena& a, Value expr, Value prev, Value head, Value adv)
    {
        auto first_time = bf_mkprogn(a, a.list({
            a.list({ sym(a, u8"SETQ"), head, expr }),
            a.list({ sym(a, u8"SETQ"), prev,
                     is_symbol(adv, u8"CDR") ? head
                         : a.list({ adv, head }) }) }));
        auto other_time = bf_mkprogn(a, a.list({
            a.list({ sym(a, u8"RPLACD"), prev, expr }),
            a.list({ sym(a, u8"SETQ"), prev,
                     a.list({ adv, prev }) }) }));
        return bf_if(a, a.list({ sym(a, u8"NULL"), head }),
                     first_time, other_time);
    }

    // -- bfDoCollect: build a collect loop.
    static Value bf_do_collect(TranslationUnit& tu, Value expr, Value itl,
                           Value adv, Value skip_nil)
    {
        auto& a = tu.arena;
        auto head = tu.gensym();
        auto prev = tu.gensym();

        Value body;
        if (not null(skip_nil))
        {
            auto x = tu.gensym();
            body = a.list({
                sym(a, u8"LET"),
                a.list({ a.list({ x, expr }) }),
                bf_if(a, a.list({ sym(a, u8"NULL"), x }), nil,
                      bf_make_collect_insn(a, x, prev, head, adv)) });
        }
        else
        {
            body = bf_make_collect_insn(a, expr, prev, head, adv);
        }

        auto extrait = a.list({
            a.list({
                a.list({ head, prev }),
                a.list({ nil, nil }),
                nil, nil, nil,
                a.cons(head, nil) }) });

        // -- bfLp2
        Value iters;
        if (consp(itl) and is_symbol(car(itl), u8"ITERATORS"))
            iters = bf_iterators(a,
                a.cons(extrait, cdr(itl)));
        else if (consp(itl) and is_symbol(car(itl), u8"CROSS"))
        {
            auto factors = cdr(itl);
            iters = bf_cross(a,
                a.cons(bf_iterators(a, a.cons(extrait, 
                    consp(car(factors)) ? cdr(car(factors)) : nil)),
                    cdr(factors)));
        }
        else
            iters = bf_iterators(a, a.cons(extrait, nil));

        return bf_lp(tu, iters, body);
    }

    // -- bfCollect: translate a collect form.
    static Value bf_collect(TranslationUnit& tu, Value y, Value itl)
    {
        auto& a = tu.arena;
        // -- Check for :x (append collect).
        if (consp(y) and is_symbol(car(y), u8"COLON") and consp(cdr(y)))
        {
            auto body = cadr(y);
            if ((consp(body) and is_symbol(car(body), u8"CONS"))
                or (consp(body) and is_symbol(car(body), u8"LIST")))
            {
                return bf_do_collect(tu, body, itl,
                                     sym(a, u8"lastNode"),
                                     sym(a, u8"skipNil"));
            }
            return bf_do_collect(tu,
                a.list({ sym(a, u8"copyList"), body }),
                itl, sym(a, u8"lastNode"), sym(a, u8"skipNil"));
        }
        if (consp(y) and is_symbol(car(y), u8"TUPLE"))
        {
            return bf_do_collect(tu, bf_construct(a, y), itl,
                                 sym(a, u8"lastNode"),
                                 sym(a, u8"skipNil"));
        }
        // -- Default: simple collect.
        return bf_do_collect(tu,
            a.list({ sym(a, u8"CONS"), y, nil }),
            itl, sym(a, u8"CDR"), nil);
    }

    // -- bfReduce: translate a reduce form.
    static Value bf_reduce(TranslationUnit& tu, Value op, Value y)
    {
        auto& a = tu.arena;
        // -- Extract the actual operator name.
        Value actual_op = op;
        if (consp(op) and is_symbol(car(op), u8"QUOTE"))
            actual_op = cadr(op);

        auto g = tu.gensym();
        auto g1 = tu.gensym();
        auto body = a.list({ sym(a, u8"SETQ"), g,
                             a.list({ actual_op, g, g1 }) });

        // -- Simple case: no initial value, iterate with first element.
        auto g2 = tu.gensym();
        auto init = a.list({ sym(a, u8"CAR"), g2 });
        auto ny = a.list({ sym(a, u8"CDR"), g2 });
        auto iter_g = a.list({
            a.list({
                a.cons(g, nil),
                a.cons(init, nil),
                nil, nil, nil,
                a.cons(g, nil) }) });
        auto iter_g1 = bf_in(tu, g1, ny);
        auto iters = bf_iterators(a,
            a.list({ iter_g, iter_g1 }));
        return bf_mkprogn(a, a.list({
            a.list({ sym(a, u8"L%T"), g2, y }),
            bf_lp(tu, iters, body) }));
    }

    // -- Pattern matching (bfIS / bfIS1)

    // -- Convert AST pattern to CONS-cell representation suitable
    // for bfIS1. A bracket pattern ['%Key, k, v] becomes
    // (CONS (QUOTE |%Key|) (CONS |k| (CONS |v| NIL))).
    static Value tx_pattern(TranslationUnit& tu, Value ast)
    {
        auto& a = tu.arena;

        if (null(ast)) return nil;
        if (not consp(ast))
        {
            // -- Symbols and literals pass through.
            return ast;
        }

        auto tag_v = car(ast);
        if (not symbolp(tag_v))
            return ast;
        auto tag = symbol_name(tag_v);

        // -- %Quote pattern: ('x) > (QUOTE x).
        if (tag == u8"%Quote")
            return a.list({ sym(a, u8"QUOTE"), cadr(ast) });

        // -- %EqualPattern: (= x) > (EQUAL x).
        if (tag == u8"%EqualPattern")
        {
            auto expr = translate(tu, cadr(ast));
            return a.list({ sym(a, u8"EQUAL"), expr });
        }

        // -- %Bracket: [a, b, c] > (CONS a' (CONS b' (CONS c' NIL)))
        // If single element is a Tuple, expand it directly.
        if (tag == u8"%Bracket")
        {
            auto elems_list = cadr(ast);
            // -- Single-element Tuple > expand directly.
            if (consp(elems_list) and null(cdr(elems_list)))
            {
                auto elem = car(elems_list);
                if (consp(elem) and symbolp(car(elem))
                    and symbol_name(car(elem)) == u8"%Tuple")
                {
                    return tx_pattern(tu, elem);
                }
                // -- Single non-tuple element.
                return tx_pattern(tu, elem);
            }
            // -- Multiple elements > CONS chain.
            Value result = nil;
            std::vector<Value> elems;
            for (Value e = elems_list; consp(e); e = cdr(e))
                elems.push_back(tx_pattern(tu, car(e)));
            for (int i = static_cast<int>(elems.size()) - 1;
                 i >= 0; --i)
            {
                result = a.list({ sym(a, u8"CONS"),
                                  elems[i], result });
            }
            return result;
        }

        // -- %Tuple: (a, b, c) > (CONS a' (CONS b' (CONS c' NIL)))
        if (tag == u8"%Tuple")
        {
            auto elems_list = cadr(ast);
            Value result = nil;
            std::vector<Value> elems;
            for (Value e = elems_list; consp(e); e = cdr(e))
                elems.push_back(tx_pattern(tu, car(e)));
            for (int i = static_cast<int>(elems.size()) - 1;
                 i >= 0; --i)
            {
                result = a.list({ sym(a, u8"CONS"),
                                  elems[i], result });
            }
            return result;
        }

        // -- %ColonAppend: [:a, rest] in pattern > DOT
        if (tag == u8"%ColonAppend")
        {
            auto head_list = cadr(ast);
            auto rest_ast = caddr(ast);
            // -- Translate head elements and rest.
            std::vector<Value> heads;
            for (Value e = head_list; consp(e); e = cdr(e))
                heads.push_back(tx_pattern(tu, car(e)));
            Value tail = null(rest_ast)
                ? sym(a, u8"DOT")
                : tx_pattern(tu, rest_ast);
            Value result = tail;
            for (int i = static_cast<int>(heads.size()) - 1;
                 i >= 0; --i)
            {
                result = a.list({ sym(a, u8"CONS"),
                                  heads[i], result });
            }
            return result;
        }

        // -- %Call in pattern context: translate the expression.
        if (tag == u8"%Call")
        {
            auto fun = translate(tu, cadr(ast));
            auto args_list = caddr(ast);
            std::vector<Value> targs;
            for (Value e = args_list; consp(e); e = cdr(e))
                targs.push_back(translate(tu, car(e)));
            if (targs.empty())
                return fun;
            return a.cons(fun, a.list(targs));
        }

        // -- Default: translate as expression.
        return translate(tu, ast);
    }

    // -- bfIS(tu, lhs, rhs): pattern match with variable counter save.
    static Value bf_is(TranslationUnit& tu, Value lhs, Value rhs)
    {
        auto saved_isno = tu.is_var_counter;
        tu.is_var_counter = 0;
        auto result = bf_is1(tu, lhs, rhs);
        tu.is_var_counter = saved_isno;
        return result;
    }

    // -- bfIS1: the core pattern matching logic.
    static Value bf_is1(TranslationUnit& tu, Value lhs, Value rhs)
    {
        auto& a = tu.arena;

        // -- Atoms handled by shared equality_test.
        if (null(rhs) or (symbolp(rhs) and symbol_name(rhs) == u8"T")
            or stringp(rhs) or integerp(rhs))
            return equality_test(a, lhs, rhs);

        // -- Keyword pattern.
        if (keywordp(rhs))
            return a.list({ sym(a, u8"EQ"), lhs, rhs });

        // -- Simple variable binding.
        if (not consp(rhs))
        {
            return a.list({ sym(a, u8"PROGN"),
                            bf_let_form(a, rhs, lhs), a.T });
        }

        // -- QUOTE pattern - use shared equality_test.
        if (is_symbol(car(rhs), u8"QUOTE"))
            return equality_test(a, lhs, rhs);

        // -- L%T pattern (let in pattern).
        if (is_symbol(car(rhs), u8"L%T"))
        {
            auto c = cadr(rhs);
            auto d = caddr(rhs);
            return bf_and(a, a.list({
                bf_is1(tu, lhs, d),
                bf_mkprogn(a, a.list({
                    bf_let_form(a, c, lhs), a.T })) }));
        }

        // -- EQUAL pattern.
        if (is_symbol(car(rhs), u8"EQUAL") and consp(cdr(rhs))
            and null(cddr(rhs)))
        {
            return bf_q(a, lhs, cadr(rhs));
        }

        // -- CONS pattern (destructuring).
        if (is_symbol(car(rhs), u8"CONS") and consp(cdr(rhs)))
        {
            auto head = cadr(rhs);
            auto tail = caddr(rhs);

            // -- (CONS DOT DOT) > just CONSP.
            if (is_symbol(head, u8"DOT") and is_symbol(tail, u8"DOT"))
                return a.list({ sym(a, u8"CONSP"), lhs });

            // -- Complex LHS > bind to temp var.
            if (consp(lhs))
            {
                auto g = tu.is_var();
                return bf_mkprogn(a, a.list({
                    bf_let_form(a, g, lhs),
                    bf_is1(tu, g, rhs) }));
            }

            // -- (CONS DOT tail)
            if (is_symbol(head, u8"DOT"))
            {
                if (null(tail))
                    return bf_and(a, a.list({
                        a.list({ sym(a, u8"CONSP"), lhs }),
                        a.list({ sym(a, u8"NULL"),
                                 a.list({ sym(a, u8"CDR"), lhs }) }) }));
                if (is_symbol(tail, u8"DOT"))
                    return a.list({ sym(a, u8"CONSP"), lhs });
                return bf_and(a, a.list({
                    a.list({ sym(a, u8"CONSP"), lhs }),
                    bf_is1(tu, a.list({ sym(a, u8"CDR"), lhs }), tail) }));
            }

            // -- (CONS head NIL)
            if (null(tail))
                return bf_and(a, a.list({
                    a.list({ sym(a, u8"CONSP"), lhs }),
                    a.list({ sym(a, u8"NULL"),
                             a.list({ sym(a, u8"CDR"), lhs }) }),
                    bf_is1(tu, a.list({ sym(a, u8"CAR"), lhs }), head) }));

            // -- (CONS head DOT) > just car matters.
            if (is_symbol(tail, u8"DOT"))
                return bf_and(a, a.list({
                    a.list({ sym(a, u8"CONSP"), lhs }),
                    bf_is1(tu, a.list({ sym(a, u8"CAR"), lhs }), head) }));

            // -- General case: match both car and cdr.
            return bf_and(a, a.list({
                a.list({ sym(a, u8"CONSP"), lhs }),
                bf_is1(tu, a.list({ sym(a, u8"CAR"), lhs }), head),
                bf_is1(tu, a.list({ sym(a, u8"CDR"), lhs }), tail) }));
        }

        // -- Default: error/unhandled.
        return a.list({ sym(a, u8"EQUAL"), lhs, rhs });
    }

    // -- bfLET / bfLET1: let-binding with destructuring

    // -- bfCONTAINED: check if symbol x appears in tree y.
    static bool bf_contained(Value x, Value y)
    {
        if (null(y)) return false;
        if (symbolp(y))
            return symbolp(x) and symbol_name(x) == symbol_name(y);
        if (not consp(y)) return false;
        return bf_contained(x, car(y)) or bf_contained(x, cdr(y));
    }

    // -- bfLET2: destructuring let for cons patterns.
    static Value bf_let2(TranslationUnit& tu, Value lhs, Value rhs)
    {
        auto& a = tu.arena;
        if (not consp(lhs)) return bf_let_form(a, lhs, rhs);

        if (is_symbol(car(lhs), u8"CONS") and consp(cdr(lhs)))
        {
            auto h = cadr(lhs);
            auto t = caddr(lhs);
            Value r1, r2;
            if (null(t))
            {
                r1 = bf_let2(tu, h, a.list({ sym(a, u8"CAR"), rhs }));
                return r1;
            }
            if (null(h) or is_symbol(h, u8"DOT"))
            {
                r2 = bf_let2(tu, t, a.list({ sym(a, u8"CDR"), rhs }));
                return r2;
            }
            r1 = bf_let2(tu, h, a.list({ sym(a, u8"CAR"), rhs }));
            r2 = bf_let2(tu, t, a.list({ sym(a, u8"CDR"), rhs }));
            if (consp(r2) and is_symbol(car(r2), u8"PROGN"))
                return bf_mkprogn(a, a.cons(r1, cdr(r2)));
            return bf_mkprogn(a, a.list({ r1, r2 }));
        }
        return bf_let_form(a, lhs, rhs);
    }

    // -- bfLET1: let-binding with destructuring.
    static Value bf_let1(TranslationUnit& tu, Value lhs, Value rhs)
    {
        auto& a = tu.arena;
        // -- Simple symbol.
        if (symbolp(lhs)) return bf_let_form(a, lhs, rhs);

        // -- %Dynamic or %Signature.
        if (consp(lhs) and (is_symbol(car(lhs), u8"%Dynamic")
            or is_symbol(car(lhs), u8"%Signature")))
            return bf_let_form(a, lhs, rhs);

        // -- Simple rhs (symbol) not contained in lhs.
        if (symbolp(rhs) and not bf_contained(rhs, lhs))
            return bf_let2(tu, lhs, rhs);

        // -- General case: bind rhs to a temp var.
        auto g = tu.let_var();
        auto r = bf_let_form(a, g, rhs);
        auto l = bf_let1(tu, lhs, g);
        if (consp(l) and is_symbol(car(l), u8"PROGN"))
            return bf_mkprogn(a, a.cons(r, cdr(l)));
        return bf_mkprogn(a, a.list({ r, l, g }));
    }

    // -- bfLET: let-binding with counter save/restore.
    static Value bf_let(TranslationUnit& tu, Value lhs, Value rhs)
    {
        auto saved = tu.let_var_counter;
        tu.let_var_counter = 0;
        auto result = bf_let1(tu, lhs, rhs);
        tu.let_var_counter = saved;
        return result;
    }

    // -- Main expression translator

    // -- translate: translate an expression-level AST node.
    static Value translate(TranslationUnit& tu, Value ast)
    {
        auto& a = tu.arena;

        // -- NIL stays NIL.
        if (null(ast)) return nil;

        // -- Atoms pass through.
        if (atomp(ast))
        {
            // -- Map Boot false/true/nil to Lisp.
            if (symbolp(ast))
            {
                auto name = symbol_name(ast);
                if (name == u8"false") return nil;
                if (name == u8"true") return a.T;
                if (name == u8"nil") return nil;
            }
            return ast;
        }

        // -- Tagged AST node: dispatch on the tag.
        if (not consp(ast)) return ast;

        auto tag_v = car(ast);
        if (not symbolp(tag_v)) return ast;
        auto tag = symbol_name(tag_v);

        // -- Function call.
        if (tag == u8"%Call")
        {
            auto fun = translate(tu, cadr(ast));
            auto args_list = caddr(ast);
            // -- Translate each argument.
            std::vector<Value> targs;
            for (Value e = args_list; consp(e); e = cdr(e))
                targs.push_back(translate(tu, car(e)));
            auto targ_list = a.list(targs);

            // -- If there's a single %Tuple arg, expand it.
            if (targs.size() == 1 and bf_tuple_p(targs[0]))
                return tu.arena.cons(fun, cdr(targs[0]));
            return tu.arena.cons(fun, targ_list);
        }

        // -- Infix expression.
        if (tag == u8"%InfixExpr")
        {
            auto op = translate(tu, cadr(ast));
            auto lhs = translate(tu, caddr(ast));
            auto rhs = translate(tu, car(cddr(cdr(ast))));
            return bf_inf_application(a, op, lhs, rhs);
        }

        // -- Prefix expression.
        if (tag == u8"%PrefixExpr")
        {
            auto op = translate(tu, cadr(ast));
            auto operand = translate(tu, caddr(ast));
            // -- Special case: 'not' > NOT.
            if (is_symbol(op, u8"not") or is_symbol(op, u8"NOT"))
                return bf_not(a, operand);
            return a.list({ op, operand });
        }

        // -- Quote.
        if (tag == u8"%Quote")
        {
            auto expr = cadr(ast);
            // -- Don't translate the quoted expression.
            return a.list({ sym(a, u8"QUOTE"), expr });
        }

        // -- Bracket (list construction).
        if (tag == u8"%Bracket")
        {
            auto elems_list = cadr(ast);
            // -- If bracket has single element that is a Tuple,
            // extract tuple elements and check for splices.
            if (consp(elems_list) and null(cdr(elems_list)))
            {
                auto elem = car(elems_list);
                if (consp(elem) and symbolp(car(elem))
                    and symbol_name(car(elem)) == u8"%Tuple")
                {
                    auto tuple_elems = cadr(elem);
                    std::vector<Value> telems;
                    for (Value e = tuple_elems; consp(e); e = cdr(e))
                    {
                        auto te = car(e);
                        if (consp(te) and symbolp(car(te))
                            and symbol_name(car(te)) == u8"%PrefixExpr"
                            and consp(cdr(te))
                            and symbolp(cadr(te))
                            and symbol_name(cadr(te)) == u8":")
                        {
                            auto operand = translate(tu, caddr(te));
                            telems.push_back(
                                a.list({ sym(a, u8"COLON"),
                                         operand }));
                        }
                        else
                        {
                            telems.push_back(translate(tu, te));
                        }
                    }
                    Value arg;
                    if (telems.size() == 1)
                        arg = telems[0];
                    else
                        arg = a.cons(sym(a, u8"TUPLE"),
                                     a.list(telems));
                    return bf_tuple_construct(a, arg);
                }
            }
            // -- Non-tuple bracket: translate elements directly.
            std::vector<Value> telems;
            for (Value e = elems_list; consp(e); e = cdr(e))
            {
                auto elem = car(e);
                if (consp(elem) and symbolp(car(elem))
                    and symbol_name(car(elem)) == u8"%PrefixExpr"
                    and consp(cdr(elem)) and symbolp(cadr(elem))
                    and symbol_name(cadr(elem)) == u8":")
                {
                    auto operand = translate(tu, caddr(elem));
                    telems.push_back(
                        a.list({ sym(a, u8"COLON"), operand }));
                }
                else
                {
                    telems.push_back(translate(tu, elem));
                }
            }
            Value arg;
            if (telems.size() == 1)
                arg = telems[0];
            else
                arg = a.cons(sym(a, u8"TUPLE"),
                             a.list(telems));
            return bf_tuple_construct(a, arg);
        }

        // -- Tuple.
        if (tag == u8"%Tuple")
        {
            return a.cons(sym(a, u8"TUPLE"),
                          tx_list(tu, cadr(ast)));
        }

        // -- Pile (sequence of statements).
        if (tag == u8"%Pile")
        {
            return bf_sequence(tu, tx_list(tu, cadr(ast)));
        }

        // -- Conditional.
        if (tag == u8"IF")
        {
            auto cond_v = translate(tu, cadr(ast));
            auto consq = translate(tu, caddr(ast));
            auto alt_v = translate(tu, car(cddr(cdr(ast))));
            return bf_if(a, cond_v, consq, alt_v);
        }

        // -- Implies (one-armed conditional / exit).
        if (tag == u8"%Implies")
        {
            auto cond_v = translate(tu, cadr(ast));
            auto body_v = translate(tu, caddr(ast));
            return bf_exit(a, cond_v, body_v);
        }

        // -- Assignment (expression-level).
        if (tag == u8"%Assignment")
        {
            auto target = translate(tu, cadr(ast));
            auto value = translate(tu, caddr(ast));
            return bf_assign(tu, target, value);
        }

        // -- Is (pattern matching).
        if (tag == u8"%Is")
        {
            auto lhs_v = translate(tu, cadr(ast));
            auto rhs_v = tx_pattern(tu, caddr(ast));
            return bf_is(tu, lhs_v, rhs_v);
        }

        // -- Isnt (negated pattern matching).
        if (tag == u8"%Isnt")
        {
            auto lhs_v = translate(tu, cadr(ast));
            auto rhs_v = tx_pattern(tu, caddr(ast));
            return bf_not(a, bf_is(tu, lhs_v, rhs_v));
        }

        // -- EqualPattern.
        if (tag == u8"%EqualPattern")
        {
            auto expr = translate(tu, cadr(ast));
            return bf_equal(a, expr);
        }

        // -- ColonAppend.
        if (tag == u8"%ColonAppend")
        {
            auto head_list = cadr(ast);
            auto rest = translate(tu, caddr(ast));
            std::vector<Value> thead;
            for (Value e = head_list; consp(e); e = cdr(e))
                thead.push_back(translate(tu, car(e)));
            // -- Build the &REST form.
            Value result = a.list({ sym(a, u8"&REST"), rest });
            for (auto it = thead.rbegin(); it != thead.rend(); ++it)
                result = a.cons(*it, result);
            return result;
        }

        // -- Signature.
        if (tag == u8"%Signature")
        {
            auto name = translate(tu, cadr(ast));
            auto type = translate(tu, caddr(ast));
            return bf_signature(a, name, type);
        }

        // -- SuffixDot.
        if (tag == u8"%SuffixDot")
        {
            auto operand = translate(tu, cadr(ast));
            return bf_suffix_dot(a, operand);
        }

        // -- Restrict.
        if (tag == u8"%Restrict")
        {
            auto expr = translate(tu, cadr(ast));
            auto type = translate(tu, caddr(ast));
            return bf_restrict(a, expr, type);
        }

        // -- Return.
        if (tag == u8"%Return")
        {
            auto value = translate(tu, cadr(ast));
            return bf_return(a, value);
        }

        // -- Leave.
        if (tag == u8"%Leave")
        {
            auto value = translate(tu, cadr(ast));
            return bf_leave(a, value);
        }

        // -- Lambda.
        if (tag == u8"%Lambda")
        {
            auto body_v = translate(tu, caddr(ast));
            return a.list({
                sym(a, u8"LAMBDA"),
                tx_list(tu, cadr(ast)),
                body_v });
        }

        // -- Mapping (type: sources -> target).
        if (tag == u8"%Mapping")
        {
            auto target = translate(tu, cadr(ast));
            return a.list({
                sym(a, u8"%Mapping"),
                target,
                tx_list(tu, caddr(ast)) });
        }

        // -- Forall (quantified type).
        if (tag == u8"%Forall")
        {
            auto vars = cadr(ast);
            auto body_v = translate(tu, caddr(ast));
            return a.list({ sym(a, u8"%Forall"), vars, body_v });
        }

        // -- Dynamic.
        if (tag == u8"%Dynamic")
        {
            auto expr = translate(tu, cadr(ast));
            return a.list({ sym(a, u8"%Dynamic"), expr });
        }

        // -- Segments.
        if (tag == u8"%BoundedSegment")
        {
            auto lo = translate(tu, cadr(ast));
            auto hi = translate(tu, caddr(ast));
            return bf_segment2(a, lo, hi);
        }
        if (tag == u8"%UnboundedSegment")
        {
            auto lo = translate(tu, cadr(ast));
            return bf_segment1(a, lo);
        }

        // -- Iterators.
        if (tag == u8"%While")
        {
            auto cond_v = translate(tu, cadr(ast));
            return bf_while(tu, cond_v);
        }
        if (tag == u8"%Until")
        {
            auto cond_v = translate(tu, cadr(ast));
            return bf_until(tu, cond_v);
        }
        if (tag == u8"%SuchThat")
        {
            auto pred = translate(tu, cadr(ast));
            return bf_suchthat(tu, pred);
        }
        if (tag == u8"%For")
        {
            auto var = translate(tu, cadr(ast));
            auto seq = translate(tu, caddr(ast));
            auto step_v = translate(tu, car(cddr(cdr(ast))));
            return bf_for(tu, var, seq, step_v);
        }
        if (tag == u8"%Iterators")
        {
            return bf_iterators(a, tx_list(tu, cadr(ast)));
        }
        if (tag == u8"%Cross")
        {
            return bf_cross(a, tx_list(tu, cadr(ast)));
        }

        // -- Repeat (loop).
        if (tag == u8"%Repeat")
        {
            auto iters = translate(tu, cadr(ast));
            auto body_v = translate(tu, caddr(ast));
            return bf_lp(tu, iters, body_v);
        }

        // -- Reduce.
        if (tag == u8"%Reduce")
        {
            auto op = translate(tu, cadr(ast));
            auto body_v = translate(tu, caddr(ast));
            // -- Check if body is a COLLECT form.
            return bf_reduce(tu, op, body_v);
        }

        // -- Where.
        if (tag == u8"%Where")
        {
            auto body_v = translate(tu, cadr(ast));
            auto defs_raw = caddr(ast);
            // -- defSheepAndGoats: separate definitions from
            // non-definitions in the where-clause.
            // - Definitions with args > rename to
            //   enclosingFunction,localName, add as side
            //   condition
            // - Definitions without args > substitute
            // - Non-definitions > include inline
            struct DefInfo
            {
                Value original_name;
                Value renamed;      // nil if no-arg (substitution)
                Value body;         // translated body
                Value args;
            };
            std::vector<std::pair<Value, Value>> opassoc;
            std::vector<DefInfo> def_stack;
            std::vector<Value> nondefs;

            // -- Unwrap Pile if present.
            // defs_raw may be (%Pile items) or ((%Pile items)).
            Value defs_list = defs_raw;
            if (consp(defs_raw) and consp(car(defs_raw))
                and is_symbol(caar(defs_raw), u8"%Pile"))
                defs_list = cadr(car(defs_raw));
            else if (consp(defs_raw) and symbolp(car(defs_raw))
                and symbol_name(car(defs_raw)) == u8"%Pile")
                defs_list = cadr(defs_raw);

            for (Value d = defs_list; consp(d); d = cdr(d))
            {
                auto def = car(d);
                if (not consp(def)) { nondefs.push_back(def); continue; }
                auto dtag_v = car(def);
                if (not symbolp(dtag_v)) { nondefs.push_back(def); continue; }
                auto dtag = symbol_name(dtag_v);

                if (dtag == u8"%ConstantDefinition")
                {
                    auto lhs = cadr(def);
                    auto rhs = caddr(def);
                    // -- Check if LHS is a %Call (function def).
                    if (consp(lhs) and symbolp(car(lhs))
                        and symbol_name(car(lhs)) == u8"%Call")
                    {
                        auto op_name = cadr(lhs);
                        auto args_ast = caddr(lhs);
                        if (symbolp(op_name))
                        {
                            // -- Check if it has args.
                            // Empty tuple ((%Tuple NIL)) means
                            // zero-arg function > substitute.
                            bool has_args = not null(args_ast);
                            if (has_args
                                and consp(args_ast)
                                and null(cdr(args_ast))
                                and consp(car(args_ast))
                                and is_symbol(
                                    caar(args_ast), u8"%Tuple")
                                and null(
                                    cadr(car(args_ast))))
                                has_args = false;

                            if (has_args)
                            {
                                // -- Rename: enclosingFunction,localName
                                auto renamed_name =
                                    tu.enclosing_function + u8","
                                    + std::u8string(
                                        symbol_name(op_name));
                                auto renamed = sym(a, renamed_name);
                                opassoc.push_back(
                                    { op_name, renamed });
                                // -- Translate args same as in
                                // %ConstantDefinition handler.
                                Value args_v;
                                if (consp(args_ast)
                                    and null(cdr(args_ast)))
                                {
                                    auto single = car(args_ast);
                                    args_v = translate(tu, single);
                                }
                                else
                                {
                                    std::vector<Value> targs;
                                    for (Value e = args_ast;
                                         consp(e); e = cdr(e))
                                        targs.push_back(
                                            translate(tu, car(e)));
                                    args_v = a.cons(
                                        sym(a, u8"TUPLE"),
                                        a.list(targs));
                                }
                                auto body_v2 = translate(tu, rhs);
                                body_v2 = translate_form(a, body_v2);
                                def_stack.push_back(
                                    { op_name, renamed,
                                      body_v2, args_v });
                            }
                            else
                            {
                                // -- No args: substitute body.
                                auto body_v2 = translate(tu, rhs);
                                body_v2 = translate_form(a, body_v2);
                                opassoc.push_back(
                                    { op_name, body_v2 });
                            }
                        }
                        else
                            nondefs.push_back(def);
                    }
                    else if (symbolp(lhs))
                    {
                        // -- Simple constant def in where.
                        auto body_v2 = translate(tu, rhs);
                        body_v2 = translate_form(a, body_v2);
                        opassoc.push_back({ lhs, body_v2 });
                    }
                    else
                        nondefs.push_back(def);
                }
                else
                    nondefs.push_back(def);
            }

            // -- Add def_stack entries to side_conditions.
            for (auto& di : def_stack)
            {
                // -- (renamed args body) triple for bf_def to emit.
                tu.side_conditions.push_back(
                    a.list({ di.renamed, di.args, di.body }));
            }

            // -- Apply substitutions (opassoc) to body and nondefs.
            // bfSUBLIS: replace each op_name with its renamed
            // value in the expression tree.
            using SubPair = std::pair<Value, Value>;
            struct SubHelper
            {
                Lisp::Arena& arena;
                const std::vector<SubPair>& pairs;
                Value apply(Value x) const
                {
                    if (null(x)) return x;
                    if (symbolp(x))
                    {
                        for (auto& [from, to] : pairs)
                        {
                            if (symbolp(from)
                                and symbol_name(from)
                                   == symbol_name(x))
                                return to;
                        }
                        return x;
                    }
                    if (not consp(x)) return x;
                    if (is_symbol(car(x), u8"QUOTE"))
                        return x;
                    auto h = apply(car(x));
                    auto t = apply(cdr(x));
                    if (h == car(x) and t == cdr(x))
                        return x;
                    return arena.cons(h, t);
                }
            };
            SubHelper sublis_helper{ a, opassoc };

            // -- Apply substitutions to nondefs and body.
            std::vector<Value> tnondefs;
            for (auto& nd : nondefs)
                tnondefs.push_back(
                    sublis_helper.apply(translate(tu, nd)));
            auto sub_body = sublis_helper.apply(body_v);

            // -- Build result: nondefs + body.
            tnondefs.push_back(sub_body);
            return bf_mkprogn(a, a.list(tnondefs));
        }

        // -- DefaultValue.
        if (tag == u8"%DefaultValue")
        {
            auto name = translate(tu, cadr(ast));
            auto value = translate(tu, caddr(ast));
            return a.list({ sym(a, u8"DEFAULT"), name, value });
        }

        // -- Key.
        if (tag == u8"%Key")
        {
            auto key = translate(tu, cadr(ast));
            auto value = translate(tu, caddr(ast));
            return a.list({ sym(a, u8"%Key"), key, value });
        }

        // -- QualifiedName.
        if (tag == u8"%QualifiedName")
        {
            auto qualifier = translate(tu, cadr(ast));
            auto name = translate(tu, caddr(ast));
            // -- In old Boot, this becomes bfColonColon(qualifier, name).
            // Which calls makeSymbol or symbolBinding.
            // For now, just return as-is.
            return a.list({ sym(a, u8"bfColonColon"),
                            qualifier, name });
        }

        // -- Throw.
        if (tag == u8"%Throw")
        {
            auto expr = translate(tu, cadr(ast));
            return a.list({ sym(a, u8"THROW"), expr });
        }

        // -- Try/Catch/Finally.
        if (tag == u8"%Try")
        {
            auto body_v = translate(tu, cadr(ast));
            return a.list({
                sym(a, u8"HANDLER-CASE"), body_v,
                tx_list(tu, caddr(ast)) });
        }
        if (tag == u8"%Catch")
        {
            auto sig = translate(tu, cadr(ast));
            auto body_v = translate(tu, caddr(ast));
            return a.list({ sig, body_v });
        }
        if (tag == u8"%Finally")
        {
            auto body_v = translate(tu, cadr(ast));
            return a.list({ sym(a, u8"UNWIND-PROTECT"), body_v });
        }

        // -- Case.
        if (tag == u8"%Case")
        {
            auto expr = translate(tu, cadr(ast));
            return a.cons(sym(a, u8"CASE"),
                a.cons(expr, tx_list(tu, caddr(ast))));
        }

        // -- Lisp (embedded Lisp expression).
        if (tag == u8"%Lisp")
        {
            // -- Return the string as a raw Lisp form marker.
            return a.list({ sym(a, u8"+LINE"), cadr(ast) });
        }

        // -- Fallback: return as-is.
        return ast;
    }

    // -- bfSequence: sequence/pile to PROGN.
    // Check if form is a single-clause COND exit:
    // (COND (test (IDENTITY val))) > returns true and extracts
    // test and val.
    static bool exit_cond_p(Value form, Value& test, Value& val)
    {
        if (not consp(form)) return false;
        if (not is_symbol(car(form), u8"COND")) return false;
        auto clauses = cdr(form);
        if (not consp(clauses)) return false;
        if (not null(cdr(clauses))) return false; // must be single clause
        auto clause = car(clauses); // (test (IDENTITY val))
        if (not consp(clause)) return false;
        test = car(clause);
        auto body = cdr(clause);
        if (not consp(body)) return false;
        if (not null(cdr(body))) return false;
        auto identity_form = car(body);
        if (not consp(identity_form)) return false;
        if (not is_symbol(car(identity_form), u8"IDENTITY"))
            return false;
        auto id_args = cdr(identity_form);
        if (not consp(id_args)) return false;
        if (not null(cdr(id_args))) return false;
        val = car(id_args);
        return true;
    }

    // -- bfAlternative: builds a COND clause (test body...).
    // Handles the special case where test is
    //   (AND c1 c2 ... (PROGN stmt T))
    // turning it into ((AND c1 c2 ...) stmt body...).
    static Value bf_alternative(Lisp::Arena& a, Value test, Value body)
    {
        // -- Check if test is (AND ... (PROGN stmt T))
        if (consp(test) and is_symbol(car(test), u8"AND"))
        {
            auto and_args = cdr(test);
            if (consp(and_args))
            {
                // -- Get the last element of the AND args.
                Value rev = nil;
                for (Value e = and_args; consp(e); e = cdr(e))
                    rev = a.cons(car(e), rev);
                // -- rev = reversed list, car(rev) = last element
                auto last = car(rev);
                if (consp(last) and is_symbol(car(last), u8"PROGN"))
                {
                    auto progn_body = cdr(last);
                    // -- Check it ends with T.
                    Value last_pb = nil;
                    Value rest_pb = nil;
                    Value prev = nil;
                    for (Value e = progn_body; consp(e); e = cdr(e))
                    {
                        if (not null(cdr(e)))
                        {
                            if (null(rest_pb))
                                rest_pb = e;
                        }
                        else
                            last_pb = car(e);
                    }
                    if (symbolp(last_pb)
                        and is_symbol(last_pb, u8"T"))
                    {
                        // -- Extract stmt (everything before the T).
                        Value stmt = nil;
                        if (consp(progn_body)
                            and null(cdr(progn_body)))
                        {
                            // -- Only T, no stmt.
                        }
                        else
                        {
                            // -- Collect all but last.
                            std::vector<Value> stmts;
                            for (Value e = progn_body; consp(e);
                                 e = cdr(e))
                            {
                                if (not null(cdr(e)))
                                    stmts.push_back(car(e));
                            }
                            if (stmts.size() == 1)
                                stmt = stmts[0];
                            else
                                stmt = a.cons(sym(a, u8"PROGN"),
                                              a.list(stmts));
                        }

                        // -- Rebuild AND without the last element.
                        std::vector<Value> conds;
                        // -- rev has the reversed list; skip car(rev)
                        // = last, reverse the rest.
                        Value crev = cdr(rev);
                        for (Value e = crev; consp(e); e = cdr(e))
                            conds.insert(conds.begin(), car(e));
                        Value new_test;
                        if (conds.size() == 1)
                            new_test = conds[0];
                        else
                            new_test = a.cons(
                                sym(a, u8"AND"),
                                a.list(conds));

                        Value merged_body;
                        if (not null(stmt))
                            merged_body = bf_mkprogn(a,
                                a.cons(stmt, a.cons(body, nil)));
                        else
                            merged_body = body;
                        return a.cons(new_test,
                            unwrap_progn(a, merged_body));
                    }
                }
            }
        }
        return a.cons(test,
            unwrap_progn(a, body));
    }

    static Value bf_sequence(TranslationUnit& tu, Value lst)
    {
        auto& a = tu.arena;
        if (null(lst)) return nil;

        // -- Collect consecutive exit-COND forms from the front.
        std::vector<Value> clauses;
        Value rest = lst;
        while (consp(rest))
        {
            Value test, val;
            if (not exit_cond_p(car(rest), test, val))
                break;
            clauses.push_back(bf_alternative(a, test, val));
            rest = cdr(rest);
        }

        if (clauses.empty())
        {
            // -- No exits at the front.
            if (null(cdr(lst)))
            {
                auto f = car(lst);
                if (consp(f) and is_symbol(car(f), u8"PROGN"))
                    return bf_sequence(tu, cdr(f));
                return f;
            }
            return bf_mkprogn(a,
                a.cons(car(lst),
                       a.cons(bf_sequence(tu, cdr(lst)), nil)));
        }

        // -- We have some exit clauses.
        if (null(rest))
        {
            // -- All exits, no fallthrough > (COND cl1 cl2 ...)
            return a.cons(sym(a, u8"COND"),
                          a.list(clauses));
        }

        // -- Exits + fallthrough > (COND cl1 cl2 ... (T fallthrough))
        auto fallthrough = bf_sequence(tu, rest);
        clauses.push_back(bf_alternative(a, sym(a, u8"T"), fallthrough));
        return a.cons(sym(a, u8"COND"),
                      a.list(clauses));
    }

    // -- translateForm: final cleanup pass

    // -- Rename table for Boot > CL function names.
    static const char8_t* rename_op(std::u8string_view name)
    {
        static const std::pair<const char8_t*, const char8_t*> table[] = {
            {u8"abs", u8"ABS"},
            {u8"abstractChar", u8"CODE-CHAR"},
            {u8"alphabetic?", u8"ALPHA-CHAR-P"},
            {u8"alphanumeric?", u8"ALPHANUMERICP"},
            {u8"array?", u8"ARRAYP"},
            {u8"arrayRef", u8"AREF"},
            {u8"atom", u8"ATOM"},
            {u8"bitref", u8"SBIT"},
            {u8"canonicalFilename", u8"PROBE-FILE"},
            {u8"charByName", u8"NAME-CHAR"},
            {u8"charDowncase", u8"CHAR-DOWNCASE"},
            {u8"charEq?", u8"CHAR="},
            {u8"charUpcase", u8"CHAR-UPCASE"},
            {u8"charString", u8"STRING"},
            {u8"char?", u8"CHARACTERP"},
            {u8"codePoint", u8"CHAR-CODE"},
            {u8"cons?", u8"CONSP"},
            {u8"copy", u8"COPY"},
            {u8"copyString", u8"COPY-SEQ"},
            {u8"copyVector", u8"COPY-SEQ"},
            {u8"croak", u8"CROAK"},
            {u8"digit?", u8"DIGIT-CHAR-P"},
            {u8"exit", u8"EXIT"},
            {u8"fifth", u8"FIFTH"},
            {u8"first", u8"CAR"},
            {u8"fileNameString", u8"FILE-NAMESTRING"},
            {u8"filePath", u8"PATHNAME"},
            {u8"filePath?", u8"PATHNAMEP"},
            {u8"filePathDirectory", u8"PATHNAME-DIRECTORY"},
            {u8"filePathName", u8"PATHNAME-NAME"},
            {u8"filePathString", u8"NAMESTRING"},
            {u8"filePathType", u8"PATHNAME-TYPE"},
            {u8"float?", u8"FLOATP"},
            {u8"flushOutput", u8"FORCE-OUTPUT"},
            {u8"fourth", u8"CADDDR"},
            {u8"freshLine", u8"FRESH-LINE"},
            {u8"function?", u8"FUNCTIONP"},
            {u8"functionSymbol?", u8"FBOUNDP"},
            {u8"gensym", u8"GENSYM"},
            {u8"genvar", u8"GENVAR"},
            {u8"importSymbol", u8"IMPORT"},
            {u8"inert?", u8"KEYWORDP"},
            {u8"integer?", u8"INTEGERP"},
            {u8"LAST", u8"last"},
            {u8"list", u8"LIST"},
            {u8"listEq?", u8"EQUAL"},
            {u8"lowerCase?", u8"LOWER-CASE-P"},
            {u8"makeFilePath", u8"MAKE-PATHNAME"},
            {u8"makeSymbol", u8"INTERN"},
            {u8"mergeFilePaths", u8"MERGE-PATHNAMES"},
            {u8"newVector", u8"MAKE-ARRAY"},
            {u8"not", u8"NOT"},
            {u8"null", u8"NULL"},
            {u8"odd?", u8"ODDP"},
            {u8"property", u8"GET"},
            {u8"readInteger", u8"PARSE-INTEGER"},
            {u8"readLispFromString", u8"READ-FROM-STRING"},
            {u8"readOnly?", u8"CONSTANTP"},
            {u8"removeDuplicates", u8"REMDUP"},
            {u8"rest", u8"CDR"},
            {u8"sameObject?", u8"EQ"},
            {u8"scalarEq?", u8"EQL"},
            {u8"scalarEqual?", u8"EQL"},
            {u8"second", u8"CADR"},
            {u8"setPart", u8"SETELT"},
            {u8"strconc", u8"CONCAT"},
            {u8"stringChar", u8"SCHAR"},
            {u8"stringDowncase", u8"STRING-DOWNCASE"},
            {u8"string?", u8"STRINGP"},
            {u8"stringEq?", u8"STRING="},
            {u8"stringUpcase", u8"STRING-UPCASE"},
            {u8"subSequence", u8"SUBSEQ"},
            {u8"symbolBinding", u8"FIND-SYMBOL"},
            {u8"symbolScope", u8"SYMBOL-PACKAGE"},
            {u8"symbolEq?", u8"EQ"},
            {u8"symbolFunction", u8"SYMBOL-FUNCTION"},
            {u8"symbolGlobal?", u8"BOUNDP"},
            {u8"symbolName", u8"SYMBOL-NAME"},
            {u8"symbolValue", u8"SYMBOL-VALUE"},
            {u8"symbol?", u8"SYMBOLP"},
            {u8"third", u8"CADDR"},
            {u8"toString", u8"WRITE-TO-STRING"},
            {u8"upperCase?", u8"UPPER-CASE-P"},
            {u8"valueEq?", u8"EQUAL"},
            {u8"vector?", u8"SIMPLE-VECTOR-P"},
            {u8"vectorRef", u8"SVREF"},
            {u8"writeByte", u8"WRITE-BYTE"},
            {u8"writeChar", u8"WRITE-CHAR"},
            {u8"writeInteger", u8"PRINC"},
            {u8"writeLine", u8"WRITE-LINE"},
            {u8"writeNewline", u8"TERPRI"},
            {u8"writeString", u8"WRITE-STRING"},
            {u8"PLUS", u8"+"},
            {u8"MINUS", u8"-"},
            {u8"TIMES", u8"*"},
            {u8"POWER", u8"EXPT"},
            {u8"SLASH", u8"/"},
            {u8"LT", u8"<"},
            {u8"GT", u8">"},
            {u8"LE", u8"<="},
            {u8"GE", u8">="},
            {u8"SHOEEQ", u8"EQUAL"},
            {u8"SHOENE", u8"/="},
            {u8"T", u8"T$"},
        };
        for (auto& [from, to] : table)
        {
            if (name == from)
                return to;
        }
        return nullptr;
    }

    Value translate_form(Lisp::Arena& a, Value x)
    {
        if (not consp(x)) return x;
        if (is_symbol(car(x), u8"QUOTE")) return x;

        // -- apply > FUNCALL/APPLY.
        if (is_symbol(car(x), u8"apply"))
        {
            auto fun = cadr(x);
            auto args = cddr(x);
            auto last_arg = last_elem(args);
            if (null(last_arg))
            {
                // -- (apply f arg1 ... NIL) > (FUNCALL f targ1 ...)
                Value result = nil;
                auto translated = but_last(a, cdr(x));
                std::vector<Value> telems;
                for (Value e = translated; consp(e); e = cdr(e))
                    telems.push_back(translate_form(a, car(e)));
                return a.cons(sym(a, u8"FUNCALL"),
                              a.list(telems));
            }
            // -- General case: (APPLY f args...).
            std::vector<Value> telems;
            for (Value e = cdr(x); consp(e); e = cdr(e))
                telems.push_back(translate_form(a, car(e)));
            return a.cons(sym(a, u8"APPLY"), a.list(telems));
        }

        // -- LET > translate bindings.
        if (is_symbol(car(x), u8"LET"))
        {
            auto bindings = cadr(x);
            auto body = caddr(x);
            std::vector<Value> tbindings;
            for (Value b = bindings; consp(b); b = cdr(b))
            {
                auto binding = car(b);
                if (consp(binding) and consp(cdr(binding)))
                {
                    auto var = car(binding);
                    auto init = cadr(binding);
                    tbindings.push_back(
                        a.list({ var, translate_form(a, init) }));
                }
                else
                    tbindings.push_back(binding);
            }
            return a.list({
                car(x),
                a.list(tbindings),
                translate_form(a, body) });
        }

        // -- L%T > translate init.
        if (is_symbol(car(x), u8"L%T") and consp(cdr(x)) and consp(cddr(x)))
        {
            auto var = cadr(x);
            auto init = caddr(x);
            return a.list({ car(x), var, translate_form(a, init) });
        }

        // -- COND > translate each branch.
        if (is_symbol(car(x), u8"COND"))
        {
            std::vector<Value> tbranches;
            for (Value e = cdr(x); consp(e); e = cdr(e))
            {
                auto branch = car(e);
                std::vector<Value> telems;
                for (Value b = branch; consp(b); b = cdr(b))
                    telems.push_back(translate_form(a, car(b)));
                tbranches.push_back(a.list(telems));
            }
            return a.cons(sym(a, u8"COND"),
                          a.list(tbranches));
        }

        // -- PROGN > re-sequence after translating children.
        if (is_symbol(car(x), u8"PROGN"))
        {
            std::vector<Value> telems;
            for (Value e = cdr(x); consp(e); e = cdr(e))
                telems.push_back(translate_form(a, car(e)));
            // -- bf_sequence only uses tu.arena - safe to use a
            // temporary TranslationUnit here.
            TranslationUnit dummy_tu(a);
            return bf_sequence(dummy_tu, a.list(telems));
        }

        // -- LOOP, RETURN > translate args.
        if (is_symbol(car(x), u8"LOOP")
            or is_symbol(car(x), u8"RETURN"))
        {
            std::vector<Value> telems;
            for (Value e = cdr(x); consp(e); e = cdr(e))
                telems.push_back(translate_form(a, car(e)));
            return a.cons(car(x), a.list(telems));
        }

        // -- Default: check for rename, translate all elements.
        auto op = car(x);
        if (symbolp(op))
        {
            auto renamed = rename_op(symbol_name(op));
            if (renamed)
                op = sym(a, renamed);
        }
        std::vector<Value> targs;
        for (Value e = cdr(x); consp(e); e = cdr(e))
            targs.push_back(translate_form(a, car(e)));
        return a.cons(op, a.list(targs));
    }

    // -- shoeCompTran: variable analysis

    // -- Collect atoms from a parameter list.
    static void collect_atoms(Value x, std::vector<Value>& result)
    {
        if (null(x)) return;
        if (symbolp(x))
        {
            result.push_back(x);
            return;
        }
        if (consp(x))
        {
            collect_atoms(car(x), result);
            collect_atoms(cdr(x), result);
        }
    }

    // -- Check if a symbol is a dynamic ($-prefixed) variable.
    static bool dynamic_var_p(Value x)
    {
        return symbolp(x) and dollar_prefixed(x);
    }

    // -- shoeCompTran1: scan body for L%T, %Dynamic, %Leave, %Namespace.
    // (Replaced by scan_for_vars below.)

    // -- scan_for_vars: recursive scanner for variable declarations.
    static void scan_for_vars(Value x,
                               std::vector<Value>& fluid_vars,
                               std::vector<Value>& local_vars,
                               std::vector<Value>& dollar_vars)
    {
        if (null(x) or not consp(x)) return;

        // -- L%T var init > local variable.
        if (is_symbol(car(x), u8"L%T") and consp(cdr(x)))
        {
            auto var = cadr(x);
            if (symbolp(var))
            {
                if (dynamic_var_p(var))
                    dollar_vars.push_back(var);
                else
                    local_vars.push_back(var);
            }
            if (consp(cddr(x)))
                scan_for_vars(caddr(x), fluid_vars, local_vars, dollar_vars);
            return;
        }

        // -- %Dynamic x > fluid variable.
        if (is_symbol(car(x), u8"%Dynamic") and consp(cdr(x)))
        {
            auto var = cadr(x);
            if (symbolp(var))
                fluid_vars.push_back(var);
            return;
        }

        // -- QUOTE > don't recurse.
        if (is_symbol(car(x), u8"QUOTE")) return;

        // -- Recurse into all sub-forms.
        for (Value e = x; consp(e); e = cdr(e))
            scan_for_vars(car(e), fluid_vars, local_vars, dollar_vars);
    }

    // -- Convert L%T to SETQ, %Dynamic to fluid, %Leave to RETURN.
    static Value shoe_comp_tran1_transform(Arena& a, Value x)
    {
        if (null(x) or not consp(x)) return x;
        if (is_symbol(car(x), u8"QUOTE")) return x;

        if (is_symbol(car(x), u8"L%T") and consp(cdr(x)))
        {
            auto var = cadr(x);
            auto init = consp(cddr(x)) ? caddr(x) : nil;
            return a.list({ sym(a, u8"SETQ"), var,
                            shoe_comp_tran1_transform(a, init) });
        }

        if (is_symbol(car(x), u8"%Leave") and consp(cdr(x)))
        {
            return a.list({ sym(a, u8"RETURN"),
                            shoe_comp_tran1_transform(a, cadr(x)) });
        }

        if (is_symbol(car(x), u8"%Dynamic") and consp(cdr(x)))
        {
            // -- Dynamic vars are handled by declarations; strip the wrapper.
            return cadr(x);
        }

        // -- Recurse.
        std::vector<Value> result;
        for (Value e = x; consp(e); e = cdr(e))
            result.push_back(shoe_comp_tran1_transform(a, car(e)));
        return a.list(result);
    }

    // -- Remove duplicates from a symbol list.
    static std::vector<Value> unique_syms(const std::vector<Value>& v)
    {
        std::vector<Value> result;
        for (auto s : v)
        {
            bool found = false;
            for (auto r : result)
            {
                if (symbolp(s) and symbolp(r)
                    and symbol_name(s) == symbol_name(r))
                {
                    found = true;
                    break;
                }
            }
            if (not found) result.push_back(s);
        }
        return result;
    }

    // -- Set difference: elements in a but not in b.
    static std::vector<Value> set_diff(const std::vector<Value>& a,
                                    const std::vector<Value>& b)
    {
        std::vector<Value> result;
        for (auto x : a)
        {
            bool in_b = false;
            for (auto y : b)
            {
                if (symbolp(x) and symbolp(y)
                    and symbol_name(x) == symbol_name(y))
                {
                    in_b = true;
                    break;
                }
            }
            if (not in_b) result.push_back(x);
        }
        return result;
    }

    // -- shoeCompTran: full variable analysis.
    //    Takes (LAMBDA args body...) and produces
    //    (LAMBDA args (LET* ((locals..)) (DECLARE ...) body'...)).
    static Value shoe_comp_tran(Arena& arena, Value x)
    {
        if (not consp(x)) return x;
        auto lamtype = car(x);
        auto args = cadr(x);
        auto body = cddr(x);

        // -- Scan for variables.
        std::vector<Value> fluid_vars, local_vars, dollar_vars;
        for (Value s = body; consp(s); s = cdr(s))
            scan_for_vars(car(s), fluid_vars, local_vars, dollar_vars);

        fluid_vars = unique_syms(fluid_vars);
        local_vars = unique_syms(local_vars);
        dollar_vars = unique_syms(dollar_vars);

        // -- Remove fluid vars and parameter atoms from local vars.
        std::vector<Value> param_atoms;
        collect_atoms(args, param_atoms);
        local_vars = set_diff(local_vars, fluid_vars);
        local_vars = set_diff(local_vars, param_atoms);

        // -- Transform the body: L%T > SETQ, %Leave > RETURN.
        std::vector<Value> tbody;
        for (Value s = body; consp(s); s = cdr(s))
            tbody.push_back(shoe_comp_tran1_transform(arena, car(s)));

        // -- Build the new body with declarations.
        std::vector<Value> new_body;

        // -- Dollar vars that aren't fluid > DECLARE SPECIAL.
        auto fvars = set_diff(dollar_vars, fluid_vars);
        if (not fvars.empty())
        {
            auto decl = arena.cons(sym(arena, u8"SPECIAL"),
                                    arena.list(fvars));
            new_body.push_back(arena.list({ sym(arena, u8"DECLARE"), decl }));
        }

        // -- Fluid parameters > DECLARE SPECIAL.
        std::vector<Value> fluid_params;
        for (auto p : param_atoms)
        {
            for (auto f : fluid_vars)
            {
                if (symbolp(p) and symbolp(f)
                    and symbol_name(p) == symbol_name(f))
                {
                    fluid_params.push_back(p);
                    break;
                }
            }
        }
        if (not fluid_params.empty())
        {
            auto decl = arena.cons(sym(arena, u8"SPECIAL"),
                                    arena.list(fluid_params));
            new_body.push_back(
                arena.list({ sym(arena, u8"DECLARE"), decl }));
        }

        // -- Local vars > LET* wrapping.
        if (not local_vars.empty())
        {
            // -- Wrap in LET*.
            std::vector<Value> letbindings;
            for (auto v : local_vars)
                letbindings.push_back(v);

            for (auto& b : tbody) new_body.push_back(b);
            auto let_body = arena.list(new_body);
            auto result_body = arena.list({
                sym(arena, u8"LET*"),
                arena.list(letbindings) });
            // -- Append the body elements.
            Value result = arena.cons(sym(arena, u8"LET*"),
                arena.cons(arena.list(letbindings),
                           let_body));
            return arena.list({ lamtype, args, result });
        }
        else
        {
            for (auto& b : tbody) new_body.push_back(b);
            auto result = arena.cons(lamtype,
                arena.cons(args, arena.list(new_body)));
            return result;
        }
    }

    // -- bfDef: function definition translation

    // -- Simplified bfDef: extract function name, params, translate body.
    static std::vector<Value> bf_def(TranslationUnit& tu,
                                  Value op, Value args, Value body)
    {
        auto& a = tu.arena;

        // -- Extract parameter list.
        Value param_list;
        if (bf_tuple_p(args))
            param_list = cdr(args);
        else
            param_list = a.cons(args, nil);

        // -- Build (LAMBDA params body) and run shoeCompTran.
        auto lambda = a.cons(sym(a, u8"LAMBDA"),
                              a.cons(param_list, a.cons(body, nil)));
        auto translated = shoe_comp_tran(a, lambda);

        // -- Extract the translated args and body.
        auto targs = cadr(translated);
        auto tbody = cddr(translated);

        // -- Build DEFUN.
        auto defun = a.cons(sym(a, u8"DEFUN"),
                             a.cons(op, a.cons(targs, tbody)));

        // -- Collect: main def + all side conditions.
        std::vector<Value> results;

        // -- First emit side conditions (where-clause local defs).
        auto saved_sides = std::move(tu.side_conditions);
        tu.side_conditions.clear();
        for (auto sc : saved_sides)
        {
            // -- Each sc is (renamed args body).
            auto sc_op = car(sc);
            auto sc_args = cadr(sc);
            auto sc_body = caddr(sc);
            auto sc_defs = bf_def(tu, sc_op, sc_args, sc_body);
            for (auto& d : sc_defs)
                results.push_back(d);
        }

        // -- When clamming, generate the implementation (with ;
        // suffix), a hash-table cache, a wrapper DEFUN, and
        // cache-info metadata.
        if (tu.clamming and symbolp(op))
        {
            auto name = std::u8string(symbol_name(op));
            auto auxfn = sym(a, name + u8";");
            auto cache_name = sym(a, name + u8";AL");

            // -- 1. (DEFUN op; (params) body)
            auto impl_defun = a.cons(sym(a, u8"DEFUN"),
                a.cons(auxfn, a.cons(targs, tbody)));
            results.push_back(impl_defun);

            // -- 2. (DEFPARAMETER cache (MAKE-HASHTABLE 'UEQUAL))
            auto make_ht = a.list({
                sym(a, u8"MAKE-HASHTABLE"),
                a.list({ sym(a, u8"QUOTE"), sym(a, u8"UEQUAL") }) });
            results.push_back(a.list({
                sym(a, u8"DEFPARAMETER"), cache_name, make_ht }));

            // -- 3. (DEFUN op (&REST g1)
            //      (PROG (g2) (RETURN (COND
            //        ((SETQ g2 (GETHASH g1 cache)) g2)
            //        (T (SETF (GETHASH g1 cache)
            //                 (APPLY #'auxfn g1)))))))
            auto g1 = tu.gensym();
            auto g2 = tu.gensym();
            auto rest_arg = a.list({
                sym(a, u8"&REST"), g1 });
            auto gethash = a.list({
                sym(a, u8"GETHASH"), g1, cache_name });
            auto setq_clause = a.list({
                a.list({ sym(a, u8"SETQ"), g2, gethash }),
                g2 });
            auto apply_call = a.list({
                sym(a, u8"APPLY"),
                a.list({ sym(a, u8"FUNCTION"), auxfn }),
                g1 });
            auto setf_clause = a.list({
                sym(a, u8"T"),
                a.list({ sym(a, u8"SETF"), gethash, apply_call }) });
            auto cond = a.list({
                sym(a, u8"COND"), setq_clause, setf_clause });
            auto prog_body = a.list({
                sym(a, u8"PROG"),
                a.list({ g2 }),
                a.list({ sym(a, u8"RETURN"), cond }) });
            auto wrapper_defun = a.list({
                sym(a, u8"DEFUN"), op, rest_arg, prog_body });
            results.push_back(wrapper_defun);

            // -- 4. (SETF (GET 'op 'cacheInfo)
            //      '(op cache hash-table
            //        (SETQ cache (MAKE-HASHTABLE 'UEQUAL))
            //        (hashCount cache)))
            auto reset_code = a.list({
                sym(a, u8"SETQ"), cache_name, make_ht });
            auto count_code = a.list({
                sym(a, u8"hashCount"), cache_name });
            auto cache_vec = a.list({
                op, cache_name, sym(a, u8"hash-table"),
                reset_code, count_code });
            results.push_back(a.list({
                sym(a, u8"SETF"),
                a.list({ sym(a, u8"GET"),
                         a.list({ sym(a, u8"QUOTE"), op }),
                         a.list({ sym(a, u8"QUOTE"),
                                  sym(a, u8"cacheInfo") }) }),
                a.list({ sym(a, u8"QUOTE"), cache_vec }) }));
        }
        else
        {
            // -- Then the main DEFUN.
            results.push_back(defun);
        }
        return results;
    }

    // -- bfMDef: macro definition translation

    static std::vector<Value> bf_mdef(TranslationUnit& tu,
                                   Value op, Value args, Value body)
    {
        auto& a = tu.arena;
        Value param_list;
        if (bf_tuple_p(args))
            param_list = cdr(args);
        else
            param_list = a.cons(args, nil);

        // -- Build DEFMACRO with a MLAMBDA-style body.
        // Simplified: just use the body directly.
        auto defmacro = a.cons(sym(a, u8"DEFMACRO"),
            a.cons(op, a.cons(param_list, a.cons(body, nil))));
        return { defmacro };
    }

    // -- def_from_call: shared tail for %ConstantDefinition
    //    when the LHS is a %Call (direct or via %Signature).
    //    Sets enclosing_function, translates the body, and
    //    calls bf_def.
    static std::vector<Value> def_from_call(
        TranslationUnit& tu, Value op, Value args, Value rhs)
    {
        auto saved_ef = tu.enclosing_function;
        if (symbolp(op))
            tu.enclosing_function =
                std::u8string(symbol_name(op));
        tu.side_conditions.clear();
        auto body = translate(tu, rhs);
        body = translate_form(tu.arena, body);
        auto result = bf_def(tu, op, args, body);
        tu.enclosing_function = saved_ef;
        return result;
    }

    // -- genDeclaration: type declaration

    // -- bfType: convert a Boot type to a Lisp type specifier.
    static Value bf_type(Arena& a, Value t)
    {
        if (consp(t) and is_symbol(car(t), u8"%Mapping"))
        {
            auto target = cadr(t);
            auto sources = caddr(t);
            // -- (FUNCTION (src1 src2 ...) target)
            return a.list({
                sym(a, u8"FUNCTION"),
                sources,
                bf_type(a, target) });
        }
        return t;
    }

    static Value gen_declaration(Arena& a, Value name, Value t)
    {
        if (consp(t) and is_symbol(car(t), u8"%Mapping"))
        {
            return a.list({
                sym(a, u8"DECLAIM"),
                a.list({ sym(a, u8"FTYPE"), bf_type(a, t), name }) });
        }
        if (consp(t) and is_symbol(car(t), u8"%Forall"))
        {
            // -- Substitute type vars with *.
            auto body_t = caddr(t);
            return gen_declaration(a, name, body_t);
        }
        return a.list({
            sym(a, u8"DECLAIM"),
            a.list({ sym(a, u8"TYPE"), bf_type(a, t), name }) });
    }

    // -- translateToplevel: top-level dispatch

    std::vector<Value> translate_toplevel(TranslationUnit& tu, Value ast)
    {
        auto& a = tu.arena;

        if (not consp(ast))
            return { ast };

        auto tag_v = car(ast);
        if (not symbolp(tag_v))
            return { ast };
        auto tag = symbol_name(tag_v);

        // -- %Where at top level: lift where-clause into the
        //    body of the wrapped definition.
        if (tag == u8"%Where")
        {
            auto body = cadr(ast);
            auto defs = caddr(ast);

            // -- If the body is a definition node, lift the
            // where-clause into the definition's body (RHS).
            if (consp(body))
            {
                auto btag = car(body);
                if (symbolp(btag))
                {
                    auto bn = symbol_name(btag);
                    if (bn == u8"%ConstantDefinition"
                        or bn == u8"%Definition")
                    {
                        auto def_lhs = cadr(body);
                        auto def_rhs = caddr(body);
                        auto new_rhs = a.list(
                            { sym(a, u8"%Where"),
                              def_rhs, defs });
                        auto new_def = a.list(
                            { btag, def_lhs, new_rhs });
                        return translate_toplevel(tu, new_def);
                    }
                    if (bn == u8"%Macro")
                    {
                        auto op = cadr(body);
                        auto args = caddr(body);
                        auto mbody = car(cddr(cdr(body)));
                        auto new_body = a.list(
                            { sym(a, u8"%Where"),
                              mbody, defs });
                        auto new_macro = a.list(
                            { btag, op, args, new_body });
                        return translate_toplevel(tu, new_macro);
                    }
                }
            }

            // -- Fall through: translate as expression.
            auto expr = translate(tu, ast);
            expr = translate_form(a, expr);
            return { expr };
        }

        // -- %Signature(name, type) > (DECLAIM ...)
        if (tag == u8"%Signature")
        {
            auto name = cadr(ast);
            auto type = caddr(ast);
            auto name_t = translate(tu, name);
            auto type_t = translate(tu, type);
            return { gen_declaration(a, name_t, type_t) };
        }

        // -- %Definition(op, args, body) > DEFUN
        if (tag == u8"%Definition")
        {
            auto op = translate(tu, cadr(ast));
            auto args = translate(tu, caddr(ast));
            auto body = translate(tu, car(cddr(cdr(ast))));
            body = translate_form(a, body);
            return bf_def(tu, op, args, body);
        }

        // -- %ConstantDefinition(lhs, rhs)
        //    If lhs is a %Call, treat as function definition.
        //    Otherwise, treat as DEFCONSTANT.
        if (tag == u8"%ConstantDefinition")
        {
            auto lhs_raw = cadr(ast);
            auto rhs = caddr(ast);

            // -- Unwrap %Restrict (qualified names like
            // AxiomCore::%sysInit()).
            // (%Restrict qualifier (%Call name args)) > (%Call name args)
            Value lhs_inner = lhs_raw;
            if (consp(lhs_raw)
                and is_symbol(car(lhs_raw), u8"%Restrict"))
            {
                auto inner = caddr(lhs_raw);
                if (consp(inner)
                    and is_symbol(car(inner), u8"%Call"))
                    lhs_inner = inner;
            }

            // -- Check if LHS is a function call shape.
            if (consp(lhs_inner) and is_symbol(car(lhs_inner), u8"%Call"))
            {
                auto op_ast = cadr(lhs_inner);
                // -- For qualified names (%QualifiedName pkg name),
                // extract just the name for the DEFUN.
                Value op;
                if (consp(op_ast) and is_symbol(
                    car(op_ast), u8"%QualifiedName"))
                {
                    op = translate(tu, caddr(op_ast));
                }
                else
                {
                    op = translate(tu, op_ast);
                }
                auto raw_args = caddr(lhs_inner);

                // -- Functions with double-quoted (lazy) parameters
                // get a ,LAM suffix on their DEFUN name.
                if (lazy_params_p(raw_args) and symbolp(op))
                {
                    auto name = symbol_name(op);
                    auto lam_name = std::u8string(name) + u8",LAM";
                    op = sym(a, lam_name);
                }

                // -- Build a tuple from the args.
                Value args;
                if (consp(raw_args) and length(raw_args) == 1)
                {
                    // -- Single arg: check if it's a tuple.
                    auto single = car(raw_args);
                    if (consp(single) and is_symbol(car(single), u8"%Tuple"))
                        args = translate(tu, single);
                    else
                        args = translate(tu, single);
                }
                else
                {
                    // -- Multiple args in a list > make a TUPLE.
                    args = a.cons(sym(a, u8"TUPLE"),
                                   tx_list(tu, raw_args));
                }
                return def_from_call(tu, op, args, rhs);
            }

            // -- Check if LHS is a %Signature wrapping a function call.
            if (consp(lhs_raw) and is_symbol(car(lhs_raw), u8"%Signature"))
            {
                auto inner = cadr(lhs_raw);
                auto sig_type = caddr(lhs_raw);
                if (consp(inner) and is_symbol(car(inner), u8"%Call"))
                {
                    auto op = translate(tu, cadr(inner));
                    Value args = a.cons(sym(a, u8"TUPLE"),
                                     tx_list(tu, caddr(inner)));
                    auto defs = def_from_call(tu, op, args, rhs);
                    // -- Add the signature declaration.
                    auto sig = gen_declaration(a, op, translate(tu, sig_type));
                    defs.insert(defs.begin(), sig);
                    return defs;
                }
            }

            // -- Simple constant.
            auto lhs = translate(tu, lhs_raw);
            auto rhs_t = translate(tu, rhs);
            rhs_t = translate_form(a, rhs_t);
            return { a.list({ sym(a, u8"DEFCONSTANT"), lhs, rhs_t }) };
        }

        // -- %Assignment(target, value) > DEFPARAMETER
        if (tag == u8"%Assignment")
        {
            auto lhs_raw = cadr(ast);
            auto rhs = caddr(ast);

            // -- Check for %Signature wrapper.
            Value sig = nil;
            Value lhs;
            if (consp(lhs_raw) and is_symbol(car(lhs_raw), u8"%Signature"))
            {
                auto name = cadr(lhs_raw);
                auto type = caddr(lhs_raw);
                sig = gen_declaration(a, translate(tu, name), translate(tu, type));
                lhs = translate(tu, name);
            }
            else
            {
                lhs = translate(tu, lhs_raw);
            }

            auto rhs_t = translate(tu, rhs);
            rhs_t = translate_form(a, rhs_t);
            std::vector<Value> result;
            if (not null(sig)) result.push_back(sig);
            result.push_back(
                a.list({ sym(a, u8"DEFPARAMETER"), lhs, rhs_t }));
            return result;
        }

        // -- %Module(name, exports, interface)
        if (tag == u8"%Module")
        {
            auto name = cadr(ast);
            auto exports = caddr(ast);
            auto iface = car(cddr(cdr(ast)));

            std::vector<Value> result;
            // -- (PROVIDE "name")
            auto name_str = symbolp(name) ? symbol_name(name) : std::u8string_view(u8"???");
            result.push_back(
                a.list({ sym(a, u8"PROVIDE"), str(a, name_str) }));

            // -- (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
            //   (EXPORT '(names...)))
            if (not null(exports))
            {
                std::vector<Value> export_names;
                for (Value e = exports; consp(e); e = cdr(e))
                {
                    auto n = car(e);
                    if (consp(n) and is_symbol(car(n), u8"%Quote"))
                        export_names.push_back(cadr(n));
                    else
                        export_names.push_back(translate(tu, n));
                }
                auto export_list = a.list(export_names);
                result.push_back(a.list({
                    sym(a, u8"EVAL-WHEN"),
                    a.list({
                        sym(a, u8":COMPILE-TOPLEVEL"),
                        sym(a, u8":LOAD-TOPLEVEL"),
                        sym(a, u8":EXECUTE") }),
                    a.list({
                        sym(a, u8"EXPORT"),
                        a.list({ sym(a, u8"QUOTE"), export_list }) }) }));
            }

            // -- Translate interface definitions.
            for (Value d = iface; consp(d); d = cdr(d))
            {
                auto forms = translate_toplevel(tu, car(d));
                for (auto f : forms) result.push_back(f);
            }

            return result;
        }

        // -- %Import(what)
        if (tag == u8"%Import")
        {
            auto what = cadr(ast);
            auto what_t = translate(tu, what);
            auto name_str = symbolp(what_t)
                ? symbol_name(what_t) : std::u8string_view(u8"???");
            return { a.list({
                sym(a, u8"IMPORT-MODULE"), str(a, name_str) }) };
        }

        // -- %Namespace(name)
        if (tag == u8"%Namespace")
        {
            auto name = cadr(ast);
            auto name_str = symbolp(name)
                ? symbol_name(name) : std::u8string_view(u8"???");
            return { a.list({
                sym(a, u8"IN-PACKAGE"), str(a, name_str) }) };
        }

        // -- %Macro(op, args, body)
        if (tag == u8"%Macro")
        {
            auto op = translate(tu, cadr(ast));
            auto args = translate(tu, caddr(ast));
            auto body = translate(tu, car(cddr(cdr(ast))));
            body = translate_form(a, body);
            return bf_mdef(tu, op, args, body);
        }

        // -- %TypeAlias(name, type)
        if (tag == u8"%TypeAlias")
        {
            auto name = translate(tu, cadr(ast));
            auto type = translate(tu, caddr(ast));
            return { a.list({
                sym(a, u8"DEFTYPE"), name,
                nil,  // empty arg list
                a.list({ sym(a, u8"QUOTE"), bf_type(a, type) }) }) };
        }

        // -- %Structure(name, variants)
        if (tag == u8"%Structure")
        {
            auto name_ast = cadr(ast);
            auto variants_raw = caddr(ast);
            std::vector<Value> result;

            // -- Unwrap single-element list (from span_list).
            Value content = variants_raw;
            if (consp(content) and null(cdr(content)))
                content = car(content);

            // -- Check for Record: (%Record fields accessors)
            if (consp(content) and symbolp(car(content))
                and symbol_name(car(content)) == u8"%Record")
            {
                auto struct_name = translate(tu, name_ast);
                auto fields_raw = cadr(content);
                auto accessors_raw = caddr(content);

                // -- Extract field names from the Record(...)
                // call inside fields_raw.
                // fields_raw is a list of one %Call node:
                // ((%Call Record (%Tuple (%Sig f1 t1) ...)))
                Value fields = nil;
                if (consp(fields_raw))
                {
                    auto record_call = car(fields_raw);
                    if (consp(record_call)
                        and symbolp(car(record_call))
                        and symbol_name(car(record_call))
                           == u8"%Call")
                    {
                        auto args = caddr(record_call);
                        // -- Unwrap span_list wrapper.
                        if (consp(args) and null(cdr(args)))
                        {
                            auto inner = car(args);
                            if (consp(inner)
                                and symbolp(car(inner))
                                and symbol_name(car(inner))
                                   == u8"%Tuple")
                                fields = cadr(inner);
                        }
                    }
                }

                std::vector<std::u8string> field_names;
                for (Value f = fields; consp(f); f = cdr(f))
                {
                    auto field = car(f);
                    if (consp(field)
                        and symbolp(car(field))
                        and symbol_name(car(field))
                           == u8"%Signature"
                        and consp(cdr(field))
                        and symbolp(cadr(field)))
                    {
                        auto fname = symbol_name(
                            cadr(field));
                        field_names.push_back(
                            Boot::unescape_boot_id(fname));
                    }
                }

                if (not field_names.empty())
                {
                    // -- (DEFSTRUCT (%LoadUnit (:COPIER copy%LoadUnit))
                    //   fdefs sigs xports ...)
                    auto copier_name = std::u8string(u8"copy")
                        + std::u8string(symbol_name(struct_name));
                    auto struct_opts = a.list({
                        struct_name,
                        a.list({ sym(a, u8":COPIER"),
                                 sym(a, copier_name) }) });
                    std::vector<Value> slot_names;
                    for (auto& fn : field_names)
                        slot_names.push_back(sym(a, fn));
                    auto defstruct = a.cons(sym(a, u8"DEFSTRUCT"),
                        a.cons(struct_opts,
                               a.list(slot_names)));
                    result.push_back(defstruct);

                    // -- (DEFMACRO mk%LoadUnit (f1 f2 ...)
                    //   (LIST 'MAKE-%LoadUnit :f1 f1 :f2 f2 ...))
                    auto mk_name = std::u8string(u8"mk")
                        + std::u8string(symbol_name(struct_name));
                    auto make_name = std::u8string(u8"MAKE-")
                        + std::u8string(symbol_name(struct_name));
                    std::vector<Value> mk_params;
                    std::vector<Value> mk_body_elems;
                    mk_body_elems.push_back(
                        a.list({ sym(a, u8"QUOTE"),
                                 sym(a, make_name) }));
                    for (auto& fn : field_names)
                    {
                        auto p = sym(a, fn);
                        mk_params.push_back(p);
                        // -- keyword :fieldname
                        mk_body_elems.push_back(
                            sym(a, std::u8string(u8":") + fn));
                        mk_body_elems.push_back(p);
                    }
                    auto mk_macro = a.list({
                        sym(a, u8"DEFMACRO"),
                        sym(a, mk_name),
                        a.list(mk_params),
                        a.cons(sym(a, u8"LIST"),
                               a.list(mk_body_elems)) });
                    result.push_back(mk_macro);

                    // -- Generate accessor DEFMACROs from
                    // AccessorDef nodes.
                    // Each accessor: (%AccessorDef name
                    //   (%Call . fieldname))
                    // Generates: (DEFMACRO name (bfVar#1)
                    //   (LIST 'StructName-field bfVar#1))
                    for (Value ac = accessors_raw; consp(ac);
                         ac = cdr(ac))
                    {
                        auto adef = car(ac);
                        if (not consp(adef)) continue;
                        if (not symbolp(car(adef))) continue;
                        if (symbol_name(car(adef))
                            != u8"%AccessorDef")
                            continue;
                        auto acc_name = cadr(adef);
                        auto selector = caddr(adef);
                        if (not symbolp(acc_name)) continue;
                        // -- Extract field name from selector.
                        // selector is (%Call . fieldname)
                        // where . is the dot operator.
                        std::u8string field_name;
                        if (consp(selector)
                            and symbolp(car(selector))
                            and symbol_name(car(selector))
                               == u8"%Call")
                        {
                            // -- (%Call . fieldname)
                            // caddr = fieldname or
                            // (fieldname) list
                            auto fn = caddr(selector);
                            if (consp(fn))
                                fn = car(fn);
                            if (symbolp(fn))
                            {
                                field_name =
                                    Boot::unescape_boot_id(
                                        symbol_name(fn));
                            }
                        }
                        if (field_name.empty()) continue;
                        // -- Build: StructName-field
                        auto slot_accessor =
                            std::u8string(
                                symbol_name(struct_name))
                            + u8"-" + field_name;
                        auto param = tu.gensym();
                        auto macro = a.list({
                            sym(a, u8"DEFMACRO"),
                            acc_name,
                            a.list({ param }),
                            a.list({
                                sym(a, u8"LIST"),
                                a.list({ sym(a, u8"QUOTE"),
                                    sym(a, slot_accessor) }),
                                param }) });
                        result.push_back(macro);
                    }
                }

                return result;
            }

            // -- Check for Pile of variants.
            Value variant_list = nil;
            if (consp(content) and symbolp(car(content))
                and symbol_name(car(content)) == u8"%Pile")
            {
                variant_list = cadr(content);
            }
            else
            {
                // -- Single variant or already a list.
                variant_list = variants_raw;
            }

            // -- Check for Enumeration variant:
            // ((%Call Enumeration ((%Tuple (c1 c2 ...)))))
            // > (DEFTYPE name () '(MEMBER c1 c2 ...))
            if (consp(variant_list) and null(cdr(variant_list)))
            {
                auto variant = car(variant_list);
                if (consp(variant) and is_symbol(car(variant), u8"%Call")
                    and symbolp(cadr(variant))
                    and symbol_name(cadr(variant)) == u8"Enumeration")
                {
                    auto vargs = caddr(variant);
                    // -- Extract constants from args.
                    std::vector<Value> csts;
                    if (consp(vargs) and null(cdr(vargs)))
                    {
                        auto inner = car(vargs);
                        Value elems = nil;
                        if (consp(inner)
                            and is_symbol(car(inner), u8"%Tuple"))
                            elems = cadr(inner);
                        else
                            elems = vargs;
                        for (Value e = elems; consp(e); e = cdr(e))
                        {
                            if (symbolp(car(e)))
                                csts.push_back(car(e));
                        }
                    }
                    if (not csts.empty())
                    {
                        auto struct_name = translate(tu, name_ast);
                        auto member_list = a.cons(sym(a, u8"MEMBER"),
                            a.list(csts));
                        auto quoted = a.list({
                            sym(a, u8"QUOTE"), member_list });
                        result.push_back(a.list({
                            sym(a, u8"DEFTYPE"),
                            struct_name, nil, quoted }));
                        return result;
                    }
                }
            }

            // -- Walk variants: each is a %Call node like
            // (%Call %Command %String) or
            // (%Call %Module (%Tuple %Symbol %List %List))
            for (Value v = variant_list; consp(v); v = cdr(v))
            {
                auto variant = car(v);
                if (not consp(variant)) continue;
                if (not symbolp(car(variant))) continue;

                auto vtag = symbol_name(car(variant));
                if (vtag != u8"%Call") continue;

                auto vname = cadr(variant);
                if (not symbolp(vname)) continue;

                // -- Count arguments from the third element.
                // vargs is a list (from span_list).
                // Single arg: (%String) > arity 1
                // Multi arg: ((%Tuple (%Symbol %List %List)))
                //   > unwrap > count tuple elements
                auto vargs = caddr(variant);
                int arity = 0;
                if (null(vargs))
                {
                    arity = 0;
                }
                else if (consp(vargs))
                {
                    auto first = car(vargs);
                    // -- Check if it's a single-element list
                    // wrapping a %Tuple node.
                    if (null(cdr(vargs)) and consp(first)
                        and symbolp(car(first))
                        and symbol_name(car(first)) == u8"%Tuple")
                    {
                        // -- Count elements in the tuple's
                        // children list.
                        for (Value e = cadr(first); consp(e);
                             e = cdr(e))
                            ++arity;
                    }
                    else
                    {
                        // -- Count list elements.
                        for (Value e = vargs; consp(e);
                             e = cdr(e))
                            ++arity;
                    }
                }
                else if (symbolp(vargs))
                {
                    arity = 1;
                }

                // -- Generate constructor DEFUN.
                std::vector<Value> params;
                for (int i = 0; i < arity; ++i)
                    params.push_back(tu.gensym());
                auto param_list = a.list(params);
                auto cons_body = a.list({
                    sym(a, u8"CONS"),
                    a.list({ sym(a, u8"QUOTE"), vname }),
                    a.cons(sym(a, u8"LIST"), param_list) });
                auto defun = a.list({
                    sym(a, u8"DEFUN"), vname, param_list,
                    cons_body });
                result.push_back(defun);
            }

            return result;
        }

        // -- %Lisp(string)
        if (tag == u8"%Lisp")
        {
            auto text = cadr(ast);
            return { a.list({ sym(a, u8"+LINE"), text }) };
        }

        // -- Default: translate as top-level expression.
        {
            auto expr = translate(tu, ast);
            expr = translate_form(a, expr);
            return { expr };
        }
    }

    // -- translate_file: full file translation

    std::vector<Value> translate_file(
        TranslationUnit& tu,
        const std::vector<Value>& top_levels)
    {
        auto& a = tu.arena;
        std::vector<Value> result;

        // -- Add preamble.
        result.push_back(a.list({
            sym(a, u8"PROCLAIM"),
            a.list({ sym(a, u8"QUOTE"),
                     a.list({ sym(a, u8"OPTIMIZE"),
                              sym(a, u8"SPEED") }) }) }));

        // -- Translate each top-level form.
        for (auto& tl : top_levels)
        {
            auto forms = translate_toplevel(tu, tl);
            for (auto f : forms)
                result.push_back(f);
        }

        return result;
    }

    // -- Pretty printer

    void pretty_print(std::ostream& os, Value form, int indent)
    {
        // -- For now, use the simple printer.
        Lisp::print(os, form);
    }
}
