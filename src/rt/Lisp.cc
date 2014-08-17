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
//     - Neither the name of OpenAxiom nor the names of its contributors
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

#include <open-axiom/Lisp>
#include <typeinfo>
#include <ostream>
#include <sstream>

namespace OpenAxiom {
   namespace Lisp {
      Unimplemented::Unimplemented(const std::string& s)
            : BasicError(s)
      { }

      IntegerOverflow::IntegerOverflow(const std::string& s)
            : BasicError(s)
      { }

      std::string
      show(Value v) {
         std::ostringstream os;
         format(v, os);
         return os.str();
      }

      Fixnum
      retract_to_fixnum(Value v) {
         if (not is_fixnum(v))
            throw Diagnostics::BasicError(show(v) + " is not a fixnum");
         return to_fixnum(v);
      }

      Pair
      retract_to_pair(Value v) {
         if (not is_pair(v))
            throw Diagnostics::BasicError(show(v) + " is not a pair");
         return to_pair(v);
      }


      static void
      unimplemented(const Sexpr::Syntax& x) {
         std::string s = "unimplemented eval for ";
         throw Unimplemented{ s + typeid(x).name() };
      }

      static void
      integer_too_large(const Sexpr::IntegerSyntax& x) {
         std::string s { x.lexeme().begin(), x.lexeme().end() };
         throw IntegerOverflow{ s + " is too large for Fixnum; max value is "
               + std::to_string(FixnumBits(Fixnum::maximum)) };
      }

      constexpr auto fixmax_by_ten = FixnumBits(Fixnum::maximum) / 10;
      constexpr auto fixmax_lsd = FixnumBits(Fixnum::maximum) % 10;

      static Value
      construct(Evaluator* ctx, const Sexpr::IntegerSyntax& x) {
         bool neg = false;
         auto cur = x.lexeme().begin();
         FixnumBits val = 0;
         switch (*cur) {
         case '-': neg = true;
         case '+': ++cur;
         default:
            for (; cur < x.lexeme().end(); ++cur) {
               auto d = *cur - '0';
               if (val < fixmax_by_ten)
                  val = 10 * val + d;
               else if (val > fixmax_by_ten or d > fixmax_lsd)
                  integer_too_large(x);
               else
                  val = 10 * val + d;
            }
            if (neg) {
               if (val > FixnumBits(Fixnum::maximum))
                  integer_too_large(x);
               val = -val;
            }
         }
         return VM::from_fixnum(Fixnum(val));
      }

      static Value
      construct(Evaluator* ctx, const Sexpr::ListSyntax& x) {
         if (x.empty())
            return nil;
         auto result = nil;
         auto p = x.rbegin();
         if (x.dotted())
            result = ctx->make_value(*p++);
         while (p != x.rend())
            result = from_pair(ctx->make_pair(ctx->make_value(*p++), result));
         return result;
      }

      static Value
      construct(Evaluator* ctx, const Sexpr::StringSyntax& x) {
         auto s = ctx->intern(x.lexeme().begin(), x.lexeme().size());
         return from_string(s);
      }

      static Value
      construct(Evaluator* ctx, const Sexpr::SymbolSyntax& x) {
         auto s = ctx->intern(x.lexeme().begin(), x.lexeme().size());
         switch (x.kind()) {
         case Sexpr::SymbolSyntax::uninterned:
            return from_symbol(ctx->make_symbol(s, nullptr));

         case Sexpr::SymbolSyntax::keyword:
            return from_symbol(ctx->make_symbol(s, ctx->keyword_namespace()));

         default:
            return from_symbol(ctx->make_symbol(s, ctx->active_namespace()));
         }
      }

      VM::Value
      Evaluator::make_value(const Sexpr::Syntax* x) {
         using namespace Sexpr;
         struct V : Sexpr::Syntax::Visitor {
            Evaluator* ctx;
            Value result;
            V(Evaluator* e) : ctx(e), result(nil) { }
            void visit(const IntegerSyntax& x) { result = construct(ctx, x); }
            void visit(const CharacterSyntax& x) { unimplemented(x); }
            void visit(const StringSyntax& x) { result = construct(ctx, x); }
            void visit(const SymbolSyntax& x) { result = construct(ctx, x); }
            void visit(const ReferenceSyntax& x) {
               auto p = ctx->anchor_map.find(x.tag());
               if (p == ctx->anchor_map.end())
                  throw Diagnostics::BasicError{ "undefined anchor "
                        + std::to_string(x.tag())
                        };
               result = p->second;
            }
            void visit(const AnchorSyntax& x) {
               auto& v = ctx->anchor_map[x.ref()];
               if (v != nil)
                  throw Diagnostics::BasicError{
                     "duplicate anchor " + std::to_string(x.ref())
                        };
               result = v = ctx->make_value(x.value());
            }
            void visit(const QuoteSyntax& x) { unimplemented(x); }
            void visit(const AntiquoteSyntax& x) { unimplemented(x); }
            void visit(const Expand& x) { unimplemented(x); }
            void visit(const Eval& x) { unimplemented(x); }
            void visit(const Splice& x) { unimplemented(x); }
            void visit(const Function& x) { unimplemented(x); }
            void visit(const Include& x) { unimplemented(x); }
            void visit(const Exclude& x) { unimplemented(x); }
            void visit(const ListSyntax& x) { result = construct(ctx, x); }
            void visit(const VectorSyntax& x) { unimplemented(x); }
         };

         if (x == nullptr)
            return nil;
         V v { this };
         x->accept(v);
         return v.result;
      }

      Value
      Evaluator::toplevel_form(const Sexpr::Syntax* x) {
         auto anchors = std::move(anchor_map);
         anchor_map = AnchorTable{ };
         auto v = make_value(x);
         anchor_map = std::move(anchors);
         return v;
      }

      Evaluator::Evaluator() : keys(intern("KEYWORD")), ns() {
         env_stack.push_back(Environment{ });
      }

      Environment*
      Evaluator::global_environment() {
         return &env_stack.front();
      }


      // -- Formatting

      static void format(Pair p, std::ostream& os) {
         os << '(';
         while (true) {
            format(p->head, os);
            auto v = p->tail;
            if (v == nil)
               break;
            os << ' ';
            if (auto q = to_pair_if_can(v)) {
               p = q;
               continue;
            }
            os << '.' << ' ';
            format(v, os);
            break;
         }
         os << ')';
      }

      static void format(String s, std::ostream& os) {
         os << '"';
         for (auto c : *s) {
            if (c == '"')
               os << '\\';
            os << char(c);
         }
         os << '"';
      }

      static void format(const Symbol* s, std::ostream& os) {
         // FIXME: Handle escapes.
         auto n = s->name();
         std::copy(n->begin(), n->end(), std::ostream_iterator<char>(os));
      }
      
      void format(Value v, std::ostream& os) {
         if (v == nil)
            os << "NIL";
         else if (is_fixnum(v))
            os << FixnumBits(to_fixnum(v));
         else if (auto p = to_pair_if_can(v))
            format(p, os);
         else if (auto s = to_string_if_can(v))
            format(s, os);
         else if (auto s = to_symbol_if_can(v))
            format(s, os);
         else
            os << "<unprintable>";
      }

      // -- assoc: (T, List Pair(T, S)) -> S
      Value assoc(Value key, Pair al) {
         while (al != nullptr) {
            auto entry = retract_to_pair(al->head);
            if (entry->head == key)
               return entry->tail;
            else if (al->tail == nil)
               return nil;
            al = retract_to_pair(al->tail);
         }
         return nil;
      }
   }
}
