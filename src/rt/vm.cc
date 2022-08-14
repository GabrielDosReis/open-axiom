// Copyright (C) 2011-2022, Gabriel Dos Reis.
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

// --% Author: Gabriel Dos Reis

#include <open-axiom/vm>
#include <iterator>
#include <algorithm>
#include <ostream>

namespace OpenAxiom {
   namespace VM {
      void Boxed::Visitor::visit(const NullaryOperator& x) {
         visit(as<FunctionBase>(x));
      }
      
      void Boxed::Visitor::visit(const UnaryOperator& x) {
         visit(as<FunctionBase>(x));
      }
      
      void Boxed::Visitor::visit(const BinaryOperator& x) {
         visit(as<FunctionBase>(x));
      }

      void FunctionBase::accept(Visitor& v) const { v.visit(*this); }
      
      // -- Environement
      Environment::Environment() = default;

      Environment::~Environment() {
         // Restore value of special variables bound in this environment.
         const auto end = dynamic.rend();
         for (auto p = dynamic.rbegin(); p != end; ++p)
            p->symbol->value = p->value;
      }

      Environment::Binding*
      Environment::lookup(InternedString name) {
         for (auto& b : lexical) {
            if (b.symbol->name == name)
               return &b;
         }
         return nullptr;
      }

      // -- Symbol
      Symbol::Symbol(InternedString s)
            : name(s),
              value(),
              function(),
              properties(),
              package(),
              attributes()
      { }

      void Symbol::accept(Visitor& v) const { v.visit(*this); }

      // -- Binding
      void Binding::accept(Visitor& v) const { v.visit(*this); }

      // -- Package
      Package::Package(InternedString s)
            : name(s)
      { }

      Symbol*
      Package::make_symbol(InternedString s) {
         auto sym = const_cast<Symbol*>(&*symbols.insert(Symbol(s)).first);
         sym->package = this;
         return sym;
      }

      Symbol*
      Package::find_symbol(InternedString s) {
         auto p = symbols.find(Symbol(s));
         return p == symbols.end() ? nullptr : const_cast<Symbol*>(&*p);
      }

      void Package::accept(Visitor& v) const { v.visit(*this); }

      Fixnum
      count_nodes(Pair p) {
         FixnumBits n = 1;
         for (; auto q = if_pair(p->tail); p = q)
            ++n;
         return Fixnum(n);
      }


      // -- BasicContext --
      Package*
      BasicContext::make_package(InternedString n) {
         auto p = &*packages.insert(Package(n)).first;
         return const_cast<Package*>(p);
      }

      Symbol*
      BasicContext::make_keyword(InternedString n) {
         auto sym = keyword_package()->make_symbol(n);
         sym->value = to_value(sym);
         sym->attributes = SymbolAttribute::Keyword;
         return sym;
      }

      Pair BasicContext::make_pair(Value h, Value t) {
         return conses.make(h, t);
      }

      const NullaryOperator*
      BasicContext::make_operator(Symbol* n, NullaryCode c) {
         return setf_symbol_function(n, nullaries.make(n, c));
      }
      
      const UnaryOperator*
      BasicContext::make_operator(Symbol* n, UnaryCode c) {
         return setf_symbol_function(n, unaries.make(n, c));
      }
      
      const BinaryOperator*
      BasicContext::make_operator(Symbol* n, BinaryCode c) {
         return setf_symbol_function(n, binaries.make(n, c));
      }
      
      const TernaryOperator*
      BasicContext::make_operator(Symbol* n, TernaryCode c) {
         return setf_symbol_function(n, ternaries.make(n, c));
      }
      
      BasicContext::BasicContext()
            : keywords(make_package(intern("KEYWORD"))),
              homeless(make_package(nullptr))
      {
      }

      BasicContext::~BasicContext() {
      }
   }
}
