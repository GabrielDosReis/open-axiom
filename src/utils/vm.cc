// Copyright (C) 2011-2013, Gabriel Dos Reis.
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

namespace OpenAxiom {
   namespace VM {
      Dynamic::~Dynamic() { }

      Symbol::Symbol(String n, Scope* s)
            : std::pair<String, Scope*>(n, s)
      { }

      Fixnum
      count_nodes(Pair p) {
         Fixnum n = 1;
         for (; auto q = to_pair_if_can(p->tail); p = q)
            ++n;
         return n;
      }

      // -- BasicContext --
      Pair BasicContext::make_pair(Value h, Value t) {
         return conses.make(h, t);
      }

      const Symbol*
      BasicContext::make_symbol(String n, Scope* s) {
         return &*syms.insert({ n, s }).first;
      }

      const NullaryOperator*
      BasicContext::make_operator(Symbol n, NullaryCode c) {
         return nullaries.make(n, c);
      }
      
      const UnaryOperator*
      BasicContext::make_operator(Symbol n, UnaryCode c) {
         return unaries.make(n, c);
      }
      
      const BinaryOperator*
      BasicContext::make_operator(Symbol n, BinaryCode c) {
         return binaries.make(n, c);
      }
      
      const TernaryOperator*
      BasicContext::make_operator(Symbol n, TernaryCode c) {
         return ternaries.make(n, c);
      }
      
      BasicContext::BasicContext() {
      }

      BasicContext::~BasicContext() {
      }
   }
}
