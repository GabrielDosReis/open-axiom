// Copyright (C) 2013-2014, Gabriel Dos Reis.
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
// --% Abstract:
// --%    Very simple support for some core Lisp-like operations.

#ifndef OPENAXIOM_LISP_included
#define OPENAXIOM_LISP_included

#include <list>
#include <open-axiom/sexpr>
#include <open-axiom/vm>
#include <map>
#include <open-axiom/diagnostics>
#include <iosfwd>
#include <unordered_set>

namespace std {
   template<>
   struct hash<OpenAxiom::VM::Package>  {
      hash<OpenAxiom::VM::String>::result_type
      operator()(const OpenAxiom::VM::Package& s) const {
         return h(s.name);
      }
      hash<OpenAxiom::VM::String> h;
   };

   template<>
   struct equal_to<OpenAxiom::VM::Package> {
      using arg_type = OpenAxiom::VM::Package;
      bool operator()(const arg_type& x, const arg_type& y) const {
         constexpr equal_to<OpenAxiom::VM::String> eq { };
         return eq(x.name, y.name);
      }
   };
}

namespace OpenAxiom {
   namespace Lisp {
      using namespace VM;

      // -- Unimplemented features
      struct Unimplemented : Diagnostics::BasicError {
         explicit Unimplemented(const std::string&);
      };

      // -- Integer overflow
      struct IntegerOverflow : Diagnostics::BasicError {
         explicit IntegerOverflow(const std::string&);
      };

      // -- Unbound symbol
      struct UnboundSymbol : Diagnostics::BasicError {
         explicit UnboundSymbol(const std::string&);
      };

      // -- Unbound functiom symbol
      struct UnboundFunctionSymbol : Diagnostics::BasicError {
         explicit UnboundFunctionSymbol(const std::string&);
      };
      
      // -- Anchor maps
      using AnchorTable = std::map<Ordinal, Value>;

      // -- Evaluator --
      struct Evaluator : VM::BasicContext {
         Evaluator();
         Value eval(const Sexpr::Syntax*);
         Package* core_package() { return core; }
         Package* current_package() { return ns; }
         Value toplevel_form(const Sexpr::Syntax*);
         Value make_value(const Sexpr::Syntax*);
         Value* lexical_binding(String);
         Environment* global_environment();
      private:
         Package* core;
         Package* ns;
         Symbol* feature_list;
         std::list<Environment> env_stack;
         AnchorTable anchor_map;
      };

      // -- Format a value onto an output stream.
      void format(Value, std::ostream&);
      std::string show(Value);

      // -- Retracts
      Fixnum retract_to_fixnum(Value);
      Pair retract_to_pair(Value);
      Symbol* retract_to_symbol(Value);

      // -- Alist functions
      Value assoc(Value, Pair);
   }
}

#endif  // OPENAXIOM_LISP_included

