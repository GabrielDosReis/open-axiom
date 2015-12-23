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

#ifndef OPENAXIOM_CONSTRUCTOR_included
#define OPENAXIOM_CONSTRUCTOR_included

#include <open-axiom/string-pool>

namespace OpenAxiom {
   namespace VM {
      // -- identifier
      using Identifier = String;

      // -- A constructor kind.
      enum class ConstructorKind { category, domain, package };
      
      // -- Data structure for a constructor.
      struct Constructor {
         Identifier name;
         Identifier abbrev;
         String source_file;
         String object_file;
         ConstructorKind kind;
      };
   }
}

// -- specialize helper function objects
namespace std {
   template<>
   struct hash<OpenAxiom::VM::Constructor> {
      using H = hash<OpenAxiom::VM::Identifier>;
      H::result_type operator()(const Constructor& c) const {
         return h(c.name);
      }

      H h;
   };

   template<>
   struct equal_to<OpenAxiom::VM::Constructor> {
      using E = equal_to<OpenAxiom::VM::Identifier>;
      using arg_type = OpenAxiom::VM::Constructor;
      bool operator()(const arg_type& x, const arg_type& y) const {
         return e(x.name, y.name);
      }
      E e;
   };
}

#endif  // OPENAXIOM_CONSTRUCTOR_included
