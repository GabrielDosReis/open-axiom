// Copyright (C) 2011-2012, Gabriel Dos Reis.
// All rights reserved.
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
// --% Description:
// --%   Interface and implementation of basic services of the 
// --%   OpenAxiom Virtual Machine.

#ifndef OPENAXIOM_VM_INCLUDED
#define OPENAXIOM_VM_INCLUDED

#include <open-axiom/storage>
#if HAVE_STDINT_H
#  include <stdint.h>
#endif 
#include <open-axiom/string-pool>
#include <utility>
#include <map>

#define internal_type struct openaxiom_alignas(16)
#define internal_data  openaxiom_alignas(16)

namespace OpenAxiom {
   namespace VM {
      // --%
      // --% Value representation
      // --%
      // A far reaching design decision is to provide a uniform
      // representation for values.  That is all values, irrespective
      // of type have fit in a fixed format, i.e. a scalar register.
      // This means that values that are more complicated than a scalar,
      // that is the vast majority and most interesting values, have to
      // be stored in allocated objects and addresses of their container
      // objects used in place of the actual values.  This is folklore
      // in the communities of garbage collected languages.
      // 
      // An unfortunate but widely held belief is that AXIOM-based
      // systems (and computer algebra systems in general) are
      // Lisp-based systems.  Nothing could be further from the truth
      // for OpenAxiom.  The type system is believed to support
      // erasure semantics, at least for values.
      //
      // However the current implementation, being Lisp-based,
      // unwittingly makes use of some Lisp features that are not
      // strictly necessary.  It would take a certain amount of effort
      // to get rid of them.  Consequently, we must cope -- at least
      // for now -- with the notion of uniform value representation and
      // use runtime predicates to descriminate between values.
      // On the other hand, we do not want to carry an unduly expensive
      // abstraction penalty for perfectly well behaved and well
      // disciplined programs.  So, here are a few constraints:
      //   1. Small integers should represent themselves -- not allocated.
      //      Furthermore, the maximum range should be sought where possible.
      //   2. Since we have to deal with characters, they should be
      //      directly represented -- not allocated.
      //   3. List values and list manipulation should be efficient.
      //      Ideally, a pair should occupy no more than what it
      //      takes to store two values in a type-erasure semantics.
      //   4. Idealy, pointers to foreign objects (at least) should be
      //      left unmolested.
      // 
      // * Assumptions:
      //     (a) the host machine has sizeof(Value) quo 4 = 0.
      //     (b) allocatd objects can be aligned on sizeof(Value) boundary.
      //     (c) the host machine has 2's complement arithmetic.
      //
      // If:
      //   -- we use a dedicated allocation pool for cons cells
      //   -- we allocate the first cell in each cons-storage arena
      //      on a 8-byte boundary
      //   -- we use exactly 2 * sizeof(Value) to store a cons cell
      //      therefore realizing constraint (3)
      // then:
      //   every pointer to a cons cell will have its last 3 bits cleared.
      //
      // Therefore, we can use the last 3 bits to tag a cons value, instead
      // of storing the tag inside the cons cell.  We can't leave those
      // bits cleared for we would not be able to easily and cheaply
      // distinguish a pointer to a cons cell from a pointer to other
      // objects, in particular foreign objects.
      //
      // To meet constraint (1), we must logically use at least one bit
      // to distinguish a small integer from a pointer to a cons cell.
      // The good news is that we need no more than that if pointers
      // to foreign pointers do not have the last bit set.  Which is
      // the case with assumption (a).  Furthermore, if we align all
      // other internal data on 16 byte boundary, then we have 4 spare bits
      // for use to categorize values.
      // Therefore we arrive at the first design:
      //    I. the value representation of a small integer always has the
      //       the least significant bit set.  All other bits are
      //       significant.  In other words, the last four bits of a small
      //       integer are 0bxxx1
      // 
      // As a consequence, the last bit of all other values must be cleared.
      //
      // Next,
      //   II. All foreign pointers must have the last two bits cleared.
      //       As a consequence, the last four bits of all foreign addresses
      //       follow the pattern 0bxx00.
      // 
      // As a consequence, the second bit of a cons cell value must be set
      // so that we can distinguish it from foreign pointers.
      // 
      //  III. Cons cells are represented by their addresses with the
      //       last 4 bits matching the pattern 0bx010.
      //
      //   IV. All internal objects are allocated on 16-byte boundary.
      //       Their last 4 bits are set to the pattern 0b0110.
      //
      // Finally:
      //    V. The representation of a character shall have the last four
      //       bits set to 0b1110.
      //
      // Note: These choices do not fully satisfy constraint 4.  This is 
      //     because we restrict foreign pointers to address aligned
      //     to 4-byte boundaries.


      // -----------
      // -- Value --
      // -----------
      // All VM values fit in a universal value datatype.
      typedef uintptr_t Value;
      const Value nil = Value();

      // -------------
      // -- Fixnum ---
      // -------------
      // VM integers are divided into classes: small numbers,
      // and large numbers.  A small number fits entirely in a register.
      // A large number is allocated and represented by its address.
      typedef intptr_t Fixnum;

      const Value fix_tag = 0x1;

      inline bool is_fixnum(Value v) {
         return (v & 0x1) == fix_tag;
      }

      inline Fixnum to_fixnum(Value v) {
         return Fixnum(v >> 1);
      }

      inline Value from_fixnum(Fixnum i) {
         return (Fixnum(i) << 1 ) | fix_tag;
      }

      // -------------
      // -- Pointer --
      // -------------
      // Allocated objects are represented by their addresses.
      using Memory::Pointer;

      const Value ptr_tag = 0x0;

      inline bool is_pointer(Value v) {
         return (v & 0x3) == ptr_tag;
      }

      inline Pointer to_pointer(Value v) {
         return Pointer(v);
      }

      inline Value from_pointer(Pointer p) {
         return Value(p);
      }

      // ----------
      // -- Pair --
      // ----------
      struct ConsCell {
         Value head;
         Value tail;
         ConsCell(Value h, Value t) : head(h), tail(t) { }
      };

      typedef ConsCell* Pair;

      const Value pair_tag = 0x2;

      inline bool is_pair(Value v) {
         return (v & 0x7) == pair_tag;
      }

      inline Pair to_pair(Value v) {
         return Pair(v & ~0x7);
      }

      inline Value from_pair(Pair p) {
         return Value(p) | pair_tag;
      }

      // If `v' designates a pair, return a pointer to its
      // concrete representation.
      inline Pair pair_if_can(Value v) {
         return is_pair(v) ? to_pair(v) : 0;
      }

      // -- List<T> --
      // There is no dedicated list type.  Any pair that ends with
      // nil is considered a list.  Similarly, the notion of homogeneous
      // list is dynamic.
      template<typename T>
      struct List : ConsCell {
         List<T> rest() const {
            return static_cast<List<T>*>(pair_if_can(tail));
         }
      };

      // ---------------
      // -- Character --
      // ---------------
      // This datatype is prepared for Uncode characters even if
      // we do not handle UCN characters at the moment.
      typedef Value Character;

      const Value char_tag = 0xE;

      inline bool is_character(Value v) {
         return (v & 0xF) == char_tag;
      }

      inline Character to_character(Value v) {
         return Character(v >> 4);
      }

      inline Value from_character(Character c) {
         return (Value(c) << 4) | char_tag;
      }

      // ------------
      // -- Object --
      // ------------
      // Any internal object is of a class derived from this.
      internal_type BasicObject {
         Value kind;
      };

      typedef BasicObject* Object;

      const Value obj_tag = 0x6;

      inline bool is_object(Value v) {
         return (v & 0xF) == obj_tag;
      }

      inline Object to_object(Value v) {
         return Object(v & ~0xF);
      }

      inline Value from_object(Object* o) {
         return Value(o) | obj_tag;
      }

      // ------------
      // -- Symbol --
      // ------------
      struct SymbolObject : BasicObject, std::pair<BasicString, Value> {
         SymbolObject(BasicString n, Value s = nil)
               : std::pair<BasicString, Value>(n, s) { }
         BasicString name() const { return first; }
         Value scope() const { return second; }
      };

      typedef SymbolObject* Symbol;

      // -----------
      // -- Scope --
      // -----------
      struct ScopeObject : BasicObject, private std::map<Symbol, Value> {
         explicit ScopeObject(BasicString n) : id(n) { }
         BasicString name() const { return id; }
         Value* lookup(Symbol) const;
         Value* define(Symbol, Value);
      private:
         const BasicString id;
      };

      typedef ScopeObject* Scope;

      // --------------
      // -- Function --
      // --------------
      struct FunctionBase : BasicObject {
         const Symbol name;
         Value type;
         FunctionBase(Symbol n, Value t = nil)
               : name(n), type(t) { }
      };

      // ------------------------
      // -- Builtin Operations --
      // ------------------------
      // Types for native implementation of builtin operators.
      struct BasicContext;
      typedef Value (*NullaryCode)(BasicContext*);
      typedef Value (*UnaryCode)(BasicContext*, Value);
      typedef Value (*BinaryCode)(BasicContext*, Value, Value);
      typedef Value (*TernaryCode)(BasicContext*, Value, Value, Value);

      template<typename Code>
      struct BuiltinFunction : FunctionBase {
         Code code;
         BuiltinFunction(Symbol n, Code c) : FunctionBase(n), code(c) { }
      };

      typedef BuiltinFunction<NullaryCode> NullaryOperatorObject;
      typedef NullaryOperatorObject* NullaryOperator;

      typedef BuiltinFunction<UnaryCode> UnaryOperatorObject;
      typedef UnaryOperatorObject* UnaryOperator;
      
      typedef BuiltinFunction<BinaryCode> BinaryOperatorObject;
      typedef BinaryOperatorObject* BinaryOperator;

      typedef BuiltinFunction<TernaryCode> TernaryOperatorObject;
      typedef TernaryOperatorObject* TernaryOperator;

      // ------------------
      // -- BasicContext --
      // ------------------
      // Provides basic evaluation services.
      struct BasicContext : StringPool {
         BasicContext();

         Pair make_cons(Value, Value);
         NullaryOperator make_operator(Symbol, NullaryCode);
         UnaryOperator make_operator(Symbol, UnaryCode);
         BinaryOperator make_operator(Symbol, BinaryCode);
         TernaryOperator make_operator(Symbol, TernaryCode);

      protected:
         Memory::Factory<ConsCell> conses;
         Memory::Factory<NullaryOperatorObject> nullaries;
         Memory::Factory<UnaryOperatorObject> unaries;
         Memory::Factory<BinaryOperatorObject> binaries;
         Memory::Factory<TernaryOperatorObject> ternaries;
      };
   };
}

#endif  // OPENAXIOM_VM_INCLUDED

