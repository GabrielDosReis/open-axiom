// Copyright (C) 2011-2014, Gabriel Dos Reis.
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

#ifndef OPENAXIOM_VM_included
#define OPENAXIOM_VM_included

#include <open-axiom/storage>
#if HAVE_STDINT_H
#  include <stdint.h>
#endif 
#include <open-axiom/string-pool>
#include <utility>
#include <map>
#include <set>

#define internal_type struct alignas(16)
#define internal_data alignas(16)

namespace OpenAxiom {
   namespace VM {
      // --%
      // --% Value representation
      // --%
      // A far reaching design decision is that of providing a uniform
      // representation for values.  That is all values, irrespective
      // of type have fit in a fixed format, i.e. a scalar register.
      // This means that values that are more complicated than a scalar,
      // i.e. the vast majority and most interesting values, have to
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
      //   5. Ideally, we want efficient access to string literals
      // 
      // * Assumptions:
      //     (a) the host machine has sizeof(Value) quo 4 = 0.
      //     (b) allocated objects can be aligned on sizeof(Value) boundary.
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
      //   II. All foreign pointers that are aligned on 8-boundary are
      //       directly represented.  Any foreign pointer not meeting
      //       this condition is allocated in internal obejcts.  As a
      //       consequence, the last four bits of all foreign addresses 
      //       directly represented follow the pattern 0bx000.
      // 
      //  III. Cons cells are represented by their addresses with the
      //       last 4 bits matching the pattern 0bx010.
      //
      //   IV. All internal objects are allocated on 16-byte boundary.
      //       Their last 4 bits are set to the pattern 0b0110.
      //
      //    V. String literals are represented by their addressed with
      //       the last four bits following the pattern 0bx100..
      //
      // Finally:
      //   IV. The representation of a character shall have the last four
      //       bits set to 0b1110.
      //
      // Note: These choices do not fully satisfy constraint 4.  This is 
      //     because we restrict foreign pointers to address aligned
      //     to 8-byte boundaries.  A modest constraint.


      // -----------
      // -- Value --
      // -----------
      // All VM values fit in a universal value datatype.
      using ValueBits = uintptr_t;
      using ValueMask = ValueBits;
      using Value = ValueBits;

      // The distinguished `nil' value.
      constexpr Value nil { };

      // -------------
      // -- Fixnum ---
      // -------------
      // VM integers are divided into classes: small numbers,
      // and large numbers.  A small number fits entirely in a register.
      // A large number is allocated and represented by its address.
      using Fixnum = intptr_t;

      constexpr ValueBits fix_tag = 0x1;

      constexpr bool is_fixnum(Value v) {
         return (v & 0x1) == fix_tag;
      }

      constexpr Fixnum to_fixnum(Value v) {
         return Fixnum(v >> 1);
      }

      constexpr Value from_fixnum(Fixnum i) {
         return (Fixnum(i) << 1 ) | fix_tag;
      }

      constexpr Fixnum fixnum_maximum = to_fixnum(~Value{ });
      constexpr Fixnum fixnum_minimum = -fixnum_maximum - 1;

      // ------------
      // -- String --
      // ------------
      using String = BasicString;

      constexpr ValueBits str_tag = 0x4;

      constexpr bool is_string(Value v) {
         return (v & 0x7) == str_tag;
      }

      inline BasicString to_string(Value v) {
         return reinterpret_cast<BasicString>(v & ~Value(0x7));
      }

      inline Value from_string(BasicString s) {
         return Value(s) | str_tag;
      }

      inline BasicString to_string_if_can(Value v) {
         return is_string(v) ? to_string(v) : nullptr;
      }

      // -------------
      // -- Pointer --
      // -------------
      // Allocated objects are represented by their addresses.
      using Memory::Pointer;

      constexpr ValueBits ptr_tag = 0x0;

      constexpr bool is_pointer(Value v) {
         return (v & 0x7) == ptr_tag;
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
      };

      using Pair = ConsCell*;

      constexpr ValueBits pair_tag = 0x2;

      constexpr bool is_pair(Value v) {
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
      inline Pair to_pair_if_can(Value v) {
         return is_pair(v) ? to_pair(v) : nullptr;
      }

      Fixnum count_nodes(Pair);
      inline Fixnum count_nodes(Value v) {
         if (auto p = to_pair_if_can(v))
            return count_nodes(p);
         return 0;
      }

      // ---------------
      // -- Character --
      // ---------------
      // This datatype is prepared for Uncode characters even if
      // we do not handle UCN characters at the moment.
      enum class Character : ValueBits { };

      constexpr ValueBits char_tag = 0xE;

      constexpr bool is_character(Value v) {
         return (v & 0xF) == char_tag;
      }

      constexpr Character to_character(Value v) {
         return Character(v >> 4);
      }

      constexpr Value from_character(Character c) {
         return (Value(c) << 4) | char_tag;
      }

      // -- Object --
      // An object is a typed value.
      struct Type;
      struct Object {
         Value value;
         const Type* type;
      };

      // -------------
      // -- Dynamic --
      // -------------
      // Any internal value is of a class derived from this.
      internal_type Dynamic {
         virtual ~Dynamic();
      };

      constexpr ValueBits dyn_tag = 0x6;

      constexpr bool is_dynamic(Value v) {
         return (v & 0xF) == dyn_tag;
      }

      inline Dynamic* to_dynamic(Value v) {
         return reinterpret_cast<Dynamic*>(v & ~0xF);
      }

      inline Dynamic* to_dynamic_if_can(Value v) {
         return is_dynamic(v)
            ? reinterpret_cast<Dynamic*>(v & ~0xF)
            : nullptr;
      }

      inline Value from_dynamic(const Dynamic* o) {
         return Value(o) | dyn_tag;
      }

      struct Scope;
      
      // ------------
      // -- Symbol --
      // ------------
      struct Symbol : Dynamic, std::pair<String, Scope*> {
         Symbol(String, Scope*);
         String name() const { return first; }
         Scope* scope() const { return second; }
      };

      inline Symbol* to_symbol_if_can(Value v) {
         return dynamic_cast<Symbol*>(to_dynamic_if_can(v));
      }

      inline bool is_symbol(Value v) {
         return to_symbol_if_can(v) != nullptr;
      }

      inline Value from_symbol(const Symbol* s) {
         return from_dynamic(s);
      }

      // -----------
      // -- Scope --
      // -----------
      struct Scope : Dynamic, private std::map<Symbol*, Value> {
         explicit Scope(BasicString n) : id(n) { }
         BasicString name() const { return id; }
         Value* lookup(Symbol*) const;
         Value* define(Symbol*, Value);
      private:
         const BasicString id;
      };

      // --------------
      // -- Function --
      // --------------
      struct FunctionBase : Dynamic {
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
      using NullaryCode = Value (*)(BasicContext*);
      using UnaryCode = Value (*)(BasicContext*, Value);
      using BinaryCode = Value (*)(BasicContext*, Value, Value);
      using TernaryCode = Value (*)(BasicContext*, Value, Value, Value);

      template<typename Code>
      struct BuiltinFunction : FunctionBase {
         Code code;
         BuiltinFunction(Symbol n, Code c) : FunctionBase(n), code(c) { }
      };

      using NullaryOperator = BuiltinFunction<NullaryCode>;
      using UnaryOperator = BuiltinFunction<UnaryCode>;
      using BinaryOperator = BuiltinFunction<BinaryCode>;
      using TernaryOperator = BuiltinFunction<TernaryCode>;

      // ------------------
      // -- BasicContext --
      // ------------------
      // Provides basic evaluation services.
      struct BasicContext : StringPool {
         BasicContext();
         ~BasicContext();

         Pair make_pair(Value, Value);
         const Symbol* make_symbol(String, Scope*);
         const NullaryOperator* make_operator(Symbol, NullaryCode);
         const UnaryOperator* make_operator(Symbol, UnaryCode);
         const BinaryOperator* make_operator(Symbol, BinaryCode);
         const TernaryOperator* make_operator(Symbol, TernaryCode);

      protected:
         std::set<Symbol> syms;
         Memory::Factory<ConsCell> conses;
         Memory::Factory<NullaryOperator> nullaries;
         Memory::Factory<UnaryOperator> unaries;
         Memory::Factory<BinaryOperator> binaries;
         Memory::Factory<TernaryOperator> ternaries;
      };
   };
}

#endif  // OPENAXIOM_VM_INCLUDED

