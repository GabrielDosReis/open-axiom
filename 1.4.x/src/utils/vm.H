// Copyright (C) 2011, Gabriel Dos Reis.
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
      // in the communities of garbage collected language implementations.
      // An unfortunate but widely held belief is that AXIOM-based
      // systems (and computer algebra systems in general) are
      // Lisp-based systems.  Nothing could be further from the truth
      // for OpenAxiom.  The type system is believed to support
      // erasure semantics, at least for values.
      //
      // However,the current implementation being Lisp-based, it does
      // unwittingly make use of some Lisp features that are not
      // strictly necessary, but which would take a certain amount
      // of effort to get rid of.  Consequently, we must cope -- at least
      // for now -- with the notion of uniform value representation and
      // use runtime predicates to descriminate between values.
      // However, we do not want to carry an unduly expensive abstraction
      // penalty for perfectly well behaved and well disciplined programs.
      // Here are a few constraints:
      //   1. Small integers should represent themselves, not allocated.
      //      Furthermore, the maximum range should be sought.
      //   2. Since we have to deal with characters, they should be
      //      directly represented.
      //   3. List values and list manipulation should be efficient.
      //      Ideally, a pair should not occupy more than what it
      //      takes to store two values.
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
      //      therefore realizing constraint (1)
      // then:
      //   each pointer to a cons cell will have its last 3 bits cleared.
      //
      // Therefore, we can use the last 3 bits to tag a cons value, instead
      // of storing the tag inside the cons cell.  we can't leave those
      // bits cleared for we would not be able to easily and cheaply
      // distinguish a pointer to a cons cell from a pointer to other objects.
      //
      // To meet constraint (1), we must logically use at least one bit
      // to distinguish a small integer from a pointer to a cons cell.
      // The good news is that we need not need more than that if pointers
      // to foreign pointers do not have the last bit set.  Which is
      // pretty much the case in practice as crystallized in assumption (a).
      // Therefore we arrive at the first design:
      //    I. the value representation of small integer always have the
      //       the least significant bit set.  All other bits are
      //       significant in representing small integers.
      // As a consequence, the last bit of all other values must be cleared.
      //
      // Next, cons cell values:
      //    II. Cons cells are represented by their addresses with the
      //        last 3 bits set to 0b110.
      //
      //    III. Any allocated object obj shall saisfy sizeof obj quo  4 = 0.
      //         All allocated objects shall have alignment divisible by 4.
      //         In consequence, the last two bits of object addresses
      //         are cleared, i.e. 0b00.
      // Finally:
      //    IV.  The representation of a character shall have the last two
      //         bits set to 0b10.
      //
      // Note: These choices do not satisfy constraint 4.  If we could
      //     restrict pointer to foreign objects to address aligned
      //     to multiple of 4 boundaries, then we can make different choices
      //     that represents foreign address directly.
      //     In the current design, foriegn addresses are allocated.


      // -----------
      // -- Value --
      // -----------
      // All VM values fit in a universal value datatype.
      typedef uintptr_t Value;

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

      // ---------------
      // -- Character --
      // ---------------

      // This datatype is prepared for Uncode characters even if
      // we do not handle UCN characters.
      typedef Value Character;

      const Value char_tag = 0x2;

      inline bool is_character(Value v) {
         return (v & 0x3) == char_tag;
      }

      inline Character to_character(Value v) {
         return Character(v >> 2);
      }

      inline Value from_character(Character c) {
         return (Value(c) << 2) | char_tag;
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

      const Value pair_tag = 0x6;

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

      // --
      // -- Builtin Operations
      // --
      // Types for native implementation of builtin operators.
      struct BasicContext;
      typedef Value (*NullaryCode)(BasicContext*);
      typedef Value (*UnaryCode)(BasicContext*, Value);
      typedef Value (*BinaryCode)(BasicContext*, Value, Value);
      typedef Value (*TernaryCode)(BasicContext*, Value, Value, Value);

      // ------------
      // -- Object --
      // ------------
      template<typename T>
      struct Object {
         Value dict;
         T data;
      };

      // ------------------
      // -- BasicContext --
      // ------------------
      // Provides basic evaluation services.
      struct BasicContext : StringPool {
         BasicContext();

         Pair make_cons(Value, Value);

      protected:
         Memory::Factory<ConsCell> conses;
      };
   };
}

#endif  // OPENAXIOM_VM_INCLUDED

