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
#include <open-axiom/string-pool>
#include <stdint.h>
#include <utility>
#include <set>
#include <vector>
#include <type_traits>

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
      //       this condition is stored in an internal object.  As a
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
      //
      // Special Constants:
      //     NIL    0x00
      //     T      0x10


      // -----------
      // -- Value --
      // -----------
      // All VM values fit in a universal value datatype.
      using ValueBits = uintptr_t;
      using ValueMask = ValueBits;
      enum class Value : ValueBits {
         nil = 0x00,            // distinguished NIL value
         t   = 0x10,            // distinguished T value
      };

      // -- Testing for nil value.
      constexpr Value null(Value v) {
         return v == Value::nil ? Value::t : Value::nil;
      }

      // -- Convert VM Boolean value to C++ view
      constexpr bool to_bool(Value v) { return v != Value::nil; }

      // -- Convert a C++ Boolean value to VM view.
      constexpr Value to_value(bool b) {
         return b ? Value::t : Value::nil;
      }

      // -- Identity equality.
      constexpr Value eq(Value x, Value y) { return to_value(x == y); }

      template<typename>
      struct ValueTrait {
      };

      // Return the tag of an abstract value, when viewed as a potential
      // T-value.
      template<typename T>
      constexpr ValueBits tag(Value v) {
         return ValueBits(v) & ValueTrait<T>::tag_mask;
      }

      // Return true if the abstract value is, in fact, a T-value.
      template<typename T>
      constexpr bool is(Value v) {
         return tag<T>(v) == ValueTrait<T>::tag;
      }

      // Return the pristine bits of an abstract value without its tag.
      template<typename T>
      constexpr ValueBits native(Value v) {
         return ValueBits(v) & ~ValueTrait<T>::tag_mask;
      }

      // -- Arity: number of arguments or forms taken by a function
      //           or a special operator.
      enum class Arity : intptr_t {
         variable = -1,         // Any number of arguments.
         zero     = 0,          // Exactly no argument.
         one      = 1,          // Exactly one argument.
         two      = 2,          // Exactly two arguments.
         three    = 3,          // Exactly three arguments.
      };

      // -------------
      // -- Dynamic --
      // -------------
      // Any internal value is of a class derived from this.
      internal_type Dynamic {
         struct Visitor;
         virtual ~Dynamic();
         virtual void accept(Visitor&) const = 0;
      };

      // Provide an S-view of a T-typed expression, assuming the type T
      // derives from S.
      template<typename S, typename T>
      inline const S& as(const T& t) { return t; }

      template<>
      struct ValueTrait<Dynamic> {
         enum Tag : ValueBits { tag = 0x6 };
         enum Mask : ValueBits { tag_mask = 0xF };
      };

      inline Dynamic* to_dynamic(Value v) {
         return reinterpret_cast<Dynamic*>(native<Dynamic>(v));
      }

      inline Dynamic* to_dynamic_if_can(Value v) {
         return is<Dynamic>(v) ? to_dynamic(v) : nullptr;
      }

      template<typename T>
      using IfDynamic = typename
         std::enable_if<std::is_base_of<Dynamic, T>::value, Value>::type;

      template<typename T>
      inline IfDynamic<T> to_value(const T* o) {
         return Value(ValueBits(o) | ValueTrait<Dynamic>::tag);
      }

      // -- Callable --
      struct Callable : Dynamic {
      };
      
      // -------------
      // -- Fixnum ---
      // -------------
      // VM integers are divided into classes: small numbers,
      // and large numbers.  A small number fits entirely in a register.
      // A large number is allocated and represented by its address.
      using FixnumBits = intptr_t;
      enum class Fixnum : FixnumBits {
         minimum = FixnumBits(~(~ValueBits() >> 2)),
         zero = FixnumBits(0),
         one = FixnumBits(1),
         maximum = FixnumBits(~ValueBits() >> 2),
      };

      template<>
      struct ValueTrait<Fixnum> {
         enum Tag : ValueBits { tag = 0x1  };
         enum Mask : ValueBits { tag_mask = 0x1 };
      };

      constexpr Fixnum to_fixnum(Value v) {
         return Fixnum(FixnumBits(v) >> 1);
      }

      constexpr Value from_fixnum(Fixnum i) {
         return Value((ValueBits(i) << 1 ) | ValueTrait<Fixnum>::tag);
      }

      // ------------
      // -- String --
      // ------------
      using String = InternedString;

      template<>
      struct ValueTrait<String> {
         enum Tag : ValueBits { tag = 0x4 };
         enum Mask : ValueBits { tag_mask = 0x7 };
      };

      inline InternedString to_string(Value v) {
         return reinterpret_cast<String>(native<String>(v));
      }

      inline Value from_string(InternedString s) {
         return Value(ValueBits(s) | ValueTrait<String>::tag);
      }

      inline InternedString to_string_if_can(Value v) {
         return is<String>(v) ? to_string(v) : nullptr;
      }

      // -------------
      // -- Pointer --
      // -------------
      // Allocated objects are represented by their addresses.
      using Memory::Pointer;

      template<>
      struct ValueTrait<Memory::Pointer> {
         enum Tag : ValueBits { tag = 0x0 };
         enum Mask : ValueBits { tag_mask = 0x7 };
      };

      inline Pointer to_pointer(Value v) {
         return Pointer(ValueBits(v));
      }

      inline Value from_pointer(Pointer p) {
         return Value(ValueBits(p) | ValueTrait<Memory::Pointer>::tag);
      }

      // ----------
      // -- Pair --
      // ----------
      struct alignas(8) ConsCell {
         Value head;
         Value tail;
      };

      using Pair = ConsCell*;

      template<>
      struct ValueTrait<Pair> {
         enum Tag : ValueBits { tag = 0x2 };
         enum Mask : ValueBits { tag_mask = 0x7 };
      };

      inline Pair to_pair(Value v) {
         return reinterpret_cast<Pair>(native<Pair>(v));
      }

      inline Value from_pair(Pair p) {
         return Value(ValueBits(p) | ValueTrait<Pair>::tag);
      }

      // Return true if argument designates a pair.
      constexpr Value consp(Value v) {
         return to_value(v != Value::nil and v != Value::t and is<Pair>(v));
      }

      inline Value atom(Value v) {
         return null(consp(v));
      }

      // If `v' designates a pair, return a pointer to its
      // concrete representation.
      inline Pair to_pair_if_can(Value v) {
         return consp(v) == Value::t ? to_pair(v) : nullptr;
      }

      Fixnum count_nodes(Pair);
      inline Fixnum count_nodes(Value v) {
         if (auto p = to_pair_if_can(v))
            return count_nodes(p);
         return Fixnum::zero;
      }

      // ---------------
      // -- Character --
      // ---------------
      // This datatype is prepared for Uncode characters even if
      // we do not handle UCN characters at the moment.
      enum class Character : ValueBits { };

      template<>
      struct ValueTrait<Character> {
         enum Tag : ValueBits { tag = 0xE };
         enum Mask : ValueBits { tag_mask = 0xF };
      };

      constexpr Character to_character(Value v) {
         return Character(ValueBits(v) >> 4);
      }

      constexpr Value from_character(Character c) {
         return Value((ValueBits(c) << 4) | ValueTrait<Character>::tag);
      }

      // -- Object --
      // An object is a typed value.
      struct Type;
      struct Object {
         Value value;
         const Type* type;
      };

      struct Package;

      enum class SymbolAttribute : ValueBits {
         None             = 0x0,    // No particular attribute.
         Constant         = 0x1,    // Symbol defined constant.
         Special          = 0x2,    // Symbol declared special.
         Keyword          = 0x4,    // A keyword symbol.
         SpecialConstant  = Constant | Special,
      };

      constexpr SymbolAttribute
      operator&(SymbolAttribute x, SymbolAttribute y) {
         return SymbolAttribute(ValueBits(x) & ValueBits(y));
      }

      // ------------
      // -- Symbol --
      // ------------
      struct Symbol : Dynamic {
         const InternedString name;
         Value value;
         const Callable* function;
         Pair properties;
         Package* package;
         SymbolAttribute attributes;
         explicit Symbol(InternedString);
         void accept(Visitor&) const override;
         bool has(SymbolAttribute x) const { return (attributes & x) == x; }
      };

      inline Symbol* to_symbol_if_can(Value v) {
         return dynamic_cast<Symbol*>(to_dynamic_if_can(v));
      }

      inline bool is_symbol(Value v) {
         return to_symbol_if_can(v) != nullptr;
      }

      // -- Test if a value is a symbol.
      inline Value symbolp(Value v) {
         return to_value(v == Value::nil or v == Value::t or is_symbol(v));
      }

      // -- Test if a value is a keyword symbol.
      inline Value keywordp(Value v) {
         if (auto sym = to_symbol_if_can(v))
            return to_value(sym->has(SymbolAttribute::Keyword));
         return Value::nil;
      }

      struct CmpByName {
         template<typename T>
         bool operator()(const T& x, const T& y) const {
            return std::less<String>()(x.name, y.name);
         }
      };

      template<typename T>
      inline const T* setf_symbol_function(Symbol* sym, const T* fun) {
         sym->function = fun;
         return fun;
      }

      // -- Argument binding as value.
      // Binding a parameter to a value in a call.
      struct Binding : Dynamic {
         Symbol* symbol;
         Value value;
         void accept(Visitor&) const override;
      };
      
      // -- Environments.
      struct Environment {
         struct Binding {
            Symbol* symbol;
            Value value;
         };

         Environment();
         ~Environment();

         void bind(Symbol*, Value);
         Binding* lookup(InternedString);
      private:
         std::vector<Binding> lexical;
         std::vector<Binding> dynamic;
      };

      // -------------
      // -- Package --
      // -------------
      struct Package : Dynamic {
         const InternedString name;
         std::set<Symbol, CmpByName> symbols;

         explicit Package(InternedString);
         void accept(Visitor&) const override;
         Symbol* make_symbol(InternedString);
         Symbol* find_symbol(InternedString);
      };

      // --------------
      // -- Function --
      // --------------
      struct FunctionBase : Callable {
         const Symbol* name;
         Value type;
         FunctionBase(const Symbol* n, Value t = Value::nil)
               : name(n), type(t) { }
         void accept(Visitor&) const override;
      };

      // ------------------------
      // -- Builtin Operations --
      // ------------------------
      // Types for native implementation of builtin operators.
      struct BasicContext;

      template<typename... Ts>
      using RuntimeOperation = Value(*)(BasicContext*, Ts...);
      using NullaryCode = RuntimeOperation<>;
      using UnaryCode = RuntimeOperation<Value>;
      using BinaryCode = RuntimeOperation<Value, Value>;
      using TernaryCode = RuntimeOperation<Value, Value, Value>;

      template<typename Code>
      struct BuiltinFunction : FunctionBase {
         Code code;
         BuiltinFunction(const Symbol* n, Code c)
               : FunctionBase(n), code(c)
         { }
         void accept(Visitor&) const override;
      };

      using NullaryOperator = BuiltinFunction<NullaryCode>;
      using UnaryOperator = BuiltinFunction<UnaryCode>;
      using BinaryOperator = BuiltinFunction<BinaryCode>;
      using TernaryOperator = BuiltinFunction<TernaryCode>;

      // -- Operand stack.
      struct OperandStack : private std::vector<Value> {
         using super = std::vector<Value>;
         using iterator = std::reverse_iterator<super::iterator>;
         using super::size;
         using super::empty;
         iterator begin() { return rbegin(); }
         iterator end() { return rend(); }
         Value top() { return back(); }
         void push(Value v) { push_back(v); }
         Value pop() { auto v = back(); pop_back(); return v; }
         void operator-=(std::size_t i) { resize(size() - i); }
         Value operator[](std::size_t i) {
            return super::operator[](size() - i - 1);
         }
      };

      // -- Dynamic::Visitor --
      struct Dynamic::Visitor {
         virtual void visit(const Symbol&) = 0;
         virtual void visit(const Binding&) = 0;
         virtual void visit(const Package&) = 0;
         virtual void visit(const FunctionBase&) = 0;
         virtual void visit(const NullaryOperator&);
         virtual void visit(const UnaryOperator&);
         virtual void visit(const BinaryOperator&);
      };

      template<typename Code>
      void BuiltinFunction<Code>::accept(Visitor& v) const { v.visit(*this); }

      // ------------------
      // -- BasicContext --
      // ------------------
      // Provides basic evaluation services.
      struct BasicContext : StringPool {
         BasicContext();
         ~BasicContext();

         Package* make_package(InternedString);
         Symbol* make_keyword(InternedString);
         Pair make_pair(Value, Value);
         const NullaryOperator* make_operator(Symbol*, NullaryCode);
         const UnaryOperator* make_operator(Symbol*, UnaryCode);
         const BinaryOperator* make_operator(Symbol*, BinaryCode);
         const TernaryOperator* make_operator(Symbol*, TernaryCode);

         Package* keyword_package() const { return keywords; }
         Package* homeless_package() const { return homeless; }

      protected:
         std::set<Package, CmpByName> packages;
         Memory::Factory<ConsCell> conses;
         Memory::Factory<NullaryOperator> nullaries;
         Memory::Factory<UnaryOperator> unaries;
         Memory::Factory<BinaryOperator> binaries;
         Memory::Factory<TernaryOperator> ternaries;
         Package* keywords;
         Package* homeless;
      };
   };
}

#endif  // OPENAXIOM_VM_INCLUDED

