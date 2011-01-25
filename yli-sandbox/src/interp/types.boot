-- Copyright (C) 2007-2010 Gabriel Dos Reis
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     - Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     - Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in
--       the documentation and/or other materials provided with the
--       distribution.
--
--     - Neither the name of The Numerical Algorithms Group Ltd. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--

import boot_-pkg
namespace BOOT

--% Basic types used throughout Boot codes.

++ Type of nothing.  Bottom of the abstract machine type lattice.
++ Since Lisp functions always returns something, we cannot
++ use the `nil' type specifier (the ideal answer).  Second
++ best possibility is to have Void-returning functions
++ systematically return `nil'.  However, until the Lisp
++ backend is fixed, we will use the interpretation that a 
++ Void-returning function may return anything, but nobody cares.
++ Hence, the choice below which contradicts the very first line
++ of this description.
%Void <=> 
  true

++ Type of truth values.
%Boolean <=> 
  BOOLEAN

++ Type of a bit value.
%Bit <=>
  BIT

++ Type of 8-bit sized unsigned integer values.
%Byte <=>
  UNSIGNED_-BYTE 8

++ Type of characters -- no distinction yet.
%Char <=>
  CHARACTER

++ Type of fixnums.
%Short <=> 
  FIXNUM

++ Type of unlimited precision integers.
%Bignum <=> 
  BIGNUM

%Integer <=>
  INTEGER

%IntegerSection n <=>
  INTEGER n

++ Type of single precision floating point numbers.  Most of the
++ time, this is a 32-bit datatype on IEEE-754 host.
%SingleFloat <=>
)if %hasFeature KEYWORD::GCL
  SHORT_-FLOAT
)else
  SINGLE_-FLOAT
)endif

++ Type of double precision floating point numbers.  Most of the time,
++ this is a 64-bit sized datatype on IEEE-756 host.
%DoubleFloat <=>
  DOUBLE_-FLOAT

++ General type for numbers.
%Number <=> 
  NUMBER

++ Type of identifiers.  Ideally, we want actually want to exclude
++ Lisp oddities such as NIL and T.
%Symbol <=> 
  SYMBOL

++ The type of literal strings
%String <=> 
  STRING

++ Anything that is not a cons cell.
%Atom <=> atom

++ nil or a cons cell.  Ideally, this should be parameterized, but
++ we cannot afford that luxury with Lisp.
%List <=> 
  LIST

++ The type of a linear homogeneous non-extensible array.
%SimpleArray a <=> 
  SIMPLE_-ARRAY a

%Vector a <=> VECTOR a

%BitVector <=> %Vector %Bit

%Thing <=> true

%Sequence <=> SEQUENCE

%Pair <=> CONS

%Maybe a <=> null or a

--% Data structures for the compiler
%Constructor <=> %Symbol                       -- constructor
%Form <=> %Number or %Symbol or %String or %Pair -- input syntax form
%Instantiation <=> [%Constructor,:%Form]       -- constructor instance
%Env <=> %List                                 -- compiling env
%Mode <=> %Symbol or %String or %List          -- type of forms
%Code <=> %Form or %Char                       -- generated code
%Triple <=>                                    -- form + type + env
  [%Code,:[%Mode,:[%Env,:null]]]

%Signature                      -- signature
  <=> %Symbol or %Pair

%Modemap <=> %List                             -- modemap

%ConstructorKind <=>                           -- kind of ctor instances
  MEMBER(category,domain,package)

%Shell <=> SIMPLE_-VECTOR                      -- constructor instantiation
