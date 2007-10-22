-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007, Gabriel Dos Reis.
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
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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

import '"sys-macros"
)package "BOOT"

--% Functions on interpreter objects

-- Interpreter objects used to be called triples because they had the
-- structure [value, type, environment].  For many years, the environment
-- was not used, so finally in January, 1990, the structure of objects
-- was changed to be (type . value).  This was chosen because it was the
-- structure of objects of type Any.  Sometimes the values are wrapped
-- (see the function isWrapped to see what this means physically).
-- Wrapped values are not actual values belonging to their types.  An
-- unwrapped value must be evaluated to get an actual value.  A wrapped
-- value must be unwrapped before being passed to a library function.
-- Typically, an unwrapped value in the interpreter consists of LISP
-- code, e.g., parts of a function that is being constructed.
--                 RSS 1/14/90

-- These are the new structure functions.

objNew(val, mode) == CONS(mode,val)             -- new names as of 10/14/93
objNewWrap(val, mode) == CONS(mode,wrap val)
objNewCode(val, mode) == ["CONS", MKQ mode,val ]
objSetVal(obj,val) == RPLACD(obj,val)
objSetMode(obj,mode) == RPLACA(obj,mode)

objVal obj == CDR obj
objValUnwrap obj == unwrap CDR obj
objMode obj == CAR obj
objEnv obj == $NE

objCodeVal obj == CADDR obj
objCodeMode obj == CADR obj


--% Library compiler structures needed by the interpreter

-- Tuples and Crosses

asTupleNew(size, listOfElts) == CONS(size, LIST2VEC listOfElts)
asTupleNew0(listOfElts) == CONS(#listOfElts, LIST2VEC listOfElts)

asTupleNewCode(size, listOfElts) == ["asTupleNew", size, ["LIST", :listOfElts]]
asTupleNewCode0(listForm) == ["asTupleNew0", listForm]

asTupleSize(at) == CAR at
asTupleAsVector(at) == CDR at
asTupleAsList(at) == VEC2LIST asTupleAsVector at

--% Basic Object Type Identification

++ If x is a literal of the basic types (Integer String DoubleFloat) then
++ this function returns its type, and nil otherwise.
getBasicMode x ==  getBasicMode0(x,$useIntegerSubdomain)

++ Subroutine of getBasicMode.
getBasicMode0(x,useIntegerSubdomain) ==
  x is nil => $EmptyMode
  STRINGP x => $String
  INTEGERP x =>
    useIntegerSubdomain =>
      x > 0 => $PositiveInteger
      x = 0 => $NonNegativeInteger
      $Integer
    $Integer
  FLOATP x => $DoubleFloat
  (x='noBranch) or (x='noValue) => $NoValueMode
  nil

++ If x is a literal of the basic types then returns
++ an interpreter object denoting x, and nil otherwise.
getBasicObject x ==
  INTEGERP    x =>
    t :=
      not $useIntegerSubdomain => $Integer
      x > 0 => $PositiveInteger
      x = 0 => $NonNegativeInteger
      $Integer
    objNewWrap(x,t)
  STRINGP x => objNewWrap(x,$String)
  FLOATP  x => objNewWrap(x,$DoubleFloat)
  NIL

