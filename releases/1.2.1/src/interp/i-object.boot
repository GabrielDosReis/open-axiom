-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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

import g_-util
namespace BOOT

++ true when the interpreter should evaluate forms to values, as
++ opposed to just generating code to compute values.
$genValue := true

++ true if we are about to generate a function definition
$definingMap := false

++ List variables locat to the current function.
$localVars := []

++ declared mode of the current entity being processed.
$declaredMode := nil

++
$useIntegerSubdomain := true

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
objEnv obj == $EmptyEnvironment

objCodeVal obj == CADDR obj
objCodeMode obj == CADR obj

--% Utility Functions Used Only by the Intepreter
 
wrap x ==
  isWrapped x => x
  ["WRAPPED",:x]
 
isWrapped x == x is ['WRAPPED,:.] or NUMBERP x or FLOATP x or CVECP x
 
unwrap x ==
  NUMBERP x or FLOATP x or CVECP x => x
  x is ["WRAPPED",:y] => y
  x
 
quote2Wrapped x ==
  x is ["QUOTE",y] => wrap y
  x
 
removeQuote x ==
  x is ["QUOTE",y] => y
  x
 
++ returns the normal form of `obj''s value, suitable for use as
++ argument to a (library) function call.
getValueNormalForm obj ==
  val := objVal obj
  atom val => val
  [op,:argl] := val
  op = "WRAPPED" => MKQ argl
  IDENTP op and isConstructorName op => 
    isConceptualCategory objMode obj => instantiationNormalForm(op,argl)
    MKQ val
  -- This is not the final value of `obj', rather something that needs
  -- further evaluation, e.g. generated code to compute the value.
  val

instantiationNormalForm(op,argl) ==
  [op,:[normalVal for arg in argl]] where normalVal() ==
     atom arg => arg
     [h,:t] := arg
     IDENTP h and isConstructorName h => instantiationNormalForm(h,t)
     MKQ arg


-- addQuote x ==
--   NUMBERP x => x
--   ['QUOTE,x]
 
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

++ The VAT class for literal values.  Also used in the
++ algebra interface to the interpreter.
$immediateDataSymbol ==
  "--immediateData--"

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
  (x='%noBranch) or (x='noValue) => $NoValueMode
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


--%% Vectorized Attributed Trees

--% The interpreter translates parse forms into vats for analysis.
--% These contain a number of slots in each node for information.
--% The leaves are now all vectors, though the leaves for basic types
--% such as integers and strings used to just be the objects themselves.
--% The vectors for the leaves with such constants now have the value
--% of $immediateDataSymbol as their name. Their are undoubtably still
--% some functions that still check whether a leaf is a constant. Note
--% that if it is not a vector it is a subtree.

--% attributed tree nodes have the following form:
--% slot         description
--% ----         -----------------------------------------------------
--%  0           operation name or literal
--%  1           declared mode of variable
--%  2           computed value of subtree from this node
--%  3           modeset: list of single computed mode of subtree
--%  4           prop list for extra things


++ create a leaf VAT node.
mkAtreeNode x ==
  -- maker of attrib tree node
  v := newShell 5
  v.0 := x
  v

++ remove mode, value, and misc. info from attrib tree
emptyAtree expr ==
  VECP expr =>
    $immediateDataSymbol = expr.0 => nil
    expr.1:= NIL
    expr.2:= NIL
    expr.3:= NIL
    -- kill proplist too?
  atom expr => nil
  for e in expr repeat emptyAtree e


++ returns true if x is a leaf VAT object.
isLeaf x == 
  atom x     --may be a number or a vector

++ returns the mode of the VAT node x.
++ Also used by the algebra interface to the interpreter.
getMode x ==
  x is [op,:.] => getMode op
  VECP x => x.1
  m := getBasicMode x => m
  keyedSystemError("S2II0001",[x])

++ sets the mode for the VAT node x to y.
putMode(x,y) ==
  x is [op,:.] => putMode(op,y)
  null VECP x => keyedSystemError("S2II0001",[x])
  x.1 := y

++ returns an interpreter object that represents the value of node x.
++ Note that an interpreter object is a pair of mode and value.
++ Also used by the algebra interface to the interperter.
getValue x ==
  VECP x => x.2
  atom x =>
    t := getBasicObject x => t
    keyedSystemError("S2II0001",[x])
  getValue first x

++ sets the value of VAT node x to interpreter object y.
putValue(x,y) ==
  x is [op,:.] => putValue(op,y)
  null VECP x => keyedSystemError("S2II0001",[x])
  x.2 := y

++ same as putValue(vec, val), except that vec is returned instead of val.
putValueValue(vec,val) ==
  putValue(vec,val)
  vec

++ Returns the node class of x, if possible; otherwise nil.
++ Also used by the algebra interface to the interpreter.
getUnnameIfCan x ==
  VECP x => x.0
  x is [op,:.] => getUnnameIfCan op
  atom x => x
  nil

++ Returns the node class of x; otherwise raise an error.
getUnname x ==
  x is [op,:.] => getUnname op
  getUnname1 x

++ Subroutine of getUnname.
getUnname1 x ==
  VECP x => x.0
  null atom x => keyedSystemError("S2II0001",[x])
  x

++ returns the mode-set of VAT node x.
getModeSet x ==
  x and PAIRP x => getModeSet first x
  VECP x =>
    y:= x.aModeSet =>
      (y = [$EmptyMode]) and ((m := getMode x) is ['Mapping,:.]) =>
        [m]
      y
    keyedSystemError("S2GE0016",['"getModeSet",'"no mode set"])
  m:= getBasicMode x => [m]
  not atom x => getModeSet first x
  keyedSystemError("S2GE0016",['"getModeSet",
    '"not an attributed tree"])

++ Sets the mode-set of VAT node x to y.
putModeSet(x,y) ==
  x is [op,:.] => putModeSet(op,y)
  not VECP x => keyedSystemError("S2II0001",[x])
  x.3 := y
  y

getModeOrFirstModeSetIfThere x ==
  x is [op,:.] => getModeOrFirstModeSetIfThere op
  VECP x =>
    m := x.1 => m
    val := x.2 => objMode val
    y := x.aModeSet =>
      (y = [$EmptyMode]) and ((m := getMode x) is ['Mapping,:.]) => m
      first y
    NIL
  m := getBasicMode x => m
  NIL

getModeSetUseSubdomain x ==
  x and PAIRP x => getModeSetUseSubdomain first x
  VECP(x) =>
    -- don't play subdomain games with retracted args
    getAtree(x,'retracted) => getModeSet x
    y := x.aModeSet =>
      (y = [$EmptyMode]) and ((m := getMode x) is ['Mapping,:.]) =>
        [m]
      val := getValue x
      (x.0 = $immediateDataSymbol) and (y = [$Integer]) =>
        val := objValUnwrap val
        m := getBasicMode0(val,true)
        x.2 := objNewWrap(val,m)
        x.aModeSet := [m]
        [m]
      null val => y
      isEqualOrSubDomain(objMode(val),$Integer) and
        INTEGERP(f := objValUnwrap val) =>
          [getBasicMode0(f,true)]
      y
    keyedSystemError("S2GE0016",
      ['"getModeSetUseSubomain",'"no mode set"])
  m := getBasicMode0(x,true) => [m]
  null atom x => getModeSetUseSubdomain first x
  keyedSystemError("S2GE0016",
    ['"getModeSetUseSubomain",'"not an attributed tree"])


computedMode t ==
  getModeSet t is [m] => m
  keyedSystemError("S2GE0016",['"computedMode",'"non-singleton modeset"])

--% Other VAT properties

insertShortAlist(prop,val,al) ==
  pair := QASSQ(prop,al) =>
    RPLACD(pair,val)
    al
  [[prop,:val],:al]

putAtree(x,prop,val) ==
  x is [op,:.] =>
    -- only willing to add property if op is a vector
    -- otherwise will be pushing to deeply into calling structure
    if VECP op then putAtree(op,prop,val)
    x
  null VECP x => x     -- just ignore it
  n := QLASSQ(prop,'((mode . 1) (value . 2) (modeSet . 3)))
    => x.n := val
  x.4 := insertShortAlist(prop,val,x.4)
  x

getAtree(x,prop) ==
  x is [op,:.] =>
    -- only willing to get property if op is a vector
    -- otherwise will be pushing to deeply into calling structure
    VECP op => getAtree(op,prop)
    NIL
  null VECP x => NIL     -- just ignore it
  n:= QLASSQ(prop,'((mode . 1) (value . 2) (modeSet . 3)))
    => x.n
  QLASSQ(prop,x.4)

putTarget(x, targ) ==
  -- want to put nil modes perhaps to clear old target
  if targ = $EmptyMode then targ := nil
  putAtree(x,'target,targ)

getTarget(x) == 
  getAtree(x,'target)

--% Source and position information

-- In the following, src is a string containing an original input line,
-- line is the line number of the string within the source file,
-- and col is the index within src of the start of the form represented
-- by x. x is a VAT.

++ returns source position information for VAT node x.
getSrcPos(x) == 
  getAtree(x, 'srcAndPos)

++ sets the source location information for VAT node x.
putSrcPos(x, file, src, line, col) ==
  putAtree(x, 'srcAndPos, srcPos_New(file, src, line, col))

srcPosNew(file, src, line, col) == 
  LIST2VEC [file, src, line, col]

++ returns the name of source file for source location `sp'.
srcPosFile(sp) ==
  if sp then sp.0 else nil

++ returns the input source string for source location `sp'.
srcPosSource(sp) ==
  if sp then sp.1 else nil

++ returns the line number for source location `sp'.
srcPosLine(sp) ==
  if sp then sp.2 else nil

++ returns the column number for source location `sp'.
srcPosColumn(sp) ==
  if sp then sp.3 else nil

srcPosDisplay(sp) ==
  null sp => nil
  s := STRCONC('"_"", srcPosFile sp, '"_", line ",
      STRINGIMAGE srcPosLine sp, '": ")
  sayBrightly [s, srcPosSource sp]
  col  := srcPosColumn sp
  dots :=
      col = 0 => '""
      fillerSpaces(col, '".")
  sayBrightly [fillerSpaces(#s, '" "), dots, '"^"]
  true


++ Returns the calling convention vector for an operation
++ represented by the VAT `t'.
getFlagArgsPos t ==
  VECP t => getAtree(t, 'flagArgsPos)
  atom t => keyedSystemError("S2II0001",[t])
  getFlagArgsPos car t

--% Transfer of VAT properties.


transferPropsToNode(x,t) ==
  propList := getProplist(x,$env)
  QLASSQ('Led,propList) or QLASSQ('Nud,propList) => nil
  node :=
    VECP t => t
    first t
  for prop in '(mode localModemap value name generatedCode)
    repeat transfer(x,node,prop)
      where
        transfer(x,node,prop) ==
          u := get(x,prop,$env) => putAtree(node,prop,u)
          (not (x in $localVars)) and (u := get(x,prop,$e)) =>
            putAtree(node,prop,u)
  if not getMode(t) and (am := get(x,'automode,$env)) then
    putModeSet(t,[am])
    putMode(t,am)
  t
