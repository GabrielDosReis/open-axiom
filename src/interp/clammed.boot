-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
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

import g_-timer
namespace BOOT

++ All functions defined in this file are `clammed', e.g. they are
++ translated in such a way that they cache results they compute.
)eval BOOTTRAN::$bfClamming := true

--% Functions on $clamList

-- These files are read in by the system so that they can be cached
-- properly.  Otherwise, must read in compiled versions and then
-- recompile these, resulting in wasted BPI space.

canCoerceFrom(mr,m) ==
  -- bind flag for recording/reporting instantiations
  -- (see recordInstantiation)
  $insideCanCoerceFrom: local := [mr,m]
  canCoerceFrom0(mr,m)

canCoerce(t1, t2) ==
  val := canCoerce1(t1, t2) => val
  t1 is ['Variable, :.] =>
    newMode := getMinimalVarMode(t1, nil)
    canCoerce1(t1, newMode) and canCoerce1(newMode, t2)
  nil

coerceConvertMmSelection(funName,m1,m2) ==
  -- calls selectMms with $Coerce=nil and tests for required
  -- target type. funName is either 'coerce or 'convert.
  $declaredMode : local:= nil
  $reportBottomUpFlag : local:= nil
  l := selectMms1(funName,m2,[m1],[m1],nil)
  mmS := [[sig,[targ,arg],:pred] for x in l | x is [sig,[.,arg],:pred] and
    hasCorrectTarget(m2,sig) and sig is [dc,targ,oarg] and oarg = m1]
  mmS and first mmS

++ Note: this function is used in the algebra part.
isValidType form ==
  -- returns true IFF form is a type whose arguments satisfy the
  --  predicate of the type constructor
  -- Note that some forms are said to be invalid because they would
  -- cause problems with the interpreter. Thus things like P P I
  -- are not valid.
  string? form => true
  ident?  form => false
  member(form,$LangSupportTypes) => true
  form is ['Record,:selectors] =>
    and/[isValidType type for [:.,type] in selectors]
  form is ['Enumeration,:args] =>
    null (and/[ident? x for x in args]) => false
    ((# args) = (# removeDuplicates args)) => true
    false
  form is ['Mapping,:mapargs] =>
    null mapargs => false
    and/[isValidType type for type in mapargs]
  form is ['Union,:args] =>
    -- check for a tagged union
    args and first args is [":",:.] =>
      and/[isValidType type for [:.,type] in args]
    null (and/[isValidType arg for arg in args]) => false
    ((# args) = (# removeDuplicates args)) => true
    sayKeyedMsg("S2IR0005",[form])
    false

  badDoubles := [$QuotientField,:'(Gaussian Complex Polynomial Expression)]
  form is [T1, [T2, :.]] and T1 = T2 and member(T1, badDoubles) => nil

  form is [=$QuotientField,D] and not isPartialMode(D) and
    ofCategory(D,$Field) => nil
  form is ['UnivariatePolynomial, x, ['UnivariatePolynomial, y, .]] and x=y =>
    nil
  form = '(Complex (AlgebraicNumber)) => nil
  form is ['Expression, ['Kernel, . ]] => nil
  form is [op,:argl] =>
    not constructor? op => nil
    cosig := getDualSignature op
    cosig and null rest cosig => -- niladic constructor
        null argl => true
        false
    null (sig := getConstructorSignature op) => nil
    [.,:cl] := sig
    -- following line is needed to deal with mutable domains
    if # cl ~= # argl and gensym? last argl then argl:= drop(-1,argl)
    # cl ~= # argl => nil
    cl:= replaceSharps(cl,form)
    and/[isValid for x in argl for c in cl] where isValid() ==
      categoryForm?(c) =>
        evalCategory(x,substitute(x,'_$,c)) and isValidType x
      -- Arguments to constructors are general expressions.  Below
      -- domain constructors are not considered valid arguments (yet).
      x' := opOf x
      cons? x' or not ident? x' => true   -- surely not constructors
      getConstructorKindFromDB x' ~= "domain"

selectMms1(op,tar,args1,args2,$Coerce) ==
    -- for new compiler/old world compatibility, sometimes have to look
    -- for operations given two names.

    -- NEW COMPILER COMPATIBILITY ON

    op = "^" or op = "**" =>
        append(selectMms2("**",tar,args1,args2,$Coerce),
               selectMms2("^",tar,args1,args2,$Coerce))

    -- NEW COMPILER COMPATIBILITY OFF

    selectMms2(op,tar,args1,args2,$Coerce)


resolveTT(t1,t2) ==
  -- resolves two types
  -- this symmetric resolve looks for a type t to which both t1 and t2
  -- can be coerced
  -- if resolveTT fails, the result will be nil
  startTimingProcess 'resolve
  t1 := eqType t1
  t2 := eqType t2
  null (t := resolveTT1(t1,t2)) =>
    stopTimingProcess 'resolve
    nil
  isValidType (t := eqType t) =>
    stopTimingProcess 'resolve
    t
  stopTimingProcess 'resolve
  nil

isLegitimateMode(t,hasPolyMode,polyVarList) ==
  -- returns true IFF t is a valid type.  i.e. if t has no repeated
  --  variables, or two levels of Polynomial
  null t        => true    -- a terminating condition with underDomainOf
  t = $EmptyMode => true
  string? t     => true
  t isnt [.,:.] => false

  badDoubles := [$QuotientField,:'(Gaussian Complex Polynomial Expression)]
  t is [T1, [T2, :.]] and T1 = T2 and member(T1, badDoubles) => false

  t is [=$QuotientField,D] and not isPartialMode(D) and
    ofCategory(D,$Field) => false
  t = '(Complex (AlgebraicNumber)) => false

  t := equiType t
  vl := isPolynomialMode t =>
    if vl~='all then
      var:= or/[(member(x,polyVarList) => x;nil) for x in vl] => return false
      listOfDuplicates vl => return false
      polyVarList:= union(vl,polyVarList)
    hasPolyMode => false
    con := first t
    poly? := (con is 'Polynomial or con is 'Expression)
    isLegitimateMode(underDomainOf t,poly?,polyVarList)

  ident?(op := first t) and constructor? op =>
    isLegitimateMode(underDomainOf t,hasPolyMode,polyVarList) => t
  t is ['Mapping,:ml] =>
    null ml => false
    -- first arg is target, which can be Void
    null isLegitimateMode(first ml,nil,nil) => false
    for m in rest ml repeat
      m = $Void =>
        return false
      null isLegitimateMode(m,nil,nil) => return false
    true
  t is ['Union,:ml] =>
    -- check for tagged union
    ml and first ml is [":",:.] => isLegitimateRecordOrTaggedUnion ml
    null (and/[isLegitimateMode(m,nil,nil) for m in ml]) => false
    ((# ml) = (# removeDuplicates ml)) => true
    false
  t is ['Record,:r] => isLegitimateRecordOrTaggedUnion r
  t is ['Enumeration,:r] =>
    null (and/[ident? x for x in r]) => false
    ((# r) = (# removeDuplicates r)) => true
    false
  false

underDomainOf t ==
  t = $RationalNumber => $Integer
  t isnt [.,:.] => nil
  d := deconstructT t
  1 = #d => nil
  u := getUnderModeOf(t) => u
  last d

