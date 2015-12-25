-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


import i_-coerfn
namespace BOOT

$domPvar := nil

sayFunctionSelection(op,args,target,dc,func) ==
  $abbreviateTypes : local := true
  startTimingProcess 'debug
  fsig := formatSignatureArgs args
  if not LISTP fsig then fsig := LIST fsig
  if func then func := bright ['"by ",func]
  sayMSG concat ['"%l",:bright '"Function Selection for",op,:func,'"%l",
    '"      Arguments:",:bright fsig]
  if target then sayMSG concat ['"      Target type:",
    :bright prefix2String target]
  if dc  then sayMSG concat ['"      From:     ",
    :bright prefix2String dc]
  stopTimingProcess 'debug

sayFunctionSelectionResult(op,args,mmS) ==
  $abbreviateTypes : local := true
  startTimingProcess 'debug
  if mmS then printMms mmS
  else sayMSG concat ['"   -> no function",:bright op,
    '"found for arguments",:bright formatSignatureArgs args]
  stopTimingProcess 'debug

selectMms(op,args,$declaredMode) ==
  -- selects applicable modemaps for node op and arguments args
  -- if there is no local modemap, and it is not a package call, then
  --   the cached function selectMms1 is called
  startTimingProcess 'modemaps
  n:= getUnname op
  val := getValue op
  opMode := objMode val

  -- see if we have a functional parameter
  ((isSharpVarWithNum(n) and opMode) or (val and opMode)) and
      opMode is ['Mapping,:ta] =>
        imp :=
          val => getValueNormalForm val
          n
        [[['local,:ta], imp , nil]]

  ((isSharpVarWithNum(n) and opMode) or (val and opMode)) and
      opMode is ['Variable,f] =>
         emptyAtree op
         op.0 := f
         selectMms(op,args,$declaredMode)

  isSharpVarWithNum(n) and opMode is ['FunctionCalled,f] =>
         op.0 := f
         selectMms(op,args,$declaredMode)

  types1 := getOpArgTypes(n,args)
  numArgs := #args
  member($EmptyMode,types1) => nil

  tar := getTarget op
  dc  := getAtree(op,'dollar)

  dc = nil and val and objMode(val) = $AnonymousFunction =>
      tree := mkAtree objValUnwrap getValue op
      putTarget(tree,['Mapping,tar,:types1])
      bottomUp tree
      val := getValue tree
      [[['local,:rest objMode val], getValueNormalForm val, nil]]

  if (n is 'map) and (first types1 = $AnonymousFunction)
    then
      tree := mkAtree objValUnwrap getValue first args
      ut :=
        tar => underDomainOf tar
        nil
      ua := [underDomainOf x for x in rest types1]
      member(nil,ua) => nil
      putTarget(tree,['Mapping,ut,:ua])
      bottomUp tree
      val := getValue tree
      types1 := [objMode val,:rest types1]
      args.first := tree

  if numArgs = 1 and (n is "numer" or n is "denom") and
    isEqualOrSubDomain(first types1,$Integer) and dc = nil then
      dc := ['Fraction, $Integer]
      putAtree(op, 'dollar, dc)


  if $reportBottomUpFlag then sayFunctionSelection(n,types1,tar,dc,nil)

  identType := 'Variable
  for x in types1 while not $declaredMode repeat
      x isnt [=identType,:.] => $declaredMode:= x
  types2 := [altTypeOf(x,y,$declaredMode) for x in types1 for y in args]

  mmS:=
    dc => selectDollarMms(dc,n,types1,types2)

    if n is "/" and tar = $Integer then
      tar := $RationalNumber
      putTarget(op,tar)

    -- now to speed up some standard selections
    if not tar then
      tar := defaultTarget(op,n,#types1,types1)
      if tar and $reportBottomUpFlag then
        sayMSG concat ['"      Default target type:",
          :bright prefix2String tar]

    selectLocalMms(op,n,types1,tar) or
      (VECTORP op and selectMms1(n,tar,types1,types2,'T))
  if $reportBottomUpFlag then sayFunctionSelectionResult(n,types1,mmS)
  stopTimingProcess 'modemaps
  mmS

-- selectMms1 is in clammed.boot

selectMms2(op,tar,args1,args2,$Coerce) ==
  -- decides whether to find functions from a domain or package
  --   or by general modemap evaluation
  or/[string? arg for arg in args1] => nil
  if tar = $EmptyMode then tar := nil
  nargs := #args1
  mmS := nil
  mmS :=
    -- special case map for the time being
    $Coerce and (op is 'map) and (2 = nargs) and
      (first(args1) is ['Variable,fun]) =>
        null (ud := underDomainOf second args1) => nil
        if tar then ut := underDomainOf(tar)
        else ut := nil
        null (mapMms := selectMms1(fun,ut,[ud],[nil],true)) => nil
        mapMm := CDAAR mapMms
        selectMms1(op,tar,[['Mapping,:mapMm],second args1],
          [nil,second args2],$Coerce)

    $Coerce and (op is 'map) and (2 = nargs) and
      (first(args1) is ['FunctionCalled,fun]) =>
        null (ud := underDomainOf second args1) => nil
        if tar then ut := underDomainOf(tar)
        else ut := nil
        funNode := mkAtreeNode fun
        transferPropsToNode(fun,funNode)
        null (mapMms := selectLocalMms(funNode,fun,[ud],nil)) => nil
        mapMm := CDAAR mapMms
        selectMms1(op,tar,[['Mapping,:mapMm],second args1],
          [nil,second args2],$Coerce)

    -- get the argument domains and the target
    a := nil
    for x in args1 repeat if x then a := [x,:a]
    for x in args2 repeat if x then a := [x,:a]
    if tar and not isPartialMode tar then a := [tar,:a]

    -- for typically homogeneous functions, throw in resolve too
    if op in '(_= _+ _* _- ) then
      r := resolveTypeList a
      if r ~= nil then a := [r,:a]

    if tar and not isPartialMode tar then
      if xx := underDomainOf(tar) then a := [xx,:a]
    for x in args1 repeat
      cons?(x) and first(x) in '(List Vector Stream FiniteSet Array) =>
        xx := underDomainOf(x) => a := [xx,:a]

    -- now extend this list with those from the arguments to
    -- any Unions, Mapping or Records

    a' := nil
    a := reverse! removeDuplicates a
    for x in a repeat
      x = nil => 'iterate
      x is '(RationalRadicals) => a' := [$RationalNumber,:a']
      x is ['Union,:l] =>
        -- check if we have a tagged union
        l and first l is [":",:.] =>
          for [.,.,t] in l repeat
            a' := [t,:a']
        a' := append(reverse l,a')
      x is ['Mapping,:l] => a' := append(reverse l,a')
      x is ['Record,:l] =>
        a' := append(reverse [third s for s in l],a')
      x is ['FunctionCalled,name] =>
        (xm := get(name,'mode,$e)) and not isPartialMode xm =>
          a' := [xm,:a']
    a := append(a,removeDuplicates a')
    a := [x for x in a | cons?(x)]

    -- step 1. see if we have one without coercing
    a' := a
    while a repeat
      x:= first a
      a:= rest a
      x isnt [.,:.] => 'iterate
      mmS := append(mmS, findFunctionInDomain(op,x,tar,args1,args2,nil,nil))

    -- step 2. if we didn't get one, trying coercing (if we are
    --         suppose to)

    if mmS = nil and $Coerce then
      a := a'
      while a repeat
        x:= first a
        a:= rest a
        x isnt [.,:.] => 'iterate
        mmS := append(mmS,
          findFunctionInDomain(op,x,tar,args1,args2,$Coerce,nil))

    mmS or selectMmsGen(op,tar,args1,args2)
  mmS and orderMms(op, mmS,args1,args2,tar)

isAVariableType t ==
    t is ['Variable,.] or t = $Symbol or t is ['OrderedVariableList,.]

defaultTarget(opNode,op,nargs,args) ==
  -- this is for efficiency. Chooses standard targets for operations
  -- when no target exists.

  target := nil

  nargs = 0 =>
    op is "nil" =>
      putTarget(opNode, target := '(List (None)))
      target
    op is "true"  or op is "false" =>
      putTarget(opNode, target := $Boolean)
      target
    op is 'pi =>
      putTarget(opNode, target := ['Pi])
      target
    op is 'infinity =>
      putTarget(opNode, target := ['OnePointCompletion, $Integer])
      target
    op in '(plusInfinity minusInfinity) =>
      putTarget(opNode, target := ['OrderedCompletion, $Integer])
      target
    target

  a1 := first args
  a1 isnt [.,:.] => target
  a1f := first a1

  nargs = 1 =>
    op is 'kernel =>
      putTarget(opNode, target := ['Kernel, ['Expression, $Integer]])
      target
    op is 'list =>
      putTarget(opNode, target := ['List, a1])
      target
    target

  a2 := second args

  nargs >= 2 and op is "draw" and a1 is ['FunctionCalled,sym] and a2 is ['Segment,.] =>

    -- this clears up some confusion over 2D and 3D graphics

    symNode := mkAtreeNode sym
    transferPropsToNode(sym,symNode)

    nargs >= 3 and third args is ['Segment,.] =>
      selectLocalMms(symNode,sym,[$DoubleFloat, $DoubleFloat],nil)
      putTarget(opNode, target := '(ThreeDimensionalViewport))
      target

    (mms := selectLocalMms(symNode,sym,[$DoubleFloat],nil)) =>
      [.,targ,:.] := CAAR mms
      targ = $DoubleFloat =>
          putTarget(opNode, target := '(TwoDimensionalViewport))
          target
      targ = ['Point, $DoubleFloat] =>
          putTarget(opNode, target := '(ThreeDimensionalViewport))
          target
      target

    target

  nargs >= 2 and op is "makeObject" and a1 is ['FunctionCalled,sym] and a2 is ['Segment,.] =>
    -- we won't actually bother to put a target on makeObject
    -- this is just to figure out what the first arg is
    symNode := mkAtreeNode sym
    transferPropsToNode(sym,symNode)

    nargs >= 3 and third args is ['Segment,.] =>
      selectLocalMms(symNode,sym,[$DoubleFloat, $DoubleFloat],nil)
      target

    selectLocalMms(symNode,sym,[$DoubleFloat],nil)
    target

  nargs = 2 =>
    op is "elt" =>
        a1 = $BasicOperator and a2 is ['List, ['OrderedVariableList, .]] =>
           ['Expression, $Integer]
        target

    op is "eval" =>
        a1 is ['Expression,b1] and a2 is ['Equation, ['Polynomial,b2]] =>
            target :=
              canCoerce(b2, a1) => a1
              t := resolveTT(b1, b2)
              (not t) or (t = $Any) => nil
              resolveTT(a1, t)
            if target then putTarget(opNode, target)
            target
        a1 is ['Equation, .] and a2 is ['Equation, .] =>
            target := resolveTT(a1, a2)
            if target and not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target
        a1 is ['Equation, .] and a2 is ['List, a2e] and a2e is ['Equation, .] =>
            target := resolveTT(a1, a2e)
            if target and not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target
        a2 is ['Equation, a2e] or a2 is ['List, ['Equation, a2e]] =>
            target := resolveTT(a1, a2e)
            if target and not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target

    op is "**" or op is "^" =>
      a2 = $Integer =>
        if (target := resolveTCat(a1,$Field)) then
          putTarget(opNode,target)
        target
      a1 = $AlgebraicNumber and (a2 = $Float or a2 = $DoubleFloat) =>
          target := ['Expression, a2]
          putTarget(opNode,target)
          target
      a1 = $AlgebraicNumber and a2 is ['Complex, a3] and (a3 = $Float or a3 = $DoubleFloat) =>
          target := ['Expression, a3]
          putTarget(opNode,target)
          target
      ((a2 = $RationalNumber) and
        (typeIsASmallInteger(a1) or isEqualOrSubDomain(a1,$Integer))) =>
            putTarget(opNode, target := $AlgebraicNumber)
            target
      ((a2 = $RationalNumber) and (isAVariableType(a1)
          or a1 is ['Polynomial,.] or a1 is ['RationalFunction,.])) =>
            putTarget(opNode, target := defaultTargetFE a1)
            target
      isAVariableType(a1) and (a2 = $PositiveInteger or a2 = $NonNegativeInteger) =>
          putTarget(opNode, target := '(Polynomial (Integer)))
          target
      isAVariableType(a2) =>
        putTarget(opNode, target := defaultTargetFE a1)
        target
      a2 is ['Polynomial, D] =>
        (a1 = a2) or isAVariableType(a1)
         or ((a1 is ['RationalFunction, D1]) and (D1 = D)) or (a1 = D)
          or ((a1 is [=$QuotientField, D1]) and (D1 = a1)) =>
            putTarget(opNode, target := defaultTargetFE a2)
            target
        target
      a2 is ['RationalFunction, D] =>
        (a1 = a2) or isAVariableType(a1)
         or ((a1 is ['RationalFunction, D1]) and (D1 = D)) or (a1 = D)
          or ((a1 is [=$QuotientField, D1]) and (D1 = a1)) =>
            putTarget(opNode, target := defaultTargetFE a2)
            target
        target
      target

    op is "/" =>
      isEqualOrSubDomain(a1, $Integer) and isEqualOrSubDomain(a2, $Integer) =>
        putTarget(opNode, target := $RationalNumber)
        target
      a1 = a2 =>
        if (target := resolveTCat(first args,$Field)) then
          putTarget(opNode,target)
        target
      a1 is ['Variable,.] and a2 is ['Variable,.] =>
        putTarget(opNode,target := mkRationalFunction $Integer)
        target
      isEqualOrSubDomain(a1,$Integer) and a2 is ['Variable,.] =>
        putTarget(opNode,target := mkRationalFunction $Integer)
        target
      a1 is ['Variable,.] and
        a2 is ['Polynomial,D] =>
          putTarget(opNode,target := mkRationalFunction D)
          target
        target
      a2 is ['Variable,.] and
        a1 is ['Polynomial,D] =>
          putTarget(opNode,target := mkRationalFunction D)
          target
        target
      a2 is ['Polynomial,D] and (a1 = D) =>
        putTarget(opNode,target := mkRationalFunction D)
        target
      target

  a3 := third args
  nargs = 3 =>
    op is "eval" =>
        a3 is ['List, a3e] =>
            target := resolveTT(a1, a3e)
            if not (target = $Any) then putTarget(opNode,target)
            else target := nil
            target

        target := resolveTT(a1, a3)
        if not (target = $Any) then putTarget(opNode,target)
        else target := nil
        target
  target

mkRationalFunction D ==  ['Fraction, ['Polynomial, D]]

defaultTargetFE(a,:options) ==
  a is ['Variable,.] or a = $RationalNumber or symbolMember?(a.op,
    [$Symbol.op, 'RationalRadicals,
     'Pi]) or typeIsASmallInteger(a) or isEqualOrSubDomain(a, $Integer) or
       a = $AlgebraicNumber =>
          IFCAR options => [$FunctionalExpression, ['Complex, $Integer]]
          [$FunctionalExpression, $Integer]
  a is ['Complex,uD] => defaultTargetFE(uD, true)
  a is [D,uD] and D in '(Polynomial RationalFunction Fraction) =>
     defaultTargetFE(uD, IFCAR options)
  a is [=$FunctionalExpression,.] => a
  IFCAR options => [$FunctionalExpression, ['Complex, a]]
  [$FunctionalExpression, a]

altTypeOf(type,val,$declaredMode) ==
  (type = $Symbol or type is ["Variable",:.]) and
    (a := getMinimalVarMode(objValUnwrap getValue(val),$declaredMode)) =>
      a
  type is ['OrderedVariableList,vl] and
    integer?(val1 := objValUnwrap getValue(val)) and
      (a := getMinimalVarMode(vl.(val1 - 1),$declaredMode)) =>
        a
  type = $PositiveInteger    => $Integer
  type = $NonNegativeInteger => $Integer
  type is '(List (PositiveInteger)) => '(List (Integer))
  nil

getOpArgTypes(opname, args) ==
  l := getOpArgTypes1(opname, args)
  [f(a,opname) for a in l] where
    f(x,op) ==
      x is ['FunctionCalled,g] and op isnt 'name =>
        m := get(g,'mode,$e) =>
          m is ['Mapping,:.] => m
          x
        x
      x

getOpArgTypes1(opname, args) ==
  args = nil => nil
  -- special cases first
  opname is 'coef and args is [b,n] =>
    [first getModeSet b, first getModeSetUseSubdomain n]
  opname is 'monom and args is [d,c] =>
    [first getModeSetUseSubdomain d,first getModeSet c]
  opname is 'monom and args is [v,d,c] =>
    [first getModeSet v,first getModeSetUseSubdomain d,first getModeSet c]
  (opname is 'cons) and (2 = #args) and (second(args) is "nil") =>
    ms := [first getModeSet x for x in args]
    if second(ms) is '(List (None)) then
      ms := [first ms,['List,first ms]]
    ms
  nargs := #args
  v := argCouldBelongToSubdomain(opname,nargs)
  mss := nil
  for i in 0..(nargs-1) for x in args repeat
    ms :=
      v.i = 0 => first getModeSet x
      first getModeSetUseSubdomain x
    mss := [ms,:mss]
  reverse! mss

argCouldBelongToSubdomain(op, nargs) ==
  -- this returns a vector containing 0 or ^0 for each argument.
  -- if ^0, this indicates that there exists a modemap for the
  -- op that needs a subdomain in that position
  nargs = 0 => nil
  v := mkIntArray nargs
  isMap(op) => v
  mms := getModemapsFromDatabase(op,nargs)
  mms = nil => v
  nargs:=nargs-1
  -- each signature has form
  -- [domain of implementation, target, arg1, arg2, ...]
  for [sig,cond,:.] in mms repeat
    for t in CDDR sig for i in 0..nargs repeat
      CONTAINEDisDomain(t,cond) =>
          v.i := 1 + v.i
  v

CONTAINEDisDomain(symbol,cond) ==
-- looks for [isSubDomain,symbol,[domain]] in cond: returning T or nil
-- with domain being one of PositiveInteger and NonNegativeInteger
   cond isnt [.,:.] => false
   cond.op in '(AND OR and or %and %or) =>
       or/[CONTAINEDisDomain(symbol, u) for u in cond.args]
   cond.op is 'isDomain =>
       sameObject?(symbol,second cond) and cons?(dom:=third cond) and
         dom in '(PositiveInteger NonNegativeInteger)
   false

selectDollarMms(dc,name,types1,types2) ==
  -- finds functions for name in domain dc
  isPartialMode dc => throwKeyedMsg("S2IF0001",nil)
  mmS := findFunctionInDomain(name,dc,nil,types1,types2,'T,'T) =>
    orderMms(name, mmS,types1,types2,nil)
  if $reportBottomUpFlag then sayMSG
    ["%b",'"          function not found in ",prefix2String dc,"%d","%l"]
  nil

selectLocalMms(op,name,types,tar) ==
  -- partial rewrite, looks now for exact local modemap
  mmS:= getLocalMms(name,types,tar) => mmS
  obj := getValue op
  obj and (objVal obj is ["%Map",:mapDef]) and
    analyzeMap(op,types,mapDef,tar) and getLocalMms(name,types,tar)

-- next defn may be better, test when more time. RSS 3/11/94
-- selectLocalMms(op,name,types,tar) ==
--  mmS := getLocalMms(name,types,tar)
--  -- if no target, just return what we got
--  mmS and tar = nil => mmS
--  matchingMms := nil
--  for mm in mmS repeat
--    [., targ, :.] := mm
--    if tar = targ then matchingMms := [mm,:matchingMms]
--  -- if we got some exact matchs on the target, return them
--  matchingMms => reverse! matchingMms
--
--  obj := getValue op
--  obj and (objVal obj is ["%Map",:mapDef]) and
--    analyzeMap(op,types,mapDef,tar) and getLocalMms(name,types,tar)

getLocalMms(name,types,tar) ==
  -- looks for exact or subsumed local modemap in $e
  mmS := nil
  for  (mm:=[dcSig,:.]) in get(name,'localModemap,$e) repeat
    -- check format and destructure
    dcSig isnt [dc,result,:args] => nil
    -- make number of args is correct
    #types ~= #args => nil
    -- check for equal or subsumed arguments
    subsume := (not $useIntegerSubdomain) or (tar = result) or
      get(name,'recursive,$e)
    acceptableArgs :=
      and/[f(b,a,subsume) for a in args for b in types] where
        f(x,y,subsume) ==
          if subsume
            then isEqualOrSubDomain(x,y)
            else x = y
    not acceptableArgs =>
      -- interpreted maps are ok
      dc is 'interpOnly and not($Coerce)=> mmS := [mm,:mmS]
      nil
    mmS := [mm,:mmS]
  reverse! mmS

mmCost(name, sig,cond,tar,args1,args2) ==
  cost := mmCost0(name, sig,cond,tar,args1,args2)
  res := second sig
  res = $PositiveInteger => cost - 2
  res = $NonNegativeInteger => cost - 1
  res = $DoubleFloat => cost + 1
  cost

mmCost0(name, sig,cond,tar,args1,args2) ==
  sigArgs := CDDR sig
  n :=
    cond = nil => 1
    not (or/cond) => 1
    0

  -- try to favor homogeneous multiplication

--if name is "*" and 2 = #sigArgs and first sigArgs ~= second sigArgs then n := n + 1

  -- because of obscure problem in evalMm, sometimes we will have extra
  -- modemaps with the wrong number of arguments if we want to the one
  -- with no arguments and the name is overloaded. Thus check for this.

  if args1 then
    for x1 in args1 for x2 in args2 for x3 in sigArgs repeat
      n := n +
        isEqualOrSubDomain(x1,x3) => 0
        topcon := first deconstructT x1
        topcon2 := first deconstructT x3
        topcon = topcon2 => 3
        first topcon2 is 'Mapping => 2
        4
  else if sigArgs then n := n + 100000000000

  res := second sig
  res=tar => 10000*n
  10000*n + 1000*domainDepth(res) + hitListOfTarget(res)

orderMms(name, mmS,args1,args2,tar) ==
  -- it counts the number of necessary coercions of the argument types
  -- if this isn't enough, it compares the target types
  mmS and rest mmS = nil => mmS
  mS:= nil
  N:= nil
  for mm in MSORT mmS repeat
    [sig,.,cond]:= mm
    b:= 'T
    p:= [m := mmCost(name, sig,cond,tar,args1,args2),:mm]
    mS :=
      mS = nil => list p
      m < CAAR mS => [p,:mS]
      S:= mS
      until b repeat
        b := rest S = nil or m < CAADR S =>
          S.rest := [p,:rest S]
        S:= rest S
      mS
  mmS and [rest p for p in mS]

domainDepth(d) ==
  -- computes the depth of lisp structure d
  d isnt [.,:.] => 0
  MAX(domainDepth(first d)+1,domainDepth(rest d))

hitListOfTarget(t) ==
  -- assigns a number between 1 and 998 to a type t

  -- want to make it hard to go to Polynomial Pi

  t = '(Polynomial (Pi)) => 90000

  t.op is 'Polynomial => 300
  t.op is 'List => 400
  t.op is 'Matrix => 910
  t.op is 'UniversalSegment => 501
  t.op is 'RationalFunction => 900
  t.op is 'Union => 999
  t.op is 'Expression => 1600
  500

getFunctionFromDomain(op,dc,args) ==
  -- finds the function op with argument types args in dc
  -- complains, if no function or ambiguous
  $reportBottomUpFlag:local:= nil
  member(dc.op,$nonLisplibDomains) =>
    throwKeyedMsg("S2IF0002",[dc.op])
  not constructor? dc.op =>
    throwKeyedMsg("S2IF0003",[dc.op])
  p:= findFunctionInDomain(op,dc,nil,args,args,nil,nil) =>
--+
    --sig := [nil,:args]
    domain := evalDomain dc
    for mm in reverse! p until b repeat
      [[.,:osig],nsig,:.] := mm
      b := compiledLookup(op,nsig,domain)
    b or  throwKeyedMsg("S2IS0023",[op,dc])
  throwKeyedMsg("S2IF0004",[op,dc])

isOpInDomain(opName,dom,nargs) ==
  -- returns true only if there is an op in the given domain with
  -- the given number of arguments
  mmList := objectAssoc(opName,getConstructorOperationsFromDB dom.op)
  mmList := subCopy(mmList,constructSubst dom)
  mmList = nil => nil
  gotOne := nil
  nargs := nargs + 1
  for mm in rest mmList while not gotOne repeat
    nargs = #first mm => gotOne := [mm, :gotOne]
  gotOne

findCommonSigInDomain(opName,dom,nargs) ==
  -- this looks at all signatures in dom with given opName and nargs
  -- number of arguments. If no matches, returns nil. Otherwise returns
  -- a "signature" where a type position is non-nil only if all
  -- signatures shares that type .
  dom.op in '(Union Record Mapping) => nil
  mmList := objectAssoc(opName,getConstructorOperationsFromDB dom.op)
  mmList := subCopy(mmList,constructSubst dom)
  mmList = nil => nil
  gotOne := nil
  nargs := nargs + 1
  vec := nil
  for mm in rest mmList repeat
    nargs = #first mm =>
      vec = nil  => vec := vector first mm
      for i in 0.. for x in first mm repeat
        if vec.i and vec.i ~= x then vec.i := nil
  VEC2LIST vec

findUniqueOpInDomain(op,opName,dom) ==
  -- return function named op in domain dom if unique, choose one if not
  mmList := objectAssoc(opName,getConstructorOperationsFromDB dom.op)
  mmList := subCopy(mmList,constructSubst dom)
  mmList = nil =>
    throwKeyedMsg("S2IS0021",[opName,dom])
  mmList := rest mmList   -- ignore the operator name
  -- use evaluation type context to narrow down the candidate set
  if target := getTarget op then
    mmList := [mm for mm in mmList | mm is [=rest target,:.]]
    mmList = nil => throwKeyedMsg("S2IS0062",[opName,target,dom])
  if #mmList > 1 then
    mm := selectMostGeneralMm mmList
    sayKeyedMsg("S2IS0022",[opName,dom,['Mapping,:first mm]])
  else mm := first mmList
  [sig,slot,:.] := mm
  fun :=
      $genValue =>
         compiledLookupCheck(opName,sig,evalDomain dom)
      compileEvalForm(opName, sig, evalDomain dom)
  fun=nil or not cons? fun => nil
  first fun = function(Undef) => throwKeyedMsg("S2IS0023",[opName,dom])
  binVal :=
    $genValue => wrap fun
    fun
  putValue(op,objNew(binVal,m:=['Mapping,:sig]))
  putModeSet(op,[m])

selectMostGeneralMm mmList ==
  -- selects the modemap in mmList with arguments all the other
  -- argument types can be coerced to
  -- also selects function with #args closest to 2
  min := 100
  mml := mmList
  while mml repeat
    [mm,:mml] := mml
    sz := #first mm
    if (met := abs(sz - 3)) < min then
      min := met
      fsz := sz
  mmList := [mm for mm in mmList | (#first mm) = fsz]
  mml := rest mmList
  genMm := first mmList
  while mml repeat
    [mm,:mml] := mml
    and/[canCoerceFrom(genMmArg,mmArg) for mmArg in mm.mmSignature
      for genMmArg in genMm.mmSignature] => genMm := mm
  genMm

findFunctionInDomain(op,dc,tar,args1,args2,$Coerce,$SubDom) ==
  -- looks for a modemap for op with signature  args1 -> tar
  --   in the domain of computation dc
  -- tar may be nil (= unknown)
  not isLegitimateMode(tar, nil, nil) => nil
  dcName:= dc.op
  dcName in '(Union Record Mapping Enumeration) =>
    -- First cut code that ignores args2, $Coerce and $SubDom
    -- When domains no longer have to have Set, the hard coded 6 and 7
    -- should go.
    op is '_= =>
        #args1 ~= 2 or args1.0 ~= dc or args1.1 ~= dc => nil
        tar and tar ~= $Boolean => nil
        [[[dc, $Boolean, dc, dc], [$Boolean,'$,'$], [nil, nil]]]
    op is 'coerce =>
        #args1 ~= 1 => nil
        dcName is 'Enumeration and (args1.0=$Symbol or tar=dc)=>
           [[[dc, dc, $Symbol], ['$,$Symbol], [nil, nil]]]
        args1.0 ~= dc => nil
        tar and tar ~= $OutputForm => nil
        [[[dc, $OutputForm, dc], [$OutputForm,'$], [nil, nil]]]
    dcName in '(Record Union) =>
      findFunctionInCategory(op,dc,tar,args1,args2,$Coerce,$SubDom)
    nil
  fun:= nil
  ( p := objectAssoc(op,getConstructorOperationsFromDB dcName) ) and
    SL := constructSubst dc
    -- if the arglist is homogeneous, first look for homogeneous
    -- functions. If we don't find any, look at remaining ones
    if isHomogeneousList args1 then
      q := nil
      r := nil
      for mm in rest p repeat
        if isHomogeneousList mm.mmSignature then q := [mm,:q]
        else r := [mm,:r]
      q := allOrMatchingMms(q,args1,tar,dc)
      for mm in q repeat
        fun:= append!(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
      r := reverse r
    else r := rest p
    r := allOrMatchingMms(r,args1,tar,dc)
    if not fun then    -- consider remaining modemaps
      for mm in r repeat
        fun:= append!(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
  if not fun and $reportBottomUpFlag then
    sayMSG concat
      ['"   -> no appropriate",:bright op,'"found in",
        :bright prefix2String dc]
  fun

allOrMatchingMms(mms,args1,tar,dc) ==
  -- if there are exact matches on the arg types, return them
  -- otherwise return the original list
  mms = nil or rest mms = nil => mms
  x := nil
  for mm in mms repeat
    [sig,:.] := mm
    [res,:args] := substitute(dc,"$",sig)
    args ~= args1 => nil
    x := [mm,:x]
  if x then x
  else mms

isHomogeneousList y ==
  y is [x] => true
  y and rest y =>
    z := first y
    "and"/[x = z for x in rest y]
  nil

findFunctionInDomain1(omm,op,tar,args1,args2,SL) ==
  dc := rest (dollarPair := objectAssoc('$,SL))
  -- need to drop '$ from SL
  mm:= subCopy(omm, SL)
  -- tests whether modemap mm is appropriate for the function
  -- defined by op, target type tar and argument types args
  $RTC:local:= nil
  -- $RTC is a list of run-time checks to be performed

  [sig,slot,cond,y] := mm
  [osig,:.]  := omm
  osig := subCopy(osig, substitute(['$,:'$], dollarPair, SL))
  if CONTAINED('_#, sig) or CONTAINED('construct,sig) then
    sig := [replaceSharpCalls t for t in sig]
  matchMmCond cond and matchMmSig(mm,tar,args1,args2) and
    y is "Subsumed" and
      -- hmmmm: do Union check in following because (as in DP)
      -- Unions are subsumed by total modemaps which are in the
      -- mm list in findFunctionInDomain.
      y := 'ELT      -- if subsumed fails try it again
      not $SubDom and first sig isnt ['Union,:.] and slot is [tar,:args] and
        (f := findFunctionInDomain(op,dc,tar,args,args,nil,nil)) => f
    y is 'ELT => [[[dc,:sig],osig,reverse! $RTC]]
    y is 'CONST => [[[dc,:sig],osig,reverse! $RTC]]
    y is 'ASCONST => [[[dc,:sig],osig,reverse! $RTC]]
    y is ['XLAM,:.] => [[[dc,:sig],y,reverse! $RTC]]
    sayKeyedMsg("S2IF0006",[y])
    nil

findFunctionInCategory(op,dc,tar,args1,args2,$Coerce,$SubDom) ==
  -- looks for a modemap for op with signature  args1 -> tar
  --   in the domain of computation dc
  -- tar may be nil (= unknown)
  dcName := dc.op
  not (dcName in '(Record Union Enumeration)) => nil
  fun:= nil
 --  cat := constructorCategory dc
  makeFunc := property(dcName,"makeFunctionList") or
      systemErrorHere ["findFunctionInCategory",dcName]
  [funlist,.] := apply(makeFunc,["$",dc,$CategoryFrame])
  -- get list of implementations and remove sharps
  maxargs := -1
  impls := nil
  for [a,b,d] in funlist repeat
    not sameObject?(a,op) => nil
    d is ['XLAM,xargs,:.] =>
      if cons?(xargs) then maxargs := MAX(maxargs,#xargs)
      else maxargs := MAX(maxargs,1)
      impls := [[b,nil,true,d],:impls]
    d isnt [k,"$",n] => systemErrorHere ["findFunctionInCategory",d]
    impls := [[b,n,true,k],:impls]
  impls := reverse! impls
  if maxargs ~= -1 then
    SL:= nil
    for i in 1..maxargs repeat
      impls := substitute(gensym(),makeSymbol strconc('"#",toString i),impls)
  impls and
    SL:= constructSubst dc
    for mm in impls repeat
      fun:= append!(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
  if not fun and $reportBottomUpFlag then
    sayMSG concat
      ['"   -> no appropriate",:bright op,'"found in",
        :bright prefix2String dc]
  fun

matchMmCond(cond) ==
  -- tests the condition, which comes with a modemap
  -- cond is 'T or a list, but I hate to test for 'T (ALBI)
  $domPvar: local := nil
  cond isnt [.,:.] or 
    cond.op in '(AND and %and) =>
      and/[matchMmCond c for c in cond.args]
    cond.op in '(OR or %or) =>
      or/[matchMmCond c for c in cond.args]
    cond is ["has",dom,x] =>
      hasCaty(dom,x,nil) isnt 'failed
    cond is [op,cond1] and op in '(not NOT %not) => not matchMmCond cond1
    keyedSystemError("S2GE0016",
      ['"matchMmCond",'"unknown form of condition"])

matchMmSig(mm,tar,args1,args2) ==
  -- matches the modemap signature against  args1 -> tar
  -- if necessary, runtime checks are created for subdomains
  -- then the modemap condition is evaluated
  [sig,:.]:= mm
  if CONTAINED('_#, sig) then
    sig := [replaceSharpCalls copyTree t for t in sig]
  args1 = nil => matchMmSigTar(tar,first sig)
  a:= rest sig
  arg:= nil
  for i in 1.. while args1 and args2 and a until not b repeat
    x1:= first args1
    args1:= rest args1
    x2:= first args2
    args2:= rest args2
    x:= first a
    a:= rest a
    rtc:= nil
    if x is ['SubDomain,y,:.] then x:= y
    b := isEqualOrSubDomain(x1,x) or
      (string?(x) and (x1 is ['Variable,v]) and (x = PNAME v)) or
        $SubDom and isSubDomain(x,x1) => rtc:= 'T
        $Coerce => x2=x or canCoerceFrom(x1,x)
        x1 is ['Variable,:.] and x = $Symbol
    $RTC:= [rtc,:$RTC]
  args1 = nil and a = nil and b and matchMmSigTar(tar,first sig)

matchMmSigTar(t1,t2) ==
  -- t1 is a target type specified by :: or by a declared variable
  -- t2 is the target of a modemap signature
  t1 = nil or
    isEqualOrSubDomain(t2,t1) => true
    if t2 is ['Union,a,b] then
      if a is '"failed" then return matchMmSigTar(t1, b)
      if b is '"failed" then return matchMmSigTar(t1, a)
    $Coerce and
      isPartialMode t1 => resolveTM(t2,t1)
-- I think this should be true  -SCM
--    true
      canCoerceFrom(t2,t1)

constructSubst(d) ==
  -- constructs a substitution which substitutes d for $
  -- and the arguments of d for #1, #2 ..
  SL:= list ['$,:d]
  for x in rest d for i in 1.. repeat
    SL:= [[makeSymbol strconc('"#",toString i),:x],:SL]
  SL

filterModemapsFromPackages(mms, names, op) ==
  -- mms is a list of modemaps
  -- names is a list of domain constructors
  -- this returns a 2-list containing those modemaps that have one
  -- of the names in the package source of the modemap and all the
  -- rest of the modemaps in the second element.
  good := nil
  bad  := nil
  -- hack to speed up factorization choices for mpolys and to overcome
  -- some poor naming of packages
  mpolys := '("Polynomial" "MultivariatePolynomial"
   "DistributedMultivariatePolynomial"
      "HomogeneousDistributedMultivariatePolynomial")
  mpacks := '("MFactorize" "MRationalFactorize")
  for mm in mms repeat
    isFreeFunctionFromMm(mm) => bad := [mm,:bad]
    type := getDomainFromMm mm
    type = nil => bad := [mm,:bad]
    if cons? type then type := first type
    getConstructorKindFromDB type is "category" => bad := [mm,:bad]
    name := object2String type
    found := nil
    for n in names while not found repeat
      findString(n,name) => found := true
      -- hack, hack
      (op is 'factor) and member(n,mpolys) and member(name,mpacks) =>
        found := true
    if found
      then good := [mm,:good]
      else bad := [mm,:bad]
  [good,bad]


isTowerWithSubdomain(towerType,elem) ==
  towerType isnt [.,:.] => nil
  dt := deconstructT towerType
  2 ~= #dt => nil
  s := underDomainOf(towerType)
  isEqualOrSubDomain(s,elem) and constructM(first dt,[elem])

selectMmsGen(op,tar,args1,args2) ==
  -- general modemap evaluation of op with argument types args1
  -- evaluates the condition and looks for the slot number
  -- returns all functions which are applicable
  -- args2 is a list of polynomial types for symbols
  $Subst: local := nil
  $SymbolType: local := nil

  null (S := getModemapsFromDatabase(op,#args1)) => nil

  if (op is 'map) and (2 = #args1) and
    (first(args1) is ['Mapping,., elem]) and
      (a := isTowerWithSubdomain(second args1,elem))
        then args1 := [first args1,a]

  -- we first split the modemaps into two groups:
  --   haves:    these are from packages that have one of the top level
  --             constructor names in the package name
  --   havenots: everything else

  -- get top level constructor names for constructors with parameters
  conNames := nil
  if op is 'reshape then args := append(rest args1, rest args2)
  else args := append(args1,args2)
  if tar then args := [tar,:args]
  -- for common aggregates, use under domain also
  for a in removeDuplicates args repeat
    a =>
      a isnt [.,:.] => nil
      fa := a.op
      fa in '(Record Union) => nil
      conNames := insert(STRINGIMAGE fa, conNames)

  if conNames
    then [haves,havenots] := filterModemapsFromPackages(S,conNames,op)
    else
      haves := nil
      havenots := S

  mmS := nil

  if $reportBottomUpFlag then
    sayMSG ['"%l",:bright '"Modemaps from Associated Packages"]

  if haves then
    [havesExact,havesInexact] := exact?(haves,tar,args1)
    if $reportBottomUpFlag then
      for mm in append(havesExact,havesInexact) for i in 1.. repeat
        sayModemapWithNumber(mm,i)
    if havesExact then
      mmS := matchMms(havesExact,op,tar,args1,args2)
      if mmS then
        if $reportBottomUpFlag then
          sayMSG '"   found an exact match!"
        return mmS
    mmS := matchMms(havesInexact,op,tar,args1,args2)
  else if $reportBottomUpFlag then sayMSG '"   no modemaps"
  mmS => mmS

  if $reportBottomUpFlag then
    sayMSG ['"%l",:bright '"Remaining General Modemaps"]
  --  for mm in havenots for i in 1.. repeat sayModemapWithNumber(mm,i)

  if havenots then
    [havesNExact,havesNInexact] := exact?(havenots,tar,args1)
    if $reportBottomUpFlag then
      for mm in append(havesNExact,havesNInexact) for i in 1.. repeat
        sayModemapWithNumber(mm,i)
    if havesNExact then
      mmS := matchMms(havesNExact,op,tar,args1,args2)
      if mmS then
        if $reportBottomUpFlag then
          sayMSG '"   found an exact match!"
        return mmS
    mmS := matchMms(havesNInexact,op,tar,args1,args2)
  else if $reportBottomUpFlag then sayMSG '"   no modemaps"
  mmS
 where
  exact?(mmS,tar,args) ==
    ex := inex := nil
    for (mm := [sig,[mmC,:.],:.]) in mmS repeat
      [c,t,:a] := sig
      ok := true
      for pat in a for arg in args while ok repeat
        not CONTAINED(['isDomain,pat,arg],mmC) => ok := nil
      ok => ex := [mm,:ex]
      inex := [mm,:inex]
    [ex,inex]
  matchMms(mmaps,op,tar,args1,args2) ==
    mmS := nil
    for [sig,mmC] in mmaps repeat
      -- sig is [dc,result,:args]
      $Subst :=
        tar and not isPartialMode tar =>
          -- throw in the target if it is not the same as one
          -- of the arguments
          res := second sig
          member(res,CDDR sig) => nil
          [[res,:tar]]
        nil
      [c,t,:a] := sig
      if a then matchTypes(a,args1,args2)
      $Subst isnt 'failed =>
        mmS := append!(evalMm(op,tar,sig,mmC),mmS)
    mmS

matchTypes(pm,args1,args2) ==
  -- pm is a list of pattern variables, args1 a list of argument types,
  --   args2 a list of polynomial types for symbols
  -- the result is a match from pm to args, if one exists
  for v in pm for t1 in args1 for t2 in args2 until $Subst is 'failed repeat
    p := objectAssoc(v,$Subst) =>
      t:= rest p
      t=t1 => $Coerce and t1 = $Symbol and
        (q := objectAssoc(v,$SymbolType)) and t2 and
          (t3 := resolveTT(rest q, t2)) and
            (q.rest := t3)
      $Coerce =>
        if t = $Symbol and (q := objectAssoc(v,$SymbolType)) then
          t := rest q
        if t1 = $Symbol and t2 then t1:= t2
        t0 := resolveTT(t,t1) => p.rest := t0
        $Subst:= 'failed
      $Subst:= 'failed
    $Subst:= [[v,:t1],:$Subst]
    if t1 = $Symbol and t2 then $SymbolType:= [[v,:t2],:$SymbolType]

evalMm(op,tar,sig,mmC) ==
  -- evaluates a modemap with signature sig and condition mmC
  -- the result is a list of lists [sig,slot,cond] or nil
  --if $Coerce is nil, tar has to be the same as the computed target type
--if CONTAINED('LinearlyExplicitRingOver,mmC) then hohoho()
  mS:= nil
  for st in evalMmStack mmC repeat
    SL:= evalMmCond(op,sig,st)
    SL isnt 'failed =>
      SL := fixUpTypeArgs SL
      sig:= [subCopy(deepSubCopy(x,SL),$Subst) for x in sig]
      not containsVars sig =>
        isFreeFunctionFromMmCond mmC and (m := evalMmFreeFunction(op,tar,sig,mmC)) =>
           mS:= append!(m,mS)
        "or"/[not isValidType(arg) for arg in sig] => nil
        [dc,t,:args]:= sig
        $Coerce or tar = nil or tar=t =>
          mS:= append!(findFunctionInDomain(op,dc,t,args,args,nil,'T),mS)
  mS

evalMmFreeFunction(op,tar,sig,mmC) ==
  [dc,t,:args]:= sig
  $Coerce or tar = nil or tar=t =>
     nilArgs := nil
     for a in args repeat nilArgs := [nil,:nilArgs]
     [[[["__FreeFunction__",:dc],t,:args], [t, :args], nilArgs]]
  nil

evalMmStack(mmC) ==
  -- translates the modemap condition mmC into a list of stacks
  mmC is [op,:a] and op in '(AND and %and) =>
    ["append!"/[evalMmStackInner cond for cond in a]]
  mmC is [op,:args] and op in '(OR or %or) => 
    [:evalMmStack a for a in args]
  mmC is ['partial,:mmD] => evalMmStack mmD
  mmC is ['ofCategory,pvar,cat] and cat is ['Join,:args] =>
    evalMmStack ['%and,:[['ofCategory,pvar,c] for c in args]]
  mmC is ['ofType,:.] => [nil]
  mmC is ["has",pat,x] =>
    x in '(ATTRIBUTE SIGNATURE) =>
      [[['ofCategory,pat,['CATEGORY,'unknown,x]]]]
    [['ofCategory,pat,x]]
  [[mmC]]

evalMmStackInner(mmC) ==
  mmC is [op,:args] and op in '(OR or %or) =>
    keyedSystemError("S2GE0016",
      ['"evalMmStackInner",'"OR condition nested inside an AND"])
  mmC is ['partial,:mmD] => evalMmStackInner mmD
  mmC is ['ofCategory,pvar,cat] and cat is ['Join,:args] =>
    [['ofCategory, pvar, c] for c in args]
  mmC is ['ofType,:.] => nil
  mmC is ['isAsConstant] => nil
  mmC is ["has",pat,x] =>
    x in '(ATTRIBUTE SIGNATURE) =>
      [['ofCategory,pat,['CATEGORY,'unknown,x]]]
    [['ofCategory,pat,x]]
  [mmC]

evalMmCond(op,sig,st) ==
  $insideEvalMmCondIfTrue : local := true
  evalMmCond0(op,sig,st)

evalMmCond0(op,sig,st) ==
  -- evaluates the nonempty list of modemap conditions st
  -- the result is either 'failed or a substitution list
  SL := evalMmDom st
  SL is 'failed => 'failed
  for p in SL until p1 ~= nil and not b repeat b:=
    p1 := objectAssoc(first p,$Subst)
    p1 and
      t1:= rest p1
      t:= rest p
      t=t1 or
        containsVars t =>
          if $Coerce and t1 = $Symbol then t1:= getSymbolType first p
          resolveTM1(t1,t)
        $Coerce and
          -- if we are looking at the result of a function, the coerce
          -- goes the opposite direction
          (t1 = $AnonymousFunction and t is ['Mapping, :.]) => t
          first p = second sig and not member(first p, CDDR sig) =>
            canCoerceFrom(t,t1) => 'T
            nil
          canCoerceFrom(t1,t) => 'T
          isSubDomain(t,t1) => p.rest := t1
          t1 = $Symbol and canCoerceFrom(getSymbolType first p,t)
  SL ~= nil and p1 ~= nil and not b => 'failed
  evalMmGuard(op,sig,st,SL)

fixUpTypeArgs SL ==
  for (p := [v, :t2]) in SL repeat
    t1 := LASSOC(v, $Subst)
    t1 = nil => p.rest := replaceSharpCalls t2
    p.rest := coerceTypeArgs(t1, t2, SL)
  SL

replaceSharpCalls t ==
  noSharpCallsHere t => t
  doReplaceSharpCalls t

doReplaceSharpCalls t ==
  t isnt [.,:.] => t
  t is ['_#, l] => #l
  t is ['construct,: l] => eval ['LIST,:l]
  [first t,:[ doReplaceSharpCalls u for u in rest t]]

noSharpCallsHere t ==
  t isnt [con, :args] => true
  con in '(construct _#) => nil
  and/[noSharpCallsHere u for u in args]

coerceTypeArgs(t1, t2, SL) ==
  -- if the type t has type-valued arguments, coerce them to the new types,
  -- if needed.
  t1 isnt [con1, :args1] or t2 isnt [con2, :args2] => t2
  con1 ~= con2 => t2
  coSig := rest getDualSignature first t1
  and/coSig => t2
  csub1 := constructSubst t1
  csub2 := constructSubst t2
  cs1 := rest getConstructorSignature con1
  cs2 := rest getConstructorSignature con2
  [con1, :
    [makeConstrArg(arg1, arg2, constrArg(c1,csub1,SL),
      constrArg(c2,csub2,SL), cs)
       for arg1 in args1 for arg2 in args2 for c1 in cs1 for c2 in cs2
         for cs in coSig]]

constrArg(v,sl,SL) ==
  x := LASSOC(v,sl) =>
    y := LASSOC(x,SL) => y
    y := LASSOC(x, $Subst) => y
    x
  y := LASSOC(x, $Subst) => y
  v

makeConstrArg(arg1, arg2, t1, t2, cs) ==
  if arg1 is ['_#, l] then arg1 := # l
  if arg2 is ['_#, l] then arg2 := # l
  cs => arg2
  t1 = t2 => arg2
  obj1 := objNewWrap(arg1, t1)
  obj2 := coerceInt(obj1, t2)
  obj2 = nil => throwKeyedMsgCannotCoerceWithValue(wrap arg1,t1,t2)
  objValUnwrap obj2

evalMmDom(st) ==
  -- evals all isDomain(v,d) of st
  SL:= nil
  for mmC in st until SL is 'failed repeat
    mmC is ['isDomain,v,d] =>
      string? d => SL:= 'failed
      (p := objectAssoc(v,SL)) and d ~= rest p => SL:= 'failed
      d1:= subCopy(d,SL)
      cons?(d1) and symbolMember?(v,d1) => SL:= 'failed
      SL:= augmentSub(v,d1,SL)
    mmC is ['isFreeFunction,v,fun] =>
      SL:= augmentSub(v,subCopy(fun,SL),SL)
  SL

orderMmCatStack st ==
  -- tries to reorder stack so that free pattern variables appear
  -- as parameters first
  st = nil or rest(st) = nil => st
  vars := removeDuplicates [v for [.,v,.] in st | isPatternVar v]
  vars = nil => st
  havevars := nil
  haventvars := nil
  for s in st repeat
    cat := third s
    mem := nil
    for v in vars while not mem repeat
      if symbolMember?(v,cat) then
        mem := true
        havevars := [s,:havevars]
    if not mem then haventvars := [s,:haventvars]
  havevars = nil => st
  st := reverse! append!(haventvars,havevars)
  SORT(st, function mmCatComp)

mmCatComp(c1, c2) ==
  b1 := objectAssoc(second c1, $Subst)
  b2 := objectAssoc(second c2, $Subst)
  b1 and b2 = nil => true
  false

++ Evaluate the non-domain equality constraints of the predicates,
++ given by `stack', guarding the selection of operator `op' with
++ signature `sig'.  If successful, return the resulting augmented
++ substitution `SL', otherwise failed.
evalMmGuard(op,sig,stack,SL) ==
  SL := evalMmCat(op,sig,
          orderMmCatStack [c for c in stack | c is ['ofCategory,:.]],SL)
  SL is 'failed => 'failed
  stack := [c for c in stack | c is ['%exist,:.]] or return SL
  and/[SL := check(op,sig,vars,conds,SL) for [.,vars,conds] in stack
         | SL isnt 'failed or leave 'failed] where
            check(op,sig,vars,conds,SL) ==
              -- Each query variable must have an assigned value
              and/[symbolTarget(v,SL) for v in vars] or return 'failed
              -- All constraints on query variables must be satisfied.
              evalMmCat(op,sig,
                orderMmCatStack [c for c in conds | c is ['ofCategory,:.]],SL)

++ Like evalMmGuard, but evaluate only category satisfaction contraints.
evalMmCat(op,sig,stack,SL) ==
  -- evaluates all ofCategory's of stack as soon as possible
  $hope:local:= nil
  while stack until not makingProgress repeat
    st := stack
    stack := nil
    makingProgress := nil
    for mmC in st repeat
      S:= evalMmCat1(mmC,op, SL)
      S is 'failed and $hope =>
        stack:= [mmC,:stack]
      S is 'failed => return S
      cons? S =>
        makingProgress:= 'T
        SL:= mergeSubs(S,SL)
  if stack or S is 'failed then 'failed else SL

evalMmCat1(mmC is ['ofCategory,d,c],op, SL) ==
  -- evaluates mmC using information from the lisplib
  -- d may contain variables, and the substitution list $Subst is used
  -- the result is a substitution or failed
  $domPvar: local := nil
  $hope:= nil
  NSL:= hasCate(d,c,SL)
  NSL is 'failed and isPatternVar d and $Coerce and ( p:= objectAssoc(d,$Subst) )
    and (rest(p) is ["Variable",:.] or rest(p) = $Symbol) =>
      p.rest := getSymbolType d
      hasCate(d,c,SL)
  NSL is 'failed and isPatternVar d =>
    -- following is hack to take care of the case where we have a
    -- free substitution variable with a category condition on it.
    -- This would arise, for example, where a package has an argument
    -- that is not in a needed modemap.  After making the following
    -- dummy substitutions, the package can be instantiated and the
    -- modemap used.       RSS 12-22-85
    -- If c is not Set, Ring or Field then the more general mechanism
    dom := defaultTypeForCategory(c, SL)
    dom = nil =>
      op isnt 'coerce => 'failed
    null (p := objectAssoc(d,$Subst)) =>
      dom =>
        NSL := [[d,:dom]]
      op isnt 'coerce => 'failed
    if containsVars dom then dom := resolveTM(rest p, dom)
    $Coerce and canCoerce(rest p, dom) =>
      NSL := [[d,:dom]]
    op isnt 'coerce => 'failed
  NSL

hasCate(dom,cat,SL) ==
  -- asks whether dom has cat under SL
  -- augments substitution SL or returns 'failed
  dom = $EmptyMode => nil
  isPatternVar dom =>
    (p:= objectAssoc(dom,SL)) and ((NSL := hasCate(rest p,cat,SL)) isnt 'failed) =>
       NSL
    (p:= objectAssoc(dom,$Subst)) or (p := objectAssoc(dom, SL)) =>
--      S:= hasCate(rest p,cat,augmentSub(first p,rest p,copy SL))
      S:= hasCate1(rest p,cat,SL, dom)
      S isnt 'failed => S
      hasCateSpecial(dom,rest p,cat,SL)
    if SL isnt 'failed then $hope:= 'T
    'failed
  SL1 := [[v,:d] for [v,:d] in SL | not containsVariables d]
  if SL1 then cat := subCopy(cat, SL1)
  -- Replace query variables by their values and try again
  ident? dom and queryVar? dom =>
    dom' := symbolTarget(dom,SL) =>
      hasCate(dom',substitute(dom',dom,cat),SL)
    'failed
  hasCaty(dom,cat,SL)

hasCate1(dom, cat, SL, domPvar) ==
  $domPvar:local := domPvar
  hasCate(dom, cat, SL)

hasCateSpecial(v,dom,cat,SL) ==
  -- v is a pattern variable, dom it's binding under $Subst
  -- tries to change dom, so that it has category cat under SL
  -- the result is a substitution list or 'failed
  dom is ['FactoredForm,arg] =>
    if isSubDomain(arg,$Integer) then arg := $Integer
    d := ['FactoredRing,arg]
    SL:= hasCate(arg,$Ring,augmentSub(v,d,SL))
    SL is 'failed => 'failed
    hasCaty(d,cat,SL)
  cat = $Field or cat = $DivisionRing =>
    if isSubDomain(dom,$Integer) then dom := $Integer
    d:= eqType [$QuotientField, dom]
    hasCaty(dom,$IntegralDomain,augmentSub(v,d,SL))
  cat is ['PolynomialCategory, d, :.] =>
    dom' := ['Polynomial, d]
    (containsVars d or canCoerceFrom(dom, dom'))
       and hasCaty(dom', cat, augmentSub(v,dom',SL))
  isSubDomain(dom,$Integer) =>
    NSL:= hasCate($Integer,cat,augmentSub(v,$Integer,SL))
    NSL is 'failed =>
      hasCateSpecialNew(v, dom, cat, SL)
    hasCaty($Integer,cat,NSL)
  hasCateSpecialNew(v, dom, cat, SL)

-- to be used in $newSystem only
hasCateSpecialNew(v,dom,cat,SL) ==
  fe := cat.op in '(ElementaryFunctionCategory
       TrigonometricFunctionCategory ArcTrigonometricFunctionCategory
        HyperbolicFunctionCategory ArcHyperbolicFunctionCategory
         PrimitiveFunctionCategory SpecialFunctionCategory Evalable
          CombinatorialOpsCategory TranscendentalFunctionCategory
           AlgebraicallyClosedFunctionSpace ExpressionSpace
             LiouvillianFunctionCategory FunctionSpace)
  alg := cat.op in '(RadicalCategory AlgebraicallyClosedField)
  fefull := fe or alg or cat = $CombinatorialFunctionCategory
  partialResult :=
    dom is ["Variable",:.] or dom = $Symbol =>
      first(cat) in
       '(SemiGroup AbelianSemiGroup Monoid AbelianGroup AbelianMonoid
         PartialDifferentialRing Ring InputForm) =>
                d := ['Polynomial, $Integer]
                augmentSub(v, d, SL)
      cat = $Group =>
        d := ['Fraction, ['Polynomial, $Integer]]
        augmentSub(v, d, SL)
      fefull =>
        d := defaultTargetFE dom
        augmentSub(v, d, SL)
      'failed
    isEqualOrSubDomain(dom, $Integer) =>
      fe =>
        d := defaultTargetFE $Integer
        augmentSub(v, d, SL)
      alg =>
        d := $AlgebraicNumber
        --d := defaultTargetFE $Integer
        augmentSub(v, d, SL)
      'failed
    underDomainOf dom = $ComplexInteger =>
      d := defaultTargetFE $ComplexInteger
      hasCaty(d,cat,augmentSub(v, d, SL))
    (dom = $RationalNumber) and alg =>
      d := '(AlgebraicNumber)
      --d := defaultTargetFE $Integer
      augmentSub(v, d, SL)
    fefull =>
      d := defaultTargetFE dom
      augmentSub(v, d, SL)
    'failed
  partialResult is 'failed => 'failed
  hasCaty(d, cat, partialResult)

hasCaty(d,cat,SL) ==
  -- calls hasCat, which looks up a hashtable and returns:
  -- 1. T, nil or a (has x1 x2) condition, if cat is not parameterized
  -- 2. a list of pairs (argument to cat,condition) otherwise
  -- then the substitution SL is augmented, or the result is 'failed
  cat is ['CATEGORY,.,:y] => hasAttSig(d,subCopy(y,constructSubst d),SL)
  cat is ['SIGNATURE,foo,sig,:.] =>
    hasSig(d,foo,subCopy(sig,constructSubst d),SL)
  cat is ['ATTRIBUTE,a] => hasAtt(d,subCopy(a,constructSubst d),SL)
  cat is ["Join",:.] =>
    for c in cat.args while SL isnt "failed" repeat
      SL := hasCaty(d,c,SL)
    SL
  x := (cons? d and cons? cat and hasCat(d,cat)) =>
    y := KDR cat =>
      S := constructSubst d
      for [z,:cond] in x until S1 isnt 'failed repeat
        S' := [[p, :mkDomPvar(p, d, z, y)] for [p,:d] in S]
        if $domPvar then
          dom := [d.op, :[domArg(arg, i, z, y) for i in 0..
                           for arg in d.args]]
          SL := augmentSub($domPvar, dom, copy SL)
        z' := [domArg2(a, S, S') for a in z]
        S1:= unifyStruct(y,z',copy SL)
        if S1 isnt 'failed then S1:=
          cond isnt [.,:.] => S1
          ncond := subCopy(cond, S)
          ncond is ["has", =d, =cat] => 'failed
          hasCaty1(ncond,S1)
      S1
    x isnt [.,:.] => SL
    ncond := subCopy(x, constructSubst d)
    ncond is ["has", =d, =cat] => 'failed
    hasCaty1(ncond, SL)
  'failed

mkDomPvar(p, d, subs, y) ==
  l := upwardCut(p,$FormalMapVariableList) =>
    domArg(d, #$FormalMapVariableList - #l, subs, y)
  d

domArg(type, i, subs, y) ==
  p := upwardCut($FormalMapVariableList.i, subs) =>
    y.(#subs - #p)
  type

domArg2(arg, SL1, SL2) ==
  isSharpVar arg => subCopy(arg, SL1)
  arg is '$ and $domPvar => $domPvar
  subCopy(arg, SL2)

hasCaty1(cond,SL) ==
  -- cond is either a (has a b) or an OR clause of such conditions
  -- SL is augmented, if cond is true, otherwise the result is 'failed
  $domPvar: local := nil
  cond is ["has",a,b] => hasCate(a,b,SL)
  cond is [op,:args] and op in '(AND and %and) =>
    for x in args while S isnt 'failed repeat S:=
      x is ["has",a,b] => hasCate(a,b, SL)
      -- next line is for an obscure bug in the table
      x is [["has",a,b]] => hasCate(a,b, SL)
      --'failed
      hasCaty1(x, SL)
    S
  cond is [op,:args] and op in '(OR or %or) =>
    for x in args until S isnt 'failed repeat S:=
      x is ["has",a,b] => hasCate(a,b,copy SL)
      -- next line is for an obscure bug in the table
      x is [["has",a,b]] => hasCate(a,b,copy SL)
      --'failed
      hasCaty1(x, copy SL)
    S
  keyedSystemError("S2GE0016",
    ['"hasCaty1",'"unexpected condition from category table"])

hasAttSig(d,x,SL) ==
  -- d is domain, x a list of attributes and signatures
  -- the result is an augmented SL, if d has x, 'failed otherwise
  for y in x until SL is 'failed repeat SL:=
    y is ['ATTRIBUTE,a] => hasAtt(d,a,SL)
    y is ['SIGNATURE,foo,s,:.] => hasSig(d,foo,s,SL)
    keyedSystemError("S2GE0016",
      ['"hasAttSig",'"unexpected form of unnamed category"])
  SL

hasSigAnd(andCls, S0, SL) ==
  dead := nil
  SA := 'failed
  for cls in andCls while not dead repeat
    SA :=
      cls isnt [.,:.] => copy SL
      cls is ["has",a,b] =>
        hasCate(subCopy(a,S0),subCopy(b,S0),copy SL)
      keyedSystemError("S2GE0016",
        ['"hasSigAnd",'"unexpected condition for signature"])
    if SA is 'failed then dead := true
  SA

hasSigOr(orCls, S0, SL) ==
  found := nil
  SA := 'failed
  for cls in orCls until found repeat
    SA :=
      cls isnt [.,:.] => copy SL
      cls is ["has",a,b] =>
        hasCate(subCopy(a,S0),subCopy(b,S0),copy SL)
      cls is [op,:andCls] and op in '(AND and %and) =>
        hasSigAnd(andCls, S0, SL)
      keyedSystemError("S2GE0016",
        ['"hasSigOr",'"unexpected condition for signature"])
    if SA isnt 'failed then found := true
  SA

hasSig(dom,foo,sig,SL) ==
  -- tests whether domain dom has function foo with signature sig
  -- under substitution SL
  $domPvar: local := nil
  fun:= getConstructorAbbreviationFromDB dom.op =>
    S0:= constructSubst dom
    p := objectAssoc(foo,getConstructorOperationsFromDB dom.op) =>
      for [x,.,cond,.] in rest p until S isnt 'failed repeat
        S:=
          cond isnt [.,:.] => copy SL
          cond is ["has",a,b] =>
            hasCate(subCopy(a,S0),subCopy(b,S0),copy SL)
          cond is [op,:andCls] and op in '(AND and %and) =>
            hasSigAnd(andCls, S0, SL)
          cond is [op,:orCls] and op in '(OR or %or) =>
            hasSigOr(orCls, S0, SL)
          keyedSystemError("S2GE0016",
             ['"hasSig",'"unexpected condition for signature"])
        S isnt 'failed => S:= unifyStruct(subCopy(x,S0),sig,S)
      S
    'failed
  'failed

hasAtt(dom,att,SL) ==
  -- tests whether dom has attribute att under SL
  -- needs S0 similar to hasSig above ??
  $domPvar: local := nil
  fun := dom.op =>
    atts:= subCopy(getConstructorAttributes fun,constructSubst dom) =>
      cons? (u := getInfovec dom.op) =>
        --UGH! New world has attributes stored as pairs not as lists!!
        for [x,:cond] in atts until S isnt 'failed repeat
          S:= unifyStruct(x,att,copy SL)
          cons? cond and S isnt 'failed => S := hasCatExpression(cond,S)
        S
      for [x,cond] in atts until S isnt 'failed repeat
        S:= unifyStruct(x,att,copy SL)
        cons? cond and S isnt 'failed => S := hasCatExpression(cond,S)
      S
    'failed
  'failed

hasCatExpression(cond,SL) ==
  cond is [op,:l] and op in '(OR or %or) =>
    or/[(y:=hasCatExpression(x,SL)) isnt 'failed for x in l] => y
  cond is [op,:l] and op in '(AND and %and) =>
    and/[(SL:= hasCatExpression(x,SL)) isnt 'failed for x in l] => SL
  cond is ["has",a,b] => hasCate(a,b,SL)
  keyedSystemError("S2GE0016",
    ['"hasSig",'"unexpected condition for attribute"])

unifyStruct(s1,s2,SL) ==
  -- tests for equality of s1 and s2 under substitutions SL and $Subst
  -- the result is a substitution list or 'failed
  s1=s2 => SL
  if s1 is [":",x,.] then s1:= x
  if s2 is [":",x,.] then s2:= x
  if cons? s1 and first s1 is '_# then s1:= # second s1
  if cons? s2 and first s2 is '_# then s2:= # second s2
  s1=s2 => SL
  isPatternVar s1 => unifyStructVar(s1,s2,SL)
  isPatternVar s2 => unifyStructVar(s2,s1,SL)
  ident? s1 and queryVar? s1 => unifyQueryStruct(s1,s2,SL)
  ident? s2 and queryVar? s2 => unifyQueryStruct(s2,s1,SL)
  s1 isnt [.,:.] or s2 isnt [.,:.] => 'failed
  until s1 = nil or s2 = nil or SL is 'failed repeat
    SL:= unifyStruct(first s1,first s2,SL)
    s1:= rest s1
    s2:= rest s2
  s1 or s2 => 'failed
  SL

unifyStructVar(v,s,SL) ==
  -- the first argument is a pattern variable, which is not substituted
  -- by SL
  CONTAINED(v,s) => 'failed
  ps := LASSOC(s, SL)
  s1 := (ps => ps; s)
  (s0 := LASSOC(v, SL)) or (s0 := LASSOC(v,$Subst)) =>
    S:= unifyStruct(s0,s1,copy SL)
    S is 'failed =>
      $Coerce and cons? s0 and constructor? s0.op =>
        containsVars s0 or containsVars s1 =>
          ns0 := subCopy(s0, SL)
          ns1 := subCopy(s1, SL)
          containsVars ns0 or containsVars ns1 =>
            $hope:= 'T
            'failed
          if canCoerce(ns0, ns1) then s3 := s1
          else if canCoerce(ns1, ns0) then s3 := s0
          else s3 := nil
          s3 =>
            if (s3 ~= s0) then SL := augmentSub(v,s3,SL)
            if (s3 ~= s1) and isPatternVar(s) then SL := augmentSub(s,s3,SL)
            SL
          'failed
        $domPvar =>
          s3 := resolveTT(s0,s1)
          s3 =>
            if (s3 ~= s0) then SL := augmentSub(v,s3,SL)
            if (s3 ~= s1) and isPatternVar(s) then SL := augmentSub(s,s3,SL)
            SL
          'failed
--        isSubDomain(s,s0) => augmentSub(v,s0,SL)
        'failed
      'failed
    augmentSub(v,s,S)
  augmentSub(v,s,SL)

++ `v' is a query variable; we are asked to unify it with
++ structure `s' and existing substitution `SL'.
unifyQueryStruct(v,s,SL) ==
  CONTAINED(v,s) => 'failed
  s' := LASSOC(v,SL) =>
    s = s' => SL
    'failed
  [[v,:s],:SL]

ofCategory(dom,cat) ==
  -- entry point to category evaluation from other points than type
  --   analysis
  -- the result is true or nil
  $Subst:local:= nil
  $hope:local := nil
  ident? dom => nil
  cat is ['Join,:cats] => and/[ofCategory(dom,c) for c in cats]
  (hasCaty(dom,cat,nil) isnt 'failed)

printMms(mmS) ==
  -- mmS a list of modemap signatures
  sayMSG '" "
  for [sig,imp,.] in mmS for i in 1.. repeat
    istr := strconc('"[",toString i,'"]")
    if #istr = 3 then istr := strconc(istr,'" ")
    sayMSG [:bright istr,'"signature:   ",:formatSignature rest sig]
    first sig is 'local =>
      sayMSG ['"      implemented: local function ",imp]
    imp is ['XLAM,:.] =>
      sayMSG concat('"      implemented: XLAM from ",
        prefix2String first sig)
    sayMSG concat('"      implemented: slot ",imp,
      '" from ",prefix2String first sig)
  sayMSG '" "

containsVars(t) ==
  -- tests whether term t contains a * variable
  t isnt [.,:.] => isPatternVar t
  containsVars1(t)

containsVars1(t) ==
  -- recursive version, which works on a list
  [t1,:t2]:= t
  t1 isnt [.,:.] =>
    isPatternVar t1 or
      t2 isnt [.,:.] => isPatternVar t2
      containsVars1(t2)
  containsVars1(t1) or
    t2 isnt [.,:.] => isPatternVar t2
    containsVars1(t2)

isPartialMode m ==
  CONTAINED($EmptyMode,m)


getSymbolType var ==
-- var is a pattern variable
  p:= objectAssoc(var,$SymbolType) => rest p
  t:= '(Polynomial (Integer))
  $SymbolType:= [[var,:t],:$SymbolType]
  t

isEqualOrSubDomain(d1,d2) ==
  -- last 2 parts are for tagged unions (hack for now, RSS)
  (d1=d2) or isSubDomain(d1,d2) or
    (d1 isnt [.,:.] and ((d2 is ['Variable,=d1]) or (d2 is [=d1])))
     or (d2 isnt [.,:.] and ((d1 is ['Variable,=d2]) or (d1 is [=d2])))

defaultTypeForCategory(cat, SL) ==
  -- this function returns a domain belonging to cat
  -- note that it is important to note that in some contexts one
  -- might not want to use this result. For example, evalMmCat1
  -- calls this and should possibly fail in some cases.
  cat := subCopy(cat, SL)
  c := cat.op
  d := getConstructorDefaultFromDB c
  d => [d, :cat.args]
  cat is [c] =>
    c is 'Field => $RationalNumber
    c in '(Ring IntegralDomain EuclideanDomain GcdDomain
      OrderedRing DifferentialRing) => $Integer
    c is 'OrderedSet => $Symbol
    c is 'FloatingPointSystem => $Float
    nil
  cat is [c,p1] =>
    c is 'FiniteLinearAggregate => ['Vector, p1]
    c is 'VectorCategory => ['Vector, p1]
    c is 'SetAggregate => ['Set, p1]
    c is 'SegmentCategory => ['Segment, p1]
    nil
  cat is [c,p1,p2] =>
    nil
  cat is [c,p1,p2,p3] =>
    cat is ['MatrixCategory, d, ['Vector, =d], ['Vector, =d]] =>
      ['Matrix, d]
    nil
  nil


