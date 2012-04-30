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


import i_-object
namespace BOOT

$univariateDomains ==
  '(UnivariatePolynomial
    UnivariateTaylorSeries
    UnivariateLaurentSeries
    UnivariatePuiseuxSeries)

$multivariateDomains ==
  '(MultivariatePolynomial
    DistributedMultivariatePolynomial
    HomogeneousDistributedMultivariatePolynomial
    GeneralDistributedMultivariatePolynomial)

--%
$inRetract := false

--% Interpreter Analysis Functions

++ Record calling context information in the VAT `t'.
putCallInfo(t,op,arg,nargs) ==
  putAtree(t,"callingFunction",op)
  putAtree(t,"argumentNumber",arg)
  putAtree(t,"totalArgs",nargs)
  t

getMinimalVariableTower(var,t) ==
  -- gets the minimal polynomial subtower of t that contains the
  -- given variable. Returns nil if none.
  string?(t) or ident?(t) => nil
  t = $Symbol => t
  t is ['Variable,u] =>
    (u = var) => t
    nil
  t is ['Polynomial,.] => t
  t is ['RationalFunction,D] => ['Polynomial,D]
  t is [up,t',u,.] and symbolMember?(up,$univariateDomains) =>
    -- power series have one more arg and different ordering
    u = var => t
    getMinimalVariableTower(var,t')
  t is [up,u,t'] and symbolMember?(up,$univariateDomains) =>
    u = var => t
    getMinimalVariableTower(var,t')
  t is [mp,u,t'] and symbolMember?(mp,$multivariateDomains) =>
    member(var,u) => t
    getMinimalVariableTower(var,t')
  null (t' := underDomainOf t) => nil
  getMinimalVariableTower(var,t')

getMinimalVarMode(id,m) ==
  --  This function finds the minimum polynomial subtower type of the
  --  polynomial domain tower m which id to which can be coerced
  --  It includes all polys above the found level if they are
  --  contiguous.
  --  E.g.:    x and G P[y] P[x] I ---> P[y] P[x] I
  --           x and P[y] G P[x] I ---> P[x] I
  m is ['Mapping, :.] => m
  defaultMode :=
    $Symbol
  null m => defaultMode
  (vl := polyVarlist m) and (member(id,vl) or 'all in vl) =>
    substitute($Integer,$EmptyMode,m)
  (um := underDomainOf m) => getMinimalVarMode(id,um)
  defaultMode

polyVarlist m ==
  --  If m is a polynomial type this function returns a list of its
  --  top level variables, and nil otherwise
  -- ignore any QuotientFields that may separate poly types
  m is [=$QuotientField,op] => polyVarlist op
  m is [op,a,:.] =>
    op in '(UnivariateTaylorSeries UnivariateLaurentSeries
      UnivariatePuiseuxSeries) =>
        [., ., a, :.] := m
        a := removeQuote a
        [a]
    op in '(Polynomial RationalFunction Expression) => '(all)
    a := removeQuote a
    op in '(UnivariatePolynomial) => [a]
    symbolMember?(op,$multivariateDomains) => a
  nil

--% Pushing Down Target Information

pushDownTargetInfo(op,target,arglist) ==
  -- put target info on args for certain operations
  target = $OutputForm => nil
  target = $Any        => nil
  n := # arglist
  pushDownOnArithmeticVariables(op,target,arglist)
  (pdArgs := pushDownOp?(op,n)) =>
    for i in pdArgs repeat
      x := arglist.i
      if not getTarget(x) then putTarget(x,target)
  nargs := #arglist
  1 = nargs =>
    (op = 'SEGMENT) and (target is ['UniversalSegment,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
  2 = nargs =>
    op = "*" =>            -- only push down on 1st arg if not immed
      if not getTarget second arglist then putTarget(second arglist,target)
      getTarget(x := first arglist) => nil
      if getUnname(x) ~= $immediateDataSymbol then putTarget(x,target)
    op = "**" or op = "^" =>           -- push down on base
      if not getTarget first arglist then putTarget(first arglist,target)
    (op = 'equation) and (target is ['Equation,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    (op = 'gauss) and (target is ['Gaussian,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    (op = '_/) =>
      targ :=
        target is ['Fraction,S] => S
        target
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,targ)
    (op = 'SEGMENT) and (target is ['Segment,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    (op = 'SEGMENT) and (target is ['UniversalSegment,S]) =>
      for x in arglist repeat
        if not getTarget(x) then putTarget(x,S)
    nil
  nil

pushDownOnArithmeticVariables(op,target,arglist) ==
  -- tries to push appropriate target information onto variable
  -- occurring in arithmetic expressions
  cons?(target) and first(target) = 'Variable => nil
  not symbolMember?(op,'(_+ _- _* _*_* _/)) => nil
  not containsPolynomial(target)   => nil
  for x in arglist for i in 1.. repeat
    vector?(x) =>   -- leaf
      transferPropsToNode(xn := getUnname(x),x)
      getValue(x) or (xn = $immediateDataSymbol) => nil
      t := getMinimalVariableTower(xn,target) or target
      if not getTarget(x) then putTarget(x,t)
    cons?(x) =>  -- node
      [op',:arglist'] := x
      pushDownOnArithmeticVariables(getUnname op',target,arglist')
  arglist

pushDownOp?(op,n) ==
  -- determine if for op with n arguments whether for all modemaps
  -- the target type is equal to one or more arguments. If so, a list
  -- of the appropriate arguments is returned.
  ops := [sig for [sig,:.] in getModemapsFromDatabase(op,n)]
  null ops => nil
  op in '(_+ _* _- _exquo) => [i for i in 0..(n-1)]
  -- each signature has form
  -- [domain of implementation, target, arg1, arg2, ...]
  -- sameAsTarg is a vector that counts the number of modemaps that
  -- have the corresponding argument equal to the target type
  sameAsTarg := mkIntArray n
  numMms := # ops
  for [.,targ,:argl] in ops repeat
    for arg in argl for i in 0.. repeat
      targ = arg => vectorRef(sameAsTarg,i) := 1 + sameAsTarg.i
  -- now see which args have their count = numMms
  ok := nil
  for i in 0..(n-1) repeat
    if numMms = sameAsTarg.i then ok := [i,:ok]
  reverse ok

--% Bottom Up Processing

-- Also see I-SPEC BOOT for special handlers and I-MAP BOOT for
-- user function processing.

++ Take a parse form and return the VAT for its elaboration.
elaborateForm pf ==
  t := mkAtree1 pf
  bottomUp t
  t

++ Elaborate a VAT with specified target type.
elaborateTree(t,mode) ==
  putTarget(t,mode)
  bottomUp t

bottomUp t ==
  -- bottomUp takes an attributed tree, and returns the modeSet for it.
  -- As a side-effect it also evaluates the tree.
  t is [op,:argl] =>
    tar := getTarget op
    getUnname(op) ~= $immediateDataSymbol and (v := getValue op) =>
      om := objMode(v)
      null tar => [om]
      (r := resolveTM(om,tar)) => [r]
      [om]
    if op isnt [.,:.] then
      opName:= getUnname op
      if isLocallyBound opName then
        putModeSet(op,bottomUpIdentifier(op,opName))
      else
        transferPropsToNode(opName,op)
    else
      opName := nil
      bottomUp op

    opVal := getValue op

    -- call a special handler if we are not being package called
    dol := getAtree(op,'dollar) and (opName ~= 'construct)

    (null dol) and (fn:= property(opName,"up")) and (u:= FUNCALL(fn, t)) => u
    nargs := #argl
    if opName then for x in argl for i in 1.. repeat
      putCallInfo(x,opName,i,nargs)

    if tar then pushDownTargetInfo(opName,tar,argl)

    -- see if we are calling a declared user map
    -- if so, push down the declared types as targets on the args
    if opVal and (objVal opVal  is ["%Map",:.]) and
      (getMode op is ['Mapping,:ms]) and (nargs + 1= #ms) then
        for m in rest ms for x in argl repeat putTarget(x,m)

    argModeSetList:= [bottomUp x for x in argl]

    if null tar and opName = "*" and nargs = 2 then
        [[t1],[t2]] := argModeSetList
        tar := computeTypeWithVariablesTarget(t1, t2)
        tar =>
            pushDownTargetInfo(opName,tar,argl)
            argModeSetList:= [bottomUp x for x in argl]

    bottomUpWithArgModesets(t,op,opName,argl,argModeSetList)
  m := getBasicMode t => [m]
  ident? (id := getUnname t) =>
    putModeSet(t,bottomUpIdentifier(t,id))
  keyedSystemError("S2GE0016",['"bottomUp",'"unknown object form"])

bottomUpWithArgModesets(t,op,opName,args,argModeSetList) ==
  ms := bottomUpForm(t,op,opName,args,argModeSetList)
  -- If this is a type producing form, then we don't want
  -- to store the representation object in the environment.
  -- Rather, we want to record the reified canonical form.
  if ms is [m] and (member(m,$LangSupportTypes) or isCategoryForm(m,$e))
  then putValue(t,objNew(devaluate objValUnwrap getValue t, m))

  -- given no target or package calling, force integer constants to
  -- belong to tightest possible subdomain
  pcall? := getAtree(op,'dollar) and (opName ~= 'construct)
  op := t.op -- may have changed in bottomUpElt
  $useIntegerSubdomain and getTarget op = nil and not pcall? and
    isEqualOrSubDomain(first ms,$Integer) =>
      val := objVal getValue op
      isWrapped val =>       -- constant if wrapped
        val := unwrap val
        bm := getBasicMode val
        putValue(op,objNewWrap(val,bm))
        putModeSet(op,[bm])
      ms
  ms


computeTypeWithVariablesTarget(p, q) ==
    polyVarlist(p) or polyVarlist(q) =>
        t := resolveTT(p, q)
        polyVarlist(t) => t
        nil
    nil

bottomUpCompile t ==
  $genValue:local := false
  ms := bottomUp t
  massageBackendCode objVal getValue t
  ms

bottomUpUseSubdomain t ==
  $useIntegerSubdomain : local := true
  ms := bottomUp t
  ($immediateDataSymbol ~= getUnname(t)) or ($Integer ~= first(ms)) => ms
  null integer?(num := objValUnwrap getValue t) => ms
  o := getBasicObject(num)
  putValue(t,o)
  ms := [objMode o]
  putModeSet(t,ms)
  ms

bottomUpPredicate(pred, name) ==
  putTarget(pred,$Boolean)
  ms := bottomUp pred
  $Boolean ~= first ms => throwKeyedMsg('"S2IB0001",[name])
  ms

bottomUpCompilePredicate(pred, name) ==
  $genValue:local := false
  bottomUpPredicate(pred,name)


++ We are in the process of elaborating the identifier `id' into
++ the  VAT `t'.  Return the modeset of the elaboration if `id'
++ unambiguously denote a constructor.  Ambiguous constructor
++ identifiers are precisely those that denote niladic constructors.
++ By default, the ambiguity is resolved to types. 
++ See bottomUpIdentifier and isType.
isUnambiguouslyConstructor(id,t) ==
  niladicConstructor? id => nil
  k := getConstructorKindFromDB id or
        builtinFunctorName? id => "domain"
        builtinCategoryName? id => "category"
  k = nil => nil
  ms := 
    k = "category" => [$CategoryConstructor]
    [$DomainConstructor]
  if not builtinConstructor? id then 
    loadIfNecessary id
  putValue(t,objNewWrap(id,first ms))
  putModeSet(t,ms)
  ms
  


bottomUpIdentifier(t,id) ==
  ms := isUnambiguouslyConstructor(id,t) => ms
  m := isType t => bottomUpType(t, m)
  id = "%noMapVal" => throwKeyedMsg('"S2IB0002",nil)
  id = "%noBranch" =>
    keyedSystemError("S2GE0016",
      ['"bottomUpIdentifier",'"trying to evaluate %noBranch"])
  transferPropsToNode(id,t)
  defaultType := ['Variable,id]
  -- This was meant to stop building silly symbols but had some unfortunate
  -- side effects, like not being able to say e:=foo in the interpreter.  MCD
--  defaultType :=
--    getModemapsFromDatabase(id,1) =>
--      userError ['"Cannot use operation name as a variable: ", id]
--    ['Variable, id]
  u := getValue t => --non-cached values MAY be re-evaluated
    tar := getTarget t
    expr:= objVal u
    om := objMode(u)
    (om ~= $EmptyMode) and (om isnt ['RuleCalled,.]) =>
      $genValue or gensym?(id) =>
        null tar => [om]
        (r := resolveTM(om,tar)) => [r]
        [om]
      bottomUpDefault(t,id,defaultType,getTarget t)
    interpRewriteRule(t,id,expr) or
      (isMapExpr expr and [objMode(u)]) or
        keyedSystemError("S2GE0016",
          ['"bottomUpIdentifier",'"cannot evaluate identifier"])
  m := namedConstant(id,t) => [m]
  bottomUpDefault(t,id,defaultType,getTarget t)

getConstantObject(id,dc,sig) ==
  mode := substitute(dc,"$",first sig)
  $genValue =>
    objNewWrap(SPADCALL compiledLookupCheck(id,sig,evalDomain dc),mode)
  objNew(["SPADCALL",["compiledLookupCheck",id,sig,["evalDomain",dc]]],mode)

namedConstant(id,t) ==
  -- for the time being, ignore the case where the target type is imposed.
  getTarget(t) ~= nil => nil
  sysmms := getModemapsFromDatabase(id,0) or return nil
  -- ignore polymorphic constants are not supported yet.
  doms := [getDCFromSystemModemap sysmm for sysmm in sysmms]
  candidates := nil
  for dc in doms | niladicConstructor? first dc repeat
    LASSOC(id,getConstructorOperationsFromDB dc.op) is [[sig,.,.,"CONST"]] =>
      candidates := [[dc,sig],:candidates]
  null candidates => nil
  #candidates = 1 =>
    [[dc,sig]] := candidates
    val := getConstantObject(id,dc,sig)
    putValue(t,val)
    putMode(t,objMode val)
    
  -- error for ambiguity.

bottomUpDefault(t,id,defaultMode,target) ==
  if $genValue
    then bottomUpDefaultEval(t,id,defaultMode,target,nil)
    else bottomUpDefaultCompile(t,id,defaultMode,target,nil)

bottomUpDefaultEval(t,id,defaultMode,target,isSub) ==
  -- try to get value case.

  -- 1. declared mode but no value case
  (m := getMode t) =>
    m is ['Mapping,:.] => throwKeyedMsg('"S2IB0003",[getUnname t])

    -- hmm, try to treat it like target mode or declared mode
    if isPartialMode(m) then m := resolveTM(['Variable,id],m)
    -- if there is a target, probably want it to be that way and not
    -- declared mode. Like "x" in second line:
    --   x : P[x] I
    --   y : P[x] I
    target and not isSub and
      (val := coerceInteractive(objNewWrap(id,['Variable,id]),target))=>
        putValue(t,val)
        [target]
    -- Ok, see if we can make it into declared mode from symbolic form
    -- For example, (x : P[x] I; x + 1)
    not target and not isSub and m and
      (val := coerceInteractive(objNewWrap(id,['Variable,id]),m)) =>
        putValue(t,val)
        [m]

    -- Hmm, assume it is a symbolic variable with a user-supplied type.
    m' := ["Expression",m]
    val := coerceInteractive(objNewWrap(id,$Symbol),m') =>
      putValue(t,val)
      putMode(t, m')
      [m']

    -- give up
    throwKeyedMsg('"S2IB0004",[id,m])

  -- 2. no value and no mode case
  val := objNewWrap(id,defaultMode)
  (null target) or (defaultMode = target) =>
    putValue(t,val)
    [defaultMode]
  if isPartialMode target then
    -- this hackery will go away when Symbol is not the default type
    if defaultMode = $Symbol and (target is [D,x,.]) then
      (symbolMember?(D,$univariateDomains) and (x = id)) or
        (symbolMember?(D,$multivariateDomains) and member(id,x)) =>
           dmode := [D,x,$Integer]
           (val' := coerceInteractive(objNewWrap(id,
             ['Variable,id]),dmode)) =>
               defaultMode := dmode
               val := val'
      nil
    target := resolveTM(defaultMode,target)
  -- The following is experimental.  SCM 10/11/90
  if target and (tm := getMinimalVarMode(id, target)) then
    target := tm
  (null target) or null (val' := coerceInteractive(val,target)) =>
    putValue(t,val)
    [defaultMode]
  putValue(t,val')
  [target]

bottomUpDefaultCompile(t,id,defaultMode,target,isSub) ==
  tmode := getMode t
  tval  := getValue t
  expr:=
    isLocallyBound id => id
    get(id,"mode",$env) => id       -- declared local variable
    tmode or tval =>
      envMode := tmode or objMode tval
      envMode is ['Variable, :.] => objVal tval
      id = $immediateDataSymbol => objVal tval
      ['getValueFromEnvironment,MKQ id,MKQ envMode]
    wrap id
  tmode and tval and (mdv := objMode tval) =>
    if isPartialMode tmode then
      null (tmode := resolveTM(mdv,tmode)) =>
        keyedMsgCompFailure("S2IB0010",nil)
    putValue(t,objNew(expr,tmode))
    [tmode]
  tmode or (tval and (tmode := objMode tval)) =>
    putValue(t,objNew(expr,tmode))
    [tmode]
  obj := objNew(expr,defaultMode)
  canCoerceFrom(defaultMode, target) and
    (obj' := coerceInteractive(obj, target)) =>
        putValue(t, obj')
        [target]
  putValue(t,obj)
  [defaultMode]

interpRewriteRule(t,id,expr) ==
  null get(id,'isInterpreterRule,$e) => nil
  (ms:= selectLocalMms(t,id,nil,nil)) and (ms:=evalForm(t,id,nil,ms)) =>
    ms
  nil

bottomUpForm(t,op,opName,argl,argModeSetList) ==
  not($inRetract) =>
    bottomUpForm3(t,op,opName,argl,argModeSetList)
  bottomUpForm2(t,op,opName,argl,argModeSetList)

bottomUpForm3(t,op,opName,argl,argModeSetList) ==
  $origArgModeSetList:local  := copyTree argModeSetList
  bottomUpForm2(t,op,opName,argl,argModeSetList)

bottomUpForm2(t,op,opName,argl,argModeSetList) ==
  cons? t and opName="%%" => bottomUpPercent t
  opVal := getValue op

  -- for things with objects in operator position, be careful before
  -- we enter general modemap selection

  lookForIt :=
    getAtree(op,'dollar) => true
    not opVal => true
    opMode := objMode opVal
    not (opModeTop := IFCAR opMode) => true
    opModeTop in '(Record Union) => false
    opModeTop in '(Variable Mapping FunctionCalled RuleCalled AnonymousFunction) => true
    false

  -- get rid of Union($, "failed") except when op is "=" and all
  -- modesets are the same

  $genValue and
    not (opName = "=" and argModeSetList is [[m],[=m]] and m is ['Union,:.]) and
      (u := bottomUpFormUntaggedUnionRetract(t,op,opName,argl,argModeSetList)) => u

  lookForIt and (u := bottomUpFormTuple(t, op, opName, argl, argModeSetList)) => u

  -- opName can change in the call to selectMms

  (lookForIt and (mmS := selectMms(op,argl,getTarget op))) and
    (mS := evalForm(op,opName := getUnname op,argl,mmS)) =>
      putModeSet(op,mS)
  bottomUpForm0(t,op,opName,argl,argModeSetList)

bottomUpFormTuple(t, op, opName, args, argModeSetList) ==
  getAtree(op,'dollar) => nil
  null (singles := getModemapsFromDatabase(opName, 1)) => nil

  -- see if any of the modemaps have Tuple arguments
  haveTuple := false
  for mm in singles while not haveTuple repeat
    if getFirstArgTypeFromMm(mm) is ["Tuple",.] then haveTuple := true
  not haveTuple => nil
  nargs := #args
  nargs = 1 and getUnname first args = "Tuple" => nil
  nargs = 1 and (ms := bottomUp first args) and
    (ms is [["Tuple",.]] or ms is [["List",.]]) => nil

  -- now make the args into a tuple

  newArg := [mkAtreeNode "tuple",:args]
  bottomUp [op, newArg]

removeUnionsAtStart(argl,modeSets) ==
  null $genValue => modeSets
  for arg in argl for ms in modeSets repeat
    null (v := getValue arg) => nil
    m := objMode(v)
    m isnt ['Union,:.] => nil
    val := objVal(v)
    not isWrapped val => nil
    val' := retract v
    m' := objMode val'
    putValue(arg,val')
    putModeSet(arg,[m'])
    ms.first := m'
  modeSets

printableArgModeSetList() ==
  amsl := nil
  for a in reverse $origArgModeSetList repeat
    b := first a
    if b isnt [.,:.] then b := [b]
    amsl := ['"%l",b,:amsl]
  if amsl then amsl := rest amsl
  amsl

bottomUpForm0(t,op,opName,argl,argModeSetList) ==
  op0 := op
  opName0 := opName

  m := isType t =>
    bottomUpType(t, m)

  m := getModeOrFirstModeSetIfThere op
  m is ['Record,:.] and argModeSetList is [[['Variable,x]]] and
      member(x,getUnionOrRecordTags m) and (u := bottomUpElt t) => u
  m is ['Union,:.] and argModeSetList is [[['Variable,x]]] =>
      member(x,getUnionOrRecordTags m) and (u := bottomUpElt t) => u
      not $genValue =>
        amsl := printableArgModeSetList()
        throwKeyedMsgSP("S2IB0008",['"the union object",amsl], op)
      object := retract getValue op
      object = 'failed =>
        throwKeyedMsgSP("S2IB0008",['"the union object",amsl], op)
      putModeSet(op,[objMode(object)])
      putValue(op,object)
      (u := bottomUpElt t) => u
      bottomUpForm0(t,op,opName,argl,argModeSetList)

  (opName ~= "elt") and (opName ~= "apply") and
    #argl = 1 and first first argModeSetList is ['Variable, var]
      and var in '(first last rest) and
        isEltable(op, argl, #argl) and (u := bottomUpElt t) => u

  $genValue and
    ( u:= bottomUpFormRetract(t,op,opName,argl,argModeSetList) ) => u

  (opName ~= "elt") and (opName ~= "apply") and
    isEltable(op, argl, #argl) and (u := bottomUpElt t) => u

  if integer? $HTCompanionWindowID then
    mkCompanionPage('operationError, t)

  amsl := printableArgModeSetList()
  opName1 :=
    opName0 = $immediateDataSymbol =>
        (o := coerceInteractive(getValue op0,$OutputForm)) =>
            outputTran objValUnwrap o
        nil
    opName0

  if null(opName1) then
    opName1 :=
        (o := getValue op0) => objMode o
        '"<unknown type>"
    msgKey :=
        null amsl => "S2IB0013"
        "S2IB0012"
  else
    msgKey :=
        null amsl => "S2IB0011"
        (n := isSharpVarWithNum opName1) =>
            opName1 := n
            "S2IB0008g"
        "S2IB0008"

  sayIntelligentMessageAboutOpAvailability(opName1, #argl)

  not $genValue =>
    keyedMsgCompFailureSP(msgKey,[opName1, amsl], op0)
  throwKeyedMsgSP(msgKey,[opName1, amsl], op0)

sayIntelligentMessageAboutOpAvailability(opName, nArgs) ==
  -- see if we can give some decent messages about the availability if
  -- library messages

  integer? opName => nil

  oo :=  object2Identifier opOf opName
  if ( oo = "%" ) or ( oo = "Domain" ) or ( domainForm? opName ) then
    opName := "elt"

  nAllExposedMmsWithName := #getModemapsFromDatabase(opName, nil)
  nAllMmsWithName        := #getAllModemapsFromDatabase(opName, nil)

  -- first see if there are ANY ops with this name

  if nAllMmsWithName = 0 then
    sayKeyedMsg("S2IB0008a", [opName])
  else if nAllExposedMmsWithName = 0 then
    nAllMmsWithName = 1 => sayKeyedMsg("S2IB0008b", [opName])
    sayKeyedMsg("S2IB0008c", [opName, nAllMmsWithName])
  else
    -- now talk about specific arguments
    nAllExposedMmsWithNameAndArgs   := #getModemapsFromDatabase(opName, nArgs)
    nAllMmsWithNameAndArgs          := #getAllModemapsFromDatabase(opName, nArgs)
    nAllMmsWithNameAndArgs = 0 =>
        sayKeyedMsg("S2IB0008d", [opName, nArgs, nAllExposedMmsWithName, nAllMmsWithName - nAllExposedMmsWithName])
    nAllExposedMmsWithNameAndArgs = 0 =>
        sayKeyedMsg("S2IB0008e", [opName, nArgs, nAllMmsWithNameAndArgs - nAllExposedMmsWithNameAndArgs])
    sayKeyedMsg("S2IB0008f", [opName, nArgs, nAllExposedMmsWithNameAndArgs, nAllMmsWithNameAndArgs - nAllExposedMmsWithNameAndArgs])
  nil


++ Returns the `conceptual' type of `type', e.g., the type of type in
++ the abstract semantics, not necessarily the one from implementation
++ point of view.
conceptualType: %Thing -> %Mode
conceptualType type ==
  isPartialMode type => $Mode
  member(type,[$Mode,$Domain,$Category]) => $Type
  categoryForm?(type) => $Category
  $Domain

++ Returns true is `t' conceptually describes a domain or package.
isConceptualCategory: %Mode -> %Boolean
isConceptualCategory t ==
  t = $Type or t = $Category or t = $Domain or categoryForm? t

bottomUpType(t, type) ==
  mode := conceptualType type
  val:= objNew(type,mode)
  putValue(t,val)
  -- have to fix the following
  putModeSet(t,[mode])

bottomUpPercent(tree is [op,:argl]) ==
  -- handles a call %%(5), which means the output of step 5
  -- %%() is the same as %%(-1)
  null argl =>
    val:= fetchOutput(-1)
    putValue(op,val)
    putModeSet(op,[objMode(val)])
  argl is [t] =>
    i:= getArgValue(t,$Integer) =>
      val:= fetchOutput i
      putValue(op,val)
      putModeSet(op,[objMode(val)])
    throwKeyedMsgSP('"S2IB0006",nil,t)
  throwKeyedMsgSP('"S2IB0006",nil,op)

bottomUpFormRetract(t,op,opName,argl,amsl) ==
  -- tries to find one argument, which can be pulled back, and calls
  -- bottomUpForm again. We do not retract the first argument to a
  -- setelt, because this is presumably a destructive operation and
  -- the retract can create a new object.

  -- if no such operation exists in the database, don't bother
  $inRetract: local := true
  null getAllModemapsFromDatabase(getUnname op,#argl) => nil

  u := bottomUpFormAnyUnionRetract(t,op,opName,argl,amsl) => u

  a  := nil
  b  := nil
  ms := nil
  for x in argl for m in amsl for i in 1.. repeat
    -- do not retract first arg of a setelt
    (i = 1) and (opName = "setelt") =>
        a := [x,:a]
        ms := [m,:ms]
    (i = 1) and (opName = "set!") =>
        a := [x,:a]
        ms := [m,:ms]
    if cons?(m) and first(m) = $EmptyMode then return nil
    object:= retract getValue x
    a:= [x,:a]
    object="failed" =>
        putAtree(x,'retracted,nil)
        ms := [m, :ms]
    b:= true
    m.first := objMode(object)
    ms := [copyTree m, :ms]
    putAtree(x,'retracted,true)
    putValue(x,object)
    putModeSet(x,[objMode(object)])
  --insert pulled-back items
  a := reverse! a
  ms := reverse! ms

  -- check that we haven't seen these types before
  typesHad := getAtree(t, 'typesHad)
  if member(ms, typesHad) then b := nil
  else putAtree(t, 'typesHad, [ms, :typesHad])

  b and bottomUpForm(t,op,opName,a,amsl)

retractAtree atr ==
    object:= retract getValue atr
    object="failed" =>
        putAtree(atr,'retracted,nil)
        nil
    putAtree(atr,'retracted,true)
    putValue(atr,object)
    putModeSet(atr,[objMode(object)])
    true

bottomUpFormAnyUnionRetract(t,op,opName,argl,amsl) ==
  -- see if we have a Union

  ok := nil
  for m in amsl while not ok repeat
    if first(m) isnt [.,:.] then return nil
    first m = $Any => ok := true
    (first first m = 'Union) => ok := true
  not ok => nil

  a:= nil
  b:= nil

  for x in argl for m in amsl for i in 0.. repeat
    m0 := first m
    if ( (m0 = $Any) or (first m0 = 'Union) ) and
      ('failed ~= (object:=retract getValue x)) then
        b := true
        m.first := objMode(object)
        putModeSet(x,[objMode(object)])
        putValue(x,object)
    a := [x,:a]
  b and bottomUpForm(t,op,opName,reverse! a,amsl)

bottomUpFormUntaggedUnionRetract(t,op,opName,argl,amsl) ==
  -- see if we have a Union with no tags, if so retract all such guys

  ok := nil
  for [m] in amsl while not ok repeat
    if m isnt [.,:.] then return nil
    if m is ['Union, :.] and null getUnionOrRecordTags m then ok := true
  not ok => nil

  a:= nil
  b:= nil

  for x in argl for m in amsl for i in 0.. repeat
    m0 := first m
    if (m0 is ['Union, :.] and null getUnionOrRecordTags m0) and
      ('failed ~= (object:=retract getValue x)) then
        b := true
        m.first := objMode(object)
        putModeSet(x,[objMode(object)])
        putValue(x,object)
    a := [x,:a]
  b and bottomUpForm(t,op,opName,reverse! a,amsl)

bottomUpElt (form:=[op,:argl]) ==
  -- this transfers expressions that look like function calls into
  -- forms with elt or apply.

    ms := bottomUp op
    ms and (ms is [['Union,:.]] or ms is [['Record,:.]]) =>
        form.rest := [op,:argl]
        form.first := mkAtreeNode "elt"
        bottomUp form

    target  := getTarget form

    newOps := [mkAtreeNode "elt", mkAtreeNode "apply"]
    u := nil

    while null u for newOp in newOps repeat
        newArgs := [op,:argl]
        if selectMms(newOp, newArgs, target) then
            form.rest := newArgs
            form.first := newOp
            u := bottomUp form

    while null u and ( "and"/[retractAtree(a) for a in newArgs] ) repeat
        while null u for newOp in newOps repeat
            newArgs := [op,:argl]
            if selectMms(newOp, newArgs, target) then
                form.rest := newArgs
                form.first := newOp
                u := bottomUp form
    u

isEltable(op,argl,numArgs) ==
  -- determines if the object might possible have an elt function
  -- we exclude Mapping and Variable types explicitly
  v := getValue op =>
    ZEROP numArgs => true
    not(m := objMode(v)) => nil
    m is ['Mapping, :.] => nil
    objVal(v) is ["%Map",:mapDef] and numMapArgs(mapDef) > 0 => nil
    true
  m := getMode op =>
    ZEROP numArgs => true
    m is ['Mapping, :.] => nil
    true
  numArgs ~= 1 => nil
  name := getUnname op
  name = 'SEQ => nil
--not (name in '(a e h s)) and getAllModemapsFromDatabase(name, nil) => nil
  arg := first argl
  (getUnname arg) ~= 'construct => nil
  true

