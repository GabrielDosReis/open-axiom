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


import i_-analy
namespace BOOT


-- Functions which require special handlers (also see end of file)

$specialOps == '(
  ADEF AlgExtension _and _case COERCE COLLECT construct Declare DEF Dollar
   equation error free _has IF _is _isnt iterate _break %LET _local MDEF _or
    pretend QUOTE REDUCE REPEAT _return SEQ TARGET tuple typeOf _where 
     _[_|_|_] %Macro %MLambda %Import %Export %Inline %With %Add %Match)

$repeatLabel := nil
$anonymousMapCounter := 0

++ List of free variables in the current function
$freeVariables := []

++ List of bound variables in the current function
$boundVariables := []

--% Void stuff

voidValue() == '"()"

--% Handlers for Anonymous Function Definitions

upADEF t ==
  t isnt [.,[vars,types,.,body],pred,.] => nil
  -- do some checking on what we got
  for var in vars repeat
    if not ident?(var) then throwKeyedMsg("S2IS0057",[var])
  -- unabbreviate types
  types := [(if t then evaluateType unabbrev t else nil) for t in types]
  -- we do not allow partial types
  if isPartialMode(m := first types) then throwKeyedMsg("S2IS0058",[m])

  -- we want everything to be declared or nothing. The exception is that
  -- we do not require a target type since we will compute one anyway.
  if null(m) and rest types then
    m := second types
    types' := rest rest types
  else
    types' := rest types
  for type in types' repeat
    if (type and null m) or (m and null type) then
      throwKeyedMsg("S2IS0059",nil)
    if isPartialMode type  then throwKeyedMsg("S2IS0058",[type])

--  $localVars: local := nil
--  $freeVars:  local := nil
--  $env:       local := [[nil]]
  $compilingMap : local := true

  -- if there is a predicate, merge it in with the body
  if pred ~= true then body := ['IF,pred,body,'%noMapVal]

  tar := getTarget t
  null m and tar is ['Mapping,.,:argTypes] and (#vars = #argTypes) =>
    if isPartialMode tar then throwKeyedMsg("S2IS0058",[tar])
    evalTargetedADEF(t,vars,rest tar,body)
  null m => evalUntargetedADEF(t,vars,types,body)
  evalTargetedADEF(t,vars,types,body)

evalUntargetedADEF(t,vars,types,body) ==
  -- recreate a parse form
  if vars is [var]
    then vars := var
    else vars := ["tuple",:vars]
  val := objNewWrap(["+->",vars,body],$AnonymousFunction)
  putValue(t,val)
  putModeSet(t,[objMode val])

evalTargetedADEF(t,vars,types,body) ==
  $mapName : local := makeInternalMapName('"anonymousFunction",
    #vars,$anonymousMapCounter,'"internal")
  $anonymousMapCounter := 1 + $anonymousMapCounter
  $compilingMap   : local := true  -- state that we are trying to compile
  $mapThrowCount  : local := 0     -- number of "return"s encountered
  $mapReturnTypes : local := nil   -- list of types from returns
  $repeatLabel    : local := nil   -- for loops; see upREPEAT
  $breakCount     : local := 0     -- breaks from loops; ditto

  -- now substitute formal names for the parm variables
  -- this is used in the interpret-code case, but isn't so bad any way
  -- since it makes the bodies look more like regular map bodies

  sublist := [[var,:gensym()] for var in vars]
  body := sublisNQ(sublist,body)
  vars := [rest v for v in sublist]

  for m in rest types for var in vars repeat
    $env:= put(var,'mode,m,$env)
    mkLocalVar($mapName,var)
  for lvar in getLocalVars($mapName,body) repeat
    mkLocalVar($mapName,lvar)
  -- set up catch point for interpret-code mode
  x := CATCH('mapCompiler,compileTargetedADEF(t,vars,types,body))
  x = 'tryInterpOnly => mkInterpTargetedADEF(t,vars,types,body)
  x

mkInterpTargetedADEF(t,vars,types,oldBody) ==
  null first types =>
    throwKeyedMsg("S2IS0056",nil)
    throwMessage '"   map result type needed but not present."
  arglCode := ["LIST",:[argCode for type in rest types for var in vars]]
    where argCode() == ['putValueValue,['mkAtreeNode,MKQ var],
      objNewCode(["wrap",var],type)]
  put($mapName,'mapBody,oldBody,$e)
  body := ['rewriteMap1,MKQ $mapName,arglCode,MKQ types]
  compileADEFBody(t,vars,types,body,first types)

compileTargetedADEF(t,vars,types,body) ==
  val := compileBody(body,first types)
  computedResultType := objMode val
  body := wrapMapBodyWithCatch flattenCOND objVal val
  compileADEFBody(t,vars,types,body,computedResultType)

compileADEFBody(t,vars,types,body,computedResultType) ==
--+
  $compiledOpNameList := [$mapName]
  minivectorName := makeInternalMapMinivectorName symbolName $mapName
  body := substitute(["%dynval",MKQ minivectorName],"$$$",body)
  symbolValue(minivectorName) := vector $minivector

  -- The use of the three variables $definingMap, $genValue and $compilingMap
  -- is to cover the following cases:
  --
  -- $definingMap: This is set in analyzeMap and covers examples like:
  --  addx x == ((y: Integer): Integer +-> x + y)
  --  g := addx 10
  --  g 3
  -- i.e. we are storing the mapping as an object.
  --
  -- $compilingMap: This covers mappings which are created and applied "on the
  -- "fly", for example:
  --  [map(h +-> D(h, t), v) for v in [t]]
  --
  -- $genValue: This seems to be needed when we create a map as an argument 
  -- for a constructor, e.g.:
  --  Dx: LODO(EXPR INT, f +-> D(f, x)) := D()
  --
  -- MCD 13/3/96
  parms := [:vars,"envArg"]
  if not $definingMap and ($genValue or $compilingMap) then
    code := wrap compileInteractive [$mapName,["LAMBDA",parms,body]]
  else
    $freeVariables: local := []
    $boundVariables: local := [minivectorName,:vars]
    -- CCL does not support upwards funargs, so we check for any free variables
    -- and pass them into the lambda as part of envArg.
    body := checkForFreeVariables(body,"ALL")
    fun := ["function",["LAMBDA",parms,body]]
    code := ["CONS", fun, ["VECTOR", :reverse $freeVariables]]

  val := objNew(code,rt := ['Mapping,computedResultType,:rest types])
  putValue(t,val)
  putModeSet(t,[rt])

--% Handler for Algebraic Extensions

upAlgExtension t ==
  -- handler for algebraic extension declaration.  These are of
  --  the form "a | a**2+1", and have the effect that "a" is declared
  --  to be a simple algebraic extension, with respect to the given
  --  polynomial, and given the value "a" in this type.
  t isnt [op,var,eq] => nil
  null $genValue => throwKeyedMsg("S2IS0001",nil)
  a := getUnname var
  clearCmdParts ['propert,a]  --clear properties of a
  algExtension:= eq2AlgExtension eq
  upmode := ['UnivariatePolynomial,a,$EmptyMode]
  $declaredMode : local := upmode
  putTarget(algExtension,upmode)
  ms:= bottomUp algExtension
  triple:= getValue algExtension
  upmode:= resolveTMOrCroak(objMode(triple),upmode)
  null (T:= coerceInteractive(triple,upmode)) =>
    throwKeyedMsgCannotCoerceWithValue(objVal(triple),
      objMode(triple),upmode)
  newmode := objMode T
  (field := resolveTCat(third newmode,$Field)) or
    throwKeyedMsg("S2IS0002",[eq])
  pd:= ['UnivariatePolynomial,a,field]
  null (canonicalAE:= coerceInteractive(T,pd)) =>
    throwKeyedMsgCannotCoerceWithValue(objVal T,objMode T,pd)
  sae:= ['SimpleAlgebraicExtension,field,pd,objValUnwrap canonicalAE]
  saeTypeSynonym := makeSymbol strconc('"SAE",STRINGIMAGE a)
  saeTypeSynonymValue := objNew(sae,$Domain)
  fun := getFunctionFromDomain('generator,sae,nil)
  expr:= wrap SPADCALL(fun)
  putHist(saeTypeSynonym,'value,saeTypeSynonymValue,$e)
  putHist(a,'mode,sae,$e)
  putHist(a,'value,T2:= objNew(expr,sae),$e)
  clearDependencies(a,true)
  if $printTypeIfTrue then
    sayKeyedMsg("S2IS0003",nil)
    sayMSG concat ['"%l",'"   ",saeTypeSynonym,'" := ",
      :prefix2String objVal saeTypeSynonymValue]
    sayMSG concat ['"   ",a,'" : ",saeTypeSynonym,'" := ",a]
  putValue(op,T2)
  putModeSet(op,[sae])

eq2AlgExtension eq ==
  -- transforms "a=b" to a-b for processing
  eq is [op,:l] and vector? op and (getUnname op='equation) =>
    [mkAtreeNode "-",:l]
  eq

--% Handlers for booleans

++ `t' is a VAT that represents a propositional formula syntax.
++ Attempt to elaborate the whole tree into a `PropositionalFormula mode'
++ object.
bottomUpProposition(t,mode) ==
  -- FIXME: we should not hard code here the expected types of
  -- FIXME: the domain.
  not ofCategory(mode,$SetCategory) => nil
  mode := ['PropositionalFormula,mode]
  argModeSets := [elaborateTree(arg,mode) for arg in t.args]
  bottomUpWithArgModesets(t,t.op,getUnname t.op,t.args,argModeSets)

upand x ==
  -- generates code for  and  forms. The second argument is only
  -- evaluated if the first argument is true.
  x isnt [op,term1,term2] => nil
  putTarget(term1,$Boolean)
  putCallInfo(term1,"and",1,2)
  putTarget(term2,$Boolean)
  putCallInfo(term2,"and",2,2)
  ms := bottomUp term1
  ms isnt [=$Boolean] => bottomUpProposition(x,first ms)
  $genValue =>
    -- ??? we should find a way to check whether the
    -- ??? the type of the second operand matters or not.
    not objValUnwrap(getValue term1) =>     -- first operand is `false'
        putValue(x,getValue term1)
        putModeSet(x,ms)
    -- first term is true, so look at the second one
    ms := bottomUp term2
    ms isnt [=$Boolean] => nil
    putValue(x,getValue term2)
    putModeSet(x,ms)

  ms := bottomUp term2
  ms isnt [=$Boolean] => bottomUpProposition(x,first ms)
  -- generate an IF expression and let the rest of the code handle it
  -- ??? In full generality, this is still incorrect.  We should be
  -- ??? looking up modemaps to see whether the interpretation is
  -- ??? unique and the target type is Boolean before going on
  -- ??? generating LISP IF-expression. -- gdr 2008/01/14
  cond := [mkAtreeNode "=",mkAtree "false",term1]
  putTarget(cond,$Boolean)
  code := [mkAtreeNode "IF",cond,mkAtree "false",term2]
  putTarget(code,$Boolean)
  bottomUp code
  putValue(x,getValue code)
  putModeSet(x,ms)

upor x ==
  -- generates code for  or  forms. The second argument is only
  -- evaluated if the first argument is false.
  x isnt [op,term1,term2] => nil
  putTarget(term1,$Boolean)
  putCallInfo(term1,"or",1,2)
  putTarget(term2,$Boolean)
  putCallInfo(term2,"or",2,2)
  ms := bottomUp term1
  ms isnt [=$Boolean] => bottomUpProposition(x,first ms)
  $genValue =>
    objValUnwrap(getValue term1) =>  -- first operand is true, we are done.
        putValue(x,getValue term1)
        putModeSet(x,ms)
    -- first term is false, so look at the second one
    ms := bottomUp term2
    ms isnt [=$Boolean] => nil
    putValue(x,getValue term2)
    putModeSet(x,ms)

  ms := bottomUp term2
  ms isnt [=$Boolean] => bottomUpProposition(x,first ms)
  -- generate an IF expression and let the rest of the code handle it
  cond := [mkAtreeNode "=",mkAtree "true",term1]
  putTarget(cond,$Boolean)
  -- ??? the following code generation is incorrect.  -- gdr
  code := [mkAtreeNode "IF",cond,mkAtree "true",term2]
  putTarget(code,$Boolean)
  bottomUp code
  putValue(x,getValue code)
  putModeSet(x,ms)

--% Handlers for case

++ subroutine of upcase. Handles the situation where `case' may
++ have been defined as a library function.  
++ `op', `lhs' are VATs; `rhs' is unevaluated.
userDefinedCase(t is [op, lhs, rhs]) ==
  -- We want to resolve the situation by general modemap selection.
  -- So, we want to let bottomUp (which called us through upcase)
  -- to continue the work.  The way we do that is to return `nil'.
  -- Therefore we need a VAT for `rhs' with sufficient information
  -- to prevent bottomUp from trying to evaluate `rhs'.
  putAtree(op, 'flagArgsPos, flagArguments("case",2))
  r := mkAtreeNode $immediateDataSymbol
  m := quasiquote rhs
  putMode(r, m)
  putValue(r, objNewWrap(MKQ rhs,m))
  putModeSet(r, [m])
  t.rest.rest := [r]                   -- fix up contained for rhs.
  nil                                  -- tell bottomUp to continue.

upcase t ==
  t isnt [op,lhs,rhs] => nil
  putCallInfo(lhs,"case",1,2)
  bottomUp lhs
  triple := getValue lhs
  objMode(triple) isnt ['Union,:unionDoms] => userDefinedCase t
  if (rhs' := isDomainValuedVariable(rhs)) then rhs := rhs'
  if first unionDoms is [":",.,.] then
     for i in 0.. for d in unionDoms repeat
        if d is [":",=rhs,.] then rhstag := i
     if null rhstag then error '"upcase: bad Union form"
     $genValue =>
        rhstag = first unwrap objVal triple => code := wrap true
        code := wrap false
     code :=
        ['%when,
          [["EQL",rhstag,["CAR",["unwrap",objVal triple]]],
            true],
              ['%otherwise,false]]
  else
    $genValue =>
        t' := coerceUnion2Branch triple
        rhs = objMode t' => code := wrap true
        code := wrap false
    triple' := objNewCode(["wrap",objVal triple],objMode triple)
    code :=
        ['%when,
          [["EQUAL",MKQ rhs,["objMode",['coerceUnion2Branch,triple']]],
            true],
              ['%otherwise,false]]
  putValue(op,objNew(code,$Boolean))
  putModeSet(op,[$Boolean])

--% Handlers for TARGET

upTARGET t ==
  -- Evaluates the rhs to a mode,which is used as the target type for
  -- the lhs.
  t isnt [op,lhs,rhs] => nil
  -- do not (yet) support local variables on the rhs
  (not $genValue) and or/[CONTAINED(var,rhs) for var in $localVars] =>
    keyedMsgCompFailure("S2IC0010",[rhs])
  $declaredMode: local := nil
  m:= evaluateType unabbrev rhs
  not isLegitimateMode(m,nil,nil) => throwKeyedMsg("S2IE0004",[m])
  categoryForm?(m) => throwKeyedMsg("S2IE0014",[m])
  $declaredMode:= m
  cons? lhs and putTarget(lhs,m)
  ms := bottomUp lhs
  first ms ~= m =>
    throwKeyedMsg("S2IC0011",[first ms,m])
  putValue(op,getValue lhs)
  putModeSet(op,ms)

--% Handlers for COERCE

upCOERCE t ==
  -- evaluate the lhs and then tries to coerce the result to the
  -- mode which is the rhs.
  -- previous to 5/16/89, this had the same semantics as
  --    (lhs@rhs) :: rhs
  -- this must be made explicit now.
  t isnt [op,lhs,rhs] => nil
  $useConvertForCoercions : local := true
  -- do not (yet) support local variables on the rhs
  (not $genValue) and or/[CONTAINED(var,rhs) for var in $localVars] =>
    keyedMsgCompFailure("S2IC0006",[rhs])
  $declaredMode: local := nil
  m := evaluateType unabbrev rhs
  not isLegitimateMode(m,nil,nil) => throwKeyedMsg("S2IE0004",[m])
  categoryForm?(m) => throwKeyedMsg("S2IE0014",[m])
  $declaredMode:= m
  -- 05/16/89 (RSS) following line commented out to give correct
  -- semantic difference between :: and @
  bottomUp lhs
  type:=evalCOERCE(op,lhs,m)
  putModeSet(op,[type])

evalCOERCE(op,tree,m) ==
  -- the value of tree is coerced to mode m
  -- this is not necessary, if the target property of tree was used
  v  := getValue tree
  t1 := objMode(v)
  if $genValue and t1 is ['Union,:.] then
    v := coerceUnion2Branch v
    t1 := objMode(v)
  e  := objVal(v)
  value:=
    t1=m => v
    t2 :=
      if isPartialMode m
        then
          $genValue and (t1 = $Symbol) and containsPolynomial m =>
            resolveTM(['UnivariatePolynomial,objValUnwrap(v),$Integer],m)
          resolveTM(t1,m)
        else m
    null t2 => throwKeyedMsgCannotCoerceWithValue(e,t1,m)
    $genValue => coerceOrRetract(v,t2)
    objNew(getArgValue(tree,t2),t2)
  val:= value or throwKeyedMsgCannotCoerceWithValue(e,t1,m)
  putValue(op,val)
  objMode(val)

--% Handlers for COLLECT

upCOLLECT t ==
  -- $compilingLoop variable insures that throw to interp-only mode
  --   goes to the outermost loop.
  $compilingLoop => upCOLLECT1 t
  upCOLLECT0 t

upCOLLECT0 t ==
  -- sets up catch point for interpret-code mode
  $compilingLoop: local := true
  ms:=CATCH('loopCompiler,upCOLLECT1 t)
  ms = 'tryInterpOnly => interpOnlyCOLLECT t
  ms

upCOLLECT1 t ==
  t isnt [op,:itrl,body] => nil
  -- upCOLLECT with compiled body
  if (target := getTarget t) and not getTarget(body) then
    if target is [agg,S] and agg in '(List Vector Stream InfiniteTuple) then
      putTarget(body,S)
  $interpOnly => interpCOLLECT(op,itrl,body)
  isStreamCollect itrl => collectStream(t,op,itrl,body)
  $iteratorVars: local := nil
  upLoopIters itrl
  ms:= bottomUpCompile body
  [m]:= ms
  for itr in itrl repeat
    itr is ["UNTIL", pred] => bottomUpCompilePredicate(pred,'"until")
  mode:= ['Tuple,m]
  evalCOLLECT(op,rest t,mode)
  putModeSet(op,[mode])

upLoopIters itrl ==
  -- type analyze iterator loop iterators
  for iter in itrl repeat
    iter is ["WHILE",pred] =>
      bottomUpCompilePredicate(pred,'"while")
    iter is ["SUCHTHAT",pred] =>
      bottomUpCompilePredicate(pred,'"|")
    iter is ["UNTIL",:.] =>
      nil      -- handle after body is analyzed
    iter is ["IN",index,s] =>
      upLoopIterIN(iter,index,s)
    iter is ["STEP",index,lower,step,:upperList] =>
      upLoopIterSTEP(index,lower,step,upperList)
      -- following is an optimization
      typeIsASmallInteger(get(index,'mode,$env)) =>
        iter.first := 'ISTEP
    -- at this point, the AST may already be badly corrupted,
    -- but better late than never.
    throwKeyedMsg("S2IS0061",nil)

upLoopIterIN(iter,index,s) ==
  iterMs := bottomUp s

  not ident? index =>  throwKeyedMsg("S2IS0005",[index])

  if $genValue and first iterMs is ['Union,:.] then
    v := coerceUnion2Branch getValue s
    m := objMode v
    putValue(s,v)
    putMode(s,m)
    iterMs := [m]
    putModeSet(s,iterMs)

  -- transform segment variable into STEP
  iterMs is [['Segment,.]] or iterMs is [['UniversalSegment,.]] =>
    lower := [mkAtreeNode 'lo,s]
    step := [mkAtreeNode 'incr, s]
    upperList :=
      CAAR(iterMs) = 'Segment => [[mkAtreeNode 'hi,s]]
      nil
    upLoopIterSTEP(index,lower,step,upperList)
    newIter := ['STEP,index,lower,step,:upperList]
    iter.first := first newIter
    iter.rest := rest newIter

  iterMs isnt [['List,ud]] => throwKeyedMsg("S2IS0006",[index])
  put(index,'mode,ud,$env)
  mkIteratorVariable index

upLoopIterSTEP(index,lower,step,upperList) ==
  not ident? index => throwKeyedMsg("S2IS0005",[index])
  ltype := IFCAR bottomUpUseSubdomain(lower)
  not (typeIsASmallInteger(ltype) or isEqualOrSubDomain(ltype,$Integer))=>
    throwKeyedMsg("S2IS0007",['"lower"])
  stype := IFCAR bottomUpUseSubdomain(step)
  not (typeIsASmallInteger(stype) or isEqualOrSubDomain(stype,$Integer))=>
    throwKeyedMsg("S2IS0008",nil)
  types := [ltype]
  utype := nil
  for upper in upperList repeat
    utype := IFCAR bottomUpUseSubdomain(upper)
    not (typeIsASmallInteger(utype) or isEqualOrSubDomain(utype,$Integer))=>
      throwKeyedMsg("S2IS0007",['"upper"])
  if utype then types := [utype, :types]
  else types := [stype, :types]
  type := resolveTypeListAny removeDuplicates types
  put(index,'mode,type,$env)
  mkIteratorVariable index

evalCOLLECT(op,[:itrl,body],m) ==
  iters := [evalLoopIter itr for itr in itrl]
  bod := getArgValue(body,computedMode body)
  if bod isnt ['SPADCALL,:.] then bod := ['unwrap,bod]
  code := timedOptimization asTupleNewCode0(second m, ['%collect,:iters,bod])
  putValue(op,object(code,m))

falseFun(x) == nil

evalLoopIter itr ==
  -- generate code for loop iterator
  itr is ['STEP,index,lower,step,:upperList] =>
    ['STEP,getUnname index,getArgValue(lower,$Integer),
      getArgValue(step,$Integer),
        :[getArgValue(upper,$Integer) for upper in upperList]]
  itr is ['ISTEP,index,lower,step,:upperList] =>
    ['ISTEP,getUnname index,getArgValue(lower,$SmallInteger),
      getArgValue(step,$SmallInteger),
        :[getArgValue(upper,$SmallInteger) for upper in upperList]]
  itr is ['IN,index,s] =>
    ['IN,getUnname index,getArgValue(s,['List,get(index,'mode,$env)])]
  (itr is [x,pred]) and (x in '(WHILE UNTIL SUCHTHAT)) =>
    [x,getArgValue(pred,$Boolean)]

interpCOLLECT(op,itrl,body) ==
  -- interpret-code mode COLLECT handler
  $collectTypeList: local := nil
  $indexVars: local := nil
  $indexTypes: local := nil
  emptyAtree op
  emptyAtree itrl
  emptyAtree body
  code := ['%collect,:[interpIter itr for itr in itrl],
    interpCOLLECTbody(body,$indexVars,$indexTypes)]
  value := timedEVALFUN code
  t :=
    null value => $None
    last $collectTypeList
  rm := ['Tuple,t]
  value := [objValUnwrap coerceInteractive(objNewWrap(v,m),t)
    for v in value for m in $collectTypeList]
  putValue(op,objNewWrap(asTupleNew(getVMType t, #value, value),rm))
  putModeSet(op,[rm])

interpIter itr ==
  -- interpret loop iterator
  itr is ['STEP,index,lower,step,:upperList] =>
    $indexVars:= [getUnname index,:$indexVars]
    [m]:= bottomUp lower
    $indexTypes:= [m,:$indexTypes]
    for up in upperList repeat bottomUp up
    ['STEP,getUnname index,getArgValue(lower,$Integer),
      getArgValue(step,$Integer),
        :[getArgValue(upper,$Integer) for upper in upperList]]
  itr is ['ISTEP,index,lower,step,:upperList] =>
    $indexVars:= [getUnname index,:$indexVars]
    [m]:= bottomUp lower
    $indexTypes:= [m,:$indexTypes]
    for up in upperList repeat bottomUp up
    ['ISTEP,getUnname index,getArgValue(lower,$SmallInteger),
      getArgValue(step,$SmallInteger),
        :[getArgValue(upper,$SmallInteger) for upper in upperList]]
  itr is ['IN,index,s] =>
    $indexVars:=[getUnname index,:$indexVars]
    [m]:= bottomUp s
    m isnt ['List,um] => throwKeyedMsg("S2IS0009",[m])
    $indexTypes:=[um,:$indexTypes]
    ['IN,getUnname index,getArgValue(s,m)]
  (itr is [x,pred]) and (x in '(WHILE UNTIL SUCHTHAT)) =>
    [x,interpLoop(pred,$indexVars,$indexTypes,$Boolean)]

interpOnlyCOLLECT t ==
  -- called when compilation failed in COLLECT body, not in compiling map
  $genValue: local := true
  $interpOnly: local := true
  upCOLLECT t

interpCOLLECTbody(expr,indexList,indexTypes) ==
  -- generate code for interpret-code collect
  ['interpCOLLECTbodyIter,MKQ expr,MKQ indexList,['LIST,:indexList],
    MKQ indexTypes]

interpCOLLECTbodyIter(exp,indexList,indexVals,indexTypes) ==
  -- execute interpret-code collect body.  keeps list of type of
  --  elements in list in $collectTypeList.
  emptyAtree exp
  for i in indexList for val in indexVals for type in indexTypes repeat
    put(i,'value,objNewWrap(val,type),$env)
  [m]:=bottomUp exp
  $collectTypeList:=
    null $collectTypeList => [rm:=m]
    [:$collectTypeList,rm:=resolveTT(m,last $collectTypeList)]
  null rm => throwKeyedMsg("S2IS0010",nil)
  value:=
    rm ~= m => coerceInteractive(getValue exp,rm)
    getValue exp
  objValUnwrap(value)

--% Stream Collect functions

isStreamCollect itrl ==
  -- calls bottomUp on iterators and if any of them are streams
  -- then whole shebang is a stream
  isStream := false
  for itr in itrl until isStream repeat
    itr is ['IN,.,s] =>
      iterMs := bottomUp s
      iterMs is [['Stream,:.]] => isStream := true
      iterMs is [['InfiniteTuple,:.]] => isStream := true
      iterMs is [['UniversalSegment,:.]] => isStream := true
    itr is ['STEP,.,.,.] => isStream := true
  isStream

collectStream(t,op,itrl,body) ==
  v := CATCH('loopCompiler,collectStream1(t,op,itrl,body))
  v = 'tryInterpOnly => throwKeyedMsg("S2IS0011",nil)
  v

collectStream1(t,op,itrl,body) ==
  $indexVars:local := nil
  upStreamIters itrl
  if #$indexVars = 1 then mode:=collectOneStream(t,op,itrl,body)
  else mode:=collectSeveralStreams(t,op,itrl,body)
  putModeSet(op,[mode])

upStreamIters itrl ==
  -- type analyze stream collect loop iterators
  for iter in itrl repeat
    iter is ['IN,index,s] =>
      upStreamIterIN(iter,index,s)
    iter is ['STEP,index,lower,step,:upperList] =>
      upStreamIterSTEP(index,lower,step,upperList)

upStreamIterIN(iter,index,s) ==
  iterMs := bottomUp s

  -- transform segment variable into STEP
  iterMs is [['Segment,.]] or iterMs is [['UniversalSegment,.]] =>
    lower := [mkAtreeNode 'lo, s]
    step := [mkAtreeNode 'incr, s]
    upperList :=
      CAAR(iterMs) = 'Segment => [[mkAtreeNode 'hi,s]]
      nil
    upStreamIterSTEP(index,lower,step,upperList)
    newIter := ['STEP,index,lower,step,:upperList]
    iter.first := first newIter
    iter.rest := rest newIter

  (iterMs isnt [['List,ud]]) and (iterMs isnt [['Stream,ud]])
    and (iterMs isnt [['InfinitTuple, ud]]) =>
      throwKeyedMsg("S2IS0006",[index])
  put(index,'mode,ud,$env)
  mkIteratorVariable index
  s :=
    iterMs is [['List,ud],:.] =>
      form:=[mkAtreeNode 'pretend, [mkAtreeNode 'COERCE,s,['Stream,ud]],
             ['InfiniteTuple, ud]]
      bottomUp form
      form
    s
  $indexVars:= [[index,:s],:$indexVars]

upStreamIterSTEP(index,lower,step,upperList) ==
  null isEqualOrSubDomain(ltype := IFCAR bottomUpUseSubdomain(lower),
    $Integer) => throwKeyedMsg("S2IS0007",['"lower"])
  null isEqualOrSubDomain(stype := IFCAR bottomUpUseSubdomain(step),
    $Integer) => throwKeyedMsg("S2IS0008",nil)
  for upper in upperList repeat
    null isEqualOrSubDomain(IFCAR bottomUpUseSubdomain(upper),
      $Integer) => throwKeyedMsg("S2IS0007",['"upper"])

  put(index,'mode,type := resolveTT(ltype,stype),$env)
  null type => throwKeyedMsg("S2IS0010", nil)
  mkIteratorVariable index

  s :=
    null upperList =>
      -- create the function that does the appropriate incrementing
      genFun := 'generate
      form := [mkAtreeNode genFun,
        [[mkAtreeNode 'Dollar, ['IncrementingMaps,type],
          mkAtreeNode 'incrementBy],step],lower]
      bottomUp form
      form
    form := [mkAtreeNode 'SEGMENT,lower,first upperList]
    putTarget(form,['Segment,type])
    form := [mkAtreeNode 'construct,form]
    putTarget(form,['List,['Segment,type]])
    form := [mkAtreeNode 'expand,form]
    putTarget(form,'(List (Integer)))
    form:=[mkAtreeNode 'pretend, [mkAtreeNode 'COERCE,form,['Stream,$Integer]],
           ['InfiniteTuple, $Integer]]
    bottomUp form
    form
  $indexVars:= [[index,:s],:$indexVars]

collectOneStream(t,op,itrl,body) ==
  -- build stream collect for case of iterating over a single stream
  --  In this case we don't need to build records
  form := mkAndApplyPredicates itrl
  bodyVec := mkIterFun(first $indexVars,body)
  form := [mkAtreeNode 'map,bodyVec,form]
  bottomUp form
  val := getValue form
  m := objMode val
  m isnt ['Stream, ud] and m isnt ['InfiniteTuple, ud] =>
    systemError '"Not a Stream"
  newVal := objNew(objVal val, ['InfiniteTuple, ud])
  putValue(op,newVal)
  objMode newVal

mkAndApplyPredicates itrl ==
  -- for one index variable case for now.  may generalize later
  [indSet] := $indexVars
  [.,:s] := indSet
  for iter in itrl repeat
    iter is ['WHILE,pred] =>
      fun := 'filterWhile
      predVec := mkIterFun(indSet,pred)
      s := [mkAtreeNode fun,predVec,s]
    iter is ['UNTIL,pred] =>
      fun := 'filterUntil
      predVec := mkIterFun(indSet,pred)
      s := [mkAtreeNode fun,predVec,s]
    iter is ['SUCHTHAT,pred] =>
      fun := 'select
      putTarget(pred,$Boolean)
      predVec := mkIterFun(indSet,pred)
      s := [mkAtreeNode fun,predVec,s]
  s

mkIterFun([index,:s],funBody) ==
  -- transform funBody into a lambda with index as the parameter
  mode := objMode getValue s
  mode isnt ['Stream, indMode] and mode isnt ['InfiniteTuple, indMode] =>
    keyedSystemError('"S2GE0016", '("mkIterFun" "bad stream index type"))
  put(index,'mode,indMode,$env)
  mkLocalVar($mapName,index)
  [m]:=bottomUpCompile funBody
  mapMode := ['Mapping,m,indMode]
  -- Check generated code for free variables and pass them into the
  -- lambda as part of envArg.  Since only `index' is bound, every
  -- other symbol in non-operator position is a free variable.
  $freeVariables: local := []
  $boundVariables: local := [index]
  body := checkForFreeVariables(objVal getValue funBody,"ALL")
  parms := [index,"envArg"]
  val:=['function,declareUnusedParameters ['LAMBDA,parms,body]]
  vec := mkAtreeNode gensym()
  putValue(vec,objNew(['CONS,val,["VECTOR",:reverse $freeVariables]],mapMode))
  vec

checkForFreeVariables(v,locals) ==
  -- v is the body of a lambda expression.  The list $boundVariables is all the
  -- bound variables, the parameter locals contains local variables which might
  -- be free, or the token ALL, which means that any parameter is a candidate
  -- to be free.
  null v => v
  symbol? v =>
    v="$$$" => v -- Placeholder for mini-vector
    symbolMember?(v,$boundVariables) => v
    p := POSITION(v,$freeVariables) =>
      ["getSimpleArrayEntry","envArg",positionInVec(p,#($freeVariables))]
    (locals = "ALL") or symbolMember?(v,locals) =>
      $freeVariables := [v,:$freeVariables]
      ["getSimpleArrayEntry","envArg",positionInVec(0,#($freeVariables))]
    v
  LISTP v =>
    rest(lastNode v) => -- Must be a better way to check for a genuine list?
      v
    [op,:args] := v
    LISTP op => 
      -- Might have a mode at the front of a list, or be calling a function
      -- which returns a function.
      [checkForFreeVariables(op,locals),:[checkForFreeVariables(a,locals) for a in args]]
    op in '(LAMBDA QUOTE getValueFromEnvironment) => v
    op = "LETT" => -- Expands to a SETQ.
      ["SETF",:[checkForFreeVariables(a,locals) for a in args]]
    op in '(COLLECT REPEAT %collect %repeat) =>
      first(args) is ["STEP",var,:.] =>
       $boundVariables := [var,:$boundVariables]
       r := [op,:[checkForFreeVariables(a,locals) for a in args]]
       $boundVariables := removeSymbol($boundVariables,var)
       r
      [op,:[checkForFreeVariables(a,locals) for a in args]]
    op = "%LET" =>
      args is [var,form,name] =>
        -- This is some bizarre %LET, not what one would expect in Common Lisp!
        -- Treat var as a free variable, since it may be bound out of scope
        -- if we are in a lambda within another lambda.
        newvar := 
          p := POSITION(var,$freeVariables) =>
            ["getSimpleArrayEntry","envArg",positionInVec(p,#($freeVariables))]
          $freeVariables := [var,:$freeVariables]
          ["getSimpleArrayEntry","envArg",positionInVec(0,#($freeVariables))]
        ["SETF",newvar,checkForFreeVariables(form,locals)]
      error "Non-simple variable bindings are not currently supported"
    op in '(LET LET_* %bind) =>
      vars := [first init for init in first args]
      inits := [checkInit(init,locals) for init in first args] where
                  checkInit([var,init],locals) ==
                     init := checkForFreeVariables(init,locals)
                     $boundVariables := [var,:$boundVariables]
                     [var,init]
      body := checkForFreeVariables(rest args,locals)
      $boundVariables := setDifference($boundVariables,vars)
      [op,inits,:body]
    op = "PROG" =>
      error "Non-simple variable bindings are not currently supported"
    [op,:[checkForFreeVariables(a,locals) for a in args]]
  v

positionInVec(p,l) ==
  -- We cons up the free list, but need to keep positions consistent so
  -- count from the end of the list.
  l-p-1

collectSeveralStreams(t,op,itrl,body) ==
  -- performs collects over several streams in parallel
  $index: local := nil
  [form,:zipType] := mkZipCode $indexVars
  form := mkAndApplyZippedPredicates(form,zipType,itrl)
  vec := mkIterZippedFun($indexVars,body,zipType,$localVars)
  form := [mkAtreeNode 'map, vec, form]
  bottomUp form
  val := getValue form
  m := objMode val
  m isnt ['Stream, ud] and m isnt ['InfiniteTuple, ud] =>
    systemError '"Not a Stream"
  newVal := objNew(objVal val, ['InfiniteTuple, ud])
  putValue(op,newVal)
  objMode newVal

mkZipCode indexList ==
  -- create interpreter form for turning a list of parallel streams
  -- into a stream of nested record types.  returns [form,:recordType]
  #indexList = 2 =>
    [[.,:s2],[.,:s1]] := indexList
    t1 := second objMode getValue s1
    t2 := second objMode getValue s2
    zipType := ['Record,['_:,'part1,t1], ['_:,'part2,t2] ]
    zipFun := [mkAtreeNode 'Dollar, ['MakeRecord,mkEvalable t1,
                                     mkEvalable t2],
               mkAtreeNode 'makeRecord]
    form := [mkAtreeNode 'map,zipFun,s1,s2]
    [form,:zipType]
  [form,:zipType] := mkZipCode rest indexList
  [[.,:s],:.] := indexList
  t := second objMode getValue s
  zipFun := [mkAtreeNode 'Dollar, ['MakeRecord,mkEvalable t,
                                   mkEvalable zipType],
             mkAtreeNode 'makeRecord]
  form := [mkAtreeNode 'map,zipFun,s,form]
  zipType := ['Record,['_:,'part1,t],['_:,'part2,zipType]]
  [form,:zipType]

mkAndApplyZippedPredicates (s,zipType,itrl) ==
  -- for one index variable case for now.  may generalize later
  for iter in itrl repeat
    iter is ['WHILE,pred] =>
      predVec := mkIterZippedFun($indexVars,pred,zipType,$localVars)
      s := [mkAtreeNode 'swhile,predVec,s]
    iter is ['UNTIL,pred] =>
      predVec := mkIterZippedFun($indexVars,pred,zipType,$localVars)
      s := [mkAtreeNode 'suntil,predVec,s]
    iter is ['SUCHTHAT,pred] =>
      putTarget(pred,$Boolean)
      predVec := mkIterZippedFun($indexVars,pred,zipType,$localVars)
      s := [mkAtreeNode 'select,predVec,s]
  s

mkIterZippedFun(indexList,funBody,zipType,$localVars) ==
  -- transform funBody into a lamda with $index as the parameter
  numVars:= #indexList
  for [var,:.] in indexList repeat
    funBody := subVecNodes(mkIterVarSub(var,numVars),var,funBody)
  put($index,'mode,zipType,$env)
  mkLocalVar($mapName,$index)
  [m]:=bottomUpCompile funBody
  mapMode := ['Mapping,m,zipType]
  $freeVariables: local := []
  $boundVariables: local := [$index]
  -- CCL does not support upwards funargs, so we check for any free variables
  -- and pass them into the lambda as part of envArg.
  body :=
   [checkForFreeVariables(form,$localVars) for form in getValue funBody]
  parms := [$index,'envArg]
  val:=['function,declareUnusedParameters ['LAMBDA,parms,objVal body]]
  vec := mkAtreeNode gensym()
  putValue(vec,objNew(['CONS,val,["VECTOR",:reverse $freeVariables]],mapMode))
  vec

subVecNodes(new,old,form) ==
  form isnt [.,:.] =>
    (vector? form) and (form.0 = old) => new
    form
  [subVecNodes(new,old,first form), :subVecNodes(new,old,rest form)]

mkIterVarSub(var,numVars) ==
  n := iterVarPos var
  n=2 =>
    [mkAtreeNode "elt",mkNestedElts(numVars-2),mkAtreeNode 'part2]
  n=1 =>
    [mkAtreeNode "elt",mkNestedElts(numVars-2),mkAtreeNode 'part1]
  [mkAtreeNode "elt",mkNestedElts(numVars-n),mkAtreeNode 'part1]

iterVarPos var ==
  for [index,:.] in reverse $indexVars for i in 1.. repeat
    index=var => return(i)

mkNestedElts n ==
  n=0 => mkAtreeNode($index or ($index:= gensym()))
  [mkAtreeNode "elt", mkNestedElts(n-1), mkAtreeNode 'part2]

--% Handlers for construct

upconstruct t ==
  --Computes the common mode set of the construct by resolving across
  --the argument list, and evaluating
  t isnt [op,:l] => nil
  dol := getAtree(op,'dollar)
  tar := getTarget(op) or dol
  null l => upNullList(op,l,tar)
  tar is ['Record,:types] => upRecordConstruct(op,l,tar)
  isTaggedUnion tar => upTaggedUnionConstruct(op,l,tar)
  aggs := '(List)
  if tar and cons?(tar) and not isPartialMode(tar) then
    symbolMember?(first(tar),aggs) =>
      ud :=
        (l is [[realOp, :.]]) and (getUnname(realOp) = 'COLLECT) => tar
        second tar
      for x in l repeat if not getTarget(x) then putTarget(x,ud)
    first(tar) in '(Matrix SquareMatrix RectangularMatrix) =>
      vec := ['List,underDomainOf tar]
      for x in l repeat if not getTarget(x) then putTarget(x,vec)
  nargs := #l
  argModeSetList:= [bottomUp putCallInfo(x,"construct",i,nargs)
                      for x in l for i in 1..]
  dol and dol is [topType,:.] and not symbolMember?(topType,aggs) =>
    (mmS:= selectMms(op,l,tar)) and (mS:= evalForm(op,getUnname op,l,mmS)) =>
      putModeSet(op,mS)
    nil
  (tar and tar is [topType,:.] and not symbolMember?(topType,aggs)) and
    (mmS:= modemapsHavingTarget(selectMms(op,l,tar),tar)) and
        (mS:= evalForm(op,getUnname op,l,mmS)) =>
          putModeSet(op,mS)
  eltTypes := replaceSymbols([first x for x in argModeSetList],l)
  eltTypes is [['Tuple, td]] =>
    mode := ['List, td]
    evalTupleConstruct(op, l, mode, tar)
  eltTypes is [['InfiniteTuple, td]] =>
    mode := ['Stream, td]
    evalInfiniteTupleConstruct(op, l, mode, tar)
  if not isPartialMode(tar) and tar is ['List,ud] then
    mode := ['List, resolveTypeListAny [ud,:eltTypes]]
  else mode := ['List, resolveTypeListAny eltTypes]
  if isPartialMode tar then tar:=resolveTM(mode,tar)
  evalconstruct(op,l,mode,tar)

modemapsHavingTarget(mmS,target) ==
  -- returns those modemaps have the signature result matching the
  -- given target
  [mm for mm in mmS | ([[.,res,:.],:.] := mm) and res = target]

evalTupleConstruct(op,l,m,tar) ==
  ['List, ud] := m
  code := ['APPEND,
    :([["asTupleAsList", getArgValueOrThrow(x,['Tuple, ud])] for x in l])]
  val := object(code,m)

  (val1 := coerceInteractive(val,tar or m)) =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

evalInfiniteTupleConstruct(op,l,m,tar) ==
  ['Stream, ud] := m
  code := first [(getArgValue(x,['InfiniteTuple, ud]) or
    throwKeyedMsg("S2IC0007",[['InifinteTuple, ud]])) for x in l]
  val := object(code,m)
  if tar then val1 := coerceInteractive(val,tar) else val1 := val

  val1 =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

evalconstruct(op,l,m,tar) ==
  [agg,:.,underMode]:= m
  code := ['LIST, :(argCode:=[(getArgValue(x,underMode) or
    throwKeyedMsg("S2IC0007",[underMode])) for x in l])]
  val := object(code,m)
  if tar then val1 := coerceInteractive(val,tar) else val1 := val

  val1 =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

replaceSymbols(modeList,l) ==
  -- replaces symbol types with their corresponding polynomial types
  --  if not all type are symbols
  not member($Symbol,modeList) => modeList
  modeList is [a,:b] and and/[a=x for x in b] => modeList
  [if m=$Symbol then getMinimalVarMode(objValUnwrap(getValue arg),
    $declaredMode) else m for m in modeList for arg in l]

upNullList(op,l,tar) ==
  -- handler for [] (empty list)
  defMode :=
    tar and tar is [a,b] and (a in '(Stream Vector List)) and
      not isPartialMode(b) => ['List,b]
    '(List (None))
  val := objNewWrap(nil,defMode)
  tar and not isPartialMode(tar) =>
    null (val' := coerceInteractive(val,tar)) =>
      throwKeyedMsg("S2IS0013",[tar])
    putValue(op,val')
    putModeSet(op,[tar])
  putValue(op,val)
  putModeSet(op,[defMode])

upTaggedUnionConstruct(op,l,tar) ==
  -- special handler for tagged union constructors
  tar isnt [.,:types] => nil
  #l ~= 1 => throwKeyedMsg("S2IS0051",[#l,tar])
  bottomUp first l
  obj := getValue first l
  (code := coerceInteractive(getValue first l,tar)) or
    throwKeyedMsgCannotCoerceWithValue(objVal obj, objMode obj,tar)
  putValue(op,code)
  putModeSet(op,[tar])

upRecordConstruct(op,l,tar) ==
  -- special handler for record constructors
  tar isnt [.,:types] => nil
  argModes := nil
  for arg in l repeat bottomUp arg
  argCode :=
    [(getArgValue(arg,type) or throwKeyedMsgCannotCoerceWithValue(
      objVal getValue arg,objMode getValue arg,type))
        for arg in l for ['_:,.,type] in types]
  len := #l
  code :=
    (len = 1) => ['%list,:argCode]
    (len = 2) => ['%pair,:argCode]
    ['%vector,:argCode]
  putValue(op,object(code,tar))
  putModeSet(op,[tar])

--% Handlers for declarations

upDeclare t ==
  t isnt  [op,lhs,rhs] => nil
  (not $genValue) and or/[CONTAINED(var,rhs) for var in $localVars] =>
    keyedMsgCompFailure("S2IS0014",[lhs])
  mode := evaluateType unabbrev rhs
  mode = $Void => throwKeyedMsgSP("S2IS0015",nil,op)
  not isLegitimateMode(mode,nil,nil) => throwKeyedMsgSP("S2IE0004",[mode],op)
  categoryForm?(mode) => throwKeyedMsgSP("S2IE0011",[mode, 'category],op)
  packageForm?(mode) => throwKeyedMsgSP("S2IE0011",[mode, 'package],op)
  getAtree(op,"callingFunction") =>
    -- This isn't a real declaration, rather a field specification.
    not ident? lhs => throwKeyedMsg("S2IE0020",nil)
    -- ??? When we come to support field spec as type, change this.
    putValue(op,objNewWrap([":",lhs,mode],mode))
    putModeSet(op,[mode])
  junk :=
    lhs is ["free",["tuple",:vars]] or lhs is ['free,['LISTOF,:vars]] or
      lhs is ["free",:vars] =>
        for var in vars repeat declare(["free",var],mode)
    lhs is ["local",["tuple",:vars]] or lhs is ["local",['LISTOF,:vars]] or
      lhs is ["local",:vars] =>
        for var in vars repeat declare(["local",var],mode)
    lhs is ["tuple",:vars] or lhs is ["LISTOF",:vars] =>
      for var in vars repeat declare(var,mode)
    declare(lhs,mode)
  putValue(op,objNewWrap(voidValue(), $Void))
  putModeSet(op,[$Void])

declare(var,mode) ==
  -- performs declaration.
  -- 10/31/89: no longer coerces value to new declared type
  if var is ['local,v] then
    uplocalWithType(v,mode)
    var := v
  if var is ['free,v] then
    upfreeWithType(v,mode)
    var := v
  validateVariableNameOrElse var
  if get(var,'isInterpreterFunction,$e) then
    mode isnt ['Mapping,.,:args] =>
      throwKeyedMsg("S2IS0017",[var,mode])
    -- validate that the new declaration has the defined # of args
    mapval := objVal get(var,'value,$e)
    -- mapval looks like '(%Map (args . defn))
    margs := CAADR mapval
    -- if one args, margs is not a pair, just #1 or nil
    -- otherwise it looks like (tuple #1 #2 ...)
    nargs :=
      null margs => 0
      cons? margs => -1 + #margs
      1
    nargs ~= #args => throwKeyedMsg("S2IM0008",[var])
  if $compilingMap then mkLocalVar($mapName,var)
  else clearDependencies(var,true)
  isLocallyBound var => put(var,'mode,mode,$env)
  mode is ['Mapping,:.] => declareMap(var,mode)
  v := get(var,'value,$e) =>
    -- only allow this if either
    --   - value already has given type
    --   - new mode is same as old declared mode
    objMode(v) = mode => putHist(var,'mode,mode,$e)
    mode = get(var,'mode,$e) => nil   -- nothing to do
    throwKeyedMsg("S2IS0052",[var,mode])
  putHist(var,'mode,mode,$e)

declareMap(var,mode) ==
  -- declare a Mapping property
  (v:=get(var,'value,$e)) and objVal(v) isnt ["%Map",:.] =>
    throwKeyedMsg("S2IS0019",[var])
  isPartialMode mode => throwKeyedMsg("S2IM0004",nil)
  putHist(var,'mode,mode,$e)

getAndEvalConstructorArgument tree ==
  triple := getValue tree
  objMode triple = $Domain => triple
  isWrapped objVal(triple) => triple
  isLocallyBound objVal triple =>
    compFailure('"   Local variable or parameter used in type")
  objNewWrap(timedEVALFUN objVal(triple), objMode(triple))

replaceSharps(x,d) ==
  -- replaces all sharps in x by the arguments of domain d
  -- all replaces the triangle variables
  SL:= nil
  for e in rest d for var in $FormalMapVariableList repeat
    SL:= [[var,:e],:SL]
  x := subCopy(x,SL)
  SL:= nil
  for e in rest d for var in $TriangleVariableList repeat
    SL:= [[var,:e],:SL]
  subCopy(x,SL)

isDomainValuedVariable form ==
  -- returns the value of form if form is a variable with a type value
  ident? form and (val := (
    get(form,'value,$InteractiveFrame) or _
    (cons?($env) and get(form,'value,$env)) or _
    (cons?($e) and get(form,'value,$e)))) and
      (member(m := objMode(val),'((Domain) (Category)))
          or conceptualType m = $Category) =>
        objValUnwrap(val)
  nil


++ returns true if category form `c1' implies category form `c2'.
++ Both are assumed to be definite categories, i.e. they contain
++ no variables.
categoryImplies(c1,c2) ==
  c2 = $Type => true
  c1 is ["Join",:cats] =>  
    or/[categoryImplies(c,c2) for c in cats] => true
  c1 = c2
  -- ??? Should also check conditional definition and
  -- ??? possibly attributes

++ returns true if domain `d' satisfies category `c'.
evalCategory(d,c) ==
  -- tests whether domain d has category c
  isPartialMode d => true            -- maybe too generous
  -- If this is a local variable then, its declared type 
  -- must imply category `c' satisfaction.
  ident? d and (m := getmode(d,$env)) => categoryImplies(m,c)
  ofCategory(d,c)

isOkInterpMode m ==
  isPartialMode(m) => isLegitimateMode(m,nil,nil)
  isValidType(m) and isLegitimateMode(m,nil,nil)

isLegitimateRecordOrTaggedUnion u ==
  and/[x is [":",.,d] and isLegitimateMode(d,nil,nil) for x in u]

isPolynomialMode m ==
  -- If m is a polynomial type this function returns a list of its
  --  variables, and nil otherwise
  m is [op,a,:rargs] =>
    a := removeQuote a
    op in '(Polynomial RationalFunction AlgebraicFunction Expression
      ElementaryFunction LiouvillianFunction FunctionalExpression
        CombinatorialFunction) => 'all
    op = 'UnivariatePolynomial => [a]
    op = 'Variable       => [a]
    op in '(MultivariatePolynomial DistributedMultivariatePolynomial
      HomogeneousDistributedMultivariatePolynomial) => a
    nil
  nil

containsPolynomial m ==
  m isnt [.,:.] => nil
  [d,:.] := m
  symbolMember?(d,$univariateDomains) or symbolMember?(d,$multivariateDomains) or
    d in '(Polynomial RationalFunction) => true
  (m' := underDomainOf m) and containsPolynomial m'

containsVariables m ==
  m isnt [.,:.] => nil
  [d,:.] := m
  symbolMember?(d,$univariateDomains) or symbolMember?(d,$multivariateDomains) => true
  (m' := underDomainOf m) and containsVariables m'

listOfDuplicates l ==
  l is [x,:l'] =>
    member(x,l') => [x,:listOfDuplicates deleteAll(x,l')]
    listOfDuplicates l'

-- The following function removes all occurrences of x from the list l

deleteAll(x,l) ==
  null l => nil
  x = first(l) => deleteAll(x,rest l)
  [first l,:deleteAll(x,rest l)]


$iteratorVars := nil  

mkIteratorVariable id ==
  $iteratorVars := [id,:$iteratorVars]
  -- mkLocalVar('"the iterator expression",id)


++ The `void' value object (an oxymoron).  There really are constants.
$VoidValueObject := objNew(voidValue(), $Void)
$VoidCodeObject := objNew('(voidValue), $Void)

setValueToVoid t ==
  putValue(t,$VoidValueObject)
  putModeSet(t,[$Void])

setCodeToVoid t ==
  putValue(t,$VoidCodeObject)
  putModeSet(t,[$Void])

++ Interpreter macros
$InterpreterMacroAlist ==
  '((%i . (complex 0 1))
    (%e . (exp 1))
    (%pi . (pi))
    (SF . (DoubleFloat))
    (%infinity . (infinity))
    (%plusInfinity . (plusInfinity))
    (%minusInfinity . (minusInfinity)))


-- Functions which require special handlers (also see end of file)

--% Handlers for map definitions

upDEF t ==
  -- performs map definitions.  value is thrown away
  t isnt [op,def,pred,.] => nil
  v:=addDefMap(["DEF",:def],pred)
  not(LISTP(def)) or null(def) =>
    keyedSystemError("S2GE0016",['"upDEF",'"bad map definition"])
  mapOp := first def
  if LISTP(mapOp) then
    null mapOp =>
      keyedSystemError("S2GE0016",['"upDEF",'"bad map definition"])
    mapOp := first mapOp
  put(mapOp,"value",v,$e)
  setValueToVoid op

--% Handler for package calling and $ constants

++ Return non-nil if `form' designate a constant defined in the
++ domain designated by `domainForm'.  More specifically, returns:
++   nil: no such constant 
++   <%Mode>: the type of the constant.
++   T: too many constants designated by `form'.
constantInDomain?(form,domainForm) ==
    opAlist := getConstructorOperationsFromDB domainForm.op
    key := opOf form
    entryList := [entry for (entry := [.,.,.,k]) in LASSOC(key,opAlist) 
                    | k in '(CONST ASCONST)]
    entryList is [[sig,.,.,.]] => sig.target
    #entryList > 2 => true
    key = "One" => constantInDomain?(["1"], domainForm)
    key = "Zero" => constantInDomain?(["0"], domainForm)
    nil

++ Constant `c' of `type' is referenced from domain `d'; return its value
++ in the VAT `op'.
findConstantInDomain(op,c,type,d) ==
  isPartialMode d => throwKeyedMsg("S2IS0020",nil)
  val := 
    $genValue => wrap getConstantFromDomain([c],d)
    ["getConstantFromDomain",["LIST",MKQ c],MKQ d]
  type := substitute(d,"$",type)
  putValue(op,objNew(val,type))
  putModeSet(op,[type])

upDollar t ==
  -- Puts "dollar" property in atree node, and calls bottom up
  t isnt [op,D,form] => nil
  t2 := t
  (not $genValue) and "or"/[CONTAINED(var,D) for var in $localVars] =>
    keyedMsgCompFailure("S2IS0032",nil)
  D="Lisp" => upLispCall(op,form)
  if vector? D and (# D > 0) then D := D.0
  t := evaluateType unabbrev D
  categoryForm? t =>
    throwKeyedMsg("S2IE0012", [t])
  f := getUnname form
  if f = $immediateDataSymbol then
    f := objValUnwrap coerceInteractive(getValue form,$OutputForm)
    if f = '(construct) then f := "nil"
  form isnt [.,:.] and (f ~= $immediateDataSymbol) =>
    type := constantInDomain?([f],t) =>
      type ~= true => findConstantInDomain(op,f,type,t)
      -- Ambiguous constant.  FIXME: try to narrow before giving up.
      throwKeyedMsg("S2IB0008h",[f,t])
    findUniqueOpInDomain(op,f,t)

  nargs := #rest form

  (ms := upDollarTuple(op, f, t, t2, rest form, nargs)) => ms

  f ~= "construct" and null isOpInDomain(f,t,nargs) =>
    throwKeyedMsg("S2IS0023",[f,t])
  if (sig := findCommonSigInDomain(f,t,nargs)) then
    for x in sig for y in form repeat
      if x then putTarget(y,x)
  putAtree(first form,"dollar",t)
  ms := bottomUp form
  f in '(One Zero) and cons? (ms) and first(ms) = $OutputForm =>
    throwKeyedMsg("S2IS0021",[f,t])
  putValue(op,getValue first form)
  putModeSet(op,ms)


upDollarTuple(op, f, t, t2, args, nargs) ==
  -- this function tries to find a tuple function to use
  -- nargs = 1 and getUnname first args = "Tuple" => nil
  -- nargs = 1 and (ms := bottomUp first args) and ms is [["Tuple",.]] => nil
  null (singles := isOpInDomain(f,t,1)) => nil
  tuple := nil
  for [[.,arg], :.] in singles while null tuple repeat
    if arg is ['Tuple,.] then tuple := arg
  null tuple => nil
  [.,D,form] := t2
  newArg := [mkAtreeNode "tuple",:args]
  putTarget(newArg, tuple)
  ms := bottomUp newArg
  first ms ~= tuple => nil
  form := [first form, newArg]
  putAtree(first form,"dollar",t)
  ms := bottomUp form
  putValue(op,getValue first form)
  putModeSet(op,ms)

upLispCall(op,t) ==
  -- process $Lisp calls
  if t isnt [.,:.] then code:=getUnname t else
    [lispOp,:argl]:= t
    null functionp lispOp.0 =>
      throwKeyedMsg("S2IS0024",[lispOp.0])
    for arg in argl repeat bottomUp arg
    code:=[getUnname lispOp,
      :[getArgValue(arg,computedMode arg) for arg in argl]]
  rt := '(SExpression)
  putValue(op,object(code,rt))
  putModeSet(op,[rt])

--% Handlers for equation

upequation tree ==
  -- only handle this if there is a target of Boolean
  -- this should speed things up a bit
  tree isnt [op,lhs,rhs] => nil
  $Boolean ~= getTarget(op) => nil
  not vector? op => nil
  -- change equation into '='
  op.0 := "="
  bottomUp tree

--% Handler for error

uperror t ==
  -- when compiling a function, this merely inserts another argument
  -- which is the name of the function.
  not $compilingMap => nil
  t isnt [op,msg] => nil
  msgMs := bottomUp putCallInfo(msg,"error",1,1)
  msgMs isnt [=$String] => nil
  t.rest := [mkAtree object2String $mapName,msg]
  bottomUp t

--% Handlers for free and local

upfree t ==
  setCodeToVoid t

uplocal t ==
  setCodeToVoid t

upfreeWithType(var,type) ==
  sayKeyedMsg("S2IS0055",['"free",var])
  var

uplocalWithType(var,type) ==
  sayKeyedMsg("S2IS0055",['"local",var])
  var

--% Handlers for has

uphas t ==
  t isnt [op,type,prop] => nil
  -- handler for category and attribute queries
  type :=
    x := elaborateForm type
    getModeSet x is [m] and (conceptualType m = $Type or categoryForm? m) =>
      val := objValUnwrap getValue x
      $genValue => MKQ val
      ["devaluate",val]
    throwKeyedMsg("S2IE0021",[type])
  catCode :=
    -- FIXME: when we come to support category valued variable
    --        this code needs to be adapted.
    prop := unabbrev prop
    evaluateType0 prop => ["evaluateType", MKQ prop]
    MKQ prop
  code := ["NOT",["NULL",["newHasTest",type, catCode]]]
  putValue(op,object(code,$Boolean))
  putModeSet(op,[$Boolean])

--hasTest(a,b) ==
--  newHasTest(a,b)  --see NRUNFAST BOOT

--% Handlers for IF

upIF t ==
  t isnt [op,cond,a,b] => nil
  bottomUpPredicate(cond,'"if/when")
  $genValue => interpIF(op,cond,a,b)
  compileIF(op,cond,a,b,t)

compileIF(op,cond,a,b,t) ==
  -- type analyzer for compiled case where types of both branches of
  --  IF are resolved.
  ms1 := bottomUp a
  [m1] := ms1
  b = "%noBranch" =>
    evalIF(op,rest t,$Void)
    putModeSet(op,[$Void])
  b = "%noMapVal" =>
    -- if this was a return statement, we take the mode to be that
    -- of what is being returned.
    if getUnname a = 'return then
      ms1 := bottomUp second a
      [m1] := ms1
    evalIF(op,rest t,m1)
    putModeSet(op,ms1)
  ms2 := bottomUp b
  [m2] := ms2
  m:=
    m2=m1 => m1
    m2 = $Exit => m1
    m1 = $Exit => m2
    if m1 = $Symbol then
      m1:=getMinimalVarMode(getUnname a,$declaredMode)
    if m2 = $Symbol then
      m2:=getMinimalVarMode(getUnname b,$declaredMode)
    (r := resolveTTAny(m2,m1)) => r
    rempropI($mapName,'localModemap)
    rempropI($mapName,'localVars)
    rempropI($mapName,'mapBody)
    throwKeyedMsg("S2IS0026",[m2,m1])
  evalIF(op,rest t,m)
  putModeSet(op,[m])

evalIF(op,[cond,a,b],m) ==
  -- generate code form compiled IF
  elseCode:=
    b="%noMapVal" =>
      [[MKQ true, ["throwKeyedMsg",MKQ "S2IM0018",
        ["CONS",MKQ object2Identifier $mapName,nil]]]]
    b='%noBranch =>
      $lastLineInSEQ => [[MKQ true,["voidValue"]]]
      nil
    [[MKQ true,genIFvalCode(b,m)]]
  code:=['%when,[getArgValue(cond,$Boolean),
    genIFvalCode(a,m)],:elseCode]
  triple:= objNew(code,m)
  putValue(op,triple)

genIFvalCode(t,m) ==
  -- passes type information down braches of IF statement
  --  So that coercions can be performed on data at branches of IF.
  m1 := computedMode t
  m1=m => getArgValue(t,m)
  code:=objVal getValue t
  IFcodeTran(code,m,m1)

IFcodeTran(code,m,m1) ==
  -- coerces values at branches of IF
  null code => code
  code is ["spadThrowBrightly",:.] => code
  m1 = $Exit => code
  code isnt ['%when,[p1,a1],['%otherwise,a2]] =>
    m = $Void => code
    code' := coerceInteractive(objNew(quote2Wrapped code,m1),m) =>
      getValueNormalForm code'
    throwKeyedMsgCannotCoerceWithValue(quote2Wrapped code,m1,m)
  a1:=IFcodeTran(a1,m,m1)
  a2:=IFcodeTran(a2,m,m1)
  ['%when,[p1,a1],['%otherwise,a2]]

interpIF(op,cond,a,b) ==
  -- non-compiled version of IF type analyzer.  Doesn't resolve accross
  --  branches of the IF.
  val:= getValue cond
  val:= coerceInteractive(val,$Boolean) =>
    objValUnwrap(val) => upIFgenValue(op,a)
    b="%noBranch" => setValueToVoid op
    upIFgenValue(op,b)
  throwKeyedMsg("S2IS0031",nil)

upIFgenValue(op,tree) ==
  -- evaluates tree and transfers the results to op
  ms:=bottomUp tree
  val:= getValue tree
  putValue(op,val)
  putModeSet(op,ms)

--% Handlers for is

upis t ==
  t isnt [op,a,pattern] => nil
  $opIsIs : local := true
  upisAndIsnt t

upisnt t ==
  t isnt [op,a,pattern] => nil
  $opIsIs : local := nil
  upisAndIsnt t

upisAndIsnt(t:=[op,a,pattern]) ==
  -- handler for "is" pattern matching
  mS:= bottomUp a
  mS isnt [m] =>
    keyedSystemError("S2GE0016",['"upisAndIsnt",'"non-unique modeset"])
  putPvarModes(removeConstruct pattern,m)
  evalis(op,rest t,m)
  putModeSet(op,[$Boolean])

putPvarModes(pattern,m) ==
  -- Puts the modes for the pattern variables into $env
  m isnt ["List",um] => throwKeyedMsg("S2IS0030",nil)
  for pvar in pattern repeat
    ident? pvar => (not (pvar=$quadSymbol)) and put(pvar,'mode,um,$env)
    pvar is ['_:,var] =>
      null (var=$quadSymbol) and put(var,"mode",m,$env)
    pvar is ['_=,var] =>
      null (var=$quadSymbol) and put(var,"mode",um,$env)
    putPvarModes(pvar,um)

evalis(op,[a,pattern],mode) ==
  -- actually handles is and isnt
  if $opIsIs
    then fun := 'evalIsPredicate
    else fun := 'evalIsntPredicate
  if isLocalPred pattern then
    code:= compileIs(a,pattern)
  else code:=[fun,getArgValue(a,mode),
    MKQ pattern,MKQ mode]
  triple := object(code,$Boolean)
  putValue(op,triple)

isLocalPred pattern ==
  -- returns true if this predicate is to be compiled
  for pat in pattern repeat
    ident? pat and isLocallyBound pat => return true
    pat is [":",var] and isLocallyBound var => return true
    pat is ["=",var] and isLocallyBound var => return true

compileIs(val,pattern) ==
  -- produce code for compiled "is" predicate.  makes pattern variables
  --  into local variables of the function
  vars:= nil
  for pat in rest pattern repeat
    ident?(pat) and isLocallyBound pat => vars:=[pat,:vars]
    pat is [":",var] => vars:= [var,:vars]
    pat is ["=",var] => vars:= [var,:vars]
  predCode:=["%LET",g:=gensym(),["isPatternMatch",
    getArgValue(val,computedMode val),MKQ removeConstruct pattern]]
  for var in removeDuplicates vars repeat
    assignCode:=[["%LET",var,["CDR",["objectAssoc",MKQ var,g]]],:assignCode]
  null $opIsIs =>
    ['%when,[["EQ",predCode,MKQ "failed"],['%seq,:assignCode,'%true]]]
  ['%when,[['%not,["EQ",predCode,MKQ "failed"]],['%seq,:assignCode,'%true]]]

evalIsPredicate(value,pattern,mode) ==
  --This function pattern matches value to pattern, and returns
  --true if it matches, and false otherwise.  As a side effect
  --if the pattern matches then the bindings given in the pattern
  --are made
  pattern:= removeConstruct pattern
  not ((valueAlist:=isPatternMatch(value,pattern))='failed) =>
    for [id,:value] in valueAlist repeat
      evalLETchangeValue(id,objNewWrap(value,get(id,'mode,$env)))
    true
  false

evalIsntPredicate(value,pattern,mode) ==
  evalIsPredicate(value,pattern,mode) => false
  true

removeConstruct pat ==
  -- removes the "construct" from the beginning of patterns
  if pat is ["construct",:p] then pat:=p
  if pat is ["cons", a, b] then pat := [a, [":", b]]
  pat isnt [.,:.] => pat
  pat.first := removeConstruct first pat
  pat.rest := removeConstruct rest pat
  pat

isPatternMatch(l,pats) ==
  -- perform the actual pattern match
  $subs: local := nil
  isPatMatch(l,pats)
  $subs

isPatMatch(l,pats) ==
  null pats =>
    null l => $subs
    $subs:='failed
  null l =>
    null pats => $subs
    pats is [[":",var]] =>
      $subs := [[var],:$subs]
    $subs:='failed
  pats is [pat,:restPats] =>
    ident? pat =>
      $subs:=[[pat,:first l],:$subs]
      isPatMatch(rest l,restPats)
    pat is ["=",var] =>
      p := objectAssoc(var,$subs) =>
        first l = rest p => isPatMatch(rest l, restPats)
        $subs:="failed"
      $subs:="failed"
    pat is [":",var] =>
      n:=#restPats
      m:=#l-n
      m<0 => $subs:="failed"
      n = 0 => $subs:=[[var,:l],:$subs]
      $subs:=[[var,:[x for x in l for i in 1..m]],:$subs]
      isPatMatch(drop(m,l),restPats)
    isPatMatch(first l,pat) = "failed" => "failed"
    isPatMatch(rest l,restPats)
  keyedSystemError("S2GE0016",['"isPatMatch",
     '"unknown form of is predicate"])

--% Handler for iterate

upiterate t ==
  null $repeatBodyLabel => throwKeyedMsg("S2IS0029",['"iterate"])
  $iterateCount := $iterateCount + 1
  code := ["THROW",$repeatBodyLabel,'(voidValue)]
  $genValue => THROW(eval $repeatBodyLabel,voidValue())
  putValue(t,objNew(code,$Void))
  putModeSet(t,[$Void])

--% Handler for break

upbreak t ==
  t isnt [op,.] => nil
  null $repeatLabel => throwKeyedMsg("S2IS0029",['"break"])
  $breakCount := $breakCount + 1
  code := ["THROW",$repeatLabel,'(voidValue)]
  $genValue => THROW(eval $repeatLabel,voidValue())
  putValue(op,objNew(code,$Void))
  putModeSet(op,[$Void])

--% Handlers for %LET

up%LET t ==
  -- analyzes and evaluates the righthand side, and does the variable
  -- binding
  t isnt [op,lhs,rhs] => nil
  $declaredMode: local := nil
  cons? lhs =>
    var:= getUnname first lhs
    var = "construct" => upLETWithPatternOnLhs t
    var = 'QUOTE => throwKeyedMsg("S2IS0027",['"A quoted form"])
    upLETWithFormOnLhs(op,lhs,rhs)
  var:= getUnname lhs
  var = $immediateDataSymbol =>
    -- following will be immediate data, so probably ok to not
    -- specially format it
    obj := objValUnwrap coerceInteractive(getValue lhs,$OutputForm)
    throwKeyedMsg("S2IS0027",[obj])
  var in '(% %%) =>               -- for history
    throwKeyedMsg("S2IS0027",[var])
  (ident? var) and not (var in '(true false elt QUOTE)) =>
    var ~= (var' := unabbrev(var)) =>  -- constructor abbreviation
      throwKeyedMsg("S2IS0028",[var,var'])
    if get(var,'isInterpreterFunction,$e) then
      putHist(var,'isInterpreterFunction,false,$e)
      sayKeyedMsg("S2IS0049",['"Function",var])
    else if get(var,'isInterpreterRule,$e) then
      putHist(var,'isInterpreterRule,false,$e)
      sayKeyedMsg("S2IS0049",['"Rule",var])
    (m := isType rhs) => upLETtype(op,lhs,m)
    transferPropsToNode(var,lhs)
    if ( m:= getMode(lhs) ) then
      $declaredMode := m
      putTarget(rhs,m)
    if (val := getValue lhs) and (objMode val = $Boolean) and
      getUnname(rhs) = 'equation then putTarget(rhs,$Boolean)
    (rhsMs:= bottomUp rhs) = [$Void] =>
      throwKeyedMsg("S2IS0034",[var])
    val:=evalLET(lhs,rhs)
    putValue(op,val)
    putModeSet(op,[objMode(val)])
  throwKeyedMsg("S2IS0027",[var])

evalLET(lhs,rhs) ==
  -- lhs is a vector for a variable, and rhs is the evaluated atree
  --  for the value which is coerced to the mode of lhs
  $useConvertForCoercions: local := true
  v' := (v:= getValue rhs)
  ((not getMode lhs) and (getModeSet rhs is [.])) or
    get(getUnname lhs,'autoDeclare,$env) =>
      v:=
        $genValue => v
        objNew(getValueNormalForm v,objMode v)
      evalLETput(lhs,v)
  t1:= objMode v
  t2' := (t2 := getMode lhs)
  value:=
    t1 = t2 =>
      $genValue => v
      objNew(getValueNormalForm v,objMode v)
    if isPartialMode t2 then
      if t1 = $Symbol and $declaredMode then
        t1:= getMinimalVarMode(objValUnwrap v,$declaredMode)
      t' := t2
      null (t2 := resolveTM(t1,t2)) =>
        if not t2 then t2 := t'
        throwKeyedMsg("S2IS0035",[t1,t2])
    null (v := getArgValue(rhs,t2)) =>
      isWrapped(objVal v') and (v2:=coerceInteractive(v',$OutputForm)) =>
        throwKeyedMsg("S2IS0036",[objValUnwrap v2,t2])
      throwKeyedMsg("S2IS0037",[t2])
    t2 and object(v,t2)
  value => evalLETput(lhs,value)
  throwKeyedMsgCannotCoerceWithValue(objVal v,t1,getMode lhs)

evalLETput(lhs,value) ==
  -- put value into the cell for lhs
  name:= getUnname lhs
  if not $genValue then
    code:=
      isLocallyBound name =>
        om := objMode(value)
        dm := get(name,'mode,$env)
        dm and not ((om = dm) or isSubDomain(om,dm) or
          isSubDomain(dm,om)) =>
            compFailure ['"   The type of the local variable",
              :bright name,'"has changed in the computation."]
        if dm and isSubDomain(dm,om) then put(name,'mode,om,$env)
        ["%LET",name,objVal value,$mapName]
               -- $mapName is set in analyzeMap
      om := objMode value
      dm := get(name, 'mode, $env) or objMode(get(name, 'value, $e))
      dm and (null $compilingMap) and not(om = dm) and not(isSubDomain(om, dm)) =>
        THROW('loopCompiler,'tryInterpOnly)
      ['unwrap,['evalLETchangeValue,MKQ name,
        objNewCode(['wrap,objVal value],objMode value)]]
    value:= objNew(code,objMode value)
    isLocallyBound name =>
      if not get(name,'mode,$env) then put(name,'autoDeclare,'T,$env)
      put(name,'mode,objMode(value),$env)
    put(name,'automode,objMode(value),$env)
  $genValue and evalLETchangeValue(name,value)
  putValue(lhs,value)

upLETWithPatternOnLhs(t := [op,pattern,a]) ==
  $opIsIs : local := true
  [m] := bottomUp a
  putPvarModes(pattern,m)
  object := evalis(op,[a,pattern],m)
  -- have to change code to return value of a
  failCode :=
    ['spadThrowBrightly,['concat,
      '"   Pattern",quote bright form2String pattern,
        '"is not matched in assignment to right-hand side."]]
  if $genValue
    then
      null objValUnwrap object => eval failCode
      putValue(op,getValue a)
    else
      code := ['%when,[objVal object,objVal getValue a],['%otherwise,failCode]]
      putValue(op,objNew(code,m))
  putModeSet(op,[m])

evalLETchangeValue(name,value) ==
  -- write the value of name into the environment, clearing dependent
  --  maps if its type changes from its last value
  localEnv := cons? $env
  clearCompilationsFlag :=
    val:= (localEnv and get(name,'value,$env)) or get(name,'value,$e)
    null val =>
      not ((localEnv and get(name,'mode,$env)) or get(name,'mode,$e))
    objMode val ~= objMode(value)
  if clearCompilationsFlag then
    clearDependencies(name,true)
  if localEnv and isLocallyBound name
    then $env:= putHist(name,'value,value,$env)
    else putIntSymTab(name,'value,value,$e)
  objVal value

upLETWithFormOnLhs(op,lhs,rhs) ==
  -- bottomUp for assignment to forms (setelt, table or tuple)
  lhs' := getUnnameIfCan lhs
  rhs' := getUnnameIfCan rhs
  lhs' = "tuple" =>
    rhs' ~= "tuple" => throwKeyedMsg("S2IS0039",nil)
    #(lhs) ~= #(rhs) => throwKeyedMsg("S2IS0038",nil)
    -- generate a sequence of assignments, using local variables
    -- to first hold the assignments so that things like
    -- (t1,t2) := (t2,t1) will work.
    seq := []
    temps := [gensym() for l in rest lhs]
    for lvar in temps repeat mkLocalVar($mapName,lvar)
    for l in reverse rest lhs for t in temps repeat
      transferPropsToNode(getUnname l,l)
      let := mkAtreeNode "%LET"
      t'  := mkAtreeNode t
      if m := getMode(l) then putMode(t',m)
      seq := [[let,l,t'],:seq]
    for t in temps for r in reverse rest rhs
      for l in reverse rest lhs repeat
        let := mkAtreeNode "%LET"
        t'  := mkAtreeNode t
        if m := getMode(l) then putMode(t',m)
        seq := [[let,t',r],:seq]
    seq := [mkAtreeNode 'SEQ,:seq]
    ms := bottomUp seq
    putValue(op,getValue seq)
    putModeSet(op,ms)
  rhs' = "tuple" => throwKeyedMsg("S2IS0039",nil)
  tree:= seteltable(lhs,rhs) => upSetelt(op,lhs,tree)
  throwKeyedMsg("S2IS0060", nil)
--  upTableSetelt(op,lhs,rhs)

seteltable(lhs is [f,:argl],rhs) ==
  -- produces the setelt form for trees such as "l.2:= 3"
  null (g := getUnnameIfCan f) => nil
  g="elt" => altSeteltable [:argl, rhs]
  get(g,'value,$e) is [expr,:.] and isMapExpr expr => nil
  transferPropsToNode(g,f)
  getValue(lhs) or getMode(lhs) =>
    f is [f',:argl'] => altSeteltable [f',:argl',:argl,rhs]
    altSeteltable [:lhs,rhs]
  nil

altSeteltable args ==
    for x in args repeat bottomUp x
    newOps := [mkAtreeNode "setelt", mkAtreeNode "set!"]
    form := nil

    -- first look for exact matches for any of the possibilities
    while null form for newOp in newOps  repeat
        if selectMms(newOp, args, nil) then form := [newOp, :args]

    -- now try retracting arguments after the first
    while null form and ( "and"/[retractAtree(a) for a in rest args] ) repeat
        while null form for newOp in newOps  repeat
            if selectMms(newOp, args, nil) then form := [newOp, :args]

    form


upSetelt(op,lhs,tree) ==
  -- type analyzes implicit setelt forms
  var:=opOf lhs
  transferPropsToNode(getUnname var,var)
  if (m1:=getMode var) then $declaredMode:= m1
  if m1 or ((v1 := getValue var) and (m1 := objMode v1)) then
    putModeSet(var,[m1])
  ms := bottomUp tree
  putValue(op,getValue tree)
  putModeSet(op,ms)

upTableSetelt(op,lhs is [htOp,:args],rhs) ==
  -- called only for undeclared, uninitialized table setelts
  ("*" = (PNAME getUnname htOp).0) and (1 ~= # args) =>
    throwKeyedMsg("S2IS0040",nil)
  # args ~= 1 =>
    throwKeyedMsg("S2IS0041",[[getUnname htOp,'".[",
      getUnname first args,
        ['",",getUnname arg for arg in rest args],'"]"]])
  keyMode := $Any
  putMode (htOp,['Table,keyMode,$Any])
  -- if we are to use a new table, we must call the "table"
  -- function to give it an initial value.
  bottomUp [mkAtreeNode "%LET",htOp,[mkAtreeNode 'table]]
  tableCode := objVal getValue htOp
  r := upSetelt(op, lhs, [mkAtreeNode "setelt",:lhs,rhs])
  $genValue => r
  -- construct code
  t := getValue op
  putValue(op,objNew(['PROGN,tableCode,objVal t],objMode t))
  r

unVectorize body ==
  -- transforms from an atree back into a tree
  vector? body =>
    name := getUnname body
    name ~= $immediateDataSymbol => name
    objValUnwrap getValue body
  body isnt [.,:.] => body
  body is [op,:argl] =>
    newOp:=unVectorize op
    if newOp = 'SUCHTHAT then newOp := "|"
    if newOp = 'COERCE then newOp := "::"
    if newOp = 'Dollar then newOp := "$elt"
    [newOp,:unVectorize argl]
  systemErrorHere ["unVectorize",body]

isType t ==
  -- Returns the evaluated type if t is a tree representing a type,
  -- and nil otherwise
   op:=opOf t
   vector? op =>
     isMap(op:= getUnname op) => nil
     op = 'Mapping and cons? t =>
       argTypes := [isType type for type in rest t]
       "or"/[null type for type in argTypes] => nil
       ['Mapping, :argTypes]
     isLocallyBound op => nil
     d := isDomainValuedVariable op => d
     type:=
       -- next line handles subscripted vars
         (abbreviation?(op) or (op = 'typeOf) or
           constructor?(op) or (op in '(Record Union Enumeration))) and
             unabbrev unVectorize t
     type and evaluateType type
   d := isDomainValuedVariable op => d
   nil

upLETtype(op,lhs,type) ==
  -- performs type assignment
  opName:= getUnname lhs
  (not $genValue) and "or"/[CONTAINED(var,type) for var in $localVars] =>
    compFailure ['"   Cannot compile type assignment to",:bright opName]
  mode := conceptualType type
  val:= objNew(type,mode)
  if isLocallyBound opName then put(opName,'value,val,$env)
  else putHist(opName,'value,val,$e)
  putValue(op,val)
  -- have to fix the following
  putModeSet(op,[mode])

++ Note: this function is used in the algebra part.
assignSymbol(symbol, value, domain) ==
-- Special function for binding an interpreter variable from within algebra
-- code.  Does not do the assignment and returns nil, if the variable is
-- already assigned
  val := get(symbol, 'value, $e) => nil
  obj := objNew(wrap value, devaluate domain)
  put(symbol, 'value, obj, $e)
  true

--% Handler for Interpreter Macros

getInterpMacroNames() ==
  names := [n for [n,:.] in $InterpreterMacroAlist]
  if (e := CAAR $InteractiveFrame) and (m := assoc("--macros--",e)) then
    names := append(names,[n for [n,:.] in rest m])
  MSORT names

isInterpMacro name ==
  -- look in local and then global environment for a macro
  not ident? name => nil
  symbolMember?(name,$specialOps) => nil
  (m := get("--macros--",name,$env)) => m
  (m := get("--macros--",name,$e))   => m
  (m := get("--macros--",name,$InteractiveFrame))   => m
  -- $InterpreterMacroAlist will probably be phased out soon
  (sv := assoc(name,$InterpreterMacroAlist)) => [nil,:rest sv]
  nil

--% Handlers for prefix QUOTE

upQUOTE t ==
  t isnt [op,expr] => nil
  ms:= list
    m:= getBasicMode expr => m
    ident? expr =>
--    $useSymbolNotVariable => $Symbol
      getTarget t = $Identifier => $Identifier
      ['Variable,expr]
    $InputForm
  evalQUOTE(op,[expr],ms)
  putModeSet(op,ms)

evalQUOTE(op,[expr],[m]) ==
  triple:=
    $genValue => objNewWrap(expr,m)
    objNew(quote expr,m)
  putValue(op,triple)

--% Quasiquotation
up_[_|_|_] t ==
  t isnt [op, x] => nil
  mode := getTypeOfSyntax x
  putValue(op, objNewWrap(x, mode))
  putModeSet(op, [mode])

--% Handler for pretend

uppretend t ==
  t isnt [op,expr,type] => nil
  mode := evaluateType unabbrev type
  not isValidType(mode) => throwKeyedMsg("S2IE0004",[mode])
  bottomUp expr
  putValue(op,objNew(objVal getValue expr,mode))
  putModeSet(op,[mode])

--% Handlers for REDUCE

-----------------------Compiler for Interpreter---------------------------------
NRTcompileEvalForm(opName,sigTail,dcVector) ==
  u := NRTcompiledLookup(opName,sigTail,dcVector)
  not $insideCompileBodyIfTrue => MKQ u
  k := NRTgetMinivectorIndex(u,opName,sigTail,dcVector)
  ['ELT,"$$$",k]  --$$$ denotes minivector

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
NRTcompiledLookup(op,sig,dom) ==
  if CONTAINED('_#,sig) then
      sig := [NRTtypeHack t for t in sig]
  compiledLookupCheck(op,sig,dom)

NRTtypeHack t ==
  t isnt [.,:.] => t
  first t = '_# => # second t
  [first t,:[NRTtypeHack tt for tt in rest t]]

NRTgetMinivectorIndex(u,op,sig,domVector) ==
  s := # $minivector
  k := or/[k for k in 0..(s-1)
        for x in $minivector | sameObject?(x,u)] => k
  $minivector := [:$minivector,u]
  s

getReduceFunction(op,type,result, locale) ==
  -- return the function cell for operation with the signature
  --  (type,type) -> type, possible from locale
  if type is ['Variable,var] then
    args := [arg := mkAtreeNode var,arg]
    putValue(arg,objNewWrap(var,type))
  else
    args := [arg := mkAtreeNode "%1",arg]
    if type=$Symbol then putValue(arg,objNewWrap("%1",$Symbol))
  putModeSet(arg,[type])
  vecOp:=mkAtreeNode op
  transferPropsToNode(op,vecOp)
  if locale then putAtree(vecOp,'dollar,locale)
  mmS:= selectMms(vecOp,args,result)
  mm:= or/[mm for (mm:=[[.,:sig],fun,cond]) in mmS |
    (isHomogeneousArgs sig) and "and"/[null c for c in cond]]
  null mm => 'failed
  [[dc,:sig],fun,:.]:=mm
  dc='local => [MKQ [fun,:'local],:first sig]
  dcVector := evalDomain dc
  $compilingMap =>
    k := NRTgetMinivectorIndex(
      NRTcompiledLookup(op,sig,dcVector),op,sig,dcVector)
    ['ELT,"$$$",k]  --$$$ denotes minivector
  env:=
    NRTcompiledLookup(op,sig,dcVector)
  MKQ env

isHomogeneous sig ==
  --return true if sig describes a homogeneous binary operation
  sig.0=sig.1 and sig.1=sig.2

isHomogeneousArgs sig ==
  --return true if sig describes a homogeneous binary operation
  sig.1=sig.2

--% Handlers for REPEAT

transformREPEAT [:itrl,body] ==
  -- syntactic transformation of repeat iterators, called from mkAtree2
  iterList:=[:iterTran1 for it in itrl] where iterTran1() ==
    it is ["STEP",index,lower,step,:upperList] =>
      [["STEP",index,mkAtree1 lower,mkAtree1 step,:[mkAtree1 upper
        for upper in upperList]]]
    it is ["IN",index,s] =>
      [['IN,index,mkAtree1 s]]
    it is ["ON",index,s] =>
      [['IN,index,mkAtree1 ['tails,s]]]
    it is ["WHILE",b] =>
      [["WHILE",mkAtree1 b]]
    it is ["|",pred] =>
      [["SUCHTHAT",mkAtree1 pred]]
    it is [op,:.] and (op in '(VALUE UNTIL)) => nil
  bodyTree:=mkAtree1 body
  iterList:=append!(iterList,[:iterTran2 for it in itrl]) where iterTran2() ==
    it is ["STEP",:.] => nil
    it is ["IN",:.] => nil
    it is ["ON",:.] => nil
    it is ["WHILE",:.] => nil
    it is [op,b] and (op in '(UNTIL VALUE)) =>
      [[op,mkAtree1 b]]
    it is ['_|,pred] => nil
    keyedSystemError("S2GE0016",
      ['"transformREPEAT",'"Unknown type of iterator"])
  [:iterList,bodyTree]

upREPEAT t ==
  -- REPEATS always return void() of Void
  -- assures throw to interpret-code mode goes to outermost loop
  $repeatLabel : local := MKQ gensym()
  $breakCount  : local := 0
  $repeatBodyLabel : local := MKQ gensym()
  $iterateCount    : local := 0
  $compilingLoop => upREPEAT1 t
  upREPEAT0 t

upREPEAT0 t ==
  -- sets up catch point for interp-only mode
  $compilingLoop: local := true
  ms := CATCH('loopCompiler,upREPEAT1 t)
  ms = 'tryInterpOnly => interpOnlyREPEAT t
  ms

upREPEAT1 t ==
  -- repeat loop handler with compiled body
  -- see if it has the expected form
  t isnt [op,:itrl,body] => nil
  -- determine the mode of the repeat loop. At the moment, if there
  -- there are no iterators and there are no "break" statements, then
  -- the return type is Exit, otherwise Void.
  repeatMode :=
    null(itrl) and ($breakCount=0) => $Void
    $Void

  -- if interpreting, go do that
  $interpOnly => interpREPEAT(op,itrl,body,repeatMode)

  -- analyze iterators and loop body
  $iteratorVars: local := nil
  upLoopIters itrl
  bottomUpCompile body

  -- now that the body is analyzed, we should know everything that
  -- is in the UNTIL clause
  for itr in itrl repeat
    itr is ["UNTIL", pred] => bottomUpCompilePredicate(pred,'"until")

  -- now go do it
  evalREPEAT(op,rest t,repeatMode)
  putModeSet(op,[repeatMode])

evalREPEAT(op,[:itrl,body],repeatMode) ==
  -- generate code for loop
  bodyMode := computedMode body
  bodyCode := getArgValue(body,bodyMode)
  if $iterateCount > 0 then
    bodyCode := ["CATCH",$repeatBodyLabel,bodyCode]
  code := ['%repeat,:[evalLoopIter itr for itr in itrl],bodyCode,voidValue()]
  code := timedOptimization code
  if $breakCount > 0 then code := ['CATCH,$repeatLabel,code]
  val :=
    $genValue =>
      timedEVALFUN code
      objNewWrap(voidValue(),repeatMode)
    objNew(code,repeatMode)
  putValue(op,val)

interpOnlyREPEAT t ==
  -- interpret-code mode call to upREPEAT
  $genValue: local := true
  $interpOnly: local := true
  upREPEAT1 t

interpREPEAT(op,itrl,body,repeatMode) ==
  -- performs interpret-code repeat
  $indexVars: local := nil
  $indexTypes: local := nil
  code :=
      -- we must insert a CATCH for the iterate clause
      ['%repeat,:[interpIter itr for itr in itrl],
        ["CATCH",$repeatBodyLabel,interpLoop(body,$indexVars,
          $indexTypes,nil)],voidValue()]
  CATCH(eval $repeatLabel,timedEVALFUN code)
  val:= objNewWrap(voidValue(),repeatMode)
  putValue(op,val)
  putModeSet(op,[repeatMode])

interpLoop(expr,indexList,indexTypes,requiredType) ==
  -- generates code for interp-only repeat body
  ['interpLoopIter,MKQ expr,MKQ indexList,["LIST",:indexList],
    MKQ indexTypes, MKQ requiredType]

interpLoopIter(exp,indexList,indexVals,indexTypes,requiredType) ==
  -- call interpreter on exp with loop vars in indexList with given
  --  values and types, requiredType is used from interpCOLLECT
  --  to indicate the required type of the result
  emptyAtree exp
  for i in indexList for val in indexVals for type in indexTypes repeat
    put(i,'value,objNewWrap(val,type),$env)
  bottomUp exp
  v:= getValue exp
  val :=
    null requiredType => v
    coerceInteractive(v,requiredType)
  null val =>
    throwKeyedMsgCannotCoerceWithValue(objVal v,objMode v,requiredType)
  objValUnwrap val

--% Handler for return

upreturn t ==
  -- make sure we are in a user function
  t isnt [op,val] => nil
  (null $compilingMap) and (null $interpOnly) =>
    throwKeyedMsg("S2IS0047",nil)
  if $mapTarget then putTarget(val,$mapTarget)
  bottomUp val
  if $mapTarget
    then
      val' := getArgValue(val, $mapTarget)
      m := $mapTarget
    else
      val' := getValueNormalForm getValue val
      m := computedMode val
  cn := mapCatchName $mapName
  $mapReturnTypes := insert(m, $mapReturnTypes)
  $mapThrowCount := $mapThrowCount + 1
  -- if $genValue then we are interpreting the map
  $genValue => THROW(cn,objNewWrap(removeQuote val',m))
  putValue(op,objNew(['THROW,MKQ cn,val'],m))
  putModeSet(op,[$Exit])

--% Handler for SEQ

upSEQ u ==
  -- assumes that exits were translated into if-then-elses
  -- handles flat SEQs and embedded returns
  u isnt [op,:args] => nil
  if (target := getTarget(op)) then putTarget(last args, target)
  for x in args repeat bottomUp x
  null (m := computedMode last args) =>
    keyedSystemError("S2GE0016",['"upSEQ",
      '"last line of SEQ has no mode"])
  evalSEQ(op,args,m)
  putModeSet(op,[m])

evalSEQ(op,args,m) ==
  -- generate code for SEQ
  [:argl,last] := args
  val:=
    $genValue => getValue last
    bodyCode := nil
    for x in args repeat
      (m1 := computedMode x) and (m1 ~= '$ThrowAwayMode) =>
        (av := getArgValue(x,m1)) ~= voidValue() =>
          bodyCode := [av,:bodyCode]
    code:=
      bodyCode is [c] => c
      ['PROGN,:reverse bodyCode]
    objNew(code,m)
  putValue(op,val)

--% Handlers for tuple

uptuple t ==
  --Computes the common mode set of the construct by resolving across
  --the argument list, and evaluating
  t isnt [op,:l] => nil
  dol := getAtree(op,'dollar)
  tar := getTarget(op) or dol
  null l => upNullTuple(op,l,tar)
  isTaggedUnion tar => upTaggedUnionConstruct(op,l,tar)
  aggs := '(List)
  if tar and cons?(tar) and not isPartialMode(tar) then
    symbolMember?(first(tar),aggs) =>
      ud := second tar
      for x in l repeat if not getTarget(x) then putTarget(x,ud)
    first(tar) in '(Matrix SquareMatrix RectangularMatrix) =>
      vec := ['List,underDomainOf tar]
      for x in l repeat if not getTarget(x) then putTarget(x,vec)
  argModeSetList:= [bottomUp x for x in l]
  eltTypes := replaceSymbols([first x for x in argModeSetList],l)
  if not isPartialMode(tar) and tar is ['Tuple,ud] then
    mode := ['Tuple, resolveTypeListAny [ud,:eltTypes]]
  else mode := ['Tuple, resolveTypeListAny eltTypes]
  if isPartialMode tar then tar:=resolveTM(mode,tar)
  evalTuple(op,l,mode,tar)

evalTuple(op,l,m,tar) ==
  [agg,:.,underMode]:= m
  code := asTupleNewCode(underMode, #l,
    [(getArgValue(x,underMode) or throwKeyedMsg("S2IC0007",[underMode])) for x in l])
  val := object(code,m)
  if tar then val1 := coerceInteractive(val,tar) else val1 := val

  val1 =>
    putValue(op,val1)
    putModeSet(op,[tar or m])
  putValue(op,val)
  putModeSet(op,[m])

upNullTuple(op,l,tar) ==
  -- handler for the empty tuple
  defMode :=
    tar and tar is [a,b] and (a in '(Stream Vector List)) and
      not isPartialMode(b) => ['Tuple,b]
    '(Tuple (None))
  val := objNewWrap(asTupleNew(getVMType second defMode,0,nil), defMode)
  tar and not isPartialMode(tar) =>
    null (val' := coerceInteractive(val,tar)) =>
      throwKeyedMsg("S2IS0013",[tar])
    putValue(op,val')
    putModeSet(op,[tar])
  putValue(op,val)
  putModeSet(op,[defMode])

--% Handler for typeOf

uptypeOf form ==
  form isnt [op, arg] => nil
  if vector? arg then transferPropsToNode(getUnname arg,arg)
  if m := isType(arg) then
    m := conceptualType m
  else if not (m := getMode arg) then [m] := bottomUp arg
  t := conceptualType m        -- ??? shall we reveal more impl. details?
  putValue(op, objNew(m,t))
  putModeSet(op,[t])

--% Handler for where

upwhere t ==
  -- upwhere does the puts in where into a local environment
  t isnt [op,tree,clause] => nil
  -- since the "clause" might be a local macro, we now call mkAtree
  -- on the "tree" part (it is not yet a vat)
  not $genValue =>
    compFailure [:bright '"  where",
      '"for compiled code is not yet implemented."]
  $whereCacheList : local := nil
  [env,:e] := upwhereClause(clause,$env,$e)
  tree := upwhereMkAtree(tree,env,e)
  if x := getAtree(op,'dollar) then
    tree isnt [.,:.] => throwKeyedMsg("S2IS0048",nil)
    putAtree(first tree,'dollar,x)
  upwhereMain(tree,env,e)
  val := getValue tree
  putValue(op,val)
  result := putModeSet(op,getModeSet tree)
  wcl := [op for op in $whereCacheList]
  for op in wcl repeat clearDependencies(op,'T)
  result

upwhereClause(tree,env,e) ==
  -- uses the variable bindings from env and e and returns an environment
  -- of its own bindings
  $env: local := copyHack env
  $e: local := copyHack e
  bottomUp tree
  [$env,:$e]

upwhereMkAtree(tree,$env,$e) == mkAtree tree

upwhereMain(tree,$env,$e) ==
  -- uses local copies of $env and $e while evaluating tree
  bottomUp tree

copyHack(env) ==
  -- makes a copy of an environment with the exception of pairs
  -- (localModemap . something)
  c:= CAAR env
  d:= [fn p for p in c] where fn(p) ==
    [first p,:[(q is ["localModemap",:.] => q; copy q) for q in rest p]]
  [[d]]


--% Case patterns

up%Match t ==
  sorry '"case pattern"


--% importing domains
up%Import t ==
  t isnt [.,:types] => nil
  -- ??? shall we error in case types is nil?
  for x in types repeat
    $e := addDomain(devaluate objVal getValue x,$e)
  setValueToVoid t

--% Macro handling

-- Well, in fact we never handle macros in the interpreter directly.
-- Rather, they are saved in the `macro processing phase' (phMacro)
-- to be used in future macro expansions, and the AST we get at this
-- point already went through the macro expansion massage.  So, all we
-- have to do is to the rubber stamp.
up%Macro t ==
  setValueToVoid t

up%MLambda t ==
  setValueToVoid t


--% Sorry for unhandled input constructs
sorry kind ==
  throwKeyedMsg("S2IP0006",[kind])

--% Export
up%Export t ==
  sorry '"export declaration"

--% Inline
up%Inline t ==
  sorry '"inline declaration"

--% Category
up%With t ==
  sorry '"category definition"

--% Domain
up%Add t ==
  sorry '"domain definition"

-- Creates the function names of the special function handlers and puts
--  them on the property list of the function name

for name in $specialOps repeat
   functionName := makeSymbol strconc('up,name)
   property(name,'up) := functionName

  
