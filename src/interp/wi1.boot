-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2009, Gabriel Dos Reis.
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


import macros
namespace BOOT

-- !! do not delete the next function !

spad2AsTranslatorAutoloadOnceTrigger() == nil

--======================================================================
--    Temporary definitions---for tracing and debugging
--======================================================================
$convertingSpadFile := false
tr fn ==
  $convertingSpadFile : local := true
  $options: local := nil
  sfn  := STRINGIMAGE fn
  newname := STRCONC(sfn,'".as")
  $outStream :local := MAKE_-OUTSTREAM newname
  markSay '"#pile"
  markSay('"#include _"axiom.as_"")
  markTerpri()
  CATCH($SpadReaderTag,compiler [INTERN sfn])
  SHUT $outStream

ppFull x ==
  SETQ(_*PRINT_-LEVEL_*,nil)
  SETQ(_*PRINT_-LENGTH_*,nil)
  pp x

put(x,prop,val,e) ==
--if prop = 'mode and CONTAINED('PART,val) then foobar val
  $InteractiveMode and not EQ(e,$CategoryFrame) =>
    putIntSymTab(x,prop,val,e)
  --e must never be $CapsuleModemapFrame
  null atom x => put(first x,prop,val,e)
  newProplist:= augProplistOf(x,prop,val,e)
  prop="modemap" and $insideCapsuleFunctionIfTrue=true =>
    SAY ["**** modemap PUT on CapsuleModemapFrame: ",val]
    $CapsuleModemapFrame:=
      addBinding(x,augProplistOf(x,"modemap",val,$CapsuleModemapFrame),
        $CapsuleModemapFrame)
    e
  addBinding(x,newProplist,e)

addBinding(var,proplist,e is [[curContour,:tailContour],:tailEnv]) ==
--if CONTAINED('PART,proplist) then foobar proplist
  EQ(proplist,getProplist(var,e)) => e
  $InteractiveMode => addBindingInteractive(var,proplist,e)
  if curContour is [[ =var,:.],:.] then curContour:= rest curContour
                 --Previous line should save some space
  [[[lx,:curContour],:tailContour],:tailEnv] where lx:= [var,:proplist]


pmatch(s,p) == pmatchWithSl(s,p,"ok")

pmatchWithSl(s,p,al) ==
  s=$EmptyMode => nil
  s=p => al
  v:= assoc(p,al) => s=rest v or al
  MEMQ(p,$PatternVariableList) => [[p,:s],:al]
  null atom p and null atom s and (al':= pmatchWithSl(first s,first p,al)) and
    pmatchWithSl(rest s,rest p,al')

--======================================================================
--                    From define.boot
--======================================================================
compJoin(["Join",:argl],m,e) ==
  catList:= [(compForMode(x,$Category,e) or return 'failed).expr for x in argl]
  catList='failed => stackSemanticError(["cannot form Join of: ",argl],nil)
  catList':=
    [extract for x in catList] where
      extract() ==
        x := markKillAll x
        isCategoryForm(x,e) =>
          parameters:=
            union("append"/[getParms(y,e) for y in rest x],parameters)
              where getParms(y,e) ==
                atom y =>
                  isDomainForm(y,e) => LIST y
                  nil
                y is ['LENGTH,y'] => [y,y']
                LIST y
          x
        x is ["DomainSubstitutionMacro",pl,body] =>
          (parameters:= union(pl,parameters); body)
        x is ["mkCategory",:.] => x
        atom x and getmode(x,e)=$Category => x
        stackSemanticError(["invalid argument to Join: ",x],nil)
        x
  T:= [wrapDomainSub(parameters,["Join",:catList']),$Category,e]
  convert(T,m)


compDefineFunctor(dfOriginal,m,e,prefix,fal) ==
  df := markInsertParts dfOriginal
  $domainShell: local -- holds the category of the object being compiled
  $profileCompiler: local := true
  $profileAlist:    local := nil
  $LISPLIB => compDefineLisplib(df,m,e,prefix,fal,'compDefineFunctor1)
  compDefineFunctor1(df,m,e,prefix,fal)

compDefineLisplib(df,m,e,prefix,fal,fn) ==
  ["DEF",[op,:.],:.] := df
  --fn= compDefineCategory OR compDefineFunctor
  sayMSG fillerSpaces(72,'"-")
  $LISPLIB: local := 'T
  $op: local := op
  $lisplibAttributes: local := NIL
  $lisplibPredicates: local := NIL -- set by makePredicateBitVector
  $lisplibCategoriesExtended: local := NIL -- this is always nil. why? (tpd)
  $lisplibForm: local := NIL
  $lisplibKind: local := NIL
  $lisplibModemap: local := NIL
  $lisplibModemapAlist: local := NIL
  $lisplibSlot1 : local := NIL   -- used by NRT mechanisms
  $lisplibOperationAlist: local := NIL
  $lisplibSuperDomain: local := NIL
  $libFile: local := NIL
  $lisplibVariableAlist: local := NIL
  $lisplibRelatedDomains: local := NIL   --from ++ Related Domains: see c-doc
  $lisplibCategory: local := nil
  --for categories, is rhs of definition; otherwise, is target of functor
  --will eventually become the "constructorCategory" property in lisplib
  --set in compDefineCategory if category, otherwise in finalizeLisplib
  libName := getConstructorAbbreviation op
  $compileDocumentation => compileDocumentation libName
  sayMSG ['"   initializing ",$spadLibFT,:bright libName,
    '"for",:bright op]
  initializeLisplib libName
  sayMSG ['"   compiling into ",$spadLibFT,:bright libName]
  res:= FUNCALL(fn,df,m,e,prefix,fal)
  sayMSG ['"   finalizing ",$spadLibFT,:bright libName]
--finalizeLisplib libName
  FRESH_-LINE $algebraOutputStream
  sayMSG fillerSpaces(72,'"-")
  unloadOneConstructor(op,libName)
  res

compTopLevel(x,m,e) ==
--+ signals that target is derived from lhs-- see NRTmakeSlot1Info
  $NRTderivedTargetIfTrue: local := false
  $killOptimizeIfTrue: local:= false
  $forceAdd: local:= false
  -- The next line allows the new compiler to be tested interactively.
  compFun := 'compOrCroak
  if x is ["where",:.] then x := markWhereTran x
  def :=
    x is ["where",a,:.] => a
    x
  $originalTarget : local :=
    def is ["DEF",.,[target,:.],:.] => target
    'sorry
  x is ["DEF",:.] or x is ["where",["DEF",:.],:.] =>
    ([val,mode,.]:= FUNCALL(compFun,x,m,e); [val,mode,e])
        --keep old environment after top level function defs
  FUNCALL(compFun,x,m,e)

markWhereTran ["where",["DEF",form,sig,clist,body],:tail] ==
  items :=
    tail is [['SEQ,:l,['exit,n,x]]] => [:l,x]
    [first tail]
  [op,:argl] := form
  [target,:atypeList] := sig
  decls := [[":",a,b] for a in argl for b in atypeList | b]
--  not (and/[null x for x in atypeList]) =>
--    systemError ['"unexpected WHERE argument list: ",:atypeList]
  for x in items repeat
    x is [":",a,b] =>
      a is ['LISTOF,:r] =>
        for y in r repeat decls := [[":",y,b],:decls]
      decls := [x,:decls]
    x is [key,fn,p,q,bd] and key in '(DEF MDEF) and p='(NIL) and q='(NIL) =>
      fn = target or fn is [=target] => ttype := bd
      fn = body   or fn is [=body]   => body  := bd
      macros := [x,:macros]
    systemError ['"unexpected WHERE item: ",x]
  nargtypes := [p for arg in argl |
                  p := or/[t for d in decls | d is [.,=arg,t]] or
                    systemError ['"Missing WHERE declaration for :", arg]]
  nform := form
  ntarget := ttype or target
  ndef := ['DEF,nform,[ntarget,:nargtypes],clist,body]
  result :=
    reverse macros is [:m,e] =>
      mpart :=
        m => ['SEQ,:m,['exit,1,e]]
        e
      ['where,ndef,mpart]
    ndef
  result

compPART(u,m,e) ==
--------new------------------------------------------94/10/11
  ['PART,.,x] := u
  T := comp(x,m,e) => markAny('compPART,u, T)
  nil

xxxxx x == x

qt(n,T) ==
  null T => nil
  if null getProplist('R,T.env) then xxxxx n
  T

qe(n,e) ==
  if null getProplist('R,e) then xxxxx n
  e

comp(x,m,e) ==
  qe(7,e)
  T := qt(8,comp0(x,m,e)) => qt(9,markComp(x,T))
--T := m = "$" and comp(x,$EmptyMode,e) => coerce(T, m)
  --------------------------------------------------------94/11/10
  nil

comp0(x,m,e) ==
  qe(8,e)
--version of comp which skips the marking (see compReduce1)
  T:= compNoStacking(x,m,e) =>
    $compStack:= nil
    qt(10,T)
  $compStack:= [[x,m,e,$exitModeStack],:$compStack]
  nil

compNoStacking(xOrig,m,e) ==
  $partExpression: local := nil
  xOrig := markKillAllRecursive xOrig
-->xOrig is ['PART,n,x] => compNoStackingAux(xOrig,m,e)
----------------------------------------------------------94/10/11
  qt(11,compNoStacking0(xOrig,m,e))

markKillAllRecursive x ==
  x is [op,:r] =>
--->op = 'PART => markKillAllRecursive second r
    op = 'PART => ['PART, first r, markKillAllRecursive second r]
----------------------------------------------------------94/10/11
    constructor? op => markKillAll x
    op = 'elt and constructor? opOf first r =>
      ['elt,markKillAllRecursive first r,second r]
    x
  x

compNoStackingAux($partExpression,m,e) ==
-----------------not used---------------------94/10/11
  x := third $partExpression
  T := compNoStacking0(x,m,e) or return nil
  markParts($partExpression,T)

compNoStacking0(x,m,e) ==
  qe(1,e)
  T := compNoStacking01(x,m,qe(51,e))
  qt(52,T)

compNoStacking01(x,m,e) ==
--compNoStacking0(x,m,e) ==
  if CONTAINED('MI,m) then m := markKillAll(m)
  T:= comp2(x,m,e) =>
    (m=$EmptyMode and T.mode=IFCAR(get('Rep,'value,e)) => 
       [T.expr,"Rep",T.env]; qt(12,T))
         --$Representation is bound in compDefineFunctor, set by doIt
         --this hack says that when something is undeclared, $ is
         --preferred to the underlying representation -- RDJ 9/12/83
  T := compNoStacking1(x,m,e,$compStack)
  qt(13,T)

compNoStacking1(x,m,e,$compStack) ==
  u:= get(if m="$" then "Rep" else m,"value",e) =>
    m1 := markKillAll u.expr
--------------------> new <-------------------------
    T:= comp2(x,m1,e) => coerce(T,m)
    nil
--------------------> new <-------------------------
  nil

compWithMappingMode(x,m,oldE) ==
  ["Mapping",m',:sl] := m
  $killOptimizeIfTrue: local:= true
  e:= oldE
  x := markKillAll x
  ------------------
  m := markKillAll m
  ------------------
--if x is ['PART,.,y] then x := y
---------------------------------
  isFunctor x =>
    if get(x,"modemap",$CategoryFrame) is [[[.,target,:argModeList],.],:.] and
      (and/[extendsCategoryForm("$",s,mode) for mode in argModeList for s in sl]
        ) and extendsCategoryForm("$",target,m') then return [x,m,e]
  if STRINGP x then x:= INTERN x
  for m in sl for v in (vl:= take(#sl,$FormalMapVariableList)) repeat
    [.,.,e]:= compMakeDeclaration(v,m,e)
  not null vl and not hasFormalMapVariable(x, vl) => return
    [u,.,.] := comp([x,:vl],m',e) or return nil
    extractCodeAndConstructTriple(u, m, oldE)
  null vl and (t := comp([x], m', e)) => return
    [u,.,.] := t
    extractCodeAndConstructTriple(u, m, oldE)
  [u,.,.]:= comp(x,m',e) or return nil
  originalFun := u
  if originalFun is ['WI,a,b] then u := b
  uu := ['LAMBDA,vl,u]
  T := [uu,m,oldE]
  originalFun is ['WI,a,b] => markLambda(vl,a,m,T)
  markLambda(vl,originalFun,m,T)

compAtom(x,m,e) ==
  T:= compAtomWithModemap(x,m,e,get(x,"modemap",e)) => markCompAtom(x,T)
  x="nil" =>
    T:=
      modeIsAggregateOf('List,m,e) is [.,R]=> compList(x,['List,R],e)
      modeIsAggregateOf('Vector,m,e) is [.,R]=> compVector(x,['Vector,R],e)
    T => convert(T,m)
-->
  FIXP x and opOf m in '(Integer NonNegativeInteger PositiveInteger SmallInteger) => markAt [x,m,e]
--  FIXP x and (T := [x, $Integer,e]) and (T' := convert(T,m)) => markAt(T, T')
  t:=
    isSymbol x =>
      compSymbol(x,m,e) or return nil
    m = $Expression and primitiveType x => [x,m,e]
    STRINGP x => 
      x ~= '"failed" and (member($Symbol, $localImportStack) or
        member($Symbol, $globalImportStack)) => markAt [x, '(String), e]
      [x, x, e]
    [x,primitiveType x or return nil,e]
  convert(t,m)

extractCodeAndConstructTriple(u, m, oldE) ==
  u := markKillAll u
  u is ["call",fn,:.] =>
    if fn is ["applyFun",a] then fn := a
    [fn,m,oldE]
  [op,:.,env] := u
  [["CONS",["function",op],env],m,oldE]

compSymbol(s,m,e) ==
  s="$NoValue" => ["$NoValue",$NoValueMode,e]
  isFluid s => [s,getmode(s,e) or return nil,e]
  s="true" => ['(QUOTE T),$Boolean,e]
  s="false" => [false,$Boolean,e]
  s=m or isLiteral(s,e) => [["QUOTE",s],s,e]
  v:= get(s,"value",e) =>
--+
    MEMQ(s,$functorLocalParameters) =>
        NRTgetLocalIndex s
        [s,v.mode,e] --s will be replaced by an ELT form in beforeCompile
    [s,v.mode,e] --s has been SETQd
  m':= getmode(s,e) =>
    if not MEMQ(s,$formalArgList) and not MEMQ(s,$FormalMapVariableList) and
      not isFunction(s,e) and null ($compForModeIfTrue=true) then errorRef s
    [s,m',e] --s is a declared argument
  MEMQ(s,$FormalMapVariableList) => stackMessage ["no mode found for",s]
--->
  m = $Symbol or m = $Expression => [['QUOTE,s],m,e]
                                   ---> was ['QUOTE, s]
  not isFunction(s,e) => errorRef s

compForm(form,m,e) ==
  if form is [['PART,.,op],:r] then form := [op,:r]
  ----------------------------------------------------- 94/10/16
  T:=
    compForm1(form,m,e) or compArgumentsAndTryAgain(form,m,e) or return
      stackMessageIfNone ["cannot compile","%b",form,"%d"]
  T

compForm1(form,m,e) ==
  [op,:argl] := form
  op="error" =>
    [[op,:[([.,.,e]:=outputComp(x,e)).expr
      for x in argl]],m,e]
  op is ['MI,a,b] => compForm1([markKillExpr b,:argl],m,e)
  op is ["elt",domain,op'] =>
    domain := markKillAll domain
    domain="Lisp" =>
      --op'='QUOTE and null rest argl => [first argl,m,e]
      val := [op',:[([.,.,e]:= compOrCroak(x,$EmptyMode,e)).expr for x in argl]]
      markLisp([val,m,e],m)
-------> new <-------------
--    foobar domain
--    markImport(domain,true)
-------> new <-------------
    domain=$Expression and op'="construct" => compExpressionList(argl,m,e)
    (op'="COLLECT") and coerceable(domain,m,e) =>
      (T:= comp([op',:argl],domain,e) or return nil; coerce(T,m))
-------> new <-------------
    domain= 'Rep and
      (ans := compForm2([op',:argl],SUBST('Rep,'_$,m),e:= addDomain(domain,e),
        [SUBST('Rep,'_$,x) for x in getFormModemaps([op',:argl],e)
          | x is [[ =domain,:.],:.]])) => ans
-------> new <-------------
    ans := compForm2([op',:argl],m,e:= addDomain(domain,e),
      [x for x in getFormModemaps([op',:argl],e) | x is [[ =domain,:.],:.]]) => ans
    (op'="construct") and coerceable(domain,m,e) =>
      (T:= comp([op',:argl],domain,e) or return nil; coerce(T,m))
    nil

  e:= addDomain(m,e) --???unneccessary because of comp2's call???
  (mmList:= getFormModemaps(form,e)) and (T:= compForm2(form,m,e,mmList)) => T
  compToApply(op,argl,m,e)

--% WI and MI

compForm3(form is [op,:argl],m,e,modemapList) ==
--order modemaps so that ones from Rep are moved to the front
  modemapList := compFormOrderModemaps(modemapList,m = "$")
  qe(22,e)
  T:=
    or/
      [compFormWithModemap(form,m,e,first (mml:= ml))
        for ml in tails modemapList] or return nil
  qt(14,T)
  result := 
    $compUniquelyIfTrue =>
      or/[compFormWithModemap(form,m,e,mm) for mm in rest mml] =>
        THROW("compUniquely",nil)
      qt(15,T)
    qt(16,T)
  qt(17,markAny('compForm3,form,result))

compFormOrderModemaps(mml,targetIsDollar?) ==
--order modemaps so that ones from Rep are moved to the front
--exceptions: if $ is the target and there are 2 modemaps with
--            identical signatures, move the $ one ahead
  repMms := [mm for (mm:= [[dc,:.],:.]) in mml | dc = 'Rep]
  if repMms and targetIsDollar? then 
    dollarMms := [mm for (mm := [[dc,:sig],:.]) in mml | dc = "$"
       and or/[mm1 for (mm1:= [[dc1,:sig1],:.]) in repMms | sig1 = sig]]
    repMms := [:dollarMms, :repMms]
  null repMms => mml
  [:repMms,:SETDIFFERENCE(mml,repMms)]

compWI(["WI",a,b],m,E) ==
  u := comp(b,m,E)
  pp (u => "====> ok"; 'NO)
  u

compMI(["MI",a,b],m,E) ==
  u := comp(b,m,E)
  pp (u => "====> ok"; 'NO)
  u

compWhere([.,form,:exprList],m,eInit) ==
  $insideExpressionIfTrue: local:= false
  $insideWhereIfTrue: local:= true
--  if not $insideFunctorIfTrue then
--   $originalTarget :=
--    form is ['DEF,a,osig,:.] and osig is [otarget,:.] =>
--      exprList is [['SEQ,:l,['exit,n,y]]] and (u := [:l,y]) and
--        (ntarget := or/[def for x in u | x is [op,a',:.,def] and ([op,a',otarget]) and
--          op in '(DEF MDEF) and (a' = otarget or a' is [=otarget])]) =>
--            [ntarget,:rest osig]
--      osig
--    nil
--  foobum exprList
  e:= eInit
  u:=
    for item in exprList repeat
      [.,.,e]:= comp(item,$EmptyMode,e) or return "failed"
  u="failed" => return nil
  $insideWhereIfTrue:= false
  [x,m,eAfter]:= comp(macroExpand(form,eBefore:= e),m,e) or return nil
  eFinal:=
    del:= deltaContour(eAfter,eBefore) => addContour(del,eInit)
    eInit
  [x,m,eFinal]

compMacro(form,m,e) ==
  $macroIfTrue: local:= true
  ["MDEF",lhs,signature,specialCases,rhs]:= form := markKillAll form
  firstForm := ["MDEF",first lhs,'(NIL),'(NIL),rhs]
  markMacro(first lhs,rhs)
  if $verbose then
    rhs :=
      rhs is ['CATEGORY,:.] => ['"-- the constructor category"]
      rhs is ['Join,:.]     => ['"-- the constructor category"]
      rhs is ['CAPSULE,:.]  => ['"-- the constructor capsule"]
      rhs is ['add,:.]      => ['"-- the constructor capsule"]
      formatUnabbreviated rhs
    sayBrightly ['"   processing macro definition",'%b,
      :formatUnabbreviated lhs,'" ==> ",:rhs,'%d]
  ["MDEF",lhs,signature,specialCases,rhs]:= form:= macroExpand(form,e)
  m=$EmptyMode or m=$NoValueMode =>
    ["/throwAway",$NoValueMode,put(first lhs,"macro",rhs,e)]

--compMacro(form,m,e) ==
--  $macroIfTrue: local:= true
--  ["MDEF",lhs,signature,specialCases,rhs]:= form
--  rhs :=
--    rhs is ['CATEGORY,:.] => ['"-- the constructor category"]
--    rhs is ['Join,:.]     => ['"-- the constructor category"]
--    rhs is ['CAPSULE,:.]  => ['"-- the constructor capsule"]
--    rhs is ['add,:.]      => ['"-- the constructor capsule"]
--    formatUnabbreviated rhs
--  sayBrightly ['"   processing macro definition",'%b,
--    :formatUnabbreviated lhs,'" ==> ",:rhs,'%d]
--  ["MDEF",lhs,signature,specialCases,rhs]:= form:= macroExpand(form,e)
--  m=$EmptyMode or m=$NoValueMode =>
--    rhs := markMacro(lhs,rhs)
--    ["/throwAway",$NoValueMode,put(first lhs,"macro",rhs,e)]

compSetq(oform,m,E) ==
  ["%LET",form,val] := oform
  T := compSetq1(form,val,m,E) => markSetq(oform,T)
  nil

compSetq1(oform,val,m,E) ==
  form := markKillAll oform
  IDENTP form => setqSingle(form,val,m,E)
  form is [":",x,y] =>
    [.,.,E']:= compMakeDeclaration(x,y,E)
    compSetq(["%LET",x,val],m,E')
  form is [op,:l] =>
    op="CONS"  => setqMultiple(uncons form,val,m,E)
    op="%Comma" => setqMultiple(l,val,m,E)
    setqSetelt(oform,form,val,m,E)

setqSetelt(oform,[v,:s],val,m,E) ==
  T:= comp0(["setelt",:oform,val],m,E) or return nil
--->                  -------
  markComp(oform,T)

setqSingle(id,val,m,E) ==
  $insideSetqSingleIfTrue: local:= true
    --used for comping domain forms within functions
  currentProplist:= getProplist(id,E)
  m'':= get(id,'mode,E) or getmode(id,E) or 
       (if m=$NoValueMode then $EmptyMode else m)
-----------------------> new <-------------------------
  trialT := m'' = "$" and get("Rep",'value,E) and comp(val,'Rep,E)
-----------------------> new <-------------------------
  T:=
    (trialT and coerce(trialT,m'')) or eval or return nil where
      eval() ==
        T:= comp(val,m'',E) => T
        not get(id,"mode",E) and m'' ~= (maxm'':=maximalSuperType m'') and
           (T:=comp(val,maxm'',E)) => T
        (T:= comp(val,$EmptyMode,E)) and getmode(T.mode,E) =>
          assignError(val,T.mode,id,m'')
  T':= [x,m',e']:= convert(T,m) or return nil
  if $profileCompiler = true then
    null IDENTP id => nil
    key :=
      MEMQ(id,rest $form) => 'arguments
      'locals
    profileRecord(key,id,T.mode)
  newProplist:= consProplistOf(id,currentProplist,"value",markKillAll removeEnv T)
  e':= (CONSP id => e'; addBinding(id,newProplist,e'))
  x1 := markKillAll x
  if isDomainForm(x1,e') then
    if isDomainInScope(id,e') then
      stackWarning ["domain valued variable","%b",id,"%d",
        "has been reassigned within its scope"]
    e':= augModemapsFromDomain1(id,x1,e')
      --all we do now is to allocate a slot number for lhs
      --e.g. the LET form below will be changed by putInLocalDomainReferences
--+
  if (k:=NRTassocIndex(id))
     then
       $markFreeStack := [id,:$markFreeStack]
       form:=["setShellEntry","$",k,x]
     else form:=
         $QuickLet => ["%LET",id,x]
         ["%LET",id,x,
            (isDomainForm(x,e') => ['ELT,id,0];first outputComp(id,e'))]
  [form,m',e']

setqMultiple(nameList,val,m,e) ==
  val is ["CONS",:.] and m=$NoValueMode =>
    setqMultipleExplicit(nameList,uncons val,m,e)
  val is ["%Comma",:l] and m=$NoValueMode => setqMultipleExplicit(nameList,l,m,e)
  --1. create a gensym, %add to local environment, compile and assign rhs
  g:= genVariable()
  e:= addBinding(g,nil,e)
  T:= [.,m1,.]:= compSetq1(g,val,$EmptyMode,e) or return nil
  e:= put(g,"mode",m1,e)
  [x,m',e]:= convert(T,m) or return nil
  --1.1 exit if result is a list
  m1 is ["List",D] =>
    for y in nameList repeat e:= put(y,"value",[genSomeVariable(),D,$noEnv],e)
    convert([["PROGN",x,["%LET",nameList,g],g],m',e],m)
  --2. verify that the #nameList = number of parts of right-hand-side
  selectorModePairs:=
                                                --list of modes
    decompose(m1,#nameList,e) or return nil where
      decompose(t,length,e) ==
        t is ["Record",:l] => [[name,:mode] for [":",name,mode] in l]
        comp(t,$EmptyMode,e) is [.,["RecordCategory",:l],.] =>
          [[name,:mode] for [":",name,mode] in l]
        stackMessage ["no multiple assigns to mode: ",t]
  #nameList ~= #selectorModePairs =>
    stackMessage [val," must decompose into ",#nameList," components"]
  -- 3.generate code; return
  assignList:=
    [([.,.,e]:= compSetq1(x,["elt",g,y],z,e) or return "failed").expr
      for x in nameList for [y,:z] in selectorModePairs]
  if assignList="failed" then NIL
  else [MKPROGN [x,:assignList,g],m',e]

setqMultipleExplicit(nameList,valList,m,e) ==
  #nameList ~= #valList =>
    stackMessage ["Multiple assignment error; # of items in: ",nameList,
      "must = # in: ",valList]
  gensymList:= [genVariable() for name in nameList]
  for g in gensymList for name in nameList repeat
    e := put(g,"mode",get(name,"mode",e),e)
  assignList:=
             --should be fixed to declare genVar when possible
    [[.,.,e]:= compSetq1(g,val,$EmptyMode,e) or return "failed"
      for g in gensymList for val in valList for name in nameList]
  assignList="failed" => nil
  reAssignList:=
    [[.,.,e]:= compSetq1(name,g,$EmptyMode,e) or return "failed"
      for g in gensymList for name in nameList]
  reAssignList="failed" => nil
  T := [["PROGN",:[T.expr for T in assignList],
    :[T.expr for T in reAssignList]], $NoValueMode, (LAST reAssignList).env]
  markMultipleExplicit(nameList,valList,T)

canReturn(expr,level,exitCount,ValueFlag) ==  --SPAD: exit and friends
  atom expr => ValueFlag and level=exitCount
  (op:= first expr)="QUOTE" => ValueFlag and level=exitCount
  op in '(WI MI) => canReturn(third expr,level,count,ValueFlag)
  op="TAGGEDexit" =>
    expr is [.,count,data] => canReturn(data.expr,level,count,count=level)
  level=exitCount and not ValueFlag => nil
  op="SEQ" => or/[canReturn(u,level+1,exitCount,false) for u in rest expr]
  op="TAGGEDreturn" => nil
  op="CATCH" =>
    [.,gs,data]:= expr
    (findThrow(gs,data,level,exitCount,ValueFlag) => true) where
      findThrow(gs,expr,level,exitCount,ValueFlag) ==
        atom expr => nil
        expr is ["THROW", =gs,data] => true
            --this is pessimistic, but I know of no more accurate idea
        expr is ["SEQ",:l] =>
          or/[findThrow(gs,u,level+1,exitCount,ValueFlag) for u in l]
        or/[findThrow(gs,u,level,exitCount,ValueFlag) for u in rest expr]
    canReturn(data,level,exitCount,ValueFlag)
  op = "COND" =>
    level = exitCount =>
      or/[canReturn(last u,level,exitCount,ValueFlag) for u in rest expr]
    or/[or/[canReturn(u,level,exitCount,ValueFlag) for u in v]
                for v in rest expr]
  op="IF" =>
    expr is [.,a,b,c]
    if not canReturn(a,0,0,true) and not $convert2NewCompiler then
      SAY "IF statement can not cause consequents to be executed"
      pp expr
    canReturn(a,level,exitCount,nil) or canReturn(b,level,exitCount,ValueFlag)
      or canReturn(c,level,exitCount,ValueFlag)
  --now we have an ordinary form
  atom op => and/[canReturn(u,level,exitCount,ValueFlag) for u in expr]
  op is ["XLAM",args,bods] =>
    and/[canReturn(u,level,exitCount,ValueFlag) for u in expr]
  systemErrorHere ["canReturn",expr] --for the time being

compList(l,m is ["List",mUnder],e) ==
  markImport m
  markImport mUnder
  null l => [NIL,m,e]
  Tl:= [[.,mUnder,e]:=
    comp(x,mUnder,e) or return "failed" for i in 1.. for x in l]
  Tl="failed" => nil
  T:= [["LIST",:[T.expr for T in Tl]],["List",mUnder],e]

compVector(l,m is ["Vector",mUnder],e) ==
  markImport m
  markImport mUnder
  null l => [$EmptyVector,m,e]
  Tl:= [[.,mUnder,e]:= comp(x,mUnder,e) or return "failed" for x in l]
  Tl="failed" => nil
  [["VECTOR",:[T.expr for T in Tl]],m,e]

compColon([":",f,t],m,e) ==
  $insideExpressionIfTrue=true => compPretend(["pretend",f,t],m,e)
    --if inside an expression, ":" means to convert to m "on faith"
  f := markKillAll f
  $lhsOfColon: local:= f
  t:=
    t := markKillAll t
    atom t and (t':= ASSOC(t,getDomainsInScope e)) => t'
    isDomainForm(t,e) and not $insideCategoryIfTrue =>
      (if not member(t,getDomainsInScope e) then e:= addDomain(t,e); t)
    isDomainForm(t,e) or isCategoryForm(t,e) => t
    t is ["Mapping",m',:r] => t
    unknownTypeError t
    t
  if $insideCapsuleFunctionIfTrue then markDeclaredImport t
  f is ["LISTOF",:l] =>
    (for x in l repeat T:= [.,.,e]:= compColon([":",x,t],m,e); T)
  e:=
    f is [op,:argl] and not (t is ["Mapping",:.]) =>
      --for MPOLY--replace parameters by formal arguments: RDJ 3/83
      newTarget:= EQSUBSTLIST(take(#argl,$FormalMapVariableList),
        [(x is [":",a,m] => a; x) for x in argl],t)
      signature:=
        ["Mapping",newTarget,:
          [(x is [":",a,m] => m;
              getmode(x,e) or systemErrorHere '"compColonOld") for x in argl]]
      put(op,"mode",signature,e)
    put(f,"mode",t,e)
  if not $bootStrapMode and $insideFunctorIfTrue and
    makeCategoryForm(t,e) is [catform,e] then
        e:= put(f,"value",[genSomeVariable(),t,$noEnv],e)
  ["/throwAway",getmode(f,e),e]

compConstruct(form,m,e) == (T := compConstruct1(form,m,e)) and markConstruct(form,T) 
  
compConstruct1(form is ["construct",:l],m,e) ==
  y:= modeIsAggregateOf("List",m,e) =>
    T:= compList(l,["List",second y],e) => convert(T,m)
  y:= modeIsAggregateOf("Vector",m,e) =>
    T:= compVector(l,["Vector",second y],e) => convert(T,m)
  T:= compForm(form,m,e) => T
  for D in getDomainsInScope e repeat
    (y:=modeIsAggregateOf("List",D,e)) and
      (T:= compList(l,["List",second y],e)) and (T':= convert(T,m)) =>
         return T'
    (y:=modeIsAggregateOf("Vector",D,e)) and
      (T:= compVector(l,["Vector",second y],e)) and (T':= convert(T,m)) =>
         return T'

compPretend(u := ["pretend",x,t],m,e) ==
  t := markKillAll t
  m := markKillAll m
  e:= addDomain(t,e)
  T:= comp(x,t,e) or comp(x,$EmptyMode,e) or return nil
  if T.mode=t then warningMessage:= ["pretend",t," -- should replace by @"]
  T1:= [T.expr,t,T.env]
  t = "$" and m = "Rep" => markPretend(T1,T1)  -->! WATCH OUT: correct? !<--
  T':= coerce(T1,m) =>
    warningMessage =>
      stackWarning warningMessage
      markCompColonInside("@",T')
    markPretend(T1,T')
  nil

compAtSign(["@",x,m'],m,e) ==
  m' := markKillAll m'
  m  := markKillAll m
  e:= addDomain(m',e)
  T:= comp(x,m',e) or return nil
  coerce(T,m)

compColonInside(x,m,e,m') ==
  m' := markKillAll m'
  e:= addDomain(m',e)
  T:= comp(x,$EmptyMode,e) or return nil
  if T.mode=m' then warningMessage:= [":",m'," -- should replace by ::"]
  T:= [T.expr,m',T.env]
  m := markKillAll m
  T':= coerce(T,m) =>
    warningMessage =>
      stackWarning warningMessage
      markCompColonInside("@",T')
    stackWarning [":",m'," -- should replace by pretend"]
    markCompColonInside("pretend",T')
  nil

resolve(min, mout) ==
  din  := markKillAll min
  dout := markKillAll mout
  din=$NoValueMode or dout=$NoValueMode => $NoValueMode
  dout=$EmptyMode => din
  STRINGP din and dout = $Symbol => dout   ------> hack 8/14/94
  STRINGP dout and din = $Symbol => din    ------> hack 8/14/94
  din ~= dout and (STRINGP din or STRINGP dout) =>
    modeEqual(dout,$String) => dout
    modeEqual(din,$String) =>  nil
    mkUnion(din,dout)
  dout

coerce(T,m) ==
  T := [T.expr,markKillAll T.mode,T.env]
  m := markKillAll m
  if not isLiteral(m,T.env) then markImport m
  $InteractiveMode =>
    keyedSystemError("S2GE0016",['"coerce",
      '"function coerce called from the interpreter."])
--==================> changes <======================
--The following line is inappropriate for our needs:::
--rplac(second T,substitute("$",$Rep,second T))
  T' := coerce0(T,m) => T'
  T := [T.expr,fullSubstitute("$",$Representation,T.mode),T.env]
--==================> changes <======================
  coerce0(T,m)

coerce0(T,m) ==
  T':= coerceEasy(T,m) => T'
  T':= coerceSubset(T,m) => markCoerce(T,T','AUTOSUBSET)
  T':= coerceHard(T,m)   => markCoerce(T,T','AUTOHARD)
  T':= coerceExtraHard(T,m) => T'
  T.expr = "$fromCoerceable$" or isSomeDomainVariable m => nil
  T' := coerceRep(T,m) => markCoerce(T,T','AUTOREP)
  stackMessage fn(T.expr,T.mode,m) where
      -- if from from coerceable, this coerce was just a trial coercion
      -- from compFormWithModemap to filter through the modemaps
    fn(x,m1,m2) ==
      ["Cannot coerce","%b",x,"%d","%l","      of mode","%b",m1,"%d","%l",
        "      to mode","%b",m2,"%d"]

coerceSubset(T := [x,m,e],m') ==
  m = $SmallInteger =>
    m' = $Integer => [x,m',e]
    m' = (r := get(x,'range,e)) or isSubset(r,m',e) => [x,r,e]
    nil
--  pp [m, m']
  isSubset(m,m',e) => [x,m',e]
  -- if m is a type variable, we can't know.
  (pred:= isSubset(m',m,e)) and INTEGERP x and
     -- obviously this is temporary
    eval substitute(x,"#1",pred) => [x,m',e]
  nil

coerceRep(T,m) ==
  md := T.mode
  atom md => nil
  CONTAINED('Rep,md) and SUBST('$,'Rep,md) = m or
    CONTAINED('Rep,m) and SUBST('$,'Rep,m) = md => T
  nil

--- GET rid of XLAMs
spadCompileOrSetq form ==
        --bizarre hack to take account of the existence of "known" functions
        --good for performance (LISPLLIB size, BPI size, NILSEC)
  [nam,[lam,vl,body]] := form
  CONTAINED("",body) => sayBrightly ['"  ",:bright nam,'" not compiled"]
  if vl is [:vl',E] and body is [nam',: =vl'] then
      LAM_,EVALANDFILEACTQ ['PUT,MKQ nam,MKQ 'SPADreplace,MKQ nam']
      sayBrightly ['"     ",:bright nam,'"is replaced by",:bright nam']
  else if (ATOM body or and/[ATOM x for x in body])
         and vl is [:vl',E] and not CONTAINED(E,body) then
           macform := ['XLAM,vl',body]
           LAM_,EVALANDFILEACTQ ['PUT,MKQ nam,MKQ 'SPADreplace,MKQ macform]
           sayBrightly ['"     ",:bright nam,'"is replaced by",:bright body]
  $insideCapsuleFunctionIfTrue => first backendCompile LIST form
  compileConstructor form

coerceHard(T,m) ==
  $e: local:= T.env
  m':= T.mode
  STRINGP m' and modeEqual(m,$String) => [T.expr,m,$e]
  modeEqual(m',m) or
    (get(m',"value",$e) is [m'',:.] or getmode(m',$e) is ["Mapping",m'']) and
      modeEqual(m'',m) or
        (get(m,"value",$e) is [m'',:.] or getmode(m,$e) is ["Mapping",m'']) and
          modeEqual(m'',m') => [T.expr,m,T.env]
  STRINGP T.expr and T.expr=m => [T.expr,m,$e]
  isCategoryForm(m,$e) =>
      $bootStrapMode = true => [T.expr,m,$e]
      extendsCategoryForm(T.expr,T.mode,m) => [T.expr,m,$e]
      nil
  nil

coerceExtraHard(T is [x,m',e],m) ==
  T':= autoCoerceByModemap(T,m) => T'
  isUnionMode(m',e) is ["Union",:l] and (t:= hasType(x,e)) and
    member(t,l) and (T':= autoCoerceByModemap(T,t)) and
      (T'':= coerce(T',m)) => T''
  m' is ['Record,:.] and m = $Expression =>
      [['coerceRe2E,x,['ELT,COPY m',0]],m,e]
  nil

compCoerce(u := ["::",x,m'],m,e) ==
  m' := markKillAll m'
  e:= addDomain(m',e)
  m := markKillAll m
--------------> new code <-------------------
  T:= compCoerce1(x,m',e) => coerce(T,m)
  T := comp(x,$EmptyMode,e) or return nil
  T.mode = $SmallInteger and
    opOf m in '(NonNegativeInteger PositiveInteger) =>
      compCoerce(["::",["::",x,$Integer],m'],m,e)
--------------> new code <-------------------
  getmode(m',e) is ["Mapping",["UnionCategory",:l]] =>
    l := [markKillAll x for x in l]
    T:= (or/[compCoerce1(x,m1,e) for m1 in l]) or return nil
    coerce([T.expr,m',T.env],m)

compCoerce1(x,m',e) ==
  T:= comp(x,m',e)
  if null T then T := comp(x,$EmptyMode,e)
  null T => return nil
  m1:=
    STRINGP T.mode => $String
    T.mode
  m':=resolve(m1,m')
  T:=[T.expr,m1,T.env]
  T':= coerce(T,m') => T'
  T':= coerceByModemap(T,m') => T'
  pred:=isSubset(m',T.mode,e) =>
    gg:=GENSYM()
    pred:= substitute(gg,"#1",pred)
    code:= ['PROG1,["%LET",gg,T.expr], ['check_-subtype,pred,MKQ m',gg]]
    [code,m',T.env]

coerceByModemap([x,m,e],m') ==
--+ modified 6/27 for new runtime system
  u:=
    [modemap
      for (modemap:= [map,cexpr]) in getModemapList("coerce",1,e) | map is [.,t,
        s] and (modeEqual(t,m') or isSubset(t,m',e))
           and (modeEqual(s,m) or isSubset(m,s,e))] or return nil
  mm:=first u  -- patch for non-trival conditons
  fn := genDeltaEntry ['coerce,:mm]
  T := [["call",fn,x],m',e]
  markCoerceByModemap(x,m,m',markCallCoerce(x,m',T),nil)
 
autoCoerceByModemap([x,source,e],target) ==
  u:=
    [cexpr
      for (modemap:= [map,cexpr]) in getModemapList("autoCoerce",1,e) | map is [
        .,t,s] and modeEqual(t,target) and modeEqual(s,source)] or return nil
  fn:= (or/[selfn for [cond,selfn] in u | cond=true]) or return nil
  markCoerceByModemap(x,source,target,[["call",fn,x],target,e],true)

--======================================================================
--                    From compiler.boot
--======================================================================
--comp3x(x,m,$e) ==

comp3(x,m,$e) ==
    --returns a Triple or %else nil to signalcan't do'
  $e:= addDomain(m,$e)
  e:= $e --for debugging purposes
  m is ["Mapping",:.] => compWithMappingMode(x,m,e)
  m is ["QUOTE",a] => (x=a => [x,m,$e]; nil)
  STRINGP m => (atom x => (m=x or m=STRINGIMAGE x => [m,m,e]; nil); nil)
  null x or atom x => compAtom(x,m,e)
  op:= first x
  getmode(op,e) is ["Mapping",:ml] and (u:= applyMapping(x,m,e,ml)) => u
  op=":" => compColon(x,m,e)
  op="::" => compCoerce(x,m,e)
  not ($insideCompTypeOf=true) and stringPrefix?('"TypeOf",PNAME op) =>
    compTypeOf(x,m,e)
  ------------special jump out code for PART (don't want $insideExpressionIfTrue=true)--
  x is ['PART,:.] => compPART(x,m,e)
  ----------------------------------
  t:= qt(14,compExpression(x,m,e))
  t is [x',m',e'] and not member(m',getDomainsInScope e') =>
    qt(15,[x',m',addDomain(m',e')])
  qt(16,t)

yyyyy x == x
compExpression(x,m,e) ==
  $insideExpressionIfTrue: local:= true
  if x is ["%LET",['PART,.,w],[['elt,B,'new],['PART,.,["#",['PART,.,l]]],:.],:.] then yyyyy x 
  x := compRenameOp x
  atom first x and (fn:= GETL(first x,"SPECIAL")) =>
    FUNCALL(fn,x,m,e)
  compForm(x,m,e)

compRenameOp x ==   ----------> new 12/3/94
  x is [op,:r] and op is ['PART,.,op1] =>
    [op1,:r]
  x

compCase(["case",x,m1],m,e) ==
  m' := markKillAll m1
  e:= addDomain(m',e)
  T:= compCase1(x,m',e) => coerce(T,m)
  nil

compCase1(x,m,e) ==
  x1 :=
    x is ['PART,.,a] => a
    x
  [x',m',e']:= comp(x1,$EmptyMode,e) or return nil
  if m' = "$" then (m' := IFCAR get('Rep,'value,e)) and (switchMode := true)
  --------------------------------------------------------------------------
  m' isnt ['Union,:r] => nil
  mml := [mm for (mm := [map,cexpr]) in getModemapList("case",2,e') 
    | map is [.,.,s,t] and modeEqual(t,m) and 
         (modeEqual(s,m') or switchMode and modeEqual(s,"$"))]
        or return nil
  u := [cexpr for [.,cexpr] in mml] 
  fn:= (or/[selfn for [cond,selfn] in u | cond=true]) or return nil
  tag := genCaseTag(m, r, 1) or return nil
  x1 :=
    switchMode => markRepper('rep, x)
    x
  markCase(x, tag, markCaseWas(x1,[["call",fn,x'],$Boolean,e']))

genCaseTag(t,l,n) ==
  l is [x, :l] =>
    x = t     => 
      STRINGP x => INTERN x
      INTERN STRCONC("value", STRINGIMAGE n)
    x is ["::",=t,:.] => t
    STRINGP x => genCaseTag(t, l, n)
    genCaseTag(t, l, n + 1)
  nil

compIf(["IF",aOrig,b,c],m,E) ==
  a := markKillButIfs aOrig
  [xa,ma,Ea,Einv]:= compBoolean(a,aOrig,$Boolean,E) or return nil
  [xb,mb,Eb]:= Tb:= compFromIf(b,m,Ea) or return nil
  [xc,mc,Ec]:= Tc:= compFromIf(c,resolve(mb,m),Einv) or return nil
  xb':= coerce(Tb,mc) or return nil
  x:= ["IF",xa,xb'.expr,xc]
  (returnEnv:= Env(xb'.env,Ec,xb'.expr,xc,E)) where
    Env(bEnv,cEnv,b,c,E) ==
      canReturn(b,0,0,true) =>
        (canReturn(c,0,0,true) => intersectionEnvironment(bEnv,cEnv); bEnv)
      canReturn(c,0,0,true) => cEnv
      E
  [x,mc,returnEnv]

compBoolean(p,pWas,m,Einit) ==
  op := opOf p
  [p',m,E]:= 
    fop := LASSOC(op,'((and . compAnd) (or . compOr) (not . compNot))) =>
       APPLY(fop,[p,pWas,m,Einit]) or return nil
    T := comp(p,m,Einit) or return nil
    markAny('compBoolean,pWas,T) 
  [p',m,getSuccessEnvironment(markKillAll p,E),
        getInverseEnvironment(markKillAll p,E)]

compAnd([op,:args], pWas, m, e) ==
--called ONLY from compBoolean
  cargs := [T.expr for x in args 
              | [.,.,e,.] := T := compBoolean(x,x,$Boolean,e) or return nil]
  null cargs => nil
  coerce(markAny('compAnd,pWas,[["AND",:cargs],$Boolean,e]),m)

compOr([op,:args], pWas, m, e) ==
--called ONLY from compBoolean
  cargs := [T.expr for x in args 
              | [.,.,.,e] := T := compBoolean(x,x,$Boolean,e) or return nil]
  null cargs => nil
  coerce(markAny('compOr,pWas, [["OR",:cargs],$Boolean,e]),m)

compNot([op,arg], pWas, m, e) ==
--called ONLY from compBoolean
  [x,m1,.,ei] := compBoolean(arg,arg,$Boolean,e) or return nil
  coerce(markAny('compNot, pWas, [["NOT",x],$Boolean,ei]),m)

compDefine(form,m,e) ==
  $macroIfTrue: local
  ['DEF,.,originalSignature,.,body] := form
  if not $insideFunctorIfTrue then
    $originalBody := COPY body
  compDefine1(form,m,e)

compDefine1(form,m,e) ==
  $insideExpressionIfTrue: local:= false
  --1. decompose after macro-expanding form
  ['DEF,lhs,signature,specialCases,rhs]:= form:= macroExpand(form,e)
  $insideWhereIfTrue and isMacro(form,e) and (m=$EmptyMode or m=$NoValueMode)
     => [lhs,m,put(first lhs,"macro",rhs,e)]
  null signature.target and not MEMQ(KAR rhs,$BuiltinConstructorNames) and
    (sig:= getSignatureFromMode(lhs,e)) =>
  -- here signature of lhs is determined by a previous declaration
      compDefine1(['DEF,lhs,[first sig,:rest signature],specialCases,rhs],m,e)
  if signature.target=$Category then $insideCategoryIfTrue:= true
  if signature.target is ['Mapping,:map] then
    signature:= map
    form:= ['DEF,lhs,signature,specialCases,rhs]


-- RDJ (11/83): when argument and return types are all declared,
--  or arguments have types declared in the environment,
--  and there is no existing modemap for this signature, add
--  the modemap by a declaration, then strip off declarations and recurse
  e := compDefineAddSignature(lhs,signature,e)
-- 2. if signature list for arguments is not empty, replace ('DEF,..) by
--       ('where,('DEF,..),..) with an empty signature list;
--     otherwise, fill in all NILs in the signature
  not (and/[null x for x in rest signature]) => compDefWhereClause(form,m,e)
  signature.target=$Category =>
    compDefineCategory(form,m,e,nil,$formalArgList)
  isDomainForm(rhs,e) and not $insideFunctorIfTrue =>
    if null signature.target then signature:=
      [getTargetFromRhs(lhs,rhs,giveFormalParametersValues(rest lhs,e)),:
          rest signature]
    rhs:= addEmptyCapsuleIfNecessary(signature.target,rhs)
    compDefineFunctor(['DEF,lhs,signature,specialCases,rhs],m,e,nil,
      $formalArgList)
  null $form => stackAndThrow ['"bad == form ",form]
  newPrefix:=
    $prefix => INTERN STRCONC(encodeItem $prefix,'",",encodeItem $op)
    getAbbreviation($op,#rest $form)
  compDefineCapsuleFunction(form,m,e,newPrefix,$formalArgList)

compDefineCategory(df,m,e,prefix,fal) ==
  $domainShell: local -- holds the category of the object being compiled
  $lisplibCategory: local
  not $insideFunctorIfTrue and $LISPLIB =>
    compDefineLisplib(df,m,e,prefix,fal,'compDefineCategory1)
  compDefineCategory1(df,m,e,prefix,fal)

compDefineCategory1(df,m,e,prefix,fal) ==
  $DEFdepth     : local := 0            --for conversion to new compiler 3/93
  $capsuleStack : local := nil          --for conversion to new compiler 3/93
  $predicateStack:local := nil          --for conversion to new compiler 3/93
  $signatureStack:local := nil          --for conversion to new compiler 3/93
  $importStack  : local := nil          --for conversion to new compiler 3/93
  $globalImportStack  : local := nil    --for conversion to new compiler 3/93
  $catAddForm  : local := nil           --for conversion to new compiler 2/95
  $globalDeclareStack : local := nil
  $globalImportDefAlist: local:= nil
  $localMacroStack  : local := nil      --for conversion to new compiler 3/93
  $freeStack   : local := nil           --for conversion to new compiler 3/93
  $domainLevelVariableList: local := nil--for conversion to new compiler 3/93
  $categoryTranForm : local := nil      --for conversion to new compiler 10/93
  ['DEF,form,sig,sc,body] := df
  body := markKillAll body --these parts will be replaced by compDefineLisplib
  categoryCapsule :=
--+
    body is ['add,cat,capsule] =>
      body := cat
      capsule
    nil
  [d,m,e]:= compDefineCategory2(form,sig,sc,body,m,e,prefix,fal)
--+ next two lines
--  if $convertingSpadFile then nil
--  else
  if categoryCapsule and not $bootStrapMode then
    [.,.,e] :=
      $insideCategoryPackageIfTrue: local := true
      $categoryPredicateList: local :=
          makeCategoryPredicates(form,$lisplibCategory)
      defform := mkCategoryPackage(form,cat,categoryCapsule)
      ['DEF,[.,arg,:.],:.] := defform
      $categoryNameForDollar :local := arg
      compDefine1(defform,$EmptyMode,e)
  else
    [body,T] := $categoryTranForm
    markFinish(body,T)

  [d,m,e]

compDefineCategory2(form,signature,specialCases,body,m,e,
  $prefix,$formalArgList) ==
    --1. bind global variables
    $insideCategoryIfTrue: local:= true
    $definition: local := nil
                 --used by DomainSubstitutionFunction
    $form: local := nil
    $op: local := nil
    $extraParms: local := nil
             --Set in DomainSubstitutionFunction, used further down
--  1.1  augment e to add declaration $: <form>
    [$op,:argl]:= $definition:= form
    e:= addBinding("$",[['mode,:$definition]],e)

--  2. obtain signature
    signature':=
      [first signature,:[getArgumentModeOrMoan(a,$definition,e) for a in argl]]
    e:= giveFormalParametersValues(argl,e)

--   3. replace arguments by $1,..., substitute into body,
--     and introduce declarations into environment
    sargl:= TAKE(# argl, $TriangleVariableList)
    $functorForm:= $form:= [$op,:sargl]
    $formalArgList:= [:sargl,:$formalArgList]
    aList:= [[a,:sa] for a in argl for sa in sargl]
    formalBody:= SUBLIS(aList,body)
    signature' := SUBLIS(aList,signature')
--Begin lines for category default definitions
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $getDomainCode: local := nil
    $addForm: local:= nil
    for x in sargl for t in rest signature' repeat
      [.,.,e]:= compMakeDeclaration(x,t,e)

--   4. compile body in environment of %type declarations for arguments
    op':= $op
    -- following line causes cats with no with or Join to be fresh copies
    if opOf(formalBody) ~= 'Join and opOf(formalBody) ~= 'mkCategory then
           formalBody := ['Join, formalBody]
    T := compOrCroak(formalBody,signature'.target,e)
--------------------> new <-------------------
    $catAddForm :=
      $originalBody is ['add,y,:.] => y
      $originalBody
    $categoryTranForm := [$originalBody,[$form,['Mapping,:signature'],T.env]]
--------------------> new <-------------------
    body:= optFunctorBody markKillAll T.expr
    if $extraParms then
      formals:=actuals:=nil
      for u in $extraParms repeat
        formals:=[first u,:formals]
        actuals:=[MKQ rest u,:actuals]
      body := ['sublisV,['PAIR,['QUOTE,formals],['LIST,:actuals]],body]
    if argl then body:=  -- always subst for args after extraparms
        ['sublisV,['PAIR,['QUOTE,sargl],['LIST,:
          [['devaluate,u] for u in sargl]]],body]
    body:=
      ['PROG1,["%LET",g:= GENSYM(),body],
         ["setShellEntry",g,0,mkConstructor $functorForm]]
    fun:= compile [op',['LAM,sargl,body]]

--  5. give operator a 'modemap property
    pairlis:= [[a,:v] for a in argl for v in $FormalMapVariableList]
    parSignature:= SUBLIS(pairlis,signature')
    parForm:= SUBLIS(pairlis,form)
----    lisplibWrite('"compilerInfo",
----      ['SETQ,'$CategoryFrame,
----       ['put,['QUOTE,op'],'
----        (QUOTE isCategory),true,['addModemap,MKQ op',MKQ parForm,
----          MKQ parSignature,true,MKQ fun,'$CategoryFrame]]],$libFile)
    --Equivalent to the following two lines, we hope
    if null sargl then
      evalAndRwriteLispForm('NILADIC,
            ['MAKEPROP,['QUOTE,op'],'(QUOTE NILADIC),true])

--   6. put modemaps into InteractiveModemapFrame
    $domainShell :=
      $convertingSpadFile => nil
      eval [op',:MAPCAR('MKQ,sargl)]
    $lisplibCategory:= formalBody
----    if $LISPLIB then
----      $lisplibForm:= form
----      $lisplibKind:= 'category
----      modemap:= [[parForm,:parSignature],[true,op']]
----      $lisplibModemap:= modemap
----      $lisplibCategory:= formalBody
----      form':=[op',:sargl]
----      augLisplibModemapsFromCategory(form',formalBody,signature')
    [fun,'(Category),e]
