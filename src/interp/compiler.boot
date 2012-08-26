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


import msgdb
import pathname
import define
namespace BOOT

module compiler where
  coerce: (%Triple,%Mode) -> %Maybe %Triple
  convert: (%Triple,%Mode) -> %Maybe %Triple
  comp: (%Form,%Mode,%Env) -> %Maybe %Triple
  compOrCroak: (%Form,%Mode,%Env) -> %Maybe %Triple
  compCompilerPredicate: (%Form,%Env) -> %Maybe %Triple
  checkCallingConvention: (%List %Sig,%Short) -> %SimpleArray %Short


--% 
compUniquely: (%Form,%Mode,%Env) -> %Maybe %Triple
compNoStacking: (%Form,%Mode,%Env) -> %Maybe %Triple
compNoStacking1: (%Form,%Mode,%Env,%List %Thing) -> %Maybe %Triple
compOrCroak1: (%Form,%Mode,%Env) -> %Maybe %Triple
comp2: (%Form,%Mode,%Env) -> %Maybe %Triple
comp3: (%Form,%Mode,%Env) -> %Maybe %Triple
compExpression: (%Form,%Mode,%Env) -> %Maybe %Triple
compAtom: (%Form,%Mode,%Env) -> %Maybe %Triple
compSymbol: (%Form,%Mode,%Env) -> %Maybe %Triple
compForm: (%Form,%Mode,%Env) -> %Maybe %Triple
compForm1: (%Form,%Mode,%Env) -> %Maybe %Triple
compForm2: (%Form,%Mode,%Env,%List %Modemap) -> %Maybe %Triple
compForm3: (%Form,%Mode,%Env,%List %Modemap) -> %Maybe %Triple
compArgumentsAndTryAgain: (%Form,%Mode,%Env) -> %Maybe %Triple
compWithMappingMode: (%Form,%Mode,%Env) -> %Maybe %Triple
compFormMatch: (%Modemap,%List %Mode) -> %Boolean
compFormWithModemap: (%Form,%Mode,%Env,%Modemap) -> %Maybe %Triple
compToApply: (%Form,%List %Form,%Mode,%Env) -> %Maybe %Triple
compApplication: (%Form,%List %Form,%Mode,%Triple) -> %Maybe %Triple

primitiveType: %Form -> %Mode
modeEqual: (%Form,%Form) -> %Boolean
hasUniqueCaseView: (%Form,%Mode,%Env) -> %Boolean
convertOrCroak: (%Triple,%Mode) -> %Maybe %Triple
getFormModemaps: (%Form,%Env) -> %List %Modemap
reshapeArgumentList: (%Form,%Sig) -> %Form
applyMapping: (%Form,%Mode,%Env,%List %Mode) -> %Maybe %Triple


++ A list of routines for diagnostic reports.  These functions, in an
++ abstract sense, have type: forall T: Type . String -> T, so they
++ can be used in T-returning functions, for any T.  
$coreDiagnosticFunctions == 
  '(error userError systemError)

$IOFormDomains == 
  [$InputForm,$OutputForm,$Syntax]

--%

compTopLevel: (%Form,%Mode,%Env) -> %Maybe %Triple
compTopLevel(x,m,e) ==
  -- signals that target is derived from lhs-- see NRTmakeSlot1Info
  $NRTderivedTargetIfTrue: local := false
  $forceAdd: local:= false
  -- start with a base list of domains we may want to inline.
  $optimizableConstructorNames: local := $SystemInlinableConstructorNames
  x is ["DEF",:.] or x is ["where",["DEF",:.],:.] =>
    ([val,mode,.]:= compOrCroak(x,m,e); [val,mode,e])
        --keep old environment after top level function defs
  compOrCroak(x,m,e)


++ True if no ambiguity is allowed in overload resolution.
$compUniquelyIfTrue := false

compUniquely(x,m,e) ==
  $compUniquelyIfTrue: local:= true
  CATCH("compUniquely",comp(x,m,e))

compOrCroak(x,m,e) == 
  compOrCroak1(x,m,e)

compOrCroak1(x,m,e) ==
  fn(x,m,e,nil,nil) where
    fn(x,m,e,$compStack,$compErrorMessageStack) ==
      T:= CATCH("compOrCroak",comp(x,m,e)) => T
      --stackAndThrow here and moan in UT LISP K does the appropriate THROW
      $compStack:= [[x,m,e,$exitModeStack],:$compStack]
      $s: local :=
        compactify $compStack where
          compactify al ==
            null al => nil
            LASSOC(first first al,rest al) => compactify rest al
            [first al,:compactify rest al]
      $level: local := #$s
      errorMessage:=
        $compErrorMessageStack ~= nil => first $compErrorMessageStack
        "unspecified error"
      $scanIfTrue =>
        stackSemanticError(errorMessage,mkErrorExpr $level)
        ["failedCompilation",m,e]
      displaySemanticErrors()
      SAY("****** comp fails at level ",$level," with expression: ******")
      displayComp $level
      userError errorMessage

++ The form `x' is intended to be evaluated by the compiler, e.g. in
++ toplevel conditional definition or as sub-domain predicate.  
++ Normalize operators and compile the form.
compCompilerPredicate(x,e) ==
  $normalizeTree: local := true
  compOrCroak(parseTran x, $Boolean, e)

comp(x,m,e) ==
  T:= compNoStacking(x,m,e) => ($compStack:= nil; T)
  $compStack:= [[x,m,e,$exitModeStack],:$compStack]
  nil

compNoStacking(x,m,e) ==
  T:= comp2(x,m,e) =>
    $useRepresentationHack and m=$EmptyMode and T.mode=$Representation => 
      [T.expr,"$",T.env]
    T
    --$Representation is bound in compDefineFunctor, set by doIt
    --this hack says that when something is undeclared, $ is
    --preferred to the underlying representation -- RDJ 9/12/83
    --Now that `per' and `rep' are built in, we use the above
    --hack only when `Rep' is defined the old way. -- gdr 2008/01/26
  compNoStacking1(x,m,e,$compStack)

compNoStacking1(x,m,e,$compStack) ==
  u:= get(RepIfRepHack m,"value",e) =>
    (T:= comp2(x,u.expr,e) => [T.expr,m,T.env]; nil)
  nil

comp2(x,m,e) ==
  [y,m',e]:= comp3(x,m,e) or return nil
  --if cons? y and isDomainForm(y,e) then e := addDomain(x,e)
        --line commented out to prevent adding derived domain forms
  m~=m' and ($bootStrapMode or isDomainForm(m',e))=>[y,m',addDomain(m',e)]
        --isDomainForm test needed to prevent error while compiling Ring
        --$bootStrapMode-test necessary for compiling Ring in $bootStrapMode
  [y,m',e]

comp3(x,m,$e) ==
  --returns a Triple or %else nil to signalcan't do'
  $e:= addDomain(m,$e)
  e:= $e --for debugging purposes
  m is ["Mapping",:.] => compWithMappingMode(x,m,e)
  m is ['QUOTE,a] => (x=a => [x,m,$e]; nil)
  string? m => (x isnt [.,:.] => (m=x or m=STRINGIMAGE x => [m,m,e]; nil); nil)
  -- In quasiquote mode, x should match exactly
  (y := isQuasiquote m) =>
     y = x => [quote x, m, $e]
     nil
  x isnt [.,:.] => compAtom(x,m,e)
  op:= x.op
  ident? op and getXmode(op,e) is ["Mapping",:ml]
    and (T := applyMapping(x,m,e,ml)) => T
  op is ":" => compColon(x,m,e)
  op is "::" => compCoerce(x,m,e)
  t:= compExpression(x,m,e)
  t is [x',m',e'] and not listMember?(m',getDomainsInScope e') =>
    [x',m',addDomain(m',e')]
  t

++ We just determined that `op' is called with argument list `args', where
++  `op' is either a local capsule function, or an external function 
++ with a local signature-import declaration.  Emit insn for the call.
emitLocalCallInsn: (%Symbol,%List %Code,%Env) -> %Code
emitLocalCallInsn(op,args,e) ==
  op' :=          -- Find out the linkage name for `op'.
    get(op,"%Link",e) or encodeLocalFunctionName op
  get(op,"%Lang",e) =>  -- non-Spad calling convention
    ['%call,['%external,op'],:args]
  ['%call,['%closure,['%function,op'],'$],:args]   

applyMapping([op,:argl],m,e,ml) ==
  #argl ~= #ml-1 => nil
  isCategoryForm(first ml,e) => --is op a functor?
    pairlis := pairList($FormalMapVariableList,argl)
    ml' := applySubst(pairlis,ml)
    argl' :=
      [T.expr for x in argl for m' in rest ml'] where
        T() == [.,.,e]:= comp(x,m',e) or return "failed"
    argl' is "failed"  => nil
    form :=
      ident? op and symbolMember?(op,$formalArgList) =>
        -- this domain form is given by a general function application
        ['%funcall,op,:argl'] -- constructor call linkage is special
      [op,:argl']
    convert([form,first ml',e],m)
  argl':=
    [T.expr for x in argl for m' in rest ml] where
      T() == [.,.,e]:= comp(x,m',e) or return "failed"
  if argl' is "failed" then return nil
  form:=
    symbol? op and not symbolMember?(op,$formalArgList)
      and (u := get(op,"value",e)) = nil => emitLocalCallInsn(op,argl',e)
    -- Compiler synthetized operators are inline.
    u ~= nil and u.expr is ["XLAM",:.] => ['%call,u.expr,:argl']
    ['%call,['%apply,op],:argl']
  pairlis := pairList($FormalMapVariableList,argl')
  convert([form,applySubst(pairlis,first ml),e],m)

hasFormalMapVariable(x, vl) ==
  $formalMapVariables: local := vl
  null vl => false
  ScanOrPairVec(function hasone?,x) where
     hasone? x == symbolMember?(x,$formalMapVariables)


++ Return the usage list of free variables in a lambda expresion.
++ The usage list is an a-list (name, number of timed used.)
freeVarUsage([.,vars,body],env) ==
  freeList(body,vars,nil,env) where
    freeList(u,bound,free,e) ==
      u isnt [.,:.] =>
        not ident? u => free
        symbolMember?(u,bound) => free
        v := objectAssoc(u,free) =>
          v.rest := 1 + rest v
          free
        getmode(u,e) = nil => free
        [[u,:1],:free]
      atomic? u => free
      op := u.op
      op in '(GO %external %function function) => free
      op in '(LAMBDA %lambda) =>
        bound := setUnion(u.absParms,bound)
        for v in CDDR u repeat
          free := freeList(v,bound,free,e)
        free
      op = 'PROG =>
        bound := setUnion(bound, second u)
        for v in CDDR u | cons? v repeat
          free := freeList(v,bound,free,e)
        free
      op = '%bind =>
        for [v,init] in second u repeat
          bound := [v,:bound]
          free := freeList(init,bound,free,e)
        freeList(third u,bound,free,e)
      op = 'LET =>
        locals := nil
        for [v,init] in second u repeat
          free := freeList(init,bound,free,e)
          locals := [v,:locals]
        freeList(third u,setUnion(locals,bound),free,e)
      op = '%seq =>
        for v in rest u | cons? v repeat
          free := freeList(v,bound,free,e)
        free
      op in '(COND %when) =>
        for v in rest u repeat
          for vv in v repeat
            free := freeList(vv,bound,free,e)
        free
      if op isnt [.,:.] then --Atomic functions aren't descended
        u := rest u
      for v in u repeat
        free := freeList(v,bound,free,e)
      free

++ Finish processing a lambda expression with parameter list `vars',
++ and `env' as the environement after the compilation its body. 
finishLambdaExpression(expr is ['%lambda,vars,.],env) ==
  $FUNNAME: local := nil
  $FUNNAME__TAIL: local := [nil]
  expandedFunction := transformToBackendCode expr
  frees := freeVarUsage(expandedFunction,env)
  vec := nil          -- mini-vector
  expandedFunction :=
    frees = nil => ["LAMBDA",[:vars,"$$"], :CDDR expandedFunction]
    -- At this point, we have a function that we would like to pass.
    -- Unfortunately, it makes various free variable references outside
    -- itself.  So we build a mini-vector that contains them all, and
    -- pass this as the environment to our inner function.
    -- One free can go by itself, more than one needs a vector.
    frees is [[var,:.]] =>
      vec := var
      ["LAMBDA",[:vars,var],:CDDR expandedFunction]
    scode := nil   -- list of multiple used variables, need local bindings.
    slist := nil   -- list of single used variables, no local bindings.
    for v in frees for i in 0.. repeat
      val := ['%vref,"$$",i]
      vec := [first v,:vec]
      rest v = 1 => slist := [[first v,:val],:slist]
      scode := [[first v,val],:scode]
    body :=
      slist => applySubstNQ(slist,CDDR expandedFunction)
      CDDR expandedFunction
    if scode ~= nil then
      body := [['%bind,reverse! scode,:body]]
    vec := ['%vector,:reverse! vec]
    ["LAMBDA",[:vars,"$$"],:body]
  fname := ["CLOSEDFN",expandedFunction] --Like QUOTE, but gets compiled
  ['%closure,fname,vec]

compWithMappingMode(x,m is ["Mapping",m',:sl],oldE) ==
  e := oldE
  isFunctor x =>
    if get(x,"modemap",$CategoryFrame) is [[[.,target,:argModeList],.],:.] and
      (and/[extendsCategoryForm("$",s,mode) for mode in argModeList for s in sl]
        ) and extendsCategoryForm("$",target,m') then
            return [['%function,x],m,e]
  x is ["+->",:.] => compLambda(x,m,oldE)
  if string? x then x := makeSymbol x
  for m in sl for v in (vl:= take(#sl,$FormalMapVariableList)) repeat
    [.,.,e]:= compMakeDeclaration(v,m,e)
  (vl ~= nil) and not hasFormalMapVariable(x, vl) =>
    [u,.,.] := comp([x,:vl],m',e) or return nil
    [extractCode(u,vl),m,oldE]
  null vl and (t := comp([x], m', e)) =>
    [u,.,.] := t
    [extractCode(u,nil),m,oldE]
  [u,.,.]:= comp(x,m',e) or return nil
  [.,fun] := optimizeFunctionDef [nil,['%lambda,vl,u]]
  [finishLambdaExpression(fun,e),m,oldE]

extractCode(u,vars) ==
  u is ['%call,[q,:etc],: =vars] and q in '(ELT CONST) => ['%tref,:etc]
  u is ['%call,['%apply,a],: =vars] => a
  u is ['%call,['%closure,:.],: =vars] => first u.args
  ['%closure,['%function,['%lambda,[:vars,'$],u]],'$]

compExpression(x,m,e) ==
  $insideExpressionIfTrue: local:= true
  -- special forms have dedicated compilers.
  (op := x.op) and ident? op and (fn := property(op,'SPECIAL)) =>
    FUNCALL(fn,x,m,e)
  compForm(x,m,e)

++ Subroutine of compAtomWithModemap.
++ `Ts' is list of (at least 2) triples.  Return the one with most
++ specific mode.  Otherwise, return nil.
mostSpecificTriple(Ts,e) ==
  [T,:Ts] := Ts
  and/[T := lesser(T,T',e) for T' in Ts] where
    lesser(t,t',e) ==
      isSubset(t.mode,t'.mode,e) => t
      isSubset(t'.mode,t.mode,e) => t'
      nil

++ Elaborate use of an overloaded constant.
compAtomWithModemap: (%Symbol,%Mode,%Env,%List %Modemap) -> %Maybe %Triple
compAtomWithModemap(x,m,e,mmList) ==
  mmList := [mm for mm in mmList | mm.mmImplementation is ['CONST,:.]]
  mmList = nil => nil
  name := -- constant name displayed in diagnostics.
    externalName x  -- FIXME: Remove when the parser is fixed.

  -- Try constants with exact type matches, first.
  Ts := [[['%call,first y],mm.mmTarget,e] for mm in mmList |
             mm.mmTarget = m and
               (y := compViableModemap(x,nil,mm,e))]
  Ts is [T] => T            -- Only one possibility, take it.
  Ts ~= nil =>              -- Ambiguous constant.
    stackMessage('"Too many (%1b) constants named %2b with type %3pb",
                  [#Ts,name,m])
  -- Fallback to constants that are coercible to the target.
  Ts := [[['%call,first y],mm.mmTarget,nil] for mm in mmList |
             coerceable(mm.mmTarget,m,e) and
               (y := compViableModemap(x,nil,mm,e))]
  Ts = nil =>
    stackMessage('"No viable constant named %1b in %2pb context",[name,m])
  Ts is [T] or (T := mostSpecificTriple(Ts,e)) =>
    coerce([T.expr,T.mode,e],m)
  stackMessage('"Ambiguous constant %1b in %2pb constext.  Candidates are %3f",
    [name,m,[function formatConstantCandidates,name,Ts]])

++ Format constants named `op' with mode given in the list of triples `Ts'.
formatConstantCandidates(op,Ts) ==
  displayAmbiguousSignatures(op,[[T.mode,'constant] for T in Ts])

++ Attempt to elaborate the integer literal `x' as an exported operator
++ in the type context `m' and assumption environment `e'.
compIntegerLiteral(x,m,e) ==
  x := internalName x
  compAtomWithModemap(x,m,e,get(x,'modemap,e))

compAtom(x,m,e) ==
  x is "break" => compBreak(x,m,e)
  x is "iterate" => compIterate(x,m,e)
  T := ident? x and compAtomWithModemap(x,m,e,get(x,"modemap",e)) => T
  T := integer? x and x > 1 and compIntegerLiteral(x,m,e) => T
  t :=
    ident? x => compSymbol(x,m,e) or return nil
    listMember?(m,$IOFormDomains) and primitiveType x => [x,m,e]
    string? x => [x,x,e]
    [x,primitiveType x or return nil,e]
  convert(t,m)

primitiveType x ==
  x is nil => $EmptyMode
  string? x => $String
  integer? x =>
    x = 0 => $NonNegativeInteger
    x > 0 => $PositiveInteger
    $Integer
  float? x => $DoubleFloat
  nil

compSymbol(s,m,e) ==
  s is "$NoValue" => ["$NoValue",$NoValueMode,e]
  isFluid s => [s,getmode(s,e) or return nil,e]
  sameObject?(s,m) or isLiteral(s,e) => [quote s,s,e]
  v := get(s,"value",e) =>
    symbolMember?(s,$functorLocalParameters) =>
        getLocalIndex(constructorDB currentConstructor e,s)
        [s,v.mode,e] --s will be replaced by an ELT form in beforeCompile

    [s,v.mode,e] --s has been SETQd
  m' := getmode(s,e) =>
    if not symbolMember?(s,$formalArgList) and not symbolMember?(s,$FormalMapVariableList) and
      not isFunction(s,e) and not $compForModeIfTrue then errorRef s
    [s,m',e] --s is a declared argument
  symbolMember?(s,$FormalMapVariableList) => 
    stackMessage('"no mode found for %1b",[s])
  listMember?(m,$IOFormDomains) or member(m,[$Identifier,$Symbol]) =>
    [quote s,m,e]
  not isFunction(s,e) => errorRef s

++ Return true if `m' is the most recent unique type case assumption 
++ on `x' that predates its declaration in environment `e'.
hasUniqueCaseView(x,m,e) ==
  props := getProplist(x,e)
  for [p,:v] in props repeat
    p is "condition" and v is [["case",.,t],:.] => return modeEqual(t,m)
    p is "value" => return false


convertOrCroak(T,m) ==
  u := convert(T,m) => u
  userError ['"CANNOT CONVERT: ",T.expr,"%l",'" OF MODE: ",T.mode,"%l",
    '" TO MODE: ",m,"%l"]

convert(T,m) ==
  coerce(T,resolve(T.mode,m) or return nil)

mkUnion(a,b) ==
  b is "$" and $Rep is ["Union",:l] => b
  a is ["Union",:l] =>
    b is ["Union",:l'] => ["Union",:union(l,l')]
    ["Union",:union([b],l)]
  b is ["Union",:l] => ["Union",:union([a],l)]
  ["Union",a,b]

hasType(x,e) ==
  fn get(x,"condition",e) where
    fn x ==
      x = nil => nil
      x is [["case",.,y],:.] => y
      fn rest x

--% General Forms

compForm(form,m,e) ==
  T :=
    compForm1(form,m,e) or compArgumentsAndTryAgain(form,m,e) or return
      stackMessageIfNone ["cannot compile","%b",form,"%d"]
  T

compArgumentsAndTryAgain(form is [.,:argl],m,e) ==
  -- used in case: f(g(x)) where f is in domain introduced by
  -- comping g, e.g. for (ELT (ELT x a) b), environment can have no
  -- modemap with selector b
  form is ["elt",a,.] =>
    ([.,.,e]:= comp(a,$EmptyMode,e) or return nil; compForm1(form,m,e))
  +/[(e := T.env; 1) for x in argl | T := comp(x,$EmptyMode,e)] = 0 => nil
  compForm1(form,m,e)

outputComp(x,e) ==
  u:=comp(['_:_:,x,$OutputForm],$OutputForm,e) => u
  x is ['construct,:argl] =>
    [['%list,:[([.,.,e]:=outputComp(x,e)).expr for x in argl]],$OutputForm,e]
  (v:= get(x,"value",e)) and (v.mode is ['Union,:l]) =>
    [['coerceUn2E,x,v.mode],$OutputForm,e]
  [x,$OutputForm,e]

compForm1(form is [op,:argl],m,e) ==
  symbolMember?(op,$coreDiagnosticFunctions) =>
    [[op,:[([.,.,e]:=outputComp(x,e)).expr for x in argl]],m,e]
  op is ["elt",domain,op'] =>
    domain="Lisp" =>
      [[op',:[([.,.,e]:= compOrCroak(x,$EmptyMode,e)).expr for x in argl]],m,e]
    domain is ["Foreign",lang] => compForeignPackageCall(lang,op',argl,m,e)
    (op'="COLLECT") and coerceable(domain,m,e) =>
      (T:= comp([op',:argl],domain,e) or return nil; coerce(T,m))
    -- Next clause added JHD 8/Feb/94: the clause after doesn't work
    -- since addDomain refuses to add modemaps from Mapping
    (domain is ['Mapping,:.]) and
      (ans := compForm2([op',:argl],m,e:= augModemapsFromDomain1(domain,domain,e),
        [x for x in getFormModemaps([op',:argl],e) | x.mmDC = domain]))             => ans
    ans := compForm2([op',:argl],m,e:= addDomain(domain,e),
      [x for x in getFormModemaps([op',:argl],e) | x.mmDC = domain])             => ans
    (op'="construct") and coerceable(domain,m,e) =>
      (T:= comp([op',:argl],domain,e) or return nil; coerce(T,m))
    nil

  T := compForm2(form,m,e,getFormModemaps(form,e)) => T
  --FIXME: remove next line when the parser is fixed.
  form = $Zero or form = $One => nil
  compToApply(op,argl,m,e)

compForm2(form is [op,:argl],m,e,modemapList) ==
  modemapList = nil => nil
  aList := pairList($TriangleVariableList,argl)
  modemapList := applySubst(aList,modemapList)
  -- The calling convention vector is used to determine when it is
  -- appropriate to infer type by compiling the argument vs. just
  -- looking up the parameter type for flag arguments.
  cc := checkCallingConvention([mm.mmSignature for mm in modemapList], #argl)
  Tl :=
    [[.,.,e] := T for x in argl for i in 0..
       while (T := inferMode(x,cc.i > 0,e))] where
      inferMode(x,flag,e) ==
        flag => [x,quasiquote x,e]
        isSimple x => compUniquely(x,$EmptyMode,e)
        nil

  or/[x for x in Tl] =>
    partialModeList := [(x => x.mode; nil) for x in Tl]
    compFormPartiallyBottomUp(form,m,e,modemapList,partialModeList) or
      compForm3(form,m,e,modemapList)
  compForm3(form,m,e,modemapList)

++ We are about to compile a call.  Returns true if each argument
++ partially matches (as could be determined by type inference) the
++ corresponding expected type in the callee's modemap.
compFormMatch(mm,partialModeList) == main where
  main() ==
    match(mm.mmSource,partialModeList) 
      or wantArgumentsAsTuple(partialModeList,mm.mmSource)
  match(a,b) ==
    b = nil => true
    first b = nil => match(rest a,rest b)
    first a=first b and match(rest a,rest b)

compFormPartiallyBottomUp(form,m,e,modemapList,partialModeList) ==
  mmList := [mm for mm in modemapList | compFormMatch(mm,partialModeList)] =>
    compForm3(form,m,e,mmList)
  nil

compForm3(form is [op,:argl],m,e,modemapList) ==
  T :=
    or/
      [compFormWithModemap(form,m,e,first (mml:= ml))
        for ml in tails modemapList]
  $compUniquelyIfTrue =>
    or/[compFormWithModemap(form,m,e,mm) for mm in rest mml] =>
      THROW("compUniquely",nil)
    T
  T


compFormWithModemap(form,m,e,modemap) ==
  [map:= [.,target,:sig],[pred,impl]]:= modemap
  [op,:argl] := form := reshapeArgumentList(form,sig)
  if isCategoryForm(target,e) and isFunctor op then
    [modemap,e] := evaluateConstructorModemap(argl,modemap,e) or return nil
    [map:=[.,target,:.],:cexpr] := modemap
  sv := listOfSharpVars map
  if sv ~= nil then
     -- SAY [ "compiling ", op, " in compFormWithModemap,
     -- mode= ",map," sharp vars=",sv]
    for x in argl for ss in $FormalMapVariableList repeat
      if symbolMember?(ss,sv) then
        [map:= [.,target,:.],:cexpr]:= modemap :=SUBST(x,ss,modemap)
        -- SAY ["new map is",map]
  not coerceable(target,m,e) => nil
  [f,Tl] := compApplyModemap(form,modemap,e) or return nil

  --generate code; return
  T :=
    [x',target,e'] where
      x':=
        form' := [f,:[t.expr for t in Tl]]
        target = $Category or isCategoryForm(target,e) =>
          -- Constructor instantiations are direct calls
          ident? f and constructorDB f ~= nil => form'
          -- Otherwise, this is an indirect call
          ['%call,:form']
        -- try to deal with new-style Unions where we know the conditions
        op = "elt" and f is ['XLAM,:.] and ident?(z := first argl) and
          (c := get(z,'condition,e)) and
            c is [["case",=z,c1]] and
              (c1 is [":",=(second argl),=m] or sameObject?(c1,second argl) ) =>
                -- first is a full tag, as placed by getInverseEnvironment
                -- second is what getSuccessEnvironment will place there
                ['%tail,z]
        ['%call,:form']
      e':=
        Tl ~= nil => last(Tl).env
        e
  convert(T,m)

++ Returns the list of candidate modemaps for a form.  A modemap
++ is candidate for a form if its signature has the same number
++ of paramter types as arguments supplied to the form.  A special
++ case is made for a modemap whose sole parameter type is a Tuple.
++ In that case, it matches any number of supplied arguments.
getFormModemaps(form is [op,:argl],e) ==
  op is ["elt",domain,op1] and isDomainForm(domain,e) =>
    [x for x in getFormModemaps([op1,:argl],e) | x.mmDC = domain]
  op is [.,:.] => nil
  modemapList := get(op,"modemap",e)
  -- Within default implementations, modemaps cannot mention the
  -- current domain. 
  if $insideCategoryPackageIfTrue then
    modemapList := [x for x in modemapList | x.mmDC isnt '$]
  if form is ["elt",.,f] then
    modemapList := eltModemapFilter(f,modemapList,e) or return nil
  else if form is ["setelt",.,f,.] then
    modemapList := seteltModemapFilter(f,modemapList,e) or return nil
  nargs := #argl
  finalModemapList:= [mm for mm in modemapList 
                       | enoughArguments(argl,mm.mmSource)]
  modemapList and null finalModemapList =>
    stackMessage('"no modemap for %1b with %2 arguments", [op,nargs])
  finalModemapList

++ We are either compiling a function call, or trying to determine
++ whether we know something about a function being defined with
++ parameters are not declared in the definition.  `sigs' is the list of
++ candidate signatures for `nargs' arguments or parameters.  We need
++ to detemine whether any of the arguments are flags.  If any
++ operation takes a flag argument, then all other overloads must have
++ the same arity and must take flag argument in the same position.
++ Returns a vector of length `nargs' with positive entries indicating
++ flag arguments, and negative entries for normal argument passing.
checkCallingConvention(sigs,nargs) ==
  v := makeFilledSimpleArray("%Short",nargs,0)
  for sig in sigs repeat
    for t in rest sig 
      for i in 0.. repeat
         isQuasiquote t => 
           arrayRef(v,i) < 0 =>
             userError '"flag argument restriction violation"
           arrayRef(v,i) := arrayRef(v,i) + 1
         arrayRef(v,i) > 0 => userError '"flag argument restriction violation"
         arrayRef(v,i) := arrayRef(v,i) - 1
  v


eltModemapFilter(name,mmList,e) ==
  isConstantId(name,e) =>
    l:= [mm for mm in mmList | second mm.mmSource = name] => l
            --there are elts with extra parameters
    stackMessage('"selector variable: %1b is undeclared and unbound",[name])
    nil
  mmList

seteltModemapFilter(name,mmList,e) ==
  isConstantId(name,e) =>
    l:= [mm for mm in mmList | second mm.mmSource = name] => l
            --there are setelts with extra parameters
    stackMessage('"selector variable: %1b is undeclared and unbound",[name])
    nil
  mmList

compApplication(op,argl,m,T) ==
  e := T.env
  T.mode is ['Mapping, retm, :argml] =>
    #argl ~= #argml => nil
    retm := resolve(m, retm)
    retm = $Category or isCategoryForm(retm,e) => nil  -- not handled
    argTl := [[.,.,e] := comp(x,m,e) or return "failed"
              for x in argl for m in argml]
    argTl = "failed" => nil
    form:=
      args := [a.expr for a in argTl]
      ident? T.expr and 
        not (symbolMember?(op,$formalArgList) or symbolMember?(T.expr,$formalArgList)) and
          null get(T.expr,"value",e) =>
            emitLocalCallInsn(T.expr,args,e)
      ['%call,['%apply,T.expr],:args]
    coerce([form, retm, e],resolve(retm,m))
  op is 'elt => nil
  eltForm := ['elt, op, :argl]
  comp(eltForm, m, e)

compToApply(op,argl,m,e) ==
  T := compNoStacking(op,$EmptyMode,e) or return nil
  T.expr is ['QUOTE, =T.mode] => nil
  compApplication(op,argl,m,T)

++ `form' is a call to a operation described by the signature `sig'.
++ Massage the call so that homogeneous variable length argument lists
++ are properly tuplified.
reshapeArgumentList(form,sig) ==
  [op,:args] := form
  wantArgumentsAsTuple(args,sig) => [op,["%Comma",:args]]
  form

++ Attempt to find values for queries variables `vars' so that
++ the category expression `x' equals the category expression `p'.
solveEquation(x,p,sl,vars) ==
  ident? p and symbolMember?(p,vars) =>
    z := symbolTarget(p,sl) =>
      x = z => sl
      'failed
    [[p,:x],:sl]
  x isnt [.,:.] or p isnt [.,:.] =>
    x = p => sl
    'failed
  symbolEq?(x.op,p.op) =>
    #x.args ~= #p.args => 'failed
    x.args = nil => sl
    and/[sl := solveEquation(x',p',sl,vars)
           for x' in x.args for p' in p.args
             | sl isnt 'failed or leave 'failed]
  'failed

++ Attempt to find values for queries variables `vars' so that
++ the category expression `x' subsumes the category expression `p'.
solveSubsumption(x,p,sl,vars,typings,e) ==
  x isnt [.,:.] or p isnt [.,:.] => solveEquation(x,p,sl,vars)
  p = $Type => sl
  symbolEq?(x.op,p.op) => solveEquation(x,p,sl,vars)
  x.op is 'Join =>
    x.args = nil => 'failed
    or/[sl' := solveSubsumption(x',p,sl,vars,typings,e) for x' in x.args
          | sl' isnt 'failed] or 'failed
  x is ['CATEGORY,.,:xs] =>
    or/[sl' := solveSubsumption(x',p,sl,vars,typings,e) for x' in xs
          | sl' isnt 'failed] or 'failed
  x.op in '(SIGNATURE ATTRIBUTE) => 'failed
  getConstructorKind x.op isnt 'category => 'failed  --FIXME: for now.
  x := applySubst(constructSubst x,getConstructorCategory x.op)
  solveSubsumption(x,p,sl,vars,typings,e)

++ Subroutine of bindPredicateExistentials, with similar semantics.
++ `vars' is the list of quantified variables, and `conds' is a
++ of conditions the conjunction of which makes the whole predicate.
deduceImplicitArguments(vars,conds,e) ==
  eqs := nil       -- equation constraints
  typings := nil   -- typing constraints
  sl := nil
  for c in conds while sl isnt 'failed repeat
    c is ['ofCategory,x,y] =>  -- subsumption constraint
      ident? x and symbolMember?(x,vars) =>
         typings := [[x,:y],:typings]
      eqs := [[x,:y],:eqs]
    c is ['ofType,x,y] =>      -- exact type constraints
      T := comp(x,$EmptyMode,e)
      T = nil => sl := 'failed
      sl := solveEquation(T.mode,y,sl,vars)
  for [x,:y] in eqs while sl isnt 'failed repeat
    cat :=
      x isnt [.,:.] => getXmode(x,e)
      applySubst(constructSubst x,getConstructorCategory x.op)
    sl := solveSubsumption(cat,y,sl,vars,typings,e)
  sl is 'failed => sl
  -- Every existential must have a value
  or/[symbolTarget(v,sl) = nil for v in vars] => 'failed
  sl  --FIXME: check typing constraints

++ Attempt to find values for existentially quantified variables in
++ the predicate `cond' so that it holds in the environment `e'.
++ Return a substitution on success; otherwise fail.
bindPredicateExistentials(cond,e) ==
  cond is true => nil -- identity substitution
  cond is ['%exist,vars,['AND,:conds]] =>
    deduceImplicitArguments(vars,conds,e)
  'failed

++ The argument list `argl' is used to instantiate a constructor
++ with `modemap' in environment `e'.  Return the resulting
++ modemap is instantiation is legit.
evaluateConstructorModemap(argl,modemap is [[dc,:sig],:.],e) ==
  #dc ~= #sig =>
    keyedSystemError("S2GE0016",['"evaluateConstructorModemap",
      '"Incompatible maps"])
  #argl ~= #sig.source => nil
  -- Get `source-level' subtitution in an attempt to deduce implicits.
  sl := pairList(dc.args,argl)
  sl' := bindPredicateExistentials(applySubst(sl,modemap.mmCondition),e)
  sl' is 'failed => nil
  -- Subtitute values for implicit in formal modemap.  Then substitute
  -- the `source-level' arguments into the resulting modemap, before
  -- compiling them.  Note the sort of bootstrapping process.
  signature := applySubst(sl',modemap.mmSignature)
  args' := [x for a in argl for m in applySubst(sl,signature.source)
              | [x,.,e] := compOrCroak(a,m,e)]
  -- Now substitutte elaborations of actual arguments into the formal
  -- signature to construct the final result.
  signature := applySubst(pairList(dc.args,args'),signature)
  -- At this point, the modemap condition was evaluated successfully,
  -- so we return plain `true' for that part of the modemap.
  [[[[dc.op,:args'],:signature],[true,dc.op]],e]


--% SPECIAL EVALUATION FUNCTIONS

compEnumCat(x,m,e) ==
  for arg in x.args repeat
    ident? arg => nil   -- OK
    stackAndThrow('"all arguments to %1b must be identifiers",[x.op])
  [x,resolve($Category,m),e]

compConstructorCategory(x,m,e) ==
  x is [ctor,:args] =>
    ctor in '(RecordCategory UnionCategory MappingCategory) =>
      failed := false
      colons := 0
      args' := []
      while not failed for y in args repeat
        y is [":",.,t] =>
          colons := colons + 1
          [t',.,e] := compForMode(t,$EmptyMode,e) or return (failed := true)
          args' := [[y.op,second y,t'],:args']
        [t',.,e] := compForMode(y,$EmptyMode,e) or return (failed := true)
        args' := [t',:args']
      failed => nil
      colons ~= 0 and colons ~= #args and ctor isnt 'MappingCategory => nil
      [[ctor,:reverse! args'],resolve($Category,m),e]
    ctor is 'EnumerationCategory => compEnumCat(x,m,e)
    nil
  nil

--% SUBSET CATEGORY

compSubsetCategory: (%Form,%Mode,%Env) -> %Maybe %Triple
compSubsetCategory(["SubsetCategory",cat,R],m,e) ==
  --1. put "Subsets" property on R to allow directly coercion to subset;
  --   allow automatic coercion from subset to R but not vice versa
  e:= put(R,"Subsets",[[$lhsOfColon,"isFalse"]],e)
  --2. give the subset domain modemaps of cat plus 3 new functions
  comp(["Join",cat,C'],m,e) where
    C'() ==
      substitute($lhsOfColon,"$",C'') where
        C''() ==
          ["CATEGORY","domain",["SIGNATURE","coerce",[R,"$"]],["SIGNATURE",
            "lift",[R,"$"]],["SIGNATURE","reduce",["$",R]]]

--% CONS

compCons: (%Form,%Mode,%Env) -> %Maybe %Triple
compCons1: (%Form,%Mode,%Env) -> %Maybe %Triple

compCons(form,m,e) == compCons1(form,m,e) or compForm(form,m,e)

compCons1(["CONS",x,y],m,e) ==
  [x,mx,e]:= comp(x,$EmptyMode,e) or return nil
  null y => coerce([['%list,x],["List",mx],e],m)
  yt:= [y,my,e]:= comp(y,$EmptyMode,e) or return nil
  T:=
    my is ["List",m',:.] =>
      mr:= ["List",resolve(m',mx) or return nil]
      yt':= coerce(yt,mr) or return nil
      [x,.,e]:= coerce([x,mx,yt'.env],second mr) or return nil
      yt'.expr is ['%list,:.] => [['%list,x,:rest yt'.expr],mr,e]
      [['%pair,x,yt'.expr],mr,e]
    [['%pair,x,y],["Pair",mx,my],e]
  coerce(T,m)

--% SETQ

compSetq: (%Instantiation,%Mode,%Env) -> %Maybe %Triple
compSetq1: (%Form,%Form,%Mode,%Env) -> %Maybe %Triple

compSetq([":=",form,val],m,E) == 
  compSetq1(form,val,m,E)

compSetq1(form,val,m,E) ==
  ident? form => setqSingle(form,val,m,E)
  form is [":",x,y] =>
    [.,.,E']:= compMakeDeclaration(x,y,E)
    compSetq1(x,val,m,E')
  form is [op,:l] =>
    op is "CONS"  => setqMultiple(uncons form,val,m,E)
    op is "%Comma" => setqMultiple(l,val,m,E)
    setqSetelt(form,val,m,E)

compMakeDeclaration: (%Form,%Mode,%Env) -> %Maybe %Triple
compMakeDeclaration(x,m,e) ==
  $insideExpressionIfTrue: local := false
  compColon([":",x,m],$EmptyMode,e)

setqSetelt([v,:s],val,m,E) ==
  comp(["setelt",v,:s,val],m,E)

setqSingle(id,val,m,E) ==
  checkVariableName id
  $insideSetqSingleIfTrue: local:= true
    --used for comping domain forms within functions
  currentProplist:= getProplist(id,E)
  m'':=
    get(id,"mode",E) or getmode(id,E) or
       (if m=$NoValueMode then $EmptyMode else m)
  T:=
    eval or return nil where
      eval() ==
        T:= comp(val,m'',E) => T
        get(id,"mode",E) = nil and m'' ~= (maxm'':=maximalSuperType m'') and
           (T:=comp(val,maxm'',E)) => T
        (T:= comp(val,$EmptyMode,E)) and getmode(T.mode,E) =>
          assignError(val,T.mode,id,m'')
  T':= [x,m',e']:= coerce(T,m) or return nil
  if $profileCompiler then
    not ident? id => nil
    key :=
      symbolMember?(id,$form.args) => "arguments"
      "locals"
    profileRecord(key,id,T.mode)
  newProplist := 
    consProplistOf(id,currentProplist,"value",removeEnv [val,:rest T])
  e':= 
    cons? id => e'
    addBinding(id,newProplist,e')
  if isDomainForm(val,e') then
    if isDomainInScope(id,e') then
      stackWarning('"domain valued variable %1b has been reassigned within its scope",[id])
    -- single domains have constant values in their scopes, we might just
    -- as well take advantage of that at compile-time where appropriate.
    e' := put(id,'%macro,val,e')
    e':= augModemapsFromDomain1(id,val,e')
      --all we do now is to allocate a slot number for lhs
      --e.g. the %LET form below will be changed by putInLocalDomainReferences
  form :=
    db := constructorDB currentConstructor e'
    k := assocIndex(db,id) => ['%store,['%tref,'$,k],x]
    ["%LET",id,x]
  [form,m',e']

assignError(val,m',form,m) ==
  val =>
    stackMessage('"CANNOT ASSIGN: %1b OF MODE: %2pb TO: %3b OF MODE: %4bp",
      [val,m',form,m])
  stackMessage('"CANNOT ASSIGN: %1b TO: %2b OF MODE: %3pb",[val,form,m])

setqMultiple(nameList,val,m,e) ==
  val is ["CONS",:.] and m=$NoValueMode =>
    setqMultipleExplicit(nameList,uncons val,m,e)
  val is ["%Comma",:l] and m=$NoValueMode => 
    setqMultipleExplicit(nameList,l,m,e)
  -- 1. create a gensym, %add to local environment, compile and assign rhs
  g:= genVariable()
  e:= addBinding(g,nil,e)
  T:= [.,m1,.]:= compSetq1(g,val,$EmptyMode,e) or return nil
  e:= put(g,"mode",m1,e)
  [x,m',e]:= coerce(T,m) or return nil
  -- 2. exit if result is a list
  m1 is ["List",D] =>
    for y in nameList repeat 
      e:= giveVariableSomeValue(y,D,e)
    coerce([['%seq,x,["%LET",nameList,g],g],m',e],m)
  -- 3. For a cross, do it by hand here instead of general mm.  FIXME.
  m1 is ['Cross,:.] =>
    n := #m1.args
    #nameList ~= n =>
      stackMessage('"%1b must decompose into %2 components",[val,n])
    stmts := nil
    for y in nameList for t in m1.args for i in 0.. repeat
      e := giveVariableSomeValue(y,t,e)
      stmts := [['%LET,y,['%call,eltRecordFun(n,i),g,i]],:stmts]
    coerce([['%seq,x,:reverse! stmts,g],m1,e],m)
  -- 4. verify that the #nameList = number of parts of right-hand-side
  selectorModePairs:=
                                                --list of modes
    decompose(m1,#nameList,e) or return nil where
      decompose(t,length,e) ==
        t is ["Record",:l] => [[name,:mode] for [":",name,mode] in l]
        comp(t,$EmptyMode,e) is [.,["RecordCategory",:l],.] =>
          [[name,:mode] for [":",name,mode] in l]
        stackMessage('"no multiple assigns to mode: %1p",[t])
  #nameList~=#selectorModePairs =>
    stackMessage('"%1b must decompose into %2 components",[val,#nameList])
  -- 5. generate code; return
  assignList:=
    [([.,.,e]:= compSetq1(x,["elt",g,y],z,e) or return "failed").expr
      for x in nameList for [y,:z] in selectorModePairs]
  assignList="failed" => nil
  [['%seq,x,:assignList,g],m',e]

setqMultipleExplicit(nameList,valList,m,e) ==
  #nameList~=#valList =>
    stackMessage('"Multiple assignment error; # of items in: %1b must = # in: %2",[nameList,valList])
  gensymList:= [genVariable() for name in nameList]
  bindings := --should be fixed to declare genVar when possible
    [insn.args for g in gensymList for val in valList
      | [insn,.,e] := compSetq1(g,val,$EmptyMode,e) or leave "failed" ]
  bindings is "failed" => nil
  reAssignList :=
    [[.,.,e] := compSetq1(name,g,$EmptyMode,e) or return "failed"
      for g in gensymList for name in nameList]
  reAssignList is "failed" => nil
  [['%bind,bindings,['%seq,:[T.expr for T in reAssignList]]],
      $NoValueMode, last(reAssignList).env]

--% Quasiquotation

++ Compile a quotation `[| form |]'.  form is not type-checked, and
++ is returned as is.  Note:  when get to support splicing, we would
++ need to scan `form' to see whether there is any computation that
++ must be done.
++ ??? Another strategy would be to infer a more accurate domain
++ ??? based on the meta operator, e.g. (DEF ...) would be a
++ DefinitionAst, etc.  That however requires that we have a full
++ fledged AST algebra -- which we don't have yet in mainstream.
compileQuasiquote: (%Instantiation,%Mode,%Env) -> %Maybe %Triple
compileQuasiquote(["[||]",:form],m,e) ==
  null form => nil
  coerce([['QUOTE, :form],$Syntax,e], m)


--% WHERE

++ The form `item' appears in a side condition of a where-expression.
++ Register all declarations it locally introduces.
recordDeclarationInSideCondition(item,e,decls) ==
  item is [":",x,t] =>
    t := macroExpand(t,e)
    ident? x => deref(decls) := [[x,t],:deref decls]
    x is ['%Comma,:.] =>
      deref(decls) := [:[[x',t] for x' in x.args],:deref decls]
  item is ['SEQ,:stmts,["exit",.,val]] =>
    for stmt in stmts repeat
      recordDeclarationInSideCondition(stmt,e,decls)
    recordDeclarationInSideCondition(val,e,decls)

compWhere: (%Form,%Mode,%Env) -> %Maybe %Triple
compWhere([.,form,:exprList],m,eInit) ==
  $insideExpressionIfTrue: local:= false
  $insideWhereIfTrue: local := true
  e := eInit
  decls := ref get('%compilerData,'%whereDecls,e)
  u :=
    for item in exprList repeat
      recordDeclarationInSideCondition(item,e,decls)
      [.,.,e]:= comp(item,$EmptyMode,e) or return "failed"
  u is "failed" => return nil
  -- Remember side declaration constraints, if any.
  if deref decls ~= nil then
    e := put('%compilerData,'%whereDecls,deref decls,e)
  $insideWhereIfTrue := false
  [x,m,eAfter] := comp(macroExpand(form,eBefore := e),m,e) or return nil
  eFinal :=
    del := deltaContour(eAfter,eBefore) => addContour(del,eInit)
    eInit
  [x,m,eFinal]

compConstruct: (%Form,%Mode,%Env) -> %Maybe %Triple
compConstruct(form is ["construct",:l],m,e) ==
  y:= modeIsAggregateOf("List",m,e) =>
    T:= compList(l,["List",second y],e) => coerce(T,m)
    compForm(form,m,e)
  y:= modeIsAggregateOf("Vector",m,e) =>
    T:= compVector(l,["Vector",second y],e) => coerce(T,m)
    compForm(form,m,e)
  T:= compForm(form,m,e) => T
  for D in getDomainsInScope e repeat
    (y:=modeIsAggregateOf("List",D,e)) and
      (T:= compList(l,["List",second y],e)) and (T':= coerce(T,m)) =>
         return T'
    (y:=modeIsAggregateOf("Vector",D,e)) and
      (T:= compVector(l,["Vector",second y],e)) and (T':= coerce(T,m)) =>
         return T'

++ Compile a literal (quoted) symbol.
compQuote: (%Form,%Mode,%Env) -> %Maybe %Triple
compQuote(expr,m,e) == 
  expr is ['QUOTE,x] and ident? x => 
    -- Ideally, Identifier should be the default type.  However, for
    -- historical reasons we cannot afford that luxury yet.
    m = $Identifier or listMember?(m,$IOFormDomains) => [expr,m,e]
    coerce([expr,$Symbol,e],m)
  stackAndThrow('"%1b is not a literal symbol.",[x])

compList: (%Form,%Mode,%Env) -> %Maybe %Triple
compList(l,m is ["List",mUnder],e) ==
  null l => ['%nil,m,e]
  Tl:= [[.,mUnder,e]:= comp(x,mUnder,e) or return "failed" for x in l]
  Tl is "failed" => nil
  T := [['%list,:[T.expr for T in Tl]],["List",mUnder],e]

compVector: (%Form,%Mode,%Env) -> %Maybe %Triple
compVector(l,m is ["Vector",mUnder],e) ==
  Tl := [[.,mUnder,e]:= comp(x,mUnder,e) or return "failed" for x in l]
  Tl is "failed" => nil
  [["MAKE-ARRAY", #Tl, KEYWORD::ELEMENT_-TYPE, quote getVMType mUnder,
     KEYWORD::INITIAL_-CONTENTS, ['%list, :[T.expr for T in Tl]]],m,e]

--% MACROS

++ True if we are compiling a macro definition.
$macroIfTrue := false

compMacro(form,m,e) ==
  $macroIfTrue: local:= true
  ["MDEF",lhs,signature,rhs] := form
  if $verbose then
    prhs :=
      rhs is ['CATEGORY,:.] => ['"-- the constructor category"]
      rhs is ['Join,:.]     => ['"-- the constructor category"]
      rhs is ['CAPSULE,:.]  => ['"-- the constructor capsule"]
      rhs is ['add,:.]      => ['"-- the constructor capsule"]
      formatUnabbreviated rhs
    sayBrightly ['"   processing macro definition",'"%b",
      :formatUnabbreviated lhs,'" ==> ",:prhs,'"%d"]
  m=$EmptyMode or m=$NoValueMode =>
    -- Macro names shall be identifiers.
    (lhs isnt [.,:.] and not ident? lhs)
      or (lhs is [op,:.] and not ident? op) =>
        stackMessage('"invalid left-hand-side in macro definition",nil)
        e
    -- We do not have the means, at this late stage, to make a distinction
    -- between a niladic functional macro and an identifier that is
    -- defined as a macro.
    if lhs is [op] then lhs := op
    ["/throwAway",$NoValueMode,putMacro(lhs,macroExpand(rhs,e),e)]
  nil

--% SEQ

compSeq: (%Form,%Mode,%Env) -> %Maybe %Triple
compSeq1: (%Form,%List %Thing,%Env) -> %Maybe %Triple
compSeqItem: (%Form,%Mode,%Env) -> %Maybe %Triple

compSeq(["SEQ",:l],m,e) == 
  compSeq1(l,[m,:$exitModeStack],e)

compSeq1(l,$exitModeStack,e) ==
  $insideExpressionIfTrue: local := false
  $finalEnv: local := nil   --used in replaceExitEtc.
  c :=
    [([.,.,e] := compSeqItem(x,$NoValueMode,e) or leave "failed").expr
      for x in l]
  if c is "failed" then return nil
  catchTag := MKQ gensym()
  form := ['%seq,:replaceExitEtc(c,catchTag,"TAGGEDexit",first $exitModeStack)]
  [['%scope,catchTag,form],first $exitModeStack,$finalEnv]

compSeqItem(x,m,e) ==
  $insideExpressionIfTrue := false
  comp(macroExpand(x,e),m,e)

replaceExitEtc(x,tag,opFlag,opMode) ==
  (fn(x,tag,opFlag,opMode); x) where
    fn(x,tag,opFlag,opMode) ==
      atomic? x => nil
      x is [ =opFlag,n,t] =>
        t.expr := replaceExitEtc(t.expr,tag,opFlag,opMode)
        n=0 =>
          $finalEnv :=
            $finalEnv ~= nil => intersectionEnvironment($finalEnv,t.env)
            t.env
          x.op :=
            opFlag is 'TAGGEDreturn => '%return
            second(x) := tag
            '%leave
          third(x) := convertOrCroak(t,opMode).expr
        second(x) := n-1
      x is [key,n,t] and key in '(TAGGEDreturn TAGGEDexit) =>
        t.expr := replaceExitEtc(t.expr,tag,opFlag,opMode)
      replaceExitEtc(first x,tag,opFlag,opMode)
      replaceExitEtc(rest x,tag,opFlag,opMode)

--% SUCHTHAT
compSuchthat: (%Form,%Mode,%Env) -> %Maybe %Triple
compSuchthat([.,x,p],m,e) ==
  [x',m',e]:= comp(x,m,e) or return nil
  [p',.,e]:= comp(p,$Boolean,e) or return nil
  e:= put(x',"condition",p',e)
  [x',m',e]

--% exit

compExit: (%Form,%Mode,%Env) -> %Maybe %Triple
compExit(["exit",level,x],m,e) ==
  index := level-1
  $exitModeStack = [] => comp(x,m,e)
  m1 := $exitModeStack.index
  [x',m',e']:=
    u :=
      comp(x,m1,e) or return
        stackMessageIfNone ["cannot compile exit expression",x,"in mode",m1]
  modifyModeStack(m',index)
  [["TAGGEDexit",index,u],m,e]

modifyModeStack(m,index) ==
  $reportExitModeStack =>
    SAY("exitModeStack: ",copyTree $exitModeStack," ====> ",
      ($exitModeStack.index:= resolve(m,$exitModeStack.index); $exitModeStack))
  $exitModeStack.index:= resolve(m,$exitModeStack.index)

compLeave: (%Form,%Mode,%Env) -> %Maybe %Triple
compLeave(["leave",level,x],m,e) ==
  index := #$exitModeStack - 1 - $leaveLevelStack.(level-1)
  [x',m',e'] := u := comp(x,$exitModeStack.index,e) or return nil
  modifyModeStack(m',index)
  [["TAGGEDexit",index,u],m,e]

jumpFromLoop(kind,key) ==
  null $exitModeStack or kind ~= $loopKind =>
    stackAndThrow('"You can use %1b only in %2b loop",[key,kind])
    false
  true

compBreak: (%Symbol,%Mode,%Env) -> %Maybe %Triple
compBreak(x,m,e) ==
  x isnt "break" or not jumpFromLoop("REPEAT",x) => nil
  index := #$exitModeStack - 1 - $leaveLevelStack.0
  $breakCount := $breakCount + 1
  u := coerce(["$NoValue",$Void,e],$exitModeStack.index) or return nil
  u := coerce(u,m) or return nil
  modifyModeStack(u.mode,index)
  [["TAGGEDexit",index,u],m,e]

compIterate: (%Symbol,%Mode,%Env) -> %Maybe %Triple
compIterate(x,m,e) ==
  x isnt "iterate" or not jumpFromLoop("REPEAT",x) => nil
  index := #$exitModeStack - 1 - ($leaveLevelStack.0 + 1)
  $iterateCount := $iterateCount + 1
  u := coerce(['%nil,'$Void,e],$exitModeStack.index) or return nil
  u := coerce(u,m) or return nil
  modifyModeStack(u.mode,index)
  if $loopBodyTag = nil then       -- bound in compRepeatOrCollect
    $loopBodyTag := MKQ gensym()
  [['%leave,$loopBodyTag,u.expr],u.mode,e]

--% return

compReturn: (%Form,%Mode,%Env) -> %Maybe %Triple
compReturn(["return",x],m,e) ==
  null $exitModeStack =>
    stackAndThrow('"the return before %1b is unneccessary",[x])
    nil
  index:= MAX(0,#$exitModeStack-1)
  if index >= 0 then 
    $returnMode:= resolve($exitModeStack.index,$returnMode)
  [x',m',e']:= u:= comp(x,$returnMode,e) or return nil
  if index>=0 then
    $returnMode:= resolve(m',$returnMode)
    modifyModeStack(m',index)
  [["TAGGEDreturn",0,u],m,e']

--% throw expressions

compThrow: (%Form,%Mode,%Env) -> %Maybe %Triple
compThrow(["%Throw",x],m,e) ==
  T := compOrCroak(x,$EmptyMode,e)
  -- An exception does not use the normal exit/return route, so
  -- we don't take into account neither $exitModeStack nor $returnMode.
  [['%throw,T.mode,T.expr],$NoValueMode,T.env]

compCatch: (%Form,%Mode,%Env) -> %Maybe %Triple
compCatch([x,s],m,e) ==
  [.,m',e] := compMakeDeclaration(second x, third x,e)
  T := compOrCroak(s,m,e)
  [['%catch,second x,m',T.expr],T.mode,T.env]
  
compTry: (%Form,%Mode,%Env) -> %Maybe %Triple
compTry(['%Try,x,ys,z],m,e) ==
  x' := compOrCroak(x,m,e).expr
  ys' := [compCatch(y,m,e).expr for y in ys]
  z' :=
    z = nil => nil
    ['%finally,compOrCroak(z,$NoValueMode,e).expr]
  [['%try,x',ys',z'],m,e]
  
--% ELT

getModemapListFromDomain(op,numOfArgs,D,e) ==
  [mm
    for (mm:= [[dc,:sig],:.]) in get(op,'modemap,e) | dc=D and #rest sig=
      numOfArgs]

++ `op' supposedly designate an external entity with language linkage
++ `lang'.  Return the mode of its local declaration (import).
getExternalSymbolMode(op,lang,e) ==
  lang is 'Builtin => "%Thing"      -- for the time being
  lang is 'Lisp => "%Thing"         -- for the time being
  lang is "C" =>
    stackAndThrow('"Sorry: %b Foreign %1b %d is invalid at the moment",[lang])
  get(op,"%Lang",e) ~= lang =>
    stackAndThrow('"%1bp is not known to have language linkage %2bp",[op,lang])
  getmode(op,e) or stackAndThrow('"Operator %1bp is not in scope",[op])

compElt: (%Form,%Mode,%Env) -> %Maybe %Triple
compElt(form,m,E) ==
  form isnt ["elt",aDomain,anOp] => compForm(form,m,E)
  aDomain is "Lisp" or (aDomain is ["Foreign",lang] and lang="Builtin") =>
    [anOp',m,E] where anOp'() == (anOp = $Zero => 0; anOp = $One => 1; anOp)
  lang ~= nil =>
    opMode := getExternalSymbolMode(anOp,lang,E)
    op := get(anOp,"%Link",E) or anOp
    coerce([op,opMode,E],m)
  isDomainForm(aDomain,E) =>
    E := addDomain(aDomain,E)
    mmList:= getModemapListFromDomain(internalName anOp,0,aDomain,E)
    modemap:=
      -- FIXME: do this only for constants.
      n:=#mmList
      1=n => mmList.0
      0=n =>
        return
          stackMessage('"Operation %1b missing from domain: %2p",
            [anOp,aDomain])
      stackWarning('"more than 1 modemap for: %1 with dc = %2p ===> %3",
        [anOp,aDomain,mmList])
      mmList.0
    [sig,[pred,val]]:= modemap
    #sig ~= 2 and val isnt ["CONST",:.] => nil
    val := genDeltaEntry(opOf anOp,modemap,E)
    coerce([['%call,val],second sig,E], m)
  compForm(form,m,E)

--% HAS

compHas: (%Form,%Mode,%Env) -> %Maybe %Triple
compHas(pred is ["has",a,b],m,e) ==
  e := chaseInferences(pred,e)
  predCode := compHasFormat(pred,e)
  coerce([predCode,$Boolean,e],m)

      --used in various other places to make the discrimination

compHasFormat(pred is ["has",olda,b],e) ==
  argl := $form.args
  formals := take(#argl,$FormalMapVariableList)
  a := applySubst(pairList(formals,argl),olda)
  [a,.,e] := comp(a,$EmptyMode,e) or return nil
  a := applySubst(pairList(argl,formals),a)
  b is ["ATTRIBUTE",c] => ["HasAttribute",a,quote c]
  b is ["SIGNATURE",op,sig,:.] =>
     ["HasSignature",a,
       mkList [MKQ op,mkList [mkTypeForm type for type in sig]]]
  b is ["Join",:l] or b is ["CATEGORY",.,:l] => 
     ["AND",:[compHasFormat(["has",olda,c],e) for c in l]]
  isCategoryForm(b,e) => ["HasCategory",a,optimize! mkTypeForm b]
  stackAndThrow('"Second argument to %1b must be a category, or a signature or an attribute",["has"])

--% IF

compIf: (%Form,%Mode,%Env) -> %Maybe %Triple
compPredicate: (%Form,%Env) -> %Code
compFromIf: (%Form,%Mode,%Env) -> %Maybe %Triple

compIf(["IF",a,b,c],m,E) ==
  [xa,ma,Ea,Einv]:= compPredicate(a,E) or return nil
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

canReturn(expr,level,exitCount,ValueFlag) ==  --SPAD: exit and friends
  expr isnt [.,:.] => ValueFlag and level=exitCount
  op := expr.op
  op in '(QUOTE CLOSEDFN %lambda) => ValueFlag and level=exitCount
  op is "TAGGEDexit" =>
    expr is [.,count,data] => canReturn(data.expr,level,count,count=level)
  level=exitCount and not ValueFlag => nil
  op is '%seq => or/[canReturn(u,level+1,exitCount,false) for u in rest expr]
  op is "TAGGEDreturn" => nil
  op is '%scope =>
    [.,gs,data]:= expr
    (findThrow(gs,data,level,exitCount,ValueFlag) => true) where
      findThrow(gs,expr,level,exitCount,ValueFlag) ==
        expr isnt [.,:.] => nil
        expr is ['%leave, =gs,data] => true
            --this is pessimistic, but I know of no more accurate idea
        expr is ['%seq,:l] =>
          or/[findThrow(gs,u,level+1,exitCount,ValueFlag) for u in l]
        or/[findThrow(gs,u,level,exitCount,ValueFlag) for u in rest expr]
    canReturn(data,level,exitCount,ValueFlag)
  op is '%when =>
    level = exitCount =>
      or/[canReturn(last u,level,exitCount,ValueFlag) for u in rest expr]
    or/[or/[canReturn(u,level,exitCount,ValueFlag) for u in v]
                for v in rest expr]
  op is "IF" =>
    expr is [.,a,b,c]
    if not canReturn(a,0,0,true) then
      SAY "IF statement can not cause consequents to be executed"
      pp expr
    canReturn(a,level,exitCount,nil) or canReturn(b,level,exitCount,ValueFlag)
      or canReturn(c,level,exitCount,ValueFlag)
  op in '(LET %bind) =>
    or/[canReturn(init,level,exitCount,false) for [.,init] in second expr]
       or canReturn(third expr,level,exitCount,ValueFlag)
  --now we have an ordinary form
  op isnt [.,:.] => and/[canReturn(u,level,exitCount,ValueFlag) for u in expr]
  systemErrorHere ['"canReturn",expr] --for the time being

++ We are compiling a conditional expression, type check and generate
++ code for the predicate of the branch as a Boolean expression.
compPredicate(p,E) ==
  -- Ideally, we should be first inferring the type of the predicate
  -- `p'.  That would have the virtue of pointing out possible 
  -- ambiguities.  Then, on a second phase, implicitly coerce the
  -- the result to Boolean.  However, that would not quite work.  The 
  -- being that there are cases, such as equality, that are highgly
  -- ambiguous (e.g. see the various overloading of `=') for which it
  -- would be unfortunate to require more type annotation.  Note that
  -- the problem here is many misguided overloading of some operators.
  -- Consequently, we compile directly with Boolean as target.
  [p',m,E] := comp(p,$Boolean,E) or return nil
  [p',m,getSuccessEnvironment(p,E),getInverseEnvironment(p,E)]

compFromIf(a,m,E) ==
  a is "%noBranch" => ["%noBranch",m,E]
  comp(a,m,E)

compImport: (%Form,%Mode,%Env) -> %Triple
compImport(["import",:doms],m,e) ==
  if not $bootStrapMode then
    for dom in doms repeat
      e := addDomain(dom,e)
  ["/throwAway",$NoValueMode,e]

--% Foreign Function Interface

bootDenotation: %Symbol -> %Symbol
bootDenotation s == 
  makeSymbol(symbolName s,"BOOTTRAN")

++ Return the Boot denotation of a basic FFI type.
getBasicFFIType: %Mode -> %Symbol
getBasicFFIType t ==
  t = $Byte => bootDenotation "byte"
  t = $Int16 => bootDenotation "int16"
  t = $UInt16 => bootDenotation "uint16"
  t = $Int32 => bootDenotation "int32"
  t = $UInt32 => bootDenotation "uint32"
  t = $Int64 => bootDenotation "int64"
  t = $UInt64 => bootDenotation "uint64"
  t = $SingleInteger => bootDenotation "int"
  t = $DoubleFloat => bootDenotation "double"
  t = $String => bootDenotation "string"
  t = $SystemPointer => bootDenotation "pointer"
  nil


++ List of admissible type modifiers in an FFI import declaration.
$FFITypeModifier == '(ReadOnly WriteOnly ReadWrite)

++ List of admissible element types of contiguously stored
++ homogeneous FFI aggregate types.
$FFIAggregableDataType ==
  [$Byte,
    $Int16,$UInt16,
      $Int32,$UInt32,
        $Int64, $UInt64,
          $DoubleFloat]

++ Return the Boot denotation of an FFI datatype.  This is either
++ a basic VM type, or a simple array of sized integer or floating
++ point type.
getFFIDatatype: %Mode -> %Form
getFFIDatatype t ==
  x := getBasicFFIType t => x
  t is [m,["PrimitiveArray",t']] and symbolMember?(m,$FFITypeModifier) and
    listMember?(t',$FFIAggregableDataType) =>
      m' := 
        m is "ReadOnly" => bootDenotation "readonly"
        m is "WriteOnly" => bootDenotation "writeonly"
        bootDenotation "readwrite"
      [m',[bootDenotation "buffer",getBasicFFIType t']]
  nil

++ Return the Boot denotation of a type that is valid in a external entity
++ signature.  
getBootType: %Mode -> %Form
getBootType t ==
  x := getFFIDatatype t => x
  t is ["Mapping",ret,:args] =>
    ret' := 
      ret = $Void => bootDenotation "void"
      getBasicFFIType ret or return nil
    args' := [getFFIDatatype arg or return "failed" for arg in args]
    args' = "failed" => return nil
    [bootDenotation "%Mapping",ret',args']
  nil

++ Verify that mode `t' is admissible in an external entity signature
++ specification, and return its Boot denotation.
checkExternalEntityType(t,e) ==
  t isnt [.,:.] => 
    stackAndThrow('"Type variable not allowed in import of external entity",nil)
  t' := getBootType t => t'
  stackAndThrow('"Type %1bp is invalid in a foreign signature",[t])


++ An external entity named `id' is being imported under signature
++ `type' from a foreign language `lang'.  Check that the import
++ is valid, and if so return the linkage name of the entity.
checkExternalEntity(id,type,lang,e) ==
  checkVariableName id
  -- An external entity name shall be unique in scope.
  getmode(id,e) =>
    stackAndThrow('"%1b is already in scope",[id])
  -- In particular, an external entity name cannot be overloaded
  -- with exported operators.
  get(id,"modemap",e) =>
    stackAndThrow('"%1b already names exported operations in scope",[id])
  -- We don't type check builtin declarations at the moment.
  lang is 'Builtin or lang is 'Lisp => id
  -- Only functions are accepted at the moment.  And all mentioned
  -- types must be those that are supported by the FFI.
  type' := checkExternalEntityType(type,e) 
  type' isnt [=bootDenotation "%Mapping",:.] =>
    stackAndThrow('"Signature for external entity must be a Mapping type",nil)
  id' := encodeLocalFunctionName id
  [def] := genImportDeclaration(id',[bootDenotation "%Signature",id,type'])
  compileLispDefinition(id,def)
  id'


++ Remove possible modifiers in the FFI type expression `t'.
removeModifiers t ==
  for (ts := [x,:.]) in tails t repeat
     x is [m,t'] and symbolMember?(m,$FFITypeModifier) =>
       ts.first := t'
  t

++ Compile external entity signature import.
compSignatureImport: (%Form,%Mode,%Env) -> %Maybe %Triple
compSignatureImport(["%SignatureImport",id,type,home],m,e) ==
  -- 1. Make sure we have the right syntax.
  home isnt ["Foreign",:args] =>
    stackAndThrow('"signature import must be from a %1bp domain",["Foreign"])
  args isnt [lang] =>
    stackAndThrow('"%1bp takes exactly one argument",["Foreign"])
  not ident? lang =>
    stackAndThrow('"Argument to %1bp must be an identifier",["Foreign"])
  not (lang in '(Builtin C Lisp)) =>
    stackAndThrow('"Sorry: Only %1bp is valid at the moment",["Foreign C"])
  -- 2. Make sure this import is not subverting anything we know
  id' := checkExternalEntity(id,type,lang,e)
  -- 3. Make a local declaration for it.
  T := [.,.,e] := compMakeDeclaration(id,removeModifiers type,e) or return nil
  e := put(id,"%Lang",lang,e)
  e := put(id,"%Link",id',e)
  -- 4. Also make non-function externals self-evaluating so we don't
  --    complain later for undefined variable references.
  if T.mode isnt ['Mapping,:.] then
    e := put(id,"value",[id',T.mode,nil],e)
  T.env := e
  coerce(T,m)


++ Compile package call to an external function.  
++ `lang' is the language calling convention
++ `op' is the operator name
++ `args' is the list of arguments
++ `m' is the context mode.
++ `e' is the compilation environment in effect.
compForeignPackageCall(lang,op,args,m,e) ==
  lang = "Builtin" =>
    -- Note: We don't rename builtin functions.
    [[op,:[([.,.,e]:= compOrCroak(x,$EmptyMode,e)).expr
             for x in args]],m,e]
  getExternalSymbolMode(op,lang,e) is ["Mapping",:argModes]
    and (#argModes = #args + 1) => applyMapping([op,:args],m,e,argModes)
  stackAndThrow('"OpenAxiom could not determine the meaning of %1bp",[op])

--% Compilation of logical operators that may have a pre-defined
--% meaning, or may need special handling because or short-circuiting
--% etc.

++ Compile a logical negation form `(not ...)'.
compLogicalNot: (%Form,%Mode,%Env) -> %Maybe %Triple
compLogicalNot(x,m,e) ==
  x isnt ["not", y] => nil
  -- ??? For the time being compiler values cannot handle operations
  -- ??? selected through general modemaps, and their semantics
  -- ??? are quite hardwired with their syntax.
  -- ??? Eventually, we should not need to do this.
  yTarget :=
    $normalizeTree and resolve(m,$Boolean) = $Boolean => $Boolean
    $EmptyMode
  yT := comp(y,yTarget,e) or return nil
  yT.mode = $Boolean and yTarget = $Boolean => 
    [["%not",yT.expr],yT.mode,yT.env]
  compResolveCall("not",[yT],m,yT.env)


++ Compile an exclusive `xor' expression.
compExclusiveOr: (%Form,%Mode,%Env) -> %Maybe %Triple
compExclusiveOr(x,m,e) ==
  x isnt ["xor",a,b] => nil
  aT := comp(a,$EmptyMode,e) or return nil
  e := 
    aT.mode = $Boolean => getSuccessEnvironment(a,aT.env)
    aT.env
  bT := comp(b,$EmptyMode,e) or return nil
  compResolveCall("xor",[aT,bT],m,bT.env)

--% Case
compCase: (%Form,%Mode,%Env) -> %Maybe %Triple
compCase1: (%Form,%Mode,%Env) -> %Maybe %Triple

getModemapList(op,nargs,e) ==
  op is ['elt,D,op'] => getModemapListFromDomain(internalName op',nargs,D,e)
  [mm for
    (mm:= [[.,.,:sigl],:.]) in get(op,'modemap,e) | nargs=#sigl]
 
--Will the jerk who commented out these two functions please NOT do so
--again.  These functions ARE needed, and case can NOT be done by
--modemap alone.  The reason is that A case B requires to take A
--evaluated, but B unevaluated.  Therefore a special function is
--required.  You may have thought that you had tested this on "failed"
--etc., but "failed" evaluates to it's own mode.  Try it on x case $
--next time.
--                An angry JHD - August 15th., 1984

compCase(["case",x,m'],m,e) ==
  e:= addDomain(m',e)
  T:= compCase1(x,m',e) => coerce(T,m)
  nil

compCase1(x,m,e) ==
  [x',m',e'] := comp(x,$EmptyMode,e) or return nil
  u :=
    [mm for mm in getModemapList("case",2,e') 
      | mm.mmSignature is [=$Boolean,s,t] and modeEqual(maybeSpliceMode t,m) 
            and modeEqual(s,m')] or return nil
  fn := (or/[mm for mm in u | mm.mmCondition = true]) or return nil
  fn := genDeltaEntry("case",fn,e)
  [['%call,fn,x',MKQ m],$Boolean,e']



++ For `case' operation implemented in library, the second operand
++ (target type) is taken unevaluated. The corresponding parameter 
++ type in the modemap was specified as quasiquotation.  We
++ want to look at the actual type when comparing with modeEqual.
maybeSpliceMode: %Mode -> %Mode
maybeSpliceMode m ==
  (m' := isQuasiquote m) => m'
  m

compColon: (%Form,%Mode,%Env) -> %Maybe %Triple
compColon([":",f,t],m,e) ==
  $insideExpressionIfTrue => compColonInside(f,m,e,t)
    --if inside an expression, ":" means to convert to m "on faith"
  $lhsOfColon: local:= f
  t:=
    t isnt [.,:.] and (t':= assoc(t,getDomainsInScope e)) => t'
    isDomainForm(t,e) and not $insideCategoryIfTrue =>
      (if not listMember?(t,getDomainsInScope e) then e:= addDomain(t,e); t)
    isDomainForm(t,e) or isCategoryForm(t,e) => t
    t is ["Mapping",m',:r] => t
    string? t => t              -- literal flag types are OK
    unknownTypeError t
    t
  f is ["LISTOF",:l] =>
    z := [T.expr for x in l while ([.,.,e] := T := compColon([":",x,t],m,e))]
    [['%seq,:z],t,e]
  e:=
    f is [op,:argl] =>
      --for MPOLY--replace parameters by formal arguments: RDJ 3/83
      --FIXME: why?  -- gdr 2011-04-30
      newTarget :=
        applySubst(pairList([(x is [":",a,m] => a; x) for x in argl],
                     $FormalMapVariableList),t)
      signature :=
        ["Mapping",newTarget,:
          [(x is [":",a,m] => m;
              getmode(x,e) or systemErrorHere ['"compColon",x]) for x in argl]]
      put(op,"mode",signature,e)
    put(f,"mode",t,e)
  if not $bootStrapMode and $insideFunctorIfTrue and
    makeCategoryForm(t,e) is [catform,e] then
        e := giveVariableSomeValue(f,t,e)
  val :=
    $insideCapsuleFunctionIfTrue => ['%LET,f,'%undefined]
    "/throwAway"
  [val,getmode(f,e),e]

unknownTypeError name ==
  name:=
    name is [op,:.] => op
    name
  stackAndThrow('"%1b is not a known type",[name])

compPretend: (%Form,%Mode,%Env) -> %Maybe %Triple
compPretend(["pretend",x,t],m,e) ==
  e:= addDomain(t,e)
  T:= comp(x,t,e) or comp(x,$EmptyMode,e) or return nil
  t' := T.mode            -- save this, in case we need to make suggestions
  T:= [T.expr,t,T.env]
  T':= coerce(T,m) => 
    -- If the `pretend' wasn't necessary, we should advise user to use
    -- less crude way of selecting expressions of thr `right type'.
    if t' = t then 
      stackWarning('"pretend %1p -- should replace by @",[t])
    T'
  nil

compColonInside(x,m,e,m') ==
  e:= addDomain(m',e)
  T:= comp(x,$EmptyMode,e) or return nil
  if (m'':=T.mode)=m' then warningMessage:= [":",m'," -- should replace by @"]
  T:= [T.expr,m',T.env]
  T':= coerce(T,m) =>
    if m'' = m' then
       stackWarning('": %1p -- should replace by @",[m'])
    else
       stackWarning('" : %1p -- replace by pretend", [m'])
    T'

compIs: (%Form,%Mode,%Env) -> %Maybe %Triple
compIs(["is",a,b],m,e) ==
  [aval,am,e] := comp(a,$EmptyMode,e) or 
    stackAndThrow('"Cannot determine the type of the expression %1b",[a])
  not isCategoryForm(am,e) =>
    stackAndThrow('"Expression %1b does not designate a domain",[a])
  [bval,bm,e] := comp(b,$EmptyMode,e) or return nil
  T:= [["domainEqual",aval,bval],$Boolean,e]
  coerce(T,m)

--%  Functions for coercion by the compiler

--  The function coerce is used by the old compiler for coercions.
--  The function coerceInteractive is used by the interpreter.
--  One should always call the correct function, since the represent-
--  ation of basic objects may not be the same.

tryCourtesyCoercion: (%Triple, %Mode) -> %Maybe %Triple
tryCourtesyCoercion(T,m) ==
  $InteractiveMode =>
    keyedSystemError("S2GE0016",['"coerce",
      '"function coerce called from the interpreter."])
  if $useRepresentationHack then
    T.rest.first := MSUBST("$",$Rep,second T)
  T' := coerceEasy(T,m) => T'
  T' := coerceSubset(T,m) => T'
  T' := coerceHard(T,m) => T'
  nil

coerce(T,m) ==
  T' := tryCourtesyCoercion(T,m) => T'
  isSomeDomainVariable m => nil
  stackMessage('"Cannot coerce %1b of mode %2pb to mode %3pb",
    [T.expr,T.mode,m])


coerceEasy: (%Triple,%Mode) -> %Maybe %Triple
coerceEasy(T,m) ==
  m=$EmptyMode => T
  m=$NoValueMode or m=$Void => [T.expr,m,T.env]
  T.mode =m => T
  T.mode =$Exit =>
      [['%seq,T.expr,["userError", '"Did not really exit."]],m,T.env]
  T.mode=$EmptyMode or modeEqualSubst(T.mode,m,T.env) =>
    [T.expr,m,T.env]

++ Return true if the VM constant form `val' is known to satisfy
++ the predicate `pred'.  Note that this is a fairly conservatism
++ approximation in the sense that the retunred value maye be false
++ for some other reasons, such as the predicate not being closed
++ with respect to the parameter `#1'.
satisfies(val,pred) ==
  pred=false or pred=true => pred
  vars := findVMFreeVars pred
  vars ~= nil and vars isnt ["#1"] => false
  eval ['%bind,[["#1",val]],pred]


++ If the domain designated by the domain forms `m' and `m'' have
++ a common super domain, return least such super domaon (ordered
++ in terms of sub-domain relationship).  Otherwise, return nil.
commonSuperType(m,m') ==
  lineage := [m']
  while (t := superType m') ~= nil repeat
    lineage := [t,:lineage]
    m' := t
  while m ~= nil repeat
    listMember?(m,lineage) => return m
    m := superType m

++ Coerce value `x' of mode `m' to mode `m'', if m is a subset of
++ of m'.  A special case is made for cross-subdomain conversion
++ for integral literals.
coerceSubset: (%Triple,%Mode) -> %Maybe %Triple
coerceSubset([x,m,e],m') ==
  isSubset(m,m',e) => [x,m',e]
  integer? x and (m'' := commonSuperType(m,m')) =>
    -- obviously this is temporary
    satisfies(x,isSubDomain(m',m'')) => [x,m',e]
    nil
  nil

coerceHard: (%Triple,%Mode) -> %Maybe %Triple
coerceHard(T,m) ==
  $e: local:= T.env
  m':= T.mode
  string? m' and modeEqual(m,$String) => [T.expr,m,$e]
  modeEqual(m',m) or ident? m' and
    (get(m',"value",$e) is [m'',:.] or getXmode(m',$e) is ["Mapping",m'']) and
      modeEqual(m'',m) or ident? m and
        (get(m,"value",$e) is [m'',:.] or getXmode(m,$e) is ["Mapping",m'']) and
          modeEqual(m'',m') => [T.expr,m,T.env]
  string? T.expr and T.expr=m => [T.expr,m,$e]
  isCategoryForm(m,$e) =>
      $bootStrapMode => [T.expr,m,$e]
      extendsCategoryForm(T.expr,T.mode,m) => [T.expr,m,$e]
      coerceExtraHard(T,m)
  (m' is "$" and m = $functorForm) or (m' = $functorForm and m = "$") =>
    [T.expr,m,$e]
  coerceExtraHard(T,m)

coerceExtraHard: (%Triple,%Mode) -> %Maybe %Triple
coerceExtraHard(T is [x,m',e],m) ==
  -- Allow implicit injection into Union, if that is
  -- clear from the context
  isUnionMode(m,e) is ['Union,:l] and listMember?(m',l) =>
    autoCoerceByModemap(T,m)
  -- For values from domains satisfying Union-like properties, apply
  -- implicit retraction if clear from context.
  (t := hasType(x,e)) and unionLike?(m',e) is ['UnionCategory,:l]
     and listMember?(t,l) =>
       T' := autoCoerceByModemap(T,t) => coerce(T',m)
       nil
  -- Give it one last chance.
  -- FIXME: really, we shouldn't.  Codes relying on this are
  -- FIXME: inherently difficult to comprehend and likely broken.
  T' := autoCoerceByModemap(T,m) => T'
  m' is ['Record,:.] and m = $OutputForm =>
      [['coerceRe2E,x,['ELT,copyTree m',0]],m,e]
  -- Domain instantiations are first class objects
  m = $Domain =>
    m' = $Category => nil
    isCategoryForm(m',e) => [x,m',e]
    nil
  nil

coerceable(m,m',e) ==
  m=m' => m
  tryCourtesyCoercion(["$fromCoerceable$",m,e],m') => m'
  nil

coerceExit: (%Triple,%Mode) -> %Maybe %Triple
coerceExit([x,m,e],m') ==
  m' := resolve(m,m')
  x' := replaceExitEtc(x,catchTag := MKQ gensym(),"TAGGEDexit",$exitMode)
  coerce([['%scope,catchTag,x'],m,e],m')

compAtSign: (%Form,%Mode,%Env) -> %Maybe %Triple
compAtSign(["@",x,m'],m,e) ==
  e:= addDomain(m',e)
  T:= comp(x,m',e) or return nil
  coerce(T,m)

compCoerce: (%Form,%Mode,%Env) -> %Maybe %Triple
compCoerce1: (%Form,%Mode,%Env) -> %Maybe %Triple
coerceByModemap: (%Maybe %Triple,%Mode) -> %Maybe %Triple
autoCoerceByModemap: (%Maybe %Triple,%Mode) -> %Maybe %Triple

compCoerce(["::",x,m'],m,e) ==
  e:= addDomain(m',e)
  T:= compCoerce1(x,m',e) => coerce(T,m)
  ident? m' and getXmode(m',e) is ["Mapping",["UnionCategory",:l]] =>
    T:= (or/[compCoerce1(x,m1,e) for m1 in l]) or return nil
    coerce([T.expr,m',T.env],m)

++ Subroutine of compCoerce1.  If `T' is a triple whose mode is
++ a super-domain of `sub', then return code that performs the
++ checked courtesy coercion to `sub'.
coerceSuperset: (%Triple, %Mode) -> %Maybe %Triple
coerceSuperset(T,sub) ==
  sub is "$" =>
    T' := coerceSuperset(T,$functorForm) or return nil
    T'.rest.first := "$"
    T'
  pred := isSubset(sub,T.mode,T.env) =>
    [["%retract",T.expr,sub,pred],sub,T.env]
  nil

compCoerce1(x,m',e) ==
  T:= comp(x,m',e) or comp(x,$EmptyMode,e) or return nil
  m1:=
    string? T.mode => $String
    T.mode
  m':=resolve(m1,m')
  T:=[T.expr,m1,T.env]
  T':= coerce(T,m') => T'
  T':= coerceByModemap(T,m') => T'
  T' := coerceSuperset(T,m') => T'
  nil

coerceByModemap([x,m,e],m') ==
  u :=
    [mm for mm in getModemapList("coerce",1,e)
      | mm.mmSignature is [t,s] and (modeEqual(t,m') or isSubset(t,m',e))
           and (modeEqual(s,m) or isSubset(m,s,e))] or return nil

  --mm:= (or/[mm for (mm:=[.,[cond,.]]) in u | cond=true]) or return nil
  mm:=first u  -- patch for non-trival conditons
  fn := genDeltaEntry('coerce,mm,e)
  [['%call,fn,x],m',e]

autoCoerceByModemap([x,source,e],target) ==
  u :=
    [mm for mm in getModemapList("autoCoerce",1,e)
      | mm.mmSignature is [t,s] and modeEqual(t,target) 
          and modeEqual(s,source)] or return nil
  fn := (or/[mm for mm in u | mm.mmCondition=true]) or return nil

  source is ["Union",:l] and listMember?(target,l) =>
    (y:= get(x,"condition",e)) and (or/[u is ["case",., =target] for u in y])
       => [['%call,genDeltaEntry("autoCoerce",fn,e),x],target,e]
    x="$fromCoerceable$" => nil
    stackMessage('"cannot coerce %1b of mode %2pb to %3pb without a case statement",
      [x,source,target])
  [['%call,genDeltaEntry("autoCoerce",fn,e),x],target,e]


++ Compile a comma separated expression list. These typically are
++ tuple objects, or argument list in a call to a homogeneous
++ vararg operations.
compComma: (%Form,%Mode,%Env) -> %Maybe %Triple
compComma(form,m,e) ==
  form isnt ["%Comma",:argl] => systemErrorHere ["compComma",form]
  Tl := [comp(a,$EmptyMode,e) or return "failed" for a in argl]
  Tl = "failed" => nil
  -- ??? Ideally, we would like to compile to a Cross type, then
  -- convert to the target type.  However, the current compiler and
  -- runtime data structures are not regular enough in their interfaces;
  -- so we make a special rule when compiling with a Tuple as target,
  -- we do the convertion here (instead of calling convert).  Semantically,
  -- there should be no difference, but it makes the compiler code
  -- less regular, with duplicated effort.
  m is ["Tuple",t] =>
    Tl' := [coerce(T,t) or return "failed" for T in Tl]
    Tl' = "failed" => nil
    [["asTupleNew0", ["getVMType",t], [T.expr for T in Tl']], m, e]
  T := [['%call,mkRecordFun #argl,:[T.expr for T in Tl]], 
        ["Cross",:[T.mode for T in Tl]], e]
  coerce(T,m)

--% Very old resolve
-- should only be used in the old (preWATT) compiler

resolve(din,dout) ==
  din=$NoValueMode or dout=$NoValueMode => $NoValueMode
  dout=$EmptyMode => din
  din~=dout and (string? din or string? dout) =>
    modeEqual(dout,$String) => dout
    modeEqual(din,$String) => nil
    mkUnion(din,dout)
  dout

modeEqual(x,y) ==
  -- this is the late modeEqual
  -- orders Unions
  x isnt [.,:.] or y isnt [.,:.] => x=y
  #x ~= #y => nil
  x is ['Union,:xl] and y is ['Union,:yl] =>
    for x1 in xl repeat
      for y1 in yl repeat
        modeEqual(x1,y1) =>
          xl := remove(xl,x1)
          yl := remove(yl,y1)
          return nil
    xl or yl => nil
    true
  (and/[modeEqual(u,v) for u in x for v in y])

modeEqualSubst(m1,m,e) ==
  modeEqual(m1, m) => true
  m1 isnt [.,:.] => get(m1,"value",e) is [m',:.] and modeEqual(m',m)
  m1 is [op,:l1] and m is [=op,:l2]  and # l1 = # l2 =>
-- Above length test inserted JHD 4:47 on 15/8/86
-- Otherwise Records can get fouled up - consider expressIdealElt
-- in the DEFAULTS package
        and/[modeEqualSubst(xm1,xm2,e) for xm1 in l1 for xm2 in l2]
  nil

--% Categories

compBuiltinDomain(form is [functorName,:argl],m,e) ==
  fn := property(functorName,"makeFunctionList") or return nil
  diagnoseUnknownType(form,e)
  [funList,e]:= FUNCALL(fn,form,form,e)
  exports := [cat for x in parentsOfBuiltinInstance form]
    where cat() ==
            x.rest is true => x.first
            ['IF,x.rest,['ATTRIBUTE,x.first],'%noBranch]
  catForm:=
    ["Join",:exports,["CATEGORY","domain",:
      [["SIGNATURE",op,sig] for [op,sig,.] in funList | op~="="]]]
  --RDJ: for coercion purposes, it necessary to know it's a Set; I'm not
  --sure if it uses any of the other signatures(see extendsCategoryForm)
  [form,catForm,e]
 
--% APPLY MODEMAPS

++ `op' has been selected as a viable candidate exported operation, 
++ for argument triple list `argTl', modemap `mm'.
++ Return the most refined implementation that makes the call successful.
compViableModemap(op,argTl,mm,e) ==
  [[dc,.,:margl],fnsel] := mm
  -- 1. Give up if the call is hopeless.
  argTl := [coerce(x,m) or return "failed" for x in argTl for m in margl]
  argTl = "failed" => nil

  -- 2.  obtain domain-specific function, if possible
  f := compMapCond(dc,fnsel,e) or return nil

  -- 3. Mark `f' as used.
  -- We can no longer trust what the modemap says for a reference into
  -- an exterior domain (it is calculating the displacement based on view
  -- information which is no longer valid; thus ignore this index and
  -- store the signature instead.
  f is [op1,.,.] and op1 in '(ELT CONST Subsumed) =>
    [genDeltaEntry(op,mm,e),argTl]
  [f,argTl]

compApplyModemap(form,modemap,$e) ==
  [op,:argl] := form                   --form to be compiled
  [[mc,mr,:margl],fnsel] := modemap    --modemap we are testing

  -- $e     is the current environment

  -- 0.  fail immediately if #argl=#margl
  if #argl~=#margl then return nil

  -- 1.  use modemap to evaluate arguments, returning failed if
  --     not possible
  lt:=
    [[.,.,$e]:= comp(y,m,$e) or return "failed"
            for y in argl for m in margl]
  lt="failed" => return nil

  -- 2. Select viable modemap implementation.
  compViableModemap(op,lt,modemap,$e)

compMapCond': (%Form,%Mode,%Env) -> %Boolean
compMapCond'(cexpr,dc,env) ==
  cexpr=true => true
  cexpr is ["AND",:l] => and/[compMapCond'(u,dc,env) for u in l]
  cexpr is ["OR",:l] => or/[compMapCond'(u,dc,env) for u in l]
  cexpr is ["not",u] => not compMapCond'(u,dc,env)
  cexpr is ["has",name,cat] => (knownInfo(cexpr,env) => true; false)
        --for the time being we'll stop here - shouldn't happen so far
        --$disregardConditionIfTrue => true
        --stackSemanticError(("not known that",'"%b",name,
        -- '"%d","has",'"%b",cat,'"%d"),nil)
  --now it must be an attribute
  listMember?(["ATTRIBUTE",dc,cexpr],get("$Information","special",$e)) => true
  --for the time being we'll stop here - shouldn't happen so far
  stackMessage('"not known that %1pb has %2pb",[dc,cexpr])
  false

compMapCond: (%Mode,%List %Code,%Env) -> %Code
compMapCond(dc,[cexpr,fnexpr],env) ==
  compMapCond'(cexpr,dc,env) => fnexpr
  stackMessage('"not known that %1pb has %2pb",[dc,cexpr])


--%

compResolveCall(op,argTs,m,$e) ==
  outcomes := 
    [t for mm in getModemapList(op,#argTs,$e) | t := tryMM] where
       tryMM() ==
         not coerceable(mm.mmTarget,m,$e) =>nil
         compViableModemap(op,argTs,mm,$e) isnt [f,Ts] => nil
         coerce([['%call,f,:[T.expr for T in Ts]],mm.mmTarget,$e],m)
  #outcomes ~= 1 => nil
  first outcomes

--% %Match

++ Subroutine of compAlternativeGuardItem, responsible of compiling 
++ individual alternative of the form 
++      x@t => stmt
++ in environment `e'.  Here `sn' is the temporary holding the
++ value of the scrutinee, and `sm' is its type.
++ Return a quadruple [init,guard,init',envTrue,envFalse], where
++   `init' is code that intializes the retract intermediate entity.
++   `guard' is code that gates the body of the alternative
++   `init'' is list of possible initializations local to the branch
++   `envTrue' is an environment after the guard evaluates to true
++   `envFalse' is an environment after the guard evaluates to false.
compRetractGuard(x,t,sn,sm,e) ==
  -- The retract pattern is compiled by transforming
  --     x@t => stmt
  -- into the following program fragment
  --   sn case t => (x := <expr>; stmt)
  -- where <expr> is code that computes appropriate initialization
  -- for `x' under the condition that either `sn' may be implicitly
  -- convertible to t (using only courtesy coercions) or that
  -- `sn' is retractable to t.
  --
  -- 1.  Evaluate the retract condition, and retract.
  initCode := nil
  caseCode := nil
  restrictCode := nil
  envFalse := e
  -- 1.1. Try courtesy coercions first.  That way we can use 
  --      optimized versions where available.  That also
  --      makes the scheme work for untagged unions.
  if testT := compPredicate(["case",sn,t],e) then
    [caseCode,.,e,envFalse] := testT
    [restrictCode,.,e] := 
      tryCourtesyCoercion([sn,sm,e],t) or
        comp(["retract",sn],t,e) or return 
          stackAndThrow('"Could not retract from %1bp to %2bp",[sm,t])
  -- 1.2. Otherwise try retractIfCan, for those `% has RetractableTo t'.
  else if retractT := comp(["retractIfCan",sn],["Union",t,'"failed"],e) then
    [retractCode,.,e] := retractT
    -- Assign this value to a temporary.  That temporary needs to
    -- have a lifetime that covers both the condition and the body
    -- of the alternative
    z := gensym()
    initCode := [[z,retractCode]]
    caseCode := ['%ieq,['%head,z],0]
    restrictCode := ["%tail",z]
  -- 1.3. Everything else failed; nice try.
  else return stackAndThrow('"%1bp is not retractable to %2bp",[sm,t])
  -- 2.  Now declare `x'.
  [.,.,e] := compMakeDeclaration(x,t,e) or return nil
  e := giveVariableSomeValue(x,t,e)
  -- 3.  Assemble result.
  [initCode,caseCode,[[x,restrictCode]],e,envFalse]


++ Subroutine of compRecoverGuard.  The parameters and the result
++ have the same meaning as in compRecoverGuard.
++ Note: a value of type Any is a dotted pair (dom . val) where
++       `dom' is a devaluated form of the domain of `val'.
compRecoverDomain(x,t,sn,e) ==
  -- 1. We recover domains only.
  not isDomainForm(t,e) =>
    stackAndThrow('"Form %1b does not designate a domain",[t])
  caseCode := ["%equal",["devaluate",t],["%head",sn]]
  -- 2. Declare `x'.
  originalEnv := e
  [.,.,e] := compMakeDeclaration(x,t,e) or return nil
  e := giveVariableSomeValue(x,t,e)
  -- 3.  Assemble the result
  [nil,caseCode,[[x,['%tail,sn]]],e,originalEnv]

++ Subroutine of compAlternativeGuardItem, responsible for 
++ compiling a guad item of the form
++    x: t
++ in environment `e', where `sn' is the temporary holding
++ the value of the scrutinee, and `sm' is its mode.
++ Return a quadruple [guard,init,envTrue,envFalse], where
++   `guard' is code that gates the body of the alternative
++   `init' is list of possible initializations 
++   `envTrue' is an environment after the guard evaluates to true
++   `envFalse' is an environment after the guard environment to false.
compRecoverGuard(x,t,sn,sm,e) ==
  -- The retract pattern is compiled by transforming
  --     x: t => stmt
  -- into the following program fragment
  --   domainOf y is t => (x := <expr>; stmt)
  -- where <expr> is code that compute appropriate initialization
  -- for `x' under the condition that sm is Any, and the
  -- underlying type is t.
  --
  -- 0. Type recovery is for expressions of type 'Any'.
  (sm is "$" => get('$,'%dc,e); sm) ~= $Any  =>
    stackAndThrow('"Scrutinee must be of type %1pb in type recovery alternative of case pattern",[$Any])
  -- 1. Do some preprocessing if this is existential type recovery.
  t is ["%Exist",var,t'] =>
    var isnt [":",var',cat'] =>
      stackAndThrow('"Sorry: Only univariate type schemes are supported in this context",nil)
    -- We have a univariate type scheme.  At the moment we insist
    -- that the body of the type scheme be identical to the type
    -- variable.  This restriction should be lifted in future work.
    not ident? t' or t' ~= var' =>
      stackAndThrow('"Sorry: type %1b too complex",[t'])
    not isCategoryForm(cat',e) =>
      stackAndThrow('"Expression %1b does not designate a category",[cat'])
    getmode(var',e) =>
      stackAndThrow('"You cannot redeclare identifier %1b",[var'])
    -- Extract the type component. 
    varDef := [":=",[":",var',$Type],
                [["elt",["Foreign","Builtin"],"evalDomain"],
                  [["elt",["Foreign","Builtin"],"%head"], sn]]]
    [def,.,e] := compOrCroak(varDef,$EmptyMode,e)
    [hasTest,.,e] := compOrCroak(["has",var',cat'],$EmptyMode,e)
    [defs',guard,inits,e,envFalse] := compRecoverDomain(x,var',sn,e)
    [[def.args,:defs'],hasTest,inits,e,envFalse]
  -- 2. Hand it to whoever is in charge.
  compRecoverDomain(x,t,sn,e)

warnUnreachableAlternative pat ==
  stackWarning('"Alternative with pattern %1b will not be reached",[pat])

warnTooManyOtherwise() ==
  stackWarning('"One too many `otherwise' alternative",nil)

++ Subroutine of compMatch.  Perform semantics analysis of the scrutinee
++ in a case-pattern.  Return a triple if everything is OK, otherwise nil.
compMatchScrutinee(form,e) ==
  form is ["%Comma",:exprs] =>
    Xs := Ms := nil
    for expr in exprs repeat
      [x,m,e] := compOrCroak(expr,$EmptyMode,e)
      Xs := [x,:Xs]
      Ms := [m,:Ms]
    [["%Comma",:reverse! Xs], ["%Cross",:reverse! Ms],e]
  compOrCroak(form,$EmptyMode,e)

++ Subroutine of compMatch.  We just finished semantics analysis of
++ the scrutinee.  Define temporary to hold the resulting value in store.
++ Returns declared temporaries if everything is fine, otherwise nil.
defineMatchScrutinee(m,e) ==
    m is ["%Cross",:.] =>
      [[t for m' in rest m | [t,e] := defTemp(m',e)], e]
    defTemp(m,e)
  where defTemp(m,e) ==
    t := gensym()
    [.,.,e] := compMakeDeclaration(t,m,e)
    [t,giveVariableSomeValue(t,m,e)]

++ Generate code for guard in a simple pattern where
++ `sn' is the name of the temporary holding the scrutinee value,
++ `sn' is its mode,
++ `pat' is the simple pattern being compiled.
++ On success, return a quintuple of the form [inits,guard,inits',eT,eF] where
++   inits is initialization to perform before the guard test.  This
++     iniialization extends to the corresponding branch of the pattern clause.
++   guard is the code for guard alternative
++   inits' is initialization to perform after the pattrn test.
++   eT is the environment for successful guard
++   eF is the environment for unsuccessful guard 
compAlternativeGuardItem(sn,sm,pat,e) ==
  pat is [op,x,t] and op in '(_: _@) =>
    not ident? x => 
      stackAndThrow('"pattern %1b must declare a variable",[pat])
    if $catchAllCount > 0 then
      warnUnreachableAlternative pat
    op is ":" => compRecoverGuard(x,t,sn,sm,e)
    compRetractGuard(x,t,sn,sm,e) or
       stackAndThrow('"cannot compile %1b",[pat])
  stackAndThrow('"invalid pattern %1b",[pat])

++ Subroutine of compMatchAlternative.  The parameters
++ have the same meaning.
++ Return value has same structure and semantics as compAlternativeGuardItem.
compAlternativeGuard(sn,sm,pat,e) ==
  pat = "otherwise" =>
    if $catchAllCount > 0 then
      warnTooManyOtherwise()
    $catchAllCount := $catchAllCount + 1
    [nil,'%otherwise,nil,e,e]
  cons? sn =>
    pat isnt ["%Comma",:.] =>
      stackAndThrow('"Pattern must be a tuple for a tuple scrutinee",nil)
    #sn ~= #rest pat =>
      stackAndThrow('"Tuple pattern must match tuple scrutinee in length",nil)
    inits := nil
    guards := nil
    inits' := nil
    ok := true
    originalEnv := e
    for n in sn for m in rest sm for p in rest pat while ok repeat
      [init,guard,init',e,.] := compAlternativeGuardItem(n,m,p,e) => 
        inits := [init,:inits]
        guards := [guard,:guards]
        inits' := [init',:inits']
      ok := false
    ok => [append/reverse! inits,
            ['%and,:reverse! guards],
              append/reverse! inits',
                e,originalEnv]
    nil
  compAlternativeGuardItem(sn,sm,pat,e)

++ Subroutine of compMatch. Analyze an alternative in a case-pattern.
++ `sn' is a name or a list of name for temporaries holding the
++      value of the scrutinee.
++ `sm' is the mode of list of modes for the scrutinee.
++ `pat' is the pattern of the alternative we are compiling
++ `stmt' is the body of the alternative we are compiling
++ `m' is the desired mode for the return value.
++ `e' is the environment in effect at the start of the environment.
++ Return a doublet with the first part being a 3-uple with components
++ as follows:
++    0. initialization code (if any) to run before performing the test
++    1. code for the guard
++    2. code to execute when the guard test succeeds.
++ and the second part being an environment to consider when
++ the guard test fails.
compMatchAlternative(sn,sm,pat,stmt,m,e) ==
  [inits,guard,inits',e,eF] := compAlternativeGuard(sn,sm,pat,e) or return nil
  stmtT := compOrCroak(stmt,m,e)
  body :=
    inits' = nil => stmtT.expr
    ['%bind,inits',stmtT.expr]
  [[inits,guard,body],eF]

++ Analyze and generate code for `case is'-pattern where the
++ scrutinee is `subject' and the alternatives are `altBlock'.
-- FIXME: Make sure nobody asks for creating matter out of void.
compMatch(["%Match",subject,altBlock],m,env) ==
  altBlock isnt ["%Block",:alts] => 
    stackAndThrow('"case pattern must specify block of alternatives",nil)
  savedEnv := env
  -- 1.  subjectTmp := subject
  [se,sm,env] := compMatchScrutinee(subject,env)
  [sn,env] := defineMatchScrutinee(sm,env)
  -- 2. compile alternatives.
  $catchAllCount: local := 0
  altsCode := nil
  for alt in alts repeat
     alt is ["=>",pat,stmt] => 
       [block,env] := compMatchAlternative(sn,sm,pat,stmt,m,env) or
             return stackAndThrow('"cannot compile pattern %1b",[pat])
       altsCode := [block,:altsCode]
     return stackAndThrow('"invalid alternative %1b",[alt])
  body := '%noBranch
  for [inits,guard,stmt] in altsCode repeat
     body := ['IF,guard,stmt,body]
     inits = nil => nil
     body := ['%bind,inits,body]
  $catchAllCount = 0 => 
    stackAndThrow('"missing %b otherwise %d alternative in case pattern",nil)
  inits := 
    ident? sn => [[sn,se]]
    [[n,e] for n in sn for e in rest se]
  [['%bind,inits,body],m,savedEnv]

++ Compile the form scheme `x'.
compScheme(x,m,e) ==
  stackSemanticError(["Sorry: Expression schemes are not supported in this context"],nil)

--%
--% Inline Requests
--%

++ We are processing a capsule and `t is nominated in an inline
++ directive.  This means that the compiler can `bypass' the usual 
++ indirect call through domain interface and attempt to resolve
++ modemap references.  
++ Concretely, this means that `t is evaluated.
processInlineRequest(t,e) ==
  T := compOrCroak(t,$EmptyMode,e)
  not isCategoryForm(T.mode,e) =>
    stackAndThrow('"%1b does not designate a domain",[t])
  T.expr isnt [.,:.] =>
    stackWarning('"inline request for type variable %1bp is meaningless",[t])
  registerInlinableDomain T.expr


--%
--% ITERATORS
--%
 
++ Generate code for collecting values generated by the expression `body'
++ controlled by iterators in `iters' into a list.
finishListCollect(iters,body) ==
  val := gensym()    -- result of the list comprehension
  -- Transform the body to build the list as we go.
  body := ['%store,val,['%pair,body,val]]
  -- Don't forget we built the result in reverse order.
  ['%repeat,:iters,['%init,val,'%nil],body,['%lreverse!,val]]

++ Generate code for collecting values generated by the expression `body'
++ controlled by iterators in `iters' into a vector with element
++ type indicated by `eltType'.
finishVectorCollect(eltType,iters,body) ==
  fromList := false      -- are we drawing from a list?
  vecSize := nil         -- size of vector
  index := nil           -- loop/vector index.
  for iter in iters while not fromList repeat
    [op,:.] := iter
    op in '(_| SUCHTHAT WHILE UNTIL) => fromList := true
    op in '(IN ON) => vecSize := [['%llength,third iter],:vecSize]
    op in '(STEP ISTEP) =>
      -- pick a loop variable that we can use as the loop index.
      [.,var,lo,inc,:etc] := iter
      if lo = 0 and inc = 1 then
        index :=
          var is [.,:var'] => var'
          var
      if [hi] := etc then
        sz :=
          inc = 1 =>
            lo = 1 => hi
            lo = 0 => ['%iinc,hi]
            ['%iinc,['%isub,hi,lo]]
          lo = 1 => ['%idiv,hi,inc]
          lo = 0 => ['%idiv,['%iinc,hi],inc]
          ['%idiv,['%isub,['%iinc,hi], lo],inc]
        vecSize := [sz, :vecSize]
    systemErrorHere ['finishVectorCollect, iter]
  -- if we draw from a list, then just build a list and convert to vector.
  fromList => 
    ['homogeneousListToVector,['getVMType,eltType],
      finishListCollect(iters,body)]
  vecSize = nil => systemErrorHere ['finishVectorCollect,eltType,iters,body]
  -- get the actual size of the vector.
  vecSize :=
    vecSize is [hi] => hi
    ['%imin,:reverse! vecSize]
  -- if no suitable loop index was found, introduce one.
  if index = nil then
    index := gensym()
    iters := [:iters,['STEP,index,0,1]]
  vec := gensym()
  ['%bind,[[vec,['makeSimpleArray,['getVMType,eltType],vecSize]]],
    ['%repeat,:iters,['setSimpleArrayEntry,vec,index,body],vec]]


compReduce(form,m,e) ==
 compReduce1(form,m,e,$formalArgList)

compReduce1(form is ["REDUCE",op,.,collectForm],m,e,$formalArgList) ==
  [collectOp,:itl,body] := collectForm
  if string? op then op := makeSymbol op
  collectOp ~= "COLLECT" => systemError ['"illegal reduction form:",form]
  $until: local := nil
  oldEnv := e
  itl := [([.,e]:= compIterator(x,e) or return "failed").0 for x in itl]
  itl="failed" => return nil
  b := gensym()          -- holds value of the body
  [bval,bmode,e] := comp([":=",b,body],$EmptyMode,e) or return nil
  accu := gensym()       -- holds value of the accumulator
  [move,.,e] := comp([":=",accu,b],$EmptyMode,e) or return nil
  move.op := '%store     -- in reality, we are not defining a new variable
  [update,mode,e] := comp([":=",accu,[op,accu,b]],m,e) or return nil
  update.op := '%store   -- just update the accumulation variable.
  nval :=
    id := getIdentity(op,e) => u.expr where
      u() == comp(id,mode,e) or return nil
    ["IdentityError",MKQ op]
  if $until then
    [untilCode,.,e]:= comp($until,$Boolean,e) or return nil
    itl := substitute(["UNTIL",untilCode],'$until,itl)
  firstTime := gensym()
  finalCode := ['%repeat,
                 ['%init,accu,'%nil],['%init,firstTime,'%true],:itl,
                   ['%bind,[[b,third bval]],
                     ['%seq, 
                       ['%when,[firstTime,move],['%otherwise,update]],
                         ['%store,firstTime,'%false]]],
                           ['%when,[firstTime,nval],['%otherwise,accu]]]
  T := coerce([finalCode,mode,e],m) or return nil
  [T.expr,T.mode,oldEnv]

++ returns the identity element of the `reduction' operation `x'
++ over a list -- a monoid homomorphism.   
getIdentity(x,e) ==
  property(x,"THETA") is [y] =>
    y = 0 => $Zero
    y = 1 => $One
    -- The empty list should be indicated by name, not  by its
    -- object representation.
    y => y
    "nil"
  nil
 
numberize x ==
  x=$Zero => 0
  x=$One => 1
  x isnt [.,:.] => x
  [numberize first x,:numberize rest x]

++ If there is a local reference to mode `m', return it.  
localReferenceIfThere(m,e) ==
  m is "$" => m
  idx := assocIndex(constructorDB currentConstructor e,m) => ['%tref,'$,idx]
  quote m

compRepeatOrCollect(form,m,e) ==
  fn(form,[m,:$exitModeStack],[#$exitModeStack,:$leaveLevelStack],$formalArgList
    ,e) where
      fn(form,$exitModeStack,$leaveLevelStack,$formalArgList,e) ==
        $until: local := nil
        $loopKind: local := nil
        $iterateCount: local := 0
        $loopBodyTag: local := nil
        $breakCount: local := 0
        oldEnv := e
        aggr := nil
        [$loopKind,:itl,body]:= form
        itl':=
          [([x',e]:= compIterator(x,e) or return "failed"; x') for x in itl]
        itl'="failed" => nil
        targetMode:= first $exitModeStack
        bodyMode:=
          $loopKind="COLLECT" =>
            targetMode = $EmptyMode => (aggr:=["List",$EmptyMode]; $EmptyMode)
            [aggr,u] := modeIsAggregateOf('List,targetMode,e) => u
            [aggr,u] := modeIsAggregateOf('PrimitiveArray,targetMode,e) =>
              $loopKind := "%CollectV"
              u
            [aggr,u] := modeIsAggregateOf('Vector,targetMode,e) =>
              $loopKind := "%CollectV"
              u
            stackMessage('"Invalid collect bodytype")
            return nil
            -- If we're doing a collect, and the type isn't conformable
            -- then we've boobed. JHD 26.July.1990
          -- ??? we hve a plain old loop; the return type should be Void
          $NoValueMode
        [body',m',e'] := compOrCroak(body,bodyMode,e) or return nil
        -- Massage the loop body if we have a structured jump.
        if $iterateCount > 0 then
           body' := ['%scope,$loopBodyTag,body']
        if $until then
          [untilCode,.,e']:= comp($until,$Boolean,e')
          itl':= substitute(["UNTIL",untilCode],'$until,itl')
        form':= 
           $loopKind = "%CollectV" => 
             finishVectorCollect(localReferenceIfThere(m',e'),itl',body')
           -- We are phasing out use of LISP macros COLLECT and REPEAT.
           $loopKind = "COLLECT" => finishListCollect(itl',body')
           ['%repeat,:itl',body','%nil]
        m'' := 
          aggr is [c,.] and c in '(List PrimitiveArray Vector) => [c,m']
          m'
        T := coerceExit([form',m'',e'],targetMode) or return nil
        -- iterator variables and other variables declared in
        -- in a loop are local to the loop.
        [T.expr,T.mode,oldEnv]
 
--constructByModemap([x,source,e],target) ==
--  u:=
--    [cexpr
--      for (modemap:= [map,cexpr]) in getModemapList("construct",1,e) | map is [
--        .,t,s] and modeEqual(t,target) and modeEqual(s,source)] or return nil
--  fn:= (or/[selfn for [cond,selfn] in u | cond=true]) or return nil
--  [['%call,fn,x],target,e]
 
++ Return the least Integer subdomain that can represent values
++ of both Integer subdomains denoted by the forms `x' and `y.
joinIntegerModes(x,y,e) ==
  isSubset(x,y,e) => y
  isSubset(y,x,e) => x
  $Integer

++ Given a for-loop iterator `x', return
++   a. its storage class
++   b. its name
++   c. an environment containing its declaration in case a type
++      was specified.
classifyIteratorVariable(x,e) == check(main(x,e),x) where
  main(x,e) ==
    x is [":",var,t] =>
      not ident? var => nil
      checkVariableName var
      t is 'local => ['%local,var,e]
      t is 'free => ['%free,var,e]
      [.,.,e] := compMakeDeclaration(var,t,e) => ['%local,var,e]
      nil
    ident? x =>
      checkVariableName x
      ['%local,x,e]
    nil
  check(x,y) ==
    x ~= nil => x
    stackAndThrow('"invalid loop variable %1bp",[y])

++ Subroutine of compStepIterator.
++ We are elaborating the STEP form of a for-iterator, where all
++ bounds and increment are expected to be integer-valued expressions.
++ Compile the expression `x' in the context `e', under those
++ circumstances.  When successful we return either the declared
++ mode of the expression, or infer the tightest mode that can
++ represents the resulting value.  Note that we do not attempt any
++ SmallInteger optimization at this stage.  Such a transformation can
++ be done only when we have all information about the bound.
compIntegerValue(x,e) ==
  -- 1. Preliminary transformation.
  -- The literal values 0 and 1 get transformed by the parser
  -- into calls Zero() and One(), respectively.  Undo that transformation
  -- locally.  Note that this local transformation is OK, because
  -- it presents semantics.
  x :=
    x = $Zero => 0
    x = $One => 1
    x
  -- 2. Attempt to infer the type of the expression if at all possible.
  --    The inferred mode is valid only if it is an integer (sub)domain.
  T := comp(x,$EmptyMode,e)
  isSubset(T.mode,$Integer,e) => T
  -- 3. Otherwise, compile in checking mode.
  comp(x,$PositiveInteger,e) or
    comp(x,$NonNegativeInteger,e) or
      compOrCroak(x,$Integer,e)

++ Attempt to compile a `for' iterator of the form
++     for index in start..final by inc
++ where the bound `final' may be missing.
compStepIterator(index,start,final,inc,e) ==
  [sc,index,e] := classifyIteratorVariable(index,e)
  if sc = '%local then
    $formalArgList := [index,:$formalArgList]
  [start,startMode,e] := compIntegerValue(start,e) or return
    stackMessage('"start value of index: %1b must be an integer",[start])
  [inc,incMode,e] := compIntegerValue(inc,e) or return
    stackMessage('"index increment: %1b must be an integer",[inc])
  if final ~= nil then
    [final,finalMode,e] := compIntegerValue(first final,e) or return
      stackMessage('"final value of index: %1b must be an integer",[final])
    final := [final]
  indexMode :=
    final = nil or isSubset(incMode,$NonNegativeInteger,e) => startMode
    joinIntegerModes(startMode,finalMode,e)
  if get(index,"mode",e) = nil then
    [.,.,e] := compMakeDeclaration(index,indexMode,e) or return nil
  e := giveVariableSomeValue(index,indexMode,e)
  [["STEP",[sc,:index],start,inc,:final],e]
 
compINIterator(x,y,e) ==
  [sc,x,e] := classifyIteratorVariable(x,e)
  --these two lines must be in this order, to get "for f in list f"
  --to give  an error message if f is undefined
  [y',m,e]:= comp(y,$EmptyMode,e) or return nil
  if sc = '%local then
    $formalArgList := [x,:$formalArgList]
  [mOver,mUnder]:=
    modeIsAggregateOf("List",m,e) or return
       stackMessage('"mode: %1pb must be a list of some mode",[m])
  if null get(x,"mode",e) then [.,.,e]:=
    compMakeDeclaration(x,mUnder,e) or return nil
  e:= giveVariableSomeValue(x,mUnder,e)
  [y'',m'',e] := coerce([y',m,e], mOver) or return nil
  [["IN",[sc,:x],y''],e]

compONIterator(x,y,e) ==
  [sc,x,e] := classifyIteratorVariable(x,e)
  if sc = '%local then
    $formalArgList := [x,:$formalArgList]
  [y',m,e]:= comp(y,$EmptyMode,e) or return nil
  [mOver,mUnder]:=
    modeIsAggregateOf("List",m,e) or return
      stackMessage('"mode: %1pb must be a list of other modes",[m])
  if null get(x,"mode",e) then [.,.,e]:=
    compMakeDeclaration(x,m,e) or return nil
  e:= giveVariableSomeValue(x,m,e)
  [y'',m'',e] := coerce([y',m,e], mOver) or return nil
  [["ON",[sc,:x],y''],e]

compIterator(it,e) ==
    -- ??? Allow for declared iterator variable.
  it is ["IN",x,y] => compINIterator(x,y,e)
  it is ["ON",x,y] => compONIterator(x,y,e)
  it is ["STEP",index,start,inc,:optFinal] =>
    compStepIterator(index,start,optFinal,inc,e)
  it is ["WHILE",p] =>
    [p',m,e]:=
      comp(p,$Boolean,e) or return
        stackMessage('"WHILE operand: %1b is not Boolean valued",[p])
    [["WHILE",p'],e]
  it is ["UNTIL",p] => ($until:= p; ['$until,e])
  it is ["|",x] =>
    u:=
      comp(x,$Boolean,e) or return
        stackMessage('"SUCHTHAT operand: %1b is not Boolean value",[x])
    [["|",u.expr],u.env]
  nil
 
--isAggregateMode(m,e) ==
--  m is [c,R] and c in '(Vector List) => R
--  name:=
--    m is [fn,:.] => fn
--    m="$" => "Rep"
--    m
--  get(name,"value",e) is [c,R] and c in '(Vector List) => R
 
modeIsAggregateOf(agg,m,e) ==
  m is [ =agg,R] => [m,R]
  m is ["Union",:l] =>
    mList:= [pair for m' in l | (pair:= modeIsAggregateOf(agg,m',e))]
    1=#mList => first mList
  name:=
    m is [fn,:.] => fn
    RepIfRepHack m
  get(name,"value",e) is [[ =agg,R],:.] => [m,R]
 

--% rep/per morphisms

++ Compile the form `per x' under the mode `m'.
++ The `per' operator is active only for new-style definition for
++ representation domain.
compPer(["per",x],m,e) ==
  $useRepresentationHack => nil
  inType := getRepresentation e or return nil
  T := comp(x,inType,e) or return nil
  if $subdomain then
    T := 
      integer? T.expr and satisfies(T.expr,domainVMPredicate "$") => 
        [T.expr,"$",e]
      coerceSuperset(T,"$") or return nil
  else 
    T.rest.first := "$"
  coerce(T,m)

++ Compile the form `rep x' under the mode `m'.
++ Like `per', the `rep' operator is active only for new-style 
++ definition for representation domain.
compRep(["rep",x],m,e) ==
  $useRepresentationHack => nil
  T := comp(x,"$",e) or return nil
  T.rest.first := getRepresentation e or return nil
  coerce(T,m)

--% Lambda expressions

compUnnamedMapping(parms,source,target,body,env) ==
  savedEnv := env
  for p in parms for s in source repeat
    [.,.,env] := compMakeDeclaration(p,s,env)
    env := giveVariableSomeValue(p,get(p,'mode,env),env)
  T := comp(body,target,env) or return nil
  fun := ['%closure,['%function,['%lambda,[:parms,'$],T.expr]],'$]
  [fun,["Mapping",T.mode,:source],savedEnv]

gatherParameterList vars == main(vars,nil,nil) where
  main(vars,parms,source) ==
    vars = nil => [reverse! parms,reverse! source]
    vars isnt [.,:.] or vars is [":",:.] => [[x] for x in check vars]
    [v,s] := check first vars
    main(rest vars,[v,:parms],[s,:source])
  check var == 
    var isnt [.,:.] =>
      not ident? var =>
        stackAndThrow('"invalid parameter %1b in lambda expression",[var])
      [checkVariableName var,nil]
    var is [":",p,t] =>
      not ident? p =>
        stackAndThrow('"invalid parameter %1b in lambda expression",[p])
      [checkVariableName p,t]
    stackAndThrow('"invalid parameter for mapping",nil)

compLambda(x is ["+->",vars,body],m,e) ==
  -- 1. Gather parameters and their types.
  if vars is ["%Comma",:vars'] then
    vars := vars'
  [parms,source] := gatherParameterList vars
  -- 2. Compile the form
  T := 
    -- 2.1. No parameter is declared
    and/[s = nil for s in source] =>
      -- Guess from context
      m is ["Mapping",dst,:src] =>
        #src ~= #parms =>
           stackAndThrow('"inappropriate function type for unnamed mapping",nil)
        compUnnamedMapping(parms,src,dst,body,e) or return nil
      -- Otherwise, assumes this is just purely syntactic code block.
      [quote ["+->",parms,body],$AnonymousFunction,e]
    -- 2.2. If all parameters are declared, then compile as a mapping.
    and/[s ~= nil for s in source] =>
      compUnnamedMapping(parms,source,$EmptyMode,body,e) or return nil
    -- 2.3.  Well, give up for now.
    stackAndThrow('"parameters in a lambda expression must be all declared or none declared",nil)
  coerce(T,m)

--%
--% Entry point to the compiler
--%

preprocessParseTree pt ==
  $postStack := []
  pf := parseTransform postTransform pt
  $postStack = nil => pf
  displayPreCompilationErrors()
  nil

++ Takes a parse tree `pt', typecheck it and compile it down 
++ to VM instructions.
compileParseTree pt ==
  pt = nil => nil
  CURSTRM: local := $OutputStream
  pf := preprocessParseTree pt
  pf = nil => nil       -- stop if preprocessing was a disaster.
  -- Don't go further if only preprocessing was requested.
  $PrintOnly =>
    formatToStdout('"~S   =====>~%",$currentLine)
    PRETTYPRINT pf
  -- Now start actual compilation.
  $x: local := nil         -- ???
  $m: local := nil         -- ???
  $s: local := nil         -- ???
  $exitModeStack: local := []    -- Used by the compiler proper
  -- We don't usually call the compiler to process interpreter
  -- input, however attempt to second guess nevertheless.
  if $InteractiveMode then
    processInteractive(pf,nil)
  else if T := compTopLevel(pf,$EmptyMode,$InteractiveFrame) then
    [.,.,$InteractiveFrame] := T
  finishLine $OutputStream


--%
--% Register compilers for special forms.
-- Those compilers are on the `SPECIAL' property of the corresponding
-- special form operator symbol.
for x in [["|", :"compSuchthat"],_
	  ["@", :"compAtSign"],_
	  [":", :"compColon"],_
	  ["::", :"compCoerce"],_
          ["+->", :"compLambda"],_
	  ["QUOTE", :"compQuote"],_
	  ["add", :"compAdd"],_
	  ["CAPSULE", :"compCapsule"],_
	  ["case", :"compCase"],_
	  ["CATEGORY", :"compCategory"],_
	  ["COLLECT", :"compRepeatOrCollect"],_
	  ["CONS", :"compCons"],_
	  ["construct", :"compConstruct"],_
	  ["DEF", :"compDefine"],_
	  ["elt", :"compElt"],_
	  ["Enumeration", :"compBuiltinDomain"],_
          ["EnumerationCategory", :"compEnumCat"],_
	  ["exit", :"compExit"],_
	  ["has", :"compHas"],_
	  ["IF", : "compIf"],_
          ["xor",: "compExclusiveOr"],_
	  ["import", :"compImport"],_
	  ["is", :"compIs"],_
	  ["Join", :"compJoin"],_
	  ["leave", :"compLeave"],_
	  [":=", :"compSetq"],_
	  ["MDEF", :"compMacro"],_
          ["not", :"compLogicalNot"],_
	  ["pretend", :"compPretend"],_
	  ["Record", :"compBuiltinDomain"],_
	  ["RecordCategory", :"compConstructorCategory"],_
	  ["REDUCE", :"compReduce"],_
	  ["REPEAT", :"compRepeatOrCollect"],_
	  ["return", :"compReturn"],_
	  ["SEQ", :"compSeq"],_
	  ["SubDomain", :"compSubDomain"],_
	  ["SubsetCategory", :"compSubsetCategory"],_
	  ["Mapping", :"compBuiltinDomain"],_
          ["MappingCategory", :"compConstructorCategory"],_
	  ["Union", :"compBuiltinDomain"],_
	  ["UnionCategory", :"compConstructorCategory"],_
	  ["where", :"compWhere"],_
          ["per",:"compPer"],_
          ["rep",:"compRep"],_
          ["%Comma",:"compComma"],_
          ["%Exist", : "compScheme"] , _
          ["%Forall", : "compSceheme"] , _
          ["%Match",:"compMatch"],_
          ["%SignatureImport",:"compSignatureImport"],_
          ['%Throw,:'compThrow],
          ['%Try, :'compTry],
          ["[||]", :"compileQuasiquote"]] repeat
  property(first x, 'SPECIAL) := rest x
