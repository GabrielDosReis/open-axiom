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


import debug
namespace BOOT

--% Code for tracing functions

-- This code supports the )trace system command and allows the
-- tracing of LISP, BOOT and SPAD functions and interpreter maps.

$traceNoisely := nil  -- give trace and untrace messages

$reportSpadTrace := nil  -- reports traced funs

$optionAlist := nil

$tracedMapSignatures := nil

$traceOptionList == '(
    after _
    before _
    break_
    cond_
    count_
    depth_
    local_
    mathprint _
    nonquietly_
    nt_
    of_
    only_
    ops_
    restore_
    timer_
    varbreak _
    vars_
    within _
    )


$lastUntraced := nil

SETLETPRINTFLAG x == x

trace l == traceSpad2Cmd l

traceSpad2Cmd l ==
  if l is ["%Comma", l1] then l := l1
  $mapSubNameAlist:= getMapSubNames(l)
  trace1 augmentTraceNames(l,$mapSubNameAlist)
  traceReply()

trace1 l ==
  $traceNoisely: local := nil
  if hasOption($options,'nonquietly) then $traceNoisely := true
  hasOption($options,'off) =>
    (ops := hasOption($options,'ops)) or
      (lops := hasOption($options,'local)) =>
        null l => throwKeyedMsg("S2IT0019",nil)
        constructor := unabbrev
          l isnt [.,:.] => l
          null rest l =>
            first l isnt [.,:.] => first l
            first first l
          nil
        not(isFunctor constructor) => throwKeyedMsg("S2IT0020",nil)
        if ops then
          ops := getTraceOption ops
          nil
        if lops then
          lops := rest getTraceOption lops
          untraceDomainLocalOps(constructor,lops)
    (1 < # $options) and not hasOption($options,'nonquietly) =>
      throwKeyedMsg("S2IT0021",nil)
    untrace l
  hasOption($options,'stats) =>
    (1 < # $options) =>
      throwKeyedMsg("S2IT0001",['")trace ... )stats"])
    [.,:opt] := first $options
    -- look for )trace )stats       to list the statistics
    --          )trace )stats reset to reset them
    null opt =>      -- list the statistics
      centerAndHighlight('"Traced function execution times",78,"-")
      ptimers ()
      SAY '" "
      centerAndHighlight('"Traced function execution counts",78,"-")
      pcounters ()
    selectOptionLC(first opt,'(reset),'optionError)
    resetSpacers()
    resetTimers()
    resetCounters()
    throwKeyedMsg("S2IT0002",nil)
  a:= hasOption($options,'restore) =>
    null(oldL:= $lastUntraced) => nil
    newOptions:= remove($options,a)
    null l => trace1 oldL
    for x in l repeat
      x is [domain,:opList] and vector? domain =>
        sayKeyedMsg("S2IT0003",[devaluate domain])
      $options:= [:newOptions,:LASSOC(x,$optionAlist)]
      trace1 [x]
  null l => nil
  l is ["?"] => _?t()
  traceList:= [transTraceItem x for x in l] or return nil
  for x in traceList repeat $optionAlist:=
    ADDASSOC(x,$options,$optionAlist)
  optionList:= getTraceOptions $options
  argument:=
    domainList := symbolTarget("of",optionList) =>
      symbolTarget("ops",optionList) =>
        throwKeyedMsg("S2IT0004",nil)
      opList:=
        traceList => [["ops",:traceList]]
        nil
      varList:=
        y := symbolTarget("vars",optionList) => [["vars",:y]]
        nil
      [:domainList,:opList,:varList]
    optionList => [:traceList,:optionList]
    traceList
  _/TRACE_,0 [funName for funName in argument]
  saveMapSig [funName for funName in argument]

getTraceOptions options ==
  $traceErrorStack: local := nil
  optionList:= [getTraceOption x for x in options]
  $traceErrorStack =>
    null rest $traceErrorStack =>
      [key,parms] := first $traceErrorStack
      throwKeyedMsg(key,['"",:parms])
    throwListOfKeyedMsgs("S2IT0017",[# $traceErrorStack],
      reverse! $traceErrorStack)
  optionList

saveMapSig(funNames) ==
  for name in funNames repeat
    map:= rassoc(name,$mapSubNameAlist) =>
      $tracedMapSignatures:= ADDASSOC(name,getMapSig(map,name),
        $tracedMapSignatures)

getMapSig(mapName,subName) ==
  lmms:= get(mapName,'localModemap,$InteractiveFrame) =>
    for mm in lmms until sig repeat
      second mm = subName => sig := mm.mmSignature
    sig

getTraceOption (x is [key,:l]) ==
  key:= selectOptionLC(key,$traceOptionList,'traceOptionError)
  x := [key,:l]
  key in '(nonquietly timer nt) => x
  key='break =>
    null l => ['break,'before]
    opts := [selectOptionLC(y,'(before after),nil) for y in l]
    and/[ident? y for y in opts] => ['break,:opts]
    stackTraceOptionError ["S2IT0008",nil]
  key='restore =>
    null l => x
    stackTraceOptionError ["S2IT0009",[strconc('")",object2String key)]]
  key='only => ['only,:transOnlyOption l]
  key='within =>
    l is [a] and ident? a => x
    stackTraceOptionError ["S2IT0010",['")within"]]
  key in '(cond before after) =>
    key:=
      key="cond" => "when"
      key
    l is [a] => [key,:l]
    stackTraceOptionError ["S2IT0011",[strconc('")",object2String key)]]
  key='depth =>
    l is [n] and integer? n => x
    stackTraceOptionError ["S2IT0012",['")depth"]]
  key='count =>
    (null l) or (l is [n] and integer? n) => x
    stackTraceOptionError ["S2IT0012",['")count"]]
  key="of" =>
    ["of",:[hn y for y in l]] where
      hn x ==
        x isnt [.,:.] and not upperCase? STRINGIMAGE(x).0 =>
          isDomainOrPackage eval x => x
          stackTraceOptionError ["S2IT0013",[x]]
        g:= domainToGenvar x => g
        stackTraceOptionError ["S2IT0013",[x]]
  key in '(local ops vars) =>
    null l or l is ["all"] => [key,:"all"]
    isListOfIdentifiersOrStrings l => x
    stackTraceOptionError ["S2IT0015",[strconc('")",object2String key)]]
  key='varbreak =>
    null l or l is ["all"] => ["varbreak",:"all"]
    isListOfIdentifiers l => x
    stackTraceOptionError ["S2IT0016",[strconc('")",object2String key)]]
  key='mathprint =>
    null l => x
    stackTraceOptionError ["S2IT0009",[strconc('")",object2String key)]]
  key => throwKeyedMsg("S2IT0005",[key])

traceOptionError(opt,keys) ==
  null keys => stackTraceOptionError ["S2IT0007",[opt]]
  commandAmbiguityError("trace option",opt,keys)

resetTimers () ==
  for timer in _/TIMERLIST repeat
    symbolValue(makeSymbol strconc(timer,'"_,TIMER")) := 0

resetSpacers () ==
  for spacer in _/SPACELIST repeat
    symbolValue(makeSymbol strconc(spacer,'"_,SPACE")) := 0

resetCounters () ==
  for k in _/COUNTLIST repeat
    symbolValue(makeSymbol strconc(k,'"_,COUNT")) := 0

ptimers() ==
  null _/TIMERLIST => sayBrightly '"   no functions are timed"
  for timer in _/TIMERLIST repeat
    sayBrightly ["  ",:bright timer,'_:,'" ",
      eval(makeSymbol strconc(timer,'"_,TIMER")) / float $timerTicksPerSecond,'" sec."]

pspacers() ==
  null _/SPACELIST => sayBrightly '"   no functions have space monitored"
  for spacer in _/SPACELIST repeat
    sayBrightly ["  ",:bright spacer,'_:,'" ",
      eval makeSymbol strconc(spacer,'"_,SPACE"),'" bytes"]

pcounters() ==
  null _/COUNTLIST => sayBrightly '"   no functions are being counted"
  for k in _/COUNTLIST repeat
    sayBrightly ["  ",:bright k,'_:,'" ",
      eval makeSymbol strconc(k,'"_,COUNT"),'" times"]

transOnlyOption l ==
  l is [n,:y] =>
    integer? n => [n,:transOnlyOption y]
    symbolMember?(n:= UPCASE n,'(V A C)) => [n,:transOnlyOption y]
    stackTraceOptionError ["S2IT0006",[n]]
    transOnlyOption y
  nil

stackTraceOptionError x ==
  $traceErrorStack:= [x,:$traceErrorStack]
  nil

removeOption(op,options) ==
  [optEntry for (optEntry:=[opt,:.]) in options | opt ~= op]

domainToGenvar x ==
  $doNotAddEmptyModeIfTrue: local:= true
  (y:= unabbrevAndLoad x) and getConstructorKindFromDB opOf y = "domain" =>
    g:= genDomainTraceName y
    symbolValue(g) := evalDomain y
    g

genDomainTraceName y ==
  u:= LASSOC(y,$domainTraceNameAssoc) => u
  g:= GENVAR()
  $domainTraceNameAssoc:= [[y,:g],:$domainTraceNameAssoc]
  g

--this is now called from trace with the )off option
untrace l ==
  $lastUntraced:=
    null l => copyTree _/TRACENAMES
    l
  untraceList:= [transTraceItem x for x in l]
  _/UNTRACE_,0 [lassocSub(funName,$mapSubNameAlist) for
      funName in untraceList]
  removeTracedMapSigs untraceList

transTraceItem x ==
  $doNotAddEmptyModeIfTrue: local:=true
  x isnt [.,:.] =>
    (value:=get(x,"value",$InteractiveFrame)) and
      member(objMode value,$LangSupportTypes) =>
        x := objVal value
        (y:= domainToGenvar x) => y
        x
    upperCase? STRINGIMAGE(x).0 =>
      y := opOf unabbrev x
      constructor? y => y
      (y:= domainToGenvar x) => y
      x
    x
  vector? first x => transTraceItem devaluate first x
  y:= domainToGenvar x => y
  throwKeyedMsg("S2IT0018",[x])

removeTracedMapSigs untraceList ==
  for name in untraceList repeat
    property(name,$tracedMapSignatures) := nil

coerceTraceArgs2E(traceName,subName,args) ==
  symbolMember?(name:= subName,$mathTraceList) =>
    SPADSYSNAMEP PNAME name => coerceSpadArgs2E(reverse rest reverse args)
    [["=",name,objValUnwrap coerceInteractive(objNewWrap(arg,type),$OutputForm)]
      for name in '(arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 )
       for arg in args for type in rest LASSOC(subName,
        $tracedMapSignatures)]
  SPADSYSNAMEP PNAME name => reverse rest reverse args
  args

coerceSpadArgs2E(args) ==
  -- following binding is to prevent forcing calculation of stream elements
  $streamCount:local := 0
  [["=",name,objValUnwrap coerceInteractive(objNewWrap(arg,type),$OutputForm)]
      for name in '(arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 )
        for arg in args for type in rest $tracedSpadModemap]

subTypes(mm,sublist) ==
  mm isnt [.,:.] =>
    (s:= LASSOC(mm,sublist)) => s
    mm
  [subTypes(m,sublist) for m in mm]

coerceTraceFunValue2E(traceName,subName,value) ==
  symbolMember?(name:= subName,$mathTraceList) =>
    SPADSYSNAMEP PNAME traceName => coerceSpadFunValue2E(value)
    (u:=LASSOC(subName,$tracedMapSignatures)) =>
      objValUnwrap coerceInteractive(objNewWrap(value,first u),$OutputForm)
    value
  value

coerceSpadFunValue2E(value) ==
  -- following binding is to prevent forcing calculation of stream elements
  $streamCount:local := 0
  objValUnwrap coerceInteractive(objNewWrap(value,first $tracedSpadModemap),
    $OutputForm)

isListOfIdentifiers l == and/[ident? x for x in l]

isListOfIdentifiersOrStrings l == and/[ident? x or string? x for x in l]

getMapSubNames(l) ==
  subs:= nil
  for mapName in l repeat
    lmm:= get(mapName,'localModemap,$InteractiveFrame) =>
      subs:= append([[mapName,:second mm] for mm in lmm],subs)
  setUnion(subs,getPreviousMapSubNames setUnion(_/TRACENAMES,
    $lastUntraced))

getPreviousMapSubNames(traceNames) ==
  subs:= nil
  for mapName in ASSOCLEFT CAAR $InteractiveFrame repeat
    lmm:= get(mapName,'localModemap,$InteractiveFrame) =>
      symbolMember?(CADAR lmm,traceNames) =>
        for mm in lmm repeat
          subs:= [[mapName,:second mm],:subs]
  subs

lassocSub(x,subs)  ==
  y := symbolTarget(x,subs) => y
  x

rassocSub(x,subs) ==
  y:= rassoc(x,subs) => y
  x

isUncompiledMap(x) ==
  y:= get(x,'value,$InteractiveFrame) =>
    (CAAR y) = "%Map" and null get(x,'localModemap,$InteractiveFrame)

isInterpOnlyMap(map) ==
  x:= get(map,'localModemap,$InteractiveFrame) =>
    (CAAAR x) = 'interpOnly

augmentTraceNames(l,mapSubNames) ==
  res:= nil
  for traceName in l repeat
    mml:= get(traceName,'localModemap,$InteractiveFrame) =>
      res:= append([second mm for mm in mml],res)
    res:= [traceName,:res]
  res

isSubForRedundantMapName(subName) ==
  mapName:= rassocSub(subName,$mapSubNameAlist) =>
    tail:=member([mapName,:subName],$mapSubNameAlist) =>
      symbolMember?(mapName,rest ASSOCLEFT tail)

untraceMapSubNames traceNames ==
  null($mapSubNameAlist:local:= getPreviousMapSubNames traceNames) => nil
  for name in (subs:= ASSOCRIGHT $mapSubNameAlist)
    | symbolMember?(name,_/TRACENAMES) repeat
      _/UNTRACE_,2(name,nil)
      $lastUntraced := setDifference($lastUntraced,subs)

funfind("functor","opname") ==
  ops:= isFunctor functor
  [u for u in ops | u is [[ =opname,:.],:.]]

isDomainOrPackage dom ==
  vector? dom and #dom>0 and isFunctor opOf dom.0

isTraceGensym x == gensym? x

spadTrace(domain,options) ==
  $fromSpadTrace:= true
  $tracedModemap:local:= nil
  cons? domain and vector? first domain and (first domain).0 = 0 =>
      aldorTrace(domain,options)
  not isDomainOrPackage domain => userError '"bad argument to trace"
  listOfOperations:=
    [g x for x in getOption("OPS",options)] where
      g x ==
        string? x => makeSymbol x
        x
  if listOfVariables := getOption("VARS",options) then
    options := removeOption("VARS",options)
  if listOfBreakVars := getOption("VARBREAK",options) then
    options := removeOption("VARBREAK",options)
  anyifTrue:= null listOfOperations
  domainId:= opOf domain.0
  currentEntry:= assoc(domain,_/TRACENAMES)
  currentAlist:= KDR currentEntry
  opStructureList :=
    flattenOperationAlist getConstructorOperationsFromDB domainId
  sigSlotNumberAlist:=
    [triple
      --new form is (<op> <signature> <slotNumber> <condition> <kind>)
      for [op,sig,n,.,kind] in opStructureList | kind = 'ELT
        and (anyifTrue or symbolMember?(op,listOfOperations)) and
         integer? n and
          isTraceable(triple:= [op,sig,n],domain)] where
            isTraceable(x is [.,.,n,:.],domain) ==
              domain.n  isnt [.,:.] => nil
              functionSlot:= first domain.n
              gensym? functionSlot =>
                (reportSpadTrace("Already Traced",x); nil)
              null (BPINAME functionSlot) =>
                (reportSpadTrace("No function for",x); nil)
              true
  if listOfVariables then
    for [.,.,n] in sigSlotNumberAlist repeat
      fn := first domain.n
      $letAssoc := AS_-INSERT(BPINAME fn,
        listOfVariables,$letAssoc)
  if listOfBreakVars then
    for [.,.,n] in sigSlotNumberAlist repeat
      fn := first domain.n
      $letAssoc := AS_-INSERT(BPINAME fn,
        [["BREAK",:listOfBreakVars]],$letAssoc)
  for (pair:= [op,mm,n]) in sigSlotNumberAlist repeat
    alias:= spadTraceAlias(domainId,op,n)
    $tracedModemap:= subTypes(mm,constructSubst(domain.0))
    traceName:= BPITRACE(first domain.n,alias, options)
    append!(pair,[listOfVariables,first domain.n,traceName,alias])
    domain.n.first := traceName
  sigSlotNumberAlist:= [x for x in sigSlotNumberAlist | CDDDR x]
  if $reportSpadTrace then
    if $traceNoisely then printDashedLine()
    for x in orderBySlotNumber sigSlotNumberAlist repeat
      reportSpadTrace("TRACING",x)
  if $letAssoc then SETLETPRINTFLAG true
  currentEntry =>
    currentEntry.rest := [:sigSlotNumberAlist,:currentAlist]
  SETQ(_/TRACENAMES,[[domain,:sigSlotNumberAlist],:_/TRACENAMES])
  spadReply()

traceDomainLocalOps(dom,lops,options) ==
 sayMSG ['"  ",'"The )local option has been withdrawn"]
 sayMSG ['"  ",'"Use )ltr to trace local functions."]
 nil
--  abb := abbreviate dom
--  loadLibIfNotLoaded abb
--  actualLops := getLocalOpsFromLisplib abb
--  null actualLops =>
--    sayMSG ['"  ",:bright abb,'"has no local functions to trace."]
--  lops = 'all => _/TRACE_,1(actualLops,options)
--  l := nil
--  for lop in lops repeat
--    internalName := makeSymbol strconc(PNAME abb,'";",PNAME lop)
--    not symbolMember?(internalName,actualLops) =>
--      sayMSG ['"  ",:bright abb,'"does not have a local",
--        '" function called",:bright lop]
--    l := [internalName,:l]
--  l => _/TRACE_,1(l,options)
--  nil

untraceDomainLocalOps(dom,lops) ==
 abb := abbreviate dom
 sayMSG ['"  ",:bright abb,'"has no local functions to untrace."]
 nil
--  lops = "all" => untraceAllDomainLocalOps(dom)
--  abb := abbreviate dom
--  loadLibIfNotLoaded abb
--  actualLops := getLocalOpsFromLisplib abb
--  null actualLops =>
--    sayMSG ['"  ",:bright abb,'"has no local functions to untrace."]
--  l := nil
--  for lop in lops repeat
--    internalName := makeSymbol strconc(PNAME abb,'";",PNAME lop)
--    not symbolMember?(internalName,actualLops) =>
--      sayMSG ['"  ",:bright abb,'"does not have a local",
--        '" function called",:bright lop]
--    l := [internalName,:l]
--  l => untrace l
--  nil

untraceAllDomainLocalOps(dom) == nil
--  abb := abbreviate dom
--  actualLops := getLocalOpsFromLisplib abb
--  null (l := setIntersection(actualLops,_/TRACENAMES)) => nil
--  _/UNTRACE_,1(l,nil)
--  nil

traceDomainConstructor(domainConstructor,options) ==
  -- Trace all domains built with the given domain constructor,
  -- including all presently instantiated domains, and all future
  -- instantiations, while domain constructor is traced.
  loadLibIfNotLoaded domainConstructor
  listOfLocalOps := getOption("LOCAL",options)
  if listOfLocalOps then
    traceDomainLocalOps(domainConstructor,listOfLocalOps,
      [opt for opt in options | opt isnt ['LOCAL,:.]])
  listOfLocalOps and not getOption("OPS",options) => nil
  for [argl,.,:domain] in tableValue($ConstructorCache,domainConstructor)
    repeat spadTrace(domain,options)
  SETQ(_/TRACENAMES,[domainConstructor,:_/TRACENAMES])
  innerDomainConstructor := makeSymbol strconc(domainConstructor,'";")
  if functionSymbol? innerDomainConstructor then domainConstructor := innerDomainConstructor
  EMBED(domainConstructor,
    ['LAMBDA, ['_&REST, 'args],
      ['PROG, ['domain],
        ['SETQ,'domain,['APPLY,domainConstructor,'args]],
        ['spadTrace,'domain,MKQ options],
        ['RETURN,'domain]]] )

untraceDomainConstructor domainConstructor ==
  --untrace all the domains in domainConstructor, and unembed it
  SETQ(_/TRACENAMES, 
    [df for df in _/TRACENAMES | keepTraced?(df, domainConstructor)]) where 
      keepTraced?(df, domainConstructor) ==
        (df is [dc,:.]) and (isDomainOrPackage dc) and 
           ((KAR devaluate dc) = domainConstructor) =>
               _/UNTRACE_,0 [dc]
               false
        true
  untraceAllDomainLocalOps domainConstructor
  innerDomainConstructor := makeSymbol strconc(domainConstructor,'";")
  if functionSymbol? innerDomainConstructor then UNEMBED innerDomainConstructor
    else UNEMBED domainConstructor
  SETQ(_/TRACENAMES,removeSymbol(_/TRACENAMES,domainConstructor))

flattenOperationAlist(opAlist) ==
   res:= nil
   for [op,:mmList] in opAlist repeat
     res:=[:res,:[[op,:mm] for mm in mmList]]
   res

mapLetPrint(x,val,currentFunction) ==
  x:= getAliasIfTracedMapParameter(x,currentFunction)
  currentFunction:= getBpiNameIfTracedMap currentFunction
  letPrint(x,val,currentFunction)

-- This is the version for use when we have no idea
-- what print representation to use for the data object

letPrint(x,val,currentFunction) ==
  if $letAssoc and
    ((y:= LASSOC(currentFunction,$letAssoc)) or (y:= symbolTarget("all",$letAssoc))) then
      if (y="all" or symbolMember?(x,y)) and
        not (IS__GENVAR(x) or isSharpVarWithNum(x) or gensym? x) then
         sayBrightlyNT [:bright x,": "]
         PRIN1 shortenForPrinting val
         finishLine $OutputStream
      if (y:= hasPair("BREAK",y)) and
        (y="all" or symbolMember?(x,y) and
          (not symbolMember?(PNAME(x).0,'($ _#)) and not gensym? x)) then
            break [:bright currentFunction,'"breaks after",:bright x,'":= ",
              shortenForPrinting val]
  val

-- This is the version for use when we have already
-- converted the data into type "Expression"
letPrint2(x,printform,currentFunction) ==
  $BreakMode:local := nil
  if $letAssoc and
    ((y:= symbolTarget(currentFunction,$letAssoc)) or (y:= symbolTarget("all",$letAssoc))) then
      if (y="all" or symbolMember?(x,y)) and
        not (IS__GENVAR(x) or isSharpVarWithNum(x) or gensym? x) then
         $BreakMode:='letPrint2
         flag:=nil
         CATCH('letPrint2,mathprint ["=",x,printform],flag)
         if flag='letPrint2 then print printform
      if (y:= hasPair("BREAK",y)) and
        (y="all" or symbolMember?(x,y) and
          (not symbolMember?(PNAME(x).0,'($ _#)) and not gensym? x)) then
            break [:bright currentFunction,'"breaks after",:bright x,":= ",
              printform]
  x

-- This is the version for use when we have our hands on a function
-- to convert the data into type "Expression"

letPrint3(x,xval,printfn,currentFunction) ==
  $BreakMode:local := nil
  if $letAssoc and
    ((y:= LASSOC(currentFunction,$letAssoc)) or (y:= symbolTarget("all",$letAssoc))) then
      if (y="all" or symbolMember?(x,y)) and
        not (IS__GENVAR(x) or isSharpVarWithNum(x) or gensym? x) then
         $BreakMode:='letPrint2
         flag:=nil
         CATCH('letPrint2,mathprint ["=",x,SPADCALL(xval,printfn)],flag)
         if flag='letPrint2 then print xval
      if (y:= hasPair("BREAK",y)) and
        (y="all" or symbolMember?(x,y) and
          (not (PNAME(x).0 in '($ _#)) and not gensym? x)) then
            break [:bright currentFunction,'"breaks after",:bright x,'":= ",
              xval]
  x

getAliasIfTracedMapParameter(x,currentFunction) ==
  isSharpVarWithNum x =>
    aliasList:= get(currentFunction,'alias,$InteractiveFrame) =>
      aliasList.(STRING2PINT_-N(subString(PNAME x,1,nil),1)-1)
  x

getBpiNameIfTracedMap(name) ==
  lmm:= get(name,'localModemap,$InteractiveFrame) =>
    symbolMember?(bpiName:= CADAR lmm,_/TRACENAMES) => bpiName
  name

hasPair(key,l) ==
  l isnt [.,:.] => nil
  l is [[ =key,:a],:.] => a
  hasPair(key,rest l)

shortenForPrinting val ==
  isDomainOrPackage val => devaluate val
  val

spadTraceAlias(domainId,op,n) ==
  makeSymbol strconc(domainId,".",op,",",STRINGIMAGE n)

getOption(opt,l) ==
  y:= ASSOC(opt,l) => rest y

reportSpadTrace(header,[op,sig,n,:t]) ==
  null $traceNoisely => nil
  msg:= [header,'"%b",op,":",'"%d",rest sig," -> ",first sig," in slot ",n]
  namePart:= nil --(t is (.,.,name,:.) => (" named ",name); nil)
  tracePart:=
    t is [y,:.] and not null y =>
      (y="all" => ['"%b","all",'"%d","vars"]; [" vars: ",y])
    nil
  sayBrightly [:msg,:namePart,:tracePart]

orderBySlotNumber l ==
  ASSOCRIGHT orderList [[n,:x] for (x:= [.,.,n,:.]) in l]

_/TRACEREPLY() ==
  null _/TRACENAMES => '"   Nothing is traced."
  for x in _/TRACENAMES repeat
    x is [d,:.] and isDomainOrPackage d =>
      domainList:= [devaluate d,:domainList]
    functionList:= [x,:functionList]
  [:functionList,:domainList,"traced"]

spadReply() ==
  [printName x for x in _/TRACENAMES] where
    printName x ==
      x is [d,:.] and isDomainOrPackage d => devaluate d
      x

spadUntrace(domain,options) ==
  not isDomainOrPackage domain => userError '"bad argument to untrace"
  anyifTrue:= null options
  listOfOperations:= getOption("ops:",options)
  domainId := devaluate domain
  null (pair:= ASSOC(domain,_/TRACENAMES)) =>
    sayMSG ['"   No functions in",
      :bright prefix2String domainId,'"are now traced."]
  sigSlotNumberAlist:= rest pair
  for (pair:= [op,sig,n,lv,bpiPointer,traceName,alias]) in sigSlotNumberAlist |
    anyifTrue or symbolMember?(op,listOfOperations) repeat
      BPIUNTRACE(traceName,alias)
      domain.n.first := bpiPointer
      pair.rest.rest.rest := nil
      if assocPair:=ASSOC(BPINAME bpiPointer,$letAssoc) then
        $letAssoc := REMOVER($letAssoc,assocPair)
        if null $letAssoc then SETLETPRINTFLAG nil
  newSigSlotNumberAlist:= [x for x in sigSlotNumberAlist | CDDDR x]
  newSigSlotNumberAlist => pair.rest := newSigSlotNumberAlist
  SETQ(_/TRACENAMES,DELASC(domain,_/TRACENAMES))
  spadReply()

prTraceNames() ==
  (for x in _/TRACENAMES repeat PRINT fn x; nil) where
    fn x ==
      x is [d,:t] and isDomainOrPackage d => [devaluate d,:t]
      x

traceReply() ==
  $domains: local:= nil
  $packages: local:= nil
  $constructors: local:= nil
  null _/TRACENAMES =>
    sayMessage '"   Nothing is traced now."
  sayBrightly '" "
  for x in _/TRACENAMES repeat
    x is [d,:.] and (isDomainOrPackage d) => addTraceItem d
    x isnt [.,:.] =>
      isFunctor x => addTraceItem x
      (IS__GENVAR x =>
        addTraceItem eval x; functionList:= [x,:functionList])
    userError '"bad argument to trace"
  functionList:= "append"/[[rassocSub(x,$mapSubNameAlist),'" "]
    for x in functionList | not isSubForRedundantMapName x]
  if functionList then
    2 = #functionList =>
      sayMSG ["   Function traced: ",:functionList]
    (22 + sayBrightlyLength functionList) <= $LINELENGTH =>
      sayMSG ["   Functions traced: ",:functionList]
    sayBrightly "   Functions traced:"
    sayBrightly flowSegmentedMsg(functionList,$LINELENGTH,6)
  if $domains then
    displayList:= concat(prefix2String first $domains,
          [:concat('",",'" ",prefix2String x) for x in rest $domains])
    if displayList isnt [.,:.] then displayList:= [displayList]
    sayBrightly '"   Domains traced: "
    sayBrightly flowSegmentedMsg(displayList,$LINELENGTH,6)
  if $packages then
    displayList:= concat(prefix2String first $packages,
          [:concat(", ",prefix2String x) for x in rest $packages])
    if displayList isnt [.,:.] then displayList:= [displayList]
    sayBrightly '"   Packages traced: "
    sayBrightly flowSegmentedMsg(displayList,$LINELENGTH,6)
  if $constructors then
    displayList:= concat(abbreviate first $constructors,
          [:concat(", ",abbreviate x) for x in rest $constructors])
    if displayList isnt [.,:.] then displayList:= [displayList]
    sayBrightly '"   Parameterized constructors traced:"
    sayBrightly flowSegmentedMsg(displayList,$LINELENGTH,6)

addTraceItem d ==
  isDomainOrPackage d => $packages:= [devaluate d,:$packages]
  constructor? d => $constructors:=[d,:$constructors]

tracelet(fn,vars) ==
  if gensym? fn and stupidIsSpadFunction eval fn then
    fn := eval fn
    if COMPILED_-FUNCTION_-P fn then fn:=BPINAME fn
  fn = 'Undef => nil
  vars:=
    vars="all" => "all"
    l:= LASSOC(fn,$letAssoc) => setUnion(vars,l)
    vars
  $letAssoc:= [[fn,:vars],:$letAssoc]
  if $letAssoc then SETLETPRINTFLAG true
  $TRACELETFLAG : local := true
  $QuickLet : local := false
  not symbolMember?(fn,$traceletFunctions) and not IS__GENVAR fn and COMPILED_-FUNCTION_-P symbolFunction fn
    and not stupidIsSpadFunction fn and not gensym? fn =>
      ($traceletFunctions:= [fn,:$traceletFunctions]; compileBoot fn ;
       $traceletFunctions:= remove($traceletFunctions,fn) )

breaklet(fn,vars) ==
                       --vars is "all" or a list of variables
  --$letAssoc ==> (.. (=fn .. (BREAK . all))) OR (.. (=fn .. (BREAK . vl)))
  if gensym? fn and stupidIsSpadFunction eval fn then
    fn := eval fn
    if COMPILED_-FUNCTION_-P fn then fn:= BPINAME fn
  fn = "Undef" => nil
  fnEntry:= LASSOC(fn,$letAssoc)
  vars:=
    pair := symbolTarget("BREAK",fnEntry) => setUnion(vars,rest pair)
    vars
  $letAssoc:=
    null fnEntry => [[fn,:[["BREAK",:vars]]],:$letAssoc]
    pair => (pair.rest := vars; $letAssoc)
  if $letAssoc then SETLETPRINTFLAG true
  $QuickLet:local := false
  not symbolMember?(fn,$traceletFunctions) and not stupidIsSpadFunction fn
    and not gensym? fn =>
      $traceletFunctions:= [fn,:$traceletFunctions]
      compileBoot fn
      $traceletFunctions:= removeSymbol($traceletFunctions,fn)

stupidIsSpadFunction fn ==
  -- returns true if the function pname has a semi-colon in it
  -- eventually, this will use isSpadFunction from luke boot
  findChar(char ";",PNAME fn)

break msg ==
  condition:= MONITOR_,EVALTRAN(_/BREAKCONDITION,nil)
  eval condition =>
    sayBrightly msg
    INTERRUPT()

compileBoot fn == _/D_,1([fn],'(_/COMP),nil,nil)

