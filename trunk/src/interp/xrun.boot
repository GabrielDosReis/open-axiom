-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


)package "BOOT"

$noSubsumption:=true
--$MERGELIB := nil
------- from nrunopt.boot -----------

--------------------> NEW DEFINITION (see nrunopt.boot.pamphlet)
NRTmakeCategoryAlist() ==
  $depthAssocCache: local := MAKE_-HASHTABLE 'ID
  $catAncestorAlist: local := NIL
  pcAlist := [:[[x,:"T"] for x in $uncondAlist],:$condAlist]
  $levelAlist: local := depthAssocList [CAAR x for x in pcAlist]
  opcAlist := NREVERSE SORTBY(function NRTcatCompare,pcAlist)
  newPairlis := [[5 + i,:b] for [.,:b] in $pairlis for i in 1..]
  slot1 := [[a,:k] for [a,:b] in SUBLIS($pairlis,opcAlist)
                   | (k := predicateBitIndex b) ^= -1]
  slot0 := [hasDefaultPackage opOf a for [a,:b] in slot1]
  sixEtc := [5 + i for i in 1..#$pairlis]
  formals := ASSOCRIGHT $pairlis
  for x in slot1 repeat
       RPLACA(x,EQSUBSTLIST(CONS("$$",sixEtc),CONS('$,formals),CAR x))
  -----------code to make a new style slot4 -----------------
  predList := ASSOCRIGHT slot1  --is list of predicate indices
  maxPredList := "MAX"/predList
  catformvec := ASSOCLEFT slot1
  maxElement := "MAX"/$byteVec
  ['CONS, ['makeByteWordVec2,MAX(maxPredList,1),MKQ predList],
    ['CONS, MKQ LIST2VEC slot0,
      ['CONS, MKQ LIST2VEC [encodeCatform x for x in catformvec],
        ['makeByteWordVec2,maxElement,MKQ $byteVec]]]]
  --NOTE: this is new form: old form satisfies VECP CDDR form

--------------------> NEW DEFINITION (see nrunopt.boot.pamphlet)
encodeCatform x ==
  k := NRTassocIndex x => k
  atom x or atom rest x => x
  [first x,:[encodeCatform y for y in rest x]]

------- from nrunfast.boot -----------

--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
replaceGoGetSlot env ==
  [thisDomain,index,:op] := env
  thisDomainForm := devaluate thisDomain
  bytevec := getDomainByteVector thisDomain
  numOfArgs := bytevec.index
  goGetDomainSlotIndex := bytevec.(index := QSADD1 index)
  goGetDomain :=
     goGetDomainSlotIndex = 0 => thisDomain
     thisDomain.goGetDomainSlotIndex
  if PAIRP goGetDomain then
     goGetDomain := lazyDomainSet(goGetDomain,thisDomain,goGetDomainSlotIndex)
  sig :=
    [newExpandTypeSlot(bytevec.(index := QSADD1 index),thisDomain,thisDomain)
      for i in 0..numOfArgs]
  thisSlot := bytevec.(QSADD1 index)
  if $monitorNewWorld then
    sayLooking(concat('"%l","..",form2String thisDomainForm,
      '" wants",'"%l",'"  "),op,sig,goGetDomain)
  slot :=  basicLookup(op,sig,goGetDomain,goGetDomain)
  slot = nil =>
    $returnNowhereFromGoGet = true =>
      ['nowhere,:goGetDomain]  --see newGetDomainOpTable
    sayBrightly concat('"Function: ",formatOpSignature(op,sig),
      '" is missing from domain: ",form2String goGetDomain.0)
    keyedSystemError("S2NR0001",[op,sig,goGetDomain.0])
  if $monitorNewWorld then
    sayLooking1(['"goget stuffing slot",:bright thisSlot,'"of "],thisDomain)
  SETELT(thisDomain,thisSlot,slot)
  if $monitorNewWorld then
    sayLooking1('"<------",[CAR slot,:devaluate CDR slot])
  slot

--=======================================================
--        Expand Signature from Encoded Slot Form
--=======================================================
--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
newExpandGoGetTypeSlot(slot,dollar,domain) ==
  newExpandTypeSlot(slot,domain,domain)

--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
newExpandTypeSlot(slot, dollar, domain) ==
--> returns domain form for dollar.slot
   newExpandLocalType(sigDomainVal(dollar, domain, slot), dollar,domain)


--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
newExpandLocalType(lazyt,dollar,domain) ==
  VECP lazyt => lazyt.0
  ATOM lazyt => lazyt
  lazyt is [vec,.,:lazyForm] and VECP vec =>              --old style
    newExpandLocalTypeForm(lazyForm,dollar,domain)
  newExpandLocalTypeForm(lazyt,dollar,domain)             --new style

--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
newExpandLocalTypeForm([functorName,:argl],dollar,domain) ==
  MEMQ(functorName, '(Record Union)) and first argl is [":",:.] =>
    [functorName,:[['_:,tag,newExpandLocalTypeArgs(dom,dollar,domain,true)]
                                 for [.,tag,dom] in argl]]
  MEMQ(functorName, '(Union Mapping)) =>
          [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,true) for a in argl]]
  functorName = 'QUOTE => [functorName,:argl]
  coSig := GETDATABASE(functorName,'COSIG)
  NULL coSig => error ["bad functorName", functorName]
  [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,flag)
        for a in argl for flag in rest coSig]]

--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
newExpandLocalTypeArgs(u,dollar,domain,typeFlag) ==
  u = '$ => u
  INTEGERP u =>
     typeFlag => newExpandTypeSlot(u, dollar,domain)
     domain.u
  u is ['NRTEVAL,y] => nrtEval(y,domain)
  u is ['QUOTE,y] => y
  u = "$$" => domain.0
  atom u => u   --can be first, rest, etc.
  newExpandLocalTypeForm(u,dollar,domain)

--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
nrtEval(expr,dom) ==
  $:fluid := dom
  eval expr

sigDomainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => "$"
  index = 2 => domain
  domain.index

--------------------> NEW DEFINITION (see nrunfast.boot.pamphlet)
lazyMatchArg2(s,a,dollar,domain,typeFlag) ==
  if s = '$ then
--  a = 0 => return true  --needed only if extra call in newGoGet to basicLookup
    s := devaluate dollar -- calls from HasCategory can have $s
  INTEGERP a =>
    not typeFlag => s = domain.a
    a = 6 and $isDefaultingPackage => s = devaluate dollar
    VECP (d := domainVal(dollar,domain,a)) =>
      s = d.0 => true
      domainArg := ($isDefaultingPackage => domain.6.0; domain.0)
      KAR s = QCAR d.0 and
        lazyMatchArgDollarCheck(replaceSharpCalls s,d.0,dollar.0,domainArg)
    --VECP CAR d => lazyMatch(s,CDDR d,dollar,domain)      --old style (erase)
    lazyMatch(replaceSharpCalls s,d,dollar,domain)       --new style
  a = '$ => s = devaluate dollar
  a = "$$" => s = devaluate domain
  STRINGP a =>
    STRINGP s => a = s
    s is ['QUOTE,y] and PNAME y = a
    IDENTP s and PNAME s = a
  atom a =>  a = s
  op := opOf a
  op  = 'NRTEVAL => s = nrtEval(CADR a,domain)
  op = 'QUOTE => s = CADR a
  lazyMatch(s,a,dollar,domain)
  --above line is temporarily necessary until system is compiled 8/15/90
--s = a

------- from template.boot -----------

--------------------> NEW DEFINITION (see template.boot.pamphlet)
evalSlotDomain(u,dollar) ==
  $returnNowhereFromGoGet: local := false
  $ : fluid := dollar
  $lookupDefaults : local := nil -- new world
  u = '$ => dollar
  u = "$$" => dollar
  FIXP u =>
    VECP (y := dollar.u) => y
    y is ['SETELT,:.] => eval y--lazy domains need to marked; this is dangerous?
    y is [v,:.] =>
      VECP v => lazyDomainSet(y,dollar,u)               --old style has [$,code,:lazyt]
      constructor? v or MEMQ(v,'(Record Union Mapping)) =>
        lazyDomainSet(y,dollar,u)                       --new style has lazyt
      y
    y
  u is ['NRTEVAL,y] => eval  y
  u is ['QUOTE,y] => y
  u is ['Record,:argl] =>
     FUNCALL('Record0,[[tag,:evalSlotDomain(dom,dollar)]
                                 for [.,tag,dom] in argl])
  u is ['Union,:argl] and first argl is ['_:,.,.] =>
     APPLY('Union,[['_:,tag,evalSlotDomain(dom,dollar)]
                                 for [.,tag,dom] in argl])
  u is [op,:argl] => APPLY(op,[evalSlotDomain(x,dollar) for x in argl])
  systemErrorHere '"evalSlotDomain"


------- from nrungo.boot -----------

--------------------> NEW DEFINITION (see nrungo.boot.pamphlet)
lazyCompareSigEqual(s,tslot,dollar,domain) ==
  tslot = '$ => s = tslot -- devaluate dollar  --needed for browser
  INTEGERP tslot and PAIRP(lazyt:=domain.tslot) and PAIRP s =>
      lazyt is [.,.,.,[.,item,.]] and
        item is [.,[functorName,:.]] and functorName = CAR s =>
          compareSigEqual(s,(NRTevalDomain lazyt).0,dollar,domain)
      nil
  compareSigEqual(s,NRTreplaceLocalTypes(tslot,domain),dollar,domain)

------- from i-funsel.boot -----------

--------------------> NEW DEFINITION (see i-funsel.boot.pamphlet)
findFunctionInDomain(op,dc,tar,args1,args2,$Coerce,$SubDom) ==
  -- looks for a modemap for op with signature  args1 -> tar
  --   in the domain of computation dc
  -- tar may be NIL (= unknown)
  null isLegitimateMode(tar, nil, nil) => nil
  dcName:= CAR dc
  member(dcName,'(Union Record Mapping Enumeration)) =>
    -- First cut code that ignores args2, $Coerce and $SubDom
    -- When domains no longer have to have Set, the hard coded 6 and 7
    -- should go.
    op = '_= =>
        #args1 ^= 2 or args1.0 ^= dc or args1.1 ^= dc => NIL
        tar and tar ^= '(Boolean) => NIL
        [[[dc, '(Boolean), dc, dc], ['(Boolean),'$,'$], [NIL, NIL]]]
    op = 'coerce =>
        #args1 ^= 1 
        dcName='Enumeration and (args1.0=$Symbol or tar=dc)=>
           [[[dc, dc, $Symbol], ['$,$Symbol], [NIL, NIL]]]
        args1.0 ^= dc => NIL
        tar and tar ^= $Expression => NIL
        [[[dc, $Expression, dc], [$Expression,'$], [NIL, NIL]]]
    member(dcName,'(Record Union)) =>
      findFunctionInCategory(op,dc,tar,args1,args2,$Coerce,$SubDom)
    NIL
  fun:= NIL
  ( p := ASSQ(op,getOperationAlistFromLisplib dcName) ) and
    SL := constructSubst dc
    -- if the arglist is homogeneous, first look for homogeneous
    -- functions. If we don't find any, look at remaining ones
    if isHomogeneousList args1 then
      q := NIL
      r := NIL
      for mm in CDR p repeat
        -- CDAR of mm is the signature argument list
        if isHomogeneousList CDAR mm then q := [mm,:q]
        else r := [mm,:r]
      q := allOrMatchingMms(q,args1,tar,dc)
      for mm in q repeat
        fun:= nconc(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
      r := reverse r
    else r := CDR p
    r := allOrMatchingMms(r,args1,tar,dc)
    if not fun then    -- consider remaining modemaps
      for mm in r repeat
        fun:= nconc(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
  if not fun and $reportBottomUpFlag then
    sayMSG concat
      ['"   -> no appropriate",:bright op,'"found in",
        :bright prefix2String dc]
  fun

--------------------> NEW DEFINITION (see i-funsel.boot.pamphlet)
findFunctionInDomain1(omm,op,tar,args1,args2,SL) ==
  dc:= CDR (dollarPair := ASSQ('$,SL))
  -- need to drop '$ from SL
  mm:= subCopy(omm, SL)
  -- tests whether modemap mm is appropriate for the function
  -- defined by op, target type tar and argument types args
  $RTC:local:= NIL
  -- $RTC is a list of run-time checks to be performed

  [sig,slot,cond,y] := mm
  [osig,:.]  := omm
  osig := subCopy(osig, SUBSTQ(CONS('$,'$), dollarPair, SL))
  if CONTAINED('_#, sig) or CONTAINED('construct,sig) then
    sig := [replaceSharpCalls t for t in sig]
  matchMmCond cond and matchMmSig(mm,tar,args1,args2) and
    EQ(y,'Subsumed) and
      -- hmmmm: do Union check in following because (as in DP)
      -- Unions are subsumed by total modemaps which are in the
      -- mm list in findFunctionInDomain.
      y := 'ELT      -- if subsumed fails try it again
      not $SubDom and CAR sig isnt ['Union,:.] and slot is [tar,:args] and
        (f := findFunctionInDomain(op,dc,tar,args,args,NIL,NIL)) => f
    EQ(y,'ELT) => [[CONS(dc,sig),osig,nreverse $RTC]]
    EQ(y,'CONST) => [[CONS(dc,sig),osig,nreverse $RTC]]
    EQ(y,'ASCONST) => [[CONS(dc,sig),osig,nreverse $RTC]]
    y is ['XLAM,:.] => [[CONS(dc,sig),y,nreverse $RTC]]
    sayKeyedMsg("S2IF0006",[y])
    NIL

--------------------> NEW DEFINITION (see i-funsel.boot.pamphlet)
findFunctionInCategory(op,dc,tar,args1,args2,$Coerce,$SubDom) ==
  -- looks for a modemap for op with signature  args1 -> tar
  --   in the domain of computation dc
  -- tar may be NIL (= unknown)
  dcName:= CAR dc
  not MEMQ(dcName,'(Record Union Enumeration)) => NIL
  fun:= NIL
 --  cat := constructorCategory dc
  makeFunc := GETL(dcName,"makeFunctionList") or
      systemErrorHere '"findFunctionInCategory"
  [funlist,.] := FUNCALL(makeFunc,"$",dc,$CategoryFrame)
  -- get list of implementations and remove sharps
  maxargs := -1
  impls := nil
  for [a,b,d] in funlist repeat
    not EQ(a,op) => nil
    d is ['XLAM,xargs,:.] =>
      if PAIRP(xargs) then maxargs := MAX(maxargs,#xargs)
      else maxargs := MAX(maxargs,1)
      impls := cons([b,nil,true,d],impls)
    impls := cons([b,d,true,d],impls)
  impls := NREVERSE impls
  if maxargs ^= -1 then
    SL:= NIL
    for i in 1..maxargs repeat
      impls := SUBSTQ(GENSYM(),INTERNL('"#",STRINGIMAGE i),impls)
  impls and
    SL:= constructSubst dc
    for mm in impls repeat
      fun:= nconc(fun,findFunctionInDomain1(mm,op,tar,args1,args2,SL))
  if not fun and $reportBottomUpFlag then
    sayMSG concat
      ['"   -> no appropriate",:bright op,'"found in",
        :bright prefix2String dc]
  fun

------- from i-eval.boot -----------

--------------------> NEW DEFINITION (see i-eval.boot.pamphlet)
evalForm(op,opName,argl,mmS) ==
  -- applies the first applicable function
  for mm in mmS until form repeat
    [sig,fun,cond]:= mm
    (CAR sig) = 'interpOnly => form := CAR sig
    #argl ^= #CDDR sig => 'skip ---> RDJ 6/95
    form:=
      $genValue or null cond =>
        [getArgValue2(x,t,sideEffectedArg?(t,sig,opName),opName) or return NIL
         for x in argl for t in CDDR sig]
      [getArgValueComp2(x,t,c,sideEffectedArg?(t,sig,opName),opName) or return NIL
        for x in argl for t in CDDR sig for c in cond]
    form or null argl =>
      dc:= CAR sig
      form :=
        dc='local => --[fun,:form]
          atom fun =>
            fun in $localVars => ['SPADCALL,:form,fun]
            [fun,:form,NIL]
          ['SPADCALL,:form,fun]
        dc is ["__FreeFunction__",:freeFun] =>
          ['SPADCALL,:form,freeFun]
        fun is ['XLAM,xargs,:xbody] =>
          rec :=  first form
          xbody is [['RECORDELT,.,ind,len]] =>
            optRECORDELT([CAAR xbody,rec,ind,len])
          xbody is [['SETRECORDELT,.,ind,len,.]] =>
            optSETRECORDELT([CAAR xbody,rec,ind,len,CADDR form])
          xbody is [['RECORDCOPY,.,len]] =>
            optRECORDCOPY([CAAR xbody,rec,len])
          ['FUNCALL,['function , ['LAMBDA,xargs,:xbody]],:TAKE(#xargs, form)]
        dcVector := evalDomain dc
        fun0 :=
          newType? CAAR mm =>
            mm' := first ncSigTransform mm
            ncGetFunction(opName, first mm', rest mm')
          NRTcompileEvalForm(opName,fun,dcVector)
        null fun0 => throwKeyedMsg("S2IE0008",[opName])
        [bpi,:domain] := fun0
        EQ(bpi,function Undef) =>
         sayKeyedMsg("S2IE0009",[opName,formatSignature CDR sig,CAR sig])
         NIL
        if $NRTmonitorIfTrue = true then
          sayBrightlyNT ['"Applying ",first fun0,'" to:"]
          pp [devaluateDeeply x for x in form]
        _$:fluid := domain
        ['SPADCALL, :form, fun0]
  not form => nil
--  not form => throwKeyedMsg("S2IE0008",[opName])
  form='interpOnly => rewriteMap(op,opName,argl)
  targetType := CADR sig
  if CONTAINED('_#,targetType) then targetType := NRTtypeHack targetType
  evalFormMkValue(op,form,targetType)

------- from clammed.boot -----------

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
coerceConvertMmSelection(funName,m1,m2) ==
  -- calls selectMms with $Coerce=NIL and tests for required
  -- target type. funName is either 'coerce or 'convert.
  $declaredMode : local:= NIL
  $reportBottomUpFlag : local:= NIL
  l := selectMms1(funName,m2,[m1],[m1],NIL)
  mmS := [[sig,[targ,arg],:pred] for x in l | x is [sig,[.,arg],:pred] and
    hasCorrectTarget(m2,sig) and sig is [dc,targ,oarg] and oarg = m1]
  mmS and CAR mmS

------- from i-coerce.boot -----------

--------------------> NEW DEFINITION (see i-coerce.boot.pamphlet)
coerceByFunction(T,m2) ==
  -- using the new modemap selection without coercions
  -- should not be called by canCoerceFrom
  x := objVal T
  x = '_$fromCoerceable_$ => NIL
  m2 is ['Union,:.] => NIL
  m1 := objMode T
  m2 is ['Boolean,:.] and m1 is ['Equation,ud] =>
    dcVector := evalDomain ud
    fun :=
      isWrapped x =>
        NRTcompiledLookup("=", [$Boolean, '$, '$], dcVector)
      NRTcompileEvalForm("=", [$Boolean, '$, '$], dcVector)
    [fn,:d]:= fun
    isWrapped x =>
      x:= unwrap x
      mkObjWrap(SPADCALL(CAR x,CDR x,fun),m2)
    x isnt ['SPADCALL,a,b,:.] => keyedSystemError("S2IC0015",NIL)
    code := ['SPADCALL, a, b, fun]
    objNew(code,$Boolean)
  -- If more than one function is found, any should suffice, I think -scm
  if not (mm := coerceConvertMmSelection(funName := 'coerce,m1,m2)) then
    mm := coerceConvertMmSelection(funName := 'convert,m1,m2)
  mm =>
    [[dc,tar,:args],slot,.]:= mm
    dcVector := evalDomain(dc)
    fun:=
--+
      isWrapped x =>
        NRTcompiledLookup(funName,slot,dcVector)
      NRTcompileEvalForm(funName,slot,dcVector)
    [fn,:d]:= fun
    fn = function Undef => NIL
    isWrapped x =>
--+
      $: fluid := dcVector
      val := CATCH('coerceFailure, SPADCALL(unwrap x,fun))
      (val = $coerceFailure) => NIL
      objNewWrap(val,m2)
    env := fun
    code := ['failCheck, ['SPADCALL, x, env]]
--  tar is ['Union,:.] => objNew(['failCheck,code],m2)
    objNew(code,m2)
  -- try going back to types like RN instead of QF I
  m1' := eqType m1
  m2' := eqType m2
  (m1 ^= m1') or (m2 ^= m2') => coerceByFunction(objNew(x,m1'),m2')
  NIL

--------------------> NEW DEFINITION (see i-coerce.boot.pamphlet)
equalOne(object, domain) ==
  -- tries using constant One and "=" from domain
  -- object should not be wrapped
  algEqual(object, getConstantFromDomain('(One),domain), domain)

--------------------> NEW DEFINITION (see i-coerce.boot.pamphlet)
equalZero(object, domain) ==
  -- tries using constant Zero and "=" from domain
  -- object should not be wrapped
  algEqual(object, getConstantFromDomain('(Zero),domain), domain)

--------------------> NEW DEFINITION (see i-coerce.boot.pamphlet)
algEqual(object1, object2, domain) ==
  -- sees if 2 objects of the same domain are equal by using the
  -- "=" from the domain
  -- objects should not be wrapped
--  eqfunc := getFunctionFromDomain("=",domain,[domain,domain])
  eqfunc := compiledLookupCheck("=",[$Boolean,'$,'$],evalDomain domain)
  SPADCALL(object1,object2, eqfunc)
