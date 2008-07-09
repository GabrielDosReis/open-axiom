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


import c_-util
namespace BOOT

++
$doNotCompressHashTableIfTrue := false

++
$lookupDefaults := false

++
$NRTmonitorIfTrue := false

--=======================================================================
--                     Basic Functions
--=======================================================================
initNewWorld() ==
  $NRTvec := true
  $monitorNewWorld := false
  $spadLibFT := 'NRLIB
  $NRTmonitorIfTrue := false
  $updateCatTableIfTrue := false
  $doNotCompressHashTableIfTrue := true
 
isNewWorldDomain domain == INTEGERP domain.3    --see HasCategory/Attribute
 
getDomainByteVector dom == CDDR dom.4
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
getOpCode(op,vec,max) ==
--search Op vector for "op" returning code if found, nil otherwise
  res := nil
  for i in 0..max by 2 repeat
    EQ(QVELT(vec,i),op) => return (res := QSADD1 i)
  res
 
--=======================================================
--                 Lookup From Compiled Code
--=======================================================
newGoGet(:l) ==
  [:arglist,env] := l
  slot := replaceGoGetSlot env
  APPLY(first slot,[:arglist,rest slot])  --SPADCALL it!
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
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
  setShellEntry(thisDomain,thisSlot,slot)
  if $monitorNewWorld then
    sayLooking1('"<------",[CAR slot,:devaluate CDR slot])
  slot
 
--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================
lookupFF(op,sig,dollar,env) == newLookupInTable(op,sig,dollar,env,nil)
 
lookupUF(op,sig,dollar,env) == newLookupInTable(op,sig,dollar,env,true)
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
lookupComplete(op,sig,dollar,env) == newLookupInTable(op,sig,dollar,env,nil)
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
lookupIncomplete(op,sig,dollar,env) == newLookupInTable(op,sig,dollar,env,true)
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
lookupInCompactTable(op,sig,dollar,env) ==
  newLookupInTable(op,sig,dollar,env,true)
 
newLookupInTable(op,sig,dollar,[domain,opvec],flag) ==
  dollar = nil => systemError()
  $lookupDefaults = true =>
    newLookupInCategories(op,sig,domain,dollar)      --lookup first in my cats
      or newLookupInAddChain(op,sig,domain,dollar)
  --fast path when called from newGoGet
  success := false
  if $monitorNewWorld then
    sayLooking(concat('"---->",form2String devaluate domain,
      '"----> searching op table for:","%l","  "),op,sig,dollar)
  someMatch := false
  numvec := getDomainByteVector domain
  predvec := domain.3
  max := MAXINDEX opvec
  k := getOpCode(op,opvec,max) or return
    flag => newLookupInAddChain(op,sig,domain,dollar)
    nil
  maxIndex := MAXINDEX numvec
  start := ELT(opvec,k)
  finish :=
    QSGREATERP(max,k) => opvec.(QSPLUS(k,2))
    maxIndex
  if QSGREATERP(finish,maxIndex) then systemError '"limit too large"
  numArgs := QSDIFFERENCE(#sig,1)
  success := nil
  $isDefaultingPackage: local :=
    -- use special defaulting handler when dollar non-trivial
    dollar ^= domain and isDefaultPackageForm? devaluate domain
  while finish > start repeat
    PROGN
      i := start
      numArgs ^= (numTableArgs :=numvec.i) => nil
      predIndex := numvec.(i := QSADD1 i)
      NE(predIndex,0) and null testBitVector(predvec,predIndex) => nil
      loc := newCompareSig(sig,numvec,(i := QSADD1 i),dollar,domain)
      null loc => nil  --signifies no match
      loc = 1 => (someMatch := true)
      loc = 0 =>
        start := QSPLUS(start,QSPLUS(numTableArgs,4))
        i := start + 2
        someMatch := true --mark so that if subsumption fails, look for original
        subsumptionSig :=
          [newExpandTypeSlot(numvec.(QSPLUS(i,j)),
            dollar,domain) for j in 0..numTableArgs]
        if $monitorNewWorld then
          sayBrightly [formatOpSignature(op,sig),'"--?-->",
            formatOpSignature(op,subsumptionSig)]
        nil
      slot := domain.loc
      null atom slot =>
        EQ(QCAR slot,'newGoGet) => someMatch:=true
                   --treat as if operation were not there
        --if EQ(QCAR slot,'newGoGet) then
        --  UNWIND_-PROTECT --break infinite recursion
        --    ((SETELT(domain,loc,'skip); slot := replaceGoGetSlot QCDR slot),
        --      if domain.loc = 'skip then domain.loc := slot)
        return (success := slot)
      slot = 'skip =>       --recursive call from above 'replaceGoGetSlot
        return (success := newLookupInAddChain(op,sig,domain,dollar))
      systemError '"unexpected format"
    start := QSPLUS(start,QSPLUS(numTableArgs,4))
  NE(success,'failed) and success =>
    if $monitorNewWorld then
      sayLooking1('"<----",uu) where uu() ==
        PAIRP success => [first success,:devaluate rest success]
        success
    success
  subsumptionSig and (u:= basicLookup(op,subsumptionSig,domain,dollar)) => u
  flag or someMatch => newLookupInAddChain(op,sig,domain,dollar)
  nil
 
 
isDefaultPackageForm? x == x is [op,:.]
  and IDENTP op and (s := PNAME op).(MAXINDEX s) = "&"
 
 
--=======================================================
--       Lookup Addlist (from lookupInDomainTable or lookupInDomain)
--=======================================================
newLookupInAddChain(op,sig,addFormDomain,dollar) ==
  if $monitorNewWorld then sayLooking1('"looking up add-chain: ",addFormDomain)
  addFunction:=newLookupInDomain(op,sig,addFormDomain,dollar,5)
  addFunction =>
    if $monitorNewWorld then
      sayLooking1(concat('"<----add-chain function found for ",
        form2String devaluate addFormDomain,'"<----"),CDR addFunction)
    addFunction
  nil
 
--=======================================================
--   Lookup In Domain (from lookupInAddChain)
--=======================================================
newLookupInDomain(op,sig,addFormDomain,dollar,index) ==
  addFormCell := addFormDomain.index =>
    INTEGERP KAR addFormCell =>
      or/[newLookupInDomain(op,sig,addFormDomain,dollar,i) for i in addFormCell]
    if null VECP addFormCell then lazyDomainSet(addFormCell,addFormDomain,index)
    lookupInDomainVector(op,sig,addFormDomain.index,dollar)
  nil
 
--=======================================================
--       Category Default Lookup (from goGet or lookupInAddChain)
--=======================================================
newLookupInCategories(op,sig,dom,dollar) ==
  slot4 := dom.4
  catVec := CADR slot4
  SIZE catVec = 0 => nil                      --early exit if no categories
  INTEGERP KDR catVec.0 =>
    newLookupInCategories1(op,sig,dom,dollar) --old style
  $lookupDefaults : local := nil
  if $monitorNewWorld = true then sayBrightly concat('"----->",
    form2String devaluate dom,'"-----> searching default packages for ",op)
  predvec := dom.3
  packageVec := QCAR slot4
--the next three lines can go away with new category world
  varList := ['$,:$FormalMapVariableList]
  valueList := [dom,:[dom.(5+i) for i in 1..(# rest dom.0)]]
  valueList := [MKQ val for val in valueList]
  nsig := MSUBST(dom.0,dollar.0,sig)
  for i in 0..MAXINDEX packageVec |
       (entry := packageVec.i) and entry ^= 'T repeat
    package :=
      VECP entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      IDENTP entry =>
        cat := catVec.i
        packageForm := nil
        if not GETL(entry,'LOADED) then loadLib entry
        infovec := GETL(entry,'infovec)
        success :=
          --VECP infovec =>  ----new world
          true =>  ----new world
            opvec := infovec.1
            max := MAXINDEX opvec
            code := getOpCode(op,opvec,max)
            null code => nil
            byteVector := CDDDR infovec.3
            endPos :=
              code+2 > max => SIZE byteVector
              opvec.(code+2)
            not nrunNumArgCheck(#(QCDR sig),byteVector,opvec.code,endPos) => nil
            --numOfArgs := byteVector.(opvec.code)
            --numOfArgs ^= #(QCDR sig) => nil
            packageForm := [entry,'$,:CDR cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
                           ----old world
          table := HGET($Slot1DataBase,entry) or systemError nil
          (u := LASSQ(op,table))
            and (v := or/[rest x for x in u | #sig = #x.0]) =>
              packageForm := [entry,'$,:CDR cat]
              package := evalSlotDomain(packageForm,dom)
              packageVec.i := package
              package
          nil
        null success =>
          if $monitorNewWorld = true then
            sayBrightlyNT '"  not in: "
            pp (packageForm and devaluate package or entry)
          nil
        if $monitorNewWorld then
          sayLooking1('"candidate default package instantiated: ",success)
        success
      entry
    null package => nil
    if $monitorNewWorld then
      sayLooking1('"Looking at instantiated package ",package)
    res := basicLookup(op,sig,package,dollar) =>
      if $monitorNewWorld = true then
        sayBrightly '"candidate default package succeeds"
      return res
    if $monitorNewWorld = true then
      sayBrightly '"candidate fails -- continuing to search categories"
    nil
 
nrunNumArgCheck(num,bytevec,start,finish) ==
   args := bytevec.start
   num = args => true
   (start := start + args + 4) = finish => nil
   nrunNumArgCheck(num,bytevec,start,finish)
 
newLookupInCategories1(op,sig,dom,dollar) ==
  $lookupDefaults : local := nil
  if $monitorNewWorld = true then sayBrightly concat('"----->",
    form2String devaluate dom,'"-----> searching default packages for ",op)
  predvec := dom.3
  slot4 := dom.4
  packageVec := CAR slot4
  catVec := CAR QCDR slot4
--the next three lines can go away with new category world
  varList := ['$,:$FormalMapVariableList]
  valueList := [dom,:[dom.(5+i) for i in 1..(# rest dom.0)]]
  valueList := [MKQ val for val in valueList]
  nsig := MSUBST(dom.0,dollar.0,sig)
  for i in 0..MAXINDEX packageVec | (entry := ELT(packageVec,i))
      and (VECP entry or (predIndex := CDR (node := ELT(catVec,i))) and
          (EQ(predIndex,0) or testBitVector(predvec,predIndex))) repeat
    package :=
      VECP entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      IDENTP entry =>
        cat := QCAR node
        packageForm := nil
        if not GETL(entry,'LOADED) then loadLib entry
        infovec := GETL(entry,'infovec)
        success :=
          VECP infovec =>
            opvec := infovec.1
            max := MAXINDEX opvec
            code := getOpCode(op,opvec,max)
            null code => nil
            byteVector := CDDR infovec.3
            numOfArgs := byteVector.(opvec.code)
            numOfArgs ^= #(QCDR sig) => nil
            packageForm := [entry,'$,:CDR cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
          table := HGET($Slot1DataBase,entry) or systemError nil
          (u := LASSQ(op,table))
            and (v := or/[rest x for x in u | #sig = #x.0]) =>
              packageForm := [entry,'$,:CDR cat]
              package := evalSlotDomain(packageForm,dom)
              packageVec.i := package
              package
          nil
        null success =>
          if $monitorNewWorld = true then
            sayBrightlyNT '"  not in: "
            pp (packageForm and devaluate package or entry)
          nil
        if $monitorNewWorld then
          sayLooking1('"candidate default package instantiated: ",success)
        success
      entry
    null package => nil
    if $monitorNewWorld then
      sayLooking1('"Looking at instantiated package ",package)
    res := lookupInDomainVector(op,sig,package,dollar) =>
      if $monitorNewWorld = true then
        sayBrightly '"candidate default package succeeds"
      return res
    if $monitorNewWorld = true then
      sayBrightly '"candidate fails -- continuing to search categories"
    nil
 
--=======================================================
--         Compare Signature to One Derived from Table
--=======================================================
newCompareSig(sig, numvec, index, dollar, domain) ==
  k := index
  null (target := first sig)
   or lazyMatchArg(target,numvec.k,dollar,domain) =>
     and/[lazyMatchArg(s,numvec.(k := i),dollar,domain)
              for s in rest sig for i in (index+1)..] => numvec.(QSINC1 k)
     nil
  nil
 
--=======================================================
--     Compare Signature to One Derived from Table
--=======================================================
lazyMatchArg(s,a,dollar,domain) == lazyMatchArg2(s,a,dollar,domain,true)
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
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
 
lazyMatch(source,lazyt,dollar,domain) ==
  lazyt is [op,:argl] and null atom source and op=CAR source
    and #(sargl := CDR source) = #argl =>
      MEMQ(op,'(Record Union)) and first argl is [":",:.] =>
        and/[stag = atag and lazyMatchArg(s,a,dollar,domain)
              for [.,stag,s] in sargl for [.,atag,a] in argl]
      MEMQ(op,'(Union Mapping _[_|_|_] QUOTE Enumeration)) =>
         and/[lazyMatchArg(s,a,dollar,domain) for s in sargl for a in argl]
      coSig := getDualSignatureFromDB op
      null coSig => error ["bad Constructor op", op]
      and/[lazyMatchArg2(s,a,dollar,domain,flag)
           for s in sargl for a in argl for flag in rest coSig]
  STRINGP source and lazyt is ['QUOTE,=source] => true
  NUMBERP source =>
      lazyt is ['_#, slotNum] => source = #(domain.slotNum)
      lazyt is ['call,'LENGTH, slotNum] => source = #(domain.slotNum)
      nil

  -- A hideous hack on the same lines as the previous four lines JHD/MCD
  source is ['construct,:l] => l = lazyt
  nil

 
lazyMatchArgDollarCheck(s,d,dollarName,domainName) ==
  #s ^= #d => nil
  scoSig := getDualSignatureFromDB opOf s or return nil
  if MEMQ(opOf s, '(Union Mapping Record)) then 
     scoSig := [true for x in s]
  and/[fn for x in rest s for arg in rest d for xt in rest scoSig] where
   fn() ==
    x = arg => true
    x is ['elt,someDomain,opname] => lookupInDomainByName(opname,evalDomain someDomain,arg)
    x = '$ and (arg = dollarName or arg = domainName) => true
    x = dollarName and arg = domainName => true
    ATOM x or ATOM arg => false
    xt and CAR x = CAR arg =>
      lazyMatchArgDollarCheck(x,arg,dollarName,domainName)
    false

lookupInDomainByName(op,domain,arg) ==
  atom arg => nil
  opvec := domain . 1 . 2
  numvec := getDomainByteVector domain
  predvec := domain.3
  max := MAXINDEX opvec
  k := getOpCode(op,opvec,max) or return nil
  maxIndex := MAXINDEX numvec
  start := ELT(opvec,k)
  finish :=
    QSGREATERP(max,k) => opvec.(QSPLUS(k,2))
    maxIndex
  if QSGREATERP(finish,maxIndex) then systemError '"limit too large"
  success := false
  while finish > start repeat
    i := start
    numberOfArgs :=numvec.i
    predIndex := numvec.(i := QSADD1 i)
    NE(predIndex,0) and null testBitVector(predvec,predIndex) => nil
    slotIndex := numvec.(i + 2 + numberOfArgs)
    newStart := QSPLUS(start,QSPLUS(numberOfArgs,4))
    slot := domain.slotIndex
    null atom slot and EQ(CAR slot,CAR arg) and EQ(CDR slot,CDR arg) => return (success := true)
    start := QSPLUS(start,QSPLUS(numberOfArgs,4))
  success
 
--=======================================================
--        Expand Signature from Encoded Slot Form
--=======================================================
newExpandGoGetTypeSlot(slot,dollar,domain) ==
  newExpandTypeSlot(slot,domain,domain)
 
newExpandTypeSlot(slot, dollar, domain) ==
--> returns domain form for dollar.slot
   newExpandLocalType(sigDomainVal(dollar, domain, slot), dollar,domain)
 
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
newExpandLocalType(lazyt,dollar,domain) ==
  VECP lazyt => lazyt.0
  ATOM lazyt => lazyt
  lazyt is [vec,.,:lazyForm] and VECP vec =>              --old style
    newExpandLocalTypeForm(lazyForm,dollar,domain)
  newExpandLocalTypeForm(lazyt,dollar,domain)             --new style
 
newExpandLocalTypeForm([functorName,:argl],dollar,domain) ==
  MEMQ(functorName, '(Record Union)) and first argl is [":",:.] =>
    [functorName,:[['_:,tag,newExpandLocalTypeArgs(dom,dollar,domain,true)]
                                 for [.,tag,dom] in argl]]
  MEMQ(functorName, '(Union Mapping _[_|_|_] Enumeration)) =>
          [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,true) for a in argl]]
  functorName = "QUOTE"  => [functorName,:argl]
  coSig := getDualSignatureFromDB functorName
  null coSig => error ["bad functorName", functorName]
  [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,flag)
        for a in argl for flag in rest coSig]]
 
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
 
nrtEval(expr,dom) ==
  $:fluid := dom             --??? this should be a substitute
  eval expr
 
domainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => dollar
  index = 2 => domain
  domain.index

-- ??? This function should be merged into the preceding one. 
sigDomainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => "$"
  index = 2 => domain
  domain.index

--=======================================================
--          Convert Lazy Domain to Domain Form
--=======================================================
 
--------------------> NEW DEFINITION (see interop.boot.pamphlet)
lazyDomainSet(lazyForm,thisDomain,slot) ==
  form :=
    lazyForm is [vec,.,:u] and VECP vec => u        --old style
    lazyForm                                        --new style
  slotDomain := evalSlotDomain(form,thisDomain)
  if $monitorNewWorld then
    sayLooking1(concat(form2String devaluate thisDomain,
      '" activating lazy slot ",slot,'": "),slotDomain)
  name := CAR form
  setShellEntry(thisDomain,slot,slotDomain)
 
--=======================================================
--                   HasCategory/Attribute
--=======================================================
-- PLEASE NOTE: This function has the rather charming side-effect that
-- e.g. it works if domform is an Aldor Category.  This is being used
-- by extendscategoryForm in c-util to allow Aldor domains to be used
-- in spad code.  Please do not break this!  An example is the use of
-- Interval (an Aldor domain) by SIGNEF in limitps.spad.  MCD.
newHasTest(domform,catOrAtt) ==
  domform is [dom,:.] and dom in '(Union Record Mapping Enumeration) =>
    ofCategory(domform, catOrAtt)
  catOrAtt = '(Type) => true
  asharpConstructorFromDB opOf domform => fn(domform,catOrAtt) where
  -- atom (infovec := getInfovec opOf domform) => fn(domform,catOrAtt) where
    fn(a,b) ==
      categoryForm?(a) => assoc(b, ancestorsOf(a, nil))
      isPartialMode a => throwKeyedMsg("S2IS0025",NIL)
      b is ["SIGNATURE",:opSig] =>
        HasSignature(evalDomain a,opSig)
      b is ["ATTRIBUTE",attr] => HasAttribute(evalDomain a,attr)
      hasCaty(a,b,NIL) ^= 'failed
      HasCategory(evalDomain a,b) => true -- for asharp domains: must return Boolean
  op := opOf catOrAtt
  isAtom := atom catOrAtt
  null isAtom and op = 'Join =>
    and/[newHasTest(domform,x) for x in rest catOrAtt]
-- we will refuse to say yes for 'Cat has Cat'
--getConstructorKindFromDB opOf domform = "category" => throwKeyedMsg("S2IS0025",NIL)
-- on second thoughts we won't!
  getConstructorKindFromDB opOf domform = "category" =>
      domform = catOrAtt => 'T
      for [aCat,:cond] in [:ancestorsOf(domform,NIL),:SUBLISLIS (rest domform,$FormalMapVariableList,getConstructorAttributesFromDB(opOf domform))] |  aCat = catOrAtt  repeat
         return evalCond cond where
           evalCond x ==
             ATOM x => x
             [pred,:l] := x
             pred = 'has => 
                  l is [ w1,['ATTRIBUTE,w2]] => newHasTest(w1,w2) 
                  l is [ w1,['SIGNATURE,:w2]] => compiledLookup(CAR w2,CADR w2, eval mkEvalable w1)
                  newHasTest(first  l ,first rest l) 
             pred = 'OR => or/[evalCond i for i in l]
             pred = 'AND => and/[evalCond i for i in l]
             x  
  null isAtom and constructor? op  =>
    domain := eval mkEvalable domform
    newHasCategory(domain,catOrAtt)
  newHasAttribute(eval mkEvalable domform,catOrAtt)
 
lazyMatchAssocV(x,auxvec,catvec,domain) ==      --new style slot4
  n := MAXINDEX catvec
  xop := CAR x
  or/[ELT(auxvec,i) for i in 0..n |
    xop = CAR (lazyt := QVELT(catvec,i)) and lazyMatch(x,lazyt,domain,domain)]
 
lazyMatchAssocV1(x,vec,domain) ==               --old style slot4
  n  := MAXINDEX vec
  xop := CAR x
  or/[QCDR QVELT(vec,i) for i in 0..n |
    xop = CAR (lazyt := CAR QVELT(vec,i)) and lazyMatch(x,lazyt,domain,domain)]
 
--newHasAttribute(domain,attrib) ==
--  predIndex := LASSOC(attrib,domain.2) =>
--    EQ(predIndex,0) => true
--    predvec := domain.3
--    testBitVector(predvec,predIndex)
--  false
 
--=======================================================
--                   Utility Functions
--=======================================================
 
sayLooking(prefix,op,sig,dom) ==
  $monitorNewWorld := false
  dollar := devaluate dom
  atom dollar or VECP dollar or "or"/[VECP x for x in dollar] => systemError nil
  sayBrightly
    concat(prefix,formatOpSignature(op,sig),bright '"from ",form2String dollar)
  $monitorNewWorld := true
 
sayLooking1(prefix,dom) ==
  $monitorNewWorld := false
  dollar :=
    VECP dom => devaluate dom
    devaluateList dom
  sayBrightly concat(prefix,form2String dollar)
  $monitorNewWorld := true
 
cc() == -- don't remove this function
  clearConstructorCaches()
  clearClams()
