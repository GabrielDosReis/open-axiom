-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2011, Gabriel Dos Reis.
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

module nrunfast where
  getOpCode: (%Symbol, %Vector %Thing, %Short) -> %Maybe %Short

++
$doNotCompressHashTableIfTrue := false

++
$monitorNewWorld := false

++
$lookupDefaults := false

++
$NRTmonitorIfTrue := false

--=======================================================================
--                     Basic Functions
--=======================================================================
initNewWorld() ==
  $monitorNewWorld := false
  $spadLibFT := 'NRLIB
  $NRTmonitorIfTrue := false
  $updateCatTableIfTrue := false
  $doNotCompressHashTableIfTrue := true
 
isNewWorldDomain domain == 
  integer? domain.3    --see HasCategory/Attribute
 
getDomainByteVector dom == 
  CDDR dom.4

++ Return the sequence of categories `dom' belongs to, as a vector
++ of lazy category forms.
getDomainCategoriesVector dom ==
  second(dom.4)

++ Same as getDomainCategoriesVector except that we return a list of
++ input forms for the categories.
getDomainCompleteCategories dom ==
  vec := getDomainCategoriesVector dom
  cats := nil
  for i in 0..MAXINDEX vec repeat
    cats := [newExpandLocalType(vec.i,dom,dom), :cats]
  nreverse cats
 
getOpCode(op,vec,max) ==
--search Op vector for "op" returning code if found, nil otherwise
  res := nil
  for i in 0..max by 2 repeat
    sameObject?(vectorRef(vec,i),op) => return (res := i + 1)
  res

evalSlotDomain(u,dollar) ==
  $returnNowhereFromGoGet: local := false
  $ : fluid := dollar                      -- ??? substitute
  $lookupDefaults : local := false -- new world
  u = '$ => dollar
  u = "$$" => dollar
  integer? u =>
    y := dollar.u
    vector? y => y
    y is ['%store,:.] => eval y
             --lazy domains need to marked; this is dangerous?
    y is [v,:.] =>
      vector? v => lazyDomainSet(y,dollar,u)   --old style has [$,code,:lazyt]
      IDENTP v and constructor? v 
        or v in '(Record Union Mapping Enumeration) =>
           lazyDomainSet(y,dollar,u)        --new style has lazyt
      y
    y
  u is ['NRTEVAL,y] => eval y
  u is ['QUOTE,y] => y
  u is ['Record,:argl] =>
     apply('Record,[[":",tag,evalSlotDomain(dom,dollar)]
                                 for [.,tag,dom] in argl])
  u is ['Union,:argl] and first argl is ['_:,.,.] =>
     apply('Union,[['_:,tag,evalSlotDomain(dom,dollar)]
                                 for [.,tag,dom] in argl])
  u is ["Enumeration",:.] => eval u
  cons? u =>
    -- The domain form may value arguments, get VM form first.
    u := expandToVMForm u
    cons? u => apply(u.op,[evalSlotDomain(x,dollar) for x in u.args])
    u
  systemErrorHere '"evalSlotDomain"

--=======================================================
--                 Lookup From Compiled Code
--=======================================================
newGoGet(:l) ==
  [:arglist,env] := l
  slot := replaceGoGetSlot env
  apply(first slot,[:arglist,rest slot])  --SPADCALL it!
 
replaceGoGetSlot env ==
  [thisDomain,index,:op] := env
  thisDomainForm := devaluate thisDomain
  bytevec := getDomainByteVector thisDomain
  numOfArgs := bytevec.index
  goGetDomainSlotIndex := bytevec.(index := index + 1)
  goGetDomain :=
     goGetDomainSlotIndex = 0 => thisDomain
     thisDomain.goGetDomainSlotIndex
  if cons? goGetDomain then
     goGetDomain := lazyDomainSet(goGetDomain,thisDomain,goGetDomainSlotIndex)
  sig :=
    [newExpandTypeSlot(bytevec.(index := index + 1),thisDomain,thisDomain)
      for i in 0..numOfArgs]
  thisSlot := bytevec.(index + 1)
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
  vectorRef(thisDomain,thisSlot) := slot
  if $monitorNewWorld then
    sayLooking1('"<------",[first slot,:devaluate rest slot])
  slot
 
--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================

lookupComplete(op,sig,dollar,env) == 
  newLookupInTable(op,sig,dollar,env,nil)
 
lookupIncomplete(op,sig,dollar,env) == 
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
  start := opvec.k
  finish :=
    QSGREATERP(max,k) => opvec.(QSPLUS(k,2))
    maxIndex
  if QSGREATERP(finish,maxIndex) then systemError '"limit too large"
  numArgs := QSDIFFERENCE(#sig,1)
  success := nil
  $isDefaultingPackage: local :=
    -- use special defaulting handler when dollar non-trivial
    dollar ~= domain and isDefaultPackageForm? devaluate domain
  while finish > start repeat
    PROGN
      i := start
      numArgs ~= (numTableArgs :=numvec.i) => nil
      predIndex := numvec.(i := i + 1)
      predIndex ~= 0 and not testBitVector(predvec,predIndex) => nil
      loc := newCompareSig(sig,numvec,(i := i + 1),dollar,domain)
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
      cons? slot =>
        slot.op = 'newGoGet => someMatch:=true
                   --treat as if operation were not there
        --if sameObject?(QCAR slot,'newGoGet) then
        --  UNWIND_-PROTECT --break infinite recursion
        --    ((SETELT(domain,loc,'skip); slot := replaceGoGetSlot rest slot),
        --      if domain.loc = 'skip then domain.loc := slot)
        return (success := slot)
      slot = 'skip =>       --recursive call from above 'replaceGoGetSlot
        return (success := newLookupInAddChain(op,sig,domain,dollar))
      systemError '"unexpected format"
    start := QSPLUS(start,QSPLUS(numTableArgs,4))
  success ~= 'failed and success =>
    if $monitorNewWorld then
      sayLooking1('"<----",uu) where uu() ==
        cons? success => [first success,:devaluate rest success]
        success
    success
  subsumptionSig and (u:= basicLookup(op,subsumptionSig,domain,dollar)) => u
  flag or someMatch => newLookupInAddChain(op,sig,domain,dollar)
  nil
 
 
--=======================================================
--   Lookup In Domain (from lookupInAddChain)
--=======================================================
lookupInDomain(op,sig,addFormDomain,dollar,index) ==
  addFormCell := addFormDomain.index =>
    integer? KAR addFormCell =>
      or/[lookupInDomain(op,sig,addFormDomain,dollar,i) for i in addFormCell]
    if not vector? addFormCell then addFormCell := eval addFormCell
    lookupInDomainVector(op,sig,addFormCell,dollar)
  nil

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
lookupInDomainVector(op,sig,domain,dollar) ==
  slot1 := domain.1
  SPADCALL(op,sig,dollar,slot1)


++ same as lookupInDomainVector except that the use of defaults
++ (either in category packages or add-chains) is controlled
++ by `useDefaults'.
lookupInDomainAndDefaults(op,sig,domain,dollar,useDefaults) ==
  savedLookupDefaults := $lookupDefaults
  $lookupDefaults := useDefaults
  fun := lookupInDomainVector(op,sig,domain,dollar)
  $lookupDefaults := savedLookupDefaults
  fun

--=======================================================
--       Lookup Addlist (from lookupInDomainTable or lookupInDomain)
--=======================================================
newLookupInAddChain(op,sig,addFormDomain,dollar) ==
  if $monitorNewWorld then sayLooking1('"looking up add-chain: ",addFormDomain)
  addFunction:=newLookupInDomain(op,sig,addFormDomain,dollar,5)
  addFunction =>
    if $monitorNewWorld then
      sayLooking1(concat('"<----add-chain function found for ",
        form2String devaluate addFormDomain,'"<----"),rest addFunction)
    addFunction
  nil
 
--=======================================================
--   Lookup In Domain (from lookupInAddChain)
--=======================================================
newLookupInDomain(op,sig,addFormDomain,dollar,index) ==
  addFormCell := addFormDomain.index =>
    integer? KAR addFormCell =>
      or/[newLookupInDomain(op,sig,addFormDomain,dollar,i) for i in addFormCell]
    if not vector? addFormCell then lazyDomainSet(addFormCell,addFormDomain,index)
    lookupInDomainVector(op,sig,addFormDomain.index,dollar)
  nil
 
--=======================================================
--       Category Default Lookup (from goGet or lookupInAddChain)
--=======================================================
newLookupInCategories(op,sig,dom,dollar) ==
  slot4 := dom.4
  catVec := second slot4
  # catVec = 0 => nil                      --early exit if no categories
  integer? KDR catVec.0 =>
    newLookupInCategories1(op,sig,dom,dollar) --old style
  $lookupDefaults : local := nil
  if $monitorNewWorld = true then sayBrightly concat('"----->",
    form2String devaluate dom,'"-----> searching default packages for ",op)
  predvec := dom.3
  packageVec := first slot4
--the next three lines can go away with new category world
  varList := ['$,:$FormalMapVariableList]
  valueList := [dom,:[dom.(5+i) for i in 1..(# rest dom.0)]]
  valueList := [MKQ val for val in valueList]
  nsig := MSUBST(dom.0,dollar.0,sig)
  for i in 0..MAXINDEX packageVec |
       (entry := packageVec.i) and entry ~= 'T repeat
    package :=
      vector? entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      IDENTP entry =>
        cat := catVec.i
        packageForm := nil
        if not GETL(entry,'LOADED) then loadLib entry
        infovec := GETL(entry,'infovec)
        success :=
          --vector? infovec =>  ----new world
          true =>  ----new world
            opvec := infovec.1
            max := MAXINDEX opvec
            code := getOpCode(op,opvec,max)
            null code => nil
            byteVector := CDDDR infovec.3
            endPos :=
              code+2 > max => # byteVector
              opvec.(code+2)
            not nrunNumArgCheck(#sig.source,byteVector,opvec.code,endPos) => nil
            --numOfArgs := byteVector.(opvec.code)
            --numOfArgs ~= #sig.source => nil
            packageForm := [entry,'$,:rest cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
                           ----old world
          table := HGET($Slot1DataBase,entry) or systemError nil
          (u := LASSQ(op,table))
            and (v := or/[rest x for x in u | #sig = #x.0]) =>
              packageForm := [entry,'$,:rest cat]
              package := evalSlotDomain(packageForm,dom)
              packageVec.i := package
              package
          nil
        not success =>
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
  packageVec := first slot4
  catVec := first rest slot4
  --the next three lines can go away with new category world
  varList := ['$,:$FormalMapVariableList]
  valueList := [dom,:[dom.(5+i) for i in 1..(# rest dom.0)]]
  valueList := [MKQ val for val in valueList]
  nsig := MSUBST(dom.0,dollar.0,sig)
  for i in 0..MAXINDEX packageVec | (entry := packageVec.i)
      and (vector? entry or (predIndex := rest (node := catVec.i)) and
          (predIndex = 0 or testBitVector(predvec,predIndex))) repeat
    package :=
      vector? entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      IDENTP entry =>
        cat := first node
        packageForm := nil
        if not GETL(entry,'LOADED) then loadLib entry
        infovec := GETL(entry,'infovec)
        success :=
          vector? infovec =>
            opvec := infovec.1
            max := MAXINDEX opvec
            code := getOpCode(op,opvec,max)
            null code => nil
            byteVector := CDDR infovec.3
            numOfArgs := byteVector.(opvec.code)
            numOfArgs ~= #sig.source => nil
            packageForm := [entry,'$,:rest cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
          table := HGET($Slot1DataBase,entry) or systemError nil
          (u := LASSQ(op,table))
            and (v := or/[rest x for x in u | #sig = #x.0]) =>
              packageForm := [entry,'$,:rest cat]
              package := evalSlotDomain(packageForm,dom)
              packageVec.i := package
              package
          nil
        not success =>
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
              for s in rest sig for i in (index+1)..] => numvec.(k + 1)
     nil
  nil
 
--=======================================================
--     Compare Signature to One Derived from Table
--=======================================================
lazyMatchArg(s,a,dollar,domain) == lazyMatchArg2(s,a,dollar,domain,true)
 
lazyMatchArg2(s,a,dollar,domain,typeFlag) ==
  if s = '$ then
    --  a = 0 => return true  --needed only if extra call in newGoGet to basicLookup
    s := devaluate dollar -- calls from HasCategory can have $s
  integer? a =>
    not typeFlag => s = domain.a
    a = 6 and $isDefaultingPackage => s = devaluate dollar
    vector? (d := domainVal(dollar,domain,a)) =>
      s = d.0 => true
      domainArg := ($isDefaultingPackage => domain.6.0; domain.0)
      KAR s = first d.0 and
        lazyMatchArgDollarCheck(replaceSharpCalls s,d.0,dollar.0,domainArg)
    --vector? first d => lazyMatch(s,CDDR d,dollar,domain)      --old style (erase)
    lazyMatch(replaceSharpCalls s,d,dollar,domain)       --new style
  a = '$ => s = devaluate dollar
  a = "$$" => s = devaluate domain
  string? a =>
    string? s => a = s
    s is ['QUOTE,y] and PNAME y = a
    IDENTP s and symbolName s = a
  atom a =>  a = s
  op := opOf a
  op  = 'NRTEVAL => s = nrtEval(second a,domain)
  op = 'QUOTE => s = second a
  lazyMatch(s,a,dollar,domain)
  --above line is temporarily necessary until system is compiled 8/15/90
--s = a

++ Return true if the symbol `s' designates a builtin constructor.
builtinConstructor? s ==
  s in $BuiltinConstructorNames

++ Return true if the symbol `s' designates a generalized builtin
++ constructor, that is a builtin constructor or any operator we
++ deem as a constructor from the domain slot-filling machinery perspective.
generalizedBuiltinConstructor? s ==
  builtinConstructor? s or s is "QUOTE" or s is "[||]"

lazyMatch(source,lazyt,dollar,domain) ==
  lazyt is [op,:argl] and cons? source and op=first source
    and #(sargl := rest source) = #argl =>
      builtinConstructor? op and first argl is [":",:.] =>
        and/[stag = atag and lazyMatchArg(s,a,dollar,domain)
              for [.,stag,s] in sargl for [.,atag,a] in argl]
      generalizedBuiltinConstructor? op =>
         and/[lazyMatchArg(s,a,dollar,domain) for s in sargl for a in argl]
      coSig := getDualSignatureFromDB op
      null coSig => error ["bad Constructor op", op]
      and/[lazyMatchArg2(s,a,dollar,domain,flag)
           for s in sargl for a in argl for flag in rest coSig]
  string? source and lazyt is ['QUOTE,=source] => true
  integer? source =>
      lazyt is ['_#, slotNum] => source = #(domain.slotNum)
      lazyt is ['%call,f,slotNum] and f in '(LENGTH %llength) =>
        source = #(domain.slotNum)
      nil

  -- A hideous hack on the same lines as the previous four lines JHD/MCD
  source is ['construct,:l] => l = lazyt
  nil

 
lazyMatchArgDollarCheck(s,d,dollarName,domainName) ==
  #s ~= #d => nil
  scoSig := getDualSignatureFromDB opOf s or return nil
  if opOf s in '(Union Mapping Record) then 
     scoSig := [true for x in s]
  and/[fn for x in rest s for arg in rest d for xt in rest scoSig] where
   fn() ==
    x = arg => true
    x is ['elt,someDomain,opname] => lookupInDomainByName(opname,evalDomain someDomain,arg)
    x = '$ and (arg = dollarName or arg = domainName) => true
    x = dollarName and arg = domainName => true
    atom x or atom arg => false
    xt and first x = first arg =>
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
  start := opvec.k
  finish :=
    QSGREATERP(max,k) => opvec.(QSPLUS(k,2))
    maxIndex
  if QSGREATERP(finish,maxIndex) then systemError '"limit too large"
  success := false
  while finish > start repeat
    i := start
    numberOfArgs :=numvec.i
    predIndex := numvec.(i := i + 1)
    predIndex ~= 0 and not testBitVector(predvec,predIndex) => nil
    slotIndex := numvec.(i + 2 + numberOfArgs)
    newStart := QSPLUS(start,QSPLUS(numberOfArgs,4))
    slot := domain.slotIndex
    cons? slot and sameObject?(first slot,first arg) and sameObject?(rest slot,rest arg) => return (success := true)
    start := QSPLUS(start,QSPLUS(numberOfArgs,4))
  success
 
--=======================================================
--        Expand Signature from Encoded Slot Form
--=======================================================
newExpandGoGetTypeSlot(slot,dollar,domain) ==
  newExpandTypeSlot(slot,domain,domain)
 
newExpandTypeSlot(slot, dollar, domain) ==
-- returns domain form for dollar.slot
   newExpandLocalType(sigDomainVal(dollar, domain, slot), dollar,domain)
 
 
newExpandLocalType(lazyt,dollar,domain) ==
  vector? lazyt => lazyt.0
  atom lazyt => lazyt
  lazyt is [vec,.,:lazyForm] and vector? vec =>              --old style
    newExpandLocalTypeForm(lazyForm,dollar,domain)
  newExpandLocalTypeForm(lazyt,dollar,domain)             --new style
 
newExpandLocalTypeForm([functorName,:argl],dollar,domain) ==
  functorName in '(Record Union) and first argl is [":",:.] =>
    [functorName,:[['_:,tag,newExpandLocalTypeArgs(dom,dollar,domain,true)]
                                 for [.,tag,dom] in argl]]
  functorName in '(Union Mapping _[_|_|_] Enumeration) =>
          [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,true) for a in argl]]
  functorName = "QUOTE"  => [functorName,:argl]
  coSig := getDualSignatureFromDB functorName
  null coSig => error ["bad functorName", functorName]
  [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,flag)
        for a in argl for flag in rest coSig]]
 
newExpandLocalTypeArgs(u,dollar,domain,typeFlag) ==
  u = '$ => u
  integer? u =>
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
 
lazyDomainSet(lazyForm,thisDomain,slot) ==
  form :=
    lazyForm is [vec,.,:u] and vector? vec => u        --old style
    lazyForm                                        --new style
  slotDomain := evalSlotDomain(form,thisDomain)
  if $monitorNewWorld then
    sayLooking1(concat(form2String devaluate thisDomain,
      '" activating lazy slot ",slot,'": "),slotDomain)
  name := first form
  vectorRef(thisDomain,slot) := slotDomain
 

++ Return a type form where all niladic constructors are
++ resolved to constructor calls.  Note: it is assumed that no
++ such resolution has already occured.
resolveNiladicConstructors form ==
  IDENTP form and niladicConstructorFromDB form => [form]
  atom form => form
  form is ["QUOTE",:.] => form
  for args in tails rest form repeat
    args.first := resolveNiladicConstructors first args
  form

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
      hasCaty(a,b,NIL) ~= 'failed
      HasCategory(evalDomain a,b) => true -- for asharp domains: must return Boolean
  op := opOf catOrAtt
  isAtom := atom catOrAtt
  not isAtom and op = 'Join =>
    and/[newHasTest(domform,x) for x in rest catOrAtt]
-- we will refuse to say yes for 'Cat has Cat'
--getConstructorKindFromDB opOf domform = "category" => throwKeyedMsg("S2IS0025",NIL)
-- on second thoughts we won't!
  categoryForm? domform =>
      domform = catOrAtt => 'T
      for [aCat,:cond] in [:ancestorsOf(domform,NIL),:SUBLISLIS (rest domform,$FormalMapVariableList,getConstructorAttributesFromDB(opOf domform))] |  aCat = catOrAtt  repeat
         return evalCond cond where
           evalCond x ==
             atom x => x
             [pred,:l] := x
             pred = "has" => 
                  l is [ w1,['ATTRIBUTE,w2]] => newHasTest(w1,w2) 
                  l is [ w1,['SIGNATURE,:w2]] => compiledLookup(first w2,second w2, eval mkEvalable w1)
                  newHasTest(first  l ,second l) 
             pred in '(OR or %or) => or/[evalCond i for i in l]
             pred in '(AND and %and) => and/[evalCond i for i in l]
             x  
  not isAtom and categoryForm? catOrAtt  =>
    domain := eval mkEvalable domform
    newHasCategory(domain,catOrAtt)
  catOrAtt is [":",op,type] =>
    sig := 
      type is ["Mapping",:sig'] =>
         for ts in tails sig' repeat
           ts.first := resolveNiladicConstructors first ts
         sig'
      -- a constant; make it look like op: () -> type
      [resolveNiladicConstructors type]
    HasSignature(evalDomain domform, [op,sig])
  newHasAttribute(eval mkEvalable domform,catOrAtt)
 
lazyMatchAssocV(x,auxvec,catvec,domain) ==      --new style slot4
  n := MAXINDEX catvec
  xop := first x
  or/[auxvec.i for i in 0..n |
    xop = first (lazyt := vectorRef(catvec,i)) and lazyMatch(x,lazyt,domain,domain)]
 
lazyMatchAssocV1(x,vec,domain) ==               --old style slot4
  n  := MAXINDEX vec
  xop := first x
  or/[rest vectorRef(vec,i) for i in 0..n |
    xop = first (lazyt := first vectorRef(vec,i)) and lazyMatch(x,lazyt,domain,domain)]
 
--=======================================================
--                   Utility Functions
--=======================================================
 
sayLooking(prefix,op,sig,dom) ==
  $monitorNewWorld := false
  dollar := devaluate dom
  atom dollar or vector? dollar or "or"/[vector? x for x in dollar] => systemError nil
  sayBrightly
    concat(prefix,formatOpSignature(op,sig),bright '"from ",form2String dollar)
  $monitorNewWorld := true
 
sayLooking1(prefix,dom) ==
  $monitorNewWorld := false
  dollar :=
    vector? dom => devaluate dom
    devaluateList dom
  sayBrightly concat(prefix,form2String dollar)
  $monitorNewWorld := true
 
cc() == -- don't remove this function
  clearConstructorCaches()
  clearClams()
