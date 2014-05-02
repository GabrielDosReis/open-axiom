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
 
getDomainByteVector dom == 
  CDDR domainData dom

++ Return the sequence of categories `dom' belongs to, as a vector
++ of lazy category forms.
getDomainCategoriesVector dom ==
  second domainData dom

++ Same as getDomainCategoriesVector except that we return a list of
++ input forms for the categories.
getDomainCompleteCategories dom ==
  vec := getDomainCategoriesVector dom
  cats := nil
  for i in 0..maxIndex vec repeat
    cats := [newExpandLocalType(vectorRef(vec,i),dom,dom), :cats]
  reverse! cats
 
getOpCode(op,vec,max) ==
--search Op vector for "op" returning code if found, nil otherwise
  res := nil
  for i in 0..max by 2 repeat
    sameObject?(vectorRef(vec,i),op) => return (res := i + 1)
  res

evalSlotDomain(u,dollar) ==
  $returnNowhereFromGoGet: local := false
  $: local := dollar                      -- ??? substitute
  $lookupDefaults : local := false -- new world
  u is '$ => dollar
  u is "$$" => dollar
  integer? u =>
    y := dollar.u
    vector? y => y
    y is ['%store,:.] => eval y
             --lazy domains need to marked; this is dangerous?
    y is [v,:.] =>
      vector? v => lazyDomainSet(y,dollar,u)   --old style has [$,code,:lazyt]
      ident? v and constructor? v 
        or v in '(Record Union Mapping Enumeration) =>
           lazyDomainSet(y,dollar,u)        --new style has lazyt
      y
    y
  u is ['%eval,y] => eval y
  u is ['QUOTE,y] => y
  u is [":",tag,dom] => [":",tag,evalSlotDomain(dom,dollar)]
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
  numOfArgs := arrayRef(bytevec,index)
  goGetDomainSlotIndex := arrayRef(bytevec,index := index + 1)
  goGetDomain :=
     goGetDomainSlotIndex = 0 => thisDomain
     domainRef(thisDomain,goGetDomainSlotIndex)
  if cons? goGetDomain then
     goGetDomain := lazyDomainSet(goGetDomain,thisDomain,goGetDomainSlotIndex)
  sig :=
    [newExpandTypeSlot(arrayRef(bytevec,index := index + 1),thisDomain,thisDomain)
      for i in 0..numOfArgs]
  thisSlot := arrayRef(bytevec,index + 1)
  if $monitorNewWorld then
    sayLooking(concat('"%l","..",form2String thisDomainForm,
      '" wants",'"%l",'"  "),op,sig,goGetDomain)
  slot :=  basicLookup(op,sig,goGetDomain,goGetDomain)
  slot = nil =>
    $returnNowhereFromGoGet =>
      ['nowhere,:goGetDomain]  --see newGetDomainOpTable
    sayBrightly concat('"Function: ",formatOpSignature(op,sig),
      '" is missing from domain: ",form2String goGetDomain.0)
    keyedSystemError("S2NR0001",[op,sig,goGetDomain.0])
  if $monitorNewWorld then
    sayLooking1(['"goget stuffing slot",:bright thisSlot,'"of "],thisDomain)
  domainRef(thisDomain,thisSlot) := slot
  if $monitorNewWorld then
    sayLooking1('"<------",[first slot,:devaluate rest slot])
  slot
 
--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================

lookupComplete(op,sig,dollar,env) == 
  newLookupInTable(op,sig,dollar,env,false)
 
lookupIncomplete(op,sig,dollar,env) == 
  newLookupInTable(op,sig,dollar,env,true)
 
newLookupInTable(op,sig,dollar,[domain,opvec],flag) ==
  dollar = nil => systemError()
  $lookupDefaults =>
    newLookupInCategories(op,sig,domain,dollar)      --lookup first in my cats
      or newLookupInAddChain(op,sig,domain,dollar)
  --fast path when called from newGoGet
  success := false
  if $monitorNewWorld then
    sayLooking(concat('"---->",form2String devaluate domain,
      '"----> searching op table for:","%l","  "),op,sig,dollar)
  someMatch := false
  numvec := getDomainByteVector domain
  predvec := domainPredicates domain
  max := maxIndex opvec
  k := getOpCode(op,opvec,max) or return
    flag => newLookupInAddChain(op,sig,domain,dollar)
    nil
  idxmax := maxIndex numvec
  start := vectorRef(opvec,k)
  finish :=
    max > k => vectorRef(opvec,k + 2)
    idxmax
  if finish > idxmax then
    systemError '"limit too large"
  numArgs := #sig - 1
  success := nil
  $isDefaultingPackage: local :=
    -- use special defaulting handler when dollar non-trivial
    not sameObject?(dollar,domain) and defaultPackageForm? canonicalForm domain
  while finish > start repeat
    do
      -- a. Skip if non-matching arity.
      i := start
      numTableArgs := arrayRef(numvec,i)
      numArgs ~= numTableArgs => nil
      -- b. Skip if predicate untrue.
      i := i + 1
      predIndex := arrayRef(numvec,i)
      predIndex ~= 0 and not testBitVector(predvec,predIndex) => nil
      -- c. Skip if differing signature.
      i := i + 1
      loc := newCompareSig(sig,numvec,i,dollar,domain)
      null loc => nil  --signifies no match
      -- d. Should we consider for inherited operator?
      loc = 1 => someMatch := true
      -- e. Operator may be subsumed?
      loc = 0 =>
        start := start + numTableArgs + 4
        i := start + 2
        someMatch := true --mark so that if subsumption fails, look for original
        subsumptionSig :=
          [newExpandTypeSlot(arrayRef(numvec,i + j),
            dollar,domain) for j in 0..numTableArgs]
        if $monitorNewWorld then
          sayBrightly [formatOpSignature(op,sig),'"--?-->",
            formatOpSignature(op,subsumptionSig)]
      slot := domainRef(domain,loc)
      cons? slot =>
        slot.op is 'newGoGet => someMatch := true
                   --treat as if operation were not there
        return (success := slot)
      systemError '"unexpected format"
    start := start + numTableArgs + 4
  success isnt 'failed and success =>
    if $monitorNewWorld then
      sayLooking1('"<----",uu) where uu() ==
        cons? success => [first success,:devaluate rest success]
        success
    success
  subsumptionSig ~= nil
    and (u := basicLookup(op,subsumptionSig,domain,dollar)) => u
  flag or someMatch ~= nil => newLookupInAddChain(op,sig,domain,dollar)
  nil
 
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
    if not vector? addFormCell then
      lazyDomainSet(addFormCell,addFormDomain,index)
    lookupInDomainVector(op,sig,addFormDomain.index,dollar)
  nil
 
--=======================================================
--       Category Default Lookup (from goGet or lookupInAddChain)
--=======================================================
newLookupInCategories(op,sig,dom,dollar) ==
  slot4 := domainData dom 
  [.,catVec,:.] := slot4
  # catVec = 0 => nil                      --early exit if no categories
  $lookupDefaults : local := false
  if $monitorNewWorld then sayBrightly concat('"----->",
    form2String devaluate dom,'"-----> searching default packages for ",op)
  predvec := domainPredicates dom
  packageVec := first slot4
--the next three lines can go away with new category world
  varList := ['$,:$FormalMapVariableList]
  valueList := [dom,:[domainRef(dom,5+i) for i in 1..(# rest dom.0)]]
  valueList := [MKQ val for val in valueList]
  nsig := MSUBST(canonicalForm dom,canonicalForm dollar,sig)
  for i in 0..maxIndex packageVec |
       (entry := vectorRef(packageVec,i)) and entry isnt 'T repeat
    package :=
      vector? entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      ident? entry =>
        cat := vectorRef(catVec,i)
        packageForm := nil
        if not dbLoaded? constructorDB entry then
          loadLib entry
        infovec := property(entry,'infovec)
        success :=
          [.,opvec,:.] := infovec
          max := maxIndex opvec
          code := getOpCode(op,opvec,max)
          null code => nil
          [.,.,.,[.,.,.,:byteVector],:.] := infovec
          endPos :=
            code + 2 > max => # byteVector
            vectorRef(opvec,code+2)
          not nrunNumArgCheck(#sig.source,byteVector,opvec.code,endPos) => nil
          packageForm := [entry,'$,:rest cat]
          package := evalSlotDomain(packageForm,dom)
          vectorRef(packageVec,i) := package
          package
        success = nil =>
          if $monitorNewWorld then
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
      if $monitorNewWorld then
        sayBrightly '"candidate default package succeeds"
      return res
    if $monitorNewWorld then
      sayBrightly '"candidate fails -- continuing to search categories"
    nil
 
nrunNumArgCheck(num,bytevec,start,finish) ==
   args := arrayRef(bytevec,start)
   num = args => true
   (start := start + args + 4) = finish => nil
   nrunNumArgCheck(num,bytevec,start,finish)
 
--=======================================================
--         Compare Signature to One Derived from Table
--=======================================================
newCompareSig(sig, numvec, index, dollar, domain) ==
  k := index
  null (target := first sig)
   or lazyMatchArg(target,arrayRef(numvec,k),dollar,domain) =>
     and/[lazyMatchArg(s,arrayRef(numvec,k := i),dollar,domain)
              for s in rest sig for i in (index+1)..] =>
                    arrayRef(numvec,k + 1)
     nil
  nil
 
--=======================================================
--     Compare Signature to One Derived from Table
--=======================================================
lazyMatchArg(s,a,dollar,domain) == lazyMatchArg2(s,a,dollar,domain,true)
 
lazyMatchArg2(s,a,dollar,domain,typeFlag) ==
  if s is '$ then
    s := devaluate dollar -- calls from HasCategory can have $s
  integer? a =>
    not typeFlag => s = domainRef(domain,a)
    a = 6 and $isDefaultingPackage => s = devaluate dollar
    d := domainVal(dollar,domain,a)
    vector? d =>
      s = canonicalForm d => true
      domainArg := ($isDefaultingPackage => domain.6.0; domain.0)
      KAR s = canonicalForm(d).op and
        lazyMatchArgDollarCheck(replaceSharpCalls s,d.0,dollar.0,domainArg)
    lazyMatch(replaceSharpCalls s,d,dollar,domain)       --new style
  a is '$ => s = devaluate dollar
  a is "$$" => s = devaluate domain
  string? a =>
    string? s => a = s
    s is ['QUOTE,y] and PNAME y = a
    ident? s and symbolName s = a
  a isnt [.,:.] =>  a = s
  op := a.op
  op is '%eval => s = nrtEval(second a,domain)
  op is 'QUOTE => s = second a
  lazyMatch(s,a,dollar,domain)
  --above line is temporarily necessary until system is compiled 8/15/90
--s = a

++ Return true if the symbol `s' designates a generalized builtin
++ constructor, that is a builtin constructor or any operator we
++ deem as a constructor from the domain slot-filling machinery perspective.
generalizedBuiltinConstructor? s ==
  builtinConstructor? s or s is 'QUOTE or s is "[||]"

lazyMatch(source,lazyt,dollar,domain) ==
  lazyt is [op,:argl] and cons? source and op=first source
    and #(sargl := rest source) = #argl =>
      builtinConstructor? op and first argl is [":",:.] =>
        and/[stag = atag and lazyMatchArg(s,a,dollar,domain)
              for [.,stag,s] in sargl for [.,atag,a] in argl]
      generalizedBuiltinConstructor? op =>
         and/[lazyMatchArg(s,a,dollar,domain) for s in sargl for a in argl]
      coSig := getDualSignature op
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
  scoSig := getDualSignature opOf s or return nil
  if opOf s in '(Union Mapping Record) then 
     scoSig := [true for x in s]
  and/[fn for x in rest s for arg in rest d for xt in rest scoSig] where
   fn() ==
    x = arg => true
    x is ['elt,someDomain,opname] => lookupInDomainByName(opname,evalDomain someDomain,arg)
    x is '$ and (arg = dollarName or arg = domainName) => true
    x = dollarName and arg = domainName => true
    x isnt [.,:.] or arg isnt [.,:.] => false
    xt and first x = first arg =>
      lazyMatchArgDollarCheck(x,arg,dollarName,domainName)
    false

lookupInDomainByName(op,domain,arg) ==
  arg isnt [.,:.] => nil
  opvec := domainOperatorTable domain
  numvec := getDomainByteVector domain
  predvec := domainPredicates domain
  max := maxIndex opvec
  k := getOpCode(op,opvec,max) or return nil
  idxmax := maxIndex numvec
  start := vectorRef(opvec,k)
  finish :=
    max > k => vectorRef(opvec,k + 2)
    idxmax
  if finish > idxmax then systemError '"limit too large"
  success := false
  while finish > start repeat
    i := start
    numberOfArgs := arrayRef(numvec,i)
    predIndex := arrayRef(numvec,i := i + 1)
    predIndex ~= 0 and not testBitVector(predvec,predIndex) => nil
    slotIndex := arrayRef(numvec,i + 2 + numberOfArgs)
    newStart := start + numberOfArgs + 4
    slot := domainRef(domain,slotIndex)
    cons? slot and sameObject?(first slot,first arg) and sameObject?(rest slot,rest arg) => return (success := true)
    start := start + numberOfArgs + 4
  success
 
--=======================================================
--        Expand Signature from Encoded Slot Form
--=======================================================
newExpandTypeSlot(slot, dollar, domain) ==
-- returns domain form for dollar.slot
   newExpandLocalType(sigDomainVal(dollar, domain, slot), dollar,domain)
 
newExpandLocalType(lazyt,dollar,domain) ==
  vector? lazyt => canonicalForm lazyt
  lazyt isnt [.,:.] => lazyt
  lazyt is [vec,.,:lazyForm] and vector? vec =>              --old style
    newExpandLocalTypeForm(lazyForm,dollar,domain)
  newExpandLocalTypeForm(lazyt,dollar,domain)             --new style
 
newExpandLocalTypeForm([functorName,:argl],dollar,domain) ==
  functorName is ":" =>
    [":",first argl,newExpandLocalTypeArgs(second argl,dollar,domain,true)]
  functorName is "[||]" =>
    [functorName,newExpandLocalTypeArgs(first argl,dollar,domain,true)]
  functorName is 'QUOTE  => [functorName,:argl]
  builtinConstructor? functorName =>
    [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,true) for a in argl]]
  coSig := getDualSignature functorName or
     error ["unknown constructor name", functorName]
  [functorName,:[newExpandLocalTypeArgs(a,dollar,domain,flag)
        for a in argl for flag in rest coSig]]
 
newExpandLocalTypeArgs(u,dollar,domain,typeFlag) ==
  u is '$ => u
  integer? u =>
     typeFlag => newExpandTypeSlot(u, dollar,domain)
     domainRef(domain,u)
  u is ['%eval,y] => nrtEval(y,domain)
  u is ['QUOTE,y] => y
  u is "$$" => canonicalForm domain
  u isnt [.,:.] => u   --can be first, rest, etc.
  newExpandLocalTypeForm(u,dollar,domain)
 
nrtEval(expr,dom) ==
  $: local := dom             --??? this should be a substitute
  eval expr
 
domainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => dollar
  index = 2 => domain
  domainRef(domain,index)

-- ??? This function should be merged into the preceding one. 
sigDomainVal(dollar,domain,index) ==
--returns a domain or a lazy slot
  index = 0 => "$"
  index = 2 => domain
  domainRef(domain,index)

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
  domainRef(thisDomain,slot) := slotDomain
 

++ Return a type form where all niladic constructors are
++ resolved to constructor calls.  Note: it is assumed that no
++ such resolution has already occured.
resolveNiladicConstructors form ==
  ident? form and niladicConstructor? form => [form]
  form isnt [.,:.] => form
  form is ['QUOTE,:.] => form
  for args in tails rest form repeat
    args.first := resolveNiladicConstructors first args
  form

--=======================================================
--                   HasCategory/Attribute
--=======================================================
domainEqual(a,b) == 
  vector? a and vector? b and a.0 = b.0
 
HasAttribute(domain,attrib) ==
  integer? domainRef(domain,3) => newHasAttribute(domain,attrib)
  (u := LASSOC(attrib,domain.2)) and lookupPred(first u,domain,domain)
 
newHasAttribute(domain,attrib) ==
  predIndex := LASSOC(attrib,domain.2) =>
    predIndex = 0 => true
    predvec := domainPredicates domain
    testBitVector(predvec,predIndex)
  false

newHasCategory(domain,catform) ==
  catform = $Type or catform = $Category => true  
  catform is ["Join",:cats] => 
    and/[newHasCategory(domain,cat) for cat in cats]
  slot4 := domain.4
  auxvec := first slot4
  catvec := second slot4
  $isDefaultingPackage: local := defaultPackageForm? devaluate domain
  #catvec > 0 and integer? KDR catvec.0 =>              --old style
    predIndex := lazyMatchAssocV1(catform,catvec,domain)
    null predIndex => false
    predIndex = 0 => true
    predvec := domainPredicates domain
    testBitVector(predvec,predIndex)
  lazyMatchAssocV(catform,auxvec,catvec,domain)         --new style

HasSignature(domain,[op,sig]) ==
  compiledLookup(op,sig,domain)
 
HasCategory(domain,catform') ==
  catform' is ['SIGNATURE,:f] => HasSignature(domain,f)
  catform' is ['ATTRIBUTE,f] => HasAttribute(domain,f)
  catform:= devaluate catform'
  integer? domainRef(domain,3) => newHasCategory(domain,catform)
  domain0 := canonicalForm domain -- handles old style domains, Record, Union etc.
  slot4 := domainData domain
  catlist := slot4.1
  member(catform,catlist) or
   opOf(catform) in '(Object Type) or  --temporary hack
    or/[compareSigEqual(catform,cat,domain0,domain) for cat in catlist]

-- PLEASE NOTE: This function has the rather charming side-effect that
-- e.g. it works if domform is an Aldor Category.  This is being used
-- by extendscategoryForm in c-util to allow Aldor domains to be used
-- in spad code.  Please do not break this!  An example is the use of
-- Interval (an Aldor domain) by SIGNEF in limitps.spad.  MCD.
newHasTest(domform,catOrAtt) ==
  catOrAtt is '(Type) => true
  cons? domform and builtinFunctorName? domform.op =>
    ofCategory(domform,catOrAtt)
  op := opOf catOrAtt
  isAtom := catOrAtt isnt [.,:.]
  not isAtom and op is 'Join =>
    and/[newHasTest(domform,x) for x in rest catOrAtt]
-- we will refuse to say yes for 'Cat has Cat'
--getConstructorKindFromDB opOf domform = "category" => throwKeyedMsg("S2IS0025",nil)
-- on second thoughts we won't!
  categoryForm? domform =>
      domform = catOrAtt => 'T
      for [aCat,:cond] in [:ancestorsOf(domform,nil),:applySubst(pairList($FormalMapVariableList,rest domform),getConstructorAttributes(opOf domform))] |  aCat = catOrAtt  repeat
         return evalCond cond where
           evalCond x ==
             x isnt [.,:.] => x
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
  n := maxIndex catvec
  xop := first x
  or/[vectorRef(auxvec,i) for i in 0..n |
    xop = first (lazyt := vectorRef(catvec,i)) and lazyMatch(x,lazyt,domain,domain)]
 
lazyMatchAssocV1(x,vec,domain) ==               --old style slot4
  n  := maxIndex vec
  xop := first x
  or/[rest vectorRef(vec,i) for i in 0..n |
    xop = first (lazyt := first vectorRef(vec,i)) and lazyMatch(x,lazyt,domain,domain)]
 
--=======================================================
--                   Utility Functions
--=======================================================
 
sayLooking(prefix,op,sig,dom) ==
  $monitorNewWorld := false
  dollar := devaluate dom
  dollar isnt [.,:.] or vector? dollar or "or"/[vector? x for x in dollar] => systemError nil
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
