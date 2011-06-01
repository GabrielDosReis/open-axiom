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
import hashcode
namespace BOOT

-- note domainObjects are now (dispatchVector hashCode . domainVector)
-- lazy oldAxiomDomainObjects are (dispatchVector hashCode  (Call form) . backptr), 
-- pre oldAxiomCategory is (dispatchVector . (cat form))
-- oldAxiomCategory objects are (dispatchVector . ( (cat form)  hash defaultpack parentlist))

hashCode? x == integer? x

$domainTypeTokens == ['lazyOldAxiomDomain, 'oldAxiomDomain, 'oldAxiomPreCategory,
           'oldAxiomCategory, 0]

-- The name game.
-- The compiler produces names that are of the form:
-- a) cons(0, <string>)
-- b) cons(1, type-name, arg-names...)
-- c) cons(2, arg-names...)
-- d) cons(3, value)
-- NB: (c) is for tuple-ish constructors, 
--     and (d) is for dependent types.

DNameStringID == 0
DNameApplyID  == 1
DNameTupleID  == 2
DNameOtherID  == 3

DNameToSExpr1 dname ==
  null dname => error "unexpected domain name"
  first dname = DNameStringID => 
    makeSymbol(CompStrToString rest dname)
  name0 := DNameToSExpr1 second dname
  args  := rest rest dname
  name0 is '_-_> => 
    froms := first args
    froms := [DNameToSExpr x for x in rest froms]
    ret   := second args -- a tuple
    ret   := DNameToSExpr second ret -- contents
    ['Mapping,:[ret,:froms]]
  name0 is 'Union or name0 is 'Record =>
    sxs := [DNameToSExpr x for x in rest first args]
    [name0,:sxs]
  name0 is 'Enumeration =>
    [name0,:[DNameFixEnum x for x in rest first args]]
  [name0,:[DNameToSExpr x for x in args]]

DNameToSExpr dname ==
  first dname = DNameOtherID  =>
        rest dname
  sx := DNameToSExpr1 dname
  cons? sx => sx
  [sx]

DNameFixEnum arg == CompStrToString rest arg
  
SExprToDName(sexpr, cosigVal) == 
  -- is it a non-type valued object?
  not cosigVal => [DNameOtherID, :sexpr]
  if first sexpr is '_: then sexpr := third sexpr
  first sexpr is 'Mapping =>
    args := [ SExprToDName(sx,true) for sx in rest sexpr]
    [DNameApplyID,
         [DNameStringID,: StringToCompStr '"->"],
              [DNameTupleID, : rest args],
                 [DNameTupleID, first args]]
  name0 :=   [DNameStringID, : StringToCompStr symbolName first sexpr]
  first sexpr is 'Union or first sexpr is 'Record =>
    [DNameApplyID, name0, 
        [DNameTupleID,: [ SExprToDName(sx,true) for sx in rest sexpr]]]
  newCosig := rest getDualSignatureFromDB first sexpr
  [DNameApplyID, name0,
   :[SExprToDName(x,f) for x in rest sexpr for f in newCosig]]

-- local garbage because Compiler strings are null terminated
StringToCompStr(str) == 
   strconc(str, charString abstractChar 0)

CompStrToString(str) == 
   subString(str, 0, #str - 1)
-- local garbage ends

runOldAxiomFunctor(:allArgs) ==
  [:args,env] := allArgs
  getConstructorKindFromDB env is "category" =>
      [$oldAxiomPreCategoryDispatch,: [env, :args]]
  dom:=apply(env, args)
  makeOldAxiomDispatchDomain dom

makeLazyOldAxiomDispatchDomain domform ==
  attribute? domform =>
      [$attributeDispatch, domform, hashString(symbolName domform)]
  getConstructorKindFromDB opOf domform is "category" =>
      [$oldAxiomPreCategoryDispatch,: domform]
  dd := [$lazyOldAxiomDomainDispatch, hashTypeForm(domform,0), domform]
  append!(dd,dd) -- installs back pointer to head of domain.
  dd

makeOldAxiomDispatchDomain dom ==
  cons? dom => dom
  [$oldAxiomDomainDispatch,hashTypeForm(dom.0,0),:dom]

closeOldAxiomFunctor(name) ==
   [function runOldAxiomFunctor,:symbolFunction name]

lazyOldAxiomDomainLookupExport(domenv, self, op, sig, box, skipdefaults, env) ==
  dom := instantiate domenv
  SPADCALL(rest dom, self, op, sig, box, skipdefaults, first dom.3)

lazyOldAxiomDomainHashCode(domenv, env) == first domenv

lazyOldAxiomDomainDevaluate(domenv, env) ==
  dom := instantiate domenv
  SPADCALL(rest dom, first dom.1)

lazyOldAxiomAddChild(domenv, kid, env) ==
  [$lazyOldAxiomDomainDispatch,:domenv]

$lazyOldAxiomDomainDispatch :=
   VECTOR('lazyOldAxiomDomain,
          [function lazyOldAxiomDomainDevaluate],
          [nil],
          [function lazyOldAxiomDomainLookupExport],
          [function lazyOldAxiomDomainHashCode],
          [function lazyOldAxiomAddChild])

-- old Axiom pre category objects are just (dispatch . catform)
-- where catform is ('categoryname,: evaluated args)
-- old Axiom category objects are  (dispatch . [catform, hashcode, defaulting package, parent vector, dom])
oldAxiomPreCategoryBuild(catform, dom, env) ==
   pack := oldAxiomCategoryDefaultPackage(catform, dom)
   [$oldAxiomCategoryDispatch,
       :[catform, hashTypeForm(catform,0), pack, oldAxiomPreCategoryParents(catform,dom), dom]]
oldAxiomPreCategoryHashCode(catform, env) == hashTypeForm(catform,0)
oldAxiomCategoryDefaultPackage(catform, dom) ==
    hasDefaultPackage opOf catform 

oldAxiomPreCategoryDevaluate([op,:args], env) ==
   SExprToDName([op,:devaluateList args], true)

$oldAxiomPreCategoryDispatch :=
   VECTOR('oldAxiomPreCategory,
          [function oldAxiomPreCategoryDevaluate],
          [nil],
          [nil],
          [function oldAxiomPreCategoryHashCode],
          [function oldAxiomPreCategoryBuild],
          [nil])

oldAxiomCategoryDevaluate([[op,:args],:.], env) ==
   SExprToDName([op,:devaluateList args], true)

oldAxiomPreCategoryParents(catform,dom) ==
  vars := ["$",:rest getConstructorFormFromDB opOf catform]
  vals := [dom,:rest catform]
  -- parents :=  getConstructorParentsFromDB opOf catform
  parents := parentsOf opOf catform
  PROGV(vars, vals,
    LIST2VEC
      [eval quoteCatOp cat for [cat,:pred] in parents | eval pred])

quoteCatOp cat == 
   atom cat => MKQ cat
   ['LIST, MKQ first cat,: rest cat]


oldAxiomCategoryLookupExport(catenv, self, op, sig, box, env) ==
   [catform,hash, pack,:.] := catenv
   opIsHasCat op => if scalarEq?(sig, hash) then [self] else nil
   null(pack) => nil
   if not vector? pack then
       pack:=apply(pack, [self, :rest catform])
       catenv.rest.rest.first := pack
   fun := basicLookup(op, sig, pack, self) => [fun]
   nil

oldAxiomCategoryParentCount([.,.,.,parents,.], env) == # parents
oldAxiomCategoryNthParent([.,.,.,parvec,dom], n, env) ==
  catform := parvec.(n-1)
  VECTORP KAR catform => catform
  newcat := oldAxiomPreCategoryBuild(catform,dom,nil)
  parvec.(n-1) := newcat
  newcat

oldAxiomCategoryBuild([catform,:.], dom, env) ==
  oldAxiomPreCategoryBuild(catform,dom, env)
oldAxiomCategoryHashCode([.,hash,:.], env) == hash

$oldAxiomCategoryDispatch :=
   VECTOR('oldAxiomCategory,
          [function oldAxiomCategoryDevaluate],
          [nil],
          [function oldAxiomCategoryLookupExport],
          [function oldAxiomCategoryHashCode],
          [function oldAxiomCategoryBuild], -- builder ??
          [function oldAxiomCategoryParentCount],
          [function oldAxiomCategoryNthParent]) -- 1 indexed

attributeDevaluate(attrObj, env) ==
   [name, hash] := attrObj
   StringToCompStr symbolName name

attributeLookupExport(attrObj, self, op, sig, box, env) ==
   [name, hash] := attrObj
   opIsHasCat op => if scalarEq?(hash, sig) then [self] else nil

attributeHashCode(attrObj, env) ==
   [name, hash] := attrObj
   hash

attributeCategoryBuild(attrObj, dom, env) ==
   [name, hash] := attrObj
   [$attributeDispatch, name, hash] 

attributeCategoryParentCount(attrObj, env) == 0

attributeNthParent(attrObj, env) == nil

$attributeDispatch :=
   VECTOR('attribute,
          [function attributeDevaluate],
          [nil],
          [function attributeLookupExport],
          [function attributeHashCode],
          [function attributeCategoryBuild], -- builder ??
          [function attributeCategoryParentCount],
          [function attributeNthParent]) -- 1 indexed


--=======================================================================
--             Generate Category Level Alist
--=======================================================================
orderCatAnc x ==
  reverse! ASSOCLEFT SORTBY(function rest,rest depthAssoc x)
 
depthAssocList u == 
  u := removeSymbol(u,'DomainSubstitutionMacro)  --hack by RDJ 8/90
  removeDuplicates ("append"/[depthAssoc(y) for y in u])
 
depthAssoc x ==
  y := tableValue($depthAssocCache,x) => y
  x is ['Join,:u] or (u := getCatAncestors x) =>
    v := depthAssocList u
    tableValue($depthAssocCache,x) := [[x,:n],:v]
      where n() == 1 + "MAX"/[rest y for y in v]
  tableValue($depthAssocCache,x) := [[x,:0]]
 
getCatAncestors x ==  [CAAR y for y in parentsOf opOf x]
 
listOfEntries form ==
  atom form => form
  form is [op,:l] =>
    op is 'Join => "append"/[listOfEntries x for x in l]
    op is 'CATEGORY => listOfCategoryEntries rest l
    op is 'PROGN => listOfCategoryEntries l
    op is 'ATTRIBUTE and first l is [f,:.] and constructor? f => [first l]
    op in '(ATTRIBUTE SIGNATURE) => nil
    [form]
  categoryFormatError()
 
listOfCategoryEntries l ==
  null l => nil
  l is [[op,:u],:v] =>
    firstItemList:=
      op is 'ATTRIBUTE and first u is [f,:.] and constructor? f =>
        [first u]
      op in '(ATTRIBUTE SIGNATURE) => nil
      op is 'IF and u is [pred,conseq,alternate] =>
          listOfCategoryEntriesIf(pred,conseq,alternate)
      categoryFormatError()
    [:firstItemList,:listOfCategoryEntries v]
  l is ['PROGN,:l] => listOfCategoryEntries l
  l is '(NIL) => nil
  sayBrightly '"unexpected category format encountered:"
  pp l
 
listOfCategoryEntriesIf(pred,conseq,alternate) ==
  alternate in '(%noBranch NIL) =>
    conseq is ['IF,p,c,a] => listOfCategoryEntriesIf(makePrefixForm([pred,p],'AND),c,a)
    [fn for x in listOfEntries conseq] where fn() ==
      x is ['IF,a,b] => ['IF,makePrefixForm([pred,a],'AND),b]
      ['IF,pred,x]
  notPred := makePrefixForm(pred,'NOT)
  conseq is ['IF,p,c,a] =>
    listOfCategoryEntriesIf(makePrefixForm([notPred,p],'AND),c,a)
  [gn for x in listOfEntries conseq] where gn() ==
    x is ['IF,a,b] => ['IF,makePrefixForm([notPred,a],'AND),b]
    ['IF,notPred,x]
 
orderedDefaults(conform,domform) ==
  $depthAssocCache : local := hashTable 'EQ
  conList := [x for x in orderCatAnc (op := opOf conform) | hasDefaultPackage op]
  acc := nil
  ancestors := ancestorsOf(conform,domform)
  for x in conList repeat
    for y in ancestors | x = CAAR y repeat acc := [y,:acc]
  reverse! acc

instantiate domenv ==
   -- following is a patch for a bug in runtime.as
   -- has a lazy dispatch vector with an instantiated domenv
  VECTORP rest domenv => [$oldAxiomDomainDispatch ,: domenv]
  callForm := second domenv
  oldDom := CDDR domenv
  [functor,:args] := callForm
--  if null(fn := property(functor,'instantiate)) then
--     ofn := symbolFunction functor
--     loadFunctor functor
--     fn := symbolFunction functor
--     symbolFunction(functor) := ofn
--     proprty(functor, 'instantiate) := fn
--  domvec := apply(fn, args)
  domvec := apply(functor, args)
  oldDom.first := $oldAxiomDomainDispatch
  oldDom.rest := [second oldDom,: domvec]
  oldDom

hashTypeForm([fn,: args], percentHash) == 
   hashType([fn,:devaluateList args], percentHash)

$hashOp1 == hashString '"1"
$hashOp0 == hashString '"0"
$hashOpApply == hashString '"apply"
$hashOpSet == hashString '"set!"
$hashSeg == hashString '".."
$hashPercent == hashString '"%"

oldAxiomDomainLookupExport _
  (domenv, self, op, sig, box, skipdefaults, env) ==
     domainVec := rest domenv
     if hashCode? op then
         scalarEq?(op, $hashOp1) => op := 'One
         scalarEq?(op, $hashOp0) => op := 'Zero
         scalarEq?(op, $hashOpApply) => op := 'elt
         scalarEq?(op, $hashOpSet) => op := 'setelt
         scalarEq?(op, $hashSeg) => op := 'SEGMENT
     constant := nil
     if hashCode? sig and self and scalarEq?(sig, getDomainHash self) then
       sig := '($)
       constant := true
     val :=
       skipdefaults => 
          oldCompLookupNoDefaults(op, sig, domainVec, self)
       oldCompLookup(op, sig, domainVec, self)
     null val => val
     if constant then val := SPADCALL val
     box.first := val
     box
     
oldAxiomDomainHashCode(domenv, env) == first domenv

oldAxiomDomainHasCategory(domenv, cat, env) ==
  HasAttribute(domvec := rest domenv, cat) or
    HasCategory(domvec, devaluate cat)

oldAxiomDomainDevaluate(domenv, env) == 
   SExprToDName(rest domenv.0, 'T)

oldAxiomAddChild(domenv, child, env) == [$oldAxiomDomainDispatch,:domenv]

$oldAxiomDomainDispatch :=
   VECTOR('oldAxiomDomain,
          [function oldAxiomDomainDevaluate],
          [nil],
          [function oldAxiomDomainLookupExport],
          [function oldAxiomDomainHashCode],
          [function oldAxiomAddChild])

basicLookupCheckDefaults(op,sig,domain,dollar) ==
  box := [nil]
  not vector?(dispatch := first dollar) => error "bad domain format"
  lookupFun := dispatch.3
  dispatch.0 = 0  =>  -- new compiler domain object
       hashPercent :=
          vector? dollar => hashType(dollar.0,0)
          hashType(dollar,0)

       hashSig :=
         hashCode? sig => sig
         hashType( ['Mapping,:sig], hashPercent)

       if symbol? op then op := hashString symbolName op
       first SPADCALL(rest dollar, dollar, op, hashSig, box, not $lookupDefaults, lookupFun)
  first SPADCALL(rest dollar, dollar, op, sig, box, not $lookupDefaults, lookupFun)

$hasCatOpHash == hashString '"%%"
opIsHasCat op ==
  hashCode? op => scalarEq?(op, $hasCatOpHash)
  op = "%%"

-- has cat questions lookup up twice if false
-- replace with following ?
--  not(opIsHasCat op) and
--     (u := lookupInDomainVector(op,sig,domvec,domvec)) => u

oldCompLookup(op, sig, domvec, dollar) ==
  $lookupDefaults: local := false
  u := lookupInDomainVector(op,sig,domvec,dollar) => u
  $lookupDefaults := true
  lookupInDomainVector(op,sig,domvec,dollar)

oldCompLookupNoDefaults(op, sig, domvec, dollar) ==
  $lookupDefaults: local := false
  lookupInDomainVector(op,sig,domvec,dollar)

hashNewLookupInTable(op,sig,dollar,[domain,opvec],flag) ==
  opIsHasCat op =>
      HasCategory(domain, sig)
  if hashCode? op and scalarEq?(op, $hashOp1) then op := 'One
  if hashCode? op and scalarEq?(op, $hashOp0) then op := 'Zero
  hashPercent :=
    vector? dollar => hashType(dollar.0,0)
    hashType(dollar,0)
  if hashCode? sig and scalarEq?(sig, hashPercent) then 
         sig := hashType('(Mapping $), hashPercent)
  dollar = nil => systemError()
  $lookupDefaults =>
    hashNewLookupInCategories(op,sig,domain,dollar)      --lookup first in my cats
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
  if finish > idxmax then systemError '"limit too large"
  numArgs := if hashCode? sig then -1 else (#sig)-1
  success := nil
  $isDefaultingPackage: local :=
    -- use special defaulting handler when dollar non-trivial
    dollar ~= domain and isDefaultPackageForm? devaluate domain
  while finish > start repeat
    PROGN
      i := start
      numTableArgs := arrayRef(numvec,i)
      predIndex := arrayRef(numvec,i := i + 1)
      (predIndex ~= 0) and null testBitVector(predvec,predIndex) => nil
      exportSig :=
          [newExpandTypeSlot(numvec.(i + j + 1),
            dollar,domain) for j in 0..numTableArgs]
      sig ~= hashType(['Mapping,: exportSig],hashPercent) => nil --signifies no match
      loc := arrayRef(numvec,i + numTableArgs + 2)
      loc = 1 => (someMatch := true)
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
        nil
      slot := vectorRef(domain,loc)
      cons? slot =>
        slot.op = 'newGoGet => someMatch:=true
                   --treat as if operation were not there
        --if sameObject?(QCAR slot,'newGoGet) then
        --  UNWIND_-PROTECT --break infinite recursion
        --    ((SETELT(domain,loc,'skip); slot := replaceGoGetSlot rest slot),
        --      if domain.loc = 'skip then domain.loc := slot)
        return (success := slot)
      slot is 'skip =>       --recursive call from above 'replaceGoGetSlot
        return (success := newLookupInAddChain(op,sig,domain,dollar))
      systemError '"unexpected format"
    start := start + numTableArgs + 4
  (success ~= 'failed) and success =>
    if $monitorNewWorld then
      sayLooking1('"<----",uu) where uu() ==
        cons? success => [first success,:devaluate rest success]
        success
    success
  subsumptionSig and (u:= basicLookup(op,subsumptionSig,domain,dollar)) => u
  flag or someMatch => newLookupInAddChain(op,sig,domain,dollar)
  nil

hashNewLookupInCategories(op,sig,dom,dollar) ==
  slot4 := vectorRef(dom,4)
  catVec := second slot4
  # catVec = 0 => nil                      --early exit if no categories
  integer? KDR catVec.0 =>
    newLookupInCategories1(op,sig,dom,dollar) --old style
  $lookupDefaults : local := false
  if $monitorNewWorld then sayBrightly concat('"----->",
    form2String devaluate dom,'"-----> searching default packages for ",op)
  predvec := domainPredicates dom
  packageVec := first slot4
--the next three lines can go away with new category world
  varList := ['$,:$FormalMapVariableList]
  valueList := [dom,:[vectorRef(dom,5+i) for i in 1..(# rest dom.0)]]
  valueList := [MKQ val for val in valueList]
  nsig := MSUBST(dom.0,dollar.0,sig)
  for i in 0..maxIndex packageVec |
       (entry := packageVec.i) and entry ~= true repeat
    package :=
      vector? entry =>
         if $monitorNewWorld then
           sayLooking1('"already instantiated cat package",entry)
         entry
      IDENTP entry =>
        cat := catVec.i
        packageForm := nil
        if not property(entry,'LOADED) then loadLib entry
        infovec := property(entry,'infovec)
        success :=
          --vector? infovec =>  ----new world
          true =>  ----new world
            opvec := infovec.1
            max := maxIndex opvec
            code := getOpCode(op,opvec,max)
            null code => nil
            byteVector := CDDDR infovec.3
            endPos :=
              code+2 > max => # byteVector
              vectorRef(opvec,code+2)
            --not nrunNumArgCheck(#sig.source,byteVector,opvec.code,endPos) => nil
            --numOfArgs := byteVector.(opvec.code)
            --numOfArgs ~= #sig.source => nil
            packageForm := [entry,'$,:rest cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
                           ----old world
          table := tableValue($Slot1DataBase,entry) or systemError nil
          (u := LASSQ(op,table))
            and (v := or/[rest x for x in u]) =>
              packageForm := [entry,'$,:rest cat]
              package := evalSlotDomain(packageForm,dom)
              packageVec.i := package
              package
          nil
        null success =>
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

HasAttribute(domain,attrib) ==
  hashPercent :=
       vector? domain => hashType(domain.0,0)
       hashType(domain,0)
  isDomain domain =>
     integer?((first domain).0) => 
        -- following call to hashType was missing 2nd arg. 
        -- getDomainHash domain added on 4/01/94 by RSS
        basicLookup("%%",hashType(attrib, hashPercent),domain,domain)
     HasAttribute(CDDR domain, attrib)
  integer? domainRef(domain,3) => newHasAttribute(domain,attrib)
  (u := LASSOC(attrib,domain.2)) and lookupPred(first u,domain,domain)
 
newHasAttribute(domain,attrib) ==
  hashPercent :=
       vector? domain => hashType(domain.0,0)
       hashType(domain,0)
  predIndex :=
     hashCode? attrib =>
        -- following call to hashType was missing 2nd arg. 
        -- hashPercent added by PAB 15/4/94
        or/[x for x in domain.2 | attrib = hashType(first x, hashPercent)]
     LASSOC(attrib,domain.2)
  predIndex =>
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
  $isDefaultingPackage: local := isDefaultPackageForm? devaluate domain
  #catvec > 0 and integer? KDR catvec.0 =>              --old style
    predIndex := lazyMatchAssocV1(catform,catvec,domain)
    null predIndex => false
    predIndex = 0 => true
    predvec := domainPredicates domain
    testBitVector(predvec,predIndex)
  lazyMatchAssocV(catform,auxvec,catvec,domain)         --new style

getCatForm(catvec, index, domain) ==
   integer?(form := vectorRef(catvec,index)) => domain.form
   form

HasSignature(domain,[op,sig]) ==
  compiledLookup(op,sig,domain)
 
HasCategory(domain,catform') ==
  catform' is ['SIGNATURE,:f] => HasSignature(domain,f)
  catform' is ['ATTRIBUTE,f] => HasAttribute(domain,f)
  isDomain domain =>
     integer?((first domain).0) =>
        catform' := devaluate catform'
        basicLookup("%%",catform',domain,domain)
     HasCategory(CDDR domain, catform')
  catform:= devaluate catform'
  integer? domainRef(domain,3) => newHasCategory(domain,catform)
  domain0 := canonicalForm domain -- handles old style domains, Record, Union etc.
  slot4 := domainRef(domain,4)
  catlist := slot4.1
  member(catform,catlist) or
   opOf(catform) in '(Object Type) or  --temporary hack
    or/[compareSigEqual(catform,cat,domain0,domain) for cat in catlist]

--systemDependentMkAutoload(fn,cnam) ==
--    FBOUNDP(cnam) => "next"
--    symbolFunction(cnam) := mkAutoLoad(fn, cnam)

domainEqual(a,b) == 
  vector? a and vector? b and a.0 = b.0
 
