-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2010, Gabriel Dos Reis.
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

$domainTypeTokens := ['lazyOldAxiomDomain, 'oldAxiomDomain, 'oldAxiomPreCategory,
           'oldAxiomCategory, 0]

-- The name game.
-- The compiler produces names that are of the form:
-- a) cons(0, <string>)
-- b) cons(1, type-name, arg-names...)
-- c) cons(2, arg-names...)
-- d) cons(3, value)
-- NB: (c) is for tuple-ish constructors, 
--     and (d) is for dependent types.

DNameStringID := 0
DNameApplyID  := 1
DNameTupleID  := 2
DNameOtherID  := 3

DNameToSExpr1 dname ==
  null dname => error "unexpected domain name"
  first dname = DNameStringID => 
    INTERN(CompStrToString rest dname)
  name0 := DNameToSExpr1 second dname
  args  := rest rest dname
  name0 = '_-_> => 
    froms := first args
    froms := MAPCAR(function DNameToSExpr, rest froms)
    ret   := second args -- a tuple
    ret   := DNameToSExpr second ret -- contents
    ['Mapping,:[ret,:froms]]
  name0 = 'Union or name0 = 'Record =>
    sxs := MAPCAR(function DNameToSExpr, rest first args)
    [name0,:sxs]
  name0 = 'Enumeration =>
    [name0,:MAPCAR(function DNameFixEnum, rest first args)]
  [name0,:MAPCAR(function DNameToSExpr, args)]

DNameToSExpr dname ==
  first dname = DNameOtherID  =>
        rest dname
  sx := DNameToSExpr1 dname
  cons? sx => sx
  [sx]

DNameFixEnum arg == CompStrToString rest arg
  
SExprToDName(sexpr, cosigVal) == 
  -- is it a non-type valued object?
  NOT cosigVal => [DNameOtherID, :sexpr]
  if first sexpr = '_: then sexpr := third sexpr
  first sexpr = 'Mapping =>
    args := [ SExprToDName(sx,true) for sx in rest sexpr]
    [DNameApplyID,
         [DNameStringID,: StringToCompStr '"->"],
              [DNameTupleID, : rest args],
                 [DNameTupleID, first args]]
  name0 :=   [DNameStringID, : StringToCompStr SYMBOL_-NAME first sexpr]
  first sexpr = 'Union or first sexpr = 'Record =>
    [DNameApplyID, name0, 
        [DNameTupleID,: [ SExprToDName(sx,true) for sx in rest sexpr]]]
  newCosig := rest getDualSignatureFromDB first sexpr
  [DNameApplyID, name0,
   : MAPCAR(function SExprToDName, rest sexpr, newCosig)]

-- local garbage because Compiler strings are null terminated
StringToCompStr(str) == 
   strconc(str, STRING (CODE_-CHAR 0))

CompStrToString(str) == 
   SUBSTRING(str, 0, (# str - 1))
-- local garbage ends

runOldAxiomFunctor(:allArgs) ==
  [:args,env] := allArgs
  getConstructorKindFromDB env = "category" =>
      [$oldAxiomPreCategoryDispatch,: [env, :args]]
  dom:=apply(env, args)
  makeOldAxiomDispatchDomain dom

makeLazyOldAxiomDispatchDomain domform ==
  attribute? domform =>
      [$attributeDispatch, domform, hashString(SYMBOL_-NAME domform)]
  getConstructorKindFromDB opOf domform = "category" =>
      [$oldAxiomPreCategoryDispatch,: domform]
  dd := [$lazyOldAxiomDomainDispatch, hashTypeForm(domform,0), domform]
  NCONC(dd,dd) -- installs back pointer to head of domain.
  dd

makeOldAxiomDispatchDomain dom ==
  cons? dom => dom
  [$oldAxiomDomainDispatch,hashTypeForm(dom.0,0),:dom]

closeOldAxiomFunctor(name) ==
   [function runOldAxiomFunctor,:SYMBOL_-FUNCTION name]

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
   opIsHasCat op => if EQL(sig, hash) then [self] else nil
   null(pack) => nil
   if not VECP pack then
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
   StringToCompStr SYMBOL_-NAME name

attributeLookupExport(attrObj, self, op, sig, box, env) ==
   [name, hash] := attrObj
   opIsHasCat op => if EQL(hash, sig) then [self] else nil

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


orderedDefaults(conform,domform) ==
  $depthAssocCache : local := hashTable 'EQ
  conList := [x for x in orderCatAnc (op := opOf conform) | hasDefaultPackage op]
  acc := nil
  ancestors := ancestorsOf(conform,domform)
  for x in conList repeat
    for y in ancestors | x = CAAR y repeat acc := [y,:acc]
  nreverse acc

instantiate domenv ==
   -- following is a patch for a bug in runtime.as
   -- has a lazy dispatch vector with an instantiated domenv
  VECTORP rest domenv => [$oldAxiomDomainDispatch ,: domenv]
  callForm := second domenv
  oldDom := CDDR domenv
  [functor,:args] := callForm
--  if null(fn := GETL(functor,'instantiate)) then
--     ofn := SYMBOL_-FUNCTION functor
--     loadFunctor functor
--     fn := SYMBOL_-FUNCTION functor
--     SETF(SYMBOL_-FUNCTION functor, ofn)
--     PUT(functor, 'instantiate, fn)
--  domvec := apply(fn, args)
  domvec := apply(functor, args)
  oldDom.first := $oldAxiomDomainDispatch
  oldDom.rest := [second oldDom,: domvec]
  oldDom

hashTypeForm([fn,: args], percentHash) == 
   hashType([fn,:devaluateList args], percentHash)

$hashOp1 := hashString '"1"
$hashOp0 := hashString '"0"
$hashOpApply := hashString '"apply"
$hashOpSet := hashString '"set!"
$hashSeg := hashString '".."
$hashPercent := hashString '"%"

oldAxiomDomainLookupExport _
  (domenv, self, op, sig, box, skipdefaults, env) ==
     domainVec := rest domenv
     if hashCode? op then
         EQL(op, $hashOp1) => op := 'One
         EQL(op, $hashOp0) => op := 'Zero
         EQL(op, $hashOpApply) => op := 'elt
         EQL(op, $hashOpSet) => op := 'setelt
         EQL(op, $hashSeg) => op := 'SEGMENT
     constant := nil
     if hashCode? sig and self and EQL(sig, getDomainHash self) then
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
  not VECP(dispatch := first dollar) => error "bad domain format"
  lookupFun := dispatch.3
  dispatch.0 = 0  =>  -- new compiler domain object
       hashPercent :=
          VECP dollar => hashType(dollar.0,0)
          hashType(dollar,0)

       hashSig :=
         hashCode? sig => sig
         hashType( ['Mapping,:sig], hashPercent)

       if symbol? op then op := hashString SYMBOL_-NAME op
       first SPADCALL(rest dollar, dollar, op, hashSig, box, not $lookupDefaults, lookupFun)
  first SPADCALL(rest dollar, dollar, op, sig, box, not $lookupDefaults, lookupFun)

$hasCatOpHash := hashString '"%%"
opIsHasCat op ==
  hashCode? op => EQL(op, $hasCatOpHash)
  op = "%%"

-- has cat questions lookup up twice if false
-- replace with following ?
--  not(opIsHasCat op) and
--     (u := lookupInDomainVector(op,sig,domvec,domvec)) => u

oldCompLookup(op, sig, domvec, dollar) ==
  $lookupDefaults:local := nil
  u := lookupInDomainVector(op,sig,domvec,dollar) => u
  $lookupDefaults := true
  lookupInDomainVector(op,sig,domvec,dollar)

oldCompLookupNoDefaults(op, sig, domvec, dollar) ==
  $lookupDefaults:local := nil
  lookupInDomainVector(op,sig,domvec,dollar)

hashNewLookupInTable(op,sig,dollar,[domain,opvec],flag) ==
  opIsHasCat op =>
      HasCategory(domain, sig)
  if hashCode? op and EQL(op, $hashOp1) then op := 'One
  if hashCode? op and EQL(op, $hashOp0) then op := 'Zero
  hashPercent :=
    VECP dollar => hashType(dollar.0,0)
    hashType(dollar,0)
  if hashCode? sig and EQL(sig, hashPercent) then 
         sig := hashType('(Mapping $), hashPercent)
  dollar = nil => systemError()
  $lookupDefaults = true =>
    hashNewLookupInCategories(op,sig,domain,dollar)      --lookup first in my cats
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
  numArgs := if hashCode? sig then -1 else (#sig)-1
  success := nil
  $isDefaultingPackage: local :=
    -- use special defaulting handler when dollar non-trivial
    dollar ~= domain and isDefaultPackageForm? devaluate domain
  while finish > start repeat
    PROGN
      i := start
      numTableArgs :=numvec.i
      predIndex := numvec.(i := QSADD1 i)
      (predIndex ~= 0) and null testBitVector(predvec,predIndex) => nil
      exportSig :=
          [newExpandTypeSlot(numvec.(i + j + 1),
            dollar,domain) for j in 0..numTableArgs]
      sig ~= hashType(['Mapping,: exportSig],hashPercent) => nil --signifies no match
      loc := numvec.(i + numTableArgs + 2)
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
        --if EQ(QCAR slot,'newGoGet) then
        --  UNWIND_-PROTECT --break infinite recursion
        --    ((SETELT(domain,loc,'skip); slot := replaceGoGetSlot rest slot),
        --      if domain.loc = 'skip then domain.loc := slot)
        return (success := slot)
      slot = 'skip =>       --recursive call from above 'replaceGoGetSlot
        return (success := newLookupInAddChain(op,sig,domain,dollar))
      systemError '"unexpected format"
    start := QSPLUS(start,QSPLUS(numTableArgs,4))
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
  slot4 := dom.4
  catVec := second slot4
  SIZE catVec = 0 => nil                      --early exit if no categories
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
       (entry := packageVec.i) and entry ~= true repeat
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
            --not nrunNumArgCheck(#sig.source,byteVector,opvec.code,endPos) => nil
            --numOfArgs := byteVector.(opvec.code)
            --numOfArgs ~= #sig.source => nil
            packageForm := [entry,'$,:rest cat]
            package := evalSlotDomain(packageForm,dom)
            packageVec.i := package
            package
                           ----old world
          table := HGET($Slot1DataBase,entry) or systemError nil
          (u := LASSQ(op,table))
            and (v := or/[rest x for x in u]) =>
              packageForm := [entry,'$,:rest cat]
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

HasAttribute(domain,attrib) ==
  hashPercent :=
       VECP domain => hashType(domain.0,0)
       hashType(domain,0)
  isDomain domain =>
     FIXP((first domain).0) => 
        -- following call to hashType was missing 2nd arg. 
        -- getDomainHash domain added on 4/01/94 by RSS
        basicLookup("%%",hashType(attrib, hashPercent),domain,domain)
     HasAttribute(CDDR domain, attrib)
-->
  isNewWorldDomain domain => newHasAttribute(domain,attrib)
--+
  (u := LASSOC(attrib,domain.2)) and lookupPred(first u,domain,domain)
 
newHasAttribute(domain,attrib) ==
  hashPercent :=
       VECP domain => hashType(domain.0,0)
       hashType(domain,0)
  predIndex :=
     hashCode? attrib =>
        -- following call to hashType was missing 2nd arg. 
        -- hashPercent added by PAB 15/4/94
        or/[x for x in domain.2 | attrib = hashType(first x, hashPercent)]
     LASSOC(attrib,domain.2)
  predIndex =>
    predIndex = 0 => true
    predvec := domain.3
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
    predvec := QVELT(domain,3)
    testBitVector(predvec,predIndex)
  lazyMatchAssocV(catform,auxvec,catvec,domain)         --new style

getCatForm(catvec, index, domain) ==
   NUMBERP(form := QVELT(catvec,index)) => domain.form
   form

HasSignature(domain,[op,sig]) ==
  compiledLookup(op,sig,domain)
 
HasCategory(domain,catform') ==
  catform' is ['SIGNATURE,:f] => HasSignature(domain,f)
  catform' is ['ATTRIBUTE,f] => HasAttribute(domain,f)
  isDomain domain =>
     FIXP((first domain).0) =>
        catform' := devaluate catform'
        basicLookup("%%",catform',domain,domain)
     HasCategory(CDDR domain, catform')
  catform:= devaluate catform'
  isNewWorldDomain domain => newHasCategory(domain,catform)
  domain0:=domain.0 -- handles old style domains, Record, Union etc.
  slot4 := domain.4
  catlist := slot4.1
  member(catform,catlist) or
   opOf(catform) in '(Object Type) or  --temporary hack
    or/[compareSigEqual(catform,cat,domain0,domain) for cat in catlist]

--systemDependentMkAutoload(fn,cnam) ==
--    FBOUNDP(cnam) => "next"
--    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))

domainEqual(a,b) == 
  VECP a and VECP b and a.0 = b.0
 
