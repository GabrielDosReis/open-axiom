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
import clam
import interop

namespace BOOT

++
$insideCompileBodyIfTrue := false

--% Monitoring functions

lookupDisplay(op,sig,vectorOrForm,suffix) ==
  null $NRTmonitorIfTrue => nil
  prefix := (suffix = '"" => ">"; "<")
  sayBrightly
    concat(prefix,formatOpSignature(op,sig),
        '" from ", prefix2String devaluateDeeply vectorOrForm,suffix)

isInstantiated [op,:argl] ==
  u:= lassocShiftWithFunction(argl,HGET($ConstructorCache,op),'domainEqualList)
    => CDRwithIncrement u
  nil

--=======================================================
--             Lookup From Interpreter
--=======================================================

NRTevalDomain form ==
  form is ["setShellEntry",:.] => eval form
  form is ['SETELT,:.] => systemErrorHere ["NRTevalDomain",form]
  evalDomain form

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
compiledLookup(op,sig,dollar) ==
--called by coerceByFunction, evalForm, findEqualFun, findUniqueOpInDomain,
--  getFunctionFromDomain, optDeltaEntry, retractByFunction
  if not VECP dollar then dollar := NRTevalDomain dollar
  -- "^" is an alternate name for "**" in OpenAxiom libraries.
  -- ??? When, we get to support Aldor libraries and the equivalence
  -- ??? does not hold, we may want to do the reverse lookup too.
  -- ??? See compiledLookupCheck below.
  if op = "^" then op := "**"
  basicLookup(op,sig,dollar,dollar)

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
basicLookup(op,sig,domain,dollar) ==
  item := domain.1
  cons? item and first item in '(lookupInDomain lookupInTable) => 
    lookupInDomainVector(op,sig,domain,dollar)
  ----------new world code follows------------
  u := lookupInDomainAndDefaults(op,sig,domain,dollar,false) => u
  lookupInDomainAndDefaults(op,sig,domain,dollar,true)

compiledLookupCheck(op,sig,dollar) ==
  fn := compiledLookup(op,sig,dollar)

  -- NEW COMPILER COMPATIBILITY ON
  if (fn = nil)  and (op = "**") then
    fn := compiledLookup("^",sig,dollar)
  -- NEW COMPILER COMPATIBILITY OFF

  fn = nil =>
    keyedSystemError("S2NR0001",[op,formatSignature sig,dollar.0])
  fn

--=======================================================
--                 Lookup From Compiled Code
--=======================================================
goGet(:l) ==
  [:arglist,env] := l
  arglist is ['goGet,:.] => stop()
  [[.,[op,initSig,:code]],thisDomain] := env
  domainSlot := QSQUOTIENT(code,8192)
  code1 := QSREMAINDER(code,8192)
  if QSODDP code1 then isConstant := true
  code2 := QSQUOTIENT(code1,2)
  if QSODDP code2 then explicitLookupDomainIfTrue := true
  index := QSQUOTIENT(code2,2)
  kind := (isConstant = true => 'CONST; 'ELT)
  sig := [NRTreplaceLocalTypes(s,thisDomain) for s in initSig]
  sig := substDomainArgs(thisDomain,sig)
  lookupDomain :=
     domainSlot = 0 => thisDomain
     thisDomain.domainSlot -- where we look for the operation
  if cons? lookupDomain then lookupDomain := NRTevalDomain lookupDomain
  dollar :=                             -- what matches $ in signatures
    explicitLookupDomainIfTrue => lookupDomain
    thisDomain
  if cons? dollar then dollar := NRTevalDomain dollar
  fn:= basicLookup(op,sig,lookupDomain,dollar)
  fn = nil => keyedSystemError("S2NR0001",[op,sig,lookupDomain.0])
  val:= apply(first fn,[:arglist,rest fn])
  setShellEntry(thisDomain,index,fn)
  val

NRTreplaceLocalTypes(t,dom) ==
   atom t =>
     not integer? t => t
     t:= dom.t
     if cons? t then t:= NRTevalDomain t
     t.0
   first t in '(Mapping Union Record _:) =>
      [first t,:[NRTreplaceLocalTypes(x,dom) for x in rest t]]
   t

substDomainArgs(domain,object) ==
    form := devaluate domain
    SUBLISLIS([form,:rest form],["$$",:$FormalMapVariableList],object)

--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================
domainTableLookup(op,sig,dollar,env) == lookupInTable(op,sig,dollar,env)
lookupInTable(op,sig,dollar,[domain,table]) ==
  table = "derived" => lookupInAddChain(op,sig,domain,dollar)
  success := false
  someMatch := false
  while not success for [sig1,:code] in LASSQ(op,table) repeat
    success :=
      null compareSig(sig,sig1,dollar.0,domain) => false
      code is ['subsumed,a] =>
            subsumptionSig :=
               EQSUBSTLIST(rest(domain.0),$FormalMapVariableList,a)
            someMatch:=true
            false
      predIndex := QSQUOTIENT(code,8192)
      predIndex ~= 0 and null lookupPred($predVector.predIndex,dollar,domain)
        => false
      loc := QSQUOTIENT(QSREMAINDER(code,8192),2)
      loc = 0 =>
        someMatch := true
        nil
      slot := domain.loc
      slot is ["goGet",:.] =>
        lookupDisplay(op,sig,domain,'" !! goGet found, will ignore")
        lookupInAddChain(op,sig,domain,dollar) or 'failed
      null slot =>
        lookupDisplay(op,sig,domain,'" !! null slot entry, continuing")
        lookupInAddChain(op,sig,domain,dollar) or 'failed
      lookupDisplay(op,sig,domain,'" !! found in NEW table!!")
      slot
  NE(success,'failed) and success => success
  subsumptionSig and (u:= SPADCALL(op,subsumptionSig,dollar,domain.1)) => u
  someMatch => lookupInAddChain(op,sig,domain,dollar)
  nil

--=======================================================
--       Lookup Addlist (from lookupInDomainTable or lookupInDomain)
--=======================================================
lookupInAddChain(op,sig,addFormDomain,dollar) ==
  addFunction:=lookupInDomain(op,sig,addFormDomain,dollar,5)
  defaultingFunction addFunction =>
     lookupInCategories(op,sig,addFormDomain,dollar) or addFunction
  addFunction or lookupInCategories(op,sig,addFormDomain,dollar)


defaultingFunction op ==
  not(op is [.,:dom]) => false
  not VECP dom => false
  not (#dom > 0) => false
  not (dom.0 is [packageName,:.]) => false
  not IDENTP packageName => false
  pname := PNAME packageName
  pname.(MAXINDEX pname) = char "&"

--=======================================================
--   Lookup In Domain (from lookupInAddChain)
--=======================================================
lookupInDomain(op,sig,addFormDomain,dollar,index) ==
  addFormCell := addFormDomain.index =>
    integer? KAR addFormCell =>
      or/[lookupInDomain(op,sig,addFormDomain,dollar,i) for i in addFormCell]
    if not VECP addFormCell then addFormCell := eval addFormCell
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
--       Category Default Lookup (from goGet or lookupInAddChain)
--=======================================================
lookupInCategories(op,sig,dom,dollar) ==
  catformList := dom.4.0
  varList := ["$",:$FormalMapVariableList]
  nsig := MSUBST(dom.0,dollar.0,sig)
  -- the following lines don't need to check for predicates because
  -- this code (the old runtime scheme) is used only for
  -- builtin constructors -- their predicates are always true.
  r := or/[lookupInDomainVector(op,nsig,
              eval EQSUBSTLIST(valueList,varList,catform),dollar)
        for catform in catformList | not null catform] where
           valueList() ==
              [MKQ dom,:[MKQ dom.(5+i) for i in 1..(#rest catform)]]
  r or lookupDisplay(op,sig,'"category defaults",'"-- not found")

--=======================================================
--                       Predicates
--=======================================================
lookupPred(pred,dollar,domain) ==
  pred = true => true
  pred is ['AND,:pl] or pred is ['and,:pl] =>
    and/[lookupPred(p,dollar,domain) for p in pl]
  pred is ['OR,:pl] or pred is ['or,:pl] =>
    or/[lookupPred(p,dollar,domain) for p in pl]
  pred is ['NOT,p] or pred is ['not,p] => not lookupPred(p,dollar,domain)
  pred is ['is,dom1,dom2] => domainEqual(dom1,dom2)
  pred is ["has",a,b] =>
    VECP a =>
      keyedSystemError("S2GE0016",['"lookupPred",
        '"vector as  first argument to has"])
    a := eval mkEvalable substDollarArgs(dollar,domain,a)
    b := substDollarArgs(dollar,domain,b)
    HasCategory(a,b)
  keyedSystemError("S2NR0002",[pred])

substDollarArgs(dollar,domain,object) ==
    form := devaluate domain
    SUBLISLIS([devaluate dollar,:rest form],
                ["$",:$FormalMapVariableList],object)

compareSig(sig,tableSig,dollar,domain) ==
  not (#sig = #tableSig) => false
  null (target := first sig)
   or lazyCompareSigEqual(target,first tableSig,dollar,domain) =>
     and/[lazyCompareSigEqual(s,t,dollar,domain)
              for s in rest sig for t in rest tableSig]

lazyCompareSigEqual(s,tslot,dollar,domain) ==
  tslot = '$ => s = "$" or s = devaluate dollar
  integer? tslot and cons?(lazyt:=domain.tslot) and cons? s =>
      lazyt is [.,.,.,[.,item,.]] and
        item is [.,[functorName,:.]] and functorName = first s =>
          compareSigEqual(s,(NRTevalDomain lazyt).0,dollar,domain)
      nil
  compareSigEqual(s,NRTreplaceLocalTypes(tslot,domain),dollar,domain)


compareSigEqual(s,t,dollar,domain) ==
  EQUAL(s,t) => true
  atom t =>
    u :=
      t='$ => dollar
      isSharpVar t =>
        VECP domain => rest(domain.0).(POSN1(t,$FormalMapVariableList))
        rest(domain).(POSN1(t,$FormalMapVariableList))
      string? t and IDENTP s => (s := PNAME s; t)
      nil
    s = '$ => compareSigEqual(dollar,u,dollar,domain)
    u => compareSigEqual(s,u,dollar,domain)
    EQUAL(s,u)
  s='$ => compareSigEqual(dollar,t,dollar,domain)
  atom s => nil
  #s ~= #t => nil
  match := true
  for u in s for v in t repeat
    not compareSigEqual(u,v,dollar,domain) => return(match:=false)
  match

-----------------------Compiler for Interpreter---------------------------------
NRTcompileEvalForm(opName,sigTail,dcVector) ==
  u := NRTcompiledLookup(opName,sigTail,dcVector)
  not ($insideCompileBodyIfTrue = true) => MKQ u
  k := NRTgetMinivectorIndex(u,opName,sigTail,dcVector)
  ['ELT,"$$$",k]  --$$$ denotes minivector

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
NRTcompiledLookup(op,sig,dom) ==
  if CONTAINED('_#,sig) then
      sig := [NRTtypeHack t for t in sig]
  compiledLookupCheck(op,sig,dom)

NRTtypeHack t ==
  atom t => t
  first t = '_# => # second t
  [first t,:[NRTtypeHack tt for tt in rest t]]

NRTgetMinivectorIndex(u,op,sig,domVector) ==
  s := # $minivector
  k := or/[k for k in 0..(s-1)
        for x in $minivector | EQ(x,u)] => k
  $minivector := [:$minivector,u]
  s
