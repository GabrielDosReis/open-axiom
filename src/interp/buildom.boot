-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2015, Gabriel Dos Reis.
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


-- This file contains the constructors for the domains that cannot
-- be written in ScratchpadII yet.  They are not cached because they
-- are very cheap to instantiate.
-- SMW and SCM July 86
-- These have been substantially modified to work with the new
-- runtime system. 
-- GDR, March 2008.

import sys_-macros
import c_-util
import nruncomp
namespace BOOT

$noCategoryDomains == '(Mode SubDomain)
$nonLisplibDomains == append($DomainNames,$noCategoryDomains)

++ Category ancestors for Record, Union, Mapping, and Enumeration domains.
$commonCategoryAncestors ==
  ['(SetCategory), '(BasicType), '(CoercibleTo (OutputForm))]

++ Default category packages for Record, Union, Mapping and 
++ Enumeration domains.
$commonCategoryDefaults ==
  ['(SetCategory& $), '(BasicType& $), nil]

++ The slot number in a domain shell that holds the first parameter to
++ a domain constructor.
$FirstParamSlot == 
  6

--% Monitoring functions

lookupDisplay(op,sig,vectorOrForm,suffix) ==
  not $NRTmonitorIfTrue => nil
  prefix := (suffix is '"" => ">"; "<")
  sayBrightly
    concat(prefix,formatOpSignature(op,sig),
        '" from ", prefix2String devaluateDeeply vectorOrForm,suffix)

isInstantiated [op,:argl] ==
  u:= lassocShiftWithFunction(argl,tableValue($ConstructorCache,op),
        function domainEqualList)
    => CDRwithIncrement u
  nil

--=======================================================
--                       Predicates
--=======================================================
lookupPred(pred,dollar,domain) ==
  pred = true => true
  pred is [op,:pl] and op in '(AND and %and) =>
    and/[lookupPred(p,dollar,domain) for p in pl]
  pred is [op,:pl] and op in '(OR or %or) =>
    or/[lookupPred(p,dollar,domain) for p in pl]
  pred is [op,p] and op in '(NOT not %not) => not lookupPred(p,dollar,domain)
  pred is ['is,dom1,dom2] => domainEqual(dom1,dom2)
  pred is ["has",a,b] =>
    vector? a =>
      keyedSystemError("S2GE0016",['"lookupPred",
        '"vector as  first argument to has"])
    a := eval mkEvalable substDollarArgs(dollar,domain,a)
    b := substDollarArgs(dollar,domain,b)
    HasCategory(a,b)
  keyedSystemError("S2NR0002",[pred])

substDollarArgs(dollar,domain,object) ==
  form := devaluate domain
  applySubst(pairList(["$",:$FormalMapVariableList],[devaluate dollar,:rest form]),
                object)

compareSig(sig,tableSig,dollar,domain) ==
  #sig ~= #tableSig => false
  null(target := first sig)
   or lazyCompareSigEqual(target,first tableSig,dollar,domain) =>
     and/[lazyCompareSigEqual(s,t,dollar,domain)
              for s in rest sig for t in rest tableSig]

lazyCompareSigEqual(s,tslot,dollar,domain) ==
  tslot is '$ => s is "$" or s = devaluate dollar
  integer? tslot and cons?(lazyt := domainRef(domain,tslot)) and cons? s =>
    lazyt is [.,.,.,[.,item,.]] and
      item is [.,[functorName,:.]] and functorName = s.op =>
        compareSigEqual(s,canonicalForm evalDomain lazyt,dollar,domain)
    nil
  compareSigEqual(s,replaceLocalTypes(tslot,domain),dollar,domain)


compareSigEqual(s,t,dollar,domain) ==
  s = t => true
  t isnt [.,:.] =>
    u :=
      t is '$ => dollar
      isSharpVar t =>
        vector? domain =>
          instantiationArgs(domain).(symbolPosition(t,$FormalMapVariableList))
        domain.args.(symbolPosition(t,$FormalMapVariableList))
      string? t and ident? s => (s := symbolName s; t)
      nil
    s is '$ => compareSigEqual(dollar,u,dollar,domain)
    u => compareSigEqual(s,u,dollar,domain)
    s = u
  s is '$ => compareSigEqual(dollar,t,dollar,domain)
  s isnt [.,:.] => nil
  #s ~= #t => nil
  match := true
  for u in s for v in t repeat
    not compareSigEqual(u,v,dollar,domain) => return(match:=false)
  match

--=======================================================
--             Lookup From Interpreter
--=======================================================

compiledLookup(op,sig,dollar) ==
--called by coerceByFunction, evalForm, findEqualFun, findUniqueOpInDomain,
--  getFunctionFromDomain, optDeltaEntry, retractByFunction
  if not vector? dollar then dollar := evalDomain dollar
  -- "^" is an alternate name for "**" in OpenAxiom libraries.
  -- ??? When, we get to support Aldor libraries and the equivalence
  -- ??? does not hold, we may want to do the reverse lookup too.
  -- ??? See compiledLookupCheck below.
  if op = "^" then op := "**"
  basicLookup(op,sig,dollar,dollar)

lookupInDomainVector(op,sig,domain,dollar) ==
  SPADCALL(op,sig,dollar,domainRef(domain,1))

lookupInDomain(op,sig,addFormDomain,dollar,index) ==
  addFormCell := vectorRef(addFormDomain,index) =>
    integer? KAR addFormCell =>
      or/[lookupInDomain(op,sig,addFormDomain,dollar,i) for i in addFormCell]
    if not vector? addFormCell then
      addFormCell := eval addFormCell
    lookupInDomainVector(op,sig,addFormCell,dollar)
  nil

++ same as lookupInDomainVector except that the use of defaults
++ (either in category packages or add-chains) is controlled
++ by `useDefaults'.
lookupInDomainAndDefaults(op,sig,domain,dollar,useDefaults) ==
  $lookupDefaults: local := useDefaults
  lookupInDomainVector(op,sig,domain,dollar)


basicLookup(op,sig,domain,dollar) ==
  item := domainDirectory domain
  item is ['lookupInTable,:.] => lookupInDomainVector(op,sig,domain,dollar)
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
    keyedSystemError("S2NR0001",[op,formatSignature sig,canonicalForm dollar])
  fn

--=======================================================
--                 Lookup From Compiled Code
--=======================================================
goGet(:l) ==
  [:arglist,env] := l
  arglist is ['goGet,:.] => stop()
  [[.,[op,initSig,:code]],thisDomain] := env
  domainSlot := code quo 8192
  code1 := code rem 8192
  isConstant := odd? code1
  code2 := code1 quo 2
  explicitLookupDomainIfTrue := odd? code2
  index := code2 quo 2
  kind := (isConstant => 'CONST; 'ELT)
  sig := [replaceLocalTypes(s,thisDomain) for s in initSig]
  sig := substDomainArgs(thisDomain,sig)
  lookupDomain :=
     domainSlot = 0 => thisDomain
     domainRef(thisDomain,domainSlot) -- where we look for the operation
  if cons? lookupDomain then lookupDomain := evalDomain lookupDomain
  dollar :=                             -- what matches $ in signatures
    explicitLookupDomainIfTrue => lookupDomain
    thisDomain
  if cons? dollar then dollar := evalDomain dollar
  fn := basicLookup(op,sig,lookupDomain,dollar)
  fn = nil => keyedSystemError("S2NR0001",[op,sig,canonicalForm lookupDomain])
  val := apply(first fn,[:arglist,rest fn])
  domainRef(thisDomain,index) := fn
  val

replaceLocalTypes(t,dom) ==
  t isnt [.,:.] =>
    not integer? t => t
    t := domainRef(dom,t)
    if cons? t then t := evalDomain t
    canonicalForm t
  t.op is ":" or builtinConstructor? t.op =>
     [t.op,:[replaceLocalTypes(x,dom) for x in t.args]]
  t

substDomainArgs(domain,object) ==
  form := devaluate domain
  applySubst(pairList(["$$",:$FormalMapVariableList],[form,:form.args]),object)

--=======================================================
--       Category Default Lookup (from goGet or lookupInAddChain)
--=======================================================
lookupInCategories(op,sig,dom,dollar) ==
  catformList := domainRef(dom,4).0
  varList := ["$",:$FormalMapVariableList]
  nsig := MSUBST(canonicalForm dom,canonicalForm dollar,sig)
  -- the following lines don't need to check for predicates because
  -- this code (the old runtime scheme) is used only for
  -- builtin constructors -- their predicates are always true.
  r := or/[lookupInDomainVector(op,nsig,
              eval applySubst(pairList(varList,valueList),catform),dollar)
        for catform in catformList | catform ~= nil ] where
           valueList() ==
              [MKQ dom,:[MKQ domainRef(dom,$AddChainIndex+i) for i in 1..(#rest catform)]]
  r or lookupDisplay(op,sig,'"category defaults",'"-- not found")

--=======================================================
--       Lookup Addlist (from lookupInDomainTable or lookupInDomain)
--=======================================================
defaultingFunction op ==
  op isnt [.,:dom] => false
  not vector? dom => false
  not (#dom > 0) => false
  canonicalForm dom isnt [packageName,:.] => false
  not ident? packageName => false
  isDefaultPackageName packageName

lookupInAddChain(op,sig,addFormDomain,dollar) ==
  addFunction := lookupInDomain(op,sig,addFormDomain,dollar,5)
  defaultingFunction addFunction =>
     lookupInCategories(op,sig,addFormDomain,dollar) or addFunction
  addFunction or lookupInCategories(op,sig,addFormDomain,dollar)

--=======================================================
--       Lookup Function in Slot 1 (via SPADCALL)
--=======================================================
lookupInTable(op,sig,dollar,[domain,table]) ==
  table is "derived" => lookupInAddChain(op,sig,domain,dollar)
  success := nil             -- lookup result
  someMatch := false
  while not success for [sig1,:code] in symbolTarget(op,table) repeat
    success :=
      not compareSig(sig,sig1,canonicalForm dollar,domain) => false
      code is ['Subsumed,a] =>
        subsumptionSig :=
          applySubst(pairList($FormalMapVariableList,canonicalForm(domain).args),a)
        someMatch := true
        nil
      predIndex := code quo 8192
      predIndex ~= 0 and not lookupPred($predVector.predIndex,dollar,domain)
        => nil
      loc := (code rem 8192) quo 2
      loc = 0 =>
        someMatch := true
        nil
      slot := domainRef(domain,loc)
      slot is ["goGet",:.] =>
        lookupDisplay(op,sig,domain,'" !! goGet found, will ignore")
        lookupInAddChain(op,sig,domain,dollar) or 'failed
      slot = nil =>
        lookupDisplay(op,sig,domain,'" !! null slot entry, continuing")
        lookupInAddChain(op,sig,domain,dollar) or 'failed
      lookupDisplay(op,sig,domain,'" !! found in NEW table!!")
      slot
  success isnt 'failed and success ~= nil => success
  subsumptionSig ~= nil and
    (u := SPADCALL(op,subsumptionSig,dollar,domainRef(domain,1))) => u
  someMatch => lookupInAddChain(op,sig,domain,dollar)
  nil

knownEqualPred dom ==
  fun := compiledLookup("=",[$Boolean,"$","$"],dom) =>
    getFunctionReplacement BPINAME first fun
  nil

hashable dom ==
  -- FIXME: there should test for OIL opcodes.
  symbolMember?(knownEqualPred dom,'(EQ EQL EQUAL)) 

--% Record
--  Want to eventually have the elts and setelts.
--  Record is a macro in BUILDOM LISP. It takes out the colons.

isRecord type == 
  type is ["Record",:.]

++ returns the code for the `n'th item recorded in a domain shell,
++ according to the old runtime system.  Note that the old runtime
++ scheme is used only for the handful of constructors created 
++ in this file.
oldSlotCode: %Short -> %Short
oldSlotCode n ==
  2 * ($FirstParamSlot + n)

++ Same as `oldSlotCode', except that it is used for constants.
macro oldConstantSlodCode n ==
  oldSlotCode n + 1

Record(:args) ==
  srcArgs := [[":", second a, devaluate third a] for a in args]
  nargs := #args
  dom := newShell(nargs + 10)
  -- JHD added an extra slot to cache EQUAL methods
  canonicalForm(dom) := ["Record", :srcArgs]
  domainDirectory(dom) :=
    ["lookupInTable",dom,
	[["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	  ["~=",[[$Boolean,"$","$"],:0]], 
            ["hash",[[$SingleInteger,"$"],:0]],
               ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs + 1)]]]]
  domainRef(dom,3) := ["RecordCategory",:instantiationArgs dom]
  domainRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  for i in $FirstParamSlot.. for a in args repeat
    domainRef(dom,i) := third a
  domainRef(dom,$FirstParamSlot + nargs) := [function RecordEqual, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 1) := [function RecordPrint, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 2) := [function Undef, :dom]
  -- following is cache for equality functions
  domainRef(dom,$FirstParamSlot + nargs + 3) := if nargs <= 2
	    then [nil,:nil]
	    else newShell nargs
  dom

RecordEqual(x,y,dom) ==
  nargs := #instantiationArgs dom
  cons? x =>
    b :=
       SPADCALL(first x, first y, first(domainRef(dom,nargs + 9)) or
         first(domainRef(dom,nargs + 9).first :=
                 findEqualFun domainRef(dom,$FirstParamSlot)))
    nargs = 1 => b
    b and
       SPADCALL(rest x, rest y, rest (dom.(nargs + 9)) or
         rest (dom.(nargs + 9).rest := findEqualFun(dom.($FirstParamSlot+1))))
  vector? x =>
    equalfuns := domainRef(dom,nargs + 9)
    and/[SPADCALL(x.i,y.i,equalfuns.i or _
           (equalfuns.i := findEqualFun domainRef(dom,$FirstParamSlot + i)))_
              for i in 0..(nargs - 1)]
  error '"Bug: Silly record representation"

RecordPrint(x,dom) == 
  coerceRe2E(x,dom.3)

coerceVal2E(x,m) ==
   objValUnwrap coerceByFunction(objNewWrap(x,m),$OutputForm)

findEqualFun(dom) ==
  compiledLookup("=",[$Boolean,"$","$"],dom)

coerceRe2E(x,source) ==
  n := # rest source
  n = 1 =>
    ["construct",
     ["=", source.1.1, coerceVal2E(first x,source.1.2)] ]
  n = 2 =>
    ["construct",
     ["=", source.1.1, coerceVal2E(first x,source.1.2)], _
     ["=", source.2.1, coerceVal2E(rest x,source.2.2)] ]
  vector? x =>
    ['construct,
     :[["=",tag,coerceVal2E(x.i, fdom)]
       for i in 0.. for [.,tag,fdom] in rest source]]
  error '"Bug: ridiculous record representation"


--% Union
--  Want to eventually have the coerce to and from branch types.

Union(:args) ==
  srcArgs := [(a is [":",tag,d] => [":",tag,devaluate d]; devaluate a)
                 for a in args]
  nargs := #args
  dom := newShell (nargs + 9)
  canonicalForm(dom) := ["Union", :srcArgs]
  domainDirectory(dom) :=
    ["lookupInTable",dom,
       [["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	 ["~=",[[$Boolean,"$","$"],:0]],
           ["hash", [[$SingleInteger,"$"],:0]],
              ["coerce",[[$OutputForm,"$"],:oldSlotCode (nargs+1)]]]]
  domainRef(dom,3) := ["UnionCategory",:instantiationArgs dom]
  domainRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  for i in $FirstParamSlot.. for a in args repeat
    domainRef(dom,i) := a
  domainRef(dom,$FirstParamSlot + nargs) := [function UnionEqual, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 1) := [function UnionPrint, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 2) := [function Undef, :dom]
  dom

UnionEqual(x, y, dom) ==
  ["Union",:branches] := canonicalForm dom
  predlist := mkPredList branches
  same := false
  for b in stripTags branches for p in predlist while not same repeat
    typeFun := eval ['%lambda,'(_#1),p]
    apply(typeFun,[x]) and apply(typeFun,[y]) =>
      string? b => same := (x = y)
      if p is ['%ieq,['%head,.],:.] then (x := rest x; y := rest y)
      same := SPADCALL(x, y, findEqualFun(evalDomain b))
  same

UnionPrint(x, dom) ==
  coerceUn2E(x, canonicalForm dom)

coerceUn2E(x,source) ==
  ["Union",:branches] := source
  predlist := mkPredList branches
  byGeorge := byJane := gensym()
  for b in stripTags branches for p in predlist  repeat
    typeFun := eval ['%lambda,'(_#1),p]
    if apply(typeFun,[x]) then return
      if p is ['%ieq,['%head,.],:.] then x := rest x
--    string? b => return x  -- to catch "failed" etc.
      string? b => byGeorge := x  -- to catch "failed" etc.
      byGeorge := coerceVal2E(x,b)
  byGeorge = byJane =>
    error '"Union bug: Cannot find appropriate branch for coerce to E"
  byGeorge

--% Mapping
--  Want to eventually have elt: ($, args) -> target

++ Implementation of the `MappinCategory' as builtin.
++ A domain that satisfy this predicate provides implementation
++ to abstraction that map values from some type to values
++ of another type.
MappingCategory(:sig) ==       
  sig = nil =>
    error '"MappingCategory requires at least one argument"
  cat := eval ['Join,$Type,
                ['mkCategory,quote 'domain,
                   quote [[['elt,[first sig,'$,:rest sig]],true]],
                     [], [], nil]]
  canonicalForm(cat) := ['MappingCategory,:sig]
  cat

Mapping(:args) ==
  srcArgs := [devaluate a for a in args]
  nargs := #args
  dom := newShell(nargs + 9)
  canonicalForm(dom) := ["Mapping", :srcArgs]
  domainDirectory(dom) :=
    ["lookupInTable",dom,
       [["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	 ["~=",[[$Boolean,"$","$"],:0]],
           ["hash", [[$SingleInteger,"$"],:0]],
              ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs + 1)]]]]
  domainRef(dom,3) := $SetCategory
  domainRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  for i in $FirstParamSlot.. for a in args repeat
    domainRef(dom,i) := a
  domainRef(dom,$FirstParamSlot + nargs) := [function MappingEqual, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 1) := [function MappingPrint, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 2) := [function Undef, :dom]
  dom

MappingEqual(x, y, dom) == sameObject?(x,y)
MappingPrint(x, dom) == coerceMap2E(x)

coerceMap2E(x) ==
  -- nrlib domain
  array? rest x => ["theMap", BPINAME first x,
    if $testingSystem then 0 else HASHEQ(rest x) rem 1000]
  -- aldor 
  ["theMap", BPINAME first x  ]

--% Enumeration

EnumerationCategory(:"args") ==
  cat := eval ['Join,$SetCategory,
                ['mkCategory,quote 'domain,
                   quote [[[arg,['$],'constant],'T] for arg in args],
                     [], [], nil]]
  canonicalForm(cat) := ['EnumerationCategory,:args]
  cat

Enumeration(:"args") ==
  nargs := #args
  dom := newShell(2 * nargs + 9)
  -- JHD added an extra slot to cache EQUAL methods
  canonicalForm(dom) := ["Enumeration",:args]
  domainDirectory(dom) :=
    ["lookupInTable",dom,
	[["=",[[$Boolean,"$","$"],:oldSlotCode nargs]],
	  ["~=",[[$Boolean,"$","$"],:0]],
            ["hash", [[$SingleInteger,"$"],:0]],
              ["coerce",[[$OutputForm,"$"],:oldSlotCode(nargs+1)], 
                [["$", $Symbol], :oldSlotCode(nargs+2)]],
                  :[[arg,[["$"],:oldConstantSlodCode(nargs+3+i)]]
                      for arg in args for i in 0..]
                          ]]
  domainRef(dom,3) := ["EnumerationCategory",:instantiationArgs dom]
  domainRef(dom,4) := [$commonCategoryDefaults, $commonCategoryAncestors]
  for i in $FirstParamSlot.. for a in args repeat
    domainRef(dom,i) := a
  domainRef(dom,$FirstParamSlot + nargs) := [function EnumEqual, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 1) := [function EnumPrint, :dom]
  domainRef(dom,$FirstParamSlot + nargs + 2) := [function createEnum, :dom]
  -- Fille slots for constant returning functions.
  -- Note: this is wasteful in terms of space since the constants are
  --       already stored as arguments to this domain.
  for i in ($FirstParamSlot + nargs + 3).. for . in args for v in 0.. repeat
    domainRef(dom,i) := [function IDENTITY,:v]
  dom

EnumEqual(e1,e2,dom) == 
  scalarEq?(e1,e2)

EnumPrint(enum, dom) == 
  instantiationArgs(dom).enum

createEnum(sym, dom) ==
  args := instantiationArgs dom
  val := -1
  for v in args for i in 0.. repeat
     symbolEq?(sym,v) => return(val:=i)
  val < 0 => userError ['"Cannot coerce",sym,'"to",["Enumeration",:args]]
  val

--% INSTANTIATORS

RecordCategory(:"x") == constructorCategory ["Record",:x]

UnionCategory(:"x") == constructorCategory ["Union",:x]

constructorCategory (title is [op,:.]) ==
  constructorFunction:= property(op,"makeFunctionList") or
              systemErrorHere ['"constructorCategory",title]
  [funlist,.]:= apply(constructorFunction,["$",title,$CategoryFrame])
  oplist:= [[[a,b],true,c] for [a,b,c] in funlist]
  cat:=
    JoinInner([eval $SetCategory,mkCategory("domain",oplist,nil,nil,nil)],
      $EmptyEnvironment)
  canonicalForm(cat) := title
  cat

mkMappingFunList(nam,mapForm,e) ==
  nargs := #rest mapForm
  dc := gensym()
  sigFunAlist:=
    [["=",[$Boolean,nam ,nam], ["ELT",dc,$FirstParamSlot + nargs]], 
      ["~=",[$Boolean,nam,nam],["ELT",dc,0]],
        ["hash",[$SingleInteger,nam],["ELT",dc,0]],
          ["coerce",[$OutputForm,nam], 
            ["ELT",dc,$FirstParamSlot + nargs + 1]]]
  [substitute(nam,dc,substituteDollarIfRepHack sigFunAlist),e]


++ Build an inline function for constructing records of length `n'.
mkRecordFun n ==
  args := take(n,$FormalMapVariableList)
  op := 
    n < 2 => '%list
    n = 2 => '%pair
    '%vector
  ["XLAM",args,[op,:args]]

++ Build expression for selecting the i-th field of a fomal record
++ variable of length `n'.
formalRecordField(n,i) ==
  n < 2 => ['%head,"#1"]
  n = 2 =>
    i = 0 => ['%head,"#1"]
    ['%tail,"#1"]
  ['%vref,"#1",i]

++ Build an inline function for selecting field `i' or a
++ record of length `n'.  
eltRecordFun(n,i) ==
  ["XLAM",["#1","#2"],formalRecordField(n,i)]

seteltRecordFun(n,i) ==
  args := take(3,$FormalMapVariableList)
  field := formalRecordField(n,i)
  body := 
    n > 2 => ['%store,field,"#3"]
    ['%seq,['%store,field,"#3"],field]
  ["XLAM",args,body]

copyRecordFun n ==
  body := 
    n < 2 => ['%list,['%head,"#1"]]
    n = 2 => ['%pair,['%head,"#1"],['%tail,"#1"]]
    ['%vcopy,"#1"]
  ["XLAM",["#1"],body]

mkRecordFunList(nam,["Record",:Alist],e) ==
  len:= #Alist
  dc := gensym()
  sigFunAlist:=
    [["construct",[nam,:[A for [.,a,A] in Alist]],mkRecordFun len],
      ["=",[$Boolean,nam ,nam],["ELT",dc,$FirstParamSlot + len]],
        ["~=",[$Boolean,nam,nam],["ELT",dc,0]],
         ["hash",[$SingleInteger,nam],["ELT",dc,0]],
	  ["coerce",[$OutputForm,nam],["ELT",dc,$FirstParamSlot+len+1]],:
	   [["elt",[A,nam,PNAME a],eltRecordFun(len,i)]
	       for i in 0.. for [.,a,A] in Alist],:
	     [["setelt",[A,nam,PNAME a,A],seteltRecordFun(len,i)]
		 for i in 0.. for [.,a,A] in Alist],
		   ["copy",[nam,nam],copyRecordFun len]]
  [substitute(nam,dc,substituteDollarIfRepHack sigFunAlist),e]

mkNewUnionFunList(name,form is ["Union",:listOfEntries],e) ==
  nargs := #listOfEntries
  dc := name
  m := dollarIfRepHack name
  --2. create coercions from subtypes to subUnion
  cList:=
    [["=",[$Boolean,name,name],["ELT",dc,$FirstParamSlot+nargs]],
       ["~=",[$Boolean,name,name],["ELT",dc,0]],
        ["hash",[$SingleInteger,name],["ELT",dc,0]],
	 ["coerce",[$OutputForm,name],["ELT",dc,$FirstParamSlot+nargs+1]],:
	   ("append"/
	    [[["construct",[name,type],["XLAM",["#1"],["%pair",i,"#1"]]],
	      ["elt",[type,name,tag],cdownFun],
		["case",[$Boolean,name,tag],
		   ["XLAM",["#1","#2"],['%ieq,['%head,"#1"],i]]]]
		     for [.,tag,type] in listOfEntries for i in 0..])] where
		       cdownFun() ==
                         ['XLAM,["#1","#2"],['%pullback,"#1",type,i]]
  [cList,e]

mkEnumerationFunList(dc,["Enumeration",:SL],e) ==
  len := #SL
  cList :=
    [nil,
      ["=",[$Boolean,dc,dc],["XLAM",["#1","#2"],['%ieq,"#1","#2"]]],
        ["~=",[$Boolean,dc,dc],["XLAM",["#1","#2"],['%not,['%ieq,"#1","#2"]]]],
          ["coerce",[dc,$Symbol],["ELT",dc,$FirstParamSlot+len+1]],
            ["coerce",[$OutputForm,dc],["ELT",dc,$FirstParamSlot+len+2]],
              :[[arg,[dc],["XLAM",[],v]] for arg in SL for v in 0..]
                 ]
  [cList,e]

mkUnionFunList(op,form is ["Union",:listOfEntries],e) ==
  first listOfEntries is [":",.,.] => mkNewUnionFunList(op,form,e)
  nargs := #listOfEntries
  -- create coercions from subtypes to subUnion
  g := gensym()
  cList:=
   [["=",[$Boolean,g ,g],["ELT",op,$FirstParamSlot + nargs]],
     ["~=",[$Boolean,g,g],["ELT",op,0]],
      ["hash",[$SingleInteger,g],["ELT",op,0]],
       ["coerce",[$OutputForm,g],["ELT",op,$FirstParamSlot+nargs+1]],:
	("append"/
	 [[["autoCoerce",[g,t],upFun],
	   ["coerce",[t,g],cdownFun],
	   ["autoCoerce",[t,g],downFun], --this should be removed eventually
	   ["case",[$Boolean,g,t],typeFun]]
	      for t in listOfEntries for n in 0..])] where
		upFun() == ["XLAM",["#1"],["%pair",n,"#1"]]
		cdownFun() == ['XLAM,["#1"],['%pullback,"#1",t,n]]
		downFun() == ["XLAM",["#1"],["%tail","#1"]]
		typeFun() == ["XLAM",["#1","#2"],['%ieq,['%head,"#1"],n]]
  cList:= substitute(dollarIfRepHack op,g,cList)
  [cList,e]

--%

parentsOfBuiltinInstance form ==
  [op,:args] := form
  -- builtin categories
  op in '(RecordCategory UnionCategory) =>
    [[$SetCategory,:['AND,:[['has,t,$SetCategory] for t in stripTags args]]]]
  op is 'MappingCategory => nil -- [[$Type,:true]]
  op is 'EnumerationCategory => [[$SetCategory,:true]]
  -- builtin domains
  op is 'Mapping => [[['MappingCategory,:args],:true],[$SetCategory,:true]]
  op is 'Record => [[['RecordCategory,:args],:true]]
  op is 'Union => [[['UnionCategory,:args],:true]]
  op is 'Enumeration => [[['EnumerationCategory,:args],:true]]
  nil

$CapitalLetters ==
  '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)

builtinInstanceForm form ==
  [op,:args] := form
  op in '(Mapping MappingCategory Enumeration EnumerationCategory) =>
    [op,:take(#args,$CapitalLetters)]
  op in '(Record RecordCategory Union UnionCategory) =>
    [op,:[T for a in args for t in $CapitalLetters]] where
      T() ==
        a is [":",x,.] => [":",x,t]
        t
  nil

--%
for x in '((Record mkRecordFunList)
           (Union mkUnionFunList)
           (Mapping mkMappingFunList)
           (Enumeration mkEnumerationFunList)) 
 repeat
   property(first x, 'makeFunctionList) := second x

