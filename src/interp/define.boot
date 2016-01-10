-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2016, Gabriel Dos Reis.
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

import nruncomp
import g_-error
import c_-util
import database

namespace BOOT

module define where
  compDefine: (%Maybe %Database,%Form,%Mode,%Env) -> %Maybe %Triple 
  compSubDomain: (%Form,%Mode,%Env) -> %Maybe %Triple
  compCapsule: (%Form, %Mode, %Env) -> %Maybe %Triple
  compJoin: (%Form,%Mode,%Env) -> %Maybe %Triple 
  compAdd: (%Form, %Mode, %Env) -> %Maybe %Triple 
  compCategory: (%Form,%Mode,%Env) -> %Maybe %Triple
  evalCategoryForm: (%Form,%Env) -> %Maybe %Shell
  getCategoryObjectIfCan: (%Table,%Form,%Env) -> %Maybe %Shell
  getCategoryObject: (%Table,%Form,%Env) -> %Shell


--%

compDefine1: (%Maybe %Database,%Form,%Mode,%Env) -> %Maybe %Triple 

$doNotCompileJustPrint := false

++ stack of pending capsule function definitions.
$capsuleFunctionStack := []

--%

$forceAdd := false

$functionStats := nil
$functorStats := nil

$functorTarget := nil
$condAlist := []
$uncondAlist := []
$NRTslot1PredicateList := []
$NRTattributeAlist := []
$signature := nil
$byteAddress := nil
$sigAlist := []
$predAlist := []
$argumentConditionList := []
$finalEnv := nil
$initCapsuleErrorCount := nil
$CapsuleModemapFrame := nil
$CapsuleDomainsInScope := nil
$signatureOfForm := nil
$addFormLhs := nil

++ True if the current functor definition refines a domain.
$subdomain := false

--%

compDefineAddSignature: (%Form,%Sig,%Env) -> %Env


--% ADDINFORMATION CODE
--% This code adds various items to the special value of $Information,
--% in order to keep track of all the compiler's information about
--% various categories and similar objects
--% An actual piece of (unconditional) information can have one of 3 forms:
--%  (ATTRIBUTE domainname attribute)
--%              --These are only stored here
--%  (SIGNATURE domainname operator signature)
--%              --These are also stored as 'modemap' properties
--%  (has domainname categoryexpression)
--%              --These are also stored as 'value' properties
--% Conditional attributes are of the form
--%  (%when
--%  (condition info info ...)
--%  ... )
--% where the condition looks like a 'has' clause, or the 'and' of several
--% 'has' clauses:
--%   (has name categoryexpression)
--%   (has name (ATTRIBUTE attribute))
--%   (has name (SIGNATURE operator signature))
--% The use of two representations is admitted to be clumsy


liftCond (clause is [ante,conseq]) ==
  conseq is ['%when,:l] =>
    [[lcAnd(ante,a),:b] for [a,:b] in l] where
      lcAnd(pred,conj) ==
        conj is ["and",:ll] => ["and",pred,:ll]
        ["and",pred,conj]
  [clause]
 
formatPred(u,e) ==
  u is ["has",a,b] =>
    b isnt [.,:.] and isCategoryForm([b],e) => ["has",a,[b]]
    b isnt [.,:.] => ["has",a,["ATTRIBUTE",b]]
    isCategoryForm(b,e) => u
    b is ["ATTRIBUTE",.] => u
    b is ["SIGNATURE",:.] => u
    ["has",a,["ATTRIBUTE",b]]
  u isnt [.,:.] => u
  u is ["and",:v] => ["and",:[formatPred(w,e) for w in v]]
  systemError ['"formatPred",u]
 
formatInfo(u,e) ==
  u isnt [.,:.] => u
  u is ["SIGNATURE",:v] => ["SIGNATURE","$",:v]
  u is ["PROGN",:l] => ["PROGN",:[formatInfo(v,e) for v in l]]
  u is ["ATTRIBUTE",v] =>
    -- The parser can't tell between those attributes that really
    -- are attributes, and those that are category names
    v isnt [.,:.] and isCategoryForm([v],e) => ["has","$",[v]]
    v isnt [.,:.] => ["ATTRIBUTE","$",v]
    isCategoryForm(v,e) => ["has","$",v]
    ["ATTRIBUTE","$",v]
  u is ["IF",a,b,c] =>
    c is "%noBranch" =>
      ['%when,:liftCond [formatPred(a,e),formatInfo(b,e)]]
    b is "%noBranch" =>
      ['%when,:liftCond [["not",formatPred(a,e)],formatInfo(c,e)]]
    ['%when,:liftCond [formatPred(a,e),formatInfo(b,e)],:
      liftCond [["not",formatPred(a,e)],formatInfo(c,e)]]
  systemError ['"formatInfo",u]
 
addInformation(m,e) ==
  facts := ref nil               -- list of facts to derive from `m'.
  deduce(m,facts,e) where
    deduce(m,facts,e) ==
      m isnt [.,:.] => nil
      m is ["CATEGORY",.,:stuff] =>
        for u in stuff repeat
          deref(facts) := [formatInfo(u,e),:deref facts]
      m is ["Join",:stuff] =>
        for u in stuff repeat deduce(u,facts,e)
      nil
  put("$Information","special",
       [:deref facts,:get("$Information","special",e)],e)
 
hasToInfo (pred is ["has",a,b]) ==
  b is ["SIGNATURE",:data] => ["SIGNATURE",a,:data]
  b is ["ATTRIBUTE",c] => ["ATTRIBUTE",a,c]
  pred
 
++ Return true if we are certain that the information
++ denotated by `pred' is derivable from the current environment `env'.
++ The third parameter `tbl' serves as a memo-table to help avoid
++ repeated computation of the same piece of information.
++ Note that because this is a compile-time determination, the value
++ computed by this subroutine is by necessary an approximation.
++ If it returns true, when we know for certain that the predicate
++ will hold also at runtime.  However, if it returns false the predicate
++ may or may not hold at runtime.
knownInfo(pred,env,tbl == makeTable function valueEq?) ==
  pred=true => true
  tableValue(tbl,pred) => true    -- re-use previously computed value
  listMember?(pred,get("$Information","special",env)) =>
    tableValue(tbl,pred) := true
  pred is ["OR",:l] => or/[knownInfo(u,env,tbl) for u in l]
  pred is ["AND",:l] => and/[knownInfo(u,env,tbl) for u in l]
  pred is ["or",:l] => or/[knownInfo(u,env,tbl) for u in l]
  pred is ["and",:l] => and/[knownInfo(u,env,tbl) for u in l]
  pred is ["ATTRIBUTE",name,attr] =>
    v := compForMode(name,$EmptyMode,env) or return
          stackAndThrow('"can't find category of %1pb",[name])
    [vv,.,.] := compMakeCategoryObject(v.mode,env) or return
                 stackAndThrow('"can't make category of %1pb",[name])
    listMember?(attr,categoryAttributes vv) =>
      tableValue(tbl,pred) := true
    x := assoc(attr,categoryAttributes vv) =>
      --format is a list of two elements: information, predicate
      tableValue(tbl,pred) := knownInfo(second x,env,tbl)
    false
  pred is ["has",name,cat] =>
    cat is ["ATTRIBUTE",:a] =>
      tableValue(tbl,pred) := knownInfo(["ATTRIBUTE",name,:a],env,tbl)
    cat is ["SIGNATURE",:a] =>
      tableValue(tbl,pred) := knownInfo(["SIGNATURE",name,:a],env,tbl)
    -- unnamed category expressions imply structural checks.
    cat is ["Join",:.] =>
      tableValue(tbl,pred) :=
        and/[knownInfo(["has",name,c],env,tbl) for c in cat.args]
    cat is ["CATEGORY",.,:atts] =>
      tableValue(tbl,pred) :=
        and/[knownInfo(hasToInfo ["has",name,att],env,tbl) for att in atts]
    name is ['Union,:.] => false
    -- we have a named category expression
    v:= compForMode(name,$EmptyMode,env) or return
          stackAndThrow('"can't find category of %1pb",[name])
    vmode := v.mode
    cat = vmode => tableValue(tbl,pred) := true
    vmode is ["Join",:l] and listMember?(cat,l) =>
      tableValue(tbl,pred) := true
    [vv,.,.]:= compMakeCategoryObject(vmode,env) or return
                 stackAndThrow('"cannot find category %1pb",[vmode])
    listMember?(cat,categoryPrincipals vv) =>
      --checks princ. ancestors
      tableValue(tbl,pred) := true
    (u:=assoc(cat,categoryAncestors vv)) and knownInfo(second u,env,tbl) =>
      tableValue(tbl,pred) := true
    -- previous line checks fundamental anscestors, we should check their
    --   principal anscestors but this requires instantiating categories

    or/[ancestor?(cat,[first u],tbl,env) 
         for u in categoryAncestors vv | knownInfo(second u,env,tbl)] =>
            tableValue(tbl,pred) := true
    false
  pred is ["SIGNATURE",name,op,sig,:.] =>
    v:= get(op,"modemap",env)
    for w in v repeat
      ww := w.mmSignature  --the actual signature part
      ww = sig =>
        w.mmCondition  = true => return (tableValue(tbl,pred) := true)
        false
        --error '"knownInfo"
  false
 
mkJoin(cat,mode) ==
  mode is ['Join,:cats] => ['Join,cat,:cats]
  ['Join,cat,mode]
 

getvalue(name,e) ==
  u := get(name,"value",e) => u
  u := comp(name,$EmptyMode,e) => u  --name may be a form
  systemError [name,'" is not bound in the current environment"]
 
actOnInfo(u,$e) ==
  null u => $e
  u is ["PROGN",:l] =>
    for v in l repeat $e := actOnInfo(v,$e)
    $e
  db := currentDB $e
  $e:=
    put("$Information","special",Info:= [u,:get("$Information","special",$e)],$e
      )
  u is ['%when,:l] =>
      --there is nowhere %else that this sort of thing exists
    for [ante,:conseq] in l repeat
      if listMember?(hasToInfo ante,Info) then for v in conseq repeat
        $e := actOnInfo(v,$e)
    $e
  u is ["ATTRIBUTE",name,att] =>
    [vval,vmode,.] := getvalue(name,$e)
    compilerMessage('"augmenting %1: %2p", [name,["ATTRIBUTE",att]])
    key :=
      -- FIXME: there should be a better to tell whether name
      --        designates a domain, as opposed to a package
      CONTAINED("$",vmode) => 'domain
      'package
    cat := ["CATEGORY",key,["ATTRIBUTE",att]]
    $e:= put(name,"value",[vval,mkJoin(cat,vmode),nil],$e)
      --there is nowhere %else that this sort of thing exists
  u is ["SIGNATURE",name,operator,modemap,:q] =>
    kind := 
      q is ["constant"] => "CONST" 
      "ELT"
    implem:=
      (implem:=ASSOC([name,:modemap],get(operator,'modemap,$e))) =>
          CADADR implem
      name = "$" => [kind,name,-1]
      [kind,name,substitute('$,name,modemap)]
    $e:= addModemap(operator,name,modemap,true,implem,$e)
    [vval,vmode,.] := getvalue(name,$e)
    compilerMessage('"augmenting %1: %2p", 
       [name,["SIGNATURE",operator,modemap,:q]])
    key :=
      -- FIXME: there should be a better to tell whether name
      --        designates a domain, as opposed to a package
      CONTAINED("$",vmode) => 'domain
      'package
    cat:= ["CATEGORY",key,["SIGNATURE",operator,modemap,:q]]
    $e:= put(name,"value",[vval,mkJoin(cat,vmode),nil],$e)
  u is ["has",name,cat] =>
    [vval,vmode,.] := getvalue(name,$e)
    cat=vmode => $e --stating the already known
    u:= compMakeCategoryObject(cat,$e) =>
         --we are adding information about a category
      [catvec,.,$e]:= u
      [ocatvec,.,$e]:= compMakeCategoryObject(vmode,$e)
 
      --we are adding a principal descendant of what was already known
      listMember?(cat,categoryPrincipals ocatvec) or
         assoc(cat,categoryAncestors ocatvec) is [.,"T",.] => $e
             --what was being asserted is an ancestor of what was known
      if name="$"
        then $e:= augModemapsFromCategory(db,name,name,cat,$e)
        else
          genDomainView(db,name,cat,"HasCategory")
          -- a domain upgrade at function level is local to that function.
          if not $insideCapsuleFunctionIfTrue and 
            not symbolMember?(name,$functorLocalParameters) then
              $functorLocalParameters:=[:$functorLocalParameters,name]
      compilerMessage('"augmenting %1: %2p", [name,cat])
      $e:= put(name,"value",[vval,mkJoin(cat,vmode),nil],$e)
    SAY("extension of ",vval," to ",cat," ignored")
    $e
  systemError ['"actOnInfo",u]
 
infoToHas a ==
  a is ["SIGNATURE",b,:data] => ["has",b,["SIGNATURE",:data]]
  a is ["ATTRIBUTE",b,c] => ["has",b,["ATTRIBUTE",c]]
  a

chaseInferences(pred,$e) ==
  foo hasToInfo pred where
    foo pred ==
      knownInfo(pred,$e) => nil
      $e := actOnInfo(pred,$e)
      pred:= infoToHas pred
      for u in get("$Information","special",$e) repeat
        u is ['%when,:l] =>
          for [ante,:conseq] in l repeat
            ante=pred => [foo w for w in conseq]
            ante is ["and",:ante'] and listMember?(pred,ante') =>
              ante':= remove(ante',pred)
              v':=
                # ante'=1 => first ante'
                ["and",:ante']
              v':= ['%when,[v',:conseq]]
              listMember?(v',get("$Information","special",$e)) => nil
              $e:=
                put("$Information","special",[v',:
                  get("$Information","special",$e)],$e)
            nil
  $e
 
--%

--=======================================================================
--            Generate Code to Create Infovec
--=======================================================================
++ Called by compDefineFunctor1 to create infovec at compile time
getInfovecCode(db,e) == 
  $byteAddress: local := 0
  ['LIST,
    MKQ makeDomainTemplate db,
      MKQ makeCompactDirect(db,makeSlot1Info db),
        MKQ genFinalAttributeAlist(db,e),
          makeCategoryAlist(db,e),
            MKQ dbLookupFunction db]

--=======================================================================
--         Generation of Domain Vector Template (Compile Time)
--=======================================================================
makeDomainTemplate db ==   
--NOTES: This function is called at compile time to create the template
--  (slot 0 of the infovec); called by getInfovecCode from compDefineFunctor1
  vec := dbTemplate db
  for index in 0..maxIndex vec repeat
    item := domainRef(vec,index)
    item isnt [.,:.] => nil
    domainRef(vec,index) :=
      cons? first item => makeGoGetSlot(db,item,index)
      item   
  dbByteList(db) := "append"/reverse! dbByteList db
  vec
 
makeGoGetSlot(db,item,index) ==
--NOTES: creates byte vec strings for LATCH slots
--these parts of the dbByteList are created first; see also makeCompactDirect
  [sig,whereToGo,op,:flag] := item
  n := #sig - 1
  newcode := [n,whereToGo,:makeCompactSigCode sig,index]
  dbByteList(db) := [newcode,:dbByteList db]
  curAddress := $byteAddress
  $byteAddress := $byteAddress + n + 4
  [curAddress,:op]
 
--=======================================================================
--                Generate OpTable at Compile Time
--=======================================================================
--> called by getInfovecCode (see top of this file) from compDefineFunctor1
makeCompactDirect(db,u) ==
  $predListLength :local := # $NRTslot1PredicateList
  $byteVecAcc: local := nil
  [nam,[addForm,:opList]] := u
  --pp opList 
  d := [:[op,y] for [op,:items] in opList
          | y := makeCompactDirect1(db,op,items)]
  dbByteList(db) := [:dbByteList db,:"append"/reverse! $byteVecAcc]
  dbOperationTable(db) := vector d
 
makeCompactDirect1(db,op,items) ==
--NOTES: creates byte codes for ops implemented by the domain
    curAddress := $byteAddress
    $op: local := op  --temp hack by RDJ 8/90 (see orderBySubsumption)
    newcodes := "append"/[u for y in orderBySubsumption items |
                            u := fn(db,y)] or return nil
    $byteVecAcc := [newcodes,:$byteVecAcc]
    curAddress
 where fn(db,y) ==
  [sig,:r] := y
  r = ['Subsumed] =>
    n := #sig - 1
    $byteAddress := $byteAddress + n + 4
    [n,0,:makeCompactSigCode sig,0]  --always followed by subsuming signature
    --identified by a 0 in slot position
  if r is [n,:s] then
    slot :=
      n is [p,:.] => p  --the rest is linenumber of function definition
      n
    predCode :=
      s is [pred,:.] => predicateBitIndex(db,pred,$e)
      0
  --> drop items which are not present (predCode = -1)
  predCode = -1 => return nil
  --> drop items with nil slots if lookup function is incomplete
  if null slot then
     dbLookupFunction db is 'lookupIncomplete => return nil
     slot := 1   --signals that operation is not present
  n := #sig - 1
  $byteAddress := $byteAddress + n + 4
  res := [n,predCode,:makeCompactSigCode sig,slot]
  res
 
orderBySubsumption items ==
  acc := subacc := nil
  for x in items repeat
    not ($op in '(Zero One)) and x is [.,.,.,'Subsumed] =>
      subacc := [x,:subacc]
    acc := [x,:acc]
  y := z := nil
  for [a,b,:.] in subacc | b repeat   
  --NOTE: b = nil means that the signature a will appear in acc, that this
  --  entry is be ignored (e.g. init: -> $ in ULS)
    while (u := assoc(b,subacc)) repeat b := second u
    u := assoc(b,acc) or systemError nil
    if null second u then u := [first u,1] --mark as missing operation
    y := [[a,'Subsumed],u,:y] --makes subsuming signature follow one subsumed
    z := insert(b,z)  --mark a signature as already present
  [:y,:[w for (w := [c,:.]) in acc | not listMember?(c,z)]] --add those not subsuming
 
makeCompactSigCode sig == [fn for x in sig] where 
  fn() == 
    x is "$$" => 2
    x is "$" => 0
    not integer? x => 
      systemError ['"code vector slot is ",x,'"; must be number"]
    x
  
--=======================================================================
--               Generate Slot 4 Constructor Vectors
--=======================================================================
depthAssocList(u,cache) == 
  u := removeSymbol(u,'DomainSubstitutionMacro)  --hack by RDJ 8/90
  removeDuplicates ("append"/[depthAssoc(y,cache) for y in u])
 
depthAssoc(x,cache) ==
  y := tableValue(cache,x) => y
  x is ['Join,:u] or (u := substSource parentsOfForm x) =>
    v := depthAssocList(u,cache)
    tableValue(cache,x) := [[x,:n],:v]
      where n() == 1 + "MAX"/[rest y for y in v]
  tableValue(cache,x) := [[x,:0]]
 
makeCategoryAlist(db,e) ==
  pcAlist := [:[[x,:true] for x in $uncondAlist],:$condAlist]
  levelAlist := depthAssocList(substSource pcAlist,hashTable 'EQUAL)
  opcAlist := sortBy(function(x +-> LASSOC(first x,levelAlist)),pcAlist)
  newPairlis := [[i,:b] for [.,:b] in dbFormalSubst db for i in $NRTbase..]
  slot1 := [[a,:k] for [a,:b] in dbSubstituteAllQuantified(db,opcAlist)
                   | (k := predicateBitIndex(db,b,e)) ~= -1]
  slot0 := [getCategoryConstructorDefault a.op for [a,:.] in slot1]
  sixEtc := [$AddChainIndex + i for i in 1..dbArity db]
  formals := substTarget dbFormalSubst db
  for x in slot1 repeat
    x.first := applySubst(pairList(['$,:formals],["$$",:sixEtc]),first x)
  -----------code to make a new style slot4 -----------------
  predList := substTarget slot1  --is list of predicate indices
  maxPredList := "MAX"/predList
  catformvec := substSource slot1
  maxElement := "MAX"/dbByteList db
  ['CONS, ['makeByteWordVec2,MAX(maxPredList,1),MKQ predList],
    ['CONS, MKQ vector slot0,
      ['CONS, MKQ vector [encodeCatform(db,x) for x in catformvec],
        ['makeByteWordVec2,maxElement,MKQ dbByteList db]]]]
  --NOTE: this is new form: old form satisfies vector? CDDR form

encodeCatform(db,x) ==
  x is '$ => x
  k := assocIndex(db,x) => k
  x isnt [.,:.] or rest x isnt [.,:.] => x
  [first x,:[encodeCatform(db,y) for y in rest x]]
 
++ Like getmode, except that if the mode is local variable with
++ defined value, we want that value instead.
getXmode(x,e) ==
  m := getmode(x,e) or return nil
  ident? m and get(m,'%macro,e) or m

 
--=======================================================================
--              Compute the lookup function (complete or incomplete)
--=======================================================================
NRTgetLookupFunction(db,addForm,tbl,env) ==
  $why: local := nil
  domform := dbSubstituteFormals(db,dbConstructorForm db)
  cat := dbCategory db
  addForm isnt [.,:.] =>
    ident? addForm and (m := getmode(addForm,env)) ~= nil and
      isCategoryForm(m,env) and
        extendsCategory(db,domform,cat,dbSubstituteFormals(db,m),tbl,env) =>
          'lookupIncomplete
    'lookupComplete
  addForm := dbSubstituteFormals(db,addForm)
  NRTextendsCategory1(db,domform,cat,getBaseExports(db,addForm),tbl,env) =>
    'lookupIncomplete
  [u,msg,:v] := $why
  SAY '"--------------non extending category----------------------"
  sayPatternMsg('"%1p of category %2p", [domform,u])
  if v ~= nil then
    sayPatternMsg('"%1b %2p",[msg,first v])
  else
    sayPatternMsg('"%1b",[msg])
  SAY '"----------------------------------------------------------"
  'lookupComplete

getBaseExports(db,form) ==
  [op,:argl] := form
  op is 'Record => ['RecordCategory,:argl]
  op is 'Union => ['UnionCategory,:argl]
  op is 'Enumeration => ['EnumerationCategory,:argl]
  op is 'Mapping => ['MappingCategory,:argl]
  op is '%Comma => ['Join,
    :[getBaseExports(db,substSlotNumbers(x,dbTemplate db,dbConstructorForm db))
        for x in argl]]
  [[.,target,:tl],:.] := getConstructorModemap op
  applySubst(pairList($FormalMapVariableList,argl),target)
 
NRTextendsCategory1(db,domform,exCategory,addForm,tbl,env) ==
  addForm is ["%Comma",:r] => 
    and/[extendsCategory(db,domform,exCategory,x,tbl,env) for x in r]
  extendsCategory(db,domform,exCategory,addForm,tbl,env)

--=======================================================================
--         Compute if a domain constructor is forgetful functor
--=======================================================================
extendsCategory(db,dom,u,v,tbl,env) ==
  --does category u extend category v (yes iff u contains everything in v)
  --is dom of category u also of category v?
  u=v => true
  v is ["Join",:l] => and/[extendsCategory(db,dom,u,x,tbl,env) for x in l]
  v is ["CATEGORY",.,:l] =>
    and/[extendsCategory(db,dom,u,x,tbl,env) for x in l]
  v is ["SubsetCategory",cat,d] =>
    extendsCategory(db,dom,u,cat,tbl,env) and isSubset(dom,d,env)
  v := substSlotNumbers(v,dbTemplate db,dbConstructorForm db)
  extendsCategoryBasic(dom,u,v,tbl,env) => true
  $why :=
    v is ['SIGNATURE,op,sig,:.] =>
      [u,['"  has no ",:formatOpSignature(op,sig)]]
    [u,'" has no",v]
  nil
 
extendsCategoryBasic(dom,u,v,tbl,env) ==
  v is ['IF,p,['ATTRIBUTE,c],.] =>
    uVec := getCategoryObjectIfCan(tbl,u,env) or return false
    cons? c and isCategoryForm(c,env) =>
      LASSOC(c,categoryAncestors uVec) is [=p,:.]
    LASSOC(c,categoryAttributes uVec) is [=p,:.]
  u is ["Join",:l] => or/[extendsCategoryBasic(dom,x,v,tbl,env) for x in l]
  u = v => true
  v is ['ATTRIBUTE,c] =>
    cons? c and isCategoryForm(c,env) =>
      extendsCategoryBasic(dom,u,c,tbl,env)
    u is ['CATEGORY,.,:l] =>
      or/[extendsCategoryBasic(dom,x,v,tbl,env) for x in l]
    uVec := getCategoryObjectIfCan(tbl,u,env) or return false
    LASSOC(c,categoryAttributes uVec) is [=true]
  isCategoryForm(v,env) => catExtendsCat?(u,v,tbl,env)
  v is ['SIGNATURE,op,sig,:.] =>
    uVec := getCategoryObjectIfCan(tbl,u,env) or return false
    or/[categoryRef(uVec,i) is [[=op,=sig],:.] for i in $NRTbase..maxIndex uVec]
  u is ['CATEGORY,.,:l] =>
    v is ['IF,:.] => listMember?(v,l)
    false
  false
 
catExtendsCat?(u,v,tbl,env) ==
  u = v => true
  uvec := getCategoryObject(tbl,u,env)
  prinAncestorList := categoryPrincipals uvec
  listMember?(v,prinAncestorList) => true
  vOp := KAR v
  if similarForm := assoc(vOp,prinAncestorList) then
    PRINT u
    sayBrightlyNT '"   extends "
    PRINT similarForm
    sayBrightlyNT '"   but not "
    PRINT v
  or/[catExtendsCat?(x,v,tbl,env) for x in substSource categoryAncestors uvec]
 
substSlotNumbers(form,template,domain) ==
  form is ['SIGNATURE,op,sig,:q] =>
    ['SIGNATURE,op,[substSlotNumbers(x,template,domain) for x in sig],:q]
  form is ['CATEGORY,k,:u] =>
    ['CATEGORY,k,:[substSlotNumbers(x,template,domain) for x in u]]
  expandType(form,template,domain)
 
expandType(lazyt,template,domform) ==
  lazyt isnt [.,:.] => expandTypeArgs(lazyt,template,domform)
  [functorName,:argl] := lazyt
  functorName is ":" =>
    [functorName,first argl,expandTypeArgs(second argl,template,domform)]
  lazyt is ['local,x] =>
    n := symbolPosition(x,$FormalMapVariableList)
    domform.(1 + n)
  [functorName,:[expandTypeArgs(a,template,domform) for a in argl]]
 
expandTypeArgs(u,template,domform) ==
  u is '$ => u
  integer? u => expandType(vectorRef(template,u),template,domform)
  u is [.,y] and u.op in '(%eval QUOTE) => y
  u isnt [.,:.] => u
  expandType(u,template,domform)

folks u == --called by getParentsFor
  u isnt [.,:.] => nil
  u is [op,:v] and op in '(Join PROGN)
    or u is ['CATEGORY,.,:v] => "append"/[folks x for x in v]
  u is ['SIGNATURE,:.] => nil
  u is ['ATTRIBUTE,a] =>
    a is [.,:.] and constructor? a.op => folks a
    nil
  u is ['IF,p,q,r] =>
    q1 := folks q
    r1 := folks r
    q1 or r1 => [['IF,p,q1,r1]]
    nil
  [u]

explodeIfs x == main where  --called by getParentsFor
  main() ==
    x is ['IF,p,a,b] => fn(p,a,b)
    [[x,:true]]
  fn(p,a,b) ==
    [:"append"/[gn(p,y) for y in a],:"append"/[gn(['NOT,p],y) for y in b]]
  gn(p,a) ==
    a is ['IF,q,b,:.] => fn(MKPF([p,q],'AND),b,nil)
    [[a,:p]]

getParentsFor db ==
  constructorForm := dbConstructorForm db
  n := #constructorForm.args
  s1 := pairList(take(n,$TriangleVariableList),$FormalMapVariableList)
  s2 := pairList($FormalMapVariableList,constructorForm.args)
  [:explodeIfs applySubst(s2,applySubst(s1,x)) for x in folks dbCategory db]

--% Subdomains

++ We are defining a functor with head given by `form', as a subdomain
++ of the domain designated by the domain form `super', and predicate
++ `pred' (a VM instruction form).  Emit appropriate info into the
++ databases.
emitSubdomainInfo(form,super,pred) ==
  pred := applySubst!(pairList(form.args,$AtVariables),pred)
  super := applySubst!(pairList(form.args,$AtVariables),super)
  dbSuperDomain(constructorDB form.op) := [super,pred]

++ List of operations defined in a given capsule
++ Each item on this list is of the form
++    (op sig pred)
++ where
++   op:   name of the operation
++   sig:  signature of the operation
++   pred: scope predicate of the operation.
$capsuleFunctions := nil

++ record that the operation `op' with signature `sig' and predicate
++ `pred' is defined in the current capsule of the current domain
++ being compiled.
noteCapsuleFunctionDefinition(op,sig,pred) ==
  listMember?([op,sig,pred],$capsuleFunctions) =>
    stackAndThrow('"redefinition of %1b: %2 %3",
      [op,formatUnabbreviated ["Mapping",:sig],formatIf pred])
  $capsuleFunctions := [[op,sig,pred],:$capsuleFunctions]

++ Clear the list of functions defined in the last domain capsule.
clearCapsuleFunctionTable() ==
  $capsuleFunctions := nil


++ List of exports (paireed with scope predicate) declared in
++ the category of the currend domain or package.
++ Note: for category packages, this list is nil.
$exports := nil

noteExport(db,form,pred) ==
  -- don't recheck category package exports; we just check
  -- them when defining the category.  Plus, we might actually
  -- get indirect duplicates, which is OK.
  $insideCategoryPackageIfTrue => nil
  listMember?([form,pred],$exports) =>
    stackAndThrow('"redeclaration of %1 %2",
      [form,formatIf pred])
  $exports := [[form,pred],:$exports]

clearExportsTable() ==
  $exports := nil

makePredicate l ==
  null l => true
  MKPF(l,"and")

--% FUNCTIONS WHICH MUNCH ON == STATEMENTS

++ List of reserved identifiers for which the compiler has special
++ meanings and that shall not be redefined.
$reservedNames == '(per rep _$)

++ Check that `var' (a variable of parameter name) is not a reversed name.
checkVariableName var ==
  symbolMember?(var,$reservedNames) =>
    stackAndThrow('"You cannot use reserved name %1b as variable",[var])
  var

checkParameterNames parms ==
  for p in parms repeat
    checkVariableName p

compDefine(db,form,m,e) ==
  $macroIfTrue: local := false
  compDefine1(db,form,m,e)

++ We are about to process the body of a capsule.  Check the form of
++ `Rep' definition, and whether it is appropriate to activate the
++ implicitly generated morphisms
++     per: Rep -> %
++     rep: % -> Rep
++ as local inline functions.
checkRepresentation: (%Thing, %Form,%List %Form,%Env) -> %Env
checkRepresentation(db,addForm,body,env) ==
  domainRep := nil
  hasAssignRep := false        -- assume code does not assign to Rep.
  viewFuns := nil

  null body => env             -- Don't be too hard on nothing.
  
  -- Locate possible Rep definition
  for [stmt,:.] in tails body repeat
    stmt is [":=","Rep",val] =>
      domainRep ~= nil =>
        stackAndThrow('"You cannot assign to constant domain %1b",["Rep"])
      if addForm = val then
        stackWarning('"OpenAxiom suggests removing assignment to %1b",["Rep"])
      else if addForm ~= nil then
        stackWarning('"%1b differs from the base domain",["Rep"])
      return hasAssignRep := true
    stmt is ["MDEF","Rep",:.] =>
      stackWarning('"Consider using == definition for %1b",["Rep"])
      return hasAssignRep := true
    stmt is ["IF",.,:l] or stmt is ["SEQ",:l] or stmt is ["exit",:l] =>
      checkRepresentation(db,nil,l,env)
    stmt isnt ["DEF",lhs,sig,val] => nil -- skip for now.
    op := opOf lhs
    op in '(rep per) =>
      domainRep ~= nil =>
        stackAndThrow('"You cannot define implicitly generated %1b",[op])
      viewFuns := [op,:viewFuns]
    op ~= "Rep" => nil        -- we are only interested in Rep definition
    domainRep := val
    viewFuns ~= nil =>
      stackAndThrow('"You cannot define both %1b and %2b",["Rep",:viewFuns])
    -- A package has no "%".
    dbConstructorKind db = "package" =>
      stackAndThrow('"You cannot define %1b in a package",["Rep"])
    -- It is a mistake to define Rep in category defaults
    $insideCategoryPackageIfTrue =>
      stackAndThrow('"You cannot define %1b in category defaults",["Rep"])
    if lhs is [.,.,:.] then   --FIXME: ideally should be 'lhs is [.,:.]'
      stackAndThrow('"%1b does take arguments",["Rep"])
    if sig.target ~= nil then
      stackAndThrow('"You cannot specify type for %1b",["Rep"])
    -- Now, trick the rest of the compiler into believing that
    -- `Rep' was defined the Old Way, for lookup purpose.
    stmt.op := ":="
    stmt.args := ["Rep",domainRep]
    $useRepresentationHack := false          -- Don't confuse `Rep' and `%'.

  -- Shall we perform the dirty tricks?
  if hasAssignRep then
    $useRepresentationHack := true
  -- Domain extensions with no explicit Rep definition have the
  -- the base domain as representation (at least operationally).
  else if null domainRep and addForm ~= nil then
    if dbConstructorKind db = "domain" and addForm isnt ["%Comma",:.] then
      domainRep :=
        addForm is ["SubDomain",dom,.] => 
          $subdomain := true
          dom
        addForm
      $useRepresentationHack := false
      env := putMacro('Rep,domainRep,env)
  env


getSignatureFromMode(form,e) ==
  getXmode(opOf form,e) is ['Mapping,:signature] =>
    #form~=#signature => stackAndThrow ["Wrong number of arguments: ",form]
    applySubst(pairList($FormalMapVariableList,form.args),signature)

compDefine1(db,form,m,e) ==
  $insideExpressionIfTrue: local:= false
  --1. decompose after macro-expanding form
  ['DEF,lhs,signature,rhs] := form := macroExpand(form,e)
  $insideWhereIfTrue and isMacro(form,e) and (m=$EmptyMode or m=$NoValueMode)
     => [lhs,m,putMacro(lhs.op,rhs,e)]
  if lhs is [.,:.] then
    checkParameterNames lhs.args
  null signature.target and symbol? KAR rhs and not builtinConstructor? KAR rhs and
    (sig := getSignatureFromMode(lhs,e)) =>
  -- here signature of lhs is determined by a previous declaration
      compDefine1(db,['DEF,lhs,[sig.target,:signature.source],rhs],m,e)
 
-- RDJ (11/83): when argument and return types are all declared,
--  or arguments have types declared in the environment,
--  and there is no existing modemap for this signature, add
--  the modemap by a declaration, then strip off declarations and recurse
  if lhs is [.,:.] then
    e := compDefineAddSignature(lhs,signature,e)
-- 2. if signature list for arguments is not empty, replace ('DEF,..) by
--       ('where,('DEF,..),..) with an empty signature list;
--     otherwise, fill in all NILs in the signature
  lhs is [.,:.] and (or/[x ~= nil for x in signature.source]) =>
    compDefWhereClause(form,m,e)
  signature.target=$Category =>
    compDefineCategory(form,m,e,$formalArgList)
  isDomainForm(rhs,e) and not $insideFunctorIfTrue =>
    if lhs is [.,:.] then
      e := giveFormalParametersValues(lhs.args,e)
    if signature.target = nil then
      signature := [getTargetFromRhs(lhs,rhs,e),:signature.source]
    rhs := addEmptyCapsuleIfNecessary(signature.target,rhs)
    compDefineFunctor(['DEF,lhs,signature,rhs],m,e,$formalArgList)
  db = nil =>
    -- no free function in library, yet.
    stackAndThrow ['"malformed definition syntax:",form]
  newPrefix :=
    $prefix => makeSymbol strconc(symbolName $prefix,'",",symbolName $op)
    dbAbbreviation db
  compDefineCapsuleFunction(db,form,m,e,newPrefix,$formalArgList)

compDefineAddSignature([op,:argl],signature,e) ==
  (sig:= hasFullSignature(argl,signature,e)) and
   null assoc(['$,:sig],symbolTarget('modemap,getProplist(op,e))) =>
     declForm:=
       [":",[op,:[[":",x,m] for x in argl for m in sig.source]],signature.target]
     [.,.,e]:= comp(declForm,$EmptyMode,e)
     e
  e
 
hasFullSignature(argl,[target,:ml],e) ==
  target =>
    u := [m or get(x,"mode",e) or return 'failed for x in argl for m in ml]
    u is 'failed => nil
    [target,:u]
  nil
 
addEmptyCapsuleIfNecessary: (%Form,%Form) -> %Form
addEmptyCapsuleIfNecessary(target,rhs) ==
  symbolMember?(KAR rhs,$SpecialDomainNames) => rhs
  ['add,rhs,['CAPSULE]]

++ We are about to elaborate a functor definition, but there
++ is no source-level user-supplied target mode on the result.
++ Attempt to infer the target type by compiling the body.
getTargetFromRhs: (%Form, %Form, %Env) -> %Form 
getTargetFromRhs(lhs,rhs,e) ==
  --undeclared target mode obtained from rhs expression
  rhs is ['CAPSULE,:.] =>
    stackSemanticError(['"target category of ",lhs,
      '" cannot be determined from definition"],nil)
  rhs is ['SubDomain,D,:.] => getTargetFromRhs(lhs,D,e)
  rhs is ['add,D,['CAPSULE,:.]] => getTargetFromRhs(lhs,D,e)
  rhs is ['Record,:l] => ['RecordCategory,:l]
  rhs is ['Union,:l] => ['UnionCategory,:l]
  mode(rhs,e) where
    mode(x,e) ==
      $onlyAbstractSlot: local := true -- not yet in codegen phase.
      compOrCroak(x,$EmptyMode,e).mode
 
giveFormalParametersValues(argl,e) ==
  for x in argl | ident? x repeat
    e := giveVariableSomeValue(x,get(x,'mode,e),e)
  e


macroExpand!: (%Form,%Env) -> %Form 
macroExpand!(x,e) ==
  y:= macroExpand(x,e)
  x isnt [.,:.] or y isnt [.,:.] => y
  x.first := first y
  x.rest := rest y
  x

macroExpand: (%Form,%Env) -> %Form 
macroExpand(x,e) ==   --not worked out yet
  x isnt [.,:.] =>
    not ident? x or (u := get(x,"macro",e)) = nil => x
    -- Don't expand a functional macro name by itself.
    u is ['%mlambda,:.] => x
    macroExpand(u,e)
  x is ['DEF,lhs,sig,rhs] =>
    ['DEF,lhs,macroExpandList(sig,e),macroExpand(rhs,e)]
  -- macros should override niladic props
  [op,:args] := x
  ident? op and args = nil and niladicConstructor? op and
    (u := get(op,"macro", e)) => macroExpand(u,e)
  ident? op and (get(op,"macro",e) is ['%mlambda,parms,body]) =>
    nargs := #args
    nparms := #parms
    msg :=
      nargs < nparms => '"Too few arguments"
      nargs > nparms => '"Too many arguments"
      nil
    msg => (stackMessage(strconc(msg,'" to macro %1bp"),[op]); x)
    args' := macroExpandList(args,e)
    applySubst(pairList(parms,args'),body)
  macroExpandList(x,e)
 
macroExpandList(l,e) ==
  [macroExpand(x,e) for x in l]

--% constructor evaluation
 
mkEvalableCategoryForm c ==
  c is [op,:argl] =>
    op is "DomainSubstitutionMacro" => mkEvalableCategoryForm second argl
    op in '(QUOTE mkCategory EnumerationCategory) => c
    op is ":" => [op,second c,mkEvalableCategoryForm third c]
    op in '(CATEGORY SubsetCategory) =>
      [x,m,$e] := compOrCroak(c,$EmptyMode,$e)
      m = $Category => x
      MKQ c
    categoryConstructor? op =>
      [op,:[mkEvalableCategoryForm x for x in argl]]
    MKQ c
  MKQ c
 
evalCategoryForm(x,e) ==
  eval mkEvalableCategoryForm x

++ Return true if we should skip compilation of category package.
++ This situation happens either when there is no default, of we are in
++ bootstrap mode.
skipCategoryPackage? capsule ==
  null capsule or $bootStrapMode

compDefineCategory1(db,df is ['DEF,form,sig,body],m,e,fal) ==
  categoryCapsule :=
    body is ['add,cat,capsule] =>
      body := cat
      capsule
    nil
  if form isnt [.,:.] then
    form := [form]
  [d,m,e]:= compDefineCategory2(db,form,sig,body,m,e,fal)
  if not skipCategoryPackage? categoryCapsule then [.,.,e] :=
    $insideCategoryPackageIfTrue: local := true
    $categoryPredicateList: local := makeCategoryPredicates db
    defaults := mkCategoryPackage(db,cat,categoryCapsule,e)
    T := compDefine1(nil,defaults,$EmptyMode,e)
           or return stackSemanticError(
                        ['"cannot compile defaults of",:bright opOf form],nil)
  [d,m,e]

makeCategoryPredicates db ==
  n := dbArity db
  sl := pairList(take(n,$TriangleVariableList),take(n,rest $FormalMapVariableList))
  fn(dbCategory db,sl,nil) where
    fn(u,sl,pl) ==
      u is ['Join,:.,a] => fn(a,sl,pl)
      u is ["IF",p,:x] => fnl(x,sl,insert(applySubst(sl,p),pl))
      u is ["has",:.] => insert(applySubst(sl,u),pl)
      u is [op,:.] and op in '(SIGNATURE ATTRIBUTE) => pl
      u isnt [.,:.] => pl
      fnl(u,sl,pl)
    fnl(u,sl,pl) ==
      for x in u repeat pl := fn(x,sl,pl)
      pl
 
++ Subroutine of mkCategoryPackage.
++ Return a category-level declaration of an operation described by `desc'.
mkExportFromDescription desc ==
  t :=
    desc.mapKind = 'CONST => ['constant]
    nil
  ['SIGNATURE,desc.mapOperation,desc.mapSignature,:t]

mkCategoryPackage(db,cat,def,e) ==
  [op,:argl] := dbConstructorForm db
  packageName:= makeDefaultPackageName symbolName op
  dbConstructorDefault(db) := packageName
  packageAbb := makeSymbol strconc(symbolName dbAbbreviation db,'"-")
  $options:local := []
  -- This stops the next line from becoming confused
  abbreviationsSpad2Cmd ['package,packageAbb,packageName]
  -- This is a little odd, but the parser insists on calling
  -- domains, rather than packages
  nameForDollar := first setDifference('(S A B C D E F G H I),argl)
  packageArgl := [nameForDollar,:argl]
  capsuleDefAlist := fn(def,nil) where fn(x,oplist) ==
    x isnt [.,:.] => oplist
    x is ['DEF,y,:.] => [opOf y,:oplist]
    fn(x.args,fn(x.op,oplist))
  catvec := evalCategoryForm(dbConstructorForm db,e)
  fullCatOpList := categoryExports JoinInner([catvec],e)
  catOpList :=
    [mkExportFromDescription desc for desc in fullCatOpList
        | symbolMember?(desc.mapOperation,capsuleDefAlist)]
  null catOpList => nil
  packageCategory :=
    ['CATEGORY,'package,
       :applySubst(pairList($FormalMapVariableList,argl),catOpList)]
  nils:= [nil for x in argl]
  packageSig := [packageCategory,dbConstructorForm db,:nils]
  $categoryPredicateList := substitute(nameForDollar,'$,$categoryPredicateList)
  substitute(nameForDollar,'$,['DEF,[packageName,:packageArgl],packageSig,def])
 
++ Return the typing constraint operator for `t' in the environment `e'.
typingKind(t,e) ==
  isCategoryForm(t,e) => 'ofCategory
  'ofType

++ Subroutine of compDefineFunctor1 and compDefineCategory2.
++ Given a constructor definition defining `db', compute implicit
++ parameters and store that list in `db'.
deduceImplicitParameters(db,e) ==
  parms := dbParameters db
  nonparms := [x for [x,:.] in get('%compilerData,'%whereDecls,e)
                 | not symbolMember?(x,parms)]
  nonparms = nil => true
  -- Collect all first-order dependencies.
  preds := nil
  qvars := $QueryVariables
  subst := nil
  for p in parms for i in 1.. repeat
    m := getXmode(p,e)
    ident? m and symbolMember?(m,nonparms) =>
      stackAndThrow('"Parameter %1b cannot be of type implicit parameter %2pb",
                      [p,m])
    m isnt [.,:.] => nil
    preds := [[typingKind(m,e),dbSubstituteFormals(db,p),m],:preds]
    st := [qpair for a in m.args for [v,:qvars] in tails qvars
            | ident? a and symbolMember?(a,nonparms)] where
                 qpair() ==
                   t := getXmode(a,e)
                   preds := [[typingKind(t,e),a,t],:preds]
                   [a,:v]
    subst := [:st,:subst]
  -- Now, build the predicate for implicit parameters.
  for s in nonparms repeat
    x := [rest y for y in subst | symbolEq?(s,first y)]
    x = nil =>
      stackAndThrow('"Implicit parameter %1b has no visible constraint",[s])
    x is [.] => nil -- OK.
    stackAndThrow('"Too many constraints for implicit parameter %1b",[s])
  dbImplicitData(db) := [subst,preds]
    
buildConstructorCondition db ==
  dbImplicitData db is [subst,cond] =>
    ['%exist,substTarget subst,mkpf(applySubst(subst,cond),'AND)]
  true

getArgumentMode: (%Form,%Env) -> %Maybe %Mode 
getArgumentMode(x,e) ==
  string? x => x
  get(x,'mode,e)
 
getArgumentModeOrMoan: (%Form, %Form, %Env) -> %Mode
getArgumentModeOrMoan(x,form,e) ==
  getArgumentMode(x,e) or
    stackSemanticError(["argument ",x," of ",form," is not declared"],nil)

compDefineCategory2(db,form,signature,body,m,e,$formalArgList) ==
    --1. bind global variables
    $prefix: local := nil
    $op: local := form.op
    $definition: local := form   --used by DomainSubstitutionFunction
    $form: local := nil
    $extraParms: local := nil
    e := registerConstructor($op,e)
    -- Remember the body for checking the current instantiation.
    $currentCategoryBody : local := body
         --Set in DomainSubstitutionFunction, used further down
    -- 1.1  augment e to add declaration $: <form>
    dbFormalSubst(db) := pairList(form.args,$TriangleVariableList)
    dbInstanceCache(db) := true
    deduceImplicitParameters(db,e)
    e:= addBinding("$",[['mode,:form]],e)
 
    -- 2. obtain signature
    signature':=
      [signature.target,
        :[getArgumentModeOrMoan(a,form,e) for a in form.args]]
    e := giveFormalParametersValues(form.args,e)
    dbDualSignature(db) :=
      [true,:[isCategoryForm(t,e) for t in signature'.source]]

    -- 3. replace arguments by $1,..., substitute into body,
    --    and introduce declarations into environment
    sargl := take(# form.args, $TriangleVariableList)
    $functorForm:= $form:= [$op,:sargl]
    $formalArgList:= [:sargl,:$formalArgList]
    formalBody := dbSubstituteFormals(db,body)
    signature' := dbSubstituteFormals(db,signature')
    --Begin lines for category default definitions
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $getDomainCode: local := nil
    $addForm: local:= nil
    for x in sargl for t in signature'.source repeat
      [.,.,e]:= compMakeDeclaration(x,t,e)
 
    -- 4. compile body in environment of %type declarations for arguments
    op':= $op
    -- following line causes cats with no with or Join to be fresh copies
    if opOf(formalBody)~='Join and opOf(formalBody)~='mkCategory then
           formalBody := ['Join, formalBody]
    dbCategory(db) := formalBody
    body := optFunctorBody(db,compOrCroak(formalBody,signature'.target,e).expr)
    if $extraParms ~= nil then
      formals := nil
      actuals := nil
      for [u,:v] in $extraParms repeat
        formals := [u,:formals]
        actuals := [MKQ v,:actuals]
      body := ['sublisV,['pairList,quote formals,['%list,:actuals]],body]
    if form.args then body :=  -- always subst for args after extraparms
        ['sublisV,['pairList,quote sargl,['%list,:
          [['devaluate,u] for u in sargl]]],body]
    body:=
      ["%bind",[[g:= gensym(),body]],
         ['%seq,['%store,['%tref,g,0],mkConstructor $form],g]]
    fun := compile(db,[op',['%lambda,sargl,body]],signature')
 
    -- 5. give operator a 'modemap property
    pairlis := pairList(form.args,$FormalMapVariableList)
    parSignature := applySubst(pairlis,dbSubstituteQueries(db,signature'))
    parForm := applySubst(pairlis,form)
 
    -- 6. put modemaps into InteractiveModemapFrame
    dbDomainShell(db) := eval [op',:[MKQ f for f in sargl]]
    dbConstructorModemap(db) :=
      [[parForm,:parSignature],[buildConstructorCondition db,$op]]
    dbPrincipals(db) := getParentsFor db
    dbAncestors(db) := computeAncestorsOf(db,nil)
    dbModemaps(db) := modemapsFromCategory(db,[op',:sargl],formalBody,signature')
    [fun,$Category,e]

mkConstructor: %Form -> %Form
mkConstructor form ==
  form isnt [.,:.] => ['devaluate,form]
  null form.args => quote [form.op]
  ['%list,MKQ form.op,:[mkConstructor x for x in form.args]]
 
compDefineCategory(df,m,e,fal) ==
  -- since we have so many ways to say state the kind of a constructor,
  -- make sure we do have some minimal internal coherence.
  lhs := second df
  ctor := opOf lhs
  db := constructorDB ctor
  kind := dbConstructorKind db
  kind ~= "category" => throwKeyedMsg("S2IC0016",[ctor,"category",kind])
  dbClearForCompilation! db
  dbConstructorForm(db) := lhs
  dbCompilerData(db) := makeCompilationData()
  dbOutputPath(db) := getOutputPath()
  $backend: local := function(x +-> printBackendStmt(dbLibstream db,x))
  try
    $insideFunctorIfTrue => compDefineCategory1(db,df,m,e,fal)
    compDefineLisplib(db,df,m,e,fal,'compDefineCategory1)
  finally dbCompilerData(db) := nil


%CatObjRes                   -- result of compiling a category
  <=> [%Shell,:[%Mode,:[%Env,:null]]]
 
compMakeCategoryObject: (%Form,%Env) -> %Maybe %CatObjRes
compMakeCategoryObject(c,$e) ==
  not isCategoryForm(c,$e) => nil
  u := evalCategoryForm(c,$e) => [u,$Category,$e]
  nil

getCategoryObjectIfCan(tbl,x,env) ==
  obj := tableValue(tbl,x) => obj
  T := compMakeCategoryObject(x,env) => tableValue(tbl,x) := T.expr
  nil

getCategoryObject(tbl,x,env) ==
  getCategoryObjectIfCan(tbl,x,env)
    or systemErrorHere ['getCategoryObject]

predicatesFromAttributes: %List %Form -> %List %Form
predicatesFromAttributes attrList ==
  removeDuplicates [second x for x in attrList]

getModemap(x is [op,:.],e) ==
  for modemap in get(op,'modemap,e) repeat
    if u:= compApplyModemap(x,modemap,e) then return
      ([.,.,sl]:= u; applySubst(sl,modemap))
 
addModemap(op,mc,sig,pred,fn,$e) ==
  $InteractiveMode => $e
  if knownInfo(pred,$e) then pred:=true
  $insideCapsuleFunctionIfTrue =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)
 
addModemapKnown(op,mc,sig,pred,fn,$e) ==
  $insideCapsuleFunctionIfTrue =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)
 
addModemap0(op,mc,sig,pred,fn,e) ==
  --mc is the "mode of computation"; fn the "implementation"
  --fn is ['Subsumed,:.] => e  -- don't skip subsumed modemaps
                               -- breaks -:($,$)->U($,failed) in DP
  op='elt or op='setelt => addEltModemap(op,mc,sig,pred,fn,e)
  addModemap1(op,mc,sig,pred,fn,e)
 
addEltModemap(op,mc,sig,pred,fn,e) ==
   --hack to change selectors from strings to identifiers; and to
   --add flag identifiers as literals in the envir
  op='elt and sig is [:lt,sel] =>
    string? sel =>
      id:= makeSymbol sel
      if $insideCapsuleFunctionIfTrue
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id],pred,fn,e)
    -- sel isnt [.,:.] => systemErrorHere '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  op='setelt and sig is [:lt,sel,v] =>
    string? sel =>
      id:= makeSymbol sel
      if $insideCapsuleFunctionIfTrue
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id,v],pred,fn,e)
    -- sel isnt [.,:.] => systemError '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  systemErrorHere '"addEltModemap"
 
mergeModemap(entry is [[mc,:sig],[pred,:.],:.],modemapList,e) ==
  for (mmtail:= [[[mc',:sig'],[pred',:.],:.],:.]) in tails modemapList repeat
    mc=mc' or isSubset(mc,mc',e) =>
      newmm:= nil
      mm:= modemapList
      while (not sameObject?(mm,mmtail)) repeat (newmm:= [first mm,:newmm]; mm:= rest mm)
      if (mc=mc') and (sig=sig') then
        --We only need one of these, unless the conditions are hairy
        not $forceAdd and TruthP pred' =>
          entry:=nil
              --the new predicate buys us nothing
          return modemapList
        TruthP pred => mmtail:=rest mmtail
          --the thing we matched against is useless, by comparison
      modemapList:= append!(reverse! newmm,[entry,:mmtail])
      entry:= nil
      return modemapList
  if entry then [:modemapList,entry] else modemapList
 
insertModemap(new,mmList) ==
  null mmList => [new]
--isMoreSpecific(new,old:= first mmList) => [new,:mmList]
--[old,:insertModemap(new,rest mmList)]
  [new,:mmList]

mkNewModemapList(mc,sig,pred,fn,curModemapList,e,filenameOrNil) ==
  entry:= [map:= [mc,:sig],[pred,fn],:filenameOrNil]
  listMember?(entry,curModemapList) => curModemapList
  (oldMap:= assoc(map,curModemapList)) and oldMap is [.,[opred, =fn],:.] =>
    $forceAdd => mergeModemap(entry,curModemapList,e)
    opred=true => curModemapList
    if pred ~= true and pred ~= opred then pred:= ["OR",pred,opred]
    [if x=oldMap then [map,[pred,fn],:filenameOrNil] else x
 
  --if new modemap less general, put at end; otherwise, at front
      for x in curModemapList]
  $InteractiveMode => insertModemap(entry,curModemapList)
  mergeModemap(entry,curModemapList,e)
 
addModemap1(op,mc,sig,pred,fn,e) ==
   --mc is the "mode of computation"; fn the "implementation"
  if mc="Rep" then sig := substituteDollarIfRepHack sig
  currentProplist:= getProplist(op,e) or nil
  newModemapList:=
    mkNewModemapList(mc,sig,pred,fn,symbolTarget('modemap,currentProplist),e,nil)
  newProplist:= augProplist(currentProplist,'modemap,newModemapList)
  newProplist':= augProplist(newProplist,"FLUID",true)
  unErrorRef op
        --There may have been a warning about op having no value
  addBinding(op,newProplist',e)
 
getDomainsInScope e ==
  $insideCapsuleFunctionIfTrue => $CapsuleDomainsInScope
  get("$DomainsInScope","special",e)
 
putDomainsInScope(x,e) ==
  l:= getDomainsInScope e
  if $verbose and listMember?(x,l) then 
    sayBrightly ['" Note: Domain ",x," already in scope"]
  newValue := [x,:remove(l,x)]
  $insideCapsuleFunctionIfTrue => ($CapsuleDomainsInScope:= newValue; e)
  put("$DomainsInScope","special",newValue,e)

getOperationAlist(db,name,functorForm,form) ==
  if ident? name and niladicConstructor? name then 
    functorForm := [functorForm]
  (u:= get(functorForm,'isFunctor,$CategoryFrame)) and not
    ($insideFunctorIfTrue and first functorForm=first $functorForm) => u
  $insideFunctorIfTrue and name is "$" =>
    dbDomainShell db = nil => systemError '"$ has no shell now"
    categoryExports dbDomainShell db
  T := compMakeCategoryObject(form,$e) =>
    [.,.,$e] := T
    categoryExports T.expr
  stackMessage('"not a category form: %1bp",[form])
 
substNames(domainName,functorForm,opalist) ==
  functorForm := substitute("$$","$", functorForm)
  nameForDollar :=
    isCategoryPackageName functorForm => second functorForm
    domainName
  [[:substitute("$","$$",substitute(nameForDollar,"$",modemapform)),
       [sel, domainName,if domainName is "$" then pos else
                                         modemapform.mmTarget]]
     for [:modemapform,[sel,"$",pos]] in
       applySubst(pairList($FormalMapVariableList,KDR functorForm),opalist)]
 
evalAndSub(db,domainName,functorForm,form,$e) ==
  $lhsOfColon: local:= domainName
  categoryObject? form =>
    [substNames(domainName,functorForm,categoryExports form),$e]
  --next lines necessary-- see MPOLY for which $ is actual arg. --- RDJ 3/83
  if CONTAINED("$$",form) then $e:= put("$$","mode",get("$","mode",$e),$e)
  opAlist:= getOperationAlist(db,domainName,functorForm,form)
  substAlist:= substNames(domainName,functorForm,opAlist)
  [substAlist,$e]
 
augModemapsFromCategory(db,domainName,functorForm,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(db,domainName,functorForm,categoryForm,e)
  compilerMessage('"Adding %1p modemaps",[domainName])
  e:= putDomainsInScope(domainName,e)
  for [[op,sig,:.],cond,fnsel] in fnAlist repeat
    e:= addModemapKnown(op,domainName,sig,cond,fnsel,e)
  e
 
addConstructorModemaps(name,form is [functorName,:.],e) ==
  $InteractiveMode: local:= nil
  e:= putDomainsInScope(name,e) --frame
  fn := property(functorName,"makeFunctionList")
  [funList,e]:= apply(fn,[name,form,e])
  for [op,sig,opcode] in funList repeat
    if opcode is [sel,dc,n] and sel='ELT then
          nsig := substitute("$$$",name,sig)
          nsig := substitute('$,"$$$",substitute("$$",'$,nsig))
          opcode := [sel,dc,nsig]
    e:= addModemap(op,name,sig,true,opcode,e)
  e
 
augModemapsFromDomain1(db,name,functorForm,e) ==
  property(KAR functorForm,"makeFunctionList") =>
    addConstructorModemaps(name,functorForm,e)
  functorForm isnt [.,:.] and (catform := getmode(functorForm,e)) =>
    augModemapsFromCategory(db,name,functorForm,catform,e)
  mappingForm := getmodeOrMapping(KAR functorForm,e) =>
    ["Mapping",categoryForm,:functArgTypes] := mappingForm
    catform := substituteCategoryArguments(rest functorForm,categoryForm)
    augModemapsFromCategory(db,name,functorForm,catform,e)
  stackMessage('"%1pb is an unknown mode",[functorForm])
  e
 
AMFCR_,redefinedList(op,l) == "OR"/[AMFCR_,redefined(op,u) for u in l]
 
AMFCR_,redefined(opname,u) ==
  not(u is [op,:l]) => nil
  op = 'DEF => opname = CAAR l
  op in '(PROGN SEQ) => AMFCR_,redefinedList(opname,l)
  op = '%when => "OR"/[AMFCR_,redefinedList(opname,rest u) for u in l]

dbClearForCompilation! db ==
  dbTemplate(db) := nil
  dbLookupFunction(db) := nil
  dbCapsuleDefinitions(db) := nil
  dbModemaps(db) := nil
  dbDocumentation(db) := nil
  dbOperations(db) := nil
  dbAttributes(db) := nil
  dbPredicates(db) := nil
  dbAncestors(db) := nil
  dbPrincipals(db) := nil
  dbCategory(db) := nil
  dbPredicates(db) := nil
  dbConstructorModemap(db) := nil
  dbDefaultDomain(db) := nil
  dbDualSignature(db) := nil

substituteCategoryArguments(argl,catform) ==
  argl := substitute("$$","$",argl)
  applySubst(pairList($FormalMapVariableList,argl),catform)
 
++ Register in the current environment, the variable name for the
++ current domain.  Usually it is $; except when we are compiling
++ a synthesized package containing category defaults.
setDollarName(form,env) ==
  name :=
    isCategoryPackageName form.op => first form.args
    '$
  put('%compilerData,'%dollar,name,env)

++ Retrieve the variable name for the current instantiation.
getDollarName env ==
  get('%compilerData,'%dollar,env)

getOutputPath() ==
  outfile := getOptionValue "output"
  outfile = nil => nil
  $insideCategoryPackageIfTrue =>
    d := filePathDirectory outfile
    n := strconc(filePathString filePathName outfile,'"-")
    t := filePathType outfile
    filePathString makeFilePath(directory <- d,name <- n,type <- t)
  outfile

compDefineFunctor(df,m,e,fal) ==
  $profileCompiler: local := true
  $profileAlist:    local := nil
  form := second df
  db := constructorDB opOf form
  dbClearForCompilation! db
  dbConstructorForm(db) := form
  dbCompilerData(db) := makeCompilationData()
  dbOutputPath(db) := getOutputPath()
  $backend: local := function(x +-> printBackendStmt(dbLibstream db,x))
  try compDefineLisplib(db,df,m,e,fal,'compDefineFunctor1)
  finally dbCompilerData(db) := nil
 
compDefineFunctor1(db,df is ['DEF,form,signature,body],m,$e,$formalArgList) ==
    -- 0.  Make `form' a constructor instantiation form
    if form isnt [.,:.] then
      form := [form]
    --  1. bind global variables
    $prefix: local := nil
    $op: local := form.op
    $addForm: local := nil
    $subdomain: local := false
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $form: local := form
    $signature: local := nil
    $functorTarget: local := nil
    $Representation: local := nil
         --Set in doIt, accessed in the compiler - compNoStacking
    $functorForm: local := form
    $functorLocalParameters: local := nil
    $getDomainCode: local := nil -- code for getting views
    $insideFunctorIfTrue: local:= true
    $genSDVar: local:= 0
    originale:= $e
    dbFormalSubst(db) := pairList(form.args,$FormalMapVariableList)
    $e := registerConstructor($op,$e)
    $e := setDollarName(form,$e)
    deduceImplicitParameters(db,$e)
    $formalArgList:= [:form.args,:$formalArgList]
    -- all defaulting packages should have caching turned off
    dbInstanceCache(db) := not isCategoryPackageName $op
    signature':=
      [signature.target,:[getArgumentModeOrMoan(a,form,$e) for a in form.args]]
    if signature'.target = nil then
      signature' := modemap2Signature getModemap($form,$e)
    dbDualSignature(db) :=
      [false,:[isCategoryForm(t,$e) for t in signature'.source]]

    $functorTarget := target := signature'.target
    $e := giveFormalParametersValues(form.args,$e)
    tbl := makeTable function valueEq?  -- category-form/object table
    ds := getCategoryObjectIfCan(tbl,target,$e) or return
       stackAndThrow('"   cannot produce category object: %1pb",[target])
    dbDomainShell(db) := copyVector ds
    attributeList := categoryAttributes ds --see below under "loadTimeAlist"
    $condAlist: local := nil
    $uncondAlist: local := nil
    $NRTslot1PredicateList: local := predicatesFromAttributes attributeList
    $NRTattributeAlist: local := genInitialAttributeAlist(db,attributeList)
    $NRTaddForm: local := nil   -- see compAdd
    -- Generate slots for arguments first, then implicit parameters,
    -- then for $NRTaddForm (if any) in compAdd
    for x in form.args repeat getLocalIndex(db,x)
    for x in dbImplicitParameters db repeat getLocalIndex(db,x)
    [.,.,$e] := compMakeDeclaration("$",target,$e)
    if not $insideCategoryPackageIfTrue  then
      $e := augModemapsFromCategory(db,'$,form,target,$e)
      $e := put('$,'%dc,form,$e)
    $signature := signature'
    parSignature := dbSubstituteAllQuantified(db,signature')
    parForm := dbSubstituteAllQuantified(db,form)
 
    --  3. give operator a 'modemap property
    modemap := [[parForm,:parSignature],[buildConstructorCondition db,$op]]
    dbConstructorModemap(db) := modemap
    dbCategory(db) := modemap.mmTarget

    --  (3.1) now make a list of the functor's local parameters; for
    --  domain D in form.args,check its signature: if domain, its type is Join(A1,..,An);
    --  in this case, D is replaced by D1,..,Dn (gensyms) which are set
    --  to the A1,..,An view of D
    makeFunctorArgumentParameters(db,form.args,signature'.source,signature'.target)
    $functorLocalParameters := form.args

    --  4. compile body in environment of %type declarations for arguments
    op':= $op
    rettype:= signature'.target
    -- If this functor is defined as instantiation of a functor
    -- that is a subdomain of `D', then make this functor also a subdomain
    -- of that super domain `D'.
    if body is ["add",[rhsCtor,:rhsArgs],["CAPSULE"]] 
        and constructor? rhsCtor 
         and (u := getSuperDomainFromDB rhsCtor) then 
           u := sublisFormal(rhsArgs,u,$AtVariables)
           emitSubdomainInfo($form,first u, second u)
    T:= compFunctorBody(db,body,rettype,$e)
    body':= T.expr
    lamOrSlam :=
      dbInstanceCache db = nil => '%lambda
      '%slam
    fun := compile(db,dbSubstituteFormals(db,[op',[lamOrSlam,form.args,body']]),signature')
    --The above statement stops substitutions gettting in one another's way
    operationAlist := dbSubstituteAllQuantified(db,$lisplibOperationAlist)
    dbModemaps(db) := modemapsFromFunctor(db,parForm,operationAlist)
    reportOnFunctorCompilation()
 
    --  5.
    dbPrincipals(db) := getParentsFor db
    dbAncestors(db) := computeAncestorsOf(db,nil)
    $insideFunctorIfTrue:= false
    if not $bootStrapMode then
      dbLookupFunction(db) := NRTgetLookupFunction(db,$NRTaddForm,tbl,$e)
          --either lookupComplete (for forgetful guys) or lookupIncomplete
      $NRTslot1PredicateList :=
        [simpBool x for x in $NRTslot1PredicateList]
      printBackendStmt(dbLibstream db,
        ['MAKEPROP,MKQ $op,''infovec,getInfovecCode(db,$e)])
    $lisplibOperationAlist:= operationAlist
    [fun,['Mapping,:signature'],originale]


++ Finish the incomplete compilation of a functor body.
incompleteFunctorBody(db,m,body,e) ==
  -- The slot numbers from the category shell are bogus at this point.
  -- Nullify them so people don't think they bear any meaningful
  -- semantics (well, they should not think these are forwarding either).
  ops := nil
  for [opsig,pred,funsel] in categoryExports dbDomainShell db repeat
    if pred isnt true then
      pred := simpBool pred
    if funsel is [op,.,.] and op in '(ELT CONST) then
      third(funsel) := nil
    ops := [[opsig,pred,funsel],:ops]
  $lisplibOperationAlist := listSort(function GGREATERP,ops,function first)
  dbSuperDomain(db) :=
    body is ['SubDomain,dom,pred] => [dom,pred]
    body is ['add,['SubDomain,dom,pred],:.] => [dom,pred]
    nil
  [bootStrapError(dbConstructorForm db,$editFile),m,e]

++ Subroutine of compDefineFunctor1.  Called to generate backend code
++ for a functor definition. 
compFunctorBody(db,body,m,e) ==
  $bootStrapMode => incompleteFunctorBody(db,m,body,e)
  clearCapsuleDirectory()        -- start collecting capsule functions.
  T:= compOrCroak(body,m,e)
  $capsuleFunctionStack := reverse! $capsuleFunctionStack
  -- ??? Don't resolve default definitions, yet.
  backendCompile(db,defs) where defs() ==
    $insideCategoryPackageIfTrue => $capsuleFunctionStack
    foldExportedFunctionReferences(db,$capsuleFunctionStack)
  clearCapsuleDirectory()        -- release storage.
  body is [op,:.] and op in '(add CAPSULE) => T
  $NRTaddForm :=
    body is ["SubDomain",domainForm,predicate] => domainForm
    body
  T
 
reportOnFunctorCompilation() ==
  if $semanticErrorStack then sayBrightly '" "
  displaySemanticErrors()
  if $warningStack then sayBrightly '" "
  displayWarnings()
  $functorStats:= addStats($functorStats,$functionStats)
  [byteCount,elapsedSeconds] := $functorStats
  sayBrightly ['%l,:bright '"  Cumulative Statistics for Constructor",$op]
  timeString := normalizeStatAndStringify elapsedSeconds
  sayBrightly ['"      Time:",:bright timeString,'"seconds"]
  sayBrightly '" "
  'done
 
--% domain view code
 
makeFunctorArgumentParameters(db,argl,sigl,target) ==
  $forceAdd: local:= true
  $ConditionalOperators: local := nil
  ("append"/[fn(db,a,augmentSig(s,findExtras(a,target)))
              for a in argl for s in sigl]) where
    findExtras(a,target) ==
      --  see if conditional information implies anything else
      --  in the signature of a
      target is ['Join,:l] => "union"/[findExtras(a,x) for x in l]
      target is ['CATEGORY,.,:l] => "union"/[findExtras1(a,x) for x in l] where
        findExtras1(a,x) ==
          x is ['AND,:l] => "union"/[findExtras1(a,y) for y in l]
          x is ['OR,:l] => "union"/[findExtras1(a,y) for y in l]
          x is ['IF,c,p,q] =>
            union(findExtrasP(a,c),
                  union(findExtras1(a,p),findExtras1(a,q))) where
              findExtrasP(a,x) ==
                x is ['AND,:l] => "union"/[findExtrasP(a,y) for y in l]
                x is ['OR,:l] => "union"/[findExtrasP(a,y) for y in l]
                x is ["has",=a,y] and y is ['SIGNATURE,:.] => [y]
                nil
        nil
    augmentSig(s,ss) ==
       -- if we find something extra, add it to the signature
      null ss => s
      for u in ss repeat
        $ConditionalOperators:=[rest u,:$ConditionalOperators]
      s is ['Join,:sl] =>
        u := objectAssoc('CATEGORY,ss) =>
          MSUBST([:u,:ss],u,s)
        ['Join,:sl,['CATEGORY,'package,:ss]]
      ['Join,s,['CATEGORY,'package,:ss]]
    fn(db,a,s) ==
      isCategoryForm(s,$CategoryFrame) =>
        s is ["Join",:catlist] => genDomainViewList(db,a,s.args)
        [genDomainView(db,a,s,"getDomainView")]
      [a]
 
genDomainOps(db,dom,cat) ==
  oplist:= getOperationAlist(db,dom,dom,cat)
  siglist:= [sig for [sig,:.] in oplist]
  oplist:= substNames(dom,dom,oplist)
  cd:=
    ["%LET",dom,['mkOpVec,dom,['%list,:
      [['%list,MKQ op,['%list,:[mkTypeForm mode for mode in sig]]]
        for [op,sig] in siglist]]]]
  $getDomainCode:= [cd,:$getDomainCode]
  for [opsig,cond,:.] in oplist for i in 0.. repeat
    if listMember?(opsig,$ConditionalOperators) then cond:=nil
    [op,sig]:=opsig
    $e := addModemap(op,dom,sig,cond,['ELT,dom,i],$e)
  dom
 
genDomainView(db,viewName,c,viewSelector) ==
  c is ['CATEGORY,.,:l] => genDomainOps(db,viewName,c)
  code:=
    c is ['SubsetCategory,c',.] => c'
    c
  $e:= augModemapsFromCategory(db,viewName,nil,c,$e)
  cd:= ["%LET",viewName,[viewSelector,viewName,mkTypeForm code]]
  if not listMember?(cd,$getDomainCode) then
          $getDomainCode:= [cd,:$getDomainCode]
  viewName

genDomainViewList(db,id,catlist) ==
  [genDomainView(db,id,cat,"getDomainView") 
     for cat in catlist | isCategoryForm(cat,$EmptyEnvironment)]
 
mkOpVec(dom,siglist) ==
  dom:= getPrincipalView dom
  substargs := [['$,:canonicalForm dom],
                  :pairList($FormalMapVariableList,instantiationArgs dom)]
  oplist:= getConstructorOperationsFromDB instantiationCtor dom
  --new form is (<op> <signature> <slotNumber> <condition> <kind>)
  ops := newVector #siglist
  for (opSig:= [op,sig]) in siglist for i in 0.. repeat
    u := objectAssoc(op,oplist)
    assoc(sig,u) is [.,n,.,'ELT] =>
      vectorRef(ops,i) := vectorRef(dom,n)
    noplist := applySubst(substargs,u)
  -- following variation on assoc needed for GENSYMS in Mutable domains
    AssocBarGensym(substitute(dom.0,'$,sig),noplist) is [.,n,.,'ELT] =>
      vectorRef(ops,i) := vectorRef(dom,n)
    vectorRef(ops,i) := [function Undef,[dom.0,i],:opSig]
  ops
 

++ form is lhs (f a1 ... an) of definition; body is rhs;
++ signature is (t0 t1 ... tn) where t0= target type, ti=type of ai, i > 0;
++ removes declarative and assignment information from form and
++ signature, placing it in list L, replacing form by ("where",form',:L),
++ signature by a list of NILs (signifying declarations are in e)
compDefWhereClause(['DEF,form,signature,body],m,e) ==
  $sigAlist: local := nil
  $predAlist: local := nil
  -- 1. create sigList= list of all signatures which have embedded
  --    declarations moved into global variable $sigAlist
  sigList:=
    [transformType fetchType(a,x,e,form)
      for a in form.args for x in signature.source] where
        fetchType(a,x,e,form) ==
          x => x
          getmode(a,e) or userError concat(
            '"There is no mode for argument",a,'"of function",form.op)
        transformType x ==
          x isnt [.,:.] => x
          x is [":",R,Rtype] =>
            ($sigAlist:= [[R,:transformType Rtype],:$sigAlist]; x)
          x is ['Record,:.] => x --RDJ 8/83
          [x.op,:[transformType y for y in x.args]]
 
  -- 2. replace each argument of the form (|| x p) by x, recording
  --    the given predicate in global variable $predAlist
  argList:=
    [removeSuchthat a for a in form.args] where
      removeSuchthat x ==
        x is ["|",y,p] => ($predAlist:= [[y,:p],:$predAlist]; y)
        x
 
  -- 3. obtain a list of parameter identifiers (x1 .. xn) ordered so that
  --       the type of xi is independent of xj if i < j
  varList :=
    orderByDependency(substSource argDepAlist,substTarget argDepAlist) where
      argDepAlist :=
        [[x,:dependencies] for [x,:y] in argSigAlist] where
          dependencies() ==
            setUnion(listOfIdentifiersIn y,
              remove(listOfIdentifiersIn LASSOC(x,$predAlist),x))
          argSigAlist := [:$sigAlist,:pairList(argList,sigList)]
 
  -- 4. construct a WhereList which declares and/or defines the xi's in
  --    the order constructed in step 3
  whereList := [addSuchthat(x,[":",x,symbolTarget(x,argSigAlist)]) for x in varList]
     where addSuchthat(x,y) ==
             p := LASSOC(x,$predAlist) => ["|",y,p]
             y
 
  -- 5. compile new ('DEF,("where",form',:WhereList),:.) where
  --    all argument parameters of form' are bound/declared in WhereList
  comp(form',m,e) where
    form' := ["where",defform,:whereList] where
      defform := ['DEF,form'',signature',body] where
        form'' := [form.op,:argList]
        signature' := [signature.target,:[nil for x in signature.source]]
 
orderByDependency(vl,dl) ==
  -- vl is list of variables, dl is list of dependency-lists
  selfDependents:= [v for v in vl for d in dl | symbolMember?(v,d)]
  for v in vl for d in dl | symbolMember?(v,d) repeat
    (SAY(v," depends on itself"); fatalError:= true)
  fatalError => userError '"Parameter specification error"
  until vl = nil repeat
    newl:=
      [v for v in vl for d in dl | setIntersection(d,vl) = nil] or return nil
    orderedVarList:= [:newl,:orderedVarList]
    vl' := setDifference(vl,newl)
    dl' := [setDifference(d,newl) for x in vl for d in dl
             | symbolMember?(x,vl')]
    vl := vl'
    dl := dl'
  removeDuplicates reverse! orderedVarList --ordered so ith is indep. of jth if i < j
 
++ Subroutine of compDefineCapsuleFunction.
assignCapsuleFunctionSlot(db,op,sig) ==
  kind := or/[u.mapKind for u in categoryExports dbDomainShell db
                | symbolEq?(op,u.mapOperation) and sig = u.mapSignature]
  kind = nil => nil -- op is local and need not be assigned
  if $insideCategoryPackageIfTrue then
    sig := substitute('$,second dbConstructorForm db,sig)
  desc := [op,'$,:[getLocalIndex(db,x) for x in sig],kind]
  n := dbEntitySlot(db,desc) => n   --already there
  n := dbEntityCount db + $NRTbase
  dbUsedEntities(db) := [[desc,op,'$,:sig,kind],:dbUsedEntities db]
  dbEntityCount(db) := dbEntityCount db + 1
  n

localOperation?(op,e) ==
  not symbolMember?(op,$formalArgList) and getXmode(op,e) is ['Mapping,:.]

++ Subroutine of hasSigInTargetCategory.  
candidateSignatures(op,nmodes,slot1) ==
  [sig for [[=op,sig,:.],:.] in slot1 | #sig = nmodes]

compareMode2Arg(x,m) == null x or modeEqual(x,m)
 
++ Subroutine of compDefineCapsuleFunction.  
++ We are compiling a capsule function definition with head given by `form'.
++ Determine whether the function with possibly partial signature `target'
++ is exported.  Return the complete signature if yes; otherwise
++ return nil, with diagnostic in ambiguity case.
hasSigInTargetCategory(db,form,target,e) ==
  sigs := candidateSignatures(form.op,#form,categoryExports dbDomainShell db)
  cc := checkCallingConvention(sigs,#form.args)
  mList:= [(cc.i > 0 => quasiquote x; getArgumentMode(x,e))
            for x in form.args for i in 0..]
    --each element is a declared mode for the variable or nil if none exists
  potentialSigList :=
    removeDuplicates [sig for sig in sigs | fn(sig,target,mList)] where
      fn(sig,target,mList) ==
        (target = nil or target=sig.target) and
          "and"/[compareMode2Arg(x,m) for x in mList for m in sig.source]
  potentialSigList is [sig] => sig
  potentialSigList = nil => nil
  ambiguousSignatureError(form.op,potentialSigList)
  first potentialSigList
 
++ Subroutine of compDefineCapsuleFunction.
checkAndDeclare(db,form,sig,e) ==
  stack := nil
  -- arguments with declared types must agree with those in sig;
  -- those that don't get declarations put into e
  for a in form.args for m in sig.source repeat
    isQuasiquote m => nil	  -- we just built m from a.
    symbolMember?(a,dbParameters db) =>
      stackAndThrow('"Redeclaration of constructor parameter %1b",[a])
    m1:= getArgumentMode(a,e) =>
      not modeEqual(m1,m) =>
        stack:= ["   ",:bright a,'"must have type ",m,
          '" not ",m1,'"%l",:stack]
    e:= put(a,'mode,m,e)
  if stack then
    sayBrightly ['"   Parameters of ",:bright form.op,
      '" are of wrong type:",'"%l",:stack]
  e

++ Subroutine of compDefineCapsuleFunction.  
addArgumentConditions($body,$functionName) ==
  $argumentConditionList =>
               --$body is only used in this function
    fn $argumentConditionList where
      fn clist ==
        clist is [[n,untypedCondition,typedCondition],:.] =>
          ['%when,[typedCondition,fn rest clist],
            ['%otherwise,["argumentDataError",n,
              MKQ untypedCondition,MKQ $functionName]]]
        null clist => $body
        systemErrorHere ["addArgumentConditions",clist]
  $body
 
++ Subroutine of compDefineCapsuleFunction.
compArgumentConditions: %Env -> %Env
compArgumentConditions e ==
  $argumentConditionList:=
    [f for [n,a,x] in $argumentConditionList] where
      f() ==
        y:= substitute(a,"#1",x)
        T := [.,.,e]:= compOrCroak(y,$Boolean,e)
        [n,x,T.expr]
  e

++ Return true if signature `sig' contains unspeccified modes.
partialSignature? sig ==
  sig.target = nil or (or/[s = nil for s in sig.source])

++ Subroutine of compDefineCapsuleFunction.
++ We are about to elaborate a definition with `form' as head, and
++ parameter types specified in `signature'.  Refine that signature
++ in case some or all of the parameter types are missing.
refineDefinitionSignature(db,form,signature,e) ==
  --let target and local signatures help determine modes of arguments
  signature' :=
    x := hasSigInTargetCategory(db,form,signature.target,e) => x
    x := getSignatureFromMode(form,e) => x
    [signature.target,:[getArgumentMode(a,e) for a in form.args]]
  signature'.source := stripOffSubdomainConditions(signature'.source,form.args)
  partialSignature? signature' => getSignature(form.op,signature'.source,e)
  signature'

++ Subroutine of compDefineCapsuleFunction.
processDefinitionParameters(db,form,signature,e) ==
  e := checkAndDeclare(db,form,signature,e)
  e := giveFormalParametersValues(form.args,e)
  e := addDomain(db,signature.target,e)
  e := compArgumentConditions e
  if $profileCompiler then
    for x in form.args for t in signature.source repeat 
      profileRecord('arguments,x,t)
  for domain in signature repeat
    e := addDomain(db,domain,e)
  e
 
mkRepititionAssoc l ==
  mkRepfun(l,1) where
    mkRepfun(l,n) ==
      null l => nil
      l is [x] => [[n,:x]]
      l is [x, =x,:l'] => mkRepfun(rest l,n+1)
      [[n,:first l],:mkRepfun(rest l,1)]
 
encodeItem x ==
  x is [op,:argl] => getCaps op
  ident? x => symbolName x
  STRINGIMAGE x
 
getCaps x ==
  s := symbolName x
  clist := [c for i in 0..maxIndex s | upperCase? (c := stringChar(s,i))]
  clist = nil => '"__"
  strconc/[charString first clist,
             :[charString charDowncase u for u in rest clist]]

encodeFunctionName(db,fun,signature,count) ==
    if dbDefaultPackage? db then
      signature := substitute('$,first dbParameters db,signature)
    reducedSig := mkRepititionAssoc [:signature.source,signature.target]
    encodedSig :=
      (strconc/[encodedPair for [n,:x] in reducedSig]) where
        encodedPair() ==
          n=1 => encodeItem x
          strconc(toString n,encodeItem x)
    encodedName:= makeSymbol strconc(symbolName dbAbbreviation db,'";",
        symbolName fun,'";",encodedSig,'";",toString count)
    dbCapsuleDefinitions(db) :=
      [[encodedName,signature],:dbCapsuleDefinitions db]
    encodedName

compDefineCapsuleFunction(db,df is ['DEF,form,signature,body],
  m,$e,$prefix,$formalArgList) ==
    e := $e
    --1. bind global variables
    $form: local := nil
    $op: local := nil
    $functionStats: local:= [0,0]
    $argumentConditionList: local := nil
    $finalEnv: local := nil
             --used by ReplaceExitEtc to get a common environment
    $initCapsuleErrorCount: local:= #$semanticErrorStack
    $insideCapsuleFunctionIfTrue: local:= true
    $CapsuleModemapFrame: local:= e
    $CapsuleDomainsInScope: local:= get("$DomainsInScope","special",e)
    $insideExpressionIfTrue: local:= true
    $returnMode: local := m
    $suffix := $suffix + 1
    -- Change "^" to "**" in definitions.  All other places have 
    -- been changed before we get here.
    if form is ["^",:.] then 
      sayBrightly ['"Replacing", :bright '"^", '"with",:bright '"**"]
      form.op := "**"
    [$op,:argl] := form
    $form := [$op,:argl]
    argl:= stripOffArgumentConditions argl
    $formalArgList:= [:argl,:$formalArgList]
    signature := refineDefinitionSignature(db,form,signature,e) or return nil
    $signatureOfForm := signature --this global is bound in compCapsuleItems
    e := processDefinitionParameters(db,form,signature,e)
    rettype := resolve(signature.target,$returnMode)
 
    localOrExported :=
      localOperation?($op,e) => 'local
      'exported
    formattedSig := formatUnabbreviatedSig signature
    sayBrightly ['"   compiling ",localOrExported,
      :bright $op,'": ",:formattedSig]

    pred := makePredicate $predl
    noteCapsuleFunctionDefinition($op,signature,pred)
    T := CATCH('compCapsuleBody, compOrCroak(body,rettype,e))
	 or [$ClearBodyToken,rettype,e]
    --  A THROW to the above CATCH occurs if too many semantic errors occur
    --  see stackSemanticError
    n := assignCapsuleFunctionSlot(db,$op,signature)
    -- Build a name for the implementation.
    op' :=
      localOperation?($op,e) =>
        -- object if the operation is both local and exported.
        if or/[mm.mmDC is '$ for mm in get($op,'modemap,e)] then
          userError ['"%b",$op,'"%d",'" is local and exported"]
        makeSymbol strconc(symbolName $prefix,'";",symbolName $op) 
      encodeFunctionName(db,$op,signature,$suffix)
    if n ~= nil and not $insideCategoryPackageIfTrue then
      updateCapsuleDirectory([n,:op'],pred)
    -- Let the backend know about this function's type
    if $optProclaim then
      proclaimCapsuleFunction(db,op',signature)
    clearReplacement op'   -- Make sure we have fresh info
    -- Finally, build a lambda expression for this function.
    fun :=
      catchTag := MKQ gensym()
      body' := replaceExitEtc(T.expr,catchTag,"TAGGEDreturn",$returnMode)
      body' := addArgumentConditions(body',$op)
      finalBody := ['%scope,catchTag,body']
      compile(db,[op',['%lambda,[:argl,'$],finalBody]],signature)
    $functorStats:= addStats($functorStats,$functionStats)
 
    --7. give operator a 'value property
    [fun,['Mapping,:signature],$e]
 
domainMember(dom,domList) ==
  or/[modeEqual(dom,d) for d in domList]
 
augModemapsFromDomain(db,name,functorForm,e) ==
  symbolMember?(KAR name or name,$DummyFunctorNames) => e
  name = $Category or isCategoryForm(name,e) => e
  listMember?(name,getDomainsInScope e) => e
  if super := superType functorForm then
    e := addNewDomain(db,super,e)
  if name is ["Union",:dl] then for d in stripTags dl
                         repeat e:= addDomain(db,d,e)
  augModemapsFromDomain1(db,name,functorForm,e)

addNewDomain(db,domain,e) ==
  augModemapsFromDomain(db,domain,domain,e)

addDomain(db,domain,e) ==
  domain isnt [.,:.] =>
    domain="$EmptyMode" => e
    domain="$NoValueMode" => e
    not ident? domain or 2 < #(s:= symbolName domain) and
      char "#" = stringChar(s,0) and char "#" = stringChar(s,1) => e
    symbolMember?(domain,getDomainsInScope e) => e
    isLiteral(domain,e) => e
    addNewDomain(db,domain,e)
  (name:= first domain)='Category => e
  domainMember(domain,getDomainsInScope e) => e
  getXmode(name,e) is ["Mapping",target,:.] and isCategoryForm(target,e) =>
      addNewDomain(db,domain,e)
    -- constructor? test needed for domains compiled with $bootStrapMode=true
  isDomainForm(domain,e) => addNewDomain(db,domain,e)
  -- ??? we should probably augment $DummyFunctorNames with CATEGORY
  -- ??? so that we don't have to do this special check here.  Investigate.
  isQuasiquote domain => e 
  if not isCategoryForm(domain,e) and name ~= "Mapping" then
    unknownTypeError name
  e        --is not a functor

++ Subroutine of getSignature.
++ Return true if the given parameter type list `src' is a refinment of
++ of the seed `pat'.
sourceMatches?(src,pat) ==
  repeat
    src = nil => return pat = nil
    pat = nil => return src = nil
    pat.first ~= nil and src.first ~= pat.first => return false
    src := src.rest
    pat := pat.rest

getSignature(op,argModeList,e) ==
  mmList := get(op,'modemap,e)
  dollar := getDollarName e
  sigl := removeDuplicates
      [sig for [[dc,:sig],[pred,:.]] in mmList
         | dc=dollar and sourceMatches?(sig.source,argModeList)
            and knownInfo(pred,e)]
  sigl is [sig] => sig
  null sigl =>
    getXmode(op,e) is ['Mapping,:sig] => sig
    SAY '"************* USER ERROR **********"
    SAY("available signatures for ",op,": ")
    if null mmList
       then SAY "    NONE"
       else for [[dc,:sig],:.] in mmList repeat printSignature("     ",op,sig)
    printSignature("NEED ",op,["?",:argModeList])
    nil
  stackSemanticError(["duplicate signatures for ",op,": ",argModeList],nil)
 
--% ARGUMENT CONDITION CODE
 
stripOffArgumentConditions argl ==
  [f for x in argl for i in 1..] where
    f() ==
      x is ["|",arg,condition] =>
        condition:= substitute('_#1,arg,condition)
        -- in case conditions are given in terms of argument names, replace
        $argumentConditionList:= [[i,arg,condition],:$argumentConditionList]
        arg
      x
 
stripOffSubdomainConditions(margl,argl) ==
  [f for x in margl for arg in argl for i in 1..] where
    f() ==
      x is ['SubDomain,marg,condition] =>
        pair:= assoc(i,$argumentConditionList) =>
          (pair.rest.first := MKPF([condition,second pair],'AND); marg)
        $argumentConditionList:= [[i,arg,condition],:$argumentConditionList]
        marg
      x
 
putInLocalDomainReferences(db,def := [opName,[lam,varl,body]]) ==
  NRTputInTail(db,CDDADR def)
  def
 
 
compile(db,u,signature) ==
  optimizedBody := optimizeFunctionDef u
  stuffToCompile :=
    $insideCapsuleFunctionIfTrue =>
      putInLocalDomainReferences(db,optimizedBody)
    optimizedBody
  $doNotCompileJustPrint => (PRETTYPRINT stuffToCompile; first u)
  $macroIfTrue => constructMacro stuffToCompile
  try spadCompileOrSetq(db,stuffToCompile)
  finally
    functionStats := [0,elapsedTime()]
    $functionStats := addStats($functionStats,functionStats)
    printStats functionStats

++ Subroutine of compile.  Called to generate backend code for
++ items defined directly or indirectly at capsule level.   This is
++ also used to compile functors.
spadCompileOrSetq(db,form is [nam,[lam,vl,body]]) ==
        --bizarre hack to take account of the existence of "known" functions
        --good for performance (LISPLLIB size, BPI size, NILSEC)
  CONTAINED($ClearBodyToken,body) => sayBrightly ['"  ",:bright nam,'" not compiled"]

  vl := cleanParameterList! vl
  if $optReplaceSimpleFunctions then
    body := replaceSimpleFunctions body

  if nam' := forwardingCall?(vl,body) then
      registerFunctionReplacement(db,nam,nam')
      sayBrightly ['"     ",:bright nam,'"is replaced by",:bright nam']
  else if macform := expandableDefinition?(vl,body) then
    registerFunctionReplacement(db,nam,macform)
    [:vl',.] := vl
    sayBrightly ['"     ",:bright prefix2String [nam,:vl'],
                   '"is replaced by",:bright prefix2String body]

  form := 
    getFunctionReplacement nam => 
      [nam,[lam,vl,["DECLARE",["IGNORE",last vl]],body]]
    [nam,[lam,vl,body]]

  $insideCapsuleFunctionIfTrue => 
    $optExportedFunctionReference =>
      $capsuleFunctionStack := [form,:$capsuleFunctionStack]
      first form
    first backendCompile(db,[form])
  compileConstructor(db,form)
 
compileConstructor(db,form) ==
  u := compileConstructor1(db,form)
  clearClams()                  --clear all CLAMmed functions
  clearConstructorCache u      --clear cache for constructor
  u
 
compileConstructor1(db,form:=[fn,[key,vl,:bodyl]]) ==
-- fn is the name of some category/domain/package constructor;
-- we will cache all of its values on $ConstructorCache with reference
-- counts
  dbConstructorKind db = 'category =>
    first compAndDefine(db,[[fn,['%slam,vl,:bodyl]]])
  dbInstanceCache db = nil =>
    first backendCompile(db,[[fn,['%lambda,vl,:bodyl]]])
  compHash(db,fn,vl,bodyl)
 
++ Subroutine of compileConstructor1.  Called to compile the body
++ of a category constructor definition.
compAndDefine(db,l) ==
  $backend: local := function(x +-> evalAndPrintBackendStmt(dbLibstream db,x))
  backendCompile(db,l)

compHash(db,op,argl,body) ==
--   Entries will be stored on the global hashtable in a uniform way:
--        (<argument list>, <reference count>,:<value>)
--   where the reference count is optional
  auxfn := makeWorkerName op
  cacheName := "$ConstructorCache"
  g2 := gensym()  --value computed by calling function
  putCode :=
    argl = nil =>
      ['CDDAR,['%store,['tableValue,cacheName,MKQ op],
        ['%list,['%pair,'%nil,['%pair,1,[auxfn]]]]]]
    [auxfn,:argl]
  putCode :=
     ['UNWIND_-PROTECT,['PROG1,putCode,['%store,g2,'%true]],
                  ['%when,[['%not,g2],['tableRemove!,cacheName,MKQ op]]]]
  getCode :=
    argl = nil => ['tableValue,cacheName,MKQ op]
    key :=
      argl is [g] => ['%list,['devaluate,g]]
      ['%list,:[['devaluate,x] for x in argl]]
    ['lassocShiftWithFunction,key,
      ['tableValue,cacheName,MKQ op],['%function,'domainEqualList]]
  returnFoundValue :=
    argl = nil => ['CDRwithIncrement,['CDAR,g2]]
    ['CDRwithIncrement,g2]
  codeBody := mkBind([[g2,getCode]],
                ['%when,[g2,returnFoundValue],['%otherwise,putCode]])
 
  computeFunction := [auxfn,['%lambda,argl,:body]]
  if $reportCompilation then
    sayBrightlyI bright '"Generated code for function:"
    pp computeFunction
  backendCompile(db,[[op,['%lambda,argl,codeBody]],computeFunction])
  op
 
constructMacro: %Form -> %Form
constructMacro (form is [nam,[lam,vl,body]]) ==
  not (and/[x isnt [.,:.] for x in vl]) =>
    stackSemanticError(["illegal parameters for macro: ",vl],nil)
  ["XLAM",vl':= [x for x in vl | ident? x],body]
 
listInitialSegment(u,v) ==
  null u => true
  null v => nil
  first u=first v and listInitialSegment(rest u,rest v)
  --returns true iff u.i=v.i for i in 1..(#u)-1
 
modemap2Signature [[.,:sig],:.] == sig

uncons: %Form -> %Form 
uncons x ==
  x isnt [.,:.] => x
  x is ["CONS",a,b] => [a,:uncons b]
 
--% CAPSULE
 
bootStrapError(functorForm,sourceFile) ==
  ['%when, _
    ['$bootStrapMode, _
        ['%vector,mkTypeForm functorForm,nil,nil,nil,nil,nil]],
    ['%otherwise, ['systemError,['%list,'"%b",MKQ functorForm.op,'"%d",'"from", _
      '"%b",MKQ namestring sourceFile,'"%d",'"needs to be compiled"]]]]

registerInlinableDomain x ==
  x is [ctor,:.] =>
    ctor is ":" => registerInlinableDomain third x
    ctor is 'Enumeration => nil
    builtinFunctorName? ctor =>
      for t in x.args repeat
        registerInlinableDomain t
    constructor? ctor =>
      nominateForInlining ctor
      cosig := getDualSignature ctor or return nil
      for a in x.args for t in cosig.source | t and a is [.,:.] repeat
        registerInlinableDomain a
  nil

compAdd(['add,$addForm,capsule],m,e) ==
  $bootStrapMode =>
    if $addForm is ["%Comma",:.] then code := nil
       else [code,m,e]:= comp($addForm,m,e)
    [['%when, _
       ['$bootStrapMode, _
           code],_
       ['%otherwise, ['systemError,['%list,'"%b",MKQ $functorForm.op,'"%d",'"from", _
         '"%b",MKQ namestring $editFile,'"%d",'"needs to be compiled"]]]],m,e]
  $addFormLhs: local:= $addForm
  db := currentDB e
  if $addForm is ["SubDomain",domainForm,predicate] then
    $NRTaddForm := domainForm
    getLocalIndex(db,domainForm)
    registerInlinableDomain domainForm
    --need to generate slot for add form since all $ go-get
    --  slots will need to access it
    [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
  else
    $NRTaddForm := $addForm
    [$addForm,.,e]:=
      $addForm is ["%Comma",:.] =>
        $NRTaddForm := ["%Comma",:[getLocalIndex(db,x) for x in $addForm.args]]
        for x in $addForm.args repeat
          registerInlinableDomain x
        compOrCroak(compTuple2Record $addForm,$EmptyMode,e)
      registerInlinableDomain $addForm
      compOrCroak($addForm,$EmptyMode,e)
  compCapsule(capsule,m,e)
 
compTuple2Record u ==
  ['Record,:[[":",i,x] for i in 1.. for x in u.args]]

compCapsule(['CAPSULE,:itemList],m,e) ==
  $bootStrapMode =>
    [bootStrapError($functorForm, $editFile),m,e]
  $insideExpressionIfTrue: local:= false
  $useRepresentationHack := true
  db := currentDB e
  clearCapsuleFunctionTable()
  e := checkRepresentation(db,$addFormLhs,itemList,e)
  compCapsuleInner(db,itemList,m,addDomain(db,'$,e))
 
compSubDomain(["SubDomain",domainForm,predicate],m,e) ==
  $addFormLhs: local:= domainForm
  $addForm: local := nil
  $NRTaddForm := domainForm
  [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
  compCapsule(['CAPSULE],m,e)
 
compSubDomain1(domainForm,predicate,m,e) ==
  [.,.,e]:=
    compMakeDeclaration("#1",domainForm,addDomain(currentDB e,domainForm,e))
  u:=
    compCompilerPredicate(predicate,e) or
      stackSemanticError(["predicate: ",predicate,
        " cannot be interpreted with #1: ",domainForm],nil)
  pred := simplifyVMForm u.expr
  -- For now, reject predicates that directly reference domains
  usesVariable?(pred,'$) => 
    stackAndThrow('"predicate %1pb is not simple enough",[predicate])
  emitSubdomainInfo($form,domainForm,pred)
  [domainForm,m,e]

compCapsuleInner(db,itemList,m,e) ==
  e:= addInformation(m,e)
           --puts a new 'special' property of $Information
  data := ["PROGN",:itemList]
      --RPLACd by compCapsuleItems and Friends
  e := compCapsuleItems(db,itemList,nil,e)
  localParList:= $functorLocalParameters
  if $addForm ~= nil then
    data := ['add,$addForm,data]
  code :=
    dbForCategory? db => data
    buildFunctor(db,$signature,data,localParList,e)
  [MKPF([:$getDomainCode,code],"PROGN"),m,e]
 
--% PROCESS FUNCTOR CODE
 
compCapsuleItems(db,itemlist,$predl,$e) ==
  $signatureOfForm: local := nil
  $suffix: local:= 0
  for item in itemlist repeat 
    $e:= compSingleCapsuleItem(db,item,$predl,$e)
  $e
 
compSingleCapsuleItem(db,item,$predl,$e) ==
  doIt(db,macroExpand!(item,$e),$predl)
  $e
 

++ subroutine of doIt.  Called to generate runtime noop insn.
mutateToNothing item ==
  item.op := 'PROGN
  item.rest := nil

doIt(db,item,$predl) ==
  $GENNO: local:= 0
  item is ['SEQ,:l,['exit,1,x]] =>
    item.op := "PROGN"
    lastNode(item).first := x
    for it1 in rest item repeat $e:= compSingleCapsuleItem(db,it1,$predl,$e)
        --This will RPLAC as appropriate
  isDomainForm(item,$e) =>
    -- convert naked top level domains to import.
    -- Note: The apparent useless destructing of `item' below is necessary
    -- because it is subject to RPLACA/RPLACD, which would create
    -- a cycle otherwise.
    u:= ["import", [first item,:rest item]]
    stackWarning('"Use: import %1p",[[first item,:rest item]])
    item.op := u.op
    item.rest := rest u
    doIt(db,item,$predl)
  item is [":=",lhs,rhs,:.] =>
    compOrCroak(item,$EmptyMode,$e) isnt [code,.,$e] =>
      stackSemanticError(["cannot compile assigned value to",:bright lhs],nil)
    not (code is ["%LET",lhs',rhs',:.] and lhs' isnt [.,:.]) =>
      code is ["PROGN",:.] =>
         stackSemanticError(["multiple assignment ",item," not allowed"],nil)
      item.first := first code
      item.rest := rest code
    lhs:= lhs'
    if not symbolMember?(KAR rhs,$NonMentionableDomainNames) and
      not symbolMember?(lhs, $functorLocalParameters) then
         $functorLocalParameters:= [:$functorLocalParameters,lhs]
    if code is ["%LET",.,rhs',:.] and isDomainForm(rhs',$e) then
      if lhs="Rep" then
        --$Representation bound by compDefineFunctor, used in compNoStacking
        $Representation := getRepresentation $e
        if $optimizeRep then
          registerInlinableDomain $Representation
    code is ["%LET",:.] =>
      db := currentDB $e
      item.op := '%store
      rhsCode := rhs'
      item.args := [['%tref,'$,getLocalIndex(db,lhs)],rhsCode]
    item.op := code.op
    item.rest := rest code
  item is [":",a,t] => [.,.,$e]:= compOrCroak(item,$EmptyMode,$e)
  item is ["import",:doms] =>
     for dom in doms repeat
       sayBrightly ['"   importing ",:formatUnabbreviated dom]
     [.,.,$e] := compOrCroak(item,$EmptyMode,$e)
     mutateToNothing item
  item is ["%Inline",type] =>
    processInlineRequest(type,$e)
    mutateToNothing item
  item is ["%SignatureImport",:.] =>
    [.,.,$e] := compSignatureImport(item,$EmptyMode,$e)
    mutateToNothing item
  item is ["IF",p,x,y] => doItConditionally(db,item,$predl)
  item is ["where",b,:l] => compOrCroak(item,$EmptyMode,$e)
  item is ["MDEF",:.] => [.,.,$e]:= compOrCroak(item,$EmptyMode,$e)
  item is ['DEF,lhs,:.] =>
    op := opOf lhs
    body := isMacro(item,$e) => $e := putMacro(op,body,$e)
    [.,.,$e] := t := compOrCroak(item,$EmptyMode,$e)
    item.op := "CodeDefine"
        --Note that DescendCode, in CodeDefine, is looking for this
    second(item) := [op,$signatureOfForm]
      --This is how the signature is updated for buildFunctor to recognise
    third(item) := ['%function,t.expr]
    item.rest.rest.rest := nil
  u := compOrCroak(item,$EmptyMode,$e) =>
    ([code,.,$e]:= u; item.first := first code; item.rest := rest code)
  systemErrorHere ["doIt", item]
 
isMacro(x,e) ==
  x is ['DEF,[op],[nil],body] and
    get(op,'modemap,e) = nil and get(op,'mode,e) = nil => body
  nil

++ Compile capsule-level `item' which is a conditional expression.
++ OpenAxiom's take on prepositional logical is a constructive
++ interpretation of logical connectives, in terms of IF-expresions.
++ In particular, a negation is positively interpretated by swapping
++ branches, and- and or-expressions are decomposed into nested
++ IF-expressions.  -- gdr, 2009-06-15.
doItConditionally(db,item,predl) ==
  item isnt ["IF",p,x,y] => systemErrorHere ["doItConditionally",item]
  p is ["not",p'] =>
    -- swap branches and recurse for positive interpretation.
    item.rest.first := p'
    item.rest.rest.first := y
    item.rest.rest.rest.first := x
    doItConditionally(db,item,predl)
  p is ["and",p',p''] =>
    item.rest.first := p'
    item.rest.rest.first := ["IF",p'',x,copyTree y]
    doItConditionally(db,item,predl)
  p is ["or",p',p''] =>
    item.rest.first := p'
    item.rest.rest.rest.first := ["IF",p'',copyTree x,y]
    doItConditionally(db,item,predl)
  doItIf(db,item,predl,$e)
    
 
doItIf(db,item is [.,p,x,y],$predl,$e) ==
  olde:= $e
  [p',.,$e]:= compCompilerPredicate(p,$e) or userError ['"not a Boolean:",p]
  oldFLP:=$functorLocalParameters
  if x~="%noBranch" then
    compSingleCapsuleItem(db,x,[p,:$predl],getSuccessEnvironment(p,$e))
    x':=localExtras(oldFLP)
  oldFLP:=$functorLocalParameters
  if y~="%noBranch" then
    compSingleCapsuleItem(db,y,[["not",p],:$predl],getInverseEnvironment(p,olde))
    y':=localExtras(oldFLP)
  item.op := '%when
  item.args := [[p',x,:x'],['%otherwise,y,:y']]
 where localExtras(oldFLP) ==
   sameObject?(oldFLP,$functorLocalParameters) => nil
   flp1:=$functorLocalParameters
   oldFLP':=oldFLP
   n:=0
   while oldFLP' repeat
     oldFLP':=rest oldFLP'
     flp1:=rest flp1
     n:=n+1
   -- Now we have to add code to compile all the elements
   -- of functorLocalParameters that were added during the
   -- conditional compilation
   nils:=ans:=[]
   for u in flp1 repeat -- is =u form always an atom?
     if u isnt [.,:.] or (or/[v is [.,=u,:.] for v in $getDomainCode])
       then
         nils:=[u,:nils]
       else
         gv := gensym()
         ans:=[["%LET",gv,u],:ans]
         nils:=[gv,:nils]
     n:=n+1
   $functorLocalParameters:=[:oldFLP,:reverse! nils]
   reverse! ans
 
--% CATEGORY AND DOMAIN FUNCTIONS

compJoin(["Join",:argl],m,e) ==
  catList:= [(compForMode(x,$Category,e) or return 'failed).expr for x in argl]
  catList='failed => stackSemanticError(["cannot form Join of: ",argl],nil)
  catList':=
    [extract for x in catList] where
      extract() ==
        isCategoryForm(x,e) =>
          parameters:=
            union("append"/[getParms(y,e) for y in rest x],parameters)
              where getParms(y,e) ==
                y isnt [.,:.] =>
                  isDomainForm(y,e) => [y]
                  nil
                y is [op,y'] and op in '(LENGTH %llength) => [y,y']
                [y]
          x
        x is ["DomainSubstitutionMacro",pl,body] =>
          (parameters:= union(pl,parameters); body)
        x is ["mkCategory",:.] => x
        ident? x and getXmode(x,e) = $Category => x
        stackSemanticError(["invalid argument to Join: ",x],nil)
        x
  T := [['DomainSubstitutionMacro,parameters,["Join",:catList']],$Category,e]
  convert(T,m)

compForMode: (%Form,%Mode,%Env) -> %Maybe %Triple 
compForMode(x,m,e) ==
  $compForModeIfTrue: local:= true
  $bootStrapMode and m = $Category =>
    op := opOf x
    ident? op and (db := constructorDB op) =>
      dbConstructorKind db = "category" => [x,m,e]
      nil
    comp(x,m,e)
  comp(x,m,e)

makeCategoryForm(c,e) ==
  not isCategoryForm(c,e) => nil
  [x,m,e]:= compOrCroak(c,$EmptyMode,e)
  [x,e]

mustInstantiate: %Form -> %Thing 
mustInstantiate D ==
  D is [fn,:.] and 
    not (symbolMember?(fn,$DummyFunctorNames) or property(fn,"makeFunctionList"))

mkExplicitCategoryFunction(domainOrPackage,sigList,atList) ==
  body:=
    ["mkCategory",MKQ domainOrPackage,['%list,:reverse sigList],
      ['%list,:reverse atList],MKQ domList,nil] where
        domList() ==
          ("union"/[fn sig for ['QUOTE,[[.,sig,:.],:.]] in sigList]) where
            fn sig == [D for D in sig | mustInstantiate D]
  parameters:=
    removeDuplicates
      ("append"/
        [[x for x in sig | ident? x and x~='_$]
          for ['QUOTE,[[.,sig,:.],:.]] in sigList])
  ['DomainSubstitutionMacro,parameters,body]

DomainSubstitutionFunction(parameters,body) ==
  if parameters ~= nil then
    (body := Subst(parameters,body)) where
      Subst(parameters,body) ==
        body isnt [.,:.] =>
          objectMember?(body,parameters) => MKQ body
          body
        listMember?(body,parameters) =>
          g := gensym()
          $extraParms := [[g,:body],:$extraParms]
           --Used in SetVector12 to generate a substitution list
           --bound in buildFunctor
           --For categories, bound and used in compDefineCategory
          MKQ g
        first body is 'QUOTE => body
        cons? $definition and isFunctor body.op and 
          body.op ~= $definition.op => quote simplifyVMForm body
        [Subst(parameters,u) for u in body]
  body isnt ["Join",:.] => body
  $definition isnt [.,:.] => body
  $definition.args = nil => body 
  g := gensym()
  ['%bind,[[g,['constructorDB,quote $definition.op]]],
    ['%when,[['dbTemplate,g]],
      ['%otherwise,['%store,['dbTemplate,g],body]]]]


++ Subroutine of compCategoryItem.
++ Compile exported signature `opsig' under predicate `pred' in
++ environment `env'. The parameters `sigs' is a reference to a list
++ of signatures elaborated so far.
compSignature(db,opsig,pred,env,sigs) ==
  [op,:sig] := opsig
  cons? op =>
    for y in op repeat 
      compSignature(db,[y,:sig],pred,env,sigs)
  op in '(per rep) =>
    stackSemanticError(['"cannot export signature for", :bright op],nil)
    nil
  noteExport(db,opsig,pred)
  deref(sigs) := [MKQ [opsig,pred],:deref sigs]
 
++ Subroutine of comCategory.
++ Elaborate a category-level item `x' under the predicates `predl'.
++ The parameters `sigs' and `atts' are references to list of
++ signatures and attributes elaborated so far.
compCategoryItem(db,x,predl,env,sigs,atts) ==
  x is nil => nil
  --1. if x is a conditional expression, recurse; otherwise, form the predicate
  x is ['%when,[p,e]] =>
    predl':= [p,:predl]
    e is ["PROGN",:l] =>
      for y in l repeat compCategoryItem(db,y,predl',env,sigs,atts)
    compCategoryItem(db,e,predl',env,sigs,atts)
  x is ["IF",a,b,c] =>
    a is ["not",p] => compCategoryItem(db,["IF",p,c,b],predl,env,sigs,atts)
    a is ["and",p,q] =>
      compCategoryItem(db,["IF",p,["IF",q,b,c],copyTree c],predl,env,sigs,atts)
    a is ["or",p,q] =>
      compCategoryItem(db,["IF",p,b,["IF",q,copyTree b,c]],predl,env,sigs,atts)
    predl':= [a,:predl]
    if b~="%noBranch" then
      b is ["PROGN",:l] => 
        for y in l repeat compCategoryItem(db,y,predl',env,sigs,atts)
      compCategoryItem(db,b,predl',env,sigs,atts)
    c="%noBranch" => nil
    predl':= [["not",a],:predl]
    c is ["PROGN",:l] => 
      for y in l repeat
        compCategoryItem(db,y,predl',env,sigs,atts)
    compCategoryItem(db,c,predl',env,sigs,atts)
  pred := (predl => MKPF(predl,"AND"); true)
 
  --2. if attribute, push it and return
  x is ["ATTRIBUTE",y] => 
    -- Attribute 'nil' carries no semantics.
    y = "nil" => nil
    noteExport(db,y,pred)
    deref(atts) := [MKQ [y,pred],:deref atts]
 
  --3. it may be a list, with PROGN as the first, and some information as the rest
  x is ["PROGN",:l] => 
    for u in l repeat 
      compCategoryItem(db,u,predl,env,sigs,atts)
 
  -- 4. otherwise, x gives a signature for a
  --    single operator name or a list of names; if a list of names,
  --    recurse
  x is ["SIGNATURE",:opsig] => compSignature(db,opsig,pred,env,sigs)
  systemErrorHere ["compCategoryItem",x]

compCategory: (%Form,%Mode,%Env) -> %Maybe %Triple
compCategory(x,m,e) ==
  clearExportsTable()
  db := currentDB e
  m := resolve(m,$Category)
  m = $Category and x is ['CATEGORY,kind,:l] =>
      sigs := ref nil
      atts := ref nil
      for x in l repeat
        compCategoryItem(db,x,nil,e,sigs,atts)
      rep := mkExplicitCategoryFunction(kind,deref sigs,deref atts)
    --if inside compDefineCategory, provide for category argument substitution
      [rep,m,e]
  systemErrorHere ["compCategory",x]

--%
