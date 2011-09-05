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

import nruncomp
import g_-error
import c_-util
import database

namespace BOOT

module define where
  compDefine: (%Form,%Mode,%Env) -> %Maybe %Triple 
  compSubDomain: (%Form,%Mode,%Env) -> %Maybe %Triple
  compCapsule: (%Form, %Mode, %Env) -> %Maybe %Triple
  compJoin: (%Form,%Mode,%Env) -> %Maybe %Triple 
  compAdd: (%Form, %Mode, %Env) -> %Maybe %Triple 
  compCategory: (%Form,%Mode,%Env) -> %Maybe %Triple 


--%

$newCompCompare := false

++ when non nil, holds the declaration number of a function in a capsule.
$suffix := nil

$doNotCompileJustPrint := false

++ stack of pending capsule function definitions.
$capsuleFunctionStack := []

--%

$forceAdd := false

$functionStats := nil
$functorStats := nil

$lisplibCategory := nil
$CheckVectorList := []
$pairlis := []
$functorTarget := nil
$condAlist := []
$uncondAlist := []
$NRTslot1PredicateList := []
$NRTattributeAlist := []
$NRTslot1Info := nil
$NRTdeltaListComp := []
$template := nil
$signature := nil
$lookupFunction := nil
$byteAddress := nil
$byteVec := nil
$sigAlist := []
$predAlist := []
$argumentConditionList := []
$finalEnv := nil
$initCapsuleErrorCount := nil
$CapsuleModemapFrame := nil
$CapsuleDomainsInScope := nil
$signatureOfForm := nil
$addFormLhs := nil
$sigList := []
$atList := []

++ List of declarations appearing as side conditions of a where-expression.
$whereDecls := nil

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
 
addInfo(u,e) == 
  $Information:= [formatInfo(u,e),:$Information]
 
addInformation(m,e) ==
  $Information: local := nil
  info(m,e) where
    info(m,e) ==
      --Processes information from a mode declaration in compCapsule
      m isnt [.,:.] => nil
      m is ["CATEGORY",.,:stuff] => for u in stuff repeat addInfo(u,e)
      m is ["Join",:stuff] => for u in stuff repeat info(u,e)
      nil
  put("$Information","special",
       [:$Information,:get("$Information","special",e)],e)
 
hasToInfo (pred is ["has",a,b]) ==
  b is ["SIGNATURE",:data] => ["SIGNATURE",a,:data]
  b is ["ATTRIBUTE",c] => ["ATTRIBUTE",a,c]
  pred
 
++ Return true if we are certain that the information
++ denotated by `pred' is derivable from the current environment. 
knownInfo(pred,env) ==
  pred=true => true
  listMember?(pred,get("$Information","special",env)) => true
  pred is ["OR",:l] => or/[knownInfo(u,env) for u in l]
  pred is ["AND",:l] => and/[knownInfo(u,env) for u in l]
  pred is ["or",:l] => or/[knownInfo(u,env) for u in l]
  pred is ["and",:l] => and/[knownInfo(u,env) for u in l]
  pred is ["ATTRIBUTE",name,attr] =>
    v := compForMode(name,$EmptyMode,env) or return
          stackAndThrow('"can't find category of %1pb",[name])
    [vv,.,.] := compMakeCategoryObject(v.mode,env) or return
                 stackAndThrow('"can't make category of %1pb",[name])
    listMember?(attr,categoryAttributes vv) => true
    x := assoc(attr,categoryAttributes vv) => knownInfo(second x,env)
          --format is a list of two elements: information, predicate
    false
  pred is ["has",name,cat] =>
    cat is ["ATTRIBUTE",:a] => knownInfo(["ATTRIBUTE",name,:a],env)
    cat is ["SIGNATURE",:a] => knownInfo(["SIGNATURE",name,:a],env)
    -- unnamed category expressions imply structural checks.
    cat is ["Join",:.] => and/[knownInfo(["has",name,c],env) for c in cat.args]
    cat is ["CATEGORY",.,:atts] =>
      and/[knownInfo(hasToInfo ["has",name,att],env) for att in atts]
    name is ['Union,:.] => false
    -- we have a named category expression
    v:= compForMode(name,$EmptyMode,env) or return
          stackAndThrow('"can't find category of %1pb",[name])
    vmode := v.mode
    cat = vmode => true
    vmode is ["Join",:l] and listMember?(cat,l) => true
    [vv,.,.]:= compMakeCategoryObject(vmode,env) or return
                 stackAndThrow('"cannot find category %1pb",[vmode])
    listMember?(cat,categoryPrincipals vv) => true  --checks princ. ancestors
    (u:=assoc(cat,categoryAncestors vv)) and knownInfo(second u,env) => true
    -- previous line checks fundamental anscestors, we should check their
    --   principal anscestors but this requires instantiating categories

    or/[AncestorP(cat,[first u],env) 
         for u in categoryAncestors vv | knownInfo(second u,env)] => true
    false
  pred is ["SIGNATURE",name,op,sig,:.] =>
    v:= get(op,"modemap",env)
    for w in v repeat
      ww := w.mmSignature  --the actual signature part
      ww = sig =>
        w.mmCondition  = true => return true
        false
        --error '"knownInfo"
  false
 
mkJoin(cat,mode) ==
  mode is ['Join,:cats] => ['Join,cat,:cats]
  ['Join,cat,mode]
 

GetValue name ==
  u:= get(name,"value",$e) => u
  u:= comp(name,$EmptyMode,$e) => u  --name may be a form
  systemError [name,'" is not bound in the current environment"]
 
actOnInfo(u,$e) ==
  null u => $e
  u is ["PROGN",:l] => (for v in l repeat $e:= actOnInfo(v,$e); $e)
  $e:=
    put("$Information","special",Info:= [u,:get("$Information","special",$e)],$e
      )
  u is ['%when,:l] =>
      --there is nowhere %else that this sort of thing exists
    for [ante,:conseq] in l repeat
      if listMember?(hasToInfo ante,Info) then for v in conseq repeat
        $e:= actOnInfo(v,$e)
    $e
  u is ["ATTRIBUTE",name,att] =>
    [vval,vmode,.]:= GetValue name
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
    [vval,vmode,.]:= GetValue name
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
    [vval,vmode,.]:= GetValue name
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
        then $e:= augModemapsFromCategory(name,name,cat,$e)
        else
          genDomainView(name,cat,"HasCategory")
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
      $e:= actOnInfo(pred,$e)
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
getInfovecCode() == 
--Function called by compDefineFunctor1 to create infovec at compile time
  ['LIST,
    MKQ makeDomainTemplate $template,
      MKQ makeCompactDirect $NRTslot1Info,
        MKQ NRTgenFinalAttributeAlist $e,
          NRTmakeCategoryAlist $e,
            MKQ $lookupFunction]

--=======================================================================
--         Generation of Domain Vector Template (Compile Time)
--=======================================================================
makeDomainTemplate vec ==   
--NOTES: This function is called at compile time to create the template
--  (slot 0 of the infovec); called by getInfovecCode from compDefineFunctor1
  newVec := newShell # vec
  for index in 0..maxIndex vec repeat
    item := vectorRef(vec,index)
    null item => nil
    vectorRef(newVec,index) :=
      item isnt [.,:.] => item
      cons? first item => makeGoGetSlot(item,index)
      item   
  $byteVec := "append"/reverse! $byteVec
  newVec
 
makeGoGetSlot(item,index) ==
--NOTES: creates byte vec strings for LATCH slots
--these parts of the $byteVec are created first; see also makeCompactDirect
  [sig,whereToGo,op,:flag] := item
  n := #sig - 1
  newcode := [n,whereToGo,:makeCompactSigCode sig,index]
  $byteVec := [newcode,:$byteVec]
  curAddress := $byteAddress
  $byteAddress := $byteAddress + n + 4
  [curAddress,:op]
 
--=======================================================================
--                Generate OpTable at Compile Time
--=======================================================================
--> called by getInfovecCode (see top of this file) from compDefineFunctor1
makeCompactDirect u ==
  $predListLength :local := # $NRTslot1PredicateList
  $byteVecAcc: local := nil
  [nam,[addForm,:opList]] := u
  --pp opList 
  d := [[op,y] for [op,:items] in opList | y := makeCompactDirect1(op,items)]
  $byteVec := [:$byteVec,:"append"/reverse! $byteVecAcc]
  vector("append"/d)
 
makeCompactDirect1(op,items) ==
--NOTES: creates byte codes for ops implemented by the domain
    curAddress := $byteAddress
    $op: local := op  --temp hack by RDJ 8/90 (see orderBySubsumption)
    newcodes :=
      "append"/[u for y in orderBySubsumption items | u := fn y] or return nil
    $byteVecAcc := [newcodes,:$byteVecAcc]
    curAddress
 where fn y ==
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
      s is [pred,:.] => predicateBitIndex(pred,$e)
      0
  --> drop items which are not present (predCode = -1)
  predCode = -1 => return nil
  --> drop items with nil slots if lookup function is incomplete
  if null slot then
     $lookupFunction is 'lookupIncomplete => return nil
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
 
NRTmakeCategoryAlist e ==
  $depthAssocCache: local := hashTable 'EQ
  $catAncestorAlist: local := nil
  pcAlist := [:[[x,:"T"] for x in $uncondAlist],:$condAlist]
  $levelAlist: local := depthAssocList [CAAR x for x in pcAlist]
  opcAlist := reverse! SORTBY(function NRTcatCompare,pcAlist)
  newPairlis := [[5 + i,:b] for [.,:b] in $pairlis for i in 1..]
  slot1 := [[a,:k] for [a,:b] in applySubst($pairlis,opcAlist)
                   | (k := predicateBitIndex(b,e)) ~= -1]
  slot0 := [hasDefaultPackage opOf a for [a,:b] in slot1]
  sixEtc := [5 + i for i in 1..#$pairlis]
  formals := ASSOCRIGHT $pairlis
  for x in slot1 repeat
    x.first := applySubst(pairList(['$,:formals],["$$",:sixEtc]),first x)
  -----------code to make a new style slot4 -----------------
  predList := ASSOCRIGHT slot1  --is list of predicate indices
  maxPredList := "MAX"/predList
  catformvec := ASSOCLEFT slot1
  maxElement := "MAX"/$byteVec
  ['CONS, ['makeByteWordVec2,MAX(maxPredList,1),MKQ predList],
    ['CONS, MKQ vector slot0,
      ['CONS, MKQ vector [encodeCatform x for x in catformvec],
        ['makeByteWordVec2,maxElement,MKQ $byteVec]]]]
  --NOTE: this is new form: old form satisfies vector? CDDR form

encodeCatform x ==
  k := NRTassocIndex x => k
  x isnt [.,:.] or rest x isnt [.,:.] => x
  [first x,:[encodeCatform y for y in rest x]]
 
NRTcatCompare [catform,:pred] == LASSOC(first catform,$levelAlist)
 
hasDefaultPackage catname ==
  defname := makeDefaultPackageName symbolName catname
  constructor? defname => defname
  nil
 
 
--=======================================================================
--              Compute the lookup function (complete or incomplete)
--=======================================================================
NRTgetLookupFunction(domform,exCategory,addForm,env) ==
  $why: local := nil
  domform := applySubst($pairlis,domform)
  addForm isnt [.,:.] =>
    IDENTP addForm and (m := getmode(addForm,env)) ~= nil
      and isCategoryForm(m,env) 
        and extendsCategory(domform,exCategory,applySubst($pairlis,m),env) =>
          'lookupIncomplete
    'lookupComplete
  addForm := applySubst($pairlis,addForm)
  NRTextendsCategory1(domform,exCategory,getExportCategory addForm,env) =>
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

getExportCategory form ==
  [op,:argl] := form
  op is 'Record => ['RecordCategory,:argl]
  op is 'Union => ['UnionCategory,:argl]
  op is 'Enumeration => ['EnumerationCategory,:argl]
  op is 'Mapping => ['MappingCategory,:argl]
  [[.,target,:tl],:.] := getConstructorModemap op
  applySubst(pairList($FormalMapVariableList,argl),target)
 
NRTextendsCategory1(domform,exCategory,addForm,env) ==
  addForm is ["%Comma",:r] => 
    and/[extendsCategory(domform,exCategory,x,env) for x in r]
  extendsCategory(domform,exCategory,addForm,env)

--=======================================================================
--         Compute if a domain constructor is forgetful functor
--=======================================================================
extendsCategory(dom,u,v,env) ==
  --does category u extend category v (yes iff u contains everything in v)
  --is dom of category u also of category v?
  u=v => true
  v is ["Join",:l] => and/[extendsCategory(dom,u,x,env) for x in l]
  v is ["CATEGORY",.,:l] => and/[extendsCategory(dom,u,x,env) for x in l]
  v is ["SubsetCategory",cat,d] =>
    extendsCategory(dom,u,cat,env) and isSubset(dom,d,env)
  v := substSlotNumbers(v,$template,$functorForm)
  extendsCategoryBasic(dom,u,v,env) => true
  $why :=
    v is ['SIGNATURE,op,sig] => [u,['"  has no ",:formatOpSignature(op,sig)]]
    [u,'" has no",v]
  nil
 
extendsCategoryBasic(dom,u,v,env) ==
  v is ['IF,p,['ATTRIBUTE,c],.] =>
    uVec := compMakeCategoryObject(u,env).expr or return false
    cons? c and isCategoryForm(c,env) =>
      LASSOC(c,categoryAncestors uVec) is [=p,:.]
    LASSOC(c,categoryAttributes uVec) is [=p,:.]
  u is ["Join",:l] => or/[extendsCategoryBasic(dom,x,v,env) for x in l]
  u = v => true
  v is ['ATTRIBUTE,c] =>
    cons? c and isCategoryForm(c,env) => extendsCategoryBasic(dom,u,c,env)
    u is ['CATEGORY,.,:l] => or/[extendsCategoryBasic(dom,x,v,env) for x in l]
    uVec := compMakeCategoryObject(u,env).expr or return false
    LASSOC(c,categoryAttributes uVec) is [=true]
  isCategoryForm(v,env) => catExtendsCat?(u,v,env)
  v is ['SIGNATURE,op,sig] =>
    uVec := compMakeCategoryObject(u,env).expr or return false
    or/[categoryRef(uVec,i) is [[=op,=sig],:.] for i in 6..maxIndex uVec]
  u is ['CATEGORY,.,:l] =>
    v is ['IF,:.] => listMember?(v,l)
    false
  false
 
catExtendsCat?(u,v,env) ==
  u = v => true
  uvec := compMakeCategoryObject(u,env).expr
  prinAncestorList := categoryPrincipals uvec
  listMember?(v,prinAncestorList) => true
  vOp := KAR v
  if similarForm := assoc(vOp,prinAncestorList) then
    PRINT u
    sayBrightlyNT '"   extends "
    PRINT similarForm
    sayBrightlyNT '"   but not "
    PRINT v
  or/[catExtendsCat?(x,v,env) for x in ASSOCLEFT categoryAncestors uvec]
 
substSlotNumbers(form,template,domain) ==
  form is [op,:.] and
    symbolMember?(op,allConstructors()) => expandType(form,template,domain)
  form is ['SIGNATURE,op,sig] =>
    ['SIGNATURE,op,[substSlotNumbers(x,template,domain) for x in sig]]
  form is ['CATEGORY,k,:u] =>
    ['CATEGORY,k,:[substSlotNumbers(x,template,domain) for x in u]]
  expandType(form,template,domain)
 
expandType(lazyt,template,domform) ==
  lazyt isnt [.,:.] => expandTypeArgs(lazyt,template,domform)
  [functorName,:argl] := lazyt
  functorName is ":" =>
    [functorName,first argl,expandTypeArgs(second argl,template,domform)]
  lazyt is ['local,x] =>
    n := POSN1(x,$FormalMapVariableList)
    domform.(1 + n)
  [functorName,:[expandTypeArgs(a,template,domform) for a in argl]]
 
expandTypeArgs(u,template,domform) ==
  u is '$ => u
  integer? u => expandType(vectorRef(template,u),template,domform)
  u is [.,y] and u.op in '(NRTEVAL QUOTE) => y
  u isnt [.,:.] => u
  expandType(u,template,domform)

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

noteExport(form,pred) ==
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

compDefine(form,m,e) ==
  $macroIfTrue: local := false
  compDefine1(form,m,e)

++ We are about to process the body of a capsule.  Check the form of
++ `Rep' definition, and whether it is appropriate to activate the
++ implicitly generated morphisms
++     per: Rep -> %
++     rep: % -> Rep
++ as local inline functions.
checkRepresentation: (%Form,%List %Form,%Env) -> %Env
checkRepresentation(addForm,body,env) ==
  domainRep := nil
  hasAssignRep := false        -- assume code does not assign to Rep.
  viewFuns := nil

  null body => env             -- Don't be too hard on nothing.
  
  -- Locate possible Rep definition
  for [stmt,:.] in tails body repeat
    stmt is ["%LET","Rep",val] =>
      domainRep ~= nil =>
        stackAndThrow('"You cannot assign to constant domain %1b",["Rep"])
      if addForm = val then
        stackWarning('"OpenAxiom suggests removing assignment to %1b",["Rep"])
      else if addForm ~= nil then
        stackWarning('"%1b differs from the base domain",["Rep"])
      return hasAssignRep := true
    stmt is ["MDEF",["Rep",:.],:.] =>
      stackWarning('"Consider using == definition for %1b",["Rep"])
      return hasAssignRep := true
    stmt is ["IF",.,:l] or stmt is ["SEQ",:l] or stmt is ["exit",:l] =>
      checkRepresentation(nil,l,env)
    stmt isnt ["DEF",[op,:args],sig,.,val] => nil -- skip for now.
    op in '(rep per) =>
      domainRep ~= nil =>
        stackAndThrow('"You cannot define implicitly generated %1b",[op])
      viewFuns := [op,:viewFuns]
    op ~= "Rep" => nil        -- we are only interested in Rep definition
    domainRep := val
    viewFuns ~= nil =>
      stackAndThrow('"You cannot define both %1b and %2b",["Rep",:viewFuns])
    -- A package has no "%".
    $functorKind = "package" =>
      stackAndThrow('"You cannot define %1b in a package",["Rep"])
    -- It is a mistake to define Rep in category defaults
    $insideCategoryPackageIfTrue =>
      stackAndThrow('"You cannot define %1b in category defaults",["Rep"])
    if args ~= nil then
      stackAndThrow('"%1b does take arguments",["Rep"])
    if sig.target ~= nil then
      stackAndThrow('"You cannot specify type for %1b",["Rep"])
    -- Now, trick the rest of the compiler into believing that
    -- `Rep' was defined the Old Way, for lookup purpose.
    stmt.op := "%LET"
    stmt.rest := ["Rep",domainRep]
    $useRepresentationHack := false          -- Don't confuse `Rep' and `%'.

  -- Shall we perform the dirty tricks?
  if hasAssignRep then
    $useRepresentationHack := true
  -- Domain extensions with no explicit Rep definition have the
  -- the base domain as representation (at least operationally).
  else if null domainRep and addForm ~= nil then
    if $functorKind = "domain" and addForm isnt ["%Comma",:.] then
      domainRep :=
        addForm is ["SubDomain",dom,.] => 
          $subdomain := true
          dom
        addForm
      $useRepresentationHack := false
      env := putMacro('Rep,domainRep,env)
  env


compDefine1: (%Form,%Mode,%Env) -> %Maybe %Triple 
compDefine1(form,m,e) ==
  $insideExpressionIfTrue: local:= false
  --1. decompose after macro-expanding form
  ['DEF,lhs,signature,specialCases,rhs]:= form:= macroExpand(form,e)
  $insideWhereIfTrue and isMacro(form,e) and (m=$EmptyMode or m=$NoValueMode)
     => [lhs,m,putMacro(lhs.op,rhs,e)]
  checkParameterNames lhs.args
  null signature.target and symbol? KAR rhs and not builtinConstructor? KAR rhs and
    (sig:= getSignatureFromMode(lhs,e)) =>
  -- here signature of lhs is determined by a previous declaration
      compDefine1(['DEF,lhs,[sig.target,:signature.source],specialCases,rhs],m,e)
  if signature.target=$Category then $insideCategoryIfTrue:= true
 
-- RDJ (11/83): when argument and return types are all declared,
--  or arguments have types declared in the environment,
--  and there is no existing modemap for this signature, add
--  the modemap by a declaration, then strip off declarations and recurse
  e := compDefineAddSignature(lhs,signature,e)
-- 2. if signature list for arguments is not empty, replace ('DEF,..) by
--       ('where,('DEF,..),..) with an empty signature list;
--     otherwise, fill in all NILs in the signature
  or/[x ~= nil for x in signature.source] => compDefWhereClause(form,m,e)
  signature.target=$Category =>
    compDefineCategory(form,m,e,nil,$formalArgList)
  isDomainForm(rhs,e) and not $insideFunctorIfTrue =>
    if null signature.target then signature:=
      [getTargetFromRhs(lhs,rhs,giveFormalParametersValues(lhs.args,e)),:
          signature.source]
    rhs:= addEmptyCapsuleIfNecessary(signature.target,rhs)
    compDefineFunctor(['DEF,lhs,signature,specialCases,rhs],m,e,nil,
      $formalArgList)
  null $form => stackAndThrow ['"bad == form ",form]
  newPrefix:=
    $prefix => makeSymbol strconc(encodeItem $prefix,'",",encodeItem $op)
    dbAbbreviation constructorDB $op
  compDefineCapsuleFunction(form,m,e,newPrefix,$formalArgList)

compDefineAddSignature([op,:argl],signature,e) ==
  (sig:= hasFullSignature(argl,signature,e)) and
   null assoc(['$,:sig],symbolLassoc('modemap,getProplist(op,e))) =>
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
  (compOrCroak(rhs,$EmptyMode,e)).mode
 
giveFormalParametersValues(argl,e) ==
  for x in argl | ident? x repeat
    e := giveVariableSomeValue(x,get(x,'mode,e),e)
  e


macroExpandInPlace: (%Form,%Env) -> %Form 
macroExpandInPlace(x,e) ==
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
  x is ['DEF,lhs,sig,spCases,rhs] =>
    ['DEF,macroExpand(lhs,e),macroExpandList(sig,e),macroExpandList(spCases,e),
      macroExpand(rhs,e)]
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
    op="Join" => ["Join",:[mkEvalableCategoryForm x for x in argl]]
    op is "DomainSubstitutionMacro" => mkEvalableCategoryForm second argl
    op is "mkCategory" => c
    builtinCategoryName? op =>
      ([x,m,$e]:= compOrCroak(c,$EmptyMode,$e); m=$Category => x)
    --loadIfNecessary op
    getConstructorKindFromDB op = 'category or
      get(op,"isCategory",$CategoryFrame) =>
        [op,:[MKQ x for x in argl]]
    [x,m,$e]:= compOrCroak(c,$EmptyMode,$e)
    m=$Category => x
  MKQ c
 
++ Return true if we should skip compilation of category package.
++ This situation happens either when there is no default, of we are in
++ bootstrap mode.
skipCategoryPackage? capsule ==
  null capsule or $bootStrapMode

compDefineCategory1(df is ['DEF,form,sig,sc,body],m,e,prefix,fal) ==
  categoryCapsule :=
    body is ['add,cat,capsule] =>
      body := cat
      capsule
    nil
  [d,m,e]:= compDefineCategory2(form,sig,sc,body,m,e,prefix,fal)
  if not skipCategoryPackage? categoryCapsule then [.,.,e] :=
    $insideCategoryPackageIfTrue: local := true
    $categoryPredicateList: local :=
        makeCategoryPredicates(form,$lisplibCategory)
    T := compDefine1(mkCategoryPackage(form,cat,categoryCapsule),$EmptyMode,e)
           or return stackSemanticError(
                        ['"cannot compile defaults of",:bright opOf form],nil)
    if $compileDefaultsOnly then 
      [d,m,e] := T
  [d,m,e]

makeCategoryPredicates(form,u) ==
      $tvl: local := TAKE(#rest form,$TriangleVariableList)
      $mvl: local := TAKE(#rest form,rest $FormalMapVariableList)
      fn(u,nil) where
        fn(u,pl) ==
          u is ['Join,:.,a] => fn(a,pl)
          u is ["IF",p,:x] =>
            fnl(x,insert(applySubst(pairList($tvl,$mvl),p),pl))
          u is ["has",:.] =>
            insert(applySubst(pairList($tvl,$mvl),u),pl)
          u is [op,:.] and op in '(SIGNATURE ATTRIBUTE) => pl
          u isnt [.,:.] => pl
          fnl(u,pl)
        fnl(u,pl) ==
          for x in u repeat pl := fn(x,pl)
          pl
 
mkCategoryPackage(form is [op,:argl],cat,def) ==
  packageName:= makeDefaultPackageName symbolName op
  packageAbb := makeSymbol(strconc(getConstructorAbbreviationFromDB op,'"-"))
  $options:local := []
  -- This stops the next line from becoming confused
  abbreviationsSpad2Cmd ['domain,packageAbb,packageName]
  -- This is a little odd, but the parser insists on calling
  -- domains, rather than packages
  nameForDollar := first SETDIFFERENCE('(S A B C D E F G H I),argl)
  packageArgl := [nameForDollar,:argl]
  capsuleDefAlist := fn(def,nil) where fn(x,oplist) ==
    x isnt [.,:.] => oplist
    x is ['DEF,y,:.] => [y,:oplist]
    fn(x.args,fn(x.op,oplist))
  catvec := eval mkEvalableCategoryForm form
  fullCatOpList := categoryExports JoinInner([catvec],$e)
  catOpList :=
    [['SIGNATURE,op1,sig] for [[op1,sig],:.] in fullCatOpList
        | assoc(op1,capsuleDefAlist)]
  null catOpList => nil
  packageCategory :=
    ['CATEGORY,'domain,
       :applySubst(pairList($FormalMapVariableList,argl),catOpList)]
  nils:= [nil for x in argl]
  packageSig := [packageCategory,form,:nils]
  $categoryPredicateList := substitute(nameForDollar,'$,$categoryPredicateList)
  substitute(nameForDollar,'$,
      ['DEF,[packageName,:packageArgl],packageSig,[nil,:nils],def])
 
compDefineCategory2(form,signature,specialCases,body,m,e,
  $prefix,$formalArgList) ==
    --1. bind global variables
    $insideCategoryIfTrue: local := true
    $definition: local := form   --used by DomainSubstitutionFunction
    $form: local := nil
    $op: local := nil
    $extraParms: local := nil
    -- Remember the body for checking the current instantiation.
    $currentCategoryBody : local := body
         --Set in DomainSubstitutionFunction, used further down
    -- 1.1  augment e to add declaration $: <form>
    [$op,:argl] := $definition
    db := constructorDB $op
    dbBeingDefined?(db) := true
    dbInstanceCache(db) := true
    e:= addBinding("$",[['mode,:$definition]],e)
 
    -- 2. obtain signature
    signature':=
      [signature.target,
        :[getArgumentModeOrMoan(a,$definition,e) for a in argl]]
    e:= giveFormalParametersValues(argl,e)
 
    -- 3. replace arguments by $1,..., substitute into body,
    --    and introduce declarations into environment
    sargl:= TAKE(# argl, $TriangleVariableList)
    $functorForm:= $form:= [$op,:sargl]
    $formalArgList:= [:sargl,:$formalArgList]
    aList := pairList(argl,sargl)
    formalBody:= applySubst(aList,body)
    signature' := applySubst(aList,signature')
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
    body := optFunctorBody compOrCroak(formalBody,signature'.target,e).expr
    if $extraParms ~= nil then
      formals := nil
      actuals := nil
      for [u,:v] in $extraParms repeat
        formals := [u,:formals]
        actuals := [MKQ v,:actuals]
      body := ['sublisV,['pairList,['QUOTE,formals],['%list,:actuals]],body]
    if argl then body:=  -- always subst for args after extraparms
        ['sublisV,['pairList,['QUOTE,sargl],['%list,:
          [['devaluate,u] for u in sargl]]],body]
    body:=
      ["%bind",[[g:= gensym(),body]],
         ['%store,['%tref,g,0],mkConstructor $form],g]
    fun:= compile [op',["LAM",sargl,body]]
 
    -- 5. give operator a 'modemap property
    pairlis := pairList(argl,$FormalMapVariableList)
    parSignature:= applySubst(pairlis,signature')
    parForm:= applySubst(pairlis,form)
 
    -- 6. put modemaps into InteractiveModemapFrame
    $domainShell := eval [op',:[MKQ f for f in sargl]]
    dbConstructorModemap(constructorDB op') := [[parForm,:parSignature],[true,op']]
    $lisplibCategory:= formalBody
    dbPrincipals(db) := getParentsFor(db,$FormalMapVariableList,$lisplibCategory)
    dbAncestors(db) := computeAncestorsOf($form,nil)
    if $LISPLIB then
      augLisplibModemapsFromCategory([op',:sargl],formalBody,signature')
    dbBeingDefined?(db) := false
    [fun,$Category,e]

mkConstructor: %Form -> %Form
mkConstructor form ==
  form isnt [.,:.] => ['devaluate,form]
  null form.args => ['QUOTE,[form.op]]
  ['%list,MKQ form.op,:[mkConstructor x for x in form.args]]
 
compDefineCategory(df,m,e,prefix,fal) ==
  $domainShell: local := nil -- holds the category of the object being compiled
  $lisplibCategory: local := nil
  -- since we have so many ways to say state the kind of a constructor,
  -- make sure we do have some minimal internal coherence.
  lhs := second df
  ctor := opOf lhs
  kind := getConstructorKindFromDB ctor
  kind ~= "category" => throwKeyedMsg("S2IC0016",[ctor,"category",kind])
  dbConstructorForm(constructorDB ctor) := lhs
  $insideFunctorIfTrue or $LISPLIB = nil or $compileDefaultsOnly =>
    compDefineCategory1(df,m,e,prefix,fal)
  compDefineLisplib(df,m,e,prefix,fal,'compDefineCategory1)


%CatObjRes                   -- result of compiling a category
  <=> [%Shell,:[%Mode,:[%Env,:null]]]
 
compMakeCategoryObject: (%Form,%Env) -> %Maybe %CatObjRes
compMakeCategoryObject(c,$e) ==
  not isCategoryForm(c,$e) => nil
  u:= mkEvalableCategoryForm c => [eval u,$Category,$e]
  nil

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
    mkNewModemapList(mc,sig,pred,fn,symbolLassoc('modemap,currentProplist),e,nil)
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

getOperationAlist(name,functorForm,form) ==
  if ident? name and niladicConstructor? name then 
    functorForm := [functorForm]
  (u:= isFunctor functorForm) and not
    ($insideFunctorIfTrue and first functorForm=first $functorForm) => u
  $insideFunctorIfTrue and name is "$" =>
    $domainShell => categoryExports $domainShell
    systemError '"$ has no shell now"
  T:= compMakeCategoryObject(form,$e) => ([.,.,$e]:= T; categoryExports T.expr)
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
 
evalAndSub(domainName,functorForm,form,$e) ==
  $lhsOfColon: local:= domainName
  categoryObject? form =>
    [substNames(domainName,functorForm,categoryExports form),$e]
  --next lines necessary-- see MPOLY for which $ is actual arg. --- RDJ 3/83
  if CONTAINED("$$",form) then $e:= put("$$","mode",get("$","mode",$e),$e)
  opAlist:= getOperationAlist(domainName,functorForm,form)
  substAlist:= substNames(domainName,functorForm,opAlist)
  [substAlist,$e]
 
augModemapsFromCategory(domainName,functorForm,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(domainName,functorForm,categoryForm,e)
  compilerMessage('"Adding %1p modemaps",[domainName])
  e:= putDomainsInScope(domainName,e)
  condlist:=[]
  for [[op,sig,:.],cond,fnsel] in fnAlist repeat
    e:= addModemapKnown(op,domainName,sig,cond,fnsel,e) -- cond was cond1
  e
 
addConstructorModemaps(name,form is [functorName,:.],e) ==
  $InteractiveMode: local:= nil
  e:= putDomainsInScope(name,e) --frame
  fn := property(functorName,"makeFunctionList")
  [funList,e]:= FUNCALL(fn,name,form,e)
  for [op,sig,opcode] in funList repeat
    if opcode is [sel,dc,n] and sel='ELT then
          nsig := substitute("$$$",name,sig)
          nsig := substitute('$,"$$$",substitute("$$",'$,nsig))
          opcode := [sel,dc,nsig]
    e:= addModemap(op,name,sig,true,opcode,e)
  e
 
augModemapsFromDomain1(name,functorForm,e) ==
  property(KAR functorForm,"makeFunctionList") =>
    addConstructorModemaps(name,functorForm,e)
  functorForm isnt [.,:.] and (catform := getmode(functorForm,e)) =>
    augModemapsFromCategory(name,functorForm,catform,e)
  mappingForm := getmodeOrMapping(KAR functorForm,e) =>
    ["Mapping",categoryForm,:functArgTypes] := mappingForm
    catform := substituteCategoryArguments(rest functorForm,categoryForm)
    augModemapsFromCategory(name,functorForm,catform,e)
  stackMessage('"%1pb is an unknown mode",[functorForm])
  e
 
AMFCR_,redefinedList(op,l) == "OR"/[AMFCR_,redefined(op,u) for u in l]
 
AMFCR_,redefined(opname,u) ==
  not(u is [op,:l]) => nil
  op = 'DEF => opname = CAAR l
  op in '(PROGN SEQ) => AMFCR_,redefinedList(opname,l)
  op = '%when => "OR"/[AMFCR_,redefinedList(opname,rest u) for u in l]

substituteCategoryArguments(argl,catform) ==
  argl := substitute("$$","$",argl)
  arglAssoc := [[INTERNL("#",STRINGIMAGE i),:a] for i in 1.. for a in argl]
  applySubst(arglAssoc,catform)
 
++ Subroutine of inferConstructorImplicitParameters.
typeDependencyPath(m,path,e) ==
  ident? m and assoc(m,$whereDecls) =>
    get(m,'value,e) => nil  -- parameter was given value
    [[m,:reverse path],:typeDependencyPath(getmode(m,e),path,e)]
  atomic? m => nil
  [ctor,:args] := m
  -- We don't expect implicit parameters in builtin constructors.
  builtinConstructor? ctor => nil
  -- FIXME: assume constructors cannot be parameters
  not constructor? ctor => nil
  [:typeDependencyPath(m',[i,:path],e) for m' in args for i in 0..]

++ Given the list `parms' of explicit constructor parameters, compute
++ a list of pairs `(p . path)' where `p' is a parameter implicitly
++ introduced (either directly or indirectly) by a declaration of
++ one of the explicit parameters.
inferConstructorImplicitParameters(parms,e) ==
  removeDuplicates
    [:typeDependencyPath(getmode(p,e),[i],e) for p in parms for i in 0..]
 
compDefineFunctor(df,m,e,prefix,fal) ==
  $domainShell: local := nil -- holds the category of the object being compiled
  $profileCompiler: local := true
  $profileAlist:    local := nil
  $LISPLIB = nil => compDefineFunctor1(df,m,e,prefix,fal)
  compDefineLisplib(df,m,e,prefix,fal,'compDefineFunctor1)
 
compDefineFunctor1(df is ['DEF,form,signature,nils,body],
  m,$e,$prefix,$formalArgList) ==
    --  1. bind global variables
    $addForm: local := nil
    $subdomain: local := false
    $functionStats: local:= [0,0]
    $functorStats: local:= [0,0]
    $form: local := nil
    $op: local := nil
    $signature: local := nil
    $functorTarget: local := nil
    $Representation: local := nil
         --Set in doIt, accessed in the compiler - compNoStacking
    $functorForm: local := nil
    $functorLocalParameters: local := nil
    $CheckVectorList: local := nil
    $getDomainCode: local := nil -- code for getting views
    $insideFunctorIfTrue: local:= true
    $genSDVar: local:= 0
    originale:= $e
    [$op,:argl]:= form
    db := constructorDB $op
    dbBeingDefined?(db) := true
    dbConstructorForm(db) := form
    $formalArgList:= [:argl,:$formalArgList]
    $pairlis: local := pairList(argl,$FormalMapVariableList)
    -- all defaulting packages should have caching turned off
    dbInstanceCache(db) := not isCategoryPackageName $op
    signature':=
      [signature.target,:[getArgumentModeOrMoan(a,form,$e) for a in argl]]
    $functorForm := $form := [$op,:argl]
    if null signature'.target then signature':=
      modemap2Signature getModemap($form,$e)
    $functorTarget := target := signature'.target
    $functorKind: local :=
      $functorTarget is ["CATEGORY",key,:.] => key
      "domain"
    $e := giveFormalParametersValues(argl,$e)
    $implicitParameters: local := inferConstructorImplicitParameters(argl,$e)
    [ds,.,$e]:= compMakeCategoryObject(target,$e) or return
       stackAndThrow('"   cannot produce category object: %1pb",[target])
    $domainShell: local := copyVector ds
    attributeList := categoryAttributes ds --see below under "loadTimeAlist"
    $condAlist: local := nil
    $uncondAlist: local := nil
    $NRTslot1PredicateList: local := predicatesFromAttributes attributeList
    $NRTattributeAlist: local := NRTgenInitialAttributeAlist(db,attributeList)
    $NRTslot1Info: local := nil  --set in NRTmakeSlot1Info
    $NRTaddForm: local := nil   -- see compAdd
    $NRTdeltaList: local := nil --list of misc. elts used in compiled fncts
    $NRTdeltaListComp: local := nil --list of compiled forms for $NRTdeltaList
    $NRTdeltaLength: local := 0 -- =length of block of extra entries in vector
    $template: local:= nil --stored in the lisplib
    $functionLocations: local := nil --locations of defined functions in source
    -- generate slots for arguments first, then for $NRTaddForm in compAdd
    for x in argl repeat NRTgetLocalIndex x
    [.,.,$e]:= compMakeDeclaration("$",target,$e)
    if not $insideCategoryPackageIfTrue  then
      $e:= augModemapsFromCategory('_$,'_$,target,$e)
    $signature:= signature'
    parSignature:= applySubst($pairlis,signature')
    parForm:= applySubst($pairlis,form)
 
    --  (3.1) now make a list of the functor's local parameters; for
    --  domain D in argl,check its signature: if domain, its type is Join(A1,..,An);
    --  in this case, D is replaced by D1,..,Dn (gensyms) which are set
    --  to the A1,..,An view of D
    makeFunctorArgumentParameters(argl,signature'.source,signature'.target)
    $functorLocalParameters := argl

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
    T:= compFunctorBody(body,rettype,$e,parForm)
    -- If only compiling certain items, then ignore the body shell.
    $compileOnlyCertainItems =>
       reportOnFunctorCompilation()
       [nil, ['Mapping, :signature'], originale]
 
    body':= T.expr
    lamOrSlam :=
      dbInstanceCache db = nil => 'LAM
      'SPADSLAM
    fun:= compile applySubst($pairlis, [op',[lamOrSlam,argl,body']])
    --The above statement stops substitutions gettting in one another's way
    operationAlist := applySubst($pairlis,$lisplibOperationAlist)
    if $LISPLIB then
      augmentLisplibModemapsFromFunctor(parForm,operationAlist,parSignature)
    reportOnFunctorCompilation()
 
    --  5. give operator a 'modemap property
    modemap := [[parForm,:parSignature],[true,op']]
    dbConstructorModemap(constructorDB op') := modemap
    $lisplibCategory := modemap.mmTarget
    dbPrincipals(db) := getParentsFor(db,$FormalMapVariableList,$lisplibCategory)
    dbAncestors(db) := computeAncestorsOf($form,nil)
    $insideFunctorIfTrue:= false
    if $LISPLIB then
      if not $bootStrapMode then
        $NRTslot1Info := NRTmakeSlot1Info()
        libFn := getConstructorAbbreviationFromDB op'
        $lookupFunction: local :=
            NRTgetLookupFunction($functorForm,modemap.mmTarget,$NRTaddForm,$e)
            --either lookupComplete (for forgetful guys) or lookupIncomplete
        $byteAddress :local := 0
        $byteVec :local := nil
        $NRTslot1PredicateList :=
          [simpBool x for x in $NRTslot1PredicateList]
        LAM_,FILEACTQ('loadTimeStuff,
          ['MAKEPROP,MKQ $op,''infovec,getInfovecCode()])
      $lisplibOperationAlist:= operationAlist
    -- Functors are incomplete during bootstrap
    if $bootStrapMode then
      evalAndRwriteLispForm('%incomplete,
            ['MAKEPROP, ['QUOTE,op'], ['QUOTE,'%incomplete], true])
    dbBeingDefined?(db) := false
    [fun,['Mapping,:signature'],originale]


++ Finish the incomplete compilation of a functor body.
incompleteFunctorBody(form,m,body,e) ==
  -- The slot numbers from the category shell are bogus at this point.
  -- Nullify them so people don't think they bear any meaningful
  -- semantics (well, they should not think these are forwarding either).
  ops := nil
  for [opsig,pred,funsel] in categoryExports $domainShell repeat
    if pred isnt 'T then
      pred := simpBool pred
    if funsel is [op,.,.] and op in '(ELT CONST) then
      third(funsel) := nil
    ops := [[opsig,pred,funsel],:ops]
  $lisplibOperationAlist := listSort(function GGREATERP,ops,function first)
  dbSuperDomain(constructorDB form.op) :=
    body is ['SubDomain,dom,pred] => [dom,pred]
    body is ['add,['SubDomain,dom,pred],:.] => [dom,pred]
    nil
  [bootStrapError(form, _/EDITFILE),m,e]

++ Subroutine of compDefineFunctor1.  Called to generate backend code
++ for a functor definition. 
compFunctorBody(body,m,e,parForm) ==
  $bootStrapMode => incompleteFunctorBody($functorForm,m,body,e)
  clearCapsuleDirectory()        -- start collecting capsule functions.
  T:= compOrCroak(body,m,e)
  $capsuleFunctionStack := reverse! $capsuleFunctionStack
  -- ??? Don't resolve default definitions, yet.
  backendCompile
    $insideCategoryPackageIfTrue => $capsuleFunctionStack
    foldExportedFunctionReferences $capsuleFunctionStack
  clearCapsuleDirectory()        -- release storage.
  body is [op,:.] and op in '(add CAPSULE) => T
  $NRTaddForm :=
    body is ["SubDomain",domainForm,predicate] => domainForm
    body
  T
 
reportOnFunctorCompilation() ==
  displayMissingFunctions()
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
 
displayMissingFunctions() ==
  null $CheckVectorList => nil
  loc := nil              -- list of local operation signatures
  exp := nil              -- list of exported operation signatures
  for [[op,sig,:.],:pred] in $CheckVectorList  | not pred repeat
    not symbolMember?(op,$formalArgList) and getmode(op,$e) is ['Mapping,:.] =>
      loc := [[op,sig],:loc]
    exp := [[op,sig],:exp]
  if loc then
    sayBrightly ['"%l",:bright '"  Missing Local Functions:"]
    for [op,sig] in loc for i in 1.. repeat
      sayBrightly ['"      [",i,'"]",:bright op,
        ": ",:formatUnabbreviatedSig sig]
  if exp then
    sayBrightly ['"%l",:bright '"  Missing Exported Functions:"]
    for [op,sig] in exp for i in 1.. repeat
      sayBrightly ['"      [",i,'"]",:bright op,
        ": ",:formatUnabbreviatedSig sig]
 
--% domain view code
 
makeFunctorArgumentParameters(argl,sigl,target) ==
  $forceAdd: local:= true
  $ConditionalOperators: local := nil
  ("append"/[fn(a,augmentSig(s,findExtras(a,target)))
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
        u:=ASSQ('CATEGORY,ss) =>
          MSUBST([:u,:ss],u,s)
        ['Join,:sl,['CATEGORY,'package,:ss]]
      ['Join,s,['CATEGORY,'package,:ss]]
    fn(a,s) ==
      isCategoryForm(s,$CategoryFrame) =>
        s is ["Join",:catlist] => genDomainViewList(a,s.args)
        [genDomainView(a,s,"getDomainView")]
      [a]
 
genDomainOps(dom,cat) ==
  oplist:= getOperationAlist(dom,dom,cat)
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
 
genDomainView(viewName,c,viewSelector) ==
  c is ['CATEGORY,.,:l] => genDomainOps(viewName,c)
  code:=
    c is ['SubsetCategory,c',.] => c'
    c
  $e:= augModemapsFromCategory(viewName,nil,c,$e)
  cd:= ["%LET",viewName,[viewSelector,viewName,mkTypeForm code]]
  if not listMember?(cd,$getDomainCode) then
          $getDomainCode:= [cd,:$getDomainCode]
  viewName

genDomainViewList: (%Symbol,%List %Form) -> %List %Code
genDomainViewList(id,catlist) ==
  [genDomainView(id,cat,"getDomainView") 
     for cat in catlist | isCategoryForm(cat,$EmptyEnvironment)]
 
mkOpVec(dom,siglist) ==
  dom:= getPrincipalView dom
  substargs := [['$,:canonicalForm dom],
                  :pairList($FormalMapVariableList,instantiationArgs dom)]
  oplist:= getConstructorOperationsFromDB instantiationCtor dom
  --new form is (<op> <signature> <slotNumber> <condition> <kind>)
  ops := newVector #siglist
  for (opSig:= [op,sig]) in siglist for i in 0.. repeat
    u:= ASSQ(op,oplist)
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
++ specialCases is (NIL l1 ... ln) where li is list of special cases
++ which can be given for each ti
++ removes declarative and assignment information from form and
++ signature, placing it in list L, replacing form by ("where",form',:L),
++ signature by a list of NILs (signifying declarations are in e)
compDefWhereClause(['DEF,form,signature,specialCases,body],m,e) ==
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
    orderByDependency(ASSOCLEFT argDepAlist,ASSOCRIGHT argDepAlist) where
      argDepAlist :=
        [[x,:dependencies] for [x,:y] in argSigAlist] where
          dependencies() ==
            union(listOfIdentifiersIn y,
              remove(listOfIdentifiersIn LASSOC(x,$predAlist),x))
          argSigAlist := [:$sigAlist,:pairList(argList,sigList)]
 
  -- 4. construct a WhereList which declares and/or defines the xi's in
  --    the order constructed in step 3
  whereList := [addSuchthat(x,[":",x,LASSOC(x,argSigAlist)]) for x in varList]
     where addSuchthat(x,y) ==
             p := LASSOC(x,$predAlist) => ["|",y,p]
             y
 
  -- 5. compile new ('DEF,("where",form',:WhereList),:.) where
  --    all argument parameters of form' are bound/declared in WhereList
  comp(form',m,e) where
    form' := ["where",defform,:whereList] where
      defform := ['DEF,form'',signature',specialCases,body] where
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
      [v for v in vl for d in dl | null intersection(d,vl)] or return nil
    orderedVarList:= [:newl,:orderedVarList]
    vl' := setDifference(vl,newl)
    dl' := [setDifference(d,newl) for x in vl for d in dl
             | symbolMember?(x,vl')]
    vl := vl'
    dl := dl'
  removeDuplicates reverse! orderedVarList --ordered so ith is indep. of jth if i < j
 
compDefineCapsuleFunction(df is ['DEF,form,signature,specialCases,body],
  m,$e,$prefix,$formalArgList) ==
    [lineNumber,:specialCases] := specialCases
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
    $returnMode:= m
    -- Change "^" to "**" in definitions.  All other places have 
    -- been changed before we get here.
    if form.op = "^" then 
      sayBrightly ['"Replacing", :bright '"^", '"with",:bright '"**"]
      form.op := "**"
    [$op,:argl]:= form
    $form:= [$op,:argl]
    argl:= stripOffArgumentConditions argl
    $formalArgList:= [:argl,:$formalArgList]
 
    --let target and local signatures help determine modes of arguments
    argModeList :=
      identSig := hasSigInTargetCategory(argl,form,signature.target,e) =>
        (e:= checkAndDeclare(argl,form,identSig,e); identSig.source)
      [getArgumentModeOrMoan(a,form,e) for a in argl]
    argModeList := stripOffSubdomainConditions(argModeList,argl)
    signature' := [signature.target,:argModeList]
    if null identSig then  --make $op a local function
      $e := put($op,'mode,['Mapping,:signature'],$e)
 
    --obtain target type if not given
    if null signature'.target then signature':=
      identSig => identSig
      getSignature($op,signature'.source,e) or return nil
    e:= giveFormalParametersValues(argl,e)
 
    $signatureOfForm:= signature' --this global is bound in compCapsuleItems
    $functionLocations := [[[$op,$signatureOfForm],:lineNumber],
      :$functionLocations]
    e:= addDomain(signature'.target,e)
    e:= compArgumentConditions e
 
    if $profileCompiler then
      for x in argl for t in signature'.source repeat 
        profileRecord('arguments,x,t)

    --4. introduce needed domains into extendedEnv
    for domain in signature' repeat e:= addDomain(domain,e)
 
    --6. compile body in environment with extended environment
    rettype:= resolve(signature'.target,$returnMode)
 
    localOrExported :=
      not symbolMember?($op,$formalArgList) and
        getmode($op,e) is ['Mapping,:.] => 'local
      'exported
 
    --6a skip if compiling only certain items but not this one
    -- could be moved closer to the top
    formattedSig := formatUnabbreviated ['Mapping,:signature']
    $compileOnlyCertainItems and _
      not symbolMember?($op, $compileOnlyCertainItems) =>
        sayBrightly ['"   skipping ", localOrExported,:bright $op]
        [nil,['Mapping,:signature'],$e]
    sayBrightly ['"   compiling ",localOrExported,
      :bright $op,'": ",:formattedSig]
 
    noteCapsuleFunctionDefinition($op,signature', makePredicate $predl)
    T := CATCH('compCapsuleBody, compOrCroak(body,rettype,e))
	 or [$ClearBodyToken,rettype,e]
    NRTassignCapsuleFunctionSlot($op,signature')
    if $newCompCompare=true then
       SAY '"The old compiler generates:"
       prTriple T
    --  A THROW to the above CATCH occurs if too many semantic errors occur
    --  see stackSemanticError
    catchTag:= MKQ gensym()
    fun:=
      body':= replaceExitEtc(T.expr,catchTag,"TAGGEDreturn",$returnMode)
      body':= addArgumentConditions(body',$op)
      finalBody:= ["CATCH",catchTag,body']
      compile [$op,["LAM",[:argl,'_$],finalBody]]
    $functorStats:= addStats($functorStats,$functionStats)
 
    --7. give operator a 'value property
    val:= [fun,signature',e]
    [fun,['Mapping,:signature'],$e]
 
getSignatureFromMode(form,e) ==
  getmode(opOf form,e) is ['Mapping,:signature] =>
    #form~=#signature => stackAndThrow ["Wrong number of arguments: ",form]
    applySubst(pairList($FormalMapVariableList,form.args),signature)

candidateSignatures(op,nmodes,slot1) ==
  [sig for [[=op,sig,:.],:.] in slot1 | #sig = nmodes]

domainMember(dom,domList) ==
  or/[modeEqual(dom,d) for d in domList]
 
augModemapsFromDomain(name,functorForm,e) ==
  symbolMember?(KAR name or name,$DummyFunctorNames) => e
  name = $Category or isCategoryForm(name,e) => e
  listMember?(name,getDomainsInScope e) => e
  if super := superType functorForm then
    e := addNewDomain(super,e)
  if name is ["Union",:dl] then for d in stripUnionTags dl
                         repeat e:= addDomain(d,e)
  augModemapsFromDomain1(name,functorForm,e)

addNewDomain(domain,e) ==
  augModemapsFromDomain(domain,domain,e)

addDomain(domain,e) ==
  domain isnt [.,:.] =>
    domain="$EmptyMode" => e
    domain="$NoValueMode" => e
    not ident? domain or 2 < #(s:= STRINGIMAGE domain) and
      char "#" = stringChar(s,0) and char "#" = stringChar(s,1) => e
    symbolMember?(domain,getDomainsInScope e) => e
    isLiteral(domain,e) => e
    addNewDomain(domain,e)
  (name:= first domain)='Category => e
  domainMember(domain,getDomainsInScope e) => e
  getmode(name,e) is ["Mapping",target,:.] and isCategoryForm(target,e)=>
      addNewDomain(domain,e)
    -- constructor? test needed for domains compiled with $bootStrapMode=true
  isFunctor name or constructor? name => addNewDomain(domain,e)
  -- ??? we should probably augment $DummyFunctorNames with CATEGORY
  -- ??? so that we don't have to do this special check here.  Investigate.
  isQuasiquote domain => e 
  if not isCategoryForm(domain,e) and name ~= "Mapping" then
    unknownTypeError name
  e        --is not a functor
 

++ We are compiling a capsule function definition with head given by `form'.
++ Determine whether the function with possibly partial signature `opsig'
++ is exported.  Return the complete signature if yes; otherwise
++ return nil, with diagnostic in ambiguity case.
hasSigInTargetCategory(argl,form,opsig,e) ==
  sigs := candidateSignatures($op,#form,categoryExports $domainShell)
  cc := checkCallingConvention(sigs,#argl)
  mList:= [(cc.i > 0 => quasiquote x; getArgumentMode(x,e))
            for x in argl for i in 0..]
    --each element is a declared mode for the variable or nil if none exists
  potentialSigList:=
    removeDuplicates
      [sig for sig in sigs |
          fn(sig,opsig,mList)] where
            fn(sig,opsig,mList) ==
              (null opsig or opsig=sig.target) and
                (and/[compareMode2Arg(x,m) for x in mList for m in sig.source])
  c:= #potentialSigList
  1=c => first potentialSigList
    --accept only those signatures op right length which match declared modes
  0=c => (#(sig:= getSignatureFromMode(form,e))=#form => sig; nil)
  1<c =>
    ambiguousSignatureError($op,potentialSigList)
    first potentialSigList
  nil --this branch will force all arguments to be declared
 
compareMode2Arg(x,m) == null x or modeEqual(x,m)
 
getArgumentModeOrMoan: (%Form, %Form, %Env) -> %Mode
getArgumentModeOrMoan(x,form,e) ==
  getArgumentMode(x,e) or
    stackSemanticError(["argument ",x," of ",form," is not declared"],nil)

getArgumentMode: (%Form,%Env) -> %Mode 
getArgumentMode(x,e) ==
  string? x => x
  m:= get(x,'mode,e) => m
 
checkAndDeclare(argl,form,sig,e) ==
-- arguments with declared types must agree with those in sig;
-- those that don't get declarations put into e
  for a in argl for m in sig.source repeat
    isQuasiquote m => nil	  -- we just built m from a.
    m1:= getArgumentMode(a,e) =>
      not modeEqual(m1,m) =>
        stack:= ["   ",:bright a,'"must have type ",m,
          '" not ",m1,'"%l",:stack]
    e:= put(a,'mode,m,e)
  if stack then
    sayBrightly ['"   Parameters of ",:bright form.op,
      '" are of wrong type:",'"%l",:stack]
  e
 
getSignature(op,argModeList,$e) ==
  1=#
    (sigl:=
      removeDuplicates
        [sig
          for [[dc,:sig],[pred,:.]] in (mmList:= get(op,'modemap,$e)) | dc='_$
            and sig.source = argModeList and knownInfo(pred,$e)]) => first sigl
  null sigl =>
    (u:= getmode(op,$e)) is ['Mapping,:sig] => sig
    SAY '"************* USER ERROR **********"
    SAY("available signatures for ",op,": ")
    if null mmList
       then SAY "    NONE"
       else for [[dc,:sig],:.] in mmList repeat printSignature("     ",op,sig)
    printSignature("NEED ",op,["?",:argModeList])
    nil
  1=#sigl => first sigl
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
 
compArgumentConditions: %Env -> %Env
compArgumentConditions e ==
  $argumentConditionList:=
    [f for [n,a,x] in $argumentConditionList] where
      f() ==
        y:= substitute(a,'_#1,x)
        T := [.,.,e]:= compOrCroak(y,$Boolean,e)
        [n,x,T.expr]
  e

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
 
putInLocalDomainReferences (def := [opName,[lam,varl,body]]) ==
  NRTputInTail CDDADR def
  def
 
 
$savableItems := nil
 
compile u ==
  [op,lamExpr] := u
  if $suffix then
    $suffix:= $suffix+1
    op':=
      opexport:=nil
      opmodes:=
        [sel
          for [[DC,:sig],[.,sel]] in get(op,'modemap,$e) |
            DC='_$ and (opexport:=true) and
             (and/[modeEqual(x,y) for x in sig for y in $signatureOfForm])]
      isLocalFunction op =>
        if opexport then userError ['"%b",op,'"%d",'" is local and exported"]
        makeSymbol strconc(encodeItem $prefix,'";",encodeItem op) 
      encodeFunctionName(op,$functorForm,$signatureOfForm,";",$suffix)
     where
       isLocalFunction op ==
         null symbolMember?(op,$formalArgList) and
           getmode(op,$e) is ['Mapping,:.]
    u:= [op',lamExpr]
  -- If just updating certain functions, check for previous existence.
  -- Deduce old sequence number and use it (items have been skipped).
  if $LISPLIB and $compileOnlyCertainItems then
    parts := splitEncodedFunctionName(u.op, ";")
  --  Next line JHD/SMWATT 7/17/86 to deal with inner functions
    parts='inner => $savableItems:=[u.op,:$savableItems]
    unew  := nil
    for [s,t] in $splitUpItemsAlreadyThere repeat
       if parts.0=s.0 and parts.1=s.1 and parts.2=s.2 then unew := t
    null unew =>
      sayBrightly ['"   Error: Item did not previously exist"]
      sayBrightly ['"   Item not saved: ", :bright u.op]
      sayBrightly ['"   What's there is: ", $lisplibItemsAlreadyThere]
      nil
    sayBrightly ['"   Renaming ", u.op, '" as ", unew]
    u := [unew, :rest u]
    $savableItems := [unew, :$saveableItems] -- tested by embedded RWRITE
  optimizedBody:= optimizeFunctionDef u
  stuffToCompile:=
    if not $insideCapsuleFunctionIfTrue
       then optimizedBody
       else putInLocalDomainReferences optimizedBody
  $doNotCompileJustPrint => (PRETTYPRINT stuffToCompile; op')
  $macroIfTrue => constructMacro stuffToCompile

  -- Let the backend know about this function's type
  if $insideCapsuleFunctionIfTrue and $optProclaim then
    proclaimCapsuleFunction(op',$signatureOfForm)

  result:= spadCompileOrSetq stuffToCompile
  functionStats:=[0,elapsedTime()]
  $functionStats:= addStats($functionStats,functionStats)
  printStats functionStats
  result

++ Subroutine of compile.  Called to generate backend code for
++ items defined directly or indirectly at capsule level.   This is
++ also used to compile functors.
spadCompileOrSetq (form is [nam,[lam,vl,body]]) ==
        --bizarre hack to take account of the existence of "known" functions
        --good for performance (LISPLLIB size, BPI size, NILSEC)
  CONTAINED($ClearBodyToken,body) => sayBrightly ['"  ",:bright nam,'" not compiled"]

  -- flag parameters needs to be made atomic, otherwise Lisp is confused.
  -- We try our best to preserve
  -- Note that we don't need substitution in the body because flag
  -- parameters are never used in the body.
  vl := [ renameParameter for v in vl] where
    renameParameter() ==
      integer? v or ident? v or string? v => v
      gensym '"flag"
  clearReplacement nam   -- Make sure we have fresh info
  if $optReplaceSimpleFunctions then
    body := replaceSimpleFunctions body

  if nam' := forwardingCall?(vl,body) then
      registerFunctionReplacement(nam,nam')
      sayBrightly ['"     ",:bright nam,'"is replaced by",:bright nam']
  else if macform := expandableDefinition?(vl,body) then
           registerFunctionReplacement(nam,macform)
           sayBrightly ['"     ",:bright nam,'"is replaced by",:bright body]

  form := 
    getFunctionReplacement nam => 
      [nam,[lam,vl,["DECLARE",["IGNORE",last vl]],body]]
    [nam,[lam,vl,body]]

  $insideCapsuleFunctionIfTrue => 
    $optExportedFunctionReference =>
      $capsuleFunctionStack := [form,:$capsuleFunctionStack]
      first form
    first backendCompile [form]
  compileConstructor form
 
compileConstructor form ==
  u:= compileConstructor1 form
  clearClams()                  --clear all CLAMmed functions
  u
 
compileConstructor1 (form:=[fn,[key,vl,:bodyl]]) ==
-- fn is the name of some category/domain/package constructor;
-- we will cache all of its values on $ConstructorCache with reference
-- counts
  $clamList: local := nil
  lambdaOrSlam :=
    getConstructorKindFromDB fn = "category" => 'SPADSLAM
    dbInstanceCache constructorDB fn = nil => 'LAMBDA
    $clamList:=
      [[fn,"$ConstructorCache",'domainEqualList,'count],:$clamList]
    'LAMBDA
  compForm:= [[fn,[lambdaOrSlam,vl,:bodyl]]]
  if getConstructorKindFromDB fn = "category"
      then u:= compAndDefine compForm
      else u:= backendCompile compForm
  clearConstructorCache fn      --clear cache for constructor
  first u
 
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

registerInlinableDomain(x,e) ==
  x := macroExpand(x,e)
  x is [ctor,:.] =>
    constructor? ctor => nominateForInlining ctor
    ctor is ":" => registerInlinableDomain(third x,e)
    builtinFunctorName? ctor =>
      for t in x.args repeat
        registerInlinableDomain(t,e)
    nil
  nil

compAdd(['add,$addForm,capsule],m,e) ==
  $bootStrapMode =>
    if $addForm is ["%Comma",:.] then code := nil
       else [code,m,e]:= comp($addForm,m,e)
    [['%when, _
       ['$bootStrapMode, _
           code],_
       ['%otherwise, ['systemError,['%list,'"%b",MKQ $functorForm.op,'"%d",'"from", _
         '"%b",MKQ namestring _/EDITFILE,'"%d",'"needs to be compiled"]]]],m,e]
  $addFormLhs: local:= $addForm
  if $addForm is ["SubDomain",domainForm,predicate] then
    $NRTaddForm := domainForm
    NRTgetLocalIndex domainForm
    registerInlinableDomain(domainForm,e)
    --need to generate slot for add form since all $ go-get
    --  slots will need to access it
    [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
  else
    $NRTaddForm := $addForm
    [$addForm,.,e]:=
      $addForm is ["%Comma",:.] =>
        $NRTaddForm := ["%Comma",:[NRTgetLocalIndex x for x in $addForm.args]]
        for x in $addForm.args repeat registerInlinableDomain(x,e)
        compOrCroak(compTuple2Record $addForm,$EmptyMode,e)
      registerInlinableDomain($addForm,e)
      compOrCroak($addForm,$EmptyMode,e)
  compCapsule(capsule,m,e)
 
compTuple2Record u ==
  ['Record,:[[":",i,x] for i in 1.. for x in u.args]]

compCapsule(['CAPSULE,:itemList],m,e) ==
  $bootStrapMode =>
    [bootStrapError($functorForm, _/EDITFILE),m,e]
  $insideExpressionIfTrue: local:= false
  $useRepresentationHack := true
  clearCapsuleFunctionTable()
  e := checkRepresentation($addFormLhs,itemList,e)
  compCapsuleInner(constructorDB $form.op,itemList,m,addDomain('_$,e))
 
compSubDomain(["SubDomain",domainForm,predicate],m,e) ==
  $addFormLhs: local:= domainForm
  $addForm: local := nil
  $NRTaddForm := domainForm
  [$addForm,.,e]:= compSubDomain1(domainForm,predicate,m,e)
  compCapsule(['CAPSULE],m,e)
 
compSubDomain1(domainForm,predicate,m,e) ==
  [.,.,e]:=
    compMakeDeclaration("#1",domainForm,addDomain(domainForm,e))
  u:=
    compCompilerPredicate(predicate,e) or
      stackSemanticError(["predicate: ",predicate,
        " cannot be interpreted with #1: ",domainForm],nil)
  pred := simplifyVMForm u.expr
  -- For now, reject predicates that directly reference domains
  CONTAINED("$",pred) => 
    stackAndThrow('"predicate %1pb is not simple enough",[predicate])
  emitSubdomainInfo($form,domainForm,pred)
  [domainForm,m,e]

compCapsuleInner(db,itemList,m,e) ==
  e:= addInformation(m,e)
           --puts a new 'special' property of $Information
  data := ["PROGN",:itemList]
      --RPLACd by compCapsuleItems and Friends
  e := compCapsuleItems(itemList,nil,e)
  localParList:= $functorLocalParameters
  if $addForm ~= nil then
    data := ['add,$addForm,data]
  code :=
    $insideCategoryIfTrue and not $insideCategoryPackageIfTrue => data
    buildFunctor(db,$signature,data,localParList,e)
  [MKPF([:$getDomainCode,code],"PROGN"),m,e]
 
--% PROCESS FUNCTOR CODE
 
compCapsuleItems(itemlist,$predl,$e) ==
  $signatureOfForm: local := nil
  $suffix: local:= 0
  for item in itemlist repeat 
    $e:= compSingleCapsuleItem(item,$predl,$e)
  $e
 
compSingleCapsuleItem(item,$predl,$e) ==
  doIt(macroExpandInPlace(item,$e),$predl)
  $e
 

++ subroutine of doIt.  Called to generate runtime noop insn.
mutateToNothing item ==
  item.op := 'PROGN
  item.rest := nil

doIt(item,$predl) ==
  $GENNO: local:= 0
  item is ['SEQ,:l,['exit,1,x]] =>
    item.op := "PROGN"
    lastNode(item).first := x
    for it1 in rest item repeat $e:= compSingleCapsuleItem(it1,$predl,$e)
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
    doIt(item,$predl)
  item is ["%LET",lhs,rhs,:.] =>
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
          registerInlinableDomain($Representation,$e)
    code is ["%LET",:.] =>
      item.op := '%store
      rhsCode := rhs'
      item.args := [['%tref,'$,NRTgetLocalIndex lhs],rhsCode]
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
  item is ["IF",p,x,y] => doItConditionally(item,$predl)
  item is ["where",b,:l] => compOrCroak(item,$EmptyMode,$e)
  item is ["MDEF",:.] => [.,.,$e]:= compOrCroak(item,$EmptyMode,$e)
  item is ['DEF,[op,:.],:.] =>
    body := isMacro(item,$e) => $e := putMacro(op,body,$e)
    [.,.,$e]:= t:= compOrCroak(item,$EmptyMode,$e)
    item.op := "CodeDefine"
        --Note that DescendCode, in CodeDefine, is looking for this
    second(item).rest := [$signatureOfForm]
      --This is how the signature is updated for buildFunctor to recognise
    functionPart:= ['dispatchFunction,t.expr]
    item.rest.rest.first := functionPart
    item.rest.rest.rest := nil
  u:= compOrCroak(item,$EmptyMode,$e) =>
    ([code,.,$e]:= u; item.first := first code; item.rest := rest code)
  systemErrorHere ["doIt", item]
 
isMacro(x,e) ==
  x is ['DEF,[op,:args],signature,specialCases,body] and
    null get(op,'modemap,e) and null args and null get(op,'mode,e)
      and signature is [nil] => body

++ Compile capsule-level `item' which is a conditional expression.
++ OpenAxiom's take on prepositional logical is a constructive
++ interpretation of logical connectives, in terms of IF-expresions.
++ In particular, a negation is positively interpretated by swapping
++ branches, and- and or-expressions are decomposed into nested
++ IF-expressions.  -- gdr, 2009-06-15.
doItConditionally(item,predl) ==
  item isnt ["IF",p,x,y] => systemErrorHere ["doItConditionally",item]
  p is ["not",p'] =>
    -- swap branches and recurse for positive interpretation.
    item.rest.first := p'
    item.rest.rest.first := y
    item.rest.rest.rest.first := x
    doItConditionally(item,predl)
  p is ["and",p',p''] =>
    item.rest.first := p'
    item.rest.rest.first := ["IF",p'',x,COPY y]
    doItConditionally(item,predl)
  p is ["or",p',p''] =>
    item.rest.first := p'
    item.rest.rest.rest.first := ["IF",p'',COPY x,y]
    doItConditionally(item,predl)
  doItIf(item,predl,$e)
    
 
doItIf(item is [.,p,x,y],$predl,$e) ==
  olde:= $e
  [p',.,$e]:= compCompilerPredicate(p,$e) or userError ['"not a Boolean:",p]
  oldFLP:=$functorLocalParameters
  if x~="%noBranch" then
    compSingleCapsuleItem(x,[p,:$predl],getSuccessEnvironment(p,$e))
    x':=localExtras(oldFLP)
  oldFLP:=$functorLocalParameters
  if y~="%noBranch" then
    compSingleCapsuleItem(y,[["not",p],:$predl],getInverseEnvironment(p,olde))
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

compContained: (%Form, %Mode, %Env) -> %Maybe %Triple
compContained(["CONTAINED",a,b],m,e) ==
  [a,ma,e]:= comp(a,$EmptyMode,e) or return nil
  [b,mb,e]:= comp(b,$EmptyMode,e) or return nil
  isCategoryForm(ma,e) and isCategoryForm(mb,e) =>
    (T:= [["CONTAINED",a,b],$Boolean,e]; convert(T,m))
  nil

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
        x isnt [.,:.] and getmode(x,e) = $Category => x
        stackSemanticError(["invalid argument to Join: ",x],nil)
        x
  T:= [wrapDomainSub(parameters,["Join",:catList']),$Category,e]
  convert(T,m)

compForMode: (%Form,%Mode,%Env) -> %Maybe %Triple 
compForMode(x,m,e) ==
  $compForModeIfTrue: local:= true
  comp(x,m,e)

makeCategoryForm(c,e) ==
  not isCategoryForm(c,e) => nil
  [x,m,e]:= compOrCroak(c,$EmptyMode,e)
  [x,e]

mustInstantiate: %Form -> %Thing 
mustInstantiate D ==
  D is [fn,:.] and 
    not (symbolMember?(fn,$DummyFunctorNames) or GET(fn,"makeFunctionList"))

wrapDomainSub: (%List %Form, %Form) -> %Form 
wrapDomainSub(parameters,x) ==
   ["DomainSubstitutionMacro",parameters,x]
 
mkExplicitCategoryFunction(domainOrPackage,sigList,atList) ==
  body:=
    ["mkCategory",MKQ domainOrPackage,['%list,:reverse sigList],
      ['%list,:reverse atList],MKQ domList,nil] where
        domList() ==
          ("union"/[fn sig for ["QUOTE",[[.,sig,:.],:.]] in sigList]) where
            fn sig == [D for D in sig | mustInstantiate D]
  parameters:=
    removeDuplicates
      ("append"/
        [[x for x in sig | ident? x and x~='_$]
          for ["QUOTE",[[.,sig,:.],:.]] in sigList])
  wrapDomainSub(parameters,body)

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
        first body is "QUOTE" => body
        cons? $definition and isFunctor body.op and 
          body.op ~= $definition.op => quoteForm simplifyVMForm body
        [Subst(parameters,u) for u in body]
  body isnt ["Join",:.] => body
  $definition isnt [.,:.] => body
  $definition.args = nil => body 
  name := makeSymbol strconc(KAR $definition,";CAT")
  SETANDFILE(name,nil)
  body := ['%when,[name],['%otherwise,['%store,name,body]]]
  body


++ Subroutine of compCategoryItem.
++ Compile exported signature `opsig' under predicate `pred' in
++ environment `env'.
compSignature(opsig,pred,env) ==
  [op,:sig] := opsig
  cons? op =>
    for y in op repeat 
      compSignature([y,:sig],pred,env)
  op in '(per rep) =>
    stackSemanticError(['"cannot export signature for", :bright op],nil)
    nil
  noteExport(opsig,pred)
  PUSH(MKQ [opsig,pred],$sigList)
 
compCategoryItem(x,predl,env) ==
  x is nil => nil
  --1. if x is a conditional expression, recurse; otherwise, form the predicate
  x is ['%when,[p,e]] =>
    predl':= [p,:predl]
    e is ["PROGN",:l] =>
      for y in l repeat compCategoryItem(y,predl',env)
    compCategoryItem(e,predl',env)
  x is ["IF",a,b,c] =>
    a is ["not",p] => compCategoryItem(["IF",p,c,b],predl,env)
    a is ["and",p,q] =>
      compCategoryItem(["IF",p,["IF",q,b,c],COPY c],predl,env)
    a is ["or",p,q] =>
      compCategoryItem(["IF",p,b,["IF",q,COPY b,c]],predl,env)
    predl':= [a,:predl]
    if b~="%noBranch" then
      b is ["PROGN",:l] => 
        for y in l repeat compCategoryItem(y,predl',env)
      compCategoryItem(b,predl',env)
    c="%noBranch" => nil
    predl':= [["not",a],:predl]
    c is ["PROGN",:l] => 
      for y in l repeat compCategoryItem(y,predl',env)
    compCategoryItem(c,predl',env)
  pred := (predl => MKPF(predl,"AND"); true)
 
  --2. if attribute, push it and return
  x is ["ATTRIBUTE",y] => 
    -- Attribute 'nil' carries no semantics.
    y = "nil" => nil
    noteExport(y,pred)
    PUSH(MKQ [y,pred],$atList)
 
  --3. it may be a list, with PROGN as the first, and some information as the rest
  x is ["PROGN",:l] => 
    for u in l repeat 
      compCategoryItem(u,predl,env)
 
  -- 4. otherwise, x gives a signature for a
  --    single operator name or a list of names; if a list of names,
  --    recurse
  x is ["SIGNATURE",:opsig] => compSignature(opsig,pred,env)
  systemErrorHere ["compCategoryItem",x]

compCategory: (%Form,%Mode,%Env) -> %Maybe %Triple
compCategory(x,m,e) ==
  clearExportsTable()
  (m:= resolve(m,$Category))=$Category and x is ['CATEGORY,
    domainOrPackage,:l] =>
      $sigList: local := nil
      $atList: local := nil
      for x in l repeat compCategoryItem(x,nil,e)
      rep:= mkExplicitCategoryFunction(domainOrPackage,$sigList,$atList)
    --if inside compDefineCategory, provide for category argument substitution
      [rep,m,e]
  systemErrorHere ["compCategory",x]

--%
