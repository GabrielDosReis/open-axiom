-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2009, Gabriel Dos Reis.
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
import info
namespace BOOT

--%

$forceAdd := false

--% EXTERNAL ROUTINES
 
--These functions are called from outside this file to add a domain
--   or to get the current domains in scope;
 
addDomain(domain,e) ==
  atom domain =>
    domain="$EmptyMode" => e
    domain="$NoValueMode" => e
    not IDENTP domain or 2<#(s:= STRINGIMAGE domain) and
      char "#" = s.0 and char "#" = s.1 => e
    MEMQ(domain,getDomainsInScope e) => e
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
  if not isCategoryForm(domain,e) and
    not member(name,'(Mapping CATEGORY)) then
      unknownTypeError name
  e        --is not a functor
 
domainMember(dom,domList) == or/[modeEqual(dom,d) for d in domList]
 
--% MODEMAP FUNCTIONS
 
--getTargetMode(x is [op,:argl],e) ==
--  CASES(#(mml:= getModemapList(op,#argl,e)),
--    (1 =>
--    ([[.,target,:.],:.]:= first mml; substituteForFormalArguments(argl,target))
--      ; 0 => MOAN(x," has no modemap"); systemError [x," has duplicate modemaps"]))
 
getModemap(x is [op,:.],e) ==
  for modemap in get(op,'modemap,e) repeat
    if u:= compApplyModemap(x,modemap,e) then return
      ([.,.,sl]:= u; SUBLIS(sl,modemap))
 
getUniqueSignature(form,e) ==
  [[.,:sig],:.]:= getUniqueModemap(first form,#rest form,e) or return nil
  sig
 
getUniqueModemap(op,numOfArgs,e) ==
  1=#(mml:= getModemapList(op,numOfArgs,e)) => first mml
  1<#mml =>
    stackWarning('"%1 argument form of %2b has more than one modemap", 
      [numOfArgs,op])
    first mml
  nil
 
getModemapList(op,numOfArgs,e) ==
  op is ['elt,D,op'] => getModemapListFromDomain(op',numOfArgs,D,e)
  [mm for
    (mm:= [[.,.,:sigl],:.]) in get(op,'modemap,e) | numOfArgs=#sigl]
 
getModemapListFromDomain(op,numOfArgs,D,e) ==
  [mm
    for (mm:= [[dc,:sig],:.]) in get(op,'modemap,e) | dc=D and #rest sig=
      numOfArgs]


insertModemap(new,mmList) ==
  null mmList => [new]
--isMoreSpecific(new,old:= first mmList) => [new,:mmList]
--[old,:insertModemap(new,rest mmList)]
  [new,:mmList]

addModemap(op,mc,sig,pred,fn,$e) ==
  $InteractiveMode => $e
  if knownInfo pred then pred:=true
  $insideCapsuleFunctionIfTrue=true =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)
 
addModemapKnown(op,mc,sig,pred,fn,$e) ==
--  if knownInfo pred then pred:=true
--  that line is handled elsewhere
  $insideCapsuleFunctionIfTrue=true =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)
 
addModemap0(op,mc,sig,pred,fn,e) ==
  --mc is the "mode of computation"; fn the "implementation"
  $functorForm is ['CategoryDefaults,:.] and mc="$" => e
    --don't put CD modemaps into environment
  --fn is ['Subsumed,:.] => e  -- don't skip subsumed modemaps
                               -- breaks -:($,$)->U($,failed) in DP
  op='elt or op='setelt => addEltModemap(op,mc,sig,pred,fn,e)
  addModemap1(op,mc,sig,pred,fn,e)
 
addEltModemap(op,mc,sig,pred,fn,e) ==
   --hack to change selectors from strings to identifiers; and to
   --add flag identifiers as literals in the envir
  op='elt and sig is [:lt,sel] =>
    STRINGP sel =>
      id:= INTERN sel
      if $insideCapsuleFunctionIfTrue=true
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id],pred,fn,e)
    -- atom sel => systemErrorHere '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  op='setelt and sig is [:lt,sel,v] =>
    STRINGP sel =>
      id:= INTERN sel
      if $insideCapsuleFunctionIfTrue=true
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id,v],pred,fn,e)
    -- atom sel => systemError '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  systemErrorHere '"addEltModemap"
 
addModemap1(op,mc,sig,pred,fn,e) ==
   --mc is the "mode of computation"; fn the "implementation"
  if mc="Rep" then sig := substituteDollarIfRepHack sig
  currentProplist:= getProplist(op,e) or nil
  newModemapList:=
    mkNewModemapList(mc,sig,pred,fn,LASSOC('modemap,currentProplist),e,nil)
  newProplist:= augProplist(currentProplist,'modemap,newModemapList)
  newProplist':= augProplist(newProplist,"FLUID",true)
  unErrorRef op
        --There may have been a warning about op having no value
  addBinding(op,newProplist',e)
 
mkNewModemapList(mc,sig,pred,fn,curModemapList,e,filenameOrNil) ==
  entry:= [map:= [mc,:sig],[pred,fn],:filenameOrNil]
  member(entry,curModemapList) => curModemapList
  (oldMap:= assoc(map,curModemapList)) and oldMap is [.,[opred, =fn],:.] =>
    $forceAdd => mergeModemap(entry,curModemapList,e)
    opred=true => curModemapList
    if pred ~= true and pred ~= opred then pred:= ["OR",pred,opred]
    [if x=oldMap then [map,[pred,fn],:filenameOrNil] else x
 
  --if new modemap less general, put at end; otherwise, at front
      for x in curModemapList]
  $InteractiveMode => insertModemap(entry,curModemapList)
  mergeModemap(entry,curModemapList,e)
 
mergeModemap(entry is [[mc,:sig],[pred,:.],:.],modemapList,e) ==
  for (mmtail:= [[[mc',:sig'],[pred',:.],:.],:.]) in tails modemapList repeat
    mc=mc' or isSubset(mc,mc',e) =>
      newmm:= nil
      mm:= modemapList
      while (not EQ(mm,mmtail)) repeat (newmm:= [first mm,:newmm]; mm:= rest mm)
      if (mc=mc') and (sig=sig') then
        --We only need one of these, unless the conditions are hairy
        not $forceAdd and TruthP pred' =>
          entry:=nil
              --the new predicate buys us nothing
          return modemapList
        TruthP pred => mmtail:=rest mmtail
          --the thing we matched against is useless, by comparison
      modemapList:= NCONC(nreverse newmm,[entry,:mmtail])
      entry:= nil
      return modemapList
  if entry then [:modemapList,entry] else modemapList
 
-- next definition RPLACs, and hence causes problems.
-- In ptic., SubResGcd in SparseUnivariatePolynomial is miscompiled
--mergeModemap(entry:=((mc,:sig),:.),modemapList,e) ==
--    for (mmtail:= (((mc',:sig'),:.),:.)) in tails modemapList do
--       mc=mc' or isSubset(mc,mc',e)  =>
--         RPLACD(mmtail,(first mmtail,: rest mmtail))
--         RPLACA(mmtail,entry)
--         entry := nil
--         return modemapList
--     if entry then (:modemapList,entry) else modemapList
 
--substituteForRep(entry is [[mc,:sig],:.],curModemapList) ==
--  --change 'Rep to "$" unless the resulting signature is already in $
--  member(entry':= substitute("$",'Rep,entry),curModemapList) =>
--    [entry,:curModemapList]
--  [entry,entry',:curModemapList]
 
addNewDomain(domain,e) ==
  augModemapsFromDomain(domain,domain,e)

augModemapsFromDomain(name,functorForm,e) ==
  member(KAR name or name,$DummyFunctorNames) => e
  name=$Category or isCategoryForm(name,e) => e
  member(name,curDomainsInScope:= getDomainsInScope e) => e
  if super := superType functorForm then
    e := addNewDomain(super,e)
  if innerDom:= listOrVectorElementMode name then e:= addDomain(innerDom,e)
  if name is ["Union",:dl] then for d in stripUnionTags dl
                         repeat e:= addDomain(d,e)
  augModemapsFromDomain1(name,functorForm,e)

augModemapsFromDomain1(name,functorForm,e) ==
  GETL(KAR functorForm,"makeFunctionList") =>
    addConstructorModemaps(name,functorForm,e)
  atom functorForm and (catform:= getmode(functorForm,e)) =>
    augModemapsFromCategory(name,name,functorForm,catform,e)
  mappingForm:= getmodeOrMapping(KAR functorForm,e) =>
    ["Mapping",categoryForm,:functArgTypes]:= mappingForm
    catform:= substituteCategoryArguments(rest functorForm,categoryForm)
    augModemapsFromCategory(name,name,functorForm,catform,e)
  stackMessage('"%1pb is an unknown mode",[functorForm])
  e
 
substituteCategoryArguments(argl,catform) ==
  argl:= substitute("$$","$",argl)
  arglAssoc:= [[INTERNL("#",STRINGIMAGE i),:a] for i in 1.. for a in argl]
  SUBLIS(arglAssoc,catform)
 
         --Called, by compDefineFunctor, to add modemaps for $ that may
         --be equivalent to those of Rep. We must check that these
         --operations are not being redefined.
augModemapsFromCategoryRep(domainName,repDefn,functorBody,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(domainName,domainName,domainName,categoryForm,e)
  [repFnAlist,e]:= evalAndSub("Rep","Rep",repDefn,getmode(repDefn,e),e)
  catform:= (isCategory categoryForm => categoryForm.(0); categoryForm)
  compilerMessage('"Adding %1p modemaps",[domainName])
  e:= putDomainsInScope(domainName,e)
  for [lhs:=[op,sig,:.],cond,fnsel] in fnAlist repeat
    u:=assoc(substitute("Rep",domainName,lhs),repFnAlist)
    u and not AMFCR_,redefinedList(op,functorBody) =>
      fnsel' := third u
      e:= addModemap(op,domainName,sig,cond,fnsel',e)
    e:= addModemap(op,domainName,sig,cond,fnsel,e)
  e
 
AMFCR_,redefinedList(op,l) == "OR"/[AMFCR_,redefined(op,u) for u in l]
 
AMFCR_,redefined(opname,u) ==
  not(u is [op,:l]) => nil
  op = 'DEF => opname = CAAR l
  op in '(PROGN SEQ) => AMFCR_,redefinedList(opname,l)
  op = 'COND => "OR"/[AMFCR_,redefinedList(opname,rest u) for u in l]
 
augModemapsFromCategory(domainName,domainView,functorForm,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(domainName,domainView,functorForm,categoryForm,e)
  --  catform:= (isCategory categoryForm => categoryForm.(0); categoryForm)
  -- catform appears not to be used, so why set it?
  --if not $InteractiveMode then
  compilerMessage('"Adding %1p modemaps",[domainName])
  e:= putDomainsInScope(domainName,e)
  condlist:=[]
  for [[op,sig,:.],cond,fnsel] in fnAlist repeat
--  e:= addModemap(op,domainName,sig,cond,fnsel,e)
---------next 5 lines commented out to avoid wasting time checking knownInfo on
---------conditions attached to each modemap being added, takes a very long time
---------instead conditions will be checked when maps are actually used
  --v:=ASSOC(cond,condlist) =>
  --  e:= addModemapKnown(op,domainName,sig,rest v,fnsel,e)
  --$e:local := e  -- $e is used by knownInfo
  --if knownInfo cond then cond1:=true else cond1:=cond
  --condlist:=[[cond,:cond1],:condlist]
    e:= addModemapKnown(op,domainName,sig,cond,fnsel,e) -- cond was cond1
--  for u in sig | (not member(u,$DomainsInScope)) and
--                   (not atom u) and
--                     (not isCategoryForm(u,e)) do
--     e:= addNewDomain(u,e)
  e
 
--subCatParametersInto(domainForm,catForm,e) ==
--  -- JHD 08/08/84 perhaps we are fortunate that it is not used
--  --this is particularly dirty and should be cleaned up, say, by wrapping
--  -- an appropriate lambda expression around mapping forms
--  domainForm is [op,:l] and l =>
--    get(op,'modemap,e) is [[[mc,:.],:.]] => SUBLIS(PAIR(rest mc,l),catForm)
--  catForm
 
evalAndSub(domainName,viewName,functorForm,form,$e) ==
  $lhsOfColon: local:= domainName
  isCategory form => [substNames(domainName,viewName,functorForm,form.(1)),$e]
  --next lines necessary-- see MPOLY for which $ is actual arg. --- RDJ 3/83
  if CONTAINED("$$",form) then $e:= put("$$","mode",get("$","mode",$e),$e)
  opAlist:= getOperationAlist(domainName,functorForm,form)
  substAlist:= substNames(domainName,viewName,functorForm,opAlist)
  [substAlist,$e]
 
getOperationAlist(name,functorForm,form) ==
  if atom name and niladicConstructorFromDB name then 
    functorForm:= [functorForm]
-- (null isConstructorForm functorForm) and (u:= isFunctor functorForm)
  (u:= isFunctor functorForm) and not
    ($insideFunctorIfTrue and first functorForm=first $functorForm) => u
  $insideFunctorIfTrue and name="$" =>
    ($domainShell => $domainShell.(1); systemError '"$ has no shell now")
  T:= compMakeCategoryObject(form,$e) => ([.,.,$e]:= T; T.expr.(1))
  stackMessage('"not a category form: %1bp",[form])
 
substNames(domainName,viewName,functorForm,opalist) ==
  functorForm := SUBSTQ("$$","$", functorForm)
  nameForDollar :=
    isCategoryPackageName functorForm => second functorForm
    domainName
    
       -- following calls to SUBSTQ must copy to save RPLAC's in
       -- putInLocalDomainReferences
  [[:SUBSTQ("$","$$",SUBSTQ(nameForDollar,"$",modemapform)),
       [sel, viewName,if domainName = "$" then pos else
                                         CADAR modemapform]]
     for [:modemapform,[sel,"$",pos]] in
          EQSUBSTLIST(KDR functorForm,$FormalMapVariableList, opalist)]
 
addConstructorModemaps(name,form is [functorName,:.],e) ==
  $InteractiveMode: local:= nil
  e:= putDomainsInScope(name,e) --frame
  fn := GETL(functorName,"makeFunctionList")
  [funList,e]:= FUNCALL(fn,name,form,e)
  for [op,sig,opcode] in funList repeat
    if opcode is [sel,dc,n] and sel='ELT then
          nsig := substitute("$$$",name,sig)
          nsig := substitute('$,"$$$",substitute("$$",'$,nsig))
          opcode := [sel,dc,nsig]
    e:= addModemap(op,name,sig,true,opcode,e)
  e
 
 
--The way XLAMs work:
--  ((XLAM ($1 $2 $3) (SETELT $1 0 $3)) X "c" V) ==> (SETELT X 0 V)
 
getDomainsInScope e ==
  $insideCapsuleFunctionIfTrue=true => $CapsuleDomainsInScope
  get("$DomainsInScope","special",e)
 
putDomainsInScope(x,e) ==
  l:= getDomainsInScope e
  if $verbose and member(x,l) then 
    sayBrightly ['" Note: Domain ",x," already in scope"]
  newValue:= [x,:delete(x,l)]
  $insideCapsuleFunctionIfTrue => ($CapsuleDomainsInScope:= newValue; e)
  put("$DomainsInScope","special",newValue,e)
 
