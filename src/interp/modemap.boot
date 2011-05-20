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

--%

$forceAdd := false

--% EXTERNAL ROUTINES
 
--These functions are called from outside this file to add a domain
--   or to get the current domains in scope;
 
addDomain(domain,e) ==
  atom domain =>
    domain="$EmptyMode" => e
    domain="$NoValueMode" => e
    not IDENTP domain or 2 < #(s:= STRINGIMAGE domain) and
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
      ([.,.,sl]:= u; applySubst(sl,modemap))
 
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
  $insideCapsuleFunctionIfTrue =>
    $CapsuleModemapFrame :=
      addModemap0(op,mc,sig,pred,fn,$CapsuleModemapFrame)
    $e
  addModemap0(op,mc,sig,pred,fn,$e)
 
addModemapKnown(op,mc,sig,pred,fn,$e) ==
--  if knownInfo pred then pred:=true
--  that line is handled elsewhere
  $insideCapsuleFunctionIfTrue =>
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
    string? sel =>
      id:= makeSymbol sel
      if $insideCapsuleFunctionIfTrue
         then $e:= makeLiteral(id,$e)
         else e:= makeLiteral(id,e)
      addModemap1(op,mc,[:lt,id],pred,fn,e)
    -- atom sel => systemErrorHere '"addEltModemap"
    addModemap1(op,mc,sig,pred,fn,e)
  op='setelt and sig is [:lt,sel,v] =>
    string? sel =>
      id:= makeSymbol sel
      if $insideCapsuleFunctionIfTrue
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
    mkNewModemapList(mc,sig,pred,fn,symbolLassoc('modemap,currentProplist),e,nil)
  newProplist:= augProplist(currentProplist,'modemap,newModemapList)
  newProplist':= augProplist(newProplist,"FLUID",true)
  unErrorRef op
        --There may have been a warning about op having no value
  addBinding(op,newProplist',e)
 
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
 
-- next definition RPLACs, and hence causes problems.
-- In ptic., SubResGcd in SparseUnivariatePolynomial is miscompiled
--mergeModemap(entry:=((mc,:sig),:.),modemapList,e) ==
--    for (mmtail:= (((mc',:sig'),:.),:.)) in tails modemapList do
--       mc=mc' or isSubset(mc,mc',e)  =>
--         mmtail.rest := (first mmtail,: rest mmtail)
--         mmtail.first := entry
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
  symbolMember?(KAR name or name,$DummyFunctorNames) => e
  name=$Category or isCategoryForm(name,e) => e
  listMember?(name,curDomainsInScope:= getDomainsInScope e) => e
  if super := superType functorForm then
    e := addNewDomain(super,e)
  if innerDom:= listOrVectorElementMode name then e:= addDomain(innerDom,e)
  if name is ["Union",:dl] then for d in stripUnionTags dl
                         repeat e:= addDomain(d,e)
  augModemapsFromDomain1(name,functorForm,e)

augModemapsFromDomain1(name,functorForm,e) ==
  property(KAR functorForm,"makeFunctionList") =>
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
  applySubst(arglAssoc,catform)
 
         --Called, by compDefineFunctor, to add modemaps for $ that may
         --be equivalent to those of Rep. We must check that these
         --operations are not being redefined.
augModemapsFromCategoryRep(domainName,repDefn,functorBody,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(domainName,domainName,domainName,categoryForm,e)
  [repFnAlist,e]:= evalAndSub("Rep","Rep",repDefn,getmode(repDefn,e),e)
  catform:= (categoryObject? categoryForm => categoryForm.0; categoryForm)
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
  op = '%when => "OR"/[AMFCR_,redefinedList(opname,rest u) for u in l]
 
augModemapsFromCategory(domainName,domainView,functorForm,categoryForm,e) ==
  [fnAlist,e]:= evalAndSub(domainName,domainView,functorForm,categoryForm,e)
  --  catform:= (categoryObject? categoryForm => categoryForm.0; categoryForm)
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
--                   (cons? u) and
--                     (not isCategoryForm(u,e)) do
--     e:= addNewDomain(u,e)
  e
 
--subCatParametersInto(domainForm,catForm,e) ==
--  -- JHD 08/08/84 perhaps we are fortunate that it is not used
--  --this is particularly dirty and should be cleaned up, say, by wrapping
--  -- an appropriate lambda expression around mapping forms
--  domainForm is [op,:l] and l =>
--    get(op,'modemap,e) is [[[mc,:.],:.]] => applySubst(pairList(rest mc,l),catForm)
--  catForm
 
evalAndSub(domainName,viewName,functorForm,form,$e) ==
  $lhsOfColon: local:= domainName
  categoryObject? form =>
    [substNames(domainName,viewName,functorForm,categoryExports form),$e]
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
    $domainShell => categoryExports $domainShell
    systemError '"$ has no shell now"
  T:= compMakeCategoryObject(form,$e) => ([.,.,$e]:= T; categoryExports T.expr)
  stackMessage('"not a category form: %1bp",[form])
 
substNames(domainName,viewName,functorForm,opalist) ==
  functorForm := substitute("$$","$", functorForm)
  nameForDollar :=
    isCategoryPackageName functorForm => second functorForm
    domainName
  [[:substitute("$","$$",substitute(nameForDollar,"$",modemapform)),
       [sel, viewName,if domainName = "$" then pos else
                                         modemapform.mmTarget]]
     for [:modemapform,[sel,"$",pos]] in
       applySubst(pairList($FormalMapVariableList,KDR functorForm),opalist)]
 
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
 
 
--The way XLAMs work:
--  ((XLAM ($1 $2 $3) (%store (%vref $1 0) $3)) X "c" V) ==>
--               (%store (%vref X 0) V)
--
 
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


import g_-util
namespace BOOT

printInfo $e ==
  for u in get("$Information","special",$e) repeat PRETTYPRINT u
  nil
 
addInformation(m,$e) ==
  $Information: local := nil
  info m where
    info m ==
      --Processes information from a mode declaration in compCapsule
      atom m => nil
      m is ["CATEGORY",.,:stuff] => for u in stuff repeat addInfo u
      m is ["Join",:stuff] => for u in stuff repeat info u
      nil
  $e:=
    put("$Information","special",[:$Information,:
      get("$Information","special",$e)],$e)
  $e
 
addInfo u == 
  $Information:= [formatInfo u,:$Information]
 
formatInfo u ==
  atom u => u
  u is ["SIGNATURE",:v] => ["SIGNATURE","$",:v]
  u is ["PROGN",:l] => ["PROGN",:[formatInfo v for v in l]]
  u is ["ATTRIBUTE",v] =>
 
    -- The parser can't tell between those attributes that really
    -- are attributes, and those that are category names
    atom v and isCategoryForm([v],$e) => ["has","$",[v]]
    atom v => ["ATTRIBUTE","$",v]
    isCategoryForm(v,$e) => ["has","$",v]
    ["ATTRIBUTE","$",v]
  u is ["IF",a,b,c] =>
    c="%noBranch" => ['%when,:liftCond [formatPred a,formatInfo b]]
    b="%noBranch" => ['%when,:liftCond [["not",formatPred a],formatInfo c]]
    ['%when,:liftCond [formatPred a,formatInfo b],:
      liftCond [["not",formatPred a],formatInfo c]]
  systemError ['"formatInfo",u]
 
liftCond (clause is [ante,conseq]) ==
  conseq is ['%when,:l] =>
    [[lcAnd(ante,a),:b] for [a,:b] in l] where
      lcAnd(pred,conj) ==
        conj is ["and",:ll] => ["and",pred,:ll]
        ["and",pred,conj]
  [clause]
 
formatPred u ==
         --Assumes that $e is set up to point to an environment
  u is ["has",a,b] =>
    atom b and isCategoryForm([b],$e) => ["has",a,[b]]
    atom b => ["has",a,["ATTRIBUTE",b]]
    isCategoryForm(b,$e) => u
    b is ["ATTRIBUTE",.] => u
    b is ["SIGNATURE",:.] => u
    ["has",a,["ATTRIBUTE",b]]
  atom u => u
  u is ["and",:v] => ["and",:[formatPred w for w in v]]
  systemError ['"formatPred",u]
 
chaseInferences(pred,$e) ==
  foo hasToInfo pred where
    foo pred ==
      knownInfo pred => nil
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
 
hasToInfo (pred is ["has",a,b]) ==
  b is ["SIGNATURE",:data] => ["SIGNATURE",a,:data]
  b is ["ATTRIBUTE",c] => ["ATTRIBUTE",a,c]
  pred
 
infoToHas a ==
  a is ["SIGNATURE",b,:data] => ["has",b,["SIGNATURE",:data]]
  a is ["ATTRIBUTE",b,c] => ["has",b,["ATTRIBUTE",c]]
  a

++ Return true if we are certain that the information
++ denotated by `pred' is derivable from the current environment. 
knownInfo pred ==
  pred=true => true
  listMember?(pred,get("$Information","special",$e)) => true
  pred is ["OR",:l] => or/[knownInfo u for u in l]
  pred is ["AND",:l] => and/[knownInfo u for u in l]
  pred is ["or",:l] => or/[knownInfo u for u in l]
  pred is ["and",:l] => and/[knownInfo u for u in l]
  pred is ["ATTRIBUTE",name,attr] =>
    v := compForMode(name,$EmptyMode,$e) or return
          stackAndThrow('"can't find category of %1pb",[name])
    [vv,.,.] := compMakeCategoryObject(v.mode,$e) or return
                 stackAndThrow('"can't make category of %1pb",[name])
    listMember?(attr,vv.2) => true
    x := assoc(attr,vv.2) => knownInfo second x
          --format is a list of two elements: information, predicate
    false
  pred is ["has",name,cat] =>
    cat is ["ATTRIBUTE",:a] => knownInfo ["ATTRIBUTE",name,:a]
    cat is ["SIGNATURE",:a] => knownInfo ["SIGNATURE",name,:a]
    -- unnamed category expressions imply structural checks.
    cat is ["Join",:.] => and/[knownInfo ["has",name,c] for c in cat.args]
    cat is ["CATEGORY",.,:atts] =>
      and/[knownInfo hasToInfo ["has",name,att] for att in atts]
    name is ['Union,:.] => false
    -- we have a named category expression
    v:= compForMode(name,$EmptyMode,$e) or return
          stackAndThrow('"can't find category of %1pb",[name])
    vmode := v.mode
    cat = vmode => true
    vmode is ["Join",:l] and listMember?(cat,l) => true
    [vv,.,.]:= compMakeCategoryObject(vmode,$e) or return
                 stackAndThrow('"cannot find category %1pb",[vmode])
    catlist := vv.4
    listMember?(cat,first catlist) => true  --checks princ. ancestors
    (u:=assoc(cat,second catlist)) and knownInfo second u => true
    -- previous line checks fundamental anscestors, we should check their
    --   principal anscestors but this requires instantiating categories

    or/[AncestorP(cat,[first u]) 
         for u in second catlist | knownInfo second u] => true
    false
  pred is ["SIGNATURE",name,op,sig,:.] =>
    v:= get(op,"modemap",$e)
    for w in v repeat
      ww := w.mmSignature  --the actual signature part
      ww = sig =>
        w.mmCondition  = true => return true
        false
        --error '"knownInfo"
  false
 
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
      listMember?(cat,first ocatvec.4) or
         assoc(cat,second ocatvec.4) is [.,"T",.] => $e
             --what was being asserted is an ancestor of what was known
      if name="$"
        then $e:= augModemapsFromCategory(name,name,name,cat,$e)
        else
          genDomainView(name,name,cat,"HasCategory")
          -- a domain upgrade at function level is local to that function.
          if not $insideCapsuleFunctionIfTrue and 
            not symbolMember?(name,$functorLocalParameters) then
              $functorLocalParameters:=[:$functorLocalParameters,name]
      compilerMessage('"augmenting %1: %2p", [name,cat])
      $e:= put(name,"value",[vval,mkJoin(cat,vmode),nil],$e)
    SAY("extension of ",vval," to ",cat," ignored")
    $e
  systemError ['"actOnInfo",u]
 
mkJoin(cat,mode) ==
  mode is ['Join,:cats] => ['Join,cat,:cats]
  ['Join,cat,mode]
 
GetValue name ==
  u:= get(name,"value",$e) => u
  u:= comp(name,$EmptyMode,$e) => u  --name may be a form
  systemError [name,'" is not bound in the current environment"]
 
