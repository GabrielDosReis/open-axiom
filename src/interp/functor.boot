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


import lisplib
import nruncomp
import category
namespace BOOT

--% Functions for building categories
 
CategoryPrint(D,$e) ==
  SAY "--------------------------------------"
  SAY "Name (and arguments) of category:"
  PRETTYPRINT canonicalForm D
  SAY "operations:"
  PRETTYPRINT categoryExports D
  SAY "attributes:"
  PRETTYPRINT categoryAttributes D
  SAY "This is a sub-category of"
  PRETTYPRINT first categoryRef(D,4)
  for u in second categoryRef(D,4) repeat
    SAY("This has an alternate view: slot ",rest u," corresponds to ",first u)
  for u in third categoryRef(D,4) repeat
    SAY("This has a local domain: slot ",rest u," corresponds to ",first u)
  for j in $NRTbase..maxIndex D repeat
    u := categoryRef(D,j)
    null u => SAY "another domain"
    first u isnt [.,:.] => SAY("Alternate View corresponding to: ",u)
    PRETTYPRINT u

--%  Domain printing
--Global strategy here is to maintain a list of substitutions
--  ( in $Sublis), of vectors and the names that they have,
--  which may be either local names ('View1') or global names ('Where1')
--  The global names are remembered on $Sublis from one
--  invocation of DomainPrint1 to the next
 
DomainPrint(D,brief) ==
  -- If brief is non-nil, %then only a summary is printed
  $WhereList: local := nil
  $Sublis: local := nil
  $WhereCounter: local := 1
  env:= $e or $EmptyEnvironment --in case we are called from top level
  categoryObject? D => CategoryPrint(D,env)
  $Sublis:= [[D,:'original]]
  SAY '"-----------------------------------------------------------------------"
  DomainPrint1(D,nil,env)
  while ($WhereList) repeat
    s:= $WhereList
    $WhereList:= nil
    for u in s repeat
      finishLine $OutputStream
      SAY ['"Where ",first u,'" is:"]
      DomainPrint1(rest u,brief,env)
  SAY '"-----------------------------------------------------------------------"
 
DomainPrint1(D,brief,$e) ==
  if vector? D then
    D := D.4 --if we were passed a vector, go to the domain
  Sublis:=
    [:
      [[rest u,:makeSymbol strconc('"View",toString i)]
        for u in D for i in 1..],:$Sublis]
  for u in D for i in 1.. repeat
    brief and i>1 => nil
    uu := copyVector rest u
    uu.4 := '"This domain"
    if not brief then
      SAY ['"View number ",i,'" corresponding to categories:"]
      PRETTYPRINT first u
    if i=1 and vector? uu.5 then
      vv := copyVector uu.5
      uu.5 := vv
      for j in 0..maxIndex vv repeat
        if vector? vv.j then
          l := objectAssoc(vv.j,Sublis)
          if l
             then name:= rest l
             else
              name := DPname()
              Sublis := [[vv.j,:name],:Sublis]
              $Sublis := [first Sublis,:$Sublis]
              $WhereList := [[name,:vv.j],:$WhereList]
          vv.j := name
    if i>1 then
      uu.1 := uu.2 := uu.5 := '"As in first view"
    for i in $NRTbase..maxIndex uu repeat
      uu.i := DomainPrintSubst(uu.i,Sublis)
      if vector? uu.i then
        name := DPname()
        Sublis := [[uu.i,:name],:Sublis]
        $Sublis := [first Sublis,:$Sublis]
        $WhereList := [[name,:uu.i],:$WhereList]
        uu.i := name
      if uu.i is [.,:v] and vector? v then
        name := DPname()
        Sublis := [[v,:name],:Sublis]
        $Sublis := [first Sublis,:$Sublis]
        $WhereList := [[name,:v],:$WhereList]
        uu.i := [first uu.i,:name]
    brief => PRETTYPRINT uu.0
    PRETTYPRINT uu
 
DPname() ==
  name := makeSymbol strconc('"Where",toString $WhereCounter)
  $WhereCounter := $WhereCounter+1
  name
 
PacPrint v ==
  vv := copyVector v
  for j in 0..maxIndex vv repeat
    if vector? vv.j then
      l := objectAssoc(vv.j,Sublis)
      if l
         then name := rest l
         else
          name := DPname()
          Sublis := [[vv.j,:name],:Sublis]
          $Sublis := [first Sublis,:$Sublis]
          $WhereList := [[name,:vv.j],:$WhereList]
      vv.j := name
    if cons? vv.j and vector?(u:=rest vv.j) then
      l := objectAssoc(u,Sublis)
      if l
         then name := rest l
         else
          name := DPname()
          Sublis := [[u,:name],:Sublis]
          $Sublis := [first Sublis,:$Sublis]
          $WhereList := [[name,:u],:$WhereList]
      vv.j.rest := name
  PRETTYPRINT vv
 
DomainPrintSubst(item,Sublis) ==
  item is [a,:b] =>
    c1 := DomainPrintSubst(a,Sublis)
    c2 := DomainPrintSubst(b,Sublis)
    sameObject?(c1,a) and sameObject?(c2,b) => item
    [c1,:c2]
  l := objectAssoc(item,Sublis)
  l => rest l
  l := objectAssoc(item,Sublis)
  l => rest l
  item
 
--%  Utilities
 
mkDevaluate a ==
  a = nil => nil
  a is ['QUOTE,a'] =>
    a' = nil => nil
    a
  a is '$ => MKQ '$
  a is ['%list,:.] =>
    a.args = nil => nil
    a
  ['devaluate,a]
 
getDomainView(domain,catform) ==
  u := HasCategory(domain,catform) => u
  c := eval catform
  u := HasCategory(domain,c.0) => u
  -- note:  this is necessary because of domain == another domain, e.g.
  -- Ps are defined to be SUPs with specific arguments so that if one
  -- asks if a P is a Module over itself, here one has catform= (Module
  -- (P I)) yet domain is a SUP.  By oding this evaluation, c.0=SUP as
  -- well and test works --- RDJ 10/31/83
  throwKeyedMsg("S2IF0009",[devaluate domain, catform])
 
getPrincipalView domain ==
  pview := domain
  for [.,:view] in domain.4 repeat
    if #view > #pview then
      pview := view
  pview
 
CategoriesFromGDC x ==
  x isnt [.,:.] => nil
  x is ['%list,a,:b] and a is ['QUOTE,a'] =>
    union([[a']],"union"/[CategoriesFromGDC u for u in b])
  x is ['QUOTE,a] and a is [b] => [a]
 
compCategories(u,e) ==
  u isnt [.,:.] => u
  cons? u.op =>
    error ['"compCategories: need an atom in operator position", u.op]
  u.op in '(Record Union Mapping) =>
    -- There is no modemap property for these guys so do it by hand.
    [u.op, :[compCategories1(a,$SetCategory,e) for a in u.args]]
  u is ['SubDomain,D,.] => compCategories(D,e)
  v := get(u.op,'modemap,e)
  v isnt [.,:.] =>
    error ['"compCategories: could not get proper modemap for operator",u.op]
  if rest v then
    sayBrightly ['"compCategories: ", '"%b", '"Warning", '"%d",
                 '"ignoring unexpected stuff at end of modemap"]
    pp rest v
  -- the next line "fixes" a bad modemap which sometimes appears ....
  --
  if rest v and null CAAAR v then
    v := rest v
  v := resolvePatternVars(first(v).mmSource, u.args) -- replaces #n forms
  -- select the modemap part of the first entry, and skip result etc.
  [u.op,:[compCategories1(a,b,e) for a in u.args for b in v]]
 
compCategories1(u,v,e) ==
-- v is the mode of u
  u isnt [.,:.] => u
  u is [":",x,t] => [u.op,x,compCategories1(t,v,e)]
  isCategoryForm(v,e) => compCategories(u,e)
  [c,:.] := comp(macroExpand(u,e),v,e) => c
  error 'compCategories1
 
optFunctorBody x ==
  atomic? x => x
  x is ['DomainSubstitutionMacro,parms,body] =>
    optFunctorBody DomainSubstitutionFunction(parms,body)
  x is ['%list,:l] =>
    null l => nil
    l:= [optFunctorBody u for u in l]
    every?(function optFunctorBodyQuotable,l) =>
      quote [optFunctorBodyRequote u for u in l]
    ['%list,:l]
  x is ['PROGN,:l] => ['%seq,:optFunctorPROGN l]
  x is ['%when,:l] =>
    l := [v for u in l | v := relevantClause u] where
            relevantClause u ==
              u is [pred,:conseq] =>
                u := [optFunctorBody pred,:optFunctorPROGN conseq]
                u is ['%otherwise] => nil
                u
              nil
    l = nil => nil
    CAAR l='%otherwise =>
      (null CDAR l => nil; null CDDAR l => CADAR l; ['%seq,:CDAR l])
    null rest l and null CDAR l =>
            --there is no meat to this conditional form
      pred:= CAAR l
      pred isnt [.,:.] => nil
      first pred="HasCategory" => nil
      ['%when,:l]
    ['%when,:l]
  [optFunctorBody first x,:optFunctorBody rest x]
 
optFunctorBodyQuotable u ==
  u = nil or integer? u or string? u => true
  u isnt [.,:.] => false
  u is ['QUOTE,:.] => true
  false
 
optFunctorBodyRequote u ==
  u isnt [.,:.] => u
  u is ['QUOTE,v] => v
  systemErrorHere ["optFunctorBodyRequote",u]
 
optFunctorPROGN l ==
  l is [x,:l'] =>
    worthlessCode x => optFunctorPROGN l'
    l':= optFunctorBody l'
    l' is [nil] => [optFunctorBody x]
    [optFunctorBody x,:l']
  l
 
worthlessCode x ==
  x is ['%when,:l] => and/[x is [.,y] and worthlessCode y for x in l]
  x is ['PROGN,:l] => optFunctorPROGN l = nil
  x is ['%list] => true
  x = nil => true
  false
 
cons5(p,l) ==
  l and (CAAR l = first p) => [p,: rest l]
  # l < 5 => [p,:l]
  l.rest.rest.rest.rest.rest := nil
  [p,:l]
 
SetDomainSlots124(dom,names,vals) ==
  l := pairList(names,vals)
  domainDirectory(dom) := sublisProp(l,domainDirectory dom)
  domainAttributes(dom) := sublisProp(l,domainAttributes dom)
  l := [[a,:devaluate b] for a in names for b in vals]
  domainData(dom) := applySubst(l,domainData dom)
  domainDirectory(dom) := sublisProp(l,domainDirectory dom)
 
sublisProp(subst,props) ==
  null props => nil
  [cp,:props']:= props
  (a' := inspect(cp,subst)) where
    inspect(cp is [a,cond,:l],subst) ==
      cond=true => cp
                        --keep original CONS
      cond is ['or,:x] =>
        (or/[inspect(u,subst) for u in x] => [a,true,:l]; nil)
      cond is ["has",nam,b] and (val := objectAssoc(nam,subst)) =>
        ev :=
          b is ['ATTRIBUTE,c] => HasAttribute(rest val,c)
          b is ['SIGNATURE,c] => HasSignature(rest val,c)
          isDomainForm(b,$CategoryFrame) => b=rest val
          HasCategory(rest val,b)
        ev => [a,true,:l]
        nil
      cp
  not a' => sublisProp(subst,props')
  props' := sublisProp(subst,props')
  sameObject?(a',cp) and sameObject?(props',rest props) => props
  [a',:props']
 
mkTypeForm x ==
  x isnt [.,:.] => mkDevaluate x
  x.op in '(CATEGORY mkCategory) => MKQ x
  x is [":",selector,dom] =>
    ['%list,MKQ ":",MKQ selector,mkTypeForm dom]
  x.op is '%call => ['MKQ, optCall x]
        --The previous line added JHD/BMT 20/3/84
        --Necessary for proper compilation of DPOLY SPAD
  x is [op] =>
    op in '(Join %list) => nil
    MKQ x
  ['%list,MKQ x.op,:[mkTypeForm a for a in x.args]]
 
DescendCodeAdd(db,base,flag) ==
  base isnt [.,:.] => DescendCodeVarAdd(db,base,flag)
  modemap := get(base.op,'modemap,$CategoryFrame)
  modemap = nil  =>
      if getmode(base.op,$e) is ["Mapping",target,:formalArgModes]
         then formalArgs := take(#formalArgModes,$FormalMapVariableList)
                --argument substitution if parameterized?
 
         else keyedSystemError("S2OR0001",[base.op])
      DescendCodeAdd1(db,base,flag,target,formalArgs,formalArgModes)
  for [[[.,:formalArgs],target,:formalArgModes],.] in modemap repeat
    (ans:= DescendCodeAdd1(db,base,flag,target,formalArgs,formalArgModes)) =>
      return ans
  ans
 
DescendCodeAdd1(db,base,flag,target,formalArgs,formalArgModes) ==
  slist := pairList(formalArgs,rest $addFormLhs)
         --base = comp $addFormLhs-- bound in compAdd
  e:= $e
  newModes := applySubst(slist,formalArgModes)
  or/[not comp(u,m,e) for u in rest $addFormLhs for m in newModes] =>
    return nil
      --I should check that the actual arguments are of the right type
  for u in formalArgs for m in newModes repeat
    [.,.,e]:= compMakeDeclaration(u,m,e)
      --we can not substitute in the formal arguments before we comp
      --for that may change the shape of the object, but we must before
      --we match signatures
  cat:= (compMakeCategoryObject(target,e)).expr
  instantiatedBase:= genvar()
  n:=maxIndex cat
  code:=
    [u
      for i in $NRTbase..n | cons? cat.i and cons? (sig:= first cat.i)
         and
          (u:=
            SetFunctionSlots(applySubst(slist,sig),['ELT,instantiatedBase,i],flag,
              'adding))~=nil]
     --The code from here to the end is designed to replace repeated LOAD/STORE
     --combinations (SETELT ...(ELT ..)) by MVCs where this is practicable
  copyvec := newShell (1+n)
  (for u in code repeat
      if update(u,copyvec,[]) then code := remove(code,u))
    where update(code,copyvec,sofar) ==
      code isnt [.,:.] => nil
      code.op in '(%tref ELT) =>
          copyvec.(third code):=union(copyvec.(third code), sofar)
          true
      code is ['%store,['%tref,name,number],u'] =>
        update(u',copyvec,[[name,:number],:sofar])
  for i in $NRTbase..n repeat
    for u in copyvec.i repeat
      [name,:count]:=u
      j:=i+1
      while j<= MIN(n,i+63) and LASSOC(name,copyvec.j) = count+j-i repeat j:=j+1
             --Maximum length of an MVC is 64 words
      j:=j-1
      j > i+2 =>
        for k in i..j repeat
          copyvec.k := remove(copyvec.k,[name,:count+k-i])
        code:=[["REPLACE", name, instantiatedBase,
                 KEYWORD::START1, count,
                  KEYWORD::START2, i,
                   KEYWORD::END2, j+1],:code]
    copyvec.i =>
      v:=['%tref,instantiatedBase,i]
      for u in copyvec.i repeat
        [name,:count]:=u
        v:=['%store,['%tref,name,count],v]
      code:=[v,:code]
  [["%LET",instantiatedBase,base],:code]
 
++ In a conditional branch, 'cond' is the new condition guarding
++ a branch; return an updated predicate taking into account the
++ logical combination of preceding guards and an updated
++ 'continuation predicate reflecting the new condition.
addConditionToGuard(cond,existing) ==
  TruthP cond => [existing,:existing]
  TruthP existing => [cond,:['NOT,cond]]
  [['AND,existing,cond],:['AND,existing,['NOT,cond]]]

viewsUnderCondition(views,cond) ==
  cond is ['HasCategory,dom,cat] => [[dom,:cat],:views]
  views

DescendCode(db,code,flag,viewAssoc,e) ==
  -- flag = true if we are walking down code always executed;
  -- otherwise set to conditions in which
  code = nil => nil
  code is '%noBranch => nil
  isMacro(code,e) => nil --RDJ: added 3/16/83
  code is ['add,base,:codelist] =>
    codelist:=
      [v for u in codelist | v := DescendCode(db,u,flag,viewAssoc,e)]
                  -- must do this first, to get this overriding Add code
    ['PROGN,:DescendCodeAdd(db,base,flag),:codelist]
  code is ['PROGN,:codelist] =>
    ['PROGN,:
            --Two REVERSEs leave original order, but ensure last guy wins
      reverse! [v for u in reverse codelist |
                    v := DescendCode(db,u,flag,viewAssoc,e)]]
  code is ['%when,:condlist] =>
    c:= [[u2:= ProcessCond(db,first u,e),:q] for u in condlist] where q() ==
          null u2 => nil
          [f,:flag] := addConditionToGuard(u2,flag)
          [DescendCode(db,v,f,viewsUnderCondition(viewAssoc,first u),e)
             for v in rest u]
    TruthP CAAR c => ['%seq,:CDAR c]
    while (c and (last c is [c1] or last c is [c1,[]]) and
            (c1 = '%true or c1 is ['HasAttribute,:.])) repeat
                   --strip out some worthless junk at the end
        c:=reverse! rest reverse! c
    c = nil => ['%list]
    ['%when,:c]
  code is ["%LET",name,body,:.] =>
                    --only keep the names that are useful
    u := member(name,$locals) =>
        CONTAINED('$,body) and isDomainForm(body,e) =>
          --instantiate domains which depend on $ after constants are set
          code:=['%store,['%tref,['%tref,'$,$AddChainIndex],#$locals-#u],code]
          $epilogue:=
            TruthP flag => [code,:$epilogue]
            [['%when,[ProcessCond(db,flag,e),code]],:$epilogue]
          nil
        code
    code -- doItIf deletes entries from $locals so can't optimize this
  code is ['CodeDefine,sig,implem] =>
             --Generated by doIt in COMPILER BOOT
    dom :=
      u := symbolTarget('$,viewAssoc) => ['getDomainView,'$,u]
      '$
    body:= ['%closure,implem,dom]
    SetFunctionSlots(sig,body,flag,'original)
  code is ['_:,:.] => (code.first := '%list; code.rest := nil)
      --Yes, I know that's a hack, but how else do you kill a line?
  code is ['%list,:.] => nil
  code is ['devaluate,:.] => nil
  code is ['MDEF,:.] => nil
  code is ['%call,:.] => code
  code is ['%store,:.] => code -- can be generated by doItIf
  stackWarning('"unknown Functor code: %1 ",[code])
  code
 
ProcessCond(db,cond,e) ==
  ncond := dbSubstituteFormals(db,cond)
  valuePosition(ncond,$NRTslot1PredicateList) => predicateBitRef(db,ncond,e)
  cond

TryGDC cond ==
            --sees if a condition can be optimised by the use of
            --information in $getDomainCode
  cond isnt [.,:.] => cond
  cond is ['HasCategory,:l] =>
    solved := nil
    for u in $getDomainCode while solved = nil repeat
      if u is ["%LET",name, =cond] then
        solved := name
    solved ~= nil => solved
    cond
  cond
 
findOperatorImplementations opsig ==
  if $insideCategoryPackageIfTrue then
    opsig := substitute('$,second($functorForm),opsig)
  removeDuplicates [u.mapImpl for u in $lisplibOperationAlist |
                      opsig = u.mapOpsig and u.mapImpl isnt [.,.,nil]]
 
SetFunctionSlots(sig,body,flag,mode) == --mode is either "original" or "adding"
  null body => return nil
  for catImplem in findOperatorImplementations sig repeat
    catImplem is [q,.,index] and q in '(ELT CONST) =>
      if q = 'CONST and body is ['%closure,a,b] then
         body := ['%closure,'%constant,[second a,b]]
      body:= ['%store,['%tref,'$,index],body]
      not vector? $SetFunctions => nil --packages don't set it
      TruthP vectorRef($SetFunctions,index) =>   -- the function was already assigned
        return body := nil
      vectorRef($SetFunctions,index) :=
        TruthP flag => true
        not vectorRef($SetFunctions,index) => flag
        ['_or,vectorRef($SetFunctions,index),flag]
    catImplem is ['Subsumed,:truename] =>
      mode='original =>
        truename is [fn,:.] and fn in '(Zero One) => nil --hack by RDJ 8/90
        body := SetFunctionSlots(truename,body,nil,mode)
    keyedSystemError("S2OR0002",[catImplem])
  body is ['%store,:.] => body
  nil
 
--%  Under what conditions may views exist?
 
InvestigateConditions(db,catvecListMaker,tbl,env) ==
  -- given a principal view and a list of secondary views,
  -- discover under what conditions the secondary view are
  -- always present.
  $Conditions: local:= nil
  $principal: local := nil
  [$principal,:secondaries]:= catvecListMaker
      --We are not interested in the principal view
      --The next block allows for the possibility that $principal may
      --have conditional secondary views
  null secondaries => '(T)
      --return for packages which generally have no secondary views
  if $principal is [op,:.] then
    principal' := getCategoryObject(tbl,$principal,$e)
    for u in categoryAncestors principal' repeat
      if not TruthP(cond:=second u) then
        new:=['CATEGORY,'domain,['IF,cond,['ATTRIBUTE,first u], '%noBranch]]
        $principal is ['Join,:l] =>
          not listMember?(new,l) =>
            $principal:=['Join,:l,new]
        $principal:=['Join,$principal,new]
  principal' :=
    pessimise $principal where
      pessimise a ==
        a isnt [.,:.] => a
        a is ['SIGNATURE,:.] => a
        a is ['IF,cond,:.] =>
          if not listMember?(cond,$Conditions) then
            $Conditions:= [cond,:$Conditions]
          nil
        [pessimise first a,:pessimise rest a]
  null $Conditions => [true,:[true for u in secondaries]]
  PrincipalSecondaries:= getViewsConditions(principal',tbl)
  MinimalPrimary:= first first PrincipalSecondaries
  MaximalPrimary := first categoryPrincipals dbDomainShell db
  necessarySecondaries:= [first u for u in PrincipalSecondaries | rest u=true]
  and/[listMember?(u,necessarySecondaries) for u in secondaries] =>
    [true,:[true for u in secondaries]]
  $HackSlot4:=
    MinimalPrimary=MaximalPrimary => nil
    MaximalPrimaries :=
      [MaximalPrimary,:categoryPrincipals CatEval(MaximalPrimary,tbl,env)]
    MinimalPrimaries :=
      [MinimalPrimary,:categoryPrincipals CatEval(MinimalPrimary,tbl,env)]
    MaximalPrimaries:=S_-(MaximalPrimaries,MinimalPrimaries)
    [[x] for x in MaximalPrimaries]
  ($Conditions:= Conds($principal,nil)) where
    Conds(code,previous) ==
           --each call takes a list of conditions, and returns a list
           --of refinements of that list
      code isnt [.,:.] => [previous]
      code is ['DomainSubstitutionMacro,.,b] => Conds(b,previous)
      code is ['IF,a,b,c] => union(Conds(b,[a,:previous]),Conds(c,previous))
      code is ['PROGN,:l] => "union"/[Conds(u,previous) for u in l]
      code is ['CATEGORY,:l] => "union"/[Conds(u,previous) for u in l]
      code is ['Join,:l] => "union"/[Conds(u,previous) for u in l]
      [previous]
  $Conditions := remove!([remove!(u,nil) for u in $Conditions],nil)
  partList:=
    [getViewsConditions(partPessimise($principal,cond),tbl) for cond in $Conditions]
  masterSecondaries:= secondaries
  for u in partList repeat
    for [v,:.] in u repeat
      if not listMember?(v,secondaries) then
        secondaries:= [v,:secondaries]
  list:= [listMember?(u,necessarySecondaries) for u in secondaries]
  for u in $Conditions for newS in partList repeat
    --newS is a list of secondaries and conditions (over and above
    --u) for which they apply
    u:=
      # u=1 => first u
      ['AND,:u]
    for [v,:.] in newS repeat
      for v' in [v,:categoryPrincipals CatEval(v,tbl,env)] repeat
        if (w:=assoc(v',$HackSlot4)) then
          w.rest := if rest w then mkOr(u,rest w,tbl,env) else u
    (list:= update(list,u,secondaries,newS,tbl,env)) where
      update(list,cond,secondaries,newS,tbl,env) ==
        (list2:=
          [flist(sec,newS,old,cond,tbl,env) for sec in secondaries for old in list]) where
            flist(sec,newS,old,cond,tbl,env) ==
              old=true => old
              for [newS2,:morecond] in newS repeat
                old:=
                  not ancestor?(sec,[newS2],tbl,env) => old
                  cond2 := mkAnd(cond,morecond,tbl,env)
                  null old => cond2
                  mkOr(cond2,old,tbl,env)
              old
        list2
  list:= [[sec,:ICformat(db,u,tbl,env)] for u in list for sec in secondaries]
  pv:= getPossibleViews($principal,tbl)
  -- $HackSlot4 is used in SetVector4 to ensure that conditional
  -- extensions of the principal view are handles correctly
  -- here we build the code necessary to remove spurious extensions
  ($HackSlot4:= [reshape(db,u,tbl,env) for u in $HackSlot4]) where
    reshape(db,u,tbl,env) ==
      ['%when,[TryGDC ICformat(db,rest u,tbl,env)],
             ['%otherwise,['RPLACA,'(CAR TrueDomain),
                             ['delete, quote first u,'(CAAR TrueDomain)]]]]
  $supplementaries:=
    [u
      for u in list | not listMember?(first u,masterSecondaries)
        and not (true=rest u) and not listMember?(first u,pv)]
  [true,:[LASSOC(ms,list) for ms in masterSecondaries]]
 
ICformat(db,u,tbl,env) ==
      u isnt [.,:.] => u
      u is ["has",:.] => compHasFormat(db,u,env)
      u is ['AND,:l] or u is ['and,:l] =>
        l:= removeDuplicates [ICformat(db,v,tbl,env) for [v,:l'] in tails l
                                | not listMember?(v,l')]
             -- we could have duplicates after, even if not before
        # l=1 => first l
        l1:= first l
        for u in rest l repeat
          l1 := mkAnd(u,l1,tbl,env)
        l1
      u is ['OR,:l] =>
        (l:= ORreduce l)
        # l=1 => ICformat(db,first l,tbl,env)
        l:= ORreduce removeDuplicates [ICformat(db,u,tbl,env) for u in l]
                 --causes multiple ANDs to be squashed, etc.
                 -- and duplicates that have been built up by tidying
        (l:= Hasreduce(l,tbl,env)) where
          Hasreduce(l,tbl,env) ==
            for u in l | u is ['HasCategory,name,cond] and cond is ['QUOTE,
              cond] repeat
                                  --check that v causes descendants to go
                for v in l | not (v=u) and v is ['HasCategory, =name,['QUOTE,
                  cond2]] repeat
                       if descendant?(cond,cond2,tbl,env) then l:= remove(l,u)
                       --v subsumes u
            for u in l | u is ['AND,:l'] or u is ['and,:l'] repeat
              for u' in l' | u' is ['HasCategory,name,cond] and cond is ['QUOTE,
                cond] repeat
                                    --check that v causes descendants to go
                  for v in l | v is ['HasCategory, =name,['QUOTE,
                    cond2]] repeat
                        if descendant?(cond,cond2,tbl,env) then l:= remove(l,u)
                         --v subsumes u
            l
        # l=1 => first l
        ['OR,:l]
      systemErrorHere ["ICformat",u]
  where
    ORreduce l ==
      for u in l | u is ['AND,:.] or u is ['and,:.] repeat
                            --check that B causes (and A B) to go
        for v in l | not (v=u) repeat
          if listMember?(v,u) or (and/[member(w,u) for w in v]) then l:=
            remove(l,u)
                 --v subsumes u
                     --Note that we are ignoring AND as a component.
                     --Convince yourself that this code still works
      l
 
partPessimise(a,trueconds) ==
  a isnt [.,:.] => a
  a is ['SIGNATURE,:.] => a
  a is ['IF,cond,:.] => (listMember?(cond,trueconds) => a; nil)
  [partPessimise(first a,trueconds),:partPessimise(rest a,trueconds)]
 
getPossibleViews(u,tbl) ==
  --returns a list of all the categories that can be views of this one
  vec := getCategoryObject(tbl,u,$e)
  views:= [first u for u in categoryAncestors vec]
  null vec.0 => [first categoryPrincipals vec,:views] --*
  [vec.0,:views] --*
      --the two lines marked  ensure that the principal view comes first
      --if you don't want it, rest it off
 
getViewsConditions(u,tbl) ==
  --returns a list of all the categories that can be views of this one
  --paired with the condition under which they are such views
  vec := getCategoryObject(tbl,u,$e)
  views:= [[first u,:second u] for u in categoryAncestors vec]
  null vec.0 =>
    null categoryPrincipals vec => views
    [[first categoryPrincipals vec,:true],:views] --*
  [[vec.0,:true],:views] --*
      --the two lines marked  ensure that the principal view comes first
      --if you don't want it, rest it off
 
DescendCodeVarAdd(db,base,flag) ==
   [SetFunctionSlots(sig,implem,flag,'adding) repeat
       for i in $NRTbase..maxIndex dbDomainShell db |
         categoryRef(dbDomainShell db,i) is [sig:=[op,types],:.] and
           LASSOC([base,:substitute(base,'$,types)],get(op,'modemap,$e)) is
                  [[pred,implem]]]
 
resolvePatternVars(p,args) ==
  p := applySubst(pairList($TriangleVariableList,args),p)
  applySubst(pairList($FormalMapVariableList,args),p)

--% Code Processing Packages

isCategoryPackageName nam ==
  isDefaultPackageName opOf nam

mkOperatorEntry(opSig is [op,sig,:flag],pred,count) ==
  null flag => [opSig,pred,["ELT","$",count]]
  first flag="constant" => [[op,sig],pred,["CONST","$",count]]
  systemError ["unknown variable mode: ",flag]
 
--% Code for encoding function names inside package or domain
 
++ Return the linkage name of the local operation named `op'.
encodeLocalFunctionName op ==
  prefix :=
    $prefix => $prefix
    $functorForm => symbolName dbAbbreviation constructorDB $functorForm.op
    stackAndThrow('"There is no context for local function %1b",[op]) 
  makeSymbol strconc(prefix,'";",symbolName op)
 
splitEncodedFunctionName(encodedName, sep) ==
    -- [encodedPackage, encodedItem, encodedSig, sequenceNo] or nil
    -- sep0 is the separator used in "encodeFunctionName".
    sep0 := '";"
    if not string? encodedName then
        encodedName := STRINGIMAGE encodedName
    null (p1 := STRPOS(sep0, encodedName, 0,    '"*")) => nil
    null (p2 := STRPOS(sep0, encodedName, p1+1, '"*")) => 'inner
--  This is picked up in compile for inner functions in partial compilation
    null (p3 := STRPOS(sep,  encodedName, p2+1, '"*")) => nil
    s1 := subString(encodedName, 0,    p1)
    s2 := subString(encodedName, p1+1, p2-p1-1)
    s3 := subString(encodedName, p2+1, p3-p2-1)
    s4 := subString(encodedName, p3+1)
    [s1, s2, s3, s4]
 
