-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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
import nrungo
import category
namespace BOOT

--%  Domain printing
keyItem a ==
  isDomain a => CDAR a.4
  a
  --The item that domain checks on
 
--Global strategy here is to maintain a list of substitutions
--  ( %in Sublis), of vectors and the names that they have,
--  which may be either local names ('View1') or global names ('Where1')
--  The global names are remembered on $Sublis from one
--  invocation of DomainPrint1 to the next
 
DomainPrint(D,brief) ==
  -- If brief is non-NIL, %then only a summary is printed
  $WhereList: local := nil
  $Sublis: local := nil
  $WhereCounter: local := 1
  env:=
    null $e => $EmptyEnvironment
    $e --in case we are called from top level
  isCategory D => CategoryPrint(D,env)
  $Sublis:= [[keyItem D,:'original]]
  SAY '"-----------------------------------------------------------------------"
  DomainPrint1(D,NIL,env)
  while ($WhereList) repeat
    s:= $WhereList
    $WhereList:= nil
    for u in s repeat
      TERPRI()
      SAY ['"Where ",first u,'" is:"]
      DomainPrint1(rest u,brief,env)
  SAY '"-----------------------------------------------------------------------"
 
DomainPrint1(D,brief,$e) ==
  REFVECP D and not isDomain D => PacPrint D
  if REFVECP D then D:= D.4
             --if we were passed a vector, go to the domain
  Sublis:=
    [:
      [[rest u,:INTERNL STRCONC('"View",STRINGIMAGE i)]
        for u in D for i in 1..],:$Sublis]
  for u in D for i in 1.. repeat
    brief and i>1 => nil
    uu:= COPY_-SEQ rest u
    uu.4:= '"This domain"
    if not brief then
      SAY ['"View number ",i,'" corresponding to categories:"]
      PRETTYPRINT first u
    if i=1 and REFVECP uu.5 then
      vv:= COPY_-SEQ uu.5
      uu.5:= vv
      for j in 0..MAXINDEX vv repeat
        if REFVECP vv.j then
          l:= ASSQ(keyItem vv.j,Sublis)
          if l
             then name:= rest l
             else
              name:=DPname()
              Sublis:= [[keyItem vv.j,:name],:Sublis]
              $Sublis:= [first Sublis,:$Sublis]
              $WhereList:= [[name,:vv.j],:$WhereList]
          vv.j:= name
    if i>1 then
      uu.1:= uu.2:= uu.5:= '"As in first view"
    for i in 6..MAXINDEX uu repeat
      uu.i:= DomainPrintSubst(uu.i,Sublis)
      if REFVECP uu.i then
        name:=DPname()
        Sublis:= [[keyItem uu.i,:name],:Sublis]
        $Sublis:= [first Sublis,:$Sublis]
        $WhereList:= [[name,:uu.i],:$WhereList]
        uu.i:= name
      if uu.i is [.,:v] and REFVECP v then
        name:=DPname()
        Sublis:= [[keyItem v,:name],:Sublis]
        $Sublis:= [first Sublis,:$Sublis]
        $WhereList:= [[name,:v],:$WhereList]
        uu.i:= [first uu.i,:name]
    if brief then PRETTYPRINT uu.0 else PRETTYPRINT uu
 
DPname() ==
  name:= INTERNL STRCONC('"Where",STRINGIMAGE $WhereCounter)
  $WhereCounter:= $WhereCounter+1
  name
 
PacPrint v ==
  vv:= COPY_-SEQ v
  for j in 0..MAXINDEX vv repeat
    if REFVECP vv.j then
      l:= ASSQ(keyItem vv.j,Sublis)
      if l
         then name:= rest l
         else
          name:=DPname()
          Sublis:= [[keyItem vv.j,:name],:Sublis]
          $Sublis:= [first Sublis,:$Sublis]
          $WhereList:= [[name,:vv.j],:$WhereList]
      vv.j:= name
    if PAIRP vv.j and REFVECP(u:=CDR vv.j) then
      l:= ASSQ(keyItem u,Sublis)
      if l
         then name:= rest l
         else
          name:=DPname()
          Sublis:= [[keyItem u,:name],:Sublis]
          $Sublis:= [first Sublis,:$Sublis]
          $WhereList:= [[name,:u],:$WhereList]
      RPLACD(vv.j,name)
  PRETTYPRINT vv
 
DomainPrintSubst(item,Sublis) ==
  item is [a,:b] =>
    c1:= DomainPrintSubst(a,Sublis)
    c2:= DomainPrintSubst(b,Sublis)
    EQ(c1,a) and EQ(c2,b) => item
    [c1,:c2]
  l:= ASSQ(item,Sublis)
  l => rest l
  l:= ASSQ(keyItem item,Sublis)
  l => rest l
  item
 
--%  Utilities
 
mkDevaluate a ==
  null a => nil
  a is ['QUOTE,a'] => (a' => a; nil)
  a='$ => MKQ '$
  a is ['LIST] => nil
  a is ['LIST,:.] => a
  ['devaluate,a]
 
getDomainView(domain,catform) ==
  u:= HasCategory(domain,catform) => u
  c:= eval catform
  u:= HasCategory(domain,c.0) => u
  -- note:  this is necessary because of domain == another domain, e.g.
  -- Ps are defined to be SUPs with specific arguments so that if one
  -- asks if a P is a Module over itself, here one has catform= (Module
  -- (P I)) yet domain is a SUP.  By oding this evaluation, c.0=SUP as
  -- well and test works --- RDJ 10/31/83
  throwKeyedMsg("S2IF0009",[devaluate domain, catform])
 
getPrincipalView domain ==
  pview:= domain
  for [.,:view] in domain.4 repeat if #view>#pview then pview:= view
  pview
 
CategoriesFromGDC x ==
  atom x => nil
  x is ['LIST,a,:b] and a is ['QUOTE,a'] =>
    union(LIST LIST a',"union"/[CategoriesFromGDC u for u in b])
  x is ['QUOTE,a] and a is [b] => [a]
 
compCategories u ==
  ATOM u => u
  not ATOM first u =>
    error ['"compCategories: need an atom in operator position", first u]
  first u = "Record" =>
    -- There is no modemap property for these guys so do it by hand.
    [first u, :[[":", a.1, compCategories1(a.2,'(SetCategory))] for a in rest u]]
  first u = "Union" or first u = "Mapping" =>
    -- There is no modemap property for these guys so do it by hand.
    [first u, :[compCategories1(a,'(SetCategory)) for a in rest u]]
  u is ['SubDomain,D,.] => compCategories D
  v:=get(first u,'modemap,$e)
  ATOM v =>
    error ['"compCategories: could not get proper modemap for operator",first u]
  if rest v then
    sayBrightly ['"compCategories: ", '%b, '"Warning", '%d,
                 '"ignoring unexpected stuff at end of modemap"]
    pp rest v
  -- the next line "fixes" a bad modemap which sometimes appears ....
  --
  if rest v and NULL CAAAR v then v:=CDR v
  v:= CDDAAR v
  v:=resolvePatternVars(v, rest u) -- replaces #n forms
  -- select the modemap part of the first entry, and skip result etc.
  u:=[first u,:[compCategories1(a,b) for a in rest u for b in v]]
  u
 
compCategories1(u,v) ==
-- v is the mode of u
  ATOM u => u
  isCategoryForm(v,$e) => compCategories u
  [c,:.] := comp(macroExpand(u,$e),v,$e) => c
  error 'compCategories1
 
NewbFVectorCopy(u,domName) ==
  v:= newShell SIZE u
  for i in 0..5 repeat v.i:= u.i
  for i in 6..MAXINDEX v | PAIRP u.i repeat v.i:= [function Undef,[domName,i],:first u.i]
  v
 
mkVector u ==
  u => ['VECTOR,:u]
  nil
 
optFunctorBody x ==
  atom x => x
  x is ['QUOTE,:l] => x
  x is ['DomainSubstitutionMacro,parms,body] =>
    optFunctorBody DomainSubstitutionFunction(parms,body)
  x is ['LIST,:l] =>
    null l => nil
    l:= [optFunctorBody u for u in l]
    and/[optFunctorBodyQuotable u for u in l] =>
      ['QUOTE,[optFunctorBodyRequote u for u in l]]
    l=rest x => x --CONS-saving hack
    ['LIST,:l]
  x is ['PROGN,:l] => ['PROGN,:optFunctorPROGN l]
  x is ['COND,:l] =>
    l:=
      [CondClause u for u in l | u and first u] where
        CondClause [pred,:conseq] ==
          [optFunctorBody pred,:optFunctorPROGN conseq]
    l:= EFFACE('((QUOTE T)),l)
                   --delete any trailing ("T)
    null l => nil
    CAAR l='(QUOTE T) =>
      (null CDAR l => nil; null CDDAR l => CADAR l; ["PROGN",:CDAR l])
    null rest l and null CDAR l =>
            --there is no meat to this COND
      pred:= CAAR l
      atom pred => nil
      first pred="HasCategory" => nil
      ['COND,:l]
    ['COND,:l]
  [optFunctorBody u for u in x]
 
optFunctorBodyQuotable u ==
  null u => true
  NUMBERP u => true
  atom u => nil
  u is ['QUOTE,:.] => true
  nil
 
optFunctorBodyRequote u ==
  atom u => u
  u is ['QUOTE,v] => v
  systemErrorHere ["optFunctorBodyRequote",u]
 
optFunctorPROGN l ==
  l is [x,:l'] =>
    worthlessCode x => optFunctorPROGN l'
    l':= optFunctorBody l'
    l'=[nil] => [optFunctorBody x]
    [optFunctorBody x,:l']
  l
 
worthlessCode x ==
  x is ['COND,:l] and (and/[x is [.,y] and worthlessCode y for x in l]) => true
  x is ['PROGN,:l] => (null (l':= optFunctorPROGN l) => true; false)
  x is ['LIST] => true
  null x => true
  false
 
cons5(p,l) ==
  l and (CAAR l = CAR p) => [p,: rest l]
  LENGTH l < 5 => [p,:l]
  RPLACD(QCDDDDR l,nil)
  [p,:l]
 
-- TrimEnvironment e ==
--   [TrimLocalEnvironment u for u in e] where
--     TrimLocalEnvironment e ==
--       [TrimContour u for u in e] where
--         TrimContour e ==
--           [u for u in e | Interesting u] where Interesting u == nil
--                         --clearly a temporary definition
 
setVector0(catNames,definition) ==
          --returns code to set element 0 of the vector
          --to the definition of the category
  definition:= mkDomainConstructor definition
-- If we call addMutableArg this early, then recurise calls to this domain
-- (e.g. while testing predicates) will generate new domains => trouble
--definition:= addMutableArg mkDomainConstructor definition
  for u in catNames repeat
    definition:= ["setShellEntry",u,0,definition]
  definition
 
--presence of GENSYM in arg-list differentiates mutable-domains
-- addMutableArg nameFormer ==
--   $mutableDomain =>
--     nameFormer is ['LIST,:.] => [:nameFormer, '(GENSYM)]
--     ['APPEND,nameFormer,'(LIST (GENSYM))]
--   nameFormer
 
--getname D ==
--  isDomain D or isCategory D => D.0
--  D
 
setVector12 args ==
            --The purpose of this function is to replace place holders
            --e.g. argument names or gensyms, by real values
  null args => nil
  args1:=args2:=args
  for u in $extraParms repeat
            --A typical element of $extraParms, which is set in
            --DomainSubstitutionFunction, would be (gensym) cons
            --(category parameter), e.g. DirectProduct(length vl,NNI)
            --as in DistributedMultivariatePolynomial
    args1:=[CAR u,:args1]
    args2:=[CDR u,:args2]
  freeof($domainShell.1,args1) and
      freeof($domainShell.2,args1) and
          freeof($domainShell.4,args1) => nil  
  [['SetDomainSlots124,'$,['QUOTE,args1],['LIST,:args2]]]
 where freeof(a,b) ==
         ATOM a => NULL MEMQ(a,b)
         freeof(CAR a,b) => freeof(CDR a,b)
         false
 
SetDomainSlots124(vec,names,vals) ==
  l:= PAIR(names,vals)
  vec.1:= sublisProp(l,vec.1)
  vec.2:= sublisProp(l,vec.2)
  l:= [[a,:devaluate b] for a in names for b in vals]
  vec.4:= SUBLIS(l,vec.4)
  vec.1:= SUBLIS(l,vec.1)
 
sublisProp(subst,props) ==
  null props => nil
  [cp,:props']:= props
  (a' := inspect(cp,subst)) where
    inspect(cp is [a,cond,:l],subst) ==
      cond=true => cp
                        --keep original CONS
      cond is ['or,:x] =>
        (or/[inspect(u,subst) for u in x] => [a,true,:l]; nil)
      cond is ['has,nam,b] and (val:= ASSQ(nam,subst)) =>
        ev:=
          b is ['ATTRIBUTE,c] => HasAttribute(rest val,c)
          b is ['SIGNATURE,c] => HasSignature(rest val,c)
          isDomainForm(b,$CategoryFrame) => b=rest val
          HasCategory(rest val,b)
        ev => [a,true,:l]
        nil
      cp
  not a' => sublisProp(subst,props')
  props' := sublisProp(subst,props')
  EQ(a',cp) and EQ(props',rest props) => props
  [a',:props']
 
setVector3(name,instantiator) ==
      --generates code to set element 3 of 'name' from 'instantiator'
      --element 3 is data structure representing category
      --returns a single LISP statement
  instantiator is ['DomainSubstitutionMacro,.,body] => setVector3(name,body)
  ["setShellEntry",name,3,mkDomainConstructor instantiator]
 
mkDomainFormer x ==
  if x is ['DomainSubstitutionMacro,parms,body] then
    x:=DomainSubstitutionFunction(parms,body)
    x:=SUBLIS($extraParms,x)
    --The next line ensures that only one copy of this structure will
    --appear in the BPI being generated, thus saving (some) space
  x is ['Join,:.] => ['eval,['QUOTE,x]]
  x
 
mkDomainConstructor x ==
  atom x => mkDevaluate x
  x is ['Join] => nil
  x is ['LIST] => nil
  x is ['CATEGORY,:.] => MKQ x
  x is ['mkCategory,:.] => MKQ x
  x is ['_:,selector,dom] =>
    ['LIST,MKQ '_:,MKQ selector,mkDomainConstructor dom]
  x is ['Record,:argl] =>
    ['LIST,MKQ 'Record,:[mkDomainConstructor y for y in argl]]
  x is ['Join,:argl] =>
    ['LIST,MKQ 'Join,:[mkDomainConstructor y for y in argl]]
  x is ['call,:argl] => ['MKQ, optCall x]
        --The previous line added JHD/BMT 20/3/84
        --Necessary for proper compilation of DPOLY SPAD
  x is [op] => MKQ x
  x is [op,:argl] => ['LIST,MKQ op,:[mkDomainConstructor a for a in argl]]
 
setVector4(catNames,catsig,conditions) ==
  if $HackSlot4 then
    for ["%LET",name,cond,:.] in $getDomainCode repeat
      $HackSlot4:=MSUSBT(name,cond,$HackSlot4)
  code := ["setShellEntry",'$,4,'TrueDomain]
  code:=['(%LET TrueDomain (NREVERSE TrueDomain)),:$HackSlot4,code]
  code:=
    [:
      [setVector4Onecat(u,v,w)
        for u in catNames for v in catsig for w in conditions],:code]
  ['(%LET TrueDomain NIL),:code]
 
setVector4Onecat(name,instantiator,info) ==
            --generates code to create one item in the
            --Alist representing a domain
            --returns a single LISP expression
  instantiator is ['DomainSubstitutionMacro,.,body] =>
    setVector4Onecat(name,body,info)
  data:=
       --CAR name.4 contains all the names except itself
       --hence we need to add this on, by the above CONS
    ['CONS,['CONS,mkDomainConstructor instantiator,['CAR,['ELT,name,4]]],
      name]
  data:= ['SETQ,'TrueDomain,['CONS,data,'TrueDomain]]
  TruthP info => data
  ['COND,[TryGDC PrepareConditional info,data],:
    Supplementaries(instantiator,name)] where
      Supplementaries(instantiator,name) ==
        slist:=
          [u for u in $supplementaries | AncestorP(first u,[instantiator])]
        null slist => nil
        $supplementaries:= S_-($supplementaries,slist)
        PRETTYPRINT [instantiator,'" should solve"]
        PRETTYPRINT slist
        slist:=
          [form(u,name) for u in slist] where
            form([cat,:cond],name) ==
              u:= ['QUOTE,[cat,:first (eval cat).4]]
              ['COND,[TryGDC cond,['SETQ,'TrueDomain,['CONS,['CONS,u,name],
                'TrueDomain]]]]
        LENGTH slist=1 => [CADAR slist]
                      --return a list, since it is CONSed
        slist:= ['PROGN,:slist]
        [['(QUOTE T),slist]]
 
setVector4part3(catNames,catvecList) ==
    --the names are those that will be applied to the various vectors
  generated:= nil
  for u in catvecList for uname in catNames repeat
    for v in CADDR u.4 repeat
      if w:= assoc(first v,generated)
         then RPLACD(w,[[rest v,:uname],:rest w])
         else generated:= [[first v,[rest v,:uname]],:generated]
  codeList := nil
  for [w,:u] in generated repeat
     code := compCategories w
     for v in u repeat
       code:= ["setShellEntry",rest v,first v,code]
     if CONTAINED('$,w) then $epilogue := [code,:$epilogue]
                        else codeList := [code,:codeList]
  codeList
 
PrepareConditional u == u
 
setVector5(catNames,locals) ==
  generated:= nil
  for u in locals for uname in catNames repeat
    if w:= assoc(u,generated)
       then RPLACD(w,[uname,:rest w])
       else generated:= [[u,uname],:generated]
  [(w:= mkVectorWithDeferral(first u,first rest u);
      for v in rest u repeat
         w:= ["setShellEntry",v,5,w];
        w)
          for u in generated]
 
mkVectorWithDeferral(objects,tag) ==
-- Basically a mkVector, but spots things that aren't safe to instantiate
-- and places them at the end of $ConstantAssignments, so that they get
-- called AFTER the constants of $ have been set up.   JHD 26.July.89
  ['VECTOR,:
   [if CONTAINED('$,u) then -- It's not safe to instantiate this now
      $ConstantAssignments:=[:$ConstantAssignments,
                             ["setShellEntry",
                              ["getShellEntry", tag, 5],
                                count,
                                 u]]
      []
    else u
       for u in objects for count in 0..]]
 
DescendCodeAdd(base,flag) ==
  atom base => DescendCodeVarAdd(base,flag)
  not (modemap:=get(opOf base,'modemap,$CategoryFrame)) =>
      if getmode(opOf base,$e) is ["Mapping",target,:formalArgModes]
         then formalArgs:= take(#formalArgModes,$FormalMapVariableList)
                --argument substitution if parameterized?
 
         else keyedSystemError("S2OR0001",[opOf base])
      DescendCodeAdd1(base,flag,target,formalArgs,formalArgModes)
  for [[[.,:formalArgs],target,:formalArgModes],.] in modemap repeat
    (ans:= DescendCodeAdd1(base,flag,target,formalArgs,formalArgModes))=>
      return ans
  ans
 
DescendCodeAdd1(base,flag,target,formalArgs,formalArgModes) ==
  slist:= pairList(formalArgs,rest $addFormLhs)
         --base = comp $addFormLhs-- bound in compAdd
  e:= $e
  newModes:= SUBLIS(slist,formalArgModes)
  or/[not comp(u,m,e) for u in rest $addFormLhs for m in newModes] =>
    return nil
      --I should check that the actual arguments are of the right type
  for u in formalArgs for m in newModes repeat
    [.,.,e]:= compMakeDeclaration(['_:,u,m],m,e)
      --we can not substitute in the formal arguments before we comp
      --for that may change the shape of the object, but we must before
      --we match signatures
  cat:= (compMakeCategoryObject(target,e)).expr
  instantiatedBase:= GENVAR()
  n:=MAXINDEX cat
  code:=
    [u
      for i in 6..n | not atom cat.i and not atom (sig:= first cat.i)
         and
          (u:=
            SetFunctionSlots(SUBLIS(slist,sig),['ELT,instantiatedBase,i],flag,
              'adding))^=nil]
     --The code from here to the end is designed to replace repeated LOAD/STORE
     --combinations (SETELT ...(ELT ..)) by MVCs where this is practicable
  copyvec := newShell (1+n)
  (for u in code repeat
      if update(u,copyvec,[]) then code:=delete(u,code))
    where update(code,copyvec,sofar) ==
      ATOM code =>nil
      MEMQ(QCAR code,'(getShellEntry ELT QREFELT)) =>
          copyvec.(CADDR code):=union(copyvec.(CADDR code), sofar)
          true
      code is [x,name,number,u'] and MEMQ(x,'(setShellEntry SETELT QSETREFV)) =>
        update(u',copyvec,[[name,:number],:sofar])
  for i in 6..n repeat
    for u in copyvec.i repeat
      [name,:count]:=u
      j:=i+1
      while j<= MIN(n,i+63) and LASSOC(name,copyvec.j) = count+j-i repeat j:=j+1
             --Maximum length of an MVC is 64 words
      j:=j-1
      j > i+2 =>
        for k in i..j repeat copyvec.k:=delete([name,:count+k-i],copyvec.k)
        code:=[["REPLACE", name, instantiatedBase,
                 KEYWORD::START1, count,
                  KEYWORD::START2, i,
                   KEYWORD::END2, j+1],:code]
    copyvec.i =>
      v:=["getShellEntry",instantiatedBase,i]
      for u in copyvec.i repeat
        [name,:count]:=u
        v:=["setShellEntry",name,count,v]
      code:=[v,:code]
  [["%LET",instantiatedBase,base],:code]
 
DescendCode(code,flag,viewAssoc,EnvToPass) ==
  -- flag = true if we are walking down code always executed;
  -- otherwise set to conditions in which
  code=nil => nil
  code='%noBranch => nil
  isMacro(code,$e) => nil --RDJ: added 3/16/83
  code is ['add,base,:codelist] =>
    codelist:=
      [v for u in codelist | (v:= DescendCode(u,flag,viewAssoc,EnvToPass))^=nil]
                  -- must do this first, to get this overriding Add code
    ['PROGN,:DescendCodeAdd(base,flag),:codelist]
  code is ['PROGN,:codelist] =>
    ['PROGN,:
            --Two REVERSEs leave original order, but ensure last guy wins
      NREVERSE [v for u in REVERSE codelist |
                    (v:= DescendCode(u,flag,viewAssoc,EnvToPass))^=nil]]
  code is ['COND,:condlist] =>
    c:= [[u2:= ProcessCond(first u,viewAssoc),:q] for u in condlist] where q() ==
          null u2 => nil
          f:=
            TruthP u2 => flag;
            TruthP flag =>
               flag := ['NOT,u2]
               u2
            flag := ['AND,flag,['NOT,u2]];
            ['AND,flag,u2]
          [DescendCode(v, f,
            if first u is ['HasCategory,dom,cat]
              then [[dom,:cat],:viewAssoc]
              else viewAssoc,EnvToPass) for v in rest u]
    TruthP CAAR c => ['PROGN,:CDAR c]
    while (c and (LAST c is [c1] or LAST c is [c1,[]]) and
            (c1 = '(QUOTE T) or c1 is ['HasAttribute,:.])) repeat
                   --strip out some worthless junk at the end
        c:=NREVERSE CDR NREVERSE c
    null c => '(LIST)
    ['COND,:c]
  code is ["%LET",name,body,:.] =>
                    --only keep the names that are useful
    u:=member(name,$locals) =>
        CONTAINED('$,body) and isDomainForm(body,$e) =>
          --instantiate domains which depend on $ after constants are set
          code:=["setShellEntry",["getShellEntry",'$,5],#$locals-#u,code]
          $epilogue:=
            TruthP flag => [code,:$epilogue]
            [['COND,[ProcessCond(flag,viewAssoc),code]],:$epilogue]
          nil
        code
    code -- doItIf deletes entries from $locals so can't optimize this
  code is ['CodeDefine,sig,implem] =>
             --Generated by doIt in COMPILER BOOT
    dom:= EnvToPass
    dom:=
      u:= LASSOC(dom,viewAssoc) => ['getDomainView,dom,u]
      dom
    body:= ['CONS,implem,dom]
    u:= SetFunctionSlots(sig,body,flag,'original)
    -- ??? We do not resolve default definitions, yet.
    if not $insideCategoryPackageIfTrue then
      updateCapsuleDirectory(rest u, flag)
    ConstantCreator u =>
      if not (flag=true) then u:= ['COND,[ProcessCond(flag,viewAssoc),u]]
      $ConstantAssignments:= [u,:$ConstantAssignments]
      nil
    u
  code is ['_:,:.] => (RPLACA(code,'LIST); RPLACD(code,NIL))
      --Yes, I know that's a hack, but how else do you kill a line?
  code is ['LIST,:.] => nil
  code is ['devaluate,:.] => nil
  code is ['MDEF,:.] => nil
  code is ['call,:.] => code
  code is ["setShellEntry",:.] => code -- can be generated by doItIf
  code is ['SETELT,:.] => systemErrorHere ["DescendCode",code]
  code is ['QSETREFV,:.] => systemErrorHere ["DescendCode",code]
  stackWarning('"unknown Functor code: %1 ",[code])
  code
 
ConstantCreator u ==
  null u => false
  u is [q,.,.,u'] and (q in '(setShellEntry SETELT QSETREFV)) => 
    ConstantCreator u'
  u is ['CONS,:.] => false
  true
 
ProcessCond(cond,viewassoc) ==
  ncond := SUBLIS($pairlis,cond)
  INTEGERP POSN1(ncond,$NRTslot1PredicateList) => predicateBitRef ncond
  cond

TryGDC cond ==
            --sees if a condition can be optimised by the use of
            --information in $getDomainCode
  atom cond => cond
  cond is ['HasCategory,:l] =>
    solved:= nil
    for u in $getDomainCode | not solved repeat
      if u is ["%LET",name, =cond] then solved:= name
    solved => solved
    cond
  cond
 
SetFunctionSlots(sig,body,flag,mode) == --mode is either "original" or "adding"
  catNames := ['$]
  for u in $catvecList for v in catNames repeat
    null body => return nil
    for catImplem in LookUpSigSlots(sig,u.1) repeat
      if catImplem is [q,.,index] and (q='ELT or q='CONST)
         then
          if q is 'CONST and body is ['CONS,a,b] then
             body := ['CONS,'IDENTITY,['FUNCALL,a,b]]
          body:= ["setShellEntry",v,index,body]
          if REFVECP $SetFunctions and TruthP flag then u.index:= true
                 --used by CheckVector to determine which ops are missing
          if v='$ then  -- i.e. we are looking at the principal view
            not REFVECP $SetFunctions => nil
                    --packages don't set it
            $MissingFunctionInfo.index:= flag
            TruthP $SetFunctions.index => (body:= nil; return nil)
                     -- the function was already assigned
            $SetFunctions.index:=
              TruthP flag => true
              not $SetFunctions.index=>flag --JHD didn't set $SF on this branch
              ["or",$SetFunctions.index,flag]
       else
        if catImplem is ['Subsumed,:truename]
                  --a special marker generated by SigListUnion
           then
            if mode='original 
               then if truename is [fn,:.] and MEMQ(fn,'(Zero One))
                    then nil  --hack by RDJ 8/90
                    else body:= SetFunctionSlots(truename,body,nil,mode)
               else nil
           else
             keyedSystemError("S2OR0002",[catImplem])
  body is ["setShellEntry",:.] => body
  body is ['SETELT,:.] => systemErrorHere ["SetFunctionSlots",body]
  body is ['QSETREFV,:.] => systemErrorHere ["SetFunctionSlots",body]
  nil
 
LookUpSigSlots(sig,siglist) ==
--+ must kill any implementations below of the form (ELT $ NIL)
  if $insideCategoryPackageIfTrue then
           sig := substitute('$,CADR($functorForm),sig)
  siglist := $lisplibOperationAlist
  REMDUP [implem for u in siglist | SigSlotsMatch(sig,first u,implem:=CADDR u)
              and KADDR implem]
 
SigSlotsMatch(sig,pattern,implem) ==
  sig=pattern => true
  not (LENGTH CADR sig=LENGTH CADR pattern) => nil
                       --CADR sig is the actual signature part
  not (first sig=first pattern) => nil
  pat' :=SUBSTQ($definition,'$,CADR pattern)
  sig' :=SUBSTQ($definition,'$,CADR sig)
  sig'=pat' => true
  --If we don't have this next test, then we'll recurse in SetFunctionSlots
  implem is ['Subsumed,:.] => nil
  SourceLevelSubsume(sig',pat') => true
  nil
 
CheckVector(vec,name,catvecListMaker) ==
  code:= nil
  condAlist :=
      [[a,:first b] for [.,a,:b] in $getDomainCode]
        -- used as substitution alist below
  for i in 6..MAXINDEX vec repeat
    v:= vec.i
    v=true => nil
    null v => nil
            --a domain, which setVector4part3 will fill in
    atom v => systemErrorHere ["CheckVector",v]
    atom first v =>
                  --It's a secondary view of a domain, which we
                  --must generate code to fill in
      for x in $catNames for y in catvecListMaker repeat
        if y=v then code:=
          [["setShellEntry",name,i,x],:code]
    if name='$ then
      assoc(first v,$CheckVectorList) => nil
      $CheckVectorList:=
        [[first v,:makeMissingFunctionEntry(condAlist,i)],:$CheckVectorList]
--  member(first v,$CheckVectorList) => nil
--  $CheckVectorList:= [first v,:$CheckVectorList]
  code
 
makeMissingFunctionEntry(alist,i) ==
  tran SUBLIS(alist,$MissingFunctionInfo.i) where
    tran x ==
      x is ["HasCategory",a,["QUOTE",b]] => ['has,a,b]
      x is [op,:l] and op in '(AND OR NOT) => [op,:[tran y for y in l]]
      x
 
--%  Under what conditions may views exist?
 
InvestigateConditions catvecListMaker ==
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
    [principal',:.]:=compMakeCategoryObject($principal,$e)
              --Rather like eval, but quotes parameters first
    for u in CADR principal'.4 repeat
      if not TruthP(cond:=CADR u) then
        new:=['CATEGORY,'domain,['IF,cond,['ATTRIBUTE,CAR u], '%noBranch]]
        $principal is ['Join,:l] =>
          not member(new,l) =>
            $principal:=['Join,:l,new]
        $principal:=['Join,$principal,new]
  principal' :=
    pessimise $principal where
      pessimise a ==
        atom a => a
        a is ['SIGNATURE,:.] => a
        a is ['IF,cond,:.] =>
          if not member(cond,$Conditions) then $Conditions:= [cond,:$Conditions]
          nil
        [pessimise first a,:pessimise rest a]
  null $Conditions => [true,:[true for u in secondaries]]
  PrincipalSecondaries:= getViewsConditions principal'
  MinimalPrimary:= CAR first PrincipalSecondaries
  MaximalPrimary:= CAAR $domainShell.4
  necessarySecondaries:= [first u for u in PrincipalSecondaries | rest u=true]
  and/[member(u,necessarySecondaries) for u in secondaries] =>
    [true,:[true for u in secondaries]]
  $HackSlot4:=
    MinimalPrimary=MaximalPrimary => nil
    MaximalPrimaries:=[MaximalPrimary,:CAR (CatEval MaximalPrimary).4]
    MinimalPrimaries:=[MinimalPrimary,:CAR (CatEval MinimalPrimary).4]
    MaximalPrimaries:=S_-(MaximalPrimaries,MinimalPrimaries)
    [[x] for x in MaximalPrimaries]
  ($Conditions:= Conds($principal,nil)) where
    Conds(code,previous) ==
           --each call takes a list of conditions, and returns a list
           --of refinements of that list
      atom code => [previous]
      code is ['DomainSubstitutionMacro,.,b] => Conds(b,previous)
      code is ['IF,a,b,c] => union(Conds(b,[a,:previous]),Conds(c,previous))
      code is ['PROGN,:l] => "union"/[Conds(u,previous) for u in l]
      code is ['CATEGORY,:l] => "union"/[Conds(u,previous) for u in l]
      code is ['Join,:l] => "union"/[Conds(u,previous) for u in l]
      [previous]
  $Conditions:= EFFACE(nil,[EFFACE(nil,u) for u in $Conditions])
  partList:=
    [getViewsConditions partPessimise($principal,cond) for cond in $Conditions]
  masterSecondaries:= secondaries
  for u in partList repeat
    for [v,:.] in u repeat
      if not member(v,secondaries) then secondaries:= [v,:secondaries]
  (list:= [mkNilT member(u,necessarySecondaries) for u in secondaries]) where
    mkNilT u ==
      u => true
      nil
  for u in $Conditions for newS in partList repeat
    --newS is a list of secondaries and conditions (over and above
    --u) for which they apply
    u:=
      LENGTH u=1 => first u
      ['AND,:u]
    for [v,:.] in newS repeat
      for v' in [v,:CAR (CatEval v).4] repeat
        if (w:=assoc(v',$HackSlot4)) then
          RPLAC(rest w,if rest w then mkOr(u,rest w) else u)
    (list:= update(list,u,secondaries,newS)) where
      update(list,cond,secondaries,newS) ==
        (list2:=
          [flist(sec,newS,old,cond) for sec in secondaries for old in list]) where
            flist(sec,newS,old,cond) ==
              old=true => old
              for [newS2,:morecond] in newS repeat
                old:=
                  not AncestorP(sec,[newS2]) => old
                  cond2:= mkAnd(cond,morecond)
                  null old => cond2
                  mkOr(cond2,old)
              old
        list2
  list:= [[sec,:ICformat u] for u in list for sec in secondaries]
  pv:= getPossibleViews $principal
-- $HackSlot4 is used in SetVector4 to ensure that conditional
-- extensions of the principal view are handles correctly
-- here we build the code necessary to remove spurious extensions
  ($HackSlot4:= [reshape u for u in $HackSlot4]) where
    reshape u ==
      ['COND,[TryGDC ICformat rest u],
             ['(QUOTE T),['RPLACA,'(CAR TrueDomain),
                             ['delete,['QUOTE,first u],'(CAAR TrueDomain)]]]]
  $supplementaries:=
    [u
      for u in list | not member(first u,masterSecondaries)
        and not (true=rest u) and not member(first u,pv)]
  [true,:[LASSOC(ms,list) for ms in masterSecondaries]]
 
ICformat u ==
      atom u => u
      u is ['has,:.] => compHasFormat u
      u is ['AND,:l] or u is ['and,:l] =>
        l:= REMDUP [ICformat v for [v,:l'] in tails l | not member(v,l')]
             -- we could have duplicates after, even if not before
        LENGTH l=1 => first l
        l1:= first l
        for u in rest l repeat
          l1:=mkAnd(u,l1)
        l1
      u is ['OR,:l] =>
        (l:= ORreduce l)
        LENGTH l=1 => ICformat first l
        l:= ORreduce REMDUP [ICformat u for u in l]
                 --causes multiple ANDs to be squashed, etc.
                 -- and duplicates that have been built up by tidying
        (l:= Hasreduce l) where
          Hasreduce l ==
            for u in l | u is ['HasCategory,name,cond] and cond is ['QUOTE,
              cond] repeat
                                  --check that v causes descendants to go
                for v in l | not (v=u) and v is ['HasCategory, =name,['QUOTE,
                  cond2]] repeat if DescendantP(cond,cond2) then l:= delete(u,l)
                       --v subsumes u
            for u in l | u is ['AND,:l'] or u is ['and,:l'] repeat
              for u' in l' | u' is ['HasCategory,name,cond] and cond is ['QUOTE,
                cond] repeat
                                    --check that v causes descendants to go
                  for v in l | v is ['HasCategory, =name,['QUOTE,
                    cond2]] repeat if DescendantP(cond,cond2) then l:= delete(u,l)
                         --v subsumes u
            l
        LENGTH l=1 => first l
        ['OR,:l]
      systemErrorHere ["ICformat",u]
  where
    ORreduce l ==
      for u in l | u is ['AND,:.] or u is ['and,:.] repeat
                            --check that B causes (and A B) to go
        for v in l | not (v=u) repeat
          if member(v,u) or (and/[member(w,u) for w in v]) then l:=
            delete(u,l)
                 --v subsumes u
                     --Note that we are ignoring AND as a component.
                     --Convince yourself that this code still works
      l
 
partPessimise(a,trueconds) ==
  atom a => a
  a is ['SIGNATURE,:.] => a
  a is ['IF,cond,:.] => (member(cond,trueconds) => a; nil)
  [partPessimise(first a,trueconds),:partPessimise(rest a,trueconds)]
 
getPossibleViews u ==
  --returns a list of all the categories that can be views of this one
  [vec,:.]:= compMakeCategoryObject(u,$e) or
    systemErrorHere ["getPossibleViews",u]
  views:= [first u for u in CADR vec.4]
  null vec.0 => [CAAR vec.4,:views] --*
  [vec.0,:views] --*
      --the two lines marked  ensure that the principal view comes first
      --if you don't want it, CDR it off
 
getViewsConditions u ==
 
  --returns a list of all the categories that can be views of this one
  --paired with the condition under which they are such views
  [vec,:.]:= compMakeCategoryObject(u,$e) or
    systemErrorHere ["getViewsConditions",u]
  views:= [[first u,:CADR u] for u in CADR vec.4]
  null vec.0 =>
    null CAR vec.4 => views
    [[CAAR vec.4,:true],:views] --*
  [[vec.0,:true],:views] --*
      --the two lines marked  ensure that the principal view comes first
      --if you don't want it, CDR it off
 
DescendCodeVarAdd(base,flag) ==
   princview := CAR $catvecList
   [SetFunctionSlots(sig,substitute('ELT,'CONST,implem),flag,'adding) repeat
       for i in 6..MAXINDEX princview |
         princview.i is [sig:=[op,types],:.] and
           LASSOC([base,:substitute(base,'$,types)],get(op,'modemap,$e)) is
                  [[pred,implem]]]
 
resolvePatternVars(p,args) ==
  p := SUBLISLIS(args, $TriangleVariableList, p)
  SUBLISLIS(args, $FormalMapVariableList, p)

--resolvePatternVars(p,args) ==
--  atom p =>
--    isSharpVarWithNum p => args.(position(p,$FormalMapVariableList))
--    p
--  [resolvePatternVars(CAR p,args),:resolvePatternVars(CDR p,args)]
 
-- Mysterious JENKS definition follows:
--DescendCodeVarAdd(base,flag) ==
--  baseops := [(u:=LASSOC([base,:SUBST(base,'$,types)],
--                    get(op,'modemap,$e))) and [sig,:u]
--                       for (sig := [op,types]) in $CheckVectorList]
--  $CheckVectorList := [sig for sig in $CheckVectorList
--                           for op in baseops | null op]
--  [SetFunctionSlots(sig,implem,flag,'adding)
--                   for u in baseops | u is [sig,[pred,implem]]]
 

--% Code Processing Packages

isPackageFunction() ==
  -- called by compile/putInLocalDomainReferences
  nil

isCategoryPackageName nam ==
  p := PNAME opOf nam
  p.(MAXINDEX p) = char '_&

mkOperatorEntry(opSig is [op,sig,:flag],pred,count) ==
  null flag => [opSig,pred,["ELT","$",count]]
  first flag="constant" => [[op,sig],pred,["CONST","$",count]]
  systemError ["unknown variable mode: ",flag]
 
--% Code for encoding function names inside package or domain
 
encodeFunctionName(fun,package is [packageName,:arglist],signature,sep,count)
   ==
    signature':= MSUBST("$",package,signature)
    reducedSig:= mkRepititionAssoc [:rest signature',first signature']
    encodedSig:=
      ("STRCONC"/[encodedPair for [n,:x] in reducedSig]) where
        encodedPair() ==
          n=1 => encodeItem x
          STRCONC(STRINGIMAGE n,encodeItem x)
    encodedName:= INTERNL(getAbbreviation(packageName,#arglist),";",
        encodeItem fun,";",encodedSig, sep,STRINGIMAGE count)
    if $LISPLIB then
      $lisplibSignatureAlist:=
        [[encodedName,:signature'],:$lisplibSignatureAlist]
    encodedName
 
splitEncodedFunctionName(encodedName, sep) ==
    -- [encodedPackage, encodedItem, encodedSig, sequenceNo] or NIL
    -- sep0 is the separator used in "encodeFunctionName".
    sep0 := '";"
    if not STRINGP encodedName then
        encodedName := STRINGIMAGE encodedName
    null (p1 := STRPOS(sep0, encodedName, 0,    '"*")) => nil
    null (p2 := STRPOS(sep0, encodedName, p1+1, '"*")) => 'inner
--  This is picked up in compile for inner functions in partial compilation
    null (p3 := STRPOS(sep,  encodedName, p2+1, '"*")) => nil
    s1 := SUBSTRING(encodedName, 0,    p1)
    s2 := SUBSTRING(encodedName, p1+1, p2-p1-1)
    s3 := SUBSTRING(encodedName, p2+1, p3-p2-1)
    s4 := SUBSTRING(encodedName, p3+1, nil)
    [s1, s2, s3, s4]
 
mkRepititionAssoc l ==
  mkRepfun(l,1) where
    mkRepfun(l,n) ==
      null l => nil
      l is [x] => [[n,:x]]
      l is [x, =x,:l'] => mkRepfun(rest l,n+1)
      [[n,:first l],:mkRepfun(rest l,1)]
 
encodeItem x ==
  x is [op,:argl] => getCaps op
  IDENTP x => PNAME x
  STRINGIMAGE x
 
getCaps x ==
  s:= STRINGIMAGE x
  clist:= [c for i in 0..MAXINDEX s | UPPER_-CASE_-P (c:= s.i)]
  null clist => '"__"
  "STRCONC"/[first clist,:[L_-CASE u for u in rest clist]]
 
--% abbreviation code
 
getAbbreviation(name,c) ==
  --returns abbreviation of name with c arguments
  x := constructor? name
  X := ASSQ(x,$abbreviationTable) =>
    N:= ASSQ(name,rest X) =>
      C:= ASSQ(c,rest N) => rest C --already there
      newAbbreviation:= mkAbbrev(X,x)
      RPLAC(rest N,[[c,:newAbbreviation],:rest N])
      newAbbreviation
    newAbbreviation:= mkAbbrev(X,x)
    RPLAC(rest X,[[name,[c,:newAbbreviation]],:rest X])
    newAbbreviation
  $abbreviationTable:= [[x,[name,[c,:x]]],:$abbreviationTable]
  x
 
mkAbbrev(X,x) == addSuffix(alistSize rest X,x)
 
alistSize c ==
  count(c,1) where
    count(x,level) ==
      level=2 => #x
      null x => 0
      count(CDAR x,level+1)+count(rest x,level)
 
addSuffix(n,u) ==
  ALPHA_-CHAR_-P (s:= STRINGIMAGE u).(MAXINDEX s) => 
    INTERN STRCONC(s,STRINGIMAGE n)
  INTERNL STRCONC(s,STRINGIMAGE ";",STRINGIMAGE n)
 
