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
import g_-cndata
namespace BOOT

--%

++ List of global attributes.
$Attributes := []

$NewCatVec := nil

--%

++ Returns true if `a' is a category (runtime) object.
categoryObject?: %Thing -> %Boolean
categoryObject? a == 
  vector? a and #a > 5 and vectorRef(a,3) = $Category

++ Return true if the form `x' designates an instantiaion of a
++ category constructor known to the global database or the
++ envronement `e'.
isCategoryForm: (%Form,%Env) -> %Boolean
isCategoryForm(x,e) ==
  x isnt [.,:.] =>
    u := macroExpand(x,e)
    cons? u and categoryForm? u
  categoryForm? x
 
++ Returns a freshly built category object for a domain or package
++ (as indicated by `domainOrPackage'), with signature list
++ designated by `sigList', attribute list designated by `attList',
++ used domains list designated by `domList', and a princical ancestor
++ category object designated by `PrincipalAncestor'.
mkCategory: (%ConstructorKind,%List %Sig,%List %Form,%List %Instantiation, %Maybe %Shell) -> %Shell
mkCategory(domainOrPackage,sigList,attList,domList,PrincipalAncestor) ==
  NSigList := nil
  -- Unless extending a principal ancestor (from the end), start
  -- from the first free, unencumbered slot.
  count := 
    PrincipalAncestor = nil => $NRTbase
    #PrincipalAncestor
  sigList:=
    [if s is [sig,pred]
       then
         -- Don't retain duplicate signatures.
         -- ??? Should we not check for predicate subsumption too?
         or/[x is [[ =sig,.,:impl],:num] for x in NSigList] => [sig,pred,:impl]
                 --only needed for multiple copies of sig
         num :=
           domainOrPackage is "domain" => count
           count-5
         nsig := mkOperatorEntry(sig,pred,num)
         NSigList := [[nsig,:count],:NSigList]
         count := count+1
         nsig
     else s for s in sigList]
  NewLocals:= nil
  for s in sigList repeat
    NewLocals:= union(NewLocals,Prepare s.mmTarget) where
      Prepare u == "union"/[Prepare2 v for v in u]
      Prepare2 v ==
        v is '$ => nil
        string? v => nil
        v isnt [.,:.] => [v]
        v.op is 'Union =>
          "union"/[Prepare2 x for x in stripUnionTags v.args]
        v.op is 'Mapping => "union"/[Prepare2 x for x in v.args]
        v.op is 'Record => "union"/[Prepare2 third x for x in v.args]
        v.op is 'Enumeration => nil
        [v]
  OldLocals:= nil
  -- Remove possible duplicate local domain caches.
  if PrincipalAncestor then 
    for u in (OldLocals := categoryLocals PrincipalAncestor) repeat 
      NewLocals := remove(NewLocals,first u)
  -- New local domains caches are hosted in slots at the end onward
  for u in NewLocals repeat
    OldLocals := [[u,:count],:OldLocals]
    count := count+1
  -- Build a fresh category object stuffed with all updated information
  v := newShell count
  canonicalForm(v) := nil
  categoryExports(v) := sigList
  categoryAttributes(v) := attList
  categoryRef(v,3) := $Category
  if PrincipalAncestor ~= nil then
    for x in 6..#PrincipalAncestor-1 repeat 
      categoryRef(v,x) := PrincipalAncestor.x
    categoryAssociatedTypes(v) :=
      [categoryPrincipals PrincipalAncestor,
        categoryAncestors PrincipalAncestor,
          OldLocals]
  else
    categoryAssociatedTypes(v) := [nil,nil,OldLocals]
  categoryRef(v,5) := domList
  for [nsig,:n] in NSigList repeat 
    categoryRef(v,n) := nsig
  v

--% Subsumption code (for operators)
 
DropImplementations a ==
  a is [sig,pred,[q,:.]] and q in '(ELT CONST) =>
    q = "ELT" => [sig,pred]
    [[:sig,'constant],pred]
  a
 
SigListUnion(extra,original) ==
  --augments original %with everything in extra that is not in original
  for (o:=[[ofn,osig,:.],opred,:.]) in original repeat
    -- The purpose of this loop is to detect cases when the
    -- original list contains, e.g. ** with NonNegativeIntegers, and
    -- the extra list would like to add ** with PositiveIntegers.
    -- The PI map is therefore gives an implementation of "Subsumed"
    for x in SigListOpSubsume(o,extra) repeat
      [[xfn,xsig,:.],xpred,:.] := x
      symbolEq?(xfn,ofn) and xsig = osig =>
              --checking name and signature, but not a 'constant' marker
        xpred = opred => extra := remove(extra,x)
             --same signature and same predicate
        opred = true => extra := remove(extra,x)
   -- PRETTYPRINT ("we ought to subsume",x,o)
      not MachineLevelSubsume(first o,first x) =>
         '"Source level subsumption not implemented"
      extra := remove(extra,x)
  for e in extra repeat
    [esig,epred,:.]:= e
    eimplem:=[]
    for x in SigListOpSubsume(e,original) repeat
        --PRETTYPRINT(LIST("SigListOpSubsume",e,x))
      not MachineLevelSubsume(first e,first x) =>
        --systemError '"Source level subsumption not implemented"
        original:= [e,:original]
        return nil -- this exits from the innermost for loop
      original := remove(original,x)
      [xsig,xpred,:ximplem]:= x
--      if xsig ~= esig then   -- not quite strong enough
      if first xsig ~= first esig or second xsig ~= second esig then
-- the new version won't get confused by "constant"markers
         if ximplem is [["Subsumed",:.],:.] then
            original := [x,:original]
          else
            original:= [[xsig,xpred,["Subsumed",:esig]],:original]
       else epred:=mkOr(epred,xpred)
-- this used always to be done, as noted below, but that's not safe
      if not(ximplem is [["Subsumed",:.],:.]) then eimplem:= ximplem
      if eimplem then esig:=[first esig,second esig] 
           -- in case there's a constant marker
      e:= [esig,epred,:eimplem]
--    e:= [esig,mkOr(xpred,epred),:ximplem]
-- Original version -gets it wrong if the new operator is only
-- present under certain conditions
        -- We must pick up the previous implementation, if any
--+
      if ximplem is [[q,.,index]] and integer? index and (q="ELT" or q="CONST")
        then $NewCatVec. index:= e
    original:= [e,:original]
  original
 
mkOr: (%Form,%Form) -> %Form
mkOr(a,b) ==
  a=true => true
  b=true => true
  b=a => a
  l:=
    a is ["OR",:a'] =>
      (b is ["OR",:b'] => union(a',b'); mkOr2(b,a') )
    b is ["OR",:b'] => mkOr2(a,b')
    (a is ["has",avar,acat]) and (b is ["has",=avar,bcat]) =>
      DescendantP(acat,bcat) => [b]
      DescendantP(bcat,acat) => [a]
      [a,b]
    a is ['AND,:a'] and listMember?(b,a') => [b]
    b is ['AND,:b'] and listMember?(a,b') => [a]
    a is ["and",:a'] and listMember?(b,a') => [b]
    b is ["and",:b'] and listMember?(a,b') => [a]
    [a,b]
  #l = 1 => first l
  ["OR",:l]
 
mkOr2: (%Form,%Form) -> %Form
mkOr2(a,b) ==
  --a is a condition, "b" a list of them
  listMember?(a,b) => b
  a is ["has",avar,acat] =>
    aRedundant:=false
    for c in b | c is ["has",=avar,ccat] repeat
      DescendantP(acat,ccat) =>
        return (aRedundant:=true)
      if DescendantP(ccat,acat) then b := remove(b,c)
    aRedundant => b
    [a,:b]
  [a,:b]
 
mkAnd: (%Form,%Form) -> %Form
mkAnd(a,b) ==
  a=true => b
  b=true => a
  b=a => a
  l:=
    a is ["AND",:a'] =>
      (b is ["AND",:b'] => union(a',b'); mkAnd2(b,a') )
    b is ["AND",:b'] => mkAnd2(a,b')
    (a is ["has",avar,acat]) and (b is ["has",=avar,bcat]) =>
      DescendantP(acat,bcat) => [a]
      DescendantP(bcat,acat) => [b]
      [a,b]
    [a,b]
  #l = 1 => first l
  ["AND",:l]
 
mkAnd2: (%Form,%Form) -> %Form
mkAnd2(a,b) ==
  --a is a condition, "b" a list of them
  listMember?(a,b) => b
  a is ["has",avar,acat] =>
    aRedundant:=false
    for c in b | c is ["has",=avar,ccat] repeat
      DescendantP(ccat,acat) =>
        return (aRedundant:=true)
      if DescendantP(acat,ccat) then b := remove(b,c)
    aRedundant => b
    [a,:b]
  [a,:b]
 
SigListMember(m,list) ==
  list=nil => false
  SigEqual(m,first list) => true
  SigListMember(m,rest list)
 
SigEqual([sig1,pred1,:.],[sig2,pred2,:.]) ==
  -- Notice asymmetry: checks that arg1 is a consequence of arg2
  sig1=sig2 and PredImplies(pred2,pred1)
 
PredImplies(a,b) ==
    --true if a => b in the sense of logical implication
--a = "true" => true
  a=true => true
  a=b => true
  false         -- added by RDJ: 12/21/82
--error()       -- for the time being
 
SigListOpSubsume([[name1,sig1,:.],:.],list) ==
  --does m subsume another operator in the list?
        --see "operator subsumption" in JHD's report
        --if it does, returns the subsumed member
  lsig1 := #sig1
  ans := []
  for (n:=[[name2,sig2,:.],:.]) in list | symbolEq?(name1,name2) repeat
    lsig1 = #sig2 and sig1 = sig2 => ans := [n,:ans]
  return ans
 
MachineLevelSubsume([name1,[out1,:in1],:flag1],[name2,[out2,:in2],:flag2]) ==
  -- Checks for machine-level subsumption
  --  true if the first signature subsumes the second
  --  flag1 = flag2 and: this really should be checked, but
  symbolEq?(name1,name2) and MachineLevelSubset(out1,out2) and
    (and/[MachineLevelSubset(inarg2,inarg1) for inarg1 in in1 for inarg2 in in2]
      )
 
MachineLevelSubset(a,b) ==
  --true if a is a machine-level subset of b
  a=b => true
  b is ["Union",:blist] and listMember?(a,blist) and
    (and/[string? x for x in blist | x~=a]) => true
           --all other branches must be distinct objects
  not null isSubDomain(a,b)
             --we assume all subsets are true at the machine level
 
--% Ancestor chasing code
 
FindFundAncs l ==
  --l is a list of categories and associated conditions (a list of 2-lists
  --returns a list of them and all their fundamental ancestors
  --also as two-lists with the appropriate conditions
  l=nil => nil
  f1:= CatEval CAAR l
  canonicalForm f1 = nil => FindFundAncs rest l
  ans:= FindFundAncs rest l
  for u in FindFundAncs [[CatEval first x,mkAnd(CADAR l,second x)]
   for x in categoryAncestors f1] repeat
    x:= ASSQ(first u,ans) =>
      ans:= [[first u,mkOr(second x,second u)],:remove(ans,x)]
    ans:= [u,:ans]
        --testing to see if first l is already there
  x:= ASSQ(CAAR l,ans) => [[CAAR l,mkOr(CADAR l,second x)],:remove(ans,x)]
  CADAR l=true =>
    for x in categoryPrincipals f1 repeat
      if y:= ASSQ(CatEval x,ans) then ans := remove(ans,y)
    [first l,:ans]
  for x in categoryPrincipals f1 repeat
    if y:= ASSQ(CatEval x,ans) then ans:=
      [[first y,mkOr(CADAR l,second y)],:remove(ans,y)]
  [first l,:ans]
  -- Our new thing may have, as an alternate view, a principal
  -- descendant of something previously added which is therefore
  -- subsumed
 
CatEval: %Thing -> %Shell
CatEval x ==
  vector? x => x
  e := 
    $InteractiveMode => $CategoryFrame
    $e
  compMakeCategoryObject(x,e).expr
 
AncestorP: (%Form, %List %Instantiation) -> %Form
AncestorP(xname,leaves) ==
  -- checks for being a principal ancestor of one of the leaves
  listMember?(xname,leaves) => xname
  for y in leaves repeat
    listMember?(xname,categoryPrincipals CatEval y) => return y
 
CondAncestorP(xname,leaves,condition) ==
  -- checks for being a principal ancestor of one of the leaves
  for u in leaves repeat
    u':=first u
    ucond:=
      null rest u => true
      second u
    xname = u' or listMember?(xname,categoryPrincipals CatEval u') =>
      PredImplies(ucond,condition) => return u'


++ Returns true if the form `a' designates a category that is any 
++ kind of descendant of the category designated by the form `b'.
DescendantP: (%Form,%Form) -> %Boolean
DescendantP(a,b) ==
  a=b => true
  a is ["ATTRIBUTE",:.] => false
  a is ["SIGNATURE",:.] => false
  a:= CatEval a
  b is ["ATTRIBUTE",b'] =>
    (l:=assoc(b',a.2)) => TruthP second l
  listMember?(b,categoryPrincipals a) => true
  AncestorP(b,[first u for u in categoryAncestors a]) => true
  false
 
--% The implementation of Join
 
JoinInner(l,$e) ==
  $NewCatVec: local := nil
  CondList:= nil
  for u in l repeat
    for at in u.2 repeat
      at2:= first at
      if at2 isnt [.,:.] then at2 := [at2]
        -- the variable $Attributes is built globally, so that true
        -- attributes can be detected without calling isCategoryForm
      symbolMember?(first at2,$Attributes) => nil
      null isCategoryForm(at2,$e) =>
        $Attributes:=[first at2,:$Attributes]
        nil
      pred:= second at
        -- The predicate under which this category is conditional
      listMember?(pred,get("$Information","special",$e)) =>
        l:= [:l,CatEval at2]
          --It's true, so we add this as unconditional
      not (pred is ["and",:.]) => CondList:= [[CatEval at2,pred],:CondList]
      pred':=
        [u
          for u in rest pred | not listMember?(u,get("$Information","special",$e))
            and not (u=true)]
      null pred' => l:= [:l,CatEval at2]
      # pred'=1 => CondList:= [[CatEval at2,pred'],:CondList]
      CondList:= [[CatEval at2,["and",:pred']],:CondList]
  [$NewCatVec,:l]:= l
  l':= [:CondList,:[[u,true] for u in l]]
    -- This is a list of all the categories that this extends
    -- conditionally or unconditionally
  sigl := categoryExports $NewCatVec
  attl:= $NewCatVec.2
  globalDomains:= $NewCatVec.5
  FundamentalAncestors := categoryAncestors $NewCatVec
  if $NewCatVec.0 then FundamentalAncestors:=
    [[$NewCatVec.0],:FundamentalAncestors]
                    --principal ancestor . all those already included
  copied:= nil
  -- we can not decide to extend the vector in multiple ways
  -- this flag helps us detect this case
  originalVector := false
    -- this skips buggy code which discards needed categories
  for [b,condition] in FindFundAncs l' repeat
      --This loop implements Category Subsumption
          --as described in JHD's report
    if not (b.0=nil) then
                   --It's a named category
      bname:= b.0
      CondAncestorP(bname,FundamentalAncestors,condition) => nil
      (f:=AncestorP(bname,[first u for u in FundamentalAncestors])) =>
        [.,.,index]:=assoc(f,FundamentalAncestors)
        FundamentalAncestors:=[[bname,condition,index],:FundamentalAncestors]
      PrinAncb := categoryPrincipals CatEval bname
               --Principal Ancestors of b
      reallynew:= true
      for anc in FundamentalAncestors repeat
        if listMember?(first anc,PrinAncb) then
                  --This is the check for "Category Subsumption"
          if rest anc
             then (anccond:= second anc; ancindex:= third anc)
             else (anccond:= true; ancindex:= nil)
          if PredImplies(condition,anccond)
             then FundamentalAncestors:=
 
               -- the new 'b' is more often true than the old one 'anc'
              [[bname,condition,ancindex],:remove(FundamentalAncestors,anc)]
           else
            if ancindex and (PredImplies(anccond,condition); true)
-- I have no idea who effectively commented out the predImplies
-- JHD 25/8/86
               then
                     --the new 'b' is less often true
                newentry:=[bname,condition,ancindex]
                if not listMember?(newentry,FundamentalAncestors) then
                  FundamentalAncestors:= [newentry,:FundamentalAncestors]
             else ancindex:= nil
          if not copied then
            $NewCatVec:= copyVector $NewCatVec
            copied:= true
          if ancindex
             then ($NewCatVec.ancindex:= bname; reallynew:= nil)
             else
              if originalVector and (condition=true) then
                $NewCatVec:= CatEval bname
                copied:= nil
                FundamentalAncestors:= [[bname],:categoryAncestors $NewCatVec]
                         --bname is Principal, so comes first
                reallynew:= nil
                objectMember?(b,l) =>
                  --objectMember? since category vectors are guaranteed unique
                  (sigl:= categoryExports $NewCatVec; attl:= $NewCatVec.2; l:= remove(l,b))
             --     SAY("domain ",bname," subsumes")
             --     SAY("adding a conditional domain ",
             --         bname,
             --         " replacing",
             --         first anc)
                bCond:= ASSQ(b,CondList)
                CondList := remove(CondList,bCond)
             -- value of bCond not used and could be nil
             -- bCond:= second bCond
                globalDomains:= $NewCatVec.5
                for u in categoryExports $NewCatVec repeat
                  if not listMember?(u,sigl) then
                    [s,c,i]:= u
                    if c=true
                       then sigl:= [[s,condition,i],:sigl]
                       else sigl:= [[s,["and",condition,c],i],:sigl]
                for u in $NewCatVec.2 repeat
                  if not listMember?(u,attl) then
                    [a,c]:= u
                    if c=true
                       then attl:= [[a,condition],:attl]
                       else attl:= [[a,["and",condition,c]],:attl]
      if reallynew then
        n:= # $NewCatVec
        FundamentalAncestors:= [[b.0,condition,n],:FundamentalAncestors]
        $NewCatVec:= LENGTHENVEC($NewCatVec,n+1)
-- We need to copy the vector otherwise the FundamentalAncestors
-- list will get stepped on while compiling "If R has ... " code
-- Camm Maguire July 26, 2003
--        copied:= true
        copied:= false
        originalvector:= false
        $NewCatVec.n:= b.0
  if not copied then
    $NewCatVec:= copyVector $NewCatVec
    -- It is important to copy the vector now,
    -- in case SigListUnion alters it while
    -- performing Operator Subsumption
  for b in l repeat
    sigl:= SigListUnion([DropImplementations u for u in categoryExports b],sigl)
    attl:=
-- next two lines are merely performance improvements
      symbolMember?(attl,b.2) => b.2
      symbolMember?(b.2,attl) => attl
      S_+(b.2,attl)
    globalDomains:= [:globalDomains,:S_-(b.5,globalDomains)]
  for b in CondList repeat
    newpred:= second b
    for u in (first b).2 repeat
      v:= assoc(first u,attl)
      null v =>
        attl:=
          second u=true => [[first u,newpred],:attl]
          [[first u,["and",newpred,second u]],:attl]
      second v=true => nil
      attl:= remove(attl,v)
      attl:=
        second u=true => [[first u,mkOr(second v,newpred)],:attl]
        [[first u,mkOr(second v,mkAnd(newpred,second u))],:attl]
    sigl:=
      SigListUnion(
        [AddPredicate(DropImplementations u,newpred) for u in categoryExports(first b)],sigl) where
          AddPredicate(op is [sig,oldpred,:implem],newpred) ==
            newpred=true => op
            oldpred=true => [sig,newpred,:implem]
            [sig,mkpf([oldpred,newpred],"and"),:implem]
  FundamentalAncestors:= [x for x in FundamentalAncestors | rest x]
               --strip out the pointer to Principal Ancestor
  c := categoryPrincipals $NewCatVec
  pName:= $NewCatVec.0
  if pName and not listMember?(pName,c) then c:= [pName,:c]
  categoryAssociatedTypes($NewCatVec) :=
    [c,FundamentalAncestors,categoryLocals $NewCatVec]
  mkCategory("domain",sigl,attl,globalDomains,$NewCatVec)

Join(:l) ==
  e :=
    null $e or $InteractiveMode => $CategoryFrame
    $e
  JoinInner(l, e)
