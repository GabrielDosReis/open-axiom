-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
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
  if x isnt [.,:.] then
    x := macroExpand(x,e)
  x isnt [.,:.] => ident? x and getmode(x,e) = $Category
  getConstructorKind(x.op) is 'category -- FIXME: check arguments too.
 
++ Returns a freshly built category object for a domain or package
++ (as indicated by `domainOrPackage'), with signature list
++ designated by `sigList', attribute list designated by `attList',
++ used domains list designated by `domList', and a princical ancestor
++ category object designated by `principal'.
mkCategory: (%ConstructorKind,%List %Sig,%List %Form,%List %Instantiation, %Maybe %Shell) -> %Shell
mkCategory(domainOrPackage,sigList,attList,domList,principal) ==
  NSigList := nil
  -- Unless extending a principal ancestor (from the end), start
  -- from the first free, unencumbered slot.
  count := 
    principal = nil => $NRTbase
    #principal
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
          "union"/[Prepare2 x for x in stripTags v.args]
        v.op is 'Mapping => "union"/[Prepare2 x for x in v.args]
        v.op is 'Record => "union"/[Prepare2 third x for x in v.args]
        v.op is 'Enumeration => nil
        [v]
  OldLocals:= nil
  -- Remove possible duplicate local domain caches.
  if principal then 
    for u in (OldLocals := categoryLocals principal) repeat 
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
  if principal ~= nil then
    for x in $NRTbase..#principal-1 repeat 
      categoryRef(v,x) := categoryRef(principal,x)
    categoryAssociatedTypes(v) :=
      [categoryPrincipals principal,
        categoryAncestors principal,
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
 
SigListUnion(extra,original,principal,tbl) ==
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
        --PRETTYPRINT(["SigListOpSubsume",e,x])
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
       else epred := mkOr(epred,xpred,tbl,$e)
-- this used always to be done, as noted below, but that's not safe
      if not(ximplem is [["Subsumed",:.],:.]) then eimplem:= ximplem
      if eimplem then esig:=[first esig,second esig] 
           -- in case there's a constant marker
      e:= [esig,epred,:eimplem]
--    e:= [esig,mkOr(xpred,epred,tbl,$e),:ximplem]
-- Original version -gets it wrong if the new operator is only
-- present under certain conditions
        -- We must pick up the previous implementation, if any
--+
      if ximplem is [[q,.,index]] and integer? index and (q="ELT" or q="CONST")
        then bufferRef(principal,index) := e
    original:= [e,:original]
  original
 
mkOr: (%Form,%Form,%Table,%Env) -> %Form
mkOr(a,b,tbl,e) ==
  a=true => true
  b=true => true
  b=a => a
  l:=
    a is ["OR",:a'] =>
      (b is ["OR",:b'] => union(a',b'); mkOr2(b,a',tbl,e) )
    b is ["OR",:b'] => mkOr2(a,b',tbl,e)
    (a is ["has",avar,acat]) and (b is ["has",=avar,bcat]) =>
      descendant?(acat,bcat,tbl,e) => [b]
      descendant?(bcat,acat,tbl,e) => [a]
      [a,b]
    a is ['AND,:a'] and listMember?(b,a') => [b]
    b is ['AND,:b'] and listMember?(a,b') => [a]
    a is ["and",:a'] and listMember?(b,a') => [b]
    b is ["and",:b'] and listMember?(a,b') => [a]
    [a,b]
  #l = 1 => first l
  ["OR",:l]
 
mkOr2: (%Form,%Form,%Table,%Env) -> %Form
mkOr2(a,b,tbl,e) ==
  --a is a condition, "b" a list of them
  listMember?(a,b) => b
  a is ["has",avar,acat] =>
    aRedundant:=false
    for c in b | c is ["has",=avar,ccat] repeat
      descendant?(acat,ccat,tbl,e) =>
        return (aRedundant:=true)
      if descendant?(ccat,acat,tbl,e) then b := remove(b,c)
    aRedundant => b
    [a,:b]
  [a,:b]
 
mkAnd: (%Form,%Form,%Table,%Env) -> %Form
mkAnd(a,b,tbl,e) ==
  a=true => b
  b=true => a
  b=a => a
  l:=
    a is ["AND",:a'] =>
      (b is ["AND",:b'] => union(a',b'); mkAnd2(b,a',tbl,e) )
    b is ["AND",:b'] => mkAnd2(a,b',tbl,e)
    (a is ["has",avar,acat]) and (b is ["has",=avar,bcat]) =>
      descendant?(acat,bcat,tbl,e) => [a]
      descendant?(bcat,acat,tbl,e) => [b]
      [a,b]
    [a,b]
  #l = 1 => first l
  ["AND",:l]
 
mkAnd2: (%Form,%Form,%Table,%Env) -> %Form
mkAnd2(a,b,tbl,e) ==
  --a is a condition, "b" a list of them
  listMember?(a,b) => b
  a is ["has",avar,acat] =>
    aRedundant:=false
    for c in b | c is ["has",=avar,ccat] repeat
      descendant?(ccat,acat,tbl,e) =>
        return (aRedundant:=true)
      if descendant?(acat,ccat,tbl,e) then b := remove(b,c)
    aRedundant => b
    [a,:b]
  [a,:b]
 
++ Return true if `a implies b' in the logical sense.  
predicateImplies(a,b) ==
  b is true => true
  a is true => false
  a = b -- for now.
 
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

++ Subroutine of JoinInner. 
++ Given a list `l' of 2-list [cat,pred] of category object and associated
++ predicate, return a list of similar structures of all fundamental
++ ancestors with appropriate conditions.
FindFundAncs(l,tbl,e) ==
  l = nil => nil
  [hd:=[f1,p1],:l] := l
  canonicalForm f1 = nil => FindFundAncs(l,tbl,e)
  ans := FindFundAncs(l,tbl,e)
  for u in FindFundAncs([[CatEval(first x,tbl,e),mkAnd(p1,second x,tbl,e)]
   for x in categoryAncestors f1],tbl,e) repeat
    x := objectAssoc(first u,ans) =>
      ans := [[first u,mkOr(second x,second u,tbl,e)],:remove(ans,x)]
    ans := [u,:ans]
  --testing to see if hd is already there
  x := objectAssoc(f1,ans) => [[f1,mkOr(p1,second x,tbl,e)],:remove(ans,x)]
  p1 is true =>
    for x in categoryPrincipals f1 repeat
      if y := objectAssoc(CatEval(x,tbl,e),ans) then ans := remove(ans,y)
    [hd,:ans]
  for x in categoryPrincipals f1 repeat
    if y := objectAssoc(CatEval(x,tbl,e),ans) then ans:=
      [[first y,mkOr(p1,second y,tbl,e)],:remove(ans,y)]
  [hd,:ans]
  -- Our new thing may have, as an alternate view, a principal
  -- descendant of something previously added which is therefore
  -- subsumed
 
CatEval: (%Thing,%Table,%Env) -> %Shell
CatEval(x,tbl,e) ==
  vector? x => x
  if $InteractiveMode then
    e := $CategoryFrame
  getCategoryObject(tbl,x,e)
 
ancestor?: (%Form,%List %Instantiation,%Table,%Env) -> %Form
ancestor?(xname,leaves,tbl,env) ==
  -- checks for being a principal ancestor of one of the leaves
  listMember?(xname,leaves) => xname
  for y in leaves repeat
    listMember?(xname,categoryPrincipals CatEval(y,tbl,env)) => return y
 
conditionalAncestor?(xname,leaves,condition,tbl,env) ==
  -- checks for being a principal ancestor of one of the leaves
  for u in leaves repeat
    u':=first u
    ucond:=
      null rest u => true
      second u
    xname = u' or listMember?(xname,categoryPrincipals CatEval(u',tbl,env)) =>
      predicateImplies(condition,ucond) => return u'


++ Returns true if the form `a' designates a category that is any 
++ kind of descendant of the category designated by the form `b'.
descendant?: (%Form,%Form,%Table,%Env) -> %Boolean
descendant?(a,b,tbl,e) ==
  a=b => true
  a is ["ATTRIBUTE",:.] => false
  a is ["SIGNATURE",:.] => false
  a:= CatEval(a,tbl,e)
  b is ["ATTRIBUTE",b'] =>
    (l := assoc(b',categoryAttributes a)) => TruthP second l
  listMember?(b,categoryPrincipals a) => true
  bool ancestor?(b,[first u for u in categoryAncestors a],tbl,e)
 
--% The implementation of Join

++ We have a list `l' of category objects to be joined.
++ Some of them may harbor other categories that exist only under
++ certain conditions.  Collect all those that are indisputably conditional
++ and attempt to detect those apparent conditional categories whose
++ predicates are satified in the current elaboration environment.
++ The end result is a 2-list, the first component being a list of
++ (catobj,pred) pairs and the second component being the list of
++ newly discovered unconditional categories.
filterConditionalCategories(l,tbl,e) ==
  conditionals := nil
  unconditionals := nil
  for cat in l repeat
    for [at,pred] in categoryAttributes cat repeat
      if at isnt [.,:.] then at := [at]
      -- the variable $Attributes is built globally, so that true
      -- attributes can be detected without calling isCategoryForm
      symbolMember?(first at,$Attributes) => nil
      not isCategoryForm(at,e) => $Attributes := [first at,:$Attributes]
      listMember?(pred,get("$Information","special",e)) =>
        --It's true, so we add it as unconditional
        unconditionals := [CatEval(at,tbl,e),:unconditionals]
      pred isnt ['and,:.] =>
        conditionals := [[CatEval(at,tbl,e),pred],:conditionals]
      -- Predicate is a conjunctive; decompose it.
      pred' := [x for x in pred.args |
                  not listMember?(x,get("$Information","special",e))
                    and x isnt true]
      pred' = nil => unconditionals := [CatEval(at,tbl,e),:unconditionals]
      pred' is [x] => conditionals := [[CatEval(at,tbl,e),x],:conditionals]
      conditionals := [[CatEval(at,tbl,e),mkpf(pred','and)],:conditionals]
  [conditionals,reverse! unconditionals]
 
JoinInner(l,$e) ==
  tbl := makeTable function valueEq?
  [CondList,uncondList] := filterConditionalCategories(l,tbl,$e)
  [principal,:l] := [:l,:uncondList]
  principal := mkBuffer principal
  l' := [:CondList,:[[u,true] for u in l]]
    -- This is a list of all the categories that this extends
    -- conditionally or unconditionally
  sigl := categoryExports bufferData principal
  attl := categoryAttributes bufferData principal
  globalDomains := categoryParameters bufferData principal
  FundamentalAncestors := categoryAncestors bufferData principal
  if name := canonicalForm bufferData principal then
    FundamentalAncestors := [[name],:FundamentalAncestors]
  -- this skips buggy code which discards needed categories
  for [b,condition] in FindFundAncs(l',tbl,$e) | bname := b.0 repeat
    conditionalAncestor?(bname,FundamentalAncestors,condition,tbl,$e) => nil
    f := ancestor?(bname,[first u for u in FundamentalAncestors],tbl,$e) =>
      [.,.,index] := assoc(f,FundamentalAncestors)
      FundamentalAncestors := [[bname,condition,index],:FundamentalAncestors]
    PrinAncb := categoryPrincipals CatEval(bname,tbl,$e)
             --Principal Ancestors of b
    reallynew := true
    -- This loop implements Category Subsumption
    for anc in FundamentalAncestors | listMember?(first anc,PrinAncb) repeat
      if rest anc then
        anccond := second anc
        ancindex := third anc
      else
        anccond := true
        ancindex := nil
      if predicateImplies(anccond,condition) then
        FundamentalAncestors :=
            -- the new 'b' is more often true than the old one 'anc'
          [[bname,condition,ancindex],:remove(FundamentalAncestors,anc)]
      else if ancindex then
                 --the new 'b' is less often true
          newentry := [bname,condition,ancindex]
          if not listMember?(newentry,FundamentalAncestors) then
            FundamentalAncestors := [newentry,:FundamentalAncestors]
      else ancindex := nil
      if ancindex then
        bufferRef(principal,ancindex) := bname
        reallynew := false
    if reallynew then
      n := bufferLength principal
      FundamentalAncestors := [[b.0,condition,n],:FundamentalAncestors]
      resizeBuffer(principal,n + 1)
      bufferRef(principal,n) := b.0
  for b in l repeat
    sigl := SigListUnion([DropImplementations u for u in categoryExports b],
             sigl,principal,tbl)
    attl := S_+(categoryAttributes b,attl)
    globalDomains := [:globalDomains,:S_-(categoryParameters b,globalDomains)]
  for b in CondList repeat
    newpred := second b
    for u in categoryAttributes first b repeat
      v := assoc(first u,attl)
      null v =>
        attl := [[first u,mkpf([newpred,second u],'and)],:attl]
      second v is true => nil
      attl := remove(attl,v)
      attl :=
        second u is true => [[first u,mkOr(second v,newpred,tbl,$e)],:attl]
        [[first u,mkOr(second v,mkAnd(newpred,second u,tbl,$e),tbl,$e)],:attl]
    sigl := SigListUnion(
        [AddPredicate(DropImplementations u,newpred)
           for u in categoryExports(first b)],sigl,principal,tbl) where
          AddPredicate(op is [sig,oldpred,:implem],newpred) ==
            newpred is true => op
            oldpred is true => [sig,newpred,:implem]
            [sig,mkpf([oldpred,newpred],'and),:implem]
  FundamentalAncestors := [x for x in FundamentalAncestors | rest x]
               --strip out the pointer to Principal Ancestor
  c := categoryPrincipals bufferData principal
  pName := canonicalForm bufferData principal
  if pName and not listMember?(pName,c) then c := [pName,:c]
  categoryAssociatedTypes(bufferData principal) :=
    [c,FundamentalAncestors,categoryLocals bufferData principal]
  mkCategory("domain",sigl,attl,globalDomains,bufferToVector principal)

Join(:l) ==
  l = nil => mkCategory("domain",nil,nil,nil,nil)
  e :=
    null $e or $InteractiveMode => $CategoryFrame
    $e
  JoinInner(l, e)
