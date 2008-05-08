-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007, Gabriel Dos Reis.
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
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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
--%  (COND
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
  $Information: local
  --$Information:= nil: done by previous statement anyway
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
 
addInfo u == $Information:= [formatInfo u,:$Information]
 
formatInfo u ==
  atom u => u
  u is ["SIGNATURE",:v] => ["SIGNATURE","$",:v]
 --u is ("CATEGORY",junk,:l) => ("PROGN",:(formatInfo v for v in l))
  u is ["PROGN",:l] => ["PROGN",:[formatInfo v for v in l]]
  u is ["ATTRIBUTE",v] =>
 
    -- The parser can't tell between those attributes that really
    -- are attributes, and those that are category names
    atom v and isCategoryForm([v],$e) => ["has","$",[v]]
    atom v => ["ATTRIBUTE","$",v]
    isCategoryForm(v,$e) => ["has","$",v]
    ["ATTRIBUTE","$",v]
  u is ["IF",a,b,c] =>
    c="%noBranch" => ["COND",:liftCond [formatPred a,formatInfo b]]
    b="%noBranch" => ["COND",:liftCond [["not",formatPred a],formatInfo c]]
    ["COND",:liftCond [formatPred a,formatInfo b],:
      liftCond [["not",formatPred a],formatInfo c]]
  systemError '"formatInfo"
 
liftCond (clause is [ante,conseq]) ==
  conseq is ["COND",:l] =>
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
  systemError '"formatPred"
 
chaseInferences(pred,$e) ==
  foo hasToInfo pred where
    foo pred ==
      knownInfo pred => nil
      $e:= actOnInfo(pred,$e)
      pred:= infoToHas pred
      for u in get("$Information","special",$e) repeat
        u is ["COND",:l] =>
          for [ante,:conseq] in l repeat
            ante=pred => [foo w for w in conseq]
            ante is ["and",:ante'] and member(pred,ante') =>
              ante':= delete(pred,ante')
              v':=
                LENGTH ante'=1 => first ante'
                ["and",:ante']
              v':= ["COND",[v',:conseq]]
              member(v',get("$Information","special",$e)) => nil
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
 
knownInfo pred ==
               --true %if the information is already known
  pred=true => true
  --pred = "true" => true
  member(pred,get("$Information","special",$e)) => true
  pred is ["OR",:l] => or/[knownInfo u for u in l]
  pred is ["AND",:l] => and/[knownInfo u for u in l]
  pred is ["or",:l] => or/[knownInfo u for u in l]
  pred is ["and",:l] => and/[knownInfo u for u in l]
  pred is ["ATTRIBUTE",name,attr] =>
    v:= compForMode(name,$EmptyMode,$e)
    null v => stackSemanticError(["can't find category of ",name],nil)
    [vv,.,.]:= compMakeCategoryObject(CADR v,$e)
    null vv => stackSemanticError(["can't make category of ",name],nil)
    member(attr,vv.2) => true
    x:= assoc(attr,vv.2) => knownInfo CADR x
          --format is a list of two elements: information, predicate
    false
  pred is ["has",name,cat] =>
    cat is ["ATTRIBUTE",:a] => knownInfo ["ATTRIBUTE",name,:a]
    cat is ["SIGNATURE",:a] => knownInfo ["SIGNATURE",name,:a]
    name is ['Union,:.] => false
    v:= compForMode(name,$EmptyMode,$e)
    null v => stackSemanticError(["can't find category of ",name],nil)
    vmode := CADR v
    cat = vmode => true
    vmode is ["Join",:l] and member(cat,l) => true
    [vv,.,.]:= compMakeCategoryObject(vmode,$e)
    catlist := vv.4
    --catlist := SUBST(name,'$,vv.4)
    null vv => stackSemanticError(["can't make category of ",name],nil)
    member(cat,first catlist) => true  --checks princ. ancestors
    (u:=assoc(cat,CADR catlist)) and knownInfo(CADR u) => true
    -- previous line checks fundamental anscestors, we should check their
    --   principal anscestors but this requires instantiating categories

    -- This line caused recursion on predicates which are no use in deciding
    -- whether a category was present.
-- this is correct TPD feb, 19, 2003
    or/[AncestorP(cat,LIST CAR u) for u in CADR catlist | knownInfo CADR u] => true
-- this is wrong TPD feb, 19, 2003
    -- or/[AncestorP(cat,LIST CAR u) and knownInfo CADR u for u in CADR catlist] => true
    false
  pred is ["SIGNATURE",name,op,sig,:.] =>
    v:= get(op,"modemap",$e)
    for w in v repeat
      ww:= CDAR w
          --the actual signature part
      LENGTH ww=LENGTH sig and SourceLevelSubsume(ww,sig) =>
        --NULL CAADR w => return false
        CAADR w  = true => return true
        --return false
        --error '"knownInfo"
  false
 
actOnInfo(u,$e) ==
  null u => $e
  u is ["PROGN",:l] => (for v in l repeat $e:= actOnInfo(v,$e); $e)
  $e:=
    put("$Information","special",Info:= [u,:get("$Information","special",$e)],$e
      )
  u is ["COND",:l] =>
      --there is nowhere %else that this sort of thing exists
    for [ante,:conseq] in l repeat
      if member(hasToInfo ante,Info) then for v in conseq repeat
        $e:= actOnInfo(v,$e)
    $e
  u is ["ATTRIBUTE",name,att] =>
    [vval,vmode,venv]:= GetValue name
    SAY("augmenting ",name,": ",u)
    key:= if CONTAINED("$",vmode) then "domain" else name
    cat:= ["CATEGORY",key,["ATTRIBUTE",att]]
    $e:= put(name,"value",[vval,mkJoin(cat,vmode),venv],$e)
      --there is nowhere %else that this sort of thing exists
  u is ["SIGNATURE",name,operator,modemap] =>
    implem:=
      (implem:=ASSOC([name,:modemap],get(operator,'modemap,$e))) =>
          CADADR implem
      name = "$" => ['ELT,name,-1]
      ['ELT,name,substitute('$,name,modemap)]
    $e:= addModemap(operator,name,modemap,true,implem,$e)
    [vval,vmode,venv]:= GetValue name
    SAY("augmenting ",name,": ",u)
    key:= if CONTAINED("$",vmode) then "domain" else name
    cat:= ["CATEGORY",key,["SIGNATURE",operator,modemap]]
    $e:= put(name,"value",[vval,mkJoin(cat,vmode),venv],$e)
  u is ["has",name,cat] =>
    [vval,vmode,venv]:= GetValue name
    cat=vmode => $e --stating the already known
    u:= compMakeCategoryObject(cat,$e) =>
         --we are adding information about a category
      [catvec,.,$e]:= u
      [ocatvec,.,$e]:= compMakeCategoryObject(vmode,$e)
      -- member(vmode,CAR catvec.4) =>
      --    JHD 82/08/08 01:40 This does not mean that we can ignore the
      --    extension, since this may not be compatible with the view we
      --    were passed
 
      --we are adding a principal descendant of what was already known
      --    $e:= augModemapsFromCategory(name,name,nil,catvec,$e)
      --    SAY("augmenting ",name,": ",cat)
      --    put(name, "value", (vval, cat, venv), $e)
      member(cat,first ocatvec.4) or
         ASSOC(cat,CADR ocatvec.4) is [.,"T",.] => $e
        --SAY("Category extension error:
        --cat shouldn't be a join
                      --what was being asserted is an ancestor of what was known
      if name="$"
        then $e:= augModemapsFromCategory(name,name,name,cat,$e)
        else
          viewName:=genDomainViewName(name,cat)
          genDomainView(viewName,name,cat,"HasCategory")
          if not MEMQ(viewName,$functorLocalParameters) then
             $functorLocalParameters:=[:$functorLocalParameters,viewName]
      SAY("augmenting ",name,": ",cat)
      $e:= put(name,"value",[vval,mkJoin(cat,vmode),venv],$e)
    SAY("extension of ",vval," to ",cat," ignored")
    $e
  systemError '"knownInfo"
 
mkJoin(cat,mode) ==
  mode is ['Join,:cats] => ['Join,cat,:cats]
  ['Join,cat,mode]
 
GetValue name ==
  u:= get(name,"value",$e) => u
  u:= comp(name,$EmptyMode,$e) => u  --name may be a form
  systemError [name,'" is not bound in the current environment"]
 
