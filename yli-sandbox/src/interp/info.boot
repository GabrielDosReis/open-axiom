-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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
    c="%noBranch" => ["COND",:liftCond [formatPred a,formatInfo b]]
    b="%noBranch" => ["COND",:liftCond [["not",formatPred a],formatInfo c]]
    ["COND",:liftCond [formatPred a,formatInfo b],:
      liftCond [["not",formatPred a],formatInfo c]]
  systemError ['"formatInfo",u]
 
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
  systemError ['"formatPred",u]
 
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
                # ante'=1 => first ante'
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

++ Return true if we are certain that the information
++ denotated by `pred' is derivable from the current environment. 
knownInfo pred ==
  pred=true => true
  member(pred,get("$Information","special",$e)) => true
  pred is ["OR",:l] => or/[knownInfo u for u in l]
  pred is ["AND",:l] => and/[knownInfo u for u in l]
  pred is ["or",:l] => or/[knownInfo u for u in l]
  pred is ["and",:l] => and/[knownInfo u for u in l]
  pred is ["ATTRIBUTE",name,attr] =>
    v:= compForMode(name,$EmptyMode,$e) or return
          stackAndThrow('"can't find category of %1pb",[name])
    [vv,.,.]:= compMakeCategoryObject(second v,$e) or return
                 stackAndThrow('"can't make category of %1pb",[name])
    member(attr,vv.2) => true
    x:= assoc(attr,vv.2) => knownInfo second x
          --format is a list of two elements: information, predicate
    false
  pred is ["has",name,cat] =>
    cat is ["ATTRIBUTE",:a] => knownInfo ["ATTRIBUTE",name,:a]
    cat is ["SIGNATURE",:a] => knownInfo ["SIGNATURE",name,:a]
    -- unnamed category expressions imply structural checks.
    cat is ["Join",:.] => and/[knownInfo ["has",name,c] for c in rest cat]
    cat is ["CATEGORY",.,:atts] =>
      and/[knownInfo hasToInfo ["has",name,att] for att in atts]
    name is ['Union,:.] => false
    -- we have a named category expression
    v:= compForMode(name,$EmptyMode,$e) or return
          stackAndThrow('"can't find category of %1pb",[name])
    vmode := v.mode
    cat = vmode => true
    vmode is ["Join",:l] and member(cat,l) => true
    [vv,.,.]:= compMakeCategoryObject(vmode,$e) or return
                 stackAndThrow('"cannot find category %1pb",[vmode])
    catlist := vv.4
    member(cat,first catlist) => true  --checks princ. ancestors
    (u:=assoc(cat,second catlist)) and knownInfo second u => true
    -- previous line checks fundamental anscestors, we should check their
    --   principal anscestors but this requires instantiating categories

    or/[AncestorP(cat,[first u]) 
         for u in second catlist | knownInfo second u] => true
    false
  pred is ["SIGNATURE",name,op,sig,:.] =>
    v:= get(op,"modemap",$e)
    for w in v repeat
      ww:= CDAR w  --the actual signature part
      ww = sig =>
        CAADR w  = true => return true
        false
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
    [vval,vmode,.]:= GetValue name
    compilerMessage('"augmenting %1: %2p", [name,["ATTRIBUTE",att]])
    key:= if CONTAINED("$",vmode) then "domain" else name
    cat:= ["CATEGORY",key,["ATTRIBUTE",att]]
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
    key:= if CONTAINED("$",vmode) then "domain" else name
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
      member(cat,first ocatvec.4) or
         assoc(cat,second ocatvec.4) is [.,"T",.] => $e
             --what was being asserted is an ancestor of what was known
      if name="$"
        then $e:= augModemapsFromCategory(name,name,name,cat,$e)
        else
          genDomainView(name,name,cat,"HasCategory")
          -- a domain upgrade at function level is local to that function.
          if not $insideCapsuleFunctionIfTrue and 
            not MEMQ(name,$functorLocalParameters) then
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
 
