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


import clam
namespace BOOT

isPackageFunction() ==
  -- called by compile/putInLocalDomainReferences
--+
  nil

isCategoryPackageName nam ==
  p := PNAME opOf nam
  p.(MAXINDEX p) = char '_&

processFunctorOrPackage(form,signature,data,localParList,m,e) ==
--+
  processFunctor(form,signature,data,localParList,e)
 
processPackage($definition is [name,:args],[$catsig,:argssig],code,locals,$e) ==
  $GENNO: local:= 0 --for GENVAR()
  $catsig: local := nil
               --used in ProcessCond
  $maximalViews: local := nil
                      --read by ProcessCond
  $ResetItems: local := nil
       --stores those items that get SETQed, and may need re-processing
  $catvecList: local:= [$domainShell]
  $catNames: local:= ["$"]
--PRINT $definition
--PRINT ($catsig,:argssig)
--PRETTYPRINT code
  catvec:= $domainShell --from compDefineFunctor
  $getDomainCode:= optFunctorBody $getDomainCode
      --the purpose of this is so ProcessCond recognises such items
  code:= PackageDescendCode(code,true,nil)
  if delete(nil,locals) then code:=[:code,:(setPackageCode locals)] where
      setPackageCode locals ==
          locals':=[[u,:i] for u in locals for i in 0.. | u]
          locals'' :=[]
          while locals' repeat
            for v in locals' repeat
              [u,:i]:=v
              if and/[EQ(v,v') or not subTree(u,CAR v') for v' in locals']
              then
                locals'':=[v,:locals'']
                locals':=delete(v,locals')
          precomp:=code:=[]
          for elem in locals'' repeat
            [u,:i]:=elem
            if ATOM u then u':=u
            else
              u':=opt(u,precomp) where
                  opt(u,alist) ==
                    ATOM u => u
                    for v in u repeat
                      if (a:=assoc(v,alist)) then
                        [.,:i]:=a
                        u:=replace(v,["getShellEntry","$",i],u) where
                           replace(old,new,l) ==
                             l isnt [h,:t] => l
                             h = old => [new,:t]
                             [h,:replace(old,new,t)]
                      v':=opt(v,alist)
                      EQ(v,v') => nil
                      u:=replace(v,v',u)
                    u
              precomp:=[elem,:precomp]
            code:=[["setShellEntry","$",i,u'],:code]
          nreverse code
  code:=
    ["PROGN",:$getDomainCode,["LET","$",["newShell",#locals]],
                           --It is important to place this code here,
                           --after $ is set up
                   --slam functor with shell
                   --the order of steps in this PROGN are critical
      addToSlam($definition,"$"),code,[
        "SETELT","$",0, mkDomainConstructor $definition],:
-- If we call addMutableArg this early, then recurise calls to this domain
-- (e.g. while testing predicates) will generate new domains => trouble
--      "SETELT","$",0,addMutableArg mkDomainConstructor $definition],:
          [["SETELT","$",position(name,locals),name]
            for name in $ResetItems | MEMQ(name,locals)],
             :[($mutableDomain => '(RPLACD (LASTNODE (ELT $ 0))
                                           (LIST (GENSYM)));[]) ],
              "$"]
  for u in $getDomainCode repeat
    u is ['LET,.,u'] and u' is ['getDomainView,.,u''] =>
      $packagesUsed:=union(CategoriesFromGDC u'',$packagesUsed)
  $packagesUsed:=union($functorLocalParameters,$packagesUsed)
  $getDomainCode:= nil
     --if we didn't kill this, DEFINE would insert it in the wrong place
  optFunctorBody code
 
subTree(u,v) ==
  v=u => true
  ATOM v => nil
  or/[subTree(u,v') for v' in v]
 
mkList u ==
  u => ["LIST",:u]
  nil
 
setPackageLocals(pac,locs) ==
  for var in locs for i in 0.. | var^=nil repeat pac.i:= var
 
PackageDescendCode(code,flag,viewAssoc) ==
               --flag is true if we are walking down code always executed
               --nil if we are in conditional code
  code=nil => nil
  code="%noBranch" => nil
  code is ["add",base,:codelist] =>
    systemError '"packages may not have add clauses"
  code is ["PROGN",:codelist] =>
    ["PROGN",:
      [v for u in codelist | (v:= PackageDescendCode(u,flag,viewAssoc))^=nil]]
  code is ["COND",:condlist] =>
    c:=
      ["COND",:
        [[u2:= ProcessCond(first u,viewAssoc),:
         (if null u2
             then nil
             else
              [PackageDescendCode(v,flag and TruthP u2,
                if first u is ["HasCategory",dom,cat]
                   then [[dom,:cat],:viewAssoc]
                   else viewAssoc) for v in rest u])] for u in condlist]]
    TruthP CAADR c => ["PROGN",:CDADR c]
    c
  code is ["LET",name,body,:.] =>
    if not MEMQ(name,$ResetItems) then $ResetItems:= [name,:$ResetItems]
    if body is [a,:.] and isFunctor a
      then $packagesUsed:=[body,:$packagesUsed]
    code
  code is ["CodeDefine",sig,implem] =>
             --Generated by doIt in COMPILER BOOT
    dom:= "$"
    dom:=
      u:= LASSOC(dom,viewAssoc) => ["getDomainView",dom,u]
      dom
    body:= ["CONS",implem,dom]
    SetFunctionSlots(sig,body,flag,"original")
  code is [":",:.] => (RPLACA(code,"LIST"); RPLACD(code,NIL))
      --Yes, I know that's a hack, but how else do you kill a line?
  code is ["LIST",:.] => nil
  code is ["MDEF",:.] => nil
  code is ["devaluate",:.] => nil
  code is ["call",:.] => code
  code is ["SETELT",:.] => code
  code is ["QSETREFV",:.] => code
  code is ["setShellEntry",:.] => code
  stackWarning('"unknown Package code: %1 ",[code])
  code
 
mkOperatorEntry(domainOrPackage,opSig is [op,sig,:flag],pred,count) ==
  domainOrPackage^="domain" =>
    [opSig,pred,["PAC","$",name]] where
      name() == encodeFunctionName(op,domainOrPackage,sig,":",count)
  null flag => [opSig,pred,["ELT","$",count]]
  first flag="constant" => [[op,sig],pred,["CONST","$",count]]
  systemError ["unknown variable mode: ",flag]
 
optPackageCall(x,["PAC",packageVariableOrForm,functionName],arglist) ==
  RPLACA(x,functionName)
  RPLACD(x,[:arglist,packageVariableOrForm])
  x
 
--% Code for encoding function names inside package or domain
 
encodeFunctionName(fun,package is [packageName,:arglist],signature,sep,count)
   ==
    signature':= substitute("$",package,signature)
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
  ALPHA_-CHAR_-P (s:= STRINGIMAGE u).(MAXINDEX s) => INTERN STRCONC(s,STRINGIMAGE n)
  INTERNL STRCONC(s,STRINGIMAGE ";",STRINGIMAGE n)
 

