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


import bc_-util
namespace BOOT

--============================================================================
--                Browser Code for Profiling
--============================================================================
kciPage(htPage,junk) ==
  --info alist must have NEW format with [op,:sig] in its CAARs
  which:= '"operation"
  htpSetProperty(htPage,'which,which)
  domname := htpProperty(htPage,'domname)
  conform := htpProperty(htPage,'conform)
  heading := ['"Capsule Cross Reference for ",:htpProperty(htPage,'heading)]
  page := htInitPage(heading,htCopyProplist htPage)
  conname := opOf conform
  htpSetProperty(page,'infoAlist,infoAlist := getInfoAlist conname)
  dbGetExpandedOpAlist page      --expand opAlist "in place"
  opAlist := kciReduceOpAlist(htpProperty(page,'opAlist),infoAlist)
  dbShowOperationsFromConform(page,which,opAlist)

kciReduceOpAlist(opAlist,infoAlist) ==
--count opAlist
  res := [pair for [op,:items] in opAlist | pair] where pair() ==
    u := LASSOC(op,infoAlist) =>
      y := [x for x in items
            | x is [sig,:.] and "or"/[sig = sig1 for [sig1,:.] in u]] => [op,:y]
      nil
    nil
  res

displayInfoOp(htPage,infoAlist,op,sig) ==
  (sigAlist := LASSOC(op,infoAlist)) and (itemlist := LASSOC(sig,sigAlist)) =>
     dbShowInfoOp(htPage,op,sig,itemlist)
  nil

dbShowInfoOp(htPage,op,sig,alist) ==
  heading := htpProperty(htPage,'heading)
  domname := htpProperty(htPage,'domname)
  conform := htpProperty(htPage,'conform)
  opAlist := htpProperty(htPage,'opAlist)
  conname := opOf conform
  kind     := getConstructorKindFromDB conname
  honestConform :=
    kind = 'category =>
      [makeDefaultPackageName symbolName conname,"$",:rest conform]
    conform
  faTypes  := CDDAR getConstructorModemapFromDB conname

  conArgTypes := 
    applySubst(pairList($FormalMapVariableList,IFCDR conform),faTypes)
  conform := htpProperty(htPage,'conform)
  conname := opOf conform
--argTypes := reverse ASSOCRIGHT symbolLassoc('arguments,alist)
--sig := or/[sig for [sig,:.] in LASSOC(op,opAlist) | rest sig = argTypes]
  ops := escapeSpecialChars STRINGIMAGE zeroOneConvert op
  oppart := ['"{\em ", ops, '"}"]
  head :=
    sig => [:oppart,'": ",:dbConformGen dbInfoSig sig]
    oppart
  heading := [:head,'" from {\sf ",form2HtString conform,'"}"]
  for u in alist repeat
    [x,:y] := u
    x = 'locals => locals := y
    x = 'arguments => arguments := y
    fromAlist := [[x,:zeroOneConvertAlist y], :fromAlist]
  fromAlist :=
    cons := args := nil
    for (p := [x,:y]) in fromAlist repeat
      x = $ => dollar := [[honestConform,:y]]
      x = 'Rep => rep := [['Rep,:y]]
      IDENTP x => args := [dbInfoFindCat(conform,conArgTypes,p), :args]
      cons := [dbInfoTran(x,y), :cons]
    [:mySort args, :dollar, :rep, :mySort cons]
  sigAlist  := LASSOC(op,opAlist)
  item := or/[x for x in sigAlist | x is [sig1,:.] and sig1 = sig] or
    systemError '"cannot find signature"
  --item is [sig,pred,origin,exposeFlag,comments]
  [sig,pred,origin,exposeFlag,doc] := item
  htpSetProperty(htPage,'fromAlist,fromAlist)
  htSayHline()
  htSay('"\center{Cross Reference for definition of {\em ",ops,'"}}\beginmenu ")
--  if arguments then
--    htSay '"\item\menuitemstyle{}{\em arguments:}\newline"
--    dbShowInfoList(arguments,0,false)
  if locals then
    htSay '"\item\menuitemstyle{}{\em local variables:}\newline"
    dbShowInfoList(locals,8192,false)
  bincount := 2
  for [con,:fns] in fromAlist repeat
    htSay '"\item"
    if IDENTP con then
        htSay '"\menuitemstyle{} {\em calls to} "
        if con ~= 'Rep then htSay '"{\em argument} "
        htSay con
        if "and"/[fn is ['origin,orig,.] and
          (null origin and (origin := orig) or origin = orig) for fn in fns] then
            htSay '" {\em of type} "
            bcConform orig
        buttonForOp := false
    else
      htMakePage [['bcLinks,['"\menuitemstyle{}",'"",'dbInfoChoose,bincount]]]
      htSay '"{\em calls to} "
      bcConform con
      buttonForOp := true
    htSay('":\newline ")
    dbShowInfoList(fns, bincount * 8192,buttonForOp)
    bincount := bincount + 1
  htSay '"\endmenu "

dbShowInfoList(dataItems,count,buttonForOp?) ==
--dataItems are [op,:sig]
  single? := null rest dataItems
  htSay '"\table{"
  for item in dataItems repeat
    [op,:sig] :=
       item is ['origin,.,s] =>
         buttonForOp? := true
         s
       item
    ops := escapeSpecialChars STRINGIMAGE op
    htSay '"{"
    if count < 16384 or not buttonForOp? then
      htSay [ops,'": "]
      atom sig => bcConform sig
      bcConform dbInfoSig sig
    else
      htMakePage [['bcLinks,[ops,'"",'dbInfoChooseSingle,count]]]
      htSay '": "
      if atom sig then htSay sig else
        bcConform dbInfoSig sig
    htSay '"}"
    count := count + 1
  htSay '"} "
  count

dbInfoFindCat(conform,conArgTypes,u) ==
  [argName,:opSigList] := u
  n := POSITION(argName,IFCDR conform) or systemError()
  t := conArgTypes . n
  [argName,:[dbInfoWrapOrigin(x,t) for x in opSigList]]

dbInfoWrapOrigin(x, t) ==
  [op, :sig] := x
  origin := dbInfoOrigin(op,sig,t) => ['origin, origin, x]
  x

dbInfoOrigin(op,sig,t) ==
  t is ['Join, :r] => or/[dbInfoOrigin(op,sig,x) for x in r]
  t is ['CATEGORY,:.] => false
  [sig = sig1 for [sig1,:.] in LASSOC(op, koOps(t,nil))] => t
  false

dbInfoTran(con,opSigList) == [con,:SUBST("$",con,mySort opSigList)]

zeroOneConvertAlist u == [[zeroOneConvert x,:y] for [x,:y] in u]

dbInfoChoose(htPage,count) ==
  fromAlist := htpProperty(htPage,'fromAlist)
  index := count - 2
  [con, :alist] := fromAlist.index
  dbInfoChoose1(htPage,con,alist)

dbInfoChooseSingle(htPage,count) ==
  fromAlist := htpProperty(htPage,'fromAlist)
  [index, binkey] := DIVIDE(count, 8192)
  [con, :alist] := fromAlist.(index - 2)
  item := alist . binkey
  alist :=
    item is ['origin,origin,s] =>
      con := origin
      [s]
    [item]
  dbInfoChoose1(htPage,con,alist)

dbInfoChoose1(htPage,con,alist) ==
  $conform: local := con
  opAlist := [pair for x in koOps(con,nil) | pair:=dbInfoSigMatch(x,alist)]
  page := htInitPage(nil,nil)
  htpSetProperty(page,'conform,con)
  htpSetProperty(page,'kind,symbolName getConstructorKindFromDB opOf con)
  dbShowOperationsFromConform(page,'"operation",opAlist)

dbInfoSigMatch(x,alist) ==
  [op,:sigAlist] := x
  candidates := [sig for [op1,:sig] in alist | op1 = op] or return nil
  sigs := [s for s in sigAlist | "or"/[first s = s1 for s1 in candidates] or
    (s2 := SUBST($conform,"$",s)) and "or"/[first s2 = s1 for s1 in candidates]]
  sigs and [op,:sigs]


dbInfoSig sig ==
  null rest sig => first sig
  ['Mapping,:sig]

--============================================================================
--                Code to Expand opAlist
--============================================================================
dbGetExpandedOpAlist htPage ==
  expand := htpProperty(htPage,'expandOperations)
  if expand ~= 'fullyExpanded then
    if null expand then htpSetProperty(htPage,'expandOperations,'lists)
    opAlist := koOps(htpProperty(htPage,'conform),nil)
    htpSetProperty(htPage,'opAlist,opAlist)
    dbExpandOpAlistIfNecessary(htPage,opAlist,'"operation",false,false)
  htpProperty(htPage,'opAlist)

--============================================================================
--                  Get Info File Alist
--============================================================================
hasNewInfoAlist conname ==
  (u := getInfoAlist conname) and hasNewInfoText u

hasNewInfoText u ==
  and/[atom op and "and"/[item is [sig,:alist] and
    null sig or cons? sig and cons? alist for item in items] for [op,:items] in u]

getInfoAlist conname ==
  cat? := getConstructorKindFromDB conname = "category"
  if cat? then conname := makeDefaultPackageName symbolName conname
  abb := getConstructorAbbreviationFromDB conname or return '"not a constructor"
  fs  := strconc(symbolName abb,'".NRLIB/info")
  inStream :=
    PROBE_-FILE fs => inputTextFile fs
    filename := strconc('"/spad/int/algebra/",symbolName abb,'".NRLIB/info")
    PROBE_-FILE filename => inputTextFile filename
    return nil
  alist := mySort READ inStream
  if cat? then
    [.,dollarName,:.] := getConstructorFormFromDB conname
    alist := SUBST("$",dollarName,alist)
  alist



