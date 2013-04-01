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


import g_-timer
namespace BOOT

--% Cache Lambda Facility
-- for remembering previous values to functions
 
--to CLAM a function f, there must be an entry on $clamList as follows:
--    (functionName  --the name of the function to be CLAMed (e.g. f)
--     kind          --"hash" or number of values to be stored in
--                     circular list
--     eqEtc         --the equal function to be used
--                     (EQ, EQUAL, UEQUAL,..)
--     "shift"       --(opt) for circular lists, shift most recently
--                      used to front
--     "count")      --(opt) use reference counts (see below)
--
-- Notes:
--   Functions with "hash" as kind must give EQ, CVEC, or UEQUAL
--   Functions with some other <identifier> as kind hashed as property
--   lists with eqEtc used to compare entries
--   Functions which have 0 arguments may only be CLAMmed when kind is
--   identifier other than hash (circular/private hashtable for no args
--   makes no sense)
--
--   Functions which have more than 1 argument must never be CLAMed with EQ
--     since arguments are cached as lists
--   For circular lists, "count" will do "shift"ing; entries with lowest
--     use count are replaced
--   For cache option without "count", all entries are cleared on garbage
--     collection; For cache option with "count",
--     entries have their use count set
--     to 0 on garbage collection; those with 0 use count at garbage collection
--     are cleared

++
$clamList ==
  '((canCoerce hash UEQUAL count)                 _
    (canCoerceFrom hash UEQUAL count)             _
    (coerceConvertMmSelection hash UEQUAL count)  _
    (isLegitimateMode hash UEQUAL count)          _
    (isValidType hash UEQUAL count)               _
    (resolveTT hash UEQUAL count)                 _
    (selectMms1 hash UEQUAL count)                _
    (underDomainOf hash UEQUAL count))

++
$failed := '"failed"
 
compHash(op,argl,body) ==
--   Entries will be stored on the global hashtable in a uniform way:
--        (<argument list>, <reference count>,:<value>)
--   where the reference count is optional
  auxfn := makeWorkerName op
  cacheName := "$ConstructorCache"
  g2 := gensym()  --value computed by calling function
  putCode :=
    argl = nil =>
      ['CDDAR,['%store,['tableValue,cacheName,MKQ op],
        ['%list,['%pair,'%nil,['%pair,1,[auxfn]]]]]]
    [auxfn,:argl]
  putCode :=
     ['UNWIND_-PROTECT,['PROG1,putCode,['%store,g2,'%true]],
                  ['%when,[['%not,g2],['tableRemove!,cacheName,MKQ op]]]]
  getCode :=
    argl = nil => ['tableValue,cacheName,MKQ op]
    key :=
      argl is [g] => ['%list,['devaluate,g]]
      ['%list,:[['devaluate,x] for x in argl]]
    ['lassocShiftWithFunction,key,
      ['tableValue,cacheName,MKQ op],['%function,'domainEqualList]]
  returnFoundValue :=
    argl = nil => ['CDRwithIncrement,['CDAR,g2]]
    ['CDRwithIncrement,g2]
  codeBody := mkBind([[g2,getCode]],
                ['%when,[g2,returnFoundValue],['%otherwise,putCode]])
 
  computeFunction := [auxfn,['LAMBDA,argl,:body]]
  if $reportCompilation then
    sayBrightlyI bright '"Generated code for function:"
    pp computeFunction
  compQuietly [[op,['LAMBDA,argl,codeBody]],computeFunction]
  op
 
CDRwithIncrement x ==
  x.first := first x + 1
  rest x
 
clearClams() ==
  for [fn,kind,:.] in $clamList | kind = 'hash or integer? kind repeat
    clearClam fn
 
clearClam fn ==
  infovec := property(fn,'cacheInfo) or keyedSystemError("S2GE0003",[fn])
  eval infovec.cacheReset

reportAndClearClams() ==
  cacheStats()
  clearClams()
 
clearConstructorCaches() ==
  clearCategoryCaches()
  CLRHASH $ConstructorCache
 
clearConstructorCache(cname) ==
  (kind := getConstructorKindFromDB cname) =>
    kind = "category" => clearCategoryCache cname
    tableRemove!($ConstructorCache,cname)
 
clearConstructorAndLisplibCaches() ==
  clearClams()
  clearConstructorCaches()
 
clearCategoryCaches() ==
  for name in allConstructors() repeat
    if getConstructorKindFromDB name = "category" then
      if symbolGlobal?(cacheName:= mkCacheName name)
            then symbolValue(cacheName) := nil
      db := constructorDB name =>
        dbTemplate(db) := nil
 
clearCategoryCache catName ==
  symbolValue(mkCacheName catName) := nil
 
displayHashtable x ==
  l := sortBy(function first,[[opOf val,key] for [key,:val] in entries x])
  for [a,b] in l repeat
    sayBrightlyNT ['"%b",a,'"%d"]
    pp b
 
cacheStats() ==
  for [fn,kind,:u] in $clamList repeat
    not ('count in u) =>
      sayBrightly ["%b",fn,"%d","does not keep reference counts"]
    integer? kind => reportCircularCacheStats(fn,kind)
    kind = 'hash => reportHashCacheStats fn
    sayBrightly ["Unknown cache type for","%b",fn,"%d"]
 
reportCircularCacheStats(fn,n) ==
  infovec:= property(fn,'cacheInfo)
  circList:= eval infovec.cacheName
  numberUsed :=
    +/[1 for i in 1..n for x in circList while x isnt ['_$failed,:.]]
  sayBrightly ["%b",fn,"%d","has","%b",numberUsed,"%d","/ ",n," values cached"]
  displayCacheFrequency mkCircularCountAlist(circList,n)
  finishLine $OutputStream
 
displayCacheFrequency al ==
  al := sortBy(function first,al)
  sayBrightlyNT "    #hits/#occurrences: "
  for [a,:b] in al repeat sayBrightlyNT [a,"/",b,"  "]
  finishLine $OutputStream
 
mkCircularCountAlist(cl,len) ==
  for [x,count,:.] in cl for i in 1..len while x ~= '_$failed repeat
    u:= assoc(count,al) => u.rest := 1 + rest u
    if integer? $reportFavoritesIfNumber and count >= $reportFavoritesIfNumber then
      sayBrightlyNT ["   ",count,"  "]
      pp x
    al:= [[count,:1],:al]
  al
 
reportHashCacheStats fn ==
  infovec:= property(fn,'cacheInfo)
  ht := eval infovec.cacheName
  hashValues:= [val for [.,:val] in entries ht]
  sayBrightly [:bright fn,'"has",:bright(# hashValues),'"values cached."]
  displayCacheFrequency mkHashCountAlist hashValues
  finishLine $OutputStream
 
mkHashCountAlist vl ==
  for [count,:.] in vl repeat
    u:= assoc(count,al) => u.rest := 1 + rest u
    al:= [[count,:1],:al]
  al
 
clamStats() ==
  for [op,kind,:.] in $clamList repeat
    cacheVec:= property(op,'cacheInfo) or systemErrorHere ["clamStats",op]
    postString:=
      cacheValue:= eval cacheVec.cacheName
      kind = 'hash => [" (","%b",tableLength cacheValue,"%d","entries)"]
      empties:= numberOfEmptySlots eval cacheVec.cacheName
      empties = 0 => nil
      [" (","%b",kind-empties,"/",kind,"%d","slots used)"]
    sayBrightly [op,:postString]
 
numberOfEmptySlots cache==
  count:= (CAAR cache ='$failed => 1; 0)
  for x in tails rest cache while not sameObject?(x,cache) repeat
    if CAAR x='$failed then count:= count+1
  count
 
addToConstructorCache(op,args,value) ==
  ['haddProp,'$ConstructorCache,MKQ op,args,['CONS,1,value]]
 
haddProp(ht,op,prop,val) ==
  --presently, ht always = $ConstructorCache
  statRecordInstantiationEvent()
  if $reportInstantiations or $reportEachInstantiation then
    startTimingProcess 'debug
    recordInstantiation(op,prop,false)
    stopTimingProcess 'debug
  u:= tableValue(ht,op) =>     --hope that one exists most of the time
    assoc(prop,u) => val     --value is already there--must = val; exit now
    u.rest := [first u,:rest u]
    u.first := [prop,:val]
    $op: local := op
    listTruncate(u,20)        --save at most 20 instantiations
    val
  tableValue(ht,op) := [[prop,:val]]
  val
 
recordInstantiation(op,prop,dropIfTrue) ==
  startTimingProcess 'debug
  recordInstantiation1(op,prop,dropIfTrue)
  stopTimingProcess 'debug
 
recordInstantiation1(op,prop,dropIfTrue) ==
  if $reportEachInstantiation then
    trailer:= (dropIfTrue => '"  dropped"; '"  instantiated")
    if $insideCoerceInteractive= true then
      $instantCoerceCount:= 1+$instantCoerceCount
    if $insideCanCoerceFrom is [m1,m2] and not dropIfTrue then
      $instantCanCoerceCount:= 1+$instantCanCoerceCount
      xtra:=
        ['" for ",outputDomainConstructor m1,'"-->",outputDomainConstructor m2]
    if $insideEvalMmCondIfTrue and not dropIfTrue then
      $instantMmCondCount:= $instantMmCondCount + 1
    typeTimePrin ["CONCAT",outputDomainConstructor [op,:prop],trailer,:xtra]
  not $reportInstantiations => nil
  u:= tableValue($instantRecord,op) =>     --hope that one exists most of the time
    v := LASSOC(prop,u) =>
      dropIfTrue => v.rest := 1+rest v
      v.first := 1+first v
    u.rest := [first u,:rest u]
    val :=
      dropIfTrue => [0,:1]
      [1,:0]
    u.first := [prop,:val]
  val :=
    dropIfTrue => [0,:1]
    [1,:0]
  tableValue($instantRecord,op) := [[prop,:val]]
 
reportInstantiations() ==
  --assumed to be a hashtable with reference counts
    conList:=
      [:[[n,m,[key,:argList]] for [argList,n,:m] in item]
        for [key,:item] in entries $instantRecord]
    sayBrightly ['"# instantiated/# dropped/domain name",
      "%l",'"------------------------------------"]
    nTotal:= mTotal:= rTotal := nForms:= 0
    for [n,m,form] in sortBy(function third,conList) repeat
      nTotal:= nTotal+n; mTotal:= mTotal+m
      if n > 1 then rTotal:= rTotal + n-1
      nForms:= nForms + 1
      typeTimePrin ['CONCATB,n,m,outputDomainConstructor form]
    sayBrightly ["%b",'"Totals:","%d",nTotal,'" instantiated","%l",
      '"         ",$instantCoerceCount,'" inside coerceInteractive","%l",
       '"         ",$instantCanCoerceCount,'" inside canCoerceFrom","%l",
        '"         ",$instantMmCondCount,'" inside evalMmCond","%l",
         '"         ",rTotal,'" reinstantiated","%l",
          '"         ",mTotal,'" dropped","%l",
           '"         ",nForms,'" distinct domains instantiated/dropped"]
 
listTruncate(l,n) ==
  u:= l
  n:= n - 1
  while n ~= 0 and cons? u repeat
    n := n - 1
    u := rest u
  if cons? u then
    if cons? rest u and $reportInstantiations then
      recordInstantiation($op,CAADR u,true)
    u.rest := nil
  l
 
lassocShiftWithFunction(x,l,fn) ==
  y:= l
  while cons? y repeat
    FUNCALL(fn,x,first first y) => return (result := first y)
    y:= rest y
  result =>
    if not sameObject?(y,l) then
      y.first := first l
      l.first := result
    rest result
  nil
 
rightJustifyString(x,maxWidth) ==
  size:= entryWidth x
  size > maxWidth => keyedSystemError("S2GE0014",[x])
  [fillerSpaces(maxWidth-size,char " "),x]
 
domainEqualList(argl1,argl2) ==
  --function used to match argument lists of constructors
  while argl1 and argl2 repeat
    item1:= devaluate first argl1
    item2:= first argl2
    partsMatch:=
      item1 = item2 => true
      false
    not partsMatch => return nil
    argl1:= rest argl1; argl2 := rest argl2
  argl1 or argl2 => nil
  true
 
removeAllClams() ==
  for [fun,:.] in $clamList repeat
    sayBrightly ['"Un-clamming function",'"%b",fun,'"%d"]
    symbolValue(fun) := eval makeSymbol strconc(STRINGIMAGE fun,'";")
