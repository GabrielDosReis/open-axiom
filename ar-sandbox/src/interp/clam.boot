-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2010, Gabriel Dos Reis.
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
-- see definition of backendCompile2 in c-util which calls clamComp below

++
$hashNode := [[]]

++
$failed := '"failed"
 
-- see SETQ LISP for initial def of $hashNode
 
compClam(op,argl,body,$clamList) ==
  --similar to reportFunctionCompilation in SLAM BOOT
  if $InteractiveMode then startTimingProcess 'compilation
  if (u:= LASSQ(op,$clamList)) isnt [kind,eqEtc,:options]
    then keyedSystemError("S2GE0004",[op])
  $clamList:= nil            --clear to avoid looping
  if u:= S_-(options,'(shift count)) then
    keyedSystemError("S2GE0006",[op,:u])
  shiftFl := 'shift in options
  countFl := 'count in options
  if #argl > 1 and eqEtc= 'EQ then
    keyedSystemError("S2GE0007",[op])
  (not IDENTP kind) and (not integer? kind or kind < 1) =>
    keyedSystemError("S2GE0005",[op])
  IDENTP kind =>
    shiftFl => keyedSystemError("S2GE0008",[op])
    compHash(op,argl,body,(kind='hash => nil; kind),eqEtc,countFl)
  cacheCount:= kind
  if null argl then keyedSystemError("S2GE0009",[op])
  phrase:=
    cacheCount=1 => ['"computed value only"]
    [:bright cacheCount,'"computed values"]
  sayBrightly [:bright op,'"will save last",:phrase]
  auxfn:= INTERNL(op,'";")
  g1:= GENSYM()  --argument or argument list
  [arg,computeValue] :=
    argl is [.] => [[g1],[auxfn,g1]]  --g1 is a parameter
    [g1,['APPLX,['function,auxfn],g1]]          --g1 is a parameter list
  cacheName:= INTERNL(op,'";AL")
  if $reportCounts=true then
    hitCounter:= INTERNL(op,'";hit")
    callCounter:= INTERNL(op,'";calls")
    setDynamicBinding(hitCounter,0)
    setDynamicBinding(callCounter,0)
    callCountCode:= [['SETQ,callCounter,['QSADD1,callCounter]]]
    hitCountCode:=  [['SETQ,hitCounter,['QSADD1,hitCounter]]]
  g2:= GENSYM()  --length of cache or arg-value pair
  g3:= GENSYM()  --value computed by calling function
  lookUpFunction:=
    shiftFl =>
      countFl => 'assocCacheShiftCount
      'assocCacheShift
    countFl => 'assocCacheCount
    'assocCache
  returnFoundValue:=
    countFl => ['CDDR,g3]
    ['CDR,g3]
  namePart:=
    countFl => cacheName
    MKQ cacheName
  secondPredPair:=
--   null argl => [cacheName]
    [['SETQ,g3,[lookUpFunction,g1,namePart,eqEtc]],
      :hitCountCode,
        returnFoundValue]
  resetCacheEntry:=
    countFl => ['CONS,1,g2]
    g2
  thirdPredPair:=
--   null argl => ['(QUOTE T),['SETQ,cacheName,computeValue]]
    ['(QUOTE T),
      ['SETQ,g2,computeValue],
        ['SETQ,g3,['CAR,cacheName]],
          ['RPLACA,g3,g1],
            ['RPLACD,g3,resetCacheEntry],
              g2]
  codeBody:= ['PROG,[g2,g3],
                :callCountCode,
                  ['RETURN,['COND,secondPredPair,thirdPredPair]]]
  lamex:= ['LAM,arg,codeBody]
  mainFunction:= [op,lamex]
  computeFunction:= [auxfn,['LAMBDA,argl,:body]]
 
  -- compile generated function stub
  compileInteractive mainFunction
 
  -- compile main body: this has already been compTran'ed
  if $reportCompilation then
    sayBrightlyI bright '"Generated LISP code for function:"
    pp computeFunction
  compileQuietly [computeFunction]
 
  cacheType:= 'function
  cacheResetCode:= ['SETQ,cacheName,['initCache,cacheCount]]
  cacheCountCode:= ['countCircularAlist,cacheName,cacheCount]
  cacheVector:= mkCacheVec(op,cacheName,cacheType,
    cacheResetCode,cacheCountCode)
  LAM_,EVALANDFILEACTQ ['PUT, MKQ op, MKQ 'cacheInfo, MKQ cacheVector]
  LAM_,EVALANDFILEACTQ cacheResetCode
  if $InteractiveMode then stopTimingProcess 'compilation
  op
 
compHash(op,argl,body,cacheNameOrNil,eqEtc,countFl) ==
  --Note: when cacheNameOrNil~=nil, it names a global hashtable
 
-- cacheNameOrNil => compHashGlobal(op,argl,body,cacheNameOrNil,eqEtc,countFl)
--   This branch to compHashGlobal is now omitted; as a result,
--   entries will be stored on the global hashtable in a uniform way:
--        (<argument list>, <reference count>,:<value>)
--   where the reference count is optional
 
  if cacheNameOrNil and cacheNameOrNil~='_$ConstructorCache then
    keyedSystemError("S2GE0010",[op])
    --restriction due to omission of call to hputNewValue (see *** lines below)
 
  if null argl then
    null cacheNameOrNil => keyedSystemError("S2GE0011",[op])
    nil
  (not cacheNameOrNil) and not (eqEtc in '(EQ CVEC UEQUAL)) =>
    keyedSystemError("S2GE0012",[op])
--withWithout := (countFl => "with"; "without")
--middle:=
--  cacheNameOrNil => ["on","%b",cacheNameOrNil,"%d"]
--  '"privately "
--sayBrightly
--  ["%b",op,"%d","hashes ",:middle,withWithout," reference counts"]
  auxfn:= INTERNL(op,'";")
  g1:= GENSYM()  --argument or argument list
  [arg,cacheArgKey,computeValue] :=
  --    arg: to be used as formal argument of lambda construction;
  --    cacheArgKey: the form used to look up the value in the cache
  --    computeValue: the form used to compute the value from arg
    null argl => [nil,nil,[auxfn]]
    argl is [.] =>
      key:= (cacheNameOrNil => ['devaluate,g1]; g1)
      [[g1],['LIST,key],[auxfn,g1]]  --g1 is a parameter
    key:= (cacheNameOrNil => ['devaluateList,g1] ; g1)
    [g1,key,['APPLY,['function,auxfn],g1]]   --g1 is a parameter list
  cacheName:= cacheNameOrNil or INTERNL(op,'";AL")
  if $reportCounts=true then
    hitCounter:= INTERNL(op,'";hit")
    callCounter:= INTERNL(op,'";calls")
    setDynamicBinding(hitCounter,0)
    setDynamicBinding(callCounter,0)
    callCountCode:= [['SETQ,callCounter,['QSADD1,callCounter]]]
    hitCountCode:=  [['SETQ,hitCounter,['QSADD1,hitCounter]]]
  g2:= GENSYM()  --value computed by calling function
  returnFoundValue:=
    null argl =>
    --  if we have a global hastable, functions with no arguments are
    --  stored in the same format as those with several arguments, e.g.
    --  to cache the value <val> given by f(), the structure
    --  ((nil <count> <val>)) is stored in the cache
      countFl => ['CDRwithIncrement,['CDAR,g2]]
      ['CDAR,g2]
    countFl => ['CDRwithIncrement,g2]
    g2
  getCode:=
    null argl => ['HGET,cacheName,MKQ op]
    cacheNameOrNil =>
      eqEtc ~= 'EQUAL =>
        ['lassocShiftWithFunction,cacheArgKey,
          ['HGET,cacheNameOrNil,MKQ op],MKQ eqEtc]
      ['lassocShift,cacheArgKey,['HGET,cacheNameOrNil,MKQ op]]
    ['HGET,cacheName,g1]
  secondPredPair:= [['SETQ,g2,getCode],:hitCountCode,returnFoundValue]
  putCode:=
    null argl =>
      cacheNameOrNil =>
        countFl => ['CDDAR,['HPUT,cacheNameOrNil,MKQ op,
                      ['LIST,['CONS,nil,['CONS,1,computeValue]]]]]
        ['HPUT,cacheNameOrNil,MKQ op,['LIST,['CONS,nil,computeValue]]]
      systemError '"unexpected"
    cacheNameOrNil => computeValue
    --countFl => ['CDR,['hputNewProp,cacheNameOrNil,MKQ op,cacheArgKey, --***
    --             ['CONS,1,computeValue]]]                             --***
    --['hputNewProp,cacheNameOrNil,MKQ op,cacheArgKey,computeValue]    --***
    countFl => ['CDR,['HPUT,cacheName,g1,['CONS,1,computeValue]]]
    ['HPUT,cacheName,g1,computeValue]
  if cacheNameOrNil then putCode :=
     ['UNWIND_-PROTECT,['PROG1,putCode,['SETQ,g2,'T]],
                  ['COND,[['NOT,g2],['HREM,cacheName,MKQ op]]]]
  thirdPredPair:= ['(QUOTE T),putCode]
  codeBody:= ['PROG,[g2],
               :callCountCode,['RETURN,['COND,secondPredPair,thirdPredPair]]]
  lamex:= ['LAM,arg,codeBody]
  mainFunction:= [op,lamex]
  computeFunction:= [auxfn,['LAMBDA,argl,:body]]
 
  -- compile generated function stub
  compileInteractive mainFunction
 
  -- compile main body: this has already been compTran'ed
  if $reportCompilation then
    sayBrightlyI bright '"Generated LISP code for function:"
    pp computeFunction
  compileQuietly [computeFunction]
 
  if null cacheNameOrNil then
    cacheType:=
      countFl => 'hash_-tableWithCounts
      'hash_-table
    weakStrong:= (countFl => 'STRONG; 'WEAK)
      --note: WEAK means that key/value pairs disappear at garbage collection
    cacheResetCode:=
      ['SETQ,cacheName,['MAKE_-HASHTABLE,MKQ eqEtc]]
    cacheCountCode:= ['hashCount,cacheName]
    cacheVector:=
      mkCacheVec(op,cacheName,cacheType,cacheResetCode,cacheCountCode)
    LAM_,EVALANDFILEACTQ ['PUT, MKQ op, MKQ 'cacheInfo, MKQ cacheVector]
    LAM_,EVALANDFILEACTQ cacheResetCode
  op
 
compHashGlobal(op,argl,body,cacheName,eqEtc,countFl) ==
  --Note: when cacheNameOrNil~=nil, it names a global hashtable
 
  if (not (eqEtc in '(UEQUAL))) then
    sayBrightly "for hash option, only EQ, CVEC, and UEQUAL are allowed"
  auxfn:= INTERNL(op,'";")
  g1:= GENSYM()  --argument or argument list
  [arg,cacheArgKey,computeValue] :=
  --    arg: to be used as formal argument of lambda construction;
  --    cacheArgKey: the form used to look up the value in the cache
  --    computeValue: the form used to compute the value from arg
    application:=
      null argl => [auxfn]
      argl is [.] => [auxfn,g1]  --g1 is a parameter
      ['APPLX,['function,auxfn],g1]          --g1 is a parameter list
    [g1,['consForHashLookup,MKQ op,g1],application]
  g2:= GENSYM()  --value computed by calling function
  returnFoundValue:=
    countFl => ['CDRwithIncrement,g2]
    g2
  getCode:= ['HGET,cacheName,cacheArgKey]
  secondPredPair:= [['SETQ,g2,getCode],returnFoundValue]
  putForm:= ['CONS,MKQ op,g1]
  putCode:=
    countFl => ['HPUT,cacheName,putForm,['CONS,1,computeValue]]
    ['HPUT,cacheName,putForm,computeValue]
  thirdPredPair:= ['(QUOTE T),putCode]
  codeBody:= ['PROG,[g2], ['RETURN,['COND,secondPredPair,thirdPredPair]]]
  lamex:= ['LAM,arg,codeBody]
  mainFunction:= [op,lamex]
  computeFunction:= [auxfn,['LAMBDA,argl,:body]]
  compileInteractive mainFunction
  compileInteractive computeFunction
  op
 
consForHashLookup(a,b) ==
  $hashNode.first := a
  $hashNode.rest := b
  $hashNode
 
CDRwithIncrement x ==
  x.first := QSADD1 first x
  rest x
 
HGETandCount(hashTable,prop) ==
  u:= HGET(hashTable,prop) or return nil
  u.first := QSADD1 first u
  u
 
clearClams() ==
  for [fn,kind,:.] in $clamList | kind = 'hash or integer? kind repeat
    clearClam fn
 
clearClam fn ==
  infovec:= GETL(fn,'cacheInfo) or keyedSystemError("S2GE0003",[fn])
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
    HREM($ConstructorCache,cname)
 
clearConstructorAndLisplibCaches() ==
  clearClams()
  clearConstructorCaches()
 
clearCategoryCaches() ==
  for name in allConstructors() repeat
    if getConstructorKindFromDB name = "category" then
      if BOUNDP(cacheName:= INTERNL strconc(PNAME name,'";AL"))
            then setDynamicBinding(cacheName,nil)
    if BOUNDP(cacheName:= INTERNL strconc(PNAME name,'";CAT"))
          then setDynamicBinding(cacheName,nil)
 
clearCategoryCache catName ==
  cacheName:= INTERNL strconc(PNAME catName,'";AL")
  setDynamicBinding(cacheName,nil)
 
displayHashtable x ==
  l:= nreverse SORTBY('CAR,[[opOf HGET(x,key),key] for key in HKEYS x])
  for [a,b] in l repeat
    sayBrightlyNT ['%b,a,'%d]
    pp b
 
cacheStats() ==
  for [fn,kind,:u] in $clamList repeat
    not ('count in u) =>
      sayBrightly ["%b",fn,"%d","does not keep reference counts"]
    integer? kind => reportCircularCacheStats(fn,kind)
    kind = 'hash => reportHashCacheStats fn
    sayBrightly ["Unknown cache type for","%b",fn,"%d"]
 
reportCircularCacheStats(fn,n) ==
  infovec:= GETL(fn,'cacheInfo)
  circList:= eval infovec.cacheName
  numberUsed :=
    +/[1 for i in 1..n for x in circList while x isnt [='_$failed,:.]]
  sayBrightly ["%b",fn,"%d","has","%b",numberUsed,"%d","/ ",n," values cached"]
  displayCacheFrequency mkCircularCountAlist(circList,n)
  TERPRI()
 
displayCacheFrequency al ==
  al := nreverse SORTBY('CAR,al)
  sayBrightlyNT "    #hits/#occurrences: "
  for [a,:b] in al repeat sayBrightlyNT [a,"/",b,"  "]
  TERPRI()
 
mkCircularCountAlist(cl,len) ==
  for [x,count,:.] in cl for i in 1..len while x ~= '_$failed repeat
    u:= assoc(count,al) => u.rest := 1 + rest u
    if integer? $reportFavoritesIfNumber and count >= $reportFavoritesIfNumber then
      sayBrightlyNT ["   ",count,"  "]
      pp x
    al:= [[count,:1],:al]
  al
 
reportHashCacheStats fn ==
  infovec:= GETL(fn,'cacheInfo)
  hashTable:= eval infovec.cacheName
  hashValues:= [HGET(hashTable,key) for key in HKEYS hashTable]
  sayBrightly [:bright fn,'"has",:bright(# hashValues),'"values cached."]
  displayCacheFrequency mkHashCountAlist hashValues
  TERPRI()
 
mkHashCountAlist vl ==
  for [count,:.] in vl repeat
    u:= assoc(count,al) => u.rest := 1 + rest u
    al:= [[count,:1],:al]
  al
 
clearHashReferenceCounts() ==
  --free all cells with 0 reference counts; clear other counts to 0
  for x in $clamList repeat
    x.cacheType='hash_-tableWithCounts =>
      remHashEntriesWith0Count eval x.cacheName
    x.cacheType='hash_-table => CLRHASH eval x.cacheName
 
remHashEntriesWith0Count $hashTable ==
  MAPHASH(function fn,$hashTable) where fn(key,obj) ==
    first obj = 0 => HREM($hashTable,key)  --free store
    nil
 
initCache n ==
  tail:= '(0 . $failed)
  l:= [[$failed,:tail] for i in 1..n]
  lastNode(l).rest := l
 
assocCache(x,cacheName,fn) ==
  --fn=equality function; do not SHIFT or COUNT
  al:= eval cacheName
  forwardPointer:= al
  val:= nil
  until EQ(forwardPointer,al) repeat
    FUNCALL(fn,CAAR forwardPointer,x) => return (val:= first forwardPointer)
    backPointer:= forwardPointer
    forwardPointer:= rest forwardPointer
  val => val
  setDynamicBinding(cacheName,backPointer)
  nil
 
assocCacheShift(x,cacheName,fn) ==  --like ASSOC except that al is circular
  --fn=equality function; SHIFT but do not COUNT
  al:= eval cacheName
  forwardPointer:= al
  val:= nil
  until EQ(forwardPointer,al) repeat
    FUNCALL(fn, first (y:=first forwardPointer),x) =>
      if not EQ(forwardPointer,al) then   --shift referenced entry to front
        forwardPointer.first := first al
        al.first := y
      return (val:= y)
    backPointer := forwardPointer      --first is slot replaced on failure
    forwardPointer:= rest forwardPointer
  val => val
  setDynamicBinding(cacheName,backPointer)
  nil
 
assocCacheShiftCount(x,al,fn) ==
  -- if x is found, entry containing x becomes first element of list; if
  -- x is not found, entry with smallest use count is shifted to front so
  -- as to be replaced
  --fn=equality function; COUNT and SHIFT
  forwardPointer:= al
  val:= nil
  minCount:= 10000 --preset minCount but not newFrontPointer here
  until EQ(forwardPointer,al) repeat
    FUNCALL(fn, first (y:=first forwardPointer),x) =>
      newFrontPointer := forwardPointer
      y.rest.first := QSADD1 second y         --increment use count
      return (val:= y)
    if QSLESSP(c := second y,minCount) then --initial c is 1 so is true 1st time
      minCount := c
      newFrontPointer := forwardPointer   --CAR is slot replaced on failure
    forwardPointer:= rest forwardPointer
  if not EQ(newFrontPointer,al) then       --shift referenced entry to front
    temp:= first newFrontPointer           --or entry with smallest count
    newFrontPointer.first := first al
    al.first := temp
  val
 
clamStats() ==
  for [op,kind,:.] in $clamList repeat
    cacheVec:= GETL(op,'cacheInfo) or systemErrorHere ["clamStats",op]
    prefix:=
      $reportCounts ~= true => nil
      hitCounter:= INTERNL(op,'";hit")
      callCounter:= INTERNL(op,'";calls")
      res:= ["%b",eval hitCounter,"/",eval callCounter,"%d","calls to "]
      setDynamicBinding(hitCounter,0)
      setDynamicBinding(callCounter,0)
      res
    postString:=
      cacheValue:= eval cacheVec.cacheName
      kind = 'hash => [" (","%b",HASH_-TABLE_-COUNT cacheValue,"%d","entries)"]
      empties:= numberOfEmptySlots eval cacheVec.cacheName
      empties = 0 => nil
      [" (","%b",kind-empties,"/",kind,"%d","slots used)"]
    sayBrightly
      [:prefix,op,:postString]
 
numberOfEmptySlots cache==
  count:= (CAAR cache ='$failed => 1; 0)
  for x in tails rest cache while NE(x,cache) repeat
    if CAAR x='$failed then count:= count+1
  count
 
addToSlam([name,:argnames],shell) ==
  $mutableDomain => return nil
  null argnames => addToConstructorCache(name,nil,shell)
  args:= ['LIST,:[mkDevaluate a for a in argnames]]
  addToConstructorCache(name,args,shell)
 
addToConstructorCache(op,args,value) ==
  ['haddProp,'$ConstructorCache,MKQ op,args,['CONS,1,value]]
 
haddProp(ht,op,prop,val) ==
  --presently, ht always = $ConstructorCache
  statRecordInstantiationEvent()
  if $reportInstantiations = true or $reportEachInstantiation = true then
    startTimingProcess 'debug
    recordInstantiation(op,prop,false)
    stopTimingProcess 'debug
  u:= HGET(ht,op) =>     --hope that one exists most of the time
    assoc(prop,u) => val     --value is already there--must = val; exit now
    u.rest := [first u,:rest u]
    u.first := [prop,:val]
    $op: local := op
    listTruncate(u,20)        --save at most 20 instantiations
    val
  HPUT(ht,op,[[prop,:val]])
  val
 
recordInstantiation(op,prop,dropIfTrue) ==
  startTimingProcess 'debug
  recordInstantiation1(op,prop,dropIfTrue)
  stopTimingProcess 'debug
 
recordInstantiation1(op,prop,dropIfTrue) ==
  op in '(CategoryDefaults RepeatedSquaring) => nil--ignore defaults for now
  if $reportEachInstantiation = true then
    trailer:= (dropIfTrue => '"  dropped"; '"  instantiated")
    if $insideCoerceInteractive= true then
      $instantCoerceCount:= 1+$instantCoerceCount
    if $insideCanCoerceFrom is [m1,m2] and null dropIfTrue then
      $instantCanCoerceCount:= 1+$instantCanCoerceCount
      xtra:=
        ['" for ",outputDomainConstructor m1,'"-->",outputDomainConstructor m2]
    if $insideEvalMmCondIfTrue = true and null dropIfTrue then
      $instantMmCondCount:= $instantMmCondCount + 1
    typeTimePrin ["CONCAT",outputDomainConstructor [op,:prop],trailer,:xtra]
  null $reportInstantiations => nil
  u:= HGET($instantRecord,op) =>     --hope that one exists most of the time
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
  HPUT($instantRecord,op,[[prop,:val]])
 
reportInstantiations() ==
  --assumed to be a hashtable with reference counts
    conList:=
      [:[[n,m,[key,:argList]] for [argList,n,:m] in HGET($instantRecord,key)]
        for key in HKEYS $instantRecord]
    sayBrightly ['"# instantiated/# dropped/domain name",
      "%l",'"------------------------------------"]
    nTotal:= mTotal:= rTotal := nForms:= 0
    for [n,m,form] in nreverse SORTBY('CADDR,conList) repeat
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
 
hputNewProp(ht,op,argList,val) ==
  --NOTE: obselete if lines *** are commented out
  -- Warning!!!  This function should only be called for
  -- $ConstructorCache slamming --- since it maps devaluate onto prop, an
  -- argument list
  --
  -- This function may be called when property is already there; for
  -- example, Polynomial applied to '(Integer), not finding it in the
  -- cache will invoke Polynomial to compute it; inside of Polynomial is
  -- a call to this function which will hputNewProp the property onto the
  -- cache so that when this function is called by the outer Polynomial,
  -- the value will always be there
 
  prop:= [devaluate x for x in argList]
  haddProp(ht,op,prop,val)
 
listTruncate(l,n) ==
  u:= l
  n:= QSSUB1 n
  while n ~= 0 and null atom u repeat
    n:= QSSUB1 n
    u:= QCDR u
  if null atom u then
    if null atom rest u and $reportInstantiations = true then
      recordInstantiation($op,CAADR u,true)
    u.rest := nil
  l
 
lassocShift(x,l) ==
  y:= l
  while not atom y repeat
    EQUAL(x,first QCAR y) => return (result := QCAR y)
    y:= QCDR y
  result =>
    if not EQ(y,l) then
      y.first := first l
      l.first := result
    QCDR result
  nil
 
lassocShiftWithFunction(x,l,fn) ==
  y:= l
  while not atom y repeat
    FUNCALL(fn,x,first QCAR y) => return (result := QCAR y)
    y:= QCDR y
  result =>
    if not EQ(y,l) then
      y.first := first l
      l.first := result
    QCDR result
  nil
 
lassocShiftQ(x,l) ==
  y:= l
  while not atom y repeat
    EQ(x,first first y) => return (result := first y)
    y:= rest y
  result =>
    if not EQ(y,l) then
      y.first := first l
      l.first := result
    rest result
  nil
 
-- rassocShiftQ(x,l) ==
--   y:= l
--   while not atom y repeat
--     EQ(x,rest first y) => return (result := first y)
--     y:= rest y
--   result =>
--     if not EQ(y,l) then
--       y.first := first l
--       l.first := result
--     first result
--   nil
 
globalHashtableStats(x,sortFn) ==
  --assumed to be a hashtable with reference counts
  keys:= HKEYS x
  for key in keys repeat
    u:= HGET(x,key)
    for [argList,n,:.] in u repeat
      not integer? n =>   keyedSystemError("S2GE0013",[x])
      argList1:= [constructor2ConstructorForm x for x in argList]
      reportList:= [[n,key,argList1],:reportList]
  sayBrightly ["%b","  USE  NAME ARGS","%d"]
  for [n,fn,args] in nreverse SORTBY(sortFn,reportList) repeat
    sayBrightlyNT [:rightJustifyString(n,6),"  ",fn,": "]
    pp args
 
constructor2ConstructorForm x ==
  VECP x => x.0
  x
 
rightJustifyString(x,maxWidth) ==
  size:= entryWidth x
  size > maxWidth => keyedSystemError("S2GE0014",[x])
  [fillerSpaces(maxWidth-size," "),x]
 
domainEqualList(argl1,argl2) ==
  --function used to match argument lists of constructors
  while argl1 and argl2 repeat
    item1:= devaluate first argl1
    item2:= first argl2
    partsMatch:=
      item1 = item2 => true
      false
    null partsMatch => return nil
    argl1:= rest argl1; argl2 := rest argl2
  argl1 or argl2 => nil
  true
 
removeAllClams() ==
  for [fun,:.] in $clamList repeat
    sayBrightly ['"Un-clamming function",'%b,fun,'%d]
    setDynamicBinding(fun,eval INTERN strconc(STRINGIMAGE fun,'";"))