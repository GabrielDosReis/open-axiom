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

++ List of compiled function names.
$compiledOpNameList := []


isRecurrenceRelation(op,body,minivectorName) ==
  -- returns [body p1 p2 ... pk] for a k-term recurrence relation
  -- where the n-th term is computed using the (n-1)st,...,(n-k)th
  -- whose values are initially computed using the expressions
  -- p1,...,pk respectively; body has #2,#3,... in place of
  -- f(k-1),f(k-2),...

  body isnt ['COND,:pcl] => false
  -- body should have a conditional expression which
  -- gives k boundary values, one general term plus possibly an
  -- "out of domain" condition
  --pcl is [:.,[ ''T,:mess]] and not (CONTAINED('throwMessage,mess) or
  --  CONTAINED('throwKeyedMsg,mess)) => NIL
  pcl := [x for x in pcl | not (x is [''T,:mess] and
    (CONTAINED('throwMessage,mess) or
      CONTAINED('throwKeyedMsg,mess)))]
  integer := eval $Integer
  iequalSlot:=compiledLookupCheck("=",[$Boolean,"$","$"],integer)
  lesspSlot:=compiledLookupCheck("<",[$Boolean,"$","$"],integer)
  notpSlot:= compiledLookupCheck("not",["$","$"],eval $Boolean)
  for [p,c] in pcl repeat
    p is ['SPADCALL,sharpVar,n1,
      ["ELT",["%dynval",=MKQ minivectorName],slot]]
        and EQ(iequalSlot,$minivector.slot) =>
          initList:= [[n1,:c],:initList]
          sharpList := insert(sharpVar,sharpList)
          n:=n1
    miscList:= [[p,c],:miscList]
  miscList isnt [[generalPred,generalTerm]] or sharpList isnt [sharpArg] =>
      return false
    --first general term starts at n

  --Must have at least one special value; insist that they be consecutive
  null initList => false
  specialValues:= MSORT ASSOCLEFT initList
  or/[null integer? n for n in specialValues] => false
  minIndex:= "MIN"/specialValues
  not (and/[i=x for i in minIndex..(minIndex+n-1) for x in specialValues]) =>
    sayKeyedMsg("S2IX0005",
      ["append"/[['" ",sv]  for sv in specialValues]])
    return nil

  --Determine the order k of the recurrence and index n of first general term
  k:= #specialValues
  n:= k+minIndex
  --Check general predicate
  predOk :=
    generalPred = '%true => true
    generalPred is ['SPADCALL,m,=sharpArg,
      ["ELT",["%dynval",=MKQ minivectorName],slot]]
        and EQ(lesspSlot,$minivector.slot)=> m+1
    generalPred is ['SPADCALL,['SPADCALL,=sharpArg,m,
      ["ELT",["%dynval",=MKQ minivectorName],slot]],
        ["ELT",["%dynval",=MKQ minivectorName],notSlot]]
          and EQ(lesspSlot,$minivector.slot)
            and EQ(notpSlot,$minivector.notSlot) => m
    generalPred is ['NOT,['SPADCALL,=sharpArg,m,
      ["ELT",["%dynval",=MKQ minivectorName], =lesspSlot]]]
        and EQ(lesspSlot,$minivector.slot) => m
    return nil
  integer? predOk and predOk ~= n =>
    sayKeyedMsg("S2IX0006",[n,m])
    return nil

  --Check general term for references to just the k previous values
  diffCell:=compiledLookupCheck("-",'($ $ $),integer)
  diffSlot := or/[i for i in 0.. for x in $minivector | EQ(x,diffCell)]
                or return nil
  --Check general term for references to just the k previous values
  sharpPosition := readInteger SUBSTRING(sharpArg,1,nil)
  al:= mkDiffAssoc(op,generalTerm,k,sharpPosition,sharpArg,diffSlot,minivectorName)
  null al => false
  "$failed" in al => false
  body:= generalTerm
  for [a,:b] in al repeat
    body:= substitute(b,a,body)
  result:= [body,sharpArg,n-1,:nreverse [LASSOC(i,initList) or
      systemErrorHere('"isRecurrenceRelation")
        for i in minIndex..(n-1)]]

mkDiffAssoc(op,body,k,sharpPosition,sharpArg,diffSlot,vecname) ==
  -- returns alist which should not have any entries = $failed
  -- form substitution list of the form:
  -- ( ((f (,DIFFERENCE #1 1)) . #2) ((f (,DIFFERENCE #1 2)) . #3) ...)
  --   but also checking that all difference values lie in 1..k
  atom body => nil
  body is ['COND,:pl] =>
    "union"/[mkDiffAssoc(op,c,k,sharpPosition,sharpArg,diffSlot,vecname) for [p,c] in pl]
  body is [fn,:argl] =>
    (fn = op) and argl.(sharpPosition-1) is
      ['SPADCALL,=sharpArg,n,["ELT",["%dynval",=MKQ vecname],=diffSlot]] =>
          NUMP n and n > 0 and n <= k =>
            [[body,:$TriangleVariableList.n]]
          ['$failed]
    "union"/[mkDiffAssoc(op,x,k,sharpPosition,sharpArg,diffSlot,vecname) for x in argl]
  systemErrorHere '"mkDiffAssoc"

reportFunctionCompilation(op,nam,argl,body,isRecursive) ==
  -- for an alternate definition of this function which does not allow
  -- dynamic caching, see SLAMOLD BOOT
--+
  $compiledOpNameList := [nam]
  minivectorName := makeInternalMapMinivectorName nam
  body := substitute(["%dynval",MKQ minivectorName],"$$$",body)
  setDynamicBinding(minivectorName,LIST2VEC $minivector)
  argl := COPY argl     -- play it safe for optimization
  init :=
    not(isRecursive and $compileRecurrence and #argl = 1) => nil
    isRecurrenceRelation(nam,body,minivectorName)
  init => compileRecurrenceRelation(op,nam,argl,body,init)
  cacheCount:= getCacheCount op
  cacheCount = "all" => reportFunctionCacheAll(op,nam,argl,body)
  parms := [:argl,"envArg"]
  cacheCount = 0 or null argl =>
    compileInteractive [nam,["LAMBDA",parms,body]]
    nam
  num :=
    FIXP cacheCount =>
      cacheCount < 1 =>
        keyedSystemError("S2IM0019",[cacheCount,op])
      cacheCount
    keyedSystemError("S2IM0019",[cacheCount,op])
  sayKeyedMsg("S2IX0003",[op,num])
  auxfn := mkAuxiliaryName nam
  g1:= gensym()  --argument or argument list
  [arg,computeValue] :=
    null argl => [nil,[auxfn]]
    argl is [.] => [[g1, 'envArg],[auxfn,g1, 'envArg]]  --g1 is a parameter
    [g1,['APPLX,MKQ auxfn,g1]]          --g1 is a parameter list
  cacheName := mkCacheName nam
  g2:= gensym()  --length of cache or arg-value pair
  g3:= gensym()  --value computed by calling function
  secondPredPair:=
    null argl => [cacheName]
    [["%store",g3,['assocCircular,g1,["%dynval",MKQ cacheName]]],['CDR,g3]]
  thirdPredPair:=
    null argl => ['%true,[['%store,['%dynval,MKQ cacheName],computeValue]]]
    ['%true,
      ['%store,g2,computeValue],
        ["SETQ",g3,
            ["CAR",["%store",["%dynval",MKQ cacheName],['predCircular,["%dynval",cacheName],cacheCount]]]],
          ["RPLACA",g3,g1],
            ["RPLACD",g3,g2],
              g2]
  codeBody:=
    ["PROG",[g2,g3],["RETURN",["COND",secondPredPair,thirdPredPair]]]
  -- cannot use envArg in next statement without redoing much
  -- of above.
  lamex:= ["LAM",arg,codeBody]
  mainFunction:= [nam,lamex]
  computeFunction:= [auxfn,["LAMBDA",parms,body]]
  compileInteractive mainFunction
  compileInteractive computeFunction
  cacheType:= "function"
  cacheResetCode:= ["SETQ",cacheName,['mkCircularAlist,cacheCount]]
  cacheCountCode:= ['countCircularAlist,cacheName,cacheCount]
  cacheVector:=
    mkCacheVec(op,cacheName,cacheType,cacheResetCode,cacheCountCode)
  $e:= put(nam,'cacheInfo, cacheVector,$e)
  eval cacheResetCode
  SETANDFILE(cacheName,mkCircularAlist cacheCount)
  nam
 
getCacheCount fn ==
  n:= LASSOC(fn,$cacheAlist) => n
  $cacheCount
 
reportFunctionCacheAll(op,nam,argl,body) ==
  sayKeyedMsg("S2IX0004",[op])
  auxfn:= mkAuxiliaryName nam
  g1:= gensym()  --argument or argument list
  [arg,computeValue] :=
    null argl => [['envArg],[auxfn, 'envArg]]
    argl is [.] => [[g1, 'envArg],[auxfn,g1, 'envArg]]  --g1 is a parameter
    [g1,["APPLX",MKQ auxfn,g1]]          --g1 is a parameter list
  if null argl then g1:=nil
  cacheName:= mkCacheName nam
  g2:= gensym()  --value computed by calling function
  secondPredPair:= [["SETQ",g2,["HGET",["%dynval",MKQ cacheName],g1]],g2]
  thirdPredPair:= ['%true,["HPUT",['%dynval,MKQ cacheName],g1,computeValue]]
  codeBody:= ["PROG",[g2],["RETURN",["COND",secondPredPair,thirdPredPair]]]
  lamex:= ["LAM",arg,codeBody]
  mainFunction:= [nam,lamex]
  parms := [:argl, "envArg"]
  computeFunction:= [auxfn,["LAMBDA",parms,body]]
  compileInteractive mainFunction
  compileInteractive computeFunction
  cacheType:= 'hash_-table
  cacheResetCode:= ["%store",["%dynval",MKQ cacheName],['hashTable,''EQUAL]]
  cacheCountCode:= ['hashCount,cacheName]
  cacheVector:=
    mkCacheVec(op,cacheName,cacheType,cacheResetCode,cacheCountCode)
  $e:= put(nam,'cacheInfo, cacheVector,$e)
  eval cacheResetCode
  nam
 
hashCount table ==
  +/[ADD1 nodeCount HGET(table,key) for key in HKEYS table]
 
mkCircularAlist n ==
  l:= [[$failed,:$failed] for i in 1..n]
  lastNode(l).rest := l
 
countCircularAlist(cal,n) ==
  +/[nodeCount x for x in cal for i in 1..n]
 
predCircular(al,n) ==
  for i in 1..QSSUB1 n repeat al:= rest al
  al
 
assocCircular(x,al) ==  --like ASSOC except that al is circular
  forwardPointer:= al
  val:= nil
  until EQ(forwardPointer,al) repeat
    EQUAL(CAAR forwardPointer,x) => return (val:= first forwardPointer)
    forwardPointer:= rest forwardPointer
  val
 
compileRecurrenceRelation(op,nam,argl,junk,[body,sharpArg,n,:initCode]) ==
  k:= #initCode
  extraArgumentCode :=
    extraArguments := [x for x in argl | x ~= sharpArg] =>
      extraArguments is [x] => x
      ['LIST,:extraArguments]
    nil
  g:= gensym()
  gIndex:= gensym()
  gsList:= [gensym() for x in initCode]
  auxfn := mkAuxiliaryName(nam)
  $compiledOpNameList := [:$compiledOpNameList,auxfn]
  stateNam:= GENVAR()
  stateVar:= gensym()
  stateVal:= gensym()
  lastArg := INTERNL strconc('"#",STRINGIMAGE QSADD1 # argl)
  decomposeBindings:=
    [[gIndex,["ELT",lastArg,0]],:[[g,["ELT",lastArg,i]]
      for g in gsList for i in 1..]]
  gsRev:= reverse gsList
  rotateCode:= [["%LET",p,q] for p in gsRev for q in [:rest gsRev,g]]
  advanceCode:= ["%LET",gIndex,['ADD1,gIndex]]
 
  newTripleCode := ["LIST",sharpArg,:gsList]
  newStateCode :=
    null extraArguments => ["%store",["%dynval", MKQ stateNam],newTripleCode]
    ["HPUT",["%dynval", MKQ stateNam],extraArgumentCode,newTripleCode]
 
  computeFunction:= [auxfn,["LAM",cargl,cbody]] where
    cargl:= [:argl,lastArg]
    returnValue:= ["PROGN",newStateCode,first gsList]
    cbody:=
      endTest:=
        ["COND", [["EQL",sharpArg,gIndex],['RETURN,returnValue]]]
      newValueCode:= ["%LET",g,substitute(gIndex,sharpArg,
        EQSUBSTLIST(gsList,rest $TriangleVariableList,body))]
      ["%bind",decomposeBindings,
        ['%loop,["WHILE",true],["PROGN",endTest,advanceCode,
          newValueCode,:rotateCode],voidValue()]]
  fromScratchInit:=
    [["%LET",gIndex,n],:[["%LET",g,x] for g in gsList for x in initCode]]
  continueInit:=
    [["%LET",gIndex,["%ELT",stateVar,0]],
      :[["%LET",g,["%ELT",stateVar,i]] for g in gsList for i in 1..]]
  mainFunction:= [nam,["LAM",margl,mbody]] where
    margl:= [:argl,'envArg]
    max:= gensym()
    tripleCode := ["CONS",n,["LIST",:initCode]]
 
    -- initialSetCode initializes the global variable if necessary and
    --  also binds "stateVar" to its current value
    initialSetCode :=
      initialValueCode :=
        extraArguments => ["hashTable",''EQUAL]
        tripleCode
      cacheResetCode := ["%store",["%dynval", MKQ stateNam],initialValueCode]
      ["COND",[["%not",["%and",["BOUNDP",MKQ stateNam], _
                          ["CONSP",["%dynval",MKQ stateNam]]]],    _
                 ["%LET",stateVar,cacheResetCode]], _
             [''T, ["%LET",stateVar,["%dynval",MKQ stateNam]]]]
 
    -- when there are extra arguments, initialResetCode resets "stateVar"
    --  to the hashtable entry for the extra arguments
    initialResetCode :=
      null extraArguments => nil
      [["%LET",stateVar,["OR",
         ["HGET",stateVar,extraArgumentCode],
          ["HPUT",stateVar,extraArgumentCode,tripleCode]]]]
 
    mbody :=
      preset := [initialSetCode,:initialResetCode,["%LET",max,["ELT",stateVar,0]]]
      phrase1:= [["%and",["%LET",max,["ELT",stateVar,0]],["%ige",sharpArg,max]],
                  [auxfn,:argl,stateVar]]
      phrase2:= [["%igt",sharpArg,["SETQ",max,["DIFFERENCE",max,k]]],
                  ["ELT",stateVar,["QSADD1",["QSDIFFERENCE",k,["DIFFERENCE",sharpArg,max]]]]]
      phrase3:= [["%igt",sharpArg,n],[auxfn,:argl,["LIST",n,:initCode]]]
      phrase4:= [["%igt",sharpArg,n-k],
        ["ELT",["LIST",:initCode],["QSDIFFERENCE",n,sharpArg]]]
      phrase5:= ['%true,['recurrenceError,MKQ op,sharpArg]]
      ['PROGN,:preset,['COND,phrase1,phrase2,phrase3,phrase4,phrase5]]
  if $verbose then
    sayKeyedMsg("S2IX0001",[op])
  compileInteractive computeFunction
  compileInteractive mainFunction
  cacheType:= 'recurrence
  cacheCountCode:= ['nodeCount,stateNam]
  cacheVector:= mkCacheVec(op,stateNam,cacheType,cacheResetCode,cacheCountCode)
  $e:= put(nam,'cacheInfo, cacheVector,$e)
  nam
 
nodeCount x == NUMOFNODES x
 
recurrenceError(op,arg) == throwKeyedMsg("S2IX0002",[op,arg])
 
mkCacheVec(op,nam,kind,resetCode,countCode) ==
  [op,nam,kind,resetCode,countCode]
 
-- reportCacheStore vl ==
--   sayMSG concat(centerString('"Name",22,'" "),"   Kind          #Cells")
--   sayMSG concat(centerString('"----",22,'" "),"   ----          ------")
--   for x in vl repeat reportCacheStoreFor x
--
-- op2String op ==
--   u:= linearFormatName op
--   atom u => PNAME u
--   strconc/u
--
-- reportCacheStorePrint(op,kind,count) ==
--   ops:= op2String op
--   opString:= centerString(ops,22,'" ")
--   kindString:= centerString(PNAME kind,10,'" ")
--   countString:= centerString(count,19,'" ")
--   sayMSG concat(opString,kindString,countString)
--
-- reportCacheStoreFor op ==
--   u:= getI(op,'localModemap) =>
--     for [['local,target,:.],[.,fn],:.] in u repeat
--       [op1,cacheName,kind,.,countCode]:= getI(fn,'cacheInfo) or
--         keyedSystemError("S2GE0016",['"reportCacheStoreFor",
--           '"missing cache information vector"])
--       reportCacheStorePrint(op,kind,eval countCode)
--     true
--   u:= getI(op,"cache") =>
--     reportCacheStorePrint(op,'variable,nodeCount u)
--   nil

++ We are about to clear local modemaps associated with `x'.
++ It is insufficient to just remove the internal functions
++ form the 'localModemap property list in the current environment.
++ We also need to clear any Lisp-level function resulting from
++ previous compilations.
reallyClearLocalModemaps x ==
  for mm in get(x,'localModemap,$e) repeat
    FMAKUNBOUND second mm
  $e:= putHist(x,'localModemap,nil,$e)
  

clearCache x ==
  get(x,'localModemap,$e) or get(x,'mapBody,$e) =>
    for [map,:sub] in $mapSubNameAlist repeat
      map=x => _/UNTRACE_,2(sub,NIL)
    $e := reallyClearLocalModemaps x
    $e:= putHist(x,'mapBody,nil,$e)
    $e:= putHist(x,'localVars,nil,$e)
    sayKeyedMsg("S2IX0007",[x])
 
clearLocalModemaps x ==
  u := get(x,"localModemap",$e) =>
    for sub in ASSOCRIGHT $mapSubNameAlist repeat
      _/UNTRACE_,2(sub,NIL)
    $e:= reallyClearLocalModemaps x
    for mm in u repeat
      [.,fn,:.] := mm
      if def:= get(fn,'definition,$e) then
        $e:= putHist(x,'value,objNew(def,$EmptyMode),$e)
      if cacheVec:= get(fn,'cacheInfo,$e) then
        setDynamicBinding(cacheVec.cacheName,NIL)
      -- now clear the property list of the identifier
      $e := addIntSymTabBinding(x,nil,$e)
    sayKeyedMsg("S2IX0007",[x])
    $e
  $e
 
compileInteractive fn ==
  if $InteractiveMode then startTimingProcess 'compilation
  if fn is [.,[bindOp,.,.]] and bindOp in $AbstractionOperator then
    fn := [first fn,declareUnusedParameters second fn]
  if $reportCompilation then
    sayBrightlyI bright '"Generated LISP code for function:"
    pp fn
  optfn :=
     $InteractiveMode => [timedOptimization fn]
     [fn]
  result := compQuietly optfn
  if $InteractiveMode then stopTimingProcess 'compilation
  result
 
clearAllSlams x ==
  fn(x,nil) where
    fn(thoseToClear,thoseCleared) ==
      for x in thoseToClear | not MEMQ(x,thoseCleared) repeat
        slamListName:= mkCacheName x
        setDynamicBinding(slamListName,nil)
        thoseCleared:= ADJOIN(x,thoseCleared)
        someMoreToClear:=
          setDifference(LASSOC(x,$functorDependencyAlist),[:thoseToClear,:
            thoseCleared])
        NCONC(thoseToClear,someMoreToClear)
 
clearSlam("functor")==
  setDynamicBinding(mkCacheName functor,nil)
