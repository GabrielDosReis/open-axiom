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

import c_-util
namespace BOOT

getOperationAlistFromLisplib x ==
  -- used to be in clammed.boot. Moved on 1/24/94
--+
--  newType? x => getConstructorOperationsFromDB x
  NRTgetOperationAlistFromLisplib x

NRTgetOperationAlistFromLisplib x ==
  u := getConstructorOperationsFromDB x
--  u := removeZeroOneDestructively u
  null u => u          -- this can happen for Object
  CAAR u = '_$unique => rest u
  f:= addConsDB '(NIL T ELT)
  for [op,:sigList] in u repeat
    for items in tails sigList repeat
      [sig,:r] := first items
      if r is [.,:s] then
        if s is [.,:t] then
          if t is [.] then nil
          else RPLACD(s,QCDDR f)
        else RPLACD(r,QCDR f)
      else RPLACD(first items,f)
      RPLACA(items,addConsDB CAR items)
  u and markUnique u

markUnique x ==
  u := first x
  RPLACA(x,'(_$unique))
  RPLACD(x,[u,:rest x])
  rest x

--=======================================================================
--                  Instantiation/Run-Time Operations
--=======================================================================

stuffSlots(dollar,template) ==
  _$: fluid := dollar               --??? substitute
  dollarTail := [dollar]
  for i in 5..MAXINDEX template | item := template.i repeat
    dollar.i :=
      atom item => [SYMBOL_-FUNCTION item,:dollar]
      item is ['QUOTE,x] =>
        x is [.,.,:n] and FIXP n => ['goGet,item,:dollarTail]
        ['SETELT,dollar,i,['evalSlotDomain,item,dollar]]
      item is ['CONS,:.] =>
        item is [.,'IDENTITY,['FUNCALL,a,b]] =>
          b = '$ => ['makeSpadConstant,eval a,dollar,i]
          sayBrightlyNT '"Unexpected constant environment!!"
          pp devaluate b
          nil
        sayBrightlyNT '"Unexpected constant format!!"
        pp devaluate item
        nil
      sayBrightlyNT '"Unidentified stuff:"
      pp item

--------------------> NEW DEFINITION (see interop.boot.pamphlet)
evalSlotDomain(u,dollar) ==
  $returnNowhereFromGoGet: local := false
  $ : fluid := dollar                      -- ??? substitute
  $lookupDefaults : local := nil -- new world
  u = '$ => dollar
  u = "$$" => dollar
  FIXP u =>
    VECP (y := dollar.u) => y
    y is ['SETELT,:.] => eval y--lazy domains need to marked; this is dangerous?
    y is [v,:.] =>
      VECP v => lazyDomainSet(y,dollar,u)   --old style has [$,code,:lazyt]
      IDENTP v and constructor? v 
        or MEMQ(v,'(Record Union Mapping Enumeration)) =>
           lazyDomainSet(y,dollar,u)        --new style has lazyt
      y
    y
  u is ['NRTEVAL,y] => eval  y
  u is ['QUOTE,y] => y
  u is ['Record,:argl] =>
     apply('Record,[[":",tag,evalSlotDomain(dom,dollar)]
                                 for [.,tag,dom] in argl])
  u is ['Union,:argl] and first argl is ['_:,.,.] =>
     APPLY('Union,[['_:,tag,evalSlotDomain(dom,dollar)]
                                 for [.,tag,dom] in argl])
  u is ["Enumeration",:.] => eval u
  u is [op,:argl] => APPLY(op,[evalSlotDomain(x,dollar) for x in argl])
  systemErrorHere '"evalSlotDomain"


--=======================================================================
--                       Loadtime Operations
--=======================================================================
setLoadTime alist ==
  for [nam,:val] in alist repeat setDynamicBinding(nam,eval val)

setLoadTimeQ alist ==
  for [nam,:val] in alist repeat setDynamicBinding(nam,val)

makeTemplate vec ==
--called at instantiation time by setLoadTime
--the form ['makeTemplate,MKQ $template] is recorded by compDefineFunctor1
--  $template is set below in NRTdescendCodeTran and NRTaddDeltaOpt
  newVec := newShell SIZE vec
  for index in 0..MAXINDEX vec repeat
    item := vec.index
    null item => nil
    item is ['local,:.] => nil --this information used to for display of domains
    newVec.index :=
      atom item => item
      null atom first item =>
        [sig,dcIndex,op,:flag] := item
        code := 4*index
        if dcIndex > 0 then
          code := code + 2   --means "bind"
          else dcIndex := -dcIndex
        if flag = 'CONST then code := code + 1 --means "constant"
        sourceIndex := 8192*dcIndex + code
        uniqueSig:= addConsDB sig
        MKQ [op,uniqueSig,:sourceIndex]
      item is ['CONS,:.] =>  item --constant case
      MKQ item
  newVec

makeOpDirect u ==
  [nam,[addForm,:opList]] := u
  opList = 'derived => 'derived
  [[op,:[fn y for y in items]] for [op,:items] in opList] where fn y ==
        [sig,:r] := y
        uniqueSig := addConsDB sig
        predCode := 0
        isConstant := false
        r is [subSig,pred,'Subsumed] => [uniqueSig,'subsumed,addConsDB subSig]
        if r is [n,:s] then
          slot :=
            n is [p,:.] => p  --the CDR is linenumber of function definition
            n
          if s is [pred,:t] then
            predCode := (pred = 'T => 0; mkUniquePred pred)
            if t is [='CONST,:.] then isConstant := true
        index:= 8192*predCode
        if NUMBERP slot and slot ^= 0 then index := index + 2*slot
        if isConstant then index := index + 1
        [uniqueSig,:index]

--=======================================================================
--          Creation of System Sig/Pred Vectors & Hash Tables
--=======================================================================

mkUniquePred pred == putPredHash addConsDB pred

putPredHash pred == --pred MUST have had addConsDB applied to it
  if pred is [op,:u] and MEMQ(op,'(AND OR NOT)) then
    for x in u repeat putPredHash x
  k := HGET($predHash,pred) => k
  HPUT($predHash,pred,$predVectorFrontier)
  if $predVectorFrontier > MAXINDEX $predVector
    then $predVector := extendVectorSize $predVector
  $predVector.$predVectorFrontier := pred
  $predVectorFrontier := $predVectorFrontier + 1
  $predVectorFrontier - 1

extendVectorSize v ==
  n:= MAXINDEX v
  m:= (7*n)/5   -- make 40% longer
  newVec := newShell m
  for i in 0..n repeat newVec.i := v.i
  newVec

mkSigPredVectors() ==
  $predHash:= MAKE_-HASHTABLE 'UEQUAL
  $consDB:= MAKE_-HASHTABLE 'UEQUAL
  $predVectorFrontier:= 1   --slot 0 in vector will be vacant
  $predVector:= newShell 100
  for nam in allConstructors() |
          getConstuctorKindFromDB nam ^= "package" repeat
    for [op,:sigList] in getConstructorOperationsFromDB nam repeat
      for [sig,:r] in sigList repeat
        addConsDB sig
        r is [.,pred,:.] => putPredHash addConsDB pred
  'done

list2LongerVec(u,n) ==
  vec := newShell ((7*n)/5) -- make 40% longer
  for i in 0.. for x in u repeat vec.i := x
  vec

squeezeConsDB u ==
  fn u where fn u ==
    VECP u => for i in 0..MAXINDEX u repeat fn u.i
    PAIRP u =>
      EQ(x := QCAR u,'QUOTE) => RPLAC(CADR u,addConsDB CADR u)
      squeezeConsDB x
      squeezeConsDB QCDR u
    nil
  u

mapConsDB x == [addConsDB y for y in x]
addConsDB x ==
  min x where
    min x ==
      y:=HGET($consDB,x)
      y => y
      PAIRP x =>
        for z in tails x repeat
          u:=min CAR z
          if not EQ(u,CAR z) then RPLACA(z,u)
        HashCheck x
      REFVECP x =>
        for i in 0..MAXINDEX x repeat
          x.i:=min (x.i)
        HashCheck x
      STRINGP x => HashCheck x
      x
    HashCheck x ==
      y:=HGET($consDB,x)
      y => y
      HPUT($consDB,x,x)
      x
  x

--=======================================================================
--               Functions Creating Lisplib Information
--=======================================================================
NRTdescendCodeTran(u,condList) ==
--NRTbuildFunctor calls to fill $template slots with names of compiled functions
  null u => nil
  u is ['LIST] => nil
  u is [op,.,i,a] and MEMQ(op,'(setShellEntry SETELT QSETREFV)) =>
    null condList and a is ['CONS,fn,:.] =>
      RPLACA(u,'LIST)
      RPLACD(u,nil)
      $template.i :=
        fn = 'IDENTITY => a
        fn is ['dispatchFunction,fn'] => fn'
        fn
    nil   --code for this will be generated by the instantiator
  u is ['COND,:c] =>
    for [pred,:y] in c|y repeat NRTdescendCodeTran(first y,[pred,:condList])
  u is ['PROGN,:c] => for x in c repeat NRTdescendCodeTran(x,condList)
  nil

--=======================================================================
--                  Miscellaneous Functions
--=======================================================================
NRTaddInner x ==
--called by genDeltaEntry and others that affect $NRTdeltaList
  PROGN
    atom x => nil
    x is ['Record,:l] =>
      for [.,.,y] in l repeat NRTinnerGetLocalIndex y
    first x in '(Union Mapping _[_|_|_]) =>
      for y in rest x repeat
         y is [":",.,z] => NRTinnerGetLocalIndex z
         NRTinnerGetLocalIndex y
    x is ['SubDomain,y,:.] => NRTinnerGetLocalIndex y
    getConstructorSignature first x is [.,:ml] =>
      for y in rest x for m in ml | not (y = '$) repeat
        isCategoryForm(m,$CategoryFrame) => NRTinnerGetLocalIndex y
    x is ["Enumeration",:.] =>
      for y in rest x repeat NRTinnerGetLocalIndex y
    keyedSystemError("S2NR0003",[x])
  x

-- NRTaddInner should call following function instead of NRTgetLocalIndex
-- This would prevent putting spurious items in $NRTdeltaList
NRTinnerGetLocalIndex x ==
  atom x => x
  -- following test should skip Unions, Records, Mapping
  op := first x
  MEMQ(op,'(Union Record Mapping Enumeration _[_|_|_])) => NRTgetLocalIndex x
  constructor? op => NRTgetLocalIndex x
  NRTaddInner x

assignSlotToPred cond ==
--called by ProcessCond
  cond is ['AND,:u] => ['AND,:[assignSlotToPred x for x in u]]
  cond is ['OR,:u] => ['OR,:[assignSlotToPred x for x in u]]
  cond is ['NOT,u] => ['NOT,assignSlotToPred u]
  thisNeedsTOBeFilledIn()

makeSpadConstant [fn,dollar,slot] ==
  val := FUNCALL(fn,dollar)
  u:= dollar.slot
  RPLACA(u,function IDENTITY)
  RPLACD(u,val)
  val





