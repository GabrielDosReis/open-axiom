-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


import '"i-object"
import '"ptrees"
)package "BOOT"

$useParserSrcPos :=  NIL
$transferParserSrcPos := NIL

--  Making Trees

mkAtree x ==
  -- maker of attrib tree from parser form
  mkAtree1 mkAtreeExpandMacros x

mkAtreeWithSrcPos(form, posnForm) ==
    posnForm and $useParserSrcPos => pf2Atree(posnForm)
    transferSrcPosInfo(posnForm, mkAtree form)

mkAtree1WithSrcPos(form, posnForm) ==
  transferSrcPosInfo(posnForm, mkAtree1 form)

mkAtreeNodeWithSrcPos(form, posnForm) ==
  transferSrcPosInfo(posnForm, mkAtreeNode form)

transferSrcPosInfo(pf, atree) ==
    not (pf and $transferParserSrcPos) => atree
    pos := pfPosOrNopos(pf)
    pfNoPosition?(pos) => atree

    -- following is a hack because parser code for getting filename
    -- seems wrong.
    fn := lnPlaceOfOrigin poGetLineObject(pos)
    if NULL fn or fn = '"strings" then fn := '"console"

    putSrcPos(atree, fn, pfSourceText(pf), pfLinePosn(pos), pfCharPosn(pos))
    atree

mkAtreeExpandMacros x ==
  -- handle macro expansion. if the macros have args we require that
  -- we match the correct number of args
  if x isnt ["MDEF",:.] and x isnt ["DEF",["macro",:.],:.] then
    atom x and (m := isInterpMacro x) =>
      [args,:body] := m
      args => "doNothing"
      x := body
    x is [op,:argl] =>
      op = "QUOTE" => "doNothing"
      op = "where" and argl is [before,after] =>
        -- in a where clause, what follows "where" (the "after" parm
        -- above) might be a local macro, so do not expand the "before"
        -- part yet
        x := [op,before,mkAtreeExpandMacros after]
      argl := [mkAtreeExpandMacros a for a in argl]
      (m := isInterpMacro op) =>
        [args,:body] := m
        #args = #argl =>
          sl := [[a,:s] for a in args for s in argl]
          x := SUBLISNQ(sl,body)
        null args => x := [body,:argl]
        x := [op,:argl]
      x := [mkAtreeExpandMacros op,:argl]
  x

mkAtree1 x ==
  -- first special handler for making attrib tree
  null x => throwKeyedMsg("S2IP0005",['"NIL"])
  VECP x => x
  atom x =>
    x in '(noBranch noMapVal) => x
    x in '(nil true false) => mkAtree2([x],x,NIL)
    x = '_/throwAway =>
      -- don't want to actually compute this
      tree := mkAtree1 '(void)
      putValue(tree,objNewWrap(voidValue(),$Void))
      putModeSet(tree,[$Void])
      tree
    getBasicMode x =>
      v := mkAtreeNode $immediateDataSymbol
      putValue(v,getBasicObject x)
      v
    IDENTP x => mkAtreeNode x
    keyedSystemError("S2II0002",[x])
  x is [op,:argl] => mkAtree2(x,op,argl)
  systemErrorHere '"mkAtree1"

-- mkAtree2 and mkAtree3 were created because mkAtree1 got so big

mkAtree2(x,op,argl) ==
  nargl := #argl
  (op= "-") and (nargl = 1) and (INTEGERP CAR argl) =>
    mkAtree1(MINUS CAR argl)
  op=":" and argl is [y,z] => [mkAtreeNode "Declare",:argl]
  op="COLLECT" => [mkAtreeNode op,:transformCollect argl]
  op= "break" =>
    argl is [.,val] =>
      if val = '$NoValue then val := '(void)
      [mkAtreeNode op,mkAtree1 val]
    [mkAtreeNode op,mkAtree1 '(void)]
  op= "return" =>
    argl is [val] =>
      if val = '$NoValue then val := '(void)
      [mkAtreeNode op,mkAtree1 val]
    [mkAtreeNode op,mkAtree1 '(void)]
  op="exit" => mkAtree1 CADR argl
  op = "QUOTE" => [mkAtreeNode op,:argl]
  op="SEGMENT" =>
    argl is [a] => [mkAtreeNode op, mkAtree1 a]
    z :=
      null argl.1 => nil
      mkAtree1 argl.1
    [mkAtreeNode op, mkAtree1 argl.0,z]
  op in '(pretend is isnt) =>
    [mkAtreeNode op,mkAtree1 first argl,:rest argl]
  op =  "::" =>
    [mkAtreeNode "COERCE",mkAtree1 first argl,CADR argl]
  x is ["@", expr, type] =>
    t := evaluateType unabbrev type
    t = '(DoubleFloat) and expr is [['_$elt, '(Float), 'float], :args] =>
        mkAtree1 [['_$elt, '(DoubleFloat), 'float], :args]
    t = '(DoubleFloat) and INTEGERP expr =>
        v := mkAtreeNode $immediateDataSymbol
        putValue(v,getBasicObject float expr)
        v
    t = '(Float) and INTEGERP expr =>
        mkAtree1 ["::", expr, t]
    typeIsASmallInteger(t) and INTEGERP expr =>
        mkAtree1 ["::", expr, t]
    [mkAtreeNode 'TARGET,mkAtree1 expr, type]
  (op="case") and (nargl = 2)  =>
    [mkAtreeNode "case",mkAtree1 first argl,unabbrev CADR argl]
  op="REPEAT" => [mkAtreeNode op,:transformREPEAT argl]
  op="LET" and argl is [['construct,:.],rhs] =>
    [mkAtreeNode "LET",first argl,mkAtree1 rhs]
  op="LET" and argl is [[":",a,.],rhs] =>
    mkAtree1 ["SEQ",first argl,["LET",a,rhs]]
  op is ['_$elt,D,op1] =>
    op1 is "=" =>
      a' := [mkAtreeNode '_=,:[mkAtree1 arg for arg in argl]]
      [mkAtreeNode "Dollar",D,a']
    [mkAtreeNode "Dollar",D,mkAtree1 [op1,:argl]]
  op='_$elt =>
    argl is [D,a] =>
      INTEGERP a =>
        a = 0 => mkAtree1 [['_$elt,D,'Zero]]
        a = 1 => mkAtree1 [['_$elt,D,'One]]
        t := evaluateType unabbrev [D]
        typeIsASmallInteger(t) and SINTP a =>
            v := mkAtreeNode $immediateDataSymbol
            putValue(v,objNewWrap(a, t))
            v
        mkAtree1 ["*",a,[['_$elt,D,'One]]]
      [mkAtreeNode "Dollar",D,mkAtree1 a]
    keyedSystemError("S2II0003",['"$",argl,
      '"not qualifying an operator"])
  mkAtree3(x,op,argl)

mkAtree3(x,op,argl) ==
  op="REDUCE" and argl is [op1,axis,body] =>
    [mkAtreeNode op,axis,mkAtree1 op1,mkAtree1 body]
  op="has" => [mkAtreeNode op, :argl]
  op="|" => [mkAtreeNode "AlgExtension",:[mkAtree1 arg for arg in argl]]
  op="=" => [mkAtreeNode "equation",:[mkAtree1 arg for arg in argl]]
  op="not" and argl is [["=",lhs,rhs]] =>
    [mkAtreeNode "not",[mkAtreeNode "=",mkAtree1 lhs,mkAtree1 rhs]]
  op="in" and argl is [var ,["SEGMENT",lb,ul]] =>
    upTest:=
      null ul => NIL
      mkLessOrEqual(var,ul)
    lowTest:=mkLessOrEqual(lb,var)
    z :=
      ul => ['and,lowTest,upTest]
      lowTest
    mkAtree1 z
  x is ["IF",p,"noBranch",a] => mkAtree1 ["IF",["not",p],a,"noBranch"]
  x is ["RULEDEF",:.] => [mkAtreeNode "RULEDEF",:CDR x]
  x is ["MDEF",sym,junk1,junk2,val] =>
    -- new macros look like  macro f ==  or macro f(x) ===
    -- so transform into that format
    mkAtree1 ["DEF",["macro",sym],junk1,junk2,val]
  x is ["~=",a,b] => mkAtree1 ["not",["=",a,b]]
  x is ["+->",funargs,funbody] =>
    if funbody is [":",body,type] then
      types := [type]
      funbody := body
    else types := [NIL]
    v := collectDefTypesAndPreds funargs
    types := [:types,:v.1]
    [mkAtreeNode "ADEF",[v.0,types,[NIL for a in types],funbody],
      if v.2 then v.2 else true, false]
  x is ['ADEF,arg,:r] =>
    r := mkAtreeValueOf r
    v :=
      null arg => VECTOR(NIL,NIL,NIL)
      PAIRP arg and rest arg and first arg^= "|" =>
        collectDefTypesAndPreds ['Tuple,:arg]
      null rest arg => collectDefTypesAndPreds first arg
      collectDefTypesAndPreds arg
    [types,:r'] := r
    at := [fn(x,y) for x in rest types for y in v.1]
    r := [[first types,:at],:r']
    [mkAtreeNode "ADEF",[v.0,:r],if v.2 then v.2 else true,false]
  x is ["where",before,after] =>
    [mkAtreeNode "where",before,mkAtree1 after]
  x is ["DEF",["macro",form],.,.,body] =>
    [mkAtreeNode "MDEF",form,body]
  x is ["DEF",a,:r] =>
    r := mkAtreeValueOf r
    a is [op,:arg] =>
      v :=
        null arg => VECTOR(NIL,NIL,NIL)
        PAIRP arg and rest arg and first arg^= "|" =>
          collectDefTypesAndPreds ['Tuple,:arg]
        null rest arg => collectDefTypesAndPreds first arg
        collectDefTypesAndPreds arg
      [types,:r'] := r
      -- see case for ADEF above for defn of fn
      at := [fn(x,y) for x in rest types for y in v.1]
      r := [[first types,:at],:r']
      [mkAtreeNode 'DEF,[[op,:v.0],:r],if v.2 then v.2 else true,false]
    [mkAtreeNode 'DEF,[a,:r],true,false]
--x is ['when,y,pred] =>
--  y isnt ['DEF,a,:r] =>
--    keyedSystemError("S2II0003",['"when",y,'"improper argument form"])
--  a is [op,p1,:pr] =>
--    null pr => mkAtree1 ['DEF,[op,["|",p1,pred]],:r]
--    mkAtree1 ['DEF,[op,["|",['Tuple,p1,:pr],pred]],:r]
--  [mkAtreeNode 'DEF, CDR y,pred,false]
--x is ['otherwise,u] =>
--  throwMessage '"   otherwise is no longer supported."
  z :=
    getBasicMode op =>
      v := mkAtreeNode $immediateDataSymbol
      putValue(v,getBasicObject op)
      v
    atom op => mkAtreeNode op
    mkAtree1 op
  [z,:[mkAtree1 y for y in argl]]
 where
  fn(a,b) ==
    a and b =>
      if a = b then a
      else throwMessage '"   double declaration of parameter"
    a or b

collectDefTypesAndPreds args ==
  -- given an arglist to a DEF-like form, this function returns
  -- a vector of three things:
  --   slot 0: just the variables
  --   slot 1: the type declarations on the variables
  --   slot 2: a predicate for all arguments
  pred := types := vars := NIL
  junk :=
    IDENTP args =>
      types := [NIL]
      vars  := [args]
    args is [":",var,type] =>
      types := [type]
      var is ["|",var',p] =>
        vars := [var']
        pred := addPred(pred,p)
      vars := [var]
    args is ["|",var,p] =>
      pred := addPred(pred,p)
      var is [":",var',type] =>
        types := [type]
        vars := [var']
      var is ['Tuple,:.] or var is ["|",:.] =>
        v := collectDefTypesAndPreds var
        vars  := [:vars,:v.0]
        types := [:types,:v.1]
        pred  := addPred(pred,v.2)
      vars := [var]
      types := [NIL]
    args is ['Tuple,:args'] =>
      for a in args' repeat
        v := collectDefTypesAndPreds a
        vars  := [:vars,first v.0]
        types := [:types,first v.1]
        pred  := addPred(pred,v.2)
    types := [NIL]
    vars  := [args]
  VECTOR(vars,types,pred)
 where
  addPred(old,new) ==
    null new => old
    null old => new
    ['and,old,new]

mkAtreeValueOf l ==
  -- scans for ['valueOf,atom]
  not CONTAINED("valueOf",l) => l
  mkAtreeValueOf1 l

mkAtreeValueOf1 l ==
  null l or atom l or null rest l => l
  l is ["valueOf",u] and IDENTP u =>
    v := mkAtreeNode $immediateDataSymbol
    putValue(v,get(u,"value",$InteractiveFrame) or
      objNewWrap(u,['Variable,u]))
    v
  [mkAtreeValueOf1 x for x in l]

mkLessOrEqual(lhs,rhs) == ["not",["<",rhs,lhs]]

atree2EvaluatedTree x == atree2Tree1(x,true)

atree2Tree1(x,evalIfTrue) ==
  (triple := getValue x) and objMode(triple) ^= $EmptyMode =>
    coerceOrCroak(triple,$OutputForm,$mapName)
  isLeaf x =>
    VECP x => x.0
    x
  [atree2Tree1(y,evalIfTrue) for y in x]

--% Environment Utilities

-- getValueFromEnvironment(x,mode) ==
--   $failure ^= (v := getValueFromSpecificEnvironment(x,mode,$env)) => v
--   $failure ^= (v := getValueFromSpecificEnvironment(x,mode,$e))   => v
--   throwKeyedMsg("S2IE0001",[x])
getValueFromEnvironment(x,mode) ==
  $failure ^= (v := getValueFromSpecificEnvironment(x,mode,$env)) => v
  $failure ^= (v := getValueFromSpecificEnvironment(x,mode,$e))   => v
  null(v := coerceInt(objNew(x, ['Variable, x]), mode)) =>
     throwKeyedMsg("S2IE0001",[x])
  objValUnwrap v

getValueFromSpecificEnvironment(id,mode,e) ==
  PAIRP e =>
    u := get(id,'value,e) =>
      objMode(u) = $EmptyMode =>
        systemErrorHere '"getValueFromSpecificEnvironment"
      v := objValUnwrap u
      mode isnt ['Mapping,:mapSig] => v
      v isnt ['MAP,:.] => v
      v' := coerceInt(u,mode)
      null v' => throwKeyedMsg("S2IC0002",[objMode u,mode])
      objValUnwrap v'

    m := get(id,'mode,e) =>
      -- See if we can make it into declared mode from symbolic form
      -- For example, (x : P[x] I; x + 1)
      if isPartialMode(m) then m' := resolveTM(['Variable,id],m)
      else m' := m
      m' and
        (u := coerceInteractive(objNewWrap(id,['Variable,id]),m')) =>
          objValUnwrap u

      throwKeyedMsg("S2IE0002",[id,m])
    $failure
  $failure

addBindingInteractive(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := ASSQ(var,curContour) =>
    RPLACD(u,proplist)
    e
  RPLAC(CAAR e,[[var,:proplist],:curContour])
  e

augProplistInteractive(proplist,prop,val) ==
  u := ASSQ(prop,proplist) =>
    RPLACD(u,val)
    proplist
  [[prop,:val],:proplist]

getFlag x == get("--flags--",x,$e)

putFlag(flag,value) ==
  $e := put ("--flags--", flag, value, $e)

getI(x,prop) == get(x,prop,$InteractiveFrame)

putI(x,prop,val) == ($InteractiveFrame := put(x,prop,val,$InteractiveFrame))

getIProplist x == getProplist(x,$InteractiveFrame)

removeBindingI x ==
  RPLAC(CAAR $InteractiveFrame,deleteAssocWOC(x,CAAR $InteractiveFrame))

rempropI(x,prop) ==
  id:=
    atom x => x
    first x
  getI(id,prop) =>
    recordNewValue(id,prop,NIL)
    recordOldValue(id,prop,getI(id,prop))
    $InteractiveFrame:= remprop(id,prop,$InteractiveFrame)

remprop(x,prop,e) ==
  u:= ASSOC(prop,pl:= getProplist(x,e)) =>
    e:= addBinding(x,DELASC(first u,pl),e)
    e
  e

fastSearchCurrentEnv(x,currentEnv) ==
  u:= QLASSQ(x,CAR currentEnv) => u
  while (currentEnv:= QCDR currentEnv) repeat
    u:= QLASSQ(x,CAR currentEnv) => u

putIntSymTab(x,prop,val,e) ==
  null atom x => putIntSymTab(first x,prop,val,e)
  pl0 := pl := search(x,e)
  pl :=
    null pl => [[prop,:val]]
    u := ASSQ(prop,pl) =>
      RPLACD(u,val)
      pl
    lp := LASTPAIR pl
    u := [[prop,:val]]
    RPLACD(lp,u)
    pl
  EQ(pl0,pl) => e
  addIntSymTabBinding(x,pl,e)

addIntSymTabBinding(var,proplist,e is [[curContour,:.],:.]) ==
  -- change proplist of var in e destructively
  u := ASSQ(var,curContour) =>
    RPLACD(u,proplist)
    e
  RPLAC(CAAR e,[[var,:proplist],:curContour])
  e


