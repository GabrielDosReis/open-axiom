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


import '"g-util"
)package "BOOT"

--% ITERATORS
 
compReduce(form,m,e) ==
 compReduce1(form,m,e,$formalArgList)

compReduce1(form is ["REDUCE",op,.,collectForm],m,e,$formalArgList) ==
  [collectOp,:itl,body]:= collectForm
  if STRINGP op then op:= INTERN op
  ^MEMQ(collectOp,'(COLLECT COLLECTV COLLECTVEC)) =>
        systemError ["illegal reduction form:",form]
  $sideEffectsList: local
  $until: local
  $initList: local
  $endTestList: local
  $e:= e
  itl:= [([.,$e]:= compIterator(x,$e) or return "failed").(0) for x in itl]
  itl="failed" => return nil
  e:= $e
  acc:= GENSYM()
  afterFirst:= GENSYM()
  bodyVal:= GENSYM()
  [part1,m,e]:= comp(["LET",bodyVal,body],m,e) or return nil
  [part2,.,e]:= comp(["LET",acc,bodyVal],m,e) or return nil
  [part3,.,e]:= comp(["LET",acc,parseTran [op,acc,bodyVal]],m,e) or return nil
  identityCode:=
    id:= getIdentity(op,e) => u.expr where u() == comp(id,m,e) or return nil
    ["IdentityError",MKQ op]
  finalCode:=
    ["PROGN",
      ["LET",afterFirst,nil],
       ["REPEAT",:itl,
        ["PROGN",part1,
          ["IF", afterFirst,part3,
                   ["PROGN",part2,["LET",afterFirst,MKQ true]]]]],
                    ["IF",afterFirst,acc,identityCode]]
  if $until then
    [untilCode,.,e]:= comp($until,$Boolean,e)
    finalCode:= substitute(["UNTIL",untilCode],'$until,finalCode)
  [finalCode,m,e]

++ returns the identity element of the `reduction' operation `x'
++ over a list -- a monoid homomorphism.   
getIdentity(x,e) ==
  -- The empty list should be indicated by name, not  by its
  -- object representation.
  GETL(x,"THETA") is [y] => (y => y; "nil")
 
numberize x ==
  x=$Zero => 0
  x=$One => 1
  atom x => x
  [numberize first x,:numberize rest x]
 
compRepeatOrCollect(form,m,e) ==
  fn(form,[m,:$exitModeStack],[#$exitModeStack,:$leaveLevelStack],$formalArgList
    ,e) where
      fn(form,$exitModeStack,$leaveLevelStack,$formalArgList,e) ==
        $until: local
        [repeatOrCollect,:itl,body]:= form
        itl':=
          [([x',e]:= compIterator(x,e) or return "failed"; x') for x in itl]
        itl'="failed" => nil
        targetMode:= first $exitModeStack
        bodyMode:=
          repeatOrCollect="COLLECT" =>
            targetMode = '$EmptyMode => '$EmptyMode
            (u:=modeIsAggregateOf('List,targetMode,e)) =>
              CADR u
            (u:=modeIsAggregateOf('PrimitiveArray,targetMode,e)) =>
              repeatOrCollect:='COLLECTV
              CADR u
            (u:=modeIsAggregateOf('Vector,targetMode,e)) =>
              repeatOrCollect:='COLLECTVEC
              CADR u
            stackMessage('"Invalid collect bodytype")
            return nil
            -- If we're doing a collect, and the type isn't conformable
            -- then we've boobed. JHD 26.July.1990
          $NoValueMode
        [body',m',e']:=
          -- (m1:= listOrVectorElementMode targetMode) and comp(body,m1,e) or
            compOrCroak(body,bodyMode,e) or return nil
        if $until then
          [untilCode,.,e']:= comp($until,$Boolean,e')
          itl':= substitute(["UNTIL",untilCode],'$until,itl')
        form':= [repeatOrCollect,:itl',body']
        m'':=
          repeatOrCollect="COLLECT" =>
            (u:=modeIsAggregateOf('List,targetMode,e)) => CAR u
            ["List",m']
          repeatOrCollect="COLLECTV" =>
            (u:=modeIsAggregateOf('PrimitiveArray,targetMode,e)) => CAR u
            ["PrimitiveArray",m']
          repeatOrCollect="COLLECTVEC" =>
            (u:=modeIsAggregateOf('Vector,targetMode,e)) => CAR u
            ["Vector",m']
          m'
        coerceExit([form',m'',e'],targetMode)
 
--constructByModemap([x,source,e],target) ==
--  u:=
--    [cexpr
--      for (modemap:= [map,cexpr]) in getModemapList("construct",1,e) | map is [
--        .,t,s] and modeEqual(t,target) and modeEqual(s,source)] or return nil
--  fn:= (or/[selfn for [cond,selfn] in u | cond=true]) or return nil
--  [["call",fn,x],target,e]
 
listOrVectorElementMode x ==
  x is [a,b,:.] and member(a,'(PrimitiveArray Vector List)) => b
 
compIterator(it,e) ==
  it is ["IN",x,y] =>
    --these two lines must be in this order, to get "for f in list f"
    --to give  an error message if f is undefined
    [y',m,e]:= comp(y,$EmptyMode,e) or return nil
    $formalArgList:= [x,:$formalArgList]
    [mOver,mUnder]:=
      modeIsAggregateOf("List",m,e) or return
         stackMessage ["mode: ",m," must be a list of some mode"]
    if null get(x,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",x,mUnder],$EmptyMode,e) or return nil
    e:= put(x,"value",[genSomeVariable(),mUnder,e],e)
    [y'',m'',e] := coerce([y',m,e], mOver) or return nil
    [["IN",x,y''],e]
  it is ["ON",x,y] =>
    $formalArgList:= [x,:$formalArgList]
    [y',m,e]:= comp(y,$EmptyMode,e) or return nil
    [mOver,mUnder]:=
      modeIsAggregateOf("List",m,e) or return
        stackMessage ["mode: ",m," must be a list of other modes"]
    if null get(x,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",x,m],$EmptyMode,e) or return nil
    e:= put(x,"value",[genSomeVariable(),m,e],e)
    [y'',m'',e] := coerce([y',m,e], mOver) or return nil
    [["ON",x,y''],e]
  it is ["STEP",index,start,inc,:optFinal] =>
    $formalArgList:= [index,:$formalArgList]
    --if all start/inc/end compile as small integers, then loop
    --is compiled as a small integer loop
    final':= nil
    (start':= comp(start,$SmallInteger,e)) and
      (inc':= comp(inc,$NonNegativeInteger,start'.env)) and
        (not (optFinal is [final]) or
          (final':= comp(final,$SmallInteger,inc'.env))) =>
            indexmode:=
              comp(start,$NonNegativeInteger,e) =>
                      $NonNegativeInteger
              $SmallInteger
            if null get(index,"mode",e) then [.,.,e]:=
              compMakeDeclaration([":",index,indexmode],$EmptyMode,
                (final' => final'.env; inc'.env)) or return nil
            e:= put(index,"value",[genSomeVariable(),indexmode,e],e)
            if final' then optFinal:= [final'.expr]
            [["ISTEP",index,start'.expr,inc'.expr,:optFinal],e]
    [start,.,e]:=
      comp(start,$Integer,e) or return
        stackMessage ["start value of index: ",start," must be an integer"]
    [inc,.,e]:=
      comp(inc,$Integer,e) or return
        stackMessage ["index increment:",inc," must be an integer"]
    if optFinal is [final] then
      [final,.,e]:=
        comp(final,$Integer,e) or return
          stackMessage ["final value of index: ",final," must be an integer"]
      optFinal:= [final]
    indexmode:=
      comp(CADDR it,$NonNegativeInteger,e) => $NonNegativeInteger
      $Integer
    if null get(index,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",index,indexmode],$EmptyMode,e) or return nil
    e:= put(index,"value",[genSomeVariable(),indexmode,e],e)
    [["STEP",index,start,inc,:optFinal],e]
  it is ["WHILE",p] =>
    [p',m,e]:=
      comp(p,$Boolean,e) or return
        stackMessage ["WHILE operand: ",p," is not Boolean valued"]
    [["WHILE",p'],e]
  it is ["UNTIL",p] => ($until:= p; ['$until,e])
  it is ["|",x] =>
    u:=
      comp(x,$Boolean,e) or return
        stackMessage ["SUCHTHAT operand: ",x," is not Boolean value"]
    [["|",u.expr],u.env]
  nil
 
--isAggregateMode(m,e) ==
--  m is [c,R] and MEMQ(c,'(Vector List)) => R
--  name:=
--    m is [fn,:.] => fn
--    m="$" => "Rep"
--    m
--  get(name,"value",e) is [c,R] and MEMQ(c,'(Vector List)) => R
 
modeIsAggregateOf(ListOrVector,m,e) ==
  m is [ =ListOrVector,R] => [m,R]
--m = '$EmptyMode => [m,m] I don't think this is correct, breaks POLY +
  m is ["Union",:l] =>
    mList:= [pair for m' in l | (pair:= modeIsAggregateOf(ListOrVector,m',e))]
    1=#mList => first mList
  name:=
    m is [fn,:.] => fn
    m="$" => "Rep"
    m
  get(name,"value",e) is [[ =ListOrVector,R],:.] => [m,R]
 
--% VECTOR ITERATORS
 
--the following 4 functions are not currently used
 
compCollectV(form,m,e) ==
  fn(form,[m,:$exitModeStack],[#$exitModeStack,:$leaveLevelStack],e) where
    fn(form,$exitModeStack,$leaveLevelStack,e) ==
      [repeatOrCollect,it,body]:= form
      [it',e]:= compIteratorV(it,e) or return nil
      m:= first $exitModeStack
      [mOver,mUnder]:= modeIsAggregateOf("Vector",m,e) or $EmptyMode
      [body',m',e']:= compOrCroak(body,mUnder,e) or return nil
      form':= ["COLLECTV",it',body']
      n:=
	it' is ["STEP",.,s,i,f] or it' is ["ISTEP",.,s,i,f] =>
	      computeMaxIndex(s,f,i);
	return nil
      coerce([form',mOver,e'],m)

compIteratorV(it,e) ==
  it is ["STEP",index,start,inc,final] =>
    (start':= comp(start,$Integer,e)) and
      (inc':= comp(inc,$NonNegativeInteger,start'.env)) and
       (final':= comp(final,$Integer,inc'.env)) =>
	indexmode:=
	  comp(start,$NonNegativeInteger,e) => $NonNegativeInteger
	  $Integer
	if null get(index,"mode",e) then [.,.,e]:=
	  compMakeDeclaration([":",index,indexmode],$EmptyMode,final'.env) or
	    return nil
	e:= put(index,"value",[genSomeVariable(),indexmode,e],e)
	[["ISTEP",index,start'.expr,inc'.expr,final'.expr],e]
    [start,.,e]:=
      comp(start,$Integer,e) or return
	stackMessage ["start value of index: ",start," is not an integer"]
    [inc,.,e]:=
      comp(inc,$NonNegativeInteger,e) or return
	stackMessage ["index increment: ",inc," must be a non-negative integer"]
    [final,.,e]:=
      comp(final,$Integer,e) or return
	stackMessage ["final value of index: ",final," is not an integer"]
    indexmode:=
      comp(CADDR it,$NonNegativeInteger,e) => $NonNegativeInteger
      $Integer
    if null get(index,"mode",e) then [.,.,e]:=
      compMakeDeclaration([":",index,indexmode],$EmptyMode,e) or return nil
    e:= put(index,"value",[genSomeVariable(),indexmode,e],e)
    [["STEP",index,start,inc,final],e]
  nil

computeMaxIndex(s,f,i) ==
  i^=1 => cannotDo()
  s=1 => f
  exprDifference(f,exprDifference(s,1))

exprDifference(x,y) ==
  y=0 => x
  FIXP x and FIXP y => DIFFERENCE(x,y)
  ["DIFFERENCE",x,y]
 
