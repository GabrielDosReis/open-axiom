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


import def
import g_-util
namespace BOOT

-- @(#)g-boot.boot      2.2      89/11/02  14:44:09

--% BOOT to LISP Translation
 
-- these supplement those in DEF and MACRO LISP
 
--% Utilities

$inDefLET := false
$inDefIS := false
$letGenVarCounter := 1
$isGenVarCounter := 1 
 
$LET := 'SPADLET    -- LET is a standard macro in Common Lisp
 
nakedEXIT? c ==
  ATOM c => NIL
  [a,:d] := c
  IDENTP a =>
    a = 'EXIT  => true
    a = 'QUOTE => NIL
    a in '(SEQ PROG LAMBDA MLAMBDA LAM) => NIL
    nakedEXIT?(d)
  nakedEXIT?(a) or nakedEXIT?(d)
 
mergeableCOND x ==
  ATOM(x) or x isnt ['COND,:cls] => NIL
  -- to be mergeable, every result must be an EXIT and the last
  -- predicate must be a pair
  ok := true
  while (cls and ok) repeat
    [[p,:r],:cls] := cls
    CONSP QCDR r => ok := NIL
    first(r) isnt ['EXIT,.] => ok := NIL
    NULL(cls) and ATOM(p) => ok := NIL
    NULL(cls) and (p = ''T) => ok := NIL
  ok
 
mergeCONDsWithEXITs l ==
  -- combines things like
  -- (COND (foo (EXIT a)))
  -- (COND (bar (EXIT b)))
  -- into one COND
  NULL l => NIL
  atom l => l
  atom QCDR l => l
  a := QCAR l
  if a is ['COND,:.] then a := flattenCOND a
  am := mergeableCOND a
  rest(l) is [b,:k] and am and mergeableCOND(b) =>
    b:= flattenCOND b
    c := ['COND,:QCDR a,:QCDR b]
    mergeCONDsWithEXITs [flattenCOND c,:k]
  rest(l) is [b] and am =>
    [removeEXITFromCOND flattenCOND ['COND,:QCDR a,[''T,b]]]
  [a,:mergeCONDsWithEXITs rest l]
 
removeEXITFromCOND? c ==
  -- c is '(COND ...)
  -- only can do it if every clause simply EXITs
  ok := true
  c := rest c
  while ok and c repeat
    [[p,:r],:c] := c
    nakedEXIT? p => ok := NIL
    [:f,r1] := r
    nakedEXIT? f => ok := NIL
    r1 isnt ['EXIT,r2] => ok := NIL
    nakedEXIT? r2 => ok := NIL
  ok
 
removeEXITFromCOND c ==
  -- c is '(COND ...)
  z := NIL
  for cl in rest c repeat
    ATOM cl => z := CONS(cl,z)
    cond := QCAR cl
    length1? cl =>
      cond is ["EXIT",:.] => z := CONS(QCDR cond,z)
      z := CONS(cl,z)
    cl' := reverse cl
    lastSE := QCAR cl'
    ATOM lastSE => z := CONS(cl,z)
    lastSE is ["EXIT",:.] =>
      z := CONS(reverse CONS(second lastSE,rest cl'),z)
    z := CONS(cl,z)
  CONS('COND,nreverse z)
 
flattenCOND body ==
  -- transforms nested COND clauses to flat ones, if possible
  body isnt ['COND,:.] => body
  ['COND,:extractCONDClauses body]
 
extractCONDClauses clauses ==
  -- extracts nested COND clauses into a flat structure
  clauses is ['COND, [pred1,:act1],:restClauses] =>
    if act1 is [['PROGN,:acts]] then act1 := acts
    restClauses is [[''T,restCond]] =>
      [[pred1,:act1],:extractCONDClauses restCond]
    [[pred1,:act1],:restClauses]
  [[''T,clauses]]
 
--% COND and IF
 
bootIF c ==
  -- handles IF expressions by turning them into CONDs
  c is [.,p,t] => bootCOND ['COND,[p,t]]
  [.,p,t,e] := c
  bootCOND ['COND,[p,t],[''T,e]]
 
bootCOND c ==
  -- handles COND expressions: c is ['COND,:.]
  cls := rest c
  NULL cls => NIL
  cls is [[''T,r],:.] => r
  [:icls,fcls] := cls
  ncls := NIL
  for cl in icls repeat
    [p,:r] := cl
    ncls :=
      r is [['PROGN,:r1]] => CONS([p,:r1],ncls)
      CONS(cl,ncls)
  fcls := bootPushEXITintoCONDclause fcls
  ncls :=
    fcls is [''T,['COND,:mcls]] =>
      append(reverse mcls,ncls)
    fcls is [''T,['PROGN,:mcls]] =>
      CONS([''T,:mcls],ncls)
    CONS(fcls,ncls)
  ['COND,:reverse ncls]
 
bootPushEXITintoCONDclause e ==
  e isnt [''T,['EXIT,['COND,:cls]]] => e
  ncls := NIL
  for cl in cls repeat
    [p,:r] := cl
    ncls :=
      r is [['EXIT,:.]] => CONS(cl,ncls)
      r is [r1]           => CONS([p,['EXIT,r1]],ncls)
      CONS([p,['EXIT,bootTran ['PROGN,:r]]],ncls)
  [''T,['COND,:nreverse ncls]]
 
--% SEQ and PROGN
 
-- following is a more sophisticated def than that in MACRO LISP
-- it is used for boot code
 
tryToRemoveSEQ e ==
  -- returns e if unsuccessful
  e isnt ['SEQ,cl,:cls] => NIL
  nakedEXIT? cl =>
    cl is ['COND,[p,['EXIT,r]],:ccls] =>
      nakedEXIT? p or nakedEXIT? r => e
      null ccls =>
        bootCOND ['COND,[p,r],[''T,bootSEQ ['SEQ,:cls]]]
      bootCOND ['COND,[p,r],[''T,bootSEQ ['SEQ,['COND,:ccls],:cls]]]
    e
  bootPROGN ['PROGN,cl,bootSEQ ['SEQ,:cls]]
 
bootAbsorbSEQsAndPROGNs e ==
  -- assume e is a list from a SEQ or a PROGN
  ATOM e => e
  [:cls,lcl] := e
  g := [:flatten(f) for f in cls] where
    flatten x ==
      NULL x => NIL
      IDENTP x =>
        MEMQ(x,$labelsForGO) => [x]
        NIL
      ATOM x => NIL
      x is ['PROGN,:pcls,lpcl] =>
        ATOM lpcl => pcls
        rest x
      -- next usually comes about from if foo then bar := zap
      x is ['COND,y,[''T,'NIL]] => [['COND,y]]
      [x]
  while lcl is ['EXIT,f] repeat
    lcl := f
  lcl is ['PROGN,:pcls] => append(g,pcls)
  lcl is ['COND,[''T,:pcls]] => append(g,pcls)
  lcl is ['COND,[pred,['EXIT,h]]] =>
    append(g,[['COND,[pred,h]]])
  append(g,[lcl])
 
bootSEQ e ==
  e := ['SEQ,:mergeCONDsWithEXITs bootAbsorbSEQsAndPROGNs rest e]
  if e is [.,:cls,lcl] and IDENTP lcl and not MEMQ(lcl,$labelsForGO) then
    e := ['SEQ,:cls,['EXIT,lcl]]
  cls := QCDR e
  cls is [['SEQ,:.]] => tryToRemoveSEQ QCAR cls
  cls is [['EXIT,body]] =>
    nakedEXIT? body => bootTran ['SEQ,body]
    body
  not (nakedEXIT?(cls) or "or"/[MEMQ(g,$labelsForGO) for g in cls]) =>
    bootTran ['PROGN,:cls]
  e is ['SEQ,['COND,[pred,['EXIT,r1]]],:r2] =>
    nakedEXIT?(pred) or nakedEXIT?(r1) or nakedEXIT?(r2) =>
      tryToRemoveSEQ e
    bootTran ['COND,[pred,r1],[''T,:r2]]
  tryToRemoveSEQ e
 
bootPROGN e ==
  e := ['PROGN,:bootAbsorbSEQsAndPROGNs rest e]
  [.,:cls] := e
  NULL cls => NIL
  cls is [body] => body
  e
 
--% LET
 
defLetForm(lhs,rhs) ==
--if functionp lhs then
--  sayMSG ['"Danger: Reassigning value to LISP function:",:bright lhs]
  [$LET,lhs,rhs]
 
defLET1(lhs,rhs) ==
  IDENTP lhs         => defLetForm(lhs,rhs)
  lhs is ['FLUID,id] => defLetForm(lhs,rhs)
  IDENTP rhs and not CONTAINED(rhs,lhs) =>
    rhs' := defLET2(lhs,rhs)
    EQCAR(rhs',$LET) => MKPROGN [rhs',rhs]
    rhs' is ["PROGN",:.] => append(rhs',[rhs])
    if IDENTP first rhs' then rhs' := CONS(rhs',NIL)
    MKPROGN [:rhs',rhs]
  rhs is [=$LET,:.] and IDENTP(name := second rhs) =>
    -- handle things like [a] := x := foo
    l1 := defLET1(name,third rhs)
    l2 := defLET1(lhs,name)
    l2 is ["PROGN",:.] => MKPROGN [l1,:rest l2]
    if IDENTP first l2 then l2 := cons(l2,nil)
    MKPROGN [l1,:l2,name]
  g := INTERN STRCONC('"LETTMP#",STRINGIMAGE $letGenVarCounter)
  $letGenVarCounter := $letGenVarCounter + 1
  rhs' := [$LET,g,rhs]
  let' := defLET1(lhs,g)
  let' is ["PROGN",:.] => MKPROGN [rhs',:rest let']
  if IDENTP first let' then let' := CONS(let',NIL)
  MKPROGN [rhs',:let',g]
 
defLET2(lhs,rhs) ==
  IDENTP lhs => defLetForm(lhs,rhs)
  NULL lhs   => NIL
  lhs is ['FLUID,id] => defLetForm(lhs,rhs)
  lhs is [=$LET,a,b] =>
    a := defLET2(a,rhs)
    null (b := defLET2(b,rhs)) => a
    ATOM b => [a,b]
    CONSP QCAR b => CONS(a,b)
    [a,b]
  lhs is ['CONS,var1,var2] =>
    var1 = "." or (var1 is ["QUOTE",:.]) =>
      defLET2(var2,addCARorCDR('CDR,rhs))
    l1 := defLET2(var1,addCARorCDR('CAR,rhs))
    var2 in '(NIL _.) => l1
    if CONSP l1 and ATOM first l1 then l1 := cons(l1,nil)
    IDENTP var2 =>
      [:l1,defLetForm(var2,addCARorCDR('CDR,rhs))]
    l2 := defLET2(var2,addCARorCDR('CDR,rhs))
    if CONSP l2 and ATOM first l2 then l2 := cons(l2,nil)
    append(l1,l2)
  lhs is ['APPEND,var1,var2] =>
    patrev := defISReverse(var2,var1)
    rev := ['REVERSE,rhs]
    g := INTERN STRCONC('"LETTMP#",STRINGIMAGE $letGenVarCounter)
    $letGenVarCounter := $letGenVarCounter + 1
    l2 := defLET2(patrev,g)
    if CONSP l2 and ATOM first l2 then l2 := cons(l2,nil)
    var1 = "." => [[$LET,g,rev],:l2]
    last l2 is [=$LET, =var1, val1] =>
      [[$LET,g,rev],:reverse rest reverse l2,
       defLetForm(var1,['NREVERSE,val1])]
    [[$LET,g,rev],:l2,defLetForm(var1,['NREVERSE,var1])]
  lhs is ['EQUAL,var1] =>
    ['COND,[['EQUAL,var1,rhs],var1]]
  -- let the IS code take over from here
  isPred :=
    $inDefIS => defIS1(rhs,lhs)
    defIS(rhs,lhs)
  ['COND,[isPred,rhs]]
 
defLET(lhs,rhs) ==
  $letGenVarCounter : local := 1
  $inDefLET : local := true
  defLET1(lhs,rhs)
 
addCARorCDR(acc,expr) ==
  atom expr => [acc,expr]
  acc = 'CAR and expr is ["REVERSE",:.] =>
    cons('last,QCDR expr)
  funs := '(CAR CDR CAAR CDAR CADR CDDR CAAAR CADAR CAADR CADDR
            CDAAR CDDAR CDADR CDDDR)
  p := position(QCAR expr,funs)
  p = -1 => [acc,expr]
  funsA := '(CAAR CADR CAAAR CADAR CAADR CADDR CAAAAR CAADAR CAAADR
             CAADDR CADAAR CADDAR CADADR CADDDR)
  funsR := '(CDAR CDDR CDAAR CDDAR CDADR CDDDR CDAAAR CDADAR CDAADR
             CDADDR CDDAAR CDDDAR CDDADR CDDDDR)
  if acc = 'CAR then CONS(funsA.p,QCDR expr)
  else               CONS(funsR.p,QCDR expr)
 
 
--% IS
 
defISReverse(x,a) ==
  -- reverses forms coming from APPENDs in patterns
  -- pretty much just a translation of DEF-IS-REV
  x is ['CONS,:.] =>
    NULL third x => ['CONS,second x, a]
    y := defISReverse(third x, NIL)
    RPLAC(third y,['CONS,second x,a])
    y
  ERRHUH()
 
defIS1(lhs,rhs) ==
  NULL rhs =>
    ['NULL,lhs]
  STRINGP rhs =>
    ['EQ,lhs,['QUOTE,INTERN rhs]]
  NUMBERP rhs =>
    ['EQUAL,lhs,rhs]
  ATOM rhs =>
    ['PROGN,defLetForm(rhs,lhs),''T]
  rhs is ['QUOTE,a] =>
    IDENTP a => ['EQ,lhs,rhs]
    ['EQUAL,lhs,rhs]
  rhs is [=$LET,c,d] =>
    l :=
      $inDefLET => defLET1(c,lhs)
      defLET(c,lhs)
    ['AND,defIS1(lhs,d),MKPROGN [l,''T]]
  rhs is ['EQUAL,a] =>
    ['EQUAL,lhs,a]
  CONSP lhs =>
    g := INTERN STRCONC('"ISTMP#",STRINGIMAGE $isGenVarCounter)
    $isGenVarCounter := $isGenVarCounter + 1
    MKPROGN [[$LET,g,lhs],defIS1(g,rhs)]
  rhs is ['CONS,a,b] =>
    a = "." =>
      NULL b =>
        ['AND,['CONSP,lhs],
              ['EQ,['QCDR,lhs],'NIL]]
      ['AND,['CONSP,lhs],
            defIS1(['QCDR,lhs],b)]
    NULL b =>
      ['AND,['CONSP,lhs],
            ['EQ,['QCDR,lhs],'NIL],_
            defIS1(['QCAR,lhs],a)]
    b = "." =>
      ['AND,['CONSP,lhs],defIS1(['QCAR,lhs],a)]
    a1 := defIS1(['QCAR,lhs],a)
    b1 := defIS1(['QCDR,lhs],b)
    a1 is ['PROGN,c,''T] and b1 is ['PROGN,:cls] =>
      ['AND,['CONSP,lhs],MKPROGN [c,:cls]]
    ['AND,['CONSP,lhs],a1,b1]
  rhs is ['APPEND,a,b] =>
    patrev := defISReverse(b,a)
    g := INTERN STRCONC('"ISTMP#",STRINGIMAGE $isGenVarCounter)
    $isGenVarCounter := $isGenVarCounter + 1
    rev := ['AND,['CONSP,lhs],['PROGN,[$LET,g,['REVERSE,lhs]],''T]]
    l2 := defIS1(g,patrev)
    if CONSP l2 and ATOM first l2 then l2 := cons(l2,nil)
    a = "." => ['AND,rev,:l2]
    ['AND,rev,:l2,['PROGN,defLetForm(a,['NREVERSE,a]),''T]]
  SAY '"WARNING (defIS1): possibly bad IS code being generated"
  DEF_-IS [lhs,rhs]
 
defIS(lhs,rhs) ==
  $isGenVarCounter : local := 1
  $inDefIS : local := true
  defIS1(DEFTRAN lhs,rhs)
 
--% OR and AND
 
bootOR e ==
  -- flatten any contained ORs.
  cls := rest e
  NULL cls => NIL
  NULL rest cls => first cls
  ncls := [:flatten(c) for c in cls] where
    flatten x ==
      x is ['OR,:.] => QCDR x
      [x]
  ['OR,:ncls]
 
bootAND e ==
  -- flatten any contained ANDs.
  cls := rest e
  NULL cls => 'T
  NULL rest cls => first cls
  ncls := [:flatten(c) for c in cls] where
    flatten x ==
      x is ['AND,:.] => QCDR x
      [x]
  ['AND,:ncls]
 
--% Main Transformation Functions
 
bootLabelsForGO e ==
  ATOM e => NIL
  [head,:tail] := e
  IDENTP head =>
    head = 'GO => $labelsForGO := CONS(first tail,$labelsForGO)
    head = 'QUOTE => NIL
    bootLabelsForGO tail
  bootLabelsForGO head
  bootLabelsForGO tail
 
bootTran e ==
  ATOM e => e
  [head,:tail] := e
  head = 'QUOTE => e
  tail := [bootTran t for t in tail]
  e := [head,:tail]
  IDENTP head =>
    head = 'IF    => bootIF e
    head = 'COND  => bootCOND e
    head = 'PROGN => bootPROGN  e
    head = 'SEQ   => bootSEQ  e
    head = 'OR    => bootOR  e
    head = 'AND   => bootAND  e
    e
  [bootTran head,:QCDR e]
 
bootTransform e ==
--NULL $BOOT => e
  $labelsForGO : local := NIL
  bootLabelsForGO e
  bootTran e
