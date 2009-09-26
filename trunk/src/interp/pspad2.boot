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


import pspad1
namespace BOOT

--======================================================================
--                Constructor Transformation Functions
--======================================================================
formatDeftranForm(form,tlist) ==
  [ttype,:atypeList] := tlist
  if form is [":",f,t] then 
    form := f
    ttype := t
  if form is ['elt,a,b] then    ----> a.b ====> apply(b,a)
    form := 
      isTypeProbably? a =>
        atypeList := REVERSE atypeList
        ["$$", b, a]
      ["apply",a, b]
  op   := KAR form
  argl := KDR form
  if or/[t for t in atypeList] then
    form := [op,:[(t => [":",a,t]; a) for a in argl for t in atypeList]]
  if ttype then form := [":",form,ttype]
  form
 
formatDeftran(u,SEQflag) ==
  u is ['Join,:x] => formatDeftranJoin(u,SEQflag)
  u is ['CATEGORY,kind,:l,x] => formatDeftran(['with,['SEQ,:l,['exit,n,x]]],SEQflag)
  u is ['CAPSULE,:l,x] => formatDeftranCapsule(l,x,SEQflag)
  u is [op,:.] and op in '(rep per) => formatDeftranRepper(u,SEQflag)
  u is [op,:.] and op in '(_: _:_: _pretend _@) => 
    formatDeftranColon(u,SEQflag)
  u is ['PROGN,:l,x] => formatDeftran(['SEQ,:l,['exit,1,x]],SEQflag)
  u is ['SEQ,:l,[.,n,x]] => 
    v := [:l,x]
    a := "APPEND"/[formatDeftranSEQ(x,true) for x in l]
    b := formatDeftranSEQ(x,false)
    if b is [:.,c] and c = '(void) then b := DROP(-1, b)
    [:m,y] := [:a,:b]
    ['SEQ,:m,['exit,n,y]]
--  u is ['not,arg] and (op := LASSOC(KAR arg,'((_= . _~_=) (_< . _>_=)))) =>
--    formatDeftran([op,:rest arg],nil)
  u is ["^",a] => formatDeftran(['not,a],SEQflag)
  u is ["exquo",a,b] => formatDeftran(['xquo,a,b],SEQflag)
  u is ['IF,a,b,c] => 
    a := formatDeftran(a,nil)
    b := formatDeftran(b,nil)
    c := formatDeftran(c,nil)
    null SEQflag and $insideDEF =>
      [:y,last] := formatDeftranIf(a,b,c)
      ['SEQ,:y,['exit,1,last]]
    ['IF,a,b,c]
  u is ['Union,:argl] => 
    ['Union,:[x for a in argl 
      | x := (STRINGP a => [":",INTERN a,'Branch]; formatDeftran(a,nil))]]
  u is [op,:itl,body] and op in '(REPEAT COLLECT) and
    ([nitl,:nbody] := formatDeftranREPEAT(itl,body)) =>
      formatDeftran([op,:nitl,nbody],SEQflag)
  u is [":",a,b] => [":",formatDeftran(a,nil),formatDeftran(markMacroTran(b),nil)]
  u is ["DEF",:.] => formatCapsuleFunction(u)
  u is [op,:argl]=>[formatDeftran(op,nil),:[formatDeftran(x,nil) for x in argl]]
  u = 'nil => 'empty
  u

formatCapsuleFunction ["DEF",form,tlist,b,body] ==
  $insideDEF : local := true
  ["DEF", formatDeftran(form,nil),tlist,b,formatDeftran(body,nil)]
 
formatDeftranCapsule(l,x,SEQflag) ==
  $insideCAPSULE: local := true
  formatDeftran(['SEQ,:l,['exit,1,x]],SEQflag)

formatDeftranRepper([op,a],SEQflag) ==
    a is [op1,b] and op1 in '(rep per) =>
        op = op1 => formatDeftran(a,SEQflag)
        formatDeftran(b,SEQflag)
    a is ["::",b,t] =>
      b := formatDeftran(b,SEQflag)
      t := formatDeftran(t,SEQflag)
      a := ["::",b,t]
      op = "per" and t = "$" or op = "rep" and t = "Rep" => a
      [op,a]
    a is ['SEQ,:r] => ['SEQ,:[formatSeqRepper(op,x) for x in r]]
    a is ['IF,p,b,c] => 
      formatDeftran(['IF,p,[op,b],[op, c]], SEQflag)
    a is ["%LET",a,b] => formatDeftran(["%LET",a,[op,b]],SEQflag)
    a is ['not,[op,a,b]] and (op1 := LASSOC(op,$pspadRelationAlist)) =>
      formatDeftran([op1,a,b],SEQflag)
    a is ["return",n,r] => 
      opOf r in '(true false) => a
      ["return",n,[op,formatDeftran(r,SEQflag)]]
    a is ['error,:.] => a
    [op,formatDeftran(a,SEQflag)]

formatDeftranColon([op,a,t],SEQflag) ==  --op is one of :  ::  pretend  @
    a := formatDeftran(a,SEQflag)
    t := formatDeftran(t,SEQflag)
    a is ["UNCOERCE",b] => b
    a is [op1,b,t1] and t1 = t and op in '(_: _:_: _pretend _@) =>
      op1 = "pretend" or op = "pretend" => ["pretend",b,t]
      null SEQflag and op1 = ":" or op = ":" => ["pretend",b,t]
      a
    a is [=op,b,t1] =>
      t1 = t => a
      [op,b,t]
    t = "$" =>
      a is ['rep,b] => b
      a is ['per,b] => a
      [op,a,t]
    t = "Rep" =>
      a is ['per,b] => b
      a is ['rep,b] => a
      [op,a,t]
    [op,a,t]

formatSeqRepper(op,x) ==
  x is ['exit,n,y] => ['exit,n,[op,formatDeftran(y,nil)]]
  x is ["=>",a,b] => ["=>",formatDeftran(a,nil),[op,formatDeftran(b,nil)]]
  atom x => x
  [formatSeqRepper(op,y) for y in x]
  
formatDeftranJoin(u,SEQflag) ==
  ['Join,:cats,lastcat] := u
  lastcat is ['CATEGORY,kind,:l,x] =>
    cat := 
      rest cats => ['Join,:cats]
      first cats
    formatDeftran(['with,cat,['SEQ,:l,['exit,1,x]]],SEQflag)
  u
 
formatENUM ['MyENUM, x] == format "'" and format x and format "'"

formatDeftranREPEAT(itl,body) == 
--do nothing unless "itl" contains UNTIL statements
  u := [x for x in itl | x is ["UNTIL",p]] or return nil
  nitl := SETDIFFERENCE(itl,u)
  pred := MKPF([p for ['UNTIL,p] in u],'or)
  cond := ['IF,pred,['leave,n,nil],'%noBranch]
  nbody :=
    body is ['SEQ,:l,[.,n,x]] => ['SEQ,:l,x,['exit,n,cond]]
    ['SEQ,body,['exit,n,cond]]
  [nitl,:nbody]

formatDeftranSEQ(x,flag) ==
  u := formatDeftran(x,flag)
  u is ['SEQ,:.] => rest u
  [u]

formatDeftranIf(a,b,c) ==
  b = '%noBranch =>
    a is [op,:r] and (al := '((_= . _~_=) (_< . _>_=) (_> . _<_=));
                      iop := LASSOC(op, al) or rassoc(op, al)) =>
      [["=>",[iop, :r],c]]
    a is [op,r] and op in '(NOT not NULL null) =>
      [["=>", r, c]]
    [["=>", ['not, a], c]]
  post := 
    c = '%noBranch => nil
    c is ['SEQ,:.] => rest c
    [c]
  [["=>",a,b],:post]

formatWHERE ["where",a,b] ==
  $insideTypeExpression: local := nil
  $insideCAPSULE: local := false
  tryBreak(formatLeft("format",a,"where","Led") and format " where ",b,"where","Led")
 
--======================================================================
--              Special Handlers:  Categories
--======================================================================
formatATTRIBUTE ['ATTRIBUTE,att] == format att
 
formatDeftranCategory ['CATEGORY,kind,:items,item] == ["SEQ",:items,["exit",1,item]]

formatCategory ['Category] == format " " and format "Category"
  
formatCATEGORY cat == 
  con := opOf $form
  $comments: local := SUBST('_$,'_%,getConstructorDocumentationFromDB con)
  $insideEXPORTS : local := true
  format ["with",formatDeftranCategory cat]
 
formatSIGNATURE ['SIGNATURE,op,types,:r] ==
  'constant in r => format op and format ": " and (u := format first types) and 
    formatSC() and formatComments(u,op,types)
  format op and format ": " and (u := format ['Mapping,:types]) and formatSC() and
    formatComments(u,op,types) 
  
formatDefault ["default",a] ==
  $insideCategoryIfTrue : local := false
  $insideCAPSULE: local := true
  $insideTypeExpression: local := false
  tryBreak(format "default ",a,"with","Nud")
--======================================================================
--              Special Handlers:  Control Structures
--======================================================================
formatUNCOERCE ['UNCOERCE,x] == format x
 
formatIF ['IF,a,b,c] == 
  c = '%noBranch => formatIF2(a,b,"if ")
  b = '%noBranch => formatIF ['IF,['not,a],c,'%noBranch]
  formatIF2(a,b,"if ") and newLine() and formatIF3 c

formatIF2(a,b,prefix) ==  
  tryBreakNB(format prefix and format a and format " then ",b,"then","Nud")

formatIF3 x == 
  x is ['IF,a,b,c] => 
    c = '%noBranch => tryBreak(format "else if " 
      and format a and format " then ",b,"then","Nud")
    formatIF2(a,b,"else if ") and newLine() and formatIF3 c
  tryBreak(format "else ",x,"else","Nud")
    
formatBlock(l,x) ==
  null l => format x
  $pilesAreOkHere: local
  format "{ " and format first l and
    (and/[formatSC() and format y for y in rest l]) 
      and formatSC() and format x and format " }"
 
formatExit ["exit",.,u] == format u

formatvoid ["void"] == format "()"

formatLeave ["leave",.,u] == format "break"
 
formatCOLLECT u == formatSpill("formatCOLLECT1",u)
 
formatCOLLECT1 ["COLLECT",:iteratorList,body] ==
  $pilesAreOkHere: local
  format "[" and format body and format " " and
    formatSpill("formatIteratorTail",iteratorList)
 
formatIteratorTail iteratorList ==
  formatIterator first iteratorList and
    (and/[format " " and formatIterator x for x in rest iteratorList]) and format "]"

--======================================================================
--                  Special Handlers:  Keywords
--======================================================================
 
formatColon [":",a,b] ==
  b is ['with,c,:d] => formatColonWith(a,c,d)
  if not $insideTypeExpression then 
    insideCat() => nil
    format
      $insideDEF => "local "
      "default "
  op := 
    $insideCAPSULE and not $insideDEF => ": "
    insideCat() => ": "
    ":"
  b := (atom b => b; markMacroTran b)
  a is ['LISTOF,:c] => formatComma c and format ": " and formatLocal1 b
  formatInfix(op,[a, b],formatOpBindingPower(":","Led","left"),
                              formatOpBindingPower(":","Led","right"))

formatColonWith(form,a,b) ==
  con := opOf $form
  $comments: local := SUBST('_$,'_%,getConstructorDocumentationFromDB con)
  $insideEXPORTS : local := true
  $pilesAreOkHere: local := true
  $insideTypeExpression : local := false
  b => tryBreak(formatDefForm form and format ": " 
        and format a and format " with ",first b,"with","Led")
  tryBreak(formatDefForm form and format ": with ",a,"with","Nud")

formatCOND ["COND",:l] ==
  originalC:= $c
  and/[x is [a,[.,.,b]] for x in l] =>
    (originalC=$m or indent() and newLine()) and first l is [a,[.,.,b]] and
      formatIfExit(a,b) and
        (and/[newLine() and formatIfExit(a,b) for [a,[.,.,b]] in rest l]) and (originalC=$m or undent()) and originalC
  formatIfThenElse l

formatPROGN ["PROGN",:l] ==
  l is [:u,x] => formatPiles(u,x)
  error '"formatPROGN"
  
formatELT ["ELT",a,b] == formatApplication [a,b]
 
formatCONS ["CONS",a,b] ==
  $pilesAreOkHere: local
  format "[" and formatConstructItem a and formatTail b
 
formatTail x ==
  null x => format "]"
  format "," and formatTail1 x 
 
formatTail1 x ==
  x is ["CONS",a,b] => formatConstructItem a and formatTail b
  x is ["APPEND",a,b] =>
    null b => formatConstructItem a and format "]"
    format ":" and formatConstructItem a and formatTail b
  format ":" and formatConstructItem x and format "]"
 
-- x = "." => format ""
formatConstructItem x == format x
 
formatLET ["%LET",a,b] == 
  $insideTypeExpression: local := true
  a = "Rep" or atom a and constructor? opOf b =>
    tryBreakNB(formatAtom a and format " == ",b,":=","Led")
  tryBreakNB((IDENTP a => formatAtom a; format a) and format " := ",b,":=","Led")
 
formatIfExit(a,b) ==
                       --called from SCOND or COND only
  $numberOfSpills: local:= 0
  curMargin:= $m
  curMarginStack:= $currentMarginStack
  $doNotResetMarginIfTrue:= true
  format a and format " => " and formatRight("formatCut",b,"=>","Led") =>
    ($currentMarginStack:= curMarginStack; $m:= curMargin)
 
formatIfThenElse x == formatSpill("formatIf1",x)
 
formatIf1 x ==
  x is [[a,:r],:c] and null c =>
    b:=
      r is [:l,s] and l => ['SEQ,:l,['exit,nil,s]]
      first r
    isTrue a => format b
    format "if " and format a and format " then " and format b
  format "if " and format a and
    (tryLine
      (format " then " and format b and format " else "
        and formatIfThenElse c) or spillLine()
          and format " then " and format b and
--           ($c:= $m:= $m+6) and
            ($numberOfSpills:= $numberOfSpills-1)
              and spillLine() and format " else " and formatIfThenElse c)
 
formatQUOTE ["QUOTE",x] == format "('" and format x and format ")"

formatMI ["MI",a,b] == format a

formatMapping ['Mapping,target,:sources] ==
  $noColonDeclaration: local := true
  formatTuple ['Tuple,:sources] and format " -> " and format target
 
formatTuple ['Tuple,:types] ==
  null types => format "()"
  null rest types => format first types
  formatFunctionCallTail types
 
formatConstruct(['construct,:u]) == 
  format "[" and (null u or format first u and
    "and"/[format "," and formatCut x for x in rest u]) and format "]"
 
formatNextConstructItem x ==
  tryLine format x or ($m := $m + 2) and newLine() and format x
 
formatREPEAT ["REPEAT",:iteratorList,body] ==
  tryBreakNB(null iteratorList or (formatIterator first iteratorList and
    (and/[format " " and formatIterator x for x in rest iteratorList]) and format " ")
      and format "repeat ",body,"repeat","Led")
 
formatFATARROW ["=>",a,b] == tryBreak(format a and format " => ",b,"=>","Led")

formatMap ["+->",a,b] == 
  $noColonDeclaration: local := true
  tryBreak(format a and format " +-> ", b, "+->","Led") 

formatREDUCE ["REDUCE",op,.,u] == formatReduce1(op,u)

formatreduce ["reduce",op,u] == formatReduce1(op,u)

formatReduce1(op,u) ==
  if STRINGP op then op := INTERN op
  id := LASSOC(op,
    '((_+ Zero)(_* One)(append . NIL)(gcd Zero) (lcm One) (strconc . "")(lcm One)))
  formatFunctionCall
    id => ['reduce,op,u,id]
    ['reduce,op,u]

formatIterator u ==
  $noColonDeclaration : local := true
  u is ["IN",x,y] =>
    format "for " and formatLeft("format",x,"in","Led") and format " in " and
      formatRight("format",y,"in","Led")
  u is ["WHILE",x] => format "while " and formatRight("format",x,"while","Nud")
  u is ["UNTIL",x] => format "until " and formatRight("format",x,"until","Nud")
  u is ["|",x] => format "| " and formatRight("format",x,"|","Led")
  u is ["STEP",i,init,step,:v] =>
    final := IFCAR v
    format "for " and formatLeft("format",i,"in","Led") and format " in " and
      (seg := ['SEGMENT,init,final]) and (formatStepOne? step => format seg; formatBy ['by,seg,step])
  error "formatIterator"

formatStepOne? step ==
  step = 1 or step = '(One) => true
  step is [op,n,.] and op in '(_:_:  _@) => n = 1 or n = '(One)
  false
 
formatBy ['by,seg,step] == format seg and format " by " and format step
 
formatSCOND ["SCOND",:l] ==
  $pilesAreOkHere =>
                            --called from formatPileLine or formatBlock
                     --if from formatPileLine
    initialC:= $c
    and/[x is [a,["exit",.,b]] for x in l] =>
      first l is [a,["exit",.,b]] and formatIfExit(a,b) and
        (and/[newLine() and formatIfExit(a,b) for [a,["exit",.,b]] in rest l]) and initialC
    formatIfThenElse l and initialC
  and/[x is [a,["exit",.,b]] for x in l] =>
    first l is [a,["exit",.,b]] and formatIfExit(a,b) and
      (and/[format "; " and formatIfExit(a,b) for [a,["exit",.,b]] in rest l]) and $c
   --warning: and/(...) returns T if there are no entries
  formatIfThenElse l
 
formatSEGMENT ["SEGMENT",a,b] ==
  $pilesAreOkHere: local
  (if pspadBindingPowerOf("right",a)<750 then formatPren a else format a) and
    formatInfixOp ".." and
      (null b and $c or
        (if 750>pspadBindingPowerOf("left",b) then formatPren b else format b))
 
formatSexpr x ==
  atom x =>
    null x or IDENTP x => consBuffer ident2PrintImage PNAME x
    consBuffer x
  spill("formatNonAtom",x)
 
formatNonAtom x ==
  format "_(" and formatSexpr first x and
    (and/[format " " and formatSexpr y for y in rest x])
      and (y:= LASTATOM x => format " . "
        and formatSexpr y; true) and format "_)"
 
formatCAPSULE ['CAPSULE,:l,x] == 
  $insideCAPSULE: local := true
  tryLine formatBlock(l,x) or formatPiles(l,x) or spillLine() and formatBlock(l,x)

formatPAREN [.,:argl] == formatFunctionCallTail argl
 
formatSEQ ["SEQ",:l,[.,.,x]] == 
  tryLine formatBlock(l,x) or formatPiles(l,x) or spillLine() and formatBlock(l,x)
 
--======================================================================
--              Comment Handlers
--======================================================================
formatCOMMENT ["COMMENT",x,marg,startXY,endXY,commentStack] ==
  $commentsToPrint:= [[marg,startXY,endXY,commentStack],:$commentsToPrint]
  format x
 
formatComments(u,op,types) ==
  $numberOfSpills :local := $commentIndentation/2 - 1
  not $insideEXPORTS => u
  alist := LASSOC(op,$comments) or
    sayBrightly ['"No documentation for ",op]
    return u
  ftypes := SUBLISLIS($FormalMapVariableList,rest $form,types)
  consComments(LASSOC(ftypes,alist),'"++ ")
  u   
 
consComments(s,plusPlus) ==
  s is [word,:r] and null atom r => consComments(r, plusPlus)
  s := first s
  null s => nil
  s := consCommentsTran s
  indent() and newLine() or return nil
  columnsLeft := $lineLength - $m - 2
  while (m := MAXINDEX s) >= columnsLeft repeat
    k := or/[i for i in (columnsLeft - 1)..1 by -1 | s.i = $charBlank] 
    k := (k => k + 1; columnsLeft)
    piece := SUBSTRING(s,0,k)
    formatDoCommentLine [plusPlus,piece]
    s := SUBSTRING(s,k,nil)
  formatDoCommentLine [plusPlus,s]
  undent()
  $m

consCommentsTran s ==
  m := MAXINDEX s 
  k := or/[i for i in 0..(m - 7) | substring?('"\spad{",s,i)] =>
    r := charPosition(char '_},s,k + 6)
    r = m + 1 => s
    STRCONC(SUBSTRING(s,0,k),'"`",SUBSTRING(s,k+6,r-k-6),'"'",consCommentsTran SUBSTRING(s,r+1,nil))
  s
  
formatDoCommentLine line ==
  $lineBuffer := consLineBuffer [nBlanks $c,:line]
  $c := $m+2*$numberOfSpills

--======================================================================
--                  Pile Handlers
--======================================================================
formatPreferPile y ==
  y is ["SEQ",:l,[.,.,x]] => 
    (u:= formatPiles(l,x)) => u 
    formatSpill("format",y) 
  formatSpill("format",y)
 
formatPiles(l,x) ==
  $insideTypeExpression : local := false
  not $pilesAreOkHere => nil                  
  originalC:= $c
  lines:= [:l,x]
                                                --piles must begin at margin
  originalC=$m or indent() and newLine() or return nil
  null (formatPileLine($m,first lines,false)) => nil
  not (and/[formatPileLine($m,y,true) for y in rest lines]) => nil
  (originalC=$m or undent()) and originalC          --==> brace
 
formatPileLine($m,x,newLineIfTrue) ==
  if newLineIfTrue then newLine() or return nil
  $numberOfSpills: local:= 0
  $newLineWritten := nil
  format x and (x is ['SIGNATURE,:.] or $rightBraceFlag => $c; formatSC())
    and (x is ['DEF,:.] and optNewLine() or $c)

--======================================================================
--                       Utility Functions
--======================================================================
nBlanks m == "STRCONC"/[char('_  ) for i in 1..m]
 
isNewspadOperator op == GETL(op,"Led") or GETL(op,"Nud")
 
isTrue x == x="true" or x is '(QUOTE T)
 
nary2Binary(u,op) ==
  u is [a,b,:t] => (t => nary2Binary([[op,a,b],:t],op); [op,a,b])
  errhuh()
 
string2PrintImage s ==
  u:= GETSTR (2*SIZE s)
  for i in 0..MAXINDEX s repeat
    (if MEMQ(s.i,'(_( _{ _) _} _! _")) then
      SUFFIX('__,u); u:= SUFFIX(s.i,u))
  u
 
ident2PrintImage s ==
  m := MAXINDEX s
  if m > 1 and s.(m - 1) = $underScore then s := STRCONC(SUBSTRING(s,0,m-1),s.m)
  u:= GETSTR (2*SIZE s)
  if not (ALPHA_-CHAR_-P s.(0) or s.(0)=char '"$") then SUFFIX('__,u)
  u:= SUFFIX(s.(0),u)
  for i in 1..MAXINDEX s repeat
    if not (DIGITP s.i or ALPHA_-CHAR_-P s.i or ((c := s.i) = char '?) 
      or (c = char '_!)) then SUFFIX('__,u)
    u:= SUFFIX(s.i,u)
  INTERN u
 
isIdentifier x ==
  IDENTP x =>
    s:= PNAME x
    #s = 0 => nil
    ALPHA_-CHAR_-P s.(0) => and/[s.i ~= char '" " for i in 1..MAXINDEX s]
    #s>1 =>
      or/[ALPHA_-CHAR_-P s.i for i in 1..(m:= MAXINDEX s)] =>
        and/[s.i ~= char '" " for i in 1..m] => true
 
isGensym x == 
  s := STRINGIMAGE x
  n := MAXINDEX s
  s.0 = char '_G and and/[DIGITP s.i for i in 1..n]
 
--======================================================================
--                       Macro Helpers
--======================================================================
tryToFit(s,x) ==
--% try to format on current line; see macro tryLine in file PSPADAUX LISP
  --returns nil if unable to format stuff in x on a single line
  x => ($back:= rest $back; $c)
  restoreState()
  nil
 
restoreState(:options) ==
  back := IFCAR options or $back
  [
    [$lineBuffer, $lineFragmentBuffer,$comments,$marginStack,$braceStack,$DEFdepth,
      $bc,$c,$m,$commentsToPrint,$numberOfSpills,flags], :back]
        := back
  if null options then $back := back
  [$newLineWritten, $autoLine, $rightBraceFlag,
      $semicolonFlag,$insideDEF,$insideTypeExpression,$pilesAreOkHere,
       $insideEXPORTS, $insideCAPSULE, $insideCategoryIfTrue,
         $doNotResetMarginIfTrue,$noColonDeclaration]
           := flags
  nil
 
saveState(:options) ==
  flags := 
    [$newLineWritten, $autoLine, $rightBraceFlag,
      $semicolonFlag,$insideDEF,$insideTypeExpression,$pilesAreOkHere,
       $insideEXPORTS, $insideCAPSULE, $insideCategoryIfTrue,
         $doNotResetMarginIfTrue,$noColonDeclaration]
  newState := 
   [
    [$lineBuffer, $lineFragmentBuffer,$comments,$marginStack,$braceStack,$DEFdepth,
      $bc,$c,$m,$commentsToPrint,$numberOfSpills,flags], :$back]
  if not KAR options then $back := newState
  newState
 
formatSC() ==
  $pileStyle or $semicolonFlag => $c
  format "; "

wrapBraces(x,y,z) == y

formatLB() ==
  $pileStyle => $c
  $numberOfSpills := 
    $c > $lineLength / 2 => $braceIndentation/3 - 1 
    $braceIndentation/2 - 1
  format "{"

restoreC() == --used by macro "embrace"
  originalC := CAR $braceStack
  $braceStack := rest $braceStack
  formatRB originalC
 
saveC() ==  --used by macro "embrace"
  $braceStack := [$c,:$braceStack]

saveD() ==  --used by macro "embrace"
  $braceStack := [$c,:$braceStack]

restoreD() == --used by macro "indentNB"
  originalC := CAR $braceStack
  $braceStack := rest $braceStack
  originalC
 
formatRB(originalC) == --called only by restoreC
  while $marginStack and $m > originalC repeat undent()
  if $m < originalC then $marginStack := [originalC,:$marginStack]
  $m := originalC
  $pileStyle => $m
  newLine() and format "}" and $m    --==> brace

