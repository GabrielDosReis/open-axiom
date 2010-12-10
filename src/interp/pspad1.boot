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


import macros
namespace BOOT

$escapeWords := ["always", "assert", "but", "define", 
  "delay", "do", "except", "export", "extend", "fix", "fluid",
    "from", "generate", "goto", "import", "inline", "never", "select",
       "try", "yield"]
$pileStyle := false
$commentIndentation := 8
$braceIndentation := 8
$doNotResetMarginIfTrue := true
$marginStack := nil
$numberOfSpills := 0
$lineFragmentBuffer:= nil
$pspadRelationAlist := '((_= . _~_=) (_< . _>_=) (_<_= . _>)(_~_= . _=)(_>_= . _<) (_> . _<_=))
$lineBuffer := nil
$formatForcePren := nil
$underScore := char ('__)
$rightBraceFlag := nil 
$semicolonFlag := nil
$newLineWritten := nil
$comments := nil
$noColonDeclaration := false
$renameAlist := '(
  (SmallInteger . SingleInteger)
  (SmallFloat . DoubleFloat)
  (Void . _(_))
  (xquo . exquo)
  (setelt . set_!)
  (_$ . _%)
  (_$_$ . _$)
  (_*_* . _^)
  (_^_= . _~_=)
  (_^ . _~))

--$opRenameAlist := '(
--  (and . AND)
--  (or  . OR)
--  (not . NOT))


--======================================================================
--               Main Translator Function
--======================================================================
--% lisp-fragment to boot-fragment functions
lisp2Boot x ==
                  --entry function
  $fieldNames := nil
  $pilesAreOkHere: local:= true
  $commentsToPrint: local:= nil
  $lineBuffer: local := nil
  $braceStack: local := nil
  $marginStack: local:= [0]
  --$autoLine is true except when inside a try---if true, lines are allowed to break
  $autoLine:= true
  $lineFragmentBuffer:= nil
  $bc:=0     --brace count
  $m:= 0
  $c:= $m
  $numberOfSpills:= 0
  $lineLength:= 80
  format x
  formatOutput reverse $lineFragmentBuffer
  [fragmentsToLine y for y in reverse $lineBuffer]
 
fragmentsToLine fragments ==
  string:= lispStringList2String fragments
  line:= GETSTR 240
  for i in 0..MAXINDEX string repeat line:= SUFFIX(string.i,line)
  line
 
lispStringList2String x ==
  null x => '""
  atom x => STRINGIMAGE x
  rest x => apply(function strconc,MAPCAR(function lispStringList2String,x))
  lispStringList2String first x
 
--% routines for buffer and margin adjustment
 
formatOutput x ==
  for [currentColumn,start,end,stack] in reverse $commentsToPrint repeat
    startY:= rest start
    for [loc,comment] in stack repeat
      commentY:= rest loc
      gap:= startY-commentY
      gap>0 => before:= [[commentY,first loc,gap,comment],:before]
      gap=0 => same:= [[startY,1,gap,comment],:same]
      true => after:= [[startY,first loc,-gap,comment],:after]
  if before then putOut before
  if same then
    [y,:extraLines]:= "append"/[mkCommentLines u for u in orderList same]
    line:= fragmentsToLine x
    x:=
      #line+#y>$lineLength =>
        (y:= strconc(nBlanks $m,y); extraLines:= [y,:extraLines]; x)
      [line,y]
  consLineBuffer x
  for y in extraLines repeat consLineBuffer [y]
  if after then putOut after
  $commentsToPrint:= nil
 
consLineBuffer x ==  $lineBuffer := [x,:$lineBuffer]

putOut x ==
  eject ("min"/[gap for [.,.,gap,:.] in x])
  for u in orderList x repeat addComment u
 
eject n == for i in 2..n repeat consLineBuffer nil
 
addComment u ==
  for x in mkCommentLines u repeat consLineBuffer [x]
 
mkCommentLines [.,n,.,s] ==
  lines:= breakComments s
  lines1:= [fragmentsToLine [nBlanks n,"_{",first lines],:rest lines]
  [:l,last]:= lines1
  [:l,fragmentsToLine [last,"_}"]]
 
breakComments s ==
  n:= containsString(s,PNAME "ENDOFLINECHR") =>
    #s>n+12 => [SUBSTRING(s,0,n),:breakComments SUBSTRING(s,n+12,NIL)]
    [SUBSTRING(s,0,n)]
  [s]
 
containsString(x,y) ==
                       --if string x contains string y, return start index
  for i in 0..MAXINDEX x-MAXINDEX y repeat
    and/[x.(i+j)=y.j for j in 0..MAXINDEX y] => return i
 
--======================================================================
--               Character/String Buffer Functions
--======================================================================
consBuffer item ==
  if item = '"failed" then item := 'failed
  n:=
    string? item => 2+#item
    IDENTP item => #PNAME item
    #STRINGIMAGE item
  columnsLeft:= $lineLength-$c
  if columnsLeft <= 0 and isCloseDelimiter item then $lineLength := $lineLength + 2
  columnsLeft:= $lineLength-$c
  --cheat for semicolons, strings, and delimiters: they are NEVER too long
  not isSpecialBufferItem item and (n>columnsLeft or columnsLeft < 0) =>
    $autoLine =>
                   --is true except within try
      formatOutput reverse $lineFragmentBuffer
      $c:= ($m+2*($numberOfSpills:= $numberOfSpills+1)) rem $lineLength
      $lineFragmentBuffer:= [nBlanks $c]
      consBuffer item
    nil
  $lineFragmentBuffer:=
    null item or IDENTP item => [PNAME item,:$lineFragmentBuffer]
    integer? item or CHARP item => [STRINGIMAGE item,:$lineFragmentBuffer]
    string? item => ["_"",string2PrintImage item,"_"",:$lineFragmentBuffer]
    sayBrightly ['"Unexpected line buffer item: ", STRINGIMAGE item]
    $lineFragmentBuffer
  $rightBraceFlag := item = "}"
  $semicolonFlag  := item = "; "   --prevents consecutive semicolons
  $c:= $c+n
 
isSpecialBufferItem item ==
  item = "; " or string? item => true
  false

isCloseDelimiter item ==
  item = ")" or item = "]" or item = "}" 

--======================================================================
--               Formatting/Line Control Functions
--======================================================================
newLine() ==
  null $autoLine => nil
  $newLineWritten := true
  formatOutput reverse $lineFragmentBuffer
  $lineFragmentBuffer:= [nBlanks $m]
  $c:= $m
 
optNewLine() ==
  $newLineWritten => newLine()
  $c

spillLine() ==
  null $autoLine => nil
  formatOutput reverse $lineFragmentBuffer
  $c:= $m+2*($numberOfSpills:= $numberOfSpills+1)
  $lineFragmentBuffer:= [nBlanks $c]
  $c
 
indent() ==
  $m:= $m+2*($numberOfSpills+1)
  $marginStack:= [$m,:$marginStack]
  $numberOfSpills:= 0
  $m
 
undent() ==
--  $doNotResetMarginIfTrue=true => 
--    pp '"hoho"
--    $c
  $marginStack is [m,:r] =>
    $marginStack := r
    $m := m
  0

spill(fn,a) == 
  u := tryLine FUNCALL(fn,a) => u
  (nearMargin() or spillLine()) and FUNCALL(fn,a)
 
formatSpill(fn,a) ==
  u := tryLine FUNCALL(fn,a) => u
  v := (stay:= nearMargin() or indent() and newLine()) and FUNCALL(fn,a) 
  w := stay or undent()
  v and w
 
formatSpill2(fn,f,a) ==
  u := tryLine FUNCALL(fn,f,a) => u
  v := (stay:= nearMargin() or indent() and newLine()) and FUNCALL(fn,f,a) 
  w := stay or undent()
  v and w
 
nearMargin() ==
  $c=$m or $c=$m+1 => $c
 
--======================================================================
--               Main Formatting Functions
--======================================================================
format(x,:options) ==
  oldC:= $c
  qualification := IFCAR options
  newCOrNil:=
    x is [op,:argl] =>
      if op = "return" then argl := rest argl
      n := #argl
      op is ['elt,y,"construct"] => formatDollar(y,'construct,argl)
      op is ['elt,name,p] and upperCase? STRINGIMAGE(opOf name).0 => 
        formatDollar(name,p,argl)
      op = 'elt and upperCase? STRINGIMAGE(opOf first argl).0 => 
        formatDollar1(first argl,second argl)
      fn:= GETL(op,"PSPAD") => formatFn(fn,x,$m,$c)
      if op in '(AND OR NOT) then op:= DOWNCASE op
      n=1 and GETL(op,'Nud) and (lbp:= formatOpBindingPower(op,"Nud","left")) =>
        formatPrefix(op,first argl,lbp,formatOpBindingPower(op,"Nud","right"),qualification)
      n=2 and (op = '_$ or getOp(op,'Led)) and (lbp:= formatOpBindingPower(op,"Led","left")) =>
        formatInfix(op,argl,lbp,formatOpBindingPower(op,"Led","right"),qualification)
      formatForm x
    formatAtom x
  null newCOrNil => ($c:= oldC; nil)
  null FIXP newCOrNil => error()
  $c:= newCOrNil
 

getOp(op,kind) ==
  kind = 'Led =>
    op in '(_div _exquo) => nil
    GETL(op,'Led)
  GETL(op,'Nud)

formatDollar(name,p,argl) ==
  name := markMacroTran name
  n := #argl
  kind := (n=1 => "Nud"; "Led")
  IDENTP name and GETL(p,kind) => format([p,:argl],name)
  formatForcePren [p,:argl] and 
    (tryLine (format "$$" and formatForcePren name) 
      or (indent() and format "$__" and formatForcePren name and undent()))
 
formatMacroCheck name ==
  atom name => name
  u := or/[x for [x,:y] in $globalMacroStack | y = name] => u
  u := or/[x for [x,:y] in $localMacroStack  | y = name] => u
  [op,:argl] := name
  op in '(Record Union) => 
    pp ['"Cannot find: ",name]
    name
  [op,:[formatMacroCheck x for x in argl]]
  
formatDOLLAR ['DOLLAR,x,y] == formatDollar1(y, x)

formatDollar1(name,arg) ==
  id :=
    IDENTP name => name
    name is [p] and GETL(p,"NILADIC") => p
    name
  format arg and format "$$" and formatForcePren id
 
 
formatForcePren x ==
  $formatForcePren: local := true
  format x
 
formatAtom(x,:options) ==
  if u := LASSOC(x,$renameAlist) then x := u
  null x or isIdentifier x => 
    if MEMQ(x,$escapeWords) then 
      consBuffer $underScore
    consBuffer ident2PrintImage PNAME x
  consBuffer x
 
formatFn(fn,x,$m,$c) == FUNCALL(fn,x)

formatFree(['free,:u]) ==
  format 'free and format " " and formatComma u

formatUnion(['Union,:r]) == 
  $count : local := 0
  formatFormNoColonDecl formatTestForPartial ['Union,:[fn x for x in r]] where fn x ==
    x is [":",y,'Branch] => fn STRINGIMAGE y
    string? x => [":", INTERN x, ['Enumeration,x]]
    x is [":",:.] => x
    tag := INTERN strconc("value",STRINGIMAGE ($count := $count + 1))
    [":", tag, x]      

formatTestForPartial u ==
  u is ['Union,a,b] and b is [":","failed",:.] and a is [":",.,S] =>
    ['Partial, S]
  u

formatEnumeration(y is ['Enumeration,:r]) ==
  r is [x] => format "'" and format INTERN STRINGIMAGE x and format "'"
  formatForm y

formatRecord(u) == formatFormNoColonDecl u

formatFormNoColonDecl u ==
  $noColonDeclaration: local := true
  formatForm u  

formatElt(u) ==  
  u is ["elt",a,b] => formatApplication rest u
  formatForm u
 
formatForm (u) ==
  [op,:argl] := u
  if op in '(Record Union) then 
    $fieldNames := union(getFieldNames argl,$fieldNames)
  MEMQ(op,'(true %true)) => format "true"
  op in '(false nil) => format op
  u='(Zero) => format 0
  u='(One) => format 1
  1=#argl => formatApplication u
  formatFunctionCall u
 
formatFunctionCall u ==
  $pilesAreOkHere: local
  spill("formatFunctionCall1",u)
 
formatFunctionCall1 [op,:argl] ==
--null argl and getConstructorProperty(op,'niladic) => formatOp op
  null argl => 
    GETL(op,"NILADIC") => formatOp op
    formatOp op and format "()"
  formatOp op and formatFunctionCallTail argl 
 
formatFunctionCallTail argl == format "_(" and formatComma argl and format "_)"

formatComma argl == 
  format first argl and (and/[format "," and formatCut x for x in rest argl]) and $c
 
formatOp op ==
  atom op => formatAtom op
  formatPren op
 
formatApplication u ==
  [op,a] := u
  MEMQ(a, $fieldNames) => formatSelection u
  atom op => 
    formatHasDotLeadOp a => formatOpPren(op,a)
    formatApplication0 u
  formatSelection u

formatHasDotLeadOp u ==
  u is [op,:.] and (op = "." or cons? op)

formatApplication0 u ==
--format as f(x) as f x if possible
  $pilesAreOkHere: local
  formatSpill("formatApplication1",u)
 
formatApplication1 u ==
  [op,x] := u
  formatHasDollarOp x or $formatForcePren or 
    pspadBindingPowerOf("left",x) < 1000  => formatOpPren(op,x)
  tryLine (formatOp op and format " ") and 
    (tryLine formatApplication2 x or
      format "(" and formatApplication2 x and format ")")

formatHasDollarOp x ==
  x is ["elt",a,b] and isTypeProbably? a 

isTypeProbably? x ==
  IDENTP x and upperCase? PNAME(x).0

formatOpPren(op,x) == formatOp op and formatPren x

formatApplication2 x ==
  leadOp := 
    x is [['elt,.,y],:.] => y
    opOf x
  leadOp in '(COLLECT LIST construct) or
    pspadBindingPowerOf("left",x)<1000 => formatPren x
  format x

formatDot ["dot",a,x] ==
  tryLine (formatOp a and format ".") and
    atom x => format x
    formatPren x
 
formatSelection u ==
  $pilesAreOkHere: local
  formatSpill("formatSelection1",u)
 
formatSelection1 [f,x] == formatSelectionOp f and format "." and 
    atom x => format x
    formatPren x
 
formatSelectionOp op ==
  op is [f,.] and not GETL(f,'Nud) or 
    1000 < pspadBindingPowerOf("right",op) => formatSelectionOp1 op
  formatPren1("formatSelectionOp1",op)
 
formatSelectionOp1 f ==
  f is [op,:argl] => 
    argl is [a] => 
      cons? op and atom a => formatSelection1 [op,a]
      formatPren f
    format f
  formatOp f
 
formatPren a ==
  $pilesAreOkHere: local
  formatSpill("formatPrenAux",a)
 
formatPrenAux a == format "_(" and format a and format "_)"
 
formatPren1(f,a) ==
  $pilesAreOkHere: local
  formatSpill2("formatPren1Aux",f,a)
 
formatPren1Aux(f,a) == format "_(" and FUNCALL(f,a) and format "_)"
 
formatLeft(fn,x,op,key) ==
  lbp:= formatOpBindingPower(op,key,"left")
  formatOpBindingPower(opOf x,key,"right")<lbp => formatPren1(fn,x)
  FUNCALL(fn,x)
 
formatRight(fn,x,op,key) ==
  --are there exceptional cases where piles are ok?
  x is ["%LET",:.] => FUNCALL(fn,x)
  --decide on basis of binding power whether prens are needed
  rbp := formatOpBindingPower(op,key,"right")
  lbp := formatOpBindingPower(opOf x,key,"left")
  lbp < rbp => formatPren1(fn,x)
  FUNCALL(fn,x)
 
formatCut a == formatSpill("format",a)
 
--======================================================================
--               Prefix/Infix Operators
--======================================================================
formatPrefix(op,arg,lbp,rbp,:options) ==
  qualification := IFCAR options
  $pilesAreOkHere: local
  formatPrefixOp(op,qualification) and
    (rbp>formatGetBindingPowerOf("left",arg) => formatPren arg; format arg)
 
formatPrefixOp(op,:options) ==
  qualification := IFCAR options
  op=char '" " => format " ="
  qualification or GETL(op,"Nud") and not MEMQ(op,$spadTightList) => 
    formatQual(op,qualification) and format " "
  format op

formatQual(op,D) ==
  null D => format op
  format op and format "$$" and format D

formatInfix(op,[a,b],lbp,rbp,:options) ==
  qualification  := IFCAR options
  $pilesAreOkHere: local
  (if formatGetBindingPowerOf("right",a)<lbp then formatPren a else format a) and
    formatInfixOp(op,qualification) and (if rbp>formatGetBindingPowerOf("left",b) 
      then formatPren b else format b)
 
formatGetBindingPowerOf(leftOrRight,x) ==
--  this function is nearly identical with getBindingPowerOf
--    leftOrRight = "left" => 0
--    1
  pspadBindingPowerOf(leftOrRight,x)
 
pspadBindingPowerOf(key,x) ==
  --binding powers can be found in file NEWAUX LISP
  x is ['REDUCE,:.] => (key='left => 130; key='right => 0)
  x is ["REPEAT",:.] => (key="left" => 130; key="right" => 0)
  x is ["COND",:.] => (key="left" => 130; key="right" => 0)
  x is [op,:argl] =>
    if op is [a,:.] then op:= a
    op = 'SLASH => pspadBindingPowerOf(key,["/",:argl]) - 1
    op = 'OVER  => pspadBindingPowerOf(key,["/",:argl])
    (n:= #argl)=1 =>
      key="left" and (m:= pspadOpBindingPower(op,"Nud","left")) => m
      key="right" and (m:= pspadOpBindingPower(op,"Nud","right")) => m
      1000
    n>1 =>
      key="left" and (m:= pspadOpBindingPower(op,"Led","left")) => m
      key="right" and (m:= pspadOpBindingPower(op,"Led","right")) => m
      op="ELT" => 1002
      1000
    1000
  1002

pspadOpBindingPower(op,LedOrNud,leftOrRight) ==
  if op in '(SLASH OVER) then op := "/"
  op in '(_:) and LedOrNud = 'Led =>
    leftOrRight = 'left => 195
    196
  exception:=
    leftOrRight="left" => 0
    105
  bp:=
    leftOrRight="left" => leftBindingPowerOf(op,LedOrNud)
    rightBindingPowerOf(op,LedOrNud)
  bp ~= exception => bp
  1000

formatOpBindingPower(op,key,leftOrRight) ==
  if op in '(SLASH OVER) then op := "/"
  op = '_$ => 1002
  op in '(_:) and key = 'Led =>
    leftOrRight = 'left => 195
    196
  op in '(_~_= _>_=) => 400
  op = "not" and key = "Nud" =>
    leftOrRight = 'left => 1000
    1001
  GETL(op,key) is [.,.,:r] =>
    leftOrRight = 'left => KAR r or 0
    KAR KDR r or 1
  1000
 
formatInfixOp(op,:options) ==
  qualification := IFCAR options
  qualification or 
    (op ~= '_$) and not MEMQ(op,$spadTightList) => format " " and formatQual(op,qualification) and format " "
  format op
 
--======================================================================
--            Special Handlers: DEF forms
--======================================================================

formatDEF def == formatDEF0(def,$DEFdepth + 1)

formatDEF0(["DEF",form,tlist,sclist,body],$DEFdepth) ==
  if not (KAR form in '(Exports Implementation)) then 
    $form := 
      form is [":",a,:.] => a
      form
  con := opOf $form
  $comments: local := SUBST('_$,'_%,getConstructorDocumentationFromDB con)
  $abb :local := constructor? opOf $form
  if $DEFdepth < 2 then
    condoc := (u := LASSOC('constructor,$comments)) and KDR KAR u or ['""]
    $numberOfSpills := -1
    consComments(condoc,'"+++ ")
  form := formatDeftranForm(form,tlist)
  u := ["DEF",form,tlist,sclist,body]
  v := formatDEF1 u => v
  $insideDEF: local := $DEFdepth > 1
  $DEFdepth = 1 =>
    exname := 'Exports
    impname := 'Implementation
    form is [":",.,=exname] or body = impname => nil
    exports :=
      form is [":",a,b] => 
        form := a
        [["MDEF",exname,'(NIL),'(NIL),b]]
      nil
    [op,:argl] := form
--  decls := [x for x in argl | x is [":",:.]]
--  form := [op,:[(a is [":",b,t] => b; a) for a in argl]]
--  $DEFdepth := $DEFdepth - 1  
    formatWHERE(["where",
      ["DEF",[":",form,exname],[nil for x in form],sclist,impname],
        ['PROGN,:exports,["MDEF",impname,'(NIL),'(NIL),body]]])
  $insideTypeExpression: local := true
  body := formatDeftran(body,false)
  body is ["add",a,:b] => formatAddDef(form,a,b) 
--body is ["with",a,:b] => formatWithDef(form,a,b)
  tryBreakNB(format form and format " == ",body,"==","Led") 

formatDEF1 ["DEF",form,tlist,b,body] ==
  $insideDEF: local := $DEFdepth > 1
  $insideEXPORTS: local := form = 'Exports
  $insideTypeExpression: local := true
  form := formatDeftran(form,false)
  body := formatDeftran(body,false)
  ---------> terrible, hideous, but temporary, hack
  if not $insideDEF and body is ['SEQ,:.] then body := ["add", body]
  prefix := (opOf tlist = 'Category => "define "; nil)
  body is ["add",a,b] => formatAddDef(form,a,b)
  body is ["with",a,:b] => formatWithDef(form,a,b,"==",prefix)
  prefix => 
    tryBreak(format prefix and format form and format " == ",body,"==","Led") 
  tryBreak(format form and format " == ",body,"==","Led") 

formatDefForm(form,:options) ==
  prefix := IFCAR options 
  $insideTypeExpression : local := true
  form is [":",form1,["with",a,:b]] => formatWithDef(form1,a,b,":",prefix)
  prefix => format prefix and format form
  format form

formatAddDef(form,a,b) ==
  $insideCAPSULE : local := true
  $insideDEF     : local := false
  formatDefForm form or return nil
  $marginStack := [0]
  $m := $c := 0
  $insideTypeExpression : local := false
  cap := (b => b; "")
  tryBreakNB(newLine() and format "== " and formatLeft("format",a,"add","Led") 
      and format " add ", cap,"add","Led")

formatWithDef(form,a,b,separator,:options) ==
  prefix := IFCAR options
  $insideEXPORTS : local := true
  $insideCAPSULE : local := true
  $insideDEF     : local := false
  $insideTypeExpression : local := false
  a1 := formatWithKillSEQ a
  b => tryBreakNB(formatDefForm(form,prefix) and format separator and format " with " and formatLeft("format",a,"with","Led") 
      and format " with ",first b,"with","Led")
  tryBreak(formatDefForm(form,prefix) and format separator and format " with ",a1,"with","Nud")
 
formatWithKillSEQ x ==
  x is ['SEQ,['exit,.,y]] => ['BRACE, y]
  x

formatBrace ['BRACE, x]  == format "{" and format x and format "}"

formatWith ["with",a,:b] ==
  $pilesAreOkHere: local := true
  b => 
    tryBreakNB(formatLeft("format",a,"with","Led") and format " with ",first b,"with","Led")
  tryBreak(format "with ",a,"with","Nud")

formatWithDefault ["withDefault",a,b] ==
  if a is ['with,:init,["SEQ",:items,["exit",.,x]]] then
    part2 := ["SEQ",:items,x,["exit", nil,["defaultDefs", b]]]
    if IFCAR init then
      a:= IFCAR init
      b:= [part2]
    else
      a := part2
      b := nil
  $pilesAreOkHere: local := true
  b => 
    tryBreakNB(formatLeft("format",a,"with","Led") and format " with ",first b,"with","Led")
  tryBreak(format "with ",a,"with","Nud")

formatDefaultDefs ["default",a, :b] ==
  $insideCAPSULE : local := true
  $insideDEF     : local := false
  $insideTypeExpression : local := false
  b => 
    tryBreak(formatLeft("format",a,"default","Led") and 
      format " default ", first b,"default","Led")
  tryBreak(format "default ",a,"default","Nud")
--format "add " and formatRight("formatPreferPile",a,"add","Nud")   --==> brace

formatAdd ["add",a,:b] ==
  $insideCAPSULE : local := true
  $insideDEF     : local := false
  $insideTypeExpression : local := false
  b => 
    tryBreakNB(formatLeft("format",a,"and","Led") and 
      format " and ", first b,"and","Led")
  tryBreakNB(format "add ",a,"and","Nud")
--format "add " and formatRight("formatPreferPile",a,"add","Nud")   --==> brace

formatMDEF ["MDEF",form,.,.,body] ==
  form is '(Rep) => formatDEF ["DEF",form,nil,nil,body]
  $insideEXPORTS: local := form = 'Exports
  $insideTypeExpression: local := true
  body := formatDeftran(body,false)
  name := opOf form
  tryBreakNB(format name and format " ==> ",body,"==","Led") 
   and ($insideCAPSULE and $c or format(";"))

insideCat() == $insideCategoryIfTrue and not $insideFunctorIfTrue 
   or $noColonDeclaration

formatImport ["import",a] == 
  addFieldNames a
  addFieldNames macroExpand(a,$e)
  format "import from " and formatLocal1 a

addFieldNames a ==
  a is [op,:r] and op in '(Record Union) =>
        $fieldNames := union(getFieldNames r,$fieldNames)
  a is ['List,:b] => addFieldNames b
  nil

getFieldNames r ==
  r is [[":",a,b],:r] => [a,:getFieldNames r]
  nil

formatLocal ["local",a] == format "local " and formatLocal1 a
 
formatLocal1 a ==
  $insideTypeExpression: local := true
  format a

