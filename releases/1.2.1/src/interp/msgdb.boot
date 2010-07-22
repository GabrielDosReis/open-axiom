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


--% Description of Messages

--% OpenAxiom messages are read from a flat file database and returned
--% as one long string.  They are preceded in the database by a key and
--% this is how they are referenced from code.  For example, one key is
--% S2IL0001 which means:
--%    S2          Scratchpad II designation
--%    I           from the interpreter
--%    L           originally from LISPLIB BOOT
--%    0001        a sequence number

--% Each message may contain formatting codes and and parameter codes.
--% The formatting codes are:
--%    %b          turn on bright printing
--%    %ceoff      turn off centering
--%    %ceon       turn on centering
--%    %d          turn off bright printing
--%    %f          user defined printing
--%    %i          start indentation of 3 more spaces
--%    %l          start a new line
--%    %m          math-print an expression
--%    %rjoff      turn off right justification (actually ragged left)
--%    %rjon       turn on right justification (actually ragged left)
--%    %s          pretty-print as an S-expression
--%    %u          unindent 3 spaces
--%    %x#         insert # spaces

--% The parameter codes look like %1, %2b, %3p, %4m, %5bp, %6s where the
--% digit is the parameter number ans the letters following indicate
--% additional formatting. You can indicate as many additional formatting
--% qualifiers as you like, to the degree they make sense. The "p" code
--% means to call prefix2String on the parameter, a standard way of
--% printing abbreviated types.  The "P" operator maps prefix2String over 
--% its arguments.  The "o" operation formats the argument as an operation 
--% name.  "b" means to print that parameter in
--% a bold (bright) font. "c" means to center that parameter on a
--% new line.  "f" means that the parameter is a list [fn, :args]
--% and that "fn" is to be called on "args" to get the text. "r" means
--% to right justify (ragged left) the argument.

--% Look in the file with the name defined in $defaultMsgDatabaseName
--% above for examples.


import g_-util
namespace BOOT

--% Message Database Code and Message Utility Functions

$msgDatabase := NIL
$cacheMessages := 'T  -- for debugging purposes
$msgAlist := NIL
$msgDatabaseName := NIL
$testingErrorPrefix :=  '"Daly Bug"
$testingSystem := false
$MARG := 0


$texFormatting := false

--% Accessing the Database

string2Words l ==
  i := 0
  [w while wordFrom(l,i) is [w,i]]

wordFrom(l,i) ==
  maxIndex := MAXINDEX l
  k := or/[j for j in i..maxIndex | l.j ^= char ('_ ) ] or return nil
  buf := '""
  while k < maxIndex and (c := l.k) ^= char ('_ ) repeat
    ch :=
      c = char '__   => l.(k := 1+k)  --this may exceed bounds
      c
    buf := STRCONC(buf,ch)
    k := k + 1
  if k = maxIndex and (c := l.k) ^= char ('_ ) then buf := STRCONC(buf,c)
  [buf,k+1]

getKeyedMsg key == fetchKeyedMsg(key,false)

--% Formatting and Printing Keyed Messages

segmentKeyedMsg(msg) == string2Words msg

segmentedMsgPreprocess x ==
  ATOM x => x
  [head,:tail] := x
  center := rightJust := NIL
  if member(head, '(%ceon "%ceon")) then center := true
  if member(head, '(%rjon "%rjon")) then rightJust := true
  center or rightJust =>
    -- start collecting terms
    y := NIL
    ok := true
    while tail and ok repeat
      [t,:tail] := tail
      member(t, '(%ceoff "%ceoff" %rjoff "%rjoff")) => ok := NIL
      y := CONS(segmentedMsgPreprocess t,y)
    head1 := [(center => '"%ce"; '"%rj"),:NREVERSE y]
    NULL tail => [head1]
    [head1,:segmentedMsgPreprocess tail]
  head1 := segmentedMsgPreprocess head
  tail1 := segmentedMsgPreprocess tail
  EQ(head,head1) and EQ(tail,tail1) => x
  [head1,:tail1]

removeAttributes msg ==
    --takes a segmented message and returns it with the attributes
    --separted.
    first msg ^= '"%atbeg" =>
        [msg,NIL]
    attList := []
    until item = '"%atend" repeat
        msg     := rest  msg
        item    := first msg
        attList := [INTERN item,:attList]
    msg := rest msg
    attList := rest attList
    [msg,attList]

substituteSegmentedMsg(msg,args) ==
  -- this does substitution of the parameters
  l := NIL
  nargs := #args
  for x in segmentedMsgPreprocess msg repeat
    -- x is a list
    PAIRP x =>
      l := cons(substituteSegmentedMsg(x,args),l)
    c := x.0
    n := STRINGLENGTH x

    -- x is a special case
    (n > 2) and (c = "%") and (x.1 = "k") =>
        l := NCONC(NREVERSE pkey SUBSTRING(x,2,NIL),l)

    -- ?name gets replaced by '"Push PF10" or '"Type >b (enter)"
    (x.0 = char "?") and n > 1 and (v := pushOrTypeFuture(INTERN x,nil)) =>
      l := NCONC(NREVERSE v,l)

    -- x requires parameter substitution
    (x.0 = char "%") and (n > 1) and (DIGITP x.1) =>
      a := DIG2FIX x.1
      arg :=
        a <= nargs => args.(a-1)
        '"???"
      -- now pull out qualifiers
      q := NIL
      for i in 2..(n-1) repeat q := cons(x.i,q)
      -- Note 'f processing must come first.
      if MEMQ(char 'f,q) then
          arg :=
              PAIRP arg => APPLY(first arg, rest arg)
              arg
      if MEMQ(char 'm,q) then arg := [['"%m",:arg]]
      if MEMQ(char 's,q) then arg := [['"%s",:arg]]
      if MEMQ(char 'p,q) then 
          $texFormatting => arg := prefix2StringAsTeX arg
          arg := prefix2String arg 
      if MEMQ(char 'P,q) then
          $texFormatting => arg := [prefix2StringAsTeX x for x in arg]
          arg := [prefix2String x for x in arg]
      if MEMQ(char 'o, q) and $texFormatting then arg := operationLink(arg)

      if MEMQ(char 'c,q) then arg := [['"%ce",:arg]]
      if MEMQ(char 'r,q) then arg := [['"%rj",:arg]]

      if MEMQ(char 'l,q) then l := cons('"%l",l)
      if MEMQ(char 'b,q) then l := cons('"%b",l)
      --we splice in arguments that are lists
      --if y is not specified, then the adding of blanks is
      --stifled after the first item in the list until the
      --end of the list. (using %n and %y)
      l :=
         PAIRP(arg) =>
           MEMQ(char 'y,q) or (CAR arg = '"%y") or ((LENGTH arg) = 1)  =>
             APPEND(REVERSE arg, l)
           head := first arg
           tail := rest arg
           ['"%y",:APPEND(REVERSE tail, ['"%n",head,:l ]) ]
         cons(arg,l)
      if MEMQ(char 'b,q) then l := cons('"%d",l)
      for ch in '(_. _, _! _: _; _?) repeat
        if MEMQ(char ch,q) then l := cons(ch,l)

    --x is a plain word
    l := cons(x,l)
  addBlanks NREVERSE l

addBlanks msg ==
  -- adds proper blanks
  null PAIRP msg => msg
  null msg => msg
  LENGTH msg = 1 => msg
  blanksOff := false
  x := first msg
  if x = '"%n" then
    blanksOff := true
    msg1 := []
  else
    msg1 := LIST x
  blank := '" "
  for y in rest msg repeat
    member(y,'("%n" %n)) => blanksOff := true
    member(y,'("%y" %y)) => blanksOff  := false
    if noBlankAfterP x or noBlankBeforeP y or blanksOff then
       msg1 := [y,:msg1]
    else
       msg1 := [y,blank,:msg1]
    x := y
  NREVERSE msg1


SETANDFILEQ($msgdbPrims,'( %b %d %l %i %u %U %n %x %ce %rj "%U" "%b" "%d" "%l" "%i" "%u" "%U" "%n" "%x" "%ce" "%rj"))
SETANDFILEQ($msgdbPunct,'(_. _, _! _: _; _? _] _)  "." "," "!" ":" ";" "?" "]" ")"  ))
SETANDFILEQ($msgdbNoBlanksBeforeGroup,['" ", " ", '"%", "%",_
                            :$msgdbPrims, :$msgdbPunct])
SETANDFILEQ($msgdbListPrims,'(%m %s %ce %rj "%m" "%s" "%ce" "%rj"))

noBlankBeforeP word==
    INTP word => false
    member(word,$msgdbNoBlanksBeforeGroup) => true
    if CVECP word and SIZE word > 1 then
       word.0 = char '% and word.1 = char 'x => return true
       word.0 = char " " => return true
    (PAIRP word) and member(CAR word,$msgdbListPrims) => true
    false

$msgdbPunct := '(_[ _(  "[" "(" )
SETANDFILEQ($msgdbNoBlanksAfterGroup,['" ", " ",'"%" ,"%",_
                          :$msgdbPrims,:$msgdbPunct])

noBlankAfterP word==
    INTP word => false
    member(word,$msgdbNoBlanksAfterGroup) => true
    if CVECP word and (s := SIZE word) > 1 then
       word.0 = char '% and word.1 = char 'x => return true
       word.(s-1) = char " " => return true
    (PAIRP word) and member(CAR word, $msgdbListPrims) => true
    false

cleanUpSegmentedMsg msg ==
  -- removes any junk like double blanks
  -- takes a reversed msg and puts it in the correct order
  null PAIRP msg => msg
  blanks := ['" "," "]
  haveBlank := NIL
  prims :=
    '(%b %d %l %i %u %m %ce %rj _
     "%b" "%d" "%l" "%i" "%m" "%u" "%ce" "%rj")
  msg1 := NIL
  for x in msg repeat
    if haveBlank and (member(x,blanks) or member(x,prims)) then
      msg1 := CDR msg1
    msg1 := cons(x,msg1)
    haveBlank := (member(x,blanks) => true; NIL)
  msg1

operationLink name ==
  FORMAT(nil, '"\lispLink{\verb!(|oSearch| _"~a_")!}{~a}",
         name,
         escapeSpecialChars STRINGIMAGE name)

----------------------------------------
buildMessage(msg, args) ==
  substituteSegmentedMsg(segmentKeyedMsg msg,args)

sayPatternMsg(msg,args) ==
  sayMSG flowSegmentedMsg(buildMessage(msg, args),$LINELENGTH,3)

throwPatternMsg(key,args) ==
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayPatternMsg(key,args)
  countError()
  spadThrow()

sayKeyedMsgAsTeX(key, args) == 
  $texFormatting: fluid := true
  sayKeyedMsgLocal(key, args)

sayKeyedMsg(key,args) ==
  $texFormatting: fluid := false
  sayKeyedMsgLocal(key, args)

sayKeyedMsgLocal(key, args) ==
  msg := segmentKeyedMsg getKeyedMsg key
  msg := substituteSegmentedMsg(msg,args)
  if $displayMsgNumber then msg := ['"%b",key,":",'"%d",:msg]
  msg' := flowSegmentedMsg(msg,$LINELENGTH,$MARGIN)
  if $printMsgsToFile then sayMSG2File msg'
  sayMSG msg'

throwKeyedErrorMsg(kind,key,args) ==
  BUMPERRORCOUNT kind
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayKeyedMsg(key,args)
  spadThrow()

throwKeyedMsgSP(key,args,atree) ==
    if atree and (sp := getSrcPos(atree)) then
        sayMSG '" "
        srcPosDisplay(sp)
    throwKeyedMsg(key,args)

throwKeyedMsg(key,args) ==
  $saturn => saturnThrowKeyedMsg(key, args)
  throwKeyedMsg1(key, args)

saturnThrowKeyedMsg(key,args) ==
  SETQ($OutputStream, $texOutputStream)
  last := pushSatOutput("line")
  sayString '"\bgroup\color{red}\begin{list}\item{} "
  sayKeyedMsgAsTeX(key,args)
  sayString '"\end{list}\egroup"
  popSatOutput(last)
  spadThrow()

throwKeyedMsg1(key,args) ==
  SETQ($OutputStream, $texOutputStream)
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayKeyedMsg(key,args)
  countError()
  spadThrow()

throwListOfKeyedMsgs(descKey,descArgs,l) ==
  -- idea is that descKey and descArgs are the message describing
  -- what the list is about and l is a list of [key,args] messages
  -- the messages in the list are numbered and should have a %1 as
  -- the first token in the message text.
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayKeyedMsg(descKey,descArgs)
  sayMSG '" "
  for [key,args] in l for i in 1.. repeat
    n := STRCONC(object2String i,'".")
    sayKeyedMsg(key,[n,:args])
  countError()
  spadThrow()

--  breakKeyedMsg is like throwKeyedMsg except that the user is given
--  a chance to play around in a break loop if $BreakMode is not 'nobreak

breakKeyedMsg(key,args) ==
  BUMPERRORCOUNT "semantic"
  sayKeyedMsg(key,args)
  handleLispBreakLoop($BreakMode)

keyedSystemError(key,args) ==
  $saturn => saturnKeyedSystemError(key, args)
  keyedSystemError1(key, args)

saturnKeyedSystemError(key, args) ==
  SETQ($OutputStream, $texOutputStream)
  sayString '"\bgroup\color{red}"
  sayString '"\begin{verbatim}"
  sayKeyedMsg("S2GE0000",NIL)
  BUMPERRORCOUNT "semantic"
  sayKeyedMsgAsTeX(key,args)
  sayString '"\end{verbatim}"
  sayString '"\egroup"
  handleLispBreakLoop($BreakMode)

keyedSystemError1(key,args) ==
  sayKeyedMsg("S2GE0000",NIL)
  breakKeyedMsg(key,args)

-- these 2 functions control the mode of saturn output.
-- having the stream writing functions control this would
-- be better (eg. sayText, sayCommands)

pushSatOutput(arg) ==
  $saturnMode = arg => arg
  was := $saturnMode
  arg = "verb" => 
    $saturnMode := "verb"
    sayString '"\begin{verbatim}"
    was
  arg = "line" =>
    $saturnMode := "line"
    sayString '"\end{verbatim}"
    was
  sayString FORMAT(nil, '"What is: ~a", $saturnMode)
  $saturnMode
 
popSatOutput(newmode) == 
  newmode = $saturnMode => nil
  newmode = "verb" => 
    $saturnMode := "verb"
    sayString '"\begin{verbatim}"
  newmode = "line" =>
    $saturnMode := "line"
    sayString '"\end{verbatim}"
  sayString FORMAT(nil, '"What is: ~a", $saturnMode)
  $saturnMode

systemErrorHere functionName ==
  keyedSystemError("S2GE0017",[functionName])

isKeyedMsgInDb(key,dbName) ==
  $msgDatabaseName : fluid := pathname dbName
  fetchKeyedMsg(key,true)

getKeyedMsgInDb(key,dbName) ==
  $msgDatabaseName : fluid := pathname dbName
  fetchKeyedMsg(key,false)

sayKeyedMsgFromDb(key,args,dbName) ==
  $msgDatabaseName : fluid := pathname dbName
  msg := segmentKeyedMsg getKeyedMsg key
  msg := substituteSegmentedMsg(msg,args)
  if $displayMsgNumber then msg := ['"%b",key,":",'%d,:msg]
--sayMSG flowSegmentedMsg(msg,$LINELENGTH,3)
  u := flowSegmentedMsg(msg,$LINELENGTH,3)
  sayBrightly u

returnStLFromKey(key,argL,:optDbN) ==
    savedDbN := $msgDatabaseName
    if IFCAR optDbN then
        $msgDatabaseName := pathname CAR optDbN
    text := fetchKeyedMsg(key, false)
    $msgDatabaseName := savedDbN
    text := segmentKeyedMsg text
    text := substituteSegmentedMsg(text,argL)

throwKeyedMsgFromDb(key,args,dbName) ==
  sayMSG '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayKeyedMsgFromDb(key,args,dbName)
  countError()
  spadThrow()

queryUserKeyedMsg(key,args) ==
  -- display message and return reply
  conStream := DEFIOSTREAM ('((DEVICE . CONSOLE) (MODE . INPUT)),120,0)
  sayKeyedMsg(key,args)
  ans := read_-line conStream
  SHUT conStream
  ans

flowSegmentedMsg(msg, len, offset) ==
  -- tries to break a sayBrightly-type input msg into multiple
  -- lines, with offset and given length.
  -- msgs that are entirely centered or right justified are not flowed
  msg is [[ce,:.]] and member(ce, '(%ce "%ce" %rj "%rj")) => msg

  -- if we are formatting latex, then we assume 
  -- that nothing needs to be done
  $texFormatting => msg
  -- msgs that are entirely centered are not flowed
  msg is [[ce,:.]] and ListMember?(ce,'(%ce "%ce")) => msg
 
  potentialMarg := 0
  actualMarg    := 0

  off := (offset <= 0 => '""; fillerSpaces(offset,'" "))
  off1:= (offset <= 1 => '""; fillerSpaces(offset-1,'" "))
  firstLine := true

  PAIRP msg =>
    lnl := offset
    if msg is [a,:.] and member(a,'(%b %d _  "%b" "%d" " ")) then
      nl :=  [off1]
      lnl := lnl - 1
    else nl := [off]
    for f in msg repeat
      member(f,'("%l" %l)) =>
        actualMarg := potentialMarg
        if lnl = 99999 then nl := ['%l,:nl]
        lnl := 99999
      PAIRP(f) and member(CAR(f),'("%m" %m '%ce "%ce" %rj "%rj")) =>
        actualMarg := potentialMarg
        nl := [f,'%l,:nl]
        lnl := 199999
      member(f,'("%i" %i )) =>
        potentialMarg := potentialMarg + 3
        nl := [f,:nl]
      PAIRP(f) and member(CAR(f),'("%t" %t)) =>
        potentialMarg := potentialMarg + CDR f
        nl := [f,:nl]
      sbl := sayBrightlyLength f
      tot := lnl + offset + sbl + actualMarg
      if firstLine then
        firstLine  := false
        offset := offset + offset
        off1   := STRCONC(off, off1)
        off    := STRCONC(off, off)
      if (tot <= len) or (sbl = 1 and tot = len) then
        nl := [f,:nl]
        lnl := lnl + sbl
      else
        member(f,'(%b %d _  "%b" "%d" " ")) =>
          nl := [f,off1,'%l,:nl]
          actualMarg := potentialMarg
          lnl := -1 + offset + sbl
        nl := [f,off,'%l,:nl]
        lnl := offset + sbl
    concat nreverse nl
  concat('%l,off,msg)

--% Other handy things

keyedMsgCompFailure(key,args) ==
  -- Called when compilation fails in such a way that interpret-code
  --  mode might be of some use.
  not $useCoerceOrCroak =>   THROW('coerceOrCroaker, 'croaked)
  if not($Coerce) and  $reportInterpOnly then
    sayKeyedMsg(key,args)
    sayKeyedMsg("S2IB0009",NIL)
  null $compilingMap => THROW('loopCompiler,'tryInterpOnly)
  THROW('mapCompiler,'tryInterpOnly)

keyedMsgCompFailureSP(key,args,atree) ==
  -- Called when compilation fails in such a way that interpret-code
  --  mode might be of some use.
  not $useCoerceOrCroak =>   THROW('coerceOrCroaker, 'croaked)
  if not($Coerce) and  $reportInterpOnly then
    if atree and (sp := getSrcPos(atree)) then
        sayMSG '" "
        srcPosDisplay(sp)
    sayKeyedMsg(key,args)
    sayKeyedMsg("S2IB0009",NIL)
  null $compilingMap => THROW('loopCompiler,'tryInterpOnly)
  THROW('mapCompiler,'tryInterpOnly)

throwKeyedMsgCannotCoerceWithValue(val,t1,t2) ==
  null (val' := coerceInteractive(objNew(val,t1),$OutputForm)) =>
    throwKeyedMsg("S2IC0002",[t1,t2])
  val' := objValUnwrap(val')
  countError()
  throwKeyedMsg("S2IC0003",[t1,t2,val'])

--% Some Standard Message Printing Functions

bright x == ['"%b",:(PAIRP(x) and NULL CDR LASTNODE x => x; [x]),'"%d"]
--bright x == ['%b,:(ATOM x => [x]; x),'%d]

mkMessage msg ==
  msg and (PAIRP msg) and member((first msg),'(%l "%l"))  and
    member((last msg),'(%l "%l")) => concat msg
  concat('%l,msg,'%l)

sayMessage msg == sayMSG mkMessage msg

sayNewLine(out == $OutputStream, margin == nil) ==
  -- Note: this function should *always* be used by sayBrightly and
  -- friends rather than TERPRI --  see bindSayBrightly
  TERPRI(out)
  if margin ^= nil then BLANKS(margin,out)
  nil

sayString(x,out == $OutputStream) ==
  -- Note: this function should *always* be used by sayBrightly and
  -- friends rather than PRINTEXP --  see bindSayBrightly
  PRINTEXP(x,out)

spadStartUpMsgs() ==
  -- messages displayed when the system starts up
  $LINELENGTH < 60 => NIL
  bar := fillerSpaces($LINELENGTH,specialChar 'hbar)
  sayKeyedMsg("S2GL0001",[_*BUILD_-VERSION_*, _*YEARWEEK_*])
  sayMSG bar
  sayKeyedMsg("S2GL0018C",NIL)
  sayKeyedMsg("S2GL0018D",NIL)
  sayKeyedMsg("S2GL0003B",[$opSysName])
  sayMSG bar
  $msgAlist := NIL    -- these msgs need not be saved
  sayMSG " "

HELP() == sayKeyedMsg("S2GL0019",NIL)

version() == _*YEARWEEK_*

--% Some Advanced Formatting Functions

brightPrint(x,out == $OutputStream) ==
  $MARG : local := 0
  for y in x repeat brightPrint0(y,out)
  NIL

brightPrint0(x,out == $OutputStream) ==
  $texFormatting => brightPrint0AsTeX(x,out)
  if IDENTP x then x := PNAME x

  -- if the first character is a backslash and the second is a percent sign,
  -- don't try to give the token any special interpretation. Just print
  -- it without the backslash.

  STRINGP x and STRINGLENGTH x > 1 and x.0 = char "\" and x.1 = char "%" =>
    sayString(SUBSTRING(x,1,NIL),out)
  x = '"%l" =>
    sayNewLine(out)
    for i in 1..$MARG repeat sayString('" ",out)
  x = '"%i" =>
    $MARG := $MARG + 3
  x = '"%u" =>
    $MARG := $MARG - 3
    if $MARG < 0 then $MARG := 0
  x = '"%U" =>
    $MARG := 0
  x = '"%" =>
    sayString('" ",out)
  x = '"%%" =>
    sayString('"%",out)
  x = '"%b" =>
    -- FIXME: this kludge is GCL-specific.  Find way to support
    -- highlighting on all supported Lisp.
    not IS_-CONSOLE out or %hasFeature KEYWORD::WIN32
      or stdStreamIsTerminal(1) = 0 => sayString('" ",out)
    not $highlightAllowed => sayString('" ",out)
    sayString($highlightFontOn,out)
  k := blankIndicator x => BLANKS(k,out)
  x = '"%d" =>
    not IS_-CONSOLE out or %hasFeature KEYWORD::WIN32
      or stdStreamIsTerminal(1) = 0 => sayString('" ",out)
    not $highlightAllowed => sayString('" ",out)
    sayString($highlightFontOff,out)
  STRINGP x => sayString(x,out)
  brightPrintHighlight(x,out)

brightPrint0AsTeX(x, out == $OutputStream) == 
  x = '"%l" =>
    sayString('"\\",out)
    for i in 1..$MARG repeat sayString('"\ ",out)
  x = '"%i" =>
    $MARG := $MARG + 3
  x = '"%u" =>
    $MARG := $MARG - 3
    if $MARG < 0 then $MARG := 0
  x = '"%U" =>
    $MARG := 0
  x = '"%" =>
    sayString('"\ ",out)
  x = '"%%" =>
    sayString('"%",out)
  x = '"%b" =>
    sayString('" {\tt ",out)
  k := blankIndicator x => for i in 1..k repeat sayString('"\ ",out)
  x = '"%d" =>
    sayString('"} ",out)
  x = '"_"$_"" => 
    sayString('"_"\verb!$!_"",out)
  x = '"$" => 
    sayString('"\verb!$!",out)
  STRINGP x => sayString(x,out)
  brightPrintHighlight(x,out)

blankIndicator x ==
  if IDENTP x then x := PNAME x
  null STRINGP x or MAXINDEX x < 1 => nil
  x.0 = '% and x.1 = 'x =>
    MAXINDEX x > 1 => PARSE_-INTEGER SUBSTRING(x,2,nil)
    1
  nil

brightPrint1(x, out == $OutputStream) ==
  if member(x,'(%l "%l")) then sayNewLine(out)
  else if STRINGP x then sayString(x,out)
       else brightPrintHighlight(x,out)
  NIL

brightPrintHighlight(x, out == $OutputStream) ==
  $texFormatting => brightPrintHighlightAsTeX(x,out)
  IDENTP x =>
    pn := PNAME x
    sayString(pn,out)
  -- following line helps find certain bugs that slip through
  -- also see sayBrightlyLength1
  VECP x => sayString('"UNPRINTABLE",out)
  ATOM x => sayString(object2String x,out)
  [key,:rst] := x
  if IDENTP key then key:=PNAME key
  key = '"%m" => mathprint(rst,out)
  member(key,'("%p" "%s")) => PRETTYPRIN0(rst,out)
  key = '"%ce" => brightPrintCenter(rst,out)
  key = '"%rj" => brightPrintRightJustify(rst,out)
  key = '"%t"  => $MARG := $MARG + tabber rst
  sayString('"(",out)
  brightPrint1(key,out)
  if EQ(key,'TAGGEDreturn) then
    rst:=[CAR rst,CADR rst,CADDR rst, '"environment (omitted)"]
  for y in rst repeat
    sayString('" ",out)
    brightPrint1(y,out)
  if rst and (la := LASTATOM rst) then
    sayString('" . ",out)
    brightPrint1(la,out)
  sayString('")",out)

brightPrintHighlightAsTeX(x, out == $OutputStream) ==
  IDENTP x =>
    pn := PNAME x
    sayString(pn,out)
  ATOM x => sayString(object2String x,out)
  VECP x => sayString('"UNPRINTABLE",out)
  [key,:rst] := x
  key = '"%m" => mathprint(rst,out)
  key = '"%s" => 
    sayString('"\verb__",out)
    PRETTYPRIN0(rst,out)
    sayString('"__",out)
  key = '"%ce" => brightPrintCenter(rst,out)
  key = '"%t"  => $MARG := $MARG + tabber rst
  -- unhandled junk (print verbatim(ish)
  sayString('"(",out)
  brightPrint1(key,out)
  if EQ(key,'TAGGEDreturn) then
    rst:=[CAR rst,CADR rst,CADDR rst, '"environment (omitted)"]
  for y in rst repeat
    sayString('" ",out)
    brightPrint1(y,out)
  if rst and (la := LASTATOM rst) then
    sayString('" . ",out)
    brightPrint1(la,out)
  sayString('")",out)

tabber num ==
    maxTab := 50
    num > maxTab => maxTab
    num

brightPrintCenter(x,out == $OutputStream) ==
  $texFormatting => brightPrintCenterAsTeX(x,out)
  -- centers rst within $LINELENGTH, checking for %l's
  ATOM x =>
    x := object2String x
    wid := STRINGLENGTH x
    if wid < $LINELENGTH then
      f := DIVIDE($LINELENGTH - wid,2)
      x := LIST(fillerSpaces(f.0,'" "),x)
    for y in x repeat brightPrint0(y,out)
    NIL
  y := NIL
  ok := true
  while x and ok repeat
    if member(CAR(x),'(%l "%l")) then ok := NIL
    else y := cons(CAR x, y)
    x := CDR x
  y := NREVERSE y
  wid := sayBrightlyLength y
  if wid < $LINELENGTH then
    f := DIVIDE($LINELENGTH - wid,2)
    y := CONS(fillerSpaces(f.0,'" "),y)
  for z in y repeat brightPrint0(z,out)
  if x then
    sayNewLine(out)
    brightPrintCenter(x,out)
  NIL

brightPrintCenterAsTeX(x, out == $OutputStream) ==
  ATOM x =>
    sayString('"\centerline{",out)
    sayString(x,out)
    sayString('"}",out)
  lst := x
  while lst repeat 
    words := nil
    while lst and not CAR(lst) = "%l" repeat
      words := [CAR lst,: words]
      lst := CDR lst
    if lst then lst := cdr lst
    sayString('"\centerline{",out)
    words := nreverse words
    for zz in words repeat
      brightPrint0(zz,out)
    sayString('"}",out)
  nil 

brightPrintRightJustify(x, out == $OutputStream) ==
  -- right justifies rst within $LINELENGTH, checking for %l's
  ATOM x =>
    x := object2String x
    wid := STRINGLENGTH x
    wid < $LINELENGTH =>
      x := LIST(fillerSpaces($LINELENGTH-wid,'" "),x)
      for y in x repeat brightPrint0(y,out)
      NIL
    brightPrint0(x,out)
    NIL
  y := NIL
  ok := true
  while x and ok repeat
    if member(CAR(x),'(%l "%l")) then ok := NIL
    else y := cons(CAR x, y)
    x := CDR x
  y := NREVERSE y
  wid := sayBrightlyLength y
  if wid < $LINELENGTH then
    y := CONS(fillerSpaces($LINELENGTH-wid,'" "),y)
  for z in y repeat brightPrint0(z,out)
  if x then
    sayNewLine(out)
    brightPrintRightJustify(x,out)
  NIL

--% Message Formatting Utilities

sayBrightlyLength l ==
  null l => 0
  atom l => sayBrightlyLength1 l
  sayBrightlyLength1 first l + sayBrightlyLength rest l

sayBrightlyLength1 x ==
  member(x,'("%b" "%d" %b %d)) =>
    NULL $highlightAllowed => 1
    1
  member(x,'("%l" %l)) => 0
  STRINGP x and STRINGLENGTH x > 2 and x.0 = '"%" and x.1 = '"x" =>
    INTERN x.3
  STRINGP x => STRINGLENGTH x
  IDENTP x => STRINGLENGTH PNAME x
  -- following line helps find certain bugs that slip through
  -- also see brightPrintHighlight
  VECP x => STRINGLENGTH '"UNPRINTABLE"
  ATOM x => STRINGLENGTH STRINGIMAGE x
  2 + sayBrightlyLength x

sayAsManyPerLineAsPossible l ==
  -- it is assumed that l is a list of strings
  l := [atom2String a for a in l]
  m := 1 + "MAX"/[SIZE(a) for a in l]
  -- w will be the field width in which we will display the elements
  m > $LINELENGTH =>
    for a in l repeat sayMSG a
    NIL
  w := MIN(m + 3,$LINELENGTH)
  -- p is the number of elements per line
  p := QUOTIENT($LINELENGTH,w)
  n := # l
  str := '""
  for i in 0..(n-1) repeat
    [c,:l] := l
    str := STRCONC(str,c,fillerSpaces(w - #c,'" "))
    REMAINDER(i+1,p) = 0 => (sayMSG str ; str := '"" )
  if str ^= '"" then sayMSG str
  NIL

say2PerLine l == say2PerLineWidth(l, QUOTIENT($LINELENGTH,2))

say2PerLineWidth(l,n) ==
  [short,long] := say2Split(l,nil,nil,n)
  say2PerLineThatFit short
  for x in long repeat sayLongOperation x
  sayBrightly '""

say2Split(l,short,long,width) ==
  l is [x,:l'] =>
    sayWidth x < width => say2Split(l',[x,:short],long,width)
    say2Split(l',short,[x,:long],width)
  [nreverse short,nreverse long]

sayLongOperation x ==
  sayWidth x > $LINELENGTH and (splitListOn(x,"if") is [front,back]) =>
    sayBrightly front
    BLANKS (6 + # PNAME front.1)
    sayBrightly back
  sayBrightly x

splitListOn(x,key) ==
  member(key,x) =>
    while first x ^= key repeat
      y:= [first x,:y]
      x:= rest x
    [nreverse y,x]
  nil

say2PerLineThatFit l ==
  while l repeat
    sayBrightlyNT first l
    sayBrightlyNT
      fillerSpaces((QUOTIENT($LINELENGTH,2)-sayDisplayWidth first l),'" ")
    (l:= rest l) =>
      sayBrightlyNT first l
      l:= rest l
      sayBrightly '""
    sayBrightly '""

sayDisplayStringWidth x ==
  null x => 0
  sayDisplayWidth x

sayDisplayWidth x ==
  PAIRP x =>
    +/[fn y for y in x] where fn y ==
      member(y,'(%b %d "%b" "%d")) or y=$quadSymbol => 1
      k := blankIndicator y => k
      sayDisplayWidth y
  x = "%%" or x = '"%%" => 1
  # atom2String x

sayWidth x ==
  atom x => # atom2String x
  +/[fn y for y in x] where fn y ==
    sayWidth y

pp2Cols(al) ==
  while al repeat
    [[abb,:name],:al]:= al
    ppPair(abb,name)
    if canFit2ndEntry(name,al) then
      [[abb,:name],:al]:= al
      TAB QUOTIENT($LINELENGTH,2)
      ppPair(abb,name)
    sayNewLine()
  nil

ppPair(abb,name) ==
    sayBrightlyNT [:bright abb,fillerSpaces(8-entryWidth abb," "),name]

canFit2ndEntry(name,al) ==
  wid := QUOTIENT($LINELENGTH,2) - 10
  null al => nil
  entryWidth name > wid => nil
  entryWidth CDAR al > wid => nil
  'T

entryWidth x == # atom2String x

center80 text == centerNoHighlight(text,$LINELENGTH,'" ")

centerAndHighlight(text,:argList) ==
  width := IFCAR argList or $LINELENGTH
  fillchar := IFCAR IFCDR argList or '" "
  wid := entryWidth text + 2
  wid >= width - 2 => sayBrightly ['%b,text,'%d]
  f := DIVIDE(width - wid - 2,2)
  fill1 := '""
  for i in 1..(f.0) repeat
    fill1 := STRCONC(fillchar,fill1)
  if f.1 = 0 then fill2 := fill1 else fill2 := STRCONC(fillchar,fill1)
  sayBrightly [fill1,'%b,text,'%d,fill2]
  nil

centerNoHighlight(text,:argList) == sayBrightly center(text,argList)

center(text,argList) ==
  width := IFCAR argList or $LINELENGTH
  fillchar := IFCAR IFCDR argList or '" "
  if (u:= splitSayBrightlyArgument text) then [text,:moreLines]:= u
  wid := sayBrightlyLength text
  wid >= width - 2 => sayBrightly text
  f := DIVIDE(width - wid - 2,2)
  fill1 := '""
  for i in 1..(f.0) repeat
    fill1 := STRCONC(fillchar,fill1)
  if f.1 = 0 then fill2 := fill1 else fill2 := STRCONC(fillchar,fill1)
  concat(fill1,text,fill2)

splitSayBrightly u ==
  width:= 0
  while u and (width:= width + sayWidth first u) < $LINELENGTH repeat
    segment:= [first u,:segment]
    u := rest u
  null u => NREVERSE segment
  segment => [:NREVERSE segment,"%l",:splitSayBrightly(u)]
  u

splitSayBrightlyArgument u ==
  atom u => nil
  while splitListSayBrightly u is [head,:u] repeat result:= [head,:result]
  result => [:NREVERSE result,u]
  [u]

splitListSayBrightly u ==
  for x in tails u repeat
    y := rest x
    null y => nil
    first y = '%l =>
      RPLACD(x,nil)
      ans:= [u,:rest y]
  ans


--=======================================================================
--                Utility Functions
--=======================================================================

$htSpecialChars == ['"_#", '"[", '"]", '"%", '"{", '"}", '"_\",
                    '"$", '"&", '"^", '"__", '"_~"]

$htCharAlist == '(
  ("$"  . "\%")
  ("[]" . "\[\]")
  ("{}" . "\{\}")
  ("\\" . "\\\\")
  ("\/" . "\\/" )
  ("/\" . "/\\" ) )

escapeSpecialChars s ==
  u := LASSOC(s,$htCharAlist) => u
  member(s, $htSpecialChars) => STRCONC('"_\", s)
  null $saturn => s
  ALPHA_-CHAR_-P (s.0) => s
  not (or/[dbSpecialDisplayOpChar? s.i for i in 0..MAXINDEX s]) => s
  buf := '""
  for i in 0..MAXINDEX s repeat buf :=
    dbSpecialDisplayOpChar?(s.i) => STRCONC(buf,'"\verb!",s.i,'"!")
    STRCONC(buf,s.i)
  buf

dbSpecialDisplayOpChar? c == (c = char '_~)
