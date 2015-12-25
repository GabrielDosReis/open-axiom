-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2014, Gabriel Dos Reis.
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


import incl
import i_-toplev
import unlisp
namespace BOOT

ncParseAndInterpretString s ==
  processInteractive(packageTran parseFromString s, nil)

ncParseFromString s ==
  zeroOneTran packageTran CATCH($SpadReaderTag, parseFromString s)

ncINTERPFILE(file, echo) ==
  $Echo: local := echo
  $ReadingFile: local := true
  SpadInterpretFile file

ncGetFunction(op, dom, sig) ==
  applyInPackage(function getNCfunction,_
                 [rePackageTran(op, '"boot"),_
                  rePackageTran(dom, '"boot"),_
                  rePackageTran(sig, '"boot")],_
                 '"BOOT")

applyInPackage(fun, args, package) ==
  savedPackage := _*PACKAGE_*
  SETQ(_*PACKAGE_*, FIND_-PACKAGE package)
  result := apply(fun, args)
  SETQ(_*PACKAGE_*, savedPackage)
  result  

ncSetCurrentLine l ==
  setCurrentLine l


--% INTERPRETER TOP LEVEL

spad() ==
  -- starts the interpreter but does not read in profiles, etc.
  $inLispVM : local := nil
  setOutputAlgebra "%initialize%"
  runspad()
  'EndOfSpad

runspad() ==
  mode:='restart
  while mode='restart repeat
    resetStackLimits()
    CATCH($quitTag, CATCH('coerceFailure,
                  mode:=CATCH($intTopLevel, ncTopLevel())))

ncTopLevel() ==
-- Top-level read-parse-eval-print loop for the interpreter.  Uses
-- the Bill Burge's parser.
  $InteractiveMode: local := true
  $NEWSPAD: local := true
  $e: local := $InteractiveFrame
  ncIntLoop()


ncIntLoop() ==
  $InputStream : local := forkStreamByName "*STANDARD-INPUT*"
  $OutputStream : local := forkStreamByName "*STANDARD-OUTPUT*"
  intloop()


intloop () ==
    mode := $intRestart
    while mode = $intRestart repeat
      resetStackLimits()
      mode := CATCH($intTopLevel, SpadInterpretStream(1, nil, true))

++ If the interpreter is spawn by the session manager, then
++ each successful connection also creates its own frame.  
++ In particular, the only time we get to do anything in the `initial'
++ frame is when we get the first connection.  In that situation, we would
++ be asked by the session manager to create a frame.  The client is
++ not aware of that discrete request made by the session manager.
++ It is utterly confusing to display a prompt, because all this
++ horse-threading happens behind the client's back.
printFirstPrompt?() ==
  $interpreterFrameName ~= "initial" or
    getOptionValue '"role" ~= '"server"

SpadInterpretStream(str, source, interactive?) ==
    pile?                    := not interactive?
    $libQuiet        : local := not interactive?
 
    $newcompErrorCount: local := 0 -- SMW Feb 2/90.
                                   -- Used in highComplete, ncHardError etc.
 
    $okToExecuteMachineCode: local := true -- set false on error
    $inclAssertions: local := ["AIX", "CommonLisp"] -- Jan 28/90
 
 
    $lastPos               : local := $nopos   ------------>!!!
    $ncMsgList             : local := nil
 
    $systemCommandFunction : local := function InterpExecuteSpadSystemCommand
    $promptMsg             : local := 'S2CTP023
 
    interactive? =>
      not $leanMode and printFirstPrompt?() and printPrompt()
      intloopReadConsole('"", str)
      []
    intloopInclude (source,0)
    []
 
    -----------------------------------------------------------------

SpadInterpretFile fn ==
  SpadInterpretStream(1, fn, nil)

intloopReadConsole(b, n)==
    a:= serverReadLine $InputStream
    not string? a => leaveScratchpad()
    #a=0 =>
             not $leanMode and printPrompt()
             intloopReadConsole('"", n)
    pfx := stripSpaces intloopPrefix?('")fi",a)
    pfx and ((pfx = '")fi") or (pfx = '")fin")) => []
    b = '"" and (d := intloopPrefix?('")", a)) =>
             setCurrentLine d
             c := ncloopCommand(d,n)
             not $leanMode and printPrompt()
             intloopReadConsole('"", c)
    a:=strconc(b,a)
    ncloopEscaped a => intloopReadConsole(subSequence(a, 0, #a - 1),n)
    c := intloopProcessString(a, n)
    not $leanMode and printPrompt()
    intloopReadConsole('"", c)
 
intloopPrefix?(prefix,whole) ==
     #prefix > #whole => false
     good := true
     leading := true
     spaces := 0
     i := 0
     len := #prefix
     wlen := #whole
     for j in 0.. while (good and i < len and j < wlen) repeat
       good := (prefix.i = whole.j) or (leading and (whole.j = char " "))
       if prefix.i = whole.j then i := i+1
       if (whole.j = char " ") and leading then 
         spaces := spaces + 1
       else leading := false
     spaces = wlen => nil
     if good then subString(whole,spaces) else good
 
 
intloopProcess(n,interactive,s)==
     StreamNull s => n
     [lines,ptree]:=first s
     pfAbSynOp?(ptree,"command")=>
            if interactive then setCurrentLine tokPart ptree
            apply($systemCommandFunction,[tokPart ptree])
            intloopProcess(n ,interactive ,rest s)
     intloopProcess(intloopSpadProcess(n,lines,ptree,interactive)
                 ,interactive ,rest s)
 
intloopEchoParse s==
         [dq,stream]:=first s
         [lines,rest]:=ncloopDQlines(dq,$lines)
         setCurrentLine(mkLineList(lines))
         if $Echo then ncloopPrintLines lines
         $lines:=rest
         [[[lines,npParse dqToList dq]],:rest s]
 
intloopInclude0(st, name, n) ==
    $lines:local:=incStream(st,name)
    intloopProcess(n,false,
      next(function intloopEchoParse,
        next(function insertpile,
          next(function lineoftoks,$lines))))

intloopInclude(name, n) ==
  try
    st := inputTextFile name
    intloopInclude0(st, name, n)
  finally (if st ~= nil then closeStream st)
 
intloopInclude1(name,n) ==
          a:=ncloopIncFileName name
          a => intloopInclude(a,n)
          n
 
intloopProcessString(s,n) ==
     setCurrentLine s
     intloopProcess(n,true,
         next(function ncloopParse,
           next(function lineoftoks,incString s)))
 
$pfMacros := []

intloopSpadProcess(stepNo,lines,ptree,interactive?)==
    $stepNo:local := stepNo
    $currentCarrier := cc := ['carrier]
    ncPutQ(cc, 'stepNumber, stepNo)
    ncPutQ(cc, 'messages, $ncMsgList)
    ncPutQ(cc, 'lines, lines)
    $ncMsgList := nil
    result := CatchAsCan(flung, CATCH("SpadCompileItem",
     CATCH($intCoerceFailure, CATCH($SpadReaderTag,
       interp(cc, ptree, interactive?))))) where
 
        interp(cc, ptree, interactive?) ==
            ncConversationPhase(function phParse,            [cc, ptree])
            ncConversationPhase(function phMacro,            [cc])
            ncConversationPhase(function phIntReportMsgs,[cc, interactive?])
            ncConversationPhase(function phInterpret,        [cc])
 
            #ncEltQ(cc, 'messages) ~= 0 => ncError()
 
    intSetNeedToSignalSessionManager()
    $prevCarrier := $currentCarrier
    result = 'ncEnd     => stepNo
    result = 'ncError   => stepNo
    result = 'ncEndItem => stepNo
    stepNo+1
 
phInterpret carrier ==
  -- Don't try evaluation if the data structure may have been corrupted.
  not ncEltQ(carrier, "ok?") => "KO"
  ptree := ncEltQ(carrier, 'ptree)
  val := intInterpretPform(ptree)
  ncPutQ(carrier, 'value, val)
 
 
--% phReportMsgs: carrier[lines,messages,..]-> carrier[lines,messages,..]
phIntReportMsgs(carrier, interactive?) ==
    lines := ncEltQ(carrier, 'lines)
    msgs  := ncEltQ(carrier, 'messages)
    nerr  := #msgs
    ncPutQ(carrier, 'ok?, nerr = 0)
    nerr = 0 => 'OK
    processMsgList(msgs, lines)
    intSayKeyedMsg ('S2CTP010,[nerr])
    'OK
 
mkLineList lines ==
  l := [rest line for line in lines | nonBlank rest line]
  #l = 1 => first l
  l

nonBlank str ==
  value := false
  for i in 0..maxIndex str repeat
    stringChar(str,i) ~= char " " =>
      value := true
      return value
  value
      
ncloopCommand (line,n) ==
         a:=ncloopPrefix?('")include",line)=>
                  ncloopInclude1( a,n)
         apply($systemCommandFunction,[line])
         n

ncloopEscaped x==
     esc :=false
     done:=false
     for i in maxIndex x .. 0 by -1 while not done repeat
         done:=
              stringChar(x,i) = char " " => false
              stringChar(x,i) = char "__" =>
                       esc:=true
                       true
              true
     esc

ncloopDQlines (dq,stream)==
        StreamNull stream
        a:= poGlobalLinePosn tokPosn second dq
        b:= poGlobalLinePosn CAAR stream
        streamChop (a-b+1,stream)
 
streamChop(n,s)==
    if StreamNull s
    then [nil,nil]
    else if n = 0
         then [nil,s]
         else
            [a,b]:= streamChop(n-1,rest s)
            line:=first s
            c:=ncloopPrefix?('")command",rest line)
            d:= [first line,:(if c then c else rest line)]
            [[d,:a],b]
 
ncloopPrintLines lines ==
        for line in lines repeat writeLine(rest line,$OutputStream)
        writeLine('" ",$OutputStream)
 
ncloopIncFileName string==
      fn := incFileName string
      not fn =>
          writeLine(strconc(string, '" not found"),$ErrorStream)
          []
      fn

ncloopParse s==
         [dq,stream]:=first s
         [lines,rest]:=ncloopDQlines(dq,stream)
         [[[lines,npParse dqToList dq]],:rest s]
 
ncloopInclude0(st, name, n) ==
     $lines:local := incStream(st, name)
     ncloopProcess(n,false,
         next(function ncloopEchoParse,
           next(function insertpile,
            next(function lineoftoks,$lines))))

ncloopInclude(name, n) ==
  try
    st := inputTextFile name
    ncloopInclude0(st, name, n)
  finally (if st ~= nil then closeStream st)
 
ncloopInclude1(name,n) ==
          a:=ncloopIncFileName name
          a => ncloopInclude(a,n)
          n

incString s== incRenumber incLude(0,[s],0,['"strings"] ,[Top])

ncError() ==
    THROW("SpadCompileItem",'ncError)

--% Compilation Carriers
--  This data structure is used to carry information between phases.

--% phParse: carrier[tokens,...] -> carrier[ptree, tokens,...]
--)line (defun pretty (x) (boottran::reallyprettyprint x))
--)line (defun packagetran (x) (boot::|packageTran|))
$ncmParse :=      nil

phParse(carrier,ptree) ==
    phBegin 'Parsing
    if $ncmParse then
           nothing
           intSayKeyedMsg ('S2CTP003,[%pform ptree])
    ncPutQ(carrier, 'ptree, ptree)
    'OK
 


--% phMacro: carrier[ptree,...] -> carrier[ptree, ptreePremacro,...]
$ncmMacro := false

phMacro carrier ==
    phBegin 'Macroing
    ptree  := ncEltQ(carrier, 'ptree)
    ncPutQ(carrier, 'ptreePremacro, ptree)
 
    ptree  := macroExpanded ptree
    if $ncmMacro then
        intSayKeyedMsg ('S2CTP007,[%pform ptree] )
 
    ncPutQ(carrier, 'ptree, ptree)
    'OK
 
--% phReportMsgs: carrier[lines,messages,..]-> carrier[lines,messages,..]
phReportMsgs(carrier, interactive?) ==
    lines := ncEltQ(carrier, 'lines)
    msgs  := ncEltQ(carrier, 'messages)
    nerr  := #msgs
    ncPutQ(carrier, 'ok?, nerr = 0)
    interactive? and nerr = 0 => 'OK
    processMsgList(msgs, lines)
    intSayKeyedMsg ('S2CTP010,[nerr])
    'OK
 
ncConversationPhase(fn, args) ==
    carrier := first args
 
    $ncMsgList: local := []
    $convPhase: local := 'NoPhase
 
    (try apply(fn, args); finally wrapup(carrier)) where
        wrapup(carrier) ==
            for m in $ncMsgList repeat
                ncPutQ(carrier, 'messages, [m, :ncEltQ(carrier, 'messages)])
 
ncloopPrefix?(prefix,whole) ==
     #prefix > #whole => false
     good:=true
     for i in 0..#prefix-1 for j in 0.. while good repeat
                good:= prefix.i = whole.j
     if good then subString(whole,#prefix) else good

$ncmPhase :=      nil
 
phBegin id ==
    $convPhase := id
    if $ncmPhase then intSayKeyedMsg('S2CTP021,[id])
 
PullAndExecuteSpadSystemCommand stream ==
    ExecuteSpadSystemCommand first stream
    rest stream

ExecuteSpadSystemCommand string ==
  apply($systemCommandFunction,[string])


clearMacroTable() ==
  $pfMacros := nil

getParserMacros() ==
  $pfMacros

displayParserMacro m ==
  m := objectAssoc(m, $pfMacros) => pfPrintSrcLines third m
  nil

