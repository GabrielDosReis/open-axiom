-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2011, Gabriel Dos Reis.
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


import i_-analy
namespace BOOT

--% Top Level Interpreter Code

$intCoerceFailure ==
  "coerceFailure"

$intRestart ==
  "restart"

-- When $QuiteCommand is true Spad will not produce any output from
--  a top level command
$QuietCommand := nil
-- When $ProcessInteractiveValue is true, we don't want the value printed
-- or recorded.
$ProcessInteractiveValue := nil
$HTCompanionWindowID := nil

++ initialize the garbage collection timer
statisticsInitialization() ==
)if %hasFeature KEYWORD::GCL
  SYSTEM::GBC_-TIME 0
)else
  nil
)endif

--% Starting the interpreter from LISP

spadpo() ==
  -- starts the interpreter but only displays parsed input
  $PrintOnly: local:= true
  spad()

start(:l) ==
  -- The function  start  begins the interpreter process, reading in
  -- the profile and printing start-up messages.
  $inLispVM : local := nil
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"interpreter"])
  initializeTimedNames($interpreterTimedNames,$interpreterTimedClasses)
  statisticsInitialization()
  $InteractiveFrame := makeInitialModemapFrame()
  initializeSystemCommands()
  initializeInterpreterFrameRing()
  SETQ($ErrorStream,
    DEFIOSTREAM('((DEVICE . CONSOLE)(MODE . OUTPUT)),80,0))
  setOutputAlgebra "%initialize%"
  loadExposureGroupData()
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"database"])
  mkLowerCaseConTable()
  if not $ruleSetsInitialized then initializeRuleSets()
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"constructors"])
  makeConstructorsAutoLoad()
  GCMSG(nil)
  SETQ($IOindex,1)
  if $displayStartMsgs then sayKeyedMsg("S2IZ0053",['"history"])
  initHist()
  if functionp 'addtopath then addtopath strconc(systemRootDirectory(),'"bin")
  if null(l) then
    if $displayStartMsgs then
      sayKeyedMsg("S2IZ0053",[namestring ['_.axiom,'input]])
    readSpadProfileIfThere()
  if $displayStartMsgs then spadStartUpMsgs()
  if $OLDLINE then
    SAY fillerSpaces($LINELENGTH,char "=")
    sayKeyedMsg("S2IZ0050",[namestring ['axiom,'input]])
    if $OLDLINE ~= 'END__UNIT
      then
        centerAndHighlight($OLDLINE,$LINELENGTH,'" ")
        sayKeyedMsg("S2IZ0051",nil)
      else sayKeyedMsg("S2IZ0052",nil)
    SAY fillerSpaces($LINELENGTH,char "=")
    finishLine $OutputStream
    $OLDLINE := nil
  $superHash := hashTable 'EQUAL
  if null l then runspad()
  'EndOfSpad

readSpadProfileIfThere() ==
  -- reads SPADPROF INPUT if it exists
  file := ['_.axiom,'input]
  MAKE_-INPUT_-FILENAME file =>
    $editFile := file
    _/RQ ()
  nil

--% Parser Output --> Interpreter

processInteractive(form, posnForm) ==
  --  Top-level dispatcher for the interpreter.  It sets local variables
  --  and then calls processInteractive1 to do most of the work.
  --  This function receives the output from the parser.

  initializeTimedNames($interpreterTimedNames,$interpreterTimedClasses)

  $op: local:= (form is [op,:.] => op; form) --name of operator
  $Coerce: local := nil
  $compErrorMessageStack: local := nil
  $freeVars : local := nil
  $mapList:local := nil            --list of maps being type analyzed
  $compilingMap:local:= nil        --true when compiling a map
  $compilingLoop:local:= nil       --true when compiling a loop body
  $interpOnly: local := nil        --true when in interpret only mode
  $whereCacheList: local := nil    --maps compiled because of where
  $StreamFrame: local := nil       --used in printing streams
  $declaredMode: local := nil      --Weak type propagation for symbols
  $localVars:local := nil          --list of local variables in function
  $analyzingMapList:local := nil   --names of maps currently being
                                   --analyzed
  $lastLineInSEQ: local := true    --see evalIF and friends
  $instantCoerceCount: local := 0
  $instantCanCoerceCount: local := 0
  $instantMmCondCount: local := 0
  $defaultFortVar:= 'X             --default FORTRAN variable name
  $fortVar : local :=              --variable name for FORTRAN output
     $defaultFortVar
  $minivector: local := nil
  $domPvar: local := nil
  $inRetract: local := nil
  object := processInteractive1(form, posnForm)
  if not($ProcessInteractiveValue) then
    if $reportInstantiations then
      reportInstantiations()
      CLRHASH $instantRecord
    writeHistModesAndValues()
    updateHist()
  object

processInteractive1(form, posnForm) ==
  -- calls the analysis and output printing routines
  $e : local := $InteractiveFrame
  recordFrame 'system

  startTimingProcess 'analysis
  object   := interpretTopLevel(form, posnForm)
  stopTimingProcess 'analysis

  startTimingProcess 'print
  if not($ProcessInteractiveValue) then
    recordAndPrint(objValUnwrap object,objMode object)
  recordFrame 'normal
  stopTimingProcess 'print

--spadtestValueHook(objValUnwrap object, objMode object)

  object

--% Result Output Printing

recordAndPrint(x,md) ==
  --  Prints out the value x which is of type m, and records the changes
  --  in environment $e into $InteractiveFrame
  --  $printAnyIfTrue  is documented in setvart.boot. controlled with )se me any
  if md = $Any and $printAnyIfTrue  then
    md' := first  x
    x' := rest x
  else
    x' := x
    md' := md
  $outputMode: local := md   --used by DEMO BOOT
  mode:= (md=$EmptyMode => quadSch(); md)
  if (md ~= $Void) or $printVoidIfTrue then
    if not $QuietCommand then
      output(x',md')
  putHist('%,'value,objNewWrap(x,md),$e)
  if $printTimeIfTrue or $printTypeIfTrue then printTypeAndTime(x',md')
  if $printStorageIfTrue then printStorage()
  if $printStatisticsSummaryIfTrue then printStatisticsSummary()
  if integer? $HTCompanionWindowID then mkCompanionPage md
  $mkTestFlag => recordAndPrintTest md
  $runTestFlag =>
    $mkTestOutputType := md
    'done
  'done

printTypeAndTime(x,m) ==  --m is the mode/type of the result
  printTypeAndTimeNormal(x, m)

printTypeAndTimeNormal(x,m) ==
  -- called only if either type or time is to be displayed
  if m is ['Union, :argl] then
    x' := retract(objNewWrap(x,m))
    m' := objMode x'
    m := ['Union, :[arg for arg in argl | sameUnionBranch(arg, m')], "..."]
  if $printTimeIfTrue then
    timeString := makeLongTimeString($interpreterTimedNames,
      $interpreterTimedClasses)
  $printTimeIfTrue and $printTypeIfTrue =>
    $collectOutput =>
      $outputLines := [msgText("S2GL0012", [m]), :$outputLines]
    sayKeyedMsg("S2GL0014",[m,timeString])
  $printTimeIfTrue =>
    $collectOutput => nil
    sayKeyedMsg("S2GL0013",[timeString])
  $printTypeIfTrue =>
    $collectOutput =>
      $outputLines := [justifyMyType msgText("S2GL0012", [m]), :$outputLines]
    sayKeyedMsg("S2GL0012",[m])

sameUnionBranch(uArg, m) ==
  uArg is [":", ., t] => t = m
  uArg = m

msgText(key, args) ==
  msg := segmentKeyedMsg getKeyedMsg key
  msg := substituteSegmentedMsg(msg,args)
  msg := flowSegmentedMsg(msg,$LINELENGTH,$MARGIN)
  apply(function strconc, [STRINGIMAGE x for x in CDAR msg])

justifyMyType(t) ==
  len := #t
  len > $LINELENGTH => t
  strconc(fillerSpaces($LINELENGTH-len), t)

typeTimePrin x ==
  $highlightDelta: local:= 0
  maprinSpecial(x,0,79)

printStorage() ==
  $collectOutput => nil
  storeString :=
    makeLongSpaceString($interpreterTimedNames, $interpreterTimedClasses)
  sayKeyedMsg("S2GL0016",[storeString])

printStatisticsSummary() ==
  $collectOutput => nil
  summary := statisticsSummary()
  sayKeyedMsg("S2GL0017",[summary])

--%  Interpreter Middle-Level Driver + Utilities

interpretTopLevel(x, posnForm) ==
  --  Top level entry point from processInteractive1.  Sets up catch
  --  for a thrown result
  savedTimerStack := copyTree $timedNameStack
  c := CATCH('interpreter,interpret(x, posnForm))
  while savedTimerStack ~= $timedNameStack repeat
    stopTimingProcess peekTimedName()
  c = 'tryAgain => interpretTopLevel(x, posnForm)
  c

interpret(x, :restargs) ==
  posnForm := if cons? restargs then first restargs else restargs
  --type analyzes and evaluates expression x, returns object
  $env:local := [[nil]]
  $eval:local := true           --generate code-- don't just type analyze
  $genValue:local := true       --evaluate all generated code
  interpret1(x,nil,posnForm)

interpret1(x,rootMode,posnForm) ==
  -- dispatcher for the type analysis routines.  type analyzes and
  -- evaluates the expression x in the rootMode (if non-nil)
  -- which may be $EmptyMode.  returns an object if evaluating, and a
  -- modeset otherwise

  -- create the attributed tree

  node := mkAtreeWithSrcPos(x, posnForm)
  if rootMode then putTarget(node,rootMode)

  -- do type analysis and evaluation of expression.  The real guts

  modeSet:= bottomUp node
  not $eval => modeSet
  newRootMode := (null rootMode => first modeSet ; rootMode)
  argVal := getArgValue(node, newRootMode)
  argVal and not $genValue => objNew(argVal, newRootMode)
  argVal and (val:=getValue node) => interpret2(val,newRootMode,posnForm)
  keyedSystemError("S2IS0053",[x])

interpret2(object,m1,posnForm) ==
  -- this is the late interpretCoerce. I removed the call to
  -- coerceInteractive, so it only does the JENKS cases    ALBI
  m1=$ThrowAwayMode => object
  x := objVal object
  m := objMode object
  m=$EmptyMode =>
    x is [op,:.]  and op in '(%Map STREAM) => objNew(x,m1)
    m1 = $EmptyMode => objNew(x,m)
    systemErrorHere ["interpret2",x]
  m1 =>
    if (ans := coerceInteractive(object,m1)) then ans
    else throwKeyedMsgCannotCoerceWithValue(x,m,m1)
  object

--%
intSayKeyedMsg(key, args) ==
  sayKeyedMsg(packageTran key, packageTran args)

intProcessSynonyms str ==
  LINE: local := str
  processSynonyms
  LINE

intInterpretPform pf ==
  processInteractive(zeroOneTran packageTran pf2Sex pf, pf)

intNewFloat() ==
  ["Float"]

intSetNeedToSignalSessionManager() ==
  $NeedToSignalSessionManager := true

setCurrentLine s ==
  $currentLine := 
     null $currentLine => s
     string? $currentLine =>
       [$currentLine, :(string? s => [s]; s)]
     lastNode($currentLine).rest := (string? s => [s]; s)
     $currentLine


intnplisp s ==
  $currentLine := s
  nplisp $currentLine

intSetQuiet() ==
 $QuietCommand := true

intUnsetQuiet() ==
  $QuietCommand := false

