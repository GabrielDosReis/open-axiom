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


import macros
namespace BOOT

-- Scratchpad-II server

-- Assoc list of interpreter frame names and unique integer identifiers

$frameAlist := nil
$frameNumber := 0
$currentFrameNum := 0
$EndServerSession := false
$NeedToSignalSessionManager := false
$sockBufferLength := 9217

serverReadLine(stream) ==
-- used in place of READ-LINE in a scratchpad server system.
  FORCE_-OUTPUT()
  not $SpadServer or not IS_-CONSOLE stream =>
    read_-line(stream)
  IN_-STREAM: fluid := stream
  _*EOF_*: fluid := NIL
  line :=
   while not $EndServerSession and not _*EOF_* repeat
     if $NeedToSignalSessionManager then
       sockSendInt($SessionManager, $EndOfOutput)
     $NeedToSignalSessionManager := false
     action := serverSwitch()
     action = $CallInterp =>
       l := read_-line(stream)
       $NeedToSignalSessionManager := true
       return l
     action = $CreateFrame =>
       frameName := GENSYM('"frame")
       addNewInterpreterFrame(frameName)
       $frameAlist := [[$frameNumber,:frameName], :$frameAlist]
       $currentFrameNum := $frameNumber
       sockSendInt($SessionManager, $frameNumber)
       $frameNumber := $frameNumber + 1
       sockSendString($SessionManager, MKPROMPT())
     action = $SwitchFrames =>
       $currentFrameNum := sockGetInt($SessionManager)
       currentFrame := LASSOC($currentFrameNum, $frameAlist)
       changeToNamedInterpreterFrame currentFrame
     action = $EndSession =>
       $EndServerSession := true
     action = $LispCommand =>
       $NeedToSignalSessionManager := true
       buf := sockGetString $MenuServer
       form := unescapeStringsInForm READ_-FROM_-STRING buf
       protectedEVAL form
     action = $QuietSpadCommand =>
       $NeedToSignalSessionManager := true
       executeQuietCommand()
     action = $SpadCommand =>
       $NeedToSignalSessionManager := true
       stringBuf := sockGetString $MenuServer
       CATCH('coerceFailure,CATCH('top__level, CATCH('SPAD__READER,
	 parseAndInterpret stringBuf)))
       PRINC MKPROMPT()
       FINISH_-OUTPUT()
     action = $NonSmanSession =>
       $SpadServer := nil
     action = $KillLispSystem => 
       coreQuit()
     nil
  line => line
  ""

parseAndInterpret str ==
  $InteractiveMode :fluid := true
  $SPAD: fluid := true
  $e:fluid := $InteractiveFrame
  ncParseAndInterpretString str

oldParseAndInterpret str ==
  tree := string2SpadTree str
  tree => processInteractive(parseTransform postTransform tree, NIL)
  NIL

executeQuietCommand() ==
  $QuietCommand: fluid := true
  stringBuf := sockGetString $MenuServer
  CATCH('coerceFailure,CATCH('top__level, CATCH('SPAD__READER,
    parseAndInterpret stringBuf)))

-- Includued for compatability with old-parser systems
serverLoop() ==
  IN_-STREAM: fluid := $InputStream
  _*EOF_*: fluid := NIL
  while not $EndServerSession and not _*EOF_* repeat
    if $Prompt then (PRINC MKPROMPT(); FINISH_-OUTPUT())
    $Prompt := NIL
    action := serverSwitch()
    action = $CallInterp =>
      CATCH('coerceFailure,CATCH('top__level, CATCH('SPAD__READER,
        parseAndInterpret read_-line($InputStream) )))
      PRINC MKPROMPT()
      FINISH_-OUTPUT()
      sockSendInt($SessionManager, $EndOfOutput)
    action = $CreateFrame =>
      frameName := GENSYM('"frame")
      addNewInterpreterFrame(frameName)
      $frameAlist := [[$frameNumber,:frameName], :$frameAlist]
      $currentFrameNum := $frameNumber
      sockSendInt($SessionManager, $frameNumber)
      $frameNumber := $frameNumber + 1
      sockSendString($SessionManager, MKPROMPT())
    action = $SwitchFrames =>
      $currentFrameNum := sockGetInt($SessionManager)
      currentFrame := LASSOC($currentFrameNum, $frameAlist)
      changeToNamedInterpreterFrame currentFrame
    action = $EndSession =>
      $EndServerSession := true
    action = $LispCommand =>
      stringBuf := sockGetString $MenuServer
      form := unescapeStringsInForm READ_-FROM_-STRING stringBuf
      EVAL form
    action = $QuietSpadCommand =>
      executeQuietCommand()
    action = $SpadCommand =>
      stringBuf := sockGetString $MenuServer
      CATCH('coerceFailure,CATCH('top__level, CATCH('SPAD__READER,
        parseAndInterpret stringBuf)))
      PRINC MKPROMPT()
      FINISH_-OUTPUT()
      sockSendInt($SessionManager, $EndOfOutput)
    NIL
  if _*EOF_* then $Prompt := true
  NIL

parseAndEvalToHypertex str ==
  lines := parseAndEvalToStringForHypertex str
  len := LENGTH lines
  sockSendInt($MenuServer, len)
  for s in lines repeat
    sockSendString($MenuServer, s)

parseAndEvalToString str ==
  $collectOutput:local := true
  $outputLines: local := nil
  $IOindex: local := nil
  v := CATCH('SPAD__READER, CATCH('top__level, parseAndEvalStr str))
  v = 'restart => ['"error"]
  NREVERSE $outputLines

parseAndEvalToStringForHypertex str ==
  $collectOutput:local := true
  $outputLines: local := nil
  v := CATCH('SPAD__READER, CATCH('top__level, parseAndEvalStr str))
  v = 'restart => ['"error"]
  NREVERSE $outputLines

parseAndEvalToStringEqNum str ==
  $collectOutput:local := true
  $outputLines: local := nil
  v := CATCH('SPAD__READER, CATCH('top__level, parseAndEvalStr str))
  v = 'restart => ['"error"]
  NREVERSE $outputLines

parseAndInterpToString str ==
  v := applyWithOutputToString('parseAndEvalStr, [str])
  breakIntoLines CDR v

parseAndEvalStr string ==
  $InteractiveMode :fluid := true
  $SPAD: fluid := true
  $e:fluid := $InteractiveFrame
  parseAndEvalStr1 string

parseAndEvalStr1 string ==
  string.0 = char '")" =>
    doSystemCommand SUBSEQ(string, 1)
  processInteractive(ncParseFromString string, NIL)

protectedEVAL x ==
  error := true
  val := NIL
  UNWIND_-PROTECT((val := EVAL x; error := NIL),
                   error => (resetStackLimits(); sendHTErrorSignal()))
  val
