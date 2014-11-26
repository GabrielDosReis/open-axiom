-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
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


import sys_-macros
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
  not $SpadServer or not ioTerminal? stream =>
    line := readLine stream
    line ~= %nothing => line
    nil
  line :=
   while not $EndServerSession and line ~= %nothing repeat
     if $NeedToSignalSessionManager then
       sockSendInt($SessionManager, $EndOfOutput)
     $NeedToSignalSessionManager := false
     action := serverSwitch()
     action = $CallInterp =>
       l := readLine stream
       $NeedToSignalSessionManager := true
       leave l
     action = $CreateFrame =>
       frameName := gensym('"frame")
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
       form := unescapeStringsInForm readLispFromString buf
       protectedEVAL form
     action = $QuietSpadCommand =>
       $NeedToSignalSessionManager := true
       executeQuietCommand()
     action = $SpadCommand =>
       $NeedToSignalSessionManager := true
       stringBuf := sockGetString $MenuServer
       CATCH('coerceFailure,CATCH($intTopLevel, CATCH($SpadReaderTag,
	 parseAndInterpret stringBuf)))
       not $leanMode and printPrompt()
     action = $NonSmanSession =>
       $SpadServer := nil
     action = $KillLispSystem => 
       coreQuit()
     nil
  line ~= %nothing and line ~= nil => line
  ""

parseAndInterpret str ==
  $InteractiveMode : local := true
  $e: local := $InteractiveFrame
  ncParseAndInterpretString str

executeQuietCommand() ==
  $QuietCommand: local := true
  stringBuf := sockGetString $MenuServer
  CATCH('coerceFailure,CATCH($intTopLevel, CATCH($SpadReaderTag,
    parseAndInterpret stringBuf)))

parseAndEvalToHypertex str ==
  lines := parseAndEvalToStringForHypertex str
  len := # lines
  sockSendInt($MenuServer, len)
  for s in lines repeat
    sockSendString($MenuServer, s)

parseAndEvalToString str ==
  $collectOutput:local := true
  $outputLines: local := nil
  $IOindex: local := nil
  v := CATCH($SpadReaderTag, CATCH($intTopLevel, parseAndEvalStr str))
  v = 'restart => ['"error"]
  reverse! $outputLines

parseAndEvalToStringForHypertex str ==
  $collectOutput:local := true
  $outputLines: local := nil
  v := CATCH($SpadReaderTag, CATCH($intTopLevel, parseAndEvalStr str))
  v = 'restart => ['"error"]
  reverse! $outputLines

parseAndEvalToStringEqNum str ==
  $collectOutput:local := true
  $outputLines: local := nil
  v := CATCH($SpadReaderTag, CATCH($intTopLevel, parseAndEvalStr str))
  v = 'restart => ['"error"]
  reverse! $outputLines

parseAndInterpToString str ==
  v := applyWithOutputToString('parseAndEvalStr, [str])
  breakIntoLines rest v

parseAndEvalStr string ==
  $InteractiveMode: local := true
  $e: local := $InteractiveFrame
  parseAndEvalStr1 string

parseAndEvalStr1 string ==
  string.0 = char ")" =>
    doSystemCommand subSequence(string, 1)
  processInteractive(ncParseFromString string, nil)

protectedEVAL x ==
  error := true
  val := nil
  try
    val := eval x
    error := false
  finally (error => (resetStackLimits(); sendHTErrorSignal()))
  val
