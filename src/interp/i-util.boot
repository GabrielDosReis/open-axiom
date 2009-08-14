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


import g_-util
namespace BOOT

module i_-util

--% 

$intTopLevel ==
  "top__level"

--% The function for making prompts
 
inputPrompt str ==
  -- replaces older INPUT-PROMPT
  atom (x := $SCREENSIZE()) => NIL
  p := CAR(x) - 2
  y := $OLDLINE
  SETQ($OLDLINE,NIL)
  y => _$SHOWLINE(STRCONC(str,EBCDIC 19,y),p)
  0 = SIZE str => NIL
  _$SHOWLINE(STRCONC(str,EBCDIC 19),p)
 
protectedPrompt(:p) ==
  [str,:br] := p
  0 = SIZE str => inputPrompt str
  msg := EBCDIC 29                       -- start of field
  msg :=
    if br then STRCONC(msg,EBCDIC 232)   -- bright write protect
    else       STRCONC(msg,EBCDIC  96)   -- write protect
  msg := STRCONC(msg,str,EBCDIC 29,EBCDIC 64)  -- unprotect again
  inputPrompt msg
 
MKPROMPT() ==
  $inputPromptType = 'none    => '""
  $inputPromptType = 'plain   => '"-> "
  $inputPromptType = 'step    =>
    STRCONC('"(",STRINGIMAGE $IOindex,'") -> ")
  $inputPromptType = 'frame   =>
    STRCONC(STRINGIMAGE $interpreterFrameName,
      '" (",STRINGIMAGE $IOindex,'") -> ")
  STRCONC(STRINGIMAGE $interpreterFrameName,
   '" [", SUBSTRING(CURRENTTIME(),8,NIL),'"] [",
    STRINGIMAGE $IOindex, '"] -> ")
 

printPrompt(flush? == false) ==
  PRINC(MKPROMPT(), $OutputStream)
  if flush? then
    FORCE_-OUTPUT $OutputStream

++ Return the name of a text editor, if possible.
textEditor() ==
  prog := getEnv '"EDITOR" => prog
  # $EditorProgram ~= 0 => $EditorProgram
  %hasFeature KEYWORD::WIN32 => '"notepad"
  throwKeyedMsg("S2IZ0091",nil)

--% Miscellaneous
 
$ZeroVecCache := nil
Zeros n ==
  #$ZeroVecCache = n => $ZeroVecCache
  $ZeroVecCache := MAKE_-VEC n
  for i in 0..n-1 repeat $ZeroVecCache.i:=0
  $ZeroVecCache
 
LZeros n ==
  n < 1 => nil
  l := [0]
  for i in 2..n repeat l := [0, :l]
  l
 
-- bpi2FunctionName x ==
--   s:= BPINAME x  => s
--   x
 
-- subrToName x == BPINAME x

$variableNumberAlist := nil

variableNumber(x) ==
  p := ASSQ(x, $variableNumberAlist)
  null p => 
    $variableNumberAlist := [[x,:0], :$variableNumberAlist]
    0
  RPLACD(p, 1+CDR p)
  CDR p

newType? t == nil


-- functions used at run-time which were formerly in the compiler files

Undef(:u) ==
  u':= LAST u
  [[domain,slot],op,sig]:= u'
  domain':=eval mkEvalable domain
  ^EQ(CAR ELT(domain',slot), function Undef) =>
-- OK - thefunction is now defined
    [:u'',.]:=u
    if $reportBottomUpFlag then
      sayMessage concat ['"   Retrospective determination of slot",'%b,
        slot,'%d,'"of",'%b,:prefix2String domain,'%d]
    APPLY(CAR ELT(domain',slot),[:u'',CDR ELT(domain',slot)])
  throwKeyedMsg("S2IF0008",[formatOpSignature(op,sig),domain])
 
makeInitialModemapFrame() == 
  COPY $InitialModemapFrame
 
isCapitalWord x ==
  (y := PNAME x) and and/[UPPER_-CASE_-P y.i for i in 0..MAXINDEX y]
 
mkPredList listOfEntries ==
  [['EQCAR,"#1",i] for arg in listOfEntries for i in 0..]


--%

++ Validate variable name `var', or abort analysis.
validateVariableNameOrElse var ==
  not IDENTP var => throwKeyedMsg("S2IS0016",[STRINGIMAGE var])
  var in '(% %%) => throwKeyedMsg("S2IS0050",[var])
  true
