-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


import c_-util
namespace BOOT

module i_-util

--% The function for making prompts
 
inputPrompt str ==
  -- replaces older INPUT-PROMPT
  (x := $SCREENSIZE()) isnt [.,:.] => nil
  p := first(x) - 2
  y := $OLDLINE
  SETQ($OLDLINE,nil)
  y => _$SHOWLINE(strconc(str,abstractChar 19,y),p)
  0 = # str => nil
  _$SHOWLINE(strconc(str,abstractChar 19),p)
 
protectedPrompt(:p) ==
  [str,:br] := p
  0 = # str => inputPrompt str
  msg := abstractChar 29                       -- start of field
  msg :=
    if br then strconc(msg,abstractChar 232)   -- bright write protect
    else       strconc(msg,abstractChar  96)   -- write protect
  msg := strconc(msg,str,abstractChar 29,abstractChar 64)  -- unprotect again
  inputPrompt msg
 
MKPROMPT() ==
  $inputPromptType = 'none    => '""
  $inputPromptType = 'plain   => '"-> "
  $inputPromptType = 'step    =>
    strconc('"(",toString $IOindex,'") -> ")
  $inputPromptType = 'frame   =>
    strconc(STRINGIMAGE $interpreterFrameName,
      '" (",toString $IOindex,'") -> ")
  strconc(STRINGIMAGE $interpreterFrameName,
   '" [", subString(CURRENTTIME(),8),'"] [",
    toString $IOindex, '"] -> ")
 

printPrompt(flush? == false) ==
  writeString(MKPROMPT(), $OutputStream)
  if flush? then
    flushOutput $OutputStream

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
  $ZeroVecCache := newVector n
  for i in 0..n-1 repeat
    $ZeroVecCache.i := 0
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
  p := objectAssoc(x, $variableNumberAlist)
  null p => 
    $variableNumberAlist := [[x,:0], :$variableNumberAlist]
    0
  p.rest := 1+rest p
  rest p

newType? t == nil


-- functions used at run-time which were formerly in the compiler files

Undef(:u) ==
  u':= last u
  [[domain,slot],op,sig]:= u'
  domain':=eval mkEvalable domain
  not sameObject?(first domain'.slot, function Undef) =>
-- OK - thefunction is now defined
    [:u'',.]:=u
    if $reportBottomUpFlag then
      sayMessage concat ['"   Retrospective determination of slot",'"%b",
        slot,'"%d",'"of",'"%b",:prefix2String domain,'"%d"]
    apply(first domain'.slot,[:u'',rest domain'.slot])
  throwKeyedMsg("S2IF0008",[formatOpSignature(op,sig),domain])
 
makeInitialModemapFrame() == 
  copyTree $InitialModemapFrame
 
isCapitalWord x ==
  (y := PNAME x) and and/[upperCase? y.i for i in 0..maxIndex y]
 
mkPredList listOfEntries ==
  [['%ieq,['%head,"#1"],i] for arg in listOfEntries for i in 0..]


--%

++ Validate variable name `var', or abort analysis.
validateVariableNameOrElse var ==
  not ident? var => throwKeyedMsg("S2IS0016",[STRINGIMAGE var])
  var in '(% %%) => throwKeyedMsg("S2IS0050",[var])
  true

--%

flattenCOND body ==
  -- transforms nested conditional clauses to flat ones, if possible
  body isnt ['%when,:.] => body
  ['%when,:extractCONDClauses body]
 
extractCONDClauses clauses ==
  -- extracts nested conditional clauses into a flat structure
  clauses is ['%when, [pred1,:act1],:restClauses] =>
    if act1 is [['PROGN,:acts]] then act1 := acts
    restClauses is [['%otherwise,restCond]] =>
      [[pred1,:act1],:extractCONDClauses restCond]
    [[pred1,:act1],:restClauses]
  [['%otherwise,clauses]]
 
++ Returns true if symbol `id' is either a local variable
++ or an iterator variable.
isLocallyBound id ==
  symbolMember?(id,$localVars) or symbolMember?(id,$iteratorVars)
