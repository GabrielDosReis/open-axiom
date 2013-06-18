-- Copyright (C) 2011-2013, Gabriel Dos Reis.
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
--     - Neither the name of OpenAxiom. nor the names of its contributors
--       may be used to endorse or promote products derived from this
--       software without specific prior written permission.
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
--

--%
--% Author: Gabriel Dos Reis
--%

import sys_-utility
import sys_-macros
import io

namespace BOOT

module lexing

++ List of lines returned from preparse
$lineStack := nil

nextLine rd ==
  $lineStack = nil => nil
  [[n,:l],:$lineStack] := $lineStack
  l := strconc(l,'" ")
  lineNewLine!(l,readerSourceLine rd,n)
  SETQ(LINE,l)
  $currentLine := l

nextLinesClear!() ==
  $lineStack := nil

++ Advances `rd', invoking nextLine if necessary
advanceChar! rd ==
  repeat
    not lineAtEnd? readerSourceLine rd =>
      return lineAdvanceChar! readerSourceLine rd
    nextLine rd => return currentChar rd
    return nil

--%

++ Returns the current character of the line, initially blank for
++ an unread line
currentChar rd ==
  linePastEnd? readerSourceLine rd => charByName "Return"
  lineCurrentChar readerSourceLine rd

nextChar rd ==
  lineAtEnd? readerSourceLine rd => charByName '"Return"
  lineNextChar readerSourceLine rd
  

--%
--% Token abstract datatype.
--%
structure %Token ==
  Record(sym: %Symbol, typ: %Thing, nb?: %Boolean) with
    tokenSymbol == (.sym)
    tokenType == (.typ)       -- typ in '(NUMBER IDENTIFIER SPECIAL_-CHAR)
    tokenNonblank? == (.nb?)  -- true if token is not preceded by a blank.

makeToken(sym == nil, typ == nil, blnk? == true) ==
  mk%Token(sym,typ,blnk?)

macro copyToken t ==
  copy%Token t

++ Last seen token
$priorToken := makeToken()

++ Is there no blank in front of current token?
$nonblank := true

++ First token in input stream
$currentToken := makeToken()

++ Next token in input stream
$nextToken := makeToken()

++ Number of token in the buffer (0, 1, 2)
$validTokens := 0

++ Subroutine of getSpadIntegerToken.
++ Read a the characters of a decimal integer and returns its value.
getDecimalNumberToken(rd,buf) ==
  repeat
    SUFFIX(currentChar rd,buf)
    not DIGITP nextChar rd => leave nil
    advanceChar! rd
  readIntegerIfCan buf

++ Subroutine of getSpadIntegerToken.
++ We just read the radix of an integer number; parse the
++ digits forming that integer token.
getIntegerInRadix(rd,buf,r) ==
  r < 2 => SPAD__SYNTAX__ERROR rd
  mark := #buf + 1
  repeat
    SUFFIX(currentChar rd,buf)
    d := rdigit? nextChar rd
    d = nil => leave nil
    d >= r => SPAD__SYNTAX__ERROR rd
    advanceChar! rd
  PARSE_-INTEGER(buf,start <- mark,radix <- r)

radixSuffix? c ==
  c = char "r" or c = char "R"

++ Parse an integer token, written either implicitly in decimal form,
++ or explicitly specified radix.  The number may be written in plain
++ format, where the radix is implicitly taken to be 10.  Or the spelling
++ can explicitly specify a radix.  That radix can be anything
++ in the range 2..36
getSpadIntegerToken rd ==
  buf := MAKE_-ADJUSTABLE_-STRING 0
  val := getDecimalNumberToken(rd,buf)
  advanceChar! rd
  if radixSuffix? currentChar rd then
    val := getIntegerInRadix(rd,buf,val)
    advanceChar! rd
  makeToken(val,'NUMBER,#buf)

getNumberToken rd ==
  buf := nil
  repeat
    buf := [currentChar rd,:buf]
    digit? nextChar rd => advanceChar! rd
    leave nil
  advanceChar! rd
  sz := #buf  -- keep track of digit count
  makeToken(readIntegerIfCan listToString reverse! buf,'NUMBER,sz)

getArgumentDesignator rd ==
  advanceChar! rd
  tok := getNumberToken rd
  makeToken(makeSymbol strconc('"#",formatToString('"~D",tokenSymbol tok)),
     'ARGUMENT_-DESIGNATOR,$nonblank)

getToken rd ==
  not skipBlankChars rd => nil
  tt := tokenLookaheadType(rd,currentChar rd)
  tt is 'EOF => makeToken(nil,'_*EOF,$nonblank)
  tt is 'ESCAPE =>
    advanceChar! rd
    getIdentifier(rd,true)
  tt is 'ARGUMENT_-DESIGNATOR => getArgumentDesignator rd
  tt is 'ID => getIdentifier(rd,false)
  tt is 'NUM => getSpadIntegerToken rd
  tt is 'STRING => getSpadString rd
  tt is 'SPECIAL_-CHAR => getSpecial rd
  getGliph(rd,tt)

tryGetToken rd ==
  tok := getToken rd =>
   $validTokens := $validTokens + 1
   tok
  nil

++ Returns the current token or gets a new one if necessary
currentToken rd ==
  $validTokens > 0 => $currentToken
  $currentToken := tryGetToken rd

++ Returns the token after the current token, or nil if there is none after
nextToken rd ==
  currentToken rd
  $validTokens > 1 => $nextToken
  $nextToken := tryGetToken rd

matchToken(tok,typ,sym == nil) ==
  tok ~= nil and symbolEq?(tokenType tok,typ) and
    (sym = nil or sym = tokenSymbol tok) => tok
  nil

++ Return the current token if it has type `typ', and possibly the
++ same spelling as `sym'.
matchCurrentToken(rd,typ,sym == nil) ==
  matchToken(currentToken rd,typ,sym)

++ Return the next token if it has type `typ;, and possibly the same
++ spelling as `sym'.
matchNextToken(rd,typ,sym == nil) ==
  matchToken(nextToken rd,typ,sym)

++ Makes the next token be the current token.
advanceToken rd ==
  $validTokens = 0 => $currentToken := tryGetToken rd
  $validTokens = 1 =>
    $validTokens := $validTokens - 1
    $priorToken := copyToken $currentToken
    $currentToken := tryGetToken rd
  $validTokens = 2 =>
    $priorToken := copyToken $currentToken
    $currentToken := copyToken $nextToken
    $validTokens := $validTokens - 1
  nil

makeSymbolOf tok ==
  tok = nil => nil
  tokenSymbol tok = nil => nil
  char? tokenSymbol tok => makeSymbol charString tokenSymbol tok
  tokenSymbol tok

currentSymbol rd ==
  makeSymbolOf currentToken rd

tokenStackClear!() ==
  $validTokens := 0
  $currentToken := makeToken(nil,nil,nil)
  $nextToken := makeToken(nil,nil,nil)
  $priorToken := makeToken(nil,nil,nil)

++ Predicts the kind of token to follow, based on the given initial character
tokenLookaheadType(rd,c) ==
  c = nil => 'EOF
  c = char "__" => 'ESCAPE
  c = char "#" and digit? nextChar rd => 'ARGUMENT_-DESIGNATOR
  digit? c => 'NUM
  c = char "%" or c = char "?" or c = char "?" or alphabetic? c => 'ID
  c = char "_"" => 'STRING
  c = char " " or c = charByName "Tab" or c = charByName "Return" => 'WHITE
  p := property(makeSymbol charString c,'GLIPH) => p
  'SPECIAL_-CHAR

skipBlankChars rd ==
  $nonblank := true
  repeat
    c := currentChar rd
    c = nil => return false
    tokenLookaheadType(rd,c) = 'WHITE =>
      $nonblank := false
      advanceChar! rd = nil => return false
    return true

getSpadString rd ==
  buf := nil
  currentChar rd ~= char "_"" => nil
  advanceChar! rd
  repeat
    currentChar rd = char "_"" => leave nil
    buf := [(currentChar rd = char "__" => advanceChar! rd; currentChar rd),:buf]
    advanceChar! rd = nil =>
      sayBrightly '"close quote inserted"
      leave nil
  advanceChar! rd
  makeToken(listToString reverse! buf,'SPADSTRING)

++ Take a special character off the input stream.  We let the type name
++ of each special character be the atom whose print name is the
++ character itself
getSpecial rd ==
  c := currentChar rd
  advanceChar! rd
  makeToken(c,'SPECIAL_-CHAR)

getGliph(rd,gliphs) ==
  buf := [currentChar rd]
  advanceChar! rd
  repeat
    gliphs := symbolAssoc(makeSymbol charString currentChar rd,gliphs) =>
      buf := [currentChar rd,:buf]
      gliphs := rest gliphs
      advanceChar! rd
    s := makeSymbol listToString reverse! buf
    return makeToken(s,'GLIPH,$nonblank)

Keywords == [
 "or", "and", "isnt", "is", "where", "forall", "exist", "try", "assume",
  "has", "with", "add", "case", "in", "by", "pretend", "mod", "finally",
   "exquo", "div", "quo", "else", "rem", "then", "suchthat", "catch", "throw",
    "if", "iterate", "break", "from", "exit", "leave", "return", "do",
     "not", "repeat", "until", "while", "for", "import", "inline" ]

getIdentifier(rd,esc?) ==
  buf := [currentChar rd]
  advanceChar! rd
  repeat 
    c := currentChar rd
    c = char "__" =>
      advanceChar! rd = nil => leave nil
      buf := [currentChar rd,:buf]
      esc? := true
      advanceChar! rd = nil => leave nil
    alphabetic? c or digit? c
      or scalarMember?(c,[char "%",char "'",char "?",char "!"]) =>
        buf := [c,:buf]
        advanceChar! rd = nil => leave nil
    leave nil
  s := makeSymbol listToString reverse! buf
  tt :=
    not esc? and symbolMember?(s,Keywords) => 'KEYWORD
    'IDENTIFIER
  makeToken(s,tt,$nonblank)

escapeKeywords: (%String,%Symbol) -> %String
escapeKeywords(nm,id) ==
  symbolMember?(id,Keywords) => strconc('"__",nm)
  nm

underscore: %String -> %String
underscore s ==
  n := #s - 1
  and/[alphabetic? stringChar(s,i) for i in 0..n] => s
  buf := nil
  for i in 0..n repeat
    c := stringChar(s,i)
    if not alphabetic? c then
      buf := [char "__",:buf]
    buf := [c,:buf]
  listToString reverse! buf

quoteIfString: %Thing -> %Maybe %String
quoteIfString tok ==
  tok = nil => nil
  tt := tokenType tok
  tt is 'SPADSTRING => strconc('"_"",underscore tokenSymbol tok,'"_"")
  tt is 'NUMBER => formatToString('"~v,'0D",tokenNonblank? tok,tokenSymbol tok)
  tt is 'SPECIAL_-CHAR => charString tokenSymbol tok
  tt is 'IDENTIFIER =>
    escapeKeywords(symbolName tokenSymbol tok,tokenSymbol tok)
  symbolName tokenSymbol tok

ungetTokens rd ==
  $validTokens = 0 => true
  $validTokens = 1 =>
    cursym := quoteIfString $currentToken
    curline := lineCurrentSegment readerSourceLine rd
    revisedline := strconc(cursym,curline,'" ")
    lineNewLine!(revisedline,readerSourceLine rd,lineNumber readerSourceLine rd)
    $nonblank := tokenNonblank? $currentToken
    $validTokens := 0
  $validTokens = 2 =>
    cursym := quoteIfString $currentToken
    nextsym := quoteIfString $nextToken
    curline := lineCurrentSegment readerSourceLine rd
    revisedline := strconc((tokenNonblank? $currentToken => '""; '" "),
      cursym,(tokenNonblank? $nextToken => '""; '" "),nextsym,curline,'" ")
    $nonblank := tokenNonblank? $currentToken
    lineNewLine!(revisedline,readerSourceLine rd,lineNumber readerSourceLine rd)
    $validTokens := 0
  coreError '"How many tokens do you think you have?"


++ Returns length of X if X matches initial segment of `rd'.
++ Otherwise, return nil.
matchString(rd,x) ==
  ungetTokens rd
  skipBlankChars rd
  not linePastEnd? readerSourceLine rd and currentChar rd ~= nil =>
    nx := #x
    buf := lineBuffer readerSourceLine rd
    idx := lineCurrentIndex readerSourceLine rd
    nx + idx > #buf => nil
    and/[stringChar(x,i) = stringChar(buf,idx + i) for i in 0..nx-1] and nx
  nil

++ Same as matchString except if successful, advance inputstream past `x'.
matchAdvanceString(rd,x) ==
  n := #x >= #quoteIfString currentToken rd and matchString(rd,x) =>
    lineCurrentIndex(readerSourceLine rd) :=
      lineCurrentIndex readerSourceLine rd + n
    c :=
      linePastEnd? readerSourceLine rd => charByName '"Space"
      lineBuffer(readerSourceLine rd).(lineCurrentIndex readerSourceLine rd)
    lineCurrentChar(readerSourceLine rd) := c
    $priorToken := makeToken(makeSymbol x,'IDENTIFIER,$nonblank)
    n
  nil

matchAdvanceKeyword(rd,kwd) ==
  matchToken(currentToken rd,'KEYWORD,kwd) =>
    advanceToken rd
    true
  false

matchKeywordNext(rd,kwd) ==
  matchToken(nextToken rd,'KEYWORD,kwd)

matchSpecial(rd,c) ==
  matchToken(currentToken rd,'SPECIAL_-CHAR,c)

matchAdvanceSpecial(rd,c) ==
  matchSpecial(rd,c) =>
    advanceToken rd
    true
  false

matchAdvanceGlyph(rd,s) ==
  matchToken(currentToken rd,'GLIPH,s) =>
    advanceToken rd
    true
  false

--%
--% Stack abstract datatype.
--%  Operational semantics:
--%     structure Stack ==
--%         Record(store: List T, size: Integer, top: T, updated?: Boolean)

++ Construct a new stack
makeStack() ==
  [nil,0,nil,false]

macro stackStore st ==
  first st

macro stackSize st ==
  second st

macro stackTop st ==
  third st

macro stackUpdated? st ==
  fourth st

stackLoad!(l,st) ==
  stackStore(st) := l
  stackSize(st) := #l
  stackTop(st) := first l

stackClear! st ==
  stackStore(st) := nil
  stackSize(st) := 0
  stackTop(st) := nil
  stackUpdated?(st) := false

stackPush!(x,st) ==
  stackStore(st) := [x,:stackStore st]
  stackTop(st) := x
  stackSize(st) := stackSize st + 1
  stackUpdated?(st) := true

stackPop! st ==
  y := first stackStore st
  stackStore(st) := rest stackStore st
  stackSize(st) := stackSize st - 1
  if stackStore st ~= nil then
    stackTop(st) := first stackStore st
  y


--%  
--% Parsing reduction stack
--%
--% Abstractly;
--%   structure Reduction == Record(rule: RuleName, value: ParseTree)
--%
makeReduction(p == nil,v == nil) ==
  [p,v]

macro reductionRule r ==
  first r

macro reductionValue r ==
  second r

++ stack of results of reduced productions
$reduceStack := makeStack()

pushReduction(rn,pt) ==
  stackPush!(makeReduction(rn,pt),$reduceStack)

popReduction() ==
  stackPop! $reduceStack
  
reduceStackClear() ==
  stackClear! $reduceStack

popStack1() ==
  reductionValue popReduction()

popStack2() ==
  r1 := popReduction()
  r2 := popReduction()
  stackPush!(r1,$reduceStack)
  reductionValue r2

popStack3() ==
  r1 := popReduction()
  r2 := popReduction()
  r3 := popReduction()
  stackPush!(r2,$reduceStack)
  stackPush!(r1,$reduceStack)
  reductionValue r3

popStack4() ==
  r1 := popReduction()
  r2 := popReduction()
  r3 := popReduction()
  r4 := popReduction()
  stackPush!(r3,$reduceStack)
  stackPush!(r2,$reduceStack)
  stackPush!(r1,$reduceStack)
  reductionValue r4

nthStack n ==
  reductionValue stackStore($reduceStack).(n - 1)


--%

ioClear! rd ==
  lineClear! readerSourceLine rd
  tokenStackClear!()
  reduceStackClear()
  nextLinesClear!()
