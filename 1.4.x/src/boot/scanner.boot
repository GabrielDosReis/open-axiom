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
--


import tokens
import includer
namespace BOOTTRAN
module scanner

shoeTAB == abstractChar 9
 
dqUnit s==
  a := [s]
  [a,:a]
 
dqAppend(x,y)==
  x = nil => y
  y = nil => x
  x.rest.rest := first y
  x.rest := rest y
  x
 
dqConcat ld==
  ld = nil => nil
  rest ld = nil => first ld
  dqAppend(first ld,dqConcat rest ld)
 
dqToList s ==
  s = nil => nil
  first s
 
--%
structure %Lexer ==
  Record(line: %String, pos: %Maybe %Short) with
    lexerLineString == (.line)
    lexerCurrentPosition == (.pos)

makeLexer() ==
  mk%Lexer(nil,nil)

++ Return true if the lexer wants a fresh input line.
macro lexerRefresh? lex ==
  lexerCurrentPosition lex = nil

macro lexerLineLength lex ==
  #lexerLineString lex

++ Make the lexer ready to process a new input line.
lexerSetLine!(lex,line) ==
  lexerLineString(lex) := line
  lexerCurrentPosition(lex) := 0

++ Adjust the current position to the next non-blank character.
lexerSkipBlank! lex ==
  lexerCurrentPosition(lex) :=
    firstNonblankPosition(lexerLineString lex,lexerCurrentPosition lex)

++ Move the current position by a given amount   
lexerAdvancePosition!(lex,n == 1) ==
  lexerCurrentPosition(lex) := lexerCurrentPosition lex + n

++ Move the current position to end of line.
lexerSkipToEnd! lex ==
  lexerCurrentPosition(lex) := lexerLineLength lex

++ Set the current position at a given index.
lexerPosition!(lex,k) ==
  lexerCurrentPosition(lex) := k

++ Return the amount of space characters need to complete a tab
++ to its next logical stop.
lexerCharCountToCompleteTab lex ==
  7 - (lexerCurrentPosition lex rem 8)


++ Access the character the current position.
macro lexerCurrentChar lex ==
  stringChar(lexerLineString lex,lexerCurrentPosition lex)

++ Access the character at a given position.
macro lexerCharacterAt(lex,k) ==
  stringChar(lexerLineString lex,k)

++ Return the position of next character `c', or end of line.
lexerCharPosition(lex,c) ==
  charPosition(c,lexerLineString lex,lexerCurrentPosition lex)
    or lexerLineLength lex

++ Return true if the current position is at end of line.
lexerEol? lex ==
  lexerCurrentPosition lex >= lexerLineLength lex

--%  

lexerReadLisp lex ==
  shoeReadLispString(lexerLineString lex,lexerCurrentPosition lex)

shoeNextLine(lex,s) ==
  bStreamNull s => false
  $linepos := s
  [$f,:$r] := s
  lexerSetLine!(lex,sourceLineString $f)
  lexerSkipBlank! lex
  lexerRefresh? lex => true
  lexerCurrentChar lex = shoeTAB =>
    a := makeString(lexerCharCountToCompleteTab lex,char " ")
    lexerCurrentChar(lex) := char " "
    lexerLineString(lex) := strconc(a,lexerLineString lex)
    s1 := [makeSourceLine(lexerLineString lex,sourceLineNumber $f),:$r]
    shoeNextLine(lex,s1)
  true
 
shoeLineToks s ==
  $f: local := nil
  $r: local := nil
  $floatok: local := true
  $linepos: local := s
  lex := makeLexer()
  not shoeNextLine(lex,s) =>  [nil,:nil]
  lexerRefresh? lex => shoeLineToks $r
  lexerCharacterAt(lex,0) = char ")" =>
    command := shoeLine? lexerLineString lex =>
      dq := dqUnit makeToken($linepos,shoeLeafLine command,0)
      [[dq],:$r]
    command := shoeLisp? lexerLineString lex => shoeLispToken(lex,$r,command)
    shoeLineToks $r
  toks := []
  while not lexerEol? lex repeat
    toks := dqAppend(toks,shoeToken lex)
  toks = nil => shoeLineToks $r
  [[toks],:$r]
 
shoeLispToken(lex,s,string)==
  if #string = 0 or stringChar(string,0) = char ";" then
    string := '""
  ln := lexerLineString lex
  linepos := $linepos
  [r,:st] := shoeAccumulateLines(lex,s,string)
  dq := dqUnit makeToken(linepos,shoeLeafLisp st,0)
  [[dq],:r]
 
shoeAccumulateLines(lex,s,string)==
  not shoeNextLine(lex,s) =>  [s,:string]
  lexerRefresh? lex => shoeAccumulateLines(lex,$r,string)
  lexerLineLength lex = 0 => shoeAccumulateLines(lex,$r,string)
  lexerCharacterAt(lex,0) = char ")" =>
    command := shoeLisp? lexerLineString lex
    command and #command > 0 =>
      stringChar(command,0) = char ";" =>
		  shoeAccumulateLines(lex,$r,string)
      a := charPosition(char ";",command,0) =>
	shoeAccumulateLines(lex,$r,
	   strconc(string,subString(command,0,a-1)))
      shoeAccumulateLines(lex,$r,strconc(string,command))
    shoeAccumulateLines(lex,$r,string)
  [s,:string]

-- returns true if token t is closing `parenthesis'.
shoeCloser t ==
  shoeKeyWord t in '(CPAREN CBRACK)
 
shoeToken lex ==
  linepos := $linepos
  n := lexerCurrentPosition lex
  ch := lexerCurrentChar lex
  b :=
    shoeStartsComment lex =>
      shoeComment lex
      []
    shoeStartsNegComment lex =>
      shoeNegComment lex
      []
    ch = char "!" => shoeLispEscape lex
    shoePunctuation codePoint ch => shoePunct lex
    shoeStartsId ch => shoeWord(lex,false)
    ch = char " " =>
      shoeSpace lex
      []
    ch = char "_"" => shoeString lex
    digit? ch => shoeNumber lex
    ch = char "__" => shoeEscape lex
    ch = shoeTAB =>
      lexerAdvancePosition! lex
      []
    shoeError lex
  b = nil => nil
  dqUnit makeToken(linepos,b,n)
 
-- to pair badge and badgee
shoeLeafId x ==  
  ["ID",makeSymbol x]
 
shoeLeafKey x==
  ["KEY",shoeKeyWord x]
 
shoeLeafInteger x==
  ["INTEGER",shoeIntValue x]
 
shoeLeafFloat(a,w,e)==
  b:=shoeIntValue strconc(a,w)
  c:= double b *  EXPT(double 10, e-#w)
  ["FLOAT",c]
 
shoeLeafString x  == 
  ["STRING",x]
 
shoeLeafLisp x    == 
  ["LISP",x]

shoeLeafLispExp x    == 
  ["LISPEXP",x]
 
shoeLeafLine x    == 
  ["LINE",x]
 
shoeLeafComment x == 
  ["COMMENT", x]
 
shoeLeafNegComment x== 
  ["NEGCOMMENT", x]
 
shoeLeafError x   == 
  ["ERROR",x]
 
shoeLeafSpaces x  == 
  ["SPACES",x]
 
shoeLispEscape lex ==
  lexerAdvancePosition! lex
  lexerEol? lex =>
    SoftShoeError([$linepos,:lexerCurrentPosition lex],'"lisp escape error")
    shoeLeafError lexerCurrentChar lex
  a := lexerReadLisp lex
  a = nil =>
    SoftShoeError([$linepos,:lexerCurrentPosition lex],'"lisp escape error")
    shoeLeafError lexerCurrentChar lex
  [exp,n] := a
  n = nil =>
    lexerSkipToEnd! lex
    shoeLeafLispExp exp
  lexerPosition!(lex,n)
  shoeLeafLispExp  exp

shoeEscape lex ==
  lexerAdvancePosition! lex
  shoeEsc lex => shoeWord(lex,true)
  nil
 
shoeEsc lex ==
  lexerEol? lex =>
    shoeNextLine(lex,$r) =>
      while lexerRefresh? lex repeat
        shoeNextLine(lex,$r)
      shoeEsc lex
      false
    false
  n1 := firstNonblankPosition(lexerLineString lex,lexerCurrentPosition lex)
  n1 = nil =>
    shoeNextLine(lex,$r)
    while lexerRefresh? lex repeat 
      shoeNextLine(lex,$r)
    shoeEsc lex
    false
  true
 
shoeStartsComment lex ==
  not lexerEol? lex =>
    lexerCurrentChar lex = char "+" => 
       www := lexerCurrentPosition lex + 1
       www >= lexerLineLength lex => false
       lexerCharacterAt(lex,www) = char "+"
    false
  false
 
shoeStartsNegComment lex ==
  not lexerEol? lex =>
    lexerCurrentChar lex = char "-" =>
      www := lexerCurrentPosition lex + 1
      www >= lexerLineLength lex => false
      lexerCharacterAt(lex,www) = char "-"
    false
  false
 
shoeNegComment lex ==
  n := lexerCurrentPosition lex
  lexerSkipToEnd! lex
  shoeLeafNegComment subString(lexerLineString lex,n)
 
shoeComment lex ==
  n := lexerCurrentPosition lex
  lexerSkipToEnd! lex
  shoeLeafComment subString(lexerLineString lex,n)
 
shoePunct lex ==
  sss := shoeMatch lex
  lexerAdvancePosition!(lex,#sss)
  shoeKeyTr(lex,sss)
 
shoeKeyTr(lex,w) ==
  shoeKeyWord w = "DOT" =>
    $floatok => shoePossFloat(lex,w)
    shoeLeafKey w
  $floatok := not shoeCloser w
  shoeLeafKey w
 
shoePossFloat(lex,w)==
  lexerEol? lex or not digit? lexerCurrentChar lex => shoeLeafKey w
  w := shoeInteger lex
  shoeExponent(lex,'"0",w)
 
shoeSpace lex ==
  n := lexerCurrentPosition lex
  lexerSkipBlank! lex
  $floatok := true
  lexerRefresh? lex =>
     shoeLeafSpaces 0
     lexerSkipToEnd! lex
  shoeLeafSpaces(lexerCurrentPosition lex - n)
 
shoeString lex ==
  lexerAdvancePosition! lex
  $floatok := false
  shoeLeafString shoeS lex
 
shoeS lex ==
  lexerEol? lex =>
    SoftShoeError([$linepos,:lexerCurrentPosition lex],'"quote added")
    '""
  n := lexerCurrentPosition lex
  strsym := lexerCharPosition(lex,char "_"")
  escsym := lexerCharPosition(lex,char "__")
  mn := MIN(strsym,escsym)
  mn = lexerLineLength lex =>
    lexerSkipToEnd! lex
    SoftShoeError([$linepos,:lexerCurrentPosition lex],'"quote added")
    subString(lexerLineString lex,n)
  mn = strsym =>
    lexerPosition!(lex,mn + 1)
    subString(lexerLineString lex,n,mn-n)
  str := subString(lexerLineString lex,n,mn-n)
  lexerPosition!(lex,mn + 1)
  a := shoeEsc lex
  b := 
    a =>
      str := strconc(str,charString lexerCurrentChar lex)
      lexerAdvancePosition! lex
      shoeS lex
    shoeS lex
  strconc(str,b)
 
shoeIdEnd lex ==
  n := lexerCurrentPosition lex
  while n < lexerLineLength lex and shoeIdChar lexerCharacterAt(lex,n) repeat 
    n := n + 1
  n
 
shoeW(lex,b) ==
  n1 := lexerCurrentPosition lex
  lexerAdvancePosition! lex
  l := lexerLineLength lex
  endid := shoeIdEnd lex
  endid = l or lexerCharacterAt(lex,endid) ~= char "__" => 
    lexerPosition!(lex,endid)
    [b,subString(lexerLineString lex,n1,endid-n1)]
  str := subString(lexerLineString lex,n1,endid-n1)
  lexerPosition!(lex,endid + 1)
  a := shoeEsc lex
  bb := 
    a => shoeW(lex,true)
    [b,'""]   --  escape finds space or newline
  [bb.0 or b,strconc(str,bb.1)]
 
shoeWord(lex,esp) ==
   aaa := shoeW(lex,false)
   w:=aaa.1
   $floatok:=false
   esp or aaa.0 =>  shoeLeafId w
   shoeKeyWordP w =>
     $floatok:=true
     shoeLeafKey w
   shoeLeafId  w
 
shoeInteger lex ==
  shoeInteger1(lex,false)
 
shoeInteger1(lex,zro) ==
  n := lexerCurrentPosition lex
  while not lexerEol? lex and digit? lexerCurrentChar lex repeat 
    lexerAdvancePosition! lex
  lexerEol? lex or lexerCurrentChar lex ~= char "__" =>
    n = lexerCurrentPosition lex and zro => '"0"
    subString(lexerLineString lex,n,lexerCurrentPosition lex - n)
  str := subString(lexerLineString lex,n,lexerCurrentPosition lex - n)
  lexerAdvancePosition! lex
  a := shoeEsc lex
  bb := shoeInteger1(lex,zro)
  strconc(str,bb)
 
shoeIntValue(s) ==
  ns := #s
  ival := 0
  for i in 0..ns-1 repeat
    d := digit? stringChar(s,i)
    ival := 10*ival + d
  ival
 
shoeNumber lex ==
  a := shoeInteger lex
  lexerEol? lex => shoeLeafInteger a
  $floatok and lexerCurrentChar lex = char "." => 
    n := lexerCurrentPosition lex
    lexerAdvancePosition! lex
    not lexerEol? lex and lexerCurrentChar lex = char "." =>
      lexerPosition!(lex,n)
      shoeLeafInteger a
    w := shoeInteger1(lex,true)
    shoeExponent(lex,a,w)
  shoeLeafInteger a
 
shoeExponent(lex,a,w)==
  lexerEol? lex => shoeLeafFloat(a,w,0)
  n := lexerCurrentPosition lex
  c := lexerCurrentChar lex
  c = char "E" or c = char "e" =>
    lexerAdvancePosition! lex
    lexerEol? lex =>
      lexerPosition!(lex,n)
      shoeLeafFloat(a,w,0)
    digit? lexerCurrentChar lex =>
      e := shoeInteger lex
      e := shoeIntValue e
      shoeLeafFloat(a,w,e)
    c1 := lexerCurrentChar lex
    c1 = char "+" or c1 = char "-" =>
      lexerAdvancePosition! lex
      lexerEol? lex =>
	lexerPosition!(lex,n)
	shoeLeafFloat(a,w,0)
      digit? lexerCurrentChar lex =>
	e := shoeInteger lex
	e := shoeIntValue e
	shoeLeafFloat(a,w,(c1 = char "-" => MINUS e; e))
      lexerPosition!(lex,n)
      shoeLeafFloat(a,w,0)
    -- FIXME: Missing alternative.
  shoeLeafFloat(a,w,0)
 
shoeError lex ==
  n := lexerCurrentPosition lex
  lexerAdvancePosition! lex
  SoftShoeError([$linepos,:n],
    strconc( '"The character whose number is ",
      toString codePoint lexerCharacterAt(lex,n),
        '" is not a Boot character"))
  shoeLeafError lexerCharacterAt(lex,n)
 
shoeKeyWord st   == 
  tableValue(shoeKeyTable,st)
 
shoeKeyWordP st  ==  
  tableValue(shoeKeyTable,st) ~= nil
 
shoeMatch lex == 
  shoeSubStringMatch(lexerLineString lex,shoeDict,lexerCurrentPosition lex)
 
shoeSubStringMatch(l,d,i) ==
  h := codePoint stringChar(l, i)
  u := d.h
  ll := #l
  done := false
  s1 := '""
  for j in 0.. #u - 1 while not done repeat
    s := u.j
    ls := #s
    done := 
      ls + i > ll => false
      eql := true
      for k in 1..ls-1 while eql repeat
	 eql := stringChar(s,k) = stringChar(l,k+i)
      eql => 
	s1 := s
	true
      false
  s1
 
shoePunctuation c == 
  shoePun.c = 1
 
