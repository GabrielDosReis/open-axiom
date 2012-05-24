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
  Record(line: %String, pos: %Short) with
    lexerLineString == (.line)
    lexerCurrentPosition == (.pos)

makeLexer() ==
  mk%Lexer(nil,nil)

macro lexerLineLength lex ==
  #lexerLineString lex

--%  

shoeNextLine(lex,s) ==
  bStreamNull s => false
  $linepos := s
  [$f,:$r] := s
  lexerLineString(lex) := sourceLineString $f
  $n := firstNonblankPosition(lexerLineString lex,0)
  $n = nil => true
  stringChar(lexerLineString lex,$n) = shoeTAB =>
    a := makeString(7-($n rem 8),char " ")
    stringChar(lexerLineString lex,$n) := char " "
    lexerLineString(lex) := strconc(a,lexerLineString lex)
    s1 := [makeSourceLine(lexerLineString lex,sourceLineNumber $f),:$r]
    shoeNextLine(lex,s1)
  true
 
shoeLineToks s ==
  $f: local := nil
  $r: local := nil
  $n: local := nil
  $floatok: local := true
  $linepos: local := s
  lex := makeLexer()
  not shoeNextLine(lex,s) =>  [nil,:nil]
  $n = nil => shoeLineToks $r
  stringChar(lexerLineString lex,0) = char ")" =>
    command := shoeLine? lexerLineString lex =>
      dq := dqUnit makeToken($linepos,shoeLeafLine command,0)
      [[dq],:$r]
    command := shoeLisp? lexerLineString lex => shoeLispToken(lex,$r,command)
    shoeLineToks $r
  toks := []
  while $n < lexerLineLength lex repeat
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
  $n = nil => shoeAccumulateLines(lex,$r,string)
  lexerLineLength lex = 0 => shoeAccumulateLines(lex,$r,string)
  stringChar(lexerLineString lex,0) = char ")" =>
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
  n := $n
  ch := stringChar(lexerLineString lex,$n)
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
      $n := $n + 1
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
  $n := $n + 1
  $n >= lexerLineLength lex =>
    SoftShoeError([$linepos,:$n],'"lisp escape error")
    shoeLeafError stringChar(lexerLineString lex,$n)
  a := shoeReadLispString(lexerLineString lex,$n)
  a = nil =>
    SoftShoeError([$linepos,:$n],'"lisp escape error")
    shoeLeafError stringChar(lexerLineString lex,$n)
  [exp,n] := a
  n = nil =>
    $n := lexerLineLength lex
    shoeLeafLispExp exp
  $n := n
  shoeLeafLispExp  exp

shoeEscape lex ==
  $n := $n + 1
  shoeEsc lex => shoeWord(lex,true)
  nil
 
shoeEsc lex ==
  $n >= lexerLineLength lex =>
    shoeNextLine(lex,$r) =>
      while $n = nil repeat shoeNextLine(lex,$r)
      shoeEsc lex
      false
    false
  n1 := firstNonblankPosition(lexerLineString lex,$n)
  n1 = nil =>
    shoeNextLine(lex,$r)
    while $n = nil repeat 
      shoeNextLine(lex,$r)
    shoeEsc lex
    false
  true
 
shoeStartsComment lex ==
  $n < lexerLineLength lex =>
    stringChar(lexerLineString lex,$n) = char "+" => 
       www := $n + 1
       www >= lexerLineLength lex => false
       stringChar(lexerLineString lex,www) = char "+"
    false
  false
 
shoeStartsNegComment lex ==
  $n < lexerLineLength lex =>
    stringChar(lexerLineString lex,$n) = char "-" =>
      www := $n + 1
      www >= lexerLineLength lex => false
      stringChar(lexerLineString lex,www) = char "-"
    false
  false
 
shoeNegComment lex ==
  n := $n
  $n := lexerLineLength lex
  shoeLeafNegComment subString(lexerLineString lex,n)
 
shoeComment lex ==
  n := $n
  $n := lexerLineLength lex
  shoeLeafComment subString(lexerLineString lex,n)
 
shoePunct lex ==
  sss := shoeMatch(lexerLineString lex,$n)
  $n := $n + #sss
  shoeKeyTr(lex,sss)
 
shoeKeyTr(lex,w) ==
  shoeKeyWord w = "DOT" =>
    $floatok => shoePossFloat(lex,w)
    shoeLeafKey w
  $floatok := not shoeCloser w
  shoeLeafKey w
 
shoePossFloat(lex,w)==
  $n >= lexerLineLength lex or not digit? stringChar(lexerLineString lex,$n) => shoeLeafKey w
  w := shoeInteger lex
  shoeExponent(lex,'"0",w)
 
shoeSpace lex ==
  n := $n
  $n := firstNonblankPosition(lexerLineString lex,$n)
  $floatok := true
  $n = nil =>
     shoeLeafSpaces 0
     $n:= lexerLineLength lex
  shoeLeafSpaces ($n-n)
 
shoeString lex ==
  $n := $n+1
  $floatok := false
  shoeLeafString shoeS lex
 
shoeS lex ==
  $n >= lexerLineLength lex =>
    SoftShoeError([$linepos,:$n],'"quote added")
    '""
  n := $n
  strsym := charPosition(char "_"",lexerLineString lex,$n) or lexerLineLength lex
  escsym := charPosition(char "__",lexerLineString lex,$n) or lexerLineLength lex
  mn := MIN(strsym,escsym)
  mn = lexerLineLength lex =>
    $n := lexerLineLength lex
    SoftShoeError([$linepos,:$n],'"quote added")
    subString(lexerLineString lex,n)
  mn = strsym =>
    $n := mn + 1
    subString(lexerLineString lex,n,mn-n)
  str := subString(lexerLineString lex,n,mn-n)
  $n := mn+1
  a := shoeEsc lex
  b := 
    a =>
      str := strconc(str,charString stringChar(lexerLineString lex,$n))
      $n := $n + 1
      shoeS lex
    shoeS lex
  strconc(str,b)
 
shoeIdEnd(line,n)==
  while n<#line and shoeIdChar stringChar(line,n) repeat 
    n := n+1
  n
 
shoeW(lex,b) ==
  n1 := $n
  $n := $n+1
  l := lexerLineLength lex
  endid := shoeIdEnd(lexerLineString lex,$n)
  endid = l or stringChar(lexerLineString lex,endid) ~= char "__" => 
    $n := endid
    [b,subString(lexerLineString lex,n1,endid-n1)]
  str := subString(lexerLineString lex,n1,endid-n1)
  $n := endid+1
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
  n := $n
  l := lexerLineLength lex
  while $n <l and digit? stringChar(lexerLineString lex,$n) repeat 
    $n := $n+1
  $n = l or stringChar(lexerLineString lex,$n) ~= char "__" =>
    n = $n and zro => '"0"
    subString(lexerLineString lex,n,$n - n)
  str := subString(lexerLineString lex,n,$n - n)
  $n := $n+1
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
  $n >= lexerLineLength lex => shoeLeafInteger a
  $floatok and stringChar(lexerLineString lex,$n) = char "." => 
    n := $n
    $n := $n+1
    $n < lexerLineLength lex and stringChar(lexerLineString lex,$n) = char "." =>
      $n := n
      shoeLeafInteger a
    w := shoeInteger1(lex,true)
    shoeExponent(lex,a,w)
  shoeLeafInteger a
 
shoeExponent(lex,a,w)==
  $n >= lexerLineLength lex => shoeLeafFloat(a,w,0)
  n := $n
  c := stringChar(lexerLineString lex,$n)
  c = char "E" or c = char "e" =>
    $n := $n+1
    $n >= lexerLineLength lex =>
      $n := n
      shoeLeafFloat(a,w,0)
    digit? stringChar(lexerLineString lex,$n) =>
      e := shoeInteger lex
      e := shoeIntValue e
      shoeLeafFloat(a,w,e)
    c1 := stringChar(lexerLineString lex,$n)
    c1 = char "+" or c1 = char "-" =>
      $n := $n+1
      $n >= lexerLineLength lex =>
	$n := n
	shoeLeafFloat(a,w,0)
      digit? stringChar(lexerLineString lex,$n) =>
	e := shoeInteger lex
	e := shoeIntValue e
	shoeLeafFloat(a,w,(c1 = char "-" => MINUS e; e))
      $n := n
      shoeLeafFloat(a,w,0)
    -- FIXME: Missing alternative.
  shoeLeafFloat(a,w,0)
 
shoeError lex ==
  n := $n
  $n := $n + 1
  SoftShoeError([$linepos,:n],
    strconc( '"The character whose number is ",
      toString codePoint stringChar(lexerLineString lex,n),
        '" is not a Boot character"))
  shoeLeafError stringChar(lexerLineString lex,n)
 
shoeKeyWord st   == 
  tableValue(shoeKeyTable,st)
 
shoeKeyWordP st  ==  
  tableValue(shoeKeyTable,st) ~= nil
 
shoeMatch(l,i) == 
  shoeSubStringMatch(l,shoeDict,i)
 
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
 
