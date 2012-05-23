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
 
shoeNextLine s==
  bStreamNull s => false
  $linepos := s
  [$f,:$r] := s
  $ln := first $f
  $n := firstNonblankPosition($ln,0)
  $sz := #$ln
  $n = nil => true
  stringChar($ln,$n) = shoeTAB =>
    a := makeString(7-($n rem 8),char " ")
    stringChar($ln,$n) := char " "
    $ln := strconc(a,$ln)
    s1 := [[$ln,:rest $f],:$r]
    shoeNextLine s1
  true
 
shoeLineToks s ==
  $f: local := nil
  $r: local := nil
  $ln: local := nil
  $n: local := nil
  $sz: local := nil
  $floatok: local := true
  $linepos: local := s
  not shoeNextLine s =>  [nil,:nil]
  $n = nil => shoeLineToks $r
  stringChar($ln,0) = char ")" =>
    command := shoeLine? $ln =>
      dq := dqUnit makeToken($linepos,shoeLeafLine command,0)
      [[dq],:$r]
    command := shoeLisp? $ln => shoeLispToken($r,command)
    shoeLineToks $r
  toks := []
  while $n < $sz repeat
    toks := dqAppend(toks,shoeToken())
  toks = nil => shoeLineToks $r
  [[toks],:$r]
 
shoeLispToken(s,string)==
  if #string = 0 or stringChar(string,0) = char ";" then
    string := '""
  ln := $ln
  linepos := $linepos
  [r,:st] := shoeAccumulateLines(s,string)
  dq := dqUnit makeToken(linepos,shoeLeafLisp st,0)
  [[dq],:r]
 
shoeAccumulateLines(s,string)==
  not shoeNextLine s =>  [s,:string]
  $n = nil => shoeAccumulateLines($r,string)
  #$ln = 0 => shoeAccumulateLines($r,string)
  stringChar($ln,0) = char ")" =>
    command := shoeLisp? $ln
    command and #command > 0 =>
      stringChar(command,0) = char ";" =>
		  shoeAccumulateLines($r,string)
      a := charPosition(char ";",command,0) =>
	shoeAccumulateLines($r,
	   strconc(string,subString(command,0,a-1)))
      shoeAccumulateLines($r,strconc(string,command))
    shoeAccumulateLines($r,string)
  [s,:string]

-- returns true if token t is closing `parenthesis'.
shoeCloser t ==
  shoeKeyWord t in '(CPAREN CBRACK)
 
shoeToken() ==
  linepos := $linepos
  n := $n
  ch := stringChar($ln,$n)
  b :=
    shoeStartsComment() =>
      shoeComment()
      []
    shoeStartsNegComment() =>
      shoeNegComment()
      []
    ch = char "!" => shoeLispEscape()
    shoePunctuation codePoint ch => shoePunct()
    shoeStartsId ch => shoeWord(false)
    ch = char " " =>
      shoeSpace()
      []
    ch = char "_"" => shoeString()
    digit? ch => shoeNumber()
    ch = char "__" => shoeEscape()
    ch = shoeTAB =>
      $n := $n + 1
      []
    shoeError()
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
 
shoeLispEscape()==
  $n := $n + 1
  $n >= $sz =>
    SoftShoeError([$linepos,:$n],'"lisp escape error")
    shoeLeafError stringChar($ln,$n)
  a := shoeReadLispString($ln,$n)
  a = nil =>
    SoftShoeError([$linepos,:$n],'"lisp escape error")
    shoeLeafError stringChar($ln,$n)
  [exp,n] := a
  n = nil =>
    $n := $sz
    shoeLeafLispExp exp
  $n := n
  shoeLeafLispExp  exp

shoeEscape() ==
  $n := $n + 1
  shoeEsc() => shoeWord true 
  nil
 
shoeEsc()==
  $n >= $sz =>
    shoeNextLine($r) =>
      while $n = nil repeat shoeNextLine($r)
      shoeEsc()
      false
    false
  n1 := firstNonblankPosition($ln,$n)
  n1 = nil =>
    shoeNextLine($r)
    while $n = nil repeat 
      shoeNextLine($r)
    shoeEsc()
    false
  true
 
shoeStartsComment()==
  $n < $sz =>
    stringChar($ln,$n) = char "+" => 
       www := $n + 1
       www >= $sz => false
       stringChar($ln,www) = char "+"
    false
  false
 
shoeStartsNegComment()==
  $n < $sz =>
    stringChar($ln,$n) = char "-" =>
      www := $n + 1
      www >= $sz => false
      stringChar($ln,www) = char "-"
    false
  false
 
shoeNegComment()==
  n := $n
  $n := $sz
  shoeLeafNegComment subString($ln,n)
 
shoeComment()==
  n := $n
  $n := $sz
  shoeLeafComment subString($ln,n)
 
shoePunct()==
  sss := shoeMatch($ln,$n)
  $n := $n + #sss
  shoeKeyTr sss
 
shoeKeyTr w==
  shoeKeyWord w = "DOT" =>
    $floatok => shoePossFloat(w)
    shoeLeafKey w
  $floatok := not shoeCloser w
  shoeLeafKey w
 
shoePossFloat (w)==
  $n >= $sz or not digit? stringChar($ln,$n) => shoeLeafKey w
  w := shoeInteger()
  shoeExponent('"0",w)
 
shoeSpace()==
  n := $n
  $n := firstNonblankPosition($ln,$n)
  $floatok := true
  $n = nil =>
     shoeLeafSpaces 0
     $n:= # $ln
  shoeLeafSpaces ($n-n)
 
shoeString()==
  $n := $n+1
  $floatok := false
  shoeLeafString shoeS ()
 
shoeS()==
  $n >= $sz =>
    SoftShoeError([$linepos,:$n],'"quote added")
    '""
  n := $n
  strsym := charPosition(char "_"",$ln,$n) or $sz
  escsym := charPosition(char "__",$ln,$n) or $sz
  mn := MIN(strsym,escsym)
  mn=$sz =>
    $n := $sz
    SoftShoeError([$linepos,:$n],'"quote added")
    subString($ln,n)
  mn = strsym =>
    $n := mn + 1
    subString($ln,n,mn-n)
  str := subString($ln,n,mn-n)
  $n := mn+1
  a := shoeEsc()
  b := 
    a =>
      str := strconc(str,charString stringChar($ln,$n))
      $n := $n + 1
      shoeS()
    shoeS()
  strconc(str,b)
 
shoeIdEnd(line,n)==
  while n<#line and shoeIdChar stringChar(line,n) repeat 
    n := n+1
  n
 
shoeW(b) ==
  n1 := $n
  $n := $n+1
  l := $sz
  endid := shoeIdEnd($ln,$n)
  endid = l or stringChar($ln,endid) ~= char "__" => 
    $n := endid
    [b,subString($ln,n1,endid-n1)]
  str := subString($ln,n1,endid-n1)
  $n := endid+1
  a := shoeEsc()
  bb := 
    a => shoeW(true)
    [b,'""]   --  escape finds space or newline
  [bb.0 or b,strconc(str,bb.1)]
 
shoeWord(esp) ==
   aaa:=shoeW(false)
   w:=aaa.1
   $floatok:=false
   esp or aaa.0 =>  shoeLeafId w
   shoeKeyWordP w =>
     $floatok:=true
     shoeLeafKey w
   shoeLeafId  w
 
shoeInteger() ==
  shoeInteger1(false)
 
shoeInteger1(zro) ==
  n := $n
  l := $sz
  while $n <l and digit? stringChar($ln,$n) repeat 
    $n := $n+1
  $n = l or stringChar($ln,$n) ~= char "__" =>
    n = $n and zro => '"0"
    subString($ln,n,$n - n)
  str := subString($ln,n,$n - n)
  $n := $n+1
  a := shoeEsc()
  bb := shoeInteger1(zro)
  strconc(str,bb)
 
shoeIntValue(s) ==
  ns := #s
  ival := 0
  for i in 0..ns-1 repeat
    d := digit? stringChar(s,i)
    ival := 10*ival + d
  ival
 
shoeNumber() ==
  a := shoeInteger()
  $n >= $sz => shoeLeafInteger a
  $floatok and stringChar($ln,$n) = char "." => 
    n := $n
    $n := $n+1
    $n < $sz and stringChar($ln,$n) = char "." =>
      $n := n
      shoeLeafInteger a
    w := shoeInteger1(true)
    shoeExponent(a,w)
  shoeLeafInteger a
 
shoeExponent(a,w)==
  $n >= $sz => shoeLeafFloat(a,w,0)
  n := $n
  c := stringChar($ln,$n)
  c = char "E" or c = char "e" =>
    $n := $n+1
    $n >= $sz =>
      $n := n
      shoeLeafFloat(a,w,0)
    digit? stringChar($ln,$n) =>
      e := shoeInteger()
      e := shoeIntValue e
      shoeLeafFloat(a,w,e)
    c1 := stringChar($ln,$n)
    c1 = char "+" or c1 = char "-" =>
      $n := $n+1
      $n >= $sz =>
	$n := n
	shoeLeafFloat(a,w,0)
      digit? stringChar($ln,$n) =>
	e := shoeInteger()
	e := shoeIntValue e
	shoeLeafFloat(a,w,(c1 = char "-" => MINUS e; e))
      $n := n
      shoeLeafFloat(a,w,0)
    -- FIXME: Missing alternative.
  shoeLeafFloat(a,w,0)
 
shoeError()==
  n := $n
  $n := $n + 1
  SoftShoeError([$linepos,:n],
    strconc( '"The character whose number is ",
	    toString codePoint stringChar($ln,n),'" is not a Boot character"))
  shoeLeafError stringChar($ln,n)
 
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
 
