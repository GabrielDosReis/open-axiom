-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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

-- converts X to double-float.
double x ==
  FLOAT(x, 1.0)
 
dqUnit s==
  a := [s]
  [a,:a]
 
dqAppend(x,y)==
  null x => y
  null y => x
  RPLACD (rest x,first y)
  RPLACD (x,    rest y)
  x
 
dqConcat ld==
  null ld => nil
  null rest ld => first ld
  dqAppend(first ld,dqConcat rest ld)
 
dqToList s ==
  null s => nil
  first s
 
shoeConstructToken(ln,lp,b,n) == 
  [b.0,b.1,:cons(lp,n)]

shoeTokType x == 
  first x

shoeTokPart x == 
  second x

shoeTokPosn x == 
  CDDR x

shoeTokConstruct(x,y,z) ==
  [x,y,:z]
 
shoeNextLine(s)==
  bStreamNull s => false
  $linepos:=s
  $f:= first s
  $r:= rest s
  $ln:=first $f
  $n:=STRPOSL('" ",$ln,0,true)
  $sz :=# $ln
  null $n => true
  QENUM($ln,$n)=shoeTAB =>
    a:=MAKE_-FULL_-CVEC (7-REM($n,8) ,'" ")
    $ln.$n:='" ".0
    $ln:=CONCAT(a,$ln)
    s1:=cons(cons($ln,rest $f),$r)
    shoeNextLine s1
  true
 
shoeLineToks(s)==
  $f: local:=nil
  $r:local :=nil
  $ln:local :=nil
  $n:local:=nil
  $sz:local := nil
  $floatok:local:=true
  $linepos:local:=s
  not shoeNextLine s =>  CONS(nil,nil)
  null $n => shoeLineToks $r
  fst:=QENUM($ln,0)
  EQL(fst,shoeCLOSEPAREN)=>
    command:=shoeLine? $ln=>
      dq:=dqUnit shoeConstructToken
	       ($ln,$linepos,shoeLeafLine command,0)
      cons([dq],$r)
    command:=shoeLisp? $ln=> shoeLispToken($r,command)
    command:=shoePackage? $ln=>
      a:=CONCAT('"(IN-PACKAGE ",command,'")")
      dq:=dqUnit shoeConstructToken
	       ($ln,$linepos,shoeLeafLisp a,0)
      cons([dq],$r)
    shoeLineToks $r
  toks:=[]
  while $n<$sz repeat toks:=dqAppend(toks,shoeToken())
  null toks => shoeLineToks $r
  cons([toks],$r)
 
shoeLispToken(s,string)==
  string:=
    # string=0 or EQL(QENUM(string,0),QENUM('";",0))=> '""
    string
  ln:=$ln
  linepos:=$linepos
  [r,:st]:=shoeAccumulateLines(s,string)
  dq:=dqUnit shoeConstructToken(ln,linepos,shoeLeafLisp st,0)
  cons([dq],r)
 
shoeAccumulateLines(s,string)==
  not shoeNextLine s =>  CONS(s,string)
  null $n => shoeAccumulateLines($r,string)
  # $ln=0 => shoeAccumulateLines($r,string)
  fst:=QENUM($ln,0)
  EQL(fst,shoeCLOSEPAREN)=>
    command:=shoeLisp? $ln
    command and #command>0 =>
      EQL(QENUM(command,0),QENUM('";",0))=>
		  shoeAccumulateLines($r,string)
      a:=STRPOS('";",command,0,nil)
      a=>
	shoeAccumulateLines($r,
	   CONCAT(string,SUBSTRING(command,0,a-1)))
      shoeAccumulateLines($r,CONCAT(string,command))
    shoeAccumulateLines($r,string)
  CONS(s,string)

-- returns true if token t is closing `parenthesis'.
shoeCloser t ==
  shoeKeyWord t in '(CPAREN CBRACK)
 
shoeToken () ==
  ln:=$ln
  c:=QENUM($ln,$n)
  linepos:=$linepos
  n:=$n
  ch:=$ln.$n
  b:=
    shoeStartsComment()          =>
		   shoeComment()
		   []
    shoeStartsNegComment()       =>
		   shoeNegComment()
		   []
    c=shoeLispESCAPE      =>
		   shoeLispEscape()
    shoePunctuation c        => shoePunct ()
    shoeStartsId ch          => shoeWord  (false)
    c=shoeSPACE              =>
		   shoeSpace ()
		   []
    c = shoeSTRING_CHAR        => shoeString ()
    shoeDigit ch               => shoeNumber ()
    c=shoeESCAPE               => shoeEscape()
    c=shoeTAB                  =>
			       $n:=$n+1
			       []
    shoeError ()
  null b => nil
  dqUnit shoeConstructToken(ln,linepos,b,n)
 
-- to pair badge and badgee
shoeLeafId x ==  
  ["ID",INTERN x]
 
shoeLeafKey x==
  ["KEY",shoeKeyWord x]
 
shoeLeafInteger x==
  ["INTEGER",shoeIntValue x]
 
shoeLeafFloat(a,w,e)==
  b:=shoeIntValue CONCAT(a,w)
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
  $n:=$n+1
  $n >= $sz =>
    SoftShoeError(cons($linepos,$n),'"lisp escape error")
    shoeLeafError ($ln.$n)
  a:=shoeReadLispString($ln,$n)
  null a =>
    SoftShoeError(cons($linepos,$n),'"lisp escape error")
    shoeLeafError ($ln.$n)
  [exp,n]:=a
  null n =>
    $n:= $sz
    shoeLeafLispExp  exp
  $n:=n
  shoeLeafLispExp  exp

shoeEscape()==
  $n:=$n+1
  shoeEsc() => shoeWord true 
  nil
 
shoeEsc()==
  $n >= $sz =>
    shoeNextLine($r) =>
      while null $n repeat shoeNextLine($r)
      shoeEsc()
      false
    false
  n1:=STRPOSL('" ",$ln,$n,true)
  null n1 =>
    shoeNextLine($r)
    while null $n repeat 
      shoeNextLine($r)
    shoeEsc()
    false
  true
 
shoeStartsComment()==
  $n < $sz =>
    QENUM($ln,$n) = shoePLUSCOMMENT => 
       www:=$n+1
       www >= $sz => false
       QENUM($ln,www) = shoePLUSCOMMENT
    false
  false
 
shoeStartsNegComment()==
  $n < $sz =>
    QENUM($ln,$n) = shoeMINUSCOMMENT =>
      www:=$n+1
      www >= $sz => false
      QENUM($ln,www) = shoeMINUSCOMMENT
    false
  false
 
shoeNegComment()==
  n := $n
  $n := $sz
  shoeLeafNegComment SUBSTRING($ln,n,nil)
 
shoeComment()==
  n := $n
  $n := $sz
  shoeLeafComment SUBSTRING($ln,n,nil)
 
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
  $n>=$sz or not shoeDigit $ln.$n => shoeLeafKey w
  w := shoeInteger()
  shoeExponent('"0",w)
 
shoeSpace()==
  n := $n
  $n := STRPOSL('" ",$ln,$n,true)
  $floatok := true
  null $n =>
     shoeLeafSpaces 0
     $n:= # $ln
  shoeLeafSpaces ($n-n)
 
shoeString()==
  $n := $n+1
  $floatok := false
  shoeLeafString shoeS ()
 
shoeS()==
  $n >= $sz =>
    SoftShoeError(cons($linepos,$n),'"quote added")
    '""
  n := $n
  strsym := STRPOS ('"_"",$ln,$n,nil) or $sz
  escsym := STRPOS ('"__",$ln,$n,nil) or $sz
  mn := MIN(strsym,escsym)
  mn=$sz =>
    $n:=$sz
    SoftShoeError(cons($linepos,$n),'"quote added")
    SUBSTRING($ln,n,nil)
  mn = strsym =>
    $n:=mn+1
    SUBSTRING($ln,n,mn-n)
  str := SUBSTRING($ln,n,mn-n)
  $n := mn+1
  a := shoeEsc()
  b := 
    a =>
      str := CONCAT(str,$ln.$n)
      $n := $n+1
      shoeS()
    shoeS()
  CONCAT(str,b)
 
 
 
 
shoeIdEnd(line,n)==
  while n<#line and shoeIdChar line.n repeat 
    n := n+1
  n
 
 
shoeDigit x== 
  DIGIT_-CHAR_-P x
 
shoeW(b)==
  n1 := $n
  $n := $n+1
  l := $sz
  endid := shoeIdEnd($ln,$n)
  endid=l or QENUM($ln,endid) ~= shoeESCAPE => 
    $n := endid
    [b,SUBSTRING($ln,n1,endid-n1)]
  str := SUBSTRING($ln,n1,endid-n1)
  $n := endid+1
  a := shoeEsc()
  bb := 
    a => shoeW(true)
    [b,'""]   --  escape finds space or newline
  [bb.0 or b,CONCAT(str,bb.1)]
 
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
  while $n <l and shoeDigit($ln.$n) repeat 
    $n := $n+1
  $n=l or QENUM($ln,$n)~=shoeESCAPE =>
    n = $n and zro => '"0"
    SUBSTRING($ln,n,$n-n)
  str := SUBSTRING($ln,n,$n-n)
  $n := $n+1
  a := shoeEsc()
  bb := shoeInteger1(zro)
  CONCAT(str,bb)
 
shoeIntValue(s) ==
  ns := #s
  ival := 0
  for i in 0..ns-1 repeat
      d := shoeOrdToNum ELT(s,i)
      ival := 10*ival + d
  ival
 
shoeNumber() ==
  a := shoeInteger()
  $n >= $sz => shoeLeafInteger a
  $floatok and QENUM($ln,$n) = shoeDOT => 
    n := $n
    $n := $n+1
    $n < $sz and QENUM($ln,$n)=shoeDOT =>
      $n := n
      shoeLeafInteger a
    w := shoeInteger1(true)
    shoeExponent(a,w)
  shoeLeafInteger a
 
shoeExponent(a,w)==
  $n >= $sz => shoeLeafFloat(a,w,0)
  n := $n
  c := QENUM($ln,$n)
  c = shoeEXPONENT1 or c = shoeEXPONENT2 =>
    $n := $n+1
    $n >= $sz =>
      $n := n
      shoeLeafFloat(a,w,0)
    shoeDigit($ln.$n) =>
      e := shoeInteger()
      e := shoeIntValue e
      shoeLeafFloat(a,w,e)
    c1 := QENUM($ln,$n)
    c1 = shoePLUSCOMMENT or c1 = shoeMINUSCOMMENT =>
      $n := $n+1
      $n >= $sz =>
	$n := n
	shoeLeafFloat(a,w,0)
      shoeDigit($ln.$n) =>
	e := shoeInteger()
	e := shoeIntValue e
	shoeLeafFloat(a,w,(c1=shoeMINUSCOMMENT => MINUS e; e))
      $n := n
      shoeLeafFloat(a,w,0)
    -- FIXME: Missing alternative.
  shoeLeafFloat(a,w,0)
 
shoeError()==
  n:=$n
  $n:=$n+1
  SoftShoeError(cons($linepos,n),
    CONCAT( '"The character whose number is ",
	    STRINGIMAGE QENUM($ln,n),'" is not a Boot character"))
  shoeLeafError ($ln.n)
 
shoeOrdToNum x== 
  DIGIT_-CHAR_-P x
 
shoeKeyWord st   == 
  GETHASH(st,shoeKeyTable)
 
shoeKeyWordP st  ==  
  not null GETHASH(st,shoeKeyTable)
 
shoeMatch(l,i) == 
  shoeSubStringMatch(l,shoeDict,i)
 
shoeSubStringMatch (l,d,i)==
  h := QENUM(l, i)
  u := ELT(d,h)
  ll := SIZE l
  done := false
  s1 := '""
  for j in 0.. SIZE u - 1 while not done repeat
    s := ELT(u,j)
    ls := SIZE s
    done := 
      ls+i > ll => false
      eql := true
      for k in 1..ls-1 while eql repeat
	 eql := EQL(QENUM(s,k),QENUM(l,k+i))
      eql => 
	s1:=s
	true
      false
  s1
 
shoePunctuation c == 
  shoePun.c = 1
 
