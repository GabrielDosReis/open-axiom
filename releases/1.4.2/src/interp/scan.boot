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


import bits
import dq
import incl
import sys_-utility
namespace BOOT
module scan

--%

$RDigits ==
  '"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

$smallLetters ==
  '"abcdefghijklmnopqrstuvwxyz"

--% Keywords

scanKeyWords == [ _
           ['"add",      "ADD" ],_
           ['"and",      "AND" ],_
           ['"assume","ASSUME" ],_
           ['"break",   "BREAK" ],_
           ['"by",        "BY" ],_
           ['"case",     "CASE" ],_
           ['"default",  "DEFAULT" ],_
           ['"define",  "DEFN" ],_
           ['"do",        "DO"],_
           ['"else",    "ELSE" ],_
           ['"exist",   "EXIST"],_
           ['"exit",    "EXIT" ],_
           ['"export","EXPORT" ],_
           ['"forall", "FORALL"],_
           ['"for",      "FOR" ],_
           ['"free",    "FREE" ],_
           ['"from",    "FROM" ],_
           ['"has",      "HAS" ],_
           ['"if",       "IF" ],_
           ['"import", "IMPORT" ],_
           ['"in", "IN" ],_
           ['"inline", "INLINE" ],_
           ['"is", "IS" ],_
           ['"isnt", "ISNT" ],_
           ['"iterate", "ITERATE"],_
           ['"local", "local" ],_
           ['"macro", "MACRO" ],_
           ['"mod", "MOD" ],_
           ['"or", "OR" ],_
           ['"pretend","PRETEND" ],_
           ['"quo","QUO" ],_
           ['"rem","REM" ],_
           ['"repeat","REPEAT" ],_
           ['"return","RETURN" ],_
           ['"rule","RULE" ],_
           ['"then","THEN" ],_
           ['"where","WHERE" ],_
           ['"while","WHILE" ],_
           ['"with","WITH" ],_
           ['"|","BAR"],_
           ['".","DOT" ],_
           ['"::","COERCE" ],_
           ['":","COLON" ],_
           ['":-","COLONDASH" ],_
           ['"@","AT" ],_
           ['",","COMMA" ],_
           ['";","SEMICOLON" ],_
           ['"**","POWER" ],_
           ['"*","TIMES" ],_
           ['"+","PLUS" ],_
           ['"-","MINUS" ],_
           ['"<","LT" ],_
           ['">","GT" ],_
           ['"<=","LE" ],_
           ['">=","GE" ],_
           ['"=", "EQUAL"],_
           ['"~=","NOTEQUAL" ],_
           ['"~","~" ],_
           ['"^","CARAT" ],_
           ['"..","SEG" ],_
           ['"#","#" ],_
           ['"&","AMPERSAND" ],_
           ['"$","$" ],_
           ['"/","SLASH" ],_
           ['"\","BACKSLASH" ],_
           ['"//","SLASHSLASH" ],_
           ['"\\","BACKSLASHBACKSLASH" ],_
           ['"/\","SLASHBACKSLASH" ],_
           ['"\/","BACKSLASHSLASH" ],_
           ['"=>","EXIT" ],_
           ['":=","BECOMES" ],_
           ['"==","DEF" ],_
           ['"==>","MDEF" ],_
           ['"->","ARROW" ],_
           ['"<-","LARROW" ],_
           ['"+->","GIVES" ],_
           ['"(","(" ],_
           ['")",")" ],_
           ['"(|","(|" ],_
           ['"|)","|)" ],_
           ['"[","[" ],_
           ['"]","]" ],_
           ['"[__]","[]" ],_
           ['"{","{" ],_
           ['"}","}" ],_
           ['"{__}","{}" ],_
           ['"[|","[|" ],_
           ['"|]","|]" ],_
           ['"[|__|]","[||]" ],_
           ['"{|","{|" ],_
           ['"|}","|}" ],_
           ['"{|__|}","{||}" ],_
           ['"<<","OANGLE" ],_
           ['">>","CANGLE" ],_
           ['"'", "'" ],_
           ['"`", "BACKQUOTE" ]_
                          ]


scanKeyTableCons()==
  KeyTable := hashTable 'EQUAL
  for st in scanKeyWords repeat
    tableValue(KeyTable,first st) := second st
  KeyTable

scanKeyTable:=scanKeyTableCons()


scanInsert(s,d) ==
  l := #s
  h := codePoint stringChar(s,0)
  u := vectorRef(d,h)
  n := #u
  k:=0
  while l <= #vectorRef(u,k) repeat
    k := k+1
  v := newVector(n+1)
  for i in 0..k-1 repeat
    vectorRef(v,i) := vectorRef(u,i)
  vectorRef(v,k) := s
  for i in k..n-1 repeat
    vectorRef(v,i+1) := vectorRef(u,i)
  vectorRef(d,h) := v
  s

scanDictCons()==
  d :=
    a := newVector 256
    b := newVector 1
    vectorRef(b,0) := '""
    for i in 0..255 repeat
      vectorRef(a,i) := b
    a
  for [s,:.] in entries scanKeyTable repeat
    scanInsert(s,d)
  d

scanDict:=scanDictCons()


scanPunCons()==
  a := makeBitVector 256
  for i in 0..255 repeat
    bitref(a,i) := 0
  for [k,:.] in entries scanKeyTable repeat
    if not startsId? stringChar(k,0) then
      bitref(a,codePoint stringChar(k,0)) := 1
  a

scanPun:=scanPunCons()

--for i in ["COLON","MINUS"] repeat
--   property(i,'PREGENERIC) := true

for i in   [ _
   ["EQUAL"    ,"="], _
   ["TIMES"    ,"*"], _
   ["HAS"      ,"has"], _
   ["CASE"     ,"case"], _
   ["REM"      ,"rem"], _
   ["MOD"      ,"mod"], _
   ["QUO"      ,"quo"], _
   ["SLASH"    ,"/"], _
   ["BACKSLASH","\"], _
   ["SLASHSLASH"    ,"//"], _
   ["BACKSLASHBACKSLASH","\\"], _
   ["SLASHBACKSLASH"    ,"/\"], _
   ["BACKSLASHSLASH","\/"], _
   ["POWER"    ,"**"], _
   ["CARAT"    ,"^"], _
   ["PLUS"     ,"+"], _
   ["MINUS"    ,"-"], _
   ["LT"       ,"<"], _
   ["GT"       ,">"], _
   ["OANGLE"       ,"<<"], _
   ["CANGLE"       ,">>"], _
   ["LE"       ,"<="], _
   ["GE"       ,">="], _
   ["NOTEQUAL" ,"~="], _
   ["BY"       ,"by"], _
   ["ARROW"       ,"->"], _
   ["LARROW"       ,"<-"], _
   ["BAR"       ,"|"], _
   ["SEG"       ,".."] _
    ] repeat property(first i,'INFGENERIC) := second i

-- Scanner

--  lineoftoks  bites off a token-dq from a line-stream
--  returning the token-dq and the rest of the line-stream

scanIgnoreLine(ln,n)==
  n = nil => n
  stringChar(ln,0) = char ")" =>
    incPrefix?('"command",1,ln) => true
    nil
  n

nextline(s)==
  npNull s => false
  $f := first s
  $r := rest s
  $ln := rest $f
  $linepos := CAAR $f
  $n := STRPOSL('" ",$ln,0,true)-- spaces at beginning
  $sz := #$ln
  true

lineoftoks(s)==
  $f: local := nil
  $r: local := nil
  $ln: local := nil
  $linepos: local := nil
  $n: local := nil
  $sz: local := nil
  $floatok: local := true
  not nextline s => [nil,:nil]
  null scanIgnoreLine($ln,$n) => [nil,:$r] -- line of spaces or starts ) or >
  toks := []
  a := incPrefix?('"command",1,$ln)
  a =>
    $ln := subString($ln,8)
    b := dqUnit constoken($linepos,["command",$ln],0)
    [[[b,s]],:$r]
  while $n<$sz repeat
    toks := dqAppend(toks,scanToken())
  null toks => [nil,:$r]
  [[[toks,s]],:$r]


scanToken() ==
  linepos := $linepos
  n := $n
  ch := stringChar($ln,$n)
  b :=
    startsComment?() =>
      scanComment()
      []
    startsNegComment?() =>
      scanNegComment()
      []
    ch = char "?" =>
      $n := $n+1
      lfid '"?"
    punctuation? codePoint ch => scanPunct()
    startsId? ch => scanWord(false)
    ch = char " " =>
      scanSpace()
      []
    ch = char "_"" => scanString()
    digit? ch => scanNumber()
    ch = char "__" => scanEscape()
    scanError()
  null b => nil
  dqUnit constoken(linepos,b,n+lnExtraBlanks linepos)

-- to pair badge and badgee

lfid x ==
  ["id",makeSymbol(x, '"BOOT")]

lfkey x ==
  ["key",keyword x]

lfinteger x==
  ["integer",x]

lfrinteger (r,x)==
  ["integer",strconc (r,strconc('"r",x))]

--lfrfloat(a,w,v)==["rfloat",strconc(a,'"r.",v)]

lffloat(a,w,e)==
  ["float",strconc(a,'".",w,'"e",e)]

lfstring x==
  #x = 1 => ["char",x]
  ["string",x]

lfcomment x==
  ["comment", x]

lfnegcomment x ==
  ["negcomment", x]

lferror x ==
  ["error",x]

lfspaces x ==
  ["spaces",x]

constoken(lp,b,n)==
  a := [b.0,:b.1]
  ncPutQ(a,"posn",[lp,:n])
  a

scanEscape()==
  $n := $n+1
  scanEsc() => scanWord true
  nil

scanEsc()==
  $n >= $sz =>
     nextline($r) =>
       while null $n repeat nextline($r)
       scanEsc()
       false
     false
  n1 := STRPOSL('" ",$ln,$n,true)
  n1 = nil =>
    nextline($r) =>
      while null $n repeat
        nextline($r)
      scanEsc()
      false
    false
  $n = n1 => true
  stringChar($ln,n1) = char "__" =>
    $n := n1+1
    scanEsc()
    false
  $n := n1
  startsNegComment?() or startsComment?() =>
    nextline($r)
    scanEsc()
    false
  false

startsComment?()==
  $n < $sz => 
    stringChar($ln,$n) = char "+" =>
      www := $n + 1
      www >= $sz => false
      stringChar($ln,www) = char "+"
    false
  false

startsNegComment?()==
  $n < $sz =>
    stringChar($ln,$n) = char "-" =>
      www := $n+1
      www >= $sz => false
      stringChar($ln,www) = char "-"
    false
  false

scanNegComment()==
  n := $n
  $n := $sz
  lfnegcomment subString($ln,n)

scanComment()==
  n := $n
  $n := $sz
  lfcomment subString($ln,n)

scanPunct()==
  sss := subMatch($ln,$n)
  a := #sss
  a = 0 => scanError()
  $n := $n+a
  scanKeyTr sss

scanKeyTr w==
  keyword w = "DOT" =>
    $floatok => scanPossFloat(w)
    lfkey w
  $floatok := not scanCloser? w
  lfkey w

scanPossFloat (w)==
  $n >= $sz or not digit? $ln.$n => lfkey w
  w := spleI(function digit?)
  scanExponent('"0",w)

scanCloser == [")","}","]","|)","|}","|]"]

scanCloser? w== symbolMember?(keyword w,scanCloser)

scanSpace()==
  n := $n
  $n := STRPOSL('" ",$ln,$n,true)
  if $n = nil then $n := #$ln
  $floatok := true
  lfspaces($n-n)

scanString()==
  $n := $n+1
  $floatok := false
  lfstring scanS()

scanS()==
  $n >= $sz =>
    ncSoftError([$linepos,:lnExtraBlanks $linepos+$n],"S2CN0001",[])
    '""
  n := $n
  strsym := findChar(char "_"",$ln,$n) or $sz
  escsym := findChar(char "__",$ln,$n) or $sz
  mn := MIN(strsym,escsym)
  mn = $sz =>
    $n:=$sz
    ncSoftError([$linepos,:lnExtraBlanks $linepos+$n],
            "S2CN0001",[])
    subString($ln,n)
  mn = strsym =>
    $n:=mn+1
    subString($ln,n,mn-n)
  --escape is found first
  str := subString($ln,n,mn-n)-- before escape
  $n := mn+1
  a := scanEsc() -- case of end of line when false
  b :=
    a =>
      str := strconc(str,scanTransform($ln.$n))
      $n := $n+1
      scanS()
    scanS()
  strconc(str,b)

scanTransform x ==
  x

--idChar? x== scanLetter x or digit? x or x in '(_? _%)

--scanLetter x==
--   if not char? x
--   then false
--   else STRPOSL(scanTrTable,x,0,nil)

posend(line,n)==
  while n<#line and idChar? line.n repeat
    n := n+1
  n

--numend(line,n)==
--     while n<#line and digit? line.n repeat n:=n+1
--     n

--startsId? x==  scanLetter x or x in '(_? _%)

scanW(b)==             -- starts pointing to first char
  n1 := $n         -- store starting character position
  $n := $n+1          -- the first character is not tested
  l := $sz
  endid := posend($ln,$n)
  endid=l or stringChar($ln,endid) ~= char "__" =>
     -- not escaped
    $n:=endid
    [b,subString($ln,n1,endid-n1)]   -- l overflows
  -- escape and endid ~= l
  str := subString($ln,n1,endid-n1)
  $n := endid+1
  a := scanEsc()
  bb :=
    a => scanW(true) -- escape nonspace
    $n >= $sz => [b,'""]
    idChar?($ln.$n) => scanW(b)
    [b,'""]
  [bb.0 or b,strconc(str,bb.1)]

scanWord(esp) ==
  aaa := scanW(false)
  w := aaa.1
  $floatok := false
  esp or aaa.0 => lfid w
  keyword? w =>
     $floatok:=true
     lfkey w
  lfid  w

spleI(dig) ==
  spleI1(dig,false)

spleI1(dig,zro) ==
  n := $n
  l := $sz
  while $n<l and FUNCALL(dig,($ln.$n)) repeat
    $n := $n+1
  $n = l or stringChar($ln,$n) ~= char "__" =>
     n = $n and zro => '"0"
     subString($ln,n,$n-n)
  -- escaped
  str:=subString($ln,n,$n-n)
  $n:=$n+1
  a:=scanEsc()
  bb:=spleI1(dig,zro)-- escape, anyno spaces are ignored
  strconc(str,bb)

scanCheckRadix(a,w)==
  r := readInteger a
  ns := #w
  ns = 0 => 
    ncSoftError([$linepos,:lnExtraBlanks $linepos+$n],"S2CN0004",[a])
  done := false
  for i in 0..ns-1  repeat
    a := rdigit? w.i
    a = nil or a>=r =>
      ncSoftError([$linepos,:lnExtraBlanks $linepos+$n-ns+i],"S2CN0002", [w.i])

scanNumber() ==
  a := spleI(function digit?)
  $n >= $sz => lfinteger a
  stringChar($ln,$n) ~= char "r" =>
    if $floatok and stringChar($ln,$n) = char "."
    then
      n:=$n
      $n:=$n+1
      if  $n<$sz and stringChar($ln,$n) = char "."
      then
        $n:=n
        lfinteger a
      else
        w:=spleI1(function digit?,true)
        scanExponent(a,w)
    else lfinteger a
  $n := $n+1
  w := spleI1(function rdigit?,false)
  scanCheckRadix(a,w)
  $n >= $sz => lfrinteger(a,w)
  stringChar($ln,$n) = char "." =>
    n := $n
    $n := $n+1
    $n < $sz and stringChar($ln,$n) = char "." =>
      $n :=n
      lfrinteger(a,w)
    v := spleI1(function rdigit?,true)
    scanCheckRadix(a,v)
    scanExponent(strconc(a,'"r",w),v)
  lfrinteger(a,w)

scanExponent(a,w)==
  $n >= $sz => lffloat(a,w,'"0")
  n := $n
  c := stringChar($ln,$n)
  c = char "E" or c = char "e" =>
    $n := $n + 1
    $n >= $sz =>
      $n:=n
      lffloat(a,w,'"0")
    digit?($ln.$n) =>
      e := spleI(function digit?)
      lffloat(a,w,e)
    c := stringChar($ln,$n)
    c = char "+" or c = char "-" =>
      $n := $n + 1
      $n >= $sz =>
        $n := n
        lffloat(a,w,'"0")
      digit? stringChar($ln,$n) =>
        e := spleI(function digit?)
        lffloat(a,w,(c = char "-" => strconc('"-",e); e))
      $n := n
      lffloat(a,w,'"0")
  lffloat(a,w,'"0")

rdigit? x==
   d := findChar(x,$RDigits) => d
   d := findChar(x,$smallLetters) => 10 + d
   nil

scanError()==
  n := $n
  $n := $n+1
  ncSoftError([$linepos,:lnExtraBlanks $linepos+$n],"S2CN0003",[$ln.n])
  lferror($ln.n)

keyword st ==
  tableValue(scanKeyTable,st)

keyword? st ==
  not null tableValue(scanKeyTable,st)

subMatch(l,i) ==
  substringMatch(l,scanDict,i)

substringMatch (l,d,i)==
  h := codePoint stringChar(l, i)
  u := d.h
  ll := #l
  done := false
  s1 := '""
  for j in 0..#u - 1 while not done repeat
     s := u.j
     ls := #s
     done :=
       ls+i > ll => false
       eql := true
       for k in 1..ls-1 while eql repeat
         eql := stringChar(s,k) = stringChar(l,k+i)
       eql =>
         s1 := s
         true
       false
  s1



punctuation? c ==
  scanPun.c=1

