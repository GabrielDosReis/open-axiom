-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis
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


import unlisp
import cstream
import cformat
namespace BOOT
 
++ The following symbol constants tag input source program parts, for
++ conditional inclusion purposes.
-- Outside an conditional inclusion group.
Top             == 01
-- `)if' group
IfSkipToEnd     == 10
IfKeepPart      == 11
IfSkipPart      == 12
-- `)elseif' group
ElseifSkipToEnd == 20
ElseifKeepPart  == 21
ElseifSkipPart  == 22
-- `)else' group
ElseSkipToEnd   == 30
ElseKeepPart    == 31
 
Top?     (st) == st quo 10 = 0
If?      (st) == st quo 10 = 1
Elseif?  (st) == st quo 10 = 2
Else?    (st) == st quo 10 = 3
SkipEnd? (st) == st rem 10 = 0
KeepPart?(st) == st rem 10 = 1
SkipPart?(st) == st rem 10 = 2
Skipping?(st) == not KeepPart? st
 
incStringStream s==
   incRenumber incLude(0,incRgen s,0,['"strings"] ,[Top])
 
incFile fn==
   incRenumber incLude(0,incRgen inputTextFile fn,0,[fn],[Top])
 
incStream(st, fn) ==
   incRenumber incLude(0,incRgen st,0,[fn],[Top])

incFileInput    fn == incRgen  MAKE_-INSTREAM fn
incConsoleInput () == incRgen  MAKE_-INSTREAM 0
 
incLine(eb, str, gno, lno, ufo) ==
  ln := lnCreate(eb,str,gno,lno,ufo)
  [[ln,:1],:str]
 
incPos f == first f
 
incRenumberItem(f, i) ==
  l := CAAR f
  lnSetGlobalNum(l, i)
  f
 
incRenumberLine(xl, gno) ==
  l := incRenumberItem(xl.0, gno)
  incHandleMessage xl
  l
 
incRenumber ssx ==
  incZip (function incRenumberLine, ssx, incIgen 0)
 
incPrefix?(prefix, start, whole) ==
  #prefix > #whole-start => false
  good:=true
  for i in 0..#prefix-1 for j in start.. while good repeat
      good:= prefix.i = whole.j
  good
 
incCommand?(s) ==
  #s > 0 and stringChar(s,0) = char ")"
 
incCommands :=
  ['"say"    , _
   '"include", _
   '"console", _
   '"fin"    , _
   '"assert" , _
   '"if"     , _
   '"elseif" , _
   '"else"   , _
   '"endif" ]

++ when non-nil, an integer that indicates the current line number.
$inputLineNumber := nil 

incClassify(s) ==
  $inputLineNumber = 0 and incPrefix?('"#!",0,s) =>
    [true,0,'"magicNumber"]
  not incCommand? s => [false,0, '""]
  i := 1; n := #s
  while i < n and s.i = char " " repeat i := i + 1
  i >= n => [true,0,'"other"]
  eb := (i = 1 => 0; i)
  bad:=true
  for p in incCommands while bad repeat
    incPrefix?(p, i, s) =>
      bad:=false
      p1 :=p
  if bad then [true,0,'"other"] else [true,eb,p1]
 
incCommandTail(s, info) ==
  start := (info.1 = 0 => 1; info.1)
  incDrop(start+#info.2+1, s)
 
incDrop(n, b) ==
  n >= #b => ""
  subString(b,n)
 
 
inclFname(s, info) ==
  incFileName incCommandTail(s, info)
 
incBiteOff x ==
  n:=STRPOSL('" ",x,0,true)-- first nonspace
  if null n
  then false -- all spaces
  else
     n1:=STRPOSL ('" ",x,n,nil)
     if null n1 -- all nonspaces
     then [subString(x,n),'""]
     else [subString(x,n,n1-n),subString(x,n1)]
 
incTrunc (n,x)==
  #x > n => subString(x,0,n)
  x
 
incFileName x ==
  first incBiteOff x
 
fileNameStrings fn==
  [PNAME(fn.0),PNAME(fn.1),PNAME(fn.2)]
 
ifCond(s, info) ==
  word := makeSymbol StringTrim(incCommandTail(s, info), WhiteSpaceCset)
  symbolMember?(word,$inclAssertions)
 
assertCond(s, info) ==
  word := makeSymbol StringTrim(incCommandTail(s, info), WhiteSpaceCset)
  if not symbolMember?(word,$inclAssertions) then
    $inclAssertions := [word, :$inclAssertions]
 
 
incActive?(fn,ufos) ==
  member(fn,ufos)
 
incNConsoles ufos==
  a:=member('"console",ufos)
  if a then 1+incNConsoles rest a else 0
 
        --% Message Handling
incHandleMessage(xl) ==
  xl.1.1 = "none" => 0
  xl.1.1 = "error" => inclHandleError(incPos xl.0, xl.1.0)
  xl.1.1 = "warning" => inclHandleWarning(incPos xl.0, xl.1.0)
  xl.1.1 = "say" => inclHandleSay(incPos xl.0, xl.1.0)
  inclHandleBug(incPos xl.0, xl.1.0)
 
xlOK(eb, str, lno, ufo)  ==
  [incLine(eb, str, -1, lno, ufo), [nil, "none"]]
 
xlOK1(eb, str,str1, lno, ufo)  ==
  [incLine1(eb, str,str1, -1, lno, ufo), [nil, "none"]]
 
incLine1(eb, str,str1, gno, lno, ufo) ==
  ln := lnCreate(eb,str,gno,lno,ufo)
  [[ln,:1],:str1]

xlSkip(eb, str, lno, ufo) ==
  str := strconc('"-- Omitting:", str)
  [incLine(eb, str, -1, lno, ufo), [nil, "none"]]
 
xlMsg(eb, str, lno, ufo, mess) ==
  [incLine(eb, str, -1, lno, ufo), mess]
 
xlPrematureEOF(eb, str, lno, ufos) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgPrematureEOF(ufos.0),"error"])
 
xlPrematureFin(eb, str, lno, ufos) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgPrematureFin(ufos.0),"error"])
 
xlFileCycle(eb, str, lno, ufos, fn) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgFileCycle(ufos,fn),"error"])
 
xlNoSuchFile(eb, str, lno, ufos, fn) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgNoSuchFile(fn), "error"])
 
xlCannotRead(eb, str, lno, ufos, fn) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgCannotRead(fn), "error"])
 
xlConsole(eb, str, lno, ufos)  ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgConsole(),"say"])
 
xlConActive(eb, str, lno, ufos, n) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgConActive(n),"warning"])
 
xlConStill(eb, str, lno, ufos, n) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgConStill(n), "say"])
 
xlSkippingFin(eb, str, lno, ufos) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgFinSkipped(),"warning"])
 
xlIfBug(eb, str, lno, ufos) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgIfBug(), "bug"])
 
xlCmdBug(eb, str, lno, ufos) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgCmdBug(), "bug"])
 
xlSay(eb, str, lno, ufos, x) ==
  xlMsg(eb, str, lno,ufos.0, [inclmsgSay(x), "say"])
 
xlIfSyntax(eb, str, lno,ufos,info,sts) ==
  st := sts.0
  found := info.2
  context :=
    Top? st  => "not in an )if...)endif"
    Else? st => "after an )else"
    "but can't figure out where"
  xlMsg(eb, str, lno, ufos.0,
       [inclmsgIfSyntax(ufos.0,found,context), "error"])
 
  --% This is it
 
incLude(eb, ss, ln, ufos, states) ==
  Delay(function incLude1,[eb, ss, ln, ufos, states])
 
macro Rest s ==
  incLude (eb,rest ss,lno,ufos,states)
 
incLude1(eb,ss,ln,ufos,states) ==
  $inputLineNumber := ln
  lno := ln+1
  state := states.0

  StreamNull ss =>
    not Top? state =>
      [xlPrematureEOF(eb, '")--premature end",  lno,ufos), :StreamNil]
    StreamNil

  str  :=  expandLeadingTabs first ss
  info :=  incClassify str

  not info.0 =>
    Skipping? state => [xlSkip(eb,str,lno,ufos.0), :Rest s]
    [xlOK(eb, str, lno, ufos.0),:Rest s]

  info.2 = '"other" =>
    Skipping? state => [xlSkip(eb,str,lno,ufos.0), :Rest s]
    [xlOK1(eb, str,strconc('")command",str), lno, ufos.0), :Rest s]

  info.2 = '"say" =>
    Skipping? state => [xlSkip(eb,str,lno,ufos.0), :Rest s]
    str := incCommandTail(str, info)
    [xlSay(eb, str, lno, ufos, str),
         :[xlOK(eb,str,lno,ufos.0), :Rest s]]

  info.2 = '"include" =>
    Skipping? state => [xlSkip(eb,str,lno,ufos.0), :Rest s]
    fn1 := inclFname(str, info)
    not fn1 => [xlNoSuchFile(eb, str, lno,ufos,fn1),:Rest s]
    not PROBE_-FILE fn1 => [xlCannotRead(eb, str, lno,ufos,fn1),:Rest s]
    incActive?(fn1,ufos) => [xlFileCycle (eb, str, lno,ufos,fn1),:Rest s]
    Includee  :=
      incLude(eb+info.1,incFileInput fn1,0,
                [fn1,:ufos], [Top,:states])
    [xlOK(eb,str,lno,ufos.0), :incAppend(Includee, Rest s)]

  info.2 = '"console" =>
    Skipping? state => [xlSkip(eb,str,lno,ufos.0), :Rest s]
    Head :=
     incLude(eb+info.1,incConsoleInput(),0,
         ['"console",:ufos],[Top,:states])
    Tail := Rest s

    n := incNConsoles ufos
    if n > 0 then
      Head := [xlConActive(eb, str, lno,ufos,n),:Head]
      Tail := [xlConStill (eb, str, lno,ufos,n),:Tail]

    Head := [xlConsole(eb, str, lno,ufos),:Head]
    [xlOK(eb,str,lno,ufos.0),:incAppend(Head,Tail)]

  info.2 = '"fin" =>
    Skipping? state => [xlSkippingFin(eb, str, lno,ufos), :Rest s]
    not Top? state  => [xlPrematureFin(eb, str, lno,ufos), :StreamNil]
    [xlOK(eb,str,lno,ufos.0), :StreamNil]

  info.2 = '"assert" =>
    Skipping? state => [xlSkippingFin(eb, str, lno,ufos), :Rest s]
    assertCond(str, info)
    [xlOK(eb,str,lno,ufos.0), :incAppend(Includee, Rest s)]

  info.2 = '"if" =>
    s1 :=
      Skipping? state => IfSkipToEnd
      if ifCond(str,info) then IfKeepPart else IfSkipPart
    [xlOK(eb,str,lno,ufos.0), :incLude(eb,rest ss,lno,ufos,[s1,:states])]
  info.2 = '"elseif" =>
    not If? state and not Elseif? state =>
      [xlIfSyntax(eb, str,lno,ufos,info,states), :StreamNil]

    SkipEnd? state or KeepPart? state or SkipPart? state =>
      s1 :=
        SkipPart? state =>
          ifCond(str,info) => ElseifKeepPart
          ElseifSkipPart
        ElseifSkipToEnd
      [xlOK(eb,str,lno,ufos.0),
         :incLude(eb,rest ss,lno,ufos,[s1,:rest states])]
    [xlIfBug(eb, str, lno,ufos), :StreamNil]

  info.2 = '"else" =>
    not If? state and not Elseif? state =>
      [xlIfSyntax(eb, str,lno,ufos,info,states),:StreamNil]
    SkipEnd? state or KeepPart? state or SkipPart? state =>
      s1 :=
        SkipPart? state => ElseKeepPart
        ElseSkipToEnd
      [xlOK(eb,str,lno,ufos.0),
        :incLude(eb,rest ss,lno,ufos,[s1,:rest states])]
    [xlIfBug(eb, str, lno,ufos), :StreamNil]

  info.2 = '"endif" =>
    Top? state => [xlIfSyntax(eb, str,lno,ufos,info,states),:StreamNil]
    [xlOK(eb,str,lno,ufos.0), :incLude(eb,rest ss,lno,ufos,rest states)]

  info.2 = '"magicNumber" => Rest s
  [xlCmdBug(eb, str, lno,ufos),:StreamNil]
 
--% Message handling for the source includer
--  SMW June 88
 
inclHandleError(pos, [key, args]) ==
  ncSoftError(pos, key, args)

inclHandleWarning(pos, [key, args]) ==
  ncSoftError(pos, key,args)

inclHandleBug(pos, [key, args]) ==
  ncBug(key, args)

inclHandleSay(pos, [key, args]) ==
  ncSoftError(pos, key, args)
 
inclmsgSay str  ==
  ['S2CI0001, [%id str]]

inclmsgPrematureEOF ufo  ==
  ['S2CI0002, [%origin ufo]]

inclmsgPrematureFin ufo  ==
  ['S2CI0003, [%origin ufo]]

inclmsgFileCycle(ufos,fn) ==
  flist := [porigin n for n in reverse ufos]
  f1    := porigin fn
  cycle := [:[:[n,'"==>"] for n in flist], f1]
  ['S2CI0004, [%id cycle, %id f1]]

inclmsgConsole   () ==
  ['S2CI0005, []]

inclmsgConActive n  ==
  ['S2CI0006, [%id n]]

inclmsgConStill  n  ==
  ['S2CI0007, [%id n]]

inclmsgFinSkipped() ==
  ['S2CI0008, []]

inclmsgIfSyntax(ufo,found,context) ==
  found := strconc('")", found)
  ['S2CI0009, [%id found, %id context, %origin ufo]]

inclmsgNoSuchFile fn ==
  ['S2CI0010, [%fname fn]]

inclmsgCannotRead fn ==
  ['S2CI0011, [%fname fn]]

inclmsgIfBug() ==
  ['S2CB0002, []]

inclmsgCmdBug() ==
  ['S2CB0003, []]
 
