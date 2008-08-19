-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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
--

--
-- Abstract:
--   This file defines the includer (or preprocessor) of Boot programs.
--

import tokens
namespace BOOTTRAN
module includer

-- BOOT INCLUDER
 
-- Line syntax is
--
--  Include ::= (SimpleLine | If )*  | ( )fin | empty)
--
--  SimpleLine::=
--        PlainLine |            includes the line
--        )say line |            outputs line to console
--        )eval line |           evaluates the boot line
--                                 nothing included
--        )line line |           line is reproduced as is in lisp output
--        )lisp line |           line is read by lisp READ
--        )package line |        produces (IN-PACKAGE line) in lisp
--                                     output
--        )include filename |    includes the file as boot code
--        )includelisp filename |  includes the file as lisp code
--                                   read by lisp READ
--        )includelines  filename |  includes the file as is
--                                     in lisp output
--
-- If ::= )if SimpleLine* ElseLines )endif
--
-- ElseLines ::= )else SimpleLine* | )elseif SimpleLine* ElseLines | empty

-- returns a printable representation of X, when it is a symbol
-- or a character, as string.  Otherwise, returns nil.
PNAME x ==
  SYMBOLP x => SYMBOL_-NAME x
  CHARACTERP x => STRING x
  nil

-- converts X, a 1-length string, to a character.
char x ==
  CHAR(PNAME x, 0)

EQCAR(x,y)== 
  CONSP x and EQ(first x,y)

-- returns the string representation of object X.
STRINGIMAGE x ==
  WRITE_-TO_-STRING x

-- close STREAM.
shoeCLOSE stream ==
  CLOSE stream

-- error out if file is not found. 
shoeNotFound fn == 
  coreError [fn, '" not found"]
  nil

 
shoeReadLispString(s,n) ==
    l:=# s
    n >= l => nil
    READ_-FROM_-STRING strconc ( "(", SUBSTRING(s,n,l-n) ,")")

-- read a line from stream
shoeReadLine stream ==
  READ_-LINE(stream, nil, nil)

-- write LINE to standard terminal I/O.
shoeConsole line ==
  WRITE_-LINE(line, _*TERMINAL_-IO_*)
 
shoeSpaces n  ==  
  MAKE_-FULL_-CVEC(n, '".")
 

--%

diagnosticLocation tok ==
  pos := shoeTokPosn tok
  strconc('"line ", STRINGIMAGE lineNo pos, '", column ", 
    STRINGIMAGE lineCharacter pos)

SoftShoeError(posn,key)==
    coreError ['"in line ", STRINGIMAGE lineNo posn]
    shoeConsole lineString posn
    shoeConsole strconc(shoeSpaces lineCharacter posn,'"|")
    shoeConsole key
 
bpSpecificErrorAtToken(tok, key) ==
     a:=shoeTokPosn tok
     SoftShoeError(a,key)
 
bpSpecificErrorHere(key) ==  bpSpecificErrorAtToken($stok, key)

bpGeneralErrorHere() ==  bpSpecificErrorHere('"syntax error")
 
bpIgnoredFromTo(pos1, pos2) ==
    shoeConsole strconc('"ignored from line ", STRINGIMAGE lineNo pos1)
    shoeConsole lineString pos1
    shoeConsole strconc(shoeSpaces lineCharacter pos1,'"|")
    shoeConsole strconc('"ignored through line ", STRINGIMAGE lineNo pos2)
    shoeConsole lineString pos2
    shoeConsole strconc(shoeSpaces lineCharacter pos2,'"|")

-- Line inclusion support.
 
lineNo p ==
  CDAAR p

lineString p == 
  CAAAR p

lineCharacter p == 
  rest p
 
shoePackageStartsAt (lines,sz,name,stream)==
   bStreamNull stream => [[],['nullstream]]
   a:=CAAR stream
   if #a >= 8 and SUBSTRING(a,0,8)='")package"
   then shoePackageStartsAt(cons(CAAR stream,lines),sz,name,rest stream)
   else
     if #a<sz
     then shoePackageStartsAt(lines, sz,name,rest stream)
     else if SUBSTRING(a,0,sz)=name and (#a>sz and not shoeIdChar(a.sz))
          then [lines,stream]
          else shoePackageStartsAt(lines,sz,name,rest stream)
 
shoeFindLines(fn,name,a)==
   if null a
   then
      shoeNotFound fn
      []
   else
      [lines,b]:=shoePackageStartsAt([],#name,name, shoeInclude
                        bAddLineNumber(bRgen a,bIgen 0))
      b:=shoeTransform2 b
      if bStreamNull b
      then
           shoeConsole strconc (name,'" not found in ",fn)
           []
      else
         if null lines
         then shoeConsole '")package not found"
         append(reverse lines,first b)

-- Lazy inclusion support.

$bStreamNil:=["nullstream"]
 
bStreamNull x==
  null x or EQCAR (x,"nullstream") => true
  while EQCAR(x,"nonnullstream") repeat
          st:=apply(second x,CDDR x)
          RPLACA(x,first st)
          RPLACD(x,rest st)
  EQCAR(x,"nullstream")
 
bMap(f,x) == 
  bDelay(function bMap1, [f,x])
 
bMap1(:z)==
     [f,x]:=z
     if bStreamNull x
     then $bStreamNil
     else cons(FUNCALL(f,first x),bMap(f,rest x))

shoeFileMap(f, fn)==
     a:=shoeInputFile fn
     null a =>
        shoeConsole strconc(fn,'" NOT FOUND")
        $bStreamNil
     shoeConsole strconc('"READING ",fn)
     shoeInclude  bAddLineNumber(bMap(f,bRgen a),bIgen 0)

 
bDelay(f,x) ==
  cons("nonnullstream",[f,:x])
 
bAppend(x,y) ==
  bDelay(function bAppend1,[x,y])
 
bAppend1(:z)==
     if bStreamNull first z
     then if bStreamNull second z
          then ["nullstream"]
          else second z
     else cons(CAAR z,bAppend(CDAR z,second z))
 
bNext(f,s) ==
  bDelay(function bNext1,[f,s])
 
bNext1(f,s)==
      bStreamNull s=> ["nullstream"]
      h:= apply(f, [s])
      bAppend(first h,bNext(f,rest h))
 
bRgen s ==
  bDelay(function bRgen1,[s])
 
bRgen1(:s) ==
        a:=shoeReadLine first s
        if shoePLACEP a
        then
--          shoeCLOSE first s
            ["nullstream"]
        else cons(a,bRgen first s)
 
bIgen n ==
  bDelay(function bIgen1,[n])
 
bIgen1(:n)==
        n:=first n+1
        cons(n,bIgen n)
 
bAddLineNumber(f1,f2) ==
  bDelay(function bAddLineNumber1,[f1,f2])
 
bAddLineNumber1(:f)==
     [f1,f2] := f
     bStreamNull f1 =>  ["nullstream"]
     bStreamNull f2 =>  ["nullstream"]
     cons(cons(first f1,first f2),bAddLineNumber(rest f1,rest f2))


 
shoeFileInput fn ==
  shoeFileMap(function IDENTITY,fn)
 
shoePrefixLisp x == 
  strconc('")lisp",x)

shoeLispFileInput fn==
  shoeFileMap(function shoePrefixLisp,fn)
 
shoePrefixLine x== 
  strconc('")line",x)

shoeLineFileInput fn== 
  shoeFileMap(function shoePrefixLine,fn)
 
shoePrefix?(prefix,whole) ==
     #prefix > #whole => false
     good:=true
     for i in 0..#prefix-1 for j in 0.. while good repeat
                good:= prefix.i = whole.j
     if good then SUBSTRING(whole,#prefix,nil) else good
 
shoePlainLine?(s) ==
         #s = 0 =>  true
         s.0 ^= char ")"
 
shoeSay?          s  == shoePrefix?('")say",         s)
shoeEval?         s  == shoePrefix?('")eval",        s)
shoeInclude?      s  == shoePrefix?('")include",     s)
shoeFin?          s  == shoePrefix?('")fin",         s)
shoeIf?           s  == shoePrefix?('")if",          s)
shoeEndIf?        s  == shoePrefix?('")endif",       s)
shoeElse?         s  == shoePrefix?('")else",        s)
shoeElseIf?       s  == shoePrefix?('")elseif",      s)
shoePackage?      s  == shoePrefix?('")package",     s)
shoeLisp?         s  == shoePrefix?('")lisp",        s)
shoeIncludeLisp?  s  == shoePrefix?('")includelisp" ,s)
shoeLine?         s  == shoePrefix?('")line",        s)
shoeIncludeLines? s  == shoePrefix?('")includelines",s)
shoeIncludeFunction? s  == shoePrefix?('")includefunction",s)
 
shoeBiteOff x==
         n:=STRPOSL('" ",x,0,true)
         null n =>  false
         n1:=STRPOSL ('" ",x,n,nil)
         null n1 =>  [SUBSTRING(x,n,nil),'""]
         [SUBSTRING(x,n,n1-n),SUBSTRING(x,n1,nil)]
 
shoeFileName x==
         a:=shoeBiteOff x
         null a =>  '""
         c:=shoeBiteOff second a
         null c =>  first a
         strconc(first a,'".",first c)
 
shoeFnFileName x==
         a:=shoeBiteOff x
         null a =>  ['"",'""]
         c:=shoeFileName second a
         null c =>  [first a,'""]
         [first a, c]
 
shoeFunctionFileInput [fun,fn]==
    shoeOpenInputFile (a,fn,
     shoeInclude bAddLineNumber( shoeFindLines(fn,fun,a),bIgen 0))
 
shoeInclude s == 
  bDelay(function shoeInclude1,[s])

shoeInclude1 s==
      bStreamNull s=> s
      [h,:t]  :=s
      string  :=first h
      command :=shoeFin? string  => $bStreamNil
      command :=shoeIf? string   => shoeThen([true],[STTOMC command],t)
      bAppend(shoeSimpleLine h,shoeInclude t)
 
shoeSimpleLine(h) ==
      string  :=first h
      shoePlainLine? string=> [h]
      command:=shoeLisp? string => [h]
      command:=shoeIncludeLisp? string =>
                  shoeLispFileInput shoeFileName command
      command:=shoeIncludeFunction? string =>
                  shoeFunctionFileInput shoeFnFileName command
      command:=shoeLine? string => [h]
      command:=shoeIncludeLines? string =>
                  shoeLineFileInput shoeFileName command
      command:=shoeInclude? string => shoeFileInput shoeFileName command
      command:=shoePackage? string => [h]
      command:=shoeSay? string =>
                shoeConsole command
                nil
      command:=shoeEval? string =>
                STTOMC command
                nil
      shoeLineSyntaxError(h)
      nil
 
shoeThen(keep,b,s) == 
  bDelay(function shoeThen1,[keep,b,s])

shoeThen1(keep,b,s)==
    bPremStreamNull s=> s
    [h,:t]  :=s
    string  :=first h
    command :=shoeFin? string  => bPremStreamNil(h)
    keep1:= first keep
    b1   := first b
    command :=shoeIf? string  =>
      keep1 and b1=>  shoeThen(cons(true,keep),cons(STTOMC command,b),t)
      shoeThen(cons(false,keep),cons(false,b),t)
    command :=shoeElseIf? string=>
      keep1 and not b1=>
          shoeThen(cons(true,rest keep),cons(STTOMC command,rest b),t)
      shoeThen(cons(false,rest keep),cons(false,rest b),t)
    command :=shoeElse? string =>
     keep1 and not b1=>shoeElse(cons(true,rest keep),cons(true,rest b),t)
     shoeElse(cons(false,rest keep),cons(false,rest b),t)
    command :=shoeEndIf? string=>
         null rest b=>  shoeInclude t
         shoeThen(rest keep,rest b,t)
    keep1 and b1 => bAppend(shoeSimpleLine h,shoeThen(keep,b,t))
    shoeThen(keep,b,t)
 
shoeElse(keep,b,s) ==
  bDelay(function shoeElse1,[keep,b,s])

shoeElse1(keep,b,s)==
    bPremStreamNull s=> s
    [h,:t]  :=s
    string  :=first h
    command :=shoeFin? string => bPremStreamNil(h)
    b1:=first b
    keep1:=first keep
    command :=shoeIf? string=>
      keep1 and b1=> shoeThen(cons(true,keep),cons(STTOMC command,b),t)
      shoeThen(cons(false,keep),cons(false,b),t)
    command :=shoeEndIf? string =>
         null rest b=>  shoeInclude t
         shoeThen(rest keep,rest b,t)
    keep1 and b1 => bAppend(shoeSimpleLine h,shoeElse(keep,b,t))
    shoeElse(keep,b,t)
 
shoeLineSyntaxError(h)==
     shoeConsole strconc('"INCLUSION SYNTAX ERROR IN LINE ",
                                STRINGIMAGE rest h)
     shoeConsole first h
     shoeConsole '"LINE IGNORED"
 
bPremStreamNil(h)==
       shoeConsole strconc('"UNEXPECTED )fin IN LINE ",STRINGIMAGE rest h)
       shoeConsole first h
       shoeConsole '"REST OF FILE IGNORED"
       $bStreamNil
 
bPremStreamNull(s)==
     if bStreamNull s
     then
        shoeConsole '"FILE TERMINATED BEFORE )endif"
        true
     else false
