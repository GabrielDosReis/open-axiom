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
--        )lisp line             line is read by lisp READ
--
-- If ::= )if SimpleLine* ElseLines )endif
--
-- ElseLines ::= )else SimpleLine* | )elseif SimpleLine* ElseLines | empty

++ returns a printable representation of `x', when it is a symbol
++ or a character, as string.  Otherwise, returns nil.
PNAME x ==
  symbol? x => symbolName x
  char? x => charString x
  nil

-- error out if file is not found. 
shoeNotFound fn == 
  coreError [fn, '" not found"]
  nil

 
shoeReadLispString(s,n) ==
  l := #s
  n >= l => nil
  readLispFromString strconc('"(", subString(s,n,l-n) ,'")")

-- write LINE to standard terminal I/O.
shoeConsole line ==
  writeLine(line,$stdio)
 
shoeSpaces n  ==  
  makeString(n,char ".")
 

--%

diagnosticLocation tok ==
  pos := tokenPosition tok
  strconc('"line ", toString lineNo pos, '", column ", 
    toString lineCharacter pos)

SoftShoeError(posn,key)==
  coreError ['"in line ", toString lineNo posn]
  shoeConsole lineString posn
  shoeConsole strconc(shoeSpaces lineCharacter posn,'"|")
  shoeConsole key
 
--%
structure %SourceLine ==
  Record(str: %String, num: %Short) with
    sourceLineString == (.str)
    sourceLineNumber == (.num)

macro makeSourceLine(s,n) ==
  mk%SourceLine(s,n)

-- Line inclusion support.
 
lineNo p ==
  sourceLineNumber CAAR p

lineString p == 
  sourceLineString CAAR p

lineCharacter p == 
  rest p
 
-- Lazy inclusion support.

$bStreamNil == ["nullstream"]
 
bStreamNull x ==
  x = nil or x is ["nullstream",:.] => true
  while x is ["nonnullstream",op,:args] repeat
    st := apply(op,args)
    x.first := first st
    x.rest := rest st
  x is ["nullstream",:.]
 
bMap(f,x) == 
  bDelay(function bMap1, [f,x])
 
bMap1(f,x)==
  bStreamNull x => $bStreamNil
  [apply(f,[first x]),:bMap(f,rest x)]

bDelay(f,x) ==
  ["nonnullstream",f,:x]
 
bAppend(x,y) ==
  bDelay(function bAppend1,[x,y])
 
bAppend1(x,y)==
  bStreamNull x =>
    bStreamNull y => ["nullstream"]
    y
  [first x,:bAppend(rest x,y)]
 
bNext(f,s) ==
  bDelay(function bNext1,[f,s])
 
bNext1(f,s)==
  bStreamNull s => ["nullstream"]
  h := apply(f,[s])
  bAppend(first h,bNext(f,rest h))
 
bRgen s ==
  bDelay(function bRgen1,[s])
 
bRgen1 s ==
  a := readLine s
  a ~= %nothing => [a,:bRgen s]
  ["nullstream"]
 
bIgen n ==
  bDelay(function bIgen1,[n])
 
bIgen1 n ==
  n := n + 1
  [n,:bIgen n]
 
bAddLineNumber(f1,f2) ==
  bDelay(function bAddLineNumber1,[f1,f2])
 
bAddLineNumber1(f1,f2)==
  bStreamNull f1 =>  ["nullstream"]
  bStreamNull f2 =>  ["nullstream"]
  [makeSourceLine(first f1,first f2),:bAddLineNumber(rest f1,rest f2)]


shoePrefixLisp x == 
  strconc('")lisp",x)

shoePrefixLine x== 
  strconc('")line",x)

shoePrefix?(prefix,whole) ==
  #prefix > #whole => false
  good:=true
  for i in 0..#prefix-1 for j in 0.. while good repeat
    good := stringChar(prefix,i) = stringChar(whole,j)
  good => subString(whole,#prefix) 
  good
 
shoePlainLine?(s) ==
  #s = 0 =>  true
  stringChar(s,0) ~= char ")"
 
shoeSay?          s  == shoePrefix?('")say",         s)
shoeEval?         s  == shoePrefix?('")eval",        s)
shoeFin?          s  == shoePrefix?('")fin",         s)
shoeIf?           s  == shoePrefix?('")if",          s)
shoeEndIf?        s  == shoePrefix?('")endif",       s)
shoeElse?         s  == shoePrefix?('")else",        s)
shoeElseIf?       s  == shoePrefix?('")elseif",      s)
shoeLisp?         s  == shoePrefix?('")lisp",        s)
shoeLine?         s  == shoePrefix?('")line",        s)
 
shoeInclude s == 
  bDelay(function shoeInclude1,[s])

shoeInclude1 s ==
  bStreamNull s => s
  [h,:t]  := s
  string  := sourceLineString h
  command := shoeFin? string  => $bStreamNil
  command := shoeIf? string   => shoeThen([true],[STTOMC command],t)
  bAppend(shoeSimpleLine h,shoeInclude t)
 
shoeSimpleLine(h) ==
  string := sourceLineString h
  shoePlainLine? string=> [h]
  command := shoeLisp? string => [h]
  command := shoeLine? string => [h]
  command := shoeSay? string =>
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
  [h,:t] := s
  string := sourceLineString h
  command := shoeFin? string  => bPremStreamNil(h)
  keep1 := first keep
  b1 := first b
  command := shoeIf? string  =>
    keep1 and b1 =>  shoeThen([true,:keep],[STTOMC command,:b],t)
    shoeThen([false,:keep],[false,:b],t)
  command := shoeElseIf? string =>
    keep1 and not b1 =>
      shoeThen([true,:rest keep],[STTOMC command,:rest b],t)
    shoeThen([false,:rest keep],[false,:rest b],t)
  command :=shoeElse? string =>
   keep1 and not b1=>shoeElse([true,:rest keep],[true,:rest b],t)
   shoeElse([false,:rest keep],[false,:rest b],t)
  command :=shoeEndIf? string=>
    rest b = nil =>  shoeInclude t
    shoeThen(rest keep,rest b,t)
  keep1 and b1 => bAppend(shoeSimpleLine h,shoeThen(keep,b,t))
  shoeThen(keep,b,t)
 
shoeElse(keep,b,s) ==
  bDelay(function shoeElse1,[keep,b,s])

shoeElse1(keep,b,s)==
  bPremStreamNull s=> s
  [h,:t] := s
  string := sourceLineString h
  command := shoeFin? string => bPremStreamNil(h)
  b1 := first b
  keep1 := first keep
  command := shoeIf? string =>
    keep1 and b1 => shoeThen([true,:keep],[STTOMC command,:b],t)
    shoeThen([false,:keep],[false,:b],t)
  command := shoeEndIf? string =>
    rest b = nil =>  shoeInclude t
    shoeThen(rest keep,rest b,t)
  keep1 and b1 => bAppend(shoeSimpleLine h,shoeElse(keep,b,t))
  shoeElse(keep,b,t)
 
shoeLineSyntaxError(h)==
  shoeConsole strconc('"INCLUSION SYNTAX ERROR IN LINE ",
			     toString sourceLineNumber h)
  shoeConsole sourceLineString h
  shoeConsole '"LINE IGNORED"
 
bPremStreamNil(h)==
  shoeConsole strconc('"UNEXPECTED )fin IN LINE ",toString sourceLineNumber h)
  shoeConsole sourceLineString h
  shoeConsole '"REST OF FILE IGNORED"
  $bStreamNil
 
bPremStreamNull(s)==
  bStreamNull s =>
    shoeConsole '"FILE TERMINATED BEFORE )endif"
    true
  false
