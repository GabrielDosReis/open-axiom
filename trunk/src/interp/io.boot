-- Copyright (C) 2012, Gabriel Dos Reis.
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

import sys_-constants

namespace BOOT

module io where
  blankChar? : %Char -> %Boolean
  firstNonblankCharPosition: %String -> %Maybe %Short
  trimTrailingBlank: %String -> %String

--%
--%  Individual character routines
--%

++ Return true if character `c' is a space character.
macro spaceChar? c ==
  c = char " "

++ Return true if character `c' is a horizontal tab character
macro tabChar? c ==
  c = abstractChar 9
  

++ Return true if character `c' is either a space or a horitonal tab.
blankChar? c ==
  spaceChar? c or tabChar? c

carriageRetChar? c ==
  c = abstractChar 13


--%
--% String manipulation routines.
--%

++ Replace all characters in `s' with space characters.
storeBlank!(s,n) ==
  for i in 0..maxIndex s repeat
    s.i := char " "
  s

++ Return the position of the first nonblank character in line, if any.
firstNonblankCharPosition line ==
  or/[i for i in 0..maxIndex line | not blankChar? line.i]

trimTrailingBlank line ==
  n := sz := #line
  for i in (sz-1)..0 by -1 while blankChar? line.i repeat
    n := n - 1
  n = sz => line
  subString(line,0,n)

trimCarriageReturn line ==
  carriageRetChar? line.maxIndex(line) =>
    subString(line,0,maxIndex line)
  line

expandLeadingTabs line ==
  not string? line or #line = 0 => line
  line := trimCarriageReturn line
  nbLoc := firstNonblankCharPosition line
  indLoc := indentationLocation line
  nbLoc = indLoc => line
  strconc(makeString(indLoc,char " "), subString(line,nbLoc))

findChar(c,s,k == 0) ==
  or/[i for i in k..maxIndex s | stringChar(s,i) = c]

findString(s1,s2,k == 0) ==
  n1 := #s1
  or/[i for i in k..(#s2 - n1) |
        and/[stringChar(s1,j) = stringChar(s2,i+j) for j in 0..(n1-1)]]
