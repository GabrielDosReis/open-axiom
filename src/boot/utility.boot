-- Copyright (C) 2011, Gabriel Dos Reis.
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

import initial_-env
namespace BOOTTRAN
module utility (objectMember?, symbolMember?, stringMember?,
  charMember?, scalarMember?, listMember?, reverse, reverse!,
  lastNode, append!, copyList, substitute, substitute!, setDifference)

--% membership operators

objectMember?(x,l) ==
  repeat
    l = nil => return false
    cons? l =>
      sameObject?(x,first l) => return true
      l := rest l
    return sameObject?(x,l)

symbolMember?(s,l) ==
  repeat
    l = nil => return false
    cons? l =>
      symbolEq?(s,first l) => return true
      l := rest l
    return symbolEq?(s,l)

stringMember?(s,l) ==
  repeat
    l = nil => return false
    cons? l =>
      stringEq?(s,first l) => return true
      l := rest l
    return stringEq?(s,l)

charMember?(c,l) ==
  repeat
    l = nil => return false
    cons? l =>
      charEq?(c,first l) => return true
      l := rest l
    return charEq?(c,l)

scalarMember?(s,l) ==
  repeat
    l = nil => return false
    cons? l =>
      scalarEq?(s,first l) => return true
      l := rest l
    return scalarEq?(s,l)

listMember?(x,l) ==
  repeat
    l = nil => return false
    cons? l =>
      listEq?(x,first l) => return true
      l := rest l
    return listEq?(x,l)

--% list reversal

reverse l ==
  r := nil
  repeat
    cons? l =>
      r := [first l,:r]
      l := rest l
    return r

reverse! l ==
  l1 := nil
  repeat
    cons? l =>
      l2 := rest l
      l.rest := l1
      l1 := l
      l := l2
    return l1
    
--% return a pointer to the last cons-cell in the list `l'.

lastNode l ==
  while l is [.,:l'] and cons? l' repeat
    l := l'
  l

--% list copying
copyList l ==
  not cons? l => l
  l' := t := [first l]
  repeat
    l := rest l
    cons? l =>
      t.rest := [first l]
      t := rest t
    t.rest := l
    return l'

--% append

append!(x,y) ==
  x = nil => y
  y = nil => x
  lastNode(x).rest := y
  x

--% substitution

substitute!(y,x,s) ==
  s = nil => nil
  sameObject?(x,s) => y
  if cons? s then
    s.first := substitute!(y,x,first s)
    s.rest := substitute!(y,x,rest s)
  s

substitute(y,x,s) ==
  s = nil => nil
  sameObject?(x,s) => y
  cons? s =>
    h := substitute(y,x,first s)
    t := substitute(y,x,rest s)
    sameObject?(h,first s) and sameObject?(t,rest s) => s
    [h,:t]
  s
  
--% set operations

setDifference(x,y) ==
  x = nil => nil
  y = nil => x
  l := p := [nil]
  for [a,:.] in tails x | not objectMember?(a,y) repeat
    p.rest := [a]
    p := rest p
  rest l
