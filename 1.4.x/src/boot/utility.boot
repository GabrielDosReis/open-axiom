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

--%
--% Definitions in this file provide runtime support for the Boot
--% language.  As such, some of the definitions (e.g. reverse, append, etc)
--% use `unusual' style.  These functions are used in code generated
--% by the Boot translator.  Others are handy library functions.
--%

namespace BOOTTRAN ==
  import namespace System
  import namespace AxiomCore

namespace BOOTTRAN

module utility (objectMember?, symbolMember?, stringMember?,
  charMember?, scalarMember?, listMember?, reverse, reverse!,
  lastNode, append, append!, copyList, substitute, substitute!,
  setDifference, setUnion, setIntersection,
  applySubst, applySubst!, applySubstNQ, objectAssoc,
  remove,removeSymbol,atomic?,finishLine) where
    substitute: (%Thing,%Thing,%Thing) -> %Thing
    substitute!: (%Thing,%Thing,%Thing) -> %Thing
    append: (%List %Thing,%List %Thing) -> %List %Thing
    append!: (%List %Thing,%List %Thing) -> %List %Thing
    copyList: %List %Thing -> %List %Thing
    lastNode: %List %Thing -> %Maybe %Node %Thing
    removeSymbol: (%List %Thing, %Symbol) -> %List %Thing
    remove: (%List %Thing, %Thing) -> %List %Thing
    objectAssoc: (%Thing, %List %Pair(%Thing,%Thing)) ->
                    %Maybe %Pair(%Thing,%Thing)
    setDifference: (%List %Thing,%List %Thing) -> %List %Thing
    setUnion: (%List %Thing,%List %Thing) -> %List %Thing
    setIntersection: (%List %Thing,%List %Thing) -> %List %Thing
    atomic?: %Thing -> %Boolean
    finishLine: %Thing -> %Void
    firstNonblankPosition: (%String,%Short) -> %Maybe %Short
    firstBlankPosition: (%String,%Short) -> %Maybe %Short

%defaultReadAndLoadSettings()

--%

++ Return true if `x' is an atom of a quotation.
atomic? x ==
  x isnt [.,:.] or x.op is 'QUOTE

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

append(x,y) ==
  append!(copyList x,y)

--% a-list

assocSymbol(s,al) ==
  repeat
    al is [x,:al] =>
      cons? x and symbolEq?(s,first x) =>
        return x
    return nil

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
  
applySubst(sl,t) ==
  cons? t =>
    hd := applySubst(sl,first t)
    tl := applySubst(sl,rest t)
    sameObject?(hd,first t) and sameObject?(tl,rest t) => t
    [hd,:tl]
  symbol? t and (p := assocSymbol(t,sl)) => rest p
  t

applySubst!(sl,t) ==
  cons? t =>
    hd := applySubst!(sl,first t)
    tl := applySubst!(sl,rest t)
    t.first := hd
    t.rest := tl
  symbol? t and (p := assocSymbol(t,sl)) => rest p
  t

++ Like applySubst, but skip quoted materials.
applySubstNQ(sl,t) ==
  t is [hd,:tl] =>
    hd is 'QUOTE => t
    hd := applySubstNQ(sl,hd)
    tl := applySubstNQ(sl,tl)
    sameObject?(hd,first t) and sameObject?(tl,rest t) => t
    [hd,:tl]
  symbol? t and (p := assocSymbol(t,sl)) => rest p
  t

--% set operations

setDifference(x,y) ==
  x = nil => nil
  y = nil => x
  l := p := [nil]
  for [a,:.] in tails x | not objectMember?(a,y) repeat
    p.rest := [a]
    p := rest p
  rest l

++ Return the union of two lists of objects, with no duplicates.  
setUnion(x,y) ==
  z := nil
  for a in x | not objectMember?(a,z) repeat
    z := [a,:z]
  for a in y | not objectMember?(a,z) repeat
    z := [a,:z]
  reverse! z

++ Return the intersection of two lists of objects, with no duplicates.  
setIntersection(x,y) ==
  [a for a in x | objectMember?(a,y)]

--% removal

removeSymbol(l,x) ==
  before := nil
  l' := l
  repeat
    not cons? l' => return l
    [y,:l'] := l'
    symbolEq?(x,y) => return append!(reverse! before,l')
    before := [y,:before]

removeScalar(l,x) ==
  before := nil
  l' := l
  repeat
    not cons? l' => return l
    [y,:l'] := l'
    scalarEq?(x,y) => return append!(reverse! before,l')
    before := [y,:before]

removeValue(l,x) ==
  before := nil
  l' := l
  repeat
    not cons? l' => return l
    [y,:l'] := l'
    valueEq?(x,y) => return append!(reverse! before,l')
    before := [y,:before]

remove(l,x) ==
  symbol? x => removeSymbol(l,x)
  char? x or integer? x => removeScalar(l,x)
  removeValue(l,x)

--% search

objectAssoc(x,l) ==
  repeat
    l isnt [p,:l] => return nil
    p is [a,:.] and sameObject?(a,x) => return p

++ Return the index of the character `c' in the string `s', if present.
++ Otherwise, return nil.
charPosition(c,s,k) ==
  n := # s
  repeat
    k >= n => return nil
    stringChar(s,k) = c => return k
    k := k + 1

firstNonblankPosition(s,k) ==
  or/[i for i in k..#s - 1 | stringChar(s,i) ~= char " "]

firstBlankPosition(s,k) ==
  or/[i for i in k..#s - 1 | stringChar(s,i) = char " "]
    

--% I/O

++ Add a newline character and flush the output stream.
finishLine out ==
  writeNewline out
  flushOutput out
