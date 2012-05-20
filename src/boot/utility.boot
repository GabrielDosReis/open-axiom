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
  symbolAssoc, applySubst, applySubst!, applySubstNQ, objectAssoc,
  remove, removeSymbol, atomic?, every?, any?, take, takeWhile, drop,
  copyTree, finishLine, stringSuffix?) where
    substitute: (%Thing,%Thing,%Thing) -> %Thing
    substitute!: (%Thing,%Thing,%Thing) -> %Thing
    append: (%List %Thing,%List %Thing) -> %List %Thing
    append!: (%List %Thing,%List %Thing) -> %List %Thing
    copyList: %List %Thing -> %List %Thing
    lastNode: %List %Thing -> %Maybe %Node %Thing
    removeSymbol: (%List %Thing, %Symbol) -> %List %Thing
    remove: (%List %Thing, %Thing) -> %List %Thing
    objectAssoc: (%Thing, %List %Thing) -> %Maybe %Pair(%Thing,%Thing)
    symbolAssoc: (%Symbol,%List %Thing) -> %Maybe %Pair(%Symbol,%Thing)
    setDifference: (%List %Thing,%List %Thing) -> %List %Thing
    setUnion: (%List %Thing,%List %Thing) -> %List %Thing
    setIntersection: (%List %Thing,%List %Thing) -> %List %Thing
    atomic?: %Thing -> %Boolean
    every?: (%Thing -> %Thing, %List %Thing) -> %Thing
    any?: (%Thing -> %Thing, %List %Thing) -> %Thing
    take: (%Short,%List %Thing) -> %List %Thing
    takeWhile: (%Thing -> %Thing, %List %Thing) -> %List %Thing
    drop: (%Short,%List %Thing) -> %List %Thing
    copyTree: %Thing -> %Thing
    finishLine: %Thing -> %Void
    --FIXME: Next signature commented out because of GCL bugs
    -- firstNonblankPosition: (%String,%Short) -> %Maybe %Short
    firstBlankPosition: (%String,%Short) -> %Maybe %Short
    stringSuffix?: (%String,%String) -> %Maybe %Short

%defaultReadAndLoadSettings()

--%

++ Return true if `x' is an atom of a quotation.
atomic? x ==
  x isnt [.,:.] or x.op is 'QUOTE

++ Return the last image of `f' if all images of elements in `l'
++ are non-nil.  Otherwise return nil.
every?(f,l) ==
  and/[apply(f,x,nil) for x in l]

++ Return the first non-nil image of `f' of elements in `l'.
any?(f,l) ==
  or/[apply(f,x,nil) for x in l]

++ Return the `n' node prefixes of the list `l'.  If `n' is negative,
++ take from the end of the list.
take(n,l) ==
  n >= 0 => [x for x in l for . in 1..n]
  drop(#l+n,l)

++ Return the sublist of `l' whose elements have non-nil image by `f'.
takeWhile(f,l) ==
  [x for x in l while apply(f,x,nil)]

++ Return the `n+1'th node and its successors of the list `l'.
++ If `n' is negative, drop from the end.
drop(n,l) ==
  n >= 0 =>
    while n > 0 and l is [.,:l] repeat
      n := n - 1
    l
  take(#l+n,l)

copyTree t ==
  t is [.,:.] => [copyTree first t,:copyTree rest t]
  t

--% membership operators

objectMember?(x,l) ==
  repeat
    l = nil => return false
    l is [.,:.] =>
      sameObject?(x,first l) => return true
      l := rest l
    return sameObject?(x,l)

symbolMember?(s,l) ==
  repeat
    l = nil => return false
    l is [.,:.] =>
      symbolEq?(s,first l) => return true
      l := rest l
    return symbolEq?(s,l)

stringMember?(s,l) ==
  repeat
    l = nil => return false
    l is [.,:.] =>
      stringEq?(s,first l) => return true
      l := rest l
    return stringEq?(s,l)

charMember?(c,l) ==
  repeat
    l = nil => return false
    l is [.,:.] =>
      charEq?(c,first l) => return true
      l := rest l
    return charEq?(c,l)

scalarMember?(s,l) ==
  repeat
    l = nil => return false
    l is [.,:.] =>
      scalarEq?(s,first l) => return true
      l := rest l
    return scalarEq?(s,l)

listMember?(x,l) ==
  repeat
    l = nil => return false
    l is [.,:.] =>
      listEq?(x,first l) => return true
      l := rest l
    return listEq?(x,l)

--% list reversal

reverse l ==
  r := nil
  repeat
    l is [.,:.] =>
      r := [first l,:r]
      l := rest l
    return r

reverse! l ==
  l1 := nil
  repeat
    l is [.,:.] =>
      l2 := rest l
      l.rest := l1
      l1 := l
      l := l2
    return l1
    
--% return a pointer to the last cons-cell in the list `l'.

lastNode l ==
  while l is [.,:l'] and l' is [.,:.] repeat
    l := l'
  l

--% list copying
copyList l ==
  l isnt [.,:.] => l
  l' := t := [first l]
  repeat
    l := rest l
    l is [.,:.] =>
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

symbolAssoc(s,l) ==
  repeat
    l isnt [x,:l] => return nil
    x is [.,:.] and symbolEq?(s,first x) => return x

objectAssoc(x,l) ==
  repeat
    l isnt [p,:l] => return nil
    p is [.,:.] and sameObject?(first p,x) => return p

--% substitution

substitute!(y,x,s) ==
  s = nil => nil
  sameObject?(x,s) => y
  if s is [.,:.] then
    s.first := substitute!(y,x,first s)
    s.rest := substitute!(y,x,rest s)
  s

substitute(y,x,s) ==
  s = nil => nil
  sameObject?(x,s) => y
  s is [.,:.] =>
    h := substitute(y,x,first s)
    t := substitute(y,x,rest s)
    sameObject?(h,first s) and sameObject?(t,rest s) => s
    [h,:t]
  s
  
applySubst(sl,t) ==
  sl = nil => t
  t is [.,:.] =>
    hd := applySubst(sl,first t)
    tl := applySubst(sl,rest t)
    sameObject?(hd,first t) and sameObject?(tl,rest t) => t
    [hd,:tl]
  symbol? t and (p := symbolAssoc(t,sl)) => rest p
  t

applySubst!(sl,t) ==
  sl = nil => t
  t is [.,:.] =>
    hd := applySubst!(sl,first t)
    tl := applySubst!(sl,rest t)
    t.first := hd
    t.rest := tl
  symbol? t and (p := symbolAssoc(t,sl)) => rest p
  t

++ Like applySubst, but skip quoted materials.
applySubstNQ(sl,t) ==
  sl = nil => t
  t is [hd,:tl] =>
    hd is 'QUOTE => t
    hd := applySubstNQ(sl,hd)
    tl := applySubstNQ(sl,tl)
    sameObject?(hd,first t) and sameObject?(tl,rest t) => t
    [hd,:tl]
  symbol? t and (p := symbolAssoc(t,sl)) => rest p
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
    l' isnt [.,:.] => return l
    [y,:l'] := l'
    symbolEq?(x,y) => return append!(reverse! before,l')
    before := [y,:before]

removeScalar(l,x) ==
  before := nil
  l' := l
  repeat
    l' isnt [.,:.] => return l
    [y,:l'] := l'
    scalarEq?(x,y) => return append!(reverse! before,l')
    before := [y,:before]

removeValue(l,x) ==
  before := nil
  l' := l
  repeat
    l' isnt [.,:.] => return l
    [y,:l'] := l'
    valueEq?(x,y) => return append!(reverse! before,l')
    before := [y,:before]

remove(l,x) ==
  symbol? x => removeSymbol(l,x)
  char? x or integer? x => removeScalar(l,x)
  removeValue(l,x)

--% search

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
    
++ If `suf' is a suffix of `str' return the index into `str' at which the
++ match occurs.  Otherwise return nil.
stringSuffix?(suf,str) ==
  n1 := #suf
  n2 := #str
  n1 > n2 => nil
  n := n2 - n1
  and/[stringChar(suf,i) = stringChar(str,j) for i in 0..n1-1 for j in n..] => n
  nil

--% I/O

++ Add a newline character and flush the output stream.
finishLine out ==
  writeNewline out
  flushOutput out
