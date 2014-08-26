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

import sys_-macros
namespace BOOT

simpBool x == dnf2pf reduceDnf be x
 
reduceDnf u ==
-- (OR (AND ..b..) b) ==> (OR  b  )
  u isnt [.,:.] => u
  for x in u repeat
    ok := true
    for y in u repeat
      x = y => 'skip
      dnfContains(x,y) => return (ok := false)
    ok = true => acc := [x,:acc]
  reverse! acc
 
dnfContains([a,b],[c,d]) == fn(a,c) and fn(b,d) where
  fn(x,y) == and/[member(u,x) for u in y]
 
prove x ==
  world := [p for y in listOfUserIds x | (p := getPredicate y)] =>
    'false = be mkpf([['NOT,x],:world],'AND) => true
    'false = be mkpf([x,:world],'AND) => false
    x
  'false = (y := be x) => 'false
  y = 'true => true
  dnf2pf y

simpBoolGiven(x,world) ==
  world =>
    'false = be mkpf([['NOT,x],:world],'AND) => true
    'false = (y := be mkpf([x,:world],'AND)) => false
    (u := andReduce(dnf2pf y,world)) is ['AND,:v] and
      (w := SETDIFFERENCE(v,world)) ~= v => simpBool ['AND,:w] 
    u
  'false = (y := be x) => false
  'true = y => true
  dnf2pf y

andReduce(x,y) ==
  x is ['AND,:r] =>
    y is ['AND,:s] => mkpf(S_-(r,s),'AND)
    mkpf(S_-(r,[s]),'AND)
  x
dnf2pf(x) ==
  x = 'true => 'T
  x = 'false => nil
  x isnt [.,:.] => x
  mkpf(
    [mkpf([:[k for k in b],:[['not,k] for k in a]],'AND) for [a,b] in x],'OR)
be x == b2dnf x
b2dnf x ==
  x = 'T => 'true
  x = nil => 'false
  x isnt [.,:.] => bassert x
  [op,:argl] := x
  op in '(AND and) => band argl
  op in '(OR or)   => bor argl
  op in '(NOT not) => bnot first argl
  bassert x
band x ==
  x is [h,:t] => andDnf(b2dnf h,band t)
  'true
bor x ==
  x is [a,:b] => orDnf(b2dnf a,bor b)
  'false
bnot x == notDnf b2dnf x
bassert x == [[nil,[x]]]
bassertNot x == [[[x],nil]]
------------------------Disjunctive Normal Form Code-----------------------
--        dnf is  true | false | [coaf ... ]
--        coaf is true | false | [item ... ]
--        item is anything

orDnf(a,b) ==                   -- or:  (dnf, dnf) -> dnf
  a = 'false => b
  b = 'false => a
  a = 'true or b = 'true => 'true
  null a => b     --null list means false
  a is [c] = coafOrDnf(c,b)
  coafOrDnf(first a,orDnf(rest a,b))

andDnf(a,b) ==                  -- and: (dnf, dnf) -> dnf
  a = 'true => b
  b = 'true => a
  a = 'false or b = 'false => 'false
  null a => 'false  --null list means false
  a is [c] => coafAndDnf(c,b)
  x := coafAndDnf(first a,b)
  y := andDnf(rest a,b)
  x = 'false => y
  y = 'false => x
  ordUnion(x,y)

notDnf l ==                     -- not: dnf  ->  dnf
  l = 'true => 'false
  l = 'false => 'true
  null l =>     'true --null list means false
  l is [x] => notCoaf x
  andDnf(notCoaf first l,notDnf rest l)

coafOrDnf(a,l) ==               -- or:  (coaf, dnf) -> dnf
  a = 'true or l = 'true => 'true
  a = 'false => l
  member(a,l) => l
  y := notCoaf a
  x := ordIntersection(y,l)
  null x => orDel(a,l)
  x = l => 'true
  x = y => ordSetDiff(l,x)
  ordUnion(notDnf ordSetDiff(y,x),l)

coafAndDnf(a,b) ==              --and: (coaf, dnf) -> dnf
  a = 'true => b
  a = 'false => 'false
  [c,:r] := b
  null r => coafAndCoaf(a,c) 
  x := coafAndCoaf(a,c)      --dnf
  y := coafAndDnf(a,r)       --dnf
  x = 'false => y
  y = 'false => x
  ordUnion(x,y)

coafAndCoaf([a,b],[p,q]) ==                  --and: (coaf,coaf) -> dnf
  ordIntersection(a,q) or ordIntersection(b,p) => 'false
  [[ordUnion(a,p),ordUnion(b,q)]]

notCoaf [a,b] == [:[[nil,[x]] for x in a],:[[[x],nil] for x in b]]

list1 l ==
  l isnt [h,:t] => nil
  null h => list1 t
  [[h,nil,nil],:list1 t]
list2 l ==
  l isnt [h,:t] => nil
  null h => list2 t
  [[nil,h,nil],:list2 t]
list3 l ==
  l isnt [h,:t] => nil
  null h => list3 t
  [[nil,nil,h],:list3 t]
orDel(a,l) ==
  l is [h,:t] =>
    a = h => t
    _?ORDER(a,h) => [a,:l]
    [h,:orDel(a,t)]
  [a]
ordList l ==
  l is [h,:t] and t => orDel(h,ordList t)
  l
ordUnion(a,b) ==                           
  a isnt [c,:r] => b
  b isnt [d,:s] => a
  c=d => [c,:ordUnion(r,s)]
  _?ORDER(a,b) => [c,:ordUnion(r,b)]
  [d,:ordUnion(s,a)]
ordIntersection(a,b) ==
  a isnt [h,:t] => nil
  member(h,b) => [h,:ordIntersection(t,b)]
  ordIntersection(t,b)
ordSetDiff(a,b) ==
  b isnt [h,:t] => a
  member(h,a) => ordSetDiff(remove(a,h),t)
  ordSetDiff(a,t)
-------------
testPredList u ==
  for x in u repeat
    y := simpBool x
    x = y => nil
    pp x
    pp '"==========>"
    pp y
