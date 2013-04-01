-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2013, Gabriel Dos Reis.
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

import scan
namespace BOOT

-- insertpiles converts a line-list to a line-forest where
 
-- a line is a token-dequeue and has a column which is an integer.
-- an A-forest is an A-tree-list
-- an A-tree has a root which is an A, and subtrees which is an A-forest.
 
-- A forest with more than one tree corresponds to a Scratchpad pile
-- structure (t1;t2;t3;...;tn), and a tree corresponds to a pile item.
-- The ( ; and ) tokens are inserted into a >1-forest, otherwise
-- the root of the first tree is concatenated with its forest.
-- column t is the number of spaces before the first non-space in line t
 
pileColumn t ==
  rest tokPosn CAAR t

pileComment t ==
  tokType CAAR t = "negcomment"

pilePlusComment t ==
  tokType CAAR t = "comment"
 
-- insertpile is used by next so s is non-null
-- bite off a line-tree, return it and the remaining line-list.
 
insertpile (s)==
  npNull s => [false,0,[],s]
  [h,:t] := s
  pilePlusComment h =>
    [h1,t1] := pilePlusComments s
    a := pileTree(-1,t1)
    [[pileCforest [:h1,a.2]],:a.3]
  stream := CADAR s
  a := pileTree(-1,s)
  [[[a.2,stream]],:a.3]
 
pilePlusComments s==
  npNull s => [[],s]
  [h,:t] := s
  pilePlusComment h =>
    [h1,t1]:=pilePlusComments t
    [[h,:h1],t1]
  [[],s]
 
pileTree(n,s)==
  npNull s => [false,n,[],s]
  [h,:t] := s
  hh := pileColumn first h
  hh > n => pileForests(first h,hh,t)
  [false,n,[],s]
 
eqpileTree(n,s)==
  npNull s => [false,n,[],s]
  [h,:t] := s
  hh := pileColumn first h
  hh = n => pileForests(first h,hh,t)
  [false,n,[],s]
 
pileForest(n,s)==
  [b,hh,h,t] := pileTree(n,s)
  b => 
    [h1,t1]:=pileForest1(hh,t)
    [[h,:h1],t1]
  [[],s]
 
pileForest1(n,s)==
  [b,n1,h,t] := eqpileTree(n,s)
  b =>
    [h1,t1]:=pileForest1(n,t)
    [[h,:h1],t1]
  [[],s]
 
pileForests(h,n,s)==
  [h1,t1] := pileForest(n,s)
  npNull h1 => [true,n,h,s]
  pileForests(pileCtree(h,h1),n,t1)
 
pileCtree(x,y)==
  dqAppend(x,pileCforest y)
 
-- only enpiles forests with >=2 trees
 
pileCforest x==
  x = nil => []
  x is [f] =>
    tokPart CAAR f = "IF" => enPile f
    f
  enPile separatePiles x
 
firstTokPosn t ==
  tokPosn CAAR t

lastTokPosn t ==
  tokPosn second t
 
separatePiles x==
  x = nil => []
  x is [a] => a
  a := first x
  semicolon := dqUnit tokConstruct("key", "BACKSET",lastTokPosn a)
  dqConcat [a,semicolon,separatePiles rest x]
 
enPile x==
   dqConcat [dqUnit tokConstruct("key","SETTAB",firstTokPosn x),
             x, _
             dqUnit tokConstruct("key","BACKTAB",lastTokPosn  x)]
 
