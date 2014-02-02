-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2014, Gabriel Dos Reis.
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


import includer
import scanner
namespace BOOTTRAN
module pile

shoeFirstTokPosn t == 
  tokenPosition CAAR t

shoeLastTokPosn  t== 
  tokenPosition second t

shoePileColumn t==
  rest tokenPosition CAAR t
 
-- s is a token-dq-stream
 
shoePileInsert (s)==
  bStreamNull s => [[],:s]
  toktype := tokenClass CAAAR s
  toktype = "LISP"  or toktype = "LINE" => [[first s],:rest s]
  a:=shoePileTree(-1,s)
  [[a.2],:a.3]
 
shoePileTree(n,s)==
  bStreamNull s => [false,n,[],s]
  [h,:t] := s
  hh := shoePileColumn h
  hh > n => shoePileForests(h,hh,t)
  [false,n,[],s]
 
eqshoePileTree(n,s)==
  bStreamNull s => [false,n,[],s]
  [h,:t] := s
  hh := shoePileColumn h
  hh = n => shoePileForests(h,hh,t)
  [false,n,[],s]
 
shoePileForest(n,s)==
  [b,hh,h,t] := shoePileTree(n,s)
  b => 
    [h1,t1]:=shoePileForest1(hh,t)
    [[h,:h1],t1]
  [[],s]
 
shoePileForest1(n,s)==
  [b,n1,h,t] := eqshoePileTree(n,s)
  b => 
    [h1,t1]:=shoePileForest1(n,t)
    [[h,:h1],t1]
  [[],s]
 
shoePileForests(h,n,s)==
  [h1,t1] := shoePileForest(n,s)
  bStreamNull h1 => [true,n,h,s]
  shoePileForests(shoePileCtree(h,h1),n,t1)
 
shoePileCtree(x,y) ==
  dqAppend(x,shoePileCforest y)
 
-- only enshoePiles forests with >=2 trees
 
shoePileCforest x==
  x = nil => []
  rest x = nil => first x
  a := first x
  b := shoePileCoagulate(a,rest x)
  rest b = nil => first b
  shoeEnPile shoeSeparatePiles b
 
shoePileCoagulate(a,b)==
  b = nil => [a]
  c := first b
  tokenValue CAAR c = "THEN" or tokenValue CAAR c = "ELSE" =>
    shoePileCoagulate (dqAppend(a,c),rest b)
  d := second a
  e := tokenValue d
  tokenClass d  = "KEY" and
	(e has SHOEINF or e = "COMMA" or e = "SEMICOLON") =>
    shoePileCoagulate(dqAppend(a,c),rest b)
  [a,:shoePileCoagulate(c,rest b)]
 
shoeSeparatePiles x==
  x = nil => []
  rest x = nil => first x
  a := first x
  semicolon := dqUnit mk%Token("KEY", "BACKSET",shoeLastTokPosn a)
  dqConcat [a,semicolon,shoeSeparatePiles rest x]
 
shoeEnPile x==
   dqConcat [dqUnit mk%Token("KEY","SETTAB",shoeFirstTokPosn x),
             x, _
             dqUnit mk%Token("KEY","BACKTAB",shoeLastTokPosn  x)]
 
