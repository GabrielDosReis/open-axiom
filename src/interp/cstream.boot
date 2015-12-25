-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2011, Gabriel Dos Reis.
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

--% Stream Utilities
 
npNull x ==
  StreamNull x
 
StreamNull x ==
  null x or x is ["nullstream",:.] => true
  while x is ["nonnullstream",f,:args] repeat
    st := apply(f,args)
    x.first := first st
    x.rest := rest st
  x is ["nullstream",:.]
 
Delay(f,x) ==
  ["nonnullstream",:[f,:x]]
 
StreamNil == ["nullstream"]
 
incRgen s ==
  Delay(function incRgen1,[s])
 
incRgen1 s==
  a := readLine s
  a = %nothing => (closeStream s;StreamNil)
  [a,:incRgen s]
 
incIgen n ==
  Delay(function incIgen1,[n])

incIgen1 n==
  n:=n+1
  [n,:incIgen n]
 
incZip(g,f1,f2)==
  Delay(function incZip1,[g,f1,f2])

incZip1(g,f1,f2) ==
  StreamNull f1 => StreamNil
  StreamNull f2 => StreamNil
  [apply(g,[first f1,first f2]),:incZip(g,rest f1,rest f2)]
 
incAppend(x,y) ==
  Delay(function incAppend1,[x,y])
 
incAppend1(x,y)==
  if StreamNull x
  then if StreamNull y
       then StreamNil
       else y
  else [first x,:incAppend(rest x,y)]
 
next(f,s) ==
  Delay(function next1,[f,s])

next1(f,s) ==
  StreamNull s=> StreamNil
  h := apply(f, [s])
  incAppend(first h,next(f,rest h))
 
nextown(f,g,s) ==
  Delay(function nextown1,[f,g,s])

nextown1(f,g,s) ==
  StreamNull s=>
    spadcall1 g
    StreamNil
  StreamNull s
  h := spadcall2 (f, s)
  incAppend(first h,nextown(f,g,rest h))
 
nextown2(f,g,e,x) == 
  nextown([f,:e],[g,:e],x)
 
spadcall1(g)==
  [impl, :env] := g
  apply(impl, [env])
 
spadcall2(f,args) ==
  [impl, :env] := f
  apply(impl, [args, env])
