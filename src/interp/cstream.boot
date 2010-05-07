-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2010, Gabriel Dos Reis.
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
 
npNull x== StreamNull x
 
StreamNull x==
  null x or x is ["nullstream",:.] => true
  while x is ["nonnullstream",:.] repeat
          st:=APPLY(second x,CDDR x)
          RPLACA(x,first st)
          RPLACD(x,rest st)
  x is ["nullstream",:.]
 
Delay(f,x)==cons("nonnullstream",[f,:x])
 
StreamNil:= ["nullstream"]
 
incRgen s==Delay(function incRgen1,[s])
 
incRgen1(:z)==
        [s]:=z
        a:=shoeread_-line s
        if null a
        then (CLOSE s;StreamNil)

        else cons(a,incRgen s)
 
incIgen n==Delay(function incIgen1,[n])
incIgen1(:z)==
        [n]:=z
        n:=n+1
        cons(n,incIgen n)
 
incZip(g,f1,f2)==Delay(function incZip1,[g,f1,f2])
incZip1(:z)==
     [g,f1,f2]:=z
     StreamNull f1 => StreamNil
     StreamNull f2 => StreamNil
     cons(FUNCALL(g,car f1,car f2),incZip(g,cdr f1,cdr f2))
 
incAppend(x,y)==Delay(function incAppend1,[x,y])
 
incAppend1(:z)==
     [x,y]:=z
     if StreamNull x
     then if StreamNull y
          then StreamNil
          else y
     else cons(car x,incAppend(cdr x,y))
 
next(f,s)==Delay(function next1,[f,s])
next1(:z)==
      [f,s]:=z
      StreamNull s=> StreamNil
      h:= APPLY(f, [s])
      incAppend(car h,next(f,cdr h))
 
nextown(f,g,s)==Delay(function nextown1,[f,g,s])
nextown1 (:z)==
      [f,g,s]:=z
      StreamNull s=>
           spadcall1 g
           StreamNil
      StreamNull s
      h:=spadcall2 (f, s)
      incAppend(car h,nextown(f,g,cdr h))
 
nextown2(f,g,e,x)==nextown(cons(f,e),cons(g,e),x)
 
spadcall1(g)==
    [impl, :env] := g
    APPLY(impl, [env])
 
spadcall2(f,args) ==
    [impl, :env] := f
    APPLY(impl, [args, env])
