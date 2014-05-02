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

import vmlisp
namespace BOOT
module astr where
  ncTag: %Thing -> %Symbol
  ncAlist: %Thing -> %List %Thing
  ncEltQ: (%List %Thing,%Thing) -> %Thing
  ncPutQ: (%List %Thing,%Thing,%Thing) -> %Thing

--% Attributed Structures (astr)
-- For objects which are pairs where the first field is either just a tag
-- (an identifier) or a pair which is the tag and an association list.
 
-- Pick off the tag
ncTag x ==
   x isnt [.,:.] => ncBug('S2CB0031,[])
   x := first x
   ident? x => x
   x isnt [.,:.] => ncBug('S2CB0031,[])
   first x
 
-- Pick off the property list
ncAlist x ==
   x isnt [.,:.] => ncBug('S2CB0031,[])
   x := first x
   ident? x => nil
   x isnt [.,:.] => ncBug('S2CB0031,[])
   rest x

 --- Get the entry for key k on x's association list
ncEltQ(x,k) ==
   r := objectAssoc(k,ncAlist x) => rest r
   ncBug ('S2CB0007,[k])
 
-- Put (k . v) on the association list of x and return v
-- case1: ncPutQ(x,k,v) where k is a key (an identifier), v a value
--        put the pair (k . v) on the association list of x and return v
-- case2: ncPutQ(x,k,v) where k is a list of keys, v a list of values
--        equivalent to [ncPutQ(x,key,val) for key in k for val in v]
ncPutQ(x,k,v) ==
   LISTP k =>
      for key in k for val in v repeat ncPutQ(x,key,val)
      v
   r := objectAssoc(k,ncAlist x)
   if null r then
      r := [[k,:v], :ncAlist x]
      x.first := [ncTag x,:r]
   else
      r.rest := v
   v
 
