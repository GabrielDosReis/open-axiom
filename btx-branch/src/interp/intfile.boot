-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2008, Gabriel Dos Reis.
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


import cstream
namespace BOOT
module intfile

shoeInternFile(fn)==
   a:=shoeInputFile fn
   if null a
   then WRITE_-LINE (CONCAT(fn,'" not found"),$ErrorStream)
   else shoeIntern incRgen a
 
shoeIntern (s)==
   StreamNull s => nil
   f:=CAR s
   # f < 8 => shoeIntern CDR s
   f.0=char " " =>shoeIntern CDR s
   a:=INTERN SUBSTRING (f,0,8)
   [b,c]:= shoeStrings CDR s
   SETF(GET (a,"MSGS"),b)
   shoeIntern c
 
shoeStrings (stream)==
   StreamNull stream => ['"",stream]
   a:=CAR stream
   if a.0^=char " "
   then ['"",stream]
   else
       [h,t]:=shoeStrings(cdr stream)
       [CONCAT(a,h),t]
 
--fetchKeyedMsg(key,b)== GET(key,"MSGS")
--shoeInternFile '"/usr/local/scratchpad/cur/doc/msgs/co-eng.msgs"
