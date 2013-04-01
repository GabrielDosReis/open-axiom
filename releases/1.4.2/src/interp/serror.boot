-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007, Gabriel Dos Reis.
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

import posit
namespace BOOT

--% Functions to handle specific errors (mostly syntax)
 

syGeneralErrorHere: () -> %Void
syGeneralErrorHere() ==
   sySpecificErrorHere('S2CY0002, [])
 
sySpecificErrorHere: (%Symbol,%List %Form) -> %Void
sySpecificErrorHere(key,args) ==
   sySpecificErrorAtToken($stok, key, args)
 
sySpecificErrorAtToken: (%Thing,%Symbol,%List %Form) -> %Void
sySpecificErrorAtToken(tok,key,args) ==
   pos := tokPosn tok
   ncSoftError(pos, key, args)
 
syIgnoredFromTo: (%List %Form,%List %Form) -> %Void
syIgnoredFromTo(pos1, pos2) ==
  if pfGlobalLinePosn pos1 = pfGlobalLinePosn pos2 then
      ncSoftError(FromTo(pos1,pos2), 'S2CY0005, [])
  else
      ncSoftError(From pos1, 'S2CY0003, [])
      ncSoftError(To   pos2, 'S2CY0004, [])
 
npTrapForm: %Thing -> %Void
npTrapForm(x)==
   a:=pfSourceStok x
   a='NoToken =>
         syGeneralErrorHere()
         THROW("TRAPPOINT","TRAPPED")
   ncSoftError(tokPosn a, 'S2CY0002, [])
   THROW("TRAPPOINT","TRAPPED")
 
npTrap: () -> %Thing
npTrap()==
   ncSoftError(tokPosn $stok,'S2CY0002,[])
   THROW("TRAPPOINT","TRAPPED")
 
