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


import c_-util
namesoace BOOT
module nruntime

++ fetchs the item in the nth entry of a domain shell.
getShellEntry: (%Shell,%Short) -> %Thing
getShellEntry(s,i) ==
  SVREF(s,i)

++ sets the nth nth entry of a domain shell to an item.
setShellEntry: (%Shell,%Short,%Thing) -> %Thing
setShellEntry(s,i,t) ==
  SETF(SVREF(s,i),t)

unloadOneConstructor(cnam,fn) ==
    REMPROP(cnam,'LOADED)
    SETF(SYMBOL_-FUNCTION cnam,mkAutoLoad(fn, cnam))

devaluateDeeply x ==
  VECP x => devaluate x
  atom x => x
  [devaluateDeeply y for y in x]

lookupDisplay(op,sig,vectorOrForm,suffix) ==
  null $NRTmonitorIfTrue => nil
  prefix := (suffix = '"" => ">"; "<")
  sayBrightly
    concat(prefix,formatOpSignature(op,sig),
        '" from ", prefix2String devaluateDeeply vectorOrForm,suffix)

isInstantiated [op,:argl] ==
  u:= lassocShiftWithFunction(argl,HGET($ConstructorCache,op),'domainEqualList)
    => CDRwithIncrement u
  nil

isCategoryPackageName nam ==
  p := PNAME opOf nam
  p.(MAXINDEX p) = char '_&


