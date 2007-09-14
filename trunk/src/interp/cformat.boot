-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copryight (C) 2007, Gabriel Dos Reis
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


import '"unlisp"
import '"posit"

)package "BOOT"

--% Formatting functions for various compiler data objects.
--  These are used as [%origin o, %id n] for %1f %2f... style arguments
--  in a keyed message.
--  SMW, SG June 88
 
%id a     == [IDENTITY, a]
 
-- Union(FileName,"strings","console")
%origin x ==
    [function porigin, x]
porigin x ==
    (STRINGP x => x; pfname x)
 
%fname x ==
    [function pfname, x]
pfname x ==
    PathnameString x
 

%pos p == [function ppos, p]
ppos p ==
    pfNoPosition? p => ['"no position"]
    pfImmediate? p  => ['"console"]
    cpos := pfCharPosn p
    lpos := pfLinePosn p
    org  := porigin pfFileName p
    [org,'" ",'"line",'" ",lpos]
 
%key keyStuff == [function pkey, keyStuff]
--keyStuff ::= keynumber | [ one or more keySeqs ]
--keySeq   ::= keynumber optargList optdbn
--optARgL  ::= [ 0 or more arguments ] | nothing at all
--optDbn   ::= ['dbN , databaseName ] | nothing at all
----------- (override in format.boot.pamphlet)
pkey keyStuff ==
    if not PAIRP keyStuff then keyStuff := [keyStuff]
    allMsgs := []
    while not null keyStuff repeat
        dbN := NIL
        argL := NIL
        key := first keyStuff
        keyStuff := IFCDR keyStuff
        next := IFCAR keyStuff
        while PAIRP next repeat
            if CAR next = 'dbN then dbN := CADR next
            else argL := next
            keyStuff  := IFCDR keyStuff
            next      := IFCAR keyStuff
        oneMsg  := returnStLFromKey(key,argL,dbN)
        allMsgs := NCONC (oneMsg,allMsgs)
    allMsgs
 
