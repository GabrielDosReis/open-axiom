-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis
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
--
--

--
-- This file defines functions related to diagnostics issuance, etc.
-- These routines are bused by both the interprerter and the compiler.
--

import sys_-constants
import sys_-globals
import vmlisp
namespace BOOT


++ Subroutine of moanRetract.
makeReasonable s ==
  # s > 30 =>
    strconc('"expression beginning ",subString(s,0,20))
  s

++ This rountine is used by the runtime system to report failed
++ attempt to coerce a value from one type to another.  Usually
++ this involves Union branches or SubDomains or other forms
++ of retraction.
moanRetract(v,t) ==
  error
    strconc(makeReasonable STRINGIMAGE v,'" cannot be coerce to mode ",
      outputDomainConstructor t)

++ This routine is used by the interperter to count syntax, or
++ precompilation, or semantics analysis errors.

BUMPERRORCOUNT kind ==
  countError()
  $InteractiveMode =>
    index := 
      kind = "syntax" => 0
      kind = "precompilation" => 1
      kind = "semantic" => 2
      ERROR '"BUMPERRORCOUNT: unknown error kind"
    $SPAD__ERRORS.index := 1 + $SPAD__ERRORS.index

FAIL() ==
  systemError '"Antique error (FAIL ENTERED)"

ERRHUH() ==
  systemError '"problem with BOOT to LISP translation"

MOAN(:x) ==
  sayBrightly ['"%l", '"===> ", :x, '"%l"]

CROAK(:x) ==
  systemError x

THETA__ERROR op ==
  userError ['"Sorry, do not know the identity element for ", op]

SAY(:x) ==
  MESSAGEPRINT x
  finishLine $OutputStream

MESSAGEPRINT x ==
  MAPC(function MESSAGEPRINT_-1, x)

MESSAGEPRINT_-1 x ==
  cons? x =>
    writeString '"("
    MESSAGEPRINT_-1 first x
    MESSAGEPRINT_-2 rest x
    writeString '")"
  x = "%l" or x is '"%l" => finishLine $OutputStream
  PRINC x

MESSAGEPRINT_-2 x ==
  x isnt [.,:.] =>
    not null x =>
      writeString '" . "
      MESSAGEPRINT_-1 x
  writeString '" "
  MESSAGEPRINT_-1 first x
  MESSAGEPRINT_-2 rest x

--%

++ if not nil, gives stream for sayBrightly output
$sayBrightlyStream := nil

sayBrightlyNT1(x,out) ==
  cons? x => brightPrint(x,out)
  brightPrint0(x,out)

sayBrightly1(x,out) ==
  sayBrightlyNT1(x,out)
  finishLine out

sayBrightlyNT(x,out == $OutputStream) ==
  x = nil => nil
  $sayBrightlyStream ~= nil => sayBrightlyNT1(x,$sayBrightlyStream)
  ioTerminal? out => sayBrightlyNT1(x,out)
  sayBrightly1(x,out) => sayBrightlyNT1(x,out)
  nil

sayBrightly(x,out == $OutputStream) ==
  x = nil => nil
  $sayBrightlyStream ~= nil => sayBrightly1(x,$sayBrightlyStream)
  ioTerminal? out => sayBrightly1(x,out)
  sayBrightly1(x,out) => sayBrightly1(x,$OutputStream)
  nil

sayBrightlyI(x,out == $OutputStream) ==
  x = nil => nil
  sayBrightly1(x,out)

sayMSG x ==
  x = nil => nil
  sayBrightly1(x,$algebraOutputStream)

sayMSG2File msg ==
  file := makePathname('spadmsg,'listing,nil)
  str := DEFIOSTREAM(['(MODE . OUTPUT),['FILE,:file]],255,0)
  sayBrightly1(msg,str)

sayTeX x ==
  x = nil => nil
  sayBrightly1(x,$texOutputStream)
