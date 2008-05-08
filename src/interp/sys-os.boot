-- Copyright (C) 2007-2008 Gabriel Dos Reis.
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
--
--

--
-- This file contains interface to functionalities required from the
-- hosting operating systems.  More to the point, it is mostly
-- a set of interfaces to native routines provided in the
-- supporting C runtime libopen-axiom-core.
--

module sys_-os
import types
import cfuns
namespace BOOT

)if not %hasFeature KEYWORD::GCL
loadSystemRuntimeCore()
)endif

++ change current working directory.
import changeDirectory for
  oa__chdir: string -> int -- 0: success, -1: failure

++ remove file or directory tree.
import removeFile for
  oa__unlink: string -> int -- 0: sucess, -1: failure

++ rename file or directory
import renameFile for
  oa__rename: (string,string) -> int  -- 0: success, -1 failure

++ create a directory
import mkdir for
  oa__mkdir: string -> int  -- 0: sucess, -1: failure.

import getEnv for
  oa__getenv: string -> string

++ socket interface
import openServer for
  open__server: string -> int

import sockGetInt for
  sock__get__int: int -> int

import sockSendInt for
  sock__send__int: (int,int) -> int

import sockGetString for
  sock__get__string: int -> string

import doSendString for
  sock__send__string__len: (int, string, int) -> int

sockSendString(type,str) ==
  doSendString(type, str, LENGTH str)

import sockGetFloat for
  sock__get__float: int -> double

import sockSendFloat for
  sock__send__float: (int,double) -> int

import sockSendWakeup for
  sock__send__wakeup: (int,int) -> int

import serverSwitch for
  server__switch: () -> int

import flushStdout for
  flush__stdout: () -> int

import sockSendSignal for
  sock__send__signal: (int,int) -> int

import printLine for
  print__line: string -> int

--%
import directoryp for
  directoryp: string -> int

import writeablep for
  writeablep: string -> int

import runCommand for
  oa__system: string -> int

++ run a program with specified arguments
runProgram(prog,args) ==
)if %hasFeature KEYWORD::GCL
  SYSTEM::SYSTEM CONCAT/[prog,:[:['" ",a] for a in args]]
)elseif %hasFeature KEYWORD::CLISP
  EXT::RUN_-PROGRAM(prog,KEYWORD::ARGUMENTS,args)
)elseif %hasFeature KEYWORD::SBCL
  SB_-EXT::RUN_-PROGRAM(prog,args)
)else
  systemError '"don't how to execute external program with this Lisp"
)endif

  
++ numeric limits
)if %hasFeature KEYWORD::GCL
import plusInfinity for
  plus__infinity: () -> double

import minusInfinity for
  minus__infinity: () -> double

import NaNQ for
  NANQ: () -> double

$plusInfinity := plusInfinity()
$minusInfinity := minusInfinity()
$NaNValue := NaNQ()

)elseif %hasFeature KEYWORD::SBCL
$plusInfinity == SB_-EXT::DOUBLE_-FLOAT_-POSITIVE_-INFINITY

$minusInfinity == SB_-EXT::DOUBLE_-FLOAT_-NEGATIVE_-INFINITY
)else
-- In general Common Lisp does not provide support for infinities
-- and the like.
$plusInfinity == MOST_-POSITIVE_-DOUBLE_-FLOAT

$minusInfinity == -$plusInfinity
)endif

)if not %hasFeature KEYWORD::GCL
plusInfinity() ==
  $plusInfinity

minusInfinity() ==
  $minusInfinity
)endif

++ stdStreamIsTerminal:
++   returns 1 if the standard stream is attached to a terminal;
++   otherwise 0.
import stdStreamIsTerminal for std__stream__is__terminal: int -> int

--%
