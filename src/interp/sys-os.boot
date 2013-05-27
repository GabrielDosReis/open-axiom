-- Copyright (C) 2007-2013 Gabriel Dos Reis.
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

import sys_-constants

namespace BOOT
import namespace System.Foreign

module sys_-os

loadSystemRuntimeCore()

--% File System Support

++ Current working directory
import oa__getcwd: () -> string for doGetWorkingDirectory

getWorkingDirectory() ==
  ensureTrailingSlash doGetWorkingDirectory()

++ change current working directory.
import oa__chdir: string -> int for changeDirectory
       -- 0: success, -1: failure
  
++ remove file or directory tree.
import oa__unlink: string -> int for removeFile
       -- 0: sucess, -1: failure

++ rename file or directory
import oa__rename: (string,string) -> int for renameFile
       -- 0: success, -1 failure

++ create a directory
import oa__mkdir: string -> int for mkdir
       -- 0: sucess, -1: failure.

++ Test whether a path names a directory.
import directoryp: string -> int for directoryp

++ Test whether a file exists and is accessible for read.
import readablep: string -> int for readablep
       -- -1: inexistent.
       --  0: exist but read access denied.
       --  1: exist and read accress granted.
  
++ Test whether a file exists and is accessible for write.
import writeablep: string -> int for writeablep
       -- -1: inexistent.
       --  0: exists but write access denied
       --  1: exists and write access granted
       --  2: inexistent but write access to parent directory granted.

import oa__filedesc__read: (int,writeonly buffer byte,int) -> int 
  for readFromFileHandle
       -- -1: failure; otherwise
       -- actual read bytes count

import oa__filedesc__write: (int,readonly buffer byte,int) -> int 
  for writeToFileHandle
       -- -1: failure; otherwise
       -- actual written bytes count

import oa__filedesc__close: int -> int for closeFileHandle
       -- -1: failure; otherwise 0.

import oa__getenv: string -> string for getEnv
  

--% Local IPC socket support

import oa__open__local__client__stream__socket: string -> int 
  for openLocalClientStreamSocket
       -- -1: failure

--% INET socket stream support

++ Convert an IP address (4 or 6) to numeric form.  The result of
++ the conversion is stored in the last argument.  The return value
++ is interpreted as follows:
++    -1: failure
++     0: success
++ Note that at the moment, only IP4 is supported.
import oa__inet__pton: (string, int, writeonly buffer byte) -> int
  for presentationToNumeric

++ Try to resolve a network hostname to its IP address.  On success,
++ return 0, otherwise -1.  The IP address is written into the 
++ third argument.
import oa__get__host__address: (string, int, writeonly buffer byte) -> int
  for hostnameToNumeric

++ Try to establish a client TCP/IP socket connection.  The IP numeric
++ address is specified by the first argument; second argument is the
++ version of IP used (4 or 6); third argument is the desired port.
++ Return -1 on failure, otherwise the file descriptor corresponding
++ to the obtained client socket.
import oa__connect__ip__port__stream: (readonly buffer byte,int,int) -> int
  for doConnectToHostAndPort

++ Try to read bytes of data from socket.  
++    Return -1 for failure; number of read bytes, otherwise.
import oa__socket__read: (int,writeonly buffer byte,int) -> int 
  for readFromStreamSocket

++ Try read a byte socket from a socket.  
++ Return -1 on failure; byte read, otherwise.
import oa__socket__read__byte: int -> int
  for doReadByteFromStreamSocket
  
++ Try to write bytes of data to socket.
++    Return -1 on failure; actual bytes written, otherwise.
import oa__socket__write: (int,readonly buffer byte,int) -> int 
  for writeToStreamSocket

++ Try to write a byte to socket.
++   Return -1 on failure; the written byte, otherwise.
import oa__socket__write__byte: (int,int) -> int
  for doWriteByteToStreamSocket

import oa__close__socket: int -> int for closeSocket

--% OpenAxiom subsystem socket support

++ socket interface
import open__server: string -> int for openServer

import sock__get__int: int -> int for sockGetInt

import sock__send__int: (int,int) -> int for sockSendInt

import sock__get__string: int -> string for sockGetString

import sock__send__string__len: (int, string, int) -> int
  for doSendString

sockSendString(type,str) ==
  doSendString(type, str, # str)

import sock__get__float: int -> double for sockGetFloat

import sock__send__float: (int,double) -> int for sockSendFloat

import sock__send__wakeup: (int,int) -> int for sockSendWakeup

import server__switch: () -> int for serverSwitch

import sock__send__signal: (int,int) -> int for sockSendSignal

--%

import oa__system: string -> int for runCommand

++ run a program with specified arguments
runProgram(prog,args) ==
)if %hasFeature KEYWORD::GCL
  SYSTEM::SYSTEM(strconc/[prog,:[:['" ",a] for a in args]])
)elseif %hasFeature KEYWORD::CLISP
  EXT::RUN_-PROGRAM(prog,KEYWORD::ARGUMENTS,args)
)elseif %hasFeature KEYWORD::SBCL
  SB_-EXT::RUN_-PROGRAM(prog,args)
)else
  systemError '"don't how to execute external program with this Lisp"
)endif

  
++ numeric limits

import quiet__double__NaN: () -> double for quietDoubleNaN

)if %hasFeature KEYWORD::GCL
import plus__infinity: () -> double for plusInfinity

import minus__infinity: () -> double for minusInfinity

$plusInfinity := plusInfinity()
$minusInfinity := minusInfinity()

)elseif %hasFeature KEYWORD::SBCL
$plusInfinity == SB_-EXT::DOUBLE_-FLOAT_-POSITIVE_-INFINITY

$minusInfinity == SB_-EXT::DOUBLE_-FLOAT_-NEGATIVE_-INFINITY
)else
-- In general Common Lisp does not provide support for infinities
-- and the like.
$plusInfinity == $DoubleFloatMaximum

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
import  std__stream__is__terminal: int -> int for stdStreamIsTerminal

--% Data layout

++ getHostByteOrder:
++   returns the byte order of the host machine.
++   0: unknown
++   1: little endian
++   2: big endian
import oa__get__host__byteorder: () -> int for getHostByteOrder
