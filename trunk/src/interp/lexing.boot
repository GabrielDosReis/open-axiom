-- Copyright (C) 2011, Gabriel Dos Reis.
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
--     - Neither the name of OpenAxiom. nor the names of its contributors
--       may be used to endorse or promote products derived from this
--       software without specific prior written permission.
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

--%
--% Author: Gabriel Dos Reis
--%

import sys_-utility

namespace BOOT

module lexing

--%
--% Stack abstract datatype.
--%  Operational semantics:
--%     structure Stack ==
--%         Record(store: List T, size: Integer, top: T, updated?: Boolean)

++ Construct a new stack
makeStack() ==
  [nil,0,nil,false]

macro stackStore st ==
  first st

macro stackSize st ==
  second st

macro stackTop st ==
  third st

macro stackUpdated? st ==
  fourth st

stackLoad!(l,st) ==
  stackStore(st) := l
  stackSize(st) := #l
  stackTop(st) := first l

stackClear! st ==
  stackStore(st) := nil
  stackSize(st) := 0
  stackTop(st) := nil
  stackUpdate?(st) := false

stackPush!(x,st) ==
  stackStore(st) := [x,:stackStore st]
  stackTop(st) := x
  stackSize(st) := stackSize st + 1
  stackUpdated?(st) := true

stackPop! st ==
  y := first stackStore st
  stackStore(st) := rest stackStore st
  stackSize(st) := stackSize st - 1
  if stackStore st ~= nil then
    stackTop(st) := first stackStore st
  y
