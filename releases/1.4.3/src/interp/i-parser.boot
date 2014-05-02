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

--%
--% This file provides interface to the interperter parser (which is
--% different from the compiler parser).  A higher level interface
--% to these low level facilities exists in the algrebra as domain Parser.
--%

import cparse
import pf2sex
namespace BOOT

++ returns the list of tokens making up a line in the stream `s'
tokenizeLine s ==
  [dq, stream]  := first s
  [lines, rest] := ncloopDQlines(dq, $lines)
  setCurrentLine(mkLineList lines)
  $lines := rest
  [[npParse dqToList dq], :rest s]

++ parse the IO stream `s' obtained from file `f'
parseStream(s, f) ==
  $lines := incStream(s, f)
  collectParsedLines(
    next(function tokenizeLine,
      next(function insertpile,
        next(function lineoftoks, $lines))), nil)

++ return the list of all tokens making up the stream `s'
collectTokens s ==
  StreamNull s => nil
  ts := first s
  [ts, :collectTokens rest s]

++ return the list of parsed lines from stream `s'.  `p' is the list
++ of parsed lines collected so far.
collectParsedLines(s, p) ==
  StreamNull s => p
  ptree := first s
  collectParsedLines(rest s, [:p, ptree])

++ parse the whole file `file'.  Returns a list of parse tree
++ containing full source location information.
parseInputFile file ==
  try
    st := inputTextFile file
    parseStream(st, file)
  finally (if st ~= nil then close st)

++ Same as parseInputFile, but returns a parse form, instead of
++ of a parse tree, i.e. source location information left out.
getParseFormsFromFile file ==
  [zeroOneTran packageTran pf2Sex t for t in parseInputFile file]
