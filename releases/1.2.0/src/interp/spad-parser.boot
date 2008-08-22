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

-- This file contains a low-level code for parsing a Spad source file 
-- into an internal AST.  Please note that this AST is the AST used by
-- the Spad compiler, which is different from the AST used by the 
-- interpreter (a VAT). The AST, for the entire file, is a List Syntax
-- Since this is low-level code, I don't expect people to get here,
-- and you should directly use it.  If you think, you need to get to
-- here, then something is already wrong.
-- There is a higher-level interface, written in SPAD, to this
-- code.  See the algebra file spad-parser.spad.
--
-- -- gdr/2007-11-02
--

import metalex
import parse
namespace BOOT

++ Given a pathname to a source file containing Spad code, returns
++ a list of (old) AST objects representing the toplevel expressions
++ in that file.
++ ??? system commands are still executed even if they may not be
++ ??? meaningful.  Eventually this code will go away when we 
++ ??? finally use the new parser everwhere.
parseSpadFile sourceFile ==
  $SPAD := true                          -- we are parsing Spad, 
  SETQ(_*EOF_*, false)                   -- end of current input?
  FILE_-CLOSED : local := false          -- current stream closed?

  $OutputStream := MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"    -- noise to standard output
  -- we need to tell the post-parsing transformers that we're compiling
  -- Spad because few parse forms have slightly different representations
  -- depending on whether we are interpreter mode or compiler mode.
  savedInteractiveMode := $InteractiveMode
  $InteractiveMode := false
  INIT_-BOOT_/SPAD_-READER()
  -- we need to restore the global input stream state after we
  -- finished messing with it.
  savedInStream := (IN_-STREAM : local)

  -- If soureFile cannot be processed for whatever reasons
  -- get out of here instead of being stuck later.
  not (IN_-STREAM := MAKE_-INSTREAM sourceFile) =>
    $InteractiveMode := savedInteractiveMode
    IN_-STREAM := savedInStream
    systemError '"cannot open input source file"
  INITIALIZE_-PREPARSE IN_-STREAM

  -- gather parse trees for all toplevel expressions in sourceFile.
  asts := []                                   
  while ^(_*EOF_* or FILE_-CLOSED) repeat
    BOOT_-LINE_-STACK : local := PREPARSE IN_-STREAM
    LINE : local := CDAR BOOT_-LINE_-STACK
    PARSE_-NewExpr()
    asts := [parseTransform postTransform POP_-STACK_-1(), :asts]
  -- clean up the mess, and get out of here
  IOCLEAR(IN_-STREAM, OUT_-STREAM)             
  SHUT IN_-STREAM                              
  IN_-STREAM := savedInStream
  $InteractiveMode := savedInteractiveMode
  -- we accumulated the parse trees in reverse order
  nreverse asts

