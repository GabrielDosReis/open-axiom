-- Copyright (C) 2007 Gabriel Dos Reis
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
--
--

--
-- This file collects and documents routines common to the driver of
-- both the OpenAxiom compiler and interpreter.
--

import '"boot-pkg"
)package "BOOT"

+++ Initialization routine run by the core system before handing off
+++ to the interpreter or compiler.  
+++ ??? This part is still in flux.
AxiomCore::%sysInit() ==
  SETQ(_*PACKAGE_*, FIND_-PACKAGE '"BOOT")


+++ Returns the root directory of the running system.
+++ A directory specified on command line takes precedence
+++ over directory specified at configuration time.
systemRootDirectory() ==
  dir := ASSOC(Option '"system", %systemOptions()) =>
    ensureTrailingSlash cdr dir
  $systemInstallationDirectory


+++ Returns the system algebra directory, as specified on command
+++ line.  nil, otherwise.
systemAlgebraDirectory() ==
  dir := ASSOC(Option '"sysalg", %systemOptions()) =>
    ensureTrailingSlash cdr dir
  nil

++ stdStreamIsTerminal:
++   returns 1 if the standard stream is attached to a terminal;
++   otherwise 0.
)if %hasFeature KEYWORD::GCL
import stdStreamIsTerminal for std__stream__is__terminal: INT -> INT
)else
stdStreamIsTerminal fd ==
  0
)endif

