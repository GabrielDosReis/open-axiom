-- Copyright (C) 2007-2008 Gabriel Dos Reis
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

import types
namespace BOOT

++ true means try starting an open server
$openServerIfTrue := true

++ the name of the spad server socket
$SpadServerName == '"/tmp/.d"

++ true means that the core executable acts as remote server.
$SpadServer := false

++ if true, then the interpreter or compiler should inform about 
++ code generation, etc.
$verbose := true

$PrintCompilerMessageIfTrue := true

++
$options := []

+++ Initialization routine run by the core system before handing off
+++ to the interpreter or compiler.  
+++ ??? This part is still in flux.
AxiomCore::%sysInit() ==
  SETQ(_*PACKAGE_*, FIND_-PACKAGE '"BOOT")
  initMemoryConfig()
  if not (%hasFeature KEYWORD::GCL) then
    loadSystemRuntimeCore()
)if %hasFeature KEYWORD::CLISP
    -- a goat for CLisp FFI, please.
    sys_-osInitCLispFFI()    
)endif
)if %hasFeature KEYWORD::GCL
  SETQ(COMPILER::_*COMPILE_-VERBOSE_*,false)
  SETQ(COMPILER::_*SUPPRESS_-COMPILER_-WARNINGS_*,true)
  SETQ(COMPILER::_*SUPPRESS_-COMPILER_-NOTES_*,true)
)endif


++ Returns the system algebra directory, as specified on command
++ line.  nil, otherwise.
systemAlgebraDirectory() ==
  dir := ASSOC(Option '"sysalg", %systemOptions()) =>
    ensureTrailingSlash rest dir
  nil

++ Returns a path to the directory containing algebra bootstsrap files.
algebraBootstrapDir() ==
  dir := ASSOC(Option '"strap",%systemOptions()) =>
    ensureTrailingSlash rest dir
  nil

++ Load list of exposed categories, domains, and packages.
++ User-specified list takes precedence over system wide list.
loadExposureGroupData() ==
  LOAD('"./exposed", KEYWORD::VERBOSE,false,
    KEYWORD::IF_-DOES_-NOT_-EXIST,nil) => "done"
  LOAD(CONCAT(systemRootDirectory(),'"algebra/exposed"),
    KEYWORD::VERBOSE,false,KEYWORD::IF_-DOES_-NOT_-EXIST,nil) => "done"
  "failed"

++ 
$defaultMsgDatabaseName := nil

++
REROOT: () -> %Thing
REROOT() ==
  $DIRECTORY_-LIST := MAPCAR(function makeAbsoluteFilename,
    $RELATIVE_-DIRECTORY_-LIST)
  $LIBRARY_-DIRECTORY_-LIST := MAPCAR(function makeAbsoluteFilename,
    $RELATIVE_-LIBRARY_-DIRECTORY_-LIST)
  $defaultMsgDatabaseName := PATHNAME
    makeAbsoluteFilename '"share/msgs/s2-us.msgs"
  $msgDatabaseName := nil

initMemoryConfig: () -> %Thing
initMemoryConfig() ==
)if %hasFeature KEYWORD::GCL
  SYSTEM::ALLOCATE("CONS",500)
  SYSTEM::ALLOCATE("FIXNUM",200)
  SYSTEM::ALLOCATE("SYMBOL",500)
  SYSTEM::ALLOCATE("PACKAGE",3)
  SYSTEM::ALLOCATE("ARRAY",400)
  SYSTEM::ALLOCATE("STRING",500)
  SYSTEM::ALLOCATE("CFUN",100)
  SYSTEM::ALLOCATE_-CONTIGUOUS_-PAGES 3000
  SYSTEM::ALLOCATE_-RELOCATABLE_-PAGES 1000
  SYSTEM::SET_-HOLE_-SIZE 2000
)else
  nil
)endif

++
restart() ==
  IN_-PACKAGE '"BOOT"      -- ??? is this still necessary?
  -- ??? Ideally, we should not be calling AxiomCore::topLevel.
  -- ??? Rather, we should be called by that function.  Therefore
  -- ??? it currently serves only for option processing and we cannot
  -- ??? do any substantial work if we call from it.
  AxiomCore::topLevel()
  REROOT()
)if %hasFeature KEYWORD::GCL
  SYSTEM::GBC_-TIME 0
)endif
  if $openServerIfTrue and FBOUNDP "openServer" then
    os := openServer $SpadServerName
    if ZEROP os then
      $openServerIfTrue := false
      $SpadServer := true
  $IOindex := 1
  $InteractiveFrame := makeInitialModemapFrame()
  loadExposureGroupData()
  initHist()
  initializeInterpreterFrameRing()
  if $displayStartMsgs then 
    spadStartUpMsgs()
  $currentLine := nil
  RESTART0()
  readSpadProfileIfThere()
  spad()
   

++ ??? Rework this
SPAD_-SAVE(file) ==
  SETQ($SpadServer,false)
  SETQ($openServerIfTrue,true)
)if %hasFeature KEYWORD::GCL
  SYSTEM::SAVE_-SYSTEM file
)else
  fatalError '"don't know how to same image"
)endif


SET_-RESTART_-HOOK() ==
)if %hasFeature KEYWORD::GCL
  SETQ(SYSTEM::_*TOP_-LEVEL_-HOOK_*,"restart")
)endif
  "restart"

++ execute Spad script
executeSpadScript(progname,options,file) ==
  REROOT()
  $IOindex := 1
  $InteractiveFrame := makeInitialModemapFrame()
  loadExposureGroupData()
  initHist()
  initializeInterpreterFrameRing()
  $currentLine := nil
  RESTART0()
  $NEWSPAD := true
  $SPAD := true
  if getOption(Option '"verbose",%systemOptions()) then
    $verbose := true
    $options := []
    $ProcessInteractiveValue := false
  else
    $verbose := false
    $options := [["quiet"]]
    $ProcessInteractiveValue := true
  $PrintCompilerMessageIfTrue := $verbose
  CATCH($intCoerceFailure,
   CATCH($intSpadReader,read [file]))
  coreQuit (errorCount()> 0 => 1; 0)

associateRequestWithFileType(Option '"script", '"input",
   function executeSpadScript)

++ compiler Spad Library File.
compileSpadLibrary(progname,options,file) ==
  REROOT()
  $IOindex := 1
  $InteractiveFrame := makeInitialModemapFrame()
  loadExposureGroupData()
  initHist()
  initializeInterpreterFrameRing()
  $currentLine := nil
  RESTART0()
  $NEWSPAD := true
  $SPAD := true
  $EchoLines := false
  ECHO_-META : fluid := false
  $verbose := false
  $ProcessInteractiveValue := true
  $PrintCompilerMessageIfTrue := $verbose
  CATCH($intTopLevel,
    CATCH("SpadCompileItem",
     CATCH($intSpadReader,compiler [file])))
  coreQuit (errorCount()> 0 => 1; 0)

associateRequestWithFileType(Option '"compile", '"spad",
   function compileSpadLibrary)

