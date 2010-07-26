-- Copyright (C) 2007-2010 Gabriel Dos Reis
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

$PrintCompilerMessageIfTrue := true

++
$options := []

+++ Initialization routine run by the core system before handing off
+++ to the interpreter or compiler.  
+++ ??? This part is still in flux.
AxiomCore::%sysInit() ==
  SETQ(_*READ_-DEFAULT_-FLOAT_-FORMAT_*, "DOUBLE-FLOAT")
  SETQ(_*PACKAGE_*, FIND_-PACKAGE '"BOOT")
  SETQ(_*LOAD_-VERBOSE_*,false)
  initMemoryConfig()
  loadSystemRuntimeCore()
)if %hasFeature KEYWORD::CLISP
  -- a goat for CLisp FFI, please.
  sys_-osInitCLispFFI()
  -- Tell CLISP to stop being anal retentive, please.
  SETF(CUSTOM::_*WARN_-ON_-FLOATING_-POINT_-CONTAGION_*,false)
)endif
)if %hasFeature KEYWORD::GCL or %hasFeature KEYWORD::ECL
  SETQ(COMPILER::_*COMPILE_-VERBOSE_*,false)
  SETQ(COMPILER::_*SUPPRESS_-COMPILER_-WARNINGS_*,true)
  SETQ(COMPILER::_*SUPPRESS_-COMPILER_-NOTES_*,true)
)endif

--%

++ Returns the directory name as specified through option name `opt'.
directoryFromCommandLine opt ==
  dir := ASSOC(Option opt, %systemOptions()) =>
    ensureTrailingSlash rest dir
  nil

++ Returns the system algebra directory, as specified on command
++ line.  nil, otherwise.
systemAlgebraDirectory() ==
  directoryFromCommandLine '"sysalg"

++ Returns a path to the directory containing algebra bootstsrap files.
algebraBootstrapDir() ==
  directoryFromCommandLine '"strap"

++ Returns a path to the directory containing databases, as specified
++ on command line.  Nil otherwise.
systemDatabaseDirectory() ==
  directoryFromCommandLine '"sysdb"

++ Load list of exposed categories, domains, and packages.
++ User-specified list takes precedence over system wide list.
loadExposureGroupData() ==
  LOAD('"./exposed", KEYWORD::VERBOSE,false,
    KEYWORD::IF_-DOES_-NOT_-EXIST,nil) => "done"
  LOAD(strconc(systemRootDirectory(),'"algebra/exposed"),
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

--%

openDatabases() ==
  COMPRESSOPEN()
  INTERPOPEN()
  OPERATIONOPEN()
  CATEGORYOPEN()
  BROWSEOPEN()
  CREATE_-INITIALIZERS()

++
restart() ==
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
  openDatabases()
  readSpadProfileIfThere()
)if %hasFeature KEYWORD::GCL
  spad()
)else
  SETQ(_*DEBUGGER_-HOOK_*,LAMBDA(c h, systemErrorHandler c))
  !(handler-bind ((error #'|systemErrorHandler|)) (|spad|))
)endif

--%    

++ Initialize all global states that need to.  Sub-routine of the command
++ line compiler, the script executor, etc.  Mess with care.
initializeGlobalState() ==
  REROOT()
  have_to := $StandardLinking or not %basicSystemIsComplete()

  -- 0. Global variables.
  $inLispVM := false
  $IOindex := 1
  $currentLine := nil
  $NEWSPAD := true
  $SPAD := true
  $buildingSystemAlgebra := getOptionValue "system-algebra"
  $verbose := getOptionValue "verbose"
  $bootStrapMode := getOptionValue "bootstrap"
  $compileExportsOnly := getOptionValue "exports-only"
  $compileDefaultsOnly := getOptionValue "defaults-only"
  $reportOptimization := getOptionValue "show-insn"
  $optimizeRep := getOptionValue "inline-rep"
  setCompilerOptimizations(getOptionValue "optimize" or
                             $defaultOptimizationLevel)
  GCMSG(NIL)
  if have_to then
    $superHash := hashTable 'EQUAL
  initNewWorld()

  -- 1. Macros.
  if have_to then buildHtMacroTable()

  -- 2. History
  if $verbose and $displayStartMsgs then 
    sayKeyedMsg("S2IZ0053",['"history"])
  initHist()

  -- 3. Databases
  if $verbose and $displayStartMsgs then 
    sayKeyedMsg("S2IZ0053",['"database"])
  if have_to then
    fillDatabasesInCore()
    mkLowerCaseConTable()
  else  
    openDatabases()

  -- 4. Constructors
  if $verbose and $displayStartMsgs then 
    sayKeyedMsg("S2IZ0053",['"constructors"])
  loadExposureGroupData()
  if have_to then makeConstructorsAutoLoad()

  -- 5. Rule sets.
  if not $ruleSetsInitialized 
    then initializeRuleSets()

  -- 6. Interpreter
  if have_to then
    if $verbose and $displayStartMsgs then 
      sayKeyedMsg("S2IZ0053",['"interpreter"])
    initializeTimedNames($interpreterTimedNames,$interpreterTimedClasses)
    statisticsInitialization()
    initializeSystemCommands()
  $InteractiveFrame := makeInitialModemapFrame()
  initializeInterpreterFrameRing()

  -- 7. Etc.
  if have_to and functionp 'addtopath then 
    addtopath strconc(systemRootDirectory(),'"bin")
  -- Take off


++ execute Spad script
executeSpadScript(progname,options,file) ==
  $displayStartMsgs := false
  initializeGlobalState()
  if $verbose then
    $options := []
    $ProcessInteractiveValue := false
  else
    $options := [["quiet"]]
    $ProcessInteractiveValue := true
  $PrintCompilerMessageIfTrue := $verbose
  CATCH($intCoerceFailure,
   CATCH($SpadReaderTag,read [file]))
  coreQuit (errorCount()> 0 => 1; 0)

associateRequestWithFileType(Option '"script", '"input",
   function executeSpadScript)

++ compiler Spad Library File.
compileSpadLibrary(progname,options,file) ==
  $displayStartMsgs := false
  initializeGlobalState()
  $EchoLines := false
  ECHO_-META : fluid := false
  $verbose := false
  $ProcessInteractiveValue := true
  $PrintCompilerMessageIfTrue := $verbose
  CATCH($intTopLevel,
    CATCH("SpadCompileItem",
     CATCH($SpadReaderTag,compiler [file])))
  coreQuit (errorCount()> 0 => 1; 0)

associateRequestWithFileType(Option '"compile", '"spad",
   function compileSpadLibrary)


buildDatabasesHandler(prog,options,args) ==
  $displayStartMsgs := false
  initializeGlobalState()
  MAKE_-DATABASES args
  coreQuit(errorCount() > 0 => 1; 0)

installDriver(Option '"build-databases",function buildDatabasesHandler)

--%

++ Main entry point to the interactive system.
systemMain() ==
  IN_-PACKAGE '"BOOT"      -- ??? is this still necessary?
  -- ??? Ideally, we should not be calling AxiomCore::topLevel.
  -- ??? Rather, we should be called by that function.  Therefore
  -- ??? it currently serves only for option processing and we cannot
  -- ??? do any substantial work if we call from it.
  AxiomCore::topLevel()
  REROOT()
  -- ??? Make this call unconditional
  if $StandardLinking then 
    initializeGlobalState()
  $leanMode := getOptionValue "mode" = '"lean"
  %basicSystemIsComplete() => 
    restart()
)if %hasFeature KEYWORD::ECL
    SI::_*LISP_-INITIALIZED_* : local := true
    apply($originalLispTopLevel,nil)
)elseif %hasFeature KEYWORD::SBCL 
    apply($originalLispTopLevel,nil)
)endif
  fatalError '"fell off systemMain"
