-- Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
-- All rights reserved.
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
--

--
-- This file collects and documents some the global variables used by either
-- the interpreter or the compiler or both.
--

import hash
import sys_-constants
namespace BOOT
module sys_-globals where
  --
  $IOindex : %Short := 0
  $BOOT: %Boolean := false
  $TRACELETFLAG: %Boolean := false

  ++ FIXME
  $saturn: %Boolean := false

  ++ FIXME
  $SPAD__ERRORS: %Vector %Short := VECTOR(0, 0, 0)

  ++
  $bootStrapMode: %Boolean := false

  ++
  $BreakMode: %Symbol := "query"

  ++
  $cacheAlist: %List := nil
  $cacheCount: %Short := 0


  ++
  $clamList: %List :=
    '((canCoerce hash UEQUAL count)                 _
      (canCoerceFrom hash UEQUAL count)             _
      (coerceConvertMmSelection hash UEQUAL count)  _
      (hasFileProperty hash UEQUAL count)           _
      (isLegitimateMode hash UEQUAL count)          _
      (isValidType hash UEQUAL count)               _
      (resolveTT hash UEQUAL count)                 _
      (selectMms1 hash UEQUAL count)                _
      (underDomainOf hash UEQUAL count))

  ++
  $compUniquelyIfTrue: %Boolean := false

  ++
  $ConstructorCache: %HashTable := MAKE_-HASHTABLE "ID"

  ++
  $currentFunction: %Thing := nil

  ++
  $currentLine: %Thing := nil

  ++
  $exitModeStack: %List := []

  ++
  $inputPromptType: %Symbol := "step"

  ++
  $whereList: %List := []

  ++
  $warningStack: %List := []

  ++
  $form: %Form := nil

  ++
  $fromSpadTrace: %Boolean := false

  ++
  $genSDVar: %Short := 0

  ++
  $Index: %Short := 0

  ++
  $inLispVM: %Boolean := true

  ++
  $insideCapsuleFunctionIfTrue: %Boolean := false

  ++
  $insideCategoryIfTrue: %Boolean := false

  ++
  $insideCoerceInteractiveHardIfTrue: %Boolean := false

  ++
  $insideCompTypeOf: %Boolean := false

  ++
  $insideConstructIfTrue: %Boolean := false

  ++
  $insideExpressionIfTrue: %Boolean := false

  ++
  $insideFunctorIfTrue: %Boolean := false

  ++
  $insideWhereIfTrue: %Boolean := false

  ++
  $instantRecord: %HashTable := MAKE_-HASHTABLE "ID"

  ++
  $CapsuleModemapFrame: %Env := [[nil]]
  $InteractiveFrame: %Env := $EmptyEnvironment
  $e: %Env := $EmptyEnvironment
  $env: %Env := [[nil]]

  ++
  $InteractiveMode: %Boolean := false

  ++
  $InteractiveTimingStatsIfTrue: %Boolean := false

  ++
  $forceDatabaseUpdate: %Boolean := false

  ++
  $leaveLevelStack: %List := []

  ++
  ++ FIXME: eventually move to trace.boot.
  $letAssoc: %Boolean := false

  ++
  $libFile: %Maybe %String := nil

  ++
  $lisplibForm: %Form := nil

  ++
  $lisplibKind: %Maybe %Symbol := nil

  ++
  $lisplibModemapAlist: %List := []

  ++
  $lisplibModemap: %Modemap := nil

  ++
  $lisplibOperationAlist: %List := []

  ++
  $lisplibSignatureAlist: %List := []

  ++
  $lisplibVariableAlist: %List := []

  ++
  $mapSubNameAlist: %List := []

  ++
  $mathTrace: %Boolean := false

  ++
  $mathTraceList: %List := []

  ++
  $prefix: %Maybe %Symbol := nil

  ++ FIXME: Eventually move to comp.lisp.pamphlet
  $PrettyPrint: %Boolean := false

  ++
  $previousTime: %Short := 0

  ++
  $VariableCount: %Short := 0

  ++
  $useBFasDefault: %Boolean := true

  ++
  $semanticErrorStack: %List := []


  ++
  $tracedModemap: %Boolean := nil

  ++
  $tracedSpadModemap: %List := nil

  ++
  $traceletFunctions: %List := []

  ++
  $useDCQnotLET: %Boolean := false

  ++
  $updateCatTableIfTrue: %Boolean := true

  ++
  $TranslateOnly: %Boolean := false

  ++
  $topOp: %Symbol := nil

  ++
  $streamCount: %Short := 0

  ++
  $TOP__LEVEL: %Boolean := true

  ++
  $TOKSTACK: %List := nil

  ++
  $FUNCTION: %Thing := nil

  ++
  $FUNNAME: %Symbol := nil

  ++
  $FUNNAME__TAIL: %List := '(())

  ++
  $LASTPREFIX: %String := '"S_:"

  ++
  $LINESTACK: %Symbol := "BEGIN__UNIT"

  ++
  $MAXLINENUMBER: %Short := 0

  ++
  $OLDLINE: %Maybe %String := nil

  ++
  $SPAD: %Boolean := false

  ++
  $PrintOnly: %Boolean := false

  ++
  $QuickLet: %Boolean := true

  ++
  $reportBottomUpFlag: %Boolean := false

  ++
  $reportFlag: %Boolean := false

  ++
  $returnMode: %Mode := $EmptyMode

  ++
  $SetFunctions: %Maybe %Shell := nil

  ++
  ++ FIXME: Eventually remove.
  $slamFlag: %Boolean := false

  ++
  ++ FIXME: Eventually remove.
  $sourceFileTypes: %List := ["SPAD"]

  ++
  ++ If true, make the system verbose about object files being loaded
  $printLoadMsgs: %Boolean := false

  ++
  $reportCompilation: %Boolean := false

  ++
  $LISPLIB: %Thing := false

  ++
  $CategoryFrame: %Env :=
    '((((Category (modemap (((Category) (Category)) (T *))))_
	(Join (modemap (((Category) (Category) (Category)) (T *))_
		       (((Category) (Category) (List Category)) (T *)))))))

  ++
  $spadLibFT: %Symbol := "NRLIB"

  ++ true if we are compiling a function.
  $compilingMap: %Boolean := false

  ++
  $insideCoerceInteractive: %Boolean := false

  ++
  $insideEvalMmCondIfTrue: %Boolean := false

  ++
  $libraryDirectory: %Symbol := "A"

  ++
  $listingDirectory: %Symbol := "A"

  ++
  $UserLevel: %Symbol := "development"

  ++
  $DIRECTORY_-LIST: %List := []

  ++
  $LIBRARY_-DIRECTORY_-LIST: %List := []

  ++
  $byConstructors: %List := nil

  ++
  $constructorsSeen: %List := nil

  ++
  $docList: %List := []

  ++
  $headerDocumentation: %Thing := nil

  ++
  $constructorLineNumber: %Short := 0

  ++
  $maxSignatureLineNumber: %Short := 0

  ++
  $noSubsumption: %Boolean :=true

  SPADERRORSTREAM: %Stream := _*ERROR_-OUTPUT_*

  ++
  _/VERSION: %Short := 0
  _/WSNAME: %Symbol := "NOBOOT"
  _/EDITFILE: %Maybe %String := nil

  ++
  LINE: %Maybe %String := nil
  CHR: %Maybe %Char := nil
  TOK: %Thing := nil

  ++ answers x has y category questions
  _*HASCATEGORY_-HASH_*: %HashTable := nil

  _*ANCESTORS_-HASH_*: %HashTable := nil

  ++
  _*BUILD_-VERSION_*: %Maybe %String := nil
  _*YEARWEEK_*: %Maybe %String := nil

  ++
  _/TRACENAMES: %List := nil

  ++
  $highlightAllowed: %Boolean := true

  ++
  INPUT_-LIBRARIES: %List := nil
  OUTPUT_-LIBRARY: %Thing := nil

  ++
  $newConlist: %List := nil

  ++
  $compilingInputFile: %Boolean := false

  ++
  $minivectorNames: %List := []

  ++ True if the input file uses old semantics of `Rep',
  ++ e.g. implicit equivalent Rep <-> % with capsules.  
  ++ This semenatics is in effect only when `Rep' is defined
  ++ through assignment.
  $useRepresentationHack: %Boolean := true

  ++
  $insideCanCoerceFrom: %Boolean := nil

  ++
  $sourceFiles: %List := []

  ++ ???
  $x: %Form := nil
  $f: %Form := nil
  $m: %Mode := nil

  ++ ???
  _/SOURCEFILES: %List := []
  _/SPACELIST: %List := []

  --%
  $algebraOutputStream: %Stream :=
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"

  ++
  $texOutputStream: %Stream := 
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"

  $fortranOutputStream: %Stream :=
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"

  $formulaOutputStream: %Stream :=
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"

  conOutStream: %Stream := 
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"




  $InputStream: %Stream :=
    MAKE_-SYNONYM_-STREAM "*STANDARD-INPUT*"

  $OutputStream: %Stream :=
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"

  $ErrorStream: %Stream :=
    MAKE_-SYNONYM_-STREAM "*STANDARD-OUTPUT*"
