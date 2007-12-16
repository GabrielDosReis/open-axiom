-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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
-- Copyright (C) 2007 Gabriel Dos Reis
--

--
-- This file collects and documents some the global variables used by either
-- the interpreter or the compiler or both.
--

import '"hash"
import '"sys-constants"
)package "BOOT"

++ FIXME
$saturn := false

++ FIXME
$SPAD__ERRORS := VECTOR(0, 0, 0)

++
$abbreviationTable := nil

++
$bootStrapMode := false

++
$BreakMode := "query"

++
$cacheAlist := nil
$cacheCount := 0


++
$clamList :=
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
$compCount := 0

++
$compUniquelyIfTrue := false

++
$ConstructorCache := MAKE_-HASHTABLE "ID"

++
$createUpdateFiles := false

++
$currentFunction := nil

++
$currentLine := nil

++
$domainTraceNameAssoc := []

++
$evalDomain := false

++
$exitModeStack := []

++
$IOindex := 0

++
$inputPromptType := "step"

++
$whereList := []

++
$warningStack := []

++
$form := nil

++
$fromSpadTrace := false

++
$genSDVar := 0

++
$Index := 0

++
$inLispVM := true

++
$insideCapsuleFunctionIfTrue := false

++
$insideCategoryIfTrue := false

++
$insideCoerceInteractiveHardIfTrue := false

++
$insideCompTypeOf := false

++
$insideConstructIfTrue := false

++
$insideExpressionIfTrue := false

++
$insideFunctorIfTrue := false

++
$insideWhereIfTrue := false

++
$instantRecord := MAKE_-HASHTABLE "ID"

++
$InteractiveFrame := [[nil]]

++
$InteractiveMode := false

++
$InteractiveTimingStatsIfTrue := false

++
$forceDatabaseUpdate := false

++
$leaveLevelStack := []

++
++ FIXME: eventually move to trace.boot.
$letAssoc := false

++
$libFile := nil

++
$lisplibForm := nil

++
$lisplibKind := nil

++
$lisplibModemapAlist := []

++
$lisplibModemap := nil

++
$lisplibOperationAlist := []

++
$lisplibSignatureAlist := []

++
$lisplibVariableAlist := []

++
$mapSubNameAlist := []

++
$mathTrace := false

++
$mathTraceList := []

++
$prefix := nil

++ FIXME: Eventually move to comp.lisp.pamphlet
$PrettyPrint := false

++
$previousTime := 0

++
$VariableCount := 0

++
++ FIXME: Eventually move to define.boot.pamphlet.
$suffix := 0

++
$useBFasDefault := true

++
$semanticErrorStack := []

++
++ FIXME: Eventually move to compiler.boot.pamphlet.
$reportExitModeStack := false

++
$tracedModemap := nil

++
$tracedSpadModemap := nil

++
$traceletFunctions := []

++
$useDCQnotLET := false

++
$updateCatTableIfTrue := true

++
$TranslateOnly := false

++
$topOp := nil

++
$streamCount := 0

++
$TOP__LEVEL := true

++
$TOKSTACK := nil

++
$FUNCTION := nil

++
$FUNNAME := nil

++
$FUNNAME__TAIL := '(())

++
$LASTPREFIX := '"S_:"

++
$LINESTACK := "BEGIN__UNIT"

++
$MAXLINENUMBER := 0

++
$OLDLINE := nil

++
$SPAD := false

++
$PrintOnly := false

++
$QuickCode := true

++
$QuickLet := true

++
$reportBottomUpFlag := false

++
$reportFlag := false

++
$returnMode := $EmptyMode

++
$SetFunctions := nil

++
++ FIXME: Eventually remove.
$slamFlag := false

++
++ FIXME: Eventually remove.
$sourceFileTypes := ["SPAD"]

++
++ If true, make the system verbose about object files being loaded
$printLoadMsgs := false

++
$reportCompilation := false

++
$LISPLIB := false

++
$CategoryFrame :=
  '((((Category (modemap (((Category) (Category)) (T *))))_
      (Join (modemap (((Category) (Category) (Category)) (T *))_
                     (((Category) (Category) (List Category)) (T *)))))))

++
$spadLibFT := "NRLIB"

++
$compilingMap := false

++
$definingMap := false

++
$TRACELETFLAG := false

++
$NEWSPAD := false

++
$BOOT := false

++
$insideCoerceInteractive := false

++
$insideEvalMmCondIfTrue := false

++
$libraryDirectory := "A"

++
$listingDirectory := "A"

++
$texOutputStream := MAKE_-SYNONYM_-STREAM '_*TERMINAL_-IO_*

++
$UserLevel := "development"

++
$DIRECTORY_-LIST := []

++
$LIBRARY_-DIRECTORY_-LIST := []

++
$byConstructors := nil

++
$constructorsSeen := nil

++
$docList := []

++
$headerDocumentation := nil

++
$constructorLineNumber := 0

++
$maxSignatureLineNumber := 0

++
$noSubsumption :=true

SPADERRORSTREAM := _*ERROR_-OUTPUT_*

++
_/VERSION := 0
_/WSNAME := "NOBOOT"
_/EDITFILE := nil

++
LINE := nil
CHR := nil
TOK := nil

++ answers x has y category questions
_*HASCATEGORY_-HASH_* := nil

_*ANCESTORS_-HASH_* := nil

++
_*BUILD_-VERSION_* := nil
_*YEARWEEK_* := nil

++
_/TRACENAMES := nil

++
$highlightAllowed := true

++
SETQ(_*PRINT_-CIRCLE_*, true)
SETQ(_*PRINT_-ARRAY_*, false)
SETQ(_*PRINT_-PRETTY_*, true)

++
INPUT_-LIBRARIES := nil
OUTPUT_-LIBRARY := nil

++
$newConlist := nil

++
$compilingInputFile := false

++
$minivectorNames := []
