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


import '"sys-macros"
)package "BOOT"

-- Variables to control whether old software calls the new compiler.
$ncConverse  :=    NIL
$newcompMode :=    NIL        -- )comp means new compiler.
$newComp     :=    true       -- Start workspace in new compiler.
 
-- Files used by the compiler.
$erLocMsgDatabaseName     := pathname '(co_-eng msgs a)
$erGlbMsgDatabaseName     := pathname '(co_-eng msgs i)
$LanguageConstantFileName := pathname '(stlang input _*)
$WorkspaceProfileName     := pathname '(spadprof input _*)
$OldLibraryDatabaseName   := pathname '(modemap database _*)

$SpadNcLibraryRelPath     := '"lib/lang"         
$SpadNcLibraryRelPathSrc  := '"src/lib/lang/"         
$SpadNcIncludeRelPath     := '"src/include/lang/" 

--$LibrariesSearchPath      := [PathnameDirectory '"./x",
--                              SpadDirectory $SpadNcLibraryRelPath , 
--                            SpadDirectory $SpadNcLibraryRelPathSrc]

--$IncludesSearchPath       := [PathnameDirectory '"./x",
--                              SpadDirectory $SpadNcIncludeRelPath]

$warmstab := nil
 
-- Variables to control phases and their output
 
$ncRead :=              true
$ncmRead :=             NIL
 
$ncParse :=             true
$ncmParse :=            NIL
 
$ncAbsck :=             true
$ncmAbsck :=            NIL
 
$ncMacro :=             true
$ncmMacro :=            NIL
 
$ncScope :=           false
$ncmScope :=            NIL
 
$ncAnalyze :=           true
$ncmSemantics :=        NIL
 
$ncInterpretSetr :=     false
 
$ncParseSetr :=       false
$ncmParseSetr :=        NIL
 
$ncGenerateSAM :=       true
$ncmSAM :=              NIL
$ncLastSamCode :=       NIL
 
$ncSamOptimize :=     false
$ncmSamOptimize :=      NIL
$ncLastOptimizedCode := NIL
 
$ncSamPack :=         false
$ncmSamPack :=          NIL
$ncLastPackedSam :=     NIL
 
$ncGenerateConcrete :=  true
$ncmConCode :=          NIL
$ncLastConcreteCode :=  NIL
 
$ncLibrary          :=  true
$ncmLibrary         :=  NIL
 
$ncGenerateMachine :=   true
$ncmCodeSize :=         NIL
$ncLastMachineCode :=   NIL
 
$ncInterpretSam   :=    false
$ncExecuteMachine :=    true
 
$ncReportStep :=        true
 
-- Variables to control debugging output
--they are manipulated in setvart boot
$debugApply :=        false   -- trace application matching
$debugApply0 :=       false   -- trace even more
$debugSemAnalyze :=   false   -- trace results of semAnalyze
$debugRead :=         false
$debugParse :=        false
$debugCheck :=        false
$debugMacro :=        false
$debugScope :=        false
$debugParseSetr :=    false
$debugGenSam :=       false
$debugSamOpt :=       false
$debugSamPack :=      false
$debugGenCon :=       false
$debugGenMach :=      false
$debugExecute :=      false
$debugReport :=       false
 
-- Variables to control what other parts of the compiler are executed.
$ncDoSpecialCases :=  true
$LispViaSam :=        false
 
-- Variables to control other compiler output.
 
-- note flags to control the error message facility must have
-- the prefix $ncm, since catExcpts (in ncsetvar boot) strips the
-- prefix and uses the name.  ie. $ncmWarning ==> "Warning"
$ncmPhase :=      NIL
$ncmWarning :=    "T"
$ncmStatistic :=  NIL
$ncmRemark :=     "T"
$statTmSpShow :=   4
$compBugPrefix :=      '"Bug!"
$compUnimplPrefix :=   '"Unimp"
$compDebugPrefix :=    '"Debug"
$compStatisticPrefix :='"Stat"
$compErrorPrefix :=    '"Error"
$compWarningPrefix :=  '"Warn"
$compRemarkPrefix :=   '"Note"
$compSayPrefix :=      '"Msg"

$charNumSymVector := NIL
 
-- Modes
$FullMode :=          'FullMode
$ValueMode :=         'ValueMode
 
--error message facility
$nopos   := ['noposition]
$showKeyNum   :=        NIL
$specificMsgTags :=     NIL
 
--compiler option stuff
$ncCodeDebug := true
$ncCodeTrace := true
$ncSamInline := true
 
-- Variables used in the SEMantic ANAlysis
 
--from SEFO BOOT
$sefoDerivedAttributes := [ 'type, 'tfinfo, 'signature, 'pooled ]
 
--from NCMODE BOOT
$ValueMode   := 'ValueMode
$FullMode    := 'FullMode
 
-- Miscellaneous nonsense.
$newcompInteractiveRecovery := "T"
$newcompErrorCount :=           0
$floatdolla :=        ['$elt, ['BigFloat], 'bigfloat]
$floatilla :=         [ 'elt, ['BigFloat], 'bigfloat]
$newcompStats :=      NIL
$newcompAbbrevType := true
$stabLibLevelNo :=    -1
$SyntheticSourcePosition := 'Synthetic
$Typeless :=          NIL
 
$catAbTab := '(
    ($ncmWarning     .   "warn" ) _
    ($ncmRemark      .   "rem"  ) _
    ($ncmStatistic   .   "stat" ) )
$phaseAbTab := '(
    (Reading             . "Rd" ) _
    (Parsing             . "Pa" ) _
    (Checking            . "Ck" ) _
    (Macroing            . "Ma" ) _
    (Scoping             . "Sc" ) _
    (Analyzing           . "An" ) _
    (Interpreting        . "In" ) _
    (ParseSetr           . "Ps" ) _
    (GeneratingSAM       . "Sg" ) _
    (SamOptimize         . "So" ) _
    (SamPack             . "Sp" ) _
    (GeneratingConcrete  . "Cg" ) _
    (GeneratingMachine   . "Mg" ) _
    (Executing           . "Ex" ) _
    (Reporting           . "Rp" ) )

-- Items from STATS BOOT
$LINELENGTH := 80
 
-- Items from MSG BOOT I
$showMsgCaller := nil  --## was F
$preLength := 11
$LOGLENGTH := $LINELENGTH - 6
$specificMsgTags := []
 
$imPrTagGuys := ['unimple, 'bug, 'debug, 'say, 'warn]
$toWhereGuys := ['fileOnly, 'screenOnly ]
$imPrGuys    := ['imPr]
$repGuys     := ['noRep, 'rep]
$attrCats    := ['$imPrGuys, '$toWhereGuys, '$repGuys]
 
 
-- Soon to be obsolete
$showConcrete1 := NIL
$showConcrete2 := NIL
$showPhases :=    "T"
$showSAM :=       NIL
$showform :=      NIL
$showsetr :=      NIL
$showval :=       "T"
$tafon :=         NIL
 
-- Inits for pseudo kaf files
--$CURRENT_-DIRECTORY := fileCurrentDirectory()
$DIRECTORY_-LIST    := []

--caching for inline code
$gotSam := nil
--$cachedInlineTable := EqTable()

--debugging variables for Simon.
$simon := nil
$ncmTLambdaDown := nil

$ncMsgList := nil
$oldLibraryInterface := nil  -- don't consider old library information.

--## Bug in RIOS version of KCL
NeedAtLeastOneFunctionInThisFile(x) == x
