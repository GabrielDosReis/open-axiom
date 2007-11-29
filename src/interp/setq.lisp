;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007, Gabriel Dos Reis.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     - Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(setq copyrights '(
 "Copyright The Numerical Algorithms Group Limited 1991-94."
 "All rights reserved"
 "Certain derivative-work portions Copyright (C) 1998 by Leslie Lamport."
 "Portions (c) Copyright Taiichi Yuasa and Masami Hagiya, 1984."
 "All rights reserved"))

(import-module "macros")
(in-package "BOOT")

(SETQ |/MAJOR-VERSION| 7)
(SETQ /RELEASE 0)

(defconstant |$cclSystem|
#+:CCL 't
#-:CCL nil
)

;; These two variables are referred to in setvars.boot.
#+:kcl (setq input-libraries nil)
#+:kcl (setq output-library nil)

;; For the browser, used for building local databases when a user compiles
;; their own code.
(SETQ |$newConstructorList| nil)
(SETQ |$newConlist| nil)
(SETQ |$createLocalLibDb| 't)


;; These were originally in SPAD LISP

(SETQ $BOOT NIL)
(setq |$interpOnly| nil)
(SETQ |$testingSystem| NIL)
(SETQ |$publicSystem| NIL)
(SETQ |$newcompMode| NIL)
(SETQ |$newComp| NIL)
(SETQ |$newCompCompare| NIL)
(SETQ |$permitWhere| NIL)
(SETQ |$newSystem| T)
(SETQ |$compileDontDefineFunctions| 'T)
(SETQ |$compileOnlyCertainItems| NIL)
(SETQ |$devaluateList| NIL)
(SETQ |$doNotCompressHashTableIfTrue| NIL)
(SETQ |$mutableChecking| NIL)    ; used in DEFINE BOOT
(SETQ |$mutableDomains| NIL)     ; checked in DEFINE BOOT
(SETQ |$functionLocations| NIL)
(SETQ |$functorLocalParameters| NIL) ; used in compSymbol
(SETQ /RELEASE '"UNKNOWN")
(SETQ |$insideCategoryPackageIfTrue| NIL)
(SETQ |$insideCompileBodyIfTrue| NIL)
(SETQ |$globalExposureGroupAlist| NIL)
(SETQ |$localExposureDataDefault|
  (VECTOR (LIST '|basic| '|categories|) NIL NIL))
(SETQ |$localExposureData|
  (VECTOR (LIST '|basic| '|categories|) NIL NIL))
(SETQ |$compilingInputFile| NIL)
(SETQ |$minivectorNames| NIL)
(setq |$ReadingFile| NIL)
(setq |$NonNullStream| "NonNullStream")
(setq |$NullStream| "NullStream")
(setq |$domPvar| nil)
(defvar $dalymode nil "if true then leading paren implies lisp cmd")
(setq |$Newline| #\Newline)


(SETQ STAKCOLUMN -1)
(SETQ ECHOMETA NIL)
(SETQ |$checkParseIfTrue| 'NIL)
(SETQ |$oldParserExpandAbbrs| NIL)
(SETQ |S:SPADKEY| NIL) ;" this is augmented by MAKESPADOP"
(SETQ |/EDIT,FT| 'SPAD)
(SETQ |/EDIT,FM| 'A)
(SETQ INITCOLUMN 0)
(SETQ |$functionTable| NIL)
(SETQ |$spaddefs| NIL)
(SETQ |$xeditIsConsole|  NIL)
(SETQ |$echoInputLines|  NIL)       ;; This is in SETVART also
(SETQ |$Slot1DataBase| (MAKE-HASHTABLE 'ID))  ;; See NRUNTIME BOOT
(SETQ |$pfKeysForBrowse|  NIL)
(SETQ MARG 0)
  ;" Margin for testing by ?OP"
(SETQ LCTRUE '|true|)
(SETQ |$displayParserOutput| 'T)

(SETQ |$insideReadRulesIfTrue| NIL)
(SETQ |$consistencyCheck| 'T)
(SETQ |$useUndo| NIL)
(SETQ |$ruleSetsInitialized| NIL)

;; tell the system not to use the new parser
(SETQ |$useNewParser| NIL)

(SETQ |$htPrecedenceTable| NIL)

(SETQ |$NRTmakeCompactDirect| NIL)
(SETQ |$NRTquick| NIL)
(SETQ |$NRTmakeShortDirect| NIL)
(SETQ |$newWorld| NIL)
(SETQ |$returnNowhereFromGoGet| NIL)

(SETQ |$insideCanCoerceFrom| NIL)

(SETQ |$useCoerceOrCroak| T)

(SETQ |$abbreviateJoin| NIL)

(SETQ |$InterpreterMacroAlist|
      '((|%i| . (|complex| 0 1))
        (|%e| . (|exp| 1))
        (|%pi| . (|pi|))
        (|SF| . (|DoubleFloat|))
        (|%infinity| . (|infinity|))
        (|%plusInfinity| . (|plusInfinity|))
        (|%minusInfinity| . (|minusInfinity|))))

;; variables controlling companion pages (see copage.boot)
(SETQ |$HTCompanionWindowID| nil)
(SETQ |$HTPreviousDomain| nil)
(SETQ |$HTOperationError| nil)

;; Common lisp control variables
;;(setq *load-verbose* nil)
(setq *print-array* nil)
(setq *print-pretty* nil)
(setq *print-circle* nil)

(SETQ |S:SPADTOK| 'SPADSYSTOK)
(SETQ APLMODE NIL)
(SETQ RLGENSYMFG NIL)
(SETQ RLGENSYMLST NIL)
(SETQ XTOKENREADER 'SPADTOK)
(SETQ |$delimiterTokenList|
  '(| |  |)| |(| |{| |}| |[| |]| ENDOFLINECHR EOI EOL |END_LINE|))
(SETQ |$generalTokenIfTrue| NIL)
(SETQ OPASSOC NIL)
(SETQ SPADSYSKEY '(EOI EOL))

;; These are for the output routines in OUT BOOT

(SETQ $LINELENGTH 77)
(SETQ $MARGIN 3)
(SETQ *TALLPAR NIL)
(SETQ ALLSTAR NIL)
(SETQ COLON ":")
(SETQ COMMA ",")
(SETQ DASH "-")
(SETQ DOLLAR "$")
(SETQ EQSIGN "=")
(SETQ LPAR "(")
(SETQ MATBORCH "*")
(SETQ PERIOD ".")
(SETQ PLUSS "+")
(SETQ RPAR ")")
(SETQ SLASH "/")
(SETQ STAR "*")
(SETQ |$fortranArrayStartingIndex| 0)

;; These were originally in INIT LISP

(SETQ |$dependeeClosureAlist|       NIL)
(SETQ |$userModemaps| NIL)
(SETQ |$functorForm| NIL)

(SETQ |$InitialCommandSynonymAlist| '(
       (|?|          . "what commands")
       (|ap|         . "what things")
       (|apr|        . "what things")
       (|apropos|    . "what things")
       (|cache|      . "set functions cache")
       (|cl|         . "clear")
       (|cls|        . "zsystemdevelopment )cls")
       (|cms|        . "system")
       (|co|         . "compiler")
       (|d|          . "display")
       (|dep|        . "display dependents")
       (|dependents| . "display dependents")
       (|e|          . "edit")
       (|expose|     . "set expose add constructor")
       (|fc|         . "zsystemdevelopment )c")
       (|fd|         . "zsystemdevelopment )d")
       (|fdt|        . "zsystemdevelopment )dt")
       (|fct|        . "zsystemdevelopment )ct")
       (|fctl|       . "zsystemdevelopment )ctl")
       (|fe|         . "zsystemdevelopment )e")
       (|fec|        . "zsystemdevelopment )ec")
       (|fect|       . "zsystemdevelopment )ect")
       (|fns|        . "exec spadfn")
       (|fortran|    . "set output fortran")
       (|h|          . "help")
       (|hd|         . "system hypertex &")
       (|kclam|      . "boot clearClams ( )")
       (|killcaches| . "boot clearConstructorAndLisplibCaches ( )")
       (|patch|      . "zsystemdevelopment )patch")
       (|pause|      . "zsystemdevelopment )pause")
       (|prompt|     . "set message prompt")
       (|recurrence| . "set functions recurrence")
       (|restore|    . "history )restore")
       (|save|       . "history )save")
       (|startGraphics|    .  "system $AXIOM/lib/viewman &")
       (|stopGraphics|     .  "lisp (|sockSendSignal| 2 15)")
       (|time|       . "set message time")
       (|type|       . "set message type")
       (|unexpose|   . "set expose drop constructor")
       (|up|         . "zsystemdevelopment )update")
       (|version|    . "lisp *yearweek*")
       (|w|          . "what")
       (|wc|         . "what categories")
       (|wd|         . "what domains")
       (|who|        . "lisp (pprint credits)")
       (|wp|         . "what packages")
       (|ws|         . "what synonyms")
))

(SETQ |$CommandSynonymAlist| (COPY |$InitialCommandSynonymAlist|))

(SETQ |$existingFiles| (MAKE-HASHTABLE 'UEQUAL))

(SETQ |$instantRecord| (MAKE-HASHTABLE 'ID))
(SETQ |$immediateDataSymbol| '|--immediateData--|)

(SETQ |$useIntegerSubdomain| 'T)
(SETQ |$useNewFloat| 'T)

;; the following symbol holds the canonical "failed" value
(SETQ |$failed| "failed")

(SETQ |$constructorDataTable| NIL)

(SETQ |$univariateDomains| '(
    |UnivariatePolynomial|
    |UnivariateTaylorSeries|
    |UnivariateLaurentSeries|
    |UnivariatePuiseuxSeries|
    ))
(SETQ |$multivariateDomains| '(
    |MultivariatePolynomial|
    |DistributedMultivariatePolynomial|
    |HomogeneousDistributedMultivariatePolynomial|
    |GeneralDistributedMultivariatePolynomial|
    ))

(SETQ |$DomainsWithoutLisplibs| '(
  CAPSULE |Union| |Record| |SubDomain| |Mapping| |Enumeration| |Domain| |Mode|))

(SETQ |$tracedMapSignatures| ())
(SETQ |$highlightAllowed| 'T)
         ;" used in BRIGHTPRINT and is a )set variable"

(SETQ |$printStorageIfTrue| NIL) ;; storage info disabled in common lisp

(SETQ |$AnonymousFunction| '(|AnonymousFunction|))
(SETQ |$Any|   '(|Any|))

(SETQ |$OutputForm| '(|OutputForm|))

(SETQ |$ComplexInteger| (LIST '|Complex| |$Integer|))
(SETQ |$QuotientField| '|Fraction|)
(SETQ |$FunctionalExpression| '|Expression|)
(SETQ |$defaultFunctionTargets| '(()))

;; New Names
(SETQ |$SingleInteger| '(|SingleInteger|))

(SETQ $NE (LIST (LIST NIL)))
(SETQ |$suffix| NIL)
(SETQ |$coerceIntByMapCounter| 0)
(SETQ |$prefix| NIL)
(SETQ |$formalArgList| ())
(SETQ |$TriangleVariableList|
   '(|t#1| |t#2| |t#3| |t#4| |t#5| |t#6| |t#7| |t#8| |t#9| |t#10|
     |t#11| |t#12| |t#13| |t#14| |t#15| |t#16| |t#17| |t#18| |t#19| |t#20|
     |t#21| |t#22| |t#23| |t#24| |t#25| |t#26| |t#27| |t#28| |t#29| |t#30|
     |t#31| |t#32| |t#33| |t#34| |t#35| |t#36| |t#37| |t#38| |t#39| |t#40|
     |t#41| |t#42| |t#43| |t#44| |t#45| |t#46| |t#47| |t#48| |t#49| |t#50|))

(SETQ |$NRTflag| T)
(SETQ |$NRTaddForm| NIL)
(SETQ |$NRTdeltaList| NIL)
(SETQ |$NRTbase| 0)
(SETQ |$NRTdeltaLength| 0)
(SETQ |$NRTopt| NIL) ;; turns off buggy code
(SETQ |$Slot1DataBase| NIL)
(SETQ |$NRTmonitorIfTrue| NIL)

(SETQ |$useConvertForCoercions| NIL)

(MAKEPROP '|One| '|defaultType| |$Integer|)
(MAKEPROP '|Zero| '|defaultType| |$Integer|)

;; Following were originally in EXPLORE BOOT

(SETQ |$xdatabase|         NIL)
(SETQ |$CatOfCatDatabase|  NIL)
(SETQ |$DomOfCatDatabase|  NIL)
(SETQ |$JoinOfDomDatabase| NIL)
(SETQ |$JoinOfCatDatabase| NIL)
(SETQ |$attributeDb|       NIL)

(SETQ |$abbreviateIfTrue|  NIL)
(SETQ |$deltax|  0)
(SETQ |$deltay|  0)
(SETQ |$displayDomains|  'T)
(SETQ |$displayTowardAncestors|  NIL)
(SETQ |$focus|  NIL)
(SETQ |$focusAccessPath|  NIL)
(SETQ |$minimumSeparation|  3)
(SETQ |$origMaxColumn|  80)
(SETQ |$origMaxRow|  20)
(SETQ |$origMinColumn|  1)
(SETQ |$origMinRow|  1)

;; ---- start of initial settings for variables used in test.boot

(SETQ |$testOutputLineFlag| NIL)   ;; referenced by charyTop, prnd
                                   ;; to stash lines
(SETQ |$testOutputLineStack| NIL)  ;; saves lines to be printed
                                   ;; (needed to convert lines for use
                                   ;; in hypertex)
(SETQ |$runTestFlag| NIL)          ;; referenced by maPrin to stash
                                   ;; output by recordAndPrint to not
                                   ;; print type/time
(SETQ |$mkTestFlag| NIL)           ;; referenced by READLN to stash input
                                   ;; by maPrin to stash output
                                   ;; by recordAndPrint to write i/o
                                   ;; onto $testStream
(SETQ |$mkTestInputStack| NIL)     ;; saves input for $testStream
                                   ;; (see READLN)
(SETQ |$mkTestOutputStack| NIL)    ;; saves output for $testStream
                                   ;; (see maPrin)

;; ---- end of initial settings for variables used in test.boot


;; Next are initial values for fluid variables in G-BOOT BOOT

(SETQ |$inDefLET| NIL)
(SETQ |$inDefIS|  NIL)
(SETQ |$letGenVarCounter| 1)
(SETQ |$isGenVarCounter|  1)

;; Next 2 lines originally from CLAM BOOT

;; this node is used in looking up values
(SETQ |$hashNode| (LIST NIL))

(SETQ ERRORINSTREAM (DEFIOSTREAM
    '((DEVICE . CONSOLE) (MODE . INPUT) (QUAL . T)) 133 1))

(SETQ ERROROUTSTREAM
  (DEFIOSTREAM '((DEVICE . CONSOLE)(MODE . OUTPUT)) 80 0) )

(SETQ |$algebraOutputStream|
  (DEFIOSTREAM '((DEVICE . CONSOLE)(MODE . OUTPUT)) 255 0) )

;; By default, don't generate info files with old compiler.
(setq |$profileCompiler| nil)


