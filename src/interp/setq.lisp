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

(SETQ /RELEASE 0)

;; These were originally in SPAD LISP

(SETQ |$mutableChecking| NIL)    ; used in DEFINE BOOT
(SETQ |$mutableDomains| NIL)     ; checked in DEFINE BOOT


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
(SETQ |$useUndo| NIL)
(SETQ |$ruleSetsInitialized| NIL)

;; tell the system not to use the new parser
(SETQ |$useNewParser| NIL)

(SETQ |$htPrecedenceTable| NIL)

(SETQ |$NRTquick| NIL)
(SETQ |$returnNowhereFromGoGet| NIL)

(SETQ |$insideCanCoerceFrom| NIL)

(SETQ |$useCoerceOrCroak| T)

(SETQ |$abbreviateJoin| NIL)

;; variables controlling companion pages (see copage.boot)
(SETQ |$HTCompanionWindowID| nil)
(SETQ |$HTPreviousDomain| nil)
(SETQ |$HTOperationError| nil)

(SETQ |S:SPADTOK| 'SPADSYSTOK)
(SETQ APLMODE NIL)
(SETQ RLGENSYMFG NIL)
(SETQ RLGENSYMLST NIL)
(SETQ XTOKENREADER 'SPADTOK)
(SETQ |$generalTokenIfTrue| NIL)
(SETQ OPASSOC NIL)
(SETQ SPADSYSKEY '(EOI EOL))

;; These are for the output routines in OUT BOOT

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

(SETQ |$existingFiles| (MAKE-HASHTABLE 'UEQUAL))

(SETQ |$instantRecord| (MAKE-HASHTABLE 'ID))

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

(SETQ |$printStorageIfTrue| NIL) ;; storage info disabled in common lisp

(SETQ |$defaultFunctionTargets| '(()))

(SETQ $NE (LIST (LIST NIL)))
(SETQ |$suffix| NIL)
(SETQ |$coerceIntByMapCounter| 0)
(SETQ |$prefix| NIL)
(SETQ |$formalArgList| ())

(SETQ |$NRTaddForm| NIL)
(SETQ |$NRTdeltaList| NIL)
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
