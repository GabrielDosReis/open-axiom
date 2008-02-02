;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2008, Gabriel Dos Reis.
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


(import-module "macros")
(in-package "BOOT")

;; These were originally in SPAD LISP

(defvar $dalymode nil "if true then leading paren implies lisp cmd")
(defconstant |$Newline| #\Newline)


(defvar ECHOMETA NIL)
(defvar S-SPADKEY NIL) ;" this is augmented by MAKESPADOP"
(defvar MARG 0)
  ;" Margin for testing by ?OP"

(SETQ |$NRTmakeCompactDirect| NIL)
(SETQ |$NRTquick| NIL)
(SETQ |$NRTmakeShortDirect| NIL)
(SETQ |$newWorld| NIL)
(SETQ |$returnNowhereFromGoGet| NIL)

(SETQ |$abbreviateJoin| NIL)

(SETQ |S:SPADTOK| 'SPADSYSTOK)
(SETQ APLMODE NIL)
(SETQ RLGENSYMFG NIL)
(SETQ RLGENSYMLST NIL)
(SETQ XTOKENREADER 'SPADTOK)
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

;; These were originally in INIT LISP

(SETQ |$functorForm| NIL)

(SETQ $NE (LIST (LIST NIL)))
(SETQ |$suffix| NIL)
(SETQ |$coerceIntByMapCounter| 0)
(SETQ |$prefix| NIL)
(SETQ |$formalArgList| ())

(SETQ |$NRTflag| T)
(SETQ |$NRTaddForm| NIL)
(SETQ |$NRTdeltaList| NIL)
(SETQ |$NRTdeltaLength| 0)
(SETQ |$NRTopt| NIL) ;; turns off buggy code
(SETQ |$NRTmonitorIfTrue| NIL)

(SETQ |$useConvertForCoercions| NIL)

(MAKEPROP '|One| '|defaultType| |$Integer|)
(MAKEPROP '|Zero| '|defaultType| |$Integer|)

