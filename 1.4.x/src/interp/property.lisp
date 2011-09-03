;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2011, Gabriel Dos Reis.
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


;; This file contains most of the code that puts properties on
;; identifiers in the Scratchpad II system.  If it was not possible
;; to actually put the code here, we have pointers to where such
;; property list manipulation is being done.

;; Pointers:
;; o  see NEWAUX LISP for some code that puts GENERIC and RENAMETOK
;;    properties on identifiers for the parser
;; o  coerceIntCommute puts the "commute" property on constructors.
;; o  coerceRetract puts the "retract" property on constructors.
;; o  there is some code at the end of SPECEVAL BOOT that puts "up"
;;    properties on some special handlers.


(import-module "sys-macros")
(in-package "BOOT")

;; following was in NEWSPAD LISP

(MAKEPROP 'END_UNIT 'KEY 'T)

;; following was in OUTINIT LISP

(MAKEPROP 'TAG 'Led '(TAG TAG 122 121))
(MAKEPROP 'EQUATNUM '|Nud| '(|dummy| |dummy| 0 0))
(MAKEPROP 'EQUATNUM '|Led| '(|dummy| |dummy| 10000 0))
(MAKEPROP '%LET '|Led| '(|:=| %LET 125 124))
(MAKEPROP 'RARROW '|Led| '(== DEF 122 121))
(MAKEPROP 'SEGMENT '|Led| '(|..| SEGMENT 401 699 (|P:Seg|)))
(MAKEPROP 'SEGMENT '|isSuffix| 'T)
(MAKEPROP 'EQUAL1 'CHRYBNAM 'EQ)

(REPEAT (IN X '(
   (%LET " := ")
   (= "=")
   (|/| "/")
   (+ "+")
   (* "*")
   (** "**")
   (^ "^")
   (|:| ":")
   (|::| "::")
   (|@| "@")
   (SEL ".")
   (|exquo| " exquo ")
   (|div| " div ")
   (|quo| " quo ")
   (|rem| " rem ")
   (|case| " case ")
   (|and| " and ")
   (|/\\| " /\\ ")
   (|or| " or ")
   (|\\/| " \\/ ")
   (TAG ": ")
   (|+->| " +-> ")
   (RARROW " -> ")
   (SEGMENT "..")
   (in " in ")
   (|^=|  "^=")
   (EL* ":")
   (JOIN " JOIN ")
   (EQUATNUM "  ")
   (IQUOTIENT "//")
   (= "= ")
   (|>=| " >= ")
   (|>| " > ")
   (|<=| " <= ")
   (|<| " < ")
   (\| " \| ")
   (+ " + ")
   (- " - ")
   (MEMBER " in ")
   (NMEMBER " nin ")
   (WHERE " WHERE ")
   (AT " AT ")
   (MAX " MAX ")
   (MIN " MIN ")
       )) (MAKEPROP (CAR X) 'INFIXOP (CADR X)))

(REPEAT (IN X '(
  (= "=")
  (|:| ":")
  (|not| "not ")
  (\| " \| ")
  (SEGMENT "..")  ;" 0.. is represented by (SEGMENT 0)"
 )) (MAKEPROP (CAR X) 'PREFIXOP (CADR X)))


;; following was in INIT LISP

(FLAG '(* + AND OR PROGN) 'NARY)
