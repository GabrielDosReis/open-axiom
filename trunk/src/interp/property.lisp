;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2009, Gabriel Dos Reis.
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

(REPEAT (IN X '(
  |Polynomial| |UnivariatePoly| |SquareMatrix| |QuotientField|
  )) (MAKEPROP X '|status|
     (INTERNL (STRCONC "status" (STRINGIMAGE X))) ))

(REPEAT (IN X '(
  |UnivariatePoly| |Matrix| |QuotientField| |Gaussian|
  )) (MAKEPROP X '|dataCoerce|
     (INTERNL (STRCONC "coerce" (STRINGIMAGE X))) ))

(REPEAT (IN X '(
  (|Integer| . (INTEGERP |#1|))
  ;; (|Float| . (FLOATP |#1|))
  (|DoubleFloat| . (FLOATP |#1|))
  ;; (|Symbol| . (IDENTP |#1|))
  ;;(|Boolean| . (BOOLEANP |#1|))  worthless predicate is always true
  (|String| . (STRINGP |#1|))
  (|PrimitiveSymbol| . (IDENTP |#1|))
  )) (MAKEPROP (CAR X) '|BasicPredicate| (CDR X)))

(FLAG '(|Union| |Record| |Enumration| |Mapping| |Enumeration|) 'FUNCTOR)

(FLAG '(* + AND OR PROGN) 'NARY)

(MAKEPROP 'INTEGER 'ISFUNCTION 'FIXP)
(MAKEPROP '|Integer| '|isFunction| '|IsInteger|)
(MAKEPROP '|Boolean| '|isFunction| '|isBoolean|)

;; Many of the following are now in COMPAT LISP
(REPEAT (IN X '(
  (+ PLUS)
  (|and| AND)
  (|append| APPEND)
  (|apply| APPLY)
  (|atom| ATOM)
  (|brace| REMDUP)
  (|car| CAR)
  (|cdr| CDR)
  (|cons| CONS)
  (|copy| COPY)
  (|croak| CROAK)
  (|drop| DROP)
  (|exit| EXIT)
  (|false| NIL)
  (|first| CAR)
  (|genvar| GENVAR)
  (|in| |member|)
  (|is| IS)
  (|lastNode| LASTNODE)
  (|list| LIST)
  (|mkpf| MKPF)
  (|nconc| NCONC)
  (|nil| NIL)
  (|not| NULL)
  (|NOT| NULL)
  (|nreverse| NREVERSE)
  (|null| NULL)
  (|or| OR)
  (|otherwise| 'T)
  (|removeDuplicates| REMDUP)
  (|rest| CDR)
  (|return| RETURN)
  (|reverse| REVERSE)
  (|setDifference| SETDIFFERENCE)
  (|setIntersection| |intersection|)
  (|setPart| SETELT)
  (|setUnion| |union|)
  (|size| SIZE)
  (|strconc| STRCONC)
  (|substitute| MSUBST)
  (SUBST MSUBST)
  (|take| TAKE)
  (|true| 'T)
  (|where| WHERE)
  (* TIMES)
  (** EXPT)
  (^ NULL)
  (^= NEQUAL)
  (- SPADDIFFERENCE)
  (/ QUOTIENT)
  (= EQUAL)
  (ASSOC  |assoc|)
  (DELETE |delete|)
  (GET GETL)
  (INTERSECTION |intersection|)
  (LAST |last|)
  (MEMBER |member|)
  (RASSOC |rassoc|)
  (READ VMREAD)
  (READ-LINE |read-line|)
  (REDUCE SPADREDUCE)
  (REMOVE |remove|)
  (\| SUCHTHAT)
  (T T$)
  (UNION |union|)
)) (MAKEPROP (CAR X) 'RENAME (CDR X)))

;; these are accessor names for fields in data structures. Thus one would
;; write datastructure.setName 
(REPEAT (IN X '(
  (|setName|  0)
  (|setLabel| 1)
  (|setLevel| 2)
  (|setType|  3)
  (|setVar|   4)
  (|setLeaf|  5)
  (|setDef|   6)
  (|aGeneral| 4)
  (|aMode| 1)
  (|aModeSet| 3)
  (|aTree| 0)
  (|attributes| CADDR)
  (|aValue| 2)
  (|cacheCount| CADDDDR)
  (|cacheName| CADR)
  (|cacheReset| CADDDR)
  (|cacheType| CADDR)
  (|env| CADDR)
  (|expr| CAR)
  (|first| CAR)
  (|mmCondition| CAADR)
  (|mmDC| CAAR)
  (|mmImplementation| CADADR)
  (|mmSignature| CDAR)
  (|mmTarget| CADAR)
  (|mode| CADR)
  (|op| CAR)
  (|opcode| CADR)
  (|opSig| CADR)
  (|rest| CDR)
  (|sig| CDDR)
  (|source| CDR)
  (|streamCode| CADDDR)
  (|streamDef| CADDR)
  (|streamName| CADR)
  (|target| CAR)
)) (MAKEPROP (CAR X) '|SEL,FUNCTION| (CADR X)))


(REPEAT (IN X '(
  (\: |compColonInteractive|)
  (DEF |compDefineInteractive|)
  (|construct| |compConstructInteractive|)
  (%LET |compSetqInteractive|)
)) (MAKEPROP (CAR X) 'INTERACTIVE (CADR X)))

