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
(MAKEPROP 'LET '|Led| '(|:=| LET 125 124))
(MAKEPROP 'RARROW '|Led| '(== DEF 122 121))
(MAKEPROP 'SEGMENT '|Led| '(|..| SEGMENT 401 699 (|P:Seg|)))
(MAKEPROP 'SEGMENT '|isSuffix| 'T)
(MAKEPROP 'EQUAL1 'CHRYBNAM 'EQ)

(REPEAT (IN X '(
   (LET " := ")
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
   (|or| " or ")
   (TAG " -> ")
   (|+->| " +-> ")
   (RARROW ": ")
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
  (|not| "^ ")
  (\| " \| ")
  (SEGMENT "..")  ;" 0.. is represented by (SEGMENT 0)"
 )) (MAKEPROP (CAR X) 'PREFIXOP (CADR X)))

(REPEAT (IN X '(
  (+ WIDTH |sumWidth|)
  (- APP |appneg|)
  (- WIDTH |minusWidth|)
  (/ APP |appfrac|)
  (/ SUBSPAN |fracsub|)
  (/ SUPERSPAN |fracsuper|)
  (/ WIDTH |fracwidth|)
  (AGGSET APP |argsapp|)
  (AGGSET SUBSPAN |agggsub|)
  (AGGSET SUPERSPAN |agggsuper|)
  (AGGSET WIDTH |agggwidth|)
  (|binom| APP |binomApp|)
  (|binom| SUBSPAN |binomSub|)
  (|binom| SUPERSPAN |binomSuper|)
  (|binom| WIDTH |binomWidth|)
  (ALTSUPERSUB APP       |altSuperSubApp|)
  (ALTSUPERSUB SUBSPAN   |altSuperSubSub|)
  (ALTSUPERSUB SUPERSPAN |altSuperSubSuper|)
  (ALTSUPERSUB WIDTH     |altSuperSubWidth|)
  (BOX APP |boxApp|)
  (BOX SUBSPAN |boxSub|)
  (BOX SUPERSPAN |boxSuper|)
  (BOX WIDTH |boxWidth|)
  (BRACKET SUBSPAN |qTSub|)
  (BRACKET SUPERSPAN |qTSuper|)
  (BRACKET WIDTH |qTWidth|)
  (CENTER APP |centerApp|)
  (EXT APP |appext|)
  (EXT SUBSPAN |extsub|)
  (EXT SUPERSPAN |extsuper|)
  (EXT WIDTH |extwidth|)
  (MATRIX APP |appmat|)
  (MATRIX SUBSPAN |matSub|)
  (MATRIX SUPERSPAN |matSuper|)
  (MATRIX WIDTH |matWidth|)
  (NOTHING APP       |nothingApp|)
  (NOTHING SUPERSPAN |nothingSuper|)
  (NOTHING SUBSPAN   |nothingSub|)
  (NOTHING WIDTH     |nothingWidth|)
  (OVER APP |appfrac|)
  (OVER SUBSPAN |fracsub|)
  (OVER SUPERSPAN |fracsuper|)
  (OVER WIDTH |fracwidth|)
  (OVERLABEL APP |overlabelApp|)
  (OVERLABEL SUPERSPAN |overlabelSuper|)
  (OVERLABEL WIDTH |overlabelWidth|)
  (OVERBAR APP |overbarApp|)
  (OVERBAR SUPERSPAN |overbarSuper|)
  (OVERBAR WIDTH |overbarWidth|)
  (PAREN APP |appparu1|)
  (PAREN SUBSPAN |qTSub|)
  (PAREN SUPERSPAN |qTSuper|)
  (PAREN WIDTH |qTWidth|)
  (ROOT APP       |rootApp|)
  (ROOT SUBSPAN   |rootSub|)
  (ROOT SUPERSPAN |rootSuper|)
  (ROOT WIDTH     |rootWidth|)
  (ROW WIDTH |eq0|)
  (SC APP |appsc|)
  (SC SUBSPAN |agggsub|)
  (SC SUPERSPAN |agggsuper|)
  (SC WIDTH |widthSC|)
  (SETQ APP |appsetq|)
  (SETQ WIDTH |letWidth|)
  (SLASH APP       |slashApp|)
  (SLASH SUBSPAN   |slashSub|)
  (SLASH SUPERSPAN |slashSuper|)
  (SLASH WIDTH     |slashWidth|)
  (SUB APP |appsub|)
  (SUB SUBSPAN |subSub|)
  (SUB SUPERSPAN |subSuper|)
  (SUB WIDTH |suScWidth|)
  (SUPERSUB APP |superSubApp|)
  (SUPERSUB SUBSPAN |superSubSub|)
  (SUPERSUB SUPERSPAN |superSubSuper|)
  (SUPERSUB WIDTH |superSubWidth|)
  (VCONCAT APP |vconcatapp|)
  (VCONCAT SUBSPAN |vConcatSub|)
  (VCONCAT SUPERSPAN |vConcatSuper|)
  (VCONCAT WIDTH |vConcatWidth|)
  (BINOMIAL APP |binomialApp|)
  (BINOMIAL SUBSPAN |binomialSub|)
  (BINOMIAL SUPERSPAN |binomialSuper|)
  (BINOMIAL WIDTH |binomialWidth|)
  (ZAG APP |zagApp|)
  (ZAG SUBSPAN |zagSub|)
  (ZAG SUPERSPAN |zagSuper|)
  (ZAG WIDTH |zagWidth|)
)) (PROGN (MAKEPROP (CAR X) (CADR X) (CADDR X)))
)

(REPEAT (IN X '(
  (+ APP |plusApp|)
  (* APP |timesApp|)
  (* WIDTH |timesWidth|)
  (** APP |exptApp|)
  (** WIDTH |exptWidth|)
  (** SUBSPAN |exptSub|)
  (** SUPERSPAN |exptSuper|)
  (^  APP |exptApp|)
  (^  WIDTH |exptWidth|)
  (^  SUBSPAN |exptSub|)
  (^  SUPERSPAN |exptSuper|)
  (STEP APP |stepApp|)
  (STEP WIDTH |stepWidth|)
  (STEP SUBSPAN |stepSub|)
  (STEP SUPERSPAN |stepSuper|)
  (IN APP |inApp|)
  (IN WIDTH |inWidth|)
  (IN SUBSPAN |inSub|)
  (IN SUPERSPAN |inSuper|)
  (AGGLST APP |aggApp|)
  (AGGLST SUBSPAN |aggSub|)
  (AGGLST SUPERSPAN |aggSuper|)
  (CONCATB APP |concatbApp|)
  (CONCATB SUBSPAN |concatSub|)
  (CONCATB SUPERSPAN |concatSuper|)
  (CONCATB WIDTH |concatbWidth|)
  (CONCAT APP |concatApp|)
  (CONCAT  SUBSPAN |concatSub|)
  (CONCAT SUPERSPAN |concatSuper|)
  (CONCAT WIDTH |concatWidth|)
  (QUOTE APP |quoteApp|)
  (QUOTE SUBSPAN |quoteSub|)
  (QUOTE SUPERSPAN |quoteSuper|)
  (QUOTE WIDTH |quoteWidth|)
  (STRING APP |stringApp|)
  (STRING SUBSPAN |eq0|)
  (STRING SUPERSPAN |eq0|)
  (STRING WIDTH |stringWidth|)
  (SIGMA APP |sigmaApp|)
  (SIGMA SUBSPAN |sigmaSub|)
  (SIGMA SUPERSPAN |sigmaSup|)
  (SIGMA WIDTH |sigmaWidth|)
  (SIGMA2 APP |sigma2App|)
  (SIGMA2 SUBSPAN |sigma2Sub|)
  (SIGMA2 SUPERSPAN |sigma2Sup|)
  (SIGMA2 WIDTH |sigma2Width|)
  (INTSIGN APP |intApp|)
  (INTSIGN SUBSPAN |intSub|)
  (INTSIGN SUPERSPAN |intSup|)
  (INTSIGN WIDTH |intWidth|)
  (INDEFINTEGRAL APP |indefIntegralApp|)
  (INDEFINTEGRAL SUBSPAN |indefIntegralSub|)
  (INDEFINTEGRAL SUPERSPAN |indefIntegralSup|)
  (INDEFINTEGRAL WIDTH |indefIntegralWidth|)
  (PI APP |piApp|)
  (PI SUBSPAN |piSub|)
  (PI SUPERSPAN |piSup|)
  (PI WIDTH |piWidth|)
  (PI2 APP |pi2App|)
  (PI2 SUBSPAN |pi2Sub|)
  (PI2 SUPERSPAN |pi2Sup|)
  (PI2 WIDTH |pi2Width|)
  (AGGLST WIDTH |aggWidth|)
  (BRACKET APP |bracketApp|)
  (BRACE APP |braceApp|)
  (BRACE WIDTH |qTWidth|)
)) (PROGN (MAKEPROP (CAR X) (CADR X) (CADDR X)))
)

;; from DEF LISP

(REPEAT (IN X '(
  (|:| |DEF-:|)
  (|::| |DEF-::|)
  (ELT DEF-ELT)
  (SETELT DEF-SETELT)
  (LET DEF-LET)
  (COLLECT DEF-COLLECT)
  (LESSP DEF-LESSP)
  (|<| DEF-LESSP)
  (REPEAT DEF-REPEAT)
;;(|TRACE,LET| DEF-TRACE-LET)
  (CATEGORY DEF-CATEGORY)
  (EQUAL DEF-EQUAL)
  (|is| DEF-IS)
  (SEQ DEF-SEQ)
  (|isnt| DEF-ISNT)
  (|where| DEF-WHERE)
)) (PROGN (MAKEPROP (CAR X) '|DEF-TRAN| (CADR X)) (CADR X)))

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

(MAKEPROP '|Integer| '|Subsets|
  '((|PositiveInteger| . (|>| * 0))
    (|NonNegativeInteger| . (|>=| * 0))
    (|NegativeInteger| . (|<| * 0))
    (|NonPositiveInteger| . (|<=| * 0))
    (|NonZeroInteger| . (^= * 0))
    (|SingleInteger| . (SMINTP *))
    ))

(MAKEPROP '|NonNegativeInteger| '|Subsets| '(
  (|PositiveInteger| . (|>| * 0))
  ))

(MAKEPROP '|NonPositiveInteger| '|Subsets| '(
  (|NegativeInteger| . (|<| * 0))
  ))

(FLAG '(|Union| |Record| |Enumration| |Mapping| |Enumeration|) 'FUNCTOR)

(FLAG '(* + AND OR PROGN) 'NARY)

(REPEAT (IN X '(
  (|Record| |mkRecordFunList|)
  (|Union| |mkUnionFunList|)
  (|Mapping| |mkMappingFunList|)
  (|Enumeration| |mkEnumerationFunList|)
)) (MAKEPROP (CAR X) '|makeFunctionList| (CADR X)))

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
  (LET |compSetqInteractive|)
)) (MAKEPROP (CAR X) 'INTERACTIVE (CADR X)))

