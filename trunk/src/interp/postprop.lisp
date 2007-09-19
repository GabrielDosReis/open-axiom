;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


(IMPORT-MODULE "macros")
(in-package "BOOT")

(mapcar #'(lambda (x) (MAKEPROP (CAR X) '|special| (CADR X)))
        '((LET |compSetq|)      (|Join| |compJoin|)
          (|Record| |compCat|)
          (|Union| |compCat|)   (\: |compColon|)
          (\:\: |compCoerce|)   (CAPSULE |compCapsule|)
          (|has| |compHas|)     (|is| |compIs|)
          (|add| |compAdd|)     (CONS |compCons|)
          (IF |compIf|)         (|exit| |compExit|)
          (|return| |compReturn|)       (|leave| |compLeave|)
          (|elt| |compElt|)     (DEF |compDefine|)
          (MDEF |compMacro|)    (|SubsetCategory| |compSubsetCategory|)
          (|SubDomain| |compSubDomain|)
          (|case| |compCase|)   (|String| |compString|)
          (|RecordCategory| |compConstructorCategory|)
          (|ListCategory| |compConstructorCategory|)
          (|VectorCategory| |compConstructorCategory|)
          (|UnionCategory| |compConstructorCategory|)
          (CATEGORY |compCategory|)
          (COLLECT |compRepeatOrCollect|)
          (COLLECTV |compCollectV|)
          (REPEAT |compRepeatOrCollect|)
          (REDUCE |compReduce|) (|where| |compWhere|)
          (\| |compSuchthat|)   (|construct| |compConstruct|)
          (SEQ |compSeq|)       (SETQ |compSetq|)
          (VECTOR |compVector|)))

(mapcar #'(lambda (x) (MAKEPROP (CAR X) '|postTran| (second X)))
        '((|with| |postWith|)
          (|Scripts| |postScripts|)
          (/ |postSlash|)
          (|construct| |postConstruct|)
          (|Block| |postBlock|)
          (QUOTE |postQUOTE|)
          (COLLECT |postCollect|)
          (\:BF\: |postBigFloat|)
          (|in| |postin|)               ; the infix operator version of i
          (IN |postIn|)                 ; the iterator form of i
          (REPEAT |postRepeat|)
          (|TupleCollect| |postTupleCollect|)
          (|add| |postAdd|)
          (|Reduce| |postReduce|)
          (\, |postComma|)
          (\; |postSemiColon|)
          (|where| |postWhere|)
          (\: |postColon|)
          (\@ |postAtSign|)
          (|pretend| |postPretend|)
          (|if| |postIf|)
          (|Join| |postJoin|)
          (|Signature| |postSignature|)
          (CATEGORY |postCategory|)
          (== |postDef|)
          (==> |postMDef|)
          (-> |postMapping|)
          (=> |postExit|)
          (|Tuple| |postTuple|)))

(mapcar #'(lambda (x) (MAKEPROP (CAR X) '|parseTran| (CADR X)))
        '((\<= |parseLessEqual|)
          (\> |parseGreaterThan|)
          (\>= |parseGreaterEqual|)
          ($\<= |parseDollarLessEqual|)
          ($\> |parseDollarGreaterThan|)
          ($\>= |parseDollarGreaterEqual|)
          ($^= |parseDollarNotEqual|)
          (^ |parseNot|)
          (^= |parseNotEqual|)
          (\: |parseColon|)
          (\:\: |parseCoerce|)
          (\@ |parseAtSign|)
          (|and| |parseAnd|)
          (CATEGORY |parseCategory|)
          (|construct| |parseConstruct|)
          (DEF |parseDEF|)
          (|eqv| |parseEquivalence|)
          (|exit| |parseExit|)
          (|has| |parseHas|)
          (IF |parseIf|)
          (|implies| |parseImplies|)
          (IN |parseIn|)
          (INBY |parseInBy|)
          (|is| |parseIs|)
          (|isnt| |parseIsnt|)
          (|Join| |parseJoin|)
          (|leave| |parseLeave|)
          (LET |parseLET|)
          (LETD |parseLETD|)
          (MDEF |parseMDEF|)
          (|not| |parseNot|)
          (|or| |parseOr|)
          (|pretend| |parsePretend|)
          (|return| |parseReturn|)
          (SEQ |parseSeq|)
          (VCONS |parseVCONS|)
          (|where| |parseWhere|)
          (|xor| |parseExclusiveOr|)))
