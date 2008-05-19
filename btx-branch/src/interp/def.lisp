;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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
;;     - Neither the name of The Numerical Algorithms Group Ltd. nor the
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


; NAME:     Def
; PURPOSE:  Defines BOOT code

(IMPORT-MODULE "macros")

(in-package "BOOT")

;;; Common Block

(defparameter deftran nil)
(defparameter $macroassoc nil)
(defparameter $ne nil)

(defparameter $op nil
"$OP is globalized for construction of local function names, e.g.
foo defined inside of fum gets renamed as fum,foo.")

(defparameter $opassoc nil
"$OPASSOC is a renaming accumulator to be used with SUBLIS.")

(defparameter $BODY nil)

(defun DEF (FORM SIGNATURE $BODY)
  (declare (ignore SIGNATURE))
  (let* ($opassoc
         ($op (first form))
         (argl (rest form))
         ($body (deftran $body))
         (argl (DEF-INSERT_LET argl))
         (arglp (DEF-STRINGTOQUOTE argl))
         ($body (|bootTransform| $body)))
      (COMP (SUBLIS $OPASSOC (list (list $OP (list 'LAM arglp $body)))))))

; We are making shallow binding cells for these functions as well

(mapcar #'(lambda (x) (MAKEPROP (FIRST X) 'DEF-TRAN (SECOND X)))
         '((\: DEF-\:) (\:\: DEF-\:\:) (ELT DEF-ELT)
           (SETELT DEF-SETELT) (SPADLET DEF-LET)
           (SEQ DEF-SEQ) (COLLECT DEF-COLLECT)
           (REPEAT DEF-REPEAT) (TRACE-LET DEF-TRACE-LET)
           (CATEGORY DEF-CATEGORY) (EQUAL DEF-EQUAL)
           (|is| DEF-IS) (|isnt| DEF-ISNT) (|where| DEF-WHERE)))

(defun DEF-EQUAL (X)
  (COND ((NOT (CDR X)) (CONS 'EQUAL X))
        ((OR (MEMBER '(|One|) X) (MEMBER '(|Zero|) X)
             (integerp (FIRST X)) (integerp (SECOND X))) (CONS 'EQL X))
       ; ((AND (EQCAR (FIRST X) 'QUOTE) (IDENTP (CADAR X))) (CONS 'EQ X))
        ((NOT (FIRST X)) (LIST 'NULL (SECOND X)))
        ((NOT (SECOND X)) (LIST 'NULL (FIRST X)))
       ; ((AND (EQCAR (SECOND X) 'QUOTE) (IDENTP (CADADR X))) (CONS 'EQ X))
        ($BOOT (CONS 'BOOT-EQUAL X))
        ((CONS 'EQUAL X))))
 
(defun DEF-LESSP (x)
  (cond ((null (cdr x)) (cons '< x))
        ((eq (cadr x) 0) (list 'minusp (car x)))
        ((and (smint-able (car x)) (smint-able (cadr x)))
         (cons 'qslessp x))
        ('t (list '> (CADR x) (CAR x)))))

(defun smint-able (x)
  (or (smintp x)
      (and (pairp x) (memq (car x) '(|One| |Zero| LENGTH \# QCSIZE QVSIZE QLENGTH)))))

(defun DEF-PROCESS (X &aux $MACROASSOC)
  (COND ((EQCAR X 'DEF) (DEF (SECOND X) (THIRD X) (FIRST (CDDDDR X))))
        ((EQCAR X 'MDEF) (B-MDEF (SECOND X) (THIRD X) (FIRST (CDDDDR X))))
        ((AND (EQCAR X 'WHERE) (EQCAR (cadr X) 'DEF))
         (let* ((u (cadr X)) (Y (cdr U)))
           (DEF-PROCESS (LIST 'DEF
                              (car Y)
                              (car (setq Y (cdr Y)))
                              (car (setq Y (cdr Y)))
                              (CONS 'WHERE (cons (car (setq Y (cdr Y))) (cddr X)))))))
        ((IS-CONSOLE |$OutputStream|)
         (SAY "  VALUE = " (EVAL (DEFTRAN X))))
        ((print-full (DEFTRAN X)))))

(defun B-MDEF (FORM SIGNATURE $BODY)
  (declare (ignore SIGNATURE))
 (let* ($OpAssoc
        ($op (first form)) (argl (cdr form))
        (GARGL (MAPCAR #'(LAMBDA (X) (GENSYM)) ARGL))
        ($BODY (SUBLISLIS GARGL ARGL (|bootTransform| (DEFTRAN $BODY))))
        ($BODY (LIST 'SUBLISLIS (CONS 'LIST GARGL) (LIST 'QUOTE GARGL)
                     (LIST 'QUOTE $BODY))))
   (COMP (SUBLIS $OPASSOC
                 (LIST (LIST $OP (LIST 'MLAMBDA (CONS () GARGL) $BODY)))))))

(defun DEF-INNER (FORM SIGNATURE $BODY)
  "Same as DEF but assumes body has already been DEFTRANned"
 (let ($OpAssoc ($op (first form)) (argl (rest form)))
   (let* ((ARGL (DEF-INSERT_LET ARGL))
          (ARGLP (DEF-STRINGTOQUOTE ARGL)))
    (COMP (SUBLIS $OPASSOC `((,$OP (LAM ,ARGLP ,$BODY))))))))

(defun DEF-INSERT_LET (X)
  (if (ATOM X) X
      (CONS (DEF-INSERT_LET1 (FIRST X)) (DEF-INSERT_LET (CDR X)))))

(defun DEF-INSERT_LET1 (Y)
  (if (EQCAR Y 'SPADLET)
      (COND ((IDENTP (SECOND Y))
             (setq $BODY
                   (MKPROGN
                     (LIST (DEF-LET (THIRD Y) (SECOND Y)) $BODY)))
             (setq Y (SECOND Y)))
            ((IDENTP (THIRD Y))
             (setq $BODY
                   (MKPROGN (LIST (DEFTRAN Y) $BODY))) (setq Y (THIRD Y)))
            ((ERRHUH)))
      Y))

(defun MKPROGN (L) (MKPF L 'PROGN))

(defun DEF-STRINGTOQUOTE (X)
  (COND ((STRINGP X) (LIST 'QUOTE (INTERN X)))
        ((ATOM X) X)
        ((CONS (DEF-ADDLET (FIRST X)) (DEF-STRINGTOQUOTE (CDR X))))))

(defun DEF-ADDLET (X)
  (if (ATOM X)
      (if (STRINGP X) `(QUOTE ,(intern x))  X)
      (let ((g (gensym)))
        (setq $body (mkprogn
                     (list (def-let (comp\,fluidize x) g)
                           $body)))
        g)))

(mapcar #'(lambda (x) (MAKEPROP (CAR X) 'RENAME (CDR X)))
        '((|true| 'T) (|otherwise| 'T) (|false| NIL)
          (|and| AND) (|or| OR) (|is| IS)
          (|list| LIST) (|cons| CONS) (|car| CAR) (|cdr| CDR)
          (|setDifference| SETDIFFERENCE) (INTERSECTION |intersection|)
          (|setIntersection| |intersection|) (|setUnion| |union|)
          (UNION |union|) (REMOVE |remove|) (MEMBER |member|) (ASSOC |assoc|)
          (READ VMREAD) (READ-LINE |read-line|)
          (|apply| APPLY) (|lastNode| LASTPAIR) (LAST |last|)
          (|in| |member|) (|strconc| STRCONC) (|append| APPEND)
          (|copy| COPY) (DELETE |delete|) (RASSOC |rassoc|)
          (|size| SIZE) (|nconc| NCONC)
          (|setPart| SETELT) (|where| WHERE)
          (|first| CAR) (|rest| CDR) (|substitute| MSUBST)
          (|removeDuplicates| REMDUP) (|reverse| REVERSE) (|nreverse| NREVERSE)
          (|drop| DROP) (|take| TAKE) (|croak| CROAK) (|genvar| GENVAR)
          (|mkpf| MKPF) (^= NEQUAL) (= EQUAL) (- SPADDIFFERENCE)
          (+ PLUS) (* TIMES) (/ QUOTIENT)
          (** EXPT) (|return| RETURN) (|exit| EXIT) (\| SUCHTHAT)
          (^ NULL) (|not| NULL) (NOT NULL) (REDUCE spadReduce) (DO spadDo)
          (|atom| ATOM) (|nil| NIL) (|null| NULL) (GET GETL)
          (T T$)))

; This two-level call allows DEF-RENAME to be locally bound to do
; nothing (see boot2Lisp) yet still allow function call (lisp2BootAndComp)

(defun DEF-RENAME (X) (DEF-RENAME1 X))

(defun DEF-RENAME1 (X)
  (COND ((symbolp X) (let ((y (get x 'rename))) (if y (first y) x)))
        ((and (listp X) X)
         (if (EQCAR X 'QUOTE)
             X
             (CONS (DEF-RENAME1 (FIRST X)) (DEF-RENAME1 (CDR X)))))
        (X)))

(defun DEFTRAN (X)
 (let (op Y)
   (COND ((STRINGP X) (DEF-STRING X))
         ((IDENTP X) (COND ((LASSOC X $MACROASSOC)) (X)))
         ((ATOM X) X)
         ((EQ (setq OP (FIRST X)) 'WHERE) (DEF-WHERE (CDR X)))
         ((EQ OP 'REPEAT) (DEF-REPEAT (CDR X)))
         ((EQ OP 'COLLECT) (DEF-COLLECT (CDR X)))
         ((EQ OP 'MAKESTRING)
          (COND ((STRINGP (SECOND X)) X)
                ((EQCAR (SECOND X) 'QUOTE)
                 (LIST 'MAKESTRING (STRINGIMAGE (CADADR X))))
                ((LIST 'MAKESTRING (DEFTRAN (SECOND X)) )) ))
         ((EQ OP 'QUOTE)
          (if (STRINGP (setq y (SECOND X))) (LIST 'MAKESTRING y)
             (if (and (identp y) (char= (elt (pname y) 0) #\.))
                 `(intern ,(pname y) ,(package-name *package*)) x)))
         ((EQ OP 'IS) (|defIS| (CADR X) (CADDR X)))
         ((EQ OP 'SPADLET) (DEF-LET (CADR X) (caddr x)))
         ((EQ OP 'DCQ) (LIST 'DCQ (SECOND X) (DEFTRAN (THIRD X))))
         ((EQ OP 'COND) (CONS 'COND (DEF-COND (CDR X))))
         ((member (FIRST X) '(|sayBrightly| SAY MOAN CROAK) :test #'eq)
          (DEF-MESSAGE X))
         ((setq Y (GETL (FIRST X) 'DEF-TRAN))
          (funcall Y (MAPCAR #'DEFTRAN (CDR X))))
         ((mapcar #'DEFTRAN X)))))

(defun DEF-SEQ (U) (SEQOPT (CONS 'SEQ U)))

(defun DEF-MESSAGE (U) (CONS (FIRST U) (mapcar #'def-message1 (cdr u))))

(defun DEF-MESSAGE1 (V)
  (COND ((AND (STRINGP V) (> (SIZE V) 0) (NOT (EQ (ELT V 0) '\%)))
         (LIST 'MAKESTRING V))
        ((EQCAR V 'CONS) (LIST 'CONS (DEF-MESSAGE1 (SECOND V))
                               (DEF-MESSAGE1 (THIRD V))))
        ((DEFTRAN V))))

(defun |DEF-:| (X &aux Y)
       (DCQ (x y) x)
       `(SPADLET ,(if (or (eq y '|fluid|)
                          (and (identp x) (char= #\$ (ELT (PNAME X) 0))))
                      `(FLUID ,X) X)
                 NIL))

(defmacro |DEF-::| (X)
  (let ((expr (first x)) (type (second x)))
    (if (EQUAL TYPE '(|Triple|)) EXPR (ERRHUH))))

(defun DEF-COLLECT (L) (DEF-IT 'COLLECT (MAPCAR #'DEFTRAN (HACKFORIS L))))

(defun DEF-REPEAT (L) (DEF-IT 'REPEAT (mapcar #'DEFTRAN (HACKFORIS L))))

(defun HACKFORIS (L) (mapcar #'hackforis1 L))

(defun HACKFORIS1 (X)
  (if (AND (MEMBER (KAR X) '(IN ON)) (EQCAR (SECOND X) 'IS))
      (CONS (FIRST X) (CONS (CONS 'SPADLET (CDADR X)) (CDDR X)))
      X))

(defun DEF-select (L)
  (cond ((IDENTP (FIRST L)) (DEF-select1 (FIRST L) (SECOND L)))
        ((LET* ((G (GENSYM))
                (U (DEF-select1 G (SECOND L))))
           (LIST 'PROGN (LIST 'SPADLET G (FIRST L)) U)))))

(defun DEF-select1 (X Y)
  (if (EQCAR Y 'SEQ)
      (CONS 'COND (DEF-select2 X (CDR Y)))
      (MOAN (format nil "Unexpected CASE body: ~S" Y))))

(defun DEF-select2 (X Y)
  (let (u v)
    (COND ((NOT Y) (MOAN "Unexpected CASE clause termination"))
          ((EQCAR (setq U (FIRST Y)) 'EXIT)
           (LIST (LIST ''T (SECOND U))))
          ((AND (EQCAR U 'COND) (NOT (CDDR U))
                (EQCAR (SECOND (setq V (SECOND U))) 'EXIT))
           (CONS (LIST (DEF-IS (LIST X (FIRST V))) (CADADR V))
                 (DEF-select2 X (CDR Y))))
          ((MOAN (format nil "Unexpected CASE clause: ~S" (FIRST Y)))))))

(defun DEF-IT (FN L)
  (setq L (reverse L))
  (let ((B (first L)))
    (let ((it (DEF-IN2ON (NREVERSE (rest L)))))
      (let ((itp
              (apply #'APPEND
                     (mapcar
                       #'(lambda (x &aux OP Y G)
                           (if (AND (MEMBER (setq OP (FIRST X)) '(IN ON))
                                    (NOT (ATOM (SECOND X))))
                               (if (EQCAR (setq Y (SECOND X)) 'SPADLET)
                                   (if (ATOM (setq G (SECOND Y)))
                                       (LIST `(,OP ,G
                                               ,(DEFTRAN (THIRD X)))
                                             `(RESET
                                                ,(DEF-LET
                                                   (DEFTRAN
                                                     (THIRD Y)) G)))
                                       (ERRHUH))
                                   (LIST
                                     `(,OP ,(setq G (GENSYM))
                                       ,(DEFTRAN (THIRD X)))
                                     `(RESET
                                        ,(DEF-LET (DEFTRAN (SECOND X))
                                                  G))))
                               `(,X)))
                       IT))))
        (CONS FN (NCONC ITP (LIST B)))))))

(defun DEF-IN2ON (IT)
  (mapcar #'(lambda (x) (let (u)
              (COND
                ((AND (EQCAR X 'IN) (EQCAR (THIRD X) '|tails|))
                 (LIST 'ON (SECOND X) (SECOND (THIRD X))))
                ((AND (EQCAR X 'IN) (EQCAR (setq U (THIRD X)) 'SEGMENT))
                 (COND
                   ((THIRD U) (LIST 'STEP (SECOND X) (SECOND U) 1 (THIRD U)))
                   ((LIST 'STEP (SECOND X) (SECOND U) 1))  ))
                ((AND (EQCAR X 'INBY) (EQCAR (setq U (THIRD X)) 'SEGMENT))
                 (COND
                   ((THIRD U) (LIST 'STEP (SECOND X) (SECOND U) (|last| x) (THIRD U)))
                   ((LIST 'STEP (SECOND X) (SECOND U) (|last| x)))  ))
                (X))))
          IT))

(defun DEF-COND (L)
  (COND ((NOT L) NIL)
        ((CONS (MAPCAR #'DEFTRAN (FIRST L)) (DEF-COND (CDR L))))))

(defun DEF-LET (FORM RHS)
  (setq FORM (if (EQCAR FORM '\:) FORM (macroexpand FORM)))
  (prog (F1 F2)
   (COND ((EQCAR FORM '\:)
          (SPADLET F1 (DEFTRAN FORM))
          (SPADLET F2 (DEFTRAN (LIST 'SPADLET (CADR FORM) RHS)))
          (COND ((AND (EQ (CAR F2) 'SPADLET) (EQUAL (CADR F2) (CADR FORM)))
                  (RETURN (LIST 'SPADLET (CADR F1) (CADDR F2)) ))
                ('T (RETURN (LIST 'PROGN F1 F2)) )) )
        ((EQCAR FORM 'ELT) (RETURN
           (DEFTRAN (LIST 'SETELT (CADR FORM) (CADDR FORM) RHS)) )))
   (RETURN 
     (COND (|$useDCQnotLET| (|defLETdcq| FORM (DEFTRAN RHS)))
           ('T            (|defLET| FORM (DEFTRAN RHS)))))))

(defun |defLETdcq| (FORM RHS &AUX G NAME)
  ;; see defLET in G-BOOT BOOT
  (COND
    ((IDENTP FORM) (LIST 'SPADLET FORM RHS))
    ((IDENTP RHS)
       (LIST 'COND (LIST (DEFTRAN (LIST 'IS RHS FORM)) RHS)
                   (LIST ''T (LIST 'LET_ERROR (LIST 'MAKESTRING
                                                    (MK_LEFORM FORM)) RHS))))
    ((AND (EQ (CAR RHS) 'SPADLET) (IDENTP (SETQ NAME (CADR RHS)) ))
       (SPADLET G (GENSYM))
       (LIST 'COND (LIST (SUBST RHS G (DEFTRAN (LIST 'IS G FORM))) NAME)
                   (LIST ''T (LIST 'LET_ERROR (LIST 'MAKESTRING
                                                    (MK_LEFORM FORM)) NAME))))
    ('T (SPADLET G (GENSYM))
       (LIST 'COND (LIST (SUBST (LIST 'SPADLET G RHS) G
                                (DEFTRAN (LIST 'IS G FORM))) G)
                    (LIST ''T (LIST 'LET_ERROR (LIST 'MAKESTRING
                                                  (MK_LEFORM FORM)) G))  ) )))


(defun LET_ERROR (FORM VAL)
  (|systemError| (format nil "~S is not matched by structure ~S~%" FORM VAL)))

(defun DEF-ISNT (X) (DEFTRAN (LIST 'NULL (CONS 'IS X))))

(defparameter $IS-GENSYMLIST nil)

(defparameter Initial-Gensym (list (gensym)))

(defun DEF-IS (X)
  (let (($IS-GENSYMLIST Initial-Gensym))
    (DEF-IS2 (first X) (second x))))

(defun IS-GENSYM ()
  (if (NOT (CDR $IS-GENSYMLIST)) (RPLACD $IS-GENSYMLIST (LIST (GENSYM))))
  (pop $IS-GENSYMLIST))

(defparameter $IS-EQLIST nil)
(defparameter $IS-SPILL_LIST nil)

(defun DEF-IS2 (FORM STRUCT)
  (let ($IS-EQLIST $IS-SPILL_LIST (FORM (DEFTRAN FORM)))
    (if (EQCAR STRUCT '|%Comma|)
        (MOAN "you must use square brackets around right arg. to" '%b "is" '%d))
    (let* ((X (DEF-IS-EQLIST (DEF-IS-REMDUP STRUCT)))
           (CODE (if (IDENTP X)
                     (MKPF (SUBST FORM X $IS-EQLIST) 'AND)
                     (MKPF `((DCQ ,X ,FORM) . ,$IS-EQLIST) 'AND))))
      (let ((CODE (MKPF `(,CODE . ,$IS-SPILL_LIST) 'AND)))
        (if $TRACELETFLAG
            (let ((L (remove-if #'gensymp (listofatoms x))))
              `(PROG1 ,CODE
                      ,@(mapcar #'(lambda (y) `(/tracelet-print ,y ,y)) L)))
            CODE)))))

(defun DEF-STRING (X)
 ;; following patches needed to fix reader bug in Lucid Common Lisp
  (if (and (> (size x) 0) (or (char= (elt x 0) #\.) (char= (elt x 0) #\Page)))
      `(INTERN ,X ,(package-name *PACKAGE*))
      `(QUOTE ,(DEFTRAN (INTERN X)))))

(defun DEF-IS-EQLIST (STR)
  (let (g e)
    (COND ((NOT STR) (PUSH `(EQ ,(setq G (IS-GENSYM)) NIL) $IS-EQLIST) G)
          ((EQ STR '\.) (IS-GENSYM))
          ((IDENTP STR) STR)
          ((STRINGP STR)
           (setq E (DEF-STRING STR))
           (PUSH (LIST (if (ATOM (SECOND E)) 'EQ 'EQUAL)
                       (setq G (IS-GENSYM)) E)
                 $IS-EQLIST)
           G)
          ((OR (NUMBERP STR) (MEMBER STR '((|Zero|) (|One|))))
           (PUSH (LIST 'EQ (setq G (IS-GENSYM)) STR) $IS-EQLIST)
           G)
          ((ATOM STR) (ERRHUH))
          ((EQCAR STR 'SPADLET)
           (COND ((IDENTP (SECOND STR))
                  (PUSH (DEF-IS2 (cadr str) (caddr STR)) $IS-SPILL_LIST)
                  (SECOND STR))
                 ((IDENTP (THIRD STR))
                  (PUSH (DEFTRAN STR) $IS-SPILL_LIST) (THIRD STR))
                 ((ERRHUH)) ))
          ((EQCAR STR 'QUOTE)
           (PUSH (LIST (COND ((ATOM (SECOND STR)) 'EQ)
                             ('EQUAL))
                       (setq G (IS-GENSYM)) STR) $IS-EQLIST) G)
          ((EQCAR STR 'LIST) (DEF-IS-EQLIST (LIST2CONS STR)))
          ((OR (EQCAR STR 'CONS) (EQCAR STR 'VCONS))
           (CONS (DEF-IS-EQLIST (SECOND STR)) (DEF-IS-EQLIST (THIRD STR))))
          ((EQCAR STR 'APPEND)
           (if (NOT (IDENTP (SECOND STR))) (ERROR "CANT!"))
           (PUSH (DEF-IS2 (LIST 'REVERSE (setq G (IS-GENSYM)))
                          (DEF-IS-REV (THIRD STR) (SECOND STR)))
                 $IS-EQLIST)
           (COND ((EQ (SECOND STR) '\.) ''T)
                 ((PUSH (SUBST (SECOND STR) 'L '(OR (setq L (NREVERSE L)) T))

                        $IS-SPILL_LIST)))
           G)
          ((ERRHUH)))))

(defparameter $vl nil)

(defun def-is-remdup (x) (let ($vl) (def-is-remdup1 x)))

(defun def-is-remdup1 (x)
  (let (rhs lhs g)
    (COND ((NOT X) NIL)
          ((EQ X '\.) X)
          ((IDENTP X)
           (COND ((MEMBER X $VL)
                  (PUSH (LIST 'EQUAL (setq G (IS-GENSYM)) X) $IS-EQLIST) G)
                 ((PUSH X $VL) X)))
          ((MEMBER X '((|Zero|) (|One|))) X)
          ((ATOM X) X)
          ((EQCAR X 'SPADLET)
           (setq RHS (DEF-IS-REMDUP1 (THIRD X)))
           (setq LHS (DEF-IS-REMDUP1 (SECOND X)))
           (LIST 'SPADLET LHS RHS))
          ((EQCAR X 'LET)
           (setq RHS (DEF-IS-REMDUP1 (THIRD X)))
           (setq LHS (DEF-IS-REMDUP1 (SECOND X)))
           (LIST 'LET LHS RHS))
          ((EQCAR X 'QUOTE) X)
          ((AND (EQCAR X 'EQUAL) (NOT (CDDR X)))
           (PUSH (LIST 'EQUAL (setq G (IS-GENSYM)) (SECOND X)) $IS-EQLIST) G)
          ((MEMBER (FIRST X) '(LIST APPEND CONS VCONS))
           (CONS (COND ((EQ (FIRST X) 'VCONS) 'CONS) ( (FIRST X)))
                 (mapcar #'def-is-remdup1 (cdr x))))
          ((ERRHUH)))))

(defun LIST2CONS (X)
"Produces LISP code for constructing a list, involving only CONS."
 (LIST2CONS-1 (CDR X)))

(defun LIST2CONS-1 (X)
  (if (NOT X) NIL (LIST 'CONS (FIRST X) (LIST2CONS-1 (CDR X)))))

(defun DEF-IS-REV (X A)
  (let (y)
    (if (EQ (FIRST X) 'CONS)
        (COND ((NOT (THIRD X)) (LIST 'CONS (SECOND X) A))
              ((setq Y (DEF-IS-REV (THIRD X) NIL))
               (setf (THIRD Y) (LIST 'CONS (SECOND X) A))
               Y))
        (ERRHUH))))

(defparameter $DEFSTACK nil)

(defun DEF-WHERE (args)
  (let ((x (car args)) (y (cdr args)) $DEFSTACK)
    (let ((u (DEF-WHERECLAUSELIST Y)))
      (mapc #'(lambda (X) (DEF-INNER (FIRST X) NIL
                              (SUBLIS $OPASSOC (SECOND X))))
              $DEFSTACK)
      (MKPROGN (NCONC U (LIST (DEFTRAN X)))))))

(defun DEF-WHERECLAUSELIST (L)
  (if (NOT (CDR L))
      (DEF-WHERECLAUSE (DEFTRAN (FIRST L)))
      (REDUCE #'APPEND
              (mapcar #'(lambda (u) (def-whereclause (deftran u))) L))))

(defun DEF-WHERECLAUSE (X)
  (COND ((OR (EQCAR X 'SEQ) (EQCAR X 'PROGN))
         (reduce #'append (mapcar #'def-whereclause (cdr x))))
        ((EQCAR X 'DEF) (WHDEF (SECOND X) (FIRST (CDDDDR X))) NIL)
        ((AND (EQCAR X '|exit|) (EQCAR (SECOND X) 'DEF))
         (WHDEF (CADADR X) (FIRST (CDDDDR (SECOND X)) )) NIL)
        ((LIST X))))

(defun WHDEF (X Y)
  "Returns no value -- side effect is to do a compilation or modify a global."
  (prog ((XP (if (ATOM X) (LIST X) X)) Op)
    (COND ((NOT (CDR XP))
           (RETURN (PUSH (CONS (FIRST XP) Y) $MACROASSOC))))
    (setq OP (INTERNL (PNAME $OP) "\," (FIRST XP)))
    (SETQ $OPASSOC (PUSH (CONS (FIRST XP) OP) $OPASSOC))
    (SETQ $DEFSTACK (CONS (LIST (CONS OP (CDR XP)) Y) $DEFSTACK))
    NIL))


(mapcar #'(lambda (x) (MAKEPROP (first X) 'SEL\,FUNCTION (second X)))
        '((|aTree| 0)           (|aMode| 1)
          (|aValue| 2)          (|aModeSet| 3)
          (|aGeneral| 4)        (|expr| CAR)
          (|mode| CADR)         (|env| CADDR)
          (|mmDC| CAAR)         (|cacheName| CADR)
          (|cacheType| CADDR)   (|cacheReset| CADDDR)
          (|cacheCount| CADDDDR)(|mmSignature| CDAR)
          (|mmTarget| CADAR)    (|mmCondition| CAADR)
          (|mmImplementation| CADADR)
          (|streamName| CADR)   (|streamDef| CADDR)
          (|streamCode| CADDDR) (|opSig| CADR)
          (|attributes| CADDR)  (|op| CAR)
          (|opcode| CADR)       (|sig| CDDR)
          (|source| CDR)        (|target| CAR)
          (|first| CAR)         (|rest| CDR)))

(defun DEF-ELT (args)
  (let ((EXPR (car args)) (SEL (cadr args)))
    (let (Y)
      (COND ((and (symbolp sel) (setq Y (GET SEL 'SEL\,FUNCTION)))
             (COND ((INTEGERP Y) (LIST 'ELT EXPR Y))
                   ((LIST Y EXPR))))
            ((LIST 'ELT EXPR SEL))))))

(defun DEF-SETELT (args)
  (let ((VAR (first args)) (SEL (second args)) (EXPR (third args)))
    (let ((y (and (symbolp sel) (get sel 'sel\,function))))
      (COND (y (COND ((INTEGERP Y) (LIST 'SETELT VAR Y EXPR))
                     ((LIST 'RPLAC (LIST Y VAR) EXPR))))
            ((LIST 'SETELT VAR SEL EXPR))))))

(defun DEF-CATEGORY (L)
  (let (siglist atlist)
    (mapcar #'(lambda (x) (if (EQCAR (KADR X) 'Signature)
                              (PUSH X SIGLIST)
                              (PUSH X ATLIST)))
            L)
    (LIST 'CATEGORY (MKQ (NREVERSE SIGLIST)) (MKQ (NREVERSE ATLIST)))))


(defun LIST2STRING (X)
"Converts a list to a string which looks like a printed list,
except that elements are separated by commas."
  (COND ((ATOM X) (STRINGIMAGE X))
        ((STRCONC "(" (LIST2STRING (FIRST X)) (LIST2STRING1 (CDR X)) ")"))))

(defun LIST2STRING1 (X)
  (COND
    ((NOT X) "")
    ((STRCONC "\," (LIST2STRING (FIRST X)) (LIST2STRING1 (CDR X))))))

(defvar |$new2OldRenameAssoc|
        '((\QUAD . \.) (\' . QUOTE) (|nil| . NIL) (|append| . APPEND)
          (|union| . UNION) (|cons| . CONS)))

(defun |new2OldLisp| (x) (|new2OldTran| (|postTransform| x)))

(defun |new2OldTran| (x)
  (PROG (G10463 a b G10465 G10466 G10467 G10469 d G10470 c)
        (RETURN
          (prog nil
                (if (atom x)
                    (RETURN (let ((y (ASSOC x |$new2OldRenameAssoc|)))
                              (if y (cdr y) x))))
                (if (AND (dcq (g10463 a b . g10465) x)
                         (null G10465)
                         (EQ G10463 '|where|)
                         (dcq (g10466 . g10467) b)
                         (dcq ((g10469 d . g10470) . c) (reverse g10467))
                         (null G10470)
                         (EQ G10469 '|exit|)
                         (EQ G10466 'SEQ)
                         (OR (setq c (NREVERSE c)) 'T))
                    (RETURN
                      `(|where| ,(|new2OldTran| a) ,@(|new2OldTran| c)
                                ,(|new2OldTran| d))))
                (return
                  (case (car x)
                      (QUOTE x)
                      (DEF (|newDef2Def| x))
                      (IF (|newIf2Cond| x))
                      (|construct| (|newConstruct| (|new2OldTran| (cdr x))))
                      (T `(,(|new2OldTran| (CAR x)) .
                           ,(|new2OldTran| (CDR x))))))))))

(defun |newDef2Def| (DEF-EXPR)
  (if (not (AND (= (length def-expr) 5) (eq (car def-expr) 'DEF)))
      (LET_ERROR "(DEF,form,a,b,c)" DEF-EXPR)
      (let ((form (second def-expr))
            (a (third def-expr))
            (b (fourth def-expr))
            (c (fifth def-expr)))
        `(DEF ,(|new2OldDefForm|  form) ,(|new2OldTran| a)
           ,(|new2OldTran| b) ,(|new2OldTran| c)))))

(defun |new2OldDefForm| (x)
  (cond ((ATOM x) (|new2OldTran| x))
        ((and (listp x)
              (listp (car x))
              (eq (caar x) '|is|)
              (= (length (car x)) 3))
         (let ((a (second (car x))) (b (third (car x))) (y (cdr x)))
              (|new2OldDefForm| `((SPADLET ,a ,b) ,@y))))
        ((CONS (|new2OldTran| (CAR x)) (|new2OldDefForm| (CDR x))))))

(defun |newIf2Cond| (COND-EXPR)
       (if (not (AND (= (length cond-expr) 4) (EQ (car cond-expr) 'IF)))
           (LET_ERROR "(IF,a,b,c)" COND-EXPR))
       (let ((a (second COND-EXPR))
             (b (third COND-EXPR))
             (c (fourth COND-EXPR)))
         (setq a (|new2OldTran| a) b (|new2OldTran| b) c (|new2OldTran| c))
         (cond ((EQ c '|%noBranch|) `(if ,a ,b))
               (t  `(if ,a ,b ,c)))))

(defun |newConstruct| (l)
  (if (ATOM l) l
      `(CONS  ,(CAR l) ,(|newConstruct| (CDR l)))))
