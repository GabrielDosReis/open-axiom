;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2012, Gabriel Dos Reis.
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

(IMPORT-MODULE "types")
(import-module "sys-globals")

;      VM LISP EMULATION PACKAGE
;      Lars Ericson, Barry Trager, Martial Schor, tim daly, LVMCL, et al
;      IBM Thomas J. Watson Research Center
;      Summer, 1986
;  see /spad/daly.changes

; This emulation package version is written for Symbolics Common Lisp.
; Emulation commentary refers to LISP/VM, IBM Program Number 5798-DQZ,
; as described in the LISP/VM User's Guide, document SH20-6477-1.
; Main comment section headings refer to sections in the User's Guide.

; If you are using this, you are probably in Common Lisp, yes?

(in-package "BOOT")

;; DEFVARS

(defvar *comp370-apply* nil "function (name def) for comp370 to apply")

(defvar *embedded-functions* nil)

(defvar *fileactq-apply* nil "function to apply in fileactq")

(defvar macerrorcount 0  "Put some documentation in here someday")

(defvar *read-place-holder* (make-symbol "%.EOF")
   "default value returned by read and read-line at end-of-file")

;; DEFMACROS

(defmacro closedfn (form)
 `(function ,form))

(defmacro dcq (&rest args)
 (cons 'setqp args))

(defmacro difference (&rest args)
 `(- ,@args))

(defmacro dsetq (&whole form pattern exp)
 (dodsetq form pattern exp))

(defmacro ecq (&rest args)
 (cons 'eqq args))

;;def needed to prevent recursion in def of eqcar
(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun equable (x) 
   (or (null x) 
       (and (consp x) (eq (car x) 'quote) 
            (symbolp (cadr x))))))

(defmacro eqcar (x y)
 (let ((test
        (cond
         ((equable y) 'eq)
         ((integerp y) 'i=)
         ('eql))))
  (if (atom x)
   `(and (consp ,x) (,test (qcar ,x) ,y))
    (let ((xx (gensym)))
     `(let ((,xx ,x))
       (and (consp ,xx) (,test (qcar ,xx) ,y)))))))

(defmacro eqq (pattern exp)
 `(,(ecqexp pattern nil) ,exp))

(defmacro |equal| (x y)
 `(equalp ,x ,y))

(defmacro exit (&rest value)
 `(return-from seq ,@value))

(defmacro greaterp (&rest args)
 `(> ,@args))

(defmacro i= (x y) ;; integer equality
  (if (typep y 'fixnum)
      (let ((gx (gensym)))
        `(let ((,gx ,x))
           (and (typep ,gx 'fixnum) (eql (the fixnum ,gx) ,y))))
    (let ((gx (gensym)) (gy (gensym)))
      `(let ((,gx ,x) (,gy ,y))
         (cond ((and (typep ,gx 'fixnum) (typep ,gy 'fixnum))
                (eql (the fixnum ,gx) (the fixnum ,gy)))
               ((eql (the integer ,gx) (the integer,gy))))))))

(defmacro |idChar?| (x)
 `(or (alphanumericp ,x) (member ,x '(#\? #\% #\' #\!) :test #'char=)))
 
(defmacro ifcar (x)
  (if (atom x)
      `(and (consp ,x) (qcar ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (and (consp ,xx) (qcar ,xx))))))

(defmacro ifcdr (x)
  (if (atom x)
      `(and (consp ,x) (qcdr ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (and (consp ,xx) (qcdr ,xx))))))

(defmacro minus (x)
 `(- ,x))

(defmacro ne (a b) `(not (equal ,a ,b)))

(defmacro plus (&rest args)
 `(+ ,@ args))

(defmacro qcar (x)
 `(car (the cons ,x)))

(defmacro qcdr (x)
 `(cdr (the cons ,x)))

(defmacro qcaar (x)
 `(car (the cons (car (the cons ,x)))))

(defmacro qcadr (x)
 `(car (the cons (cdr (the cons ,x)))))

(defmacro qcdar (x)
 `(cdr (the cons (car (the cons ,x)))))

(defmacro qcddr (x)
 `(cdr (the cons (cdr (the cons ,x)))))

(defmacro qcaaar (x)
 `(car (the cons (car (the cons (car (the cons ,x)))))))
(defmacro qcaadr (x)
 `(car (the cons (car (the cons (cdr (the cons ,x)))))))
(defmacro qcadar (x)
 `(car (the cons (cdr (the cons (car (the cons ,x)))))))
(defmacro qcaddr (x)
 `(car (the cons (cdr (the cons (cdr (the cons ,x)))))))
(defmacro qcdaar (x)
 `(cdr (the cons (car (the cons (car (the cons ,x)))))))
(defmacro qcdadr (x)
 `(cdr (the cons (car (the cons (cdr (the cons ,x)))))))
(defmacro qcddar (x)
 `(cdr (the cons (cdr (the cons (car (the cons ,x)))))))
(defmacro qcdddr (x)
 `(cdr (the cons (cdr (the cons (cdr (the cons ,x)))))))

(defmacro qeqq (pattern exp)
 `(,(ecqexp pattern 1) ,exp))

(defmacro qrplaca (a b)
 `(rplaca (the cons ,a) ,b))

(defmacro qrplacd (a b)
 `(rplacd (the cons ,a) ,b))

(defmacro qrplq (&whole form pattern exp)
 (if (or (consp pattern) (simple-vector-p pattern))
  `(,(rcqexp pattern) ,exp)
   (macro-invalidargs 'qrplq form "form must be updateable.")))

(defmacro resetq (a b)
 `(prog1 ,a (setq ,a ,b)))

(defmacro rplq (&whole form exp pattern)
 (if (or (consp pattern) (simple-vector-p pattern))
  `(,(rcqexp pattern) ,exp)
   (macro-invalidargs 'rplq form "form must be updateable.")))

(defmacro rvecp (v)
 `(typep ,v '(vector float)))

(defmacro setandfileq (id item)
 `(eval-when
   #+:common-lisp (:load-toplevel :execute)
   #-:common-lisp (eval load) 
   (setq ,id ,item)
   (lam\,fileactq ',id (list 'setq ',id (list 'quote ,id)))))

(defmacro setqp (&whole form pattern exp)
  `(,(dcqexp pattern '=) ,exp))

(defmacro seq (&rest form)
  (let* ((body (|reverse| form))
         (val `(return-from seq ,(pop body))))
    (|substitute!| '(progn) nil body) ;don't treat NIL as a label
    `(block seq (tagbody ,@(|reverse!| body) ,val))))

(defmacro subrp (x)
 `(compiled-function-p ,x))

(defmacro times (&rest args)
 `(* ,@args))

;; defuns

(define-function 'tempus-fugit #'get-internal-run-time)

(defun $TOTAL-ELAPSED-TIME ()
   (list (get-internal-run-time) (get-internal-real-time)))

#-(OR IBCL KCL :CMULISP)
(defun $TOTAL-GC-TIME () (list 0 0))

#+IBCL
(defun $TOTAL-GC-TIME (&aux (gcruntime (system:gbc-time-report)))
  (list gcruntime gcruntime))

#+KCL
(defun $TOTAL-GC-TIME (&aux (gcruntime (system:gbc-time)))
  (if (minusp gcruntime)
      (setq gcruntime (system:gbc-time 0)))
  (list gcruntime gcruntime))

;;; note: this requires the 11/9/89 gc patch in code/lisp/daly/misc.lisp
#+:cmulisp
(defun $TOTAL-GC-TIME ()
 (declare (special ext::*gc-runtime* ext::*gc-walltime*))
 (list ext::*gc-runtime* ext::*gc-walltime*))

; 5.2.2 Lambda Expressions

(defun WRAP (LIST-OF-ITEMS WRAPPER)
 (prog nil
  (COND ((OR (NOT (CONSP LIST-OF-ITEMS)) (not WRAPPER))
         (RETURN LIST-OF-ITEMS))
        ((NOT (consp WRAPPER))
         (SETQ WRAPPER (LOTSOF WRAPPER))))
  (RETURN
    (CONS (if (first WRAPPER)
              `(,(first WRAPPER) ,(first LIST-OF-ITEMS))
              (first LIST-OF-ITEMS))
          (WRAP (cdr LIST-OF-ITEMS) (cdr WRAPPER))))))

(defun lotsof (&rest items)
  (setq items (|copyList| items))
  (|append!| items items))

; 9.4 Vectors and Bpis

(defun IVECP (x) (and (vectorp x) (subtypep (array-element-type x) 'integer)))

(defun mbpip (item) (and (symbolp item) ;cannot know a compiled macro in CLISP
                         (compiled-function-p (macro-function item))))

(defun FBPIP (item) (or (compiled-function-p item)
                        (and (symbolp item) (fboundp item)
                             (not (macro-function item))
                             (compiled-function-p (symbol-function item)))))

; 9.5 Identifiers

(defun digitp (x)
  (or (and (symbolp x) (digitp (symbol-name x)))
      (and (characterp x) (digit-char-p x))
      (and (stringp x) (= (length x) 1) (digit-char-p (char x 0)))))

(defun dig2fix (x)
  (if (symbolp x)
    (digit-char-p (char (symbol-name x) 0))
    (digit-char-p x)))

(defun LN (x) (LOG x))

(defun LOG2 (x) (LOG x 2.0))
(defun |log| (x) (LOG x 10.0))

; 9.13 Streams

#+KCL
(defun |ioTerminal?| (stream)
  (and (streamp stream) (output-stream-p stream)
       (eq (system:fp-output-stream stream)
           (system:fp-output-stream *terminal-io*))))

#-KCL
(defun |ioTerminal?| (stream)
  (cond ((not (streamp stream))
	 nil)
	((not (typep stream 'synonym-stream))
	 nil)
	((eq (synonym-stream-symbol stream) '*standard-input*)
	 (|stdStreamIsTerminal| 0))
	((eq (synonym-stream-symbol stream) '*standard-output*)
	 (|stdStreamIsTerminal| 1))))

; 10.0 Control Structures

; 10.8.4 Auxiliary Operators

(defun nilfn (&rest ignore)
 (declare (ignore ignore)) 
 ())

; 11.0 Operations on Identifiers

; 11.1 Creation

(defun upcase (l)
  (cond ((stringp l) (string-upcase l))
        ((|ident?| l) (intern (string-upcase (symbol-name l))))
        ((characterp l) (char-upcase l))
        ((atom l) l)
        (t (mapcar #'upcase l))))

(define-function 'U-CASE #'upcase)
(define-function 'LC2UC #'upcase)

(defun downcase (l)
  (cond ((stringp l) (string-downcase l))
        ((|ident?| l) (intern (string-downcase (symbol-name l))))
        ((characterp l) (char-downcase L))
        ((atom l) l)
        (t (mapcar #'downcase l))))

(define-function 'L-CASE #'downcase)

; 11.2 Accessing

;; note it is important that PNAME returns nil not an error for non-symbols
(defun pname (x)
  (cond ((symbolp x) (symbol-name x))
        ((characterp x) (string x))
        (t nil)))

;; property lists in vmlisp are alists
(defun PROPLIST (x)
  (if (symbolp x)

   (plist2alist (symbol-plist x))
    nil))

(defun plist2alist (x)
  (if (null x) 
      nil
      (cons (cons (first x) (second x)) (plist2alist (cddr x)))))

(eval-when
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (progn
   (defun put (sym ind val) (setf (get sym ind) val))
   
   (define-function 'MAKEPROP #'put)))

; 12.0 Operations on Numbers

; 12.1 Conversion

(define-function 'FIX #'truncate)
(define-function 'INT2RNUM #'float)

; 12.3 Computation

(defun QUOTIENT (x y)
  (cond ((or (floatp x) (floatp y)) (/ x y))
        (t (truncate x y))))

(define-function 'vm/ #'quotient)

(defun REMAINDER (x y)
  (if (and (integerp x) (integerp y))
      (rem x y)
      (- x (* y (QUOTIENT x y)))))

(defun DIVIDE (x y)
  (if (and (integerp x) (integerp y))
      (multiple-value-list (truncate x y))
      (list (QUOTIENT x y) (REMAINDER x y))))

; 13.3 Updating


(defun RPLPAIR (pair1 pair2)
  (RPLACA pair1 (CAR pair2))
  (RPLACD pair1 (CDR pair2)) pair1)

(defun RPLNODE (pair1 ca2 cd2)
 (RPLACA pair1 ca2) 
 (RPLACD pair1 cd2) pair1)

; 14.0 Operations on Lists

; 14.1 Creation

(defun VEC2LIST (vec) (coerce vec 'list))

(defun |member| (item sequence)
   (cond ((symbolp item) (member item sequence :test #'eq))
         ((stringp item) (member item sequence :test #'equal))
         ((and (atom item) (not (arrayp item))) (member item sequence))
         (T (member item sequence :test #'equalp))))

; 14.3 Searching

(DEFUN |assoc| (X Y)
  "Return the pair associated with key X in association list Y."
  ; ignores non-nil list terminators
  ; ignores non-pair a-list entries
  (cond ((symbolp X) (|symbolAssoc| X Y))
        ((or (numberp x) (characterp x))
	 (|scalarAssoc| X Y))
        (t
         (PROG NIL
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQUAL (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))))

; 16.0 Operations on Vectors

; 16.1 Creation

(defun GETREFV (n)
  (make-array n :initial-element nil))

(defun LIST2VEC (list)
 (if (consp list)
     (make-array (list-length list) :initial-contents list)
   (coerce list 'vector)))

; 16.2 Accessing


;; Oddly, LENGTH is more efficient than LIST-LENGTH in CCL, since the former
;; is compiled and the latter is byte-coded!
(defun size (l) 
  (cond ((vectorp l) (length l))
        ((consp l)   (list-length l))
        (t           0)))

(define-function 'MOVEVEC #'replace)

; 17.0 Operations on Character and Bit Vectors

(defun charp (a) (or (characterp a)
                     (and (|ident?| a) (= (length (symbol-name a)) 1))))

;; Returns truthvalue (nil or t) if s1 compares lexicographically 
;; greater than s2.  Note: It is essential that this function returns
;; a canonical Boolean value because parts of the system use it in 
;; contexts where generalized Boolean values are confusing.
(defun CGREATERP (s1 s2) 
  (cond ((string> (string s1) (string s2)) t)))


(define-function 'STRGREATERP #'CGREATERP)

; 17.1 Creation


(define-function 'strconc #'concat)

; 17.2 Accessing

(defun string2id-n (cvec sint)
  (if (< sint 1)
      nil
      (let ((start (position-if-not #'(lambda (x) (char= x #\Space)) cvec)))
        (if start
            (let ((end (or (position #\Space cvec :start start) (length cvec))))
              (if (= sint 1)
                  (intern (subseq cvec start end))
                  (string2id-n (subseq cvec end) (1- sint))))
            0))))

(defun substring (cvec start length)
  (setq cvec (string cvec))
  (if length (subseq cvec start (+ start length)) (subseq cvec start)))

; 17.3 Searching

(defun strpos (what in start dontcare)
   (setq what (string what) in (string in))
   (if dontcare (progn (setq dontcare (character dontcare))
                       (search what in :start2 start
                               :test #'(lambda (x y) (or (eql x dontcare)
                                                         (eql x y)))))
                (if (= start 0)
                   (search what in)
                   (search what in :start2 start))
   ))

; In the following, table should be a string:

(defun strposl (table cvec sint item)
  (setq cvec (string cvec))
  (if (not item)
      (position table cvec :test #'(lambda (x y) (position y x)) :start sint)
      (position table cvec :test-not #'(lambda (x y) (position y x)) :start sint)))

; 17.4 Updating operators

(defun suffix (id cvec)
  "Suffixes the first char of the symbol or char ID to the string CVEC,
    changing CVEC."
  (unless (characterp id) (setq id (elt (string id) 0)))
  (cond ((array-has-fill-pointer-p cvec)
         (vector-push-extend id cvec)
         cvec)
        ((adjustable-array-p cvec)
         (let ((l (length cvec)))
           (adjust-array cvec (1+ l))
           (setf (elt cvec l) id)
           cvec))
        (t (concat cvec id))))

(defun setsize (vector size) (adjust-array vector size))

; The following version has been provided to avoid reliance on the
; Common Lisp concatenate and replace functions. These built-in Lisp
; functions would probably end up doing the character-by-character
; copying shown here, but would also need to cope with generic sorts
; of sequences and unwarranted keyword generality

(defun rplacstr (cvec1 start1 length1 cvec2
                       &optional start2 length2
                       &aux end1 end2)
  (setq cvec2 (string cvec2))
  (if (null start1) (setq start1 0))
  (if (null start2) (setq start2 0))
  (if (null length1) (setq length1 (- (length cvec1) start1)))
  (if (null length2) (setq length2 (- (length cvec2) start2)))
  (setq end1 (+ start1 length1))
  (setq end2 (+ start2 length2))
  (if (= length1 length2)
      (do ()
          ((= start1 end1) cvec1)
          (setf (aref cvec1 start1) (aref cvec2 start2))
          (setq start1 (1+ start1))
          (setq start2 (1+ start2)))
      (let* ((l1 (length cvec1))
             (r (make-string (- (+ l1 length2) length1)))
             (i 0))
         (do ((j 0 (1+ j)))
             ((= j start1))
             (setf (aref r i) (aref cvec1 j))
             (setq i (1+ i)))
         (do ((j start2 (1+ j)))
             ((= j end2))
             (setf (aref r i) (aref cvec2 j))
             (setq i (1+ i)))
         (do ((j end1 (1+ j)))
             ((= j l1))
             (setf (aref r i) (aref cvec1 j))
             (setq i (1+ i)))
         r)
  ))

; 19.0 Operations on Arbitrary Objects

; 19.1 Creating

(defun MSUBST (new old tree) (subst new old tree :test #'equal))
; note subst isn't guaranteed to copy
(defun |nsubst| (new old tree) (nsubst new old tree :test #'equal))

(defun copy (x) (copy-tree x)) ; not right since should descend vectors

; Gen code for SETQP expr

(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun DCQEXP (FORM EQTAG)
  (PROG (SV pvl avl CODE)
        (declare (special pvl avl))
        (setq SV (GENSYM))
        (setq CODE (DCQGENEXP SV FORM EQTAG NIL))
        (RETURN
          `(LAMBDA (,sv)
             (PROG ,pvl
                   ,@code
                   (RETURN 'true)
                BAD (RETURN NIL) ) ))))
)
; Generate Expr code for DCQ
(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun DCQGENEXP (SV FORM EQTAG QFLAG)
  (PROG (D A I L C W)
        (declare (special pvl avl))
    (COND ((EQ FORM SV) (RETURN NIL))
          ((|ident?| FORM) (RETURN `((setq ,form ,sv)) ))
          ((simple-vector-p FORM)
           (RETURN (SEQ
             (setq L (length FORM))
             (IF (EQ L 0)
                 (RETURN (COND ((NULL QFLAG)
                                `((cond ((not (simple-vector-p ,sv)) (go bad))))))))
             (setq I (1- L))
         LP  (setq A (elt FORM I))
             (COND ((AND (NULL W) (OR (consp A) (simple-vector-p A)))
                    (COND ((consp AVL) (setq W (car (RESETQ AVL (cdr AVL)))))
                          ((setq PVL (CONS (setq W (GENSYM)) PVL))))))
             (setq C (|append!| (COND ((|ident?| A) `((setq ,a (ELT ,sv ,i))))
                                  ((OR (consp A) (simple-vector-p A))
                                   `((setq ,w (ELT ,sv ,i))
                                     ,@(dcqgenexp w a eqtag qflag))))
                            C))
             (if (EQ I 0) (GO RET))
             (setq I (1- I))
             (GO LP)
         RET (if W (setq AVL (CONS W AVL)))
             (COND ((NULL QFLAG)
                    `((COND ((OR (NOT (simple-vector-p ,sv)) (< (length ,sv) ,l))
                             (GO BAD)))
                      ,@c))
                   ('T C)))))
          ((NOT (consp FORM)) (RETURN NIL))
          ((AND EQTAG (EQ (car FORM) EQTAG))
           (RETURN
             (COND
               ((OR (NOT (EQ 3 (LENGTH FORM))) (NOT (|ident?| (car (setq FORM (cdr FORM))))))
                (MACRO-INVALIDARGS 'DCQ\/QDCQ FORM "invalid pattern."))
               (`((setq ,(car form) ,sv)  ,@(DCQGENEXP SV (CADR FORM) EQTAG QFLAG)))))))
    (setq A (car FORM))
    (setq D (cdr FORM))
    (setq C (COND ((|ident?| A) `((setq ,a (CAR ,sv))))
                  ((OR (consp A) (simple-vector-p A))
                   (COND ((AND (NULL D) (|ident?| SV)) )
                         ((COND ((consp AVL) (setq W (car (RESETQ AVL (cdr AVL)))))
                                ((setq PVL (CONS (setq W (GENSYM)) PVL)) ) ) ) )
                   (COND ((AND (consp A) EQTAG (EQ (car A) EQTAG))
                          (DCQGENEXP (LIST 'CAR SV) A EQTAG QFLAG) )
                         (`((setq ,(or w sv) (CAR ,sv))
                            ,@(DCQGENEXP (OR W SV) A EQTAG QFLAG)))))))
    (setq C (|append!| C (COND ((|ident?| D) `((setq ,d (CDR ,sv))))
                           ((OR (consp D) (simple-vector-p D))
                            (COND
                              ((OR W (|ident?| SV)) )
                              ((COND ((consp AVL)
                                      (setq W (car (RESETQ AVL (cdr AVL)))) )
                                     ((setq PVL (CONS (setq W (GENSYM)) PVL)) ) ) ) )
                            (COND ((AND (consp D) EQTAG (EQ (car D) EQTAG))
                                   (DCQGENEXP (LIST 'CDR SV) D EQTAG QFLAG) )
                                  (`((setq ,(or w sv) (CDR ,sv))
                                     ,@(DCQGENEXP (OR W SV) D EQTAG QFLAG))))))))
    (COND (W (setq AVL (CONS W AVL))))
    (RETURN (COND ((NULL QFLAG) `((COND ((ATOM ,sv) (GO BAD))) ,@c)) (C)))))
)


; 19.3 Searching

; Generate code for EQQ

(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun ECQEXP (FORM QFLAG)
  (PROG (SV PVL CODE)
        (declare (special pvl))
        (setq SV (GENSYM))
        (setq CODE (ECQGENEXP SV FORM QFLAG))
        (RETURN
              `(LAMBDA (,sv)
                 (PROG ,pvl
                       ,@code
                       (RETURN 'true)
                    BAD (RETURN NIL) ) ))))
)

; Generate code for EQQ innards

(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun ECQGENEXP (SV FORM QFLAG)
  (PROG (D A I L C W)
        (declare (special pvl))
        (COND
          ((EQ FORM SV) (RETURN NIL))
          ((OR
              (|ident?| FORM)
              (INTEGERP FORM)
              (AND (consp FORM) (EQ (qcar FORM) 'QUOTE)))
           (RETURN
             `((COND ((NOT (EQ ,form ,sv)) (GO BAD))) )))
          ((simple-vector-p FORM)
           (RETURN (SEQ
              (setq L (length FORM))
              (if (EQ L 0)
                  (RETURN
                    (COND ((NULL QFLAG)
                           `((COND ((NOT (simple-vector-p ,sv)) (GO BAD))) )))
                    ))
              (setq I (1- L))
           LP (setq A (elt FORM I))
              (if (AND (NULL W) (OR (consp A) (simple-vector-p A)))
                  (push (setq W (GENSYM)) PVL))
              (setq C
                    (|append!|
                      (COND
                        ( (OR
                            (|ident?| A)
                            (INTEGERP A)
                            (AND (consp A) (EQ (qcar A) 'QUOTE)))
                         `((COND ( (NOT (EQ ,a (ELT ,sv ,i)))
                                  (GO BAD) ) ) ) )
                        ( (OR (consp A) (simple-vector-p A))
                         `((setq ,w (ELT ,sv ,i))
                           ,@(ECQGENEXP W A QFLAG))))
                      C) )
              (if (EQ I 0) (GO RET) )
              (setq I (1- I))
              (GO LP)
           RET
              (COND
                ( (NULL QFLAG)
                 `((COND ( (OR
                             (NOT (simple-vector-p ,sv))
                             (< (length ,sv) ,l))
                          (GO BAD) ) )
                   ,@c))
                ( 'T C ) )) ))
          ( (NOT (consp FORM))
           (RETURN NIL) ) )
        (setq A (car FORM))
        (setq D (cdr FORM))
        (if (OR (consp A) (simple-vector-p A) (consp D) (simple-vector-p D))
           (setq PVL (CONS (setq W (GENSYM)) PVL)))
        (setq C
              (COND
                ( (OR (|ident?| A) (INTEGERP A) (AND (consp A) (EQ (car A) 'QUOTE)))
                 `((COND ((NOT (EQ ,a (CAR ,sv))) (GO BAD))) ))
                ( (OR (consp A) (simple-vector-p A))
                 `((setq ,w (CAR ,sv))
                   ,@(ECQGENEXP W A QFLAG)))))
        (setq C
              (|append!|
                C
                (COND
                  ( (OR (|ident?| D) (INTEGERP D) (AND (consp D)
                                                 (EQ (car D) 'QUOTE)))
                   `((COND ((NOT (EQ ,d (CDR ,sv))) (GO BAD))) ))
                  ( (OR (consp D) (simple-vector-p D))
                   `((setq ,sv (CDR ,sv))
                     ,@(ECQGENEXP SV D QFLAG))))))
        (RETURN
          (COND
            ( (NULL QFLAG)
             `((COND ( (ATOM ,sv)
                      (GO BAD) ) )
               ,@c))
            ( 'T
             C ) )) ) )
)

; 19.4 Updating

; Generate code for RPLQ exprs

(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun RCQEXP (FORM)
    (PROG (SV PVL CODE)
          (declare (special pvl))
      (setq SV (GENSYM))
      (setq CODE (RCQGENEXP SV FORM NIL))
      (RETURN
        `(LAMBDA (,sv)
              (PROG ,pvl
                ,@code
                (RETURN 'true)
            BAD (RETURN NIL) ) ))))
)

; Generate code for RPLQ expr innards

(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun RCQGENEXP (SV FORM QFLAG)
    (PROG (D A I L C W)
          (declare (special pvl))
      (COND
        ( (EQ FORM SV)
          (RETURN NIL) )
        ( (simple-vector-p FORM)
         (RETURN (SEQ
            (setq L (length FORM))
            (if (EQ L 0) (RETURN NIL))
            (setq I (1- L))
        LP  (setq A (elt FORM I))
            (COND
              ( (AND
                  (NULL W)
                  (OR (AND (consp A) (NOT (EQ (car A) 'QUOTE)))
                           (simple-vector-p A)))
                (setq PVL (CONS (setq W (GENSYM)) PVL)) ) )
            (setq C
              (|append!|
                (COND
                  ( (OR
                      (|ident?| A)
                      (INTEGERP A)
                      (AND (consp A) (EQ (car A) 'QUOTE)))
                    `((SETF (ELT ,sv ,i) ,a)))
                  ( (OR (consp A) (simple-vector-p A))
                    `((setq ,w (ELT ,sv ,i))
                      ,@(RCQGENEXP W A QFLAG))))
                C) )
            (COND
              ( (EQ I 0)
                (GO RET) ) )
            (setq I (1- I))
            (GO LP)
        RET (RETURN
              (COND
                ( (NULL QFLAG)
                  `((COND ( (OR
                              (NOT (simple-vector-p ,sv))
                              (< (length ,sv) ,l))
                            (GO BAD) ) )
                    ,@c))
                ( 'T
                  C ) )) )))
        ( (NOT (consp FORM))
          (RETURN NIL) ) )
      (setq A (car FORM))
      (setq D (cdr FORM))
      (cond
        ( (or (and (consp A) (NOT (EQ (car A) 'QUOTE))) (simple-vector-p A))
          (setq PVL (CONS (setq W (GENSYM)) PVL)) ) )
      (setq C
        (COND
          ( (OR (|ident?| A) (INTEGERP A) (AND (consp A) (EQ (car A) 'QUOTE)))
            `((rplaca ,sv ,a)))
          ( (OR (consp A) (simple-vector-p A))
            `((setq ,w (CAR ,sv))
              ,@(RCQGENEXP W A QFLAG)))))
      (setq C
        (|append!|
          C
          (COND
            ( (OR (|ident?| D) (INTEGERP D) (AND (consp D) (EQ (car D) 'QUOTE)))
              `((RPLACD ,sv ,d)))
            ( (OR (consp D) (simple-vector-p D))
              `((setq ,sv (CDR ,sv))
                ,@(RCQGENEXP SV D QFLAG))))))
      (RETURN
        (COND
          ( (NULL QFLAG)
            `((COND ( (ATOM ,sv)
                      (GO BAD) ) )
              ,@c))
          ( 'T
            C ) )) ) )
)

; 22.0 Internal and External Forms

; 23.0 Reading


(define-function 'next #'read-char)

; 24.0 Printing

(define-function 'prin2cvec #'princ-to-string)
(define-function 'stringimage #'princ-to-string)

(defun |F,PRINT-ONE| (form &optional (stream |$OutputStream|))
 (declare (ignore stream))
    (let ((*print-level* 4) (*print-length* 4))
       (prin1 form) (terpri)))

(defun prettyprint (x &optional (stream |$OutputStream|))
  (prettyprin0 x stream) (terpri stream))

(defun prettyprin0 (x &optional (stream |$OutputStream|))
  (let ((*print-pretty* t) (*print-array* t))
    (prin1 x stream)))

(defun vmprint (x &optional (stream |$OutputStream|))
  (prin1 x stream) (terpri stream))

(defun tab (sint &optional (stream t))
  (format stream "~vT" sint))

; 27.0 Stream I/O


; 27.1 Creation

(defun MAKE-INSTREAM (filespec &optional (recnum 0))
 (declare (ignore recnum))
   (cond ((numberp filespec) (make-synonym-stream '*standard-input*))
         ((null filespec) (error "not handled yet"))
         (t (open (make-input-filename filespec)
                  :direction :input :if-does-not-exist nil))))

(defun MAKE-OUTSTREAM (filespec &optional (width nil) (recnum 0))
 (declare (ignore width) (ignore recnum))
   (cond ((numberp filespec) (make-synonym-stream '*standard-output*))
         ((null filespec) (error "not handled yet"))
         (t (open (make-filename filespec) :direction :output
		  :if-exists :supersede))))

(defun MAKE-APPENDSTREAM (filespec &optional (width nil) (recnum 0))
 "fortran support"
 (declare (ignore width) (ignore recnum))
 (cond 
  ((numberp filespec) (make-synonym-stream '*standard-output*))
  ((null filespec) (error "make-appendstream: not handled yet"))
  ('else (open (make-filename filespec) :direction :output
          :if-exists :append :if-does-not-exist :create))))

(defun DEFIOSTREAM (stream-alist buffer-size char-position)
 (declare (ignore buffer-size))
   (let ((mode (or (cdr (assoc 'MODE stream-alist)) 'INPUT))
         (filename (cdr (assoc 'FILE stream-alist)))
         (dev (cdr (assoc 'DEVICE stream-alist))))
      (if (EQ dev 'CONSOLE)
	  (case mode
		((OUTPUT O) (make-synonym-stream '*standard-output*))
		((INPUT I) (make-synonym-stream '*standard-input*)))
        (let ((strm (case mode
                          ((OUTPUT O) (open (make-filename filename)
                                            :direction :output))
                          ((INPUT I) (open (make-input-filename filename)
                                           :direction :input)))))
          (if (and (numberp char-position) (> char-position 0))
              (file-position strm char-position))
          strm))))

(defun shut (st) (if (|ioTerminal?| st) st
                   (if (streamp st) (close st) -1)))

(defun EOFP (stream) (null (peek-char nil stream nil nil)))

; 28.0 Key addressed I/O


; 46.0 Call tracing


(defun EMBEDDED () (mapcar #'car *embedded-functions*))

(defun EMBED (CURRENT-BINDING NEW-DEFINITION)
  (PROG (OP BV BODY OLD-DEF)
      (COND
        ( (NOT (|ident?| CURRENT-BINDING))
          (SETQ CURRENT-BINDING
                (error (format nil "invalid argument ~s to EMBED" CURRENT-BINDING))) ) )
      (SETQ OLD-DEF (symbol-function CURRENT-BINDING))
      (SETQ NEW-DEFINITION
        (SETF (symbol-function CURRENT-BINDING)
          (COND
            ( (NOT (consp NEW-DEFINITION))
              NEW-DEFINITION )
            ( (AND
                (DCQ (OP BV . BODY) NEW-DEFINITION)
                (OR (EQ OP 'LAMBDA) (EQ OP 'MLAMBDA)))
              (COND
                ( (NOT (|symbolMember?| CURRENT-BINDING (FLAT-BV-LIST BV)))
                 (eval `(,OP ,BV ((LAMBDA (,CURRENT-BINDING) . ,BODY)
				  ',OLD-DEF)))
                   )
                ( 'T
                  (eval NEW-DEFINITION) ) ) )
            ( 'T
              `((LAMBDA (,CURRENT-BINDING) ,NEW-DEFINITION) ',OLD-DEF)))
            ) )
      (push (LIST CURRENT-BINDING NEW-DEFINITION OLD-DEF) *embedded-functions*)
      (RETURN CURRENT-BINDING) ) )

(defun UNEMBED (CURRENT-BINDING)
    (PROG (TMP E-LIST CUR-DEF)
      (SETQ E-LIST *embedded-functions*)
      (SETQ CUR-DEF (symbol-function CURRENT-BINDING))
      (COND
        ( (NOT (consp E-LIST))
          NIL )
        ( (ECQ ((CURRENT-BINDING CUR-DEF)) E-LIST)
          (SETF (symbol-function CURRENT-BINDING) (QCADDAR E-LIST))
          (SETQ *embedded-functions* (QCDR E-LIST))
          (RETURN CURRENT-BINDING) )
        ( 'T
          (SEQ
            (SETQ TMP E-LIST)
        LP  (COND
              ( (NOT (consp (QCDR TMP)))
                (EXIT NIL) )
              ( (NULL (ECQ ((CURRENT-BINDING CUR-DEF)) (QCDR TMP)))
                (SETQ TMP (QCDR TMP))
                (GO LP) )
              ( 'T
                (SETF (symbol-function  CURRENT-BINDING) (QCAR (QCDDADR TMP)))
                (RPLACD TMP (QCDDR TMP))
                (RETURN CURRENT-BINDING) ) ) ) ) )
      (RETURN NIL) ))

(defun FLAT-BV-LIST (BV-LIST)
  (PROG (TMP1)
      (RETURN
        (COND
          ( (VARP BV-LIST)
            (LIST BV-LIST) )
          ( (simple-vector-p BV-LIST)
            (FLAT-BV-LIST (VEC2LIST (MAP 'VECTOR #'FLAT-BV-LIST BV-LIST))) )
          ( (NOT (consp BV-LIST))
            NIL )
          ( (EQ '= (SETQ TMP1 (QCAR BV-LIST)))
            (FLAT-BV-LIST (QCDR BV-LIST)) )
          ( (VARP TMP1)
            (CONS TMP1 (FLAT-BV-LIST (QCDR BV-LIST))) )
          ( (AND (NOT (consp TMP1)) (NOT (simple-vector-p TMP1)))
            (FLAT-BV-LIST (QCDR BV-LIST)) )
          ( 'T
            (|append!| (FLAT-BV-LIST TMP1) (FLAT-BV-LIST (QCDR BV-LIST))) ) )) ))

(defun VARP (TEST-ITEM)
    (COND
      ( (|ident?| TEST-ITEM)
        TEST-ITEM )
      ( (AND
          (consp TEST-ITEM)
          (OR (EQ (QCAR TEST-ITEM) 'FLUID) (EQ (QCAR TEST-ITEM) 'LEX))
          (consp (QCDR TEST-ITEM))
          (|ident?| (QCADR TEST-ITEM)))
        TEST-ITEM )
      ( 'T
        NIL ) ) )

; 48.0 Miscellaneous CMS Interactions

(defun CurrentTime ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~2,'0D/~2,'0D/~2,'0D~2,'0D:~2,'0D:~2,'0D"
            month day (rem year 100) hour min sec)))

(defun $screensize () '(24 80))          ; You tell me!!

;; This isn't really compatible but is as close as you can get in common lisp
;; In place of ((one-of 1 2 3) l)  you should use
;;   (funcall (one-of 1 2 3) l)

(defun doDSETQ (form pattern exp)
  (let (PVL AVL)
    (declare (special PVL AVL))
    (COND ((|ident?| PATTERN)
           (LIST 'SETQ PATTERN EXP))
          ((AND (NOT (consp PATTERN)) (NOT (simple-vector-p PATTERN)))
           (MACRO-INVALIDARGS 'DSETQ FORM "constant target."))
          ((let* ((SV (GENSYM))
                  (E-PART (DCQGENEXP (LIST 'IDENTITY SV) PATTERN '= NIL)))
             (setq e-part
                   `(LAMBDA (,sv)
                      (PROG ,pvl
                            ,@e-part
                            (RETURN ,sv)
                         BAD (RETURN (SETQERROR ,sv)))))
             `(,e-part ,exp))))))

(defun SETQERROR (&rest FORM) (error (format nil "in destructuring ~S" FORM)))




(defun MACRO-INVALIDARGS (NAME FORM MESSAGE)
    (setq MACERRORCOUNT  (+ 1 (eval 'MACERRORCOUNT)))
    (error (format nil 
                   "invalid arguments to macro ~S with invalid argument ~S, ~S"
                   name form message)))

(defun MACRO-MISSINGARGS (NAME ignore N)
  (declare (ignore ignore))
  (setq MACERRORCOUNT (+ 1 (eval 'MACERRORCOUNT)))
  (let ((nargs (abs N)))
    (error (concatenate 'string (symbol-name NAME) " requires "
                        (if (minusp N) "at least " "exactly ")
                        (case nargs (0 "no") (1 "one") (2 "two") (3 "three")
                              (4 "four") (5 "five") (6 "six")
                              (t (princ-to-string nargs)))
                        (if (eq nargs 1) " argument," " arguments,")))))

(defun MACERR (MESSAGE &rest ignore)
  (declare (ignore ignore))
      (setq MACERRORCOUNT (+ 1 (eval 'MACERRORCOUNT)))
      (error
        (LIST "in the expression:" MESSAGE))
      ())

; 98.0 Stuff Not In The VMLisp Manual That We Like

; A version of GET that works with lists

;; GETL(SYM, KEY)
;;   KEY: a SYMBOL
;;   SYM: a SYMBOL or a LIST whose elements are SYMBOLs or LISTs.
;; Returns:
;;   when SYM is a SYMBOL, returns the KEY-property of SYM.
;;   when SYM is a LIST, returns the either the KEY-property of the
;;   first SYMBOL of SYM that has the KEY-property, or the CDR of the
;;   first cons-cell whose CAR is EQ KEY.
(defun getl (sym key)
  (cond ((symbolp sym)
         (get sym key))
        ((null sym) nil)
        ((consp sym)
         (let ((sym-1 (car sym)))
           (cond ((symbolp sym-1)
                  (get sym-1 key))
                 ((and (consp sym-1)
                       (symbolp (car sym-1)))
                  (if (eq (car sym-1) key)
                      (cdr sym-1)
                    (getl (cdr sym) key))))))))
                      
; The following should actually position the cursor at the sint'th line of the screen:

(defun $showline (cvec sint) (terpri) sint (princ cvec))

; 99.0 Ancient Stuff We Decided To Keep

(defun LAM\,FILEACTQ (name form)
  (if *FILEACTQ-APPLY*
      (FUNCALL *FILEACTQ-APPLY* name form)))

(defun PLACEP (item) (eq item *read-place-holder*))
(defun VMREAD (&optional (st |$InputStream|) (eofval *read-place-holder*))
  (read st nil eofval))
(defun |read-line| (st &optional (eofval *read-place-holder*))
  (read-line st nil eofval))

(defun gcmsg (x)
  #+(OR IBCL KCL)
  (prog1 system:*gbc-message* 
    (setq system:*gbc-message* x))
  #+:cmulisp
  (prog1 ext:*gc-verbose* 
    (setq ext:*gc-verbose* x))
  )

(defun bpiname (func)
  #+(OR IBCL KCL) (if (functionp func)
		      (cond ((symbolp func) func)
			    ((and (consp func) (eq (car func) 'LAMBDA-BLOCK))
			     (cadr func))
			    ((compiled-function-p func)
			     (system:compiled-function-name func))
			    ('t func)))
  #+:cmulisp (when (functionp func)
	       (cond
		((symbolp func) func)
		((and (consp func) 
		      (eq (car func) 'lambda)) 
		 (second (third func)))
		((compiled-function-p func)
		 (system::%primitive header-ref func
				     system::%function-name-slot))
		('else func)))
  #+(or :SBCL :clisp :ecl :clozure) (if (symbolp func)
			       func
			     (multiple-value-bind (l c n)
                                 (function-lambda-expression func)
				 (declare (ignore l c))
				 n)))

(defun make-string-adjustable (s)
  (cond ((adjustable-array-p s) s)
        (t (make-array (array-dimensions s) :element-type 'character
                       :adjustable t :initial-contents s))))

(defun make-adjustable-string (n)
  (make-array (list n) :element-type 'character :adjustable t))
 
; 6 PREDICATES
 
; 6.3 Equality Predicates
 
(defun COMPARE (X Y)
  "True if X is an atom or X and Y are lists and X and Y are equal up to X."
  (COND ((ATOM X) T)
        ((ATOM Y) NIL)
        ((EQUAL (CAR X) (CAR Y)) (COMPARE (CDR X) (CDR Y)))))
 
 
(DEFUN ?ORDER (U V)  "Multiple-type ordering relation."
  (COND ((NULL U))
        ((NULL V) NIL)
        ((ATOM U)
         (if (ATOM V)
             (COND ((NUMBERP U) (if (NUMBERP V) (> V U) T))
                   ((NUMBERP V) NIL)
                   ((|ident?| U) (AND (|ident?| V) (string> (SYMBOL-NAME V) (SYMBOL-NAME U))))
                   ((|ident?| V) NIL)
                   ((STRINGP U) (AND (STRINGP V) (string> V U)))
                   ((STRINGP V) NIL)
                   ((AND (simple-vector-p U) (simple-vector-p V))
                    (AND (> (SIZE V) (SIZE U))
                         (DO ((I 0 (1+ I)))
                             ((GT I (MAXINDEX U)) 'T)
                           (COND ((NOT (EQUAL (ELT U I) (ELT V I)))
                                  (RETURN (?ORDER (ELT U I) (ELT V I))))))))
                   ((croak "Do not understand")))
               T))
        ((ATOM V) NIL)
        ((EQUAL U V))
        ((NOT (string> (write-to-string U) (write-to-string V))))))
 
; 7 CONTROL STRUCTURE
 
; 7.8 Iteration
 
; 7.8.2 General Iteration
 
(defmacro |Zero| (&rest L) 
 (declare (ignore l)) 
 "Needed by spadCompileOrSetq" 0)
 
(defmacro |One| (&rest L)
 (declare (ignore l))
 "Needed by spadCompileOrSetq" 1)
 
 
; 10.1 The Property List
 
 
 
(defun PROPERTY (X IND N)
  "Returns the Nth element of X's IND property, if it exists."
  (let (Y) (if (AND (INTEGERP N) (SETQ Y (GET X IND)) (>= (LENGTH Y) N)) (ELEM Y N))))
 
; 10.3 Creating Symbols
 

(defvar $GENNO 0)
 
(DEFUN GENVAR () 
  (intern (strconc "$" (write-to-string (SETQ $GENNO (1+ $GENNO))))))
 
(DEFUN IS_GENVAR (X)
  (AND (|ident?| X)
       (let ((y (symbol-name x)))
         (and (char= #\$ (elt y 0)) (> (size y) 1) (digitp (elt y 1))))))
 
(DEFUN IS_\#GENVAR (X)
  (AND (|ident?| X)
       (let ((y (symbol-name x)))
         (and (char= #\# (ELT y 0)) (> (SIZE Y) 1) (DIGITP (ELT Y 1))))))
 
; 10.7 CATCH and THROW
 
; 12 NUMBERS

; 12.6 Small Finite Field ops with vector trimming
 
(defun TRIMLZ (vec)
  (declare (simple-vector vec))
  (let ((n (position 0 vec :from-end t :test-not #'eql)))
     (cond ((null n) (vector))
           ((eql n (|maxIndex| vec)) vec)
           (t (subseq vec 0 (+ n 1))))))
 
; 14 SEQUENCES
 
; 14.1 Simple Sequence Functions
 
(defun GETCHARN (A M) "Return the code of the Mth character of A"
  (let ((a (if (|ident?| a) (symbol-name a) a))) (char-code (elt A M))))
 
; 14.2 Concatenating, Mapping, and Reducing Sequences
 
(DEFUN STRINGPAD (STR N)
  (let ((M (length STR)))
    (if (>= M N)
        STR
        (concatenate 'string str (make-string (- N M) :initial-element #\Space)))))
 
(DEFUN STRINGSUFFIX (TARGET SOURCE) "Suffix source to target if enough room else nil."
  (concatenate 'string target source))
 
(defun THETACHECK (VAL VAR OP) (if (EQL VAL VAR) (THETA_ERROR OP) val))
 
; 15 LISTS
 
; 15.2 Lists
 
 
(defmacro TL (&rest L) `(tail . ,L))
 
(DEFUN LASTELEM (X) (car (|lastNode| X)))
 
(defun LISTOFATOMS (X)
  (COND ((NULL X) NIL)
        ((ATOM X) (LIST X))
        ((|append!| (LISTOFATOMS (CAR X)) (LISTOFATOMS (CDR X))))))
 
(DEFUN LASTATOM (L) (if (ATOM L) L (LASTATOM (CDR L))))
 
(DEFUN NUMOFNODES (X) (if (ATOM X) 0 (+ 1 (NUMOFNODES (CAR X)) (NUMOFNODES (CDR X)))))
 
(DEFUN TRUNCLIST (L TL) "Truncate list L at the point marked by TL."
  (let ((U L)) (TRUNCLIST-1 L TL) U))
 
(DEFUN TRUNCLIST-1 (L TL)
  (COND ((ATOM L) L)
        ((EQL (CDR L) TL) (RPLACD L NIL))
        ((TRUNCLIST-1 (CDR L) TL))))
 
; 15.5 Using Lists as Sets

(DEFUN PREDECESSOR (TL L)
  "Returns the sublist of L whose CDR is EQ to TL."
  (COND ((ATOM L) NIL)
        ((EQ TL (CDR L)) L)
        ((PREDECESSOR TL (CDR L)))))
 
(defun remdup (l) (remove-duplicates l :test #'equalp))
 
; 15.6 Association Lists
 
;;; Operations on Association Sets (AS)
 
(defun AS-INSERT (A B L)
   ;; PF(item) x PF(item) x LIST(of pairs) -> LIST(of pairs with (A . B) added)
   ;; destructive on L; if (A . C) appears already, C is replaced by B
   (cond ((null l) (list (cons a b)))
         ((equal a (caar l)) (rplacd (car l) b) l)
         ((?order a (caar l)) (cons (cons a b) l))
         (t (as-insert1 a b l) l)))
 
(defun as-insert1 (a b l)
   (cond ((null (cdr l)) (rplacd l (list (cons a b))))
         ((equal a (caadr l)) (rplacd (cadr l) b))
         ((?order a (caadr l)) (rplacd l (cons (cons a b) (cdr l))))
         (t (as-insert1 a b (cdr l)))))
 
 
; 17 ARRAYS
 
; 17.6 Changing the Dimensions of an Array
 

(defun lengthenvec (v n)
  (if 
    (and (array-has-fill-pointer-p v) (adjustable-array-p v))
    (if 
      (>= n (array-total-size v)) 
        (adjust-array v (* n 2) :fill-pointer n) 
        (progn 
          (setf (fill-pointer v) n) 
          v))
    (replace (make-array n :fill-pointer t) v)))

(defun make-init-vector (n val) 
  (make-array n :initial-element val :fill-pointer t))
 
 
; 22 INPUT/OUTPUT
 
; 22.2 Input Functions
 
; 22.2.1 Input from Character Streams
 
(DEFUN STREAM-EOF (&optional (STRM |$InputStream|))
  "T if input stream STRM is at the end or saw a ~."
  (not (peek-char nil STRM nil nil nil))     )
 
(DEFUN CONSOLEINPUTP (STRM) (|ioTerminal?| STRM))
 
(defvar $filelinenumber 0)
(defvar $prompt "--->")
(defvar stream-buffer nil)
 
(DEFUN NEXTSTRMLINE (STRM) "Returns the next input line from stream STRM."
  (let ((v (read-line strm nil -1 nil)))
    (if (equal v -1) (throw 'spad_reader nil)
        (progn (setq stream-buffer v) v))))
 
(DEFUN CURSTRMLINE (STRM)
  "Returns the current input line from the stream buffer of STRM (VM-specific!)."
  (cond (stream-buffer)
        ((stream-eof strm) (fail))
        ((nextstrmline strm))))
 
(defvar *EOF* NIL)
 
(DEFUN CURMAXINDEX (STRM)
"Something bizarre and VM-specific with respect to streams."
  (if *EOF* (FAIL) (ELT (ELT (LASTATOM STRM) 1) 3)))
 
(DEFUN ADJCURMAXINDEX (STRM)
"Something unearthly and VM-specific with respect to streams."
  (let (v) (if *eof* (fail)
               (progn (SETQ V (ELT (LASTATOM STRM) 1))
                      (SETF (ELT V 3) (SIZE (ELT V 0)))))))
 
(DEFUN STRMBLANKLINE (STRM)
"Something diabolical and VM-specific with respect to streams."
  (if *EOF* (FAIL) (AND (EQ '\  (CAR STRM)) (EQL 1 (CURMAXINDEX STRM)))))
 
(DEFUN STRMSKIPTOBLANK (STRM)
"Munch away on the stream until you get to a blank line."
  (COND (*EOF* (FAIL))
        ((PROGN (NEXTSTRMLINE STRM) (STRMBLANKLINE STRM)) STRM)
        ((STRMSKIPTOBLANK STRM))))
 
(DEFUN CURINPUTLINE () (CURSTRMLINE |$InputStream|))
 
(DEFUN NEXTINPUTLINE () (NEXTSTRMLINE |$InputStream|))
 
; 22.3 Output Functions
 
; 22.3.1 Output to Character Streams
 
(DEFUN ATOM2STRING (X)
  "Give me the string which would be printed out to denote an atom."
  (cond ((atom x) (symbol-name x))
        ((stringp x) x)
        ((write-to-string x))))
 
;; the following are redefined in MSGDB BOOT
 
(DEFUN BLANKS (N &optional (stream |$OutputStream|))
  "Print N blanks."
  (declare (fixnum N))
  (do ((i 1 (the fixnum(1+ i))))
      ((> i N))
      (declare (fixnum i))
      (princ " " stream)))
 
; 23 FILE SYSTEM INTERFACE
 
; 23.2 Opening and Closing Files
 
(DEFUN DEFSTREAM (file MODE)
       (if (member mode '(i input))
           (MAKE-INSTREAM file)
         (MAKE-OUTSTREAM file)))
 
; 23.3 Renaming, Deleting and Other File Operations
 
(DEFUN NOTE (STRM)
"Attempts to return the current record number of a file stream.  This is 0 for
terminals and empty or at-end files.  In Common Lisp, we must assume record sizes of 1!"
   (COND ((STREAM-EOF STRM) 0)
         ((|ioTerminal?| STRM) 0)
         ((file-position STRM))))
 
(DEFUN |ioTerminal?|-NOT-XEDIT (S) (not (OR (NULL (|ioTerminal?| S)))))
 
(DEFUN POINTW (RECNO STRM)
"Does something obscure and VM-specific with respect to streams."
  (let (V)
    (if (STREAM-EOF STRM) (FAIL))
    (SETQ V (LASTATOM STRM))
    (SETF (ELT V 4) RECNO)
    (SETQ *EOF* (STREAM-EOF STRM))
    strm))
 
(DEFUN POINT (RECNO STRM) (file-position strm recno))
 
(DEFUN STRM (RECNO STRM)
"Does something obscure and VM-specific with respect to streams."
  (let (V)
    (if (STREAM-EOF STRM) (FAIL))
    (SETQ V (LASTATOM STRM))
    (SETF (ELT V 4) RECNO)
    (read-char STRM)
    (SETQ *EOF* (STREAM-EOF STRM))
    strm))
 
; 25 MISCELLANEOUS FEATURES
 
;; range tests and assertions
 
(defmacro |elapsedUserTime| () '(get-internal-run-time))
 
#+IBCL
(defmacro |elapsedGcTime| () '(system:gbc-time-report))

#+AKCL
(defmacro |elapsedGcTime| () '(system:gbc-time))

#-(OR IBCL AKCL)
(defmacro |elapsedGcTime| () '0)
 
; This function was modified by Greg Vanuxem on March 31, 2005
; to handle the special case of #'(lambda ..... which expands
; into (function (lambda .....
; 
; The extra if clause fixes bugs #196 and #114
;
; an example that used to cause the failure was:
; )set func comp off
; f(xl:LIST FRAC INT): LIST FRAC INT == map(x +-> x, xl)
; f [1,2,3]
;
; which expanded into
;
; (defun |xl;f;1;initial| (|#1| |envArg|)
;  (prog (#:G1420)
;   (return 
;    (progn
;     (lett #:G1420 'uninitialized_variable |f| |#1;f;1:initial|)
;      (spadcall 
;       (cons (|function| (lambda (#:G1420 |envArg|) #:G1420)) (vector))
;       |#1|
;       (svref |*1;f;1;initial;MV| 0))))))
;
; the (|function| (lambda form used to cause an infinite expansion loop
;      
(defun macroexpandall (sexpr)
 (cond
  ((atom sexpr) sexpr)
  ((eq (car sexpr) 'quote) sexpr)
  ((eq (car sexpr) 'defun)
   (cons (car sexpr) (cons (cadr sexpr)
       (mapcar #'macroexpandall (cddr sexpr)))))
  ((and (symbolp (car sexpr)) (macro-function (car sexpr)))
   (do ()
       ((not (and (consp sexpr) (symbolp (car sexpr))
                  (macro-function (car sexpr)))))
     (setq sexpr (macroexpand sexpr)))
   (if (consp sexpr) 
     (let ((a (car sexpr)) (b (caadr sexpr)))
       (if (and (eq a 'function) (eq b 'lambda))
         (cons a (list (cons b (mapcar #'macroexpandall (cdadr sexpr)))))
         (mapcar #'macroexpandall sexpr)))
       sexpr))
  ('else        
    (mapcar #'macroexpandall sexpr))))


(defun |deleteWOC| (item list) (delete item list :test #'equal))

;;---- Added by WFS.
 
(proclaim '(ftype (function (t t) t) |subWord|)) ;hack for bug in akcl-478
 
(DEFUN |subWord| (|str| N )
  (declare (fixnum n ) (string |str|))
  (PROG (|word| (|n| 0) |inWord|(|l| 0) )
     (declare (fixnum |n| |l|))
    (RETURN
      (SEQ (COND
             ((> 1 N) NIL)
             ('T (SPADLET |l| (SPADDIFFERENCE (|#| |str|) 1))
              (COND
                ((EQL |l| 0) NIL)
                ('T (SPADLET |n| 0) (SPADLET |word| '||)
                 (SPADLET |inWord| NIL)
                 (DO ((|i| 0 (1+ |i|))) ((> |i| |l|) NIL)
               (declare (fixnum |i|))
                   (SEQ (EXIT (COND
                                ((eql (aref |str| |i|) #\space)
                                 (COND
                                   ((NULL |inWord|) NIL)
                                   ((eql |n| N) (RETURN |word|))
                                   ('T (SPADLET |inWord| NIL))))
                                ('T
                                 (COND
                                   ((NULL |inWord|)
                                    (SPADLET |inWord| 'T)
                                    (SPADLET |n| (PLUS |n| 1))))
                                 (COND
                                   ((eql |n| N)
                       (cond ((eq |word| '||)
                           (setq |word|
                           (make-array 10 :adjustable t
                                    :element-type 'standard-char
                                  :fill-pointer 0))))
                       (or |word| (error "bad"))
                       (vector-push-extend (aref |str| |i|)
                                  (the string |word|)
                                  )
                       )
                                   ('T NIL)))))))
                 (COND ((> N |n|) NIL) ('T |word|))))))))))

(defun print-full (expr &optional (stream |$OutputStream|))
   (let ((*print-circle* t) (*print-array* t) *print-level* *print-length*)
     (print expr stream)
     (terpri stream)
     (finish-output stream)))

;; stream handling for paste-in generation

(defun |applyWithOutputToString| (func args)
  ;; returns the cons of applying func to args and a string produced
  ;; from standard-output while executing.
  (let* ((out-stream (make-string-output-stream))
         (curoutstream out-stream)
         (|$algebraOutputStream| out-stream)
         (|$OutputStream| out-stream)
        val)
    (declare (special curoutstream |$algebraOutputStream|))
    (setq val (catch |$SpadReaderTag|
                (catch |$intTopLevel|
                  (apply (symbol-function func) args))))
    (cons val (get-output-stream-string |$OutputStream|))))

(defun |breakIntoLines| (str)
  (let ((bol 0) (eol) (line-list nil))
    (loop
     (setq eol (|findChar| #\Newline str bol))
     (if (null eol) (return))
     (if (> eol bol) 
         (setq line-list (cons (subseq str bol eol) line-list)))
     (setq bol (+ eol 1)))
    (|reverse!| line-list)))


(defvar HT nil)
