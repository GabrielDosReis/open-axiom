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

(defvar *lam-name* nil "name to be used by lam macro if non-nil")

(defvar macerrorcount 0  "Put some documentation in here someday")

(defvar *read-place-holder* (make-symbol "%.EOF")
   "default value returned by read and read-line at end-of-file")

;; DEFMACROS


(defmacro absval (x)
 `(abs ,x))

#-:CCL
(defmacro add1 (x)
 `(1+ ,x))

(defmacro assemble (&rest ignore) 
 (declare (ignore ignore))
  nil)

(defmacro applx (&rest args)
 `(apply ,@args))

#-(or LispM Lucid :CCL)
(defmacro assq (a b)
 `(assoc ,a ,b :test #'eq))

#+:CCL
(defmacro assq (a b) `(atsoc ,a ,b))

#-:CCL
(defmacro bintp (n)
 `(typep ,n 'bignum))
#+:CCL
(defun bintp (n) (and (integerp n) (not (fixp n))))

(defmacro closedfn (form)
 `(function ,form))

(defmacro |copyList| (x) 
 `(copy-list ,x))

(defmacro cvecp (x)
 `(stringp ,x))

(defmacro dcq (&rest args)
 (cons 'setqp args))

#-:CCL
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

#-:CCL
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

(defmacro evalandfileactq (name &optional (form name))
 `(eval-when 
   #+:common-lisp (:load-toplevel :execute)
   #-:common-lisp (eval load)
   ,form))  

(defmacro exit (&rest value)
 `(return-from seq ,@value))

(defmacro fetchchar (x i)
 `(char ,x ,i))

#-:CCL  ;; fixp in ccl tests for fixnum
(defmacro fixp (x)
 `(integerp ,x))

#-:CCL
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
 
(defmacro identp (x) 
 (if (atom x)
  `(and ,x (symbolp ,x))
   (let ((xx (gensym)))
    `(let ((,xx ,x))
      (and ,xx (symbolp ,xx))))))

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

(defmacro intp (x)
 `(integerp ,x))

(defmacro lam (&rest body)
 (list 'quote (*lam (copy-tree body))))

(defmacro lastnode (l)
 `(last ,l))

(defmacro lastpair (l)
 `(last ,l))

#-:CCL
(defmacro lessp (&rest args)
 `(< ,@args))

(defmacro lintp (n)
 `(typep ,n 'bignum))

(defmacro makestring (a) a)

(defmacro mapelt (f vec)
 `(map 'vector ,f ,vec))

(defmacro maxindex (x)
 `(the fixnum (1- (the fixnum (length ,x)))))

#-(or LispM Lucid :CCL)
(defmacro memq (a b)
 `(member ,a ,b :test #'eq))

#-:CCL
(defmacro minus (x)
 `(- ,x))

(defmacro mrp (x)
 `(special-form-p ,x))

(defmacro namederrset (id iexp &rest item)
 (declare (ignore item))
  `(catch ,id ,iexp))

(defmacro ne (a b) `(not (equal ,a ,b)))

;;; This may need adjustment in CCL where NEQ means (NOT (EQUAL ..)))
#-:CCL
(defmacro neq (a b) `(not (eq ,a ,b)))

#-:CCL
(defmacro nreverse0 (x)
  (if (atom x)
      `(if (atom ,x) ,x (nreverse ,x))
    (let ((xx (gensym)))
      `(let ((,xx ,x))
         (if (atom ,xx) ,xx (nreverse ,xx))))))

(defmacro nump (n)
 `(numberp ,n))

(defmacro oraddtempdefs (filearg)
 `(eval-when 
   #+:common-lisp (:compile-toplevel)
   #-:common-lisp (compile)
   (load ,filearg)))

(defmacro pairp (x)
 `(consp ,x)) 

#-:CCL
(defmacro plus (&rest args)
 `(+ ,@ args))

; (defmacro qassq (a b)
;  `(assoc ,a ,b :test #'eq))
(defmacro qassq (a b) `(assq ,a ,b))

#-:CCL
(defmacro qcar (x)
 `(car (the cons ,x)))
#-:CCL
(defmacro qcdr (x)
 `(cdr (the cons ,x)))

#-:CCL
(defmacro qcaar (x)
 `(car (the cons (car (the cons ,x)))))
#-:CCL
(defmacro qcadr (x)
 `(car (the cons (cdr (the cons ,x)))))
#-:CCL
(defmacro qcdar (x)
 `(cdr (the cons (car (the cons ,x)))))
#-:CCL
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

(defmacro qcaaaar (x)
 `(car (the cons (car (the cons (car (the cons (car (the cons ,x)))))))))
(defmacro qcaaadr (x)
 `(car (the cons (car (the cons (car (the cons (cdr (the cons ,x)))))))))
(defmacro qcaadar (x)
 `(car (the cons (car (the cons (cdr (the cons (car (the cons ,x)))))))))
(defmacro qcaaddr (x)
 `(car (the cons (car (the cons (cdr (the cons (cdr (the cons ,x)))))))))
(defmacro qcadaar (x)
 `(car (the cons (cdr (the cons (car (the cons (car (the cons ,x)))))))))
(defmacro qcadadr (x)
 `(car (the cons (cdr (the cons (car (the cons (cdr (the cons ,x)))))))))
(defmacro qcaddar (x)
 `(car (the cons (cdr (the cons (cdr (the cons (car (the cons ,x)))))))))
(defmacro qcadddr (x)
 `(car (the cons (cdr (the cons (cdr (the cons (cdr (the cons ,x)))))))))
(defmacro qcdaaar (x)
 `(cdr (the cons (car (the cons (car (the cons (car (the cons ,x)))))))))
(defmacro qcdaadr (x)
 `(cdr (the cons (car (the cons (car (the cons (cdr (the cons ,x)))))))))
(defmacro qcdadar (x)
 `(cdr (the cons (car (the cons (cdr (the cons (car (the cons ,x)))))))))
(defmacro qcdaddr (x)
 `(cdr (the cons (car (the cons (cdr (the cons (cdr (the cons ,x)))))))))
(defmacro qcddaar (x)
 `(cdr (the cons (cdr (the cons (car (the cons (car (the cons ,x)))))))))
(defmacro qcddadr (x)
 `(cdr (the cons (cdr (the cons (car (the cons (cdr (the cons ,x)))))))))
(defmacro qcdddar (x)
 `(cdr (the cons (cdr (the cons (cdr (the cons (car (the cons ,x)))))))))
(defmacro qcddddr (x)
 `(cdr (the cons (cdr (the cons (cdr (the cons (cdr (the cons ,x)))))))))

(defmacro qcsize (x)
 `(the fixnum (length (the simple-string ,x))))

(defmacro qeqq (pattern exp)
 `(,(ecqexp pattern 1) ,exp))

(defmacro qlength (a)
 `(length ,a))

; (defmacro qmemq (a b)
; `(member ,a ,b :test #'eq))
(defmacro qmemq (a b) `(memq ,a ,b))

(defmacro qrefelt (vec ind)
 `(svref ,vec ,ind))

(defmacro qrplaca (a b)
 `(rplaca (the cons ,a) ,b))

(defmacro qrplacd (a b)
 `(rplacd (the cons ,a) ,b))

(defmacro qrplq (&whole form pattern exp)
 (if (or (consp pattern) (simple-vector-p pattern))
  `(,(rcqexp pattern) ,exp)
   (macro-invalidargs 'qrplq form "form must be updateable.")))

(defmacro qsadd1 (x)
 `(the fixnum (1+ (the fixnum ,x))))

(defmacro qsdec1 (x)
 `(the fixnum (1- (the fixnum ,x))))

(defmacro qsdifference (x y)
 `(the fixnum (- (the fixnum ,x) (the fixnum ,y))))

(defmacro qsetrefv (vec ind val)
 `(setf (svref ,vec (the fixnum ,ind)) ,val))

(defmacro qsetvelt (vec ind val)
 `(setf (svref ,vec (the fixnum ,ind)) ,val))

(defmacro qsetvelt-1 (vec ind val)
 `(setf (svref ,vec (the fixnum (1- (the fixnum ,ind)))) ,val))

(defmacro qsgreaterp (a b)
 `(> (the fixnum ,a) (the fixnum ,b)))

(defmacro qsinc1 (x)
 `(the fixnum (1+ (the fixnum ,x))))

(defmacro qsleftshift (a b)
 `(the fixnum (ash (the fixnum ,a) (the fixnum ,b))))

(defmacro qslessp (a b)
 `(< (the fixnum ,a) (the fixnum ,b)))

(defmacro qsmax (x y)
 `(the fixnum (max (the fixnum ,x) (the fixnum ,y))))

(defmacro qsmin (x y)
 `(the fixnum (min (the fixnum ,x) (the fixnum ,y))))

(defmacro qsminus (x)
 `(the fixnum (minus (the fixnum ,x))))

(defmacro qsminusp (x)
 `(minusp (the fixnum ,x)))

(defmacro qsoddp (x)
 `(oddp (the fixnum ,x)))

(defmacro qsabsval (x)
  `(the fixnum (abs (the fixnum ,x))))

(defmacro qsplus (x y)
 `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))

(defmacro qssub1 (x)
 `(the fixnum (1- (the fixnum ,x))))

(defmacro qstimes (x y)
 `(the fixnum (* (the fixnum ,x) (the fixnum ,y))))

(defmacro qstringlength (x)
 `(the fixnum (length (the simple-string ,x))))

(defmacro qszerop (x)
 `(zerop (the fixnum ,x)))

(defmacro qvelt (vec ind)
 `(svref ,vec (the fixnum ,ind)))

(defmacro qvelt-1 (vec ind)
 `(svref ,vec (the fixnum (1- (the fixnum ,ind)))))

(defmacro qvmaxindex (x)
 `(the fixnum (1- (the fixnum (length (the simple-vector ,x))))))

(defmacro qvsize (x)
 `(the fixnum (length (the simple-vector ,x))))

; #-:CCL
; (defmacro refvecp (v)
;  `(typep ,v '(vector t)))
; #+:CCL
; (defun refvecp (v) (and (vectorp v) (not (stringp v))))
(defmacro refvecp (v) `(simple-vector-p ,v))

(defmacro resetq (a b)
 `(prog1 ,a (setq ,a ,b)))

(defmacro rnump (n)
 `(floatp ,n))

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

#-:CCL
(defmacro setelt (vec ind val)
 `(setf (elt ,vec ,ind) ,val))

(defmacro setqp (&whole form pattern exp)
  `(,(dcqexp pattern '=) ,exp))

(defmacro seq (&rest form)
  (let* ((body (reverse form))
         (val `(return-from seq ,(pop body))))
    (nsubstitute '(progn) nil body) ;don't treat NIL as a label
    `(block seq (tagbody ,@(nreverse body) ,val))))

(defmacro sfp (x)
 `(special-form-p ,x))

#-:CCL
(defmacro sintp (n)
 `(typep ,n 'fixnum))
#+:CCL
(defmacro sintp (n)
  `(fixp ,n))

#-:CCL
(defmacro smintp (n)
 `(typep ,n 'fixnum))
#+:CCL
(defmacro smintp (n)
  `(fixp ,n))

(defmacro stringlength (x)
 `(length (the string ,x)))

(defmacro subrp (x)
 `(compiled-function-p ,x))

#-:CCL
(defmacro sub1 (x)
 `(1- ,x))

(defmacro throw-protect (exp1 exp2)
 `(unwind-protect ,exp1 ,exp2))

#-:CCL
(defmacro times (&rest args)
 `(* ,@args))

(defmacro vec-setelt (vec ind val)
 `(setf (svref ,vec ,ind) ,val))

; #-:CCL
; (defmacro vecp (v)
;  `(typep ,v '(vector t)))
; #+:CCL
; (defun vecp (v) (and (vectorp v) (not (stringp v))))
(defmacro vecp (v) `(simple-vector-p ,v))

#-:CCL
(defmacro zero? (x)
  `(and (typep ,x 'fixnum) (zerop (the fixnum ,x))))
#+:CCL
(defmacro zero? (x) `(zerop ,x))

;; defuns

(define-function 'tempus-fugit #'get-internal-run-time)

(defun $TOTAL-ELAPSED-TIME ()
   (list (get-internal-run-time) (get-internal-real-time)))

#-(OR IBCL KCL :CMULISP :CCL)
(defun $TOTAL-GC-TIME () (list 0 0))

#+:CCL
(defun $TOTAL-GC-TIME () (list (gctime) (gctime)))

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

; 7.0 Macros

; 7.2 Creating Macro Expressions

; 5.2 Functions

; 5.2.2 Lambda Expressions

(defun *LAM (body)
  (cond  ((NOT (ISQUOTEDP (first BODY))) (cons 'LAMBDA BODY))
         ((LET* ((BV (DEQUOTE (first BODY)))
                 (CONTROL (QUOTESOF (first BODY)))
                 (BODY (cdr BODY))
                 (ARGS (GENSYM))
                 (INNER-FUNC (or *lam-name* (gentemp))))
            (COMP370 (LIST INNER-FUNC `(LAMBDA ,BV . ,BODY)))
            `(MLAMBDA ,ARGS
                      (CONS (QUOTE ,INNER-FUNC)
                            (WRAP (cdr ,ARGS) ',CONTROL)))))))

(defun WRAP (LIST-OF-ITEMS WRAPPER)
 (prog nil
  (COND ((OR (NOT (PAIRP LIST-OF-ITEMS)) (not WRAPPER))
         (RETURN LIST-OF-ITEMS))
        ((NOT (consp WRAPPER))
         (SETQ WRAPPER (LOTSOF WRAPPER))))
  (RETURN
    (CONS (if (first WRAPPER)
              `(,(first WRAPPER) ,(first LIST-OF-ITEMS))
              (first LIST-OF-ITEMS))
          (WRAP (cdr LIST-OF-ITEMS) (cdr WRAPPER))))))

(defun ISQUOTEDP (bv)
  (COND ((NOT (consp BV)) NIL)
        ((EQ (first BV) 'QUOTE))
        ((AND (consp (first BV)) (EQ (QCAAR BV) 'QUOTE)))
        ((ISQUOTEDP (cdr BV)))))

(defun QUOTESOF (BV)
  (COND ((NOT (consp BV)) NIL)
      ((EQ (first BV) 'QUOTE) 'QUOTE)
      ((CONS (COND ((NOT (consp (first BV))) nil)
                   ((EQ (QCAAR BV) 'QUOTE) 'QUOTE)
                   (T NIL))
             (QUOTESOF (cdr BV))))))

(defun DEQUOTE (BV)
  (COND ((NOT (consp BV)) BV)
        ((EQ 'QUOTE (first BV)) (second BV))
        ((CONS (if (EQ 'QUOTE (IFCAR (CAR BV))) (CADAR BV) (first BV))
               (DEQUOTE (cdr BV))))))

(defun lotsof (&rest items)
  (setq items (copy-list items))
  (nconc items items))

; 7.4 Using Macros

; Beats me how to simulate macro expansion "in the environment of sd"...:

(defun MDEF (arg item &optional sd)
 (declare (ignore sd))
  (macroexpand `(,arg ,item)))

(define-function 'MDEFX #'MDEF)

; 8.0 Operator Definition and Transformation

; 8.1 Definition and Transformation Operations

(defun COMP370 (fnlist)
  (cond ((atom (car fnlist)) (list (COMPILE1 fnlist)))
        (t (MAPCAR #'(lambda (x) (COMPILE1 x)) fnlist))))

#+:CCL (proclaim '(special *vars* *decl*)) ;; declare not handled right

(defun COMPILE1 (fn)
  (let* (nargs
         (fname (car fn))
         (lamda (cadr fn))
         (ltype (car lamda))
         *vars* *decl* args
         (body (cddr lamda)))
    (declare (special *vars* *decl*))
    (if (eq ltype 'LAM)
        (let ((*lam-name* (intern (concat fname "\,LAM"))))
          (setq lamda (eval lamda) ltype (car lamda) body (cddr lamda))))
    (let ((dectest (car body)))
      (if (and (eqcar dectest 'declare) (eqcar (cadr dectest) 'special))
          (setq *decl* (cdr (cadr dectest)) body (cdr body))))
    (setq args (remove-fluids (cadr lamda)))
    (cond ((and (eq ltype 'lambda) (simple-arglist args)) (setq nargs args))
          (t (setq nargs (gensym))
     #+LispM (setq body `((dsetq ,args (copy-list ,nargs)) ,@body))
     #-LispM (setq body `((dsetq ,args  ,nargs) ,@body))
             (cond ((eq ltype 'lambda) (setq nargs `(&rest ,nargs &aux ,@*vars*)))
                   ((eq ltype 'mlambda)
                    (setq nargs `(&whole ,nargs &rest ,(gensym) &aux ,@*vars*)))
                   (t (error "bad function type")))))
    (cond (*decl* (setq body (cons `(declare (special ,@ *decl*)) body))))
    (setq body
          (cond ((eq ltype 'lambda) `(defun ,fname ,nargs . ,body))
                ((eq ltype 'mlambda) `(defmacro ,fname ,nargs . ,body))))
    (if *COMP370-APPLY* (funcall *COMP370-APPLY* fname body))

    body))

(defun simple-arglist (arglist)
  (or (null arglist)
      (and (consp arglist) (null (cdr (last arglist)))
           (every #'symbolp arglist))))

(defun remove-fluids (arglist &aux f v) ;updates specials *decl* and *vars*
  (declare (special *decl* *vars*))
   (cond ((null arglist) arglist)
         ((symbolp arglist) (push arglist *vars*) arglist)
                ;if atom but not symbol, ignore value
         ((atom arglist) (push (setq arglist (gentemp)) *vars*) arglist)
         ((and (setq f (car arglist))
               (eq f 'fluid)
               (listp (cdr arglist))
               (setq v (cadr arglist))
               (identp v)
               (null (cddr arglist)))
          (push v *decl*)
          (push v *vars*)
          v)
         (t (cons (remove-fluids (car arglist))
                  (remove-fluids (cdr arglist))))))

(define-function 'KOMPILE #'COMP370)

; 9.4 Vectors and Bpis

(defun IVECP (x) (and (vectorp x) (subtypep (array-element-type x) 'integer)))

(defun mbpip (item) (and (symbolp item) ;cannot know a compiled macro in CLISP
                         (compiled-function-p (macro-function item))))

(defun FBPIP (item) (or (compiled-function-p item)
                        (and (symbolp item) (fboundp item)
                             (not (macro-function item))
                             (compiled-function-p (symbol-function item)))))

; 9.5 Identifiers

#-:CCL
(defun gensymp (x) (and (symbolp x) (null (symbol-package x))))

(defun digitp (x)
  (or (and (symbolp x) (digitp (symbol-name x)))
      (and (characterp x) (digit-char-p x))
      (and (stringp x) (= (length x) 1) (digit-char-p (char x 0)))))

(defun dig2fix (x)
  (if (symbolp x)
    (digit-char-p (char (symbol-name x) 0))
    (digit-char-p x)))

#-:CCL
(defun LN (x) (LOG x))
#-:CCL
(defun LOG2 (x) (LOG x 2.0))
(defun |log| (x) (LOG x 10.0))

; 9.13 Streams

#+Lucid
(defun IS-CONSOLE (stream)
    (and (streamp stream)
         (or (not (consp (pathname-directory stream)))
             (equal (qcar (pathname-directory stream)) "dev")
             (null (pathname-name stream) ))))

#+KCL
(defun IS-CONSOLE (stream)
  (and (streamp stream) (output-stream-p stream)
       (eq (system:fp-output-stream stream)
           (system:fp-output-stream *terminal-io*))))

#-(OR Lucid KCL :CCL)
(defun IS-CONSOLE (stream)
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
        ((identp l) (intern (string-upcase (symbol-name l))))
        ((characterp l) (char-upcase l))
        ((atom l) l)
        (t (mapcar #'upcase l))))

(define-function 'U-CASE #'upcase)
(define-function 'LC2UC #'upcase)

(defun downcase (l)
  (cond ((stringp l) (string-downcase l))
        ((identp l) (intern (string-downcase (symbol-name l))))
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
#-:CCL
   (plist2alist (symbol-plist x))
#+:CCL
   (plist2alist (plist x))
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

; 12.2 Predicates

;(define-function 'lessp #'<)

;(define-function 'greaterp #'>)


;(define-function 'fixp #'integerp)

; 12.3 Computation

;(define-function 'add1 #'1+)
;(define-function 'sub1 #'1-)
;(define-function 'plus #'+)
;(define-function 'times #'*)
;(define-function 'difference #'-)
;(define-function 'minus #'-)
;(define-function 'absval #'abs)

(defun QUOTIENT (x y)
  (cond ((or (floatp x) (floatp y)) (/ x y))
        (t (truncate x y))))

(define-function 'vm/ #'quotient)

#-:CCL
(defun REMAINDER (x y)
  (if (and (integerp x) (integerp y))
      (rem x y)
      (- x (* y (QUOTIENT x y)))))

#-:CCL
(defun DIVIDE (x y)
  (if (and (integerp x) (integerp y))
      (multiple-value-list (truncate x y))
      (list (QUOTIENT x y) (REMAINDER x y))))

(defun QSQUOTIENT (a b) (the fixnum (truncate (the fixnum a) (the fixnum b))))

(defun QSREMAINDER (a b) (the fixnum (rem (the fixnum a) (the fixnum b))))


;(defun IFCAR (x) (if (consp x) (car (the cons x))))

;(defun IFCDR (x) (if (consp x) (cdr (the cons x))))

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

; note default test for union, intersection and set-difference is eql
;; following are defined so as to preserve ordering in union.lisp
;;(defun SETDIFFERENCE (l1 l2) (set-difference l1 l2 :test #'equalp))
;;(defun SETDIFFERENCEQ (l1 l2) (set-difference l1 l2 :test #'eq))
;;(defun |union| (l1 l2) (union l1 l2 :test #'equalp))
;;(defun UNIONQ (l1 l2) (union l1 l2 :test #'eq))
;;(defun |intersection| (l1 l2) (intersection l1 l2 :test #'equalp))
;;(defun INTERSECTIONQ (l1 l2) (intersection l1 l2 :test #'eq))
(defun |member| (item sequence)
   (cond ((symbolp item) (member item sequence :test #'eq))
         ((stringp item) (member item sequence :test #'equal))
         ((and (atom item) (not (arrayp item))) (member item sequence))
         (T (member item sequence :test #'equalp))))

(defun |remove| (list item &optional (count 1))
  (if (integerp count)
      (remove item list :count count :test #'equalp)
      (remove item list :test #'equalp)))

(defun REMOVEQ (list item &optional (count 1))
  (if (integerp count)
      (remove item list :count count :test #'eq)
      (remove item list :test #'eq)))

; 14.2 Accessing

;(define-function 'lastnode #'last)
;(define-function 'lastpair #'last)
(defun |last| (x) (car (lastpair x)))

; 14.3 Searching

#+:CCL (DEFMACRO |assoc| (X Y) `(ASSOC** ,X ,Y))
#-:CCL
(DEFUN |assoc| (X Y)
  "Return the pair associated with key X in association list Y."
  ; ignores non-nil list terminators
  ; ignores non-pair a-list entries
  (cond ((symbolp X)
         (PROG NIL
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQ (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))
        ((or (numberp x) (characterp x))
         (PROG NIL
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQL (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))
        (t
         (PROG NIL
               A  (COND ((ATOM Y) (RETURN NIL))
                        ((NOT (consp (CAR Y))) )
                        ((EQUAL (CAAR Y) X) (RETURN (CAR Y))) )
               (SETQ Y (CDR Y))
               (GO A)))))
; 14.5 Updating

(defun NREMOVE (list item &optional (count 1))
  (if (integerp count)
      (delete item list :count count :test #'equal)
      (delete item list :test #'equal)))

(defun NREMOVEQ (list item &optional (count 1))
  (if (integerp count)
      (delete item list :count count )
      (delete item list )))

(defun EFFACE (item list) (delete item list :count 1 :test #'equal))

(defun NCONC2 (x y) (NCONC x y)) ;NCONC with exactly two arguments

; 14.6 Miscellaneous

(defun QSORT (l)
 (declare (special sortgreaterp))
  (NREVERSE (sort (copy-seq l) SORTGREATERP)))

(defun SORTBY (keyfn l)
 (declare (special sortgreaterp))
  (nreverse (sort (copy-seq l) SORTGREATERP :key keyfn)))

; 16.0 Operations on Vectors

; 16.1 Creation

(defun MAKE-VEC (n) (make-array n))

(defun GETREFV (n)
  (make-array n :initial-element nil))

(defun LIST2VEC (list)
 (if (consp list)
  (let* ((len (length list))
         (vec (make-array len)))
    (dotimes (i len)
     (setf (aref vec i) (pop list)))
    vec)
  (coerce list 'vector)))

(define-function 'LIST2REFVEC #'LIST2VEC)

; 16.2 Accessing


;(define-function 'FETCHCHAR #'char)

;; Oddly, LENGTH is more efficient than LIST-LENGTH in CCL, since the former
;; is compiled and the latter is byte-coded!
(defun size (l) 
  (cond ((vectorp l) (length l))
#+:CCL  ((stringp l) (length l)) ;; Until ACN fixes his lisp -> C translator.
#-:CCL  ((consp l)   (list-length l))
#+:CCL  ((consp l)   (length l))
        (t           0)))

(define-function 'MOVEVEC #'replace)

; 17.0 Operations on Character and Bit Vectors

(defun charp (a) (or (characterp a)
                     (and (identp a) (= (length (symbol-name a)) 1))))

(defun NUM2CHAR (n) (code-char n))

(defun CHAR2NUM (c) (char-code (character c)))

;; Returns truthvalue (nil or t) if s1 compares lexicographically 
;; greater than s2.  Note: It is essential that this function returns
;; a canonical Boolean value because parts of the system use it in 
;; contexts where generalized Boolean values are confusing.
(defun CGREATERP (s1 s2) 
  (cond ((string> (string s1) (string s2)) t)))


(define-function 'STRGREATERP #'CGREATERP)

; 17.1 Creation


(define-function 'strconc #'concat)

(defun make-cvec (sint) (make-array sint :fill-pointer 0 :element-type 'character))

;(define-function 'CVECP #'stringp)

(define-function 'getstr #'make-cvec)

(defun make-full-cvec (sint &optional (char #\space))
  (make-string sint :initial-element (character char)))

(define-function 'getfullstr #'make-full-cvec)

; 17.2 Accessing

(defun QENUM (cvec ind) (char-code (char cvec ind)))

(defun QESET (cvec ind c)
  (setf (char cvec ind) c))

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

;;- (defun strpos (what in start dontcare)
;;-    (setq what (string what) in (string in))
;;-    (if dontcare (progn (setq dontcare (character dontcare))
;;-                    (search what in :start2 start
;;-                            :test #'(lambda (x y) (or (eql x dontcare)
;;-                                                      (eql x y)))))
;;-                 (search what in :start2 start)))

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

(define-function 'changelength #'setsize)

(defun trimstring (x) x)

;;-- (defun rplacstr (cvec1 start1 length1 cvec2
;;--                        &optional (start2 0) (length2 nil)
;;--                        &aux end1 end2)
;;--   (setq cvec2 (string cvec2))
;;--   (if (null start1) (setq start1 0))
;;--   (if (null start2) (setq start2 0))
;;--   (if (null length1) (setq length1 (- (length cvec1) start1)))
;;--   (if (null length2) (setq length2 (- (length cvec2) start2)))
;;--   (if (numberp length1) (setq end1 (+ start1 length1)))
;;--   (if (numberp length2) (setq end2 (+ start2 length2)))
;;--   (if (/= length1 length2)
;;--       (concatenate 'string (subseq cvec1 0 start1)
;;--                            (subseq cvec2 start2 end2)
;;--                            (subseq cvec1 end1))
;;--       (replace cvec1 cvec2 :start1 start1 :end1 end1
;;--              :start2 start2 :end2 end2)))

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
(define-function 'MSUBSTQ #'subst) ;default test is eql
(define-function 'SUBSTQ #'SUBST) ;default test is eql subst is not guaranteed to copy

(defun copy (x) (copy-tree x)) ; not right since should descend vectors

(defun eqsubstlist (new old list) (sublis (mapcar #'cons old new) list))

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
          ((IDENTP FORM) (RETURN `((setq ,form ,sv)) ))
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
             (setq C (NCONC (COND ((IDENTP A) `((setq ,a (ELT ,sv ,i))))
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
               ((OR (NOT (EQ 3 (LENGTH FORM))) (NOT (IDENTP (car (setq FORM (cdr FORM))))))
                (MACRO-INVALIDARGS 'DCQ\/QDCQ FORM (MAKESTRING "invalid pattern.")))
               (`((setq ,(car form) ,sv)  ,@(DCQGENEXP SV (CADR FORM) EQTAG QFLAG)))))))
    (setq A (car FORM))
    (setq D (cdr FORM))
    (setq C (COND ((IDENTP A) `((setq ,a (CAR ,sv))))
                  ((OR (consp A) (simple-vector-p A))
                   (COND ((AND (NULL D) (IDENTP SV)) )
                         ((COND ((consp AVL) (setq W (car (RESETQ AVL (cdr AVL)))))
                                ((setq PVL (CONS (setq W (GENSYM)) PVL)) ) ) ) )
                   (COND ((AND (consp A) EQTAG (EQ (car A) EQTAG))
                          (DCQGENEXP (LIST 'CAR SV) A EQTAG QFLAG) )
                         (`((setq ,(or w sv) (CAR ,sv))
                            ,@(DCQGENEXP (OR W SV) A EQTAG QFLAG)))))))
    (setq C (NCONC C (COND ((IDENTP D) `((setq ,d (CDR ,sv))))
                           ((OR (consp D) (simple-vector-p D))
                            (COND
                              ((OR W (IDENTP SV)) )
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
              (IDENTP FORM)
              (NUMP FORM)
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
                    (NCONC
                      (COND
                        ( (OR
                            (IDENTP A)
                            (NUMP A)
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
                ( (OR (IDENTP A) (NUMP A) (AND (consp A) (EQ (car A) 'QUOTE)))
                 `((COND ((NOT (EQ ,a (CAR ,sv))) (GO BAD))) ))
                ( (OR (consp A) (simple-vector-p A))
                 `((setq ,w (CAR ,sv))
                   ,@(ECQGENEXP W A QFLAG)))))
        (setq C
              (NCONC
                C
                (COND
                  ( (OR (IDENTP D) (NUMP D) (AND (consp D)
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
              (NCONC
                (COND
                  ( (OR
                      (IDENTP A)
                      (NUMP A)
                      (AND (consp A) (EQ (car A) 'QUOTE)))
                    `((SETELT ,sv ,i ,a)))
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
          ( (OR (IDENTP A) (NUMP A) (AND (consp A) (EQ (car A) 'QUOTE)))
            `((rplaca ,sv ,a)))
          ( (OR (consp A) (simple-vector-p A))
            `((setq ,w (CAR ,sv))
              ,@(RCQGENEXP W A QFLAG)))))
      (setq C
        (NCONC
          C
          (COND
            ( (OR (IDENTP D) (NUMP D) (AND (consp D) (EQ (car D) 'QUOTE)))
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

;(define-function 'prin2cvec #'write-to-string)
(define-function 'prin2cvec #'princ-to-string)
;(define-function 'stringimage #'write-to-string)
(define-function 'stringimage #'princ-to-string)

(define-function 'printexp #'princ)
(define-function 'prin0  #'prin1)

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

(defun shut (st) (if (is-console st) st
                   (if (streamp st) (close st) -1)))

(defun EOFP (stream) (null (peek-char nil stream nil nil)))

; 28.0 Key addressed I/O


; 46.0 Call tracing


(defun EMBEDDED () (mapcar #'car *embedded-functions*))

(defun EMBED (CURRENT-BINDING NEW-DEFINITION)
  (PROG
#+:CCL (OP BV BODY OLD-DEF *COMP)
#-:CCL (OP BV BODY OLD-DEF)
      (COND
        ( (NOT (IDENTP CURRENT-BINDING))
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
                ( (NOT (MEMQ CURRENT-BINDING (FLAT-BV-LIST BV)))
                 (eval `(,OP ,BV ((LAMBDA (,CURRENT-BINDING) . ,BODY)
				  ',OLD-DEF)))
                   )
                ( 'T
                  (eval NEW-DEFINITION) ) ) )
            ( 'T
              `((LAMBDA (,CURRENT-BINDING) ,NEW-DEFINITION) ',OLD-DEF)))
            ) )
#+:CCL (IF (CONSP NEW-DEFINITION) (SETQ NEW-DEFINITION (CDR NEW-DEFINITION)))
      (push (LIST CURRENT-BINDING NEW-DEFINITION OLD-DEF) *embedded-functions*)
      (RETURN CURRENT-BINDING) ) )

(defun UNEMBED (CURRENT-BINDING)
    (PROG
#+:CCL  (TMP E-LIST CUR-DEF *COMP)
#-:CCL  (TMP E-LIST CUR-DEF)
      (SETQ E-LIST *embedded-functions*)
      (SETQ CUR-DEF (symbol-function CURRENT-BINDING))
#+:CCL (IF (CONSP CUR-DEF) (SETQ CUR-DEF (CDR CUR-DEF)))
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
          ( (REFVECP BV-LIST)
            (FLAT-BV-LIST (VEC2LIST (MAPELT #'FLAT-BV-LIST BV-LIST))) )
          ( (NOT (consp BV-LIST))
            NIL )
          ( (EQ '= (SETQ TMP1 (QCAR BV-LIST)))
            (FLAT-BV-LIST (QCDR BV-LIST)) )
          ( (VARP TMP1)
            (CONS TMP1 (FLAT-BV-LIST (QCDR BV-LIST))) )
          ( (AND (NOT (consp TMP1)) (NOT (REFVECP TMP1)))
            (FLAT-BV-LIST (QCDR BV-LIST)) )
          ( 'T
            (NCONC (FLAT-BV-LIST TMP1) (FLAT-BV-LIST (QCDR BV-LIST))) ) )) ))

(defun VARP (TEST-ITEM)
    (COND
      ( (IDENTP TEST-ITEM)
        TEST-ITEM )
      ( (AND
          (consp TEST-ITEM)
          (OR (EQ (QCAR TEST-ITEM) 'FLUID) (EQ (QCAR TEST-ITEM) 'LEX))
          (consp (QCDR TEST-ITEM))
          (IDENTP (QCADR TEST-ITEM)))
        TEST-ITEM )
      ( 'T
        NIL ) ) )

; 48.0 Miscellaneous CMS Interactions

(defun CurrentTime ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~2,'0D/~2,'0D/~2,'0D~2,'0D:~2,'0D:~2,'0D"
            month day (rem year 100) hour min sec)))

(defun $screensize () '(24 80))          ; You tell me!!

; 97.0 Stuff In The Manual But Wierdly Documented

(defun EBCDIC (x) (code-char x))

;; This isn't really compatible but is as close as you can get in common lisp
;; In place of ((one-of 1 2 3) l)  you should use
;;   (funcall (one-of 1 2 3) l)

(defun doDSETQ (form pattern exp)
  (let (PVL AVL)
    (declare (special PVL AVL))
    (COND ((IDENTP PATTERN)
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

#+Lucid
(defun numberofargs (x)
  (setq x (system::arglist x))
  (let ((nx (- (length x) (length (memq '&aux x)))))
    (if (memq '&rest x) (setq nx (- (1- nx))))
    (if (memq '&optional x) (setq nx (- (1- (abs nx)))))
    nx))

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

(defun LAM\,EVALANDFILEACTQ (name &optional (form name))
    (LAM\,FILEACTQ name form) (eval form))

(defun LAM\,FILEACTQ (name form)
       (if *FILEACTQ-APPLY* (FUNCALL *FILEACTQ-APPLY* name form)))

(defun CALLBELOW (&rest junk) junk) ; to invoke system dependent code?

(define-function 'EVA1 #'eval) ;EVA1 and VMLISP EVAL make lexicals visible
(define-function 'EVALFUN #'eval) ;EVALFUN drops lexicals before evaluating
(define-function 'EVA1FUN #'EVALFUN)

(defun PLACEP (item) (eq item *read-place-holder*))
(defun VMREAD (&optional (st |$InputStream|) (eofval *read-place-holder*))
  (read st nil eofval))
(defun |read-line| (st &optional (eofval *read-place-holder*))
  (read-line st nil eofval))

#+Lucid
(defun gcmsg (x)
   (prog1 (not system::*gc-silence*) (setq system::*gc-silence* (not x))))
#+(OR IBCL KCL)
(defun gcmsg (x)
   (prog1 system:*gbc-message* (setq system:*gbc-message* x)))
#+:cmulisp
(defun gcmsg (x)
   (prog1 ext:*gc-verbose* (setq ext:*gc-verbose* x)))
#+ (or :allegro :sbcl :clisp :ecl)
(defun gcmsg (x))

#+Lucid
(defun reclaim () (system:gc))
#+:cmulisp
(defun reclaim () (ext:gc))
#+(OR IBCL KCL)
(defun reclaim () (gbc t))
#+:allegro
(defun reclaim () (excl::gc t))
#+:CCL
(defun reclaim () (gc))

#+Lucid
(defun BPINAME (func)
  (if (functionp func)
      (if (symbolp func) func
        (let ((name (svref func 0)))
          (if (and (consp name) (eq (car name) 'SYSTEM::NAMED-LAMBDA))
              (cadr name)
            name)) )))

#+(OR IBCL KCL)
(defun BPINAME (func)
  (if (functionp func)
      (cond ((symbolp func) func)
            ((and (consp func) (eq (car func) 'LAMBDA-BLOCK))
              (cadr func))
            ((compiled-function-p func)
             (system:compiled-function-name func))
            ('t func))))
#+:cmulisp
(defun BPINAME (func)
 (when (functionp func)
  (cond
    ((symbolp func) func)
    ((and (consp func) (eq (car func) 'lambda)) (second (third func)))
    ((compiled-function-p func)
      (system::%primitive header-ref func system::%function-name-slot))
    ('else func))))
#+:allegro
(defun bpiname (func)
 func)
#+:CCL
(defun bpiname (x)
   (if (symbolp x)
     (intern (symbol-name (symbol-function x)) "BOOT")
    nil))

#+(or :SBCL :clisp :ecl)
(defun BPINAME (x)
  (if (symbolp x)
      x
    (multiple-value-bind (l c n)
      (function-lambda-expression x)
      (declare (ignore l c))
      n)))

  
(defun RE-ENABLE-INT (number-of-handler) number-of-handler)


(defun QUOREM (i j r) ; never used, refed in parini.boot
  (multiple-value-bind (x y) (truncate i j)
   (rplaca (the cons r) x) (rplacd (the cons r) y)))

(defun MAKE-BVEC (n)
 (make-array (list n) :element-type 'bit :initial-element 0))

