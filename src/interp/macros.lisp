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

;; PURPOSE: Provide generally useful macros and functions for MetaLanguage
;;          and Boot code.  Contents are organized along Common Lisp datatype
;;          lines, with sections numbered to match the section headings of the
;;          Common Lisp Reference Manual, by Guy Steele, Digital Press, 1984,
;;          Digital Press Order Number EY-00031-DP.  This way you can
;;          look up the corresponding section in the manual and see if
;;          there isn't a cleaner and non-VM-specific way of doing things.
 

;; Camm has identified a performace problem during compiles. There is
;; a loop that continually adds one element to a vector. This causes
;; the vector to get extended by 1 and copied. These patches fix the 
;; problem since vectors with fill pointers don't need to be copied.
;;
;; These cut out the lion's share of the gc problem
;; on this compile.  30min {\tt ->} 7 min on my box.  There is still some gc
;; churning in cons pages due to many calls to 'list' with small n.  One
;; can likely improve things further with an appropriate (declare
;; (:dynamic-extent ...)) in the right place -- gcl will allocate such
;; lists on the C stack (very fast).


(import-module "sys-macros") 
(import-module "sys-utility")
(in-package "BOOT")

; 5 PROGRAM STRUCTURE
 
; 5.3 Top-Level Forms
 
(defun SETANDFILE (x y) (LAM\,EVALANDFILEACTQ `(defparameter ,x ',y)))
 
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
                   ((IDENTP U) (AND (IDENTP V) (string> (SYMBOL-NAME V) (SYMBOL-NAME U))))
                   ((IDENTP V) NIL)
                   ((STRINGP U) (AND (STRINGP V) (string> V U)))
                   ((STRINGP V) NIL)
                   ((AND (VECP U) (VECP V))
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
 
(DEFUN GENVAR () (INTERNL "$" (STRINGIMAGE (SETQ $GENNO (1+ $GENNO)))))
 
(DEFUN IS_GENVAR (X)
  (AND (IDENTP X)
       (let ((y (symbol-name x)))
         (and (char= #\$ (elt y 0)) (> (size y) 1) (digitp (elt y 1))))))
 
(DEFUN IS_\#GENVAR (X)
  (AND (IDENTP X)
       (let ((y (symbol-name x)))
         (and (char= #\# (ELT y 0)) (> (SIZE Y) 1) (DIGITP (ELT Y 1))))))
 
; 10.7 CATCH and THROW
 
; 12 NUMBERS

; 12.6 Small Finite Field ops with vector trimming
 
(defun TRIMLZ (vec)
  (declare (simple-vector vec))
  (let ((n (position 0 vec :from-end t :test-not #'eql)))
     (cond ((null n) (vector))
           ((eql n (qvmaxindex vec)) vec)
           (t (subseq vec 0 (+ n 1))))))
 
;; In CCL ASH assumes a 2's complement machine.  We use ASH in Integer and
;; assume we have a sign and magnitude setup.
#+:CCL (defmacro ash (u v) `(lisp::ash1 ,u ,v))

; 14 SEQUENCES
 
; 14.1 Simple Sequence Functions
 
(define-function 'getchar #'elt)
 
(defun GETCHARN (A M) "Return the code of the Mth character of A"
  (let ((a (if (identp a) (symbol-name a) a))) (char-code (elt A M))))
 
; 14.2 Concatenating, Mapping, and Reducing Sequences
 
(DEFUN STRINGPAD (STR N)
  (let ((M (length STR)))
    (if (>= M N)
        STR
        (concatenate 'string str (make-string (- N M) :initial-element #\Space)))))
 
(DEFUN STRINGSUFFIX (TARGET SOURCE) "Suffix source to target if enough room else nil."
  (concatenate 'string target source))
 
(defun NSTRCONC (s1 s2) (concatenate 'string (string s1) (string s2)))
 
 
(define-function '|append| #'APPEND)
 
 
(defun THETACHECK (VAL VAR OP) (if (EQL VAL VAR) (THETA_ERROR OP) val))
 
; 15 LISTS
 
; 15.2 Lists
 
 
(defmacro TL (&rest L) `(tail . ,L))
 
 
(defmacro SPADCONST (&rest L) (cons 'qrefelt L))
 
(DEFUN LASTELEM (X) (car (last X)))
 
(defun LISTOFATOMS (X)
  (COND ((NULL X) NIL)
        ((ATOM X) (LIST X))
        ((NCONC (LISTOFATOMS (CAR X)) (LISTOFATOMS (CDR X))))))
 
(DEFUN LASTATOM (L) (if (ATOM L) L (LASTATOM (CDR L))))
 
(define-function 'LASTTAIL #'last)
 
(define-function 'LISPELT #'ELT)
 
(defun DROP (N X &aux m)
  "Return a pointer to the Nth cons of X, counting 0 as the first cons."
  (COND ((EQL N 0) X)
        ((> N 0) (DROP (1- N) (CDR X)))
        ((>= (setq m (+ (length x) N)) 0) (take m x))
        ((CROAK (list "Bad args to DROP" N X)))))
 
(DEFUN TAKE (N X &aux m)
  "Returns a list of the first N elements of list X."
  (COND ((EQL N 0) NIL)
        ((> N 0) (CONS (CAR X) (TAKE (1- N) (CDR X))))
        ((>= (setq m (+ (length x) N)) 0) (drop m x))
        ((CROAK (list "Bad args to DROP" N X)))))
 
(DEFUN NUMOFNODES (X) (if (ATOM X) 0 (+ 1 (NUMOFNODES (CAR X)) (NUMOFNODES (CDR X)))))
 
(DEFUN TRUNCLIST (L TL) "Truncate list L at the point marked by TL."
  (let ((U L)) (TRUNCLIST-1 L TL) U))
 
(DEFUN TRUNCLIST-1 (L TL)
  (COND ((ATOM L) L)
        ((EQL (CDR L) TL) (RPLACD L NIL))
        ((TRUNCLIST-1 (CDR L) TL))))
 
; 15.3 Alteration of List Structure
 
(defun RPLACW (x w) (let (y z) (dsetq (Y . Z) w) (RPLACA X Y) (RPLACD X Z)  X))
 
; 15.4 Substitution of Expressions
 
(DEFUN SUBSTEQ (NEW OLD FORM)
  "Version of SUBST that uses EQ rather than EQUAL on the world."
  (PROG (NFORM HNFORM ITEM)
        (SETQ HNFORM (SETQ NFORM (CONS () ())))
     LP    (RPLACD NFORM
                   (COND ((EQ FORM OLD) (SETQ FORM ()) NEW )
                         ((NOT (PAIRP FORM)) FORM )
                         ((EQ (SETQ ITEM (CAR FORM)) OLD) (CONS NEW ()) )
                         ((PAIRP ITEM) (CONS (SUBSTEQ NEW OLD ITEM) ()) )
                         ((CONS ITEM ()))))
        (if (NOT (PAIRP FORM)) (RETURN (CDR HNFORM)))
        (SETQ NFORM (CDR NFORM))
        (SETQ FORM (CDR FORM))
        (GO LP)))
 
(DEFUN SUBLISNQ (KEY E) (declare (special KEY)) (if (NULL KEY) E (SUBANQ E)))
 
(DEFUN SUBANQ (E)
  (declare (special key))
  (COND ((ATOM E) (SUBB KEY E))
        ((EQCAR E (QUOTE QUOTE)) E)
        ((MAPCAR #'(LAMBDA (J) (SUBANQ J)) E))))
 
(DEFUN SUBB (X E)
  (COND ((ATOM X) E)
        ((EQ (CAAR X) E) (CDAR X))
        ((SUBB (CDR X) E))))
 
(defun SUBLISLIS (newl oldl form)
   (sublis (mapcar #'cons oldl newl) form))

; 15.5 Using Lists as Sets

(DEFUN PREDECESSOR (TL L)
  "Returns the sublist of L whose CDR is EQ to TL."
  (COND ((ATOM L) NIL)
        ((EQ TL (CDR L)) L)
        ((PREDECESSOR TL (CDR L)))))
 
(defun remdup (l) (remove-duplicates l :test #'equalp))
 
(DEFUN GETTAIL (X L) (member X L :test #'equal))
 
; 15.6 Association Lists
 
(defun QLASSQ (p a-list) (cdr (assq p a-list)))

(define-function 'LASSQ #'QLASSQ)
 
(defun pair (x y) (mapcar #'cons x y))
 
;;; Operations on Association Sets (AS)
 
(defun AS-INSERT (A B L)
   ;; PF(item) x PF(item) x LIST(of pairs) -> LIST(of pairs with (A . B) added)
   ;; destructive on L; if (A . C) appears already, C is replaced by B
   (cond ((null l) (list (cons a b)))
         ((equal a (caar l)) (rplac (cdar l) b) l)
         ((?order a (caar l)) (cons (cons a b) l))
         (t (as-insert1 a b l) l)))
 
(defun as-insert1 (a b l)
   (cond ((null (cdr l)) (rplac (cdr l) (list (cons a b))))
         ((equal a (caadr l)) (rplac (cdadr l) b))
         ((?order a (caadr l)) (rplac (cdr l) (cons (cons a b) (cdr l))))
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
 
(DEFUN CONSOLEINPUTP (STRM) (IS-CONSOLE STRM))
 
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
                      (SETELT V 3 (SIZE (ELT V 0)))))))
 
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
 
(defun |sayTeX| (x) (if (null x) nil (sayBrightly1 x |$texOutputStream|)))
 
(defun |sayNewLine| () (TERPRI))

(defvar |$sayBrightlyStream| nil "if not nil, gives stream for sayBrightly output")
 
(defun |sayBrightly| (x &optional (out-stream |$OutputStream|))
  (COND ((NULL X) NIL)
        (|$sayBrightlyStream| 
	 (sayBrightly1 X |$sayBrightlyStream|))
        ((IS-CONSOLE out-stream) 
	 (sayBrightly1 X out-stream))
        ((sayBrightly1 X out-stream) 
	 (sayBrightly1 X |$OutputStream|))))
 
(defun |sayBrightlyI| (x &optional (s |$OutputStream|))
    "Prints at console or output stream."
  (if (NULL X) NIL (sayBrightly1 X S)))
 
(defun |sayBrightlyNT| (x &optional (S |$OutputStream|))
  (COND ((NULL X) NIL)
        (|$sayBrightlyStream| 
	 (sayBrightlyNT1 X |$sayBrightlyStream|))
        ((IS-CONSOLE S)
	 (sayBrightlyNT1 X S))
        ((sayBrightly1 X S)
	 (sayBrightlyNT1 X |$OutputStream|))))
 
(defun sayBrightlyNT1 (X |$OutputStream|)
  (if (ATOM X) (BRIGHTPRINT-0 X) (BRIGHTPRINT X)))
 
(defun sayBrightly1 (X |$OutputStream|)
    (if (ATOM X)
        (progn (BRIGHTPRINT-0 X) (TERPRI) (force-output))
      (progn (BRIGHTPRINT X) (TERPRI) (force-output))))
 
(defun |saySpadMsg| (X)
  (if (NULL X) NIL (sayBrightly1 X |$algebraOutputStream|)))
 
(defun |sayALGEBRA| (X) "Prints on Algebra output stream."
  (if (NULL X) NIL (sayBrightly1 X |$algebraOutputStream|)))
 
(defun |sayMSG| (X)
  (if (NULL X) NIL (sayBrightly1 X |$algebraOutputStream|)))
 
(defun |sayMSGNT| (X)
  (if (NULL X) NIL (sayBrightlyNT1 X |$algebraOutputStream|)))
 
(defun |sayMSG2File| (msg)
  (PROG (file str)
        (SETQ file (|makePathname| '|spadmsg| '|listing| |$listingDirectory|))
        (SETQ str
              (DEFIOSTREAM
               (CONS '(MODE . OUTPUT) (CONS (CONS 'FILE file) NIL))
               255 0))
        (sayBrightly1 msg str)
        (SHUT str) ) )
 
(defvar |$fortranOutputStream|)
 
(defun |sayFORTRAN| (x) "Prints on Fortran output stream."
  (if (NULL X) NIL (sayBrightly1 X |$fortranOutputStream|)))
 
(defvar |$formulaOutputStream|)
 
(defun |sayFORMULA| (X) "Prints on formula output stream."
  (if (NULL X) NIL (sayBrightly1 X |$formulaOutputStream|)))
 
;; the following are redefined in MSGDB BOOT
 
(DEFUN BLANKS (N &optional (stream |$OutputStream|))
  "Print N blanks."
    (do ((i 1 (the fixnum(1+ i))))
        ((> i N))(declare (fixnum i n)) (princ " " stream)))
 
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
         ((IS-CONSOLE STRM) 0)
         ((file-position STRM))))
 
(DEFUN IS-CONSOLE-NOT-XEDIT (S) (not (OR (NULL (IS-CONSOLE S)))))
 
(DEFUN POINTW (RECNO STRM)
"Does something obscure and VM-specific with respect to streams."
  (let (V)
    (if (STREAM-EOF STRM) (FAIL))
    (SETQ V (LASTATOM STRM))
    (SETELT V 4 RECNO)
    (SETQ *EOF* (STREAM-EOF STRM))
    strm))
 
(DEFUN POINT (RECNO STRM) (file-position strm recno))
 
(DEFUN STRM (RECNO STRM)
"Does something obscure and VM-specific with respect to streams."
  (let (V)
    (if (STREAM-EOF STRM) (FAIL))
    (SETQ V (LASTATOM STRM))
    (SETELT V 4 RECNO)
    (read-char STRM)
    (SETQ *EOF* (STREAM-EOF STRM))
    strm))
 
; 25 MISCELLANEOUS FEATURES
 
;; range tests and assertions
 
(defmacro |assert| (x y) `(IF (NULL ,x) (|error| ,y)))
 
(defun coerce-failure-msg (val mode)
   (STRCONC (MAKE-REASONABLE (STRINGIMAGE val))
            " cannot be coerced to mode "
            (STRINGIMAGE (|devaluate| mode))))
 
(defmacro |check-subtype| (pred submode val)
   `(|assert| ,pred (coerce-failure-msg ,val ,submode)))
 
(defmacro |check-union| (pred branch val)
   `(|assert| ,pred (coerce-failure-msg ,val ,branch )))
 
(defun MAKE-REASONABLE (Z)
   (if (> (length Z) 30) (CONCAT "expression beginning " (subseq Z 0 20)) Z))
 
 
(defmacro |elapsedUserTime| () '(get-internal-run-time))
 
#+IBCL
(defmacro |elapsedGcTime| () '(system:gbc-time-report))
#+AKCL
(defmacro |elapsedGcTime| () '(system:gbc-time))
#+:CCL
(defmacro |elapsedGcTime| () '(lisp:gctime))
#-(OR :CCL IBCL AKCL)
(defmacro |elapsedGcTime| () '0)
 
(defmacro |do| (&rest args) (CONS 'PROGN args))

(defun DROPTRAILINGBLANKS  (LINE) (string-right-trim " " LINE))

(defun print-and-eval-defun (name body)
   (eval body)
   (print-defun name body)
  ;; (set name (symbol-function name)) ;; this should go away
   )

(defun eval-defun (name body) (eval (macroexpandall body)))

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
;       (qrefelt |*1;f;1;initial;MV| 0))))))
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


(defun compile-defun (name body) (eval body) (compile name))


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
                 (DO ((|i| 0 (QSADD1 |i|))) ((QSGREATERP |i| |l|) NIL)
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

;; moved here from preparse.lisp

(defun NEXT-TAB-LOC (i) (* (1+ (truncate i 8)) 8))
 
(defun INDENT-POS (STR)
  (do ((i 0 (1+ i))
       (pos 0))
      ((>= i (length str)) nil)
      (case (char str i)
            (#\space (incf pos))
            (#\tab (setq pos (next-tab-loc pos)))
            (otherwise (return pos)))))

;;(defun expand-tabs (str)
;;  (let ((bpos (nonblankloc str))
;;      (tpos (indent-pos str)))
;;    (if (eql bpos tpos) str
;;      (concatenate 'string (make-string tpos :initial-element #\space)
;;                 (subseq str bpos)))))
(defun expand-tabs (str)
   (if (and (stringp str) (> (length str) 0))
      (let ((bpos (nonblankloc str))
            (tpos (indent-pos str)))
        (setq str 
              (if (eql bpos tpos)
                  str
                  (concatenate 'string
                               (make-string tpos :initial-element #\space)
                               (subseq str bpos))))
         ;; remove dos CR
        (let ((lpos (maxindex str)))
          (if (eq (char str lpos) #\Return) (subseq str 0 lpos) str)))
    str))

(defun blankp (char) (or (eq char #\Space) (eq char #\tab)))
 
(defun nonblankloc (str) (position-if-not #'blankp str))
 
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
    (setq val (catch 'spad_reader
                (catch 'TOP_LEVEL
                  (apply (symbol-function func) args))))
    (cons val (get-output-stream-string |$OutputStream|))))

(defun |breakIntoLines| (str)
  (let ((bol 0) (eol) (line-list nil))
    (loop
     (setq eol (position #\Newline str :start bol))
     (if (null eol) (return))
     (if (> eol bol) 
         (setq line-list (cons (subseq str bol eol) line-list)))
     (setq bol (+ eol 1)))
    (nreverse line-list)))

; part of the old spad to new spad translator
; these are here because they need to be in depsys
; they were in nspadaux.lisp

(defmacro wi (a b) b)

(defmacro |tryLine| (X)
  `(LET ((|$autoLine|))
        (declare (special |$autoLine|))
        (|tryToFit| (|saveState|) ,X)))

(defmacro |embrace| (X) `(|wrapBraces| (|saveC|) ,X (|restoreC|)))
(defmacro |indentNB| (X) `(|wrapBraces| (|saveD|) ,X (|restoreD|)))

(defmacro |tryBreak| (a b c d) 
; Try to format <a b> by:
; (1) with no line breaking ($autoLine = nil)
; (2) with possible line breaks within a;
; (3) otherwise use a brace
  `(LET
    ((state))
    (setq state (|saveState| 't))
    (or
      (LET ((|$autoLine|))
         (declare (special |$autoLine|))
         (and ,a (|formatRight| '|formatPreferPile| ,b ,c ,d)))
      (|restoreState| state)
      (and (eqcar ,b (quote seq))
               (|embrace| (and 
                  ,a
                  (|formatLB|)
                  (|formatRight| '|formatPreferPile| ,b ,c ,d))))
      (|restoreState| state)
      (|embrace| (and ,a 
                  (|formatLB|)
                  (|formatRight| '|formatPreferPile| ,b ,c ,d))))))

(defmacro |tryBreakNB| (a b c d) 
; Try to format <a b> by:
; (1) with no line breaking ($autoLine = nil)
; (2) with possible line breaks within a;
; (3) otherwise display without a brace
  `(LET
    ((state))
    (setq state (|saveState| 't))
    (or
      (markhash ,b 0)
      (LET ((|$autoLine|))
         (declare (special |$autoLine|))
         (and ,a (|formatRight| '|formatPreferPile| ,b ,c ,d)))
      (|restoreState| state)
      (markhash ,b 1)
      (and (eqcar ,b (quote seq))
               (|embrace| (and 
                  ,a
                  (|formatLB|)
                  (|formatRight| '|formatPreferPile| ,b ,c ,d))))
      (markhash ,b 2)
      (|restoreState| state)
      (|indentNB| (and ,a 
                  (|formatRight| '|formatPreferPile| ,b ,c ,d)))
      (markhash ,b 3)

)))   

(defvar HT nil)

(defun markhash (key n) (progn (cond
  ((equal n 3) (remhash key ht))
  ('t (hput ht key n)) ) nil))

;; 
;; -*- Record Structures -*-
;; 

(defmacro |Record| (&rest x)
  `(|Record0| (LIST ,@(COLLECT (IN Y X)
                               (list 'CONS (MKQ (CADR Y)) (CADDR Y))))))

(defmacro |:| (tag expr)
  `(LIST '|:| ,(MKQ tag) ,expr))


