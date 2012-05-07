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
 
(DEFUN GENVAR () (INTERNL "$" (STRINGIMAGE (SETQ $GENNO (1+ $GENNO)))))
 
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
           ((eql n (maxindex vec)) vec)
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
 
(defun DROPTRAILINGBLANKS  (LINE) (string-right-trim " " LINE))

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

(defun expand-tabs (str)
   (if (and (stringp str) (> (length str) 0))
      (let ((bpos (nonblankloc str))
            (tpos (|indentationLocation| str)))
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
    (setq val (catch |$SpadReaderTag|
                (catch |$intTopLevel|
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
    (|reverse!| line-list)))


(defvar HT nil)
