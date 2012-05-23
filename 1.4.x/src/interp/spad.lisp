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


; NAME:    Scratchpad Package
; PURPOSE: This is an initialization and system-building file for Scratchpad.

(IMPORT-MODULE "spad-parser")
(import-module "postpar")
(import-module "debug")
(in-package "BOOT")

;;; Common  Block

(defvar |$reportInstantiations| nil)
(defvar |$reportEachInstantiation| nil)
(defvar |$compForModeIfTrue| nil "checked in compSymbol")
(defvar |$functorForm| nil "checked in addModemap0")
(defvar |$Rep| '|$Rep| "should be bound to gensym? checked in coerce")
(defvar |$definition| nil "checked in DomainSubstitutionFunction")
(defvar |$texFormat| nil "if true produce tex output")
(defvar |$fortranFormat| nil "if true produce fortran output")
(defvar |$algebraFormat| t "produce 2-d algebra output")
(defvar |$HiFiAccess| nil "if true maintain history file")

(DEFVAR _ '&)
(defvar /EDIT-FM 'A1)
(defvar /EDIT-FT 'SPAD)
(defvar /RELEASE '"UNKNOWN")
(defvar error-print)
(defvar ind)
(defvar INITCOLUMN 0)
(defvar JUNKTOKLIST '(FOR IN AS INTO OF TO))
(defvar m-chrbuffer)
(defvar m-chrindex)
(defvar MARG 0 "Margin for testing by ?OP")
(defvar RLGENSYMFG NIL)
(defvar RLGENSYMLST NIL)
(defvar S-SPADTOK 'SPADSYSTOK)
(defvar sortpred)
(defvar SPADSYSKEY '(EOI EOL))
(defvar STAKCOLUMN -1)
(defvar |$IOAlist| '((|%i| . (|gauss| 0 1))))
(defvar |InteractiveMode|)
(defvar |uc| 'UC)

(defun init-boot/spad-reader ()
  (setq $SPAD_ERRORS (VECTOR 0 0 0))
  (setq SPADERRORSTREAM |$OutputStream|)
  (|nextLinesClear!|)
  (|ioClear!|))

(defun spad (&optional
              (*spad-input-file* nil)
              (*spad-output-file* nil)
             &aux
           (*comp370-apply* (function |printBackendDecl|))
           (*fileactq-apply* (function |printBackendDecl|))
           ($SPAD T)
           (OPTIONLIST nil)
           (*EOF* NIL)
           (/editfile *spad-input-file*)
           in-stream out-stream)
  (declare (special |$Echo| /editfile *comp370-apply* *EOF* Xcape))
  (setq |$InteractiveMode| nil)
  ;; only rebind |$InteractiveFrame| if compiling
  (progv (if (not |$InteractiveMode|) '(|$InteractiveFrame|))
         (if (not |$InteractiveMode|)
             (list (|addBinding|
                    '|$DomainsInScope|
                    `((FLUID . |true|)
                      (|special| . ,(COPY-TREE |$InitialDomainsInScope|)))
                    (|addBinding| '|$Information| NIL (|makeInitialModemapFrame|)))))
  (init-boot/spad-reader)
  (unwind-protect
    (progn
      (setq in-stream (if *spad-input-file*
                         (open *spad-input-file* :direction :input)
			|$InputStream|))
      (initialize-preparse in-stream)
      (setq out-stream (if *spad-output-file*
                           (open *spad-output-file* :direction :output)
                         |$OutputStream|))
      (when *spad-output-file*
         (format out-stream "~&;;; -*- Mode:Lisp; Package:Boot  -*-~%~%")
         (print-package "BOOT"))
      (setq |$OutputStream| out-stream)
      (loop
       (if (|eof?| in-stream) (return nil))
       (catch 'SPAD_READER
	 (progn 
	   (setq |$lineStack| (|preparse| in-stream))
	   (when (null |$lineStack|)
	     (return nil))
	   (when |$lineStack|
	     (let ((LINE (cdar |$lineStack|)))
	       (declare (special LINE))
               (|parseNewExpr|)
               (let ((parseout (|popStack1|)) )
                 (when parseout
                       (let ((|$OutputStream| out-stream))
                         (|translateSpad| parseout))
                       (format out-stream "~&")))
               ))))
      (|ioClear!|)))
    (if *spad-input-file* (shut in-stream))
    (if *spad-output-file* (shut out-stream)))
  T))

(DEFUN INTEGER-BIT (N I) (LOGBITP I N))

(DEFUN /TRANSPAD (X)
  (PROG (proplist)
        (setq proplist (LIST '(FLUID . |true|)
                             (CONS '|special|
                                   (COPY-TREE |$InitialDomainsInScope|))))
        (SETQ |$InteractiveFrame|
              (|addBinding| '|$DomainsInScope| proplist
                          (|addBinding| '|$Information| NIL
                                      (COPY-TREE |$InitialModemapFrame|))))
        (RETURN (PROGN (|translateSpad| X) NIL))))

 ;; NIL needed below since END\_UNIT is not generated by current parser

(defun |traceComp| ()
  (SETQ |$compCount| 0)
  (EMBED '|comp|
     '(LAMBDA (X Y Z)
         (PROG (U)
               (SETQ |$compCount| (1+ |$compCount|))
               (SETQ |yesOrNo| (if (SETQ U (|comp| X Y Z))
                                   (if (EQUAL (SECOND U) Y) '|yes| (SECOND U))
                                 ('T '|no|)))
               (|sayBrightly| (CONS (MAKE-FULL-CVEC |$compCount| " ")
                                    (LIST X " --> " Y '|%b| |yesOrNo| '|%d|)))
               (SETQ |$compCount| (1- |$compCount|))
               (RETURN U)  )))
  (|comp| |$x| |$m| |$f|)
  (UNEMBED '|comp|))

(defun UNCONS (X)
  (COND ((ATOM X) X)
        ((EQCAR X 'CONS) (CONS (SECOND X) (UNCONS (THIRD X))))
        (T (ERROR "UNCONS"))))

(defun OPTIMIZE\&PRINT (X) (PRETTYPRINT (/MDEF X)))

(defun SPAD-PRINTTIME (A B)
  (let (c msg)
    (setq C (+ A B))
    (setq MSG (STRCONC "(" (STRINGIMAGE A) " + " (STRINGIMAGE B)
                       " = " (STRINGIMAGE C) " MS.)"))
    (PRINT (STRCONC (STRINGPAD "" (DIFFERENCE 80 (SIZE MSG))) MSG))))

(defun SPAD-MODETRAN (X) (D-TRAN X))

(defun SPAD-EVAL (X)
  (COND ((ATOM X) (EVAL X))
        ((CONS (FIRST X) (MAPCAR #'SPAD-EVAL (CDR X))))))

;************************************************************************
;         SYSTEM COMMANDS
;************************************************************************

(defun READLISP (UPPER_CASE_FG)
  (let (v expr val )
    (setq EXPR (READ-FROM-STRING
                  (IF UPPER_CASE_FG (string-upcase (line-buffer |$spadLine|))
                      (line-buffer |$spadLine|))
                  t nil :start (Line-CURRENT-INDEX |$spadLine|)))
    (VMPRINT EXPR)
    (setq VAL ((LAMBDA (|$InteractiveMode|)  (EVAL EXPR)) NIL))
    (FORMAT t "~&VALUE = ~S" VAL)
    (TERSYSCOMMAND)))

(defun TERSYSCOMMAND ()
  (FRESH-LINE)
  (SETQ CHR 'ENDOFLINECHR)
  (SETQ TOK 'END_UNIT)
  (|spadThrow|))

(defun /READ (L Q)
;  (SETQ /EDIT-FN (OR (KAR L) /EDIT-FN))
;  (SETQ /EDIT-FT (OR (KAR (KDR L)) 'INPUT))
;  (SETQ /EDIT-FM (OR (KAR (KDR (KDR L))) '*))
;  (SETQ /EDITFILE (LIST /EDIT-FN /EDIT-FT /EDIT-FM))
  (SETQ /EDITFILE L)
  (COND
    (Q  (/RQ))
    ('T (/RF)) )
  (|terminateSystemCommand|))

(defun /EF (&rest foo)
  (|runCommand| (concat "vi " (namestring (make-input-filename /EDITFILE)))))

(defun /EDIT (L)
  (SETQ /EDITFILE L)
  (/EF)
  (|terminateSystemCommand|))

(defun /COMPINTERP (L OPTS)
  (SETQ /EDITFILE (/MKINFILENAM L))
  (COND ((EQUAL OPTS "rf") (/RF))
        ((EQUAL OPTS "rq") (/RQ))
        ('T (/RQ-LIB)))
  (|terminateSystemCommand|))

(defun |fin| ()
  (SETQ *EOF* 'T)
  (THROW 'SPAD_READER NIL))


(defun STRINGREST (X) (if (EQ (SIZE X) 1) (make-string 0) (SUBSTRING X 1 NIL)))

(defun STREAM2UC (STRM)
  (LET ((X (ELT (LASTATOM STRM) 1))) (SETF (ELT X 0) (LC2UC (ELT X 0)))))

(defun GP2COND (L)
  (COND ((NOT L) (ERROR "GP2COND"))
        ((NOT (CDR L))
         (COND ((EQCAR (FIRST L) 'COLON)
                (CONS (SECOND L) (LIST (LIST T 'FAIL))))
               (T (LIST (LIST T (FIRST L)))) ))
        ((EQCAR (FIRST L) 'COLON) (CONS (CDAR L) (GP2COND (CDR L))))
        (T (ERROR "GP2COND"))))

(FLAG JUNKTOKLIST 'KEY)

(defmacro |DomainSubstitutionMacro| (&rest L)
  (|DomainSubstitutionFunction| (first L) (second L)))

(defun |sort| (seq spadfn)
    (sort (copy-seq seq) (function (lambda (x y) (SPADCALL X Y SPADFN)))))

(defun DIVIDE2 (X Y) (multiple-value-call #'cons (TRUNCATE X Y)))

(define-function '|not| #'NOT)

(defun |random| () (random (expt 2 26)))
(defun \,plus (x y) (+ x y))
(defun \,times (x y) (* x y))
(defun \,difference (x y) (- x y))
(defun \,max (x y) (max x y))
(defun \,min (x y) (min x y))

(MAKEPROP 'END_UNIT 'KEY T)

(defun |process| (x)
  (COND ((NOT (EQ TOK 'END_UNIT))
         (SETQ DEBUGMODE 'NO)
         (SPAD_SYNTAX_ERROR)
         (if |$InteractiveMode| (|spadThrow|))
         (|translateSpad| x))))

(defun INITIALIZE () 
  (init-boot/spad-reader)
  (initialize-preparse |$InputStream|))

(defmacro try (X)
  `(LET ((|$autoLine|))
        (declare (special |$autoLine|))
        (|tryToFit| (|saveState|) ,X)))

(defun SETELTFIRST (A B C) (declare (ignore b)) (RPLACA A C))

(defun SETELTREST (A B C) (declare (ignore b)) (RPLACD A C))

(DEFUN ASSOCIATER (FN LST)
  (COND ((NULL LST) NIL)
        ((NULL (CDR LST)) (CAR LST))
        ((LIST FN (CAR LST) (ASSOCIATER FN (CDR LST))))))

(defun ISLOCALOP-1 (IND)
  "Curindex points at character after '.'"
  (prog (selector buf termtok (NEWCHR (NEXTCHARACTER)))
    (if (TERMINATOR NEWCHR) (RETURN NIL))
    (setq SELECTOR
          (do ((x nil))
              (nil)
            (if (terminator newchr)
                (reverse x)
                (push (setq newchr (nextcharacter)) x))))
    (if (EQUAL NEWCHR '\.) (RETURN (ISLOCALOP-1 IND)))
    (setq BUF (|makeString| (LENGTH SELECTOR)))
    (mapc #'(lambda (x) (suffix x buf)) selector)
    (setq buf (copy-seq selector))
    (setq TERMTOK (INTERN BUF))
    (if (NOT (GET TERMTOK 'GENERIC)) (RETURN NIL))
    (if (OR (GET TERMTOK '|Led|) (GET TERMTOK '|Nud|))
        (GET TERMTOK IND))
    (return TERMTOK)))
; **** X. Random tables

(defvar $MARGIN 3)
(defvar TEMPGENSYMLIST '(|s| |r| |q| |p|))
(defvar ALPHLIST '(|a| |b| |c| |d| |e| |f| |g|))
(defvar LITTLEIN " in ")
(defvar INITALPHLIST ALPHLIST)
(defvar INITXPARLST '(|i| |j| |k| |l| |m| |n| |p| |q|))
(defvar PORDLST (COPY-tree INITXPARLST))
(defvar INITPARLST '(|x| |y| |z| |u| |v| |w| |r| |s| |t|))
(defvar LITTLEA '|a|)
(defvar LITTLEI '|i|)
(defvar ALLSTAR NIL)
(defvar PLUSS "+")
(defvar PERIOD ".")
(defvar SLASH "/")
(defvar COMMA ",")
(defvar LPAR "(")
(defvar RPAR ")")
(defvar EQSIGN "=")
(defvar DASH "-")
(defvar STAR "*")
(defvar DOLLAR "$")
(defvar COLON ":")

(FLAG TEMPGENSYMLIST 'IS-GENSYM)


;; NAME:    DECIMAL-LENGTH
;; PURPOSE: Computes number of decimal digits in print representation of x
;;  This should made as efficient as possible.

(DEFUN DECIMAL-LENGTH (X)
   (LET* ((K (FIX (* #.(LOG 2.0 10.) (INTEGER-LENGTH X))))
          (X (TRUNCATE (ABS X) (EXPT 10 (1- K)))))
     (IF (< X 10) K (1+ K))))

;(DEFUN DECIMAL-LENGTH2 (X)
;   (LET ((K (FIX (* #.(LOG 2.0 10.) (INTEGER-LENGTH X)))))
;     (IF (< (ABS X) (EXPT 10 K)) K (1+ K))))


;; function to create byte and half-word vectors in new runtime system 8/90

(defun |makeByteWordVec| (initialvalue)
  (let ((n (cond ((null initialvalue) 7) ('t (reduce #'max initialvalue)))))
    (make-array (length initialvalue)
      :element-type (list 'mod (1+ n))
      :initial-contents initialvalue)))

(defun |makeByteWordVec2| (maxelement initialvalue)
  (let ((n (cond ((null initialvalue) 7) ('t maxelement))))
    (make-array (length initialvalue)
      :element-type (list 'mod (1+ n))
      :initial-contents initialvalue)))

(defun |knownEqualPred| (dom)
  (let ((fun (|compiledLookup| '= '((|Boolean|) $ $) dom)))
    (if fun (|getFunctionReplacement| (bpiname (car fun)))
      nil)))

(defun |hashable| (dom)
  (|symbolMember?| (|knownEqualPred| dom)
        '(EQ EQL EQUAL)))

;; simpler interpface to RDEFIOSTREAM
(defun RDEFINSTREAM (&rest fn)
  ;; following line prevents rdefiostream from adding a default filetype
  (if (null (rest fn)) (setq fn (list (pathname (car fn)))))
  (rdefiostream (list (cons 'FILE fn) '(mode . INPUT))))

(defun RDEFOUTSTREAM (&rest fn)
  ;; following line prevents rdefiostream from adding a default filetype
  (if (null (rest fn)) (setq fn (list (pathname (car fn)))))
  (rdefiostream (list (cons 'FILE fn) '(mode . OUTPUT))))

(defun error-format (message args)
  (let ((|$BreakMode| '|break|))
    (declare (special |$BreakMode|))
   (if (stringp message) (apply #'format nil message args) nil)))

#+:gcl
(defun |resetStackLimits| () (system:reset-stack-limits))
#-:gcl
(defun |resetStackLimits| () nil)

(defvar |$oldBreakMode|)

;; following macro evaluates form returning Maybe type-of form
#+:gcl
(defmacro |trapNumericErrors| (form)
  `(let ((|$oldBreakMode| |$BreakMode|)
         (|$BreakMode| '|trapNumerics|)
	 (val))
     (catch '|trapNumerics| ,form)))

#-:gcl
(defmacro |trapNumericErrors| (form)
  `(handler-case ,form
		 (arithmetic-error () |%nothing|)))

;; the following form embeds around the akcl error handler
#+:gcl
(eval-when
 (load eval)
 (unembed 'system:universal-error-handler)
 (embed 'system:universal-error-handler
            '(lambda (type correctable? op
                           continue-string error-string &rest args)
               (block
                nil
                (setq |$NeedToSignalSessionManager| T)
                (if (and (boundp '|$inLispVM|) (boundp '|$BreakMode|))
                    (cond ((eq |$BreakMode| '|validate|)
                           (|systemError| (error-format error-string args)))
                          ((and (eq |$BreakMode| '|trapNumerics|)
                                (eq type :ERROR))
                           (setq |$BreakMode| nil)
			   (throw '|trapNumerics| |%nothing|))
                          ((and (eq |$BreakMode| '|trapNumerics|)
                                (boundp '|$oldBreakMode|)
                                (setq |$BreakMode| |$oldBreakMode|)
                                nil)) ;; resets error handler
                          ((and (null |$inLispVM|)
                                (|symbolMember?| |$BreakMode| '(|nobreak| |query| |resume|)))
                           (let ((|$inLispVM| T)) ;; turn off handler
                             (return
                              (|systemError| (error-format error-string args)))))
                          ((eq |$BreakMode| '|letPrint2|)
                           (setq |$BreakMode| nil)
                           (throw '|letPrint2| nil))))
                (apply system:universal-error-handler type correctable? op
                       continue-string error-string args )))))

