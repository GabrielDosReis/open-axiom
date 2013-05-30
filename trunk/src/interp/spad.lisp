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
(defvar error-print)
(defvar MARG 0 "Margin for testing by ?OP")
(defvar |uc| 'UC)

(defun init-boot/spad-reader (rd)
  (setq $SPAD_ERRORS (VECTOR 0 0 0))
  (setq SPADERRORSTREAM |$OutputStream|)
  (|nextLinesClear!|)
  (|ioClear!| rd))

(defun spad (ifile
             &aux
	     (|$editFile| ifile)
	     rd)
  (declare (special |$Echo| |$editFile|))
  (setq |$InteractiveMode| nil)
  ;; only rebind |$InteractiveFrame| if compiling
  (progv (if (not |$InteractiveMode|) '(|$InteractiveFrame|))
         (if (not |$InteractiveMode|)
             (list (|addBinding|
                    '|$DomainsInScope|
                    `((FLUID . |true|)
                      (|special| . ,(COPY-TREE |$InitialDomainsInScope|)))
                    (|addBinding| '|$Information| NIL (|makeInitialModemapFrame|)))))
	 (progn
	   (setq rd (|makeReader| ifile |$OutputStream|))
	   (init-boot/spad-reader rd)
	   (initialize-preparse rd)
	   (loop
	    (if (|readerEoi?| rd) (return nil))
	    (catch |$SpadReaderTag|
	      (progn 
		(setq |$lineStack| (|preparse| rd))
		(when (null |$lineStack|)
		  (return nil))
		(when |$lineStack|
		  (let ((LINE (cdar |$lineStack|)))
		    (declare (special LINE))
		    (|parseNewExpr| rd)
		    (let ((parseout (|popStack1|)) )
		      (when parseout
			(|translateSpad| ifile parseout)
			(format |$OutputStream| "~&")))
		    ))))
	    (|ioClear!| rd)))
	 T))

(DEFUN INTEGER-BIT (N I) (LOGBITP I N))

;************************************************************************
;         SYSTEM COMMANDS
;************************************************************************

(defun /READ (L Q)
  (SETQ |$editFile| L)
  (COND
    (Q  (/RQ))
    ('T (/RF)) )
  (|terminateSystemCommand|))

(defun |fin| ()
  (SETQ *EOF* 'T)
  (THROW 'SPAD_READER NIL))

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

(defmacro try (X)
  `(LET ((|$autoLine|))
        (declare (special |$autoLine|))
        (|tryToFit| (|saveState|) ,X)))

(DEFUN ASSOCIATER (FN LST)
  (COND ((NULL LST) NIL)
        ((NULL (CDR LST)) (CAR LST))
        ((LIST FN (CAR LST) (ASSOCIATER FN (CDR LST))))))

; **** X. Random tables

(defvar $MARGIN 3)
(defvar PLUSS "+")
(defvar SLASH "/")
(defvar COMMA ",")
(defvar DASH "-")
(defvar STAR "*")
(defvar DOLLAR "$")
(defvar COLON ":")

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

