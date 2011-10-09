;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2011, Gabriel Dos Reis.
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


; NAME:         BootLex.lisp
; PURPOSE:      Parsing support routines for Boot and Spad code
; CONTENTS:
;
;               0. Global parameters
;               1. BOOT File Handling
;               2. BOOT Line Handling
;               3. BOOT Token Handling
;               4. BOOT Token Parsing Actions
;               5. BOOT Error Handling

(import-module "sys-globals")
(IMPORT-MODULE "preparse")
(IMPORT-MODULE "macros")
(IMPORT-MODULE "nlib")
(in-package "BOOT")

; *** 0. Global parameters

; *** 1. BOOT file handling

(defun init-boot/spad-reader ()
  (setq $SPAD_ERRORS (VECTOR 0 0 0))
  (setq SPADERRORSTREAM |$OutputStream|)
  (setq File-Closed nil)
  (Next-Lines-Clear)
  (setq |$lineStack| nil)
  (ioclear))

(defmacro test (x &rest y)
  `(progn
     (setq spaderrorstream t)
     (in-boot)
     (initialize-preparse |$InputStream|)
     (,(intern (strconc "PARSE-" x)) . ,y)))

(defun print-defun (name body)
   (let* ((sp (assoc 'compiler-output-stream optionlist))
          (st (if sp (cdr sp) |$OutputStream|)))
     (if (and (is-console st) (symbolp name) (fboundp name)
              (not (compiled-function-p (symbol-function name))))
         (compile name))
     (when (or |$PrettyPrint| (not (is-console st)))
           (print-full body st) (force-output st))))

(defun spad (&optional
              (*spad-input-file* nil)
              (*spad-output-file* nil)
             &aux
         ;;  (*comp370-apply* (function print-and-eval-defun))
           (*comp370-apply* (function print-defun))
           (*fileactq-apply* (function print-defun))
           ($SPAD T)
           (OPTIONLIST nil)
           (*EOF* NIL)
           (File-Closed NIL)
           (/editfile *spad-input-file*)
           in-stream out-stream)
  (declare (special |$Echo| /editfile *comp370-apply* *EOF*
                    File-Closed Xcape))
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
       (if (or *eof* file-closed) (return nil))
       (catch 'SPAD_READER
         (if (setq |$lineStack| (PREPARSE in-stream))
             (let ((LINE (cdar |$lineStack|)))
               (declare (special LINE))
               (|PARSE-NewExpr|)
               (let ((parseout (|popStack1|)) )
                 (when parseout
                       (let ((|$OutputStream| out-stream))
                         (S-PROCESS parseout))
                       (format out-stream "~&")))
               ;(IOClear in-stream out-stream)
               )))
      (IOClear in-stream out-stream)))
    (if *spad-input-file* (shut in-stream))
    (if *spad-output-file* (shut out-stream)))
  T))

(defun READ-SPAD1 (FN FT FM TO)
    (LET ((STRM IN-STREAM))
      (SETQ $MAXLINENUMBER 0)
      (SETQ $SPAD_ERRORS (VECTOR 0 0 0))
      (SETQ IN-STREAM (open (strconc fm ">" fn "." ft) :direction :input))
      ($ERASE (LIST FN 'ERROR 'A))
      (SETQ OUT-STREAM (if TO (open to :direction :output) OUT-STREAM))
      (SETQ SPADERRORSTREAM (open (strconc "a>" fn ".error") :direction :output))
      (READ-SPAD-1)
      (close SPADERRORSTREAM)
      (SETQ IN-STREAM STRM)
      (OR (EQUAL #(0 0 0) $SPAD_ERRORS)
          (|sayBrightly| (LIST '|%b| (ELT $SPAD_ERRORS 0) '|%d| '|syntax errors|
            '|%l| '|%b| (ELT $SPAD_ERRORS 1) '|%d| '|precompilation errors|
            '|%l| '|%b| (ELT $SPAD_ERRORS 2) '|%d| '|semantic errors| '|%l|)))
      (+ (ELT $SPAD_ERRORS 0) (ELT $SPAD_ERRORS 1) (ELT $SPAD_ERRORS 2))))


;  *** 3. BOOT Token Handling ***

(defun get-BOOT-token (token)

  "If you have an _, go to the next line.
If you have a . followed by an integer, get a floating point number.
Otherwise, get a .. identifier."

  (if (not (|skipBlankChars|))
      nil
      (let ((token-type (|tokenLookaheadType| (|currentChar|))))
        (case token-type
          (eof                  (|tokenInstall| nil '*eof token |$nonblank|))
          (escape               (|advanceChar!|)
                                (|getIdentifier| token t))
          (argument-designator  (get-argument-designator-token token))
          (id                   (|getIdentifier| token nil))
          (num                  (get-spad-integer-token token))
          (string               (|getSpadString| token))
          (special-char         (|getSpecial| token))
          (t                    (|getGliph| token token-type))))))

(defun get-argument-designator-token (token)
  (|advanceChar!|)
  (get-number-token token)
  (|tokenInstall| (intern (strconc "#" (format nil "~D" (|tokenSymbol| token))))
                 'argument-designator token |$nonblank|))


;; -*- Parse an integer number -*-
;; The number may be written in plain format, where the radix
;; is implicitly taken to be 10.  Or the spelling can explicitly
;; specify a radix.  That radix can be anything in the range 2..36

;; Subroutine GET-NUMBER-TOKEN-MAYBE-WITH-RADIX.
;; Read a the characters of a decimal integer and returns its
;; value.
(defun get-decimal-number-token (buf)
  (tagbody lp 
	   (suffix (|currentChar|) buf)
	   (let ((next-chr (|nextChar|)))
	     (cond ((digitp next-chr)
		    (|advanceChar!|)
		    (go lp)))))
  (parse-integer buf))

;; Subroutine of GET-NUMBER-TOKEN-MAYBE-WITH-RADIX.
;; We just read the radix of an integer number; parse the
;; digits forming that integer token.
(defun get-integer-in-radix (buf r)
  (unless (> r 1)
    (spad_syntax_error))
  (let ((mark (1+ (size buf))))
    (tagbody lp
	     (suffix (|currentChar|) buf)
	     (let* ((nxt (|nextChar|))
		    (dig (|rdigit?| nxt)))
	       (when dig
		 (unless (< dig r)
		   (spad_syntax_error))
		 (|advanceChar!|)
		 (go lp))))
    (parse-integer buf :start mark :radix r)))

(defun is-radix-char (c)
  (or (eql c #\r)
      (eql c #\R)))

;; Parse an integer token, written either implicitly in decimal form,
;; or explicitly specified radix.
(defun get-spad-integer-token (token)
  (let* ((buf (make-adjustable-string 0))
	 (val (get-decimal-number-token buf)))
    (|advanceChar!|)
    (when (is-radix-char (|currentChar|))
      (setq val (get-integer-in-radix buf val))
      (|advanceChar!|))
    (|tokenInstall| val 'number token (size buf))))

; **** 4. BOOT token parsing actions


(defun TRANSLABEL (X AL) (TRANSLABEL1 X AL) X)

(defun TRANSLABEL1 (X AL)
 "Transforms X according to AL = ((<label> . Sexpr) ..)."
  (COND ((simple-vector-p X)
         (do ((i 0 (1+ i))
              (k (maxindex x)))
             ((> i k))
           (if (LET ((Y (LASSOC (ELT X I) AL))) (SETF (ELT X I) Y))
               (TRANSLABEL1 (ELT X I) AL))))
        ((ATOM X) NIL)
        ((LET ((Y (LASSOC (FIRST X) AL)))
           (if Y (setf (FIRST X) Y) (TRANSLABEL1 (CDR X) AL))))
        ((TRANSLABEL1 (FIRST X) AL) (TRANSLABEL1 (CDR X) AL))))

; **** 5. BOOT Error Handling

(defun SPAD_SYNTAX_ERROR (&rest byebye)
  "Print syntax error indication, underline character, scrub line."
  (BUMPERRORCOUNT '|syntax|)
  (COND ((AND (EQ DEBUGMODE 'YES) (NOT(CONSOLEINPUTP IN-STREAM)))
         (SPAD_LONG_ERROR))
        ((SPAD_SHORT_ERROR)))
  (IOClear)
  (throw 'spad_reader nil))

(defun SPAD_LONG_ERROR ()
  (SPAD_ERROR_LOC SPADERRORSTREAM)
  (iostat)
  (unless (EQUAL OUT-STREAM SPADERRORSTREAM)
    (SPAD_ERROR_LOC OUT-STREAM)
    (TERPRI OUT-STREAM)))

(defun SPAD_SHORT_ERROR () (current-line-show))

(defun SPAD_ERROR_LOC (STR)
  (format str "******** Spad Syntax Error detected ********"))

