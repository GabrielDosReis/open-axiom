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

; *** 1. BOOT file handling

(defun print-defun (name body)
   (let* ((sp (assoc 'compiler-output-stream optionlist))
          (st (if sp (cdr sp) |$OutputStream|)))
     (if (and (|ioTerminal?| st) (symbolp name) (fboundp name)
              (not (compiled-function-p (symbol-function name))))
         (compile name))
     (when (or |$PrettyPrint| (not (|ioTerminal?| st)))
           (print-full body st) (force-output st))))


;  *** 3. BOOT Token Handling ***

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
              (k (|maxIndex| x)))
             ((> i k))
           (if (LET ((Y (LASSOC (ELT X I) AL))) (SETF (ELT X I) Y))
               (TRANSLABEL1 (ELT X I) AL))))
        ((ATOM X) NIL)
        ((LET ((Y (LASSOC (FIRST X) AL)))
           (if Y (setf (FIRST X) Y) (TRANSLABEL1 (CDR X) AL))))
        ((TRANSLABEL1 (FIRST X) AL) (TRANSLABEL1 (CDR X) AL))))

