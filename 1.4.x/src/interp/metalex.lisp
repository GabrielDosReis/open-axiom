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


; NAME:         MetaLex.lisp
; PURPOSE:      Parsing support routines for Meta code
; CONTENTS:
;
;               1. META File Handling
;               2. META Line Handling
;               4. META Token Parsing Actions
;               5. META Error Handling

(IMPORT-MODULE "lexing") 
(IMPORT-MODULE "macros") 
(in-package "BOOT")

; 0. Current I/O Stream definition

(defparameter out-stream t "Current output stream.")
(defparameter File-Closed nil   "Way to stop EOF tests for console input.")


; 1. Data structure declarations (defstructs) for parsing objects
;
;               A. Line Buffer

; 1A. A Line Buffer
;
; The philosophy of lines is that
;
;       a) NEXT LINE will always get you a non-blank line or fail.
;       b) Every line is terminated by a blank character.
;
; Hence there is always a current character, because there is never a non-blank line,
; and there is always a separator character between tokens on separate lines.
; Also, when a line is read, the character pointer is always positioned ON the first
; character.

; FUNCTIONS DEFINED IN THIS SECTION:
;
;       Line-New-Line, Line-Advance-Char, Line-Past-End-P, Line-At-End-P
;       Make-Line

(defun Line-Print (line)
  (format out-stream "~&~5D> ~A~%" (|lineNumber| line) (|lineBuffer| Line))
  (format out-stream "~v@T^~%" (+ 7 (|lineCurrentIndex| line))))

; *** Next Line

(defun make-string-adjustable (s)
  (cond ((adjustable-array-p s) s)
        (t (make-array (array-dimensions s) :element-type 'character
                       :adjustable t :initial-contents s))))

(defun get-a-line (stream)
  (if (and (IS-CONSOLE stream) (not |$leanMode|))
      (|printPrompt|))
  (let ((ll (read-a-line stream)))
    (if (stringp ll) (make-string-adjustable ll) ll)))

(defparameter Current-Fragment nil
  "A string containing remaining chars from readline; needed because
Symbolics read-line returns embedded newlines in a c-m-Y.")

(defun input-clear () (setq Current-Fragment nil))

(defun Next-Lines-Clear () (setq |$lineStack| nil))

(defun Next-Lines-Show ()
  (and |$lineStack| (format t "Currently preparsed lines are:~%~%"))
  (mapcar #'(lambda (line)
              (format t "~&~5D> ~A~%" (car line) (cdr Line)))
          |$lineStack|))


; 3. Routines for handling lexical scanning
;
; Lexical scanning of tokens is performed off of the current line.  No
; token can span more than 1 line.  All real I/O is handled in a line-oriented
; fashion (in a slight paradox) below the character level.  All character
; routines implicitly assume the parameter |$spadLine|.  We do not make
; |$spadLine| an explicit optional parameter for reasons of efficiency.

(defmacro current-line-print () '(Line-Print |$spadLine|))

(defmacro current-line-show ()
  `(if (|linePastEnd?| |$spadLine|)
       (format t "~&The current line is empty.~%")
       (progn (format t "~&The current line is:~%~%")
              (current-line-print))))

(defmacro current-line-clear () `(|lineClear!| |$spadLine|))

(defun read-a-line (&optional (stream t))
  (let (cp)
    (if (and Current-Fragment (> (length Current-Fragment) 0))
        (let ((line (with-input-from-string
                      (s Current-Fragment :index cp :start 0)
                      (read-line s nil nil))))
          (setq Current-Fragment (subseq Current-Fragment cp))
          line)
        (prog nil
              (if (stream-eof in-stream)
                  (progn (setq File-Closed t *EOF* t)
                         (|lineNewLine!| (make-string 0) |$spadLine|)
                         (return nil)))
              (if (setq Current-Fragment (read-line stream))
                  (return (read-a-line stream)))))))

; *** Print New Line

(defparameter Printer-Line-Stack (|makeStack|)
  "Stack of output listing lines waiting to print. [local to PRINT-NEW-LINE]")

(defparameter Read-Quietly nil
  "Whether or not to produce an output listing. [local to PRINT-NEW-LINE]")

(defun Print-New-Line (string &optional (strm |$OutputStream|))
  "Makes output listings."
  (if Read-Quietly (|stackPush!| (copy-tree string) Printer-Line-Stack)
      (progn (mapc #'(lambda (x) (format strm "; ~A~%" x) (terpri))
                   (|reverse!| (|stackStore| Printer-Line-Stack)))
             (|stackClear!| Printer-Line-Stack)
             (format strm "~&; ~A~%" string))))

; 1C. Token
(defun Token-Print (token)
  (format out-stream "(token (symbol ~S) (type ~S))~%"
          (|tokenSymbol| token) (|tokenType| token)))

(defun reduce-stack-show ()
  (let ((store (|stackStore| |$reduceStack|))
        (*print-pretty* t))
    (if store
        (progn (format t "~%Reduction stack contains:~%")
               (mapcar #'(lambda (x)
			   (if (eq (type-of x) 'token)
                               (describe x)
			     (print x)))
                       (|stackStore| |$reduceStack|)))
        (format t "~%There is nothing on the reduction stack.~%"))))

 
; *** 3. META Token Handling
 
; STRING: "'"  { Chars - "'" }* "'"
; BSTRING: "[" ... "]*"
; ID: letters, _ and then numbers
; NUMBER: digits, ., digits, e, +-, digits

; 3A (1) Token Handling.

; Tokens are acquired from a stream of characters.  Lexical analysis is performed
; by the functiond Get Token.  One-token lookahead is maintained in variables
; |$CurrentToken| and |$NextToken| by procedures Current Token, Next Token, and
; Advance Token.  The functions Match Current Token and Match Next Token recognize
; classes of tokens, by type, or by type and symbol.  The current and next tokens
; can be shoved back on the input stream (to the current line) with Unget-Tokens.

(defmacro Defun-Parse-Token (token)
  `(defun ,(intern (concatenate 'string "PARSE-" (string token))) ()
     (let* ((tok (|matchCurrentToken| ',token))
            (symbol (if tok (|tokenSymbol| tok))))
       (if tok (progn (|pushReduction|
                        ',(intern (concatenate 'string (string token)
                                               "-TOKEN"))
                        (copy-tree symbol))
                      (|advanceToken|)
                      t)))))

(defun token-stack-show ()
  (if (= |$validTokens| 0) (format t "~%There are no valid tokens.~%")
      (format t "~%The number of valid tokens is ~S.~%" |$validTokens|))
  (if (> |$validTokens| 0)
      (progn (format t "The current token is~%")
             (describe |$currentToken|)))
  (if (> |$validTokens| 1)
      (progn (format t "The next token is~%")
             (describe |$nextToken|)))
  (if (|tokenType| |$priorToken|)
      (progn (format t "The prior token was~%")
             (describe |$priorToken|))))

 
(defun-parse-token STRING)
(defun-parse-token BSTRING)
(defun-parse-token IDENTIFIER)
(defun-parse-token NUMBER)
 
; Parsing of operator tokens depends on tables initialized by BOTTOMUP.LISP

(defun-parse-token SPADSTRING)
(defun-parse-token KEYWORD)
(defun-parse-token ARGUMENT-DESIGNATOR)

(defun |PARSE-OperatorFunctionName| ()
  (let ((id (|makeSymbolOf| (or (|matchCurrentToken| 'keyword)
				(|matchCurrentToken| 'gliph)
				(|matchCurrentToken| 'special-char)))))
    (when (and id (member id |$OperatorFunctionNames|))
      (|pushReduction| '|PARSE-OperatorFunctionName| id)
      (action (|advanceToken|)))))
 
(defun make-adjustable-string (n)
  (make-array (list n) :element-type 'character :adjustable t))
 
(defun get-special-token (token)
  "Take a special character off the input stream.  We let the type name of each
special character be the atom whose print name is the character itself."
  (let ((symbol (|currentChar|)))
    (|advanceChar!|)
    (|tokenInstall| symbol 'special-char token)))
 
(defun get-number-token (token)
  "Take a number off the input stream."
  (prog ((buf (make-adjustable-string 0)))
	nu1 
	(suffix (|currentChar|) buf)                     ; Integer part
        (let ((next-chr (|nextChar|)))
          (cond ((digitp next-chr)
                 (|advanceChar!|)
                 (go nu1))))
        (|advanceChar!|) 
	(return (|tokenInstall| (read-from-string buf)
			       'number token
			       (size buf) ;used to keep track of digit count
			       ))))
 
; *** 4. META Auxiliary Parsing Actions
 
(defparameter Meta_Prefix nil)
 
(defun make-defun (nametok vars body)
  (let ((name (INTERN (STRCONC |META_PREFIX| nametok))))
    (if vars
        `(DEFUN ,name ,vars (declare (special . ,vars)) ,body)
        `(DEFUN ,name ,vars ,body))))
 
(defun print-fluids (fluids)
  (terpri out-stream)
  (mapcar #'(lambda (x) (format out-stream "~&(DEFPARAMETER ~S NIL)~%" x)) fluids)
  (terpri out-stream))
 
(defun print-package (package)
  (format out-stream "~&~%(IN-PACKAGE ~S )~%~%" package))
 
(defun set-prefix (prefix)  (setq META_PREFIX prefix))
 
(defun print-rule (x)  (print x out-stream) (format out-stream "~%~%"))
 
; *** 5. META Error Handling

(defparameter $num_of_meta_errors 0)

(defparameter Meta_Errors_Occurred nil  "Did any errors occur")
