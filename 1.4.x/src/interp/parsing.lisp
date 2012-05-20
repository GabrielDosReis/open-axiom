;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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
;;     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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


; NAME:    META/LISP Parser Generator and Lexical Analysis Utilities (Parsing)
;
; PURPOSE: This package provides routines to support the Metalanguage
;          translator writing system.   Metalanguage is described
;          in META/LISP, R.D. Jenks, Tech Report, IBM T.J. Watson Research Center,
;          1969.  Familiarity with this document is assumed.
;
;          The parser generator itself is described in either the file
;          MetaBoot.lisp (hand-coded version) or the file MetaMeta.lisp (machine
;          generated from self-descriptive Meta code), both of which load themselves
;          into package Parsing.

; CONTENTS:
;
;       2. Recursive descent parsing support routines
;               A. Stacking and retrieving reductions of rules.
;               B. Applying metagrammatical elements of a production (e.g., Star).
;
;       3. Routines for handling lexical scanning
;
;               A. Manipulating the token stack and reading tokens
;               B. Error handling
;               C. Constructing parsing procedures
;               D. Managing rule sets
;
;       4. Tracing routines
;
;       5. Routines for inspecting and resetting total I/O system state
;


(import-module "lexing")
(import-module "macros")

(in-package "BOOT")


; 0. Current I/O Stream definition

(MAKEPROP 'END_UNIT 'KEY 'T)

(defparameter out-stream t "Current output stream.")

(defun Line-Print (line)
  (format out-stream "~&~5D> ~A~%" (|lineNumber| line) (|lineBuffer| Line))
  (format out-stream "~v@T^~%" (+ 7 (|lineCurrentIndex| line))))

(defun make-string-adjustable (s)
  (cond ((adjustable-array-p s) s)
        (t (make-array (array-dimensions s) :element-type 'character
                       :adjustable t :initial-contents s))))

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
 
(defun make-adjustable-string (n)
  (make-array (list n) :element-type 'character :adjustable t))
 
; *** 5. META Error Handling

(defparameter $num_of_meta_errors 0)

(defparameter Meta_Errors_Occurred nil  "Did any errors occur")

(defun IOStreams-Show ()
  (format t "~&Input is coming from ~A, and output is going to ~A.~%"
           (or (streamp in-stream) "the keyboard")
           (or (streamp out-stream) "the screen"))
  (format t "~:[~;The current input stream is logically closed.~%~]~%"
	  (|eof?| in-stream)))

(defmacro IOStreams-Set (input output) `(setq in-stream ,input out-stream ,output))

(defmacro IOStreams-Clear (&optional (in t) (out t))
  `(progn (and (streamp in-stream) (close in-stream))
          (and (streamp out-stream) (close out-stream))
          (IOStreams-Set ,in ,out)))

; 2B. Routines for applying certain metagrammatical elements
;     of a production (e.g., Star).

; Must means that if it is not present in the token stream, it is a syntax error.

; FUNCTIONS DEFINED IN THIS SECTION:
;
;       Star, Bang, Must, Optional, Action

(defmacro Star (lab prod)

"Succeeds if there are one or more of PROD, stacking as one unit
the sub-reductions of PROD and labelling them with LAB.
E.G., (Star IDs (parse-id)) with A B C will stack (3 IDs (A B C)),
where (parse-id) would stack (1 ID (A)) when applied once."

  `(prog ((oldstacksize (|stackSize| |$reduceStack|)))
         (if (not ,prod) ;(progn (format t "~&Star failed for ~A.~%" ',lab) (return nil)))
             (return nil))
    loop (if (not ,prod)
             (let* ((newstacksize (|stackSize| |$reduceStack|))
                    (number-of-new-reductions (- newstacksize oldstacksize)))
;              (format t "~&Starring ~A with ~D new reductions.~%"
;                      ',lab number-of-new-reductions)
               (if (> number-of-new-reductions 0)
                   (return (do ((i 0 (1+ i)) (accum nil))
                               ((= i number-of-new-reductions)
                                (|pushReduction| ',lab accum)
;                               (format t "~&Star accumulated ~D reductions.~%"
;                                       (length accum))
                                (return t))
                             (push (|popStack1|) accum)))
                   (return t)))
             (go loop))))

(defmacro Bang (lab prod)

"If the execution of prod does not result in an increase in the size of
the stack, then stack a NIL. Return the value of prod."

  `(progn (setf (|stackUpdated?| |$reduceStack|) nil)
          (let* ((prodvalue ,prod)
                 (updated (|stackUpdated?| |$reduceStack|)))
            (if updated
                (progn ; (format t "~&Banged ~A and I think the stack is updated!~%" ',lab)
                       prodvalue)
                (progn (|pushReduction| ',lab nil)
                       prodvalue)))))

(defmacro must (dothis &optional (this-is nil) (in-rule nil))
  `(or ,dothis (spad_syntax_error ,this-is ,in-rule)))

; Optional means that if it is present in the token stream, that is a good thing,
; otherwise don't worry (like [ foo ] in BNF notation).

(defun Optional (dothis) (or dothis t))

; Action is something we do as a consequence of successful parsing; it is
; inserted at the end of the conjunction of requirements for a successful
; parse, and so should return T.

(defun action (dothis) (or dothis t))

; 3A.  Manipulating the token stack and reading tokens

; This section is broken up into 3 levels:
;
;       (0) String grabbing:    Match String, Match Advance String
;       (1) Token handling:     Current Token, Next Token, Advance Token
;       (2) Character handling: Current Char, Next Char, Advance Char
;       (3) Line handling:      Next Line, Print Next Line
;       (X) Random Stuff

(defun match-advance-special (str)
  (and (|matchToken| (|currentToken|) 'special-char (character str))
       (action (|advanceToken|))))

(defun match-special (str)
  (|matchToken| (|currentToken|) 'special-char (character str)))

(defun match-keyword-next (str)
  (|matchToken| (|nextToken|) 'keyword (intern str)))

(defun initial-substring-p (part whole)
  "Returns length of part if part matches initial segment of whole."
  (let ((x (string<= part whole)))
    (and x (= x (length part)) x)))

; 3B. Error handling

(defparameter line nil)

(defun termchr ()  "Is CHR a terminating character?"
  (|findChar| (|currentChar|) " *,;<>()[]/\\"))

;       5. Routines for inspecting and resetting total I/O system state
;
; The package largely assumes that:
;
;       A. One I/O stream pair is in effect at any moment.
;       B. There is a Current Line
;       C. There is a Current Token and a Next Token
;       D. There is a Reduction Stack
;
; This state may be examined and reset with the procedures IOSTAT and IOCLEAR.

(defun IOStat ()
  "Tell me what the current state of the parsing world is."
  (current-line-show)
  (if $SPAD (next-lines-show))
  (token-stack-show)
  nil)

(defun IOClear (&optional (in t) (out t))
  ;(IOStreams-clear in out)
  (current-line-clear)
  (|tokenStackClear!|)
  (|reduceStackClear|)
  (if $SPAD (next-lines-clear))
  nil)

;; auxiliary functions needed by the parser

(Defun FLOATEXPID (X &aux S)
  (if (AND (|ident?| X) (char= (char-upcase (ELT (SETQ S (PNAME X)) 0)) #\E)
           (> (LENGTH S) 1)
           (SPADREDUCE AND 0 (COLLECT (STEP I 1 1 (|maxIndex| S))
                                      (DIGITP (ELT S I)))))
       (READ-FROM-STRING S t nil :start 1)
    NIL))

(defun |dollarTran| (dom rand)
       (let ((eltWord (if |$InteractiveMode| '|$elt| '|elt|)))
         (if (and (not (atom rand)) (cdr rand))
             (cons (list eltWord dom (car rand)) (cdr rand))
             (list eltWord dom rand))))
