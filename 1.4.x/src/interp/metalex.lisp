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

(defparameter in-stream  t "Current input stream.")
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
;       Line-Buffer, Line-Current-Char, Line-Current-Index, Line-Last-Index, Line-Number
;       Line-New-Line, Line-Advance-Char, Line-Past-End-P, Line-At-End-P
;       Make-Line

(defstruct Line "Line of input file to parse."
           (Buffer (make-string 0) :type string)
           (Current-Char #\Return :type character)
           (Current-Index 1 :type fixnum)
           (Last-Index 0 :type fixnum)
           (Number 0 :type fixnum))

(defun Line-Print (line)
  (format out-stream "~&~5D> ~A~%" (Line-Number line) (Line-Buffer Line))
  (format out-stream "~v@T^~%" (+ 7 (Line-Current-Index line))))

(defmacro Line-Clear (line)
  `(let ((l ,line))
     (setf (Line-Buffer l) (make-string 0)
           (Line-Current-Char l) #\Return
           (Line-Current-Index l) 1
           (Line-Last-Index l) 0
           (Line-Number l) 0)))

(defun Line-Current-Segment (line)
  "Buffer from current index to last index."
  (if (line-at-end-p line) (make-string 0)
      (subseq (Line-Buffer line)
              (Line-Current-Index line)
              (Line-Last-Index line))))

(defun Line-New-Line (string line &optional (linenum nil))
  "Sets string to be the next line stored in line."
  (setf (Line-Last-Index line) (1- (length string))
        (Line-Current-Index line) 0
        (Line-Current-Char line) (or (and (> (length string) 0) (elt string 0)) #\Return)
        (Line-Buffer line) string
        (Line-Number line) (or linenum (1+ (Line-Number line)))))

(defun Line-Advance-Char (line)
  (setf (Line-Current-Char line)
        (elt (Line-Buffer line) (incf (Line-Current-Index line)))))

(defun Line-Next-Char (line)
  (elt (Line-Buffer line) (1+ (Line-Current-Index line))))

(defun Line-Past-End-P (line)
  "Tests if line is empty or positioned past the last character."
  (> (line-current-index line) (line-last-index line)))

(defun Line-At-End-P (line)
  "Tests if line is empty or positioned past the last character."
  (>= (line-current-index line) (line-last-index line)))

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


(defparameter Boot-Line-Stack nil       "List of lines returned from PREPARSE.")

(defun Next-Lines-Clear () (setq Boot-Line-Stack nil))

(defun Next-Lines-Show ()
  (and Boot-Line-Stack (format t "Currently preparsed lines are:~%~%"))
  (mapcar #'(lambda (line)
              (format t "~&~5D> ~A~%" (car line) (cdr Line)))
          Boot-Line-Stack))


; 3. Routines for handling lexical scanning
;
; Lexical scanning of tokens is performed off of the current line.  No
; token can span more than 1 line.  All real I/O is handled in a line-oriented
; fashion (in a slight paradox) below the character level.  All character
; routines implicitly assume the parameter Current-Line.  We do not make
; Current-Line an explicit optional parameter for reasons of efficiency.

(defparameter Current-Line (make-line)  "Current input line.")

(defmacro current-line-print () '(Line-Print Current-Line))

(defmacro current-line-show ()
  `(if (line-past-end-p current-line)
       (format t "~&The current line is empty.~%")
       (progn (format t "~&The current line is:~%~%")
              (current-line-print))))

(defmacro current-line-clear () `(Line-Clear Current-Line))

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
                         (Line-New-Line (make-string 0) Current-Line)
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


; 3A (2) Character handling.

; FUNCTIONS DEFINED IN THIS SECTION:
;
;       Current-Char, Next-Char, Advance-Char

; *** Current Char, Next Char, Advance Char

(defun Current-Char ()
  "Returns the current character of the line, initially blank for an unread line."
  (if (Line-Past-End-P Current-Line) #\Return (Line-Current-Char Current-Line)))

(defun Next-Char ()
   "Returns the character after the current character, blank if at end of line.
The blank-at-end-of-line assumption is allowable because we assume that end-of-line
is a token separator, which blank is equivalent to."

  (if (Line-At-End-P Current-Line) #\Return (Line-Next-Char Current-Line)))

(defun Advance-Char ()
  "Advances IN-STREAM, invoking Next Line if necessary."
  (loop (cond ((not (Line-At-End-P Current-Line))
               (return (Line-Advance-Char Current-Line)))
              ((next-boot-line in-stream)
               (return (current-char)))
              ((return nil)))))

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

 
; *** 2. META Line Handling
(defparameter Comment-Character #\% "Delimiter of comments in Meta code.")
 
(defun kill-comments (string)
  "Deletes from comment character % to end of STRING."
  (subseq string 0
          (let ((mi (maxindex string)))
            (do ((i 0 (1+ i)))
                ((> i mi) i)
              (if (and (char= (elt string i) Comment-Character)
                       (or (eq i 0) (char/= (elt string (1- i)) #\\)))
                  (return i))))))
 
(defun kill-trailing-blanks (string)
 
  "Remove white space from end of STRING."
 
  ; Coding note: yes, I know, use string-trim --  but it is broken
  ; in Symbolics Common Lisp for short strings
 
  (let* ((sl (length string))
         (right (if (= sl 0) -1
                    (or
                      (position-if-not
                        #'(lambda (x)
                            (member x '(#\Space #\Tab #\Newline) :test #'char=))
                        string :from-end t)
                      -1))))
    (if (>= right 0) (subseq string 0 (1+ right)) (make-string 0))))
 
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

(defmacro token-stack-clear ()
  `(progn (setq |$validTokens| 0)
          (|tokenInstall| nil nil |$currentToken| nil)
          (|tokenInstall| nil nil |$nextToken| nil)
          (|tokenInstall| nil nil |$priorToken| nil)))

; Unget-Tokens

(defun quote-if-string (token)
  (if token   ;only use |tokenType| on non-null tokens
  (case (|tokenType| token)
    (bstring            (strconc "[" (|tokenSymbol| token) "]*"))
    (string             (strconc "'" (|tokenSymbol| token) "'"))
    (spadstring         (strconc "\"" (underscore (|tokenSymbol| token)) "\""))
    (number             (format nil "~v,'0D" (|tokenNonblank?| token)
                                (|tokenSymbol| token)))
    (special-char       (string (|tokenSymbol| token)))
    (identifier         (let ((id (symbol-name (|tokenSymbol| token)))
                              (pack (package-name (symbol-package
                                                   (|tokenSymbol| token)))))
                          (if $SPAD
                              (if (equal pack "BOOT")
                                  (escape-keywords (underscore id) (|tokenSymbol| token))
                                (concatenate 'string
                                             (underscore pack) "'" (underscore id)))
                            id)))
    (t                  (|tokenSymbol| token)))
   nil))


(defconstant Keywords 
  '(|or| |and| |isnt| |is| |when| |where| |forall| |exist| |try|
    |has| |with| |add| |case| |in| |by| |pretend| |mod| |finally|
    |exquo| |div| |quo| |else| |rem| |then| |suchthat| |catch| |throw|
    |if| |yield| |iterate| |break| |from| |exit| |leave| |return|
    |not| |unless| |repeat| |until| |while| |for| |import| |inline|)

"Alphabetic literal strings occurring in the New Meta code constitute
keywords.   These are recognized specifically by the AnyId production,
GET-BOOT-IDENTIFIER will recognize keywords but flag them
as keywords.")



(defun escape-keywords (pname id)
  (if (member id keywords)
      (concatenate 'string "_" pname)
    pname))

(defun underscore (string)
  (if (every #'alpha-char-p string) string
    (let* ((size (length string))
           (out-string (make-array (* 2 size)
                                   :element-type 'character
                                   :fill-pointer 0))
           next-char)
      (dotimes (i size)
               (setq next-char (char string i))
               (if (not (alpha-char-p next-char))
                   (vector-push #\_ out-string))
               (vector-push next-char out-string))
      out-string)))

(defun Unget-Tokens ()
  (case |$validTokens|
    (0 t)
    (1 (let* ((cursym (quote-if-string |$currentToken|))
              (curline (line-current-segment current-line))
              (revised-line (strconc cursym curline (copy-seq " "))))
         (line-new-line revised-line current-line (line-number current-line))
         (setq |$nonblank| (|tokenNonblank?| |$currentToken|))
         (setq |$validTokens| 0)))
    (2 (let* ((cursym (quote-if-string |$currentToken|))
              (nextsym (quote-if-string |$nextToken|))
              (curline (line-current-segment current-line))
              (revised-line
                (strconc (if (|tokenNonblank?| |$currentToken|) "" " ")
                         cursym
                         (if (|tokenNonblank?| |$nextToken|) "" " ")
                         nextsym curline " ")))
         (setq |$nonblank| (|tokenNonblank?| |$currentToken|))
         (line-new-line revised-line current-line (line-number current-line))
         (setq |$validTokens| 0)))
    (t (error "How many tokens do you think you have?"))))
 
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

; Meta tokens fall into the following categories:
;
;               Number
;               Identifier
;               Dollar-sign
;               Special character
;
; Special characters are represented as characters, numbers as numbers, and
; identifiers as strings.  The reason identifiers are represented as strings is
; that the full print-name of the intern of a string depends on the package you
; are currently executing in; this can lead to very confusing results!
 
(defun get-META-token (token)
  (prog nil
   loop (if (not (skip-blanks)) (return nil))
        (case (token-lookahead-type (current-char))
          (id           (return (get-identifier-token token)))
          (num          (return (get-number-token token)))
          (string       (return (get-string-token token)))
          (bstring      (return (get-bstring-token token)))
;         (dollar       (return (get-identifier-token token)))
          (special-char (return (get-special-token token)))
          (eof          (return nil)))))
 
(defun skip-blanks ()
  (loop (let ((cc (current-char)))
          (if (not cc) (return nil))
          (if (eq (token-lookahead-type cc) 'white)
              (if (not (advance-char)) (return nil))
              (return t)))))
 
(defparameter Escape-Character #\\ "Superquoting character.")
 
(defun token-lookahead-type (char)
  "Predicts the kind of token to follow, based on the given initial character."
  (cond ((not char)                                             'eof)
        ((or (char= char Escape-Character) (alpha-char-p char)) 'id)
        ((digitp char)                                          'num)
        ((char= char #\')                                       'string)
        ((char= char #\[)                                       'bstring)
;       ((char= char #\$) (advance-char)                        'dollar)
        ((member char '(#\Space #\Tab #\Return) :test #'char=)  'white)
        (t                                                      'special-char)))
 
(defun make-adjustable-string (n)
  (make-array (list n) :element-type 'character :adjustable t))

(defun get-identifier-token (token)
  "Take an identifier off the input stream."
  (prog ((buf (make-adjustable-string 0)))
   id (let ((cur-char (current-char)))
         (cond ((equal cur-char Escape-Character)
                (if (not (advance-char)) (go bye))
                (suffix (current-char) buf)
                (if (not (advance-char)) (go bye))
                (go id))
               ((or (alpha-char-p cur-char)
                    (char= cur-char #\-)
                    (digitp cur-char)
                    (char= cur-char #\_))
                (suffix (current-char) buf)
                (if (not (advance-char)) (go bye))
                (go id))))
  bye (return (|tokenInstall| (intern buf) 'identifier token))))
 
(defun get-string-token (token)
  "With 'ABC' on IN-STREAM, extracts and stacks String 'ABC'."
  (let ((buf (make-adjustable-string 0)))
    (if (char= (current-char) #\')
        (progn (advance-char)
               (loop (case (current-char)
                       (#\' (advance-char)
                        (return (|tokenInstall| buf 'string token)))
                       (#\\ (advance-char)
                        (suffix (current-char) buf)
                        (advance-char))
                       (#\Return
                        (moan "String should fit on one line!")
                        (advance-char)
                        (spad_syntax_error)
                        (return nil))
                       (t (suffix (current-char) buf)
                          (advance-char))))))))
 
(defun get-bstring-token (token)
  "With ABC]* on in-stream, extracts and stacks string ABC."
  (let ((buf (make-adjustable-string 0)))
    (if (char= (current-char) #\[)
        (progn (advance-char)
               (loop (case (current-char)
                       (#\] (if (char= (next-char) #\*)
                                (progn (advance-char)
                                       (advance-char)
                                       (return (|tokenInstall| buf 'bstring token)))
                                (progn (suffix (current-char) buf)
                                       (advance-char))))
                       (#\\ (advance-char)
                        (suffix (current-char) buf)
                        (advance-char))
                       (#\Return
                        (moan "String should fit on one line!")
                        (advance-char)
                        (spad_syntax_error)
                        (return nil))
                       (t (suffix (current-char) buf)
                          (advance-char))))))))
 
(defun get-special-token (token)
  "Take a special character off the input stream.  We let the type name of each
special character be the atom whose print name is the character itself."
  (let ((symbol (current-char)))
    (advance-char)
    (|tokenInstall| symbol 'special-char token)))
 
(defun get-number-token (token)
  "Take a number off the input stream."
  (prog ((buf (make-adjustable-string 0)))
	nu1 
	(suffix (current-char) buf)                     ; Integer part
        (let ((next-chr (next-char)))
          (cond ((digitp next-chr)
                 (advance-char)
                 (go nu1))))
        (advance-char) 
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
