;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


; NAME:         MetaLex.lisp
; PURPOSE:      Parsing support routines for Meta code
; CONTENTS:
;
;               1. META File Handling
;               2. META Line Handling
;               3. META Token Handling
;               4. META Token Parsing Actions
;               5. META Error Handling
 
(in-package "BOOT")
 
; *** 2. META Line Handling
 
(defun next-META-line (&optional (in-stream t))
 
"Get next line, trimming trailing blanks and trailing comments.
One trailing blank is added to a non-blank line to ease between-line
processing for Next Token (i.e., blank takes place of return).  Returns T
if it gets a non-blank line, and NIL at end of stream."
 
  (prog (string)
empty (if File-Closed (return nil))
      (setq string (kill-trailing-blanks (kill-comments
                                          (get-a-line in-stream))))
      (if (= (length string) 0) (go empty))
      (Line-New-Line (suffix #\Space string) Current-Line)
      (if Echo-Meta (Print-New-Line (Line-Buffer Current-Line) out-stream))
      (return t)))
 
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
 
(defun-parse-token STRING)
(defun-parse-token BSTRING)
(defun-parse-token IDENTIFIER)
(defun-parse-token NUMBER)
 
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
  bye (return (token-install (intern buf) 'identifier token))))
 
(defun get-string-token (token)
  "With 'ABC' on IN-STREAM, extracts and stacks String 'ABC'."
  (let ((buf (make-adjustable-string 0)))
    (if (char= (current-char) #\')
        (progn (advance-char)
               (loop (case (current-char)
                       (#\' (advance-char)
                        (return (token-install buf 'string token)))
                       (#\\ (advance-char)
                        (suffix (current-char) buf)
                        (advance-char))
                       (#\Return
                        (moan "String should fit on one line!")
                        (advance-char)
                        (meta-syntax-error)
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
                                       (return (token-install buf 'bstring token)))
                                (progn (suffix (current-char) buf)
                                       (advance-char))))
                       (#\\ (advance-char)
                        (suffix (current-char) buf)
                        (advance-char))
                       (#\Return
                        (moan "String should fit on one line!")
                        (advance-char)
                        (meta-syntax-error)
                        (return nil))
                       (t (suffix (current-char) buf)
                          (advance-char))))))))
 
(defun get-special-token (token)
  "Take a special character off the input stream.  We let the type name of each
special character be the atom whose print name is the character itself."
  (let ((symbol (current-char)))
    (advance-char)
    (token-install symbol 'special-char token)))
 
(defun get-number-token (token)
  "Take a number off the input stream."
  (prog ((buf (make-adjustable-string 0)))
    nu1 (suffix (current-char) buf)                     ; Integer part
        (let ((next-chr (next-char)))
          (cond ((digitp next-chr)
                 (advance-char)
                 (go nu1))))
        (advance-char) 
 formint(return (token-install
                 (read-from-string buf)
                  'number token
                  (size buf) ;used to keep track of digit count
                  ))))
 
; *** 4. META Auxiliary Parsing Actions
 
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
 
(defparameter Meta_Prefix nil)
 
(defun set-prefix (prefix)  (setq META_PREFIX prefix))
 
(defun print-rule (x)  (print x out-stream) (format out-stream "~%~%"))
 
; *** 5. META Error Handling

(defparameter $num_of_meta_errors 0)
 
(defun meta-meta-error-handler (&optional (wanted nil) (parsing nil))
  "Print syntax error indication, underline character, scrub line."
  (format out-stream "~&% MetaLanguage syntax error: ")
  (if (Line-Past-End-P Current-Line)
      (cond ((and wanted parsing)
             (format out-stream "wanted ~A while parsing ~A.~%"
                     wanted parsing))
            (wanted (format out-stream "wanted ~A.~%" wanted))
            (parsing (format out-stream "while parsing ~A.~%" parsing)))
      (progn (format out-stream "~:[here~;wanted ~A here~]" wanted wanted)
             (format out-stream "~:[~; while parsing ~A~]:~%" parsing parsing)
             (current-line-print)
             (current-line-clear)
             (current-token)
             (incf $num_of_meta_errors)
             (setq Meta_Errors_Occurred t)))
   nil)
