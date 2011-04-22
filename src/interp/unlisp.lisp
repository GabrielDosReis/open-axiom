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

;; Uncommon 1.6
;; This package is a Boot interface for Common Lisp.
;; SMW 1989, 1990

(IMPORT-MODULE "sys-macros")
(in-package "BOOT")  

;;; This macro catches as much as it can.  
;;; Systems with a catchall should use it.  
;;; It is legitimate to not catch anything, if there is no system support.
;;; 
;;; If the result was caught, then tagvar is set to the desination tag
;;; and the thown value is returned.  Otherwise, tagvar is set to nil
;;; and the first result of the expression is returned.

#+:Lucid 
(defmacro |CatchAsCan| (tagvar expr)
  `(let ((catch-result nil) 
         (expr-result nil) 
         (normal-exit (gensym)))

        (setq catch-result 
          (catch 'lucid::top-level 
            (setq expr-result ,expr)
            normal-exit))
        (cond 
          ((eq catch-result normal-exit)
            (setq ,tagvar nil)
            expr-result )
          ('t
            (setq ,tagvar 'lucid::top-level)
            catch-result )) ))

#-:Lucid 
(defmacro |CatchAsCan| (tagvar expr)
  `(let ((,tagvar nil))
      ,expr ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro |Eq| (a b)
  `(eq ,a ,b) )

(defvar |Nil| nil)

(defun |Sort| (l pred)
  (sort (copy-tree l) pred) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Streams
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun |Prompt| (line &optional (readfn nil))
  (format |$OutputStream| "~a" line)
  (when readfn (apply readfn (list |$InputStream|))) )

(defun |PrettyPrint| (expr &optional (outstream |$OutputStream|))
  (write expr :stream outstream :level nil :length nil :pretty 't :escape 't) 
  (finish-output outstream) )

(defun plain-print-format-string (l)
  (format nil "~~~d{~~a~~}~~%" (length l)) )

;;;
;;; Character Sets
;;;

(defun |Cset| (str) 
  (let 
   ((cset (make-array
           (list char-code-limit)
           :element-type 'bit
           :initial-element 0 ))
    (len (length str)) )
   
   (do ((i 0 (+ 1 i)))
       ((= i len))
       (setf (sbit cset (char-code (char str i))) 1) )
   cset ))

(defvar |WhiteSpaceCset|   
  (|Cset| (coerce 
    (list #\Space #\Newline #\Tab #\Page #\Linefeed #\Return #\Backspace)
    'string )) )

;;;
;;; Association lists
;;;

(defun |AlistRemoveQ| (key l)
  (let ((pr (assoc key l :test #'eq)))
       (if pr 
           (remove pr l :test #'eq) 
           l) ))
