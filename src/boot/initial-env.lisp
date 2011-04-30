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
;;


;;
;; Abstract:
;;  This file defines the base initial environment for building
;;  a Boot translator image.  It essentially etablishes a namespace
;;  (package BOOTTRAN) for the Boot translator, and defines 
;;  some macros that need to be present during translation of Boot
;;  source files.
;; 

(defpackage "BOOTTRAN"
  (:use "AxiomCore")
  #+:common-lisp  (:use "COMMON-LISP")
  #-:common-lisp  (:use "LISP")
  (:export "loadNativeModule"
	   "loadSystemRuntimeCore"
           "$InteractiveMode"
	   "string2BootTree"
	   "genImportDeclaration"))

(in-package "BOOTTRAN")

(eval-when (:compile-toplevel :load-toplevel :execute)
	   (progn
	     (setq *read-default-float-format* 'double-float)
	     (setq *load-verbose* nil)))

;## need the conditional here so it appears in boottran
#+:ieee-floating-point (defparameter $ieee t)
#-:ieee-floating-point (defparameter $ieee nil)

;; when true indicate that that the Boot translator
;; is called interactively.
(defparameter |$InteractiveMode| nil)

(defvar *lisp-bin-filetype* "o")

(defvar *lisp-source-filetype* "lisp")

(defun |shoeInputFile| (filespec )
  (open filespec :direction :input :if-does-not-exist nil))

(defmacro |shoeOpenInputFile|
  (stream fn prog)
    `(with-open-file (,stream ,fn :direction :input
       :if-does-not-exist nil) ,prog))

(defmacro |shoeOpenOutputFile|
  (stream fn prog)
    `(with-open-file (,stream ,fn :direction :output
       :if-exists :supersede) ,prog))

(defun shoeprettyprin1 (x &optional (stream *standard-output*))
  (let ((*print-pretty* t)
        (*print-array* t)
        (*print-circle* t)
        (*print-level* nil)
        (*print-length* nil))
    (prin1 x stream)))
 
(defun reallyprettyprint (x &optional (stream *terminal-io*))
  (shoeprettyprin1 x stream) (terpri stream))
 
(defun shoeprettyprin0 (x &optional (stream *standard-output*))
  (let ((*print-pretty* nil)
        (*print-array* t)
        (*print-circle* t)
        (*print-level* nil)
        (*print-length* nil))
    (prin1 x stream)))
 
(defun shoenotprettyprint (x &optional (stream *terminal-io*))
  (shoeprettyprin0 x stream) 
  (terpri stream))

(defun |shoePLACEP| (item) 
  (eq item nil))

(defun MAKE-HASHTABLE (id1)
  (let ((test (case id1
                    ((EQ ID) #'eq)
                    (CVEC #'equal)
                    ((UEQUAL EQUAL) #'equal)
                    (otherwise (error "bad arg to make-hashtable")))))
    (make-hash-table :test test)))

(defun HKEYS (table)
  (let (keys)
    (maphash #'(lambda (key val) 
                 (declare (ignore val))
                 (push key keys)) table)
    keys))


(defun HPUT (table key value)
  (setf (gethash key table) value))
 
(defun strpos (what in start dontcare)
  (setq what (string what) in (string in))
  (if dontcare
      (progn 
	(setq dontcare (character dontcare))
	(search what in :start2 start
		:test #'(lambda (x y) (or (eql x dontcare)
					  (eql x y)))))
    (search what in :start2 start)))
 

(defun strposl (table cvec sint item)
  (setq cvec (string cvec))
  (if (not item)
      (position table cvec 
		:test #'(lambda (x y) (position y x))
		:start sint)
    (position table cvec 
	      :test-not #'(lambda (x y) (position y x))
              :start sint)))

(defun  bvec-make-full (n x)
  (make-array (list n) 
	      :element-type 'bit
	      :initial-element x))

(defun make-bvec (n)
  (bvec-make-full n 0))
 
(defun |shoeReadLisp| (s n)
  (multiple-value-list (read-from-string s nil nil :start n)))

(defun |last| (x)
  (car (last x)))
