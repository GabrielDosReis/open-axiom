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


(defpackage "BOOT"
  #+:common-lisp (:use "COMMON-LISP")
  #-:common-lisp (:use "LISP")
  #+:SBCL (:use "SB-ALIEN")
  (:use "AxiomCore" "BOOTTRAN"))

(in-package "BOOT")
  
(eval-when (:compile-toplevel :load-toplevel :execute)
	   (progn
	     (setq *read-default-float-format* 'double-float)
	     (setq *load-verbose* nil)))

(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun define-function (f v)
   (setf (symbol-function f) v)))

(defun |gensym?| (s)
  (and (symbolp s) (null (symbol-package s))))

(defmacro |complex?| (x)
  `(complexp ,x))

(defmacro |complex| (x (&optional (y 0.0)))
  `(complex ,x ,y))

(defmacro |realPart| (z)
  `(realpart ,z))

(defmacro |imagPart| (z)
  `(imagpart ,z))

(defmacro |conjugate| (z)
  `(conjugate ,z))

(defmacro |sqrt| (x)
  `(sqrt ,x))

;; Below are some missing functions.  There here for lack of better
;; place (sys-funs.lisp?)
;;
;; These functions should be defined for DoubleFloat inputs but are not.
;; These are cheap and easy definitions that work but should be rewritten.

;; Contributed by Juergen Weiss from a suggestion by Arthur Norman.

(defun cot (a)
  (if (or (> a 1000.0) (< a -1000.0))
    (/ (cos a) (sin a))
    (/ 1.0 (tan a))))

(defun acot (a)
  (if (> a 0.0)
    (if (> a 1.0)
       (atan (/ 1.0 a))
       (- (/ pi 2.0) (atan a)))
    (if (< a -1.0)
       (- pi (atan (/ -1.0 a)))
       (+ (/ pi 2.0) (atan (- a))))))

; This is a Mantissa and Exponent function. 
(defun manexp (u)
  (multiple-value-bind (f e s) 
    (decode-float u)
    (cons (* s f) e)))

;; Format a DoubleFloat value in a reasonable way.  Similar code
;; has been submitted for inclusion in SBCL.  If and when
;; that version is integrated, we should remove it from here.
#- :sbcl
(defun dfloat-format-general (number)
  (format nil "~G" number))
#+ :sbcl
(defun dfloat-format-general (number)
  (declare (type double-float number))
  (cond
   ((zerop number) "0.")
   (t
    (with-output-to-string (stream)
      (if (or (sb-ext:float-infinity-p number)
	      (sb-ext:float-nan-p number))
	  (prin1 number stream)
	(flet ((dfloat-format-fixed (stream number d)
		 (declare (type double-float number))
		 (multiple-value-bind (str len lpoint tpoint)
		   (sb-impl::flonum-to-string number nil d)
		   (declare (ignore len))
		   ;;if caller specifically requested no fraction digits,
		   ;;suppress the optional trailing zero
		   (when (and d (zerop d))
		     (setq tpoint nil))
		   (when lpoint
		     (write-char #\0 stream))
		   (write-string str stream)
		   nil))
	       (dfloat-format-exp (stream number)
		 (declare (type double-float number))
		 (multiple-value-bind (num expt)
		   (sb-impl::scale-exponent number)
		   (let* ((expt (1- expt))
			  (estr (sb-format::decimal-string (abs expt))))
		     (multiple-value-bind (fstr flen lpoint tpoint)
		       (sb-impl::flonum-to-string num nil nil 1)
		       (declare (ignore tpoint))
		       (when lpoint (write-char #\0 stream))
		       (write-string fstr stream)
		       (when (char= (aref fstr (1- flen)) #\.)
			 (write-char #\0 stream))
		       (write-char #\E stream)
		       (write-char (if (minusp expt) #\- #\+) stream)
		       (write-string estr stream))
		     nil))))
	      (when (minusp number)
		(setq number (- number))
		(write-char #\- stream))
	      (multiple-value-bind (ignore n) (sb-impl::scale-exponent number)
	      (declare (ignore ignore))
	      (let* ((q (length
			 (nth-value 1 (sb-impl::flonum-to-digits number))))
		     (d (max q (min n 7)))
		     (dd (- d n)))
		(if (<= 0 dd d)
		    (dfloat-format-fixed stream number dd)
		  (dfloat-format-exp stream number))))))))))

