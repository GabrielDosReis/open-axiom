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
;; 
;; Copyright (C) 2007 Gabriel Dos Reis


;; This file contains the primary definition of the BOOT package

;; This is a horrible hack to work around a horrible bug in GCL
;; as reported here:
;;    http://lists.gnu.org/archive/html/gcl-devel/2007-08/msg00004.html
;; 
#+(and :gcl (not :common-lisp)) (in-package "VMLISP")
#+(and :gcl (not :common-lisp)) (in-package "AxiomCore")

(import-module "vmlisp")
(defpackage "BOOT"
  #+:common-lisp (:use "COMMON-LISP")
  #-:common-lisp (:use "LISP")
  (:use "VMLISP" "AxiomCore"))

(in-package "BOOT")

;; Below are some missing functions.  There here for lack of better
;; place (sys-funs.lisp?)
;;
;; These functions should be defined for DoubleFloat inputs but are not.
;; These are cheap and easy definitions that work but should be rewritten.

;; Contributed by Juergen Weiss from a suggestion by Arthur Norman.

(defun sec (x) (/ 1 (cos x)))
(defun csc (x) (/ 1 (sin x)))
(defun acsc (x) (asin (/ 1 x)))
(defun asec (x) (acos (/ 1 x)))
(defun csch (x) (/ 1 (sinh x)))
(defun coth (x) (* (cosh x) (csch x)))
(defun sech (x) (/ 1 (cosh x)))
(defun acsch (x) (asinh (/ 1 x)))
(defun acoth (x) (atanh (/ 1 x)))
(defun asech (x) (acosh (/ 1 x)))

#+(or :cmu :akcl :gcl)
(defun cot (a)
  (if (or (> a 1000.0) (< a -1000.0))
    (/ (cos a) (sin a))
    (/ 1.0 (tan a))))

#+(or :cmu :akcl :gcl)
(defun acot (a)
  (if (> a 0.0)
    (if (> a 1.0)
       (atan (/ 1.0 a))
       (- (/ pi 2.0) (atan a)))
    (if (< a -1.0)
       (- pi (atan (/ -1.0 a)))
       (+ (/ pi 2.0) (atan (- a))))))

; This is a Mantissa and Exponent function. 
#+(or :cmu :akcl :gcl)
(defun manexp (u)
  (multiple-value-bind (f e s) 
    (decode-float u)
    (cons (* s f) e)))


