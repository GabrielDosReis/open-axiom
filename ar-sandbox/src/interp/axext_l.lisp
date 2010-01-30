;; Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2008, Gabriel Dos Reis.
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


;; File containing primitives needed by exextend in order to interop with axiom
;; This file could do with some declares

(import-module "foam_l")
(in-package "FOAM-USER")

;; Literals should be null-terminated strings

;; SingleInteger

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn

(defmacro |AXL-LiteralToSingleInteger| (l)
  `(parse-integer ,l :junk-allowed t))

(defmacro |AXL-LiteralToInteger| (l)
  `(parse-integer ,l :junk-allowed t))

(defmacro |AXL-LiteralToDoubleFloat| (l)
  `(read-from-string ,l nil (|DFlo0|)
                     :preserve-whitespace t))

(defmacro |AXL-LiteralToString| (l)
  `(subseq ,l 0 (- (length ,l) 1)))

(defmacro |AXL-SingleIntegerToInteger| (si)
  `(coerce (the |SInt| ,si) |BInt|))

(defmacro |AXL-StringToFloat| (s)
  `(boot::|string2Float| ,s))

(defmacro |AXL-IntegerIsNonNegative| (i)
  `(not (< ,i 0)))

(defmacro |AXL-IntegerIsPositive| (i)
  `(< 0 (the |BInt| ,i)))

(defmacro |AXL-plusInteger| (a b)
  `(the |BInt| (+ (the |BInt| ,a)
                  (the |BInt| ,b))))

(defmacro |AXL-minusInteger| (a b)
  `(the |BInt| (- (the |BInt| ,a)
                  (the |BInt| ,b))))

(defmacro |AXL-timesInteger| (a b)
  `(the |BInt| (* (the |BInt| ,a)
                  (the |BInt| ,b))))

(defmacro |AXL-eqInteger| (a b)
  `(= (the |BInt| ,a)
      (the |BInt| ,b)))

(defmacro |AXL-ltInteger| (a b)
  `(< (the |BInt| ,a)
      (the |BInt| ,b)))

(defmacro |AXL-leInteger| (a b)
  `(<= (the |BInt| ,a)
       (the |BInt| ,b)))

(defmacro |AXL-gtInteger| (a b)
  `(> (the |BInt| ,a)
      (the |BInt| ,b)))

(defmacro |AXL-geInteger| (a b)
  `(>= (the |BInt| ,a)
       (the |BInt| ,b)))

(defmacro |AXL-plusSingleInteger| (a b)
  `(the |SInt| (+ (the |SInt| ,a)
                  (the |SInt| ,b))))

(defmacro |AXL-minusSingleInteger| (a b)
  `(the |SInt| (- (the |SInt| ,a)
                  (the |SInt| ,b))))

(defmacro |AXL-timesSingleInteger| (a b)
  `(the |SInt| (* (the |SInt| ,a)
                  (the |SInt| ,b))))

(defmacro |AXL-eqSingleInteger| (a b)
  `(= (the |SInt| ,a)
      (the |SInt| ,b)))

(defmacro |AXL-ltSingleInteger| (a b)
  `(< (the |SInt| ,a)
      (the |SInt| ,b)))

(defmacro |AXL-leSingleInteger| (a b)
  `(<= (the |SInt| ,a)
       (the |SInt| ,b)))

(defmacro |AXL-gtSingleInteger| (a b)
  `(> (the |SInt| ,a)
      (the |SInt| ,b)))

(defmacro |AXL-geSingleInteger| (a b)
  `(>= (the |SInt| ,a)
       (the |SInt| ,b)))

(defmacro |AXL-incSingleInteger| (i)
  `(the |SInt| (+ (the |SInt| ,i) 1)))

(defmacro |AXL-decSingleInteger| (i)
  `(- (the |SInt| ,i)
     (the |SInt| 1)))

(defmacro |AXL-onefnSingleInteger|  () '(the |SInt| 1))
(defmacro |AXL-zerofnSingleInteger| () '(the |SInt| 0))

(defmacro |AXL-cons| (x y)
  `(cons ,x ,y))

(defmacro |AXL-nilfn| () nil)

(defmacro |AXL-car| (x) `(car ,x))

(defmacro |AXL-cdr| (x) `(cdr ,x))

(defmacro |AXL-null?| (x) `(null ,x))

(defmacro |AXL-rplaca| (x y) `(rplaca ,x ,y))

(defmacro |AXL-rplacd| (x y) `(rplacd ,x ,y))

(defmacro |AXL-error| (msg) `(error ,msg))
  
;; arrays
;; 0 based!
(defmacro |AXL-arrayRef| (arr i)
  `(|AElt| ,arr ,i))

(defmacro |AXL-arraySet| (arr i v)
  `(setf (|AElt| ,arr ,i) ,v))

(defmacro |AXL-arrayToList| (x)
  `(coerce ,x 'list))

(defmacro |AXL-arraySize| (x)
  `(length ,x))

(defmacro |AXL-arrayNew| (n)
  `(make-array ,n))

(defmacro |AXL-arrayCopy| (x)
  `(copy-seq ,x))

;; Vectors

;; tacky but means we can run programs

(defun H-integer (l e)
  (|AXL-LiteralToInteger| l))
        
(defun  H-string (l e)
  (|AXL-LiteralToString| l))

(defun  H-error (l e)
  (|AXL-error| l))

))

(eval-when (load eval)
           (defconstant |G-axclique_string_305639517| (cons #'H-String nil))
           (defconstant |G-axclique_integer_685864888| (cons #'H-integer nil))
           (defconstant |G-axclique_error_011667951| (cons #'H-error nil)))

;; Testing

(defun |AXL-spitSInt| (x)
  (print x))

