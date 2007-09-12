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


;;; The types "bit" and "bit vector" are implemented differently
;;; in different variants of lisp.
;;; These lisp macros/functions will have different implementations 
;;; on different lisp systems.

;;; The operations which traverse entire vectors are given as functions
;;; since the function calling overhead will be relatively small.
;;; The operations which extract or set a single part of the vector are
;;; provided as macros.

(IMPORT-MODULE "boot-pkg")
(in-package "BOOT")

;;; SMW Nov 88: Created
 
(defmacro truth-to-bit (x) `(cond (,x 1) ('else 0)))
(defmacro bit-to-truth (b) `(eq ,b 1))

(defun    bvec-make-full (n x) 
    (make-array (list n) :element-type 'bit :initial-element x))

(defmacro bvec-elt       (bv i)    `(sbit ,bv ,i))
(defmacro bvec-setelt    (bv i x)  `(setf (sbit ,bv ,i) ,x))
(defmacro bvec-size      (bv)      `(size ,bv))

(defun    bvec-copy      (bv)      (copy-seq bv))
(defun    bvec-concat    (bv1 bv2) (concatenate '(vector bit) bv1 bv2))
(defun    bvec-equal     (bv1 bv2) (equal    bv1 bv2))
(defun    bvec-greater   (bv1 bv2)
  (let ((pos (mismatch bv1 bv2)))
    (cond ((or (null pos) (>= pos (length bv1))) nil)
          ((< pos (length bv2)) (> (bit bv1 pos) (bit bv2 pos)))
          ((find 1 bv1 :start pos) t)
          (t nil))))
(defun    bvec-and       (bv1 bv2) (bit-and  bv1 bv2))
(defun    bvec-or        (bv1 bv2) (bit-ior  bv1 bv2))
(defun    bvec-xor       (bv1 bv2) (bit-xor  bv1 bv2))
(defun    bvec-nand      (bv1 bv2) (bit-nand bv1 bv2))
(defun    bvec-nor       (bv1 bv2) (bit-nor  bv1 bv2))
(defun    bvec-not       (bv)      (bit-not  bv))
