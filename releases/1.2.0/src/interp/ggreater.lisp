;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007, Gabriel Dos Reis.
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


(IMPORT-MODULE "vmlisp")
(in-package "BOOT")

(DEFUN LEXGREATERP (COMPERAND-1 COMPERAND-2)
    ;;  "Order of types: pair NIL vec ivec/rvec cvec ident num fbpi mbpi other"
    (COND
      ((EQ COMPERAND-1 COMPERAND-2) NIL)
      ((consp COMPERAND-1)
        (COND
          ( (consp COMPERAND-2)
            (COND
              ( (EQUAL (qcar COMPERAND-1) (qcar COMPERAND-2))
                (LEXGREATERP (qcdr COMPERAND-1) (qcdr COMPERAND-2)) )
              ( (LEXGREATERP (qcar COMPERAND-1) (qcar COMPERAND-2)) ) ) )
          ('else t)))
      ((consp COMPERAND-2) NIL)
      ((NULL COMPERAND-1) 'T )
      ((NULL COMPERAND-2) NIL)
      ((VECP COMPERAND-1)
        (COND
          ((VECP COMPERAND-2) (LEXVGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((VECP COMPERAND-2) NIL)
      ((OR (IVECP COMPERAND-1) (RVECP COMPERAND-1))
        (COND
          ( (OR (IVECP COMPERAND-2) (RVECP COMPERAND-2))
            (LEXVGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((OR (IVECP COMPERAND-2) (RVECP COMPERAND-2)) NIL )
      ((stringp COMPERAND-1)
        (COND
          ((stringp COMPERAND-2)
            (STRING-GREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((stringp COMPERAND-2) NIL)
      ((symbolp COMPERAND-1)
        (COND
          ((symbolp COMPERAND-2)
            (STRING-GREATERP (symbol-name COMPERAND-1) (symbol-name COMPERAND-2)) )
          ('else t)))
      ((symbolp COMPERAND-2) NIL )
      ((numberp COMPERAND-1)
        (COND
          ( (numberp COMPERAND-2)
            (> COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((numberp COMPERAND-2) NIL)
      ((CHARACTERP COMPERAND-1)
        (COND 
          ((CHARACTERP COMPERAND-2)
            (CHAR-GREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((CHARACTERP COMPERAND-2) NIL )
      ((FBPIP COMPERAND-1)
        (COND
          ((FBPIP COMPERAND-2)
            (LEXGREATERP (BPINAME COMPERAND-1) (BPINAME COMPERAND-2)) )
          ('else t)))
      ((FBPIP COMPERAND-2) NIL)
      ((MBPIP COMPERAND-1)
        (COND
          ((MBPIP COMPERAND-2)
            (LEXGREATERP (BPINAME COMPERAND-1) (BPINAME COMPERAND-2)) )
          ('else t)))
      ((MBPIP COMPERAND-2)
        NIL )
      ((> (SXHASH COMPERAND-1) (SXHASH COMPERAND-2)))))

(DEFUN LEXVGREATERP (VECTOR-COMPERAND-1 VECTOR-COMPERAND-2)
  (declare (simple-vector vector-comperand-1 vector-comperand-2))
  (let ((I -1)
        (L1 (length VECTOR-COMPERAND-1))
        (L2 (length VECTOR-COMPERAND-2)))
    (declare (fixnum I L1 L2) )
    (PROG (T1 T2)
          LP 
          (setq I (1+ I))
          (COND ((EQL L1 I) 
                 (RETURN NIL))
                ((EQL L2 I)
                 (RETURN 'T)))
          (COND ((EQUAL (SETQ T1 (svref VECTOR-COMPERAND-1 I))
                        (SETQ T2 (svref VECTOR-COMPERAND-2 I)))
                 (GO LP)))
          (RETURN (LEXGREATERP T1 T2)) ) ))


(DEFUN GGREATERP (COMPERAND-1 COMPERAND-2)
    ;;  "Order of types: pair NIL vec ivec/rvec cvec ident num fbpi mbpi other"
    (COND
      ((EQ COMPERAND-1 COMPERAND-2) NIL)
      ((symbolp COMPERAND-1)
        (COND
          ((symbolp COMPERAND-2)
            (CGREATERP (symbol-name COMPERAND-1) (symbol-name COMPERAND-2)) )
          ('else t)))
      ((symbolp COMPERAND-2) NIL )
      ((consp COMPERAND-1)
        (COND
          ( (consp COMPERAND-2)
            (COND
              ( (EQUAL (qcar COMPERAND-1) (qcar COMPERAND-2))
                (GGREATERP (qcdr COMPERAND-1) (qcdr COMPERAND-2)) )
              ( (GGREATERP (qcar COMPERAND-1) (qcar COMPERAND-2)) ) ) )
          ('else t)))
      ((consp COMPERAND-2) NIL)
      ((NULL COMPERAND-1) 'T )
      ((NULL COMPERAND-2) NIL)
      ((VECP COMPERAND-1)
        (COND
          ((VECP COMPERAND-2) (VGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((VECP COMPERAND-2) NIL)
      ((OR (IVECP COMPERAND-1) (RVECP COMPERAND-1))
        (COND
          ( (OR (IVECP COMPERAND-2) (RVECP COMPERAND-2))
            (VGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((OR (IVECP COMPERAND-2) (RVECP COMPERAND-2)) NIL )
      ((stringp COMPERAND-1)
        (COND
          ((stringp COMPERAND-2)
            (CGREATERP COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((stringp COMPERAND-2) NIL)
      ((numberp COMPERAND-1)
        (COND
          ( (numberp COMPERAND-2)
            (> COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((numberp COMPERAND-2) NIL)
      ((CHARACTERP COMPERAND-1)
        (COND 
          ((CHARACTERP COMPERAND-2)
            (CHAR> COMPERAND-1 COMPERAND-2) )
          ('else t)))
      ((CHARACTERP COMPERAND-2) NIL )
      ((FBPIP COMPERAND-1)
        (COND
          ((FBPIP COMPERAND-2)
            (GGREATERP (BPINAME COMPERAND-1) (BPINAME COMPERAND-2)) )
          ('else t)))
      ((FBPIP COMPERAND-2) NIL)
      ((MBPIP COMPERAND-1)
        (COND
          ((MBPIP COMPERAND-2)
            (GGREATERP (BPINAME COMPERAND-1) (BPINAME COMPERAND-2)) )
          ('else t)))
      ((MBPIP COMPERAND-2)
        NIL )
      ((> (SXHASH COMPERAND-1) (SXHASH COMPERAND-2)))))

(DEFUN VGREATERP (VECTOR-COMPERAND-1 VECTOR-COMPERAND-2)
  (declare (simple-vector vector-comperand-1 vector-comperand-2))
  (let ((I -1)
        (L1 (length VECTOR-COMPERAND-1))
        (L2 (length VECTOR-COMPERAND-2)))
    (declare (fixnum I L1 L2))
    (PROG (T1 T2)
          LP 
          (setq I (1+ I))
          (COND ((EQL L1 I) 
                 (RETURN NIL))
                ((EQL L2 I) 
                 (RETURN 'T)))
          (COND ((EQUAL (SETQ T1 (svref VECTOR-COMPERAND-1 I))
                        (SETQ T2 (svref VECTOR-COMPERAND-2 I)))
                 (GO LP)))
          (RETURN (GGREATERP T1 T2)) ) ))

(defvar SORTGREATERP #'GGREATERP "default sorting predicate")



