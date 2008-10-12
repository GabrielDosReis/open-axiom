;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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


; NAME:    Compiler Utilities Package

; PURPOSE: Comp is a modified version of Compile which is a preprocessor for
;          calls to Lisp Compile.  It searches for variable assignments that use
;          (SPADLET a b). It allows you to create local variables without
;          declaring them local by moving them into a PROG variable list.
;          This is not an ordinary SPADLET.  It looks and is used like a SETQ.
;          This preprocessor then collects the uses and creates the PROG.
;
;          SPADLET is defined in Macro.Lisp.
;
;          Comp recognizes as new lambda types the forms ILAM, SPADSLAM, SLAM,
;          and entries on $clamList.  These cache results.  ("Saving LAMbda".)
;          If the function is called with EQUAL arguments, returns the previous
;          result computed.
;
;          The package also causes traced things which are recompiled to
;          become untraced.

(IMPORT-MODULE "macros")
(in-package "BOOT")

(defparameter |$compileDontDefineFunctions| 'T)

;;; Common Block section

;; The following are used mainly in setvars.boot
(defun notEqualLibs (u v)
  (if (string= u (library-name v)) (seq (close-library v) t) nil))

(defun |dropInputLibrary| (lib) 
  ;; Close any existing copies of this library on the input path
 (setq input-libraries
  (delete lib input-libraries :test #'notEqualLibs )))

(defun |openOutputLibrary| (lib)
  (|dropInputLibrary| lib)
  (setq output-library (open-library lib 't))
  (setq input-libraries (cons output-library input-libraries)) )

(defun |addInputLibrary| (lib)
  (|dropInputLibrary| lib)
   (setq input-libraries (cons (open-library lib) input-libraries)) )

;; used to be called POSN - but that interfered with a CCL function
(DEFUN POSN1 (X L) (position x l :test #'equal))

; Fluidize: Returns a list of fluid variables in X

(DEFUN COMP\,FLUIDIZE  (X) (COND
  ((AND (IDENTP X)
        (NE X '$)
        (NE X '$$)
        (char= #\$ (ELT (PNAME X) 0)) (NULL (DIGITP (ELT (PNAME X) 1))))
    (LIST 'FLUID X))
  ((ATOM X) X)
  ((EQ (QCAR X) 'FLUID) X)
  ('T (PROG (A B)
      (SETQ A (COMP\,FLUIDIZE (QCAR X)))
      (SETQ B (COMP\,FLUIDIZE (QCDR X)))
      (COND ((AND (EQ A (QCAR X)) (EQ B (QCDR X)))
              (RETURN X))
            ('T (RETURN (CONS A B)) )) )    )))

(defmacro PRELET (L) `(spadlet . ,L))
(defmacro RELET (L) `(spadlet . ,L))
(defmacro PRESET (L) `(spadlet . ,L))
(defmacro RESET (L) `(spadlet . ,L))
