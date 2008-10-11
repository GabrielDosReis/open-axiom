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

(defparameter FluidVars nil)
(defparameter LocVars nil)
; (defparameter OptionList nil) defined in nlib.lisp
(defparameter SpecialVars nil)

(defvar $closedfns nil)

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

(DEFUN COMP-NEWNAM (X)
  (let (y u)
    (cond ((ATOM X) NIL)
          ((ATOM (setq Y (CAR X)))
          ;; (AND (IDENTP Y) (setq U (GET Y 'NEWNAM)) (RPLACA X U))
           (AND (NOT (eq Y 'QUOTE)) (COMP-NEWNAM (CDR X)))
           (WHEN (and (EQ Y 'CLOSEDFN) (boundp '$closedfns))
                 (SETQ U (MAKE-CLOSEDFN-NAME))
                 (PUSH (list U (CADR X)) $closedfns)
                 (rplaca x 'FUNCTION)
                 (rplaca (cdr x) u)))
          (t (COMP-NEWNAM (CAR X)) (COMP-NEWNAM (CDR X))))))

(defun make-closedfn-name ()
  (internl $FUNNAME "!" (STRINGIMAGE (LENGTH $CLOSEDFNS))))

(DEFUN COMP-TRAN (X)
  "SEXPR<FN. BODY> -> SEXPR"
  (let ((X (|middleEndExpand| X)) FluidVars LocVars SpecialVars)
    (COMP-TRAN-1 (CDDR X))
    (setq X (list (first x) (second x)
                  (if (and (null (cdddr x))
                           (or (atom (third x))
                               (eq (car (third x)) 'SEQ)
                               (not (contained 'EXIT (third x)))))
                      (caddr x)
                      (cons 'SEQ (cddr x))))) ;catch naked EXITs
    (let* ((FluidVars (REMDUP (NREVERSE FLUIDVARS)))
           (LOCVARS (S- (S- (REMDUP (NREVERSE LOCVARS)) FLUIDVARS)
                        (LISTOFATOMS (CADR X))))
           (LVARS (append fluidvars LOCVARS)))
      (let ((fluids (S+ fluidvars SpecialVars)))
        (setq x
              (if fluids
                  `(,(first x) ,(second x)
                    (prog ,lvars (declare (special . ,fluids))
                      (return ,(third x))))
                  (list (first x) (second x)
                     (if (or lvars (contained 'RETURN (third x)))
                         `(prog ,lvars (return ,(third x)))
                         (third x)) )))))
    (let ((fluids (S+ (|backendFluidize| (second x)) SpecialVars)))
      (if fluids
          `(,(first x) ,(second x) (declare (special . ,fluids)) . ,(cddr x))
          `(,(first x) ,(second x) . ,(cddr x))))))

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

(DEFUN COMP-TRAN-1 (X)
  (let (u)
    (cond ((ATOM X) NIL)
          ((eq (setq U (CAR X)) 'QUOTE) NIL)
          ((AND (eq U 'MAKEPROP) $TRACELETFLAG (RPLAC (CAR X) 'MAKEPROP-SAY) NIL)
           NIL)
           ; temporarily make TRACELET cause MAKEPROPs to be reported
          ((MEMQ U '(DCQ RELET PRELET SPADLET SETQ %LET) )
           (COND ((NOT (eq U 'DCQ))
                  (COND ((OR (AND (eq $NEWSPAD T))
                             (MEMQ $FUNNAME |$traceletFunctions|))
                         (NCONC X $FUNNAME_TAIL)
                         (RPLACA X 'LETT))
                        ; this devious trick (due to RDJ) is needed since the compile
                        ; looks only at global variables in top-level environment;
                        ; thus SPADLET cannot itself test for such flags (7/83).
                        ($TRACELETFLAG (RPLACA X '/TRACE-LET))
                        ((eq U '%LET) (RPLACA X 'SPADLET)))))
           (COMP-TRAN-1 (CDDR X))
           (AND (NOT (MEMQ U '(setq RELET)))
                (COND ((IDENTP (CADR X)) (PUSHLOCVAR (CADR X)))
                      ((EQCAR (CADR X) 'FLUID)
                       (PUSH (CADADR X) FLUIDVARS)
                       (RPLAC (CADR X) (CADADR X)))
                      ((mapc #'pushlocvar (listofatoms (cadr x))) nil))))
          ((and (symbolp u) (GET U 'ILAM))
           (RPLACA X (EVAL U)) (COMP-TRAN-1 X))
          ((MEMQ U '(PROG LAMBDA))
           (PROG (NEWBINDINGS RES)
                 (setq NEWBINDINGS NIL)
                 (mapcar #'(lambda (Y)
                             (COND ((NOT (MEMQ Y LOCVARS))
                                    (setq LOCVARS (CONS Y LOCVARS))
                                    (setq NEWBINDINGS (CONS Y NEWBINDINGS)))))
                         (second x))
                 (setq RES (COMP-TRAN-1 (CDDR X)))
                 (setq locvars (remove-if #'(lambda (y) (memq y newbindings))
                                          locvars))
                 (RETURN (CONS U (CONS (CADR X) RES)) )) )
          ((PROGN (COMP-TRAN-1 U) (COMP-TRAN-1 (CDR X)))))))

(DEFUN PUSHLOCVAR (X)
  (let (p)
    (cond ((AND (NE X '$)
                (char= #\$ (ELT (setq P (PNAME X)) 0))
                (NOT (char= #\, (ELT P 1)))
                (NOT (DIGITP (ELT P 1)))) NIL)
          ((PUSH X LOCVARS)))))

(defmacro PRELET (L) `(spadlet . ,L))
(defmacro RELET (L) `(spadlet . ,L))
(defmacro PRESET (L) `(spadlet . ,L))
(defmacro RESET (L) `(spadlet . ,L))
