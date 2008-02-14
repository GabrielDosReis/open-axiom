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

(export '(Comp FluidVars LocVars OptionList SLAM SPADSLAM ILAM FLUID))

(defparameter |$compileDontDefineFunctions| 'T)

;;; Common Block section

(defparameter FluidVars nil)
(defparameter LocVars nil)
; (defparameter OptionList nil) defined in nlib.lisp
(defparameter SpecialVars nil)

(defvar $closedfns nil)

(defun |compAndDefine| (L)
  (let ((*comp370-apply* (function print-and-eval-defun)))
    (declare (special *comp370-apply*))
    (COMP L)))

(defun COMP (L) (MAPCAR #'COMP-2 (MAPCAN #'COMP-1 L)))

;;(defun |compQuietly| (L)
;;  (let (U CUROUTSTREAM)
;;    (declare (special CUROUTSTREAM))
;;    (ADDOPTIONS 'LISTING NULLOUTSTREAM)                     
;;    (SETQ CUROUTSTREAM NULLOUTSTREAM)                       
;;    (setq U (COMP L))
;;    (setq OPTIONLIST (CDDR OPTIONLIST))
;;    U))

(defun |compQuietly| (fn)
  (let ((*comp370-apply*
         (if |$InteractiveMode|
             (if |$compileDontDefineFunctions| #'compile-defun #'eval-defun)
           #'print-defun))
     ;; following creates a null outputstream if $InteractiveMode
        (*standard-output*
         (if |$InteractiveMode| (make-broadcast-stream)
           *standard-output*)))
    (COMP fn)))

#-:CCL
(defun |compileFileQuietly| (fn) 
  (let (
     ;; following creates a null outputstream if $InteractiveMode
        (*standard-output*
         (if |$InteractiveMode| (make-broadcast-stream)
           *standard-output*)))
    (COMPILE-FILE fn)))

#+:CCL
(defun |compileFileQuietly| (fn)
  (let (
     ;; following creates a null outputstream if $InteractiveMode
     (*standard-output*
       (if |$InteractiveMode| (make-broadcast-stream) *standard-output*)))
     ;; The output-library is not opened before use unless set explicitly
     (if (null output-library)
         (|openOutputLibrary| 
           (setq |$outputLibraryName|
            (if (null |$outputLibraryName|)
                (make-pathname :directory (get-current-directory)
                               :name "user.lib")
                (if (filep |$outputLibraryName|) (truename |$outputLibraryName|)
                                                 |$outputLibraryName|)))))
     (compile-lib-file fn)))

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



;;(defun |compileQuietly| (L) (PROG (U CUROUTSTREAM)
;;  ;; calls lisp system COMPILE or DEFINE                  
;;  (ADDOPTIONS 'QUIET 'T)                                  
;;  (ADDOPTIONS 'LISTING NULLOUTSTREAM)                     
;;  (SETQ CUROUTSTREAM NULLOUTSTREAM)                       
;;  (SETQ U (COND                                           
;;    (|$compileDontDefineFunctions| (COMPILE L))           
;;    ('T (DEFINE L))))                                     
;;  (SETQ OPTIONLIST (CDDR OPTIONLIST))                     
;;  (RETURN U)   ))                                       

(defun |compileQuietly| (fn)
  (let ((*comp370-apply*
         (if |$InteractiveMode|
             (if |$compileDontDefineFunctions| #'compile-defun #'eval-defun)
           #'print-defun))
     ;; following creates a null outputstream if $InteractiveMode
        (*standard-output*
         (if |$InteractiveMode| (make-broadcast-stream)
           *standard-output*)))
    (COMP370 fn)))

(defun COMP-1 (X)
  (let* ((FNAME (car X))
         ($FUNNAME FNAME)
         ($FUNNAME_TAIL (LIST FNAME))
         (LAMEX (second X))
         ($closedfns nil))
    (declare (special $FUNNAME $FUNNAME_TAIL $CLOSEDFNS))
    (setq LAMEX (COMP-TRAN LAMEX))
    (COMP-NEWNAM LAMEX)
    (if (fboundp FNAME)
        (format t "~&~%;;;     ***       ~S REDEFINED~%" FNAME))
    (CONS (LIST FNAME LAMEX) $CLOSEDFNS)))

(defun Comp-2 (args &aux name type argl bodyl junk)
    (dsetq (NAME (TYPE ARGL . BODYL) . JUNK) args)
    (cond (JUNK (MOAN (format nil "******pren error in (~S (~S ...) ...)" NAME TYPE)))
          ((eq TYPE 'SLAM) (COMP-SLAM NAME ARGL BODYL))
          ((LASSQ NAME |$clamList|) (|compClam| NAME ARGL BODYL |$clamList|))
          ((eq TYPE 'SPADSLAM) (COMP-SPADSLAM NAME ARGL BODYL))
          ((eq TYPE 'ILAM) (COMP-ILAM NAME ARGL BODYL))
          ((setq BODYL (LIST NAME (CONS TYPE (CONS ARGL BODYL))))
           (if |$PrettyPrint| (pprint bodyl))
           (if (null $COMPILE) (SAY "No Compilation")
               (COMP370 (LIST BODYL)))
           NAME)))

;; used to be called POSN - but that interfered with a CCL function
(DEFUN POSN1 (X L) (position x l :test #'equal))

(DEFUN COMP-ILAM (NAME ARGL BODYL)
  (let* ((FARGL (NLIST (LENGTH ARGL) '(GENSYM)))
         (BODYLP (SUBLISLIS FARGL ARGL BODYL)))
        (MAKEPROP NAME 'ILAM T)
        (SET NAME (CONS 'LAMBDA (CONS FARGL BODYLP)))
        NAME))

(DEFUN COMP-SPADSLAM (NAME ARGL BODYL)
  (let* ((AL (INTERNL NAME ";AL"))
         (AUXFN (INTERNL NAME ";"))
         (G1 (GENSYM))
         (G2 (GENSYM))
         (U (COND ((NOT ARGL) (LIST NIL NIL (LIST AUXFN)))
                  ((NOT (CDR ARGL))
                   (LIST (LIST G1) (LIST '|devaluate| G1) (LIST AUXFN G1)))
                  ((LIST G1
                         (LIST '|devaluateList| G1)
                         (LIST 'APPLY (LIST 'FUNCTION AUXFN) G1)))))
         (ARG (first U))
         (ARGTRAN (second U))
         (APP (third U))
         (LAMEX  `(lam ,ARG
                       (let (,g2)
                         (cond ,(COND (ARGL `((setq ,g2 (|assoc| ,argtran ,al))
                                              (cdr ,g2)))
                                      ((LIST AL)))
                               ,(COND (ARGL
                                       `(t(setq ,al(|cons5|(cons ,argtran
                                                                 (setq ,g2 ,app))
                                                           ,al))
                                          ,g2))
                                      (`(t (setq ,al ,app)))))))))
    (setandfile AL NIL)
    (setq U (LIST NAME LAMEX))
    (if |$PrettyPrint| (PRETTYPRINT U))
    (COMP370 (LIST U))
    (setq U (LIST AUXFN (CONS 'LAMBDA (CONS ARGL BODYL))))
    (COND (|$PrettyPrint| (PRETTYPRINT U)))
    (COMP370 (LIST U))
    NAME))

(DEFUN COMP-SLAM (NAME ARGL BODYL)
  (let* ((AL (INTERNL NAME ";AL"))
         (AUXFN (INTERNL NAME ";"))
         (G1 (GENSYM))
         (G2 (GENSYM))
         (U (COND ((NOT ARGL) `(nil (,auxfn)))
                  ((NOT (CDR ARGL)) `((,g1)(,auxfn ,g1)))
                  (`(,g1 (applx (function ,auxfn) ,g1)))))
         (ARG (CAR U))
         (APP (CADR U))
         (LAMEX
           (LIST 'LAM ARG
                 (LIST 'PROG (LIST G2)
                       (LIST 'RETURN
                             (LIST 'COND
                                   (COND (ARGL
                                          `((setq ,G2 (|assoc| ,G1 ,AL))
                                            (CDR ,G2)))
                                         ((LIST AL)))
                                   (COND (ARGL (LIST ''T `(setq ,G2 ,APP)
                                                     (LIST 'SETQ AL
                                                           `(CONS
                                                              (CONS ,G1 ,G2) ,AL))
                                                     G2))
                                         ((LIST ''T `(setq ,AL ,APP))))))))))
    (set AL NIL)
    (setq U (LIST NAME LAMEX))
    (if |$PrettyPrint| (PRETTYPRINT U))
    (COMP370 (LIST U))
    (setq U (LIST AUXFN (CONS 'LAMBDA (CONS ARGL BODYL))))
    (if |$PrettyPrint| (PRETTYPRINT U))
    (COMP370 (LIST U))
    NAME))

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
  (let ((X (COMP-EXPAND X)) FluidVars LocVars SpecialVars)
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
    (let ((fluids (S+ (comp-fluidize (second x)) SpecialVars)))
      (if fluids
          `(,(first x) ,(second x) (declare (special . ,fluids)) . ,(cddr x))
          `(,(first x) ,(second x) . ,(cddr x))))))

; Fluidize: Returns a list of fluid variables in X

(DEFUN COMP-FLUIDIZE (X)
  (COND ((AND (symbolp X)
              (NE X '$)
              (NE X '$$)
              (char= #\$ (ELT (PNAME X) 0))
              (NOT (DIGITP (ELT (PNAME X) 1))))
         x)
        ((atom x) nil)
        ((eq (first X) 'FLUID) (second X))
        ((let ((a (comp-fluidize (first x)))
               (b (comp-fluidize (rest x))))
           (if a (cons a b) b)))))

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

; NOTE: It is potentially dangerous to assume every occurrence of element of
; $COMP-MACROLIST is actually a macro call

(defparameter $COMP-MACROLIST
  '(COLLECT REPEAT SUCHTHATCLAUSE THETA COLLECTV COLLECTVEC
            THETA1 SPADREDUCE SPADDO)
  "???")

(DEFUN COMP-EXPAND (X)
  (COND ((atom x) x)
        ((eq (CAR X) 'QUOTE) X)
        ((memq (CAR X) $COMP-MACROLIST)
         (comp-expand (macroexpand-1 x)))
        ((let ((a (comp-expand (car x)))
               (b (comp-expand (cdr x))))
           (if (AND (eq A (CAR X)) (eq B (CDR X)))
               x
               (CONS A B))))))

(DEFUN COMP-TRAN-1 (X)
  (let (u)
    (cond ((ATOM X) NIL)
          ((eq (setq U (CAR X)) 'QUOTE) NIL)
          ((AND (eq U 'MAKEPROP) $TRACELETFLAG (RPLAC (CAR X) 'MAKEPROP-SAY) NIL)
           NIL)
           ; temporarily make TRACELET cause MAKEPROPs to be reported
          ((MEMQ U '(DCQ RELET PRELET SPADLET SETQ LET) )
           (COND ((NOT (eq U 'DCQ))
                  (COND ((OR (AND (eq $NEWSPAD T) (NOT $BOOT))
                             (MEMQ $FUNNAME |$traceletFunctions|))
                         (NCONC X $FUNNAME_TAIL)
                         (RPLACA X 'LETT))
                        ; this devious trick (due to RDJ) is needed since the compile
                        ; looks only at global variables in top-level environment;
                        ; thus SPADLET cannot itself test for such flags (7/83).
                        ($TRACELETFLAG (RPLACA X '/TRACE-LET))
                        ((eq U 'LET) (RPLACA X 'SPADLET)))))
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
