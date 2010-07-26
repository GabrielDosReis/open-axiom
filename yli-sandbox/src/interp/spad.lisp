;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2010, Gabriel Dos Reis.
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


; NAME:    Scratchpad Package
; PURPOSE: This is an initialization and system-building file for Scratchpad.

(IMPORT-MODULE "bootlex")
(import-module "postpar")
(import-module "debug")
(in-package "BOOT")

;;; Common  Block

(defconstant |$Newline| #\Newline)
(defvar |$preserveSystemLisplib| t "if nil finalizeLisplib does MA REP")
(defvar |$reportInstantiations| nil)
(defvar |$reportEachInstantiation| nil)
(defvar |$reportCounts| nil)
(defvar |$compForModeIfTrue| nil "checked in compSymbol")
(defvar |$functorForm| nil "checked in addModemap0")
(defvar |$Rep| '|$Rep| "should be bound to gensym? checked in coerce")
(defvar |$definition| nil "checked in DomainSubstitutionFunction")
(defvar |$getPutTrace| nil)
(defvar |$formulaFormat| nil "if true produce script formula output")
(defvar |$texFormat| nil "if true produce tex output")
(defvar |$fortranFormat| nil "if true produce fortran output")
(defvar |$algebraFormat| t "produce 2-d algebra output")
(defvar |$HiFiAccess| nil "if true maintain history file")

(defvar |boot-NewKEY| NIL)

(DEFVAR _ '&)
(defvar /EDIT-FM 'A1)
(defvar /EDIT-FT 'SPAD)
(defvar /RELEASE '"UNKNOWN")
(defvar /rp '/RP)
(defvar error-print)
(defvar ind)
(defvar INITCOLUMN 0)
(defvar JUNKTOKLIST '(FOR IN AS INTO OF TO))
(defvar m-chrbuffer)
(defvar m-chrindex)
(defvar MARG 0 "Margin for testing by ?OP")
(defvar NewFlag)
(defvar ParseMode)
(defvar RLGENSYMFG NIL)
(defvar RLGENSYMLST NIL)
(defvar S-SPADTOK 'SPADSYSTOK)
(defvar sortpred)
(defvar SPADSYSKEY '(EOI EOL))
(defvar STAKCOLUMN -1)
(defvar XTOKENREADER 'SPADTOK)
(defvar xtrans '|boot-new|)
(defvar |$IOAlist| '((|%i| . (|gauss| 0 1))))
(defvar |InteractiveMode|)
(defvar |NewFLAG| t)
(defvar |uc| 'UC)

(DEFUN INTEGER-BIT (N I) (LOGBITP I N))

(DEFUN /TRANSPAD (X)
  (PROG (proplist)
        (setq proplist (LIST '(FLUID . |true|)
                             (CONS '|special|
                                   (COPY-TREE |$InitialDomainsInScope|))))
        (SETQ |$InteractiveFrame|
              (|addBinding| '|$DomainsInScope| proplist
                          (|addBinding| '|$Information| NIL
                                      (COPY-TREE |$InitialModemapFrame|))))
        (RETURN (PROGN (S-PROCESS X) NIL))))

 ;; NIL needed below since END\_UNIT is not generated by current parser

(defun |traceComp| ()
  (SETQ |$compCount| 0)
  (EMBED '|comp|
     '(LAMBDA (X Y Z)
         (PROG (U)
               (SETQ |$compCount| (1+ |$compCount|))
               (SETQ |yesOrNo| (if (SETQ U (|comp| X Y Z))
                                   (if (EQUAL (SECOND U) Y) '|yes| (SECOND U))
                                 ('T '|no|)))
               (|sayBrightly| (CONS (MAKE-FULL-CVEC |$compCount| " ")
                                    (LIST X " --> " Y '|%b| |yesOrNo| '|%d|)))
               (SETQ |$compCount| (1- |$compCount|))
               (RETURN U)  )))
  (|comp| |$x| |$m| |$f|)
  (UNEMBED '|comp|))

(defun READ-SPAD (FN FM TO)
  (LET ((proplist
          (LIST '(FLUID . |true|)
                (CONS '|special| (COPY-TREE |$InitialDomainsInScope|)))))
    (SETQ |$InteractiveFrame|
          (|addBinding| '|$DomainsInScope| proplist
                      (|addBinding| '|$Information| NIL
                                  (|makeInitialModemapFrame|))))
    (READ-SPAD0 FN 'SPAD FM TO)))

(defun READ-INPUT (FN FM TO) (READ-SPAD0 FN 'INPUT FM TO))

(defun READ-SPAD0 (FN FT FM TO)
  (let (($newspad t)) (READ-SPAD1 FN FT FM TO)))

(defun READ-SPAD-1 () (|New,ENTRY,1|))

(defun UNCONS (X)
  (COND ((ATOM X) X)
        ((EQCAR X 'CONS) (CONS (SECOND X) (UNCONS (THIRD X))))
        (T (ERROR "UNCONS"))))

(defun OPTIMIZE\&PRINT (X) (PRETTYPRINT (/MDEF X)))

(defun SPAD-PRINTTIME (A B)
  (let (c msg)
    (setq C (+ A B))
    (setq MSG (STRCONC "(" (STRINGIMAGE A) " + " (STRINGIMAGE B)
                       " = " (STRINGIMAGE C) " MS.)"))
    (PRINT (STRCONC (STRINGPAD "" (DIFFERENCE 80 (SIZE MSG))) MSG))))

(defun SPAD-MODETRAN (X) (D-TRAN X))

(defun SPAD-EVAL (X)
  (COND ((ATOM X) (EVAL X))
        ((CONS (FIRST X) (MAPCAR #'SPAD-EVAL (CDR X))))))

;************************************************************************
;         SYSTEM COMMANDS
;************************************************************************

(defun CLEARDATABASE () (|runCommand| "ERASE MODEMAP DATABASE"))

(defun erase (FN FT)
  (|runCommand| (STRCONC "ERASE " (STRINGIMAGE FN) " " (STRINGIMAGE FT))))

(defun READLISP (UPPER_CASE_FG)
  (let (v expr val )
    (setq EXPR (READ-FROM-STRING
                  (IF UPPER_CASE_FG (string-upcase (line-buffer CURRENT-LINE))
                      (line-buffer CURRENT-LINE))
                  t nil :start (Line-CURRENT-INDEX CURRENT-LINE)))
    (VMPRINT EXPR)
    (setq VAL ((LAMBDA (|$InteractiveMode|)  (EVAL EXPR)) NIL))
    (FORMAT t "~&VALUE = ~S" VAL)
    (TERSYSCOMMAND)))

(defun TERSYSCOMMAND ()
  (FRESH-LINE)
  (SETQ CHR 'ENDOFLINECHR)
  (SETQ TOK 'END_UNIT)
  (|spadThrow|))

(defun /READ (L Q)
;  (SETQ /EDIT-FN (OR (KAR L) /EDIT-FN))
;  (SETQ /EDIT-FT (OR (KAR (KDR L)) 'INPUT))
;  (SETQ /EDIT-FM (OR (KAR (KDR (KDR L))) '*))
;  (SETQ /EDITFILE (LIST /EDIT-FN /EDIT-FT /EDIT-FM))
  (SETQ /EDITFILE L)
  (COND
    (Q  (/RQ))
    ('T (/RF)) )
  (FLAG |boot-NewKEY| 'KEY)
  (|terminateSystemCommand|))

(defun /EDIT (L)
  (SETQ /EDITFILE L)
  (/EF)
  (|terminateSystemCommand|))

(defun /COMPINTERP (L OPTS)
  (SETQ /EDITFILE (/MKINFILENAM L))
  (COND ((EQUAL OPTS "rf") (/RF))
        ((EQUAL OPTS "rq") (/RQ))
        ('T (/RQ-LIB)))
  (|terminateSystemCommand|))

(defun CPSAY (X) (let (n) (if (EQ 0 (setq N (|runCommand| X))) NIL (PRINT N))))

(defun |fin| ()
  (SETQ *EOF* 'T)
  (THROW 'SPAD_READER NIL))


(defun STRINGREST (X) (if (EQ (SIZE X) 1) (make-string 0) (SUBSTRING X 1 NIL)))

(defun STREAM2UC (STRM)
  (LET ((X (ELT (LASTATOM STRM) 1))) (SETF (ELT X 0) (LC2UC (ELT X 0)))))

(defun NEWNAMTRANS (X)
  (COND
    ((IDENTP X) (COND ( (GET X 'NEWNAM) (GET X 'NEWNAM)) ('T X)))
    ((STRINGP X) X)
    ((*VECP X) (MAPVWOC X (FUNCTION NEWNAMTRANS)))
    ((ATOM X) X)
    ((EQCAR X 'QUOTE))
    (T (CONS (NEWNAMTRANS (FIRST X)) (NEWNAMTRANS (CDR X))))))

(defun GP2COND (L)
  (COND ((NOT L) (ERROR "GP2COND"))
        ((NOT (CDR L))
         (COND ((EQCAR (FIRST L) 'COLON)
                (CONS (SECOND L) (LIST (LIST T 'FAIL))))
               (T (LIST (LIST T (FIRST L)))) ))
        ((EQCAR (FIRST L) 'COLON) (CONS (CDAR L) (GP2COND (CDR L))))
        (T (ERROR "GP2COND"))))

(FLAG JUNKTOKLIST 'KEY)

(defmacro |DomainSubstitutionMacro| (&rest L)
  (|DomainSubstitutionFunction| (first L) (second L)))

(defun |sort| (seq spadfn)
    (sort (copy-seq seq) (function (lambda (x y) (SPADCALL X Y SPADFN)))))

#-Lucid
(defun QUOTIENT2 (X Y) (values (TRUNCATE X Y)))

#+Lucid
(defun QUOTIENT2 (X Y) ; following to force error check in division by zero
  (values (if (zerop y) (truncate 1 Y) (TRUNCATE X Y))))

#-Lucid
(define-function 'REMAINDER2 #'REM)

#+Lucid
(defun REMAINDER2 (X Y)
  (if (zerop y) (REM 1 Y) (REM X Y)))

#-Lucid
(defun DIVIDE2 (X Y) (multiple-value-call #'cons (TRUNCATE X Y)))

#+Lucid
(defun DIVIDE2 (X Y)
  (if (zerop y) (truncate 1 Y)
    (multiple-value-call #'cons (TRUNCATE X Y))))

(define-function '|not| #'NOT)

(defun |random| () (random (expt 2 26)))
(defun \,plus (x y) (+ x y))
(defun \,times (x y) (* x y))
(defun \,difference (x y) (- x y))
(defun \,max (x y) (max x y))
(defun \,min (x y) (min x y))

(defun S-PROCESS (X)
  (let ((|$Index| 0)
        (*print-pretty* t)
        ($MACROASSOC ())
        ($NEWSPAD T)
        |$currentFunction|
        |$topOp|
        (|$semanticErrorStack| ())
        (|$warningStack| ())
        (|$returnMode| |$EmptyMode|)
        (|$leaveLevelStack| ())
        |$insideFunctorIfTrue| |$insideExpressionIfTrue|
        |$insideCoerceInteractiveHardIfTrue| |$insideWhereIfTrue|
        |$insideCategoryIfTrue| |$insideCapsuleFunctionIfTrue| |$form|
        (|$e| |$EmptyEnvironment|)
        (|$genSDVar| 0)
        (|$VariableCount| 0)
        (|$previousTime| (TEMPUS-FUGIT)))
    (|compileParseTree| X)))

(MAKEPROP 'END_UNIT 'KEY T)

(defun |process| (x)
  (COND ((NOT (EQ TOK 'END_UNIT))
         (SETQ DEBUGMODE 'NO)
         (SPAD_SYNTAX_ERROR)
         (if |$InteractiveMode| (|spadThrow|))
         (S-PROCESS x))))

(defun |New,ENTRY,1| ()
    (let (ZZ str N RLGENSYMFG RLGENSYMLST |NewFLAG| XCAPE
          SINGLELINEMODE OK ISID NBLNK COUNT CHR ULCASEFG ($LINESTACK 'BEGIN_UNIT)
          $TOKSTACK COMMENTCHR TOK LINE BACK INPUTSTREAM XTRANS
          XTOKENREADER STACK STACKX TRAPFLAG)
      (SETQ XTRANS '|boot-New|
            XTOKENREADER 'NewSYSTOK
            Meta_Error_Handler 'SPAD_SYNTAX_ERROR)
      (FLAG |boot-NewKEY| 'KEY)
      (PROMPT)
      (SETQ XCAPE '_)
      (SETQ COMMENTCHR 'IGNORE)
      (SETQ INITCOLUMN 0)
      (SETQ SINGLELINEMODE T)   ; SEE NewSYSTOK
      (SETQ NewFLAG T)
      (SETQ ULCASEFG T)
      (setq STR (|New,ENTRY,2| '|PARSE-NewEXPR| '|process| |$InputStream|))
      (if (/= 0 (setq N (NOTE STR)))
          (progn  (SETQ |$InputStream| (POINTW N |$InputStream|)))
          )
      '|END_OF_New|))

(defun |New,ENTRY,2| (RULE FN INPUTSTREAM) (declare (special INPUTSTREAM))
  (let (zz)
      (INITIALIZE)
      (SETQ |$previousTime| (TEMPUS-FUGIT))
      (setq ZZ (CONVERSATION '|PARSE-NewExpr| '|process|))
      (REMFLAG |boot-NewKEY| 'KEY)
      INPUTSTREAM))

(defun INITIALIZE () 
  (init-boot/spad-reader)
  (initialize-preparse |$InputStream|))

(defmacro try (X)
  `(LET ((|$autoLine|))
        (declare (special |$autoLine|))
        (|tryToFit| (|saveState|) ,X)))

(mapcar #'(lambda (X) (MAKEPROP (CAR X) 'format (CADR X)))
        '((COMMENT |formatCOMMENT|)
          (SEQ |formatSEQ|)
          (DEF |formatDEF|)
          (%LET |formatLET|)
          (\: |formatColon|)
          (ELT |formatELT|)
          (SEGMENT |formatSEGMENT|)
          (COND |formatCOND|)
          (SCOND |formatSCOND|)
          (QUOTE |formatQUOTE|)
          (CONS |formatCONS|)
          (|where| |formatWHERE|)
          (APPEND |formatAPPEND|)
          (REPEAT |formatREPEAT|)
          (COLLECT |formatCOLLECT|)
          (REDUCE |formatREDUCE|)))

(defun SETELTFIRST (A B C) (declare (ignore b)) (RPLACA A C))

(defun SETELTREST (A B C) (declare (ignore b)) (RPLACD A C))

(DEFUN ASSOCIATER (FN LST)
  (COND ((NULL LST) NIL)
        ((NULL (CDR LST)) (CAR LST))
        ((LIST FN (CAR LST) (ASSOCIATER FN (CDR LST))))))

(defun ISLOCALOP-1 (IND)
  "Curindex points at character after '.'"
  (prog (selector buf termtok (NEWCHR (NEXTCHARACTER)))
    (if (TERMINATOR NEWCHR) (RETURN NIL))
    (setq SELECTOR
          (do ((x nil))
              (nil)
            (if (terminator newchr)
                (reverse x)
                (push (setq newchr (nextcharacter)) x))))
    (if (EQUAL NEWCHR '\.) (RETURN (ISLOCALOP-1 IND)))
    (setq BUF (GETSTR (LENGTH SELECTOR)))
    (mapc #'(lambda (x) (suffix x buf)) selector)
    (setq buf (copy-seq selector))
    (setq TERMTOK (INTERN BUF))
    (if (NOT (GET TERMTOK 'GENERIC)) (RETURN NIL))
    (if (OR (GET TERMTOK '|Led|) (GET TERMTOK '|Nud|))
        (GET TERMTOK IND))
    (return TERMTOK)))
; **** X. Random tables

(defvar $MARGIN 3)
(defvar TEMPGENSYMLIST '(|s| |r| |q| |p|))
(defvar ALPHLIST '(|a| |b| |c| |d| |e| |f| |g|))
(defvar LITTLEIN " in ")
(defvar INITALPHLIST ALPHLIST)
(defvar INITXPARLST '(|i| |j| |k| |l| |m| |n| |p| |q|))
(defvar PORDLST (COPY-tree INITXPARLST))
(defvar INITPARLST '(|x| |y| |z| |u| |v| |w| |r| |s| |t|))
(defvar LITTLEA '|a|)
(defvar LITTLEI '|i|)
(defvar ALLSTAR NIL)
(defvar PLUSS "+")
(defvar PERIOD ".")
(defvar SLASH "/")
(defvar COMMA ",")
(defvar LPAR "(")
(defvar RPAR ")")
(defvar EQSIGN "=")
(defvar DASH "-")
(defvar STAR "*")
(defvar DOLLAR "$")
(defvar COLON ":")

(FLAG TEMPGENSYMLIST 'IS-GENSYM)

(MAKEPROP 'COND '|Nud| '(|if| |if| 130 0))
(MAKEPROP 'CONS '|Led| '(CONS CONS 1000 1000))
(MAKEPROP 'APPEND '|Led| '(APPEND APPEND 1000 1000))
(MAKEPROP 'TAG '|Led| '(TAG TAG 122 121))
(MAKEPROP 'EQUATNUM '|Nud| '(|dummy| |dummy| 0 0))
(MAKEPROP 'EQUATNUM '|Led| '(|dummy| |dummy| 10000 0))
(MAKEPROP '%LET '|Led| '(:= %LET 125 124))
(MAKEPROP 'RARROW '|Led| '(== DEF 122 121))
(MAKEPROP 'SEGMENT '|Led| '(\.\. SEGMENT 401 699 (|boot-Seg|)))

;; NAME:    DECIMAL-LENGTH
;; PURPOSE: Computes number of decimal digits in print representation of x
;;  This should made as efficient as possible.

(DEFUN DECIMAL-LENGTH (X)
   (LET* ((K (FIX (* #.(LOG 2.0 10.) (INTEGER-LENGTH X))))
          (X (TRUNCATE (ABS X) (EXPT 10 (1- K)))))
     (IF (< X 10) K (1+ K))))

;(DEFUN DECIMAL-LENGTH2 (X)
;   (LET ((K (FIX (* #.(LOG 2.0 10.) (INTEGER-LENGTH X)))))
;     (IF (< (ABS X) (EXPT 10 K)) K (1+ K))))


;; function to create byte and half-word vectors in new runtime system 8/90

(defun |makeByteWordVec| (initialvalue)
  (let ((n (cond ((null initialvalue) 7) ('t (reduce #'max initialvalue)))))
    (make-array (length initialvalue)
      :element-type (list 'mod (1+ n))
      :initial-contents initialvalue)))

(defun |makeByteWordVec2| (maxelement initialvalue)
  (let ((n (cond ((null initialvalue) 7) ('t maxelement))))
    (make-array (length initialvalue)
      :element-type (list 'mod (1+ n))
      :initial-contents initialvalue)))

(defun |knownEqualPred| (dom)
  (let ((fun (|compiledLookup| '= '((|Boolean|) $ $) dom)))
    (if fun (|getFunctionReplacement| (bpiname (car fun)))
      nil)))

(defun |hashable| (dom)
  (memq (|knownEqualPred| dom)
        #-Lucid '(EQ EQL EQUAL)
        #+Lucid '(EQ EQL EQUAL EQUALP)
        ))

;; simpler interpface to RDEFIOSTREAM
(defun RDEFINSTREAM (&rest fn)
  ;; following line prevents rdefiostream from adding a default filetype
  (if (null (rest fn)) (setq fn (list (pathname (car fn)))))
  (rdefiostream (list (cons 'FILE fn) '(mode . INPUT))))

(defun RDEFOUTSTREAM (&rest fn)
  ;; following line prevents rdefiostream from adding a default filetype
  (if (null (rest fn)) (setq fn (list (pathname (car fn)))))
  (rdefiostream (list (cons 'FILE fn) '(mode . OUTPUT))))


