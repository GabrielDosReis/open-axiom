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


;;  the old compiler splits source files on a domain by domain basis

;;  the new compiler compiles all of the domains in a file together into a
;;  single output file

;;  in order to converge these two approaches nrlibs are being combined on
;;  a file basis rather than split on a domain basis. this change should be
;;  transparent to all code that properly accesses the files.

;;  INTERP.EXPOSED will be enhanced to contain the source file name of
;;  the domain. thus, instead of:
;;     INT                             Integer
;;  it will be:
;;     INT   integer                   Integer

;;  which would mean that the library that contains INT would be integer.NRLIB
;;  by using this mechanism we can continue to use the old libraries 
;;  since each entry would now contain:
;;     INT   INT                       Integer
;;  which would mean that the library that contains the domain INT is INT.NRLIB

;;  old file formats for nrlibs:

;;  first sexpr is integer specifying the byte position of the index of the file
;;  next n sexprs are information in the nrlib
;;  last sexpr is an alist (pointed at by the first number in the file) which
;;  contains triples. e.g. (("slot1info" 0 2550)...)
;;  each triple consists of a string, a zero, and an byte offset into the file
;;  of the information requested e.g. slot1info starts at byte 2550

;;  new file formats for libs:

;;  first sexpr is either an integer (in which case this is exactly an old nrlib
;;        --- or ---
;;  first sexpr is an alist of the form:
;;  ((abbreviation . index) ...)
;;  where each abbreviation is the abbreviation of the domain name and each
;;  index is a pointer to the triples alist

;;  so, for example, integer.spad contains 5 domains:
;;   INTSLPE, INT, NNI, PI and ROMAN
;;  previously INT.NRLIB/index.KAF contained:
;;  2550  
;;  (sexpr1...) 
;;  (sexpr2....) 
;;  (sexpr3...) 
;;  (("sexpr1" 0 8) ("sexpr2" 0 22) ("sexpr3 0 45))
;;  and the individual index.KAF files were similar for the other 4 domains.

;;  under the new scheme integer.nrlib/index.KAF would contain:
;;  ((INTSLPE . 2000) (INT . 4000) (NNI . 6000) (PI . 8000) (ROMAN . 10000))
;;  (sexpr1...)       --- info for INTSLPE
;;  (sexpr2....) 
;;  (sexpr3...) 
;;  (("sexpr1" 0 8) ("sexpr2" 0 22) ("sexpr3 0 45))
;;  (sexpr1...)       --- info for INT
;;  (sexpr2....) 
;;  (sexpr3...) 
;;  (("sexpr1" 0 2800) ("sexpr2" 0 2900) ("sexpr3 0 3000))
;;  (sexpr1...)       --- info for NNI
;;  (sexpr2....) 
;;  (sexpr3...) 
;;  (("sexpr1" 0 4100) ("sexpr2" 0 4200) ("sexpr3 0 4300))
;;  (sexpr1...)       --- info for PI
;;  (sexpr2....) 
;;  (sexpr3...) 
;;  (("sexpr1" 0 6100) ("sexpr2" 0 6200) ("sexpr3 0 6300))
;;  (sexpr1...)       --- info for ROMAN
;;  (sexpr2....) 
;;  (sexpr3...) 
;;  (("sexpr1" 0 8100) ("sexpr2" 0 8200) ("sexpr3 0 8300))

;;  when an NRLIB is opened currently the position information is first
;;  read into the libstream-indextable slot, then this information is
;;  overwritten by the index table itself.

;;  we need the name of the NRLIB passed down to the low level functions
;;  so they can open the new NRLIB format and perform the correct file
;;  position operation. once the NRLIB is open it is only referenced
;;  within one constructor so we can lose the master index table. 


(in-package "BOOT")

; this is a function that expects to be called with a list of old .NRLIB
; names and a string of the new .NRLIB name. e.g.
; (mergelib '(INT NNI PI ROMAN INTSLPE) "integer")

(defun mergelibs (innames outname)
 "each .NRLIB in the inname list is merged into outname.NRLIB"
 (labels (
  (libname (name) (concatenate 'string (string name) ".NRLIB"))
  (indexname (name) (concatenate 'string (string name) ".NRLIB/index.KAF"))
  (lspname (name) (concatenate 'string (string name) ".NRLIB/code.lsp"))
  (fullname (name)
    (concatenate 'string
     (|systemRootDirectory|) "/../../int/algebra/" (string name) ".NRLIB/index.KAF"))
  (fullcode (name)
   (concatenate 'string (|systemRootDirectory|) "/../../int/algebra/" (string name) ".NRLIB/code.lsp")))
 (let (masterindex blanks index newindex (space (* 22 (length innames))))
 (setq newindex space)
 (|removeFile| (libname outname))
 (|checkMkdir| (libname outname))
 (with-open-file (out (indexname outname) :direction :output)
  (setq blanks (make-string space :initial-element #\ ))
  (write  blanks :stream out)       ; reserve space for the masterindex
  (finish-output out)
  (dolist (inname innames)
   (when (probe-file (fullname inname))
   (with-open-file (in (fullname inname))
    (let (alist pos)
     (setq index (read in))
     (file-position in index)
     (setq alist (read in))
     (dolist (ptr alist)
      (when (setq pos (third ptr))
       (file-position in pos)
       (setf (third ptr) (file-position out))
       (print (read in) out)
       (finish-output out)))
     (finish-output out)
     (push (cons inname (file-position out)) masterindex)
     (write alist :stream out :level nil :length nil :escape t)))))
  (file-position out 0)
  (print masterindex out))
  (dolist (inname innames)
   (format t "cat ~a >>~a~%" (fullcode inname) (lspname outname))
   (system::system 
    (format nil "cat ~a >>~a" (fullcode inname) (lspname outname)))))))


(defun |pathname| (p)
 (cond
  ((null p) p) 
  ((pathnamep p) p) 
  ((null (pairp p)) (pathname p)) 
  ('else 
   (when (> (length p) 2) 
      (setq p (list (mergelib (first p)) (second p))))
   (apply (function make-filename) p))))

(defun mergelib (x) 
 (declare (special $mergelib))
 (let (result)
  (setq result (assoc x $mergelib))
  (if result
   (cdr result)
   x)))


; from lisplib.boot
(defun |readLib1| (fn ft fm)
  (|readLibPathFast| (|pathname| (list fn ft fm)) fn))

(defun |readLibPathFast| (p &optional fn)
 (rdefiostream (list (cons 'file p) '(mode . input)) nil fn))


(in-package "BOOT")

; from nlib.lisp
(defun get-index-table-from-stream (stream &optional abbrev)
 (let (pos)
  (file-position stream 0)
  (setq pos (read stream))
  (cond 
   ((numberp pos)
     (file-position stream pos)
     (read stream))
   ((consp pos)
     (setq pos (cdr (assoc abbrev pos)))
     (file-position stream pos)
     (read stream))
   ('else pos))))

(defun loadvol (&rest filearg)
  (cond ((typep (car filearg) 'libstream)
         (load (concat (libstream-dirname (car filearg)) "/code")))
        (t 
          (setq filearg (make-input-filename (boot::mergelib filearg) 'LISPLIB))
          (if (library-file filearg)
              (load (concat filearg "/code"))
            (load filearg)))))

; from nlib.lisp
;; (RDEFIOSTREAM ((MODE . IO) (FILE fn ft dir))) IO is I,O,INPUT,OUTPUT
(defun rdefiostream
              (options &optional (missing-file-error-flag t) abbrev)
  (let ((mode (cdr (assoc 'mode options)))
        (file (assoc 'file options))
        (stream nil)
        (fullname nil)
        (indextable nil))
        (cond ((equal (elt (string mode) 0) #\I)
               (setq fullname (make-input-filename (cdr file) 'LISPLIB))
               (setq stream (get-input-index-stream fullname))
               (if (null stream)
                   (if missing-file-error-flag
                       (ERROR (format nil "Library ~s doesn't exist"
                              (make-filename (cdr file) 'LISPLIB)))
                     NIL)
               (make-libstream :mode 'input  :dirname fullname
                   :indextable (get-index-table-from-stream stream abbrev)
                   :indexstream stream)))
              ((equal (elt (string mode) 0) #\O)
               (setq fullname (make-full-namestring (cdr file) 'LISPLIB))
               (case (directory? fullname)
                     (-1 (|checkMkdir| fullname))
                     (0 (error (format nil "~s is an existing file, not a library" fullname)))
                     (otherwise))
               (multiple-value-setq (stream indextable) (get-io-index-stream fullname))
               (make-libstream :mode 'output  :dirname fullname
                               :indextable indextable
                               :indexstream stream ))
              ('t  (ERROR "Unknown MODE")))))

(in-package "BOOT")

; from lisplib.boot
(defun |readLibPathFast| (p &optional abbrev)
  (rdefiostream (list (list 'file p) '(mode . input)) nil abbrev))

 
; from lisplib.boot
(defun |hasFilePropertyNoCache| (p id &optional abbrev)
 (let (fnstream result)
  (when (eq id '|constructorModemap|)(format t "~a (~a) has ~a~%" p abbrev id))
  (setq fnstream (|readLibPathFast| p abbrev))
  (when fnstream
   (setq result (|rread| id fnstream nil))
   (rshut fnstream)
   result)))



(defun |loadLibNoUpdate| (cname libName)
 (let (fullLibName libDir kind)
    (setq fullLibName (make-input-filename (mergelib libName) |$spadLibFT|))
    (setq libDir (directory-namestring fullLibName)) 
    (setq kind (|getConstuctorKindFromDB| cname)) 
    (when |$printLoadMsgs| 
     (|sayKeyedMsg| 'S2IL0002 (list (|namestring| fullLibName) kind cname)))
    (load (concatenate 'string libDir (mergelib libName)))
    (|clearConstructorCache| cname) 
    (when (get cname 'loaded)
      (|unInstantiate| 
       (cons cname
         (mapcar '|getConstructorUnabbreviation| 
          (|dependentClosure| (list cname))))))
    (|installConstructor| cname kind) 
    (makeprop libName 'loaded fullLibName) 
    (when |$InteractiveMode| (setq |$CategoryFrame| (list (list (list nil)))))
    (|stopTimingProcess| '|load|)
    t))


; this is a program which, given the path to the source file INTERP.EXPOSED and
; a path to the old style index.KAF files, will create a new interp.exposed
; in the current directory such that each library line has the source file
; name appended to the line. this is a useful one-time function for 
; converting from old style INTERP.EXPOSED to new style interp.exposed

(defun make-interp (src int)
 (labels (
  (FINDSRC (libname)
   "return a string name of the source file given the library file
    name (eg PI) as a string"
   (let (kaffile index alist result)
    (setq kaffile 
     (concatenate 'string int "/" libname ".NRLIB/index.KAF"))
    (if (probe-file kaffile)
     (with-open-file (kaf kaffile)
      (setq index (read kaf))
      (file-position kaf index)
      (setq alist (read kaf))
     (setq index (third (assoc "sourceFile" alist :test #'string=)))
      (file-position kaf index)
     (setq result (pathname-name (pathname (read kaf index)))))
     (format t "~a does not exist~%" kaffile))
    result)))
 (let (expr)
  (with-open-file (out "interp.exposed" :direction :output)
  (with-open-file (in (concatenate 'string src "/INTERP.EXPOSED"))
   (catch 'eof
    (loop
     (setq expr (read-line in nil 'eof))
     (when (eq expr 'eof) (throw 'eof nil))
     (if
      (and
       (> (length expr) 58)
       (char= (schar expr 0) #\space) 
       (not (char= (schar expr 8) #\space)))
      (format out "~66a ~a~%" expr 
         (findsrc (string-right-trim '(#\space) (subseq expr 58)))) 
      (format out "~a~%" expr)))))))))   


; mergeall is a utility that will scan all of the .spad files and copy
; all of the resulting old .NRLIBs into the correct new .NRLIBs.
; one complication is that category constructors may not have old .NRLIBs
(defun mergeall (src)
 (labels (
  (SRCSCAN ()
   (let (spads)
    (|changeDirectory| src)
    (setq spads (directory "*.spad"))
    (dolist (spad spads) (srcabbrevs spad))
    nil))
  (SRCABBREVS (sourcefile)
   (let (expr names abbrev point mark newmark)
    (catch 'done
     (with-open-file (in sourcefile)
      (loop
       (setq expr (read-line in nil 'done))
       (when (eq expr 'done) (throw 'done nil))
       (when (and (> (length expr) 4)
             (string= ")abb" (subseq expr 0 4)))
        (setq point (position #\space expr :from-end t :test #'char=))
        (setq mark
         (position #\space 
          (string-right-trim '(#\space)
           (subseq expr 0 (1- point))) :from-end t))
        (setq abbrev (string-trim '(#\space) (subseq expr mark point)))
        (push abbrev names)
        (setq newmark
         (position #\space 
          (string-right-trim '(#\space)
           (subseq expr 0 (1- mark))) :from-end t))
        (when (string= "CAT"
              (string-upcase 
                (string-trim '(#\space) (subseq expr newmark (+ newmark 4)))))
          (push (concatenate 'string abbrev "-") names))))))
    (format t "(mergelibs '~a ~s)~%" names (string (pathname-name sourcefile)))
    names)))
 (srcscan)))


; rescan will search new style NRLIBs and construct a new $MERGELIB list
; this should be run after merging the libraries
(defun rescan ()
 (labels (
  (indexname (name) (concatenate 'string (namestring name) "/index.KAF")))
 (let (result)
  (mapcar #'(lambda (f) 
   (dolist (i (car f)) (push (cons (car i) (cdr f)) result)))
   (mapcar #'(lambda (f)
    (with-open-file (in (indexname f)) (cons (read in) (pathname-name f))))
    (directory "*.NRLIB")))
  result)))
   

(defvar $mergelib nil) ;; use old scheme for now

;;(defvar $mergelib '(
;; (INT . "integer") (NNI . "integer") (PI . "integer") 
;; (ROMAN . "integer") (INTSLPE . "integer")))


;((YSTREAM . "ystream") (WEIER . "weier") (RESLATC . "void") 
;(EXIT . "void") (VOID . "void") (VIEW . "viewpack") 
;(VIEWDEF . "viewDef") (VIEW3D . "view3D") (VIEW2D . "view2D") 
;(GRIMAGE . "view2D") (DIRPROD2 . "vector") (DIRPROD . "vector") 
;(DIRPCAT- . "vector") (DIRPCAT . "vector") (VECTOR2 . "vector") 
;(VECTOR . "vector") (IVECTOR . "vector") (VECTCAT- . "vector") 
;(VECTCAT . "vector") (ANON . "variable") (FUNCTION . "variable") 
;(RULECOLD . "variable") (VARIABLE . "variable") (OVAR . "variable") 
;(UTSODE . "utsode") (UNIFACT . "unifact") (TWOFACT . "twofact") 
;(NUMTUBE . "tube") (EXPRTUBE . "tube") (TUBETOOL . "tube") 
;(TUBE . "tube") (SPFCAT . "trigcat") (CFCAT . "trigcat") 
;(LFCAT . "trigcat") (PRIMCAT . "trigcat") (TRIGCAT- . "trigcat") 
;(TRIGCAT . "trigcat") (TRANFUN- . "trigcat") (TRANFUN . "trigcat") 
;(HYPCAT- . "trigcat") (HYPCAT . "trigcat") (ATRIG- . "trigcat") 
;(ATRIG . "trigcat") (AHYP . "trigcat") (ELEMFUN- . "trigcat") 
;(ELEMFUN . "trigcat") (PENDTREE . "tree") (BBTREE . "tree") 
;(BTOURN . "tree") (BSTREE . "tree") (BTREE . "tree") 
;(BTCAT- . "tree") (BTCAT . "tree") (TREE . "tree") 
;(SOLVESER . "transsolve") (SOLVETRA . "transsolve") (TEX1 . "tex") 
;(TEX . "tex") (UTS2 . "taylor") (UTS . "taylor") 
;(ITAYLOR . "taylor") (TABLBUMP . "tableau") (TABLEAU . "tableau") 
;(STBL . "table") (GSTBL . "table") (STRTBL . "table") 
;(EQTBL . "table") (TABLE . "table") (INTABL . "table") 
;(HASHTBL . "table") (MSYSCMD . "system") (SYSSOLP . "syssolp") 
;(SYMBOL . "symbol") (SUTS . "suts") (SUMRF . "sum") 
;(GOSPER . "sum") (ISUMP . "sum") (SUCH . "suchthat") 
;(STTF . "sttf") (STTAYLOR . "sttaylor") (STRICAT . "string") 
;(STRING . "string") (ISTRING . "string") (CCLASS . "string") 
;(CHAR . "string") (STREAM3 . "stream") (STREAM2 . "stream") 
;(STREAM1 . "stream") (STREAM . "stream") (CSTTOOLS . "stream") 
;(LZSTAGG- . "stream") (LZSTAGG . "stream") (NTPOLFN . "special") 
;(ORTHPOL . "special") (SFSFUN . "special") (TOPSP . "space") 
;(SPACE3 . "space") (SPACEC . "space") (SORTPAK . "sortpak") 
;(SORTPAK . "sort") (SOLVERAD . "solverad") (LSPP . "solvelin") 
;(LSMP . "solvelin") (SOLVEFOR . "solvefor") (DIOSP . "solvedio") 
;(SMITH . "smith") (LIMITRF . "sign") (SIGNRF . "sign") 
;(INPSIGN . "sign") (TOOLSIGN . "sign") (SI . "si") 
;(INS- . "si") (INS . "si") (SGCF . "sgcf") 
;(SF . "sf") (FPS- . "sf") (FPS . "sf") 
;(RNS- . "sf") (RNS . "sf") (RADCAT- . "sf") 
;(RADCAT . "sf") (SEX . "sex") (SEXOF . "sex") 
;(SEXCAT . "sex") (SET . "sets") (UDVO . "setorder") 
;(UDPO . "setorder") (INCRMAPS . "seg") (UNISEG2 . "seg") 
;(UNISEG . "seg") (SEGBIND2 . "seg") (SEGBIND . "seg") 
;(SEG2 . "seg") (SEG . "seg") (SEGXCAT . "seg") 
;(SEGCAT . "seg") (RULESET . "rule") (APPRULE . "rule") 
;(RULE . "rule") (ODERTRIC . "riccati") (ODEPRRIC . "riccati") 
;(MKODRING . "riccati") (RF . "rf") (POLYCATQ . "rf") 
;(RATRET . "retract") (INTRET . "retract") (FRETRCT- . "retract") 
;(FRETRCT . "retract") (RESRING . "resring") (REP2 . "rep2") 
;(REP1 . "rep1") (REAL0 . "realzero") (REAL0Q . "real0q") 
;(RDEEFS . "rdesys") (RDETRS . "rdesys") (RDETR . "rderf") 
;(RDEEF . "rdeef") (INTTOOLS . "rdeef") (RATFACT . "ratfact") 
;(RFDIST . "random") (RIDIST . "random") (INTBIT . "random") 
;(RDIST . "random") (RANDSRC . "random") (RADUTIL . "radix") 
;(HEXADEC . "radix") (DECIMAL . "radix") (BINARY . "radix") 
;(RADIX . "radix") (REP . "radeigen") (QUATCT2 . "quat") 
;(QUAT . "quat") (QUATCAT- . "quat") (QUATCAT . "quat") 
;(QALGSET2 . "qalgset") (QALGSET . "qalgset") (UPXS2 . "puiseux") 
;(UPXS . "puiseux") (UPXSCONS . "puiseux") (UPXSCCA- . "puiseux") 
;(UPXSCCA . "puiseux") (PTRANFN . "ptranfn") (MTSCAT . "pscat") 
;(UPXSCAT . "pscat") (ULSCAT . "pscat") (UTSCAT- . "pscat") 
;(UTSCAT . "pscat") (UPSCAT- . "pscat") (UPSCAT . "pscat") 
;(PSCAT- . "pscat") (PSCAT . "pscat") (SYMPOLY . "prtition") 
;(PRTITION . "prtition") (PRODUCT . "product") (PRINT . "print") 
;(FSPRMELT . "primelt") (PRIMELT . "primelt") (COMMUPC . "polycat") 
;(UPOLYC2 . "polycat") (UPOLYC- . "polycat") (UPOLYC . "polycat") 
;(POLYLIFT . "polycat") (POLYCAT- . "polycat") (POLYCAT . "polycat") 
;(FAMR- . "polycat") (FAMR . "polycat") (AMR- . "polycat") 
;(AMR . "polycat") (PSQFR . "poly") (UPSQFREE . "poly") 
;(POLY2UP . "poly") (UP2 . "poly") (UP . "poly") 
;(SUP2 . "poly") (SUP . "poly") (PR . "poly") 
;(FM . "poly") (POLTOPOL . "poltopol") (MPC3 . "poltopol") 
;(MPC2 . "poltopol") (PLOTTOOL . "plottool") (PLOT3D . "plot3d") 
;(PLOT1 . "plot") (PLOT . "plot") (PLEQN . "pleqn") 
;(PINTERP . "pinterp") (PINTERPA . "pinterp") (PGROEB . "pgrobner") 
;(PGCD . "pgcd") (PFRPAC . "pfr") (PFR . "pfr") 
;(PFO . "pfo") (FSRED . "pfo") (PFOQ . "pfo") 
;(PFOTOOLS . "pfo") (RDIV . "pfo") (FORDER . "pfo") 
;(PFBR . "pfbr") (PFBRU . "pfbr") (PF . "pf") 
;(IPF . "pf") (PGE . "permgrps") (PERMGRP . "permgrps") 
;(PERMAN . "perman") (GRAY . "perman") (PERM . "perm") 
;(PERMCAT . "perm") (PSCURVE . "pcurve") (PPCURVE . "pcurve") 
;(PATAB . "pattern") (PATTERN2 . "pattern") (PATTERN1 . "pattern") 
;(PATTERN . "pattern") (PATMATCH . "patmatch") (PMLSAGG . "patmatch") 
;(PMFS . "patmatch") (PMPLCAT . "patmatch") (PMTOOLS . "patmatch") 
;(PMQFCAT . "patmatch") (PMDOWN . "patmatch") (PMINS . "patmatch") 
;(PMKERNEL . "patmatch") (PMSYM . "patmatch") (FPATMAB . "patmatch") 
;(PATMAB . "patmatch") (PATLRES . "patmatch") (PATRES2 . "patmatch") 
;(PATRES . "patmatch") (PARTPERM . "partperm") (PARSU2 . "paramete") 
;(PARSURF . "paramete") (PARSC2 . "paramete") (PARSCURV . "paramete") 
;(PARPC2 . "paramete") (PARPCURV . "paramete") (BPADICRT . "padic") 
;(PADICRAT . "padic") (PADICRC . "padic") (BPADIC . "padic") 
;(PADIC . "padic") (IPADIC . "padic") (PADICCT . "padic") 
;(PADE . "pade") (PADEPAC . "pade") (OUTFORM . "outform") 
;(NUMFMT . "outform") (DISPLAY . "out") (SPECOUT . "out") 
;(OUT . "out") (OP . "opalg") (COMMONOP . "op") 
;(BOP1 . "op") (BOP . "op") (ODECONST . "oderf") 
;(ODEINT . "oderf") (ODETOOLS . "oderf") (ODERAT . "oderf") 
;(RTODETLS . "oderf") (SCFRAC . "oderf") (ODEPRIM . "oderf") 
;(BOUNDZRO . "oderf") (BALFACT . "oderf") (ODEEF . "odeef") 
;(REDORDER . "odeef") (ODEPAL . "odealg") (ODERED . "odealg") 
;(ODESYS . "odealg") (OCTCT2 . "oct") (OCT . "oct") 
;(OC- . "oct") (OC . "oct") (PNTHEORY . "numtheor") 
;(INTHEORY . "numtheor") (FLOATCP . "numsolve") (FLOATRP . "numsolve") 
;(INFSP . "numsolve") (NUMQUAD . "numquad") (NUMODE . "numode") 
;(DRAWHACK . "numeric") (NUMERIC . "numeric") (NCEP . "numeigen") 
;(NREP . "numeigen") (INEP . "numeigen") (NPCOEF . "npcoef") 
;(NODE1 . "nlode") (NLINSOL . "nlinsol") (RETSOL . "nlinsol") 
;(PTFUNC2 . "newpoint") (PTPACK . "newpoint") (SUBSPACE . "newpoint") 
;(COMPPROP . "newpoint") (POINT . "newpoint") (PTCAT . "newpoint") 
;(FRNAALG- . "naalgc") (FRNAALG . "naalgc") (FINAALG- . "naalgc") 
;(FINAALG . "naalgc") (NAALG- . "naalgc") (NAALG . "naalgc") 
;(NASRING- . "naalgc") (NASRING . "naalgc") (NARNG- . "naalgc") 
;(NARNG . "naalgc") (MONADWU- . "naalgc") (MONADWU . "naalgc") 
;(MONAD- . "naalgc") (MONAD . "naalgc") (FRNAAF2 . "naalg") 
;(ALGPKG . "naalg") (SCPKG . "naalg") (ALGSC . "naalg") 
;(MULTSQFR . "multsqfr") (INDE . "multpoly") (SMP . "multpoly") 
;(MPOLY . "multpoly") (POLY2 . "multpoly") (POLY . "multpoly") 
;(ALGMFACT . "multfact") (MULTFACT . "multfact") (INNMFACT . "multfact") 
;(TS . "mts") (SMTS . "mts") (MSET . "mset") 
;(MRF2 . "mring") (MRING . "mring") (MOEBIUS . "moebius") 
;(MODFIELD . "modring") (EMR . "modring") (MODRING . "modring") 
;(MODMON . "modmon") (INMODGCD . "modgcd") (MDDFACT . "moddfact") 
;(MLIFT . "mlift") (MKRECORD . "mkrecord") (MKFLCFN . "mkfunc") 
;(MKBCFUNC . "mkfunc") (MKUCFUNC . "mkfunc") (MKFUNC . "mkfunc") 
;(INFORM1 . "mkfunc") (INFORM . "mkfunc") (SAOS . "misc") 
;(MFINFACT . "mfinfact") (MESH . "mesh") (MATSTOR . "matstor") 
;(SQMATRIX . "matrix") (RMATRIX . "matrix") (MATRIX . "matrix") 
;(IMATRIX . "matrix") (MATLIN . "matfuns") (IMATQF . "matfuns") 
;(RMCAT2 . "matfuns") (MATCAT2 . "matfuns") (IMATLIN . "matfuns") 
;(SMATCAT- . "matcat") (SMATCAT . "matcat") (RMATCAT- . "matcat") 
;(RMATCAT . "matcat") (MATCAT- . "matcat") (MATCAT . "matcat") 
;(MAPPKG3 . "mappkg") (MAPPKG2 . "mappkg") (MAPPKG1 . "mappkg") 
;(MAPHACK3 . "mappkg") (MAPHACK2 . "mappkg") (MAPHACK1 . "mappkg") 
;(TRMANIP . "manip") (ALGMANIP . "manip") (POLYROOT . "manip") 
;(FACTFUNC . "manip") (LODOF . "lodof") (DPMM . "lodo") 
;(DPMO . "lodo") (ODR . "lodo") (LODO . "lodo") 
;(NCODIV . "lodo") (OMLO . "lodo") (MLO . "lodo") 
;(LMDICT . "lmdict") (HEUGCD . "listgcd") (ALIST . "list") 
;(LIST2MAP . "list") (LIST3 . "list") (LIST2 . "list") 
;(LIST . "list") (ILIST . "list") (LF . "liouv") 
;(LGROBP . "lingrob") (ZLINDEP . "lindep") (LINDEP . "lindep") 
;(SIGNEF . "limitps") (LIMITPS . "limitps") (LSQM . "lie") 
;(JORDAN . "lie") (LIE . "lie") (LEADCDET . "leadcdet") 
;(ULS2 . "laurent") (ULS . "laurent") (ULSCONS . "laurent") 
;(ULSCCAT- . "laurent") (ULSCCAT . "laurent") (INVLAPLA . "laplace") 
;(LAPLACE . "laplace") (KOVACIC . "kovacic") (KERNEL2 . "kl") 
;(KERNEL . "kl") (MKCHSET . "kl") (SCACHE . "kl") 
;(CACHSET . "kl") (ITFUN3 . "ituple") (ITFUN2 . "ituple") 
;(ITUPLE . "ituple") (IRSN . "irsn") (IRRF2F . "irexpand") 
;(IR2F . "irexpand") (INTRF . "intrf") (INTRAT . "intrf") 
;(INTTR . "intrf") (INTHERTR . "intrf") (MONOTOOL . "intrf") 
;(SUBRESP . "intrf") (INTPM . "intpm") (INTFACT . "intfact") 
;(IROOT . "intfact") (PRIMES . "intfact") (FSINT . "integrat") 
;(FSCINT . "integrat") (ROMAN . "integer") (PI . "integer") 
;(NNI . "integer") (INT . "integer") (INTSLPE . "integer") 
;(INTEF . "intef") (NFINTBAS . "intclos") (WFFINTBS . "intclos") 
;(FFINTBAS . "intclos") (IBATOOL . "intclos") (TRIMAT . "intclos") 
;(IR2 . "intaux") (IR . "intaux") (INTALG . "intalg") 
;(INTHERAL . "intalg") (DBLRESP . "intalg") (INTAF . "intaf") 
;(INTPAF . "intaf") (INTG0 . "intaf") (INPRODFF . "infprod") 
;(INPRODPF . "infprod") (INFPROD0 . "infprod") (STINPROD . "infprod") 
;(IDPAG . "indexedp") (IDPOAMS . "indexedp") (IDPOAM . "indexedp") 
;(IDPAM . "indexedp") (IDPO . "indexedp") (IDPC . "indexedp") 
;(IDECOMP . "idecomp") (IDEAL . "ideal") (GROEBSOL . "groebsol") 
;(GBF . "groebf") (GRDEF . "grdef") (LAUPOL . "gpol") 
;(GENPGCD . "gpgcd") (GHENSEL . "ghensel") (GENUPS . "genups") 
;(GENUFACT . "genufact") (CVMP . "generic") (GCNAALG . "generic") 
;(GENEEZ . "geneez") (HDMP . "gdpoly") (DMP . "gdpoly") 
;(GDMP . "gdpoly") (HDP . "gdirprod") (ODP . "gdirprod") 
;(ORDFUNS . "gdirprod") (GBINTERN . "gbintern") (GBEUCLID . "gbeuclid") 
;(GB . "gb") (CINTSLPE . "gaussian") (COMPFACT . "gaussian") 
;(COMPLEX2 . "gaussian") (COMPLEX . "gaussian") (COMPCAT- . "gaussian") 
;(COMPCAT . "gaussian") (GAUSSFAC . "gaussfac") (FSUPFACT . "funcpkgs") 
;(FS2 . "fspace") (FS- . "fspace") (FS . "fspace") 
;(ES2 . "fspace") (ES1 . "fspace") (ES- . "fspace") 
;(ES . "fspace") (FS2UPS . "fs2ups") (FS2EXPXP . "fs2expxp") 
;(FAGROUP . "free") (FAMONOID . "free") (IFAMON . "free") 
;(FAMONC . "free") (FGROUP . "free") (FMONOID . "free") 
;(LMOPS . "free") (FRAC2 . "fraction") (LPEFRAC . "fraction") 
;(FRAC . "fraction") (QFCAT2 . "fraction") (QFCAT- . "fraction") 
;(QFCAT . "fraction") (LA . "fraction") (LO . "fraction") 
;(FR2 . "fr") (FRUTIL . "fr") (FR . "fr") 
;(FORMULA1 . "formula") (FORMULA . "formula") (FNLA . "fnla") 
;(HB . "fnla") (COMM . "fnla") (OSI . "fnla") 
;(FNAME . "fname") (FNCAT . "fname") (ZMOD . "fmod") 
;(FLOAT . "float") (LIB . "files") (KAFILE . "files") 
;(TEXTFILE . "files") (FILE . "files") (FILECAT . "files") 
;(IRREDFFX . "ffx") (FFPOLY2 . "ffpoly2") (FFPOLY . "ffpoly") 
;(FF . "ffp") (IFF . "ffp") (FFX . "ffp") 
;(FFP . "ffp") (FFNB . "ffnb") (FFNBX . "ffnb") 
;(FFNBP . "ffnb") (INBFF . "ffnb") (FFHOM . "ffhom") 
;(FFF . "fff") (FFCG . "ffcg") (FFCGX . "ffcg") 
;(FFCGP . "ffcg") (FFSLPE . "ffcat") (FFIELDC- . "ffcat") 
;(FFIELDC . "ffcat") (DLP . "ffcat") (FAXF- . "ffcat") 
;(FAXF . "ffcat") (XF- . "ffcat") (XF . "ffcat") 
;(FPC- . "ffcat") (FPC . "ffcat") (PUSHVAR . "facutil") 
;(FACUTIL . "facutil") (EXPRODE . "exprode") (EXPR2UPS . "expr2ups") 
;(PICOERCE . "expr") (HACKPI . "expr") (PMASS . "expr") 
;(PMPRED . "expr") (PMASSFS . "expr") (PMPREDFS . "expr") 
;(EXPR2 . "expr") (PAN2EXPR . "expr") (EXPR . "expr") 
;(EXPEXPAN . "expexpan") (UPXSSING . "expexpan") (EXPUPXS . "expexpan") 
;(ERROR . "error") (FEVALAB- . "equation") (FEVALAB . "equation") 
;(EVALAB- . "equation") (EVALAB . "equation") (IEVALAB- . "equation") 
;(IEVALAB . "equation") (EQ2 . "equation") (EQ . "equation") 
;(ELFUTS . "elfuts") (EF . "elemntry") (CHARPOL . "eigen") 
;(EP . "eigen") (EFUPXS . "efupxs") (EFULS . "efuls") 
;(TRIGMNIP . "efstruc") (ITRIGMNP . "efstruc") (EFSTRUC . "efstruc") 
;(TANEXP . "efstruc") (SYMFUNC . "efstruc") (DRAWCX . "drawpak") 
;(DROPT0 . "drawopt") (DROPT1 . "drawopt") (DROPT . "drawopt") 
;(DRAWCURV . "draw") (DRAW . "draw") (DRAWCFUN . "draw") 
;(SDPOL . "dpolcat") (ODPOL . "dpolcat") (DSMP . "dpolcat") 
;(DPOLCAT- . "dpolcat") (DPOLCAT . "dpolcat") (SDVAR . "dpolcat") 
;(ODVAR . "dpolcat") (DVARCAT- . "dpolcat") (DVARCAT . "dpolcat") 
;(FDIV2 . "divisor") (FDIV . "divisor") (FRMOD . "divisor") 
;(MHROWRED . "divisor") (FRIDEAL2 . "divisor") (FRIDEAL . "divisor") 
;(DHMATRIX . "dhmatrix") (DERHAM . "derham") (ANTISYM . "derham") 
;(EAB . "derham") (LALG- . "derham") (LALG . "derham") 
;(DEGRED . "degred") (DEFINTRF . "defintrf") (DFINTTLS . "defintrf") 
;(DEFINTEF . "defintef") (FLASORT . "defaults") (REPDB . "defaults") 
;(REPSQ . "defaults") (DDFACT . "ddfact") (CYCLOTOM . "cyclotom") 
;(EVALCYC . "cycles") (CYCLES . "cycles") (ALGFF . "curve") 
;(RADFF . "curve") (CHVAR . "curve") (FFCAT2 . "curve") 
;(MMAP . "curve") (FFCAT- . "curve") (FFCAT . "curve") 
;(CRFP . "crfp") (CRAPACK . "cra") (COORDSYS . "coordsys") 
;(NCNTFRAC . "contfrac") (CONTFRAC . "contfrac") (AN . "constant") 
;(INFINITY . "complet") (ONECOMP2 . "complet") (ONECOMP . "complet") 
;(ORDCOMP2 . "complet") (ORDCOMP . "complet") (COMBINAT . "combinat") 
;(SUMFS . "combfunc") (FSPECF . "combfunc") (COMBF . "combfunc") 
;(COMBOPC . "combfunc") (PALETTE . "color") (COLOR . "color") 
;(RETRACT- . "coerce") (RETRACT . "coerce") (KONVERT . "coerce") 
;(KOERCE . "coerce") (OBJECT . "coerce") (TYPE . "coerce") 
;(CMPLXRT . "cmplxrt") (CLIP . "clip") (CLIF . "clifford") 
;(QFORM . "clifford") (MCDEN . "cden") (UPCDEN . "cden") 
;(CDEN . "cden") (ICDEN . "cden") (VSPACE . "catdef") 
;(UFD- . "catdef") (UFD . "catdef") (STEP . "catdef") 
;(SGROUP- . "catdef") (SGROUP . "catdef") (SETCAT- . "catdef") 
;(SETCAT . "catdef") (RNG . "catdef") (RMODULE . "catdef") 
;(RING- . "catdef") (RING . "catdef") (REAL . "catdef") 
;(PID . "catdef") (PFECAT- . "catdef") (PFECAT . "catdef") 
;(PDRING- . "catdef") (PDRING . "catdef") (ORDSET- . "catdef") 
;(ORDSET . "catdef") (ORDRING- . "catdef") (ORDRING . "catdef") 
;(ORDMON . "catdef") (ORDFIN . "catdef") (OASGP . "catdef") 
;(OAMONS . "catdef") (OCAMON . "catdef") (OAMON . "catdef") 
;(OAGROUP . "catdef") (MONOID- . "catdef") (MONOID . "catdef") 
;(MODULE- . "catdef") (MODULE . "catdef") (LMODULE . "catdef") 
;(LINEXP . "catdef") (INTDOM- . "catdef") (INTDOM . "catdef") 
;(GROUP- . "catdef") (GROUP . "catdef") (GCDDOM- . "catdef") 
;(GCDDOM . "catdef") (FRAMALG- . "catdef") (FRAMALG . "catdef") 
;(FLINEXP- . "catdef") (FLINEXP . "catdef") (FINITE . "catdef") 
;(FIELD- . "catdef") (FIELD . "catdef") (EUCDOM- . "catdef") 
;(EUCDOM . "catdef") (ENTIRER . "catdef") (DIVRING- . "catdef") 
;(DIVRING . "catdef") (DIFEXT- . "catdef") (DIFEXT . "catdef") 
;(DIFRING- . "catdef") (DIFRING . "catdef") (COMRING . "catdef") 
;(CHARNZ . "catdef") (CHARZ . "catdef") (CABMON . "catdef") 
;(BMODULE . "catdef") (ALGEBRA- . "catdef") (ALGEBRA . "catdef") 
;(ABELSG- . "catdef") (ABELSG . "catdef") (ABELMON- . "catdef") 
;(ABELMON . "catdef") (ABELGRP- . "catdef") (ABELGRP . "catdef") 
;(CARTEN2 . "carten") (CARTEN . "carten") (GRALG- . "carten") 
;(GRALG . "carten") (GRMOD- . "carten") (GRMOD . "carten") 
;(CARD . "card") (BITS . "boolean") (IBITS . "boolean") 
;(BOOLEAN . "boolean") (REF . "boolean") (BEZOUT . "bezout") 
;(HEAP . "bags") (HEAP . "bags") (DEQUEUE . "bags") 
;(QUEUE . "bags") (ASTACK . "bags") (STACK . "bags") 
;(ATTREG . "attreg") (ARRAY2 . "array2") (IARRAY2 . "array2") 
;(IIARRAY2 . "array2") (ARR2CAT- . "array2") (ARR2CAT . "array2") 
;(ARRAY12 . "array1") (ARRAY1 . "array1") (IARRAY1 . "array1") 
;(FARRAY . "array1") (IFARRAY . "array1") (TUPLE . "array1") 
;(PRIMARR2 . "array1") (PRIMARR . "array1") (OPQUERY . "aql") 
;(MTHING . "aql") (QEQUAT . "aql") (DBASE . "aql") 
;(ICARD . "aql") (DLIST . "aql") (ANY1 . "any") 
;(ANY . "any") (NONE1 . "any") (NONE . "any") 
;(OPQUERY . "alql") (MTHING . "alql") (QEQUAT . "alql") 
;(DBASE . "alql") (ICARD . "alql") (DLIST . "alql") 
;(RFFACTOR . "allfact") (GENMFACT . "allfact") (MPCPF . "allfact") 
;(MPRFF . "allfact") (MRATFAC . "allfact") (AF . "algfunc") 
;(ACFS- . "algfunc") (ACFS . "algfunc") (ACF- . "algfunc") 
;(ACF . "algfunc") (ALGFACT . "algfact") (SAERFFC . "algfact") 
;(RFFACT . "algfact") (SAEFACT . "algfact") (IALGFACT . "algfact") 
;(SAE . "algext") (MONOGEN- . "algcat") (MONOGEN . "algcat") 
;(FRAMALG- . "algcat") (FRAMALG . "algcat") (FINRALG- . "algcat") 
;(FINRALG . "algcat") (FSAGG2 . "aggcat2") (FLAGG2 . "aggcat2") 
;(BTAGG- . "aggcat") (BTAGG . "aggcat") (SRAGG- . "aggcat") 
;(SRAGG . "aggcat") (ALAGG . "aggcat") (LSAGG- . "aggcat") 
;(LSAGG . "aggcat") (ELAGG- . "aggcat") (ELAGG . "aggcat") 
;(A1AGG- . "aggcat") (A1AGG . "aggcat") (FLAGG- . "aggcat") 
;(FLAGG . "aggcat") (LNAGG- . "aggcat") (LNAGG . "aggcat") 
;(STAGG- . "aggcat") (STAGG . "aggcat") (URAGG- . "aggcat") 
;(URAGG . "aggcat") (DLAGG . "aggcat") (BRAGG- . "aggcat") 
;(BRAGG . "aggcat") (RCAGG- . "aggcat") (RCAGG . "aggcat") 
;(TBAGG- . "aggcat") (TBAGG . "aggcat") (IXAGG- . "aggcat") 
;(IXAGG . "aggcat") (ELTAGG- . "aggcat") (ELTAGG . "aggcat") 
;(ELTAB . "aggcat") (KDAGG- . "aggcat") (KDAGG . "aggcat") 
;(OMSAGG . "aggcat") (MSETAGG . "aggcat") (FSAGG- . "aggcat") 
;(FSAGG . "aggcat") (SETAGG- . "aggcat") (SETAGG . "aggcat") 
;(MDAGG . "aggcat") (DIAGG- . "aggcat") (DIAGG . "aggcat") 
;(DIOPS- . "aggcat") (DIOPS . "aggcat") (PRQAGG . "aggcat") 
;(DQAGG . "aggcat") (QUAGG . "aggcat") (SKAGG . "aggcat") 
;(BGAGG- . "aggcat") (BGAGG . "aggcat") (CLAGG- . "aggcat") 
;(CLAGG . "aggcat") (HOAGG- . "aggcat") (HOAGG . "aggcat") 
;(AGG- . "aggcat") (AGG . "aggcat") (ACPLOT . "acplot") 
;(REALSOLV . "acplot")))

; in the old system each constructor (e.g. LIST) had one library directory
; (e.g. LIST.NRLIB). this directory contained a random access file called
; the index.KAF file. the interpreter needed this KAF file at runtime for
; two entries, the operationAlist and the ConstructorModemap.
; during the redesign for the new compiler we decided to merge all of
; these .NRLIB/index.KAF files into one database, CONSTRUCT.DAASE.
; requests to get information from this database are intended to be
; cached so that multiple references do not cause additional disk i/o.
; this database is left open at all times as it is used frequently by
; the interpreter. one minor complication is that newly compiled files
; need to override information that exists in this database. 
;   the design calls for constructing a random read (KAF format) file
; that is accessed by functions that cache their results. when the
; database is opened the list of constructor-index pairs is hashed
; by constructor name. a request for information about a constructor
; causes the information to replace the index in the hash table. since
; the index is a number and the data is a non-numeric sexpr there is
; no source of confusion about when the data needs to be read.
;
; the format of this new database is as follows:
;
;first entry:
; an integer giving the byte offset to the constructor alist
; at the bottom of the file
;second and subsequent entries (one per constructor)
; (operationAlist)
; (constructorModemap)
; (("operationAlist" 0 index) ("constructorModemap" 0 index))
;last entry: (pointed at by the first entry)
; an alist of (constructor . index) e.g.
;   ((PI . index) (NNI . index) ....) ...)
; this list is read at open time and hashed by the car of each item.


; this is a hashtable which is indexed by constructor name (eg PI) and
; returns the constructorModemap or the index into the construct.daase
; file that contains the constructorModemap for PI
(defvar *modemap-hash* nil "a hash table for caching constructorModemap data")

; this is a hashtable which is indexed by constructor name (eg PI) and
; returns the operationAlist or the index into the construct.daase
; file that contains the operationAlist for PI
(defvar *opalist-hash* nil "a hash table for caching operationAlist data")

; this a a stream for the construct.daase database. it is always open.
(defvar *construct-stream* nil "an open stream to the construct database")

; this is an initialization function for the constructor database
; it sets up 2 hash tables, opens the database and hashes the index values

(defun constructOpen ()
 "open the constructor database and hash the keys"
 (let (constructors pos)
  (setq *opalist-hash* (make-hash-table))
  (setq *modemap-hash* (make-hash-table))
  (setq *construct-stream*
   (open (concatenate 'string (|systemRootDirectory|) "/algebra/construct.daase")))
  (setq pos (read *construct-stream*))
  (file-position *construct-stream* pos)
  (setq constructors (read *construct-stream*))
  (dolist (item constructors)
   (setf (gethash (car item) *opalist-hash*) (cdr item))
   (setf (gethash (car item) *modemap-hash*) (cdr item)))))

; this is the function to call if you want to get the operationAlist
; property out of an NRLIB. this will read the property the first time
; and cache the result in a hash table

(defun getopalist (constructor)
 (let (data alist)
  (setq data (gethash constructor *opalist-hash*))
  (when (numberp data)
   (file-position *construct-stream* data)
   (setq alist (read *construct-stream*))
   (file-position *construct-stream*
     (third (assoc "operationAlist" alist :test #'string=)))
   (setq data (read *construct-stream*))
   (setf (gethash constructor *opalist-hash*) data))
  data))

; this is the function to call if you want to get the constructorModemap
; property out of an NRLIB. this will read the property the first time
; and cache the result in a hash table

(defun getmodemap (constructor)
 (let (data alist)
  (setq data (gethash constructor *modemap-hash*))
  (when (numberp data)
   (file-position *construct-stream* data)
   (setq alist (read *construct-stream*))
   (file-position *construct-stream* 
    (third (assoc "constructorModemap" alist :test #'string=)))
   (setq data (read *construct-stream*))
   (setf (gethash constructor *modemap-hash*) data))
  data))

; this is a utility function that walks over all of the libs given in
; the list (it should be a list of *SYMBOLS*, not strings, like
; '(pi nni ....) and constructs the contstruct.daase database
(defun constructdb (libs)
 (let (alist opalist modemap opalistpos modemappos index master masterpos pos)
 (labels (
  (name (x)
   (concatenate 'string (|systemRootDirectory|) "/algebra/" (string x) ".NRLIB/index.KAF")))
 (with-open-file (out "construct.daase" :direction :output)
  (print "                " out)
  (finish-output out)
  (dolist (lib libs)
   (print lib)
   (with-open-file (in (name lib))
    (file-position in (read in))
    (setq alist (read in))
    (setq pos (third (assoc "operationAlist" alist :test #'string=)))
    (if pos
     (progn
      (file-position in pos)
      (setq opalist (read in)))
     (setq opalist nil))
    (setq pos (third (assoc "constructorModemap" alist :test #'string=)))
    (if pos
     (progn
      (file-position in pos)
      (setq modemap (read in)))
     (setq modemap nil))
    (finish-output out)
    (setq opalistpos (file-position out))
    (print opalist out)
    (finish-output out)
    (setq modemappos (file-position out))
    (print modemap out)
    (finish-output out)
    (setq index (file-position out))
    (print (list (list "operationAlist" 0 opalistpos) 
                 (list "constructorModemap" 0 modemappos)) out)
    (finish-output out)
    (push (cons lib index) master)))
  (finish-output out)
  (setq masterpos (file-position out))
  (print master out)
  (finish-output out)
  (file-position out 0)
  (print masterpos out)
  (finish-output out)))))
