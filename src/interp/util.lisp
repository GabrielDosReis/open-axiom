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

;; This file is a collection of utility functions that are useful
;; for system level work. A couple of the functions, 
;; `build-interpsys' interface to the src/interp/Makefile.

;; A second group of related functions allows us to rebuild portions
;; of the system from the command prompt. This varies from rebuilding
;; individual files to whole directories. The most complex functions
;; like `makespad' can rebuild the whole algebra tree.

;; A fourth group of related functions are used to construct and
;; search Emacs TAGS files.

;; A fifth group of related functions are some translated boot
;; functions we need to define here so they work and are available
;; at load time.



(IMPORT-MODULE "vmlisp")
(import-module "lexing")

(in-package "BOOT")

(defun our-write-date (file) (and #+kcl (probe-file file)
                                  (file-write-date file)))

(defun make-directory (direc)
  (setq direc (namestring direc))
  (if (string= direc "")  (|systemRootDirectory|)
   (if (or (|symbolMember?| :unix *features*)
           (|symbolMember?| 'unix *features*))
    (progn
      (if (char/= (char direc 0) #\/)
          (setq direc (concat (|systemRootDirectory|) "/" direc)))
      (if (char/= (char direc (1- (length direc))) #\/)
          (setq direc (concat direc "/")))
      direc)
    (progn ;; Assume Windows conventions
      (if (not (or (char= (char direc 0) #\/)
                   (char= (char direc 0) #\\)
                   (find #\: direc)))
          (setq direc (concat (|systemRootDirectory|) "\\" direc)))
      (if (not (or (char= (char direc (1- (length direc))) #\/)
                   (char= (char direc (1- (length direc))) #\\ )))
          (setq direc (concat direc "\\")))
      direc))))

(defun interp-make-directory (direc)
  (let ((current-dir (|getWorkingDirectory|)))
    (setq direc (namestring direc))
    (|ensureTrailingSlash|
     (if (string= direc "")  
	 current-dir
       (concat (|ensureTrailingSlash| current-dir) direc)))))

(defun load-directory (dir)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type |$faslType|))
          (files (directory pattern)))
      (mapcar #'load files)))

(defun compspadfiles (filelist ;; should be a file containing files to compile
                      &optional (*default-pathname-defaults*
                                 (pathname (concat (|systemRootDirectory|)
                                                   "nalgebra/"))))
   (with-open-file (stream filelist)
        (do ((fname (read-line stream nil nil) (read-line stream nil nil)))
            ((null fname) 'done)
          (setq fname (string-right-trim " *" fname))
          (when (not (equal (elt fname 0) #\*))
              (spad fname (concat (pathname-name fname) ".out"))))))

(defun recompile-all-algebra-files (dir) ;; a desperation measure
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type "spad"))
          (files (directory pattern))
          (*default-pathname-defaults* (pathname direc)))
     (mapcar
       #'(lambda (fname) (spad fname (concat (pathname-name fname) ".out")))
       files)))

;; This function will compile any lisp code that has changed in a directory.
(defun recompile-directory (dir)
  (let* ((direc (make-directory dir))
         (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type "lisp"))
          (files (directory pattern)))
      (mapcan #'recompile-file-if-necessary files)))

;; This is a helper function that checks the time stamp between
;; the given file and its compiled binary. If the file has changed
;; since it was last compiled this function will recompile it.
(defun recompile-file-if-necessary (lfile)
   (let* ((bfile (make-pathname :type |$faslType| :defaults lfile))
          (bdate (our-write-date bfile))
          (ldate (our-write-date lfile)))
       (if (and bdate ldate (> bdate ldate)) nil
           (progn
             (format t "compiling ~a~%" lfile)
             (compile-file lfile)
             (list bfile)))))

;; Force recompilation of all lisp files in a directory.
(defun recompile-all-files (dir)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type "lisp"))
          (files (directory pattern)))
     (mapcar #'compile-file files)))


;; Recompile library lisp code if necessary.
(defun recompile-lib-directory (dir)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type "NRLIB"))
          (files (directory pattern)))
      (mapcan #'recompile-NRLIB-if-necessary files)))

(defun recompile-all-libs (dir)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type "NRLIB"))
          (files (directory pattern)))
     (mapcar
       #'(lambda (lib) (compile-lib-file (concat (namestring lib) "/code.lsp")))
       files)))

;; Recompile a single library's lisp file if it is out of date.
;; The {\bf recompile-lib-file-if-necessary} is defined in nlib.lisp.
(defun recompile-NRLIB-if-necessary (lib)
  (recompile-lib-file-if-necessary (concat (namestring lib) "/code.lsp"))
  (lift-NRLIB-name (namestring lib)))


;; We used to use FOO.NRLIB/code.o files for algebra. However there
;; was no need for this additional level of indirection since the rest
;; of the information in an NRLIB is now kept in the daase files. Thus
;; we lift the FOO.NRLIB/code.o to FOO.o in the final system.
(defun lift-NRLIB-name (f)
  (|runCommand| (concat "cp " f "/code.o " (subseq f 0 (|findChar| #\. f)) ".o"))
  nil)

;; Translate a directory of boot code to common lisp if the boot code
;; is newer.
(defun retranslate-directory (dir)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type "boot"))
          (files (directory pattern)))
      (mapcan #'retranslate-file-if-necessary files)))


;; Retranslate a single boot file if it has been changed.
(defun retranslate-file-if-necessary (bootfile)
   (let* ((lfile (make-pathname :type "lisp" :defaults bootfile))
          (ldate (our-write-date lfile))
          (binfile (make-pathname :type |$faslType|  :defaults bootfile))
          (bindate (our-write-date binfile))
          (bootdate (our-write-date   bootfile)))
       (if (and ldate bootdate (> ldate bootdate)) nil
           (if (and bindate bootdate (> bindate bootdate)) nil
               (progn (format t "translating ~a~%" bootfile)
                      (boot bootfile lfile) (list bootfile))))))


;; Translate a single boot file to common lisp, compile it 
;; and load it.
(defun compile-boot-file (file)
  "compile and load a boot file"
  (boot (concat file ".boot") (concat file ".lisp"))
  (compile-file (concat file ".lisp"))
  (load (concat file "." |$faslType|))
)


;; Translate a single boot file to common lisp
(defun translate (file) ;; translates a single boot file
#+:AKCL
  (in-package "BOOT")
  (let (*print-level* *print-length* (fn (pathname-name file))
        (bootfile  (merge-pathnames file (concat (|systemRootDirectory|) "nboot/.boot"))))
    (boot bootfile (make-pathname :type "lisp" :defaults bootfile))))


;; Translate a list of boot files to common lisp.
(defun translist (fns)
  (mapcar #'(lambda (f) (format t "translating ~a~%" (concat f ".boot"))
                        (translate f))
          fns))


;; The relative directory list specifies a search path for files 
;; for the current directory structure. It has been changed from the
;; NAG distribution back to the original form. 
(defvar $relative-directory-list
  '("/../../src/input/"
    "/share/msgs/"
    "/../../src/algebra/"
    "/../../src/interp/"  ; for boot and lisp  files (helps fd)
    "/doc/help/" ))

;; The relative directory list specifies how to find the algebra
;; directory from the current {\bf AXIOM} shell variable.
(defvar $relative-library-directory-list '("/algebra/"))

;; This function calls `reroot' to set up pathnames we need. Next
;; it sets up the lisp system memory (at present only for AKCL/GCL). Next
;; it loads all of the named files, resets a few global state variables,
;; loads the databases.
;; After this function is called the image is clean and can be saved.

(defun build-interpsys ()
  (reroot)
  (|loadDelayedFFI|)
  (|resetWorkspaceVariables|)
  (|AxiomCore|::|%sysInit|)
  (|buildHtMacroTable|)
  (|initHist|)
  (|initNewWorld|)
  (interpopen)
  (|start| :fin)
  (setq *load-verbose* nil)
  (|fillDatabasesInCore|) ; the databases into core, then close the streams
  (|closeAllDatabaseStreams|)
 )

;; the following are for conditional reading
#+:ieee-floating-point (defparameter $ieee t)
#-:ieee-floating-point (defparameter $ieee nil)
(defparameter |$opSysName| '"shell")

(defconstant |$machineType| (machine-type))
; spad-clear-input patches around fact that akcl clear-input leaves newlines chars
(defun spad-clear-input (st) (clear-input st) (if (listen st) (read-char st)))

;; We need a way of distinguishing different versions of the system.
;; There used to be a way to touch the src/timestamp file whenever
;; you checked in a change to the change control subsystem. 
;; During make PART=interp (the default for make) we set timestamp
;; to the filename of this timestamp file. This function converts it
;; to a luser readable string and sets the *yearweek* variable.
;; The result of this function is a string that is printed as a banner 
;; when Axiom starts. The actual printing is done by the function
;; [[spadStartUpMsgs]] in [[src/interp/msgdb.boot]]. It uses a 
;; format string from the file [[src/doc/msgs/s2-us.msgs]].
(defun yearweek ()
 "set *yearweek* to the current time string for the version banner"
  (declare (special timestamp) (special *yearweek*))
  (if (and (boundp 'timestamp) (probe-file timestamp))
      (let (sec min hour date month year day dayvec monvec)
        (setq dayvec '("Monday" "Tuesday" "Wednesday" "Thursday"
                       "Friday" "Saturday" "Sunday"))
        (setq monvec '("January" "February" "March" "April" "May" "June"
                       "July" "August" "September" "October" "November"
                       "December"))
        (multiple-value-setq (sec min hour date month year day)
                             (decode-universal-time
                              (file-write-date timestamp)))
        (setq *yearweek*
          (copy-seq
              (format nil "~a ~a ~d, ~d at ~2,'0d:~2,'0d:~2,'0d "
                      (elt dayvec day)
                      (elt monvec (1- month)) date year hour min sec))))
      (setq *yearweek* "no timestamp")))

(defun sourcepath (f)
 "find the sourcefile in the system directories"
 (let (axiom algebra)
  (setq axiom (|systemRootDirectory|))
  (setq algebra (concatenate 'string axiom "/../../src/algebra/" f ".spad"))
  (cond
   ((probe-file algebra) algebra)
   ('else nil))))

(defun srcabbrevs (sourcefile)
 "read spad source files and return the constructor names and abbrevs"
 (let (expr point mark names longnames)
  (catch 'done
   (with-open-file (in sourcefile)
    (loop
     (setq expr (read-line in nil 'done))
     (when (eq expr 'done) (throw 'done nil))
     (when (and (> (length expr) 4) (string= ")abb" (subseq expr 0 4)))
      (setq expr (string-right-trim '(#\space #\tab) expr))
      (setq point (position #\space expr :from-end t :test #'char=))
      (push (subseq expr (1+ point)) longnames)
      (setq expr (string-right-trim '(#\space #\tab)
                       (subseq expr 0 point)))
      (setq mark (position #\space expr  :from-end t))
      (push (subseq expr (1+ mark)) names)))))
  (values longnames names)))


#+(and :AKCL (not (or :dos :win32)))
(in-package "COMPILER")
#+(and :AKCL (not (or :dos :win32)))
(defun gazonk-name ( &aux tem)
 "return the name of the intermediate compiler file"
 (dotimes (i 1000)
  (setq tem (merge-pathnames (format nil "/tmp/gazonk~d.lsp" i)))
  (unless (probe-file tem)
    (return-from gazonk-name (pathname tem))))
 (error "1000 gazonk names used already!"))

(in-package "BOOT")

;; We need to ensure that the INTERP.EXPOSED list, which is a list
;; of the exposed constructors, is consistent with the actual libraries.
(defun libcheck (int)
 "check that INTERP.EXPOSED and NRLIBs are consistent"
 (let (interp nrlibs abbrevs srcabbrevs srcconstructors constructors)
 (labels (
  (CONSTRUCTORNAME (nrlib)
   "find the long name of a constructor given an abbreviation string"
   (let (file sourcefile name)
    (setq file (findsrc nrlib))
    (setq sourcefile
      (concatenate 'string int "/" file ".spad"))
    (when (and file (probe-file sourcefile))
     (setq name (searchsource sourcefile nrlib)))))
  (NOCAT (longnames)
   "remove the categories from the list of long names"
   (remove-if 
    #'(lambda (x) 
       (let ((c (schar x (1- (length x)))))
        (or (char= c #\&) (char= c #\-)))) longnames))
  (FINDSRC (libname)
   "return a string name of the source file given the library file
    name (eg PI) as a string"
   (let (kaffile index alist result)
    (setq kaffile 
     (concatenate 'string int "/" libname ".NRLIB/index.KAF*"))
    (if (probe-file kaffile)
     (with-open-file (kaf kaffile)
      (setq index (read kaf))
      (file-position kaf index)
      (setq alist (read kaf))
     (setq index (third (assoc "sourceFile" alist :test #'string=)))
      (file-position kaf index)
     (setq result (pathname-name (pathname (read kaf index))))))
     (format t "~a does not exist~%" kaffile)
    result))
  (READINTERP ()
   "read INTERP.EXPOSED and return a sorted abbreviation list"
   (let (expr names longnames)
    (with-open-file (in (concatenate 'string int "/INTERP.EXPOSED"))
     (catch 'eof
      (loop
       (setq expr (read-line in nil 'eof))
       (when (eq expr 'eof) (throw 'eof nil))
       (when
        (and
         (> (length expr) 58)
         (char= (schar expr 0) #\space) 
         (not (char= (schar expr 8) #\space)))
         (push (string-trim '(#\space) (subseq expr 8 57)) longnames)
         (push (string-right-trim '(#\space) (subseq expr 58)) names)))))
    (setq longnames (sort longnames #'string<))
    (setq names (sort names #'string<))
    (values names longnames)))
  (READLIBS (algebra)
   "read the NRLIB directory and return a sorted abbreviation list"
   (let (libs nrlibs)
      (|changeDirectory| algebra)
    (setq nrlibs (directory "*.NRLIB"))
    (unless nrlibs
     (error "libcheck: (directory ~s) returned NIL~%" 
         (concatenate 'string algebra "/*.NRLIB")))
    (dolist (lib nrlibs)
     (push (pathname-name lib) libs))
    (sort libs #'string<)))
  (SEARCHSOURCE (sourcefile nrlib)
   "search a sourcefile for the long constructor name of the nrlib string"
   (let (in expr start)
    (setq nrlib (concatenate 'string " " nrlib " "))
    (catch 'done
     (with-open-file (in sourcefile)
      (loop
       (setq expr (read-line in nil 'done))
       (when (eq expr 'done) (throw 'done nil))
       (when (and (> (length expr) 4)
                (string= ")abb" (subseq expr 0 4))
                (|findString| nrlib expr)
                (setq start (position #\space expr :from-end t :test #'char=)))
        (throw 'done (string-trim '(#\space) (subseq expr start)))))))))
  (SRCABBREVS (sourcefile)
   (let (in expr start end names longnames point mark)
    (catch 'done
     (with-open-file (in sourcefile)
      (loop
       (setq expr (read-line in nil 'done))
       (when (eq expr 'done) (throw 'done nil))
       (when (and (> (length expr) 4)
             (string= ")abb" (subseq expr 0 4)))
        (setq point (position #\space expr :from-end t :test #'char=))
        (push (string-trim '(#\space) (subseq expr point)) longnames)
        (setq mark
         (position #\space 
          (string-right-trim '(#\space)
           (subseq expr 0 (1- point))) :from-end t))
        (push (string-trim '(#\space) (subseq expr mark point)) names)))))
    (values names longnames)))
  (SRCSCAN ()
   (let (longnames names spads long short)
     (|changeDirectory| int)
    (setq spads (directory "*.spad"))
    (dolist (spad spads)
     (multiple-value-setq (short long) (srcabbrevs spad))
     (setq names (|append!| names short))
     (setq longnames (|append!| longnames long)))
    (setq names (sort names #'string<))
    (setq longnames (sort longnames #'string<))
    (values names longnames))))
  (multiple-value-setq (abbrevs constructors) (readinterp))
  (setq nrlibs (readlibs int))
  (dolist (lib (set-difference nrlibs abbrevs :test #'string=))
    (format t "libcheck:~a/~a.NRLIB is not in INTERP.EXPOSED~%" int lib))
  (dolist (expose (set-difference abbrevs nrlibs :test #'string=))
    (format t "libcheck:~a is in INTERP.EXPOSED with no NRLIB~%" expose))
  (multiple-value-setq (srcabbrevs srcconstructors) (srcscan))
  (setq abbrevs (nocat abbrevs))
  (setq constructors (nocat constructors))
  (dolist (item (set-difference srcabbrevs abbrevs :test #'string=))
    (format t "libcheck:~a is in ~a but not in INTERP.EXPOSED~%" item
     (findsrc item)))
  (dolist (item (set-difference abbrevs srcabbrevs :test #'string=))
    (format t "libcheck:~a is in INTERP.EXPOSED but has no spad sourcfile~%"
      item))
  (dolist (item (set-difference srcconstructors constructors :test #'string=))
    (format t "libcheck:~a is not in INTERP.EXPOSED~%" item))
  (dolist (item (set-difference constructors srcconstructors :test #'string=))
    (format t "libcheck:~a has no spad source file~%" item)))))


