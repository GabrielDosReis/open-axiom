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

;; This file is a collection of utility functions that are useful
;; for system level work. A couple of the functions, 
;; `build-interpsys' interface to the src/interp/Makefile.

;; A second group of related functions allows us to rebuild portions
;; of the system from the command prompt. This varies from rebuilding
;; individual files to whole directories. The most complex functions
;; like `makespad' can rebuild the whole algebra tree.

;; A third group of related functions are used to set up the 
;; `autoload' mechanism. These enable whole subsystems to
;; be kept out of memory until they are used.

;; A fourth group of related functions are used to construct and
;; search Emacs TAGS files.

;; A fifth group of related functions are some translated boot
;; functions we need to define here so they work and are available
;; at load time.



(IMPORT-MODULE "vmlisp")
(import-module "parsing")

(in-package "BOOT")

(defun our-write-date (file) (and #+kcl (probe-file file)
                                  (file-write-date file)))

(defun make-directory (direc)
  (setq direc (namestring direc))
  (if (string= direc "")  (|systemRootDirectory|)
   (if (or (memq :unix *features*)
           (memq 'unix *features*))
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
  (let ((current-dir (get-current-directory)))
    (setq direc (namestring direc))
    (|ensureTrailingSlash|
     (if (string= direc "")  
	 current-dir
       (concat (|ensureTrailingSlash| current-dir) direc)))))

;; Various lisps use different ``extensions'' on the filename to indicate
;; that a file has been compiled. We set this variable correctly depending
;; on the system we are using.
(defvar *bin-path*
  #+kcl "o"
  #+lucid "bbin"
  #+symbolics "bin"
  #+cmulisp "fasl"
  #+:ccl "not done this way at all")

(defun load-directory (dir)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type *bin-path*))
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
   (let* ((bfile (make-pathname :type *bin-path* :defaults lfile))
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
  (obey (concat "cp " f "/code.o " (subseq f 0 (position #\. f)) ".o"))
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
          (binfile (make-pathname :type *bin-path*  :defaults bootfile))
          (bindate (our-write-date binfile))
          (bootdate (our-write-date   bootfile)))
       (if (and ldate bootdate (> ldate bootdate)) nil
           (if (and bindate bootdate (> bindate bootdate)) nil
               (progn (format t "translating ~a~%" bootfile)
                      (boot bootfile lfile) (list bootfile))))))


;; TAGS are useful for finding functions if you run Emacs. We have a
;; set of functions that construct TAGS files for Axiom.
(defun make-tags-file ()
  (|changeDirectory| "/tmp")
  (obey (concat "etags " (|makeAbsoluteFilename| "../../src/interp/*.lisp")))
  (spadtags-from-directory "../../src/interp" "boot")
  (obey "cat /tmp/boot.TAGS >> /tmp/TAGS"))

(defun spadtags-from-directory (dir type)
   (let* ((direc (make-directory dir))
          (pattern (make-pathname :directory (pathname-directory direc)
                                  :name :wild :type type))
          (files (directory pattern)))
     (with-open-file
      (tagstream (concatenate 'string "/tmp/" type ".TAGS") :direction :output
                 :if-exists :supersede :if-does-not-exist :create)
      (dolist (file files (namestring tagstream))
              (print (list "processing:" file))
              (write-char #\page tagstream)
              (terpri tagstream)
              (write-string (namestring file) tagstream)
              (write-char #\, tagstream)
              (princ (spadtags-from-file file) tagstream)
              (terpri tagstream)
              (with-open-file (stream "/tmp/*TAGS")
                 (do ((line (read-line stream nil nil)
                            (read-line stream nil nil)))
                     ((null line) nil)
                     (write-line line tagstream)))))))

(defun spadtags-from-file (spadfile)
  (with-open-file (tagstream "/tmp/*TAGS" :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (stream spadfile)
       (do ((char-count 0 (file-position stream))
            (line (read-line stream nil nil) (read-line stream nil nil))
            (line-count 1 (1+ line-count)))
           ((null line) (file-length tagstream))
           (if (/= (length line) 0)
               (let ((firstchar (elt line 0)) (end nil)
                     (len (length line)))
                 (cond ((member firstchar '(#\space #\{ #\} #\tab )
                                :test #'char= ) "skip")
                       ((string= line ")abb" :end1 (min 4 len))
                        (setq end (position #\space line :from-end t
                                            :test-not #'eql)
                              end (and end (position #\space line :from-end t
                                                     :end end)))
                        (write-tag-line line tagstream end
                                        line-count char-count))
                       ((char= firstchar #\)) "skip")
                       ((and (> len 1) (string= line "--" :end1 2)) "skip")
                       ((and (> len 1) (string= line "++" :end1 2)) "skip")
                       ((search "==>" line) "skip")
                       ((and (setq end (position #\space line)
                                   end (or (position #\( line :end end) end)
                                   end (or (position #\: line :end end) end)
                                   end (or (position #\[ line :end end) end))
                             (equal end 0)) "skip")
                       ((position #\] line :end end) "skip")
                       ((string= line "SETANDFILEQ" :end1 end) "skip")
                       ((string= line "EVALANDFILEACTQ" :end1 end) "skip")
                       (t (write-tag-line line tagstream
                                          (if (numberp end) (+ end 1) end)
                                          line-count char-count))  )))))))

(defun write-tag-line (line tagstream endcol line-count char-count)
  (write-string line tagstream :end endcol)
  (write-char #\rubout tagstream)
  (princ line-count tagstream)
  (write-char #\, tagstream)
  (princ char-count tagstream)
  (terpri tagstream))

(defun blankcharp (c) (char= c #\Space))

(defun findtag (tag &optional (tagfile (concat (|systemRootDirectory|) "/../../src/interp/TAGS")) )
  ;; tag is an identifier
    (with-open-file (tagstream tagfile)
        (do ((tagline (read-line tagstream nil nil)
                      (read-line tagstream nil nil))
             (*package* (symbol-package tag))
             (sourcefile)
             (stringtag (string tag))
             (pos)
             (tpos)
             (type))
            ((null tagline) ())
            (cond ((char= (char tagline 0) #\Page)
                   (setq tagline (read-line tagstream nil nil))
                   (setq sourcefile (subseq tagline 0
                                            (position #\, tagline)))
                   (setq type (pathname-type sourcefile)))
                  ((string= type "lisp")
                   (if (match-lisp-tag tag tagline)
                       (return (cons sourcefile tagline))))
                  ((> (mismatch ")abb" tagline) 3)
                   (setq pos (position #\Space tagline :start 3))
                   (setq pos (position-if-not #'blankcharp tagline
                                              :start pos))
                   (setq pos (position #\Space tagline :start pos))
                   (setq pos (position-if-not #'blankcharp tagline
                                              :start pos))
                   (setq tpos (mismatch stringtag tagline :start2 pos))
                   (if (and (= tpos (length (string tag)))
                            (member (char tagline (+ pos tpos)) '(#\Space #\Rubout)))
                       (return (cons sourcefile tagline))))
                  ((setq pos (mismatch stringtag tagline))
                   (if (and (= pos (length stringtag))
                            (> (length tagline) pos)
                            (member (char tagline pos)
                                    '( #\Space #\( #\:) ))
                       (return (cons sourcefile tagline))))))))

(defun match-lisp-tag (tag tagline &optional (prefix nil)
                           &aux (stringtag (string tag)) pos tpos)
  (when (and (if prefix
                 (= (mismatch prefix tagline :test #'char-equal)
                    (length prefix))
               t)
             (numberp (setq pos (position #\Space tagline)))
             (numberp (setq pos (position-if-not #'blankcharp tagline
                                                 :start pos))))
        (if (char= (char tagline pos) #\') (incf pos))
        (if (member (char tagline pos) '( #\\ #\|))
            (setq tpos (1+ pos))
          (setq tpos pos))
        (and (= (mismatch stringtag tagline :start2 tpos :test #'char-equal)
                (length stringtag))
             (eq tag (read-from-string tagline nil nil :start pos))) ))

;; Translate a single boot file to common lisp, compile it 
;; and load it.
(defun compile-boot-file (file)
  "compile and load a boot file"
  (boot (concat file ".boot") (concat file ".lisp"))
#+:AKCL
  (compile-file (concat file ".lisp"))
#+:AKCL
  (load (concat file "." *bin-path*))
#+:CCL
  (load (concat file ".lisp"))
)


;; Translate a single boot file to common lisp
(defun translate (file) ;; translates a single boot file
#+:CCL
  (setq *package* (find-package "BOOT"))
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
    "/doc/spadhelp/" ))

;; The relative directory list specifies how to find the algebra
;; directory from the current {\bf AXIOM} shell variable.
(defvar $relative-library-directory-list '("/algebra/"))

;; This is a little used subsystem to generate {\bf ALDOR} code
;; from {\bf Spad} code. Frankly, I'd be amazed if it worked.
(defparameter translate-functions '(
;; .spad to .as translator, in particular
;;      loadtranslate
        |spad2AsTranslatorAutoloadOnceTrigger|
        ))

;; This is part of the {\bf ALDOR subsystem}. These will be loaded
;; if you compile a {\bf .as} file rather than a {\bf .spad} file.
;; {\bf ALDOR} is an external compiler that gets automatically called
;; if the file extension is {\bf .as}.
(defparameter asauto-functions '(
        loadas
;;      |as|                         ;; now in as.boot
;;      |astran|                     ;; now in as.boot
        |spad2AxTranslatorAutoloadOnceTrigger|
        |sourceFilesToAxcliqueAxFile|
        |sourceFilesToAxFile|
        |setExtendedDomains|
        |makeAxFile|
        |makeAxcliqueAxFile|
        |nrlibsToAxFile|
        |attributesToAxFile| ))

;; These are some {\bf debugging} functions that I use. I can't imagine
;; why you might autoload them but they don't need to be in a running
;; system.
(defparameter debug-functions '(
        loaddebug
        |showSummary|
        |showPredicates|
        |showAttributes|
        |showFrom|
        |showImp|))

;; This function is called by {\bf build-interpsys}. It takes two lists.
;; The first is a list of functions that need to be used as 
;; ``autoload triggers''. The second is a list of files to load if one
;; of the trigger functions is called. At system build time each of the
;; functions in the first list is set up to load every file in the second
;; list. In this way we will automatically load a whole subsystem if we
;; touch any function in that subsystem. We call a helper function
;; called {\bf setBootAutoLoadProperty} to set up the autoload trigger.
;; This helper function is listed below.
(defun |setBootAutloadProperties| (fun-list file-list)
#+:AKCL
  (mapc #'(lambda (fun) (|setBootAutoLoadProperty| fun file-list)) fun-list)
#+:CCL
  (mapc #'(lambda (fun) (lisp::set-autoload fun file-list)) fun-list)
)


;; This function knows where the {\bf autoload} subdirectory lives.
;; It is called by {\bf mkBootAutoLoad} above to find the necessary
;; files.
(defun boot-load (file)
  (let ((name (concat (|systemRootDirectory|)
                      "/autoload/" 
                      (pathname-name file))))
    (if |$printLoadMsgs|
        (format t "   Loading ~A.~%" name))
    (load name)))

;; This is a helper function to set up the autoload trigger. It sets
;; the function cell of each symbol to {\bf mkBootAutoLoad} which is
;; listed below. 
(defun |setBootAutoLoadProperty| (func file-list)
  (setf (symbol-function func) (|mkBootAutoLoad| func file-list)) )

;; This is how the autoload magic happens. Every function named in the
;; autoload lists is actually just another name for this function. When
;; the named function is called we call {\bf boot-load} on all of the
;; files in the subsystem. This overwrites all of the autoload triggers.
;; We then look up the new (real) function definition and call it again
;; with the real arguments. Thus the subsystem loads and the original
;; call succeeds.
(defun |mkBootAutoLoad| (fn file-list)
   (function (lambda (&rest args)
                 (mapc #'boot-load file-list)
                 (unless (string= (subseq (string fn) 0 4) "LOAD")
                  (apply (symbol-function fn) args)))))

;############################################################################
;# autoload dependencies
;#
;# if you are adding a file which is to be autoloaded the following step
;# information is useful:
;#  there are 2 cases:
;#   1) adding files to currently autoloaded parts
;#	(as of 2/92: browser old parser and old compiler)
;#   2) adding new files
;#   case 1:
;#     a) you have to add the file to the list of files currently there
;#	  (e.g. see BROBJS above)
;#     b) add an autolaod rule
;#	  (e.g. ${AUTO}/parsing.${O}: ${OUT}/parsing.${O})
;#     c) edit util.lisp to add the 'external' function (those that
;#	  should trigger the autoload
;#   case 2:
;#     build-interpsys (in util.lisp) needs an extra argument for the
;#     new autoload things and several functions in util.lisp need hacking.
;############################################################################

;; The `build-interpsys' function takes a list of files to load
;; into the image (`load-files'). It also takes several lists of files, 
;; one for each subsystem which will be autoloaded. Autoloading is explained
;; below. This function is called in the src/interp/Makefile. 

;; This function calls `reroot' to set up pathnames we need. Next
;; it sets up the lisp system memory (at present only for AKCL/GCL). Next
;; it loads all of the named files, resets a few global state variables,
;; loads the databases, sets up autoload triggers and clears out hash tables.
;; After this function is called the image is clean and can be saved.

(defun build-interpsys (load-files translate-files asauto-files)
  (reroot)
  (mapcar #'|AxiomCore|::|importModule| load-files)
  (|resetWorkspaceVariables|)
  (|AxiomCore|::|%sysInit|)
  (|initHist|)
  (|initNewWorld|)
  (compressopen)
  (interpopen)
  (create-initializers)
  (|start| :fin)
  (setq *load-verbose* nil)
  (|setBootAutloadProperties| translate-functions translate-files)
  (|setBootAutloadProperties| asauto-functions asauto-files)
  (setf (symbol-function '|addConsDB|) #'identity)
  (resethashtables) ; the databases into core, then close the streams
 )


(DEFUN |string2SpadTree| (LINE)
  (DECLARE (SPECIAL LINE))
  (if (and (> (LENGTH LINE) 0) (EQ (CHAR LINE 0) #\) ))
    (|processSynonyms|))
  (ioclear)
  (LET* ((BOOT-LINE-STACK (LIST (CONS 1 LINE)))
     ($SPAD T)
     (XTOKENREADER 'GET-BOOT-TOKEN)
     (LINE-HANDLER 'NEXT-BOOT-LINE)
     (PARSEOUT (PROG2 (|PARSE-NewExpr|) (POP-STACK-1))))
    (DECLARE (SPECIAL BOOT-LINE-STACK $SPAD XTOKENREADER LINE-HANDLER))
    PARSEOUT))


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

(defun |tr| (fn)
  (|spad2AsTranslatorAutoloadOnceTrigger|)
  (|convertSpadFile| fn) )


;; Make will not compare dates across directories.
;; Rather than copy all of the code.lsp files to the MNT directory
;; we run this function to compile the files that are out of date
;; this function assumes that the shell variables INT and MNT are set.
;; Also of note: on the rt some files (those in the nooptimize list)
;; need to be compiled without optimize due to compiler bugs
(defun makelib (mid out stype btype)
 "iterate over the NRLIBs, compiling ones that are out of date.
  mid is the directory containing code.lsp
  out is the directory containing code.o"
 (let (libs lspdate odate nooptimize (alphabet #\space))
#+(and :akcl :rt)
  (setq nooptimize '("FFCAT-.NRLIB" "CHVAR.NRLIB" "PFO.NRLIB" "SUP.NRLIB"
                     "INTG0.NRLIB" "FSPRMELT.NRLIB" "VECTOR.NRLIB"
                     "EUCDOM-.NRLIB"))
  (if (and mid out)
   (format t "doing directory on ~s...~%" (concatenate 'string mid "/*"))
   (error "makelib:MID=~a OUT=~a~% these are not set properly~%" mid out))
#+:akcl (compiler::emit-fn nil)
  (|changeDirectory| mid)
  (setq libs (directory "*.NRLIB"))
  (unless libs
   (format t "makelib:directory of ~a returned NIL~%" mid)
   (bye -1))
  (princ "checking ")
  (dolist (lib libs)
   (unless (char= (schar (pathname-name lib) 0) alphabet)
    (setq alphabet (schar (pathname-name lib) 0))
    (princ alphabet)
    (finish-output))
   (let (dotlsp doto mntlib intkaf mntkaf intkafdate mntkafdate)
    (setq dotlsp
      (concatenate 'string mid "/" (file-namestring lib) "/code." stype))
    (setq doto
      (concatenate 'string out "/" (pathname-name lib) ".NRLIB/code." btype))
    (setq mntlib
      (concatenate 'string out "/" (pathname-name lib) ".NRLIB"))
    (setq intkaf
      (concatenate 'string mid "/" (file-namestring lib) "/index.KAF*"))
    (setq mntkaf
      (concatenate 'string out "/" (pathname-name lib) ".NRLIB/index.KAF*"))
    (unless (probe-file mntlib)
     (format t "creating directory ~a~%" mntlib)
     (obey (concatenate 'string "cp -pr " (namestring lib) " " out))
     (when (probe-file (concatenate 'string mntlib "/code." stype))
      (delete-file  (concatenate 'string mntlib "/code." stype))))
    (setq intkafdate (and (probe-file intkaf) (file-write-date intkaf)))
    (setq mntkafdate (and (probe-file mntkaf) (file-write-date mntkaf)))
    (when intkafdate
     (unless (and mntkafdate (> mntkafdate intkafdate))
      (format t "~&copying ~s to ~s" intkaf mntkaf)
      (obey
       (concatenate 'string "cp " 
         (namestring intkaf) " " (namestring mntkaf)))))
    (setq lspdate (and (probe-file dotlsp) (file-write-date dotlsp)))
    (setq odate (and (probe-file doto) (file-write-date doto)))
    (when lspdate
     (unless (and odate (> odate lspdate))
#+(and :akcl :rt)
      (if (member (file-namestring lib) nooptimize :test #'string=)
       (setq compiler::*speed* 0)
       (setq compiler::*speed* 3))
      (compile-lib-file dotlsp :output-file doto)))))))


;; Make will not compare dates across directories.
;; In particular, it cannot compare the algebra files because there
;; is a one-to-many correspondence. This function will walk over
;; all of the algebra NRLIB files and find all of the spad files
;; that are out of date and need to be recompiled. This function
;; creates a file "/tmp/compile.input" to be used later in the
;; makefile.
;; Note that the file /tmp/compile.input is not currently used
;; as algebra source recompiles are not necessarily something
;; we want done automatically. Nevertheless, in the quest for
;; quality we check anyway.
(defun makespad (src mid stype)
 "iterate over the spad files, compiling ones that are out of date.
  src is the directory containing .spad
  mid is the directory containing code.lsp
  out is the directory containing code.o"
 (let (mntlibs spadwork (alphabet #\space))
  (labels (
   (findsrc (mid libname)
    "return a string name of the source file given the library file
     name (eg PI) as a string"
    (let (kaffile index alist)
     (setq kaffile 
      (concatenate 'string mid "/" libname ".NRLIB/index.KAF*"))
     (with-open-file (kaf kaffile)
      (setq index (read kaf))
      (file-position kaf index)
      (setq alist (read kaf))
     (setq index (third (assoc "sourceFile" alist :test #'string=)))
      (file-position kaf index)
     (pathname-name (pathname (read kaf index)))))))
  (format t "makespad:src=~s mid=~s stype=~s~%" src mid stype)
  (if (and src mid)
   (format t "doing directory on ~s...~%" (concatenate 'string src "/*"))
   (error "makespad:SRC=~a MID=~a not set properly~%" src mid))
  (|changeDirectory| mid)
  (setq mntlibs (directory "*.NRLIB"))
  (unless mntlibs
   (format t "makespad:directory of ~a returned NIL~%" src)
   (bye 1))
  (princ "checking ")
  (dolist (lib mntlibs)
   (unless (char= (schar (pathname-name lib) 0) alphabet)
    (setq alphabet (schar (pathname-name lib) 0))
    (princ alphabet)
    (finish-output))
   (let (spad spaddate lsp lspdate)
    (setq spad
      (concatenate 'string src "/" (findsrc mid (pathname-name lib)) ".spad"))
    (setq spaddate
      (and (probe-file spad) (file-write-date spad)))
    (setq lsp
      (concatenate 'string mid "/" (pathname-name lib) ".NRLIB/code." stype))
    (setq lspdate
      (and (probe-file lsp) (file-write-date lsp)))
    (cond
     ((and spaddate lspdate (<= spaddate lspdate)))
     ((and spaddate lspdate (> spaddate lspdate))
       (setq spadwork (adjoin spad spadwork :test #'string=)))
     ((and spaddate (not lspdate)) 
       (setq spadwork (adjoin spad spadwork :test #'string=)))
     ((and (not spaddate) lspdate)
       (format t "makespad:missing spad file ~a for lisp file ~a~%" spad lsp))
     ((and (not spaddate) (not lspdate))
       (format t "makespad:NRLIB ~a exist but is spad ~a and lsp ~a don't~%"
         lib spad lsp)))))
   (with-open-file (tmp "/tmp/compile.input" :direction :output)
    (dolist (spad spadwork)
     (format t "~a is out of date~%" spad)
     (format tmp ")co ~a~%" spad))))))


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
                (search nrlib expr :test #'string=)
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
     (setq names (nconc names short))
     (setq longnames (nconc longnames long)))
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


