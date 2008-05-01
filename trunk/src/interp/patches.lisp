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

(import-module "macros")
(import-module "g-timer")
(import-module "sys-driver")
(in-package "BOOT")
;;patches for now

;; browser stuff:
;; gdr NOTES: it is WRONG to test for platforms, when in fact
;; gdr NOTES: one should test for functionalities.
#+:UNIX (defvar |$standard| 't)
#-:UNIX (defvar |$standard| 'nil)
#+(or :UNIX :winnt) (defvar |$saturn| 'nil)
#-(or :UNIX :winnt) (defvar |$saturn| 't)

(defun CATCHALL (a &rest b) a) ;; not correct but ok for now
(defvar |$demoFlag| nil)

(define-function '|construct| #'list) ;; NEEDED , SPAD-COMPILER generated Lisp code
(define-function '|COMP,TRAN| #'comp-tran) ;called by |compWithMappingMode|

(define-function '|spadHash| #'sxhash)

(defun |mkAutoLoad| (fn cname)
   (function (lambda (&rest args)
                 (|autoLoad| fn cname)
                 (apply cname args))))

(defmacro dribinit (streamvar)
  `(if (is-console ,streamvar)
       (setq ,streamvar *terminal-io*)))

;; The function top-level is the very root of the normal invocation
;; history stack. Control will pass to the restart function which is 
;; also in this file.
;; For some unknown reason toplevel was redefined to incorrectly
;; call lisp::unwind whereas it is defined (in this file) to be
;; interned in the boot package. We've returned toplevel to its
;; previous definition.
(defun toplevel (&rest foo) (throw '|top_level| '|restart|))
;;(defun toplevel (&rest foo) (lisp::unwind))

(define-function 'top-level #'toplevel)
(define-function 'unwind #'|spadThrow|)
(define-function 'resume #'|spadThrow|)

(DEFUN BUMPCOMPERRORCOUNT () ())

(define-function '|isBpiOrLambda| #'FBOUNDP)
;;(defun |isSharpVar| (x) (and (identp x) (char= (elt (pname x) 0) #\#)))

(defvar |$internalHistoryTable| ())
(defun |cpCms| (prefix &optional (string (|getSystemCommandLine|)))
  (setq string (concat prefix string))
  (if (equal string "") (obey "sh")
    (obey string))
   (|terminateSystemCommand|))
(setq *print-escape* nil) ;; so stringimage doesn't escape idents?
#+(and :GCL :IEEE-FLOATING-POINT )
 (setq system:*print-nans* T)

(defun /RF (&rest foo &aux (Echo-Meta 'T))
  (declare (special Echo-Meta))
  (/RF-1 nil))

(defun /RQ (&rest foo &aux (Echo-Meta nil))
  (declare (special Echo-Meta))
  (/RF-1 nil))

(defun |/RQ,LIB| (&rest foo &aux (Echo-Meta nil) ($LISPLIB T))
  (declare (special Echo-Meta $LISPLIB))
  (/RF-1 nil))

(defun /RF-1 (ignore)
 (declare (ignore ignore))
 (declare (special echo-meta))
  (let* ((input-file (make-input-filename /EDITFILE))
     (lfile ())
     (type (pathname-type input-file)))
    (cond
     ((string= type "boot")
#-:CCL
      (boot input-file
         (setq lfile (make-pathname :type "lisp"
                           :defaults input-file)))
#+:CCL
      (boot input-file
         (setq lfile (make-pathname :name (pathname-name input-file)
                                :type "lisp")))
      (load lfile))
     ((string= type "lisp") (load input-file))
     ((string= type "bbin") (load input-file))
     ((string= type "input")
      (|ncINTERPFILE| input-file Echo-Meta))
     (t (spad input-file)))))

(defun /EF (&rest foo)
  (obey (concat "vi " (namestring (make-input-filename /EDITFILE)))))

;; non-interactive restarts...
(defun restart0 ()
  (compressopen);; set up the compression tables
  (interpopen);; open up the interpreter database
  (operationopen);; all of the operations known to the system
  (categoryopen);; answer hasCategory question
  (browseopen)
  (let ((asharprootlib (strconc (|systemRootDirectory|) "/aldor/lib/")))
    (set-file-getter (strconc asharprootlib "runtime.o"))
    (set-file-getter (strconc asharprootlib "lang.o"))
    (set-file-getter (strconc asharprootlib "attrib.o"))
    (set-file-getter (strconc asharprootlib "axlit.o"))
    (set-file-getter (strconc asharprootlib "minimach.o"))
    (set-file-getter (strconc asharprootlib "axextend.o")))
)

(defun SHAREDITEMS (x) T) ;;checked in history code
(defun whocalled (n) nil) ;; no way to look n frames up the stack
(defun setletprintflag (x) x)
(defun |normalizeTimeAndStringify| (time)
  (if (= time 0.0) "0" (format nil "~,1F" time)))

(define-function '|eval| #'eval)

(defun |libraryFileLists| () '((SPAD SPADLIBS J)))

;;--------------------> NEW DEFINITION (see cattable.boot.pamphlet)
(defun |compressHashTable| (ht) ht)
(defun GETZEROVEC (n) (MAKE-ARRAY n :initial-element 0))

(defun |normalizeArgFileName| (l) l)

(defun READSPADEXPR ()
  (declare (special in-stream))
  (let* ((line (cdar (preparse in-stream))))
    (cond ((or (not (stringp line)) (zerop (SIZE line)))
       (SAY "   Scratchpad -- input")
       (READSPADEXPR))
      (t (|parseTransform| (|postTransform| (|string2SpadTree| line)))))))

(define-function 'SUBSTQ #'SUBSTEQ) ;; needed for substNames (always copy)
#+(and :lucid (not :ibm/370))
 (define-function 'RUN-AIX-PROGRAM #'SYS:RUN-AIX-PROGRAM)
;; following should be no longer necessary
;; (eval-when (eval load compile) (shadow 'delete))
;; (define-function 'boot::delete #'|delete|)

;; following code is to mimic def of MAP in NEWSPAD LISP
;; i.e. MAP in boot package is a self evaluating form
;; #-:CCL (eval-when (eval load compile) (shadow 'map))
;; #-:CCL (defmacro map (&rest args) `'(map ,@args))
(eval-when (eval load compile) (shadow 'map))
(defmacro map (&rest args) `'(map ,@args))

#+:Lucid
(defun save-system (filename)
      (in-package "BOOT")
      (UNTRACE)
      (|untrace| NIL)
      (|clearClams|)
        ;; bind output to nulloutstream
      (let ((|$OutputStream| (make-broadcast-stream)))
        (|resetWorkspaceVariables|))
      (setq |$specialCharacters| |$plainRTspecialCharacters|)

          (load (|makeAbsoluteFilename| "lib/interp/obey"))
      (system:disksave filename :restart-function restart-hook :full-gc t))
#+:Lucid (define-function 'user::save-system  #'boot::save-system)
(defun |undoINITIALIZE| () ())
;; following are defined in spadtest.boot and stantest.boot
(defun |installStandardTestPackages| () ())
(defun |spadtestValueHook| (val type) ())
(defun |testError| (errotype erroValue) ())
(defvar |$TestOptions| ())
;; following in defined in word.boot
(defun |bootFind| (word) ())
;; following 3 are replacements for g-util.boot
(define-function '|isLowerCaseLetter| #'LOWER-CASE-P)
(define-function '|isUpperCaseLetter| #'UPPER-CASE-P)
(define-function '|isLetter| #'ALPHA-CHAR-P)

(defvar *msghash* nil "hash table keyed by msg number")

(defun cacheKeyedMsg (file)
  (let ((line "") (msg "") key)
   (with-open-file (in file)
    (catch 'done
     (loop
      (setq line (read-line in nil nil))
      (cond
       ((null line)
         (when key
          (setf (gethash key *msghash*) msg))
          (throw 'done nil))
       ((= (length line) 0))
       ((char= (schar line 0) #\S)
         (when key
          (setf (gethash key *msghash*) msg))
         (setq key (intern line "BOOT"))
         (setq msg ""))
       ('else
        (setq msg (concatenate 'string msg line)))))))))

(defun |fetchKeyedMsg| (key ignore)
 (declare (ignore ignore))
 (setq key (|object2Identifier| key))
 (unless *msghash*
  (setq *msghash* (make-hash-table))
  (cacheKeyedMsg |$defaultMsgDatabaseName|))
 (gethash key *msghash*))

#+:AKCL (proclaim '(ftype (function (t) t) identity))
#+:AKCL (defun identity (x) x)

(|initializeTimedNames| |$interpreterTimedNames| |$interpreterTimedClasses|)

(defun |rebuild| (filemode)
 "rebuild MODEMAP.DAASE, exit lisp with bad return code on failure"
 (let ((returncode -16))
  (unwind-protect
   (let (|$databaseQueue| |$e|)
    (declare (special |$databaseQueue| |$e|))
    (|clearConstructorAndLisplibCaches|)
    (setq |$databaseQueue| nil)
    (setq |$e| (cons (cons nil nil) nil))
    (|buildDatabase| filemode t)
    (setq |$IOindex| 1)
    (setq |$InteractiveFrame| (cons (cons nil nil) nil))
    (setq returncode 0))
   (unless (zerop returncode) (bye returncode)))))

#+:dos
(defun user-homedir-pathname ()
 (truename "."))

(defun boot::|printCopyright| ()
 (format t "there is no such thing as a simple job -- ((iHy))~%"))

(defvar |$ViewportProcessToWatch| nil)
(defun |setViewportProcess| ()
  (setq |$ViewportProcessToWatch|
     (stringimage (CDR
          (|processInteractive|  '(|key| (|%%| -2)) NIL) ))))

(defun |waitForViewport| ()
  (progn
   (do ()
       ((not (zerop (obey
         (concat
          "ps "
          |$ViewportProcessToWatch|
          " > /dev/null")))))
       ())
   (|sockSendInt| |$MenuServer| 1)
   (|setIOindex| (- |$IOindex| 3))
  )
)

#+:akcl
(defun print-xdr-stream (x y z) (format y "XDR:~A" (xdr-stream-name x)))
#+:akcl
(defstruct (xdr-stream
                (:print-function  print-xdr-stream))
           "A structure to hold XDR streams. The stream is printed out."
           (handle ) ;; this is what is used for xdr-open xdr-read xdr-write
           (name ))  ;; this is used for printing
#+(and :gcl (not (or :dos :win32)))
(defun |xdrOpen| (str dir) (make-xdr-stream :handle (system:xdr-open str) :name str))
#+:CCL
(defun |xdrOpen| (str dir) (xdr-open str dir) )
#+(and :gcl (or :dos :win32))
(defun |xdrOpen| (str dir) (format t "xdrOpen called"))

#+(and :akcl (not (or :dos :win32)))
(defun |xdrRead| (xstr r) (system:xdr-read (xdr-stream-handle xstr) r) )
#+:CCL
(defun |xdrRead| (xstr r) (xdr-read xstr r) )
#+(and :gcl (or :dos :win32))
(defun |xdrRead| (str) (format t "xdrRead called"))

#+(and :akcl (not (or :dos :win32)))
(defun |xdrWrite| (xstr d) (system:xdr-write (xdr-stream-handle xstr) d) )
#+:CCL
(defun |xdrWrite| (xstr d) (xdr-write xstr d) )
#+(and :gcl (or :dos :win32))
(defun |xdrWrite| (str) (format t "xdrWrite called"))

;; here is a test for XDR
;; (setq *print-array* T)
;; (setq foo (open "xdrtest" :direction :output))
;; (setq xfoo (|xdrOpen| foo))
;; (|xdrWrite| xfoo "hello: This contains an integer, a float and a float array")
;; (|xdrWrite| xfoo 42)
;; (|xdrWrite| xfoo 3.14159)
;; (|xdrWrite| xfoo (make-array 10 :element-type 'long-float :initial-element 2.78111D12))
;; (close foo)
;; (setq foo (open "xdrtest" :direction :input))
;; (setq xfoo (|xdrOpen| foo))
;; (|xdrRead| xfoo "")
;; (|xdrRead| xfoo 0)
;; (|xdrRead| xfoo 0.0)
;; (|xdrRead| xfoo (make-array 10 :element-type 'long-float ))
;; (setq *print-array* NIL)

(defun /versioncheck (n) (unless (= n /MAJOR-VERSION) (throw 'versioncheck -1)))

