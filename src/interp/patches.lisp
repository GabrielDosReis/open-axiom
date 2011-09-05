;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2011, Gabriel Dos Reis.
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

(defun CATCHALL (a &rest b) a) ;; not correct but ok for now
(defvar |$demoFlag| nil)

(define-function '|construct| #'list) ;; NEEDED , SPAD-COMPILER generated Lisp code

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

(setq *print-escape* nil) ;; so stringimage doesn't escape idents?
#+(and :GCL :IEEE-FLOATING-POINT )
 (setq system:*print-nans* T)

(defun /RF (&rest foo &aux (|$Echo| 'T))
  (declare (special |$Echo|))
  (/RF-1 nil))

(defun /RQ (&rest foo &aux (|$Echo| nil))
  (declare (special |$Echo|))
  (/RF-1 nil))

(defun |/RQ,LIB| (&rest foo &aux (|$Echo| nil))
  (declare (special |$Echo|))
  (/RF-1 nil))

(defun /RF-1 (ignore)
 (declare (ignore ignore))
 (declare (special |$Echo|))
  (let* ((input-file (make-input-filename /EDITFILE))
     (lfile ())
     (type (pathname-type input-file)))
    (cond
     ((string= type "boot")
      (boot input-file
         (setq lfile (make-pathname :type "lisp"
                           :defaults input-file)))
      (load lfile))
     ((string= type "lisp") (load input-file))
     ((string= type "bbin") (load input-file))
     ((string= type "input")
      (|ncINTERPFILE| input-file |$Echo|))
     (t (spad input-file)))))

(defun /EF (&rest foo)
  (|runCommand| (concat "vi " (namestring (make-input-filename /EDITFILE)))))

(defun SHAREDITEMS (x) T) ;;checked in history code

(defun GETZEROVEC (n) (MAKE-ARRAY n :initial-element 0))

(defun READSPADEXPR ()
  (declare (special in-stream))
  (let* ((line (cdar (preparse in-stream))))
    (cond ((or (not (stringp line)) (zerop (SIZE line)))
       (SAY "   Scratchpad -- input")
       (READSPADEXPR))
      (t (|parseTransform| (|postTransform| (|string2SpadTree| line)))))))

;; following are defined in spadtest.boot and stantest.boot
(defun |installStandardTestPackages| () ())
(defun |spadtestValueHook| (val type) ())
(defun |testError| (errotype erroValue) ())
(defvar |$TestOptions| ())
;; following in defined in word.boot
(defun |bootFind| (word) ())

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

(defvar |$ViewportProcessToWatch| nil)
(defun |setViewportProcess| ()
  (setq |$ViewportProcessToWatch|
     (stringimage (CDR
          (|processInteractive|  '(|key| (|%%| -2)) NIL) ))))

(defun |waitForViewport| ()
  (progn
   (do ()
       ((not (zerop (|runCommand|
         (concat
          "ps "
          |$ViewportProcessToWatch|
          " > /dev/null")))))
       ())
   (|sockSendInt| |$MenuServer| 1)
   (|setIOindex| (- |$IOindex| 3))
  )
)

