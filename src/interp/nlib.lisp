;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2013, Gabriel Dos Reis.
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


(IMPORT-MODULE "sys-macros")
(IMPORT-MODULE "sys-utility")
(in-package "BOOT")

;; (RDEFIOSTREAM ((MODE . IO) (FILE fn ft dir))) IO is I,O,INPUT,OUTPUT
(defun rdefiostream (options &optional (missing-file-error-flag t))
  (let ((mode (cdr (assoc 'mode options)))
        (file (assoc 'file options))
        (stream nil)
        (fullname nil)
        (indextable nil))
        (cond ((equal (elt (string mode) 0) #\I)
               (setq fullname (|makeInputFilename| (cdr file) 'NIL))
               (setq stream (|openIndexFileIfPresent| fullname))
               (if (null stream)
                   (if missing-file-error-flag
                       (ERROR (format nil "Library ~s doesn't exist"
                              (|makeFilename| (cdr file) 'NIL)))
                     NIL)
               (|makeLibstream| 'input fullname
                                (get-index-table-from-stream stream)
                                stream)))
              ((equal (elt (string mode) 0) #\O)
               (setq fullname (|makeFullFilePath| (cdr file) 'NIL))
               (case (|directoryp| fullname)
                     (-1 (|checkMkdir| fullname))
                     (0 (error (format nil "~s is an existing file, not a library" fullname)))
                     (otherwise))
	       ;; Make sure parent directory exists.
	       #-:GCL (ensure-directories-exist 
		       (|ensureTrailingSlash| fullname))
               (multiple-value-setq (stream indextable) (get-io-index-stream fullname))
               (|makeLibstream| 'output fullname indextable stream))
              ('t  (ERROR "Unknown MODE")))))


(defun get-index-table-from-stream (stream)
  (let ((pos (read  stream)))
    (cond ((numberp pos)
           (file-position stream pos)
           (read stream))
          (t pos))))

(defun get-io-index-stream (dirname)
  (let* ((index-file (concat dirname "/" |$IndexFilename|))
         (stream (open index-file :direction :io :if-exists :overwrite
                       :if-does-not-exist :create))
         (indextable ())
         (pos (read stream nil nil)))
    (cond ((numberp pos)
           (file-position stream pos)
           (setq indextable (read stream))
           (file-position stream pos))
          (t (file-position stream 0)
             (princ "                    " stream)
	     (terpri stream)
             (setq indextable pos)))
    (values stream indextable)))

(defun |getIndexIOStreamAndTable| (d)
  (multiple-value-call #'cons (get-io-index-stream d)))

;substitute indextable in dirname

(defun write-indextable (indextable stream)
  (let ((pos (file-position stream)))
    (write indextable :stream stream :level nil :length nil :escape t)
    (finish-output stream)
    (file-position stream 0)
    (princ pos stream)
    (finish-output stream)))

(defun putindextable (indextable dirname)
  (with-open-file
    (stream (concat dirname "/" |$IndexFilename|)
             :direction :io :if-exists :overwrite
             :if-does-not-exist :create)
    (file-position stream :end)
    (write-indextable indextable stream)))

;; (RREAD key rstream)
(defun rread (key rstream &optional (error-val nil error-val-p))
  (if (equal (|libIOMode| rstream) 'output) (error "not input stream"))
  (let* ((entry
         (and (stringp key)
              (assoc key (|libIndexTable| rstream) :test #'string=)))
         (file-or-pos (and entry (caddr entry))))
    (cond ((null entry)
           (if error-val-p error-val (error (format nil "key ~a not found" key))))
          ((null (caddr entry)) (cdddr entry))  ;; for small items
          ((numberp file-or-pos)
           (file-position (|libIndexStream| rstream) file-or-pos)
           (read (|libIndexStream| rstream)))
          (t
           (with-open-file
            (stream (concat (|libDirname| rstream) "/" file-or-pos))
            (read  stream))) )))

(defvar *lib-var*)

;; (RKEYIDS filearg) -- interned version of keys
(defun rkeyids (&rest filearg)
  (mapcar #'intern (mapcar #'car (|getIndexTable|
                                  (|makeInputFilename| filearg 'NIL)))))

;; (RWRITE cvec item rstream)
(defun rwrite (key item rstream)
  (if (equal (|libIOMode| rstream) 'input) (error "not output stream"))
  (let ((stream (|libIndexStream| rstream))
        (pos (if item (cons (file-position (|libIndexStream| rstream)) nil)
               (cons nil item))))   ;; for small items
    (make-entry (string key) rstream pos)
    (when (numberp (car pos))
          (write item :stream stream :level nil :length nil
                 :circle t :array t :escape t)
          (terpri stream))))

(defun make-entry (key rstream value-or-pos)
   (let ((entry (assoc key (|libIndexTable| rstream) :test #'equal)))
     (if (null entry)
         (push (setq entry (cons key (cons 0 value-or-pos)))
               (|libIndexTable| rstream))
       (progn
         (if (stringp (caddr entry))
	     (|removeFile| (|makeFullFilePath| (caddr entry))))
         (setf (cddr entry) value-or-pos)))
     entry))

(defun rshut (rstream)
  (when (|libCodeStream| rstream)
    (close (|libCodeStream| rstream)))
  (when (|libInsnStream| rstream)
    (close (|libInsnStream| rstream)))
  (if (eq (|libIOMode| rstream) 'output)
      (write-indextable (|libIndexTable| rstream) (|libIndexStream| rstream)))
  (close (|libIndexStream| rstream)))

;; filespec is id or list of 1, 2 or 3 ids
;; filearg is filespec or 1, 2 or 3 ids
;; (RPACKFILE filearg)  -- compiles code files and converts to compressed format
(defun rpackfile (filespec)
  (setq filespec (|makeFilename| filespec))
  (if (string= (pathname-type filespec) "NRLIB")
      (recompile-lib-file-if-necessary 
       (concat (namestring filespec) "/code.lsp"))

  ;; only pack non libraries to avoid lucid file handling problems    
    (let* ((rstream (rdefiostream (list (cons 'file filespec) (cons 'mode 'input))))
           (nstream nil)
           (nindextable nil)
           (nrstream nil)
           (index-file-name (concat (truename filespec) "/" |$IndexFilename|))
           (temp-index-file-name (make-pathname :name "oldindex"
                                                :defaults index-file-name)))
      (rename-file index-file-name temp-index-file-name ) ;; stays until closed
      (multiple-value-setq (nstream nindextable) (get-io-index-stream filespec))
      (setq nrstream (|makeLibstream| 'output filespec nindextable nstream))
      (dolist (entry (|libIndexTable| rstream))
              (rwrite (car entry) (rread (car entry) rstream) nrstream)
              (if (stringp (caddr entry))
                  (delete-file (concat filespec "/" (caddr entry)))))
      (close (|libIndexStream| rstream))
      (delete-file temp-index-file-name)
      (rshut nrstream)))
  filespec)

(defun recompile-lib-file-if-necessary (lfile)
   (let* ((bfile (make-pathname :type |$faslType| :defaults lfile))
          (bdate (and (probe-file bfile) (file-write-date bfile)))
          (ldate (and (probe-file lfile) (file-write-date lfile))))
     (if ldate
         (if (and bdate (> bdate ldate)) nil
           (progn (compile-lib-file lfile) (list bfile))))))

#+:AKCL
(defun spad-fixed-arg (fname )
   (and (equal (symbol-package fname) (find-package "BOOT"))
        (not (get fname 'compiler::spad-var-arg))
        (|findChar| #\; (symbol-name fname))
        (or (get fname 'compiler::fixed-args)
            (setf (get fname 'compiler::fixed-args) t)))
   nil)

(defun compile-lib-file (file)
  (multiple-value-bind (result warning-p failure-p)
    (compile-file file)
    (cond ((null result)
	   (|systemError| (list "Generated Lisp was malformed")))
	  (failure-p
	   (|removeFile| (namestring result))
	   (|systemError| (list "Compilation of generated Lisp failed"))))
    result))

;; (RDROPITEMS filearg keys) don't delete, used in files.spad
(defun rdropitems (filearg keys &aux (ctable (|getIndexTable| filearg)))
  (mapc #'(lambda(x)
           (setq ctable (delete x ctable :key #'car :test #'equal)) )
           (mapcar #'string keys))
  (putindextable ctable filearg))

(defun $FCOPY (filespec1 filespec2)
    (let ((name1 (|makeFullFilePath| filespec1))
          (name2 (|makeFullFilePath| filespec2)))
      (if (library-file name1)
        (copy-lib-directory name1 name2)
        (|copyFile| name1 name2))))


#+ :AKCL
(defun copy-lib-directory (name1 name2)
   (|checkMkdir| name2)
   (system (concat "sh -c 'cp " name1 "/* " name2 "'")))
