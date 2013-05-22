;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


(import-module "sys-macros")
(import-module "sys-utility")
(in-package "BOOT")

(defun monitor-help ()
 (format t "~%
;;; MONITOR                                                                
;;;                                                                        
;;; This file contains a set of function for monitoring the execution      
;;; of the functions in a file. It constructs a hash table that contains   
;;; the function name as the key and monitor-data structures as the value  
;;;                                                                        
;;; The technique is to use a :cond parameter on trace to call the         
;;; monitor-incr function to incr the count every time a function is called
;;;                                                                        
;;; *monitor-table*                                HASH TABLE              
;;;    is the monitor table containing the hash entries                    
;;; *monitor-nrlibs*                               LIST of STRING
;;;    list of NRLIB filenames that are monitored
;;; *monitor-domains*                              LIST of STRING
;;;    list of domains to monitor-report (default is all exposed domains)
;;; monitor-data                                   STRUCTURE               
;;;    is the defstruct name of records in the table                       
;;;    name is the first field and is the name of the monitored function   
;;;    count contains a count of times the function was called             
;;;    monitorp is a flag that skips counting if nil, counts otherwise     
;;;    sourcefile is the name of the file that contains the source code    
;;;                                                                        
;;;  ***** SETUP, SHUTDOWN ****                                            
;;;                                                                        
;;; monitor-inittable ()                           FUNCTION                
;;;    creates the hashtable and sets *monitor-table*                      
;;;    note that it is called every time this file is loaded               
;;; monitor-end ()                                 FUNCTION                
;;;    unhooks all of the trace hooks                                      
;;;                                                                        
;;;  ***** TRACE, UNTRACE *****                                            
;;;                                                                        
;;; monitor-add (name &optional sourcefile)        FUNCTION                
;;;    sets up the trace and adds the function to the table                
;;; monitor-delete (fn)                            FUNCTION                
;;;    untraces a function and removes it from the table                   
;;; monitor-enable (&optional fn)                  FUNCTION                
;;;    starts tracing for all (or optionally one) functions that           
;;;    are in the table                                                    
;;; monitor-disable (&optional fn)                 FUNCTION                
;;;    stops tracing for all (or optionally one) functions that            
;;;    are in the table                                                    
;;;                                                                        
;;; ***** COUNTING, RECORDING  *****                                       
;;;                                                                        
;;; monitor-reset (&optional fn)                   FUNCTION                
;;;    reset the table count for the table (or optionally, for a function) 
;;; monitor-incr (fn)                              FUNCTION                
;;;    increments the count information for a function                     
;;;    it is called by trace to increment the count                        
;;; monitor-decr (fn)                              FUNCTION                
;;;    decrements the count information for a function                     
;;; monitor-info (fn)                              FUNCTION                
;;;    returns the monitor-data structure for a function                   
;;;                                                                        
;;; ***** FILE IO *****                                                    
;;;                                                                        
;;; monitor-write (items file)                     FUNCTION                
;;;    writes a list of symbols or structures to a file                    
;;; monitor-file (file)                            FUNCTION                
;;;    will read a file, scan for defuns, monitor each defun               
;;;    NOTE: monitor-file assumes that the file has been loaded            
;;;                                                                        
;;; ***** RESULTS *****                                                    
;;;                                                                        
;;; monitor-results ()                             FUNCTION                
;;;    returns a list of the monitor-data structures                       
;;; monitor-untested ()                            FUNCTION                
;;;    returns a list of files that have zero counts                       
;;; monitor-tested (&optional delete)              FUNCTION                
;;;    returns a list of files that have nonzero counts                    
;;;    optionally calling monitor-delete on those functions
;;;
;;; ***** CHECKPOINT/RESTORE *****
;;;
;;; monitor-checkpoint (file)                     FUNCTION
;;;    save the *monitor-table* in a loadable form
;;; monitor-restore (file)                        FUNCTION
;;;   restore a checkpointed file so that everything is monitored
;;;
;;; ***** ALGEBRA *****
;;;
;;; monitor-autoload ()                           FUNCTION
;;;   traces autoload of algebra to monitor corresponding source files
;;;   NOTE: this requires the /spad/int/algebra directory
;;; monitor-dirname (args)                        FUNCTION
;;;   expects a list of 1 libstream (loadvol's arglist) and monitors the source
;;;   this is a function called by monitor-autoload 
;;; monitor-nrlib (nrlib)                         FUNCTION
;;;   takes an nrlib name as a string (eg POLY) and returns a list of 
;;;   monitor-data structures from that source file
;;; monitor-report ()                             FUNCTION
;;;   generate a report of the monitored activity for domains in 
;;;   *monitor-domains*
;;; monitor-spadfile (name)                       FUNCTION
;;;   given a spad file, report all NRLIBS it creates
;;;   this adds each NRLIB name to *monitor-domains* but does not
;;;   trace the functions from those domains
;;; monitor-percent ()                            FUNCTION
;;;   ratio of (functions executed)/(functions traced)
;;; monitor-apropos (str)                         FUNCTION
;;;   given a string, find all monitored symbols containing the string
;;;   the search is case-insensitive. returns a list of monitor-data items
") nil)

(defvar *monitor-domains* nil "a list of domains to report")

(defvar *monitor-nrlibs* nil "a list of nrlibs that have been traced")

(defvar *monitor-table* nil "a table of all of the monitored data")

(defstruct monitor-data name count monitorp sourcefile)

(defun monitor-inittable ()
 "initialize the table"
 (setq *monitor-table* (make-hash-table)))

(eval-when (eval load) 
 (unless *monitor-table* (monitor-inittable)))

(defun monitor-end ()
 "stop the whole monitoring process. we cannot restart"
 (maphash 
  #'(lambda (key value)
     (declare (ignore value))
     (eval `(untrace ,key)))
   *monitor-table*))

(defun monitor-results ()
 "return a list of the monitor-data structures"
 (let (result)
  (maphash 
  #'(lambda (key value)
     (declare (ignore key))
     (push value result))
   *monitor-table*)
  result))

(defun monitor-add (name &optional sourcefile)
 "add a function to the hash table"
 (unless (fboundp name) (load sourcefile))
 (when (gethash name *monitor-table*) 
  (monitor-delete name))
 (eval `(trace (,name :cond (progn (monitor-incr ',name) nil))))
 (setf (gethash name *monitor-table*)
  (make-monitor-data 
     :name name :count 0 :monitorp t :sourcefile sourcefile)))

(defun monitor-delete (fn)
 "delete a function from the monitor table"
 (eval `(untrace ,fn))
 (remhash fn *monitor-table*))

(defun monitor-enable (&optional fn)
 "enable all (or optionally one) function for monitoring"
 (if fn 
  (progn
   (eval `(trace (,fn :cond (progn (monitor-incr ',fn) nil))))
   (setf (monitor-data-monitorp (gethash fn *monitor-table*)) t))
  (maphash 
   #'(lambda (key value)
      (declare (ignore value))
      (eval `(trace (,fn :cond (progn (monitor-incr ',fn) nil))))
      (setf (monitor-data-monitorp (gethash key *monitor-table*)) t))
   *monitor-table*)))

(defun monitor-disable (&optional fn)
 "disable all (or optionally one) function for monitoring"
 (if fn
  (progn
   (eval `(untrace ,fn))   
   (setf (monitor-data-monitorp (gethash fn *monitor-table*)) nil))
  (maphash 
   #'(lambda (key value)
      (declare (ignore value))
      (eval `(untrace ,fn))
      (setf (monitor-data-monitorp (gethash key *monitor-table*)) nil))
   *monitor-table*)))

(defun monitor-reset (&optional fn)
 "reset the table count for the table (or optionally, for a function)"
 (if fn
  (setf (monitor-data-count (gethash fn *monitor-table*)) 0)
  (maphash 
   #'(lambda (key value)
      (declare (ignore value))
      (setf (monitor-data-count (gethash key *monitor-table*)) 0))
   *monitor-table*)))

(defun monitor-incr (fn)
 "incr the count of fn by 1"
 (let (data)
  (setq data (gethash fn *monitor-table*))
  (if data 
   (incf (monitor-data-count data))  ;; change table entry by side-effect
   (warn "~s is monitored but not in table..do (untrace ~s)~%" fn fn))))

(defun monitor-decr (fn)
 "decr the count of fn by 1"
 (let (data)
  (setq data (gethash fn *monitor-table*))
  (if data 
   (decf (monitor-data-count data))  ;; change table entry by side-effect
   (warn "~s is monitored but not in table..do (untrace ~s)~%" fn fn))))

(defun monitor-info (fn)
 "return the information for a function"
 (gethash fn *monitor-table*))

(defun monitor-file (file)
 "hang a monitor call on all of the defuns in a file"
 (let (expr (package "BOOT"))
  (format t "monitoring ~s~%" file)
  (with-open-file (in file)
   (catch 'done
    (loop
     (setq expr (read in nil 'done))
     (when (eq expr 'done) (throw 'done nil))
     (if (and (consp expr) (eq (car expr) 'in-package))
      (if (and (consp (second expr)) (eq (first (second expr)) 'quote))
       (setq package (string (second (second expr))))
       (setq package (second expr)))
      (when (and (consp expr) (eq (car expr) 'defun))
       (monitor-add (intern (string (second expr)) package) file))))))))

(defun monitor-untested ()
 "return a list of the functions with zero count fields"
 (let (result)
  (maphash 
   #'(lambda (key value)
      (if (and (monitor-data-monitorp value) (= (monitor-data-count value) 0))
       (push key result)))
    *monitor-table*)
 result))

(defun monitor-tested (&optional delete)
 "return a list of the functions with non-zero count fields, optionally deleting them"
 (let (result)
  (maphash 
   #'(lambda (key value)
      (when (and (monitor-data-monitorp value) (> (monitor-data-count value) 0))
       (when delete (monitor-delete key))
       (push key result)))
    *monitor-table*)
 result))

(defun monitor-write (items file)
 "write out a list of symbols or structures to a file"
 (with-open-file (out file :direction :output)
  (dolist (item items)
    (if (symbolp item)
     (format out "~s~%" item)
     (format out "~s~50t~s~100t~s~%"
       (monitor-data-sourcefile item)
       (monitor-data-name item)
       (monitor-data-count item))))))

(defun monitor-checkpoint (file)
 "save the *monitor-table* in loadable form"
 (let ((*print-package* t))
  (declare (special *print-package*))
  (with-open-file (out file :direction :output)
   (format out "(in-package \"BOOT\")~%")
   (format out "(monitor-inittable)~%")
   (dolist (data (monitor-results))
    (format out "(monitor-add '~s ~s)~%"
     (monitor-data-name data)
     (monitor-data-sourcefile data))
    (format out "(setf (gethash '~s *monitor-table*) 
                  (make-monitor-data :name '~s :count ~s :monitorp ~s
                                     :sourcefile ~s))~%"
     (monitor-data-name data)
     (monitor-data-name data)
     (monitor-data-count data)
     (monitor-data-monitorp data)
     (monitor-data-sourcefile data))))))

(defun monitor-restore (file)
 "restore a checkpointed file so that everything is monitored"
 (load file))

;; these functions are used for testing the algebra code

(defun monitor-dirname (args)
  "expects a list of 1 libstream (loadvol's arglist) and monitors the source"
   (let (name)
    (setq name (|libDirname| (car args)))
    (setq name (file-namestring name))
    (setq name (concatenate 'string "/spad/int/algebra/" name "/code.lsp"))
    (when (probe-file name)
     (push name *monitor-nrlibs*)
     (monitor-file name))))

(defun monitor-autoload ()
  "traces autoload of algebra to monitor corresponding source files"
  #+:GCL(trace (loadvol 
		:entrycond nil
		:exitcond (progn (monitor-dirname system::arglist) nil))))

(defun monitor-nrlib (nrlib)
 "takes an nrlib name as a string (eg POLY) and returns a list of 
  monitor-data structures from that source file"
 (let (result)
  (maphash 
   #'(lambda (k v)
      (declare (ignore k))
      (when (string= nrlib 
             (pathname-name (car (last
                (pathname-directory (monitor-data-sourcefile v))))))
       (push v result)))
   *monitor-table*)
 result))

(defun monitor-libname (item)
  "given a monitor-data item, extract the NRLIB name"
  (pathname-name (car (last
   (pathname-directory (monitor-data-sourcefile item))))))

(defun monitor-exposedp (fn)
 "exposed functions have more than 1 semicolon. given a symbol, count them"
   (> (count #\; (symbol-name fn)) 1))

(defun monitor-readinterp ()
  "read INTERP.EXPOSED to initialize *monitor-domains* to exposed domains.
   this is the default action. adding or deleting domains from the list
   will change the report results"
  (let (skip expr name)
   (declare (special *monitor-domains*))
   (setq *monitor-domains* nil)
   (with-open-file (in "/spad/src/algebra/INTERP.EXPOSED")
    (read-line in)
    (read-line in)
    (read-line in)
    (read-line in)
    (catch 'done
     (loop
      (setq expr (read-line in nil "done"))
      (when (string= expr "done") (throw 'done nil))
      (cond
       ((string= expr "basic") (setq skip nil))
       ((string= expr "categories") (setq skip t))
       ((string= expr "hidden") (setq skip t))
       ((string= expr "defaults") (setq skip nil)))
      (when (and (not skip) (> (length expr) 58))
       (setq name (subseq expr 58 (length expr)))
       (setq name (string-right-trim '(#\space) name))
       (when (> (length name) 0)
        (push name *monitor-domains*))))))))

(defun monitor-report ()
 "generate a report of the monitored activity for domains in *monitor-domains*"
 (let (nrlibs nonzero total)
  (unless *monitor-domains* (monitor-readinterp))
  (setq nonzero 0)
  (setq total 0)
  (maphash 
   #'(lambda (k v)
      (declare (ignore k))
      (let (nextlib point)
       (when (> (monitor-data-count v) 0) (incf nonzero))
       (incf total)
       (setq nextlib (monitor-libname v))
       (setq point (member nextlib nrlibs :test #'string= :key #'car))
       (if point
         (setf (cdr (first point)) (cons v (cdr (first point))))
         (push (cons nextlib (list v)) nrlibs))))
   *monitor-table*)
  (format t "~d of ~d (~d percent) tested~%" nonzero total 
    (round (/ (* 100.0 nonzero) total)))
  (setq nrlibs (sort nrlibs #'string< :key #'car))
  (dolist (pair nrlibs)
   (let ((exposedcount 0) (testcount 0))
    (when (member (car pair) *monitor-domains* :test #'string=)
     (format t "for library ~s~%" (car pair))
     (dolist (item (sort (cdr pair)  #'> :key #'monitor-data-count))
      (when (monitor-exposedp (monitor-data-name item))
       (incf exposedcount)
       (when (> (monitor-data-count item) 0) (incf testcount))
       (format t "~5d ~s~%" 
         (monitor-data-count item) 
         (monitor-data-name item))))
      (if (= exposedcount testcount)
       (format t "~a has all exposed functions tested~%" (car pair))
       (format t "Daly bug:~a has untested exposed functions~%" (car pair))))))
 nil))
      
(defun monitor-parse (expr)
  (let (point1 point2)
   (setq point1 (|findChar| #\space expr))
   (setq point1 (position #\space expr :start point1 :test-not #'char=))
   (setq point1 (|findChar| #\space expr point1))
   (setq point1 (position #\space expr :start point1 :test-not #'char=))
   (setq point2 (|findChar| #\space expr point1))
   (subseq expr point1 point2)))

(defun monitor-spadfile (name)
 "given a spad file, report all NRLIBS it creates"
 (let (expr)
  (with-open-file (in name)
   (catch 'done
    (loop
     (setq expr (read-line in nil 'done))
     (when (eq expr 'done) (throw 'done nil))
     (when (and (> (length expr) 4) (string= (subseq expr 0 4) ")abb"))
      (setq *monitor-domains* 
       (adjoin (monitor-parse expr) *monitor-domains* :test #'string=))))))))

(defun monitor-percent ()
 (let (nonzero total)
  (setq nonzero 0)
  (setq total 0)
  (maphash 
   #'(lambda (k v)
      (declare (ignore k))
      (when (> (monitor-data-count v) 0) (incf nonzero))
      (incf total))
   *monitor-table*)
   (format t "~d of ~d (~d percent) tested~%" nonzero total 
     (round (/ (* 100.0 nonzero) total)))))

(defun monitor-apropos (str)
  "given a string, find all monitored symbols containing the string
   the search is case-insensitive. returns a list of monitor-data items"
 (let (result)
  (maphash 
   #'(lambda (k v)
      (when
       (|findString| (string-upcase str) (string-upcase (symbol-name k)))
        (push v result)))
   *monitor-table*)
 result))
