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

;; Uncommon 1.6
;; This package is a Boot interface for Common Lisp.
;; SMW 1989, 1990

;; Operating system interface

;; The only non-common lisp functions used in this file are in this section.
;; The following functions are provided:

;;   OsRunProgram program &rest args  
;;      Run the named program with given arguments.
;;      All I/O is to the current places.
;;      Value returned is implementation-dependent.

;;   OsRunProgramToStream program &rest args
;;      Run the named program  with given arguments.
;;      Input and error output to the current places.
;;      Value returned is a stream of the program's standard output.

;;   OsEnvVarCharacter
;;      The character which indicates OS environment variables in a string.
;;      On Unix this is "$".

;;   OsEnvGet name
;;      name is a string or a symbol
;;      The string associated with the given name is returned.
;;      This is from the environment on Unix. On CMS globalvars could be used.

;;   OsProcessNumber
;;      Returns a unique number associated with the current session.
;;      On Unix this is the process id.  
;;      The same workspace started a second time must give a different result.



(IMPORT-MODULE "sys-macros")
(in-package "BOOT")  

(defun |OsRunProgram| (program &rest args)
  #+(and :Lucid (not :ibm/370))   (lucid-os-run-program   program args)
  #+:CmuLisp (cmulisp-os-run-program program args)
  #+:KCL     (kcl-os-run-program     program args)
  #-(or (and :Lucid (not :ibm/370)) :CmuLisp :KCL) nil )

(defun |OsRunProgramToStream| (program &rest args)
  #+(and :Lcid (not ibm/370))
     (lucid-os-run-program-to-stream     program args)
  #+:CmuLisp (cmulisp-os-run-program-to-stream   program args)
  #+:KCL     (kcl-os-run-program-to-stream       program args)
  #-(or (and :Lucid (not :ibm/370)) :CmuLisp :KCL)
     (make-string-output-stream "") )

;Unix:
(defvar |OsEnvVarCharacter| #\$)

(defun |OsEnvGet| (sym)
  #+(and :Lucid (not :ibm/370))  (lucid-os-env-get   sym)
  #+:CmuLisp (cmulisp-os-env-get sym)
  #+:KCL     (kcl-os-env-get     sym)
  #-(or (and :Lucid (not :ibm/370)) :CmuLisp :KCL) "" )

(defun |OsProcessNumber| ()
  #+(and :Lucid (not :ibm/370))    (lucid-os-process-number)
  #+:CmuLisp  (cmulisp-os-process-number)
  #+:KCL      (kcl-os-process-number)
  #-(or (and :Lucid (not :ibm/370)) :CmuLisp :KCL) 42 )

;;;
;;; Lucid-only implementations
;;;

#+(and :Lucid (not :ibm/370)) (progn
(defun lucid-os-run-program (program args)
  (system:run-aix-program program :arguments args))

(defun lucid-os-run-program-to-stream (program args)
  (system:run-aix-program program
        :wait nil
        :output :stream
        :arguments args))

(defun lucid-os-env-get (sym)
   (c-to-lisp-string (getenv (string sym))) )

(defun lucid-os-process-number ()
  (getpid))

(system:define-foreign-function :c 'getenv  :pointer)
(system:define-foreign-function :c 'sprintf :pointer)
(system:define-foreign-function :c 'strlen  :fixnum)
(system:define-foreign-function :c 'getpid  :fixnum)

(defun c-to-lisp-string (ptr)
  (let (str len)
       (setq len (strlen ptr))
       (setq str (make-array (list len) :element-type 'character))
       (sprintf str "%s" ptr)  ; Cannot use strcpy because it stops in a \0.
       str ))
)

;;;
;;; Cmulisp-only implementations
;;;

#+:CmuLisp (progn
(defun cmulisp-os-run-program (program args)
  (extensions:run-program program args 
        :input  't     ; use current standard input  -- default is /dev/null
        :output 't     ; use current standard output
        :error  't ))  ; use current standard error

(defun cmulisp-os-run-program-to-stream (program args)
  (second (multiple-value-list
    (extensions:run-program program args 
        :wait   nil        ; don't wait
        :input  't         ; use current standard input
        :output :stream    ; slurp the output of the process
        :error  't )) ))   ; use current standard error

(defun cmulisp-os-env-get (sym) 
  (let ((key (intern (string sym) (find-package "KEYWORD"))))
    (cdr (assoc key *environment-list* :test #'eq)) ))

(defun cmulisp-os-process-number () 
  (Aix::Unix-getpid) )
)

;;;
;;; KCL-only implementations
;;;

#+:KCL (progn
(defun kcl-os-run-program (program args)
  (system (format nil "~{~a ~}" (cons program args))) )

(defun kcl-os-run-program-to-stream (program args)
  (system (format nil "~{~a ~}" (cons program args))) )

(defun kcl-os-env-get (sym) 
  (system:getenv (string sym)) )

(defun kcl-os-process-number () 
  77 )

;(defentry |getpid| () (int "getpid"))
)

;;;;
;;;; Time
;;;;

(defun |TimeStampString| ()
  (multiple-value-bind (sec min hr mody mo yr wkdy daylight zone)
     (get-decoded-time)
     (declare (ignore wkdy daylight zone))
     (format nil "~2,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
       yr mo mody hr min sec) ))
        
;;;;
;;;; File system interface
;;;;

;;(defun |FileExists?| (path)
;;  (probe-file path) )
;;
;;(defun |FileRemove| (path)
;;  (delete-file path) )
;;
;;(defun |FileRename| (oldpath newpath)
;;  (rename-file oldpath newpath) )
;;
;;(defun |FileAbsolutePath| (path)
;;  (truename path) )
;;
;;(defun |FileDate| (path)
;;  (file-write-date path) )
;;
;;(defun |TextFileOpenIn| (path)
;;  (open path 
;;        :element-type 'character
;;        :direction :input ))
;;
;;(defun |TextFileOpenOut| (path)
;;  (open path 
;;        :element-type 'character
;;        :direction :output 
;;        :if-exists :supersede 
;;        :if-does-not-exist :create ))
;;
;;(defun |TextFileOpenIO| (path)
;;  (open path
;;        :element-type 'character
;;        :direction :io
;;        :if-exists :overwrite        ; open at beginning
;;        :if-does-not-exist :create ))
;;
;;(defun |TextFileOpenAppend| (path)
;;  (open path 
;;        :element-type 'character
;;        :direction :output 
;;        :if-exists :append 
;;        :if-does-not-exist :create ))
;;
;;
;;(defun |ByteFileOpenIn| (path)
;;  (open path 
;;        :element-type 'unsigned-byte
;;        :direction :input ))
;;
;;(defun |ByteFileOpenOut| (path)
;;  (open path 
;;        :element-type 'unsigned-byte
;;        :direction :output 
;;        :if-exists :supersede 
;;        :if-does-not-exist :create ))
;;
;;(defun |ByteFileOpenIO| (path)
;;  (open path
;;        :element-type 'unsigned-byte
;;        :direction :io
;;        :if-exists :overwrite        ; open at beginning
;;        :if-does-not-exist :create ))
;;
;;(defun |ByteFileOpenAppend| (path)
;;  (open path 
;;        :element-type 'unsigned-byte
;;        :direction :output 
;;        :if-exists :append 
;;        :if-does-not-exist :create ))
;;
;;(defun |ReadFileLineAt| (path pos)
;;  (with-open-file (stream path :direction :input)
;;                (file-position stream pos)
;;                (read-line stream) ))
;;
;;(defun |UserHomeDirectory| ()
;;  (pathname-directory (user-homedir-pathname)) )
;;
;;(defun |DirectoryFiles| (path)
;;  (directory path) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Lisp Interface
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |LispReadFromString| (str &optional (startpos 0))
  (prog (ob nextpos)
        (multiple-value-setq
         (ob nextpos)
         (read-from-string str nil nil :start startpos) )
        (return (list ob nextpos)) ))

(defun |LispEval| (expr)
  (eval expr) )

;;; expr must be a defun, defmacro, etc.
(defun |LispCompile| (expr)
   (eval expr)
   (compile (second expr)) )
   
(defun |LispLoadFileQuietly| (object)
   (load object :verbose nil :print nil))

(defun |LispCompileFile| (fname)
  (compile-file fname) )

(defun |LispLoadFile| (fname)
  (load fname) )

(defun |LispKeyword| (str)
  (intern str 'keyword) )

;;;
;;; Control
;;;

           
(defmacro |funcall| (&rest args)
  (cons 'funcall args) )

(defmacro |Catch| (tag expr) 
  `(catch ,tag ,expr) )

(defmacro |Throw| (tag expr) 
  `(Throw ,tag ,expr) )

(defmacro |UnwindProtect| (a b)
  `(unwind-protect ,a ,b) )

;;; This macro catches as much as it can.  
;;; Systems with a catchall should use it.  
;;; It is legitimate to not catch anything, if there is no system support.
;;; 
;;; If the result was caught, then tagvar is set to the desination tag
;;; and the thown value is returned.  Otherwise, tagvar is set to nil
;;; and the first result of the expression is returned.

#+:Lucid 
(defmacro |CatchAsCan| (tagvar expr)
  `(let ((catch-result nil) 
         (expr-result nil) 
         (normal-exit (gensym)))

        (setq catch-result 
          (catch 'lucid::top-level 
            (setq expr-result ,expr)
            normal-exit))
        (cond 
          ((eq catch-result normal-exit)
            (setq ,tagvar nil)
            expr-result )
          ('t
            (setq ,tagvar 'lucid::top-level)
            catch-result )) ))

#-:Lucid 
(defmacro |CatchAsCan| (tagvar expr)
  `(progn 
      (setq tagvar nil) 
      ,expr ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro |Eq| (a b)
  `(eq ,a ,b) )

(defvar |Nil| nil)

(defun |DeepCopy| (x)
  (copy-tree x) )

(defun |SortInPlace| (l pred)
  (sort l pred) )

(defun |Sort| (l pred)
  (sort (copy-tree l) pred) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Streams
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun |Prompt| (line &optional (readfn nil))
  (format *query-io* "~a" line)
  (when readfn (apply readfn (list *query-io*))) )

(defun |PlainError| (&rest args)
  (let ((fmt (plain-print-format-string args)))
       (error fmt args) ))

(defun |PrettyPrint| (expr &optional (outstream *standard-output*))
  (write expr :stream outstream :level nil :length nil :pretty 't :escape 't) 
  (finish-output outstream) )

(defun |PlainPrint| (&rest args)
  (let ((fmt (plain-print-format-string args)))
       (format *standard-output* fmt args) ))

(defun |PlainPrintOn| (stream &rest args)
  (let ((fmt (plain-print-format-string args)))
       (format stream fmt args) ))

(defun plain-print-format-string (l)
  (format nil "~~~d{~~a~~}~~%" (length l)) )


;;; Lucid 1.01 bug:  Must flush output after each write or else
;;;                  strange errors arise from invalid buffer reuse.

(defmacro |WriteByte| (byte &rest outstream)
  `(write-byte ,byte ,@outstream) )

(defmacro |WriteChar| (char &rest outstream)
  `(write-char ,char ,@outstream) )

;; Write a string -- no new line.
(defun |WriteString| (string &optional (outstream *standard-output*))
  (format outstream "~a" string) 
  (finish-output outstream) )

;; Write a string then start a new line.
(defun |WriteLine| (string &optional (outstream *standard-output*))
  (write-line string outstream) 
  (finish-output outstream) )    

(defun |ByteFileWriteLine| (string outstream)
  (let ((n (length string)))
    (do ((i 0 (+ i 1)))
        ((= i n))
        (write-byte (char-code (char string i)) outstream) ))
  (write-byte (char-code #\Newline) outstream)
  (finish-output outstream) )


(defmacro |ReadByte| (instream)
  `(read-byte ,instream nil nil) )

(defmacro |ReadChar| (&rest instream)
  (if instream
    `(read-char ,@instream nil nil) 
    '(read-char *standard-input* nil nil) ))

(defun |ReadLine| (&optional (instream *standard-input*))
  (read-line instream nil nil) )

(defun |ByteFileReadLine| (instream)
  (do ((buf (make-array '(80) 
                :element-type 'character 
                :fill-pointer 0
                :adjustable 't ))
       (b (read-byte instream nil nil) (read-byte instream nil nil))
       (c) )

      ((or (null b) (char= (setq c (code-char b)) #\Newline)) buf)
     
      (vector-push-extend c buf) ))

;;; Reads no more than the rest of the current line into the string argument.
;;; The #\Newline is not included in the string.
;;;
;;; The result is an integer, 'T or nil.
;;;   Nil  the stream was already exhausted.
;;;   T    the string was filled before the end of line was reached.
;;;   k    the end of line was reached and k characters were copied.
;;;
;;; If the argument "flags" is passed a cons cell, it is updated
;;; to contain (Eof . Eol).  
;;; Eof indicates whether the end of file was detected.
;;; Eol indicates whether the line was terminated by a #\newline.

(defun |ReadLineIntoString| (string &optional (instream *standard-input*) 
                                              (flags nil) )

  (when (consp flags) (rplaca flags nil) (rplacd flags nil))

  (let ((n (length string))
        (i 0)
        (c (read-char instream nil nil)) )
    
       (loop
         (cond 
           ((null c)
              (when (consp flags) (rplaca flags 't))
              (return (if (= i 0) nil i)) )
           ((char= c #\Newline)
              (when (consp flags) (rplacd flags 't))
              (return i) )
           ((= i n)
              (unread-char c instream)
              (return 't) ))

         (setf (char string i) c)
         (setq i (+ i 1))
         (setq c (read-char instream nil nil)) )))
   

;;; Similar to ReadLineIntoString but reads from a ByteFile.
(defun |ByteFileReadLineIntoString| (string instream &optional (flags nil))

  (when (consp flags) (rplaca flags nil) (rplacd flags nil))

  (let ((n (length string))
        (i 0)
        (b nil)
        (c nil) )
    
       (loop
         (when (= i n) (return 't) )
         (setq b (read-byte instream nil nil)) 
         (when (null b)
           (when (consp flags) (rplaca flags 't))
           (return i) )

         (setq c (code-char b))
         (when (char= c #\Newline)
           (when (consp flags) (rplacd flags 't))
           (return i) )

         (setf (char string i) c)
         (setq i (+ i 1)) )))

(defun |ReadBytesIntoVector| 
           (vector &optional (instream *standard-input*) (flags nil) )

  (when (consp flags) (rplaca flags nil) (rplacd flags nil))

  (let ((n (length vector))
        (i 0)
        (b nil) )
    
       (loop
         (when (= i n) (return 't))
         (setq b (read-byte instream nil nil))
         (when (null b)
           (when (consp flags) (rplaca flags 't))
           (return i) )

         (setf (aref vector i) b)
         (setq i (+ i 1)) )))


(defun |InputStream?| (stream)
   (input-stream-p stream) )

(defun |OutputStream?| (stream)
   (output-stream-p stream) )

;;; Whether the position is a record number or character number is 
;;; implementation specific.  In Common Lisp it is a character number.

(defun |StreamGetPosition| (stream)
  (file-position stream) )

(defun |StreamSetPosition| (stream pos)
  (file-position stream pos))

(defun |StreamSize| (stream)
  (file-length stream))

(defmacro |WithOpenStream| (var stream-form body)
  `(with-open-stream (,var ,stream-form) ,body) )

;;; Copy up to n characters or eof.
;;; Return number of characters actually copied
(defun |StreamCopyChars| (instream outstream n)
  (do ((i 0 (+ i 1))
       (c (read-char instream nil nil) (read-char instream nil nil)) )
      ((or (null c) (= i n))  (finish-output outstream) i)

      (write-char c outstream) ))

(defun |StreamCopyBytes| (instream outstream n)
  (do ((i 0 (+ i 1))
       (b (read-byte instream nil nil) (read-byte instream nil nil)) )
      ((or (null b) (= i n))  (finish-output outstream) i)

      (write-byte b outstream) ))

(defun |StreamEnd?| (instream)
  (null (peek-char nil instream nil nil)) )

(defun |StreamFlush| (&optional (outstream *standard-output*))
  (finish-output outstream) )

(defun |StreamClose| (stream)
  (close stream) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Types
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Functions for manipulating values of type Xxxx are prefixed with Xxxx.
;;; E.g., CsetUnion
;;; Values of type Xxxx are suffixed with Xxxx.
;;; E.g., AlphaCset
;;; The primary function for creating object of this type is named Xxxx.
;;; The type-testing predicate is Xxxx?

;;; xx    := Xxxx(args)
;;; val   := XxxxGet(xx, key)  or  XxxxGet(xx, key, default)
;;; val   := XxxxSet(xx, key, val)
;;; val   := XxxxUnset(xx, key)
;;;
;;; xx    := XxxxRemove(val, xx)    XxxxRemoveQ
;;; truth := XxxxMember?(val, xx)   XxxxMemberQ?
;;; xx    := XxxxUnion(xx1, xx2)
;;;
;;; The suffix "Q" means the test involved is "EQ".  "N" between the
;;; the type name and the function name proper means the function is
;;; non-copying (destructive).

;;;
;;; Pathnames
;;;

(defvar |TempFileDirectory| (pathname-directory "/tmp/"))
(defvar |LispFileType| "lisp")
(defvar |FaslFileType| "bbin")

(defun |Pathname| (name &optional (type nil) (dir 'none))
  (if (equal dir 'none)
      (make-pathname :name name :type type :defaults name) 
      (make-pathname :directory dir :name name :type type) ))

(defun |ToPathname| (string)
  (pathname string) )

;;; System-wide unique name on each call.
(defvar *new-pathname-counter* 1)

(defun |NewPathname| (&optional (prefix "t")(type nil)(dir '(:relative)))
   (let ((name 
          (format nil "~a~a-~a" 
            prefix (|OsProcessNumber|) *new-pathname-counter* )))
     (setq *new-pathname-counter* (+ *new-pathname-counter* 1))
     (make-pathname :directory dir :name name :type type) ))
         
;;; System-wide unique name for the current session.
(defun |SessionPathname| (&optional (prefix "t")(type nil)(dir '(:relative)))
   (let ((name (format nil "~a~a" prefix (|OsProcessNumber|))))
     (make-pathname :directory dir :name name :type type) ))
  
(defun |PathnameDirectory| (path)
  (pathname-directory path) )

(defun |PathnameName| (path)
  (pathname-name path) )

(defun |PathnameType| (path) 
  (pathname-type path) )


(defun |PathnameWithType| (path type)
  (make-pathname :type type :defaults path) )

(defun |PathnameWithoutType| (path)
  (make-pathname :type nil :defaults path) )


(defun |PathnameWithDirectory| (path dir)
  (make-pathname :directory dir :defaults path) )

(defun |PathnameWithoutDirectory| (path)
  (make-pathname :directory nil :defaults path) )


(defun |PathnameString| (path)
  (namestring path) )

(defun |PathnameToUsualCase| (path)
  (pathname (|StringLowerCase| (namestring path))) )


;; Lucid 1.01 specific  -- uses representation of directories.
(defun |PathnameAbsolute?| (path)
  (let ((dir (pathname-directory path)))
       (not (and (consp dir) (or
           (eq (car dir) :current) 
           (eq (car dir) :relative) ))) ))

;; Lucid 1.01 specific  -- uses representation of directories.
(defun |PathnameWithinDirectory| (dir relpath)
  (if (|PathnameAbsolute?| relpath)
    (|PlainError| "The path " relpath " cannot be used within directory " dir)
    (make-pathname 
       :directory (append dir (cdr (pathname-directory relpath)))
       :defaults  relpath )))

;; Unix specific -- uses unix file syntax.
(defun |PathnameDirectoryOfDirectoryPathname| (dirpath)
  (pathname-directory 
    (concatenate 'string  (namestring dirpath) "/junk.bar") ))

;; Unix specific -- uses environment variables.
(defun |PathnameWithinOsEnvVar| (varname relpath)
  (let ((envstr (|OsEnvGet| varname)))
    (parse-namestring (concatenate 'string envstr "/" relpath)) ))

;;;
;;; Symbols
;;;


;;!! Worry about packages a later day.  
;;!! For now, the responsibility of setting *package* is on the caller.
(defun |MakeSymbol| (str)
  (let ((a (intern str))) a) ) ; Return only 1 value

(defmacro |Symbol?| (ob)
  `(and ,ob (symbolp ,ob)) )

(defmacro |SymbolString| (sym)
  `(string ,sym) )

;;;
;;; Bits
;;;
(defmacro  |Bit| (x)
  (cond 
   ((eq x 1) 1) 
   ((eq x 0) 0) 
   (x 1) 
   (t 0)))

(defun |Bit?| (x) 
  (or (eql x 1) (eql x 0)) )

(defvar |TrueBit|  1)
(defvar |FalseBit| 0)

(defmacro  |BitOn?|   (b) `(eq ,b 1))

(defmacro |BitOr| (x y)
  `(bit-ior ,x ,y) )

;;;
;;; General Sequences
;;;
;;  ELT and SETELT work on these.

;; Removed because it clashed with size in vmlisp.lisp
;; (defun SIZE (x)  ;; #x in boot generates (SIZE x)
;;  (length x))

;;;
;;; Vectors
;;;
(defun |FullVector| (size &optional (init nil))
  (make-array 
   (list size)
   :element-type 't
   :initial-element init ))

(defun |Vector?| (x)
   (vectorp x) )

;;;
;;; Bit Vectors
;;;

;; Common Lisp simple bit vectors

(defun |FullBvec| (size &optional (init 0))
  (make-array 
   (list size)
   :element-type 'bit
   :initial-element init ))

;;;
;;; Characters
;;;

;;(defun |char| (x) 
;;  (char (string x) 0) )

(defmacro |Char| (x) 
  `(char (string ,x) 0) )

(defmacro |Char?| (c) 
  `(characterp ,c) )
  ;; (or (characterp a) 
  ;;     (and (symbolp a) (= (length (symbol-name a)) 1))))


(defmacro |CharCode| (c)
  `(char-code ,c) )

(defmacro |CharGreater?| (c1 c2) 
  `(char> ,c1 ,c2) )

(defun |CharDigit?| (x)
  (or 
   (and (characterp x) (digit-char-p x))
   (and (stringp x) (= (length x) 1) (digit-char-p (char x 0)))
   (and (symbolp x) (|CharDigit?| (string x))) ))

(defvar |SpaceChar|   #\Space)
(defvar |NewlineChar| #\Newline)

;;;
;;; Character Sets
;;;

(defun |Cset| (str) 
  (let 
   ((cset (make-array
           (list char-code-limit)
           :element-type 'bit
           :initial-element 0 ))
    (len (length str)) )
   
   (do ((i 0 (+ 1 i)))
       ((= i len))
       (setf (sbit cset (char-code (char str i))) 1) )
   cset ))

(defun |CsetMember?| (c cset)
  (eql 1 (sbit cset (char-code c))) )

(defun |CsetUnion| (cset1 cset2)
  (bit-ior cset1 cset2) )

(defun |CsetComplement| (cset)
  (bit-not cset) )

(defun |CsetString| (cset)
  (let 
   ((chars '())
    (len (length cset)))
   (do ((i 0 (+ 1 i)))
       ((= i len))
       (if (eql 1 (sbit cset i)) (push (string (code-char i)) chars)) )
   (apply #'concatenate (cons 'string (nreverse chars))) ))

(defvar |NumericCset|      (|Cset| "0123456789") )
(defvar |LowerCaseCset|    (|Cset| "abcdefghijklmnopqrstuvwxyz") )
(defvar |UpperCaseCset|    (|Cset| "ABCDEFGHIJKLMNOPQRSTUVWXYZ") )
(defvar |AlphaCset|        (|CsetUnion| |LowerCaseCset| |UpperCaseCset|))
(defvar |AlphaNumericCset| (|CsetUnion| |AlphaCset| |NumericCset|) )
(defvar |WhiteSpaceCset|   
  (|Cset| (coerce 
    (list #\Space #\Newline #\Tab #\Page #\Linefeed #\Return #\Backspace)
    'string )) )

;;;
;;; Character Strings 
;;;

;;  Common Lisp simple strings
;;  ELT and SETELT work on these.


(defun |FullString| (size &optional (init #\Space))
  (make-array
   (list size)
   :element-type 'character
   :initial-element init ))

(defun |ToString| (ob)
  (string ob) )

(defun |StringImage| (ob)
  (format nil "~a" ob) )

(defun |String?| (ob)
  (stringp ob) )

(defmacro |StringGetCode| (str ix)
  `(char-code (char ,str ,ix)) )

(defun |StringConcat| (&rest l)
  (progn
   (setq l (mapcar #'string l))
   (apply #'concatenate 'string l) ))

(defun |StringFromTo| (string from to)
  (subseq string from (+ to 1)) )

(defun |StringFromToEnd| (string from)
  (subseq string from) )

(defun |StringFromLong| (string from len)
  (subseq string from (+ from len)) )

(defun |StringPrefix?| (pref string)
  (let ((mm (mismatch pref string)))
       (or (not mm) (eql mm (length pref))) ))

(defun |StringUpperCase| (l)
  (cond ((stringp l) (string-upcase l))
        ((symbolp l) (intern (string-upcase (symbol-name l))))
        ((characterp l) (char-upcase l))
        ((atom l) l)
        (t (mapcar #'|StringUpperCase| l)) ))

(defun |StringLowerCase| (l)
  (cond ((stringp l) (string-downcase l))
        ((symbolp l) (intern (string-downcase (symbol-name l))))
        ((characterp l) (char-downcase L))
        ((atom l) l)
        (t (mapcar #'|StringLowerCase| l)) ))

(defun |StringGreater?| (s1 s2)
  (string> s1 s2) )

(defun |StringToInteger| (s)
  (read-from-string s) )

(defun |StringToFloat| (s)
  (read-from-string s) )

(defun |StringLength| (s)
  (length s) )

;;;
;;; Numbers
;;;



(defmacro |Number?|       (x) `(numberp ,x))
(defmacro |Integer?|      (x) `(integerp ,x))
(defmacro |Float?|        (x) `(floatp ,x))

(defmacro |Odd?|     (n)   `(oddp ,n))
(defmacro |Remainder|(a b) `(rem ,a ,b))

(defmacro |DoublePrecision| (x) `(coerce ,x 'double-precision))

(defmacro |Abs|   (x) `(abs  ,x))
(defmacro |Min|   (x &rest yz) `(min ,x ,@yz))
(defmacro |Max|   (x &rest yz) `(max ,x ,@yz))

(defmacro |Exp|   (x) `(exp ,x))
(defmacro |Ln|    (x) `(log ,x))
(defmacro |Log10| (x) `(log ,x 10))
(defmacro |Sin|   (x) `(sin ,x))
(defmacro |Cos|   (x) `(cos ,x))
(defmacro |Tan|   (x) `(tan ,x))
(defmacro |Cotan| (x) `(/ 1.0 (tan ,x)))
(defmacro |Arctan|(x) `(atan ,x))

;;;
;;; Pairs
;;;

(defmacro |Pair?| (x) `(consp ,x))

(defmacro |car|    (x) `(car    ,x))
(defmacro |cdr|    (x) `(cdr    ,x))

(defmacro |caar|   (x) `(caar   ,x))
(defmacro |cadr|   (x) `(cadr   ,x))
(defmacro |cdar|   (x) `(cdar   ,x))
(defmacro |cddr|   (x) `(cddr   ,x))

(defmacro |caaar|  (x) `(caaar  ,x))
(defmacro |caadr|  (x) `(caadr  ,x))
(defmacro |cadar|  (x) `(cadar  ,x))
(defmacro |caddr|  (x) `(caddr  ,x))
(defmacro |cdaar|  (x) `(cdaar  ,x))
(defmacro |cdadr|  (x) `(cdadr  ,x))
(defmacro |cddar|  (x) `(cddar  ,x))
(defmacro |cdddr|  (x) `(cdddr  ,x))

(defmacro |FastCar|    (x) `(car (the cons ,x)))
(defmacro |FastCdr|    (x) `(cdr (the cons ,x)))

(defmacro |FastCaar|   (x) `(|FastCar| (|FastCar| ,x)))
(defmacro |FastCadr|   (x) `(|FastCar| (|FastCdr| ,x)))
(defmacro |FastCdar|   (x) `(|FastCdr| (|FastCar| ,x)))
(defmacro |FastCddr|   (x) `(|FastCdr| (|FastCdr| ,x)))

(defmacro |FastCaaar|  (x) `(|FastCar| (|FastCaar| ,x)))
(defmacro |FastCaadr|  (x) `(|FastCar| (|FastCadr| ,x)))
(defmacro |FastCadar|  (x) `(|FastCar| (|FastCdar| ,x)))
(defmacro |FastCaddr|  (x) `(|FastCar| (|FastCddr| ,x)))
(defmacro |FastCdaar|  (x) `(|FastCdr| (|FastCaar| ,x)))
(defmacro |FastCdadr|  (x) `(|FastCdr| (|FastCadr| ,x)))
(defmacro |FastCddar|  (x) `(|FastCdr| (|FastCdar| ,x)))
(defmacro |FastCdddr|  (x) `(|FastCdr| (|FastCddr| ,x)))

(defmacro |IfCar| (x) `(if (consp ,x) (car ,x)))
(defmacro |IfCdr| (x) `(if (consp ,x) (cdr ,x)))

(defmacro |EqCar| (l a) `(eq (car ,l) ,a))
(defmacro |EqCdr| (l d) `(eq (cdr ,l) ,d))

;;;
;;; Lists
;;;


(defun |ListNReverse| (l)
  (nreverse l) )

(defun |ListIsLength?| (l n)
  (if l (= n 0) (|ListIsLength?| (cdr l) (1- n))) )

;;--------------------> NEW DEFINITION (override in vmlisp.lisp.pamphlet)
(defun |ListMemberQ?| (ob l)
  (member ob l :test #'eq) )

(defun |ListRemoveQ| (ob l)
  (remove ob l :test #'eq :count 1) )

(defun |ListNRemoveQ| (ob l)
  (delete ob l :test #'eq :count 1) )

(defun |ListRemoveDuplicatesQ| (l)
  (remove-duplicates l :test #'eq) )

(defun |ListUnion| (l1 l2)
  (union l1 l2 :test #'equal) )

(defun |ListUnionQ| (l1 l2)
  (union l1 l2 :test #'eq) )

(defun |ListIntersection| (l1 l2)
  (intersection l1 l2 :test #'equal) )

(defun |ListIntersectionQ| (l1 l2)
  (intersection l1 l2 :test #'eq) )

(defun |ListAdjoin| (ob l)
  (adjoin ob l :test #'equal) )

(defun |ListAdjoinQ| (ob l)
  (adjoin ob l :test #'eq) )

;;;
;;; Association lists
;;;


(defun |AlistAssoc| (key l)
  (assoc key l :test #'equal) )

;;--------------------> NEW DEFINITION (override in vmlisp.lisp.pamphlet)
(defun |AlistAssocQ| (key l)
  (assoc key l :test #'eq) )

(defun |AlistRemove| (key l)
  (let ((pr (assoc key l :test #'equal)))
       (if pr 
           (remove pr l :test #'equal) 
           l) ))

(defun |AlistRemoveQ| (key l)
  (let ((pr (assoc key l :test #'eq)))
       (if pr 
           (remove pr l :test #'eq) 
           l) ))

(defun |AlistAdjoinQ| (pr l)
  (cons pr (|AlistRemoveQ| (car pr) l)) )

(defun |AlistUnionQ| (l1 l2)
  (union l1 l2 :test #'eq :key #'car) )

;;;
;;; Tables
;;;

;;(defmacro |EqTable| ()
;;  `(make-hash-table :test #'eq) )
;;(defmacro |EqualTable| ()
;;  `(make-hash-table :test #'equal) )
;;(defmacro |StringTable| ()
;;  `(make-hash-table :test #'equal) )
;; following is not used and causes CCL problems
;;(defmacro |SymbolTable| ()
;;  `(make-hash-table :test #'eq) )


(defmacro |Table?| (ob)
  `(hash-table-p ,ob) )

(defmacro |TableCount| (tab)
  `(hash-table-count ,tab) )

(defmacro |TableGet| (tab key &rest default)
  `(gethash ,key ,tab ,@default) )

(defmacro |TableSet| (tab key val)
  `(setf (gethash ,key ,tab) ,val) )

(defun |TableUnset| (tab key)
  (let ((val (gethash key tab)))
       (remhash key tab)
       val ))

(defun |TableKeys| (tab)
  (let ((key-list nil))
       (maphash 
        #'(lambda (key val) (declare (ignore val))
                  (setq key-list (cons key key-list)) )
        tab )
       key-list ))

;; CCL supplies a slightly more efficient version of logs to base 10, which
;; is useful in the WIDTH function. MCD.
#+:KCL (defun log10 (u) (log u 10))
