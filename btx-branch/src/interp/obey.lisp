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
(import-module "sys-os")
(in-package "BOOT")

#+ (and :lucid :unix)
(defun OBEY (S)
   (system:run-aix-program (|makeAbsoluteFilename| "/lib/obey")
             :arguments (list "-c" S)))

#+ (and :lucid :unix)
(defun delete-directory (dirname)
   (system:run-aix-program "rm" :arguments (list "-r" dirname)))

#+ (and :lucid :unix)
(defun move-file (namestring1 namestring2)
  (system:run-aix-program "mv" :arguments (list namestring1 namestring2)))

#+ (and :lucid :unix)
(defun copy-lib-directory (name1 name2)
   (makedir name2)
   (system:run-aix-program "sh" :arguments
                        (list "-c" (concat "cp " name1 "/* " name2))))

#+ (and :lucid :unix)
(defun copy-file (namestring1 namestring2)
  (system:run-aix-program "cp" :arguments (list namestring1 namestring2)))


#+(or :sbcl :clisp)
(defun obey(s) (|runCommand| s))
