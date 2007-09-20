;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


(in-package "BOOT")

#+(and :Lucid (not :ibm/370))
(progn
;  (system:define-foreign-function :c '|findString| :fixnum)
  (system:define-foreign-function :c '|addtopath|  :fixnum)
  (system:define-foreign-function :c '|chdir|      :fixnum)
  (system:define-foreign-function :c '|writeablep| :fixnum)
  (system:define-foreign-function :c '|directoryp| :fixnum)
  (system:define-foreign-function :c '|copyEnvValue| :fixnum)
  )

#+KCL
(progn
  (defentry |directoryp| (string)        (int "directoryp"))
  (defentry |writeablep| (string)        (int "writeablep"))
;  (defentry |findString| (string string) (int "findString"))
  )

#+:CCL
(defun |directoryp| (fn)
  (cond ((not (probe-file fn)) -1)
        ((directoryp fn) 1)
        (t 0)))



; (defun |findStringInFile| (str p) 
;     (|findString| (namestring p) str) )


(defun |getEnv| (var-name)  (system::getenv var-name))

;;stolen from AXIOM-XL src/strops.c
#+(AND KCL (NOT ELF))
(Clines 
"MYHASH(s)"
"char *s;"
"{"
" register unsigned int   h = 0;"
" register int    c;"
""
" while ((c = *s++) != 0) {"
"  h ^= (h << 8);"
"  h += ((c) + 200041);"
"  h &= 0x3FFFFFFF;"
" }"
" return h;"
"}"
)
#+(AND KCL (NOT ELF))
(defentry |hashString| (string) (int "MYHASH"))
#+(AND KCL ELF)
(defun |hashString| (string) (system:|hashString| string))

#+(AND KCL (NOT ELF))
(Clines
"int MYCOMBINE(i,j)"
"int i,j;"
"{"
"return ( (((((unsigned int)j) & 16777215) << 6)+((unsigned int)i)) % 1073741789);"
"}"
)
#+(AND KCL (NOT ELF))
(defentry |hashCombine| (int int) (int "MYCOMBINE"))
#+(AND KCL  ELF)
(defun |hashCombine| (x y) (system:|hashCombine| x y))


