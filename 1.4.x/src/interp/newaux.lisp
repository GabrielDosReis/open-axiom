;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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

; PURPOSE: This file sets up properties which are used by the Boot lexical
;          analyzer for bottom-up recognition of operators.  Also certain
;          other character-class definitions are included, as well as
;          table accessing functions.
;
; ORGANIZATION: Each section is organized in terms of Creation and Access code.
;
;               1. Led and Nud Tables
;               2. GLIPH  Table
;               4. GENERIC Table
;               5. Character syntax class predicates

; **** 1. LED and NUD Tables
 
; ** TABLE PURPOSE
 
; Led and Nud have to do with operators. An operator with a Led property takes
; an operand on its left (infix/suffix operator).
 
; An operator with a Nud takes no operand on its left (prefix/nilfix).
; Some have both (e.g. - ).  This terminology is from the Pratt parser.
; The translator for Scratchpad II is a modification of the Pratt parser which
; branches to special handlers when it is most convenient and practical to
; do so (Pratt's scheme cannot handle local contexts very easily).
 
; Both LEDs and NUDs have right and left binding powers.  This is meaningful 
; for prefix and infix operators.  These powers are stored as the values of 
; the LED and NUD properties of an atom, if the atom has such a property. 
; The format is:
 
;       <Operator Left-Binding-Power  Right-Binding-Power <Special-Handler>>
 
; where the Special-Handler is the name of a function to be evaluated when that
; keyword is encountered.
 
; The default values of Left and Right Binding-Power are NIL.  NIL is a 
; legitimate value signifying no precedence.  If the Special-Handler is NIL,
; this is just an ordinary operator (as opposed to a surfix operator like 
; if-then-else).
 


(IMPORT-MODULE "sys-macros") 
(IMPORT-MODULE "sys-utility") 
(in-package "BOOT")
 
; ** TABLE CREATION
 
(defparameter OpAssoc nil 
  "Information used by OUT BOOT operator pretty printing routines")

(defun MAKEOP (X Y KEYNAME)
  (if (OR (NOT (CDR X)) (NUMBERP (SECOND X)))
      (SETQ X (CONS (FIRST X) X)))
  (if (AND (alpha-char-p (ELT (STRINGIMAGE (FIRST X)) 0))
           (NOT (MEMBER (FIRST X) (EVAL KEYNAME))))
      (SET KEYNAME (CONS (FIRST X) (EVAL KEYNAME))))
  (MAKEPROP (FIRST X) Y X)
  (SETQ OPASSOC (ADDASSOC Y (CONS (CONS X X) (LASSOC Y OPASSOC)) OPASSOC))
  (SECOND X))
 
(defvar |PARSE-NewKEY| nil) ;;list of keywords
 
(defun SPECIALCASESYNTAX () (OR (AND (char= TOK '#\#) (DIGITP CHR))))
 
(defun TERMINATOR (CHR)
  (member CHR '(#\  #\( #\) #\. #\; #\, #\Return)) :test #'char=)
 
