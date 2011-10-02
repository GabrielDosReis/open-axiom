;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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


;; this files contains basic routines for error handling
(import-module "macros")
(in-package "BOOT")

(defun error-format (message args)
  (let ((|$BreakMode| '|break|))
    (declare (special |$BreakMode|))
   (if (stringp message) (apply #'format nil message args) nil)))
 
;;(defmacro |trappedSpadEval| (form) form) ;;nop for now

#+:gcl
(defun |resetStackLimits| () (system:reset-stack-limits))
#-:gcl
(defun |resetStackLimits| () nil)

(defvar |$oldBreakMode|)

;; following macro evaluates form returning Maybe type-of form
#+:gcl
(defmacro |trapNumericErrors| (form)
  `(let ((|$oldBreakMode| |$BreakMode|)
         (|$BreakMode| '|trapNumerics|)
	 (val))
     (catch '|trapNumerics| ,form)))

#-:gcl
(defmacro |trapNumericErrors| (form)
  `(handler-case ,form
		 (arithmetic-error () |%nothing|)))

;; the following form embeds around the akcl error handler
#+:gcl
(eval-when
 (load eval)
 (unembed 'system:universal-error-handler)
 (embed 'system:universal-error-handler
            '(lambda (type correctable? op
                           continue-string error-string &rest args)
               (block
                nil
                (setq |$NeedToSignalSessionManager| T)
                (if (and (boundp '|$inLispVM|) (boundp '|$BreakMode|))
                    (cond ((eq |$BreakMode| '|validate|)
                           (|systemError| (error-format error-string args)))
                          ((and (eq |$BreakMode| '|trapNumerics|)
                                (eq type :ERROR))
                           (setq |$BreakMode| nil)
			   (throw '|trapNumerics| |%nothing|))
                          ((and (eq |$BreakMode| '|trapNumerics|)
                                (boundp '|$oldBreakMode|)
                                (setq |$BreakMode| |$oldBreakMode|)
                                nil)) ;; resets error handler
                          ((and (null |$inLispVM|)
                                (|symbolMember?| |$BreakMode| '(|nobreak| |query| |resume|)))
                           (let ((|$inLispVM| T)) ;; turn off handler
                             (return
                              (|systemError| (error-format error-string args)))))
                          ((eq |$BreakMode| '|letPrint2|)
                           (setq |$BreakMode| nil)
                           (throw '|letPrint2| nil))))
                (apply system:universal-error-handler type correctable? op
                       continue-string error-string args )))))










