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


import '"i-toplev"
(in-package "BOOT")

(defun |intSayKeyedMsg| (key args)
  (|sayKeyedMsg| (|packageTran| key) (|packageTran| args)))

;;(defun |intMakeFloat| (int frac len exp)
;;  (MAKE-FLOAT int frac len exp))

;;(defun |intSystemCommand| (command)
;;  (catch 'SPAD_READER
;;    (|systemCommand| (|packageTran| command))))

;;(defun |intUnAbbreviateKeyword| (keyword)
;;  (|unAbbreviateKeyword| (|packageTran| keyword)))

(defun |intProcessSynonyms| (str)
  (let ((LINE str))
    (declare (special LINE))
    (|processSynonyms|)
    LINE))

;; (defun |intNoParseCommands| ()
;;   |$noParseCommands|)

;;(defun |intTokenCommands| ()
;; |$tokenCommands|)

(defun |intInterpretPform| (pf)
  (|processInteractive| (|zeroOneTran| (|packageTran| (|pf2Sex| pf))) pf))

;;(defun |intSpadThrow| ()
;;  (|spadThrow|))

;;(defun |intMKPROMPT| (should? step)
;;  (if should? (PRINC (MKPROMPT))))

(defvar |$intCoerceFailure| '|coerceFailure|)
(defvar |$intTopLevel| '|top_level|)
(defvar |$intSpadReader| 'SPAD_READER)
(defvar |$intRestart| '|restart|)

;;(defun |intString2BootTree| (str)
;;  (|string2BootTree| str))

;;(defun |intPackageTran| (sex)
;;  (|packageTran| sex))

;;--------------------> NEW DEFINITION (override in i-syscmd.boot.pamphlet)
(defun |stripSpaces| (str)
  (string-trim '(#\Space) str))

;;(defvar |$SessionManager| |$SessionManager|)
;;(defvar |$EndOfOutput| |$EndOfOutput|)

;;(defun |intServerReadLine| (foo)
;;  (|serverReadLine| foo))

;; (defun |intProcessSynonym| (str)
;;   (|npProcessSynonym| str))

(defun |SpadInterpretFile| (fn)
      (|SpadInterpretStream| 1 fn nil) )

(defun |intNewFloat| ()
  (list '|Float|))

;; (defun |intDoSystemCommand| (string)
;;   (|doSystemCommand| string))

(defun |intSetNeedToSignalSessionManager| ()
  (setq |$NeedToSignalSessionManager| T))

;; (defun |intKeyedSystemError| (msg args)
;;   (|keyedSystemError| msg args))

;;#-:CCL
;;(defun |stashInputLines| (l)
;;  (|stashInputLines| l))

;;(defun |setCurrentLine| (s)
;;  (setq |$currentLine| s))

(defun |setCurrentLine| (s)
  (setq |$currentLine|
        (cond ((null |$currentLine|) s)
              ((stringp |$currentLine|)
               (cons |$currentLine|
                         (if (stringp s) (cons s nil) s)))
              (t (rplacd (last |$currentLine|)
                         (if (stringp s) (cons s nil) s))
                 |$currentLine|))))

(defun |intnplisp| (s)
 (setq |$currentLine| s)
 (|nplisp| |$currentLine|))

;; (defun |intResetStackLimits| () (|resetStackLimits|))

(defun |intSetQuiet| ()
  (setq |$QuietCommand| T))

(defun |intUnsetQuiet| ()
  (setq |$QuietCommand| NIL))

;; (defun |expandTabs| (s)
;;   (expand-tabs s))

;; #-:CCL
;; (defun |leaveScratchpad| ()
;;   (|leaveScratchpad|))

;;(defun |readingFile?| ()
;;  |$ReadingFile|)

