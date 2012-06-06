;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
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

;; NAME:    Pre-Parsing Code
;; PURPOSE: BOOT lines are massaged by preparse to make them easier to parse:
;;        1. Trailing -- comments are removed (this is already done, actually).
;;        2. Comments between { and } are removed.
;;        3. BOOT code is column-sensitive. Code which lines up columnarly is
;;           parenthesized and semicolonized accordingly.  For example,
;;
;;                a
;;                        b
;;                        c
;;                                d
;;                e
;;
;;           becomes
;;
;;                a
;;                        (b;
;;                         c
;;                                d)
;;                e
;;
;;           Note that to do this correctly, we also need to keep track of
;;           parentheses already in the code.
 


(IMPORT-MODULE "lexing")
 
(in-package "BOOT")
 
; Global storage
 
(defparameter $INDEX 0                          "File line number of most recently read line.")
(defparameter |$preparseLastLine| ()            "Most recently read line.")
(defparameter |$preparseReportIfTrue| NIL         "Should we print listings?")
(defparameter $IOIndex 0                        "Number of latest terminal input line.")
 
(DEFPARAMETER TOK NIL)
(DEFPARAMETER DEFINITION_NAME NIL)

(defun Initialize-Preparse (rd)
  (setq $INDEX 0)
  (setq |$preparseLastLine| (|readLine| (|readerInput| rd))))
 
(defvar $skipme)
 
(defun |preparse1| (rd)
 (PROG (NUM A I L PSLOC
        INSTRING PCOUNT COMSYM STRSYM OPARSYM CPARSYM N NCOMSYM
        (SLOC -1) (CONTINUE NIL)  (PARENLEV 0) (NCOMBLOCK ())
        (LINES ()) (LOCS ()) (NUMS ()) functor  )
 READLOOP (DCQ (NUM . A) (|preparseReadLine| rd))
         (cond ((|atEndOfUnit?| A)
                (|preparseEcho| (|readerPendingLines| rd))
                (COND ((NULL LINES) (RETURN NIL))
                      (NCOMBLOCK
                       (|findCommentBlock| NIL NUMS LOCS NCOMBLOCK)))
                (RETURN (|pairList| (|reverse!| NUMS)
			 (|parsePiles| (|reverse!| LOCS) (|reverse!| LINES))))))
         (cond ((and (NULL LINES) (> (LENGTH A) 0) (EQ (CHAR A 0) #\) ))
                ; this is a command line, don't parse it
                (|preparseEcho| (|readerPendingLines| rd))
                (setq |$preparseLastLine| nil) ;don't reread this line
                (SETQ LINE a)
                (CATCH |$SpadReaderTag| (|doSystemCommand| (subseq LINE 1)))
                (GO READLOOP)))
         (setq L (LENGTH A))
         (if (EQ L 0)
	     (GO READLOOP))
         (setq PSLOC SLOC)
         (setq I 0 
	       INSTRING ()
	       PCOUNT 0)
 STRLOOP (setq STRSYM (OR (|findChar| #\" A I) L))
         (setq COMSYM (OR (|findString| "--" A I) L))
         (setq NCOMSYM (OR (|findString| "++" A I) L))
         (setq OPARSYM (OR (|findChar| #\( A I) L))
         (setq CPARSYM (OR (|findChar| #\) A I) L))
         (setq N (MIN STRSYM COMSYM NCOMSYM OPARSYM CPARSYM))
         (cond ((= N L)
		(GO NOCOMS))
               ((|escaped?| A N))
               ((= N STRSYM) (setq INSTRING (NOT INSTRING)))
               (INSTRING)
               ((= N COMSYM)
		(setq A (subseq A 0 N))
		(GO NOCOMS)) ; discard trailing comment
               ((= N NCOMSYM)
                (setq SLOC (|indentationLocation| A))
                (COND
                  ((= SLOC N)
                   (COND ((AND NCOMBLOCK (NOT (= N (CAR NCOMBLOCK))))
                          (|findCommentBlock| NUM NUMS LOCS NCOMBLOCK)
                          (SETQ NCOMBLOCK NIL)))
                   (SETQ NCOMBLOCK (CONS N (CONS A (IFCDR NCOMBLOCK))))
                   (SETQ A ""))
                  ('T 
		   (|readerDeferLine| rd (STRCONC (|makeString| N #\Space)
						  (SUBSTRING A N NIL)))
		   (SETQ $INDEX (1- $INDEX))
		   (SETQ A (SUBSEQ A 0 N))))
		(GO NOCOMS))
               ((= N OPARSYM) (setq PCOUNT (1+ PCOUNT)))
               ((= N CPARSYM) (setq PCOUNT (1- PCOUNT))))
         (setq I (1+ N))
         (GO STRLOOP)
 NOCOMS  (setq SLOC (|indentationLocation| A))
         (setq A (|trimTrailingBlank| A))
         (cond ((NULL SLOC)
		(setq SLOC PSLOC)
		(GO READLOOP)))
         (cond ((EQ (ELT A (|maxIndex| A)) #\_)
                (setq CONTINUE T a (subseq A (|maxIndex| A))))
               ((setq CONTINUE NIL)))
         (if (and (null LINES) (= SLOC 0)) ;;test for skipping constructors
             (if (and |$byConstructors|
                      (null (|findString| "==>" a))
                      (not (member (setq functor (intern
                                    (substring a 0 (STRPOSL ": (=" A 0 NIL))))
                                   |$byConstructors|)))
                 (setq $skipme 't)
               (progn (push functor |$constructorsSeen|)
		      (setq $skipme nil))))
         (when (and LINES (EQL SLOC 0))
             (IF (AND NCOMBLOCK (NOT (ZEROP (CAR NCOMBLOCK))))
		 (|findCommentBlock| NUM NUMS LOCS NCOMBLOCK))
             (RETURN (|pairList| (|reverse!| NUMS)
                        (|parsePiles| (|reverse!| LOCS) (|reverse!| LINES)))))
         (cond ((> PARENLEV 0)
		(PUSH NIL LOCS)
		(setq SLOC PSLOC)
		(GO REREAD)))
         (COND (NCOMBLOCK
                (|findCommentBlock| NUM NUMS LOCS NCOMBLOCK)
                (setq NCOMBLOCK ())))
         (PUSH SLOC LOCS)
 REREAD  (|preparseEcho| (|readerPendingLines| rd))
         (PUSH A LINES)
         (PUSH NUM NUMS)
         (setq PARENLEV (+ PARENLEV PCOUNT))
         (GO READLOOP)))
