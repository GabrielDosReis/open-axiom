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

;; % Binding powers stored under the Led and Red properties of an operator
;; % are set up by the file BOTTOMUP.LISP.  The format for a Led property
;; % is <Operator Left-Power Right-Power>, and the same for a Nud, except that
;; % it may also have a fourth component <Special-Handler>. ELEMN attempts to
;; % get the Nth indicator, counting from 1.

(IMPORT-MODULE "parsing")
(IN-PACKAGE "BOOT" )


(DEFPARAMETER |tmptok| NIL)
(DEFPARAMETER TOK NIL)
(DEFPARAMETER |ParseMode| NIL)
(DEFPARAMETER DEFINITION_NAME NIL)
(DEFPARAMETER LABLASOC NIL)

(defun |isTokenDelimiter| () 
       (MEMBER (CURRENT-SYMBOL) '(\) END\_UNIT NIL)))

(DEFUN |PARSE-NewExpr| ()
  (OR (AND (MATCH-STRING ")") (ACTION (|processSynonyms|))
           (MUST (|PARSE-Command|)))
      (AND (ACTION (SETQ DEFINITION_NAME (CURRENT-SYMBOL)))
           (|PARSE-Statement|)))) 


(DEFUN |PARSE-Command| ()
  (AND (MATCH-ADVANCE-STRING ")") (MUST (|PARSE-SpecialKeyWord|))
       (MUST (|PARSE-SpecialCommand|))
       (PUSH-REDUCTION '|PARSE-Command| NIL))) 


(DEFUN |PARSE-SpecialKeyWord| ()
  (AND (MATCH-CURRENT-TOKEN 'IDENTIFIER)
       (ACTION (SETF (TOKEN-SYMBOL (CURRENT-TOKEN))
                     (|unAbbreviateKeyword| (CURRENT-SYMBOL)))))) 


(DEFUN |PARSE-SpecialCommand| ()
  (OR (AND (MATCH-ADVANCE-STRING "show")
           (BANG FIL_TEST
                 (OPTIONAL
                     (OR (MATCH-ADVANCE-STRING "?")
                         (|PARSE-Expression|))))
           (PUSH-REDUCTION '|PARSE-SpecialCommand|
               (CONS '|show| (CONS (POP-STACK-1) NIL)))
           (MUST (|PARSE-CommandTail|)))
      (AND (MEMBER (CURRENT-SYMBOL) |$noParseCommands|)
           (ACTION (FUNCALL (CURRENT-SYMBOL))))
      (AND (MEMBER (CURRENT-SYMBOL) |$tokenCommands|)
           (|PARSE-TokenList|) (MUST (|PARSE-TokenCommandTail|)))
      (AND (STAR REPEATOR (|PARSE-PrimaryOrQM|))
           (MUST (|PARSE-CommandTail|))))) 


(DEFUN |PARSE-TokenList| ()
  (STAR REPEATOR
        (AND (NOT (|isTokenDelimiter|))
             (PUSH-REDUCTION '|PARSE-TokenList| (CURRENT-SYMBOL))
             (ACTION (ADVANCE-TOKEN))))) 


(DEFUN |PARSE-TokenCommandTail| ()
  (AND (BANG FIL_TEST (OPTIONAL (STAR REPEATOR (|PARSE-TokenOption|))))
       (|atEndOfLine|)
       (PUSH-REDUCTION '|PARSE-TokenCommandTail|
           (CONS (POP-STACK-2) (APPEND (POP-STACK-1) NIL)))
       (ACTION (|systemCommand| (POP-STACK-1))))) 


(DEFUN |PARSE-TokenOption| ()
  (AND (MATCH-ADVANCE-STRING ")") (MUST (|PARSE-TokenList|)))) 


(DEFUN |PARSE-CommandTail| ()
  (AND (BANG FIL_TEST (OPTIONAL (STAR REPEATOR (|PARSE-Option|))))
       (|atEndOfLine|)
       (PUSH-REDUCTION '|PARSE-CommandTail|
           (CONS (POP-STACK-2) (APPEND (POP-STACK-1) NIL)))
       (ACTION (|systemCommand| (POP-STACK-1))))) 


(DEFUN |PARSE-PrimaryOrQM| ()
  (OR (AND (MATCH-ADVANCE-STRING "?")
           (PUSH-REDUCTION '|PARSE-PrimaryOrQM| '?))
      (|PARSE-Primary|))) 


(DEFUN |PARSE-Option| ()
  (AND (MATCH-ADVANCE-STRING ")")
       (MUST (STAR REPEATOR (|PARSE-PrimaryOrQM|))))) 


(DEFUN |PARSE-Statement| ()
  (AND (|PARSE-Expr| 0)
       (OPTIONAL
           (AND (STAR REPEATOR
                      (AND (MATCH-ADVANCE-STRING ",")
                           (MUST (|PARSE-Expr| 0))))
                (PUSH-REDUCTION '|PARSE-Statement|
                    (CONS '|Series|
                          (CONS (POP-STACK-2)
                                (APPEND (POP-STACK-1) NIL)))))))) 


(DEFUN |PARSE-InfixWith| ()
  (AND (|PARSE-With|)
       (PUSH-REDUCTION '|PARSE-InfixWith|
           (CONS '|Join| (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-With| ()
  (AND (MATCH-ADVANCE-KEYWORD "with") (MUST (|PARSE-Category|))
       (PUSH-REDUCTION '|PARSE-With|
           (CONS '|with| (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-Category| ()
  (PROG (G1)
    (RETURN
      (OR (AND (MATCH-ADVANCE-KEYWORD "if") (MUST (|PARSE-Expression|))
               (MUST (MATCH-ADVANCE-KEYWORD "then"))
               (MUST (|PARSE-Category|))
               (BANG FIL_TEST
                     (OPTIONAL
                         (AND (MATCH-ADVANCE-KEYWORD "else")
                              (MUST (|PARSE-Category|)))))
               (PUSH-REDUCTION '|PARSE-Category|
                   (CONS '|if|
                         (CONS (POP-STACK-3)
                               (CONS (POP-STACK-2)
                                     (CONS (POP-STACK-1) NIL))))))
          (AND (MATCH-ADVANCE-STRING "(") (MUST (|PARSE-Category|))
               (BANG FIL_TEST
                     (OPTIONAL
                         (STAR REPEATOR
                               (AND (MATCH-ADVANCE-STRING ";")
                                    (MUST (|PARSE-Category|))))))
               (MUST (MATCH-ADVANCE-STRING ")"))
               (PUSH-REDUCTION '|PARSE-Category|
                   (CONS 'CATEGORY
                         (CONS (POP-STACK-2)
                               (APPEND (POP-STACK-1) NIL)))))
          (AND (ACTION (SETQ G1 (LINE-NUMBER CURRENT-LINE)))
               (OR (|PARSE-Application|)
		   (|PARSE-OperatorFunctionName|))
               (MUST (OR (AND (MATCH-ADVANCE-STRING ":")
                              (MUST (|PARSE-Expression|))
                              (PUSH-REDUCTION '|PARSE-Category|
                                  (CONS '|%Signature|
                                        (CONS (POP-STACK-2)
                                         (CONS (POP-STACK-1) NIL))))
                              (ACTION (|recordSignatureDocumentation|
                                       (NTH-STACK 1) G1)))
                         (AND (PUSH-REDUCTION '|PARSE-Category|
                                  (CONS '|%Attribute|
                                        (CONS (POP-STACK-1) NIL)))
                              (ACTION (|recordAttributeDocumentation|
                                       (NTH-STACK 1) G1)))))))))) 


(DEFUN |PARSE-Expression| ()
  (AND (|PARSE-Expr|
           (|PARSE-rightBindingPowerOf| (MAKE-SYMBOL-OF PRIOR-TOKEN)
               |ParseMode|))
       (PUSH-REDUCTION '|PARSE-Expression| (POP-STACK-1)))) 


(DEFUN |PARSE-Import| ()
  (AND (MATCH-ADVANCE-KEYWORD "import") 
       (MUST (|PARSE-Expr| 1000))
       (OR (AND (MATCH-ADVANCE-STRING ":")
                (MUST (|PARSE-Expression|))
                (MUST (MATCH-ADVANCE-KEYWORD "from"))
                (MUST (|PARSE-Expr| 1000))
                (PUSH-REDUCTION '|PARSE-Import|
		    (CONS '|%SignatureImport|
			  (CONS (POP-STACK-3)
				(CONS (POP-STACK-2)
				      (CONS (POP-STACK-1) NIL))))))
           (AND (BANG FIL_TEST
		      (OPTIONAL
		       (STAR REPEATOR
			     (AND (MATCH-ADVANCE-STRING ",")
				  (MUST (|PARSE-Expr| 1000))))))
		(PUSH-REDUCTION '|PARSE-Import|
                    (CONS '|import|
			  (CONS (POP-STACK-2) (APPEND (POP-STACK-1) NIL))))))))

;; domain inlining.  Same syntax as import directive; except
;; deliberate restriction on naming one type at a time.
;; -- gdr, 2009-02-28.
(DEFUN |PARSE-Inline| ()
  (AND (MATCH-ADVANCE-KEYWORD "inline")
       (MUST (|PARSE-Expr| 1000))
       (PUSH-REDUCTION '|PARSE-Inline|
           (CONS '|%Inline| (CONS (POP-STACK-1) NIL)))))

;; quantified types.  At the moment, these are used only in
;; pattern-mathing cases.
;; -- gdr, 2009-06-14.
(DEFUN |PARSE-Scheme| ()
  (OR (AND (|PARSE-Quantifier|)
	   (MUST (|PARSE-QuantifiedVariableList|))
	   (MUST (MATCH-ADVANCE-STRING "."))
	   (MUST (|PARSE-Expr| 200))
	   (MUST (PUSH-REDUCTION '|PARSE-Forall|
				 (CONS (POP-STACK-3)
				       (CONS (POP-STACK-2)
					     (CONS (POP-STACK-1) NIL))))))
      (|PARSE-Application|)))

(DEFUN |PARSE-Quantifier| ()
  (OR (AND (MATCH-ADVANCE-KEYWORD "forall")
	   (MUST (PUSH-REDUCTION '|PARSE-Quantifier| '|%Forall|)))
      (AND (MATCH-ADVANCE-KEYWORD "exist")
	   (MUST (PUSH-REDUCTION '|PARSE-Quantifier| '|%Exist|)))))

(DEFUN |PARSE-QuantifiedVariableList| ()
  (AND (MATCH-ADVANCE-STRING "(")
       (MUST (|PARSE-QuantifiedVariable|))
       (OPTIONAL 
	(AND (STAR REPEATOR 
		   (AND (MATCH-ADVANCE-STRING ",")
			(MUST (|PARSE-QuantifiedVariable|))))
	     (PUSH-REDUCTION '|PARSE-QuantifiedVariableList|
			     (CONS '|%Sequence|
				   (CONS (POP-STACK-2) 
					 (APPEND (POP-STACK-1) NIL))))))
       (MUST (MATCH-ADVANCE-STRING ")"))))

(DEFUN |PARSE-QuantifiedVariable| ()
  (AND (PARSE-IDENTIFIER)
       (MUST (MATCH-ADVANCE-STRING ":"))
       (MUST (|PARSE-Application|))
       (MUST (PUSH-REDUCTION '|PARSE-QuantifiedVariable|
			     (CONS '|:| 
				   (CONS (POP-STACK-2)
					 (CONS (POP-STACK-1) NIL)))))))

(DEFUN |PARSE-Infix| ()
  (AND (PUSH-REDUCTION '|PARSE-Infix| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (OPTIONAL (|PARSE-TokTail|))
       (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Infix|
           (CONS (POP-STACK-2)
                 (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Prefix| ()
  (AND (PUSH-REDUCTION '|PARSE-Prefix| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (OPTIONAL (|PARSE-TokTail|))
       (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Prefix|
           (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-Suffix| ()
  (AND (PUSH-REDUCTION '|PARSE-Suffix| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (OPTIONAL (|PARSE-TokTail|))
       (PUSH-REDUCTION '|PARSE-Suffix|
           (CONS (POP-STACK-1) (CONS (POP-STACK-1) NIL))))) 


(DEFUN |PARSE-TokTail| ()
  (PROG (G1)
    (RETURN
      (AND (EQ (CURRENT-SYMBOL) '$)
           (OR (ALPHA-CHAR-P (CURRENT-CHAR))
               (CHAR-EQ (CURRENT-CHAR) "$")
               (CHAR-EQ (CURRENT-CHAR) "%")
               (CHAR-EQ (CURRENT-CHAR) "("))
           (ACTION (SETQ G1 (COPY-TOKEN PRIOR-TOKEN)))
           (|PARSE-Qualification|) (ACTION (SETQ PRIOR-TOKEN G1)))))) 


(DEFUN |PARSE-Qualification| ()
  (AND (MATCH-ADVANCE-STRING "$") (MUST (|PARSE-Primary1|))
       (PUSH-REDUCTION '|PARSE-Qualification|
           (|dollarTran| (POP-STACK-1) (POP-STACK-1))))) 


(DEFUN |PARSE-SemiColon| ()
  (AND (MATCH-ADVANCE-STRING ";")
       (MUST (OR (|PARSE-Expr| 82)
                 (PUSH-REDUCTION '|PARSE-SemiColon| '|/throwAway|)))
       (PUSH-REDUCTION '|PARSE-SemiColon|
           (CONS '|;| (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 

;; We should factorize these boilerplates
(DEFUN |PARSE-Return| ()
  (AND (MATCH-ADVANCE-KEYWORD "return") (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Return|
           (CONS '|return| (CONS (POP-STACK-1) NIL))))) 

(DEFUN |PARSE-Throw| ()
  (AND (MATCH-ADVANCE-KEYWORD "throw")
       (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Throw|
           (CONS '|%Throw| (CONS (POP-STACK-1) NIL))))) 

(DEFUN |PARSE-Catch| ()
  (AND (MATCH-SPECIAL ";")
       (MATCH-KEYWORD-NEXT "catch")
       (ACTION (ADVANCE-TOKEN))
       (ACTION (ADVANCE-TOKEN))
       (MUST (|PARSE-GlyphTok| "("))
       (MUST (|PARSE-QuantifiedVariable|))
       (MUST (MATCH-ADVANCE-SPECIAL ")"))
       (MUST (|PARSE-GlyphTok| "=>"))
       (MUST (|PARSE-Expression|))
       (PUSH-REDUCTION '|PARSE-Catch|
	  (CONS (POP-STACK-2)
		(CONS (POP-STACK-1) NIL)))))

(DEFUN |PARSE-Finally| ()
  (AND (MATCH-SPECIAL ";")
       (MATCH-KEYWORD-NEXT "finally")
       (ACTION (ADVANCE-TOKEN))
       (ACTION (ADVANCE-TOKEN))
       (MUST (|PARSE-Expression|))))

(DEFUN |PARSE-Try| ()
  (AND (MATCH-ADVANCE-KEYWORD "try")
       (MUST (|PARSE-Expression|))
       ;; exception handlers: either a finally-expression, or
       ;; a series of catch-expressions optionally followed by
       ;; a finally-expression.
       (MUST (OR (AND (|PARSE-Finally|)
		      (PUSH-REDUCTION '|PARSE-Try|
                         (CONS '|%Try| 
			       (CONS (POP-STACK-2)
				     (CONS NIL
					   (CONS (POP-STACK-1) NIL))))))
		 (AND (MUST (STAR REPEATOR (|PARSE-Catch|)))
		      (BANG FIL_TEST
			    (OPTIONAL (|PARSE-Finally|)))
		      (PUSH-REDUCTION '|PARSE-Try|
                         (CONS '|%Try|
			       (CONS (POP-STACK-3)
				     (CONS (POP-STACK-2)
					   (CONS (POP-STACK-1) 
						 NIL))))))))))


(DEFUN |PARSE-Jump| ()
  (LET ((S (CURRENT-SYMBOL)))
       (AND S 
	    (ACTION (ADVANCE-TOKEN))
	    (PUSH-REDUCTION '|PARSE-Jump| S))))


(DEFUN |PARSE-Exit| ()
  (AND (MATCH-ADVANCE-KEYWORD "exit")
       (MUST (OR (|PARSE-Expression|)
                 (PUSH-REDUCTION '|PARSE-Exit| '|$NoValue|)))
       (PUSH-REDUCTION '|PARSE-Exit|
           (CONS '|exit| (CONS (POP-STACK-1) NIL)))))


(DEFUN |PARSE-Leave| ()
  (AND (MATCH-ADVANCE-KEYWORD "leave")
       (MUST (OR (|PARSE-Expression|)
                 (PUSH-REDUCTION '|PARSE-Leave| '|$NoValue|)))
       (MUST (PUSH-REDUCTION '|PARSE-Leave|
                 (CONS '|leave| (CONS (POP-STACK-1) NIL))))))


(DEFUN |PARSE-Seg| ()
  (AND (|PARSE-GlyphTok| "..")
       (BANG FIL_TEST (OPTIONAL (|PARSE-Expression|)))
       (PUSH-REDUCTION '|PARSE-Seg|
           (CONS 'SEGMENT
                 (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Conditional| ()
  (AND (MATCH-ADVANCE-KEYWORD "if") (MUST (|PARSE-Expression|))
       (MUST (MATCH-ADVANCE-KEYWORD "then")) (MUST (|PARSE-Expression|))
       (BANG FIL_TEST
             (OPTIONAL
                 (AND (MATCH-ADVANCE-KEYWORD "else")
                      (MUST (|PARSE-ElseClause|)))))
       (PUSH-REDUCTION '|PARSE-Conditional|
           (CONS '|if|
                 (CONS (POP-STACK-3)
                       (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))) 


(DEFUN |PARSE-ElseClause| ()
  (OR (AND (EQ (CURRENT-SYMBOL) '|if|) (|PARSE-Conditional|))
      (|PARSE-Expression|))) 


(DEFUN |PARSE-Loop| ()
  (OR (AND (STAR REPEATOR (|PARSE-Iterator|))
           (MUST (MATCH-ADVANCE-KEYWORD "repeat"))
           (MUST (|PARSE-Expr| 110))
           (PUSH-REDUCTION '|PARSE-Loop|
               (CONS 'REPEAT
                     (APPEND (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))
      (AND (MATCH-ADVANCE-KEYWORD "repeat") (MUST (|PARSE-Expr| 110))
           (PUSH-REDUCTION '|PARSE-Loop|
               (CONS 'REPEAT (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Iterator| ()
  (OR (AND (MATCH-ADVANCE-KEYWORD "for") (MUST (|PARSE-Primary|))
           (MUST (MATCH-ADVANCE-KEYWORD "in"))
           (MUST (|PARSE-Expression|))
           (MUST (OR (AND (MATCH-ADVANCE-KEYWORD "by")
                          (MUST (|PARSE-Expr| 200))
                          (PUSH-REDUCTION '|PARSE-Iterator|
                              (CONS 'INBY
                                    (CONS (POP-STACK-3)
                                     (CONS (POP-STACK-2)
                                      (CONS (POP-STACK-1) NIL))))))
                     (PUSH-REDUCTION '|PARSE-Iterator|
                         (CONS 'IN
                               (CONS (POP-STACK-2)
                                     (CONS (POP-STACK-1) NIL))))))
           (OPTIONAL
               (AND (MATCH-ADVANCE-STRING "|")
                    (MUST (|PARSE-Expr| 111))
                    (PUSH-REDUCTION '|PARSE-Iterator|
                        (CONS '|\|| (CONS (POP-STACK-1) NIL))))))
      (AND (MATCH-ADVANCE-KEYWORD "while") (MUST (|PARSE-Expr| 190))
           (PUSH-REDUCTION '|PARSE-Iterator|
               (CONS 'WHILE (CONS (POP-STACK-1) NIL))))
      (AND (MATCH-ADVANCE-KEYWORD "until") (MUST (|PARSE-Expr| 190))
           (PUSH-REDUCTION '|PARSE-Iterator|
               (CONS 'UNTIL (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Match| ()
  (AND (MATCH-ADVANCE-KEYWORD "case")
       (MUST (|PARSE-Expr| 400))
       (MATCH-ADVANCE-KEYWORD "is")
       (MUST (|PARSE-Expr| 110))
       (PUSH-REDUCTION '|PARSE-Match|
		       (CONS '|%Match|
			     (CONS (POP-STACK-2)
				   (CONS (POP-STACK-1) NIL))))))

(DEFUN |PARSE-Expr| (RBP)
  (DECLARE (SPECIAL RBP))
  (AND (|PARSE-NudPart| RBP)
       (OPTIONAL (STAR OPT_EXPR (|PARSE-LedPart| RBP)))
       (PUSH-REDUCTION '|PARSE-Expr| (POP-STACK-1)))) 


(DEFUN |PARSE-Label| ()
  (AND (MATCH-ADVANCE-STRING "<<") (MUST (|PARSE-Name|))
       (MUST (MATCH-ADVANCE-STRING ">>")))) 


(DEFUN |PARSE-LedPart| (RBP)
  (DECLARE (SPECIAL RBP))
  (AND (|PARSE-Operation| '|Led| RBP)
       (PUSH-REDUCTION '|PARSE-LedPart| (POP-STACK-1)))) 


(DEFUN |PARSE-NudPart| (RBP)
  (DECLARE (SPECIAL RBP))
  (AND (OR (|PARSE-Operation| '|Nud| RBP) (|PARSE-Reduction|)
           (|PARSE-Form|))
       (PUSH-REDUCTION '|PARSE-NudPart| (POP-STACK-1)))) 


(DEFUN |PARSE-Operation| (|ParseMode| RBP)
  (DECLARE (SPECIAL |ParseMode| RBP))
  (AND (NOT (MATCH-CURRENT-TOKEN 'IDENTIFIER))
       (GETL (SETQ |tmptok| (CURRENT-SYMBOL)) |ParseMode|)
       (LT RBP (|PARSE-leftBindingPowerOf| |tmptok| |ParseMode|))
       (ACTION (SETQ RBP
                     (|PARSE-rightBindingPowerOf| |tmptok| |ParseMode|)))
       (|PARSE-getSemanticForm| |tmptok| |ParseMode|
           (ELEMN (GETL |tmptok| |ParseMode|) 5 NIL)))) 


(DEFUN |PARSE-leftBindingPowerOf| (X IND)
  (DECLARE (SPECIAL X IND))
  (LET ((Y (GETL X IND))) (IF Y (ELEMN Y 3 0) 0))) 


(DEFUN |PARSE-rightBindingPowerOf| (X IND)
  (DECLARE (SPECIAL X IND))
  (LET ((Y (GETL X IND))) (IF Y (ELEMN Y 4 105) 105))) 


(DEFUN |PARSE-getSemanticForm| (X IND Y)
  (DECLARE (SPECIAL X IND Y))
  (OR (AND Y (EVAL Y)) (AND (EQ IND '|Nud|) (|PARSE-Prefix|))
      (AND (EQ IND '|Led|) (|PARSE-Infix|)))) 


(DEFUN |PARSE-Reduction| ()
  (AND (|PARSE-ReductionOp|) (MUST (|PARSE-Expr| 1000))
       (PUSH-REDUCTION '|PARSE-Reduction|
           (CONS '|%Reduce|
                 (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-ReductionOp| ()
  (AND (GETL (CURRENT-SYMBOL) '|Led|)
       (MATCH-NEXT-TOKEN 'GLIPH '/)
       (PUSH-REDUCTION '|PARSE-ReductionOp| (CURRENT-SYMBOL))
       (ACTION (ADVANCE-TOKEN)) (ACTION (ADVANCE-TOKEN)))) 


(DEFUN |PARSE-Form| ()
  (OR (AND (MATCH-ADVANCE-KEYWORD "iterate")
           (BANG FIL_TEST
                 (OPTIONAL
                     (AND (MATCH-ADVANCE-KEYWORD "from")
                          (MUST (|PARSE-Label|))
                          (PUSH-REDUCTION '|PARSE-Form|
                              (CONS (POP-STACK-1) NIL)))))
           (PUSH-REDUCTION '|PARSE-Form|
               (CONS '|iterate| (APPEND (POP-STACK-1) NIL))))
      (AND (MATCH-ADVANCE-KEYWORD "yield") (MUST (|PARSE-Application|))
           (PUSH-REDUCTION '|PARSE-Form|
               (CONS '|yield| (CONS (POP-STACK-1) NIL))))
      (|PARSE-Application|))) 


(DEFUN |PARSE-Application| ()
  (AND (|PARSE-Primary|) (OPTIONAL (STAR OPT_EXPR (|PARSE-Selector|)))
       (OPTIONAL
           (AND (|PARSE-Application|)
                (PUSH-REDUCTION '|PARSE-Application|
                    (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))) 


(DEFUN |PARSE-Selector| ()
  (OR (AND NONBLANK (EQ (CURRENT-SYMBOL) '|.|)
           (CHAR-NE (CURRENT-CHAR) '| |) (MATCH-ADVANCE-STRING ".")
           (MUST (|PARSE-PrimaryNoFloat|))
           (MUST (PUSH-REDUCTION '|PARSE-Selector|
                     (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL)))))
      (AND (OR (|PARSE-Float|)
               (AND (MATCH-ADVANCE-STRING ".")
                    (MUST (|PARSE-Primary|))))
           (MUST (PUSH-REDUCTION '|PARSE-Selector|
                     (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))) 


(DEFUN |PARSE-PrimaryNoFloat| ()
  (AND (|PARSE-Primary1|) (OPTIONAL (|PARSE-TokTail|)))) 


(DEFUN |PARSE-Primary| ()
  (OR (|PARSE-Float|) (|PARSE-PrimaryNoFloat|))) 


(DEFUN |PARSE-Primary1| ()
  (OR (AND (|PARSE-VarForm|)
           (OPTIONAL
               (AND NONBLANK (EQ (CURRENT-SYMBOL) '|(|)
                    (MUST (|PARSE-Primary1|))
                    (PUSH-REDUCTION '|PARSE-Primary1|
                        (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))
      (|PARSE-Quad|) (|PARSE-String|) (|PARSE-IntegerTok|)
      (|PARSE-FormalParameter|)
      (AND (MATCH-ADVANCE-STRING "'")
           (MUST (AND (MUST (|PARSE-Data|))
                      (PUSH-REDUCTION '|PARSE-Primary1| (POP-STACK-1)))))
      (|PARSE-Sequence|) (|PARSE-Enclosure|))) 


(DEFUN |PARSE-Float| ()
  (AND (|PARSE-FloatBase|)
       (MUST (OR (AND NONBLANK (|PARSE-FloatExponent|))
                 (PUSH-REDUCTION '|PARSE-Float| 0)))
       (PUSH-REDUCTION '|PARSE-Float|
           (MAKE-FLOAT (POP-STACK-4) (POP-STACK-2) (POP-STACK-2)
               (POP-STACK-1))))) 


(DEFUN |PARSE-FloatBase| ()
  (OR (AND (INTEGERP (CURRENT-SYMBOL)) (CHAR-EQ (CURRENT-CHAR) ".")
           (CHAR-NE (NEXT-CHAR) ".") (|PARSE-IntegerTok|)
           (MUST (|PARSE-FloatBasePart|)))
      (AND (INTEGERP (CURRENT-SYMBOL))
           (CHAR-EQ (CHAR-UPCASE (CURRENT-CHAR)) 'E)
           (|PARSE-IntegerTok|) (PUSH-REDUCTION '|PARSE-FloatBase| 0)
           (PUSH-REDUCTION '|PARSE-FloatBase| 0))
      (AND (DIGITP (CURRENT-CHAR)) (EQ (CURRENT-SYMBOL) '|.|)
           (PUSH-REDUCTION '|PARSE-FloatBase| 0)
           (|PARSE-FloatBasePart|)))) 


(DEFUN |PARSE-FloatBasePart| ()
  (AND (MATCH-ADVANCE-STRING ".")
       (MUST (OR (AND (DIGITP (CURRENT-CHAR))
                      (PUSH-REDUCTION '|PARSE-FloatBasePart|
                          (TOKEN-NONBLANK (CURRENT-TOKEN)))
                      (|PARSE-IntegerTok|))
                 (AND (PUSH-REDUCTION '|PARSE-FloatBasePart| 0)
                      (PUSH-REDUCTION '|PARSE-FloatBasePart| 0)))))) 


(DEFUN |PARSE-FloatExponent| ()
  (PROG (G1)
    (RETURN
      (OR (AND (MEMBER (CURRENT-SYMBOL) '(E |e|))
               (FIND (CURRENT-CHAR) "+-") (ACTION (ADVANCE-TOKEN))
               (MUST (OR (|PARSE-IntegerTok|)
                         (AND (MATCH-ADVANCE-STRING "+")
                              (MUST (|PARSE-IntegerTok|)))
                         (AND (MATCH-ADVANCE-STRING "-")
                              (MUST (|PARSE-IntegerTok|))
                              (PUSH-REDUCTION '|PARSE-FloatExponent|
                                  (MINUS (POP-STACK-1))))
                         (PUSH-REDUCTION '|PARSE-FloatExponent| 0))))
          (AND (IDENTP (CURRENT-SYMBOL))
               (SETQ G1 (FLOATEXPID (CURRENT-SYMBOL)))
               (ACTION (ADVANCE-TOKEN))
               (PUSH-REDUCTION '|PARSE-FloatExponent| G1)))))) 


(DEFUN |PARSE-Enclosure| ()
  (OR (AND (MATCH-ADVANCE-STRING "(")
           (MUST (OR (AND (|PARSE-Expr| 6)
                          (MUST (MATCH-ADVANCE-STRING ")")))
                     (AND (MATCH-ADVANCE-STRING ")")
                          (PUSH-REDUCTION '|PARSE-Enclosure|
                              (CONS '|%Comma| NIL))))))
      (AND (MATCH-ADVANCE-STRING "{")
           (MUST (OR (AND (|PARSE-Expr| 6)
                          (MUST (MATCH-ADVANCE-STRING "}"))
                          (PUSH-REDUCTION '|PARSE-Enclosure|
                              (CONS '|brace|
                                    (CONS
                                     (CONS '|construct|
                                      (CONS (POP-STACK-1) NIL))
                                     NIL))))
                     (AND (MATCH-ADVANCE-STRING "}")
                          (PUSH-REDUCTION '|PARSE-Enclosure|
                              (CONS '|brace| NIL))))))
      (AND (MATCH-ADVANCE-STRING "[|")
	   (MUST (AND (|PARSE-Statement|)
		      (MUST (MATCH-ADVANCE-STRING "|]"))
		      (PUSH-REDUCTION '|PARSE-Enclosure|
				      (CONS '|[\|\|]|
					    (CONS (POP-STACK-1) NIL)))
		      )))
      )) 


(DEFUN |PARSE-IntegerTok| () (PARSE-NUMBER)) 


(DEFUN |PARSE-FloatTok| ()
  (AND (PARSE-NUMBER)
       (PUSH-REDUCTION '|PARSE-FloatTok| (POP-STACK-1)))) 


(DEFUN |PARSE-FormalParameter| () (|PARSE-FormalParameterTok|)) 


(DEFUN |PARSE-FormalParameterTok| () (PARSE-ARGUMENT-DESIGNATOR)) 


(DEFUN |PARSE-Quad| ()
  (AND (MATCH-ADVANCE-STRING "$")
       (PUSH-REDUCTION '|PARSE-Quad| '$)))


(DEFUN |PARSE-String| () (PARSE-SPADSTRING)) 


(DEFUN |PARSE-VarForm| ()
  (AND (|PARSE-Name|)
       (OPTIONAL
           (AND (|PARSE-Scripts|)
                (PUSH-REDUCTION '|PARSE-VarForm|
                    (CONS '|Scripts|
                          (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))))
       (PUSH-REDUCTION '|PARSE-VarForm| (POP-STACK-1)))) 


(DEFUN |PARSE-Scripts| ()
  (AND NONBLANK (MATCH-ADVANCE-STRING "[") (MUST (|PARSE-ScriptItem|))
       (MUST (MATCH-ADVANCE-STRING "]")))) 


(DEFUN |PARSE-ScriptItem| ()
  (OR (AND (|PARSE-Expr| 90)
           (OPTIONAL
               (AND (STAR REPEATOR
                          (AND (MATCH-ADVANCE-STRING ";")
                               (MUST (|PARSE-ScriptItem|))))
                    (PUSH-REDUCTION '|PARSE-ScriptItem|
                        (CONS '|;|
                              (CONS (POP-STACK-2)
                                    (APPEND (POP-STACK-1) NIL)))))))
      (AND (MATCH-ADVANCE-STRING ";") (MUST (|PARSE-ScriptItem|))
           (PUSH-REDUCTION '|PARSE-ScriptItem|
               (CONS '|PrefixSC| (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Name| ()
  (AND (PARSE-IDENTIFIER) (PUSH-REDUCTION '|PARSE-Name| (POP-STACK-1)))) 


(DEFUN |PARSE-Data| ()
  (AND (ACTION (SETQ LABLASOC NIL)) (|PARSE-Sexpr|)
       (PUSH-REDUCTION '|PARSE-Data|
           (CONS 'QUOTE (CONS (TRANSLABEL (POP-STACK-1) LABLASOC) NIL))))) 


(DEFUN |PARSE-Sexpr| ()
  (AND (ACTION (ADVANCE-TOKEN)) (|PARSE-Sexpr1|))) 


(DEFUN |PARSE-Sexpr1| ()
  (OR (|PARSE-IntegerTok|)
      (|PARSE-String|)
      (AND (|PARSE-AnyId|)
           (OPTIONAL
               (AND (|PARSE-NBGliphTok| '=) (MUST (|PARSE-Sexpr1|))
                    (ACTION (SETQ LABLASOC
                                  (CONS (CONS (POP-STACK-2)
                                         (NTH-STACK 1))
                                        LABLASOC))))))
      (AND (MATCH-ADVANCE-STRING "'") (MUST (|PARSE-Sexpr1|))
           (PUSH-REDUCTION '|PARSE-Sexpr1|
               (CONS 'QUOTE (CONS (POP-STACK-1) NIL))))
      ;; next form disabled -- gdr, 2009-06-15.
;      (AND (MATCH-ADVANCE-STRING "-") (MUST (|PARSE-IntegerTok|))
;           (PUSH-REDUCTION '|PARSE-Sexpr1| (MINUS (POP-STACK-1))))
      (AND (MATCH-ADVANCE-STRING "[")
           (BANG FIL_TEST (OPTIONAL (STAR REPEATOR (|PARSE-Sexpr1|))))
           (MUST (MATCH-ADVANCE-STRING "]"))
           (PUSH-REDUCTION '|PARSE-Sexpr1| (LIST2VEC (POP-STACK-1))))
      (AND (MATCH-ADVANCE-STRING "(")
           (BANG FIL_TEST
                 (OPTIONAL
                     (AND (STAR REPEATOR (|PARSE-Sexpr1|))
                          (OPTIONAL
                              (AND (|PARSE-GlyphTok| ".")
                                   (MUST (|PARSE-Sexpr1|))
                                   (PUSH-REDUCTION '|PARSE-Sexpr1|
                                    (|append!| (POP-STACK-2) (POP-STACK-1))))))))
           (MUST (MATCH-ADVANCE-STRING ")"))))) 


(DEFUN |PARSE-NBGliphTok| (|tok|)
  (DECLARE (SPECIAL |tok|))
  (AND (MATCH-CURRENT-TOKEN 'GLIPH |tok|) NONBLANK
       (ACTION (ADVANCE-TOKEN)))) 


(DEFUN |PARSE-GlyphTok| (|tok|)
  (DECLARE (SPECIAL |tok|))
  (AND (MATCH-CURRENT-TOKEN 'GLIPH (INTERN |tok|))
       (ACTION (ADVANCE-TOKEN)))) 


(DEFUN |PARSE-AnyId| ()
  (OR (PARSE-IDENTIFIER)
      (OR (AND (MATCH-STRING "$")
               (PUSH-REDUCTION '|PARSE-AnyId| (CURRENT-SYMBOL))
               (ACTION (ADVANCE-TOKEN)))
          (PARSE-KEYWORD)
	  (|PARSE-OperatorFunctionName|))))


(DEFUN |PARSE-Sequence| ()
  (OR (AND (|PARSE-OpenBracket|) (MUST (|PARSE-Sequence1|))
           (MUST (MATCH-ADVANCE-STRING "]")))
      (AND (|PARSE-OpenBrace|) (MUST (|PARSE-Sequence1|))
           (MUST (MATCH-ADVANCE-STRING "}"))
           (PUSH-REDUCTION '|PARSE-Sequence|
               (CONS '|brace| (CONS (POP-STACK-1) NIL)))))) 


(DEFUN |PARSE-Sequence1| ()
  (AND (OR (AND (|PARSE-Expression|)
                (PUSH-REDUCTION '|PARSE-Sequence1|
                    (CONS (POP-STACK-2) (CONS (POP-STACK-1) NIL))))
           (PUSH-REDUCTION '|PARSE-Sequence1| (CONS (POP-STACK-1) NIL)))
       (OPTIONAL
           (AND (|PARSE-IteratorTail|)
                (PUSH-REDUCTION '|PARSE-Sequence1|
                    (CONS 'COLLECT
                          (APPEND (POP-STACK-1)
                                  (CONS (POP-STACK-1) NIL)))))))) 


(DEFUN |PARSE-OpenBracket| ()
  (LET ((G1 (CURRENT-SYMBOL)))
    (AND (EQ (|getToken| G1) '[)
	 (MUST (OR (AND (EQCAR G1 '|elt|)
			(PUSH-REDUCTION '|PARSE-OpenBracket|
			    (CONS '|elt|
				  (CONS (CADR G1)
				   (CONS '|construct| NIL)))))
		   (PUSH-REDUCTION '|PARSE-OpenBracket| '|construct|)))
	 (ACTION (ADVANCE-TOKEN)))))


(DEFUN |PARSE-OpenBrace| ()
  (LET ((G1 (CURRENT-SYMBOL)))
    (AND (EQ (|getToken| G1) '{)
	 (MUST (OR (AND (EQCAR G1 '|elt|)
			(PUSH-REDUCTION '|PARSE-OpenBrace|
			    (CONS '|elt|
				  (CONS (CADR G1)
				   (CONS '|brace| NIL)))))
		   (PUSH-REDUCTION '|PARSE-OpenBrace| '|construct|)))
	 (ACTION (ADVANCE-TOKEN)))))


(DEFUN |PARSE-IteratorTail| ()
  (OR (AND (MATCH-ADVANCE-KEYWORD "repeat")
           (BANG FIL_TEST
                 (OPTIONAL (STAR REPEATOR (|PARSE-Iterator|)))))
      (STAR REPEATOR (|PARSE-Iterator|)))) 

