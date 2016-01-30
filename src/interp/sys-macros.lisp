;; Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
;; All rights reserved.
;; Copyright (C) 2007-2014, Gabriel Dos Reis.
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
;; 


(IMPORT-MODULE "union")
(IMPORT-MODULE "sys-globals") 
(IMPORT-MODULE "diagnostics") 
(in-package "BOOT")

;; 
;; -*- Charcters and Strings -*-
;; 

;; Special character constants
(defconstant |$Bell| (code-char 7))
(defconstant |$Backspace| #\Backspace)
(defconstant |$HorizontalTab| #\Tab)
(defconstant |$Newline| #\Newline)
(defconstant |$VerticalTab| (code-char 11))
(defconstant |$FormFeed| #\Page)
(defconstant |$CarriageReturn| #\Return)


(defmacro |startsId?| (x)
 `(or (alpha-char-p ,x)
      (member ,x '(#\? #\% #\!) :test #'char=)))


;; 
;; -*- Byte -*-
;; 

(defmacro |byteEqual| (|x| |y|)
  `(EQL (the |%Byte| ,|x|) (the |%Byte| ,|y|)))

(defmacro |byteLessThan| (|x| |y|)
  `(< (the |%Byte| ,|x|) (the |%Byte| ,|y|)))

(defmacro |byteGreaterEqual| (|x| |y|)
  `(>= (the |%Byte| ,|x|) (the |%Byte| ,|y|)))

(defmacro |byteGreaterThan| (|x| |y|)
  `(> (the |%Byte| ,|x|) (the |%Byte| ,|y|)))

;; 
;; -*- BigFloat Constructors -*-
;; 

(defmacro |float| (x &optional (y 0.0d0))
  `(float ,x ,y))

(defun |makeSF| (mantissa exponent)
  (|float| (/ mantissa (expt 2 (- exponent)))))

(defmacro MAKE-BF (MT EP)
  `(CONS |$BFtag| (CONS ,MT ,EP)))

(defun MAKE-FLOAT (int frac fraclen exp)
  (if |$useBFasDefault|
      (if (= frac 0)
          (MAKE-BF int exp)
	(MAKE-BF (+ (* int (expt 10 fraclen)) frac) 
		 (- exp fraclen)) )
    (read-from-string
     (format nil "~D.~v,'0De~D" int fraclen frac exp))) )


;; 
;; -*- Equality Predicates -*-
;; 

;; QEQCAR should be used when you know the first arg is a pair
;; the second arg should either be a literal fixnum or a symbol
;; the car of the first arg is always of the same type as the second
;; use eql unless we are sure fixnums are represented canonically
(defmacro QEQCAR (x y)
  (if (integerp y) 
      `(eql (the fixnum (QCAR ,x)) (the fixnum ,y))
    `(eq (QCAR ,x) ,y)))

(eval-when
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun IDENT-CHAR-LIT (x)
   (and (EQCAR x 'quote) 
	(|ident?| (cadr x))
	(= (length (PNAME (cadr x))) 1)))) 

(defmacro BOOT-EQUAL (a b)
  (cond ((IDENT-CHAR-LIT a)
	 `(or (eql ,a ,b) (eql (character ,a) ,b)))
	((IDENT-CHAR-LIT b)
	 `(or (eql ,a ,b) (eql ,a (character ,b))))
	(t `(EQQUAL ,a ,b))))


(defmacro EQQUAL (a b)
  (cond ((OR (EQUABLE a) (EQUABLE b))
	 `(eq ,a ,b))
	((OR (numberp a) (numberp b))
	 `(eql ,a ,b))
	(t  `(equal ,a ,b))))

(defmacro NEQUAL (a b)
  `(not (BOOT-EQUAL ,a ,b)))
 
(defmacro IEQUAL (&rest L) 
  `(eql . ,L))

(defmacro GE (&rest L)
  `(>= . ,L))

(defmacro GT (&rest L)
  `(> . ,L))

(defmacro LE (&rest L) 
  `(<= . ,L))

(defmacro LT (&rest L)
  `(< . ,L))

;; 
;; -*- Cons Cell Accessors -*-
;; 
(defmacro KAR (ARG)
  `(IFCAR ,arg))

(defmacro KDR (ARG)
  `(IFCDR ,arg))

(defmacro KADR (ARG)
  `(IFCAR (IFCDR ,arg)))

(defmacro KADDR (ARG)
  `(IFCAR (IFCDR (IFCDR ,arg))))


(eval-when 
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (defun FIRST-ERROR ()
   (error "Cannot take first of an empty list")))

(defmacro |SPADfirst| (l)
  (let ((tem (gensym)))
    `(let ((,tem ,l)) 
       (if ,tem 
	   (car ,tem) 
	 (FIRST-ERROR)))))

(defmacro ELEM (val &rest indices)
  (if (null indices) 
      val 
    `(ELEM (nth (1- ,(car indices)) ,val) ,@(cdr indices))))

(eval-when
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (progn
   (defun NLIST (N FN)
     "Returns a list of N items, each initialized to the value of an
 invocation of FN"
     (if (LT N 1)
	 NIL 
       (CONS (EVAL FN) (NLIST (1- N) FN))))
    
   ))

;; 
;; -*- Simple Arrays -*-
;; 

;; Note: these macros should probably be just ordinary functions.

(defmacro |makeSimpleArray| (|t| |n|)
  `(make-array ,|n| :element-type ,|t|))

(defmacro |makeFilledSimpleArray| (|t| |n| |v|)
  `(make-array ,|n| :element-type ,|t| :initial-element ,|v|))

(defmacro |makeSimpleArrayFromList| (|t| |l|)
  `(make-array (list-length ,|l|) :element-type ,|t| :initial-contents ,|l|))

(defmacro |getSimpleArrayEntry| (|a| |i|)
  `(aref (the simple-array ,|a|) (the fixnum ,|i|)))

(defmacro |setSimpleArrayEntry| (|a| |i| |v|)
  `(setf (aref (the simple-array ,|a|) (the fixnum ,|i|)) ,|v|))

(defmacro |sizeOfSimpleArray| (|a|)
  `(the fixnum (length (the simple-array ,|a|))))

(defmacro |maxIndexOfSimpleArray| (|a|)
  `(the fixnum (1- (the fixnum (length (the simple-array ,|a|))))))

;; 
;; -*- Functions -*-
;; 
 
(defmacro |function| (name)
  `(FUNCTION ,name))

(defmacro SPADCALL (&rest L)
  (let ((args (butlast l))
	(fn (car (|lastNode| l)))
	(gi (gensym)))
    ;; (values t) indicates a single return value
    `(let ((,gi ,fn)) 
       (the (values t) (funcall (car ,gi) ,@args (cdr ,gi))))))

;; 
;; -*- Arithmetics -*-
;; 
 
(defmacro SPADDIFFERENCE (&rest x)
  `(- . ,x))

;; 
;; -*- Pattern Matching -*-
;; 
 
(defmacro IS (x y) `(DCQ ,y ,x))

;; 
;; -*- Evaluation Strategies -*-
;; 

(eval-when
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (DEFUN MKQ (X)
   "Evaluates an object and returns it with QUOTE wrapped around it."
   (if (NUMBERP X) 
       X 
     (LIST 'QUOTE X))))

;; 
;; -*- Assignments -*-
;; 
 
(defmacro LETT (var val &rest L)
  (COND
   (|$QuickLet| `(SETQ ,var ,val))
   (|$compilingMap|
    ;; map tracing
    `(PROGN
      (SETQ ,var ,val)
      (COND (|$letAssoc|
	     (|mapLetPrint| ,(MKQ var)
                            ,var
                            (QUOTE ,(KAR L))))
	    ('T ,var))))
   ;; used for LETs in SPAD code --- see devious trick in COMP-TRAN-1
   ((ATOM var)
    `(PROGN
      (SETQ ,var ,val)
      (IF |$letAssoc|
	  ,(cond ((null (cdr l))
		  `(|letPrint| ,(MKQ var) ,var (QUOTE ,(KAR L))))
		 ((and (eqcar (car l) 'SPADCALL)
		       (= (length (car l)) 3))
		  `(|letPrint3| ,(MKQ var) ,var ,(third (car l))
		                (QUOTE ,(KADR L))))
		 (t `(|letPrint2| ,(MKQ var) ,(car l) (QUOTE ,(KADR L))))))
      ,var))
   ('T (ERROR "Cannot compileLET construct"))))

 
(defmacro SPADLET (A B)
  (if (ATOM A)
      `(SETQ ,A ,B)
    `(OR (IS ,B ,A) 
	 (LET_ERROR ,(MK_LEFORM A) ,(MKQ B) ))))


;; 
;; -*- Helper Functions For Iteration Control Structures -*-
;; 

(eval-when
 #+:common-lisp (:compile-toplevel :load-toplevel :execute)
 #-:common-lisp (compile load eval)
 (progn
   (DEFUN S+ (X Y)
     (COND ((ATOM Y) 
	    X)
	   ((ATOM X)
	    Y)
	   ((MEMBER (CAR X) Y :test #'equal)
	    (S+ (CDR X) Y))
	   ((S+ (CDR X) (CONS (CAR X) Y)))))
 
   (defun S* (l1 l2) 
     (INTERSECTION l1 l2 :test #'equal))
   
   (defun S- (l1 l2)
     (set-difference l1 l2 :test #'equal))

   (defun MKPFFLATTEN (X OP)
     (COND ((ATOM X) X)
	   ((EQL (CAR X) OP) 
	    (CONS OP (MKPFFLATTEN-1 (CDR X) OP NIL)))
	   ((CONS (MKPFFLATTEN (CAR X) OP) (MKPFFLATTEN (CDR X) OP)))))
   
   (defun MKPFFLATTEN-1 (L OP R)
     (let (X)
       (if (NULL L)
	   R
	 (MKPFFLATTEN-1 (CDR L) 
			OP
			(APPEND R 
				(if (EQCAR (SETQ X (MKPFFLATTEN (CAR L) OP))
					   OP)
				    (CDR X) (LIST X)))))))
   
   (defun MKPF1 (L OP)
     (let (X) 
       (case OP 
	     (PLUS 
	      (COND ((EQL 0 (SETQ X (LENGTH (SETQ L (S- L '(0 (ZERO)))))))
		     0)
		    ((EQL 1 X) 
		     (CAR L))
		    ((CONS 'PLUS L)) ))
	     (TIMES 
	      (COND ((S* L '(0 (ZERO)))
		     0)
		    ((EQL 0 (SETQ X (LENGTH (SETQ L (S- L '(1 (ONE))))))) 
		     1)
		    ((EQL 1 X) 
		     (CAR L))
		    ((CONS 'TIMES L)) ))
	     (QUOTIENT
	      (COND ((GREATERP (LENGTH L) 2)
		     (fail))
		    ((EQL 0 (CAR L)) 
		     0)
		    ((EQL (CADR L) 1)
		     (CAR L))
		    ((CONS 'QUOTIENT L)) ))
	     (MINUS 
	      (COND ((CDR L) 
		     (FAIL))
		    ((NUMBERP (SETQ X (CAR L)))
		     (MINUS X))
		    ((EQCAR X 'MINUS)
		     (CADR X))
		    ((CONS 'MINUS L))  ))
	     (DIFFERENCE 
	      (COND ((GREATERP (LENGTH L) 2)
		     (FAIL))
		    ((EQUAL (CAR L) (CADR L))
		     '(ZERO))
		    ((|member| (CAR L) '(0 (ZERO)))
		     (|mkpf| (CDR L) 'MINUS))
		    ((|member| (CADR L) '(0 (ZERO)))
		     (CAR L))
		    ((EQCAR (CADR L) 'MINUS)
		     (|mkpf| (LIST (CAR L) (CADADR L)) 'PLUS))
		    ((CONS 'DIFFERENCE L)) ))
	     (EXPT 
	      (COND ((GREATERP (LENGTH L) 2)
		     (FAIL))
		    ((EQL 0 (CADR L))
		     1)
		    ((EQL 1 (CADR L))
		     (CAR L))
		    ((|member| (CAR L) '(0 1 (ZERO) (ONE)))
		     (CAR L))
		    ((CONS 'EXPT L)) ))
	     (OR 
	      (COND ((MEMBER T L) 
		     T)
		    ((EQL 0 (SETQ X (LENGTH (SETQ L (REMOVE NIL L)))))
		     NIL)
		    ((EQL 1 X)
		     (CAR L))
		    ((CONS 'OR L)) ))
	     (|or| 
	      (COND ((MEMBER T L) T)
		    ((EQL 0 (SETQ X (LENGTH (SETQ L (REMOVE NIL L)))))
		     NIL)
		    ((EQL 1 X)
		     (CAR L))
		    ((CONS '|or| L)) ))
	     (NULL
	      (COND ((CDR L)
		     (FAIL))
		    ((EQCAR (CAR L) 'NULL)
		     (CADAR L))
		    ((EQL (CAR L) 'T)
		     NIL)
		    ((NULL (CAR L))
		     ''T)
		    ((CONS 'NULL L)) ))
	     (|and|
	      (COND ((EQL 0 (SETQ X 
				  (LENGTH 
				   (SETQ L (REMOVE T L)))))
		     T)
		    ((EQL 1 X)
		     (CAR L))
		    ((CONS '|and| L)) ))
	     (AND 
	      (COND ((EQL 0 (SETQ X (LENGTH
				     (SETQ L (REMOVE T L))))) 
		     T)
		    ((EQL 1 X)
		     (CAR L))
		    ((CONS 'AND L)) ))
	     (PROGN
	      (COND ((AND (NOT (ATOM L)) 
			  (NULL (LAST L)))
		     (if (CDR L) `(PROGN . ,L) (CAR L)))
		    ((NULL (SETQ L (REMOVE NIL L)))
		     NIL)
		    ((CDR L) 
		     (CONS 'PROGN L))
		    ((CAR L))))
	     (SEQ 
	      (COND ((EQCAR (CAR L) 'EXIT)
		     (CADAR L))
		    ((CDR L) 
		     (CONS 'SEQ L))
		    ((CAR L))))
	     (LIST
	      (if L 
		  (cons 'LIST L)))
	     (CONS 
	      (if (cdr L) (cons 'CONS L) (car L)))
	     (t (CONS OP L) ))))

 
   (defun FLAG (L KEY)
     "Set the KEY property of every item in list L to T."
     (mapc #'(lambda (item) (makeprop item KEY T)) L))
   
   (defun FLAGP (X KEY)
     "If X has a KEY property, then FLAGP is true."
     (GET X KEY))

   (DEFUN REMFLAG (L KEY)
     "Set the KEY property of every item in list L to NIL."
     (OR (ATOM L) 
	 (SEQ 
	  (REMPROP (CAR L) KEY) 
	  (REMFLAG (CDR L) KEY))))
   

   (FLAG '(* + AND |and| OR |or| PROGN) 'NARY) ; flag for |mkpf|


   (defun |mkpf| (L OP)
     (if (FLAGP OP 'NARY)
	 (SETQ L (MKPFFLATTEN-1 L OP NIL)))
     (MKPF1 L OP))

   (defun MKQSADD1 (X)
     (COND ((ATOM X)
	    `(1+ ,X))
	   ((AND (member (CAR X) '(-DIFFERENCE |%isub| -) :test #'eq)
		 (EQL 1 (CADDR X)))
	    (CADR X))
	   (`(1+ ,X))))


   (defun SEQOPT (U)
     (if (AND (EQCAR U 'SEQ)
	      (EQCAR (CADR U) 'EXIT)
	      (EQCAR (CADADR U) 'SEQ))
	 (CADADR U)
       U))

   (defun -REPEAT (BD SPL)
     (let (u g g1 inc final xcl xv il rsl tll funPLUS funGT fun? funIdent
	     funPLUSform funGTform)
       (DO ((X SPL (CDR X)))
	   ((ATOM X)
	    (LIST 'spadDO 
		  (|reverse!| IL) 
		  (LIST (|mkpf| (|reverse!| XCL) 'OR) XV)
		  (SEQOPT (CONS 'SEQ (|append!| (|reverse!| RSL)
					    (LIST (LIST 'EXIT BD)))))))
	   (COND ((ATOM (CAR X)) 
		  (FAIL)))
	   (COND ((AND (EQ (CAAR X) 'STEP)
		       (|member| (CADDAR X) '(2 1 0 (|One|) (|Zero|)))
		       (|member| (CADR (CDDAR X)) '(1 (|One|))))
		  (SETQ X (CONS (CONS 'ISTEP (CDAR X)) (CDR X))) ))
	   ;; A hack to increase the likelihood of small integers
	   (SETQ U (CDAR X))
	   (case (CAAR X)
		 (GENERAL 
		  (AND (CDDDR U) (PUSH (CADDDR U) XCL))
		  (PUSH (LIST (CAR U) (CADR U) (CADDR U)) IL) )
		 (GSTEP
		  (SETQ tll (CDDDDR U))  ;tll is (+fun >fun type? ident)
		  (SETQ funPLUSform (CAR tll))
		  (SETQ funGTform   (CAR (SETQ tll (QCDR tll))))
		  (PUSH (LIST (SETQ funPLUS (GENSYM)) funPLUSform) IL)
		  (PUSH (LIST (SETQ funGT   (GENSYM)) funGTform) IL)
		  (COND ((SETQ tll (CDR tll)) 
			 (SETQ fun?     (CAR tll))
			 (SETQ funIdent (CAR (SETQ tll (QCDR tll))))))
		  (IF (NOT (ATOM (SETQ inc (CADDR U)) ))
		      (PUSH (LIST (SETQ inc (GENSYM)) (CADDR U)) IL))
		  (SETQ final (CADDDR U))
		  (COND (final
			 (COND ((ATOM final))
			       ((PUSH (LIST (SETQ final (GENSYM)) (CADDDR U))
				      IL)))
			 ;; If CADDDR U is not an atom, only compute the value once
			 (PUSH
			  (if fun? 
			      (if (FUNCALL fun? INC)
				  (if  (FUNCALL (EVAL funGTform) INC funIdent) 
				      (LIST 'FUNCALL funGT (CAR U) FINAL)
				    (LIST 'FUNCALL funGT FINAL (CAR U)))
				(LIST 'IF (LIST 'FUNCALL funGT INC funIdent)
				      (LIST 'FUNCALL funGT (CAR U) FINAL)
				      (LIST 'FUNCALL funGT FINAL  (CAR U))))
			    (LIST 'FUNCALL funGT (CAR U) final))
			  XCL)))
		  (PUSH (LIST (CAR U) 
			      (CADR U) 
			      (LIST 'FUNCALL funPLUS (CAR U) INC)) 
			IL))
		 (STEP
		  (IF (NOT (ATOM (SETQ inc (CADDR U)) ))
		      (PUSH (LIST (SETQ inc (GENSYM)) (CADDR U)) IL))
		  (COND ((CDDDR U)
			 (COND ((ATOM (SETQ final (CADDDR U)) ))
			       ((PUSH (LIST (SETQ final (GENSYM)) (CADDDR U)) 
				      IL)))
			 ;; If CADDDR U is not an atom, only compute the value once
			 (PUSH
			  (if (INTEGERP INC)
			      (LIST (if  (MINUSP INC) '< '>) (CAR U) FINAL)
			    `(if (MINUSP ,INC)
				 (< ,(CAR U) ,FINAL)
			       (> ,(CAR U) ,FINAL)))
			  XCL)))
		  (PUSH (LIST (CAR U) (CADR U) (LIST '+ (CAR U) INC)) IL))
		 (ISTEP
		  (IF (NOT (ATOM (SETQ inc (CADDR U)) ))
		      (PUSH (LIST (SETQ inc (GENSYM)) (CADDR U)) IL))
		  (COND ((CDDDR U)
			 (COND ((ATOM (SETQ final (CADDDR U)) ))
			       ((PUSH (LIST (SETQ final (GENSYM)) (CADDDR U))
				      IL)))
			 ;; If CADDDR U is not an atom, only compute the value once
			 (PUSH
			  (if (INTEGERP INC)
			      (LIST (if  (MINUSP INC)
					'<
				      '>)
				    (CAR U)
				    FINAL)
			    `(if (MINUSP ,INC)
				 (< ,(CAR U) ,FINAL)
			       (> ,(CAR U) ,FINAL)))
			  XCL)))
		  (PUSH (LIST (CAR U) (CADR U)
			      (COND ((|member| INC '(1 (|One|)))
				     (MKQSADD1 (CAR U)))
				    ((LIST '+ (CAR U) INC)) ))
			IL))
		 (ON 
		  (PUSH (LIST 'ATOM (CAR U)) XCL)
		  (PUSH (LIST (CAR U) (CADR U) (LIST 'CDR (CAR U))) IL))
		 (RESET
		  (PUSH (LIST 'PROGN (CAR U) NIL) XCL))
		 (IN
		  (PUSH (LIST 'OR
			      (LIST 'ATOM (SETQ G (GENSYM)))
			      (CONS 'PROGN
				    (CONS
				     (LIST 'SETQ (CAR U) (LIST 'CAR G))
				     (APPEND
				      (COND ((AND (symbol-package (car U)) 
						  $TRACELETFLAG)
					     (LIST (LIST '/TRACELET-PRINT 
							 (CAR U)
							 (CAR U))))
					    (NIL))
				      (LIST NIL))))  ) XCL)
		  (PUSH (LIST G (CADR U) (LIST 'CDR G)) IL)
		  (PUSH (LIST (CAR U) NIL) IL))
		 (INDOM
		  (SETQ G (GENSYM))
		  (SETQ G1 (GENSYM))
		  (PUSH (LIST 'ATOM G) XCL)
		  (PUSH (LIST G (LIST 'INDOM-FIRST (CADR U))
			      (LIST 'INDOM-NEXT G1)) IL)
		  (PUSH (LIST (CAR U) NIL) IL)
		  (PUSH (LIST G1 NIL) IL)
		  (PUSH (LIST 'SETQ G1 (LIST 'CDR G)) RSL)
		  (PUSH (LIST 'SETQ (CAR U) (LIST 'CAR G)) RSL))
		 (UNTIL
		  (SETQ G (GENSYM))
		  (PUSH (LIST G NIL (CAR U)) IL)
		  (PUSH G XCL))
		 (WHILE
		  (PUSH (LIST 'NULL (CAR U)) XCL))
		 (SUCHTHAT
		  (SETQ BD (LIST 'SUCHTHATCLAUSE BD (CAR U))))
		 (EXIT 
		  (SETQ XV (CAR U))) (FAIL)))))

   (defun REPEAT-TRAN (L LP)
     (COND ((ATOM L) 
	    (ERROR "REPEAT FORMAT ERROR"))
	   ((MEMBER (KAR (KAR L))
		    '(EXIT RESET IN ON GSTEP ISTEP STEP 
			   GENERAL UNTIL WHILE SUCHTHAT EXIT))
	    (REPEAT-TRAN (CDR L) (CONS (CAR L) LP)))
	   ((CONS (|reverse!| LP) (|mkpf| L 'PROGN)))))
   
   (defun MK_LEFORM (U)
     (COND ((|ident?| U) 
	    (PNAME U))
	   ((STRINGP U)
	    U)
	   ((ATOM U)
	    (STRINGIMAGE U))
	   ((MEMBER (FIRST U) '(VCONS CONS) :test #'eq)
	    (STRCONC "(" (MK_LEFORM-CONS U) ")") )
	   ((EQ (FIRST U) 'LIST) 
	    (STRCONC "(" (MK_LEFORM (SECOND U)) ")") )
	   ((EQ (FIRST U) 'APPEND)
	    (STRCONC "(" (MK_LEFORM-CONS U) ")") )
	   ((EQ (FIRST U) 'QUOTE)
	    (MK_LEFORM (SECOND U)))
	   ((EQ (FIRST U) 'EQUAL)
	    (STRCONC "=" (MK_LEFORM (SECOND U)) ))
	   ((EQ (FIRST U) 'SPADLET)
	    (MK_LEFORM (THIRD U)))
	   ((ERRHUH))))

   (defun MK_LEFORM-CONS (U)
     (COND ((ATOM U) 
	    (STRCONC ":" (MK_LEFORM U)))
	   ((EQ (FIRST U) 'APPEND)
	    (STRCONC ":" 
		     (MK_LEFORM (SECOND U))
		     "\," 
		     (MK_LEFORM-CONS (THIRD U)) ))
	   ((EQ (THIRD U) NIL)
	    (MK_LEFORM (SECOND U)))
	   ((STRCONC (MK_LEFORM (SECOND U))
		     "\,"
		     (MK_LEFORM-CONS (THIRD U))))))

   
   (defun DO_LET (VARS INITS)
     (if (OR (NULL VARS) 
	     (NULL INITS)) 
	 NIL
       (CONS (LIST 'SPADLET (CAR VARS) (CAR INITS))
	     (DO_LET (CDR VARS) (CDR INITS)))))

 
   ;; cons-cell constructor is a righ associative.
   (MAKEPROP 'CONS 'RIGHT-ASSOCIATIVE T)

   ;; monoid operators -- leading to reduction. Each operator is
   ;; is paired with its neutral element.   
   (MAPC #'(LAMBDA (X) (MAKEPROP (CAR X) 'THETA (CDR X)))
	 '((PLUS 0) 
	   (+ (|Zero|))
	   (|lcm| (|One|)) 
	   (STRCONC "")
	   (|strconc| "")
	   (MAX -999999)
	   (MIN 999999) 
	   (TIMES 1) 
	   (* (|One|)) 
	   (CONS NIL)
	   (APPEND NIL)
	   (|append| NIL) 
	   (UNION NIL) 
	   (|setUnion| NIL)
	   (|gcd| (|Zero|))
	   (|union| NIL)
	   (|append!| NIL)
	   (|and| |true|)
	   (|or| |false|)
	   (AND 'T)
	   (OR NIL)))

 
   (MAPC #'(LAMBDA (J) (MAKEPROP (CAR J) 'UNMACRO (CADR J)))
	 '( (AND AND2)
	    (OR OR2)))
   
   
   (defun AND2 (x y)
     (and x y))
 
   (defun OR2 (x y) (or x y))

   (defun NREVERSE-N (X AXIS)
     (COND ((EQL AXIS 0)
	    (|reverse!| X))
	   ((MAPCAR #'(LAMBDA (Y) (NREVERSE-N Y (1- AXIS))) X))))
 

   (defun REDUCE-1 (OP AXIS BOD)
     (let (u op1 tran iden)
       (SEQ 
	(SETQ OP1 (cond ((EQ OP '\,)
			 'CONS)
			((EQCAR OP 'QUOTE)
			 (CADR OP))
			(OP)))
	(SETQ IDEN (if (SETQ U (GET OP1 'THETA))
		       (CAR U) 
		     'NO_THETA_PROPERTY))
	(SETQ TRAN (if (EQCAR BOD 'COLLECT)
		       (PROG (L BOD1 ITL)
			     (SETQ L (REVERSE (CDR BOD)))
			     (SETQ BOD1 (CAR L))
			     (SETQ ITL (|reverse!| (CDR L)))
			     (RETURN (-REDUCE OP1 AXIS IDEN BOD1 ITL)) )
		     (progn 
		       (SETQ U (-REDUCE-OP OP1 AXIS))
		       (LIST 'REDUCE-N (MKQ (OR (GET U 'UNMACRO) U))
			     (GET OP1 'RIGHT-ASSOCIATIVE)
			     BOD IDEN))))
	(if (EQ OP '\,) 
	    (LIST 'NREVERSE-N TRAN AXIS)
	  TRAN))))
   
   (defun REDUCE-N (OP RIGHT L ACC)
     (COND (RIGHT 
	    (PROG (U L1)
		  (SETQ L1 (|reverse!| L))
		  (SETQ U (REDUCE-N-1 OP 'T L1 ACC))
		  (|reverse!| L1)
		  (RETURN U) ))
	   ((REDUCE-N-1 OP NIL L ACC))))

   (defun REDUCE-N-1 (OP RIGHT L ACC)
     (COND ((EQ ACC 'NO_THETA_PROPERTY)
	    (COND ((NULL L) 
		   (THETA_ERROR OP))
		  ((REDUCE-N-2 OP RIGHT (CDR L) (CAR L))) ))
	   ((REDUCE-N-2 OP RIGHT L ACC))))
 
   (defun REDUCE-N-2 (OP RIGHT L ACC)
     (COND ((NULL L) ACC)
	   (RIGHT 
	    (REDUCE-N-2 OP 
			RIGHT 
			(CDR L)
			(funcall (symbol-function OP) (CAR L) ACC)))
	   ((REDUCE-N-2 OP
			RIGHT 
			(CDR L) 
			(funcall (symbol-function OP) ACC (CAR L))))))
 
 
   (defun DELASC (u v) 
     "Returns a copy of a-list V in which any pair with key U is deleted."
     (cond ((atom v) nil)
	   ((or (atom (car v))
		(not (equal u (caar v))))
	    (cons (car v) (DelAsc u (cdr v))))
	   ((cdr v))))
 
   (defun -REDUCE (OP AXIS Y BODY SPL)
     (PROG (X G AUX EXIT VALUE PRESET CONSCODE RESETCODE)
	   (SETQ G (GENSYM))
	   ;; create preset of accumulate
	   (SETQ PRESET 
		 (COND ((EQ Y 'NO_THETA_PROPERTY)
			(LIST 'SPADLET G (MKQ G)))
		       ((LIST 'SPADLET G Y)) ))
	   (SETQ EXIT 
		 (COND ((SETQ X (ASSOC 'EXIT SPL))
			(SETQ SPL (DELASC 'EXIT SPL))
			(COND ((MEMBER OP '(AND OR))
			       (LIST 'AND G (CADR X)))
			      ((CADR X)) ))
		       ((EQ Y 'NO_THETA_PROPERTY)
			(LIST 'THETACHECK G (MKQ G)(MKQ OP)))
		       (G) ))
	   (COND ((EQ OP 'CONS)
		  (SETQ EXIT (LIST '|reverse!| EXIT))))
	   ;; CONSCODE= code which conses a member onto the list
	   (SETQ VALUE 
		 (COND ((EQ Y 'NO_THETA_PROPERTY)
			(GENSYM))
		       (BODY)))
	   (SETQ CONSCODE 
		 (CONS (-REDUCE-OP OP AXIS) 
		       (COND ((FLAGP OP 'RIGHT-ASSOCIATIVE)
			      (LIST VALUE G))
			     ((LIST G VALUE) ) ) ) )
	   ;; next reset code which varies if THETA property is|/is not given
	   (SETQ RESETCODE (LIST 'SETQ 
				 G 
				 (COND ((EQ Y 'NO_THETA_PROPERTY)
					(LIST 'COND 
					      (LIST (LIST 'EQ G (MKQ G)) VALUE)
					      (LIST ''T CONSCODE)) )
				       (CONSCODE) )))
	   ;; create body
	   (SETQ BODY 
		 (COND ((EQ VALUE BODY)
			RESETCODE)
		       ((LIST 'PROGN
			      (LIST 'SPADLET VALUE BODY)
			      RESETCODE)) ))
	   (SETQ AUX 
		 (CONS (LIST 'EXIT EXIT)
		       (COND ((EQ OP 'AND) 
			      (LIST (LIST 'UNTIL (LIST 'NULL G))))
			     ((EQ OP 'OR)
			      (LIST (LIST 'UNTIL G)))
			     (NIL) )))
	   (RETURN (COND ((AND $NEWSPAD)
			  (LIST 'PROGN 
				PRESET
				(CONS 'REPEAT 
				      (APPEND AUX (APPEND SPL (LIST BODY))) )))
			 ((LIST 'PROG
				(COND ((EQ RESETCODE BODY)
				       (LIST G))
				      ((LIST G VALUE)))
				PRESET 
				(LIST 'RETURN
				      (CONS 'REPEAT 
					    (APPEND AUX 
						    (APPEND SPL (LIST BODY)))))))))))

   (defun CONS-N (X Y)
     (COND ((NULL Y) 
	    (CONS-N X (NLIST (LENGTH X) NIL)))
	   ((MAPCAR #'CONS X Y))))
 
   (defun APPEND-N (X Y)
     (COND ((NULL X)
	    (APPEND-N (NLIST (LENGTH Y) NIL) Y))
	   ((MAPCAR #'APPEND X Y))))
   
   (defun -REDUCE-OP (OP AXIS)
     (COND ((EQL AXIS 0)
	    OP)
	   ((EQL AXIS 1)
	    (COND ((EQ OP 'CONS)
		   'CONS-N)
		  ((EQ OP 'APPEND)
		   'APPEND-N)
		  ((FAIL))))
	   ((FAIL))))



   ;; # Gives the number of elements of a list, 0 for atoms.
   ;; If we quote it, then an interpreter trip is necessary every time
   ;; we call #, and this costs us - 4% in the RATINT DEMO."
   (define-function '|#| #'SIZE)
    
   ))

;; 
;; -*- Iteration -*-
;; 
 
(defmacro REPEAT (&rest L)
  (let ((U (REPEAT-TRAN L NIL)))
    (-REPEAT (CDR U) (CAR U))))
 
(defmacro SUCHTHATCLAUSE  (&rest L)
  (LIST 'COND (LIST (CADR L) (CAR L))))

(defmacro SPADDO (&rest OL)
  (PROG (VARS L VL V U INITS U-VARS U-VALS ENDTEST EXITFORMS BODYFORMS)
	(if (NOT $NEWSPAD)
	    (return (CONS 'DO OL)))
	(SETQ L  (COPY-LIST OL))
	(if (OR (ATOM L) (ATOM (CDR L)))
	    (GO BADO))
	(setq vl (POP L))
	(COND ((|ident?| VL)
	       (SETQ VARS (LIST VL))
	       (AND (OR (ATOM L)
			(ATOM (progn (setq inits (POP L)) L))
			(ATOM (progn (setq u-vals (pop L)) L)))
		    (GO BADO))
	       (SETQ INITS (LIST INITS)
		     U-VARS (LIST (CAR VARS))
		     U-VALS (LIST U-VALS))
	       (setq endtest (POP L)))
	      ((prog nil
		     (COND ((NULL VL) 
			    (GO TG5)) 
			   ((ATOM VL) 
			    (GO BADO)))
		     G180   
		     (AND (NOT (CONSP (SETQ V (CAR VL)))) 
			  (SETQ V (LIST V)))
		     (AND (NOT (|ident?| (CAR V)))
			  (GO BADO))
		     (PUSH (CAR V) VARS)
		     (PUSH (COND ((CONSP (CDR V)) (CADR V))) INITS)
		     (AND (CONSP (CDR V))
			  (CONSP (CDDR V))
			  (SEQ (PUSH (CAR V) U-VARS)
			       (PUSH (CADDR V) U-VALS)))
		     (AND (CONSP (progn (POP VL) VL))
			  (GO G180))
                    TG5 
		    (setq exitforms (POP L))
		    (and (CONSP EXITFORMS)
			 (progn 
			   (setq endtest (POP EXITFORMS))
			   exitforms)))))
	(AND L
	     (COND ((CDR L) 
		    (SETQ BODYFORMS (CONS 'SEQ L)))
		   ((NULL (EQCAR (CAR L) 'SEQ))
		    (SETQ BODYFORMS (CONS 'SEQ L)))
		   ((SETQ BODYFORMS (CAR L)))))
	(SETQ EXITFORMS `(EXIT ,(|mkpf| EXITFORMS 'PROGN)))
	(AND ENDTEST 
	     (SETQ ENDTEST (LIST 'COND (LIST ENDTEST '(GO G191)))))
	(COND ((NULL U-VARS)
	       (GO XT) )
	      ((NULL (CDR U-VARS))
	       (SEQ 
		(SETQ U-VARS (LIST 'SETQ (CAR U-VARS) (CAR U-VALS)))
		(GO XT)) ))
	(SETQ VL (LIST 'SETQ (CAR U-VARS) (CAR U-VALS)))
	(SEQ 
	 (SETQ V (CDR U-VARS))
	 (SETQ U (CDR U-VALS)))
	TG  
	(SETQ VL (LIST 'SETQ (CAR V) (LIST 'PROG1 (CAR U) VL)))
	(POP U)
	(AND (progn (POP V) V)
	     (GO TG))
	(SETQ U-VARS VL)
	XT  
	(RETURN (COND
		 ((AND $NEWSPAD)
		  (CONS 'SEQ
			(|append!| (DO_LET VARS INITS)
			       (LIST 'G190 
				     ENDTEST 
				     BODYFORMS 
				     U-VARS 
				     '(GO G190)
				     'G191 
				     EXITFORMS))))
		 ((CONS `(LAMBDA ,(NRECONC VARS NIL)
				 (SEQ 
				  G190 
				  ,ENDTEST 
				  ,BODYFORMS 
				  ,U-VARS 
				  (GO G190) 
				  G191 
				  ,EXITFORMS))
			(NRECONC INITS NIL)))))
	BADO  
	(ERROR (FORMAT NIL "BAD DO FORMAT~%~A" OL))))

(defmacro THETA (&rest LL)
  (let (U (L (|copyList| LL)))
    (if (EQ (KAR L) '\,) 
	`(theta CONS . ,(CDR L))
      (progn
	(if (EQCAR (CAR L) 'QUOTE) 
	    (RPLACA L (CADAR L)))
	(-REDUCE (CAR L) 0
		 (if (SETQ U (GET (CAR L) 'THETA))
		     (CAR U)
		   (MOAN "NO THETA PROPERTY"))
		 (CAR (SETQ L (|reverse!| (CDR L))))
		 (|reverse!| (CDR L)))))))

(defmacro THETA1 (&rest LL)
  (let (U (L (|copyList| LL)))
    (if (EQ (KAR L) '\,)
        (LIST 'NREVERSE-N (CONS 'THETA1 (CONS 'CONS (CDR L))) 1)
      (-REDUCE (CAR L) 
	       1
	       (if (SETQ U (GET (CAR L) 'THETA))
		   (CAR U)
		 (MOAN "NO THETA PROPERTY"))
	       (CAR (SETQ L (|reverse!| (CDR L))))
	       (|reverse!| (CDR L))))))

(defmacro SPADREDUCE (OP AXIS BOD)
  (REDUCE-1 OP AXIS BOD))

;; 
;; -*- List Comprehension -*-
;; 
 
(defmacro COLLECT (&rest L)
  (let ((U (REPEAT-TRAN L NIL)))
    (CONS 'THETA (CONS '\, (|append!| (CAR U) (LIST (CDR U)))))))

;; 
;; -*- Input/Output -*-
;; 

(defmacro |report| (L)
  (SUBST (SECOND L) 'x
         '(COND ($reportFlag (sayBrightly x)) ((QUOTE T) NIL))))

;; Quietly execute `cmd' if in interactive session
(defmacro |quietlyIfInteractive| (cmd)
  `(let ((|$OutputStream| (if |$InteractiveMode|
			      (make-broadcast-stream)
			    (|forkStreamByName| '*standard-output*))))
     ,cmd))

(defmacro |withOutputFile| (stream filespec form)
  `(with-open-file (,stream ,filespec :direction :output 
			    :if-exists :supersede)
		   ,form))

(defmacro |spadConstant| (dollar n)
 `(spadcall (svref ,dollar (the fixnum ,n))))

(defmacro |shellEntry| (dollar n)
  `(svref ,dollar ,n))

(defmacro |assert| (x y)
  `(if (null ,x) (|error| ,y)))


(defmacro |:| (tag expr)
  `(list '|:| ,(mkq tag) ,expr))


