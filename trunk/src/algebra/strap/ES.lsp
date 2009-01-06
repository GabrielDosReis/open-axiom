
(/VERSIONCHECK 2) 

(DEFPARAMETER |ExpressionSpace;AL| 'NIL) 

(DEFUN |ExpressionSpace;| ()
  (PROG (#0=#:G1413)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1411 #2=#:G1412)
                             (LIST '(|Kernel| $) '(|Kernel| $)))
                       (|Join| (|OrderedSet|) (|RetractableTo| '#1#)
                               (|InnerEvalable| '#2# '$)
                               (|Evalable| '$)
                               (|mkCategory| '|domain|
                                   '(((|elt| ($ (|BasicOperator|) $))
                                      T)
                                     ((|elt| ($ (|BasicOperator|) $ $))
                                      T)
                                     ((|elt|
                                       ($ (|BasicOperator|) $ $ $))
                                      T)
                                     ((|elt|
                                       ($ (|BasicOperator|) $ $ $ $))
                                      T)
                                     ((|elt|
                                       ($ (|BasicOperator|) (|List| $)))
                                      T)
                                     ((|subst| ($ $ (|Equation| $))) T)
                                     ((|subst|
                                       ($ $ (|List| (|Equation| $))))
                                      T)
                                     ((|subst|
                                       ($ $ (|List| (|Kernel| $))
                                        (|List| $)))
                                      T)
                                     ((|box| ($ $)) T)
                                     ((|box| ($ (|List| $))) T)
                                     ((|paren| ($ $)) T)
                                     ((|paren| ($ (|List| $))) T)
                                     ((|distribute| ($ $)) T)
                                     ((|distribute| ($ $ $)) T)
                                     ((|height|
                                       ((|NonNegativeInteger|) $))
                                      T)
                                     ((|mainKernel|
                                       ((|Union| (|Kernel| $) "failed")
                                        $))
                                      T)
                                     ((|kernels|
                                       ((|List| (|Kernel| $)) $))
                                      T)
                                     ((|tower|
                                       ((|List| (|Kernel| $)) $))
                                      T)
                                     ((|operators|
                                       ((|List| (|BasicOperator|)) $))
                                      T)
                                     ((|operator|
                                       ((|BasicOperator|)
                                        (|BasicOperator|)))
                                      T)
                                     ((|belong?|
                                       ((|Boolean|) (|BasicOperator|)))
                                      T)
                                     ((|is?|
                                       ((|Boolean|) $
                                        (|BasicOperator|)))
                                      T)
                                     ((|is?|
                                       ((|Boolean|) $ (|Symbol|)))
                                      T)
                                     ((|kernel|
                                       ($ (|BasicOperator|) $))
                                      T)
                                     ((|kernel|
                                       ($ (|BasicOperator|) (|List| $)))
                                      T)
                                     ((|map|
                                       ($ (|Mapping| $ $) (|Kernel| $)))
                                      T)
                                     ((|freeOf?| ((|Boolean|) $ $)) T)
                                     ((|freeOf?|
                                       ((|Boolean|) $ (|Symbol|)))
                                      T)
                                     ((|eval|
                                       ($ $ (|List| (|Symbol|))
                                        (|List| (|Mapping| $ $))))
                                      T)
                                     ((|eval|
                                       ($ $ (|List| (|Symbol|))
                                        (|List|
                                         (|Mapping| $ (|List| $)))))
                                      T)
                                     ((|eval|
                                       ($ $ (|Symbol|)
                                        (|Mapping| $ (|List| $))))
                                      T)
                                     ((|eval|
                                       ($ $ (|Symbol|) (|Mapping| $ $)))
                                      T)
                                     ((|eval|
                                       ($ $ (|List| (|BasicOperator|))
                                        (|List| (|Mapping| $ $))))
                                      T)
                                     ((|eval|
                                       ($ $ (|List| (|BasicOperator|))
                                        (|List|
                                         (|Mapping| $ (|List| $)))))
                                      T)
                                     ((|eval|
                                       ($ $ (|BasicOperator|)
                                        (|Mapping| $ (|List| $))))
                                      T)
                                     ((|eval|
                                       ($ $ (|BasicOperator|)
                                        (|Mapping| $ $)))
                                      T)
                                     ((|minPoly|
                                       ((|SparseUnivariatePolynomial|
                                         $)
                                        (|Kernel| $)))
                                      (|has| $ (|Ring|)))
                                     ((|definingPolynomial| ($ $))
                                      (|has| $ (|Ring|)))
                                     ((|even?| ((|Boolean|) $))
                                      (|has| $
                                       (|RetractableTo| (|Integer|))))
                                     ((|odd?| ((|Boolean|) $))
                                      (|has| $
                                       (|RetractableTo| (|Integer|)))))
                                   NIL
                                   '((|Boolean|)
                                     (|SparseUnivariatePolynomial| $)
                                     (|Kernel| $) (|BasicOperator|)
                                     (|List| (|BasicOperator|))
                                     (|List| (|Mapping| $ (|List| $)))
                                     (|List| (|Mapping| $ $))
                                     (|Symbol|) (|List| (|Symbol|))
                                     (|List| $) (|List| (|Kernel| $))
                                     (|NonNegativeInteger|)
                                     (|List| (|Equation| $))
                                     (|Equation| $))
                                   NIL)))
                   |ExpressionSpace|)
        (|setShellEntry| #0# 0 '(|ExpressionSpace|)))))) 

(DEFUN |ExpressionSpace| ()
  (LET ()
    (COND
      (|ExpressionSpace;AL|)
      (T (SETQ |ExpressionSpace;AL| (|ExpressionSpace;|)))))) 

(MAKEPROP '|ExpressionSpace| 'NILADIC T) 
