
(/VERSIONCHECK 2) 

(DEFPARAMETER |GcdDomain;AL| 'NIL) 

(DEFUN |GcdDomain;| ()
  (LET ((#0=#:G1378
            (|Join| (|IntegralDomain|)
                    (|mkCategory| '|domain|
                        '(((|gcd| ($ $ $)) T)
                          ((|gcd| ($ (|List| $))) T)
                          ((|lcm| ($ $ $)) T)
                          ((|lcm| ($ (|List| $))) T)
                          ((|gcdPolynomial|
                               ((|SparseUnivariatePolynomial| $)
                                (|SparseUnivariatePolynomial| $)
                                (|SparseUnivariatePolynomial| $)))
                           T))
                        NIL
                        '((|SparseUnivariatePolynomial| $) (|List| $))
                        NIL))))
    (SETF (|shellEntry| #0# 0) '(|GcdDomain|))
    #0#)) 

(DEFUN |GcdDomain| ()
  (COND (|GcdDomain;AL|) (T (SETQ |GcdDomain;AL| (|GcdDomain;|))))) 

(MAKEPROP '|GcdDomain| 'NILADIC T) 
