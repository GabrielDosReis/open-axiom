
(/VERSIONCHECK 2) 

(DEFPARAMETER |GcdDomain;AL| 'NIL) 

(DEFUN |GcdDomain;| ()
  (PROG (#0=#:G1404)
    (RETURN
      (PROG1 (LETT #0#
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
                               '((|SparseUnivariatePolynomial| $)
                                 (|List| $))
                               NIL))
                   |GcdDomain|)
        (|setShellEntry| #0# 0 '(|GcdDomain|)))))) 

(DEFUN |GcdDomain| ()
  (LET ()
    (COND (|GcdDomain;AL|) (T (SETQ |GcdDomain;AL| (|GcdDomain;|)))))) 

(MAKEPROP '|GcdDomain| 'NILADIC T) 
