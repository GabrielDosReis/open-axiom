
(/VERSIONCHECK 2) 

(DEFPARAMETER |GcdDomain;AL| 'NIL) 

(DEFUN |GcdDomain| ()
  (LET (#:G1393)
    (COND (|GcdDomain;AL|) (T (SETQ |GcdDomain;AL| (|GcdDomain;|)))))) 

(DEFUN |GcdDomain;| ()
  (PROG (#0=#:G1391)
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
        (SETELT #0# 0 '(|GcdDomain|)))))) 

(MAKEPROP '|GcdDomain| 'NILADIC T) 
