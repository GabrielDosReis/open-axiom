
(/VERSIONCHECK 2) 

(DEFPARAMETER |GcdDomain;AL| 'NIL) 

(DEFUN |GcdDomain| ()
  (LET (#:G1402)
    (COND (|GcdDomain;AL|) (T (SETQ |GcdDomain;AL| (|GcdDomain;|)))))) 

(DEFUN |GcdDomain;| ()
  (PROG (#0=#:G1400)
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

(SETQ |$CategoryFrame|
      (|put| '|GcdDomain| '|isCategory| T
             (|addModemap| '|GcdDomain| '(|GcdDomain|) '((|Category|))
                 T '|GcdDomain| |$CategoryFrame|))) 

(MAKEPROP '|GcdDomain| 'NILADIC T) 
