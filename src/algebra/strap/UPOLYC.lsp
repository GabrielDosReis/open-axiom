
(/VERSIONCHECK 2) 

(DEFPARAMETER |UnivariatePolynomialCategory;CAT| 'NIL) 

(DEFPARAMETER |UnivariatePolynomialCategory;AL| 'NIL) 

(DEFUN |UnivariatePolynomialCategory;| (|t#1|)
  (PROG (#0=#:G1435)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (|sublisV|
                           (PAIR '(#1=#:G1433 #2=#:G1434)
                                 (LIST '(|NonNegativeInteger|)
                                       '(|SingletonAsOrderedSet|)))
                           (COND
                             (|UnivariatePolynomialCategory;CAT|)
                             ('T
                              (LETT |UnivariatePolynomialCategory;CAT|
                                    (|Join|
                                     (|PolynomialCategory| '|t#1| '#1#
                                      '#2#)
                                     (|Eltable| '|t#1| '|t#1|)
                                     (|Eltable| '$ '$)
                                     (|DifferentialRing|)
                                     (|DifferentialExtension| '|t#1|)
                                     (|mkCategory| '|domain|
                                      '(((|vectorise|
                                          ((|Vector| |t#1|) $
                                           (|NonNegativeInteger|)))
                                         T)
                                        ((|makeSUP|
                                          ((|SparseUnivariatePolynomial|
                                            |t#1|)
                                           $))
                                         T)
                                        ((|unmakeSUP|
                                          ($
                                           (|SparseUnivariatePolynomial|
                                            |t#1|)))
                                         T)
                                        ((|multiplyExponents|
                                          ($ $ (|NonNegativeInteger|)))
                                         T)
                                        ((|divideExponents|
                                          ((|Union| $ "failed") $
                                           (|NonNegativeInteger|)))
                                         T)
                                        ((|monicDivide|
                                          ((|Record| (|:| |quotient| $)
                                            (|:| |remainder| $))
                                           $ $))
                                         T)
                                        ((|karatsubaDivide|
                                          ((|Record| (|:| |quotient| $)
                                            (|:| |remainder| $))
                                           $ (|NonNegativeInteger|)))
                                         T)
                                        ((|shiftRight|
                                          ($ $ (|NonNegativeInteger|)))
                                         T)
                                        ((|shiftLeft|
                                          ($ $ (|NonNegativeInteger|)))
                                         T)
                                        ((|pseudoRemainder| ($ $ $)) T)
                                        ((|differentiate|
                                          ($ $ (|Mapping| |t#1| |t#1|)
                                           $))
                                         T)
                                        ((|discriminant| (|t#1| $))
                                         (|has| |t#1|
                                          (|CommutativeRing|)))
                                        ((|resultant| (|t#1| $ $))
                                         (|has| |t#1|
                                          (|CommutativeRing|)))
                                        ((|elt|
                                          ((|Fraction| $)
                                           (|Fraction| $)
                                           (|Fraction| $)))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|order|
                                          ((|NonNegativeInteger|) $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|subResultantGcd| ($ $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|composite|
                                          ((|Union| $ "failed") $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|composite|
                                          ((|Union| (|Fraction| $)
                                            "failed")
                                           (|Fraction| $) $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|pseudoQuotient| ($ $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|pseudoDivide|
                                          ((|Record| (|:| |coef| |t#1|)
                                            (|:| |quotient| $)
                                            (|:| |remainder| $))
                                           $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|separate|
                                          ((|Record|
                                            (|:| |primePart| $)
                                            (|:| |commonPart| $))
                                           $ $))
                                         (|has| |t#1| (|GcdDomain|)))
                                        ((|elt|
                                          (|t#1| (|Fraction| $) |t#1|))
                                         (|has| |t#1| (|Field|)))
                                        ((|integrate| ($ $))
                                         (|has| |t#1|
                                          (|Algebra|
                                           (|Fraction| (|Integer|))))))
                                      '(((|StepThrough|)
                                         (|has| |t#1| (|StepThrough|)))
                                        ((|Eltable| (|Fraction| $)
                                          (|Fraction| $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|EuclideanDomain|)
                                         (|has| |t#1| (|Field|)))
                                        (|additiveValuation|
                                         (|has| |t#1| (|Field|))))
                                      '((|Fraction| $)
                                        (|NonNegativeInteger|)
                                        (|SparseUnivariatePolynomial|
                                         |t#1|)
                                        (|Vector| |t#1|))
                                      NIL))
                                    . #3=(|UnivariatePolynomialCategory|)))))) . #3#)
        (SETELT #0# 0
                (LIST '|UnivariatePolynomialCategory|
                      (|devaluate| |t#1|))))))) 

(DEFUN |UnivariatePolynomialCategory| (#0=#:G1436)
  (LET (#1=#:G1437)
    (COND
      ((SETQ #1#
             (|assoc| (|devaluate| #0#)
                      |UnivariatePolynomialCategory;AL|))
       (CDR #1#))
      (T (SETQ |UnivariatePolynomialCategory;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1#
                                    (|UnivariatePolynomialCategory;|
                                     #0#)))
                        |UnivariatePolynomialCategory;AL|))
         #1#)))) 

(SETQ |$CategoryFrame|
      (|put| '|UnivariatePolynomialCategory| '|isCategory| T
             (|addModemap| '|UnivariatePolynomialCategory|
                 '(|UnivariatePolynomialCategory| |#1|)
                 '((|Category|) (|Ring|)) T
                 '|UnivariatePolynomialCategory| |$CategoryFrame|))) 
