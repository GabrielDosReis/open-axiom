
(/VERSIONCHECK 2) 

(DEFPARAMETER |PolynomialSetCategory;CAT| 'NIL) 

(DEFPARAMETER |PolynomialSetCategory;AL| 'NIL) 

(DEFUN |PolynomialSetCategory;| (|t#1| |t#2| |t#3| |t#4|)
  (PROG (#0=#:G1431)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1| |t#2| |t#3| |t#4|)
                             (LIST (|devaluate| |t#1|)
                                   (|devaluate| |t#2|)
                                   (|devaluate| |t#3|)
                                   (|devaluate| |t#4|)))
                       (|sublisV|
                           (PAIR '(#1=#:G1430) (LIST '(|List| |t#4|)))
                           (COND
                             (|PolynomialSetCategory;CAT|)
                             ('T
                              (LETT |PolynomialSetCategory;CAT|
                                    (|Join| (|SetCategory|)
                                     (|Collection| '|t#4|)
                                     (|CoercibleTo| '#1#)
                                     (|mkCategory| '|domain|
                                      '(((|retractIfCan|
                                          ((|Union| $ "failed")
                                           (|List| |t#4|)))
                                         T)
                                        ((|retract| ($ (|List| |t#4|)))
                                         T)
                                        ((|mvar| (|t#3| $)) T)
                                        ((|variables|
                                          ((|List| |t#3|) $))
                                         T)
                                        ((|mainVariables|
                                          ((|List| |t#3|) $))
                                         T)
                                        ((|mainVariable?|
                                          ((|Boolean|) |t#3| $))
                                         T)
                                        ((|collectUnder| ($ $ |t#3|))
                                         T)
                                        ((|collect| ($ $ |t#3|)) T)
                                        ((|collectUpper| ($ $ |t#3|))
                                         T)
                                        ((|sort|
                                          ((|Record| (|:| |under| $)
                                            (|:| |floor| $)
                                            (|:| |upper| $))
                                           $ |t#3|))
                                         T)
                                        ((|trivialIdeal?|
                                          ((|Boolean|) $))
                                         T)
                                        ((|roughBase?| ((|Boolean|) $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|roughSubIdeal?|
                                          ((|Boolean|) $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|roughEqualIdeals?|
                                          ((|Boolean|) $ $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|roughUnitIdeal?|
                                          ((|Boolean|) $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|headRemainder|
                                          ((|Record| (|:| |num| |t#4|)
                                            (|:| |den| |t#1|))
                                           |t#4| $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|remainder|
                                          ((|Record| (|:| |rnum| |t#1|)
                                            (|:| |polnum| |t#4|)
                                            (|:| |den| |t#1|))
                                           |t#4| $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|rewriteIdealWithHeadRemainder|
                                          ((|List| |t#4|)
                                           (|List| |t#4|) $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|rewriteIdealWithRemainder|
                                          ((|List| |t#4|)
                                           (|List| |t#4|) $))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|triangular?|
                                          ((|Boolean|) $))
                                         (|has| |t#1|
                                          (|IntegralDomain|))))
                                      '((|finiteAggregate| T))
                                      '((|Boolean|) (|List| |t#4|)
                                        (|List| |t#3|))
                                      NIL))
                                    . #2=(|PolynomialSetCategory|)))))) . #2#)
        (|setShellEntry| #0# 0
            (LIST '|PolynomialSetCategory| (|devaluate| |t#1|)
                  (|devaluate| |t#2|) (|devaluate| |t#3|)
                  (|devaluate| |t#4|))))))) 

(DEFUN |PolynomialSetCategory| (&REST #0=#:G1434 &AUX #1=#:G1432)
  (DSETQ #1# #0#)
  (LET (#2=#:G1433)
    (COND
      ((SETQ #2#
             (|assoc| (|devaluateList| #1#) |PolynomialSetCategory;AL|))
       (CDR #2#))
      (T (SETQ |PolynomialSetCategory;AL|
               (|cons5| (CONS (|devaluateList| #1#)
                              (SETQ #2#
                                    (APPLY #'|PolynomialSetCategory;|
                                     #1#)))
                        |PolynomialSetCategory;AL|))
         #2#)))) 
