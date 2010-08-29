
(/VERSIONCHECK 2) 

(DEFPARAMETER |MultivariateTaylorSeriesCategory;CAT| 'NIL) 

(DEFPARAMETER |MultivariateTaylorSeriesCategory;AL| 'NIL) 

(DEFUN |MultivariateTaylorSeriesCategory;| (|t#1| |t#2|)
  (LET ((#0=#:G1399
            (|sublisV|
                (PAIR '(|t#1| |t#2|)
                      (LIST (|devaluate| |t#1|) (|devaluate| |t#2|)))
                (|sublisV|
                    (PAIR '(#1=#:G1398)
                          (LIST '(|IndexedExponents| |t#2|)))
                    (COND
                      (|MultivariateTaylorSeriesCategory;CAT|)
                      (T (SETQ |MultivariateTaylorSeriesCategory;CAT|
                               (|Join| (|PartialDifferentialRing|
                                        '|t#2|)
                                       (|PowerSeriesCategory| '|t#1|
                                        '#1# '|t#2|)
                                       (|InnerEvalable| '|t#2| '$)
                                       (|Evalable| '$)
                                       (|mkCategory| '|domain|
                                        '(((|coefficient|
                                            ($ $ |t#2|
                                             (|NonNegativeInteger|)))
                                           T)
                                          ((|coefficient|
                                            ($ $ (|List| |t#2|)
                                             (|List|
                                              (|NonNegativeInteger|))))
                                           T)
                                          ((|extend|
                                            ($ $
                                             (|NonNegativeInteger|)))
                                           T)
                                          ((|monomial|
                                            ($ $ |t#2|
                                             (|NonNegativeInteger|)))
                                           T)
                                          ((|monomial|
                                            ($ $ (|List| |t#2|)
                                             (|List|
                                              (|NonNegativeInteger|))))
                                           T)
                                          ((|order|
                                            ((|NonNegativeInteger|) $
                                             |t#2|))
                                           T)
                                          ((|order|
                                            ((|NonNegativeInteger|) $
                                             |t#2|
                                             (|NonNegativeInteger|)))
                                           T)
                                          ((|polynomial|
                                            ((|Polynomial| |t#1|) $
                                             (|NonNegativeInteger|)))
                                           T)
                                          ((|polynomial|
                                            ((|Polynomial| |t#1|) $
                                             (|NonNegativeInteger|)
                                             (|NonNegativeInteger|)))
                                           T)
                                          ((|integrate| ($ $ |t#2|))
                                           (|has| |t#1|
                                            (|Algebra|
                                             (|Fraction| (|Integer|))))))
                                        '(((|RadicalCategory|)
                                           (|has| |t#1|
                                            (|Algebra|
                                             (|Fraction| (|Integer|)))))
                                          ((|TranscendentalFunctionCategory|)
                                           (|has| |t#1|
                                            (|Algebra|
                                             (|Fraction| (|Integer|))))))
                                        '((|Polynomial| |t#1|)
                                          (|NonNegativeInteger|)
                                          (|List| |t#2|)
                                          (|List|
                                           (|NonNegativeInteger|)))
                                        NIL)))))))))
    (|setShellEntry| #0# 0
        (LIST '|MultivariateTaylorSeriesCategory| (|devaluate| |t#1|)
              (|devaluate| |t#2|)))
    #0#)) 

(DEFUN |MultivariateTaylorSeriesCategory|
       (&REST #0=#:G1402 &AUX #1=#:G1400)
  (DSETQ #1# #0#)
  (LET ((#2=#:G1401
            (|assoc| (|devaluateList| #1#)
                     |MultivariateTaylorSeriesCategory;AL|)))
    (COND
      (#2# (CDR #2#))
      (T (PROGN
           (SETQ #2# (APPLY #'|MultivariateTaylorSeriesCategory;| #1#))
           (SETQ |MultivariateTaylorSeriesCategory;AL|
                 (|cons5| (CONS (|devaluateList| #1#) #2#)
                          |MultivariateTaylorSeriesCategory;AL|))
           #2#))))) 
