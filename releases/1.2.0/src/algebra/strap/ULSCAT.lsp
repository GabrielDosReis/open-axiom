
(/VERSIONCHECK 2) 

(DEFPARAMETER |UnivariateLaurentSeriesCategory;CAT| 'NIL) 

(DEFPARAMETER |UnivariateLaurentSeriesCategory;AL| 'NIL) 

(DEFUN |UnivariateLaurentSeriesCategory| (#0=#:G1397)
  (LET (#1=#:G1398)
    (COND
      ((SETQ #1#
             (|assoc| (|devaluate| #0#)
                      |UnivariateLaurentSeriesCategory;AL|))
       (CDR #1#))
      (T (SETQ |UnivariateLaurentSeriesCategory;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1#
                                    (|UnivariateLaurentSeriesCategory;|
                                     #0#)))
                        |UnivariateLaurentSeriesCategory;AL|))
         #1#)))) 

(DEFUN |UnivariateLaurentSeriesCategory;| (|t#1|)
  (PROG (#0=#:G1396)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (|sublisV|
                           (PAIR '(#1=#:G1395) (LIST '(|Integer|)))
                           (COND
                             (|UnivariateLaurentSeriesCategory;CAT|)
                             ('T
                              (LETT |UnivariateLaurentSeriesCategory;CAT|
                                    (|Join|
                                     (|UnivariatePowerSeriesCategory|
                                      '|t#1| '#1#)
                                     (|mkCategory| '|domain|
                                      '(((|series|
                                          ($
                                           (|Stream|
                                            (|Record|
                                             (|:| |k| (|Integer|))
                                             (|:| |c| |t#1|)))))
                                         T)
                                        ((|multiplyCoefficients|
                                          ($
                                           (|Mapping| |t#1|
                                            (|Integer|))
                                           $))
                                         T)
                                        ((|rationalFunction|
                                          ((|Fraction|
                                            (|Polynomial| |t#1|))
                                           $ (|Integer|)))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|rationalFunction|
                                          ((|Fraction|
                                            (|Polynomial| |t#1|))
                                           $ (|Integer|) (|Integer|)))
                                         (|has| |t#1|
                                          (|IntegralDomain|)))
                                        ((|integrate| ($ $))
                                         (|has| |t#1|
                                          (|Algebra|
                                           (|Fraction| (|Integer|)))))
                                        ((|integrate| ($ $ (|Symbol|)))
                                         (AND
                                          (|has| |t#1|
                                           (SIGNATURE |variables|
                                            ((|List| (|Symbol|)) |t#1|)))
                                          (|has| |t#1|
                                           (SIGNATURE |integrate|
                                            (|t#1| |t#1| (|Symbol|))))
                                          (|has| |t#1|
                                           (|Algebra|
                                            (|Fraction| (|Integer|))))))
                                        ((|integrate| ($ $ (|Symbol|)))
                                         (AND
                                          (|has| |t#1|
                                           (|AlgebraicallyClosedFunctionSpace|
                                            (|Integer|)))
                                          (|has| |t#1|
                                           (|PrimitiveFunctionCategory|))
                                          (|has| |t#1|
                                           (|TranscendentalFunctionCategory|))
                                          (|has| |t#1|
                                           (|Algebra|
                                            (|Fraction| (|Integer|)))))))
                                      '(((|RadicalCategory|)
                                         (|has| |t#1|
                                          (|Algebra|
                                           (|Fraction| (|Integer|)))))
                                        ((|TranscendentalFunctionCategory|)
                                         (|has| |t#1|
                                          (|Algebra|
                                           (|Fraction| (|Integer|)))))
                                        ((|Field|)
                                         (|has| |t#1| (|Field|))))
                                      '((|Symbol|)
                                        (|Fraction|
                                         (|Polynomial| |t#1|))
                                        (|Integer|)
                                        (|Stream|
                                         (|Record|
                                          (|:| |k| (|Integer|))
                                          (|:| |c| |t#1|))))
                                      NIL))
                                    . #2=(|UnivariateLaurentSeriesCategory|)))))) . #2#)
        (SETELT #0# 0
                (LIST '|UnivariateLaurentSeriesCategory|
                      (|devaluate| |t#1|))))))) 

(SETQ |$CategoryFrame|
      (|put| '|UnivariateLaurentSeriesCategory| '|isCategory| T
             (|addModemap| '|UnivariateLaurentSeriesCategory|
                 '(|UnivariateLaurentSeriesCategory| |#1|)
                 '((|Category|) (|Ring|)) T
                 '|UnivariateLaurentSeriesCategory| |$CategoryFrame|))) 
