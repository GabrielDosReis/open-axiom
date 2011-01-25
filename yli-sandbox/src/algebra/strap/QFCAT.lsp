
(/VERSIONCHECK 2) 

(DEFPARAMETER |QuotientFieldCategory;CAT| 'NIL) 

(DEFPARAMETER |QuotientFieldCategory;AL| 'NIL) 

(DEFUN |QuotientFieldCategory;| (|t#1|)
  (LET ((#0=#:G1374
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|QuotientFieldCategory;CAT|)
                  (T (SETQ |QuotientFieldCategory;CAT|
                           (|Join| (|Field|) (|Algebra| '|t#1|)
                                   (|RetractableTo| '|t#1|)
                                   (|FullyEvalableOver| '|t#1|)
                                   (|DifferentialExtension| '|t#1|)
                                   (|FullyLinearlyExplicitRingOver|
                                    '|t#1|)
                                   (|Patternable| '|t#1|)
                                   (|FullyPatternMatchable| '|t#1|)
                                   (|mkCategory| '|domain|
                                    '(((/ ($ |t#1| |t#1|)) T)
                                      ((|numer| (|t#1| $)) T)
                                      ((|denom| (|t#1| $)) T)
                                      ((|numerator| ($ $)) T)
                                      ((|denominator| ($ $)) T)
                                      ((|wholePart| (|t#1| $))
                                       (|has| |t#1|
                                        (|EuclideanDomain|)))
                                      ((|fractionPart| ($ $))
                                       (|has| |t#1|
                                        (|EuclideanDomain|)))
                                      ((|random| ($))
                                       (|has| |t#1|
                                        (|IntegerNumberSystem|)))
                                      ((|ceiling| (|t#1| $))
                                       (|has| |t#1|
                                        (|IntegerNumberSystem|)))
                                      ((|floor| (|t#1| $))
                                       (|has| |t#1|
                                        (|IntegerNumberSystem|))))
                                    '(((|StepThrough|)
                                       (|has| |t#1| (|StepThrough|)))
                                      ((|RetractableTo| (|Integer|))
                                       (|has| |t#1|
                                        (|RetractableTo| (|Integer|))))
                                      ((|RetractableTo|
                                        (|Fraction| (|Integer|)))
                                       (|has| |t#1|
                                        (|RetractableTo| (|Integer|))))
                                      ((|OrderedSet|)
                                       (|has| |t#1| (|OrderedSet|)))
                                      ((|OrderedIntegralDomain|)
                                       (|has| |t#1|
                                        (|OrderedIntegralDomain|)))
                                      ((|RealConstant|)
                                       (|has| |t#1| (|RealConstant|)))
                                      ((|ConvertibleTo| (|InputForm|))
                                       (|has| |t#1|
                                        (|ConvertibleTo| (|InputForm|))))
                                      ((|CharacteristicZero|)
                                       (|has| |t#1|
                                        (|CharacteristicZero|)))
                                      ((|CharacteristicNonZero|)
                                       (|has| |t#1|
                                        (|CharacteristicNonZero|)))
                                      ((|RetractableTo| (|Symbol|))
                                       (|has| |t#1|
                                        (|RetractableTo| (|Symbol|))))
                                      ((|PolynomialFactorizationExplicit|)
                                       (|has| |t#1|
                                        (|PolynomialFactorizationExplicit|))))
                                    'NIL NIL))))))))
    (|setShellEntry| #0# 0
        (LIST '|QuotientFieldCategory| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |QuotientFieldCategory| (#0=#:G1375)
  (LET ((#1=#:G1376
            (|assoc| (|devaluate| #0#) |QuotientFieldCategory;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|QuotientFieldCategory;| #0#))
           (SETQ |QuotientFieldCategory;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |QuotientFieldCategory;AL|))
           #1#))))) 
