
(/VERSIONCHECK 2) 

(DEFPARAMETER |IntegerNumberSystem;AL| 'NIL) 

(DEFUN |IntegerNumberSystem;| ()
  (LET ((#0=#:G1389
            (|sublisV|
                (PAIR '(#1=#:G1383 #2=#:G1384 #3=#:G1385 #4=#:G1386
                           #5=#:G1387 #6=#:G1388)
                      '((|Integer|) (|Integer|) (|Integer|)
                        (|InputForm|) (|Pattern| (|Integer|))
                        (|Integer|)))
                (|Join| (|UniqueFactorizationDomain|)
                        (|EuclideanDomain|) (|OrderedIntegralDomain|)
                        (|DifferentialRing|) (|ConvertibleTo| '#1#)
                        (|RetractableTo| '#2#)
                        (|LinearlyExplicitRingOver| '#3#)
                        (|ConvertibleTo| '#4#) (|ConvertibleTo| '#5#)
                        (|PatternMatchable| '#6#)
                        (|CombinatorialFunctionCategory|)
                        (|RealConstant|) (|CharacteristicZero|)
                        (|StepThrough|)
                        (|mkCategory| '|domain|
                            '(((|odd?| ((|Boolean|) $)) T)
                              ((|even?| ((|Boolean|) $)) T)
                              ((|base| ($)) T) ((|length| ($ $)) T)
                              ((|shift| ($ $ $)) T)
                              ((|bit?| ((|Boolean|) $ $)) T)
                              ((|positiveRemainder| ($ $ $)) T)
                              ((|symmetricRemainder| ($ $ $)) T)
                              ((|rational?| ((|Boolean|) $)) T)
                              ((|rational|
                                   ((|Fraction| (|Integer|)) $))
                               T)
                              ((|rationalIfCan|
                                   ((|Union| (|Fraction| (|Integer|))
                                     "failed")
                                    $))
                               T)
                              ((|random| ($)) T) ((|random| ($ $)) T)
                              ((|copy| ($ $)) T) ((|inc| ($ $)) T)
                              ((|dec| ($ $)) T) ((|mask| ($ $)) T)
                              ((|addmod| ($ $ $ $)) T)
                              ((|submod| ($ $ $ $)) T)
                              ((|mulmod| ($ $ $ $)) T)
                              ((|powmod| ($ $ $ $)) T)
                              ((|invmod| ($ $ $)) T))
                            '((|multiplicativeValuation| T)
                              (|canonicalUnitNormal| T))
                            '((|Fraction| (|Integer|)) (|Boolean|))
                            NIL)))))
    (|setShellEntry| #0# 0 '(|IntegerNumberSystem|))
    #0#)) 

(DEFUN |IntegerNumberSystem| ()
  (COND
    (|IntegerNumberSystem;AL|)
    (T (SETQ |IntegerNumberSystem;AL| (|IntegerNumberSystem;|))))) 

(MAKEPROP '|IntegerNumberSystem| 'NILADIC T) 
