
(/VERSIONCHECK 2) 

(DEFPARAMETER |IntegerNumberSystem;AL| 'NIL) 

(DEFUN |IntegerNumberSystem;| ()
  (PROG (#0=#:G1413)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1407 #2=#:G1408 #3=#:G1409
                                  #4=#:G1410 #5=#:G1411 #6=#:G1412)
                             (LIST '(|Integer|) '(|Integer|)
                                   '(|Integer|) '(|InputForm|)
                                   '(|Pattern| (|Integer|))
                                   '(|Integer|)))
                       (|Join| (|UniqueFactorizationDomain|)
                               (|EuclideanDomain|)
                               (|OrderedIntegralDomain|)
                               (|DifferentialRing|)
                               (|ConvertibleTo| '#1#)
                               (|RetractableTo| '#2#)
                               (|LinearlyExplicitRingOver| '#3#)
                               (|ConvertibleTo| '#4#)
                               (|ConvertibleTo| '#5#)
                               (|PatternMatchable| '#6#)
                               (|CombinatorialFunctionCategory|)
                               (|RealConstant|) (|CharacteristicZero|)
                               (|StepThrough|)
                               (|mkCategory| '|domain|
                                   '(((|odd?| ((|Boolean|) $)) T)
                                     ((|even?| ((|Boolean|) $)) T)
                                     ((|base| ($)) T)
                                     ((|length| ($ $)) T)
                                     ((|shift| ($ $ $)) T)
                                     ((|bit?| ((|Boolean|) $ $)) T)
                                     ((|positiveRemainder| ($ $ $)) T)
                                     ((|symmetricRemainder| ($ $ $)) T)
                                     ((|rational?| ((|Boolean|) $)) T)
                                     ((|rational|
                                       ((|Fraction| (|Integer|)) $))
                                      T)
                                     ((|rationalIfCan|
                                       ((|Union|
                                         (|Fraction| (|Integer|))
                                         "failed")
                                        $))
                                      T)
                                     ((|random| ($)) T)
                                     ((|random| ($ $)) T)
                                     ((|copy| ($ $)) T)
                                     ((|inc| ($ $)) T)
                                     ((|dec| ($ $)) T)
                                     ((|mask| ($ $)) T)
                                     ((|addmod| ($ $ $ $)) T)
                                     ((|submod| ($ $ $ $)) T)
                                     ((|mulmod| ($ $ $ $)) T)
                                     ((|powmod| ($ $ $ $)) T)
                                     ((|invmod| ($ $ $)) T))
                                   '((|multiplicativeValuation| T)
                                     (|canonicalUnitNormal| T))
                                   '((|Fraction| (|Integer|))
                                     (|Boolean|))
                                   NIL)))
                   |IntegerNumberSystem|)
        (|setShellEntry| #0# 0 '(|IntegerNumberSystem|)))))) 

(DEFUN |IntegerNumberSystem| ()
  (LET ()
    (COND
      (|IntegerNumberSystem;AL|)
      (T (SETQ |IntegerNumberSystem;AL| (|IntegerNumberSystem;|)))))) 

(MAKEPROP '|IntegerNumberSystem| 'NILADIC T) 
