
(/VERSIONCHECK 2) 

(DEFPARAMETER |FiniteFieldCategory;AL| 'NIL) 

(DEFUN |FiniteFieldCategory;| ()
  (PROG (#0=#:G1405)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|FieldOfPrimeCharacteristic|) (|Finite|)
                           (|StepThrough|) (|DifferentialRing|)
                           (|mkCategory| '|domain|
                               '(((|charthRoot| ($ $)) T)
                                 ((|conditionP|
                                      ((|Union| (|Vector| $) "failed")
                                       (|Matrix| $)))
                                  T)
                                 ((|factorsOfCyclicGroupSize|
                                      ((|List|
                                        (|Record|
                                         (|:| |factor| (|Integer|))
                                         (|:| |exponent| (|Integer|))))))
                                  T)
                                 ((|tableForDiscreteLogarithm|
                                      ((|Table| (|PositiveInteger|)
                                        (|NonNegativeInteger|))
                                       (|Integer|)))
                                  T)
                                 ((|createPrimitiveElement| ($)) T)
                                 ((|primitiveElement| ($)) T)
                                 ((|primitive?| ((|Boolean|) $)) T)
                                 ((|discreteLog|
                                      ((|NonNegativeInteger|) $))
                                  T)
                                 ((|order| ((|PositiveInteger|) $)) T)
                                 ((|representationType|
                                      ((|Union| "prime" "polynomial"
                                        "normal" "cyclic")))
                                  T))
                               NIL
                               '((|PositiveInteger|)
                                 (|NonNegativeInteger|) (|Boolean|)
                                 (|Table| (|PositiveInteger|)
                                          (|NonNegativeInteger|))
                                 (|Integer|)
                                 (|List| (|Record|
                                          (|:| |factor| (|Integer|))
                                          (|:| |exponent| (|Integer|))))
                                 (|Matrix| $))
                               NIL))
                   |FiniteFieldCategory|)
        (|setShellEntry| #0# 0 '(|FiniteFieldCategory|)))))) 

(DEFUN |FiniteFieldCategory| ()
  (LET ()
    (COND
      (|FiniteFieldCategory;AL|)
      (T (SETQ |FiniteFieldCategory;AL| (|FiniteFieldCategory;|)))))) 

(MAKEPROP '|FiniteFieldCategory| 'NILADIC T) 
