
(/VERSIONCHECK 2) 

(DEFPARAMETER |TriangularSetCategory;CAT| 'NIL) 

(DEFPARAMETER |TriangularSetCategory;AL| 'NIL) 

(DEFUN |TriangularSetCategory;| (|t#1| |t#2| |t#3| |t#4|)
  (PROG (#0=#:G1448)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1| |t#2| |t#3| |t#4|)
                             (LIST (|devaluate| |t#1|)
                                   (|devaluate| |t#2|)
                                   (|devaluate| |t#3|)
                                   (|devaluate| |t#4|)))
                       (COND
                         (|TriangularSetCategory;CAT|)
                         ('T
                          (LETT |TriangularSetCategory;CAT|
                                (|Join| (|PolynomialSetCategory| '|t#1|
                                         '|t#2| '|t#3| '|t#4|)
                                        (|mkCategory| '|domain|
                                         '(((|infRittWu?|
                                             ((|Boolean|) $ $))
                                            T)
                                           ((|basicSet|
                                             ((|Union|
                                               (|Record| (|:| |bas| $)
                                                (|:| |top|
                                                 (|List| |t#4|)))
                                               "failed")
                                              (|List| |t#4|)
                                              (|Mapping| (|Boolean|)
                                               |t#4| |t#4|)))
                                            T)
                                           ((|basicSet|
                                             ((|Union|
                                               (|Record| (|:| |bas| $)
                                                (|:| |top|
                                                 (|List| |t#4|)))
                                               "failed")
                                              (|List| |t#4|)
                                              (|Mapping| (|Boolean|)
                                               |t#4|)
                                              (|Mapping| (|Boolean|)
                                               |t#4| |t#4|)))
                                            T)
                                           ((|initials|
                                             ((|List| |t#4|) $))
                                            T)
                                           ((|degree|
                                             ((|NonNegativeInteger|) $))
                                            T)
                                           ((|quasiComponent|
                                             ((|Record|
                                               (|:| |close|
                                                (|List| |t#4|))
                                               (|:| |open|
                                                (|List| |t#4|)))
                                              $))
                                            T)
                                           ((|normalized?|
                                             ((|Boolean|) |t#4| $))
                                            T)
                                           ((|normalized?|
                                             ((|Boolean|) $))
                                            T)
                                           ((|reduced?|
                                             ((|Boolean|) |t#4| $
                                              (|Mapping| (|Boolean|)
                                               |t#4| |t#4|)))
                                            T)
                                           ((|stronglyReduced?|
                                             ((|Boolean|) |t#4| $))
                                            T)
                                           ((|headReduced?|
                                             ((|Boolean|) |t#4| $))
                                            T)
                                           ((|initiallyReduced?|
                                             ((|Boolean|) |t#4| $))
                                            T)
                                           ((|autoReduced?|
                                             ((|Boolean|) $
                                              (|Mapping| (|Boolean|)
                                               |t#4| (|List| |t#4|))))
                                            T)
                                           ((|stronglyReduced?|
                                             ((|Boolean|) $))
                                            T)
                                           ((|headReduced?|
                                             ((|Boolean|) $))
                                            T)
                                           ((|initiallyReduced?|
                                             ((|Boolean|) $))
                                            T)
                                           ((|reduce|
                                             (|t#4| |t#4| $
                                              (|Mapping| |t#4| |t#4|
                                               |t#4|)
                                              (|Mapping| (|Boolean|)
                                               |t#4| |t#4|)))
                                            T)
                                           ((|rewriteSetWithReduction|
                                             ((|List| |t#4|)
                                              (|List| |t#4|) $
                                              (|Mapping| |t#4| |t#4|
                                               |t#4|)
                                              (|Mapping| (|Boolean|)
                                               |t#4| |t#4|)))
                                            T)
                                           ((|stronglyReduce|
                                             (|t#4| |t#4| $))
                                            T)
                                           ((|headReduce|
                                             (|t#4| |t#4| $))
                                            T)
                                           ((|initiallyReduce|
                                             (|t#4| |t#4| $))
                                            T)
                                           ((|removeZero|
                                             (|t#4| |t#4| $))
                                            T)
                                           ((|collectQuasiMonic| ($ $))
                                            T)
                                           ((|reduceByQuasiMonic|
                                             (|t#4| |t#4| $))
                                            T)
                                           ((|zeroSetSplit|
                                             ((|List| $)
                                              (|List| |t#4|)))
                                            T)
                                           ((|zeroSetSplitIntoTriangularSystems|
                                             ((|List|
                                               (|Record|
                                                (|:| |close| $)
                                                (|:| |open|
                                                 (|List| |t#4|))))
                                              (|List| |t#4|)))
                                            T)
                                           ((|first|
                                             ((|Union| |t#4| "failed")
                                              $))
                                            T)
                                           ((|last|
                                             ((|Union| |t#4| "failed")
                                              $))
                                            T)
                                           ((|rest|
                                             ((|Union| $ "failed") $))
                                            T)
                                           ((|algebraicVariables|
                                             ((|List| |t#3|) $))
                                            T)
                                           ((|algebraic?|
                                             ((|Boolean|) |t#3| $))
                                            T)
                                           ((|select|
                                             ((|Union| |t#4| "failed")
                                              $ |t#3|))
                                            T)
                                           ((|extendIfCan|
                                             ((|Union| $ "failed") $
                                              |t#4|))
                                            T)
                                           ((|extend| ($ $ |t#4|)) T)
                                           ((|coHeight|
                                             ((|NonNegativeInteger|) $))
                                            (|has| |t#3| (|Finite|))))
                                         '((|finiteAggregate| T)
                                           (|shallowlyMutable| T))
                                         '((|NonNegativeInteger|)
                                           (|Boolean|) (|List| |t#3|)
                                           (|List|
                                            (|Record| (|:| |close| $)
                                             (|:| |open|
                                              (|List| |t#4|))))
                                           (|List| |t#4|) (|List| $))
                                         NIL))
                                . #1=(|TriangularSetCategory|))))) . #1#)
        (|setShellEntry| #0# 0
            (LIST '|TriangularSetCategory| (|devaluate| |t#1|)
                  (|devaluate| |t#2|) (|devaluate| |t#3|)
                  (|devaluate| |t#4|))))))) 

(DEFUN |TriangularSetCategory| (&REST #0=#:G1451 &AUX #1=#:G1449)
  (DSETQ #1# #0#)
  (LET (#2=#:G1450)
    (COND
      ((SETQ #2#
             (|assoc| (|devaluateList| #1#) |TriangularSetCategory;AL|))
       (CDR #2#))
      (T (SETQ |TriangularSetCategory;AL|
               (|cons5| (CONS (|devaluateList| #1#)
                              (SETQ #2#
                                    (APPLY #'|TriangularSetCategory;|
                                     #1#)))
                        |TriangularSetCategory;AL|))
         #2#)))) 
