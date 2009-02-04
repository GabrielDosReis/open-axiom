
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |VECTOR;vector;L$;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |VECTOR;convert;$If;2|)) 

(DEFUN |VECTOR;vector;L$;1| (|l| $)
  (SPADCALL |l| (|getShellEntry| $ 8))) 

(DEFUN |VECTOR;convert;$If;2| (|x| $)
  (SPADCALL
      (LIST (SPADCALL '|vector| (|getShellEntry| $ 12))
            (SPADCALL (SPADCALL |x| (|getShellEntry| $ 13))
                      (|getShellEntry| $ 14)))
      (|getShellEntry| $ 16))) 

(DEFUN |Vector| (#0=#:G1408)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1409)
        (RETURN
          (COND
            ((LETT #1#
                   (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                       (HGET |$ConstructorCache| '|Vector|)
                       '|domainEqualList|)
                   |Vector|)
             (|CDRwithIncrement| #1#))
            ('T
             (UNWIND-PROTECT
               (PROG1 (|Vector;| #0#) (LETT #1# T |Vector|))
               (COND ((NOT #1#) (HREM |$ConstructorCache| '|Vector|))))))))))) 

(DEFUN |Vector;| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Vector|))
        (LETT |dv$| (LIST '|Vector| |dv$1|) . #0#)
        (LETT $ (|newShell| 35) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (OR (AND (|HasCategory| |#1|
                                      '(|OrderedSet|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|))))
                                (AND (|HasCategory| |#1|
                                      '(|SetCategory|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|)))))
                            (OR (AND (|HasCategory| |#1|
                                      '(|SetCategory|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|))))
                                (|HasCategory| |#1|
                                    '(|CoercibleTo| (|OutputForm|))))
                            (|HasCategory| |#1|
                                '(|ConvertibleTo| (|InputForm|)))
                            (OR (|HasCategory| |#1| '(|OrderedSet|))
                                (|HasCategory| |#1| '(|SetCategory|)))
                            (|HasCategory| |#1| '(|OrderedSet|))
                            (|HasCategory| (|Integer|) '(|OrderedSet|))
                            (|HasCategory| |#1| '(|SetCategory|))
                            (|HasCategory| |#1| '(|AbelianSemiGroup|))
                            (|HasCategory| |#1| '(|AbelianMonoid|))
                            (|HasCategory| |#1| '(|AbelianGroup|))
                            (|HasCategory| |#1| '(|Monoid|))
                            (|HasCategory| |#1| '(|Ring|))
                            (AND (|HasCategory| |#1|
                                     '(|RadicalCategory|))
                                 (|HasCategory| |#1| '(|Ring|)))
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|)))
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|)))))) . #0#))
        (|haddProp| |$ConstructorCache| '|Vector| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (COND
          ((|testBitVector| |pv$| 3)
           (|setShellEntry| $ 17
               (CONS (|dispatchFunction| |VECTOR;convert;$If;2|) $))))
        $)))) 

(MAKEPROP '|Vector| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|IndexedVector| 6 (NRTEVAL 1))
             (|local| |#1|) (|List| 6) (0 . |construct|)
             |VECTOR;vector;L$;1| (|Symbol|) (|InputForm|)
             (5 . |convert|) (10 . |parts|) (15 . |convert|) (|List| $)
             (20 . |convert|) (25 . |convert|) (|Mapping| 6 6 6)
             (|Boolean|) (|NonNegativeInteger|) (|Equation| 6)
             (|List| 21) (|Integer|) (|Mapping| 19 6)
             (|Mapping| 19 6 6) (|UniversalSegment| 23) (|Void|)
             (|Mapping| 6 6) (|OutputForm|) (|Matrix| 6)
             (|SingleInteger|) (|String|) (|Union| 6 '"failed")
             (|List| 23))
          '#(|vector| 30 |parts| 35 |convert| 40 |construct| 45)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 5
                    '(0 0 0 0 0 0 0 5 0 0 1 4 0 0 1 2 3 4))
                (CONS '#(|VectorCategory&|
                         |OneDimensionalArrayAggregate&|
                         |FiniteLinearAggregate&| |LinearAggregate&|
                         |IndexedAggregate&| |Collection&|
                         |HomogeneousAggregate&| |OrderedSet&|
                         |Aggregate&| |EltableAggregate&| |Evalable&|
                         |SetCategory&| NIL NIL |InnerEvalable&| NIL
                         NIL |BasicType&|)
                      (CONS '#((|VectorCategory| 6)
                               (|OneDimensionalArrayAggregate| 6)
                               (|FiniteLinearAggregate| 6)
                               (|LinearAggregate| 6)
                               (|IndexedAggregate| 23 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 23 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|)
                               (|Eltable| 23 6) (|InnerEvalable| 6 6)
                               (|CoercibleTo| 29) (|ConvertibleTo| 11)
                               (|BasicType|))
                            (|makeByteWordVec2| 17
                                '(1 0 0 7 8 1 11 0 10 12 1 0 7 0 13 1 7
                                  11 0 14 1 11 0 15 16 1 0 11 0 17 1 0
                                  0 7 9 1 0 7 0 13 1 3 11 0 17 1 0 0 7
                                  8)))))
          '|lookupIncomplete|)) 
