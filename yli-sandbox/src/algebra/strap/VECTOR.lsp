
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |VECTOR;vector;L$;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |VECTOR;maxIndex;$I;2|)) 

(PUT '|VECTOR;maxIndex;$I;2| '|SPADreplace| '|sizeOfSimpleArray|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |VECTOR;convert;$If;3|)) 

(DEFUN |VECTOR;vector;L$;1| (|l| $)
  (SPADCALL |l| (|getShellEntry| $ 10))) 

(DEFUN |VECTOR;maxIndex;$I;2| (|x| $)
  (DECLARE (IGNORE $))
  (|sizeOfSimpleArray| |x|)) 

(DEFUN |VECTOR;convert;$If;3| (|x| $)
  (SPADCALL
      (LIST (SPADCALL '|vector| (|getShellEntry| $ 15))
            (SPADCALL (SPADCALL |x| (|getShellEntry| $ 16))
                      (|getShellEntry| $ 17)))
      (|getShellEntry| $ 19))) 

(DEFUN |Vector| (#0=#:G1407)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1408)
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
        (LETT $ (|newShell| 36) . #0#)
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
           (|setShellEntry| $ 20
               (CONS (|dispatchFunction| |VECTOR;convert;$If;3|) $))))
        $)))) 

(MAKEPROP '|Vector| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|IndexedVector| 6 (NRTEVAL 1))
             (|local| |#1|) (|Integer|) (0 . |One|) (|List| 6)
             (4 . |construct|) |VECTOR;vector;L$;1|
             |VECTOR;maxIndex;$I;2| (|OutputForm|) (|InputForm|)
             (9 . |convert|) (14 . |parts|) (19 . |convert|) (|List| $)
             (24 . |convert|) (29 . |convert|) (|Mapping| 6 6 6)
             (|Boolean|) (|NonNegativeInteger|) (|Equation| 6)
             (|List| 24) (|Mapping| 22 6) (|Mapping| 22 6 6)
             (|UniversalSegment| 7) (|Void|) (|Mapping| 6 6)
             (|Matrix| 6) (|SingleInteger|) (|String|)
             (|Union| 6 '"failed") (|List| 7))
          '#(|vector| 34 |parts| 39 |maxIndex| 44 |convert| 49
             |construct| 54)
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
                               (|IndexedAggregate| 7 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 7 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|) (|Eltable| 7 6)
                               (|InnerEvalable| 6 6) (|CoercibleTo| 13)
                               (|ConvertibleTo| 14) (|BasicType|))
                            (|makeByteWordVec2| 20
                                '(0 7 0 8 1 0 0 9 10 1 14 0 13 15 1 0 9
                                  0 16 1 9 14 0 17 1 14 0 18 19 1 0 14
                                  0 20 1 0 0 9 11 1 0 9 0 16 1 6 7 0 12
                                  1 3 14 0 20 1 0 0 9 10)))))
          '|lookupIncomplete|)) 
