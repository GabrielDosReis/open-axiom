
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION ((|%SimpleArray| *) |%Shell|)
                    (|%IntegerSection| 0))
                |PRIMARR;#;$Nni;1|)) 

(PUT '|PRIMARR;#;$Nni;1| '|SPADreplace| '|sizeOfSimpleArray|) 

(DECLAIM (FTYPE (FUNCTION ((|%SimpleArray| *) |%Shell|) |%Integer|)
                |PRIMARR;minIndex;$I;2|)) 

(PUT '|PRIMARR;minIndex;$I;2| '|SPADreplace| '(XLAM (|x|) 0)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%SimpleArray| *))
                |PRIMARR;empty;$;3|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Thing| |%Shell|)
                    (|%SimpleArray| *))
                |PRIMARR;new;NniS$;4|)) 

(DECLAIM (FTYPE (FUNCTION ((|%SimpleArray| *) |%Integer| |%Shell|)
                    |%Thing|)
                |PRIMARR;qelt;$IS;5|)) 

(PUT '|PRIMARR;qelt;$IS;5| '|SPADreplace| '|getSimpleArrayEntry|) 

(DECLAIM (FTYPE (FUNCTION ((|%SimpleArray| *) |%Integer| |%Shell|)
                    |%Thing|)
                |PRIMARR;elt;$IS;6|)) 

(PUT '|PRIMARR;elt;$IS;6| '|SPADreplace| '|getSimpleArrayEntry|) 

(DECLAIM (FTYPE (FUNCTION
                    ((|%SimpleArray| *) |%Integer| |%Thing| |%Shell|)
                    |%Thing|)
                |PRIMARR;qsetelt!;$I2S;7|)) 

(PUT '|PRIMARR;qsetelt!;$I2S;7| '|SPADreplace| '|setSimpleArrayEntry|) 

(DECLAIM (FTYPE (FUNCTION
                    ((|%SimpleArray| *) |%Integer| |%Thing| |%Shell|)
                    |%Thing|)
                |PRIMARR;setelt;$I2S;8|)) 

(PUT '|PRIMARR;setelt;$I2S;8| '|SPADreplace| '|setSimpleArrayEntry|) 

(DECLAIM (FTYPE (FUNCTION ((|%SimpleArray| *) |%Thing| |%Shell|)
                    (|%SimpleArray| *))
                |PRIMARR;fill!;$S$;9|)) 

(DEFUN |PRIMARR;#;$Nni;1| (|x| $)
  (DECLARE (IGNORE $))
  (|sizeOfSimpleArray| |x|)) 

(DEFUN |PRIMARR;minIndex;$I;2| (|x| $) (DECLARE (IGNORE $)) 0) 

(DEFUN |PRIMARR;empty;$;3| ($)
  (|makeSimpleArray| (|getVMType| (|getShellEntry| $ 6)) 0)) 

(DEFUN |PRIMARR;new;NniS$;4| (|n| |x| $)
  (|makeFilledSimpleArray| (|getVMType| (|getShellEntry| $ 6)) |n| |x|)) 

(DEFUN |PRIMARR;qelt;$IS;5| (|x| |i| $)
  (DECLARE (IGNORE $))
  (|getSimpleArrayEntry| |x| |i|)) 

(DEFUN |PRIMARR;elt;$IS;6| (|x| |i| $)
  (DECLARE (IGNORE $))
  (|getSimpleArrayEntry| |x| |i|)) 

(DEFUN |PRIMARR;qsetelt!;$I2S;7| (|x| |i| |s| $)
  (DECLARE (IGNORE $))
  (|setSimpleArrayEntry| |x| |i| |s|)) 

(DEFUN |PRIMARR;setelt;$I2S;8| (|x| |i| |s| $)
  (DECLARE (IGNORE $))
  (|setSimpleArrayEntry| |x| |i| |s|)) 

(DEFUN |PRIMARR;fill!;$S$;9| (|x| |s| $)
  (PROG (|i| #0=#:G1415)
    (RETURN
      (SEQ (SEQ (LETT |i| 0 |PRIMARR;fill!;$S$;9|)
                (LETT #0# (|maxIndexOfSimpleArray| |x|)
                      |PRIMARR;fill!;$S$;9|)
                G190 (COND ((QSGREATERP |i| #0#) (GO G191)))
                (SEQ (EXIT (|setSimpleArrayEntry| |x| |i| |s|)))
                (LETT |i| (QSADD1 |i|) |PRIMARR;fill!;$S$;9|) (GO G190)
                G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |PrimitiveArray| (#0=#:G1416)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1417)
        (RETURN
          (COND
            ((LETT #1#
                   (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                       (HGET |$ConstructorCache| '|PrimitiveArray|)
                       '|domainEqualList|)
                   |PrimitiveArray|)
             (|CDRwithIncrement| #1#))
            ('T
             (UNWIND-PROTECT
               (PROG1 (|PrimitiveArray;| #0#)
                 (LETT #1# T |PrimitiveArray|))
               (COND
                 ((NOT #1#)
                  (HREM |$ConstructorCache| '|PrimitiveArray|))))))))))) 

(DEFUN |PrimitiveArray;| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|PrimitiveArray|))
        (LETT |dv$| (LIST '|PrimitiveArray| |dv$1|) . #0#)
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
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|))))
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|))))) . #0#))
        (|haddProp| |$ConstructorCache| '|PrimitiveArray| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|PrimitiveArray| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) |PRIMARR;#;$Nni;1| (|Integer|)
             |PRIMARR;minIndex;$I;2| |PRIMARR;empty;$;3|
             |PRIMARR;new;NniS$;4| |PRIMARR;qelt;$IS;5|
             |PRIMARR;elt;$IS;6| |PRIMARR;qsetelt!;$I2S;7|
             |PRIMARR;setelt;$I2S;8| |PRIMARR;fill!;$S$;9|
             (|Mapping| 6 6 6) (|Boolean|) (|List| 6) (|Equation| 6)
             (|List| 21) (|Mapping| 19 6) (|Mapping| 19 6 6)
             (|UniversalSegment| 9) (|Void|) (|Mapping| 6 6)
             (|OutputForm|) (|InputForm|) (|String|) (|SingleInteger|)
             (|List| $) (|Union| 6 '"failed") (|List| 9))
          '#(~= 0 |swap!| 6 |sorted?| 13 |sort!| 24 |sort| 35 |size?|
             46 |setelt| 52 |select| 66 |sample| 72 |reverse!| 76
             |reverse| 81 |removeDuplicates| 86 |remove| 91 |reduce|
             103 |qsetelt!| 124 |qelt| 131 |position| 137 |parts| 156
             |new| 161 |more?| 167 |minIndex| 173 |min| 178 |merge| 184
             |members| 197 |member?| 202 |maxIndex| 208 |max| 213
             |map!| 219 |map| 225 |less?| 238 |latex| 244 |insert| 249
             |indices| 263 |index?| 268 |hash| 274 |first| 279 |find|
             284 |fill!| 290 |every?| 296 |eval| 302 |eq?| 328 |entry?|
             334 |entries| 340 |empty?| 345 |empty| 350 |elt| 354
             |delete| 373 |count| 385 |copyInto!| 397 |copy| 404
             |convert| 409 |construct| 414 |concat| 419 |coerce| 442
             |any?| 447 >= 453 > 459 = 465 <= 471 < 477 |#| 483)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 5
                    '(0 0 0 0 0 0 5 0 0 1 4 0 0 1 2 3 4))
                (CONS '#(|OneDimensionalArrayAggregate&|
                         |FiniteLinearAggregate&| |LinearAggregate&|
                         |IndexedAggregate&| |Collection&|
                         |HomogeneousAggregate&| |OrderedSet&|
                         |Aggregate&| |EltableAggregate&| |Evalable&|
                         |SetCategory&| NIL NIL |InnerEvalable&| NIL
                         NIL |BasicType&|)
                      (CONS '#((|OneDimensionalArrayAggregate| 6)
                               (|FiniteLinearAggregate| 6)
                               (|LinearAggregate| 6)
                               (|IndexedAggregate| 9 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 9 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|) (|Eltable| 9 6)
                               (|InnerEvalable| 6 6) (|CoercibleTo| 28)
                               (|ConvertibleTo| 29) (|BasicType|))
                            (|makeByteWordVec2| 34
                                '(2 7 19 0 0 1 3 0 26 0 9 9 1 1 5 19 0
                                  1 2 0 19 24 0 1 1 5 0 0 1 2 0 0 24 0
                                  1 1 5 0 0 1 2 0 0 24 0 1 2 0 19 0 7 1
                                  3 0 6 0 25 6 1 3 0 6 0 9 6 16 2 0 0
                                  23 0 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1
                                  7 0 0 1 2 7 0 6 0 1 2 0 0 23 0 1 4 7
                                  6 18 0 6 6 1 3 0 6 18 0 6 1 2 0 6 18
                                  0 1 3 0 6 0 9 6 15 2 0 6 0 9 13 2 7 9
                                  6 0 1 3 7 9 6 0 9 1 2 0 9 23 0 1 1 0
                                  20 0 1 2 0 0 7 6 12 2 0 19 0 7 1 1 6
                                  9 0 10 2 5 0 0 0 1 2 5 0 0 0 1 3 0 0
                                  24 0 0 1 1 0 20 0 1 2 7 19 6 0 1 1 6
                                  9 0 1 2 5 0 0 0 1 2 0 0 27 0 1 3 0 0
                                  18 0 0 1 2 0 0 27 0 1 2 0 19 0 7 1 1
                                  7 30 0 1 3 0 0 0 0 9 1 3 0 0 6 0 9 1
                                  1 0 34 0 1 2 0 19 9 0 1 1 7 31 0 1 1
                                  6 6 0 1 2 0 33 23 0 1 2 0 0 0 6 17 2
                                  0 19 23 0 1 3 8 0 0 20 20 1 2 8 0 0
                                  21 1 3 8 0 0 6 6 1 2 8 0 0 22 1 2 0
                                  19 0 0 1 2 7 19 6 0 1 1 0 20 0 1 1 0
                                  19 0 1 0 0 0 11 2 0 0 0 25 1 2 0 6 0
                                  9 14 3 0 6 0 9 6 1 2 0 0 0 9 1 2 0 0
                                  0 25 1 2 7 7 6 0 1 2 0 7 23 0 1 3 0 0
                                  0 0 9 1 1 0 0 0 1 1 3 29 0 1 1 0 0 20
                                  1 1 0 0 32 1 2 0 0 6 0 1 2 0 0 0 0 1
                                  2 0 0 0 6 1 1 9 28 0 1 2 0 19 23 0 1
                                  2 5 19 0 0 1 2 5 19 0 0 1 2 7 19 0 0
                                  1 2 5 19 0 0 1 2 5 19 0 0 1 1 0 7 0
                                  8)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|PrimitiveArray| '|isFunctor|
             '(((~= ((|Boolean|) $ $)) (|has| |#1| (|SetCategory|))
                (ELT $ NIL))
               ((= ((|Boolean|) $ $)) (|has| |#1| (|SetCategory|))
                (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $))
                (|has| |#1| (|CoercibleTo| (|OutputForm|)))
                (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|latex| ((|String|) $)) (|has| |#1| (|SetCategory|))
                (ELT $ NIL))
               ((< ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((> ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((<= ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|max| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|min| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|sort!| ($ $))
                (AND (|has| $ (ATTRIBUTE |shallowlyMutable|))
                     (|has| |#1| (|OrderedSet|)))
                (ELT $ NIL))
               ((|sort!| ($ (|Mapping| (|Boolean|) |#1| |#1|) $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|reverse!| ($ $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|copyInto!| ($ $ $ (|Integer|)))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|sorted?| ((|Boolean|) $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|sort| ($ $)) (|has| |#1| (|OrderedSet|)) (ELT $ NIL))
               ((|merge| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|position| ((|Integer|) |#1| $ (|Integer|)))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|position| ((|Integer|) |#1| $))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|position|
                    ((|Integer|) (|Mapping| (|Boolean|) |#1|) $))
                T (ELT $ NIL))
               ((|sorted?|
                    ((|Boolean|) (|Mapping| (|Boolean|) |#1| |#1|) $))
                T (ELT $ NIL))
               ((|sort| ($ (|Mapping| (|Boolean|) |#1| |#1|) $)) T
                (ELT $ NIL))
               ((|reverse| ($ $)) T (ELT $ NIL))
               ((|merge| ($ (|Mapping| (|Boolean|) |#1| |#1|) $ $)) T
                (ELT $ NIL))
               ((|setelt|
                    (|#1| $ (|UniversalSegment| (|Integer|)) |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|insert| ($ $ $ (|Integer|))) T (ELT $ NIL))
               ((|insert| ($ |#1| $ (|Integer|))) T (ELT $ NIL))
               ((|delete| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ NIL))
               ((|delete| ($ $ (|Integer|))) T (ELT $ NIL))
               ((|elt| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ NIL))
               ((|map| ($ (|Mapping| |#1| |#1| |#1|) $ $)) T
                (ELT $ NIL))
               ((|concat| ($ (|List| $))) T (ELT $ NIL))
               ((|concat| ($ $ $)) T (ELT $ NIL))
               ((|concat| ($ |#1| $)) T (ELT $ NIL))
               ((|concat| ($ $ |#1|)) T (ELT $ NIL))
               ((|new| ($ (|NonNegativeInteger|) |#1|)) T (ELT $ 12))
               ((|construct| ($ (|List| |#1|))) T (ELT $ NIL))
               ((|find| ((|Union| |#1| "failed")
                         (|Mapping| (|Boolean|) |#1|) $))
                T (ELT $ NIL))
               ((|reduce| (|#1| (|Mapping| |#1| |#1| |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|reduce| (|#1| (|Mapping| |#1| |#1| |#1|) $ |#1|))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|remove| ($ (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|select| ($ (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|reduce|
                    (|#1| (|Mapping| |#1| |#1| |#1|) $ |#1| |#1|))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|remove| ($ |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|removeDuplicates| ($ $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|convert| ((|InputForm|) $))
                (|has| |#1| (|ConvertibleTo| (|InputForm|)))
                (ELT $ NIL))
               ((|swap!| ((|Void|) $ (|Integer|) (|Integer|)))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|fill!| ($ $ |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 17))
               ((|first| (|#1| $)) (|has| (|Integer|) (|OrderedSet|))
                (ELT $ NIL))
               ((|minIndex| ((|Integer|) $))
                (|has| (|Integer|) (|OrderedSet|)) (ELT $ 10))
               ((|maxIndex| ((|Integer|) $))
                (|has| (|Integer|) (|OrderedSet|)) (ELT $ NIL))
               ((|entry?| ((|Boolean|) |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|indices| ((|List| (|Integer|)) $)) T (ELT $ NIL))
               ((|index?| ((|Boolean|) (|Integer|) $)) T (ELT $ NIL))
               ((|entries| ((|List| |#1|) $)) T (ELT $ NIL))
               ((|elt| (|#1| $ (|Integer|))) T (ELT $ 14))
               ((|elt| (|#1| $ (|Integer|) |#1|)) T (ELT $ NIL))
               ((|qelt| (|#1| $ (|Integer|))) T (ELT $ 13))
               ((|setelt| (|#1| $ (|Integer|) |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 16))
               ((|qsetelt!| (|#1| $ (|Integer|) |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 15))
               ((|eval| ($ $ (|List| |#1|) (|List| |#1|)))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|eval| ($ $ |#1| |#1|))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|eval| ($ $ (|Equation| |#1|)))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|eval| ($ $ (|List| (|Equation| |#1|))))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|member?| ((|Boolean|) |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|count| ((|NonNegativeInteger|) |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|members| ((|List| |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|parts| ((|List| |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|count| ((|NonNegativeInteger|)
                          (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|every?| ((|Boolean|) (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|any?| ((|Boolean|) (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|map!| ($ (|Mapping| |#1| |#1|) $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|map| ($ (|Mapping| |#1| |#1|) $)) T (ELT $ NIL))
               ((|#| ((|NonNegativeInteger|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ 8))
               ((|sample| ($)) T (CONST $ NIL))
               ((|size?| ((|Boolean|) $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|more?| ((|Boolean|) $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|less?| ((|Boolean|) $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|empty?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|empty| ($)) T (ELT $ 11))
               ((|copy| ($ $)) T (ELT $ NIL))
               ((|eq?| ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|PrimitiveArray| '(|PrimitiveArray| |#1|)
                 '((|OneDimensionalArrayAggregate| |#1|) (|Type|)) T
                 '|PrimitiveArray|
                 (|put| '|PrimitiveArray| '|mode|
                        '(|Mapping|
                             (|OneDimensionalArrayAggregate| |#1|)
                             (|Type|))
                        |$CategoryFrame|)))) 
