
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
  (PROG (|i| #0=#:G1416)
    (RETURN
      (SEQ (SEQ (LETT |i| 0 |PRIMARR;fill!;$S$;9|)
                (LETT #0# (|maxIndexOfSimpleArray| |x|)
                      |PRIMARR;fill!;$S$;9|)
                G190 (COND ((QSGREATERP |i| #0#) (GO G191)))
                (SEQ (EXIT (|setSimpleArrayEntry| |x| |i| |s|)))
                (LETT |i| (QSADD1 |i|) |PRIMARR;fill!;$S$;9|) (GO G190)
                G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |PrimitiveArray| (#0=#:G1417)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1418)
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
        (LETT $ (|newShell| 38) . #0#)
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
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|)))
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|)))))) . #0#))
        (|haddProp| |$ConstructorCache| '|PrimitiveArray| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|PrimitiveArray| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) |PRIMARR;#;$Nni;1| (|Integer|)
             (0 . |Zero|) |PRIMARR;minIndex;$I;2| |PRIMARR;empty;$;3|
             |PRIMARR;new;NniS$;4| |PRIMARR;qelt;$IS;5|
             |PRIMARR;elt;$IS;6| |PRIMARR;qsetelt!;$I2S;7|
             |PRIMARR;setelt;$I2S;8| (|SingleInteger|) (4 . |Zero|)
             (8 . |Zero|) |PRIMARR;fill!;$S$;9| (|Mapping| 6 6 6)
             (|Boolean|) (|List| 6) (|Equation| 6) (|List| 25)
             (|Mapping| 23 6) (|Mapping| 23 6 6) (|UniversalSegment| 9)
             (|Void|) (|Mapping| 6 6) (|OutputForm|) (|InputForm|)
             (|String|) (|List| $) (|Union| 6 '"failed") (|List| 9))
          '#(~= 12 |swap!| 18 |sorted?| 25 |sort!| 36 |sort| 47 |size?|
             58 |setelt| 64 |select| 78 |sample| 84 |reverse!| 88
             |reverse| 93 |removeDuplicates| 98 |remove| 103 |reduce|
             115 |qsetelt!| 136 |qelt| 143 |position| 149 |parts| 168
             |new| 173 |more?| 179 |minIndex| 185 |min| 190 |merge| 196
             |members| 209 |member?| 214 |maxIndex| 220 |max| 225
             |map!| 231 |map| 237 |less?| 250 |latex| 256 |insert| 261
             |indices| 275 |index?| 280 |hash| 286 |first| 291 |find|
             296 |fill!| 302 |every?| 308 |eval| 314 |eq?| 340 |entry?|
             346 |entries| 352 |empty?| 357 |empty| 362 |elt| 366
             |delete| 385 |count| 397 |copyInto!| 409 |copy| 416
             |convert| 421 |construct| 426 |concat| 431 |coerce| 454
             |any?| 459 >= 465 > 471 = 477 <= 483 < 489 |#| 495)
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
                               (|InnerEvalable| 6 6) (|CoercibleTo| 32)
                               (|ConvertibleTo| 33) (|BasicType|))
                            (|makeByteWordVec2| 37
                                '(0 9 0 10 0 18 0 19 0 7 0 20 2 7 23 0
                                  0 1 3 0 30 0 9 9 1 1 5 23 0 1 2 0 23
                                  28 0 1 1 5 0 0 1 2 0 0 28 0 1 1 5 0 0
                                  1 2 0 0 28 0 1 2 0 23 0 7 1 3 0 6 0
                                  29 6 1 3 0 6 0 9 6 17 2 0 0 27 0 1 0
                                  0 0 1 1 0 0 0 1 1 0 0 0 1 1 7 0 0 1 2
                                  7 0 6 0 1 2 0 0 27 0 1 4 7 6 22 0 6 6
                                  1 3 0 6 22 0 6 1 2 0 6 22 0 1 3 0 6 0
                                  9 6 16 2 0 6 0 9 14 2 7 9 6 0 1 3 7 9
                                  6 0 9 1 2 0 9 27 0 1 1 0 24 0 1 2 0 0
                                  7 6 13 2 0 23 0 7 1 1 6 9 0 11 2 5 0
                                  0 0 1 2 5 0 0 0 1 3 0 0 28 0 0 1 1 0
                                  24 0 1 2 7 23 6 0 1 1 6 9 0 1 2 5 0 0
                                  0 1 2 0 0 31 0 1 3 0 0 22 0 0 1 2 0 0
                                  31 0 1 2 0 23 0 7 1 1 7 34 0 1 3 0 0
                                  0 0 9 1 3 0 0 6 0 9 1 1 0 37 0 1 2 0
                                  23 9 0 1 1 7 18 0 1 1 6 6 0 1 2 0 36
                                  27 0 1 2 0 0 0 6 21 2 0 23 27 0 1 3 9
                                  0 0 24 24 1 2 9 0 0 25 1 3 9 0 0 6 6
                                  1 2 9 0 0 26 1 2 0 23 0 0 1 2 7 23 6
                                  0 1 1 0 24 0 1 1 0 23 0 1 0 0 0 12 2
                                  0 0 0 29 1 2 0 6 0 9 15 3 0 6 0 9 6 1
                                  2 0 0 0 9 1 2 0 0 0 29 1 2 7 7 6 0 1
                                  2 0 7 27 0 1 3 0 0 0 0 9 1 1 0 0 0 1
                                  1 3 33 0 1 1 0 0 24 1 1 0 0 35 1 2 0
                                  0 6 0 1 2 0 0 0 0 1 2 0 0 0 6 1 1 8
                                  32 0 1 2 0 23 27 0 1 2 5 23 0 0 1 2 5
                                  23 0 0 1 2 7 23 0 0 1 2 5 23 0 0 1 2
                                  5 23 0 0 1 1 0 7 0 8)))))
          '|lookupComplete|)) 
