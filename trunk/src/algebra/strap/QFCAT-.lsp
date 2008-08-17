
(/VERSIONCHECK 2) 

(DEFUN |QFCAT-;numerator;2A;1| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8)) (|getShellEntry| $ 9))) 

(DEFUN |QFCAT-;denominator;2A;2| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
      (|getShellEntry| $ 9))) 

(DEFUN |QFCAT-;init;A;3| ($)
  (SPADCALL (|spadConstant| $ 13) (|spadConstant| $ 14)
      (|getShellEntry| $ 15))) 

(DEFUN |QFCAT-;nextItem;AU;4| (|n| $)
  (PROG (|m|)
    (RETURN
      (SEQ (LETT |m|
                 (SPADCALL (SPADCALL |n| (|getShellEntry| $ 8))
                     (|getShellEntry| $ 18))
                 |QFCAT-;nextItem;AU;4|)
           (EXIT (COND
                   ((QEQCAR |m| 1)
                    (|error| "We seem to have a Fraction of a finite object"))
                   ('T
                    (CONS 0
                          (SPADCALL (QCDR |m|) (|spadConstant| $ 14)
                              (|getShellEntry| $ 15)))))))))) 

(DEFUN |QFCAT-;map;M2A;5| (|fn| |x| $)
  (SPADCALL (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8)) |fn|)
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11)) |fn|)
      (|getShellEntry| $ 15))) 

(DEFUN |QFCAT-;reducedSystem;MM;6| (|m| $)
  (SPADCALL |m| (|getShellEntry| $ 26))) 

(DEFUN |QFCAT-;characteristic;Nni;7| ($)
  (SPADCALL (|getShellEntry| $ 30))) 

(DEFUN |QFCAT-;differentiate;AMA;8| (|x| |deriv| $)
  (PROG (|n| |d|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |x| (|getShellEntry| $ 8))
                 |QFCAT-;differentiate;AMA;8|)
           (LETT |d| (SPADCALL |x| (|getShellEntry| $ 11))
                 |QFCAT-;differentiate;AMA;8|)
           (EXIT (SPADCALL
                     (SPADCALL
                         (SPADCALL (SPADCALL |n| |deriv|) |d|
                             (|getShellEntry| $ 32))
                         (SPADCALL |n| (SPADCALL |d| |deriv|)
                             (|getShellEntry| $ 32))
                         (|getShellEntry| $ 33))
                     (SPADCALL |d| 2 (|getShellEntry| $ 35))
                     (|getShellEntry| $ 15))))))) 

(DEFUN |QFCAT-;convert;AIf;9| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (|getShellEntry| $ 38))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
          (|getShellEntry| $ 38))
      (|getShellEntry| $ 39))) 

(DEFUN |QFCAT-;convert;AF;10| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (|getShellEntry| $ 42))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
          (|getShellEntry| $ 42))
      (|getShellEntry| $ 43))) 

(DEFUN |QFCAT-;convert;ADf;11| (|x| $)
  (/ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
         (|getShellEntry| $ 46))
     (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
         (|getShellEntry| $ 46)))) 

(DEFUN |QFCAT-;<;2AB;12| (|x| |y| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (SPADCALL |y| (|getShellEntry| $ 11)) (|getShellEntry| $ 32))
      (SPADCALL (SPADCALL |y| (|getShellEntry| $ 8))
          (SPADCALL |x| (|getShellEntry| $ 11)) (|getShellEntry| $ 32))
      (|getShellEntry| $ 49))) 

(DEFUN |QFCAT-;<;2AB;13| (|x| |y| $)
  (PROG (|#G19| |#G20| |#G21| |#G22|)
    (RETURN
      (SEQ (COND
             ((SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
                  (|spadConstant| $ 51) (|getShellEntry| $ 49))
              (PROGN
                (LETT |#G19| |y| |QFCAT-;<;2AB;13|)
                (LETT |#G20| |x| |QFCAT-;<;2AB;13|)
                (LETT |x| |#G19| |QFCAT-;<;2AB;13|)
                (LETT |y| |#G20| |QFCAT-;<;2AB;13|))))
           (COND
             ((SPADCALL (SPADCALL |y| (|getShellEntry| $ 11))
                  (|spadConstant| $ 51) (|getShellEntry| $ 49))
              (PROGN
                (LETT |#G21| |y| |QFCAT-;<;2AB;13|)
                (LETT |#G22| |x| |QFCAT-;<;2AB;13|)
                (LETT |x| |#G21| |QFCAT-;<;2AB;13|)
                (LETT |y| |#G22| |QFCAT-;<;2AB;13|))))
           (EXIT (SPADCALL
                     (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
                         (SPADCALL |y| (|getShellEntry| $ 11))
                         (|getShellEntry| $ 32))
                     (SPADCALL (SPADCALL |y| (|getShellEntry| $ 8))
                         (SPADCALL |x| (|getShellEntry| $ 11))
                         (|getShellEntry| $ 32))
                     (|getShellEntry| $ 49))))))) 

(DEFUN |QFCAT-;<;2AB;14| (|x| |y| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (SPADCALL |y| (|getShellEntry| $ 11)) (|getShellEntry| $ 32))
      (SPADCALL (SPADCALL |y| (|getShellEntry| $ 8))
          (SPADCALL |x| (|getShellEntry| $ 11)) (|getShellEntry| $ 32))
      (|getShellEntry| $ 49))) 

(DEFUN |QFCAT-;fractionPart;2A;15| (|x| $)
  (SPADCALL |x|
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 52))
          (|getShellEntry| $ 9))
      (|getShellEntry| $ 53))) 

(DEFUN |QFCAT-;coerce;SA;16| (|s| $)
  (SPADCALL (SPADCALL |s| (|getShellEntry| $ 56))
      (|getShellEntry| $ 9))) 

(DEFUN |QFCAT-;retract;AS;17| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 58))
      (|getShellEntry| $ 59))) 

(DEFUN |QFCAT-;retractIfCan;AU;18| (|x| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (SPADCALL |x| (|getShellEntry| $ 62))
                 |QFCAT-;retractIfCan;AU;18|)
           (EXIT (COND
                   ((QEQCAR |r| 1) (CONS 1 "failed"))
                   ('T (SPADCALL (QCDR |r|) (|getShellEntry| $ 64))))))))) 

(DEFUN |QFCAT-;convert;AP;19| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (|getShellEntry| $ 68))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
          (|getShellEntry| $ 68))
      (|getShellEntry| $ 69))) 

(DEFUN |QFCAT-;patternMatch;AP2Pmr;20| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 73))) 

(DEFUN |QFCAT-;convert;AP;21| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (|getShellEntry| $ 77))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
          (|getShellEntry| $ 77))
      (|getShellEntry| $ 78))) 

(DEFUN |QFCAT-;patternMatch;AP2Pmr;22| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 82))) 

(DEFUN |QFCAT-;coerce;FA;23| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 86))
          (|getShellEntry| $ 87))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 88))
          (|getShellEntry| $ 87))
      (|getShellEntry| $ 89))) 

(DEFUN |QFCAT-;retract;AI;24| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 58))
      (|getShellEntry| $ 91))) 

(DEFUN |QFCAT-;retractIfCan;AU;25| (|x| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |x| (|getShellEntry| $ 62))
                 |QFCAT-;retractIfCan;AU;25|)
           (EXIT (COND
                   ((QEQCAR |u| 1) (CONS 1 "failed"))
                   ('T (SPADCALL (QCDR |u|) (|getShellEntry| $ 94))))))))) 

(DEFUN |QFCAT-;random;A;26| ($)
  (PROG (|d|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (SPADCALL
                             (LETT |d|
                                   (SPADCALL (|getShellEntry| $ 96))
                                   |QFCAT-;random;A;26|)
                             (|getShellEntry| $ 97)))
                   (GO G191)))
                (SEQ (EXIT |d|)) NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL (SPADCALL (|getShellEntry| $ 96)) |d|
                     (|getShellEntry| $ 15))))))) 

(DEFUN |QFCAT-;reducedSystem;MVR;27| (|m| |v| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n|
                 (SPADCALL
                     (SPADCALL (SPADCALL |v| (|getShellEntry| $ 100))
                         |m| (|getShellEntry| $ 101))
                     (|getShellEntry| $ 102))
                 |QFCAT-;reducedSystem;MVR;27|)
           (EXIT (CONS (SPADCALL |n|
                           (SPADCALL |n| (|getShellEntry| $ 103))
                           (SPADCALL |n| (|getShellEntry| $ 104))
                           (+ 1 (SPADCALL |n| (|getShellEntry| $ 105)))
                           (SPADCALL |n| (|getShellEntry| $ 106))
                           (|getShellEntry| $ 107))
                       (SPADCALL |n|
                           (SPADCALL |n| (|getShellEntry| $ 105))
                           (|getShellEntry| $ 109)))))))) 

(DEFUN |QuotientFieldCategory&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|)
              . #0=(|QuotientFieldCategory&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$|
              (LIST '|QuotientFieldCategory&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 120) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#2|
                                '(|PolynomialFactorizationExplicit|))
                            (|HasCategory| |#2|
                                '(|IntegerNumberSystem|))
                            (|HasCategory| |#2| '(|EuclideanDomain|))
                            (|HasCategory| |#2|
                                '(|RetractableTo| (|Symbol|)))
                            (|HasCategory| |#2|
                                '(|CharacteristicNonZero|))
                            (|HasCategory| |#2|
                                '(|CharacteristicZero|))
                            (|HasCategory| |#2|
                                '(|ConvertibleTo| (|InputForm|)))
                            (|HasCategory| |#2| '(|RealConstant|))
                            (|HasCategory| |#2|
                                '(|OrderedIntegralDomain|))
                            (|HasCategory| |#2| '(|OrderedSet|))
                            (|HasCategory| |#2|
                                '(|RetractableTo| (|Integer|)))
                            (|HasCategory| |#2| '(|StepThrough|)))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 12)
           (PROGN
             (|setShellEntry| $ 16
                 (CONS (|dispatchFunction| |QFCAT-;init;A;3|) $))
             (|setShellEntry| $ 20
                 (CONS (|dispatchFunction| |QFCAT-;nextItem;AU;4|) $)))))
        (COND
          ((|testBitVector| |pv$| 7)
           (|setShellEntry| $ 40
               (CONS (|dispatchFunction| |QFCAT-;convert;AIf;9|) $))))
        (COND
          ((|testBitVector| |pv$| 8)
           (PROGN
             (|setShellEntry| $ 44
                 (CONS (|dispatchFunction| |QFCAT-;convert;AF;10|) $))
             (|setShellEntry| $ 47
                 (CONS (|dispatchFunction| |QFCAT-;convert;ADf;11|) $)))))
        (COND
          ((|testBitVector| |pv$| 9)
           (COND
             ((|HasAttribute| |#2| '|canonicalUnitNormal|)
              (|setShellEntry| $ 50
                  (CONS (|dispatchFunction| |QFCAT-;<;2AB;12|) $)))
             ('T
              (|setShellEntry| $ 50
                  (CONS (|dispatchFunction| |QFCAT-;<;2AB;13|) $)))))
          ((|testBitVector| |pv$| 10)
           (|setShellEntry| $ 50
               (CONS (|dispatchFunction| |QFCAT-;<;2AB;14|) $))))
        (COND
          ((|testBitVector| |pv$| 3)
           (|setShellEntry| $ 54
               (CONS (|dispatchFunction| |QFCAT-;fractionPart;2A;15|)
                     $))))
        (COND
          ((|testBitVector| |pv$| 4)
           (PROGN
             (|setShellEntry| $ 57
                 (CONS (|dispatchFunction| |QFCAT-;coerce;SA;16|) $))
             (|setShellEntry| $ 60
                 (CONS (|dispatchFunction| |QFCAT-;retract;AS;17|) $))
             (|setShellEntry| $ 65
                 (CONS (|dispatchFunction| |QFCAT-;retractIfCan;AU;18|)
                       $)))))
        (COND
          ((|HasCategory| |#2|
               '(|ConvertibleTo| (|Pattern| (|Integer|))))
           (PROGN
             (|setShellEntry| $ 70
                 (CONS (|dispatchFunction| |QFCAT-;convert;AP;19|) $))
             (COND
               ((|HasCategory| |#2| '(|PatternMatchable| (|Integer|)))
                (|setShellEntry| $ 75
                    (CONS (|dispatchFunction|
                              |QFCAT-;patternMatch;AP2Pmr;20|)
                          $)))))))
        (COND
          ((|HasCategory| |#2|
               '(|ConvertibleTo| (|Pattern| (|Float|))))
           (PROGN
             (|setShellEntry| $ 79
                 (CONS (|dispatchFunction| |QFCAT-;convert;AP;21|) $))
             (COND
               ((|HasCategory| |#2| '(|PatternMatchable| (|Float|)))
                (|setShellEntry| $ 84
                    (CONS (|dispatchFunction|
                              |QFCAT-;patternMatch;AP2Pmr;22|)
                          $)))))))
        (COND
          ((|testBitVector| |pv$| 11)
           (PROGN
             (|setShellEntry| $ 90
                 (CONS (|dispatchFunction| |QFCAT-;coerce;FA;23|) $))
             (COND
               ((|domainEqual| |#2| (|Integer|)))
               ('T
                (PROGN
                  (|setShellEntry| $ 92
                      (CONS (|dispatchFunction| |QFCAT-;retract;AI;24|)
                            $))
                  (|setShellEntry| $ 95
                      (CONS (|dispatchFunction|
                                |QFCAT-;retractIfCan;AU;25|)
                            $))))))))
        (COND
          ((|testBitVector| |pv$| 2)
           (|setShellEntry| $ 98
               (CONS (|dispatchFunction| |QFCAT-;random;A;26|) $))))
        $)))) 

(MAKEPROP '|QuotientFieldCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (0 . |numer|) (5 . |coerce|) |QFCAT-;numerator;2A;1|
             (10 . |denom|) |QFCAT-;denominator;2A;2| (15 . |init|)
             (19 . |One|) (23 . /) (29 . |init|) (|Union| $ '"failed")
             (33 . |nextItem|) (38 . |One|) (42 . |nextItem|)
             (|Mapping| 7 7) |QFCAT-;map;M2A;5| (|Matrix| 7)
             (|Matrix| 6) (|MatrixCommonDenominator| 7 6)
             (47 . |clearDenominator|) (|Matrix| $)
             |QFCAT-;reducedSystem;MM;6| (|NonNegativeInteger|)
             (52 . |characteristic|) |QFCAT-;characteristic;Nni;7|
             (56 . *) (62 . -) (|PositiveInteger|) (68 . **)
             |QFCAT-;differentiate;AMA;8| (|InputForm|)
             (74 . |convert|) (79 . /) (85 . |convert|) (|Float|)
             (90 . |convert|) (95 . /) (101 . |convert|)
             (|DoubleFloat|) (106 . |convert|) (111 . |convert|)
             (|Boolean|) (116 . <) (122 . <) (128 . |Zero|)
             (132 . |wholePart|) (137 . -) (143 . |fractionPart|)
             (|Symbol|) (148 . |coerce|) (153 . |coerce|)
             (158 . |retract|) (163 . |retract|) (168 . |retract|)
             (|Union| 7 '"failed") (173 . |retractIfCan|)
             (|Union| 55 '"failed") (178 . |retractIfCan|)
             (183 . |retractIfCan|) (|Integer|) (|Pattern| 66)
             (188 . |convert|) (193 . /) (199 . |convert|)
             (|PatternMatchResult| 66 6)
             (|PatternMatchQuotientFieldCategory| 66 7 6)
             (204 . |patternMatch|) (|PatternMatchResult| 66 $)
             (211 . |patternMatch|) (|Pattern| 41) (218 . |convert|)
             (223 . /) (229 . |convert|) (|PatternMatchResult| 41 6)
             (|PatternMatchQuotientFieldCategory| 41 7 6)
             (234 . |patternMatch|) (|PatternMatchResult| 41 $)
             (241 . |patternMatch|) (|Fraction| 66) (248 . |numer|)
             (253 . |coerce|) (258 . |denom|) (263 . /)
             (269 . |coerce|) (274 . |retract|) (279 . |retract|)
             (|Union| 66 '"failed") (284 . |retractIfCan|)
             (289 . |retractIfCan|) (294 . |random|) (298 . |zero?|)
             (303 . |random|) (|Vector| 6) (307 . |coerce|)
             (312 . |horizConcat|) (318 . |reducedSystem|)
             (323 . |minRowIndex|) (328 . |maxRowIndex|)
             (333 . |minColIndex|) (338 . |maxColIndex|)
             (343 . |subMatrix|) (|Vector| 7) (352 . |column|)
             (|Record| (|:| |mat| 23) (|:| |vec| 108)) (|Vector| $)
             |QFCAT-;reducedSystem;MVR;27| (|Union| 85 '"failed")
             (|Matrix| 66) (|Vector| 66)
             (|Record| (|:| |mat| 114) (|:| |vec| 115)) (|List| 55)
             (|List| 29) (|OutputForm|))
          '#(|retractIfCan| 358 |retract| 368 |reducedSystem| 378
             |random| 389 |patternMatch| 393 |numerator| 407 |nextItem|
             412 |map| 417 |init| 423 |fractionPart| 427
             |differentiate| 432 |denominator| 438 |convert| 443
             |coerce| 468 |characteristic| 478 < 482)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 112
                                '(1 6 7 0 8 1 6 0 7 9 1 6 7 0 11 0 7 0
                                  13 0 7 0 14 2 6 0 7 7 15 0 0 0 16 1 7
                                  17 0 18 0 6 0 19 1 0 17 0 20 1 25 23
                                  24 26 0 7 29 30 2 7 0 0 0 32 2 7 0 0
                                  0 33 2 7 0 0 34 35 1 7 37 0 38 2 37 0
                                  0 0 39 1 0 37 0 40 1 7 41 0 42 2 41 0
                                  0 0 43 1 0 41 0 44 1 7 45 0 46 1 0 45
                                  0 47 2 7 48 0 0 49 2 0 48 0 0 50 0 7
                                  0 51 1 6 7 0 52 2 6 0 0 0 53 1 0 0 0
                                  54 1 7 0 55 56 1 0 0 55 57 1 6 7 0 58
                                  1 7 55 0 59 1 0 55 0 60 1 6 61 0 62 1
                                  7 63 0 64 1 0 63 0 65 1 7 67 0 68 2
                                  67 0 0 0 69 1 0 67 0 70 3 72 71 6 67
                                  71 73 3 0 74 0 67 74 75 1 7 76 0 77 2
                                  76 0 0 0 78 1 0 76 0 79 3 81 80 6 76
                                  80 82 3 0 83 0 76 83 84 1 85 66 0 86
                                  1 6 0 66 87 1 85 66 0 88 2 6 0 0 0 89
                                  1 0 0 85 90 1 7 66 0 91 1 0 66 0 92 1
                                  7 93 0 94 1 0 93 0 95 0 7 0 96 1 7 48
                                  0 97 0 0 0 98 1 24 0 99 100 2 24 0 0
                                  0 101 1 6 23 27 102 1 23 66 0 103 1
                                  23 66 0 104 1 23 66 0 105 1 23 66 0
                                  106 5 23 0 0 66 66 66 66 107 2 23 108
                                  0 66 109 1 0 93 0 95 1 0 63 0 65 1 0
                                  66 0 92 1 0 55 0 60 2 0 110 27 111
                                  112 1 0 23 27 28 0 0 0 98 3 0 83 0 76
                                  83 84 3 0 74 0 67 74 75 1 0 0 0 10 1
                                  0 17 0 20 2 0 0 21 0 22 0 0 0 16 1 0
                                  0 0 54 2 0 0 0 21 36 1 0 0 0 12 1 0
                                  45 0 47 1 0 37 0 40 1 0 41 0 44 1 0
                                  67 0 70 1 0 76 0 79 1 0 0 55 57 1 0 0
                                  85 90 0 0 29 31 2 0 48 0 0 50)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|QuotientFieldCategory&| '|isFunctor|
             '(((< ((|Boolean|) $ $)) T (ELT $ 50))
               ((|init| ($)) T (ELT $ 16))
               ((|nextItem| ((|Union| $ "failed") $)) T (ELT $ 20))
               ((|retract| ((|Integer|) $)) T (ELT $ 92))
               ((|retractIfCan| ((|Union| (|Integer|) "failed") $)) T
                (ELT $ 95))
               ((|retract| ((|Fraction| (|Integer|)) $)) T (ELT $ NIL))
               ((|retractIfCan|
                    ((|Union| (|Fraction| (|Integer|)) "failed") $))
                T (ELT $ NIL))
               ((|convert| ((|DoubleFloat|) $)) T (ELT $ 47))
               ((|convert| ((|Float|) $)) T (ELT $ 44))
               ((|convert| ((|InputForm|) $)) T (ELT $ 40))
               ((|retract| ((|Symbol|) $)) T (ELT $ 60))
               ((|retractIfCan| ((|Union| (|Symbol|) "failed") $)) T
                (ELT $ 65))
               ((|coerce| ($ (|Symbol|))) T (ELT $ 57))
               ((|random| ($)) T (ELT $ 98))
               ((|fractionPart| ($ $)) T (ELT $ 54))
               ((|denominator| ($ $)) T (ELT $ 12))
               ((|numerator| ($ $)) T (ELT $ 10))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Float|) $) $
                     (|Pattern| (|Float|))
                     (|PatternMatchResult| (|Float|) $)))
                T (ELT $ 84))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Integer|) $) $
                     (|Pattern| (|Integer|))
                     (|PatternMatchResult| (|Integer|) $)))
                T (ELT $ 75))
               ((|convert| ((|Pattern| (|Float|)) $)) T (ELT $ 79))
               ((|convert| ((|Pattern| (|Integer|)) $)) T (ELT $ 70))
               ((|reducedSystem| ((|Matrix| |#2|) (|Matrix| $))) T
                (ELT $ 28))
               ((|reducedSystem|
                    ((|Record| (|:| |mat| (|Matrix| |#2|))
                         (|:| |vec| (|Vector| |#2|)))
                     (|Matrix| $) (|Vector| $)))
                T (ELT $ 112))
               ((|reducedSystem|
                    ((|Record| (|:| |mat| (|Matrix| (|Integer|)))
                         (|:| |vec| (|Vector| (|Integer|))))
                     (|Matrix| $) (|Vector| $)))
                T (ELT $ NIL))
               ((|reducedSystem| ((|Matrix| (|Integer|)) (|Matrix| $)))
                T (ELT $ NIL))
               ((|differentiate| ($ $ (|Mapping| |#2| |#2|))) T
                (ELT $ 36))
               ((|differentiate|
                    ($ $ (|Mapping| |#2| |#2|) (|NonNegativeInteger|)))
                T (ELT $ NIL))
               ((|differentiate|
                    ($ $ (|List| (|Symbol|))
                       (|List| (|NonNegativeInteger|))))
                T (ELT $ NIL))
               ((|differentiate|
                    ($ $ (|Symbol|) (|NonNegativeInteger|)))
                T (ELT $ NIL))
               ((|differentiate| ($ $ (|List| (|Symbol|)))) T
                (ELT $ NIL))
               ((|differentiate| ($ $ (|Symbol|))) T (ELT $ NIL))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|differentiate| ($ $)) T (ELT $ NIL))
               ((|map| ($ (|Mapping| |#2| |#2|) $)) T (ELT $ 22))
               ((|retract| (|#2| $)) T (ELT $ NIL))
               ((|retractIfCan| ((|Union| |#2| "failed") $)) T
                (ELT $ NIL))
               ((|coerce| ($ |#2|)) T (ELT $ NIL))
               ((|coerce| ($ (|Fraction| (|Integer|)))) T (ELT $ 90))
               ((|coerce| ($ $)) T (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ NIL))
               ((|characteristic| ((|NonNegativeInteger|))) T
                (ELT $ 31))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ NIL)))
             (|addModemap| '|QuotientFieldCategory&|
                 '(|QuotientFieldCategory&| |#1| |#2|)
                 '((CATEGORY |domain|
                       (SIGNATURE < ((|Boolean|) |#1| |#1|))
                       (SIGNATURE |init| (|#1|))
                       (SIGNATURE |nextItem|
                           ((|Union| |#1| "failed") |#1|))
                       (SIGNATURE |retract| ((|Integer|) |#1|))
                       (SIGNATURE |retractIfCan|
                           ((|Union| (|Integer|) "failed") |#1|))
                       (SIGNATURE |retract|
                           ((|Fraction| (|Integer|)) |#1|))
                       (SIGNATURE |retractIfCan|
                           ((|Union| (|Fraction| (|Integer|)) "failed")
                            |#1|))
                       (SIGNATURE |convert| ((|DoubleFloat|) |#1|))
                       (SIGNATURE |convert| ((|Float|) |#1|))
                       (SIGNATURE |convert| ((|InputForm|) |#1|))
                       (SIGNATURE |retract| ((|Symbol|) |#1|))
                       (SIGNATURE |retractIfCan|
                           ((|Union| (|Symbol|) "failed") |#1|))
                       (SIGNATURE |coerce| (|#1| (|Symbol|)))
                       (SIGNATURE |random| (|#1|))
                       (SIGNATURE |fractionPart| (|#1| |#1|))
                       (SIGNATURE |denominator| (|#1| |#1|))
                       (SIGNATURE |numerator| (|#1| |#1|))
                       (SIGNATURE |patternMatch|
                           ((|PatternMatchResult| (|Float|) |#1|) |#1|
                            (|Pattern| (|Float|))
                            (|PatternMatchResult| (|Float|) |#1|)))
                       (SIGNATURE |patternMatch|
                           ((|PatternMatchResult| (|Integer|) |#1|)
                            |#1| (|Pattern| (|Integer|))
                            (|PatternMatchResult| (|Integer|) |#1|)))
                       (SIGNATURE |convert|
                           ((|Pattern| (|Float|)) |#1|))
                       (SIGNATURE |convert|
                           ((|Pattern| (|Integer|)) |#1|))
                       (SIGNATURE |reducedSystem|
                           ((|Matrix| |#2|) (|Matrix| |#1|)))
                       (SIGNATURE |reducedSystem|
                           ((|Record| (|:| |mat| (|Matrix| |#2|))
                                (|:| |vec| (|Vector| |#2|)))
                            (|Matrix| |#1|) (|Vector| |#1|)))
                       (SIGNATURE |reducedSystem|
                           ((|Record|
                                (|:| |mat| (|Matrix| (|Integer|)))
                                (|:| |vec| (|Vector| (|Integer|))))
                            (|Matrix| |#1|) (|Vector| |#1|)))
                       (SIGNATURE |reducedSystem|
                           ((|Matrix| (|Integer|)) (|Matrix| |#1|)))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|Mapping| |#2| |#2|)))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|Mapping| |#2| |#2|)
                                 (|NonNegativeInteger|)))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|List| (|Symbol|))
                                 (|List| (|NonNegativeInteger|))))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|Symbol|)
                                 (|NonNegativeInteger|)))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|List| (|Symbol|))))
                       (SIGNATURE |differentiate|
                                  (|#1| |#1| (|Symbol|)))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE |differentiate| (|#1| |#1|))
                       (SIGNATURE |map|
                           (|#1| (|Mapping| |#2| |#2|) |#1|))
                       (SIGNATURE |retract| (|#2| |#1|))
                       (SIGNATURE |retractIfCan|
                           ((|Union| |#2| "failed") |#1|))
                       (SIGNATURE |coerce| (|#1| |#2|))
                       (SIGNATURE |coerce|
                           (|#1| (|Fraction| (|Integer|))))
                       (SIGNATURE |coerce| (|#1| |#1|))
                       (SIGNATURE |coerce| (|#1| (|Integer|)))
                       (SIGNATURE |characteristic|
                           ((|NonNegativeInteger|)))
                       (SIGNATURE |coerce| ((|OutputForm|) |#1|)))
                   (|QuotientFieldCategory| |#2|) (|IntegralDomain|))
                 T '|QuotientFieldCategory&|
                 (|put| '|QuotientFieldCategory&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE < ((|Boolean|) |#1| |#1|))
                                 (SIGNATURE |init| (|#1|))
                                 (SIGNATURE |nextItem|
                                     ((|Union| |#1| "failed") |#1|))
                                 (SIGNATURE |retract|
                                     ((|Integer|) |#1|))
                                 (SIGNATURE |retractIfCan|
                                     ((|Union| (|Integer|) "failed")
                                      |#1|))
                                 (SIGNATURE |retract|
                                     ((|Fraction| (|Integer|)) |#1|))
                                 (SIGNATURE |retractIfCan|
                                     ((|Union| (|Fraction| (|Integer|))
                                       "failed")
                                      |#1|))
                                 (SIGNATURE |convert|
                                     ((|DoubleFloat|) |#1|))
                                 (SIGNATURE |convert| ((|Float|) |#1|))
                                 (SIGNATURE |convert|
                                     ((|InputForm|) |#1|))
                                 (SIGNATURE |retract|
                                     ((|Symbol|) |#1|))
                                 (SIGNATURE |retractIfCan|
                                     ((|Union| (|Symbol|) "failed")
                                      |#1|))
                                 (SIGNATURE |coerce| (|#1| (|Symbol|)))
                                 (SIGNATURE |random| (|#1|))
                                 (SIGNATURE |fractionPart| (|#1| |#1|))
                                 (SIGNATURE |denominator| (|#1| |#1|))
                                 (SIGNATURE |numerator| (|#1| |#1|))
                                 (SIGNATURE |patternMatch|
                                     ((|PatternMatchResult| (|Float|)
                                       |#1|)
                                      |#1| (|Pattern| (|Float|))
                                      (|PatternMatchResult| (|Float|)
                                       |#1|)))
                                 (SIGNATURE |patternMatch|
                                     ((|PatternMatchResult| (|Integer|)
                                       |#1|)
                                      |#1| (|Pattern| (|Integer|))
                                      (|PatternMatchResult| (|Integer|)
                                       |#1|)))
                                 (SIGNATURE |convert|
                                     ((|Pattern| (|Float|)) |#1|))
                                 (SIGNATURE |convert|
                                     ((|Pattern| (|Integer|)) |#1|))
                                 (SIGNATURE |reducedSystem|
                                     ((|Matrix| |#2|) (|Matrix| |#1|)))
                                 (SIGNATURE |reducedSystem|
                                     ((|Record|
                                       (|:| |mat| (|Matrix| |#2|))
                                       (|:| |vec| (|Vector| |#2|)))
                                      (|Matrix| |#1|) (|Vector| |#1|)))
                                 (SIGNATURE |reducedSystem|
                                     ((|Record|
                                       (|:| |mat|
                                        (|Matrix| (|Integer|)))
                                       (|:| |vec|
                                        (|Vector| (|Integer|))))
                                      (|Matrix| |#1|) (|Vector| |#1|)))
                                 (SIGNATURE |reducedSystem|
                                     ((|Matrix| (|Integer|))
                                      (|Matrix| |#1|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|Mapping| |#2| |#2|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|Mapping| |#2| |#2|)
                                      (|NonNegativeInteger|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|List| (|Symbol|))
                                      (|List| (|NonNegativeInteger|))))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|Symbol|)
                                      (|NonNegativeInteger|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|List| (|Symbol|))))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|Symbol|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1|))
                                 (SIGNATURE |map|
                                     (|#1| (|Mapping| |#2| |#2|) |#1|))
                                 (SIGNATURE |retract| (|#2| |#1|))
                                 (SIGNATURE |retractIfCan|
                                     ((|Union| |#2| "failed") |#1|))
                                 (SIGNATURE |coerce| (|#1| |#2|))
                                 (SIGNATURE |coerce|
                                     (|#1| (|Fraction| (|Integer|))))
                                 (SIGNATURE |coerce| (|#1| |#1|))
                                 (SIGNATURE |coerce|
                                     (|#1| (|Integer|)))
                                 (SIGNATURE |characteristic|
                                     ((|NonNegativeInteger|)))
                                 (SIGNATURE |coerce|
                                     ((|OutputForm|) |#1|)))
                             (|QuotientFieldCategory| |#2|)
                             (|IntegralDomain|))
                        |$CategoryFrame|)))) 
