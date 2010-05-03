
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;numerator;2A;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;denominator;2A;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |QFCAT-;init;A;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |QFCAT-;nextItem;AU;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |QFCAT-;map;M2A;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;reducedSystem;MM;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |QFCAT-;characteristic;Nni;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |QFCAT-;differentiate;AMA;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;convert;AIf;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;convert;AF;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%DoubleFloat|)
                |QFCAT-;convert;ADf;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |QFCAT-;<;2AB;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |QFCAT-;<;2AB;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |QFCAT-;<;2AB;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;fractionPart;2A;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;coerce;SA;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;retract;AS;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |QFCAT-;retractIfCan;AU;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;convert;AP;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |QFCAT-;patternMatch;AP2Pmr;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;convert;AP;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |QFCAT-;patternMatch;AP2Pmr;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |QFCAT-;coerce;FA;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |QFCAT-;retract;AI;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |QFCAT-;retractIfCan;AU;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |QFCAT-;random;A;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |QFCAT-;reducedSystem;MVR;27|)) 

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

(DEFUN |QFCAT-;characteristic;Nni;7| ($) (|spadConstant| $ 30)) 

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
      (|getShellEntry| $ 50))) 

(DEFUN |QFCAT-;<;2AB;13| (|x| |y| $)
  (PROG (|#G19| |#G20| |#G21| |#G22|)
    (RETURN
      (SEQ (COND
             ((SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
                  (|spadConstant| $ 52) (|getShellEntry| $ 50))
              (PROGN
                (LETT |#G19| |y| |QFCAT-;<;2AB;13|)
                (LETT |#G20| |x| |QFCAT-;<;2AB;13|)
                (LETT |x| |#G19| |QFCAT-;<;2AB;13|)
                (LETT |y| |#G20| |QFCAT-;<;2AB;13|))))
           (COND
             ((SPADCALL (SPADCALL |y| (|getShellEntry| $ 11))
                  (|spadConstant| $ 52) (|getShellEntry| $ 50))
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
                     (|getShellEntry| $ 50))))))) 

(DEFUN |QFCAT-;<;2AB;14| (|x| |y| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (SPADCALL |y| (|getShellEntry| $ 11)) (|getShellEntry| $ 32))
      (SPADCALL (SPADCALL |y| (|getShellEntry| $ 8))
          (SPADCALL |x| (|getShellEntry| $ 11)) (|getShellEntry| $ 32))
      (|getShellEntry| $ 50))) 

(DEFUN |QFCAT-;fractionPart;2A;15| (|x| $)
  (SPADCALL |x|
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 53))
          (|getShellEntry| $ 9))
      (|getShellEntry| $ 54))) 

(DEFUN |QFCAT-;coerce;SA;16| (|s| $)
  (SPADCALL (SPADCALL |s| (|getShellEntry| $ 57))
      (|getShellEntry| $ 9))) 

(DEFUN |QFCAT-;retract;AS;17| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 59))
      (|getShellEntry| $ 60))) 

(DEFUN |QFCAT-;retractIfCan;AU;18| (|x| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (SPADCALL |x| (|getShellEntry| $ 63))
                 |QFCAT-;retractIfCan;AU;18|)
           (EXIT (COND
                   ((QEQCAR |r| 1) (CONS 1 "failed"))
                   ('T (SPADCALL (QCDR |r|) (|getShellEntry| $ 65))))))))) 

(DEFUN |QFCAT-;convert;AP;19| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (|getShellEntry| $ 69))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
          (|getShellEntry| $ 69))
      (|getShellEntry| $ 70))) 

(DEFUN |QFCAT-;patternMatch;AP2Pmr;20| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 74))) 

(DEFUN |QFCAT-;convert;AP;21| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 8))
          (|getShellEntry| $ 78))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11))
          (|getShellEntry| $ 78))
      (|getShellEntry| $ 79))) 

(DEFUN |QFCAT-;patternMatch;AP2Pmr;22| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 83))) 

(DEFUN |QFCAT-;coerce;FA;23| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 87))
          (|getShellEntry| $ 88))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 89))
          (|getShellEntry| $ 88))
      (|getShellEntry| $ 90))) 

(DEFUN |QFCAT-;retract;AI;24| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 59))
      (|getShellEntry| $ 92))) 

(DEFUN |QFCAT-;retractIfCan;AU;25| (|x| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |x| (|getShellEntry| $ 63))
                 |QFCAT-;retractIfCan;AU;25|)
           (EXIT (COND
                   ((QEQCAR |u| 1) (CONS 1 "failed"))
                   ('T (SPADCALL (QCDR |u|) (|getShellEntry| $ 95))))))))) 

(DEFUN |QFCAT-;random;A;26| ($)
  (PROG (|d|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (SPADCALL
                             (LETT |d|
                                   (SPADCALL (|getShellEntry| $ 97))
                                   |QFCAT-;random;A;26|)
                             (|getShellEntry| $ 98)))
                   (GO G191)))
                (SEQ (EXIT |d|)) NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL (SPADCALL (|getShellEntry| $ 97)) |d|
                     (|getShellEntry| $ 15))))))) 

(DEFUN |QFCAT-;reducedSystem;MVR;27| (|m| |v| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n|
                 (SPADCALL
                     (SPADCALL (SPADCALL |v| (|getShellEntry| $ 101))
                         |m| (|getShellEntry| $ 102))
                     (|getShellEntry| $ 103))
                 |QFCAT-;reducedSystem;MVR;27|)
           (EXIT (CONS (SPADCALL |n|
                           (SPADCALL |n| (|getShellEntry| $ 104))
                           (SPADCALL |n| (|getShellEntry| $ 105))
                           (+ 1 (SPADCALL |n| (|getShellEntry| $ 107)))
                           (SPADCALL |n| (|getShellEntry| $ 109))
                           (|getShellEntry| $ 110))
                       (SPADCALL |n|
                           (SPADCALL |n| (|getShellEntry| $ 107))
                           (|getShellEntry| $ 112)))))))) 

(DEFUN |QuotientFieldCategory&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|QuotientFieldCategory&| |dv$1| |dv$2|))
         ($ (|newShell| 123))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasCategory| |#2|
                              '(|PolynomialFactorizationExplicit|))
                          (|HasCategory| |#2| '(|IntegerNumberSystem|))
                          (|HasCategory| |#2| '(|EuclideanDomain|))
                          (|HasCategory| |#2|
                              (LIST '|RetractableTo| '(|Symbol|)))
                          (|HasCategory| |#2|
                              '(|CharacteristicNonZero|))
                          (|HasCategory| |#2| '(|CharacteristicZero|))
                          (|HasCategory| |#2|
                              (LIST '|ConvertibleTo| '(|InputForm|)))
                          (|HasCategory| |#2| '(|RealConstant|))
                          (|HasCategory| |#2|
                              '(|OrderedIntegralDomain|))
                          (|HasCategory| |#2| '(|OrderedSet|))
                          (|HasCategory| |#2|
                              (LIST '|RetractableTo| '(|Integer|)))
                          (|HasCategory| |#2| '(|StepThrough|))))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
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
         (|setShellEntry| $ 48
             (CONS (|dispatchFunction| |QFCAT-;convert;ADf;11|) $)))))
    (COND
      ((|testBitVector| |pv$| 9)
       (COND
         ((|HasAttribute| |#2| '|canonicalUnitNormal|)
          (|setShellEntry| $ 51
              (CONS (|dispatchFunction| |QFCAT-;<;2AB;12|) $)))
         ('T
          (|setShellEntry| $ 51
              (CONS (|dispatchFunction| |QFCAT-;<;2AB;13|) $)))))
      ((|testBitVector| |pv$| 10)
       (|setShellEntry| $ 51
           (CONS (|dispatchFunction| |QFCAT-;<;2AB;14|) $))))
    (COND
      ((|testBitVector| |pv$| 3)
       (|setShellEntry| $ 55
           (CONS (|dispatchFunction| |QFCAT-;fractionPart;2A;15|) $))))
    (COND
      ((|testBitVector| |pv$| 4)
       (PROGN
         (|setShellEntry| $ 58
             (CONS (|dispatchFunction| |QFCAT-;coerce;SA;16|) $))
         (|setShellEntry| $ 61
             (CONS (|dispatchFunction| |QFCAT-;retract;AS;17|) $))
         (|setShellEntry| $ 66
             (CONS (|dispatchFunction| |QFCAT-;retractIfCan;AU;18|) $)))))
    (COND
      ((|HasCategory| |#2| '(|ConvertibleTo| (|Pattern| (|Integer|))))
       (PROGN
         (|setShellEntry| $ 71
             (CONS (|dispatchFunction| |QFCAT-;convert;AP;19|) $))
         (COND
           ((|HasCategory| |#2| '(|PatternMatchable| (|Integer|)))
            (|setShellEntry| $ 76
                (CONS (|dispatchFunction|
                          |QFCAT-;patternMatch;AP2Pmr;20|)
                      $)))))))
    (COND
      ((|HasCategory| |#2| '(|ConvertibleTo| (|Pattern| (|Float|))))
       (PROGN
         (|setShellEntry| $ 80
             (CONS (|dispatchFunction| |QFCAT-;convert;AP;21|) $))
         (COND
           ((|HasCategory| |#2| '(|PatternMatchable| (|Float|)))
            (|setShellEntry| $ 85
                (CONS (|dispatchFunction|
                          |QFCAT-;patternMatch;AP2Pmr;22|)
                      $)))))))
    (COND
      ((|testBitVector| |pv$| 11)
       (PROGN
         (|setShellEntry| $ 91
             (CONS (|dispatchFunction| |QFCAT-;coerce;FA;23|) $))
         (COND
           ((|domainEqual| |#2| (|Integer|)))
           ('T
            (PROGN
              (|setShellEntry| $ 93
                  (CONS (|dispatchFunction| |QFCAT-;retract;AI;24|) $))
              (|setShellEntry| $ 96
                  (CONS (|dispatchFunction|
                            |QFCAT-;retractIfCan;AU;25|)
                        $))))))))
    (COND
      ((|testBitVector| |pv$| 2)
       (|setShellEntry| $ 99
           (CONS (|dispatchFunction| |QFCAT-;random;A;26|) $))))
    $)) 

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
             (|DoubleFloat|) (106 . |convert|) (111 . /)
             (117 . |convert|) (|Boolean|) (122 . <) (128 . <)
             (134 . |Zero|) (138 . |wholePart|) (143 . -)
             (149 . |fractionPart|) (|Symbol|) (154 . |coerce|)
             (159 . |coerce|) (164 . |retract|) (169 . |retract|)
             (174 . |retract|) (|Union| 7 '"failed")
             (179 . |retractIfCan|) (|Union| 56 '"failed")
             (184 . |retractIfCan|) (189 . |retractIfCan|) (|Integer|)
             (|Pattern| 67) (194 . |convert|) (199 . /)
             (205 . |convert|) (|PatternMatchResult| 67 6)
             (|PatternMatchQuotientFieldCategory| 67 7 6)
             (210 . |patternMatch|) (|PatternMatchResult| 67 $)
             (217 . |patternMatch|) (|Pattern| 41) (224 . |convert|)
             (229 . /) (235 . |convert|) (|PatternMatchResult| 41 6)
             (|PatternMatchQuotientFieldCategory| 41 7 6)
             (240 . |patternMatch|) (|PatternMatchResult| 41 $)
             (247 . |patternMatch|) (|Fraction| 67) (254 . |numer|)
             (259 . |coerce|) (264 . |denom|) (269 . /)
             (275 . |coerce|) (280 . |retract|) (285 . |retract|)
             (|Union| 67 '"failed") (290 . |retractIfCan|)
             (295 . |retractIfCan|) (300 . |random|) (304 . |zero?|)
             (309 . |random|) (|Vector| 6) (313 . |coerce|)
             (318 . |horizConcat|) (324 . |reducedSystem|)
             (329 . |minRowIndex|) (334 . |maxRowIndex|) (339 . |One|)
             (343 . |minColIndex|) (348 . +) (354 . |maxColIndex|)
             (359 . |subMatrix|) (|Vector| 7) (368 . |column|)
             (|Record| (|:| |mat| 23) (|:| |vec| 111)) (|Vector| $)
             |QFCAT-;reducedSystem;MVR;27| (|Union| 86 '"failed")
             (|Matrix| 67) (|Vector| 67)
             (|Record| (|:| |mat| 117) (|:| |vec| 118)) (|List| 56)
             (|List| 29) (|OutputForm|))
          '#(|retractIfCan| 374 |retract| 384 |reducedSystem| 394
             |random| 405 |patternMatch| 409 |numerator| 423 |nextItem|
             428 |map| 433 |init| 439 |fractionPart| 443
             |differentiate| 448 |denominator| 454 |convert| 459
             |coerce| 484 |characteristic| 494 < 498)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 115
                                '(1 6 7 0 8 1 6 0 7 9 1 6 7 0 11 0 7 0
                                  13 0 7 0 14 2 6 0 7 7 15 0 0 0 16 1 7
                                  17 0 18 0 6 0 19 1 0 17 0 20 1 25 23
                                  24 26 0 7 29 30 2 7 0 0 0 32 2 7 0 0
                                  0 33 2 7 0 0 34 35 1 7 37 0 38 2 37 0
                                  0 0 39 1 0 37 0 40 1 7 41 0 42 2 41 0
                                  0 0 43 1 0 41 0 44 1 7 45 0 46 2 45 0
                                  0 0 47 1 0 45 0 48 2 7 49 0 0 50 2 0
                                  49 0 0 51 0 7 0 52 1 6 7 0 53 2 6 0 0
                                  0 54 1 0 0 0 55 1 7 0 56 57 1 0 0 56
                                  58 1 6 7 0 59 1 7 56 0 60 1 0 56 0 61
                                  1 6 62 0 63 1 7 64 0 65 1 0 64 0 66 1
                                  7 68 0 69 2 68 0 0 0 70 1 0 68 0 71 3
                                  73 72 6 68 72 74 3 0 75 0 68 75 76 1
                                  7 77 0 78 2 77 0 0 0 79 1 0 77 0 80 3
                                  82 81 6 77 81 83 3 0 84 0 77 84 85 1
                                  86 67 0 87 1 6 0 67 88 1 86 67 0 89 2
                                  6 0 0 0 90 1 0 0 86 91 1 7 67 0 92 1
                                  0 67 0 93 1 7 94 0 95 1 0 94 0 96 0 7
                                  0 97 1 7 49 0 98 0 0 0 99 1 24 0 100
                                  101 2 24 0 0 0 102 1 6 23 27 103 1 23
                                  67 0 104 1 23 67 0 105 0 67 0 106 1
                                  23 67 0 107 2 67 0 0 0 108 1 23 67 0
                                  109 5 23 0 0 67 67 67 67 110 2 23 111
                                  0 67 112 1 0 94 0 96 1 0 64 0 66 1 0
                                  67 0 93 1 0 56 0 61 2 0 113 27 114
                                  115 1 0 23 27 28 0 0 0 99 3 0 84 0 77
                                  84 85 3 0 75 0 68 75 76 1 0 0 0 10 1
                                  0 17 0 20 2 0 0 21 0 22 0 0 0 16 1 0
                                  0 0 55 2 0 0 0 21 36 1 0 0 0 12 1 0
                                  45 0 48 1 0 37 0 40 1 0 41 0 44 1 0
                                  68 0 71 1 0 77 0 80 1 0 0 56 58 1 0 0
                                  86 91 0 0 29 31 2 0 49 0 0 51)))))
          '|lookupComplete|)) 
