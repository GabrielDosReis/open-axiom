
(/VERSIONCHECK 2) 

(PUT '|INS-;characteristic;Nni;1| '|SPADreplace| '(XLAM NIL 0)) 

(DEFUN |INS-;characteristic;Nni;1| ($) 0) 

(DEFUN |INS-;differentiate;2S;2| (|x| $) (|spadConstant| $ 9)) 

(DEFUN |INS-;even?;SB;3| (|x| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 12)) (QREFELT $ 13))) 

(DEFUN |INS-;positive?;SB;4| (|x| $)
  (SPADCALL (|spadConstant| $ 9) |x| (QREFELT $ 15))) 

(PUT '|INS-;copy;2S;5| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |INS-;copy;2S;5| (|x| $) |x|) 

(DEFUN |INS-;bit?;2SB;6| (|x| |i| $)
  (SPADCALL (SPADCALL |x| (SPADCALL |i| (QREFELT $ 18)) (QREFELT $ 19))
      (QREFELT $ 12))) 

(DEFUN |INS-;mask;2S;7| (|n| $)
  (SPADCALL (SPADCALL (|spadConstant| $ 21) |n| (QREFELT $ 19))
      (QREFELT $ 22))) 

(PUT '|INS-;rational?;SB;8| '|SPADreplace| '(XLAM (|x|) 'T)) 

(DEFUN |INS-;rational?;SB;8| (|x| $) 'T) 

(DEFUN |INS-;euclideanSize;SNni;9| (|x| $)
  (PROG (#0=#:G1412 #1=#:G1413)
    (RETURN
      (COND
        ((SPADCALL |x| (|spadConstant| $ 9) (QREFELT $ 25))
         (|error| "euclideanSize called on zero"))
        ((SPADCALL |x| (|spadConstant| $ 9) (QREFELT $ 15))
         (PROG1 (LETT #0# (- (SPADCALL |x| (QREFELT $ 27)))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#)))
        ('T
         (PROG1 (LETT #1# (SPADCALL |x| (QREFELT $ 27))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (>= #1# 0) '(|NonNegativeInteger|) #1#))))))) 

(DEFUN |INS-;convert;SF;10| (|x| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 27)) (QREFELT $ 30))) 

(DEFUN |INS-;convert;SDf;11| (|x| $)
  (FLOAT (SPADCALL |x| (QREFELT $ 27)) MOST-POSITIVE-LONG-FLOAT)) 

(DEFUN |INS-;convert;SIf;12| (|x| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 27)) (QREFELT $ 35))) 

(DEFUN |INS-;retract;SI;13| (|x| $) (SPADCALL |x| (QREFELT $ 27))) 

(DEFUN |INS-;convert;SP;14| (|x| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 27)) (QREFELT $ 39))) 

(DEFUN |INS-;factor;SF;15| (|x| $) (SPADCALL |x| (QREFELT $ 43))) 

(DEFUN |INS-;squareFree;SF;16| (|x| $) (SPADCALL |x| (QREFELT $ 46))) 

(DEFUN |INS-;prime?;SB;17| (|x| $) (SPADCALL |x| (QREFELT $ 49))) 

(DEFUN |INS-;factorial;2S;18| (|x| $) (SPADCALL |x| (QREFELT $ 52))) 

(DEFUN |INS-;binomial;3S;19| (|n| |m| $)
  (SPADCALL |n| |m| (QREFELT $ 54))) 

(DEFUN |INS-;permutation;3S;20| (|n| |m| $)
  (SPADCALL |n| |m| (QREFELT $ 56))) 

(DEFUN |INS-;retractIfCan;SU;21| (|x| $)
  (CONS 0 (SPADCALL |x| (QREFELT $ 27)))) 

(DEFUN |INS-;init;S;22| ($) (|spadConstant| $ 9)) 

(DEFUN |INS-;nextItem;SU;23| (|n| $)
  (COND
    ((SPADCALL |n| (QREFELT $ 61)) (CONS 0 (|spadConstant| $ 21)))
    ((SPADCALL (|spadConstant| $ 9) |n| (QREFELT $ 15))
     (CONS 0 (SPADCALL |n| (QREFELT $ 18))))
    ('T (CONS 0 (SPADCALL (|spadConstant| $ 21) |n| (QREFELT $ 62)))))) 

(DEFUN |INS-;patternMatch;SP2Pmr;24| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (QREFELT $ 67))) 

(DEFUN |INS-;rational;SF;25| (|x| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 27)) (QREFELT $ 71))) 

(DEFUN |INS-;rationalIfCan;SU;26| (|x| $)
  (CONS 0 (SPADCALL (SPADCALL |x| (QREFELT $ 27)) (QREFELT $ 71)))) 

(DEFUN |INS-;symmetricRemainder;3S;27| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (SPADCALL |x| |n| (QREFELT $ 75))
                 |INS-;symmetricRemainder;3S;27|)
           (EXIT (COND
                   ((SPADCALL |r| (|spadConstant| $ 9) (QREFELT $ 25))
                    |r|)
                   ('T
                    (SEQ (COND
                           ((SPADCALL |n| (|spadConstant| $ 9)
                                (QREFELT $ 15))
                            (LETT |n| (SPADCALL |n| (QREFELT $ 18))
                                  |INS-;symmetricRemainder;3S;27|)))
                         (EXIT (COND
                                 ((SPADCALL (|spadConstant| $ 9) |r|
                                      (QREFELT $ 15))
                                  (COND
                                    ((SPADCALL |n|
                                      (SPADCALL 2 |r| (QREFELT $ 77))
                                      (QREFELT $ 15))
                                     (SPADCALL |r| |n| (QREFELT $ 62)))
                                    ('T |r|)))
                                 ((NULL (SPADCALL (|spadConstant| $ 9)
                                         (SPADCALL
                                          (SPADCALL 2 |r|
                                           (QREFELT $ 77))
                                          |n| (QREFELT $ 78))
                                         (QREFELT $ 15)))
                                  (SPADCALL |r| |n| (QREFELT $ 78)))
                                 ('T |r|))))))))))) 

(DEFUN |INS-;invmod;3S;28| (|a| |b| $)
  (PROG (|q| |r| |r1| |c| |c1| |d| |d1|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (QREFELT $ 80))
              (LETT |a| (SPADCALL |a| |b| (QREFELT $ 81))
                    |INS-;invmod;3S;28|)))
           (LETT |c| |a| |INS-;invmod;3S;28|)
           (LETT |c1| (|spadConstant| $ 21) |INS-;invmod;3S;28|)
           (LETT |d| |b| |INS-;invmod;3S;28|)
           (LETT |d1| (|spadConstant| $ 9) |INS-;invmod;3S;28|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (SPADCALL |d| (QREFELT $ 61))
                             (QREFELT $ 13)))
                   (GO G191)))
                (SEQ (LETT |q| (SPADCALL |c| |d| (QREFELT $ 82))
                           |INS-;invmod;3S;28|)
                     (LETT |r|
                           (SPADCALL |c|
                               (SPADCALL |q| |d| (QREFELT $ 83))
                               (QREFELT $ 62))
                           |INS-;invmod;3S;28|)
                     (LETT |r1|
                           (SPADCALL |c1|
                               (SPADCALL |q| |d1| (QREFELT $ 83))
                               (QREFELT $ 62))
                           |INS-;invmod;3S;28|)
                     (LETT |c| |d| |INS-;invmod;3S;28|)
                     (LETT |c1| |d1| |INS-;invmod;3S;28|)
                     (LETT |d| |r| |INS-;invmod;3S;28|)
                     (EXIT (LETT |d1| |r1| |INS-;invmod;3S;28|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |c| (|spadConstant| $ 21) (QREFELT $ 25))
                    (COND
                      ((SPADCALL |c1| (QREFELT $ 80))
                       (SPADCALL |c1| |b| (QREFELT $ 78)))
                      ('T |c1|)))
                   ('T (|error| "inverse does not exist")))))))) 

(DEFUN |INS-;powmod;4S;29| (|x| |n| |p| $)
  (PROG (|y| #0=#:G1470 |z|)
    (RETURN
      (SEQ (EXIT (SEQ (COND
                        ((SPADCALL |x| (QREFELT $ 80))
                         (LETT |x| (SPADCALL |x| |p| (QREFELT $ 81))
                               |INS-;powmod;4S;29|)))
                      (EXIT (COND
                              ((SPADCALL |x| (QREFELT $ 61))
                               (|spadConstant| $ 9))
                              ((SPADCALL |n| (QREFELT $ 61))
                               (|spadConstant| $ 21))
                              ('T
                               (SEQ (LETT |y| (|spadConstant| $ 21)
                                     |INS-;powmod;4S;29|)
                                    (LETT |z| |x| |INS-;powmod;4S;29|)
                                    (EXIT
                                     (SEQ G190 NIL
                                      (SEQ
                                       (COND
                                         ((SPADCALL |n| (QREFELT $ 12))
                                          (LETT |y|
                                           (SPADCALL |y| |z| |p|
                                            (QREFELT $ 85))
                                           |INS-;powmod;4S;29|)))
                                       (EXIT
                                        (COND
                                          ((SPADCALL
                                            (LETT |n|
                                             (SPADCALL |n|
                                              (SPADCALL
                                               (|spadConstant| $ 21)
                                               (QREFELT $ 18))
                                              (QREFELT $ 19))
                                             |INS-;powmod;4S;29|)
                                            (QREFELT $ 61))
                                           (PROGN
                                             (LETT #0# |y|
                                              |INS-;powmod;4S;29|)
                                             (GO #0#)))
                                          ('T
                                           (LETT |z|
                                            (SPADCALL |z| |z| |p|
                                             (QREFELT $ 85))
                                            |INS-;powmod;4S;29|)))))
                                      NIL (GO G190) G191 (EXIT NIL)))))))))
           #0# (EXIT #0#))))) 

(DEFUN |IntegerNumberSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IntegerNumberSystem&|))
        (LETT |dv$| (LIST '|IntegerNumberSystem&| |dv$1|) . #0#)
        (LETT $ (GETREFV 87) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|IntegerNumberSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) |INS-;characteristic;Nni;1|
             (0 . |Zero|) |INS-;differentiate;2S;2| (|Boolean|)
             (4 . |odd?|) (9 . |not|) |INS-;even?;SB;3| (14 . <)
             |INS-;positive?;SB;4| |INS-;copy;2S;5| (20 . -)
             (25 . |shift|) |INS-;bit?;2SB;6| (31 . |One|) (35 . |dec|)
             |INS-;mask;2S;7| |INS-;rational?;SB;8| (40 . =)
             (|Integer|) (46 . |convert|) |INS-;euclideanSize;SNni;9|
             (|Float|) (51 . |coerce|) |INS-;convert;SF;10|
             (|DoubleFloat|) |INS-;convert;SDf;11| (|InputForm|)
             (56 . |convert|) |INS-;convert;SIf;12|
             |INS-;retract;SI;13| (|Pattern| 26) (61 . |coerce|)
             |INS-;convert;SP;14| (|Factored| 6)
             (|IntegerFactorizationPackage| 6) (66 . |factor|)
             (|Factored| $) |INS-;factor;SF;15| (71 . |squareFree|)
             |INS-;squareFree;SF;16| (|IntegerPrimesPackage| 6)
             (76 . |prime?|) |INS-;prime?;SB;17|
             (|IntegerCombinatoricFunctions| 6) (81 . |factorial|)
             |INS-;factorial;2S;18| (86 . |binomial|)
             |INS-;binomial;3S;19| (92 . |permutation|)
             |INS-;permutation;3S;20| (|Union| 26 '"failed")
             |INS-;retractIfCan;SU;21| |INS-;init;S;22| (98 . |zero?|)
             (103 . -) (|Union| $ '"failed") |INS-;nextItem;SU;23|
             (|PatternMatchResult| 26 6)
             (|PatternMatchIntegerNumberSystem| 6)
             (109 . |patternMatch|) (|PatternMatchResult| 26 $)
             |INS-;patternMatch;SP2Pmr;24| (|Fraction| 26)
             (116 . |coerce|) |INS-;rational;SF;25|
             (|Union| 70 '"failed") |INS-;rationalIfCan;SU;26|
             (121 . |rem|) (|PositiveInteger|) (127 . *) (133 . +)
             |INS-;symmetricRemainder;3S;27| (139 . |negative?|)
             (144 . |positiveRemainder|) (150 . |quo|) (156 . *)
             |INS-;invmod;3S;28| (162 . |mulmod|) |INS-;powmod;4S;29|)
          '#(|symmetricRemainder| 169 |squareFree| 175 |retractIfCan|
             180 |retract| 185 |rationalIfCan| 190 |rational?| 195
             |rational| 200 |prime?| 205 |powmod| 210 |positive?| 217
             |permutation| 222 |patternMatch| 228 |nextItem| 235 |mask|
             240 |invmod| 245 |init| 251 |factorial| 255 |factor| 260
             |even?| 265 |euclideanSize| 270 |differentiate| 275 |copy|
             280 |convert| 285 |characteristic| 305 |bit?| 309
             |binomial| 315)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 86
                                '(0 6 0 9 1 6 11 0 12 1 11 0 0 13 2 6
                                  11 0 0 15 1 6 0 0 18 2 6 0 0 0 19 0 6
                                  0 21 1 6 0 0 22 2 6 11 0 0 25 1 6 26
                                  0 27 1 29 0 26 30 1 34 0 26 35 1 38 0
                                  26 39 1 42 41 6 43 1 42 41 6 46 1 48
                                  11 6 49 1 51 6 6 52 2 51 6 6 6 54 2
                                  51 6 6 6 56 1 6 11 0 61 2 6 0 0 0 62
                                  3 66 65 6 38 65 67 1 70 0 26 71 2 6 0
                                  0 0 75 2 6 0 76 0 77 2 6 0 0 0 78 1 6
                                  11 0 80 2 6 0 0 0 81 2 6 0 0 0 82 2 6
                                  0 0 0 83 3 6 0 0 0 0 85 2 0 0 0 0 79
                                  1 0 44 0 47 1 0 58 0 59 1 0 26 0 37 1
                                  0 73 0 74 1 0 11 0 24 1 0 70 0 72 1 0
                                  11 0 50 3 0 0 0 0 0 86 1 0 11 0 16 2
                                  0 0 0 0 57 3 0 68 0 38 68 69 1 0 63 0
                                  64 1 0 0 0 23 2 0 0 0 0 84 0 0 0 60 1
                                  0 0 0 53 1 0 44 0 45 1 0 11 0 14 1 0
                                  7 0 28 1 0 0 0 10 1 0 0 0 17 1 0 32 0
                                  33 1 0 29 0 31 1 0 38 0 40 1 0 34 0
                                  36 0 0 7 8 2 0 11 0 0 20 2 0 0 0 0
                                  55)))))
          '|lookupComplete|)) 
