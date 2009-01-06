
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |INS-;characteristic;Nni;1|)) 

(PUT '|INS-;characteristic;Nni;1| '|SPADreplace| '(XLAM NIL 0)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;differentiate;2S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |INS-;even?;SB;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |INS-;positive?;SB;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;copy;2S;5|)) 

(PUT '|INS-;copy;2S;5| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |INS-;bit?;2SB;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;mask;2S;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |INS-;rational?;SB;8|)) 

(PUT '|INS-;rational?;SB;8| '|SPADreplace| '(XLAM (|x|) 'T)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |INS-;euclideanSize;SNni;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;convert;SF;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%DoubleFloat|)
                |INS-;convert;SDf;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;convert;SIf;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |INS-;retract;SI;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;convert;SP;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;factor;SF;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;squareFree;SF;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |INS-;prime?;SB;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;factorial;2S;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |INS-;binomial;3S;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |INS-;permutation;3S;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |INS-;retractIfCan;SU;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |INS-;init;S;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |INS-;nextItem;SU;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |INS-;patternMatch;SP2Pmr;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INS-;rational;SF;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |INS-;rationalIfCan;SU;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |INS-;symmetricRemainder;3S;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |INS-;invmod;3S;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |INS-;powmod;4S;29|)) 

(DEFUN |INS-;characteristic;Nni;1| ($) (DECLARE (IGNORE $)) 0) 

(DEFUN |INS-;differentiate;2S;2| (|x| $) (|spadConstant| $ 9)) 

(DEFUN |INS-;even?;SB;3| (|x| $)
  (NOT (SPADCALL |x| (|getShellEntry| $ 12)))) 

(DEFUN |INS-;positive?;SB;4| (|x| $)
  (SPADCALL (|spadConstant| $ 9) |x| (|getShellEntry| $ 14))) 

(DEFUN |INS-;copy;2S;5| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INS-;bit?;2SB;6| (|x| |i| $)
  (SPADCALL
      (SPADCALL |x| (SPADCALL |i| (|getShellEntry| $ 17))
          (|getShellEntry| $ 18))
      (|getShellEntry| $ 12))) 

(DEFUN |INS-;mask;2S;7| (|n| $)
  (SPADCALL (SPADCALL (|spadConstant| $ 20) |n| (|getShellEntry| $ 18))
      (|getShellEntry| $ 21))) 

(DEFUN |INS-;rational?;SB;8| (|x| $) (DECLARE (IGNORE $)) 'T) 

(DEFUN |INS-;euclideanSize;SNni;9| (|x| $)
  (PROG (#0=#:G1425 #1=#:G1426)
    (RETURN
      (COND
        ((SPADCALL |x| (|spadConstant| $ 9) (|getShellEntry| $ 24))
         (|error| "euclideanSize called on zero"))
        ((SPADCALL |x| (|spadConstant| $ 9) (|getShellEntry| $ 14))
         (PROG1 (LETT #0# (- (SPADCALL |x| (|getShellEntry| $ 26)))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (COND ((< #0# 0) 'NIL) ('T 'T))
               '(|NonNegativeInteger|) #0#)))
        ('T
         (PROG1 (LETT #1# (SPADCALL |x| (|getShellEntry| $ 26))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (COND ((< #1# 0) 'NIL) ('T 'T))
               '(|NonNegativeInteger|) #1#))))))) 

(DEFUN |INS-;convert;SF;10| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26))
      (|getShellEntry| $ 29))) 

(DEFUN |INS-;convert;SDf;11| (|x| $)
  (FLOAT (SPADCALL |x| (|getShellEntry| $ 26)) |$DoubleFloatMaximum|)) 

(DEFUN |INS-;convert;SIf;12| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26))
      (|getShellEntry| $ 34))) 

(DEFUN |INS-;retract;SI;13| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 26))) 

(DEFUN |INS-;convert;SP;14| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26))
      (|getShellEntry| $ 38))) 

(DEFUN |INS-;factor;SF;15| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 42))) 

(DEFUN |INS-;squareFree;SF;16| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 45))) 

(DEFUN |INS-;prime?;SB;17| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 48))) 

(DEFUN |INS-;factorial;2S;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 51))) 

(DEFUN |INS-;binomial;3S;19| (|n| |m| $)
  (SPADCALL |n| |m| (|getShellEntry| $ 53))) 

(DEFUN |INS-;permutation;3S;20| (|n| |m| $)
  (SPADCALL |n| |m| (|getShellEntry| $ 55))) 

(DEFUN |INS-;retractIfCan;SU;21| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 26)))) 

(DEFUN |INS-;init;S;22| ($) (|spadConstant| $ 9)) 

(DEFUN |INS-;nextItem;SU;23| (|n| $)
  (COND
    ((SPADCALL |n| (|getShellEntry| $ 60))
     (CONS 0 (|spadConstant| $ 20)))
    ((SPADCALL (|spadConstant| $ 9) |n| (|getShellEntry| $ 14))
     (CONS 0 (SPADCALL |n| (|getShellEntry| $ 17))))
    ('T
     (CONS 0
           (SPADCALL (|spadConstant| $ 20) |n| (|getShellEntry| $ 61)))))) 

(DEFUN |INS-;patternMatch;SP2Pmr;24| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 66))) 

(DEFUN |INS-;rational;SF;25| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26))
      (|getShellEntry| $ 70))) 

(DEFUN |INS-;rationalIfCan;SU;26| (|x| $)
  (CONS 0
        (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26))
            (|getShellEntry| $ 70)))) 

(DEFUN |INS-;symmetricRemainder;3S;27| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (SPADCALL |x| |n| (|getShellEntry| $ 74))
                 |INS-;symmetricRemainder;3S;27|)
           (EXIT (COND
                   ((SPADCALL |r| (|spadConstant| $ 9)
                        (|getShellEntry| $ 24))
                    |r|)
                   ('T
                    (SEQ (COND
                           ((SPADCALL |n| (|spadConstant| $ 9)
                                (|getShellEntry| $ 14))
                            (LETT |n|
                                  (SPADCALL |n| (|getShellEntry| $ 17))
                                  |INS-;symmetricRemainder;3S;27|)))
                         (EXIT (COND
                                 ((SPADCALL (|spadConstant| $ 9) |r|
                                      (|getShellEntry| $ 14))
                                  (COND
                                    ((SPADCALL |n|
                                      (SPADCALL 2 |r|
                                       (|getShellEntry| $ 76))
                                      (|getShellEntry| $ 14))
                                     (SPADCALL |r| |n|
                                      (|getShellEntry| $ 61)))
                                    ('T |r|)))
                                 ((NULL (SPADCALL (|spadConstant| $ 9)
                                         (SPADCALL
                                          (SPADCALL 2 |r|
                                           (|getShellEntry| $ 76))
                                          |n| (|getShellEntry| $ 77))
                                         (|getShellEntry| $ 14)))
                                  (SPADCALL |r| |n|
                                      (|getShellEntry| $ 77)))
                                 ('T |r|))))))))))) 

(DEFUN |INS-;invmod;3S;28| (|a| |b| $)
  (PROG (|q| |r| |r1| |c| |c1| |d| |d1|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|getShellEntry| $ 79))
              (LETT |a| (SPADCALL |a| |b| (|getShellEntry| $ 80))
                    |INS-;invmod;3S;28|)))
           (LETT |c| |a| |INS-;invmod;3S;28|)
           (LETT |c1| (|spadConstant| $ 20) |INS-;invmod;3S;28|)
           (LETT |d| |b| |INS-;invmod;3S;28|)
           (LETT |d1| (|spadConstant| $ 9) |INS-;invmod;3S;28|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |d| (|getShellEntry| $ 60))))
                   (GO G191)))
                (SEQ (LETT |q|
                           (SPADCALL |c| |d| (|getShellEntry| $ 81))
                           |INS-;invmod;3S;28|)
                     (LETT |r|
                           (SPADCALL |c|
                               (SPADCALL |q| |d|
                                   (|getShellEntry| $ 82))
                               (|getShellEntry| $ 61))
                           |INS-;invmod;3S;28|)
                     (LETT |r1|
                           (SPADCALL |c1|
                               (SPADCALL |q| |d1|
                                   (|getShellEntry| $ 82))
                               (|getShellEntry| $ 61))
                           |INS-;invmod;3S;28|)
                     (LETT |c| |d| |INS-;invmod;3S;28|)
                     (LETT |c1| |d1| |INS-;invmod;3S;28|)
                     (LETT |d| |r| |INS-;invmod;3S;28|)
                     (EXIT (LETT |d1| |r1| |INS-;invmod;3S;28|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |c| (|spadConstant| $ 20)
                        (|getShellEntry| $ 24))
                    (COND
                      ((SPADCALL |c1| (|getShellEntry| $ 79))
                       (SPADCALL |c1| |b| (|getShellEntry| $ 77)))
                      ('T |c1|)))
                   ('T (|error| "inverse does not exist")))))))) 

(DEFUN |INS-;powmod;4S;29| (|x| |n| |p| $)
  (PROG (|y| #0=#:G1483 |z|)
    (RETURN
      (SEQ (EXIT (SEQ (COND
                        ((SPADCALL |x| (|getShellEntry| $ 79))
                         (LETT |x|
                               (SPADCALL |x| |p|
                                   (|getShellEntry| $ 80))
                               |INS-;powmod;4S;29|)))
                      (EXIT (COND
                              ((SPADCALL |x| (|getShellEntry| $ 60))
                               (|spadConstant| $ 9))
                              ((SPADCALL |n| (|getShellEntry| $ 60))
                               (|spadConstant| $ 20))
                              ('T
                               (SEQ (LETT |y| (|spadConstant| $ 20)
                                     |INS-;powmod;4S;29|)
                                    (LETT |z| |x| |INS-;powmod;4S;29|)
                                    (EXIT
                                     (SEQ G190 NIL
                                      (SEQ
                                       (COND
                                         ((SPADCALL |n|
                                           (|getShellEntry| $ 12))
                                          (LETT |y|
                                           (SPADCALL |y| |z| |p|
                                            (|getShellEntry| $ 84))
                                           |INS-;powmod;4S;29|)))
                                       (EXIT
                                        (COND
                                          ((SPADCALL
                                            (LETT |n|
                                             (SPADCALL |n|
                                              (SPADCALL
                                               (|spadConstant| $ 20)
                                               (|getShellEntry| $ 17))
                                              (|getShellEntry| $ 18))
                                             |INS-;powmod;4S;29|)
                                            (|getShellEntry| $ 60))
                                           (PROGN
                                             (LETT #0# |y|
                                              |INS-;powmod;4S;29|)
                                             (GO #0#)))
                                          ('T
                                           (LETT |z|
                                            (SPADCALL |z| |z| |p|
                                             (|getShellEntry| $ 84))
                                            |INS-;powmod;4S;29|)))))
                                      NIL (GO G190) G191 (EXIT NIL)))))))))
           #0# (EXIT #0#))))) 

(DEFUN |IntegerNumberSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IntegerNumberSystem&|))
        (LETT |dv$| (LIST '|IntegerNumberSystem&| |dv$1|) . #0#)
        (LETT $ (|newShell| 86) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|IntegerNumberSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) |INS-;characteristic;Nni;1|
             (0 . |Zero|) |INS-;differentiate;2S;2| (|Boolean|)
             (4 . |odd?|) |INS-;even?;SB;3| (9 . <)
             |INS-;positive?;SB;4| |INS-;copy;2S;5| (15 . -)
             (20 . |shift|) |INS-;bit?;2SB;6| (26 . |One|) (30 . |dec|)
             |INS-;mask;2S;7| |INS-;rational?;SB;8| (35 . =)
             (|Integer|) (41 . |convert|) |INS-;euclideanSize;SNni;9|
             (|Float|) (46 . |coerce|) |INS-;convert;SF;10|
             (|DoubleFloat|) |INS-;convert;SDf;11| (|InputForm|)
             (51 . |convert|) |INS-;convert;SIf;12|
             |INS-;retract;SI;13| (|Pattern| 25) (56 . |coerce|)
             |INS-;convert;SP;14| (|Factored| 6)
             (|IntegerFactorizationPackage| 6) (61 . |factor|)
             (|Factored| $) |INS-;factor;SF;15| (66 . |squareFree|)
             |INS-;squareFree;SF;16| (|IntegerPrimesPackage| 6)
             (71 . |prime?|) |INS-;prime?;SB;17|
             (|IntegerCombinatoricFunctions| 6) (76 . |factorial|)
             |INS-;factorial;2S;18| (81 . |binomial|)
             |INS-;binomial;3S;19| (87 . |permutation|)
             |INS-;permutation;3S;20| (|Union| 25 '"failed")
             |INS-;retractIfCan;SU;21| |INS-;init;S;22| (93 . |zero?|)
             (98 . -) (|Union| $ '"failed") |INS-;nextItem;SU;23|
             (|PatternMatchResult| 25 6)
             (|PatternMatchIntegerNumberSystem| 6)
             (104 . |patternMatch|) (|PatternMatchResult| 25 $)
             |INS-;patternMatch;SP2Pmr;24| (|Fraction| 25)
             (111 . |coerce|) |INS-;rational;SF;25|
             (|Union| 69 '"failed") |INS-;rationalIfCan;SU;26|
             (116 . |rem|) (|PositiveInteger|) (122 . *) (128 . +)
             |INS-;symmetricRemainder;3S;27| (134 . |negative?|)
             (139 . |positiveRemainder|) (145 . |quo|) (151 . *)
             |INS-;invmod;3S;28| (157 . |mulmod|) |INS-;powmod;4S;29|)
          '#(|symmetricRemainder| 164 |squareFree| 170 |retractIfCan|
             175 |retract| 180 |rationalIfCan| 185 |rational?| 190
             |rational| 195 |prime?| 200 |powmod| 205 |positive?| 212
             |permutation| 217 |patternMatch| 223 |nextItem| 230 |mask|
             235 |invmod| 240 |init| 246 |factorial| 250 |factor| 255
             |even?| 260 |euclideanSize| 265 |differentiate| 270 |copy|
             275 |convert| 280 |characteristic| 300 |bit?| 304
             |binomial| 310)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 85
                                '(0 6 0 9 1 6 11 0 12 2 6 11 0 0 14 1 6
                                  0 0 17 2 6 0 0 0 18 0 6 0 20 1 6 0 0
                                  21 2 6 11 0 0 24 1 6 25 0 26 1 28 0
                                  25 29 1 33 0 25 34 1 37 0 25 38 1 41
                                  40 6 42 1 41 40 6 45 1 47 11 6 48 1
                                  50 6 6 51 2 50 6 6 6 53 2 50 6 6 6 55
                                  1 6 11 0 60 2 6 0 0 0 61 3 65 64 6 37
                                  64 66 1 69 0 25 70 2 6 0 0 0 74 2 6 0
                                  75 0 76 2 6 0 0 0 77 1 6 11 0 79 2 6
                                  0 0 0 80 2 6 0 0 0 81 2 6 0 0 0 82 3
                                  6 0 0 0 0 84 2 0 0 0 0 78 1 0 43 0 46
                                  1 0 57 0 58 1 0 25 0 36 1 0 72 0 73 1
                                  0 11 0 23 1 0 69 0 71 1 0 11 0 49 3 0
                                  0 0 0 0 85 1 0 11 0 15 2 0 0 0 0 56 3
                                  0 67 0 37 67 68 1 0 62 0 63 1 0 0 0
                                  22 2 0 0 0 0 83 0 0 0 59 1 0 0 0 52 1
                                  0 43 0 44 1 0 11 0 13 1 0 7 0 27 1 0
                                  0 0 10 1 0 0 0 16 1 0 31 0 32 1 0 28
                                  0 30 1 0 37 0 39 1 0 33 0 35 0 0 7 8
                                  2 0 11 0 0 19 2 0 0 0 0 54)))))
          '|lookupComplete|)) 
