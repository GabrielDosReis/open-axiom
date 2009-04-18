
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

(DEFUN |INS-;differentiate;2S;2| (|x| $) (|spadConstant| $ 10)) 

(DEFUN |INS-;even?;SB;3| (|x| $)
  (NOT (SPADCALL |x| (|getShellEntry| $ 13)))) 

(DEFUN |INS-;positive?;SB;4| (|x| $)
  (SPADCALL (|spadConstant| $ 10) |x| (|getShellEntry| $ 15))) 

(DEFUN |INS-;copy;2S;5| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INS-;bit?;2SB;6| (|x| |i| $)
  (SPADCALL
      (SPADCALL |x| (SPADCALL |i| (|getShellEntry| $ 18))
          (|getShellEntry| $ 19))
      (|getShellEntry| $ 13))) 

(DEFUN |INS-;mask;2S;7| (|n| $)
  (SPADCALL (SPADCALL (|spadConstant| $ 21) |n| (|getShellEntry| $ 19))
      (|getShellEntry| $ 22))) 

(DEFUN |INS-;rational?;SB;8| (|x| $) (DECLARE (IGNORE $)) 'T) 

(DEFUN |INS-;euclideanSize;SNni;9| (|x| $)
  (PROG (#0=#:G1426 #1=#:G1427)
    (RETURN
      (COND
        ((SPADCALL |x| (|spadConstant| $ 10) (|getShellEntry| $ 26))
         (|error| "euclideanSize called on zero"))
        ((SPADCALL |x| (|spadConstant| $ 10) (|getShellEntry| $ 15))
         (PROG1 (LETT #0# (- (SPADCALL |x| (|getShellEntry| $ 28)))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (COND ((< #0# 0) 'NIL) ('T 'T))
               '(|NonNegativeInteger|) #0#)))
        ('T
         (PROG1 (LETT #1# (SPADCALL |x| (|getShellEntry| $ 28))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (COND ((< #1# 0) 'NIL) ('T 'T))
               '(|NonNegativeInteger|) #1#))))))) 

(DEFUN |INS-;convert;SF;10| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 28))
      (|getShellEntry| $ 32))) 

(DEFUN |INS-;convert;SDf;11| (|x| $)
  (FLOAT (SPADCALL |x| (|getShellEntry| $ 28)) |$DoubleFloatMaximum|)) 

(DEFUN |INS-;convert;SIf;12| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 28))
      (|getShellEntry| $ 38))) 

(DEFUN |INS-;retract;SI;13| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 28))) 

(DEFUN |INS-;convert;SP;14| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 28))
      (|getShellEntry| $ 42))) 

(DEFUN |INS-;factor;SF;15| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 46))) 

(DEFUN |INS-;squareFree;SF;16| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 49))) 

(DEFUN |INS-;prime?;SB;17| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 52))) 

(DEFUN |INS-;factorial;2S;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 55))) 

(DEFUN |INS-;binomial;3S;19| (|n| |m| $)
  (SPADCALL |n| |m| (|getShellEntry| $ 57))) 

(DEFUN |INS-;permutation;3S;20| (|n| |m| $)
  (SPADCALL |n| |m| (|getShellEntry| $ 59))) 

(DEFUN |INS-;retractIfCan;SU;21| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 28)))) 

(DEFUN |INS-;init;S;22| ($) (|spadConstant| $ 10)) 

(DEFUN |INS-;nextItem;SU;23| (|n| $)
  (COND
    ((SPADCALL |n| (|getShellEntry| $ 64))
     (CONS 0 (|spadConstant| $ 21)))
    ((SPADCALL (|spadConstant| $ 10) |n| (|getShellEntry| $ 15))
     (CONS 0 (SPADCALL |n| (|getShellEntry| $ 18))))
    ('T
     (CONS 0
           (SPADCALL (|spadConstant| $ 21) |n| (|getShellEntry| $ 65)))))) 

(DEFUN |INS-;patternMatch;SP2Pmr;24| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 70))) 

(DEFUN |INS-;rational;SF;25| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 28))
      (|getShellEntry| $ 74))) 

(DEFUN |INS-;rationalIfCan;SU;26| (|x| $)
  (CONS 0
        (SPADCALL (SPADCALL |x| (|getShellEntry| $ 28))
            (|getShellEntry| $ 74)))) 

(DEFUN |INS-;symmetricRemainder;3S;27| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (SPADCALL |x| |n| (|getShellEntry| $ 78))
                 |INS-;symmetricRemainder;3S;27|)
           (EXIT (COND
                   ((SPADCALL |r| (|spadConstant| $ 10)
                        (|getShellEntry| $ 26))
                    |r|)
                   ('T
                    (SEQ (COND
                           ((SPADCALL |n| (|spadConstant| $ 10)
                                (|getShellEntry| $ 15))
                            (LETT |n|
                                  (SPADCALL |n| (|getShellEntry| $ 18))
                                  |INS-;symmetricRemainder;3S;27|)))
                         (EXIT (COND
                                 ((SPADCALL (|spadConstant| $ 10) |r|
                                      (|getShellEntry| $ 15))
                                  (COND
                                    ((SPADCALL |n|
                                      (SPADCALL 2 |r|
                                       (|getShellEntry| $ 80))
                                      (|getShellEntry| $ 15))
                                     (SPADCALL |r| |n|
                                      (|getShellEntry| $ 65)))
                                    ('T |r|)))
                                 ((NOT (SPADCALL (|spadConstant| $ 10)
                                        (SPADCALL
                                         (SPADCALL 2 |r|
                                          (|getShellEntry| $ 80))
                                         |n| (|getShellEntry| $ 81))
                                        (|getShellEntry| $ 15)))
                                  (SPADCALL |r| |n|
                                      (|getShellEntry| $ 81)))
                                 ('T |r|))))))))))) 

(DEFUN |INS-;invmod;3S;28| (|a| |b| $)
  (PROG (|q| |r| |r1| |c| |c1| |d| |d1|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|getShellEntry| $ 83))
              (LETT |a| (SPADCALL |a| |b| (|getShellEntry| $ 84))
                    |INS-;invmod;3S;28|)))
           (LETT |c| |a| |INS-;invmod;3S;28|)
           (LETT |c1| (|spadConstant| $ 21) |INS-;invmod;3S;28|)
           (LETT |d| |b| |INS-;invmod;3S;28|)
           (LETT |d1| (|spadConstant| $ 10) |INS-;invmod;3S;28|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |d| (|getShellEntry| $ 64))))
                   (GO G191)))
                (SEQ (LETT |q|
                           (SPADCALL |c| |d| (|getShellEntry| $ 85))
                           |INS-;invmod;3S;28|)
                     (LETT |r|
                           (SPADCALL |c|
                               (SPADCALL |q| |d|
                                   (|getShellEntry| $ 86))
                               (|getShellEntry| $ 65))
                           |INS-;invmod;3S;28|)
                     (LETT |r1|
                           (SPADCALL |c1|
                               (SPADCALL |q| |d1|
                                   (|getShellEntry| $ 86))
                               (|getShellEntry| $ 65))
                           |INS-;invmod;3S;28|)
                     (LETT |c| |d| |INS-;invmod;3S;28|)
                     (LETT |c1| |d1| |INS-;invmod;3S;28|)
                     (LETT |d| |r| |INS-;invmod;3S;28|)
                     (EXIT (LETT |d1| |r1| |INS-;invmod;3S;28|)))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NOT (SPADCALL |c| (|getShellEntry| $ 87)))
              (EXIT (|error| "inverse does not exist"))))
           (EXIT (COND
                   ((SPADCALL |c1| (|getShellEntry| $ 83))
                    (SPADCALL |c1| |b| (|getShellEntry| $ 81)))
                   ('T |c1|))))))) 

(DEFUN |INS-;powmod;4S;29| (|x| |n| |p| $)
  (PROG (|y| #0=#:G1484 |z|)
    (RETURN
      (SEQ (EXIT (SEQ (COND
                        ((SPADCALL |x| (|getShellEntry| $ 83))
                         (LETT |x|
                               (SPADCALL |x| |p|
                                   (|getShellEntry| $ 84))
                               |INS-;powmod;4S;29|)))
                      (EXIT (COND
                              ((SPADCALL |x| (|getShellEntry| $ 64))
                               (|spadConstant| $ 10))
                              ((SPADCALL |n| (|getShellEntry| $ 64))
                               (|spadConstant| $ 21))
                              ('T
                               (SEQ (LETT |y| (|spadConstant| $ 21)
                                     |INS-;powmod;4S;29|)
                                    (LETT |z| |x| |INS-;powmod;4S;29|)
                                    (EXIT
                                     (SEQ G190 NIL
                                      (SEQ
                                       (COND
                                         ((SPADCALL |n|
                                           (|getShellEntry| $ 13))
                                          (LETT |y|
                                           (SPADCALL |y| |z| |p|
                                            (|getShellEntry| $ 89))
                                           |INS-;powmod;4S;29|)))
                                       (EXIT
                                        (COND
                                          ((SPADCALL
                                            (LETT |n|
                                             (SPADCALL |n|
                                              (SPADCALL
                                               (|spadConstant| $ 21)
                                               (|getShellEntry| $ 18))
                                              (|getShellEntry| $ 19))
                                             |INS-;powmod;4S;29|)
                                            (|getShellEntry| $ 64))
                                           (PROGN
                                             (LETT #0# |y|
                                              |INS-;powmod;4S;29|)
                                             (GO #0#)))
                                          ('T
                                           (LETT |z|
                                            (SPADCALL |z| |z| |p|
                                             (|getShellEntry| $ 89))
                                            |INS-;powmod;4S;29|)))))
                                      NIL (GO G190) G191 (EXIT NIL)))))))))
           #0# (EXIT #0#))))) 

(DEFUN |IntegerNumberSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IntegerNumberSystem&|))
        (LETT |dv$| (LIST '|IntegerNumberSystem&| |dv$1|) . #0#)
        (LETT $ (|newShell| 91) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|IntegerNumberSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (0 . |Zero|)
             |INS-;characteristic;Nni;1| (4 . |Zero|)
             |INS-;differentiate;2S;2| (|Boolean|) (8 . |odd?|)
             |INS-;even?;SB;3| (13 . <) |INS-;positive?;SB;4|
             |INS-;copy;2S;5| (19 . -) (24 . |shift|) |INS-;bit?;2SB;6|
             (30 . |One|) (34 . |dec|) |INS-;mask;2S;7| (39 . |true|)
             |INS-;rational?;SB;8| (43 . =) (|Integer|)
             (49 . |convert|) (54 . -) |INS-;euclideanSize;SNni;9|
             (|Float|) (59 . |coerce|) |INS-;convert;SF;10|
             (|DoubleFloat|) (64 . |coerce|) |INS-;convert;SDf;11|
             (|InputForm|) (69 . |convert|) |INS-;convert;SIf;12|
             |INS-;retract;SI;13| (|Pattern| 27) (74 . |coerce|)
             |INS-;convert;SP;14| (|Factored| 6)
             (|IntegerFactorizationPackage| 6) (79 . |factor|)
             (|Factored| $) |INS-;factor;SF;15| (84 . |squareFree|)
             |INS-;squareFree;SF;16| (|IntegerPrimesPackage| 6)
             (89 . |prime?|) |INS-;prime?;SB;17|
             (|IntegerCombinatoricFunctions| 6) (94 . |factorial|)
             |INS-;factorial;2S;18| (99 . |binomial|)
             |INS-;binomial;3S;19| (105 . |permutation|)
             |INS-;permutation;3S;20| (|Union| 27 '"failed")
             |INS-;retractIfCan;SU;21| |INS-;init;S;22| (111 . |zero?|)
             (116 . -) (|Union| $ '"failed") |INS-;nextItem;SU;23|
             (|PatternMatchResult| 27 6)
             (|PatternMatchIntegerNumberSystem| 6)
             (122 . |patternMatch|) (|PatternMatchResult| 27 $)
             |INS-;patternMatch;SP2Pmr;24| (|Fraction| 27)
             (129 . |coerce|) |INS-;rational;SF;25|
             (|Union| 73 '"failed") |INS-;rationalIfCan;SU;26|
             (134 . |rem|) (|PositiveInteger|) (140 . *) (146 . +)
             |INS-;symmetricRemainder;3S;27| (152 . |negative?|)
             (157 . |positiveRemainder|) (163 . |quo|) (169 . *)
             (175 . |one?|) |INS-;invmod;3S;28| (180 . |mulmod|)
             |INS-;powmod;4S;29|)
          '#(|symmetricRemainder| 187 |squareFree| 193 |retractIfCan|
             198 |retract| 203 |rationalIfCan| 208 |rational?| 213
             |rational| 218 |prime?| 223 |powmod| 228 |positive?| 235
             |permutation| 240 |patternMatch| 246 |nextItem| 253 |mask|
             258 |invmod| 263 |init| 269 |factorial| 273 |factor| 278
             |even?| 283 |euclideanSize| 288 |differentiate| 293 |copy|
             298 |convert| 303 |characteristic| 323 |bit?| 327
             |binomial| 333)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 90
                                '(0 7 0 8 0 6 0 10 1 6 12 0 13 2 6 12 0
                                  0 15 1 6 0 0 18 2 6 0 0 0 19 0 6 0 21
                                  1 6 0 0 22 0 12 0 24 2 6 12 0 0 26 1
                                  6 27 0 28 1 27 0 0 29 1 31 0 27 32 1
                                  34 0 27 35 1 37 0 27 38 1 41 0 27 42
                                  1 45 44 6 46 1 45 44 6 49 1 51 12 6
                                  52 1 54 6 6 55 2 54 6 6 6 57 2 54 6 6
                                  6 59 1 6 12 0 64 2 6 0 0 0 65 3 69 68
                                  6 41 68 70 1 73 0 27 74 2 6 0 0 0 78
                                  2 6 0 79 0 80 2 6 0 0 0 81 1 6 12 0
                                  83 2 6 0 0 0 84 2 6 0 0 0 85 2 6 0 0
                                  0 86 1 6 12 0 87 3 6 0 0 0 0 89 2 0 0
                                  0 0 82 1 0 47 0 50 1 0 61 0 62 1 0 27
                                  0 40 1 0 76 0 77 1 0 12 0 25 1 0 73 0
                                  75 1 0 12 0 53 3 0 0 0 0 0 90 1 0 12
                                  0 16 2 0 0 0 0 60 3 0 71 0 41 71 72 1
                                  0 66 0 67 1 0 0 0 23 2 0 0 0 0 88 0 0
                                  0 63 1 0 0 0 56 1 0 47 0 48 1 0 12 0
                                  14 1 0 7 0 30 1 0 0 0 11 1 0 0 0 17 1
                                  0 34 0 36 1 0 31 0 33 1 0 41 0 43 1 0
                                  37 0 39 0 0 7 9 2 0 12 0 0 20 2 0 0 0
                                  0 58)))))
          '|lookupComplete|)) 
