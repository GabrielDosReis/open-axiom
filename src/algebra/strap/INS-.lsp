
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

(PUT '|INS-;rational?;SB;8| '|SPADreplace| '(XLAM (|x|) T)) 

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
  (NOT (SPADCALL |x| (|shellEntry| $ 13)))) 

(DEFUN |INS-;positive?;SB;4| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 10) (|shellEntry| $ 16))) 

(DEFUN |INS-;copy;2S;5| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INS-;bit?;2SB;6| (|x| |i| $)
  (SPADCALL
      (SPADCALL |x| (SPADCALL |i| (|shellEntry| $ 19))
          (|shellEntry| $ 20))
      (|shellEntry| $ 13))) 

(DEFUN |INS-;mask;2S;7| (|n| $)
  (SPADCALL (SPADCALL (|spadConstant| $ 22) |n| (|shellEntry| $ 20))
      (|shellEntry| $ 23))) 

(DEFUN |INS-;rational?;SB;8| (|x| $) (DECLARE (IGNORE $)) T) 

(DEFUN |INS-;euclideanSize;SNni;9| (|x| $)
  (COND
    ((SPADCALL |x| (|spadConstant| $ 10) (|shellEntry| $ 27))
     (|error| "euclideanSize called on zero"))
    ((SPADCALL |x| (|spadConstant| $ 10) (|shellEntry| $ 28))
     (LET ((#0=#:G1401 (- (SPADCALL |x| (|shellEntry| $ 30)))))
       (|check-subtype| (NOT (MINUSP #0#)) '(|NonNegativeInteger|) #0#)))
    (T (LET ((#1=#:G1402 (SPADCALL |x| (|shellEntry| $ 30))))
         (|check-subtype| (NOT (MINUSP #1#)) '(|NonNegativeInteger|)
             #1#))))) 

(DEFUN |INS-;convert;SF;10| (|x| $)
  (SPADCALL (SPADCALL |x| (|shellEntry| $ 30)) (|shellEntry| $ 34))) 

(DEFUN |INS-;convert;SDf;11| (|x| $)
  (FLOAT (SPADCALL |x| (|shellEntry| $ 30)) |$DoubleFloatMaximum|)) 

(DEFUN |INS-;convert;SIf;12| (|x| $)
  (SPADCALL (SPADCALL |x| (|shellEntry| $ 30)) (|shellEntry| $ 40))) 

(DEFUN |INS-;retract;SI;13| (|x| $)
  (SPADCALL |x| (|shellEntry| $ 30))) 

(DEFUN |INS-;convert;SP;14| (|x| $)
  (SPADCALL (SPADCALL |x| (|shellEntry| $ 30)) (|shellEntry| $ 44))) 

(DEFUN |INS-;factor;SF;15| (|x| $) (SPADCALL |x| (|shellEntry| $ 48))) 

(DEFUN |INS-;squareFree;SF;16| (|x| $)
  (SPADCALL |x| (|shellEntry| $ 51))) 

(DEFUN |INS-;prime?;SB;17| (|x| $) (SPADCALL |x| (|shellEntry| $ 54))) 

(DEFUN |INS-;factorial;2S;18| (|x| $)
  (SPADCALL |x| (|shellEntry| $ 57))) 

(DEFUN |INS-;binomial;3S;19| (|n| |m| $)
  (SPADCALL |n| |m| (|shellEntry| $ 59))) 

(DEFUN |INS-;permutation;3S;20| (|n| |m| $)
  (SPADCALL |n| |m| (|shellEntry| $ 61))) 

(DEFUN |INS-;retractIfCan;SU;21| (|x| $)
  (CONS 0 (SPADCALL |x| (|shellEntry| $ 30)))) 

(DEFUN |INS-;init;S;22| ($) (|spadConstant| $ 10)) 

(DEFUN |INS-;nextItem;SU;23| (|n| $)
  (COND
    ((SPADCALL |n| (|shellEntry| $ 66)) (CONS 0 (|spadConstant| $ 22)))
    ((SPADCALL |n| (|spadConstant| $ 10) (|shellEntry| $ 16))
     (CONS 0 (SPADCALL |n| (|shellEntry| $ 19))))
    (T (CONS 0
             (SPADCALL (|spadConstant| $ 22) |n| (|shellEntry| $ 67)))))) 

(DEFUN |INS-;patternMatch;SP2Pmr;24| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|shellEntry| $ 72))) 

(DEFUN |INS-;rational;SF;25| (|x| $)
  (SPADCALL (SPADCALL |x| (|shellEntry| $ 30)) (|shellEntry| $ 76))) 

(DEFUN |INS-;rationalIfCan;SU;26| (|x| $)
  (CONS 0
        (SPADCALL (SPADCALL |x| (|shellEntry| $ 30))
            (|shellEntry| $ 76)))) 

(DEFUN |INS-;symmetricRemainder;3S;27| (|x| |n| $)
  (LET ((|r| (SPADCALL |x| |n| (|shellEntry| $ 80))))
    (COND
      ((SPADCALL |r| (|spadConstant| $ 10) (|shellEntry| $ 27)) |r|)
      (T (SEQ (COND
                ((SPADCALL |n| (|spadConstant| $ 10)
                     (|shellEntry| $ 28))
                 (SETQ |n| (SPADCALL |n| (|shellEntry| $ 19)))))
              (EXIT (COND
                      ((SPADCALL |r| (|spadConstant| $ 10)
                           (|shellEntry| $ 16))
                       (COND
                         ((SPADCALL
                              (SPADCALL 2 |r| (|shellEntry| $ 82)) |n|
                              (|shellEntry| $ 16))
                          (SPADCALL |r| |n| (|shellEntry| $ 67)))
                         (T |r|)))
                      ((SPADCALL
                           (SPADCALL (SPADCALL 2 |r|
                                      (|shellEntry| $ 82))
                                     |n| (|shellEntry| $ 83))
                           (|spadConstant| $ 10) (|shellEntry| $ 84))
                       (SPADCALL |r| |n| (|shellEntry| $ 83)))
                      (T |r|)))))))) 

(DEFUN |INS-;invmod;3S;28| (|a| |b| $)
  (PROG (|c| |c1| |d| |d1| |q| |r| |r1|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|shellEntry| $ 86))
              (SETQ |a| (SPADCALL |a| |b| (|shellEntry| $ 87)))))
           (LETT |c| |a| |INS-;invmod;3S;28|)
           (LETT |c1| (|spadConstant| $ 22) |INS-;invmod;3S;28|)
           (LETT |d| |b| |INS-;invmod;3S;28|)
           (LETT |d1| (|spadConstant| $ 10) |INS-;invmod;3S;28|)
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |d| (|shellEntry| $ 66))))
                (RETURN NIL))
               (T (SEQ (LETT |q| (SPADCALL |c| |d| (|shellEntry| $ 88))
                             |INS-;invmod;3S;28|)
                       (LETT |r|
                             (SPADCALL |c|
                                 (SPADCALL |q| |d| (|shellEntry| $ 89))
                                 (|shellEntry| $ 67))
                             |INS-;invmod;3S;28|)
                       (LETT |r1|
                             (SPADCALL |c1|
                                 (SPADCALL |q| |d1|
                                     (|shellEntry| $ 89))
                                 (|shellEntry| $ 67))
                             |INS-;invmod;3S;28|)
                       (SETQ |c| |d|) (SETQ |c1| |d1|) (SETQ |d| |r|)
                       (EXIT (SETQ |d1| |r1|))))))
           (COND
             ((NOT (SPADCALL |c| (|shellEntry| $ 90)))
              (EXIT (|error| "inverse does not exist"))))
           (EXIT (COND
                   ((SPADCALL |c1| (|shellEntry| $ 86))
                    (SPADCALL |c1| |b| (|shellEntry| $ 83)))
                   (T |c1|))))))) 

(DEFUN |INS-;powmod;4S;29| (|x| |n| |p| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (|shellEntry| $ 86))
              (SETQ |x| (SPADCALL |x| |p| (|shellEntry| $ 87)))))
           (EXIT (COND
                   ((SPADCALL |x| (|shellEntry| $ 66))
                    (|spadConstant| $ 10))
                   ((SPADCALL |n| (|shellEntry| $ 66))
                    (|spadConstant| $ 22))
                   (T (SEQ (LETT |y| (|spadConstant| $ 22)
                                 |INS-;powmod;4S;29|)
                           (LETT |z| |x| |INS-;powmod;4S;29|)
                           (EXIT (LOOP
                                   (COND
                                     (NIL (RETURN NIL))
                                     (T
                                      (SEQ
                                       (COND
                                         ((SPADCALL |n|
                                           (|shellEntry| $ 13))
                                          (SETQ |y|
                                           (SPADCALL |y| |z| |p|
                                            (|shellEntry| $ 92)))))
                                       (EXIT
                                        (COND
                                          ((SPADCALL
                                            (SETQ |n|
                                             (SPADCALL |n|
                                              (SPADCALL
                                               (|spadConstant| $ 22)
                                               (|shellEntry| $ 19))
                                              (|shellEntry| $ 20)))
                                            (|shellEntry| $ 66))
                                           (RETURN-FROM
                                            |INS-;powmod;4S;29|
                                             |y|))
                                          (T
                                           (SETQ |z|
                                            (SPADCALL |z| |z| |p|
                                             (|shellEntry| $ 92))))))))))))))))))) 

(DEFUN |IntegerNumberSystem&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|IntegerNumberSystem&| |dv$1|))
         ($ (|newShell| 94)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    $)) 

(MAKEPROP '|IntegerNumberSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (0 . |Zero|)
             |INS-;characteristic;Nni;1| (4 . |Zero|)
             |INS-;differentiate;2S;2| (|Boolean|) (8 . |odd?|)
             (13 . |not|) |INS-;even?;SB;3| (18 . >)
             |INS-;positive?;SB;4| |INS-;copy;2S;5| (24 . -)
             (29 . |shift|) |INS-;bit?;2SB;6| (35 . |One|) (39 . |dec|)
             |INS-;mask;2S;7| (44 . |true|) |INS-;rational?;SB;8|
             (48 . =) (54 . <) (|Integer|) (60 . |convert|) (65 . -)
             |INS-;euclideanSize;SNni;9| (|Float|) (70 . |coerce|)
             |INS-;convert;SF;10| (|DoubleFloat|) (75 . |coerce|)
             |INS-;convert;SDf;11| (|InputForm|) (80 . |convert|)
             |INS-;convert;SIf;12| |INS-;retract;SI;13| (|Pattern| 29)
             (85 . |coerce|) |INS-;convert;SP;14| (|Factored| 6)
             (|IntegerFactorizationPackage| 6) (90 . |factor|)
             (|Factored| $) |INS-;factor;SF;15| (95 . |squareFree|)
             |INS-;squareFree;SF;16| (|IntegerPrimesPackage| 6)
             (100 . |prime?|) |INS-;prime?;SB;17|
             (|IntegerCombinatoricFunctions| 6) (105 . |factorial|)
             |INS-;factorial;2S;18| (110 . |binomial|)
             |INS-;binomial;3S;19| (116 . |permutation|)
             |INS-;permutation;3S;20| (|Union| 29 '"failed")
             |INS-;retractIfCan;SU;21| |INS-;init;S;22| (122 . |zero?|)
             (127 . -) (|Union| $ '"failed") |INS-;nextItem;SU;23|
             (|PatternMatchResult| 29 6)
             (|PatternMatchIntegerNumberSystem| 6)
             (133 . |patternMatch|) (|PatternMatchResult| 29 $)
             |INS-;patternMatch;SP2Pmr;24| (|Fraction| 29)
             (140 . |coerce|) |INS-;rational;SF;25|
             (|Union| 75 '"failed") |INS-;rationalIfCan;SU;26|
             (145 . |rem|) (|PositiveInteger|) (151 . *) (157 . +)
             (163 . <=) |INS-;symmetricRemainder;3S;27|
             (169 . |negative?|) (174 . |positiveRemainder|)
             (180 . |quo|) (186 . *) (192 . |one?|) |INS-;invmod;3S;28|
             (197 . |mulmod|) |INS-;powmod;4S;29|)
          '#(|symmetricRemainder| 204 |squareFree| 210 |retractIfCan|
             215 |retract| 220 |rationalIfCan| 225 |rational?| 230
             |rational| 235 |prime?| 240 |powmod| 245 |positive?| 252
             |permutation| 257 |patternMatch| 263 |nextItem| 270 |mask|
             275 |invmod| 280 |init| 286 |factorial| 290 |factor| 295
             |even?| 300 |euclideanSize| 305 |differentiate| 310 |copy|
             315 |convert| 320 |characteristic| 340 |bit?| 344
             |binomial| 350)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 93
                                '(0 7 0 8 0 6 0 10 1 6 12 0 13 1 12 0 0
                                  14 2 6 12 0 0 16 1 6 0 0 19 2 6 0 0 0
                                  20 0 6 0 22 1 6 0 0 23 0 12 0 25 2 6
                                  12 0 0 27 2 6 12 0 0 28 1 6 29 0 30 1
                                  29 0 0 31 1 33 0 29 34 1 36 0 29 37 1
                                  39 0 29 40 1 43 0 29 44 1 47 46 6 48
                                  1 47 46 6 51 1 53 12 6 54 1 56 6 6 57
                                  2 56 6 6 6 59 2 56 6 6 6 61 1 6 12 0
                                  66 2 6 0 0 0 67 3 71 70 6 43 70 72 1
                                  75 0 29 76 2 6 0 0 0 80 2 6 0 81 0 82
                                  2 6 0 0 0 83 2 6 12 0 0 84 1 6 12 0
                                  86 2 6 0 0 0 87 2 6 0 0 0 88 2 6 0 0
                                  0 89 1 6 12 0 90 3 6 0 0 0 0 92 2 0 0
                                  0 0 85 1 0 49 0 52 1 0 63 0 64 1 0 29
                                  0 42 1 0 78 0 79 1 0 12 0 26 1 0 75 0
                                  77 1 0 12 0 55 3 0 0 0 0 0 93 1 0 12
                                  0 17 2 0 0 0 0 62 3 0 73 0 43 73 74 1
                                  0 68 0 69 1 0 0 0 24 2 0 0 0 0 91 0 0
                                  0 65 1 0 0 0 58 1 0 49 0 50 1 0 12 0
                                  15 1 0 7 0 32 1 0 0 0 11 1 0 0 0 18 1
                                  0 36 0 38 1 0 33 0 35 1 0 43 0 45 1 0
                                  39 0 41 0 0 7 9 2 0 12 0 0 21 2 0 0 0
                                  0 60)))))
          '|lookupComplete|)) 
