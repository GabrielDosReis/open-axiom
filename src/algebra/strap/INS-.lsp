
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
  (SPADCALL (|spadConstant| $ 10) |x| (|getShellEntry| $ 16))) 

(DEFUN |INS-;copy;2S;5| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INS-;bit?;2SB;6| (|x| |i| $)
  (SPADCALL
      (SPADCALL |x| (SPADCALL |i| (|getShellEntry| $ 19))
          (|getShellEntry| $ 20))
      (|getShellEntry| $ 13))) 

(DEFUN |INS-;mask;2S;7| (|n| $)
  (SPADCALL (SPADCALL (|spadConstant| $ 22) |n| (|getShellEntry| $ 20))
      (|getShellEntry| $ 23))) 

(DEFUN |INS-;rational?;SB;8| (|x| $) (DECLARE (IGNORE $)) 'T) 

(DEFUN |INS-;euclideanSize;SNni;9| (|x| $)
  (PROG (#0=#:G1426 #1=#:G1427)
    (RETURN
      (COND
        ((SPADCALL |x| (|spadConstant| $ 10) (|getShellEntry| $ 27))
         (|error| "euclideanSize called on zero"))
        ((SPADCALL |x| (|spadConstant| $ 10) (|getShellEntry| $ 16))
         (PROG1 (LETT #0# (- (SPADCALL |x| (|getShellEntry| $ 29)))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (COND ((< #0# 0) 'NIL) ('T 'T))
               '(|NonNegativeInteger|) #0#)))
        ('T
         (PROG1 (LETT #1# (SPADCALL |x| (|getShellEntry| $ 29))
                      |INS-;euclideanSize;SNni;9|)
           (|check-subtype| (COND ((< #1# 0) 'NIL) ('T 'T))
               '(|NonNegativeInteger|) #1#))))))) 

(DEFUN |INS-;convert;SF;10| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 29))
      (|getShellEntry| $ 33))) 

(DEFUN |INS-;convert;SDf;11| (|x| $)
  (FLOAT (SPADCALL |x| (|getShellEntry| $ 29)) |$DoubleFloatMaximum|)) 

(DEFUN |INS-;convert;SIf;12| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 29))
      (|getShellEntry| $ 39))) 

(DEFUN |INS-;retract;SI;13| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 29))) 

(DEFUN |INS-;convert;SP;14| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 29))
      (|getShellEntry| $ 43))) 

(DEFUN |INS-;factor;SF;15| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 47))) 

(DEFUN |INS-;squareFree;SF;16| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 50))) 

(DEFUN |INS-;prime?;SB;17| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 53))) 

(DEFUN |INS-;factorial;2S;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 56))) 

(DEFUN |INS-;binomial;3S;19| (|n| |m| $)
  (SPADCALL |n| |m| (|getShellEntry| $ 58))) 

(DEFUN |INS-;permutation;3S;20| (|n| |m| $)
  (SPADCALL |n| |m| (|getShellEntry| $ 60))) 

(DEFUN |INS-;retractIfCan;SU;21| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 29)))) 

(DEFUN |INS-;init;S;22| ($) (|spadConstant| $ 10)) 

(DEFUN |INS-;nextItem;SU;23| (|n| $)
  (COND
    ((SPADCALL |n| (|getShellEntry| $ 65))
     (CONS 0 (|spadConstant| $ 22)))
    ((SPADCALL (|spadConstant| $ 10) |n| (|getShellEntry| $ 16))
     (CONS 0 (SPADCALL |n| (|getShellEntry| $ 19))))
    ('T
     (CONS 0
           (SPADCALL (|spadConstant| $ 22) |n| (|getShellEntry| $ 66)))))) 

(DEFUN |INS-;patternMatch;SP2Pmr;24| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 71))) 

(DEFUN |INS-;rational;SF;25| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 29))
      (|getShellEntry| $ 75))) 

(DEFUN |INS-;rationalIfCan;SU;26| (|x| $)
  (CONS 0
        (SPADCALL (SPADCALL |x| (|getShellEntry| $ 29))
            (|getShellEntry| $ 75)))) 

(DEFUN |INS-;symmetricRemainder;3S;27| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (SPADCALL |x| |n| (|getShellEntry| $ 79))
                 |INS-;symmetricRemainder;3S;27|)
           (EXIT (COND
                   ((SPADCALL |r| (|spadConstant| $ 10)
                        (|getShellEntry| $ 27))
                    |r|)
                   ('T
                    (SEQ (COND
                           ((SPADCALL |n| (|spadConstant| $ 10)
                                (|getShellEntry| $ 16))
                            (LETT |n|
                                  (SPADCALL |n| (|getShellEntry| $ 19))
                                  |INS-;symmetricRemainder;3S;27|)))
                         (EXIT (COND
                                 ((SPADCALL (|spadConstant| $ 10) |r|
                                      (|getShellEntry| $ 16))
                                  (COND
                                    ((SPADCALL |n|
                                      (SPADCALL 2 |r|
                                       (|getShellEntry| $ 81))
                                      (|getShellEntry| $ 16))
                                     (SPADCALL |r| |n|
                                      (|getShellEntry| $ 66)))
                                    ('T |r|)))
                                 ((NOT (SPADCALL (|spadConstant| $ 10)
                                        (SPADCALL
                                         (SPADCALL 2 |r|
                                          (|getShellEntry| $ 81))
                                         |n| (|getShellEntry| $ 82))
                                        (|getShellEntry| $ 16)))
                                  (SPADCALL |r| |n|
                                      (|getShellEntry| $ 82)))
                                 ('T |r|))))))))))) 

(DEFUN |INS-;invmod;3S;28| (|a| |b| $)
  (PROG (|q| |r| |r1| |c| |c1| |d| |d1|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|getShellEntry| $ 84))
              (LETT |a| (SPADCALL |a| |b| (|getShellEntry| $ 85))
                    |INS-;invmod;3S;28|)))
           (LETT |c| |a| |INS-;invmod;3S;28|)
           (LETT |c1| (|spadConstant| $ 22) |INS-;invmod;3S;28|)
           (LETT |d| |b| |INS-;invmod;3S;28|)
           (LETT |d1| (|spadConstant| $ 10) |INS-;invmod;3S;28|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |d| (|getShellEntry| $ 65))))
                   (GO G191)))
                (SEQ (LETT |q|
                           (SPADCALL |c| |d| (|getShellEntry| $ 86))
                           |INS-;invmod;3S;28|)
                     (LETT |r|
                           (SPADCALL |c|
                               (SPADCALL |q| |d|
                                   (|getShellEntry| $ 87))
                               (|getShellEntry| $ 66))
                           |INS-;invmod;3S;28|)
                     (LETT |r1|
                           (SPADCALL |c1|
                               (SPADCALL |q| |d1|
                                   (|getShellEntry| $ 87))
                               (|getShellEntry| $ 66))
                           |INS-;invmod;3S;28|)
                     (LETT |c| |d| |INS-;invmod;3S;28|)
                     (LETT |c1| |d1| |INS-;invmod;3S;28|)
                     (LETT |d| |r| |INS-;invmod;3S;28|)
                     (EXIT (LETT |d1| |r1| |INS-;invmod;3S;28|)))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NOT (SPADCALL |c| (|getShellEntry| $ 88)))
              (EXIT (|error| "inverse does not exist"))))
           (EXIT (COND
                   ((SPADCALL |c1| (|getShellEntry| $ 84))
                    (SPADCALL |c1| |b| (|getShellEntry| $ 82)))
                   ('T |c1|))))))) 

(DEFUN |INS-;powmod;4S;29| (|x| |n| |p| $)
  (PROG (|y| #0=#:G1484 |z|)
    (RETURN
      (SEQ (EXIT (SEQ (COND
                        ((SPADCALL |x| (|getShellEntry| $ 84))
                         (LETT |x|
                               (SPADCALL |x| |p|
                                   (|getShellEntry| $ 85))
                               |INS-;powmod;4S;29|)))
                      (EXIT (COND
                              ((SPADCALL |x| (|getShellEntry| $ 65))
                               (|spadConstant| $ 10))
                              ((SPADCALL |n| (|getShellEntry| $ 65))
                               (|spadConstant| $ 22))
                              ('T
                               (SEQ (LETT |y| (|spadConstant| $ 22)
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
                                            (|getShellEntry| $ 90))
                                           |INS-;powmod;4S;29|)))
                                       (EXIT
                                        (COND
                                          ((SPADCALL
                                            (LETT |n|
                                             (SPADCALL |n|
                                              (SPADCALL
                                               (|spadConstant| $ 22)
                                               (|getShellEntry| $ 19))
                                              (|getShellEntry| $ 20))
                                             |INS-;powmod;4S;29|)
                                            (|getShellEntry| $ 65))
                                           (PROGN
                                             (LETT #0# |y|
                                              |INS-;powmod;4S;29|)
                                             (GO #0#)))
                                          ('T
                                           (LETT |z|
                                            (SPADCALL |z| |z| |p|
                                             (|getShellEntry| $ 90))
                                            |INS-;powmod;4S;29|)))))
                                      NIL (GO G190) G191 (EXIT NIL)))))))))
           #0# (EXIT #0#))))) 

(DEFUN |IntegerNumberSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IntegerNumberSystem&|))
        (LETT |dv$| (LIST '|IntegerNumberSystem&| |dv$1|) . #0#)
        (LETT $ (|newShell| 92) . #0#)
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
             (13 . |not|) |INS-;even?;SB;3| (18 . <)
             |INS-;positive?;SB;4| |INS-;copy;2S;5| (24 . -)
             (29 . |shift|) |INS-;bit?;2SB;6| (35 . |One|) (39 . |dec|)
             |INS-;mask;2S;7| (44 . |true|) |INS-;rational?;SB;8|
             (48 . =) (|Integer|) (54 . |convert|) (59 . -)
             |INS-;euclideanSize;SNni;9| (|Float|) (64 . |coerce|)
             |INS-;convert;SF;10| (|DoubleFloat|) (69 . |coerce|)
             |INS-;convert;SDf;11| (|InputForm|) (74 . |convert|)
             |INS-;convert;SIf;12| |INS-;retract;SI;13| (|Pattern| 28)
             (79 . |coerce|) |INS-;convert;SP;14| (|Factored| 6)
             (|IntegerFactorizationPackage| 6) (84 . |factor|)
             (|Factored| $) |INS-;factor;SF;15| (89 . |squareFree|)
             |INS-;squareFree;SF;16| (|IntegerPrimesPackage| 6)
             (94 . |prime?|) |INS-;prime?;SB;17|
             (|IntegerCombinatoricFunctions| 6) (99 . |factorial|)
             |INS-;factorial;2S;18| (104 . |binomial|)
             |INS-;binomial;3S;19| (110 . |permutation|)
             |INS-;permutation;3S;20| (|Union| 28 '"failed")
             |INS-;retractIfCan;SU;21| |INS-;init;S;22| (116 . |zero?|)
             (121 . -) (|Union| $ '"failed") |INS-;nextItem;SU;23|
             (|PatternMatchResult| 28 6)
             (|PatternMatchIntegerNumberSystem| 6)
             (127 . |patternMatch|) (|PatternMatchResult| 28 $)
             |INS-;patternMatch;SP2Pmr;24| (|Fraction| 28)
             (134 . |coerce|) |INS-;rational;SF;25|
             (|Union| 74 '"failed") |INS-;rationalIfCan;SU;26|
             (139 . |rem|) (|PositiveInteger|) (145 . *) (151 . +)
             |INS-;symmetricRemainder;3S;27| (157 . |negative?|)
             (162 . |positiveRemainder|) (168 . |quo|) (174 . *)
             (180 . |one?|) |INS-;invmod;3S;28| (185 . |mulmod|)
             |INS-;powmod;4S;29|)
          '#(|symmetricRemainder| 192 |squareFree| 198 |retractIfCan|
             203 |retract| 208 |rationalIfCan| 213 |rational?| 218
             |rational| 223 |prime?| 228 |powmod| 233 |positive?| 240
             |permutation| 245 |patternMatch| 251 |nextItem| 258 |mask|
             263 |invmod| 268 |init| 274 |factorial| 278 |factor| 283
             |even?| 288 |euclideanSize| 293 |differentiate| 298 |copy|
             303 |convert| 308 |characteristic| 328 |bit?| 332
             |binomial| 338)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 91
                                '(0 7 0 8 0 6 0 10 1 6 12 0 13 1 12 0 0
                                  14 2 6 12 0 0 16 1 6 0 0 19 2 6 0 0 0
                                  20 0 6 0 22 1 6 0 0 23 0 12 0 25 2 6
                                  12 0 0 27 1 6 28 0 29 1 28 0 0 30 1
                                  32 0 28 33 1 35 0 28 36 1 38 0 28 39
                                  1 42 0 28 43 1 46 45 6 47 1 46 45 6
                                  50 1 52 12 6 53 1 55 6 6 56 2 55 6 6
                                  6 58 2 55 6 6 6 60 1 6 12 0 65 2 6 0
                                  0 0 66 3 70 69 6 42 69 71 1 74 0 28
                                  75 2 6 0 0 0 79 2 6 0 80 0 81 2 6 0 0
                                  0 82 1 6 12 0 84 2 6 0 0 0 85 2 6 0 0
                                  0 86 2 6 0 0 0 87 1 6 12 0 88 3 6 0 0
                                  0 0 90 2 0 0 0 0 83 1 0 48 0 51 1 0
                                  62 0 63 1 0 28 0 41 1 0 77 0 78 1 0
                                  12 0 26 1 0 74 0 76 1 0 12 0 54 3 0 0
                                  0 0 0 91 1 0 12 0 17 2 0 0 0 0 61 3 0
                                  72 0 42 72 73 1 0 67 0 68 1 0 0 0 24
                                  2 0 0 0 0 89 0 0 0 64 1 0 0 0 57 1 0
                                  48 0 49 1 0 12 0 15 1 0 7 0 31 1 0 0
                                  0 11 1 0 0 0 18 1 0 35 0 37 1 0 32 0
                                  34 1 0 42 0 44 1 0 38 0 40 0 0 7 9 2
                                  0 12 0 0 21 2 0 0 0 0 59)))))
          '|lookupComplete|)) 
