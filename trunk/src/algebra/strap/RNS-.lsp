
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |RNS-;characteristic;Nni;1|)) 

(PUT '|RNS-;characteristic;Nni;1| '|SPADreplace| '(XLAM NIL 0)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;fractionPart;2S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;truncate;2S;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;round;2S;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;norm;2S;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;coerce;FS;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;convert;SP;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;floor;2S;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |RNS-;ceiling;2S;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |RNS-;patternMatch;SP2Pmr;10|)) 

(DEFUN |RNS-;characteristic;Nni;1| ($) (DECLARE (IGNORE $)) 0) 

(DEFUN |RNS-;fractionPart;2S;2| (|x| $)
  (SPADCALL |x| (SPADCALL |x| (|getShellEntry| $ 10))
      (|getShellEntry| $ 11))) 

(DEFUN |RNS-;truncate;2S;3| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 14))
     (SPADCALL
         (SPADCALL (SPADCALL |x| (|getShellEntry| $ 15))
             (|getShellEntry| $ 16))
         (|getShellEntry| $ 15)))
    ('T (SPADCALL |x| (|getShellEntry| $ 16))))) 

(DEFUN |RNS-;round;2S;4| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 14))
     (SPADCALL
         (SPADCALL |x|
             (SPADCALL (|spadConstant| $ 18)
                 (SPADCALL 2 (|getShellEntry| $ 20))
                 (|getShellEntry| $ 21))
             (|getShellEntry| $ 11))
         (|getShellEntry| $ 10)))
    ('T
     (SPADCALL
         (SPADCALL |x|
             (SPADCALL (|spadConstant| $ 18)
                 (SPADCALL 2 (|getShellEntry| $ 20))
                 (|getShellEntry| $ 21))
             (|getShellEntry| $ 24))
         (|getShellEntry| $ 10))))) 

(DEFUN |RNS-;norm;2S;5| (|x| $) (SPADCALL |x| (|getShellEntry| $ 26))) 

(DEFUN |RNS-;coerce;FS;6| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 29))
          (|getShellEntry| $ 20))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 30))
          (|getShellEntry| $ 20))
      (|getShellEntry| $ 21))) 

(DEFUN |RNS-;convert;SP;7| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 33))
      (|getShellEntry| $ 35))) 

(DEFUN |RNS-;floor;2S;8| (|x| $)
  (PROG (|x1|)
    (RETURN
      (SEQ (LETT |x1|
                 (SPADCALL (SPADCALL |x| (|getShellEntry| $ 37))
                     (|getShellEntry| $ 20))
                 |RNS-;floor;2S;8|)
           (EXIT (COND
                   ((SPADCALL |x| |x1| (|getShellEntry| $ 38)) |x|)
                   ((SPADCALL |x| (|spadConstant| $ 39)
                        (|getShellEntry| $ 41))
                    (SPADCALL |x1| (|spadConstant| $ 18)
                        (|getShellEntry| $ 11)))
                   ('T |x1|))))))) 

(DEFUN |RNS-;ceiling;2S;9| (|x| $)
  (PROG (|x1|)
    (RETURN
      (SEQ (LETT |x1|
                 (SPADCALL (SPADCALL |x| (|getShellEntry| $ 37))
                     (|getShellEntry| $ 20))
                 |RNS-;ceiling;2S;9|)
           (EXIT (COND
                   ((SPADCALL |x| |x1| (|getShellEntry| $ 38)) |x|)
                   ((SPADCALL |x| (|spadConstant| $ 39)
                        (|getShellEntry| $ 41))
                    |x1|)
                   ('T
                    (SPADCALL |x1| (|spadConstant| $ 18)
                        (|getShellEntry| $ 24))))))))) 

(DEFUN |RNS-;patternMatch;SP2Pmr;10| (|x| |p| |l| $)
  (PROG (|r|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 45))
              (SPADCALL |p| |x| |l| (|getShellEntry| $ 47)))
             ((SPADCALL |p| (|getShellEntry| $ 48))
              (SEQ (LETT |r| (SPADCALL |p| (|getShellEntry| $ 50))
                         |RNS-;patternMatch;SP2Pmr;10|)
                   (EXIT (COND
                           ((QEQCAR |r| 0)
                            (COND
                              ((SPADCALL
                                   (SPADCALL |x|
                                    (|getShellEntry| $ 33))
                                   (QCDR |r|) (|getShellEntry| $ 51))
                               |l|)
                              ('T (SPADCALL (|getShellEntry| $ 52)))))
                           ('T (SPADCALL (|getShellEntry| $ 52)))))))
             ('T (SPADCALL (|getShellEntry| $ 52)))))))) 

(DEFUN |RealNumberSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|RealNumberSystem&|))
        (LETT |dv$| (LIST '|RealNumberSystem&| |dv$1|) . #0#)
        (LETT $ (|newShell| 57) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|RealNumberSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (0 . |Zero|)
             |RNS-;characteristic;Nni;1| (4 . |truncate|) (9 . -)
             |RNS-;fractionPart;2S;2| (|Boolean|) (15 . |negative?|)
             (20 . -) (25 . |floor|) |RNS-;truncate;2S;3| (30 . |One|)
             (|Integer|) (34 . |coerce|) (39 . /) (|PositiveInteger|)
             (45 . |One|) (49 . +) |RNS-;round;2S;4| (55 . |abs|)
             |RNS-;norm;2S;5| (|Fraction| 19) (60 . |numer|)
             (65 . |denom|) |RNS-;coerce;FS;6| (|Float|)
             (70 . |convert|) (|Pattern| 32) (75 . |coerce|)
             |RNS-;convert;SP;7| (80 . |wholePart|) (85 . =)
             (91 . |Zero|) (95 . |Zero|) (99 . <) (105 . |One|)
             |RNS-;floor;2S;8| |RNS-;ceiling;2S;9| (109 . |generic?|)
             (|PatternMatchResult| 32 6) (114 . |addMatch|)
             (121 . |constant?|) (|Union| 32 '"failed")
             (126 . |retractIfCan|) (131 . =) (137 . |failed|)
             (|PatternMatchResult| 32 $) |RNS-;patternMatch;SP2Pmr;10|
             (|DoubleFloat|) (|OutputForm|))
          '#(|truncate| 141 |round| 146 |patternMatch| 151 |norm| 158
             |fractionPart| 163 |floor| 168 |convert| 173 |coerce| 178
             |characteristic| 188 |ceiling| 192)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 54
                                '(0 7 0 8 1 6 0 0 10 2 6 0 0 0 11 1 6
                                  13 0 14 1 6 0 0 15 1 6 0 0 16 0 6 0
                                  18 1 6 0 19 20 2 6 0 0 0 21 0 22 0 23
                                  2 6 0 0 0 24 1 6 0 0 26 1 28 19 0 29
                                  1 28 19 0 30 1 6 32 0 33 1 34 0 32 35
                                  1 6 19 0 37 2 6 13 0 0 38 0 6 0 39 0
                                  19 0 40 2 6 13 0 0 41 0 19 0 42 1 34
                                  13 0 45 3 46 0 34 6 0 47 1 34 13 0 48
                                  1 34 49 0 50 2 32 13 0 0 51 0 46 0 52
                                  1 0 0 0 17 1 0 0 0 25 3 0 53 0 34 53
                                  54 1 0 0 0 27 1 0 0 0 12 1 0 0 0 43 1
                                  0 34 0 36 1 0 0 28 31 1 0 0 28 31 0 0
                                  7 9 1 0 0 0 44)))))
          '|lookupComplete|)) 
