
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
  (LET ((|x1| (SPADCALL (SPADCALL |x| (|getShellEntry| $ 37))
                  (|getShellEntry| $ 20))))
    (COND
      ((SPADCALL |x| |x1| (|getShellEntry| $ 38)) |x|)
      ((SPADCALL |x| (|spadConstant| $ 39) (|getShellEntry| $ 41))
       (SPADCALL |x1| (|spadConstant| $ 18) (|getShellEntry| $ 11)))
      ('T |x1|)))) 

(DEFUN |RNS-;ceiling;2S;9| (|x| $)
  (LET ((|x1| (SPADCALL (SPADCALL |x| (|getShellEntry| $ 37))
                  (|getShellEntry| $ 20))))
    (COND
      ((SPADCALL |x| |x1| (|getShellEntry| $ 38)) |x|)
      ((SPADCALL |x| (|spadConstant| $ 39) (|getShellEntry| $ 44))
       (SPADCALL |x1| (|spadConstant| $ 18) (|getShellEntry| $ 24)))
      ('T |x1|)))) 

(DEFUN |RNS-;patternMatch;SP2Pmr;10| (|x| |p| |l| $)
  (PROG (|r|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 46))
              (SPADCALL |p| |x| |l| (|getShellEntry| $ 48)))
             ((SPADCALL |p| (|getShellEntry| $ 49))
              (SEQ (LETT |r| (SPADCALL |p| (|getShellEntry| $ 51))
                         |RNS-;patternMatch;SP2Pmr;10|)
                   (EXIT (COND
                           ((EQL (CAR |r|) 0)
                            (COND
                              ((SPADCALL
                                   (SPADCALL |x|
                                    (|getShellEntry| $ 33))
                                   (CDR |r|) (|getShellEntry| $ 52))
                               |l|)
                              ('T (SPADCALL (|getShellEntry| $ 53)))))
                           ('T (SPADCALL (|getShellEntry| $ 53)))))))
             ('T (SPADCALL (|getShellEntry| $ 53)))))))) 

(DEFUN |RealNumberSystem&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|RealNumberSystem&| |dv$1|)) ($ (|newShell| 58))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

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
             |RNS-;floor;2S;8| (109 . >=) |RNS-;ceiling;2S;9|
             (115 . |generic?|) (|PatternMatchResult| 32 6)
             (120 . |addMatch|) (127 . |constant?|)
             (|Union| 32 '"failed") (132 . |retractIfCan|) (137 . =)
             (143 . |failed|) (|PatternMatchResult| 32 $)
             |RNS-;patternMatch;SP2Pmr;10| (|DoubleFloat|)
             (|OutputForm|))
          '#(|truncate| 147 |round| 152 |patternMatch| 157 |norm| 164
             |fractionPart| 169 |floor| 174 |convert| 179 |coerce| 184
             |characteristic| 194 |ceiling| 198)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 55
                                '(0 7 0 8 1 6 0 0 10 2 6 0 0 0 11 1 6
                                  13 0 14 1 6 0 0 15 1 6 0 0 16 0 6 0
                                  18 1 6 0 19 20 2 6 0 0 0 21 0 22 0 23
                                  2 6 0 0 0 24 1 6 0 0 26 1 28 19 0 29
                                  1 28 19 0 30 1 6 32 0 33 1 34 0 32 35
                                  1 6 19 0 37 2 6 13 0 0 38 0 6 0 39 0
                                  19 0 40 2 6 13 0 0 41 0 19 0 42 2 6
                                  13 0 0 44 1 34 13 0 46 3 47 0 34 6 0
                                  48 1 34 13 0 49 1 34 50 0 51 2 32 13
                                  0 0 52 0 47 0 53 1 0 0 0 17 1 0 0 0
                                  25 3 0 54 0 34 54 55 1 0 0 0 27 1 0 0
                                  0 12 1 0 0 0 43 1 0 34 0 36 1 0 0 28
                                  31 1 0 0 28 31 0 0 7 9 1 0 0 0 45)))))
          '|lookupComplete|)) 
