
(/VERSIONCHECK 2) 

(PUT '|RNS-;characteristic;Nni;1| '|SPADreplace| '(XLAM NIL 0)) 

(DEFUN |RNS-;characteristic;Nni;1| ($) 0) 

(DEFUN |RNS-;fractionPart;2S;2| (|x| $)
  (SPADCALL |x| (SPADCALL |x| (|getShellEntry| $ 9))
      (|getShellEntry| $ 10))) 

(DEFUN |RNS-;truncate;2S;3| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 13))
     (SPADCALL
         (SPADCALL (SPADCALL |x| (|getShellEntry| $ 14))
             (|getShellEntry| $ 15))
         (|getShellEntry| $ 14)))
    ('T (SPADCALL |x| (|getShellEntry| $ 15))))) 

(DEFUN |RNS-;round;2S;4| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 13))
     (SPADCALL
         (SPADCALL |x|
             (SPADCALL (|spadConstant| $ 17)
                 (SPADCALL 2 (|getShellEntry| $ 19))
                 (|getShellEntry| $ 20))
             (|getShellEntry| $ 10))
         (|getShellEntry| $ 9)))
    ('T
     (SPADCALL
         (SPADCALL |x|
             (SPADCALL (|spadConstant| $ 17)
                 (SPADCALL 2 (|getShellEntry| $ 19))
                 (|getShellEntry| $ 20))
             (|getShellEntry| $ 21))
         (|getShellEntry| $ 9))))) 

(DEFUN |RNS-;norm;2S;5| (|x| $) (SPADCALL |x| (|getShellEntry| $ 23))) 

(DEFUN |RNS-;coerce;FS;6| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26))
          (|getShellEntry| $ 19))
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 27))
          (|getShellEntry| $ 19))
      (|getShellEntry| $ 20))) 

(DEFUN |RNS-;convert;SP;7| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 30))
      (|getShellEntry| $ 32))) 

(DEFUN |RNS-;floor;2S;8| (|x| $)
  (PROG (|x1|)
    (RETURN
      (SEQ (LETT |x1|
                 (SPADCALL (SPADCALL |x| (|getShellEntry| $ 34))
                     (|getShellEntry| $ 19))
                 |RNS-;floor;2S;8|)
           (EXIT (COND
                   ((SPADCALL |x| |x1| (|getShellEntry| $ 35)) |x|)
                   ((SPADCALL |x| (|spadConstant| $ 36)
                        (|getShellEntry| $ 37))
                    (SPADCALL |x1| (|spadConstant| $ 17)
                        (|getShellEntry| $ 10)))
                   ('T |x1|))))))) 

(DEFUN |RNS-;ceiling;2S;9| (|x| $)
  (PROG (|x1|)
    (RETURN
      (SEQ (LETT |x1|
                 (SPADCALL (SPADCALL |x| (|getShellEntry| $ 34))
                     (|getShellEntry| $ 19))
                 |RNS-;ceiling;2S;9|)
           (EXIT (COND
                   ((SPADCALL |x| |x1| (|getShellEntry| $ 35)) |x|)
                   ((SPADCALL |x| (|spadConstant| $ 36)
                        (|getShellEntry| $ 37))
                    |x1|)
                   ('T
                    (SPADCALL |x1| (|spadConstant| $ 17)
                        (|getShellEntry| $ 21))))))))) 

(DEFUN |RNS-;patternMatch;SP2Pmr;10| (|x| |p| |l| $)
  (PROG (|r|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 40))
              (SPADCALL |p| |x| |l| (|getShellEntry| $ 42)))
             ((SPADCALL |p| (|getShellEntry| $ 43))
              (SEQ (LETT |r| (SPADCALL |p| (|getShellEntry| $ 45))
                         |RNS-;patternMatch;SP2Pmr;10|)
                   (EXIT (COND
                           ((QEQCAR |r| 0)
                            (COND
                              ((SPADCALL
                                   (SPADCALL |x|
                                    (|getShellEntry| $ 30))
                                   (QCDR |r|) (|getShellEntry| $ 46))
                               |l|)
                              ('T (SPADCALL (|getShellEntry| $ 47)))))
                           ('T (SPADCALL (|getShellEntry| $ 47)))))))
             ('T (SPADCALL (|getShellEntry| $ 47)))))))) 

(DEFUN |RealNumberSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|RealNumberSystem&|))
        (LETT |dv$| (LIST '|RealNumberSystem&| |dv$1|) . #0#)
        (LETT $ (|newShell| 52) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|RealNumberSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) |RNS-;characteristic;Nni;1|
             (0 . |truncate|) (5 . -) |RNS-;fractionPart;2S;2|
             (|Boolean|) (11 . |negative?|) (16 . -) (21 . |floor|)
             |RNS-;truncate;2S;3| (26 . |One|) (|Integer|)
             (30 . |coerce|) (35 . /) (41 . +) |RNS-;round;2S;4|
             (47 . |abs|) |RNS-;norm;2S;5| (|Fraction| 18)
             (52 . |numer|) (57 . |denom|) |RNS-;coerce;FS;6| (|Float|)
             (62 . |convert|) (|Pattern| 29) (67 . |coerce|)
             |RNS-;convert;SP;7| (72 . |wholePart|) (77 . =)
             (83 . |Zero|) (87 . <) |RNS-;floor;2S;8|
             |RNS-;ceiling;2S;9| (93 . |generic?|)
             (|PatternMatchResult| 29 6) (98 . |addMatch|)
             (105 . |constant?|) (|Union| 29 '"failed")
             (110 . |retractIfCan|) (115 . =) (121 . |failed|)
             (|PatternMatchResult| 29 $) |RNS-;patternMatch;SP2Pmr;10|
             (|DoubleFloat|) (|OutputForm|))
          '#(|truncate| 125 |round| 130 |patternMatch| 135 |norm| 142
             |fractionPart| 147 |floor| 152 |convert| 157 |coerce| 162
             |characteristic| 172 |ceiling| 176)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 49
                                '(1 6 0 0 9 2 6 0 0 0 10 1 6 12 0 13 1
                                  6 0 0 14 1 6 0 0 15 0 6 0 17 1 6 0 18
                                  19 2 6 0 0 0 20 2 6 0 0 0 21 1 6 0 0
                                  23 1 25 18 0 26 1 25 18 0 27 1 6 29 0
                                  30 1 31 0 29 32 1 6 18 0 34 2 6 12 0
                                  0 35 0 6 0 36 2 6 12 0 0 37 1 31 12 0
                                  40 3 41 0 31 6 0 42 1 31 12 0 43 1 31
                                  44 0 45 2 29 12 0 0 46 0 41 0 47 1 0
                                  0 0 16 1 0 0 0 22 3 0 48 0 31 48 49 1
                                  0 0 0 24 1 0 0 0 11 1 0 0 0 38 1 0 31
                                  0 33 1 0 0 25 28 1 0 0 25 28 0 0 7 8
                                  1 0 0 0 39)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|RealNumberSystem&| '|isFunctor|
             '(((|round| ($ $)) T (ELT $ 22))
               ((|truncate| ($ $)) T (ELT $ 16))
               ((|fractionPart| ($ $)) T (ELT $ 11))
               ((|floor| ($ $)) T (ELT $ 38))
               ((|ceiling| ($ $)) T (ELT $ 39))
               ((|norm| ($ $)) T (ELT $ 24))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Float|) $) $
                     (|Pattern| (|Float|))
                     (|PatternMatchResult| (|Float|) $)))
                T (ELT $ 49))
               ((|convert| ((|Pattern| (|Float|)) $)) T (ELT $ 33))
               ((|coerce| ($ (|Fraction| (|Integer|)))) T (ELT $ 28))
               ((|coerce| ($ (|Integer|))) T (ELT $ NIL))
               ((|convert| ((|DoubleFloat|) $)) T (ELT $ NIL))
               ((|convert| ((|Float|) $)) T (ELT $ NIL))
               ((|coerce| ($ (|Fraction| (|Integer|)))) T (ELT $ 28))
               ((|coerce| ($ $)) T (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ NIL))
               ((|characteristic| ((|NonNegativeInteger|))) T
                (ELT $ 8))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ NIL)))
             (|addModemap| '|RealNumberSystem&|
                 '(|RealNumberSystem&| |#1|)
                 '((CATEGORY |domain| (SIGNATURE |round| (|#1| |#1|))
                       (SIGNATURE |truncate| (|#1| |#1|))
                       (SIGNATURE |fractionPart| (|#1| |#1|))
                       (SIGNATURE |floor| (|#1| |#1|))
                       (SIGNATURE |ceiling| (|#1| |#1|))
                       (SIGNATURE |norm| (|#1| |#1|))
                       (SIGNATURE |patternMatch|
                           ((|PatternMatchResult| (|Float|) |#1|) |#1|
                            (|Pattern| (|Float|))
                            (|PatternMatchResult| (|Float|) |#1|)))
                       (SIGNATURE |convert|
                           ((|Pattern| (|Float|)) |#1|))
                       (SIGNATURE |coerce|
                           (|#1| (|Fraction| (|Integer|))))
                       (SIGNATURE |coerce| (|#1| (|Integer|)))
                       (SIGNATURE |convert| ((|DoubleFloat|) |#1|))
                       (SIGNATURE |convert| ((|Float|) |#1|))
                       (SIGNATURE |coerce|
                           (|#1| (|Fraction| (|Integer|))))
                       (SIGNATURE |coerce| (|#1| |#1|))
                       (SIGNATURE |coerce| (|#1| (|Integer|)))
                       (SIGNATURE |characteristic|
                           ((|NonNegativeInteger|)))
                       (SIGNATURE |coerce| ((|OutputForm|) |#1|)))
                   (|RealNumberSystem|))
                 T '|RealNumberSystem&|
                 (|put| '|RealNumberSystem&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |round| (|#1| |#1|))
                                 (SIGNATURE |truncate| (|#1| |#1|))
                                 (SIGNATURE |fractionPart| (|#1| |#1|))
                                 (SIGNATURE |floor| (|#1| |#1|))
                                 (SIGNATURE |ceiling| (|#1| |#1|))
                                 (SIGNATURE |norm| (|#1| |#1|))
                                 (SIGNATURE |patternMatch|
                                     ((|PatternMatchResult| (|Float|)
                                       |#1|)
                                      |#1| (|Pattern| (|Float|))
                                      (|PatternMatchResult| (|Float|)
                                       |#1|)))
                                 (SIGNATURE |convert|
                                     ((|Pattern| (|Float|)) |#1|))
                                 (SIGNATURE |coerce|
                                     (|#1| (|Fraction| (|Integer|))))
                                 (SIGNATURE |coerce|
                                     (|#1| (|Integer|)))
                                 (SIGNATURE |convert|
                                     ((|DoubleFloat|) |#1|))
                                 (SIGNATURE |convert| ((|Float|) |#1|))
                                 (SIGNATURE |coerce|
                                     (|#1| (|Fraction| (|Integer|))))
                                 (SIGNATURE |coerce| (|#1| |#1|))
                                 (SIGNATURE |coerce|
                                     (|#1| (|Integer|)))
                                 (SIGNATURE |characteristic|
                                     ((|NonNegativeInteger|)))
                                 (SIGNATURE |coerce|
                                     ((|OutputForm|) |#1|)))
                             (|RealNumberSystem|))
                        |$CategoryFrame|)))) 
