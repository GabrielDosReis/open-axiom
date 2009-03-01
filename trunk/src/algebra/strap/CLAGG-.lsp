
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |CLAGG-;#;ANni;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |CLAGG-;count;MANni;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |CLAGG-;any?;MAB;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |CLAGG-;every?;MAB;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |CLAGG-;find;MAU;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |CLAGG-;reduce;MAS;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |CLAGG-;reduce;MA2S;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |CLAGG-;remove;M2A;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |CLAGG-;select;M2A;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |CLAGG-;remove;S2A;10|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |CLAGG-;reduce;MA3S;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |CLAGG-;removeDuplicates;2A;12|)) 

(DEFUN |CLAGG-;#;ANni;1| (|c| $)
  (LENGTH (SPADCALL |c| (|getShellEntry| $ 9)))) 

(DEFUN |CLAGG-;count;MANni;2| (|f| |c| $)
  (PROG (|x| #0=#:G1430 #1=#:G1404 #2=#:G1402 #3=#:G1403)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |CLAGG-;count;MANni;2|)
             (SEQ (LETT |x| NIL |CLAGG-;count;MANni;2|)
                  (LETT #0# (SPADCALL |c| (|getShellEntry| $ 9))
                        |CLAGG-;count;MANni;2|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |CLAGG-;count;MANni;2|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (COND
                               ((SPADCALL |x| |f|)
                                (PROGN
                                  (LETT #1# 1 |CLAGG-;count;MANni;2|)
                                  (COND
                                    (#3#
                                     (LETT #2# (+ #2# #1#)
                                      |CLAGG-;count;MANni;2|))
                                    ('T
                                     (PROGN
                                       (LETT #2# #1#
                                        |CLAGG-;count;MANni;2|)
                                       (LETT #3# 'T
                                        |CLAGG-;count;MANni;2|)))))))))
                  (LETT #0# (CDR #0#) |CLAGG-;count;MANni;2|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 0))))))) 

(DEFUN |CLAGG-;any?;MAB;3| (|f| |c| $)
  (PROG (|x| #0=#:G1431 #1=#:G1409 #2=#:G1407 #3=#:G1408)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |CLAGG-;any?;MAB;3|)
             (SEQ (LETT |x| NIL |CLAGG-;any?;MAB;3|)
                  (LETT #0# (SPADCALL |c| (|getShellEntry| $ 9))
                        |CLAGG-;any?;MAB;3|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |CLAGG-;any?;MAB;3|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (PROGN
                               (LETT #1# (SPADCALL |x| |f|)
                                     |CLAGG-;any?;MAB;3|)
                               (COND
                                 (#3# (LETT #2#
                                       (COND (#2# 'T) ('T #1#))
                                       |CLAGG-;any?;MAB;3|))
                                 ('T
                                  (PROGN
                                    (LETT #2# #1# |CLAGG-;any?;MAB;3|)
                                    (LETT #3# 'T |CLAGG-;any?;MAB;3|)))))))
                  (LETT #0# (CDR #0#) |CLAGG-;any?;MAB;3|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 'NIL))))))) 

(DEFUN |CLAGG-;every?;MAB;4| (|f| |c| $)
  (PROG (|x| #0=#:G1432 #1=#:G1413 #2=#:G1411 #3=#:G1412)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |CLAGG-;every?;MAB;4|)
             (SEQ (LETT |x| NIL |CLAGG-;every?;MAB;4|)
                  (LETT #0# (SPADCALL |c| (|getShellEntry| $ 9))
                        |CLAGG-;every?;MAB;4|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |CLAGG-;every?;MAB;4|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (PROGN
                               (LETT #1# (SPADCALL |x| |f|)
                                     |CLAGG-;every?;MAB;4|)
                               (COND
                                 (#3# (LETT #2#
                                       (COND (#2# #1#) ('T 'NIL))
                                       |CLAGG-;every?;MAB;4|))
                                 ('T
                                  (PROGN
                                    (LETT #2# #1#
                                     |CLAGG-;every?;MAB;4|)
                                    (LETT #3# 'T |CLAGG-;every?;MAB;4|)))))))
                  (LETT #0# (CDR #0#) |CLAGG-;every?;MAB;4|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 'T))))))) 

(DEFUN |CLAGG-;find;MAU;5| (|f| |c| $)
  (SPADCALL |f| (SPADCALL |c| (|getShellEntry| $ 9))
      (|getShellEntry| $ 24))) 

(DEFUN |CLAGG-;reduce;MAS;6| (|f| |x| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
      (|getShellEntry| $ 27))) 

(DEFUN |CLAGG-;reduce;MA2S;7| (|f| |x| |s| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9)) |s|
      (|getShellEntry| $ 29))) 

(DEFUN |CLAGG-;remove;M2A;8| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
                (|getShellEntry| $ 31))
            (|getShellEntry| $ 32))) 

(DEFUN |CLAGG-;select;M2A;9| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
                (|getShellEntry| $ 34))
            (|getShellEntry| $ 32))) 

(DEFUN |CLAGG-;remove;S2A;10| (|s| |x| $)
  (SPADCALL (CONS #'|CLAGG-;remove;S2A;10!0| (VECTOR $ |s|)) |x|
      (|getShellEntry| $ 37))) 

(DEFUN |CLAGG-;remove;S2A;10!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 36))) 

(DEFUN |CLAGG-;reduce;MA3S;11| (|f| |x| |s1| |s2| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9)) |s1| |s2|
      (|getShellEntry| $ 39))) 

(DEFUN |CLAGG-;removeDuplicates;2A;12| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 9))
          (|getShellEntry| $ 41))
      (|getShellEntry| $ 32))) 

(DEFUN |Collection&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Collection&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|Collection&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 43) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#2|
                                '(|ConvertibleTo| (|InputForm|)))
                            (|HasCategory| |#2| '(|SetCategory|))
                            (|HasAttribute| |#1| '|finiteAggregate|))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (|setShellEntry| $ 12
                 (CONS (|dispatchFunction| |CLAGG-;#;ANni;1|) $))
             (|setShellEntry| $ 18
                 (CONS (|dispatchFunction| |CLAGG-;count;MANni;2|) $))
             (|setShellEntry| $ 21
                 (CONS (|dispatchFunction| |CLAGG-;any?;MAB;3|) $))
             (|setShellEntry| $ 22
                 (CONS (|dispatchFunction| |CLAGG-;every?;MAB;4|) $))
             (|setShellEntry| $ 25
                 (CONS (|dispatchFunction| |CLAGG-;find;MAU;5|) $))
             (|setShellEntry| $ 28
                 (CONS (|dispatchFunction| |CLAGG-;reduce;MAS;6|) $))
             (|setShellEntry| $ 30
                 (CONS (|dispatchFunction| |CLAGG-;reduce;MA2S;7|) $))
             (|setShellEntry| $ 33
                 (CONS (|dispatchFunction| |CLAGG-;remove;M2A;8|) $))
             (|setShellEntry| $ 35
                 (CONS (|dispatchFunction| |CLAGG-;select;M2A;9|) $))
             (COND
               ((|testBitVector| |pv$| 2)
                (PROGN
                  (|setShellEntry| $ 38
                      (CONS (|dispatchFunction| |CLAGG-;remove;S2A;10|)
                            $))
                  (|setShellEntry| $ 40
                      (CONS (|dispatchFunction|
                                |CLAGG-;reduce;MA3S;11|)
                            $))
                  (|setShellEntry| $ 42
                      (CONS (|dispatchFunction|
                                |CLAGG-;removeDuplicates;2A;12|)
                            $))))))))
        $)))) 

(MAKEPROP '|Collection&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|List| 7) (0 . |parts|) (|NonNegativeInteger|) (5 . |#|)
             (10 . |#|) (15 . |One|) (19 . +) (25 . |Zero|) (|Boolean|)
             (|Mapping| 16 7) (29 . |count|) (35 . |true|)
             (39 . |false|) (43 . |any?|) (49 . |every?|)
             (|Union| 7 '"failed") (55 . |find|) (61 . |find|)
             (|Mapping| 7 7 7) (67 . |reduce|) (73 . |reduce|)
             (79 . |reduce|) (86 . |reduce|) (93 . |remove|)
             (99 . |construct|) (104 . |remove|) (110 . |select|)
             (116 . |select|) (122 . =) (128 . |remove|)
             (134 . |remove|) (140 . |reduce|) (148 . |reduce|)
             (156 . |removeDuplicates|) (161 . |removeDuplicates|))
          '#(|select| 166 |removeDuplicates| 172 |remove| 177 |reduce|
             189 |find| 210 |every?| 216 |count| 222 |any?| 228 |#|
             234)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 42
                                '(1 6 8 0 9 1 8 10 0 11 1 0 10 0 12 0
                                  10 0 13 2 10 0 0 0 14 0 10 0 15 2 0
                                  10 17 0 18 0 16 0 19 0 16 0 20 2 0 16
                                  17 0 21 2 0 16 17 0 22 2 8 23 17 0 24
                                  2 0 23 17 0 25 2 8 7 26 0 27 2 0 7 26
                                  0 28 3 8 7 26 0 7 29 3 0 7 26 0 7 30
                                  2 8 0 17 0 31 1 6 0 8 32 2 0 0 17 0
                                  33 2 8 0 17 0 34 2 0 0 17 0 35 2 7 16
                                  0 0 36 2 6 0 17 0 37 2 0 0 7 0 38 4 8
                                  7 26 0 7 7 39 4 0 7 26 0 7 7 40 1 8 0
                                  0 41 1 0 0 0 42 2 0 0 17 0 35 1 0 0 0
                                  42 2 0 0 7 0 38 2 0 0 17 0 33 4 0 7
                                  26 0 7 7 40 3 0 7 26 0 7 30 2 0 7 26
                                  0 28 2 0 23 17 0 25 2 0 16 17 0 22 2
                                  0 10 17 0 18 2 0 16 17 0 21 1 0 10 0
                                  12)))))
          '|lookupComplete|)) 
