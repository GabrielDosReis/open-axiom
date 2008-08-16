
(/VERSIONCHECK 2) 

(DEFUN |CLAGG-;#;ANni;1| (|c| $)
  (LENGTH (SPADCALL |c| (QREFELT $ 9)))) 

(DEFUN |CLAGG-;count;MANni;2| (|f| |c| $)
  (PROG (|x| #0=#:G1406 #1=#:G1403 #2=#:G1401 #3=#:G1402)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |CLAGG-;count;MANni;2|)
             (SEQ (LETT |x| NIL |CLAGG-;count;MANni;2|)
                  (LETT #0# (SPADCALL |c| (QREFELT $ 9))
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
  (PROG (|x| #0=#:G1411 #1=#:G1409 #2=#:G1407 #3=#:G1408)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |CLAGG-;any?;MAB;3|)
             (SEQ (LETT |x| NIL |CLAGG-;any?;MAB;3|)
                  (LETT #0# (SPADCALL |c| (QREFELT $ 9))
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
  (PROG (|x| #0=#:G1416 #1=#:G1414 #2=#:G1412 #3=#:G1413)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |CLAGG-;every?;MAB;4|)
             (SEQ (LETT |x| NIL |CLAGG-;every?;MAB;4|)
                  (LETT #0# (SPADCALL |c| (QREFELT $ 9))
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
  (SPADCALL |f| (SPADCALL |c| (QREFELT $ 9)) (QREFELT $ 18))) 

(DEFUN |CLAGG-;reduce;MAS;6| (|f| |x| $)
  (SPADCALL |f| (SPADCALL |x| (QREFELT $ 9)) (QREFELT $ 21))) 

(DEFUN |CLAGG-;reduce;MA2S;7| (|f| |x| |s| $)
  (SPADCALL |f| (SPADCALL |x| (QREFELT $ 9)) |s| (QREFELT $ 23))) 

(DEFUN |CLAGG-;remove;M2A;8| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (QREFELT $ 9)) (QREFELT $ 25))
      (QREFELT $ 26))) 

(DEFUN |CLAGG-;select;M2A;9| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (QREFELT $ 9)) (QREFELT $ 28))
      (QREFELT $ 26))) 

(DEFUN |CLAGG-;remove;S2A;10| (|s| |x| $)
  (SPADCALL (CONS #'|CLAGG-;remove;S2A;10!0| (VECTOR $ |s|)) |x|
      (QREFELT $ 31))) 

(DEFUN |CLAGG-;remove;S2A;10!0| (|#1| $$)
  (SPADCALL |#1| (QREFELT $$ 1) (QREFELT (QREFELT $$ 0) 30))) 

(DEFUN |CLAGG-;reduce;MA3S;11| (|f| |x| |s1| |s2| $)
  (SPADCALL |f| (SPADCALL |x| (QREFELT $ 9)) |s1| |s2| (QREFELT $ 33))) 

(DEFUN |CLAGG-;removeDuplicates;2A;12| (|x| $)
  (SPADCALL (SPADCALL (SPADCALL |x| (QREFELT $ 9)) (QREFELT $ 35))
      (QREFELT $ 26))) 

(DEFUN |Collection&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Collection&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|Collection&| |dv$1| |dv$2|) . #0#)
        (LETT $ (GETREFV 37) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#2|
                                '(|ConvertibleTo| (|InputForm|)))
                            (|HasCategory| |#2| '(|SetCategory|))
                            (|HasAttribute| |#1| '|finiteAggregate|))) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (QSETREFV $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (QSETREFV $ 11
                 (CONS (|dispatchFunction| |CLAGG-;#;ANni;1|) $))
             (QSETREFV $ 13
                 (CONS (|dispatchFunction| |CLAGG-;count;MANni;2|) $))
             (QSETREFV $ 15
                 (CONS (|dispatchFunction| |CLAGG-;any?;MAB;3|) $))
             (QSETREFV $ 16
                 (CONS (|dispatchFunction| |CLAGG-;every?;MAB;4|) $))
             (QSETREFV $ 19
                 (CONS (|dispatchFunction| |CLAGG-;find;MAU;5|) $))
             (QSETREFV $ 22
                 (CONS (|dispatchFunction| |CLAGG-;reduce;MAS;6|) $))
             (QSETREFV $ 24
                 (CONS (|dispatchFunction| |CLAGG-;reduce;MA2S;7|) $))
             (QSETREFV $ 27
                 (CONS (|dispatchFunction| |CLAGG-;remove;M2A;8|) $))
             (QSETREFV $ 29
                 (CONS (|dispatchFunction| |CLAGG-;select;M2A;9|) $))
             (COND
               ((|testBitVector| |pv$| 2)
                (PROGN
                  (QSETREFV $ 32
                      (CONS (|dispatchFunction| |CLAGG-;remove;S2A;10|)
                            $))
                  (QSETREFV $ 34
                      (CONS (|dispatchFunction|
                                |CLAGG-;reduce;MA3S;11|)
                            $))
                  (QSETREFV $ 36
                      (CONS (|dispatchFunction|
                                |CLAGG-;removeDuplicates;2A;12|)
                            $))))))))
        $)))) 

(MAKEPROP '|Collection&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|List| 7) (0 . |parts|) (|NonNegativeInteger|) (5 . |#|)
             (|Mapping| 14 7) (10 . |count|) (|Boolean|) (16 . |any?|)
             (22 . |every?|) (|Union| 7 '"failed") (28 . |find|)
             (34 . |find|) (|Mapping| 7 7 7) (40 . |reduce|)
             (46 . |reduce|) (52 . |reduce|) (59 . |reduce|)
             (66 . |remove|) (72 . |construct|) (77 . |remove|)
             (83 . |select|) (89 . |select|) (95 . =) (101 . |remove|)
             (107 . |remove|) (113 . |reduce|) (121 . |reduce|)
             (129 . |removeDuplicates|) (134 . |removeDuplicates|))
          '#(|select| 139 |removeDuplicates| 145 |remove| 150 |reduce|
             162 |find| 183 |every?| 189 |count| 195 |any?| 201 |#|
             207)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 36
                                '(1 6 8 0 9 1 0 10 0 11 2 0 10 12 0 13
                                  2 0 14 12 0 15 2 0 14 12 0 16 2 8 17
                                  12 0 18 2 0 17 12 0 19 2 8 7 20 0 21
                                  2 0 7 20 0 22 3 8 7 20 0 7 23 3 0 7
                                  20 0 7 24 2 8 0 12 0 25 1 6 0 8 26 2
                                  0 0 12 0 27 2 8 0 12 0 28 2 0 0 12 0
                                  29 2 7 14 0 0 30 2 6 0 12 0 31 2 0 0
                                  7 0 32 4 8 7 20 0 7 7 33 4 0 7 20 0 7
                                  7 34 1 8 0 0 35 1 0 0 0 36 2 0 0 12 0
                                  29 1 0 0 0 36 2 0 0 7 0 32 2 0 0 12 0
                                  27 4 0 7 20 0 7 7 34 3 0 7 20 0 7 24
                                  2 0 7 20 0 22 2 0 17 12 0 19 2 0 14
                                  12 0 16 2 0 10 12 0 13 2 0 14 12 0 15
                                  1 0 10 0 11)))))
          '|lookupComplete|)) 
