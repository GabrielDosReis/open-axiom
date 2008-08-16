
(/VERSIONCHECK 2) 

(DEFUN |HOAGG-;eval;ALA;1| (|u| |l| $)
  (SPADCALL (CONS #'|HOAGG-;eval;ALA;1!0| (VECTOR $ |l|)) |u|
      (QREFELT $ 11))) 

(DEFUN |HOAGG-;eval;ALA;1!0| (|#1| $$)
  (SPADCALL |#1| (QREFELT $$ 1) (QREFELT (QREFELT $$ 0) 9))) 

(DEFUN |HOAGG-;#;ANni;2| (|c| $)
  (LENGTH (SPADCALL |c| (QREFELT $ 14)))) 

(DEFUN |HOAGG-;any?;MAB;3| (|f| |c| $)
  (PROG (|x| #0=#:G1409 #1=#:G1406 #2=#:G1404 #3=#:G1405)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |HOAGG-;any?;MAB;3|)
             (SEQ (LETT |x| NIL |HOAGG-;any?;MAB;3|)
                  (LETT #0# (SPADCALL |c| (QREFELT $ 14))
                        |HOAGG-;any?;MAB;3|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |HOAGG-;any?;MAB;3|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (PROGN
                               (LETT #1# (SPADCALL |x| |f|)
                                     |HOAGG-;any?;MAB;3|)
                               (COND
                                 (#3# (LETT #2#
                                       (COND (#2# 'T) ('T #1#))
                                       |HOAGG-;any?;MAB;3|))
                                 ('T
                                  (PROGN
                                    (LETT #2# #1# |HOAGG-;any?;MAB;3|)
                                    (LETT #3# 'T |HOAGG-;any?;MAB;3|)))))))
                  (LETT #0# (CDR #0#) |HOAGG-;any?;MAB;3|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 'NIL))))))) 

(DEFUN |HOAGG-;every?;MAB;4| (|f| |c| $)
  (PROG (|x| #0=#:G1414 #1=#:G1412 #2=#:G1410 #3=#:G1411)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |HOAGG-;every?;MAB;4|)
             (SEQ (LETT |x| NIL |HOAGG-;every?;MAB;4|)
                  (LETT #0# (SPADCALL |c| (QREFELT $ 14))
                        |HOAGG-;every?;MAB;4|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |HOAGG-;every?;MAB;4|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (PROGN
                               (LETT #1# (SPADCALL |x| |f|)
                                     |HOAGG-;every?;MAB;4|)
                               (COND
                                 (#3# (LETT #2#
                                       (COND (#2# #1#) ('T 'NIL))
                                       |HOAGG-;every?;MAB;4|))
                                 ('T
                                  (PROGN
                                    (LETT #2# #1#
                                     |HOAGG-;every?;MAB;4|)
                                    (LETT #3# 'T |HOAGG-;every?;MAB;4|)))))))
                  (LETT #0# (CDR #0#) |HOAGG-;every?;MAB;4|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 'T))))))) 

(DEFUN |HOAGG-;count;MANni;5| (|f| |c| $)
  (PROG (|x| #0=#:G1419 #1=#:G1417 #2=#:G1415 #3=#:G1416)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |HOAGG-;count;MANni;5|)
             (SEQ (LETT |x| NIL |HOAGG-;count;MANni;5|)
                  (LETT #0# (SPADCALL |c| (QREFELT $ 14))
                        |HOAGG-;count;MANni;5|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |HOAGG-;count;MANni;5|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (COND
                               ((SPADCALL |x| |f|)
                                (PROGN
                                  (LETT #1# 1 |HOAGG-;count;MANni;5|)
                                  (COND
                                    (#3#
                                     (LETT #2# (+ #2# #1#)
                                      |HOAGG-;count;MANni;5|))
                                    ('T
                                     (PROGN
                                       (LETT #2# #1#
                                        |HOAGG-;count;MANni;5|)
                                       (LETT #3# 'T
                                        |HOAGG-;count;MANni;5|)))))))))
                  (LETT #0# (CDR #0#) |HOAGG-;count;MANni;5|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 0))))))) 

(DEFUN |HOAGG-;members;AL;6| (|x| $) (SPADCALL |x| (QREFELT $ 14))) 

(DEFUN |HOAGG-;count;SANni;7| (|s| |x| $)
  (SPADCALL (CONS #'|HOAGG-;count;SANni;7!0| (VECTOR $ |s|)) |x|
      (QREFELT $ 24))) 

(DEFUN |HOAGG-;count;SANni;7!0| (|#1| $$)
  (SPADCALL (QREFELT $$ 1) |#1| (QREFELT (QREFELT $$ 0) 23))) 

(DEFUN |HOAGG-;member?;SAB;8| (|e| |c| $)
  (SPADCALL (CONS #'|HOAGG-;member?;SAB;8!0| (VECTOR $ |e|)) |c|
      (QREFELT $ 26))) 

(DEFUN |HOAGG-;member?;SAB;8!0| (|#1| $$)
  (SPADCALL (QREFELT $$ 1) |#1| (QREFELT (QREFELT $$ 0) 23))) 

(DEFUN |HOAGG-;=;2AB;9| (|x| |y| $)
  (PROG (|b| #0=#:G1429 |a| #1=#:G1428 #2=#:G1425 #3=#:G1423
             #4=#:G1424)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (SPADCALL |y| (QREFELT $ 28))
                  (QREFELT $ 29))
              (PROGN
                (LETT #4# NIL |HOAGG-;=;2AB;9|)
                (SEQ (LETT |b| NIL |HOAGG-;=;2AB;9|)
                     (LETT #0# (SPADCALL |y| (QREFELT $ 14))
                           |HOAGG-;=;2AB;9|)
                     (LETT |a| NIL |HOAGG-;=;2AB;9|)
                     (LETT #1# (SPADCALL |x| (QREFELT $ 14))
                           |HOAGG-;=;2AB;9|)
                     G190
                     (COND
                       ((OR (ATOM #1#)
                            (PROGN
                              (LETT |a| (CAR #1#) |HOAGG-;=;2AB;9|)
                              NIL)
                            (ATOM #0#)
                            (PROGN
                              (LETT |b| (CAR #0#) |HOAGG-;=;2AB;9|)
                              NIL))
                        (GO G191)))
                     (SEQ (EXIT (PROGN
                                  (LETT #2#
                                        (SPADCALL |a| |b|
                                         (QREFELT $ 23))
                                        |HOAGG-;=;2AB;9|)
                                  (COND
                                    (#4#
                                     (LETT #3#
                                      (COND (#3# #2#) ('T 'NIL))
                                      |HOAGG-;=;2AB;9|))
                                    ('T
                                     (PROGN
                                       (LETT #3# #2# |HOAGG-;=;2AB;9|)
                                       (LETT #4# 'T |HOAGG-;=;2AB;9|)))))))
                     (LETT #1#
                           (PROG1 (CDR #1#)
                             (LETT #0# (CDR #0#) |HOAGG-;=;2AB;9|))
                           |HOAGG-;=;2AB;9|)
                     (GO G190) G191 (EXIT NIL))
                (COND (#4# #3#) ('T 'T))))
             ('T 'NIL)))))) 

(DEFUN |HOAGG-;coerce;AOf;10| (|x| $)
  (PROG (#0=#:G1433 |a| #1=#:G1434)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (PROGN
                     (LETT #0# NIL |HOAGG-;coerce;AOf;10|)
                     (SEQ (LETT |a| NIL |HOAGG-;coerce;AOf;10|)
                          (LETT #1# (SPADCALL |x| (QREFELT $ 14))
                                |HOAGG-;coerce;AOf;10|)
                          G190
                          (COND
                            ((OR (ATOM #1#)
                                 (PROGN
                                   (LETT |a| (CAR #1#)
                                    |HOAGG-;coerce;AOf;10|)
                                   NIL))
                             (GO G191)))
                          (SEQ (EXIT (LETT #0#
                                      (CONS
                                       (SPADCALL |a| (QREFELT $ 32))
                                       #0#)
                                      |HOAGG-;coerce;AOf;10|)))
                          (LETT #1# (CDR #1#) |HOAGG-;coerce;AOf;10|)
                          (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                   (QREFELT $ 34))
               (QREFELT $ 35)))))) 

(DEFUN |HomogeneousAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|HomogeneousAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$|
              (LIST '|HomogeneousAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (GETREFV 38) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasAttribute| |#1| '|finiteAggregate|)
                            (|HasAttribute| |#1| '|shallowlyMutable|)
                            (|HasCategory| |#2|
                                (LIST '|Evalable| (|devaluate| |#2|)))
                            (|HasCategory| |#2| '(|SetCategory|))
                            (|HasCategory| |#2|
                                '(|CoercibleTo| (|OutputForm|))))) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (QSETREFV $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 3)
           (QSETREFV $ 12
               (CONS (|dispatchFunction| |HOAGG-;eval;ALA;1|) $))))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (QSETREFV $ 16
                 (CONS (|dispatchFunction| |HOAGG-;#;ANni;2|) $))
             (QSETREFV $ 19
                 (CONS (|dispatchFunction| |HOAGG-;any?;MAB;3|) $))
             (QSETREFV $ 20
                 (CONS (|dispatchFunction| |HOAGG-;every?;MAB;4|) $))
             (QSETREFV $ 21
                 (CONS (|dispatchFunction| |HOAGG-;count;MANni;5|) $))
             (QSETREFV $ 22
                 (CONS (|dispatchFunction| |HOAGG-;members;AL;6|) $))
             (COND
               ((|testBitVector| |pv$| 4)
                (PROGN
                  (QSETREFV $ 25
                      (CONS (|dispatchFunction| |HOAGG-;count;SANni;7|)
                            $))
                  (QSETREFV $ 27
                      (CONS (|dispatchFunction| |HOAGG-;member?;SAB;8|)
                            $))
                  (QSETREFV $ 30
                      (CONS (|dispatchFunction| |HOAGG-;=;2AB;9|) $)))))
             (COND
               ((|testBitVector| |pv$| 5)
                (QSETREFV $ 36
                    (CONS (|dispatchFunction| |HOAGG-;coerce;AOf;10|)
                          $)))))))
        $)))) 

(MAKEPROP '|HomogeneousAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|List| 37) (0 . |eval|) (|Mapping| 7 7) (6 . |map|)
             (12 . |eval|) (|List| 7) (18 . |parts|)
             (|NonNegativeInteger|) (23 . |#|) (|Boolean|)
             (|Mapping| 17 7) (28 . |any?|) (34 . |every?|)
             (40 . |count|) (46 . |members|) (51 . =) (57 . |count|)
             (63 . |count|) (69 . |any?|) (75 . |member?|) (81 . |#|)
             (86 . |size?|) (92 . =) (|OutputForm|) (98 . |coerce|)
             (|List| $) (103 . |commaSeparate|) (108 . |bracket|)
             (113 . |coerce|) (|Equation| 7))
          '#(|members| 118 |member?| 123 |every?| 129 |eval| 135
             |count| 141 |coerce| 153 |any?| 158 = 164 |#| 170)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 36
                                '(2 7 0 0 8 9 2 6 0 10 0 11 2 0 0 0 8
                                  12 1 6 13 0 14 1 0 15 0 16 2 0 17 18
                                  0 19 2 0 17 18 0 20 2 0 15 18 0 21 1
                                  0 13 0 22 2 7 17 0 0 23 2 6 15 18 0
                                  24 2 0 15 7 0 25 2 6 17 18 0 26 2 0
                                  17 7 0 27 1 6 15 0 28 2 6 17 0 15 29
                                  2 0 17 0 0 30 1 7 31 0 32 1 31 0 33
                                  34 1 31 0 0 35 1 0 31 0 36 1 0 13 0
                                  22 2 0 17 7 0 27 2 0 17 18 0 20 2 0 0
                                  0 8 12 2 0 15 7 0 25 2 0 15 18 0 21 1
                                  0 31 0 36 2 0 17 18 0 19 2 0 17 0 0
                                  30 1 0 15 0 16)))))
          '|lookupComplete|)) 
