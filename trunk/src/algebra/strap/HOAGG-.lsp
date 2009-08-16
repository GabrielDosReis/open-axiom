
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |HOAGG-;eval;ALA;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |HOAGG-;#;ANni;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |HOAGG-;any?;MAB;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |HOAGG-;every?;MAB;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |HOAGG-;count;MANni;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |HOAGG-;members;AL;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |HOAGG-;count;SANni;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |HOAGG-;member?;SAB;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |HOAGG-;=;2AB;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |HOAGG-;coerce;AOf;10|)) 

(DEFUN |HOAGG-;eval;ALA;1| (|u| |l| $)
  (SPADCALL (CONS #'|HOAGG-;eval;ALA;1!0| (VECTOR $ |l|)) |u|
      (|getShellEntry| $ 12))) 

(DEFUN |HOAGG-;eval;ALA;1!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 10))) 

(DEFUN |HOAGG-;#;ANni;2| (|c| $)
  (LENGTH (SPADCALL |c| (|getShellEntry| $ 15)))) 

(DEFUN |HOAGG-;any?;MAB;3| (|f| |c| $)
  (PROG (|x| #0=#:G1429 #1=#:G1407 #2=#:G1405 #3=#:G1406)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |HOAGG-;any?;MAB;3|)
             (SEQ (LETT |x| NIL |HOAGG-;any?;MAB;3|)
                  (LETT #0# (SPADCALL |c| (|getShellEntry| $ 15))
                        |HOAGG-;any?;MAB;3|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |HOAGG-;any?;MAB;3|)
                           NIL))
                     (GO G191)))
                  (PROGN
                    (LETT #1# (SPADCALL |x| |f|) |HOAGG-;any?;MAB;3|)
                    (COND
                      (#3# (LETT #2# (COND (#2# T) ('T #1#))
                                 |HOAGG-;any?;MAB;3|))
                      ('T
                       (PROGN
                         (LETT #2# #1# |HOAGG-;any?;MAB;3|)
                         (LETT #3# 'T |HOAGG-;any?;MAB;3|)))))
                  (LETT #0# (CDR #0#) |HOAGG-;any?;MAB;3|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T NIL))))))) 

(DEFUN |HOAGG-;every?;MAB;4| (|f| |c| $)
  (PROG (|x| #0=#:G1430 #1=#:G1412 #2=#:G1410 #3=#:G1411)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |HOAGG-;every?;MAB;4|)
             (SEQ (LETT |x| NIL |HOAGG-;every?;MAB;4|)
                  (LETT #0# (SPADCALL |c| (|getShellEntry| $ 15))
                        |HOAGG-;every?;MAB;4|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |HOAGG-;every?;MAB;4|)
                           NIL))
                     (GO G191)))
                  (PROGN
                    (LETT #1# (SPADCALL |x| |f|) |HOAGG-;every?;MAB;4|)
                    (COND
                      (#3# (LETT #2# (COND (#2# #1#) ('T NIL))
                                 |HOAGG-;every?;MAB;4|))
                      ('T
                       (PROGN
                         (LETT #2# #1# |HOAGG-;every?;MAB;4|)
                         (LETT #3# 'T |HOAGG-;every?;MAB;4|)))))
                  (LETT #0# (CDR #0#) |HOAGG-;every?;MAB;4|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T T))))))) 

(DEFUN |HOAGG-;count;MANni;5| (|f| |c| $)
  (PROG (|x| #0=#:G1431 #1=#:G1416 #2=#:G1414 #3=#:G1415)
    (RETURN
      (SEQ (PROGN
             (LETT #3# NIL |HOAGG-;count;MANni;5|)
             (SEQ (LETT |x| NIL |HOAGG-;count;MANni;5|)
                  (LETT #0# (SPADCALL |c| (|getShellEntry| $ 15))
                        |HOAGG-;count;MANni;5|)
                  G190
                  (COND
                    ((OR (ATOM #0#)
                         (PROGN
                           (LETT |x| (CAR #0#) |HOAGG-;count;MANni;5|)
                           NIL))
                     (GO G191)))
                  (COND
                    ((SPADCALL |x| |f|)
                     (PROGN
                       (LETT #1# 1 |HOAGG-;count;MANni;5|)
                       (COND
                         (#3# (LETT #2# (+ #2# #1#)
                                    |HOAGG-;count;MANni;5|))
                         ('T
                          (PROGN
                            (LETT #2# #1# |HOAGG-;count;MANni;5|)
                            (LETT #3# 'T |HOAGG-;count;MANni;5|)))))))
                  (LETT #0# (CDR #0#) |HOAGG-;count;MANni;5|) (GO G190)
                  G191 (EXIT NIL))
             (COND (#3# #2#) ('T 0))))))) 

(DEFUN |HOAGG-;members;AL;6| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 15))) 

(DEFUN |HOAGG-;count;SANni;7| (|s| |x| $)
  (SPADCALL (CONS #'|HOAGG-;count;SANni;7!0| (VECTOR $ |s|)) |x|
      (|getShellEntry| $ 31))) 

(DEFUN |HOAGG-;count;SANni;7!0| (|#1| $$)
  (SPADCALL (|getShellEntry| $$ 1) |#1|
      (|getShellEntry| (|getShellEntry| $$ 0) 30))) 

(DEFUN |HOAGG-;member?;SAB;8| (|e| |c| $)
  (SPADCALL (CONS #'|HOAGG-;member?;SAB;8!0| (VECTOR $ |e|)) |c|
      (|getShellEntry| $ 33))) 

(DEFUN |HOAGG-;member?;SAB;8!0| (|#1| $$)
  (SPADCALL (|getShellEntry| $$ 1) |#1|
      (|getShellEntry| (|getShellEntry| $$ 0) 30))) 

(DEFUN |HOAGG-;=;2AB;9| (|x| |y| $)
  (PROG (|b| #0=#:G1433 |a| #1=#:G1432 #2=#:G1423 #3=#:G1421
             #4=#:G1422)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (SPADCALL |y| (|getShellEntry| $ 35))
                  (|getShellEntry| $ 36))
              (PROGN
                (LETT #4# NIL |HOAGG-;=;2AB;9|)
                (SEQ (LETT |b| NIL |HOAGG-;=;2AB;9|)
                     (LETT #0# (SPADCALL |y| (|getShellEntry| $ 15))
                           |HOAGG-;=;2AB;9|)
                     (LETT |a| NIL |HOAGG-;=;2AB;9|)
                     (LETT #1# (SPADCALL |x| (|getShellEntry| $ 15))
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
                     (PROGN
                       (LETT #2#
                             (SPADCALL |a| |b| (|getShellEntry| $ 30))
                             |HOAGG-;=;2AB;9|)
                       (COND
                         (#4# (LETT #3# (COND (#3# #2#) ('T NIL))
                                    |HOAGG-;=;2AB;9|))
                         ('T
                          (PROGN
                            (LETT #3# #2# |HOAGG-;=;2AB;9|)
                            (LETT #4# 'T |HOAGG-;=;2AB;9|)))))
                     (LETT #1#
                           (PROG1 (CDR #1#)
                             (LETT #0# (CDR #0#) |HOAGG-;=;2AB;9|))
                           |HOAGG-;=;2AB;9|)
                     (GO G190) G191 (EXIT NIL))
                (COND (#4# #3#) ('T T))))
             ('T NIL)))))) 

(DEFUN |HOAGG-;coerce;AOf;10| (|x| $)
  (PROG (#0=#:G1434 |a| #1=#:G1435)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (PROGN
                     (LETT #0# NIL |HOAGG-;coerce;AOf;10|)
                     (SEQ (LETT |a| NIL |HOAGG-;coerce;AOf;10|)
                          (LETT #1#
                                (SPADCALL |x| (|getShellEntry| $ 15))
                                |HOAGG-;coerce;AOf;10|)
                          G190
                          (COND
                            ((OR (ATOM #1#)
                                 (PROGN
                                   (LETT |a| (CAR #1#)
                                    |HOAGG-;coerce;AOf;10|)
                                   NIL))
                             (GO G191)))
                          (LETT #0#
                                (CONS (SPADCALL |a|
                                       (|getShellEntry| $ 39))
                                      #0#)
                                |HOAGG-;coerce;AOf;10|)
                          (LETT #1# (CDR #1#) |HOAGG-;coerce;AOf;10|)
                          (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                   (|getShellEntry| $ 41))
               (|getShellEntry| $ 42)))))) 

(DEFUN |HomogeneousAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|HomogeneousAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$|
              (LIST '|HomogeneousAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 44) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
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
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 3)
           (|setShellEntry| $ 13
               (CONS (|dispatchFunction| |HOAGG-;eval;ALA;1|) $))))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 18
                 (CONS (|dispatchFunction| |HOAGG-;#;ANni;2|) $))
             (|setShellEntry| $ 23
                 (CONS (|dispatchFunction| |HOAGG-;any?;MAB;3|) $))
             (|setShellEntry| $ 24
                 (CONS (|dispatchFunction| |HOAGG-;every?;MAB;4|) $))
             (|setShellEntry| $ 28
                 (CONS (|dispatchFunction| |HOAGG-;count;MANni;5|) $))
             (|setShellEntry| $ 29
                 (CONS (|dispatchFunction| |HOAGG-;members;AL;6|) $))
             (COND
               ((|testBitVector| |pv$| 4)
                (PROGN
                  (|setShellEntry| $ 32
                      (CONS (|dispatchFunction| |HOAGG-;count;SANni;7|)
                            $))
                  (|setShellEntry| $ 34
                      (CONS (|dispatchFunction| |HOAGG-;member?;SAB;8|)
                            $))
                  (|setShellEntry| $ 37
                      (CONS (|dispatchFunction| |HOAGG-;=;2AB;9|) $)))))
             (COND
               ((|testBitVector| |pv$| 5)
                (|setShellEntry| $ 43
                    (CONS (|dispatchFunction| |HOAGG-;coerce;AOf;10|)
                          $)))))))
        $)))) 

(MAKEPROP '|HomogeneousAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Equation| 7) (|List| 8) (0 . |eval|) (|Mapping| 7 7)
             (6 . |map|) (12 . |eval|) (|List| 7) (18 . |parts|)
             (|NonNegativeInteger|) (23 . |#|) (28 . |#|) (|Boolean|)
             (33 . |true|) (37 . |false|) (|Mapping| 19 7)
             (41 . |any?|) (47 . |every?|) (53 . |One|) (57 . +)
             (63 . |Zero|) (67 . |count|) (73 . |members|) (78 . =)
             (84 . |count|) (90 . |count|) (96 . |any?|)
             (102 . |member?|) (108 . |#|) (113 . |size?|) (119 . =)
             (|OutputForm|) (125 . |coerce|) (|List| $)
             (130 . |commaSeparate|) (135 . |bracket|)
             (140 . |coerce|))
          '#(|members| 145 |member?| 150 |every?| 156 |eval| 162
             |count| 168 |coerce| 180 |any?| 185 = 191 |#| 197)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 43
                                '(2 7 0 0 9 10 2 6 0 11 0 12 2 0 0 0 9
                                  13 1 6 14 0 15 1 14 16 0 17 1 0 16 0
                                  18 0 19 0 20 0 19 0 21 2 0 19 22 0 23
                                  2 0 19 22 0 24 0 16 0 25 2 16 0 0 0
                                  26 0 16 0 27 2 0 16 22 0 28 1 0 14 0
                                  29 2 7 19 0 0 30 2 6 16 22 0 31 2 0
                                  16 7 0 32 2 6 19 22 0 33 2 0 19 7 0
                                  34 1 6 16 0 35 2 6 19 0 16 36 2 0 19
                                  0 0 37 1 7 38 0 39 1 38 0 40 41 1 38
                                  0 0 42 1 0 38 0 43 1 0 14 0 29 2 0 19
                                  7 0 34 2 0 19 22 0 24 2 0 0 0 9 13 2
                                  0 16 7 0 32 2 0 16 22 0 28 1 0 38 0
                                  43 2 0 19 22 0 23 2 0 19 0 0 37 1 0
                                  16 0 18)))))
          '|lookupComplete|)) 
