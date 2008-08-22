
(/VERSIONCHECK 2) 

(DEFUN |HOAGG-;eval;ALA;1| (|u| |l| $)
  (SPADCALL (CONS #'|HOAGG-;eval;ALA;1!0| (VECTOR $ |l|)) |u|
      (|getShellEntry| $ 12))) 

(DEFUN |HOAGG-;eval;ALA;1!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 10))) 

(DEFUN |HOAGG-;#;ANni;2| (|c| $)
  (LENGTH (SPADCALL |c| (|getShellEntry| $ 15)))) 

(DEFUN |HOAGG-;any?;MAB;3| (|f| |c| $)
  (PROG (|x| #0=#:G1406 #1=#:G1403 #2=#:G1401 #3=#:G1402)
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
  (PROG (|x| #0=#:G1411 #1=#:G1409 #2=#:G1407 #3=#:G1408)
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
  (PROG (|x| #0=#:G1416 #1=#:G1414 #2=#:G1412 #3=#:G1413)
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

(DEFUN |HOAGG-;members;AL;6| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 15))) 

(DEFUN |HOAGG-;count;SANni;7| (|s| |x| $)
  (SPADCALL (CONS #'|HOAGG-;count;SANni;7!0| (VECTOR $ |s|)) |x|
      (|getShellEntry| $ 25))) 

(DEFUN |HOAGG-;count;SANni;7!0| (|#1| $$)
  (SPADCALL (|getShellEntry| $$ 1) |#1|
      (|getShellEntry| (|getShellEntry| $$ 0) 24))) 

(DEFUN |HOAGG-;member?;SAB;8| (|e| |c| $)
  (SPADCALL (CONS #'|HOAGG-;member?;SAB;8!0| (VECTOR $ |e|)) |c|
      (|getShellEntry| $ 27))) 

(DEFUN |HOAGG-;member?;SAB;8!0| (|#1| $$)
  (SPADCALL (|getShellEntry| $$ 1) |#1|
      (|getShellEntry| (|getShellEntry| $$ 0) 24))) 

(DEFUN |HOAGG-;=;2AB;9| (|x| |y| $)
  (PROG (|b| #0=#:G1426 |a| #1=#:G1425 #2=#:G1422 #3=#:G1420
             #4=#:G1421)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (SPADCALL |y| (|getShellEntry| $ 29))
                  (|getShellEntry| $ 30))
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
                     (SEQ (EXIT (PROGN
                                  (LETT #2#
                                        (SPADCALL |a| |b|
                                         (|getShellEntry| $ 24))
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
  (PROG (#0=#:G1430 |a| #1=#:G1431)
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
                          (SEQ (EXIT (LETT #0#
                                      (CONS
                                       (SPADCALL |a|
                                        (|getShellEntry| $ 33))
                                       #0#)
                                      |HOAGG-;coerce;AOf;10|)))
                          (LETT #1# (CDR #1#) |HOAGG-;coerce;AOf;10|)
                          (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                   (|getShellEntry| $ 35))
               (|getShellEntry| $ 36)))))) 

(DEFUN |HomogeneousAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|HomogeneousAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$|
              (LIST '|HomogeneousAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 38) . #0#)
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
             (|setShellEntry| $ 17
                 (CONS (|dispatchFunction| |HOAGG-;#;ANni;2|) $))
             (|setShellEntry| $ 20
                 (CONS (|dispatchFunction| |HOAGG-;any?;MAB;3|) $))
             (|setShellEntry| $ 21
                 (CONS (|dispatchFunction| |HOAGG-;every?;MAB;4|) $))
             (|setShellEntry| $ 22
                 (CONS (|dispatchFunction| |HOAGG-;count;MANni;5|) $))
             (|setShellEntry| $ 23
                 (CONS (|dispatchFunction| |HOAGG-;members;AL;6|) $))
             (COND
               ((|testBitVector| |pv$| 4)
                (PROGN
                  (|setShellEntry| $ 26
                      (CONS (|dispatchFunction| |HOAGG-;count;SANni;7|)
                            $))
                  (|setShellEntry| $ 28
                      (CONS (|dispatchFunction| |HOAGG-;member?;SAB;8|)
                            $))
                  (|setShellEntry| $ 31
                      (CONS (|dispatchFunction| |HOAGG-;=;2AB;9|) $)))))
             (COND
               ((|testBitVector| |pv$| 5)
                (|setShellEntry| $ 37
                    (CONS (|dispatchFunction| |HOAGG-;coerce;AOf;10|)
                          $)))))))
        $)))) 

(MAKEPROP '|HomogeneousAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Equation| 7) (|List| 8) (0 . |eval|) (|Mapping| 7 7)
             (6 . |map|) (12 . |eval|) (|List| 7) (18 . |parts|)
             (|NonNegativeInteger|) (23 . |#|) (|Boolean|)
             (|Mapping| 18 7) (28 . |any?|) (34 . |every?|)
             (40 . |count|) (46 . |members|) (51 . =) (57 . |count|)
             (63 . |count|) (69 . |any?|) (75 . |member?|) (81 . |#|)
             (86 . |size?|) (92 . =) (|OutputForm|) (98 . |coerce|)
             (|List| $) (103 . |commaSeparate|) (108 . |bracket|)
             (113 . |coerce|))
          '#(|members| 118 |member?| 123 |every?| 129 |eval| 135
             |count| 141 |coerce| 153 |any?| 158 = 164 |#| 170)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 37
                                '(2 7 0 0 9 10 2 6 0 11 0 12 2 0 0 0 9
                                  13 1 6 14 0 15 1 0 16 0 17 2 0 18 19
                                  0 20 2 0 18 19 0 21 2 0 16 19 0 22 1
                                  0 14 0 23 2 7 18 0 0 24 2 6 16 19 0
                                  25 2 0 16 7 0 26 2 6 18 19 0 27 2 0
                                  18 7 0 28 1 6 16 0 29 2 6 18 0 16 30
                                  2 0 18 0 0 31 1 7 32 0 33 1 32 0 34
                                  35 1 32 0 0 36 1 0 32 0 37 1 0 14 0
                                  23 2 0 18 7 0 28 2 0 18 19 0 21 2 0 0
                                  0 9 13 2 0 16 7 0 26 2 0 16 19 0 22 1
                                  0 32 0 37 2 0 18 19 0 20 2 0 18 0 0
                                  31 1 0 16 0 17)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|HomogeneousAggregate&| '|isFunctor|
             '(((|coerce| ((|OutputForm|) $)) T (ELT $ 37))
               ((= ((|Boolean|) $ $)) T (ELT $ 31))
               ((|eval| ($ $ (|List| |#2|) (|List| |#2|))) T
                (ELT $ NIL))
               ((|eval| ($ $ |#2| |#2|)) T (ELT $ NIL))
               ((|eval| ($ $ (|Equation| |#2|))) T (ELT $ NIL))
               ((|eval| ($ $ (|List| (|Equation| |#2|)))) T (ELT $ 13))
               ((|member?| ((|Boolean|) |#2| $)) T (ELT $ 28))
               ((|count| ((|NonNegativeInteger|) |#2| $)) T (ELT $ 26))
               ((|members| ((|List| |#2|) $)) T (ELT $ 23))
               ((|count| ((|NonNegativeInteger|)
                          (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 22))
               ((|every?| ((|Boolean|) (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 21))
               ((|any?| ((|Boolean|) (|Mapping| (|Boolean|) |#2|) $)) T
                (ELT $ 20))
               ((|#| ((|NonNegativeInteger|) $)) T (ELT $ 17)))
             (|addModemap| '|HomogeneousAggregate&|
                 '(|HomogeneousAggregate&| |#1| |#2|)
                 '((CATEGORY |domain|
                       (SIGNATURE |coerce| ((|OutputForm|) |#1|))
                       (SIGNATURE = ((|Boolean|) |#1| |#1|))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| |#2|) (|List| |#2|)))
                       (SIGNATURE |eval| (|#1| |#1| |#2| |#2|))
                       (SIGNATURE |eval| (|#1| |#1| (|Equation| |#2|)))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|Equation| |#2|))))
                       (SIGNATURE |member?| ((|Boolean|) |#2| |#1|))
                       (SIGNATURE |count|
                           ((|NonNegativeInteger|) |#2| |#1|))
                       (SIGNATURE |members| ((|List| |#2|) |#1|))
                       (SIGNATURE |count|
                           ((|NonNegativeInteger|)
                            (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |every?|
                           ((|Boolean|) (|Mapping| (|Boolean|) |#2|)
                            |#1|))
                       (SIGNATURE |any?|
                           ((|Boolean|) (|Mapping| (|Boolean|) |#2|)
                            |#1|))
                       (SIGNATURE |#| ((|NonNegativeInteger|) |#1|)))
                   (|HomogeneousAggregate| |#2|) (|Type|))
                 T '|HomogeneousAggregate&|
                 (|put| '|HomogeneousAggregate&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |coerce|
                                     ((|OutputForm|) |#1|))
                                 (SIGNATURE = ((|Boolean|) |#1| |#1|))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|List| |#2|)
                                      (|List| |#2|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| |#2| |#2|))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|Equation| |#2|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1|
                                      (|List| (|Equation| |#2|))))
                                 (SIGNATURE |member?|
                                     ((|Boolean|) |#2| |#1|))
                                 (SIGNATURE |count|
                                     ((|NonNegativeInteger|) |#2| |#1|))
                                 (SIGNATURE |members|
                                     ((|List| |#2|) |#1|))
                                 (SIGNATURE |count|
                                     ((|NonNegativeInteger|)
                                      (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |every?|
                                     ((|Boolean|)
                                      (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |any?|
                                     ((|Boolean|)
                                      (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |#|
                                     ((|NonNegativeInteger|) |#1|)))
                             (|HomogeneousAggregate| |#2|) (|Type|))
                        |$CategoryFrame|)))) 
