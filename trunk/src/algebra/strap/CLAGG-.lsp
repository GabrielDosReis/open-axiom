
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
  (PROG (|x| #0=#:G1429 #1=#:G1403 #2=#:G1401 #3=#:G1402)
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
  (PROG (|x| #0=#:G1430 #1=#:G1408 #2=#:G1406 #3=#:G1407)
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
  (PROG (|x| #0=#:G1431 #1=#:G1412 #2=#:G1410 #3=#:G1411)
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
      (|getShellEntry| $ 18))) 

(DEFUN |CLAGG-;reduce;MAS;6| (|f| |x| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
      (|getShellEntry| $ 21))) 

(DEFUN |CLAGG-;reduce;MA2S;7| (|f| |x| |s| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9)) |s|
      (|getShellEntry| $ 23))) 

(DEFUN |CLAGG-;remove;M2A;8| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
                (|getShellEntry| $ 25))
            (|getShellEntry| $ 26))) 

(DEFUN |CLAGG-;select;M2A;9| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
                (|getShellEntry| $ 28))
            (|getShellEntry| $ 26))) 

(DEFUN |CLAGG-;remove;S2A;10| (|s| |x| $)
  (SPADCALL (CONS #'|CLAGG-;remove;S2A;10!0| (VECTOR $ |s|)) |x|
      (|getShellEntry| $ 31))) 

(DEFUN |CLAGG-;remove;S2A;10!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 30))) 

(DEFUN |CLAGG-;reduce;MA3S;11| (|f| |x| |s1| |s2| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9)) |s1| |s2|
      (|getShellEntry| $ 33))) 

(DEFUN |CLAGG-;removeDuplicates;2A;12| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 9))
          (|getShellEntry| $ 35))
      (|getShellEntry| $ 26))) 

(DEFUN |Collection&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Collection&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|Collection&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 37) . #0#)
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
             (|setShellEntry| $ 11
                 (CONS (|dispatchFunction| |CLAGG-;#;ANni;1|) $))
             (|setShellEntry| $ 14
                 (CONS (|dispatchFunction| |CLAGG-;count;MANni;2|) $))
             (|setShellEntry| $ 15
                 (CONS (|dispatchFunction| |CLAGG-;any?;MAB;3|) $))
             (|setShellEntry| $ 16
                 (CONS (|dispatchFunction| |CLAGG-;every?;MAB;4|) $))
             (|setShellEntry| $ 19
                 (CONS (|dispatchFunction| |CLAGG-;find;MAU;5|) $))
             (|setShellEntry| $ 22
                 (CONS (|dispatchFunction| |CLAGG-;reduce;MAS;6|) $))
             (|setShellEntry| $ 24
                 (CONS (|dispatchFunction| |CLAGG-;reduce;MA2S;7|) $))
             (|setShellEntry| $ 27
                 (CONS (|dispatchFunction| |CLAGG-;remove;M2A;8|) $))
             (|setShellEntry| $ 29
                 (CONS (|dispatchFunction| |CLAGG-;select;M2A;9|) $))
             (COND
               ((|testBitVector| |pv$| 2)
                (PROGN
                  (|setShellEntry| $ 32
                      (CONS (|dispatchFunction| |CLAGG-;remove;S2A;10|)
                            $))
                  (|setShellEntry| $ 34
                      (CONS (|dispatchFunction|
                                |CLAGG-;reduce;MA3S;11|)
                            $))
                  (|setShellEntry| $ 36
                      (CONS (|dispatchFunction|
                                |CLAGG-;removeDuplicates;2A;12|)
                            $))))))))
        $)))) 

(MAKEPROP '|Collection&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|List| 7) (0 . |parts|) (|NonNegativeInteger|) (5 . |#|)
             (|Boolean|) (|Mapping| 12 7) (10 . |count|) (16 . |any?|)
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
                                '(1 6 8 0 9 1 0 10 0 11 2 0 10 13 0 14
                                  2 0 12 13 0 15 2 0 12 13 0 16 2 8 17
                                  13 0 18 2 0 17 13 0 19 2 8 7 20 0 21
                                  2 0 7 20 0 22 3 8 7 20 0 7 23 3 0 7
                                  20 0 7 24 2 8 0 13 0 25 1 6 0 8 26 2
                                  0 0 13 0 27 2 8 0 13 0 28 2 0 0 13 0
                                  29 2 7 12 0 0 30 2 6 0 13 0 31 2 0 0
                                  7 0 32 4 8 7 20 0 7 7 33 4 0 7 20 0 7
                                  7 34 1 8 0 0 35 1 0 0 0 36 2 0 0 13 0
                                  29 1 0 0 0 36 2 0 0 7 0 32 2 0 0 13 0
                                  27 4 0 7 20 0 7 7 34 3 0 7 20 0 7 24
                                  2 0 7 20 0 22 2 0 17 13 0 19 2 0 12
                                  13 0 16 2 0 10 13 0 14 2 0 12 13 0 15
                                  1 0 10 0 11)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Collection&| '|isFunctor|
             '(((|removeDuplicates| ($ $)) T (ELT $ 36))
               ((|remove| ($ |#2| $)) T (ELT $ 32))
               ((|reduce|
                    (|#2| (|Mapping| |#2| |#2| |#2|) $ |#2| |#2|))
                T (ELT $ 34))
               ((|select| ($ (|Mapping| (|Boolean|) |#2|) $)) T
                (ELT $ 29))
               ((|remove| ($ (|Mapping| (|Boolean|) |#2|) $)) T
                (ELT $ 27))
               ((|reduce| (|#2| (|Mapping| |#2| |#2| |#2|) $ |#2|)) T
                (ELT $ 24))
               ((|reduce| (|#2| (|Mapping| |#2| |#2| |#2|) $)) T
                (ELT $ 22))
               ((|find| ((|Union| |#2| "failed")
                         (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 19))
               ((|count| ((|NonNegativeInteger|) |#2| $)) T
                (ELT $ NIL))
               ((|count| ((|NonNegativeInteger|)
                          (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 14))
               ((|every?| ((|Boolean|) (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 16))
               ((|any?| ((|Boolean|) (|Mapping| (|Boolean|) |#2|) $)) T
                (ELT $ 15))
               ((|#| ((|NonNegativeInteger|) $)) T (ELT $ 11)))
             (|addModemap| '|Collection&| '(|Collection&| |#1| |#2|)
                 '((CATEGORY |domain|
                       (SIGNATURE |removeDuplicates| (|#1| |#1|))
                       (SIGNATURE |remove| (|#1| |#2| |#1|))
                       (SIGNATURE |reduce|
                           (|#2| (|Mapping| |#2| |#2| |#2|) |#1| |#2|
                                 |#2|))
                       (SIGNATURE |select|
                           (|#1| (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |remove|
                           (|#1| (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |reduce|
                           (|#2| (|Mapping| |#2| |#2| |#2|) |#1| |#2|))
                       (SIGNATURE |reduce|
                           (|#2| (|Mapping| |#2| |#2| |#2|) |#1|))
                       (SIGNATURE |find|
                           ((|Union| |#2| "failed")
                            (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |count|
                           ((|NonNegativeInteger|) |#2| |#1|))
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
                   (|Collection| |#2|) (|Type|))
                 T '|Collection&|
                 (|put| '|Collection&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |removeDuplicates|
                                     (|#1| |#1|))
                                 (SIGNATURE |remove| (|#1| |#2| |#1|))
                                 (SIGNATURE |reduce|
                                     (|#2| (|Mapping| |#2| |#2| |#2|)
                                      |#1| |#2| |#2|))
                                 (SIGNATURE |select|
                                     (|#1| (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |remove|
                                     (|#1| (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |reduce|
                                     (|#2| (|Mapping| |#2| |#2| |#2|)
                                      |#1| |#2|))
                                 (SIGNATURE |reduce|
                                     (|#2| (|Mapping| |#2| |#2| |#2|)
                                      |#1|))
                                 (SIGNATURE |find|
                                     ((|Union| |#2| "failed")
                                      (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |count|
                                     ((|NonNegativeInteger|) |#2| |#1|))
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
                             (|Collection| |#2|) (|Type|))
                        |$CategoryFrame|)))) 
