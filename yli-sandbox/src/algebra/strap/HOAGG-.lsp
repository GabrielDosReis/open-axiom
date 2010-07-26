
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

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |HOAGG-;=;2AB;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |HOAGG-;count;SANni;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |HOAGG-;member?;SAB;9|)) 

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
  (LET ((#0=#:G1405 NIL) (#1=#:G1406 T)
        (#2=#:G1428 (SPADCALL |c| (|getShellEntry| $ 15))))
    (LOOP
      (COND
        ((ATOM #2#) (RETURN (COND (#1# NIL) (T #0#))))
        (T (LET ((|x| (CAR #2#)))
             (LET ((#3=#:G1404 (SPADCALL |x| |f|)))
               (COND (#1# (SETQ #0# #3#)) (T (SETQ #0# (OR #0# #3#))))
               (SETQ #1# NIL)))))
      (SETQ #2# (CDR #2#))))) 

(DEFUN |HOAGG-;every?;MAB;4| (|f| |c| $)
  (LET ((#0=#:G1410 NIL) (#1=#:G1411 T)
        (#2=#:G1429 (SPADCALL |c| (|getShellEntry| $ 15))))
    (LOOP
      (COND
        ((ATOM #2#) (RETURN (COND (#1# T) (T #0#))))
        (T (LET ((|x| (CAR #2#)))
             (LET ((#3=#:G1409 (SPADCALL |x| |f|)))
               (COND
                 (#1# (SETQ #0# #3#))
                 (T (SETQ #0# (AND #0# #3#))))
               (SETQ #1# NIL)))))
      (SETQ #2# (CDR #2#))))) 

(DEFUN |HOAGG-;count;MANni;5| (|f| |c| $)
  (LET ((#0=#:G1414 NIL) (#1=#:G1415 T)
        (#2=#:G1430 (SPADCALL |c| (|getShellEntry| $ 15))))
    (LOOP
      (COND
        ((ATOM #2#) (RETURN (COND (#1# 0) (T #0#))))
        (T (LET ((|x| (CAR #2#)))
             (AND (SPADCALL |x| |f|)
                  (LET ((#3=#:G1413 1))
                    (COND
                      (#1# (SETQ #0# #3#))
                      (T (SETQ #0# (+ #0# #3#))))
                    (SETQ #1# NIL))))))
      (SETQ #2# (CDR #2#))))) 

(DEFUN |HOAGG-;members;AL;6| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 15))) 

(DEFUN |HOAGG-;=;2AB;7| (|x| |y| $)
  (COND
    ((SPADCALL |x| (SPADCALL |y| (|getShellEntry| $ 32))
         (|getShellEntry| $ 33))
     (LET ((#0=#:G1419 NIL) (#1=#:G1420 T)
           (#2=#:G1431 (SPADCALL |x| (|getShellEntry| $ 15)))
           (#3=#:G1432 (SPADCALL |y| (|getShellEntry| $ 15))))
       (LOOP
         (COND
           ((OR (ATOM #2#) (ATOM #3#)) (RETURN (COND (#1# T) (T #0#))))
           (T (LET ((|a| (CAR #2#)) (|b| (CAR #3#)))
                (LET ((#4=#:G1418
                          (SPADCALL |a| |b| (|getShellEntry| $ 34))))
                  (COND
                    (#1# (SETQ #0# #4#))
                    (T (SETQ #0# (AND #0# #4#))))
                  (SETQ #1# NIL)))))
         (SETQ #2# (CDR #2#))
         (SETQ #3# (CDR #3#)))))
    (T NIL))) 

(DEFUN |HOAGG-;count;SANni;8| (|s| |x| $)
  (SPADCALL (CONS #'|HOAGG-;count;SANni;8!0| (VECTOR $ |s|)) |x|
      (|getShellEntry| $ 36))) 

(DEFUN |HOAGG-;count;SANni;8!0| (|#1| $$)
  (SPADCALL (|getShellEntry| $$ 1) |#1|
      (|getShellEntry| (|getShellEntry| $$ 0) 34))) 

(DEFUN |HOAGG-;member?;SAB;9| (|e| |c| $)
  (SPADCALL (CONS #'|HOAGG-;member?;SAB;9!0| (VECTOR $ |e|)) |c|
      (|getShellEntry| $ 38))) 

(DEFUN |HOAGG-;member?;SAB;9!0| (|#1| $$)
  (SPADCALL (|getShellEntry| $$ 1) |#1|
      (|getShellEntry| (|getShellEntry| $$ 0) 34))) 

(DEFUN |HOAGG-;coerce;AOf;10| (|x| $)
  (SPADCALL
      (SPADCALL
          (LET ((#0=#:G1434 (SPADCALL |x| (|getShellEntry| $ 15)))
                (#1=#:G1433 NIL))
            (LOOP
              (COND
                ((ATOM #0#) (RETURN (NREVERSE #1#)))
                (T (LET ((|a| (CAR #0#)))
                     (SETQ #1#
                           (CONS (SPADCALL |a| (|getShellEntry| $ 41))
                                 #1#)))))
              (SETQ #0# (CDR #0#))))
          (|getShellEntry| $ 43))
      (|getShellEntry| $ 44))) 

(DEFUN |HomogeneousAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|HomogeneousAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 46))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasAttribute| |#1| '|finiteAggregate|)
                          (|HasAttribute| |#1| '|shallowlyMutable|)
                          (|HasCategory| |#2|
                              (LIST '|Evalable| (|devaluate| |#2|)))
                          (|HasCategory| |#2| '(|SetCategory|))
                          (|HasCategory| |#2| '(|BasicType|))
                          (|HasCategory| |#2|
                              (LIST '|CoercibleTo| '(|OutputForm|)))))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
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
         (|setShellEntry| $ 26
             (CONS (|dispatchFunction| |HOAGG-;every?;MAB;4|) $))
         (|setShellEntry| $ 30
             (CONS (|dispatchFunction| |HOAGG-;count;MANni;5|) $))
         (|setShellEntry| $ 31
             (CONS (|dispatchFunction| |HOAGG-;members;AL;6|) $))
         (COND
           ((|testBitVector| |pv$| 5)
            (|setShellEntry| $ 35
                (CONS (|dispatchFunction| |HOAGG-;=;2AB;7|) $))))
         (COND
           ((|testBitVector| |pv$| 4)
            (PROGN
              (|setShellEntry| $ 37
                  (CONS (|dispatchFunction| |HOAGG-;count;SANni;8|) $))
              (|setShellEntry| $ 39
                  (CONS (|dispatchFunction| |HOAGG-;member?;SAB;9|) $)))))
         (COND
           ((|testBitVector| |pv$| 6)
            (|setShellEntry| $ 45
                (CONS (|dispatchFunction| |HOAGG-;coerce;AOf;10|) $)))))))
    $)) 

(MAKEPROP '|HomogeneousAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Equation| 7) (|List| 8) (0 . |eval|) (|Mapping| 7 7)
             (6 . |map|) (12 . |eval|) (|List| 7) (18 . |parts|)
             (|NonNegativeInteger|) (23 . |#|) (28 . |#|) (|Boolean|)
             (33 . |or|) (39 . |false|) (|Mapping| 19 7) (43 . |any?|)
             (49 . |and|) (55 . |true|) (59 . |every?|) (65 . |One|)
             (69 . +) (75 . |Zero|) (79 . |count|) (85 . |members|)
             (90 . |#|) (95 . |size?|) (101 . =) (107 . =)
             (113 . |count|) (119 . |count|) (125 . |any?|)
             (131 . |member?|) (|OutputForm|) (137 . |coerce|)
             (|List| $) (142 . |commaSeparate|) (147 . |bracket|)
             (152 . |coerce|))
          '#(|members| 157 |member?| 162 |every?| 168 |eval| 174
             |count| 180 |coerce| 192 |any?| 197 = 203 |#| 209)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 45
                                '(2 7 0 0 9 10 2 6 0 11 0 12 2 0 0 0 9
                                  13 1 6 14 0 15 1 14 16 0 17 1 0 16 0
                                  18 2 19 0 0 0 20 0 19 0 21 2 0 19 22
                                  0 23 2 19 0 0 0 24 0 19 0 25 2 0 19
                                  22 0 26 0 16 0 27 2 16 0 0 0 28 0 16
                                  0 29 2 0 16 22 0 30 1 0 14 0 31 1 6
                                  16 0 32 2 6 19 0 16 33 2 7 19 0 0 34
                                  2 0 19 0 0 35 2 6 16 22 0 36 2 0 16 7
                                  0 37 2 6 19 22 0 38 2 0 19 7 0 39 1 7
                                  40 0 41 1 40 0 42 43 1 40 0 0 44 1 0
                                  40 0 45 1 0 14 0 31 2 0 19 7 0 39 2 0
                                  19 22 0 26 2 0 0 0 9 13 2 0 16 7 0 37
                                  2 0 16 22 0 30 1 0 40 0 45 2 0 19 22
                                  0 23 2 0 19 0 0 35 1 0 16 0 18)))))
          '|lookupComplete|)) 
