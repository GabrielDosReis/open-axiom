
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
  (LET ((#0=#:G1402 NIL) (#1=#:G1403 T)
        (#2=#:G1429 (SPADCALL |c| (|getShellEntry| $ 9))))
    (LOOP
      (COND
        ((ATOM #2#) (RETURN (COND (#1# 0) (T #0#))))
        (T (LET ((|x| (CAR #2#)))
             (AND (SPADCALL |x| |f|)
                  (LET ((#3=#:G1401 1))
                    (COND
                      (#1# (SETQ #0# #3#))
                      (T (SETQ #0# (+ #0# #3#))))
                    (SETQ #1# NIL))))))
      (SETQ #2# (CDR #2#))))) 

(DEFUN |CLAGG-;any?;MAB;3| (|f| |c| $)
  (LET ((#0=#:G1407 NIL) (#1=#:G1408 T)
        (#2=#:G1430 (SPADCALL |c| (|getShellEntry| $ 9))))
    (LOOP
      (COND
        ((ATOM #2#) (RETURN (COND (#1# NIL) (T #0#))))
        (T (LET ((|x| (CAR #2#)))
             (LET ((#3=#:G1406 (SPADCALL |x| |f|)))
               (COND (#1# (SETQ #0# #3#)) (T (SETQ #0# (OR #0# #3#))))
               (SETQ #1# NIL)))))
      (SETQ #2# (CDR #2#))))) 

(DEFUN |CLAGG-;every?;MAB;4| (|f| |c| $)
  (LET ((#0=#:G1411 NIL) (#1=#:G1412 T)
        (#2=#:G1431 (SPADCALL |c| (|getShellEntry| $ 9))))
    (LOOP
      (COND
        ((ATOM #2#) (RETURN (COND (#1# T) (T #0#))))
        (T (LET ((|x| (CAR #2#)))
             (LET ((#3=#:G1410 (SPADCALL |x| |f|)))
               (COND
                 (#1# (SETQ #0# #3#))
                 (T (SETQ #0# (AND #0# #3#))))
               (SETQ #1# NIL)))))
      (SETQ #2# (CDR #2#))))) 

(DEFUN |CLAGG-;find;MAU;5| (|f| |c| $)
  (SPADCALL |f| (SPADCALL |c| (|getShellEntry| $ 9))
      (|getShellEntry| $ 26))) 

(DEFUN |CLAGG-;reduce;MAS;6| (|f| |x| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
      (|getShellEntry| $ 29))) 

(DEFUN |CLAGG-;reduce;MA2S;7| (|f| |x| |s| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9)) |s|
      (|getShellEntry| $ 31))) 

(DEFUN |CLAGG-;remove;M2A;8| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
                (|getShellEntry| $ 33))
            (|getShellEntry| $ 34))) 

(DEFUN |CLAGG-;select;M2A;9| (|f| |x| $)
  (SPADCALL (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9))
                (|getShellEntry| $ 36))
            (|getShellEntry| $ 34))) 

(DEFUN |CLAGG-;remove;S2A;10| (|s| |x| $)
  (SPADCALL (CONS #'|CLAGG-;remove;S2A;10!0| (VECTOR $ |s|)) |x|
      (|getShellEntry| $ 39))) 

(DEFUN |CLAGG-;remove;S2A;10!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 38))) 

(DEFUN |CLAGG-;reduce;MA3S;11| (|f| |x| |s1| |s2| $)
  (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 9)) |s1| |s2|
      (|getShellEntry| $ 41))) 

(DEFUN |CLAGG-;removeDuplicates;2A;12| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 9))
          (|getShellEntry| $ 43))
      (|getShellEntry| $ 34))) 

(DEFUN |Collection&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|Collection&| |dv$1| |dv$2|))
         ($ (|newShell| 45))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasCategory| |#2|
                              (LIST '|ConvertibleTo| '(|InputForm|)))
                          (|HasCategory| |#2| '(|SetCategory|))
                          (|HasAttribute| |#1| '|finiteAggregate|)))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
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
         (|setShellEntry| $ 24
             (CONS (|dispatchFunction| |CLAGG-;every?;MAB;4|) $))
         (|setShellEntry| $ 27
             (CONS (|dispatchFunction| |CLAGG-;find;MAU;5|) $))
         (|setShellEntry| $ 30
             (CONS (|dispatchFunction| |CLAGG-;reduce;MAS;6|) $))
         (|setShellEntry| $ 32
             (CONS (|dispatchFunction| |CLAGG-;reduce;MA2S;7|) $))
         (|setShellEntry| $ 35
             (CONS (|dispatchFunction| |CLAGG-;remove;M2A;8|) $))
         (|setShellEntry| $ 37
             (CONS (|dispatchFunction| |CLAGG-;select;M2A;9|) $))
         (COND
           ((|testBitVector| |pv$| 2)
            (PROGN
              (|setShellEntry| $ 40
                  (CONS (|dispatchFunction| |CLAGG-;remove;S2A;10|) $))
              (|setShellEntry| $ 42
                  (CONS (|dispatchFunction| |CLAGG-;reduce;MA3S;11|) $))
              (|setShellEntry| $ 44
                  (CONS (|dispatchFunction|
                            |CLAGG-;removeDuplicates;2A;12|)
                        $))))))))
    $)) 

(MAKEPROP '|Collection&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|List| 7) (0 . |parts|) (|NonNegativeInteger|) (5 . |#|)
             (10 . |#|) (15 . |One|) (19 . +) (25 . |Zero|) (|Boolean|)
             (|Mapping| 16 7) (29 . |count|) (35 . |or|) (41 . |false|)
             (45 . |any?|) (51 . |and|) (57 . |true|) (61 . |every?|)
             (|Union| 7 '"failed") (67 . |find|) (73 . |find|)
             (|Mapping| 7 7 7) (79 . |reduce|) (85 . |reduce|)
             (91 . |reduce|) (98 . |reduce|) (105 . |remove|)
             (111 . |construct|) (116 . |remove|) (122 . |select|)
             (128 . |select|) (134 . =) (140 . |remove|)
             (146 . |remove|) (152 . |reduce|) (160 . |reduce|)
             (168 . |removeDuplicates|) (173 . |removeDuplicates|))
          '#(|select| 178 |removeDuplicates| 184 |remove| 189 |reduce|
             201 |find| 222 |every?| 228 |count| 234 |any?| 240 |#|
             246)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 44
                                '(1 6 8 0 9 1 8 10 0 11 1 0 10 0 12 0
                                  10 0 13 2 10 0 0 0 14 0 10 0 15 2 0
                                  10 17 0 18 2 16 0 0 0 19 0 16 0 20 2
                                  0 16 17 0 21 2 16 0 0 0 22 0 16 0 23
                                  2 0 16 17 0 24 2 8 25 17 0 26 2 0 25
                                  17 0 27 2 8 7 28 0 29 2 0 7 28 0 30 3
                                  8 7 28 0 7 31 3 0 7 28 0 7 32 2 8 0
                                  17 0 33 1 6 0 8 34 2 0 0 17 0 35 2 8
                                  0 17 0 36 2 0 0 17 0 37 2 7 16 0 0 38
                                  2 6 0 17 0 39 2 0 0 7 0 40 4 8 7 28 0
                                  7 7 41 4 0 7 28 0 7 7 42 1 8 0 0 43 1
                                  0 0 0 44 2 0 0 17 0 37 1 0 0 0 44 2 0
                                  0 7 0 40 2 0 0 17 0 35 4 0 7 28 0 7 7
                                  42 3 0 7 28 0 7 32 2 0 7 28 0 30 2 0
                                  25 17 0 27 2 0 16 17 0 24 2 0 10 17 0
                                  18 2 0 16 17 0 21 1 0 10 0 12)))))
          '|lookupComplete|)) 