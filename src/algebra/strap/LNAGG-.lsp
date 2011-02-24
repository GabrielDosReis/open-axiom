
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |LNAGG-;indices;AL;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Thing| |%Shell|) |%Boolean|)
                |LNAGG-;index?;IAB;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LNAGG-;concat;ASA;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LNAGG-;concat;S2A;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |LNAGG-;insert;SAIA;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |LNAGG-;maxIndex;AI;6|)) 

(DEFUN |LNAGG-;indices;AL;1| (|a| $)
  (LET ((|i| (SPADCALL |a| (|getShellEntry| $ 9)))
        (#0=#:G1387 (SPADCALL |a| (|getShellEntry| $ 10)))
        (#1=#:G1386 NIL))
    (LOOP
      (COND
        ((> |i| #0#) (RETURN (NREVERSE #1#)))
        (T (SETQ #1# (CONS |i| #1#))))
      (SETQ |i| (+ |i| 1))))) 

(DEFUN |LNAGG-;index?;IAB;2| (|i| |a| $)
  (COND
    ((NOT (< |i| (SPADCALL |a| (|getShellEntry| $ 9))))
     (NOT (< (SPADCALL |a| (|getShellEntry| $ 10)) |i|)))
    (T NIL))) 

(DEFUN |LNAGG-;concat;ASA;3| (|a| |x| $)
  (SPADCALL |a| (SPADCALL 1 |x| (|getShellEntry| $ 22))
      (|getShellEntry| $ 23))) 

(DEFUN |LNAGG-;concat;S2A;4| (|x| |y| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 22)) |y|
      (|getShellEntry| $ 23))) 

(DEFUN |LNAGG-;insert;SAIA;5| (|x| |a| |i| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 22)) |a| |i|
      (|getShellEntry| $ 26))) 

(DEFUN |LNAGG-;maxIndex;AI;6| (|l| $)
  (+ (- (SPADCALL |l| (|getShellEntry| $ 28)) 1)
     (SPADCALL |l| (|getShellEntry| $ 9)))) 

(DEFUN |LinearAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|LinearAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 33))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasAttribute| |#1| '|shallowlyMutable|)))))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    (SETF (|shellEntry| $ 7) |#2|)
    (COND
      ((|HasAttribute| |#1| '|finiteAggregate|)
       (SETF (|shellEntry| $ 31)
             (CONS (|dispatchFunction| |LNAGG-;maxIndex;AI;6|) $))))
    $)) 

(MAKEPROP '|LinearAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Integer|) (0 . |minIndex|) (5 . |maxIndex|) (|List| 8)
             |LNAGG-;indices;AL;1| (|Boolean|) (10 . >=) (16 . >)
             (22 . |not|) (27 . |false|) |LNAGG-;index?;IAB;2|
             (|NonNegativeInteger|) (31 . |One|) (35 . |One|)
             (39 . |new|) (45 . |concat|) |LNAGG-;concat;ASA;3|
             |LNAGG-;concat;S2A;4| (51 . |insert|)
             |LNAGG-;insert;SAIA;5| (58 . |#|) (63 . -) (69 . +)
             (75 . |maxIndex|) (|List| $))
          '#(|maxIndex| 80 |insert| 85 |indices| 92 |index?| 97
             |concat| 103)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 31
                                '(1 6 8 0 9 1 6 8 0 10 2 8 13 0 0 14 2
                                  8 13 0 0 15 1 13 0 0 16 0 13 0 17 0
                                  19 0 20 0 8 0 21 2 6 0 19 7 22 2 6 0
                                  0 0 23 3 6 0 0 0 8 26 1 6 19 0 28 2 8
                                  0 0 0 29 2 8 0 0 0 30 1 0 8 0 31 1 0
                                  8 0 31 3 0 0 7 0 8 27 1 0 11 0 12 2 0
                                  13 8 0 18 2 0 0 0 7 24 2 0 0 7 0 25)))))
          '|lookupComplete|)) 
