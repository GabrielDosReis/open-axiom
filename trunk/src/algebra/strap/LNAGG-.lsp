
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
  (LET ((|i| (SPADCALL |a| (|shellEntry| $ 9)))
        (#0=#:G1387 (SPADCALL |a| (|shellEntry| $ 10)))
        (#1=#:G1386 NIL))
    (LOOP
      (COND
        ((> |i| #0#) (RETURN (NREVERSE #1#)))
        (T (SETQ #1# (CONS |i| #1#))))
      (SETQ |i| (+ |i| 1))))) 

(DEFUN |LNAGG-;index?;IAB;2| (|i| |a| $)
  (AND (NOT (< |i| (SPADCALL |a| (|shellEntry| $ 9))))
       (NOT (< (SPADCALL |a| (|shellEntry| $ 10)) |i|)))) 

(DEFUN |LNAGG-;concat;ASA;3| (|a| |x| $)
  (SPADCALL |a| (SPADCALL 1 |x| (|shellEntry| $ 21))
      (|shellEntry| $ 22))) 

(DEFUN |LNAGG-;concat;S2A;4| (|x| |y| $)
  (SPADCALL (SPADCALL 1 |x| (|shellEntry| $ 21)) |y|
      (|shellEntry| $ 22))) 

(DEFUN |LNAGG-;insert;SAIA;5| (|x| |a| |i| $)
  (SPADCALL (SPADCALL 1 |x| (|shellEntry| $ 21)) |a| |i|
      (|shellEntry| $ 25))) 

(DEFUN |LNAGG-;maxIndex;AI;6| (|l| $)
  (+ (- (SPADCALL |l| (|shellEntry| $ 27)) 1)
     (SPADCALL |l| (|shellEntry| $ 9)))) 

(DEFUN |LinearAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|LinearAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 32))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasAttribute| |#1| '|shallowlyMutable|)))))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    (SETF (|shellEntry| $ 7) |#2|)
    (COND
      ((|HasAttribute| |#1| '|finiteAggregate|)
       (SETF (|shellEntry| $ 30)
             (CONS (|dispatchFunction| |LNAGG-;maxIndex;AI;6|) $))))
    $)) 

(MAKEPROP '|LinearAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Integer|) (0 . |minIndex|) (5 . |maxIndex|) (|List| 8)
             |LNAGG-;indices;AL;1| (|Boolean|) (10 . >=) (16 . <=)
             (22 . |false|) |LNAGG-;index?;IAB;2|
             (|NonNegativeInteger|) (26 . |One|) (30 . |One|)
             (34 . |new|) (40 . |concat|) |LNAGG-;concat;ASA;3|
             |LNAGG-;concat;S2A;4| (46 . |insert|)
             |LNAGG-;insert;SAIA;5| (53 . |#|) (58 . -) (64 . +)
             (70 . |maxIndex|) (|List| $))
          '#(|maxIndex| 75 |insert| 80 |indices| 87 |index?| 92
             |concat| 98)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 30
                                '(1 6 8 0 9 1 6 8 0 10 2 8 13 0 0 14 2
                                  8 13 0 0 15 0 13 0 16 0 18 0 19 0 8 0
                                  20 2 6 0 18 7 21 2 6 0 0 0 22 3 6 0 0
                                  0 8 25 1 6 18 0 27 2 8 0 0 0 28 2 8 0
                                  0 0 29 1 0 8 0 30 1 0 8 0 30 3 0 0 7
                                  0 8 26 1 0 11 0 12 2 0 13 8 0 17 2 0
                                  0 0 7 23 2 0 0 7 0 24)))))
          '|lookupComplete|)) 
