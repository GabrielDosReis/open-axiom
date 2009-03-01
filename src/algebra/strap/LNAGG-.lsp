
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
  (PROG (#0=#:G1411 |i| #1=#:G1412)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |LNAGG-;indices;AL;1|)
             (SEQ (LETT |i| (SPADCALL |a| (|getShellEntry| $ 9))
                        |LNAGG-;indices;AL;1|)
                  (LETT #1# (SPADCALL |a| (|getShellEntry| $ 10))
                        |LNAGG-;indices;AL;1|)
                  G190 (COND ((> |i| #1#) (GO G191)))
                  (SEQ (EXIT (LETT #0# (CONS |i| #0#)
                                   |LNAGG-;indices;AL;1|)))
                  (LETT |i| (+ |i| 1) |LNAGG-;indices;AL;1|) (GO G190)
                  G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |LNAGG-;index?;IAB;2| (|i| |a| $)
  (COND
    ((< |i| (SPADCALL |a| (|getShellEntry| $ 9))) 'NIL)
    ('T (NOT (< (SPADCALL |a| (|getShellEntry| $ 10)) |i|))))) 

(DEFUN |LNAGG-;concat;ASA;3| (|a| |x| $)
  (SPADCALL |a| (SPADCALL 1 |x| (|getShellEntry| $ 20))
      (|getShellEntry| $ 21))) 

(DEFUN |LNAGG-;concat;S2A;4| (|x| |y| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 20)) |y|
      (|getShellEntry| $ 21))) 

(DEFUN |LNAGG-;insert;SAIA;5| (|x| |a| |i| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 20)) |a| |i|
      (|getShellEntry| $ 24))) 

(DEFUN |LNAGG-;maxIndex;AI;6| (|l| $)
  (+ (- (SPADCALL |l| (|getShellEntry| $ 26)) 1)
     (SPADCALL |l| (|getShellEntry| $ 9)))) 

(DEFUN |LinearAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|LinearAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|LinearAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 31) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasAttribute| |#1| '|shallowlyMutable|))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|finiteAggregate|)
           (|setShellEntry| $ 29
               (CONS (|dispatchFunction| |LNAGG-;maxIndex;AI;6|) $))))
        $)))) 

(MAKEPROP '|LinearAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Integer|) (0 . |minIndex|) (5 . |maxIndex|) (|List| 8)
             |LNAGG-;indices;AL;1| (|Boolean|) (10 . <) (16 . |false|)
             |LNAGG-;index?;IAB;2| (|NonNegativeInteger|) (20 . |One|)
             (24 . |One|) (28 . |new|) (34 . |concat|)
             |LNAGG-;concat;ASA;3| |LNAGG-;concat;S2A;4|
             (40 . |insert|) |LNAGG-;insert;SAIA;5| (47 . |#|) (52 . -)
             (58 . +) (64 . |maxIndex|) (|List| $))
          '#(|maxIndex| 69 |insert| 74 |indices| 81 |index?| 86
             |concat| 92)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 29
                                '(1 6 8 0 9 1 6 8 0 10 2 8 13 0 0 14 0
                                  13 0 15 0 17 0 18 0 8 0 19 2 6 0 17 7
                                  20 2 6 0 0 0 21 3 6 0 0 0 8 24 1 6 17
                                  0 26 2 8 0 0 0 27 2 8 0 0 0 28 1 0 8
                                  0 29 1 0 8 0 29 3 0 0 7 0 8 25 1 0 11
                                  0 12 2 0 13 8 0 16 2 0 0 0 7 22 2 0 0
                                  7 0 23)))))
          '|lookupComplete|)) 
