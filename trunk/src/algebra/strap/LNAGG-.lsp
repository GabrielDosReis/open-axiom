
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
  (SPADCALL |a| (SPADCALL 1 |x| (|getShellEntry| $ 16))
      (|getShellEntry| $ 17))) 

(DEFUN |LNAGG-;concat;S2A;4| (|x| |y| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 16)) |y|
      (|getShellEntry| $ 17))) 

(DEFUN |LNAGG-;insert;SAIA;5| (|x| |a| |i| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 16)) |a| |i|
      (|getShellEntry| $ 20))) 

(DEFUN |LNAGG-;maxIndex;AI;6| (|l| $)
  (+ (- (SPADCALL |l| (|getShellEntry| $ 22)) 1)
     (SPADCALL |l| (|getShellEntry| $ 9)))) 

(DEFUN |LinearAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|LinearAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|LinearAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 25) . #0#)
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
           (|setShellEntry| $ 23
               (CONS (|dispatchFunction| |LNAGG-;maxIndex;AI;6|) $))))
        $)))) 

(MAKEPROP '|LinearAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Integer|) (0 . |minIndex|) (5 . |maxIndex|) (|List| 8)
             |LNAGG-;indices;AL;1| (|Boolean|) |LNAGG-;index?;IAB;2|
             (|NonNegativeInteger|) (10 . |new|) (16 . |concat|)
             |LNAGG-;concat;ASA;3| |LNAGG-;concat;S2A;4|
             (22 . |insert|) |LNAGG-;insert;SAIA;5| (29 . |#|)
             (34 . |maxIndex|) (|List| $))
          '#(|maxIndex| 39 |insert| 44 |indices| 51 |index?| 56
             |concat| 62)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 23
                                '(1 6 8 0 9 1 6 8 0 10 2 6 0 15 7 16 2
                                  6 0 0 0 17 3 6 0 0 0 8 20 1 6 15 0 22
                                  1 0 8 0 23 1 0 8 0 23 3 0 0 7 0 8 21
                                  1 0 11 0 12 2 0 13 8 0 14 2 0 0 0 7
                                  18 2 0 0 7 0 19)))))
          '|lookupComplete|)) 
