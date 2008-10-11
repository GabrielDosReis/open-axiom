
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
  (PROG (#0=#:G1410 |i| #1=#:G1411)
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
    ('T
     (SPADCALL (< (SPADCALL |a| (|getShellEntry| $ 10)) |i|)
         (|getShellEntry| $ 14))))) 

(DEFUN |LNAGG-;concat;ASA;3| (|a| |x| $)
  (SPADCALL |a| (SPADCALL 1 |x| (|getShellEntry| $ 17))
      (|getShellEntry| $ 18))) 

(DEFUN |LNAGG-;concat;S2A;4| (|x| |y| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 17)) |y|
      (|getShellEntry| $ 18))) 

(DEFUN |LNAGG-;insert;SAIA;5| (|x| |a| |i| $)
  (SPADCALL (SPADCALL 1 |x| (|getShellEntry| $ 17)) |a| |i|
      (|getShellEntry| $ 21))) 

(DEFUN |LNAGG-;maxIndex;AI;6| (|l| $)
  (+ (- (SPADCALL |l| (|getShellEntry| $ 23)) 1)
     (SPADCALL |l| (|getShellEntry| $ 9)))) 

(DEFUN |LinearAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|LinearAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|LinearAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 26) . #0#)
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
           (|setShellEntry| $ 24
               (CONS (|dispatchFunction| |LNAGG-;maxIndex;AI;6|) $))))
        $)))) 

(MAKEPROP '|LinearAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Integer|) (0 . |minIndex|) (5 . |maxIndex|) (|List| 8)
             |LNAGG-;indices;AL;1| (|Boolean|) (10 . |not|)
             |LNAGG-;index?;IAB;2| (|NonNegativeInteger|) (15 . |new|)
             (21 . |concat|) |LNAGG-;concat;ASA;3|
             |LNAGG-;concat;S2A;4| (27 . |insert|)
             |LNAGG-;insert;SAIA;5| (34 . |#|) (39 . |maxIndex|)
             (|List| $))
          '#(|maxIndex| 44 |insert| 49 |indices| 56 |index?| 61
             |concat| 67)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 24
                                '(1 6 8 0 9 1 6 8 0 10 1 13 0 0 14 2 6
                                  0 16 7 17 2 6 0 0 0 18 3 6 0 0 0 8 21
                                  1 6 16 0 23 1 0 8 0 24 1 0 8 0 24 3 0
                                  0 7 0 8 22 1 0 11 0 12 2 0 13 8 0 15
                                  2 0 0 0 7 19 2 0 0 7 0 20)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|LinearAggregate&| '|isFunctor|
             '(((|insert| ($ $ $ (|Integer|))) T (ELT $ NIL))
               ((|insert| ($ |#2| $ (|Integer|))) T (ELT $ 22))
               ((|concat| ($ (|List| $))) T (ELT $ NIL))
               ((|concat| ($ $ $)) T (ELT $ NIL))
               ((|concat| ($ |#2| $)) T (ELT $ 20))
               ((|concat| ($ $ |#2|)) T (ELT $ 19))
               ((|maxIndex| ((|Integer|) $)) T (ELT $ 24))
               ((|indices| ((|List| (|Integer|)) $)) T (ELT $ 12))
               ((|index?| ((|Boolean|) (|Integer|) $)) T (ELT $ 15)))
             (|addModemap| '|LinearAggregate&|
                 '(|LinearAggregate&| |#1| |#2|)
                 '((CATEGORY |domain|
                       (SIGNATURE |insert|
                           (|#1| |#1| |#1| (|Integer|)))
                       (SIGNATURE |insert|
                           (|#1| |#2| |#1| (|Integer|)))
                       (SIGNATURE |concat| (|#1| (|List| |#1|)))
                       (SIGNATURE |concat| (|#1| |#1| |#1|))
                       (SIGNATURE |concat| (|#1| |#2| |#1|))
                       (SIGNATURE |concat| (|#1| |#1| |#2|))
                       (SIGNATURE |maxIndex| ((|Integer|) |#1|))
                       (SIGNATURE |indices|
                           ((|List| (|Integer|)) |#1|))
                       (SIGNATURE |index?|
                           ((|Boolean|) (|Integer|) |#1|)))
                   (|LinearAggregate| |#2|) (|Type|))
                 T '|LinearAggregate&|
                 (|put| '|LinearAggregate&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |insert|
                                     (|#1| |#1| |#1| (|Integer|)))
                                 (SIGNATURE |insert|
                                     (|#1| |#2| |#1| (|Integer|)))
                                 (SIGNATURE |concat|
                                     (|#1| (|List| |#1|)))
                                 (SIGNATURE |concat| (|#1| |#1| |#1|))
                                 (SIGNATURE |concat| (|#1| |#2| |#1|))
                                 (SIGNATURE |concat| (|#1| |#1| |#2|))
                                 (SIGNATURE |maxIndex|
                                     ((|Integer|) |#1|))
                                 (SIGNATURE |indices|
                                     ((|List| (|Integer|)) |#1|))
                                 (SIGNATURE |index?|
                                     ((|Boolean|) (|Integer|) |#1|)))
                             (|LinearAggregate| |#2|) (|Type|))
                        |$CategoryFrame|)))) 
