
(/VERSIONCHECK 2) 

(DEFUN |ABELGRP-;-;3S;1| (|x| |y| $)
  (SPADCALL |x| (SPADCALL |y| (QREFELT $ 7)) (QREFELT $ 8))) 

(DEFUN |ABELGRP-;subtractIfCan;2SU;2| (|x| |y| $)
  (CONS 0 (SPADCALL |x| |y| (QREFELT $ 10)))) 

(DEFUN |ABELGRP-;*;Nni2S;3| (|n| |x| $)
  (SPADCALL |n| |x| (QREFELT $ 14))) 

(DEFUN |ABELGRP-;*;I2S;4| (|n| |x| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 17))
    ((< 0 |n|) (SPADCALL |n| |x| (QREFELT $ 20)))
    ('T (SPADCALL (- |n|) (SPADCALL |x| (QREFELT $ 7)) (QREFELT $ 20))))) 

(DEFUN |AbelianGroup&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|AbelianGroup&|))
        (LETT |dv$| (LIST '|AbelianGroup&| |dv$1|) . #0#)
        (LETT $ (GETREFV 22) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (COND
          ((|HasCategory| |#1| '(|Ring|)))
          ('T
           (QSETREFV $ 21
               (CONS (|dispatchFunction| |ABELGRP-;*;I2S;4|) $))))
        $)))) 

(MAKEPROP '|AbelianGroup&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . -) (5 . +)
             |ABELGRP-;-;3S;1| (11 . -) (|Union| $ '"failed")
             |ABELGRP-;subtractIfCan;2SU;2| (|Integer|) (17 . *)
             (|NonNegativeInteger|) |ABELGRP-;*;Nni2S;3| (23 . |Zero|)
             (|PositiveInteger|) (|RepeatedDoubling| 6) (27 . |double|)
             (33 . *))
          '#(|subtractIfCan| 39 - 45 * 51) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 21
                                '(1 6 0 0 7 2 6 0 0 0 8 2 6 0 0 0 10 2
                                  6 0 13 0 14 0 6 0 17 2 19 6 18 6 20 2
                                  0 0 13 0 21 2 0 11 0 0 12 2 0 0 0 0 9
                                  2 0 0 13 0 21 2 0 0 15 0 16)))))
          '|lookupComplete|)) 
