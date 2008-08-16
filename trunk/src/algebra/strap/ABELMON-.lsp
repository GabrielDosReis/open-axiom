
(/VERSIONCHECK 2) 

(DEFUN |ABELMON-;zero?;SB;1| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 7) (QREFELT $ 9))) 

(DEFUN |ABELMON-;*;Pi2S;2| (|n| |x| $)
  (SPADCALL |n| |x| (QREFELT $ 12))) 

(DEFUN |ABELMON-;sample;S;3| ($) (|spadConstant| $ 7)) 

(DEFUN |ABELMON-;*;Nni2S;4| (|n| |x| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 7))
    ('T (SPADCALL |n| |x| (QREFELT $ 17))))) 

(DEFUN |AbelianMonoid&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|AbelianMonoid&|))
        (LETT |dv$| (LIST '|AbelianMonoid&| |dv$1|) . #0#)
        (LETT $ (GETREFV 19) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (COND
          ((|HasCategory| |#1| '(|Ring|)))
          ('T
           (QSETREFV $ 18
               (CONS (|dispatchFunction| |ABELMON-;*;Nni2S;4|) $))))
        $)))) 

(MAKEPROP '|AbelianMonoid&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . =) |ABELMON-;zero?;SB;1|
             (|NonNegativeInteger|) (10 . *) (|PositiveInteger|)
             |ABELMON-;*;Pi2S;2| |ABELMON-;sample;S;3|
             (|RepeatedDoubling| 6) (16 . |double|) (22 . *))
          '#(|zero?| 28 |sample| 33 * 37) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 18
                                '(0 6 0 7 2 6 8 0 0 9 2 6 0 11 0 12 2
                                  16 6 13 6 17 2 0 0 11 0 18 1 0 8 0 10
                                  0 0 0 15 2 0 0 11 0 18 2 0 0 13 0 14)))))
          '|lookupComplete|)) 
