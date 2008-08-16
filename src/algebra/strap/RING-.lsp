
(/VERSIONCHECK 2) 

(DEFUN |RING-;coerce;IS;1| (|n| $)
  (SPADCALL |n| (|spadConstant| $ 7) (QREFELT $ 9))) 

(DEFUN |Ring&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Ring&|))
        (LETT |dv$| (LIST '|Ring&| |dv$1|) . #0#)
        (LETT $ (GETREFV 12) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|Ring&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |One|)
             (|Integer|) (4 . *) |RING-;coerce;IS;1| (|OutputForm|))
          '#(|coerce| 10) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 10
                                '(0 6 0 7 2 6 0 8 0 9 1 0 0 8 10)))))
          '|lookupComplete|)) 
