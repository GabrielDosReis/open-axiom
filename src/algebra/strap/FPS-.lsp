
(/VERSIONCHECK 2) 

(DEFUN |FPS-;float;2IS;1| (|ma| |ex| $)
  (SPADCALL |ma| |ex| (SPADCALL (QREFELT $ 8)) (QREFELT $ 10))) 

(DEFUN |FPS-;digits;Pi;2| ($)
  (PROG (#0=#:G1389)
    (RETURN
      (PROG1 (LETT #0#
                   (MAX 1
                        (QUOTIENT2
                            (SPADCALL 4004
                                (- (SPADCALL (QREFELT $ 13)) 1)
                                (QREFELT $ 14))
                            13301))
                   |FPS-;digits;Pi;2|)
        (|check-subtype| (> #0# 0) '(|PositiveInteger|) #0#))))) 

(DEFUN |FloatingPointSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|FloatingPointSystem&|))
        (LETT |dv$| (LIST '|FloatingPointSystem&| |dv$1|) . #0#)
        (LETT $ (GETREFV 17) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasAttribute| |#1| '|arbitraryExponent|)
                            (|HasAttribute| |#1| '|arbitraryPrecision|))) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|FloatingPointSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|PositiveInteger|)
             (0 . |base|) (|Integer|) (4 . |float|) |FPS-;float;2IS;1|
             (11 . |One|) (15 . |bits|) (19 . *) (25 . |max|)
             |FPS-;digits;Pi;2|)
          '#(|float| 29 |digits| 35) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 16
                                '(0 6 7 8 3 6 0 9 9 7 10 0 6 0 12 0 6 7
                                  13 2 9 0 7 0 14 0 6 0 15 2 0 0 9 9 11
                                  0 0 7 16)))))
          '|lookupComplete|)) 
