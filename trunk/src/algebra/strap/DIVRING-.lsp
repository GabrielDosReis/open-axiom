
(/VERSIONCHECK 2) 

(DEFUN |DIVRING-;^;SIS;1| (|x| |n| $)
  (SPADCALL |x| |n| (QREFELT $ 8))) 

(DEFUN |DIVRING-;**;SIS;2| (|x| |n| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 10))
    ((SPADCALL |x| (QREFELT $ 12))
     (COND ((< |n| 0) (|error| "division by zero")) ('T |x|)))
    ((< |n| 0)
     (SPADCALL (SPADCALL |x| (QREFELT $ 14)) (- |n|) (QREFELT $ 17)))
    ('T (SPADCALL |x| |n| (QREFELT $ 17))))) 

(DEFUN |DIVRING-;*;F2S;3| (|q| |x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |q| (QREFELT $ 20))
          (SPADCALL
              (SPADCALL (SPADCALL |q| (QREFELT $ 21)) (QREFELT $ 22))
              (QREFELT $ 14))
          (QREFELT $ 23))
      |x| (QREFELT $ 24))) 

(DEFUN |DivisionRing&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|DivisionRing&|))
        (LETT |dv$| (LIST '|DivisionRing&| |dv$1|) . #0#)
        (LETT $ (GETREFV 27) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|DivisionRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Integer|)
             (0 . **) |DIVRING-;^;SIS;1| (6 . |One|) (|Boolean|)
             (10 . |zero?|) (15 . |Zero|) (19 . |inv|)
             (|PositiveInteger|) (|RepeatedSquaring| 6) (24 . |expt|)
             |DIVRING-;**;SIS;2| (|Fraction| 7) (30 . |numer|)
             (35 . |denom|) (40 . |coerce|) (45 . *) (51 . *)
             |DIVRING-;*;F2S;3| (|NonNegativeInteger|))
          '#(^ 57 ** 63 * 69) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 25
                                '(2 6 0 0 7 8 0 6 0 10 1 6 11 0 12 0 6
                                  0 13 1 6 0 0 14 2 16 6 6 15 17 1 19 7
                                  0 20 1 19 7 0 21 1 6 0 7 22 2 6 0 7 0
                                  23 2 6 0 0 0 24 2 0 0 0 7 9 2 0 0 0 7
                                  18 2 0 0 19 0 25)))))
          '|lookupComplete|)) 
