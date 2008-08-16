
(/VERSIONCHECK 2) 

(DEFUN |MONOID-;^;SNniS;1| (|x| |n| $)
  (SPADCALL |x| |n| (QREFELT $ 8))) 

(DEFUN |MONOID-;one?;SB;2| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 10) (QREFELT $ 12))) 

(DEFUN |MONOID-;sample;S;3| ($) (|spadConstant| $ 10)) 

(DEFUN |MONOID-;recip;SU;4| (|x| $)
  (COND
    ((SPADCALL |x| (|spadConstant| $ 10) (QREFELT $ 12)) (CONS 0 |x|))
    ('T (CONS 1 "failed")))) 

(DEFUN |MONOID-;**;SNniS;5| (|x| |n| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 10))
    ('T (SPADCALL |x| |n| (QREFELT $ 19))))) 

(DEFUN |Monoid&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Monoid&|))
        (LETT |dv$| (LIST '|Monoid&| |dv$1|) . #0#)
        (LETT $ (GETREFV 21) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|Monoid&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (0 . **) |MONOID-;^;SNniS;1|
             (6 . |One|) (|Boolean|) (10 . =) |MONOID-;one?;SB;2|
             |MONOID-;sample;S;3| (|Union| $ '"failed")
             |MONOID-;recip;SU;4| (|PositiveInteger|)
             (|RepeatedSquaring| 6) (16 . |expt|) |MONOID-;**;SNniS;5|)
          '#(|sample| 22 |recip| 26 |one?| 31 ^ 36 ** 42) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 20
                                '(2 6 0 0 7 8 0 6 0 10 2 6 11 0 0 12 2
                                  18 6 6 17 19 0 0 0 14 1 0 15 0 16 1 0
                                  11 0 13 2 0 0 0 7 9 2 0 0 0 7 20)))))
          '|lookupComplete|)) 
