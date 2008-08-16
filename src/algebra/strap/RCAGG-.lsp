
(/VERSIONCHECK 2) 

(DEFUN |RCAGG-;elt;AvalueS;1| (|x| T0 $) (SPADCALL |x| (QREFELT $ 8))) 

(DEFUN |RCAGG-;setelt;Avalue2S;2| (|x| T1 |y| $)
  (SPADCALL |x| |y| (QREFELT $ 11))) 

(DEFUN |RCAGG-;child?;2AB;3| (|x| |l| $)
  (SPADCALL |x| (SPADCALL |l| (QREFELT $ 14)) (QREFELT $ 17))) 

(DEFUN |RecursiveAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|RecursiveAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|RecursiveAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (GETREFV 19) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasAttribute| |#1| '|shallowlyMutable|)
                            (|HasCategory| |#2| '(|SetCategory|)))) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (QSETREFV $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 1)
           (QSETREFV $ 12
               (CONS (|dispatchFunction| |RCAGG-;setelt;Avalue2S;2|) $))))
        (COND
          ((|testBitVector| |pv$| 2)
           (QSETREFV $ 18
               (CONS (|dispatchFunction| |RCAGG-;child?;2AB;3|) $))))
        $)))) 

(MAKEPROP '|RecursiveAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (0 . |value|) '"value" |RCAGG-;elt;AvalueS;1|
             (5 . |setvalue!|) (11 . |setelt|) (|List| $)
             (18 . |children|) (|Boolean|) (|List| 6) (23 . |member?|)
             (29 . |child?|))
          '#(|setelt| 35 |elt| 42 |child?| 48) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 18
                                '(1 6 7 0 8 2 6 7 0 7 11 3 0 7 0 9 7 12
                                  1 6 13 0 14 2 16 15 6 0 17 2 0 15 0 0
                                  18 3 0 7 0 9 7 12 2 0 7 0 9 10 2 0 15
                                  0 0 18)))))
          '|lookupComplete|)) 
