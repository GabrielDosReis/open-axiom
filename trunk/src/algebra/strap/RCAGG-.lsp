
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |RCAGG-;elt;AvalueS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |RCAGG-;setelt;Avalue2S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |RCAGG-;child?;2AB;3|)) 

(DEFUN |RCAGG-;elt;AvalueS;1| (|x| T0 $)
  (SPADCALL |x| (|getShellEntry| $ 8))) 

(DEFUN |RCAGG-;setelt;Avalue2S;2| (|x| T1 |y| $)
  (SPADCALL |x| |y| (|getShellEntry| $ 11))) 

(DEFUN |RCAGG-;child?;2AB;3| (|x| |l| $)
  (SPADCALL |x| (SPADCALL |l| (|getShellEntry| $ 14))
      (|getShellEntry| $ 17))) 

(DEFUN |RecursiveAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|RecursiveAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 19))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasAttribute| |#1| '|shallowlyMutable|)
                          (|HasCategory| |#2| '(|SetCategory|))))))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    (SETF (|shellEntry| $ 7) |#2|)
    (COND
      ((|testBitVector| |pv$| 1)
       (SETF (|shellEntry| $ 12)
             (CONS (|dispatchFunction| |RCAGG-;setelt;Avalue2S;2|) $))))
    (COND
      ((|testBitVector| |pv$| 2)
       (SETF (|shellEntry| $ 18)
             (CONS (|dispatchFunction| |RCAGG-;child?;2AB;3|) $))))
    $)) 

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
