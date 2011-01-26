
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |SETAGG-;symmetricDifference;3A;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |SETAGG-;union;ASA;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |SETAGG-;union;S2A;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |SETAGG-;difference;ASA;4|)) 

(DEFUN |SETAGG-;symmetricDifference;3A;1| (|x| |y| $)
  (SPADCALL (SPADCALL |x| |y| (|getShellEntry| $ 8))
      (SPADCALL |y| |x| (|getShellEntry| $ 8)) (|getShellEntry| $ 9))) 

(DEFUN |SETAGG-;union;ASA;2| (|s| |x| $)
  (SPADCALL |s| (SPADCALL (LIST |x|) (|getShellEntry| $ 12))
      (|getShellEntry| $ 9))) 

(DEFUN |SETAGG-;union;S2A;3| (|x| |s| $)
  (SPADCALL |s| (SPADCALL (LIST |x|) (|getShellEntry| $ 12))
      (|getShellEntry| $ 9))) 

(DEFUN |SETAGG-;difference;ASA;4| (|s| |x| $)
  (SPADCALL |s| (SPADCALL (LIST |x|) (|getShellEntry| $ 12))
      (|getShellEntry| $ 8))) 

(DEFUN |SetAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|SetAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 16)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    $)) 

(MAKEPROP '|SetAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (0 . |difference|) (6 . |union|)
             |SETAGG-;symmetricDifference;3A;1| (|List| 7)
             (12 . |brace|) |SETAGG-;union;ASA;2| |SETAGG-;union;S2A;3|
             |SETAGG-;difference;ASA;4|)
          '#(|union| 17 |symmetricDifference| 29 |difference| 35) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 15
                                '(2 6 0 0 0 8 2 6 0 0 0 9 1 6 0 11 12 2
                                  0 0 7 0 14 2 0 0 0 7 13 2 0 0 0 0 10
                                  2 0 0 0 7 15)))))
          '|lookupComplete|)) 
