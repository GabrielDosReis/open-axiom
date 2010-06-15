
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |DIFRING-;differentiate;SNniS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |DIFRING-;D;SNniS;2|)) 

(DEFUN |DIFRING-;differentiate;SNniS;1| (|r| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (SEQ (LETT |i| 1 |DIFRING-;differentiate;SNniS;1|) G190
                (COND ((QSGREATERP |i| |n|) (GO G191)))
                (SEQ (EXIT (LETT |r|
                                 (SPADCALL |r| (|getShellEntry| $ 7))
                                 |DIFRING-;differentiate;SNniS;1|)))
                (SETQ |i| (QSADD1 |i|)) (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |DIFRING-;D;SNniS;2| (|r| |n| $)
  (SPADCALL |r| |n| (|getShellEntry| $ 10))) 

(DEFUN |DifferentialRing&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|DifferentialRing&| |dv$1|)) ($ (|newShell| 12))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

(MAKEPROP '|DifferentialRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (0 . |differentiate|) (|NonNegativeInteger|)
             |DIFRING-;differentiate;SNniS;1| (5 . |differentiate|)
             |DIFRING-;D;SNniS;2|)
          '#(|differentiate| 11 D 17) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 11
                                '(1 6 0 0 7 2 6 0 0 8 10 2 0 0 0 8 9 2
                                  0 0 0 8 11)))))
          '|lookupComplete|)) 
