
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |DIFRING-;D;2S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |DIFRING-;differentiate;SNniS;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |DIFRING-;D;SNniS;3|)) 

(DEFUN |DIFRING-;D;2S;1| (|r| $) (SPADCALL |r| (|getShellEntry| $ 7))) 

(DEFUN |DIFRING-;differentiate;SNniS;2| (|r| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (SEQ (LETT |i| 1 |DIFRING-;differentiate;SNniS;2|) G190
                (COND ((QSGREATERP |i| |n|) (GO G191)))
                (SEQ (EXIT (LETT |r|
                                 (SPADCALL |r| (|getShellEntry| $ 7))
                                 |DIFRING-;differentiate;SNniS;2|)))
                (SETQ |i| (QSADD1 |i|)) (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |DIFRING-;D;SNniS;3| (|r| |n| $)
  (SPADCALL |r| |n| (|getShellEntry| $ 11))) 

(DEFUN |DifferentialRing&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|DifferentialRing&| |dv$1|)) ($ (|newShell| 13))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

(MAKEPROP '|DifferentialRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (0 . |differentiate|) |DIFRING-;D;2S;1|
             (|NonNegativeInteger|) |DIFRING-;differentiate;SNniS;2|
             (5 . |differentiate|) |DIFRING-;D;SNniS;3|)
          '#(|differentiate| 11 D 17) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 12
                                '(1 6 0 0 7 2 6 0 0 9 11 2 0 0 0 9 10 2
                                  0 0 0 9 12 1 0 0 0 8)))))
          '|lookupComplete|)) 
