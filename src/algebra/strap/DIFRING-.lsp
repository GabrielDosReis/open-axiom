
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
                (LETT |i| (QSADD1 |i|)
                      |DIFRING-;differentiate;SNniS;2|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |DIFRING-;D;SNniS;3| (|r| |n| $)
  (SPADCALL |r| |n| (|getShellEntry| $ 11))) 

(DEFUN |DifferentialRing&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|DifferentialRing&|))
        (LETT |dv$| (LIST '|DifferentialRing&| |dv$1|) . #0#)
        (LETT $ (|newShell| 13) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

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

(SETQ |$CategoryFrame|
      (|put| '|DifferentialRing&| '|isFunctor|
             '(((D ($ $ (|NonNegativeInteger|))) T (ELT $ 12))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ 10))
               ((D ($ $)) T (ELT $ 8))
               ((|differentiate| ($ $)) T (ELT $ NIL)))
             (|addModemap| '|DifferentialRing&|
                 '(|DifferentialRing&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE D (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE D (|#1| |#1|))
                       (SIGNATURE |differentiate| (|#1| |#1|)))
                   (|DifferentialRing|))
                 T '|DifferentialRing&|
                 (|put| '|DifferentialRing&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE D
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE D (|#1| |#1|))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1|)))
                             (|DifferentialRing|))
                        |$CategoryFrame|)))) 
