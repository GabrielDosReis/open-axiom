
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Thing| |%Shell|)
                    |%Thing|)
                |ABELSG-;*;Pi2S;1|)) 

(DEFUN |ABELSG-;*;Pi2S;1| (|n| |x| $)
  (SPADCALL |n| |x| (|getShellEntry| $ 9))) 

(DEFUN |AbelianSemiGroup&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|AbelianSemiGroup&|))
        (LETT |dv$| (LIST '|AbelianSemiGroup&| |dv$1|) . #0#)
        (LETT $ (|newShell| 11) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (COND
          ((|HasCategory| |#1| '(|Ring|)))
          ('T
           (|setShellEntry| $ 10
               (CONS (|dispatchFunction| |ABELSG-;*;Pi2S;1|) $))))
        $)))) 

(MAKEPROP '|AbelianSemiGroup&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|PositiveInteger|)
             (|RepeatedDoubling| 6) (0 . |double|) (6 . *))
          '#(* 12) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 10
                                '(2 8 6 7 6 9 2 0 0 7 0 10 2 0 0 7 0
                                  10)))))
          '|lookupComplete|)) 
