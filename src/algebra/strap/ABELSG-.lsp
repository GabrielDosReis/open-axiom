
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Thing| |%Shell|)
                    |%Thing|)
                |ABELSG-;*;Pi2S;1|)) 

(DEFUN |ABELSG-;*;Pi2S;1| (|n| |x| $)
  (SPADCALL |n| |x| (|getShellEntry| $ 9))) 

(DEFUN |AbelianSemiGroup&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|AbelianSemiGroup&| |dv$1|)) ($ (|newShell| 11))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    (COND
      ((|HasCategory| |#1| '(|Ring|)))
      (T (SETF (|shellEntry| $ 10)
               (CONS (|dispatchFunction| |ABELSG-;*;Pi2S;1|) $))))
    $)) 

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
