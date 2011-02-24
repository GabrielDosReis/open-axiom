
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |RING-;coerce;IS;1|)) 

(DEFUN |RING-;coerce;IS;1| (|n| $)
  (SPADCALL |n| (|spadConstant| $ 7) (|getShellEntry| $ 9))) 

(DEFUN |Ring&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$| (LIST '|Ring&| |dv$1|))
         ($ (|newShell| 12)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    $)) 

(MAKEPROP '|Ring&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |One|)
             (|Integer|) (4 . *) |RING-;coerce;IS;1| (|OutputForm|))
          '#(|coerce| 10) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 10
                                '(0 6 0 7 2 6 0 8 0 9 1 0 0 8 10)))))
          '|lookupComplete|)) 
