
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |RING-;coerce;IS;1|)) 

(DEFUN |RING-;coerce;IS;1| (|n| $)
  (SPADCALL |n| (|spadConstant| $ 7) (|getShellEntry| $ 9))) 

(DEFUN |Ring&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Ring&|))
        (LETT |dv$| (LIST '|Ring&| |dv$1|) . #0#)
        (LETT $ (|newShell| 12) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

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

(SETQ |$CategoryFrame|
      (|put| '|Ring&| '|isFunctor|
             '(((|coerce| ($ (|Integer|))) T (ELT $ 10))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ NIL)))
             (|addModemap| '|Ring&| '(|Ring&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |coerce| (|#1| (|Integer|)))
                       (SIGNATURE |coerce| ((|OutputForm|) |#1|)))
                   (|Ring|))
                 T '|Ring&|
                 (|put| '|Ring&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |coerce|
                                     (|#1| (|Integer|)))
                                 (SIGNATURE |coerce|
                                     ((|OutputForm|) |#1|)))
                             (|Ring|))
                        |$CategoryFrame|)))) 
