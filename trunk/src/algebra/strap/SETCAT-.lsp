
(/VERSIONCHECK 2) 

(PUT '|SETCAT-;hash;SSi;1| '|SPADreplace| '(XLAM (|s|) 0)) 

(DEFUN |SETCAT-;hash;SSi;1| (|s| $) 0) 

(PUT '|SETCAT-;latex;SS;2| '|SPADreplace|
     '(XLAM (|s|) "\\mbox{\\bf Unimplemented}")) 

(DEFUN |SETCAT-;latex;SS;2| (|s| $) "\\mbox{\\bf Unimplemented}") 

(DEFUN |SetCategory&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|SetCategory&|))
        (LETT |dv$| (LIST '|SetCategory&| |dv$1|) . #0#)
        (LETT $ (GETREFV 11) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|SetCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|SingleInteger|)
             |SETCAT-;hash;SSi;1| (|String|) |SETCAT-;latex;SS;2|)
          '#(|latex| 0 |hash| 5) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 10
                                '(1 0 9 0 10 1 0 7 0 8)))))
          '|lookupComplete|)) 
