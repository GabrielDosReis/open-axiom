
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
        (LETT $ (|newShell| 11) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
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

(SETQ |$CategoryFrame|
      (|put| '|SetCategory&| '|isFunctor|
             '(((|latex| ((|String|) $)) T (ELT $ 10))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ 8)))
             (|addModemap| '|SetCategory&| '(|SetCategory&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |latex| ((|String|) |#1|))
                       (SIGNATURE |hash| ((|SingleInteger|) |#1|)))
                   (|SetCategory|))
                 T '|SetCategory&|
                 (|put| '|SetCategory&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |latex| ((|String|) |#1|))
                                 (SIGNATURE |hash|
                                     ((|SingleInteger|) |#1|)))
                             (|SetCategory|))
                        |$CategoryFrame|)))) 
