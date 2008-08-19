
(/VERSIONCHECK 2) 

(DEFUN |UFD-;squareFreePart;2S;1| (|x| $)
  (PROG (|s| |f| #0=#:G1405 #1=#:G1403 #2=#:G1401 #3=#:G1402)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (LETT |s| (SPADCALL |x| (|getShellEntry| $ 8))
                         |UFD-;squareFreePart;2S;1|)
                   (|getShellEntry| $ 10))
               (PROGN
                 (LETT #3# NIL |UFD-;squareFreePart;2S;1|)
                 (SEQ (LETT |f| NIL |UFD-;squareFreePart;2S;1|)
                      (LETT #0# (SPADCALL |s| (|getShellEntry| $ 14))
                            |UFD-;squareFreePart;2S;1|)
                      G190
                      (COND
                        ((OR (ATOM #0#)
                             (PROGN
                               (LETT |f| (CAR #0#)
                                     |UFD-;squareFreePart;2S;1|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (PROGN
                                   (LETT #1# (QCAR |f|)
                                    |UFD-;squareFreePart;2S;1|)
                                   (COND
                                     (#3#
                                      (LETT #2#
                                       (SPADCALL #2# #1#
                                        (|getShellEntry| $ 15))
                                       |UFD-;squareFreePart;2S;1|))
                                     ('T
                                      (PROGN
                                        (LETT #2# #1#
                                         |UFD-;squareFreePart;2S;1|)
                                        (LETT #3# 'T
                                         |UFD-;squareFreePart;2S;1|)))))))
                      (LETT #0# (CDR #0#) |UFD-;squareFreePart;2S;1|)
                      (GO G190) G191 (EXIT NIL))
                 (COND (#3# #2#) ('T (|spadConstant| $ 16))))
               (|getShellEntry| $ 15)))))) 

(DEFUN |UFD-;prime?;SB;2| (|x| $)
  (EQL (LENGTH (SPADCALL (SPADCALL |x| (|getShellEntry| $ 18))
                   (|getShellEntry| $ 22)))
       1)) 

(DEFUN |UniqueFactorizationDomain&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|)
              . #0=(|UniqueFactorizationDomain&|))
        (LETT |dv$| (LIST '|UniqueFactorizationDomain&| |dv$1|) . #0#)
        (LETT $ (|newShell| 25) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|UniqueFactorizationDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Factored| $)
             (0 . |squareFree|) (|Factored| 6) (5 . |unit|) (|Integer|)
             (|Record| (|:| |factor| 6) (|:| |exponent| 11))
             (|List| 12) (10 . |factors|) (15 . *) (21 . |One|)
             |UFD-;squareFreePart;2S;1| (25 . |factor|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime")
             (|Record| (|:| |flg| 19) (|:| |fctr| 6) (|:| |xpnt| 11))
             (|List| 20) (30 . |factorList|) (|Boolean|)
             |UFD-;prime?;SB;2|)
          '#(|squareFreePart| 35 |prime?| 40) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 24
                                '(1 6 7 0 8 1 9 6 0 10 1 9 13 0 14 2 6
                                  0 0 0 15 0 6 0 16 1 6 7 0 18 1 9 21 0
                                  22 1 0 0 0 17 1 0 23 0 24)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|UniqueFactorizationDomain&| '|isFunctor|
             '(((|squareFreePart| ($ $)) T (ELT $ 17))
               ((|prime?| ((|Boolean|) $)) T (ELT $ 24)))
             (|addModemap| '|UniqueFactorizationDomain&|
                 '(|UniqueFactorizationDomain&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |squareFreePart| (|#1| |#1|))
                       (SIGNATURE |prime?| ((|Boolean|) |#1|)))
                   (|UniqueFactorizationDomain|))
                 T '|UniqueFactorizationDomain&|
                 (|put| '|UniqueFactorizationDomain&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |squareFreePart|
                                     (|#1| |#1|))
                                 (SIGNATURE |prime?|
                                     ((|Boolean|) |#1|)))
                             (|UniqueFactorizationDomain|))
                        |$CategoryFrame|)))) 
