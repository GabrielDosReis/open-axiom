
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UFD-;squareFreePart;2S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |UFD-;prime?;SB;2|)) 

(DEFUN |UFD-;squareFreePart;2S;1| (|x| $)
  (PROG (|s|)
    (RETURN
      (SPADCALL
          (SPADCALL
              (LETT |s| (SPADCALL |x| (|getShellEntry| $ 8))
                    |UFD-;squareFreePart;2S;1|)
              (|getShellEntry| $ 10))
          (LET ((#0=#:G1380 NIL) (#1=#:G1381 T)
                (#2=#:G1394 (SPADCALL |s| (|getShellEntry| $ 14))))
            (LOOP
              (COND
                ((ATOM #2#)
                 (RETURN (COND (#1# (|spadConstant| $ 16)) (T #0#))))
                (T (LET ((|f| (CAR #2#)))
                     (LET ((#3=#:G1379 (CAR |f|)))
                       (COND
                         (#1# (SETQ #0# #3#))
                         (T (SETQ #0#
                                  (SPADCALL #0# #3#
                                      (|getShellEntry| $ 15)))))
                       (SETQ #1# NIL)))))
              (SETQ #2# (CDR #2#))))
          (|getShellEntry| $ 15))))) 

(DEFUN |UFD-;prime?;SB;2| (|x| $)
  (EQL (LENGTH (SPADCALL (SPADCALL |x| (|getShellEntry| $ 18))
                   (|getShellEntry| $ 22)))
       1)) 

(DEFUN |UniqueFactorizationDomain&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|UniqueFactorizationDomain&| |dv$1|))
         ($ (|newShell| 29)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

(MAKEPROP '|UniqueFactorizationDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Factored| $)
             (0 . |squareFree|) (|Factored| 6) (5 . |unit|) (|Integer|)
             (|Record| (|:| |factor| 6) (|:| |exponent| 11))
             (|List| 12) (10 . |factors|) (15 . *) (21 . |One|)
             |UFD-;squareFreePart;2S;1| (25 . |factor|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime")
             (|Record| (|:| |flg| 19) (|:| |fctr| 6) (|:| |xpnt| 11))
             (|List| 20) (30 . |factorList|) (|NonNegativeInteger|)
             (35 . |#|) (40 . |One|) (|Boolean|) (44 . =)
             |UFD-;prime?;SB;2|)
          '#(|squareFreePart| 50 |prime?| 55) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 28
                                '(1 6 7 0 8 1 9 6 0 10 1 9 13 0 14 2 6
                                  0 0 0 15 0 6 0 16 1 6 7 0 18 1 9 21 0
                                  22 1 21 23 0 24 0 23 0 25 2 23 26 0 0
                                  27 1 0 0 0 17 1 0 26 0 28)))))
          '|lookupComplete|)) 
