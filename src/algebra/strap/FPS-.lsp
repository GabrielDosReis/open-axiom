
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Thing|)
                |FPS-;float;2IS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 1))
                |FPS-;digits;Pi;2|)) 

(DEFUN |FPS-;float;2IS;1| (|ma| |ex| $)
  (SPADCALL |ma| |ex| (SPADCALL (|getShellEntry| $ 8))
      (|getShellEntry| $ 10))) 

(DEFUN |FPS-;digits;Pi;2| ($)
  (LET ((#0=#:G1401
            (MAX 1
                 (QUOTIENT2
                     (SPADCALL 4004
                         (- (SPADCALL (|getShellEntry| $ 14)) 1)
                         (|getShellEntry| $ 16))
                     13301))))
    (|check-subtype| (AND (>= #0# 0) (> #0# 0)) '(|PositiveInteger|)
        #0#))) 

(DEFUN |FloatingPointSystem&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|FloatingPointSystem&|))
        (LETT |dv$| (LIST '|FloatingPointSystem&| |dv$1|) . #0#)
        (LETT $ (|newShell| 20) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasAttribute| |#1| '|arbitraryExponent|)
                            (|HasAttribute| |#1| '|arbitraryPrecision|))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|FloatingPointSystem&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|PositiveInteger|)
             (0 . |base|) (|Integer|) (4 . |float|) |FPS-;float;2IS;1|
             (11 . |One|) (15 . |One|) (19 . |bits|) (23 . -) (29 . *)
             (35 . |quo|) (41 . |max|) |FPS-;digits;Pi;2|)
          '#(|float| 47 |digits| 53) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 19
                                '(0 6 7 8 3 6 0 9 9 7 10 0 6 0 12 0 7 0
                                  13 0 6 7 14 2 9 0 0 0 15 2 9 0 7 0 16
                                  2 9 0 0 0 17 2 9 0 0 0 18 2 0 0 9 9
                                  11 0 0 7 19)))))
          '|lookupComplete|)) 
