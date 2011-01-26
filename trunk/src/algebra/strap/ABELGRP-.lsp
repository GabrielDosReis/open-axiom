
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ABELGRP-;-;3S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |ABELGRP-;subtractIfCan;2SU;2|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Thing| |%Shell|)
                    |%Thing|)
                |ABELGRP-;*;Nni2S;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Thing| |%Shell|) |%Thing|)
                |ABELGRP-;*;I2S;4|)) 

(DEFUN |ABELGRP-;-;3S;1| (|x| |y| $)
  (SPADCALL |x| (SPADCALL |y| (|getShellEntry| $ 7))
      (|getShellEntry| $ 8))) 

(DEFUN |ABELGRP-;subtractIfCan;2SU;2| (|x| |y| $)
  (CONS 0 (SPADCALL |x| |y| (|getShellEntry| $ 10)))) 

(DEFUN |ABELGRP-;*;Nni2S;3| (|n| |x| $)
  (SPADCALL |n| |x| (|getShellEntry| $ 14))) 

(DEFUN |ABELGRP-;*;I2S;4| (|n| |x| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 19))
    ((PLUSP |n|) (SPADCALL |n| |x| (|getShellEntry| $ 24)))
    (T (SPADCALL (- |n|) (SPADCALL |x| (|getShellEntry| $ 7))
           (|getShellEntry| $ 24))))) 

(DEFUN |AbelianGroup&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|AbelianGroup&| |dv$1|)) ($ (|newShell| 27))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (COND
      ((|HasCategory| |#1| '(|Ring|)))
      (T (|setShellEntry| $ 26
             (CONS (|dispatchFunction| |ABELGRP-;*;I2S;4|) $))))
    $)) 

(MAKEPROP '|AbelianGroup&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . -) (5 . +)
             |ABELGRP-;-;3S;1| (11 . -) (|Union| $ '"failed")
             |ABELGRP-;subtractIfCan;2SU;2| (|Integer|) (17 . *)
             (|NonNegativeInteger|) |ABELGRP-;*;Nni2S;3| (|Boolean|)
             (23 . |zero?|) (28 . |Zero|) (32 . |Zero|) (36 . >)
             (|PositiveInteger|) (|RepeatedDoubling| 6) (42 . |double|)
             (48 . -) (53 . *))
          '#(|subtractIfCan| 59 - 65 * 71) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 26
                                '(1 6 0 0 7 2 6 0 0 0 8 2 6 0 0 0 10 2
                                  6 0 13 0 14 1 13 17 0 18 0 6 0 19 0
                                  13 0 20 2 13 17 0 0 21 2 23 6 22 6 24
                                  1 13 0 0 25 2 0 0 13 0 26 2 0 11 0 0
                                  12 2 0 0 0 0 9 2 0 0 13 0 26 2 0 0 15
                                  0 16)))))
          '|lookupComplete|)) 
