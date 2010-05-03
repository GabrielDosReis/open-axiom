
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ABELMON-;zero?;SB;1|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Thing| |%Shell|)
                    |%Thing|)
                |ABELMON-;*;Pi2S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |ABELMON-;sample;S;3|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Thing| |%Shell|)
                    |%Thing|)
                |ABELMON-;*;Nni2S;4|)) 

(DEFUN |ABELMON-;zero?;SB;1| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 7) (|getShellEntry| $ 9))) 

(DEFUN |ABELMON-;*;Pi2S;2| (|n| |x| $)
  (SPADCALL |n| |x| (|getShellEntry| $ 12))) 

(DEFUN |ABELMON-;sample;S;3| ($) (|spadConstant| $ 7)) 

(DEFUN |ABELMON-;*;Nni2S;4| (|n| |x| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 7))
    ('T (SPADCALL |n| |x| (|getShellEntry| $ 18))))) 

(DEFUN |AbelianMonoid&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|AbelianMonoid&| |dv$1|)) ($ (|newShell| 20))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (COND
      ((|HasCategory| |#1| '(|Ring|)))
      ('T
       (|setShellEntry| $ 19
           (CONS (|dispatchFunction| |ABELMON-;*;Nni2S;4|) $))))
    $)) 

(MAKEPROP '|AbelianMonoid&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . =) |ABELMON-;zero?;SB;1|
             (|NonNegativeInteger|) (10 . *) (|PositiveInteger|)
             |ABELMON-;*;Pi2S;2| |ABELMON-;sample;S;3| (16 . |zero?|)
             (|RepeatedDoubling| 6) (21 . |double|) (27 . *))
          '#(|zero?| 33 |sample| 38 * 42) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 19
                                '(0 6 0 7 2 6 8 0 0 9 2 6 0 11 0 12 1
                                  11 8 0 16 2 17 6 13 6 18 2 0 0 11 0
                                  19 1 0 8 0 10 0 0 0 15 2 0 0 11 0 19
                                  2 0 0 13 0 14)))))
          '|lookupComplete|)) 
