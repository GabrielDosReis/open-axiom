
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ORDRING-;positive?;SB;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ORDRING-;negative?;SB;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |ORDRING-;sign;SI;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ORDRING-;abs;2S;4|)) 

(DEFUN |ORDRING-;positive?;SB;1| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 7) (|getShellEntry| $ 9))) 

(DEFUN |ORDRING-;negative?;SB;2| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 7) (|getShellEntry| $ 11))) 

(DEFUN |ORDRING-;sign;SI;3| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 13)) 1)
    ((SPADCALL |x| (|getShellEntry| $ 16)) -1)
    ((SPADCALL |x| (|getShellEntry| $ 19)) 0)
    ('T (|error| "x satisfies neither positive?, negative? or zero?")))) 

(DEFUN |ORDRING-;abs;2S;4| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 13)) |x|)
    ((SPADCALL |x| (|getShellEntry| $ 16))
     (SPADCALL |x| (|getShellEntry| $ 22)))
    ((SPADCALL |x| (|getShellEntry| $ 19)) (|spadConstant| $ 7))
    ('T (|error| "x satisfies neither positive?, negative? or zero?")))) 

(DEFUN |OrderedRing&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|OrderedRing&| |dv$1|)) ($ (|newShell| 24))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

(MAKEPROP '|OrderedRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . >) |ORDRING-;positive?;SB;1| (10 . <)
             |ORDRING-;negative?;SB;2| (16 . |positive?|) (|Integer|)
             (21 . |One|) (25 . |negative?|) (30 . |One|) (34 . -)
             (39 . |zero?|) (44 . |Zero|) |ORDRING-;sign;SI;3| (48 . -)
             |ORDRING-;abs;2S;4|)
          '#(|sign| 53 |positive?| 58 |negative?| 63 |abs| 68) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 23
                                '(0 6 0 7 2 6 8 0 0 9 2 6 8 0 0 11 1 6
                                  8 0 13 0 14 0 15 1 6 8 0 16 0 6 0 17
                                  1 14 0 0 18 1 6 8 0 19 0 14 0 20 1 6
                                  0 0 22 1 0 14 0 21 1 0 8 0 10 1 0 8 0
                                  12 1 0 0 0 23)))))
          '|lookupComplete|)) 
