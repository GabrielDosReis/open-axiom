
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
  (SPADCALL (|spadConstant| $ 7) |x| (|getShellEntry| $ 9))) 

(DEFUN |ORDRING-;negative?;SB;2| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 7) (|getShellEntry| $ 9))) 

(DEFUN |ORDRING-;sign;SI;3| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 12)) 1)
    ((SPADCALL |x| (|getShellEntry| $ 13)) -1)
    ((SPADCALL |x| (|getShellEntry| $ 15)) 0)
    ('T (|error| "x satisfies neither positive?, negative? or zero?")))) 

(DEFUN |ORDRING-;abs;2S;4| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 12)) |x|)
    ((SPADCALL |x| (|getShellEntry| $ 13))
     (SPADCALL |x| (|getShellEntry| $ 18)))
    ((SPADCALL |x| (|getShellEntry| $ 15)) (|spadConstant| $ 7))
    ('T (|error| "x satisfies neither positive?, negative? or zero?")))) 

(DEFUN |OrderedRing&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|OrderedRing&|))
        (LETT |dv$| (LIST '|OrderedRing&| |dv$1|) . #0#)
        (LETT $ (|newShell| 20) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|OrderedRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . <) |ORDRING-;positive?;SB;1|
             |ORDRING-;negative?;SB;2| (10 . |positive?|)
             (15 . |negative?|) (20 . |One|) (24 . |zero?|) (|Integer|)
             |ORDRING-;sign;SI;3| (29 . -) |ORDRING-;abs;2S;4|)
          '#(|sign| 34 |positive?| 39 |negative?| 44 |abs| 49) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 19
                                '(0 6 0 7 2 6 8 0 0 9 1 6 8 0 12 1 6 8
                                  0 13 0 6 0 14 1 6 8 0 15 1 6 0 0 18 1
                                  0 16 0 17 1 0 8 0 10 1 0 8 0 11 1 0 0
                                  0 19)))))
          '|lookupComplete|)) 
