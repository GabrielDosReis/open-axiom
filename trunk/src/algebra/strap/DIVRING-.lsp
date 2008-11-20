
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |DIVRING-;**;SIS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |DIVRING-;*;F2S;2|)) 

(DEFUN |DIVRING-;**;SIS;1| (|x| |n| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 7))
    ((SPADCALL |x| (|getShellEntry| $ 9))
     (COND ((< |n| 0) (|error| "division by zero")) ('T |x|)))
    ((< |n| 0)
     (SPADCALL (SPADCALL |x| (|getShellEntry| $ 11)) (- |n|)
         (|getShellEntry| $ 14)))
    ('T (SPADCALL |x| |n| (|getShellEntry| $ 14))))) 

(DEFUN |DIVRING-;*;F2S;2| (|q| |x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |q| (|getShellEntry| $ 18))
          (SPADCALL
              (SPADCALL (SPADCALL |q| (|getShellEntry| $ 19))
                  (|getShellEntry| $ 20))
              (|getShellEntry| $ 11))
          (|getShellEntry| $ 21))
      |x| (|getShellEntry| $ 22))) 

(DEFUN |DivisionRing&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|DivisionRing&|))
        (LETT |dv$| (LIST '|DivisionRing&| |dv$1|) . #0#)
        (LETT $ (|newShell| 25) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|DivisionRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |One|)
             (|Boolean|) (4 . |zero?|) (9 . |Zero|) (13 . |inv|)
             (|PositiveInteger|) (|RepeatedSquaring| 6) (18 . |expt|)
             (|Integer|) |DIVRING-;**;SIS;1| (|Fraction| 15)
             (24 . |numer|) (29 . |denom|) (34 . |coerce|) (39 . *)
             (45 . *) |DIVRING-;*;F2S;2| (|NonNegativeInteger|))
          '#(** 51 * 57) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 23
                                '(0 6 0 7 1 6 8 0 9 0 6 0 10 1 6 0 0 11
                                  2 13 6 6 12 14 1 17 15 0 18 1 17 15 0
                                  19 1 6 0 15 20 2 6 0 15 0 21 2 6 0 0
                                  0 22 2 0 0 0 15 16 2 0 0 17 0 23)))))
          '|lookupComplete|)) 
