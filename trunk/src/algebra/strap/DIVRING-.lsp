
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |DIVRING-;**;SIS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |DIVRING-;*;F2S;2|)) 

(DEFUN |DIVRING-;**;SIS;1| (|x| |n| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 10))
    ((SPADCALL |x| (|shellEntry| $ 11))
     (COND ((MINUSP |n|) (|error| "division by zero")) (T |x|)))
    ((MINUSP |n|)
     (SPADCALL (SPADCALL |x| (|shellEntry| $ 15)) (- |n|)
         (|shellEntry| $ 19)))
    (T (SPADCALL |x| |n| (|shellEntry| $ 19))))) 

(DEFUN |DIVRING-;*;F2S;2| (|q| |x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |q| (|shellEntry| $ 22))
          (SPADCALL
              (SPADCALL (SPADCALL |q| (|shellEntry| $ 23))
                  (|shellEntry| $ 24))
              (|shellEntry| $ 15))
          (|shellEntry| $ 25))
      |x| (|shellEntry| $ 26))) 

(DEFUN |DivisionRing&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|DivisionRing&| |dv$1|)) ($ (|newShell| 29))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    $)) 

(MAKEPROP '|DivisionRing&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Boolean|)
             (|Integer|) (0 . |zero?|) (5 . |One|) (9 . |zero?|)
             (14 . |Zero|) (18 . |Zero|) (22 . <) (28 . |inv|) (33 . -)
             (|PositiveInteger|) (|RepeatedSquaring| 6) (38 . |expt|)
             |DIVRING-;**;SIS;1| (|Fraction| 8) (44 . |numer|)
             (49 . |denom|) (54 . |coerce|) (59 . *) (65 . *)
             |DIVRING-;*;F2S;2| (|NonNegativeInteger|))
          '#(** 71 * 77) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 27
                                '(1 8 7 0 9 0 6 0 10 1 6 7 0 11 0 6 0
                                  12 0 8 0 13 2 8 7 0 0 14 1 6 0 0 15 1
                                  8 0 0 16 2 18 6 6 17 19 1 21 8 0 22 1
                                  21 8 0 23 1 6 0 8 24 2 6 0 8 0 25 2 6
                                  0 0 0 26 2 0 0 0 8 20 2 0 0 21 0 27)))))
          '|lookupComplete|)) 
