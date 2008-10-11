
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
    ('T (SPADCALL |n| |x| (|getShellEntry| $ 17))))) 

(DEFUN |AbelianMonoid&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|AbelianMonoid&|))
        (LETT |dv$| (LIST '|AbelianMonoid&| |dv$1|) . #0#)
        (LETT $ (|newShell| 19) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (COND
          ((|HasCategory| |#1| '(|Ring|)))
          ('T
           (|setShellEntry| $ 18
               (CONS (|dispatchFunction| |ABELMON-;*;Nni2S;4|) $))))
        $)))) 

(MAKEPROP '|AbelianMonoid&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . =) |ABELMON-;zero?;SB;1|
             (|NonNegativeInteger|) (10 . *) (|PositiveInteger|)
             |ABELMON-;*;Pi2S;2| |ABELMON-;sample;S;3|
             (|RepeatedDoubling| 6) (16 . |double|) (22 . *))
          '#(|zero?| 28 |sample| 33 * 37) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 18
                                '(0 6 0 7 2 6 8 0 0 9 2 6 0 11 0 12 2
                                  16 6 13 6 17 2 0 0 11 0 18 1 0 8 0 10
                                  0 0 0 15 2 0 0 11 0 18 2 0 0 13 0 14)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|AbelianMonoid&| '|isFunctor|
             '(((* ($ (|NonNegativeInteger|) $)) T (ELT $ 18))
               ((|zero?| ((|Boolean|) $)) T (ELT $ 10))
               ((|sample| ($)) T (ELT $ 15))
               ((* ($ (|PositiveInteger|) $)) T (ELT $ 14)))
             (|addModemap| '|AbelianMonoid&| '(|AbelianMonoid&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE * (|#1| (|NonNegativeInteger|) |#1|))
                       (SIGNATURE |zero?| ((|Boolean|) |#1|))
                       (SIGNATURE |sample| (|#1|))
                       (SIGNATURE * (|#1| (|PositiveInteger|) |#1|)))
                   (|AbelianMonoid|))
                 T '|AbelianMonoid&|
                 (|put| '|AbelianMonoid&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE *
                                     (|#1| (|NonNegativeInteger|) |#1|))
                                 (SIGNATURE |zero?| ((|Boolean|) |#1|))
                                 (SIGNATURE |sample| (|#1|))
                                 (SIGNATURE *
                                     (|#1| (|PositiveInteger|) |#1|)))
                             (|AbelianMonoid|))
                        |$CategoryFrame|)))) 
