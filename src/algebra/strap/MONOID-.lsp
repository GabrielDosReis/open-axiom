
(/VERSIONCHECK 2) 

(DEFUN |MONOID-;one?;SB;1| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 7) (|getShellEntry| $ 9))) 

(DEFUN |MONOID-;sample;S;2| ($) (|spadConstant| $ 7)) 

(DEFUN |MONOID-;recip;SU;3| (|x| $)
  (COND
    ((SPADCALL |x| (|spadConstant| $ 7) (|getShellEntry| $ 9))
     (CONS 0 |x|))
    ('T (CONS 1 "failed")))) 

(DEFUN |MONOID-;**;SNniS;4| (|x| |n| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 7))
    ('T (SPADCALL |x| |n| (|getShellEntry| $ 16))))) 

(DEFUN |Monoid&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Monoid&|))
        (LETT |dv$| (LIST '|Monoid&| |dv$1|) . #0#)
        (LETT $ (|newShell| 19) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|Monoid&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |One|)
             (|Boolean|) (4 . =) |MONOID-;one?;SB;1|
             |MONOID-;sample;S;2| (|Union| $ '"failed")
             |MONOID-;recip;SU;3| (|PositiveInteger|)
             (|RepeatedSquaring| 6) (10 . |expt|)
             (|NonNegativeInteger|) |MONOID-;**;SNniS;4|)
          '#(|sample| 16 |recip| 20 |one?| 25 ** 30) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 18
                                '(0 6 0 7 2 6 8 0 0 9 2 15 6 6 14 16 0
                                  0 0 11 1 0 12 0 13 1 0 8 0 10 2 0 0 0
                                  17 18)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Monoid&| '|isFunctor|
             '(((|recip| ((|Union| $ "failed") $)) T (ELT $ 13))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ 18))
               ((|one?| ((|Boolean|) $)) T (ELT $ 10))
               ((|sample| ($)) T (ELT $ 11))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL)))
             (|addModemap| '|Monoid&| '(|Monoid&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |recip|
                           ((|Union| |#1| "failed") |#1|))
                       (SIGNATURE **
                           (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE |one?| ((|Boolean|) |#1|))
                       (SIGNATURE |sample| (|#1|))
                       (SIGNATURE ** (|#1| |#1| (|PositiveInteger|))))
                   (|Monoid|))
                 T '|Monoid&|
                 (|put| '|Monoid&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |recip|
                                     ((|Union| |#1| "failed") |#1|))
                                 (SIGNATURE **
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE |one?| ((|Boolean|) |#1|))
                                 (SIGNATURE |sample| (|#1|))
                                 (SIGNATURE **
                                     (|#1| |#1| (|PositiveInteger|))))
                             (|Monoid|))
                        |$CategoryFrame|)))) 
