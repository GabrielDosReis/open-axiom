
(/VERSIONCHECK 2) 

(DEFUN |MONOID-;^;SNniS;1| (|x| |n| $)
  (SPADCALL |x| |n| (|getShellEntry| $ 8))) 

(DEFUN |MONOID-;one?;SB;2| (|x| $)
  (SPADCALL |x| (|spadConstant| $ 10) (|getShellEntry| $ 12))) 

(DEFUN |MONOID-;sample;S;3| ($) (|spadConstant| $ 10)) 

(DEFUN |MONOID-;recip;SU;4| (|x| $)
  (COND
    ((SPADCALL |x| (|spadConstant| $ 10) (|getShellEntry| $ 12))
     (CONS 0 |x|))
    ('T (CONS 1 "failed")))) 

(DEFUN |MONOID-;**;SNniS;5| (|x| |n| $)
  (COND
    ((ZEROP |n|) (|spadConstant| $ 10))
    ('T (SPADCALL |x| |n| (|getShellEntry| $ 19))))) 

(DEFUN |Monoid&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Monoid&|))
        (LETT |dv$| (LIST '|Monoid&| |dv$1|) . #0#)
        (LETT $ (|newShell| 21) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|Monoid&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (0 . **) |MONOID-;^;SNniS;1|
             (6 . |One|) (|Boolean|) (10 . =) |MONOID-;one?;SB;2|
             |MONOID-;sample;S;3| (|Union| $ '"failed")
             |MONOID-;recip;SU;4| (|PositiveInteger|)
             (|RepeatedSquaring| 6) (16 . |expt|) |MONOID-;**;SNniS;5|)
          '#(|sample| 22 |recip| 26 |one?| 31 ^ 36 ** 42) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 20
                                '(2 6 0 0 7 8 0 6 0 10 2 6 11 0 0 12 2
                                  18 6 6 17 19 0 0 0 14 1 0 15 0 16 1 0
                                  11 0 13 2 0 0 0 7 9 2 0 0 0 7 20)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Monoid&| '|isFunctor|
             '(((|recip| ((|Union| $ "failed") $)) T (ELT $ 16))
               ((^ ($ $ (|NonNegativeInteger|))) T (ELT $ 9))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ 20))
               ((|one?| ((|Boolean|) $)) T (ELT $ 13))
               ((|sample| ($)) T (ELT $ 14))
               ((^ ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL)))
             (|addModemap| '|Monoid&| '(|Monoid&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |recip|
                           ((|Union| |#1| "failed") |#1|))
                       (SIGNATURE ^ (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE **
                           (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE |one?| ((|Boolean|) |#1|))
                       (SIGNATURE |sample| (|#1|))
                       (SIGNATURE ^ (|#1| |#1| (|PositiveInteger|)))
                       (SIGNATURE ** (|#1| |#1| (|PositiveInteger|))))
                   (|Monoid|))
                 T '|Monoid&|
                 (|put| '|Monoid&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |recip|
                                     ((|Union| |#1| "failed") |#1|))
                                 (SIGNATURE ^
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE **
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE |one?| ((|Boolean|) |#1|))
                                 (SIGNATURE |sample| (|#1|))
                                 (SIGNATURE ^
                                     (|#1| |#1| (|PositiveInteger|)))
                                 (SIGNATURE **
                                     (|#1| |#1| (|PositiveInteger|))))
                             (|Monoid|))
                        |$CategoryFrame|)))) 
