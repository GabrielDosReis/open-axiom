
(/VERSIONCHECK 2) 

(SETQ |$CategoryFrame|
      (|put| #0='|PositiveInteger| '|SuperDomain|
             #1='(|NonNegativeInteger|)
             (|put| #1# '|SubDomain|
                    (CONS '(|PositiveInteger| < 0 |#1|)
                          (DELASC #0#
                                  (|get| #1# '|SubDomain|
                                         |$CategoryFrame|)))
                    |$CategoryFrame|))) 

(DEFUN |PositiveInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1396)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|PositiveInteger|)
                   |PositiveInteger|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache|
                                   '|PositiveInteger|
                                   (LIST
                                    (CONS NIL
                                     (CONS 1 (|PositiveInteger;|))))))
                 (LETT #0# T |PositiveInteger|))
               (COND
                 ((NOT #0#)
                  (HREM |$ConstructorCache| '|PositiveInteger|))))))))))) 

(DEFUN |PositiveInteger;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|PositiveInteger|) . #0=(|PositiveInteger|))
        (LETT $ (|newShell| 12) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|PositiveInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|PositiveInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|NonNegativeInteger|)
             (|PositiveInteger|) (|Boolean|) (|Union| $ '"failed")
             (|SingleInteger|) (|String|) (|OutputForm|))
          '#(~= 0 |sample| 6 |recip| 10 |one?| 15 |min| 20 |max| 26
             |latex| 32 |hash| 37 |gcd| 42 |coerce| 48 ^ 53 |One| 65 >=
             69 > 75 = 81 <= 87 < 93 + 99 ** 105 * 117)
          '(((|commutative| "*") . 0))
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0))
                (CONS '#(NIL |Monoid&| |OrderedSet&| |SemiGroup&|
                         |AbelianSemiGroup&| |SetCategory&|
                         |BasicType&| NIL)
                      (CONS '#((|OrderedAbelianSemiGroup|) (|Monoid|)
                               (|OrderedSet|) (|SemiGroup|)
                               (|AbelianSemiGroup|) (|SetCategory|)
                               (|BasicType|) (|CoercibleTo| 11))
                            (|makeByteWordVec2| 11
                                '(2 0 7 0 0 1 0 0 0 1 1 0 8 0 1 1 0 7 0
                                  1 2 0 0 0 0 1 2 0 0 0 0 1 1 0 10 0 1
                                  1 0 9 0 1 2 0 0 0 0 1 1 0 11 0 1 2 0
                                  0 0 6 1 2 0 0 0 5 1 0 0 0 1 2 0 7 0 0
                                  1 2 0 7 0 0 1 2 0 7 0 0 1 2 0 7 0 0 1
                                  2 0 7 0 0 1 2 0 0 0 0 1 2 0 0 0 6 1 2
                                  0 0 0 5 1 2 0 0 0 0 1 2 0 0 6 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|PositiveInteger| 'NILADIC T) 
