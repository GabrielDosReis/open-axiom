
(/VERSIONCHECK 2) 

(|noteSubDomainInfo| '|PositiveInteger| '(|NonNegativeInteger|)
    '(> |#1| 0)) 

(DEFUN |PositiveInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1400)
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
  (LET ((|dv$| (LIST '|PositiveInteger|)) ($ (|newShell| 16))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|PositiveInteger| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    $)) 

(MAKEPROP '|PositiveInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|NonNegativeInteger|) (0 . |Zero|)
             (|Integer|) (4 . |Zero|) (|Boolean|) (8 . >)
             (|PositiveInteger|) (|Union| $ '"failed")
             (|SingleInteger|) (|String|) (|OutputForm|))
          '#(~= 14 |sample| 20 |recip| 24 |one?| 29 |min| 34 |max| 40
             |latex| 46 |hash| 51 |gcd| 56 |coerce| 62 |before?| 67
             |One| 73 >= 77 > 83 = 89 <= 95 < 101 + 107 ** 113 * 125)
          '(((|commutative| "*") . 0))
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0))
                (CONS '#(NIL |Monoid&| |OrderedSet&| |SemiGroup&|
                         |AbelianSemiGroup&| |SetCategory&|
                         |BasicType&| NIL)
                      (CONS '#((|OrderedAbelianSemiGroup|) (|Monoid|)
                               (|OrderedSet|) (|SemiGroup|)
                               (|AbelianSemiGroup|) (|SetCategory|)
                               (|BasicType|) (|CoercibleTo| 15))
                            (|makeByteWordVec2| 15
                                '(0 5 0 6 0 7 0 8 2 5 9 0 0 10 2 0 9 0
                                  0 1 0 0 0 1 1 0 12 0 1 1 0 9 0 1 2 0
                                  0 0 0 1 2 0 0 0 0 1 1 0 14 0 1 1 0 13
                                  0 1 2 0 0 0 0 1 1 0 15 0 1 2 0 9 0 0
                                  1 0 0 0 1 2 0 9 0 0 1 2 0 9 0 0 1 2 0
                                  9 0 0 1 2 0 9 0 0 1 2 0 9 0 0 1 2 0 0
                                  0 0 1 2 0 0 0 11 1 2 0 0 0 5 1 2 0 0
                                  0 0 1 2 0 0 11 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|PositiveInteger| 'NILADIC T) 
