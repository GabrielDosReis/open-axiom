
(/VERSIONCHECK 2) 

(|noteSubDomainInfo| '|NonNegativeInteger| '(|Integer|)
    '(COND ((< |#1| 0) 'NIL) ('T 'T))) 

(DECLAIM (FTYPE (FUNCTION
                    ((|%IntegerSection| 0) (|%IntegerSection| 0)
                     |%Shell|)
                    (|%IntegerSection| 0))
                |NNI;sup;3$;1|)) 

(PUT '|NNI;sup;3$;1| '|SPADreplace| 'MAX) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Integer| |%Shell|)
                    (|%IntegerSection| 0))
                |NNI;shift;$I$;2|)) 

(PUT '|NNI;shift;$I$;2| '|SPADreplace| 'ASH) 

(DECLAIM (FTYPE (FUNCTION
                    ((|%IntegerSection| 0) (|%IntegerSection| 0)
                     |%Shell|)
                    |%Pair|)
                |NNI;subtractIfCan;2$U;3|)) 

(DEFUN |NNI;sup;3$;1| (|x| |y| $) (DECLARE (IGNORE $)) (MAX |x| |y|)) 

(DEFUN |NNI;shift;$I$;2| (|x| |n| $)
  (DECLARE (IGNORE $))
  (ASH |x| |n|)) 

(DEFUN |NNI;subtractIfCan;2$U;3| (|x| |y| $)
  (PROG (|c|)
    (RETURN
      (SEQ (LETT |c| (- |x| |y|) |NNI;subtractIfCan;2$U;3|)
           (EXIT (COND
                   ((< |c| 0) (CONS 1 "failed"))
                   ('T
                    (CONS 0
                          (PROG1 |c|
                            (|check-subtype|
                                (COND ((< |c| 0) 'NIL) ('T 'T))
                                '(|NonNegativeInteger|) |c|)))))))))) 

(DEFUN |NonNegativeInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1410)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|NonNegativeInteger|)
                   |NonNegativeInteger|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache|
                                   '|NonNegativeInteger|
                                   (LIST
                                    (CONS NIL
                                     (CONS 1 (|NonNegativeInteger;|))))))
                 (LETT #0# T |NonNegativeInteger|))
               (COND
                 ((NOT #0#)
                  (HREM |$ConstructorCache| '|NonNegativeInteger|))))))))))) 

(DEFUN |NonNegativeInteger;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|NonNegativeInteger|)
              . #0=(|NonNegativeInteger|))
        (LETT $ (|newShell| 17) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|NonNegativeInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|NonNegativeInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) |NNI;sup;3$;1|
             |NNI;shift;$I$;2| (|Union| $ '"failed")
             |NNI;subtractIfCan;2$U;3|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (|PositiveInteger|) (|NonNegativeInteger|) (|Boolean|)
             (|String|) (|OutputForm|) (|SingleInteger|))
          '#(~= 0 |zero?| 6 |sup| 11 |subtractIfCan| 17 |shift| 23
             |sample| 29 |rem| 33 |recip| 39 |random| 44 |quo| 49
             |one?| 55 |min| 60 |max| 66 |latex| 72 |hash| 77 |gcd| 82
             |exquo| 88 |divide| 94 |coerce| 100 |Zero| 105 |One| 109
             >= 113 > 119 = 125 <= 131 < 137 + 143 ** 149 * 161)
          '(((|commutative| "*") . 0))
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(NIL NIL NIL NIL NIL |Monoid&| |AbelianMonoid&|
                         |OrderedSet&| |SemiGroup&| |AbelianSemiGroup&|
                         |SetCategory&| |BasicType&| NIL)
                      (CONS '#((|OrderedAbelianMonoidSup|)
                               (|OrderedCancellationAbelianMonoid|)
                               (|OrderedAbelianMonoid|)
                               (|CancellationAbelianMonoid|)
                               (|OrderedAbelianSemiGroup|) (|Monoid|)
                               (|AbelianMonoid|) (|OrderedSet|)
                               (|SemiGroup|) (|AbelianSemiGroup|)
                               (|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 15))
                            (|makeByteWordVec2| 16
                                '(2 0 13 0 0 1 1 0 13 0 1 2 0 0 0 0 6 2
                                  0 8 0 0 9 2 0 0 0 5 7 0 0 0 1 2 0 0 0
                                  0 1 1 0 8 0 1 1 0 0 0 1 2 0 0 0 0 1 1
                                  0 13 0 1 2 0 0 0 0 1 2 0 0 0 0 1 1 0
                                  14 0 1 1 0 16 0 1 2 0 0 0 0 1 2 0 8 0
                                  0 1 2 0 10 0 0 1 1 0 15 0 1 0 0 0 1 0
                                  0 0 1 2 0 13 0 0 1 2 0 13 0 0 1 2 0
                                  13 0 0 1 2 0 13 0 0 1 2 0 13 0 0 1 2
                                  0 0 0 0 1 2 0 0 0 11 1 2 0 0 0 12 1 2
                                  0 0 0 0 1 2 0 0 12 0 1 2 0 0 11 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|NonNegativeInteger| 'NILADIC T) 
