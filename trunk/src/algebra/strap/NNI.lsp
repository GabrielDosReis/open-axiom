
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
        (LETT $ (|newShell| 23) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|NonNegativeInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|NonNegativeInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) (0 . |Zero|) (4 . |Zero|)
             (|Boolean|) (8 . <) (14 . |false|) (18 . |true|)
             |NNI;sup;3$;1| |NNI;shift;$I$;2| (22 . -)
             (|Union| $ '"failed") |NNI;subtractIfCan;2$U;3|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (|PositiveInteger|) (|NonNegativeInteger|) (|String|)
             (|OutputForm|) (|SingleInteger|))
          '#(~= 28 |zero?| 34 |sup| 39 |subtractIfCan| 45 |shift| 51
             |sample| 57 |rem| 61 |recip| 67 |random| 72 |quo| 77
             |one?| 83 |min| 88 |max| 94 |latex| 100 |hash| 105 |gcd|
             110 |exquo| 116 |divide| 122 |coerce| 128 |Zero| 133 |One|
             137 >= 141 > 147 = 153 <= 159 < 165 + 171 ** 177 * 189)
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
                               (|CoercibleTo| 21))
                            (|makeByteWordVec2| 22
                                '(0 0 0 6 0 5 0 7 2 5 8 0 0 9 0 8 0 10
                                  0 8 0 11 2 5 0 0 0 14 2 0 8 0 0 1 1 0
                                  8 0 1 2 0 0 0 0 12 2 0 15 0 0 16 2 0
                                  0 0 5 13 0 0 0 1 2 0 0 0 0 1 1 0 15 0
                                  1 1 0 0 0 1 2 0 0 0 0 1 1 0 8 0 1 2 0
                                  0 0 0 1 2 0 0 0 0 1 1 0 20 0 1 1 0 22
                                  0 1 2 0 0 0 0 1 2 0 15 0 0 1 2 0 17 0
                                  0 1 1 0 21 0 1 0 0 0 6 0 0 0 1 2 0 8
                                  0 0 1 2 0 8 0 0 1 2 0 8 0 0 1 2 0 8 0
                                  0 1 2 0 8 0 0 1 2 0 0 0 0 1 2 0 0 0
                                  18 1 2 0 0 0 19 1 2 0 0 0 0 1 2 0 0
                                  19 0 1 2 0 0 18 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|NonNegativeInteger| 'NILADIC T) 
