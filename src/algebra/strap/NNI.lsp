
(/VERSIONCHECK 2) 

(|noteSubDomainInfo| '|NonNegativeInteger| '(|Integer|)
    '(|%not| (|%ilt| |#1| 0))) 

(DECLAIM (FTYPE (FUNCTION
                    ((|%IntegerSection| 0) (|%IntegerSection| 0)
                     |%Shell|)
                    (|%IntegerSection| 0))
                |NNI;sup;3$;1|)) 

(PUT '|NNI;sup;3$;1| '|SPADreplace| '|%imax|) 

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
  (LET ((|c| (- |x| |y|)))
    (COND
      ((MINUSP |c|) (CONS 1 "failed"))
      (T (CONS 0
               (|check-subtype| (NOT (MINUSP |c|))
                   '(|NonNegativeInteger|) |c|)))))) 

(DEFUN |NonNegativeInteger| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#0=#:G1409)
    (RETURN
      (COND
        ((SETQ #0# (HGET |$ConstructorCache| '|NonNegativeInteger|))
         (|CDRwithIncrement| (CDAR #0#)))
        (T (UNWIND-PROTECT
             (PROG1 (CDDAR (HPUT |$ConstructorCache|
                                 '|NonNegativeInteger|
                                 (LIST (CONS NIL
                                        (CONS 1
                                         (|NonNegativeInteger;|))))))
               (SETQ #0# T))
             (COND
               ((NOT #0#)
                (HREM |$ConstructorCache| '|NonNegativeInteger|))))))))) 

(DEFUN |NonNegativeInteger;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| (LIST '|NonNegativeInteger|)) ($ (|newShell| 22))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|NonNegativeInteger| NIL
        (CONS 1 $))
    (|stuffDomainSlots| $)
    $)) 

(MAKEPROP '|NonNegativeInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) (0 . |Zero|) (4 . |Zero|)
             (|Boolean|) (8 . >=) |NNI;sup;3$;1| |NNI;shift;$I$;2|
             (14 . -) (20 . <) (|Union| $ '"failed")
             |NNI;subtractIfCan;2$U;3|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (|PositiveInteger|) (|NonNegativeInteger|) (|String|)
             (|OutputForm|) (|SingleInteger|))
          '#(~= 26 |zero?| 32 |sup| 37 |subtractIfCan| 43 |shift| 49
             |sample| 55 |rem| 59 |recip| 65 |random| 70 |quo| 75
             |one?| 81 |min| 86 |max| 92 |latex| 98 |hash| 103 |gcd|
             108 |exquo| 114 |divide| 120 |coerce| 126 |before?| 131
             |Zero| 137 |One| 141 >= 145 > 151 = 157 <= 163 < 169 + 175
             ** 181 * 193)
          '(((|commutative| "*") . 0))
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(NIL NIL NIL NIL NIL |Monoid&| |AbelianMonoid&|
                         NIL |SemiGroup&| |AbelianSemiGroup&|
                         |SetCategory&| |OrderedType&| |BasicType&| NIL
                         NIL)
                      (CONS '#((|OrderedAbelianMonoidSup|)
                               (|OrderedCancellationAbelianMonoid|)
                               (|OrderedAbelianMonoid|)
                               (|CancellationAbelianMonoid|)
                               (|OrderedAbelianSemiGroup|) (|Monoid|)
                               (|AbelianMonoid|) (|OrderedSet|)
                               (|SemiGroup|) (|AbelianSemiGroup|)
                               (|SetCategory|) (|OrderedType|)
                               (|BasicType|) (|Type|)
                               (|CoercibleTo| 20))
                            (|makeByteWordVec2| 21
                                '(0 0 0 6 0 5 0 7 2 5 8 0 0 9 2 5 0 0 0
                                  12 2 5 8 0 0 13 2 0 8 0 0 1 1 0 8 0 1
                                  2 0 0 0 0 10 2 0 14 0 0 15 2 0 0 0 5
                                  11 0 0 0 1 2 0 0 0 0 1 1 0 14 0 1 1 0
                                  0 0 1 2 0 0 0 0 1 1 0 8 0 1 2 0 0 0 0
                                  1 2 0 0 0 0 1 1 0 19 0 1 1 0 21 0 1 2
                                  0 0 0 0 1 2 0 14 0 0 1 2 0 16 0 0 1 1
                                  0 20 0 1 2 0 8 0 0 1 0 0 0 6 0 0 0 1
                                  2 0 8 0 0 1 2 0 8 0 0 1 2 0 8 0 0 1 2
                                  0 8 0 0 1 2 0 8 0 0 1 2 0 0 0 0 1 2 0
                                  0 0 17 1 2 0 0 0 18 1 2 0 0 0 0 1 2 0
                                  0 18 0 1 2 0 0 17 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|NonNegativeInteger| 'NILADIC T) 
