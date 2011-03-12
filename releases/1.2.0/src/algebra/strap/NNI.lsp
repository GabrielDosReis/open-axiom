
(/VERSIONCHECK 2) 

(SETQ |$CategoryFrame|
      (|put| #0='|NonNegativeInteger| '|SuperDomain| #1='(|Integer|)
             (|put| #1# '|SubDomain|
                    (CONS '(|NonNegativeInteger| COND ((< |#1| 0) 'NIL)
                               ('T 'T))
                          (DELASC #0#
                                  (|get| #1# '|SubDomain|
                                         |$CategoryFrame|)))
                    |$CategoryFrame|))) 

(PUT '|NNI;sup;3$;1| '|SPADreplace| 'MAX) 

(DEFUN |NNI;sup;3$;1| (|x| |y| $) (MAX |x| |y|)) 

(PUT '|NNI;shift;$I$;2| '|SPADreplace| 'ASH) 

(DEFUN |NNI;shift;$I$;2| (|x| |n| $) (ASH |x| |n|)) 

(DEFUN |NNI;subtractIfCan;2$U;3| (|x| |y| $)
  (PROG (|c|)
    (RETURN
      (SEQ (LETT |c| (- |x| |y|) |NNI;subtractIfCan;2$U;3|)
           (EXIT (COND
                   ((< |c| 0) (CONS 1 "failed"))
                   ('T (CONS 0 |c|)))))))) 

(DEFUN |NonNegativeInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1406)
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
             (|PositiveInteger|) (|Boolean|) (|NonNegativeInteger|)
             (|SingleInteger|) (|String|) (|OutputForm|))
          '#(~= 0 |zero?| 6 |sup| 11 |subtractIfCan| 17 |shift| 23
             |sample| 29 |rem| 33 |recip| 39 |random| 44 |quo| 49
             |one?| 55 |min| 60 |max| 66 |latex| 72 |hash| 77 |gcd| 82
             |exquo| 88 |divide| 94 |coerce| 100 ^ 105 |Zero| 117 |One|
             121 >= 125 > 131 = 137 <= 143 < 149 + 155 ** 161 * 173)
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
                               (|CoercibleTo| 16))
                            (|makeByteWordVec2| 16
                                '(2 0 12 0 0 1 1 0 12 0 1 2 0 0 0 0 6 2
                                  0 8 0 0 9 2 0 0 0 5 7 0 0 0 1 2 0 0 0
                                  0 1 1 0 8 0 1 1 0 0 0 1 2 0 0 0 0 1 1
                                  0 12 0 1 2 0 0 0 0 1 2 0 0 0 0 1 1 0
                                  15 0 1 1 0 14 0 1 2 0 0 0 0 1 2 0 8 0
                                  0 1 2 0 10 0 0 1 1 0 16 0 1 2 0 0 0
                                  11 1 2 0 0 0 13 1 0 0 0 1 0 0 0 1 2 0
                                  12 0 0 1 2 0 12 0 0 1 2 0 12 0 0 1 2
                                  0 12 0 0 1 2 0 12 0 0 1 2 0 0 0 0 1 2
                                  0 0 0 11 1 2 0 0 0 13 1 2 0 0 0 0 1 2
                                  0 0 13 0 1 2 0 0 11 0 1)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|NonNegativeInteger| '|isFunctor|
             '(((|random| ($ $)) T (ELT $ NIL))
               ((|shift| ($ $ (|Integer|))) T (ELT $ 7))
               ((|exquo| ((|Union| $ "failed") $ $)) T (ELT $ NIL))
               ((|divide|
                    ((|Record| (|:| |quotient| $) (|:| |remainder| $))
                     $ $))
                T (ELT $ NIL))
               ((|gcd| ($ $ $)) T (ELT $ NIL))
               ((|rem| ($ $ $)) T (ELT $ NIL))
               ((|quo| ($ $ $)) T (ELT $ NIL))
               ((* ($ $ $)) T (ELT $ NIL))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((^ ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((|One| ($)) T (CONST $ NIL))
               ((|one?| ((|Boolean|) $)) T (ELT $ NIL))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((^ ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|recip| ((|Union| $ "failed") $)) T (ELT $ NIL))
               ((|sup| ($ $ $)) T (ELT $ 6))
               ((|subtractIfCan| ((|Union| $ "failed") $ $)) T
                (ELT $ 9))
               ((|Zero| ($)) T (CONST $ NIL))
               ((|sample| ($)) T (CONST $ NIL))
               ((|zero?| ((|Boolean|) $)) T (ELT $ NIL))
               ((* ($ (|NonNegativeInteger|) $)) T (ELT $ NIL))
               ((+ ($ $ $)) T (ELT $ NIL))
               ((* ($ (|PositiveInteger|) $)) T (ELT $ NIL))
               ((|min| ($ $ $)) T (ELT $ NIL))
               ((|max| ($ $ $)) T (ELT $ NIL))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|latex| ((|String|) $)) T (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ NIL))
               ((= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|NonNegativeInteger|
                 '(|NonNegativeInteger|)
                 '((|Join| (|OrderedAbelianMonoidSup|) (|Monoid|)
                           (CATEGORY |domain| (SIGNATURE |quo| ($ $ $))
                               (SIGNATURE |rem| ($ $ $))
                               (SIGNATURE |gcd| ($ $ $))
                               (SIGNATURE |divide|
                                   ((|Record| (|:| |quotient| $)
                                     (|:| |remainder| $))
                                    $ $))
                               (SIGNATURE |exquo|
                                   ((|Union| $ "failed") $ $))
                               (SIGNATURE |shift| ($ $ (|Integer|)))
                               (SIGNATURE |random| ($ $))
                               (ATTRIBUTE (|commutative| "*")))))
                 T '|NonNegativeInteger|
                 (|put| '|NonNegativeInteger| '|mode|
                        '(|Mapping|
                             (|Join| (|OrderedAbelianMonoidSup|)
                                     (|Monoid|)
                                     (CATEGORY |domain|
                                      (SIGNATURE |quo| ($ $ $))
                                      (SIGNATURE |rem| ($ $ $))
                                      (SIGNATURE |gcd| ($ $ $))
                                      (SIGNATURE |divide|
                                       ((|Record| (|:| |quotient| $)
                                         (|:| |remainder| $))
                                        $ $))
                                      (SIGNATURE |exquo|
                                       ((|Union| $ "failed") $ $))
                                      (SIGNATURE |shift|
                                       ($ $ (|Integer|)))
                                      (SIGNATURE |random| ($ $))
                                      (ATTRIBUTE (|commutative| "*")))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|NonNegativeInteger| 'NILADIC T) 