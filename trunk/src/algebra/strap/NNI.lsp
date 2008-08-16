
(|/VERSIONCHECK| 2) 

(SETQ |$CategoryFrame| 
  (|put| 
    #1=(QUOTE |NonNegativeInteger|) 
   (QUOTE |SuperDomain|) 
   #2=(QUOTE (|Integer|)) 
  (|put| 
    #2# 
    #3=(QUOTE |SubDomain|) 
    (CONS 
      (QUOTE 
        (|NonNegativeInteger| 
          COND ((|<| |#1| 0) (QUOTE NIL)) ((QUOTE T) (QUOTE T))))
      (DELASC #1# (|get| #2# #3# |$CategoryFrame|)))
   |$CategoryFrame|))) 

(PUT 
  (QUOTE |NNI;sup;3$;1|) 
  (QUOTE |SPADreplace|) 
  (QUOTE MAX)) 

(DEFUN |NNI;sup;3$;1| (|x| |y| |$|) (MAX |x| |y|)) 

(PUT 
  (QUOTE |NNI;shift;$I$;2|) 
  (QUOTE |SPADreplace|) 
  (QUOTE ASH)) 

(DEFUN |NNI;shift;$I$;2| (|x| |n| |$|) (ASH |x| |n|)) 

(DEFUN |NNI;subtractIfCan;2$U;3| (|x| |y| |$|) 
  (PROG (|c|) 
    (RETURN 
      (SEQ 
        (LETT |c| (|-| |x| |y|) |NNI;subtractIfCan;2$U;3|)
        (EXIT 
          (COND 
            ((|<| |c| 0) (CONS 1 "failed"))
            ((QUOTE T) (CONS 0 |c|)))))))) 

(DEFUN |NonNegativeInteger| NIL 
  (PROG NIL 
    (RETURN 
      (PROG (#1=#:G96708) 
        (RETURN 
          (COND 
            ((LETT #1# 
                (HGET |$ConstructorCache| (QUOTE |NonNegativeInteger|))
                |NonNegativeInteger|)
              (|CDRwithIncrement| (CDAR #1#)))
            ((QUOTE T) 
              (|UNWIND-PROTECT| 
                (PROG1 
                  (CDDAR 
                    (HPUT 
                       |$ConstructorCache| 
                       (QUOTE |NonNegativeInteger|) 
                       (LIST (CONS NIL (CONS 1 (|NonNegativeInteger;|))))))
                  (LETT #1# T |NonNegativeInteger|))
                (COND 
                  ((NOT #1#) 
                    (HREM 
                      |$ConstructorCache| 
                      (QUOTE |NonNegativeInteger|)))))))))))) 

(DEFUN |NonNegativeInteger;| NIL 
  (PROG (|dv$| |$| |pv$|) 
    (RETURN 
      (PROGN 
        (LETT |dv$| (QUOTE (|NonNegativeInteger|)) . #1=(|NonNegativeInteger|))
        (LETT |$| (GETREFV 17) . #1#)
        (QSETREFV |$| 0 |dv$|)
        (QSETREFV |$| 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #1#))
        (|haddProp| 
           |$ConstructorCache| 
           (QUOTE |NonNegativeInteger|) 
           NIL 
           (CONS 1 |$|))
        (|stuffDomainSlots| |$|) |$|)))) 

(MAKEPROP 
  (QUOTE |NonNegativeInteger|)
  (QUOTE |infovec|)
  (LIST 
    (QUOTE 
      #(NIL NIL NIL NIL NIL 
        (|Integer|) 
        |NNI;sup;3$;1| 
        |NNI;shift;$I$;2| 
        (|Union| |$| (QUOTE "failed"))
        |NNI;subtractIfCan;2$U;3| 
        (|Record| (|:| |quotient| |$|) (|:| |remainder| |$|))
        (|PositiveInteger|)
        (|Boolean|)
        (|NonNegativeInteger|)
        (|SingleInteger|)
        (|String|)
        (|OutputForm|)))
    (QUOTE 
      #(|~=| 0 |zero?| 6 |sup| 11 |subtractIfCan| 17 |shift| 23 |sample| 29 
        |rem| 33 |recip| 39 |random| 44 |quo| 49 |one?| 55 |min| 60 |max| 66 
        |latex| 72 |hash| 77 |gcd| 82 |exquo| 88 |divide| 94 |coerce| 100 
        |^| 105 |Zero| 117 |One| 121 |>=| 125 |>| 131 |=| 137 |<=| 143 
        |<| 149 |+| 155 |**| 161 |*| 173)) 
    (QUOTE (((|commutative| "*") . 0)))
    (CONS 
      (|makeByteWordVec2| 1 (QUOTE (0 0 0 0 0 0 0 0 0 0 0 0 0)))
      (CONS 
        (QUOTE 
          #(NIL NIL NIL NIL NIL 
            |Monoid&| 
            |AbelianMonoid&|
            |OrderedSet&|
            |SemiGroup&|
            |AbelianSemiGroup&|
            |SetCategory&|
            |BasicType&|
            NIL))
        (CONS 
          (QUOTE 
            #((|OrderedAbelianMonoidSup|)
              (|OrderedCancellationAbelianMonoid|)
              (|OrderedAbelianMonoid|)
              (|OrderedAbelianSemiGroup|)
              (|CancellationAbelianMonoid|)
              (|Monoid|)
              (|AbelianMonoid|)
              (|OrderedSet|)
              (|SemiGroup|)
              (|AbelianSemiGroup|)
              (|SetCategory|)
              (|BasicType|)
              (|CoercibleTo| 16)))
          (|makeByteWordVec2| 16 
            (QUOTE 
              (2 0 12 0 0 1 1 0 12 0 1 2 0 0 0 0 6 2 0 8 0 0 9 2 0 0 0 5 7 0 0
               0 1 2 0 0 0 0 1 1 0 8 0 1 1 0 0 0 1 2 0 0 0 0 1 1 0 12 0 1 2 0
               0 0 0 1 2 0 0 0 0 1 1 0 15 0 1 1 0 14 0 1 2 0 0 0 0 1 2 0 8 0 0
               1 2 0 10 0 0 1 1 0 16 0 1 2 0 0 0 11 1 2 0 0 0 13 1 0 0 0 1 0 0
               0 1 2 0 12 0 0 1 2 0 12 0 0 1 2 0 12 0 0 1 2 0 12 0 0 1 2 0 12
               0 0 1 2 0 0 0 0 1 2 0 0 0 11 1 2 0 0 0 13 1 2 0 0 0 0 1 2 0 0 
               11 0 1 2 0 0 13 0 1))))))
     (QUOTE |lookupComplete|))) 

(MAKEPROP (QUOTE |NonNegativeInteger|) (QUOTE NILADIC) T) 

