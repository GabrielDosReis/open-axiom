
(/VERSIONCHECK 2) 

(DEFUN |INTDOM-;unitNormal;SR;1| (|x| $)
  (VECTOR (|spadConstant| $ 7) |x| (|spadConstant| $ 7))) 

(DEFUN |INTDOM-;unitCanonical;2S;2| (|x| $)
  (QVELT (SPADCALL |x| (QREFELT $ 10)) 1)) 

(DEFUN |INTDOM-;recip;SU;3| (|x| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 13)) (CONS 1 "failed"))
    ('T (SPADCALL (|spadConstant| $ 7) |x| (QREFELT $ 15))))) 

(DEFUN |INTDOM-;unit?;SB;4| (|x| $)
  (COND ((QEQCAR (SPADCALL |x| (QREFELT $ 17)) 1) 'NIL) ('T 'T))) 

(DEFUN |INTDOM-;associates?;2SB;5| (|x| |y| $)
  (SPADCALL (QVELT (SPADCALL |x| (QREFELT $ 10)) 1)
      (QVELT (SPADCALL |y| (QREFELT $ 10)) 1) (QREFELT $ 19))) 

(DEFUN |INTDOM-;associates?;2SB;6| (|x| |y| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 13)) (SPADCALL |y| (QREFELT $ 13)))
    ((OR (SPADCALL |y| (QREFELT $ 13))
         (OR (QEQCAR (SPADCALL |x| |y| (QREFELT $ 15)) 1)
             (QEQCAR (SPADCALL |y| |x| (QREFELT $ 15)) 1)))
     'NIL)
    ('T 'T))) 

(DEFUN |IntegralDomain&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IntegralDomain&|))
        (LETT |dv$| (LIST '|IntegralDomain&| |dv$1|) . #0#)
        (LETT $ (GETREFV 21) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (COND
          ((|HasCategory| |#1| '(|Field|)))
          ('T
           (QSETREFV $ 9
               (CONS (|dispatchFunction| |INTDOM-;unitNormal;SR;1|) $))))
        (COND
          ((|HasAttribute| |#1| '|canonicalUnitNormal|)
           (QSETREFV $ 20
               (CONS (|dispatchFunction| |INTDOM-;associates?;2SB;5|)
                     $)))
          ('T
           (QSETREFV $ 20
               (CONS (|dispatchFunction| |INTDOM-;associates?;2SB;6|)
                     $))))
        $)))) 

(MAKEPROP '|IntegralDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |One|)
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (4 . |unitNormal|) (9 . |unitNormal|)
             |INTDOM-;unitCanonical;2S;2| (|Boolean|) (14 . |zero?|)
             (|Union| $ '"failed") (19 . |exquo|) |INTDOM-;recip;SU;3|
             (25 . |recip|) |INTDOM-;unit?;SB;4| (30 . =)
             (36 . |associates?|))
          '#(|unitNormal| 42 |unitCanonical| 47 |unit?| 52 |recip| 57
             |associates?| 62)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 20
                                '(0 6 0 7 1 0 8 0 9 1 6 8 0 10 1 6 12 0
                                  13 2 6 14 0 0 15 1 6 14 0 17 2 6 12 0
                                  0 19 2 0 12 0 0 20 1 0 8 0 9 1 0 0 0
                                  11 1 0 12 0 18 1 0 14 0 16 2 0 12 0 0
                                  20)))))
          '|lookupComplete|)) 
