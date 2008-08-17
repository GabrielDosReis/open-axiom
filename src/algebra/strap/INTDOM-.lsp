
(/VERSIONCHECK 2) 

(DEFUN |INTDOM-;unitNormal;SR;1| (|x| $)
  (VECTOR (|spadConstant| $ 7) |x| (|spadConstant| $ 7))) 

(DEFUN |INTDOM-;unitCanonical;2S;2| (|x| $)
  (QVELT (SPADCALL |x| (|getShellEntry| $ 10)) 1)) 

(DEFUN |INTDOM-;recip;SU;3| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 13)) (CONS 1 "failed"))
    ('T (SPADCALL (|spadConstant| $ 7) |x| (|getShellEntry| $ 15))))) 

(DEFUN |INTDOM-;unit?;SB;4| (|x| $)
  (COND
    ((QEQCAR (SPADCALL |x| (|getShellEntry| $ 17)) 1) 'NIL)
    ('T 'T))) 

(DEFUN |INTDOM-;associates?;2SB;5| (|x| |y| $)
  (SPADCALL (QVELT (SPADCALL |x| (|getShellEntry| $ 10)) 1)
      (QVELT (SPADCALL |y| (|getShellEntry| $ 10)) 1)
      (|getShellEntry| $ 19))) 

(DEFUN |INTDOM-;associates?;2SB;6| (|x| |y| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 13))
     (SPADCALL |y| (|getShellEntry| $ 13)))
    ((OR (SPADCALL |y| (|getShellEntry| $ 13))
         (OR (QEQCAR (SPADCALL |x| |y| (|getShellEntry| $ 15)) 1)
             (QEQCAR (SPADCALL |y| |x| (|getShellEntry| $ 15)) 1)))
     'NIL)
    ('T 'T))) 

(DEFUN |IntegralDomain&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IntegralDomain&|))
        (LETT |dv$| (LIST '|IntegralDomain&| |dv$1|) . #0#)
        (LETT $ (|newShell| 21) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (COND
          ((|HasCategory| |#1| '(|Field|)))
          ('T
           (|setShellEntry| $ 9
               (CONS (|dispatchFunction| |INTDOM-;unitNormal;SR;1|) $))))
        (COND
          ((|HasAttribute| |#1| '|canonicalUnitNormal|)
           (|setShellEntry| $ 20
               (CONS (|dispatchFunction| |INTDOM-;associates?;2SB;5|)
                     $)))
          ('T
           (|setShellEntry| $ 20
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

(SETQ |$CategoryFrame|
      (|put| '|IntegralDomain&| '|isFunctor|
             '(((|unit?| ((|Boolean|) $)) T (ELT $ 18))
               ((|associates?| ((|Boolean|) $ $)) T (ELT $ 20))
               ((|unitCanonical| ($ $)) T (ELT $ 11))
               ((|unitNormal|
                    ((|Record| (|:| |unit| $) (|:| |canonical| $)
                         (|:| |associate| $))
                     $))
                T (ELT $ 9))
               ((|recip| ((|Union| $ "failed") $)) T (ELT $ 16)))
             (|addModemap| '|IntegralDomain&| '(|IntegralDomain&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |unit?| ((|Boolean|) |#1|))
                       (SIGNATURE |associates?|
                                  ((|Boolean|) |#1| |#1|))
                       (SIGNATURE |unitCanonical| (|#1| |#1|))
                       (SIGNATURE |unitNormal|
                           ((|Record| (|:| |unit| |#1|)
                                (|:| |canonical| |#1|)
                                (|:| |associate| |#1|))
                            |#1|))
                       (SIGNATURE |recip|
                           ((|Union| |#1| "failed") |#1|)))
                   (|IntegralDomain|))
                 T '|IntegralDomain&|
                 (|put| '|IntegralDomain&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |unit?| ((|Boolean|) |#1|))
                                 (SIGNATURE |associates?|
                                     ((|Boolean|) |#1| |#1|))
                                 (SIGNATURE |unitCanonical|
                                     (|#1| |#1|))
                                 (SIGNATURE |unitNormal|
                                     ((|Record| (|:| |unit| |#1|)
                                       (|:| |canonical| |#1|)
                                       (|:| |associate| |#1|))
                                      |#1|))
                                 (SIGNATURE |recip|
                                     ((|Union| |#1| "failed") |#1|)))
                             (|IntegralDomain|))
                        |$CategoryFrame|)))) 
