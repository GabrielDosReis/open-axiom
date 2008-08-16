
(/VERSIONCHECK 2) 

(DEFUN |EUCDOM-;sizeLess?;2SB;1| (|x| |y| $)
  (COND
    ((SPADCALL |y| (|getShellEntry| $ 8)) 'NIL)
    ((SPADCALL |x| (|getShellEntry| $ 8)) 'T)
    ('T
     (< (SPADCALL |x| (|getShellEntry| $ 10))
        (SPADCALL |y| (|getShellEntry| $ 10)))))) 

(DEFUN |EUCDOM-;quo;3S;2| (|x| |y| $)
  (QCAR (SPADCALL |x| |y| (|getShellEntry| $ 13)))) 

(DEFUN |EUCDOM-;rem;3S;3| (|x| |y| $)
  (QCDR (SPADCALL |x| |y| (|getShellEntry| $ 13)))) 

(DEFUN |EUCDOM-;exquo;2SU;4| (|x| |y| $)
  (PROG (|qr|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |y| (|getShellEntry| $ 8)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |qr| (SPADCALL |x| |y| (|getShellEntry| $ 13))
                         |EUCDOM-;exquo;2SU;4|)
                   (EXIT (COND
                           ((SPADCALL (QCDR |qr|)
                                (|getShellEntry| $ 8))
                            (CONS 0 (QCAR |qr|)))
                           ('T (CONS 1 "failed"))))))))))) 

(DEFUN |EUCDOM-;gcd;3S;5| (|x| |y| $)
  (PROG (|#G13| |#G14|)
    (RETURN
      (SEQ (LETT |x| (SPADCALL |x| (|getShellEntry| $ 18))
                 |EUCDOM-;gcd;3S;5|)
           (LETT |y| (SPADCALL |y| (|getShellEntry| $ 18))
                 |EUCDOM-;gcd;3S;5|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (SPADCALL |y| (|getShellEntry| $ 8))
                             (|getShellEntry| $ 19)))
                   (GO G191)))
                (SEQ (PROGN
                       (LETT |#G13| |y| |EUCDOM-;gcd;3S;5|)
                       (LETT |#G14|
                             (SPADCALL |x| |y| (|getShellEntry| $ 20))
                             |EUCDOM-;gcd;3S;5|)
                       (LETT |x| |#G13| |EUCDOM-;gcd;3S;5|)
                       (LETT |y| |#G14| |EUCDOM-;gcd;3S;5|))
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 18))
                                 |EUCDOM-;gcd;3S;5|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |EUCDOM-;unitNormalizeIdealElt| (|s| $)
  (PROG (|#G16| |u| |c| |a|)
    (RETURN
      (SEQ (PROGN
             (LETT |#G16|
                   (SPADCALL (QVELT |s| 2) (|getShellEntry| $ 23))
                   |EUCDOM-;unitNormalizeIdealElt|)
             (LETT |u| (QVELT |#G16| 0)
                   |EUCDOM-;unitNormalizeIdealElt|)
             (LETT |c| (QVELT |#G16| 1)
                   |EUCDOM-;unitNormalizeIdealElt|)
             (LETT |a| (QVELT |#G16| 2)
                   |EUCDOM-;unitNormalizeIdealElt|)
             |#G16|)
           (EXIT (COND
                   ((SPADCALL |a| (|spadConstant| $ 24)
                        (|getShellEntry| $ 25))
                    |s|)
                   ('T
                    (VECTOR (SPADCALL |a| (QVELT |s| 0)
                                (|getShellEntry| $ 26))
                            (SPADCALL |a| (QVELT |s| 1)
                                      (|getShellEntry| $ 26))
                            |c|)))))))) 

(DEFUN |EUCDOM-;extendedEuclidean;2SR;7| (|x| |y| $)
  (PROG (|s3| |s2| |qr| |s1|)
    (RETURN
      (SEQ (LETT |s1|
                 (|EUCDOM-;unitNormalizeIdealElt|
                     (VECTOR (|spadConstant| $ 24)
                             (|spadConstant| $ 27) |x|)
                     $)
                 |EUCDOM-;extendedEuclidean;2SR;7|)
           (LETT |s2|
                 (|EUCDOM-;unitNormalizeIdealElt|
                     (VECTOR (|spadConstant| $ 27)
                             (|spadConstant| $ 24) |y|)
                     $)
                 |EUCDOM-;extendedEuclidean;2SR;7|)
           (EXIT (COND
                   ((SPADCALL |y| (|getShellEntry| $ 8)) |s1|)
                   ((SPADCALL |x| (|getShellEntry| $ 8)) |s2|)
                   ('T
                    (SEQ (SEQ G190
                              (COND
                                ((NULL (SPADCALL
                                        (SPADCALL (QVELT |s2| 2)
                                         (|getShellEntry| $ 8))
                                        (|getShellEntry| $ 19)))
                                 (GO G191)))
                              (SEQ (LETT |qr|
                                    (SPADCALL (QVELT |s1| 2)
                                     (QVELT |s2| 2)
                                     (|getShellEntry| $ 13))
                                    |EUCDOM-;extendedEuclidean;2SR;7|)
                                   (LETT |s3|
                                    (VECTOR
                                     (SPADCALL (QVELT |s1| 0)
                                      (SPADCALL (QCAR |qr|)
                                       (QVELT |s2| 0)
                                       (|getShellEntry| $ 26))
                                      (|getShellEntry| $ 28))
                                     (SPADCALL (QVELT |s1| 1)
                                      (SPADCALL (QCAR |qr|)
                                       (QVELT |s2| 1)
                                       (|getShellEntry| $ 26))
                                      (|getShellEntry| $ 28))
                                     (QCDR |qr|))
                                    |EUCDOM-;extendedEuclidean;2SR;7|)
                                   (LETT |s1| |s2|
                                    |EUCDOM-;extendedEuclidean;2SR;7|)
                                   (EXIT
                                    (LETT |s2|
                                     (|EUCDOM-;unitNormalizeIdealElt|
                                      |s3| $)
                                     |EUCDOM-;extendedEuclidean;2SR;7|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (COND
                           ((NULL (SPADCALL (QVELT |s1| 0)
                                      (|getShellEntry| $ 8)))
                            (COND
                              ((NULL (SPADCALL (QVELT |s1| 0) |y|
                                      (|getShellEntry| $ 29)))
                               (SEQ (LETT |qr|
                                     (SPADCALL (QVELT |s1| 0) |y|
                                      (|getShellEntry| $ 13))
                                     |EUCDOM-;extendedEuclidean;2SR;7|)
                                    (QSETVELT |s1| 0 (QCDR |qr|))
                                    (QSETVELT |s1| 1
                                     (SPADCALL (QVELT |s1| 1)
                                      (SPADCALL (QCAR |qr|) |x|
                                       (|getShellEntry| $ 26))
                                      (|getShellEntry| $ 30)))
                                    (EXIT
                                     (LETT |s1|
                                      (|EUCDOM-;unitNormalizeIdealElt|
                                       |s1| $)
                                      |EUCDOM-;extendedEuclidean;2SR;7|)))))))
                         (EXIT |s1|))))))))) 

(DEFUN |EUCDOM-;extendedEuclidean;3SU;8| (|x| |y| |z| $)
  (PROG (|s| |w| |qr|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |z| (|getShellEntry| $ 8))
              (CONS 0
                    (CONS (|spadConstant| $ 27) (|spadConstant| $ 27))))
             ('T
              (SEQ (LETT |s| (SPADCALL |x| |y| (|getShellEntry| $ 33))
                         |EUCDOM-;extendedEuclidean;3SU;8|)
                   (LETT |w|
                         (SPADCALL |z| (QVELT |s| 2)
                             (|getShellEntry| $ 34))
                         |EUCDOM-;extendedEuclidean;3SU;8|)
                   (EXIT (COND
                           ((QEQCAR |w| 1) (CONS 1 "failed"))
                           ((SPADCALL |y| (|getShellEntry| $ 8))
                            (CONS 0
                                  (CONS (SPADCALL (QVELT |s| 0)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 26))
                                        (SPADCALL (QVELT |s| 1)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 26)))))
                           ('T
                            (SEQ (LETT |qr|
                                       (SPADCALL
                                        (SPADCALL (QVELT |s| 0)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 26))
                                        |y| (|getShellEntry| $ 13))
                                       |EUCDOM-;extendedEuclidean;3SU;8|)
                                 (EXIT (CONS 0
                                        (CONS (QCDR |qr|)
                                         (SPADCALL
                                          (SPADCALL (QVELT |s| 1)
                                           (QCDR |w|)
                                           (|getShellEntry| $ 26))
                                          (SPADCALL (QCAR |qr|) |x|
                                           (|getShellEntry| $ 26))
                                          (|getShellEntry| $ 30)))))))))))))))) 

(DEFUN |EUCDOM-;principalIdeal;LR;9| (|l| $)
  (PROG (|uca| |v| |u| #0=#:G1478 |vv| #1=#:G1479)
    (RETURN
      (SEQ (COND
             ((SPADCALL |l| NIL (|getShellEntry| $ 39))
              (|error| "empty list passed to principalIdeal"))
             ((SPADCALL (CDR |l|) NIL (|getShellEntry| $ 39))
              (SEQ (LETT |uca|
                         (SPADCALL (|SPADfirst| |l|)
                             (|getShellEntry| $ 23))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (LIST (QVELT |uca| 0)) (QVELT |uca| 1)))))
             ((SPADCALL (CDR (CDR |l|)) NIL (|getShellEntry| $ 39))
              (SEQ (LETT |u|
                         (SPADCALL (|SPADfirst| |l|)
                             (SPADCALL |l| (|getShellEntry| $ 40))
                             (|getShellEntry| $ 33))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (LIST (QVELT |u| 0) (QVELT |u| 1))
                               (QVELT |u| 2)))))
             ('T
              (SEQ (LETT |v|
                         (SPADCALL (CDR |l|) (|getShellEntry| $ 43))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (LETT |u|
                         (SPADCALL (|SPADfirst| |l|) (QCDR |v|)
                             (|getShellEntry| $ 33))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (CONS (QVELT |u| 0)
                                     (PROGN
                                       (LETT #0# NIL
                                        |EUCDOM-;principalIdeal;LR;9|)
                                       (SEQ
                                        (LETT |vv| NIL
                                         |EUCDOM-;principalIdeal;LR;9|)
                                        (LETT #1# (QCAR |v|)
                                         |EUCDOM-;principalIdeal;LR;9|)
                                        G190
                                        (COND
                                          ((OR (ATOM #1#)
                                            (PROGN
                                              (LETT |vv| (CAR #1#)
                                               |EUCDOM-;principalIdeal;LR;9|)
                                              NIL))
                                           (GO G191)))
                                        (SEQ
                                         (EXIT
                                          (LETT #0#
                                           (CONS
                                            (SPADCALL (QVELT |u| 1)
                                             |vv|
                                             (|getShellEntry| $ 26))
                                            #0#)
                                           |EUCDOM-;principalIdeal;LR;9|)))
                                        (LETT #1# (CDR #1#)
                                         |EUCDOM-;principalIdeal;LR;9|)
                                        (GO G190) G191
                                        (EXIT (NREVERSE0 #0#)))))
                               (QVELT |u| 2)))))))))) 

(DEFUN |EUCDOM-;expressIdealMember;LSU;10| (|l| |z| $)
  (PROG (#0=#:G1494 #1=#:G1495 |pid| |q| #2=#:G1496 |v| #3=#:G1497)
    (RETURN
      (SEQ (COND
             ((SPADCALL |z| (|spadConstant| $ 27)
                  (|getShellEntry| $ 25))
              (CONS 0
                    (PROGN
                      (LETT #0# NIL
                            |EUCDOM-;expressIdealMember;LSU;10|)
                      (SEQ (LETT |v| NIL
                                 |EUCDOM-;expressIdealMember;LSU;10|)
                           (LETT #1# |l|
                                 |EUCDOM-;expressIdealMember;LSU;10|)
                           G190
                           (COND
                             ((OR (ATOM #1#)
                                  (PROGN
                                    (LETT |v| (CAR #1#)
                                     |EUCDOM-;expressIdealMember;LSU;10|)
                                    NIL))
                              (GO G191)))
                           (SEQ (EXIT (LETT #0#
                                       (CONS (|spadConstant| $ 27) #0#)
                                       |EUCDOM-;expressIdealMember;LSU;10|)))
                           (LETT #1# (CDR #1#)
                                 |EUCDOM-;expressIdealMember;LSU;10|)
                           (GO G190) G191 (EXIT (NREVERSE0 #0#))))))
             ('T
              (SEQ (LETT |pid| (SPADCALL |l| (|getShellEntry| $ 43))
                         |EUCDOM-;expressIdealMember;LSU;10|)
                   (LETT |q|
                         (SPADCALL |z| (QCDR |pid|)
                             (|getShellEntry| $ 34))
                         |EUCDOM-;expressIdealMember;LSU;10|)
                   (EXIT (COND
                           ((QEQCAR |q| 1) (CONS 1 "failed"))
                           ('T
                            (CONS 0
                                  (PROGN
                                    (LETT #2# NIL
                                     |EUCDOM-;expressIdealMember;LSU;10|)
                                    (SEQ
                                     (LETT |v| NIL
                                      |EUCDOM-;expressIdealMember;LSU;10|)
                                     (LETT #3# (QCAR |pid|)
                                      |EUCDOM-;expressIdealMember;LSU;10|)
                                     G190
                                     (COND
                                       ((OR (ATOM #3#)
                                         (PROGN
                                           (LETT |v| (CAR #3#)
                                            |EUCDOM-;expressIdealMember;LSU;10|)
                                           NIL))
                                        (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (LETT #2#
                                        (CONS
                                         (SPADCALL (QCDR |q|) |v|
                                          (|getShellEntry| $ 26))
                                         #2#)
                                        |EUCDOM-;expressIdealMember;LSU;10|)))
                                     (LETT #3# (CDR #3#)
                                      |EUCDOM-;expressIdealMember;LSU;10|)
                                     (GO G190) G191
                                     (EXIT (NREVERSE0 #2#))))))))))))))) 

(DEFUN |EUCDOM-;multiEuclidean;LSU;11| (|l| |z| $)
  (PROG (|n| |l1| |l2| #0=#:G1392 #1=#:G1516 #2=#:G1503 #3=#:G1501
             #4=#:G1502 #5=#:G1393 #6=#:G1517 #7=#:G1506 #8=#:G1504
             #9=#:G1505 |u| |v1| |v2|)
    (RETURN
      (SEQ (LETT |n| (LENGTH |l|) |EUCDOM-;multiEuclidean;LSU;11|)
           (EXIT (COND
                   ((ZEROP |n|)
                    (|error| "empty list passed to multiEuclidean"))
                   ((EQL |n| 1) (CONS 0 (LIST |z|)))
                   ('T
                    (SEQ (LETT |l1|
                               (SPADCALL |l| (|getShellEntry| $ 47))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (LETT |l2|
                               (SPADCALL |l1| (QUOTIENT2 |n| 2)
                                   (|getShellEntry| $ 49))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (LETT |u|
                               (SPADCALL
                                   (PROGN
                                     (LETT #4# NIL
                                      |EUCDOM-;multiEuclidean;LSU;11|)
                                     (SEQ
                                      (LETT #0# NIL
                                       |EUCDOM-;multiEuclidean;LSU;11|)
                                      (LETT #1# |l1|
                                       |EUCDOM-;multiEuclidean;LSU;11|)
                                      G190
                                      (COND
                                        ((OR (ATOM #1#)
                                          (PROGN
                                            (LETT #0# (CAR #1#)
                                             |EUCDOM-;multiEuclidean;LSU;11|)
                                            NIL))
                                         (GO G191)))
                                      (SEQ
                                       (EXIT
                                        (PROGN
                                          (LETT #2# #0#
                                           |EUCDOM-;multiEuclidean;LSU;11|)
                                          (COND
                                            (#4#
                                             (LETT #3#
                                              (SPADCALL #3# #2#
                                               (|getShellEntry| $ 26))
                                              |EUCDOM-;multiEuclidean;LSU;11|))
                                            ('T
                                             (PROGN
                                               (LETT #3# #2#
                                                |EUCDOM-;multiEuclidean;LSU;11|)
                                               (LETT #4# 'T
                                                |EUCDOM-;multiEuclidean;LSU;11|)))))))
                                      (LETT #1# (CDR #1#)
                                       |EUCDOM-;multiEuclidean;LSU;11|)
                                      (GO G190) G191 (EXIT NIL))
                                     (COND
                                       (#4# #3#)
                                       ('T (|spadConstant| $ 24))))
                                   (PROGN
                                     (LETT #9# NIL
                                      |EUCDOM-;multiEuclidean;LSU;11|)
                                     (SEQ
                                      (LETT #5# NIL
                                       |EUCDOM-;multiEuclidean;LSU;11|)
                                      (LETT #6# |l2|
                                       |EUCDOM-;multiEuclidean;LSU;11|)
                                      G190
                                      (COND
                                        ((OR (ATOM #6#)
                                          (PROGN
                                            (LETT #5# (CAR #6#)
                                             |EUCDOM-;multiEuclidean;LSU;11|)
                                            NIL))
                                         (GO G191)))
                                      (SEQ
                                       (EXIT
                                        (PROGN
                                          (LETT #7# #5#
                                           |EUCDOM-;multiEuclidean;LSU;11|)
                                          (COND
                                            (#9#
                                             (LETT #8#
                                              (SPADCALL #8# #7#
                                               (|getShellEntry| $ 26))
                                              |EUCDOM-;multiEuclidean;LSU;11|))
                                            ('T
                                             (PROGN
                                               (LETT #8# #7#
                                                |EUCDOM-;multiEuclidean;LSU;11|)
                                               (LETT #9# 'T
                                                |EUCDOM-;multiEuclidean;LSU;11|)))))))
                                      (LETT #6# (CDR #6#)
                                       |EUCDOM-;multiEuclidean;LSU;11|)
                                      (GO G190) G191 (EXIT NIL))
                                     (COND
                                       (#9# #8#)
                                       ('T (|spadConstant| $ 24))))
                                   |z| (|getShellEntry| $ 50))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (EXIT (COND
                                 ((QEQCAR |u| 1) (CONS 1 "failed"))
                                 ('T
                                  (SEQ (LETT |v1|
                                        (SPADCALL |l1|
                                         (QCDR (QCDR |u|))
                                         (|getShellEntry| $ 51))
                                        |EUCDOM-;multiEuclidean;LSU;11|)
                                       (EXIT
                                        (COND
                                          ((QEQCAR |v1| 1)
                                           (CONS 1 "failed"))
                                          ('T
                                           (SEQ
                                            (LETT |v2|
                                             (SPADCALL |l2|
                                              (QCAR (QCDR |u|))
                                              (|getShellEntry| $ 51))
                                             |EUCDOM-;multiEuclidean;LSU;11|)
                                            (EXIT
                                             (COND
                                               ((QEQCAR |v2| 1)
                                                (CONS 1 "failed"))
                                               ('T
                                                (CONS 0
                                                 (SPADCALL (QCDR |v1|)
                                                  (QCDR |v2|)
                                                  (|getShellEntry| $
                                                   52)))))))))))))))))))))) 

(DEFUN |EuclideanDomain&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|EuclideanDomain&|))
        (LETT |dv$| (LIST '|EuclideanDomain&| |dv$1|) . #0#)
        (LETT $ (|newShell| 54) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|EuclideanDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Boolean|)
             (0 . |zero?|) (|NonNegativeInteger|) (5 . |euclideanSize|)
             |EUCDOM-;sizeLess?;2SB;1|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (10 . |divide|) |EUCDOM-;quo;3S;2| |EUCDOM-;rem;3S;3|
             (|Union| $ '"failed") |EUCDOM-;exquo;2SU;4|
             (16 . |unitCanonical|) (21 . |not|) (26 . |rem|)
             |EUCDOM-;gcd;3S;5|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (32 . |unitNormal|) (37 . |One|) (41 . =) (47 . *)
             (53 . |Zero|) (57 . -) (63 . |sizeLess?|) (69 . +)
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             |EUCDOM-;extendedEuclidean;2SR;7|
             (75 . |extendedEuclidean|) (81 . |exquo|)
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 35 '"failed") |EUCDOM-;extendedEuclidean;3SU;8|
             (|List| 6) (87 . =) (93 . |second|) (|List| $)
             (|Record| (|:| |coef| 41) (|:| |generator| $))
             (98 . |principalIdeal|) |EUCDOM-;principalIdeal;LR;9|
             (|Union| 41 '"failed") |EUCDOM-;expressIdealMember;LSU;10|
             (103 . |copy|) (|Integer|) (108 . |split!|)
             (114 . |extendedEuclidean|) (121 . |multiEuclidean|)
             (127 . |concat|) |EUCDOM-;multiEuclidean;LSU;11|)
          '#(|sizeLess?| 133 |rem| 139 |quo| 145 |principalIdeal| 151
             |multiEuclidean| 156 |gcd| 162 |extendedEuclidean| 168
             |exquo| 181 |expressIdealMember| 187)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 53
                                '(1 6 7 0 8 1 6 9 0 10 2 6 12 0 0 13 1
                                  6 0 0 18 1 7 0 0 19 2 6 0 0 0 20 1 6
                                  22 0 23 0 6 0 24 2 6 7 0 0 25 2 6 0 0
                                  0 26 0 6 0 27 2 6 0 0 0 28 2 6 7 0 0
                                  29 2 6 0 0 0 30 2 6 31 0 0 33 2 6 16
                                  0 0 34 2 38 7 0 0 39 1 38 6 0 40 1 6
                                  42 41 43 1 38 0 0 47 2 38 0 0 48 49 3
                                  6 36 0 0 0 50 2 6 45 41 0 51 2 38 0 0
                                  0 52 2 0 7 0 0 11 2 0 0 0 0 15 2 0 0
                                  0 0 14 1 0 42 41 44 2 0 45 41 0 53 2
                                  0 0 0 0 21 3 0 36 0 0 0 37 2 0 31 0 0
                                  32 2 0 16 0 0 17 2 0 45 41 0 46)))))
          '|lookupComplete|)) 
