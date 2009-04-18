
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |EUCDOM-;sizeLess?;2SB;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |EUCDOM-;quo;3S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |EUCDOM-;rem;3S;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |EUCDOM-;exquo;2SU;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |EUCDOM-;gcd;3S;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell| |%Shell|) |%Shell|)
                |EUCDOM-;unitNormalizeIdealElt|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Shell|)
                |EUCDOM-;extendedEuclidean;2SR;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Pair|)
                |EUCDOM-;extendedEuclidean;3SU;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Pair|)
                |EUCDOM-;principalIdeal;LR;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |EUCDOM-;expressIdealMember;LSU;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |EUCDOM-;multiEuclidean;LSU;11|)) 

(DEFUN |EUCDOM-;sizeLess?;2SB;1| (|x| |y| $)
  (COND
    ((SPADCALL |y| (|getShellEntry| $ 8)) 'NIL)
    ((SPADCALL |x| (|getShellEntry| $ 8)) 'T)
    ('T
     (< (SPADCALL |x| (|getShellEntry| $ 12))
        (SPADCALL |y| (|getShellEntry| $ 12)))))) 

(DEFUN |EUCDOM-;quo;3S;2| (|x| |y| $)
  (QCAR (SPADCALL |x| |y| (|getShellEntry| $ 16)))) 

(DEFUN |EUCDOM-;rem;3S;3| (|x| |y| $)
  (QCDR (SPADCALL |x| |y| (|getShellEntry| $ 16)))) 

(DEFUN |EUCDOM-;exquo;2SU;4| (|x| |y| $)
  (PROG (|qr|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (|getShellEntry| $ 8))
              (CONS 0 (|spadConstant| $ 19)))
             ((SPADCALL |y| (|getShellEntry| $ 8)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |qr| (SPADCALL |x| |y| (|getShellEntry| $ 16))
                         |EUCDOM-;exquo;2SU;4|)
                   (EXIT (COND
                           ((SPADCALL (QCDR |qr|)
                                (|getShellEntry| $ 8))
                            (CONS 0 (QCAR |qr|)))
                           ('T (CONS 1 "failed"))))))))))) 

(DEFUN |EUCDOM-;gcd;3S;5| (|x| |y| $)
  (PROG (|#G13| |#G14|)
    (RETURN
      (SEQ (LETT |x| (SPADCALL |x| (|getShellEntry| $ 22))
                 |EUCDOM-;gcd;3S;5|)
           (LETT |y| (SPADCALL |y| (|getShellEntry| $ 22))
                 |EUCDOM-;gcd;3S;5|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |y| (|getShellEntry| $ 8))))
                   (GO G191)))
                (SEQ (PROGN
                       (LETT |#G13| |y| |EUCDOM-;gcd;3S;5|)
                       (LETT |#G14|
                             (SPADCALL |x| |y| (|getShellEntry| $ 23))
                             |EUCDOM-;gcd;3S;5|)
                       (LETT |x| |#G13| |EUCDOM-;gcd;3S;5|)
                       (LETT |y| |#G14| |EUCDOM-;gcd;3S;5|))
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 22))
                                 |EUCDOM-;gcd;3S;5|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |EUCDOM-;unitNormalizeIdealElt| (|s| $)
  (PROG (|#G16| |u| |c| |a|)
    (RETURN
      (SEQ (PROGN
             (LETT |#G16|
                   (SPADCALL (QVELT |s| 2) (|getShellEntry| $ 26))
                   |EUCDOM-;unitNormalizeIdealElt|)
             (LETT |u| (QVELT |#G16| 0)
                   |EUCDOM-;unitNormalizeIdealElt|)
             (LETT |c| (QVELT |#G16| 1)
                   |EUCDOM-;unitNormalizeIdealElt|)
             (LETT |a| (QVELT |#G16| 2)
                   |EUCDOM-;unitNormalizeIdealElt|)
             |#G16|)
           (EXIT (COND
                   ((SPADCALL |a| (|getShellEntry| $ 27)) |s|)
                   ('T
                    (VECTOR (SPADCALL |a| (QVELT |s| 0)
                                (|getShellEntry| $ 28))
                            (SPADCALL |a| (QVELT |s| 1)
                                      (|getShellEntry| $ 28))
                            |c|)))))))) 

(DEFUN |EUCDOM-;extendedEuclidean;2SR;7| (|x| |y| $)
  (PROG (|s3| |s2| |qr| |s1|)
    (RETURN
      (SEQ (LETT |s1|
                 (|EUCDOM-;unitNormalizeIdealElt|
                     (VECTOR (|spadConstant| $ 29)
                             (|spadConstant| $ 19) |x|)
                     $)
                 |EUCDOM-;extendedEuclidean;2SR;7|)
           (LETT |s2|
                 (|EUCDOM-;unitNormalizeIdealElt|
                     (VECTOR (|spadConstant| $ 19)
                             (|spadConstant| $ 29) |y|)
                     $)
                 |EUCDOM-;extendedEuclidean;2SR;7|)
           (EXIT (COND
                   ((SPADCALL |y| (|getShellEntry| $ 8)) |s1|)
                   ((SPADCALL |x| (|getShellEntry| $ 8)) |s2|)
                   ('T
                    (SEQ (SEQ G190
                              (COND
                                ((NULL (NOT
                                        (SPADCALL (QVELT |s2| 2)
                                         (|getShellEntry| $ 8))))
                                 (GO G191)))
                              (SEQ (LETT |qr|
                                    (SPADCALL (QVELT |s1| 2)
                                     (QVELT |s2| 2)
                                     (|getShellEntry| $ 16))
                                    |EUCDOM-;extendedEuclidean;2SR;7|)
                                   (LETT |s3|
                                    (VECTOR
                                     (SPADCALL (QVELT |s1| 0)
                                      (SPADCALL (QCAR |qr|)
                                       (QVELT |s2| 0)
                                       (|getShellEntry| $ 28))
                                      (|getShellEntry| $ 30))
                                     (SPADCALL (QVELT |s1| 1)
                                      (SPADCALL (QCAR |qr|)
                                       (QVELT |s2| 1)
                                       (|getShellEntry| $ 28))
                                      (|getShellEntry| $ 30))
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
                           ((NOT (SPADCALL (QVELT |s1| 0)
                                     (|getShellEntry| $ 8)))
                            (COND
                              ((NOT (SPADCALL (QVELT |s1| 0) |y|
                                     (|getShellEntry| $ 31)))
                               (SEQ (LETT |qr|
                                     (SPADCALL (QVELT |s1| 0) |y|
                                      (|getShellEntry| $ 16))
                                     |EUCDOM-;extendedEuclidean;2SR;7|)
                                    (QSETVELT |s1| 0 (QCDR |qr|))
                                    (QSETVELT |s1| 1
                                     (SPADCALL (QVELT |s1| 1)
                                      (SPADCALL (QCAR |qr|) |x|
                                       (|getShellEntry| $ 28))
                                      (|getShellEntry| $ 32)))
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
                    (CONS (|spadConstant| $ 19) (|spadConstant| $ 19))))
             ('T
              (SEQ (LETT |s| (SPADCALL |x| |y| (|getShellEntry| $ 35))
                         |EUCDOM-;extendedEuclidean;3SU;8|)
                   (LETT |w|
                         (SPADCALL |z| (QVELT |s| 2)
                             (|getShellEntry| $ 36))
                         |EUCDOM-;extendedEuclidean;3SU;8|)
                   (EXIT (COND
                           ((QEQCAR |w| 1) (CONS 1 "failed"))
                           ((SPADCALL |y| (|getShellEntry| $ 8))
                            (CONS 0
                                  (CONS (SPADCALL (QVELT |s| 0)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 28))
                                        (SPADCALL (QVELT |s| 1)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 28)))))
                           ('T
                            (SEQ (LETT |qr|
                                       (SPADCALL
                                        (SPADCALL (QVELT |s| 0)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 28))
                                        |y| (|getShellEntry| $ 16))
                                       |EUCDOM-;extendedEuclidean;3SU;8|)
                                 (EXIT (CONS 0
                                        (CONS (QCDR |qr|)
                                         (SPADCALL
                                          (SPADCALL (QVELT |s| 1)
                                           (QCDR |w|)
                                           (|getShellEntry| $ 28))
                                          (SPADCALL (QCAR |qr|) |x|
                                           (|getShellEntry| $ 28))
                                          (|getShellEntry| $ 32)))))))))))))))) 

(DEFUN |EUCDOM-;principalIdeal;LR;9| (|l| $)
  (PROG (|uca| |v| |u| #0=#:G1519 |vv| #1=#:G1520)
    (RETURN
      (SEQ (COND
             ((SPADCALL |l| NIL (|getShellEntry| $ 41))
              (|error| "empty list passed to principalIdeal"))
             ((SPADCALL (CDR |l|) NIL (|getShellEntry| $ 41))
              (SEQ (LETT |uca|
                         (SPADCALL (|SPADfirst| |l|)
                             (|getShellEntry| $ 26))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (LIST (QVELT |uca| 0)) (QVELT |uca| 1)))))
             ((SPADCALL (CDR (CDR |l|)) NIL (|getShellEntry| $ 41))
              (SEQ (LETT |u|
                         (SPADCALL (|SPADfirst| |l|)
                             (SPADCALL |l| (|getShellEntry| $ 44))
                             (|getShellEntry| $ 35))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (LIST (QVELT |u| 0) (QVELT |u| 1))
                               (QVELT |u| 2)))))
             ('T
              (SEQ (LETT |v|
                         (SPADCALL (CDR |l|) (|getShellEntry| $ 47))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (LETT |u|
                         (SPADCALL (|SPADfirst| |l|) (QCDR |v|)
                             (|getShellEntry| $ 35))
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
                                             (|getShellEntry| $ 28))
                                            #0#)
                                           |EUCDOM-;principalIdeal;LR;9|)))
                                        (LETT #1# (CDR #1#)
                                         |EUCDOM-;principalIdeal;LR;9|)
                                        (GO G190) G191
                                        (EXIT (NREVERSE0 #0#)))))
                               (QVELT |u| 2)))))))))) 

(DEFUN |EUCDOM-;expressIdealMember;LSU;10| (|l| |z| $)
  (PROG (#0=#:G1521 #1=#:G1522 |pid| |q| #2=#:G1523 |v| #3=#:G1524)
    (RETURN
      (SEQ (COND
             ((SPADCALL |z| (|spadConstant| $ 19)
                  (|getShellEntry| $ 50))
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
                                       (CONS (|spadConstant| $ 19) #0#)
                                       |EUCDOM-;expressIdealMember;LSU;10|)))
                           (LETT #1# (CDR #1#)
                                 |EUCDOM-;expressIdealMember;LSU;10|)
                           (GO G190) G191 (EXIT (NREVERSE0 #0#))))))
             ('T
              (SEQ (LETT |pid| (SPADCALL |l| (|getShellEntry| $ 47))
                         |EUCDOM-;expressIdealMember;LSU;10|)
                   (LETT |q|
                         (SPADCALL |z| (QCDR |pid|)
                             (|getShellEntry| $ 36))
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
                                          (|getShellEntry| $ 28))
                                         #2#)
                                        |EUCDOM-;expressIdealMember;LSU;10|)))
                                     (LETT #3# (CDR #3#)
                                      |EUCDOM-;expressIdealMember;LSU;10|)
                                     (GO G190) G191
                                     (EXIT (NREVERSE0 #2#))))))))))))))) 

(DEFUN |EUCDOM-;multiEuclidean;LSU;11| (|l| |z| $)
  (PROG (|n| |l1| |l2| #0=#:G1398 #1=#:G1525 #2=#:G1506 #3=#:G1504
             #4=#:G1505 #5=#:G1399 #6=#:G1526 #7=#:G1509 #8=#:G1507
             #9=#:G1508 |u| |v1| |v2|)
    (RETURN
      (SEQ (LETT |n| (LENGTH |l|) |EUCDOM-;multiEuclidean;LSU;11|)
           (EXIT (COND
                   ((ZEROP |n|)
                    (|error| "empty list passed to multiEuclidean"))
                   ((EQL |n| 1) (CONS 0 (LIST |z|)))
                   ('T
                    (SEQ (LETT |l1|
                               (SPADCALL |l| (|getShellEntry| $ 57))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (LETT |l2|
                               (SPADCALL |l1| (QUOTIENT2 |n| 2)
                                   (|getShellEntry| $ 60))
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
                                               (|getShellEntry| $ 28))
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
                                       ('T (|spadConstant| $ 29))))
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
                                               (|getShellEntry| $ 28))
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
                                       ('T (|spadConstant| $ 29))))
                                   |z| (|getShellEntry| $ 61))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (EXIT (COND
                                 ((QEQCAR |u| 1) (CONS 1 "failed"))
                                 ('T
                                  (SEQ (LETT |v1|
                                        (SPADCALL |l1|
                                         (QCDR (QCDR |u|))
                                         (|getShellEntry| $ 62))
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
                                              (|getShellEntry| $ 62))
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
                                                   63)))))))))))))))))))))) 

(DEFUN |EuclideanDomain&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|EuclideanDomain&|))
        (LETT |dv$| (LIST '|EuclideanDomain&| |dv$1|) . #0#)
        (LETT $ (|newShell| 65) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|EuclideanDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Boolean|)
             (0 . |zero?|) (5 . |false|) (9 . |true|)
             (|NonNegativeInteger|) (13 . |euclideanSize|) (18 . <)
             |EUCDOM-;sizeLess?;2SB;1|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (24 . |divide|) |EUCDOM-;quo;3S;2| |EUCDOM-;rem;3S;3|
             (30 . |Zero|) (|Union| $ '"failed") |EUCDOM-;exquo;2SU;4|
             (34 . |unitCanonical|) (39 . |rem|) |EUCDOM-;gcd;3S;5|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (45 . |unitNormal|) (50 . |one?|) (55 . *) (61 . |One|)
             (65 . -) (71 . |sizeLess?|) (77 . +)
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             |EUCDOM-;extendedEuclidean;2SR;7|
             (83 . |extendedEuclidean|) (89 . |exquo|)
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 37 '"failed") |EUCDOM-;extendedEuclidean;3SU;8|
             (|List| 6) (95 . =) (101 . |rest|) (106 . |first|)
             (111 . |second|) (|List| $)
             (|Record| (|:| |coef| 45) (|:| |generator| $))
             (116 . |principalIdeal|) (121 . |cons|)
             |EUCDOM-;principalIdeal;LR;9| (127 . =)
             (|Union| 45 '"failed") |EUCDOM-;expressIdealMember;LSU;10|
             (133 . |#|) (138 . |zero?|) (143 . |One|) (147 . =)
             (153 . |copy|) (158 . |quo|) (|Integer|) (164 . |split!|)
             (170 . |extendedEuclidean|) (177 . |multiEuclidean|)
             (183 . |concat|) |EUCDOM-;multiEuclidean;LSU;11|)
          '#(|sizeLess?| 189 |rem| 195 |quo| 201 |principalIdeal| 207
             |multiEuclidean| 212 |gcd| 218 |extendedEuclidean| 224
             |exquo| 237 |expressIdealMember| 243)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 64
                                '(1 6 7 0 8 0 7 0 9 0 7 0 10 1 6 11 0
                                  12 2 11 7 0 0 13 2 6 15 0 0 16 0 6 0
                                  19 1 6 0 0 22 2 6 0 0 0 23 1 6 25 0
                                  26 1 6 7 0 27 2 6 0 0 0 28 0 6 0 29 2
                                  6 0 0 0 30 2 6 7 0 0 31 2 6 0 0 0 32
                                  2 6 33 0 0 35 2 6 20 0 0 36 2 40 7 0
                                  0 41 1 40 0 0 42 1 40 6 0 43 1 40 6 0
                                  44 1 6 46 45 47 2 40 0 6 0 48 2 6 7 0
                                  0 50 1 40 11 0 53 1 11 7 0 54 0 11 0
                                  55 2 11 7 0 0 56 1 40 0 0 57 2 11 0 0
                                  0 58 2 40 0 0 59 60 3 6 38 0 0 0 61 2
                                  6 51 45 0 62 2 40 0 0 0 63 2 0 7 0 0
                                  14 2 0 0 0 0 18 2 0 0 0 0 17 1 0 46
                                  45 49 2 0 51 45 0 64 2 0 0 0 0 24 3 0
                                  38 0 0 0 39 2 0 33 0 0 34 2 0 20 0 0
                                  21 2 0 51 45 0 52)))))
          '|lookupComplete|)) 
