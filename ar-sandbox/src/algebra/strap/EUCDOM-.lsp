
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
    ((SPADCALL |y| (|getShellEntry| $ 8)) NIL)
    ((SPADCALL |x| (|getShellEntry| $ 8)) T)
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
                (SEQ (LETT |#G13| |y| |EUCDOM-;gcd;3S;5|)
                     (LETT |#G14|
                           (SPADCALL |x| |y| (|getShellEntry| $ 24))
                           |EUCDOM-;gcd;3S;5|)
                     (LETT |x| |#G13| |EUCDOM-;gcd;3S;5|)
                     (LETT |y| |#G14| |EUCDOM-;gcd;3S;5|)
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 22))
                                 |EUCDOM-;gcd;3S;5|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |EUCDOM-;unitNormalizeIdealElt| (|s| $)
  (PROG (|#G16| |u| |c| |a|)
    (RETURN
      (SEQ (LETT |#G16| (SPADCALL (QVELT |s| 2) (|getShellEntry| $ 27))
                 |EUCDOM-;unitNormalizeIdealElt|)
           (LETT |u| (QVELT |#G16| 0) |EUCDOM-;unitNormalizeIdealElt|)
           (LETT |c| (QVELT |#G16| 1) |EUCDOM-;unitNormalizeIdealElt|)
           (LETT |a| (QVELT |#G16| 2) |EUCDOM-;unitNormalizeIdealElt|)
           |#G16|
           (EXIT (COND
                   ((SPADCALL |a| (|getShellEntry| $ 28)) |s|)
                   ('T
                    (VECTOR (SPADCALL |a| (QVELT |s| 0)
                                (|getShellEntry| $ 29))
                            (SPADCALL |a| (QVELT |s| 1)
                                      (|getShellEntry| $ 29))
                            |c|)))))))) 

(DEFUN |EUCDOM-;extendedEuclidean;2SR;7| (|x| |y| $)
  (PROG (|s3| |s2| |qr| |s1|)
    (RETURN
      (SEQ (LETT |s1|
                 (|EUCDOM-;unitNormalizeIdealElt|
                     (VECTOR (|spadConstant| $ 30)
                             (|spadConstant| $ 19) |x|)
                     $)
                 |EUCDOM-;extendedEuclidean;2SR;7|)
           (LETT |s2|
                 (|EUCDOM-;unitNormalizeIdealElt|
                     (VECTOR (|spadConstant| $ 19)
                             (|spadConstant| $ 30) |y|)
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
                                       (|getShellEntry| $ 29))
                                      (|getShellEntry| $ 31))
                                     (SPADCALL (QVELT |s1| 1)
                                      (SPADCALL (QCAR |qr|)
                                       (QVELT |s2| 1)
                                       (|getShellEntry| $ 29))
                                      (|getShellEntry| $ 31))
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
                                     (|getShellEntry| $ 32)))
                               (SEQ (LETT |qr|
                                     (SPADCALL (QVELT |s1| 0) |y|
                                      (|getShellEntry| $ 16))
                                     |EUCDOM-;extendedEuclidean;2SR;7|)
                                    (QSETVELT |s1| 0 (QCDR |qr|))
                                    (QSETVELT |s1| 1
                                     (SPADCALL (QVELT |s1| 1)
                                      (SPADCALL (QCAR |qr|) |x|
                                       (|getShellEntry| $ 29))
                                      (|getShellEntry| $ 33)))
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
              (SEQ (LETT |s| (SPADCALL |x| |y| (|getShellEntry| $ 36))
                         |EUCDOM-;extendedEuclidean;3SU;8|)
                   (LETT |w|
                         (SPADCALL |z| (QVELT |s| 2)
                             (|getShellEntry| $ 37))
                         |EUCDOM-;extendedEuclidean;3SU;8|)
                   (EXIT (COND
                           ((QEQCAR |w| 1) (CONS 1 "failed"))
                           ((SPADCALL |y| (|getShellEntry| $ 8))
                            (CONS 0
                                  (CONS (SPADCALL (QVELT |s| 0)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 29))
                                        (SPADCALL (QVELT |s| 1)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 29)))))
                           ('T
                            (SEQ (LETT |qr|
                                       (SPADCALL
                                        (SPADCALL (QVELT |s| 0)
                                         (QCDR |w|)
                                         (|getShellEntry| $ 29))
                                        |y| (|getShellEntry| $ 16))
                                       |EUCDOM-;extendedEuclidean;3SU;8|)
                                 (EXIT (CONS 0
                                        (CONS (QCDR |qr|)
                                         (SPADCALL
                                          (SPADCALL (QVELT |s| 1)
                                           (QCDR |w|)
                                           (|getShellEntry| $ 29))
                                          (SPADCALL (QCAR |qr|) |x|
                                           (|getShellEntry| $ 29))
                                          (|getShellEntry| $ 33)))))))))))))))) 

(DEFUN |EUCDOM-;principalIdeal;LR;9| (|l| $)
  (PROG (|uca| |v| |u| #0=#:G1517 |vv| #1=#:G1518)
    (RETURN
      (SEQ (COND
             ((SPADCALL |l| NIL (|getShellEntry| $ 42))
              (|error| "empty list passed to principalIdeal"))
             ((SPADCALL (CDR |l|) NIL (|getShellEntry| $ 42))
              (SEQ (LETT |uca|
                         (SPADCALL (|SPADfirst| |l|)
                             (|getShellEntry| $ 27))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (LIST (QVELT |uca| 0)) (QVELT |uca| 1)))))
             ((SPADCALL (CDR (CDR |l|)) NIL (|getShellEntry| $ 42))
              (SEQ (LETT |u|
                         (SPADCALL (|SPADfirst| |l|)
                             (SPADCALL |l| (|getShellEntry| $ 45))
                             (|getShellEntry| $ 36))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (EXIT (CONS (LIST (QVELT |u| 0) (QVELT |u| 1))
                               (QVELT |u| 2)))))
             ('T
              (SEQ (LETT |v|
                         (SPADCALL (CDR |l|) (|getShellEntry| $ 48))
                         |EUCDOM-;principalIdeal;LR;9|)
                   (LETT |u|
                         (SPADCALL (|SPADfirst| |l|) (QCDR |v|)
                             (|getShellEntry| $ 36))
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
                                        (LETT #0#
                                         (CONS
                                          (SPADCALL (QVELT |u| 1) |vv|
                                           (|getShellEntry| $ 29))
                                          #0#)
                                         |EUCDOM-;principalIdeal;LR;9|)
                                        (LETT #1# (CDR #1#)
                                         |EUCDOM-;principalIdeal;LR;9|)
                                        (GO G190) G191
                                        (EXIT (NREVERSE0 #0#)))))
                               (QVELT |u| 2)))))))))) 

(DEFUN |EUCDOM-;expressIdealMember;LSU;10| (|l| |z| $)
  (PROG (#0=#:G1519 #1=#:G1520 |pid| |q| #2=#:G1521 |v| #3=#:G1522)
    (RETURN
      (SEQ (COND
             ((SPADCALL |z| (|spadConstant| $ 19)
                  (|getShellEntry| $ 51))
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
                           (LETT #0# (CONS (|spadConstant| $ 19) #0#)
                                 |EUCDOM-;expressIdealMember;LSU;10|)
                           (LETT #1# (CDR #1#)
                                 |EUCDOM-;expressIdealMember;LSU;10|)
                           (GO G190) G191 (EXIT (NREVERSE0 #0#))))))
             ('T
              (SEQ (LETT |pid| (SPADCALL |l| (|getShellEntry| $ 48))
                         |EUCDOM-;expressIdealMember;LSU;10|)
                   (LETT |q|
                         (SPADCALL |z| (QCDR |pid|)
                             (|getShellEntry| $ 37))
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
                                     (LETT #2#
                                      (CONS
                                       (SPADCALL (QCDR |q|) |v|
                                        (|getShellEntry| $ 29))
                                       #2#)
                                      |EUCDOM-;expressIdealMember;LSU;10|)
                                     (LETT #3# (CDR #3#)
                                      |EUCDOM-;expressIdealMember;LSU;10|)
                                     (GO G190) G191
                                     (EXIT (NREVERSE0 #2#))))))))))))))) 

(DEFUN |EUCDOM-;multiEuclidean;LSU;11| (|l| |z| $)
  (PROG (|n| |l1| |l2| #0=#:G1396 #1=#:G1523 #2=#:G1504 #3=#:G1502
             #4=#:G1503 #5=#:G1397 #6=#:G1524 #7=#:G1507 #8=#:G1505
             #9=#:G1506 |u| |v1| |v2|)
    (RETURN
      (SEQ (LETT |n| (LENGTH |l|) |EUCDOM-;multiEuclidean;LSU;11|)
           (EXIT (COND
                   ((ZEROP |n|)
                    (|error| "empty list passed to multiEuclidean"))
                   ((EQL |n| 1) (CONS 0 (LIST |z|)))
                   ('T
                    (SEQ (LETT |l1|
                               (SPADCALL |l| (|getShellEntry| $ 58))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (LETT |l2|
                               (SPADCALL |l1| (QUOTIENT2 |n| 2)
                                   (|getShellEntry| $ 61))
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
                                               (|getShellEntry| $ 29))
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
                                       ('T (|spadConstant| $ 30))))
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
                                               (|getShellEntry| $ 29))
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
                                       ('T (|spadConstant| $ 30))))
                                   |z| (|getShellEntry| $ 62))
                               |EUCDOM-;multiEuclidean;LSU;11|)
                         (EXIT (COND
                                 ((QEQCAR |u| 1) (CONS 1 "failed"))
                                 ('T
                                  (SEQ (LETT |v1|
                                        (SPADCALL |l1|
                                         (QCDR (QCDR |u|))
                                         (|getShellEntry| $ 63))
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
                                              (|getShellEntry| $ 63))
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
                                                   64)))))))))))))))))))))) 

(DEFUN |EuclideanDomain&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|EuclideanDomain&| |dv$1|)) ($ (|newShell| 66))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

(MAKEPROP '|EuclideanDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|Boolean|)
             (0 . |zero?|) (5 . |false|) (9 . |true|)
             (|NonNegativeInteger|) (13 . |euclideanSize|) (18 . <)
             |EUCDOM-;sizeLess?;2SB;1|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (24 . |divide|) |EUCDOM-;quo;3S;2| |EUCDOM-;rem;3S;3|
             (30 . |Zero|) (|Union| $ '"failed") |EUCDOM-;exquo;2SU;4|
             (34 . |unitCanonical|) (39 . |not|) (44 . |rem|)
             |EUCDOM-;gcd;3S;5|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (50 . |unitNormal|) (55 . |one?|) (60 . *) (66 . |One|)
             (70 . -) (76 . |sizeLess?|) (82 . +)
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             |EUCDOM-;extendedEuclidean;2SR;7|
             (88 . |extendedEuclidean|) (94 . |exquo|)
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 38 '"failed") |EUCDOM-;extendedEuclidean;3SU;8|
             (|List| 6) (100 . =) (106 . |rest|) (111 . |first|)
             (116 . |second|) (|List| $)
             (|Record| (|:| |coef| 46) (|:| |generator| $))
             (121 . |principalIdeal|) (126 . |cons|)
             |EUCDOM-;principalIdeal;LR;9| (132 . =)
             (|Union| 46 '"failed") |EUCDOM-;expressIdealMember;LSU;10|
             (138 . |#|) (143 . |zero?|) (148 . |One|) (152 . =)
             (158 . |copy|) (163 . |quo|) (|Integer|) (169 . |split!|)
             (175 . |extendedEuclidean|) (182 . |multiEuclidean|)
             (188 . |concat|) |EUCDOM-;multiEuclidean;LSU;11|)
          '#(|sizeLess?| 194 |rem| 200 |quo| 206 |principalIdeal| 212
             |multiEuclidean| 217 |gcd| 223 |extendedEuclidean| 229
             |exquo| 242 |expressIdealMember| 248)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 65
                                '(1 6 7 0 8 0 7 0 9 0 7 0 10 1 6 11 0
                                  12 2 11 7 0 0 13 2 6 15 0 0 16 0 6 0
                                  19 1 6 0 0 22 1 7 0 0 23 2 6 0 0 0 24
                                  1 6 26 0 27 1 6 7 0 28 2 6 0 0 0 29 0
                                  6 0 30 2 6 0 0 0 31 2 6 7 0 0 32 2 6
                                  0 0 0 33 2 6 34 0 0 36 2 6 20 0 0 37
                                  2 41 7 0 0 42 1 41 0 0 43 1 41 6 0 44
                                  1 41 6 0 45 1 6 47 46 48 2 41 0 6 0
                                  49 2 6 7 0 0 51 1 41 11 0 54 1 11 7 0
                                  55 0 11 0 56 2 11 7 0 0 57 1 41 0 0
                                  58 2 11 0 0 0 59 2 41 0 0 60 61 3 6
                                  39 0 0 0 62 2 6 52 46 0 63 2 41 0 0 0
                                  64 2 0 7 0 0 14 2 0 0 0 0 18 2 0 0 0
                                  0 17 1 0 47 46 50 2 0 52 46 0 65 2 0
                                  0 0 0 25 3 0 39 0 0 0 40 2 0 34 0 0
                                  35 2 0 20 0 0 21 2 0 52 46 0 53)))))
          '|lookupComplete|)) 
