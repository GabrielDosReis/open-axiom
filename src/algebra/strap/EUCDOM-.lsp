
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
    (T (< (SPADCALL |x| (|getShellEntry| $ 12))
          (SPADCALL |y| (|getShellEntry| $ 12)))))) 

(DEFUN |EUCDOM-;quo;3S;2| (|x| |y| $)
  (CAR (SPADCALL |x| |y| (|getShellEntry| $ 16)))) 

(DEFUN |EUCDOM-;rem;3S;3| (|x| |y| $)
  (CDR (SPADCALL |x| |y| (|getShellEntry| $ 16)))) 

(DEFUN |EUCDOM-;exquo;2SU;4| (|x| |y| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 8))
     (CONS 0 (|spadConstant| $ 19)))
    ((SPADCALL |y| (|getShellEntry| $ 8)) (CONS 1 "failed"))
    (T (LET ((|qr| (SPADCALL |x| |y| (|getShellEntry| $ 16))))
         (COND
           ((SPADCALL (CDR |qr|) (|getShellEntry| $ 8))
            (CONS 0 (CAR |qr|)))
           (T (CONS 1 "failed"))))))) 

(DEFUN |EUCDOM-;gcd;3S;5| (|x| |y| $)
  (PROG (|#G13| |#G14|)
    (RETURN
      (SEQ (SETQ |x| (SPADCALL |x| (|getShellEntry| $ 22)))
           (SETQ |y| (SPADCALL |y| (|getShellEntry| $ 22)))
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |y| (|getShellEntry| $ 8))))
                (RETURN NIL))
               (T (SEQ (LETT |#G13| |y| |EUCDOM-;gcd;3S;5|)
                       (LETT |#G14|
                             (SPADCALL |x| |y| (|getShellEntry| $ 24))
                             |EUCDOM-;gcd;3S;5|)
                       (SETQ |x| |#G13|) (SETQ |y| |#G14|)
                       (EXIT (SETQ |y|
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 22))))))))
           (EXIT |x|))))) 

(DEFUN |EUCDOM-;unitNormalizeIdealElt| (|s| $)
  (LET* ((|#G16| (SPADCALL (SVREF |s| 2) (|getShellEntry| $ 27)))
         (|u| (SVREF |#G16| 0)) (|c| (SVREF |#G16| 1))
         (|a| (SVREF |#G16| 2)))
    (SEQ |#G16|
         (EXIT (COND
                 ((SPADCALL |a| (|getShellEntry| $ 28)) |s|)
                 (T (VECTOR (SPADCALL |a| (SVREF |s| 0)
                                (|getShellEntry| $ 29))
                            (SPADCALL |a| (SVREF |s| 1)
                                (|getShellEntry| $ 29))
                            |c|))))))) 

(DEFUN |EUCDOM-;extendedEuclidean;2SR;7| (|x| |y| $)
  (PROG (|qr| |s3|)
    (RETURN
      (LET ((|s1| (|EUCDOM-;unitNormalizeIdealElt|
                      (VECTOR (|spadConstant| $ 30)
                              (|spadConstant| $ 19) |x|)
                      $))
            (|s2| (|EUCDOM-;unitNormalizeIdealElt|
                      (VECTOR (|spadConstant| $ 19)
                              (|spadConstant| $ 30) |y|)
                      $)))
        (COND
          ((SPADCALL |y| (|getShellEntry| $ 8)) |s1|)
          ((SPADCALL |x| (|getShellEntry| $ 8)) |s2|)
          (T (SEQ (LOOP
                    (COND
                      ((NOT (NOT (SPADCALL (SVREF |s2| 2)
                                     (|getShellEntry| $ 8))))
                       (RETURN NIL))
                      (T (SEQ (LETT |qr|
                                    (SPADCALL (SVREF |s1| 2)
                                     (SVREF |s2| 2)
                                     (|getShellEntry| $ 16))
                                    |EUCDOM-;extendedEuclidean;2SR;7|)
                              (LETT |s3|
                                    (VECTOR
                                     (SPADCALL (SVREF |s1| 0)
                                      (SPADCALL (CAR |qr|)
                                       (SVREF |s2| 0)
                                       (|getShellEntry| $ 29))
                                      (|getShellEntry| $ 31))
                                     (SPADCALL (SVREF |s1| 1)
                                      (SPADCALL (CAR |qr|)
                                       (SVREF |s2| 1)
                                       (|getShellEntry| $ 29))
                                      (|getShellEntry| $ 31))
                                     (CDR |qr|))
                                    |EUCDOM-;extendedEuclidean;2SR;7|)
                              (SETQ |s1| |s2|)
                              (EXIT (SETQ |s2|
                                     (|EUCDOM-;unitNormalizeIdealElt|
                                      |s3| $)))))))
                  (COND
                    ((AND (NOT (SPADCALL (SVREF |s1| 0)
                                   (|getShellEntry| $ 8)))
                          (NOT (SPADCALL (SVREF |s1| 0) |y|
                                   (|getShellEntry| $ 32))))
                     (SEQ (SETQ |qr|
                                (SPADCALL (SVREF |s1| 0) |y|
                                    (|getShellEntry| $ 16)))
                          (SETF (SVREF |s1| 0) (CDR |qr|))
                          (SETF (SVREF |s1| 1)
                                (SPADCALL (SVREF |s1| 1)
                                    (SPADCALL (CAR |qr|) |x|
                                     (|getShellEntry| $ 29))
                                    (|getShellEntry| $ 33)))
                          (EXIT (SETQ |s1|
                                      (|EUCDOM-;unitNormalizeIdealElt|
                                       |s1| $))))))
                  (EXIT |s1|)))))))) 

(DEFUN |EUCDOM-;extendedEuclidean;3SU;8| (|x| |y| |z| $)
  (PROG (|qr|)
    (RETURN
      (COND
        ((SPADCALL |z| (|getShellEntry| $ 8))
         (CONS 0 (CONS (|spadConstant| $ 19) (|spadConstant| $ 19))))
        (T (LET* ((|s| (SPADCALL |x| |y| (|getShellEntry| $ 36)))
                  (|w| (SPADCALL |z| (SVREF |s| 2)
                           (|getShellEntry| $ 37))))
             (COND
               ((EQL (CAR |w|) 1) (CONS 1 "failed"))
               ((SPADCALL |y| (|getShellEntry| $ 8))
                (CONS 0
                      (CONS (SPADCALL (SVREF |s| 0) (CDR |w|)
                                (|getShellEntry| $ 29))
                            (SPADCALL (SVREF |s| 1) (CDR |w|)
                                (|getShellEntry| $ 29)))))
               (T (SEQ (LETT |qr|
                             (SPADCALL
                                 (SPADCALL (SVREF |s| 0) (CDR |w|)
                                     (|getShellEntry| $ 29))
                                 |y| (|getShellEntry| $ 16))
                             |EUCDOM-;extendedEuclidean;3SU;8|)
                       (EXIT (CONS 0
                                   (CONS (CDR |qr|)
                                    (SPADCALL
                                     (SPADCALL (SVREF |s| 1) (CDR |w|)
                                      (|getShellEntry| $ 29))
                                     (SPADCALL (CAR |qr|) |x|
                                      (|getShellEntry| $ 29))
                                     (|getShellEntry| $ 33)))))))))))))) 

(DEFUN |EUCDOM-;principalIdeal;LR;9| (|l| $)
  (COND
    ((SPADCALL |l| NIL (|getShellEntry| $ 42))
     (|error| "empty list passed to principalIdeal"))
    ((SPADCALL (CDR |l|) NIL (|getShellEntry| $ 42))
     (LET ((|uca| (SPADCALL (|SPADfirst| |l|) (|getShellEntry| $ 27))))
       (CONS (LIST (SVREF |uca| 0)) (SVREF |uca| 1))))
    ((SPADCALL (CDR (CDR |l|)) NIL (|getShellEntry| $ 42))
     (LET ((|u| (SPADCALL (|SPADfirst| |l|)
                    (SPADCALL |l| (|getShellEntry| $ 45))
                    (|getShellEntry| $ 36))))
       (CONS (LIST (SVREF |u| 0) (SVREF |u| 1)) (SVREF |u| 2))))
    (T (LET* ((|v| (SPADCALL (CDR |l|) (|getShellEntry| $ 48)))
              (|u| (SPADCALL (|SPADfirst| |l|) (CDR |v|)
                       (|getShellEntry| $ 36))))
         (CONS (CONS (SVREF |u| 0)
                     (LET ((#0=#:G1494 (CAR |v|)) (#1=#:G1493 NIL))
                       (LOOP
                         (COND
                           ((ATOM #0#) (RETURN (NREVERSE #1#)))
                           (T (LET ((|vv| (CAR #0#)))
                                (SETQ #1#
                                      (CONS
                                       (SPADCALL (SVREF |u| 1) |vv|
                                        (|getShellEntry| $ 29))
                                       #1#)))))
                         (SETQ #0# (CDR #0#)))))
               (SVREF |u| 2)))))) 

(DEFUN |EUCDOM-;expressIdealMember;LSU;10| (|l| |z| $)
  (COND
    ((SPADCALL |z| (|spadConstant| $ 19) (|getShellEntry| $ 51))
     (CONS 0
           (LET ((#0=#:G1496 |l|) (#1=#:G1495 NIL))
             (LOOP
               (COND
                 ((ATOM #0#) (RETURN (NREVERSE #1#)))
                 (T (LET ((|v| (CAR #0#)))
                      (SETQ #1# (CONS (|spadConstant| $ 19) #1#)))))
               (SETQ #0# (CDR #0#))))))
    (T (LET* ((|pid| (SPADCALL |l| (|getShellEntry| $ 48)))
              (|q| (SPADCALL |z| (CDR |pid|) (|getShellEntry| $ 37))))
         (COND
           ((EQL (CAR |q|) 1) (CONS 1 "failed"))
           (T (CONS 0
                    (LET ((#2=#:G1498 (CAR |pid|)) (#3=#:G1497 NIL))
                      (LOOP
                        (COND
                          ((ATOM #2#) (RETURN (NREVERSE #3#)))
                          (T (LET ((|v| (CAR #2#)))
                               (SETQ #3#
                                     (CONS
                                      (SPADCALL (CDR |q|) |v|
                                       (|getShellEntry| $ 29))
                                      #3#)))))
                        (SETQ #2# (CDR #2#))))))))))) 

(DEFUN |EUCDOM-;multiEuclidean;LSU;11| (|l| |z| $)
  (PROG (|l1| |l2| |u| |v1| |v2|)
    (RETURN
      (LET ((|n| (LIST-LENGTH |l|)))
        (COND
          ((ZEROP |n|) (|error| "empty list passed to multiEuclidean"))
          ((EQL |n| 1) (CONS 0 (LIST |z|)))
          (T (SEQ (LETT |l1| (SPADCALL |l| (|getShellEntry| $ 58))
                        |EUCDOM-;multiEuclidean;LSU;11|)
                  (LETT |l2|
                        (SPADCALL |l1| (TRUNCATE |n| 2)
                            (|getShellEntry| $ 61))
                        |EUCDOM-;multiEuclidean;LSU;11|)
                  (LETT |u|
                        (SPADCALL
                            (LET ((#0=#:G1479 NIL) (#1=#:G1480 T)
                                  (#2=#:G1499 |l1|))
                              (LOOP
                                (COND
                                  ((ATOM #2#)
                                   (RETURN
                                     (COND
                                       (#1# (|spadConstant| $ 30))
                                       (T #0#))))
                                  (T (LET ((#3=#:G1372 (CAR #2#)))
                                       (LET ((#4=#:G1478 #3#))
                                         (COND
                                           (#1# (SETQ #0# #4#))
                                           (T
                                            (SETQ #0#
                                             (SPADCALL #0# #4#
                                              (|getShellEntry| $ 29)))))
                                         (SETQ #1# NIL)))))
                                (SETQ #2# (CDR #2#))))
                            (LET ((#5=#:G1482 NIL) (#6=#:G1483 T)
                                  (#7=#:G1500 |l2|))
                              (LOOP
                                (COND
                                  ((ATOM #7#)
                                   (RETURN
                                     (COND
                                       (#6# (|spadConstant| $ 30))
                                       (T #5#))))
                                  (T (LET ((#8=#:G1373 (CAR #7#)))
                                       (LET ((#9=#:G1481 #8#))
                                         (COND
                                           (#6# (SETQ #5# #9#))
                                           (T
                                            (SETQ #5#
                                             (SPADCALL #5# #9#
                                              (|getShellEntry| $ 29)))))
                                         (SETQ #6# NIL)))))
                                (SETQ #7# (CDR #7#))))
                            |z| (|getShellEntry| $ 62))
                        |EUCDOM-;multiEuclidean;LSU;11|)
                  (EXIT (COND
                          ((EQL (CAR |u|) 1) (CONS 1 "failed"))
                          (T (SEQ (LETT |v1|
                                        (SPADCALL |l1| (CDR (CDR |u|))
                                         (|getShellEntry| $ 63))
                                        |EUCDOM-;multiEuclidean;LSU;11|)
                                  (EXIT (COND
                                          ((EQL (CAR |v1|) 1)
                                           (CONS 1 "failed"))
                                          (T
                                           (SEQ
                                            (LETT |v2|
                                             (SPADCALL |l2|
                                              (CAR (CDR |u|))
                                              (|getShellEntry| $ 63))
                                             |EUCDOM-;multiEuclidean;LSU;11|)
                                            (EXIT
                                             (COND
                                               ((EQL (CAR |v2|) 1)
                                                (CONS 1 "failed"))
                                               (T
                                                (CONS 0
                                                 (SPADCALL (CDR |v1|)
                                                  (CDR |v2|)
                                                  (|getShellEntry| $
                                                   64))))))))))))))))))))) 

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
