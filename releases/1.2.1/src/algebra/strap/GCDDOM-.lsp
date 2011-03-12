
(/VERSIONCHECK 2) 

(DEFUN |GCDDOM-;lcm;3S;1| (|x| |y| $)
  (PROG (LCM)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |y| (|spadConstant| $ 7)
                      (|getShellEntry| $ 9))
                  (SPADCALL |x| (|spadConstant| $ 7)
                      (|getShellEntry| $ 9)))
              (|spadConstant| $ 7))
             ('T
              (SEQ (LETT LCM
                         (SPADCALL |y|
                             (SPADCALL |x| |y| (|getShellEntry| $ 10))
                             (|getShellEntry| $ 12))
                         |GCDDOM-;lcm;3S;1|)
                   (EXIT (COND
                           ((QEQCAR LCM 0)
                            (SPADCALL |x| (QCDR LCM)
                                (|getShellEntry| $ 13)))
                           ('T (|error| "bad gcd in lcm computation"))))))))))) 

(DEFUN |GCDDOM-;lcm;LS;2| (|l| $)
  (SPADCALL (ELT $ 15) |l| (|spadConstant| $ 16) (|spadConstant| $ 7)
      (|getShellEntry| $ 19))) 

(DEFUN |GCDDOM-;gcd;LS;3| (|l| $)
  (SPADCALL (ELT $ 10) |l| (|spadConstant| $ 7) (|spadConstant| $ 16)
      (|getShellEntry| $ 19))) 

(DEFUN |GCDDOM-;gcdPolynomial;3Sup;4| (|p1| |p2| $)
  (PROG (|e2| |e1| |c1| |p| |c2| #0=#:G1415)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p1| (|getShellEntry| $ 24))
              (SPADCALL |p2| (|getShellEntry| $ 25)))
             ((SPADCALL |p2| (|getShellEntry| $ 24))
              (SPADCALL |p1| (|getShellEntry| $ 25)))
             ('T
              (SEQ (LETT |c1| (SPADCALL |p1| (|getShellEntry| $ 26))
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (LETT |c2| (SPADCALL |p2| (|getShellEntry| $ 26))
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (LETT |p1|
                         (PROG2 (LETT #0#
                                      (SPADCALL |p1| |c1|
                                       (|getShellEntry| $ 27))
                                      |GCDDOM-;gcdPolynomial;3Sup;4|)
                                (QCDR #0#)
                           (|check-union| (QEQCAR #0# 0)
                               (|SparseUnivariatePolynomial|
                                   (|getShellEntry| $ 6))
                               #0#))
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (LETT |p2|
                         (PROG2 (LETT #0#
                                      (SPADCALL |p2| |c2|
                                       (|getShellEntry| $ 27))
                                      |GCDDOM-;gcdPolynomial;3Sup;4|)
                                (QCDR #0#)
                           (|check-union| (QEQCAR #0# 0)
                               (|SparseUnivariatePolynomial|
                                   (|getShellEntry| $ 6))
                               #0#))
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (SEQ (LETT |e1|
                              (SPADCALL |p1| (|getShellEntry| $ 29))
                              |GCDDOM-;gcdPolynomial;3Sup;4|)
                        (EXIT (COND
                                ((< 0 |e1|)
                                 (LETT |p1|
                                       (PROG2
                                        (LETT #0#
                                         (SPADCALL |p1|
                                          (SPADCALL
                                           (|spadConstant| $ 16) |e1|
                                           (|getShellEntry| $ 32))
                                          (|getShellEntry| $ 33))
                                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                                        (QCDR #0#)
                                         (|check-union| (QEQCAR #0# 0)
                                          (|SparseUnivariatePolynomial|
                                           (|getShellEntry| $ 6))
                                          #0#))
                                       |GCDDOM-;gcdPolynomial;3Sup;4|)))))
                   (SEQ (LETT |e2|
                              (SPADCALL |p2| (|getShellEntry| $ 29))
                              |GCDDOM-;gcdPolynomial;3Sup;4|)
                        (EXIT (COND
                                ((< 0 |e2|)
                                 (LETT |p2|
                                       (PROG2
                                        (LETT #0#
                                         (SPADCALL |p2|
                                          (SPADCALL
                                           (|spadConstant| $ 16) |e2|
                                           (|getShellEntry| $ 32))
                                          (|getShellEntry| $ 33))
                                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                                        (QCDR #0#)
                                         (|check-union| (QEQCAR #0# 0)
                                          (|SparseUnivariatePolynomial|
                                           (|getShellEntry| $ 6))
                                          #0#))
                                       |GCDDOM-;gcdPolynomial;3Sup;4|)))))
                   (LETT |e1| (MIN |e1| |e2|)
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (LETT |c1|
                         (SPADCALL |c1| |c2| (|getShellEntry| $ 10))
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (LETT |p1|
                         (COND
                           ((OR (EQL (SPADCALL |p1|
                                      (|getShellEntry| $ 34))
                                     0)
                                (EQL (SPADCALL |p2|
                                      (|getShellEntry| $ 34))
                                     0))
                            (SPADCALL |c1| 0 (|getShellEntry| $ 32)))
                           ('T
                            (SEQ (LETT |p|
                                       (SPADCALL |p1| |p2|
                                        (|getShellEntry| $ 35))
                                       |GCDDOM-;gcdPolynomial;3Sup;4|)
                                 (EXIT (COND
                                         ((EQL
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 34))
                                           0)
                                          (SPADCALL |c1| 0
                                           (|getShellEntry| $ 32)))
                                         ('T
                                          (SEQ
                                           (LETT |c2|
                                            (SPADCALL
                                             (SPADCALL |p1|
                                              (|getShellEntry| $ 36))
                                             (SPADCALL |p2|
                                              (|getShellEntry| $ 36))
                                             (|getShellEntry| $ 10))
                                            |GCDDOM-;gcdPolynomial;3Sup;4|)
                                           (EXIT
                                            (SPADCALL
                                             (SPADCALL |c1|
                                              (SPADCALL
                                               (PROG2
                                                (LETT #0#
                                                 (SPADCALL
                                                  (SPADCALL |c2| |p|
                                                   (|getShellEntry| $
                                                    37))
                                                  (SPADCALL |p|
                                                   (|getShellEntry| $
                                                    36))
                                                  (|getShellEntry| $
                                                   27))
                                                 |GCDDOM-;gcdPolynomial;3Sup;4|)
                                                (QCDR #0#)
                                                 (|check-union|
                                                  (QEQCAR #0# 0)
                                                  (|SparseUnivariatePolynomial|
                                                   (|getShellEntry| $
                                                    6))
                                                  #0#))
                                               (|getShellEntry| $ 38))
                                              (|getShellEntry| $ 37))
                                             (|getShellEntry| $ 25))))))))))
                         |GCDDOM-;gcdPolynomial;3Sup;4|)
                   (EXIT (COND
                           ((ZEROP |e1|) |p1|)
                           ('T
                            (SPADCALL
                                (SPADCALL (|spadConstant| $ 16) |e1|
                                    (|getShellEntry| $ 32))
                                |p1| (|getShellEntry| $ 39)))))))))))) 

(DEFUN |GcdDomain&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|GcdDomain&|))
        (LETT |dv$| (LIST '|GcdDomain&| |dv$1|) . #0#)
        (LETT $ (|newShell| 42) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|GcdDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . =) (10 . |gcd|) (|Union| $ '"failed")
             (16 . |exquo|) (22 . *) |GCDDOM-;lcm;3S;1| (28 . |lcm|)
             (34 . |One|) (|Mapping| 6 6 6) (|List| 6) (38 . |reduce|)
             (|List| $) |GCDDOM-;lcm;LS;2| |GCDDOM-;gcd;LS;3|
             (|SparseUnivariatePolynomial| 6) (46 . |zero?|)
             (51 . |unitCanonical|) (56 . |content|) (61 . |exquo|)
             (|NonNegativeInteger|) (67 . |minimumDegree|)
             (72 . |Zero|) (76 . |One|) (80 . |monomial|)
             (86 . |exquo|) (92 . |degree|) (97 . |subResultantGcd|)
             (103 . |leadingCoefficient|) (108 . *)
             (114 . |primitivePart|) (119 . *)
             (|SparseUnivariatePolynomial| $)
             |GCDDOM-;gcdPolynomial;3Sup;4|)
          '#(|lcm| 125 |gcdPolynomial| 136 |gcd| 142) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 41
                                '(0 6 0 7 2 6 8 0 0 9 2 6 0 0 0 10 2 6
                                  11 0 0 12 2 6 0 0 0 13 2 6 0 0 0 15 0
                                  6 0 16 4 18 6 17 0 6 6 19 1 23 8 0 24
                                  1 23 0 0 25 1 23 6 0 26 2 23 11 0 6
                                  27 1 23 28 0 29 0 23 0 30 0 23 0 31 2
                                  23 0 6 28 32 2 23 11 0 0 33 1 23 28 0
                                  34 2 23 0 0 0 35 1 23 6 0 36 2 23 0 6
                                  0 37 1 23 0 0 38 2 23 0 0 0 39 1 0 0
                                  20 21 2 0 0 0 0 14 2 0 40 40 40 41 1
                                  0 0 20 22)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|GcdDomain&| '|isFunctor|
             '(((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ 41))
               ((|lcm| ($ (|List| $))) T (ELT $ 21))
               ((|lcm| ($ $ $)) T (ELT $ 14))
               ((|gcd| ($ (|List| $))) T (ELT $ 22))
               ((|gcd| ($ $ $)) T (ELT $ NIL)))
             (|addModemap| '|GcdDomain&| '(|GcdDomain&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |gcdPolynomial|
                           ((|SparseUnivariatePolynomial| |#1|)
                            (|SparseUnivariatePolynomial| |#1|)
                            (|SparseUnivariatePolynomial| |#1|)))
                       (SIGNATURE |lcm| (|#1| (|List| |#1|)))
                       (SIGNATURE |lcm| (|#1| |#1| |#1|))
                       (SIGNATURE |gcd| (|#1| (|List| |#1|)))
                       (SIGNATURE |gcd| (|#1| |#1| |#1|)))
                   (|GcdDomain|))
                 T '|GcdDomain&|
                 (|put| '|GcdDomain&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |gcdPolynomial|
                                     ((|SparseUnivariatePolynomial|
                                       |#1|)
                                      (|SparseUnivariatePolynomial|
                                       |#1|)
                                      (|SparseUnivariatePolynomial|
                                       |#1|)))
                                 (SIGNATURE |lcm| (|#1| (|List| |#1|)))
                                 (SIGNATURE |lcm| (|#1| |#1| |#1|))
                                 (SIGNATURE |gcd| (|#1| (|List| |#1|)))
                                 (SIGNATURE |gcd| (|#1| |#1| |#1|)))
                             (|GcdDomain|))
                        |$CategoryFrame|)))) 