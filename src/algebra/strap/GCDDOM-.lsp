
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |GCDDOM-;lcm;3S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |GCDDOM-;lcm;LS;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |GCDDOM-;gcd;LS;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |GCDDOM-;gcdPolynomial;3Sup;4|)) 

(DEFUN |GCDDOM-;lcm;3S;1| (|x| |y| $)
  (COND
    ((OR (SPADCALL |y| (|spadConstant| $ 7) (|shellEntry| $ 9))
         (SPADCALL |x| (|spadConstant| $ 7) (|shellEntry| $ 9)))
     (|spadConstant| $ 7))
    (T (LET ((LCM (SPADCALL |y| (SPADCALL |x| |y| (|shellEntry| $ 10))
                      (|shellEntry| $ 12))))
         (COND
           ((ZEROP (CAR LCM))
            (SPADCALL |x| (CDR LCM) (|shellEntry| $ 13)))
           (T (|error| "bad gcd in lcm computation"))))))) 

(DEFUN |GCDDOM-;lcm;LS;2| (|l| $)
  (SPADCALL (ELT $ 15) |l| (|spadConstant| $ 16) (|spadConstant| $ 7)
      (|shellEntry| $ 19))) 

(DEFUN |GCDDOM-;gcd;LS;3| (|l| $)
  (SPADCALL (ELT $ 10) |l| (|spadConstant| $ 7) (|spadConstant| $ 16)
      (|shellEntry| $ 19))) 

(DEFUN |GCDDOM-;gcdPolynomial;3Sup;4| (|p1| |p2| $)
  (PROG (|e1| |e2| |p|)
    (RETURN
      (COND
        ((SPADCALL |p1| (|shellEntry| $ 24))
         (SPADCALL |p2| (|shellEntry| $ 25)))
        ((SPADCALL |p2| (|shellEntry| $ 24))
         (SPADCALL |p1| (|shellEntry| $ 25)))
        (T (LET ((|c1| (SPADCALL |p1| (|shellEntry| $ 26)))
                 (|c2| (SPADCALL |p2| (|shellEntry| $ 26))))
             (SEQ (SETQ |p1|
                        (LET ((#0=#:G1393
                                  (SPADCALL |p1| |c1|
                                      (|shellEntry| $ 27))))
                          (|check-union| (ZEROP (CAR #0#))
                              (|SparseUnivariatePolynomial|
                                  (SVREF $ 6))
                              #0#)
                          (CDR #0#)))
                  (SETQ |p2|
                        (LET ((#0# (SPADCALL |p2| |c2|
                                    (|shellEntry| $ 27))))
                          (|check-union| (ZEROP (CAR #0#))
                              (|SparseUnivariatePolynomial|
                                  (SVREF $ 6))
                              #0#)
                          (CDR #0#)))
                  (SEQ (LETT |e1| (SPADCALL |p1| (|shellEntry| $ 29))
                             |GCDDOM-;gcdPolynomial;3Sup;4|)
                       (EXIT (COND
                               ((PLUSP |e1|)
                                (SETQ |p1|
                                      (LET
                                       ((#0#
                                         (SPADCALL |p1|
                                          (SPADCALL
                                           (|spadConstant| $ 16) |e1|
                                           (|shellEntry| $ 34))
                                          (|shellEntry| $ 35))))
                                        (|check-union|
                                         (ZEROP (CAR #0#))
                                         (|SparseUnivariatePolynomial|
                                          (SVREF $ 6))
                                         #0#)
                                        (CDR #0#)))))))
                  (SEQ (LETT |e2| (SPADCALL |p2| (|shellEntry| $ 29))
                             |GCDDOM-;gcdPolynomial;3Sup;4|)
                       (EXIT (COND
                               ((PLUSP |e2|)
                                (SETQ |p2|
                                      (LET
                                       ((#0#
                                         (SPADCALL |p2|
                                          (SPADCALL
                                           (|spadConstant| $ 16) |e2|
                                           (|shellEntry| $ 34))
                                          (|shellEntry| $ 35))))
                                        (|check-union|
                                         (ZEROP (CAR #0#))
                                         (|SparseUnivariatePolynomial|
                                          (SVREF $ 6))
                                         #0#)
                                        (CDR #0#)))))))
                  (SETQ |e1| (MIN |e1| |e2|))
                  (SETQ |c1| (SPADCALL |c1| |c2| (|shellEntry| $ 10)))
                  (SETQ |p1|
                        (COND
                          ((OR (ZEROP (SPADCALL |p1|
                                       (|shellEntry| $ 37)))
                               (ZEROP (SPADCALL |p2|
                                       (|shellEntry| $ 37))))
                           (SPADCALL |c1| 0 (|shellEntry| $ 34)))
                          (T (SEQ (LETT |p|
                                        (SPADCALL |p1| |p2|
                                         (|shellEntry| $ 39))
                                        |GCDDOM-;gcdPolynomial;3Sup;4|)
                                  (EXIT (COND
                                          ((ZEROP
                                            (SPADCALL |p|
                                             (|shellEntry| $ 37)))
                                           (SPADCALL |c1| 0
                                            (|shellEntry| $ 34)))
                                          (T
                                           (SEQ
                                            (SETQ |c2|
                                             (SPADCALL
                                              (SPADCALL |p1|
                                               (|shellEntry| $ 40))
                                              (SPADCALL |p2|
                                               (|shellEntry| $ 40))
                                              (|shellEntry| $ 10)))
                                            (EXIT
                                             (SPADCALL
                                              (SPADCALL |c1|
                                               (SPADCALL
                                                (LET
                                                 ((#0#
                                                   (SPADCALL
                                                    (SPADCALL |c2| |p|
                                                     (|shellEntry| $
                                                      41))
                                                    (SPADCALL |p|
                                                     (|shellEntry| $
                                                      40))
                                                    (|shellEntry| $ 27))))
                                                  (|check-union|
                                                   (ZEROP (CAR #0#))
                                                   (|SparseUnivariatePolynomial|
                                                    (SVREF $ 6))
                                                   #0#)
                                                  (CDR #0#))
                                                (|shellEntry| $ 42))
                                               (|shellEntry| $ 41))
                                              (|shellEntry| $ 25)))))))))))
                  (EXIT (COND
                          ((ZEROP |e1|) |p1|)
                          (T (SPADCALL
                                 (SPADCALL (|spadConstant| $ 16) |e1|
                                     (|shellEntry| $ 34))
                                 |p1| (|shellEntry| $ 44)))))))))))) 

(DEFUN |GcdDomain&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|GcdDomain&| |dv$1|)) ($ (|newShell| 47))
         (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    $)) 

(MAKEPROP '|GcdDomain&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             (|Boolean|) (4 . =) (10 . |gcd|) (|Union| $ '"failed")
             (16 . |exquo|) (22 . *) |GCDDOM-;lcm;3S;1| (28 . |lcm|)
             (34 . |One|) (|Mapping| 6 6 6) (|List| 6) (38 . |reduce|)
             (|List| $) |GCDDOM-;lcm;LS;2| |GCDDOM-;gcd;LS;3|
             (|SparseUnivariatePolynomial| 6) (46 . |zero?|)
             (51 . |unitCanonical|) (56 . |content|) (61 . |exquo|)
             (|NonNegativeInteger|) (67 . |minimumDegree|)
             (72 . |Zero|) (76 . |Zero|) (80 . >) (86 . |One|)
             (90 . |monomial|) (96 . |exquo|) (102 . |min|)
             (108 . |degree|) (113 . =) (119 . |subResultantGcd|)
             (125 . |leadingCoefficient|) (130 . *)
             (136 . |primitivePart|) (141 . |zero?|) (146 . *)
             (|SparseUnivariatePolynomial| $)
             |GCDDOM-;gcdPolynomial;3Sup;4|)
          '#(|lcm| 152 |gcdPolynomial| 163 |gcd| 169) 'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 46
                                '(0 6 0 7 2 6 8 0 0 9 2 6 0 0 0 10 2 6
                                  11 0 0 12 2 6 0 0 0 13 2 6 0 0 0 15 0
                                  6 0 16 4 18 6 17 0 6 6 19 1 23 8 0 24
                                  1 23 0 0 25 1 23 6 0 26 2 23 11 0 6
                                  27 1 23 28 0 29 0 23 0 30 0 28 0 31 2
                                  28 8 0 0 32 0 23 0 33 2 23 0 6 28 34
                                  2 23 11 0 0 35 2 28 0 0 0 36 1 23 28
                                  0 37 2 28 8 0 0 38 2 23 0 0 0 39 1 23
                                  6 0 40 2 23 0 6 0 41 1 23 0 0 42 1 28
                                  8 0 43 2 23 0 0 0 44 1 0 0 20 21 2 0
                                  0 0 0 14 2 0 45 45 45 46 1 0 0 20 22)))))
          '|lookupComplete|)) 
