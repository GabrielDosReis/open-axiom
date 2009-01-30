
(/VERSIONCHECK 2) 

(DEFUN |FFIELDC-;differentiate;2S;1| (|x| $) (|spadConstant| $ 7)) 

(DEFUN |FFIELDC-;init;S;2| ($) (|spadConstant| $ 7)) 

(DEFUN |FFIELDC-;nextItem;SU;3| (|a| $)
  (COND
    ((SPADCALL
         (LETT |a|
               (SPADCALL (+ (SPADCALL |a| (|getShellEntry| $ 11)) 1)
                   (|getShellEntry| $ 12))
               |FFIELDC-;nextItem;SU;3|)
         (|getShellEntry| $ 14))
     (CONS 1 "failed"))
    ('T (CONS 0 |a|)))) 

(DEFUN |FFIELDC-;order;SOpc;4| (|e| $)
  (SPADCALL (SPADCALL |e| (|getShellEntry| $ 17))
      (|getShellEntry| $ 20))) 

(DEFUN |FFIELDC-;conditionP;MU;5| (|mat| $)
  (PROG (|l|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |mat| (|getShellEntry| $ 25))
                 |FFIELDC-;conditionP;MU;5|)
           (COND
             ((OR (NULL |l|)
                  (SPADCALL (ELT $ 14) (|SPADfirst| |l|)
                      (|getShellEntry| $ 27)))
              (EXIT (CONS 1 "failed"))))
           (EXIT (CONS 0
                       (SPADCALL (ELT $ 28) (|SPADfirst| |l|)
                           (|getShellEntry| $ 30)))))))) 

(DEFUN |FFIELDC-;charthRoot;2S;6| (|x| $)
  (SPADCALL |x|
      (QUOTIENT2 (SPADCALL (|getShellEntry| $ 36))
          (SPADCALL (|getShellEntry| $ 37)))
      (|getShellEntry| $ 38))) 

(DEFUN |FFIELDC-;charthRoot;SU;7| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 28)))) 

(DEFUN |FFIELDC-;createPrimitiveElement;S;8| ($)
  (PROG (|sm1| |start| |i| #0=#:G1443 |e| |found|)
    (RETURN
      (SEQ (LETT |sm1| (- (SPADCALL (|getShellEntry| $ 36)) 1)
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |start|
                 (COND
                   ((SPADCALL (SPADCALL (|getShellEntry| $ 43))
                        (CONS 1 "polynomial") (|getShellEntry| $ 44))
                    (SPADCALL (|getShellEntry| $ 37)))
                   ('T 1))
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |found| 'NIL |FFIELDC-;createPrimitiveElement;S;8|)
           (SEQ (LETT |i| |start|
                      |FFIELDC-;createPrimitiveElement;S;8|)
                G190
                (COND
                  ((NULL (SPADCALL |found| (|getShellEntry| $ 45)))
                   (GO G191)))
                (SEQ (LETT |e|
                           (SPADCALL
                               (PROG1 (LETT #0# |i|
                                       |FFIELDC-;createPrimitiveElement;S;8|)
                                 (|check-subtype| (> #0# 0)
                                     '(|PositiveInteger|) #0#))
                               (|getShellEntry| $ 12))
                           |FFIELDC-;createPrimitiveElement;S;8|)
                     (EXIT (LETT |found|
                                 (EQL (SPADCALL |e|
                                       (|getShellEntry| $ 17))
                                      |sm1|)
                                 |FFIELDC-;createPrimitiveElement;S;8|)))
                (LETT |i| (+ |i| 1)
                      |FFIELDC-;createPrimitiveElement;S;8|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |e|))))) 

(DEFUN |FFIELDC-;primitive?;SB;9| (|a| $)
  (PROG (|explist| |q| |exp| #0=#:G1455 |equalone|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|getShellEntry| $ 14)) 'NIL)
             ('T
              (SEQ (LETT |explist| (SPADCALL (|getShellEntry| $ 49))
                         |FFIELDC-;primitive?;SB;9|)
                   (LETT |q| (- (SPADCALL (|getShellEntry| $ 36)) 1)
                         |FFIELDC-;primitive?;SB;9|)
                   (LETT |equalone| 'NIL |FFIELDC-;primitive?;SB;9|)
                   (SEQ (LETT |exp| NIL |FFIELDC-;primitive?;SB;9|)
                        (LETT #0# |explist| |FFIELDC-;primitive?;SB;9|)
                        G190
                        (COND
                          ((OR (ATOM #0#)
                               (PROGN
                                 (LETT |exp| (CAR #0#)
                                       |FFIELDC-;primitive?;SB;9|)
                                 NIL)
                               (NULL (SPADCALL |equalone|
                                      (|getShellEntry| $ 45))))
                           (GO G191)))
                        (SEQ (EXIT (LETT |equalone|
                                    (SPADCALL
                                     (SPADCALL |a|
                                      (QUOTIENT2 |q| (QCAR |exp|))
                                      (|getShellEntry| $ 50))
                                     (|spadConstant| $ 41)
                                     (|getShellEntry| $ 51))
                                    |FFIELDC-;primitive?;SB;9|)))
                        (LETT #0# (CDR #0#) |FFIELDC-;primitive?;SB;9|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT (SPADCALL |equalone| (|getShellEntry| $ 45)))))))))) 

(DEFUN |FFIELDC-;order;SPi;10| (|e| $)
  (PROG (|lof| |rec| #0=#:G1463 |primeDivisor| |j| #1=#:G1464 |a|
               |goon| |ord|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |e| (|spadConstant| $ 7)
                  (|getShellEntry| $ 51))
              (|error| "order(0) is not defined "))
             ('T
              (SEQ (LETT |ord| (- (SPADCALL (|getShellEntry| $ 36)) 1)
                         |FFIELDC-;order;SPi;10|)
                   (LETT |a| 0 |FFIELDC-;order;SPi;10|)
                   (LETT |lof| (SPADCALL (|getShellEntry| $ 49))
                         |FFIELDC-;order;SPi;10|)
                   (SEQ (LETT |rec| NIL |FFIELDC-;order;SPi;10|)
                        (LETT #0# |lof| |FFIELDC-;order;SPi;10|) G190
                        (COND
                          ((OR (ATOM #0#)
                               (PROGN
                                 (LETT |rec| (CAR #0#)
                                       |FFIELDC-;order;SPi;10|)
                                 NIL))
                           (GO G191)))
                        (SEQ (LETT |a|
                                   (QUOTIENT2 |ord|
                                    (LETT |primeDivisor| (QCAR |rec|)
                                     |FFIELDC-;order;SPi;10|))
                                   |FFIELDC-;order;SPi;10|)
                             (LETT |goon|
                                   (SPADCALL
                                    (SPADCALL |e| |a|
                                     (|getShellEntry| $ 50))
                                    (|spadConstant| $ 41)
                                    (|getShellEntry| $ 51))
                                   |FFIELDC-;order;SPi;10|)
                             (SEQ (LETT |j| 0 |FFIELDC-;order;SPi;10|)
                                  (LETT #1# (- (QCDR |rec|) 2)
                                        |FFIELDC-;order;SPi;10|)
                                  G190
                                  (COND
                                    ((OR (QSGREATERP |j| #1#)
                                      (NULL |goon|))
                                     (GO G191)))
                                  (SEQ (LETT |ord| |a|
                                        |FFIELDC-;order;SPi;10|)
                                       (LETT |a|
                                        (QUOTIENT2 |ord|
                                         |primeDivisor|)
                                        |FFIELDC-;order;SPi;10|)
                                       (EXIT
                                        (LETT |goon|
                                         (SPADCALL
                                          (SPADCALL |e| |a|
                                           (|getShellEntry| $ 50))
                                          (|spadConstant| $ 41)
                                          (|getShellEntry| $ 51))
                                         |FFIELDC-;order;SPi;10|)))
                                  (LETT |j| (QSADD1 |j|)
                                        |FFIELDC-;order;SPi;10|)
                                  (GO G190) G191 (EXIT NIL))
                             (EXIT (COND
                                     (|goon|
                                      (LETT |ord| |a|
                                       |FFIELDC-;order;SPi;10|)))))
                        (LETT #0# (CDR #0#) |FFIELDC-;order;SPi;10|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT |ord|)))))))) 

(DEFUN |FFIELDC-;discreteLog;SNni;11| (|b| $)
  (PROG (|faclist| |gen| |groupord| |f| #0=#:G1484 |fac| |t| #1=#:G1485
            |exp| |exptable| |n| |end| |i| |rho| |found| |disc1| |c|
            |mult| |disclog| |a|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |b| (|getShellEntry| $ 14))
              (|error| "discreteLog: logarithm of zero"))
             ('T
              (SEQ (LETT |faclist| (SPADCALL (|getShellEntry| $ 49))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |a| |b| |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |gen| (SPADCALL (|getShellEntry| $ 54))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (EXIT (COND
                           ((SPADCALL |b| |gen| (|getShellEntry| $ 51))
                            1)
                           ('T
                            (SEQ (LETT |disclog| 0
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |mult| 1
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |groupord|
                                       (-
                                        (SPADCALL
                                         (|getShellEntry| $ 36))
                                        1)
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |exp| |groupord|
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (SEQ (LETT |f| NIL
                                       |FFIELDC-;discreteLog;SNni;11|)
                                      (LETT #0# |faclist|
                                       |FFIELDC-;discreteLog;SNni;11|)
                                      G190
                                      (COND
                                        ((OR (ATOM #0#)
                                          (PROGN
                                            (LETT |f| (CAR #0#)
                                             |FFIELDC-;discreteLog;SNni;11|)
                                            NIL))
                                         (GO G191)))
                                      (SEQ
                                       (LETT |fac| (QCAR |f|)
                                        |FFIELDC-;discreteLog;SNni;11|)
                                       (EXIT
                                        (SEQ
                                         (LETT |t| 0
                                          |FFIELDC-;discreteLog;SNni;11|)
                                         (LETT #1# (- (QCDR |f|) 1)
                                          |FFIELDC-;discreteLog;SNni;11|)
                                         G190
                                         (COND
                                           ((QSGREATERP |t| #1#)
                                            (GO G191)))
                                         (SEQ
                                          (LETT |exp|
                                           (QUOTIENT2 |exp| |fac|)
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |exptable|
                                           (SPADCALL |fac|
                                            (|getShellEntry| $ 56))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |n|
                                           (SPADCALL |exptable|
                                            (|getShellEntry| $ 57))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |c|
                                           (SPADCALL |a| |exp|
                                            (|getShellEntry| $ 50))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |end|
                                           (QUOTIENT2 (- |fac| 1) |n|)
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |found| 'NIL
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |disc1| 0
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (SEQ
                                           (LETT |i| 0
                                            |FFIELDC-;discreteLog;SNni;11|)
                                           G190
                                           (COND
                                             ((OR
                                               (QSGREATERP |i| |end|)
                                               (NULL
                                                (SPADCALL |found|
                                                 (|getShellEntry| $ 45))))
                                              (GO G191)))
                                           (SEQ
                                            (LETT |rho|
                                             (SPADCALL
                                              (SPADCALL |c|
                                               (|getShellEntry| $ 11))
                                              |exptable|
                                              (|getShellEntry| $ 59))
                                             |FFIELDC-;discreteLog;SNni;11|)
                                            (EXIT
                                             (COND
                                               ((QEQCAR |rho| 0)
                                                (SEQ
                                                 (LETT |found| 'T
                                                  |FFIELDC-;discreteLog;SNni;11|)
                                                 (EXIT
                                                  (LETT |disc1|
                                                   (*
                                                    (+ (* |n| |i|)
                                                     (QCDR |rho|))
                                                    |mult|)
                                                   |FFIELDC-;discreteLog;SNni;11|))))
                                               ('T
                                                (LETT |c|
                                                 (SPADCALL |c|
                                                  (SPADCALL |gen|
                                                   (*
                                                    (QUOTIENT2
                                                     |groupord| |fac|)
                                                    (- |n|))
                                                   (|getShellEntry| $
                                                    50))
                                                  (|getShellEntry| $
                                                   60))
                                                 |FFIELDC-;discreteLog;SNni;11|)))))
                                           (LETT |i| (QSADD1 |i|)
                                            |FFIELDC-;discreteLog;SNni;11|)
                                           (GO G190) G191 (EXIT NIL))
                                          (EXIT
                                           (COND
                                             (|found|
                                              (SEQ
                                               (LETT |mult|
                                                (* |mult| |fac|)
                                                |FFIELDC-;discreteLog;SNni;11|)
                                               (LETT |disclog|
                                                (+ |disclog| |disc1|)
                                                |FFIELDC-;discreteLog;SNni;11|)
                                               (EXIT
                                                (LETT |a|
                                                 (SPADCALL |a|
                                                  (SPADCALL |gen|
                                                   (- |disc1|)
                                                   (|getShellEntry| $
                                                    50))
                                                  (|getShellEntry| $
                                                   60))
                                                 |FFIELDC-;discreteLog;SNni;11|))))
                                             ('T
                                              (|error|
                                               "discreteLog: ?? discrete logarithm")))))
                                         (LETT |t| (QSADD1 |t|)
                                          |FFIELDC-;discreteLog;SNni;11|)
                                         (GO G190) G191 (EXIT NIL))))
                                      (LETT #0# (CDR #0#)
                                       |FFIELDC-;discreteLog;SNni;11|)
                                      (GO G190) G191 (EXIT NIL))
                                 (EXIT |disclog|)))))))))))) 

(DEFUN |FFIELDC-;discreteLog;2SU;12| (|logbase| |b| $)
  (PROG (|groupord| |faclist| |f| #0=#:G1503 |fac| |primroot| |t|
            #1=#:G1504 |exp| |rhoHelp| #2=#:G1502 |rho| |disclog|
            |mult| |a|)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |b| (|getShellEntry| $ 14))
                    (SEQ (SPADCALL "discreteLog: logarithm of zero"
                             (|getShellEntry| $ 65))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |logbase| (|getShellEntry| $ 14))
                    (SEQ (SPADCALL
                             "discreteLog: logarithm to base zero"
                             (|getShellEntry| $ 65))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |b| |logbase| (|getShellEntry| $ 51))
                    (CONS 0 1))
                   ('T
                    (COND
                      ((NULL (ZEROP (REMAINDER2
                                     (LETT |groupord|
                                      (SPADCALL |logbase|
                                       (|getShellEntry| $ 17))
                                      |FFIELDC-;discreteLog;2SU;12|)
                                     (SPADCALL |b|
                                      (|getShellEntry| $ 17)))))
                       (SEQ (SPADCALL
                                "discreteLog: second argument not in cyclic group generated by first argument"
                                (|getShellEntry| $ 65))
                            (EXIT (CONS 1 "failed"))))
                      ('T
                       (SEQ (LETT |faclist|
                                  (SPADCALL
                                      (SPADCALL |groupord|
                                       (|getShellEntry| $ 67))
                                      (|getShellEntry| $ 69))
                                  |FFIELDC-;discreteLog;2SU;12|)
                            (LETT |a| |b|
                                  |FFIELDC-;discreteLog;2SU;12|)
                            (LETT |disclog| 0
                                  |FFIELDC-;discreteLog;2SU;12|)
                            (LETT |mult| 1
                                  |FFIELDC-;discreteLog;2SU;12|)
                            (LETT |exp| |groupord|
                                  |FFIELDC-;discreteLog;2SU;12|)
                            (SEQ (LETT |f| NIL
                                       |FFIELDC-;discreteLog;2SU;12|)
                                 (LETT #0# |faclist|
                                       |FFIELDC-;discreteLog;2SU;12|)
                                 G190
                                 (COND
                                   ((OR (ATOM #0#)
                                     (PROGN
                                       (LETT |f| (CAR #0#)
                                        |FFIELDC-;discreteLog;2SU;12|)
                                       NIL))
                                    (GO G191)))
                                 (SEQ (LETT |fac| (QCAR |f|)
                                       |FFIELDC-;discreteLog;2SU;12|)
                                      (LETT |primroot|
                                       (SPADCALL |logbase|
                                        (QUOTIENT2 |groupord| |fac|)
                                        (|getShellEntry| $ 50))
                                       |FFIELDC-;discreteLog;2SU;12|)
                                      (EXIT
                                       (SEQ
                                        (LETT |t| 0
                                         |FFIELDC-;discreteLog;2SU;12|)
                                        (LETT #1# (- (QCDR |f|) 1)
                                         |FFIELDC-;discreteLog;2SU;12|)
                                        G190
                                        (COND
                                          ((QSGREATERP |t| #1#)
                                           (GO G191)))
                                        (SEQ
                                         (LETT |exp|
                                          (QUOTIENT2 |exp| |fac|)
                                          |FFIELDC-;discreteLog;2SU;12|)
                                         (LETT |rhoHelp|
                                          (SPADCALL |primroot|
                                           (SPADCALL |a| |exp|
                                            (|getShellEntry| $ 50))
                                           |fac|
                                           (|getShellEntry| $ 71))
                                          |FFIELDC-;discreteLog;2SU;12|)
                                         (EXIT
                                          (COND
                                            ((QEQCAR |rhoHelp| 1)
                                             (PROGN
                                               (LETT #2#
                                                (CONS 1 "failed")
                                                |FFIELDC-;discreteLog;2SU;12|)
                                               (GO #2#)))
                                            ('T
                                             (SEQ
                                              (LETT |rho|
                                               (* (QCDR |rhoHelp|)
                                                |mult|)
                                               |FFIELDC-;discreteLog;2SU;12|)
                                              (LETT |disclog|
                                               (+ |disclog| |rho|)
                                               |FFIELDC-;discreteLog;2SU;12|)
                                              (LETT |mult|
                                               (* |mult| |fac|)
                                               |FFIELDC-;discreteLog;2SU;12|)
                                              (EXIT
                                               (LETT |a|
                                                (SPADCALL |a|
                                                 (SPADCALL |logbase|
                                                  (- |rho|)
                                                  (|getShellEntry| $
                                                   50))
                                                 (|getShellEntry| $ 60))
                                                |FFIELDC-;discreteLog;2SU;12|)))))))
                                        (LETT |t| (QSADD1 |t|)
                                         |FFIELDC-;discreteLog;2SU;12|)
                                        (GO G190) G191 (EXIT NIL))))
                                 (LETT #0# (CDR #0#)
                                       |FFIELDC-;discreteLog;2SU;12|)
                                 (GO G190) G191 (EXIT NIL))
                            (EXIT (CONS 0 |disclog|))))))))
           #2# (EXIT #2#))))) 

(DEFUN |FFIELDC-;squareFreePolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 76))) 

(DEFUN |FFIELDC-;factorPolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 78))) 

(DEFUN |FFIELDC-;factorSquareFreePolynomial| (|f| $)
  (PROG (|flist| |u| #0=#:G1517 #1=#:G1514 #2=#:G1512 #3=#:G1513)
    (RETURN
      (SEQ (COND
             ((SPADCALL |f| (|spadConstant| $ 79)
                  (|getShellEntry| $ 80))
              (|spadConstant| $ 81))
             ('T
              (SEQ (LETT |flist|
                         (SPADCALL |f| 'T (|getShellEntry| $ 85))
                         |FFIELDC-;factorSquareFreePolynomial|)
                   (EXIT (SPADCALL
                             (SPADCALL (QCAR |flist|)
                                 (|getShellEntry| $ 86))
                             (PROGN
                               (LETT #3# NIL
                                     |FFIELDC-;factorSquareFreePolynomial|)
                               (SEQ (LETT |u| NIL
                                     |FFIELDC-;factorSquareFreePolynomial|)
                                    (LETT #0# (QCDR |flist|)
                                     |FFIELDC-;factorSquareFreePolynomial|)
                                    G190
                                    (COND
                                      ((OR (ATOM #0#)
                                        (PROGN
                                          (LETT |u| (CAR #0#)
                                           |FFIELDC-;factorSquareFreePolynomial|)
                                          NIL))
                                       (GO G191)))
                                    (SEQ
                                     (EXIT
                                      (PROGN
                                        (LETT #1#
                                         (SPADCALL (QCAR |u|)
                                          (QCDR |u|)
                                          (|getShellEntry| $ 87))
                                         |FFIELDC-;factorSquareFreePolynomial|)
                                        (COND
                                          (#3#
                                           (LETT #2#
                                            (SPADCALL #2# #1#
                                             (|getShellEntry| $ 88))
                                            |FFIELDC-;factorSquareFreePolynomial|))
                                          ('T
                                           (PROGN
                                             (LETT #2# #1#
                                              |FFIELDC-;factorSquareFreePolynomial|)
                                             (LETT #3# 'T
                                              |FFIELDC-;factorSquareFreePolynomial|)))))))
                                    (LETT #0# (CDR #0#)
                                     |FFIELDC-;factorSquareFreePolynomial|)
                                    (GO G190) G191 (EXIT NIL))
                               (COND
                                 (#3# #2#)
                                 ('T (|spadConstant| $ 89))))
                             (|getShellEntry| $ 90)))))))))) 

(DEFUN |FFIELDC-;gcdPolynomial;3Sup;16| (|f| |g| $)
  (SPADCALL |f| |g| (|getShellEntry| $ 92))) 

(DEFUN |FiniteFieldCategory&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|FiniteFieldCategory&|))
        (LETT |dv$| (LIST '|FiniteFieldCategory&| |dv$1|) . #0#)
        (LETT $ (|newShell| 95) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|FiniteFieldCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             |FFIELDC-;differentiate;2S;1| |FFIELDC-;init;S;2|
             (|PositiveInteger|) (4 . |lookup|) (9 . |index|)
             (|Boolean|) (14 . |zero?|) (|Union| $ '"failed")
             |FFIELDC-;nextItem;SU;3| (19 . |order|) (|Integer|)
             (|OnePointCompletion| 10) (24 . |coerce|)
             |FFIELDC-;order;SOpc;4| (|Vector| 6) (|List| 22)
             (|Matrix| 6) (29 . |nullSpace|) (|Mapping| 13 6)
             (34 . |every?|) (40 . |charthRoot|) (|Mapping| 6 6)
             (45 . |map|) (|Vector| $) (|Union| 31 '"failed")
             (|Matrix| $) |FFIELDC-;conditionP;MU;5|
             (|NonNegativeInteger|) (51 . |size|)
             (55 . |characteristic|) (59 . **)
             |FFIELDC-;charthRoot;2S;6| |FFIELDC-;charthRoot;SU;7|
             (65 . |One|)
             (|Union| '"prime" '"polynomial" '"normal" '"cyclic")
             (69 . |representationType|) (73 . =) (79 . |not|)
             |FFIELDC-;createPrimitiveElement;S;8|
             (|Record| (|:| |factor| 18) (|:| |exponent| 18))
             (|List| 47) (84 . |factorsOfCyclicGroupSize|) (88 . **)
             (94 . =) |FFIELDC-;primitive?;SB;9|
             |FFIELDC-;order;SPi;10| (100 . |primitiveElement|)
             (|Table| 10 35) (104 . |tableForDiscreteLogarithm|)
             (109 . |#|) (|Union| 35 '"failed") (114 . |search|)
             (120 . *) |FFIELDC-;discreteLog;SNni;11| (|Void|)
             (|String|) (|OutputForm|) (126 . |messagePrint|)
             (|Factored| $) (131 . |factor|) (|Factored| 18)
             (136 . |factors|) (|DiscreteLogarithmPackage| 6)
             (141 . |shanksDiscLogAlgorithm|)
             |FFIELDC-;discreteLog;2SU;12|
             (|SparseUnivariatePolynomial| 6) (|Factored| 73)
             (|UnivariatePolynomialSquareFree| 6 73)
             (148 . |squareFree|) (|DistinctDegreeFactorize| 6 73)
             (153 . |factor|) (158 . |Zero|) (162 . =) (168 . |Zero|)
             (|Record| (|:| |irr| 73) (|:| |pow| 18)) (|List| 82)
             (|Record| (|:| |cont| 6) (|:| |factors| 83))
             (172 . |distdfact|) (178 . |coerce|) (183 . |primeFactor|)
             (189 . *) (195 . |One|) (199 . *) (|EuclideanDomain&| 73)
             (205 . |gcd|) (|SparseUnivariatePolynomial| $)
             |FFIELDC-;gcdPolynomial;3Sup;16|)
          '#(|primitive?| 211 |order| 216 |nextItem| 226 |init| 231
             |gcdPolynomial| 235 |discreteLog| 241 |differentiate| 252
             |createPrimitiveElement| 257 |conditionP| 261 |charthRoot|
             266)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 94
                                '(0 6 0 7 1 6 10 0 11 1 6 0 10 12 1 6
                                  13 0 14 1 6 10 0 17 1 19 0 18 20 1 24
                                  23 0 25 2 22 13 26 0 27 1 6 0 0 28 2
                                  22 0 29 0 30 0 6 35 36 0 6 35 37 2 6
                                  0 0 35 38 0 6 0 41 0 6 42 43 2 42 13
                                  0 0 44 1 13 0 0 45 0 6 48 49 2 6 0 0
                                  18 50 2 6 13 0 0 51 0 6 0 54 1 6 55
                                  18 56 1 55 35 0 57 2 55 58 10 0 59 2
                                  6 0 0 0 60 1 64 62 63 65 1 18 66 0 67
                                  1 68 48 0 69 3 70 58 6 6 35 71 1 75
                                  74 73 76 1 77 74 73 78 0 73 0 79 2 73
                                  13 0 0 80 0 74 0 81 2 77 84 73 13 85
                                  1 73 0 6 86 2 74 0 73 18 87 2 74 0 0
                                  0 88 0 74 0 89 2 74 0 73 0 90 2 91 0
                                  0 0 92 1 0 13 0 52 1 0 10 0 53 1 0 19
                                  0 21 1 0 15 0 16 0 0 0 9 2 0 93 93 93
                                  94 1 0 35 0 61 2 0 58 0 0 72 1 0 0 0
                                  8 0 0 0 46 1 0 32 33 34 1 0 0 0 39 1
                                  0 15 0 40)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|FiniteFieldCategory&| '|isFunctor|
             '(((|order| ((|PositiveInteger|) $)) T (ELT $ 53))
               ((|discreteLog| ((|NonNegativeInteger|) $)) T
                (ELT $ 61))
               ((|primitive?| ((|Boolean|) $)) T (ELT $ 52))
               ((|createPrimitiveElement| ($)) T (ELT $ 46))
               ((|conditionP|
                    ((|Union| (|Vector| $) "failed") (|Matrix| $)))
                T (ELT $ 34))
               ((|charthRoot| ($ $)) T (ELT $ 39))
               ((|differentiate| ($ $)) T (ELT $ 8))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|init| ($)) T (ELT $ 9))
               ((|nextItem| ((|Union| $ "failed") $)) T (ELT $ 16))
               ((|discreteLog|
                    ((|Union| (|NonNegativeInteger|) "failed") $ $))
                T (ELT $ 72))
               ((|order| ((|OnePointCompletion| (|PositiveInteger|)) $))
                T (ELT $ 21))
               ((|charthRoot| ((|Union| $ "failed") $)) T (ELT $ 40))
               ((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ 94)))
             (|addModemap| '|FiniteFieldCategory&|
                 '(|FiniteFieldCategory&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |order| ((|PositiveInteger|) |#1|))
                       (SIGNATURE |discreteLog|
                           ((|NonNegativeInteger|) |#1|))
                       (SIGNATURE |primitive?| ((|Boolean|) |#1|))
                       (SIGNATURE |createPrimitiveElement| (|#1|))
                       (SIGNATURE |conditionP|
                           ((|Union| (|Vector| |#1|) "failed")
                            (|Matrix| |#1|)))
                       (SIGNATURE |charthRoot| (|#1| |#1|))
                       (SIGNATURE |differentiate| (|#1| |#1|))
                       (SIGNATURE |differentiate|
                           (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE |init| (|#1|))
                       (SIGNATURE |nextItem|
                           ((|Union| |#1| "failed") |#1|))
                       (SIGNATURE |discreteLog|
                           ((|Union| (|NonNegativeInteger|) "failed")
                            |#1| |#1|))
                       (SIGNATURE |order|
                           ((|OnePointCompletion| (|PositiveInteger|))
                            |#1|))
                       (SIGNATURE |charthRoot|
                           ((|Union| |#1| "failed") |#1|))
                       (SIGNATURE |gcdPolynomial|
                           ((|SparseUnivariatePolynomial| |#1|)
                            (|SparseUnivariatePolynomial| |#1|)
                            (|SparseUnivariatePolynomial| |#1|))))
                   (|FiniteFieldCategory|))
                 T '|FiniteFieldCategory&|
                 (|put| '|FiniteFieldCategory&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |order|
                                     ((|PositiveInteger|) |#1|))
                                 (SIGNATURE |discreteLog|
                                     ((|NonNegativeInteger|) |#1|))
                                 (SIGNATURE |primitive?|
                                     ((|Boolean|) |#1|))
                                 (SIGNATURE |createPrimitiveElement|
                                     (|#1|))
                                 (SIGNATURE |conditionP|
                                     ((|Union| (|Vector| |#1|)
                                       "failed")
                                      (|Matrix| |#1|)))
                                 (SIGNATURE |charthRoot| (|#1| |#1|))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1|))
                                 (SIGNATURE |differentiate|
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE |init| (|#1|))
                                 (SIGNATURE |nextItem|
                                     ((|Union| |#1| "failed") |#1|))
                                 (SIGNATURE |discreteLog|
                                     ((|Union| (|NonNegativeInteger|)
                                       "failed")
                                      |#1| |#1|))
                                 (SIGNATURE |order|
                                     ((|OnePointCompletion|
                                       (|PositiveInteger|))
                                      |#1|))
                                 (SIGNATURE |charthRoot|
                                     ((|Union| |#1| "failed") |#1|))
                                 (SIGNATURE |gcdPolynomial|
                                     ((|SparseUnivariatePolynomial|
                                       |#1|)
                                      (|SparseUnivariatePolynomial|
                                       |#1|)
                                      (|SparseUnivariatePolynomial|
                                       |#1|))))
                             (|FiniteFieldCategory|))
                        |$CategoryFrame|)))) 
