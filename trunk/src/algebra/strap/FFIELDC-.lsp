
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
  (SPADCALL |x| (QUOTIENT2 2 0) (|getShellEntry| $ 36))) 

(DEFUN |FFIELDC-;charthRoot;SU;7| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 28)))) 

(DEFUN |FFIELDC-;createPrimitiveElement;S;8| ($)
  (PROG (|sm1| |start| |i| #0=#:G1443 |e| |found|)
    (RETURN
      (SEQ (LETT |sm1| (- (SPADCALL (|getShellEntry| $ 39)) 1)
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |start|
                 (COND
                   ((SPADCALL (SPADCALL (|getShellEntry| $ 41))
                        (CONS 1 "polynomial") (|getShellEntry| $ 42))
                    0)
                   ('T 1))
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |found| 'NIL |FFIELDC-;createPrimitiveElement;S;8|)
           (SEQ (LETT |i| |start|
                      |FFIELDC-;createPrimitiveElement;S;8|)
                G190
                (COND
                  ((NULL (SPADCALL |found| (|getShellEntry| $ 43)))
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
              (SEQ (LETT |explist| (SPADCALL (|getShellEntry| $ 47))
                         |FFIELDC-;primitive?;SB;9|)
                   (LETT |q| (- 2 1) |FFIELDC-;primitive?;SB;9|)
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
                                      (|getShellEntry| $ 43))))
                           (GO G191)))
                        (SEQ (EXIT (LETT |equalone|
                                    (SPADCALL
                                     (SPADCALL |a|
                                      (QUOTIENT2 |q| (QCAR |exp|))
                                      (|getShellEntry| $ 48))
                                     (|spadConstant| $ 49)
                                     (|getShellEntry| $ 50))
                                    |FFIELDC-;primitive?;SB;9|)))
                        (LETT #0# (CDR #0#) |FFIELDC-;primitive?;SB;9|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT (SPADCALL |equalone| (|getShellEntry| $ 43)))))))))) 

(DEFUN |FFIELDC-;order;SPi;10| (|e| $)
  (PROG (|lof| |rec| #0=#:G1463 |primeDivisor| |j| #1=#:G1464 |a|
               |goon| |ord|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |e| (|spadConstant| $ 7)
                  (|getShellEntry| $ 50))
              (|error| "order(0) is not defined "))
             ('T
              (SEQ (LETT |ord| (- 2 1) |FFIELDC-;order;SPi;10|)
                   (LETT |a| 0 |FFIELDC-;order;SPi;10|)
                   (LETT |lof| (SPADCALL (|getShellEntry| $ 47))
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
                                     (|getShellEntry| $ 48))
                                    (|spadConstant| $ 49)
                                    (|getShellEntry| $ 50))
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
                                           (|getShellEntry| $ 48))
                                          (|spadConstant| $ 49)
                                          (|getShellEntry| $ 50))
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
              (SEQ (LETT |faclist| (SPADCALL (|getShellEntry| $ 47))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |a| |b| |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |gen| (SPADCALL (|getShellEntry| $ 53))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (EXIT (COND
                           ((SPADCALL |b| |gen| (|getShellEntry| $ 50))
                            1)
                           ('T
                            (SEQ (LETT |disclog| 0
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |mult| 1
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |groupord| (- 2 1)
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
                                            (|getShellEntry| $ 55))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |n|
                                           (SPADCALL |exptable|
                                            (|getShellEntry| $ 56))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |c|
                                           (SPADCALL |a| |exp|
                                            (|getShellEntry| $ 48))
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
                                                 (|getShellEntry| $ 43))))
                                              (GO G191)))
                                           (SEQ
                                            (LETT |rho|
                                             (SPADCALL
                                              (SPADCALL |c|
                                               (|getShellEntry| $ 11))
                                              |exptable|
                                              (|getShellEntry| $ 58))
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
                                                    48))
                                                  (|getShellEntry| $
                                                   59))
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
                                                    48))
                                                  (|getShellEntry| $
                                                   59))
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
                             (|getShellEntry| $ 64))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |logbase| (|getShellEntry| $ 14))
                    (SEQ (SPADCALL
                             "discreteLog: logarithm to base zero"
                             (|getShellEntry| $ 64))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |b| |logbase| (|getShellEntry| $ 50))
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
                                (|getShellEntry| $ 64))
                            (EXIT (CONS 1 "failed"))))
                      ('T
                       (SEQ (LETT |faclist|
                                  (SPADCALL
                                      (SPADCALL |groupord|
                                       (|getShellEntry| $ 66))
                                      (|getShellEntry| $ 68))
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
                                        (|getShellEntry| $ 48))
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
                                            (|getShellEntry| $ 48))
                                           |fac|
                                           (|getShellEntry| $ 70))
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
                                                   48))
                                                 (|getShellEntry| $ 59))
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
  (SPADCALL |f| (|getShellEntry| $ 75))) 

(DEFUN |FFIELDC-;factorPolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 77))) 

(DEFUN |FFIELDC-;factorSquareFreePolynomial| (|f| $)
  (PROG (|flist| |u| #0=#:G1517 #1=#:G1514 #2=#:G1512 #3=#:G1513)
    (RETURN
      (SEQ (COND
             ((SPADCALL |f| (|spadConstant| $ 78)
                  (|getShellEntry| $ 79))
              (|spadConstant| $ 80))
             ('T
              (SEQ (LETT |flist|
                         (SPADCALL |f| 'T (|getShellEntry| $ 84))
                         |FFIELDC-;factorSquareFreePolynomial|)
                   (EXIT (SPADCALL
                             (SPADCALL (QCAR |flist|)
                                 (|getShellEntry| $ 85))
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
                                          (|getShellEntry| $ 86))
                                         |FFIELDC-;factorSquareFreePolynomial|)
                                        (COND
                                          (#3#
                                           (LETT #2#
                                            (SPADCALL #2# #1#
                                             (|getShellEntry| $ 87))
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
                                 ('T (|spadConstant| $ 88))))
                             (|getShellEntry| $ 89)))))))))) 

(DEFUN |FFIELDC-;gcdPolynomial;3Sup;16| (|f| |g| $)
  (SPADCALL |f| |g| (|getShellEntry| $ 91))) 

(DEFUN |FiniteFieldCategory&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|FiniteFieldCategory&|))
        (LETT |dv$| (LIST '|FiniteFieldCategory&| |dv$1|) . #0#)
        (LETT $ (|newShell| 94) . #0#)
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
             (|NonNegativeInteger|) (51 . **)
             |FFIELDC-;charthRoot;2S;6| |FFIELDC-;charthRoot;SU;7|
             (57 . |size|)
             (|Union| '"prime" '"polynomial" '"normal" '"cyclic")
             (61 . |representationType|) (65 . =) (71 . |not|)
             |FFIELDC-;createPrimitiveElement;S;8|
             (|Record| (|:| |factor| 18) (|:| |exponent| 18))
             (|List| 45) (76 . |factorsOfCyclicGroupSize|) (80 . **)
             (86 . |One|) (90 . =) |FFIELDC-;primitive?;SB;9|
             |FFIELDC-;order;SPi;10| (96 . |primitiveElement|)
             (|Table| 10 35) (100 . |tableForDiscreteLogarithm|)
             (105 . |#|) (|Union| 35 '"failed") (110 . |search|)
             (116 . *) |FFIELDC-;discreteLog;SNni;11| (|Void|)
             (|String|) (|OutputForm|) (122 . |messagePrint|)
             (|Factored| $) (127 . |factor|) (|Factored| 18)
             (132 . |factors|) (|DiscreteLogarithmPackage| 6)
             (137 . |shanksDiscLogAlgorithm|)
             |FFIELDC-;discreteLog;2SU;12|
             (|SparseUnivariatePolynomial| 6) (|Factored| 72)
             (|UnivariatePolynomialSquareFree| 6 72)
             (144 . |squareFree|) (|DistinctDegreeFactorize| 6 72)
             (149 . |factor|) (154 . |Zero|) (158 . =) (164 . |Zero|)
             (|Record| (|:| |irr| 72) (|:| |pow| 18)) (|List| 81)
             (|Record| (|:| |cont| 6) (|:| |factors| 82))
             (168 . |distdfact|) (174 . |coerce|) (179 . |primeFactor|)
             (185 . *) (191 . |One|) (195 . *) (|EuclideanDomain&| 72)
             (201 . |gcd|) (|SparseUnivariatePolynomial| $)
             |FFIELDC-;gcdPolynomial;3Sup;16|)
          '#(|primitive?| 207 |order| 212 |nextItem| 222 |init| 227
             |gcdPolynomial| 231 |discreteLog| 237 |differentiate| 248
             |createPrimitiveElement| 253 |conditionP| 257 |charthRoot|
             262)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 93
                                '(0 6 0 7 1 6 10 0 11 1 6 0 10 12 1 6
                                  13 0 14 1 6 10 0 17 1 19 0 18 20 1 24
                                  23 0 25 2 22 13 26 0 27 1 6 0 0 28 2
                                  22 0 29 0 30 2 6 0 0 35 36 0 6 35 39
                                  0 6 40 41 2 40 13 0 0 42 1 13 0 0 43
                                  0 6 46 47 2 6 0 0 18 48 0 6 0 49 2 6
                                  13 0 0 50 0 6 0 53 1 6 54 18 55 1 54
                                  35 0 56 2 54 57 10 0 58 2 6 0 0 0 59
                                  1 63 61 62 64 1 18 65 0 66 1 67 46 0
                                  68 3 69 57 6 6 35 70 1 74 73 72 75 1
                                  76 73 72 77 0 72 0 78 2 72 13 0 0 79
                                  0 73 0 80 2 76 83 72 13 84 1 72 0 6
                                  85 2 73 0 72 18 86 2 73 0 0 0 87 0 73
                                  0 88 2 73 0 72 0 89 2 90 0 0 0 91 1 0
                                  13 0 51 1 0 10 0 52 1 0 19 0 21 1 0
                                  15 0 16 0 0 0 9 2 0 92 92 92 93 1 0
                                  35 0 60 2 0 57 0 0 71 1 0 0 0 8 0 0 0
                                  44 1 0 32 33 34 1 0 0 0 37 1 0 15 0
                                  38)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|FiniteFieldCategory&| '|isFunctor|
             '(((|order| ((|PositiveInteger|) $)) T (ELT $ 52))
               ((|discreteLog| ((|NonNegativeInteger|) $)) T
                (ELT $ 60))
               ((|primitive?| ((|Boolean|) $)) T (ELT $ 51))
               ((|createPrimitiveElement| ($)) T (ELT $ 44))
               ((|conditionP|
                    ((|Union| (|Vector| $) "failed") (|Matrix| $)))
                T (ELT $ 34))
               ((|charthRoot| ($ $)) T (ELT $ 37))
               ((|differentiate| ($ $)) T (ELT $ 8))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|init| ($)) T (ELT $ 9))
               ((|nextItem| ((|Union| $ "failed") $)) T (ELT $ 16))
               ((|discreteLog|
                    ((|Union| (|NonNegativeInteger|) "failed") $ $))
                T (ELT $ 71))
               ((|order| ((|OnePointCompletion| (|PositiveInteger|)) $))
                T (ELT $ 21))
               ((|charthRoot| ((|Union| $ "failed") $)) T (ELT $ 38))
               ((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ 93)))
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
