
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;differentiate;2S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |FFIELDC-;init;S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;nextItem;SU;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;order;SOpc;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;conditionP;MU;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;charthRoot;2S;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;charthRoot;SU;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|)
                |FFIELDC-;createPrimitiveElement;S;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |FFIELDC-;primitive?;SB;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 1))
                |FFIELDC-;order;SPi;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |FFIELDC-;discreteLog;SNni;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;discreteLog;2SU;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;squareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;factorPolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;factorSquareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;gcdPolynomial;3Sup;16|)) 

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
  (PROG (|sm1| |start| |i| #0=#:G1446 |e| |found|)
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
                G190 (COND ((NULL (NOT |found|)) (GO G191)))
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
  (PROG (|explist| |q| |exp| #0=#:G1514 |equalone|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|getShellEntry| $ 14)) 'NIL)
             ('T
              (SEQ (LETT |explist| (SPADCALL (|getShellEntry| $ 46))
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
                               (NULL (NOT |equalone|)))
                           (GO G191)))
                        (SEQ (EXIT (LETT |equalone|
                                    (SPADCALL
                                     (SPADCALL |a|
                                      (QUOTIENT2 |q| (QCAR |exp|))
                                      (|getShellEntry| $ 47))
                                     (|spadConstant| $ 48)
                                     (|getShellEntry| $ 49))
                                    |FFIELDC-;primitive?;SB;9|)))
                        (LETT #0# (CDR #0#) |FFIELDC-;primitive?;SB;9|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT (NOT |equalone|))))))))) 

(DEFUN |FFIELDC-;order;SPi;10| (|e| $)
  (PROG (|lof| |rec| #0=#:G1515 |primeDivisor| |j| #1=#:G1516 |a|
               |goon| |ord|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |e| (|spadConstant| $ 7)
                  (|getShellEntry| $ 49))
              (|error| "order(0) is not defined "))
             ('T
              (SEQ (LETT |ord| (- 2 1) |FFIELDC-;order;SPi;10|)
                   (LETT |a| 0 |FFIELDC-;order;SPi;10|)
                   (LETT |lof| (SPADCALL (|getShellEntry| $ 46))
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
                                     (|getShellEntry| $ 47))
                                    (|spadConstant| $ 48)
                                    (|getShellEntry| $ 49))
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
                                           (|getShellEntry| $ 47))
                                          (|spadConstant| $ 48)
                                          (|getShellEntry| $ 49))
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
  (PROG (|faclist| |gen| |groupord| |f| #0=#:G1517 |fac| |t| #1=#:G1518
            |exp| |exptable| |n| |end| |i| |rho| |found| |disc1| |c|
            |mult| |disclog| |a|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |b| (|getShellEntry| $ 14))
              (|error| "discreteLog: logarithm of zero"))
             ('T
              (SEQ (LETT |faclist| (SPADCALL (|getShellEntry| $ 46))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |a| |b| |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |gen| (SPADCALL (|getShellEntry| $ 52))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (EXIT (COND
                           ((SPADCALL |b| |gen| (|getShellEntry| $ 49))
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
                                            (|getShellEntry| $ 54))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |n|
                                           (SPADCALL |exptable|
                                            (|getShellEntry| $ 55))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |c|
                                           (SPADCALL |a| |exp|
                                            (|getShellEntry| $ 47))
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
                                               (NULL (NOT |found|)))
                                              (GO G191)))
                                           (SEQ
                                            (LETT |rho|
                                             (SPADCALL
                                              (SPADCALL |c|
                                               (|getShellEntry| $ 11))
                                              |exptable|
                                              (|getShellEntry| $ 57))
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
                                                    47))
                                                  (|getShellEntry| $
                                                   58))
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
                                                    47))
                                                  (|getShellEntry| $
                                                   58))
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
  (PROG (|groupord| |faclist| |f| #0=#:G1519 |fac| |primroot| |t|
            #1=#:G1520 |exp| |rhoHelp| #2=#:G1500 |rho| |disclog|
            |mult| |a|)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |b| (|getShellEntry| $ 14))
                    (SEQ (SPADCALL "discreteLog: logarithm of zero"
                             (|getShellEntry| $ 63))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |logbase| (|getShellEntry| $ 14))
                    (SEQ (SPADCALL
                             "discreteLog: logarithm to base zero"
                             (|getShellEntry| $ 63))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |b| |logbase| (|getShellEntry| $ 49))
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
                                (|getShellEntry| $ 63))
                            (EXIT (CONS 1 "failed"))))
                      ('T
                       (SEQ (LETT |faclist|
                                  (SPADCALL
                                      (SPADCALL |groupord|
                                       (|getShellEntry| $ 65))
                                      (|getShellEntry| $ 67))
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
                                        (|getShellEntry| $ 47))
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
                                            (|getShellEntry| $ 47))
                                           |fac|
                                           (|getShellEntry| $ 69))
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
                                                   47))
                                                 (|getShellEntry| $ 58))
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
  (SPADCALL |f| (|getShellEntry| $ 74))) 

(DEFUN |FFIELDC-;factorPolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 76))) 

(DEFUN |FFIELDC-;factorSquareFreePolynomial| (|f| $)
  (PROG (|flist| |u| #0=#:G1521 #1=#:G1510 #2=#:G1508 #3=#:G1509)
    (RETURN
      (SEQ (COND
             ((SPADCALL |f| (|spadConstant| $ 77)
                  (|getShellEntry| $ 78))
              (|spadConstant| $ 79))
             ('T
              (SEQ (LETT |flist|
                         (SPADCALL |f| 'T (|getShellEntry| $ 83))
                         |FFIELDC-;factorSquareFreePolynomial|)
                   (EXIT (SPADCALL
                             (SPADCALL (QCAR |flist|)
                                 (|getShellEntry| $ 84))
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
                                          (|getShellEntry| $ 85))
                                         |FFIELDC-;factorSquareFreePolynomial|)
                                        (COND
                                          (#3#
                                           (LETT #2#
                                            (SPADCALL #2# #1#
                                             (|getShellEntry| $ 86))
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
                                 ('T (|spadConstant| $ 87))))
                             (|getShellEntry| $ 88)))))))))) 

(DEFUN |FFIELDC-;gcdPolynomial;3Sup;16| (|f| |g| $)
  (SPADCALL |f| |g| (|getShellEntry| $ 90))) 

(DEFUN |FiniteFieldCategory&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|FiniteFieldCategory&|))
        (LETT |dv$| (LIST '|FiniteFieldCategory&| |dv$1|) . #0#)
        (LETT $ (|newShell| 93) . #0#)
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
             (61 . |representationType|) (65 . =)
             |FFIELDC-;createPrimitiveElement;S;8|
             (|Record| (|:| |factor| 18) (|:| |exponent| 18))
             (|List| 44) (71 . |factorsOfCyclicGroupSize|) (75 . **)
             (81 . |One|) (85 . =) |FFIELDC-;primitive?;SB;9|
             |FFIELDC-;order;SPi;10| (91 . |primitiveElement|)
             (|Table| 10 35) (95 . |tableForDiscreteLogarithm|)
             (100 . |#|) (|Union| 35 '"failed") (105 . |search|)
             (111 . *) |FFIELDC-;discreteLog;SNni;11| (|Void|)
             (|String|) (|OutputForm|) (117 . |messagePrint|)
             (|Factored| $) (122 . |factor|) (|Factored| 18)
             (127 . |factors|) (|DiscreteLogarithmPackage| 6)
             (132 . |shanksDiscLogAlgorithm|)
             |FFIELDC-;discreteLog;2SU;12|
             (|SparseUnivariatePolynomial| 6) (|Factored| 71)
             (|UnivariatePolynomialSquareFree| 6 71)
             (139 . |squareFree|) (|DistinctDegreeFactorize| 6 71)
             (144 . |factor|) (149 . |Zero|) (153 . =) (159 . |Zero|)
             (|Record| (|:| |irr| 71) (|:| |pow| 18)) (|List| 80)
             (|Record| (|:| |cont| 6) (|:| |factors| 81))
             (163 . |distdfact|) (169 . |coerce|) (174 . |primeFactor|)
             (180 . *) (186 . |One|) (190 . *) (|EuclideanDomain&| 71)
             (196 . |gcd|) (|SparseUnivariatePolynomial| $)
             |FFIELDC-;gcdPolynomial;3Sup;16|)
          '#(|primitive?| 202 |order| 207 |nextItem| 217 |init| 222
             |gcdPolynomial| 226 |discreteLog| 232 |differentiate| 243
             |createPrimitiveElement| 248 |conditionP| 252 |charthRoot|
             257)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 92
                                '(0 6 0 7 1 6 10 0 11 1 6 0 10 12 1 6
                                  13 0 14 1 6 10 0 17 1 19 0 18 20 1 24
                                  23 0 25 2 22 13 26 0 27 1 6 0 0 28 2
                                  22 0 29 0 30 2 6 0 0 35 36 0 6 35 39
                                  0 6 40 41 2 40 13 0 0 42 0 6 45 46 2
                                  6 0 0 18 47 0 6 0 48 2 6 13 0 0 49 0
                                  6 0 52 1 6 53 18 54 1 53 35 0 55 2 53
                                  56 10 0 57 2 6 0 0 0 58 1 62 60 61 63
                                  1 18 64 0 65 1 66 45 0 67 3 68 56 6 6
                                  35 69 1 73 72 71 74 1 75 72 71 76 0
                                  71 0 77 2 71 13 0 0 78 0 72 0 79 2 75
                                  82 71 13 83 1 71 0 6 84 2 72 0 71 18
                                  85 2 72 0 0 0 86 0 72 0 87 2 72 0 71
                                  0 88 2 89 0 0 0 90 1 0 13 0 50 1 0 10
                                  0 51 1 0 19 0 21 1 0 15 0 16 0 0 0 9
                                  2 0 91 91 91 92 1 0 35 0 59 2 0 56 0
                                  0 70 1 0 0 0 8 0 0 0 43 1 0 32 33 34
                                  1 0 0 0 37 1 0 15 0 38)))))
          '|lookupComplete|)) 
