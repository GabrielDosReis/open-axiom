
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
                   (|getShellEntry| $ 14))
               |FFIELDC-;nextItem;SU;3|)
         (|getShellEntry| $ 16))
     (CONS 1 "failed"))
    ('T (CONS 0 |a|)))) 

(DEFUN |FFIELDC-;order;SOpc;4| (|e| $)
  (SPADCALL (SPADCALL |e| (|getShellEntry| $ 19))
      (|getShellEntry| $ 22))) 

(DEFUN |FFIELDC-;conditionP;MU;5| (|mat| $)
  (PROG (|l|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |mat| (|getShellEntry| $ 27))
                 |FFIELDC-;conditionP;MU;5|)
           (COND
             ((OR (NULL |l|)
                  (SPADCALL (ELT $ 16) (|SPADfirst| |l|)
                      (|getShellEntry| $ 31)))
              (EXIT (CONS 1 "failed"))))
           (EXIT (CONS 0
                       (SPADCALL (ELT $ 32) (|SPADfirst| |l|)
                           (|getShellEntry| $ 34)))))))) 

(DEFUN |FFIELDC-;charthRoot;2S;6| (|x| $)
  (SPADCALL |x| (QUOTIENT2 2 0) (|getShellEntry| $ 43))) 

(DEFUN |FFIELDC-;charthRoot;SU;7| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 32)))) 

(DEFUN |FFIELDC-;createPrimitiveElement;S;8| ($)
  (PROG (|sm1| |start| |i| |e| |found|)
    (RETURN
      (SEQ (LETT |sm1| (- (SPADCALL (|getShellEntry| $ 46)) 1)
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |start|
                 (COND
                   ((SPADCALL (SPADCALL (|getShellEntry| $ 49))
                        (CONS 1 "polynomial") (|getShellEntry| $ 50))
                    0)
                   ('T 1))
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |found| 'NIL |FFIELDC-;createPrimitiveElement;S;8|)
           (SEQ (LETT |i| |start|
                      |FFIELDC-;createPrimitiveElement;S;8|)
                G190 (COND ((NULL (NOT |found|)) (GO G191)))
                (SEQ (LETT |e|
                           (SPADCALL
                               (PROG1 |i|
                                 (|check-subtype|
                                     (AND
                                      (COND ((< |i| 0) 'NIL) ('T 'T))
                                      (< 0 |i|))
                                     '(|PositiveInteger|) |i|))
                               (|getShellEntry| $ 14))
                           |FFIELDC-;createPrimitiveElement;S;8|)
                     (EXIT (LETT |found|
                                 (EQL (SPADCALL |e|
                                       (|getShellEntry| $ 19))
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
             ((SPADCALL |a| (|getShellEntry| $ 16)) 'NIL)
             ('T
              (SEQ (LETT |explist| (SPADCALL (|getShellEntry| $ 56))
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
                                      (|getShellEntry| $ 58))
                                     (|getShellEntry| $ 59))
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
                  (|getShellEntry| $ 63))
              (|error| "order(0) is not defined "))
             ('T
              (SEQ (LETT |ord| (- 2 1) |FFIELDC-;order;SPi;10|)
                   (LETT |a| 0 |FFIELDC-;order;SPi;10|)
                   (LETT |lof| (SPADCALL (|getShellEntry| $ 56))
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
                                     (|getShellEntry| $ 58))
                                    (|getShellEntry| $ 59))
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
                                           (|getShellEntry| $ 58))
                                          (|getShellEntry| $ 59))
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
             ((SPADCALL |b| (|getShellEntry| $ 16))
              (|error| "discreteLog: logarithm of zero"))
             ('T
              (SEQ (LETT |faclist| (SPADCALL (|getShellEntry| $ 56))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |a| |b| |FFIELDC-;discreteLog;SNni;11|)
                   (LETT |gen| (SPADCALL (|getShellEntry| $ 67))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (EXIT (COND
                           ((SPADCALL |b| |gen| (|getShellEntry| $ 63))
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
                                            (|getShellEntry| $ 69))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |n|
                                           (SPADCALL |exptable|
                                            (|getShellEntry| $ 70))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |c|
                                           (SPADCALL |a| |exp|
                                            (|getShellEntry| $ 58))
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
                                              (|getShellEntry| $ 73))
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
                                                    58))
                                                  (|getShellEntry| $
                                                   79))
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
                                                    58))
                                                  (|getShellEntry| $
                                                   79))
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
                   ((SPADCALL |b| (|getShellEntry| $ 16))
                    (SEQ (SPADCALL "discreteLog: logarithm of zero"
                             (|getShellEntry| $ 85))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |logbase| (|getShellEntry| $ 16))
                    (SEQ (SPADCALL
                             "discreteLog: logarithm to base zero"
                             (|getShellEntry| $ 85))
                         (EXIT (CONS 1 "failed"))))
                   ((SPADCALL |b| |logbase| (|getShellEntry| $ 63))
                    (CONS 0 1))
                   ('T
                    (COND
                      ((NULL (ZEROP (REMAINDER2
                                     (LETT |groupord|
                                      (SPADCALL |logbase|
                                       (|getShellEntry| $ 19))
                                      |FFIELDC-;discreteLog;2SU;12|)
                                     (SPADCALL |b|
                                      (|getShellEntry| $ 19)))))
                       (SEQ (SPADCALL
                                "discreteLog: second argument not in cyclic group generated by first argument"
                                (|getShellEntry| $ 85))
                            (EXIT (CONS 1 "failed"))))
                      ('T
                       (SEQ (LETT |faclist|
                                  (SPADCALL
                                      (SPADCALL |groupord|
                                       (|getShellEntry| $ 89))
                                      (|getShellEntry| $ 91))
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
                                        (|getShellEntry| $ 58))
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
                                            (|getShellEntry| $ 58))
                                           |fac|
                                           (|getShellEntry| $ 93))
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
                                                   58))
                                                 (|getShellEntry| $ 79))
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
  (SPADCALL |f| (|getShellEntry| $ 98))) 

(DEFUN |FFIELDC-;factorPolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 100))) 

(DEFUN |FFIELDC-;factorSquareFreePolynomial| (|f| $)
  (PROG (|flist| |u| #0=#:G1521 #1=#:G1510 #2=#:G1508 #3=#:G1509)
    (RETURN
      (SEQ (COND
             ((SPADCALL |f| (|spadConstant| $ 101)
                  (|getShellEntry| $ 102))
              (|spadConstant| $ 103))
             ('T
              (SEQ (LETT |flist|
                         (SPADCALL |f| 'T (|getShellEntry| $ 107))
                         |FFIELDC-;factorSquareFreePolynomial|)
                   (EXIT (SPADCALL
                             (SPADCALL (QCAR |flist|)
                                 (|getShellEntry| $ 108))
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
                                          (|getShellEntry| $ 109))
                                         |FFIELDC-;factorSquareFreePolynomial|)
                                        (COND
                                          (#3#
                                           (LETT #2#
                                            (SPADCALL #2# #1#
                                             (|getShellEntry| $ 110))
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
                                 ('T (|spadConstant| $ 111))))
                             (|getShellEntry| $ 112)))))))))) 

(DEFUN |FFIELDC-;gcdPolynomial;3Sup;16| (|f| |g| $)
  (SPADCALL |f| |g| (|getShellEntry| $ 114))) 

(DEFUN |FiniteFieldCategory&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|FiniteFieldCategory&|))
        (LETT |dv$| (LIST '|FiniteFieldCategory&| |dv$1|) . #0#)
        (LETT $ (|newShell| 117) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

(MAKEPROP '|FiniteFieldCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             |FFIELDC-;differentiate;2S;1| |FFIELDC-;init;S;2|
             (|PositiveInteger|) (4 . |lookup|) (9 . |One|) (13 . +)
             (19 . |index|) (|Boolean|) (24 . |zero?|)
             (|Union| $ '"failed") |FFIELDC-;nextItem;SU;3|
             (29 . |order|) (|Integer|) (|OnePointCompletion| 10)
             (34 . |coerce|) |FFIELDC-;order;SOpc;4| (|Vector| 6)
             (|List| 24) (|Matrix| 6) (39 . |nullSpace|)
             (44 . |empty?|) (49 . |first|) (|Mapping| 15 6)
             (54 . |every?|) (60 . |charthRoot|) (|Mapping| 6 6)
             (65 . |map|) (|Vector| $) (|Union| 35 '"failed")
             (|Matrix| $) |FFIELDC-;conditionP;MU;5|
             (|NonNegativeInteger|) (71 . |size|)
             (75 . |characteristic|) (79 . |quo|) (85 . **)
             |FFIELDC-;charthRoot;2S;6| |FFIELDC-;charthRoot;SU;7|
             (91 . |size|) (95 . -)
             (|Union| '"prime" '"polynomial" '"normal" '"cyclic")
             (101 . |representationType|) (105 . =) (111 . |false|)
             (115 . =) |FFIELDC-;createPrimitiveElement;S;8|
             (|Record| (|:| |factor| 20) (|:| |exponent| 20))
             (|List| 54) (121 . |factorsOfCyclicGroupSize|)
             (125 . |quo|) (131 . **) (137 . |one?|)
             |FFIELDC-;primitive?;SB;9| (142 . |Zero|) (146 . |Zero|)
             (150 . =) (|SingleInteger|) (156 . |Zero|)
             |FFIELDC-;order;SPi;10| (160 . |primitiveElement|)
             (|Table| 10 39) (164 . |tableForDiscreteLogarithm|)
             (169 . |#|) (174 . |One|) (|Union| 39 '"failed")
             (178 . |search|) (184 . |true|) (188 . *) (194 . +)
             (200 . *) (206 . -) (211 . *) (217 . +)
             |FFIELDC-;discreteLog;SNni;11| (|Void|) (|String|)
             (|OutputForm|) (223 . |messagePrint|) (228 . |rem|)
             (234 . |zero?|) (|Factored| $) (239 . |factor|)
             (|Factored| 20) (244 . |factors|)
             (|DiscreteLogarithmPackage| 6)
             (249 . |shanksDiscLogAlgorithm|)
             |FFIELDC-;discreteLog;2SU;12|
             (|SparseUnivariatePolynomial| 6) (|Factored| 95)
             (|UnivariatePolynomialSquareFree| 6 95)
             (256 . |squareFree|) (|DistinctDegreeFactorize| 6 95)
             (261 . |factor|) (266 . |Zero|) (270 . =) (276 . |Zero|)
             (|Record| (|:| |irr| 95) (|:| |pow| 20)) (|List| 104)
             (|Record| (|:| |cont| 6) (|:| |factors| 105))
             (280 . |distdfact|) (286 . |coerce|) (291 . |primeFactor|)
             (297 . *) (303 . |One|) (307 . *) (|EuclideanDomain&| 95)
             (313 . |gcd|) (|SparseUnivariatePolynomial| $)
             |FFIELDC-;gcdPolynomial;3Sup;16|)
          '#(|primitive?| 319 |order| 324 |nextItem| 334 |init| 339
             |gcdPolynomial| 343 |discreteLog| 349 |differentiate| 360
             |createPrimitiveElement| 365 |conditionP| 369 |charthRoot|
             374)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 116
                                '(0 6 0 7 1 6 10 0 11 0 10 0 12 2 10 0
                                  0 0 13 1 6 0 10 14 1 6 15 0 16 1 6 10
                                  0 19 1 21 0 20 22 1 26 25 0 27 1 25
                                  15 0 28 1 25 24 0 29 2 24 15 30 0 31
                                  1 6 0 0 32 2 24 0 33 0 34 0 15 39 40
                                  0 20 39 41 2 39 0 0 0 42 2 6 0 0 39
                                  43 0 6 39 46 2 20 0 0 0 47 0 6 48 49
                                  2 48 15 0 0 50 0 15 0 51 2 10 15 0 0
                                  52 0 6 55 56 2 20 0 0 0 57 2 6 0 0 20
                                  58 1 6 15 0 59 0 39 0 61 0 20 0 62 2
                                  6 15 0 0 63 0 64 0 65 0 6 0 67 1 6 68
                                  20 69 1 68 39 0 70 0 39 0 71 2 68 72
                                  10 0 73 0 15 0 74 2 39 0 39 0 75 2 39
                                  0 0 0 76 2 20 0 20 0 77 1 20 0 0 78 2
                                  6 0 0 0 79 2 20 0 0 0 80 1 84 82 83
                                  85 2 39 0 0 0 86 1 39 15 0 87 1 20 88
                                  0 89 1 90 55 0 91 3 92 72 6 6 39 93 1
                                  97 96 95 98 1 99 96 95 100 0 95 0 101
                                  2 95 15 0 0 102 0 96 0 103 2 99 106
                                  95 15 107 1 95 0 6 108 2 96 0 95 20
                                  109 2 96 0 0 0 110 0 96 0 111 2 96 0
                                  95 0 112 2 113 0 0 0 114 1 0 15 0 60
                                  1 0 10 0 66 1 0 21 0 23 1 0 17 0 18 0
                                  0 0 9 2 0 115 115 115 116 1 0 39 0 81
                                  2 0 72 0 0 94 1 0 0 0 8 0 0 0 53 1 0
                                  36 37 38 1 0 0 0 44 1 0 17 0 45)))))
          '|lookupComplete|)) 
