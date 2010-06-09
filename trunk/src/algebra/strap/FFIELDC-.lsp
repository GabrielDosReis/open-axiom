
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
  (SPADCALL |x|
      (QUOTIENT2 (SPADCALL (|getShellEntry| $ 40))
          (|spadConstant| $ 41))
      (|getShellEntry| $ 43))) 

(DEFUN |FFIELDC-;charthRoot;SU;7| (|x| $)
  (CONS 0 (SPADCALL |x| (|getShellEntry| $ 32)))) 

(DEFUN |FFIELDC-;createPrimitiveElement;S;8| ($)
  (PROG (|sm1| |start| |i| |e| |found|)
    (RETURN
      (SEQ (LETT |sm1| (- (SPADCALL (|getShellEntry| $ 40)) 1)
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |start|
                 (COND
                   ((SPADCALL (SPADCALL (|getShellEntry| $ 48))
                        (CONS 1 "polynomial") (|getShellEntry| $ 49))
                    (|spadConstant| $ 41))
                   ('T 1))
                 |FFIELDC-;createPrimitiveElement;S;8|)
           (LETT |found| NIL |FFIELDC-;createPrimitiveElement;S;8|)
           (SEQ (LETT |i| |start|
                      |FFIELDC-;createPrimitiveElement;S;8|)
                G190 (COND ((NULL (NOT |found|)) (GO G191)))
                (SEQ (LETT |e|
                           (SPADCALL
                               (|check-subtype|
                                   (AND (>= |i| 0) (> |i| 0))
                                   '(|PositiveInteger|) |i|)
                               (|getShellEntry| $ 14))
                           |FFIELDC-;createPrimitiveElement;S;8|)
                     (EXIT (LETT |found|
                                 (EQL (SPADCALL |e|
                                       (|getShellEntry| $ 19))
                                      |sm1|)
                                 |FFIELDC-;createPrimitiveElement;S;8|)))
                (SETQ |i| (+ |i| 1)) (GO G190) G191 (EXIT NIL))
           (EXIT |e|))))) 

(DEFUN |FFIELDC-;primitive?;SB;9| (|a| $)
  (PROG (|explist| |q| |exp| #0=#:G1513 |equalone|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |a| (|getShellEntry| $ 16)) NIL)
             ('T
              (SEQ (LETT |explist| (SPADCALL (|getShellEntry| $ 56))
                         |FFIELDC-;primitive?;SB;9|)
                   (LETT |q| (- (SPADCALL (|getShellEntry| $ 40)) 1)
                         |FFIELDC-;primitive?;SB;9|)
                   (LETT |equalone| NIL |FFIELDC-;primitive?;SB;9|)
                   (SEQ (LETT |exp| NIL |FFIELDC-;primitive?;SB;9|)
                        (LETT #0# |explist| |FFIELDC-;primitive?;SB;9|)
                        G190
                        (COND
                          ((OR (ATOM #0#)
                               (PROGN (SETQ |exp| (CAR #0#)) NIL)
                               (NULL (NOT |equalone|)))
                           (GO G191)))
                        (LETT |equalone|
                              (SPADCALL
                                  (SPADCALL |a|
                                      (QUOTIENT2 |q| (CAR |exp|))
                                      (|getShellEntry| $ 58))
                                  (|getShellEntry| $ 59))
                              |FFIELDC-;primitive?;SB;9|)
                        (SETQ #0# (CDR #0#)) (GO G190) G191 (EXIT NIL))
                   (EXIT (NOT |equalone|))))))))) 

(DEFUN |FFIELDC-;order;SPi;10| (|e| $)
  (PROG (|lof| |rec| #0=#:G1514 |primeDivisor| |j| #1=#:G1515 |a|
               |goon| |ord|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |e| (|spadConstant| $ 7)
                  (|getShellEntry| $ 63))
              (|error| "order(0) is not defined "))
             ('T
              (SEQ (LETT |ord| (- (SPADCALL (|getShellEntry| $ 40)) 1)
                         |FFIELDC-;order;SPi;10|)
                   (LETT |lof| (SPADCALL (|getShellEntry| $ 56))
                         |FFIELDC-;order;SPi;10|)
                   (SEQ (LETT |rec| NIL |FFIELDC-;order;SPi;10|)
                        (LETT #0# |lof| |FFIELDC-;order;SPi;10|) G190
                        (COND
                          ((OR (ATOM #0#)
                               (PROGN (SETQ |rec| (CAR #0#)) NIL))
                           (GO G191)))
                        (SEQ (LETT |a|
                                   (QUOTIENT2 |ord|
                                    (LETT |primeDivisor| (CAR |rec|)
                                     |FFIELDC-;order;SPi;10|))
                                   |FFIELDC-;order;SPi;10|)
                             (LETT |goon|
                                   (SPADCALL
                                    (SPADCALL |e| |a|
                                     (|getShellEntry| $ 58))
                                    (|getShellEntry| $ 59))
                                   |FFIELDC-;order;SPi;10|)
                             (SEQ (LETT |j| 0 |FFIELDC-;order;SPi;10|)
                                  (LETT #1# (- (CDR |rec|) 2)
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
                                  (SETQ |j| (QSADD1 |j|)) (GO G190)
                                  G191 (EXIT NIL))
                             (EXIT (COND
                                     (|goon|
                                      (LETT |ord| |a|
                                       |FFIELDC-;order;SPi;10|)))))
                        (SETQ #0# (CDR #0#)) (GO G190) G191 (EXIT NIL))
                   (EXIT |ord|)))))))) 

(DEFUN |FFIELDC-;discreteLog;SNni;11| (|b| $)
  (PROG (|faclist| |gen| |groupord| |f| #0=#:G1516 |fac| |t| #1=#:G1517
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
                   (LETT |gen| (SPADCALL (|getShellEntry| $ 65))
                         |FFIELDC-;discreteLog;SNni;11|)
                   (EXIT (COND
                           ((SPADCALL |b| |gen| (|getShellEntry| $ 63))
                            1)
                           ('T
                            (SEQ (LETT |disclog| 0
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |mult| 1
                                       |FFIELDC-;discreteLog;SNni;11|)
                                 (LETT |groupord|
                                       (-
                                        (SPADCALL
                                         (|getShellEntry| $ 40))
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
                                            (SETQ |f| (CAR #0#))
                                            NIL))
                                         (GO G191)))
                                      (SEQ
                                       (LETT |fac| (CAR |f|)
                                        |FFIELDC-;discreteLog;SNni;11|)
                                       (EXIT
                                        (SEQ
                                         (LETT |t| 0
                                          |FFIELDC-;discreteLog;SNni;11|)
                                         (LETT #1# (- (CDR |f|) 1)
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
                                            (|getShellEntry| $ 67))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |n|
                                           (SPADCALL |exptable|
                                            (|getShellEntry| $ 68))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |c|
                                           (SPADCALL |a| |exp|
                                            (|getShellEntry| $ 58))
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |end|
                                           (QUOTIENT2 (- |fac| 1) |n|)
                                           |FFIELDC-;discreteLog;SNni;11|)
                                          (LETT |found| NIL
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
                                              (|getShellEntry| $ 71))
                                             |FFIELDC-;discreteLog;SNni;11|)
                                            (EXIT
                                             (COND
                                               ((EQL (CAR |rho|) 0)
                                                (SEQ
                                                 (LETT |found| T
                                                  |FFIELDC-;discreteLog;SNni;11|)
                                                 (EXIT
                                                  (LETT |disc1|
                                                   (*
                                                    (+ (* |n| |i|)
                                                     (CDR |rho|))
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
                                                   77))
                                                 |FFIELDC-;discreteLog;SNni;11|)))))
                                           (SETQ |i| (QSADD1 |i|))
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
                                                   77))
                                                 |FFIELDC-;discreteLog;SNni;11|))))
                                             ('T
                                              (|error|
                                               "discreteLog: ?? discrete logarithm")))))
                                         (SETQ |t| (QSADD1 |t|))
                                         (GO G190) G191 (EXIT NIL))))
                                      (SETQ #0# (CDR #0#)) (GO G190)
                                      G191 (EXIT NIL))
                                 (EXIT |disclog|)))))))))))) 

(DEFUN |FFIELDC-;discreteLog;2SU;12| (|logbase| |b| $)
  (PROG (|groupord| |faclist| |f| #0=#:G1518 |fac| |primroot| |t|
            #1=#:G1519 |exp| |rhoHelp| |rho| |disclog| |mult| |a|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |b| (|getShellEntry| $ 16))
              (SEQ (SPADCALL "discreteLog: logarithm of zero"
                       (|getShellEntry| $ 83))
                   (EXIT (CONS 1 "failed"))))
             ((SPADCALL |logbase| (|getShellEntry| $ 16))
              (SEQ (SPADCALL "discreteLog: logarithm to base zero"
                       (|getShellEntry| $ 83))
                   (EXIT (CONS 1 "failed"))))
             ((SPADCALL |b| |logbase| (|getShellEntry| $ 63))
              (CONS 0 1))
             ('T
              (COND
                ((NOT (ZEROP (REMAINDER2
                                 (LETT |groupord|
                                       (SPADCALL |logbase|
                                        (|getShellEntry| $ 19))
                                       |FFIELDC-;discreteLog;2SU;12|)
                                 (SPADCALL |b| (|getShellEntry| $ 19)))))
                 (SEQ (SPADCALL
                          "discreteLog: second argument not in cyclic group generated by first argument"
                          (|getShellEntry| $ 83))
                      (EXIT (CONS 1 "failed"))))
                ('T
                 (SEQ (LETT |faclist|
                            (SPADCALL
                                (SPADCALL |groupord|
                                    (|getShellEntry| $ 87))
                                (|getShellEntry| $ 89))
                            |FFIELDC-;discreteLog;2SU;12|)
                      (LETT |a| |b| |FFIELDC-;discreteLog;2SU;12|)
                      (LETT |disclog| 0 |FFIELDC-;discreteLog;2SU;12|)
                      (LETT |mult| 1 |FFIELDC-;discreteLog;2SU;12|)
                      (LETT |exp| |groupord|
                            |FFIELDC-;discreteLog;2SU;12|)
                      (SEQ (LETT |f| NIL |FFIELDC-;discreteLog;2SU;12|)
                           (LETT #0# |faclist|
                                 |FFIELDC-;discreteLog;2SU;12|)
                           G190
                           (COND
                             ((OR (ATOM #0#)
                                  (PROGN (SETQ |f| (CAR #0#)) NIL))
                              (GO G191)))
                           (SEQ (LETT |fac| (CAR |f|)
                                      |FFIELDC-;discreteLog;2SU;12|)
                                (LETT |primroot|
                                      (SPADCALL |logbase|
                                       (QUOTIENT2 |groupord| |fac|)
                                       (|getShellEntry| $ 58))
                                      |FFIELDC-;discreteLog;2SU;12|)
                                (EXIT (SEQ
                                       (LETT |t| 0
                                        |FFIELDC-;discreteLog;2SU;12|)
                                       (LETT #1# (- (CDR |f|) 1)
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
                                          |fac| (|getShellEntry| $ 91))
                                         |FFIELDC-;discreteLog;2SU;12|)
                                        (EXIT
                                         (COND
                                           ((EQL (CAR |rhoHelp|) 1)
                                            (RETURN-FROM
                                             |FFIELDC-;discreteLog;2SU;12|
                                              (CONS 1 "failed")))
                                           ('T
                                            (SEQ
                                             (LETT |rho|
                                              (* (CDR |rhoHelp|)
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
                                                 (|getShellEntry| $ 58))
                                                (|getShellEntry| $ 77))
                                               |FFIELDC-;discreteLog;2SU;12|)))))))
                                       (SETQ |t| (QSADD1 |t|))
                                       (GO G190) G191 (EXIT NIL))))
                           (SETQ #0# (CDR #0#)) (GO G190) G191
                           (EXIT NIL))
                      (EXIT (CONS 0 |disclog|))))))))))) 

(DEFUN |FFIELDC-;squareFreePolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 96))) 

(DEFUN |FFIELDC-;factorPolynomial| (|f| $)
  (SPADCALL |f| (|getShellEntry| $ 98))) 

(DEFUN |FFIELDC-;factorSquareFreePolynomial| (|f| $)
  (PROG (|flist| |u| #0=#:G1520 #1=#:G1509 #2=#:G1507 #3=#:G1508)
    (RETURN
      (SEQ (COND
             ((SPADCALL |f| (|spadConstant| $ 99)
                  (|getShellEntry| $ 100))
              (|spadConstant| $ 101))
             ('T
              (SEQ (LETT |flist|
                         (SPADCALL |f| T (|getShellEntry| $ 105))
                         |FFIELDC-;factorSquareFreePolynomial|)
                   (EXIT (SPADCALL
                             (SPADCALL (CAR |flist|)
                                 (|getShellEntry| $ 106))
                             (PROGN
                               (LETT #3# NIL
                                     |FFIELDC-;factorSquareFreePolynomial|)
                               (SEQ (LETT |u| NIL
                                     |FFIELDC-;factorSquareFreePolynomial|)
                                    (LETT #0# (CDR |flist|)
                                     |FFIELDC-;factorSquareFreePolynomial|)
                                    G190
                                    (COND
                                      ((OR (ATOM #0#)
                                        (PROGN
                                          (SETQ |u| (CAR #0#))
                                          NIL))
                                       (GO G191)))
                                    (PROGN
                                      (LETT #1#
                                       (SPADCALL (CAR |u|) (CDR |u|)
                                        (|getShellEntry| $ 107))
                                       |FFIELDC-;factorSquareFreePolynomial|)
                                      (COND
                                        (#3#
                                         (LETT #2#
                                          (SPADCALL #2# #1#
                                           (|getShellEntry| $ 108))
                                          |FFIELDC-;factorSquareFreePolynomial|))
                                        ('T
                                         (PROGN
                                           (LETT #2# #1#
                                            |FFIELDC-;factorSquareFreePolynomial|)
                                           (LETT #3# 'T
                                            |FFIELDC-;factorSquareFreePolynomial|)))))
                                    (SETQ #0# (CDR #0#)) (GO G190) G191
                                    (EXIT NIL))
                               (COND
                                 (#3# #2#)
                                 ('T (|spadConstant| $ 109))))
                             (|getShellEntry| $ 110)))))))))) 

(DEFUN |FFIELDC-;gcdPolynomial;3Sup;16| (|f| |g| $)
  (SPADCALL |f| |g| (|getShellEntry| $ 112))) 

(DEFUN |FiniteFieldCategory&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|FiniteFieldCategory&| |dv$1|))
         ($ (|newShell| 115)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

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
             (91 . -)
             (|Union| '"prime" '"polynomial" '"normal" '"cyclic")
             (97 . |representationType|) (101 . =) (107 . |false|)
             (111 . |not|) (116 . =)
             |FFIELDC-;createPrimitiveElement;S;8|
             (|Record| (|:| |factor| 20) (|:| |exponent| 20))
             (|List| 54) (122 . |factorsOfCyclicGroupSize|)
             (126 . |quo|) (132 . **) (138 . |one?|)
             |FFIELDC-;primitive?;SB;9| (143 . |Zero|) (147 . |Zero|)
             (151 . =) |FFIELDC-;order;SPi;10|
             (157 . |primitiveElement|) (|Table| 10 39)
             (161 . |tableForDiscreteLogarithm|) (166 . |#|)
             (171 . |One|) (|Union| 39 '"failed") (175 . |search|)
             (181 . |true|) (185 . *) (191 . +) (197 . *) (203 . -)
             (208 . *) (214 . +) |FFIELDC-;discreteLog;SNni;11|
             (|Void|) (|String|) (|OutputForm|) (220 . |messagePrint|)
             (225 . |rem|) (231 . |zero?|) (|Factored| $)
             (236 . |factor|) (|Factored| 20) (241 . |factors|)
             (|DiscreteLogarithmPackage| 6)
             (246 . |shanksDiscLogAlgorithm|)
             |FFIELDC-;discreteLog;2SU;12|
             (|SparseUnivariatePolynomial| 6) (|Factored| 93)
             (|UnivariatePolynomialSquareFree| 6 93)
             (253 . |squareFree|) (|DistinctDegreeFactorize| 6 93)
             (258 . |factor|) (263 . |Zero|) (267 . =) (273 . |Zero|)
             (|Record| (|:| |irr| 93) (|:| |pow| 20)) (|List| 102)
             (|Record| (|:| |cont| 6) (|:| |factors| 103))
             (277 . |distdfact|) (283 . |coerce|) (288 . |primeFactor|)
             (294 . *) (300 . |One|) (304 . *) (|EuclideanDomain&| 93)
             (310 . |gcd|) (|SparseUnivariatePolynomial| $)
             |FFIELDC-;gcdPolynomial;3Sup;16|)
          '#(|primitive?| 316 |order| 321 |nextItem| 331 |init| 336
             |gcdPolynomial| 340 |discreteLog| 346 |differentiate| 357
             |createPrimitiveElement| 362 |conditionP| 366 |charthRoot|
             371)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 114
                                '(0 6 0 7 1 6 10 0 11 0 10 0 12 2 10 0
                                  0 0 13 1 6 0 10 14 1 6 15 0 16 1 6 10
                                  0 19 1 21 0 20 22 1 26 25 0 27 1 25
                                  15 0 28 1 25 24 0 29 2 24 15 30 0 31
                                  1 6 0 0 32 2 24 0 33 0 34 0 6 39 40 0
                                  6 39 41 2 39 0 0 0 42 2 6 0 0 39 43 2
                                  20 0 0 0 46 0 6 47 48 2 47 15 0 0 49
                                  0 15 0 50 1 15 0 0 51 2 10 15 0 0 52
                                  0 6 55 56 2 20 0 0 0 57 2 6 0 0 20 58
                                  1 6 15 0 59 0 39 0 61 0 20 0 62 2 6
                                  15 0 0 63 0 6 0 65 1 6 66 20 67 1 66
                                  39 0 68 0 39 0 69 2 66 70 10 0 71 0
                                  15 0 72 2 39 0 39 0 73 2 39 0 0 0 74
                                  2 20 0 20 0 75 1 20 0 0 76 2 6 0 0 0
                                  77 2 20 0 0 0 78 1 82 80 81 83 2 39 0
                                  0 0 84 1 39 15 0 85 1 20 86 0 87 1 88
                                  55 0 89 3 90 70 6 6 39 91 1 95 94 93
                                  96 1 97 94 93 98 0 93 0 99 2 93 15 0
                                  0 100 0 94 0 101 2 97 104 93 15 105 1
                                  93 0 6 106 2 94 0 93 20 107 2 94 0 0
                                  0 108 0 94 0 109 2 94 0 93 0 110 2
                                  111 0 0 0 112 1 0 15 0 60 1 0 10 0 64
                                  1 0 21 0 23 1 0 17 0 18 0 0 0 9 2 0
                                  113 113 113 114 1 0 39 0 79 2 0 70 0
                                  0 92 1 0 0 0 8 0 0 0 53 1 0 36 37 38
                                  1 0 0 0 44 1 0 17 0 45)))))
          '|lookupComplete|)) 
