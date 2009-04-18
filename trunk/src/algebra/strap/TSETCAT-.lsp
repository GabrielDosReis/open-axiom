
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;=;2SB;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;infRittWu?;2SB;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Boolean|)
                |TSETCAT-;reduced?;PSMB;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;basicSet;LMU;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;basicSet;LMMU;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |TSETCAT-;initials;SL;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |TSETCAT-;degree;SNni;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;quasiComponent;SR;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;normalized?;PSB;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;stronglyReduced?;PSB;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;headReduced?;PSB;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;initiallyReduced?;PSB;12|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |TSETCAT-;reduce;PSMMP;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Thing| |%Thing| |%Shell|)
                    |%List|)
                |TSETCAT-;rewriteSetWithReduction;LSMML;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;stronglyReduce;PSP;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;headReduce;PSP;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;initiallyReduce;PSP;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;removeZero;PSP;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;reduceByQuasiMonic;PSP;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;autoReduced?;SMB;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;stronglyReduced?;SB;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;normalized?;SB;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;headReduced?;SB;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;initiallyReduced?;SB;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;mvar;SV;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;first;SU;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;last;SU;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;rest;SU;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |TSETCAT-;coerce;SL;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |TSETCAT-;algebraicVariables;SL;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |TSETCAT-;algebraic?;VSB;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |TSETCAT-;select;SVU;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;collectQuasiMonic;2S;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;collectUnder;SVS;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;collectUpper;SVS;35|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |TSETCAT-;construct;LS;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Pair|)
                |TSETCAT-;retractIfCan;LU;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |TSETCAT-;extend;SPS;38|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |TSETCAT-;coHeight;SNni;39|)) 

(DEFUN |TSETCAT-;=;2SB;1| (|ts| |us| $)
  (PROG (#0=#:G1457 #1=#:G1463)
    (RETURN
      (COND
        ((SPADCALL |ts| (|getShellEntry| $ 12))
         (SPADCALL |us| (|getShellEntry| $ 12)))
        ((OR (SPADCALL |us| (|getShellEntry| $ 12))
             (NOT (SPADCALL
                      (PROG2 (LETT #0#
                                   (SPADCALL |ts|
                                    (|getShellEntry| $ 15))
                                   |TSETCAT-;=;2SB;1|)
                             (QCDR #0#)
                        (|check-union| (QEQCAR #0# 0)
                            (|getShellEntry| $ 10) #0#))
                      (PROG2 (LETT #0#
                                   (SPADCALL |us|
                                    (|getShellEntry| $ 15))
                                   |TSETCAT-;=;2SB;1|)
                             (QCDR #0#)
                        (|check-union| (QEQCAR #0# 0)
                            (|getShellEntry| $ 10) #0#))
                      (|getShellEntry| $ 16))))
         'NIL)
        ('T
         (SPADCALL
             (PROG2 (LETT #1# (SPADCALL |ts| (|getShellEntry| $ 18))
                          |TSETCAT-;=;2SB;1|)
                    (QCDR #1#)
               (|check-union| (QEQCAR #1# 0) (|getShellEntry| $ 6) #1#))
             (PROG2 (LETT #1# (SPADCALL |us| (|getShellEntry| $ 18))
                          |TSETCAT-;=;2SB;1|)
                    (QCDR #1#)
               (|check-union| (QEQCAR #1# 0) (|getShellEntry| $ 6) #1#))
             (|getShellEntry| $ 19))))))) 

(DEFUN |TSETCAT-;infRittWu?;2SB;2| (|ts| |us| $)
  (PROG (|p| #0=#:G1470 |q| |v|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |us| (|getShellEntry| $ 12))
              (NOT (SPADCALL |ts| (|getShellEntry| $ 12))))
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 'NIL)
             ('T
              (SEQ (LETT |p|
                         (PROG2 (LETT #0#
                                      (SPADCALL |ts|
                                       (|getShellEntry| $ 21))
                                      |TSETCAT-;infRittWu?;2SB;2|)
                                (QCDR #0#)
                           (|check-union| (QEQCAR #0# 0)
                               (|getShellEntry| $ 10) #0#))
                         |TSETCAT-;infRittWu?;2SB;2|)
                   (LETT |q|
                         (PROG2 (LETT #0#
                                      (SPADCALL |us|
                                       (|getShellEntry| $ 21))
                                      |TSETCAT-;infRittWu?;2SB;2|)
                                (QCDR #0#)
                           (|check-union| (QEQCAR #0# 0)
                               (|getShellEntry| $ 10) #0#))
                         |TSETCAT-;infRittWu?;2SB;2|)
                   (EXIT (COND
                           ((SPADCALL |p| |q| (|getShellEntry| $ 22))
                            'T)
                           ((SPADCALL |p| |q| (|getShellEntry| $ 24))
                            'NIL)
                           ('T
                            (SEQ (LETT |v|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 25))
                                       |TSETCAT-;infRittWu?;2SB;2|)
                                 (EXIT (SPADCALL
                                        (SPADCALL |ts| |v|
                                         (|getShellEntry| $ 26))
                                        (SPADCALL |us| |v|
                                         (|getShellEntry| $ 26))
                                        (|getShellEntry| $ 27)))))))))))))) 

(DEFUN |TSETCAT-;reduced?;PSMB;3| (|p| |ts| |redOp?| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 30))
                 |TSETCAT-;reduced?;PSMB;3|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (SPADCALL |p| (|SPADfirst| |lp|) |redOp?|))))
                   (GO G191)))
                (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;reduced?;PSMB;3|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (NULL |lp|)))))) 

(DEFUN |TSETCAT-;basicSet;LMU;4| (|ps| |redOp?| $)
  (PROG (|b| |bs| |p| |ts|)
    (RETURN
      (SEQ (LETT |ps| (SPADCALL (ELT $ 36) |ps| (|getShellEntry| $ 38))
                 |TSETCAT-;basicSet;LMU;4|)
           (EXIT (COND
                   ((SPADCALL (ELT $ 39) |ps| (|getShellEntry| $ 40))
                    (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |ps|
                               (SPADCALL (ELT $ 22) |ps|
                                   (|getShellEntry| $ 41))
                               |TSETCAT-;basicSet;LMU;4|)
                         (LETT |bs| (SPADCALL (|getShellEntry| $ 42))
                               |TSETCAT-;basicSet;LMU;4|)
                         (LETT |ts| NIL |TSETCAT-;basicSet;LMU;4|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |ps|))) (GO G191)))
                              (SEQ (LETT |b| (|SPADfirst| |ps|)
                                    |TSETCAT-;basicSet;LMU;4|)
                                   (LETT |bs|
                                    (SPADCALL |bs| |b|
                                     (|getShellEntry| $ 43))
                                    |TSETCAT-;basicSet;LMU;4|)
                                   (LETT |ps| (CDR |ps|)
                                    |TSETCAT-;basicSet;LMU;4|)
                                   (EXIT
                                    (SEQ G190
                                     (COND
                                       ((NULL
                                         (COND
                                           ((NULL |ps|) 'NIL)
                                           ('T
                                            (NOT
                                             (SPADCALL
                                              (LETT |p|
                                               (|SPADfirst| |ps|)
                                               |TSETCAT-;basicSet;LMU;4|)
                                              |bs| |redOp?|
                                              (|getShellEntry| $ 44))))))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |ts| (CONS |p| |ts|)
                                       |TSETCAT-;basicSet;LMU;4|)
                                      (EXIT
                                       (LETT |ps| (CDR |ps|)
                                        |TSETCAT-;basicSet;LMU;4|)))
                                     NIL (GO G190) G191 (EXIT NIL))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (CONS 0 (CONS |bs| |ts|))))))))))) 

(DEFUN |TSETCAT-;basicSet;LMMU;5| (|ps| |pred?| |redOp?| $)
  (PROG (|bps| |b| |bs| |p| |gps| |ts|)
    (RETURN
      (SEQ (LETT |ps| (SPADCALL (ELT $ 36) |ps| (|getShellEntry| $ 38))
                 |TSETCAT-;basicSet;LMMU;5|)
           (EXIT (COND
                   ((SPADCALL (ELT $ 39) |ps| (|getShellEntry| $ 40))
                    (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |gps| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |bps| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |ps|))) (GO G191)))
                              (SEQ (LETT |p| (|SPADfirst| |ps|)
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (LETT |ps| (CDR |ps|)
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (EXIT
                                    (COND
                                      ((SPADCALL |p| |pred?|)
                                       (LETT |gps| (CONS |p| |gps|)
                                        |TSETCAT-;basicSet;LMMU;5|))
                                      ('T
                                       (LETT |bps| (CONS |p| |bps|)
                                        |TSETCAT-;basicSet;LMMU;5|)))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (LETT |gps|
                               (SPADCALL (ELT $ 22) |gps|
                                   (|getShellEntry| $ 41))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |bs| (SPADCALL (|getShellEntry| $ 42))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |ts| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |gps|))) (GO G191)))
                              (SEQ (LETT |b| (|SPADfirst| |gps|)
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (LETT |bs|
                                    (SPADCALL |bs| |b|
                                     (|getShellEntry| $ 43))
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (LETT |gps| (CDR |gps|)
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (EXIT
                                    (SEQ G190
                                     (COND
                                       ((NULL
                                         (COND
                                           ((NULL |gps|) 'NIL)
                                           ('T
                                            (NOT
                                             (SPADCALL
                                              (LETT |p|
                                               (|SPADfirst| |gps|)
                                               |TSETCAT-;basicSet;LMMU;5|)
                                              |bs| |redOp?|
                                              (|getShellEntry| $ 44))))))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |ts| (CONS |p| |ts|)
                                       |TSETCAT-;basicSet;LMMU;5|)
                                      (EXIT
                                       (LETT |gps| (CDR |gps|)
                                        |TSETCAT-;basicSet;LMMU;5|)))
                                     NIL (GO G190) G191 (EXIT NIL))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (LETT |ts|
                               (SPADCALL (ELT $ 22)
                                   (SPADCALL |ts| |bps|
                                    (|getShellEntry| $ 49))
                                   (|getShellEntry| $ 41))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (EXIT (CONS 0 (CONS |bs| |ts|))))))))))) 

(DEFUN |TSETCAT-;initials;SL;6| (|ts| $)
  (PROG (|p| |ip| |lip| |lp|)
    (RETURN
      (SEQ (LETT |lip| NIL |TSETCAT-;initials;SL;6|)
           (EXIT (COND
                   ((SPADCALL |ts| (|getShellEntry| $ 12)) |lip|)
                   ('T
                    (SEQ (LETT |lp|
                               (SPADCALL |ts| (|getShellEntry| $ 30))
                               |TSETCAT-;initials;SL;6|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |lp|))) (GO G191)))
                              (SEQ (LETT |p| (|SPADfirst| |lp|)
                                    |TSETCAT-;initials;SL;6|)
                                   (COND
                                     ((NOT
                                       (SPADCALL
                                        (LETT |ip|
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 51))
                                         |TSETCAT-;initials;SL;6|)
                                        (|getShellEntry| $ 39)))
                                      (LETT |lip|
                                       (CONS
                                        (SPADCALL |ip|
                                         (|getShellEntry| $ 52))
                                        |lip|)
                                       |TSETCAT-;initials;SL;6|)))
                                   (EXIT
                                    (LETT |lp| (CDR |lp|)
                                     |TSETCAT-;initials;SL;6|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL |lip| (|getShellEntry| $ 53))))))))))) 

(DEFUN |TSETCAT-;degree;SNni;7| (|ts| $)
  (PROG (|lp| |d|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 0)
             ('T
              (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 30))
                         |TSETCAT-;degree;SNni;7|)
                   (LETT |d|
                         (SPADCALL (|SPADfirst| |lp|)
                             (|getShellEntry| $ 57))
                         |TSETCAT-;degree;SNni;7|)
                   (SEQ G190
                        (COND
                          ((NULL (NOT (NULL
                                       (LETT |lp| (CDR |lp|)
                                        |TSETCAT-;degree;SNni;7|))))
                           (GO G191)))
                        (SEQ (EXIT (LETT |d|
                                    (* |d|
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 57)))
                                    |TSETCAT-;degree;SNni;7|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |TSETCAT-;quasiComponent;SR;8| (|ts| $)
  (CONS (SPADCALL |ts| (|getShellEntry| $ 30))
        (SPADCALL |ts| (|getShellEntry| $ 60)))) 

(DEFUN |TSETCAT-;normalized?;PSB;9| (|p| |ts| $)
  (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 30))
      (|getShellEntry| $ 64))) 

(DEFUN |TSETCAT-;stronglyReduced?;PSB;10| (|p| |ts| $)
  (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 30))
      (|getShellEntry| $ 66))) 

(DEFUN |TSETCAT-;headReduced?;PSB;11| (|p| |ts| $)
  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 68)) |ts|
      (|getShellEntry| $ 69))) 

(DEFUN |TSETCAT-;initiallyReduced?;PSB;12| (|p| |ts| $)
  (PROG (|lp| |red|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 30))
                 |TSETCAT-;initiallyReduced?;PSB;12|)
           (LETT |red| 'T |TSETCAT-;initiallyReduced?;PSB;12|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((OR (NULL |lp|)
                                (SPADCALL |p| (|getShellEntry| $ 39)))
                            'NIL)
                           ('T |red|)))
                   (GO G191)))
                (SEQ (SEQ G190
                          (COND
                            ((NULL (COND
                                     ((NULL |lp|) 'NIL)
                                     ('T
                                      (SPADCALL
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 25))
                                       (SPADCALL (|SPADfirst| |lp|)
                                        (|getShellEntry| $ 25))
                                       (|getShellEntry| $ 71)))))
                             (GO G191)))
                          (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                      |TSETCAT-;initiallyReduced?;PSB;12|)))
                          NIL (GO G190) G191 (EXIT NIL))
                     (EXIT (COND
                             ((NOT (NULL |lp|))
                              (COND
                                ((SPADCALL
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 25))
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 25))
                                     (|getShellEntry| $ 72))
                                 (COND
                                   ((SPADCALL |p| (|SPADfirst| |lp|)
                                     (|getShellEntry| $ 73))
                                    (SEQ
                                     (LETT |lp| (CDR |lp|)
                                      |TSETCAT-;initiallyReduced?;PSB;12|)
                                     (EXIT
                                      (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 51))
                                       |TSETCAT-;initiallyReduced?;PSB;12|))))
                                   ('T
                                    (LETT |red| 'NIL
                                     |TSETCAT-;initiallyReduced?;PSB;12|))))
                                ('T
                                 (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 51))
                                       |TSETCAT-;initiallyReduced?;PSB;12|)))))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |red|))))) 

(DEFUN |TSETCAT-;reduce;PSMMP;13| (|p| |ts| |redOp| |redOp?| $)
  (PROG (|ts0| #0=#:G1545 |reductor| #1=#:G1548)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |ts| (|getShellEntry| $ 12))
                  (SPADCALL |p| (|getShellEntry| $ 39)))
              |p|)
             ('T
              (SEQ (LETT |ts0| |ts| |TSETCAT-;reduce;PSMMP;13|)
                   (SEQ G190
                        (COND
                          ((NULL (COND
                                   ((SPADCALL |ts|
                                     (|getShellEntry| $ 12))
                                    'NIL)
                                   ('T
                                    (NOT
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 39))))))
                           (GO G191)))
                        (SEQ (LETT |reductor|
                                   (PROG2
                                    (LETT #0#
                                     (SPADCALL |ts|
                                      (|getShellEntry| $ 15))
                                     |TSETCAT-;reduce;PSMMP;13|)
                                    (QCDR #0#)
                                     (|check-union| (QEQCAR #0# 0)
                                      (|getShellEntry| $ 10) #0#))
                                   |TSETCAT-;reduce;PSMMP;13|)
                             (LETT |ts|
                                   (PROG2
                                    (LETT #1#
                                     (SPADCALL |ts|
                                      (|getShellEntry| $ 18))
                                     |TSETCAT-;reduce;PSMMP;13|)
                                    (QCDR #1#)
                                     (|check-union| (QEQCAR #1# 0)
                                      (|getShellEntry| $ 6) #1#))
                                   |TSETCAT-;reduce;PSMMP;13|)
                             (EXIT (COND
                                     ((NOT
                                       (SPADCALL |p| |reductor|
                                        |redOp?|))
                                      (SEQ
                                       (LETT |p|
                                        (SPADCALL |p| |reductor|
                                         |redOp|)
                                        |TSETCAT-;reduce;PSMMP;13|)
                                       (EXIT
                                        (LETT |ts| |ts0|
                                         |TSETCAT-;reduce;PSMMP;13|)))))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |p|)))))))) 

(DEFUN |TSETCAT-;rewriteSetWithReduction;LSMML;14|
       (|lp| |ts| |redOp| |redOp?| $)
  (PROG (|p| |rs|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 77)) |lp|)
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 36) |lp|
                             (|getShellEntry| $ 38))
                         |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                   (EXIT (COND
                           ((NULL |lp|) |lp|)
                           ((SPADCALL (ELT $ 39) |lp|
                                (|getShellEntry| $ 40))
                            (LIST (|spadConstant| $ 78)))
                           ('T
                            (SEQ (LETT |rs| NIL
                                       |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                 (SEQ G190
                                      (COND
                                        ((NULL (NOT (NULL |lp|)))
                                         (GO G191)))
                                      (SEQ
                                       (LETT |p| (|SPADfirst| |lp|)
                                        |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                       (LETT |lp| (CDR |lp|)
                                        |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                       (LETT |p|
                                        (SPADCALL
                                         (SPADCALL |p| |ts| |redOp|
                                          |redOp?|
                                          (|getShellEntry| $ 79))
                                         (|getShellEntry| $ 52))
                                        |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                       (EXIT
                                        (COND
                                          ((NOT
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 36)))
                                           (COND
                                             ((SPADCALL |p|
                                               (|getShellEntry| $ 39))
                                              (SEQ
                                               (LETT |lp| NIL
                                                |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                               (EXIT
                                                (LETT |rs|
                                                 (LIST
                                                  (|spadConstant| $ 78))
                                                 |TSETCAT-;rewriteSetWithReduction;LSMML;14|))))
                                             ('T
                                              (LETT |rs|
                                               (CONS |p| |rs|)
                                               |TSETCAT-;rewriteSetWithReduction;LSMML;14|)))))))
                                      NIL (GO G190) G191 (EXIT NIL))
                                 (EXIT (SPADCALL |rs|
                                        (|getShellEntry| $ 53)))))))))))))) 

(DEFUN |TSETCAT-;stronglyReduce;PSP;15| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 81) (ELT $ 73) (|getShellEntry| $ 79))) 

(DEFUN |TSETCAT-;headReduce;PSP;16| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 83) (ELT $ 84) (|getShellEntry| $ 79))) 

(DEFUN |TSETCAT-;initiallyReduce;PSP;17| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 86) (ELT $ 87) (|getShellEntry| $ 79))) 

(DEFUN |TSETCAT-;removeZero;PSP;18| (|p| |ts| $)
  (PROG (|v| |tsv-| #0=#:G1571 #1=#:G1580 |q|)
    (RETURN
      (SEQ (EXIT (COND
                   ((OR (SPADCALL |p| (|getShellEntry| $ 39))
                        (SPADCALL |ts| (|getShellEntry| $ 12)))
                    |p|)
                   ('T
                    (SEQ (LETT |v|
                               (SPADCALL |p| (|getShellEntry| $ 25))
                               |TSETCAT-;removeZero;PSP;18|)
                         (LETT |tsv-|
                               (SPADCALL |ts| |v|
                                   (|getShellEntry| $ 89))
                               |TSETCAT-;removeZero;PSP;18|)
                         (COND
                           ((SPADCALL |v| |ts| (|getShellEntry| $ 90))
                            (SEQ (LETT |q|
                                       (SPADCALL |p|
                                        (PROG2
                                         (LETT #0#
                                          (SPADCALL |ts| |v|
                                           (|getShellEntry| $ 91))
                                          |TSETCAT-;removeZero;PSP;18|)
                                         (QCDR #0#)
                                          (|check-union| (QEQCAR #0# 0)
                                           (|getShellEntry| $ 10) #0#))
                                        (|getShellEntry| $ 81))
                                       |TSETCAT-;removeZero;PSP;18|)
                                 (EXIT (COND
                                         ((SPADCALL |q|
                                           (|getShellEntry| $ 36))
                                          (PROGN
                                            (LETT #1# |q|
                                             |TSETCAT-;removeZero;PSP;18|)
                                            (GO #1#)))
                                         ((SPADCALL
                                           (SPADCALL |q| |tsv-|
                                            (|getShellEntry| $ 92))
                                           (|getShellEntry| $ 36))
                                          (PROGN
                                            (LETT #1#
                                             (|spadConstant| $ 93)
                                             |TSETCAT-;removeZero;PSP;18|)
                                            (GO #1#))))))))
                         (EXIT (COND
                                 ((SPADCALL |tsv-|
                                      (|getShellEntry| $ 12))
                                  |p|)
                                 ('T
                                  (SEQ (LETT |q| (|spadConstant| $ 93)
                                        |TSETCAT-;removeZero;PSP;18|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (SPADCALL
                                             (SPADCALL |p| |v|
                                              (|getShellEntry| $ 94))
                                             (|getShellEntry| $ 96)))
                                           (GO G191)))
                                        (SEQ
                                         (LETT |q|
                                          (SPADCALL
                                           (SPADCALL
                                            (SPADCALL
                                             (SPADCALL |p|
                                              (|getShellEntry| $ 51))
                                             |tsv-|
                                             (|getShellEntry| $ 92))
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 97))
                                            (|getShellEntry| $ 98))
                                           |q| (|getShellEntry| $ 99))
                                          |TSETCAT-;removeZero;PSP;18|)
                                         (EXIT
                                          (LETT |p|
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 100))
                                           |TSETCAT-;removeZero;PSP;18|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT
                                        (SPADCALL |q|
                                         (SPADCALL |p| |tsv-|
                                          (|getShellEntry| $ 92))
                                         (|getShellEntry| $ 99)))))))))))
           #1# (EXIT #1#))))) 

(DEFUN |TSETCAT-;reduceByQuasiMonic;PSP;19| (|p| |ts| $)
  (COND
    ((OR (SPADCALL |p| (|getShellEntry| $ 39))
         (SPADCALL |ts| (|getShellEntry| $ 12)))
     |p|)
    ('T
     (QVELT (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 102))
                (|getShellEntry| $ 104))
            1)))) 

(DEFUN |TSETCAT-;autoReduced?;SMB;20| (|ts| |redOp?| $)
  (PROG (|p| |lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 'T)
             ('T
              (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 30))
                         |TSETCAT-;autoReduced?;SMB;20|)
                   (LETT |p| (|SPADfirst| |lp|)
                         |TSETCAT-;autoReduced?;SMB;20|)
                   (LETT |lp| (CDR |lp|)
                         |TSETCAT-;autoReduced?;SMB;20|)
                   (SEQ G190
                        (COND
                          ((NULL (COND
                                   ((NULL |lp|) 'NIL)
                                   ('T (SPADCALL |p| |lp| |redOp?|))))
                           (GO G191)))
                        (SEQ (LETT |p| (|SPADfirst| |lp|)
                                   |TSETCAT-;autoReduced?;SMB;20|)
                             (EXIT (LETT |lp| (CDR |lp|)
                                    |TSETCAT-;autoReduced?;SMB;20|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT (NULL |lp|))))))))) 

(DEFUN |TSETCAT-;stronglyReduced?;SB;21| (|ts| $)
  (SPADCALL |ts| (ELT $ 66) (|getShellEntry| $ 108))) 

(DEFUN |TSETCAT-;normalized?;SB;22| (|ts| $)
  (SPADCALL |ts| (ELT $ 64) (|getShellEntry| $ 108))) 

(DEFUN |TSETCAT-;headReduced?;SB;23| (|ts| $)
  (SPADCALL |ts| (ELT $ 111) (|getShellEntry| $ 108))) 

(DEFUN |TSETCAT-;initiallyReduced?;SB;24| (|ts| $)
  (SPADCALL |ts| (ELT $ 113) (|getShellEntry| $ 108))) 

(DEFUN |TSETCAT-;mvar;SV;25| (|ts| $)
  (PROG (#0=#:G1599)
    (RETURN
      (COND
        ((SPADCALL |ts| (|getShellEntry| $ 12))
         (|error| "Error from TSETCAT in mvar : #1 is empty"))
        ('T
         (SPADCALL
             (PROG2 (LETT #0# (SPADCALL |ts| (|getShellEntry| $ 15))
                          |TSETCAT-;mvar;SV;25|)
                    (QCDR #0#)
               (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 10)
                   #0#))
             (|getShellEntry| $ 25))))))) 

(DEFUN |TSETCAT-;first;SU;26| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 24)
                             (SPADCALL |ts| (|getShellEntry| $ 30))
                             (|getShellEntry| $ 41))
                         |TSETCAT-;first;SU;26|)
                   (EXIT (CONS 0 (|SPADfirst| |lp|)))))))))) 

(DEFUN |TSETCAT-;last;SU;27| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 22)
                             (SPADCALL |ts| (|getShellEntry| $ 30))
                             (|getShellEntry| $ 41))
                         |TSETCAT-;last;SU;27|)
                   (EXIT (CONS 0 (|SPADfirst| |lp|)))))))))) 

(DEFUN |TSETCAT-;rest;SU;28| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 24)
                             (SPADCALL |ts| (|getShellEntry| $ 30))
                             (|getShellEntry| $ 41))
                         |TSETCAT-;rest;SU;28|)
                   (EXIT (CONS 0
                               (SPADCALL (CDR |lp|)
                                   (|getShellEntry| $ 118))))))))))) 

(DEFUN |TSETCAT-;coerce;SL;29| (|ts| $)
  (SPADCALL (ELT $ 24) (SPADCALL |ts| (|getShellEntry| $ 30))
            (|getShellEntry| $ 41))) 

(DEFUN |TSETCAT-;algebraicVariables;SL;30| (|ts| $)
  (PROG (#0=#:G1667 |p| #1=#:G1668)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |TSETCAT-;algebraicVariables;SL;30|)
             (SEQ (LETT |p| NIL |TSETCAT-;algebraicVariables;SL;30|)
                  (LETT #1# (SPADCALL |ts| (|getShellEntry| $ 30))
                        |TSETCAT-;algebraicVariables;SL;30|)
                  G190
                  (COND
                    ((OR (ATOM #1#)
                         (PROGN
                           (LETT |p| (CAR #1#)
                                 |TSETCAT-;algebraicVariables;SL;30|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (LETT #0#
                                   (CONS
                                    (SPADCALL |p|
                                     (|getShellEntry| $ 25))
                                    #0#)
                                   |TSETCAT-;algebraicVariables;SL;30|)))
                  (LETT #1# (CDR #1#)
                        |TSETCAT-;algebraicVariables;SL;30|)
                  (GO G190) G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |TSETCAT-;algebraic?;VSB;31| (|v| |ts| $)
  (SPADCALL |v| (SPADCALL |ts| (|getShellEntry| $ 123))
      (|getShellEntry| $ 124))) 

(DEFUN |TSETCAT-;select;SVU;32| (|ts| |v| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 24)
                     (SPADCALL |ts| (|getShellEntry| $ 30))
                     (|getShellEntry| $ 41))
                 |TSETCAT-;select;SVU;32|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (NOT (SPADCALL |v|
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 25))
                                     (|getShellEntry| $ 72))))))
                   (GO G191)))
                (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;select;SVU;32|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((NULL |lp|) (CONS 1 "failed"))
                   ('T (CONS 0 (|SPADfirst| |lp|))))))))) 

(DEFUN |TSETCAT-;collectQuasiMonic;2S;33| (|ts| $)
  (PROG (|newlp| |lp|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 30))
                 |TSETCAT-;collectQuasiMonic;2S;33|)
           (LETT |newlp| NIL |TSETCAT-;collectQuasiMonic;2S;33|)
           (SEQ G190 (COND ((NULL (NOT (NULL |lp|))) (GO G191)))
                (SEQ (COND
                       ((SPADCALL
                            (SPADCALL (|SPADfirst| |lp|)
                                (|getShellEntry| $ 51))
                            (|getShellEntry| $ 39))
                        (LETT |newlp| (CONS (|SPADfirst| |lp|) |newlp|)
                              |TSETCAT-;collectQuasiMonic;2S;33|)))
                     (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;collectQuasiMonic;2S;33|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |newlp| (|getShellEntry| $ 118))))))) 

(DEFUN |TSETCAT-;collectUnder;SVS;34| (|ts| |v| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 24)
                     (SPADCALL |ts| (|getShellEntry| $ 30))
                     (|getShellEntry| $ 41))
                 |TSETCAT-;collectUnder;SVS;34|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (NOT (SPADCALL
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 25))
                                     |v| (|getShellEntry| $ 71))))))
                   (GO G191)))
                (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;collectUnder;SVS;34|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |lp| (|getShellEntry| $ 118))))))) 

(DEFUN |TSETCAT-;collectUpper;SVS;35| (|ts| |v| $)
  (PROG (|lp2| |lp1|)
    (RETURN
      (SEQ (LETT |lp1|
                 (SPADCALL (ELT $ 24)
                     (SPADCALL |ts| (|getShellEntry| $ 30))
                     (|getShellEntry| $ 41))
                 |TSETCAT-;collectUpper;SVS;35|)
           (LETT |lp2| NIL |TSETCAT-;collectUpper;SVS;35|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp1|) 'NIL)
                           ('T
                            (SPADCALL |v|
                                (SPADCALL (|SPADfirst| |lp1|)
                                    (|getShellEntry| $ 25))
                                (|getShellEntry| $ 71)))))
                   (GO G191)))
                (SEQ (LETT |lp2| (CONS (|SPADfirst| |lp1|) |lp2|)
                           |TSETCAT-;collectUpper;SVS;35|)
                     (EXIT (LETT |lp1| (CDR |lp1|)
                                 |TSETCAT-;collectUpper;SVS;35|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL (REVERSE |lp2|) (|getShellEntry| $ 118))))))) 

(DEFUN |TSETCAT-;construct;LS;36| (|lp| $)
  (PROG (|rif|)
    (RETURN
      (SEQ (LETT |rif| (SPADCALL |lp| (|getShellEntry| $ 131))
                 |TSETCAT-;construct;LS;36|)
           (EXIT (COND
                   ((QEQCAR |rif| 0) (QCDR |rif|))
                   ('T
                    (|error| "in construct : LP -> $ from TSETCAT : bad arg")))))))) 

(DEFUN |TSETCAT-;retractIfCan;LU;37| (|lp| $)
  (PROG (|rif|)
    (RETURN
      (SEQ (COND
             ((NULL |lp|) (CONS 0 (SPADCALL (|getShellEntry| $ 42))))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 24) |lp|
                             (|getShellEntry| $ 41))
                         |TSETCAT-;retractIfCan;LU;37|)
                   (LETT |rif|
                         (SPADCALL (CDR |lp|) (|getShellEntry| $ 131))
                         |TSETCAT-;retractIfCan;LU;37|)
                   (EXIT (COND
                           ((QEQCAR |rif| 0)
                            (SPADCALL (QCDR |rif|) (|SPADfirst| |lp|)
                                (|getShellEntry| $ 133)))
                           ('T
                            (|error| "in retractIfCan : LP -> ... from TSETCAT : bad arg"))))))))))) 

(DEFUN |TSETCAT-;extend;SPS;38| (|ts| |p| $)
  (PROG (|eif|)
    (RETURN
      (SEQ (LETT |eif| (SPADCALL |ts| |p| (|getShellEntry| $ 133))
                 |TSETCAT-;extend;SPS;38|)
           (EXIT (COND
                   ((QEQCAR |eif| 0) (QCDR |eif|))
                   ('T
                    (|error| "in extend : ($,P) -> $ from TSETCAT : bad ars")))))))) 

(DEFUN |TSETCAT-;coHeight;SNni;39| (|ts| $)
  (PROG (|n| |m| #0=#:G1663)
    (RETURN
      (SEQ (LETT |n| (SPADCALL (|getShellEntry| $ 136))
                 |TSETCAT-;coHeight;SNni;39|)
           (LETT |m| (LENGTH (SPADCALL |ts| (|getShellEntry| $ 30)))
                 |TSETCAT-;coHeight;SNni;39|)
           (EXIT (PROG2 (LETT #0#
                              (SPADCALL |n| |m|
                                  (|getShellEntry| $ 138))
                              |TSETCAT-;coHeight;SNni;39|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|NonNegativeInteger|)
                       #0#))))))) 

(DEFUN |TriangularSetCategory&| (|#1| |#2| |#3| |#4| |#5|)
  (PROG (|dv$1| |dv$2| |dv$3| |dv$4| |dv$5| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|)
              . #0=(|TriangularSetCategory&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$3| (|devaluate| |#3|) . #0#)
        (LETT |dv$4| (|devaluate| |#4|) . #0#)
        (LETT |dv$5| (|devaluate| |#5|) . #0#)
        (LETT |dv$|
              (LIST '|TriangularSetCategory&| |dv$1| |dv$2| |dv$3|
                    |dv$4| |dv$5|) . #0#)
        (LETT $ (|newShell| 141) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#4| '(|Finite|)))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (|setShellEntry| $ 8 |#3|)
        (|setShellEntry| $ 9 |#4|)
        (|setShellEntry| $ 10 |#5|)
        (COND
          ((|testBitVector| |pv$| 1)
           (|setShellEntry| $ 139
               (CONS (|dispatchFunction| |TSETCAT-;coHeight;SNni;39|)
                     $))))
        $)))) 

(MAKEPROP '|TriangularSetCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|local| |#3|) (|local| |#4|) (|local| |#5|) (|Boolean|)
             (0 . |empty?|) (5 . |false|) (|Union| 10 '"failed")
             (9 . |first|) (14 . =) (|Union| $ '"failed") (20 . |rest|)
             (25 . =) |TSETCAT-;=;2SB;1| (31 . |last|)
             (36 . |infRittWu?|) (42 . |true|) (46 . |supRittWu?|)
             (52 . |mvar|) (57 . |collectUpper|) (63 . |infRittWu?|)
             |TSETCAT-;infRittWu?;2SB;2| (|List| 10) (69 . |members|)
             (74 . |empty?|) (79 . |first|) (84 . |rest|)
             (|Mapping| 11 10 10) |TSETCAT-;reduced?;PSMB;3|
             (89 . |zero?|) (|Mapping| 11 10) (94 . |remove|)
             (100 . |ground?|) (105 . |any?|) (111 . |sort|)
             (117 . |empty|) (121 . |extend|) (127 . |reduced?|)
             (134 . |cons|) (|Record| (|:| |bas| $) (|:| |top| 29))
             (|Union| 46 '"failed") |TSETCAT-;basicSet;LMU;4|
             (140 . |concat|) |TSETCAT-;basicSet;LMMU;5| (146 . |init|)
             (151 . |primPartElseUnitCanonical|)
             (156 . |removeDuplicates|) |TSETCAT-;initials;SL;6|
             (|NonNegativeInteger|) (161 . |Zero|) (165 . |mdeg|)
             (170 . *) |TSETCAT-;degree;SNni;7| (176 . |initials|)
             (|Record| (|:| |close| 29) (|:| |open| 29))
             |TSETCAT-;quasiComponent;SR;8| (|List| $)
             (181 . |normalized?|) |TSETCAT-;normalized?;PSB;9|
             (187 . |reduced?|) |TSETCAT-;stronglyReduced?;PSB;10|
             (193 . |head|) (198 . |stronglyReduced?|)
             |TSETCAT-;headReduced?;PSB;11| (204 . <) (210 . =)
             (216 . |reduced?|) |TSETCAT-;initiallyReduced?;PSB;12|
             (|Mapping| 10 10 10) |TSETCAT-;reduce;PSMMP;13|
             (222 . |trivialIdeal?|) (227 . |One|) (231 . |reduce|)
             |TSETCAT-;rewriteSetWithReduction;LSMML;14|
             (239 . |lazyPrem|) |TSETCAT-;stronglyReduce;PSP;15|
             (245 . |headReduce|) (251 . |headReduced?|)
             |TSETCAT-;headReduce;PSP;16| (257 . |initiallyReduce|)
             (263 . |initiallyReduced?|)
             |TSETCAT-;initiallyReduce;PSP;17| (269 . |collectUnder|)
             (275 . |algebraic?|) (281 . |select|) (287 . |removeZero|)
             (293 . |Zero|) (297 . |degree|) (|Integer|)
             (303 . |positive?|) (308 . |mainMonomial|) (313 . *)
             (319 . +) (325 . |tail|) |TSETCAT-;removeZero;PSP;18|
             (330 . |collectQuasiMonic|)
             (|Record| (|:| |rnum| 7) (|:| |polnum| 10) (|:| |den| 7))
             (335 . |remainder|) |TSETCAT-;reduceByQuasiMonic;PSP;19|
             (|Mapping| 11 10 29) |TSETCAT-;autoReduced?;SMB;20|
             (341 . |autoReduced?|) |TSETCAT-;stronglyReduced?;SB;21|
             |TSETCAT-;normalized?;SB;22| (347 . |headReduced?|)
             |TSETCAT-;headReduced?;SB;23| (353 . |initiallyReduced?|)
             |TSETCAT-;initiallyReduced?;SB;24| |TSETCAT-;mvar;SV;25|
             |TSETCAT-;first;SU;26| |TSETCAT-;last;SU;27|
             (359 . |construct|) |TSETCAT-;rest;SU;28|
             |TSETCAT-;coerce;SL;29| (|List| 9)
             |TSETCAT-;algebraicVariables;SL;30|
             (364 . |algebraicVariables|) (369 . |member?|)
             |TSETCAT-;algebraic?;VSB;31| |TSETCAT-;select;SVU;32|
             |TSETCAT-;collectQuasiMonic;2S;33|
             |TSETCAT-;collectUnder;SVS;34| (375 . |reverse|)
             |TSETCAT-;collectUpper;SVS;35| (380 . |retractIfCan|)
             |TSETCAT-;construct;LS;36| (385 . |extendIfCan|)
             |TSETCAT-;retractIfCan;LU;37| |TSETCAT-;extend;SPS;38|
             (391 . |size|) (395 . |#|) (400 . |subtractIfCan|)
             (406 . |coHeight|) (|OutputForm|))
          '#(|stronglyReduced?| 411 |stronglyReduce| 422 |select| 428
             |rewriteSetWithReduction| 434 |retractIfCan| 442 |rest|
             447 |removeZero| 452 |reduced?| 458 |reduceByQuasiMonic|
             465 |reduce| 471 |quasiComponent| 479 |normalized?| 484
             |mvar| 495 |last| 500 |initials| 505 |initiallyReduced?|
             510 |initiallyReduce| 521 |infRittWu?| 527 |headReduced?|
             533 |headReduce| 544 |first| 550 |extend| 555 |degree| 561
             |construct| 566 |collectUpper| 571 |collectUnder| 577
             |collectQuasiMonic| 583 |coerce| 588 |coHeight| 593
             |basicSet| 598 |autoReduced?| 611 |algebraicVariables| 617
             |algebraic?| 622 = 628)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 139
                                '(1 6 11 0 12 0 11 0 13 1 6 14 0 15 2
                                  10 11 0 0 16 1 6 17 0 18 2 6 11 0 0
                                  19 1 6 14 0 21 2 10 11 0 0 22 0 11 0
                                  23 2 10 11 0 0 24 1 10 9 0 25 2 6 0 0
                                  9 26 2 6 11 0 0 27 1 6 29 0 30 1 29
                                  11 0 31 1 29 10 0 32 1 29 0 0 33 1 10
                                  11 0 36 2 29 0 37 0 38 1 10 11 0 39 2
                                  29 11 37 0 40 2 29 0 34 0 41 0 6 0 42
                                  2 6 0 0 10 43 3 6 11 10 0 34 44 2 29
                                  0 10 0 45 2 29 0 0 0 49 1 10 0 0 51 1
                                  10 0 0 52 1 29 0 0 53 0 55 0 56 1 10
                                  55 0 57 2 55 0 55 0 58 1 6 29 0 60 2
                                  10 11 0 63 64 2 10 11 0 63 66 1 10 0
                                  0 68 2 6 11 10 0 69 2 9 11 0 0 71 2 9
                                  11 0 0 72 2 10 11 0 0 73 1 6 11 0 77
                                  0 10 0 78 4 6 10 10 0 75 34 79 2 10 0
                                  0 0 81 2 10 0 0 0 83 2 10 11 0 0 84 2
                                  10 0 0 0 86 2 10 11 0 0 87 2 6 0 0 9
                                  89 2 6 11 9 0 90 2 6 14 0 9 91 2 6 10
                                  10 0 92 0 10 0 93 2 10 55 0 9 94 1 95
                                  11 0 96 1 10 0 0 97 2 10 0 0 0 98 2
                                  10 0 0 0 99 1 10 0 0 100 1 6 0 0 102
                                  2 6 103 10 0 104 2 6 11 0 106 108 2
                                  10 11 0 63 111 2 10 11 0 63 113 1 6 0
                                  29 118 1 6 121 0 123 2 121 11 9 0 124
                                  1 29 0 0 129 1 6 17 29 131 2 6 17 0
                                  10 133 0 9 55 136 1 29 55 0 137 2 55
                                  17 0 0 138 1 0 55 0 139 1 0 11 0 109
                                  2 0 11 10 0 67 2 0 10 10 0 82 2 0 14
                                  0 9 126 4 0 29 29 0 75 34 80 1 0 17
                                  29 134 1 0 17 0 119 2 0 10 10 0 101 3
                                  0 11 10 0 34 35 2 0 10 10 0 105 4 0
                                  10 10 0 75 34 76 1 0 61 0 62 1 0 11 0
                                  110 2 0 11 10 0 65 1 0 9 0 115 1 0 14
                                  0 117 1 0 29 0 54 1 0 11 0 114 2 0 11
                                  10 0 74 2 0 10 10 0 88 2 0 11 0 0 28
                                  1 0 11 0 112 2 0 11 10 0 70 2 0 10 10
                                  0 85 1 0 14 0 116 2 0 0 0 10 135 1 0
                                  55 0 59 1 0 0 29 132 2 0 0 0 9 130 2
                                  0 0 0 9 128 1 0 0 0 127 1 0 29 0 120
                                  1 0 55 0 139 3 0 47 29 37 34 50 2 0
                                  47 29 34 48 2 0 11 0 106 107 1 0 121
                                  0 122 2 0 11 9 0 125 2 0 11 0 0 20)))))
          '|lookupComplete|)) 
