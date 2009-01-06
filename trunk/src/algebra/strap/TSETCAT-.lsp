
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
             (NULL (SPADCALL
                       (PROG2 (LETT #0#
                                    (SPADCALL |ts|
                                     (|getShellEntry| $ 14))
                                    |TSETCAT-;=;2SB;1|)
                              (QCDR #0#)
                         (|check-union| (QEQCAR #0# 0)
                             (|getShellEntry| $ 10) #0#))
                       (PROG2 (LETT #0#
                                    (SPADCALL |us|
                                     (|getShellEntry| $ 14))
                                    |TSETCAT-;=;2SB;1|)
                              (QCDR #0#)
                         (|check-union| (QEQCAR #0# 0)
                             (|getShellEntry| $ 10) #0#))
                       (|getShellEntry| $ 15))))
         'NIL)
        ('T
         (SPADCALL
             (PROG2 (LETT #1# (SPADCALL |ts| (|getShellEntry| $ 17))
                          |TSETCAT-;=;2SB;1|)
                    (QCDR #1#)
               (|check-union| (QEQCAR #1# 0) (|getShellEntry| $ 6) #1#))
             (PROG2 (LETT #1# (SPADCALL |us| (|getShellEntry| $ 17))
                          |TSETCAT-;=;2SB;1|)
                    (QCDR #1#)
               (|check-union| (QEQCAR #1# 0) (|getShellEntry| $ 6) #1#))
             (|getShellEntry| $ 18))))))) 

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
                                       (|getShellEntry| $ 20))
                                      |TSETCAT-;infRittWu?;2SB;2|)
                                (QCDR #0#)
                           (|check-union| (QEQCAR #0# 0)
                               (|getShellEntry| $ 10) #0#))
                         |TSETCAT-;infRittWu?;2SB;2|)
                   (LETT |q|
                         (PROG2 (LETT #0#
                                      (SPADCALL |us|
                                       (|getShellEntry| $ 20))
                                      |TSETCAT-;infRittWu?;2SB;2|)
                                (QCDR #0#)
                           (|check-union| (QEQCAR #0# 0)
                               (|getShellEntry| $ 10) #0#))
                         |TSETCAT-;infRittWu?;2SB;2|)
                   (EXIT (COND
                           ((SPADCALL |p| |q| (|getShellEntry| $ 21))
                            'T)
                           ((SPADCALL |p| |q| (|getShellEntry| $ 22))
                            'NIL)
                           ('T
                            (SEQ (LETT |v|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 23))
                                       |TSETCAT-;infRittWu?;2SB;2|)
                                 (EXIT (SPADCALL
                                        (SPADCALL |ts| |v|
                                         (|getShellEntry| $ 24))
                                        (SPADCALL |us| |v|
                                         (|getShellEntry| $ 24))
                                        (|getShellEntry| $ 25)))))))))))))) 

(DEFUN |TSETCAT-;reduced?;PSMB;3| (|p| |ts| |redOp?| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 28))
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
      (SEQ (LETT |ps| (SPADCALL (ELT $ 31) |ps| (|getShellEntry| $ 33))
                 |TSETCAT-;basicSet;LMU;4|)
           (EXIT (COND
                   ((SPADCALL (ELT $ 34) |ps| (|getShellEntry| $ 35))
                    (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |ps|
                               (SPADCALL (ELT $ 21) |ps|
                                   (|getShellEntry| $ 36))
                               |TSETCAT-;basicSet;LMU;4|)
                         (LETT |bs| (SPADCALL (|getShellEntry| $ 37))
                               |TSETCAT-;basicSet;LMU;4|)
                         (LETT |ts| NIL |TSETCAT-;basicSet;LMU;4|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |ps|))) (GO G191)))
                              (SEQ (LETT |b| (|SPADfirst| |ps|)
                                    |TSETCAT-;basicSet;LMU;4|)
                                   (LETT |bs|
                                    (SPADCALL |bs| |b|
                                     (|getShellEntry| $ 38))
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
                                              (|getShellEntry| $ 39))))))
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
      (SEQ (LETT |ps| (SPADCALL (ELT $ 31) |ps| (|getShellEntry| $ 33))
                 |TSETCAT-;basicSet;LMMU;5|)
           (EXIT (COND
                   ((SPADCALL (ELT $ 34) |ps| (|getShellEntry| $ 35))
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
                               (SPADCALL (ELT $ 21) |gps|
                                   (|getShellEntry| $ 36))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |bs| (SPADCALL (|getShellEntry| $ 37))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |ts| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |gps|))) (GO G191)))
                              (SEQ (LETT |b| (|SPADfirst| |gps|)
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (LETT |bs|
                                    (SPADCALL |bs| |b|
                                     (|getShellEntry| $ 38))
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
                                              (|getShellEntry| $ 39))))))
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
                               (SPADCALL (ELT $ 21)
                                   (SPADCALL |ts| |bps|
                                    (|getShellEntry| $ 43))
                                   (|getShellEntry| $ 36))
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
                               (SPADCALL |ts| (|getShellEntry| $ 28))
                               |TSETCAT-;initials;SL;6|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (NULL |lp|))) (GO G191)))
                              (SEQ (LETT |p| (|SPADfirst| |lp|)
                                    |TSETCAT-;initials;SL;6|)
                                   (COND
                                     ((NULL
                                       (SPADCALL
                                        (LETT |ip|
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 45))
                                         |TSETCAT-;initials;SL;6|)
                                        (|getShellEntry| $ 34)))
                                      (LETT |lip|
                                       (CONS
                                        (SPADCALL |ip|
                                         (|getShellEntry| $ 46))
                                        |lip|)
                                       |TSETCAT-;initials;SL;6|)))
                                   (EXIT
                                    (LETT |lp| (CDR |lp|)
                                     |TSETCAT-;initials;SL;6|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL |lip| (|getShellEntry| $ 47))))))))))) 

(DEFUN |TSETCAT-;degree;SNni;7| (|ts| $)
  (PROG (|lp| |d|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 0)
             ('T
              (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 28))
                         |TSETCAT-;degree;SNni;7|)
                   (LETT |d|
                         (SPADCALL (|SPADfirst| |lp|)
                             (|getShellEntry| $ 50))
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
                                      (|getShellEntry| $ 50)))
                                    |TSETCAT-;degree;SNni;7|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |TSETCAT-;quasiComponent;SR;8| (|ts| $)
  (CONS (SPADCALL |ts| (|getShellEntry| $ 28))
        (SPADCALL |ts| (|getShellEntry| $ 52)))) 

(DEFUN |TSETCAT-;normalized?;PSB;9| (|p| |ts| $)
  (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 28))
      (|getShellEntry| $ 56))) 

(DEFUN |TSETCAT-;stronglyReduced?;PSB;10| (|p| |ts| $)
  (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 28))
      (|getShellEntry| $ 58))) 

(DEFUN |TSETCAT-;headReduced?;PSB;11| (|p| |ts| $)
  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 60)) |ts|
      (|getShellEntry| $ 61))) 

(DEFUN |TSETCAT-;initiallyReduced?;PSB;12| (|p| |ts| $)
  (PROG (|lp| |red|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 28))
                 |TSETCAT-;initiallyReduced?;PSB;12|)
           (LETT |red| 'T |TSETCAT-;initiallyReduced?;PSB;12|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((OR (NULL |lp|)
                                (SPADCALL |p| (|getShellEntry| $ 34)))
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
                                        (|getShellEntry| $ 23))
                                       (SPADCALL (|SPADfirst| |lp|)
                                        (|getShellEntry| $ 23))
                                       (|getShellEntry| $ 63)))))
                             (GO G191)))
                          (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                      |TSETCAT-;initiallyReduced?;PSB;12|)))
                          NIL (GO G190) G191 (EXIT NIL))
                     (EXIT (COND
                             ((NULL (NULL |lp|))
                              (COND
                                ((SPADCALL
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 23))
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 23))
                                     (|getShellEntry| $ 64))
                                 (COND
                                   ((SPADCALL |p| (|SPADfirst| |lp|)
                                     (|getShellEntry| $ 65))
                                    (SEQ
                                     (LETT |lp| (CDR |lp|)
                                      |TSETCAT-;initiallyReduced?;PSB;12|)
                                     (EXIT
                                      (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 45))
                                       |TSETCAT-;initiallyReduced?;PSB;12|))))
                                   ('T
                                    (LETT |red| 'NIL
                                     |TSETCAT-;initiallyReduced?;PSB;12|))))
                                ('T
                                 (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 45))
                                       |TSETCAT-;initiallyReduced?;PSB;12|)))))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |red|))))) 

(DEFUN |TSETCAT-;reduce;PSMMP;13| (|p| |ts| |redOp| |redOp?| $)
  (PROG (|ts0| #0=#:G1545 |reductor| #1=#:G1548)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |ts| (|getShellEntry| $ 12))
                  (SPADCALL |p| (|getShellEntry| $ 34)))
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
                                      (|getShellEntry| $ 34))))))
                           (GO G191)))
                        (SEQ (LETT |reductor|
                                   (PROG2
                                    (LETT #0#
                                     (SPADCALL |ts|
                                      (|getShellEntry| $ 14))
                                     |TSETCAT-;reduce;PSMMP;13|)
                                    (QCDR #0#)
                                     (|check-union| (QEQCAR #0# 0)
                                      (|getShellEntry| $ 10) #0#))
                                   |TSETCAT-;reduce;PSMMP;13|)
                             (LETT |ts|
                                   (PROG2
                                    (LETT #1#
                                     (SPADCALL |ts|
                                      (|getShellEntry| $ 17))
                                     |TSETCAT-;reduce;PSMMP;13|)
                                    (QCDR #1#)
                                     (|check-union| (QEQCAR #1# 0)
                                      (|getShellEntry| $ 6) #1#))
                                   |TSETCAT-;reduce;PSMMP;13|)
                             (EXIT (COND
                                     ((NULL
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
             ((SPADCALL |ts| (|getShellEntry| $ 69)) |lp|)
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 31) |lp|
                             (|getShellEntry| $ 33))
                         |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                   (EXIT (COND
                           ((NULL |lp|) |lp|)
                           ((SPADCALL (ELT $ 34) |lp|
                                (|getShellEntry| $ 35))
                            (LIST (|spadConstant| $ 70)))
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
                                          (|getShellEntry| $ 71))
                                         (|getShellEntry| $ 46))
                                        |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                       (EXIT
                                        (COND
                                          ((NULL
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 31)))
                                           (COND
                                             ((SPADCALL |p|
                                               (|getShellEntry| $ 34))
                                              (SEQ
                                               (LETT |lp| NIL
                                                |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                               (EXIT
                                                (LETT |rs|
                                                 (LIST
                                                  (|spadConstant| $ 70))
                                                 |TSETCAT-;rewriteSetWithReduction;LSMML;14|))))
                                             ('T
                                              (LETT |rs|
                                               (CONS |p| |rs|)
                                               |TSETCAT-;rewriteSetWithReduction;LSMML;14|)))))))
                                      NIL (GO G190) G191 (EXIT NIL))
                                 (EXIT (SPADCALL |rs|
                                        (|getShellEntry| $ 47)))))))))))))) 

(DEFUN |TSETCAT-;stronglyReduce;PSP;15| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 73) (ELT $ 65) (|getShellEntry| $ 71))) 

(DEFUN |TSETCAT-;headReduce;PSP;16| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 75) (ELT $ 76) (|getShellEntry| $ 71))) 

(DEFUN |TSETCAT-;initiallyReduce;PSP;17| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 78) (ELT $ 79) (|getShellEntry| $ 71))) 

(DEFUN |TSETCAT-;removeZero;PSP;18| (|p| |ts| $)
  (PROG (|v| |tsv-| #0=#:G1571 #1=#:G1580 |q|)
    (RETURN
      (SEQ (EXIT (COND
                   ((OR (SPADCALL |p| (|getShellEntry| $ 34))
                        (SPADCALL |ts| (|getShellEntry| $ 12)))
                    |p|)
                   ('T
                    (SEQ (LETT |v|
                               (SPADCALL |p| (|getShellEntry| $ 23))
                               |TSETCAT-;removeZero;PSP;18|)
                         (LETT |tsv-|
                               (SPADCALL |ts| |v|
                                   (|getShellEntry| $ 81))
                               |TSETCAT-;removeZero;PSP;18|)
                         (COND
                           ((SPADCALL |v| |ts| (|getShellEntry| $ 82))
                            (SEQ (LETT |q|
                                       (SPADCALL |p|
                                        (PROG2
                                         (LETT #0#
                                          (SPADCALL |ts| |v|
                                           (|getShellEntry| $ 83))
                                          |TSETCAT-;removeZero;PSP;18|)
                                         (QCDR #0#)
                                          (|check-union| (QEQCAR #0# 0)
                                           (|getShellEntry| $ 10) #0#))
                                        (|getShellEntry| $ 73))
                                       |TSETCAT-;removeZero;PSP;18|)
                                 (EXIT (COND
                                         ((SPADCALL |q|
                                           (|getShellEntry| $ 31))
                                          (PROGN
                                            (LETT #1# |q|
                                             |TSETCAT-;removeZero;PSP;18|)
                                            (GO #1#)))
                                         ((SPADCALL
                                           (SPADCALL |q| |tsv-|
                                            (|getShellEntry| $ 84))
                                           (|getShellEntry| $ 31))
                                          (PROGN
                                            (LETT #1#
                                             (|spadConstant| $ 85)
                                             |TSETCAT-;removeZero;PSP;18|)
                                            (GO #1#))))))))
                         (EXIT (COND
                                 ((SPADCALL |tsv-|
                                      (|getShellEntry| $ 12))
                                  |p|)
                                 ('T
                                  (SEQ (LETT |q| (|spadConstant| $ 85)
                                        |TSETCAT-;removeZero;PSP;18|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (SPADCALL
                                             (SPADCALL |p| |v|
                                              (|getShellEntry| $ 86))
                                             (|getShellEntry| $ 88)))
                                           (GO G191)))
                                        (SEQ
                                         (LETT |q|
                                          (SPADCALL
                                           (SPADCALL
                                            (SPADCALL
                                             (SPADCALL |p|
                                              (|getShellEntry| $ 45))
                                             |tsv-|
                                             (|getShellEntry| $ 84))
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 89))
                                            (|getShellEntry| $ 90))
                                           |q| (|getShellEntry| $ 91))
                                          |TSETCAT-;removeZero;PSP;18|)
                                         (EXIT
                                          (LETT |p|
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 92))
                                           |TSETCAT-;removeZero;PSP;18|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT
                                        (SPADCALL |q|
                                         (SPADCALL |p| |tsv-|
                                          (|getShellEntry| $ 84))
                                         (|getShellEntry| $ 91)))))))))))
           #1# (EXIT #1#))))) 

(DEFUN |TSETCAT-;reduceByQuasiMonic;PSP;19| (|p| |ts| $)
  (COND
    ((OR (SPADCALL |p| (|getShellEntry| $ 34))
         (SPADCALL |ts| (|getShellEntry| $ 12)))
     |p|)
    ('T
     (QVELT (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 94))
                (|getShellEntry| $ 96))
            1)))) 

(DEFUN |TSETCAT-;autoReduced?;SMB;20| (|ts| |redOp?| $)
  (PROG (|p| |lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 'T)
             ('T
              (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 28))
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
  (SPADCALL |ts| (ELT $ 58) (|getShellEntry| $ 100))) 

(DEFUN |TSETCAT-;normalized?;SB;22| (|ts| $)
  (SPADCALL |ts| (ELT $ 56) (|getShellEntry| $ 100))) 

(DEFUN |TSETCAT-;headReduced?;SB;23| (|ts| $)
  (SPADCALL |ts| (ELT $ 103) (|getShellEntry| $ 100))) 

(DEFUN |TSETCAT-;initiallyReduced?;SB;24| (|ts| $)
  (SPADCALL |ts| (ELT $ 105) (|getShellEntry| $ 100))) 

(DEFUN |TSETCAT-;mvar;SV;25| (|ts| $)
  (PROG (#0=#:G1599)
    (RETURN
      (COND
        ((SPADCALL |ts| (|getShellEntry| $ 12))
         (|error| "Error from TSETCAT in mvar : #1 is empty"))
        ('T
         (SPADCALL
             (PROG2 (LETT #0# (SPADCALL |ts| (|getShellEntry| $ 14))
                          |TSETCAT-;mvar;SV;25|)
                    (QCDR #0#)
               (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 10)
                   #0#))
             (|getShellEntry| $ 23))))))) 

(DEFUN |TSETCAT-;first;SU;26| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 22)
                             (SPADCALL |ts| (|getShellEntry| $ 28))
                             (|getShellEntry| $ 36))
                         |TSETCAT-;first;SU;26|)
                   (EXIT (CONS 0 (|SPADfirst| |lp|)))))))))) 

(DEFUN |TSETCAT-;last;SU;27| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 21)
                             (SPADCALL |ts| (|getShellEntry| $ 28))
                             (|getShellEntry| $ 36))
                         |TSETCAT-;last;SU;27|)
                   (EXIT (CONS 0 (|SPADfirst| |lp|)))))))))) 

(DEFUN |TSETCAT-;rest;SU;28| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 22)
                             (SPADCALL |ts| (|getShellEntry| $ 28))
                             (|getShellEntry| $ 36))
                         |TSETCAT-;rest;SU;28|)
                   (EXIT (CONS 0
                               (SPADCALL (CDR |lp|)
                                   (|getShellEntry| $ 110))))))))))) 

(DEFUN |TSETCAT-;coerce;SL;29| (|ts| $)
  (SPADCALL (ELT $ 22) (SPADCALL |ts| (|getShellEntry| $ 28))
            (|getShellEntry| $ 36))) 

(DEFUN |TSETCAT-;algebraicVariables;SL;30| (|ts| $)
  (PROG (#0=#:G1667 |p| #1=#:G1668)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |TSETCAT-;algebraicVariables;SL;30|)
             (SEQ (LETT |p| NIL |TSETCAT-;algebraicVariables;SL;30|)
                  (LETT #1# (SPADCALL |ts| (|getShellEntry| $ 28))
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
                                     (|getShellEntry| $ 23))
                                    #0#)
                                   |TSETCAT-;algebraicVariables;SL;30|)))
                  (LETT #1# (CDR #1#)
                        |TSETCAT-;algebraicVariables;SL;30|)
                  (GO G190) G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |TSETCAT-;algebraic?;VSB;31| (|v| |ts| $)
  (SPADCALL |v| (SPADCALL |ts| (|getShellEntry| $ 115))
      (|getShellEntry| $ 116))) 

(DEFUN |TSETCAT-;select;SVU;32| (|ts| |v| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 22)
                     (SPADCALL |ts| (|getShellEntry| $ 28))
                     (|getShellEntry| $ 36))
                 |TSETCAT-;select;SVU;32|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (NOT (SPADCALL |v|
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 23))
                                     (|getShellEntry| $ 64))))))
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
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 28))
                 |TSETCAT-;collectQuasiMonic;2S;33|)
           (LETT |newlp| NIL |TSETCAT-;collectQuasiMonic;2S;33|)
           (SEQ G190 (COND ((NULL (NOT (NULL |lp|))) (GO G191)))
                (SEQ (COND
                       ((SPADCALL
                            (SPADCALL (|SPADfirst| |lp|)
                                (|getShellEntry| $ 45))
                            (|getShellEntry| $ 34))
                        (LETT |newlp| (CONS (|SPADfirst| |lp|) |newlp|)
                              |TSETCAT-;collectQuasiMonic;2S;33|)))
                     (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;collectQuasiMonic;2S;33|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |newlp| (|getShellEntry| $ 110))))))) 

(DEFUN |TSETCAT-;collectUnder;SVS;34| (|ts| |v| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 22)
                     (SPADCALL |ts| (|getShellEntry| $ 28))
                     (|getShellEntry| $ 36))
                 |TSETCAT-;collectUnder;SVS;34|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (NOT (SPADCALL
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 23))
                                     |v| (|getShellEntry| $ 63))))))
                   (GO G191)))
                (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;collectUnder;SVS;34|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |lp| (|getShellEntry| $ 110))))))) 

(DEFUN |TSETCAT-;collectUpper;SVS;35| (|ts| |v| $)
  (PROG (|lp2| |lp1|)
    (RETURN
      (SEQ (LETT |lp1|
                 (SPADCALL (ELT $ 22)
                     (SPADCALL |ts| (|getShellEntry| $ 28))
                     (|getShellEntry| $ 36))
                 |TSETCAT-;collectUpper;SVS;35|)
           (LETT |lp2| NIL |TSETCAT-;collectUpper;SVS;35|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp1|) 'NIL)
                           ('T
                            (SPADCALL |v|
                                (SPADCALL (|SPADfirst| |lp1|)
                                    (|getShellEntry| $ 23))
                                (|getShellEntry| $ 63)))))
                   (GO G191)))
                (SEQ (LETT |lp2| (CONS (|SPADfirst| |lp1|) |lp2|)
                           |TSETCAT-;collectUpper;SVS;35|)
                     (EXIT (LETT |lp1| (CDR |lp1|)
                                 |TSETCAT-;collectUpper;SVS;35|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL (REVERSE |lp2|) (|getShellEntry| $ 110))))))) 

(DEFUN |TSETCAT-;construct;LS;36| (|lp| $)
  (PROG (|rif|)
    (RETURN
      (SEQ (LETT |rif| (SPADCALL |lp| (|getShellEntry| $ 122))
                 |TSETCAT-;construct;LS;36|)
           (EXIT (COND
                   ((QEQCAR |rif| 0) (QCDR |rif|))
                   ('T
                    (|error| "in construct : LP -> $ from TSETCAT : bad arg")))))))) 

(DEFUN |TSETCAT-;retractIfCan;LU;37| (|lp| $)
  (PROG (|rif|)
    (RETURN
      (SEQ (COND
             ((NULL |lp|) (CONS 0 (SPADCALL (|getShellEntry| $ 37))))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 22) |lp|
                             (|getShellEntry| $ 36))
                         |TSETCAT-;retractIfCan;LU;37|)
                   (LETT |rif|
                         (SPADCALL (CDR |lp|) (|getShellEntry| $ 122))
                         |TSETCAT-;retractIfCan;LU;37|)
                   (EXIT (COND
                           ((QEQCAR |rif| 0)
                            (SPADCALL (QCDR |rif|) (|SPADfirst| |lp|)
                                (|getShellEntry| $ 124)))
                           ('T
                            (|error| "in retractIfCan : LP -> ... from TSETCAT : bad arg"))))))))))) 

(DEFUN |TSETCAT-;extend;SPS;38| (|ts| |p| $)
  (PROG (|eif|)
    (RETURN
      (SEQ (LETT |eif| (SPADCALL |ts| |p| (|getShellEntry| $ 124))
                 |TSETCAT-;extend;SPS;38|)
           (EXIT (COND
                   ((QEQCAR |eif| 0) (QCDR |eif|))
                   ('T
                    (|error| "in extend : ($,P) -> $ from TSETCAT : bad ars")))))))) 

(DEFUN |TSETCAT-;coHeight;SNni;39| (|ts| $)
  (PROG (|n| |m| #0=#:G1663)
    (RETURN
      (SEQ (LETT |n| (SPADCALL (|getShellEntry| $ 127))
                 |TSETCAT-;coHeight;SNni;39|)
           (LETT |m| (LENGTH (SPADCALL |ts| (|getShellEntry| $ 28)))
                 |TSETCAT-;coHeight;SNni;39|)
           (EXIT (PROG2 (LETT #0#
                              (SPADCALL |n| |m|
                                  (|getShellEntry| $ 128))
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
        (LETT $ (|newShell| 131) . #0#)
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
           (|setShellEntry| $ 129
               (CONS (|dispatchFunction| |TSETCAT-;coHeight;SNni;39|)
                     $))))
        $)))) 

(MAKEPROP '|TriangularSetCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|local| |#3|) (|local| |#4|) (|local| |#5|) (|Boolean|)
             (0 . |empty?|) (|Union| 10 '"failed") (5 . |first|)
             (10 . =) (|Union| $ '"failed") (16 . |rest|) (21 . =)
             |TSETCAT-;=;2SB;1| (27 . |last|) (32 . |infRittWu?|)
             (38 . |supRittWu?|) (44 . |mvar|) (49 . |collectUpper|)
             (55 . |infRittWu?|) |TSETCAT-;infRittWu?;2SB;2|
             (|List| 10) (61 . |members|) (|Mapping| 11 10 10)
             |TSETCAT-;reduced?;PSMB;3| (66 . |zero?|)
             (|Mapping| 11 10) (71 . |remove|) (77 . |ground?|)
             (82 . |any?|) (88 . |sort|) (94 . |empty|) (98 . |extend|)
             (104 . |reduced?|) (|Record| (|:| |bas| $) (|:| |top| 27))
             (|Union| 40 '"failed") |TSETCAT-;basicSet;LMU;4|
             (111 . |concat|) |TSETCAT-;basicSet;LMMU;5| (117 . |init|)
             (122 . |primPartElseUnitCanonical|)
             (127 . |removeDuplicates|) |TSETCAT-;initials;SL;6|
             (|NonNegativeInteger|) (132 . |mdeg|)
             |TSETCAT-;degree;SNni;7| (137 . |initials|)
             (|Record| (|:| |close| 27) (|:| |open| 27))
             |TSETCAT-;quasiComponent;SR;8| (|List| $)
             (142 . |normalized?|) |TSETCAT-;normalized?;PSB;9|
             (148 . |reduced?|) |TSETCAT-;stronglyReduced?;PSB;10|
             (154 . |head|) (159 . |stronglyReduced?|)
             |TSETCAT-;headReduced?;PSB;11| (165 . <) (171 . =)
             (177 . |reduced?|) |TSETCAT-;initiallyReduced?;PSB;12|
             (|Mapping| 10 10 10) |TSETCAT-;reduce;PSMMP;13|
             (183 . |trivialIdeal?|) (188 . |One|) (192 . |reduce|)
             |TSETCAT-;rewriteSetWithReduction;LSMML;14|
             (200 . |lazyPrem|) |TSETCAT-;stronglyReduce;PSP;15|
             (206 . |headReduce|) (212 . |headReduced?|)
             |TSETCAT-;headReduce;PSP;16| (218 . |initiallyReduce|)
             (224 . |initiallyReduced?|)
             |TSETCAT-;initiallyReduce;PSP;17| (230 . |collectUnder|)
             (236 . |algebraic?|) (242 . |select|) (248 . |removeZero|)
             (254 . |Zero|) (258 . |degree|) (|Integer|)
             (264 . |positive?|) (269 . |mainMonomial|) (274 . *)
             (280 . +) (286 . |tail|) |TSETCAT-;removeZero;PSP;18|
             (291 . |collectQuasiMonic|)
             (|Record| (|:| |rnum| 7) (|:| |polnum| 10) (|:| |den| 7))
             (296 . |remainder|) |TSETCAT-;reduceByQuasiMonic;PSP;19|
             (|Mapping| 11 10 27) |TSETCAT-;autoReduced?;SMB;20|
             (302 . |autoReduced?|) |TSETCAT-;stronglyReduced?;SB;21|
             |TSETCAT-;normalized?;SB;22| (308 . |headReduced?|)
             |TSETCAT-;headReduced?;SB;23| (314 . |initiallyReduced?|)
             |TSETCAT-;initiallyReduced?;SB;24| |TSETCAT-;mvar;SV;25|
             |TSETCAT-;first;SU;26| |TSETCAT-;last;SU;27|
             (320 . |construct|) |TSETCAT-;rest;SU;28|
             |TSETCAT-;coerce;SL;29| (|List| 9)
             |TSETCAT-;algebraicVariables;SL;30|
             (325 . |algebraicVariables|) (330 . |member?|)
             |TSETCAT-;algebraic?;VSB;31| |TSETCAT-;select;SVU;32|
             |TSETCAT-;collectQuasiMonic;2S;33|
             |TSETCAT-;collectUnder;SVS;34|
             |TSETCAT-;collectUpper;SVS;35| (336 . |retractIfCan|)
             |TSETCAT-;construct;LS;36| (341 . |extendIfCan|)
             |TSETCAT-;retractIfCan;LU;37| |TSETCAT-;extend;SPS;38|
             (347 . |size|) (351 . |subtractIfCan|) (357 . |coHeight|)
             (|OutputForm|))
          '#(|stronglyReduced?| 362 |stronglyReduce| 373 |select| 379
             |rewriteSetWithReduction| 385 |retractIfCan| 393 |rest|
             398 |removeZero| 403 |reduced?| 409 |reduceByQuasiMonic|
             416 |reduce| 422 |quasiComponent| 430 |normalized?| 435
             |mvar| 446 |last| 451 |initials| 456 |initiallyReduced?|
             461 |initiallyReduce| 472 |infRittWu?| 478 |headReduced?|
             484 |headReduce| 495 |first| 501 |extend| 506 |degree| 512
             |construct| 517 |collectUpper| 522 |collectUnder| 528
             |collectQuasiMonic| 534 |coerce| 539 |coHeight| 544
             |basicSet| 549 |autoReduced?| 562 |algebraicVariables| 568
             |algebraic?| 573 = 579)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 129
                                '(1 6 11 0 12 1 6 13 0 14 2 10 11 0 0
                                  15 1 6 16 0 17 2 6 11 0 0 18 1 6 13 0
                                  20 2 10 11 0 0 21 2 10 11 0 0 22 1 10
                                  9 0 23 2 6 0 0 9 24 2 6 11 0 0 25 1 6
                                  27 0 28 1 10 11 0 31 2 27 0 32 0 33 1
                                  10 11 0 34 2 27 11 32 0 35 2 27 0 29
                                  0 36 0 6 0 37 2 6 0 0 10 38 3 6 11 10
                                  0 29 39 2 27 0 0 0 43 1 10 0 0 45 1
                                  10 0 0 46 1 27 0 0 47 1 10 49 0 50 1
                                  6 27 0 52 2 10 11 0 55 56 2 10 11 0
                                  55 58 1 10 0 0 60 2 6 11 10 0 61 2 9
                                  11 0 0 63 2 9 11 0 0 64 2 10 11 0 0
                                  65 1 6 11 0 69 0 10 0 70 4 6 10 10 0
                                  67 29 71 2 10 0 0 0 73 2 10 0 0 0 75
                                  2 10 11 0 0 76 2 10 0 0 0 78 2 10 11
                                  0 0 79 2 6 0 0 9 81 2 6 11 9 0 82 2 6
                                  13 0 9 83 2 6 10 10 0 84 0 10 0 85 2
                                  10 49 0 9 86 1 87 11 0 88 1 10 0 0 89
                                  2 10 0 0 0 90 2 10 0 0 0 91 1 10 0 0
                                  92 1 6 0 0 94 2 6 95 10 0 96 2 6 11 0
                                  98 100 2 10 11 0 55 103 2 10 11 0 55
                                  105 1 6 0 27 110 1 6 113 0 115 2 113
                                  11 9 0 116 1 6 16 27 122 2 6 16 0 10
                                  124 0 9 49 127 2 49 16 0 0 128 1 0 49
                                  0 129 1 0 11 0 101 2 0 11 10 0 59 2 0
                                  10 10 0 74 2 0 13 0 9 118 4 0 27 27 0
                                  67 29 72 1 0 16 27 125 1 0 16 0 111 2
                                  0 10 10 0 93 3 0 11 10 0 29 30 2 0 10
                                  10 0 97 4 0 10 10 0 67 29 68 1 0 53 0
                                  54 1 0 11 0 102 2 0 11 10 0 57 1 0 9
                                  0 107 1 0 13 0 109 1 0 27 0 48 1 0 11
                                  0 106 2 0 11 10 0 66 2 0 10 10 0 80 2
                                  0 11 0 0 26 1 0 11 0 104 2 0 11 10 0
                                  62 2 0 10 10 0 77 1 0 13 0 108 2 0 0
                                  0 10 126 1 0 49 0 51 1 0 0 27 123 2 0
                                  0 0 9 121 2 0 0 0 9 120 1 0 0 0 119 1
                                  0 27 0 112 1 0 49 0 129 3 0 41 27 32
                                  29 44 2 0 41 27 29 42 2 0 11 0 98 99
                                  1 0 113 0 114 2 0 11 9 0 117 2 0 11 0
                                  0 19)))))
          '|lookupComplete|)) 
