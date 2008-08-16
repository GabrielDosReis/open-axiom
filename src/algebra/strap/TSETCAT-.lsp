
(/VERSIONCHECK 2) 

(DEFUN |TSETCAT-;=;2SB;1| (|ts| |us| $)
  (PROG (#0=#:G1451 #1=#:G1457)
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
  (PROG (|p| #0=#:G1464 |q| |v|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |us| (|getShellEntry| $ 12))
              (SPADCALL (SPADCALL |ts| (|getShellEntry| $ 12))
                  (|getShellEntry| $ 20)))
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
                           ((SPADCALL |p| |q| (|getShellEntry| $ 23))
                            'NIL)
                           ('T
                            (SEQ (LETT |v|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 24))
                                       |TSETCAT-;infRittWu?;2SB;2|)
                                 (EXIT (SPADCALL
                                        (SPADCALL |ts| |v|
                                         (|getShellEntry| $ 25))
                                        (SPADCALL |us| |v|
                                         (|getShellEntry| $ 25))
                                        (|getShellEntry| $ 26)))))))))))))) 

(DEFUN |TSETCAT-;reduced?;PSMB;3| (|p| |ts| |redOp?| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 29))
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
      (SEQ (LETT |ps| (SPADCALL (ELT $ 32) |ps| (|getShellEntry| $ 34))
                 |TSETCAT-;basicSet;LMU;4|)
           (EXIT (COND
                   ((SPADCALL (ELT $ 35) |ps| (|getShellEntry| $ 36))
                    (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |ps|
                               (SPADCALL (ELT $ 22) |ps|
                                   (|getShellEntry| $ 37))
                               |TSETCAT-;basicSet;LMU;4|)
                         (LETT |bs| (SPADCALL (|getShellEntry| $ 38))
                               |TSETCAT-;basicSet;LMU;4|)
                         (LETT |ts| NIL |TSETCAT-;basicSet;LMU;4|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL (NULL |ps|)
                                        (|getShellEntry| $ 20)))
                                 (GO G191)))
                              (SEQ (LETT |b| (|SPADfirst| |ps|)
                                    |TSETCAT-;basicSet;LMU;4|)
                                   (LETT |bs|
                                    (SPADCALL |bs| |b|
                                     (|getShellEntry| $ 39))
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
                                            (SPADCALL
                                             (SPADCALL
                                              (LETT |p|
                                               (|SPADfirst| |ps|)
                                               |TSETCAT-;basicSet;LMU;4|)
                                              |bs| |redOp?|
                                              (|getShellEntry| $ 40))
                                             (|getShellEntry| $ 20)))))
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
      (SEQ (LETT |ps| (SPADCALL (ELT $ 32) |ps| (|getShellEntry| $ 34))
                 |TSETCAT-;basicSet;LMMU;5|)
           (EXIT (COND
                   ((SPADCALL (ELT $ 35) |ps| (|getShellEntry| $ 36))
                    (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |gps| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |bps| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL (NULL |ps|)
                                        (|getShellEntry| $ 20)))
                                 (GO G191)))
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
                                   (|getShellEntry| $ 37))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |bs| (SPADCALL (|getShellEntry| $ 38))
                               |TSETCAT-;basicSet;LMMU;5|)
                         (LETT |ts| NIL |TSETCAT-;basicSet;LMMU;5|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL (NULL |gps|)
                                        (|getShellEntry| $ 20)))
                                 (GO G191)))
                              (SEQ (LETT |b| (|SPADfirst| |gps|)
                                    |TSETCAT-;basicSet;LMMU;5|)
                                   (LETT |bs|
                                    (SPADCALL |bs| |b|
                                     (|getShellEntry| $ 39))
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
                                            (SPADCALL
                                             (SPADCALL
                                              (LETT |p|
                                               (|SPADfirst| |gps|)
                                               |TSETCAT-;basicSet;LMMU;5|)
                                              |bs| |redOp?|
                                              (|getShellEntry| $ 40))
                                             (|getShellEntry| $ 20)))))
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
                                    (|getShellEntry| $ 44))
                                   (|getShellEntry| $ 37))
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
                               (SPADCALL |ts| (|getShellEntry| $ 29))
                               |TSETCAT-;initials;SL;6|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL (NULL |lp|)
                                        (|getShellEntry| $ 20)))
                                 (GO G191)))
                              (SEQ (LETT |p| (|SPADfirst| |lp|)
                                    |TSETCAT-;initials;SL;6|)
                                   (COND
                                     ((NULL
                                       (SPADCALL
                                        (LETT |ip|
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 46))
                                         |TSETCAT-;initials;SL;6|)
                                        (|getShellEntry| $ 35)))
                                      (LETT |lip|
                                       (CONS
                                        (SPADCALL |ip|
                                         (|getShellEntry| $ 47))
                                        |lip|)
                                       |TSETCAT-;initials;SL;6|)))
                                   (EXIT
                                    (LETT |lp| (CDR |lp|)
                                     |TSETCAT-;initials;SL;6|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL |lip| (|getShellEntry| $ 48))))))))))) 

(DEFUN |TSETCAT-;degree;SNni;7| (|ts| $)
  (PROG (|lp| |d|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 0)
             ('T
              (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 29))
                         |TSETCAT-;degree;SNni;7|)
                   (LETT |d|
                         (SPADCALL (|SPADfirst| |lp|)
                             (|getShellEntry| $ 51))
                         |TSETCAT-;degree;SNni;7|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL
                                     (NULL
                                      (LETT |lp| (CDR |lp|)
                                       |TSETCAT-;degree;SNni;7|))
                                     (|getShellEntry| $ 20)))
                           (GO G191)))
                        (SEQ (EXIT (LETT |d|
                                    (* |d|
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 51)))
                                    |TSETCAT-;degree;SNni;7|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |TSETCAT-;quasiComponent;SR;8| (|ts| $)
  (CONS (SPADCALL |ts| (|getShellEntry| $ 29))
        (SPADCALL |ts| (|getShellEntry| $ 53)))) 

(DEFUN |TSETCAT-;normalized?;PSB;9| (|p| |ts| $)
  (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 29))
      (|getShellEntry| $ 57))) 

(DEFUN |TSETCAT-;stronglyReduced?;PSB;10| (|p| |ts| $)
  (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 29))
      (|getShellEntry| $ 59))) 

(DEFUN |TSETCAT-;headReduced?;PSB;11| (|p| |ts| $)
  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 61)) |ts|
      (|getShellEntry| $ 62))) 

(DEFUN |TSETCAT-;initiallyReduced?;PSB;12| (|p| |ts| $)
  (PROG (|lp| |red|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 29))
                 |TSETCAT-;initiallyReduced?;PSB;12|)
           (LETT |red| 'T |TSETCAT-;initiallyReduced?;PSB;12|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((OR (NULL |lp|)
                                (SPADCALL |p| (|getShellEntry| $ 35)))
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
                                        (|getShellEntry| $ 24))
                                       (SPADCALL (|SPADfirst| |lp|)
                                        (|getShellEntry| $ 24))
                                       (|getShellEntry| $ 64)))))
                             (GO G191)))
                          (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                      |TSETCAT-;initiallyReduced?;PSB;12|)))
                          NIL (GO G190) G191 (EXIT NIL))
                     (EXIT (COND
                             ((NULL (NULL |lp|))
                              (COND
                                ((SPADCALL
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 24))
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 24))
                                     (|getShellEntry| $ 65))
                                 (COND
                                   ((SPADCALL |p| (|SPADfirst| |lp|)
                                     (|getShellEntry| $ 66))
                                    (SEQ
                                     (LETT |lp| (CDR |lp|)
                                      |TSETCAT-;initiallyReduced?;PSB;12|)
                                     (EXIT
                                      (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 46))
                                       |TSETCAT-;initiallyReduced?;PSB;12|))))
                                   ('T
                                    (LETT |red| 'NIL
                                     |TSETCAT-;initiallyReduced?;PSB;12|))))
                                ('T
                                 (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 46))
                                       |TSETCAT-;initiallyReduced?;PSB;12|)))))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |red|))))) 

(DEFUN |TSETCAT-;reduce;PSMMP;13| (|p| |ts| |redOp| |redOp?| $)
  (PROG (|ts0| #0=#:G1539 |reductor| #1=#:G1542)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |ts| (|getShellEntry| $ 12))
                  (SPADCALL |p| (|getShellEntry| $ 35)))
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
                                    (SPADCALL
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 35))
                                     (|getShellEntry| $ 20)))))
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
             ((SPADCALL |ts| (|getShellEntry| $ 70)) |lp|)
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 32) |lp|
                             (|getShellEntry| $ 34))
                         |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                   (EXIT (COND
                           ((NULL |lp|) |lp|)
                           ((SPADCALL (ELT $ 35) |lp|
                                (|getShellEntry| $ 36))
                            (LIST (|spadConstant| $ 71)))
                           ('T
                            (SEQ (LETT |rs| NIL
                                       |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                 (SEQ G190
                                      (COND
                                        ((NULL
                                          (SPADCALL (NULL |lp|)
                                           (|getShellEntry| $ 20)))
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
                                          (|getShellEntry| $ 72))
                                         (|getShellEntry| $ 47))
                                        |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                       (EXIT
                                        (COND
                                          ((NULL
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 32)))
                                           (COND
                                             ((SPADCALL |p|
                                               (|getShellEntry| $ 35))
                                              (SEQ
                                               (LETT |lp| NIL
                                                |TSETCAT-;rewriteSetWithReduction;LSMML;14|)
                                               (EXIT
                                                (LETT |rs|
                                                 (LIST
                                                  (|spadConstant| $ 71))
                                                 |TSETCAT-;rewriteSetWithReduction;LSMML;14|))))
                                             ('T
                                              (LETT |rs|
                                               (CONS |p| |rs|)
                                               |TSETCAT-;rewriteSetWithReduction;LSMML;14|)))))))
                                      NIL (GO G190) G191 (EXIT NIL))
                                 (EXIT (SPADCALL |rs|
                                        (|getShellEntry| $ 48)))))))))))))) 

(DEFUN |TSETCAT-;stronglyReduce;PSP;15| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 74) (ELT $ 66) (|getShellEntry| $ 72))) 

(DEFUN |TSETCAT-;headReduce;PSP;16| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 76) (ELT $ 77) (|getShellEntry| $ 72))) 

(DEFUN |TSETCAT-;initiallyReduce;PSP;17| (|p| |ts| $)
  (SPADCALL |p| |ts| (ELT $ 79) (ELT $ 80) (|getShellEntry| $ 72))) 

(DEFUN |TSETCAT-;removeZero;PSP;18| (|p| |ts| $)
  (PROG (|v| |tsv-| #0=#:G1565 #1=#:G1574 |q|)
    (RETURN
      (SEQ (EXIT (COND
                   ((OR (SPADCALL |p| (|getShellEntry| $ 35))
                        (SPADCALL |ts| (|getShellEntry| $ 12)))
                    |p|)
                   ('T
                    (SEQ (LETT |v|
                               (SPADCALL |p| (|getShellEntry| $ 24))
                               |TSETCAT-;removeZero;PSP;18|)
                         (LETT |tsv-|
                               (SPADCALL |ts| |v|
                                   (|getShellEntry| $ 82))
                               |TSETCAT-;removeZero;PSP;18|)
                         (COND
                           ((SPADCALL |v| |ts| (|getShellEntry| $ 83))
                            (SEQ (LETT |q|
                                       (SPADCALL |p|
                                        (PROG2
                                         (LETT #0#
                                          (SPADCALL |ts| |v|
                                           (|getShellEntry| $ 84))
                                          |TSETCAT-;removeZero;PSP;18|)
                                         (QCDR #0#)
                                          (|check-union| (QEQCAR #0# 0)
                                           (|getShellEntry| $ 10) #0#))
                                        (|getShellEntry| $ 74))
                                       |TSETCAT-;removeZero;PSP;18|)
                                 (EXIT (COND
                                         ((SPADCALL |q|
                                           (|getShellEntry| $ 32))
                                          (PROGN
                                            (LETT #1# |q|
                                             |TSETCAT-;removeZero;PSP;18|)
                                            (GO #1#)))
                                         ((SPADCALL
                                           (SPADCALL |q| |tsv-|
                                            (|getShellEntry| $ 85))
                                           (|getShellEntry| $ 32))
                                          (PROGN
                                            (LETT #1#
                                             (|spadConstant| $ 86)
                                             |TSETCAT-;removeZero;PSP;18|)
                                            (GO #1#))))))))
                         (EXIT (COND
                                 ((SPADCALL |tsv-|
                                      (|getShellEntry| $ 12))
                                  |p|)
                                 ('T
                                  (SEQ (LETT |q| (|spadConstant| $ 86)
                                        |TSETCAT-;removeZero;PSP;18|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (SPADCALL
                                             (SPADCALL |p| |v|
                                              (|getShellEntry| $ 87))
                                             (|getShellEntry| $ 89)))
                                           (GO G191)))
                                        (SEQ
                                         (LETT |q|
                                          (SPADCALL
                                           (SPADCALL
                                            (SPADCALL
                                             (SPADCALL |p|
                                              (|getShellEntry| $ 46))
                                             |tsv-|
                                             (|getShellEntry| $ 85))
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 90))
                                            (|getShellEntry| $ 91))
                                           |q| (|getShellEntry| $ 92))
                                          |TSETCAT-;removeZero;PSP;18|)
                                         (EXIT
                                          (LETT |p|
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 93))
                                           |TSETCAT-;removeZero;PSP;18|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT
                                        (SPADCALL |q|
                                         (SPADCALL |p| |tsv-|
                                          (|getShellEntry| $ 85))
                                         (|getShellEntry| $ 92)))))))))))
           #1# (EXIT #1#))))) 

(DEFUN |TSETCAT-;reduceByQuasiMonic;PSP;19| (|p| |ts| $)
  (COND
    ((OR (SPADCALL |p| (|getShellEntry| $ 35))
         (SPADCALL |ts| (|getShellEntry| $ 12)))
     |p|)
    ('T
     (QVELT (SPADCALL |p| (SPADCALL |ts| (|getShellEntry| $ 95))
                (|getShellEntry| $ 97))
            1)))) 

(DEFUN |TSETCAT-;autoReduced?;SMB;20| (|ts| |redOp?| $)
  (PROG (|p| |lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) 'T)
             ('T
              (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 29))
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
  (SPADCALL |ts| (ELT $ 59) (|getShellEntry| $ 101))) 

(DEFUN |TSETCAT-;normalized?;SB;22| (|ts| $)
  (SPADCALL |ts| (ELT $ 57) (|getShellEntry| $ 101))) 

(DEFUN |TSETCAT-;headReduced?;SB;23| (|ts| $)
  (SPADCALL |ts| (ELT $ 104) (|getShellEntry| $ 101))) 

(DEFUN |TSETCAT-;initiallyReduced?;SB;24| (|ts| $)
  (SPADCALL |ts| (ELT $ 106) (|getShellEntry| $ 101))) 

(DEFUN |TSETCAT-;mvar;SV;25| (|ts| $)
  (PROG (#0=#:G1593)
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
             (|getShellEntry| $ 24))))))) 

(DEFUN |TSETCAT-;first;SU;26| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 23)
                             (SPADCALL |ts| (|getShellEntry| $ 29))
                             (|getShellEntry| $ 37))
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
                             (SPADCALL |ts| (|getShellEntry| $ 29))
                             (|getShellEntry| $ 37))
                         |TSETCAT-;last;SU;27|)
                   (EXIT (CONS 0 (|SPADfirst| |lp|)))))))))) 

(DEFUN |TSETCAT-;rest;SU;28| (|ts| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |ts| (|getShellEntry| $ 12)) (CONS 1 "failed"))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 23)
                             (SPADCALL |ts| (|getShellEntry| $ 29))
                             (|getShellEntry| $ 37))
                         |TSETCAT-;rest;SU;28|)
                   (EXIT (CONS 0
                               (SPADCALL (CDR |lp|)
                                   (|getShellEntry| $ 111))))))))))) 

(DEFUN |TSETCAT-;coerce;SL;29| (|ts| $)
  (SPADCALL (ELT $ 23) (SPADCALL |ts| (|getShellEntry| $ 29))
            (|getShellEntry| $ 37))) 

(DEFUN |TSETCAT-;algebraicVariables;SL;30| (|ts| $)
  (PROG (#0=#:G1618 |p| #1=#:G1619)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |TSETCAT-;algebraicVariables;SL;30|)
             (SEQ (LETT |p| NIL |TSETCAT-;algebraicVariables;SL;30|)
                  (LETT #1# (SPADCALL |ts| (|getShellEntry| $ 29))
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
                                     (|getShellEntry| $ 24))
                                    #0#)
                                   |TSETCAT-;algebraicVariables;SL;30|)))
                  (LETT #1# (CDR #1#)
                        |TSETCAT-;algebraicVariables;SL;30|)
                  (GO G190) G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |TSETCAT-;algebraic?;VSB;31| (|v| |ts| $)
  (SPADCALL |v| (SPADCALL |ts| (|getShellEntry| $ 116))
      (|getShellEntry| $ 117))) 

(DEFUN |TSETCAT-;select;SVU;32| (|ts| |v| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 23)
                     (SPADCALL |ts| (|getShellEntry| $ 29))
                     (|getShellEntry| $ 37))
                 |TSETCAT-;select;SVU;32|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (SPADCALL
                                (SPADCALL |v|
                                    (SPADCALL (|SPADfirst| |lp|)
                                     (|getShellEntry| $ 24))
                                    (|getShellEntry| $ 65))
                                (|getShellEntry| $ 20)))))
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
      (SEQ (LETT |lp| (SPADCALL |ts| (|getShellEntry| $ 29))
                 |TSETCAT-;collectQuasiMonic;2S;33|)
           (LETT |newlp| NIL |TSETCAT-;collectQuasiMonic;2S;33|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |lp|) (|getShellEntry| $ 20)))
                   (GO G191)))
                (SEQ (COND
                       ((SPADCALL
                            (SPADCALL (|SPADfirst| |lp|)
                                (|getShellEntry| $ 46))
                            (|getShellEntry| $ 35))
                        (LETT |newlp| (CONS (|SPADfirst| |lp|) |newlp|)
                              |TSETCAT-;collectQuasiMonic;2S;33|)))
                     (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;collectQuasiMonic;2S;33|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |newlp| (|getShellEntry| $ 111))))))) 

(DEFUN |TSETCAT-;collectUnder;SVS;34| (|ts| |v| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 23)
                     (SPADCALL |ts| (|getShellEntry| $ 29))
                     (|getShellEntry| $ 37))
                 |TSETCAT-;collectUnder;SVS;34|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (SPADCALL
                                (SPADCALL
                                    (SPADCALL (|SPADfirst| |lp|)
                                     (|getShellEntry| $ 24))
                                    |v| (|getShellEntry| $ 64))
                                (|getShellEntry| $ 20)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                 |TSETCAT-;collectUnder;SVS;34|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |lp| (|getShellEntry| $ 111))))))) 

(DEFUN |TSETCAT-;collectUpper;SVS;35| (|ts| |v| $)
  (PROG (|lp2| |lp1|)
    (RETURN
      (SEQ (LETT |lp1|
                 (SPADCALL (ELT $ 23)
                     (SPADCALL |ts| (|getShellEntry| $ 29))
                     (|getShellEntry| $ 37))
                 |TSETCAT-;collectUpper;SVS;35|)
           (LETT |lp2| NIL |TSETCAT-;collectUpper;SVS;35|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp1|) 'NIL)
                           ('T
                            (SPADCALL |v|
                                (SPADCALL (|SPADfirst| |lp1|)
                                    (|getShellEntry| $ 24))
                                (|getShellEntry| $ 64)))))
                   (GO G191)))
                (SEQ (LETT |lp2| (CONS (|SPADfirst| |lp1|) |lp2|)
                           |TSETCAT-;collectUpper;SVS;35|)
                     (EXIT (LETT |lp1| (CDR |lp1|)
                                 |TSETCAT-;collectUpper;SVS;35|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL (REVERSE |lp2|) (|getShellEntry| $ 111))))))) 

(DEFUN |TSETCAT-;construct;LS;36| (|lp| $)
  (PROG (|rif|)
    (RETURN
      (SEQ (LETT |rif| (SPADCALL |lp| (|getShellEntry| $ 123))
                 |TSETCAT-;construct;LS;36|)
           (EXIT (COND
                   ((QEQCAR |rif| 0) (QCDR |rif|))
                   ('T
                    (|error| "in construct : LP -> $ from TSETCAT : bad arg")))))))) 

(DEFUN |TSETCAT-;retractIfCan;LU;37| (|lp| $)
  (PROG (|rif|)
    (RETURN
      (SEQ (COND
             ((NULL |lp|) (CONS 0 (SPADCALL (|getShellEntry| $ 38))))
             ('T
              (SEQ (LETT |lp|
                         (SPADCALL (ELT $ 23) |lp|
                             (|getShellEntry| $ 37))
                         |TSETCAT-;retractIfCan;LU;37|)
                   (LETT |rif|
                         (SPADCALL (CDR |lp|) (|getShellEntry| $ 123))
                         |TSETCAT-;retractIfCan;LU;37|)
                   (EXIT (COND
                           ((QEQCAR |rif| 0)
                            (SPADCALL (QCDR |rif|) (|SPADfirst| |lp|)
                                (|getShellEntry| $ 125)))
                           ('T
                            (|error| "in retractIfCan : LP -> ... from TSETCAT : bad arg"))))))))))) 

(DEFUN |TSETCAT-;extend;SPS;38| (|ts| |p| $)
  (PROG (|eif|)
    (RETURN
      (SEQ (LETT |eif| (SPADCALL |ts| |p| (|getShellEntry| $ 125))
                 |TSETCAT-;extend;SPS;38|)
           (EXIT (COND
                   ((QEQCAR |eif| 0) (QCDR |eif|))
                   ('T
                    (|error| "in extend : ($,P) -> $ from TSETCAT : bad ars")))))))) 

(DEFUN |TSETCAT-;coHeight;SNni;39| (|ts| $)
  (PROG (|n| |m| #0=#:G1659)
    (RETURN
      (SEQ (LETT |n| (SPADCALL (|getShellEntry| $ 128))
                 |TSETCAT-;coHeight;SNni;39|)
           (LETT |m| (LENGTH (SPADCALL |ts| (|getShellEntry| $ 29)))
                 |TSETCAT-;coHeight;SNni;39|)
           (EXIT (PROG2 (LETT #0#
                              (SPADCALL |n| |m|
                                  (|getShellEntry| $ 129))
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
        (LETT $ (|newShell| 132) . #0#)
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
           (|setShellEntry| $ 130
               (CONS (|dispatchFunction| |TSETCAT-;coHeight;SNni;39|)
                     $))))
        $)))) 

(MAKEPROP '|TriangularSetCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|local| |#3|) (|local| |#4|) (|local| |#5|) (|Boolean|)
             (0 . |empty?|) (|Union| 10 '"failed") (5 . |first|)
             (10 . =) (|Union| $ '"failed") (16 . |rest|) (21 . =)
             |TSETCAT-;=;2SB;1| (27 . |not|) (32 . |last|)
             (37 . |infRittWu?|) (43 . |supRittWu?|) (49 . |mvar|)
             (54 . |collectUpper|) (60 . |infRittWu?|)
             |TSETCAT-;infRittWu?;2SB;2| (|List| 10) (66 . |members|)
             (|Mapping| 11 10 10) |TSETCAT-;reduced?;PSMB;3|
             (71 . |zero?|) (|Mapping| 11 10) (76 . |remove|)
             (82 . |ground?|) (87 . |any?|) (93 . |sort|)
             (99 . |empty|) (103 . |extend|) (109 . |reduced?|)
             (|Record| (|:| |bas| $) (|:| |top| 28))
             (|Union| 41 '"failed") |TSETCAT-;basicSet;LMU;4|
             (116 . |concat|) |TSETCAT-;basicSet;LMMU;5| (122 . |init|)
             (127 . |primPartElseUnitCanonical|)
             (132 . |removeDuplicates|) |TSETCAT-;initials;SL;6|
             (|NonNegativeInteger|) (137 . |mdeg|)
             |TSETCAT-;degree;SNni;7| (142 . |initials|)
             (|Record| (|:| |close| 28) (|:| |open| 28))
             |TSETCAT-;quasiComponent;SR;8| (|List| $)
             (147 . |normalized?|) |TSETCAT-;normalized?;PSB;9|
             (153 . |reduced?|) |TSETCAT-;stronglyReduced?;PSB;10|
             (159 . |head|) (164 . |stronglyReduced?|)
             |TSETCAT-;headReduced?;PSB;11| (170 . <) (176 . =)
             (182 . |reduced?|) |TSETCAT-;initiallyReduced?;PSB;12|
             (|Mapping| 10 10 10) |TSETCAT-;reduce;PSMMP;13|
             (188 . |trivialIdeal?|) (193 . |One|) (197 . |reduce|)
             |TSETCAT-;rewriteSetWithReduction;LSMML;14|
             (205 . |lazyPrem|) |TSETCAT-;stronglyReduce;PSP;15|
             (211 . |headReduce|) (217 . |headReduced?|)
             |TSETCAT-;headReduce;PSP;16| (223 . |initiallyReduce|)
             (229 . |initiallyReduced?|)
             |TSETCAT-;initiallyReduce;PSP;17| (235 . |collectUnder|)
             (241 . |algebraic?|) (247 . |select|) (253 . |removeZero|)
             (259 . |Zero|) (263 . |degree|) (|Integer|)
             (269 . |positive?|) (274 . |mainMonomial|) (279 . *)
             (285 . +) (291 . |tail|) |TSETCAT-;removeZero;PSP;18|
             (296 . |collectQuasiMonic|)
             (|Record| (|:| |rnum| 7) (|:| |polnum| 10) (|:| |den| 7))
             (301 . |remainder|) |TSETCAT-;reduceByQuasiMonic;PSP;19|
             (|Mapping| 11 10 28) |TSETCAT-;autoReduced?;SMB;20|
             (307 . |autoReduced?|) |TSETCAT-;stronglyReduced?;SB;21|
             |TSETCAT-;normalized?;SB;22| (313 . |headReduced?|)
             |TSETCAT-;headReduced?;SB;23| (319 . |initiallyReduced?|)
             |TSETCAT-;initiallyReduced?;SB;24| |TSETCAT-;mvar;SV;25|
             |TSETCAT-;first;SU;26| |TSETCAT-;last;SU;27|
             (325 . |construct|) |TSETCAT-;rest;SU;28|
             |TSETCAT-;coerce;SL;29| (|List| 9)
             |TSETCAT-;algebraicVariables;SL;30|
             (330 . |algebraicVariables|) (335 . |member?|)
             |TSETCAT-;algebraic?;VSB;31| |TSETCAT-;select;SVU;32|
             |TSETCAT-;collectQuasiMonic;2S;33|
             |TSETCAT-;collectUnder;SVS;34|
             |TSETCAT-;collectUpper;SVS;35| (341 . |retractIfCan|)
             |TSETCAT-;construct;LS;36| (346 . |extendIfCan|)
             |TSETCAT-;retractIfCan;LU;37| |TSETCAT-;extend;SPS;38|
             (352 . |size|) (356 . |subtractIfCan|) (362 . |coHeight|)
             (|OutputForm|))
          '#(|stronglyReduced?| 367 |stronglyReduce| 378 |select| 384
             |rewriteSetWithReduction| 390 |retractIfCan| 398 |rest|
             403 |removeZero| 408 |reduced?| 414 |reduceByQuasiMonic|
             421 |reduce| 427 |quasiComponent| 435 |normalized?| 440
             |mvar| 451 |last| 456 |initials| 461 |initiallyReduced?|
             466 |initiallyReduce| 477 |infRittWu?| 483 |headReduced?|
             489 |headReduce| 500 |first| 506 |extend| 511 |degree| 517
             |construct| 522 |collectUpper| 527 |collectUnder| 533
             |collectQuasiMonic| 539 |coerce| 544 |coHeight| 549
             |basicSet| 554 |autoReduced?| 567 |algebraicVariables| 573
             |algebraic?| 578 = 584)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 130
                                '(1 6 11 0 12 1 6 13 0 14 2 10 11 0 0
                                  15 1 6 16 0 17 2 6 11 0 0 18 1 11 0 0
                                  20 1 6 13 0 21 2 10 11 0 0 22 2 10 11
                                  0 0 23 1 10 9 0 24 2 6 0 0 9 25 2 6
                                  11 0 0 26 1 6 28 0 29 1 10 11 0 32 2
                                  28 0 33 0 34 1 10 11 0 35 2 28 11 33
                                  0 36 2 28 0 30 0 37 0 6 0 38 2 6 0 0
                                  10 39 3 6 11 10 0 30 40 2 28 0 0 0 44
                                  1 10 0 0 46 1 10 0 0 47 1 28 0 0 48 1
                                  10 50 0 51 1 6 28 0 53 2 10 11 0 56
                                  57 2 10 11 0 56 59 1 10 0 0 61 2 6 11
                                  10 0 62 2 9 11 0 0 64 2 9 11 0 0 65 2
                                  10 11 0 0 66 1 6 11 0 70 0 10 0 71 4
                                  6 10 10 0 68 30 72 2 10 0 0 0 74 2 10
                                  0 0 0 76 2 10 11 0 0 77 2 10 0 0 0 79
                                  2 10 11 0 0 80 2 6 0 0 9 82 2 6 11 9
                                  0 83 2 6 13 0 9 84 2 6 10 10 0 85 0
                                  10 0 86 2 10 50 0 9 87 1 88 11 0 89 1
                                  10 0 0 90 2 10 0 0 0 91 2 10 0 0 0 92
                                  1 10 0 0 93 1 6 0 0 95 2 6 96 10 0 97
                                  2 6 11 0 99 101 2 10 11 0 56 104 2 10
                                  11 0 56 106 1 6 0 28 111 1 6 114 0
                                  116 2 114 11 9 0 117 1 6 16 28 123 2
                                  6 16 0 10 125 0 9 50 128 2 50 16 0 0
                                  129 1 0 50 0 130 1 0 11 0 102 2 0 11
                                  10 0 60 2 0 10 10 0 75 2 0 13 0 9 119
                                  4 0 28 28 0 68 30 73 1 0 16 28 126 1
                                  0 16 0 112 2 0 10 10 0 94 3 0 11 10 0
                                  30 31 2 0 10 10 0 98 4 0 10 10 0 68
                                  30 69 1 0 54 0 55 1 0 11 0 103 2 0 11
                                  10 0 58 1 0 9 0 108 1 0 13 0 110 1 0
                                  28 0 49 1 0 11 0 107 2 0 11 10 0 67 2
                                  0 10 10 0 81 2 0 11 0 0 27 1 0 11 0
                                  105 2 0 11 10 0 63 2 0 10 10 0 78 1 0
                                  13 0 109 2 0 0 0 10 127 1 0 50 0 52 1
                                  0 0 28 124 2 0 0 0 9 122 2 0 0 0 9
                                  121 1 0 0 0 120 1 0 28 0 113 1 0 50 0
                                  130 3 0 42 28 33 30 45 2 0 42 28 30
                                  43 2 0 11 0 99 100 1 0 114 0 115 2 0
                                  11 9 0 118 2 0 11 0 0 19)))))
          '|lookupComplete|)) 
