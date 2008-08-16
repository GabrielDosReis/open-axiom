
(/VERSIONCHECK 2) 

(DEFUN |POLYCAT-;eval;SLS;1| (|p| |l| $)
  (PROG (#0=#:G1427 #1=#:G1421 #2=#:G1428 #3=#:G1429 |lvar| #4=#:G1430
            |e| #5=#:G1431)
    (RETURN
      (SEQ (COND
             ((NULL |l|) |p|)
             ('T
              (SEQ (SEQ (EXIT (SEQ (LETT |e| NIL |POLYCAT-;eval;SLS;1|)
                                   (LETT #0# |l| |POLYCAT-;eval;SLS;1|)
                                   G190
                                   (COND
                                     ((OR (ATOM #0#)
                                       (PROGN
                                         (LETT |e| (CAR #0#)
                                          |POLYCAT-;eval;SLS;1|)
                                         NIL))
                                      (GO G191)))
                                   (SEQ
                                    (EXIT
                                     (COND
                                       ((QEQCAR
                                         (SPADCALL
                                          (SPADCALL |e|
                                           (|getShellEntry| $ 11))
                                          (|getShellEntry| $ 13))
                                         1)
                                        (PROGN
                                          (LETT #1#
                                           (|error|
                                            "cannot find a variable to evaluate")
                                           |POLYCAT-;eval;SLS;1|)
                                          (GO #1#))))))
                                   (LETT #0# (CDR #0#)
                                    |POLYCAT-;eval;SLS;1|)
                                   (GO G190) G191 (EXIT NIL)))
                        #1# (EXIT #1#))
                   (LETT |lvar|
                         (PROGN
                           (LETT #2# NIL |POLYCAT-;eval;SLS;1|)
                           (SEQ (LETT |e| NIL |POLYCAT-;eval;SLS;1|)
                                (LETT #3# |l| |POLYCAT-;eval;SLS;1|)
                                G190
                                (COND
                                  ((OR (ATOM #3#)
                                    (PROGN
                                      (LETT |e| (CAR #3#)
                                       |POLYCAT-;eval;SLS;1|)
                                      NIL))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (LETT #2#
                                       (CONS
                                        (SPADCALL
                                         (SPADCALL |e|
                                          (|getShellEntry| $ 11))
                                         (|getShellEntry| $ 14))
                                        #2#)
                                       |POLYCAT-;eval;SLS;1|)))
                                (LETT #3# (CDR #3#)
                                      |POLYCAT-;eval;SLS;1|)
                                (GO G190) G191 (EXIT (NREVERSE0 #2#))))
                         |POLYCAT-;eval;SLS;1|)
                   (EXIT (SPADCALL |p| |lvar|
                             (PROGN
                               (LETT #4# NIL |POLYCAT-;eval;SLS;1|)
                               (SEQ (LETT |e| NIL
                                     |POLYCAT-;eval;SLS;1|)
                                    (LETT #5# |l|
                                     |POLYCAT-;eval;SLS;1|)
                                    G190
                                    (COND
                                      ((OR (ATOM #5#)
                                        (PROGN
                                          (LETT |e| (CAR #5#)
                                           |POLYCAT-;eval;SLS;1|)
                                          NIL))
                                       (GO G191)))
                                    (SEQ
                                     (EXIT
                                      (LETT #4#
                                       (CONS
                                        (SPADCALL |e|
                                         (|getShellEntry| $ 15))
                                        #4#)
                                       |POLYCAT-;eval;SLS;1|)))
                                    (LETT #5# (CDR #5#)
                                     |POLYCAT-;eval;SLS;1|)
                                    (GO G190) G191
                                    (EXIT (NREVERSE0 #4#))))
                             (|getShellEntry| $ 18)))))))))) 

(DEFUN |POLYCAT-;monomials;SL;2| (|p| $)
  (PROG (|ml|)
    (RETURN
      (SEQ (LETT |ml| NIL |POLYCAT-;monomials;SL;2|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL |p| (|spadConstant| $ 22)
                             (|getShellEntry| $ 25)))
                   (GO G191)))
                (SEQ (LETT |ml|
                           (CONS (SPADCALL |p| (|getShellEntry| $ 26))
                                 |ml|)
                           |POLYCAT-;monomials;SL;2|)
                     (EXIT (LETT |p|
                                 (SPADCALL |p| (|getShellEntry| $ 27))
                                 |POLYCAT-;monomials;SL;2|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (REVERSE |ml|)))))) 

(DEFUN |POLYCAT-;isPlus;SU;3| (|p| $)
  (PROG (|l|)
    (RETURN
      (COND
        ((NULL (CDR (LETT |l| (SPADCALL |p| (|getShellEntry| $ 29))
                          |POLYCAT-;isPlus;SU;3|)))
         (CONS 1 "failed"))
        ('T (CONS 0 |l|)))))) 

(DEFUN |POLYCAT-;isTimes;SU;4| (|p| $)
  (PROG (|lv| #0=#:G1453 |v| #1=#:G1454 |l| |r|)
    (RETURN
      (SEQ (COND
             ((OR (NULL (LETT |lv|
                              (SPADCALL |p| (|getShellEntry| $ 32))
                              |POLYCAT-;isTimes;SU;4|))
                  (NULL (SPADCALL |p| (|getShellEntry| $ 33))))
              (CONS 1 "failed"))
             ('T
              (SEQ (LETT |l|
                         (PROGN
                           (LETT #0# NIL |POLYCAT-;isTimes;SU;4|)
                           (SEQ (LETT |v| NIL |POLYCAT-;isTimes;SU;4|)
                                (LETT #1# |lv| |POLYCAT-;isTimes;SU;4|)
                                G190
                                (COND
                                  ((OR (ATOM #1#)
                                    (PROGN
                                      (LETT |v| (CAR #1#)
                                       |POLYCAT-;isTimes;SU;4|)
                                      NIL))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (LETT #0#
                                       (CONS
                                        (SPADCALL (|spadConstant| $ 34)
                                         |v|
                                         (SPADCALL |p| |v|
                                          (|getShellEntry| $ 37))
                                         (|getShellEntry| $ 38))
                                        #0#)
                                       |POLYCAT-;isTimes;SU;4|)))
                                (LETT #1# (CDR #1#)
                                      |POLYCAT-;isTimes;SU;4|)
                                (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                         |POLYCAT-;isTimes;SU;4|)
                   (LETT |r| (SPADCALL |p| (|getShellEntry| $ 39))
                         |POLYCAT-;isTimes;SU;4|)
                   (EXIT (COND
                           ((SPADCALL |r| (|spadConstant| $ 35)
                                (|getShellEntry| $ 40))
                            (COND
                              ((NULL (CDR |lv|)) (CONS 1 "failed"))
                              ('T (CONS 0 |l|))))
                           ('T
                            (CONS 0
                                  (CONS (SPADCALL |r|
                                         (|getShellEntry| $ 41))
                                        |l|)))))))))))) 

(DEFUN |POLYCAT-;isExpt;SU;5| (|p| $)
  (PROG (|u| |d|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |p| (|getShellEntry| $ 43))
                 |POLYCAT-;isExpt;SU;5|)
           (EXIT (COND
                   ((OR (QEQCAR |u| 1)
                        (NULL (SPADCALL |p|
                                  (SPADCALL (|spadConstant| $ 34)
                                      (QCDR |u|)
                                      (LETT |d|
                                       (SPADCALL |p| (QCDR |u|)
                                        (|getShellEntry| $ 37))
                                       |POLYCAT-;isExpt;SU;5|)
                                      (|getShellEntry| $ 38))
                                  (|getShellEntry| $ 44))))
                    (CONS 1 "failed"))
                   ('T (CONS 0 (CONS (QCDR |u|) |d|))))))))) 

(DEFUN |POLYCAT-;coefficient;SVarSetNniS;6| (|p| |v| |n| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 49)) |n|
      (|getShellEntry| $ 51))) 

(DEFUN |POLYCAT-;coefficient;SLLS;7| (|p| |lv| |ln| $)
  (COND
    ((NULL |lv|)
     (COND
       ((NULL |ln|) |p|)
       ('T (|error| "mismatched lists in coefficient"))))
    ((NULL |ln|) (|error| "mismatched lists in coefficient"))
    ('T
     (SPADCALL
         (SPADCALL
             (SPADCALL |p| (|SPADfirst| |lv|) (|getShellEntry| $ 49))
             (|SPADfirst| |ln|) (|getShellEntry| $ 51))
         (CDR |lv|) (CDR |ln|) (|getShellEntry| $ 54))))) 

(DEFUN |POLYCAT-;monomial;SLLS;8| (|p| |lv| |ln| $)
  (COND
    ((NULL |lv|)
     (COND
       ((NULL |ln|) |p|)
       ('T (|error| "mismatched lists in monomial"))))
    ((NULL |ln|) (|error| "mismatched lists in monomial"))
    ('T
     (SPADCALL
         (SPADCALL |p| (|SPADfirst| |lv|) (|SPADfirst| |ln|)
             (|getShellEntry| $ 38))
         (CDR |lv|) (CDR |ln|) (|getShellEntry| $ 56))))) 

(DEFUN |POLYCAT-;retract;SVarSet;9| (|p| $)
  (PROG (#0=#:G1479 |q|)
    (RETURN
      (SEQ (LETT |q|
                 (PROG2 (LETT #0# (SPADCALL |p| (|getShellEntry| $ 43))
                              |POLYCAT-;retract;SVarSet;9|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 9)
                       #0#))
                 |POLYCAT-;retract;SVarSet;9|)
           (EXIT (COND
                   ((SPADCALL (SPADCALL |q| (|getShellEntry| $ 58)) |p|
                        (|getShellEntry| $ 44))
                    |q|)
                   ('T (|error| "Polynomial is not a single variable")))))))) 

(DEFUN |POLYCAT-;retractIfCan;SU;10| (|p| $)
  (PROG (|q| #0=#:G1487)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |q|
                                 (SPADCALL |p| (|getShellEntry| $ 43))
                                 |POLYCAT-;retractIfCan;SU;10|)
                           (EXIT (COND
                                   ((QEQCAR |q| 0)
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL (QCDR |q|)
                                         (|getShellEntry| $ 58))
                                        |p| (|getShellEntry| $ 44))
                                       (PROGN
                                         (LETT #0# |q|
                                          |POLYCAT-;retractIfCan;SU;10|)
                                         (GO #0#))))))))
                      (EXIT (CONS 1 "failed"))))
           #0# (EXIT #0#))))) 

(DEFUN |POLYCAT-;mkPrim| (|p| $)
  (SPADCALL (|spadConstant| $ 35) (SPADCALL |p| (|getShellEntry| $ 61))
      (|getShellEntry| $ 62))) 

(DEFUN |POLYCAT-;primitiveMonomials;SL;12| (|p| $)
  (PROG (#0=#:G1492 |q| #1=#:G1493)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |POLYCAT-;primitiveMonomials;SL;12|)
             (SEQ (LETT |q| NIL |POLYCAT-;primitiveMonomials;SL;12|)
                  (LETT #1# (SPADCALL |p| (|getShellEntry| $ 29))
                        |POLYCAT-;primitiveMonomials;SL;12|)
                  G190
                  (COND
                    ((OR (ATOM #1#)
                         (PROGN
                           (LETT |q| (CAR #1#)
                                 |POLYCAT-;primitiveMonomials;SL;12|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (LETT #0#
                                   (CONS (|POLYCAT-;mkPrim| |q| $) #0#)
                                   |POLYCAT-;primitiveMonomials;SL;12|)))
                  (LETT #1# (CDR #1#)
                        |POLYCAT-;primitiveMonomials;SL;12|)
                  (GO G190) G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |POLYCAT-;totalDegree;SNni;13| (|p| $)
  (PROG (#0=#:G1495 |d| |u|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 64)) 0)
             ('T
              (SEQ (LETT |u|
                         (SPADCALL |p|
                             (PROG2 (LETT #0#
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 43))
                                     |POLYCAT-;totalDegree;SNni;13|)
                                    (QCDR #0#)
                               (|check-union| (QEQCAR #0# 0)
                                   (|getShellEntry| $ 9) #0#))
                             (|getShellEntry| $ 49))
                         |POLYCAT-;totalDegree;SNni;13|)
                   (LETT |d| 0 |POLYCAT-;totalDegree;SNni;13|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL |u| (|spadConstant| $ 65)
                                     (|getShellEntry| $ 66)))
                           (GO G191)))
                        (SEQ (LETT |d|
                                   (MAX |d|
                                    (+
                                     (SPADCALL |u|
                                      (|getShellEntry| $ 67))
                                     (SPADCALL
                                      (SPADCALL |u|
                                       (|getShellEntry| $ 68))
                                      (|getShellEntry| $ 69))))
                                   |POLYCAT-;totalDegree;SNni;13|)
                             (EXIT (LETT |u|
                                    (SPADCALL |u|
                                     (|getShellEntry| $ 70))
                                    |POLYCAT-;totalDegree;SNni;13|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |POLYCAT-;totalDegree;SLNni;14| (|p| |lv| $)
  (PROG (#0=#:G1503 |v| |w| |d| |u|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 64)) 0)
             ('T
              (SEQ (LETT |u|
                         (SPADCALL |p|
                             (LETT |v|
                                   (PROG2
                                    (LETT #0#
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 43))
                                     |POLYCAT-;totalDegree;SLNni;14|)
                                    (QCDR #0#)
                                     (|check-union| (QEQCAR #0# 0)
                                      (|getShellEntry| $ 9) #0#))
                                   |POLYCAT-;totalDegree;SLNni;14|)
                             (|getShellEntry| $ 49))
                         |POLYCAT-;totalDegree;SLNni;14|)
                   (LETT |d| 0 |POLYCAT-;totalDegree;SLNni;14|)
                   (LETT |w| 0 |POLYCAT-;totalDegree;SLNni;14|)
                   (COND
                     ((SPADCALL |v| |lv| (|getShellEntry| $ 72))
                      (LETT |w| 1 |POLYCAT-;totalDegree;SLNni;14|)))
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL |u| (|spadConstant| $ 65)
                                     (|getShellEntry| $ 66)))
                           (GO G191)))
                        (SEQ (LETT |d|
                                   (MAX |d|
                                    (+
                                     (* |w|
                                      (SPADCALL |u|
                                       (|getShellEntry| $ 67)))
                                     (SPADCALL
                                      (SPADCALL |u|
                                       (|getShellEntry| $ 68))
                                      |lv| (|getShellEntry| $ 73))))
                                   |POLYCAT-;totalDegree;SLNni;14|)
                             (EXIT (LETT |u|
                                    (SPADCALL |u|
                                     (|getShellEntry| $ 70))
                                    |POLYCAT-;totalDegree;SLNni;14|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |POLYCAT-;resultant;2SVarSetS;15| (|p1| |p2| |mvar| $)
  (SPADCALL (SPADCALL |p1| |mvar| (|getShellEntry| $ 49))
      (SPADCALL |p2| |mvar| (|getShellEntry| $ 49))
      (|getShellEntry| $ 75))) 

(DEFUN |POLYCAT-;discriminant;SVarSetS;16| (|p| |var| $)
  (SPADCALL (SPADCALL |p| |var| (|getShellEntry| $ 49))
      (|getShellEntry| $ 77))) 

(DEFUN |POLYCAT-;allMonoms| (|l| $)
  (PROG (#0=#:G1515 |p| #1=#:G1516)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (PROGN
                     (LETT #0# NIL |POLYCAT-;allMonoms|)
                     (SEQ (LETT |p| NIL |POLYCAT-;allMonoms|)
                          (LETT #1# |l| |POLYCAT-;allMonoms|) G190
                          (COND
                            ((OR (ATOM #1#)
                                 (PROGN
                                   (LETT |p| (CAR #1#)
                                    |POLYCAT-;allMonoms|)
                                   NIL))
                             (GO G191)))
                          (SEQ (EXIT (LETT #0#
                                      (CONS
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 79))
                                       #0#)
                                      |POLYCAT-;allMonoms|)))
                          (LETT #1# (CDR #1#) |POLYCAT-;allMonoms|)
                          (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                   (|getShellEntry| $ 81))
               (|getShellEntry| $ 82)))))) 

(DEFUN |POLYCAT-;P2R| (|p| |b| |n| $)
  (PROG (|w| |bj| #0=#:G1521 |i| #1=#:G1520)
    (RETURN
      (SEQ (LETT |w|
                 (SPADCALL |n| (|spadConstant| $ 23)
                     (|getShellEntry| $ 84))
                 |POLYCAT-;P2R|)
           (SEQ (LETT |bj| NIL |POLYCAT-;P2R|)
                (LETT #0# |b| |POLYCAT-;P2R|)
                (LETT |i| (SPADCALL |w| (|getShellEntry| $ 86))
                      |POLYCAT-;P2R|)
                (LETT #1# (QVSIZE |w|) |POLYCAT-;P2R|) G190
                (COND
                  ((OR (> |i| #1#) (ATOM #0#)
                       (PROGN
                         (LETT |bj| (CAR #0#) |POLYCAT-;P2R|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (SPADCALL |w| |i|
                               (SPADCALL |p| |bj|
                                   (|getShellEntry| $ 87))
                               (|getShellEntry| $ 88))))
                (LETT |i|
                      (PROG1 (+ |i| 1)
                        (LETT #0# (CDR #0#) |POLYCAT-;P2R|))
                      |POLYCAT-;P2R|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |w|))))) 

(DEFUN |POLYCAT-;eq2R| (|l| |b| $)
  (PROG (#0=#:G1525 |bj| #1=#:G1526 #2=#:G1527 |p| #3=#:G1528)
    (RETURN
      (SEQ (SPADCALL
               (PROGN
                 (LETT #0# NIL |POLYCAT-;eq2R|)
                 (SEQ (LETT |bj| NIL |POLYCAT-;eq2R|)
                      (LETT #1# |b| |POLYCAT-;eq2R|) G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |bj| (CAR #1#) |POLYCAT-;eq2R|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (PROGN
                                          (LETT #2# NIL
                                           |POLYCAT-;eq2R|)
                                          (SEQ
                                           (LETT |p| NIL
                                            |POLYCAT-;eq2R|)
                                           (LETT #3# |l|
                                            |POLYCAT-;eq2R|)
                                           G190
                                           (COND
                                             ((OR (ATOM #3#)
                                               (PROGN
                                                 (LETT |p| (CAR #3#)
                                                  |POLYCAT-;eq2R|)
                                                 NIL))
                                              (GO G191)))
                                           (SEQ
                                            (EXIT
                                             (LETT #2#
                                              (CONS
                                               (SPADCALL |p| |bj|
                                                (|getShellEntry| $ 87))
                                               #2#)
                                              |POLYCAT-;eq2R|)))
                                           (LETT #3# (CDR #3#)
                                            |POLYCAT-;eq2R|)
                                           (GO G190) G191
                                           (EXIT (NREVERSE0 #2#))))
                                        #0#)
                                       |POLYCAT-;eq2R|)))
                      (LETT #1# (CDR #1#) |POLYCAT-;eq2R|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 92)))))) 

(DEFUN |POLYCAT-;reducedSystem;MM;20| (|m| $)
  (PROG (#0=#:G1537 |r| #1=#:G1538 |b| #2=#:G1539 |bj| #3=#:G1540 |d|
            |mm| |l|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |m| (|getShellEntry| $ 95))
                 |POLYCAT-;reducedSystem;MM;20|)
           (LETT |b|
                 (SPADCALL
                     (SPADCALL
                         (PROGN
                           (LETT #0# NIL
                                 |POLYCAT-;reducedSystem;MM;20|)
                           (SEQ (LETT |r| NIL
                                      |POLYCAT-;reducedSystem;MM;20|)
                                (LETT #1# |l|
                                      |POLYCAT-;reducedSystem;MM;20|)
                                G190
                                (COND
                                  ((OR (ATOM #1#)
                                    (PROGN
                                      (LETT |r| (CAR #1#)
                                       |POLYCAT-;reducedSystem;MM;20|)
                                      NIL))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (LETT #0#
                                       (CONS
                                        (|POLYCAT-;allMonoms| |r| $)
                                        #0#)
                                       |POLYCAT-;reducedSystem;MM;20|)))
                                (LETT #1# (CDR #1#)
                                      |POLYCAT-;reducedSystem;MM;20|)
                                (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                         (|getShellEntry| $ 81))
                     (|getShellEntry| $ 82))
                 |POLYCAT-;reducedSystem;MM;20|)
           (LETT |d|
                 (PROGN
                   (LETT #2# NIL |POLYCAT-;reducedSystem;MM;20|)
                   (SEQ (LETT |bj| NIL |POLYCAT-;reducedSystem;MM;20|)
                        (LETT #3# |b| |POLYCAT-;reducedSystem;MM;20|)
                        G190
                        (COND
                          ((OR (ATOM #3#)
                               (PROGN
                                 (LETT |bj| (CAR #3#)
                                       |POLYCAT-;reducedSystem;MM;20|)
                                 NIL))
                           (GO G191)))
                        (SEQ (EXIT (LETT #2#
                                    (CONS
                                     (SPADCALL |bj|
                                      (|getShellEntry| $ 61))
                                     #2#)
                                    |POLYCAT-;reducedSystem;MM;20|)))
                        (LETT #3# (CDR #3#)
                              |POLYCAT-;reducedSystem;MM;20|)
                        (GO G190) G191 (EXIT (NREVERSE0 #2#))))
                 |POLYCAT-;reducedSystem;MM;20|)
           (LETT |mm| (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d| $)
                 |POLYCAT-;reducedSystem;MM;20|)
           (LETT |l| (CDR |l|) |POLYCAT-;reducedSystem;MM;20|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |l|) (|getShellEntry| $ 96)))
                   (GO G191)))
                (SEQ (LETT |mm|
                           (SPADCALL |mm|
                               (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d|
                                   $)
                               (|getShellEntry| $ 97))
                           |POLYCAT-;reducedSystem;MM;20|)
                     (EXIT (LETT |l| (CDR |l|)
                                 |POLYCAT-;reducedSystem;MM;20|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |mm|))))) 

(DEFUN |POLYCAT-;reducedSystem;MVR;21| (|m| |v| $)
  (PROG (#0=#:G1551 |s| #1=#:G1552 |b| #2=#:G1553 |bj| #3=#:G1554 |d|
            |n| |mm| |w| |l| |r|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |m| (|getShellEntry| $ 95))
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |r| (SPADCALL |v| (|getShellEntry| $ 101))
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |b|
                 (SPADCALL
                     (SPADCALL (|POLYCAT-;allMonoms| |r| $)
                         (SPADCALL
                             (PROGN
                               (LETT #0# NIL
                                     |POLYCAT-;reducedSystem;MVR;21|)
                               (SEQ (LETT |s| NIL
                                     |POLYCAT-;reducedSystem;MVR;21|)
                                    (LETT #1# |l|
                                     |POLYCAT-;reducedSystem;MVR;21|)
                                    G190
                                    (COND
                                      ((OR (ATOM #1#)
                                        (PROGN
                                          (LETT |s| (CAR #1#)
                                           |POLYCAT-;reducedSystem;MVR;21|)
                                          NIL))
                                       (GO G191)))
                                    (SEQ
                                     (EXIT
                                      (LETT #0#
                                       (CONS
                                        (|POLYCAT-;allMonoms| |s| $)
                                        #0#)
                                       |POLYCAT-;reducedSystem;MVR;21|)))
                                    (LETT #1# (CDR #1#)
                                     |POLYCAT-;reducedSystem;MVR;21|)
                                    (GO G190) G191
                                    (EXIT (NREVERSE0 #0#))))
                             (|getShellEntry| $ 81))
                         (|getShellEntry| $ 102))
                     (|getShellEntry| $ 82))
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |d|
                 (PROGN
                   (LETT #2# NIL |POLYCAT-;reducedSystem;MVR;21|)
                   (SEQ (LETT |bj| NIL |POLYCAT-;reducedSystem;MVR;21|)
                        (LETT #3# |b| |POLYCAT-;reducedSystem;MVR;21|)
                        G190
                        (COND
                          ((OR (ATOM #3#)
                               (PROGN
                                 (LETT |bj| (CAR #3#)
                                       |POLYCAT-;reducedSystem;MVR;21|)
                                 NIL))
                           (GO G191)))
                        (SEQ (EXIT (LETT #2#
                                    (CONS
                                     (SPADCALL |bj|
                                      (|getShellEntry| $ 61))
                                     #2#)
                                    |POLYCAT-;reducedSystem;MVR;21|)))
                        (LETT #3# (CDR #3#)
                              |POLYCAT-;reducedSystem;MVR;21|)
                        (GO G190) G191 (EXIT (NREVERSE0 #2#))))
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |n| (LENGTH |d|) |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |mm| (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d| $)
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |w| (|POLYCAT-;P2R| (|SPADfirst| |r|) |d| |n| $)
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |l| (CDR |l|) |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |r| (CDR |r|) |POLYCAT-;reducedSystem;MVR;21|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |l|) (|getShellEntry| $ 96)))
                   (GO G191)))
                (SEQ (LETT |mm|
                           (SPADCALL |mm|
                               (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d|
                                   $)
                               (|getShellEntry| $ 97))
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (LETT |w|
                           (SPADCALL |w|
                               (|POLYCAT-;P2R| (|SPADfirst| |r|) |d|
                                   |n| $)
                               (|getShellEntry| $ 103))
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (LETT |l| (CDR |l|)
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (EXIT (LETT |r| (CDR |r|)
                                 |POLYCAT-;reducedSystem;MVR;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (CONS |mm| |w|)))))) 

(DEFUN |POLYCAT-;gcdPolynomial;3Sup;22| (|pp| |qq| $)
  (SPADCALL |pp| |qq| (|getShellEntry| $ 108))) 

(DEFUN |POLYCAT-;solveLinearPolynomialEquation;LSupU;23| (|lpp| |pp| $)
  (SPADCALL |lpp| |pp| (|getShellEntry| $ 113))) 

(DEFUN |POLYCAT-;factorPolynomial;SupF;24| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 118))) 

(DEFUN |POLYCAT-;factorSquareFreePolynomial;SupF;25| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 121))) 

(DEFUN |POLYCAT-;factor;SF;26| (|p| $)
  (PROG (|v| |ansR| #0=#:G1596 |w| #1=#:G1597 |up| |ansSUP| #2=#:G1598
             |ww| #3=#:G1599)
    (RETURN
      (SEQ (LETT |v| (SPADCALL |p| (|getShellEntry| $ 43))
                 |POLYCAT-;factor;SF;26|)
           (EXIT (COND
                   ((QEQCAR |v| 1)
                    (SEQ (LETT |ansR|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 39))
                                   (|getShellEntry| $ 124))
                               |POLYCAT-;factor;SF;26|)
                         (EXIT (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |ansR|
                                     (|getShellEntry| $ 126))
                                    (|getShellEntry| $ 41))
                                   (PROGN
                                     (LETT #0# NIL
                                      |POLYCAT-;factor;SF;26|)
                                     (SEQ
                                      (LETT |w| NIL
                                       |POLYCAT-;factor;SF;26|)
                                      (LETT #1#
                                       (SPADCALL |ansR|
                                        (|getShellEntry| $ 130))
                                       |POLYCAT-;factor;SF;26|)
                                      G190
                                      (COND
                                        ((OR (ATOM #1#)
                                          (PROGN
                                            (LETT |w| (CAR #1#)
                                             |POLYCAT-;factor;SF;26|)
                                            NIL))
                                         (GO G191)))
                                      (SEQ
                                       (EXIT
                                        (LETT #0#
                                         (CONS
                                          (VECTOR (QVELT |w| 0)
                                           (SPADCALL (QVELT |w| 1)
                                            (|getShellEntry| $ 41))
                                           (QVELT |w| 2))
                                          #0#)
                                         |POLYCAT-;factor;SF;26|)))
                                      (LETT #1# (CDR #1#)
                                       |POLYCAT-;factor;SF;26|)
                                      (GO G190) G191
                                      (EXIT (NREVERSE0 #0#))))
                                   (|getShellEntry| $ 134)))))
                   ('T
                    (SEQ (LETT |up|
                               (SPADCALL |p| (QCDR |v|)
                                   (|getShellEntry| $ 49))
                               |POLYCAT-;factor;SF;26|)
                         (LETT |ansSUP|
                               (SPADCALL |up| (|getShellEntry| $ 118))
                               |POLYCAT-;factor;SF;26|)
                         (EXIT (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |ansSUP|
                                     (|getShellEntry| $ 135))
                                    (QCDR |v|) (|getShellEntry| $ 136))
                                   (PROGN
                                     (LETT #2# NIL
                                      |POLYCAT-;factor;SF;26|)
                                     (SEQ
                                      (LETT |ww| NIL
                                       |POLYCAT-;factor;SF;26|)
                                      (LETT #3#
                                       (SPADCALL |ansSUP|
                                        (|getShellEntry| $ 139))
                                       |POLYCAT-;factor;SF;26|)
                                      G190
                                      (COND
                                        ((OR (ATOM #3#)
                                          (PROGN
                                            (LETT |ww| (CAR #3#)
                                             |POLYCAT-;factor;SF;26|)
                                            NIL))
                                         (GO G191)))
                                      (SEQ
                                       (EXIT
                                        (LETT #2#
                                         (CONS
                                          (VECTOR (QVELT |ww| 0)
                                           (SPADCALL (QVELT |ww| 1)
                                            (QCDR |v|)
                                            (|getShellEntry| $ 136))
                                           (QVELT |ww| 2))
                                          #2#)
                                         |POLYCAT-;factor;SF;26|)))
                                      (LETT #3# (CDR #3#)
                                       |POLYCAT-;factor;SF;26|)
                                      (GO G190) G191
                                      (EXIT (NREVERSE0 #2#))))
                                   (|getShellEntry| $ 134))))))))))) 

(DEFUN |POLYCAT-;conditionP;MU;27| (|mat| $)
  (PROG (|ll| #0=#:G1634 |z| #1=#:G1635 |ch| |l| #2=#:G1636 #3=#:G1637
              #4=#:G1606 #5=#:G1604 #6=#:G1605 #7=#:G1638 |vars| |degs|
              #8=#:G1639 |d| #9=#:G1640 |nd| #10=#:G1633 #11=#:G1613
              |deg1| |redmons| #12=#:G1641 |v| #13=#:G1643 |u|
              #14=#:G1642 |llR| |monslist| |ans| #15=#:G1644
              #16=#:G1645 |mons| #17=#:G1646 |m| #18=#:G1647 |i|
              #19=#:G1629 #20=#:G1627 #21=#:G1628)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |ll|
                            (SPADCALL
                                (SPADCALL |mat|
                                    (|getShellEntry| $ 141))
                                (|getShellEntry| $ 95))
                            |POLYCAT-;conditionP;MU;27|)
                      (LETT |llR|
                            (PROGN
                              (LETT #0# NIL
                                    |POLYCAT-;conditionP;MU;27|)
                              (SEQ (LETT |z| NIL
                                    |POLYCAT-;conditionP;MU;27|)
                                   (LETT #1# (|SPADfirst| |ll|)
                                    |POLYCAT-;conditionP;MU;27|)
                                   G190
                                   (COND
                                     ((OR (ATOM #1#)
                                       (PROGN
                                         (LETT |z| (CAR #1#)
                                          |POLYCAT-;conditionP;MU;27|)
                                         NIL))
                                      (GO G191)))
                                   (SEQ
                                    (EXIT
                                     (LETT #0# (CONS NIL #0#)
                                      |POLYCAT-;conditionP;MU;27|)))
                                   (LETT #1# (CDR #1#)
                                    |POLYCAT-;conditionP;MU;27|)
                                   (GO G190) G191
                                   (EXIT (NREVERSE0 #0#))))
                            |POLYCAT-;conditionP;MU;27|)
                      (LETT |monslist| NIL |POLYCAT-;conditionP;MU;27|)
                      (LETT |ch| (SPADCALL (|getShellEntry| $ 142))
                            |POLYCAT-;conditionP;MU;27|)
                      (SEQ (LETT |l| NIL |POLYCAT-;conditionP;MU;27|)
                           (LETT #2# |ll| |POLYCAT-;conditionP;MU;27|)
                           G190
                           (COND
                             ((OR (ATOM #2#)
                                  (PROGN
                                    (LETT |l| (CAR #2#)
                                     |POLYCAT-;conditionP;MU;27|)
                                    NIL))
                              (GO G191)))
                           (SEQ (LETT |mons|
                                      (PROGN
                                        (LETT #6# NIL
                                         |POLYCAT-;conditionP;MU;27|)
                                        (SEQ
                                         (LETT |u| NIL
                                          |POLYCAT-;conditionP;MU;27|)
                                         (LETT #3# |l|
                                          |POLYCAT-;conditionP;MU;27|)
                                         G190
                                         (COND
                                           ((OR (ATOM #3#)
                                             (PROGN
                                               (LETT |u| (CAR #3#)
                                                |POLYCAT-;conditionP;MU;27|)
                                               NIL))
                                            (GO G191)))
                                         (SEQ
                                          (EXIT
                                           (PROGN
                                             (LETT #4#
                                              (SPADCALL |u|
                                               (|getShellEntry| $ 79))
                                              |POLYCAT-;conditionP;MU;27|)
                                             (COND
                                               (#6#
                                                (LETT #5#
                                                 (SPADCALL #5# #4#
                                                  (|getShellEntry| $
                                                   143))
                                                 |POLYCAT-;conditionP;MU;27|))
                                               ('T
                                                (PROGN
                                                  (LETT #5# #4#
                                                   |POLYCAT-;conditionP;MU;27|)
                                                  (LETT #6# 'T
                                                   |POLYCAT-;conditionP;MU;27|)))))))
                                         (LETT #3# (CDR #3#)
                                          |POLYCAT-;conditionP;MU;27|)
                                         (GO G190) G191 (EXIT NIL))
                                        (COND
                                          (#6# #5#)
                                          ('T
                                           (|IdentityError|
                                            '|setUnion|))))
                                      |POLYCAT-;conditionP;MU;27|)
                                (LETT |redmons| NIL
                                      |POLYCAT-;conditionP;MU;27|)
                                (SEQ (LETT |m| NIL
                                      |POLYCAT-;conditionP;MU;27|)
                                     (LETT #7# |mons|
                                      |POLYCAT-;conditionP;MU;27|)
                                     G190
                                     (COND
                                       ((OR (ATOM #7#)
                                         (PROGN
                                           (LETT |m| (CAR #7#)
                                            |POLYCAT-;conditionP;MU;27|)
                                           NIL))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |vars|
                                       (SPADCALL |m|
                                        (|getShellEntry| $ 32))
                                       |POLYCAT-;conditionP;MU;27|)
                                      (LETT |degs|
                                       (SPADCALL |m| |vars|
                                        (|getShellEntry| $ 144))
                                       |POLYCAT-;conditionP;MU;27|)
                                      (LETT |deg1|
                                       (PROGN
                                         (LETT #8# NIL
                                          |POLYCAT-;conditionP;MU;27|)
                                         (SEQ
                                          (LETT |d| NIL
                                           |POLYCAT-;conditionP;MU;27|)
                                          (LETT #9# |degs|
                                           |POLYCAT-;conditionP;MU;27|)
                                          G190
                                          (COND
                                            ((OR (ATOM #9#)
                                              (PROGN
                                                (LETT |d| (CAR #9#)
                                                 |POLYCAT-;conditionP;MU;27|)
                                                NIL))
                                             (GO G191)))
                                          (SEQ
                                           (EXIT
                                            (LETT #8#
                                             (CONS
                                              (SEQ
                                               (LETT |nd|
                                                (SPADCALL |d| |ch|
                                                 (|getShellEntry| $
                                                  146))
                                                |POLYCAT-;conditionP;MU;27|)
                                               (EXIT
                                                (COND
                                                  ((QEQCAR |nd| 1)
                                                   (PROGN
                                                     (LETT #10#
                                                      (CONS 1 "failed")
                                                      |POLYCAT-;conditionP;MU;27|)
                                                     (GO #10#)))
                                                  ('T
                                                   (PROG1
                                                    (LETT #11#
                                                     (QCDR |nd|)
                                                     |POLYCAT-;conditionP;MU;27|)
                                                     (|check-subtype|
                                                      (>= #11# 0)
                                                      '(|NonNegativeInteger|)
                                                      #11#))))))
                                              #8#)
                                             |POLYCAT-;conditionP;MU;27|)))
                                          (LETT #9# (CDR #9#)
                                           |POLYCAT-;conditionP;MU;27|)
                                          (GO G190) G191
                                          (EXIT (NREVERSE0 #8#))))
                                       |POLYCAT-;conditionP;MU;27|)
                                      (LETT |redmons|
                                       (CONS
                                        (SPADCALL (|spadConstant| $ 34)
                                         |vars| |deg1|
                                         (|getShellEntry| $ 56))
                                        |redmons|)
                                       |POLYCAT-;conditionP;MU;27|)
                                      (EXIT
                                       (LETT |llR|
                                        (PROGN
                                          (LETT #12# NIL
                                           |POLYCAT-;conditionP;MU;27|)
                                          (SEQ
                                           (LETT |v| NIL
                                            |POLYCAT-;conditionP;MU;27|)
                                           (LETT #13# |llR|
                                            |POLYCAT-;conditionP;MU;27|)
                                           (LETT |u| NIL
                                            |POLYCAT-;conditionP;MU;27|)
                                           (LETT #14# |l|
                                            |POLYCAT-;conditionP;MU;27|)
                                           G190
                                           (COND
                                             ((OR (ATOM #14#)
                                               (PROGN
                                                 (LETT |u| (CAR #14#)
                                                  |POLYCAT-;conditionP;MU;27|)
                                                 NIL)
                                               (ATOM #13#)
                                               (PROGN
                                                 (LETT |v| (CAR #13#)
                                                  |POLYCAT-;conditionP;MU;27|)
                                                 NIL))
                                              (GO G191)))
                                           (SEQ
                                            (EXIT
                                             (LETT #12#
                                              (CONS
                                               (CONS
                                                (SPADCALL
                                                 (SPADCALL |u| |vars|
                                                  |degs|
                                                  (|getShellEntry| $
                                                   54))
                                                 (|getShellEntry| $
                                                  147))
                                                |v|)
                                               #12#)
                                              |POLYCAT-;conditionP;MU;27|)))
                                           (LETT #14#
                                            (PROG1 (CDR #14#)
                                              (LETT #13# (CDR #13#)
                                               |POLYCAT-;conditionP;MU;27|))
                                            |POLYCAT-;conditionP;MU;27|)
                                           (GO G190) G191
                                           (EXIT (NREVERSE0 #12#))))
                                        |POLYCAT-;conditionP;MU;27|)))
                                     (LETT #7# (CDR #7#)
                                      |POLYCAT-;conditionP;MU;27|)
                                     (GO G190) G191 (EXIT NIL))
                                (EXIT (LETT |monslist|
                                       (CONS |redmons| |monslist|)
                                       |POLYCAT-;conditionP;MU;27|)))
                           (LETT #2# (CDR #2#)
                                 |POLYCAT-;conditionP;MU;27|)
                           (GO G190) G191 (EXIT NIL))
                      (LETT |ans|
                            (SPADCALL
                                (SPADCALL
                                    (SPADCALL |llR|
                                     (|getShellEntry| $ 92))
                                    (|getShellEntry| $ 148))
                                (|getShellEntry| $ 150))
                            |POLYCAT-;conditionP;MU;27|)
                      (EXIT (COND
                              ((QEQCAR |ans| 1) (CONS 1 "failed"))
                              ('T
                               (SEQ (LETT |i| 0
                                     |POLYCAT-;conditionP;MU;27|)
                                    (EXIT
                                     (CONS 0
                                      (PRIMVEC2ARR
                                       (PROGN
                                         (LETT #15#
                                          (GETREFV (SIZE |monslist|))
                                          |POLYCAT-;conditionP;MU;27|)
                                         (SEQ
                                          (LETT #16# 0
                                           |POLYCAT-;conditionP;MU;27|)
                                          (LETT |mons| NIL
                                           |POLYCAT-;conditionP;MU;27|)
                                          (LETT #17# |monslist|
                                           |POLYCAT-;conditionP;MU;27|)
                                          G190
                                          (COND
                                            ((OR (ATOM #17#)
                                              (PROGN
                                                (LETT |mons| (CAR #17#)
                                                 |POLYCAT-;conditionP;MU;27|)
                                                NIL))
                                             (GO G191)))
                                          (SEQ
                                           (EXIT
                                            (SETELT #15# #16#
                                             (PROGN
                                               (LETT #21# NIL
                                                |POLYCAT-;conditionP;MU;27|)
                                               (SEQ
                                                (LETT |m| NIL
                                                 |POLYCAT-;conditionP;MU;27|)
                                                (LETT #18# |mons|
                                                 |POLYCAT-;conditionP;MU;27|)
                                                G190
                                                (COND
                                                  ((OR (ATOM #18#)
                                                    (PROGN
                                                      (LETT |m|
                                                       (CAR #18#)
                                                       |POLYCAT-;conditionP;MU;27|)
                                                      NIL))
                                                   (GO G191)))
                                                (SEQ
                                                 (EXIT
                                                  (PROGN
                                                    (LETT #19#
                                                     (SPADCALL |m|
                                                      (SPADCALL
                                                       (SPADCALL
                                                        (QCDR |ans|)
                                                        (LETT |i|
                                                         (+ |i| 1)
                                                         |POLYCAT-;conditionP;MU;27|)
                                                        (|getShellEntry|
                                                         $ 151))
                                                       (|getShellEntry|
                                                        $ 41))
                                                      (|getShellEntry|
                                                       $ 152))
                                                     |POLYCAT-;conditionP;MU;27|)
                                                    (COND
                                                      (#21#
                                                       (LETT #20#
                                                        (SPADCALL #20#
                                                         #19#
                                                         (|getShellEntry|
                                                          $ 153))
                                                        |POLYCAT-;conditionP;MU;27|))
                                                      ('T
                                                       (PROGN
                                                         (LETT #20#
                                                          #19#
                                                          |POLYCAT-;conditionP;MU;27|)
                                                         (LETT #21# 'T
                                                          |POLYCAT-;conditionP;MU;27|)))))))
                                                (LETT #18# (CDR #18#)
                                                 |POLYCAT-;conditionP;MU;27|)
                                                (GO G190) G191
                                                (EXIT NIL))
                                               (COND
                                                 (#21# #20#)
                                                 ('T
                                                  (|spadConstant| $ 22)))))))
                                          (LETT #17#
                                           (PROG1 (CDR #17#)
                                             (LETT #16# (QSADD1 #16#)
                                              |POLYCAT-;conditionP;MU;27|))
                                           |POLYCAT-;conditionP;MU;27|)
                                          (GO G190) G191 (EXIT NIL))
                                         #15#))))))))))
           #10# (EXIT #10#))))) 

(DEFUN |POLYCAT-;charthRoot;SU;28| (|p| $)
  (PROG (|vars| |ans| |ch|)
    (RETURN
      (SEQ (LETT |vars| (SPADCALL |p| (|getShellEntry| $ 32))
                 |POLYCAT-;charthRoot;SU;28|)
           (EXIT (COND
                   ((NULL |vars|)
                    (SEQ (LETT |ans|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 147))
                                   (|getShellEntry| $ 155))
                               |POLYCAT-;charthRoot;SU;28|)
                         (EXIT (COND
                                 ((QEQCAR |ans| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |ans|)
                                         (|getShellEntry| $ 41))))))))
                   ('T
                    (SEQ (LETT |ch| (SPADCALL (|getShellEntry| $ 142))
                               |POLYCAT-;charthRoot;SU;28|)
                         (EXIT (|POLYCAT-;charthRootlv| |p| |vars| |ch|
                                   $)))))))))) 

(DEFUN |POLYCAT-;charthRootlv| (|p| |vars| |ch| $)
  (PROG (|v| |dd| |cp| |d| #0=#:G1668 |ans| |ansx| #1=#:G1675)
    (RETURN
      (SEQ (EXIT (COND
                   ((NULL |vars|)
                    (SEQ (LETT |ans|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 147))
                                   (|getShellEntry| $ 155))
                               |POLYCAT-;charthRootlv|)
                         (EXIT (COND
                                 ((QEQCAR |ans| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |ans|)
                                         (|getShellEntry| $ 41))))))))
                   ('T
                    (SEQ (LETT |v| (|SPADfirst| |vars|)
                               |POLYCAT-;charthRootlv|)
                         (LETT |vars| (CDR |vars|)
                               |POLYCAT-;charthRootlv|)
                         (LETT |d|
                               (SPADCALL |p| |v|
                                   (|getShellEntry| $ 37))
                               |POLYCAT-;charthRootlv|)
                         (LETT |ans| (|spadConstant| $ 22)
                               |POLYCAT-;charthRootlv|)
                         (SEQ G190 (COND ((NULL (< 0 |d|)) (GO G191)))
                              (SEQ (LETT |dd|
                                    (SPADCALL |d| |ch|
                                     (|getShellEntry| $ 146))
                                    |POLYCAT-;charthRootlv|)
                                   (EXIT
                                    (COND
                                      ((QEQCAR |dd| 1)
                                       (PROGN
                                         (LETT #1# (CONS 1 "failed")
                                          |POLYCAT-;charthRootlv|)
                                         (GO #1#)))
                                      ('T
                                       (SEQ
                                        (LETT |cp|
                                         (SPADCALL |p| |v| |d|
                                          (|getShellEntry| $ 158))
                                         |POLYCAT-;charthRootlv|)
                                        (LETT |p|
                                         (SPADCALL |p|
                                          (SPADCALL |cp| |v| |d|
                                           (|getShellEntry| $ 38))
                                          (|getShellEntry| $ 159))
                                         |POLYCAT-;charthRootlv|)
                                        (LETT |ansx|
                                         (|POLYCAT-;charthRootlv| |cp|
                                          |vars| |ch| $)
                                         |POLYCAT-;charthRootlv|)
                                        (EXIT
                                         (COND
                                           ((QEQCAR |ansx| 1)
                                            (PROGN
                                              (LETT #1#
                                               (CONS 1 "failed")
                                               |POLYCAT-;charthRootlv|)
                                              (GO #1#)))
                                           ('T
                                            (SEQ
                                             (LETT |d|
                                              (SPADCALL |p| |v|
                                               (|getShellEntry| $ 37))
                                              |POLYCAT-;charthRootlv|)
                                             (EXIT
                                              (LETT |ans|
                                               (SPADCALL |ans|
                                                (SPADCALL (QCDR |ansx|)
                                                 |v|
                                                 (PROG1
                                                  (LETT #0# (QCDR |dd|)
                                                   |POLYCAT-;charthRootlv|)
                                                   (|check-subtype|
                                                    (>= #0# 0)
                                                    '(|NonNegativeInteger|)
                                                    #0#))
                                                 (|getShellEntry| $ 38))
                                                (|getShellEntry| $ 153))
                                               |POLYCAT-;charthRootlv|)))))))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (LETT |ansx|
                               (|POLYCAT-;charthRootlv| |p| |vars| |ch|
                                   $)
                               |POLYCAT-;charthRootlv|)
                         (EXIT (COND
                                 ((QEQCAR |ansx| 1)
                                  (PROGN
                                    (LETT #1# (CONS 1 "failed")
                                     |POLYCAT-;charthRootlv|)
                                    (GO #1#)))
                                 ('T
                                  (PROGN
                                    (LETT #1#
                                     (CONS 0
                                      (SPADCALL |ans| (QCDR |ansx|)
                                       (|getShellEntry| $ 153)))
                                     |POLYCAT-;charthRootlv|)
                                    (GO #1#)))))))))
           #1# (EXIT #1#))))) 

(DEFUN |POLYCAT-;monicDivide;2SVarSetR;30| (|p1| |p2| |mvar| $)
  (PROG (|result|)
    (RETURN
      (SEQ (LETT |result|
                 (SPADCALL
                     (SPADCALL |p1| |mvar| (|getShellEntry| $ 49))
                     (SPADCALL |p2| |mvar| (|getShellEntry| $ 49))
                     (|getShellEntry| $ 161))
                 |POLYCAT-;monicDivide;2SVarSetR;30|)
           (EXIT (CONS (SPADCALL (QCAR |result|) |mvar|
                           (|getShellEntry| $ 136))
                       (SPADCALL (QCDR |result|) |mvar|
                           (|getShellEntry| $ 136)))))))) 

(DEFUN |POLYCAT-;squareFree;SF;31| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 164))) 

(DEFUN |POLYCAT-;squareFree;SF;32| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 167))) 

(DEFUN |POLYCAT-;squareFree;SF;33| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 167))) 

(DEFUN |POLYCAT-;squareFreePart;2S;34| (|p| $)
  (PROG (|s| |f| #0=#:G1691 #1=#:G1689 #2=#:G1687 #3=#:G1688)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (LETT |s| (SPADCALL |p| (|getShellEntry| $ 168))
                         |POLYCAT-;squareFreePart;2S;34|)
                   (|getShellEntry| $ 169))
               (PROGN
                 (LETT #3# NIL |POLYCAT-;squareFreePart;2S;34|)
                 (SEQ (LETT |f| NIL |POLYCAT-;squareFreePart;2S;34|)
                      (LETT #0# (SPADCALL |s| (|getShellEntry| $ 172))
                            |POLYCAT-;squareFreePart;2S;34|)
                      G190
                      (COND
                        ((OR (ATOM #0#)
                             (PROGN
                               (LETT |f| (CAR #0#)
                                     |POLYCAT-;squareFreePart;2S;34|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (PROGN
                                   (LETT #1# (QCAR |f|)
                                    |POLYCAT-;squareFreePart;2S;34|)
                                   (COND
                                     (#3#
                                      (LETT #2#
                                       (SPADCALL #2# #1#
                                        (|getShellEntry| $ 152))
                                       |POLYCAT-;squareFreePart;2S;34|))
                                     ('T
                                      (PROGN
                                        (LETT #2# #1#
                                         |POLYCAT-;squareFreePart;2S;34|)
                                        (LETT #3# 'T
                                         |POLYCAT-;squareFreePart;2S;34|)))))))
                      (LETT #0# (CDR #0#)
                            |POLYCAT-;squareFreePart;2S;34|)
                      (GO G190) G191 (EXIT NIL))
                 (COND (#3# #2#) ('T (|spadConstant| $ 34))))
               (|getShellEntry| $ 152)))))) 

(DEFUN |POLYCAT-;content;SVarSetS;35| (|p| |v| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 49))
      (|getShellEntry| $ 174))) 

(DEFUN |POLYCAT-;primitivePart;2S;36| (|p| $)
  (PROG (#0=#:G1694)
    (RETURN
      (QVELT (SPADCALL
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                  (SPADCALL |p|
                                      (|getShellEntry| $ 176))
                                  (|getShellEntry| $ 177))
                              |POLYCAT-;primitivePart;2S;36|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 (|getShellEntry| $ 179))
             1)))) 

(DEFUN |POLYCAT-;primitivePart;SVarSetS;37| (|p| |v| $)
  (PROG (#0=#:G1700)
    (RETURN
      (QVELT (SPADCALL
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                  (SPADCALL |p| |v|
                                      (|getShellEntry| $ 181))
                                  (|getShellEntry| $ 182))
                              |POLYCAT-;primitivePart;SVarSetS;37|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 (|getShellEntry| $ 179))
             1)))) 

(DEFUN |POLYCAT-;<;2SB;38| (|p| |q| $)
  (PROG (|dp| |dq|)
    (RETURN
      (SEQ (LETT |dp| (SPADCALL |p| (|getShellEntry| $ 61))
                 |POLYCAT-;<;2SB;38|)
           (LETT |dq| (SPADCALL |q| (|getShellEntry| $ 61))
                 |POLYCAT-;<;2SB;38|)
           (EXIT (COND
                   ((SPADCALL |dp| |dq| (|getShellEntry| $ 184))
                    (SPADCALL (|spadConstant| $ 23)
                        (SPADCALL |q| (|getShellEntry| $ 39))
                        (|getShellEntry| $ 185)))
                   ((SPADCALL |dq| |dp| (|getShellEntry| $ 184))
                    (SPADCALL (SPADCALL |p| (|getShellEntry| $ 39))
                        (|spadConstant| $ 23) (|getShellEntry| $ 185)))
                   ('T
                    (SPADCALL
                        (SPADCALL (SPADCALL |p| |q|
                                      (|getShellEntry| $ 159))
                                  (|getShellEntry| $ 39))
                        (|spadConstant| $ 23) (|getShellEntry| $ 185))))))))) 

(DEFUN |POLYCAT-;patternMatch;SP2Pmr;39| (|p| |pat| |l| $)
  (SPADCALL |p| |pat| |l| (|getShellEntry| $ 190))) 

(DEFUN |POLYCAT-;patternMatch;SP2Pmr;40| (|p| |pat| |l| $)
  (SPADCALL |p| |pat| |l| (|getShellEntry| $ 197))) 

(DEFUN |POLYCAT-;convert;SP;41| (|x| $)
  (SPADCALL (ELT $ 200) (ELT $ 201) |x| (|getShellEntry| $ 205))) 

(DEFUN |POLYCAT-;convert;SP;42| (|x| $)
  (SPADCALL (ELT $ 207) (ELT $ 208) |x| (|getShellEntry| $ 212))) 

(DEFUN |POLYCAT-;convert;SIf;43| (|p| $)
  (SPADCALL (ELT $ 215) (ELT $ 216) |p| (|getShellEntry| $ 220))) 

(DEFUN |PolynomialCategory&| (|#1| |#2| |#3| |#4|)
  (PROG (|dv$1| |dv$2| |dv$3| |dv$4| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|PolynomialCategory&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$3| (|devaluate| |#3|) . #0#)
        (LETT |dv$4| (|devaluate| |#4|) . #0#)
        (LETT |dv$|
              (LIST '|PolynomialCategory&| |dv$1| |dv$2| |dv$3| |dv$4|) . #0#)
        (LETT $ (|newShell| 229) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#2|
                                '(|PolynomialFactorizationExplicit|))
                            (|HasAttribute| |#2|
                                '|canonicalUnitNormal|)
                            (|HasCategory| |#2| '(|GcdDomain|))
                            (|HasCategory| |#2| '(|CommutativeRing|))
                            (|HasCategory| |#4|
                                '(|PatternMatchable| (|Float|)))
                            (|HasCategory| |#2|
                                '(|PatternMatchable| (|Float|)))
                            (|HasCategory| |#4|
                                '(|PatternMatchable| (|Integer|)))
                            (|HasCategory| |#2|
                                '(|PatternMatchable| (|Integer|)))
                            (|HasCategory| |#4|
                                '(|ConvertibleTo|
                                     (|Pattern| (|Float|))))
                            (|HasCategory| |#2|
                                '(|ConvertibleTo|
                                     (|Pattern| (|Float|))))
                            (|HasCategory| |#4|
                                '(|ConvertibleTo|
                                     (|Pattern| (|Integer|))))
                            (|HasCategory| |#2|
                                '(|ConvertibleTo|
                                     (|Pattern| (|Integer|))))
                            (|HasCategory| |#4|
                                '(|ConvertibleTo| (|InputForm|)))
                            (|HasCategory| |#2|
                                '(|ConvertibleTo| (|InputForm|)))
                            (|HasCategory| |#2| '(|OrderedSet|)))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (|setShellEntry| $ 8 |#3|)
        (|setShellEntry| $ 9 |#4|)
        (COND
          ((|testBitVector| |pv$| 4)
           (PROGN
             (|setShellEntry| $ 76
                 (CONS (|dispatchFunction|
                           |POLYCAT-;resultant;2SVarSetS;15|)
                       $))
             (|setShellEntry| $ 78
                 (CONS (|dispatchFunction|
                           |POLYCAT-;discriminant;SVarSetS;16|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|IntegralDomain|))
           (PROGN
             (|setShellEntry| $ 99
                 (CONS (|dispatchFunction|
                           |POLYCAT-;reducedSystem;MM;20|)
                       $))
             (|setShellEntry| $ 106
                 (CONS (|dispatchFunction|
                           |POLYCAT-;reducedSystem;MVR;21|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 109
                 (CONS (|dispatchFunction|
                           |POLYCAT-;gcdPolynomial;3Sup;22|)
                       $))
             (|setShellEntry| $ 116
                 (CONS (|dispatchFunction|
                           |POLYCAT-;solveLinearPolynomialEquation;LSupU;23|)
                       $))
             (|setShellEntry| $ 120
                 (CONS (|dispatchFunction|
                           |POLYCAT-;factorPolynomial;SupF;24|)
                       $))
             (|setShellEntry| $ 122
                 (CONS (|dispatchFunction|
                           |POLYCAT-;factorSquareFreePolynomial;SupF;25|)
                       $))
             (|setShellEntry| $ 140
                 (CONS (|dispatchFunction| |POLYCAT-;factor;SF;26|) $))
             (COND
               ((|HasCategory| |#2| '(|CharacteristicNonZero|))
                (PROGN
                  (|setShellEntry| $ 154
                      (CONS (|dispatchFunction|
                                |POLYCAT-;conditionP;MU;27|)
                            $))))))))
        (COND
          ((|HasCategory| |#2| '(|CharacteristicNonZero|))
           (PROGN
             (|setShellEntry| $ 156
                 (CONS (|dispatchFunction| |POLYCAT-;charthRoot;SU;28|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (COND
               ((|HasCategory| |#2| '(|EuclideanDomain|))
                (COND
                  ((|HasCategory| |#2| '(|CharacteristicZero|))
                   (|setShellEntry| $ 165
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;squareFree;SF;31|)
                             $)))
                  ('T
                   (|setShellEntry| $ 165
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;squareFree;SF;32|)
                             $)))))
               ('T
                (|setShellEntry| $ 165
                    (CONS (|dispatchFunction|
                              |POLYCAT-;squareFree;SF;33|)
                          $))))
             (|setShellEntry| $ 173
                 (CONS (|dispatchFunction|
                           |POLYCAT-;squareFreePart;2S;34|)
                       $))
             (|setShellEntry| $ 175
                 (CONS (|dispatchFunction|
                           |POLYCAT-;content;SVarSetS;35|)
                       $))
             (|setShellEntry| $ 180
                 (CONS (|dispatchFunction|
                           |POLYCAT-;primitivePart;2S;36|)
                       $))
             (|setShellEntry| $ 183
                 (CONS (|dispatchFunction|
                           |POLYCAT-;primitivePart;SVarSetS;37|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 15)
           (PROGN
             (|setShellEntry| $ 186
                 (CONS (|dispatchFunction| |POLYCAT-;<;2SB;38|) $))
             (COND
               ((|testBitVector| |pv$| 8)
                (COND
                  ((|testBitVector| |pv$| 7)
                   (|setShellEntry| $ 192
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;patternMatch;SP2Pmr;39|)
                             $))))))
             (COND
               ((|testBitVector| |pv$| 6)
                (COND
                  ((|testBitVector| |pv$| 5)
                   (|setShellEntry| $ 199
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;patternMatch;SP2Pmr;40|)
                             $)))))))))
        (COND
          ((|testBitVector| |pv$| 12)
           (COND
             ((|testBitVector| |pv$| 11)
              (|setShellEntry| $ 206
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SP;41|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 10)
           (COND
             ((|testBitVector| |pv$| 9)
              (|setShellEntry| $ 213
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SP;42|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 14)
           (COND
             ((|testBitVector| |pv$| 13)
              (|setShellEntry| $ 221
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SIf;43|)
                        $))))))
        $)))) 

(MAKEPROP '|PolynomialCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|local| |#3|) (|local| |#4|) (|Equation| 6) (0 . |lhs|)
             (|Union| 9 '"failed") (5 . |retractIfCan|)
             (10 . |retract|) (15 . |rhs|) (|List| 9) (|List| $)
             (20 . |eval|) (|Equation| $) (|List| 19)
             |POLYCAT-;eval;SLS;1| (27 . |Zero|) (31 . |Zero|)
             (|Boolean|) (35 . ~=) (41 . |leadingMonomial|)
             (46 . |reductum|) |POLYCAT-;monomials;SL;2|
             (51 . |monomials|) (|Union| 17 '"failed")
             |POLYCAT-;isPlus;SU;3| (56 . |variables|)
             (61 . |monomial?|) (66 . |One|) (70 . |One|)
             (|NonNegativeInteger|) (74 . |degree|) (80 . |monomial|)
             (87 . |leadingCoefficient|) (92 . =) (98 . |coerce|)
             |POLYCAT-;isTimes;SU;4| (103 . |mainVariable|) (108 . =)
             (|Record| (|:| |var| 9) (|:| |exponent| 36))
             (|Union| 45 '"failed") |POLYCAT-;isExpt;SU;5|
             (|SparseUnivariatePolynomial| $) (114 . |univariate|)
             (|SparseUnivariatePolynomial| 6) (120 . |coefficient|)
             |POLYCAT-;coefficient;SVarSetNniS;6| (|List| 36)
             (126 . |coefficient|) |POLYCAT-;coefficient;SLLS;7|
             (133 . |monomial|) |POLYCAT-;monomial;SLLS;8|
             (140 . |coerce|) |POLYCAT-;retract;SVarSet;9|
             |POLYCAT-;retractIfCan;SU;10| (145 . |degree|)
             (150 . |monomial|) |POLYCAT-;primitiveMonomials;SL;12|
             (156 . |ground?|) (161 . |Zero|) (165 . ~=)
             (171 . |degree|) (176 . |leadingCoefficient|)
             (181 . |totalDegree|) (186 . |reductum|)
             |POLYCAT-;totalDegree;SNni;13| (191 . |member?|)
             (197 . |totalDegree|) |POLYCAT-;totalDegree;SLNni;14|
             (203 . |resultant|) (209 . |resultant|)
             (216 . |discriminant|) (221 . |discriminant|)
             (227 . |primitiveMonomials|) (|List| 6) (232 . |concat|)
             (237 . |removeDuplicates!|) (|Vector| 7) (242 . |new|)
             (|Integer|) (248 . |minIndex|) (253 . |coefficient|)
             (259 . |qsetelt!|) (|List| 7) (|List| 89) (|Matrix| 7)
             (266 . |matrix|) (|List| 80) (|Matrix| 6)
             (271 . |listOfLists|) (276 . |not|) (281 . |vertConcat|)
             (|Matrix| $) (287 . |reducedSystem|) (|Vector| 6)
             (292 . |entries|) (297 . |concat|) (303 . |concat|)
             (|Record| (|:| |mat| 91) (|:| |vec| 83)) (|Vector| $)
             (309 . |reducedSystem|)
             (|GeneralPolynomialGcdPackage| 8 9 7 6)
             (315 . |gcdPolynomial|) (321 . |gcdPolynomial|)
             (|List| 50) (|Union| 110 '"failed")
             (|PolynomialFactorizationByRecursion| 7 8 9 6)
             (327 . |solveLinearPolynomialEquationByRecursion|)
             (|List| 48) (|Union| 114 '"failed")
             (333 . |solveLinearPolynomialEquation|) (|Factored| 50)
             (339 . |factorByRecursion|) (|Factored| 48)
             (344 . |factorPolynomial|)
             (349 . |factorSquareFreeByRecursion|)
             (354 . |factorSquareFreePolynomial|) (|Factored| $)
             (359 . |factor|) (|Factored| 7) (364 . |unit|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime")
             (|Record| (|:| |flg| 127) (|:| |fctr| 7) (|:| |xpnt| 85))
             (|List| 128) (369 . |factorList|)
             (|Record| (|:| |flg| 127) (|:| |fctr| 6) (|:| |xpnt| 85))
             (|List| 131) (|Factored| 6) (374 . |makeFR|)
             (380 . |unit|) (385 . |multivariate|)
             (|Record| (|:| |flg| 127) (|:| |fctr| 50) (|:| |xpnt| 85))
             (|List| 137) (391 . |factorList|) (396 . |factor|)
             (401 . |transpose|) (406 . |characteristic|)
             (410 . |setUnion|) (416 . |degree|) (|Union| $ '"failed")
             (422 . |exquo|) (428 . |ground|) (433 . |transpose|)
             (|Union| 105 '"failed") (438 . |conditionP|) (443 . |elt|)
             (449 . *) (455 . +) (461 . |conditionP|)
             (466 . |charthRoot|) (471 . |charthRoot|) (476 . |Zero|)
             (480 . |coefficient|) (487 . -)
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (493 . |monicDivide|) |POLYCAT-;monicDivide;2SVarSetR;30|
             (|MultivariateSquareFree| 8 9 7 6) (499 . |squareFree|)
             (504 . |squareFree|) (|PolynomialSquareFree| 9 8 7 6)
             (509 . |squareFree|) (514 . |squareFree|) (519 . |unit|)
             (|Record| (|:| |factor| 6) (|:| |exponent| 85))
             (|List| 170) (524 . |factors|) (529 . |squareFreePart|)
             (534 . |content|) (539 . |content|) (545 . |content|)
             (550 . |exquo|)
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (556 . |unitNormal|) (561 . |primitivePart|)
             (566 . |content|) (572 . |exquo|) (578 . |primitivePart|)
             (584 . <) (590 . <) (596 . <) (|PatternMatchResult| 85 6)
             (|Pattern| 85)
             (|PatternMatchPolynomialCategory| 85 8 9 7 6)
             (602 . |patternMatch|) (|PatternMatchResult| 85 $)
             (609 . |patternMatch|) (|Float|)
             (|PatternMatchResult| 193 6) (|Pattern| 193)
             (|PatternMatchPolynomialCategory| 193 8 9 7 6)
             (616 . |patternMatch|) (|PatternMatchResult| 193 $)
             (623 . |patternMatch|) (630 . |convert|) (635 . |convert|)
             (|Mapping| 188 9) (|Mapping| 188 7)
             (|PolynomialCategoryLifting| 8 9 7 6 188) (640 . |map|)
             (647 . |convert|) (652 . |convert|) (657 . |convert|)
             (|Mapping| 195 9) (|Mapping| 195 7)
             (|PolynomialCategoryLifting| 8 9 7 6 195) (662 . |map|)
             (669 . |convert|) (|InputForm|) (674 . |convert|)
             (679 . |convert|) (|Mapping| 214 9) (|Mapping| 214 7)
             (|PolynomialCategoryLifting| 8 9 7 6 214) (684 . |map|)
             (691 . |convert|) (|Matrix| 85) (|Vector| 85)
             (|Record| (|:| |mat| 222) (|:| |vec| 223))
             (|Union| 85 '"failed") (|Fraction| 85)
             (|Union| 226 '"failed") (|Union| 7 '"failed"))
          '#(|totalDegree| 696 |squareFreePart| 707 |squareFree| 712
             |solveLinearPolynomialEquation| 717 |retractIfCan| 723
             |retract| 728 |resultant| 733 |reducedSystem| 740
             |primitivePart| 751 |primitiveMonomials| 762
             |patternMatch| 767 |monomials| 781 |monomial| 786
             |monicDivide| 793 |isTimes| 800 |isPlus| 805 |isExpt| 810
             |gcdPolynomial| 815 |factorSquareFreePolynomial| 821
             |factorPolynomial| 826 |factor| 831 |eval| 836
             |discriminant| 842 |convert| 848 |content| 863
             |conditionP| 869 |coefficient| 874 |charthRoot| 888 < 893)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 221
                                '(1 10 6 0 11 1 6 12 0 13 1 6 9 0 14 1
                                  10 6 0 15 3 6 0 0 16 17 18 0 6 0 22 0
                                  7 0 23 2 6 24 0 0 25 1 6 0 0 26 1 6 0
                                  0 27 1 6 17 0 29 1 6 16 0 32 1 6 24 0
                                  33 0 6 0 34 0 7 0 35 2 6 36 0 9 37 3
                                  6 0 0 9 36 38 1 6 7 0 39 2 7 24 0 0
                                  40 1 6 0 7 41 1 6 12 0 43 2 6 24 0 0
                                  44 2 6 48 0 9 49 2 50 6 0 36 51 3 6 0
                                  0 16 53 54 3 6 0 0 16 53 56 1 6 0 9
                                  58 1 6 8 0 61 2 6 0 7 8 62 1 6 24 0
                                  64 0 50 0 65 2 50 24 0 0 66 1 50 36 0
                                  67 1 50 6 0 68 1 6 36 0 69 1 50 0 0
                                  70 2 16 24 9 0 72 2 6 36 0 16 73 2 50
                                  6 0 0 75 3 0 0 0 0 9 76 1 50 6 0 77 2
                                  0 0 0 9 78 1 6 17 0 79 1 80 0 17 81 1
                                  80 0 0 82 2 83 0 36 7 84 1 83 85 0 86
                                  2 6 7 0 8 87 3 83 7 0 85 7 88 1 91 0
                                  90 92 1 94 93 0 95 1 24 0 0 96 2 91 0
                                  0 0 97 1 0 91 98 99 1 100 80 0 101 2
                                  80 0 0 0 102 2 83 0 0 0 103 2 0 104
                                  98 105 106 2 107 50 50 50 108 2 0 48
                                  48 48 109 2 112 111 110 50 113 2 0
                                  115 114 48 116 1 112 117 50 118 1 0
                                  119 48 120 1 112 117 50 121 1 0 119
                                  48 122 1 7 123 0 124 1 125 7 0 126 1
                                  125 129 0 130 2 133 0 6 132 134 1 117
                                  50 0 135 2 6 0 48 9 136 1 117 138 0
                                  139 1 0 123 0 140 1 94 0 0 141 0 6 36
                                  142 2 80 0 0 0 143 2 6 53 0 16 144 2
                                  85 145 0 0 146 1 6 7 0 147 1 91 0 0
                                  148 1 7 149 98 150 2 83 7 0 85 151 2
                                  6 0 0 0 152 2 6 0 0 0 153 1 0 149 98
                                  154 1 7 145 0 155 1 0 145 0 156 0 8 0
                                  157 3 6 0 0 9 36 158 2 6 0 0 0 159 2
                                  50 160 0 0 161 1 163 133 6 164 1 0
                                  123 0 165 1 166 133 6 167 1 6 123 0
                                  168 1 133 6 0 169 1 133 171 0 172 1 0
                                  0 0 173 1 50 6 0 174 2 0 0 0 9 175 1
                                  6 7 0 176 2 6 145 0 7 177 1 6 178 0
                                  179 1 0 0 0 180 2 6 0 0 9 181 2 6 145
                                  0 0 182 2 0 0 0 9 183 2 8 24 0 0 184
                                  2 7 24 0 0 185 2 0 24 0 0 186 3 189
                                  187 6 188 187 190 3 0 191 0 188 191
                                  192 3 196 194 6 195 194 197 3 0 198 0
                                  195 198 199 1 9 188 0 200 1 7 188 0
                                  201 3 204 188 202 203 6 205 1 0 188 0
                                  206 1 9 195 0 207 1 7 195 0 208 3 211
                                  195 209 210 6 212 1 0 195 0 213 1 9
                                  214 0 215 1 7 214 0 216 3 219 214 217
                                  218 6 220 1 0 214 0 221 2 0 36 0 16
                                  74 1 0 36 0 71 1 0 0 0 173 1 0 123 0
                                  165 2 0 115 114 48 116 1 0 12 0 60 1
                                  0 9 0 59 3 0 0 0 0 9 76 1 0 91 98 99
                                  2 0 104 98 105 106 2 0 0 0 9 183 1 0
                                  0 0 180 1 0 17 0 63 3 0 191 0 188 191
                                  192 3 0 198 0 195 198 199 1 0 17 0 28
                                  3 0 0 0 16 53 57 3 0 160 0 0 9 162 1
                                  0 30 0 42 1 0 30 0 31 1 0 46 0 47 2 0
                                  48 48 48 109 1 0 119 48 122 1 0 119
                                  48 120 1 0 123 0 140 2 0 0 0 20 21 2
                                  0 0 0 9 78 1 0 214 0 221 1 0 188 0
                                  206 1 0 195 0 213 2 0 0 0 9 175 1 0
                                  149 98 154 3 0 0 0 16 53 55 3 0 0 0 9
                                  36 52 1 0 145 0 156 2 0 24 0 0 186)))))
          '|lookupComplete|)) 
