
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |POLYCAT-;eval;SLS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |POLYCAT-;monomials;SL;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;isPlus;SU;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;isTimes;SU;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;isExpt;SU;5|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |POLYCAT-;coefficient;SVarSetNniS;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |POLYCAT-;coefficient;SLLS;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |POLYCAT-;monomial;SLLS;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;retract;SVarSet;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;retractIfCan;SU;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;mkPrim|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |POLYCAT-;primitiveMonomials;SL;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |POLYCAT-;totalDegree;SNni;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|)
                    (|%IntegerSection| 0))
                |POLYCAT-;totalDegree;SLNni;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |POLYCAT-;resultant;2SVarSetS;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;discriminant;SVarSetS;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |POLYCAT-;allMonoms|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%List| (|%IntegerSection| 0) |%Shell|)
                    (|%Vector| *))
                |POLYCAT-;P2R|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%Thing|)
                |POLYCAT-;eq2R|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;reducedSystem;MM;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |POLYCAT-;reducedSystem;MVR;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;gcdPolynomial;3Sup;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;solveLinearPolynomialEquation;LSupU;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;factorPolynomial;SupF;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;factorSquareFreePolynomial;SupF;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;factor;SF;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;conditionP;MU;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |POLYCAT-;charthRoot;SU;28|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%List| (|%IntegerSection| 0) |%Shell|)
                    |%Pair|)
                |POLYCAT-;charthRootlv|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Pair|)
                |POLYCAT-;monicDivide;2SVarSetR;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;squareFree;SF;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;squareFree;SF;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;squareFree;SF;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;squareFreePart;2S;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;content;SVarSetS;35|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;primitivePart;2S;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;primitivePart;SVarSetS;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |POLYCAT-;before?;2SB;38|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |POLYCAT-;patternMatch;SP2Pmr;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |POLYCAT-;patternMatch;SP2Pmr;40|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;convert;SP;41|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;convert;SP;42|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |POLYCAT-;convert;SIf;43|)) 

(DEFUN |POLYCAT-;eval;SLS;1| (|p| |l| $)
  (PROG (#0=#:G1690 #1=#:G1428 #2=#:G1691 #3=#:G1692 |lvar| #4=#:G1693
            |e| #5=#:G1694)
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
                                   (COND
                                     ((QEQCAR
                                       (SPADCALL
                                        (SPADCALL |e|
                                         (|getShellEntry| $ 14))
                                        (|getShellEntry| $ 16))
                                       1)
                                      (PROGN
                                        (LETT #1#
                                         (|error|
                                          "cannot find a variable to evaluate")
                                         |POLYCAT-;eval;SLS;1|)
                                        (GO #1#))))
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
                                (LETT #2#
                                      (CONS
                                       (SPADCALL
                                        (SPADCALL |e|
                                         (|getShellEntry| $ 14))
                                        (|getShellEntry| $ 17))
                                       #2#)
                                      |POLYCAT-;eval;SLS;1|)
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
                                    (LETT #4#
                                     (CONS
                                      (SPADCALL |e|
                                       (|getShellEntry| $ 18))
                                      #4#)
                                     |POLYCAT-;eval;SLS;1|)
                                    (LETT #5# (CDR #5#)
                                     |POLYCAT-;eval;SLS;1|)
                                    (GO G190) G191
                                    (EXIT (NREVERSE0 #4#))))
                             (|getShellEntry| $ 21)))))))))) 

(DEFUN |POLYCAT-;monomials;SL;2| (|p| $)
  (PROG (|ml|)
    (RETURN
      (SEQ (LETT |ml| NIL |POLYCAT-;monomials;SL;2|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL |p| (|spadConstant| $ 27)
                             (|getShellEntry| $ 29)))
                   (GO G191)))
                (SEQ (LETT |ml|
                           (CONS (SPADCALL |p| (|getShellEntry| $ 30))
                                 |ml|)
                           |POLYCAT-;monomials;SL;2|)
                     (EXIT (LETT |p|
                                 (SPADCALL |p| (|getShellEntry| $ 32))
                                 |POLYCAT-;monomials;SL;2|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (REVERSE |ml|)))))) 

(DEFUN |POLYCAT-;isPlus;SU;3| (|p| $)
  (PROG (|l|)
    (RETURN
      (COND
        ((NULL (CDR (LETT |l| (SPADCALL |p| (|getShellEntry| $ 35))
                          |POLYCAT-;isPlus;SU;3|)))
         (CONS 1 "failed"))
        ('T (CONS 0 |l|)))))) 

(DEFUN |POLYCAT-;isTimes;SU;4| (|p| $)
  (PROG (|lv| #0=#:G1695 |v| #1=#:G1696 |l| |r|)
    (RETURN
      (SEQ (COND
             ((OR (NULL (LETT |lv|
                              (SPADCALL |p| (|getShellEntry| $ 40))
                              |POLYCAT-;isTimes;SU;4|))
                  (NOT (SPADCALL |p| (|getShellEntry| $ 42))))
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
                                (LETT #0#
                                      (CONS
                                       (SPADCALL (|spadConstant| $ 43)
                                        |v|
                                        (SPADCALL |p| |v|
                                         (|getShellEntry| $ 46))
                                        (|getShellEntry| $ 47))
                                       #0#)
                                      |POLYCAT-;isTimes;SU;4|)
                                (LETT #1# (CDR #1#)
                                      |POLYCAT-;isTimes;SU;4|)
                                (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                         |POLYCAT-;isTimes;SU;4|)
                   (EXIT (COND
                           ((SPADCALL
                                (LETT |r|
                                      (SPADCALL |p|
                                       (|getShellEntry| $ 48))
                                      |POLYCAT-;isTimes;SU;4|)
                                (|getShellEntry| $ 49))
                            (COND
                              ((NULL (CDR |lv|)) (CONS 1 "failed"))
                              ('T (CONS 0 |l|))))
                           ('T
                            (CONS 0
                                  (CONS (SPADCALL |r|
                                         (|getShellEntry| $ 51))
                                        |l|)))))))))))) 

(DEFUN |POLYCAT-;isExpt;SU;5| (|p| $)
  (PROG (|u| |d|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |p| (|getShellEntry| $ 53))
                 |POLYCAT-;isExpt;SU;5|)
           (EXIT (COND
                   ((OR (QEQCAR |u| 1)
                        (NOT (SPADCALL |p|
                                 (SPADCALL (|spadConstant| $ 43)
                                     (QCDR |u|)
                                     (LETT |d|
                                      (SPADCALL |p| (QCDR |u|)
                                       (|getShellEntry| $ 46))
                                      |POLYCAT-;isExpt;SU;5|)
                                     (|getShellEntry| $ 47))
                                 (|getShellEntry| $ 54))))
                    (CONS 1 "failed"))
                   ('T (CONS 0 (CONS (QCDR |u|) |d|))))))))) 

(DEFUN |POLYCAT-;coefficient;SVarSetNniS;6| (|p| |v| |n| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 59)) |n|
      (|getShellEntry| $ 61))) 

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
             (SPADCALL |p| (|SPADfirst| |lv|) (|getShellEntry| $ 59))
             (|SPADfirst| |ln|) (|getShellEntry| $ 61))
         (CDR |lv|) (CDR |ln|) (|getShellEntry| $ 68))))) 

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
             (|getShellEntry| $ 47))
         (CDR |lv|) (CDR |ln|) (|getShellEntry| $ 70))))) 

(DEFUN |POLYCAT-;retract;SVarSet;9| (|p| $)
  (PROG (#0=#:G1479 |q|)
    (RETURN
      (SEQ (LETT |q|
                 (PROG2 (LETT #0# (SPADCALL |p| (|getShellEntry| $ 53))
                              |POLYCAT-;retract;SVarSet;9|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 9)
                       #0#))
                 |POLYCAT-;retract;SVarSet;9|)
           (EXIT (COND
                   ((SPADCALL (SPADCALL |q| (|getShellEntry| $ 72)) |p|
                        (|getShellEntry| $ 54))
                    |q|)
                   ('T (|error| "Polynomial is not a single variable")))))))) 

(DEFUN |POLYCAT-;retractIfCan;SU;10| (|p| $)
  (PROG (|q| #0=#:G1487)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |q|
                                 (SPADCALL |p| (|getShellEntry| $ 53))
                                 |POLYCAT-;retractIfCan;SU;10|)
                           (EXIT (COND
                                   ((QEQCAR |q| 0)
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL (QCDR |q|)
                                         (|getShellEntry| $ 72))
                                        |p| (|getShellEntry| $ 54))
                                       (PROGN
                                         (LETT #0# |q|
                                          |POLYCAT-;retractIfCan;SU;10|)
                                         (GO #0#))))))))
                      (EXIT (CONS 1 "failed"))))
           #0# (EXIT #0#))))) 

(DEFUN |POLYCAT-;mkPrim| (|p| $)
  (SPADCALL (|spadConstant| $ 44) (SPADCALL |p| (|getShellEntry| $ 75))
      (|getShellEntry| $ 76))) 

(DEFUN |POLYCAT-;primitiveMonomials;SL;12| (|p| $)
  (PROG (#0=#:G1697 |q| #1=#:G1698)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |POLYCAT-;primitiveMonomials;SL;12|)
             (SEQ (LETT |q| NIL |POLYCAT-;primitiveMonomials;SL;12|)
                  (LETT #1# (SPADCALL |p| (|getShellEntry| $ 35))
                        |POLYCAT-;primitiveMonomials;SL;12|)
                  G190
                  (COND
                    ((OR (ATOM #1#)
                         (PROGN
                           (LETT |q| (CAR #1#)
                                 |POLYCAT-;primitiveMonomials;SL;12|)
                           NIL))
                     (GO G191)))
                  (LETT #0# (CONS (|POLYCAT-;mkPrim| |q| $) #0#)
                        |POLYCAT-;primitiveMonomials;SL;12|)
                  (LETT #1# (CDR #1#)
                        |POLYCAT-;primitiveMonomials;SL;12|)
                  (GO G190) G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |POLYCAT-;totalDegree;SNni;13| (|p| $)
  (PROG (#0=#:G1493 |d| |u|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 78)) 0)
             ('T
              (SEQ (LETT |u|
                         (SPADCALL |p|
                             (PROG2 (LETT #0#
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 53))
                                     |POLYCAT-;totalDegree;SNni;13|)
                                    (QCDR #0#)
                               (|check-union| (QEQCAR #0# 0)
                                   (|getShellEntry| $ 9) #0#))
                             (|getShellEntry| $ 59))
                         |POLYCAT-;totalDegree;SNni;13|)
                   (LETT |d| 0 |POLYCAT-;totalDegree;SNni;13|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL |u| (|spadConstant| $ 80)
                                     (|getShellEntry| $ 81)))
                           (GO G191)))
                        (SEQ (LETT |d|
                                   (MAX |d|
                                    (+
                                     (SPADCALL |u|
                                      (|getShellEntry| $ 82))
                                     (SPADCALL
                                      (SPADCALL |u|
                                       (|getShellEntry| $ 83))
                                      (|getShellEntry| $ 84))))
                                   |POLYCAT-;totalDegree;SNni;13|)
                             (EXIT (LETT |u|
                                    (SPADCALL |u|
                                     (|getShellEntry| $ 87))
                                    |POLYCAT-;totalDegree;SNni;13|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |POLYCAT-;totalDegree;SLNni;14| (|p| |lv| $)
  (PROG (#0=#:G1501 |v| |w| |d| |u|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 78)) 0)
             ('T
              (SEQ (LETT |u|
                         (SPADCALL |p|
                             (LETT |v|
                                   (PROG2
                                    (LETT #0#
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 53))
                                     |POLYCAT-;totalDegree;SLNni;14|)
                                    (QCDR #0#)
                                     (|check-union| (QEQCAR #0# 0)
                                      (|getShellEntry| $ 9) #0#))
                                   |POLYCAT-;totalDegree;SLNni;14|)
                             (|getShellEntry| $ 59))
                         |POLYCAT-;totalDegree;SLNni;14|)
                   (LETT |d| 0 |POLYCAT-;totalDegree;SLNni;14|)
                   (LETT |w| 0 |POLYCAT-;totalDegree;SLNni;14|)
                   (COND
                     ((SPADCALL |v| |lv| (|getShellEntry| $ 89))
                      (LETT |w| 1 |POLYCAT-;totalDegree;SLNni;14|)))
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL |u| (|spadConstant| $ 80)
                                     (|getShellEntry| $ 81)))
                           (GO G191)))
                        (SEQ (LETT |d|
                                   (MAX |d|
                                    (+
                                     (* |w|
                                      (SPADCALL |u|
                                       (|getShellEntry| $ 82)))
                                     (SPADCALL
                                      (SPADCALL |u|
                                       (|getShellEntry| $ 83))
                                      |lv| (|getShellEntry| $ 92))))
                                   |POLYCAT-;totalDegree;SLNni;14|)
                             (EXIT (LETT |u|
                                    (SPADCALL |u|
                                     (|getShellEntry| $ 87))
                                    |POLYCAT-;totalDegree;SLNni;14|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |d|)))))))) 

(DEFUN |POLYCAT-;resultant;2SVarSetS;15| (|p1| |p2| |mvar| $)
  (SPADCALL (SPADCALL |p1| |mvar| (|getShellEntry| $ 59))
      (SPADCALL |p2| |mvar| (|getShellEntry| $ 59))
      (|getShellEntry| $ 94))) 

(DEFUN |POLYCAT-;discriminant;SVarSetS;16| (|p| |var| $)
  (SPADCALL (SPADCALL |p| |var| (|getShellEntry| $ 59))
      (|getShellEntry| $ 96))) 

(DEFUN |POLYCAT-;allMonoms| (|l| $)
  (PROG (#0=#:G1699 |p| #1=#:G1700)
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
                          (LETT #0#
                                (CONS (SPADCALL |p|
                                       (|getShellEntry| $ 98))
                                      #0#)
                                |POLYCAT-;allMonoms|)
                          (LETT #1# (CDR #1#) |POLYCAT-;allMonoms|)
                          (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                   (|getShellEntry| $ 99))
               (|getShellEntry| $ 100)))))) 

(DEFUN |POLYCAT-;P2R| (|p| |b| |n| $)
  (PROG (|w| |bj| #0=#:G1702 |i| #1=#:G1701)
    (RETURN
      (SEQ (LETT |w|
                 (SPADCALL |n| (|spadConstant| $ 28)
                     (|getShellEntry| $ 102))
                 |POLYCAT-;P2R|)
           (SEQ (LETT |bj| NIL |POLYCAT-;P2R|)
                (LETT #0# |b| |POLYCAT-;P2R|)
                (LETT |i| (SPADCALL |w| (|getShellEntry| $ 104))
                      |POLYCAT-;P2R|)
                (LETT #1# (|sizeOfSimpleArray| |w|) |POLYCAT-;P2R|)
                G190
                (COND
                  ((OR (> |i| #1#) (ATOM #0#)
                       (PROGN
                         (LETT |bj| (CAR #0#) |POLYCAT-;P2R|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (SPADCALL |w| |i|
                               (SPADCALL |p| |bj|
                                   (|getShellEntry| $ 106))
                               (|getShellEntry| $ 107))))
                (LETT |i|
                      (PROG1 (+ |i| 1)
                        (LETT #0# (CDR #0#) |POLYCAT-;P2R|))
                      |POLYCAT-;P2R|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |w|))))) 

(DEFUN |POLYCAT-;eq2R| (|l| |b| $)
  (PROG (#0=#:G1703 |bj| #1=#:G1704 #2=#:G1705 |p| #3=#:G1706)
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
                      (LETT #0#
                            (CONS (PROGN
                                    (LETT #2# NIL |POLYCAT-;eq2R|)
                                    (SEQ (LETT |p| NIL |POLYCAT-;eq2R|)
                                     (LETT #3# |l| |POLYCAT-;eq2R|)
                                     G190
                                     (COND
                                       ((OR (ATOM #3#)
                                         (PROGN
                                           (LETT |p| (CAR #3#)
                                            |POLYCAT-;eq2R|)
                                           NIL))
                                        (GO G191)))
                                     (LETT #2#
                                      (CONS
                                       (SPADCALL |p| |bj|
                                        (|getShellEntry| $ 106))
                                       #2#)
                                      |POLYCAT-;eq2R|)
                                     (LETT #3# (CDR #3#)
                                      |POLYCAT-;eq2R|)
                                     (GO G190) G191
                                     (EXIT (NREVERSE0 #2#))))
                                  #0#)
                            |POLYCAT-;eq2R|)
                      (LETT #1# (CDR #1#) |POLYCAT-;eq2R|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 111)))))) 

(DEFUN |POLYCAT-;reducedSystem;MM;20| (|m| $)
  (PROG (#0=#:G1707 |r| #1=#:G1708 |b| #2=#:G1709 |bj| #3=#:G1710 |d|
            |mm| |l|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |m| (|getShellEntry| $ 114))
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
                         (|getShellEntry| $ 99))
                     (|getShellEntry| $ 100))
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
                                      (|getShellEntry| $ 75))
                                     #2#)
                                    |POLYCAT-;reducedSystem;MM;20|)))
                        (LETT #3# (CDR #3#)
                              |POLYCAT-;reducedSystem;MM;20|)
                        (GO G190) G191 (EXIT (NREVERSE0 #2#))))
                 |POLYCAT-;reducedSystem;MM;20|)
           (LETT |mm| (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d| $)
                 |POLYCAT-;reducedSystem;MM;20|)
           (LETT |l| (CDR |l|) |POLYCAT-;reducedSystem;MM;20|)
           (SEQ G190 (COND ((NULL (NOT (NULL |l|))) (GO G191)))
                (SEQ (LETT |mm|
                           (SPADCALL |mm|
                               (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d|
                                   $)
                               (|getShellEntry| $ 119))
                           |POLYCAT-;reducedSystem;MM;20|)
                     (EXIT (LETT |l| (CDR |l|)
                                 |POLYCAT-;reducedSystem;MM;20|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |mm|))))) 

(DEFUN |POLYCAT-;reducedSystem;MVR;21| (|m| |v| $)
  (PROG (#0=#:G1711 |s| #1=#:G1712 |b| #2=#:G1713 |bj| #3=#:G1714 |d|
            |n| |mm| |w| |l| |r|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |m| (|getShellEntry| $ 114))
                 |POLYCAT-;reducedSystem;MVR;21|)
           (LETT |r| (SPADCALL |v| (|getShellEntry| $ 123))
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
                             (|getShellEntry| $ 99))
                         (|getShellEntry| $ 124))
                     (|getShellEntry| $ 100))
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
                                      (|getShellEntry| $ 75))
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
           (SEQ G190 (COND ((NULL (NOT (NULL |l|))) (GO G191)))
                (SEQ (LETT |mm|
                           (SPADCALL |mm|
                               (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d|
                                   $)
                               (|getShellEntry| $ 119))
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (LETT |w|
                           (SPADCALL |w|
                               (|POLYCAT-;P2R| (|SPADfirst| |r|) |d|
                                   |n| $)
                               (|getShellEntry| $ 128))
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (LETT |l| (CDR |l|)
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (EXIT (LETT |r| (CDR |r|)
                                 |POLYCAT-;reducedSystem;MVR;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (CONS |mm| |w|)))))) 

(DEFUN |POLYCAT-;gcdPolynomial;3Sup;22| (|pp| |qq| $)
  (SPADCALL |pp| |qq| (|getShellEntry| $ 133))) 

(DEFUN |POLYCAT-;solveLinearPolynomialEquation;LSupU;23| (|lpp| |pp| $)
  (SPADCALL |lpp| |pp| (|getShellEntry| $ 138))) 

(DEFUN |POLYCAT-;factorPolynomial;SupF;24| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 143))) 

(DEFUN |POLYCAT-;factorSquareFreePolynomial;SupF;25| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 146))) 

(DEFUN |POLYCAT-;factor;SF;26| (|p| $)
  (PROG (|v| |ansR| #0=#:G1715 |w| #1=#:G1716 |up| |ansSUP| #2=#:G1717
             |ww| #3=#:G1718)
    (RETURN
      (SEQ (LETT |v| (SPADCALL |p| (|getShellEntry| $ 53))
                 |POLYCAT-;factor;SF;26|)
           (EXIT (COND
                   ((QEQCAR |v| 1)
                    (SEQ (LETT |ansR|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 48))
                                   (|getShellEntry| $ 149))
                               |POLYCAT-;factor;SF;26|)
                         (EXIT (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |ansR|
                                     (|getShellEntry| $ 151))
                                    (|getShellEntry| $ 51))
                                   (PROGN
                                     (LETT #0# NIL
                                      |POLYCAT-;factor;SF;26|)
                                     (SEQ
                                      (LETT |w| NIL
                                       |POLYCAT-;factor;SF;26|)
                                      (LETT #1#
                                       (SPADCALL |ansR|
                                        (|getShellEntry| $ 155))
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
                                            (|getShellEntry| $ 51))
                                           (QVELT |w| 2))
                                          #0#)
                                         |POLYCAT-;factor;SF;26|)))
                                      (LETT #1# (CDR #1#)
                                       |POLYCAT-;factor;SF;26|)
                                      (GO G190) G191
                                      (EXIT (NREVERSE0 #0#))))
                                   (|getShellEntry| $ 159)))))
                   ('T
                    (SEQ (LETT |up|
                               (SPADCALL |p| (QCDR |v|)
                                   (|getShellEntry| $ 59))
                               |POLYCAT-;factor;SF;26|)
                         (LETT |ansSUP|
                               (SPADCALL |up| (|getShellEntry| $ 143))
                               |POLYCAT-;factor;SF;26|)
                         (EXIT (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |ansSUP|
                                     (|getShellEntry| $ 160))
                                    (QCDR |v|) (|getShellEntry| $ 161))
                                   (PROGN
                                     (LETT #2# NIL
                                      |POLYCAT-;factor;SF;26|)
                                     (SEQ
                                      (LETT |ww| NIL
                                       |POLYCAT-;factor;SF;26|)
                                      (LETT #3#
                                       (SPADCALL |ansSUP|
                                        (|getShellEntry| $ 164))
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
                                            (|getShellEntry| $ 161))
                                           (QVELT |ww| 2))
                                          #2#)
                                         |POLYCAT-;factor;SF;26|)))
                                      (LETT #3# (CDR #3#)
                                       |POLYCAT-;factor;SF;26|)
                                      (GO G190) G191
                                      (EXIT (NREVERSE0 #2#))))
                                   (|getShellEntry| $ 159))))))))))) 

(DEFUN |POLYCAT-;conditionP;MU;27| (|mat| $)
  (PROG (|ll| #0=#:G1719 |z| #1=#:G1720 |ch| |l| #2=#:G1721 #3=#:G1722
              #4=#:G1584 #5=#:G1582 #6=#:G1583 #7=#:G1723 |vars| |degs|
              #8=#:G1724 |d| #9=#:G1725 |nd| #10=#:G1611 #11=#:G1591
              |deg1| |redmons| #12=#:G1726 |v| #13=#:G1728 |u|
              #14=#:G1727 |llR| |monslist| |ans| #15=#:G1729
              #16=#:G1730 |mons| #17=#:G1731 |m| #18=#:G1732 |i|
              #19=#:G1607 #20=#:G1605 #21=#:G1606)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |ll|
                            (SPADCALL
                                (SPADCALL |mat|
                                    (|getShellEntry| $ 166))
                                (|getShellEntry| $ 114))
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
                      (LETT |ch| (|spadConstant| $ 169)
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
                                               (|getShellEntry| $ 98))
                                              |POLYCAT-;conditionP;MU;27|)
                                             (COND
                                               (#6#
                                                (LETT #5#
                                                 (SPADCALL #5# #4#
                                                  (|getShellEntry| $
                                                   170))
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
                                        (|getShellEntry| $ 40))
                                       |POLYCAT-;conditionP;MU;27|)
                                      (LETT |degs|
                                       (SPADCALL |m| |vars|
                                        (|getShellEntry| $ 171))
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
                                                  173))
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
                                        (SPADCALL (|spadConstant| $ 43)
                                         |vars| |deg1|
                                         (|getShellEntry| $ 70))
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
                                                   68))
                                                 (|getShellEntry| $
                                                  175))
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
                                     (|getShellEntry| $ 111))
                                    (|getShellEntry| $ 178))
                                (|getShellEntry| $ 180))
                            |POLYCAT-;conditionP;MU;27|)
                      (EXIT (COND
                              ((QEQCAR |ans| 1) (CONS 1 "failed"))
                              ('T
                               (SEQ (LETT |i| 0
                                     |POLYCAT-;conditionP;MU;27|)
                                    (EXIT
                                     (CONS 0
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
                                           (|setSimpleArrayEntry| #15#
                                            #16#
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
                                                        $ 181))
                                                      (|getShellEntry|
                                                       $ 51))
                                                     (|getShellEntry| $
                                                      182))
                                                    |POLYCAT-;conditionP;MU;27|)
                                                   (COND
                                                     (#21#
                                                      (LETT #20#
                                                       (SPADCALL #20#
                                                        #19#
                                                        (|getShellEntry|
                                                         $ 183))
                                                       |POLYCAT-;conditionP;MU;27|))
                                                     ('T
                                                      (PROGN
                                                        (LETT #20# #19#
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
                                                 (|spadConstant| $ 27)))))))
                                         (LETT #17#
                                          (PROG1 (CDR #17#)
                                            (LETT #16# (QSADD1 #16#)
                                             |POLYCAT-;conditionP;MU;27|))
                                          |POLYCAT-;conditionP;MU;27|)
                                         (GO G190) G191 (EXIT NIL))
                                        #15#)))))))))
           #10# (EXIT #10#))))) 

(DEFUN |POLYCAT-;charthRoot;SU;28| (|p| $)
  (PROG (|vars| |ans| |ch|)
    (RETURN
      (SEQ (LETT |vars| (SPADCALL |p| (|getShellEntry| $ 40))
                 |POLYCAT-;charthRoot;SU;28|)
           (EXIT (COND
                   ((NULL |vars|)
                    (SEQ (LETT |ans|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 175))
                                   (|getShellEntry| $ 185))
                               |POLYCAT-;charthRoot;SU;28|)
                         (EXIT (COND
                                 ((QEQCAR |ans| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |ans|)
                                         (|getShellEntry| $ 51))))))))
                   ('T
                    (SEQ (LETT |ch| (|spadConstant| $ 169)
                               |POLYCAT-;charthRoot;SU;28|)
                         (EXIT (|POLYCAT-;charthRootlv| |p| |vars| |ch|
                                   $)))))))))) 

(DEFUN |POLYCAT-;charthRootlv| (|p| |vars| |ch| $)
  (PROG (|v| |dd| |cp| |d| #0=#:G1632 |ans| |ansx| #1=#:G1639)
    (RETURN
      (SEQ (EXIT (COND
                   ((NULL |vars|)
                    (SEQ (LETT |ans|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 175))
                                   (|getShellEntry| $ 185))
                               |POLYCAT-;charthRootlv|)
                         (EXIT (COND
                                 ((QEQCAR |ans| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |ans|)
                                         (|getShellEntry| $ 51))))))))
                   ('T
                    (SEQ (LETT |v| (|SPADfirst| |vars|)
                               |POLYCAT-;charthRootlv|)
                         (LETT |vars| (CDR |vars|)
                               |POLYCAT-;charthRootlv|)
                         (LETT |d|
                               (SPADCALL |p| |v|
                                   (|getShellEntry| $ 46))
                               |POLYCAT-;charthRootlv|)
                         (LETT |ans| (|spadConstant| $ 27)
                               |POLYCAT-;charthRootlv|)
                         (SEQ G190 (COND ((NULL (> |d| 0)) (GO G191)))
                              (SEQ (LETT |dd|
                                    (SPADCALL |d| |ch|
                                     (|getShellEntry| $ 173))
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
                                          (|getShellEntry| $ 188))
                                         |POLYCAT-;charthRootlv|)
                                        (LETT |p|
                                         (SPADCALL |p|
                                          (SPADCALL |cp| |v| |d|
                                           (|getShellEntry| $ 47))
                                          (|getShellEntry| $ 189))
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
                                               (|getShellEntry| $ 46))
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
                                                 (|getShellEntry| $ 47))
                                                (|getShellEntry| $ 183))
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
                                       (|getShellEntry| $ 183)))
                                     |POLYCAT-;charthRootlv|)
                                    (GO #1#)))))))))
           #1# (EXIT #1#))))) 

(DEFUN |POLYCAT-;monicDivide;2SVarSetR;30| (|p1| |p2| |mvar| $)
  (PROG (|result|)
    (RETURN
      (SEQ (LETT |result|
                 (SPADCALL
                     (SPADCALL |p1| |mvar| (|getShellEntry| $ 59))
                     (SPADCALL |p2| |mvar| (|getShellEntry| $ 59))
                     (|getShellEntry| $ 191))
                 |POLYCAT-;monicDivide;2SVarSetR;30|)
           (EXIT (CONS (SPADCALL (QCAR |result|) |mvar|
                           (|getShellEntry| $ 161))
                       (SPADCALL (QCDR |result|) |mvar|
                           (|getShellEntry| $ 161)))))))) 

(DEFUN |POLYCAT-;squareFree;SF;31| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 194))) 

(DEFUN |POLYCAT-;squareFree;SF;32| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 197))) 

(DEFUN |POLYCAT-;squareFree;SF;33| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 197))) 

(DEFUN |POLYCAT-;squareFreePart;2S;34| (|p| $)
  (PROG (|s| |f| #0=#:G1733 #1=#:G1653 #2=#:G1651 #3=#:G1652)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (LETT |s| (SPADCALL |p| (|getShellEntry| $ 198))
                         |POLYCAT-;squareFreePart;2S;34|)
                   (|getShellEntry| $ 199))
               (PROGN
                 (LETT #3# NIL |POLYCAT-;squareFreePart;2S;34|)
                 (SEQ (LETT |f| NIL |POLYCAT-;squareFreePart;2S;34|)
                      (LETT #0# (SPADCALL |s| (|getShellEntry| $ 202))
                            |POLYCAT-;squareFreePart;2S;34|)
                      G190
                      (COND
                        ((OR (ATOM #0#)
                             (PROGN
                               (LETT |f| (CAR #0#)
                                     |POLYCAT-;squareFreePart;2S;34|)
                               NIL))
                         (GO G191)))
                      (PROGN
                        (LETT #1# (QCAR |f|)
                              |POLYCAT-;squareFreePart;2S;34|)
                        (COND
                          (#3# (LETT #2#
                                     (SPADCALL #2# #1#
                                      (|getShellEntry| $ 182))
                                     |POLYCAT-;squareFreePart;2S;34|))
                          ('T
                           (PROGN
                             (LETT #2# #1#
                                   |POLYCAT-;squareFreePart;2S;34|)
                             (LETT #3# 'T
                                   |POLYCAT-;squareFreePart;2S;34|)))))
                      (LETT #0# (CDR #0#)
                            |POLYCAT-;squareFreePart;2S;34|)
                      (GO G190) G191 (EXIT NIL))
                 (COND (#3# #2#) ('T (|spadConstant| $ 43))))
               (|getShellEntry| $ 182)))))) 

(DEFUN |POLYCAT-;content;SVarSetS;35| (|p| |v| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 59))
      (|getShellEntry| $ 204))) 

(DEFUN |POLYCAT-;primitivePart;2S;36| (|p| $)
  (PROG (#0=#:G1657)
    (RETURN
      (QVELT (SPADCALL
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                  (SPADCALL |p|
                                      (|getShellEntry| $ 206))
                                  (|getShellEntry| $ 207))
                              |POLYCAT-;primitivePart;2S;36|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 (|getShellEntry| $ 209))
             1)))) 

(DEFUN |POLYCAT-;primitivePart;SVarSetS;37| (|p| |v| $)
  (PROG (#0=#:G1663)
    (RETURN
      (QVELT (SPADCALL
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                  (SPADCALL |p| |v|
                                      (|getShellEntry| $ 211))
                                  (|getShellEntry| $ 212))
                              |POLYCAT-;primitivePart;SVarSetS;37|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 (|getShellEntry| $ 209))
             1)))) 

(DEFUN |POLYCAT-;before?;2SB;38| (|p| |q| $)
  (PROG (|dp| |dq|)
    (RETURN
      (SEQ (LETT |dp| (SPADCALL |p| (|getShellEntry| $ 75))
                 |POLYCAT-;before?;2SB;38|)
           (LETT |dq| (SPADCALL |q| (|getShellEntry| $ 75))
                 |POLYCAT-;before?;2SB;38|)
           (EXIT (COND
                   ((SPADCALL |dp| |dq| (|getShellEntry| $ 214))
                    (SPADCALL (|spadConstant| $ 28)
                        (SPADCALL |q| (|getShellEntry| $ 48))
                        (|getShellEntry| $ 215)))
                   ((SPADCALL |dq| |dp| (|getShellEntry| $ 214))
                    (SPADCALL (SPADCALL |p| (|getShellEntry| $ 48))
                        (|spadConstant| $ 28) (|getShellEntry| $ 215)))
                   ('T
                    (SPADCALL
                        (SPADCALL (SPADCALL |p| |q|
                                      (|getShellEntry| $ 189))
                                  (|getShellEntry| $ 48))
                        (|spadConstant| $ 28) (|getShellEntry| $ 215))))))))) 

(DEFUN |POLYCAT-;patternMatch;SP2Pmr;39| (|p| |pat| |l| $)
  (SPADCALL |p| |pat| |l| (|getShellEntry| $ 220))) 

(DEFUN |POLYCAT-;patternMatch;SP2Pmr;40| (|p| |pat| |l| $)
  (SPADCALL |p| |pat| |l| (|getShellEntry| $ 227))) 

(DEFUN |POLYCAT-;convert;SP;41| (|x| $)
  (SPADCALL (ELT $ 230) (ELT $ 231) |x| (|getShellEntry| $ 235))) 

(DEFUN |POLYCAT-;convert;SP;42| (|x| $)
  (SPADCALL (ELT $ 237) (ELT $ 238) |x| (|getShellEntry| $ 242))) 

(DEFUN |POLYCAT-;convert;SIf;43| (|p| $)
  (SPADCALL (ELT $ 245) (ELT $ 246) |p| (|getShellEntry| $ 250))) 

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
        (LETT $ (|newShell| 259) . #0#)
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
                                '(|ConvertibleTo| (|InputForm|))))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (|setShellEntry| $ 8 |#3|)
        (|setShellEntry| $ 9 |#4|)
        (COND
          ((|testBitVector| |pv$| 4)
           (PROGN
             (|setShellEntry| $ 95
                 (CONS (|dispatchFunction|
                           |POLYCAT-;resultant;2SVarSetS;15|)
                       $))
             (|setShellEntry| $ 97
                 (CONS (|dispatchFunction|
                           |POLYCAT-;discriminant;SVarSetS;16|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|IntegralDomain|))
           (PROGN
             (|setShellEntry| $ 121
                 (CONS (|dispatchFunction|
                           |POLYCAT-;reducedSystem;MM;20|)
                       $))
             (|setShellEntry| $ 131
                 (CONS (|dispatchFunction|
                           |POLYCAT-;reducedSystem;MVR;21|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 134
                 (CONS (|dispatchFunction|
                           |POLYCAT-;gcdPolynomial;3Sup;22|)
                       $))
             (|setShellEntry| $ 141
                 (CONS (|dispatchFunction|
                           |POLYCAT-;solveLinearPolynomialEquation;LSupU;23|)
                       $))
             (|setShellEntry| $ 145
                 (CONS (|dispatchFunction|
                           |POLYCAT-;factorPolynomial;SupF;24|)
                       $))
             (|setShellEntry| $ 147
                 (CONS (|dispatchFunction|
                           |POLYCAT-;factorSquareFreePolynomial;SupF;25|)
                       $))
             (|setShellEntry| $ 165
                 (CONS (|dispatchFunction| |POLYCAT-;factor;SF;26|) $))
             (COND
               ((|HasCategory| |#2| '(|CharacteristicNonZero|))
                (PROGN
                  (|setShellEntry| $ 184
                      (CONS (|dispatchFunction|
                                |POLYCAT-;conditionP;MU;27|)
                            $))))))))
        (COND
          ((|HasCategory| |#2| '(|CharacteristicNonZero|))
           (PROGN
             (|setShellEntry| $ 186
                 (CONS (|dispatchFunction| |POLYCAT-;charthRoot;SU;28|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (COND
               ((|HasCategory| |#2| '(|EuclideanDomain|))
                (COND
                  ((|HasCategory| |#2| '(|CharacteristicZero|))
                   (|setShellEntry| $ 195
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;squareFree;SF;31|)
                             $)))
                  ('T
                   (|setShellEntry| $ 195
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;squareFree;SF;32|)
                             $)))))
               ('T
                (|setShellEntry| $ 195
                    (CONS (|dispatchFunction|
                              |POLYCAT-;squareFree;SF;33|)
                          $))))
             (|setShellEntry| $ 203
                 (CONS (|dispatchFunction|
                           |POLYCAT-;squareFreePart;2S;34|)
                       $))
             (|setShellEntry| $ 205
                 (CONS (|dispatchFunction|
                           |POLYCAT-;content;SVarSetS;35|)
                       $))
             (|setShellEntry| $ 210
                 (CONS (|dispatchFunction|
                           |POLYCAT-;primitivePart;2S;36|)
                       $))
             (|setShellEntry| $ 213
                 (CONS (|dispatchFunction|
                           |POLYCAT-;primitivePart;SVarSetS;37|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 8)
           (COND
             ((|testBitVector| |pv$| 7)
              (|setShellEntry| $ 222
                  (CONS (|dispatchFunction|
                            |POLYCAT-;patternMatch;SP2Pmr;39|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 6)
           (COND
             ((|testBitVector| |pv$| 5)
              (|setShellEntry| $ 229
                  (CONS (|dispatchFunction|
                            |POLYCAT-;patternMatch;SP2Pmr;40|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 12)
           (COND
             ((|testBitVector| |pv$| 11)
              (|setShellEntry| $ 236
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SP;41|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 10)
           (COND
             ((|testBitVector| |pv$| 9)
              (|setShellEntry| $ 243
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SP;42|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 14)
           (COND
             ((|testBitVector| |pv$| 13)
              (|setShellEntry| $ 251
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SIf;43|)
                        $))))))
        $)))) 

(MAKEPROP '|PolynomialCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|local| |#3|) (|local| |#4|) (|Boolean|) (|Equation| 6)
             (|List| 11) (0 . |empty?|) (5 . |lhs|)
             (|Union| 9 '"failed") (10 . |retractIfCan|)
             (15 . |retract|) (20 . |rhs|) (|List| 9) (|List| $)
             (25 . |eval|) (|Equation| $) (|List| 22)
             |POLYCAT-;eval;SLS;1| (|List| 6) (32 . |empty|)
             (36 . |Zero|) (40 . |Zero|) (44 . ~=)
             (50 . |leadingMonomial|) (55 . |concat|) (61 . |reductum|)
             (66 . |reverse|) |POLYCAT-;monomials;SL;2|
             (71 . |monomials|) (76 . |rest|) (81 . |empty?|)
             (|Union| 20 '"failed") |POLYCAT-;isPlus;SU;3|
             (86 . |variables|) (91 . |empty?|) (96 . |monomial?|)
             (101 . |One|) (105 . |One|) (|NonNegativeInteger|)
             (109 . |degree|) (115 . |monomial|)
             (122 . |leadingCoefficient|) (127 . |one?|) (132 . |rest|)
             (137 . |coerce|) |POLYCAT-;isTimes;SU;4|
             (142 . |mainVariable|) (147 . =)
             (|Record| (|:| |var| 9) (|:| |exponent| 45))
             (|Union| 55 '"failed") |POLYCAT-;isExpt;SU;5|
             (|SparseUnivariatePolynomial| $) (153 . |univariate|)
             (|SparseUnivariatePolynomial| 6) (159 . |coefficient|)
             |POLYCAT-;coefficient;SVarSetNniS;6| (|List| 45)
             (165 . |empty?|) (170 . |first|) (175 . |first|)
             (180 . |rest|) (185 . |coefficient|)
             |POLYCAT-;coefficient;SLLS;7| (192 . |monomial|)
             |POLYCAT-;monomial;SLLS;8| (199 . |coerce|)
             |POLYCAT-;retract;SVarSet;9| |POLYCAT-;retractIfCan;SU;10|
             (204 . |degree|) (209 . |monomial|)
             |POLYCAT-;primitiveMonomials;SL;12| (215 . |ground?|)
             (220 . |Zero|) (224 . |Zero|) (228 . ~=) (234 . |degree|)
             (239 . |leadingCoefficient|) (244 . |totalDegree|)
             (249 . +) (255 . |max|) (261 . |reductum|)
             |POLYCAT-;totalDegree;SNni;13| (266 . |member?|)
             (272 . |One|) (276 . *) (282 . |totalDegree|)
             |POLYCAT-;totalDegree;SLNni;14| (288 . |resultant|)
             (294 . |resultant|) (301 . |discriminant|)
             (306 . |discriminant|) (312 . |primitiveMonomials|)
             (317 . |concat|) (322 . |removeDuplicates!|) (|Vector| 7)
             (327 . |new|) (|Integer|) (333 . |minIndex|)
             (338 . |maxIndex|) (343 . |coefficient|)
             (349 . |qsetelt!|) (|List| 7) (|List| 108) (|Matrix| 7)
             (356 . |matrix|) (|List| 25) (|Matrix| 6)
             (361 . |listOfLists|) (366 . |first|) (371 . |rest|)
             (376 . |empty?|) (381 . |not|) (386 . |vertConcat|)
             (|Matrix| $) (392 . |reducedSystem|) (|Vector| 6)
             (397 . |entries|) (402 . |concat|) (|List| 8) (408 . |#|)
             (413 . |first|) (418 . |concat|)
             (|Record| (|:| |mat| 110) (|:| |vec| 101)) (|Vector| $)
             (424 . |reducedSystem|)
             (|GeneralPolynomialGcdPackage| 8 9 7 6)
             (430 . |gcdPolynomial|) (436 . |gcdPolynomial|)
             (|List| 60) (|Union| 135 '"failed")
             (|PolynomialFactorizationByRecursion| 7 8 9 6)
             (442 . |solveLinearPolynomialEquationByRecursion|)
             (|List| 58) (|Union| 139 '"failed")
             (448 . |solveLinearPolynomialEquation|) (|Factored| 60)
             (454 . |factorByRecursion|) (|Factored| 58)
             (459 . |factorPolynomial|)
             (464 . |factorSquareFreeByRecursion|)
             (469 . |factorSquareFreePolynomial|) (|Factored| $)
             (474 . |factor|) (|Factored| 7) (479 . |unit|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime")
             (|Record| (|:| |flg| 152) (|:| |fctr| 7) (|:| |xpnt| 103))
             (|List| 153) (484 . |factorList|)
             (|Record| (|:| |flg| 152) (|:| |fctr| 6) (|:| |xpnt| 103))
             (|List| 156) (|Factored| 6) (489 . |makeFR|)
             (495 . |unit|) (500 . |multivariate|)
             (|Record| (|:| |flg| 152) (|:| |fctr| 60)
                 (|:| |xpnt| 103))
             (|List| 162) (506 . |factorList|) (511 . |factor|)
             (516 . |transpose|) (521 . |empty|) (525 . |empty|)
             (529 . |characteristic|) (533 . |setUnion|)
             (539 . |degree|) (|Union| $ '"failed") (545 . |exquo|)
             (551 . |cons|) (557 . |ground|) (562 . |cons|)
             (568 . |cons|) (574 . |transpose|) (|Union| 130 '"failed")
             (579 . |conditionP|) (584 . |elt|) (590 . *) (596 . +)
             (602 . |conditionP|) (607 . |charthRoot|)
             (612 . |charthRoot|) (617 . >) (623 . |coefficient|)
             (630 . -)
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (636 . |monicDivide|) |POLYCAT-;monicDivide;2SVarSetR;30|
             (|MultivariateSquareFree| 8 9 7 6) (642 . |squareFree|)
             (647 . |squareFree|) (|PolynomialSquareFree| 9 8 7 6)
             (652 . |squareFree|) (657 . |squareFree|) (662 . |unit|)
             (|Record| (|:| |factor| 6) (|:| |exponent| 103))
             (|List| 200) (667 . |factors|) (672 . |squareFreePart|)
             (677 . |content|) (682 . |content|) (688 . |content|)
             (693 . |exquo|)
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (699 . |unitNormal|) (704 . |primitivePart|)
             (709 . |content|) (715 . |exquo|) (721 . |primitivePart|)
             (727 . <) (733 . |before?|) |POLYCAT-;before?;2SB;38|
             (|PatternMatchResult| 103 6) (|Pattern| 103)
             (|PatternMatchPolynomialCategory| 103 8 9 7 6)
             (739 . |patternMatch|) (|PatternMatchResult| 103 $)
             (746 . |patternMatch|) (|Float|)
             (|PatternMatchResult| 223 6) (|Pattern| 223)
             (|PatternMatchPolynomialCategory| 223 8 9 7 6)
             (753 . |patternMatch|) (|PatternMatchResult| 223 $)
             (760 . |patternMatch|) (767 . |convert|) (772 . |convert|)
             (|Mapping| 218 9) (|Mapping| 218 7)
             (|PolynomialCategoryLifting| 8 9 7 6 218) (777 . |map|)
             (784 . |convert|) (789 . |convert|) (794 . |convert|)
             (|Mapping| 225 9) (|Mapping| 225 7)
             (|PolynomialCategoryLifting| 8 9 7 6 225) (799 . |map|)
             (806 . |convert|) (|InputForm|) (811 . |convert|)
             (816 . |convert|) (|Mapping| 244 9) (|Mapping| 244 7)
             (|PolynomialCategoryLifting| 8 9 7 6 244) (821 . |map|)
             (828 . |convert|) (|Matrix| 103) (|Vector| 103)
             (|Record| (|:| |mat| 252) (|:| |vec| 253))
             (|Union| 103 '"failed") (|Fraction| 103)
             (|Union| 256 '"failed") (|Union| 7 '"failed"))
          '#(|totalDegree| 833 |squareFreePart| 844 |squareFree| 849
             |solveLinearPolynomialEquation| 854 |retractIfCan| 860
             |retract| 865 |resultant| 870 |reducedSystem| 877
             |primitivePart| 888 |primitiveMonomials| 899
             |patternMatch| 904 |monomials| 918 |monomial| 923
             |monicDivide| 930 |isTimes| 937 |isPlus| 942 |isExpt| 947
             |gcdPolynomial| 952 |factorSquareFreePolynomial| 958
             |factorPolynomial| 963 |factor| 968 |eval| 973
             |discriminant| 979 |convert| 985 |content| 1000
             |conditionP| 1006 |coefficient| 1011 |charthRoot| 1025
             |before?| 1030)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 251
                                '(1 12 10 0 13 1 11 6 0 14 1 6 15 0 16
                                  1 6 9 0 17 1 11 6 0 18 3 6 0 0 19 20
                                  21 0 25 0 26 0 6 0 27 0 7 0 28 2 6 10
                                  0 0 29 1 6 0 0 30 2 25 0 6 0 31 1 6 0
                                  0 32 1 25 0 0 33 1 6 20 0 35 1 25 0 0
                                  36 1 25 10 0 37 1 6 19 0 40 1 19 10 0
                                  41 1 6 10 0 42 0 6 0 43 0 7 0 44 2 6
                                  45 0 9 46 3 6 0 0 9 45 47 1 6 7 0 48
                                  1 7 10 0 49 1 19 0 0 50 1 6 0 7 51 1
                                  6 15 0 53 2 6 10 0 0 54 2 6 58 0 9 59
                                  2 60 6 0 45 61 1 63 10 0 64 1 19 9 0
                                  65 1 63 45 0 66 1 63 0 0 67 3 6 0 0
                                  19 63 68 3 6 0 0 19 63 70 1 6 0 9 72
                                  1 6 8 0 75 2 6 0 7 8 76 1 6 10 0 78 0
                                  45 0 79 0 60 0 80 2 60 10 0 0 81 1 60
                                  45 0 82 1 60 6 0 83 1 6 45 0 84 2 45
                                  0 0 0 85 2 45 0 0 0 86 1 60 0 0 87 2
                                  19 10 9 0 89 0 45 0 90 2 45 0 45 0 91
                                  2 6 45 0 19 92 2 60 6 0 0 94 3 0 0 0
                                  0 9 95 1 60 6 0 96 2 0 0 0 9 97 1 6
                                  20 0 98 1 25 0 20 99 1 25 0 0 100 2
                                  101 0 45 7 102 1 101 103 0 104 1 101
                                  103 0 105 2 6 7 0 8 106 3 101 7 0 103
                                  7 107 1 110 0 109 111 1 113 112 0 114
                                  1 112 25 0 115 1 112 0 0 116 1 112 10
                                  0 117 1 10 0 0 118 2 110 0 0 0 119 1
                                  0 110 120 121 1 122 25 0 123 2 25 0 0
                                  0 124 1 125 45 0 126 1 25 6 0 127 2
                                  101 0 0 0 128 2 0 129 120 130 131 2
                                  132 60 60 60 133 2 0 58 58 58 134 2
                                  137 136 135 60 138 2 0 140 139 58 141
                                  1 137 142 60 143 1 0 144 58 145 1 137
                                  142 60 146 1 0 144 58 147 1 7 148 0
                                  149 1 150 7 0 151 1 150 154 0 155 2
                                  158 0 6 157 159 1 142 60 0 160 2 6 0
                                  58 9 161 1 142 163 0 164 1 0 148 0
                                  165 1 113 0 0 166 0 108 0 167 0 112 0
                                  168 0 6 45 169 2 25 0 0 0 170 2 6 63
                                  0 19 171 2 103 172 0 0 173 2 25 0 6 0
                                  174 1 6 7 0 175 2 108 0 7 0 176 2 112
                                  0 25 0 177 1 110 0 0 178 1 7 179 120
                                  180 2 101 7 0 103 181 2 6 0 0 0 182 2
                                  6 0 0 0 183 1 0 179 120 184 1 7 172 0
                                  185 1 0 172 0 186 2 45 10 0 0 187 3 6
                                  0 0 9 45 188 2 6 0 0 0 189 2 60 190 0
                                  0 191 1 193 158 6 194 1 0 148 0 195 1
                                  196 158 6 197 1 6 148 0 198 1 158 6 0
                                  199 1 158 201 0 202 1 0 0 0 203 1 60
                                  6 0 204 2 0 0 0 9 205 1 6 7 0 206 2 6
                                  172 0 7 207 1 6 208 0 209 1 0 0 0 210
                                  2 6 0 0 9 211 2 6 172 0 0 212 2 0 0 0
                                  9 213 2 8 10 0 0 214 2 7 10 0 0 215 3
                                  219 217 6 218 217 220 3 0 221 0 218
                                  221 222 3 226 224 6 225 224 227 3 0
                                  228 0 225 228 229 1 9 218 0 230 1 7
                                  218 0 231 3 234 218 232 233 6 235 1 0
                                  218 0 236 1 9 225 0 237 1 7 225 0 238
                                  3 241 225 239 240 6 242 1 0 225 0 243
                                  1 9 244 0 245 1 7 244 0 246 3 249 244
                                  247 248 6 250 1 0 244 0 251 2 0 45 0
                                  19 93 1 0 45 0 88 1 0 0 0 203 1 0 148
                                  0 195 2 0 140 139 58 141 1 0 15 0 74
                                  1 0 9 0 73 3 0 0 0 0 9 95 2 0 129 120
                                  130 131 1 0 110 120 121 2 0 0 0 9 213
                                  1 0 0 0 210 1 0 20 0 77 3 0 228 0 225
                                  228 229 3 0 221 0 218 221 222 1 0 20
                                  0 34 3 0 0 0 19 63 71 3 0 190 0 0 9
                                  192 1 0 38 0 52 1 0 38 0 39 1 0 56 0
                                  57 2 0 58 58 58 134 1 0 144 58 147 1
                                  0 144 58 145 1 0 148 0 165 2 0 0 0 23
                                  24 2 0 0 0 9 97 1 0 218 0 236 1 0 244
                                  0 251 1 0 225 0 243 2 0 0 0 9 205 1 0
                                  179 120 184 3 0 0 0 9 45 62 3 0 0 0
                                  19 63 69 1 0 172 0 186 2 0 10 0 0
                                  216)))))
          '|lookupComplete|)) 
