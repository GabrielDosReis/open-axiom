
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
                                   (SEQ
                                    (EXIT
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
                                          (|getShellEntry| $ 14))
                                         (|getShellEntry| $ 17))
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
                                         (|getShellEntry| $ 18))
                                        #4#)
                                       |POLYCAT-;eval;SLS;1|)))
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
                                (SEQ (EXIT
                                      (LETT #0#
                                       (CONS
                                        (SPADCALL (|spadConstant| $ 43)
                                         |v|
                                         (SPADCALL |p| |v|
                                          (|getShellEntry| $ 46))
                                         (|getShellEntry| $ 47))
                                        #0#)
                                       |POLYCAT-;isTimes;SU;4|)))
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
                  (SEQ (EXIT (LETT #0#
                                   (CONS (|POLYCAT-;mkPrim| |q| $) #0#)
                                   |POLYCAT-;primitiveMonomials;SL;12|)))
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
                          (SEQ (EXIT (LETT #0#
                                      (CONS
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 98))
                                       #0#)
                                      |POLYCAT-;allMonoms|)))
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
                                                (|getShellEntry| $ 106))
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
                               (|getShellEntry| $ 118))
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
           (LETT |r| (SPADCALL |v| (|getShellEntry| $ 122))
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
                         (|getShellEntry| $ 123))
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
                               (|getShellEntry| $ 118))
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (LETT |w|
                           (SPADCALL |w|
                               (|POLYCAT-;P2R| (|SPADfirst| |r|) |d|
                                   |n| $)
                               (|getShellEntry| $ 127))
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (LETT |l| (CDR |l|)
                           |POLYCAT-;reducedSystem;MVR;21|)
                     (EXIT (LETT |r| (CDR |r|)
                                 |POLYCAT-;reducedSystem;MVR;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (CONS |mm| |w|)))))) 

(DEFUN |POLYCAT-;gcdPolynomial;3Sup;22| (|pp| |qq| $)
  (SPADCALL |pp| |qq| (|getShellEntry| $ 132))) 

(DEFUN |POLYCAT-;solveLinearPolynomialEquation;LSupU;23| (|lpp| |pp| $)
  (SPADCALL |lpp| |pp| (|getShellEntry| $ 137))) 

(DEFUN |POLYCAT-;factorPolynomial;SupF;24| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 142))) 

(DEFUN |POLYCAT-;factorSquareFreePolynomial;SupF;25| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 145))) 

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
                                   (|getShellEntry| $ 148))
                               |POLYCAT-;factor;SF;26|)
                         (EXIT (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |ansR|
                                     (|getShellEntry| $ 150))
                                    (|getShellEntry| $ 51))
                                   (PROGN
                                     (LETT #0# NIL
                                      |POLYCAT-;factor;SF;26|)
                                     (SEQ
                                      (LETT |w| NIL
                                       |POLYCAT-;factor;SF;26|)
                                      (LETT #1#
                                       (SPADCALL |ansR|
                                        (|getShellEntry| $ 154))
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
                                   (|getShellEntry| $ 158)))))
                   ('T
                    (SEQ (LETT |up|
                               (SPADCALL |p| (QCDR |v|)
                                   (|getShellEntry| $ 59))
                               |POLYCAT-;factor;SF;26|)
                         (LETT |ansSUP|
                               (SPADCALL |up| (|getShellEntry| $ 142))
                               |POLYCAT-;factor;SF;26|)
                         (EXIT (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |ansSUP|
                                     (|getShellEntry| $ 159))
                                    (QCDR |v|) (|getShellEntry| $ 160))
                                   (PROGN
                                     (LETT #2# NIL
                                      |POLYCAT-;factor;SF;26|)
                                     (SEQ
                                      (LETT |ww| NIL
                                       |POLYCAT-;factor;SF;26|)
                                      (LETT #3#
                                       (SPADCALL |ansSUP|
                                        (|getShellEntry| $ 163))
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
                                            (|getShellEntry| $ 160))
                                           (QVELT |ww| 2))
                                          #2#)
                                         |POLYCAT-;factor;SF;26|)))
                                      (LETT #3# (CDR #3#)
                                       |POLYCAT-;factor;SF;26|)
                                      (GO G190) G191
                                      (EXIT (NREVERSE0 #2#))))
                                   (|getShellEntry| $ 158))))))))))) 

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
                                    (|getShellEntry| $ 165))
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
                      (LETT |ch| (|spadConstant| $ 168)
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
                                                   169))
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
                                        (|getShellEntry| $ 170))
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
                                                  172))
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
                                                      (COND
                                                        ((< #11# 0)
                                                         'NIL)
                                                        ('T 'T))
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
                                                  174))
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
                                    (|getShellEntry| $ 177))
                                (|getShellEntry| $ 179))
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
                                                         $ 180))
                                                       (|getShellEntry|
                                                        $ 51))
                                                      (|getShellEntry|
                                                       $ 181))
                                                     |POLYCAT-;conditionP;MU;27|)
                                                    (COND
                                                      (#21#
                                                       (LETT #20#
                                                        (SPADCALL #20#
                                                         #19#
                                                         (|getShellEntry|
                                                          $ 182))
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
                                                  (|spadConstant| $ 27)))))))
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
      (SEQ (LETT |vars| (SPADCALL |p| (|getShellEntry| $ 40))
                 |POLYCAT-;charthRoot;SU;28|)
           (EXIT (COND
                   ((NULL |vars|)
                    (SEQ (LETT |ans|
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 174))
                                   (|getShellEntry| $ 184))
                               |POLYCAT-;charthRoot;SU;28|)
                         (EXIT (COND
                                 ((QEQCAR |ans| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |ans|)
                                         (|getShellEntry| $ 51))))))))
                   ('T
                    (SEQ (LETT |ch| (|spadConstant| $ 168)
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
                                    (|getShellEntry| $ 174))
                                   (|getShellEntry| $ 184))
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
                         (SEQ G190 (COND ((NULL (< 0 |d|)) (GO G191)))
                              (SEQ (LETT |dd|
                                    (SPADCALL |d| |ch|
                                     (|getShellEntry| $ 172))
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
                                          (|getShellEntry| $ 189))
                                         |POLYCAT-;charthRootlv|)
                                        (LETT |p|
                                         (SPADCALL |p|
                                          (SPADCALL |cp| |v| |d|
                                           (|getShellEntry| $ 47))
                                          (|getShellEntry| $ 190))
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
                                                    (COND
                                                      ((< #0# 0) 'NIL)
                                                      ('T 'T))
                                                    '(|NonNegativeInteger|)
                                                    #0#))
                                                 (|getShellEntry| $ 47))
                                                (|getShellEntry| $ 182))
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
                                       (|getShellEntry| $ 182)))
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
                     (|getShellEntry| $ 192))
                 |POLYCAT-;monicDivide;2SVarSetR;30|)
           (EXIT (CONS (SPADCALL (QCAR |result|) |mvar|
                           (|getShellEntry| $ 160))
                       (SPADCALL (QCDR |result|) |mvar|
                           (|getShellEntry| $ 160)))))))) 

(DEFUN |POLYCAT-;squareFree;SF;31| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 195))) 

(DEFUN |POLYCAT-;squareFree;SF;32| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 198))) 

(DEFUN |POLYCAT-;squareFree;SF;33| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 198))) 

(DEFUN |POLYCAT-;squareFreePart;2S;34| (|p| $)
  (PROG (|s| |f| #0=#:G1733 #1=#:G1653 #2=#:G1651 #3=#:G1652)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (LETT |s| (SPADCALL |p| (|getShellEntry| $ 199))
                         |POLYCAT-;squareFreePart;2S;34|)
                   (|getShellEntry| $ 200))
               (PROGN
                 (LETT #3# NIL |POLYCAT-;squareFreePart;2S;34|)
                 (SEQ (LETT |f| NIL |POLYCAT-;squareFreePart;2S;34|)
                      (LETT #0# (SPADCALL |s| (|getShellEntry| $ 203))
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
                                        (|getShellEntry| $ 181))
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
                 (COND (#3# #2#) ('T (|spadConstant| $ 43))))
               (|getShellEntry| $ 181)))))) 

(DEFUN |POLYCAT-;content;SVarSetS;35| (|p| |v| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 59))
      (|getShellEntry| $ 205))) 

(DEFUN |POLYCAT-;primitivePart;2S;36| (|p| $)
  (PROG (#0=#:G1657)
    (RETURN
      (QVELT (SPADCALL
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                  (SPADCALL |p|
                                      (|getShellEntry| $ 207))
                                  (|getShellEntry| $ 208))
                              |POLYCAT-;primitivePart;2S;36|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 (|getShellEntry| $ 210))
             1)))) 

(DEFUN |POLYCAT-;primitivePart;SVarSetS;37| (|p| |v| $)
  (PROG (#0=#:G1663)
    (RETURN
      (QVELT (SPADCALL
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                  (SPADCALL |p| |v|
                                      (|getShellEntry| $ 212))
                                  (|getShellEntry| $ 213))
                              |POLYCAT-;primitivePart;SVarSetS;37|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 (|getShellEntry| $ 210))
             1)))) 

(DEFUN |POLYCAT-;before?;2SB;38| (|p| |q| $)
  (PROG (|dp| |dq|)
    (RETURN
      (SEQ (LETT |dp| (SPADCALL |p| (|getShellEntry| $ 75))
                 |POLYCAT-;before?;2SB;38|)
           (LETT |dq| (SPADCALL |q| (|getShellEntry| $ 75))
                 |POLYCAT-;before?;2SB;38|)
           (EXIT (COND
                   ((SPADCALL |dp| |dq| (|getShellEntry| $ 215))
                    (SPADCALL (|spadConstant| $ 28)
                        (SPADCALL |q| (|getShellEntry| $ 48))
                        (|getShellEntry| $ 216)))
                   ((SPADCALL |dq| |dp| (|getShellEntry| $ 215))
                    (SPADCALL (SPADCALL |p| (|getShellEntry| $ 48))
                        (|spadConstant| $ 28) (|getShellEntry| $ 216)))
                   ('T
                    (SPADCALL
                        (SPADCALL (SPADCALL |p| |q|
                                      (|getShellEntry| $ 190))
                                  (|getShellEntry| $ 48))
                        (|spadConstant| $ 28) (|getShellEntry| $ 216))))))))) 

(DEFUN |POLYCAT-;patternMatch;SP2Pmr;39| (|p| |pat| |l| $)
  (SPADCALL |p| |pat| |l| (|getShellEntry| $ 221))) 

(DEFUN |POLYCAT-;patternMatch;SP2Pmr;40| (|p| |pat| |l| $)
  (SPADCALL |p| |pat| |l| (|getShellEntry| $ 228))) 

(DEFUN |POLYCAT-;convert;SP;41| (|x| $)
  (SPADCALL (ELT $ 231) (ELT $ 232) |x| (|getShellEntry| $ 236))) 

(DEFUN |POLYCAT-;convert;SP;42| (|x| $)
  (SPADCALL (ELT $ 238) (ELT $ 239) |x| (|getShellEntry| $ 243))) 

(DEFUN |POLYCAT-;convert;SIf;43| (|p| $)
  (SPADCALL (ELT $ 246) (ELT $ 247) |p| (|getShellEntry| $ 251))) 

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
        (LETT $ (|newShell| 260) . #0#)
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
             (|setShellEntry| $ 120
                 (CONS (|dispatchFunction|
                           |POLYCAT-;reducedSystem;MM;20|)
                       $))
             (|setShellEntry| $ 130
                 (CONS (|dispatchFunction|
                           |POLYCAT-;reducedSystem;MVR;21|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 133
                 (CONS (|dispatchFunction|
                           |POLYCAT-;gcdPolynomial;3Sup;22|)
                       $))
             (|setShellEntry| $ 140
                 (CONS (|dispatchFunction|
                           |POLYCAT-;solveLinearPolynomialEquation;LSupU;23|)
                       $))
             (|setShellEntry| $ 144
                 (CONS (|dispatchFunction|
                           |POLYCAT-;factorPolynomial;SupF;24|)
                       $))
             (|setShellEntry| $ 146
                 (CONS (|dispatchFunction|
                           |POLYCAT-;factorSquareFreePolynomial;SupF;25|)
                       $))
             (|setShellEntry| $ 164
                 (CONS (|dispatchFunction| |POLYCAT-;factor;SF;26|) $))
             (COND
               ((|HasCategory| |#2| '(|CharacteristicNonZero|))
                (PROGN
                  (|setShellEntry| $ 183
                      (CONS (|dispatchFunction|
                                |POLYCAT-;conditionP;MU;27|)
                            $))))))))
        (COND
          ((|HasCategory| |#2| '(|CharacteristicNonZero|))
           (PROGN
             (|setShellEntry| $ 185
                 (CONS (|dispatchFunction| |POLYCAT-;charthRoot;SU;28|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (COND
               ((|HasCategory| |#2| '(|EuclideanDomain|))
                (COND
                  ((|HasCategory| |#2| '(|CharacteristicZero|))
                   (|setShellEntry| $ 196
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;squareFree;SF;31|)
                             $)))
                  ('T
                   (|setShellEntry| $ 196
                       (CONS (|dispatchFunction|
                                 |POLYCAT-;squareFree;SF;32|)
                             $)))))
               ('T
                (|setShellEntry| $ 196
                    (CONS (|dispatchFunction|
                              |POLYCAT-;squareFree;SF;33|)
                          $))))
             (|setShellEntry| $ 204
                 (CONS (|dispatchFunction|
                           |POLYCAT-;squareFreePart;2S;34|)
                       $))
             (|setShellEntry| $ 206
                 (CONS (|dispatchFunction|
                           |POLYCAT-;content;SVarSetS;35|)
                       $))
             (|setShellEntry| $ 211
                 (CONS (|dispatchFunction|
                           |POLYCAT-;primitivePart;2S;36|)
                       $))
             (|setShellEntry| $ 214
                 (CONS (|dispatchFunction|
                           |POLYCAT-;primitivePart;SVarSetS;37|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 8)
           (COND
             ((|testBitVector| |pv$| 7)
              (|setShellEntry| $ 223
                  (CONS (|dispatchFunction|
                            |POLYCAT-;patternMatch;SP2Pmr;39|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 6)
           (COND
             ((|testBitVector| |pv$| 5)
              (|setShellEntry| $ 230
                  (CONS (|dispatchFunction|
                            |POLYCAT-;patternMatch;SP2Pmr;40|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 12)
           (COND
             ((|testBitVector| |pv$| 11)
              (|setShellEntry| $ 237
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SP;41|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 10)
           (COND
             ((|testBitVector| |pv$| 9)
              (|setShellEntry| $ 244
                  (CONS (|dispatchFunction| |POLYCAT-;convert;SP;42|)
                        $))))))
        (COND
          ((|testBitVector| |pv$| 14)
           (COND
             ((|testBitVector| |pv$| 13)
              (|setShellEntry| $ 252
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
             (376 . |empty?|) (381 . |vertConcat|) (|Matrix| $)
             (387 . |reducedSystem|) (|Vector| 6) (392 . |entries|)
             (397 . |concat|) (|List| 8) (403 . |#|) (408 . |first|)
             (413 . |concat|)
             (|Record| (|:| |mat| 110) (|:| |vec| 101)) (|Vector| $)
             (419 . |reducedSystem|)
             (|GeneralPolynomialGcdPackage| 8 9 7 6)
             (425 . |gcdPolynomial|) (431 . |gcdPolynomial|)
             (|List| 60) (|Union| 134 '"failed")
             (|PolynomialFactorizationByRecursion| 7 8 9 6)
             (437 . |solveLinearPolynomialEquationByRecursion|)
             (|List| 58) (|Union| 138 '"failed")
             (443 . |solveLinearPolynomialEquation|) (|Factored| 60)
             (449 . |factorByRecursion|) (|Factored| 58)
             (454 . |factorPolynomial|)
             (459 . |factorSquareFreeByRecursion|)
             (464 . |factorSquareFreePolynomial|) (|Factored| $)
             (469 . |factor|) (|Factored| 7) (474 . |unit|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime")
             (|Record| (|:| |flg| 151) (|:| |fctr| 7) (|:| |xpnt| 103))
             (|List| 152) (479 . |factorList|)
             (|Record| (|:| |flg| 151) (|:| |fctr| 6) (|:| |xpnt| 103))
             (|List| 155) (|Factored| 6) (484 . |makeFR|)
             (490 . |unit|) (495 . |multivariate|)
             (|Record| (|:| |flg| 151) (|:| |fctr| 60)
                 (|:| |xpnt| 103))
             (|List| 161) (501 . |factorList|) (506 . |factor|)
             (511 . |transpose|) (516 . |empty|) (520 . |empty|)
             (524 . |characteristic|) (528 . |setUnion|)
             (534 . |degree|) (|Union| $ '"failed") (540 . |exquo|)
             (546 . |cons|) (552 . |ground|) (557 . |cons|)
             (563 . |cons|) (569 . |transpose|) (|Union| 129 '"failed")
             (574 . |conditionP|) (579 . |elt|) (585 . *) (591 . +)
             (597 . |conditionP|) (602 . |charthRoot|)
             (607 . |charthRoot|) (612 . |Zero|) (616 . |Zero|)
             (620 . <) (626 . |coefficient|) (633 . -)
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (639 . |monicDivide|) |POLYCAT-;monicDivide;2SVarSetR;30|
             (|MultivariateSquareFree| 8 9 7 6) (645 . |squareFree|)
             (650 . |squareFree|) (|PolynomialSquareFree| 9 8 7 6)
             (655 . |squareFree|) (660 . |squareFree|) (665 . |unit|)
             (|Record| (|:| |factor| 6) (|:| |exponent| 103))
             (|List| 201) (670 . |factors|) (675 . |squareFreePart|)
             (680 . |content|) (685 . |content|) (691 . |content|)
             (696 . |exquo|)
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             (702 . |unitNormal|) (707 . |primitivePart|)
             (712 . |content|) (718 . |exquo|) (724 . |primitivePart|)
             (730 . <) (736 . |before?|) |POLYCAT-;before?;2SB;38|
             (|PatternMatchResult| 103 6) (|Pattern| 103)
             (|PatternMatchPolynomialCategory| 103 8 9 7 6)
             (742 . |patternMatch|) (|PatternMatchResult| 103 $)
             (749 . |patternMatch|) (|Float|)
             (|PatternMatchResult| 224 6) (|Pattern| 224)
             (|PatternMatchPolynomialCategory| 224 8 9 7 6)
             (756 . |patternMatch|) (|PatternMatchResult| 224 $)
             (763 . |patternMatch|) (770 . |convert|) (775 . |convert|)
             (|Mapping| 219 9) (|Mapping| 219 7)
             (|PolynomialCategoryLifting| 8 9 7 6 219) (780 . |map|)
             (787 . |convert|) (792 . |convert|) (797 . |convert|)
             (|Mapping| 226 9) (|Mapping| 226 7)
             (|PolynomialCategoryLifting| 8 9 7 6 226) (802 . |map|)
             (809 . |convert|) (|InputForm|) (814 . |convert|)
             (819 . |convert|) (|Mapping| 245 9) (|Mapping| 245 7)
             (|PolynomialCategoryLifting| 8 9 7 6 245) (824 . |map|)
             (831 . |convert|) (|Matrix| 103) (|Vector| 103)
             (|Record| (|:| |mat| 253) (|:| |vec| 254))
             (|Union| 103 '"failed") (|Fraction| 103)
             (|Union| 257 '"failed") (|Union| 7 '"failed"))
          '#(|totalDegree| 836 |squareFreePart| 847 |squareFree| 852
             |solveLinearPolynomialEquation| 857 |retractIfCan| 863
             |retract| 868 |resultant| 873 |reducedSystem| 880
             |primitivePart| 891 |primitiveMonomials| 902
             |patternMatch| 907 |monomials| 921 |monomial| 926
             |monicDivide| 933 |isTimes| 940 |isPlus| 945 |isExpt| 950
             |gcdPolynomial| 955 |factorSquareFreePolynomial| 961
             |factorPolynomial| 966 |factor| 971 |eval| 976
             |discriminant| 982 |convert| 988 |content| 1003
             |conditionP| 1009 |coefficient| 1014 |charthRoot| 1028
             |before?| 1033)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 252
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
                                  0 117 2 110 0 0 0 118 1 0 110 119 120
                                  1 121 25 0 122 2 25 0 0 0 123 1 124
                                  45 0 125 1 25 6 0 126 2 101 0 0 0 127
                                  2 0 128 119 129 130 2 131 60 60 60
                                  132 2 0 58 58 58 133 2 136 135 134 60
                                  137 2 0 139 138 58 140 1 136 141 60
                                  142 1 0 143 58 144 1 136 141 60 145 1
                                  0 143 58 146 1 7 147 0 148 1 149 7 0
                                  150 1 149 153 0 154 2 157 0 6 156 158
                                  1 141 60 0 159 2 6 0 58 9 160 1 141
                                  162 0 163 1 0 147 0 164 1 113 0 0 165
                                  0 108 0 166 0 112 0 167 0 6 45 168 2
                                  25 0 0 0 169 2 6 63 0 19 170 2 103
                                  171 0 0 172 2 25 0 6 0 173 1 6 7 0
                                  174 2 108 0 7 0 175 2 112 0 25 0 176
                                  1 110 0 0 177 1 7 178 119 179 2 101 7
                                  0 103 180 2 6 0 0 0 181 2 6 0 0 0 182
                                  1 0 178 119 183 1 7 171 0 184 1 0 171
                                  0 185 0 8 0 186 0 103 0 187 2 45 10 0
                                  0 188 3 6 0 0 9 45 189 2 6 0 0 0 190
                                  2 60 191 0 0 192 1 194 157 6 195 1 0
                                  147 0 196 1 197 157 6 198 1 6 147 0
                                  199 1 157 6 0 200 1 157 202 0 203 1 0
                                  0 0 204 1 60 6 0 205 2 0 0 0 9 206 1
                                  6 7 0 207 2 6 171 0 7 208 1 6 209 0
                                  210 1 0 0 0 211 2 6 0 0 9 212 2 6 171
                                  0 0 213 2 0 0 0 9 214 2 8 10 0 0 215
                                  2 7 10 0 0 216 3 220 218 6 219 218
                                  221 3 0 222 0 219 222 223 3 227 225 6
                                  226 225 228 3 0 229 0 226 229 230 1 9
                                  219 0 231 1 7 219 0 232 3 235 219 233
                                  234 6 236 1 0 219 0 237 1 9 226 0 238
                                  1 7 226 0 239 3 242 226 240 241 6 243
                                  1 0 226 0 244 1 9 245 0 246 1 7 245 0
                                  247 3 250 245 248 249 6 251 1 0 245 0
                                  252 2 0 45 0 19 93 1 0 45 0 88 1 0 0
                                  0 204 1 0 147 0 196 2 0 139 138 58
                                  140 1 0 15 0 74 1 0 9 0 73 3 0 0 0 0
                                  9 95 2 0 128 119 129 130 1 0 110 119
                                  120 2 0 0 0 9 214 1 0 0 0 211 1 0 20
                                  0 77 3 0 229 0 226 229 230 3 0 222 0
                                  219 222 223 1 0 20 0 34 3 0 0 0 19 63
                                  71 3 0 191 0 0 9 193 1 0 38 0 52 1 0
                                  38 0 39 1 0 56 0 57 2 0 58 58 58 133
                                  1 0 143 58 146 1 0 143 58 144 1 0 147
                                  0 164 2 0 0 0 23 24 2 0 0 0 9 97 1 0
                                  219 0 237 1 0 245 0 252 1 0 226 0 244
                                  2 0 0 0 9 206 1 0 178 119 183 3 0 0 0
                                  9 45 62 3 0 0 0 19 63 69 1 0 171 0
                                  185 2 0 10 0 0 217)))))
          '|lookupComplete|)) 
