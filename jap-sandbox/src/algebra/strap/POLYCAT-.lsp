
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
  (PROG (|lvar|)
    (RETURN
      (SEQ (COND
             ((NULL |l|) |p|)
             (T (SEQ (LET ((#0=#:G1691 |l|))
                       (LOOP
                         (COND
                           ((ATOM #0#) (RETURN NIL))
                           (T (LET ((|e| (CAR #0#)))
                                (COND
                                  ((EQL
                                    (CAR
                                     (SPADCALL
                                      (SPADCALL |e|
                                       (|getShellEntry| $ 14))
                                      (|getShellEntry| $ 16)))
                                    1)
                                   (RETURN
                                     (|error|
                                      "cannot find a variable to evaluate")))))))
                         (SETQ #0# (CDR #0#))))
                     (LETT |lvar|
                           (LET ((#1=#:G1693 |l|) (#2=#:G1692 NIL))
                             (LOOP
                               (COND
                                 ((ATOM #1#) (RETURN (NREVERSE #2#)))
                                 (T (LET ((|e| (CAR #1#)))
                                      (SETQ #2#
                                       (CONS
                                        (SPADCALL
                                         (SPADCALL |e|
                                          (|getShellEntry| $ 14))
                                         (|getShellEntry| $ 17))
                                        #2#)))))
                               (SETQ #1# (CDR #1#))))
                           |POLYCAT-;eval;SLS;1|)
                     (EXIT (SPADCALL |p| |lvar|
                               (LET ((#3=#:G1695 |l|) (#4=#:G1694 NIL))
                                 (LOOP
                                   (COND
                                     ((ATOM #3#)
                                      (RETURN (NREVERSE #4#)))
                                     (T
                                      (LET ((|e| (CAR #3#)))
                                        (SETQ #4#
                                         (CONS
                                          (SPADCALL |e|
                                           (|getShellEntry| $ 18))
                                          #4#)))))
                                   (SETQ #3# (CDR #3#))))
                               (|getShellEntry| $ 21)))))))))) 

(DEFUN |POLYCAT-;monomials;SL;2| (|p| $)
  (LET ((|ml| NIL))
    (SEQ (LOOP
           (COND
             ((NOT (SPADCALL |p| (|spadConstant| $ 27)
                       (|getShellEntry| $ 29)))
              (RETURN NIL))
             (T (SEQ (SETQ |ml|
                           (CONS (SPADCALL |p| (|getShellEntry| $ 30))
                                 |ml|))
                     (EXIT (SETQ |p|
                                 (SPADCALL |p| (|getShellEntry| $ 32))))))))
         (EXIT (REVERSE |ml|))))) 

(DEFUN |POLYCAT-;isPlus;SU;3| (|p| $)
  (PROG (|l|)
    (RETURN
      (COND
        ((NULL (CDR (LETT |l| (SPADCALL |p| (|getShellEntry| $ 35))
                          |POLYCAT-;isPlus;SU;3|)))
         (CONS 1 "failed"))
        (T (CONS 0 |l|)))))) 

(DEFUN |POLYCAT-;isTimes;SU;4| (|p| $)
  (PROG (|lv| |l| |r|)
    (RETURN
      (SEQ (COND
             ((OR (NULL (LETT |lv|
                              (SPADCALL |p| (|getShellEntry| $ 40))
                              |POLYCAT-;isTimes;SU;4|))
                  (NOT (SPADCALL |p| (|getShellEntry| $ 42))))
              (CONS 1 "failed"))
             (T (SEQ (LETT |l|
                           (LET ((#0=#:G1697 |lv|) (#1=#:G1696 NIL))
                             (LOOP
                               (COND
                                 ((ATOM #0#) (RETURN (NREVERSE #1#)))
                                 (T (LET ((|v| (CAR #0#)))
                                      (SETQ #1#
                                       (CONS
                                        (SPADCALL (|spadConstant| $ 43)
                                         |v|
                                         (SPADCALL |p| |v|
                                          (|getShellEntry| $ 46))
                                         (|getShellEntry| $ 47))
                                        #1#)))))
                               (SETQ #0# (CDR #0#))))
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
                                (T (CONS 0 |l|))))
                             (T (CONS 0
                                      (CONS
                                       (SPADCALL |r|
                                        (|getShellEntry| $ 51))
                                       |l|)))))))))))) 

(DEFUN |POLYCAT-;isExpt;SU;5| (|p| $)
  (PROG (|d|)
    (RETURN
      (LET ((|u| (SPADCALL |p| (|getShellEntry| $ 53))))
        (COND
          ((OR (EQL (CAR |u|) 1)
               (NOT (SPADCALL |p|
                        (SPADCALL (|spadConstant| $ 43) (CDR |u|)
                            (LETT |d|
                                  (SPADCALL |p| (CDR |u|)
                                      (|getShellEntry| $ 46))
                                  |POLYCAT-;isExpt;SU;5|)
                            (|getShellEntry| $ 47))
                        (|getShellEntry| $ 54))))
           (CONS 1 "failed"))
          (T (CONS 0 (CONS (CDR |u|) |d|)))))))) 

(DEFUN |POLYCAT-;coefficient;SVarSetNniS;6| (|p| |v| |n| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 59)) |n|
      (|getShellEntry| $ 61))) 

(DEFUN |POLYCAT-;coefficient;SLLS;7| (|p| |lv| |ln| $)
  (COND
    ((NULL |lv|)
     (COND
       ((NULL |ln|) |p|)
       (T (|error| "mismatched lists in coefficient"))))
    ((NULL |ln|) (|error| "mismatched lists in coefficient"))
    (T (SPADCALL
           (SPADCALL
               (SPADCALL |p| (|SPADfirst| |lv|) (|getShellEntry| $ 59))
               (|SPADfirst| |ln|) (|getShellEntry| $ 61))
           (CDR |lv|) (CDR |ln|) (|getShellEntry| $ 68))))) 

(DEFUN |POLYCAT-;monomial;SLLS;8| (|p| |lv| |ln| $)
  (COND
    ((NULL |lv|)
     (COND
       ((NULL |ln|) |p|)
       (T (|error| "mismatched lists in monomial"))))
    ((NULL |ln|) (|error| "mismatched lists in monomial"))
    (T (SPADCALL
           (SPADCALL |p| (|SPADfirst| |lv|) (|SPADfirst| |ln|)
               (|getShellEntry| $ 47))
           (CDR |lv|) (CDR |ln|) (|getShellEntry| $ 70))))) 

(DEFUN |POLYCAT-;retract;SVarSet;9| (|p| $)
  (LET ((|q| (LET ((#0=#:G1478 (SPADCALL |p| (|getShellEntry| $ 53))))
               (|check-union| (ZEROP (CAR #0#)) (|getShellEntry| $ 9)
                   #0#)
               (CDR #0#))))
    (COND
      ((SPADCALL (SPADCALL |q| (|getShellEntry| $ 72)) |p|
           (|getShellEntry| $ 54))
       |q|)
      (T (|error| "Polynomial is not a single variable"))))) 

(DEFUN |POLYCAT-;retractIfCan;SU;10| (|p| $)
  (PROG (|q| #0=#:G1486)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |q|
                                 (SPADCALL |p| (|getShellEntry| $ 53))
                                 |POLYCAT-;retractIfCan;SU;10|)
                           (EXIT (COND
                                   ((ZEROP (CAR |q|))
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL (CDR |q|)
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
  (LET ((#0=#:G1699 (SPADCALL |p| (|getShellEntry| $ 35)))
        (#1=#:G1698 NIL))
    (LOOP
      (COND
        ((ATOM #0#) (RETURN (NREVERSE #1#)))
        (T (LET ((|q| (CAR #0#)))
             (SETQ #1# (CONS (|POLYCAT-;mkPrim| |q| $) #1#)))))
      (SETQ #0# (CDR #0#))))) 

(DEFUN |POLYCAT-;totalDegree;SNni;13| (|p| $)
  (PROG (|u| |d|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 78)) 0)
             (T (SEQ (LETT |u|
                           (SPADCALL |p|
                               (LET ((#0=#:G1492
                                      (SPADCALL |p|
                                       (|getShellEntry| $ 53))))
                                 (|check-union| (ZEROP (CAR #0#))
                                     (|getShellEntry| $ 9) #0#)
                                 (CDR #0#))
                               (|getShellEntry| $ 59))
                           |POLYCAT-;totalDegree;SNni;13|)
                     (LETT |d| 0 |POLYCAT-;totalDegree;SNni;13|)
                     (LOOP
                       (COND
                         ((NOT (SPADCALL |u| (|spadConstant| $ 80)
                                   (|getShellEntry| $ 81)))
                          (RETURN NIL))
                         (T (SEQ (SETQ |d|
                                       (MAX |d|
                                        (+
                                         (SPADCALL |u|
                                          (|getShellEntry| $ 82))
                                         (SPADCALL
                                          (SPADCALL |u|
                                           (|getShellEntry| $ 83))
                                          (|getShellEntry| $ 84)))))
                                 (EXIT (SETQ |u|
                                        (SPADCALL |u|
                                         (|getShellEntry| $ 87))))))))
                     (EXIT |d|)))))))) 

(DEFUN |POLYCAT-;totalDegree;SLNni;14| (|p| |lv| $)
  (PROG (|v| |u| |d| |w|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 78)) 0)
             (T (SEQ (LETT |u|
                           (SPADCALL |p|
                               (LETT |v|
                                     (LET
                                      ((#0=#:G1500
                                        (SPADCALL |p|
                                         (|getShellEntry| $ 53))))
                                       (|check-union| (ZEROP (CAR #0#))
                                        (|getShellEntry| $ 9) #0#)
                                       (CDR #0#))
                                     |POLYCAT-;totalDegree;SLNni;14|)
                               (|getShellEntry| $ 59))
                           |POLYCAT-;totalDegree;SLNni;14|)
                     (LETT |d| 0 |POLYCAT-;totalDegree;SLNni;14|)
                     (LETT |w| 0 |POLYCAT-;totalDegree;SLNni;14|)
                     (COND
                       ((SPADCALL |v| |lv| (|getShellEntry| $ 89))
                        (SETQ |w| 1)))
                     (LOOP
                       (COND
                         ((NOT (SPADCALL |u| (|spadConstant| $ 80)
                                   (|getShellEntry| $ 81)))
                          (RETURN NIL))
                         (T (SEQ (SETQ |d|
                                       (MAX |d|
                                        (+
                                         (* |w|
                                          (SPADCALL |u|
                                           (|getShellEntry| $ 82)))
                                         (SPADCALL
                                          (SPADCALL |u|
                                           (|getShellEntry| $ 83))
                                          |lv| (|getShellEntry| $ 92)))))
                                 (EXIT (SETQ |u|
                                        (SPADCALL |u|
                                         (|getShellEntry| $ 87))))))))
                     (EXIT |d|)))))))) 

(DEFUN |POLYCAT-;resultant;2SVarSetS;15| (|p1| |p2| |mvar| $)
  (SPADCALL (SPADCALL |p1| |mvar| (|getShellEntry| $ 59))
      (SPADCALL |p2| |mvar| (|getShellEntry| $ 59))
      (|getShellEntry| $ 94))) 

(DEFUN |POLYCAT-;discriminant;SVarSetS;16| (|p| |var| $)
  (SPADCALL (SPADCALL |p| |var| (|getShellEntry| $ 59))
      (|getShellEntry| $ 96))) 

(DEFUN |POLYCAT-;allMonoms| (|l| $)
  (SPADCALL
      (SPADCALL
          (LET ((#0=#:G1701 |l|) (#1=#:G1700 NIL))
            (LOOP
              (COND
                ((ATOM #0#) (RETURN (NREVERSE #1#)))
                (T (LET ((|p| (CAR #0#)))
                     (SETQ #1#
                           (CONS (SPADCALL |p| (|getShellEntry| $ 98))
                                 #1#)))))
              (SETQ #0# (CDR #0#))))
          (|getShellEntry| $ 99))
      (|getShellEntry| $ 100))) 

(DEFUN |POLYCAT-;P2R| (|p| |b| |n| $)
  (LET ((|w| (SPADCALL |n| (|spadConstant| $ 28)
                 (|getShellEntry| $ 102))))
    (SEQ (LET ((|i| (SPADCALL |w| (|getShellEntry| $ 104)))
               (#0=#:G1702 (|sizeOfSimpleArray| |w|)) (#1=#:G1703 |b|))
           (LOOP
             (COND
               ((OR (> |i| #0#) (ATOM #1#)) (RETURN NIL))
               (T (LET ((|bj| (CAR #1#)))
                    (SPADCALL |w| |i|
                        (SPADCALL |p| |bj| (|getShellEntry| $ 106))
                        (|getShellEntry| $ 107)))))
             (SETQ |i| (+ |i| 1))
             (SETQ #1# (CDR #1#))))
         (EXIT |w|)))) 

(DEFUN |POLYCAT-;eq2R| (|l| |b| $)
  (SPADCALL
      (LET ((#0=#:G1707 |b|) (#1=#:G1704 NIL))
        (LOOP
          (COND
            ((ATOM #0#) (RETURN (NREVERSE #1#)))
            (T (LET ((|bj| (CAR #0#)))
                 (SETQ #1#
                       (CONS (LET ((#2=#:G1706 |l|) (#3=#:G1705 NIL))
                               (LOOP
                                 (COND
                                   ((ATOM #2#) (RETURN (NREVERSE #3#)))
                                   (T
                                    (LET ((|p| (CAR #2#)))
                                      (SETQ #3#
                                       (CONS
                                        (SPADCALL |p| |bj|
                                         (|getShellEntry| $ 106))
                                        #3#)))))
                                 (SETQ #2# (CDR #2#))))
                             #1#)))))
          (SETQ #0# (CDR #0#))))
      (|getShellEntry| $ 111))) 

(DEFUN |POLYCAT-;reducedSystem;MM;20| (|m| $)
  (LET* ((|l| (SPADCALL |m| (|getShellEntry| $ 114)))
         (|b| (SPADCALL
                  (SPADCALL
                      (LET ((#0=#:G1709 |l|) (#1=#:G1708 NIL))
                        (LOOP
                          (COND
                            ((ATOM #0#) (RETURN (NREVERSE #1#)))
                            (T (LET ((|r| (CAR #0#)))
                                 (SETQ #1#
                                       (CONS
                                        (|POLYCAT-;allMonoms| |r| $)
                                        #1#)))))
                          (SETQ #0# (CDR #0#))))
                      (|getShellEntry| $ 99))
                  (|getShellEntry| $ 100)))
         (|d| (LET ((#2=#:G1711 |b|) (#3=#:G1710 NIL))
                (LOOP
                  (COND
                    ((ATOM #2#) (RETURN (NREVERSE #3#)))
                    (T (LET ((|bj| (CAR #2#)))
                         (SETQ #3#
                               (CONS (SPADCALL |bj|
                                      (|getShellEntry| $ 75))
                                     #3#)))))
                  (SETQ #2# (CDR #2#)))))
         (|mm| (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d| $)))
    (SEQ (SETQ |l| (CDR |l|))
         (LOOP
           (COND
             ((NOT (NOT (NULL |l|))) (RETURN NIL))
             (T (SEQ (SETQ |mm|
                           (SPADCALL |mm|
                               (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d|
                                   $)
                               (|getShellEntry| $ 119)))
                     (EXIT (SETQ |l| (CDR |l|)))))))
         (EXIT |mm|)))) 

(DEFUN |POLYCAT-;reducedSystem;MVR;21| (|m| |v| $)
  (LET* ((|l| (SPADCALL |m| (|getShellEntry| $ 114)))
         (|r| (SPADCALL |v| (|getShellEntry| $ 123)))
         (|b| (SPADCALL
                  (SPADCALL (|POLYCAT-;allMonoms| |r| $)
                      (SPADCALL
                          (LET ((#0=#:G1713 |l|) (#1=#:G1712 NIL))
                            (LOOP
                              (COND
                                ((ATOM #0#) (RETURN (NREVERSE #1#)))
                                (T (LET ((|s| (CAR #0#)))
                                     (SETQ #1#
                                      (CONS
                                       (|POLYCAT-;allMonoms| |s| $)
                                       #1#)))))
                              (SETQ #0# (CDR #0#))))
                          (|getShellEntry| $ 99))
                      (|getShellEntry| $ 124))
                  (|getShellEntry| $ 100)))
         (|d| (LET ((#2=#:G1715 |b|) (#3=#:G1714 NIL))
                (LOOP
                  (COND
                    ((ATOM #2#) (RETURN (NREVERSE #3#)))
                    (T (LET ((|bj| (CAR #2#)))
                         (SETQ #3#
                               (CONS (SPADCALL |bj|
                                      (|getShellEntry| $ 75))
                                     #3#)))))
                  (SETQ #2# (CDR #2#)))))
         (|n| (LENGTH |d|))
         (|mm| (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d| $))
         (|w| (|POLYCAT-;P2R| (|SPADfirst| |r|) |d| |n| $)))
    (SEQ (SETQ |l| (CDR |l|)) (SETQ |r| (CDR |r|))
         (LOOP
           (COND
             ((NOT (NOT (NULL |l|))) (RETURN NIL))
             (T (SEQ (SETQ |mm|
                           (SPADCALL |mm|
                               (|POLYCAT-;eq2R| (|SPADfirst| |l|) |d|
                                   $)
                               (|getShellEntry| $ 119)))
                     (SETQ |w|
                           (SPADCALL |w|
                               (|POLYCAT-;P2R| (|SPADfirst| |r|) |d|
                                   |n| $)
                               (|getShellEntry| $ 128)))
                     (SETQ |l| (CDR |l|)) (EXIT (SETQ |r| (CDR |r|)))))))
         (EXIT (CONS |mm| |w|))))) 

(DEFUN |POLYCAT-;gcdPolynomial;3Sup;22| (|pp| |qq| $)
  (SPADCALL |pp| |qq| (|getShellEntry| $ 133))) 

(DEFUN |POLYCAT-;solveLinearPolynomialEquation;LSupU;23| (|lpp| |pp| $)
  (SPADCALL |lpp| |pp| (|getShellEntry| $ 138))) 

(DEFUN |POLYCAT-;factorPolynomial;SupF;24| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 143))) 

(DEFUN |POLYCAT-;factorSquareFreePolynomial;SupF;25| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 146))) 

(DEFUN |POLYCAT-;factor;SF;26| (|p| $)
  (PROG (|ansR| |up| |ansSUP|)
    (RETURN
      (LET ((|v| (SPADCALL |p| (|getShellEntry| $ 53))))
        (COND
          ((EQL (CAR |v|) 1)
           (SEQ (LETT |ansR|
                      (SPADCALL (SPADCALL |p| (|getShellEntry| $ 48))
                          (|getShellEntry| $ 149))
                      |POLYCAT-;factor;SF;26|)
                (EXIT (SPADCALL
                          (SPADCALL
                              (SPADCALL |ansR| (|getShellEntry| $ 151))
                              (|getShellEntry| $ 51))
                          (LET ((#0=#:G1717
                                    (SPADCALL |ansR|
                                     (|getShellEntry| $ 155)))
                                (#1=#:G1716 NIL))
                            (LOOP
                              (COND
                                ((ATOM #0#) (RETURN (NREVERSE #1#)))
                                (T (LET ((|w| (CAR #0#)))
                                     (SETQ #1#
                                      (CONS
                                       (VECTOR (QVELT |w| 0)
                                        (SPADCALL (QVELT |w| 1)
                                         (|getShellEntry| $ 51))
                                        (QVELT |w| 2))
                                       #1#)))))
                              (SETQ #0# (CDR #0#))))
                          (|getShellEntry| $ 159)))))
          (T (SEQ (LETT |up|
                        (SPADCALL |p| (CDR |v|) (|getShellEntry| $ 59))
                        |POLYCAT-;factor;SF;26|)
                  (LETT |ansSUP|
                        (SPADCALL |up| (|getShellEntry| $ 143))
                        |POLYCAT-;factor;SF;26|)
                  (EXIT (SPADCALL
                            (SPADCALL
                                (SPADCALL |ansSUP|
                                    (|getShellEntry| $ 160))
                                (CDR |v|) (|getShellEntry| $ 161))
                            (LET ((#2=#:G1719
                                      (SPADCALL |ansSUP|
                                       (|getShellEntry| $ 164)))
                                  (#3=#:G1718 NIL))
                              (LOOP
                                (COND
                                  ((ATOM #2#) (RETURN (NREVERSE #3#)))
                                  (T (LET ((|ww| (CAR #2#)))
                                       (SETQ #3#
                                        (CONS
                                         (VECTOR (QVELT |ww| 0)
                                          (SPADCALL (QVELT |ww| 1)
                                           (CDR |v|)
                                           (|getShellEntry| $ 161))
                                          (QVELT |ww| 2))
                                         #3#)))))
                                (SETQ #2# (CDR #2#))))
                            (|getShellEntry| $ 159)))))))))) 

(DEFUN |POLYCAT-;conditionP;MU;27| (|mat| $)
  (PROG (|nd| |vars| |degs| |deg1| |mons| |redmons| |ans| |i|)
    (RETURN
      (LET* ((|ll| (SPADCALL (SPADCALL |mat| (|getShellEntry| $ 166))
                       (|getShellEntry| $ 114)))
             (|llR| (LET ((#0=#:G1731 (|SPADfirst| |ll|))
                          (#1=#:G1730 NIL))
                      (LOOP
                        (COND
                          ((ATOM #0#) (RETURN (NREVERSE #1#)))
                          (T (LET ((|z| (CAR #0#)))
                               (SETQ #1# (CONS NIL #1#)))))
                        (SETQ #0# (CDR #0#)))))
             (|monslist| NIL) (|ch| (|spadConstant| $ 169)))
        (SEQ (LET ((#2=#:G1720 |ll|))
               (LOOP
                 (COND
                   ((ATOM #2#) (RETURN NIL))
                   (T (LET ((|l| (CAR #2#)))
                        (SEQ (LETT |mons|
                                   (LET
                                    ((#3=#:G1582 NIL) (#4=#:G1583 T)
                                     (#5=#:G1721 |l|))
                                     (LOOP
                                       (COND
                                         ((ATOM #5#)
                                          (RETURN
                                            (COND
                                              (#4#
                                               (|IdentityError|
                                                '|setUnion|))
                                              (T #3#))))
                                         (T
                                          (LET ((|u| (CAR #5#)))
                                            (LET
                                             ((#6=#:G1581
                                               (SPADCALL |u|
                                                (|getShellEntry| $ 98))))
                                              (COND
                                                (#4# (SETQ #3# #6#))
                                                (T
                                                 (SETQ #3#
                                                  (SPADCALL #3# #6#
                                                   (|getShellEntry| $
                                                    170)))))
                                              (SETQ #4# NIL)))))
                                       (SETQ #5# (CDR #5#))))
                                   |POLYCAT-;conditionP;MU;27|)
                             (LETT |redmons| NIL
                                   |POLYCAT-;conditionP;MU;27|)
                             (LET ((#7=#:G1722 |mons|))
                               (LOOP
                                 (COND
                                   ((ATOM #7#) (RETURN NIL))
                                   (T
                                    (LET ((|m| (CAR #7#)))
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
                                        (LET
                                         ((#8=#:G1724 |degs|)
                                          (#9=#:G1723 NIL))
                                          (LOOP
                                            (COND
                                              ((ATOM #8#)
                                               (RETURN (NREVERSE #9#)))
                                              (T
                                               (LET ((|d| (CAR #8#)))
                                                 (SETQ #9#
                                                  (CONS
                                                   (SEQ
                                                    (LETT |nd|
                                                     (SPADCALL |d| |ch|
                                                      (|getShellEntry|
                                                       $ 173))
                                                     |POLYCAT-;conditionP;MU;27|)
                                                    (EXIT
                                                     (COND
                                                       ((EQL (CAR |nd|)
                                                         1)
                                                        (RETURN-FROM
                                                         |POLYCAT-;conditionP;MU;27|
                                                          (CONS 1
                                                           "failed")))
                                                       (T
                                                        (LET
                                                         ((#10=#:G1612
                                                           (CDR |nd|)))
                                                          (|check-subtype|
                                                           (NOT
                                                            (MINUSP
                                                             #10#))
                                                           '(|NonNegativeInteger|)
                                                           #10#))))))
                                                   #9#)))))
                                            (SETQ #8# (CDR #8#))))
                                        |POLYCAT-;conditionP;MU;27|)
                                       (SETQ |redmons|
                                        (CONS
                                         (SPADCALL
                                          (|spadConstant| $ 43) |vars|
                                          |deg1|
                                          (|getShellEntry| $ 70))
                                         |redmons|))
                                       (EXIT
                                        (SETQ |llR|
                                         (LET
                                          ((#11=#:G1726 |l|)
                                           (#12=#:G1727 |llR|)
                                           (#13=#:G1725 NIL))
                                           (LOOP
                                             (COND
                                               ((OR (ATOM #11#)
                                                 (ATOM #12#))
                                                (RETURN
                                                  (NREVERSE #13#)))
                                               (T
                                                (LET
                                                 ((|u| (CAR #11#))
                                                  (|v| (CAR #12#)))
                                                  (SETQ #13#
                                                   (CONS
                                                    (CONS
                                                     (SPADCALL
                                                      (SPADCALL |u|
                                                       |vars| |degs|
                                                       (|getShellEntry|
                                                        $ 68))
                                                      (|getShellEntry|
                                                       $ 175))
                                                     |v|)
                                                    #13#)))))
                                             (SETQ #11# (CDR #11#))
                                             (SETQ #12# (CDR #12#))))))))))
                                 (SETQ #7# (CDR #7#))))
                             (EXIT (SETQ |monslist|
                                    (CONS |redmons| |monslist|)))))))
                 (SETQ #2# (CDR #2#))))
             (LETT |ans|
                   (SPADCALL
                       (SPADCALL
                           (SPADCALL |llR| (|getShellEntry| $ 111))
                           (|getShellEntry| $ 178))
                       (|getShellEntry| $ 180))
                   |POLYCAT-;conditionP;MU;27|)
             (EXIT (COND
                     ((EQL (CAR |ans|) 1) (CONS 1 "failed"))
                     (T (SEQ (LETT |i| 0 |POLYCAT-;conditionP;MU;27|)
                             (EXIT (CONS 0
                                    (LET
                                     ((#14=#:G1611
                                       (|makeSimpleArray|
                                        (|getVMType|
                                         (|getShellEntry| $ 6))
                                        (SIZE |monslist|))))
                                      (LET
                                       ((#15=#:G1728 |monslist|)
                                        (#16=#:G1610 0))
                                        (LOOP
                                          (COND
                                            ((ATOM #15#) (RETURN #14#))
                                            (T
                                             (LET ((|mons| (CAR #15#)))
                                               (|setSimpleArrayEntry|
                                                #14# #16#
                                                (LET
                                                 ((#17=#:G1604 NIL)
                                                  (#18=#:G1605 T)
                                                  (#19=#:G1729 |mons|))
                                                  (LOOP
                                                    (COND
                                                      ((ATOM #19#)
                                                       (RETURN
                                                         (COND
                                                           (#18#
                                                            (|spadConstant|
                                                             $ 27))
                                                           (T #17#))))
                                                      (T
                                                       (LET
                                                        ((|m|
                                                          (CAR #19#)))
                                                         (LET
                                                          ((#20=#:G1603
                                                            (SPADCALL
                                                             |m|
                                                             (SPADCALL
                                                              (SPADCALL
                                                               (CDR
                                                                |ans|)
                                                               (SETQ
                                                                |i|
                                                                (+ |i|
                                                                 1))
                                                               (|getShellEntry|
                                                                $ 181))
                                                              (|getShellEntry|
                                                               $ 51))
                                                             (|getShellEntry|
                                                              $ 182))))
                                                           (COND
                                                             (#18#
                                                              (SETQ
                                                               #17#
                                                               #20#))
                                                             (T
                                                              (SETQ
                                                               #17#
                                                               (SPADCALL
                                                                #17#
                                                                #20#
                                                                (|getShellEntry|
                                                                 $ 183)))))
                                                           (SETQ #18#
                                                            NIL)))))
                                                    (SETQ #19#
                                                     (CDR #19#))))))))
                                          (SETQ #15# (CDR #15#))
                                          (SETQ #16# (+ #16# 1)))))))))))))))) 

(DEFUN |POLYCAT-;charthRoot;SU;28| (|p| $)
  (PROG (|ans| |ch|)
    (RETURN
      (LET ((|vars| (SPADCALL |p| (|getShellEntry| $ 40))))
        (COND
          ((NULL |vars|)
           (SEQ (LETT |ans|
                      (SPADCALL (SPADCALL |p| (|getShellEntry| $ 175))
                          (|getShellEntry| $ 185))
                      |POLYCAT-;charthRoot;SU;28|)
                (EXIT (COND
                        ((EQL (CAR |ans|) 1) (CONS 1 "failed"))
                        (T (CONS 0
                                 (SPADCALL (CDR |ans|)
                                     (|getShellEntry| $ 51))))))))
          (T (SEQ (LETT |ch| (|spadConstant| $ 169)
                        |POLYCAT-;charthRoot;SU;28|)
                  (EXIT (|POLYCAT-;charthRootlv| |p| |vars| |ch| $))))))))) 

(DEFUN |POLYCAT-;charthRootlv| (|p| |vars| |ch| $)
  (PROG (|v| |d| |ans| |dd| |cp| |ansx|)
    (RETURN
      (SEQ (COND
             ((NULL |vars|)
              (SEQ (LETT |ans|
                         (SPADCALL
                             (SPADCALL |p| (|getShellEntry| $ 175))
                             (|getShellEntry| $ 185))
                         |POLYCAT-;charthRootlv|)
                   (EXIT (COND
                           ((EQL (CAR |ans|) 1) (CONS 1 "failed"))
                           (T (CONS 0
                                    (SPADCALL (CDR |ans|)
                                     (|getShellEntry| $ 51))))))))
             (T (SEQ (LETT |v| (|SPADfirst| |vars|)
                           |POLYCAT-;charthRootlv|)
                     (SETQ |vars| (CDR |vars|))
                     (LETT |d|
                           (SPADCALL |p| |v| (|getShellEntry| $ 46))
                           |POLYCAT-;charthRootlv|)
                     (LETT |ans| (|spadConstant| $ 27)
                           |POLYCAT-;charthRootlv|)
                     (LOOP
                       (COND
                         ((NOT (PLUSP |d|)) (RETURN NIL))
                         (T (SEQ (LETT |dd|
                                       (SPADCALL |d| |ch|
                                        (|getShellEntry| $ 173))
                                       |POLYCAT-;charthRootlv|)
                                 (EXIT (COND
                                         ((EQL (CAR |dd|) 1)
                                          (RETURN-FROM
                                           |POLYCAT-;charthRootlv|
                                            (CONS 1 "failed")))
                                         (T
                                          (SEQ
                                           (LETT |cp|
                                            (SPADCALL |p| |v| |d|
                                             (|getShellEntry| $ 188))
                                            |POLYCAT-;charthRootlv|)
                                           (SETQ |p|
                                            (SPADCALL |p|
                                             (SPADCALL |cp| |v| |d|
                                              (|getShellEntry| $ 47))
                                             (|getShellEntry| $ 189)))
                                           (LETT |ansx|
                                            (|POLYCAT-;charthRootlv|
                                             |cp| |vars| |ch| $)
                                            |POLYCAT-;charthRootlv|)
                                           (EXIT
                                            (COND
                                              ((EQL (CAR |ansx|) 1)
                                               (RETURN-FROM
                                                |POLYCAT-;charthRootlv|
                                                 (CONS 1 "failed")))
                                              (T
                                               (SEQ
                                                (SETQ |d|
                                                 (SPADCALL |p| |v|
                                                  (|getShellEntry| $
                                                   46)))
                                                (EXIT
                                                 (SETQ |ans|
                                                  (SPADCALL |ans|
                                                   (SPADCALL
                                                    (CDR |ansx|) |v|
                                                    (LET
                                                     ((#0=#:G1640
                                                       (CDR |dd|)))
                                                      (|check-subtype|
                                                       (NOT
                                                        (MINUSP #0#))
                                                       '(|NonNegativeInteger|)
                                                       #0#))
                                                    (|getShellEntry| $
                                                     47))
                                                   (|getShellEntry| $
                                                    183))))))))))))))))
                     (LETT |ansx|
                           (|POLYCAT-;charthRootlv| |p| |vars| |ch| $)
                           |POLYCAT-;charthRootlv|)
                     (EXIT (COND
                             ((EQL (CAR |ansx|) 1)
                              (RETURN-FROM |POLYCAT-;charthRootlv|
                                (CONS 1 "failed")))
                             (T (RETURN-FROM |POLYCAT-;charthRootlv|
                                  (CONS 0
                                        (SPADCALL |ans| (CDR |ansx|)
                                         (|getShellEntry| $ 183)))))))))))))) 

(DEFUN |POLYCAT-;monicDivide;2SVarSetR;30| (|p1| |p2| |mvar| $)
  (LET ((|result|
            (SPADCALL (SPADCALL |p1| |mvar| (|getShellEntry| $ 59))
                (SPADCALL |p2| |mvar| (|getShellEntry| $ 59))
                (|getShellEntry| $ 191))))
    (CONS (SPADCALL (CAR |result|) |mvar| (|getShellEntry| $ 161))
          (SPADCALL (CDR |result|) |mvar| (|getShellEntry| $ 161))))) 

(DEFUN |POLYCAT-;squareFree;SF;31| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 194))) 

(DEFUN |POLYCAT-;squareFree;SF;32| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 197))) 

(DEFUN |POLYCAT-;squareFree;SF;33| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 197))) 

(DEFUN |POLYCAT-;squareFreePart;2S;34| (|p| $)
  (PROG (|s|)
    (RETURN
      (SPADCALL
          (SPADCALL
              (LETT |s| (SPADCALL |p| (|getShellEntry| $ 198))
                    |POLYCAT-;squareFreePart;2S;34|)
              (|getShellEntry| $ 199))
          (LET ((#0=#:G1653 NIL) (#1=#:G1654 T)
                (#2=#:G1732 (SPADCALL |s| (|getShellEntry| $ 202))))
            (LOOP
              (COND
                ((ATOM #2#)
                 (RETURN (COND (#1# (|spadConstant| $ 43)) (T #0#))))
                (T (LET ((|f| (CAR #2#)))
                     (LET ((#3=#:G1652 (CAR |f|)))
                       (COND
                         (#1# (SETQ #0# #3#))
                         (T (SETQ #0#
                                  (SPADCALL #0# #3#
                                      (|getShellEntry| $ 182)))))
                       (SETQ #1# NIL)))))
              (SETQ #2# (CDR #2#))))
          (|getShellEntry| $ 182))))) 

(DEFUN |POLYCAT-;content;SVarSetS;35| (|p| |v| $)
  (SPADCALL (SPADCALL |p| |v| (|getShellEntry| $ 59))
      (|getShellEntry| $ 204))) 

(DEFUN |POLYCAT-;primitivePart;2S;36| (|p| $)
  (QVELT (SPADCALL
             (LET ((#0=#:G1658
                       (SPADCALL |p|
                           (SPADCALL |p| (|getShellEntry| $ 206))
                           (|getShellEntry| $ 207))))
               (|check-union| (ZEROP (CAR #0#)) (|getShellEntry| $ 6)
                   #0#)
               (CDR #0#))
             (|getShellEntry| $ 209))
         1)) 

(DEFUN |POLYCAT-;primitivePart;SVarSetS;37| (|p| |v| $)
  (QVELT (SPADCALL
             (LET ((#0=#:G1664
                       (SPADCALL |p|
                           (SPADCALL |p| |v| (|getShellEntry| $ 211))
                           (|getShellEntry| $ 212))))
               (|check-union| (ZEROP (CAR #0#)) (|getShellEntry| $ 6)
                   #0#)
               (CDR #0#))
             (|getShellEntry| $ 209))
         1)) 

(DEFUN |POLYCAT-;before?;2SB;38| (|p| |q| $)
  (LET ((|dp| (SPADCALL |p| (|getShellEntry| $ 75)))
        (|dq| (SPADCALL |q| (|getShellEntry| $ 75))))
    (COND
      ((SPADCALL |dp| |dq| (|getShellEntry| $ 214))
       (SPADCALL (|spadConstant| $ 28)
           (SPADCALL |q| (|getShellEntry| $ 48))
           (|getShellEntry| $ 215)))
      ((SPADCALL |dq| |dp| (|getShellEntry| $ 214))
       (SPADCALL (SPADCALL |p| (|getShellEntry| $ 48))
           (|spadConstant| $ 28) (|getShellEntry| $ 215)))
      (T (SPADCALL
             (SPADCALL (SPADCALL |p| |q| (|getShellEntry| $ 189))
                 (|getShellEntry| $ 48))
             (|spadConstant| $ 28) (|getShellEntry| $ 215)))))) 

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
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$3| (|devaluate| |#3|)) (|dv$4| (|devaluate| |#4|))
         (|dv$| (LIST '|PolynomialCategory&| |dv$1| |dv$2| |dv$3|
                      |dv$4|))
         ($ (|newShell| 259))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasCategory| |#2|
                              '(|PolynomialFactorizationExplicit|))
                          (|HasAttribute| |#2| '|canonicalUnitNormal|)
                          (|HasCategory| |#2| '(|GcdDomain|))
                          (|HasCategory| |#2| '(|CommutativeRing|))
                          (|HasCategory| |#4|
                              (LIST '|PatternMatchable| '(|Float|)))
                          (|HasCategory| |#2|
                              (LIST '|PatternMatchable| '(|Float|)))
                          (|HasCategory| |#4|
                              (LIST '|PatternMatchable| '(|Integer|)))
                          (|HasCategory| |#2|
                              (LIST '|PatternMatchable| '(|Integer|)))
                          (|HasCategory| |#4|
                              (LIST '|ConvertibleTo|
                                    (LIST '|Pattern| '(|Float|))))
                          (|HasCategory| |#2|
                              (LIST '|ConvertibleTo|
                                    (LIST '|Pattern| '(|Float|))))
                          (|HasCategory| |#4|
                              (LIST '|ConvertibleTo|
                                    (LIST '|Pattern| '(|Integer|))))
                          (|HasCategory| |#2|
                              (LIST '|ConvertibleTo|
                                    (LIST '|Pattern| '(|Integer|))))
                          (|HasCategory| |#4|
                              (LIST '|ConvertibleTo| '(|InputForm|)))
                          (|HasCategory| |#2|
                              (LIST '|ConvertibleTo| '(|InputForm|)))))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
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
             (CONS (|dispatchFunction| |POLYCAT-;reducedSystem;MM;20|)
                   $))
         (|setShellEntry| $ 131
             (CONS (|dispatchFunction| |POLYCAT-;reducedSystem;MVR;21|)
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
             (CONS (|dispatchFunction| |POLYCAT-;charthRoot;SU;28|) $)))))
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
              (T (|setShellEntry| $ 195
                     (CONS (|dispatchFunction|
                               |POLYCAT-;squareFree;SF;32|)
                           $)))))
           (T (|setShellEntry| $ 195
                  (CONS (|dispatchFunction|
                            |POLYCAT-;squareFree;SF;33|)
                        $))))
         (|setShellEntry| $ 203
             (CONS (|dispatchFunction| |POLYCAT-;squareFreePart;2S;34|)
                   $))
         (|setShellEntry| $ 205
             (CONS (|dispatchFunction| |POLYCAT-;content;SVarSetS;35|)
                   $))
         (|setShellEntry| $ 210
             (CONS (|dispatchFunction| |POLYCAT-;primitivePart;2S;36|)
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
              (CONS (|dispatchFunction| |POLYCAT-;convert;SP;41|) $))))))
    (COND
      ((|testBitVector| |pv$| 10)
       (COND
         ((|testBitVector| |pv$| 9)
          (|setShellEntry| $ 243
              (CONS (|dispatchFunction| |POLYCAT-;convert;SP;42|) $))))))
    (COND
      ((|testBitVector| |pv$| 14)
       (COND
         ((|testBitVector| |pv$| 13)
          (|setShellEntry| $ 251
              (CONS (|dispatchFunction| |POLYCAT-;convert;SIf;43|) $))))))
    $)) 

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
