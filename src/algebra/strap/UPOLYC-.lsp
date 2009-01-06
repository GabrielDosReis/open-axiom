
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |UPOLYC-;variables;SL;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |UPOLYC-;degree;SSaosNni;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|)
                    (|%IntegerSection| 0))
                |UPOLYC-;totalDegree;SLNni;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%List|)
                |UPOLYC-;degree;SLL;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |UPOLYC-;eval;SLLS;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |UPOLYC-;eval;SSaos2S;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |UPOLYC-;eval;SLLS;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |UPOLYC-;eval;SSaosRS;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |UPOLYC-;eval;SLS;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;mainVariable;SU;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |UPOLYC-;minimumDegree;SSaosNni;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%List|)
                |UPOLYC-;minimumDegree;SLL;12|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |UPOLYC-;monomial;SSaosNniS;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;coerce;SaosS;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;makeSUP;SSup;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;unmakeSUP;SupS;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Pair|)
                |UPOLYC-;karatsubaDivide;SNniR;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |UPOLYC-;shiftRight;SNniS;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |UPOLYC-;shiftLeft;SNniS;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;solveLinearPolynomialEquation;LSupU;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;factorPolynomial;SupF;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;factorSquareFreePolynomial;SupF;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;factor;SF;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    (|%Vector| *))
                |UPOLYC-;vectorise;SNniV;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;retract;SR;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;retractIfCan;SU;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |UPOLYC-;init;S;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;nextItemInner|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;nextItem;SU;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;content;SSaosS;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;primeFactor|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;separate;2SR;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |UPOLYC-;differentiate;SM2S;33|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Thing| |%Shell|)
                    |%Thing|)
                |UPOLYC-;ncdiff|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |UPOLYC-;differentiate;SM2S;35|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;differentiate;SMS;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;differentiate;2S;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;differentiate;SSaosS;38|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;elt;3F;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;pseudoQuotient;3S;40|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Shell|)
                |UPOLYC-;pseudoDivide;2SR;41|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;composite;FSU;42|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;composite;2SU;43|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;elt;S2F;44|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|)
                    (|%IntegerSection| 0))
                |UPOLYC-;order;2SNni;45|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;squareFree;SF;46|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;squareFreePart;2S;47|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;gcdPolynomial;3Sup;48|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;squareFreePolynomial;SupF;49|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;elt;F2R;50|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |UPOLYC-;euclideanSize;SNni;51|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |UPOLYC-;divide;2SR;52|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |UPOLYC-;integrate;2S;53|)) 

(DEFUN |UPOLYC-;variables;SL;1| (|p| $)
  (COND
    ((OR (SPADCALL |p| (|getShellEntry| $ 9))
         (ZEROP (SPADCALL |p| (|getShellEntry| $ 11))))
     NIL)
    ('T (LIST (SPADCALL (|getShellEntry| $ 13)))))) 

(DEFUN |UPOLYC-;degree;SSaosNni;2| (|p| |v| $)
  (SPADCALL |p| (|getShellEntry| $ 11))) 

(DEFUN |UPOLYC-;totalDegree;SLNni;3| (|p| |lv| $)
  (COND ((NULL |lv|) 0) ('T (SPADCALL |p| (|getShellEntry| $ 17))))) 

(DEFUN |UPOLYC-;degree;SLL;4| (|p| |lv| $)
  (COND
    ((NULL |lv|) NIL)
    ('T (LIST (SPADCALL |p| (|getShellEntry| $ 11)))))) 

(DEFUN |UPOLYC-;eval;SLLS;5| (|p| |lv| |lq| $)
  (COND
    ((NULL |lv|) |p|)
    ((NULL (NULL (CDR |lv|)))
     (|error| "can only eval a univariate polynomial once"))
    ('T
     (SPADCALL |p| (|SPADfirst| |lv|) (|SPADfirst| |lq|)
         (|getShellEntry| $ 21))))) 

(DEFUN |UPOLYC-;eval;SSaos2S;6| (|p| |v| |q| $)
  (SPADCALL |p| |q| (|getShellEntry| $ 24))) 

(DEFUN |UPOLYC-;eval;SLLS;7| (|p| |lv| |lr| $)
  (COND
    ((NULL |lv|) |p|)
    ((NULL (NULL (CDR |lv|)))
     (|error| "can only eval a univariate polynomial once"))
    ('T
     (SPADCALL |p| (|SPADfirst| |lv|) (|SPADfirst| |lr|)
         (|getShellEntry| $ 26))))) 

(DEFUN |UPOLYC-;eval;SSaosRS;8| (|p| |v| |r| $)
  (SPADCALL (SPADCALL |p| |r| (|getShellEntry| $ 29))
      (|getShellEntry| $ 30))) 

(DEFUN |UPOLYC-;eval;SLS;9| (|p| |le| $)
  (COND
    ((NULL |le|) |p|)
    ((NULL (NULL (CDR |le|)))
     (|error| "can only eval a univariate polynomial once"))
    ('T
     (COND
       ((QEQCAR (SPADCALL
                    (SPADCALL (|SPADfirst| |le|)
                        (|getShellEntry| $ 33))
                    (|getShellEntry| $ 35))
                1)
        |p|)
       ('T
        (SPADCALL |p|
            (SPADCALL (|SPADfirst| |le|) (|getShellEntry| $ 36))
            (|getShellEntry| $ 24))))))) 

(DEFUN |UPOLYC-;mainVariable;SU;10| (|p| $)
  (COND
    ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11))) (CONS 1 "failed"))
    ('T (CONS 0 (SPADCALL (|getShellEntry| $ 13)))))) 

(DEFUN |UPOLYC-;minimumDegree;SSaosNni;11| (|p| |v| $)
  (SPADCALL |p| (|getShellEntry| $ 41))) 

(DEFUN |UPOLYC-;minimumDegree;SLL;12| (|p| |lv| $)
  (COND
    ((NULL |lv|) NIL)
    ('T (LIST (SPADCALL |p| (|getShellEntry| $ 41)))))) 

(DEFUN |UPOLYC-;monomial;SSaosNniS;13| (|p| |v| |n| $)
  (SPADCALL (CONS #'|UPOLYC-;monomial;SSaosNniS;13!0| (VECTOR $ |n|))
      |p| (|getShellEntry| $ 46))) 

(DEFUN |UPOLYC-;monomial;SSaosNniS;13!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 44))) 

(DEFUN |UPOLYC-;coerce;SaosS;14| (|v| $)
  (SPADCALL (|spadConstant| $ 49) 1 (|getShellEntry| $ 50))) 

(DEFUN |UPOLYC-;makeSUP;SSup;15| (|p| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 9)) (|spadConstant| $ 53))
    ('T
     (SPADCALL
         (SPADCALL (SPADCALL |p| (|getShellEntry| $ 54))
             (SPADCALL |p| (|getShellEntry| $ 11))
             (|getShellEntry| $ 55))
         (SPADCALL (SPADCALL |p| (|getShellEntry| $ 56))
             (|getShellEntry| $ 57))
         (|getShellEntry| $ 58))))) 

(DEFUN |UPOLYC-;unmakeSUP;SupS;16| (|sp| $)
  (COND
    ((SPADCALL |sp| (|getShellEntry| $ 60)) (|spadConstant| $ 61))
    ('T
     (SPADCALL
         (SPADCALL (SPADCALL |sp| (|getShellEntry| $ 62))
             (SPADCALL |sp| (|getShellEntry| $ 63))
             (|getShellEntry| $ 50))
         (SPADCALL (SPADCALL |sp| (|getShellEntry| $ 64))
             (|getShellEntry| $ 65))
         (|getShellEntry| $ 66))))) 

(DEFUN |UPOLYC-;karatsubaDivide;SNniR;17| (|p| |n| $)
  (SPADCALL |p|
      (SPADCALL (|spadConstant| $ 49) |n| (|getShellEntry| $ 50))
      (|getShellEntry| $ 69))) 

(DEFUN |UPOLYC-;shiftRight;SNniS;18| (|p| |n| $)
  (QCAR (SPADCALL |p|
            (SPADCALL (|spadConstant| $ 49) |n| (|getShellEntry| $ 50))
            (|getShellEntry| $ 69)))) 

(DEFUN |UPOLYC-;shiftLeft;SNniS;19| (|p| |n| $)
  (SPADCALL |p|
      (SPADCALL (|spadConstant| $ 49) |n| (|getShellEntry| $ 50))
      (|getShellEntry| $ 72))) 

(DEFUN |UPOLYC-;solveLinearPolynomialEquation;LSupU;20| (|lpp| |pp| $)
  (SPADCALL |lpp| |pp| (|getShellEntry| $ 78))) 

(DEFUN |UPOLYC-;factorPolynomial;SupF;21| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 84))) 

(DEFUN |UPOLYC-;factorSquareFreePolynomial;SupF;22| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 87))) 

(DEFUN |UPOLYC-;factor;SF;23| (|p| $)
  (PROG (|ansR| #0=#:G1691 |w| #1=#:G1692)
    (RETURN
      (SEQ (COND
             ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11)))
              (SEQ (LETT |ansR|
                         (SPADCALL
                             (SPADCALL |p| (|getShellEntry| $ 54))
                             (|getShellEntry| $ 90))
                         |UPOLYC-;factor;SF;23|)
                   (EXIT (SPADCALL
                             (SPADCALL
                                 (SPADCALL |ansR|
                                     (|getShellEntry| $ 92))
                                 (|getShellEntry| $ 30))
                             (PROGN
                               (LETT #0# NIL |UPOLYC-;factor;SF;23|)
                               (SEQ (LETT |w| NIL
                                     |UPOLYC-;factor;SF;23|)
                                    (LETT #1#
                                     (SPADCALL |ansR|
                                      (|getShellEntry| $ 97))
                                     |UPOLYC-;factor;SF;23|)
                                    G190
                                    (COND
                                      ((OR (ATOM #1#)
                                        (PROGN
                                          (LETT |w| (CAR #1#)
                                           |UPOLYC-;factor;SF;23|)
                                          NIL))
                                       (GO G191)))
                                    (SEQ
                                     (EXIT
                                      (LETT #0#
                                       (CONS
                                        (VECTOR (QVELT |w| 0)
                                         (SPADCALL (QVELT |w| 1)
                                          (|getShellEntry| $ 30))
                                         (QVELT |w| 2))
                                        #0#)
                                       |UPOLYC-;factor;SF;23|)))
                                    (LETT #1# (CDR #1#)
                                     |UPOLYC-;factor;SF;23|)
                                    (GO G190) G191
                                    (EXIT (NREVERSE0 #0#))))
                             (|getShellEntry| $ 101)))))
             ('T
              (SPADCALL (ELT $ 65)
                  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 57))
                      (|getShellEntry| $ 102))
                  (|getShellEntry| $ 106)))))))) 

(DEFUN |UPOLYC-;vectorise;SNniV;24| (|p| |n| $)
  (PROG (|v| |m| |i| #0=#:G1693 #1=#:G1522)
    (RETURN
      (SEQ (LETT |m|
                 (SPADCALL
                     (LETT |v|
                           (SPADCALL |n| (|spadConstant| $ 108)
                               (|getShellEntry| $ 110))
                           |UPOLYC-;vectorise;SNniV;24|)
                     (|getShellEntry| $ 111))
                 |UPOLYC-;vectorise;SNniV;24|)
           (SEQ (LETT |i| (SPADCALL |v| (|getShellEntry| $ 111))
                      |UPOLYC-;vectorise;SNniV;24|)
                (LETT #0# (QVSIZE |v|) |UPOLYC-;vectorise;SNniV;24|)
                G190 (COND ((> |i| #0#) (GO G191)))
                (SEQ (EXIT (SPADCALL |v| |i|
                               (SPADCALL |p|
                                         (PROG1
                                          (LETT #1# (- |i| |m|)
                                           |UPOLYC-;vectorise;SNniV;24|)
                                           (|check-subtype|
                                            (COND
                                              ((< #1# 0) 'NIL)
                                              ('T 'T))
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 112))
                               (|getShellEntry| $ 113))))
                (LETT |i| (+ |i| 1) |UPOLYC-;vectorise;SNniV;24|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |v|))))) 

(DEFUN |UPOLYC-;retract;SR;25| (|p| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 9)) (|spadConstant| $ 108))
    ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11)))
     (SPADCALL |p| (|getShellEntry| $ 54)))
    ('T (|error| "Polynomial is not of degree 0")))) 

(DEFUN |UPOLYC-;retractIfCan;SU;26| (|p| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 9))
     (CONS 0 (|spadConstant| $ 108)))
    ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11)))
     (CONS 0 (SPADCALL |p| (|getShellEntry| $ 54))))
    ('T (CONS 1 "failed")))) 

(DEFUN |UPOLYC-;init;S;27| ($)
  (SPADCALL (|spadConstant| $ 118) (|getShellEntry| $ 30))) 

(DEFUN |UPOLYC-;nextItemInner| (|n| $)
  (PROG (|nn| |n1| |n2| #0=#:G1546 |n3|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |n| (|getShellEntry| $ 9))
              (CONS 0
                    (SPADCALL
                        (PROG2 (LETT #0#
                                     (SPADCALL (|spadConstant| $ 108)
                                      (|getShellEntry| $ 121))
                                     |UPOLYC-;nextItemInner|)
                               (QCDR #0#)
                          (|check-union| (QEQCAR #0# 0)
                              (|getShellEntry| $ 7) #0#))
                        (|getShellEntry| $ 30))))
             ((ZEROP (SPADCALL |n| (|getShellEntry| $ 11)))
              (SEQ (LETT |nn|
                         (SPADCALL
                             (SPADCALL |n| (|getShellEntry| $ 54))
                             (|getShellEntry| $ 121))
                         |UPOLYC-;nextItemInner|)
                   (EXIT (COND
                           ((QEQCAR |nn| 1) (CONS 1 "failed"))
                           ('T
                            (CONS 0
                                  (SPADCALL (QCDR |nn|)
                                      (|getShellEntry| $ 30))))))))
             ('T
              (SEQ (LETT |n1| (SPADCALL |n| (|getShellEntry| $ 56))
                         |UPOLYC-;nextItemInner|)
                   (LETT |n2| (|UPOLYC-;nextItemInner| |n1| $)
                         |UPOLYC-;nextItemInner|)
                   (EXIT (COND
                           ((QEQCAR |n2| 0)
                            (CONS 0
                                  (SPADCALL
                                      (SPADCALL
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 54))
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 11))
                                       (|getShellEntry| $ 50))
                                      (QCDR |n2|)
                                      (|getShellEntry| $ 66))))
                           ((< (+ 1
                                  (SPADCALL |n1|
                                      (|getShellEntry| $ 11)))
                               (SPADCALL |n| (|getShellEntry| $ 11)))
                            (CONS 0
                                  (SPADCALL
                                      (SPADCALL
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 54))
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 11))
                                       (|getShellEntry| $ 50))
                                      (SPADCALL
                                       (PROG2
                                        (LETT #0#
                                         (SPADCALL
                                          (|spadConstant| $ 118)
                                          (|getShellEntry| $ 121))
                                         |UPOLYC-;nextItemInner|)
                                        (QCDR #0#)
                                         (|check-union| (QEQCAR #0# 0)
                                          (|getShellEntry| $ 7) #0#))
                                       (+ 1
                                        (SPADCALL |n1|
                                         (|getShellEntry| $ 11)))
                                       (|getShellEntry| $ 50))
                                      (|getShellEntry| $ 66))))
                           ('T
                            (SEQ (LETT |n3|
                                       (SPADCALL
                                        (SPADCALL |n|
                                         (|getShellEntry| $ 54))
                                        (|getShellEntry| $ 121))
                                       |UPOLYC-;nextItemInner|)
                                 (EXIT (COND
                                         ((QEQCAR |n3| 1)
                                          (CONS 1 "failed"))
                                         ('T
                                          (CONS 0
                                           (SPADCALL (QCDR |n3|)
                                            (SPADCALL |n|
                                             (|getShellEntry| $ 11))
                                            (|getShellEntry| $ 50))))))))))))))))) 

(DEFUN |UPOLYC-;nextItem;SU;29| (|n| $)
  (PROG (|n1| #0=#:G1559)
    (RETURN
      (SEQ (LETT |n1| (|UPOLYC-;nextItemInner| |n| $)
                 |UPOLYC-;nextItem;SU;29|)
           (EXIT (COND
                   ((QEQCAR |n1| 1)
                    (CONS 0
                          (SPADCALL
                              (PROG2 (LETT #0#
                                      (SPADCALL (|spadConstant| $ 118)
                                       (|getShellEntry| $ 121))
                                      |UPOLYC-;nextItem;SU;29|)
                                     (QCDR #0#)
                                (|check-union| (QEQCAR #0# 0)
                                    (|getShellEntry| $ 7) #0#))
                              (+ 1
                                 (SPADCALL |n| (|getShellEntry| $ 11)))
                              (|getShellEntry| $ 50))))
                   ('T |n1|))))))) 

(DEFUN |UPOLYC-;content;SSaosS;30| (|p| |v| $)
  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 124))
      (|getShellEntry| $ 30))) 

(DEFUN |UPOLYC-;primeFactor| (|p| |q| $)
  (PROG (#0=#:G1565 |p1|)
    (RETURN
      (SEQ (LETT |p1|
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                        (SPADCALL |p| |q|
                                         (|getShellEntry| $ 126))
                                        (|getShellEntry| $ 127))
                              |UPOLYC-;primeFactor|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 |UPOLYC-;primeFactor|)
           (EXIT (COND
                   ((SPADCALL |p1| |p| (|getShellEntry| $ 128)) |p|)
                   ('T (|UPOLYC-;primeFactor| |p1| |q| $)))))))) 

(DEFUN |UPOLYC-;separate;2SR;32| (|p| |q| $)
  (PROG (|a| #0=#:G1571)
    (RETURN
      (SEQ (LETT |a| (|UPOLYC-;primeFactor| |p| |q| $)
                 |UPOLYC-;separate;2SR;32|)
           (EXIT (CONS |a|
                       (PROG2 (LETT #0#
                                    (SPADCALL |p| |a|
                                     (|getShellEntry| $ 127))
                                    |UPOLYC-;separate;2SR;32|)
                              (QCDR #0#)
                         (|check-union| (QEQCAR #0# 0)
                             (|getShellEntry| $ 6) #0#)))))))) 

(DEFUN |UPOLYC-;differentiate;SM2S;33| (|x| |deriv| |x'| $)
  (PROG (|dg| |lc| #0=#:G1576 |d|)
    (RETURN
      (SEQ (LETT |d| (|spadConstant| $ 61)
                 |UPOLYC-;differentiate;SM2S;33|)
           (SEQ G190
                (COND
                  ((NULL (< 0
                            (LETT |dg|
                                  (SPADCALL |x| (|getShellEntry| $ 11))
                                  |UPOLYC-;differentiate;SM2S;33|)))
                   (GO G191)))
                (SEQ (LETT |lc| (SPADCALL |x| (|getShellEntry| $ 54))
                           |UPOLYC-;differentiate;SM2S;33|)
                     (LETT |d|
                           (SPADCALL
                               (SPADCALL |d|
                                   (SPADCALL |x'|
                                    (SPADCALL
                                     (SPADCALL |dg| |lc|
                                      (|getShellEntry| $ 132))
                                     (PROG1
                                      (LETT #0# (- |dg| 1)
                                       |UPOLYC-;differentiate;SM2S;33|)
                                       (|check-subtype|
                                        (COND
                                          ((< #0# 0) 'NIL)
                                          ('T 'T))
                                        '(|NonNegativeInteger|) #0#))
                                     (|getShellEntry| $ 50))
                                    (|getShellEntry| $ 72))
                                   (|getShellEntry| $ 66))
                               (SPADCALL (SPADCALL |lc| |deriv|) |dg|
                                   (|getShellEntry| $ 50))
                               (|getShellEntry| $ 66))
                           |UPOLYC-;differentiate;SM2S;33|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 56))
                                 |UPOLYC-;differentiate;SM2S;33|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |d|
                     (SPADCALL
                         (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 54))
                             |deriv|)
                         (|getShellEntry| $ 30))
                     (|getShellEntry| $ 66))))))) 

(DEFUN |UPOLYC-;ncdiff| (|n| |x'| $)
  (PROG (#0=#:G1594 |n1|)
    (RETURN
      (COND
        ((ZEROP |n|) (|spadConstant| $ 61))
        ((ZEROP (LETT |n1|
                      (PROG1 (LETT #0# (- |n| 1) |UPOLYC-;ncdiff|)
                        (|check-subtype|
                            (COND ((< #0# 0) 'NIL) ('T 'T))
                            '(|NonNegativeInteger|) #0#))
                      |UPOLYC-;ncdiff|))
         |x'|)
        ('T
         (SPADCALL
             (SPADCALL |x'|
                 (SPADCALL (|spadConstant| $ 49) |n1|
                     (|getShellEntry| $ 50))
                 (|getShellEntry| $ 72))
             (SPADCALL
                 (SPADCALL (|spadConstant| $ 49) 1
                     (|getShellEntry| $ 50))
                 (|UPOLYC-;ncdiff| |n1| |x'| $) (|getShellEntry| $ 72))
             (|getShellEntry| $ 66))))))) 

(DEFUN |UPOLYC-;differentiate;SM2S;35| (|x| |deriv| |x'| $)
  (PROG (|dg| |lc| |d|)
    (RETURN
      (SEQ (LETT |d| (|spadConstant| $ 61)
                 |UPOLYC-;differentiate;SM2S;35|)
           (SEQ G190
                (COND
                  ((NULL (< 0
                            (LETT |dg|
                                  (SPADCALL |x| (|getShellEntry| $ 11))
                                  |UPOLYC-;differentiate;SM2S;35|)))
                   (GO G191)))
                (SEQ (LETT |lc| (SPADCALL |x| (|getShellEntry| $ 54))
                           |UPOLYC-;differentiate;SM2S;35|)
                     (LETT |d|
                           (SPADCALL
                               (SPADCALL |d|
                                   (SPADCALL (SPADCALL |lc| |deriv|)
                                    |dg| (|getShellEntry| $ 50))
                                   (|getShellEntry| $ 66))
                               (SPADCALL |lc|
                                   (|UPOLYC-;ncdiff| |dg| |x'| $)
                                   (|getShellEntry| $ 135))
                               (|getShellEntry| $ 66))
                           |UPOLYC-;differentiate;SM2S;35|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 56))
                                 |UPOLYC-;differentiate;SM2S;35|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |d|
                     (SPADCALL
                         (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 54))
                             |deriv|)
                         (|getShellEntry| $ 30))
                     (|getShellEntry| $ 66))))))) 

(DEFUN |UPOLYC-;differentiate;SMS;36| (|x| |deriv| $)
  (SPADCALL |x| |deriv| (|spadConstant| $ 48) (|getShellEntry| $ 136))) 

(DEFUN |UPOLYC-;differentiate;2S;37| (|x| $)
  (PROG (|dg| #0=#:G1603 |d|)
    (RETURN
      (SEQ (LETT |d| (|spadConstant| $ 61)
                 |UPOLYC-;differentiate;2S;37|)
           (SEQ G190
                (COND
                  ((NULL (< 0
                            (LETT |dg|
                                  (SPADCALL |x| (|getShellEntry| $ 11))
                                  |UPOLYC-;differentiate;2S;37|)))
                   (GO G191)))
                (SEQ (LETT |d|
                           (SPADCALL |d|
                               (SPADCALL
                                   (SPADCALL |dg|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 54))
                                    (|getShellEntry| $ 132))
                                   (PROG1
                                    (LETT #0# (- |dg| 1)
                                     |UPOLYC-;differentiate;2S;37|)
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 50))
                               (|getShellEntry| $ 66))
                           |UPOLYC-;differentiate;2S;37|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 56))
                                 |UPOLYC-;differentiate;2S;37|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |d|))))) 

(DEFUN |UPOLYC-;differentiate;SSaosS;38| (|x| |v| $)
  (SPADCALL |x| (|getShellEntry| $ 139))) 

(DEFUN |UPOLYC-;elt;3F;39| (|g| |f| $)
  (SPADCALL
      (SPADCALL (SPADCALL |g| (|getShellEntry| $ 142)) |f|
          (|getShellEntry| $ 144))
      (SPADCALL (SPADCALL |g| (|getShellEntry| $ 145)) |f|
          (|getShellEntry| $ 144))
      (|getShellEntry| $ 146))) 

(DEFUN |UPOLYC-;pseudoQuotient;3S;40| (|p| |q| $)
  (PROG (|n| #0=#:G1611)
    (RETURN
      (SEQ (LETT |n|
                 (+ (- (SPADCALL |p| (|getShellEntry| $ 11))
                       (SPADCALL |q| (|getShellEntry| $ 11)))
                    1)
                 |UPOLYC-;pseudoQuotient;3S;40|)
           (EXIT (COND
                   ((< |n| 1) (|spadConstant| $ 61))
                   ('T
                    (PROG2 (LETT #0#
                                 (SPADCALL
                                     (SPADCALL
                                      (SPADCALL
                                       (SPADCALL
                                        (SPADCALL |q|
                                         (|getShellEntry| $ 54))
                                        (PROG1 |n|
                                          (|check-subtype|
                                           (COND
                                             ((< |n| 0) 'NIL)
                                             ('T 'T))
                                           '(|NonNegativeInteger|) |n|))
                                        (|getShellEntry| $ 148))
                                       |p| (|getShellEntry| $ 135))
                                      (SPADCALL |p| |q|
                                       (|getShellEntry| $ 149))
                                      (|getShellEntry| $ 150))
                                     |q| (|getShellEntry| $ 127))
                                 |UPOLYC-;pseudoQuotient;3S;40|)
                           (QCDR #0#)
                      (|check-union| (QEQCAR #0# 0)
                          (|getShellEntry| $ 6) #0#))))))))) 

(DEFUN |UPOLYC-;pseudoDivide;2SR;41| (|p| |q| $)
  (PROG (|n| |prem| |lc| #0=#:G1617)
    (RETURN
      (SEQ (LETT |n|
                 (+ (- (SPADCALL |p| (|getShellEntry| $ 11))
                       (SPADCALL |q| (|getShellEntry| $ 11)))
                    1)
                 |UPOLYC-;pseudoDivide;2SR;41|)
           (EXIT (COND
                   ((< |n| 1)
                    (VECTOR (|spadConstant| $ 49) (|spadConstant| $ 61)
                            |p|))
                   ('T
                    (SEQ (LETT |prem|
                               (SPADCALL |p| |q|
                                   (|getShellEntry| $ 149))
                               |UPOLYC-;pseudoDivide;2SR;41|)
                         (LETT |lc|
                               (SPADCALL
                                   (SPADCALL |q|
                                    (|getShellEntry| $ 54))
                                   (PROG1 |n|
                                     (|check-subtype|
                                      (COND ((< |n| 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) |n|))
                                   (|getShellEntry| $ 148))
                               |UPOLYC-;pseudoDivide;2SR;41|)
                         (EXIT (VECTOR |lc|
                                       (PROG2
                                        (LETT #0#
                                         (SPADCALL
                                          (SPADCALL
                                           (SPADCALL |lc| |p|
                                            (|getShellEntry| $ 135))
                                           |prem|
                                           (|getShellEntry| $ 150))
                                          |q| (|getShellEntry| $ 127))
                                         |UPOLYC-;pseudoDivide;2SR;41|)
                                        (QCDR #0#)
                                         (|check-union| (QEQCAR #0# 0)
                                          (|getShellEntry| $ 6) #0#))
                                       |prem|)))))))))) 

(DEFUN |UPOLYC-;composite;FSU;42| (|f| |q| $)
  (PROG (|n| |d|)
    (RETURN
      (SEQ (LETT |n|
                 (SPADCALL (SPADCALL |f| (|getShellEntry| $ 142)) |q|
                     (|getShellEntry| $ 154))
                 |UPOLYC-;composite;FSU;42|)
           (EXIT (COND
                   ((QEQCAR |n| 1) (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |d|
                               (SPADCALL
                                   (SPADCALL |f|
                                    (|getShellEntry| $ 145))
                                   |q| (|getShellEntry| $ 154))
                               |UPOLYC-;composite;FSU;42|)
                         (EXIT (COND
                                 ((QEQCAR |d| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |n|) (QCDR |d|)
                                         (|getShellEntry| $ 155)))))))))))))) 

(DEFUN |UPOLYC-;composite;2SU;43| (|p| |q| $)
  (PROG (|cqr| |v| |u| |w| #0=#:G1643)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 158)) (CONS 0 |p|))
             ('T
              (SEQ (EXIT (SEQ (LETT |cqr|
                                    (SPADCALL |p| |q|
                                     (|getShellEntry| $ 159))
                                    |UPOLYC-;composite;2SU;43|)
                              (COND
                                ((SPADCALL (QVELT |cqr| 2)
                                     (|getShellEntry| $ 158))
                                 (SEQ (LETT |v|
                                       (SPADCALL (QVELT |cqr| 2)
                                        (QVELT |cqr| 0)
                                        (|getShellEntry| $ 160))
                                       |UPOLYC-;composite;2SU;43|)
                                      (EXIT
                                       (COND
                                         ((QEQCAR |v| 0)
                                          (SEQ
                                           (LETT |u|
                                            (SPADCALL (QVELT |cqr| 1)
                                             |q|
                                             (|getShellEntry| $ 154))
                                            |UPOLYC-;composite;2SU;43|)
                                           (EXIT
                                            (COND
                                              ((QEQCAR |u| 0)
                                               (SEQ
                                                (LETT |w|
                                                 (SPADCALL (QCDR |u|)
                                                  (QVELT |cqr| 0)
                                                  (|getShellEntry| $
                                                   160))
                                                 |UPOLYC-;composite;2SU;43|)
                                                (EXIT
                                                 (COND
                                                   ((QEQCAR |w| 0)
                                                    (PROGN
                                                      (LETT #0#
                                                       (CONS 0
                                                        (SPADCALL
                                                         (QCDR |v|)
                                                         (SPADCALL
                                                          (SPADCALL
                                                           (|spadConstant|
                                                            $ 49)
                                                           1
                                                           (|getShellEntry|
                                                            $ 50))
                                                          (QCDR |w|)
                                                          (|getShellEntry|
                                                           $ 72))
                                                         (|getShellEntry|
                                                          $ 66)))
                                                       |UPOLYC-;composite;2SU;43|)
                                                      (GO #0#))))))))))))))))
                              (EXIT (CONS 1 "failed"))))
                   #0# (EXIT #0#)))))))) 

(DEFUN |UPOLYC-;elt;S2F;44| (|p| |f| $)
  (PROG (|n| #0=#:G1649 |ans|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 9))
              (|spadConstant| $ 162))
             ('T
              (SEQ (LETT |ans|
                         (SPADCALL
                             (SPADCALL
                                 (SPADCALL |p| (|getShellEntry| $ 54))
                                 (|getShellEntry| $ 30))
                             (|getShellEntry| $ 163))
                         |UPOLYC-;elt;S2F;44|)
                   (LETT |n| (SPADCALL |p| (|getShellEntry| $ 11))
                         |UPOLYC-;elt;S2F;44|)
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL
                                       (LETT |p|
                                        (SPADCALL |p|
                                         (|getShellEntry| $ 56))
                                        |UPOLYC-;elt;S2F;44|)
                                       (|getShellEntry| $ 9))))
                           (GO G191)))
                        (SEQ (EXIT (LETT |ans|
                                    (SPADCALL
                                     (SPADCALL |ans|
                                      (SPADCALL |f|
                                       (PROG1
                                        (LETT #0#
                                         (- |n|
                                          (LETT |n|
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 11))
                                           |UPOLYC-;elt;S2F;44|))
                                         |UPOLYC-;elt;S2F;44|)
                                         (|check-subtype|
                                          (COND
                                            ((< #0# 0) 'NIL)
                                            ('T 'T))
                                          '(|NonNegativeInteger|) #0#))
                                       (|getShellEntry| $ 164))
                                      (|getShellEntry| $ 165))
                                     (SPADCALL
                                      (SPADCALL
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 54))
                                       (|getShellEntry| $ 30))
                                      (|getShellEntry| $ 163))
                                     (|getShellEntry| $ 166))
                                    |UPOLYC-;elt;S2F;44|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT (COND
                           ((ZEROP |n|) |ans|)
                           ('T
                            (SPADCALL |ans|
                                (SPADCALL |f| |n|
                                    (|getShellEntry| $ 167))
                                (|getShellEntry| $ 165)))))))))))) 

(DEFUN |UPOLYC-;order;2SNni;45| (|p| |q| $)
  (PROG (|u| #0=#:G1663 |ans|)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |p| (|getShellEntry| $ 9))
                    (|error| "order: arguments must be nonzero"))
                   ((< (SPADCALL |q| (|getShellEntry| $ 11)) 1)
                    (|error| "order: place must be non-trivial"))
                   ('T
                    (SEQ (LETT |ans| 0 |UPOLYC-;order;2SNni;45|)
                         (EXIT (SEQ G190 NIL
                                    (SEQ
                                     (LETT |u|
                                      (SPADCALL |p| |q|
                                       (|getShellEntry| $ 127))
                                      |UPOLYC-;order;2SNni;45|)
                                     (EXIT
                                      (COND
                                        ((QEQCAR |u| 1)
                                         (PROGN
                                           (LETT #0# |ans|
                                            |UPOLYC-;order;2SNni;45|)
                                           (GO #0#)))
                                        ('T
                                         (SEQ
                                          (LETT |p| (QCDR |u|)
                                           |UPOLYC-;order;2SNni;45|)
                                          (EXIT
                                           (LETT |ans| (+ |ans| 1)
                                            |UPOLYC-;order;2SNni;45|)))))))
                                    NIL (GO G190) G191 (EXIT NIL)))))))
           #0# (EXIT #0#))))) 

(DEFUN |UPOLYC-;squareFree;SF;46| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 171))) 

(DEFUN |UPOLYC-;squareFreePart;2S;47| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 173))) 

(DEFUN |UPOLYC-;gcdPolynomial;3Sup;48| (|pp| |qq| $)
  (COND
    ((SPADCALL |pp| (|getShellEntry| $ 175))
     (SPADCALL |qq| (|getShellEntry| $ 176)))
    ((SPADCALL |qq| (|getShellEntry| $ 175))
     (SPADCALL |pp| (|getShellEntry| $ 176)))
    ('T
     (SPADCALL
         (SPADCALL
             (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 177))
                 (SPADCALL |qq| (|getShellEntry| $ 177))
                 (|getShellEntry| $ 126))
             (SPADCALL
                 (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 178))
                     (SPADCALL |qq| (|getShellEntry| $ 178))
                     (|getShellEntry| $ 179))
                 (|getShellEntry| $ 178))
             (|getShellEntry| $ 180))
         (|getShellEntry| $ 176))))) 

(DEFUN |UPOLYC-;squareFreePolynomial;SupF;49| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 183))) 

(DEFUN |UPOLYC-;elt;F2R;50| (|f| |r| $)
  (SPADCALL
      (SPADCALL (SPADCALL |f| (|getShellEntry| $ 142)) |r|
          (|getShellEntry| $ 29))
      (SPADCALL (SPADCALL |f| (|getShellEntry| $ 145)) |r|
          (|getShellEntry| $ 29))
      (|getShellEntry| $ 185))) 

(DEFUN |UPOLYC-;euclideanSize;SNni;51| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 9))
     (|error| "euclideanSize called on 0 in Univariate Polynomial"))
    ('T (SPADCALL |x| (|getShellEntry| $ 11))))) 

(DEFUN |UPOLYC-;divide;2SR;52| (|x| |y| $)
  (PROG (|lc| |f| #0=#:G1675 |n| |quot|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |y| (|getShellEntry| $ 9))
              (|error| "division by 0 in Univariate Polynomials"))
             ('T
              (SEQ (LETT |quot| (|spadConstant| $ 61)
                         |UPOLYC-;divide;2SR;52|)
                   (LETT |lc|
                         (SPADCALL
                             (SPADCALL |y| (|getShellEntry| $ 54))
                             (|getShellEntry| $ 188))
                         |UPOLYC-;divide;2SR;52|)
                   (SEQ G190
                        (COND
                          ((NULL (COND
                                   ((SPADCALL |x|
                                     (|getShellEntry| $ 9))
                                    'NIL)
                                   ('T
                                    (NOT
                                     (<
                                      (SPADCALL |x|
                                       (|getShellEntry| $ 11))
                                      (SPADCALL |y|
                                       (|getShellEntry| $ 11)))))))
                           (GO G191)))
                        (SEQ (LETT |f|
                                   (SPADCALL |lc|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 54))
                                    (|getShellEntry| $ 189))
                                   |UPOLYC-;divide;2SR;52|)
                             (LETT |n|
                                   (PROG1
                                    (LETT #0#
                                     (-
                                      (SPADCALL |x|
                                       (|getShellEntry| $ 11))
                                      (SPADCALL |y|
                                       (|getShellEntry| $ 11)))
                                     |UPOLYC-;divide;2SR;52|)
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   |UPOLYC-;divide;2SR;52|)
                             (LETT |quot|
                                   (SPADCALL |quot|
                                    (SPADCALL |f| |n|
                                     (|getShellEntry| $ 50))
                                    (|getShellEntry| $ 66))
                                   |UPOLYC-;divide;2SR;52|)
                             (EXIT (LETT |x|
                                    (SPADCALL |x|
                                     (SPADCALL
                                      (SPADCALL |f| |n|
                                       (|getShellEntry| $ 50))
                                      |y| (|getShellEntry| $ 72))
                                     (|getShellEntry| $ 150))
                                    |UPOLYC-;divide;2SR;52|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT (CONS |quot| |x|))))))))) 

(DEFUN |UPOLYC-;integrate;2S;53| (|p| $)
  (PROG (|l| |d| |ans|)
    (RETURN
      (SEQ (LETT |ans| (|spadConstant| $ 61) |UPOLYC-;integrate;2S;53|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL |p| (|spadConstant| $ 61)
                             (|getShellEntry| $ 191)))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |p| (|getShellEntry| $ 54))
                           |UPOLYC-;integrate;2S;53|)
                     (LETT |d|
                           (+ 1 (SPADCALL |p| (|getShellEntry| $ 11)))
                           |UPOLYC-;integrate;2S;53|)
                     (LETT |ans|
                           (SPADCALL |ans|
                               (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |d|
                                     (|getShellEntry| $ 193))
                                    (|getShellEntry| $ 194))
                                   (SPADCALL |l| |d|
                                    (|getShellEntry| $ 50))
                                   (|getShellEntry| $ 195))
                               (|getShellEntry| $ 66))
                           |UPOLYC-;integrate;2S;53|)
                     (EXIT (LETT |p|
                                 (SPADCALL |p| (|getShellEntry| $ 56))
                                 |UPOLYC-;integrate;2S;53|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |ans|))))) 

(DEFUN |UnivariatePolynomialCategory&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|)
              . #0=(|UnivariatePolynomialCategory&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$|
              (LIST '|UnivariatePolynomialCategory&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 202) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#2|
                                '(|Algebra| (|Fraction| (|Integer|))))
                            (|HasCategory| |#2| '(|Field|))
                            (|HasCategory| |#2| '(|GcdDomain|))
                            (|HasCategory| |#2| '(|IntegralDomain|))
                            (|HasCategory| |#2| '(|CommutativeRing|))
                            (|HasCategory| |#2| '(|StepThrough|)))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasCategory| |#2| '(|PolynomialFactorizationExplicit|))
           (PROGN
             (|setShellEntry| $ 82
                 (CONS (|dispatchFunction|
                           |UPOLYC-;solveLinearPolynomialEquation;LSupU;20|)
                       $))
             (|setShellEntry| $ 86
                 (CONS (|dispatchFunction|
                           |UPOLYC-;factorPolynomial;SupF;21|)
                       $))
             (|setShellEntry| $ 88
                 (CONS (|dispatchFunction|
                           |UPOLYC-;factorSquareFreePolynomial;SupF;22|)
                       $))
             (|setShellEntry| $ 107
                 (CONS (|dispatchFunction| |UPOLYC-;factor;SF;23|) $)))))
        (COND
          ((|testBitVector| |pv$| 6)
           (PROGN
             (|setShellEntry| $ 119
                 (CONS (|dispatchFunction| |UPOLYC-;init;S;27|) $))
             NIL
             (|setShellEntry| $ 123
                 (CONS (|dispatchFunction| |UPOLYC-;nextItem;SU;29|) $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (|setShellEntry| $ 125
                 (CONS (|dispatchFunction| |UPOLYC-;content;SSaosS;30|)
                       $))
             NIL
             (|setShellEntry| $ 130
                 (CONS (|dispatchFunction| |UPOLYC-;separate;2SR;32|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 5)
           (|setShellEntry| $ 134
               (CONS (|dispatchFunction|
                         |UPOLYC-;differentiate;SM2S;33|)
                     $)))
          ('T
           (PROGN
             (|setShellEntry| $ 134
                 (CONS (|dispatchFunction|
                           |UPOLYC-;differentiate;SM2S;35|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 4)
           (PROGN
             (|setShellEntry| $ 147
                 (CONS (|dispatchFunction| |UPOLYC-;elt;3F;39|) $))
             (|setShellEntry| $ 151
                 (CONS (|dispatchFunction|
                           |UPOLYC-;pseudoQuotient;3S;40|)
                       $))
             (|setShellEntry| $ 153
                 (CONS (|dispatchFunction|
                           |UPOLYC-;pseudoDivide;2SR;41|)
                       $))
             (|setShellEntry| $ 157
                 (CONS (|dispatchFunction| |UPOLYC-;composite;FSU;42|)
                       $))
             (|setShellEntry| $ 161
                 (CONS (|dispatchFunction| |UPOLYC-;composite;2SU;43|)
                       $))
             (|setShellEntry| $ 168
                 (CONS (|dispatchFunction| |UPOLYC-;elt;S2F;44|) $))
             (|setShellEntry| $ 169
                 (CONS (|dispatchFunction| |UPOLYC-;order;2SNni;45|) $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (|setShellEntry| $ 172
                 (CONS (|dispatchFunction| |UPOLYC-;squareFree;SF;46|)
                       $))
             (|setShellEntry| $ 174
                 (CONS (|dispatchFunction|
                           |UPOLYC-;squareFreePart;2S;47|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|PolynomialFactorizationExplicit|))
           (PROGN
             (|setShellEntry| $ 181
                 (CONS (|dispatchFunction|
                           |UPOLYC-;gcdPolynomial;3Sup;48|)
                       $))
             (|setShellEntry| $ 184
                 (CONS (|dispatchFunction|
                           |UPOLYC-;squareFreePolynomial;SupF;49|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 2)
           (PROGN
             (|setShellEntry| $ 186
                 (CONS (|dispatchFunction| |UPOLYC-;elt;F2R;50|) $))
             (|setShellEntry| $ 187
                 (CONS (|dispatchFunction|
                           |UPOLYC-;euclideanSize;SNni;51|)
                       $))
             (|setShellEntry| $ 190
                 (CONS (|dispatchFunction| |UPOLYC-;divide;2SR;52|) $)))))
        (COND
          ((|testBitVector| |pv$| 1)
           (|setShellEntry| $ 196
               (CONS (|dispatchFunction| |UPOLYC-;integrate;2S;53|) $))))
        $)))) 

(MAKEPROP '|UnivariatePolynomialCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |zero?|) (|NonNegativeInteger|)
             (5 . |degree|) (|SingletonAsOrderedSet|) (10 . |create|)
             (|List| 12) |UPOLYC-;variables;SL;1|
             |UPOLYC-;degree;SSaosNni;2| (14 . |totalDegree|)
             |UPOLYC-;totalDegree;SLNni;3| (|List| 10)
             |UPOLYC-;degree;SLL;4| (19 . |eval|) (|List| $)
             |UPOLYC-;eval;SLLS;5| (26 . |elt|)
             |UPOLYC-;eval;SSaos2S;6| (32 . |eval|) (|List| 7)
             |UPOLYC-;eval;SLLS;7| (39 . |elt|) (45 . |coerce|)
             |UPOLYC-;eval;SSaosRS;8| (|Equation| 6) (50 . |lhs|)
             (|Union| 12 '"failed") (55 . |mainVariable|) (60 . |rhs|)
             (|Equation| $) (|List| 37) |UPOLYC-;eval;SLS;9|
             |UPOLYC-;mainVariable;SU;10| (65 . |minimumDegree|)
             |UPOLYC-;minimumDegree;SSaosNni;11|
             |UPOLYC-;minimumDegree;SLL;12| (70 . +) (|Mapping| 10 10)
             (76 . |mapExponents|) |UPOLYC-;monomial;SSaosNniS;13|
             (82 . |One|) (86 . |One|) (90 . |monomial|)
             |UPOLYC-;coerce;SaosS;14| (|SparseUnivariatePolynomial| 7)
             (96 . |Zero|) (100 . |leadingCoefficient|)
             (105 . |monomial|) (111 . |reductum|) (116 . |makeSUP|)
             (121 . +) |UPOLYC-;makeSUP;SSup;15| (127 . |zero?|)
             (132 . |Zero|) (136 . |leadingCoefficient|)
             (141 . |degree|) (146 . |reductum|) (151 . |unmakeSUP|)
             (156 . +) |UPOLYC-;unmakeSUP;SupS;16|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (162 . |monicDivide|) |UPOLYC-;karatsubaDivide;SNniR;17|
             |UPOLYC-;shiftRight;SNniS;18| (168 . *)
             |UPOLYC-;shiftLeft;SNniS;19|
             (|SparseUnivariatePolynomial| 6) (|List| 74)
             (|Union| 75 '"failed")
             (|PolynomialFactorizationByRecursionUnivariate| 7 6)
             (174 . |solveLinearPolynomialEquationByRecursion|)
             (|SparseUnivariatePolynomial| $) (|List| 79)
             (|Union| 80 '"failed")
             (180 . |solveLinearPolynomialEquation|) (|Factored| 74)
             (186 . |factorByRecursion|) (|Factored| 79)
             (191 . |factorPolynomial|)
             (196 . |factorSquareFreeByRecursion|)
             (201 . |factorSquareFreePolynomial|) (|Factored| $)
             (206 . |factor|) (|Factored| 7) (211 . |unit|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime") (|Integer|)
             (|Record| (|:| |flg| 93) (|:| |fctr| 7) (|:| |xpnt| 94))
             (|List| 95) (216 . |factorList|)
             (|Record| (|:| |flg| 93) (|:| |fctr| 6) (|:| |xpnt| 94))
             (|List| 98) (|Factored| 6) (221 . |makeFR|)
             (227 . |factorPolynomial|) (|Mapping| 6 52)
             (|Factored| 52) (|FactoredFunctions2| 52 6) (232 . |map|)
             (238 . |factor|) (243 . |Zero|) (|Vector| 7) (247 . |new|)
             (253 . |minIndex|) (258 . |coefficient|)
             (264 . |qsetelt!|) |UPOLYC-;vectorise;SNniV;24|
             |UPOLYC-;retract;SR;25| (|Union| 7 '"failed")
             |UPOLYC-;retractIfCan;SU;26| (271 . |init|) (275 . |init|)
             (|Union| $ '"failed") (279 . |nextItem|) (284 . |One|)
             (288 . |nextItem|) (293 . |content|) (298 . |content|)
             (304 . |gcd|) (310 . |exquo|) (316 . =)
             (|Record| (|:| |primePart| $) (|:| |commonPart| $))
             (322 . |separate|) (328 . |Zero|) (332 . *)
             (|Mapping| 7 7) (338 . |differentiate|) (345 . *)
             (351 . |differentiate|) |UPOLYC-;differentiate;SMS;36|
             |UPOLYC-;differentiate;2S;37| (358 . |differentiate|)
             |UPOLYC-;differentiate;SSaosS;38| (|Fraction| 6)
             (363 . |numer|) (|Fraction| $) (368 . |elt|)
             (374 . |denom|) (379 . /) (385 . |elt|) (391 . **)
             (397 . |pseudoRemainder|) (403 . -)
             (409 . |pseudoQuotient|)
             (|Record| (|:| |coef| 7) (|:| |quotient| $)
                 (|:| |remainder| $))
             (415 . |pseudoDivide|) (421 . |composite|) (427 . /)
             (|Union| 143 '"failed") (433 . |composite|)
             (439 . |ground?|) (444 . |pseudoDivide|) (450 . |exquo|)
             (456 . |composite|) (462 . |Zero|) (466 . |coerce|)
             (471 . **) (477 . *) (483 . +) (489 . **) (495 . |elt|)
             (501 . |order|) (|UnivariatePolynomialSquareFree| 7 6)
             (507 . |squareFree|) (512 . |squareFree|)
             (517 . |squareFreePart|) (522 . |squareFreePart|)
             (527 . |zero?|) (532 . |unitCanonical|) (537 . |content|)
             (542 . |primitivePart|) (547 . |subResultantGcd|)
             (553 . *) (559 . |gcdPolynomial|)
             (|UnivariatePolynomialSquareFree| 6 74)
             (565 . |squareFree|) (570 . |squareFreePolynomial|)
             (575 . /) (581 . |elt|) (587 . |euclideanSize|)
             (592 . |inv|) (597 . *) (603 . |divide|) (609 . ~=)
             (|Fraction| 94) (615 . |coerce|) (620 . |inv|) (625 . *)
             (631 . |integrate|) (|Symbol|) (|List| 197)
             (|Union| 94 '"failed") (|Union| 192 '"failed")
             (|OutputForm|))
          '#(|vectorise| 636 |variables| 642 |unmakeSUP| 647
             |totalDegree| 652 |squareFreePolynomial| 658
             |squareFreePart| 663 |squareFree| 668
             |solveLinearPolynomialEquation| 673 |shiftRight| 679
             |shiftLeft| 685 |separate| 691 |retractIfCan| 697
             |retract| 702 |pseudoQuotient| 707 |pseudoDivide| 713
             |order| 719 |nextItem| 725 |monomial| 730 |minimumDegree|
             737 |makeSUP| 749 |mainVariable| 754 |karatsubaDivide| 759
             |integrate| 765 |init| 770 |gcdPolynomial| 774
             |factorSquareFreePolynomial| 780 |factorPolynomial| 785
             |factor| 790 |eval| 795 |euclideanSize| 829 |elt| 834
             |divide| 852 |differentiate| 858 |degree| 882 |content|
             894 |composite| 900 |coerce| 912)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 196
                                '(1 6 8 0 9 1 6 10 0 11 0 12 0 13 1 6
                                  10 0 17 3 6 0 0 12 0 21 2 6 0 0 0 24
                                  3 6 0 0 12 7 26 2 6 7 0 7 29 1 6 0 7
                                  30 1 32 6 0 33 1 6 34 0 35 1 32 6 0
                                  36 1 6 10 0 41 2 10 0 0 0 44 2 6 0 45
                                  0 46 0 6 0 48 0 7 0 49 2 6 0 7 10 50
                                  0 52 0 53 1 6 7 0 54 2 52 0 7 10 55 1
                                  6 0 0 56 1 6 52 0 57 2 52 0 0 0 58 1
                                  52 8 0 60 0 6 0 61 1 52 7 0 62 1 52
                                  10 0 63 1 52 0 0 64 1 6 0 52 65 2 6 0
                                  0 0 66 2 6 68 0 0 69 2 6 0 0 0 72 2
                                  77 76 75 74 78 2 0 81 80 79 82 1 77
                                  83 74 84 1 0 85 79 86 1 77 83 74 87 1
                                  0 85 79 88 1 7 89 0 90 1 91 7 0 92 1
                                  91 96 0 97 2 100 0 6 99 101 1 7 85 79
                                  102 2 105 100 103 104 106 1 0 89 0
                                  107 0 7 0 108 2 109 0 10 7 110 1 109
                                  94 0 111 2 6 7 0 10 112 3 109 7 0 94
                                  7 113 0 7 0 118 0 0 0 119 1 7 120 0
                                  121 0 74 0 122 1 0 120 0 123 1 6 7 0
                                  124 2 0 0 0 12 125 2 6 0 0 0 126 2 6
                                  120 0 0 127 2 6 8 0 0 128 2 0 129 0 0
                                  130 0 74 0 131 2 7 0 10 0 132 3 0 0 0
                                  133 0 134 2 6 0 7 0 135 3 6 0 0 133 0
                                  136 1 6 0 0 139 1 141 6 0 142 2 6 143
                                  0 143 144 1 141 6 0 145 2 141 0 0 0
                                  146 2 0 143 143 143 147 2 7 0 0 10
                                  148 2 6 0 0 0 149 2 6 0 0 0 150 2 0 0
                                  0 0 151 2 0 152 0 0 153 2 6 120 0 0
                                  154 2 141 0 6 6 155 2 0 156 143 0 157
                                  1 6 8 0 158 2 6 152 0 0 159 2 6 120 0
                                  7 160 2 0 120 0 0 161 0 141 0 162 1
                                  141 0 6 163 2 141 0 0 94 164 2 141 0
                                  0 0 165 2 141 0 0 0 166 2 141 0 0 10
                                  167 2 0 143 0 143 168 2 0 10 0 0 169
                                  1 170 100 6 171 1 0 89 0 172 1 170 6
                                  6 173 1 0 0 0 174 1 74 8 0 175 1 74 0
                                  0 176 1 74 6 0 177 1 74 0 0 178 2 74
                                  0 0 0 179 2 74 0 6 0 180 2 0 79 79 79
                                  181 1 182 83 74 183 1 0 85 79 184 2 7
                                  0 0 0 185 2 0 7 143 7 186 1 0 10 0
                                  187 1 7 0 0 188 2 7 0 0 0 189 2 0 68
                                  0 0 190 2 6 8 0 0 191 1 192 0 94 193
                                  1 192 0 0 194 2 6 0 192 0 195 1 0 0 0
                                  196 2 0 109 0 10 114 1 0 14 0 15 1 0
                                  0 52 67 2 0 10 0 14 18 1 0 85 79 184
                                  1 0 0 0 174 1 0 89 0 172 2 0 81 80 79
                                  82 2 0 0 0 10 71 2 0 0 0 10 73 2 0
                                  129 0 0 130 1 0 116 0 117 1 0 7 0 115
                                  2 0 0 0 0 151 2 0 152 0 0 153 2 0 10
                                  0 0 169 1 0 120 0 123 3 0 0 0 12 10
                                  47 2 0 19 0 14 43 2 0 10 0 12 42 1 0
                                  52 0 59 1 0 34 0 40 2 0 68 0 10 70 1
                                  0 0 0 196 0 0 0 119 2 0 79 79 79 181
                                  1 0 85 79 88 1 0 85 79 86 1 0 89 0
                                  107 3 0 0 0 12 0 25 3 0 0 0 14 22 23
                                  3 0 0 0 14 27 28 3 0 0 0 12 7 31 2 0
                                  0 0 38 39 1 0 10 0 187 2 0 143 0 143
                                  168 2 0 7 143 7 186 2 0 143 143 143
                                  147 2 0 68 0 0 190 3 0 0 0 133 0 134
                                  2 0 0 0 133 137 1 0 0 0 138 2 0 0 0
                                  12 140 2 0 10 0 12 16 2 0 19 0 14 20
                                  2 0 0 0 12 125 2 0 120 0 0 161 2 0
                                  156 143 0 157 1 0 0 12 51)))))
          '|lookupComplete|)) 
