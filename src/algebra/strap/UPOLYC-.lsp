
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
    ('T (LIST (SPADCALL (|getShellEntry| $ 14)))))) 

(DEFUN |UPOLYC-;degree;SSaosNni;2| (|p| |v| $)
  (SPADCALL |p| (|getShellEntry| $ 11))) 

(DEFUN |UPOLYC-;totalDegree;SLNni;3| (|p| |lv| $)
  (COND ((NULL |lv|) 0) ('T (SPADCALL |p| (|getShellEntry| $ 20))))) 

(DEFUN |UPOLYC-;degree;SLL;4| (|p| |lv| $)
  (COND
    ((NULL |lv|) NIL)
    ('T (LIST (SPADCALL |p| (|getShellEntry| $ 11)))))) 

(DEFUN |UPOLYC-;eval;SLLS;5| (|p| |lv| |lq| $)
  (COND
    ((NULL |lv|) |p|)
    ((NOT (NULL (CDR |lv|)))
     (|error| "can only eval a univariate polynomial once"))
    ('T
     (SPADCALL |p| (|SPADfirst| |lv|) (|SPADfirst| |lq|)
         (|getShellEntry| $ 28))))) 

(DEFUN |UPOLYC-;eval;SSaos2S;6| (|p| |v| |q| $)
  (SPADCALL |p| |q| (|getShellEntry| $ 31))) 

(DEFUN |UPOLYC-;eval;SLLS;7| (|p| |lv| |lr| $)
  (COND
    ((NULL |lv|) |p|)
    ((NOT (NULL (CDR |lv|)))
     (|error| "can only eval a univariate polynomial once"))
    ('T
     (SPADCALL |p| (|SPADfirst| |lv|) (|SPADfirst| |lr|)
         (|getShellEntry| $ 35))))) 

(DEFUN |UPOLYC-;eval;SSaosRS;8| (|p| |v| |r| $)
  (SPADCALL (SPADCALL |p| |r| (|getShellEntry| $ 37))
      (|getShellEntry| $ 38))) 

(DEFUN |UPOLYC-;eval;SLS;9| (|p| |le| $)
  (COND
    ((NULL |le|) |p|)
    ((NOT (NULL (CDR |le|)))
     (|error| "can only eval a univariate polynomial once"))
    ('T
     (COND
       ((QEQCAR (SPADCALL
                    (SPADCALL (|SPADfirst| |le|)
                        (|getShellEntry| $ 45))
                    (|getShellEntry| $ 47))
                1)
        |p|)
       ('T
        (SPADCALL |p|
            (SPADCALL (|SPADfirst| |le|) (|getShellEntry| $ 48))
            (|getShellEntry| $ 31))))))) 

(DEFUN |UPOLYC-;mainVariable;SU;10| (|p| $)
  (COND
    ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11))) (CONS 1 "failed"))
    ('T (CONS 0 (SPADCALL (|getShellEntry| $ 14)))))) 

(DEFUN |UPOLYC-;minimumDegree;SSaosNni;11| (|p| |v| $)
  (SPADCALL |p| (|getShellEntry| $ 53))) 

(DEFUN |UPOLYC-;minimumDegree;SLL;12| (|p| |lv| $)
  (COND
    ((NULL |lv|) NIL)
    ('T (LIST (SPADCALL |p| (|getShellEntry| $ 53)))))) 

(DEFUN |UPOLYC-;monomial;SSaosNniS;13| (|p| |v| |n| $)
  (SPADCALL (CONS #'|UPOLYC-;monomial;SSaosNniS;13!0| (VECTOR $ |n|))
      |p| (|getShellEntry| $ 58))) 

(DEFUN |UPOLYC-;monomial;SSaosNniS;13!0| (|#1| $$)
  (SPADCALL |#1| (|getShellEntry| $$ 1)
      (|getShellEntry| (|getShellEntry| $$ 0) 56))) 

(DEFUN |UPOLYC-;coerce;SaosS;14| (|v| $)
  (SPADCALL (|spadConstant| $ 61) 1 (|getShellEntry| $ 63))) 

(DEFUN |UPOLYC-;makeSUP;SSup;15| (|p| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 9)) (|spadConstant| $ 66))
    ('T
     (SPADCALL
         (SPADCALL (SPADCALL |p| (|getShellEntry| $ 67))
             (SPADCALL |p| (|getShellEntry| $ 11))
             (|getShellEntry| $ 68))
         (SPADCALL (SPADCALL |p| (|getShellEntry| $ 69))
             (|getShellEntry| $ 70))
         (|getShellEntry| $ 71))))) 

(DEFUN |UPOLYC-;unmakeSUP;SupS;16| (|sp| $)
  (COND
    ((SPADCALL |sp| (|getShellEntry| $ 73)) (|spadConstant| $ 74))
    ('T
     (SPADCALL
         (SPADCALL (SPADCALL |sp| (|getShellEntry| $ 75))
             (SPADCALL |sp| (|getShellEntry| $ 76))
             (|getShellEntry| $ 63))
         (SPADCALL (SPADCALL |sp| (|getShellEntry| $ 77))
             (|getShellEntry| $ 78))
         (|getShellEntry| $ 79))))) 

(DEFUN |UPOLYC-;karatsubaDivide;SNniR;17| (|p| |n| $)
  (SPADCALL |p|
      (SPADCALL (|spadConstant| $ 61) |n| (|getShellEntry| $ 63))
      (|getShellEntry| $ 82))) 

(DEFUN |UPOLYC-;shiftRight;SNniS;18| (|p| |n| $)
  (QCAR (SPADCALL |p|
            (SPADCALL (|spadConstant| $ 61) |n| (|getShellEntry| $ 63))
            (|getShellEntry| $ 82)))) 

(DEFUN |UPOLYC-;shiftLeft;SNniS;19| (|p| |n| $)
  (SPADCALL |p|
      (SPADCALL (|spadConstant| $ 61) |n| (|getShellEntry| $ 63))
      (|getShellEntry| $ 85))) 

(DEFUN |UPOLYC-;solveLinearPolynomialEquation;LSupU;20| (|lpp| |pp| $)
  (SPADCALL |lpp| |pp| (|getShellEntry| $ 91))) 

(DEFUN |UPOLYC-;factorPolynomial;SupF;21| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 97))) 

(DEFUN |UPOLYC-;factorSquareFreePolynomial;SupF;22| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 100))) 

(DEFUN |UPOLYC-;factor;SF;23| (|p| $)
  (PROG (|ansR| #0=#:G1691 |w| #1=#:G1692)
    (RETURN
      (SEQ (COND
             ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11)))
              (SEQ (LETT |ansR|
                         (SPADCALL
                             (SPADCALL |p| (|getShellEntry| $ 67))
                             (|getShellEntry| $ 103))
                         |UPOLYC-;factor;SF;23|)
                   (EXIT (SPADCALL
                             (SPADCALL
                                 (SPADCALL |ansR|
                                     (|getShellEntry| $ 105))
                                 (|getShellEntry| $ 38))
                             (PROGN
                               (LETT #0# NIL |UPOLYC-;factor;SF;23|)
                               (SEQ (LETT |w| NIL
                                     |UPOLYC-;factor;SF;23|)
                                    (LETT #1#
                                     (SPADCALL |ansR|
                                      (|getShellEntry| $ 110))
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
                                          (|getShellEntry| $ 38))
                                         (QVELT |w| 2))
                                        #0#)
                                       |UPOLYC-;factor;SF;23|)))
                                    (LETT #1# (CDR #1#)
                                     |UPOLYC-;factor;SF;23|)
                                    (GO G190) G191
                                    (EXIT (NREVERSE0 #0#))))
                             (|getShellEntry| $ 114)))))
             ('T
              (SPADCALL (ELT $ 78)
                  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 70))
                      (|getShellEntry| $ 115))
                  (|getShellEntry| $ 119)))))))) 

(DEFUN |UPOLYC-;vectorise;SNniV;24| (|p| |n| $)
  (PROG (|v| |m| |i| #0=#:G1693 #1=#:G1522)
    (RETURN
      (SEQ (LETT |m|
                 (SPADCALL
                     (LETT |v|
                           (SPADCALL |n| (|spadConstant| $ 121)
                               (|getShellEntry| $ 123))
                           |UPOLYC-;vectorise;SNniV;24|)
                     (|getShellEntry| $ 124))
                 |UPOLYC-;vectorise;SNniV;24|)
           (SEQ (LETT |i| (SPADCALL |v| (|getShellEntry| $ 124))
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
                                         (|getShellEntry| $ 127))
                               (|getShellEntry| $ 128))))
                (LETT |i| (+ |i| 1) |UPOLYC-;vectorise;SNniV;24|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |v|))))) 

(DEFUN |UPOLYC-;retract;SR;25| (|p| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 9)) (|spadConstant| $ 121))
    ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11)))
     (SPADCALL |p| (|getShellEntry| $ 67)))
    ('T (|error| "Polynomial is not of degree 0")))) 

(DEFUN |UPOLYC-;retractIfCan;SU;26| (|p| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 9))
     (CONS 0 (|spadConstant| $ 121)))
    ((ZEROP (SPADCALL |p| (|getShellEntry| $ 11)))
     (CONS 0 (SPADCALL |p| (|getShellEntry| $ 67))))
    ('T (CONS 1 "failed")))) 

(DEFUN |UPOLYC-;init;S;27| ($)
  (SPADCALL (|spadConstant| $ 133) (|getShellEntry| $ 38))) 

(DEFUN |UPOLYC-;nextItemInner| (|n| $)
  (PROG (|nn| |n1| |n2| #0=#:G1546 |n3|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |n| (|getShellEntry| $ 9))
              (CONS 0
                    (SPADCALL
                        (PROG2 (LETT #0#
                                     (SPADCALL (|spadConstant| $ 121)
                                      (|getShellEntry| $ 136))
                                     |UPOLYC-;nextItemInner|)
                               (QCDR #0#)
                          (|check-union| (QEQCAR #0# 0)
                              (|getShellEntry| $ 7) #0#))
                        (|getShellEntry| $ 38))))
             ((ZEROP (SPADCALL |n| (|getShellEntry| $ 11)))
              (SEQ (LETT |nn|
                         (SPADCALL
                             (SPADCALL |n| (|getShellEntry| $ 67))
                             (|getShellEntry| $ 136))
                         |UPOLYC-;nextItemInner|)
                   (EXIT (COND
                           ((QEQCAR |nn| 1) (CONS 1 "failed"))
                           ('T
                            (CONS 0
                                  (SPADCALL (QCDR |nn|)
                                      (|getShellEntry| $ 38))))))))
             ('T
              (SEQ (LETT |n1| (SPADCALL |n| (|getShellEntry| $ 69))
                         |UPOLYC-;nextItemInner|)
                   (LETT |n2| (|UPOLYC-;nextItemInner| |n1| $)
                         |UPOLYC-;nextItemInner|)
                   (EXIT (COND
                           ((QEQCAR |n2| 0)
                            (CONS 0
                                  (SPADCALL
                                      (SPADCALL
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 67))
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 11))
                                       (|getShellEntry| $ 63))
                                      (QCDR |n2|)
                                      (|getShellEntry| $ 79))))
                           ((< (+ 1
                                  (SPADCALL |n1|
                                      (|getShellEntry| $ 11)))
                               (SPADCALL |n| (|getShellEntry| $ 11)))
                            (CONS 0
                                  (SPADCALL
                                      (SPADCALL
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 67))
                                       (SPADCALL |n|
                                        (|getShellEntry| $ 11))
                                       (|getShellEntry| $ 63))
                                      (SPADCALL
                                       (PROG2
                                        (LETT #0#
                                         (SPADCALL
                                          (|spadConstant| $ 133)
                                          (|getShellEntry| $ 136))
                                         |UPOLYC-;nextItemInner|)
                                        (QCDR #0#)
                                         (|check-union| (QEQCAR #0# 0)
                                          (|getShellEntry| $ 7) #0#))
                                       (+ 1
                                        (SPADCALL |n1|
                                         (|getShellEntry| $ 11)))
                                       (|getShellEntry| $ 63))
                                      (|getShellEntry| $ 79))))
                           ('T
                            (SEQ (LETT |n3|
                                       (SPADCALL
                                        (SPADCALL |n|
                                         (|getShellEntry| $ 67))
                                        (|getShellEntry| $ 136))
                                       |UPOLYC-;nextItemInner|)
                                 (EXIT (COND
                                         ((QEQCAR |n3| 1)
                                          (CONS 1 "failed"))
                                         ('T
                                          (CONS 0
                                           (SPADCALL (QCDR |n3|)
                                            (SPADCALL |n|
                                             (|getShellEntry| $ 11))
                                            (|getShellEntry| $ 63))))))))))))))))) 

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
                                      (SPADCALL (|spadConstant| $ 133)
                                       (|getShellEntry| $ 136))
                                      |UPOLYC-;nextItem;SU;29|)
                                     (QCDR #0#)
                                (|check-union| (QEQCAR #0# 0)
                                    (|getShellEntry| $ 7) #0#))
                              (+ 1
                                 (SPADCALL |n| (|getShellEntry| $ 11)))
                              (|getShellEntry| $ 63))))
                   ('T |n1|))))))) 

(DEFUN |UPOLYC-;content;SSaosS;30| (|p| |v| $)
  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 140))
      (|getShellEntry| $ 38))) 

(DEFUN |UPOLYC-;primeFactor| (|p| |q| $)
  (PROG (#0=#:G1565 |p1|)
    (RETURN
      (SEQ (LETT |p1|
                 (PROG2 (LETT #0#
                              (SPADCALL |p|
                                        (SPADCALL |p| |q|
                                         (|getShellEntry| $ 142))
                                        (|getShellEntry| $ 143))
                              |UPOLYC-;primeFactor|)
                        (QCDR #0#)
                   (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 6)
                       #0#))
                 |UPOLYC-;primeFactor|)
           (EXIT (COND
                   ((SPADCALL |p1| |p| (|getShellEntry| $ 144)) |p|)
                   ('T (|UPOLYC-;primeFactor| |p1| |q| $)))))))) 

(DEFUN |UPOLYC-;separate;2SR;32| (|p| |q| $)
  (PROG (|a| #0=#:G1571)
    (RETURN
      (SEQ (LETT |a| (|UPOLYC-;primeFactor| |p| |q| $)
                 |UPOLYC-;separate;2SR;32|)
           (EXIT (CONS |a|
                       (PROG2 (LETT #0#
                                    (SPADCALL |p| |a|
                                     (|getShellEntry| $ 143))
                                    |UPOLYC-;separate;2SR;32|)
                              (QCDR #0#)
                         (|check-union| (QEQCAR #0# 0)
                             (|getShellEntry| $ 6) #0#)))))))) 

(DEFUN |UPOLYC-;differentiate;SM2S;33| (|x| |deriv| |x'| $)
  (PROG (|dg| |lc| #0=#:G1576 |d|)
    (RETURN
      (SEQ (LETT |d| (|spadConstant| $ 74)
                 |UPOLYC-;differentiate;SM2S;33|)
           (SEQ G190
                (COND
                  ((NULL (< 0
                            (LETT |dg|
                                  (SPADCALL |x| (|getShellEntry| $ 11))
                                  |UPOLYC-;differentiate;SM2S;33|)))
                   (GO G191)))
                (SEQ (LETT |lc| (SPADCALL |x| (|getShellEntry| $ 67))
                           |UPOLYC-;differentiate;SM2S;33|)
                     (LETT |d|
                           (SPADCALL
                               (SPADCALL |d|
                                   (SPADCALL |x'|
                                    (SPADCALL
                                     (SPADCALL |dg| |lc|
                                      (|getShellEntry| $ 149))
                                     (PROG1
                                      (LETT #0# (- |dg| 1)
                                       |UPOLYC-;differentiate;SM2S;33|)
                                       (|check-subtype|
                                        (COND
                                          ((< #0# 0) 'NIL)
                                          ('T 'T))
                                        '(|NonNegativeInteger|) #0#))
                                     (|getShellEntry| $ 63))
                                    (|getShellEntry| $ 85))
                                   (|getShellEntry| $ 79))
                               (SPADCALL (SPADCALL |lc| |deriv|) |dg|
                                   (|getShellEntry| $ 63))
                               (|getShellEntry| $ 79))
                           |UPOLYC-;differentiate;SM2S;33|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 69))
                                 |UPOLYC-;differentiate;SM2S;33|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |d|
                     (SPADCALL
                         (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 67))
                             |deriv|)
                         (|getShellEntry| $ 38))
                     (|getShellEntry| $ 79))))))) 

(DEFUN |UPOLYC-;ncdiff| (|n| |x'| $)
  (PROG (#0=#:G1594 |n1|)
    (RETURN
      (COND
        ((ZEROP |n|) (|spadConstant| $ 74))
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
                 (SPADCALL (|spadConstant| $ 61) |n1|
                     (|getShellEntry| $ 63))
                 (|getShellEntry| $ 85))
             (SPADCALL
                 (SPADCALL (|spadConstant| $ 61) 1
                     (|getShellEntry| $ 63))
                 (|UPOLYC-;ncdiff| |n1| |x'| $) (|getShellEntry| $ 85))
             (|getShellEntry| $ 79))))))) 

(DEFUN |UPOLYC-;differentiate;SM2S;35| (|x| |deriv| |x'| $)
  (PROG (|dg| |lc| |d|)
    (RETURN
      (SEQ (LETT |d| (|spadConstant| $ 74)
                 |UPOLYC-;differentiate;SM2S;35|)
           (SEQ G190
                (COND
                  ((NULL (< 0
                            (LETT |dg|
                                  (SPADCALL |x| (|getShellEntry| $ 11))
                                  |UPOLYC-;differentiate;SM2S;35|)))
                   (GO G191)))
                (SEQ (LETT |lc| (SPADCALL |x| (|getShellEntry| $ 67))
                           |UPOLYC-;differentiate;SM2S;35|)
                     (LETT |d|
                           (SPADCALL
                               (SPADCALL |d|
                                   (SPADCALL (SPADCALL |lc| |deriv|)
                                    |dg| (|getShellEntry| $ 63))
                                   (|getShellEntry| $ 79))
                               (SPADCALL |lc|
                                   (|UPOLYC-;ncdiff| |dg| |x'| $)
                                   (|getShellEntry| $ 154))
                               (|getShellEntry| $ 79))
                           |UPOLYC-;differentiate;SM2S;35|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 69))
                                 |UPOLYC-;differentiate;SM2S;35|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |d|
                     (SPADCALL
                         (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 67))
                             |deriv|)
                         (|getShellEntry| $ 38))
                     (|getShellEntry| $ 79))))))) 

(DEFUN |UPOLYC-;differentiate;SMS;36| (|x| |deriv| $)
  (SPADCALL |x| |deriv| (|spadConstant| $ 60) (|getShellEntry| $ 155))) 

(DEFUN |UPOLYC-;differentiate;2S;37| (|x| $)
  (PROG (|dg| #0=#:G1603 |d|)
    (RETURN
      (SEQ (LETT |d| (|spadConstant| $ 74)
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
                                     (|getShellEntry| $ 67))
                                    (|getShellEntry| $ 149))
                                   (PROG1
                                    (LETT #0# (- |dg| 1)
                                     |UPOLYC-;differentiate;2S;37|)
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 63))
                               (|getShellEntry| $ 79))
                           |UPOLYC-;differentiate;2S;37|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 69))
                                 |UPOLYC-;differentiate;2S;37|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |d|))))) 

(DEFUN |UPOLYC-;differentiate;SSaosS;38| (|x| |v| $)
  (SPADCALL |x| (|getShellEntry| $ 158))) 

(DEFUN |UPOLYC-;elt;3F;39| (|g| |f| $)
  (SPADCALL
      (SPADCALL (SPADCALL |g| (|getShellEntry| $ 161)) |f|
          (|getShellEntry| $ 163))
      (SPADCALL (SPADCALL |g| (|getShellEntry| $ 164)) |f|
          (|getShellEntry| $ 163))
      (|getShellEntry| $ 165))) 

(DEFUN |UPOLYC-;pseudoQuotient;3S;40| (|p| |q| $)
  (PROG (|n| #0=#:G1611)
    (RETURN
      (SEQ (LETT |n|
                 (+ (- (SPADCALL |p| (|getShellEntry| $ 11))
                       (SPADCALL |q| (|getShellEntry| $ 11)))
                    1)
                 |UPOLYC-;pseudoQuotient;3S;40|)
           (EXIT (COND
                   ((< |n| 1) (|spadConstant| $ 74))
                   ('T
                    (PROG2 (LETT #0#
                                 (SPADCALL
                                     (SPADCALL
                                      (SPADCALL
                                       (SPADCALL
                                        (SPADCALL |q|
                                         (|getShellEntry| $ 67))
                                        (PROG1 |n|
                                          (|check-subtype|
                                           (COND
                                             ((< |n| 0) 'NIL)
                                             ('T 'T))
                                           '(|NonNegativeInteger|) |n|))
                                        (|getShellEntry| $ 169))
                                       |p| (|getShellEntry| $ 154))
                                      (SPADCALL |p| |q|
                                       (|getShellEntry| $ 170))
                                      (|getShellEntry| $ 171))
                                     |q| (|getShellEntry| $ 143))
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
                    (VECTOR (|spadConstant| $ 61) (|spadConstant| $ 74)
                            |p|))
                   ('T
                    (SEQ (LETT |prem|
                               (SPADCALL |p| |q|
                                   (|getShellEntry| $ 170))
                               |UPOLYC-;pseudoDivide;2SR;41|)
                         (LETT |lc|
                               (SPADCALL
                                   (SPADCALL |q|
                                    (|getShellEntry| $ 67))
                                   (PROG1 |n|
                                     (|check-subtype|
                                      (COND ((< |n| 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) |n|))
                                   (|getShellEntry| $ 169))
                               |UPOLYC-;pseudoDivide;2SR;41|)
                         (EXIT (VECTOR |lc|
                                       (PROG2
                                        (LETT #0#
                                         (SPADCALL
                                          (SPADCALL
                                           (SPADCALL |lc| |p|
                                            (|getShellEntry| $ 154))
                                           |prem|
                                           (|getShellEntry| $ 171))
                                          |q| (|getShellEntry| $ 143))
                                         |UPOLYC-;pseudoDivide;2SR;41|)
                                        (QCDR #0#)
                                         (|check-union| (QEQCAR #0# 0)
                                          (|getShellEntry| $ 6) #0#))
                                       |prem|)))))))))) 

(DEFUN |UPOLYC-;composite;FSU;42| (|f| |q| $)
  (PROG (|n| |d|)
    (RETURN
      (SEQ (LETT |n|
                 (SPADCALL (SPADCALL |f| (|getShellEntry| $ 161)) |q|
                     (|getShellEntry| $ 175))
                 |UPOLYC-;composite;FSU;42|)
           (EXIT (COND
                   ((QEQCAR |n| 1) (CONS 1 "failed"))
                   ('T
                    (SEQ (LETT |d|
                               (SPADCALL
                                   (SPADCALL |f|
                                    (|getShellEntry| $ 164))
                                   |q| (|getShellEntry| $ 175))
                               |UPOLYC-;composite;FSU;42|)
                         (EXIT (COND
                                 ((QEQCAR |d| 1) (CONS 1 "failed"))
                                 ('T
                                  (CONS 0
                                        (SPADCALL (QCDR |n|) (QCDR |d|)
                                         (|getShellEntry| $ 176)))))))))))))) 

(DEFUN |UPOLYC-;composite;2SU;43| (|p| |q| $)
  (PROG (|cqr| |v| |u| |w| #0=#:G1643)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 179)) (CONS 0 |p|))
             ('T
              (SEQ (EXIT (SEQ (LETT |cqr|
                                    (SPADCALL |p| |q|
                                     (|getShellEntry| $ 180))
                                    |UPOLYC-;composite;2SU;43|)
                              (COND
                                ((SPADCALL (QVELT |cqr| 2)
                                     (|getShellEntry| $ 179))
                                 (SEQ (LETT |v|
                                       (SPADCALL (QVELT |cqr| 2)
                                        (QVELT |cqr| 0)
                                        (|getShellEntry| $ 181))
                                       |UPOLYC-;composite;2SU;43|)
                                      (EXIT
                                       (COND
                                         ((QEQCAR |v| 0)
                                          (SEQ
                                           (LETT |u|
                                            (SPADCALL (QVELT |cqr| 1)
                                             |q|
                                             (|getShellEntry| $ 175))
                                            |UPOLYC-;composite;2SU;43|)
                                           (EXIT
                                            (COND
                                              ((QEQCAR |u| 0)
                                               (SEQ
                                                (LETT |w|
                                                 (SPADCALL (QCDR |u|)
                                                  (QVELT |cqr| 0)
                                                  (|getShellEntry| $
                                                   181))
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
                                                            $ 61)
                                                           1
                                                           (|getShellEntry|
                                                            $ 63))
                                                          (QCDR |w|)
                                                          (|getShellEntry|
                                                           $ 85))
                                                         (|getShellEntry|
                                                          $ 79)))
                                                       |UPOLYC-;composite;2SU;43|)
                                                      (GO #0#))))))))))))))))
                              (EXIT (CONS 1 "failed"))))
                   #0# (EXIT #0#)))))))) 

(DEFUN |UPOLYC-;elt;S2F;44| (|p| |f| $)
  (PROG (|n| #0=#:G1649 |ans|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 9))
              (|spadConstant| $ 183))
             ('T
              (SEQ (LETT |ans|
                         (SPADCALL
                             (SPADCALL
                                 (SPADCALL |p| (|getShellEntry| $ 67))
                                 (|getShellEntry| $ 38))
                             (|getShellEntry| $ 184))
                         |UPOLYC-;elt;S2F;44|)
                   (LETT |n| (SPADCALL |p| (|getShellEntry| $ 11))
                         |UPOLYC-;elt;S2F;44|)
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL
                                       (LETT |p|
                                        (SPADCALL |p|
                                         (|getShellEntry| $ 69))
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
                                       (|getShellEntry| $ 185))
                                      (|getShellEntry| $ 186))
                                     (SPADCALL
                                      (SPADCALL
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 67))
                                       (|getShellEntry| $ 38))
                                      (|getShellEntry| $ 184))
                                     (|getShellEntry| $ 187))
                                    |UPOLYC-;elt;S2F;44|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT (COND
                           ((ZEROP |n|) |ans|)
                           ('T
                            (SPADCALL |ans|
                                (SPADCALL |f| |n|
                                    (|getShellEntry| $ 188))
                                (|getShellEntry| $ 186)))))))))))) 

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
                                       (|getShellEntry| $ 143))
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
  (SPADCALL |p| (|getShellEntry| $ 192))) 

(DEFUN |UPOLYC-;squareFreePart;2S;47| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 194))) 

(DEFUN |UPOLYC-;gcdPolynomial;3Sup;48| (|pp| |qq| $)
  (COND
    ((SPADCALL |pp| (|getShellEntry| $ 196))
     (SPADCALL |qq| (|getShellEntry| $ 197)))
    ((SPADCALL |qq| (|getShellEntry| $ 196))
     (SPADCALL |pp| (|getShellEntry| $ 197)))
    ('T
     (SPADCALL
         (SPADCALL
             (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 198))
                 (SPADCALL |qq| (|getShellEntry| $ 198))
                 (|getShellEntry| $ 142))
             (SPADCALL
                 (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 199))
                     (SPADCALL |qq| (|getShellEntry| $ 199))
                     (|getShellEntry| $ 200))
                 (|getShellEntry| $ 199))
             (|getShellEntry| $ 201))
         (|getShellEntry| $ 197))))) 

(DEFUN |UPOLYC-;squareFreePolynomial;SupF;49| (|pp| $)
  (SPADCALL |pp| (|getShellEntry| $ 204))) 

(DEFUN |UPOLYC-;elt;F2R;50| (|f| |r| $)
  (SPADCALL
      (SPADCALL (SPADCALL |f| (|getShellEntry| $ 161)) |r|
          (|getShellEntry| $ 37))
      (SPADCALL (SPADCALL |f| (|getShellEntry| $ 164)) |r|
          (|getShellEntry| $ 37))
      (|getShellEntry| $ 206))) 

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
              (SEQ (LETT |quot| (|spadConstant| $ 74)
                         |UPOLYC-;divide;2SR;52|)
                   (LETT |lc|
                         (SPADCALL
                             (SPADCALL |y| (|getShellEntry| $ 67))
                             (|getShellEntry| $ 209))
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
                                     (|getShellEntry| $ 67))
                                    (|getShellEntry| $ 211))
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
                                     (|getShellEntry| $ 63))
                                    (|getShellEntry| $ 79))
                                   |UPOLYC-;divide;2SR;52|)
                             (EXIT (LETT |x|
                                    (SPADCALL |x|
                                     (SPADCALL
                                      (SPADCALL |f| |n|
                                       (|getShellEntry| $ 63))
                                      |y| (|getShellEntry| $ 85))
                                     (|getShellEntry| $ 171))
                                    |UPOLYC-;divide;2SR;52|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT (CONS |quot| |x|))))))))) 

(DEFUN |UPOLYC-;integrate;2S;53| (|p| $)
  (PROG (|l| |d| |ans|)
    (RETURN
      (SEQ (LETT |ans| (|spadConstant| $ 74) |UPOLYC-;integrate;2S;53|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL |p| (|spadConstant| $ 74)
                             (|getShellEntry| $ 213)))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |p| (|getShellEntry| $ 67))
                           |UPOLYC-;integrate;2S;53|)
                     (LETT |d|
                           (+ 1 (SPADCALL |p| (|getShellEntry| $ 11)))
                           |UPOLYC-;integrate;2S;53|)
                     (LETT |ans|
                           (SPADCALL |ans|
                               (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |d|
                                     (|getShellEntry| $ 215))
                                    (|getShellEntry| $ 216))
                                   (SPADCALL |l| |d|
                                    (|getShellEntry| $ 63))
                                   (|getShellEntry| $ 217))
                               (|getShellEntry| $ 79))
                           |UPOLYC-;integrate;2S;53|)
                     (EXIT (LETT |p|
                                 (SPADCALL |p| (|getShellEntry| $ 69))
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
        (LETT $ (|newShell| 224) . #0#)
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
             (|setShellEntry| $ 95
                 (CONS (|dispatchFunction|
                           |UPOLYC-;solveLinearPolynomialEquation;LSupU;20|)
                       $))
             (|setShellEntry| $ 99
                 (CONS (|dispatchFunction|
                           |UPOLYC-;factorPolynomial;SupF;21|)
                       $))
             (|setShellEntry| $ 101
                 (CONS (|dispatchFunction|
                           |UPOLYC-;factorSquareFreePolynomial;SupF;22|)
                       $))
             (|setShellEntry| $ 120
                 (CONS (|dispatchFunction| |UPOLYC-;factor;SF;23|) $)))))
        (COND
          ((|testBitVector| |pv$| 6)
           (PROGN
             (|setShellEntry| $ 134
                 (CONS (|dispatchFunction| |UPOLYC-;init;S;27|) $))
             NIL
             (|setShellEntry| $ 139
                 (CONS (|dispatchFunction| |UPOLYC-;nextItem;SU;29|) $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (|setShellEntry| $ 141
                 (CONS (|dispatchFunction| |UPOLYC-;content;SSaosS;30|)
                       $))
             NIL
             (|setShellEntry| $ 146
                 (CONS (|dispatchFunction| |UPOLYC-;separate;2SR;32|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 5)
           (|setShellEntry| $ 151
               (CONS (|dispatchFunction|
                         |UPOLYC-;differentiate;SM2S;33|)
                     $)))
          ('T
           (PROGN
             (|setShellEntry| $ 151
                 (CONS (|dispatchFunction|
                           |UPOLYC-;differentiate;SM2S;35|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 4)
           (PROGN
             (|setShellEntry| $ 166
                 (CONS (|dispatchFunction| |UPOLYC-;elt;3F;39|) $))
             (|setShellEntry| $ 172
                 (CONS (|dispatchFunction|
                           |UPOLYC-;pseudoQuotient;3S;40|)
                       $))
             (|setShellEntry| $ 174
                 (CONS (|dispatchFunction|
                           |UPOLYC-;pseudoDivide;2SR;41|)
                       $))
             (|setShellEntry| $ 178
                 (CONS (|dispatchFunction| |UPOLYC-;composite;FSU;42|)
                       $))
             (|setShellEntry| $ 182
                 (CONS (|dispatchFunction| |UPOLYC-;composite;2SU;43|)
                       $))
             (|setShellEntry| $ 189
                 (CONS (|dispatchFunction| |UPOLYC-;elt;S2F;44|) $))
             (|setShellEntry| $ 190
                 (CONS (|dispatchFunction| |UPOLYC-;order;2SNni;45|) $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (PROGN
             (|setShellEntry| $ 193
                 (CONS (|dispatchFunction| |UPOLYC-;squareFree;SF;46|)
                       $))
             (|setShellEntry| $ 195
                 (CONS (|dispatchFunction|
                           |UPOLYC-;squareFreePart;2S;47|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|PolynomialFactorizationExplicit|))
           (PROGN
             (|setShellEntry| $ 202
                 (CONS (|dispatchFunction|
                           |UPOLYC-;gcdPolynomial;3Sup;48|)
                       $))
             (|setShellEntry| $ 205
                 (CONS (|dispatchFunction|
                           |UPOLYC-;squareFreePolynomial;SupF;49|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 2)
           (PROGN
             (|setShellEntry| $ 207
                 (CONS (|dispatchFunction| |UPOLYC-;elt;F2R;50|) $))
             (|setShellEntry| $ 208
                 (CONS (|dispatchFunction|
                           |UPOLYC-;euclideanSize;SNni;51|)
                       $))
             (|setShellEntry| $ 212
                 (CONS (|dispatchFunction| |UPOLYC-;divide;2SR;52|) $)))))
        (COND
          ((|testBitVector| |pv$| 1)
           (|setShellEntry| $ 218
               (CONS (|dispatchFunction| |UPOLYC-;integrate;2S;53|) $))))
        $)))) 

(MAKEPROP '|UnivariatePolynomialCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |zero?|) (|NonNegativeInteger|)
             (5 . |degree|) (10 . |zero?|) (|SingletonAsOrderedSet|)
             (15 . |create|) (|List| 13) |UPOLYC-;variables;SL;1|
             |UPOLYC-;degree;SSaosNni;2| (19 . |empty?|) (24 . |Zero|)
             (28 . |totalDegree|) |UPOLYC-;totalDegree;SLNni;3|
             (|List| 10) |UPOLYC-;degree;SLL;4| (33 . |rest|)
             (38 . |first|) (|List| 6) (43 . |first|) (48 . |eval|)
             (|List| $) |UPOLYC-;eval;SLLS;5| (55 . |elt|)
             |UPOLYC-;eval;SSaos2S;6| (|List| 7) (61 . |first|)
             (66 . |eval|) |UPOLYC-;eval;SLLS;7| (73 . |elt|)
             (79 . |coerce|) |UPOLYC-;eval;SSaosRS;8| (|Equation| 6)
             (|List| 40) (84 . |empty?|) (89 . |rest|) (94 . |first|)
             (99 . |lhs|) (|Union| 13 '"failed") (104 . |mainVariable|)
             (109 . |rhs|) (|Equation| $) (|List| 49)
             |UPOLYC-;eval;SLS;9| |UPOLYC-;mainVariable;SU;10|
             (114 . |minimumDegree|)
             |UPOLYC-;minimumDegree;SSaosNni;11|
             |UPOLYC-;minimumDegree;SLL;12| (119 . +) (|Mapping| 10 10)
             (125 . |mapExponents|) |UPOLYC-;monomial;SSaosNniS;13|
             (131 . |One|) (135 . |One|) (139 . |One|)
             (143 . |monomial|) |UPOLYC-;coerce;SaosS;14|
             (|SparseUnivariatePolynomial| 7) (149 . |Zero|)
             (153 . |leadingCoefficient|) (158 . |monomial|)
             (164 . |reductum|) (169 . |makeSUP|) (174 . +)
             |UPOLYC-;makeSUP;SSup;15| (180 . |zero?|) (185 . |Zero|)
             (189 . |leadingCoefficient|) (194 . |degree|)
             (199 . |reductum|) (204 . |unmakeSUP|) (209 . +)
             |UPOLYC-;unmakeSUP;SupS;16|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (215 . |monicDivide|) |UPOLYC-;karatsubaDivide;SNniR;17|
             |UPOLYC-;shiftRight;SNniS;18| (221 . *)
             |UPOLYC-;shiftLeft;SNniS;19|
             (|SparseUnivariatePolynomial| 6) (|List| 87)
             (|Union| 88 '"failed")
             (|PolynomialFactorizationByRecursionUnivariate| 7 6)
             (227 . |solveLinearPolynomialEquationByRecursion|)
             (|SparseUnivariatePolynomial| $) (|List| 92)
             (|Union| 93 '"failed")
             (233 . |solveLinearPolynomialEquation|) (|Factored| 87)
             (239 . |factorByRecursion|) (|Factored| 92)
             (244 . |factorPolynomial|)
             (249 . |factorSquareFreeByRecursion|)
             (254 . |factorSquareFreePolynomial|) (|Factored| $)
             (259 . |factor|) (|Factored| 7) (264 . |unit|)
             (|Union| '"nil" '"sqfr" '"irred" '"prime") (|Integer|)
             (|Record| (|:| |flg| 106) (|:| |fctr| 7) (|:| |xpnt| 107))
             (|List| 108) (269 . |factorList|)
             (|Record| (|:| |flg| 106) (|:| |fctr| 6) (|:| |xpnt| 107))
             (|List| 111) (|Factored| 6) (274 . |makeFR|)
             (280 . |factorPolynomial|) (|Mapping| 6 65)
             (|Factored| 65) (|FactoredFunctions2| 65 6) (285 . |map|)
             (291 . |factor|) (296 . |Zero|) (|Vector| 7) (300 . |new|)
             (306 . |minIndex|) (311 . |maxIndex|) (316 . -)
             (322 . |coefficient|) (328 . |qsetelt!|)
             |UPOLYC-;vectorise;SNniV;24| |UPOLYC-;retract;SR;25|
             (|Union| 7 '"failed") |UPOLYC-;retractIfCan;SU;26|
             (335 . |init|) (339 . |init|) (|Union| $ '"failed")
             (343 . |nextItem|) (348 . |One|) (352 . <)
             (358 . |nextItem|) (363 . |content|) (368 . |content|)
             (374 . |gcd|) (380 . |exquo|) (386 . =)
             (|Record| (|:| |primePart| $) (|:| |commonPart| $))
             (392 . |separate|) (398 . |Zero|) (402 . |Zero|) (406 . *)
             (|Mapping| 7 7) (412 . |differentiate|)
             (|PositiveInteger|) (419 . |One|) (423 . *)
             (429 . |differentiate|) |UPOLYC-;differentiate;SMS;36|
             |UPOLYC-;differentiate;2S;37| (436 . |differentiate|)
             |UPOLYC-;differentiate;SSaosS;38| (|Fraction| 6)
             (441 . |numer|) (|Fraction| $) (446 . |elt|)
             (452 . |denom|) (457 . /) (463 . |elt|) (469 . +)
             (475 . <) (481 . **) (487 . |pseudoRemainder|) (493 . -)
             (499 . |pseudoQuotient|)
             (|Record| (|:| |coef| 7) (|:| |quotient| $)
                 (|:| |remainder| $))
             (505 . |pseudoDivide|) (511 . |composite|) (517 . /)
             (|Union| 162 '"failed") (523 . |composite|)
             (529 . |ground?|) (534 . |pseudoDivide|) (540 . |exquo|)
             (546 . |composite|) (552 . |Zero|) (556 . |coerce|)
             (561 . **) (567 . *) (573 . +) (579 . **) (585 . |elt|)
             (591 . |order|) (|UnivariatePolynomialSquareFree| 7 6)
             (597 . |squareFree|) (602 . |squareFree|)
             (607 . |squareFreePart|) (612 . |squareFreePart|)
             (617 . |zero?|) (622 . |unitCanonical|) (627 . |content|)
             (632 . |primitivePart|) (637 . |subResultantGcd|)
             (643 . *) (649 . |gcdPolynomial|)
             (|UnivariatePolynomialSquareFree| 6 87)
             (655 . |squareFree|) (660 . |squareFreePolynomial|)
             (665 . /) (671 . |elt|) (677 . |euclideanSize|)
             (682 . |inv|) (687 . |false|) (691 . *) (697 . |divide|)
             (703 . ~=) (|Fraction| 107) (709 . |coerce|) (714 . |inv|)
             (719 . *) (725 . |integrate|) (|Symbol|) (|List| 219)
             (|Union| 107 '"failed") (|Union| 214 '"failed")
             (|OutputForm|))
          '#(|vectorise| 730 |variables| 736 |unmakeSUP| 741
             |totalDegree| 746 |squareFreePolynomial| 752
             |squareFreePart| 757 |squareFree| 762
             |solveLinearPolynomialEquation| 767 |shiftRight| 773
             |shiftLeft| 779 |separate| 785 |retractIfCan| 791
             |retract| 796 |pseudoQuotient| 801 |pseudoDivide| 807
             |order| 813 |nextItem| 819 |monomial| 824 |minimumDegree|
             831 |makeSUP| 843 |mainVariable| 848 |karatsubaDivide| 853
             |integrate| 859 |init| 864 |gcdPolynomial| 868
             |factorSquareFreePolynomial| 874 |factorPolynomial| 879
             |factor| 884 |eval| 889 |euclideanSize| 923 |elt| 928
             |divide| 946 |differentiate| 952 |degree| 976 |content|
             988 |composite| 994 |coerce| 1006)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 218
                                '(1 6 8 0 9 1 6 10 0 11 1 10 8 0 12 0
                                  13 0 14 1 15 8 0 18 0 10 0 19 1 6 10
                                  0 20 1 15 0 0 24 1 15 13 0 25 1 26 6
                                  0 27 3 6 0 0 13 0 28 2 6 0 0 0 31 1
                                  33 7 0 34 3 6 0 0 13 7 35 2 6 7 0 7
                                  37 1 6 0 7 38 1 41 8 0 42 1 41 0 0 43
                                  1 41 40 0 44 1 40 6 0 45 1 6 46 0 47
                                  1 40 6 0 48 1 6 10 0 53 2 10 0 0 0 56
                                  2 6 0 57 0 58 0 6 0 60 0 7 0 61 0 10
                                  0 62 2 6 0 7 10 63 0 65 0 66 1 6 7 0
                                  67 2 65 0 7 10 68 1 6 0 0 69 1 6 65 0
                                  70 2 65 0 0 0 71 1 65 8 0 73 0 6 0 74
                                  1 65 7 0 75 1 65 10 0 76 1 65 0 0 77
                                  1 6 0 65 78 2 6 0 0 0 79 2 6 81 0 0
                                  82 2 6 0 0 0 85 2 90 89 88 87 91 2 0
                                  94 93 92 95 1 90 96 87 97 1 0 98 92
                                  99 1 90 96 87 100 1 0 98 92 101 1 7
                                  102 0 103 1 104 7 0 105 1 104 109 0
                                  110 2 113 0 6 112 114 1 7 98 92 115 2
                                  118 113 116 117 119 1 0 102 0 120 0 7
                                  0 121 2 122 0 10 7 123 1 122 107 0
                                  124 1 122 107 0 125 2 107 0 0 0 126 2
                                  6 7 0 10 127 3 122 7 0 107 7 128 0 7
                                  0 133 0 0 0 134 1 7 135 0 136 0 87 0
                                  137 2 10 8 0 0 138 1 0 135 0 139 1 6
                                  7 0 140 2 0 0 0 13 141 2 6 0 0 0 142
                                  2 6 135 0 0 143 2 6 8 0 0 144 2 0 145
                                  0 0 146 0 87 0 147 0 107 0 148 2 7 0
                                  10 0 149 3 0 0 0 150 0 151 0 152 0
                                  153 2 6 0 7 0 154 3 6 0 0 150 0 155 1
                                  6 0 0 158 1 160 6 0 161 2 6 162 0 162
                                  163 1 160 6 0 164 2 160 0 0 0 165 2 0
                                  162 162 162 166 2 107 0 0 0 167 2 107
                                  8 0 0 168 2 7 0 0 10 169 2 6 0 0 0
                                  170 2 6 0 0 0 171 2 0 0 0 0 172 2 0
                                  173 0 0 174 2 6 135 0 0 175 2 160 0 6
                                  6 176 2 0 177 162 0 178 1 6 8 0 179 2
                                  6 173 0 0 180 2 6 135 0 7 181 2 0 135
                                  0 0 182 0 160 0 183 1 160 0 6 184 2
                                  160 0 0 107 185 2 160 0 0 0 186 2 160
                                  0 0 0 187 2 160 0 0 10 188 2 0 162 0
                                  162 189 2 0 10 0 0 190 1 191 113 6
                                  192 1 0 102 0 193 1 191 6 6 194 1 0 0
                                  0 195 1 87 8 0 196 1 87 0 0 197 1 87
                                  6 0 198 1 87 0 0 199 2 87 0 0 0 200 2
                                  87 0 6 0 201 2 0 92 92 92 202 1 203
                                  96 87 204 1 0 98 92 205 2 7 0 0 0 206
                                  2 0 7 162 7 207 1 0 10 0 208 1 7 0 0
                                  209 0 8 0 210 2 7 0 0 0 211 2 0 81 0
                                  0 212 2 6 8 0 0 213 1 214 0 107 215 1
                                  214 0 0 216 2 6 0 214 0 217 1 0 0 0
                                  218 2 0 122 0 10 129 1 0 15 0 16 1 0
                                  0 65 80 2 0 10 0 15 21 1 0 98 92 205
                                  1 0 0 0 195 1 0 102 0 193 2 0 94 93
                                  92 95 2 0 0 0 10 84 2 0 0 0 10 86 2 0
                                  145 0 0 146 1 0 131 0 132 1 0 7 0 130
                                  2 0 0 0 0 172 2 0 173 0 0 174 2 0 10
                                  0 0 190 1 0 135 0 139 3 0 0 0 13 10
                                  59 2 0 22 0 15 55 2 0 10 0 13 54 1 0
                                  65 0 72 1 0 46 0 52 2 0 81 0 10 83 1
                                  0 0 0 218 0 0 0 134 2 0 92 92 92 202
                                  1 0 98 92 101 1 0 98 92 99 1 0 102 0
                                  120 3 0 0 0 13 0 32 3 0 0 0 15 29 30
                                  3 0 0 0 15 33 36 3 0 0 0 13 7 39 2 0
                                  0 0 50 51 1 0 10 0 208 2 0 162 0 162
                                  189 2 0 7 162 7 207 2 0 162 162 162
                                  166 2 0 81 0 0 212 3 0 0 0 150 0 151
                                  2 0 0 0 150 156 1 0 0 0 157 2 0 0 0
                                  13 159 2 0 10 0 13 17 2 0 22 0 15 23
                                  2 0 0 0 13 141 2 0 135 0 0 182 2 0
                                  177 162 0 178 1 0 0 13 64)))))
          '|lookupComplete|)) 
