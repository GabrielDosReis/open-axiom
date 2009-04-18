
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |PSETCAT-;elements|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |PSETCAT-;variables1|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |PSETCAT-;variables2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |PSETCAT-;variables;SL;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |PSETCAT-;mainVariables;SL;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;mainVariable?;VarSetSB;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |PSETCAT-;collectUnder;SVarSetS;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |PSETCAT-;collectUpper;SVarSetS;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |PSETCAT-;collect;SVarSetS;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Shell|)
                |PSETCAT-;sort;SVarSetR;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;=;2SB;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;localInf?|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Boolean|)
                |PSETCAT-;localTriangular?|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;triangular?;SB;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;trivialIdeal?;SB;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;roughUnitIdeal?;SB;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;relativelyPrimeLeadingMonomials?|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;roughBase?;SB;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;roughSubIdeal?;2SB;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |PSETCAT-;roughEqualIdeals?;2SB;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |PSETCAT-;exactQuo|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |PSETCAT-;headRemainder;PSR;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Shell|) |%Pair|)
                |PSETCAT-;makeIrreducible!|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Shell|)
                |PSETCAT-;remainder;PSR;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%List|)
                |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%List|)
                |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)) 

(DEFUN |PSETCAT-;elements| (|ps| $)
  (PROG (|lp|)
    (RETURN
      (LETT |lp| (SPADCALL |ps| (|getShellEntry| $ 12))
            |PSETCAT-;elements|)))) 

(DEFUN |PSETCAT-;variables1| (|lp| $)
  (PROG (#0=#:G1560 |p| #1=#:G1561 |lvars|)
    (RETURN
      (SEQ (LETT |lvars|
                 (PROGN
                   (LETT #0# NIL |PSETCAT-;variables1|)
                   (SEQ (LETT |p| NIL |PSETCAT-;variables1|)
                        (LETT #1# |lp| |PSETCAT-;variables1|) G190
                        (COND
                          ((OR (ATOM #1#)
                               (PROGN
                                 (LETT |p| (CAR #1#)
                                       |PSETCAT-;variables1|)
                                 NIL))
                           (GO G191)))
                        (SEQ (EXIT (LETT #0#
                                    (CONS
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 14))
                                     #0#)
                                    |PSETCAT-;variables1|)))
                        (LETT #1# (CDR #1#) |PSETCAT-;variables1|)
                        (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                 |PSETCAT-;variables1|)
           (EXIT (SPADCALL (CONS #'|PSETCAT-;variables1!0| $)
                     (SPADCALL
                         (SPADCALL |lvars| (|getShellEntry| $ 18))
                         (|getShellEntry| $ 19))
                     (|getShellEntry| $ 21))))))) 

(DEFUN |PSETCAT-;variables1!0| (|#1| |#2| $)
  (SPADCALL |#2| |#1| (|getShellEntry| $ 16))) 

(DEFUN |PSETCAT-;variables2| (|lp| $)
  (PROG (#0=#:G1562 |p| #1=#:G1563 |lvars|)
    (RETURN
      (SEQ (LETT |lvars|
                 (PROGN
                   (LETT #0# NIL |PSETCAT-;variables2|)
                   (SEQ (LETT |p| NIL |PSETCAT-;variables2|)
                        (LETT #1# |lp| |PSETCAT-;variables2|) G190
                        (COND
                          ((OR (ATOM #1#)
                               (PROGN
                                 (LETT |p| (CAR #1#)
                                       |PSETCAT-;variables2|)
                                 NIL))
                           (GO G191)))
                        (SEQ (EXIT (LETT #0#
                                    (CONS
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 22))
                                     #0#)
                                    |PSETCAT-;variables2|)))
                        (LETT #1# (CDR #1#) |PSETCAT-;variables2|)
                        (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                 |PSETCAT-;variables2|)
           (EXIT (SPADCALL (CONS #'|PSETCAT-;variables2!0| $)
                     (SPADCALL |lvars| (|getShellEntry| $ 19))
                     (|getShellEntry| $ 21))))))) 

(DEFUN |PSETCAT-;variables2!0| (|#1| |#2| $)
  (SPADCALL |#2| |#1| (|getShellEntry| $ 16))) 

(DEFUN |PSETCAT-;variables;SL;4| (|ps| $)
  (|PSETCAT-;variables1| (|PSETCAT-;elements| |ps| $) $)) 

(DEFUN |PSETCAT-;mainVariables;SL;5| (|ps| $)
  (|PSETCAT-;variables2|
      (SPADCALL (ELT $ 24) (|PSETCAT-;elements| |ps| $)
          (|getShellEntry| $ 26))
      $)) 

(DEFUN |PSETCAT-;mainVariable?;VarSetSB;6| (|v| |ps| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 24) (|PSETCAT-;elements| |ps| $)
                     (|getShellEntry| $ 26))
                 |PSETCAT-;mainVariable?;VarSetSB;6|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((NULL |lp|) 'NIL)
                           ('T
                            (NOT (SPADCALL
                                     (SPADCALL (|SPADfirst| |lp|)
                                      (|getShellEntry| $ 22))
                                     |v| (|getShellEntry| $ 31))))))
                   (GO G191)))
                (SEQ (EXIT (LETT |lp| (CDR |lp|)
                                 |PSETCAT-;mainVariable?;VarSetSB;6|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (NOT (NULL |lp|))))))) 

(DEFUN |PSETCAT-;collectUnder;SVarSetS;7| (|ps| |v| $)
  (PROG (|p| |lp| |lq|)
    (RETURN
      (SEQ (LETT |lp| (|PSETCAT-;elements| |ps| $)
                 |PSETCAT-;collectUnder;SVarSetS;7|)
           (LETT |lq| NIL |PSETCAT-;collectUnder;SVarSetS;7|)
           (SEQ G190 (COND ((NULL (NOT (NULL |lp|))) (GO G191)))
                (SEQ (LETT |p| (|SPADfirst| |lp|)
                           |PSETCAT-;collectUnder;SVarSetS;7|)
                     (LETT |lp| (CDR |lp|)
                           |PSETCAT-;collectUnder;SVarSetS;7|)
                     (EXIT (COND
                             ((OR (SPADCALL |p| (|getShellEntry| $ 24))
                                  (SPADCALL
                                      (SPADCALL |p|
                                       (|getShellEntry| $ 22))
                                      |v| (|getShellEntry| $ 16)))
                              (LETT |lq| (CONS |p| |lq|)
                                    |PSETCAT-;collectUnder;SVarSetS;7|)))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |lq| (|getShellEntry| $ 35))))))) 

(DEFUN |PSETCAT-;collectUpper;SVarSetS;8| (|ps| |v| $)
  (PROG (|p| |lp| |lq|)
    (RETURN
      (SEQ (LETT |lp| (|PSETCAT-;elements| |ps| $)
                 |PSETCAT-;collectUpper;SVarSetS;8|)
           (LETT |lq| NIL |PSETCAT-;collectUpper;SVarSetS;8|)
           (SEQ G190 (COND ((NULL (NOT (NULL |lp|))) (GO G191)))
                (SEQ (LETT |p| (|SPADfirst| |lp|)
                           |PSETCAT-;collectUpper;SVarSetS;8|)
                     (LETT |lp| (CDR |lp|)
                           |PSETCAT-;collectUpper;SVarSetS;8|)
                     (EXIT (COND
                             ((NOT (SPADCALL |p|
                                    (|getShellEntry| $ 24)))
                              (COND
                                ((SPADCALL |v|
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 22))
                                     (|getShellEntry| $ 16))
                                 (LETT |lq| (CONS |p| |lq|)
                                       |PSETCAT-;collectUpper;SVarSetS;8|)))))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |lq| (|getShellEntry| $ 35))))))) 

(DEFUN |PSETCAT-;collect;SVarSetS;9| (|ps| |v| $)
  (PROG (|p| |lp| |lq|)
    (RETURN
      (SEQ (LETT |lp| (|PSETCAT-;elements| |ps| $)
                 |PSETCAT-;collect;SVarSetS;9|)
           (LETT |lq| NIL |PSETCAT-;collect;SVarSetS;9|)
           (SEQ G190 (COND ((NULL (NOT (NULL |lp|))) (GO G191)))
                (SEQ (LETT |p| (|SPADfirst| |lp|)
                           |PSETCAT-;collect;SVarSetS;9|)
                     (LETT |lp| (CDR |lp|)
                           |PSETCAT-;collect;SVarSetS;9|)
                     (EXIT (COND
                             ((NOT (SPADCALL |p|
                                    (|getShellEntry| $ 24)))
                              (COND
                                ((SPADCALL
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 22))
                                     |v| (|getShellEntry| $ 31))
                                 (LETT |lq| (CONS |p| |lq|)
                                       |PSETCAT-;collect;SVarSetS;9|)))))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |lq| (|getShellEntry| $ 35))))))) 

(DEFUN |PSETCAT-;sort;SVarSetR;10| (|ps| |v| $)
  (PROG (|p| |lp| |us| |vs| |ws|)
    (RETURN
      (SEQ (LETT |lp| (|PSETCAT-;elements| |ps| $)
                 |PSETCAT-;sort;SVarSetR;10|)
           (LETT |us| NIL |PSETCAT-;sort;SVarSetR;10|)
           (LETT |vs| NIL |PSETCAT-;sort;SVarSetR;10|)
           (LETT |ws| NIL |PSETCAT-;sort;SVarSetR;10|)
           (SEQ G190 (COND ((NULL (NOT (NULL |lp|))) (GO G191)))
                (SEQ (LETT |p| (|SPADfirst| |lp|)
                           |PSETCAT-;sort;SVarSetR;10|)
                     (LETT |lp| (CDR |lp|) |PSETCAT-;sort;SVarSetR;10|)
                     (EXIT (COND
                             ((OR (SPADCALL |p| (|getShellEntry| $ 24))
                                  (SPADCALL
                                      (SPADCALL |p|
                                       (|getShellEntry| $ 22))
                                      |v| (|getShellEntry| $ 16)))
                              (LETT |us| (CONS |p| |us|)
                                    |PSETCAT-;sort;SVarSetR;10|))
                             ((SPADCALL
                                  (SPADCALL |p| (|getShellEntry| $ 22))
                                  |v| (|getShellEntry| $ 31))
                              (LETT |vs| (CONS |p| |vs|)
                                    |PSETCAT-;sort;SVarSetR;10|))
                             ('T
                              (LETT |ws| (CONS |p| |ws|)
                                    |PSETCAT-;sort;SVarSetR;10|)))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (VECTOR (SPADCALL |us| (|getShellEntry| $ 35))
                         (SPADCALL |vs| (|getShellEntry| $ 35))
                         (SPADCALL |ws| (|getShellEntry| $ 35)))))))) 

(DEFUN |PSETCAT-;=;2SB;11| (|ps1| |ps2| $)
  (PROG (#0=#:G1564 #1=#:G1565 #2=#:G1566 |p| #3=#:G1567)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL
                   (PROGN
                     (LETT #0# NIL |PSETCAT-;=;2SB;11|)
                     (SEQ (LETT |p| NIL |PSETCAT-;=;2SB;11|)
                          (LETT #1# (|PSETCAT-;elements| |ps1| $)
                                |PSETCAT-;=;2SB;11|)
                          G190
                          (COND
                            ((OR (ATOM #1#)
                                 (PROGN
                                   (LETT |p| (CAR #1#)
                                    |PSETCAT-;=;2SB;11|)
                                   NIL))
                             (GO G191)))
                          (SEQ (EXIT (LETT #0# (CONS |p| #0#)
                                      |PSETCAT-;=;2SB;11|)))
                          (LETT #1# (CDR #1#) |PSETCAT-;=;2SB;11|)
                          (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                   (|getShellEntry| $ 42))
               (SPADCALL
                   (PROGN
                     (LETT #2# NIL |PSETCAT-;=;2SB;11|)
                     (SEQ (LETT |p| NIL |PSETCAT-;=;2SB;11|)
                          (LETT #3# (|PSETCAT-;elements| |ps2| $)
                                |PSETCAT-;=;2SB;11|)
                          G190
                          (COND
                            ((OR (ATOM #3#)
                                 (PROGN
                                   (LETT |p| (CAR #3#)
                                    |PSETCAT-;=;2SB;11|)
                                   NIL))
                             (GO G191)))
                          (SEQ (EXIT (LETT #2# (CONS |p| #2#)
                                      |PSETCAT-;=;2SB;11|)))
                          (LETT #3# (CDR #3#) |PSETCAT-;=;2SB;11|)
                          (GO G190) G191 (EXIT (NREVERSE0 #2#))))
                   (|getShellEntry| $ 42))
               (|getShellEntry| $ 43)))))) 

(DEFUN |PSETCAT-;localInf?| (|p| |q| $)
  (SPADCALL (SPADCALL |p| (|getShellEntry| $ 45))
      (SPADCALL |q| (|getShellEntry| $ 45)) (|getShellEntry| $ 46))) 

(DEFUN |PSETCAT-;localTriangular?| (|lp| $)
  (PROG (|q| |p|)
    (RETURN
      (SEQ (LETT |lp| (SPADCALL (ELT $ 47) |lp| (|getShellEntry| $ 26))
                 |PSETCAT-;localTriangular?|)
           (EXIT (COND
                   ((NULL |lp|) 'T)
                   ((SPADCALL (ELT $ 24) |lp| (|getShellEntry| $ 49))
                    'NIL)
                   ('T
                    (SEQ (LETT |lp|
                               (SPADCALL
                                   (CONS
                                    #'|PSETCAT-;localTriangular?!0| $)
                                   |lp| (|getShellEntry| $ 51))
                               |PSETCAT-;localTriangular?|)
                         (LETT |p| (|SPADfirst| |lp|)
                               |PSETCAT-;localTriangular?|)
                         (LETT |lp| (CDR |lp|)
                               |PSETCAT-;localTriangular?|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((NULL |lp|) 'NIL)
                                         ('T
                                          (SPADCALL
                                           (SPADCALL
                                            (LETT |q|
                                             (|SPADfirst| |lp|)
                                             |PSETCAT-;localTriangular?|)
                                            (|getShellEntry| $ 22))
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 22))
                                           (|getShellEntry| $ 16)))))
                                 (GO G191)))
                              (SEQ (LETT |p| |q|
                                    |PSETCAT-;localTriangular?|)
                                   (EXIT
                                    (LETT |lp| (CDR |lp|)
                                     |PSETCAT-;localTriangular?|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (NULL |lp|)))))))))) 

(DEFUN |PSETCAT-;localTriangular?!0| (|#1| |#2| $)
  (SPADCALL (SPADCALL |#2| (|getShellEntry| $ 22))
      (SPADCALL |#1| (|getShellEntry| $ 22)) (|getShellEntry| $ 16))) 

(DEFUN |PSETCAT-;triangular?;SB;14| (|ps| $)
  (|PSETCAT-;localTriangular?| (|PSETCAT-;elements| |ps| $) $)) 

(DEFUN |PSETCAT-;trivialIdeal?;SB;15| (|ps| $)
  (NULL (SPADCALL (ELT $ 47) (|PSETCAT-;elements| |ps| $)
            (|getShellEntry| $ 26)))) 

(DEFUN |PSETCAT-;roughUnitIdeal?;SB;16| (|ps| $)
  (SPADCALL (ELT $ 24)
      (SPADCALL (ELT $ 47) (|PSETCAT-;elements| |ps| $)
          (|getShellEntry| $ 26))
      (|getShellEntry| $ 49))) 

(DEFUN |PSETCAT-;relativelyPrimeLeadingMonomials?| (|p| |q| $)
  (PROG (|dp| |dq|)
    (RETURN
      (SEQ (LETT |dp| (SPADCALL |p| (|getShellEntry| $ 45))
                 |PSETCAT-;relativelyPrimeLeadingMonomials?|)
           (LETT |dq| (SPADCALL |q| (|getShellEntry| $ 45))
                 |PSETCAT-;relativelyPrimeLeadingMonomials?|)
           (EXIT (SPADCALL (SPADCALL |dp| |dq| (|getShellEntry| $ 55))
                     (SPADCALL |dp| |dq| (|getShellEntry| $ 56))
                     (|getShellEntry| $ 57))))))) 

(DEFUN |PSETCAT-;roughBase?;SB;18| (|ps| $)
  (PROG (|p| |lp| |rB?| |copylp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (ELT $ 47) (|PSETCAT-;elements| |ps| $)
                     (|getShellEntry| $ 26))
                 |PSETCAT-;roughBase?;SB;18|)
           (EXIT (COND
                   ((NULL |lp|) 'T)
                   ('T
                    (SEQ (LETT |rB?| 'T |PSETCAT-;roughBase?;SB;18|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((NULL |lp|) 'NIL)
                                         ('T |rB?|)))
                                 (GO G191)))
                              (SEQ (LETT |p| (|SPADfirst| |lp|)
                                    |PSETCAT-;roughBase?;SB;18|)
                                   (LETT |lp| (CDR |lp|)
                                    |PSETCAT-;roughBase?;SB;18|)
                                   (LETT |copylp| |lp|
                                    |PSETCAT-;roughBase?;SB;18|)
                                   (EXIT
                                    (SEQ G190
                                     (COND
                                       ((NULL
                                         (COND
                                           ((NULL |copylp|) 'NIL)
                                           ('T |rB?|)))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |rB?|
                                       (|PSETCAT-;relativelyPrimeLeadingMonomials?|
                                        |p| (|SPADfirst| |copylp|) $)
                                       |PSETCAT-;roughBase?;SB;18|)
                                      (EXIT
                                       (LETT |copylp| (CDR |copylp|)
                                        |PSETCAT-;roughBase?;SB;18|)))
                                     NIL (GO G190) G191 (EXIT NIL))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |rB?|))))))))) 

(DEFUN |PSETCAT-;roughSubIdeal?;2SB;19| (|ps1| |ps2| $)
  (PROG (|lp|)
    (RETURN
      (SEQ (LETT |lp|
                 (SPADCALL (|PSETCAT-;elements| |ps1| $) |ps2|
                     (|getShellEntry| $ 59))
                 |PSETCAT-;roughSubIdeal?;2SB;19|)
           (EXIT (NULL (SPADCALL (ELT $ 47) |lp|
                           (|getShellEntry| $ 26)))))))) 

(DEFUN |PSETCAT-;roughEqualIdeals?;2SB;20| (|ps1| |ps2| $)
  (COND
    ((SPADCALL |ps1| |ps2| (|getShellEntry| $ 61)) 'T)
    ((SPADCALL |ps1| |ps2| (|getShellEntry| $ 62))
     (SPADCALL |ps2| |ps1| (|getShellEntry| $ 62)))
    ('T 'NIL))) 

(DEFUN |PSETCAT-;exactQuo| (|r| |s| $)
  (PROG (#0=#:G1509)
    (RETURN
      (COND
        ((|HasCategory| (|getShellEntry| $ 7) '(|EuclideanDomain|))
         (SPADCALL |r| |s| (|getShellEntry| $ 64)))
        ('T
         (PROG2 (LETT #0# (SPADCALL |r| |s| (|getShellEntry| $ 66))
                      |PSETCAT-;exactQuo|)
                (QCDR #0#)
           (|check-union| (QEQCAR #0# 0) (|getShellEntry| $ 7) #0#))))))) 

(DEFUN |PSETCAT-;headRemainder;PSR;22| (|a| |ps| $)
  (PROG (|lp1| |p| |e| |g| |#G45| |#G46| |lca| |lcp| |r| |lp2|)
    (RETURN
      (SEQ (LETT |lp1|
                 (SPADCALL (ELT $ 47) (|PSETCAT-;elements| |ps| $)
                     (|getShellEntry| $ 26))
                 |PSETCAT-;headRemainder;PSR;22|)
           (EXIT (COND
                   ((NULL |lp1|) (CONS |a| (|spadConstant| $ 67)))
                   ((SPADCALL (ELT $ 24) |lp1| (|getShellEntry| $ 49))
                    (CONS (SPADCALL |a| (|getShellEntry| $ 68))
                          (|spadConstant| $ 67)))
                   ('T
                    (SEQ (LETT |r| (|spadConstant| $ 67)
                               |PSETCAT-;headRemainder;PSR;22|)
                         (LETT |lp1|
                               (SPADCALL
                                   (CONS
                                    (|function| |PSETCAT-;localInf?|)
                                    $)
                                   (REVERSE
                                    (|PSETCAT-;elements| |ps| $))
                                   (|getShellEntry| $ 51))
                               |PSETCAT-;headRemainder;PSR;22|)
                         (LETT |lp2| |lp1|
                               |PSETCAT-;headRemainder;PSR;22|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |a|
                                           (|getShellEntry| $ 47))
                                          'NIL)
                                         ('T (NOT (NULL |lp2|)))))
                                 (GO G191)))
                              (SEQ (LETT |p| (|SPADfirst| |lp2|)
                                    |PSETCAT-;headRemainder;PSR;22|)
                                   (LETT |e|
                                    (SPADCALL
                                     (SPADCALL |a|
                                      (|getShellEntry| $ 45))
                                     (SPADCALL |p|
                                      (|getShellEntry| $ 45))
                                     (|getShellEntry| $ 70))
                                    |PSETCAT-;headRemainder;PSR;22|)
                                   (EXIT
                                    (COND
                                      ((QEQCAR |e| 0)
                                       (SEQ
                                        (LETT |g|
                                         (SPADCALL
                                          (LETT |lca|
                                           (SPADCALL |a|
                                            (|getShellEntry| $ 71))
                                           |PSETCAT-;headRemainder;PSR;22|)
                                          (LETT |lcp|
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 71))
                                           |PSETCAT-;headRemainder;PSR;22|)
                                          (|getShellEntry| $ 72))
                                         |PSETCAT-;headRemainder;PSR;22|)
                                        (PROGN
                                          (LETT |#G45|
                                           (|PSETCAT-;exactQuo| |lca|
                                            |g| $)
                                           |PSETCAT-;headRemainder;PSR;22|)
                                          (LETT |#G46|
                                           (|PSETCAT-;exactQuo| |lcp|
                                            |g| $)
                                           |PSETCAT-;headRemainder;PSR;22|)
                                          (LETT |lca| |#G45|
                                           |PSETCAT-;headRemainder;PSR;22|)
                                          (LETT |lcp| |#G46|
                                           |PSETCAT-;headRemainder;PSR;22|))
                                        (LETT |a|
                                         (SPADCALL
                                          (SPADCALL |lcp|
                                           (SPADCALL |a|
                                            (|getShellEntry| $ 68))
                                           (|getShellEntry| $ 73))
                                          (SPADCALL
                                           (SPADCALL |lca| (QCDR |e|)
                                            (|getShellEntry| $ 74))
                                           (SPADCALL |p|
                                            (|getShellEntry| $ 68))
                                           (|getShellEntry| $ 75))
                                          (|getShellEntry| $ 76))
                                         |PSETCAT-;headRemainder;PSR;22|)
                                        (LETT |r|
                                         (SPADCALL |r| |lcp|
                                          (|getShellEntry| $ 77))
                                         |PSETCAT-;headRemainder;PSR;22|)
                                        (EXIT
                                         (LETT |lp2| |lp1|
                                          |PSETCAT-;headRemainder;PSR;22|))))
                                      ('T
                                       (LETT |lp2| (CDR |lp2|)
                                        |PSETCAT-;headRemainder;PSR;22|)))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (CONS |a| |r|)))))))))) 

(DEFUN |PSETCAT-;makeIrreducible!| (|frac| $)
  (PROG (|g|)
    (RETURN
      (SEQ (LETT |g|
                 (SPADCALL (QCDR |frac|) (QCAR |frac|)
                     (|getShellEntry| $ 80))
                 |PSETCAT-;makeIrreducible!|)
           (EXIT (COND
                   ((SPADCALL |g| (|getShellEntry| $ 81)) |frac|)
                   ('T
                    (SEQ (PROGN
                           (RPLACA |frac|
                                   (SPADCALL (QCAR |frac|) |g|
                                    (|getShellEntry| $ 82)))
                           (QCAR |frac|))
                         (PROGN
                           (RPLACD |frac|
                                   (|PSETCAT-;exactQuo| (QCDR |frac|)
                                    |g| $))
                           (QCDR |frac|))
                         (EXIT |frac|))))))))) 

(DEFUN |PSETCAT-;remainder;PSR;24| (|a| |ps| $)
  (PROG (|hRa| |r| |lca| |g| |b| |c|)
    (RETURN
      (SEQ (LETT |hRa|
                 (|PSETCAT-;makeIrreducible!|
                     (SPADCALL |a| |ps| (|getShellEntry| $ 83)) $)
                 |PSETCAT-;remainder;PSR;24|)
           (LETT |a| (QCAR |hRa|) |PSETCAT-;remainder;PSR;24|)
           (LETT |r| (QCDR |hRa|) |PSETCAT-;remainder;PSR;24|)
           (EXIT (COND
                   ((SPADCALL |a| (|getShellEntry| $ 47))
                    (VECTOR (|spadConstant| $ 67) |a| |r|))
                   ('T
                    (SEQ (LETT |b|
                               (SPADCALL (|spadConstant| $ 67)
                                   (SPADCALL |a|
                                    (|getShellEntry| $ 45))
                                   (|getShellEntry| $ 74))
                               |PSETCAT-;remainder;PSR;24|)
                         (LETT |c|
                               (SPADCALL |a| (|getShellEntry| $ 71))
                               |PSETCAT-;remainder;PSR;24|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT
                                        (SPADCALL
                                         (LETT |a|
                                          (SPADCALL |a|
                                           (|getShellEntry| $ 68))
                                          |PSETCAT-;remainder;PSR;24|)
                                         (|getShellEntry| $ 47))))
                                 (GO G191)))
                              (SEQ (LETT |hRa|
                                    (|PSETCAT-;makeIrreducible!|
                                     (SPADCALL |a| |ps|
                                      (|getShellEntry| $ 83))
                                     $)
                                    |PSETCAT-;remainder;PSR;24|)
                                   (LETT |a| (QCAR |hRa|)
                                    |PSETCAT-;remainder;PSR;24|)
                                   (LETT |r|
                                    (SPADCALL |r| (QCDR |hRa|)
                                     (|getShellEntry| $ 77))
                                    |PSETCAT-;remainder;PSR;24|)
                                   (LETT |g|
                                    (SPADCALL |c|
                                     (LETT |lca|
                                      (SPADCALL |a|
                                       (|getShellEntry| $ 71))
                                      |PSETCAT-;remainder;PSR;24|)
                                     (|getShellEntry| $ 72))
                                    |PSETCAT-;remainder;PSR;24|)
                                   (LETT |b|
                                    (SPADCALL
                                     (SPADCALL
                                      (SPADCALL (QCDR |hRa|)
                                       (|PSETCAT-;exactQuo| |c| |g| $)
                                       (|getShellEntry| $ 77))
                                      |b| (|getShellEntry| $ 73))
                                     (SPADCALL
                                      (|PSETCAT-;exactQuo| |lca| |g| $)
                                      (SPADCALL |a|
                                       (|getShellEntry| $ 45))
                                      (|getShellEntry| $ 74))
                                     (|getShellEntry| $ 84))
                                    |PSETCAT-;remainder;PSR;24|)
                                   (EXIT
                                    (LETT |c| |g|
                                     |PSETCAT-;remainder;PSR;24|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (VECTOR |c| |b| |r|)))))))))) 

(DEFUN |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25| (|ps| |cs| $)
  (PROG (|p| |rs|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |cs| (|getShellEntry| $ 87)) |ps|)
             ((SPADCALL |cs| (|getShellEntry| $ 88))
              (LIST (|spadConstant| $ 89)))
             ('T
              (SEQ (LETT |ps|
                         (SPADCALL (ELT $ 47) |ps|
                             (|getShellEntry| $ 26))
                         |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                   (EXIT (COND
                           ((NULL |ps|) |ps|)
                           ((SPADCALL (ELT $ 24) |ps|
                                (|getShellEntry| $ 49))
                            (LIST (|spadConstant| $ 90)))
                           ('T
                            (SEQ (LETT |rs| NIL
                                       |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                                 (SEQ G190
                                      (COND
                                        ((NULL (NOT (NULL |ps|)))
                                         (GO G191)))
                                      (SEQ
                                       (LETT |p| (|SPADfirst| |ps|)
                                        |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                                       (LETT |ps| (CDR |ps|)
                                        |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                                       (LETT |p|
                                        (QCAR
                                         (SPADCALL |p| |cs|
                                          (|getShellEntry| $ 83)))
                                        |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                                       (EXIT
                                        (COND
                                          ((NOT
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 47)))
                                           (COND
                                             ((SPADCALL |p|
                                               (|getShellEntry| $ 24))
                                              (SEQ
                                               (LETT |ps| NIL
                                                |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                                               (EXIT
                                                (LETT |rs|
                                                 (LIST
                                                  (|spadConstant| $ 90))
                                                 |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|))))
                                             ('T
                                              (SEQ
                                               (SPADCALL |p|
                                                (|getShellEntry| $ 91))
                                               (EXIT
                                                (LETT |rs|
                                                 (CONS |p| |rs|)
                                                 |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)))))))))
                                      NIL (GO G190) G191 (EXIT NIL))
                                 (EXIT (SPADCALL |rs|
                                        (|getShellEntry| $ 92)))))))))))))) 

(DEFUN |PSETCAT-;rewriteIdealWithRemainder;LSL;26| (|ps| |cs| $)
  (PROG (|p| |rs|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |cs| (|getShellEntry| $ 87)) |ps|)
             ((SPADCALL |cs| (|getShellEntry| $ 88))
              (LIST (|spadConstant| $ 89)))
             ('T
              (SEQ (LETT |ps|
                         (SPADCALL (ELT $ 47) |ps|
                             (|getShellEntry| $ 26))
                         |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                   (EXIT (COND
                           ((NULL |ps|) |ps|)
                           ((SPADCALL (ELT $ 24) |ps|
                                (|getShellEntry| $ 49))
                            (LIST (|spadConstant| $ 90)))
                           ('T
                            (SEQ (LETT |rs| NIL
                                       |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                                 (SEQ G190
                                      (COND
                                        ((NULL (NOT (NULL |ps|)))
                                         (GO G191)))
                                      (SEQ
                                       (LETT |p| (|SPADfirst| |ps|)
                                        |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                                       (LETT |ps| (CDR |ps|)
                                        |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                                       (LETT |p|
                                        (QVELT
                                         (SPADCALL |p| |cs|
                                          (|getShellEntry| $ 94))
                                         1)
                                        |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                                       (EXIT
                                        (COND
                                          ((NOT
                                            (SPADCALL |p|
                                             (|getShellEntry| $ 47)))
                                           (COND
                                             ((SPADCALL |p|
                                               (|getShellEntry| $ 24))
                                              (SEQ
                                               (LETT |ps| NIL
                                                |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                                               (EXIT
                                                (LETT |rs|
                                                 (LIST
                                                  (|spadConstant| $ 90))
                                                 |PSETCAT-;rewriteIdealWithRemainder;LSL;26|))))
                                             ('T
                                              (LETT |rs|
                                               (CONS
                                                (SPADCALL |p|
                                                 (|getShellEntry| $ 95))
                                                |rs|)
                                               |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)))))))
                                      NIL (GO G190) G191 (EXIT NIL))
                                 (EXIT (SPADCALL |rs|
                                        (|getShellEntry| $ 92)))))))))))))) 

(DEFUN |PolynomialSetCategory&| (|#1| |#2| |#3| |#4| |#5|)
  (PROG (|dv$1| |dv$2| |dv$3| |dv$4| |dv$5| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|)
              . #0=(|PolynomialSetCategory&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$3| (|devaluate| |#3|) . #0#)
        (LETT |dv$4| (|devaluate| |#4|) . #0#)
        (LETT |dv$5| (|devaluate| |#5|) . #0#)
        (LETT |dv$|
              (LIST '|PolynomialSetCategory&| |dv$1| |dv$2| |dv$3|
                    |dv$4| |dv$5|) . #0#)
        (LETT $ (|newShell| 97) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#2| '(|IntegralDomain|)))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (|setShellEntry| $ 8 |#3|)
        (|setShellEntry| $ 9 |#4|)
        (|setShellEntry| $ 10 |#5|)
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 54
                 (CONS (|dispatchFunction|
                           |PSETCAT-;roughUnitIdeal?;SB;16|)
                       $))
             (|setShellEntry| $ 58
                 (CONS (|dispatchFunction| |PSETCAT-;roughBase?;SB;18|)
                       $))
             (|setShellEntry| $ 60
                 (CONS (|dispatchFunction|
                           |PSETCAT-;roughSubIdeal?;2SB;19|)
                       $))
             (|setShellEntry| $ 63
                 (CONS (|dispatchFunction|
                           |PSETCAT-;roughEqualIdeals?;2SB;20|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|GcdDomain|))
           (COND
             ((|HasCategory| |#4| '(|ConvertibleTo| (|Symbol|)))
              (PROGN
                (|setShellEntry| $ 79
                    (CONS (|dispatchFunction|
                              |PSETCAT-;headRemainder;PSR;22|)
                          $))
                (|setShellEntry| $ 86
                    (CONS (|dispatchFunction|
                              |PSETCAT-;remainder;PSR;24|)
                          $))
                (|setShellEntry| $ 93
                    (CONS (|dispatchFunction|
                              |PSETCAT-;rewriteIdealWithHeadRemainder;LSL;25|)
                          $))
                (|setShellEntry| $ 96
                    (CONS (|dispatchFunction|
                              |PSETCAT-;rewriteIdealWithRemainder;LSL;26|)
                          $)))))))
        $)))) 

(MAKEPROP '|PolynomialSetCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|local| |#3|) (|local| |#4|) (|local| |#5|) (|List| 10)
             (0 . |members|) (|List| 9) (5 . |variables|) (|Boolean|)
             (10 . <) (|List| $) (16 . |concat|)
             (21 . |removeDuplicates|) (|Mapping| 15 9 9) (26 . |sort|)
             (32 . |mvar|) |PSETCAT-;variables;SL;4| (37 . |ground?|)
             (|Mapping| 15 10) (42 . |remove|)
             |PSETCAT-;mainVariables;SL;5| (48 . |empty?|)
             (53 . |false|) (57 . |first|) (62 . =) (68 . |rest|)
             |PSETCAT-;mainVariable?;VarSetSB;6| (73 . |cons|)
             (79 . |construct|) |PSETCAT-;collectUnder;SVarSetS;7|
             |PSETCAT-;collectUpper;SVarSetS;8|
             |PSETCAT-;collect;SVarSetS;9|
             (|Record| (|:| |under| $) (|:| |floor| $) (|:| |upper| $))
             |PSETCAT-;sort;SVarSetR;10| (|Set| 10) (84 . |brace|)
             (89 . =) |PSETCAT-;=;2SB;11| (95 . |degree|) (100 . <)
             (106 . |zero?|) (111 . |true|) (115 . |any?|)
             (|Mapping| 15 10 10) (121 . |sort|)
             |PSETCAT-;triangular?;SB;14|
             |PSETCAT-;trivialIdeal?;SB;15| (127 . |roughUnitIdeal?|)
             (132 . |sup|) (138 . +) (144 . =) (150 . |roughBase?|)
             (155 . |rewriteIdealWithRemainder|)
             (161 . |roughSubIdeal?|) (167 . =)
             (173 . |roughSubIdeal?|) (179 . |roughEqualIdeals?|)
             (185 . |quo|) (|Union| $ '"failed") (191 . |exquo|)
             (197 . |One|) (201 . |reductum|) (206 . |reverse|)
             (211 . |subtractIfCan|) (217 . |leadingCoefficient|)
             (222 . |gcd|) (228 . *) (234 . |monomial|) (240 . *)
             (246 . -) (252 . *)
             (|Record| (|:| |num| 10) (|:| |den| 7))
             (258 . |headRemainder|) (264 . |gcd|) (270 . |one?|)
             (275 . |exactQuotient!|) (281 . |headRemainder|) (287 . +)
             (|Record| (|:| |rnum| 7) (|:| |polnum| 10) (|:| |den| 7))
             (293 . |remainder|) (299 . |trivialIdeal?|)
             (304 . |roughUnitIdeal?|) (309 . |Zero|) (313 . |One|)
             (317 . |primitivePart!|) (322 . |removeDuplicates|)
             (327 . |rewriteIdealWithHeadRemainder|)
             (333 . |remainder|) (339 . |unitCanonical|)
             (344 . |rewriteIdealWithRemainder|))
          '#(|variables| 350 |trivialIdeal?| 355 |triangular?| 360
             |sort| 365 |roughUnitIdeal?| 371 |roughSubIdeal?| 376
             |roughEqualIdeals?| 382 |roughBase?| 388
             |rewriteIdealWithRemainder| 393
             |rewriteIdealWithHeadRemainder| 399 |remainder| 405
             |mainVariables| 411 |mainVariable?| 416 |headRemainder|
             422 |collectUpper| 428 |collectUnder| 434 |collect| 440 =
             446)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 96
                                '(1 6 11 0 12 1 10 13 0 14 2 9 15 0 0
                                  16 1 13 0 17 18 1 13 0 0 19 2 13 0 20
                                  0 21 1 10 9 0 22 1 10 15 0 24 2 11 0
                                  25 0 26 1 11 15 0 28 0 15 0 29 1 11
                                  10 0 30 2 9 15 0 0 31 1 11 0 0 32 2
                                  11 0 10 0 34 1 6 0 11 35 1 41 0 11 42
                                  2 41 15 0 0 43 1 10 8 0 45 2 8 15 0 0
                                  46 1 10 15 0 47 0 15 0 48 2 11 15 25
                                  0 49 2 11 0 50 0 51 1 0 15 0 54 2 8 0
                                  0 0 55 2 8 0 0 0 56 2 8 15 0 0 57 1 0
                                  15 0 58 2 6 11 11 0 59 2 0 15 0 0 60
                                  2 6 15 0 0 61 2 6 15 0 0 62 2 0 15 0
                                  0 63 2 7 0 0 0 64 2 7 65 0 0 66 0 7 0
                                  67 1 10 0 0 68 1 11 0 0 69 2 8 65 0 0
                                  70 1 10 7 0 71 2 7 0 0 0 72 2 10 0 7
                                  0 73 2 10 0 7 8 74 2 10 0 0 0 75 2 10
                                  0 0 0 76 2 7 0 0 0 77 2 0 78 10 0 79
                                  2 10 7 7 0 80 1 7 15 0 81 2 10 0 0 7
                                  82 2 6 78 10 0 83 2 10 0 0 0 84 2 0
                                  85 10 0 86 1 6 15 0 87 1 6 15 0 88 0
                                  10 0 89 0 10 0 90 1 10 0 0 91 1 11 0
                                  0 92 2 0 11 11 0 93 2 6 85 10 0 94 1
                                  10 0 0 95 2 0 11 11 0 96 1 0 13 0 23
                                  1 0 15 0 53 1 0 15 0 52 2 0 39 0 9 40
                                  1 0 15 0 54 2 0 15 0 0 60 2 0 15 0 0
                                  63 1 0 15 0 58 2 0 11 11 0 96 2 0 11
                                  11 0 93 2 0 85 10 0 86 1 0 13 0 27 2
                                  0 15 9 0 33 2 0 78 10 0 79 2 0 0 0 9
                                  37 2 0 0 0 9 36 2 0 0 0 9 38 2 0 15 0
                                  0 44)))))
          '|lookupComplete|)) 
