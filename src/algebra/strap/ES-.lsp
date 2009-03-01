
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|) |ES-;box;2S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ES-;paren;2S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ES-;belong?;BoB;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|) |ES-;listk|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |ES-;tower;SL;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|) |ES-;allk|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |ES-;operators;SL;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |ES-;height;SNni;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ES-;freeOf?;SSB;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ES-;distribute;2S;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|) |ES-;box;LS;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |ES-;paren;LS;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ES-;freeOf?;2SB;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ES-;kernel;Bo2S;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ES-;elt;Bo2S;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ES-;elt;Bo3S;16|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ES-;elt;Bo4S;17|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| |%Thing| |%Thing| |%Thing|
                        |%Shell|)
                    |%Thing|)
                |ES-;elt;Bo5S;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ES-;eval;SSMS;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ES-;eval;SBoMS;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ES-;eval;SSMS;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ES-;eval;SBoMS;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ES-;subst;SES;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |ES-;eval;SLLS;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |ES-;eval;SLLS;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%Thing|)
                |ES-;eval;SLLS;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ES-;map;MKS;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ES-;operator;2Bo;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |ES-;mainKernel;SU;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ES-;allKernels|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |ES-;kernel;BoLS;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |ES-;okkernel|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |ES-;elt;BoLS;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ES-;retract;SK;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |ES-;retractIfCan;SU;35|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ES-;is?;SSB;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ES-;is?;SBoB;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Thing|)
                |ES-;unwrap|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ES-;distribute;3S;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |ES-;eval;SLS;40|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |ES-;subst;SLS;41|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Pair|) |ES-;mkKerLists|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ES-;even?;SB;43|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ES-;odd?;SB;44|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ES-;intpred?|)) 

(DEFUN |ES-;box;2S;1| (|x| $)
  (SPADCALL (LIST |x|) (|getShellEntry| $ 16))) 

(DEFUN |ES-;paren;2S;2| (|x| $)
  (SPADCALL (LIST |x|) (|getShellEntry| $ 18))) 

(DEFUN |ES-;belong?;BoB;3| (|op| $)
  (COND
    ((SPADCALL |op| (|getShellEntry| $ 13) (|getShellEntry| $ 21)) 'T)
    ('T (SPADCALL |op| (|getShellEntry| $ 14) (|getShellEntry| $ 21))))) 

(DEFUN |ES-;listk| (|f| $)
  (SPADCALL (|ES-;allKernels| |f| $) (|getShellEntry| $ 27))) 

(DEFUN |ES-;tower;SL;5| (|f| $)
  (SPADCALL (|ES-;listk| |f| $) (|getShellEntry| $ 28))) 

(DEFUN |ES-;allk| (|l| $)
  (PROG (#0=#:G1579 |f| #1=#:G1580)
    (RETURN
      (SEQ (SPADCALL (ELT $ 33)
               (PROGN
                 (LETT #0# NIL |ES-;allk|)
                 (SEQ (LETT |f| NIL |ES-;allk|)
                      (LETT #1# |l| |ES-;allk|) G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |f| (CAR #1#) |ES-;allk|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS (|ES-;allKernels| |f| $)
                                        #0#)
                                       |ES-;allk|)))
                      (LETT #1# (CDR #1#) |ES-;allk|) (GO G190) G191
                      (EXIT (NREVERSE0 #0#))))
               (SPADCALL NIL (|getShellEntry| $ 32))
               (|getShellEntry| $ 36)))))) 

(DEFUN |ES-;operators;SL;7| (|f| $)
  (PROG (#0=#:G1581 |k| #1=#:G1582)
    (RETURN
      (SEQ (PROGN
             (LETT #0# NIL |ES-;operators;SL;7|)
             (SEQ (LETT |k| NIL |ES-;operators;SL;7|)
                  (LETT #1# (|ES-;listk| |f| $) |ES-;operators;SL;7|)
                  G190
                  (COND
                    ((OR (ATOM #1#)
                         (PROGN
                           (LETT |k| (CAR #1#) |ES-;operators;SL;7|)
                           NIL))
                     (GO G191)))
                  (SEQ (EXIT (LETT #0#
                                   (CONS
                                    (SPADCALL |k|
                                     (|getShellEntry| $ 37))
                                    #0#)
                                   |ES-;operators;SL;7|)))
                  (LETT #1# (CDR #1#) |ES-;operators;SL;7|) (GO G190)
                  G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |ES-;height;SNni;8| (|f| $)
  (PROG (#0=#:G1583 |k| #1=#:G1584)
    (RETURN
      (SEQ (SPADCALL (ELT $ 44)
               (PROGN
                 (LETT #0# NIL |ES-;height;SNni;8|)
                 (SEQ (LETT |k| NIL |ES-;height;SNni;8|)
                      (LETT #1# (SPADCALL |f| (|getShellEntry| $ 40))
                            |ES-;height;SNni;8|)
                      G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |k| (CAR #1#) |ES-;height;SNni;8|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (SPADCALL |k|
                                         (|getShellEntry| $ 42))
                                        #0#)
                                       |ES-;height;SNni;8|)))
                      (LETT #1# (CDR #1#) |ES-;height;SNni;8|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               0 (|getShellEntry| $ 47)))))) 

(DEFUN |ES-;freeOf?;SSB;9| (|x| |s| $)
  (PROG (#0=#:G1585 |k| #1=#:G1586)
    (RETURN
      (SEQ (NOT (SPADCALL |s|
                    (PROGN
                      (LETT #0# NIL |ES-;freeOf?;SSB;9|)
                      (SEQ (LETT |k| NIL |ES-;freeOf?;SSB;9|)
                           (LETT #1# (|ES-;listk| |x| $)
                                 |ES-;freeOf?;SSB;9|)
                           G190
                           (COND
                             ((OR (ATOM #1#)
                                  (PROGN
                                    (LETT |k| (CAR #1#)
                                     |ES-;freeOf?;SSB;9|)
                                    NIL))
                              (GO G191)))
                           (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (SPADCALL |k|
                                         (|getShellEntry| $ 49))
                                        #0#)
                                       |ES-;freeOf?;SSB;9|)))
                           (LETT #1# (CDR #1#) |ES-;freeOf?;SSB;9|)
                           (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                    (|getShellEntry| $ 51))))))) 

(DEFUN |ES-;distribute;2S;10| (|x| $)
  (PROG (#0=#:G1587 |k| #1=#:G1588)
    (RETURN
      (SEQ (|ES-;unwrap|
               (PROGN
                 (LETT #0# NIL |ES-;distribute;2S;10|)
                 (SEQ (LETT |k| NIL |ES-;distribute;2S;10|)
                      (LETT #1# (|ES-;listk| |x| $)
                            |ES-;distribute;2S;10|)
                      G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |k| (CAR #1#)
                                     |ES-;distribute;2S;10|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (COND
                                   ((SPADCALL |k|
                                     (|getShellEntry| $ 13)
                                     (|getShellEntry| $ 53))
                                    (LETT #0# (CONS |k| #0#)
                                     |ES-;distribute;2S;10|)))))
                      (LETT #1# (CDR #1#) |ES-;distribute;2S;10|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               |x| $))))) 

(DEFUN |ES-;box;LS;11| (|l| $)
  (SPADCALL (|getShellEntry| $ 14) |l| (|getShellEntry| $ 55))) 

(DEFUN |ES-;paren;LS;12| (|l| $)
  (SPADCALL (|getShellEntry| $ 13) |l| (|getShellEntry| $ 55))) 

(DEFUN |ES-;freeOf?;2SB;13| (|x| |k| $)
  (NOT (SPADCALL (SPADCALL |k| (|getShellEntry| $ 58))
           (|ES-;listk| |x| $) (|getShellEntry| $ 59)))) 

(DEFUN |ES-;kernel;Bo2S;14| (|op| |arg| $)
  (SPADCALL |op| (LIST |arg|) (|getShellEntry| $ 61))) 

(DEFUN |ES-;elt;Bo2S;15| (|op| |x| $)
  (SPADCALL |op| (LIST |x|) (|getShellEntry| $ 55))) 

(DEFUN |ES-;elt;Bo3S;16| (|op| |x| |y| $)
  (SPADCALL |op| (LIST |x| |y|) (|getShellEntry| $ 55))) 

(DEFUN |ES-;elt;Bo4S;17| (|op| |x| |y| |z| $)
  (SPADCALL |op| (LIST |x| |y| |z|) (|getShellEntry| $ 55))) 

(DEFUN |ES-;elt;Bo5S;18| (|op| |x| |y| |z| |t| $)
  (SPADCALL |op| (LIST |x| |y| |z| |t|) (|getShellEntry| $ 55))) 

(DEFUN |ES-;eval;SSMS;19| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|) (LIST |f|) (|getShellEntry| $ 69))) 

(DEFUN |ES-;eval;SBoMS;20| (|x| |s| |f| $)
  (SPADCALL |x| (LIST (SPADCALL |s| (|getShellEntry| $ 71))) (LIST |f|)
      (|getShellEntry| $ 69))) 

(DEFUN |ES-;eval;SSMS;21| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|)
            (LIST (CONS #'|ES-;eval;SSMS;21!0| (VECTOR |f| $)))
            (|getShellEntry| $ 69))) 

(DEFUN |ES-;eval;SSMS;21!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 74))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SBoMS;22| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|)
      (LIST (CONS #'|ES-;eval;SBoMS;22!0| (VECTOR |f| $)))
      (|getShellEntry| $ 77))) 

(DEFUN |ES-;eval;SBoMS;22!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 74))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;subst;SES;23| (|x| |e| $)
  (SPADCALL |x| (LIST |e|) (|getShellEntry| $ 81))) 

(DEFUN |ES-;eval;SLLS;24| (|x| |ls| |lf| $)
  (PROG (#0=#:G1589 |f| #1=#:G1590)
    (RETURN
      (SEQ (SPADCALL |x| |ls|
               (PROGN
                 (LETT #0# NIL |ES-;eval;SLLS;24|)
                 (SEQ (LETT |f| NIL |ES-;eval;SLLS;24|)
                      (LETT #1# |lf| |ES-;eval;SLLS;24|) G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |f| (CAR #1#) |ES-;eval;SLLS;24|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (CONS #'|ES-;eval;SLLS;24!0|
                                         (VECTOR |f| $))
                                        #0#)
                                       |ES-;eval;SLLS;24|)))
                      (LETT #1# (CDR #1#) |ES-;eval;SLLS;24|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 77)))))) 

(DEFUN |ES-;eval;SLLS;24!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 74))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SLLS;25| (|x| |ls| |lf| $)
  (PROG (#0=#:G1591 |f| #1=#:G1592)
    (RETURN
      (SEQ (SPADCALL |x| |ls|
               (PROGN
                 (LETT #0# NIL |ES-;eval;SLLS;25|)
                 (SEQ (LETT |f| NIL |ES-;eval;SLLS;25|)
                      (LETT #1# |lf| |ES-;eval;SLLS;25|) G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |f| (CAR #1#) |ES-;eval;SLLS;25|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (CONS #'|ES-;eval;SLLS;25!0|
                                         (VECTOR |f| $))
                                        #0#)
                                       |ES-;eval;SLLS;25|)))
                      (LETT #1# (CDR #1#) |ES-;eval;SLLS;25|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 69)))))) 

(DEFUN |ES-;eval;SLLS;25!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 74))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SLLS;26| (|x| |ls| |lf| $)
  (PROG (#0=#:G1593 |s| #1=#:G1594)
    (RETURN
      (SEQ (SPADCALL |x|
               (PROGN
                 (LETT #0# NIL |ES-;eval;SLLS;26|)
                 (SEQ (LETT |s| NIL |ES-;eval;SLLS;26|)
                      (LETT #1# |ls| |ES-;eval;SLLS;26|) G190
                      (COND
                        ((OR (ATOM #1#)
                             (PROGN
                               (LETT |s| (CAR #1#) |ES-;eval;SLLS;26|)
                               NIL))
                         (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (SPADCALL |s|
                                         (|getShellEntry| $ 71))
                                        #0#)
                                       |ES-;eval;SLLS;26|)))
                      (LETT #1# (CDR #1#) |ES-;eval;SLLS;26|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               |lf| (|getShellEntry| $ 69)))))) 

(DEFUN |ES-;map;MKS;27| (|fn| |k| $)
  (PROG (#0=#:G1595 |x| #1=#:G1596 |l|)
    (RETURN
      (SEQ (COND
             ((SPADCALL
                  (LETT |l|
                        (PROGN
                          (LETT #0# NIL |ES-;map;MKS;27|)
                          (SEQ (LETT |x| NIL |ES-;map;MKS;27|)
                               (LETT #1#
                                     (SPADCALL |k|
                                      (|getShellEntry| $ 87))
                                     |ES-;map;MKS;27|)
                               G190
                               (COND
                                 ((OR (ATOM #1#)
                                      (PROGN
                                        (LETT |x| (CAR #1#)
                                         |ES-;map;MKS;27|)
                                        NIL))
                                  (GO G191)))
                               (SEQ (EXIT
                                     (LETT #0#
                                      (CONS (SPADCALL |x| |fn|) #0#)
                                      |ES-;map;MKS;27|)))
                               (LETT #1# (CDR #1#) |ES-;map;MKS;27|)
                               (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                        |ES-;map;MKS;27|)
                  (SPADCALL |k| (|getShellEntry| $ 87))
                  (|getShellEntry| $ 88))
              (SPADCALL |k| (|getShellEntry| $ 89)))
             ('T
              (SPADCALL (SPADCALL |k| (|getShellEntry| $ 37)) |l|
                  (|getShellEntry| $ 55)))))))) 

(DEFUN |ES-;operator;2Bo;28| (|op| $)
  (COND
    ((SPADCALL |op| (SPADCALL "%paren" (|getShellEntry| $ 9))
         (|getShellEntry| $ 91))
     (|getShellEntry| $ 13))
    ((SPADCALL |op| (SPADCALL "%box" (|getShellEntry| $ 9))
         (|getShellEntry| $ 91))
     (|getShellEntry| $ 14))
    ('T (|error| "Unknown operator")))) 

(DEFUN |ES-;mainKernel;SU;29| (|x| $)
  (PROG (|l| |kk| #0=#:G1597 |n| |k|)
    (RETURN
      (SEQ (COND
             ((NULL (LETT |l| (SPADCALL |x| (|getShellEntry| $ 40))
                          |ES-;mainKernel;SU;29|))
              (CONS 1 "failed"))
             ('T
              (SEQ (LETT |n|
                         (SPADCALL
                             (LETT |k| (|SPADfirst| |l|)
                                   |ES-;mainKernel;SU;29|)
                             (|getShellEntry| $ 42))
                         |ES-;mainKernel;SU;29|)
                   (SEQ (LETT |kk| NIL |ES-;mainKernel;SU;29|)
                        (LETT #0# (CDR |l|) |ES-;mainKernel;SU;29|)
                        G190
                        (COND
                          ((OR (ATOM #0#)
                               (PROGN
                                 (LETT |kk| (CAR #0#)
                                       |ES-;mainKernel;SU;29|)
                                 NIL))
                           (GO G191)))
                        (SEQ (EXIT (COND
                                     ((< |n|
                                       (SPADCALL |kk|
                                        (|getShellEntry| $ 42)))
                                      (SEQ
                                       (LETT |n|
                                        (SPADCALL |kk|
                                         (|getShellEntry| $ 42))
                                        |ES-;mainKernel;SU;29|)
                                       (EXIT
                                        (LETT |k| |kk|
                                         |ES-;mainKernel;SU;29|)))))))
                        (LETT #0# (CDR #0#) |ES-;mainKernel;SU;29|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT (CONS 0 |k|))))))))) 

(DEFUN |ES-;allKernels| (|f| $)
  (PROG (|l| |k| #0=#:G1598 |u| |s0| |n| |arg| |t| |s|)
    (RETURN
      (SEQ (LETT |s|
                 (SPADCALL
                     (LETT |l| (SPADCALL |f| (|getShellEntry| $ 40))
                           |ES-;allKernels|)
                     (|getShellEntry| $ 32))
                 |ES-;allKernels|)
           (SEQ (LETT |k| NIL |ES-;allKernels|)
                (LETT #0# |l| |ES-;allKernels|) G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |k| (CAR #0#) |ES-;allKernels|)
                         NIL))
                   (GO G191)))
                (SEQ (LETT |t|
                           (SEQ (LETT |u|
                                      (SPADCALL
                                       (SPADCALL |k|
                                        (|getShellEntry| $ 37))
                                       "%dummyVar"
                                       (|getShellEntry| $ 101))
                                      |ES-;allKernels|)
                                (EXIT (COND
                                        ((QEQCAR |u| 0)
                                         (SEQ
                                          (LETT |arg|
                                           (SPADCALL |k|
                                            (|getShellEntry| $ 87))
                                           |ES-;allKernels|)
                                          (LETT |s0|
                                           (SPADCALL
                                            (SPADCALL
                                             (SPADCALL |arg|
                                              (|getShellEntry| $ 102))
                                             (|getShellEntry| $ 58))
                                            (|ES-;allKernels|
                                             (|SPADfirst| |arg|) $)
                                            (|getShellEntry| $ 103))
                                           |ES-;allKernels|)
                                          (LETT |arg| (CDR (CDR |arg|))
                                           |ES-;allKernels|)
                                          (LETT |n| (QCDR |u|)
                                           |ES-;allKernels|)
                                          (COND
                                            ((< 1 |n|)
                                             (LETT |arg| (CDR |arg|)
                                              |ES-;allKernels|)))
                                          (EXIT
                                           (SPADCALL |s0|
                                            (|ES-;allk| |arg| $)
                                            (|getShellEntry| $ 33)))))
                                        ('T
                                         (|ES-;allk|
                                          (SPADCALL |k|
                                           (|getShellEntry| $ 87))
                                          $)))))
                           |ES-;allKernels|)
                     (EXIT (LETT |s|
                                 (SPADCALL |s| |t|
                                     (|getShellEntry| $ 33))
                                 |ES-;allKernels|)))
                (LETT #0# (CDR #0#) |ES-;allKernels|) (GO G190) G191
                (EXIT NIL))
           (EXIT |s|))))) 

(DEFUN |ES-;kernel;BoLS;31| (|op| |args| $)
  (COND
    ((NULL (SPADCALL |op| (|getShellEntry| $ 108)))
     (|error| "Unknown operator"))
    ('T (|ES-;okkernel| |op| |args| $)))) 

(DEFUN |ES-;okkernel| (|op| |l| $)
  (PROG (#0=#:G1599 |f| #1=#:G1600)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL |op| |l|
                   (+ 1
                      (SPADCALL (ELT $ 44)
                          (PROGN
                            (LETT #0# NIL |ES-;okkernel|)
                            (SEQ (LETT |f| NIL |ES-;okkernel|)
                                 (LETT #1# |l| |ES-;okkernel|) G190
                                 (COND
                                   ((OR (ATOM #1#)
                                     (PROGN
                                       (LETT |f| (CAR #1#)
                                        |ES-;okkernel|)
                                       NIL))
                                    (GO G191)))
                                 (SEQ (EXIT
                                       (LETT #0#
                                        (CONS
                                         (SPADCALL |f|
                                          (|getShellEntry| $ 110))
                                         #0#)
                                        |ES-;okkernel|)))
                                 (LETT #1# (CDR #1#) |ES-;okkernel|)
                                 (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                          0 (|getShellEntry| $ 47)))
                   (|getShellEntry| $ 112))
               (|getShellEntry| $ 89)))))) 

(DEFUN |ES-;elt;BoLS;33| (|op| |args| $)
  (PROG (|u| #0=#:G1522 |v|)
    (RETURN
      (SEQ (EXIT (COND
                   ((NULL (SPADCALL |op| (|getShellEntry| $ 108)))
                    (|error| "Unknown operator"))
                   ('T
                    (SEQ (SEQ (LETT |u|
                                    (SPADCALL |op|
                                     (|getShellEntry| $ 114))
                                    |ES-;elt;BoLS;33|)
                              (EXIT (COND
                                      ((QEQCAR |u| 0)
                                       (COND
                                         ((SPADCALL (LENGTH |args|)
                                           (QCDR |u|)
                                           (|getShellEntry| $ 116))
                                          (PROGN
                                            (LETT #0#
                                             (|error|
                                              "Wrong number of arguments")
                                             |ES-;elt;BoLS;33|)
                                            (GO #0#))))))))
                         (LETT |v|
                               (SPADCALL |op| |args|
                                   (|getShellEntry| $ 119))
                               |ES-;elt;BoLS;33|)
                         (EXIT (COND
                                 ((QEQCAR |v| 0) (QCDR |v|))
                                 ('T (|ES-;okkernel| |op| |args| $))))))))
           #0# (EXIT #0#))))) 

(DEFUN |ES-;retract;SK;34| (|f| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 121))
                 |ES-;retract;SK;34|)
           (EXIT (COND
                   ((OR (QEQCAR |k| 1)
                        (SPADCALL
                            (SPADCALL (QCDR |k|)
                                (|getShellEntry| $ 89))
                            |f| (|getShellEntry| $ 122)))
                    (|error| "not a kernel"))
                   ('T (QCDR |k|)))))))) 

(DEFUN |ES-;retractIfCan;SU;35| (|f| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 121))
                 |ES-;retractIfCan;SU;35|)
           (EXIT (COND
                   ((OR (QEQCAR |k| 1)
                        (SPADCALL
                            (SPADCALL (QCDR |k|)
                                (|getShellEntry| $ 89))
                            |f| (|getShellEntry| $ 122)))
                    (CONS 1 "failed"))
                   ('T |k|))))))) 

(DEFUN |ES-;is?;SSB;36| (|f| |s| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 125))
                 |ES-;is?;SSB;36|)
           (EXIT (COND
                   ((QEQCAR |k| 1) 'NIL)
                   ('T
                    (SPADCALL (QCDR |k|) |s| (|getShellEntry| $ 127))))))))) 

(DEFUN |ES-;is?;SBoB;37| (|f| |op| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 125))
                 |ES-;is?;SBoB;37|)
           (EXIT (COND
                   ((QEQCAR |k| 1) 'NIL)
                   ('T
                    (SPADCALL (QCDR |k|) |op| (|getShellEntry| $ 53))))))))) 

(DEFUN |ES-;unwrap| (|l| |x| $)
  (PROG (|k| #0=#:G1601)
    (RETURN
      (SEQ (SEQ (LETT |k| NIL |ES-;unwrap|)
                (LETT #0# (NREVERSE |l|) |ES-;unwrap|) G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN (LETT |k| (CAR #0#) |ES-;unwrap|) NIL))
                   (GO G191)))
                (SEQ (EXIT (LETT |x|
                                 (SPADCALL |x| |k|
                                     (|SPADfirst|
                                      (SPADCALL |k|
                                       (|getShellEntry| $ 87)))
                                     (|getShellEntry| $ 131))
                                 |ES-;unwrap|)))
                (LETT #0# (CDR #0#) |ES-;unwrap|) (GO G190) G191
                (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |ES-;distribute;3S;39| (|x| |y| $)
  (PROG (|ky| #0=#:G1602 |k| #1=#:G1603)
    (RETURN
      (SEQ (LETT |ky| (SPADCALL |y| (|getShellEntry| $ 58))
                 |ES-;distribute;3S;39|)
           (EXIT (|ES-;unwrap|
                     (PROGN
                       (LETT #0# NIL |ES-;distribute;3S;39|)
                       (SEQ (LETT |k| NIL |ES-;distribute;3S;39|)
                            (LETT #1# (|ES-;listk| |x| $)
                                  |ES-;distribute;3S;39|)
                            G190
                            (COND
                              ((OR (ATOM #1#)
                                   (PROGN
                                     (LETT |k| (CAR #1#)
                                      |ES-;distribute;3S;39|)
                                     NIL))
                               (GO G191)))
                            (SEQ (EXIT (COND
                                         ((COND
                                            ((SPADCALL |k|
                                              (SPADCALL "%paren"
                                               (|getShellEntry| $ 9))
                                              (|getShellEntry| $ 127))
                                             (SPADCALL |ky|
                                              (|ES-;listk|
                                               (SPADCALL |k|
                                                (|getShellEntry| $ 89))
                                               $)
                                              (|getShellEntry| $ 59)))
                                            ('T 'NIL))
                                          (LETT #0# (CONS |k| #0#)
                                           |ES-;distribute;3S;39|)))))
                            (LETT #1# (CDR #1#) |ES-;distribute;3S;39|)
                            (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                     |x| $)))))) 

(DEFUN |ES-;eval;SLS;40| (|f| |leq| $)
  (PROG (|rec|)
    (RETURN
      (SEQ (LETT |rec| (|ES-;mkKerLists| |leq| $) |ES-;eval;SLS;40|)
           (EXIT (SPADCALL |f| (QCAR |rec|) (QCDR |rec|)
                     (|getShellEntry| $ 133))))))) 

(DEFUN |ES-;subst;SLS;41| (|f| |leq| $)
  (PROG (|rec|)
    (RETURN
      (SEQ (LETT |rec| (|ES-;mkKerLists| |leq| $) |ES-;subst;SLS;41|)
           (EXIT (SPADCALL |f| (QCAR |rec|) (QCDR |rec|)
                     (|getShellEntry| $ 135))))))) 

(DEFUN |ES-;mkKerLists| (|leq| $)
  (PROG (|eq| #0=#:G1604 |k| |lk| |lv|)
    (RETURN
      (SEQ (LETT |lk| NIL |ES-;mkKerLists|)
           (LETT |lv| NIL |ES-;mkKerLists|)
           (SEQ (LETT |eq| NIL |ES-;mkKerLists|)
                (LETT #0# |leq| |ES-;mkKerLists|) G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |eq| (CAR #0#) |ES-;mkKerLists|)
                         NIL))
                   (GO G191)))
                (SEQ (LETT |k|
                           (SPADCALL
                               (SPADCALL |eq| (|getShellEntry| $ 140))
                               (|getShellEntry| $ 125))
                           |ES-;mkKerLists|)
                     (EXIT (COND
                             ((QEQCAR |k| 1)
                              (|error| "left hand side must be a single kernel"))
                             ((NULL (SPADCALL (QCDR |k|) |lk|
                                     (|getShellEntry| $ 59)))
                              (SEQ (LETT |lk| (CONS (QCDR |k|) |lk|)
                                    |ES-;mkKerLists|)
                                   (EXIT
                                    (LETT |lv|
                                     (CONS
                                      (SPADCALL |eq|
                                       (|getShellEntry| $ 142))
                                      |lv|)
                                     |ES-;mkKerLists|)))))))
                (LETT #0# (CDR #0#) |ES-;mkKerLists|) (GO G190) G191
                (EXIT NIL))
           (EXIT (CONS |lk| |lv|)))))) 

(DEFUN |ES-;even?;SB;43| (|x| $) (|ES-;intpred?| |x| (ELT $ 144) $)) 

(DEFUN |ES-;odd?;SB;44| (|x| $) (|ES-;intpred?| |x| (ELT $ 146) $)) 

(DEFUN |ES-;intpred?| (|x| |pred?| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |x| (|getShellEntry| $ 149))
                 |ES-;intpred?|)
           (EXIT (COND
                   ((QEQCAR |u| 0) (SPADCALL (QCDR |u|) |pred?|))
                   ('T 'NIL))))))) 

(DEFUN |ExpressionSpace&| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|ExpressionSpace&|))
        (LETT |dv$| (LIST '|ExpressionSpace&| |dv$1|) . #0#)
        (LETT $ (|newShell| 150) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#1|
                                '(|RetractableTo| (|Integer|)))
                            (|HasCategory| |#1| '(|Ring|)))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 13
            (SPADCALL (SPADCALL "%paren" (|getShellEntry| $ 9))
                (|getShellEntry| $ 12)))
        (|setShellEntry| $ 14
            (SPADCALL (SPADCALL "%box" (|getShellEntry| $ 9))
                (|getShellEntry| $ 12)))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 145
                 (CONS (|dispatchFunction| |ES-;even?;SB;43|) $))
             (|setShellEntry| $ 147
                 (CONS (|dispatchFunction| |ES-;odd?;SB;44|) $)))))
        $)))) 

(MAKEPROP '|ExpressionSpace&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|String|)
             (|Symbol|) (0 . |coerce|) (|BasicOperator|)
             (|CommonOperators|) (5 . |operator|) '|oppren| '|opbox|
             (|List| $) (10 . |box|) |ES-;box;2S;1| (15 . |paren|)
             |ES-;paren;2S;2| (|Boolean|) (20 . =) (26 . |true|)
             |ES-;belong?;BoB;3| (|Kernel| 6) (|List| 24) (|Set| 24)
             (30 . |parts|) (35 . |sort!|) (|Kernel| $) (|List| 29)
             |ES-;tower;SL;5| (40 . |brace|) (45 . |union|)
             (|Mapping| 26 26 26) (|List| 26) (51 . |reduce|)
             (58 . |operator|) (|List| 10) |ES-;operators;SL;7|
             (63 . |kernels|) (|NonNegativeInteger|) (68 . |height|)
             (73 . |Zero|) (77 . |max|) (|Mapping| 41 41 41)
             (|List| 41) (83 . |reduce|) |ES-;height;SNni;8|
             (90 . |name|) (|List| 8) (95 . |member?|)
             |ES-;freeOf?;SSB;9| (101 . |is?|) |ES-;distribute;2S;10|
             (107 . |elt|) |ES-;box;LS;11| |ES-;paren;LS;12|
             (113 . |retract|) (118 . |member?|) |ES-;freeOf?;2SB;13|
             (124 . |kernel|) |ES-;kernel;Bo2S;14| |ES-;elt;Bo2S;15|
             |ES-;elt;Bo3S;16| |ES-;elt;Bo4S;17| |ES-;elt;Bo5S;18|
             (|Mapping| $ 15) (|List| 67) (130 . |eval|)
             |ES-;eval;SSMS;19| (137 . |name|) |ES-;eval;SBoMS;20|
             (|List| 6) (142 . |first|) (|Mapping| $ $)
             |ES-;eval;SSMS;21| (147 . |eval|) |ES-;eval;SBoMS;22|
             (|Equation| $) (|List| 79) (154 . |subst|)
             |ES-;subst;SES;23| (|List| 75) |ES-;eval;SLLS;24|
             |ES-;eval;SLLS;25| |ES-;eval;SLLS;26| (160 . |argument|)
             (165 . =) (171 . |coerce|) |ES-;map;MKS;27| (176 . |is?|)
             |ES-;operator;2Bo;28| (182 . |empty?|) (187 . |first|)
             (192 . |rest|) (197 . <) (|Union| 29 '"failed")
             |ES-;mainKernel;SU;29| (|None|) (|Union| 99 '"failed")
             (203 . |property|) (209 . |second|) (214 . |remove!|)
             (220 . |rest|) (225 . |One|) (|Integer|) (229 . |One|)
             (233 . |belong?|) |ES-;kernel;BoLS;31| (238 . |height|)
             (243 . +) (249 . |kernel|) (|Union| 41 '"failed")
             (256 . |arity|) (261 . |#|) (266 . ~=)
             (|Union| 6 '"failed") (|BasicOperatorFunctions1| 6)
             (272 . |evaluate|) |ES-;elt;BoLS;33| (278 . |mainKernel|)
             (283 . ~=) |ES-;retract;SK;34| |ES-;retractIfCan;SU;35|
             (289 . |retractIfCan|) (294 . |false|) (298 . |is?|)
             |ES-;is?;SSB;36| |ES-;is?;SBoB;37| (304 . |reverse!|)
             (309 . |eval|) |ES-;distribute;3S;39| (316 . |eval|)
             |ES-;eval;SLS;40| (323 . |subst|) |ES-;subst;SLS;41|
             (330 . |empty|) (334 . |empty|) (|Equation| 6)
             (338 . |lhs|) (343 . |concat|) (349 . |rhs|)
             (354 . |concat|) (360 . |even?|) (365 . |even?|)
             (370 . |odd?|) (375 . |odd?|) (|Union| 106 '"failed")
             (380 . |retractIfCan|))
          '#(|tower| 385 |subst| 390 |retractIfCan| 402 |retract| 407
             |paren| 412 |operators| 422 |operator| 427 |odd?| 432
             |map| 437 |mainKernel| 443 |kernel| 448 |is?| 460 |height|
             472 |freeOf?| 477 |even?| 489 |eval| 494 |elt| 549
             |distribute| 585 |box| 596 |belong?| 606)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 149
                                '(1 8 0 7 9 1 11 10 8 12 1 6 0 15 16 1
                                  6 0 15 18 2 10 20 0 0 21 0 20 0 22 1
                                  26 25 0 27 1 25 0 0 28 1 26 0 25 32 2
                                  26 0 0 0 33 3 35 26 34 0 26 36 1 24
                                  10 0 37 1 6 30 0 40 1 24 41 0 42 0 41
                                  0 43 2 41 0 0 0 44 3 46 41 45 0 41 47
                                  1 24 8 0 49 2 50 20 8 0 51 2 24 20 0
                                  10 53 2 6 0 10 15 55 1 6 29 0 58 2 25
                                  20 24 0 59 2 6 0 10 15 61 3 6 0 0 50
                                  68 69 1 10 8 0 71 1 73 6 0 74 3 6 0 0
                                  38 68 77 2 6 0 0 80 81 1 24 73 0 87 2
                                  73 20 0 0 88 1 6 0 29 89 2 10 20 0 8
                                  91 1 25 20 0 93 1 25 24 0 94 1 25 0 0
                                  95 2 41 20 0 0 96 2 10 100 0 7 101 1
                                  73 6 0 102 2 26 0 24 0 103 1 73 0 0
                                  104 0 41 0 105 0 106 0 107 1 6 20 10
                                  108 1 6 41 0 110 2 41 0 0 0 111 3 24
                                  0 10 73 41 112 1 10 113 0 114 1 73 41
                                  0 115 2 41 20 0 0 116 2 118 117 10 73
                                  119 1 6 97 0 121 2 6 20 0 0 122 1 6
                                  97 0 125 0 20 0 126 2 24 20 0 8 127 1
                                  25 0 0 130 3 6 0 0 29 0 131 3 6 0 0
                                  30 15 133 3 6 0 0 30 15 135 0 25 0
                                  137 0 73 0 138 1 139 6 0 140 2 25 0
                                  24 0 141 1 139 6 0 142 2 73 0 6 0 143
                                  1 106 20 0 144 1 0 20 0 145 1 106 20
                                  0 146 1 0 20 0 147 1 6 148 0 149 1 0
                                  30 0 31 2 0 0 0 80 136 2 0 0 0 79 82
                                  1 0 97 0 124 1 0 29 0 123 1 0 0 0 19
                                  1 0 0 15 57 1 0 38 0 39 1 0 10 10 92
                                  1 0 20 0 147 2 0 0 75 29 90 1 0 97 0
                                  98 2 0 0 10 15 109 2 0 0 10 0 62 2 0
                                  20 0 8 128 2 0 20 0 10 129 1 0 41 0
                                  48 2 0 20 0 8 52 2 0 20 0 0 60 1 0 20
                                  0 145 3 0 0 0 10 75 78 3 0 0 0 38 68
                                  86 3 0 0 0 10 67 72 3 0 0 0 38 83 84
                                  3 0 0 0 8 67 70 3 0 0 0 8 75 76 3 0 0
                                  0 50 83 85 2 0 0 0 80 134 2 0 0 10 15
                                  120 5 0 0 10 0 0 0 0 66 3 0 0 10 0 0
                                  64 4 0 0 10 0 0 0 65 2 0 0 10 0 63 2
                                  0 0 0 0 132 1 0 0 0 54 1 0 0 15 56 1
                                  0 0 0 17 1 0 20 10 23)))))
          '|lookupComplete|)) 
