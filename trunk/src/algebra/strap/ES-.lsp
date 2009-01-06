
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
  (SPADCALL (|ES-;allKernels| |f| $) (|getShellEntry| $ 26))) 

(DEFUN |ES-;tower;SL;5| (|f| $)
  (SPADCALL (|ES-;listk| |f| $) (|getShellEntry| $ 27))) 

(DEFUN |ES-;allk| (|l| $)
  (PROG (#0=#:G1579 |f| #1=#:G1580)
    (RETURN
      (SEQ (SPADCALL (ELT $ 32)
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
               (SPADCALL NIL (|getShellEntry| $ 31))
               (|getShellEntry| $ 35)))))) 

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
                                     (|getShellEntry| $ 36))
                                    #0#)
                                   |ES-;operators;SL;7|)))
                  (LETT #1# (CDR #1#) |ES-;operators;SL;7|) (GO G190)
                  G191 (EXIT (NREVERSE0 #0#)))))))) 

(DEFUN |ES-;height;SNni;8| (|f| $)
  (PROG (#0=#:G1583 |k| #1=#:G1584)
    (RETURN
      (SEQ (SPADCALL (ELT $ 42)
               (PROGN
                 (LETT #0# NIL |ES-;height;SNni;8|)
                 (SEQ (LETT |k| NIL |ES-;height;SNni;8|)
                      (LETT #1# (SPADCALL |f| (|getShellEntry| $ 39))
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
                                         (|getShellEntry| $ 41))
                                        #0#)
                                       |ES-;height;SNni;8|)))
                      (LETT #1# (CDR #1#) |ES-;height;SNni;8|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               0 (|getShellEntry| $ 45)))))) 

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
                                         (|getShellEntry| $ 47))
                                        #0#)
                                       |ES-;freeOf?;SSB;9|)))
                           (LETT #1# (CDR #1#) |ES-;freeOf?;SSB;9|)
                           (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                    (|getShellEntry| $ 49))))))) 

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
                                     (|getShellEntry| $ 51))
                                    (LETT #0# (CONS |k| #0#)
                                     |ES-;distribute;2S;10|)))))
                      (LETT #1# (CDR #1#) |ES-;distribute;2S;10|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               |x| $))))) 

(DEFUN |ES-;box;LS;11| (|l| $)
  (SPADCALL (|getShellEntry| $ 14) |l| (|getShellEntry| $ 53))) 

(DEFUN |ES-;paren;LS;12| (|l| $)
  (SPADCALL (|getShellEntry| $ 13) |l| (|getShellEntry| $ 53))) 

(DEFUN |ES-;freeOf?;2SB;13| (|x| |k| $)
  (NOT (SPADCALL (SPADCALL |k| (|getShellEntry| $ 56))
           (|ES-;listk| |x| $) (|getShellEntry| $ 57)))) 

(DEFUN |ES-;kernel;Bo2S;14| (|op| |arg| $)
  (SPADCALL |op| (LIST |arg|) (|getShellEntry| $ 59))) 

(DEFUN |ES-;elt;Bo2S;15| (|op| |x| $)
  (SPADCALL |op| (LIST |x|) (|getShellEntry| $ 53))) 

(DEFUN |ES-;elt;Bo3S;16| (|op| |x| |y| $)
  (SPADCALL |op| (LIST |x| |y|) (|getShellEntry| $ 53))) 

(DEFUN |ES-;elt;Bo4S;17| (|op| |x| |y| |z| $)
  (SPADCALL |op| (LIST |x| |y| |z|) (|getShellEntry| $ 53))) 

(DEFUN |ES-;elt;Bo5S;18| (|op| |x| |y| |z| |t| $)
  (SPADCALL |op| (LIST |x| |y| |z| |t|) (|getShellEntry| $ 53))) 

(DEFUN |ES-;eval;SSMS;19| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|) (LIST |f|) (|getShellEntry| $ 67))) 

(DEFUN |ES-;eval;SBoMS;20| (|x| |s| |f| $)
  (SPADCALL |x| (LIST (SPADCALL |s| (|getShellEntry| $ 69))) (LIST |f|)
      (|getShellEntry| $ 67))) 

(DEFUN |ES-;eval;SSMS;21| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|)
            (LIST (CONS #'|ES-;eval;SSMS;21!0| (VECTOR |f| $)))
            (|getShellEntry| $ 67))) 

(DEFUN |ES-;eval;SSMS;21!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 72))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SBoMS;22| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|)
      (LIST (CONS #'|ES-;eval;SBoMS;22!0| (VECTOR |f| $)))
      (|getShellEntry| $ 75))) 

(DEFUN |ES-;eval;SBoMS;22!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 72))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;subst;SES;23| (|x| |e| $)
  (SPADCALL |x| (LIST |e|) (|getShellEntry| $ 79))) 

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
               (|getShellEntry| $ 75)))))) 

(DEFUN |ES-;eval;SLLS;24!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 72))
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
               (|getShellEntry| $ 67)))))) 

(DEFUN |ES-;eval;SLLS;25!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 72))
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
                                         (|getShellEntry| $ 69))
                                        #0#)
                                       |ES-;eval;SLLS;26|)))
                      (LETT #1# (CDR #1#) |ES-;eval;SLLS;26|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               |lf| (|getShellEntry| $ 67)))))) 

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
                                      (|getShellEntry| $ 85))
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
                  (SPADCALL |k| (|getShellEntry| $ 85))
                  (|getShellEntry| $ 86))
              (SPADCALL |k| (|getShellEntry| $ 87)))
             ('T
              (SPADCALL (SPADCALL |k| (|getShellEntry| $ 36)) |l|
                  (|getShellEntry| $ 53)))))))) 

(DEFUN |ES-;operator;2Bo;28| (|op| $)
  (COND
    ((SPADCALL |op| (SPADCALL "%paren" (|getShellEntry| $ 9))
         (|getShellEntry| $ 89))
     (|getShellEntry| $ 13))
    ((SPADCALL |op| (SPADCALL "%box" (|getShellEntry| $ 9))
         (|getShellEntry| $ 89))
     (|getShellEntry| $ 14))
    ('T (|error| "Unknown operator")))) 

(DEFUN |ES-;mainKernel;SU;29| (|x| $)
  (PROG (|l| |kk| #0=#:G1597 |n| |k|)
    (RETURN
      (SEQ (COND
             ((NULL (LETT |l| (SPADCALL |x| (|getShellEntry| $ 39))
                          |ES-;mainKernel;SU;29|))
              (CONS 1 "failed"))
             ('T
              (SEQ (LETT |n|
                         (SPADCALL
                             (LETT |k| (|SPADfirst| |l|)
                                   |ES-;mainKernel;SU;29|)
                             (|getShellEntry| $ 41))
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
                                        (|getShellEntry| $ 41)))
                                      (SEQ
                                       (LETT |n|
                                        (SPADCALL |kk|
                                         (|getShellEntry| $ 41))
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
                     (LETT |l| (SPADCALL |f| (|getShellEntry| $ 39))
                           |ES-;allKernels|)
                     (|getShellEntry| $ 31))
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
                                        (|getShellEntry| $ 36))
                                       "%dummyVar"
                                       (|getShellEntry| $ 95))
                                      |ES-;allKernels|)
                                (EXIT (COND
                                        ((QEQCAR |u| 0)
                                         (SEQ
                                          (LETT |arg|
                                           (SPADCALL |k|
                                            (|getShellEntry| $ 85))
                                           |ES-;allKernels|)
                                          (LETT |s0|
                                           (SPADCALL
                                            (SPADCALL
                                             (SPADCALL |arg|
                                              (|getShellEntry| $ 96))
                                             (|getShellEntry| $ 56))
                                            (|ES-;allKernels|
                                             (|SPADfirst| |arg|) $)
                                            (|getShellEntry| $ 97))
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
                                            (|getShellEntry| $ 32)))))
                                        ('T
                                         (|ES-;allk|
                                          (SPADCALL |k|
                                           (|getShellEntry| $ 85))
                                          $)))))
                           |ES-;allKernels|)
                     (EXIT (LETT |s|
                                 (SPADCALL |s| |t|
                                     (|getShellEntry| $ 32))
                                 |ES-;allKernels|)))
                (LETT #0# (CDR #0#) |ES-;allKernels|) (GO G190) G191
                (EXIT NIL))
           (EXIT |s|))))) 

(DEFUN |ES-;kernel;BoLS;31| (|op| |args| $)
  (COND
    ((NULL (SPADCALL |op| (|getShellEntry| $ 98)))
     (|error| "Unknown operator"))
    ('T (|ES-;okkernel| |op| |args| $)))) 

(DEFUN |ES-;okkernel| (|op| |l| $)
  (PROG (#0=#:G1599 |f| #1=#:G1600)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL |op| |l|
                   (+ 1
                      (SPADCALL (ELT $ 42)
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
                                          (|getShellEntry| $ 100))
                                         #0#)
                                        |ES-;okkernel|)))
                                 (LETT #1# (CDR #1#) |ES-;okkernel|)
                                 (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                          0 (|getShellEntry| $ 45)))
                   (|getShellEntry| $ 101))
               (|getShellEntry| $ 87)))))) 

(DEFUN |ES-;elt;BoLS;33| (|op| |args| $)
  (PROG (|u| #0=#:G1522 |v|)
    (RETURN
      (SEQ (EXIT (COND
                   ((NULL (SPADCALL |op| (|getShellEntry| $ 98)))
                    (|error| "Unknown operator"))
                   ('T
                    (SEQ (SEQ (LETT |u|
                                    (SPADCALL |op|
                                     (|getShellEntry| $ 103))
                                    |ES-;elt;BoLS;33|)
                              (EXIT (COND
                                      ((QEQCAR |u| 0)
                                       (COND
                                         ((SPADCALL (LENGTH |args|)
                                           (QCDR |u|)
                                           (|getShellEntry| $ 104))
                                          (PROGN
                                            (LETT #0#
                                             (|error|
                                              "Wrong number of arguments")
                                             |ES-;elt;BoLS;33|)
                                            (GO #0#))))))))
                         (LETT |v|
                               (SPADCALL |op| |args|
                                   (|getShellEntry| $ 107))
                               |ES-;elt;BoLS;33|)
                         (EXIT (COND
                                 ((QEQCAR |v| 0) (QCDR |v|))
                                 ('T (|ES-;okkernel| |op| |args| $))))))))
           #0# (EXIT #0#))))) 

(DEFUN |ES-;retract;SK;34| (|f| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 109))
                 |ES-;retract;SK;34|)
           (EXIT (COND
                   ((OR (QEQCAR |k| 1)
                        (SPADCALL
                            (SPADCALL (QCDR |k|)
                                (|getShellEntry| $ 87))
                            |f| (|getShellEntry| $ 110)))
                    (|error| "not a kernel"))
                   ('T (QCDR |k|)))))))) 

(DEFUN |ES-;retractIfCan;SU;35| (|f| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 109))
                 |ES-;retractIfCan;SU;35|)
           (EXIT (COND
                   ((OR (QEQCAR |k| 1)
                        (SPADCALL
                            (SPADCALL (QCDR |k|)
                                (|getShellEntry| $ 87))
                            |f| (|getShellEntry| $ 110)))
                    (CONS 1 "failed"))
                   ('T |k|))))))) 

(DEFUN |ES-;is?;SSB;36| (|f| |s| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 113))
                 |ES-;is?;SSB;36|)
           (EXIT (COND
                   ((QEQCAR |k| 1) 'NIL)
                   ('T
                    (SPADCALL (QCDR |k|) |s| (|getShellEntry| $ 114))))))))) 

(DEFUN |ES-;is?;SBoB;37| (|f| |op| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 113))
                 |ES-;is?;SBoB;37|)
           (EXIT (COND
                   ((QEQCAR |k| 1) 'NIL)
                   ('T
                    (SPADCALL (QCDR |k|) |op| (|getShellEntry| $ 51))))))))) 

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
                                       (|getShellEntry| $ 85)))
                                     (|getShellEntry| $ 117))
                                 |ES-;unwrap|)))
                (LETT #0# (CDR #0#) |ES-;unwrap|) (GO G190) G191
                (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |ES-;distribute;3S;39| (|x| |y| $)
  (PROG (|ky| #0=#:G1602 |k| #1=#:G1603)
    (RETURN
      (SEQ (LETT |ky| (SPADCALL |y| (|getShellEntry| $ 56))
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
                                              (|getShellEntry| $ 114))
                                             (SPADCALL |ky|
                                              (|ES-;listk|
                                               (SPADCALL |k|
                                                (|getShellEntry| $ 87))
                                               $)
                                              (|getShellEntry| $ 57)))
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
                     (|getShellEntry| $ 119))))))) 

(DEFUN |ES-;subst;SLS;41| (|f| |leq| $)
  (PROG (|rec|)
    (RETURN
      (SEQ (LETT |rec| (|ES-;mkKerLists| |leq| $) |ES-;subst;SLS;41|)
           (EXIT (SPADCALL |f| (QCAR |rec|) (QCDR |rec|)
                     (|getShellEntry| $ 121))))))) 

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
                               (SPADCALL |eq| (|getShellEntry| $ 124))
                               (|getShellEntry| $ 113))
                           |ES-;mkKerLists|)
                     (EXIT (COND
                             ((QEQCAR |k| 1)
                              (|error| "left hand side must be a single kernel"))
                             ((NULL (SPADCALL (QCDR |k|) |lk|
                                     (|getShellEntry| $ 57)))
                              (SEQ (LETT |lk| (CONS (QCDR |k|) |lk|)
                                    |ES-;mkKerLists|)
                                   (EXIT
                                    (LETT |lv|
                                     (CONS
                                      (SPADCALL |eq|
                                       (|getShellEntry| $ 125))
                                      |lv|)
                                     |ES-;mkKerLists|)))))))
                (LETT #0# (CDR #0#) |ES-;mkKerLists|) (GO G190) G191
                (EXIT NIL))
           (EXIT (CONS |lk| |lv|)))))) 

(DEFUN |ES-;even?;SB;43| (|x| $) (|ES-;intpred?| |x| (ELT $ 127) $)) 

(DEFUN |ES-;odd?;SB;44| (|x| $) (|ES-;intpred?| |x| (ELT $ 129) $)) 

(DEFUN |ES-;intpred?| (|x| |pred?| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |x| (|getShellEntry| $ 132))
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
        (LETT $ (|newShell| 133) . #0#)
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
             (|setShellEntry| $ 128
                 (CONS (|dispatchFunction| |ES-;even?;SB;43|) $))
             (|setShellEntry| $ 130
                 (CONS (|dispatchFunction| |ES-;odd?;SB;44|) $)))))
        $)))) 

(MAKEPROP '|ExpressionSpace&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|String|)
             (|Symbol|) (0 . |coerce|) (|BasicOperator|)
             (|CommonOperators|) (5 . |operator|) '|oppren| '|opbox|
             (|List| $) (10 . |box|) |ES-;box;2S;1| (15 . |paren|)
             |ES-;paren;2S;2| (|Boolean|) (20 . =) |ES-;belong?;BoB;3|
             (|Kernel| 6) (|List| 23) (|Set| 23) (26 . |parts|)
             (31 . |sort!|) (|Kernel| $) (|List| 28) |ES-;tower;SL;5|
             (36 . |brace|) (41 . |union|) (|Mapping| 25 25 25)
             (|List| 25) (47 . |reduce|) (54 . |operator|) (|List| 10)
             |ES-;operators;SL;7| (59 . |kernels|)
             (|NonNegativeInteger|) (64 . |height|) (69 . |max|)
             (|Mapping| 40 40 40) (|List| 40) (75 . |reduce|)
             |ES-;height;SNni;8| (82 . |name|) (|List| 8)
             (87 . |member?|) |ES-;freeOf?;SSB;9| (93 . |is?|)
             |ES-;distribute;2S;10| (99 . |elt|) |ES-;box;LS;11|
             |ES-;paren;LS;12| (105 . |retract|) (110 . |member?|)
             |ES-;freeOf?;2SB;13| (116 . |kernel|) |ES-;kernel;Bo2S;14|
             |ES-;elt;Bo2S;15| |ES-;elt;Bo3S;16| |ES-;elt;Bo4S;17|
             |ES-;elt;Bo5S;18| (|Mapping| $ 15) (|List| 65)
             (122 . |eval|) |ES-;eval;SSMS;19| (129 . |name|)
             |ES-;eval;SBoMS;20| (|List| 6) (134 . |first|)
             (|Mapping| $ $) |ES-;eval;SSMS;21| (139 . |eval|)
             |ES-;eval;SBoMS;22| (|Equation| $) (|List| 77)
             (146 . |subst|) |ES-;subst;SES;23| (|List| 73)
             |ES-;eval;SLLS;24| |ES-;eval;SLLS;25| |ES-;eval;SLLS;26|
             (152 . |argument|) (157 . =) (163 . |coerce|)
             |ES-;map;MKS;27| (168 . |is?|) |ES-;operator;2Bo;28|
             (|Union| 28 '"failed") |ES-;mainKernel;SU;29| (|None|)
             (|Union| 93 '"failed") (174 . |property|) (180 . |second|)
             (185 . |remove!|) (191 . |belong?|) |ES-;kernel;BoLS;31|
             (196 . |height|) (201 . |kernel|) (|Union| 40 '"failed")
             (208 . |arity|) (213 . ~=) (|Union| 6 '"failed")
             (|BasicOperatorFunctions1| 6) (219 . |evaluate|)
             |ES-;elt;BoLS;33| (225 . |mainKernel|) (230 . ~=)
             |ES-;retract;SK;34| |ES-;retractIfCan;SU;35|
             (236 . |retractIfCan|) (241 . |is?|) |ES-;is?;SSB;36|
             |ES-;is?;SBoB;37| (247 . |eval|) |ES-;distribute;3S;39|
             (254 . |eval|) |ES-;eval;SLS;40| (261 . |subst|)
             |ES-;subst;SLS;41| (|Equation| 6) (268 . |lhs|)
             (273 . |rhs|) (|Integer|) (278 . |even?|) (283 . |even?|)
             (288 . |odd?|) (293 . |odd?|) (|Union| 126 '"failed")
             (298 . |retractIfCan|))
          '#(|tower| 303 |subst| 308 |retractIfCan| 320 |retract| 325
             |paren| 330 |operators| 340 |operator| 345 |odd?| 350
             |map| 355 |mainKernel| 361 |kernel| 366 |is?| 378 |height|
             390 |freeOf?| 395 |even?| 407 |eval| 412 |elt| 467
             |distribute| 503 |box| 514 |belong?| 524)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 132
                                '(1 8 0 7 9 1 11 10 8 12 1 6 0 15 16 1
                                  6 0 15 18 2 10 20 0 0 21 1 25 24 0 26
                                  1 24 0 0 27 1 25 0 24 31 2 25 0 0 0
                                  32 3 34 25 33 0 25 35 1 23 10 0 36 1
                                  6 29 0 39 1 23 40 0 41 2 40 0 0 0 42
                                  3 44 40 43 0 40 45 1 23 8 0 47 2 48
                                  20 8 0 49 2 23 20 0 10 51 2 6 0 10 15
                                  53 1 6 28 0 56 2 24 20 23 0 57 2 6 0
                                  10 15 59 3 6 0 0 48 66 67 1 10 8 0 69
                                  1 71 6 0 72 3 6 0 0 37 66 75 2 6 0 0
                                  78 79 1 23 71 0 85 2 71 20 0 0 86 1 6
                                  0 28 87 2 10 20 0 8 89 2 10 94 0 7 95
                                  1 71 6 0 96 2 25 0 23 0 97 1 6 20 10
                                  98 1 6 40 0 100 3 23 0 10 71 40 101 1
                                  10 102 0 103 2 40 20 0 0 104 2 106
                                  105 10 71 107 1 6 91 0 109 2 6 20 0 0
                                  110 1 6 91 0 113 2 23 20 0 8 114 3 6
                                  0 0 28 0 117 3 6 0 0 29 15 119 3 6 0
                                  0 29 15 121 1 123 6 0 124 1 123 6 0
                                  125 1 126 20 0 127 1 0 20 0 128 1 126
                                  20 0 129 1 0 20 0 130 1 6 131 0 132 1
                                  0 29 0 30 2 0 0 0 78 122 2 0 0 0 77
                                  80 1 0 91 0 112 1 0 28 0 111 1 0 0 0
                                  19 1 0 0 15 55 1 0 37 0 38 1 0 10 10
                                  90 1 0 20 0 130 2 0 0 73 28 88 1 0 91
                                  0 92 2 0 0 10 15 99 2 0 0 10 0 60 2 0
                                  20 0 8 115 2 0 20 0 10 116 1 0 40 0
                                  46 2 0 20 0 8 50 2 0 20 0 0 58 1 0 20
                                  0 128 3 0 0 0 10 73 76 3 0 0 0 37 66
                                  84 3 0 0 0 10 65 70 3 0 0 0 37 81 82
                                  3 0 0 0 8 65 68 3 0 0 0 8 73 74 3 0 0
                                  0 48 81 83 2 0 0 0 78 120 2 0 0 10 15
                                  108 5 0 0 10 0 0 0 0 64 3 0 0 10 0 0
                                  62 4 0 0 10 0 0 0 63 2 0 0 10 0 61 2
                                  0 0 0 0 118 1 0 0 0 52 1 0 0 15 54 1
                                  0 0 0 17 1 0 20 10 22)))))
          '|lookupComplete|)) 
