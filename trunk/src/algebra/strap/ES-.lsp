
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
  (PROG (#0=#:G1578 |f| #1=#:G1579)
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
  (PROG (#0=#:G1580 |k| #1=#:G1581)
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
  (PROG (#0=#:G1582 |k| #1=#:G1583)
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
  (PROG (#0=#:G1584 |k| #1=#:G1585)
    (RETURN
      (SEQ (SPADCALL
               (SPADCALL |s|
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
                   (|getShellEntry| $ 49))
               (|getShellEntry| $ 50)))))) 

(DEFUN |ES-;distribute;2S;10| (|x| $)
  (PROG (#0=#:G1586 |k| #1=#:G1587)
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
                                     (|getShellEntry| $ 52))
                                    (LETT #0# (CONS |k| #0#)
                                     |ES-;distribute;2S;10|)))))
                      (LETT #1# (CDR #1#) |ES-;distribute;2S;10|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               |x| $))))) 

(DEFUN |ES-;box;LS;11| (|l| $)
  (SPADCALL (|getShellEntry| $ 14) |l| (|getShellEntry| $ 54))) 

(DEFUN |ES-;paren;LS;12| (|l| $)
  (SPADCALL (|getShellEntry| $ 13) |l| (|getShellEntry| $ 54))) 

(DEFUN |ES-;freeOf?;2SB;13| (|x| |k| $)
  (SPADCALL
      (SPADCALL (SPADCALL |k| (|getShellEntry| $ 57))
          (|ES-;listk| |x| $) (|getShellEntry| $ 58))
      (|getShellEntry| $ 50))) 

(DEFUN |ES-;kernel;Bo2S;14| (|op| |arg| $)
  (SPADCALL |op| (LIST |arg|) (|getShellEntry| $ 60))) 

(DEFUN |ES-;elt;Bo2S;15| (|op| |x| $)
  (SPADCALL |op| (LIST |x|) (|getShellEntry| $ 54))) 

(DEFUN |ES-;elt;Bo3S;16| (|op| |x| |y| $)
  (SPADCALL |op| (LIST |x| |y|) (|getShellEntry| $ 54))) 

(DEFUN |ES-;elt;Bo4S;17| (|op| |x| |y| |z| $)
  (SPADCALL |op| (LIST |x| |y| |z|) (|getShellEntry| $ 54))) 

(DEFUN |ES-;elt;Bo5S;18| (|op| |x| |y| |z| |t| $)
  (SPADCALL |op| (LIST |x| |y| |z| |t|) (|getShellEntry| $ 54))) 

(DEFUN |ES-;eval;SSMS;19| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|) (LIST |f|) (|getShellEntry| $ 68))) 

(DEFUN |ES-;eval;SBoMS;20| (|x| |s| |f| $)
  (SPADCALL |x| (LIST (SPADCALL |s| (|getShellEntry| $ 70))) (LIST |f|)
      (|getShellEntry| $ 68))) 

(DEFUN |ES-;eval;SSMS;21| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|)
            (LIST (CONS #'|ES-;eval;SSMS;21!0| (VECTOR |f| $)))
            (|getShellEntry| $ 68))) 

(DEFUN |ES-;eval;SSMS;21!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 73))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SBoMS;22| (|x| |s| |f| $)
  (SPADCALL |x| (LIST |s|)
      (LIST (CONS #'|ES-;eval;SBoMS;22!0| (VECTOR |f| $)))
      (|getShellEntry| $ 76))) 

(DEFUN |ES-;eval;SBoMS;22!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 73))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;subst;SES;23| (|x| |e| $)
  (SPADCALL |x| (LIST |e|) (|getShellEntry| $ 80))) 

(DEFUN |ES-;eval;SLLS;24| (|x| |ls| |lf| $)
  (PROG (#0=#:G1588 |f| #1=#:G1589)
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
               (|getShellEntry| $ 76)))))) 

(DEFUN |ES-;eval;SLLS;24!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 73))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SLLS;25| (|x| |ls| |lf| $)
  (PROG (#0=#:G1590 |f| #1=#:G1591)
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
               (|getShellEntry| $ 68)))))) 

(DEFUN |ES-;eval;SLLS;25!0| (|#1| $$)
  (SPADCALL (SPADCALL |#1| (|getShellEntry| (|getShellEntry| $$ 1) 73))
      (|getShellEntry| $$ 0))) 

(DEFUN |ES-;eval;SLLS;26| (|x| |ls| |lf| $)
  (PROG (#0=#:G1592 |s| #1=#:G1593)
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
                                         (|getShellEntry| $ 70))
                                        #0#)
                                       |ES-;eval;SLLS;26|)))
                      (LETT #1# (CDR #1#) |ES-;eval;SLLS;26|) (GO G190)
                      G191 (EXIT (NREVERSE0 #0#))))
               |lf| (|getShellEntry| $ 68)))))) 

(DEFUN |ES-;map;MKS;27| (|fn| |k| $)
  (PROG (#0=#:G1594 |x| #1=#:G1595 |l|)
    (RETURN
      (SEQ (COND
             ((SPADCALL
                  (LETT |l|
                        (PROGN
                          (LETT #0# NIL |ES-;map;MKS;27|)
                          (SEQ (LETT |x| NIL |ES-;map;MKS;27|)
                               (LETT #1#
                                     (SPADCALL |k|
                                      (|getShellEntry| $ 86))
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
                  (SPADCALL |k| (|getShellEntry| $ 86))
                  (|getShellEntry| $ 87))
              (SPADCALL |k| (|getShellEntry| $ 88)))
             ('T
              (SPADCALL (SPADCALL |k| (|getShellEntry| $ 36)) |l|
                  (|getShellEntry| $ 54)))))))) 

(DEFUN |ES-;operator;2Bo;28| (|op| $)
  (COND
    ((SPADCALL |op| (SPADCALL "%paren" (|getShellEntry| $ 9))
         (|getShellEntry| $ 90))
     (|getShellEntry| $ 13))
    ((SPADCALL |op| (SPADCALL "%box" (|getShellEntry| $ 9))
         (|getShellEntry| $ 90))
     (|getShellEntry| $ 14))
    ('T (|error| "Unknown operator")))) 

(DEFUN |ES-;mainKernel;SU;29| (|x| $)
  (PROG (|l| |kk| #0=#:G1596 |n| |k|)
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
  (PROG (|l| |k| #0=#:G1597 |u| |s0| |n| |arg| |t| |s|)
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
                                       (|getShellEntry| $ 96))
                                      |ES-;allKernels|)
                                (EXIT (COND
                                        ((QEQCAR |u| 0)
                                         (SEQ
                                          (LETT |arg|
                                           (SPADCALL |k|
                                            (|getShellEntry| $ 86))
                                           |ES-;allKernels|)
                                          (LETT |s0|
                                           (SPADCALL
                                            (SPADCALL
                                             (SPADCALL |arg|
                                              (|getShellEntry| $ 97))
                                             (|getShellEntry| $ 57))
                                            (|ES-;allKernels|
                                             (|SPADfirst| |arg|) $)
                                            (|getShellEntry| $ 98))
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
                                           (|getShellEntry| $ 86))
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
    ((NULL (SPADCALL |op| (|getShellEntry| $ 99)))
     (|error| "Unknown operator"))
    ('T (|ES-;okkernel| |op| |args| $)))) 

(DEFUN |ES-;okkernel| (|op| |l| $)
  (PROG (#0=#:G1598 |f| #1=#:G1599)
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
                                          (|getShellEntry| $ 101))
                                         #0#)
                                        |ES-;okkernel|)))
                                 (LETT #1# (CDR #1#) |ES-;okkernel|)
                                 (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                          0 (|getShellEntry| $ 45)))
                   (|getShellEntry| $ 102))
               (|getShellEntry| $ 88)))))) 

(DEFUN |ES-;elt;BoLS;33| (|op| |args| $)
  (PROG (|u| #0=#:G1521 |v|)
    (RETURN
      (SEQ (EXIT (COND
                   ((NULL (SPADCALL |op| (|getShellEntry| $ 99)))
                    (|error| "Unknown operator"))
                   ('T
                    (SEQ (SEQ (LETT |u|
                                    (SPADCALL |op|
                                     (|getShellEntry| $ 104))
                                    |ES-;elt;BoLS;33|)
                              (EXIT (COND
                                      ((QEQCAR |u| 0)
                                       (COND
                                         ((SPADCALL (LENGTH |args|)
                                           (QCDR |u|)
                                           (|getShellEntry| $ 105))
                                          (PROGN
                                            (LETT #0#
                                             (|error|
                                              "Wrong number of arguments")
                                             |ES-;elt;BoLS;33|)
                                            (GO #0#))))))))
                         (LETT |v|
                               (SPADCALL |op| |args|
                                   (|getShellEntry| $ 108))
                               |ES-;elt;BoLS;33|)
                         (EXIT (COND
                                 ((QEQCAR |v| 0) (QCDR |v|))
                                 ('T (|ES-;okkernel| |op| |args| $))))))))
           #0# (EXIT #0#))))) 

(DEFUN |ES-;retract;SK;34| (|f| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 110))
                 |ES-;retract;SK;34|)
           (EXIT (COND
                   ((OR (QEQCAR |k| 1)
                        (SPADCALL
                            (SPADCALL (QCDR |k|)
                                (|getShellEntry| $ 88))
                            |f| (|getShellEntry| $ 111)))
                    (|error| "not a kernel"))
                   ('T (QCDR |k|)))))))) 

(DEFUN |ES-;retractIfCan;SU;35| (|f| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 110))
                 |ES-;retractIfCan;SU;35|)
           (EXIT (COND
                   ((OR (QEQCAR |k| 1)
                        (SPADCALL
                            (SPADCALL (QCDR |k|)
                                (|getShellEntry| $ 88))
                            |f| (|getShellEntry| $ 111)))
                    (CONS 1 "failed"))
                   ('T |k|))))))) 

(DEFUN |ES-;is?;SSB;36| (|f| |s| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 114))
                 |ES-;is?;SSB;36|)
           (EXIT (COND
                   ((QEQCAR |k| 1) 'NIL)
                   ('T
                    (SPADCALL (QCDR |k|) |s| (|getShellEntry| $ 115))))))))) 

(DEFUN |ES-;is?;SBoB;37| (|f| |op| $)
  (PROG (|k|)
    (RETURN
      (SEQ (LETT |k| (SPADCALL |f| (|getShellEntry| $ 114))
                 |ES-;is?;SBoB;37|)
           (EXIT (COND
                   ((QEQCAR |k| 1) 'NIL)
                   ('T
                    (SPADCALL (QCDR |k|) |op| (|getShellEntry| $ 52))))))))) 

(DEFUN |ES-;unwrap| (|l| |x| $)
  (PROG (|k| #0=#:G1600)
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
                                       (|getShellEntry| $ 86)))
                                     (|getShellEntry| $ 118))
                                 |ES-;unwrap|)))
                (LETT #0# (CDR #0#) |ES-;unwrap|) (GO G190) G191
                (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |ES-;distribute;3S;39| (|x| |y| $)
  (PROG (|ky| #0=#:G1601 |k| #1=#:G1602)
    (RETURN
      (SEQ (LETT |ky| (SPADCALL |y| (|getShellEntry| $ 57))
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
                                              (|getShellEntry| $ 115))
                                             (SPADCALL |ky|
                                              (|ES-;listk|
                                               (SPADCALL |k|
                                                (|getShellEntry| $ 88))
                                               $)
                                              (|getShellEntry| $ 58)))
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
                     (|getShellEntry| $ 120))))))) 

(DEFUN |ES-;subst;SLS;41| (|f| |leq| $)
  (PROG (|rec|)
    (RETURN
      (SEQ (LETT |rec| (|ES-;mkKerLists| |leq| $) |ES-;subst;SLS;41|)
           (EXIT (SPADCALL |f| (QCAR |rec|) (QCDR |rec|)
                     (|getShellEntry| $ 122))))))) 

(DEFUN |ES-;mkKerLists| (|leq| $)
  (PROG (|eq| #0=#:G1603 |k| |lk| |lv|)
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
                               (SPADCALL |eq| (|getShellEntry| $ 125))
                               (|getShellEntry| $ 114))
                           |ES-;mkKerLists|)
                     (EXIT (COND
                             ((QEQCAR |k| 1)
                              (|error| "left hand side must be a single kernel"))
                             ((NULL (SPADCALL (QCDR |k|) |lk|
                                     (|getShellEntry| $ 58)))
                              (SEQ (LETT |lk| (CONS (QCDR |k|) |lk|)
                                    |ES-;mkKerLists|)
                                   (EXIT
                                    (LETT |lv|
                                     (CONS
                                      (SPADCALL |eq|
                                       (|getShellEntry| $ 126))
                                      |lv|)
                                     |ES-;mkKerLists|)))))))
                (LETT #0# (CDR #0#) |ES-;mkKerLists|) (GO G190) G191
                (EXIT NIL))
           (EXIT (CONS |lk| |lv|)))))) 

(DEFUN |ES-;even?;SB;43| (|x| $) (|ES-;intpred?| |x| (ELT $ 128) $)) 

(DEFUN |ES-;odd?;SB;44| (|x| $) (|ES-;intpred?| |x| (ELT $ 130) $)) 

(DEFUN |ES-;intpred?| (|x| |pred?| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL |x| (|getShellEntry| $ 133))
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
        (LETT $ (|newShell| 134) . #0#)
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
             (|setShellEntry| $ 129
                 (CONS (|dispatchFunction| |ES-;even?;SB;43|) $))
             (|setShellEntry| $ 131
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
             (87 . |member?|) (93 . |not|) |ES-;freeOf?;SSB;9|
             (98 . |is?|) |ES-;distribute;2S;10| (104 . |elt|)
             |ES-;box;LS;11| |ES-;paren;LS;12| (110 . |retract|)
             (115 . |member?|) |ES-;freeOf?;2SB;13| (121 . |kernel|)
             |ES-;kernel;Bo2S;14| |ES-;elt;Bo2S;15| |ES-;elt;Bo3S;16|
             |ES-;elt;Bo4S;17| |ES-;elt;Bo5S;18| (|Mapping| $ 15)
             (|List| 66) (127 . |eval|) |ES-;eval;SSMS;19|
             (134 . |name|) |ES-;eval;SBoMS;20| (|List| 6)
             (139 . |first|) (|Mapping| $ $) |ES-;eval;SSMS;21|
             (144 . |eval|) |ES-;eval;SBoMS;22| (|Equation| $)
             (|List| 78) (151 . |subst|) |ES-;subst;SES;23| (|List| 74)
             |ES-;eval;SLLS;24| |ES-;eval;SLLS;25| |ES-;eval;SLLS;26|
             (157 . |argument|) (162 . =) (168 . |coerce|)
             |ES-;map;MKS;27| (173 . |is?|) |ES-;operator;2Bo;28|
             (|Union| 28 '"failed") |ES-;mainKernel;SU;29| (|None|)
             (|Union| 94 '"failed") (179 . |property|) (185 . |second|)
             (190 . |remove!|) (196 . |belong?|) |ES-;kernel;BoLS;31|
             (201 . |height|) (206 . |kernel|) (|Union| 40 '"failed")
             (213 . |arity|) (218 . ~=) (|Union| 6 '"failed")
             (|BasicOperatorFunctions1| 6) (224 . |evaluate|)
             |ES-;elt;BoLS;33| (230 . |mainKernel|) (235 . ~=)
             |ES-;retract;SK;34| |ES-;retractIfCan;SU;35|
             (241 . |retractIfCan|) (246 . |is?|) |ES-;is?;SSB;36|
             |ES-;is?;SBoB;37| (252 . |eval|) |ES-;distribute;3S;39|
             (259 . |eval|) |ES-;eval;SLS;40| (266 . |subst|)
             |ES-;subst;SLS;41| (|Equation| 6) (273 . |lhs|)
             (278 . |rhs|) (|Integer|) (283 . |even?|) (288 . |even?|)
             (293 . |odd?|) (298 . |odd?|) (|Union| 127 '"failed")
             (303 . |retractIfCan|))
          '#(|tower| 308 |subst| 313 |retractIfCan| 325 |retract| 330
             |paren| 335 |operators| 345 |operator| 350 |odd?| 355
             |map| 360 |mainKernel| 366 |kernel| 371 |is?| 383 |height|
             395 |freeOf?| 400 |even?| 412 |eval| 417 |elt| 472
             |distribute| 508 |box| 519 |belong?| 529)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 133
                                '(1 8 0 7 9 1 11 10 8 12 1 6 0 15 16 1
                                  6 0 15 18 2 10 20 0 0 21 1 25 24 0 26
                                  1 24 0 0 27 1 25 0 24 31 2 25 0 0 0
                                  32 3 34 25 33 0 25 35 1 23 10 0 36 1
                                  6 29 0 39 1 23 40 0 41 2 40 0 0 0 42
                                  3 44 40 43 0 40 45 1 23 8 0 47 2 48
                                  20 8 0 49 1 20 0 0 50 2 23 20 0 10 52
                                  2 6 0 10 15 54 1 6 28 0 57 2 24 20 23
                                  0 58 2 6 0 10 15 60 3 6 0 0 48 67 68
                                  1 10 8 0 70 1 72 6 0 73 3 6 0 0 37 67
                                  76 2 6 0 0 79 80 1 23 72 0 86 2 72 20
                                  0 0 87 1 6 0 28 88 2 10 20 0 8 90 2
                                  10 95 0 7 96 1 72 6 0 97 2 25 0 23 0
                                  98 1 6 20 10 99 1 6 40 0 101 3 23 0
                                  10 72 40 102 1 10 103 0 104 2 40 20 0
                                  0 105 2 107 106 10 72 108 1 6 92 0
                                  110 2 6 20 0 0 111 1 6 92 0 114 2 23
                                  20 0 8 115 3 6 0 0 28 0 118 3 6 0 0
                                  29 15 120 3 6 0 0 29 15 122 1 124 6 0
                                  125 1 124 6 0 126 1 127 20 0 128 1 0
                                  20 0 129 1 127 20 0 130 1 0 20 0 131
                                  1 6 132 0 133 1 0 29 0 30 2 0 0 0 79
                                  123 2 0 0 0 78 81 1 0 92 0 113 1 0 28
                                  0 112 1 0 0 0 19 1 0 0 15 56 1 0 37 0
                                  38 1 0 10 10 91 1 0 20 0 131 2 0 0 74
                                  28 89 1 0 92 0 93 2 0 0 10 15 100 2 0
                                  0 10 0 61 2 0 20 0 8 116 2 0 20 0 10
                                  117 1 0 40 0 46 2 0 20 0 8 51 2 0 20
                                  0 0 59 1 0 20 0 129 3 0 0 0 10 74 77
                                  3 0 0 0 37 67 85 3 0 0 0 10 66 71 3 0
                                  0 0 37 82 83 3 0 0 0 8 66 69 3 0 0 0
                                  8 74 75 3 0 0 0 48 82 84 2 0 0 0 79
                                  121 2 0 0 10 15 109 5 0 0 10 0 0 0 0
                                  65 3 0 0 10 0 0 63 4 0 0 10 0 0 0 64
                                  2 0 0 10 0 62 2 0 0 0 0 119 1 0 0 0
                                  53 1 0 0 15 55 1 0 0 0 17 1 0 20 10
                                  22)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|ExpressionSpace&| '|isFunctor|
             '(((|odd?| ((|Boolean|) $)) T (ELT $ 131))
               ((|even?| ((|Boolean|) $)) T (ELT $ 129))
               ((|eval| ($ $ (|BasicOperator|) (|Mapping| $ $))) T
                (ELT $ 77))
               ((|eval| ($ $ (|BasicOperator|)
                           (|Mapping| $ (|List| $))))
                T (ELT $ 71))
               ((|eval| ($ $ (|List| (|BasicOperator|))
                           (|List| (|Mapping| $ (|List| $)))))
                T (ELT $ 85))
               ((|eval| ($ $ (|List| (|BasicOperator|))
                           (|List| (|Mapping| $ $))))
                T (ELT $ 83))
               ((|eval| ($ $ (|Symbol|) (|Mapping| $ $))) T (ELT $ 75))
               ((|eval| ($ $ (|Symbol|) (|Mapping| $ (|List| $)))) T
                (ELT $ 69))
               ((|eval| ($ $ (|List| (|Symbol|))
                           (|List| (|Mapping| $ (|List| $)))))
                T (ELT $ NIL))
               ((|eval| ($ $ (|List| (|Symbol|))
                           (|List| (|Mapping| $ $))))
                T (ELT $ 84))
               ((|freeOf?| ((|Boolean|) $ (|Symbol|))) T (ELT $ 51))
               ((|freeOf?| ((|Boolean|) $ $)) T (ELT $ 59))
               ((|map| ($ (|Mapping| $ $) (|Kernel| $))) T (ELT $ 89))
               ((|kernel| ($ (|BasicOperator|) (|List| $))) T
                (ELT $ 100))
               ((|kernel| ($ (|BasicOperator|) $)) T (ELT $ 61))
               ((|is?| ((|Boolean|) $ (|Symbol|))) T (ELT $ 116))
               ((|is?| ((|Boolean|) $ (|BasicOperator|))) T
                (ELT $ 117))
               ((|belong?| ((|Boolean|) (|BasicOperator|))) T
                (ELT $ 22))
               ((|operator| ((|BasicOperator|) (|BasicOperator|))) T
                (ELT $ 91))
               ((|operators| ((|List| (|BasicOperator|)) $)) T
                (ELT $ 38))
               ((|tower| ((|List| (|Kernel| $)) $)) T (ELT $ 30))
               ((|mainKernel| ((|Union| (|Kernel| $) "failed") $)) T
                (ELT $ 93))
               ((|height| ((|NonNegativeInteger|) $)) T (ELT $ 46))
               ((|distribute| ($ $ $)) T (ELT $ 119))
               ((|distribute| ($ $)) T (ELT $ 53))
               ((|paren| ($ (|List| $))) T (ELT $ 56))
               ((|paren| ($ $)) T (ELT $ 19))
               ((|box| ($ (|List| $))) T (ELT $ 55))
               ((|box| ($ $)) T (ELT $ 17))
               ((|subst| ($ $ (|List| (|Kernel| $)) (|List| $))) T
                (ELT $ NIL))
               ((|subst| ($ $ (|List| (|Equation| $)))) T (ELT $ 123))
               ((|subst| ($ $ (|Equation| $))) T (ELT $ 81))
               ((|elt| ($ (|BasicOperator|) (|List| $))) T (ELT $ 109))
               ((|elt| ($ (|BasicOperator|) $ $ $ $)) T (ELT $ 65))
               ((|elt| ($ (|BasicOperator|) $ $ $)) T (ELT $ 64))
               ((|elt| ($ (|BasicOperator|) $ $)) T (ELT $ 63))
               ((|elt| ($ (|BasicOperator|) $)) T (ELT $ 62))
               ((|eval| ($ $ (|List| $) (|List| $))) T (ELT $ NIL))
               ((|eval| ($ $ $ $)) T (ELT $ NIL))
               ((|eval| ($ $ (|Equation| $))) T (ELT $ NIL))
               ((|eval| ($ $ (|List| (|Equation| $)))) T (ELT $ 121))
               ((|eval| ($ $ (|List| (|Kernel| $)) (|List| $))) T
                (ELT $ NIL))
               ((|eval| ($ $ (|Kernel| $) $)) T (ELT $ NIL))
               ((|retract| ((|Kernel| $) $)) T (ELT $ 112))
               ((|retractIfCan| ((|Union| (|Kernel| $) "failed") $)) T
                (ELT $ 113)))
             (|addModemap| '|ExpressionSpace&|
                 '(|ExpressionSpace&| |#1|)
                 '((CATEGORY |domain|
                       (SIGNATURE |odd?| ((|Boolean|) |#1|))
                       (SIGNATURE |even?| ((|Boolean|) |#1|))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|BasicOperator|)
                                 (|Mapping| |#1| |#1|)))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|BasicOperator|)
                                 (|Mapping| |#1| (|List| |#1|))))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|BasicOperator|))
                                 (|List| (|Mapping| |#1| (|List| |#1|)))))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|BasicOperator|))
                                 (|List| (|Mapping| |#1| |#1|))))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|Symbol|) (|Mapping| |#1| |#1|)))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|Symbol|)
                                 (|Mapping| |#1| (|List| |#1|))))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|Symbol|))
                                 (|List| (|Mapping| |#1| (|List| |#1|)))))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|Symbol|))
                                 (|List| (|Mapping| |#1| |#1|))))
                       (SIGNATURE |freeOf?|
                           ((|Boolean|) |#1| (|Symbol|)))
                       (SIGNATURE |freeOf?| ((|Boolean|) |#1| |#1|))
                       (SIGNATURE |map|
                           (|#1| (|Mapping| |#1| |#1|) (|Kernel| |#1|)))
                       (SIGNATURE |kernel|
                           (|#1| (|BasicOperator|) (|List| |#1|)))
                       (SIGNATURE |kernel|
                                  (|#1| (|BasicOperator|) |#1|))
                       (SIGNATURE |is?| ((|Boolean|) |#1| (|Symbol|)))
                       (SIGNATURE |is?|
                           ((|Boolean|) |#1| (|BasicOperator|)))
                       (SIGNATURE |belong?|
                           ((|Boolean|) (|BasicOperator|)))
                       (SIGNATURE |operator|
                           ((|BasicOperator|) (|BasicOperator|)))
                       (SIGNATURE |operators|
                           ((|List| (|BasicOperator|)) |#1|))
                       (SIGNATURE |tower|
                           ((|List| (|Kernel| |#1|)) |#1|))
                       (SIGNATURE |mainKernel|
                           ((|Union| (|Kernel| |#1|) "failed") |#1|))
                       (SIGNATURE |height|
                           ((|NonNegativeInteger|) |#1|))
                       (SIGNATURE |distribute| (|#1| |#1| |#1|))
                       (SIGNATURE |distribute| (|#1| |#1|))
                       (SIGNATURE |paren| (|#1| (|List| |#1|)))
                       (SIGNATURE |paren| (|#1| |#1|))
                       (SIGNATURE |box| (|#1| (|List| |#1|)))
                       (SIGNATURE |box| (|#1| |#1|))
                       (SIGNATURE |subst|
                           (|#1| |#1| (|List| (|Kernel| |#1|))
                                 (|List| |#1|)))
                       (SIGNATURE |subst|
                           (|#1| |#1| (|List| (|Equation| |#1|))))
                       (SIGNATURE |subst|
                                  (|#1| |#1| (|Equation| |#1|)))
                       (SIGNATURE |elt|
                           (|#1| (|BasicOperator|) (|List| |#1|)))
                       (SIGNATURE |elt|
                           (|#1| (|BasicOperator|) |#1| |#1| |#1| |#1|))
                       (SIGNATURE |elt|
                                  (|#1| (|BasicOperator|) |#1| |#1|
                                        |#1|))
                       (SIGNATURE |elt|
                           (|#1| (|BasicOperator|) |#1| |#1|))
                       (SIGNATURE |elt| (|#1| (|BasicOperator|) |#1|))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| |#1|) (|List| |#1|)))
                       (SIGNATURE |eval| (|#1| |#1| |#1| |#1|))
                       (SIGNATURE |eval| (|#1| |#1| (|Equation| |#1|)))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|Equation| |#1|))))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|List| (|Kernel| |#1|))
                                 (|List| |#1|)))
                       (SIGNATURE |eval|
                           (|#1| |#1| (|Kernel| |#1|) |#1|))
                       (SIGNATURE |retract| ((|Kernel| |#1|) |#1|))
                       (SIGNATURE |retractIfCan|
                           ((|Union| (|Kernel| |#1|) "failed") |#1|)))
                   (|ExpressionSpace|))
                 T '|ExpressionSpace&|
                 (|put| '|ExpressionSpace&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |odd?| ((|Boolean|) |#1|))
                                 (SIGNATURE |even?| ((|Boolean|) |#1|))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|BasicOperator|)
                                      (|Mapping| |#1| |#1|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|BasicOperator|)
                                      (|Mapping| |#1| (|List| |#1|))))
                                 (SIGNATURE |eval|
                                     (|#1| |#1|
                                      (|List| (|BasicOperator|))
                                      (|List|
                                       (|Mapping| |#1| (|List| |#1|)))))
                                 (SIGNATURE |eval|
                                     (|#1| |#1|
                                      (|List| (|BasicOperator|))
                                      (|List| (|Mapping| |#1| |#1|))))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|Symbol|)
                                      (|Mapping| |#1| |#1|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|Symbol|)
                                      (|Mapping| |#1| (|List| |#1|))))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|List| (|Symbol|))
                                      (|List|
                                       (|Mapping| |#1| (|List| |#1|)))))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|List| (|Symbol|))
                                      (|List| (|Mapping| |#1| |#1|))))
                                 (SIGNATURE |freeOf?|
                                     ((|Boolean|) |#1| (|Symbol|)))
                                 (SIGNATURE |freeOf?|
                                     ((|Boolean|) |#1| |#1|))
                                 (SIGNATURE |map|
                                     (|#1| (|Mapping| |#1| |#1|)
                                      (|Kernel| |#1|)))
                                 (SIGNATURE |kernel|
                                     (|#1| (|BasicOperator|)
                                      (|List| |#1|)))
                                 (SIGNATURE |kernel|
                                     (|#1| (|BasicOperator|) |#1|))
                                 (SIGNATURE |is?|
                                     ((|Boolean|) |#1| (|Symbol|)))
                                 (SIGNATURE |is?|
                                     ((|Boolean|) |#1|
                                      (|BasicOperator|)))
                                 (SIGNATURE |belong?|
                                     ((|Boolean|) (|BasicOperator|)))
                                 (SIGNATURE |operator|
                                     ((|BasicOperator|)
                                      (|BasicOperator|)))
                                 (SIGNATURE |operators|
                                     ((|List| (|BasicOperator|)) |#1|))
                                 (SIGNATURE |tower|
                                     ((|List| (|Kernel| |#1|)) |#1|))
                                 (SIGNATURE |mainKernel|
                                     ((|Union| (|Kernel| |#1|)
                                       "failed")
                                      |#1|))
                                 (SIGNATURE |height|
                                     ((|NonNegativeInteger|) |#1|))
                                 (SIGNATURE |distribute|
                                     (|#1| |#1| |#1|))
                                 (SIGNATURE |distribute| (|#1| |#1|))
                                 (SIGNATURE |paren|
                                     (|#1| (|List| |#1|)))
                                 (SIGNATURE |paren| (|#1| |#1|))
                                 (SIGNATURE |box| (|#1| (|List| |#1|)))
                                 (SIGNATURE |box| (|#1| |#1|))
                                 (SIGNATURE |subst|
                                     (|#1| |#1|
                                      (|List| (|Kernel| |#1|))
                                      (|List| |#1|)))
                                 (SIGNATURE |subst|
                                     (|#1| |#1|
                                      (|List| (|Equation| |#1|))))
                                 (SIGNATURE |subst|
                                     (|#1| |#1| (|Equation| |#1|)))
                                 (SIGNATURE |elt|
                                     (|#1| (|BasicOperator|)
                                      (|List| |#1|)))
                                 (SIGNATURE |elt|
                                     (|#1| (|BasicOperator|) |#1| |#1|
                                      |#1| |#1|))
                                 (SIGNATURE |elt|
                                     (|#1| (|BasicOperator|) |#1| |#1|
                                      |#1|))
                                 (SIGNATURE |elt|
                                     (|#1| (|BasicOperator|) |#1| |#1|))
                                 (SIGNATURE |elt|
                                     (|#1| (|BasicOperator|) |#1|))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|List| |#1|)
                                      (|List| |#1|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| |#1| |#1|))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|Equation| |#1|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1|
                                      (|List| (|Equation| |#1|))))
                                 (SIGNATURE |eval|
                                     (|#1| |#1|
                                      (|List| (|Kernel| |#1|))
                                      (|List| |#1|)))
                                 (SIGNATURE |eval|
                                     (|#1| |#1| (|Kernel| |#1|) |#1|))
                                 (SIGNATURE |retract|
                                     ((|Kernel| |#1|) |#1|))
                                 (SIGNATURE |retractIfCan|
                                     ((|Union| (|Kernel| |#1|)
                                       "failed")
                                      |#1|)))
                             (|ExpressionSpace|))
                        |$CategoryFrame|)))) 
