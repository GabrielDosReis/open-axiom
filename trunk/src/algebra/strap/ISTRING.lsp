
(/VERSIONCHECK 2) 

(PUT '|ISTRING;new;NniC$;1| '|SPADreplace| 'MAKE-FULL-CVEC) 

(DEFUN |ISTRING;new;NniC$;1| (|n| |c| $) (MAKE-FULL-CVEC |n| |c|)) 

(PUT '|ISTRING;empty;$;2| '|SPADreplace|
     '(XLAM NIL (MAKE-FULL-CVEC 0))) 

(DEFUN |ISTRING;empty;$;2| ($) (MAKE-FULL-CVEC 0)) 

(DEFUN |ISTRING;empty?;$B;3| (|s| $) (EQL (QCSIZE |s|) 0)) 

(PUT '|ISTRING;#;$Nni;4| '|SPADreplace| 'QCSIZE) 

(DEFUN |ISTRING;#;$Nni;4| (|s| $) (QCSIZE |s|)) 

(PUT '|ISTRING;=;2$B;5| '|SPADreplace| 'EQUAL) 

(DEFUN |ISTRING;=;2$B;5| (|s| |t| $) (EQUAL |s| |t|)) 

(PUT '|ISTRING;<;2$B;6| '|SPADreplace|
     '(XLAM (|s| |t|) (CGREATERP |t| |s|))) 

(DEFUN |ISTRING;<;2$B;6| (|s| |t| $) (CGREATERP |t| |s|)) 

(PUT '|ISTRING;concat;3$;7| '|SPADreplace| 'STRCONC) 

(DEFUN |ISTRING;concat;3$;7| (|s| |t| $) (STRCONC |s| |t|)) 

(PUT '|ISTRING;copy;2$;8| '|SPADreplace| 'COPY-SEQ) 

(DEFUN |ISTRING;copy;2$;8| (|s| $) (COPY-SEQ |s|)) 

(DEFUN |ISTRING;insert;2$I$;9| (|s| |t| |i| $)
  (SPADCALL
      (SPADCALL
          (SPADCALL |s|
              (SPADCALL (QREFELT $ 6) (- |i| 1) (QREFELT $ 20))
              (QREFELT $ 21))
          |t| (QREFELT $ 16))
      (SPADCALL |s| (SPADCALL |i| (QREFELT $ 22)) (QREFELT $ 21))
      (QREFELT $ 16))) 

(DEFUN |ISTRING;coerce;$Of;10| (|s| $) (SPADCALL |s| (QREFELT $ 26))) 

(DEFUN |ISTRING;minIndex;$I;11| (|s| $) (QREFELT $ 6)) 

(DEFUN |ISTRING;upperCase!;2$;12| (|s| $)
  (SPADCALL (ELT $ 31) |s| (QREFELT $ 33))) 

(DEFUN |ISTRING;lowerCase!;2$;13| (|s| $)
  (SPADCALL (ELT $ 36) |s| (QREFELT $ 33))) 

(DEFUN |ISTRING;latex;$S;14| (|s| $)
  (STRCONC "\\mbox{``" (STRCONC |s| "''}"))) 

(DEFUN |ISTRING;replace;$Us2$;15| (|s| |sg| |t| $)
  (PROG (|l| |m| |n| |h| #0=#:G1770 |r| #1=#:G1776 #2=#:G1777 |i|
             #3=#:G1778 |k|)
    (RETURN
      (SEQ (LETT |l| (- (SPADCALL |sg| (QREFELT $ 39)) (QREFELT $ 6))
                 |ISTRING;replace;$Us2$;15|)
           (LETT |m| (SPADCALL |s| (QREFELT $ 13))
                 |ISTRING;replace;$Us2$;15|)
           (LETT |n| (SPADCALL |t| (QREFELT $ 13))
                 |ISTRING;replace;$Us2$;15|)
           (LETT |h|
                 (COND
                   ((SPADCALL |sg| (QREFELT $ 40))
                    (- (SPADCALL |sg| (QREFELT $ 41)) (QREFELT $ 6)))
                   ('T (- (SPADCALL |s| (QREFELT $ 42)) (QREFELT $ 6))))
                 |ISTRING;replace;$Us2$;15|)
           (COND
             ((OR (OR (< |l| 0) (NULL (< |h| |m|))) (< |h| (- |l| 1)))
              (EXIT (|error| "index out of range"))))
           (LETT |r|
                 (SPADCALL
                     (PROG1 (LETT #0# (+ (- |m| (+ (- |h| |l|) 1)) |n|)
                                  |ISTRING;replace;$Us2$;15|)
                       (|check-subtype| (>= #0# 0)
                           '(|NonNegativeInteger|) #0#))
                     (SPADCALL (QREFELT $ 43)) (QREFELT $ 9))
                 |ISTRING;replace;$Us2$;15|)
           (SEQ (LETT |i| 0 |ISTRING;replace;$Us2$;15|)
                (LETT #1# (- |l| 1) |ISTRING;replace;$Us2$;15|)
                (LETT |k| 0 |ISTRING;replace;$Us2$;15|) G190
                (COND ((QSGREATERP |i| #1#) (GO G191)))
                (SEQ (EXIT (QESET |r| |k| (CHAR |s| |i|))))
                (LETT |k|
                      (PROG1 (QSADD1 |k|)
                        (LETT |i| (QSADD1 |i|)
                              |ISTRING;replace;$Us2$;15|))
                      |ISTRING;replace;$Us2$;15|)
                (GO G190) G191 (EXIT NIL))
           (SEQ (LETT |i| 0 |ISTRING;replace;$Us2$;15|)
                (LETT #2# (- |n| 1) |ISTRING;replace;$Us2$;15|)
                (LETT |k| |k| |ISTRING;replace;$Us2$;15|) G190
                (COND ((QSGREATERP |i| #2#) (GO G191)))
                (SEQ (EXIT (QESET |r| |k| (CHAR |t| |i|))))
                (LETT |k|
                      (PROG1 (+ |k| 1)
                        (LETT |i| (QSADD1 |i|)
                              |ISTRING;replace;$Us2$;15|))
                      |ISTRING;replace;$Us2$;15|)
                (GO G190) G191 (EXIT NIL))
           (SEQ (LETT |i| (+ |h| 1) |ISTRING;replace;$Us2$;15|)
                (LETT #3# (- |m| 1) |ISTRING;replace;$Us2$;15|)
                (LETT |k| |k| |ISTRING;replace;$Us2$;15|) G190
                (COND ((> |i| #3#) (GO G191)))
                (SEQ (EXIT (QESET |r| |k| (CHAR |s| |i|))))
                (LETT |k|
                      (PROG1 (+ |k| 1)
                        (LETT |i| (+ |i| 1) |ISTRING;replace;$Us2$;15|))
                      |ISTRING;replace;$Us2$;15|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |ISTRING;setelt;$I2C;16| (|s| |i| |c| $)
  (SEQ (COND
         ((OR (< |i| (QREFELT $ 6))
              (< (SPADCALL |s| (QREFELT $ 42)) |i|))
          (|error| "index out of range"))
         ('T (SEQ (QESET |s| (- |i| (QREFELT $ 6)) |c|) (EXIT |c|)))))) 

(DEFUN |ISTRING;substring?;2$IB;17| (|part| |whole| |startpos| $)
  (PROG (|np| |nw| |iw| |ip| #0=#:G1788 #1=#:G1787 #2=#:G1783)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |np| (QCSIZE |part|)
                            |ISTRING;substring?;2$IB;17|)
                      (LETT |nw| (QCSIZE |whole|)
                            |ISTRING;substring?;2$IB;17|)
                      (LETT |startpos| (- |startpos| (QREFELT $ 6))
                            |ISTRING;substring?;2$IB;17|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((< (- |nw| |startpos|) |np|) 'NIL)
                              ('T
                               (SEQ (SEQ
                                     (EXIT
                                      (SEQ
                                       (LETT |iw| |startpos|
                                        |ISTRING;substring?;2$IB;17|)
                                       (LETT |ip| 0
                                        |ISTRING;substring?;2$IB;17|)
                                       (LETT #0# (- |np| 1)
                                        |ISTRING;substring?;2$IB;17|)
                                       G190
                                       (COND
                                         ((QSGREATERP |ip| #0#)
                                          (GO G191)))
                                       (SEQ
                                        (EXIT
                                         (COND
                                           ((NULL
                                             (CHAR= (CHAR |part| |ip|)
                                              (CHAR |whole| |iw|)))
                                            (PROGN
                                              (LETT #2#
                                               (PROGN
                                                 (LETT #1# 'NIL
                                                  |ISTRING;substring?;2$IB;17|)
                                                 (GO #1#))
                                               |ISTRING;substring?;2$IB;17|)
                                              (GO #2#))))))
                                       (LETT |ip|
                                        (PROG1 (QSADD1 |ip|)
                                          (LETT |iw| (+ |iw| 1)
                                           |ISTRING;substring?;2$IB;17|))
                                        |ISTRING;substring?;2$IB;17|)
                                       (GO G190) G191 (EXIT NIL)))
                                     #2# (EXIT #2#))
                                    (EXIT 'T)))))))
           #1# (EXIT #1#))))) 

(DEFUN |ISTRING;position;2$2I;18| (|s| |t| |startpos| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |startpos| (- |startpos| (QREFELT $ 6))
                 |ISTRING;position;2$2I;18|)
           (EXIT (COND
                   ((< |startpos| 0) (|error| "index out of bounds"))
                   ((NULL (< |startpos| (QCSIZE |t|)))
                    (- (QREFELT $ 6) 1))
                   ('T
                    (SEQ (LETT |r| (STRPOS |s| |t| |startpos| NIL)
                               |ISTRING;position;2$2I;18|)
                         (EXIT (COND
                                 ((EQ |r| NIL) (- (QREFELT $ 6) 1))
                                 ('T (+ |r| (QREFELT $ 6))))))))))))) 

(DEFUN |ISTRING;position;C$2I;19| (|c| |t| |startpos| $)
  (PROG (|r| #0=#:G1799 #1=#:G1798)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |startpos| (- |startpos| (QREFELT $ 6))
                            |ISTRING;position;C$2I;19|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((NULL (< |startpos| (QCSIZE |t|)))
                               (- (QREFELT $ 6) 1))
                              ('T
                               (SEQ (SEQ
                                     (LETT |r| |startpos|
                                      |ISTRING;position;C$2I;19|)
                                     (LETT #0#
                                      (QSDIFFERENCE (QCSIZE |t|) 1)
                                      |ISTRING;position;C$2I;19|)
                                     G190
                                     (COND ((> |r| #0#) (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (COND
                                         ((CHAR= (CHAR |t| |r|) |c|)
                                          (PROGN
                                            (LETT #1#
                                             (+ |r| (QREFELT $ 6))
                                             |ISTRING;position;C$2I;19|)
                                            (GO #1#))))))
                                     (LETT |r| (+ |r| 1)
                                      |ISTRING;position;C$2I;19|)
                                     (GO G190) G191 (EXIT NIL))
                                    (EXIT (- (QREFELT $ 6) 1))))))))
           #1# (EXIT #1#))))) 

(DEFUN |ISTRING;position;Cc$2I;20| (|cc| |t| |startpos| $)
  (PROG (|r| #0=#:G1806 #1=#:G1805)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |startpos| (- |startpos| (QREFELT $ 6))
                            |ISTRING;position;Cc$2I;20|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((NULL (< |startpos| (QCSIZE |t|)))
                               (- (QREFELT $ 6) 1))
                              ('T
                               (SEQ (SEQ
                                     (LETT |r| |startpos|
                                      |ISTRING;position;Cc$2I;20|)
                                     (LETT #0#
                                      (QSDIFFERENCE (QCSIZE |t|) 1)
                                      |ISTRING;position;Cc$2I;20|)
                                     G190
                                     (COND ((> |r| #0#) (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (COND
                                         ((SPADCALL (CHAR |t| |r|) |cc|
                                           (QREFELT $ 49))
                                          (PROGN
                                            (LETT #1#
                                             (+ |r| (QREFELT $ 6))
                                             |ISTRING;position;Cc$2I;20|)
                                            (GO #1#))))))
                                     (LETT |r| (+ |r| 1)
                                      |ISTRING;position;Cc$2I;20|)
                                     (GO G190) G191 (EXIT NIL))
                                    (EXIT (- (QREFELT $ 6) 1))))))))
           #1# (EXIT #1#))))) 

(DEFUN |ISTRING;suffix?;2$B;21| (|s| |t| $)
  (PROG (|n| |m|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |t| (QREFELT $ 42))
                 |ISTRING;suffix?;2$B;21|)
           (LETT |m| (SPADCALL |s| (QREFELT $ 42))
                 |ISTRING;suffix?;2$B;21|)
           (EXIT (COND
                   ((< |n| |m|) 'NIL)
                   ('T
                    (SPADCALL |s| |t| (- (+ (QREFELT $ 6) |n|) |m|)
                        (QREFELT $ 46))))))))) 

(DEFUN |ISTRING;split;$CL;22| (|s| |c| $)
  (PROG (|n| |j| |i| |l|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (QREFELT $ 42))
                 |ISTRING;split;$CL;22|)
           (SEQ (LETT |i| (QREFELT $ 6) |ISTRING;split;$CL;22|) G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL
                                 (SPADCALL |s| |i| (QREFELT $ 52)) |c|
                                 (QREFELT $ 53))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;split;$CL;22|) (GO G190)
                G191 (EXIT NIL))
           (LETT |l| (SPADCALL (QREFELT $ 55)) |ISTRING;split;$CL;22|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((< |n| |i|) 'NIL)
                           ('T
                            (SPADCALL
                                (< (LETT |j|
                                    (SPADCALL |c| |s| |i|
                                     (QREFELT $ 48))
                                    |ISTRING;split;$CL;22|)
                                   (QREFELT $ 6))
                                (QREFELT $ 56)))))
                   (GO G191)))
                (SEQ (LETT |l|
                           (SPADCALL
                               (SPADCALL |s|
                                   (SPADCALL |i| (- |j| 1)
                                    (QREFELT $ 20))
                                   (QREFELT $ 21))
                               |l| (QREFELT $ 57))
                           |ISTRING;split;$CL;22|)
                     (EXIT (SEQ (LETT |i| |j| |ISTRING;split;$CL;22|)
                                G190
                                (COND
                                  ((OR (> |i| |n|)
                                    (NULL
                                     (SPADCALL
                                      (SPADCALL |s| |i| (QREFELT $ 52))
                                      |c| (QREFELT $ 53))))
                                   (GO G191)))
                                (SEQ (EXIT 0))
                                (LETT |i| (+ |i| 1)
                                      |ISTRING;split;$CL;22|)
                                (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NULL (< |n| |i|))
              (LETT |l|
                    (SPADCALL
                        (SPADCALL |s| (SPADCALL |i| |n| (QREFELT $ 20))
                            (QREFELT $ 21))
                        |l| (QREFELT $ 57))
                    |ISTRING;split;$CL;22|)))
           (EXIT (SPADCALL |l| (QREFELT $ 58))))))) 

(DEFUN |ISTRING;split;$CcL;23| (|s| |cc| $)
  (PROG (|n| |j| |i| |l|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (QREFELT $ 42))
                 |ISTRING;split;$CcL;23|)
           (SEQ (LETT |i| (QREFELT $ 6) |ISTRING;split;$CcL;23|) G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL
                                 (SPADCALL |s| |i| (QREFELT $ 52)) |cc|
                                 (QREFELT $ 49))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;split;$CcL;23|) (GO G190)
                G191 (EXIT NIL))
           (LETT |l| (SPADCALL (QREFELT $ 55)) |ISTRING;split;$CcL;23|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((< |n| |i|) 'NIL)
                           ('T
                            (SPADCALL
                                (< (LETT |j|
                                    (SPADCALL |cc| |s| |i|
                                     (QREFELT $ 50))
                                    |ISTRING;split;$CcL;23|)
                                   (QREFELT $ 6))
                                (QREFELT $ 56)))))
                   (GO G191)))
                (SEQ (LETT |l|
                           (SPADCALL
                               (SPADCALL |s|
                                   (SPADCALL |i| (- |j| 1)
                                    (QREFELT $ 20))
                                   (QREFELT $ 21))
                               |l| (QREFELT $ 57))
                           |ISTRING;split;$CcL;23|)
                     (EXIT (SEQ (LETT |i| |j| |ISTRING;split;$CcL;23|)
                                G190
                                (COND
                                  ((OR (> |i| |n|)
                                    (NULL
                                     (SPADCALL
                                      (SPADCALL |s| |i| (QREFELT $ 52))
                                      |cc| (QREFELT $ 49))))
                                   (GO G191)))
                                (SEQ (EXIT 0))
                                (LETT |i| (+ |i| 1)
                                      |ISTRING;split;$CcL;23|)
                                (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NULL (< |n| |i|))
              (LETT |l|
                    (SPADCALL
                        (SPADCALL |s| (SPADCALL |i| |n| (QREFELT $ 20))
                            (QREFELT $ 21))
                        |l| (QREFELT $ 57))
                    |ISTRING;split;$CcL;23|)))
           (EXIT (SPADCALL |l| (QREFELT $ 58))))))) 

(DEFUN |ISTRING;leftTrim;$C$;24| (|s| |c| $)
  (PROG (|n| |i|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (QREFELT $ 42))
                 |ISTRING;leftTrim;$C$;24|)
           (SEQ (LETT |i| (QREFELT $ 6) |ISTRING;leftTrim;$C$;24|) G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL
                                 (SPADCALL |s| |i| (QREFELT $ 52)) |c|
                                 (QREFELT $ 53))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;leftTrim;$C$;24|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |s| (SPADCALL |i| |n| (QREFELT $ 20))
                     (QREFELT $ 21))))))) 

(DEFUN |ISTRING;leftTrim;$Cc$;25| (|s| |cc| $)
  (PROG (|n| |i|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (QREFELT $ 42))
                 |ISTRING;leftTrim;$Cc$;25|)
           (SEQ (LETT |i| (QREFELT $ 6) |ISTRING;leftTrim;$Cc$;25|)
                G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL
                                 (SPADCALL |s| |i| (QREFELT $ 52)) |cc|
                                 (QREFELT $ 49))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;leftTrim;$Cc$;25|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |s| (SPADCALL |i| |n| (QREFELT $ 20))
                     (QREFELT $ 21))))))) 

(DEFUN |ISTRING;rightTrim;$C$;26| (|s| |c| $)
  (PROG (|j| #0=#:G1830)
    (RETURN
      (SEQ (SEQ (LETT |j| (SPADCALL |s| (QREFELT $ 42))
                      |ISTRING;rightTrim;$C$;26|)
                (LETT #0# (QREFELT $ 6) |ISTRING;rightTrim;$C$;26|)
                G190
                (COND
                  ((OR (< |j| #0#)
                       (NULL (SPADCALL
                                 (SPADCALL |s| |j| (QREFELT $ 52)) |c|
                                 (QREFELT $ 53))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |j| (+ |j| -1) |ISTRING;rightTrim;$C$;26|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |s|
                     (SPADCALL (SPADCALL |s| (QREFELT $ 28)) |j|
                         (QREFELT $ 20))
                     (QREFELT $ 21))))))) 

(DEFUN |ISTRING;rightTrim;$Cc$;27| (|s| |cc| $)
  (PROG (|j| #0=#:G1834)
    (RETURN
      (SEQ (SEQ (LETT |j| (SPADCALL |s| (QREFELT $ 42))
                      |ISTRING;rightTrim;$Cc$;27|)
                (LETT #0# (QREFELT $ 6) |ISTRING;rightTrim;$Cc$;27|)
                G190
                (COND
                  ((OR (< |j| #0#)
                       (NULL (SPADCALL
                                 (SPADCALL |s| |j| (QREFELT $ 52)) |cc|
                                 (QREFELT $ 49))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |j| (+ |j| -1) |ISTRING;rightTrim;$Cc$;27|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |s|
                     (SPADCALL (SPADCALL |s| (QREFELT $ 28)) |j|
                         (QREFELT $ 20))
                     (QREFELT $ 21))))))) 

(DEFUN |ISTRING;concat;L$;28| (|l| $)
  (PROG (#0=#:G1842 #1=#:G1837 #2=#:G1835 #3=#:G1836 |t| |s| #4=#:G1843
            |i|)
    (RETURN
      (SEQ (LETT |t|
                 (SPADCALL
                     (PROGN
                       (LETT #3# NIL |ISTRING;concat;L$;28|)
                       (SEQ (LETT |s| NIL |ISTRING;concat;L$;28|)
                            (LETT #0# |l| |ISTRING;concat;L$;28|) G190
                            (COND
                              ((OR (ATOM #0#)
                                   (PROGN
                                     (LETT |s| (CAR #0#)
                                      |ISTRING;concat;L$;28|)
                                     NIL))
                               (GO G191)))
                            (SEQ (EXIT (PROGN
                                         (LETT #1#
                                          (SPADCALL |s| (QREFELT $ 13))
                                          |ISTRING;concat;L$;28|)
                                         (COND
                                           (#3#
                                            (LETT #2# (+ #2# #1#)
                                             |ISTRING;concat;L$;28|))
                                           ('T
                                            (PROGN
                                              (LETT #2# #1#
                                               |ISTRING;concat;L$;28|)
                                              (LETT #3# 'T
                                               |ISTRING;concat;L$;28|)))))))
                            (LETT #0# (CDR #0#) |ISTRING;concat;L$;28|)
                            (GO G190) G191 (EXIT NIL))
                       (COND (#3# #2#) ('T 0)))
                     (SPADCALL (QREFELT $ 43)) (QREFELT $ 9))
                 |ISTRING;concat;L$;28|)
           (LETT |i| (QREFELT $ 6) |ISTRING;concat;L$;28|)
           (SEQ (LETT |s| NIL |ISTRING;concat;L$;28|)
                (LETT #4# |l| |ISTRING;concat;L$;28|) G190
                (COND
                  ((OR (ATOM #4#)
                       (PROGN
                         (LETT |s| (CAR #4#) |ISTRING;concat;L$;28|)
                         NIL))
                   (GO G191)))
                (SEQ (SPADCALL |t| |s| |i| (QREFELT $ 66))
                     (EXIT (LETT |i|
                                 (+ |i| (SPADCALL |s| (QREFELT $ 13)))
                                 |ISTRING;concat;L$;28|)))
                (LETT #4# (CDR #4#) |ISTRING;concat;L$;28|) (GO G190)
                G191 (EXIT NIL))
           (EXIT |t|))))) 

(DEFUN |ISTRING;copyInto!;2$I$;29| (|y| |x| |s| $)
  (PROG (|m| |n|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (QREFELT $ 13))
                 |ISTRING;copyInto!;2$I$;29|)
           (LETT |n| (SPADCALL |y| (QREFELT $ 13))
                 |ISTRING;copyInto!;2$I$;29|)
           (LETT |s| (- |s| (QREFELT $ 6)) |ISTRING;copyInto!;2$I$;29|)
           (COND
             ((OR (< |s| 0) (< |n| (+ |s| |m|)))
              (EXIT (|error| "index out of range"))))
           (RPLACSTR |y| |s| |m| |x| 0 |m|) (EXIT |y|))))) 

(DEFUN |ISTRING;elt;$IC;30| (|s| |i| $)
  (COND
    ((OR (< |i| (QREFELT $ 6)) (< (SPADCALL |s| (QREFELT $ 42)) |i|))
     (|error| "index out of range"))
    ('T (CHAR |s| (- |i| (QREFELT $ 6)))))) 

(DEFUN |ISTRING;elt;$Us$;31| (|s| |sg| $)
  (PROG (|l| |h|)
    (RETURN
      (SEQ (LETT |l| (- (SPADCALL |sg| (QREFELT $ 39)) (QREFELT $ 6))
                 |ISTRING;elt;$Us$;31|)
           (LETT |h|
                 (COND
                   ((SPADCALL |sg| (QREFELT $ 40))
                    (- (SPADCALL |sg| (QREFELT $ 41)) (QREFELT $ 6)))
                   ('T (- (SPADCALL |s| (QREFELT $ 42)) (QREFELT $ 6))))
                 |ISTRING;elt;$Us$;31|)
           (COND
             ((OR (< |l| 0)
                  (NULL (< |h| (SPADCALL |s| (QREFELT $ 13)))))
              (EXIT (|error| "index out of bound"))))
           (EXIT (SUBSTRING |s| |l| (MAX 0 (+ (- |h| |l|) 1)))))))) 

(DEFUN |ISTRING;hash;$I;32| (|s| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n| (QCSIZE |s|) |ISTRING;hash;$I;32|)
           (EXIT (COND
                   ((ZEROP |n|) 0)
                   ((EQL |n| 1)
                    (SPADCALL
                        (SPADCALL |s| (QREFELT $ 6) (QREFELT $ 52))
                        (QREFELT $ 68)))
                   ('T
                    (* (* (SPADCALL
                              (SPADCALL |s| (QREFELT $ 6)
                                  (QREFELT $ 52))
                              (QREFELT $ 68))
                          (SPADCALL
                              (SPADCALL |s| (- (+ (QREFELT $ 6) |n|) 1)
                                  (QREFELT $ 52))
                              (QREFELT $ 68)))
                       (SPADCALL
                           (SPADCALL |s|
                               (+ (QREFELT $ 6) (QUOTIENT2 |n| 2))
                               (QREFELT $ 52))
                           (QREFELT $ 68)))))))))) 

(DEFUN |ISTRING;match;2$CNni;33| (|pattern| |target| |wildcard| $)
  (|stringMatch| |pattern| |target| (CHARACTER |wildcard|))) 

(DEFUN |ISTRING;match?;2$CB;34| (|pattern| |target| |dontcare| $)
  (PROG (|n| |m| #0=#:G1857 #1=#:G1859 |s| #2=#:G1860 #3=#:G1868 |i|
             |p| #4=#:G1861 |q|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |n| (SPADCALL |pattern| (QREFELT $ 42))
                            |ISTRING;match?;2$CB;34|)
                      (LETT |p|
                            (PROG1 (LETT #0#
                                    (SPADCALL |dontcare| |pattern|
                                     (LETT |m|
                                      (SPADCALL |pattern|
                                       (QREFELT $ 28))
                                      |ISTRING;match?;2$CB;34|)
                                     (QREFELT $ 48))
                                    |ISTRING;match?;2$CB;34|)
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            |ISTRING;match?;2$CB;34|)
                      (EXIT (COND
                              ((EQL |p| (- |m| 1))
                               (SPADCALL |pattern| |target|
                                   (QREFELT $ 14)))
                              ('T
                               (SEQ (COND
                                      ((NULL (EQL |p| |m|))
                                       (COND
                                         ((NULL
                                           (SPADCALL
                                            (SPADCALL |pattern|
                                             (SPADCALL |m| (- |p| 1)
                                              (QREFELT $ 20))
                                             (QREFELT $ 21))
                                            |target| (QREFELT $ 71)))
                                          (EXIT 'NIL)))))
                                    (LETT |i| |p|
                                     |ISTRING;match?;2$CB;34|)
                                    (LETT |q|
                                     (PROG1
                                      (LETT #1#
                                       (SPADCALL |dontcare| |pattern|
                                        (+ |p| 1) (QREFELT $ 48))
                                       |ISTRING;match?;2$CB;34|)
                                       (|check-subtype| (>= #1# 0)
                                        '(|NonNegativeInteger|) #1#))
                                     |ISTRING;match?;2$CB;34|)
                                    (SEQ G190
                                     (COND
                                       ((NULL
                                         (SPADCALL (EQL |q| (- |m| 1))
                                          (QREFELT $ 56)))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |s|
                                       (SPADCALL |pattern|
                                        (SPADCALL (+ |p| 1) (- |q| 1)
                                         (QREFELT $ 20))
                                        (QREFELT $ 21))
                                       |ISTRING;match?;2$CB;34|)
                                      (LETT |i|
                                       (PROG1
                                        (LETT #2#
                                         (SPADCALL |s| |target| |i|
                                          (QREFELT $ 47))
                                         |ISTRING;match?;2$CB;34|)
                                         (|check-subtype| (>= #2# 0)
                                          '(|NonNegativeInteger|) #2#))
                                       |ISTRING;match?;2$CB;34|)
                                      (EXIT
                                       (COND
                                         ((EQL |i| (- |m| 1))
                                          (PROGN
                                            (LETT #3# 'NIL
                                             |ISTRING;match?;2$CB;34|)
                                            (GO #3#)))
                                         ('T
                                          (SEQ
                                           (LETT |i|
                                            (+ |i|
                                             (SPADCALL |s|
                                              (QREFELT $ 13)))
                                            |ISTRING;match?;2$CB;34|)
                                           (LETT |p| |q|
                                            |ISTRING;match?;2$CB;34|)
                                           (EXIT
                                            (LETT |q|
                                             (PROG1
                                              (LETT #4#
                                               (SPADCALL |dontcare|
                                                |pattern| (+ |q| 1)
                                                (QREFELT $ 48))
                                               |ISTRING;match?;2$CB;34|)
                                               (|check-subtype|
                                                (>= #4# 0)
                                                '(|NonNegativeInteger|)
                                                #4#))
                                             |ISTRING;match?;2$CB;34|)))))))
                                     NIL (GO G190) G191 (EXIT NIL))
                                    (COND
                                      ((NULL (EQL |p| |n|))
                                       (COND
                                         ((NULL
                                           (SPADCALL
                                            (SPADCALL |pattern|
                                             (SPADCALL (+ |p| 1) |n|
                                              (QREFELT $ 20))
                                             (QREFELT $ 21))
                                            |target| (QREFELT $ 51)))
                                          (EXIT 'NIL)))))
                                    (EXIT 'T)))))))
           #3# (EXIT #3#))))) 

(DEFUN |IndexedString| (#0=#:G1875)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1876)
        (RETURN
          (COND
            ((LETT #1#
                   (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                       (HGET |$ConstructorCache| '|IndexedString|)
                       '|domainEqualList|)
                   |IndexedString|)
             (|CDRwithIncrement| #1#))
            ('T
             (UNWIND-PROTECT
               (PROG1 (|IndexedString;| #0#)
                 (LETT #1# T |IndexedString|))
               (COND
                 ((NOT #1#)
                  (HREM |$ConstructorCache| '|IndexedString|))))))))))) 

(DEFUN |IndexedString;| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IndexedString|))
        (LETT |dv$| (LIST '|IndexedString| |dv$1|) . #0#)
        (LETT $ (|newShell| 84) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (OR (AND (|HasCategory| (|Character|)
                                      '(|OrderedSet|))
                                     (|HasCategory| (|Character|)
                                      '(|Evalable| (|Character|))))
                                (AND (|HasCategory| (|Character|)
                                      '(|SetCategory|))
                                     (|HasCategory| (|Character|)
                                      '(|Evalable| (|Character|)))))
                            (OR (|HasCategory| (|Character|)
                                    '(|CoercibleTo| (|OutputForm|)))
                                (AND (|HasCategory| (|Character|)
                                      '(|SetCategory|))
                                     (|HasCategory| (|Character|)
                                      '(|Evalable| (|Character|)))))
                            (|HasCategory| (|Character|)
                                '(|ConvertibleTo| (|InputForm|)))
                            (OR (|HasCategory| (|Character|)
                                    '(|OrderedSet|))
                                (|HasCategory| (|Character|)
                                    '(|SetCategory|)))
                            (|HasCategory| (|Character|)
                                '(|OrderedSet|))
                            (|HasCategory| (|Integer|) '(|OrderedSet|))
                            (|HasCategory| (|Character|)
                                '(|SetCategory|))
                            (AND (|HasCategory| (|Character|)
                                     '(|SetCategory|))
                                 (|HasCategory| (|Character|)
                                     '(|Evalable| (|Character|))))
                            (|HasCategory| (|Character|)
                                '(|CoercibleTo| (|OutputForm|))))) . #0#))
        (|haddProp| |$ConstructorCache| '|IndexedString| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        $)))) 

(MAKEPROP '|IndexedString| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (|Character|) |ISTRING;new;NniC$;1|
             |ISTRING;empty;$;2| (|Boolean|) |ISTRING;empty?;$B;3|
             |ISTRING;#;$Nni;4| |ISTRING;=;2$B;5| |ISTRING;<;2$B;6|
             |ISTRING;concat;3$;7| |ISTRING;copy;2$;8| (|Integer|)
             (|UniversalSegment| 18) (0 . SEGMENT)
             |ISTRING;elt;$Us$;31| (6 . SEGMENT)
             |ISTRING;insert;2$I$;9| (|String|) (|OutputForm|)
             (11 . |outputForm|) |ISTRING;coerce;$Of;10|
             |ISTRING;minIndex;$I;11| (|CharacterClass|)
             (16 . |upperCase|) (20 . |upperCase|) (|Mapping| 8 8)
             (25 . |map!|) |ISTRING;upperCase!;2$;12|
             (31 . |lowerCase|) (35 . |lowerCase|)
             |ISTRING;lowerCase!;2$;13| |ISTRING;latex;$S;14|
             (40 . |lo|) (45 . |hasHi|) (50 . |hi|) (55 . |maxIndex|)
             (60 . |space|) |ISTRING;replace;$Us2$;15|
             |ISTRING;setelt;$I2C;16| |ISTRING;substring?;2$IB;17|
             |ISTRING;position;2$2I;18| |ISTRING;position;C$2I;19|
             (64 . |member?|) |ISTRING;position;Cc$2I;20|
             |ISTRING;suffix?;2$B;21| |ISTRING;elt;$IC;30| (70 . =)
             (|List| $$) (76 . |empty|) (80 . |not|) (85 . |concat|)
             (91 . |reverse!|) (|List| $) |ISTRING;split;$CL;22|
             |ISTRING;split;$CcL;23| |ISTRING;leftTrim;$C$;24|
             |ISTRING;leftTrim;$Cc$;25| |ISTRING;rightTrim;$C$;26|
             |ISTRING;rightTrim;$Cc$;27| |ISTRING;copyInto!;2$I$;29|
             |ISTRING;concat;L$;28| (96 . |ord|) |ISTRING;hash;$I;32|
             |ISTRING;match;2$CNni;33| (101 . |prefix?|)
             |ISTRING;match?;2$CB;34| (|List| 8) (|List| 75)
             (|Equation| 8) (|Mapping| 8 8 8) (|InputForm|)
             (|SingleInteger|) (|Mapping| 11 8) (|Mapping| 11 8 8)
             (|Void|) (|Union| 8 '"failed") (|List| 18))
          '#(~= 107 |upperCase!| 113 |upperCase| 118 |trim| 123 |swap!|
             135 |suffix?| 142 |substring?| 148 |split| 155 |sorted?|
             167 |sort!| 178 |sort| 189 |size?| 200 |setelt| 206
             |select| 220 |sample| 226 |rightTrim| 230 |reverse!| 242
             |reverse| 247 |replace| 252 |removeDuplicates| 259
             |remove| 264 |reduce| 276 |qsetelt!| 297 |qelt| 304
             |prefix?| 310 |position| 316 |parts| 349 |new| 354 |more?|
             360 |minIndex| 366 |min| 371 |merge| 377 |members| 390
             |member?| 395 |maxIndex| 401 |max| 406 |match?| 412
             |match| 419 |map!| 426 |map| 432 |lowerCase!| 445
             |lowerCase| 450 |less?| 455 |leftTrim| 461 |latex| 473
             |insert| 478 |indices| 492 |index?| 497 |hash| 503 |first|
             513 |find| 518 |fill!| 524 |every?| 530 |eval| 536 |eq?|
             562 |entry?| 568 |entries| 574 |empty?| 579 |empty| 584
             |elt| 588 |delete| 613 |count| 625 |copyInto!| 637 |copy|
             644 |convert| 649 |construct| 654 |concat| 659 |coerce|
             682 |any?| 692 >= 698 > 704 = 710 <= 716 < 722 |#| 728)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 5
                    '(0 0 0 0 0 0 0 5 0 0 1 4 0 0 1 2 3 4))
                (CONS '#(|StringAggregate&|
                         |OneDimensionalArrayAggregate&|
                         |FiniteLinearAggregate&| |LinearAggregate&|
                         |IndexedAggregate&| |Collection&|
                         |HomogeneousAggregate&| |OrderedSet&|
                         |Aggregate&| |EltableAggregate&| |Evalable&|
                         |SetCategory&| NIL NIL |InnerEvalable&| NIL
                         NIL |BasicType&|)
                      (CONS '#((|StringAggregate|)
                               (|OneDimensionalArrayAggregate| 8)
                               (|FiniteLinearAggregate| 8)
                               (|LinearAggregate| 8)
                               (|IndexedAggregate| 18 8)
                               (|Collection| 8)
                               (|HomogeneousAggregate| 8)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 18 8) (|Evalable| 8)
                               (|SetCategory|) (|Type|)
                               (|Eltable| 18 8) (|InnerEvalable| 8 8)
                               (|CoercibleTo| 25) (|ConvertibleTo| 77)
                               (|BasicType|))
                            (|makeByteWordVec2| 83
                                '(2 19 0 18 18 20 1 19 0 18 22 1 25 0
                                  24 26 0 29 0 30 1 8 0 0 31 2 0 0 32 0
                                  33 0 29 0 35 1 8 0 0 36 1 19 18 0 39
                                  1 19 11 0 40 1 19 18 0 41 1 0 18 0 42
                                  0 8 0 43 2 29 11 8 0 49 2 8 11 0 0 53
                                  0 54 0 55 1 11 0 0 56 2 54 0 2 0 57 1
                                  54 0 0 58 1 8 7 0 68 2 0 11 0 0 71 2
                                  7 11 0 0 1 1 0 0 0 34 1 0 0 0 1 2 0 0
                                  0 8 1 2 0 0 0 29 1 3 0 81 0 18 18 1 2
                                  0 11 0 0 51 3 0 11 0 0 18 46 2 0 59 0
                                  29 61 2 0 59 0 8 60 1 5 11 0 1 2 0 11
                                  80 0 1 1 5 0 0 1 2 0 0 80 0 1 1 5 0 0
                                  1 2 0 0 80 0 1 2 0 11 0 7 1 3 0 8 0
                                  19 8 1 3 0 8 0 18 8 45 2 0 0 79 0 1 0
                                  0 0 1 2 0 0 0 8 64 2 0 0 0 29 65 1 0
                                  0 0 1 1 0 0 0 1 3 0 0 0 19 0 44 1 7 0
                                  0 1 2 7 0 8 0 1 2 0 0 79 0 1 4 7 8 76
                                  0 8 8 1 3 0 8 76 0 8 1 2 0 8 76 0 1 3
                                  0 8 0 18 8 1 2 0 8 0 18 1 2 0 11 0 0
                                  71 3 7 18 8 0 18 48 2 7 18 8 0 1 3 0
                                  18 29 0 18 50 3 0 18 0 0 18 47 2 0 18
                                  79 0 1 1 0 73 0 1 2 0 0 7 8 9 2 0 11
                                  0 7 1 1 6 18 0 28 2 5 0 0 0 1 2 5 0 0
                                  0 1 3 0 0 80 0 0 1 1 0 73 0 1 2 7 11
                                  8 0 1 1 6 18 0 42 2 5 0 0 0 1 3 0 11
                                  0 0 8 72 3 0 7 0 0 8 70 2 0 0 32 0 33
                                  3 0 0 76 0 0 1 2 0 0 32 0 1 1 0 0 0
                                  37 1 0 0 0 1 2 0 11 0 7 1 2 0 0 0 8
                                  62 2 0 0 0 29 63 1 7 24 0 38 3 0 0 8
                                  0 18 1 3 0 0 0 0 18 23 1 0 83 0 1 2 0
                                  11 18 0 1 1 7 78 0 1 1 0 18 0 69 1 6
                                  8 0 1 2 0 82 79 0 1 2 0 0 0 8 1 2 0
                                  11 79 0 1 3 8 0 0 73 73 1 3 8 0 0 8 8
                                  1 2 8 0 0 74 1 2 8 0 0 75 1 2 0 11 0
                                  0 1 2 7 11 8 0 1 1 0 73 0 1 1 0 11 0
                                  12 0 0 0 10 2 0 0 0 0 1 2 0 0 0 19 21
                                  2 0 8 0 18 52 3 0 8 0 18 8 1 2 0 0 0
                                  18 1 2 0 0 0 19 1 2 7 7 8 0 1 2 0 7
                                  79 0 1 3 0 0 0 0 18 66 1 0 0 0 17 1 3
                                  77 0 1 1 0 0 73 1 1 0 0 59 67 2 0 0 0
                                  0 16 2 0 0 0 8 1 2 0 0 8 0 1 1 9 25 0
                                  27 1 0 0 8 1 2 0 11 79 0 1 2 5 11 0 0
                                  1 2 5 11 0 0 1 2 7 11 0 0 14 2 5 11 0
                                  0 1 2 5 11 0 0 15 1 0 7 0 13)))))
          '|lookupComplete|)) 
