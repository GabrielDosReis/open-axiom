
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Char| |%Shell|)
                    |%Thing|)
                |ISTRING;new;NniC$;1|)) 

(PUT '|ISTRING;new;NniC$;1| '|SPADreplace| 'MAKE-FULL-CVEC) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |ISTRING;empty;$;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ISTRING;empty?;$B;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |ISTRING;#;$Nni;4|)) 

(PUT '|ISTRING;#;$Nni;4| '|SPADreplace| 'QCSIZE) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ISTRING;=;2$B;5|)) 

(PUT '|ISTRING;=;2$B;5| '|SPADreplace| 'EQUAL) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ISTRING;<;2$B;6|)) 

(PUT '|ISTRING;<;2$B;6| '|SPADreplace|
     '(XLAM (|s| |t|) (CGREATERP |t| |s|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ISTRING;concat;3$;7|)) 

(PUT '|ISTRING;concat;3$;7| '|SPADreplace| 'STRCONC) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ISTRING;copy;2$;8|)) 

(PUT '|ISTRING;copy;2$;8| '|SPADreplace| 'COPY-SEQ) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |ISTRING;insert;2$I$;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ISTRING;coerce;$Of;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |ISTRING;minIndex;$I;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ISTRING;upperCase!;2$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ISTRING;lowerCase!;2$;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%String|)
                |ISTRING;latex;$S;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ISTRING;replace;$Us2$;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Char| |%Shell|)
                    |%Char|)
                |ISTRING;setelt;$I2C;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Boolean|)
                |ISTRING;substring?;2$IB;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Integer|)
                |ISTRING;position;2$2I;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Thing| |%Integer| |%Shell|)
                    |%Integer|)
                |ISTRING;position;C$2I;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Integer|)
                |ISTRING;position;Cc$2I;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ISTRING;suffix?;2$B;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Char| |%Shell|) |%List|)
                |ISTRING;split;$CL;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%List|)
                |ISTRING;split;$CcL;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Char| |%Shell|) |%Thing|)
                |ISTRING;leftTrim;$C$;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ISTRING;leftTrim;$Cc$;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Char| |%Shell|) |%Thing|)
                |ISTRING;rightTrim;$C$;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ISTRING;rightTrim;$Cc$;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |ISTRING;concat;L$;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |ISTRING;copyInto!;2$I$;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Char|)
                |ISTRING;elt;$IC;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ISTRING;elt;$Us$;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Short|)
                |ISTRING;hash;$Si;32|)) 

(PUT '|ISTRING;hash;$Si;32| '|SPADreplace| 'SXHASH) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Char| |%Shell|)
                    (|%IntegerSection| 0))
                |ISTRING;match;2$CNni;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Char| |%Shell|)
                    |%Boolean|)
                |ISTRING;match?;2$CB;34|)) 

(DEFUN |ISTRING;new;NniC$;1| (|n| |c| $)
  (DECLARE (IGNORE $))
  (MAKE-FULL-CVEC |n| |c|)) 

(DEFUN |ISTRING;empty;$;2| ($) (MAKE-FULL-CVEC 0)) 

(DEFUN |ISTRING;empty?;$B;3| (|s| $) (EQL (QCSIZE |s|) 0)) 

(DEFUN |ISTRING;#;$Nni;4| (|s| $) (DECLARE (IGNORE $)) (QCSIZE |s|)) 

(DEFUN |ISTRING;=;2$B;5| (|s| |t| $)
  (DECLARE (IGNORE $))
  (EQUAL |s| |t|)) 

(DEFUN |ISTRING;<;2$B;6| (|s| |t| $)
  (DECLARE (IGNORE $))
  (CGREATERP |t| |s|)) 

(DEFUN |ISTRING;concat;3$;7| (|s| |t| $)
  (DECLARE (IGNORE $))
  (STRCONC |s| |t|)) 

(DEFUN |ISTRING;copy;2$;8| (|s| $)
  (DECLARE (IGNORE $))
  (COPY-SEQ |s|)) 

(DEFUN |ISTRING;insert;2$I$;9| (|s| |t| |i| $)
  (STRCONC (STRCONC (|ISTRING;elt;$Us$;31| |s|
                        (SPADCALL (|getShellEntry| $ 6) (- |i| 1)
                            (|getShellEntry| $ 24))
                        $)
                    |t|)
           (|ISTRING;elt;$Us$;31| |s|
               (SPADCALL |i| (|getShellEntry| $ 26)) $))) 

(DEFUN |ISTRING;coerce;$Of;10| (|s| $)
  (SPADCALL |s| (|getShellEntry| $ 30))) 

(DEFUN |ISTRING;minIndex;$I;11| (|s| $) (|getShellEntry| $ 6)) 

(DEFUN |ISTRING;upperCase!;2$;12| (|s| $)
  (SPADCALL (ELT $ 35) |s| (|getShellEntry| $ 37))) 

(DEFUN |ISTRING;lowerCase!;2$;13| (|s| $)
  (SPADCALL (ELT $ 40) |s| (|getShellEntry| $ 37))) 

(DEFUN |ISTRING;latex;$S;14| (|s| $)
  (STRCONC "\\mbox{``" (STRCONC |s| "''}"))) 

(DEFUN |ISTRING;replace;$Us2$;15| (|s| |sg| |t| $)
  (PROG (|l| |m| |n| |h| |r| #0=#:G1535 #1=#:G1536 |i| #2=#:G1537 |k|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |sg| (|getShellEntry| $ 44))
                    (|getShellEntry| $ 6))
                 |ISTRING;replace;$Us2$;15|)
           (LETT |m| (QCSIZE |s|) |ISTRING;replace;$Us2$;15|)
           (LETT |n| (QCSIZE |t|) |ISTRING;replace;$Us2$;15|)
           (LETT |h|
                 (COND
                   ((SPADCALL |sg| (|getShellEntry| $ 45))
                    (- (SPADCALL |sg| (|getShellEntry| $ 46))
                       (|getShellEntry| $ 6)))
                   ('T
                    (- (SPADCALL |s| (|getShellEntry| $ 47))
                       (|getShellEntry| $ 6))))
                 |ISTRING;replace;$Us2$;15|)
           (COND
             ((OR (OR (< |l| 0) (>= |h| |m|)) (< |h| (- |l| 1)))
              (EXIT (|error| "index out of range"))))
           (LETT |r|
                 (MAKE-FULL-CVEC
                     (LET ((#3=#:G1444
                               (+ (- |m| (+ (- |h| |l|) 1)) |n|)))
                       (|check-subtype| (>= #3# 0)
                           '(|NonNegativeInteger|) #3#))
                     (|spadConstant| $ 53))
                 |ISTRING;replace;$Us2$;15|)
           (LETT |k| 0 |ISTRING;replace;$Us2$;15|)
           (SEQ (LETT |i| 0 |ISTRING;replace;$Us2$;15|)
                (LETT #0# (- |l| 1) |ISTRING;replace;$Us2$;15|) G190
                (COND ((QSGREATERP |i| #0#) (GO G191)))
                (SEQ (QESET |r| |k| (CHAR |s| |i|))
                     (EXIT (LETT |k| (+ |k| 1)
                                 |ISTRING;replace;$Us2$;15|)))
                (SETQ |i| (QSADD1 |i|)) (GO G190) G191 (EXIT NIL))
           (SEQ (LETT |i| 0 |ISTRING;replace;$Us2$;15|)
                (LETT #1# (- |n| 1) |ISTRING;replace;$Us2$;15|) G190
                (COND ((QSGREATERP |i| #1#) (GO G191)))
                (SEQ (QESET |r| |k| (CHAR |t| |i|))
                     (EXIT (LETT |k| (+ |k| 1)
                                 |ISTRING;replace;$Us2$;15|)))
                (SETQ |i| (QSADD1 |i|)) (GO G190) G191 (EXIT NIL))
           (SEQ (LETT |i| (+ |h| 1) |ISTRING;replace;$Us2$;15|)
                (LETT #2# (- |m| 1) |ISTRING;replace;$Us2$;15|) G190
                (COND ((> |i| #2#) (GO G191)))
                (SEQ (QESET |r| |k| (CHAR |s| |i|))
                     (EXIT (LETT |k| (+ |k| 1)
                                 |ISTRING;replace;$Us2$;15|)))
                (SETQ |i| (+ |i| 1)) (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |ISTRING;setelt;$I2C;16| (|s| |i| |c| $)
  (SEQ (COND
         ((OR (< |i| (|getShellEntry| $ 6))
              (> |i| (SPADCALL |s| (|getShellEntry| $ 47))))
          (|error| "index out of range"))
         ('T
          (SEQ (QESET |s| (- |i| (|getShellEntry| $ 6)) |c|)
               (EXIT |c|)))))) 

(DEFUN |ISTRING;substring?;2$IB;17| (|part| |whole| |startpos| $)
  (PROG (|np| |nw| |iw| |ip| #0=#:G1538)
    (RETURN
      (SEQ (LETT |np| (QCSIZE |part|) |ISTRING;substring?;2$IB;17|)
           (LETT |nw| (QCSIZE |whole|) |ISTRING;substring?;2$IB;17|)
           (LETT |startpos| (- |startpos| (|getShellEntry| $ 6))
                 |ISTRING;substring?;2$IB;17|)
           (EXIT (COND
                   ((< |startpos| 0) (|error| "index out of bounds"))
                   ((> |np| (- |nw| |startpos|)) NIL)
                   ('T
                    (SEQ (SEQ (LETT |iw| |startpos|
                                    |ISTRING;substring?;2$IB;17|)
                              (LETT |ip| 0
                                    |ISTRING;substring?;2$IB;17|)
                              (LETT #0# (- |np| 1)
                                    |ISTRING;substring?;2$IB;17|)
                              G190
                              (COND ((QSGREATERP |ip| #0#) (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((NOT
                                        (CHAR= (CHAR |part| |ip|)
                                         (CHAR |whole| |iw|)))
                                       (RETURN-FROM
                                        |ISTRING;substring?;2$IB;17|
                                         NIL)))))
                              (SETQ |ip|
                                    (PROG1 (QSADD1 |ip|)
                                      (SETQ |iw| (+ |iw| 1))))
                              (GO G190) G191 (EXIT NIL))
                         (EXIT T))))))))) 

(DEFUN |ISTRING;position;2$2I;18| (|s| |t| |startpos| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |startpos| (- |startpos| (|getShellEntry| $ 6))
                 |ISTRING;position;2$2I;18|)
           (EXIT (COND
                   ((< |startpos| 0) (|error| "index out of bounds"))
                   ((>= |startpos| (QCSIZE |t|))
                    (- (|getShellEntry| $ 6) 1))
                   ('T
                    (SEQ (LETT |r| (STRPOS |s| |t| |startpos| NIL)
                               |ISTRING;position;2$2I;18|)
                         (EXIT (COND
                                 ((EQ |r| NIL)
                                  (- (|getShellEntry| $ 6) 1))
                                 ('T (+ |r| (|getShellEntry| $ 6))))))))))))) 

(DEFUN |ISTRING;position;C$2I;19| (|c| |t| |startpos| $)
  (PROG (|r| #0=#:G1539)
    (RETURN
      (SEQ (LETT |startpos| (- |startpos| (|getShellEntry| $ 6))
                 |ISTRING;position;C$2I;19|)
           (EXIT (COND
                   ((< |startpos| 0) (|error| "index out of bounds"))
                   ((>= |startpos| (QCSIZE |t|))
                    (- (|getShellEntry| $ 6) 1))
                   ('T
                    (SEQ (SEQ (LETT |r| |startpos|
                                    |ISTRING;position;C$2I;19|)
                              (LETT #0# (- (QCSIZE |t|) 1)
                                    |ISTRING;position;C$2I;19|)
                              G190 (COND ((> |r| #0#) (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((CHAR= (CHAR |t| |r|) |c|)
                                       (RETURN-FROM
                                        |ISTRING;position;C$2I;19|
                                         (+ |r| (|getShellEntry| $ 6)))))))
                              (SETQ |r| (+ |r| 1)) (GO G190) G191
                              (EXIT NIL))
                         (EXIT (- (|getShellEntry| $ 6) 1)))))))))) 

(DEFUN |ISTRING;position;Cc$2I;20| (|cc| |t| |startpos| $)
  (PROG (|r| #0=#:G1540)
    (RETURN
      (SEQ (LETT |startpos| (- |startpos| (|getShellEntry| $ 6))
                 |ISTRING;position;Cc$2I;20|)
           (EXIT (COND
                   ((< |startpos| 0) (|error| "index out of bounds"))
                   ((>= |startpos| (QCSIZE |t|))
                    (- (|getShellEntry| $ 6) 1))
                   ('T
                    (SEQ (SEQ (LETT |r| |startpos|
                                    |ISTRING;position;Cc$2I;20|)
                              (LETT #0# (- (QCSIZE |t|) 1)
                                    |ISTRING;position;Cc$2I;20|)
                              G190 (COND ((> |r| #0#) (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL (CHAR |t| |r|) |cc|
                                        (|getShellEntry| $ 65))
                                       (RETURN-FROM
                                        |ISTRING;position;Cc$2I;20|
                                         (+ |r| (|getShellEntry| $ 6)))))))
                              (SETQ |r| (+ |r| 1)) (GO G190) G191
                              (EXIT NIL))
                         (EXIT (- (|getShellEntry| $ 6) 1)))))))))) 

(DEFUN |ISTRING;suffix?;2$B;21| (|s| |t| $)
  (PROG (|m| |n|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;suffix?;2$B;21|)
           (LETT |n| (SPADCALL |t| (|getShellEntry| $ 47))
                 |ISTRING;suffix?;2$B;21|)
           (EXIT (COND
                   ((> |m| |n|) NIL)
                   ('T
                    (|ISTRING;substring?;2$IB;17| |s| |t|
                        (- (+ (|getShellEntry| $ 6) |n|) |m|) $)))))))) 

(DEFUN |ISTRING;split;$CL;22| (|s| |c| $)
  (PROG (|n| |j| |i| |l|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;split;$CL;22|)
           (LETT |i| (|getShellEntry| $ 6) |ISTRING;split;$CL;22|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| |n|) NIL)
                           ('T
                            (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                |c| (|getShellEntry| $ 69)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |i| (+ |i| 1) |ISTRING;split;$CL;22|)))
                NIL (GO G190) G191 (EXIT NIL))
           (LETT |l| NIL |ISTRING;split;$CL;22|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| |n|) NIL)
                           ('T
                            (>= (LETT |j|
                                      (|ISTRING;position;C$2I;19| |c|
                                       |s| |i| $)
                                      |ISTRING;split;$CL;22|)
                                (|getShellEntry| $ 6)))))
                   (GO G191)))
                (SEQ (LETT |l|
                           (SPADCALL
                               (|ISTRING;elt;$Us$;31| |s|
                                   (SPADCALL |i| (- |j| 1)
                                    (|getShellEntry| $ 24))
                                   $)
                               |l| (|getShellEntry| $ 72))
                           |ISTRING;split;$CL;22|)
                     (LETT |i| |j| |ISTRING;split;$CL;22|)
                     (EXIT (SEQ G190
                                (COND
                                  ((NULL
                                    (COND
                                      ((> |i| |n|) NIL)
                                      ('T
                                       (SPADCALL
                                        (|ISTRING;elt;$IC;30| |s| |i|
                                         $)
                                        |c| (|getShellEntry| $ 69)))))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (LETT |i| (+ |i| 1)
                                       |ISTRING;split;$CL;22|)))
                                NIL (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NOT (> |i| |n|))
              (LETT |l|
                    (SPADCALL
                        (|ISTRING;elt;$Us$;31| |s|
                            (SPADCALL |i| |n| (|getShellEntry| $ 24))
                            $)
                        |l| (|getShellEntry| $ 72))
                    |ISTRING;split;$CL;22|)))
           (EXIT (NREVERSE |l|)))))) 

(DEFUN |ISTRING;split;$CcL;23| (|s| |cc| $)
  (PROG (|n| |j| |i| |l|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;split;$CcL;23|)
           (LETT |i| (|getShellEntry| $ 6) |ISTRING;split;$CcL;23|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| |n|) NIL)
                           ('T
                            (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                |cc| (|getShellEntry| $ 65)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |i| (+ |i| 1) |ISTRING;split;$CcL;23|)))
                NIL (GO G190) G191 (EXIT NIL))
           (LETT |l| NIL |ISTRING;split;$CcL;23|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| |n|) NIL)
                           ('T
                            (>= (LETT |j|
                                      (|ISTRING;position;Cc$2I;20| |cc|
                                       |s| |i| $)
                                      |ISTRING;split;$CcL;23|)
                                (|getShellEntry| $ 6)))))
                   (GO G191)))
                (SEQ (LETT |l|
                           (SPADCALL
                               (|ISTRING;elt;$Us$;31| |s|
                                   (SPADCALL |i| (- |j| 1)
                                    (|getShellEntry| $ 24))
                                   $)
                               |l| (|getShellEntry| $ 72))
                           |ISTRING;split;$CcL;23|)
                     (LETT |i| |j| |ISTRING;split;$CcL;23|)
                     (EXIT (SEQ G190
                                (COND
                                  ((NULL
                                    (COND
                                      ((> |i| |n|) NIL)
                                      ('T
                                       (SPADCALL
                                        (|ISTRING;elt;$IC;30| |s| |i|
                                         $)
                                        |cc| (|getShellEntry| $ 65)))))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (LETT |i| (+ |i| 1)
                                       |ISTRING;split;$CcL;23|)))
                                NIL (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NOT (> |i| |n|))
              (LETT |l|
                    (SPADCALL
                        (|ISTRING;elt;$Us$;31| |s|
                            (SPADCALL |i| |n| (|getShellEntry| $ 24))
                            $)
                        |l| (|getShellEntry| $ 72))
                    |ISTRING;split;$CcL;23|)))
           (EXIT (NREVERSE |l|)))))) 

(DEFUN |ISTRING;leftTrim;$C$;24| (|s| |c| $)
  (PROG (|n| |i|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;leftTrim;$C$;24|)
           (LETT |i| (|getShellEntry| $ 6) |ISTRING;leftTrim;$C$;24|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| |n|) NIL)
                           ('T
                            (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                |c| (|getShellEntry| $ 69)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |i| (+ |i| 1)
                                 |ISTRING;leftTrim;$C$;24|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL |i| |n| (|getShellEntry| $ 24)) $)))))) 

(DEFUN |ISTRING;leftTrim;$Cc$;25| (|s| |cc| $)
  (PROG (|n| |i|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;leftTrim;$Cc$;25|)
           (LETT |i| (|getShellEntry| $ 6) |ISTRING;leftTrim;$Cc$;25|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| |n|) NIL)
                           ('T
                            (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                |cc| (|getShellEntry| $ 65)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |i| (+ |i| 1)
                                 |ISTRING;leftTrim;$Cc$;25|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL |i| |n| (|getShellEntry| $ 24)) $)))))) 

(DEFUN |ISTRING;rightTrim;$C$;26| (|s| |c| $)
  (PROG (|j|)
    (RETURN
      (SEQ (LETT |j| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;rightTrim;$C$;26|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((>= |j| (|getShellEntry| $ 6))
                            (SPADCALL (|ISTRING;elt;$IC;30| |s| |j| $)
                                      |c| (|getShellEntry| $ 69)))
                           ('T NIL)))
                   (GO G191)))
                (SEQ (EXIT (LETT |j| (- |j| 1)
                                 |ISTRING;rightTrim;$C$;26|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL (|ISTRING;minIndex;$I;11| |s| $) |j|
                         (|getShellEntry| $ 24))
                     $)))))) 

(DEFUN |ISTRING;rightTrim;$Cc$;27| (|s| |cc| $)
  (PROG (|j|)
    (RETURN
      (SEQ (LETT |j| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;rightTrim;$Cc$;27|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((>= |j| (|getShellEntry| $ 6))
                            (SPADCALL (|ISTRING;elt;$IC;30| |s| |j| $)
                                      |cc| (|getShellEntry| $ 65)))
                           ('T NIL)))
                   (GO G191)))
                (SEQ (EXIT (LETT |j| (- |j| 1)
                                 |ISTRING;rightTrim;$Cc$;27|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL (|ISTRING;minIndex;$I;11| |s| $) |j|
                         (|getShellEntry| $ 24))
                     $)))))) 

(DEFUN |ISTRING;concat;L$;28| (|l| $)
  (PROG (|t| |s| #0=#:G1542 |i|)
    (RETURN
      (SEQ (LETT |t|
                 (MAKE-FULL-CVEC
                     (LET ((#1=#:G1497 NIL) (#2=#:G1498 T)
                           (#3=#:G1541 |l|))
                       (LOOP
                         (COND
                           ((ATOM #3#) (RETURN (COND (#2# 0) (T #1#))))
                           (T (LET ((|s| (CAR #3#)))
                                (LET ((#4=#:G1496 (QCSIZE |s|)))
                                  (COND
                                    (#2# (SETQ #1# #4#))
                                    (T (SETQ #1# (+ #1# #4#))))
                                  (SETQ #2# NIL)))))
                         (SETQ #3# (CDR #3#))))
                     (|spadConstant| $ 53))
                 |ISTRING;concat;L$;28|)
           (LETT |i| (|getShellEntry| $ 6) |ISTRING;concat;L$;28|)
           (SEQ (LETT |s| NIL |ISTRING;concat;L$;28|)
                (LETT #0# |l| |ISTRING;concat;L$;28|) G190
                (COND
                  ((OR (ATOM #0#) (PROGN (SETQ |s| (CAR #0#)) NIL))
                   (GO G191)))
                (SEQ (|ISTRING;copyInto!;2$I$;29| |t| |s| |i| $)
                     (EXIT (LETT |i| (+ |i| (QCSIZE |s|))
                                 |ISTRING;concat;L$;28|)))
                (SETQ #0# (CDR #0#)) (GO G190) G191 (EXIT NIL))
           (EXIT |t|))))) 

(DEFUN |ISTRING;copyInto!;2$I$;29| (|y| |x| |s| $)
  (PROG (|m| |n|)
    (RETURN
      (SEQ (LETT |m| (QCSIZE |x|) |ISTRING;copyInto!;2$I$;29|)
           (LETT |n| (QCSIZE |y|) |ISTRING;copyInto!;2$I$;29|)
           (LETT |s| (- |s| (|getShellEntry| $ 6))
                 |ISTRING;copyInto!;2$I$;29|)
           (COND
             ((OR (< |s| 0) (> (+ |s| |m|) |n|))
              (EXIT (|error| "index out of range"))))
           (RPLACSTR |y| |s| |m| |x| 0 |m|) (EXIT |y|))))) 

(DEFUN |ISTRING;elt;$IC;30| (|s| |i| $)
  (COND
    ((OR (< |i| (|getShellEntry| $ 6))
         (> |i| (SPADCALL |s| (|getShellEntry| $ 47))))
     (|error| "index out of range"))
    ('T (CHAR |s| (- |i| (|getShellEntry| $ 6)))))) 

(DEFUN |ISTRING;elt;$Us$;31| (|s| |sg| $)
  (PROG (|l| |h|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |sg| (|getShellEntry| $ 44))
                    (|getShellEntry| $ 6))
                 |ISTRING;elt;$Us$;31|)
           (LETT |h|
                 (COND
                   ((SPADCALL |sg| (|getShellEntry| $ 45))
                    (- (SPADCALL |sg| (|getShellEntry| $ 46))
                       (|getShellEntry| $ 6)))
                   ('T
                    (- (SPADCALL |s| (|getShellEntry| $ 47))
                       (|getShellEntry| $ 6))))
                 |ISTRING;elt;$Us$;31|)
           (COND
             ((OR (< |l| 0) (>= |h| (QCSIZE |s|)))
              (EXIT (|error| "index out of bound"))))
           (EXIT (SUBSTRING |s| |l| (MAX 0 (+ (- |h| |l|) 1)))))))) 

(DEFUN |ISTRING;hash;$Si;32| (|s| $)
  (DECLARE (IGNORE $))
  (SXHASH |s|)) 

(DEFUN |ISTRING;match;2$CNni;33| (|pattern| |target| |wildcard| $)
  (|stringMatch| |pattern| |target| (CHARACTER |wildcard|))) 

(DEFUN |ISTRING;match?;2$CB;34| (|pattern| |target| |dontcare| $)
  (PROG (|m| |n| |s| |i| |p| |q|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |pattern| (|getShellEntry| $ 47))
                 |ISTRING;match?;2$CB;34|)
           (LETT |p|
                 (LET ((#0=#:G1525
                           (|ISTRING;position;C$2I;19| |dontcare|
                               |pattern|
                               (LETT |m|
                                     (|ISTRING;minIndex;$I;11|
                                      |pattern| $)
                                     |ISTRING;match?;2$CB;34|)
                               $)))
                   (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|)
                       #0#))
                 |ISTRING;match?;2$CB;34|)
           (EXIT (COND
                   ((EQL |p| (- |m| 1)) (EQUAL |pattern| |target|))
                   ('T
                    (SEQ (COND
                           ((SPADCALL |p| |m| (|getShellEntry| $ 87))
                            (COND
                              ((NOT (SPADCALL
                                     (|ISTRING;elt;$Us$;31| |pattern|
                                      (SPADCALL |m| (- |p| 1)
                                       (|getShellEntry| $ 24))
                                      $)
                                     |target| (|getShellEntry| $ 88)))
                               (EXIT NIL)))))
                         (LETT |i| |p| |ISTRING;match?;2$CB;34|)
                         (LETT |q|
                               (LET ((#1=#:G1526
                                      (|ISTRING;position;C$2I;19|
                                       |dontcare| |pattern| (+ |p| 1)
                                       $)))
                                 (|check-subtype| (>= #1# 0)
                                     '(|NonNegativeInteger|) #1#))
                               |ISTRING;match?;2$CB;34|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL |q| (- |m| 1)
                                        (|getShellEntry| $ 87)))
                                 (GO G191)))
                              (SEQ (LETT |s|
                                    (|ISTRING;elt;$Us$;31| |pattern|
                                     (SPADCALL (+ |p| 1) (- |q| 1)
                                      (|getShellEntry| $ 24))
                                     $)
                                    |ISTRING;match?;2$CB;34|)
                                   (LETT |i|
                                    (LET
                                     ((#2=#:G1527
                                       (|ISTRING;position;2$2I;18| |s|
                                        |target| |i| $)))
                                      (|check-subtype| (>= #2# 0)
                                       '(|NonNegativeInteger|) #2#))
                                    |ISTRING;match?;2$CB;34|)
                                   (EXIT
                                    (COND
                                      ((EQL |i| (- |m| 1))
                                       (RETURN-FROM
                                        |ISTRING;match?;2$CB;34|
                                         NIL))
                                      ('T
                                       (SEQ
                                        (LETT |i| (+ |i| (QCSIZE |s|))
                                         |ISTRING;match?;2$CB;34|)
                                        (LETT |p| |q|
                                         |ISTRING;match?;2$CB;34|)
                                        (EXIT
                                         (LETT |q|
                                          (LET
                                           ((#3=#:G1528
                                             (|ISTRING;position;C$2I;19|
                                              |dontcare| |pattern|
                                              (+ |q| 1) $)))
                                            (|check-subtype| (>= #3# 0)
                                             '(|NonNegativeInteger|)
                                             #3#))
                                          |ISTRING;match?;2$CB;34|)))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (COND
                           ((SPADCALL |p| |n| (|getShellEntry| $ 87))
                            (COND
                              ((NOT (|ISTRING;suffix?;2$B;21|
                                     (|ISTRING;elt;$Us$;31| |pattern|
                                      (SPADCALL (+ |p| 1) |n|
                                       (|getShellEntry| $ 24))
                                      $)
                                     |target| $))
                               (EXIT NIL)))))
                         (EXIT T))))))))) 

(DEFUN |IndexedString| (#0=#:G1543)
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#1=#:G1544)
    (RETURN
      (COND
        ((SETQ #1#
               (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                   (HGET |$ConstructorCache| '|IndexedString|)
                   '|domainEqualList|))
         (|CDRwithIncrement| #1#))
        ('T
         (UNWIND-PROTECT
           (PROG1 (|IndexedString;| #0#) (SETQ #1# T))
           (COND
             ((NOT #1#) (HREM |$ConstructorCache| '|IndexedString|))))))))) 

(DEFUN |IndexedString;| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|IndexedString| |dv$1|)) ($ (|newShell| 100))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (OR (AND (|HasCategory| (|Character|)
                                    '(|OrderedSet|))
                                   (|HasCategory| (|Character|)
                                    (LIST '|Evalable| '(|Character|))))
                              (AND (|HasCategory| (|Character|)
                                    '(|SetCategory|))
                                   (|HasCategory| (|Character|)
                                    (LIST '|Evalable| '(|Character|)))))
                          (OR (|HasCategory| (|Character|)
                                  (LIST '|CoercibleTo| '(|OutputForm|)))
                              (AND (|HasCategory| (|Character|)
                                    '(|SetCategory|))
                                   (|HasCategory| (|Character|)
                                    (LIST '|Evalable| '(|Character|)))))
                          (|HasCategory| (|Character|)
                              (LIST '|ConvertibleTo| '(|InputForm|)))
                          (OR (|HasCategory| (|Character|)
                                  '(|OrderedSet|))
                              (|HasCategory| (|Character|)
                                  '(|SetCategory|)))
                          (|HasCategory| (|Character|) '(|OrderedSet|))
                          (|HasCategory| (|Integer|) '(|OrderedSet|))
                          (|HasCategory| (|Character|)
                              '(|SetCategory|))
                          (|HasCategory| (|Character|)
                              (LIST '|CoercibleTo| '(|OutputForm|)))
                          (AND (|HasCategory| (|Character|)
                                   '(|SetCategory|))
                               (|HasCategory| (|Character|)
                                   (LIST '|Evalable| '(|Character|))))))))
    (DECLARE (SPECIAL |$ConstructorCache|))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|IndexedString| (LIST |dv$1|)
        (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    $)) 

(MAKEPROP '|IndexedString| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|)
             (|NonNegativeInteger|) (|Character|) |ISTRING;new;NniC$;1|
             |ISTRING;empty;$;2| (|Integer|) (0 . |Zero|) (|Boolean|)
             (4 . =) |ISTRING;empty?;$B;3| |ISTRING;#;$Nni;4|
             |ISTRING;=;2$B;5| |ISTRING;<;2$B;6| |ISTRING;concat;3$;7|
             |ISTRING;copy;2$;8| (10 . |One|) (14 . -)
             (|UniversalSegment| 11) (20 . SEGMENT)
             |ISTRING;elt;$Us$;31| (26 . SEGMENT)
             |ISTRING;insert;2$I$;9| (|String|) (|OutputForm|)
             (31 . |outputForm|) |ISTRING;coerce;$Of;10|
             |ISTRING;minIndex;$I;11| (|CharacterClass|)
             (36 . |upperCase|) (40 . |upperCase|) (|Mapping| 8 8)
             (45 . |map!|) |ISTRING;upperCase!;2$;12|
             (51 . |lowerCase|) (55 . |lowerCase|)
             |ISTRING;lowerCase!;2$;13| (60 . |concat|)
             |ISTRING;latex;$S;14| (66 . |lo|) (71 . |hasHi|)
             (76 . |hi|) (81 . |maxIndex|) (86 . |Zero|) (90 . <)
             (96 . >=) (102 . |One|) (106 . +) (112 . |space|)
             (|PositiveInteger|) (116 . |One|) (120 . +)
             |ISTRING;replace;$Us2$;15| (126 . >)
             |ISTRING;setelt;$I2C;16| (132 . |false|) (136 . |true|)
             |ISTRING;substring?;2$IB;17| |ISTRING;position;2$2I;18|
             |ISTRING;position;C$2I;19| (140 . |member?|)
             |ISTRING;position;Cc$2I;20| |ISTRING;suffix?;2$B;21|
             |ISTRING;elt;$IC;30| (146 . =) (|List| $$) (152 . |empty|)
             (156 . |concat|) (162 . |reverse!|) (|List| $)
             |ISTRING;split;$CL;22| |ISTRING;split;$CcL;23|
             |ISTRING;leftTrim;$C$;24| |ISTRING;leftTrim;$Cc$;25|
             |ISTRING;rightTrim;$C$;26| |ISTRING;rightTrim;$Cc$;27|
             |ISTRING;copyInto!;2$I$;29| |ISTRING;concat;L$;28|
             (167 . |max|) (|SingleInteger|) |ISTRING;hash;$Si;32|
             |ISTRING;match;2$CNni;33| (173 . ~=) (179 . |prefix?|)
             |ISTRING;match?;2$CB;34| (|List| 8) (|Equation| 8)
             (|List| 91) (|Mapping| 8 8 8) (|InputForm|)
             (|Mapping| 13 8) (|Mapping| 13 8 8) (|Void|)
             (|Union| 8 '"failed") (|List| 11))
          '#(~= 185 |upperCase!| 191 |upperCase| 196 |trim| 201 |swap!|
             213 |suffix?| 220 |substring?| 226 |split| 233 |sorted?|
             245 |sort!| 256 |sort| 267 |size?| 278 |setelt| 284
             |select| 298 |sample| 304 |rightTrim| 308 |reverse!| 320
             |reverse| 325 |replace| 330 |removeDuplicates| 337
             |remove| 342 |reduce| 354 |qsetelt!| 375 |qelt| 382
             |prefix?| 388 |position| 394 |parts| 427 |new| 432 |more?|
             438 |minIndex| 444 |min| 449 |merge| 455 |members| 468
             |member?| 473 |maxIndex| 479 |max| 484 |match?| 490
             |match| 497 |map!| 504 |map| 510 |lowerCase!| 523
             |lowerCase| 528 |less?| 533 |leftTrim| 539 |latex| 551
             |insert| 556 |indices| 570 |index?| 575 |hash| 581 |first|
             586 |find| 591 |fill!| 597 |every?| 603 |eval| 609 |eq?|
             635 |entry?| 641 |entries| 647 |empty?| 652 |empty| 657
             |elt| 661 |delete| 686 |count| 698 |copyInto!| 710 |copy|
             717 |convert| 722 |construct| 727 |concat| 732 |coerce|
             755 |before?| 765 |any?| 771 >= 777 > 783 = 789 <= 795 <
             801 |#| 807)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 5
                    '(0 0 0 0 0 0 0 0 5 0 0 0 1 4 0 1 2 3 4))
                (CONS '#(|StringAggregate&|
                         |OneDimensionalArrayAggregate&|
                         |FiniteLinearAggregate&| |LinearAggregate&|
                         |IndexedAggregate&| |Collection&|
                         |HomogeneousAggregate&| |EltableAggregate&|
                         |OrderedSet&| NIL |Aggregate&| NIL |Evalable&|
                         |SetCategory&| NIL |InnerEvalable&| NIL NIL
                         |BasicType&|)
                      (CONS '#((|StringAggregate|)
                               (|OneDimensionalArrayAggregate| 8)
                               (|FiniteLinearAggregate| 8)
                               (|LinearAggregate| 8)
                               (|IndexedAggregate| 11 8)
                               (|Collection| 8)
                               (|HomogeneousAggregate| 8)
                               (|EltableAggregate| 11 8) (|OrderedSet|)
                               (|Eltable| 23 $$) (|Aggregate|)
                               (|Eltable| 11 8) (|Evalable| 8)
                               (|SetCategory|) (|Type|)
                               (|InnerEvalable| 8 8) (|CoercibleTo| 29)
                               (|ConvertibleTo| 94) (|BasicType|))
                            (|makeByteWordVec2| 99
                                '(0 11 0 12 2 11 13 0 0 14 0 11 0 21 2
                                  11 0 0 0 22 2 23 0 11 11 24 1 23 0 11
                                  26 1 29 0 28 30 0 33 0 34 1 8 0 0 35
                                  2 0 0 36 0 37 0 33 0 39 1 8 0 0 40 2
                                  28 0 0 0 42 1 23 11 0 44 1 23 13 0 45
                                  1 23 11 0 46 1 0 11 0 47 0 7 0 48 2
                                  11 13 0 0 49 2 11 13 0 0 50 0 7 0 51
                                  2 11 0 0 0 52 0 8 0 53 0 54 0 55 2 7
                                  0 0 0 56 2 11 13 0 0 58 0 13 0 60 0
                                  13 0 61 2 33 13 8 0 65 2 8 13 0 0 69
                                  0 70 0 71 2 70 0 2 0 72 1 70 0 0 73 2
                                  11 0 0 0 83 2 11 13 0 0 87 2 0 13 0 0
                                  88 2 7 13 0 0 1 1 0 0 0 38 1 0 0 0 1
                                  2 0 0 0 8 1 2 0 0 0 33 1 3 0 97 0 11
                                  11 1 2 0 13 0 0 67 3 0 13 0 0 11 62 2
                                  0 74 0 33 76 2 0 74 0 8 75 1 5 13 0 1
                                  2 0 13 96 0 1 1 5 0 0 1 2 0 0 96 0 1
                                  1 5 0 0 1 2 0 0 96 0 1 2 0 13 0 7 1 3
                                  0 8 0 23 8 1 3 0 8 0 11 8 59 2 0 0 95
                                  0 1 0 0 0 1 2 0 0 0 8 79 2 0 0 0 33
                                  80 1 0 0 0 1 1 0 0 0 1 3 0 0 0 23 0
                                  57 1 7 0 0 1 2 7 0 8 0 1 2 0 0 95 0 1
                                  4 7 8 93 0 8 8 1 3 0 8 93 0 8 1 2 0 8
                                  93 0 1 3 0 8 0 11 8 1 2 0 8 0 11 1 2
                                  0 13 0 0 88 3 7 11 8 0 11 64 2 7 11 8
                                  0 1 3 0 11 33 0 11 66 3 0 11 0 0 11
                                  63 2 0 11 95 0 1 1 0 90 0 1 2 0 0 7 8
                                  9 2 0 13 0 7 1 1 6 11 0 32 2 5 0 0 0
                                  1 2 5 0 0 0 1 3 0 0 96 0 0 1 1 0 90 0
                                  1 2 7 13 8 0 1 1 6 11 0 47 2 5 0 0 0
                                  1 3 0 13 0 0 8 89 3 0 7 0 0 8 86 2 0
                                  0 36 0 37 3 0 0 93 0 0 1 2 0 0 36 0 1
                                  1 0 0 0 41 1 0 0 0 1 2 0 13 0 7 1 2 0
                                  0 0 8 77 2 0 0 0 33 78 1 7 28 0 43 3
                                  0 0 8 0 11 1 3 0 0 0 0 11 27 1 0 99 0
                                  1 2 0 13 11 0 1 1 7 84 0 85 1 6 8 0 1
                                  2 0 98 95 0 1 2 0 0 0 8 1 2 0 13 95 0
                                  1 3 9 0 0 90 90 1 3 9 0 0 8 8 1 2 9 0
                                  0 92 1 2 9 0 0 91 1 2 0 13 0 0 1 2 7
                                  13 8 0 1 1 0 90 0 1 1 0 13 0 15 0 0 0
                                  10 2 0 0 0 0 1 2 0 0 0 23 25 2 0 8 0
                                  11 68 3 0 8 0 11 8 1 2 0 0 0 11 1 2 0
                                  0 0 23 1 2 7 7 8 0 1 2 0 7 95 0 1 3 0
                                  0 0 0 11 81 1 0 0 0 20 1 3 94 0 1 1 0
                                  0 90 1 2 0 0 0 0 19 1 0 0 74 82 2 0 0
                                  8 0 1 2 0 0 0 8 1 1 8 29 0 31 1 0 0 8
                                  1 2 7 13 0 0 1 2 0 13 95 0 1 2 5 13 0
                                  0 1 2 5 13 0 0 1 2 7 13 0 0 17 2 5 13
                                  0 0 1 2 5 13 0 0 18 1 0 7 0 16)))))
          '|lookupComplete|)) 
