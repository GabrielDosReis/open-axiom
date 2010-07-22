
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
  (PROG (|l| |m| |n| |h| |r| #0=#:G1531 #1=#:G1532 |i| #2=#:G1533 |k|)
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
                     (LET ((#3=#:G1440
                               (+ (- |m| (+ (- |h| |l|) 1)) |n|)))
                       (|check-subtype| (>= #3# 0)
                           '(|NonNegativeInteger|) #3#))
                     (|spadConstant| $ 53))
                 |ISTRING;replace;$Us2$;15|)
           (SEQ (LETT |i| 0 |ISTRING;replace;$Us2$;15|)
                (LETT #0# (- |l| 1) |ISTRING;replace;$Us2$;15|)
                (LETT |k| 0 |ISTRING;replace;$Us2$;15|) G190
                (COND ((QSGREATERP |i| #0#) (GO G191)))
                (SEQ (EXIT (QESET |r| |k| (CHAR |s| |i|))))
                (LETT |k|
                      (PROG1 (QSADD1 |k|)
                        (LETT |i| (QSADD1 |i|)
                              |ISTRING;replace;$Us2$;15|))
                      |ISTRING;replace;$Us2$;15|)
                (GO G190) G191 (EXIT NIL))
           (SEQ (LETT |i| 0 |ISTRING;replace;$Us2$;15|)
                (LETT #1# (- |n| 1) |ISTRING;replace;$Us2$;15|)
                (LETT |k| |k| |ISTRING;replace;$Us2$;15|) G190
                (COND ((QSGREATERP |i| #1#) (GO G191)))
                (SEQ (EXIT (QESET |r| |k| (CHAR |t| |i|))))
                (LETT |k|
                      (PROG1 (+ |k| 1)
                        (LETT |i| (QSADD1 |i|)
                              |ISTRING;replace;$Us2$;15|))
                      |ISTRING;replace;$Us2$;15|)
                (GO G190) G191 (EXIT NIL))
           (SEQ (LETT |i| (+ |h| 1) |ISTRING;replace;$Us2$;15|)
                (LETT #2# (- |m| 1) |ISTRING;replace;$Us2$;15|)
                (LETT |k| |k| |ISTRING;replace;$Us2$;15|) G190
                (COND ((> |i| #2#) (GO G191)))
                (SEQ (EXIT (QESET |r| |k| (CHAR |s| |i|))))
                (LETT |k|
                      (PROG1 (+ |k| 1)
                        (LETT |i| (+ |i| 1) |ISTRING;replace;$Us2$;15|))
                      |ISTRING;replace;$Us2$;15|)
                (GO G190) G191 (EXIT NIL))
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
  (PROG (|np| |nw| |iw| |ip| #0=#:G1534 #1=#:G1449 #2=#:G1445)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |np| (QCSIZE |part|)
                            |ISTRING;substring?;2$IB;17|)
                      (LETT |nw| (QCSIZE |whole|)
                            |ISTRING;substring?;2$IB;17|)
                      (LETT |startpos|
                            (- |startpos| (|getShellEntry| $ 6))
                            |ISTRING;substring?;2$IB;17|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((> |np| (- |nw| |startpos|)) NIL)
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
                                           ((NOT
                                             (CHAR= (CHAR |part| |ip|)
                                              (CHAR |whole| |iw|)))
                                            (PROGN
                                              (LETT #2#
                                               (PROGN
                                                 (LETT #1# NIL
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
                                    (EXIT T)))))))
           #1# (EXIT #1#))))) 

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
  (PROG (|r| #0=#:G1535 #1=#:G1459)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |startpos|
                            (- |startpos| (|getShellEntry| $ 6))
                            |ISTRING;position;C$2I;19|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((>= |startpos| (QCSIZE |t|))
                               (- (|getShellEntry| $ 6) 1))
                              ('T
                               (SEQ (SEQ
                                     (LETT |r| |startpos|
                                      |ISTRING;position;C$2I;19|)
                                     (LETT #0# (- (QCSIZE |t|) 1)
                                      |ISTRING;position;C$2I;19|)
                                     G190
                                     (COND ((> |r| #0#) (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (COND
                                         ((CHAR= (CHAR |t| |r|) |c|)
                                          (PROGN
                                            (LETT #1#
                                             (+ |r|
                                              (|getShellEntry| $ 6))
                                             |ISTRING;position;C$2I;19|)
                                            (GO #1#))))))
                                     (LETT |r| (+ |r| 1)
                                      |ISTRING;position;C$2I;19|)
                                     (GO G190) G191 (EXIT NIL))
                                    (EXIT (- (|getShellEntry| $ 6) 1))))))))
           #1# (EXIT #1#))))) 

(DEFUN |ISTRING;position;Cc$2I;20| (|cc| |t| |startpos| $)
  (PROG (|r| #0=#:G1536 #1=#:G1465)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |startpos|
                            (- |startpos| (|getShellEntry| $ 6))
                            |ISTRING;position;Cc$2I;20|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((>= |startpos| (QCSIZE |t|))
                               (- (|getShellEntry| $ 6) 1))
                              ('T
                               (SEQ (SEQ
                                     (LETT |r| |startpos|
                                      |ISTRING;position;Cc$2I;20|)
                                     (LETT #0# (- (QCSIZE |t|) 1)
                                      |ISTRING;position;Cc$2I;20|)
                                     G190
                                     (COND ((> |r| #0#) (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (COND
                                         ((SPADCALL (CHAR |t| |r|) |cc|
                                           (|getShellEntry| $ 64))
                                          (PROGN
                                            (LETT #1#
                                             (+ |r|
                                              (|getShellEntry| $ 6))
                                             |ISTRING;position;Cc$2I;20|)
                                            (GO #1#))))))
                                     (LETT |r| (+ |r| 1)
                                      |ISTRING;position;Cc$2I;20|)
                                     (GO G190) G191 (EXIT NIL))
                                    (EXIT (- (|getShellEntry| $ 6) 1))))))))
           #1# (EXIT #1#))))) 

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
           (SEQ (LETT |i| (|getShellEntry| $ 6) |ISTRING;split;$CL;22|)
                G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                 |c| (|getShellEntry| $ 68))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;split;$CL;22|) (GO G190)
                G191 (EXIT NIL))
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
                               |l| (|getShellEntry| $ 71))
                           |ISTRING;split;$CL;22|)
                     (EXIT (SEQ (LETT |i| |j| |ISTRING;split;$CL;22|)
                                G190
                                (COND
                                  ((OR (> |i| |n|)
                                    (NULL
                                     (SPADCALL
                                      (|ISTRING;elt;$IC;30| |s| |i| $)
                                      |c| (|getShellEntry| $ 68))))
                                   (GO G191)))
                                (SEQ (EXIT 0))
                                (LETT |i| (+ |i| 1)
                                      |ISTRING;split;$CL;22|)
                                (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NOT (> |i| |n|))
              (LETT |l|
                    (SPADCALL
                        (|ISTRING;elt;$Us$;31| |s|
                            (SPADCALL |i| |n| (|getShellEntry| $ 24))
                            $)
                        |l| (|getShellEntry| $ 71))
                    |ISTRING;split;$CL;22|)))
           (EXIT (NREVERSE |l|)))))) 

(DEFUN |ISTRING;split;$CcL;23| (|s| |cc| $)
  (PROG (|n| |j| |i| |l|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;split;$CcL;23|)
           (SEQ (LETT |i| (|getShellEntry| $ 6)
                      |ISTRING;split;$CcL;23|)
                G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                 |cc| (|getShellEntry| $ 64))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;split;$CcL;23|) (GO G190)
                G191 (EXIT NIL))
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
                               |l| (|getShellEntry| $ 71))
                           |ISTRING;split;$CcL;23|)
                     (EXIT (SEQ (LETT |i| |j| |ISTRING;split;$CcL;23|)
                                G190
                                (COND
                                  ((OR (> |i| |n|)
                                    (NULL
                                     (SPADCALL
                                      (|ISTRING;elt;$IC;30| |s| |i| $)
                                      |cc| (|getShellEntry| $ 64))))
                                   (GO G191)))
                                (SEQ (EXIT 0))
                                (LETT |i| (+ |i| 1)
                                      |ISTRING;split;$CcL;23|)
                                (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (COND
             ((NOT (> |i| |n|))
              (LETT |l|
                    (SPADCALL
                        (|ISTRING;elt;$Us$;31| |s|
                            (SPADCALL |i| |n| (|getShellEntry| $ 24))
                            $)
                        |l| (|getShellEntry| $ 71))
                    |ISTRING;split;$CcL;23|)))
           (EXIT (NREVERSE |l|)))))) 

(DEFUN |ISTRING;leftTrim;$C$;24| (|s| |c| $)
  (PROG (|n| |i|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;leftTrim;$C$;24|)
           (SEQ (LETT |i| (|getShellEntry| $ 6)
                      |ISTRING;leftTrim;$C$;24|)
                G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                 |c| (|getShellEntry| $ 68))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;leftTrim;$C$;24|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL |i| |n| (|getShellEntry| $ 24)) $)))))) 

(DEFUN |ISTRING;leftTrim;$Cc$;25| (|s| |cc| $)
  (PROG (|n| |i|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;leftTrim;$Cc$;25|)
           (SEQ (LETT |i| (|getShellEntry| $ 6)
                      |ISTRING;leftTrim;$Cc$;25|)
                G190
                (COND
                  ((OR (> |i| |n|)
                       (NULL (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                 |cc| (|getShellEntry| $ 64))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |i| (+ |i| 1) |ISTRING;leftTrim;$Cc$;25|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL |i| |n| (|getShellEntry| $ 24)) $)))))) 

(DEFUN |ISTRING;rightTrim;$C$;26| (|s| |c| $)
  (PROG (|j| #0=#:G1537)
    (RETURN
      (SEQ (SEQ (LETT |j| (SPADCALL |s| (|getShellEntry| $ 47))
                      |ISTRING;rightTrim;$C$;26|)
                (LETT #0# (|getShellEntry| $ 6)
                      |ISTRING;rightTrim;$C$;26|)
                G190
                (COND
                  ((OR (< |j| #0#)
                       (NULL (SPADCALL (|ISTRING;elt;$IC;30| |s| |j| $)
                                 |c| (|getShellEntry| $ 68))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |j| (+ |j| -1) |ISTRING;rightTrim;$C$;26|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL (|ISTRING;minIndex;$I;11| |s| $) |j|
                         (|getShellEntry| $ 24))
                     $)))))) 

(DEFUN |ISTRING;rightTrim;$Cc$;27| (|s| |cc| $)
  (PROG (|j| #0=#:G1538)
    (RETURN
      (SEQ (SEQ (LETT |j| (SPADCALL |s| (|getShellEntry| $ 47))
                      |ISTRING;rightTrim;$Cc$;27|)
                (LETT #0# (|getShellEntry| $ 6)
                      |ISTRING;rightTrim;$Cc$;27|)
                G190
                (COND
                  ((OR (< |j| #0#)
                       (NULL (SPADCALL (|ISTRING;elt;$IC;30| |s| |j| $)
                                 |cc| (|getShellEntry| $ 64))))
                   (GO G191)))
                (SEQ (EXIT 0))
                (LETT |j| (+ |j| -1) |ISTRING;rightTrim;$Cc$;27|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (|ISTRING;elt;$Us$;31| |s|
                     (SPADCALL (|ISTRING;minIndex;$I;11| |s| $) |j|
                         (|getShellEntry| $ 24))
                     $)))))) 

(DEFUN |ISTRING;concat;L$;28| (|l| $)
  (PROG (#0=#:G1539 #1=#:G1494 #2=#:G1492 #3=#:G1493 |t| |s| #4=#:G1540
            |i|)
    (RETURN
      (SEQ (LETT |t|
                 (MAKE-FULL-CVEC
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
                                         (LETT #1# (QCSIZE |s|)
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
                     (|spadConstant| $ 53))
                 |ISTRING;concat;L$;28|)
           (LETT |i| (|getShellEntry| $ 6) |ISTRING;concat;L$;28|)
           (SEQ (LETT |s| NIL |ISTRING;concat;L$;28|)
                (LETT #4# |l| |ISTRING;concat;L$;28|) G190
                (COND
                  ((OR (ATOM #4#)
                       (PROGN
                         (LETT |s| (CAR #4#) |ISTRING;concat;L$;28|)
                         NIL))
                   (GO G191)))
                (SEQ (|ISTRING;copyInto!;2$I$;29| |t| |s| |i| $)
                     (EXIT (LETT |i| (+ |i| (QCSIZE |s|))
                                 |ISTRING;concat;L$;28|)))
                (LETT #4# (CDR #4#) |ISTRING;concat;L$;28|) (GO G190)
                G191 (EXIT NIL))
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
  (PROG (|n| |m| |s| #0=#:G1520 |i| |p| |q|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |n|
                            (SPADCALL |pattern| (|getShellEntry| $ 47))
                            |ISTRING;match?;2$CB;34|)
                      (LETT |p|
                            (LET ((#1=#:G1521
                                      (|ISTRING;position;C$2I;19|
                                       |dontcare| |pattern|
                                       (LETT |m|
                                        (|ISTRING;minIndex;$I;11|
                                         |pattern| $)
                                        |ISTRING;match?;2$CB;34|)
                                       $)))
                              (|check-subtype| (>= #1# 0)
                                  '(|NonNegativeInteger|) #1#))
                            |ISTRING;match?;2$CB;34|)
                      (EXIT (COND
                              ((EQL |p| (- |m| 1))
                               (EQUAL |pattern| |target|))
                              ('T
                               (SEQ (COND
                                      ((SPADCALL |p| |m|
                                        (|getShellEntry| $ 88))
                                       (COND
                                         ((NOT
                                           (SPADCALL
                                            (|ISTRING;elt;$Us$;31|
                                             |pattern|
                                             (SPADCALL |m| (- |p| 1)
                                              (|getShellEntry| $ 24))
                                             $)
                                            |target|
                                            (|getShellEntry| $ 89)))
                                          (EXIT NIL)))))
                                    (LETT |i| |p|
                                     |ISTRING;match?;2$CB;34|)
                                    (LETT |q|
                                     (LET
                                      ((#2=#:G1522
                                        (|ISTRING;position;C$2I;19|
                                         |dontcare| |pattern| (+ |p| 1)
                                         $)))
                                       (|check-subtype| (>= #2# 0)
                                        '(|NonNegativeInteger|) #2#))
                                     |ISTRING;match?;2$CB;34|)
                                    (SEQ G190
                                     (COND
                                       ((NULL
                                         (SPADCALL |q| (- |m| 1)
                                          (|getShellEntry| $ 88)))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |s|
                                       (|ISTRING;elt;$Us$;31| |pattern|
                                        (SPADCALL (+ |p| 1) (- |q| 1)
                                         (|getShellEntry| $ 24))
                                        $)
                                       |ISTRING;match?;2$CB;34|)
                                      (LETT |i|
                                       (LET
                                        ((#3=#:G1523
                                          (|ISTRING;position;2$2I;18|
                                           |s| |target| |i| $)))
                                         (|check-subtype| (>= #3# 0)
                                          '(|NonNegativeInteger|) #3#))
                                       |ISTRING;match?;2$CB;34|)
                                      (EXIT
                                       (COND
                                         ((EQL |i| (- |m| 1))
                                          (PROGN
                                            (LETT #0# NIL
                                             |ISTRING;match?;2$CB;34|)
                                            (GO #0#)))
                                         ('T
                                          (SEQ
                                           (LETT |i|
                                            (+ |i| (QCSIZE |s|))
                                            |ISTRING;match?;2$CB;34|)
                                           (LETT |p| |q|
                                            |ISTRING;match?;2$CB;34|)
                                           (EXIT
                                            (LETT |q|
                                             (LET
                                              ((#4=#:G1524
                                                (|ISTRING;position;C$2I;19|
                                                 |dontcare| |pattern|
                                                 (+ |q| 1) $)))
                                               (|check-subtype|
                                                (>= #4# 0)
                                                '(|NonNegativeInteger|)
                                                #4#))
                                             |ISTRING;match?;2$CB;34|)))))))
                                     NIL (GO G190) G191 (EXIT NIL))
                                    (COND
                                      ((SPADCALL |p| |n|
                                        (|getShellEntry| $ 88))
                                       (COND
                                         ((NOT
                                           (|ISTRING;suffix?;2$B;21|
                                            (|ISTRING;elt;$Us$;31|
                                             |pattern|
                                             (SPADCALL (+ |p| 1) |n|
                                              (|getShellEntry| $ 24))
                                             $)
                                            |target| $))
                                          (EXIT NIL)))))
                                    (EXIT T)))))))
           #0# (EXIT #0#))))) 

(DEFUN |IndexedString| (#0=#:G1541)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1542)
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
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|IndexedString| |dv$1|)) ($ (|newShell| 101))
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
             (|PositiveInteger|) (116 . |One|)
             |ISTRING;replace;$Us2$;15| (120 . >)
             |ISTRING;setelt;$I2C;16| (126 . |false|) (130 . |true|)
             |ISTRING;substring?;2$IB;17| |ISTRING;position;2$2I;18|
             |ISTRING;position;C$2I;19| (134 . |member?|)
             |ISTRING;position;Cc$2I;20| |ISTRING;suffix?;2$B;21|
             |ISTRING;elt;$IC;30| (140 . =) (|List| $$) (146 . |empty|)
             (150 . |concat|) (156 . |reverse!|) (|List| $)
             |ISTRING;split;$CL;22| |ISTRING;split;$CcL;23|
             |ISTRING;leftTrim;$C$;24| |ISTRING;leftTrim;$Cc$;25|
             (161 . -) |ISTRING;rightTrim;$C$;26|
             |ISTRING;rightTrim;$Cc$;27| (166 . +)
             |ISTRING;copyInto!;2$I$;29| |ISTRING;concat;L$;28|
             (172 . |max|) (|SingleInteger|) |ISTRING;hash;$Si;32|
             |ISTRING;match;2$CNni;33| (178 . ~=) (184 . |prefix?|)
             |ISTRING;match?;2$CB;34| (|List| 8) (|Equation| 8)
             (|List| 92) (|Mapping| 8 8 8) (|InputForm|)
             (|Mapping| 13 8) (|Mapping| 13 8 8) (|Void|)
             (|Union| 8 '"failed") (|List| 11))
          '#(~= 190 |upperCase!| 196 |upperCase| 201 |trim| 206 |swap!|
             218 |suffix?| 225 |substring?| 231 |split| 238 |sorted?|
             250 |sort!| 261 |sort| 272 |size?| 283 |setelt| 289
             |select| 303 |sample| 309 |rightTrim| 313 |reverse!| 325
             |reverse| 330 |replace| 335 |removeDuplicates| 342
             |remove| 347 |reduce| 359 |qsetelt!| 380 |qelt| 387
             |prefix?| 393 |position| 399 |parts| 432 |new| 437 |more?|
             443 |minIndex| 449 |min| 454 |merge| 460 |members| 473
             |member?| 478 |maxIndex| 484 |max| 489 |match?| 495
             |match| 502 |map!| 509 |map| 515 |lowerCase!| 528
             |lowerCase| 533 |less?| 538 |leftTrim| 544 |latex| 556
             |insert| 561 |indices| 575 |index?| 580 |hash| 586 |first|
             591 |find| 596 |fill!| 602 |every?| 608 |eval| 614 |eq?|
             640 |entry?| 646 |entries| 652 |empty?| 657 |empty| 662
             |elt| 666 |delete| 691 |count| 703 |copyInto!| 715 |copy|
             722 |convert| 727 |construct| 732 |concat| 737 |coerce|
             760 |before?| 770 |any?| 776 >= 782 > 788 = 794 <= 800 <
             806 |#| 812)
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
                               (|ConvertibleTo| 95) (|BasicType|))
                            (|makeByteWordVec2| 100
                                '(0 11 0 12 2 11 13 0 0 14 0 11 0 21 2
                                  11 0 0 0 22 2 23 0 11 11 24 1 23 0 11
                                  26 1 29 0 28 30 0 33 0 34 1 8 0 0 35
                                  2 0 0 36 0 37 0 33 0 39 1 8 0 0 40 2
                                  28 0 0 0 42 1 23 11 0 44 1 23 13 0 45
                                  1 23 11 0 46 1 0 11 0 47 0 7 0 48 2
                                  11 13 0 0 49 2 11 13 0 0 50 0 7 0 51
                                  2 11 0 0 0 52 0 8 0 53 0 54 0 55 2 11
                                  13 0 0 57 0 13 0 59 0 13 0 60 2 33 13
                                  8 0 64 2 8 13 0 0 68 0 69 0 70 2 69 0
                                  2 0 71 1 69 0 0 72 1 11 0 0 78 2 7 0
                                  0 0 81 2 11 0 0 0 84 2 11 13 0 0 88 2
                                  0 13 0 0 89 2 7 13 0 0 1 1 0 0 0 38 1
                                  0 0 0 1 2 0 0 0 8 1 2 0 0 0 33 1 3 0
                                  98 0 11 11 1 2 0 13 0 0 66 3 0 13 0 0
                                  11 61 2 0 73 0 33 75 2 0 73 0 8 74 1
                                  5 13 0 1 2 0 13 97 0 1 1 5 0 0 1 2 0
                                  0 97 0 1 1 5 0 0 1 2 0 0 97 0 1 2 0
                                  13 0 7 1 3 0 8 0 23 8 1 3 0 8 0 11 8
                                  58 2 0 0 96 0 1 0 0 0 1 2 0 0 0 8 79
                                  2 0 0 0 33 80 1 0 0 0 1 1 0 0 0 1 3 0
                                  0 0 23 0 56 1 7 0 0 1 2 7 0 8 0 1 2 0
                                  0 96 0 1 4 7 8 94 0 8 8 1 3 0 8 94 0
                                  8 1 2 0 8 94 0 1 3 0 8 0 11 8 1 2 0 8
                                  0 11 1 2 0 13 0 0 89 3 7 11 8 0 11 63
                                  2 7 11 8 0 1 3 0 11 33 0 11 65 3 0 11
                                  0 0 11 62 2 0 11 96 0 1 1 0 91 0 1 2
                                  0 0 7 8 9 2 0 13 0 7 1 1 6 11 0 32 2
                                  5 0 0 0 1 2 5 0 0 0 1 3 0 0 97 0 0 1
                                  1 0 91 0 1 2 7 13 8 0 1 1 6 11 0 47 2
                                  5 0 0 0 1 3 0 13 0 0 8 90 3 0 7 0 0 8
                                  87 2 0 0 36 0 37 3 0 0 94 0 0 1 2 0 0
                                  36 0 1 1 0 0 0 41 1 0 0 0 1 2 0 13 0
                                  7 1 2 0 0 0 8 76 2 0 0 0 33 77 1 7 28
                                  0 43 3 0 0 8 0 11 1 3 0 0 0 0 11 27 1
                                  0 100 0 1 2 0 13 11 0 1 1 7 85 0 86 1
                                  6 8 0 1 2 0 99 96 0 1 2 0 0 0 8 1 2 0
                                  13 96 0 1 3 9 0 0 91 91 1 3 9 0 0 8 8
                                  1 2 9 0 0 93 1 2 9 0 0 92 1 2 0 13 0
                                  0 1 2 7 13 8 0 1 1 0 91 0 1 1 0 13 0
                                  15 0 0 0 10 2 0 0 0 0 1 2 0 0 0 23 25
                                  2 0 8 0 11 67 3 0 8 0 11 8 1 2 0 0 0
                                  11 1 2 0 0 0 23 1 2 7 7 8 0 1 2 0 7
                                  96 0 1 3 0 0 0 0 11 82 1 0 0 0 20 1 3
                                  95 0 1 1 0 0 91 1 2 0 0 0 0 19 1 0 0
                                  73 83 2 0 0 8 0 1 2 0 0 0 8 1 1 8 29
                                  0 31 1 0 0 8 1 2 7 13 0 0 1 2 0 13 96
                                  0 1 2 5 13 0 0 1 2 5 13 0 0 1 2 7 13
                                  0 0 17 2 5 13 0 0 1 2 5 13 0 0 18 1 0
                                  7 0 16)))))
          '|lookupComplete|)) 