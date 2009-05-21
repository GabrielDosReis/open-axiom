
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Char| |%Shell|)
                    |%Thing|)
                |ISTRING;new;NniC$;1|)) 

(PUT '|ISTRING;new;NniC$;1| '|SPADreplace| 'MAKE-FULL-CVEC) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |ISTRING;empty;$;2|)) 

(PUT '|ISTRING;empty;$;2| '|SPADreplace|
     '(XLAM NIL (MAKE-FULL-CVEC 0))) 

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

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |ISTRING;hash;$I;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Char| |%Shell|)
                    (|%IntegerSection| 0))
                |ISTRING;match;2$CNni;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Char| |%Shell|)
                    |%Boolean|)
                |ISTRING;match?;2$CB;34|)) 

(DEFUN |ISTRING;new;NniC$;1| (|n| |c| $)
  (DECLARE (IGNORE $))
  (MAKE-FULL-CVEC |n| |c|)) 

(DEFUN |ISTRING;empty;$;2| ($)
  (DECLARE (IGNORE $))
  (MAKE-FULL-CVEC 0)) 

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
  (PROG (|l| |m| |n| |h| #0=#:G1439 |r| #1=#:G1537 #2=#:G1538 |i|
             #3=#:G1539 |k|)
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
             ((OR (OR (< |l| 0) (NOT (< |h| |m|))) (< |h| (- |l| 1)))
              (EXIT (|error| "index out of range"))))
           (LETT |r|
                 (MAKE-FULL-CVEC
                     (PROG1 (LETT #0# (+ (- |m| (+ (- |h| |l|) 1)) |n|)
                                  |ISTRING;replace;$Us2$;15|)
                       (|check-subtype| (COND ((< #0# 0) 'NIL) ('T 'T))
                           '(|NonNegativeInteger|) #0#))
                     (SPADCALL (|getShellEntry| $ 52)))
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
         ((OR (< |i| (|getShellEntry| $ 6))
              (< (SPADCALL |s| (|getShellEntry| $ 47)) |i|))
          (|error| "index out of range"))
         ('T
          (SEQ (QESET |s| (- |i| (|getShellEntry| $ 6)) |c|)
               (EXIT |c|)))))) 

(DEFUN |ISTRING;substring?;2$IB;17| (|part| |whole| |startpos| $)
  (PROG (|np| |nw| |iw| |ip| #0=#:G1540 #1=#:G1453 #2=#:G1449)
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
                                           ((NOT
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
      (SEQ (LETT |startpos| (- |startpos| (|getShellEntry| $ 6))
                 |ISTRING;position;2$2I;18|)
           (EXIT (COND
                   ((< |startpos| 0) (|error| "index out of bounds"))
                   ((NOT (< |startpos| (QCSIZE |t|)))
                    (- (|getShellEntry| $ 6) 1))
                   ('T
                    (SEQ (LETT |r| (STRPOS |s| |t| |startpos| NIL)
                               |ISTRING;position;2$2I;18|)
                         (EXIT (COND
                                 ((EQ |r| NIL)
                                  (- (|getShellEntry| $ 6) 1))
                                 ('T (+ |r| (|getShellEntry| $ 6))))))))))))) 

(DEFUN |ISTRING;position;C$2I;19| (|c| |t| |startpos| $)
  (PROG (|r| #0=#:G1541 #1=#:G1463)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |startpos|
                            (- |startpos| (|getShellEntry| $ 6))
                            |ISTRING;position;C$2I;19|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((NOT (< |startpos| (QCSIZE |t|)))
                               (- (|getShellEntry| $ 6) 1))
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
  (PROG (|r| #0=#:G1542 #1=#:G1469)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |startpos|
                            (- |startpos| (|getShellEntry| $ 6))
                            |ISTRING;position;Cc$2I;20|)
                      (EXIT (COND
                              ((< |startpos| 0)
                               (|error| "index out of bounds"))
                              ((NOT (< |startpos| (QCSIZE |t|)))
                               (- (|getShellEntry| $ 6) 1))
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
  (PROG (|n| |m|)
    (RETURN
      (SEQ (LETT |n| (SPADCALL |t| (|getShellEntry| $ 47))
                 |ISTRING;suffix?;2$B;21|)
           (LETT |m| (SPADCALL |s| (|getShellEntry| $ 47))
                 |ISTRING;suffix?;2$B;21|)
           (EXIT (COND
                   ((< |n| |m|) 'NIL)
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
           (LETT |l| (SPADCALL (|getShellEntry| $ 70))
                 |ISTRING;split;$CL;22|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((< |n| |i|) 'NIL)
                           ('T
                            (NOT (< (LETT |j|
                                     (|ISTRING;position;C$2I;19| |c|
                                      |s| |i| $)
                                     |ISTRING;split;$CL;22|)
                                    (|getShellEntry| $ 6))))))
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
             ((NOT (< |n| |i|))
              (LETT |l|
                    (SPADCALL
                        (|ISTRING;elt;$Us$;31| |s|
                            (SPADCALL |i| |n| (|getShellEntry| $ 24))
                            $)
                        |l| (|getShellEntry| $ 71))
                    |ISTRING;split;$CL;22|)))
           (EXIT (SPADCALL |l| (|getShellEntry| $ 72))))))) 

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
           (LETT |l| (SPADCALL (|getShellEntry| $ 70))
                 |ISTRING;split;$CcL;23|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((< |n| |i|) 'NIL)
                           ('T
                            (NOT (< (LETT |j|
                                     (|ISTRING;position;Cc$2I;20| |cc|
                                      |s| |i| $)
                                     |ISTRING;split;$CcL;23|)
                                    (|getShellEntry| $ 6))))))
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
             ((NOT (< |n| |i|))
              (LETT |l|
                    (SPADCALL
                        (|ISTRING;elt;$Us$;31| |s|
                            (SPADCALL |i| |n| (|getShellEntry| $ 24))
                            $)
                        |l| (|getShellEntry| $ 71))
                    |ISTRING;split;$CcL;23|)))
           (EXIT (SPADCALL |l| (|getShellEntry| $ 72))))))) 

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
  (PROG (|j| #0=#:G1543)
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
  (PROG (|j| #0=#:G1544)
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
  (PROG (#0=#:G1545 #1=#:G1498 #2=#:G1496 #3=#:G1497 |t| |s| #4=#:G1546
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
                     (SPADCALL (|getShellEntry| $ 52)))
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
             ((OR (< |s| 0) (< |n| (+ |s| |m|)))
              (EXIT (|error| "index out of range"))))
           (RPLACSTR |y| |s| |m| |x| 0 |m|) (EXIT |y|))))) 

(DEFUN |ISTRING;elt;$IC;30| (|s| |i| $)
  (COND
    ((OR (< |i| (|getShellEntry| $ 6))
         (< (SPADCALL |s| (|getShellEntry| $ 47)) |i|))
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
             ((OR (< |l| 0) (NOT (< |h| (QCSIZE |s|))))
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
                        (|ISTRING;elt;$IC;30| |s| (|getShellEntry| $ 6)
                            $)
                        (|getShellEntry| $ 87)))
                   ('T
                    (* (* (SPADCALL
                              (|ISTRING;elt;$IC;30| |s|
                                  (|getShellEntry| $ 6) $)
                              (|getShellEntry| $ 87))
                          (SPADCALL
                              (|ISTRING;elt;$IC;30| |s|
                                  (- (+ (|getShellEntry| $ 6) |n|) 1)
                                  $)
                              (|getShellEntry| $ 87)))
                       (SPADCALL
                           (|ISTRING;elt;$IC;30| |s|
                               (+ (|getShellEntry| $ 6)
                                  (QUOTIENT2 |n| 2))
                               $)
                           (|getShellEntry| $ 87)))))))))) 

(DEFUN |ISTRING;match;2$CNni;33| (|pattern| |target| |wildcard| $)
  (|stringMatch| |pattern| |target| (CHARACTER |wildcard|))) 

(DEFUN |ISTRING;match?;2$CB;34| (|pattern| |target| |dontcare| $)
  (PROG (|n| |m| #0=#:G1517 #1=#:G1520 |s| #2=#:G1521 #3=#:G1530 |i|
             |p| #4=#:G1522 |q|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |n|
                            (SPADCALL |pattern| (|getShellEntry| $ 47))
                            |ISTRING;match?;2$CB;34|)
                      (LETT |p|
                            (PROG1 (LETT #0#
                                    (|ISTRING;position;C$2I;19|
                                     |dontcare| |pattern|
                                     (LETT |m|
                                      (|ISTRING;minIndex;$I;11|
                                       |pattern| $)
                                      |ISTRING;match?;2$CB;34|)
                                     $)
                                    |ISTRING;match?;2$CB;34|)
                              (|check-subtype|
                                  (COND ((< #0# 0) 'NIL) ('T 'T))
                                  '(|NonNegativeInteger|) #0#))
                            |ISTRING;match?;2$CB;34|)
                      (EXIT (COND
                              ((EQL |p| (- |m| 1))
                               (EQUAL |pattern| |target|))
                              ('T
                               (SEQ (COND
                                      ((SPADCALL |p| |m|
                                        (|getShellEntry| $ 92))
                                       (COND
                                         ((NOT
                                           (SPADCALL
                                            (|ISTRING;elt;$Us$;31|
                                             |pattern|
                                             (SPADCALL |m| (- |p| 1)
                                              (|getShellEntry| $ 24))
                                             $)
                                            |target|
                                            (|getShellEntry| $ 93)))
                                          (EXIT 'NIL)))))
                                    (LETT |i| |p|
                                     |ISTRING;match?;2$CB;34|)
                                    (LETT |q|
                                     (PROG1
                                      (LETT #1#
                                       (|ISTRING;position;C$2I;19|
                                        |dontcare| |pattern| (+ |p| 1)
                                        $)
                                       |ISTRING;match?;2$CB;34|)
                                       (|check-subtype|
                                        (COND
                                          ((< #1# 0) 'NIL)
                                          ('T 'T))
                                        '(|NonNegativeInteger|) #1#))
                                     |ISTRING;match?;2$CB;34|)
                                    (SEQ G190
                                     (COND
                                       ((NULL
                                         (SPADCALL |q| (- |m| 1)
                                          (|getShellEntry| $ 92)))
                                        (GO G191)))
                                     (SEQ
                                      (LETT |s|
                                       (|ISTRING;elt;$Us$;31| |pattern|
                                        (SPADCALL (+ |p| 1) (- |q| 1)
                                         (|getShellEntry| $ 24))
                                        $)
                                       |ISTRING;match?;2$CB;34|)
                                      (LETT |i|
                                       (PROG1
                                        (LETT #2#
                                         (|ISTRING;position;2$2I;18|
                                          |s| |target| |i| $)
                                         |ISTRING;match?;2$CB;34|)
                                         (|check-subtype|
                                          (COND
                                            ((< #2# 0) 'NIL)
                                            ('T 'T))
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
                                            (+ |i| (QCSIZE |s|))
                                            |ISTRING;match?;2$CB;34|)
                                           (LETT |p| |q|
                                            |ISTRING;match?;2$CB;34|)
                                           (EXIT
                                            (LETT |q|
                                             (PROG1
                                              (LETT #4#
                                               (|ISTRING;position;C$2I;19|
                                                |dontcare| |pattern|
                                                (+ |q| 1) $)
                                               |ISTRING;match?;2$CB;34|)
                                               (|check-subtype|
                                                (COND
                                                  ((< #4# 0) 'NIL)
                                                  ('T 'T))
                                                '(|NonNegativeInteger|)
                                                #4#))
                                             |ISTRING;match?;2$CB;34|)))))))
                                     NIL (GO G190) G191 (EXIT NIL))
                                    (COND
                                      ((SPADCALL |p| |n|
                                        (|getShellEntry| $ 92))
                                       (COND
                                         ((NOT
                                           (|ISTRING;suffix?;2$B;21|
                                            (|ISTRING;elt;$Us$;31|
                                             |pattern|
                                             (SPADCALL (+ |p| 1) |n|
                                              (|getShellEntry| $ 24))
                                             $)
                                            |target| $))
                                          (EXIT 'NIL)))))
                                    (EXIT 'T)))))))
           #3# (EXIT #3#))))) 

(DEFUN |IndexedString| (#0=#:G1547)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1548)
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
        (LETT $ (|newShell| 105) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
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
                            (|HasCategory| (|Character|)
                                '(|CoercibleTo| (|OutputForm|)))
                            (AND (|HasCategory| (|Character|)
                                     '(|SetCategory|))
                                 (|HasCategory| (|Character|)
                                     '(|Evalable| (|Character|)))))) . #0#))
        (|haddProp| |$ConstructorCache| '|IndexedString| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        $)))) 

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
             (96 . |One|) (100 . +) (106 . |space|) (|SingleInteger|)
             (110 . |Zero|) (114 . |One|) |ISTRING;replace;$Us2$;15|
             |ISTRING;setelt;$I2C;16| (118 . |false|) (122 . |true|)
             |ISTRING;substring?;2$IB;17| |ISTRING;position;2$2I;18|
             (126 . -) |ISTRING;position;C$2I;19| (132 . |member?|)
             |ISTRING;position;Cc$2I;20| |ISTRING;suffix?;2$B;21|
             |ISTRING;elt;$IC;30| (138 . =) (|List| $$) (144 . |empty|)
             (148 . |concat|) (154 . |reverse!|) (|List| $)
             |ISTRING;split;$CL;22| |ISTRING;split;$CcL;23|
             |ISTRING;leftTrim;$C$;24| |ISTRING;leftTrim;$Cc$;25|
             (159 . -) |ISTRING;rightTrim;$C$;26|
             |ISTRING;rightTrim;$Cc$;27| (164 . +)
             |ISTRING;copyInto!;2$I$;29| |ISTRING;concat;L$;28|
             (170 . |max|) (176 . |zero?|) (181 . |one?|) (186 . |ord|)
             (191 . *) (197 . |quo|) |ISTRING;hash;$I;32|
             |ISTRING;match;2$CNni;33| (203 . ~=) (209 . |prefix?|)
             |ISTRING;match?;2$CB;34| (|List| 8) (|Equation| 8)
             (|List| 96) (|Mapping| 8 8 8) (|InputForm|)
             (|Mapping| 13 8) (|Mapping| 13 8 8) (|Void|)
             (|Union| 8 '"failed") (|List| 11))
          '#(~= 215 |upperCase!| 221 |upperCase| 226 |trim| 231 |swap!|
             243 |suffix?| 250 |substring?| 256 |split| 263 |sorted?|
             275 |sort!| 286 |sort| 297 |size?| 308 |setelt| 314
             |select| 328 |sample| 334 |rightTrim| 338 |reverse!| 350
             |reverse| 355 |replace| 360 |removeDuplicates| 367
             |remove| 372 |reduce| 384 |qsetelt!| 405 |qelt| 412
             |prefix?| 418 |position| 424 |parts| 457 |new| 462 |more?|
             468 |minIndex| 474 |min| 479 |merge| 485 |members| 498
             |member?| 503 |maxIndex| 509 |max| 514 |match?| 520
             |match| 527 |map!| 534 |map| 540 |lowerCase!| 553
             |lowerCase| 558 |less?| 563 |leftTrim| 569 |latex| 581
             |insert| 586 |indices| 600 |index?| 605 |hash| 611 |first|
             621 |find| 626 |fill!| 632 |every?| 638 |eval| 644 |eq?|
             670 |entry?| 676 |entries| 682 |empty?| 687 |empty| 692
             |elt| 696 |delete| 721 |count| 733 |copyInto!| 745 |copy|
             752 |convert| 757 |construct| 762 |concat| 767 |coerce|
             790 |before?| 800 |any?| 806 >= 812 > 818 = 824 <= 830 <
             836 |#| 842)
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
                               (|IndexedAggregate| 11 8)
                               (|Collection| 8)
                               (|HomogeneousAggregate| 8)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 11 8) (|Evalable| 8)
                               (|SetCategory|) (|Type|)
                               (|Eltable| 11 8) (|InnerEvalable| 8 8)
                               (|CoercibleTo| 29) (|ConvertibleTo| 99)
                               (|BasicType|))
                            (|makeByteWordVec2| 104
                                '(0 11 0 12 2 11 13 0 0 14 0 11 0 21 2
                                  11 0 0 0 22 2 23 0 11 11 24 1 23 0 11
                                  26 1 29 0 28 30 0 33 0 34 1 8 0 0 35
                                  2 0 0 36 0 37 0 33 0 39 1 8 0 0 40 2
                                  28 0 0 0 42 1 23 11 0 44 1 23 13 0 45
                                  1 23 11 0 46 1 0 11 0 47 0 7 0 48 2
                                  11 13 0 0 49 0 7 0 50 2 11 0 0 0 51 0
                                  8 0 52 0 53 0 54 0 53 0 55 0 13 0 58
                                  0 13 0 59 2 53 0 0 0 62 2 33 13 8 0
                                  64 2 8 13 0 0 68 0 69 0 70 2 69 0 2 0
                                  71 1 69 0 0 72 1 53 0 0 78 2 7 0 0 0
                                  81 2 11 0 0 0 84 1 11 13 0 85 1 11 13
                                  0 86 1 8 7 0 87 2 11 0 11 0 88 2 11 0
                                  0 0 89 2 11 13 0 0 92 2 0 13 0 0 93 2
                                  7 13 0 0 1 1 0 0 0 38 1 0 0 0 1 2 0 0
                                  0 33 1 2 0 0 0 8 1 3 0 102 0 11 11 1
                                  2 0 13 0 0 66 3 0 13 0 0 11 60 2 0 73
                                  0 8 74 2 0 73 0 33 75 1 5 13 0 1 2 0
                                  13 101 0 1 1 5 0 0 1 2 0 0 101 0 1 1
                                  5 0 0 1 2 0 0 101 0 1 2 0 13 0 7 1 3
                                  0 8 0 23 8 1 3 0 8 0 11 8 57 2 0 0
                                  100 0 1 0 0 0 1 2 0 0 0 33 80 2 0 0 0
                                  8 79 1 0 0 0 1 1 0 0 0 1 3 0 0 0 23 0
                                  56 1 7 0 0 1 2 7 0 8 0 1 2 0 0 100 0
                                  1 4 7 8 98 0 8 8 1 2 0 8 98 0 1 3 0 8
                                  98 0 8 1 3 0 8 0 11 8 1 2 0 8 0 11 1
                                  2 0 13 0 0 93 2 7 11 8 0 1 3 7 11 8 0
                                  11 63 3 0 11 0 0 11 61 3 0 11 33 0 11
                                  65 2 0 11 100 0 1 1 0 95 0 1 2 0 0 7
                                  8 9 2 0 13 0 7 1 1 6 11 0 32 2 5 0 0
                                  0 1 2 5 0 0 0 1 3 0 0 101 0 0 1 1 0
                                  95 0 1 2 7 13 8 0 1 1 6 11 0 47 2 5 0
                                  0 0 1 3 0 13 0 0 8 94 3 0 7 0 0 8 91
                                  2 0 0 36 0 37 3 0 0 98 0 0 1 2 0 0 36
                                  0 1 1 0 0 0 41 1 0 0 0 1 2 0 13 0 7 1
                                  2 0 0 0 33 77 2 0 0 0 8 76 1 7 28 0
                                  43 3 0 0 0 0 11 27 3 0 0 8 0 11 1 1 0
                                  104 0 1 2 0 13 11 0 1 1 7 53 0 1 1 0
                                  11 0 90 1 6 8 0 1 2 0 103 100 0 1 2 0
                                  0 0 8 1 2 0 13 100 0 1 3 9 0 0 95 95
                                  1 3 9 0 0 8 8 1 2 9 0 0 97 1 2 9 0 0
                                  96 1 2 0 13 0 0 1 2 7 13 8 0 1 1 0 95
                                  0 1 1 0 13 0 15 0 0 0 10 2 0 0 0 0 1
                                  2 0 0 0 23 25 2 0 8 0 11 67 3 0 8 0
                                  11 8 1 2 0 0 0 23 1 2 0 0 0 11 1 2 7
                                  7 8 0 1 2 0 7 100 0 1 3 0 0 0 0 11 82
                                  1 0 0 0 20 1 3 99 0 1 1 0 0 95 1 1 0
                                  0 73 83 2 0 0 8 0 1 2 0 0 0 0 19 2 0
                                  0 0 8 1 1 8 29 0 31 1 0 0 8 1 2 7 13
                                  0 0 1 2 0 13 100 0 1 2 5 13 0 0 1 2 5
                                  13 0 0 1 2 7 13 0 0 17 2 5 13 0 0 1 2
                                  5 13 0 0 18 1 0 7 0 16)))))
          '|lookupComplete|)) 
