
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Char| |%Shell|)
                    |%Thing|)
                |ISTRING;new;NniC$;1|)) 

(PUT '|ISTRING;new;NniC$;1| '|SPADreplace| 'MAKE-FULL-CVEC) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |ISTRING;empty;$;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |ISTRING;empty?;$B;3|)) 

(PUT '|ISTRING;empty?;$B;3| '|SPADreplace|
     '(XLAM (|s|) (|%ieq| (|%strlength| |s|) 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |ISTRING;#;$Nni;4|)) 

(PUT '|ISTRING;#;$Nni;4| '|SPADreplace| '|%strlength|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ISTRING;=;2$B;5|)) 

(PUT '|ISTRING;=;2$B;5| '|SPADreplace| '|%streq|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |ISTRING;<;2$B;6|)) 

(PUT '|ISTRING;<;2$B;6| '|SPADreplace| '|%strlt|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |ISTRING;concat;3$;7|)) 

(PUT '|ISTRING;concat;3$;7| '|SPADreplace| '|%strconc|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |ISTRING;copy;2$;8|)) 

(PUT '|ISTRING;copy;2$;8| '|SPADreplace| '|%strcopy|) 

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

(PUT '|ISTRING;latex;$S;14| '|SPADreplace|
     '(XLAM (|s|) (|%strconc| "\\mbox{``" (|%strconc| |s| "''}")))) 

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

(PUT '|ISTRING;hash;$Si;32| '|SPADreplace| '|%hash|) 

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

(DEFUN |ISTRING;empty?;$B;3| (|s| $)
  (DECLARE (IGNORE $))
  (ZEROP (LENGTH |s|))) 

(DEFUN |ISTRING;#;$Nni;4| (|s| $) (DECLARE (IGNORE $)) (LENGTH |s|)) 

(DEFUN |ISTRING;=;2$B;5| (|s| |t| $)
  (DECLARE (IGNORE $))
  (NOT (NULL (STRING= |s| |t|)))) 

(DEFUN |ISTRING;<;2$B;6| (|s| |t| $)
  (DECLARE (IGNORE $))
  (NOT (NULL (STRING< |s| |t|)))) 

(DEFUN |ISTRING;concat;3$;7| (|s| |t| $)
  (DECLARE (IGNORE $))
  (STRCONC |s| |t|)) 

(DEFUN |ISTRING;copy;2$;8| (|s| $)
  (DECLARE (IGNORE $))
  (COPY-SEQ |s|)) 

(DEFUN |ISTRING;insert;2$I$;9| (|s| |t| |i| $)
  (STRCONC (STRCONC (|ISTRING;elt;$Us$;31| |s|
                        (SPADCALL (SVREF $ 6) (- |i| 1)
                            (|getShellEntry| $ 24))
                        $)
                    |t|)
           (|ISTRING;elt;$Us$;31| |s|
               (SPADCALL |i| (|getShellEntry| $ 26)) $))) 

(DEFUN |ISTRING;coerce;$Of;10| (|s| $)
  (SPADCALL |s| (|getShellEntry| $ 30))) 

(DEFUN |ISTRING;minIndex;$I;11| (|s| $) (SVREF $ 6)) 

(DEFUN |ISTRING;upperCase!;2$;12| (|s| $)
  (SPADCALL (ELT $ 35) |s| (|getShellEntry| $ 37))) 

(DEFUN |ISTRING;lowerCase!;2$;13| (|s| $)
  (SPADCALL (ELT $ 40) |s| (|getShellEntry| $ 37))) 

(DEFUN |ISTRING;latex;$S;14| (|s| $)
  (DECLARE (IGNORE $))
  (STRCONC "\\mbox{``" (STRCONC |s| "''}"))) 

(DEFUN |ISTRING;replace;$Us2$;15| (|s| |sg| |t| $)
  (PROG (|r| |k|)
    (RETURN
      (LET ((|l| (- (SPADCALL |sg| (|getShellEntry| $ 44)) (SVREF $ 6)))
            (|m| (LENGTH |s|)) (|n| (LENGTH |t|))
            (|h| (COND
                   ((SPADCALL |sg| (|getShellEntry| $ 45))
                    (- (SPADCALL |sg| (|getShellEntry| $ 46))
                       (SVREF $ 6)))
                   (T (- (SPADCALL |s| (|getShellEntry| $ 47))
                         (SVREF $ 6))))))
        (SEQ (COND
               ((OR (OR (MINUSP |l|) (NOT (< |h| |m|)))
                    (< |h| (- |l| 1)))
                (EXIT (|error| "index out of range"))))
             (LETT |r|
                   (MAKE-FULL-CVEC
                       (LET ((#0=#:G1419
                                 (+ (- |m| (+ (- |h| |l|) 1)) |n|)))
                         (|check-subtype| (NOT (MINUSP #0#))
                             '(|NonNegativeInteger|) #0#))
                       (|spadConstant| $ 53))
                   |ISTRING;replace;$Us2$;15|)
             (LETT |k| 0 |ISTRING;replace;$Us2$;15|)
             (LET ((|i| 0) (#1=#:G1510 (- |l| 1)))
               (LOOP
                 (COND
                   ((> |i| #1#) (RETURN NIL))
                   (T (SEQ (SETF (CHAR |r| |k|) (CHAR |s| |i|))
                           (EXIT (SETQ |k| (+ |k| 1))))))
                 (SETQ |i| (+ |i| 1))))
             (LET ((|i| 0) (#2=#:G1511 (- |n| 1)))
               (LOOP
                 (COND
                   ((> |i| #2#) (RETURN NIL))
                   (T (SEQ (SETF (CHAR |r| |k|) (CHAR |t| |i|))
                           (EXIT (SETQ |k| (+ |k| 1))))))
                 (SETQ |i| (+ |i| 1))))
             (LET ((|i| (+ |h| 1)) (#3=#:G1512 (- |m| 1)))
               (LOOP
                 (COND
                   ((> |i| #3#) (RETURN NIL))
                   (T (SEQ (SETF (CHAR |r| |k|) (CHAR |s| |i|))
                           (EXIT (SETQ |k| (+ |k| 1))))))
                 (SETQ |i| (+ |i| 1))))
             (EXIT |r|)))))) 

(DEFUN |ISTRING;setelt;$I2C;16| (|s| |i| |c| $)
  (SEQ (COND
         ((OR (< |i| (SVREF $ 6))
              (< (SPADCALL |s| (|getShellEntry| $ 47)) |i|))
          (|error| "index out of range"))
         (T (SEQ (SETF (CHAR |s| (- |i| (SVREF $ 6))) |c|) (EXIT |c|)))))) 

(DEFUN |ISTRING;substring?;2$IB;17| (|part| |whole| |startpos| $)
  (LET ((|np| (LENGTH |part|)) (|nw| (LENGTH |whole|)))
    (SEQ (SETQ |startpos| (- |startpos| (SVREF $ 6)))
         (EXIT (COND
                 ((MINUSP |startpos|) (|error| "index out of bounds"))
                 ((< (- |nw| |startpos|) |np|) NIL)
                 (T (SEQ (LET ((|ip| 0) (#0=#:G1513 (- |np| 1))
                               (|iw| |startpos|))
                           (LOOP
                             (COND
                               ((> |ip| #0#) (RETURN NIL))
                               (T (COND
                                    ((NOT
                                      (CHAR= (CHAR |part| |ip|)
                                       (CHAR |whole| |iw|)))
                                     (RETURN-FROM
                                      |ISTRING;substring?;2$IB;17|
                                       NIL)))))
                             (SETQ |ip| (+ |ip| 1))
                             (SETQ |iw| (+ |iw| 1))))
                         (EXIT T)))))))) 

(DEFUN |ISTRING;position;2$2I;18| (|s| |t| |startpos| $)
  (PROG (|r|)
    (RETURN
      (SEQ (SETQ |startpos| (- |startpos| (SVREF $ 6)))
           (EXIT (COND
                   ((MINUSP |startpos|)
                    (|error| "index out of bounds"))
                   ((NOT (< |startpos| (LENGTH |t|)))
                    (- (SVREF $ 6) 1))
                   (T (SEQ (LETT |r| (STRPOS |s| |t| |startpos| NIL)
                                 |ISTRING;position;2$2I;18|)
                           (EXIT (COND
                                   ((EQ |r| NIL) (- (SVREF $ 6) 1))
                                   (T (+ |r| (SVREF $ 6))))))))))))) 

(DEFUN |ISTRING;position;C$2I;19| (|c| |t| |startpos| $)
  (SEQ (SETQ |startpos| (- |startpos| (SVREF $ 6)))
       (EXIT (COND
               ((MINUSP |startpos|) (|error| "index out of bounds"))
               ((NOT (< |startpos| (LENGTH |t|))) (- (SVREF $ 6) 1))
               (T (SEQ (LET ((|r| |startpos|)
                             (#0=#:G1514 (- (LENGTH |t|) 1)))
                         (LOOP
                           (COND
                             ((> |r| #0#) (RETURN NIL))
                             (T (COND
                                  ((CHAR= (CHAR |t| |r|) |c|)
                                   (RETURN-FROM
                                    |ISTRING;position;C$2I;19|
                                     (+ |r| (SVREF $ 6)))))))
                           (SETQ |r| (+ |r| 1))))
                       (EXIT (- (SVREF $ 6) 1)))))))) 

(DEFUN |ISTRING;position;Cc$2I;20| (|cc| |t| |startpos| $)
  (SEQ (SETQ |startpos| (- |startpos| (SVREF $ 6)))
       (EXIT (COND
               ((MINUSP |startpos|) (|error| "index out of bounds"))
               ((NOT (< |startpos| (LENGTH |t|))) (- (SVREF $ 6) 1))
               (T (SEQ (LET ((|r| |startpos|)
                             (#0=#:G1515 (- (LENGTH |t|) 1)))
                         (LOOP
                           (COND
                             ((> |r| #0#) (RETURN NIL))
                             (T (COND
                                  ((SPADCALL (CHAR |t| |r|) |cc|
                                    (|getShellEntry| $ 65))
                                   (RETURN-FROM
                                    |ISTRING;position;Cc$2I;20|
                                     (+ |r| (SVREF $ 6)))))))
                           (SETQ |r| (+ |r| 1))))
                       (EXIT (- (SVREF $ 6) 1)))))))) 

(DEFUN |ISTRING;suffix?;2$B;21| (|s| |t| $)
  (LET ((|m| (SPADCALL |s| (|getShellEntry| $ 47)))
        (|n| (SPADCALL |t| (|getShellEntry| $ 47))))
    (COND
      ((< |n| |m|) NIL)
      (T (|ISTRING;substring?;2$IB;17| |s| |t|
             (- (+ (SVREF $ 6) |n|) |m|) $))))) 

(DEFUN |ISTRING;split;$CL;22| (|s| |c| $)
  (PROG (|l| |j|)
    (RETURN
      (LET ((|n| (SPADCALL |s| (|getShellEntry| $ 47)))
            (|i| (SVREF $ 6)))
        (SEQ (LOOP
               (COND
                 ((NOT (COND
                         ((< |n| |i|) NIL)
                         (T (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                |c| (|getShellEntry| $ 69)))))
                  (RETURN NIL))
                 (T (SETQ |i| (+ |i| 1)))))
             (LETT |l| NIL |ISTRING;split;$CL;22|)
             (LOOP
               (COND
                 ((NOT (COND
                         ((< |n| |i|) NIL)
                         (T (NOT (< (LETT |j|
                                     (|ISTRING;position;C$2I;19| |c|
                                      |s| |i| $)
                                     |ISTRING;split;$CL;22|)
                                    (SVREF $ 6))))))
                  (RETURN NIL))
                 (T (SEQ (SETQ |l|
                               (SPADCALL (|ISTRING;elt;$Us$;31| |s|
                                          (SPADCALL |i| (- |j| 1)
                                           (|getShellEntry| $ 24))
                                          $)
                                         |l| (|getShellEntry| $ 72)))
                         (SETQ |i| |j|)
                         (EXIT (LOOP
                                 (COND
                                   ((NOT
                                     (COND
                                       ((< |n| |i|) NIL)
                                       (T
                                        (SPADCALL
                                         (|ISTRING;elt;$IC;30| |s| |i|
                                          $)
                                         |c| (|getShellEntry| $ 69)))))
                                    (RETURN NIL))
                                   (T (SETQ |i| (+ |i| 1))))))))))
             (COND
               ((NOT (< |n| |i|))
                (SETQ |l|
                      (SPADCALL
                          (|ISTRING;elt;$Us$;31| |s|
                              (SPADCALL |i| |n| (|getShellEntry| $ 24))
                              $)
                          |l| (|getShellEntry| $ 72)))))
             (EXIT (NREVERSE |l|))))))) 

(DEFUN |ISTRING;split;$CcL;23| (|s| |cc| $)
  (PROG (|l| |j|)
    (RETURN
      (LET ((|n| (SPADCALL |s| (|getShellEntry| $ 47)))
            (|i| (SVREF $ 6)))
        (SEQ (LOOP
               (COND
                 ((NOT (COND
                         ((< |n| |i|) NIL)
                         (T (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $)
                                |cc| (|getShellEntry| $ 65)))))
                  (RETURN NIL))
                 (T (SETQ |i| (+ |i| 1)))))
             (LETT |l| NIL |ISTRING;split;$CcL;23|)
             (LOOP
               (COND
                 ((NOT (COND
                         ((< |n| |i|) NIL)
                         (T (NOT (< (LETT |j|
                                     (|ISTRING;position;Cc$2I;20| |cc|
                                      |s| |i| $)
                                     |ISTRING;split;$CcL;23|)
                                    (SVREF $ 6))))))
                  (RETURN NIL))
                 (T (SEQ (SETQ |l|
                               (SPADCALL
                                   (|ISTRING;elt;$Us$;31| |s|
                                    (SPADCALL |i| (- |j| 1)
                                     (|getShellEntry| $ 24))
                                    $)
                                   |l| (|getShellEntry| $ 72)))
                         (SETQ |i| |j|)
                         (EXIT (LOOP
                                 (COND
                                   ((NOT
                                     (COND
                                       ((< |n| |i|) NIL)
                                       (T
                                        (SPADCALL
                                         (|ISTRING;elt;$IC;30| |s| |i|
                                          $)
                                         |cc| (|getShellEntry| $ 65)))))
                                    (RETURN NIL))
                                   (T (SETQ |i| (+ |i| 1))))))))))
             (COND
               ((NOT (< |n| |i|))
                (SETQ |l|
                      (SPADCALL
                          (|ISTRING;elt;$Us$;31| |s|
                              (SPADCALL |i| |n| (|getShellEntry| $ 24))
                              $)
                          |l| (|getShellEntry| $ 72)))))
             (EXIT (NREVERSE |l|))))))) 

(DEFUN |ISTRING;leftTrim;$C$;24| (|s| |c| $)
  (LET ((|n| (SPADCALL |s| (|getShellEntry| $ 47))) (|i| (SVREF $ 6)))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((< |n| |i|) NIL)
                     (T (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $) |c|
                            (|getShellEntry| $ 69)))))
              (RETURN NIL))
             (T (SETQ |i| (+ |i| 1)))))
         (EXIT (|ISTRING;elt;$Us$;31| |s|
                   (SPADCALL |i| |n| (|getShellEntry| $ 24)) $))))) 

(DEFUN |ISTRING;leftTrim;$Cc$;25| (|s| |cc| $)
  (LET ((|n| (SPADCALL |s| (|getShellEntry| $ 47))) (|i| (SVREF $ 6)))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((< |n| |i|) NIL)
                     (T (SPADCALL (|ISTRING;elt;$IC;30| |s| |i| $) |cc|
                            (|getShellEntry| $ 65)))))
              (RETURN NIL))
             (T (SETQ |i| (+ |i| 1)))))
         (EXIT (|ISTRING;elt;$Us$;31| |s|
                   (SPADCALL |i| |n| (|getShellEntry| $ 24)) $))))) 

(DEFUN |ISTRING;rightTrim;$C$;26| (|s| |c| $)
  (LET ((|j| (SPADCALL |s| (|getShellEntry| $ 47))))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((NOT (< |j| (SVREF $ 6)))
                      (SPADCALL (|ISTRING;elt;$IC;30| |s| |j| $) |c|
                          (|getShellEntry| $ 69)))
                     (T NIL)))
              (RETURN NIL))
             (T (SETQ |j| (- |j| 1)))))
         (EXIT (|ISTRING;elt;$Us$;31| |s|
                   (SPADCALL (|ISTRING;minIndex;$I;11| |s| $) |j|
                       (|getShellEntry| $ 24))
                   $))))) 

(DEFUN |ISTRING;rightTrim;$Cc$;27| (|s| |cc| $)
  (LET ((|j| (SPADCALL |s| (|getShellEntry| $ 47))))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((NOT (< |j| (SVREF $ 6)))
                      (SPADCALL (|ISTRING;elt;$IC;30| |s| |j| $) |cc|
                          (|getShellEntry| $ 65)))
                     (T NIL)))
              (RETURN NIL))
             (T (SETQ |j| (- |j| 1)))))
         (EXIT (|ISTRING;elt;$Us$;31| |s|
                   (SPADCALL (|ISTRING;minIndex;$I;11| |s| $) |j|
                       (|getShellEntry| $ 24))
                   $))))) 

(DEFUN |ISTRING;concat;L$;28| (|l| $)
  (LET ((|t| (SPADCALL
                 (LET ((#0=#:G1472 NIL) (#1=#:G1473 T)
                       (#2=#:G1517 |l|))
                   (LOOP
                     (COND
                       ((ATOM #2#) (RETURN (COND (#1# 0) (T #0#))))
                       (T (LET ((|s| (CAR #2#)))
                            (LET ((#3=#:G1471
                                      (SPADCALL |s|
                                       (|getShellEntry| $ 16))))
                              (COND
                                (#1# (SETQ #0# #3#))
                                (T (SETQ #0# (+ #0# #3#))))
                              (SETQ #1# NIL)))))
                     (SETQ #2# (CDR #2#))))
                 (|spadConstant| $ 53) (|getShellEntry| $ 9)))
        (|i| (SVREF $ 6)))
    (SEQ (LET ((#4=#:G1516 |l|))
           (LOOP
             (COND
               ((ATOM #4#) (RETURN NIL))
               (T (LET ((|s| (CAR #4#)))
                    (SEQ (|ISTRING;copyInto!;2$I$;29| |t| |s| |i| $)
                         (EXIT (SETQ |i| (+ |i| (LENGTH |s|))))))))
             (SETQ #4# (CDR #4#))))
         (EXIT |t|)))) 

(DEFUN |ISTRING;copyInto!;2$I$;29| (|y| |x| |s| $)
  (LET ((|m| (SPADCALL |x| (|getShellEntry| $ 16))) (|n| (LENGTH |y|)))
    (SEQ (SETQ |s| (- |s| (SVREF $ 6)))
         (COND
           ((OR (MINUSP |s|) (< |n| (+ |s| |m|)))
            (EXIT (|error| "index out of range"))))
         (RPLACSTR |y| |s| |m| |x| 0 |m|) (EXIT |y|)))) 

(DEFUN |ISTRING;elt;$IC;30| (|s| |i| $)
  (COND
    ((OR (< |i| (SVREF $ 6))
         (< (SPADCALL |s| (|getShellEntry| $ 47)) |i|))
     (|error| "index out of range"))
    (T (CHAR |s| (- |i| (SVREF $ 6)))))) 

(DEFUN |ISTRING;elt;$Us$;31| (|s| |sg| $)
  (LET ((|l| (- (SPADCALL |sg| (|getShellEntry| $ 44)) (SVREF $ 6)))
        (|h| (COND
               ((SPADCALL |sg| (|getShellEntry| $ 45))
                (- (SPADCALL |sg| (|getShellEntry| $ 46)) (SVREF $ 6)))
               (T (- (SPADCALL |s| (|getShellEntry| $ 47)) (SVREF $ 6))))))
    (SEQ (COND
           ((OR (MINUSP |l|) (NOT (< |h| (LENGTH |s|))))
            (EXIT (|error| "index out of bound"))))
         (EXIT (SUBSTRING |s| |l| (MAX 0 (+ (- |h| |l|) 1))))))) 

(DEFUN |ISTRING;hash;$Si;32| (|s| $)
  (DECLARE (IGNORE $))
  (SXHASH |s|)) 

(DEFUN |ISTRING;match;2$CNni;33| (|pattern| |target| |wildcard| $)
  (|stringMatch| |pattern| |target| (CHARACTER |wildcard|))) 

(DEFUN |ISTRING;match?;2$CB;34| (|pattern| |target| |dontcare| $)
  (PROG (|m| |p| |i| |q| |s|)
    (RETURN
      (LET ((|n| (SPADCALL |pattern| (|getShellEntry| $ 47))))
        (SEQ (LETT |p|
                   (LET ((#0=#:G1500
                             (|ISTRING;position;C$2I;19| |dontcare|
                                 |pattern|
                                 (LETT |m|
                                       (|ISTRING;minIndex;$I;11|
                                        |pattern| $)
                                       |ISTRING;match?;2$CB;34|)
                                 $)))
                     (|check-subtype| (NOT (MINUSP #0#))
                         '(|NonNegativeInteger|) #0#))
                   |ISTRING;match?;2$CB;34|)
             (EXIT (COND
                     ((EQL |p| (- |m| 1))
                      (NOT (NULL (STRING= |pattern| |target|))))
                     (T (SEQ (COND
                               ((SPADCALL |p| |m|
                                    (|getShellEntry| $ 87))
                                (COND
                                  ((NOT
                                    (SPADCALL
                                     (|ISTRING;elt;$Us$;31| |pattern|
                                      (SPADCALL |m| (- |p| 1)
                                       (|getShellEntry| $ 24))
                                      $)
                                     |target| (|getShellEntry| $ 88)))
                                   (EXIT NIL)))))
                             (LETT |i| |p| |ISTRING;match?;2$CB;34|)
                             (LETT |q|
                                   (LET
                                    ((#1=#:G1501
                                      (|ISTRING;position;C$2I;19|
                                       |dontcare| |pattern| (+ |p| 1)
                                       $)))
                                     (|check-subtype|
                                      (NOT (MINUSP #1#))
                                      '(|NonNegativeInteger|) #1#))
                                   |ISTRING;match?;2$CB;34|)
                             (LOOP
                               (COND
                                 ((NOT (SPADCALL |q| (- |m| 1)
                                        (|getShellEntry| $ 87)))
                                  (RETURN NIL))
                                 (T (SEQ
                                     (LETT |s|
                                      (|ISTRING;elt;$Us$;31| |pattern|
                                       (SPADCALL (+ |p| 1) (- |q| 1)
                                        (|getShellEntry| $ 24))
                                       $)
                                      |ISTRING;match?;2$CB;34|)
                                     (SETQ |i|
                                      (LET
                                       ((#2=#:G1502
                                         (|ISTRING;position;2$2I;18|
                                          |s| |target| |i| $)))
                                        (|check-subtype|
                                         (NOT (MINUSP #2#))
                                         '(|NonNegativeInteger|) #2#)))
                                     (EXIT
                                      (COND
                                        ((EQL |i| (- |m| 1))
                                         (RETURN-FROM
                                          |ISTRING;match?;2$CB;34|
                                           NIL))
                                        (T
                                         (SEQ
                                          (SETQ |i|
                                           (+ |i| (LENGTH |s|)))
                                          (SETQ |p| |q|)
                                          (EXIT
                                           (SETQ |q|
                                            (LET
                                             ((#3=#:G1503
                                               (|ISTRING;position;C$2I;19|
                                                |dontcare| |pattern|
                                                (+ |q| 1) $)))
                                              (|check-subtype|
                                               (NOT (MINUSP #3#))
                                               '(|NonNegativeInteger|)
                                               #3#))))))))))))
                             (COND
                               ((SPADCALL |p| |n|
                                    (|getShellEntry| $ 87))
                                (COND
                                  ((NOT
                                    (|ISTRING;suffix?;2$B;21|
                                     (|ISTRING;elt;$Us$;31| |pattern|
                                      (SPADCALL (+ |p| 1) |n|
                                       (|getShellEntry| $ 24))
                                      $)
                                     |target| $))
                                   (EXIT NIL)))))
                             (EXIT T)))))))))) 

(DEFUN |IndexedString| (#0=#:G1518)
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#1=#:G1519)
    (RETURN
      (COND
        ((SETQ #1#
               (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                   (HGET |$ConstructorCache| '|IndexedString|)
                   '|domainEqualList|))
         (|CDRwithIncrement| #1#))
        (T (UNWIND-PROTECT
             (PROG1 (|IndexedString;| #0#) (SETQ #1# T))
             (COND
               ((NOT #1#) (HREM |$ConstructorCache| '|IndexedString|))))))))) 

(DEFUN |IndexedString;| (|#1|)
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|IndexedString| |dv$1|)) ($ (|newShell| 100))
         (|pv$| (|buildPredVector| 0 0
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
                          (|HasCategory| (|Character|) '(|OrderedSet|))
                          (OR (|HasCategory| (|Character|)
                                  '(|BasicType|))
                              (|HasCategory| (|Character|)
                                  '(|OrderedSet|))
                              (|HasCategory| (|Character|)
                                  '(|SetCategory|)))
                          (|HasCategory| (|Integer|) '(|OrderedSet|))
                          (|HasCategory| (|Character|)
                              '(|SetCategory|))
                          (|HasCategory| (|Character|)
                              '(|CoercibleTo| (|OutputForm|)))
                          (|HasCategory| (|Character|) '(|BasicType|))
                          (AND (|HasCategory| (|Character|)
                                   '(|SetCategory|))
                               (|HasCategory| (|Character|)
                                   '(|Evalable| (|Character|))))))))
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
             (|Integer|) (0 . |Zero|) |ISTRING;empty;$;2| (|Boolean|)
             (4 . =) |ISTRING;empty?;$B;3| |ISTRING;#;$Nni;4|
             |ISTRING;=;2$B;5| |ISTRING;<;2$B;6| |ISTRING;concat;3$;7|
             |ISTRING;copy;2$;8| (10 . |One|) (14 . -)
             (|UniversalSegment| 10) (20 . SEGMENT)
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
             (|Union| 8 '"failed") (|List| 10))
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
          (CONS (|makeByteWordVec2| 6
                    '(0 0 0 0 0 0 0 5 0 4 5 0 0 0 1 6 0 1 2 3))
                (CONS '#(|StringAggregate&|
                         |OneDimensionalArrayAggregate&|
                         |FiniteLinearAggregate&| |LinearAggregate&|
                         |IndexedAggregate&| |Collection&|
                         |HomogeneousAggregate&| NIL
                         |EltableAggregate&| |SetCategory&|
                         |OrderedType&| NIL |Aggregate&| NIL
                         |Evalable&| |BasicType&| NIL |InnerEvalable&|
                         NIL NIL)
                      (CONS '#((|StringAggregate|)
                               (|OneDimensionalArrayAggregate| 8)
                               (|FiniteLinearAggregate| 8)
                               (|LinearAggregate| 8)
                               (|IndexedAggregate| 10 8)
                               (|Collection| 8)
                               (|HomogeneousAggregate| 8)
                               (|OrderedSet|) (|EltableAggregate| 10 8)
                               (|SetCategory|) (|OrderedType|)
                               (|Eltable| 23 $$) (|Aggregate|)
                               (|Eltable| 10 8) (|Evalable| 8)
                               (|BasicType|) (|Type|)
                               (|InnerEvalable| 8 8) (|CoercibleTo| 29)
                               (|ConvertibleTo| 94))
                            (|makeByteWordVec2| 99
                                '(0 10 0 11 2 10 13 0 0 14 0 10 0 21 2
                                  10 0 0 0 22 2 23 0 10 10 24 1 23 0 10
                                  26 1 29 0 28 30 0 33 0 34 1 8 0 0 35
                                  2 0 0 36 0 37 0 33 0 39 1 8 0 0 40 2
                                  28 0 0 0 42 1 23 10 0 44 1 23 13 0 45
                                  1 23 10 0 46 1 0 10 0 47 0 7 0 48 2
                                  10 13 0 0 49 2 10 13 0 0 50 0 7 0 51
                                  2 10 0 0 0 52 0 8 0 53 0 54 0 55 2 7
                                  0 0 0 56 2 10 13 0 0 58 0 13 0 60 0
                                  13 0 61 2 33 13 8 0 65 2 8 13 0 0 69
                                  0 70 0 71 2 70 0 2 0 72 1 70 0 0 73 2
                                  10 0 0 0 83 2 10 13 0 0 87 2 0 13 0 0
                                  88 2 10 13 0 0 1 1 0 0 0 38 1 0 0 0 1
                                  2 0 0 0 8 1 2 0 0 0 33 1 3 0 97 0 10
                                  10 1 2 0 13 0 0 67 3 0 13 0 0 10 62 2
                                  0 74 0 33 76 2 0 74 0 8 75 1 5 13 0 1
                                  2 0 13 96 0 1 1 5 0 0 1 2 0 0 96 0 1
                                  1 5 0 0 1 2 0 0 96 0 1 2 0 13 0 7 1 3
                                  0 8 0 23 8 1 3 0 8 0 10 8 59 2 0 0 95
                                  0 1 0 0 0 1 2 0 0 0 8 79 2 0 0 0 33
                                  80 1 0 0 0 1 1 0 0 0 1 3 0 0 0 23 0
                                  57 1 8 0 0 1 2 8 0 8 0 1 2 0 0 95 0 1
                                  4 8 8 93 0 8 8 1 3 0 8 93 0 8 1 2 0 8
                                  93 0 1 3 0 8 0 10 8 1 2 0 8 0 10 1 2
                                  0 13 0 0 88 3 8 10 8 0 10 64 2 8 10 8
                                  0 1 3 0 10 33 0 10 66 3 0 10 0 0 10
                                  63 2 0 10 95 0 1 1 0 90 0 1 2 0 0 7 8
                                  9 2 0 13 0 7 1 1 7 10 0 32 2 5 0 0 0
                                  1 2 5 0 0 0 1 3 0 0 96 0 0 1 1 0 90 0
                                  1 2 8 13 8 0 1 1 7 10 0 47 2 5 0 0 0
                                  1 3 0 13 0 0 8 89 3 0 7 0 0 8 86 2 0
                                  0 36 0 37 3 0 0 93 0 0 1 2 0 0 36 0 1
                                  1 0 0 0 41 1 0 0 0 1 2 0 13 0 7 1 2 0
                                  0 0 8 77 2 0 0 0 33 78 1 8 28 0 43 3
                                  0 0 8 0 10 1 3 0 0 0 0 10 27 1 0 99 0
                                  1 2 0 13 10 0 1 1 8 84 0 85 1 7 8 0 1
                                  2 0 98 95 0 1 2 0 0 0 8 1 2 0 13 95 0
                                  1 3 11 0 0 90 90 1 3 11 0 0 8 8 1 2
                                  11 0 0 92 1 2 11 0 0 91 1 2 0 13 0 0
                                  1 2 8 13 8 0 1 1 0 90 0 1 1 0 13 0 15
                                  0 0 0 12 2 0 0 0 0 1 2 0 0 0 23 25 2
                                  0 8 0 10 68 3 0 8 0 10 8 1 2 0 0 0 10
                                  1 2 0 0 0 23 1 2 8 7 8 0 1 2 0 7 95 0
                                  1 3 0 0 0 0 10 81 1 0 0 0 20 1 3 94 0
                                  1 1 0 0 90 1 2 0 0 0 0 19 1 0 0 74 82
                                  2 0 0 8 0 1 2 0 0 0 8 1 1 9 29 0 31 1
                                  0 0 8 1 2 10 13 0 0 1 2 0 13 95 0 1 2
                                  5 13 0 0 1 2 5 13 0 0 1 2 10 13 0 0
                                  17 2 5 13 0 0 1 2 5 13 0 0 18 1 0 7 0
                                  16)))))
          '|lookupComplete|)) 
