
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) (|%IntegerSection| 0))
                |ILIST;#;$Nni;1|)) 

(PUT '|ILIST;#;$Nni;1| '|SPADreplace| 'LENGTH) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%List|)
                |ILIST;concat;S2$;2|)) 

(PUT '|ILIST;concat;S2$;2| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%Boolean|)
                |ILIST;eq?;2$B;3|)) 

(PUT '|ILIST;eq?;2$B;3| '|SPADreplace| 'EQ) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |ILIST;first;$S;4|)) 

(PUT '|ILIST;first;$S;4| '|SPADreplace| '|SPADfirst|) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Thing|)
                |ILIST;elt;$firstS;5|)) 

(PUT '|ILIST;elt;$firstS;5| '|SPADreplace|
     '(XLAM (|x| "first") (|SPADfirst| |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%List|) |ILIST;empty;$;6|)) 

(PUT '|ILIST;empty;$;6| '|SPADreplace| '(XLAM NIL NIL)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Boolean|)
                |ILIST;empty?;$B;7|)) 

(PUT '|ILIST;empty?;$B;7| '|SPADreplace| 'NULL) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;rest;2$;8|)) 

(PUT '|ILIST;rest;2$;8| '|SPADreplace| 'CDR) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%List|)
                |ILIST;elt;$rest$;9|)) 

(PUT '|ILIST;elt;$rest$;9| '|SPADreplace|
     '(XLAM (|x| "rest") (CDR |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Thing|)
                |ILIST;setfirst!;$2S;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |ILIST;setelt;$first2S;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%List|)
                |ILIST;setrest!;3$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%List| |%Shell|) |%List|)
                |ILIST;setelt;$rest2$;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;construct;L$;14|)) 

(PUT '|ILIST;construct;L$;14| '|SPADreplace| '(XLAM (|l|) |l|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;parts;$L;15|)) 

(PUT '|ILIST;parts;$L;15| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;reverse!;2$;16|)) 

(PUT '|ILIST;reverse!;2$;16| '|SPADreplace| 'NREVERSE) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;reverse;2$;17|)) 

(PUT '|ILIST;reverse;2$;17| '|SPADreplace| 'REVERSE) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Integer|)
                |ILIST;minIndex;$I;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| (|%IntegerSection| 0) |%Shell|)
                    |%List|)
                |ILIST;rest;$Nni$;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;copy;2$;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |ILIST;coerce;$Of;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%Boolean|)
                |ILIST;=;2$B;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%String|)
                |ILIST;latex;$S;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Boolean|)
                |ILIST;member?;S$B;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%List|)
                |ILIST;concat!;3$;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;removeDuplicates!;2$;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%List|)
                |ILIST;sort!;M2$;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%List| |%Shell|) |%List|)
                |ILIST;merge!;M3$;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Integer| |%Shell|) |%List|)
                |ILIST;split!;$I$;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Integer| |%Shell|)
                    |%List|)
                |ILIST;mergeSort|)) 

(DEFUN |ILIST;#;$Nni;1| (|x| $) (DECLARE (IGNORE $)) (LENGTH |x|)) 

(DEFUN |ILIST;concat;S2$;2| (|s| |x| $)
  (DECLARE (IGNORE $))
  (CONS |s| |x|)) 

(DEFUN |ILIST;eq?;2$B;3| (|x| |y| $)
  (DECLARE (IGNORE $))
  (EQ |x| |y|)) 

(DEFUN |ILIST;first;$S;4| (|x| $)
  (DECLARE (IGNORE $))
  (|SPADfirst| |x|)) 

(DEFUN |ILIST;elt;$firstS;5| (|x| T0 $)
  (DECLARE (IGNORE $))
  (|SPADfirst| |x|)) 

(DEFUN |ILIST;empty;$;6| ($) (DECLARE (IGNORE $)) NIL) 

(DEFUN |ILIST;empty?;$B;7| (|x| $) (DECLARE (IGNORE $)) (NULL |x|)) 

(DEFUN |ILIST;rest;2$;8| (|x| $) (DECLARE (IGNORE $)) (CDR |x|)) 

(DEFUN |ILIST;elt;$rest$;9| (|x| T1 $) (DECLARE (IGNORE $)) (CDR |x|)) 

(DEFUN |ILIST;setfirst!;$2S;10| (|x| |s| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (QCAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setelt;$first2S;11| (|x| T2 |s| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (QCAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setrest!;3$;12| (|x| |y| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (QCDR (RPLACD |x| |y|))))) 

(DEFUN |ILIST;setelt;$rest2$;13| (|x| T3 |y| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (QCDR (RPLACD |x| |y|))))) 

(DEFUN |ILIST;construct;L$;14| (|l| $) (DECLARE (IGNORE $)) |l|) 

(DEFUN |ILIST;parts;$L;15| (|s| $) (DECLARE (IGNORE $)) |s|) 

(DEFUN |ILIST;reverse!;2$;16| (|x| $)
  (DECLARE (IGNORE $))
  (NREVERSE |x|)) 

(DEFUN |ILIST;reverse;2$;17| (|x| $)
  (DECLARE (IGNORE $))
  (REVERSE |x|)) 

(DEFUN |ILIST;minIndex;$I;18| (|x| $) (|getShellEntry| $ 7)) 

(DEFUN |ILIST;rest;$Nni$;19| (|x| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (SEQ (LETT |i| 1 |ILIST;rest;$Nni$;19|) G190
                (COND ((QSGREATERP |i| |n|) (GO G191)))
                (SEQ (COND
                       ((NULL |x|) (|error| "index out of range")))
                     (EXIT (LETT |x| (QCDR |x|) |ILIST;rest;$Nni$;19|)))
                (LETT |i| (QSADD1 |i|) |ILIST;rest;$Nni$;19|) (GO G190)
                G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |ILIST;copy;2$;20| (|x| $)
  (PROG (|i| |y|)
    (RETURN
      (SEQ (LETT |y| NIL |ILIST;copy;2$;20|)
           (SEQ (LETT |i| 0 |ILIST;copy;2$;20|) G190
                (COND ((NULL (NOT (NULL |x|))) (GO G191)))
                (SEQ (COND
                       ((EQ |i| 1000)
                        (COND
                          ((SPADCALL |x| (|getShellEntry| $ 39))
                           (|error| "cyclic list")))))
                     (LETT |y| (CONS (QCAR |x|) |y|)
                           |ILIST;copy;2$;20|)
                     (EXIT (LETT |x| (QCDR |x|) |ILIST;copy;2$;20|)))
                (LETT |i| (QSADD1 |i|) |ILIST;copy;2$;20|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (NREVERSE |y|)))))) 

(DEFUN |ILIST;coerce;$Of;21| (|x| $)
  (PROG (|s| |y| |z|)
    (RETURN
      (SEQ (LETT |y| NIL |ILIST;coerce;$Of;21|)
           (LETT |s| (SPADCALL |x| (|getShellEntry| $ 44))
                 |ILIST;coerce;$Of;21|)
           (SEQ G190 (COND ((NULL (NEQ |x| |s|)) (GO G191)))
                (SEQ (LETT |y|
                           (CONS (SPADCALL (|SPADfirst| |x|)
                                     (|getShellEntry| $ 45))
                                 |y|)
                           |ILIST;coerce;$Of;21|)
                     (EXIT (LETT |x| (CDR |x|) |ILIST;coerce;$Of;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (LETT |y| (NREVERSE |y|) |ILIST;coerce;$Of;21|)
           (EXIT (COND
                   ((NULL |s|) (SPADCALL |y| (|getShellEntry| $ 49)))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL
                                   (SPADCALL (|SPADfirst| |x|)
                                    (|getShellEntry| $ 45))
                                   (|getShellEntry| $ 50))
                               |ILIST;coerce;$Of;21|)
                         (SEQ G190
                              (COND
                                ((NULL (NEQ |s| (CDR |x|))) (GO G191)))
                              (SEQ (LETT |x| (CDR |x|)
                                    |ILIST;coerce;$Of;21|)
                                   (EXIT
                                    (LETT |z|
                                     (CONS
                                      (SPADCALL (|SPADfirst| |x|)
                                       (|getShellEntry| $ 45))
                                      |z|)
                                     |ILIST;coerce;$Of;21|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL
                                   (SPADCALL |y|
                                    (SPADCALL
                                     (SPADCALL (NREVERSE |z|)
                                      (|getShellEntry| $ 51))
                                     (|getShellEntry| $ 52))
                                    (|getShellEntry| $ 53))
                                   (|getShellEntry| $ 49))))))))))) 

(DEFUN |ILIST;=;2$B;22| (|x| |y| $)
  (PROG (#0=#:G1470)
    (RETURN
      (SEQ (EXIT (COND
                   ((EQ |x| |y|) 'T)
                   ('T
                    (SEQ (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((NULL |x|) 'NIL)
                                         ('T (NOT (NULL |y|)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL (QCAR |x|) (QCAR |y|)
                                        (|getShellEntry| $ 57))
                                       (PROGN
                                         (LETT #0# 'NIL
                                          |ILIST;=;2$B;22|)
                                         (GO #0#)))
                                      ('T
                                       (SEQ
                                        (LETT |x| (QCDR |x|)
                                         |ILIST;=;2$B;22|)
                                        (EXIT
                                         (LETT |y| (QCDR |y|)
                                          |ILIST;=;2$B;22|)))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (COND
                                 ((NULL |x|) (NULL |y|))
                                 ('T 'NIL)))))))
           #0# (EXIT #0#))))) 

(DEFUN |ILIST;latex;$S;23| (|x| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s| "\\left[" |ILIST;latex;$S;23|)
           (SEQ G190 (COND ((NULL (NOT (NULL |x|))) (GO G191)))
                (SEQ (LETT |s|
                           (STRCONC |s|
                                    (SPADCALL (QCAR |x|)
                                     (|getShellEntry| $ 60)))
                           |ILIST;latex;$S;23|)
                     (LETT |x| (QCDR |x|) |ILIST;latex;$S;23|)
                     (EXIT (COND
                             ((NOT (NULL |x|))
                              (LETT |s| (STRCONC |s| ", ")
                                    |ILIST;latex;$S;23|)))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (STRCONC |s| " \\right]")))))) 

(DEFUN |ILIST;member?;S$B;24| (|s| |x| $)
  (PROG (#0=#:G1478)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND ((NULL (NOT (NULL |x|))) (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL |s| (QCAR |x|)
                                          (|getShellEntry| $ 63))
                                         (PROGN
                                           (LETT #0# 'T
                                            |ILIST;member?;S$B;24|)
                                           (GO #0#)))
                                        ('T
                                         (LETT |x| (QCDR |x|)
                                          |ILIST;member?;S$B;24|)))))
                           NIL (GO G190) G191 (EXIT NIL))
                      (EXIT 'NIL)))
           #0# (EXIT #0#))))) 

(DEFUN |ILIST;concat!;3$;25| (|x| |y| $)
  (PROG (|z|)
    (RETURN
      (SEQ (COND
             ((NULL |x|)
              (COND
                ((NULL |y|) |x|)
                ('T
                 (SEQ (PUSH (|SPADfirst| |y|) |x|)
                      (QRPLACD |x| (CDR |y|)) (EXIT |x|)))))
             ('T
              (SEQ (LETT |z| |x| |ILIST;concat!;3$;25|)
                   (SEQ G190
                        (COND
                          ((NULL (NOT (NULL (QCDR |z|)))) (GO G191)))
                        (SEQ (EXIT (LETT |z| (QCDR |z|)
                                    |ILIST;concat!;3$;25|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (QRPLACD |z| |y|) (EXIT |x|)))))))) 

(DEFUN |ILIST;removeDuplicates!;2$;26| (|l| $)
  (PROG (|f| |p| |pr| |pp|)
    (RETURN
      (SEQ (LETT |p| |l| |ILIST;removeDuplicates!;2$;26|)
           (SEQ G190 (COND ((NULL (NOT (NULL |p|))) (GO G191)))
                (SEQ (LETT |pp| |p| |ILIST;removeDuplicates!;2$;26|)
                     (LETT |f| (QCAR |p|)
                           |ILIST;removeDuplicates!;2$;26|)
                     (LETT |p| (QCDR |p|)
                           |ILIST;removeDuplicates!;2$;26|)
                     (EXIT (SEQ G190
                                (COND
                                  ((NULL
                                    (NOT
                                     (NULL
                                      (LETT |pr| (QCDR |pp|)
                                       |ILIST;removeDuplicates!;2$;26|))))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (COND
                                        ((SPADCALL (QCAR |pr|) |f|
                                          (|getShellEntry| $ 63))
                                         (QRPLACD |pp| (QCDR |pr|)))
                                        ('T
                                         (LETT |pp| |pr|
                                          |ILIST;removeDuplicates!;2$;26|)))))
                                NIL (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |ILIST;sort!;M2$;27| (|f| |l| $)
  (|ILIST;mergeSort| |f| |l| (LENGTH |l|) $)) 

(DEFUN |ILIST;merge!;M3$;28| (|f| |p| |q| $)
  (PROG (|r| |t|)
    (RETURN
      (SEQ (COND
             ((NULL |p|) |q|)
             ((NULL |q|) |p|)
             ((EQ |p| |q|) (|error| "cannot merge a list into itself"))
             ('T
              (SEQ (COND
                     ((SPADCALL (QCAR |p|) (QCAR |q|) |f|)
                      (SEQ (LETT |r|
                                 (LETT |t| |p| |ILIST;merge!;M3$;28|)
                                 |ILIST;merge!;M3$;28|)
                           (EXIT (LETT |p| (QCDR |p|)
                                       |ILIST;merge!;M3$;28|))))
                     ('T
                      (SEQ (LETT |r|
                                 (LETT |t| |q| |ILIST;merge!;M3$;28|)
                                 |ILIST;merge!;M3$;28|)
                           (EXIT (LETT |q| (QCDR |q|)
                                       |ILIST;merge!;M3$;28|)))))
                   (SEQ G190
                        (COND
                          ((NULL (COND
                                   ((NULL |p|) 'NIL)
                                   ('T (NOT (NULL |q|)))))
                           (GO G191)))
                        (SEQ (EXIT (COND
                                     ((SPADCALL (QCAR |p|) (QCAR |q|)
                                       |f|)
                                      (SEQ (QRPLACD |t| |p|)
                                       (LETT |t| |p|
                                        |ILIST;merge!;M3$;28|)
                                       (EXIT
                                        (LETT |p| (QCDR |p|)
                                         |ILIST;merge!;M3$;28|))))
                                     ('T
                                      (SEQ (QRPLACD |t| |q|)
                                       (LETT |t| |q|
                                        |ILIST;merge!;M3$;28|)
                                       (EXIT
                                        (LETT |q| (QCDR |q|)
                                         |ILIST;merge!;M3$;28|)))))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (QRPLACD |t| (COND ((NULL |p|) |q|) ('T |p|)))
                   (EXIT |r|)))))))) 

(DEFUN |ILIST;split!;$I$;29| (|p| |n| $)
  (PROG (#0=#:G1507 |q|)
    (RETURN
      (SEQ (COND
             ((< |n| 1) (|error| "index out of range"))
             ('T
              (SEQ (LETT |p|
                         (|ILIST;rest;$Nni$;19| |p|
                             (PROG1 (LETT #0# (- |n| 1)
                                     |ILIST;split!;$I$;29|)
                               (|check-subtype|
                                   (COND ((< #0# 0) 'NIL) ('T 'T))
                                   '(|NonNegativeInteger|) #0#))
                             $)
                         |ILIST;split!;$I$;29|)
                   (LETT |q| (QCDR |p|) |ILIST;split!;$I$;29|)
                   (QRPLACD |p| NIL) (EXIT |q|)))))))) 

(DEFUN |ILIST;mergeSort| (|f| |p| |n| $)
  (PROG (#0=#:G1511 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL (|SPADfirst| (CDR |p|)) (|SPADfirst| |p|)
                     |f|)
                 (LETT |p| (NREVERSE |p|) |ILIST;mergeSort|)))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   ('T
                    (SEQ (LETT |l|
                               (PROG1 (LETT #0# (QUOTIENT2 |n| 2)
                                       |ILIST;mergeSort|)
                                 (|check-subtype|
                                     (COND ((< #0# 0) 'NIL) ('T 'T))
                                     '(|NonNegativeInteger|) #0#))
                               |ILIST;mergeSort|)
                         (LETT |q| (|ILIST;split!;$I$;29| |p| |l| $)
                               |ILIST;mergeSort|)
                         (LETT |p| (|ILIST;mergeSort| |f| |p| |l| $)
                               |ILIST;mergeSort|)
                         (LETT |q|
                               (|ILIST;mergeSort| |f| |q| (- |n| |l|)
                                   $)
                               |ILIST;mergeSort|)
                         (EXIT (|ILIST;merge!;M3$;28| |f| |p| |q| $)))))))))) 

(DEFUN |IndexedList| (&REST #0=#:G1523 &AUX #1=#:G1521)
  (DSETQ #1# #0#)
  (PROG ()
    (RETURN
      (PROG (#2=#:G1522)
        (RETURN
          (COND
            ((LETT #2#
                   (|lassocShiftWithFunction| (|devaluateList| #1#)
                       (HGET |$ConstructorCache| '|IndexedList|)
                       '|domainEqualList|)
                   |IndexedList|)
             (|CDRwithIncrement| #2#))
            ('T
             (UNWIND-PROTECT
               (PROG1 (APPLY (|function| |IndexedList;|) #1#)
                 (LETT #2# T |IndexedList|))
               (COND
                 ((NOT #2#) (HREM |$ConstructorCache| '|IndexedList|))))))))))) 

(DEFUN |IndexedList;| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|IndexedList|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|IndexedList| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 88) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (OR (AND (|HasCategory| |#1|
                                      '(|OrderedSet|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|))))
                                (AND (|HasCategory| |#1|
                                      '(|SetCategory|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|)))))
                            (OR (AND (|HasCategory| |#1|
                                      '(|SetCategory|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|))))
                                (|HasCategory| |#1|
                                    '(|CoercibleTo| (|OutputForm|))))
                            (|HasCategory| |#1|
                                '(|ConvertibleTo| (|InputForm|)))
                            (OR (|HasCategory| |#1| '(|OrderedSet|))
                                (|HasCategory| |#1| '(|SetCategory|)))
                            (|HasCategory| |#1| '(|OrderedSet|))
                            (|HasCategory| (|Integer|) '(|OrderedSet|))
                            (|HasCategory| |#1| '(|SetCategory|))
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|)))
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|)))))) . #0#))
        (|haddProp| |$ConstructorCache| '|IndexedList|
            (LIST |dv$1| |dv$2|) (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 8)
           (|setShellEntry| $ 54
               (CONS (|dispatchFunction| |ILIST;coerce;$Of;21|) $))))
        (COND
          ((|testBitVector| |pv$| 7)
           (PROGN
             (|setShellEntry| $ 58
                 (CONS (|dispatchFunction| |ILIST;=;2$B;22|) $))
             (|setShellEntry| $ 62
                 (CONS (|dispatchFunction| |ILIST;latex;$S;23|) $))
             (|setShellEntry| $ 64
                 (CONS (|dispatchFunction| |ILIST;member?;S$B;24|) $)))))
        (COND
          ((|testBitVector| |pv$| 7)
           (|setShellEntry| $ 66
               (CONS (|dispatchFunction|
                         |ILIST;removeDuplicates!;2$;26|)
                     $))))
        $)))) 

(MAKEPROP '|IndexedList| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|NonNegativeInteger|) |ILIST;#;$Nni;1|
             |ILIST;concat;S2$;2| (|Boolean|) |ILIST;eq?;2$B;3|
             |ILIST;first;$S;4| '"first" |ILIST;elt;$firstS;5|
             |ILIST;empty;$;6| |ILIST;empty?;$B;7| |ILIST;rest;2$;8|
             '"rest" |ILIST;elt;$rest$;9| |ILIST;setfirst!;$2S;10|
             |ILIST;setelt;$first2S;11| |ILIST;setrest!;3$;12|
             |ILIST;setelt;$rest2$;13| (|List| 6)
             |ILIST;construct;L$;14| |ILIST;parts;$L;15|
             |ILIST;reverse!;2$;16| |ILIST;reverse;2$;17| (|Integer|)
             |ILIST;minIndex;$I;18| (|SingleInteger|) (0 . |One|)
             (4 . |One|) |ILIST;rest;$Nni$;19| (8 . |Zero|)
             (12 . |Zero|) (16 . |not|) (21 . |cyclic?|)
             |ILIST;copy;2$;20| (|OutputForm|) (|List| 41)
             (26 . |empty|) (30 . |cycleEntry|) (35 . |coerce|)
             (40 . |concat|) (46 . |reverse!|) (|List| $)
             (51 . |bracket|) (56 . |list|) (61 . |commaSeparate|)
             (66 . |overbar|) (71 . |concat!|) (77 . |coerce|)
             (82 . |true|) (86 . |false|) (90 . ~=) (96 . =) (|String|)
             (102 . |latex|) (107 . |concat|) (113 . |latex|) (118 . =)
             (124 . |member?|) |ILIST;concat!;3$;25|
             (130 . |removeDuplicates!|) (|Mapping| 11 6 6)
             |ILIST;sort!;M2$;27| |ILIST;merge!;M3$;28| (135 . |One|)
             (139 . <) (145 . -) |ILIST;split!;$I$;29| (151 . =)
             (157 . |quo|) (|Mapping| 6 6 6) (|Equation| 6) (|List| 77)
             (|Mapping| 11 6) (|Void|) (|UniversalSegment| 30) '"last"
             '"value" (|Mapping| 6 6) (|InputForm|) (|List| 30)
             (|Union| 6 '"failed"))
          '#(~= 163 |value| 169 |third| 174 |tail| 179 |swap!| 184
             |split!| 191 |sorted?| 197 |sort!| 208 |sort| 219 |size?|
             230 |setvalue!| 236 |setrest!| 242 |setlast!| 248
             |setfirst!| 254 |setelt| 260 |setchildren!| 302 |select!|
             308 |select| 314 |second| 320 |sample| 325 |reverse!| 329
             |reverse| 334 |rest| 339 |removeDuplicates!| 350
             |removeDuplicates| 355 |remove!| 360 |remove| 372 |reduce|
             384 |qsetelt!| 405 |qelt| 412 |possiblyInfinite?| 418
             |position| 423 |parts| 442 |nodes| 447 |node?| 452 |new|
             458 |more?| 464 |minIndex| 470 |min| 475 |merge!| 481
             |merge| 494 |members| 507 |member?| 512 |maxIndex| 518
             |max| 523 |map!| 529 |map| 535 |list| 548 |less?| 553
             |leaves| 559 |leaf?| 564 |latex| 569 |last| 574 |insert!|
             585 |insert| 599 |indices| 613 |index?| 618 |hash| 624
             |first| 629 |find| 640 |fill!| 646 |explicitlyFinite?| 652
             |every?| 657 |eval| 663 |eq?| 689 |entry?| 695 |entries|
             701 |empty?| 706 |empty| 711 |elt| 715 |distance| 758
             |delete!| 764 |delete| 776 |cyclic?| 788 |cycleTail| 793
             |cycleSplit!| 798 |cycleLength| 803 |cycleEntry| 808
             |count| 813 |copyInto!| 825 |copy| 832 |convert| 837
             |construct| 842 |concat!| 847 |concat| 859 |coerce| 882
             |children| 887 |child?| 892 |before?| 898 |any?| 904 >=
             910 > 916 = 922 <= 928 < 934 |#| 940)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 5
                    '(0 0 0 0 0 0 0 0 0 0 5 0 0 1 4 0 0 1 2 3 4))
                (CONS '#(|ListAggregate&| |StreamAggregate&|
                         |ExtensibleLinearAggregate&|
                         |FiniteLinearAggregate&|
                         |UnaryRecursiveAggregate&| |LinearAggregate&|
                         |RecursiveAggregate&| |IndexedAggregate&|
                         |Collection&| |HomogeneousAggregate&|
                         |OrderedSet&| |Aggregate&| |EltableAggregate&|
                         |Evalable&| |SetCategory&| NIL NIL
                         |InnerEvalable&| NIL NIL |BasicType&|)
                      (CONS '#((|ListAggregate| 6)
                               (|StreamAggregate| 6)
                               (|ExtensibleLinearAggregate| 6)
                               (|FiniteLinearAggregate| 6)
                               (|UnaryRecursiveAggregate| 6)
                               (|LinearAggregate| 6)
                               (|RecursiveAggregate| 6)
                               (|IndexedAggregate| 30 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 30 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|)
                               (|Eltable| 30 6) (|InnerEvalable| 6 6)
                               (|CoercibleTo| 41) (|ConvertibleTo| 85)
                               (|BasicType|))
                            (|makeByteWordVec2| 87
                                '(0 32 0 33 0 8 0 34 0 32 0 36 0 8 0 37
                                  1 11 0 0 38 1 0 11 0 39 0 42 0 43 1 0
                                  0 0 44 1 6 41 0 45 2 42 0 41 0 46 1
                                  42 0 0 47 1 41 0 48 49 1 42 0 41 50 1
                                  41 0 48 51 1 41 0 0 52 2 42 0 0 41 53
                                  1 0 41 0 54 0 11 0 55 0 11 0 56 2 6
                                  11 0 0 57 2 0 11 0 0 58 1 6 59 0 60 2
                                  59 0 0 0 61 1 0 59 0 62 2 6 11 0 0 63
                                  2 0 11 6 0 64 1 0 0 0 66 0 30 0 70 2
                                  30 11 0 0 71 2 30 0 0 0 72 2 30 11 0
                                  0 74 2 30 0 0 0 75 2 7 11 0 0 1 1 0 6
                                  0 1 1 0 6 0 1 1 0 0 0 1 3 0 80 0 30
                                  30 1 2 0 0 0 30 73 1 5 11 0 1 2 0 11
                                  67 0 1 1 5 0 0 1 2 0 0 67 0 68 1 5 0
                                  0 1 2 0 0 67 0 1 2 0 11 0 8 1 2 0 6 0
                                  6 1 2 0 0 0 0 23 2 0 6 0 6 1 2 0 6 0
                                  6 21 3 0 6 0 30 6 1 3 0 6 0 81 6 1 3
                                  0 6 0 82 6 1 3 0 0 0 19 0 24 3 0 6 0
                                  14 6 22 3 0 6 0 83 6 1 2 0 0 0 48 1 2
                                  0 0 79 0 1 2 0 0 79 0 1 1 0 6 0 1 0 0
                                  0 1 1 0 0 0 28 1 0 0 0 29 2 0 0 0 8
                                  35 1 0 0 0 18 1 7 0 0 66 1 7 0 0 1 2
                                  7 0 6 0 1 2 0 0 79 0 1 2 7 0 6 0 1 2
                                  0 0 79 0 1 4 7 6 76 0 6 6 1 2 0 6 76
                                  0 1 3 0 6 76 0 6 1 3 0 6 0 30 6 1 2 0
                                  6 0 30 1 1 0 11 0 1 3 7 30 6 0 30 1 2
                                  7 30 6 0 1 2 0 30 79 0 1 1 0 25 0 27
                                  1 0 48 0 1 2 7 11 0 0 1 2 0 0 8 6 1 2
                                  0 11 0 8 1 1 6 30 0 31 2 5 0 0 0 1 2
                                  5 0 0 0 1 3 0 0 67 0 0 69 2 5 0 0 0 1
                                  3 0 0 67 0 0 1 1 0 25 0 1 2 7 11 6 0
                                  64 1 6 30 0 1 2 5 0 0 0 1 2 0 0 84 0
                                  1 3 0 0 76 0 0 1 2 0 0 84 0 1 1 0 0 6
                                  1 2 0 11 0 8 1 1 0 25 0 1 1 0 11 0 1
                                  1 7 59 0 62 2 0 0 0 8 1 1 0 6 0 1 3 0
                                  0 0 0 30 1 3 0 0 6 0 30 1 3 0 0 0 0
                                  30 1 3 0 0 6 0 30 1 1 0 86 0 1 2 0 11
                                  30 0 1 1 7 32 0 1 2 0 0 0 8 1 1 0 6 0
                                  13 2 0 87 79 0 1 2 0 0 0 6 1 1 0 11 0
                                  1 2 0 11 79 0 1 3 9 0 0 6 6 1 3 9 0 0
                                  25 25 1 2 9 0 0 77 1 2 9 0 0 78 1 2 0
                                  11 0 0 12 2 7 11 6 0 1 1 0 25 0 1 1 0
                                  11 0 17 0 0 0 16 3 0 6 0 30 6 1 2 0 6
                                  0 30 1 2 0 0 0 81 1 2 0 6 0 82 1 2 0
                                  0 0 19 20 2 0 6 0 14 15 2 0 6 0 83 1
                                  2 0 30 0 0 1 2 0 0 0 30 1 2 0 0 0 81
                                  1 2 0 0 0 81 1 2 0 0 0 30 1 1 0 11 0
                                  39 1 0 0 0 1 1 0 0 0 1 1 0 8 0 1 1 0
                                  0 0 44 2 7 8 6 0 1 2 0 8 79 0 1 3 0 0
                                  0 0 30 1 1 0 0 0 40 1 3 85 0 1 1 0 0
                                  25 26 2 0 0 0 6 1 2 0 0 0 0 65 1 0 0
                                  48 1 2 0 0 0 6 1 2 0 0 6 0 10 2 0 0 0
                                  0 1 1 8 41 0 54 1 0 48 0 1 2 7 11 0 0
                                  1 2 7 11 0 0 1 2 0 11 79 0 1 2 5 11 0
                                  0 1 2 5 11 0 0 1 2 7 11 0 0 58 2 5 11
                                  0 0 1 2 5 11 0 0 1 1 0 8 0 9)))))
          '|lookupComplete|)) 
