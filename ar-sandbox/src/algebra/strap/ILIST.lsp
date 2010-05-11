
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
                          ((SPADCALL |x| (|getShellEntry| $ 34))
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
           (LETT |s| (SPADCALL |x| (|getShellEntry| $ 39))
                 |ILIST;coerce;$Of;21|)
           (SEQ G190 (COND ((NULL (NOT (EQ |x| |s|))) (GO G191)))
                (SEQ (LETT |y|
                           (CONS (SPADCALL (|SPADfirst| |x|)
                                     (|getShellEntry| $ 40))
                                 |y|)
                           |ILIST;coerce;$Of;21|)
                     (EXIT (LETT |x| (CDR |x|) |ILIST;coerce;$Of;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (LETT |y| (NREVERSE |y|) |ILIST;coerce;$Of;21|)
           (EXIT (COND
                   ((NULL |s|) (SPADCALL |y| (|getShellEntry| $ 44)))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL
                                   (SPADCALL (|SPADfirst| |x|)
                                    (|getShellEntry| $ 40))
                                   (|getShellEntry| $ 45))
                               |ILIST;coerce;$Of;21|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT (EQ |s| (CDR |x|))))
                                 (GO G191)))
                              (SEQ (LETT |x| (CDR |x|)
                                    |ILIST;coerce;$Of;21|)
                                   (EXIT
                                    (LETT |z|
                                     (CONS
                                      (SPADCALL (|SPADfirst| |x|)
                                       (|getShellEntry| $ 40))
                                      |z|)
                                     |ILIST;coerce;$Of;21|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL
                                   (SPADCALL |y|
                                    (SPADCALL
                                     (SPADCALL (NREVERSE |z|)
                                      (|getShellEntry| $ 46))
                                     (|getShellEntry| $ 47))
                                    (|getShellEntry| $ 48))
                                   (|getShellEntry| $ 44))))))))))) 

(DEFUN |ILIST;=;2$B;22| (|x| |y| $)
  (PROG (#0=#:G1468)
    (RETURN
      (SEQ (EXIT (COND
                   ((EQ |x| |y|) T)
                   ('T
                    (SEQ (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((NULL |x|) NIL)
                                         ('T (NOT (NULL |y|)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL (QCAR |x|) (QCAR |y|)
                                        (|getShellEntry| $ 52))
                                       (PROGN
                                         (LETT #0# NIL
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
                         (EXIT (COND ((NULL |x|) (NULL |y|)) ('T NIL)))))))
           #0# (EXIT #0#))))) 

(DEFUN |ILIST;latex;$S;23| (|x| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s| "\\left[" |ILIST;latex;$S;23|)
           (SEQ G190 (COND ((NULL (NOT (NULL |x|))) (GO G191)))
                (SEQ (LETT |s|
                           (STRCONC |s|
                                    (SPADCALL (QCAR |x|)
                                     (|getShellEntry| $ 55)))
                           |ILIST;latex;$S;23|)
                     (LETT |x| (QCDR |x|) |ILIST;latex;$S;23|)
                     (EXIT (COND
                             ((NOT (NULL |x|))
                              (LETT |s| (STRCONC |s| ", ")
                                    |ILIST;latex;$S;23|)))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (STRCONC |s| " \\right]")))))) 

(DEFUN |ILIST;member?;S$B;24| (|s| |x| $)
  (PROG (#0=#:G1476)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND ((NULL (NOT (NULL |x|))) (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL |s| (QCAR |x|)
                                          (|getShellEntry| $ 58))
                                         (PROGN
                                           (LETT #0# T
                                            |ILIST;member?;S$B;24|)
                                           (GO #0#)))
                                        ('T
                                         (LETT |x| (QCDR |x|)
                                          |ILIST;member?;S$B;24|)))))
                           NIL (GO G190) G191 (EXIT NIL))
                      (EXIT NIL)))
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
                        (LETT |z| (QCDR |z|) |ILIST;concat!;3$;25|) NIL
                        (GO G190) G191 (EXIT NIL))
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
                                          (|getShellEntry| $ 58))
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
                                   ((NULL |p|) NIL)
                                   ('T (NOT (NULL |q|)))))
                           (GO G191)))
                        (COND
                          ((SPADCALL (QCAR |p|) (QCAR |q|) |f|)
                           (SEQ (QRPLACD |t| |p|)
                                (LETT |t| |p| |ILIST;merge!;M3$;28|)
                                (EXIT (LETT |p| (QCDR |p|)
                                       |ILIST;merge!;M3$;28|))))
                          ('T
                           (SEQ (QRPLACD |t| |q|)
                                (LETT |t| |q| |ILIST;merge!;M3$;28|)
                                (EXIT (LETT |q| (QCDR |q|)
                                       |ILIST;merge!;M3$;28|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (QRPLACD |t| (COND ((NULL |p|) |q|) ('T |p|)))
                   (EXIT |r|)))))))) 

(DEFUN |ILIST;split!;$I$;29| (|p| |n| $)
  (PROG (|q|)
    (RETURN
      (SEQ (COND
             ((< |n| 1) (|error| "index out of range"))
             ('T
              (SEQ (LETT |p|
                         (|ILIST;rest;$Nni$;19| |p|
                             (LET ((#0=#:G1507 (- |n| 1)))
                               (|check-subtype| (>= #0# 0)
                                   '(|NonNegativeInteger|) #0#))
                             $)
                         |ILIST;split!;$I$;29|)
                   (LETT |q| (QCDR |p|) |ILIST;split!;$I$;29|)
                   (QRPLACD |p| NIL) (EXIT |q|)))))))) 

(DEFUN |ILIST;mergeSort| (|f| |p| |n| $)
  (PROG (|l| |q|)
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
                               (LET ((#0=#:G1512 (QUOTIENT2 |n| 2)))
                                 (|check-subtype| (>= #0# 0)
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

(DEFUN |IndexedList| (&REST #0=#:G1521 &AUX #1=#:G1519)
  (DSETQ #1# #0#)
  (PROG ()
    (RETURN
      (PROG (#2=#:G1520)
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
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|IndexedList| |dv$1| |dv$2|))
         ($ (|newShell| 85))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (OR (AND (|HasCategory| |#1| '(|OrderedSet|))
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
                                  (LIST '|CoercibleTo| '(|OutputForm|))))
                          (|HasCategory| |#1|
                              (LIST '|ConvertibleTo| '(|InputForm|)))
                          (OR (|HasCategory| |#1| '(|OrderedSet|))
                              (|HasCategory| |#1| '(|SetCategory|)))
                          (|HasCategory| |#1| '(|OrderedSet|))
                          (|HasCategory| (|Integer|) '(|OrderedSet|))
                          (|HasCategory| |#1| '(|SetCategory|))
                          (|HasCategory| |#1|
                              (LIST '|CoercibleTo| '(|OutputForm|)))
                          (AND (|HasCategory| |#1| '(|SetCategory|))
                               (|HasCategory| |#1|
                                   (LIST '|Evalable|
                                    (|devaluate| |#1|))))))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|IndexedList| (LIST |dv$1| |dv$2|)
        (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    (COND
      ((|testBitVector| |pv$| 8)
       (|setShellEntry| $ 49
           (CONS (|dispatchFunction| |ILIST;coerce;$Of;21|) $))))
    (COND
      ((|testBitVector| |pv$| 7)
       (PROGN
         (|setShellEntry| $ 53
             (CONS (|dispatchFunction| |ILIST;=;2$B;22|) $))
         (|setShellEntry| $ 57
             (CONS (|dispatchFunction| |ILIST;latex;$S;23|) $))
         (|setShellEntry| $ 59
             (CONS (|dispatchFunction| |ILIST;member?;S$B;24|) $)))))
    (COND
      ((|testBitVector| |pv$| 7)
       (|setShellEntry| $ 61
           (CONS (|dispatchFunction| |ILIST;removeDuplicates!;2$;26|)
                 $))))
    $)) 

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
             |ILIST;minIndex;$I;18| |ILIST;rest;$Nni$;19| (0 . |not|)
             (5 . |cyclic?|) |ILIST;copy;2$;20| (|OutputForm|)
             (|List| 36) (10 . |empty|) (14 . |cycleEntry|)
             (19 . |coerce|) (24 . |concat|) (30 . |reverse!|)
             (|List| $) (35 . |bracket|) (40 . |list|)
             (45 . |commaSeparate|) (50 . |overbar|) (55 . |concat!|)
             (61 . |coerce|) (66 . |true|) (70 . |false|) (74 . ~=)
             (80 . =) (|String|) (86 . |latex|) (91 . |concat|)
             (97 . |latex|) (102 . =) (108 . |member?|)
             |ILIST;concat!;3$;25| (114 . |removeDuplicates!|)
             (|Mapping| 11 6 6) |ILIST;sort!;M2$;27|
             |ILIST;merge!;M3$;28| (119 . |One|) (123 . <)
             (129 . |One|) (133 . -) |ILIST;split!;$I$;29| (139 . =)
             (145 . |quo|) (|Mapping| 6 6 6) (|Equation| 6) (|List| 73)
             (|Mapping| 11 6) (|Void|) (|UniversalSegment| 30) '"last"
             '"value" (|Mapping| 6 6) (|InputForm|) (|SingleInteger|)
             (|List| 30) (|Union| 6 '"failed"))
          '#(~= 151 |value| 157 |third| 162 |tail| 167 |swap!| 172
             |split!| 179 |sorted?| 185 |sort!| 196 |sort| 207 |size?|
             218 |setvalue!| 224 |setrest!| 230 |setlast!| 236
             |setfirst!| 242 |setelt| 248 |setchildren!| 290 |select!|
             296 |select| 302 |second| 308 |sample| 313 |reverse!| 317
             |reverse| 322 |rest| 327 |removeDuplicates!| 338
             |removeDuplicates| 343 |remove!| 348 |remove| 360 |reduce|
             372 |qsetelt!| 393 |qelt| 400 |possiblyInfinite?| 406
             |position| 411 |parts| 430 |nodes| 435 |node?| 440 |new|
             446 |more?| 452 |minIndex| 458 |min| 463 |merge!| 469
             |merge| 482 |members| 495 |member?| 500 |maxIndex| 506
             |max| 511 |map!| 517 |map| 523 |list| 536 |less?| 541
             |leaves| 547 |leaf?| 552 |latex| 557 |last| 562 |insert!|
             573 |insert| 587 |indices| 601 |index?| 606 |hash| 612
             |first| 617 |find| 628 |fill!| 634 |explicitlyFinite?| 640
             |every?| 645 |eval| 651 |eq?| 677 |entry?| 683 |entries|
             689 |empty?| 694 |empty| 699 |elt| 703 |distance| 746
             |delete!| 752 |delete| 764 |cyclic?| 776 |cycleTail| 781
             |cycleSplit!| 786 |cycleLength| 791 |cycleEntry| 796
             |count| 801 |copyInto!| 813 |copy| 820 |convert| 825
             |construct| 830 |concat!| 835 |concat| 847 |coerce| 870
             |children| 875 |child?| 880 |before?| 886 |any?| 892 >=
             898 > 904 = 910 <= 916 < 922 |#| 928)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 5
                    '(0 0 0 0 0 0 0 0 0 0 0 5 0 0 0 1 4 0 1 2 3 4))
                (CONS '#(|ListAggregate&| |StreamAggregate&|
                         |ExtensibleLinearAggregate&|
                         |FiniteLinearAggregate&|
                         |UnaryRecursiveAggregate&| |LinearAggregate&|
                         |RecursiveAggregate&| |IndexedAggregate&|
                         |Collection&| |HomogeneousAggregate&|
                         |EltableAggregate&| |OrderedSet&| NIL
                         |Aggregate&| NIL |Evalable&| |SetCategory&|
                         NIL |InnerEvalable&| NIL NIL |BasicType&|)
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
                               (|EltableAggregate| 30 6) (|OrderedSet|)
                               (|Eltable| 77 $$) (|Aggregate|)
                               (|Eltable| 30 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|)
                               (|InnerEvalable| 6 6) (|CoercibleTo| 36)
                               (|ConvertibleTo| 81) (|BasicType|))
                            (|makeByteWordVec2| 84
                                '(1 11 0 0 33 1 0 11 0 34 0 37 0 38 1 0
                                  0 0 39 1 6 36 0 40 2 37 0 36 0 41 1
                                  37 0 0 42 1 36 0 43 44 1 37 0 36 45 1
                                  36 0 43 46 1 36 0 0 47 2 37 0 0 36 48
                                  1 0 36 0 49 0 11 0 50 0 11 0 51 2 6
                                  11 0 0 52 2 0 11 0 0 53 1 6 54 0 55 2
                                  54 0 0 0 56 1 0 54 0 57 2 6 11 0 0 58
                                  2 0 11 6 0 59 1 0 0 0 61 0 30 0 65 2
                                  30 11 0 0 66 0 8 0 67 2 30 0 0 0 68 2
                                  30 11 0 0 70 2 30 0 0 0 71 2 7 11 0 0
                                  1 1 0 6 0 1 1 0 6 0 1 1 0 0 0 1 3 0
                                  76 0 30 30 1 2 0 0 0 30 69 1 5 11 0 1
                                  2 0 11 62 0 1 1 5 0 0 1 2 0 0 62 0 63
                                  1 5 0 0 1 2 0 0 62 0 1 2 0 11 0 8 1 2
                                  0 6 0 6 1 2 0 0 0 0 23 2 0 6 0 6 1 2
                                  0 6 0 6 21 3 0 6 0 30 6 1 3 0 6 0 77
                                  6 1 3 0 6 0 78 6 1 3 0 0 0 19 0 24 3
                                  0 6 0 14 6 22 3 0 6 0 79 6 1 2 0 0 0
                                  43 1 2 0 0 75 0 1 2 0 0 75 0 1 1 0 6
                                  0 1 0 0 0 1 1 0 0 0 28 1 0 0 0 29 2 0
                                  0 0 8 32 1 0 0 0 18 1 7 0 0 61 1 7 0
                                  0 1 2 7 0 6 0 1 2 0 0 75 0 1 2 7 0 6
                                  0 1 2 0 0 75 0 1 4 7 6 72 0 6 6 1 2 0
                                  6 72 0 1 3 0 6 72 0 6 1 3 0 6 0 30 6
                                  1 2 0 6 0 30 1 1 0 11 0 1 3 7 30 6 0
                                  30 1 2 7 30 6 0 1 2 0 30 75 0 1 1 0
                                  25 0 27 1 0 43 0 1 2 7 11 0 0 1 2 0 0
                                  8 6 1 2 0 11 0 8 1 1 6 30 0 31 2 5 0
                                  0 0 1 2 5 0 0 0 1 3 0 0 62 0 0 64 2 5
                                  0 0 0 1 3 0 0 62 0 0 1 1 0 25 0 1 2 7
                                  11 6 0 59 1 6 30 0 1 2 5 0 0 0 1 2 0
                                  0 80 0 1 3 0 0 72 0 0 1 2 0 0 80 0 1
                                  1 0 0 6 1 2 0 11 0 8 1 1 0 25 0 1 1 0
                                  11 0 1 1 7 54 0 57 2 0 0 0 8 1 1 0 6
                                  0 1 3 0 0 0 0 30 1 3 0 0 6 0 30 1 3 0
                                  0 0 0 30 1 3 0 0 6 0 30 1 1 0 83 0 1
                                  2 0 11 30 0 1 1 7 82 0 1 2 0 0 0 8 1
                                  1 0 6 0 13 2 0 84 75 0 1 2 0 0 0 6 1
                                  1 0 11 0 1 2 0 11 75 0 1 3 9 0 0 6 6
                                  1 3 9 0 0 25 25 1 2 9 0 0 73 1 2 9 0
                                  0 74 1 2 0 11 0 0 12 2 7 11 6 0 1 1 0
                                  25 0 1 1 0 11 0 17 0 0 0 16 3 0 6 0
                                  30 6 1 2 0 6 0 30 1 2 0 0 0 77 1 2 0
                                  6 0 78 1 2 0 0 0 19 20 2 0 6 0 14 15
                                  2 0 6 0 79 1 2 0 30 0 0 1 2 0 0 0 30
                                  1 2 0 0 0 77 1 2 0 0 0 77 1 2 0 0 0
                                  30 1 1 0 11 0 34 1 0 0 0 1 1 0 0 0 1
                                  1 0 8 0 1 1 0 0 0 39 2 7 8 6 0 1 2 0
                                  8 75 0 1 3 0 0 0 0 30 1 1 0 0 0 35 1
                                  3 81 0 1 1 0 0 25 26 2 0 0 0 6 1 2 0
                                  0 0 0 60 2 0 0 0 6 1 1 0 0 43 1 2 0 0
                                  6 0 10 2 0 0 0 0 1 1 8 36 0 49 1 0 43
                                  0 1 2 7 11 0 0 1 2 7 11 0 0 1 2 0 11
                                  75 0 1 2 5 11 0 0 1 2 5 11 0 0 1 2 7
                                  11 0 0 53 2 5 11 0 0 1 2 5 11 0 0 1 1
                                  0 8 0 9)))))
          '|lookupComplete|)) 
