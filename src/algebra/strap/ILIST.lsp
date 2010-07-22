
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) (|%IntegerSection| 0))
                |ILIST;#;$Nni;1|)) 

(PUT '|ILIST;#;$Nni;1| '|SPADreplace| 'LENGTH) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%List|)
                |ILIST;concat;S2$;2|)) 

(PUT '|ILIST;concat;S2$;2| '|SPADreplace| '|%makepair|) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%Boolean|)
                |ILIST;eq?;2$B;3|)) 

(PUT '|ILIST;eq?;2$B;3| '|SPADreplace| '|%peq|) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |ILIST;first;$S;4|)) 

(PUT '|ILIST;first;$S;4| '|SPADreplace| '|SPADfirst|) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Thing|)
                |ILIST;elt;$firstS;5|)) 

(PUT '|ILIST;elt;$firstS;5| '|SPADreplace|
     '(XLAM (|x| "first") (|SPADfirst| |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%List|) |ILIST;empty;$;6|)) 

(PUT '|ILIST;empty;$;6| '|SPADreplace| '(XLAM NIL |%nil|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Boolean|)
                |ILIST;empty?;$B;7|)) 

(PUT '|ILIST;empty?;$B;7| '|SPADreplace| '|%lempty?|) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%List|)
                |ILIST;rest;2$;8|)) 

(PUT '|ILIST;rest;2$;8| '|SPADreplace| '|%tail|) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%List|)
                |ILIST;elt;$rest$;9|)) 

(PUT '|ILIST;elt;$rest$;9| '|SPADreplace|
     '(XLAM (|x| "rest") (|%tail| |x|))) 

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
    ('T (CAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setelt;$first2S;11| (|x| T2 |s| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (CAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setrest!;3$;12| (|x| |y| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (CDR (RPLACD |x| |y|))))) 

(DEFUN |ILIST;setelt;$rest2$;13| (|x| T3 |y| $)
  (COND
    ((NULL |x|) (|error| "Cannot update an empty list"))
    ('T (CDR (RPLACD |x| |y|))))) 

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
  (SEQ (LET ((|i| 1))
         (LOOP
           (COND
             ((> |i| |n|) (RETURN NIL))
             (T (SEQ (COND
                       ((NULL |x|) (|error| "index out of range")))
                     (EXIT (SETQ |x| (CDR |x|))))))
           (SETQ |i| (+ |i| 1))))
       (EXIT |x|))) 

(DEFUN |ILIST;copy;2$;20| (|x| $)
  (LET ((|y| (SPADCALL (|getShellEntry| $ 16))))
    (SEQ (LET ((|i| 0))
           (LOOP
             (COND
               ((NOT (NOT (NULL |x|))) (RETURN NIL))
               (T (SEQ (COND
                         ((EQL |i| 1000)
                          (COND
                            ((SPADCALL |x| (|getShellEntry| $ 35))
                             (|error| "cyclic list")))))
                       (SETQ |y| (CONS (CAR |x|) |y|))
                       (EXIT (SETQ |x| (CDR |x|))))))
             (SETQ |i| (+ |i| 1))))
         (EXIT (NREVERSE |y|))))) 

(DEFUN |ILIST;coerce;$Of;21| (|x| $)
  (PROG (|z|)
    (RETURN
      (LET* ((|y| NIL) (|s| (SPADCALL |x| (|getShellEntry| $ 40))))
        (SEQ (LOOP
               (COND
                 ((NOT (NOT (EQ |x| |s|))) (RETURN NIL))
                 (T (SEQ (SETQ |y|
                               (CONS (SPADCALL (|SPADfirst| |x|)
                                      (|getShellEntry| $ 41))
                                     |y|))
                         (EXIT (SETQ |x| (CDR |x|)))))))
             (SETQ |y| (NREVERSE |y|))
             (EXIT (COND
                     ((NULL |s|) (SPADCALL |y| (|getShellEntry| $ 45)))
                     ('T
                      (SEQ (LETT |z|
                                 (SPADCALL
                                     (SPADCALL (|SPADfirst| |x|)
                                      (|getShellEntry| $ 41))
                                     (|getShellEntry| $ 46))
                                 |ILIST;coerce;$Of;21|)
                           (LOOP
                             (COND
                               ((NOT (NOT (EQ |s| (CDR |x|))))
                                (RETURN NIL))
                               (T (SEQ (SETQ |x| (CDR |x|))
                                       (EXIT
                                        (SETQ |z|
                                         (CONS
                                          (SPADCALL (|SPADfirst| |x|)
                                           (|getShellEntry| $ 41))
                                          |z|)))))))
                           (EXIT (SPADCALL
                                     (SPADCALL |y|
                                      (SPADCALL
                                       (SPADCALL (NREVERSE |z|)
                                        (|getShellEntry| $ 47))
                                       (|getShellEntry| $ 48))
                                      (|getShellEntry| $ 49))
                                     (|getShellEntry| $ 45)))))))))))) 

(DEFUN |ILIST;=;2$B;22| (|x| |y| $)
  (SEQ (COND
         ((EQ |x| |y|) T)
         ('T
          (SEQ (LOOP
                 (COND
                   ((NOT (COND ((NULL |x|) NIL) ('T (NOT (NULL |y|)))))
                    (RETURN NIL))
                   (T (COND
                        ((SPADCALL (CAR |x|) (CAR |y|)
                             (|getShellEntry| $ 53))
                         (RETURN-FROM |ILIST;=;2$B;22| NIL))
                        ('T
                         (SEQ (SETQ |x| (CDR |x|))
                              (EXIT (SETQ |y| (CDR |y|)))))))))
               (EXIT (COND ((NULL |x|) (NULL |y|)) ('T NIL)))))))) 

(DEFUN |ILIST;latex;$S;23| (|x| $)
  (LET ((|s| "\\left["))
    (SEQ (LOOP
           (COND
             ((NOT (NOT (NULL |x|))) (RETURN NIL))
             (T (SEQ (SETQ |s|
                           (STRCONC |s|
                                    (SPADCALL (CAR |x|)
                                     (|getShellEntry| $ 56))))
                     (SETQ |x| (CDR |x|))
                     (EXIT (COND
                             ((NOT (NULL |x|))
                              (SETQ |s| (STRCONC |s| ", ")))))))))
         (EXIT (STRCONC |s| " \\right]"))))) 

(DEFUN |ILIST;member?;S$B;24| (|s| |x| $)
  (SEQ (LOOP
         (COND
           ((NOT (NOT (NULL |x|))) (RETURN NIL))
           (T (COND
                ((SPADCALL |s| (CAR |x|) (|getShellEntry| $ 59))
                 (RETURN-FROM |ILIST;member?;S$B;24| T))
                ('T (SETQ |x| (CDR |x|)))))))
       (EXIT NIL))) 

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
                   (LOOP
                     (COND
                       ((NOT (NOT (NULL (CDR |z|)))) (RETURN NIL))
                       (T (SETQ |z| (CDR |z|)))))
                   (QRPLACD |z| |y|) (EXIT |x|)))))))) 

(DEFUN |ILIST;removeDuplicates!;2$;26| (|l| $)
  (PROG (|pp| |f| |pr|)
    (RETURN
      (LET ((|p| |l|))
        (SEQ (LOOP
               (COND
                 ((NOT (NOT (NULL |p|))) (RETURN NIL))
                 (T (SEQ (LETT |pp| |p|
                               |ILIST;removeDuplicates!;2$;26|)
                         (LETT |f| (CAR |p|)
                               |ILIST;removeDuplicates!;2$;26|)
                         (SETQ |p| (CDR |p|))
                         (EXIT (LOOP
                                 (COND
                                   ((NOT
                                     (NOT
                                      (NULL
                                       (LETT |pr| (CDR |pp|)
                                        |ILIST;removeDuplicates!;2$;26|))))
                                    (RETURN NIL))
                                   (T
                                    (COND
                                      ((SPADCALL (CAR |pr|) |f|
                                        (|getShellEntry| $ 59))
                                       (QRPLACD |pp| (CDR |pr|)))
                                      ('T (SETQ |pp| |pr|)))))))))))
             (EXIT |l|)))))) 

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
                     ((SPADCALL (CAR |p|) (CAR |q|) |f|)
                      (SEQ (LETT |r|
                                 (LETT |t| |p| |ILIST;merge!;M3$;28|)
                                 |ILIST;merge!;M3$;28|)
                           (EXIT (SETQ |p| (CDR |p|)))))
                     ('T
                      (SEQ (LETT |r|
                                 (LETT |t| |q| |ILIST;merge!;M3$;28|)
                                 |ILIST;merge!;M3$;28|)
                           (EXIT (SETQ |q| (CDR |q|))))))
                   (LOOP
                     (COND
                       ((NOT (COND
                               ((NULL |p|) NIL)
                               ('T (NOT (NULL |q|)))))
                        (RETURN NIL))
                       (T (COND
                            ((SPADCALL (CAR |p|) (CAR |q|) |f|)
                             (SEQ (QRPLACD |t| |p|)
                                  (LETT |t| |p| |ILIST;merge!;M3$;28|)
                                  (EXIT (SETQ |p| (CDR |p|)))))
                            ('T
                             (SEQ (QRPLACD |t| |q|)
                                  (LETT |t| |q| |ILIST;merge!;M3$;28|)
                                  (EXIT (SETQ |q| (CDR |q|)))))))))
                   (QRPLACD |t| (COND ((NULL |p|) |q|) ('T |p|)))
                   (EXIT |r|)))))))) 

(DEFUN |ILIST;split!;$I$;29| (|p| |n| $)
  (PROG (|q|)
    (RETURN
      (SEQ (COND
             ((< |n| 1) (|error| "index out of range"))
             ('T
              (SEQ (SETQ |p|
                         (|ILIST;rest;$Nni$;19| |p|
                             (LET ((#0=#:G1506 (- |n| 1)))
                               (|check-subtype| (NOT (MINUSP #0#))
                                   '(|NonNegativeInteger|) #0#))
                             $))
                   (LETT |q| (CDR |p|) |ILIST;split!;$I$;29|)
                   (QRPLACD |p| NIL) (EXIT |q|)))))))) 

(DEFUN |ILIST;mergeSort| (|f| |p| |n| $)
  (PROG (|l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL (|SPADfirst| (CDR |p|)) (|SPADfirst| |p|)
                     |f|)
                 (SETQ |p| (NREVERSE |p|))))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   ('T
                    (SEQ (LETT |l|
                               (LET ((#0=#:G1511 (QUOTIENT2 |n| 2)))
                                 (|check-subtype| (NOT (MINUSP #0#))
                                     '(|NonNegativeInteger|) #0#))
                               |ILIST;mergeSort|)
                         (LETT |q| (|ILIST;split!;$I$;29| |p| |l| $)
                               |ILIST;mergeSort|)
                         (SETQ |p| (|ILIST;mergeSort| |f| |p| |l| $))
                         (SETQ |q|
                               (|ILIST;mergeSort| |f| |q| (- |n| |l|)
                                   $))
                         (EXIT (|ILIST;merge!;M3$;28| |f| |p| |q| $)))))))))) 

(DEFUN |IndexedList| (&REST #0=#:G1520 &AUX #1=#:G1518)
  (DECLARE (SPECIAL |$ConstructorCache|))
  (DSETQ #1# #0#)
  (PROG (#2=#:G1519)
    (RETURN
      (COND
        ((SETQ #2#
               (|lassocShiftWithFunction| (|devaluateList| #1#)
                   (HGET |$ConstructorCache| '|IndexedList|)
                   '|domainEqualList|))
         (|CDRwithIncrement| #2#))
        ('T
         (UNWIND-PROTECT
           (PROG1 (APPLY (|function| |IndexedList;|) #1#)
             (SETQ #2# T))
           (COND
             ((NOT #2#) (HREM |$ConstructorCache| '|IndexedList|))))))))) 

(DEFUN |IndexedList;| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|IndexedList| |dv$1| |dv$2|))
         ($ (|newShell| 86))
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
                          (OR (|HasCategory| |#1| '(|BasicType|))
                              (|HasCategory| |#1| '(|OrderedSet|))
                              (|HasCategory| |#1| '(|SetCategory|)))
                          (|HasCategory| (|Integer|) '(|OrderedSet|))
                          (|HasCategory| |#1| '(|SetCategory|))
                          (|HasCategory| |#1|
                              (LIST '|CoercibleTo| '(|OutputForm|)))
                          (|HasCategory| |#1| '(|BasicType|))
                          (AND (|HasCategory| |#1| '(|SetCategory|))
                               (|HasCategory| |#1|
                                   (LIST '|Evalable|
                                    (|devaluate| |#1|))))))))
    (DECLARE (SPECIAL |$ConstructorCache|))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|IndexedList| (LIST |dv$1| |dv$2|)
        (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    (COND
      ((|testBitVector| |pv$| 9)
       (|setShellEntry| $ 50
           (CONS (|dispatchFunction| |ILIST;coerce;$Of;21|) $))))
    (COND
      ((|testBitVector| |pv$| 8)
       (PROGN
         (|setShellEntry| $ 54
             (CONS (|dispatchFunction| |ILIST;=;2$B;22|) $))
         (|setShellEntry| $ 58
             (CONS (|dispatchFunction| |ILIST;latex;$S;23|) $))
         (|setShellEntry| $ 60
             (CONS (|dispatchFunction| |ILIST;member?;S$B;24|) $)))))
    (COND
      ((|testBitVector| |pv$| 8)
       (|setShellEntry| $ 62
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
             (5 . =) (11 . |cyclic?|) |ILIST;copy;2$;20| (|OutputForm|)
             (|List| 37) (16 . |empty|) (20 . |cycleEntry|)
             (25 . |coerce|) (30 . |concat|) (36 . |reverse!|)
             (|List| $) (41 . |bracket|) (46 . |list|)
             (51 . |commaSeparate|) (56 . |overbar|) (61 . |concat!|)
             (67 . |coerce|) (72 . |true|) (76 . |false|) (80 . ~=)
             (86 . =) (|String|) (92 . |latex|) (97 . |concat|)
             (103 . |latex|) (108 . =) (114 . |member?|)
             |ILIST;concat!;3$;25| (120 . |removeDuplicates!|)
             (|Mapping| 11 6 6) |ILIST;sort!;M2$;27|
             |ILIST;merge!;M3$;28| (125 . |One|) (129 . <)
             (135 . |One|) (139 . -) |ILIST;split!;$I$;29| (145 . =)
             (151 . |quo|) (|Mapping| 6 6 6) (|Equation| 6) (|List| 74)
             (|Mapping| 11 6) (|Void|) (|UniversalSegment| 30) '"last"
             '"value" (|Mapping| 6 6) (|InputForm|) (|SingleInteger|)
             (|List| 30) (|Union| 6 '"failed"))
          '#(~= 157 |value| 163 |third| 168 |tail| 173 |swap!| 178
             |split!| 185 |sorted?| 191 |sort!| 202 |sort| 213 |size?|
             224 |setvalue!| 230 |setrest!| 236 |setlast!| 242
             |setfirst!| 248 |setelt| 254 |setchildren!| 296 |select!|
             302 |select| 308 |second| 314 |sample| 319 |reverse!| 323
             |reverse| 328 |rest| 333 |removeDuplicates!| 344
             |removeDuplicates| 349 |remove!| 354 |remove| 366 |reduce|
             378 |qsetelt!| 399 |qelt| 406 |possiblyInfinite?| 412
             |position| 417 |parts| 436 |nodes| 441 |node?| 446 |new|
             452 |more?| 458 |minIndex| 464 |min| 469 |merge!| 475
             |merge| 488 |members| 501 |member?| 506 |maxIndex| 512
             |max| 517 |map!| 523 |map| 529 |list| 542 |less?| 547
             |leaves| 553 |leaf?| 558 |latex| 563 |last| 568 |insert!|
             579 |insert| 593 |indices| 607 |index?| 612 |hash| 618
             |first| 623 |find| 634 |fill!| 640 |explicitlyFinite?| 646
             |every?| 651 |eval| 657 |eq?| 683 |entry?| 689 |entries|
             695 |empty?| 700 |empty| 705 |elt| 709 |distance| 752
             |delete!| 758 |delete| 770 |cyclic?| 782 |cycleTail| 787
             |cycleSplit!| 792 |cycleLength| 797 |cycleEntry| 802
             |count| 807 |copyInto!| 819 |copy| 826 |convert| 831
             |construct| 836 |concat!| 841 |concat| 853 |coerce| 876
             |children| 881 |child?| 886 |before?| 892 |any?| 898 >=
             904 > 910 = 916 <= 922 < 928 |#| 934)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 6
                    '(0 0 0 0 0 0 0 0 0 0 5 0 4 5 0 0 0 1 6 0 1 2 3))
                (CONS '#(|ListAggregate&| |StreamAggregate&|
                         |ExtensibleLinearAggregate&|
                         |FiniteLinearAggregate&|
                         |UnaryRecursiveAggregate&| |LinearAggregate&|
                         |RecursiveAggregate&| |IndexedAggregate&|
                         |Collection&| |HomogeneousAggregate&| NIL
                         |EltableAggregate&| |SetCategory&|
                         |OrderedType&| NIL |Aggregate&| NIL
                         |Evalable&| |BasicType&| NIL |InnerEvalable&|
                         NIL NIL)
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
                               (|OrderedSet|) (|EltableAggregate| 30 6)
                               (|SetCategory|) (|OrderedType|)
                               (|Eltable| 78 $$) (|Aggregate|)
                               (|Eltable| 30 6) (|Evalable| 6)
                               (|BasicType|) (|Type|)
                               (|InnerEvalable| 6 6) (|CoercibleTo| 37)
                               (|ConvertibleTo| 82))
                            (|makeByteWordVec2| 85
                                '(1 11 0 0 33 2 8 11 0 0 34 1 0 11 0 35
                                  0 38 0 39 1 0 0 0 40 1 6 37 0 41 2 38
                                  0 37 0 42 1 38 0 0 43 1 37 0 44 45 1
                                  38 0 37 46 1 37 0 44 47 1 37 0 0 48 2
                                  38 0 0 37 49 1 0 37 0 50 0 11 0 51 0
                                  11 0 52 2 6 11 0 0 53 2 0 11 0 0 54 1
                                  6 55 0 56 2 55 0 0 0 57 1 0 55 0 58 2
                                  6 11 0 0 59 2 0 11 6 0 60 1 0 0 0 62
                                  0 30 0 66 2 30 11 0 0 67 0 8 0 68 2
                                  30 0 0 0 69 2 30 11 0 0 71 2 30 0 0 0
                                  72 2 10 11 0 0 1 1 0 6 0 1 1 0 6 0 1
                                  1 0 0 0 1 3 0 77 0 30 30 1 2 0 0 0 30
                                  70 1 5 11 0 1 2 0 11 63 0 1 1 5 0 0 1
                                  2 0 0 63 0 64 1 5 0 0 1 2 0 0 63 0 1
                                  2 0 11 0 8 1 2 0 6 0 6 1 2 0 0 0 0 23
                                  2 0 6 0 6 1 2 0 6 0 6 21 3 0 6 0 30 6
                                  1 3 0 6 0 78 6 1 3 0 6 0 79 6 1 3 0 0
                                  0 19 0 24 3 0 6 0 14 6 22 3 0 6 0 80
                                  6 1 2 0 0 0 44 1 2 0 0 76 0 1 2 0 0
                                  76 0 1 1 0 6 0 1 0 0 0 1 1 0 0 0 28 1
                                  0 0 0 29 2 0 0 0 8 32 1 0 0 0 18 1 8
                                  0 0 62 1 8 0 0 1 2 8 0 6 0 1 2 0 0 76
                                  0 1 2 8 0 6 0 1 2 0 0 76 0 1 4 8 6 73
                                  0 6 6 1 2 0 6 73 0 1 3 0 6 73 0 6 1 3
                                  0 6 0 30 6 1 2 0 6 0 30 1 1 0 11 0 1
                                  3 8 30 6 0 30 1 2 8 30 6 0 1 2 0 30
                                  76 0 1 1 0 25 0 27 1 0 44 0 1 2 8 11
                                  0 0 1 2 0 0 8 6 1 2 0 11 0 8 1 1 7 30
                                  0 31 2 5 0 0 0 1 2 5 0 0 0 1 3 0 0 63
                                  0 0 65 2 5 0 0 0 1 3 0 0 63 0 0 1 1 0
                                  25 0 1 2 8 11 6 0 60 1 7 30 0 1 2 5 0
                                  0 0 1 2 0 0 81 0 1 3 0 0 73 0 0 1 2 0
                                  0 81 0 1 1 0 0 6 1 2 0 11 0 8 1 1 0
                                  25 0 1 1 0 11 0 1 1 8 55 0 58 2 0 0 0
                                  8 1 1 0 6 0 1 3 0 0 0 0 30 1 3 0 0 6
                                  0 30 1 3 0 0 0 0 30 1 3 0 0 6 0 30 1
                                  1 0 84 0 1 2 0 11 30 0 1 1 8 83 0 1 2
                                  0 0 0 8 1 1 0 6 0 13 2 0 85 76 0 1 2
                                  0 0 0 6 1 1 0 11 0 1 2 0 11 76 0 1 3
                                  11 0 0 6 6 1 3 11 0 0 25 25 1 2 11 0
                                  0 74 1 2 11 0 0 75 1 2 0 11 0 0 12 2
                                  8 11 6 0 1 1 0 25 0 1 1 0 11 0 17 0 0
                                  0 16 3 0 6 0 30 6 1 2 0 6 0 30 1 2 0
                                  0 0 78 1 2 0 6 0 79 1 2 0 0 0 19 20 2
                                  0 6 0 14 15 2 0 6 0 80 1 2 0 30 0 0 1
                                  2 0 0 0 30 1 2 0 0 0 78 1 2 0 0 0 78
                                  1 2 0 0 0 30 1 1 0 11 0 35 1 0 0 0 1
                                  1 0 0 0 1 1 0 8 0 1 1 0 0 0 40 2 8 8
                                  6 0 1 2 0 8 76 0 1 3 0 0 0 0 30 1 1 0
                                  0 0 36 1 3 82 0 1 1 0 0 25 26 2 0 0 0
                                  6 1 2 0 0 0 0 61 2 0 0 0 6 1 1 0 0 44
                                  1 2 0 0 6 0 10 2 0 0 0 0 1 1 9 37 0
                                  50 1 0 44 0 1 2 8 11 0 0 1 2 10 11 0
                                  0 1 2 0 11 76 0 1 2 5 11 0 0 1 2 5 11
                                  0 0 1 2 10 11 0 0 54 2 5 11 0 0 1 2 5
                                  11 0 0 1 1 0 8 0 9)))))
          '|lookupComplete|)) 
