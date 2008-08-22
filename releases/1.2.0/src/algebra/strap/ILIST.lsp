
(/VERSIONCHECK 2) 

(PUT '|ILIST;#;$Nni;1| '|SPADreplace| 'LENGTH) 

(DEFUN |ILIST;#;$Nni;1| (|x| $) (LENGTH |x|)) 

(PUT '|ILIST;concat;S2$;2| '|SPADreplace| 'CONS) 

(DEFUN |ILIST;concat;S2$;2| (|s| |x| $) (CONS |s| |x|)) 

(PUT '|ILIST;eq?;2$B;3| '|SPADreplace| 'EQ) 

(DEFUN |ILIST;eq?;2$B;3| (|x| |y| $) (EQ |x| |y|)) 

(PUT '|ILIST;first;$S;4| '|SPADreplace| '|SPADfirst|) 

(DEFUN |ILIST;first;$S;4| (|x| $) (|SPADfirst| |x|)) 

(PUT '|ILIST;elt;$firstS;5| '|SPADreplace|
     '(XLAM (|x| "first") (|SPADfirst| |x|))) 

(DEFUN |ILIST;elt;$firstS;5| (|x| T0 $) (|SPADfirst| |x|)) 

(PUT '|ILIST;empty;$;6| '|SPADreplace| '(XLAM NIL NIL)) 

(DEFUN |ILIST;empty;$;6| ($) NIL) 

(PUT '|ILIST;empty?;$B;7| '|SPADreplace| 'NULL) 

(DEFUN |ILIST;empty?;$B;7| (|x| $) (NULL |x|)) 

(PUT '|ILIST;rest;2$;8| '|SPADreplace| 'CDR) 

(DEFUN |ILIST;rest;2$;8| (|x| $) (CDR |x|)) 

(PUT '|ILIST;elt;$rest$;9| '|SPADreplace|
     '(XLAM (|x| "rest") (CDR |x|))) 

(DEFUN |ILIST;elt;$rest$;9| (|x| T1 $) (CDR |x|)) 

(DEFUN |ILIST;setfirst!;$2S;10| (|x| |s| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setelt;$first2S;11| (|x| T2 |s| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setrest!;3$;12| (|x| |y| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCDR (RPLACD |x| |y|))))) 

(DEFUN |ILIST;setelt;$rest2$;13| (|x| T3 |y| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCDR (RPLACD |x| |y|))))) 

(PUT '|ILIST;construct;L$;14| '|SPADreplace| '(XLAM (|l|) |l|)) 

(DEFUN |ILIST;construct;L$;14| (|l| $) |l|) 

(PUT '|ILIST;parts;$L;15| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DEFUN |ILIST;parts;$L;15| (|s| $) |s|) 

(PUT '|ILIST;reverse!;2$;16| '|SPADreplace| 'NREVERSE) 

(DEFUN |ILIST;reverse!;2$;16| (|x| $) (NREVERSE |x|)) 

(PUT '|ILIST;reverse;2$;17| '|SPADreplace| 'REVERSE) 

(DEFUN |ILIST;reverse;2$;17| (|x| $) (REVERSE |x|)) 

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
      (SEQ (LETT |y| (SPADCALL (|getShellEntry| $ 16))
                 |ILIST;copy;2$;20|)
           (SEQ (LETT |i| 0 |ILIST;copy;2$;20|) G190
                (COND
                  ((NULL (SPADCALL (NULL |x|) (|getShellEntry| $ 33)))
                   (GO G191)))
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
           (LETT |s| (SPADCALL |x| (|getShellEntry| $ 36))
                 |ILIST;coerce;$Of;21|)
           (SEQ G190 (COND ((NULL (NEQ |x| |s|)) (GO G191)))
                (SEQ (LETT |y|
                           (CONS (SPADCALL
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 13))
                                     (|getShellEntry| $ 38))
                                 |y|)
                           |ILIST;coerce;$Of;21|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 18))
                                 |ILIST;coerce;$Of;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (LETT |y| (NREVERSE |y|) |ILIST;coerce;$Of;21|)
           (EXIT (COND
                   ((SPADCALL |s| (|getShellEntry| $ 17))
                    (SPADCALL |y| (|getShellEntry| $ 40)))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 13))
                                    (|getShellEntry| $ 38))
                                   (|getShellEntry| $ 42))
                               |ILIST;coerce;$Of;21|)
                         (SEQ G190
                              (COND
                                ((NULL (NEQ |s|
                                        (SPADCALL |x|
                                         (|getShellEntry| $ 18))))
                                 (GO G191)))
                              (SEQ (LETT |x|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 18))
                                    |ILIST;coerce;$Of;21|)
                                   (EXIT
                                    (LETT |z|
                                     (CONS
                                      (SPADCALL
                                       (SPADCALL |x|
                                        (|getShellEntry| $ 13))
                                       (|getShellEntry| $ 38))
                                      |z|)
                                     |ILIST;coerce;$Of;21|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL
                                   (SPADCALL |y|
                                    (SPADCALL
                                     (SPADCALL (NREVERSE |z|)
                                      (|getShellEntry| $ 43))
                                     (|getShellEntry| $ 44))
                                    (|getShellEntry| $ 45))
                                   (|getShellEntry| $ 40))))))))))) 

(DEFUN |ILIST;=;2$B;22| (|x| |y| $)
  (PROG (#0=#:G1466)
    (RETURN
      (SEQ (EXIT (COND
                   ((EQ |x| |y|) 'T)
                   ('T
                    (SEQ (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((NULL |x|) 'NIL)
                                         ('T
                                          (SPADCALL (NULL |y|)
                                           (|getShellEntry| $ 33)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL (QCAR |x|) (QCAR |y|)
                                        (|getShellEntry| $ 47))
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
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |x|) (|getShellEntry| $ 33)))
                   (GO G191)))
                (SEQ (LETT |s|
                           (STRCONC |s|
                                    (SPADCALL (QCAR |x|)
                                     (|getShellEntry| $ 50)))
                           |ILIST;latex;$S;23|)
                     (LETT |x| (QCDR |x|) |ILIST;latex;$S;23|)
                     (EXIT (COND
                             ((NULL (NULL |x|))
                              (LETT |s| (STRCONC |s| ", ")
                                    |ILIST;latex;$S;23|)))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (STRCONC |s| " \\right]")))))) 

(DEFUN |ILIST;member?;S$B;24| (|s| |x| $)
  (PROG (#0=#:G1474)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND
                             ((NULL (SPADCALL (NULL |x|)
                                     (|getShellEntry| $ 33)))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL |s| (QCAR |x|)
                                          (|getShellEntry| $ 52))
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
                 (SEQ (PUSH (SPADCALL |y| (|getShellEntry| $ 13)) |x|)
                      (QRPLACD |x|
                               (SPADCALL |y| (|getShellEntry| $ 18)))
                      (EXIT |x|)))))
             ('T
              (SEQ (LETT |z| |x| |ILIST;concat!;3$;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL (QCDR |z|))
                                     (|getShellEntry| $ 33)))
                           (GO G191)))
                        (SEQ (EXIT (LETT |z| (QCDR |z|)
                                    |ILIST;concat!;3$;25|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (QRPLACD |z| |y|) (EXIT |x|)))))))) 

(DEFUN |ILIST;removeDuplicates!;2$;26| (|l| $)
  (PROG (|f| |p| |pr| |pp|)
    (RETURN
      (SEQ (LETT |p| |l| |ILIST;removeDuplicates!;2$;26|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |p|) (|getShellEntry| $ 33)))
                   (GO G191)))
                (SEQ (LETT |pp| |p| |ILIST;removeDuplicates!;2$;26|)
                     (LETT |f| (QCAR |p|)
                           |ILIST;removeDuplicates!;2$;26|)
                     (LETT |p| (QCDR |p|)
                           |ILIST;removeDuplicates!;2$;26|)
                     (EXIT (SEQ G190
                                (COND
                                  ((NULL
                                    (SPADCALL
                                     (NULL
                                      (LETT |pr| (QCDR |pp|)
                                       |ILIST;removeDuplicates!;2$;26|))
                                     (|getShellEntry| $ 33)))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (COND
                                        ((SPADCALL (QCAR |pr|) |f|
                                          (|getShellEntry| $ 52))
                                         (QRPLACD |pp| (QCDR |pr|)))
                                        ('T
                                         (LETT |pp| |pr|
                                          |ILIST;removeDuplicates!;2$;26|)))))
                                NIL (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |ILIST;sort!;M2$;27| (|f| |l| $)
  (|ILIST;mergeSort| |f| |l| (SPADCALL |l| (|getShellEntry| $ 9)) $)) 

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
                                   ('T
                                    (SPADCALL (NULL |q|)
                                     (|getShellEntry| $ 33)))))
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
  (PROG (#0=#:G1503 |q|)
    (RETURN
      (SEQ (COND
             ((< |n| 1) (|error| "index out of range"))
             ('T
              (SEQ (LETT |p|
                         (SPADCALL |p|
                             (PROG1 (LETT #0# (- |n| 1)
                                     |ILIST;split!;$I$;29|)
                               (|check-subtype| (>= #0# 0)
                                   '(|NonNegativeInteger|) #0#))
                             (|getShellEntry| $ 32))
                         |ILIST;split!;$I$;29|)
                   (LETT |q| (QCDR |p|) |ILIST;split!;$I$;29|)
                   (QRPLACD |p| NIL) (EXIT |q|)))))))) 

(DEFUN |ILIST;mergeSort| (|f| |p| |n| $)
  (PROG (#0=#:G1507 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (|getShellEntry| $ 18))
                         (|getShellEntry| $ 13))
                     (SPADCALL |p| (|getShellEntry| $ 13)) |f|)
                 (LETT |p| (SPADCALL |p| (|getShellEntry| $ 28))
                       |ILIST;mergeSort|)))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   ('T
                    (SEQ (LETT |l|
                               (PROG1 (LETT #0# (QUOTIENT2 |n| 2)
                                       |ILIST;mergeSort|)
                                 (|check-subtype| (>= #0# 0)
                                     '(|NonNegativeInteger|) #0#))
                               |ILIST;mergeSort|)
                         (LETT |q|
                               (SPADCALL |p| |l|
                                   (|getShellEntry| $ 59))
                               |ILIST;mergeSort|)
                         (LETT |p| (|ILIST;mergeSort| |f| |p| |l| $)
                               |ILIST;mergeSort|)
                         (LETT |q|
                               (|ILIST;mergeSort| |f| |q| (- |n| |l|)
                                   $)
                               |ILIST;mergeSort|)
                         (EXIT (SPADCALL |f| |p| |q|
                                   (|getShellEntry| $ 58))))))))))) 

(DEFUN |IndexedList| (&REST #0=#:G1519 &AUX #1=#:G1517)
  (DSETQ #1# #0#)
  (PROG ()
    (RETURN
      (PROG (#2=#:G1518)
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
        (LETT $ (|newShell| 73) . #0#)
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
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|))))
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|))))) . #0#))
        (|haddProp| |$ConstructorCache| '|IndexedList|
            (LIST |dv$1| |dv$2|) (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 9)
           (|setShellEntry| $ 46
               (CONS (|dispatchFunction| |ILIST;coerce;$Of;21|) $))))
        (COND
          ((|testBitVector| |pv$| 7)
           (PROGN
             (|setShellEntry| $ 48
                 (CONS (|dispatchFunction| |ILIST;=;2$B;22|) $))
             (|setShellEntry| $ 51
                 (CONS (|dispatchFunction| |ILIST;latex;$S;23|) $))
             (|setShellEntry| $ 53
                 (CONS (|dispatchFunction| |ILIST;member?;S$B;24|) $)))))
        (COND
          ((|testBitVector| |pv$| 7)
           (|setShellEntry| $ 55
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
             |ILIST;minIndex;$I;18| |ILIST;rest;$Nni$;19| (0 . |not|)
             (5 . |cyclic?|) |ILIST;copy;2$;20| (10 . |cycleEntry|)
             (|OutputForm|) (15 . |coerce|) (|List| $) (20 . |bracket|)
             (|List| 37) (25 . |list|) (30 . |commaSeparate|)
             (35 . |overbar|) (40 . |concat!|) (46 . |coerce|)
             (51 . ~=) (57 . =) (|String|) (63 . |latex|)
             (68 . |latex|) (73 . =) (79 . |member?|)
             |ILIST;concat!;3$;25| (85 . |removeDuplicates!|)
             (|Mapping| 11 6 6) |ILIST;sort!;M2$;27|
             |ILIST;merge!;M3$;28| |ILIST;split!;$I$;29|
             (|Mapping| 6 6 6) (|Equation| 6) (|List| 61)
             (|Mapping| 11 6) (|Void|) (|UniversalSegment| 30) '"last"
             '"value" (|Mapping| 6 6) (|InputForm|) (|SingleInteger|)
             (|List| 30) (|Union| 6 '"failed"))
          '#(~= 90 |value| 96 |third| 101 |tail| 106 |swap!| 111
             |split!| 118 |sorted?| 124 |sort!| 135 |sort| 146 |size?|
             157 |setvalue!| 163 |setrest!| 169 |setlast!| 175
             |setfirst!| 181 |setelt| 187 |setchildren!| 229 |select!|
             235 |select| 241 |second| 247 |sample| 252 |reverse!| 256
             |reverse| 261 |rest| 266 |removeDuplicates!| 277
             |removeDuplicates| 282 |remove!| 287 |remove| 299 |reduce|
             311 |qsetelt!| 332 |qelt| 339 |possiblyInfinite?| 345
             |position| 350 |parts| 369 |nodes| 374 |node?| 379 |new|
             385 |more?| 391 |minIndex| 397 |min| 402 |merge!| 408
             |merge| 421 |members| 434 |member?| 439 |maxIndex| 445
             |max| 450 |map!| 456 |map| 462 |list| 475 |less?| 480
             |leaves| 486 |leaf?| 491 |latex| 496 |last| 501 |insert!|
             512 |insert| 526 |indices| 540 |index?| 545 |hash| 551
             |first| 556 |find| 567 |fill!| 573 |explicitlyFinite?| 579
             |every?| 584 |eval| 590 |eq?| 616 |entry?| 622 |entries|
             628 |empty?| 633 |empty| 638 |elt| 642 |distance| 685
             |delete!| 691 |delete| 703 |cyclic?| 715 |cycleTail| 720
             |cycleSplit!| 725 |cycleLength| 730 |cycleEntry| 735
             |count| 740 |copyInto!| 752 |copy| 759 |convert| 764
             |construct| 769 |concat!| 774 |concat| 786 |coerce| 809
             |children| 814 |child?| 819 |any?| 825 >= 831 > 837 = 843
             <= 849 < 855 |#| 861)
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
                               (|CoercibleTo| 37) (|ConvertibleTo| 69)
                               (|BasicType|))
                            (|makeByteWordVec2| 72
                                '(1 11 0 0 33 1 0 11 0 34 1 0 0 0 36 1
                                  6 37 0 38 1 37 0 39 40 1 41 0 37 42 1
                                  37 0 39 43 1 37 0 0 44 2 41 0 0 37 45
                                  1 0 37 0 46 2 6 11 0 0 47 2 0 11 0 0
                                  48 1 6 49 0 50 1 0 49 0 51 2 6 11 0 0
                                  52 2 0 11 6 0 53 1 0 0 0 55 2 7 11 0
                                  0 1 1 0 6 0 1 1 0 6 0 1 1 0 0 0 1 3 0
                                  64 0 30 30 1 2 0 0 0 30 59 1 5 11 0 1
                                  2 0 11 56 0 1 1 5 0 0 1 2 0 0 56 0 57
                                  1 5 0 0 1 2 0 0 56 0 1 2 0 11 0 8 1 2
                                  0 6 0 6 1 2 0 0 0 0 23 2 0 6 0 6 1 2
                                  0 6 0 6 21 3 0 6 0 30 6 1 3 0 6 0 65
                                  6 1 3 0 6 0 66 6 1 3 0 0 0 19 0 24 3
                                  0 6 0 14 6 22 3 0 6 0 67 6 1 2 0 0 0
                                  39 1 2 0 0 63 0 1 2 0 0 63 0 1 1 0 6
                                  0 1 0 0 0 1 1 0 0 0 28 1 0 0 0 29 2 0
                                  0 0 8 32 1 0 0 0 18 1 7 0 0 55 1 7 0
                                  0 1 2 7 0 6 0 1 2 0 0 63 0 1 2 7 0 6
                                  0 1 2 0 0 63 0 1 4 7 6 60 0 6 6 1 2 0
                                  6 60 0 1 3 0 6 60 0 6 1 3 0 6 0 30 6
                                  1 2 0 6 0 30 1 1 0 11 0 1 2 7 30 6 0
                                  1 3 7 30 6 0 30 1 2 0 30 63 0 1 1 0
                                  25 0 27 1 0 39 0 1 2 7 11 0 0 1 2 0 0
                                  8 6 1 2 0 11 0 8 1 1 6 30 0 31 2 5 0
                                  0 0 1 2 5 0 0 0 1 3 0 0 56 0 0 58 2 5
                                  0 0 0 1 3 0 0 56 0 0 1 1 0 25 0 1 2 7
                                  11 6 0 53 1 6 30 0 1 2 5 0 0 0 1 2 0
                                  0 68 0 1 3 0 0 60 0 0 1 2 0 0 68 0 1
                                  1 0 0 6 1 2 0 11 0 8 1 1 0 25 0 1 1 0
                                  11 0 1 1 7 49 0 51 2 0 0 0 8 1 1 0 6
                                  0 1 3 0 0 6 0 30 1 3 0 0 0 0 30 1 3 0
                                  0 0 0 30 1 3 0 0 6 0 30 1 1 0 71 0 1
                                  2 0 11 30 0 1 1 7 70 0 1 2 0 0 0 8 1
                                  1 0 6 0 13 2 0 72 63 0 1 2 0 0 0 6 1
                                  1 0 11 0 1 2 0 11 63 0 1 3 8 0 0 6 6
                                  1 3 8 0 0 25 25 1 2 8 0 0 61 1 2 8 0
                                  0 62 1 2 0 11 0 0 12 2 7 11 6 0 1 1 0
                                  25 0 1 1 0 11 0 17 0 0 0 16 2 0 6 0
                                  30 1 3 0 6 0 30 6 1 2 0 0 0 65 1 2 0
                                  6 0 66 1 2 0 0 0 19 20 2 0 6 0 14 15
                                  2 0 6 0 67 1 2 0 30 0 0 1 2 0 0 0 65
                                  1 2 0 0 0 30 1 2 0 0 0 65 1 2 0 0 0
                                  30 1 1 0 11 0 34 1 0 0 0 1 1 0 0 0 1
                                  1 0 8 0 1 1 0 0 0 36 2 7 8 6 0 1 2 0
                                  8 63 0 1 3 0 0 0 0 30 1 1 0 0 0 35 1
                                  3 69 0 1 1 0 0 25 26 2 0 0 0 0 54 2 0
                                  0 0 6 1 1 0 0 39 1 2 0 0 0 6 1 2 0 0
                                  6 0 10 2 0 0 0 0 1 1 9 37 0 46 1 0 39
                                  0 1 2 7 11 0 0 1 2 0 11 63 0 1 2 5 11
                                  0 0 1 2 5 11 0 0 1 2 7 11 0 0 48 2 5
                                  11 0 0 1 2 5 11 0 0 1 1 0 8 0 9)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|IndexedList| '|isFunctor|
             '(((|coerce| ((|OutputForm|) $))
                (|has| |#1| (|CoercibleTo| (|OutputForm|))) (ELT $ 46))
               ((~= ((|Boolean|) $ $)) (|has| |#1| (|SetCategory|))
                (ELT $ NIL))
               ((= ((|Boolean|) $ $)) (|has| |#1| (|SetCategory|))
                (ELT $ 48))
               ((|hash| ((|SingleInteger|) $))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|latex| ((|String|) $)) (|has| |#1| (|SetCategory|))
                (ELT $ 51))
               ((|list| ($ |#1|)) T (ELT $ NIL))
               ((|concat!| ($ $ |#1|)) T (ELT $ NIL))
               ((|concat!| ($ $ $)) T (ELT $ 54))
               ((|delete!| ($ $ (|Integer|))) T (ELT $ NIL))
               ((|delete!| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ NIL))
               ((|remove!| ($ (|Mapping| (|Boolean|) |#1|) $)) T
                (ELT $ NIL))
               ((|insert!| ($ |#1| $ (|Integer|))) T (ELT $ NIL))
               ((|insert!| ($ $ $ (|Integer|))) T (ELT $ NIL))
               ((|merge!| ($ (|Mapping| (|Boolean|) |#1| |#1|) $ $)) T
                (ELT $ 58))
               ((|select!| ($ (|Mapping| (|Boolean|) |#1|) $)) T
                (ELT $ NIL))
               ((|remove!| ($ |#1| $)) (|has| |#1| (|SetCategory|))
                (ELT $ NIL))
               ((|removeDuplicates!| ($ $))
                (|has| |#1| (|SetCategory|)) (ELT $ 55))
               ((|merge!| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|merge| ($ (|Mapping| (|Boolean|) |#1| |#1|) $ $)) T
                (ELT $ NIL))
               ((|reverse| ($ $)) T (ELT $ 29))
               ((|sort| ($ (|Mapping| (|Boolean|) |#1| |#1|) $)) T
                (ELT $ NIL))
               ((|sorted?|
                    ((|Boolean|) (|Mapping| (|Boolean|) |#1| |#1|) $))
                T (ELT $ NIL))
               ((|position|
                    ((|Integer|) (|Mapping| (|Boolean|) |#1|) $))
                T (ELT $ NIL))
               ((|position| ((|Integer|) |#1| $))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|position| ((|Integer|) |#1| $ (|Integer|)))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|merge| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|sort| ($ $)) (|has| |#1| (|OrderedSet|)) (ELT $ NIL))
               ((|sorted?| ((|Boolean|) $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|copyInto!| ($ $ $ (|Integer|)))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|reverse!| ($ $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 28))
               ((|sort!| ($ (|Mapping| (|Boolean|) |#1| |#1|) $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 57))
               ((|sort!| ($ $))
                (AND (|has| $ (ATTRIBUTE |shallowlyMutable|))
                     (|has| |#1| (|OrderedSet|)))
                (ELT $ NIL))
               ((|min| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|max| ($ $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((<= ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((> ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((< ((|Boolean|) $ $)) (|has| |#1| (|OrderedSet|))
                (ELT $ NIL))
               ((|possiblyInfinite?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|explicitlyFinite?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|qsetelt!| (|#1| $ (|Integer|) |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|setelt| (|#1| $ (|Integer|) |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|qelt| (|#1| $ (|Integer|))) T (ELT $ NIL))
               ((|elt| (|#1| $ (|Integer|) |#1|)) T (ELT $ NIL))
               ((|elt| (|#1| $ (|Integer|))) T (ELT $ NIL))
               ((|entries| ((|List| |#1|) $)) T (ELT $ NIL))
               ((|index?| ((|Boolean|) (|Integer|) $)) T (ELT $ NIL))
               ((|indices| ((|List| (|Integer|)) $)) T (ELT $ NIL))
               ((|entry?| ((|Boolean|) |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|maxIndex| ((|Integer|) $))
                (|has| (|Integer|) (|OrderedSet|)) (ELT $ NIL))
               ((|minIndex| ((|Integer|) $))
                (|has| (|Integer|) (|OrderedSet|)) (ELT $ 31))
               ((|fill!| ($ $ |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|swap!| ((|Void|) $ (|Integer|) (|Integer|)))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|convert| ((|InputForm|) $))
                (|has| |#1| (|ConvertibleTo| (|InputForm|)))
                (ELT $ NIL))
               ((|removeDuplicates| ($ $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|remove| ($ |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|reduce|
                    (|#1| (|Mapping| |#1| |#1| |#1|) $ |#1| |#1|))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|select| ($ (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|remove| ($ (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|reduce| (|#1| (|Mapping| |#1| |#1| |#1|) $ |#1|))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|reduce| (|#1| (|Mapping| |#1| |#1| |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|find| ((|Union| |#1| "failed")
                         (|Mapping| (|Boolean|) |#1|) $))
                T (ELT $ NIL))
               ((|construct| ($ (|List| |#1|))) T (ELT $ 26))
               ((|new| ($ (|NonNegativeInteger|) |#1|)) T (ELT $ NIL))
               ((|concat| ($ $ |#1|)) T (ELT $ NIL))
               ((|concat| ($ (|List| $))) T (ELT $ NIL))
               ((|map| ($ (|Mapping| |#1| |#1| |#1|) $ $)) T
                (ELT $ NIL))
               ((|elt| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ NIL))
               ((|delete| ($ $ (|Integer|))) T (ELT $ NIL))
               ((|delete| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ NIL))
               ((|insert| ($ |#1| $ (|Integer|))) T (ELT $ NIL))
               ((|insert| ($ $ $ (|Integer|))) T (ELT $ NIL))
               ((|setelt|
                    (|#1| $ (|UniversalSegment| (|Integer|)) |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|split!| ($ $ (|Integer|)))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 59))
               ((|setelt| (|#1| $ "last" |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|setlast!| (|#1| $ |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|setelt| ($ $ "rest" $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 24))
               ((|setrest!| ($ $ $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 23))
               ((|setelt| (|#1| $ "first" |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 22))
               ((|setfirst!| (|#1| $ |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ 21))
               ((|cycleSplit!| ($ $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|cycleTail| ($ $)) T (ELT $ NIL))
               ((|cycleLength| ((|NonNegativeInteger|) $)) T
                (ELT $ NIL))
               ((|cycleEntry| ($ $)) T (ELT $ 36))
               ((|third| (|#1| $)) T (ELT $ NIL))
               ((|second| (|#1| $)) T (ELT $ NIL))
               ((|tail| ($ $)) T (ELT $ NIL))
               ((|last| ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|elt| (|#1| $ "last")) T (ELT $ NIL))
               ((|last| (|#1| $)) T (ELT $ NIL))
               ((|rest| ($ $ (|NonNegativeInteger|))) T (ELT $ 32))
               ((|elt| ($ $ "rest")) T (ELT $ 20))
               ((|rest| ($ $)) T (ELT $ 18))
               ((|first| ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|elt| (|#1| $ "first")) T (ELT $ 15))
               ((|first| (|#1| $)) T (ELT $ 13))
               ((|concat| ($ |#1| $)) T (ELT $ 10))
               ((|concat| ($ $ $)) T (ELT $ NIL))
               ((|setvalue!| (|#1| $ |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|setelt| (|#1| $ "value" |#1|))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|setchildren!| ($ $ (|List| $)))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|node?| ((|Boolean|) $ $))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|child?| ((|Boolean|) $ $))
                (|has| |#1| (|SetCategory|)) (ELT $ NIL))
               ((|distance| ((|Integer|) $ $)) T (ELT $ NIL))
               ((|leaves| ((|List| |#1|) $)) T (ELT $ NIL))
               ((|cyclic?| ((|Boolean|) $)) T (ELT $ 34))
               ((|elt| (|#1| $ "value")) T (ELT $ NIL))
               ((|value| (|#1| $)) T (ELT $ NIL))
               ((|leaf?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|nodes| ((|List| $) $)) T (ELT $ NIL))
               ((|children| ((|List| $) $)) T (ELT $ NIL))
               ((|eval| ($ $ (|List| |#1|) (|List| |#1|)))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|eval| ($ $ |#1| |#1|))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|eval| ($ $ (|Equation| |#1|)))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|eval| ($ $ (|List| (|Equation| |#1|))))
                (AND (|has| |#1| (|Evalable| |#1|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|member?| ((|Boolean|) |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ 53))
               ((|count| ((|NonNegativeInteger|) |#1| $))
                (AND (|has| $ (ATTRIBUTE |finiteAggregate|))
                     (|has| |#1| (|SetCategory|)))
                (ELT $ NIL))
               ((|members| ((|List| |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|parts| ((|List| |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ 27))
               ((|count| ((|NonNegativeInteger|)
                          (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|every?| ((|Boolean|) (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|any?| ((|Boolean|) (|Mapping| (|Boolean|) |#1|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ NIL))
               ((|map!| ($ (|Mapping| |#1| |#1|) $))
                (|has| $ (ATTRIBUTE |shallowlyMutable|)) (ELT $ NIL))
               ((|map| ($ (|Mapping| |#1| |#1|) $)) T (ELT $ NIL))
               ((|#| ((|NonNegativeInteger|) $))
                (|has| $ (ATTRIBUTE |finiteAggregate|)) (ELT $ 9))
               ((|sample| ($)) T (CONST $ NIL))
               ((|size?| ((|Boolean|) $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|more?| ((|Boolean|) $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|less?| ((|Boolean|) $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|empty?| ((|Boolean|) $)) T (ELT $ 17))
               ((|empty| ($)) T (ELT $ 16))
               ((|copy| ($ $)) T (ELT $ 35))
               ((|eq?| ((|Boolean|) $ $)) T (ELT $ 12)))
             (|addModemap| '|IndexedList| '(|IndexedList| |#1| |#2|)
                 '((|ListAggregate| |#1|) (|Type|) (|Integer|)) T
                 '|IndexedList|
                 (|put| '|IndexedList| '|mode|
                        '(|Mapping| (|ListAggregate| |#1|) (|Type|)
                             (|Integer|))
                        |$CategoryFrame|)))) 
