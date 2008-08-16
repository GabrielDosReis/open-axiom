
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
    ((SPADCALL |x| (QREFELT $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setelt;$first2S;11| (|x| T2 |s| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCAR (RPLACA |x| |s|))))) 

(DEFUN |ILIST;setrest!;3$;12| (|x| |y| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 17))
     (|error| "Cannot update an empty list"))
    ('T (QCDR (RPLACD |x| |y|))))) 

(DEFUN |ILIST;setelt;$rest2$;13| (|x| T3 |y| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 17))
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

(DEFUN |ILIST;minIndex;$I;18| (|x| $) (QREFELT $ 7)) 

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
      (SEQ (LETT |y| (SPADCALL (QREFELT $ 16)) |ILIST;copy;2$;20|)
           (SEQ (LETT |i| 0 |ILIST;copy;2$;20|) G190
                (COND
                  ((NULL (SPADCALL (NULL |x|) (QREFELT $ 33)))
                   (GO G191)))
                (SEQ (COND
                       ((EQ |i| 1000)
                        (COND
                          ((SPADCALL |x| (QREFELT $ 34))
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
           (LETT |s| (SPADCALL |x| (QREFELT $ 36))
                 |ILIST;coerce;$Of;21|)
           (SEQ G190 (COND ((NULL (NEQ |x| |s|)) (GO G191)))
                (SEQ (LETT |y|
                           (CONS (SPADCALL
                                     (SPADCALL |x| (QREFELT $ 13))
                                     (QREFELT $ 38))
                                 |y|)
                           |ILIST;coerce;$Of;21|)
                     (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 18))
                                 |ILIST;coerce;$Of;21|)))
                NIL (GO G190) G191 (EXIT NIL))
           (LETT |y| (NREVERSE |y|) |ILIST;coerce;$Of;21|)
           (EXIT (COND
                   ((SPADCALL |s| (QREFELT $ 17))
                    (SPADCALL |y| (QREFELT $ 40)))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL
                                   (SPADCALL
                                    (SPADCALL |x| (QREFELT $ 13))
                                    (QREFELT $ 38))
                                   (QREFELT $ 42))
                               |ILIST;coerce;$Of;21|)
                         (SEQ G190
                              (COND
                                ((NULL (NEQ |s|
                                        (SPADCALL |x| (QREFELT $ 18))))
                                 (GO G191)))
                              (SEQ (LETT |x|
                                    (SPADCALL |x| (QREFELT $ 18))
                                    |ILIST;coerce;$Of;21|)
                                   (EXIT
                                    (LETT |z|
                                     (CONS
                                      (SPADCALL
                                       (SPADCALL |x| (QREFELT $ 13))
                                       (QREFELT $ 38))
                                      |z|)
                                     |ILIST;coerce;$Of;21|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT (SPADCALL
                                   (SPADCALL |y|
                                    (SPADCALL
                                     (SPADCALL (NREVERSE |z|)
                                      (QREFELT $ 43))
                                     (QREFELT $ 44))
                                    (QREFELT $ 45))
                                   (QREFELT $ 40))))))))))) 

(DEFUN |ILIST;=;2$B;22| (|x| |y| $)
  (PROG (#0=#:G1469)
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
                                           (QREFELT $ 33)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((NULL
                                        (SPADCALL (QCAR |x|) (QCAR |y|)
                                         (QREFELT $ 47)))
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
                  ((NULL (SPADCALL (NULL |x|) (QREFELT $ 33)))
                   (GO G191)))
                (SEQ (LETT |s|
                           (STRCONC |s|
                                    (SPADCALL (QCAR |x|)
                                     (QREFELT $ 50)))
                           |ILIST;latex;$S;23|)
                     (LETT |x| (QCDR |x|) |ILIST;latex;$S;23|)
                     (EXIT (COND
                             ((NULL (NULL |x|))
                              (LETT |s| (STRCONC |s| ", ")
                                    |ILIST;latex;$S;23|)))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (STRCONC |s| " \\right]")))))) 

(DEFUN |ILIST;member?;S$B;24| (|s| |x| $)
  (PROG (#0=#:G1477)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND
                             ((NULL (SPADCALL (NULL |x|)
                                     (QREFELT $ 33)))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL |s| (QCAR |x|)
                                          (QREFELT $ 47))
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
                 (SEQ (PUSH (SPADCALL |y| (QREFELT $ 13)) |x|)
                      (QRPLACD |x| (SPADCALL |y| (QREFELT $ 18)))
                      (EXIT |x|)))))
             ('T
              (SEQ (LETT |z| |x| |ILIST;concat!;3$;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL (QCDR |z|))
                                     (QREFELT $ 33)))
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
                  ((NULL (SPADCALL (NULL |p|) (QREFELT $ 33)))
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
                                     (QREFELT $ 33)))
                                   (GO G191)))
                                (SEQ (EXIT
                                      (COND
                                        ((SPADCALL (QCAR |pr|) |f|
                                          (QREFELT $ 47))
                                         (QRPLACD |pp| (QCDR |pr|)))
                                        ('T
                                         (LETT |pp| |pr|
                                          |ILIST;removeDuplicates!;2$;26|)))))
                                NIL (GO G190) G191 (EXIT NIL))))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |ILIST;sort!;M2$;27| (|f| |l| $)
  (|ILIST;mergeSort| |f| |l| (SPADCALL |l| (QREFELT $ 9)) $)) 

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
                                     (QREFELT $ 33)))))
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
  (PROG (#0=#:G1506 |q|)
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
                             (QREFELT $ 32))
                         |ILIST;split!;$I$;29|)
                   (LETT |q| (QCDR |p|) |ILIST;split!;$I$;29|)
                   (QRPLACD |p| NIL) (EXIT |q|)))))))) 

(DEFUN |ILIST;mergeSort| (|f| |p| |n| $)
  (PROG (#0=#:G1510 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (QREFELT $ 18))
                         (QREFELT $ 13))
                     (SPADCALL |p| (QREFELT $ 13)) |f|)
                 (LETT |p| (SPADCALL |p| (QREFELT $ 28))
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
                         (LETT |q| (SPADCALL |p| |l| (QREFELT $ 58))
                               |ILIST;mergeSort|)
                         (LETT |p| (|ILIST;mergeSort| |f| |p| |l| $)
                               |ILIST;mergeSort|)
                         (LETT |q|
                               (|ILIST;mergeSort| |f| |q| (- |n| |l|)
                                   $)
                               |ILIST;mergeSort|)
                         (EXIT (SPADCALL |f| |p| |q| (QREFELT $ 57))))))))))) 

(DEFUN |IndexedList| (&REST #0=#:G1525 &AUX #1=#:G1523)
  (DSETQ #1# #0#)
  (PROG ()
    (RETURN
      (PROG (#2=#:G1524)
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
  (PROG (|dv$1| |dv$2| |dv$| $ #0=#:G1522 #1=#:G1520 |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #2=(|IndexedList|))
        (LETT |dv$2| (|devaluate| |#2|) . #2#)
        (LETT |dv$| (LIST '|IndexedList| |dv$1| |dv$2|) . #2#)
        (LETT $ (GETREFV 72) . #2#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#1|
                                '(|ConvertibleTo| (|InputForm|)))
                            (|HasCategory| |#1| '(|OrderedSet|))
                            (|HasCategory| (|Integer|) '(|OrderedSet|))
                            (LETT #0#
                                  (|HasCategory| |#1| '(|SetCategory|)) . #2#)
                            (OR (|HasCategory| |#1| '(|OrderedSet|))
                                #0#)
                            (AND #0#
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|))))
                            (OR (AND (|HasCategory| |#1|
                                      '(|OrderedSet|))
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|))))
                                (AND #0#
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|)))))
                            (LETT #1#
                                  (|HasCategory| |#1|
                                      '(|CoercibleTo| (|OutputForm|))) . #2#)
                            (OR (AND #0#
                                     (|HasCategory| |#1|
                                      (LIST '|Evalable|
                                       (|devaluate| |#1|))))
                                #1#))) . #2#))
        (|haddProp| |$ConstructorCache| '|IndexedList|
            (LIST |dv$1| |dv$2|) (CONS 1 $))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (QSETREFV $ 7 |#2|)
        (COND
          ((|testBitVector| |pv$| 8)
           (QSETREFV $ 46
               (CONS (|dispatchFunction| |ILIST;coerce;$Of;21|) $))))
        (COND
          ((|testBitVector| |pv$| 4)
           (PROGN
             (QSETREFV $ 48
                 (CONS (|dispatchFunction| |ILIST;=;2$B;22|) $))
             (QSETREFV $ 51
                 (CONS (|dispatchFunction| |ILIST;latex;$S;23|) $))
             (QSETREFV $ 52
                 (CONS (|dispatchFunction| |ILIST;member?;S$B;24|) $)))))
        (COND
          ((|testBitVector| |pv$| 4)
           (QSETREFV $ 54
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
             (35 . |overbar|) (40 . |concat!|) (46 . |coerce|) (51 . =)
             (57 . =) (|String|) (63 . |latex|) (68 . |latex|)
             (73 . |member?|) |ILIST;concat!;3$;25|
             (79 . |removeDuplicates!|) (|Mapping| 11 6 6)
             |ILIST;sort!;M2$;27| |ILIST;merge!;M3$;28|
             |ILIST;split!;$I$;29| (|Mapping| 6 6 6) (|Equation| 6)
             (|List| 60) (|Mapping| 11 6) (|Void|)
             (|UniversalSegment| 30) '"last" '"value" (|Mapping| 6 6)
             (|InputForm|) (|SingleInteger|) (|List| 30)
             (|Union| 6 '"failed"))
          '#(~= 84 |value| 90 |third| 95 |tail| 100 |swap!| 105
             |split!| 112 |sorted?| 118 |sort!| 129 |sort| 140 |size?|
             151 |setvalue!| 157 |setrest!| 163 |setlast!| 169
             |setfirst!| 175 |setelt| 181 |setchildren!| 223 |select!|
             229 |select| 235 |second| 241 |sample| 246 |reverse!| 250
             |reverse| 255 |rest| 260 |removeDuplicates!| 271
             |removeDuplicates| 276 |remove!| 281 |remove| 293 |reduce|
             305 |qsetelt!| 326 |qelt| 333 |possiblyInfinite?| 339
             |position| 344 |parts| 363 |nodes| 368 |node?| 373 |new|
             379 |more?| 385 |minIndex| 391 |min| 396 |merge!| 402
             |merge| 415 |members| 428 |member?| 433 |maxIndex| 439
             |max| 444 |map!| 450 |map| 456 |list| 469 |less?| 474
             |leaves| 480 |leaf?| 485 |latex| 490 |last| 495 |insert!|
             506 |insert| 520 |indices| 534 |index?| 539 |hash| 545
             |first| 550 |find| 561 |fill!| 567 |explicitlyFinite?| 573
             |every?| 578 |eval| 584 |eq?| 610 |entry?| 616 |entries|
             622 |empty?| 627 |empty| 632 |elt| 636 |distance| 679
             |delete!| 685 |delete| 697 |cyclic?| 709 |cycleTail| 714
             |cycleSplit!| 719 |cycleLength| 724 |cycleEntry| 729
             |count| 734 |copyInto!| 746 |copy| 753 |convert| 758
             |construct| 763 |concat!| 768 |concat| 780 |coerce| 803
             |children| 808 |child?| 813 |any?| 819 >= 825 > 831 = 837
             <= 843 < 849 |#| 855)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 9
                    '(0 0 0 0 0 0 0 0 0 0 2 0 0 7 5 0 0 7 9 1 5))
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
                               (|CoercibleTo| 37) (|ConvertibleTo| 68)
                               (|BasicType|))
                            (|makeByteWordVec2| 71
                                '(1 11 0 0 33 1 0 11 0 34 1 0 0 0 36 1
                                  6 37 0 38 1 37 0 39 40 1 41 0 37 42 1
                                  37 0 39 43 1 37 0 0 44 2 41 0 0 37 45
                                  1 0 37 0 46 2 6 11 0 0 47 2 0 11 0 0
                                  48 1 6 49 0 50 1 0 49 0 51 2 0 11 6 0
                                  52 1 0 0 0 54 2 4 11 0 0 1 1 0 6 0 1
                                  1 0 6 0 1 1 0 0 0 1 3 0 63 0 30 30 1
                                  2 0 0 0 30 58 1 2 11 0 1 2 0 11 55 0
                                  1 1 2 0 0 1 2 0 0 55 0 56 1 2 0 0 1 2
                                  0 0 55 0 1 2 0 11 0 8 1 2 0 6 0 6 1 2
                                  0 0 0 0 23 2 0 6 0 6 1 2 0 6 0 6 21 3
                                  0 6 0 30 6 1 3 0 6 0 64 6 1 3 0 6 0
                                  65 6 1 3 0 0 0 19 0 24 3 0 6 0 14 6
                                  22 3 0 6 0 66 6 1 2 0 0 0 39 1 2 0 0
                                  62 0 1 2 0 0 62 0 1 1 0 6 0 1 0 0 0 1
                                  1 0 0 0 28 1 0 0 0 29 2 0 0 0 8 32 1
                                  0 0 0 18 1 4 0 0 54 1 4 0 0 1 2 4 0 6
                                  0 1 2 0 0 62 0 1 2 4 0 6 0 1 2 0 0 62
                                  0 1 4 4 6 59 0 6 6 1 2 0 6 59 0 1 3 0
                                  6 59 0 6 1 3 0 6 0 30 6 1 2 0 6 0 30
                                  1 1 0 11 0 1 2 4 30 6 0 1 3 4 30 6 0
                                  30 1 2 0 30 62 0 1 1 0 25 0 27 1 0 39
                                  0 1 2 4 11 0 0 1 2 0 0 8 6 1 2 0 11 0
                                  8 1 1 3 30 0 31 2 2 0 0 0 1 2 2 0 0 0
                                  1 3 0 0 55 0 0 57 2 2 0 0 0 1 3 0 0
                                  55 0 0 1 1 0 25 0 1 2 4 11 6 0 52 1 3
                                  30 0 1 2 2 0 0 0 1 2 0 0 67 0 1 3 0 0
                                  59 0 0 1 2 0 0 67 0 1 1 0 0 6 1 2 0
                                  11 0 8 1 1 0 25 0 1 1 0 11 0 1 1 4 49
                                  0 51 2 0 0 0 8 1 1 0 6 0 1 3 0 0 6 0
                                  30 1 3 0 0 0 0 30 1 3 0 0 0 0 30 1 3
                                  0 0 6 0 30 1 1 0 70 0 1 2 0 11 30 0 1
                                  1 4 69 0 1 2 0 0 0 8 1 1 0 6 0 13 2 0
                                  71 62 0 1 2 0 0 0 6 1 1 0 11 0 1 2 0
                                  11 62 0 1 3 6 0 0 6 6 1 3 6 0 0 25 25
                                  1 2 6 0 0 60 1 2 6 0 0 61 1 2 0 11 0
                                  0 12 2 4 11 6 0 1 1 0 25 0 1 1 0 11 0
                                  17 0 0 0 16 2 0 6 0 30 1 3 0 6 0 30 6
                                  1 2 0 0 0 64 1 2 0 6 0 65 1 2 0 0 0
                                  19 20 2 0 6 0 14 15 2 0 6 0 66 1 2 0
                                  30 0 0 1 2 0 0 0 64 1 2 0 0 0 30 1 2
                                  0 0 0 64 1 2 0 0 0 30 1 1 0 11 0 34 1
                                  0 0 0 1 1 0 0 0 1 1 0 8 0 1 1 0 0 0
                                  36 2 4 8 6 0 1 2 0 8 62 0 1 3 0 0 0 0
                                  30 1 1 0 0 0 35 1 1 68 0 1 1 0 0 25
                                  26 2 0 0 0 0 53 2 0 0 0 6 1 1 0 0 39
                                  1 2 0 0 0 6 1 2 0 0 6 0 10 2 0 0 0 0
                                  1 1 8 37 0 46 1 0 39 0 1 2 4 11 0 0 1
                                  2 0 11 62 0 1 2 2 11 0 0 1 2 2 11 0 0
                                  1 2 4 11 0 0 48 2 2 11 0 0 1 2 2 11 0
                                  0 1 1 0 8 0 9)))))
          '|lookupComplete|)) 
