
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |URAGG-;elt;AfirstS;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |URAGG-;elt;AlastS;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |URAGG-;elt;ArestA;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;second;AS;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;third;AS;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |URAGG-;cyclic?;AB;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;last;AS;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |URAGG-;nodes;AL;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |URAGG-;children;AL;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |URAGG-;leaf?;AB;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;value;AS;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Boolean|)
                |URAGG-;less?;ANniB;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Boolean|)
                |URAGG-;more?;ANniB;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Boolean|)
                |URAGG-;size?;ANniB;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |URAGG-;#;ANni;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;tail;2A;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;findCycle|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;cycleTail;2A;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;cycleEntry;2A;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |URAGG-;cycleLength;ANni;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |URAGG-;rest;ANniA;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |URAGG-;last;ANniA;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |URAGG-;=;2AB;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |URAGG-;node?;2AB;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |URAGG-;setelt;Afirst2S;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |URAGG-;setelt;Alast2S;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |URAGG-;setelt;Arest2A;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |URAGG-;concat;3A;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |URAGG-;setlast!;A2S;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |URAGG-;setchildren!;ALA;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |URAGG-;setvalue!;A2S;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |URAGG-;split!;AIA;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |URAGG-;cycleSplit!;2A;33|)) 

(DEFUN |URAGG-;elt;AfirstS;1| (|x| T0 $)
  (SPADCALL |x| (|getShellEntry| $ 8))) 

(DEFUN |URAGG-;elt;AlastS;2| (|x| T1 $)
  (SPADCALL |x| (|getShellEntry| $ 11))) 

(DEFUN |URAGG-;elt;ArestA;3| (|x| T2 $)
  (SPADCALL |x| (|getShellEntry| $ 14))) 

(DEFUN |URAGG-;second;AS;4| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 14))
      (|getShellEntry| $ 8))) 

(DEFUN |URAGG-;third;AS;5| (|x| $)
  (SPADCALL
      (SPADCALL (SPADCALL |x| (|getShellEntry| $ 14))
                (|getShellEntry| $ 14))
      (|getShellEntry| $ 8))) 

(DEFUN |URAGG-;cyclic?;AB;6| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 20)) NIL)
    ('T
     (NOT (SPADCALL (|URAGG-;findCycle| |x| $) (|getShellEntry| $ 20)))))) 

(DEFUN |URAGG-;last;AS;7| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 24))
      (|getShellEntry| $ 8))) 

(DEFUN |URAGG-;nodes;AL;8| (|x| $)
  (PROG (|l|)
    (RETURN
      (SEQ (LETT |l| NIL |URAGG-;nodes;AL;8|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |x| (|getShellEntry| $ 20))))
                   (GO G191)))
                (SEQ (LETT |l| (CONS |x| |l|) |URAGG-;nodes;AL;8|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 14))
                                 |URAGG-;nodes;AL;8|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (NREVERSE |l|)))))) 

(DEFUN |URAGG-;children;AL;9| (|x| $)
  (PROG (|l|)
    (RETURN
      (SEQ (LETT |l| NIL |URAGG-;children;AL;9|)
           (EXIT (COND
                   ((SPADCALL |x| (|getShellEntry| $ 20)) |l|)
                   ('T
                    (CONS (SPADCALL |x| (|getShellEntry| $ 14)) |l|)))))))) 

(DEFUN |URAGG-;leaf?;AB;10| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 20))) 

(DEFUN |URAGG-;value;AS;11| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 20))
     (|error| "value of empty object"))
    ('T (SPADCALL |x| (|getShellEntry| $ 8))))) 

(DEFUN |URAGG-;less?;ANniB;12| (|l| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (LETT |i| |n| |URAGG-;less?;ANniB;12|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| 0)
                            (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                           ('T NIL)))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |l| (|getShellEntry| $ 14))
                           |URAGG-;less?;ANniB;12|)
                     (EXIT (LETT |i| (- |i| 1) |URAGG-;less?;ANniB;12|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (> |i| 0)))))) 

(DEFUN |URAGG-;more?;ANniB;13| (|l| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (LETT |i| |n| |URAGG-;more?;ANniB;13|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((> |i| 0)
                            (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                           ('T NIL)))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |l| (|getShellEntry| $ 14))
                           |URAGG-;more?;ANniB;13|)
                     (EXIT (LETT |i| (- |i| 1) |URAGG-;more?;ANniB;13|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((ZEROP |i|)
                    (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                   ('T NIL))))))) 

(DEFUN |URAGG-;size?;ANniB;14| (|l| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (LETT |i| |n| |URAGG-;size?;ANniB;14|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |l| (|getShellEntry| $ 20)) NIL)
                           ('T (> |i| 0))))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |l| (|getShellEntry| $ 14))
                           |URAGG-;size?;ANniB;14|)
                     (EXIT (LETT |i| (- |i| 1) |URAGG-;size?;ANniB;14|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |l| (|getShellEntry| $ 20)) (ZEROP |i|))
                   ('T NIL))))))) 

(DEFUN |URAGG-;#;ANni;15| (|x| $)
  (PROG (|k|)
    (RETURN
      (SEQ (SEQ (LETT |k| 0 |URAGG-;#;ANni;15|) G190
                (COND
                  ((NULL (NOT (SPADCALL |x| (|getShellEntry| $ 20))))
                   (GO G191)))
                (SEQ (COND
                       ((EQL |k| 1000)
                        (COND
                          ((SPADCALL |x| (|getShellEntry| $ 48))
                           (EXIT (|error| "cyclic list"))))))
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 14))
                                 |URAGG-;#;ANni;15|)))
                (SETQ |k| (QSADD1 |k|)) (GO G190) G191 (EXIT NIL))
           (EXIT |k|))))) 

(DEFUN |URAGG-;tail;2A;16| (|x| $)
  (PROG (|k| |y|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (|getShellEntry| $ 20))
              (|error| "empty list"))
             ('T
              (SEQ (LETT |y| (SPADCALL |x| (|getShellEntry| $ 14))
                         |URAGG-;tail;2A;16|)
                   (SEQ (LETT |k| 0 |URAGG-;tail;2A;16|) G190
                        (COND
                          ((NULL (NOT (SPADCALL |y|
                                       (|getShellEntry| $ 20))))
                           (GO G191)))
                        (SEQ (COND
                               ((EQL |k| 1000)
                                (COND
                                  ((SPADCALL |x|
                                    (|getShellEntry| $ 48))
                                   (EXIT (|error| "cyclic list"))))))
                             (EXIT (LETT |y|
                                    (SPADCALL
                                     (LETT |x| |y| |URAGG-;tail;2A;16|)
                                     (|getShellEntry| $ 14))
                                    |URAGG-;tail;2A;16|)))
                        (SETQ |k| (QSADD1 |k|)) (GO G190) G191
                        (EXIT NIL))
                   (EXIT |x|)))))))) 

(DEFUN |URAGG-;findCycle| (|x| $)
  (PROG (#0=#:G1475 |y|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |y| (SPADCALL |x| (|getShellEntry| $ 14))
                            |URAGG-;findCycle|)
                      (SEQ G190
                           (COND
                             ((NULL (NOT
                                     (SPADCALL |y|
                                      (|getShellEntry| $ 20))))
                              (GO G191)))
                           (SEQ (COND
                                  ((SPADCALL |x| |y|
                                    (|getShellEntry| $ 51))
                                   (PROGN
                                     (LETT #0# |x| |URAGG-;findCycle|)
                                     (GO #0#))))
                                (LETT |x|
                                      (SPADCALL |x|
                                       (|getShellEntry| $ 14))
                                      |URAGG-;findCycle|)
                                (LETT |y|
                                      (SPADCALL |y|
                                       (|getShellEntry| $ 14))
                                      |URAGG-;findCycle|)
                                (COND
                                  ((SPADCALL |y|
                                    (|getShellEntry| $ 20))
                                   (PROGN
                                     (LETT #0# |y| |URAGG-;findCycle|)
                                     (GO #0#))))
                                (COND
                                  ((SPADCALL |x| |y|
                                    (|getShellEntry| $ 51))
                                   (PROGN
                                     (LETT #0# |y| |URAGG-;findCycle|)
                                     (GO #0#))))
                                (EXIT (LETT |y|
                                       (SPADCALL |y|
                                        (|getShellEntry| $ 14))
                                       |URAGG-;findCycle|)))
                           NIL (GO G190) G191 (EXIT NIL))
                      (EXIT |y|)))
           #0# (EXIT #0#))))) 

(DEFUN |URAGG-;cycleTail;2A;18| (|x| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (COND
             ((SPADCALL
                  (LETT |y|
                        (LETT |x| (SPADCALL |x| (|getShellEntry| $ 52))
                              |URAGG-;cycleTail;2A;18|)
                        |URAGG-;cycleTail;2A;18|)
                  (|getShellEntry| $ 20))
              |x|)
             ('T
              (SEQ (LETT |z| (SPADCALL |x| (|getShellEntry| $ 14))
                         |URAGG-;cycleTail;2A;18|)
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL |x| |z|
                                       (|getShellEntry| $ 51))))
                           (GO G191)))
                        (SEQ (LETT |y| |z| |URAGG-;cycleTail;2A;18|)
                             (EXIT (LETT |z|
                                    (SPADCALL |z|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleTail;2A;18|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |y|)))))))) 

(DEFUN |URAGG-;cycleEntry;2A;19| (|x| $)
  (PROG (|l| |z| |k| |y|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |x| (|getShellEntry| $ 20)) |x|)
             ((SPADCALL
                  (LETT |y| (|URAGG-;findCycle| |x| $)
                        |URAGG-;cycleEntry;2A;19|)
                  (|getShellEntry| $ 20))
              |y|)
             ('T
              (SEQ (LETT |z| (SPADCALL |y| (|getShellEntry| $ 14))
                         |URAGG-;cycleEntry;2A;19|)
                   (SEQ (LETT |l| 1 |URAGG-;cycleEntry;2A;19|) G190
                        (COND
                          ((NULL (NOT (SPADCALL |y| |z|
                                       (|getShellEntry| $ 51))))
                           (GO G191)))
                        (LETT |z| (SPADCALL |z| (|getShellEntry| $ 14))
                              |URAGG-;cycleEntry;2A;19|)
                        (SETQ |l| (QSADD1 |l|)) (GO G190) G191
                        (EXIT NIL))
                   (LETT |y| |x| |URAGG-;cycleEntry;2A;19|)
                   (SEQ (LETT |k| 1 |URAGG-;cycleEntry;2A;19|) G190
                        (COND ((QSGREATERP |k| |l|) (GO G191)))
                        (LETT |y| (SPADCALL |y| (|getShellEntry| $ 14))
                              |URAGG-;cycleEntry;2A;19|)
                        (SETQ |k| (QSADD1 |k|)) (GO G190) G191
                        (EXIT NIL))
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL |x| |y|
                                       (|getShellEntry| $ 51))))
                           (GO G191)))
                        (SEQ (LETT |x|
                                   (SPADCALL |x|
                                    (|getShellEntry| $ 14))
                                   |URAGG-;cycleEntry;2A;19|)
                             (EXIT (LETT |y|
                                    (SPADCALL |y|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleEntry;2A;19|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |x|)))))))) 

(DEFUN |URAGG-;cycleLength;ANni;20| (|x| $)
  (PROG (|k| |y|)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |x| (|getShellEntry| $ 20))
                  (SPADCALL
                      (LETT |x| (|URAGG-;findCycle| |x| $)
                            |URAGG-;cycleLength;ANni;20|)
                      (|getShellEntry| $ 20)))
              0)
             ('T
              (SEQ (LETT |y| (SPADCALL |x| (|getShellEntry| $ 14))
                         |URAGG-;cycleLength;ANni;20|)
                   (SEQ (LETT |k| 1 |URAGG-;cycleLength;ANni;20|) G190
                        (COND
                          ((NULL (NOT (SPADCALL |x| |y|
                                       (|getShellEntry| $ 51))))
                           (GO G191)))
                        (LETT |y| (SPADCALL |y| (|getShellEntry| $ 14))
                              |URAGG-;cycleLength;ANni;20|)
                        (SETQ |k| (QSADD1 |k|)) (GO G190) G191
                        (EXIT NIL))
                   (EXIT |k|)))))))) 

(DEFUN |URAGG-;rest;ANniA;21| (|x| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (SEQ (LETT |i| 1 |URAGG-;rest;ANniA;21|) G190
                (COND ((QSGREATERP |i| |n|) (GO G191)))
                (SEQ (EXIT (COND
                             ((SPADCALL |x| (|getShellEntry| $ 20))
                              (|error| "Index out of range"))
                             ('T
                              (LETT |x|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;rest;ANniA;21|)))))
                (SETQ |i| (QSADD1 |i|)) (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |URAGG-;last;ANniA;22| (|x| |n| $)
  (PROG (|m|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 57))
                 |URAGG-;last;ANniA;22|)
           (EXIT (COND
                   ((> |n| |m|) (|error| "index out of range"))
                   ('T
                    (SPADCALL
                        (SPADCALL |x|
                            (LET ((#0=#:G1500 (- |m| |n|)))
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            (|getShellEntry| $ 59))
                        (|getShellEntry| $ 60))))))))) 

(DEFUN |URAGG-;=;2AB;23| (|x| |y| $)
  (PROG (|k| #0=#:G1508)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |x| |y| (|getShellEntry| $ 51)) T)
                   ('T
                    (SEQ (SEQ (LETT |k| 0 |URAGG-;=;2AB;23|) G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |x|
                                           (|getShellEntry| $ 20))
                                          NIL)
                                         ('T
                                          (NOT
                                           (SPADCALL |y|
                                            (|getShellEntry| $ 20))))))
                                 (GO G191)))
                              (SEQ (COND
                                     ((EQL |k| 1000)
                                      (COND
                                        ((SPADCALL |x|
                                          (|getShellEntry| $ 48))
                                         (EXIT (|error| "cyclic list"))))))
                                   (EXIT
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL |x|
                                         (|getShellEntry| $ 8))
                                        (SPADCALL |y|
                                         (|getShellEntry| $ 8))
                                        (|getShellEntry| $ 63))
                                       (PROGN
                                         (LETT #0# NIL
                                          |URAGG-;=;2AB;23|)
                                         (GO #0#)))
                                      ('T
                                       (SEQ
                                        (LETT |x|
                                         (SPADCALL |x|
                                          (|getShellEntry| $ 14))
                                         |URAGG-;=;2AB;23|)
                                        (EXIT
                                         (LETT |y|
                                          (SPADCALL |y|
                                           (|getShellEntry| $ 14))
                                          |URAGG-;=;2AB;23|)))))))
                              (SETQ |k| (QSADD1 |k|)) (GO G190) G191
                              (EXIT NIL))
                         (EXIT (COND
                                 ((SPADCALL |x| (|getShellEntry| $ 20))
                                  (SPADCALL |y| (|getShellEntry| $ 20)))
                                 ('T NIL)))))))
           #0# (EXIT #0#))))) 

(DEFUN |URAGG-;node?;2AB;24| (|u| |v| $)
  (PROG (|k| #0=#:G1513)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |k| 0 |URAGG-;node?;2AB;24|) G190
                           (COND
                             ((NULL (NOT
                                     (SPADCALL |v|
                                      (|getShellEntry| $ 20))))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL |u| |v|
                                          (|getShellEntry| $ 65))
                                         (PROGN
                                           (LETT #0# T
                                            |URAGG-;node?;2AB;24|)
                                           (GO #0#)))
                                        ('T
                                         (SEQ
                                          (COND
                                            ((EQL |k| 1000)
                                             (COND
                                               ((SPADCALL |v|
                                                 (|getShellEntry| $ 48))
                                                (EXIT
                                                 (|error|
                                                  "cyclic list"))))))
                                          (EXIT
                                           (LETT |v|
                                            (SPADCALL |v|
                                             (|getShellEntry| $ 14))
                                            |URAGG-;node?;2AB;24|)))))))
                           (SETQ |k| (QSADD1 |k|)) (GO G190) G191
                           (EXIT NIL))
                      (EXIT (SPADCALL |u| |v| (|getShellEntry| $ 65)))))
           #0# (EXIT #0#))))) 

(DEFUN |URAGG-;setelt;Afirst2S;25| (|x| T3 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 67))) 

(DEFUN |URAGG-;setelt;Alast2S;26| (|x| T4 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 69))) 

(DEFUN |URAGG-;setelt;Arest2A;27| (|x| T5 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 71))) 

(DEFUN |URAGG-;concat;3A;28| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 60)) |y|
      (|getShellEntry| $ 73))) 

(DEFUN |URAGG-;setlast!;A2S;29| (|x| |s| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 20))
          (|error| "setlast: empty list"))
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 24)) |s|
                   (|getShellEntry| $ 67))
               (EXIT |s|)))))) 

(DEFUN |URAGG-;setchildren!;ALA;30| (|u| |lv| $)
  (COND
    ((EQL (LENGTH |lv|) 1)
     (SPADCALL |u| (|SPADfirst| |lv|) (|getShellEntry| $ 71)))
    ('T (|error| "wrong number of children specified")))) 

(DEFUN |URAGG-;setvalue!;A2S;31| (|u| |s| $)
  (SPADCALL |u| |s| (|getShellEntry| $ 67))) 

(DEFUN |URAGG-;split!;AIA;32| (|p| |n| $)
  (PROG (|q|)
    (RETURN
      (SEQ (COND
             ((< |n| 1) (|error| "index out of range"))
             ('T
              (SEQ (LETT |p|
                         (SPADCALL |p|
                             (LET ((#0=#:G1526 (- |n| 1)))
                               (|check-subtype| (>= #0# 0)
                                   '(|NonNegativeInteger|) #0#))
                             (|getShellEntry| $ 59))
                         |URAGG-;split!;AIA;32|)
                   (LETT |q| (SPADCALL |p| (|getShellEntry| $ 14))
                         |URAGG-;split!;AIA;32|)
                   (SPADCALL |p| (SPADCALL (|getShellEntry| $ 81))
                       (|getShellEntry| $ 71))
                   (EXIT |q|)))))))) 

(DEFUN |URAGG-;cycleSplit!;2A;33| (|x| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL
                      (LETT |y| (SPADCALL |x| (|getShellEntry| $ 52))
                            |URAGG-;cycleSplit!;2A;33|)
                      (|getShellEntry| $ 20))
                  (SPADCALL |x| |y| (|getShellEntry| $ 51)))
              |y|)
             ('T
              (SEQ (LETT |z| (SPADCALL |x| (|getShellEntry| $ 14))
                         |URAGG-;cycleSplit!;2A;33|)
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL |z| |y|
                                       (|getShellEntry| $ 51))))
                           (GO G191)))
                        (SEQ (LETT |x| |z| |URAGG-;cycleSplit!;2A;33|)
                             (EXIT (LETT |z|
                                    (SPADCALL |z|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleSplit!;2A;33|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (SPADCALL |x| (SPADCALL (|getShellEntry| $ 81))
                       (|getShellEntry| $ 71))
                   (EXIT |y|)))))))) 

(DEFUN |UnaryRecursiveAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|UnaryRecursiveAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 85))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasAttribute| |#1| '|shallowlyMutable|)))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    (COND
      ((|HasAttribute| |#1| '|finiteAggregate|)
       (|setShellEntry| $ 61
           (CONS (|dispatchFunction| |URAGG-;last;ANniA;22|) $))))
    (COND
      ((|HasCategory| |#2| '(|SetCategory|))
       (PROGN
         (|setShellEntry| $ 64
             (CONS (|dispatchFunction| |URAGG-;=;2AB;23|) $))
         (|setShellEntry| $ 66
             (CONS (|dispatchFunction| |URAGG-;node?;2AB;24|) $)))))
    (COND
      ((|testBitVector| |pv$| 1)
       (PROGN
         (|setShellEntry| $ 68
             (CONS (|dispatchFunction| |URAGG-;setelt;Afirst2S;25|) $))
         (|setShellEntry| $ 70
             (CONS (|dispatchFunction| |URAGG-;setelt;Alast2S;26|) $))
         (|setShellEntry| $ 72
             (CONS (|dispatchFunction| |URAGG-;setelt;Arest2A;27|) $))
         (|setShellEntry| $ 74
             (CONS (|dispatchFunction| |URAGG-;concat;3A;28|) $))
         (|setShellEntry| $ 75
             (CONS (|dispatchFunction| |URAGG-;setlast!;A2S;29|) $))
         (|setShellEntry| $ 78
             (CONS (|dispatchFunction| |URAGG-;setchildren!;ALA;30|) $))
         (|setShellEntry| $ 79
             (CONS (|dispatchFunction| |URAGG-;setvalue!;A2S;31|) $))
         (|setShellEntry| $ 82
             (CONS (|dispatchFunction| |URAGG-;split!;AIA;32|) $))
         (|setShellEntry| $ 83
             (CONS (|dispatchFunction| |URAGG-;cycleSplit!;2A;33|) $)))))
    $)) 

(MAKEPROP '|UnaryRecursiveAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (0 . |first|) '"first" |URAGG-;elt;AfirstS;1| (5 . |last|)
             '"last" |URAGG-;elt;AlastS;2| (10 . |rest|) '"rest"
             |URAGG-;elt;ArestA;3| |URAGG-;second;AS;4|
             |URAGG-;third;AS;5| (|Boolean|) (15 . |empty?|)
             (20 . |false|) (24 . |not|) |URAGG-;cyclic?;AB;6|
             (29 . |tail|) |URAGG-;last;AS;7| (|List| 6) (34 . |empty|)
             (38 . |concat|) (44 . |reverse!|) (|List| $)
             |URAGG-;nodes;AL;8| |URAGG-;children;AL;9|
             |URAGG-;leaf?;AB;10| |URAGG-;value;AS;11|
             (|NonNegativeInteger|) (49 . |Zero|) (|Integer|)
             (53 . |Zero|) (57 . >) (63 . |One|) (67 . |One|) (71 . -)
             |URAGG-;less?;ANniB;12| (77 . |zero?|)
             |URAGG-;more?;ANniB;13| |URAGG-;size?;ANniB;14| (82 . =)
             (88 . |cyclic?|) |URAGG-;#;ANni;15| |URAGG-;tail;2A;16|
             (93 . |eq?|) (99 . |cycleEntry|) |URAGG-;cycleTail;2A;18|
             |URAGG-;cycleEntry;2A;19| |URAGG-;cycleLength;ANni;20|
             |URAGG-;rest;ANniA;21| (104 . |#|) (109 . >)
             (115 . |rest|) (121 . |copy|) (126 . |last|)
             (132 . |true|) (136 . ~=) (142 . =) (148 . =)
             (154 . |node?|) (160 . |setfirst!|) (166 . |setelt|)
             (173 . |setlast!|) (179 . |setelt|) (186 . |setrest!|)
             (192 . |setelt|) (199 . |concat!|) (205 . |concat|)
             (211 . |setlast!|) (217 . |#|) (222 . |first|)
             (227 . |setchildren!|) (233 . |setvalue!|) (239 . <)
             (245 . |empty|) (249 . |split!|) (255 . |cycleSplit!|)
             '"value")
          '#(|value| 260 |third| 265 |tail| 270 |split!| 275 |size?|
             281 |setvalue!| 287 |setlast!| 293 |setelt| 299
             |setchildren!| 320 |second| 326 |rest| 331 |nodes| 337
             |node?| 342 |more?| 348 |less?| 354 |leaf?| 360 |last| 365
             |elt| 376 |cyclic?| 394 |cycleTail| 399 |cycleSplit!| 404
             |cycleLength| 409 |cycleEntry| 414 |concat| 419 |children|
             425 = 430 |#| 436)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 83
                                '(1 6 7 0 8 1 6 7 0 11 1 6 0 0 14 1 6
                                  19 0 20 0 19 0 21 1 19 0 0 22 1 6 0 0
                                  24 0 26 0 27 2 26 0 6 0 28 1 26 0 0
                                  29 0 35 0 36 0 37 0 38 2 37 19 0 0 39
                                  0 35 0 40 0 37 0 41 2 37 0 0 0 42 1
                                  37 19 0 44 2 35 19 0 0 47 1 6 19 0 48
                                  2 6 19 0 0 51 1 6 0 0 52 1 6 35 0 57
                                  2 35 19 0 0 58 2 6 0 0 35 59 1 6 0 0
                                  60 2 0 0 0 35 61 0 19 0 62 2 7 19 0 0
                                  63 2 0 19 0 0 64 2 6 19 0 0 65 2 0 19
                                  0 0 66 2 6 7 0 7 67 3 0 7 0 9 7 68 2
                                  6 7 0 7 69 3 0 7 0 12 7 70 2 6 0 0 0
                                  71 3 0 0 0 15 0 72 2 6 0 0 0 73 2 0 0
                                  0 0 74 2 0 7 0 7 75 1 26 35 0 76 1 26
                                  6 0 77 2 0 0 0 30 78 2 0 7 0 7 79 2
                                  37 19 0 0 80 0 6 0 81 2 0 0 0 37 82 1
                                  0 0 0 83 1 0 7 0 34 1 0 7 0 18 1 0 0
                                  0 50 2 0 0 0 37 82 2 0 19 0 35 46 2 0
                                  7 0 7 79 2 0 7 0 7 75 3 0 7 0 12 7 70
                                  3 0 0 0 15 0 72 3 0 7 0 9 7 68 2 0 0
                                  0 30 78 1 0 7 0 17 2 0 0 0 35 56 1 0
                                  30 0 31 2 0 19 0 0 66 2 0 19 0 35 45
                                  2 0 19 0 35 43 1 0 19 0 33 2 0 0 0 35
                                  61 1 0 7 0 25 2 0 7 0 12 13 2 0 0 0
                                  15 16 2 0 7 0 9 10 1 0 19 0 23 1 0 0
                                  0 53 1 0 0 0 83 1 0 35 0 55 1 0 0 0
                                  54 2 0 0 0 0 74 1 0 30 0 32 2 0 19 0
                                  0 64 1 0 35 0 49)))))
          '|lookupComplete|)) 
