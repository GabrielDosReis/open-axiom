
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
    ((SPADCALL |x| (|getShellEntry| $ 20)) 'NIL)
    ('T
     (SPADCALL
         (SPADCALL (|URAGG-;findCycle| |x| $) (|getShellEntry| $ 20))
         (|getShellEntry| $ 21))))) 

(DEFUN |URAGG-;last;AS;7| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 23))
      (|getShellEntry| $ 8))) 

(DEFUN |URAGG-;nodes;AL;8| (|x| $)
  (PROG (|l|)
    (RETURN
      (SEQ (LETT |l| NIL |URAGG-;nodes;AL;8|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 20))
                             (|getShellEntry| $ 21)))
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
                           ((< 0 |i|)
                            (SPADCALL
                                (SPADCALL |l| (|getShellEntry| $ 20))
                                (|getShellEntry| $ 21)))
                           ('T 'NIL)))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |l| (|getShellEntry| $ 14))
                           |URAGG-;less?;ANniB;12|)
                     (EXIT (LETT |i| (- |i| 1) |URAGG-;less?;ANniB;12|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (< 0 |i|)))))) 

(DEFUN |URAGG-;more?;ANniB;13| (|l| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (LETT |i| |n| |URAGG-;more?;ANniB;13|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((< 0 |i|)
                            (SPADCALL
                                (SPADCALL |l| (|getShellEntry| $ 20))
                                (|getShellEntry| $ 21)))
                           ('T 'NIL)))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |l| (|getShellEntry| $ 14))
                           |URAGG-;more?;ANniB;13|)
                     (EXIT (LETT |i| (- |i| 1) |URAGG-;more?;ANniB;13|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((ZEROP |i|)
                    (SPADCALL (SPADCALL |l| (|getShellEntry| $ 20))
                        (|getShellEntry| $ 21)))
                   ('T 'NIL))))))) 

(DEFUN |URAGG-;size?;ANniB;14| (|l| |n| $)
  (PROG (|i|)
    (RETURN
      (SEQ (LETT |i| |n| |URAGG-;size?;ANniB;14|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |l| (|getShellEntry| $ 20)) 'NIL)
                           ('T (< 0 |i|))))
                   (GO G191)))
                (SEQ (LETT |l| (SPADCALL |l| (|getShellEntry| $ 14))
                           |URAGG-;size?;ANniB;14|)
                     (EXIT (LETT |i| (- |i| 1) |URAGG-;size?;ANniB;14|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |l| (|getShellEntry| $ 20)) (ZEROP |i|))
                   ('T 'NIL))))))) 

(DEFUN |URAGG-;#;ANni;15| (|x| $)
  (PROG (|k|)
    (RETURN
      (SEQ (SEQ (LETT |k| 0 |URAGG-;#;ANni;15|) G190
                (COND
                  ((NULL (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 20))
                             (|getShellEntry| $ 21)))
                   (GO G191)))
                (SEQ (COND
                       ((EQL |k| 1000)
                        (COND
                          ((SPADCALL |x| (|getShellEntry| $ 34))
                           (EXIT (|error| "cyclic list"))))))
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 14))
                                 |URAGG-;#;ANni;15|)))
                (LETT |k| (QSADD1 |k|) |URAGG-;#;ANni;15|) (GO G190)
                G191 (EXIT NIL))
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
                          ((NULL (SPADCALL
                                     (SPADCALL |y|
                                      (|getShellEntry| $ 20))
                                     (|getShellEntry| $ 21)))
                           (GO G191)))
                        (SEQ (COND
                               ((EQL |k| 1000)
                                (COND
                                  ((SPADCALL |x|
                                    (|getShellEntry| $ 34))
                                   (EXIT (|error| "cyclic list"))))))
                             (EXIT (LETT |y|
                                    (SPADCALL
                                     (LETT |x| |y| |URAGG-;tail;2A;16|)
                                     (|getShellEntry| $ 14))
                                    |URAGG-;tail;2A;16|)))
                        (LETT |k| (QSADD1 |k|) |URAGG-;tail;2A;16|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT |x|)))))))) 

(DEFUN |URAGG-;findCycle| (|x| $)
  (PROG (#0=#:G1475 |y|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |y| (SPADCALL |x| (|getShellEntry| $ 14))
                            |URAGG-;findCycle|)
                      (SEQ G190
                           (COND
                             ((NULL (SPADCALL
                                     (SPADCALL |y|
                                      (|getShellEntry| $ 20))
                                     (|getShellEntry| $ 21)))
                              (GO G191)))
                           (SEQ (COND
                                  ((SPADCALL |x| |y|
                                    (|getShellEntry| $ 37))
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
                                    (|getShellEntry| $ 37))
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
                        (LETT |x| (SPADCALL |x| (|getShellEntry| $ 38))
                              |URAGG-;cycleTail;2A;18|)
                        |URAGG-;cycleTail;2A;18|)
                  (|getShellEntry| $ 20))
              |x|)
             ('T
              (SEQ (LETT |z| (SPADCALL |x| (|getShellEntry| $ 14))
                         |URAGG-;cycleTail;2A;18|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL
                                     (SPADCALL |x| |z|
                                      (|getShellEntry| $ 37))
                                     (|getShellEntry| $ 21)))
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
                          ((NULL (SPADCALL
                                     (SPADCALL |y| |z|
                                      (|getShellEntry| $ 37))
                                     (|getShellEntry| $ 21)))
                           (GO G191)))
                        (SEQ (EXIT (LETT |z|
                                    (SPADCALL |z|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleEntry;2A;19|)))
                        (LETT |l| (QSADD1 |l|)
                              |URAGG-;cycleEntry;2A;19|)
                        (GO G190) G191 (EXIT NIL))
                   (LETT |y| |x| |URAGG-;cycleEntry;2A;19|)
                   (SEQ (LETT |k| 1 |URAGG-;cycleEntry;2A;19|) G190
                        (COND ((QSGREATERP |k| |l|) (GO G191)))
                        (SEQ (EXIT (LETT |y|
                                    (SPADCALL |y|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleEntry;2A;19|)))
                        (LETT |k| (QSADD1 |k|)
                              |URAGG-;cycleEntry;2A;19|)
                        (GO G190) G191 (EXIT NIL))
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL
                                     (SPADCALL |x| |y|
                                      (|getShellEntry| $ 37))
                                     (|getShellEntry| $ 21)))
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
                          ((NULL (SPADCALL
                                     (SPADCALL |x| |y|
                                      (|getShellEntry| $ 37))
                                     (|getShellEntry| $ 21)))
                           (GO G191)))
                        (SEQ (EXIT (LETT |y|
                                    (SPADCALL |y|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleLength;ANni;20|)))
                        (LETT |k| (QSADD1 |k|)
                              |URAGG-;cycleLength;ANni;20|)
                        (GO G190) G191 (EXIT NIL))
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
                (LETT |i| (QSADD1 |i|) |URAGG-;rest;ANniA;21|)
                (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |URAGG-;last;ANniA;22| (|x| |n| $)
  (PROG (|m| #0=#:G1498)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 43))
                 |URAGG-;last;ANniA;22|)
           (EXIT (COND
                   ((< |m| |n|) (|error| "index out of range"))
                   ('T
                    (SPADCALL
                        (SPADCALL |x|
                            (PROG1 (LETT #0# (- |m| |n|)
                                    |URAGG-;last;ANniA;22|)
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            (|getShellEntry| $ 44))
                        (|getShellEntry| $ 45))))))))) 

(DEFUN |URAGG-;=;2AB;23| (|x| |y| $)
  (PROG (|k| #0=#:G1508)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |x| |y| (|getShellEntry| $ 37)) 'T)
                   ('T
                    (SEQ (SEQ (LETT |k| 0 |URAGG-;=;2AB;23|) G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |x|
                                           (|getShellEntry| $ 20))
                                          'NIL)
                                         ('T
                                          (SPADCALL
                                           (SPADCALL |y|
                                            (|getShellEntry| $ 20))
                                           (|getShellEntry| $ 21)))))
                                 (GO G191)))
                              (SEQ (COND
                                     ((EQL |k| 1000)
                                      (COND
                                        ((SPADCALL |x|
                                          (|getShellEntry| $ 34))
                                         (EXIT (|error| "cyclic list"))))))
                                   (EXIT
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL |x|
                                         (|getShellEntry| $ 8))
                                        (SPADCALL |y|
                                         (|getShellEntry| $ 8))
                                        (|getShellEntry| $ 47))
                                       (PROGN
                                         (LETT #0# 'NIL
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
                              (LETT |k| (QSADD1 |k|) |URAGG-;=;2AB;23|)
                              (GO G190) G191 (EXIT NIL))
                         (EXIT (COND
                                 ((SPADCALL |x| (|getShellEntry| $ 20))
                                  (SPADCALL |y| (|getShellEntry| $ 20)))
                                 ('T 'NIL)))))))
           #0# (EXIT #0#))))) 

(DEFUN |URAGG-;node?;2AB;24| (|u| |v| $)
  (PROG (|k| #0=#:G1513)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |k| 0 |URAGG-;node?;2AB;24|) G190
                           (COND
                             ((NULL (SPADCALL
                                     (SPADCALL |v|
                                      (|getShellEntry| $ 20))
                                     (|getShellEntry| $ 21)))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL |u| |v|
                                          (|getShellEntry| $ 49))
                                         (PROGN
                                           (LETT #0# 'T
                                            |URAGG-;node?;2AB;24|)
                                           (GO #0#)))
                                        ('T
                                         (SEQ
                                          (COND
                                            ((EQL |k| 1000)
                                             (COND
                                               ((SPADCALL |v|
                                                 (|getShellEntry| $ 34))
                                                (EXIT
                                                 (|error|
                                                  "cyclic list"))))))
                                          (EXIT
                                           (LETT |v|
                                            (SPADCALL |v|
                                             (|getShellEntry| $ 14))
                                            |URAGG-;node?;2AB;24|)))))))
                           (LETT |k| (QSADD1 |k|)
                                 |URAGG-;node?;2AB;24|)
                           (GO G190) G191 (EXIT NIL))
                      (EXIT (SPADCALL |u| |v| (|getShellEntry| $ 49)))))
           #0# (EXIT #0#))))) 

(DEFUN |URAGG-;setelt;Afirst2S;25| (|x| T3 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 51))) 

(DEFUN |URAGG-;setelt;Alast2S;26| (|x| T4 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 53))) 

(DEFUN |URAGG-;setelt;Arest2A;27| (|x| T5 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 55))) 

(DEFUN |URAGG-;concat;3A;28| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 45)) |y|
      (|getShellEntry| $ 57))) 

(DEFUN |URAGG-;setlast!;A2S;29| (|x| |s| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 20))
          (|error| "setlast: empty list"))
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 23)) |s|
                   (|getShellEntry| $ 51))
               (EXIT |s|)))))) 

(DEFUN |URAGG-;setchildren!;ALA;30| (|u| |lv| $)
  (COND
    ((EQL (LENGTH |lv|) 1)
     (SPADCALL |u| (|SPADfirst| |lv|) (|getShellEntry| $ 55)))
    ('T (|error| "wrong number of children specified")))) 

(DEFUN |URAGG-;setvalue!;A2S;31| (|u| |s| $)
  (SPADCALL |u| |s| (|getShellEntry| $ 51))) 

(DEFUN |URAGG-;split!;AIA;32| (|p| |n| $)
  (PROG (#0=#:G1524 |q|)
    (RETURN
      (SEQ (COND
             ((< |n| 1) (|error| "index out of range"))
             ('T
              (SEQ (LETT |p|
                         (SPADCALL |p|
                             (PROG1 (LETT #0# (- |n| 1)
                                     |URAGG-;split!;AIA;32|)
                               (|check-subtype| (>= #0# 0)
                                   '(|NonNegativeInteger|) #0#))
                             (|getShellEntry| $ 44))
                         |URAGG-;split!;AIA;32|)
                   (LETT |q| (SPADCALL |p| (|getShellEntry| $ 14))
                         |URAGG-;split!;AIA;32|)
                   (SPADCALL |p| (SPADCALL (|getShellEntry| $ 62))
                       (|getShellEntry| $ 55))
                   (EXIT |q|)))))))) 

(DEFUN |URAGG-;cycleSplit!;2A;33| (|x| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL
                      (LETT |y| (SPADCALL |x| (|getShellEntry| $ 38))
                            |URAGG-;cycleSplit!;2A;33|)
                      (|getShellEntry| $ 20))
                  (SPADCALL |x| |y| (|getShellEntry| $ 37)))
              |y|)
             ('T
              (SEQ (LETT |z| (SPADCALL |x| (|getShellEntry| $ 14))
                         |URAGG-;cycleSplit!;2A;33|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL
                                     (SPADCALL |z| |y|
                                      (|getShellEntry| $ 37))
                                     (|getShellEntry| $ 21)))
                           (GO G191)))
                        (SEQ (LETT |x| |z| |URAGG-;cycleSplit!;2A;33|)
                             (EXIT (LETT |z|
                                    (SPADCALL |z|
                                     (|getShellEntry| $ 14))
                                    |URAGG-;cycleSplit!;2A;33|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (SPADCALL |x| (SPADCALL (|getShellEntry| $ 62))
                       (|getShellEntry| $ 55))
                   (EXIT |y|)))))))) 

(DEFUN |UnaryRecursiveAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|)
              . #0=(|UnaryRecursiveAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$|
              (LIST '|UnaryRecursiveAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 67) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasAttribute| |#1| '|shallowlyMutable|))) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|finiteAggregate|)
           (|setShellEntry| $ 46
               (CONS (|dispatchFunction| |URAGG-;last;ANniA;22|) $))))
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (PROGN
             (|setShellEntry| $ 48
                 (CONS (|dispatchFunction| |URAGG-;=;2AB;23|) $))
             (|setShellEntry| $ 50
                 (CONS (|dispatchFunction| |URAGG-;node?;2AB;24|) $)))))
        (COND
          ((|testBitVector| |pv$| 1)
           (PROGN
             (|setShellEntry| $ 52
                 (CONS (|dispatchFunction| |URAGG-;setelt;Afirst2S;25|)
                       $))
             (|setShellEntry| $ 54
                 (CONS (|dispatchFunction| |URAGG-;setelt;Alast2S;26|)
                       $))
             (|setShellEntry| $ 56
                 (CONS (|dispatchFunction| |URAGG-;setelt;Arest2A;27|)
                       $))
             (|setShellEntry| $ 58
                 (CONS (|dispatchFunction| |URAGG-;concat;3A;28|) $))
             (|setShellEntry| $ 59
                 (CONS (|dispatchFunction| |URAGG-;setlast!;A2S;29|) $))
             (|setShellEntry| $ 60
                 (CONS (|dispatchFunction|
                           |URAGG-;setchildren!;ALA;30|)
                       $))
             (|setShellEntry| $ 61
                 (CONS (|dispatchFunction| |URAGG-;setvalue!;A2S;31|)
                       $))
             (|setShellEntry| $ 64
                 (CONS (|dispatchFunction| |URAGG-;split!;AIA;32|) $))
             (|setShellEntry| $ 65
                 (CONS (|dispatchFunction| |URAGG-;cycleSplit!;2A;33|)
                       $)))))
        $)))) 

(MAKEPROP '|UnaryRecursiveAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (0 . |first|) '"first" |URAGG-;elt;AfirstS;1| (5 . |last|)
             '"last" |URAGG-;elt;AlastS;2| (10 . |rest|) '"rest"
             |URAGG-;elt;ArestA;3| |URAGG-;second;AS;4|
             |URAGG-;third;AS;5| (|Boolean|) (15 . |empty?|)
             (20 . |not|) |URAGG-;cyclic?;AB;6| (25 . |tail|)
             |URAGG-;last;AS;7| (|List| $) |URAGG-;nodes;AL;8|
             |URAGG-;children;AL;9| |URAGG-;leaf?;AB;10|
             |URAGG-;value;AS;11| (|NonNegativeInteger|)
             |URAGG-;less?;ANniB;12| |URAGG-;more?;ANniB;13|
             |URAGG-;size?;ANniB;14| (30 . |cyclic?|)
             |URAGG-;#;ANni;15| |URAGG-;tail;2A;16| (35 . |eq?|)
             (41 . |cycleEntry|) |URAGG-;cycleTail;2A;18|
             |URAGG-;cycleEntry;2A;19| |URAGG-;cycleLength;ANni;20|
             |URAGG-;rest;ANniA;21| (46 . |#|) (51 . |rest|)
             (57 . |copy|) (62 . |last|) (68 . ~=) (74 . =) (80 . =)
             (86 . |node?|) (92 . |setfirst!|) (98 . |setelt|)
             (105 . |setlast!|) (111 . |setelt|) (118 . |setrest!|)
             (124 . |setelt|) (131 . |concat!|) (137 . |concat|)
             (143 . |setlast!|) (149 . |setchildren!|)
             (155 . |setvalue!|) (161 . |empty|) (|Integer|)
             (165 . |split!|) (171 . |cycleSplit!|) '"value")
          '#(|value| 176 |third| 181 |tail| 186 |split!| 191 |size?|
             197 |setvalue!| 203 |setlast!| 209 |setelt| 215
             |setchildren!| 236 |second| 242 |rest| 247 |nodes| 253
             |node?| 258 |more?| 264 |less?| 270 |leaf?| 276 |last| 281
             |elt| 292 |cyclic?| 310 |cycleTail| 315 |cycleSplit!| 320
             |cycleLength| 325 |cycleEntry| 330 |concat| 335 |children|
             341 = 346 |#| 352)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 65
                                '(1 6 7 0 8 1 6 7 0 11 1 6 0 0 14 1 6
                                  19 0 20 1 19 0 0 21 1 6 0 0 23 1 6 19
                                  0 34 2 6 19 0 0 37 1 6 0 0 38 1 6 30
                                  0 43 2 6 0 0 30 44 1 6 0 0 45 2 0 0 0
                                  30 46 2 7 19 0 0 47 2 0 19 0 0 48 2 6
                                  19 0 0 49 2 0 19 0 0 50 2 6 7 0 7 51
                                  3 0 7 0 9 7 52 2 6 7 0 7 53 3 0 7 0
                                  12 7 54 2 6 0 0 0 55 3 0 0 0 15 0 56
                                  2 6 0 0 0 57 2 0 0 0 0 58 2 0 7 0 7
                                  59 2 0 0 0 25 60 2 0 7 0 7 61 0 6 0
                                  62 2 0 0 0 63 64 1 0 0 0 65 1 0 7 0
                                  29 1 0 7 0 18 1 0 0 0 36 2 0 0 0 63
                                  64 2 0 19 0 30 33 2 0 7 0 7 61 2 0 7
                                  0 7 59 3 0 7 0 12 7 54 3 0 0 0 15 0
                                  56 3 0 7 0 9 7 52 2 0 0 0 25 60 1 0 7
                                  0 17 2 0 0 0 30 42 1 0 25 0 26 2 0 19
                                  0 0 50 2 0 19 0 30 32 2 0 19 0 30 31
                                  1 0 19 0 28 2 0 0 0 30 46 1 0 7 0 24
                                  2 0 7 0 12 13 2 0 0 0 15 16 2 0 7 0 9
                                  10 1 0 19 0 22 1 0 0 0 39 1 0 0 0 65
                                  1 0 30 0 41 1 0 0 0 40 2 0 0 0 0 58 1
                                  0 25 0 27 2 0 19 0 0 48 1 0 30 0 35)))))
          '|lookupComplete|)) 
