
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
    (T (NOT (SPADCALL (|URAGG-;findCycle| |x| $)
                (|getShellEntry| $ 20)))))) 

(DEFUN |URAGG-;last;AS;7| (|x| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 24))
      (|getShellEntry| $ 8))) 

(DEFUN |URAGG-;nodes;AL;8| (|x| $)
  (LET ((|l| NIL))
    (SEQ (LOOP
           (COND
             ((NOT (NOT (SPADCALL |x| (|getShellEntry| $ 20))))
              (RETURN NIL))
             (T (SEQ (SETQ |l| (CONS |x| |l|))
                     (EXIT (SETQ |x|
                                 (SPADCALL |x| (|getShellEntry| $ 14))))))))
         (EXIT (NREVERSE |l|))))) 

(DEFUN |URAGG-;children;AL;9| (|x| $)
  (LET ((|l| NIL))
    (COND
      ((SPADCALL |x| (|getShellEntry| $ 20)) |l|)
      (T (CONS (SPADCALL |x| (|getShellEntry| $ 14)) |l|))))) 

(DEFUN |URAGG-;leaf?;AB;10| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 20))) 

(DEFUN |URAGG-;value;AS;11| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 20))
     (|error| "value of empty object"))
    (T (SPADCALL |x| (|getShellEntry| $ 8))))) 

(DEFUN |URAGG-;less?;ANniB;12| (|l| |n| $)
  (LET ((|i| |n|))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((PLUSP |i|)
                      (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                     (T NIL)))
              (RETURN NIL))
             (T (SEQ (SETQ |l| (SPADCALL |l| (|getShellEntry| $ 14)))
                     (EXIT (SETQ |i| (- |i| 1)))))))
         (EXIT (PLUSP |i|))))) 

(DEFUN |URAGG-;more?;ANniB;13| (|l| |n| $)
  (LET ((|i| |n|))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((PLUSP |i|)
                      (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                     (T NIL)))
              (RETURN NIL))
             (T (SEQ (SETQ |l| (SPADCALL |l| (|getShellEntry| $ 14)))
                     (EXIT (SETQ |i| (- |i| 1)))))))
         (EXIT (COND
                 ((ZEROP |i|)
                  (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                 (T NIL)))))) 

(DEFUN |URAGG-;size?;ANniB;14| (|l| |n| $)
  (LET ((|i| |n|))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((SPADCALL |l| (|getShellEntry| $ 20)) NIL)
                     (T (PLUSP |i|))))
              (RETURN NIL))
             (T (SEQ (SETQ |l| (SPADCALL |l| (|getShellEntry| $ 14)))
                     (EXIT (SETQ |i| (- |i| 1)))))))
         (EXIT (COND
                 ((SPADCALL |l| (|getShellEntry| $ 20)) (ZEROP |i|))
                 (T NIL)))))) 

(DEFUN |URAGG-;#;ANni;15| (|x| $)
  (LET ((|k| 0))
    (SEQ (LOOP
           (COND
             ((NOT (NOT (SPADCALL |x| (|getShellEntry| $ 20))))
              (RETURN NIL))
             (T (SEQ (COND
                       ((EQL |k| 1000)
                        (COND
                          ((SPADCALL |x| (|getShellEntry| $ 48))
                           (EXIT (|error| "cyclic list"))))))
                     (SETQ |x| (SPADCALL |x| (|getShellEntry| $ 14)))
                     (EXIT (SETQ |k| (+ |k| 1)))))))
         (EXIT |k|)))) 

(DEFUN |URAGG-;tail;2A;16| (|x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 20)) (|error| "empty list"))
    (T (LET ((|y| (SPADCALL |x| (|getShellEntry| $ 14))))
         (SEQ (LET ((|k| 0))
                (LOOP
                  (COND
                    ((NOT (NOT (SPADCALL |y| (|getShellEntry| $ 20))))
                     (RETURN NIL))
                    (T (SEQ (COND
                              ((EQL |k| 1000)
                               (COND
                                 ((SPADCALL |x| (|getShellEntry| $ 48))
                                  (EXIT (|error| "cyclic list"))))))
                            (EXIT (SETQ |y|
                                        (SPADCALL (SETQ |x| |y|)
                                         (|getShellEntry| $ 14)))))))
                  (SETQ |k| (+ |k| 1))))
              (EXIT |x|)))))) 

(DEFUN |URAGG-;findCycle| (|x| $)
  (LET ((|y| (SPADCALL |x| (|getShellEntry| $ 14))))
    (SEQ (LOOP
           (COND
             ((NOT (NOT (SPADCALL |y| (|getShellEntry| $ 20))))
              (RETURN NIL))
             (T (SEQ (COND
                       ((SPADCALL |x| |y| (|getShellEntry| $ 54))
                        (RETURN-FROM |URAGG-;findCycle| |x|)))
                     (SETQ |x| (SPADCALL |x| (|getShellEntry| $ 14)))
                     (SETQ |y| (SPADCALL |y| (|getShellEntry| $ 14)))
                     (COND
                       ((SPADCALL |y| (|getShellEntry| $ 20))
                        (RETURN-FROM |URAGG-;findCycle| |y|)))
                     (COND
                       ((SPADCALL |x| |y| (|getShellEntry| $ 54))
                        (RETURN-FROM |URAGG-;findCycle| |y|)))
                     (EXIT (SETQ |y|
                                 (SPADCALL |y| (|getShellEntry| $ 14))))))))
         (EXIT |y|)))) 

(DEFUN |URAGG-;cycleTail;2A;18| (|x| $)
  (PROG (|z| |y|)
    (RETURN
      (COND
        ((SPADCALL
             (LETT |y| (SETQ |x| (SPADCALL |x| (|getShellEntry| $ 55)))
                   |URAGG-;cycleTail;2A;18|)
             (|getShellEntry| $ 20))
         |x|)
        (T (SEQ (LETT |z| (SPADCALL |x| (|getShellEntry| $ 14))
                      |URAGG-;cycleTail;2A;18|)
                (LOOP
                  (COND
                    ((NOT (NOT (SPADCALL |x| |z|
                                   (|getShellEntry| $ 54))))
                     (RETURN NIL))
                    (T (SEQ (LETT |y| |z| |URAGG-;cycleTail;2A;18|)
                            (EXIT (SETQ |z|
                                        (SPADCALL |z|
                                         (|getShellEntry| $ 14))))))))
                (EXIT |y|))))))) 

(DEFUN |URAGG-;cycleEntry;2A;19| (|x| $)
  (PROG (|z| |l| |y|)
    (RETURN
      (COND
        ((SPADCALL |x| (|getShellEntry| $ 20)) |x|)
        ((SPADCALL
             (LETT |y| (|URAGG-;findCycle| |x| $)
                   |URAGG-;cycleEntry;2A;19|)
             (|getShellEntry| $ 20))
         |y|)
        (T (SEQ (LETT |z| (SPADCALL |y| (|getShellEntry| $ 14))
                      |URAGG-;cycleEntry;2A;19|)
                (LETT |l| 1 |URAGG-;cycleEntry;2A;19|)
                (LOOP
                  (COND
                    ((NOT (NOT (SPADCALL |y| |z|
                                   (|getShellEntry| $ 54))))
                     (RETURN NIL))
                    (T (SEQ (SETQ |z|
                                  (SPADCALL |z| (|getShellEntry| $ 14)))
                            (EXIT (SETQ |l| (+ |l| 1)))))))
                (LETT |y| |x| |URAGG-;cycleEntry;2A;19|)
                (LET ((|k| 1))
                  (LOOP
                    (COND
                      ((> |k| |l|) (RETURN NIL))
                      (T (SETQ |y|
                               (SPADCALL |y| (|getShellEntry| $ 14)))))
                    (SETQ |k| (+ |k| 1))))
                (LOOP
                  (COND
                    ((NOT (NOT (SPADCALL |x| |y|
                                   (|getShellEntry| $ 54))))
                     (RETURN NIL))
                    (T (SEQ (SETQ |x|
                                  (SPADCALL |x| (|getShellEntry| $ 14)))
                            (EXIT (SETQ |y|
                                        (SPADCALL |y|
                                         (|getShellEntry| $ 14))))))))
                (EXIT |x|))))))) 

(DEFUN |URAGG-;cycleLength;ANni;20| (|x| $)
  (COND
    ((OR (SPADCALL |x| (|getShellEntry| $ 20))
         (SPADCALL (SETQ |x| (|URAGG-;findCycle| |x| $))
             (|getShellEntry| $ 20)))
     0)
    (T (LET ((|y| (SPADCALL |x| (|getShellEntry| $ 14))) (|k| 1))
         (SEQ (LOOP
                (COND
                  ((NOT (NOT (SPADCALL |x| |y| (|getShellEntry| $ 54))))
                   (RETURN NIL))
                  (T (SEQ (SETQ |y|
                                (SPADCALL |y| (|getShellEntry| $ 14)))
                          (EXIT (SETQ |k| (+ |k| 1)))))))
              (EXIT |k|)))))) 

(DEFUN |URAGG-;rest;ANniA;21| (|x| |n| $)
  (SEQ (LET ((|i| 1))
         (LOOP
           (COND
             ((> |i| |n|) (RETURN NIL))
             (T (COND
                  ((SPADCALL |x| (|getShellEntry| $ 20))
                   (|error| "Index out of range"))
                  (T (SETQ |x| (SPADCALL |x| (|getShellEntry| $ 14)))))))
           (SETQ |i| (+ |i| 1))))
       (EXIT |x|))) 

(DEFUN |URAGG-;last;ANniA;22| (|x| |n| $)
  (LET ((|m| (SPADCALL |x| (|getShellEntry| $ 60))))
    (COND
      ((< |m| |n|) (|error| "index out of range"))
      (T (SPADCALL
             (SPADCALL |x|
                 (LET ((#0=#:G1477 (- |m| |n|)))
                   (|check-subtype| (NOT (MINUSP #0#))
                       '(|NonNegativeInteger|) #0#))
                 (|getShellEntry| $ 62))
             (|getShellEntry| $ 63)))))) 

(DEFUN |URAGG-;=;2AB;23| (|x| |y| $)
  (COND
    ((SPADCALL |x| |y| (|getShellEntry| $ 54)) T)
    (T (SEQ (LET ((|k| 0))
              (LOOP
                (COND
                  ((NOT (COND
                          ((SPADCALL |x| (|getShellEntry| $ 20)) NIL)
                          (T (NOT (SPADCALL |y| (|getShellEntry| $ 20))))))
                   (RETURN NIL))
                  (T (SEQ (COND
                            ((EQL |k| 1000)
                             (COND
                               ((SPADCALL |x| (|getShellEntry| $ 48))
                                (EXIT (|error| "cyclic list"))))))
                          (EXIT (COND
                                  ((SPADCALL
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 8))
                                    (SPADCALL |y|
                                     (|getShellEntry| $ 8))
                                    (|getShellEntry| $ 66))
                                   (RETURN-FROM |URAGG-;=;2AB;23| NIL))
                                  (T (SEQ
                                      (SETQ |x|
                                       (SPADCALL |x|
                                        (|getShellEntry| $ 14)))
                                      (EXIT
                                       (SETQ |y|
                                        (SPADCALL |y|
                                         (|getShellEntry| $ 14)))))))))))
                (SETQ |k| (+ |k| 1))))
            (EXIT (COND
                    ((SPADCALL |x| (|getShellEntry| $ 20))
                     (SPADCALL |y| (|getShellEntry| $ 20)))
                    (T NIL))))))) 

(DEFUN |URAGG-;node?;2AB;24| (|u| |v| $)
  (SEQ (LET ((|k| 0))
         (LOOP
           (COND
             ((NOT (NOT (SPADCALL |v| (|getShellEntry| $ 20))))
              (RETURN NIL))
             (T (COND
                  ((SPADCALL |u| |v| (|getShellEntry| $ 68))
                   (RETURN-FROM |URAGG-;node?;2AB;24| T))
                  (T (SEQ (COND
                            ((EQL |k| 1000)
                             (COND
                               ((SPADCALL |v| (|getShellEntry| $ 48))
                                (EXIT (|error| "cyclic list"))))))
                          (EXIT (SETQ |v|
                                      (SPADCALL |v|
                                       (|getShellEntry| $ 14)))))))))
           (SETQ |k| (+ |k| 1))))
       (EXIT (SPADCALL |u| |v| (|getShellEntry| $ 68))))) 

(DEFUN |URAGG-;setelt;Afirst2S;25| (|x| T3 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 70))) 

(DEFUN |URAGG-;setelt;Alast2S;26| (|x| T4 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 72))) 

(DEFUN |URAGG-;setelt;Arest2A;27| (|x| T5 |a| $)
  (SPADCALL |x| |a| (|getShellEntry| $ 74))) 

(DEFUN |URAGG-;concat;3A;28| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 63)) |y|
      (|getShellEntry| $ 76))) 

(DEFUN |URAGG-;setlast!;A2S;29| (|x| |s| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 20))
     (|error| "setlast: empty list"))
    (T (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 24)) |s|
                (|getShellEntry| $ 70))
            (EXIT |s|))))) 

(DEFUN |URAGG-;setchildren!;ALA;30| (|u| |lv| $)
  (COND
    ((EQL (LIST-LENGTH |lv|) 1)
     (SPADCALL |u| (|SPADfirst| |lv|) (|getShellEntry| $ 74)))
    (T (|error| "wrong number of children specified")))) 

(DEFUN |URAGG-;setvalue!;A2S;31| (|u| |s| $)
  (SPADCALL |u| |s| (|getShellEntry| $ 70))) 

(DEFUN |URAGG-;split!;AIA;32| (|p| |n| $)
  (PROG (|q|)
    (RETURN
      (COND
        ((< |n| 1) (|error| "index out of range"))
        (T (SEQ (SETQ |p|
                      (SPADCALL |p|
                          (LET ((#0=#:G1503 (- |n| 1)))
                            (|check-subtype| (NOT (MINUSP #0#))
                                '(|NonNegativeInteger|) #0#))
                          (|getShellEntry| $ 62)))
                (LETT |q| (SPADCALL |p| (|getShellEntry| $ 14))
                      |URAGG-;split!;AIA;32|)
                (SPADCALL |p| (SPADCALL (|getShellEntry| $ 84))
                    (|getShellEntry| $ 74))
                (EXIT |q|))))))) 

(DEFUN |URAGG-;cycleSplit!;2A;33| (|x| $)
  (PROG (|y| |z|)
    (RETURN
      (COND
        ((OR (SPADCALL
                 (LETT |y| (SPADCALL |x| (|getShellEntry| $ 55))
                       |URAGG-;cycleSplit!;2A;33|)
                 (|getShellEntry| $ 20))
             (SPADCALL |x| |y| (|getShellEntry| $ 54)))
         |y|)
        (T (SEQ (LETT |z| (SPADCALL |x| (|getShellEntry| $ 14))
                      |URAGG-;cycleSplit!;2A;33|)
                (LOOP
                  (COND
                    ((NOT (NOT (SPADCALL |z| |y|
                                   (|getShellEntry| $ 54))))
                     (RETURN NIL))
                    (T (SEQ (SETQ |x| |z|)
                            (EXIT (SETQ |z|
                                        (SPADCALL |z|
                                         (|getShellEntry| $ 14))))))))
                (SPADCALL |x| (SPADCALL (|getShellEntry| $ 84))
                    (|getShellEntry| $ 74))
                (EXIT |y|))))))) 

(DEFUN |UnaryRecursiveAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|UnaryRecursiveAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 88))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (|HasAttribute| |#1| '|shallowlyMutable|)))))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    (COND
      ((|HasAttribute| |#1| '|finiteAggregate|)
       (|setShellEntry| $ 64
           (CONS (|dispatchFunction| |URAGG-;last;ANniA;22|) $))))
    (COND
      ((|HasCategory| |#2| '(|SetCategory|))
       (PROGN
         (|setShellEntry| $ 67
             (CONS (|dispatchFunction| |URAGG-;=;2AB;23|) $))
         (|setShellEntry| $ 69
             (CONS (|dispatchFunction| |URAGG-;node?;2AB;24|) $)))))
    (COND
      ((|testBitVector| |pv$| 1)
       (PROGN
         (|setShellEntry| $ 71
             (CONS (|dispatchFunction| |URAGG-;setelt;Afirst2S;25|) $))
         (|setShellEntry| $ 73
             (CONS (|dispatchFunction| |URAGG-;setelt;Alast2S;26|) $))
         (|setShellEntry| $ 75
             (CONS (|dispatchFunction| |URAGG-;setelt;Arest2A;27|) $))
         (|setShellEntry| $ 77
             (CONS (|dispatchFunction| |URAGG-;concat;3A;28|) $))
         (|setShellEntry| $ 78
             (CONS (|dispatchFunction| |URAGG-;setlast!;A2S;29|) $))
         (|setShellEntry| $ 81
             (CONS (|dispatchFunction| |URAGG-;setchildren!;ALA;30|) $))
         (|setShellEntry| $ 82
             (CONS (|dispatchFunction| |URAGG-;setvalue!;A2S;31|) $))
         (|setShellEntry| $ 85
             (CONS (|dispatchFunction| |URAGG-;split!;AIA;32|) $))
         (|setShellEntry| $ 86
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
             (88 . |cyclic?|) (|PositiveInteger|) (93 . |One|) (97 . +)
             |URAGG-;#;ANni;15| |URAGG-;tail;2A;16| (103 . |eq?|)
             (109 . |cycleEntry|) |URAGG-;cycleTail;2A;18|
             |URAGG-;cycleEntry;2A;19| |URAGG-;cycleLength;ANni;20|
             |URAGG-;rest;ANniA;21| (114 . |#|) (119 . >)
             (125 . |rest|) (131 . |copy|) (136 . |last|)
             (142 . |true|) (146 . ~=) (152 . =) (158 . =)
             (164 . |node?|) (170 . |setfirst!|) (176 . |setelt|)
             (183 . |setlast!|) (189 . |setelt|) (196 . |setrest!|)
             (202 . |setelt|) (209 . |concat!|) (215 . |concat|)
             (221 . |setlast!|) (227 . |#|) (232 . |first|)
             (237 . |setchildren!|) (243 . |setvalue!|) (249 . <)
             (255 . |empty|) (259 . |split!|) (265 . |cycleSplit!|)
             '"value")
          '#(|value| 270 |third| 275 |tail| 280 |split!| 285 |size?|
             291 |setvalue!| 297 |setlast!| 303 |setelt| 309
             |setchildren!| 330 |second| 336 |rest| 341 |nodes| 347
             |node?| 352 |more?| 358 |less?| 364 |leaf?| 370 |last| 375
             |elt| 386 |cyclic?| 404 |cycleTail| 409 |cycleSplit!| 414
             |cycleLength| 419 |cycleEntry| 424 |concat| 429 |children|
             435 = 440 |#| 446)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 86
                                '(1 6 7 0 8 1 6 7 0 11 1 6 0 0 14 1 6
                                  19 0 20 0 19 0 21 1 19 0 0 22 1 6 0 0
                                  24 0 26 0 27 2 26 0 6 0 28 1 26 0 0
                                  29 0 35 0 36 0 37 0 38 2 37 19 0 0 39
                                  0 35 0 40 0 37 0 41 2 37 0 0 0 42 1
                                  37 19 0 44 2 35 19 0 0 47 1 6 19 0 48
                                  0 49 0 50 2 35 0 0 0 51 2 6 19 0 0 54
                                  1 6 0 0 55 1 6 35 0 60 2 35 19 0 0 61
                                  2 6 0 0 35 62 1 6 0 0 63 2 0 0 0 35
                                  64 0 19 0 65 2 7 19 0 0 66 2 0 19 0 0
                                  67 2 6 19 0 0 68 2 0 19 0 0 69 2 6 7
                                  0 7 70 3 0 7 0 9 7 71 2 6 7 0 7 72 3
                                  0 7 0 12 7 73 2 6 0 0 0 74 3 0 0 0 15
                                  0 75 2 6 0 0 0 76 2 0 0 0 0 77 2 0 7
                                  0 7 78 1 26 35 0 79 1 26 6 0 80 2 0 0
                                  0 30 81 2 0 7 0 7 82 2 37 19 0 0 83 0
                                  6 0 84 2 0 0 0 37 85 1 0 0 0 86 1 0 7
                                  0 34 1 0 7 0 18 1 0 0 0 53 2 0 0 0 37
                                  85 2 0 19 0 35 46 2 0 7 0 7 82 2 0 7
                                  0 7 78 3 0 7 0 12 7 73 3 0 7 0 9 7 71
                                  3 0 0 0 15 0 75 2 0 0 0 30 81 1 0 7 0
                                  17 2 0 0 0 35 59 1 0 30 0 31 2 0 19 0
                                  0 69 2 0 19 0 35 45 2 0 19 0 35 43 1
                                  0 19 0 33 2 0 0 0 35 64 1 0 7 0 25 2
                                  0 7 0 12 13 2 0 0 0 15 16 2 0 7 0 9
                                  10 1 0 19 0 23 1 0 0 0 56 1 0 0 0 86
                                  1 0 35 0 58 1 0 0 0 57 2 0 0 0 0 77 1
                                  0 30 0 32 2 0 19 0 0 67 1 0 35 0 52)))))
          '|lookupComplete|)) 
