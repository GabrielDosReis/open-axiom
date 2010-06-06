
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LSAGG-;sort!;M2A;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |LSAGG-;list;SA;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LSAGG-;reduce;MAS;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |LSAGG-;merge;M3A;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LSAGG-;select!;M2A;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |LSAGG-;merge!;M3A;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |LSAGG-;insert!;SAIA;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |LSAGG-;insert!;2AIA;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LSAGG-;remove!;M2A;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |LSAGG-;delete!;AIA;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |LSAGG-;delete!;AUsA;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |LSAGG-;find;MAU;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Integer|)
                |LSAGG-;position;MAI;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |LSAGG-;mergeSort|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |LSAGG-;sorted?;MAB;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |LSAGG-;reduce;MA2S;16|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Thing| |%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |LSAGG-;reduce;MA3S;17|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Thing| |%Shell|)
                    |%Thing|)
                |LSAGG-;new;NniSA;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |LSAGG-;map;M3A;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |LSAGG-;reverse!;2A;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |LSAGG-;copy;2A;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Thing|)
                |LSAGG-;copyInto!;2AIA;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Integer| |%Shell|)
                    |%Integer|)
                |LSAGG-;position;SA2I;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |LSAGG-;removeDuplicates!;2A;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |LSAGG-;<;2AB;25|)) 

(DEFUN |LSAGG-;sort!;M2A;1| (|f| |l| $)
  (|LSAGG-;mergeSort| |f| |l| (SPADCALL |l| (|getShellEntry| $ 9)) $)) 

(DEFUN |LSAGG-;list;SA;2| (|x| $)
  (SPADCALL |x| (SPADCALL (|getShellEntry| $ 13))
      (|getShellEntry| $ 14))) 

(DEFUN |LSAGG-;reduce;MAS;3| (|f| |x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 16))
     (|error| "reducing over an empty list needs the 3 argument form"))
    ('T
     (SPADCALL |f| (SPADCALL |x| (|getShellEntry| $ 17))
         (SPADCALL |x| (|getShellEntry| $ 18)) (|getShellEntry| $ 20))))) 

(DEFUN |LSAGG-;merge;M3A;4| (|f| |p| |q| $)
  (SPADCALL |f| (SPADCALL |p| (|getShellEntry| $ 22))
      (SPADCALL |q| (|getShellEntry| $ 22)) (|getShellEntry| $ 23))) 

(DEFUN |LSAGG-;select!;M2A;5| (|f| |x| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) NIL)
                           ('T
                            (NOT (SPADCALL
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 18))
                                     |f|)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;select!;M2A;5|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (|getShellEntry| $ 16)) |x|)
                   ('T
                    (SEQ (LETT |y| |x| |LSAGG-;select!;M2A;5|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;select!;M2A;5|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT
                                        (SPADCALL |z|
                                         (|getShellEntry| $ 16))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL |z|
                                         (|getShellEntry| $ 18))
                                        |f|)
                                       (SEQ
                                        (LETT |y| |z|
                                         |LSAGG-;select!;M2A;5|)
                                        (EXIT
                                         (LETT |z|
                                          (SPADCALL |z|
                                           (|getShellEntry| $ 17))
                                          |LSAGG-;select!;M2A;5|))))
                                      ('T
                                       (SEQ
                                        (LETT |z|
                                         (SPADCALL |z|
                                          (|getShellEntry| $ 17))
                                         |LSAGG-;select!;M2A;5|)
                                        (EXIT
                                         (SPADCALL |y| |z|
                                          (|getShellEntry| $ 27))))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;merge!;M3A;6| (|f| |p| |q| $)
  (PROG (|r| |t|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 16)) |q|)
             ((SPADCALL |q| (|getShellEntry| $ 16)) |p|)
             ((SPADCALL |p| |q| (|getShellEntry| $ 30))
              (|error| "cannot merge a list into itself"))
             ('T
              (SEQ (COND
                     ((SPADCALL (SPADCALL |p| (|getShellEntry| $ 18))
                          (SPADCALL |q| (|getShellEntry| $ 18)) |f|)
                      (SEQ (LETT |r|
                                 (LETT |t| |p| |LSAGG-;merge!;M3A;6|)
                                 |LSAGG-;merge!;M3A;6|)
                           (EXIT (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 17))
                                       |LSAGG-;merge!;M3A;6|))))
                     ('T
                      (SEQ (LETT |r|
                                 (LETT |t| |q| |LSAGG-;merge!;M3A;6|)
                                 |LSAGG-;merge!;M3A;6|)
                           (EXIT (LETT |q|
                                       (SPADCALL |q|
                                        (|getShellEntry| $ 17))
                                       |LSAGG-;merge!;M3A;6|)))))
                   (SEQ G190
                        (COND
                          ((NULL (COND
                                   ((SPADCALL |p|
                                     (|getShellEntry| $ 16))
                                    NIL)
                                   ('T
                                    (NOT
                                     (SPADCALL |q|
                                      (|getShellEntry| $ 16))))))
                           (GO G191)))
                        (COND
                          ((SPADCALL
                               (SPADCALL |p| (|getShellEntry| $ 18))
                               (SPADCALL |q| (|getShellEntry| $ 18))
                               |f|)
                           (SEQ (SPADCALL |t| |p|
                                    (|getShellEntry| $ 27))
                                (LETT |t| |p| |LSAGG-;merge!;M3A;6|)
                                (EXIT (LETT |p|
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 17))
                                       |LSAGG-;merge!;M3A;6|))))
                          ('T
                           (SEQ (SPADCALL |t| |q|
                                    (|getShellEntry| $ 27))
                                (LETT |t| |q| |LSAGG-;merge!;M3A;6|)
                                (EXIT (LETT |q|
                                       (SPADCALL |q|
                                        (|getShellEntry| $ 17))
                                       |LSAGG-;merge!;M3A;6|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (SPADCALL |t|
                       (COND
                         ((SPADCALL |p| (|getShellEntry| $ 16)) |q|)
                         ('T |p|))
                       (|getShellEntry| $ 27))
                   (EXIT |r|)))))))) 

(DEFUN |LSAGG-;insert!;SAIA;7| (|s| |x| |i| $)
  (PROG (|m| |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 33))
                 |LSAGG-;insert!;SAIA;7|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |s| |x| (|getShellEntry| $ 14)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (LET
                                    ((#0=#:G1467 (- (- |i| 1) |m|)))
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 39))
                               |LSAGG-;insert!;SAIA;7|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;SAIA;7|)
                         (SPADCALL |y|
                             (SPADCALL |s| |z| (|getShellEntry| $ 14))
                             (|getShellEntry| $ 27))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;insert!;2AIA;8| (|w| |x| |i| $)
  (PROG (|m| |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 33))
                 |LSAGG-;insert!;2AIA;8|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |w| |x| (|getShellEntry| $ 41)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (LET
                                    ((#0=#:G1471 (- (- |i| 1) |m|)))
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 39))
                               |LSAGG-;insert!;2AIA;8|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;2AIA;8|)
                         (SPADCALL |y| |w| (|getShellEntry| $ 27))
                         (SPADCALL |y| |z| (|getShellEntry| $ 41))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;remove!;M2A;9| (|f| |x| $)
  (PROG (|p| |q|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) NIL)
                           ('T
                            (SPADCALL
                                (SPADCALL |x| (|getShellEntry| $ 18))
                                |f|))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;remove!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (|getShellEntry| $ 16)) |x|)
                   ('T
                    (SEQ (LETT |p| |x| |LSAGG-;remove!;M2A;9|)
                         (LETT |q|
                               (SPADCALL |x| (|getShellEntry| $ 17))
                               |LSAGG-;remove!;M2A;9|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT
                                        (SPADCALL |q|
                                         (|getShellEntry| $ 16))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL |q|
                                         (|getShellEntry| $ 18))
                                        |f|)
                                       (LETT |q|
                                        (SPADCALL |p|
                                         (SPADCALL |q|
                                          (|getShellEntry| $ 17))
                                         (|getShellEntry| $ 27))
                                        |LSAGG-;remove!;M2A;9|))
                                      ('T
                                       (SEQ
                                        (LETT |p| |q|
                                         |LSAGG-;remove!;M2A;9|)
                                        (EXIT
                                         (LETT |q|
                                          (SPADCALL |q|
                                           (|getShellEntry| $ 17))
                                          |LSAGG-;remove!;M2A;9|)))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AIA;10| (|x| |i| $)
  (PROG (|m| |y|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 33))
                 |LSAGG-;delete!;AIA;10|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |x| (|getShellEntry| $ 17)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (LET
                                    ((#0=#:G1483 (- (- |i| 1) |m|)))
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 39))
                               |LSAGG-;delete!;AIA;10|)
                         (SPADCALL |y|
                             (SPADCALL |y| 2 (|getShellEntry| $ 39))
                             (|getShellEntry| $ 27))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AUsA;11| (|x| |i| $)
  (PROG (|l| |m| |h| |t|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |i| (|getShellEntry| $ 46))
                 |LSAGG-;delete!;AUsA;11|)
           (LETT |m| (SPADCALL |x| (|getShellEntry| $ 33))
                 |LSAGG-;delete!;AUsA;11|)
           (EXIT (COND
                   ((< |l| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 47))
                                  (SPADCALL |i| (|getShellEntry| $ 48)))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 49))))
                               |LSAGG-;delete!;AUsA;11|)
                         (EXIT (COND
                                 ((< |h| |l|) |x|)
                                 ((EQL |l| |m|)
                                  (SPADCALL |x|
                                      (LET
                                       ((#0=#:G1489 (- (+ |h| 1) |m|)))
                                        (|check-subtype| (>= #0# 0)
                                         '(|NonNegativeInteger|) #0#))
                                      (|getShellEntry| $ 39)))
                                 ('T
                                  (SEQ (LETT |t|
                                        (SPADCALL |x|
                                         (LET
                                          ((#1=#:G1490
                                            (- (- |l| 1) |m|)))
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 39))
                                        |LSAGG-;delete!;AUsA;11|)
                                       (SPADCALL |t|
                                        (SPADCALL |t|
                                         (LET
                                          ((#2=#:G1491
                                            (+ (- |h| |l|) 2)))
                                           (|check-subtype| (>= #2# 0)
                                            '(|NonNegativeInteger|)
                                            #2#))
                                         (|getShellEntry| $ 39))
                                        (|getShellEntry| $ 27))
                                       (EXIT |x|))))))))))))) 

(DEFUN |LSAGG-;find;MAU;12| (|f| |x| $)
  (SEQ (SEQ G190
            (COND
              ((NULL (COND
                       ((SPADCALL |x| (|getShellEntry| $ 16)) NIL)
                       ('T
                        (NOT (SPADCALL
                                 (SPADCALL |x| (|getShellEntry| $ 18))
                                 |f|)))))
               (GO G191)))
            (SEQ (EXIT (LETT |x| (SPADCALL |x| (|getShellEntry| $ 17))
                             |LSAGG-;find;MAU;12|)))
            NIL (GO G190) G191 (EXIT NIL))
       (EXIT (COND
               ((SPADCALL |x| (|getShellEntry| $ 16))
                (CONS 1 "failed"))
               ('T (CONS 0 (SPADCALL |x| (|getShellEntry| $ 18)))))))) 

(DEFUN |LSAGG-;position;MAI;13| (|f| |x| $)
  (PROG (|k|)
    (RETURN
      (SEQ (SEQ (LETT |k| (SPADCALL |x| (|getShellEntry| $ 33))
                      |LSAGG-;position;MAI;13|)
                G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) NIL)
                           ('T
                            (NOT (SPADCALL
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 18))
                                     |f|)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;position;MAI;13|)))
                (SETQ |k| (+ |k| 1)) (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (|getShellEntry| $ 16))
                    (- (SPADCALL |x| (|getShellEntry| $ 33)) 1))
                   ('T |k|))))))) 

(DEFUN |LSAGG-;mergeSort| (|f| |p| |n| $)
  (PROG (|l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (|getShellEntry| $ 17))
                         (|getShellEntry| $ 18))
                     (SPADCALL |p| (|getShellEntry| $ 18)) |f|)
                 (LETT |p| (SPADCALL |p| (|getShellEntry| $ 57))
                       |LSAGG-;mergeSort|)))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   ('T
                    (SEQ (LETT |l|
                               (LET ((#0=#:G1510 (QUOTIENT2 |n| 2)))
                                 (|check-subtype| (>= #0# 0)
                                     '(|NonNegativeInteger|) #0#))
                               |LSAGG-;mergeSort|)
                         (LETT |q|
                               (SPADCALL |p| |l|
                                   (|getShellEntry| $ 59))
                               |LSAGG-;mergeSort|)
                         (LETT |p| (|LSAGG-;mergeSort| |f| |p| |l| $)
                               |LSAGG-;mergeSort|)
                         (LETT |q|
                               (|LSAGG-;mergeSort| |f| |q| (- |n| |l|)
                                   $)
                               |LSAGG-;mergeSort|)
                         (EXIT (SPADCALL |f| |p| |q|
                                   (|getShellEntry| $ 23))))))))))) 

(DEFUN |LSAGG-;sorted?;MAB;15| (|f| |l| $)
  (PROG (#0=#:G1516 |p|)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |l| (|getShellEntry| $ 16)) T)
                   ('T
                    (SEQ (LETT |p|
                               (SPADCALL |l| (|getShellEntry| $ 17))
                               |LSAGG-;sorted?;MAB;15|)
                         (SEQ G190
                              (COND
                                ((NULL (NOT
                                        (SPADCALL |p|
                                         (|getShellEntry| $ 16))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((NOT
                                        (SPADCALL
                                         (SPADCALL |l|
                                          (|getShellEntry| $ 18))
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 18))
                                         |f|))
                                       (PROGN
                                         (LETT #0# NIL
                                          |LSAGG-;sorted?;MAB;15|)
                                         (GO #0#)))
                                      ('T
                                       (LETT |p|
                                        (SPADCALL
                                         (LETT |l| |p|
                                          |LSAGG-;sorted?;MAB;15|)
                                         (|getShellEntry| $ 17))
                                        |LSAGG-;sorted?;MAB;15|)))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT T)))))
           #0# (EXIT #0#))))) 

(DEFUN |LSAGG-;reduce;MA2S;16| (|f| |x| |i| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| |i| |LSAGG-;reduce;MA2S;16|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |x| (|getShellEntry| $ 16))))
                   (GO G191)))
                (SEQ (LETT |r|
                           (SPADCALL |r|
                               (SPADCALL |x| (|getShellEntry| $ 18))
                               |f|)
                           |LSAGG-;reduce;MA2S;16|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;reduce;MA2S;16|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |LSAGG-;reduce;MA3S;17| (|f| |x| |i| |a| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| |i| |LSAGG-;reduce;MA3S;17|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) NIL)
                           ('T
                            (SPADCALL |r| |a| (|getShellEntry| $ 63)))))
                   (GO G191)))
                (SEQ (LETT |r|
                           (SPADCALL |r|
                               (SPADCALL |x| (|getShellEntry| $ 18))
                               |f|)
                           |LSAGG-;reduce;MA3S;17|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;reduce;MA3S;17|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |LSAGG-;new;NniSA;18| (|n| |s| $)
  (PROG (|k| |l|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL (|getShellEntry| $ 13))
                 |LSAGG-;new;NniSA;18|)
           (SEQ (LETT |k| 1 |LSAGG-;new;NniSA;18|) G190
                (COND ((QSGREATERP |k| |n|) (GO G191)))
                (SEQ (EXIT (LETT |l|
                                 (SPADCALL |s| |l|
                                     (|getShellEntry| $ 14))
                                 |LSAGG-;new;NniSA;18|)))
                (SETQ |k| (QSADD1 |k|)) (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |LSAGG-;map;M3A;19| (|f| |x| |y| $)
  (PROG (|z|)
    (RETURN
      (SEQ (LETT |z| (SPADCALL (|getShellEntry| $ 13))
                 |LSAGG-;map;M3A;19|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) NIL)
                           ('T
                            (NOT (SPADCALL |y| (|getShellEntry| $ 16))))))
                   (GO G191)))
                (SEQ (LETT |z|
                           (SPADCALL
                               (SPADCALL
                                   (SPADCALL |x|
                                    (|getShellEntry| $ 18))
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 18))
                                   |f|)
                               |z| (|getShellEntry| $ 14))
                           |LSAGG-;map;M3A;19|)
                     (LETT |x| (SPADCALL |x| (|getShellEntry| $ 17))
                           |LSAGG-;map;M3A;19|)
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 17))
                                 |LSAGG-;map;M3A;19|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |z| (|getShellEntry| $ 57))))))) 

(DEFUN |LSAGG-;reverse!;2A;20| (|x| $)
  (PROG (|z| |y|)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |x| (|getShellEntry| $ 16))
                  (SPADCALL
                      (LETT |y| (SPADCALL |x| (|getShellEntry| $ 17))
                            |LSAGG-;reverse!;2A;20|)
                      (|getShellEntry| $ 16)))
              |x|)
             ('T
              (SEQ (SPADCALL |x| (SPADCALL (|getShellEntry| $ 13))
                       (|getShellEntry| $ 27))
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL |y|
                                       (|getShellEntry| $ 16))))
                           (GO G191)))
                        (SEQ (LETT |z|
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 17))
                                   |LSAGG-;reverse!;2A;20|)
                             (SPADCALL |y| |x| (|getShellEntry| $ 27))
                             (LETT |x| |y| |LSAGG-;reverse!;2A;20|)
                             (EXIT (LETT |y| |z|
                                    |LSAGG-;reverse!;2A;20|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |x|)))))))) 

(DEFUN |LSAGG-;copy;2A;21| (|x| $)
  (PROG (|k| |y|)
    (RETURN
      (SEQ (LETT |y| (SPADCALL (|getShellEntry| $ 13))
                 |LSAGG-;copy;2A;21|)
           (SEQ (LETT |k| 0 |LSAGG-;copy;2A;21|) G190
                (COND
                  ((NULL (NOT (SPADCALL |x| (|getShellEntry| $ 16))))
                   (GO G191)))
                (SEQ (COND
                       ((EQL |k| 1000)
                        (COND
                          ((SPADCALL |x| (|getShellEntry| $ 69))
                           (EXIT (|error| "cyclic list"))))))
                     (LETT |y|
                           (SPADCALL
                               (SPADCALL |x| (|getShellEntry| $ 18))
                               |y| (|getShellEntry| $ 14))
                           |LSAGG-;copy;2A;21|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;copy;2A;21|)))
                (SETQ |k| (QSADD1 |k|)) (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |y| (|getShellEntry| $ 57))))))) 

(DEFUN |LSAGG-;copyInto!;2AIA;22| (|y| |x| |s| $)
  (PROG (|m| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |y| (|getShellEntry| $ 33))
                 |LSAGG-;copyInto!;2AIA;22|)
           (EXIT (COND
                   ((< |s| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL |y|
                                   (LET ((#0=#:G1551 (- |s| |m|)))
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 39))
                               |LSAGG-;copyInto!;2AIA;22|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |z|
                                           (|getShellEntry| $ 16))
                                          NIL)
                                         ('T
                                          (NOT
                                           (SPADCALL |x|
                                            (|getShellEntry| $ 16))))))
                                 (GO G191)))
                              (SEQ (SPADCALL |z|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 18))
                                    (|getShellEntry| $ 71))
                                   (LETT |x|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 17))
                                    |LSAGG-;copyInto!;2AIA;22|)
                                   (EXIT
                                    (LETT |z|
                                     (SPADCALL |z|
                                      (|getShellEntry| $ 17))
                                     |LSAGG-;copyInto!;2AIA;22|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |y|))))))))) 

(DEFUN |LSAGG-;position;SA2I;23| (|w| |x| |s| $)
  (PROG (|m| |k|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 33))
                 |LSAGG-;position;SA2I;23|)
           (EXIT (COND
                   ((< |s| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |x|
                               (SPADCALL |x|
                                   (LET ((#0=#:G1557 (- |s| |m|)))
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 39))
                               |LSAGG-;position;SA2I;23|)
                         (SEQ (LETT |k| |s| |LSAGG-;position;SA2I;23|)
                              G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |x|
                                           (|getShellEntry| $ 16))
                                          NIL)
                                         ('T
                                          (SPADCALL |w|
                                           (SPADCALL |x|
                                            (|getShellEntry| $ 18))
                                           (|getShellEntry| $ 63)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (LETT |x|
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 17))
                                     |LSAGG-;position;SA2I;23|)))
                              (SETQ |k| (+ |k| 1)) (GO G190) G191
                              (EXIT NIL))
                         (EXIT (COND
                                 ((SPADCALL |x| (|getShellEntry| $ 16))
                                  (- (SPADCALL |x|
                                      (|getShellEntry| $ 33))
                                     1))
                                 ('T |k|))))))))))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24| (|l| $)
  (PROG (|p|)
    (RETURN
      (SEQ (LETT |p| |l| |LSAGG-;removeDuplicates!;2A;24|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |p| (|getShellEntry| $ 16))))
                   (GO G191)))
                (SEQ (EXIT (LETT |p|
                                 (SPADCALL |p|
                                     (SPADCALL
                                      (CONS
                                       #'|LSAGG-;removeDuplicates!;2A;24!0|
                                       (VECTOR $ |p|))
                                      (SPADCALL |p|
                                       (|getShellEntry| $ 17))
                                      (|getShellEntry| $ 75))
                                     (|getShellEntry| $ 27))
                                 |LSAGG-;removeDuplicates!;2A;24|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24!0| (|#1| $$)
  (PROG ($)
    (SETQ $ (|getShellEntry| $$ 0))
    (RETURN
      (PROGN
        (SPADCALL |#1|
            (SPADCALL (|getShellEntry| $$ 1) (|getShellEntry| $ 18))
            (|getShellEntry| $ 74)))))) 

(DEFUN |LSAGG-;<;2AB;25| (|x| |y| $)
  (PROG (#0=#:G1566)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND
                             ((NULL (COND
                                      ((SPADCALL |x|
                                        (|getShellEntry| $ 16))
                                       NIL)
                                      ('T
                                       (NOT
                                        (SPADCALL |y|
                                         (|getShellEntry| $ 16))))))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL
                                          (SPADCALL |x|
                                           (|getShellEntry| $ 18))
                                          (SPADCALL |y|
                                           (|getShellEntry| $ 18))
                                          (|getShellEntry| $ 63))
                                         (PROGN
                                           (LETT #0#
                                            (SPADCALL
                                             (SPADCALL |x|
                                              (|getShellEntry| $ 18))
                                             (SPADCALL |y|
                                              (|getShellEntry| $ 18))
                                             (|getShellEntry| $ 77))
                                            |LSAGG-;<;2AB;25|)
                                           (GO #0#)))
                                        ('T
                                         (SEQ
                                          (LETT |x|
                                           (SPADCALL |x|
                                            (|getShellEntry| $ 17))
                                           |LSAGG-;<;2AB;25|)
                                          (EXIT
                                           (LETT |y|
                                            (SPADCALL |y|
                                             (|getShellEntry| $ 17))
                                            |LSAGG-;<;2AB;25|)))))))
                           NIL (GO G190) G191 (EXIT NIL))
                      (EXIT (COND
                              ((SPADCALL |x| (|getShellEntry| $ 16))
                               (NOT (SPADCALL |y|
                                     (|getShellEntry| $ 16))))
                              ('T NIL)))))
           #0# (EXIT #0#))))) 

(DEFUN |ListAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|ListAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 80)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    (COND
      ((|HasCategory| |#2| '(|SetCategory|))
       (|setShellEntry| $ 64
           (CONS (|dispatchFunction| |LSAGG-;reduce;MA3S;17|) $))))
    (COND
      ((|HasCategory| |#2| '(|SetCategory|))
       (PROGN
         (|setShellEntry| $ 73
             (CONS (|dispatchFunction| |LSAGG-;position;SA2I;23|) $))
         (|setShellEntry| $ 76
             (CONS (|dispatchFunction|
                       |LSAGG-;removeDuplicates!;2A;24|)
                   $)))))
    (COND
      ((|HasCategory| |#2| '(|OrderedSet|))
       (|setShellEntry| $ 78
           (CONS (|dispatchFunction| |LSAGG-;<;2AB;25|) $))))
    $)) 

(MAKEPROP '|ListAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|NonNegativeInteger|) (0 . |#|) (|Boolean|)
             (|Mapping| 10 7 7) |LSAGG-;sort!;M2A;1| (5 . |empty|)
             (9 . |concat|) |LSAGG-;list;SA;2| (15 . |empty?|)
             (20 . |rest|) (25 . |first|) (|Mapping| 7 7 7)
             (30 . |reduce|) |LSAGG-;reduce;MAS;3| (37 . |copy|)
             (42 . |merge!|) |LSAGG-;merge;M3A;4| (49 . |false|)
             (53 . |not|) (58 . |setrest!|) (|Mapping| 10 7)
             |LSAGG-;select!;M2A;5| (64 . |eq?|) |LSAGG-;merge!;M3A;6|
             (|Integer|) (70 . |minIndex|) (75 . <) (81 . =)
             (87 . |One|) (91 . |One|) (95 . -) (101 . |rest|)
             |LSAGG-;insert!;SAIA;7| (107 . |concat!|)
             |LSAGG-;insert!;2AIA;8| |LSAGG-;remove!;M2A;9|
             |LSAGG-;delete!;AIA;10| (|UniversalSegment| 32)
             (113 . |lo|) (118 . |hasHi|) (123 . |hi|)
             (128 . |maxIndex|) (133 . +) |LSAGG-;delete!;AUsA;11|
             (|Union| 7 '"failed") |LSAGG-;find;MAU;12|
             (|PositiveInteger|) (139 . |One|) |LSAGG-;position;MAI;13|
             (143 . |reverse!|) (148 . |quo|) (154 . |split!|)
             (160 . |true|) |LSAGG-;sorted?;MAB;15|
             |LSAGG-;reduce;MA2S;16| (164 . ~=) (170 . |reduce|)
             |LSAGG-;new;NniSA;18| |LSAGG-;map;M3A;19|
             |LSAGG-;reverse!;2A;20| (178 . =) (184 . |cyclic?|)
             |LSAGG-;copy;2A;21| (189 . |setfirst!|)
             |LSAGG-;copyInto!;2AIA;22| (195 . |position|) (202 . =)
             (208 . |remove!|) (214 . |removeDuplicates!|) (219 . <)
             (225 . <) (|Mapping| 7 7))
          '#(|sorted?| 231 |sort!| 237 |select!| 243 |reverse!| 249
             |removeDuplicates!| 254 |remove!| 259 |reduce| 265
             |position| 286 |new| 299 |merge!| 305 |merge| 312 |map|
             319 |list| 326 |insert!| 331 |find| 345 |delete!| 351
             |copyInto!| 363 |copy| 370 < 375)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 78
                                '(1 6 8 0 9 0 6 0 13 2 6 0 7 0 14 1 6
                                  10 0 16 1 6 0 0 17 1 6 7 0 18 3 6 7
                                  19 0 7 20 1 6 0 0 22 3 6 0 11 0 0 23
                                  0 10 0 25 1 10 0 0 26 2 6 0 0 0 27 2
                                  6 10 0 0 30 1 6 32 0 33 2 32 10 0 0
                                  34 2 32 10 0 0 35 0 8 0 36 0 32 0 37
                                  2 32 0 0 0 38 2 6 0 0 8 39 2 6 0 0 0
                                  41 1 45 32 0 46 1 45 10 0 47 1 45 32
                                  0 48 1 6 32 0 49 2 32 0 0 0 50 0 54 0
                                  55 1 6 0 0 57 2 32 0 0 0 58 2 6 0 0
                                  32 59 0 10 0 60 2 7 10 0 0 63 4 0 7
                                  19 0 7 7 64 2 8 10 0 0 68 1 6 10 0 69
                                  2 6 7 0 7 71 3 0 32 7 0 32 73 2 7 10
                                  0 0 74 2 6 0 28 0 75 1 0 0 0 76 2 7
                                  10 0 0 77 2 0 10 0 0 78 2 0 10 11 0
                                  61 2 0 0 11 0 12 2 0 0 28 0 29 1 0 0
                                  0 67 1 0 0 0 76 2 0 0 28 0 43 3 0 7
                                  19 0 7 62 4 0 7 19 0 7 7 64 2 0 7 19
                                  0 21 2 0 32 28 0 56 3 0 32 7 0 32 73
                                  2 0 0 8 7 65 3 0 0 11 0 0 31 3 0 0 11
                                  0 0 24 3 0 0 19 0 0 66 1 0 0 7 15 3 0
                                  0 7 0 32 40 3 0 0 0 0 32 42 2 0 52 28
                                  0 53 2 0 0 0 45 51 2 0 0 0 32 44 3 0
                                  0 0 0 32 72 1 0 0 0 70 2 0 10 0 0 78)))))
          '|lookupComplete|)) 
