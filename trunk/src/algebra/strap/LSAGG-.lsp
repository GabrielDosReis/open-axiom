
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
                           ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
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
                                          (|getShellEntry| $ 25))))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;merge!;M3A;6| (|f| |p| |q| $)
  (PROG (|r| |t|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 16)) |q|)
             ((SPADCALL |q| (|getShellEntry| $ 16)) |p|)
             ((SPADCALL |p| |q| (|getShellEntry| $ 28))
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
                                    'NIL)
                                   ('T
                                    (NOT
                                     (SPADCALL |q|
                                      (|getShellEntry| $ 16))))))
                           (GO G191)))
                        (SEQ (EXIT (COND
                                     ((SPADCALL
                                       (SPADCALL |p|
                                        (|getShellEntry| $ 18))
                                       (SPADCALL |q|
                                        (|getShellEntry| $ 18))
                                       |f|)
                                      (SEQ
                                       (SPADCALL |t| |p|
                                        (|getShellEntry| $ 25))
                                       (LETT |t| |p|
                                        |LSAGG-;merge!;M3A;6|)
                                       (EXIT
                                        (LETT |p|
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 17))
                                         |LSAGG-;merge!;M3A;6|))))
                                     ('T
                                      (SEQ
                                       (SPADCALL |t| |q|
                                        (|getShellEntry| $ 25))
                                       (LETT |t| |q|
                                        |LSAGG-;merge!;M3A;6|)
                                       (EXIT
                                        (LETT |q|
                                         (SPADCALL |q|
                                          (|getShellEntry| $ 17))
                                         |LSAGG-;merge!;M3A;6|)))))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (SPADCALL |t|
                       (COND
                         ((SPADCALL |p| (|getShellEntry| $ 16)) |q|)
                         ('T |p|))
                       (|getShellEntry| $ 25))
                   (EXIT |r|)))))))) 

(DEFUN |LSAGG-;insert!;SAIA;7| (|s| |x| |i| $)
  (PROG (|m| #0=#:G1464 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 31))
                 |LSAGG-;insert!;SAIA;7|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |s| |x| (|getShellEntry| $ 14)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;insert!;SAIA;7|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 32))
                               |LSAGG-;insert!;SAIA;7|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;SAIA;7|)
                         (SPADCALL |y|
                             (SPADCALL |s| |z| (|getShellEntry| $ 14))
                             (|getShellEntry| $ 25))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;insert!;2AIA;8| (|w| |x| |i| $)
  (PROG (|m| #0=#:G1468 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 31))
                 |LSAGG-;insert!;2AIA;8|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |w| |x| (|getShellEntry| $ 34)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;insert!;2AIA;8|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 32))
                               |LSAGG-;insert!;2AIA;8|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;2AIA;8|)
                         (SPADCALL |y| |w| (|getShellEntry| $ 25))
                         (SPADCALL |y| |z| (|getShellEntry| $ 34))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;remove!;M2A;9| (|f| |x| $)
  (PROG (|p| |q|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
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
                                         (|getShellEntry| $ 25))
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
  (PROG (|m| #0=#:G1480 |y|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 31))
                 |LSAGG-;delete!;AIA;10|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |x| (|getShellEntry| $ 17)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;delete!;AIA;10|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 32))
                               |LSAGG-;delete!;AIA;10|)
                         (SPADCALL |y|
                             (SPADCALL |y| 2 (|getShellEntry| $ 32))
                             (|getShellEntry| $ 25))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AUsA;11| (|x| |i| $)
  (PROG (|l| |m| |h| #0=#:G1485 #1=#:G1486 |t| #2=#:G1487)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |i| (|getShellEntry| $ 39))
                 |LSAGG-;delete!;AUsA;11|)
           (LETT |m| (SPADCALL |x| (|getShellEntry| $ 31))
                 |LSAGG-;delete!;AUsA;11|)
           (EXIT (COND
                   ((< |l| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 40))
                                  (SPADCALL |i| (|getShellEntry| $ 41)))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 42))))
                               |LSAGG-;delete!;AUsA;11|)
                         (EXIT (COND
                                 ((< |h| |l|) |x|)
                                 ((EQL |l| |m|)
                                  (SPADCALL |x|
                                      (PROG1
                                       (LETT #0# (- (+ |h| 1) |m|)
                                        |LSAGG-;delete!;AUsA;11|)
                                        (|check-subtype| (>= #0# 0)
                                         '(|NonNegativeInteger|) #0#))
                                      (|getShellEntry| $ 32)))
                                 ('T
                                  (SEQ (LETT |t|
                                        (SPADCALL |x|
                                         (PROG1
                                          (LETT #1# (- (- |l| 1) |m|)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 32))
                                        |LSAGG-;delete!;AUsA;11|)
                                       (SPADCALL |t|
                                        (SPADCALL |t|
                                         (PROG1
                                          (LETT #2# (+ (- |h| |l|) 2)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype| (>= #2# 0)
                                            '(|NonNegativeInteger|)
                                            #2#))
                                         (|getShellEntry| $ 32))
                                        (|getShellEntry| $ 25))
                                       (EXIT |x|))))))))))))) 

(DEFUN |LSAGG-;find;MAU;12| (|f| |x| $)
  (SEQ (SEQ G190
            (COND
              ((NULL (COND
                       ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
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
      (SEQ (SEQ (LETT |k| (SPADCALL |x| (|getShellEntry| $ 31))
                      |LSAGG-;position;MAI;13|)
                G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
                           ('T
                            (NOT (SPADCALL
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 18))
                                     |f|)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;position;MAI;13|)))
                (LETT |k| (+ |k| 1) |LSAGG-;position;MAI;13|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (|getShellEntry| $ 16))
                    (- (SPADCALL |x| (|getShellEntry| $ 31)) 1))
                   ('T |k|))))))) 

(DEFUN |LSAGG-;mergeSort| (|f| |p| |n| $)
  (PROG (#0=#:G1507 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (|getShellEntry| $ 17))
                         (|getShellEntry| $ 18))
                     (SPADCALL |p| (|getShellEntry| $ 18)) |f|)
                 (LETT |p| (SPADCALL |p| (|getShellEntry| $ 47))
                       |LSAGG-;mergeSort|)))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   ('T
                    (SEQ (LETT |l|
                               (PROG1 (LETT #0# (QUOTIENT2 |n| 2)
                                       |LSAGG-;mergeSort|)
                                 (|check-subtype| (>= #0# 0)
                                     '(|NonNegativeInteger|) #0#))
                               |LSAGG-;mergeSort|)
                         (LETT |q|
                               (SPADCALL |p| |l|
                                   (|getShellEntry| $ 48))
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
                   ((SPADCALL |l| (|getShellEntry| $ 16)) 'T)
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
                                      ((NULL
                                        (SPADCALL
                                         (SPADCALL |l|
                                          (|getShellEntry| $ 18))
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 18))
                                         |f|))
                                       (PROGN
                                         (LETT #0# 'NIL
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
                         (EXIT 'T)))))
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
                           ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
                           ('T
                            (SPADCALL |r| |a| (|getShellEntry| $ 51)))))
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
                (LETT |k| (QSADD1 |k|) |LSAGG-;new;NniSA;18|) (GO G190)
                G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |LSAGG-;map;M3A;19| (|f| |x| |y| $)
  (PROG (|z|)
    (RETURN
      (SEQ (LETT |z| (SPADCALL (|getShellEntry| $ 13))
                 |LSAGG-;map;M3A;19|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
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
           (EXIT (SPADCALL |z| (|getShellEntry| $ 47))))))) 

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
                       (|getShellEntry| $ 25))
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL |y|
                                       (|getShellEntry| $ 16))))
                           (GO G191)))
                        (SEQ (LETT |z|
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 17))
                                   |LSAGG-;reverse!;2A;20|)
                             (SPADCALL |y| |x| (|getShellEntry| $ 25))
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
                          ((SPADCALL |x| (|getShellEntry| $ 56))
                           (EXIT (|error| "cyclic list"))))))
                     (LETT |y|
                           (SPADCALL
                               (SPADCALL |x| (|getShellEntry| $ 18))
                               |y| (|getShellEntry| $ 14))
                           |LSAGG-;copy;2A;21|)
                     (EXIT (LETT |x|
                                 (SPADCALL |x| (|getShellEntry| $ 17))
                                 |LSAGG-;copy;2A;21|)))
                (LETT |k| (QSADD1 |k|) |LSAGG-;copy;2A;21|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (SPADCALL |y| (|getShellEntry| $ 47))))))) 

(DEFUN |LSAGG-;copyInto!;2AIA;22| (|y| |x| |s| $)
  (PROG (|m| #0=#:G1545 |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |y| (|getShellEntry| $ 31))
                 |LSAGG-;copyInto!;2AIA;22|)
           (EXIT (COND
                   ((< |s| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL |y|
                                   (PROG1
                                    (LETT #0# (- |s| |m|)
                                     |LSAGG-;copyInto!;2AIA;22|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 32))
                               |LSAGG-;copyInto!;2AIA;22|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |z|
                                           (|getShellEntry| $ 16))
                                          'NIL)
                                         ('T
                                          (NOT
                                           (SPADCALL |x|
                                            (|getShellEntry| $ 16))))))
                                 (GO G191)))
                              (SEQ (SPADCALL |z|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 18))
                                    (|getShellEntry| $ 58))
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
  (PROG (|m| #0=#:G1552 |k|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 31))
                 |LSAGG-;position;SA2I;23|)
           (EXIT (COND
                   ((< |s| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |x|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- |s| |m|)
                                     |LSAGG-;position;SA2I;23|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 32))
                               |LSAGG-;position;SA2I;23|)
                         (SEQ (LETT |k| |s| |LSAGG-;position;SA2I;23|)
                              G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |x|
                                           (|getShellEntry| $ 16))
                                          'NIL)
                                         ('T
                                          (SPADCALL |w|
                                           (SPADCALL |x|
                                            (|getShellEntry| $ 18))
                                           (|getShellEntry| $ 51)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (LETT |x|
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 17))
                                     |LSAGG-;position;SA2I;23|)))
                              (LETT |k| (+ |k| 1)
                                    |LSAGG-;position;SA2I;23|)
                              (GO G190) G191 (EXIT NIL))
                         (EXIT (COND
                                 ((SPADCALL |x| (|getShellEntry| $ 16))
                                  (- (SPADCALL |x|
                                      (|getShellEntry| $ 31))
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
                                      (|getShellEntry| $ 62))
                                     (|getShellEntry| $ 25))
                                 |LSAGG-;removeDuplicates!;2A;24|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24!0| (|#1| $$)
  (PROG ($)
    (LETT $ (|getShellEntry| $$ 0) |LSAGG-;removeDuplicates!;2A;24|)
    (RETURN
      (PROGN
        (SPADCALL |#1|
            (SPADCALL (|getShellEntry| $$ 1) (|getShellEntry| $ 18))
            (|getShellEntry| $ 61)))))) 

(DEFUN |LSAGG-;<;2AB;25| (|x| |y| $)
  (PROG (#0=#:G1566)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND
                             ((NULL (COND
                                      ((SPADCALL |x|
                                        (|getShellEntry| $ 16))
                                       'NIL)
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
                                          (|getShellEntry| $ 51))
                                         (PROGN
                                           (LETT #0#
                                            (SPADCALL
                                             (SPADCALL |x|
                                              (|getShellEntry| $ 18))
                                             (SPADCALL |y|
                                              (|getShellEntry| $ 18))
                                             (|getShellEntry| $ 64))
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
                              ('T 'NIL)))))
           #0# (EXIT #0#))))) 

(DEFUN |ListAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|ListAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|ListAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 67) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (|setShellEntry| $ 52
               (CONS (|dispatchFunction| |LSAGG-;reduce;MA3S;17|) $))))
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (PROGN
             (|setShellEntry| $ 60
                 (CONS (|dispatchFunction| |LSAGG-;position;SA2I;23|)
                       $))
             (|setShellEntry| $ 63
                 (CONS (|dispatchFunction|
                           |LSAGG-;removeDuplicates!;2A;24|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|OrderedSet|))
           (|setShellEntry| $ 65
               (CONS (|dispatchFunction| |LSAGG-;<;2AB;25|) $))))
        $)))) 

(MAKEPROP '|ListAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|NonNegativeInteger|) (0 . |#|) (|Boolean|)
             (|Mapping| 10 7 7) |LSAGG-;sort!;M2A;1| (5 . |empty|)
             (9 . |concat|) |LSAGG-;list;SA;2| (15 . |empty?|)
             (20 . |rest|) (25 . |first|) (|Mapping| 7 7 7)
             (30 . |reduce|) |LSAGG-;reduce;MAS;3| (37 . |copy|)
             (42 . |merge!|) |LSAGG-;merge;M3A;4| (49 . |setrest!|)
             (|Mapping| 10 7) |LSAGG-;select!;M2A;5| (55 . |eq?|)
             |LSAGG-;merge!;M3A;6| (|Integer|) (61 . |minIndex|)
             (66 . |rest|) |LSAGG-;insert!;SAIA;7| (72 . |concat!|)
             |LSAGG-;insert!;2AIA;8| |LSAGG-;remove!;M2A;9|
             |LSAGG-;delete!;AIA;10| (|UniversalSegment| 30)
             (78 . |lo|) (83 . |hasHi|) (88 . |hi|) (93 . |maxIndex|)
             |LSAGG-;delete!;AUsA;11| (|Union| 7 '"failed")
             |LSAGG-;find;MAU;12| |LSAGG-;position;MAI;13|
             (98 . |reverse!|) (103 . |split!|) |LSAGG-;sorted?;MAB;15|
             |LSAGG-;reduce;MA2S;16| (109 . ~=) (115 . |reduce|)
             |LSAGG-;new;NniSA;18| |LSAGG-;map;M3A;19|
             |LSAGG-;reverse!;2A;20| (123 . |cyclic?|)
             |LSAGG-;copy;2A;21| (128 . |setfirst!|)
             |LSAGG-;copyInto!;2AIA;22| (134 . |position|) (141 . =)
             (147 . |remove!|) (153 . |removeDuplicates!|) (158 . <)
             (164 . <) (|Mapping| 7 7))
          '#(|sorted?| 170 |sort!| 176 |select!| 182 |reverse!| 188
             |removeDuplicates!| 193 |remove!| 198 |reduce| 204
             |position| 225 |new| 238 |merge!| 244 |merge| 251 |map|
             258 |list| 265 |insert!| 270 |find| 284 |delete!| 290
             |copyInto!| 302 |copy| 309 < 314)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 65
                                '(1 6 8 0 9 0 6 0 13 2 6 0 7 0 14 1 6
                                  10 0 16 1 6 0 0 17 1 6 7 0 18 3 6 7
                                  19 0 7 20 1 6 0 0 22 3 6 0 11 0 0 23
                                  2 6 0 0 0 25 2 6 10 0 0 28 1 6 30 0
                                  31 2 6 0 0 8 32 2 6 0 0 0 34 1 38 30
                                  0 39 1 38 10 0 40 1 38 30 0 41 1 6 30
                                  0 42 1 6 0 0 47 2 6 0 0 30 48 2 7 10
                                  0 0 51 4 0 7 19 0 7 7 52 1 6 10 0 56
                                  2 6 7 0 7 58 3 0 30 7 0 30 60 2 7 10
                                  0 0 61 2 6 0 26 0 62 1 0 0 0 63 2 7
                                  10 0 0 64 2 0 10 0 0 65 2 0 10 11 0
                                  49 2 0 0 11 0 12 2 0 0 26 0 27 1 0 0
                                  0 55 1 0 0 0 63 2 0 0 26 0 36 3 0 7
                                  19 0 7 50 4 0 7 19 0 7 7 52 2 0 7 19
                                  0 21 2 0 30 26 0 46 3 0 30 7 0 30 60
                                  2 0 0 8 7 53 3 0 0 11 0 0 29 3 0 0 11
                                  0 0 24 3 0 0 19 0 0 54 1 0 0 7 15 3 0
                                  0 7 0 30 33 3 0 0 0 0 30 35 2 0 44 26
                                  0 45 2 0 0 0 38 43 2 0 0 0 30 37 3 0
                                  0 0 0 30 59 1 0 0 0 57 2 0 10 0 0 65)))))
          '|lookupComplete|)) 
