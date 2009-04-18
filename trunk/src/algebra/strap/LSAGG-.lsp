
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
                                          (|getShellEntry| $ 26))))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;merge!;M3A;6| (|f| |p| |q| $)
  (PROG (|r| |t|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (|getShellEntry| $ 16)) |q|)
             ((SPADCALL |q| (|getShellEntry| $ 16)) |p|)
             ((SPADCALL |p| |q| (|getShellEntry| $ 29))
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
                                        (|getShellEntry| $ 26))
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
                                        (|getShellEntry| $ 26))
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
                       (|getShellEntry| $ 26))
                   (EXIT |r|)))))))) 

(DEFUN |LSAGG-;insert!;SAIA;7| (|s| |x| |i| $)
  (PROG (|m| #0=#:G1465 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
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
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 38))
                               |LSAGG-;insert!;SAIA;7|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;SAIA;7|)
                         (SPADCALL |y|
                             (SPADCALL |s| |z| (|getShellEntry| $ 14))
                             (|getShellEntry| $ 26))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;insert!;2AIA;8| (|w| |x| |i| $)
  (PROG (|m| #0=#:G1469 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
                 |LSAGG-;insert!;2AIA;8|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |w| |x| (|getShellEntry| $ 40)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;insert!;2AIA;8|)
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 38))
                               |LSAGG-;insert!;2AIA;8|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;2AIA;8|)
                         (SPADCALL |y| |w| (|getShellEntry| $ 26))
                         (SPADCALL |y| |z| (|getShellEntry| $ 40))
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
                                         (|getShellEntry| $ 26))
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
  (PROG (|m| #0=#:G1481 |y|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
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
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 38))
                               |LSAGG-;delete!;AIA;10|)
                         (SPADCALL |y|
                             (SPADCALL |y| 2 (|getShellEntry| $ 38))
                             (|getShellEntry| $ 26))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AUsA;11| (|x| |i| $)
  (PROG (|l| |m| |h| #0=#:G1486 #1=#:G1487 |t| #2=#:G1488)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |i| (|getShellEntry| $ 45))
                 |LSAGG-;delete!;AUsA;11|)
           (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
                 |LSAGG-;delete!;AUsA;11|)
           (EXIT (COND
                   ((< |l| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 46))
                                  (SPADCALL |i| (|getShellEntry| $ 47)))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 48))))
                               |LSAGG-;delete!;AUsA;11|)
                         (EXIT (COND
                                 ((< |h| |l|) |x|)
                                 ((EQL |l| |m|)
                                  (SPADCALL |x|
                                      (PROG1
                                       (LETT #0# (- (+ |h| 1) |m|)
                                        |LSAGG-;delete!;AUsA;11|)
                                        (|check-subtype|
                                         (COND
                                           ((< #0# 0) 'NIL)
                                           ('T 'T))
                                         '(|NonNegativeInteger|) #0#))
                                      (|getShellEntry| $ 38)))
                                 ('T
                                  (SEQ (LETT |t|
                                        (SPADCALL |x|
                                         (PROG1
                                          (LETT #1# (- (- |l| 1) |m|)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype|
                                            (COND
                                              ((< #1# 0) 'NIL)
                                              ('T 'T))
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 38))
                                        |LSAGG-;delete!;AUsA;11|)
                                       (SPADCALL |t|
                                        (SPADCALL |t|
                                         (PROG1
                                          (LETT #2# (+ (- |h| |l|) 2)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype|
                                            (COND
                                              ((< #2# 0) 'NIL)
                                              ('T 'T))
                                            '(|NonNegativeInteger|)
                                            #2#))
                                         (|getShellEntry| $ 38))
                                        (|getShellEntry| $ 26))
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
      (SEQ (SEQ (LETT |k| (SPADCALL |x| (|getShellEntry| $ 32))
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
                    (- (SPADCALL |x| (|getShellEntry| $ 32)) 1))
                   ('T |k|))))))) 

(DEFUN |LSAGG-;mergeSort| (|f| |p| |n| $)
  (PROG (#0=#:G1508 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (|getShellEntry| $ 17))
                         (|getShellEntry| $ 18))
                     (SPADCALL |p| (|getShellEntry| $ 18)) |f|)
                 (LETT |p| (SPADCALL |p| (|getShellEntry| $ 56))
                       |LSAGG-;mergeSort|)))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   ('T
                    (SEQ (LETT |l|
                               (PROG1 (LETT #0# (QUOTIENT2 |n| 2)
                                       |LSAGG-;mergeSort|)
                                 (|check-subtype|
                                     (COND ((< #0# 0) 'NIL) ('T 'T))
                                     '(|NonNegativeInteger|) #0#))
                               |LSAGG-;mergeSort|)
                         (LETT |q|
                               (SPADCALL |p| |l|
                                   (|getShellEntry| $ 58))
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
  (PROG (#0=#:G1517 |p|)
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
                                      ((NOT
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
                            (SPADCALL |r| |a| (|getShellEntry| $ 62)))))
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
           (EXIT (SPADCALL |z| (|getShellEntry| $ 56))))))) 

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
                       (|getShellEntry| $ 26))
                   (SEQ G190
                        (COND
                          ((NULL (NOT (SPADCALL |y|
                                       (|getShellEntry| $ 16))))
                           (GO G191)))
                        (SEQ (LETT |z|
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 17))
                                   |LSAGG-;reverse!;2A;20|)
                             (SPADCALL |y| |x| (|getShellEntry| $ 26))
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
                          ((SPADCALL |x| (|getShellEntry| $ 70))
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
           (EXIT (SPADCALL |y| (|getShellEntry| $ 56))))))) 

(DEFUN |LSAGG-;copyInto!;2AIA;22| (|y| |x| |s| $)
  (PROG (|m| #0=#:G1546 |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |y| (|getShellEntry| $ 32))
                 |LSAGG-;copyInto!;2AIA;22|)
           (EXIT (COND
                   ((< |s| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |z|
                               (SPADCALL |y|
                                   (PROG1
                                    (LETT #0# (- |s| |m|)
                                     |LSAGG-;copyInto!;2AIA;22|)
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 38))
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
                                    (|getShellEntry| $ 72))
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
  (PROG (|m| #0=#:G1553 |k|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
                 |LSAGG-;position;SA2I;23|)
           (EXIT (COND
                   ((< |s| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |x|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- |s| |m|)
                                     |LSAGG-;position;SA2I;23|)
                                     (|check-subtype|
                                      (COND ((< #0# 0) 'NIL) ('T 'T))
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 38))
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
                                           (|getShellEntry| $ 62)))))
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
                                      (|getShellEntry| $ 32))
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
                                      (|getShellEntry| $ 76))
                                     (|getShellEntry| $ 26))
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
            (|getShellEntry| $ 75)))))) 

(DEFUN |LSAGG-;<;2AB;25| (|x| |y| $)
  (PROG (#0=#:G1567)
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
                                          (|getShellEntry| $ 62))
                                         (PROGN
                                           (LETT #0#
                                            (SPADCALL
                                             (SPADCALL |x|
                                              (|getShellEntry| $ 18))
                                             (SPADCALL |y|
                                              (|getShellEntry| $ 18))
                                             (|getShellEntry| $ 78))
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
        (LETT $ (|newShell| 81) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (|setShellEntry| $ 63
               (CONS (|dispatchFunction| |LSAGG-;reduce;MA3S;17|) $))))
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (PROGN
             (|setShellEntry| $ 74
                 (CONS (|dispatchFunction| |LSAGG-;position;SA2I;23|)
                       $))
             (|setShellEntry| $ 77
                 (CONS (|dispatchFunction|
                           |LSAGG-;removeDuplicates!;2A;24|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|OrderedSet|))
           (|setShellEntry| $ 79
               (CONS (|dispatchFunction| |LSAGG-;<;2AB;25|) $))))
        $)))) 

(MAKEPROP '|ListAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|NonNegativeInteger|) (0 . |#|) (|Boolean|)
             (|Mapping| 10 7 7) |LSAGG-;sort!;M2A;1| (5 . |empty|)
             (9 . |concat|) |LSAGG-;list;SA;2| (15 . |empty?|)
             (20 . |rest|) (25 . |first|) (|Mapping| 7 7 7)
             (30 . |reduce|) |LSAGG-;reduce;MAS;3| (37 . |copy|)
             (42 . |merge!|) |LSAGG-;merge;M3A;4| (49 . |false|)
             (53 . |setrest!|) (|Mapping| 10 7) |LSAGG-;select!;M2A;5|
             (59 . |eq?|) |LSAGG-;merge!;M3A;6| (|Integer|)
             (65 . |minIndex|) (70 . <) (76 . =) (82 . |One|)
             (86 . |One|) (90 . -) (96 . |rest|)
             |LSAGG-;insert!;SAIA;7| (102 . |concat!|)
             |LSAGG-;insert!;2AIA;8| |LSAGG-;remove!;M2A;9|
             |LSAGG-;delete!;AIA;10| (|UniversalSegment| 31)
             (108 . |lo|) (113 . |hasHi|) (118 . |hi|)
             (123 . |maxIndex|) (128 . +) |LSAGG-;delete!;AUsA;11|
             (|Union| 7 '"failed") |LSAGG-;find;MAU;12|
             (|SingleInteger|) (134 . |One|) |LSAGG-;position;MAI;13|
             (138 . |reverse!|) (143 . |quo|) (149 . |split!|)
             (155 . |true|) |LSAGG-;sorted?;MAB;15|
             |LSAGG-;reduce;MA2S;16| (159 . ~=) (165 . |reduce|)
             |LSAGG-;new;NniSA;18| |LSAGG-;map;M3A;19|
             |LSAGG-;reverse!;2A;20| (173 . |Zero|) (177 . |Zero|)
             (181 . =) (187 . |cyclic?|) |LSAGG-;copy;2A;21|
             (192 . |setfirst!|) |LSAGG-;copyInto!;2AIA;22|
             (198 . |position|) (205 . =) (211 . |remove!|)
             (217 . |removeDuplicates!|) (222 . <) (228 . <)
             (|Mapping| 7 7))
          '#(|sorted?| 234 |sort!| 240 |select!| 246 |reverse!| 252
             |removeDuplicates!| 257 |remove!| 262 |reduce| 268
             |position| 289 |new| 302 |merge!| 308 |merge| 315 |map|
             322 |list| 329 |insert!| 334 |find| 348 |delete!| 354
             |copyInto!| 366 |copy| 373 < 378)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 79
                                '(1 6 8 0 9 0 6 0 13 2 6 0 7 0 14 1 6
                                  10 0 16 1 6 0 0 17 1 6 7 0 18 3 6 7
                                  19 0 7 20 1 6 0 0 22 3 6 0 11 0 0 23
                                  0 10 0 25 2 6 0 0 0 26 2 6 10 0 0 29
                                  1 6 31 0 32 2 31 10 0 0 33 2 31 10 0
                                  0 34 0 8 0 35 0 31 0 36 2 31 0 0 0 37
                                  2 6 0 0 8 38 2 6 0 0 0 40 1 44 31 0
                                  45 1 44 10 0 46 1 44 31 0 47 1 6 31 0
                                  48 2 31 0 0 0 49 0 53 0 54 1 6 0 0 56
                                  2 31 0 0 0 57 2 6 0 0 31 58 0 10 0 59
                                  2 7 10 0 0 62 4 0 7 19 0 7 7 63 0 53
                                  0 67 0 8 0 68 2 8 10 0 0 69 1 6 10 0
                                  70 2 6 7 0 7 72 3 0 31 7 0 31 74 2 7
                                  10 0 0 75 2 6 0 27 0 76 1 0 0 0 77 2
                                  7 10 0 0 78 2 0 10 0 0 79 2 0 10 11 0
                                  60 2 0 0 11 0 12 2 0 0 27 0 28 1 0 0
                                  0 66 1 0 0 0 77 2 0 0 27 0 42 3 0 7
                                  19 0 7 61 4 0 7 19 0 7 7 63 2 0 7 19
                                  0 21 2 0 31 27 0 55 3 0 31 7 0 31 74
                                  2 0 0 8 7 64 3 0 0 11 0 0 30 3 0 0 11
                                  0 0 24 3 0 0 19 0 0 65 1 0 0 7 15 3 0
                                  0 7 0 31 39 3 0 0 0 0 31 41 2 0 51 27
                                  0 52 2 0 0 0 44 50 2 0 0 0 31 43 3 0
                                  0 0 0 31 73 1 0 0 0 71 2 0 10 0 0 79)))))
          '|lookupComplete|)) 
