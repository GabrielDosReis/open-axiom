
(/VERSIONCHECK 2) 

(DEFUN |LSAGG-;sort!;M2A;1| (|f| |l| $)
  (|LSAGG-;mergeSort| |f| |l| (SPADCALL |l| (QREFELT $ 9)) $)) 

(DEFUN |LSAGG-;list;SA;2| (|x| $)
  (SPADCALL |x| (SPADCALL (QREFELT $ 12)) (QREFELT $ 13))) 

(DEFUN |LSAGG-;reduce;MAS;3| (|f| |x| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 16))
     (|error| "reducing over an empty list needs the 3 argument form"))
    ('T
     (SPADCALL |f| (SPADCALL |x| (QREFELT $ 17))
         (SPADCALL |x| (QREFELT $ 18)) (QREFELT $ 20))))) 

(DEFUN |LSAGG-;merge;M3A;4| (|f| |p| |q| $)
  (SPADCALL |f| (SPADCALL |p| (QREFELT $ 22))
      (SPADCALL |q| (QREFELT $ 22)) (QREFELT $ 23))) 

(DEFUN |LSAGG-;select!;M2A;5| (|f| |x| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (QREFELT $ 16)) 'NIL)
                           ('T
                            (SPADCALL
                                (SPADCALL (SPADCALL |x| (QREFELT $ 18))
                                    |f|)
                                (QREFELT $ 25)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                                 |LSAGG-;select!;M2A;5|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (QREFELT $ 16)) |x|)
                   ('T
                    (SEQ (LETT |y| |x| |LSAGG-;select!;M2A;5|)
                         (LETT |z| (SPADCALL |y| (QREFELT $ 17))
                               |LSAGG-;select!;M2A;5|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL
                                        (SPADCALL |z| (QREFELT $ 16))
                                        (QREFELT $ 25)))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL |z| (QREFELT $ 18))
                                        |f|)
                                       (SEQ
                                        (LETT |y| |z|
                                         |LSAGG-;select!;M2A;5|)
                                        (EXIT
                                         (LETT |z|
                                          (SPADCALL |z| (QREFELT $ 17))
                                          |LSAGG-;select!;M2A;5|))))
                                      ('T
                                       (SEQ
                                        (LETT |z|
                                         (SPADCALL |z| (QREFELT $ 17))
                                         |LSAGG-;select!;M2A;5|)
                                        (EXIT
                                         (SPADCALL |y| |z|
                                          (QREFELT $ 26))))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;merge!;M3A;6| (|f| |p| |q| $)
  (PROG (|r| |t|)
    (RETURN
      (SEQ (COND
             ((SPADCALL |p| (QREFELT $ 16)) |q|)
             ((SPADCALL |q| (QREFELT $ 16)) |p|)
             ((SPADCALL |p| |q| (QREFELT $ 29))
              (|error| "cannot merge a list into itself"))
             ('T
              (SEQ (COND
                     ((SPADCALL (SPADCALL |p| (QREFELT $ 18))
                          (SPADCALL |q| (QREFELT $ 18)) |f|)
                      (SEQ (LETT |r|
                                 (LETT |t| |p| |LSAGG-;merge!;M3A;6|)
                                 |LSAGG-;merge!;M3A;6|)
                           (EXIT (LETT |p|
                                       (SPADCALL |p| (QREFELT $ 17))
                                       |LSAGG-;merge!;M3A;6|))))
                     ('T
                      (SEQ (LETT |r|
                                 (LETT |t| |q| |LSAGG-;merge!;M3A;6|)
                                 |LSAGG-;merge!;M3A;6|)
                           (EXIT (LETT |q|
                                       (SPADCALL |q| (QREFELT $ 17))
                                       |LSAGG-;merge!;M3A;6|)))))
                   (SEQ G190
                        (COND
                          ((NULL (COND
                                   ((SPADCALL |p| (QREFELT $ 16)) 'NIL)
                                   ('T
                                    (SPADCALL
                                     (SPADCALL |q| (QREFELT $ 16))
                                     (QREFELT $ 25)))))
                           (GO G191)))
                        (SEQ (EXIT (COND
                                     ((SPADCALL
                                       (SPADCALL |p| (QREFELT $ 18))
                                       (SPADCALL |q| (QREFELT $ 18))
                                       |f|)
                                      (SEQ
                                       (SPADCALL |t| |p|
                                        (QREFELT $ 26))
                                       (LETT |t| |p|
                                        |LSAGG-;merge!;M3A;6|)
                                       (EXIT
                                        (LETT |p|
                                         (SPADCALL |p| (QREFELT $ 17))
                                         |LSAGG-;merge!;M3A;6|))))
                                     ('T
                                      (SEQ
                                       (SPADCALL |t| |q|
                                        (QREFELT $ 26))
                                       (LETT |t| |q|
                                        |LSAGG-;merge!;M3A;6|)
                                       (EXIT
                                        (LETT |q|
                                         (SPADCALL |q| (QREFELT $ 17))
                                         |LSAGG-;merge!;M3A;6|)))))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (SPADCALL |t|
                       (COND
                         ((SPADCALL |p| (QREFELT $ 16)) |q|)
                         ('T |p|))
                       (QREFELT $ 26))
                   (EXIT |r|)))))))) 

(DEFUN |LSAGG-;insert!;SAIA;7| (|s| |x| |i| $)
  (PROG (|m| #0=#:G1464 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (QREFELT $ 32))
                 |LSAGG-;insert!;SAIA;7|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|) (SPADCALL |s| |x| (QREFELT $ 13)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;insert!;SAIA;7|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (QREFELT $ 33))
                               |LSAGG-;insert!;SAIA;7|)
                         (LETT |z| (SPADCALL |y| (QREFELT $ 17))
                               |LSAGG-;insert!;SAIA;7|)
                         (SPADCALL |y|
                                   (SPADCALL |s| |z| (QREFELT $ 13))
                                   (QREFELT $ 26))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;insert!;2AIA;8| (|w| |x| |i| $)
  (PROG (|m| #0=#:G1468 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (QREFELT $ 32))
                 |LSAGG-;insert!;2AIA;8|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|) (SPADCALL |w| |x| (QREFELT $ 35)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;insert!;2AIA;8|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (QREFELT $ 33))
                               |LSAGG-;insert!;2AIA;8|)
                         (LETT |z| (SPADCALL |y| (QREFELT $ 17))
                               |LSAGG-;insert!;2AIA;8|)
                         (SPADCALL |y| |w| (QREFELT $ 26))
                         (SPADCALL |y| |z| (QREFELT $ 35)) (EXIT |x|))))))))) 

(DEFUN |LSAGG-;remove!;M2A;9| (|f| |x| $)
  (PROG (|p| |q|)
    (RETURN
      (SEQ (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (QREFELT $ 16)) 'NIL)
                           ('T
                            (SPADCALL (SPADCALL |x| (QREFELT $ 18))
                                |f|))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                                 |LSAGG-;remove!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (QREFELT $ 16)) |x|)
                   ('T
                    (SEQ (LETT |p| |x| |LSAGG-;remove!;M2A;9|)
                         (LETT |q| (SPADCALL |x| (QREFELT $ 17))
                               |LSAGG-;remove!;M2A;9|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL
                                        (SPADCALL |q| (QREFELT $ 16))
                                        (QREFELT $ 25)))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((SPADCALL
                                        (SPADCALL |q| (QREFELT $ 18))
                                        |f|)
                                       (LETT |q|
                                        (SPADCALL |p|
                                         (SPADCALL |q| (QREFELT $ 17))
                                         (QREFELT $ 26))
                                        |LSAGG-;remove!;M2A;9|))
                                      ('T
                                       (SEQ
                                        (LETT |p| |q|
                                         |LSAGG-;remove!;M2A;9|)
                                        (EXIT
                                         (LETT |q|
                                          (SPADCALL |q| (QREFELT $ 17))
                                          |LSAGG-;remove!;M2A;9|)))))))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AIA;10| (|x| |i| $)
  (PROG (|m| #0=#:G1480 |y|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (QREFELT $ 32))
                 |LSAGG-;delete!;AIA;10|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|) (SPADCALL |x| (QREFELT $ 17)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;delete!;AIA;10|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (QREFELT $ 33))
                               |LSAGG-;delete!;AIA;10|)
                         (SPADCALL |y| (SPADCALL |y| 2 (QREFELT $ 33))
                             (QREFELT $ 26))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AUsA;11| (|x| |i| $)
  (PROG (|l| |m| |h| #0=#:G1485 #1=#:G1486 |t| #2=#:G1487)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |i| (QREFELT $ 40))
                 |LSAGG-;delete!;AUsA;11|)
           (LETT |m| (SPADCALL |x| (QREFELT $ 32))
                 |LSAGG-;delete!;AUsA;11|)
           (EXIT (COND
                   ((< |l| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (QREFELT $ 41))
                                  (SPADCALL |i| (QREFELT $ 42)))
                                 ('T (SPADCALL |x| (QREFELT $ 43))))
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
                                      (QREFELT $ 33)))
                                 ('T
                                  (SEQ (LETT |t|
                                        (SPADCALL |x|
                                         (PROG1
                                          (LETT #1# (- (- |l| 1) |m|)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (QREFELT $ 33))
                                        |LSAGG-;delete!;AUsA;11|)
                                       (SPADCALL |t|
                                        (SPADCALL |t|
                                         (PROG1
                                          (LETT #2# (+ (- |h| |l|) 2)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype| (>= #2# 0)
                                            '(|NonNegativeInteger|)
                                            #2#))
                                         (QREFELT $ 33))
                                        (QREFELT $ 26))
                                       (EXIT |x|))))))))))))) 

(DEFUN |LSAGG-;find;MAU;12| (|f| |x| $)
  (SEQ (SEQ G190
            (COND
              ((NULL (COND
                       ((SPADCALL |x| (QREFELT $ 16)) 'NIL)
                       ('T
                        (SPADCALL
                            (SPADCALL (SPADCALL |x| (QREFELT $ 18))
                                |f|)
                            (QREFELT $ 25)))))
               (GO G191)))
            (SEQ (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                             |LSAGG-;find;MAU;12|)))
            NIL (GO G190) G191 (EXIT NIL))
       (EXIT (COND
               ((SPADCALL |x| (QREFELT $ 16)) (CONS 1 "failed"))
               ('T (CONS 0 (SPADCALL |x| (QREFELT $ 18)))))))) 

(DEFUN |LSAGG-;position;MAI;13| (|f| |x| $)
  (PROG (|k|)
    (RETURN
      (SEQ (SEQ (LETT |k| (SPADCALL |x| (QREFELT $ 32))
                      |LSAGG-;position;MAI;13|)
                G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (QREFELT $ 16)) 'NIL)
                           ('T
                            (SPADCALL
                                (SPADCALL (SPADCALL |x| (QREFELT $ 18))
                                    |f|)
                                (QREFELT $ 25)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                                 |LSAGG-;position;MAI;13|)))
                (LETT |k| (+ |k| 1) |LSAGG-;position;MAI;13|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (COND
                   ((SPADCALL |x| (QREFELT $ 16))
                    (- (SPADCALL |x| (QREFELT $ 32)) 1))
                   ('T |k|))))))) 

(DEFUN |LSAGG-;mergeSort| (|f| |p| |n| $)
  (PROG (#0=#:G1507 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (QREFELT $ 17))
                         (QREFELT $ 18))
                     (SPADCALL |p| (QREFELT $ 18)) |f|)
                 (LETT |p| (SPADCALL |p| (QREFELT $ 48))
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
                         (LETT |q| (SPADCALL |p| |l| (QREFELT $ 49))
                               |LSAGG-;mergeSort|)
                         (LETT |p| (|LSAGG-;mergeSort| |f| |p| |l| $)
                               |LSAGG-;mergeSort|)
                         (LETT |q|
                               (|LSAGG-;mergeSort| |f| |q| (- |n| |l|)
                                   $)
                               |LSAGG-;mergeSort|)
                         (EXIT (SPADCALL |f| |p| |q| (QREFELT $ 23))))))))))) 

(DEFUN |LSAGG-;sorted?;MAB;15| (|f| |l| $)
  (PROG (#0=#:G1516 |p|)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |l| (QREFELT $ 16)) 'T)
                   ('T
                    (SEQ (LETT |p| (SPADCALL |l| (QREFELT $ 17))
                               |LSAGG-;sorted?;MAB;15|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL
                                        (SPADCALL |p| (QREFELT $ 16))
                                        (QREFELT $ 25)))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (COND
                                      ((NULL
                                        (SPADCALL
                                         (SPADCALL |l| (QREFELT $ 18))
                                         (SPADCALL |p| (QREFELT $ 18))
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
                                         (QREFELT $ 17))
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
                  ((NULL (SPADCALL (SPADCALL |x| (QREFELT $ 16))
                             (QREFELT $ 25)))
                   (GO G191)))
                (SEQ (LETT |r|
                           (SPADCALL |r| (SPADCALL |x| (QREFELT $ 18))
                               |f|)
                           |LSAGG-;reduce;MA2S;16|)
                     (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
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
                           ((SPADCALL |x| (QREFELT $ 16)) 'NIL)
                           ('T
                            (SPADCALL (SPADCALL |r| |a| (QREFELT $ 52))
                                (QREFELT $ 25)))))
                   (GO G191)))
                (SEQ (LETT |r|
                           (SPADCALL |r| (SPADCALL |x| (QREFELT $ 18))
                               |f|)
                           |LSAGG-;reduce;MA3S;17|)
                     (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                                 |LSAGG-;reduce;MA3S;17|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |r|))))) 

(DEFUN |LSAGG-;new;NniSA;18| (|n| |s| $)
  (PROG (|k| |l|)
    (RETURN
      (SEQ (LETT |l| (SPADCALL (QREFELT $ 12)) |LSAGG-;new;NniSA;18|)
           (SEQ (LETT |k| 1 |LSAGG-;new;NniSA;18|) G190
                (COND ((QSGREATERP |k| |n|) (GO G191)))
                (SEQ (EXIT (LETT |l| (SPADCALL |s| |l| (QREFELT $ 13))
                                 |LSAGG-;new;NniSA;18|)))
                (LETT |k| (QSADD1 |k|) |LSAGG-;new;NniSA;18|) (GO G190)
                G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |LSAGG-;map;M3A;19| (|f| |x| |y| $)
  (PROG (|z|)
    (RETURN
      (SEQ (LETT |z| (SPADCALL (QREFELT $ 12)) |LSAGG-;map;M3A;19|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((SPADCALL |x| (QREFELT $ 16)) 'NIL)
                           ('T
                            (SPADCALL (SPADCALL |y| (QREFELT $ 16))
                                (QREFELT $ 25)))))
                   (GO G191)))
                (SEQ (LETT |z|
                           (SPADCALL
                               (SPADCALL (SPADCALL |x| (QREFELT $ 18))
                                   (SPADCALL |y| (QREFELT $ 18)) |f|)
                               |z| (QREFELT $ 13))
                           |LSAGG-;map;M3A;19|)
                     (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                           |LSAGG-;map;M3A;19|)
                     (EXIT (LETT |y| (SPADCALL |y| (QREFELT $ 17))
                                 |LSAGG-;map;M3A;19|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL |z| (QREFELT $ 48))))))) 

(DEFUN |LSAGG-;reverse!;2A;20| (|x| $)
  (PROG (|z| |y|)
    (RETURN
      (SEQ (COND
             ((OR (SPADCALL |x| (QREFELT $ 16))
                  (SPADCALL
                      (LETT |y| (SPADCALL |x| (QREFELT $ 17))
                            |LSAGG-;reverse!;2A;20|)
                      (QREFELT $ 16)))
              |x|)
             ('T
              (SEQ (SPADCALL |x| (SPADCALL (QREFELT $ 12))
                       (QREFELT $ 26))
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL
                                     (SPADCALL |y| (QREFELT $ 16))
                                     (QREFELT $ 25)))
                           (GO G191)))
                        (SEQ (LETT |z| (SPADCALL |y| (QREFELT $ 17))
                                   |LSAGG-;reverse!;2A;20|)
                             (SPADCALL |y| |x| (QREFELT $ 26))
                             (LETT |x| |y| |LSAGG-;reverse!;2A;20|)
                             (EXIT (LETT |y| |z|
                                    |LSAGG-;reverse!;2A;20|)))
                        NIL (GO G190) G191 (EXIT NIL))
                   (EXIT |x|)))))))) 

(DEFUN |LSAGG-;copy;2A;21| (|x| $)
  (PROG (|k| |y|)
    (RETURN
      (SEQ (LETT |y| (SPADCALL (QREFELT $ 12)) |LSAGG-;copy;2A;21|)
           (SEQ (LETT |k| 0 |LSAGG-;copy;2A;21|) G190
                (COND
                  ((NULL (SPADCALL (SPADCALL |x| (QREFELT $ 16))
                             (QREFELT $ 25)))
                   (GO G191)))
                (SEQ (COND
                       ((EQL |k| 1000)
                        (COND
                          ((SPADCALL |x| (QREFELT $ 57))
                           (EXIT (|error| "cyclic list"))))))
                     (LETT |y|
                           (SPADCALL (SPADCALL |x| (QREFELT $ 18)) |y|
                               (QREFELT $ 13))
                           |LSAGG-;copy;2A;21|)
                     (EXIT (LETT |x| (SPADCALL |x| (QREFELT $ 17))
                                 |LSAGG-;copy;2A;21|)))
                (LETT |k| (QSADD1 |k|) |LSAGG-;copy;2A;21|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (SPADCALL |y| (QREFELT $ 48))))))) 

(DEFUN |LSAGG-;copyInto!;2AIA;22| (|y| |x| |s| $)
  (PROG (|m| #0=#:G1545 |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |y| (QREFELT $ 32))
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
                                   (QREFELT $ 33))
                               |LSAGG-;copyInto!;2AIA;22|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |z| (QREFELT $ 16))
                                          'NIL)
                                         ('T
                                          (SPADCALL
                                           (SPADCALL |x|
                                            (QREFELT $ 16))
                                           (QREFELT $ 25)))))
                                 (GO G191)))
                              (SEQ (SPADCALL |z|
                                    (SPADCALL |x| (QREFELT $ 18))
                                    (QREFELT $ 59))
                                   (LETT |x|
                                    (SPADCALL |x| (QREFELT $ 17))
                                    |LSAGG-;copyInto!;2AIA;22|)
                                   (EXIT
                                    (LETT |z|
                                     (SPADCALL |z| (QREFELT $ 17))
                                     |LSAGG-;copyInto!;2AIA;22|)))
                              NIL (GO G190) G191 (EXIT NIL))
                         (EXIT |y|))))))))) 

(DEFUN |LSAGG-;position;SA2I;23| (|w| |x| |s| $)
  (PROG (|m| #0=#:G1552 |k|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (QREFELT $ 32))
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
                                   (QREFELT $ 33))
                               |LSAGG-;position;SA2I;23|)
                         (SEQ (LETT |k| |s| |LSAGG-;position;SA2I;23|)
                              G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |x| (QREFELT $ 16))
                                          'NIL)
                                         ('T
                                          (SPADCALL
                                           (SPADCALL |w|
                                            (SPADCALL |x|
                                             (QREFELT $ 18))
                                            (QREFELT $ 52))
                                           (QREFELT $ 25)))))
                                 (GO G191)))
                              (SEQ (EXIT
                                    (LETT |x|
                                     (SPADCALL |x| (QREFELT $ 17))
                                     |LSAGG-;position;SA2I;23|)))
                              (LETT |k| (+ |k| 1)
                                    |LSAGG-;position;SA2I;23|)
                              (GO G190) G191 (EXIT NIL))
                         (EXIT (COND
                                 ((SPADCALL |x| (QREFELT $ 16))
                                  (- (SPADCALL |x| (QREFELT $ 32)) 1))
                                 ('T |k|))))))))))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24| (|l| $)
  (PROG (|p|)
    (RETURN
      (SEQ (LETT |p| |l| |LSAGG-;removeDuplicates!;2A;24|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (SPADCALL |p| (QREFELT $ 16))
                             (QREFELT $ 25)))
                   (GO G191)))
                (SEQ (EXIT (LETT |p|
                                 (SPADCALL |p|
                                     (SPADCALL
                                      (CONS
                                       #'|LSAGG-;removeDuplicates!;2A;24!0|
                                       (VECTOR $ |p|))
                                      (SPADCALL |p| (QREFELT $ 17))
                                      (QREFELT $ 62))
                                     (QREFELT $ 26))
                                 |LSAGG-;removeDuplicates!;2A;24|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |l|))))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24!0| (|#1| $$)
  (PROG ($)
    (LETT $ (QREFELT $$ 0) |LSAGG-;removeDuplicates!;2A;24|)
    (RETURN
      (PROGN
        (SPADCALL |#1| (SPADCALL (QREFELT $$ 1) (QREFELT $ 18))
            (QREFELT $ 52)))))) 

(DEFUN |LSAGG-;<;2AB;25| (|x| |y| $)
  (PROG (#0=#:G1566)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND
                             ((NULL (COND
                                      ((SPADCALL |x| (QREFELT $ 16))
                                       'NIL)
                                      ('T
                                       (SPADCALL
                                        (SPADCALL |y| (QREFELT $ 16))
                                        (QREFELT $ 25)))))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((NULL
                                          (SPADCALL
                                           (SPADCALL |x|
                                            (QREFELT $ 18))
                                           (SPADCALL |y|
                                            (QREFELT $ 18))
                                           (QREFELT $ 52)))
                                         (PROGN
                                           (LETT #0#
                                            (SPADCALL
                                             (SPADCALL |x|
                                              (QREFELT $ 18))
                                             (SPADCALL |y|
                                              (QREFELT $ 18))
                                             (QREFELT $ 64))
                                            |LSAGG-;<;2AB;25|)
                                           (GO #0#)))
                                        ('T
                                         (SEQ
                                          (LETT |x|
                                           (SPADCALL |x|
                                            (QREFELT $ 17))
                                           |LSAGG-;<;2AB;25|)
                                          (EXIT
                                           (LETT |y|
                                            (SPADCALL |y|
                                             (QREFELT $ 17))
                                            |LSAGG-;<;2AB;25|)))))))
                           NIL (GO G190) G191 (EXIT NIL))
                      (EXIT (COND
                              ((SPADCALL |x| (QREFELT $ 16))
                               (SPADCALL (SPADCALL |y| (QREFELT $ 16))
                                   (QREFELT $ 25)))
                              ('T 'NIL)))))
           #0# (EXIT #0#))))) 

(DEFUN |ListAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|ListAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|ListAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (GETREFV 67) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (QSETREFV $ 7 |#2|)
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (QSETREFV $ 53
               (CONS (|dispatchFunction| |LSAGG-;reduce;MA3S;17|) $))))
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (PROGN
             (QSETREFV $ 61
                 (CONS (|dispatchFunction| |LSAGG-;position;SA2I;23|)
                       $))
             (QSETREFV $ 63
                 (CONS (|dispatchFunction|
                           |LSAGG-;removeDuplicates!;2A;24|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|OrderedSet|))
           (QSETREFV $ 65
               (CONS (|dispatchFunction| |LSAGG-;<;2AB;25|) $))))
        $)))) 

(MAKEPROP '|ListAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|NonNegativeInteger|) (0 . |#|) (|Mapping| 15 7 7)
             |LSAGG-;sort!;M2A;1| (5 . |empty|) (9 . |concat|)
             |LSAGG-;list;SA;2| (|Boolean|) (15 . |empty?|)
             (20 . |rest|) (25 . |first|) (|Mapping| 7 7 7)
             (30 . |reduce|) |LSAGG-;reduce;MAS;3| (37 . |copy|)
             (42 . |merge!|) |LSAGG-;merge;M3A;4| (49 . |not|)
             (54 . |setrest!|) (|Mapping| 15 7) |LSAGG-;select!;M2A;5|
             (60 . |eq?|) |LSAGG-;merge!;M3A;6| (|Integer|)
             (66 . |minIndex|) (71 . |rest|) |LSAGG-;insert!;SAIA;7|
             (77 . |concat!|) |LSAGG-;insert!;2AIA;8|
             |LSAGG-;remove!;M2A;9| |LSAGG-;delete!;AIA;10|
             (|UniversalSegment| 31) (83 . |lo|) (88 . |hasHi|)
             (93 . |hi|) (98 . |maxIndex|) |LSAGG-;delete!;AUsA;11|
             (|Union| 7 '"failed") |LSAGG-;find;MAU;12|
             |LSAGG-;position;MAI;13| (103 . |reverse!|)
             (108 . |split!|) |LSAGG-;sorted?;MAB;15|
             |LSAGG-;reduce;MA2S;16| (114 . =) (120 . |reduce|)
             |LSAGG-;new;NniSA;18| |LSAGG-;map;M3A;19|
             |LSAGG-;reverse!;2A;20| (128 . |cyclic?|)
             |LSAGG-;copy;2A;21| (133 . |setfirst!|)
             |LSAGG-;copyInto!;2AIA;22| (139 . |position|)
             (146 . |remove!|) (152 . |removeDuplicates!|) (157 . <)
             (163 . <) (|Mapping| 7 7))
          '#(|sorted?| 169 |sort!| 175 |select!| 181 |reverse!| 187
             |removeDuplicates!| 192 |remove!| 197 |reduce| 203
             |position| 224 |new| 237 |merge!| 243 |merge| 250 |map|
             257 |list| 264 |insert!| 269 |find| 283 |delete!| 289
             |copyInto!| 301 |copy| 308 < 313)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 65
                                '(1 6 8 0 9 0 6 0 12 2 6 0 7 0 13 1 6
                                  15 0 16 1 6 0 0 17 1 6 7 0 18 3 6 7
                                  19 0 7 20 1 6 0 0 22 3 6 0 10 0 0 23
                                  1 15 0 0 25 2 6 0 0 0 26 2 6 15 0 0
                                  29 1 6 31 0 32 2 6 0 0 8 33 2 6 0 0 0
                                  35 1 39 31 0 40 1 39 15 0 41 1 39 31
                                  0 42 1 6 31 0 43 1 6 0 0 48 2 6 0 0
                                  31 49 2 7 15 0 0 52 4 0 7 19 0 7 7 53
                                  1 6 15 0 57 2 6 7 0 7 59 3 0 31 7 0
                                  31 61 2 6 0 27 0 62 1 0 0 0 63 2 7 15
                                  0 0 64 2 0 15 0 0 65 2 0 15 10 0 50 2
                                  0 0 10 0 11 2 0 0 27 0 28 1 0 0 0 56
                                  1 0 0 0 63 2 0 0 27 0 37 3 0 7 19 0 7
                                  51 4 0 7 19 0 7 7 53 2 0 7 19 0 21 2
                                  0 31 27 0 47 3 0 31 7 0 31 61 2 0 0 8
                                  7 54 3 0 0 10 0 0 30 3 0 0 10 0 0 24
                                  3 0 0 19 0 0 55 1 0 0 7 14 3 0 0 7 0
                                  31 34 3 0 0 0 0 31 36 2 0 45 27 0 46
                                  2 0 0 0 39 44 2 0 0 0 31 38 3 0 0 0 0
                                  31 60 1 0 0 0 58 2 0 15 0 0 65)))))
          '|lookupComplete|)) 
