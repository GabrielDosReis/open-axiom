
(/VERSIONCHECK 2) 

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
                            (SPADCALL
                                (SPADCALL
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 18))
                                    |f|)
                                (|getShellEntry| $ 25)))))
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
                                ((NULL (SPADCALL
                                        (SPADCALL |z|
                                         (|getShellEntry| $ 16))
                                        (|getShellEntry| $ 25)))
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
                                    (SPADCALL
                                     (SPADCALL |q|
                                      (|getShellEntry| $ 16))
                                     (|getShellEntry| $ 25)))))
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
  (PROG (|m| #0=#:G1461 |y| |z|)
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
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 33))
                               |LSAGG-;insert!;SAIA;7|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;SAIA;7|)
                         (SPADCALL |y|
                             (SPADCALL |s| |z| (|getShellEntry| $ 14))
                             (|getShellEntry| $ 26))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;insert!;2AIA;8| (|w| |x| |i| $)
  (PROG (|m| #0=#:G1465 |y| |z|)
    (RETURN
      (SEQ (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
                 |LSAGG-;insert!;2AIA;8|)
           (EXIT (COND
                   ((< |i| |m|) (|error| "index out of range"))
                   ((EQL |i| |m|)
                    (SPADCALL |w| |x| (|getShellEntry| $ 35)))
                   ('T
                    (SEQ (LETT |y|
                               (SPADCALL |x|
                                   (PROG1
                                    (LETT #0# (- (- |i| 1) |m|)
                                     |LSAGG-;insert!;2AIA;8|)
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 33))
                               |LSAGG-;insert!;2AIA;8|)
                         (LETT |z|
                               (SPADCALL |y| (|getShellEntry| $ 17))
                               |LSAGG-;insert!;2AIA;8|)
                         (SPADCALL |y| |w| (|getShellEntry| $ 26))
                         (SPADCALL |y| |z| (|getShellEntry| $ 35))
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
                                ((NULL (SPADCALL
                                        (SPADCALL |q|
                                         (|getShellEntry| $ 16))
                                        (|getShellEntry| $ 25)))
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
  (PROG (|m| #0=#:G1477 |y|)
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
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 33))
                               |LSAGG-;delete!;AIA;10|)
                         (SPADCALL |y|
                             (SPADCALL |y| 2 (|getShellEntry| $ 33))
                             (|getShellEntry| $ 26))
                         (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AUsA;11| (|x| |i| $)
  (PROG (|l| |m| |h| #0=#:G1482 #1=#:G1483 |t| #2=#:G1484)
    (RETURN
      (SEQ (LETT |l| (SPADCALL |i| (|getShellEntry| $ 40))
                 |LSAGG-;delete!;AUsA;11|)
           (LETT |m| (SPADCALL |x| (|getShellEntry| $ 32))
                 |LSAGG-;delete!;AUsA;11|)
           (EXIT (COND
                   ((< |l| |m|) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 41))
                                  (SPADCALL |i| (|getShellEntry| $ 42)))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 43))))
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
                                      (|getShellEntry| $ 33)))
                                 ('T
                                  (SEQ (LETT |t|
                                        (SPADCALL |x|
                                         (PROG1
                                          (LETT #1# (- (- |l| 1) |m|)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 33))
                                        |LSAGG-;delete!;AUsA;11|)
                                       (SPADCALL |t|
                                        (SPADCALL |t|
                                         (PROG1
                                          (LETT #2# (+ (- |h| |l|) 2)
                                           |LSAGG-;delete!;AUsA;11|)
                                           (|check-subtype| (>= #2# 0)
                                            '(|NonNegativeInteger|)
                                            #2#))
                                         (|getShellEntry| $ 33))
                                        (|getShellEntry| $ 26))
                                       (EXIT |x|))))))))))))) 

(DEFUN |LSAGG-;find;MAU;12| (|f| |x| $)
  (SEQ (SEQ G190
            (COND
              ((NULL (COND
                       ((SPADCALL |x| (|getShellEntry| $ 16)) 'NIL)
                       ('T
                        (SPADCALL
                            (SPADCALL
                                (SPADCALL |x| (|getShellEntry| $ 18))
                                |f|)
                            (|getShellEntry| $ 25)))))
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
                            (SPADCALL
                                (SPADCALL
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 18))
                                    |f|)
                                (|getShellEntry| $ 25)))))
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
  (PROG (#0=#:G1504 |l| |q|)
    (RETURN
      (SEQ (COND
             ((EQL |n| 2)
              (COND
                ((SPADCALL
                     (SPADCALL (SPADCALL |p| (|getShellEntry| $ 17))
                         (|getShellEntry| $ 18))
                     (SPADCALL |p| (|getShellEntry| $ 18)) |f|)
                 (LETT |p| (SPADCALL |p| (|getShellEntry| $ 48))
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
                                   (|getShellEntry| $ 49))
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
  (PROG (#0=#:G1513 |p|)
    (RETURN
      (SEQ (EXIT (COND
                   ((SPADCALL |l| (|getShellEntry| $ 16)) 'T)
                   ('T
                    (SEQ (LETT |p|
                               (SPADCALL |l| (|getShellEntry| $ 17))
                               |LSAGG-;sorted?;MAB;15|)
                         (SEQ G190
                              (COND
                                ((NULL (SPADCALL
                                        (SPADCALL |p|
                                         (|getShellEntry| $ 16))
                                        (|getShellEntry| $ 25)))
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
                  ((NULL (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 16))
                             (|getShellEntry| $ 25)))
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
                            (SPADCALL |r| |a| (|getShellEntry| $ 52)))))
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
                            (SPADCALL
                                (SPADCALL |y| (|getShellEntry| $ 16))
                                (|getShellEntry| $ 25)))))
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
           (EXIT (SPADCALL |z| (|getShellEntry| $ 48))))))) 

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
                          ((NULL (SPADCALL
                                     (SPADCALL |y|
                                      (|getShellEntry| $ 16))
                                     (|getShellEntry| $ 25)))
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
                  ((NULL (SPADCALL
                             (SPADCALL |x| (|getShellEntry| $ 16))
                             (|getShellEntry| $ 25)))
                   (GO G191)))
                (SEQ (COND
                       ((EQL |k| 1000)
                        (COND
                          ((SPADCALL |x| (|getShellEntry| $ 57))
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
           (EXIT (SPADCALL |y| (|getShellEntry| $ 48))))))) 

(DEFUN |LSAGG-;copyInto!;2AIA;22| (|y| |x| |s| $)
  (PROG (|m| #0=#:G1542 |z|)
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
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 33))
                               |LSAGG-;copyInto!;2AIA;22|)
                         (SEQ G190
                              (COND
                                ((NULL (COND
                                         ((SPADCALL |z|
                                           (|getShellEntry| $ 16))
                                          'NIL)
                                         ('T
                                          (SPADCALL
                                           (SPADCALL |x|
                                            (|getShellEntry| $ 16))
                                           (|getShellEntry| $ 25)))))
                                 (GO G191)))
                              (SEQ (SPADCALL |z|
                                    (SPADCALL |x|
                                     (|getShellEntry| $ 18))
                                    (|getShellEntry| $ 59))
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
  (PROG (|m| #0=#:G1549 |k|)
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
                                     (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                   (|getShellEntry| $ 33))
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
                                           (|getShellEntry| $ 52)))))
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
                  ((NULL (SPADCALL
                             (SPADCALL |p| (|getShellEntry| $ 16))
                             (|getShellEntry| $ 25)))
                   (GO G191)))
                (SEQ (EXIT (LETT |p|
                                 (SPADCALL |p|
                                     (SPADCALL
                                      (CONS
                                       #'|LSAGG-;removeDuplicates!;2A;24!0|
                                       (VECTOR $ |p|))
                                      (SPADCALL |p|
                                       (|getShellEntry| $ 17))
                                      (|getShellEntry| $ 63))
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
            (|getShellEntry| $ 62)))))) 

(DEFUN |LSAGG-;<;2AB;25| (|x| |y| $)
  (PROG (#0=#:G1563)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ G190
                           (COND
                             ((NULL (COND
                                      ((SPADCALL |x|
                                        (|getShellEntry| $ 16))
                                       'NIL)
                                      ('T
                                       (SPADCALL
                                        (SPADCALL |y|
                                         (|getShellEntry| $ 16))
                                        (|getShellEntry| $ 25)))))
                              (GO G191)))
                           (SEQ (EXIT (COND
                                        ((SPADCALL
                                          (SPADCALL |x|
                                           (|getShellEntry| $ 18))
                                          (SPADCALL |y|
                                           (|getShellEntry| $ 18))
                                          (|getShellEntry| $ 52))
                                         (PROGN
                                           (LETT #0#
                                            (SPADCALL
                                             (SPADCALL |x|
                                              (|getShellEntry| $ 18))
                                             (SPADCALL |y|
                                              (|getShellEntry| $ 18))
                                             (|getShellEntry| $ 65))
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
                               (SPADCALL
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 16))
                                   (|getShellEntry| $ 25)))
                              ('T 'NIL)))))
           #0# (EXIT #0#))))) 

(DEFUN |ListAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|ListAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|ListAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 68) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (|setShellEntry| $ 53
               (CONS (|dispatchFunction| |LSAGG-;reduce;MA3S;17|) $))))
        (COND
          ((|HasCategory| |#2| '(|SetCategory|))
           (PROGN
             (|setShellEntry| $ 61
                 (CONS (|dispatchFunction| |LSAGG-;position;SA2I;23|)
                       $))
             (|setShellEntry| $ 64
                 (CONS (|dispatchFunction|
                           |LSAGG-;removeDuplicates!;2A;24|)
                       $)))))
        (COND
          ((|HasCategory| |#2| '(|OrderedSet|))
           (|setShellEntry| $ 66
               (CONS (|dispatchFunction| |LSAGG-;<;2AB;25|) $))))
        $)))) 

(MAKEPROP '|ListAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|NonNegativeInteger|) (0 . |#|) (|Boolean|)
             (|Mapping| 10 7 7) |LSAGG-;sort!;M2A;1| (5 . |empty|)
             (9 . |concat|) |LSAGG-;list;SA;2| (15 . |empty?|)
             (20 . |rest|) (25 . |first|) (|Mapping| 7 7 7)
             (30 . |reduce|) |LSAGG-;reduce;MAS;3| (37 . |copy|)
             (42 . |merge!|) |LSAGG-;merge;M3A;4| (49 . |not|)
             (54 . |setrest!|) (|Mapping| 10 7) |LSAGG-;select!;M2A;5|
             (60 . |eq?|) |LSAGG-;merge!;M3A;6| (|Integer|)
             (66 . |minIndex|) (71 . |rest|) |LSAGG-;insert!;SAIA;7|
             (77 . |concat!|) |LSAGG-;insert!;2AIA;8|
             |LSAGG-;remove!;M2A;9| |LSAGG-;delete!;AIA;10|
             (|UniversalSegment| 31) (83 . |lo|) (88 . |hasHi|)
             (93 . |hi|) (98 . |maxIndex|) |LSAGG-;delete!;AUsA;11|
             (|Union| 7 '"failed") |LSAGG-;find;MAU;12|
             |LSAGG-;position;MAI;13| (103 . |reverse!|)
             (108 . |split!|) |LSAGG-;sorted?;MAB;15|
             |LSAGG-;reduce;MA2S;16| (114 . ~=) (120 . |reduce|)
             |LSAGG-;new;NniSA;18| |LSAGG-;map;M3A;19|
             |LSAGG-;reverse!;2A;20| (128 . |cyclic?|)
             |LSAGG-;copy;2A;21| (133 . |setfirst!|)
             |LSAGG-;copyInto!;2AIA;22| (139 . |position|) (146 . =)
             (152 . |remove!|) (158 . |removeDuplicates!|) (163 . <)
             (169 . <) (|Mapping| 7 7))
          '#(|sorted?| 175 |sort!| 181 |select!| 187 |reverse!| 193
             |removeDuplicates!| 198 |remove!| 203 |reduce| 209
             |position| 230 |new| 243 |merge!| 249 |merge| 256 |map|
             263 |list| 270 |insert!| 275 |find| 289 |delete!| 295
             |copyInto!| 307 |copy| 314 < 319)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 66
                                '(1 6 8 0 9 0 6 0 13 2 6 0 7 0 14 1 6
                                  10 0 16 1 6 0 0 17 1 6 7 0 18 3 6 7
                                  19 0 7 20 1 6 0 0 22 3 6 0 11 0 0 23
                                  1 10 0 0 25 2 6 0 0 0 26 2 6 10 0 0
                                  29 1 6 31 0 32 2 6 0 0 8 33 2 6 0 0 0
                                  35 1 39 31 0 40 1 39 10 0 41 1 39 31
                                  0 42 1 6 31 0 43 1 6 0 0 48 2 6 0 0
                                  31 49 2 7 10 0 0 52 4 0 7 19 0 7 7 53
                                  1 6 10 0 57 2 6 7 0 7 59 3 0 31 7 0
                                  31 61 2 7 10 0 0 62 2 6 0 27 0 63 1 0
                                  0 0 64 2 7 10 0 0 65 2 0 10 0 0 66 2
                                  0 10 11 0 50 2 0 0 11 0 12 2 0 0 27 0
                                  28 1 0 0 0 56 1 0 0 0 64 2 0 0 27 0
                                  37 3 0 7 19 0 7 51 4 0 7 19 0 7 7 53
                                  2 0 7 19 0 21 2 0 31 27 0 47 3 0 31 7
                                  0 31 61 2 0 0 8 7 54 3 0 0 11 0 0 30
                                  3 0 0 11 0 0 24 3 0 0 19 0 0 55 1 0 0
                                  7 15 3 0 0 7 0 31 34 3 0 0 0 0 31 36
                                  2 0 45 27 0 46 2 0 0 0 39 44 2 0 0 0
                                  31 38 3 0 0 0 0 31 60 1 0 0 0 58 2 0
                                  10 0 0 66)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|ListAggregate&| '|isFunctor|
             '(((|list| ($ |#2|)) T (ELT $ 15))
               ((|delete!| ($ $ (|Integer|))) T (ELT $ 38))
               ((|delete!| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ 44))
               ((|remove!| ($ (|Mapping| (|Boolean|) |#2|) $)) T
                (ELT $ 37))
               ((|insert!| ($ |#2| $ (|Integer|))) T (ELT $ 34))
               ((|insert!| ($ $ $ (|Integer|))) T (ELT $ 36))
               ((|merge!| ($ (|Mapping| (|Boolean|) |#2| |#2|) $ $)) T
                (ELT $ 30))
               ((|select!| ($ (|Mapping| (|Boolean|) |#2|) $)) T
                (ELT $ 28))
               ((|remove!| ($ |#2| $)) T (ELT $ NIL))
               ((|removeDuplicates!| ($ $)) T (ELT $ 64))
               ((|merge!| ($ $ $)) T (ELT $ NIL))
               ((|merge| ($ (|Mapping| (|Boolean|) |#2| |#2|) $ $)) T
                (ELT $ 24))
               ((|sorted?|
                    ((|Boolean|) (|Mapping| (|Boolean|) |#2| |#2|) $))
                T (ELT $ 50))
               ((|position|
                    ((|Integer|) (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 47))
               ((|position| ((|Integer|) |#2| $)) T (ELT $ NIL))
               ((|position| ((|Integer|) |#2| $ (|Integer|))) T
                (ELT $ 61))
               ((|merge| ($ $ $)) T (ELT $ NIL))
               ((|sorted?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|copyInto!| ($ $ $ (|Integer|))) T (ELT $ 60))
               ((|reverse!| ($ $)) T (ELT $ 56))
               ((|sort!| ($ (|Mapping| (|Boolean|) |#2| |#2|) $)) T
                (ELT $ 12))
               ((|sort!| ($ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ 66))
               ((|reduce|
                    (|#2| (|Mapping| |#2| |#2| |#2|) $ |#2| |#2|))
                T (ELT $ 53))
               ((|reduce| (|#2| (|Mapping| |#2| |#2| |#2|) $ |#2|)) T
                (ELT $ 51))
               ((|reduce| (|#2| (|Mapping| |#2| |#2| |#2|) $)) T
                (ELT $ 21))
               ((|find| ((|Union| |#2| "failed")
                         (|Mapping| (|Boolean|) |#2|) $))
                T (ELT $ 46))
               ((|new| ($ (|NonNegativeInteger|) |#2|)) T (ELT $ 54))
               ((|map| ($ (|Mapping| |#2| |#2| |#2|) $ $)) T
                (ELT $ 55))
               ((|map| ($ (|Mapping| |#2| |#2|) $)) T (ELT $ NIL))
               ((|copy| ($ $)) T (ELT $ 58)))
             (|addModemap| '|ListAggregate&|
                 '(|ListAggregate&| |#1| |#2|)
                 '((CATEGORY |domain| (SIGNATURE |list| (|#1| |#2|))
                       (SIGNATURE |delete!| (|#1| |#1| (|Integer|)))
                       (SIGNATURE |delete!|
                           (|#1| |#1| (|UniversalSegment| (|Integer|))))
                       (SIGNATURE |remove!|
                           (|#1| (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |insert!|
                           (|#1| |#2| |#1| (|Integer|)))
                       (SIGNATURE |insert!|
                           (|#1| |#1| |#1| (|Integer|)))
                       (SIGNATURE |merge!|
                           (|#1| (|Mapping| (|Boolean|) |#2| |#2|) |#1|
                                 |#1|))
                       (SIGNATURE |select!|
                           (|#1| (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |remove!| (|#1| |#2| |#1|))
                       (SIGNATURE |removeDuplicates!| (|#1| |#1|))
                       (SIGNATURE |merge!| (|#1| |#1| |#1|))
                       (SIGNATURE |merge|
                           (|#1| (|Mapping| (|Boolean|) |#2| |#2|) |#1|
                                 |#1|))
                       (SIGNATURE |sorted?|
                           ((|Boolean|)
                            (|Mapping| (|Boolean|) |#2| |#2|) |#1|))
                       (SIGNATURE |position|
                           ((|Integer|) (|Mapping| (|Boolean|) |#2|)
                            |#1|))
                       (SIGNATURE |position| ((|Integer|) |#2| |#1|))
                       (SIGNATURE |position|
                           ((|Integer|) |#2| |#1| (|Integer|)))
                       (SIGNATURE |merge| (|#1| |#1| |#1|))
                       (SIGNATURE |sorted?| ((|Boolean|) |#1|))
                       (SIGNATURE |copyInto!|
                           (|#1| |#1| |#1| (|Integer|)))
                       (SIGNATURE |reverse!| (|#1| |#1|))
                       (SIGNATURE |sort!|
                           (|#1| (|Mapping| (|Boolean|) |#2| |#2|)
                                 |#1|))
                       (SIGNATURE |sort!| (|#1| |#1|))
                       (SIGNATURE < ((|Boolean|) |#1| |#1|))
                       (SIGNATURE |reduce|
                           (|#2| (|Mapping| |#2| |#2| |#2|) |#1| |#2|
                                 |#2|))
                       (SIGNATURE |reduce|
                           (|#2| (|Mapping| |#2| |#2| |#2|) |#1| |#2|))
                       (SIGNATURE |reduce|
                           (|#2| (|Mapping| |#2| |#2| |#2|) |#1|))
                       (SIGNATURE |find|
                           ((|Union| |#2| "failed")
                            (|Mapping| (|Boolean|) |#2|) |#1|))
                       (SIGNATURE |new|
                           (|#1| (|NonNegativeInteger|) |#2|))
                       (SIGNATURE |map|
                           (|#1| (|Mapping| |#2| |#2| |#2|) |#1| |#1|))
                       (SIGNATURE |map|
                           (|#1| (|Mapping| |#2| |#2|) |#1|))
                       (SIGNATURE |copy| (|#1| |#1|)))
                   (|ListAggregate| |#2|) (|Type|))
                 T '|ListAggregate&|
                 (|put| '|ListAggregate&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |list| (|#1| |#2|))
                                 (SIGNATURE |delete!|
                                     (|#1| |#1| (|Integer|)))
                                 (SIGNATURE |delete!|
                                     (|#1| |#1|
                                      (|UniversalSegment| (|Integer|))))
                                 (SIGNATURE |remove!|
                                     (|#1| (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |insert!|
                                     (|#1| |#2| |#1| (|Integer|)))
                                 (SIGNATURE |insert!|
                                     (|#1| |#1| |#1| (|Integer|)))
                                 (SIGNATURE |merge!|
                                     (|#1|
                                      (|Mapping| (|Boolean|) |#2| |#2|)
                                      |#1| |#1|))
                                 (SIGNATURE |select!|
                                     (|#1| (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |remove!| (|#1| |#2| |#1|))
                                 (SIGNATURE |removeDuplicates!|
                                     (|#1| |#1|))
                                 (SIGNATURE |merge!| (|#1| |#1| |#1|))
                                 (SIGNATURE |merge|
                                     (|#1|
                                      (|Mapping| (|Boolean|) |#2| |#2|)
                                      |#1| |#1|))
                                 (SIGNATURE |sorted?|
                                     ((|Boolean|)
                                      (|Mapping| (|Boolean|) |#2| |#2|)
                                      |#1|))
                                 (SIGNATURE |position|
                                     ((|Integer|)
                                      (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |position|
                                     ((|Integer|) |#2| |#1|))
                                 (SIGNATURE |position|
                                     ((|Integer|) |#2| |#1|
                                      (|Integer|)))
                                 (SIGNATURE |merge| (|#1| |#1| |#1|))
                                 (SIGNATURE |sorted?|
                                     ((|Boolean|) |#1|))
                                 (SIGNATURE |copyInto!|
                                     (|#1| |#1| |#1| (|Integer|)))
                                 (SIGNATURE |reverse!| (|#1| |#1|))
                                 (SIGNATURE |sort!|
                                     (|#1|
                                      (|Mapping| (|Boolean|) |#2| |#2|)
                                      |#1|))
                                 (SIGNATURE |sort!| (|#1| |#1|))
                                 (SIGNATURE < ((|Boolean|) |#1| |#1|))
                                 (SIGNATURE |reduce|
                                     (|#2| (|Mapping| |#2| |#2| |#2|)
                                      |#1| |#2| |#2|))
                                 (SIGNATURE |reduce|
                                     (|#2| (|Mapping| |#2| |#2| |#2|)
                                      |#1| |#2|))
                                 (SIGNATURE |reduce|
                                     (|#2| (|Mapping| |#2| |#2| |#2|)
                                      |#1|))
                                 (SIGNATURE |find|
                                     ((|Union| |#2| "failed")
                                      (|Mapping| (|Boolean|) |#2|)
                                      |#1|))
                                 (SIGNATURE |new|
                                     (|#1| (|NonNegativeInteger|) |#2|))
                                 (SIGNATURE |map|
                                     (|#1| (|Mapping| |#2| |#2| |#2|)
                                      |#1| |#1|))
                                 (SIGNATURE |map|
                                     (|#1| (|Mapping| |#2| |#2|) |#1|))
                                 (SIGNATURE |copy| (|#1| |#1|)))
                             (|ListAggregate| |#2|) (|Type|))
                        |$CategoryFrame|)))) 
