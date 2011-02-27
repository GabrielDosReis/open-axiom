
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
  (|LSAGG-;mergeSort| |f| |l| (SPADCALL |l| (|shellEntry| $ 9)) $)) 

(DEFUN |LSAGG-;list;SA;2| (|x| $)
  (SPADCALL |x| (SPADCALL (|shellEntry| $ 13)) (|shellEntry| $ 14))) 

(DEFUN |LSAGG-;reduce;MAS;3| (|f| |x| $)
  (COND
    ((SPADCALL |x| (|shellEntry| $ 16))
     (|error| "reducing over an empty list needs the 3 argument form"))
    (T (SPADCALL |f| (SPADCALL |x| (|shellEntry| $ 17))
           (SPADCALL |x| (|shellEntry| $ 18)) (|shellEntry| $ 20))))) 

(DEFUN |LSAGG-;merge;M3A;4| (|f| |p| |q| $)
  (SPADCALL |f| (SPADCALL |p| (|shellEntry| $ 22))
      (SPADCALL |q| (|shellEntry| $ 22)) (|shellEntry| $ 23))) 

(DEFUN |LSAGG-;select!;M2A;5| (|f| |x| $)
  (PROG (|y| |z|)
    (RETURN
      (SEQ (LOOP
             (COND
               ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                          (NOT (SPADCALL
                                   (SPADCALL |x| (|shellEntry| $ 18))
                                   |f|))))
                (RETURN NIL))
               (T (SETQ |x| (SPADCALL |x| (|shellEntry| $ 17))))))
           (EXIT (COND
                   ((SPADCALL |x| (|shellEntry| $ 16)) |x|)
                   (T (SEQ (LETT |y| |x| |LSAGG-;select!;M2A;5|)
                           (LETT |z| (SPADCALL |y| (|shellEntry| $ 17))
                                 |LSAGG-;select!;M2A;5|)
                           (LOOP
                             (COND
                               ((NOT (NOT
                                      (SPADCALL |z|
                                       (|shellEntry| $ 16))))
                                (RETURN NIL))
                               (T (COND
                                    ((SPADCALL
                                      (SPADCALL |z|
                                       (|shellEntry| $ 18))
                                      |f|)
                                     (SEQ (SETQ |y| |z|)
                                      (EXIT
                                       (SETQ |z|
                                        (SPADCALL |z|
                                         (|shellEntry| $ 17))))))
                                    (T
                                     (SEQ
                                      (SETQ |z|
                                       (SPADCALL |z|
                                        (|shellEntry| $ 17)))
                                      (EXIT
                                       (SPADCALL |y| |z|
                                        (|shellEntry| $ 27)))))))))
                           (EXIT |x|))))))))) 

(DEFUN |LSAGG-;merge!;M3A;6| (|f| |p| |q| $)
  (PROG (|r| |t|)
    (RETURN
      (COND
        ((SPADCALL |p| (|shellEntry| $ 16)) |q|)
        ((SPADCALL |q| (|shellEntry| $ 16)) |p|)
        ((SPADCALL |p| |q| (|shellEntry| $ 30))
         (|error| "cannot merge a list into itself"))
        (T (SEQ (COND
                  ((SPADCALL (SPADCALL |p| (|shellEntry| $ 18))
                       (SPADCALL |q| (|shellEntry| $ 18)) |f|)
                   (SEQ (LETT |r| (LETT |t| |p| |LSAGG-;merge!;M3A;6|)
                              |LSAGG-;merge!;M3A;6|)
                        (EXIT (SETQ |p|
                                    (SPADCALL |p| (|shellEntry| $ 17))))))
                  (T (SEQ (LETT |r|
                                (LETT |t| |q| |LSAGG-;merge!;M3A;6|)
                                |LSAGG-;merge!;M3A;6|)
                          (EXIT (SETQ |q|
                                      (SPADCALL |q|
                                       (|shellEntry| $ 17)))))))
                (LOOP
                  (COND
                    ((NOT (AND (NOT (SPADCALL |p| (|shellEntry| $ 16)))
                               (NOT (SPADCALL |q| (|shellEntry| $ 16)))))
                     (RETURN NIL))
                    (T (COND
                         ((SPADCALL (SPADCALL |p| (|shellEntry| $ 18))
                              (SPADCALL |q| (|shellEntry| $ 18)) |f|)
                          (SEQ (SPADCALL |t| |p| (|shellEntry| $ 27))
                               (LETT |t| |p| |LSAGG-;merge!;M3A;6|)
                               (EXIT (SETQ |p|
                                      (SPADCALL |p|
                                       (|shellEntry| $ 17))))))
                         (T (SEQ (SPADCALL |t| |q| (|shellEntry| $ 27))
                                 (LETT |t| |q| |LSAGG-;merge!;M3A;6|)
                                 (EXIT (SETQ |q|
                                        (SPADCALL |q|
                                         (|shellEntry| $ 17))))))))))
                (SPADCALL |t|
                    (COND
                      ((SPADCALL |p| (|shellEntry| $ 16)) |q|)
                      (T |p|))
                    (|shellEntry| $ 27))
                (EXIT |r|))))))) 

(DEFUN |LSAGG-;insert!;SAIA;7| (|s| |x| |i| $)
  (PROG (|y| |z|)
    (RETURN
      (LET ((|m| (SPADCALL |x| (|shellEntry| $ 33))))
        (COND
          ((< |i| |m|) (|error| "index out of range"))
          ((EQL |i| |m|) (SPADCALL |s| |x| (|shellEntry| $ 14)))
          (T (SEQ (LETT |y|
                        (SPADCALL |x|
                            (LET ((#0=#:G1442 (- (- |i| 1) |m|)))
                              (|check-subtype| (NOT (MINUSP #0#))
                                  '(|NonNegativeInteger|) #0#))
                            (|shellEntry| $ 39))
                        |LSAGG-;insert!;SAIA;7|)
                  (LETT |z| (SPADCALL |y| (|shellEntry| $ 17))
                        |LSAGG-;insert!;SAIA;7|)
                  (SPADCALL |y| (SPADCALL |s| |z| (|shellEntry| $ 14))
                      (|shellEntry| $ 27))
                  (EXIT |x|)))))))) 

(DEFUN |LSAGG-;insert!;2AIA;8| (|w| |x| |i| $)
  (PROG (|y| |z|)
    (RETURN
      (LET ((|m| (SPADCALL |x| (|shellEntry| $ 33))))
        (COND
          ((< |i| |m|) (|error| "index out of range"))
          ((EQL |i| |m|) (SPADCALL |w| |x| (|shellEntry| $ 41)))
          (T (SEQ (LETT |y|
                        (SPADCALL |x|
                            (LET ((#0=#:G1446 (- (- |i| 1) |m|)))
                              (|check-subtype| (NOT (MINUSP #0#))
                                  '(|NonNegativeInteger|) #0#))
                            (|shellEntry| $ 39))
                        |LSAGG-;insert!;2AIA;8|)
                  (LETT |z| (SPADCALL |y| (|shellEntry| $ 17))
                        |LSAGG-;insert!;2AIA;8|)
                  (SPADCALL |y| |w| (|shellEntry| $ 27))
                  (SPADCALL |y| |z| (|shellEntry| $ 41)) (EXIT |x|)))))))) 

(DEFUN |LSAGG-;remove!;M2A;9| (|f| |x| $)
  (PROG (|p| |q|)
    (RETURN
      (SEQ (LOOP
             (COND
               ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                          (SPADCALL (SPADCALL |x| (|shellEntry| $ 18))
                              |f|)))
                (RETURN NIL))
               (T (SETQ |x| (SPADCALL |x| (|shellEntry| $ 17))))))
           (EXIT (COND
                   ((SPADCALL |x| (|shellEntry| $ 16)) |x|)
                   (T (SEQ (LETT |p| |x| |LSAGG-;remove!;M2A;9|)
                           (LETT |q| (SPADCALL |x| (|shellEntry| $ 17))
                                 |LSAGG-;remove!;M2A;9|)
                           (LOOP
                             (COND
                               ((NOT (NOT
                                      (SPADCALL |q|
                                       (|shellEntry| $ 16))))
                                (RETURN NIL))
                               (T (COND
                                    ((SPADCALL
                                      (SPADCALL |q|
                                       (|shellEntry| $ 18))
                                      |f|)
                                     (SETQ |q|
                                      (SPADCALL |p|
                                       (SPADCALL |q|
                                        (|shellEntry| $ 17))
                                       (|shellEntry| $ 27))))
                                    (T
                                     (SEQ (SETQ |p| |q|)
                                      (EXIT
                                       (SETQ |q|
                                        (SPADCALL |q|
                                         (|shellEntry| $ 17))))))))))
                           (EXIT |x|))))))))) 

(DEFUN |LSAGG-;delete!;AIA;10| (|x| |i| $)
  (PROG (|y|)
    (RETURN
      (LET ((|m| (SPADCALL |x| (|shellEntry| $ 33))))
        (COND
          ((< |i| |m|) (|error| "index out of range"))
          ((EQL |i| |m|) (SPADCALL |x| (|shellEntry| $ 17)))
          (T (SEQ (LETT |y|
                        (SPADCALL |x|
                            (LET ((#0=#:G1458 (- (- |i| 1) |m|)))
                              (|check-subtype| (NOT (MINUSP #0#))
                                  '(|NonNegativeInteger|) #0#))
                            (|shellEntry| $ 39))
                        |LSAGG-;delete!;AIA;10|)
                  (SPADCALL |y| (SPADCALL |y| 2 (|shellEntry| $ 39))
                      (|shellEntry| $ 27))
                  (EXIT |x|)))))))) 

(DEFUN |LSAGG-;delete!;AUsA;11| (|x| |i| $)
  (PROG (|h| |t|)
    (RETURN
      (LET ((|l| (SPADCALL |i| (|shellEntry| $ 46)))
            (|m| (SPADCALL |x| (|shellEntry| $ 33))))
        (COND
          ((< |l| |m|) (|error| "index out of range"))
          (T (SEQ (LETT |h|
                        (COND
                          ((SPADCALL |i| (|shellEntry| $ 47))
                           (SPADCALL |i| (|shellEntry| $ 48)))
                          (T (SPADCALL |x| (|shellEntry| $ 49))))
                        |LSAGG-;delete!;AUsA;11|)
                  (EXIT (COND
                          ((< |h| |l|) |x|)
                          ((EQL |l| |m|)
                           (SPADCALL |x|
                               (LET ((#0=#:G1464 (- (+ |h| 1) |m|)))
                                 (|check-subtype| (NOT (MINUSP #0#))
                                     '(|NonNegativeInteger|) #0#))
                               (|shellEntry| $ 39)))
                          (T (SEQ (LETT |t|
                                        (SPADCALL |x|
                                         (LET
                                          ((#1=#:G1465
                                            (- (- |l| 1) |m|)))
                                           (|check-subtype|
                                            (NOT (MINUSP #1#))
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|shellEntry| $ 39))
                                        |LSAGG-;delete!;AUsA;11|)
                                  (SPADCALL |t|
                                      (SPADCALL |t|
                                       (LET
                                        ((#2=#:G1466 (+ (- |h| |l|) 2)))
                                         (|check-subtype|
                                          (NOT (MINUSP #2#))
                                          '(|NonNegativeInteger|) #2#))
                                       (|shellEntry| $ 39))
                                      (|shellEntry| $ 27))
                                  (EXIT |x|)))))))))))) 

(DEFUN |LSAGG-;find;MAU;12| (|f| |x| $)
  (SEQ (LOOP
         (COND
           ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                      (NOT (SPADCALL (SPADCALL |x| (|shellEntry| $ 18))
                               |f|))))
            (RETURN NIL))
           (T (SETQ |x| (SPADCALL |x| (|shellEntry| $ 17))))))
       (EXIT (COND
               ((SPADCALL |x| (|shellEntry| $ 16)) (CONS 1 "failed"))
               (T (CONS 0 (SPADCALL |x| (|shellEntry| $ 18)))))))) 

(DEFUN |LSAGG-;position;MAI;13| (|f| |x| $)
  (LET ((|k| (SPADCALL |x| (|shellEntry| $ 33))))
    (SEQ (LOOP
           (COND
             ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                        (NOT (SPADCALL
                                 (SPADCALL |x| (|shellEntry| $ 18))
                                 |f|))))
              (RETURN NIL))
             (T (SEQ (SETQ |x| (SPADCALL |x| (|shellEntry| $ 17)))
                     (EXIT (SETQ |k| (+ |k| 1)))))))
         (EXIT (COND
                 ((SPADCALL |x| (|shellEntry| $ 16))
                  (- (SPADCALL |x| (|shellEntry| $ 33)) 1))
                 (T |k|)))))) 

(DEFUN |LSAGG-;mergeSort| (|f| |p| |n| $)
  (PROG (|l| |q|)
    (RETURN
      (SEQ (COND
             ((AND (EQL |n| 2)
                   (SPADCALL
                       (SPADCALL (SPADCALL |p| (|shellEntry| $ 17))
                           (|shellEntry| $ 18))
                       (SPADCALL |p| (|shellEntry| $ 18)) |f|))
              (SETQ |p| (SPADCALL |p| (|shellEntry| $ 55)))))
           (EXIT (COND
                   ((< |n| 3) |p|)
                   (T (SEQ (LETT |l|
                                 (LET ((#0=#:G1486 (TRUNCATE |n| 2)))
                                   (|check-subtype| (NOT (MINUSP #0#))
                                    '(|NonNegativeInteger|) #0#))
                                 |LSAGG-;mergeSort|)
                           (LETT |q|
                                 (SPADCALL |p| |l| (|shellEntry| $ 57))
                                 |LSAGG-;mergeSort|)
                           (SETQ |p|
                                 (|LSAGG-;mergeSort| |f| |p| |l| $))
                           (SETQ |q|
                                 (|LSAGG-;mergeSort| |f| |q|
                                     (- |n| |l|) $))
                           (EXIT (SPADCALL |f| |p| |q|
                                     (|shellEntry| $ 23))))))))))) 

(DEFUN |LSAGG-;sorted?;MAB;15| (|f| |l| $)
  (PROG (|p|)
    (RETURN
      (OR (SPADCALL |l| (|shellEntry| $ 16))
          (SEQ (LETT |p| (SPADCALL |l| (|shellEntry| $ 17))
                     |LSAGG-;sorted?;MAB;15|)
               (LOOP
                 (COND
                   ((NOT (NOT (SPADCALL |p| (|shellEntry| $ 16))))
                    (RETURN NIL))
                   (T (SEQ (COND
                             ((NOT (SPADCALL
                                    (SPADCALL |l| (|shellEntry| $ 18))
                                    (SPADCALL |p| (|shellEntry| $ 18))
                                    |f|))
                              (RETURN-FROM |LSAGG-;sorted?;MAB;15|
                                NIL)))
                           (EXIT (SETQ |p|
                                       (SPADCALL (SETQ |l| |p|)
                                        (|shellEntry| $ 17))))))))
               (EXIT T)))))) 

(DEFUN |LSAGG-;reduce;MA2S;16| (|f| |x| |i| $)
  (LET ((|r| |i|))
    (SEQ (LOOP
           (COND
             ((NOT (NOT (SPADCALL |x| (|shellEntry| $ 16))))
              (RETURN NIL))
             (T (SEQ (SETQ |r|
                           (SPADCALL |r|
                               (SPADCALL |x| (|shellEntry| $ 18)) |f|))
                     (EXIT (SETQ |x|
                                 (SPADCALL |x| (|shellEntry| $ 17))))))))
         (EXIT |r|)))) 

(DEFUN |LSAGG-;reduce;MA3S;17| (|f| |x| |i| |a| $)
  (LET ((|r| |i|))
    (SEQ (LOOP
           (COND
             ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                        (SPADCALL |r| |a| (|shellEntry| $ 61))))
              (RETURN NIL))
             (T (SEQ (SETQ |r|
                           (SPADCALL |r|
                               (SPADCALL |x| (|shellEntry| $ 18)) |f|))
                     (EXIT (SETQ |x|
                                 (SPADCALL |x| (|shellEntry| $ 17))))))))
         (EXIT |r|)))) 

(DEFUN |LSAGG-;new;NniSA;18| (|n| |s| $)
  (LET ((|l| (SPADCALL (|shellEntry| $ 13))))
    (SEQ (LET ((|k| 1))
           (LOOP
             (COND
               ((> |k| |n|) (RETURN NIL))
               (T (SETQ |l| (SPADCALL |s| |l| (|shellEntry| $ 14)))))
             (SETQ |k| (+ |k| 1))))
         (EXIT |l|)))) 

(DEFUN |LSAGG-;map;M3A;19| (|f| |x| |y| $)
  (LET ((|z| (SPADCALL (|shellEntry| $ 13))))
    (SEQ (LOOP
           (COND
             ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                        (NOT (SPADCALL |y| (|shellEntry| $ 16)))))
              (RETURN NIL))
             (T (SEQ (SETQ |z|
                           (SPADCALL
                               (SPADCALL
                                   (SPADCALL |x| (|shellEntry| $ 18))
                                   (SPADCALL |y| (|shellEntry| $ 18))
                                   |f|)
                               |z| (|shellEntry| $ 14)))
                     (SETQ |x| (SPADCALL |x| (|shellEntry| $ 17)))
                     (EXIT (SETQ |y|
                                 (SPADCALL |y| (|shellEntry| $ 17))))))))
         (EXIT (SPADCALL |z| (|shellEntry| $ 55)))))) 

(DEFUN |LSAGG-;reverse!;2A;20| (|x| $)
  (PROG (|y| |z|)
    (RETURN
      (COND
        ((OR (SPADCALL |x| (|shellEntry| $ 16))
             (SPADCALL
                 (LETT |y| (SPADCALL |x| (|shellEntry| $ 17))
                       |LSAGG-;reverse!;2A;20|)
                 (|shellEntry| $ 16)))
         |x|)
        (T (SEQ (SPADCALL |x| (SPADCALL (|shellEntry| $ 13))
                    (|shellEntry| $ 27))
                (LOOP
                  (COND
                    ((NOT (NOT (SPADCALL |y| (|shellEntry| $ 16))))
                     (RETURN NIL))
                    (T (SEQ (LETT |z|
                                  (SPADCALL |y| (|shellEntry| $ 17))
                                  |LSAGG-;reverse!;2A;20|)
                            (SPADCALL |y| |x| (|shellEntry| $ 27))
                            (SETQ |x| |y|) (EXIT (SETQ |y| |z|))))))
                (EXIT |x|))))))) 

(DEFUN |LSAGG-;copy;2A;21| (|x| $)
  (LET ((|y| (SPADCALL (|shellEntry| $ 13))))
    (SEQ (LET ((|k| 0))
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |x| (|shellEntry| $ 16))))
                (RETURN NIL))
               (T (COND
                    ((AND (EQL |k| 1000)
                          (SPADCALL |x| (|shellEntry| $ 67)))
                     (|error| "cyclic list"))
                    (T (SEQ (SETQ |y|
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (|shellEntry| $ 18))
                                      |y| (|shellEntry| $ 14)))
                            (EXIT (SETQ |x|
                                        (SPADCALL |x|
                                         (|shellEntry| $ 17)))))))))
             (SETQ |k| (+ |k| 1))))
         (EXIT (SPADCALL |y| (|shellEntry| $ 55)))))) 

(DEFUN |LSAGG-;copyInto!;2AIA;22| (|y| |x| |s| $)
  (PROG (|z|)
    (RETURN
      (LET ((|m| (SPADCALL |y| (|shellEntry| $ 33))))
        (COND
          ((< |s| |m|) (|error| "index out of range"))
          (T (SEQ (LETT |z|
                        (SPADCALL |y|
                            (LET ((#0=#:G1527 (- |s| |m|)))
                              (|check-subtype| (NOT (MINUSP #0#))
                                  '(|NonNegativeInteger|) #0#))
                            (|shellEntry| $ 39))
                        |LSAGG-;copyInto!;2AIA;22|)
                  (LOOP
                    (COND
                      ((NOT (AND (NOT (SPADCALL |z|
                                       (|shellEntry| $ 16)))
                                 (NOT (SPADCALL |x|
                                       (|shellEntry| $ 16)))))
                       (RETURN NIL))
                      (T (SEQ (SPADCALL |z|
                                  (SPADCALL |x| (|shellEntry| $ 18))
                                  (|shellEntry| $ 69))
                              (SETQ |x|
                                    (SPADCALL |x| (|shellEntry| $ 17)))
                              (EXIT (SETQ |z|
                                     (SPADCALL |z| (|shellEntry| $ 17))))))))
                  (EXIT |y|)))))))) 

(DEFUN |LSAGG-;position;SA2I;23| (|w| |x| |s| $)
  (PROG (|k|)
    (RETURN
      (LET ((|m| (SPADCALL |x| (|shellEntry| $ 33))))
        (COND
          ((< |s| |m|) (|error| "index out of range"))
          (T (SEQ (SETQ |x|
                        (SPADCALL |x|
                            (LET ((#0=#:G1534 (- |s| |m|)))
                              (|check-subtype| (NOT (MINUSP #0#))
                                  '(|NonNegativeInteger|) #0#))
                            (|shellEntry| $ 39)))
                  (LETT |k| |s| |LSAGG-;position;SA2I;23|)
                  (LOOP
                    (COND
                      ((NOT (AND (NOT (SPADCALL |x|
                                       (|shellEntry| $ 16)))
                                 (SPADCALL |w|
                                     (SPADCALL |x| (|shellEntry| $ 18))
                                     (|shellEntry| $ 61))))
                       (RETURN NIL))
                      (T (SEQ (SETQ |x|
                                    (SPADCALL |x| (|shellEntry| $ 17)))
                              (EXIT (SETQ |k| (+ |k| 1)))))))
                  (EXIT (COND
                          ((SPADCALL |x| (|shellEntry| $ 16))
                           (- (SPADCALL |x| (|shellEntry| $ 33)) 1))
                          (T |k|)))))))))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24| (|l| $)
  (LET ((|p| |l|))
    (SEQ (LOOP
           (COND
             ((NOT (NOT (SPADCALL |p| (|shellEntry| $ 16))))
              (RETURN NIL))
             (T (SETQ |p|
                      (SPADCALL |p|
                          (SPADCALL
                              (CONS #'|LSAGG-;removeDuplicates!;2A;24!0|
                                    (VECTOR $ |p|))
                              (SPADCALL |p| (|shellEntry| $ 17))
                              (|shellEntry| $ 73))
                          (|shellEntry| $ 27))))))
         (EXIT |l|)))) 

(DEFUN |LSAGG-;removeDuplicates!;2A;24!0| (|#1| $$)
  (LET (($ (SVREF $$ 0)))
    (SPADCALL |#1| (SPADCALL (SVREF $$ 1) (|shellEntry| $ 18))
        (|shellEntry| $ 72)))) 

(DEFUN |LSAGG-;<;2AB;25| (|x| |y| $)
  (SEQ (LOOP
         (COND
           ((NOT (AND (NOT (SPADCALL |x| (|shellEntry| $ 16)))
                      (NOT (SPADCALL |y| (|shellEntry| $ 16)))))
            (RETURN NIL))
           (T (COND
                ((SPADCALL (SPADCALL |x| (|shellEntry| $ 18))
                     (SPADCALL |y| (|shellEntry| $ 18))
                     (|shellEntry| $ 61))
                 (RETURN-FROM |LSAGG-;<;2AB;25|
                   (SPADCALL (SPADCALL |x| (|shellEntry| $ 18))
                       (SPADCALL |y| (|shellEntry| $ 18))
                       (|shellEntry| $ 75))))
                (T (SEQ (SETQ |x| (SPADCALL |x| (|shellEntry| $ 17)))
                        (EXIT (SETQ |y|
                                    (SPADCALL |y| (|shellEntry| $ 17))))))))))
       (EXIT (AND (SPADCALL |x| (|shellEntry| $ 16))
                  (NOT (SPADCALL |y| (|shellEntry| $ 16))))))) 

(DEFUN |ListAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|ListAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 78)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    (SETF (|shellEntry| $ 7) |#2|)
    (COND
      ((|HasCategory| |#2| '(|SetCategory|))
       (SETF (|shellEntry| $ 62)
             (CONS (|dispatchFunction| |LSAGG-;reduce;MA3S;17|) $))))
    (COND
      ((|HasCategory| |#2| '(|SetCategory|))
       (PROGN
         (SETF (|shellEntry| $ 71)
               (CONS (|dispatchFunction| |LSAGG-;position;SA2I;23|) $))
         (SETF (|shellEntry| $ 74)
               (CONS (|dispatchFunction|
                         |LSAGG-;removeDuplicates!;2A;24|)
                     $)))))
    (COND
      ((|HasCategory| |#2| '(|OrderedSet|))
       (SETF (|shellEntry| $ 76)
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
             |LSAGG-;position;MAI;13| (139 . |reverse!|) (144 . |quo|)
             (150 . |split!|) (156 . |true|) |LSAGG-;sorted?;MAB;15|
             |LSAGG-;reduce;MA2S;16| (160 . ~=) (166 . |reduce|)
             |LSAGG-;new;NniSA;18| |LSAGG-;map;M3A;19|
             |LSAGG-;reverse!;2A;20| (174 . =) (180 . |cyclic?|)
             |LSAGG-;copy;2A;21| (185 . |setfirst!|)
             |LSAGG-;copyInto!;2AIA;22| (191 . |position|) (198 . =)
             (204 . |remove!|) (210 . |removeDuplicates!|) (215 . <)
             (221 . <) (|Mapping| 7 7))
          '#(|sorted?| 227 |sort!| 233 |select!| 239 |reverse!| 245
             |removeDuplicates!| 250 |remove!| 255 |reduce| 261
             |position| 282 |new| 295 |merge!| 301 |merge| 308 |map|
             315 |list| 322 |insert!| 327 |find| 341 |delete!| 347
             |copyInto!| 359 |copy| 366 < 371)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 76
                                '(1 6 8 0 9 0 6 0 13 2 6 0 7 0 14 1 6
                                  10 0 16 1 6 0 0 17 1 6 7 0 18 3 6 7
                                  19 0 7 20 1 6 0 0 22 3 6 0 11 0 0 23
                                  0 10 0 25 1 10 0 0 26 2 6 0 0 0 27 2
                                  6 10 0 0 30 1 6 32 0 33 2 32 10 0 0
                                  34 2 32 10 0 0 35 0 8 0 36 0 32 0 37
                                  2 32 0 0 0 38 2 6 0 0 8 39 2 6 0 0 0
                                  41 1 45 32 0 46 1 45 10 0 47 1 45 32
                                  0 48 1 6 32 0 49 2 32 0 0 0 50 1 6 0
                                  0 55 2 32 0 0 0 56 2 6 0 0 32 57 0 10
                                  0 58 2 7 10 0 0 61 4 0 7 19 0 7 7 62
                                  2 8 10 0 0 66 1 6 10 0 67 2 6 7 0 7
                                  69 3 0 32 7 0 32 71 2 7 10 0 0 72 2 6
                                  0 28 0 73 1 0 0 0 74 2 7 10 0 0 75 2
                                  0 10 0 0 76 2 0 10 11 0 59 2 0 0 11 0
                                  12 2 0 0 28 0 29 1 0 0 0 65 1 0 0 0
                                  74 2 0 0 28 0 43 3 0 7 19 0 7 60 4 0
                                  7 19 0 7 7 62 2 0 7 19 0 21 2 0 32 28
                                  0 54 3 0 32 7 0 32 71 2 0 0 8 7 63 3
                                  0 0 11 0 0 31 3 0 0 11 0 0 24 3 0 0
                                  19 0 0 64 1 0 0 7 15 3 0 0 7 0 32 40
                                  3 0 0 0 0 32 42 2 0 52 28 0 53 2 0 0
                                  0 45 51 2 0 0 0 32 44 3 0 0 0 0 32 70
                                  1 0 0 0 68 2 0 10 0 0 76)))))
          '|lookupComplete|)) 
