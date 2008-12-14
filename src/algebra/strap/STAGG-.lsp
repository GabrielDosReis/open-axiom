
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |STAGG-;explicitlyFinite?;AB;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |STAGG-;possiblyInfinite?;AB;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |STAGG-;first;ANniA;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |STAGG-;c2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |STAGG-;elt;AIS;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |STAGG-;elt;AUsA;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |STAGG-;concat;3A;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |STAGG-;concat;LA;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |STAGG-;map!;M2A;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |STAGG-;fill!;ASA;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Thing| |%Shell|)
                    |%Thing|)
                |STAGG-;setelt;AI2S;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |STAGG-;setelt;AUs2S;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |STAGG-;concat!;3A;13|)) 

(DEFUN |STAGG-;explicitlyFinite?;AB;1| (|x| $)
  (NOT (SPADCALL |x| (|getShellEntry| $ 9)))) 

(DEFUN |STAGG-;possiblyInfinite?;AB;2| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 9))) 

(DEFUN |STAGG-;first;ANniA;3| (|x| |n| $)
  (PROG (#0=#:G1452 |i|)
    (RETURN
      (SEQ (SPADCALL
               (PROGN
                 (LETT #0# NIL |STAGG-;first;ANniA;3|)
                 (SEQ (LETT |i| 1 |STAGG-;first;ANniA;3|) G190
                      (COND ((QSGREATERP |i| |n|) (GO G191)))
                      (SEQ (EXIT (LETT #0#
                                       (CONS
                                        (|STAGG-;c2| |x|
                                         (LETT |x|
                                          (SPADCALL |x|
                                           (|getShellEntry| $ 12))
                                          |STAGG-;first;ANniA;3|)
                                         $)
                                        #0#)
                                       |STAGG-;first;ANniA;3|)))
                      (LETT |i| (QSADD1 |i|) |STAGG-;first;ANniA;3|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 14)))))) 

(DEFUN |STAGG-;c2| (|x| |r| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 17))
     (|error| "Index out of range"))
    ('T (SPADCALL |x| (|getShellEntry| $ 18))))) 

(DEFUN |STAGG-;elt;AIS;5| (|x| |i| $)
  (PROG (#0=#:G1413)
    (RETURN
      (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 20)))
                 |STAGG-;elt;AIS;5|)
           (COND
             ((OR (< |i| 0)
                  (SPADCALL
                      (LETT |x|
                            (SPADCALL |x|
                                (PROG1 (LETT #0# |i|
                                        |STAGG-;elt;AIS;5|)
                                  (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                (|getShellEntry| $ 21))
                            |STAGG-;elt;AIS;5|)
                      (|getShellEntry| $ 17)))
              (EXIT (|error| "index out of range"))))
           (EXIT (SPADCALL |x| (|getShellEntry| $ 18))))))) 

(DEFUN |STAGG-;elt;AUsA;6| (|x| |i| $)
  (PROG (|l| #0=#:G1417 |h| #1=#:G1419 #2=#:G1420)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 24))
                    (SPADCALL |x| (|getShellEntry| $ 20)))
                 |STAGG-;elt;AUsA;6|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ((NULL (SPADCALL |i| (|getShellEntry| $ 25)))
                    (SPADCALL
                        (SPADCALL |x|
                            (PROG1 (LETT #0# |l| |STAGG-;elt;AUsA;6|)
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            (|getShellEntry| $ 21))
                        (|getShellEntry| $ 26)))
                   ('T
                    (SEQ (LETT |h|
                               (- (SPADCALL |i| (|getShellEntry| $ 27))
                                  (SPADCALL |x| (|getShellEntry| $ 20)))
                               |STAGG-;elt;AUsA;6|)
                         (EXIT (COND
                                 ((< |h| |l|)
                                  (SPADCALL (|getShellEntry| $ 28)))
                                 ('T
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (PROG1
                                        (LETT #1# |l|
                                         |STAGG-;elt;AUsA;6|)
                                         (|check-subtype| (>= #1# 0)
                                          '(|NonNegativeInteger|) #1#))
                                       (|getShellEntry| $ 21))
                                      (PROG1
                                       (LETT #2# (+ (- |h| |l|) 1)
                                        |STAGG-;elt;AUsA;6|)
                                        (|check-subtype| (>= #2# 0)
                                         '(|NonNegativeInteger|) #2#))
                                      (|getShellEntry| $ 29))))))))))))) 

(DEFUN |STAGG-;concat;3A;7| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 26)) |y|
      (|getShellEntry| $ 31))) 

(DEFUN |STAGG-;concat;LA;8| (|l| $)
  (COND
    ((NULL |l|) (SPADCALL (|getShellEntry| $ 28)))
    ('T
     (SPADCALL (SPADCALL (|SPADfirst| |l|) (|getShellEntry| $ 26))
         (SPADCALL (CDR |l|) (|getShellEntry| $ 34))
         (|getShellEntry| $ 31))))) 

(DEFUN |STAGG-;map!;M2A;9| (|f| |l| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |l| |STAGG-;map!;M2A;9|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |l| (|getShellEntry| $ 17))))
                   (GO G191)))
                (SEQ (SPADCALL |l|
                         (SPADCALL
                             (SPADCALL |l| (|getShellEntry| $ 18)) |f|)
                         (|getShellEntry| $ 36))
                     (EXIT (LETT |l|
                                 (SPADCALL |l| (|getShellEntry| $ 12))
                                 |STAGG-;map!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |y|))))) 

(DEFUN |STAGG-;fill!;ASA;10| (|x| |s| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |x| |STAGG-;fill!;ASA;10|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |y| (|getShellEntry| $ 17))))
                   (GO G191)))
                (SEQ (SPADCALL |y| |s| (|getShellEntry| $ 36))
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 12))
                                 |STAGG-;fill!;ASA;10|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |STAGG-;setelt;AI2S;11| (|x| |i| |s| $)
  (PROG (#0=#:G1436)
    (RETURN
      (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 20)))
                 |STAGG-;setelt;AI2S;11|)
           (COND
             ((OR (< |i| 0)
                  (SPADCALL
                      (LETT |x|
                            (SPADCALL |x|
                                (PROG1 (LETT #0# |i|
                                        |STAGG-;setelt;AI2S;11|)
                                  (|check-subtype| (>= #0# 0)
                                      '(|NonNegativeInteger|) #0#))
                                (|getShellEntry| $ 21))
                            |STAGG-;setelt;AI2S;11|)
                      (|getShellEntry| $ 17)))
              (EXIT (|error| "index out of range"))))
           (EXIT (SPADCALL |x| |s| (|getShellEntry| $ 36))))))) 

(DEFUN |STAGG-;setelt;AUs2S;12| (|x| |i| |s| $)
  (PROG (|l| |h| #0=#:G1441 #1=#:G1442 |z| |y|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 24))
                    (SPADCALL |x| (|getShellEntry| $ 20)))
                 |STAGG-;setelt;AUs2S;12|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 25))
                                  (- (SPADCALL |i|
                                      (|getShellEntry| $ 27))
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 20))))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 41))))
                               |STAGG-;setelt;AUs2S;12|)
                         (EXIT (COND
                                 ((< |h| |l|) |s|)
                                 ('T
                                  (SEQ (LETT |y|
                                        (SPADCALL |x|
                                         (PROG1
                                          (LETT #0# |l|
                                           |STAGG-;setelt;AUs2S;12|)
                                           (|check-subtype| (>= #0# 0)
                                            '(|NonNegativeInteger|)
                                            #0#))
                                         (|getShellEntry| $ 21))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LETT |z|
                                        (SPADCALL |y|
                                         (PROG1
                                          (LETT #1# (+ (- |h| |l|) 1)
                                           |STAGG-;setelt;AUs2S;12|)
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 21))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (NOT
                                             (SPADCALL |y| |z|
                                              (|getShellEntry| $ 42))))
                                           (GO G191)))
                                        (SEQ
                                         (SPADCALL |y| |s|
                                          (|getShellEntry| $ 36))
                                         (EXIT
                                          (LETT |y|
                                           (SPADCALL |y|
                                            (|getShellEntry| $ 12))
                                           |STAGG-;setelt;AUs2S;12|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT |s|))))))))))))) 

(DEFUN |STAGG-;concat!;3A;13| (|x| |y| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 17)) |y|)
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 44)) |y|
                   (|getShellEntry| $ 45))
               (EXIT |x|)))))) 

(DEFUN |StreamAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|StreamAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|StreamAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 51) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|shallowlyMutable|)
           (PROGN
             (|setShellEntry| $ 32
                 (CONS (|dispatchFunction| |STAGG-;concat;3A;7|) $))
             (|setShellEntry| $ 35
                 (CONS (|dispatchFunction| |STAGG-;concat;LA;8|) $))
             (|setShellEntry| $ 38
                 (CONS (|dispatchFunction| |STAGG-;map!;M2A;9|) $))
             (|setShellEntry| $ 39
                 (CONS (|dispatchFunction| |STAGG-;fill!;ASA;10|) $))
             (|setShellEntry| $ 40
                 (CONS (|dispatchFunction| |STAGG-;setelt;AI2S;11|) $))
             (|setShellEntry| $ 43
                 (CONS (|dispatchFunction| |STAGG-;setelt;AUs2S;12|) $))
             (|setShellEntry| $ 46
                 (CONS (|dispatchFunction| |STAGG-;concat!;3A;13|) $)))))
        $)))) 

(MAKEPROP '|StreamAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |cyclic?|)
             |STAGG-;explicitlyFinite?;AB;1|
             |STAGG-;possiblyInfinite?;AB;2| (5 . |rest|) (|List| 7)
             (10 . |construct|) (|NonNegativeInteger|)
             |STAGG-;first;ANniA;3| (15 . |empty?|) (20 . |first|)
             (|Integer|) (25 . |minIndex|) (30 . |rest|)
             |STAGG-;elt;AIS;5| (|UniversalSegment| 19) (36 . |lo|)
             (41 . |hasHi|) (46 . |copy|) (51 . |hi|) (56 . |empty|)
             (60 . |first|) |STAGG-;elt;AUsA;6| (66 . |concat!|)
             (72 . |concat|) (|List| $) (78 . |concat|) (83 . |concat|)
             (88 . |setfirst!|) (|Mapping| 7 7) (94 . |map!|)
             (100 . |fill!|) (106 . |setelt|) (113 . |maxIndex|)
             (118 . |eq?|) (124 . |setelt|) (131 . |tail|)
             (136 . |setrest!|) (142 . |concat!|) '"rest" '"last"
             '"first" '"value")
          '#(|setelt| 148 |possiblyInfinite?| 162 |map!| 167 |first|
             173 |fill!| 179 |explicitlyFinite?| 185 |elt| 190
             |concat!| 202 |concat| 208)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 46
                                '(1 6 8 0 9 1 6 0 0 12 1 6 0 13 14 1 6
                                  8 0 17 1 6 7 0 18 1 6 19 0 20 2 6 0 0
                                  15 21 1 23 19 0 24 1 23 8 0 25 1 6 0
                                  0 26 1 23 19 0 27 0 6 0 28 2 6 0 0 15
                                  29 2 6 0 0 0 31 2 0 0 0 0 32 1 6 0 33
                                  34 1 0 0 33 35 2 6 7 0 7 36 2 0 0 37
                                  0 38 2 0 0 0 7 39 3 0 7 0 19 7 40 1 6
                                  19 0 41 2 6 8 0 0 42 3 0 7 0 23 7 43
                                  1 6 0 0 44 2 6 0 0 0 45 2 0 0 0 0 46
                                  3 0 7 0 19 7 40 3 0 7 0 23 7 43 1 0 8
                                  0 11 2 0 0 37 0 38 2 0 0 0 15 16 2 0
                                  0 0 7 39 1 0 8 0 10 2 0 7 0 19 22 2 0
                                  0 0 23 30 2 0 0 0 0 46 1 0 0 33 35 2
                                  0 0 0 0 32)))))
          '|lookupComplete|)) 
