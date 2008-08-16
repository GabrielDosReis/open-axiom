
(/VERSIONCHECK 2) 

(DEFUN |STAGG-;explicitlyFinite?;AB;1| (|x| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 9)) (QREFELT $ 10))) 

(DEFUN |STAGG-;possiblyInfinite?;AB;2| (|x| $)
  (SPADCALL |x| (QREFELT $ 9))) 

(DEFUN |STAGG-;first;ANniA;3| (|x| |n| $)
  (PROG (#0=#:G1411 |i|)
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
                                          (SPADCALL |x| (QREFELT $ 13))
                                          |STAGG-;first;ANniA;3|)
                                         $)
                                        #0#)
                                       |STAGG-;first;ANniA;3|)))
                      (LETT |i| (QSADD1 |i|) |STAGG-;first;ANniA;3|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               (QREFELT $ 15)))))) 

(DEFUN |STAGG-;c2| (|x| |r| $)
  (COND
    ((SPADCALL |x| (QREFELT $ 18)) (|error| "Index out of range"))
    ('T (SPADCALL |x| (QREFELT $ 19))))) 

(DEFUN |STAGG-;elt;AIS;5| (|x| |i| $)
  (PROG (#0=#:G1414)
    (RETURN
      (SEQ (LETT |i| (- |i| (SPADCALL |x| (QREFELT $ 21)))
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
                                (QREFELT $ 22))
                            |STAGG-;elt;AIS;5|)
                      (QREFELT $ 18)))
              (EXIT (|error| "index out of range"))))
           (EXIT (SPADCALL |x| (QREFELT $ 19))))))) 

(DEFUN |STAGG-;elt;AUsA;6| (|x| |i| $)
  (PROG (|l| #0=#:G1418 |h| #1=#:G1420 #2=#:G1421)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (QREFELT $ 25))
                    (SPADCALL |x| (QREFELT $ 21)))
                 |STAGG-;elt;AUsA;6|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ((NULL (SPADCALL |i| (QREFELT $ 26)))
                    (SPADCALL
                        (SPADCALL |x|
                            (PROG1 (LETT #0# |l| |STAGG-;elt;AUsA;6|)
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            (QREFELT $ 22))
                        (QREFELT $ 27)))
                   ('T
                    (SEQ (LETT |h|
                               (- (SPADCALL |i| (QREFELT $ 28))
                                  (SPADCALL |x| (QREFELT $ 21)))
                               |STAGG-;elt;AUsA;6|)
                         (EXIT (COND
                                 ((< |h| |l|)
                                  (SPADCALL (QREFELT $ 29)))
                                 ('T
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (PROG1
                                        (LETT #1# |l|
                                         |STAGG-;elt;AUsA;6|)
                                         (|check-subtype| (>= #1# 0)
                                          '(|NonNegativeInteger|) #1#))
                                       (QREFELT $ 22))
                                      (PROG1
                                       (LETT #2# (+ (- |h| |l|) 1)
                                        |STAGG-;elt;AUsA;6|)
                                        (|check-subtype| (>= #2# 0)
                                         '(|NonNegativeInteger|) #2#))
                                      (QREFELT $ 30))))))))))))) 

(DEFUN |STAGG-;concat;3A;7| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (QREFELT $ 27)) |y| (QREFELT $ 32))) 

(DEFUN |STAGG-;concat;LA;8| (|l| $)
  (COND
    ((NULL |l|) (SPADCALL (QREFELT $ 29)))
    ('T
     (SPADCALL (SPADCALL (|SPADfirst| |l|) (QREFELT $ 27))
         (SPADCALL (CDR |l|) (QREFELT $ 35)) (QREFELT $ 32))))) 

(DEFUN |STAGG-;map!;M2A;9| (|f| |l| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |l| |STAGG-;map!;M2A;9|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (SPADCALL |l| (QREFELT $ 18))
                             (QREFELT $ 10)))
                   (GO G191)))
                (SEQ (SPADCALL |l|
                         (SPADCALL (SPADCALL |l| (QREFELT $ 19)) |f|)
                         (QREFELT $ 37))
                     (EXIT (LETT |l| (SPADCALL |l| (QREFELT $ 13))
                                 |STAGG-;map!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |y|))))) 

(DEFUN |STAGG-;fill!;ASA;10| (|x| |s| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |x| |STAGG-;fill!;ASA;10|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (SPADCALL |y| (QREFELT $ 18))
                             (QREFELT $ 10)))
                   (GO G191)))
                (SEQ (SPADCALL |y| |s| (QREFELT $ 37))
                     (EXIT (LETT |y| (SPADCALL |y| (QREFELT $ 13))
                                 |STAGG-;fill!;ASA;10|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |STAGG-;setelt;AI2S;11| (|x| |i| |s| $)
  (PROG (#0=#:G1437)
    (RETURN
      (SEQ (LETT |i| (- |i| (SPADCALL |x| (QREFELT $ 21)))
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
                                (QREFELT $ 22))
                            |STAGG-;setelt;AI2S;11|)
                      (QREFELT $ 18)))
              (EXIT (|error| "index out of range"))))
           (EXIT (SPADCALL |x| |s| (QREFELT $ 37))))))) 

(DEFUN |STAGG-;setelt;AUs2S;12| (|x| |i| |s| $)
  (PROG (|l| |h| #0=#:G1442 #1=#:G1443 |z| |y|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (QREFELT $ 25))
                    (SPADCALL |x| (QREFELT $ 21)))
                 |STAGG-;setelt;AUs2S;12|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (QREFELT $ 26))
                                  (- (SPADCALL |i| (QREFELT $ 28))
                                     (SPADCALL |x| (QREFELT $ 21))))
                                 ('T (SPADCALL |x| (QREFELT $ 42))))
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
                                         (QREFELT $ 22))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LETT |z|
                                        (SPADCALL |y|
                                         (PROG1
                                          (LETT #1# (+ (- |h| |l|) 1)
                                           |STAGG-;setelt;AUs2S;12|)
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (QREFELT $ 22))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (SPADCALL
                                             (SPADCALL |y| |z|
                                              (QREFELT $ 43))
                                             (QREFELT $ 10)))
                                           (GO G191)))
                                        (SEQ
                                         (SPADCALL |y| |s|
                                          (QREFELT $ 37))
                                         (EXIT
                                          (LETT |y|
                                           (SPADCALL |y|
                                            (QREFELT $ 13))
                                           |STAGG-;setelt;AUs2S;12|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT |s|))))))))))))) 

(DEFUN |STAGG-;concat!;3A;13| (|x| |y| $)
  (SEQ (COND
         ((SPADCALL |x| (QREFELT $ 18)) |y|)
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (QREFELT $ 45)) |y|
                   (QREFELT $ 46))
               (EXIT |x|)))))) 

(DEFUN |StreamAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|StreamAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|StreamAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (GETREFV 52) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 |#1|)
        (QSETREFV $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|shallowlyMutable|)
           (PROGN
             (QSETREFV $ 33
                 (CONS (|dispatchFunction| |STAGG-;concat;3A;7|) $))
             (QSETREFV $ 36
                 (CONS (|dispatchFunction| |STAGG-;concat;LA;8|) $))
             (QSETREFV $ 39
                 (CONS (|dispatchFunction| |STAGG-;map!;M2A;9|) $))
             (QSETREFV $ 40
                 (CONS (|dispatchFunction| |STAGG-;fill!;ASA;10|) $))
             (QSETREFV $ 41
                 (CONS (|dispatchFunction| |STAGG-;setelt;AI2S;11|) $))
             (QSETREFV $ 44
                 (CONS (|dispatchFunction| |STAGG-;setelt;AUs2S;12|) $))
             (QSETREFV $ 47
                 (CONS (|dispatchFunction| |STAGG-;concat!;3A;13|) $)))))
        $)))) 

(MAKEPROP '|StreamAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |cyclic?|) (5 . |not|)
             |STAGG-;explicitlyFinite?;AB;1|
             |STAGG-;possiblyInfinite?;AB;2| (10 . |rest|) (|List| 7)
             (15 . |construct|) (|NonNegativeInteger|)
             |STAGG-;first;ANniA;3| (20 . |empty?|) (25 . |first|)
             (|Integer|) (30 . |minIndex|) (35 . |rest|)
             |STAGG-;elt;AIS;5| (|UniversalSegment| 20) (41 . |lo|)
             (46 . |hasHi|) (51 . |copy|) (56 . |hi|) (61 . |empty|)
             (65 . |first|) |STAGG-;elt;AUsA;6| (71 . |concat!|)
             (77 . |concat|) (|List| $) (83 . |concat|) (88 . |concat|)
             (93 . |setfirst!|) (|Mapping| 7 7) (99 . |map!|)
             (105 . |fill!|) (111 . |setelt|) (118 . |maxIndex|)
             (123 . |eq?|) (129 . |setelt|) (136 . |tail|)
             (141 . |setrest!|) (147 . |concat!|) '"rest" '"last"
             '"first" '"value")
          '#(|setelt| 153 |possiblyInfinite?| 167 |map!| 172 |first|
             178 |fill!| 184 |explicitlyFinite?| 190 |elt| 195
             |concat!| 207 |concat| 213)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 47
                                '(1 6 8 0 9 1 8 0 0 10 1 6 0 0 13 1 6 0
                                  14 15 1 6 8 0 18 1 6 7 0 19 1 6 20 0
                                  21 2 6 0 0 16 22 1 24 20 0 25 1 24 8
                                  0 26 1 6 0 0 27 1 24 20 0 28 0 6 0 29
                                  2 6 0 0 16 30 2 6 0 0 0 32 2 0 0 0 0
                                  33 1 6 0 34 35 1 0 0 34 36 2 6 7 0 7
                                  37 2 0 0 38 0 39 2 0 0 0 7 40 3 0 7 0
                                  20 7 41 1 6 20 0 42 2 6 8 0 0 43 3 0
                                  7 0 24 7 44 1 6 0 0 45 2 6 0 0 0 46 2
                                  0 0 0 0 47 3 0 7 0 20 7 41 3 0 7 0 24
                                  7 44 1 0 8 0 12 2 0 0 38 0 39 2 0 0 0
                                  16 17 2 0 0 0 7 40 1 0 8 0 11 2 0 7 0
                                  20 23 2 0 0 0 24 31 2 0 0 0 0 47 1 0
                                  0 34 36 2 0 0 0 0 33)))))
          '|lookupComplete|)) 
