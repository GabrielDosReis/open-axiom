
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
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 9))
      (|getShellEntry| $ 10))) 

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
                                           (|getShellEntry| $ 13))
                                          |STAGG-;first;ANniA;3|)
                                         $)
                                        #0#)
                                       |STAGG-;first;ANniA;3|)))
                      (LETT |i| (QSADD1 |i|) |STAGG-;first;ANniA;3|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 15)))))) 

(DEFUN |STAGG-;c2| (|x| |r| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 18))
     (|error| "Index out of range"))
    ('T (SPADCALL |x| (|getShellEntry| $ 19))))) 

(DEFUN |STAGG-;elt;AIS;5| (|x| |i| $)
  (PROG (#0=#:G1413)
    (RETURN
      (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 21)))
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
                                (|getShellEntry| $ 22))
                            |STAGG-;elt;AIS;5|)
                      (|getShellEntry| $ 18)))
              (EXIT (|error| "index out of range"))))
           (EXIT (SPADCALL |x| (|getShellEntry| $ 19))))))) 

(DEFUN |STAGG-;elt;AUsA;6| (|x| |i| $)
  (PROG (|l| #0=#:G1417 |h| #1=#:G1419 #2=#:G1420)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 25))
                    (SPADCALL |x| (|getShellEntry| $ 21)))
                 |STAGG-;elt;AUsA;6|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ((NULL (SPADCALL |i| (|getShellEntry| $ 26)))
                    (SPADCALL
                        (SPADCALL |x|
                            (PROG1 (LETT #0# |l| |STAGG-;elt;AUsA;6|)
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            (|getShellEntry| $ 22))
                        (|getShellEntry| $ 27)))
                   ('T
                    (SEQ (LETT |h|
                               (- (SPADCALL |i| (|getShellEntry| $ 28))
                                  (SPADCALL |x| (|getShellEntry| $ 21)))
                               |STAGG-;elt;AUsA;6|)
                         (EXIT (COND
                                 ((< |h| |l|)
                                  (SPADCALL (|getShellEntry| $ 29)))
                                 ('T
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (PROG1
                                        (LETT #1# |l|
                                         |STAGG-;elt;AUsA;6|)
                                         (|check-subtype| (>= #1# 0)
                                          '(|NonNegativeInteger|) #1#))
                                       (|getShellEntry| $ 22))
                                      (PROG1
                                       (LETT #2# (+ (- |h| |l|) 1)
                                        |STAGG-;elt;AUsA;6|)
                                        (|check-subtype| (>= #2# 0)
                                         '(|NonNegativeInteger|) #2#))
                                      (|getShellEntry| $ 30))))))))))))) 

(DEFUN |STAGG-;concat;3A;7| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 27)) |y|
      (|getShellEntry| $ 32))) 

(DEFUN |STAGG-;concat;LA;8| (|l| $)
  (COND
    ((NULL |l|) (SPADCALL (|getShellEntry| $ 29)))
    ('T
     (SPADCALL (SPADCALL (|SPADfirst| |l|) (|getShellEntry| $ 27))
         (SPADCALL (CDR |l|) (|getShellEntry| $ 35))
         (|getShellEntry| $ 32))))) 

(DEFUN |STAGG-;map!;M2A;9| (|f| |l| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |l| |STAGG-;map!;M2A;9|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL
                             (SPADCALL |l| (|getShellEntry| $ 18))
                             (|getShellEntry| $ 10)))
                   (GO G191)))
                (SEQ (SPADCALL |l|
                         (SPADCALL
                             (SPADCALL |l| (|getShellEntry| $ 19)) |f|)
                         (|getShellEntry| $ 37))
                     (EXIT (LETT |l|
                                 (SPADCALL |l| (|getShellEntry| $ 13))
                                 |STAGG-;map!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |y|))))) 

(DEFUN |STAGG-;fill!;ASA;10| (|x| |s| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |x| |STAGG-;fill!;ASA;10|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL
                             (SPADCALL |y| (|getShellEntry| $ 18))
                             (|getShellEntry| $ 10)))
                   (GO G191)))
                (SEQ (SPADCALL |y| |s| (|getShellEntry| $ 37))
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 13))
                                 |STAGG-;fill!;ASA;10|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |STAGG-;setelt;AI2S;11| (|x| |i| |s| $)
  (PROG (#0=#:G1436)
    (RETURN
      (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 21)))
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
                                (|getShellEntry| $ 22))
                            |STAGG-;setelt;AI2S;11|)
                      (|getShellEntry| $ 18)))
              (EXIT (|error| "index out of range"))))
           (EXIT (SPADCALL |x| |s| (|getShellEntry| $ 37))))))) 

(DEFUN |STAGG-;setelt;AUs2S;12| (|x| |i| |s| $)
  (PROG (|l| |h| #0=#:G1441 #1=#:G1442 |z| |y|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 25))
                    (SPADCALL |x| (|getShellEntry| $ 21)))
                 |STAGG-;setelt;AUs2S;12|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 26))
                                  (- (SPADCALL |i|
                                      (|getShellEntry| $ 28))
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 21))))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 42))))
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
                                         (|getShellEntry| $ 22))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LETT |z|
                                        (SPADCALL |y|
                                         (PROG1
                                          (LETT #1# (+ (- |h| |l|) 1)
                                           |STAGG-;setelt;AUs2S;12|)
                                           (|check-subtype| (>= #1# 0)
                                            '(|NonNegativeInteger|)
                                            #1#))
                                         (|getShellEntry| $ 22))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (SPADCALL
                                             (SPADCALL |y| |z|
                                              (|getShellEntry| $ 43))
                                             (|getShellEntry| $ 10)))
                                           (GO G191)))
                                        (SEQ
                                         (SPADCALL |y| |s|
                                          (|getShellEntry| $ 37))
                                         (EXIT
                                          (LETT |y|
                                           (SPADCALL |y|
                                            (|getShellEntry| $ 13))
                                           |STAGG-;setelt;AUs2S;12|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT |s|))))))))))))) 

(DEFUN |STAGG-;concat!;3A;13| (|x| |y| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 18)) |y|)
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 45)) |y|
                   (|getShellEntry| $ 46))
               (EXIT |x|)))))) 

(DEFUN |StreamAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|StreamAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|StreamAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 52) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|shallowlyMutable|)
           (PROGN
             (|setShellEntry| $ 33
                 (CONS (|dispatchFunction| |STAGG-;concat;3A;7|) $))
             (|setShellEntry| $ 36
                 (CONS (|dispatchFunction| |STAGG-;concat;LA;8|) $))
             (|setShellEntry| $ 39
                 (CONS (|dispatchFunction| |STAGG-;map!;M2A;9|) $))
             (|setShellEntry| $ 40
                 (CONS (|dispatchFunction| |STAGG-;fill!;ASA;10|) $))
             (|setShellEntry| $ 41
                 (CONS (|dispatchFunction| |STAGG-;setelt;AI2S;11|) $))
             (|setShellEntry| $ 44
                 (CONS (|dispatchFunction| |STAGG-;setelt;AUs2S;12|) $))
             (|setShellEntry| $ 47
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

(SETQ |$CategoryFrame|
      (|put| '|StreamAggregate&| '|isFunctor|
             '(((|possiblyInfinite?| ((|Boolean|) $)) T (ELT $ 12))
               ((|explicitlyFinite?| ((|Boolean|) $)) T (ELT $ 11))
               ((|setelt| (|#2| $ (|Integer|) |#2|)) T (ELT $ 41))
               ((|elt| (|#2| $ (|Integer|) |#2|)) T (ELT $ NIL))
               ((|elt| (|#2| $ (|Integer|))) T (ELT $ 23))
               ((|fill!| ($ $ |#2|)) T (ELT $ 40))
               ((|concat| ($ $ |#2|)) T (ELT $ NIL))
               ((|concat| ($ (|List| $))) T (ELT $ 36))
               ((|elt| ($ $ (|UniversalSegment| (|Integer|)))) T
                (ELT $ 31))
               ((|setelt|
                    (|#2| $ (|UniversalSegment| (|Integer|)) |#2|))
                T (ELT $ 44))
               ((|setelt| (|#2| $ "last" |#2|)) T (ELT $ NIL))
               ((|setelt| ($ $ "rest" $)) T (ELT $ NIL))
               ((|setelt| (|#2| $ "first" |#2|)) T (ELT $ NIL))
               ((|concat!| ($ $ |#2|)) T (ELT $ NIL))
               ((|concat!| ($ $ $)) T (ELT $ 47))
               ((|elt| (|#2| $ "last")) T (ELT $ NIL))
               ((|elt| ($ $ "rest")) T (ELT $ NIL))
               ((|first| ($ $ (|NonNegativeInteger|))) T (ELT $ 17))
               ((|elt| (|#2| $ "first")) T (ELT $ NIL))
               ((|first| (|#2| $)) T (ELT $ NIL))
               ((|concat| ($ |#2| $)) T (ELT $ NIL))
               ((|concat| ($ $ $)) T (ELT $ 33))
               ((|setelt| (|#2| $ "value" |#2|)) T (ELT $ NIL))
               ((|elt| (|#2| $ "value")) T (ELT $ NIL))
               ((|map!| ($ (|Mapping| |#2| |#2|) $)) T (ELT $ 39)))
             (|addModemap| '|StreamAggregate&|
                 '(|StreamAggregate&| |#1| |#2|)
                 '((CATEGORY |domain|
                       (SIGNATURE |possiblyInfinite?|
                           ((|Boolean|) |#1|))
                       (SIGNATURE |explicitlyFinite?|
                           ((|Boolean|) |#1|))
                       (SIGNATURE |setelt|
                           (|#2| |#1| (|Integer|) |#2|))
                       (SIGNATURE |elt| (|#2| |#1| (|Integer|) |#2|))
                       (SIGNATURE |elt| (|#2| |#1| (|Integer|)))
                       (SIGNATURE |fill!| (|#1| |#1| |#2|))
                       (SIGNATURE |concat| (|#1| |#1| |#2|))
                       (SIGNATURE |concat| (|#1| (|List| |#1|)))
                       (SIGNATURE |elt|
                           (|#1| |#1| (|UniversalSegment| (|Integer|))))
                       (SIGNATURE |setelt|
                           (|#2| |#1| (|UniversalSegment| (|Integer|))
                                 |#2|))
                       (SIGNATURE |setelt| (|#2| |#1| "last" |#2|))
                       (SIGNATURE |setelt| (|#1| |#1| "rest" |#1|))
                       (SIGNATURE |setelt| (|#2| |#1| "first" |#2|))
                       (SIGNATURE |concat!| (|#1| |#1| |#2|))
                       (SIGNATURE |concat!| (|#1| |#1| |#1|))
                       (SIGNATURE |elt| (|#2| |#1| "last"))
                       (SIGNATURE |elt| (|#1| |#1| "rest"))
                       (SIGNATURE |first|
                           (|#1| |#1| (|NonNegativeInteger|)))
                       (SIGNATURE |elt| (|#2| |#1| "first"))
                       (SIGNATURE |first| (|#2| |#1|))
                       (SIGNATURE |concat| (|#1| |#2| |#1|))
                       (SIGNATURE |concat| (|#1| |#1| |#1|))
                       (SIGNATURE |setelt| (|#2| |#1| "value" |#2|))
                       (SIGNATURE |elt| (|#2| |#1| "value"))
                       (SIGNATURE |map!|
                           (|#1| (|Mapping| |#2| |#2|) |#1|)))
                   (|StreamAggregate| |#2|) (|Type|))
                 T '|StreamAggregate&|
                 (|put| '|StreamAggregate&| '|mode|
                        '(|Mapping|
                             (CATEGORY |domain|
                                 (SIGNATURE |possiblyInfinite?|
                                     ((|Boolean|) |#1|))
                                 (SIGNATURE |explicitlyFinite?|
                                     ((|Boolean|) |#1|))
                                 (SIGNATURE |setelt|
                                     (|#2| |#1| (|Integer|) |#2|))
                                 (SIGNATURE |elt|
                                     (|#2| |#1| (|Integer|) |#2|))
                                 (SIGNATURE |elt|
                                     (|#2| |#1| (|Integer|)))
                                 (SIGNATURE |fill!| (|#1| |#1| |#2|))
                                 (SIGNATURE |concat| (|#1| |#1| |#2|))
                                 (SIGNATURE |concat|
                                     (|#1| (|List| |#1|)))
                                 (SIGNATURE |elt|
                                     (|#1| |#1|
                                      (|UniversalSegment| (|Integer|))))
                                 (SIGNATURE |setelt|
                                     (|#2| |#1|
                                      (|UniversalSegment| (|Integer|))
                                      |#2|))
                                 (SIGNATURE |setelt|
                                     (|#2| |#1| "last" |#2|))
                                 (SIGNATURE |setelt|
                                     (|#1| |#1| "rest" |#1|))
                                 (SIGNATURE |setelt|
                                     (|#2| |#1| "first" |#2|))
                                 (SIGNATURE |concat!| (|#1| |#1| |#2|))
                                 (SIGNATURE |concat!| (|#1| |#1| |#1|))
                                 (SIGNATURE |elt| (|#2| |#1| "last"))
                                 (SIGNATURE |elt| (|#1| |#1| "rest"))
                                 (SIGNATURE |first|
                                     (|#1| |#1| (|NonNegativeInteger|)))
                                 (SIGNATURE |elt| (|#2| |#1| "first"))
                                 (SIGNATURE |first| (|#2| |#1|))
                                 (SIGNATURE |concat| (|#1| |#2| |#1|))
                                 (SIGNATURE |concat| (|#1| |#1| |#1|))
                                 (SIGNATURE |setelt|
                                     (|#2| |#1| "value" |#2|))
                                 (SIGNATURE |elt| (|#2| |#1| "value"))
                                 (SIGNATURE |map!|
                                     (|#1| (|Mapping| |#2| |#2|) |#1|)))
                             (|StreamAggregate| |#2|) (|Type|))
                        |$CategoryFrame|)))) 
