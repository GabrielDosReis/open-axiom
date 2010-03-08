
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
  (PROG (#0=#:G1446 |i|)
    (RETURN
      (SEQ (SPADCALL
               (PROGN
                 (LETT #0# NIL |STAGG-;first;ANniA;3|)
                 (SEQ (LETT |i| 1 |STAGG-;first;ANniA;3|) G190
                      (COND ((QSGREATERP |i| |n|) (GO G191)))
                      (LETT #0#
                            (CONS (|STAGG-;c2| |x|
                                      (LETT |x|
                                       (SPADCALL |x|
                                        (|getShellEntry| $ 17))
                                       |STAGG-;first;ANniA;3|)
                                      $)
                                  #0#)
                            |STAGG-;first;ANniA;3|)
                      (LETT |i| (QSADD1 |i|) |STAGG-;first;ANniA;3|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 19)))))) 

(DEFUN |STAGG-;c2| (|x| |r| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 21))
     (|error| "Index out of range"))
    ('T (SPADCALL |x| (|getShellEntry| $ 22))))) 

(DEFUN |STAGG-;elt;AIS;5| (|x| |i| $)
  (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 24)))
             |STAGG-;elt;AIS;5|)
       (COND
         ((OR (< |i| 0)
              (SPADCALL
                  (LETT |x|
                        (SPADCALL |x|
                            (PROG1 |i|
                              (|check-subtype| (>= |i| 0)
                                  '(|NonNegativeInteger|) |i|))
                            (|getShellEntry| $ 28))
                        |STAGG-;elt;AIS;5|)
                  (|getShellEntry| $ 21)))
          (EXIT (|error| "index out of range"))))
       (EXIT (SPADCALL |x| (|getShellEntry| $ 22))))) 

(DEFUN |STAGG-;elt;AUsA;6| (|x| |i| $)
  (PROG (|l| |h| #0=#:G1416)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 31))
                    (SPADCALL |x| (|getShellEntry| $ 24)))
                 |STAGG-;elt;AUsA;6|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ((NOT (SPADCALL |i| (|getShellEntry| $ 32)))
                    (SPADCALL
                        (SPADCALL |x|
                            (PROG1 |l|
                              (|check-subtype| (>= |l| 0)
                                  '(|NonNegativeInteger|) |l|))
                            (|getShellEntry| $ 28))
                        (|getShellEntry| $ 33)))
                   ('T
                    (SEQ (LETT |h|
                               (- (SPADCALL |i| (|getShellEntry| $ 34))
                                  (SPADCALL |x| (|getShellEntry| $ 24)))
                               |STAGG-;elt;AUsA;6|)
                         (EXIT (COND
                                 ((< |h| |l|)
                                  (SPADCALL (|getShellEntry| $ 35)))
                                 ('T
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (PROG1 |l|
                                         (|check-subtype| (>= |l| 0)
                                          '(|NonNegativeInteger|) |l|))
                                       (|getShellEntry| $ 28))
                                      (PROG1
                                       (LETT #0# (+ (- |h| |l|) 1)
                                        |STAGG-;elt;AUsA;6|)
                                        (|check-subtype| (>= #0# 0)
                                         '(|NonNegativeInteger|) #0#))
                                      (|getShellEntry| $ 37))))))))))))) 

(DEFUN |STAGG-;concat;3A;7| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 33)) |y|
      (|getShellEntry| $ 39))) 

(DEFUN |STAGG-;concat;LA;8| (|l| $)
  (COND
    ((NULL |l|) (SPADCALL (|getShellEntry| $ 35)))
    ('T
     (SPADCALL (SPADCALL (|SPADfirst| |l|) (|getShellEntry| $ 33))
         (SPADCALL (CDR |l|) (|getShellEntry| $ 46))
         (|getShellEntry| $ 39))))) 

(DEFUN |STAGG-;map!;M2A;9| (|f| |l| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |l| |STAGG-;map!;M2A;9|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |l| (|getShellEntry| $ 21))))
                   (GO G191)))
                (SEQ (SPADCALL |l|
                         (SPADCALL
                             (SPADCALL |l| (|getShellEntry| $ 22)) |f|)
                         (|getShellEntry| $ 48))
                     (EXIT (LETT |l|
                                 (SPADCALL |l| (|getShellEntry| $ 17))
                                 |STAGG-;map!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |y|))))) 

(DEFUN |STAGG-;fill!;ASA;10| (|x| |s| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |x| |STAGG-;fill!;ASA;10|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |y| (|getShellEntry| $ 21))))
                   (GO G191)))
                (SEQ (SPADCALL |y| |s| (|getShellEntry| $ 48))
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 17))
                                 |STAGG-;fill!;ASA;10|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |STAGG-;setelt;AI2S;11| (|x| |i| |s| $)
  (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 24)))
             |STAGG-;setelt;AI2S;11|)
       (COND
         ((OR (< |i| 0)
              (SPADCALL
                  (LETT |x|
                        (SPADCALL |x|
                            (PROG1 |i|
                              (|check-subtype| (>= |i| 0)
                                  '(|NonNegativeInteger|) |i|))
                            (|getShellEntry| $ 28))
                        |STAGG-;setelt;AI2S;11|)
                  (|getShellEntry| $ 21)))
          (EXIT (|error| "index out of range"))))
       (EXIT (SPADCALL |x| |s| (|getShellEntry| $ 48))))) 

(DEFUN |STAGG-;setelt;AUs2S;12| (|x| |i| |s| $)
  (PROG (|l| |h| #0=#:G1436 |z| |y|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 31))
                    (SPADCALL |x| (|getShellEntry| $ 24)))
                 |STAGG-;setelt;AUs2S;12|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 32))
                                  (- (SPADCALL |i|
                                      (|getShellEntry| $ 34))
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 24))))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 53))))
                               |STAGG-;setelt;AUs2S;12|)
                         (EXIT (COND
                                 ((< |h| |l|) |s|)
                                 ('T
                                  (SEQ (LETT |y|
                                        (SPADCALL |x|
                                         (PROG1 |l|
                                           (|check-subtype| (>= |l| 0)
                                            '(|NonNegativeInteger|)
                                            |l|))
                                         (|getShellEntry| $ 28))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LETT |z|
                                        (SPADCALL |y|
                                         (PROG1
                                          (LETT #0# (+ (- |h| |l|) 1)
                                           |STAGG-;setelt;AUs2S;12|)
                                           (|check-subtype| (>= #0# 0)
                                            '(|NonNegativeInteger|)
                                            #0#))
                                         (|getShellEntry| $ 28))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (NOT
                                             (SPADCALL |y| |z|
                                              (|getShellEntry| $ 54))))
                                           (GO G191)))
                                        (SEQ
                                         (SPADCALL |y| |s|
                                          (|getShellEntry| $ 48))
                                         (EXIT
                                          (LETT |y|
                                           (SPADCALL |y|
                                            (|getShellEntry| $ 17))
                                           |STAGG-;setelt;AUs2S;12|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT |s|))))))))))))) 

(DEFUN |STAGG-;concat!;3A;13| (|x| |y| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 21)) |y|)
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 56)) |y|
                   (|getShellEntry| $ 57))
               (EXIT |x|)))))) 

(DEFUN |StreamAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|StreamAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|StreamAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 63) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|shallowlyMutable|)
           (PROGN
             (|setShellEntry| $ 40
                 (CONS (|dispatchFunction| |STAGG-;concat;3A;7|) $))
             (|setShellEntry| $ 47
                 (CONS (|dispatchFunction| |STAGG-;concat;LA;8|) $))
             (|setShellEntry| $ 50
                 (CONS (|dispatchFunction| |STAGG-;map!;M2A;9|) $))
             (|setShellEntry| $ 51
                 (CONS (|dispatchFunction| |STAGG-;fill!;ASA;10|) $))
             (|setShellEntry| $ 52
                 (CONS (|dispatchFunction| |STAGG-;setelt;AI2S;11|) $))
             (|setShellEntry| $ 55
                 (CONS (|dispatchFunction| |STAGG-;setelt;AUs2S;12|) $))
             (|setShellEntry| $ 58
                 (CONS (|dispatchFunction| |STAGG-;concat!;3A;13|) $)))))
        $)))) 

(MAKEPROP '|StreamAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |cyclic?|) (5 . |not|)
             |STAGG-;explicitlyFinite?;AB;1|
             |STAGG-;possiblyInfinite?;AB;2| (|SingleInteger|)
             (10 . |One|) (|NonNegativeInteger|) (14 . |One|)
             (18 . |rest|) (|List| 7) (23 . |construct|)
             |STAGG-;first;ANniA;3| (28 . |empty?|) (33 . |first|)
             (|Integer|) (38 . |minIndex|) (43 . -) (49 . |Zero|)
             (53 . <) (59 . |rest|) |STAGG-;elt;AIS;5|
             (|UniversalSegment| 23) (65 . |lo|) (70 . |hasHi|)
             (75 . |copy|) (80 . |hi|) (85 . |empty|) (89 . +)
             (95 . |first|) |STAGG-;elt;AUsA;6| (101 . |concat!|)
             (107 . |concat|) (|List| 6) (113 . |empty?|)
             (118 . |first|) (123 . |rest|) (|List| $) (128 . |concat|)
             (133 . |concat|) (138 . |setfirst!|) (|Mapping| 7 7)
             (144 . |map!|) (150 . |fill!|) (156 . |setelt|)
             (163 . |maxIndex|) (168 . |eq?|) (174 . |setelt|)
             (181 . |tail|) (186 . |setrest!|) (192 . |concat!|)
             '"rest" '"last" '"first" '"value")
          '#(|setelt| 198 |possiblyInfinite?| 212 |map!| 217 |first|
             223 |fill!| 229 |explicitlyFinite?| 235 |elt| 240
             |concat!| 252 |concat| 258)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 58
                                '(1 6 8 0 9 1 8 0 0 10 0 13 0 14 0 15 0
                                  16 1 6 0 0 17 1 6 0 18 19 1 6 8 0 21
                                  1 6 7 0 22 1 6 23 0 24 2 23 0 0 0 25
                                  0 23 0 26 2 23 8 0 0 27 2 6 0 0 15 28
                                  1 30 23 0 31 1 30 8 0 32 1 6 0 0 33 1
                                  30 23 0 34 0 6 0 35 2 23 0 0 0 36 2 6
                                  0 0 15 37 2 6 0 0 0 39 2 0 0 0 0 40 1
                                  41 8 0 42 1 41 6 0 43 1 41 0 0 44 1 6
                                  0 45 46 1 0 0 45 47 2 6 7 0 7 48 2 0
                                  0 49 0 50 2 0 0 0 7 51 3 0 7 0 23 7
                                  52 1 6 23 0 53 2 6 8 0 0 54 3 0 7 0
                                  30 7 55 1 6 0 0 56 2 6 0 0 0 57 2 0 0
                                  0 0 58 3 0 7 0 23 7 52 3 0 7 0 30 7
                                  55 1 0 8 0 12 2 0 0 49 0 50 2 0 0 0
                                  15 20 2 0 0 0 7 51 1 0 8 0 11 2 0 7 0
                                  23 29 2 0 0 0 30 38 2 0 0 0 0 58 1 0
                                  0 45 47 2 0 0 0 0 40)))))
          '|lookupComplete|)) 
