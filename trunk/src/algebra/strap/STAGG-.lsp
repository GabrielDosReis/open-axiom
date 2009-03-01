
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
  (PROG (#0=#:G1448 |i|)
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
                                           (|getShellEntry| $ 16))
                                          |STAGG-;first;ANniA;3|)
                                         $)
                                        #0#)
                                       |STAGG-;first;ANniA;3|)))
                      (LETT |i| (QSADD1 |i|) |STAGG-;first;ANniA;3|)
                      (GO G190) G191 (EXIT (NREVERSE0 #0#))))
               (|getShellEntry| $ 18)))))) 

(DEFUN |STAGG-;c2| (|x| |r| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 20))
     (|error| "Index out of range"))
    ('T (SPADCALL |x| (|getShellEntry| $ 21))))) 

(DEFUN |STAGG-;elt;AIS;5| (|x| |i| $)
  (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 23)))
             |STAGG-;elt;AIS;5|)
       (COND
         ((OR (< |i| 0)
              (SPADCALL
                  (LETT |x|
                        (SPADCALL |x|
                            (PROG1 |i|
                              (|check-subtype|
                                  (COND ((< |i| 0) 'NIL) ('T 'T))
                                  '(|NonNegativeInteger|) |i|))
                            (|getShellEntry| $ 27))
                        |STAGG-;elt;AIS;5|)
                  (|getShellEntry| $ 20)))
          (EXIT (|error| "index out of range"))))
       (EXIT (SPADCALL |x| (|getShellEntry| $ 21))))) 

(DEFUN |STAGG-;elt;AUsA;6| (|x| |i| $)
  (PROG (|l| |h| #0=#:G1418)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 30))
                    (SPADCALL |x| (|getShellEntry| $ 23)))
                 |STAGG-;elt;AUsA;6|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ((NULL (SPADCALL |i| (|getShellEntry| $ 31)))
                    (SPADCALL
                        (SPADCALL |x|
                            (PROG1 |l|
                              (|check-subtype|
                                  (COND ((< |l| 0) 'NIL) ('T 'T))
                                  '(|NonNegativeInteger|) |l|))
                            (|getShellEntry| $ 27))
                        (|getShellEntry| $ 32)))
                   ('T
                    (SEQ (LETT |h|
                               (- (SPADCALL |i| (|getShellEntry| $ 33))
                                  (SPADCALL |x| (|getShellEntry| $ 23)))
                               |STAGG-;elt;AUsA;6|)
                         (EXIT (COND
                                 ((< |h| |l|)
                                  (SPADCALL (|getShellEntry| $ 34)))
                                 ('T
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (PROG1 |l|
                                         (|check-subtype|
                                          (COND
                                            ((< |l| 0) 'NIL)
                                            ('T 'T))
                                          '(|NonNegativeInteger|) |l|))
                                       (|getShellEntry| $ 27))
                                      (PROG1
                                       (LETT #0# (+ (- |h| |l|) 1)
                                        |STAGG-;elt;AUsA;6|)
                                        (|check-subtype|
                                         (COND
                                           ((< #0# 0) 'NIL)
                                           ('T 'T))
                                         '(|NonNegativeInteger|) #0#))
                                      (|getShellEntry| $ 36))))))))))))) 

(DEFUN |STAGG-;concat;3A;7| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 32)) |y|
      (|getShellEntry| $ 38))) 

(DEFUN |STAGG-;concat;LA;8| (|l| $)
  (COND
    ((NULL |l|) (SPADCALL (|getShellEntry| $ 34)))
    ('T
     (SPADCALL (SPADCALL (|SPADfirst| |l|) (|getShellEntry| $ 32))
         (SPADCALL (CDR |l|) (|getShellEntry| $ 45))
         (|getShellEntry| $ 38))))) 

(DEFUN |STAGG-;map!;M2A;9| (|f| |l| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |l| |STAGG-;map!;M2A;9|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |l| (|getShellEntry| $ 20))))
                   (GO G191)))
                (SEQ (SPADCALL |l|
                         (SPADCALL
                             (SPADCALL |l| (|getShellEntry| $ 21)) |f|)
                         (|getShellEntry| $ 47))
                     (EXIT (LETT |l|
                                 (SPADCALL |l| (|getShellEntry| $ 16))
                                 |STAGG-;map!;M2A;9|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |y|))))) 

(DEFUN |STAGG-;fill!;ASA;10| (|x| |s| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |x| |STAGG-;fill!;ASA;10|)
           (SEQ G190
                (COND
                  ((NULL (NOT (SPADCALL |y| (|getShellEntry| $ 20))))
                   (GO G191)))
                (SEQ (SPADCALL |y| |s| (|getShellEntry| $ 47))
                     (EXIT (LETT |y|
                                 (SPADCALL |y| (|getShellEntry| $ 16))
                                 |STAGG-;fill!;ASA;10|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |x|))))) 

(DEFUN |STAGG-;setelt;AI2S;11| (|x| |i| |s| $)
  (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 23)))
             |STAGG-;setelt;AI2S;11|)
       (COND
         ((OR (< |i| 0)
              (SPADCALL
                  (LETT |x|
                        (SPADCALL |x|
                            (PROG1 |i|
                              (|check-subtype|
                                  (COND ((< |i| 0) 'NIL) ('T 'T))
                                  '(|NonNegativeInteger|) |i|))
                            (|getShellEntry| $ 27))
                        |STAGG-;setelt;AI2S;11|)
                  (|getShellEntry| $ 20)))
          (EXIT (|error| "index out of range"))))
       (EXIT (SPADCALL |x| |s| (|getShellEntry| $ 47))))) 

(DEFUN |STAGG-;setelt;AUs2S;12| (|x| |i| |s| $)
  (PROG (|l| |h| #0=#:G1438 |z| |y|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 30))
                    (SPADCALL |x| (|getShellEntry| $ 23)))
                 |STAGG-;setelt;AUs2S;12|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 31))
                                  (- (SPADCALL |i|
                                      (|getShellEntry| $ 33))
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 23))))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 52))))
                               |STAGG-;setelt;AUs2S;12|)
                         (EXIT (COND
                                 ((< |h| |l|) |s|)
                                 ('T
                                  (SEQ (LETT |y|
                                        (SPADCALL |x|
                                         (PROG1 |l|
                                           (|check-subtype|
                                            (COND
                                              ((< |l| 0) 'NIL)
                                              ('T 'T))
                                            '(|NonNegativeInteger|)
                                            |l|))
                                         (|getShellEntry| $ 27))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LETT |z|
                                        (SPADCALL |y|
                                         (PROG1
                                          (LETT #0# (+ (- |h| |l|) 1)
                                           |STAGG-;setelt;AUs2S;12|)
                                           (|check-subtype|
                                            (COND
                                              ((< #0# 0) 'NIL)
                                              ('T 'T))
                                            '(|NonNegativeInteger|)
                                            #0#))
                                         (|getShellEntry| $ 27))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (SEQ G190
                                        (COND
                                          ((NULL
                                            (NOT
                                             (SPADCALL |y| |z|
                                              (|getShellEntry| $ 53))))
                                           (GO G191)))
                                        (SEQ
                                         (SPADCALL |y| |s|
                                          (|getShellEntry| $ 47))
                                         (EXIT
                                          (LETT |y|
                                           (SPADCALL |y|
                                            (|getShellEntry| $ 16))
                                           |STAGG-;setelt;AUs2S;12|)))
                                        NIL (GO G190) G191 (EXIT NIL))
                                       (EXIT |s|))))))))))))) 

(DEFUN |STAGG-;concat!;3A;13| (|x| |y| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 20)) |y|)
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 55)) |y|
                   (|getShellEntry| $ 56))
               (EXIT |x|)))))) 

(DEFUN |StreamAggregate&| (|#1| |#2|)
  (PROG (|dv$1| |dv$2| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|StreamAggregate&|))
        (LETT |dv$2| (|devaluate| |#2|) . #0#)
        (LETT |dv$| (LIST '|StreamAggregate&| |dv$1| |dv$2|) . #0#)
        (LETT $ (|newShell| 62) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 |#2|)
        (COND
          ((|HasAttribute| |#1| '|shallowlyMutable|)
           (PROGN
             (|setShellEntry| $ 39
                 (CONS (|dispatchFunction| |STAGG-;concat;3A;7|) $))
             (|setShellEntry| $ 46
                 (CONS (|dispatchFunction| |STAGG-;concat;LA;8|) $))
             (|setShellEntry| $ 49
                 (CONS (|dispatchFunction| |STAGG-;map!;M2A;9|) $))
             (|setShellEntry| $ 50
                 (CONS (|dispatchFunction| |STAGG-;fill!;ASA;10|) $))
             (|setShellEntry| $ 51
                 (CONS (|dispatchFunction| |STAGG-;setelt;AI2S;11|) $))
             (|setShellEntry| $ 54
                 (CONS (|dispatchFunction| |STAGG-;setelt;AUs2S;12|) $))
             (|setShellEntry| $ 57
                 (CONS (|dispatchFunction| |STAGG-;concat!;3A;13|) $)))))
        $)))) 

(MAKEPROP '|StreamAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |cyclic?|)
             |STAGG-;explicitlyFinite?;AB;1|
             |STAGG-;possiblyInfinite?;AB;2| (|SingleInteger|)
             (5 . |One|) (|NonNegativeInteger|) (9 . |One|)
             (13 . |rest|) (|List| 7) (18 . |construct|)
             |STAGG-;first;ANniA;3| (23 . |empty?|) (28 . |first|)
             (|Integer|) (33 . |minIndex|) (38 . -) (44 . |Zero|)
             (48 . <) (54 . |rest|) |STAGG-;elt;AIS;5|
             (|UniversalSegment| 22) (60 . |lo|) (65 . |hasHi|)
             (70 . |copy|) (75 . |hi|) (80 . |empty|) (84 . +)
             (90 . |first|) |STAGG-;elt;AUsA;6| (96 . |concat!|)
             (102 . |concat|) (|List| 6) (108 . |empty?|)
             (113 . |first|) (118 . |rest|) (|List| $) (123 . |concat|)
             (128 . |concat|) (133 . |setfirst!|) (|Mapping| 7 7)
             (139 . |map!|) (145 . |fill!|) (151 . |setelt|)
             (158 . |maxIndex|) (163 . |eq?|) (169 . |setelt|)
             (176 . |tail|) (181 . |setrest!|) (187 . |concat!|)
             '"rest" '"last" '"first" '"value")
          '#(|setelt| 193 |possiblyInfinite?| 207 |map!| 212 |first|
             218 |fill!| 224 |explicitlyFinite?| 230 |elt| 235
             |concat!| 247 |concat| 253)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 57
                                '(1 6 8 0 9 0 12 0 13 0 14 0 15 1 6 0 0
                                  16 1 6 0 17 18 1 6 8 0 20 1 6 7 0 21
                                  1 6 22 0 23 2 22 0 0 0 24 0 22 0 25 2
                                  22 8 0 0 26 2 6 0 0 14 27 1 29 22 0
                                  30 1 29 8 0 31 1 6 0 0 32 1 29 22 0
                                  33 0 6 0 34 2 22 0 0 0 35 2 6 0 0 14
                                  36 2 6 0 0 0 38 2 0 0 0 0 39 1 40 8 0
                                  41 1 40 6 0 42 1 40 0 0 43 1 6 0 44
                                  45 1 0 0 44 46 2 6 7 0 7 47 2 0 0 48
                                  0 49 2 0 0 0 7 50 3 0 7 0 22 7 51 1 6
                                  22 0 52 2 6 8 0 0 53 3 0 7 0 29 7 54
                                  1 6 0 0 55 2 6 0 0 0 56 2 0 0 0 0 57
                                  3 0 7 0 22 7 51 3 0 7 0 29 7 54 1 0 8
                                  0 11 2 0 0 48 0 49 2 0 0 0 14 19 2 0
                                  0 0 7 50 1 0 8 0 10 2 0 7 0 22 28 2 0
                                  0 0 29 37 2 0 0 0 0 57 1 0 0 44 46 2
                                  0 0 0 0 39)))))
          '|lookupComplete|)) 
