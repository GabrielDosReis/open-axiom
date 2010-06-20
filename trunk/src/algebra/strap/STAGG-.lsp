
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
  (SPADCALL
      (LET ((|i| 1) (#0=#:G1447 NIL))
        (LOOP
          (COND
            ((> |i| |n|) (RETURN (NREVERSE #0#)))
            (T (SETQ #0#
                     (CONS (|STAGG-;c2| |x|
                               (LETT |x|
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 13))
                                     |STAGG-;first;ANniA;3|)
                               $)
                           #0#))))
          (SETQ |i| (+ |i| 1))))
      (|getShellEntry| $ 15))) 

(DEFUN |STAGG-;c2| (|x| |r| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 18))
     (|error| "Index out of range"))
    ('T (SPADCALL |x| (|getShellEntry| $ 19))))) 

(DEFUN |STAGG-;elt;AIS;5| (|x| |i| $)
  (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 21)))
             |STAGG-;elt;AIS;5|)
       (COND
         ((OR (< |i| 0)
              (SPADCALL
                  (LETT |x|
                        (SPADCALL |x|
                            (|check-subtype| (>= |i| 0)
                                '(|NonNegativeInteger|) |i|)
                            (|getShellEntry| $ 25))
                        |STAGG-;elt;AIS;5|)
                  (|getShellEntry| $ 18)))
          (EXIT (|error| "index out of range"))))
       (EXIT (SPADCALL |x| (|getShellEntry| $ 19))))) 

(DEFUN |STAGG-;elt;AUsA;6| (|x| |i| $)
  (PROG (|l| |h|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 28))
                    (SPADCALL |x| (|getShellEntry| $ 21)))
                 |STAGG-;elt;AUsA;6|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ((NOT (SPADCALL |i| (|getShellEntry| $ 29)))
                    (SPADCALL
                        (SPADCALL |x|
                            (|check-subtype| (>= |l| 0)
                                '(|NonNegativeInteger|) |l|)
                            (|getShellEntry| $ 25))
                        (|getShellEntry| $ 30)))
                   ('T
                    (SEQ (LETT |h|
                               (- (SPADCALL |i| (|getShellEntry| $ 31))
                                  (SPADCALL |x| (|getShellEntry| $ 21)))
                               |STAGG-;elt;AUsA;6|)
                         (EXIT (COND
                                 ((< |h| |l|)
                                  (SPADCALL (|getShellEntry| $ 32)))
                                 ('T
                                  (SPADCALL
                                      (SPADCALL |x|
                                       (|check-subtype| (>= |l| 0)
                                        '(|NonNegativeInteger|) |l|)
                                       (|getShellEntry| $ 25))
                                      (LET
                                       ((#0=#:G1420 (+ (- |h| |l|) 1)))
                                        (|check-subtype| (>= #0# 0)
                                         '(|NonNegativeInteger|) #0#))
                                      (|getShellEntry| $ 35))))))))))))) 

(DEFUN |STAGG-;concat;3A;7| (|x| |y| $)
  (SPADCALL (SPADCALL |x| (|getShellEntry| $ 30)) |y|
      (|getShellEntry| $ 37))) 

(DEFUN |STAGG-;concat;LA;8| (|l| $)
  (COND
    ((NULL |l|) (SPADCALL (|getShellEntry| $ 32)))
    ('T
     (SPADCALL (SPADCALL (|SPADfirst| |l|) (|getShellEntry| $ 30))
         (SPADCALL (CDR |l|) (|getShellEntry| $ 44))
         (|getShellEntry| $ 37))))) 

(DEFUN |STAGG-;map!;M2A;9| (|f| |l| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |l| |STAGG-;map!;M2A;9|)
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |l| (|getShellEntry| $ 18))))
                (RETURN NIL))
               (T (SEQ (SPADCALL |l|
                           (SPADCALL
                               (SPADCALL |l| (|getShellEntry| $ 19))
                               |f|)
                           (|getShellEntry| $ 46))
                       (EXIT (LETT |l|
                                   (SPADCALL |l|
                                    (|getShellEntry| $ 13))
                                   |STAGG-;map!;M2A;9|))))))
           (EXIT |y|))))) 

(DEFUN |STAGG-;fill!;ASA;10| (|x| |s| $)
  (PROG (|y|)
    (RETURN
      (SEQ (LETT |y| |x| |STAGG-;fill!;ASA;10|)
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |y| (|getShellEntry| $ 18))))
                (RETURN NIL))
               (T (SEQ (SPADCALL |y| |s| (|getShellEntry| $ 46))
                       (EXIT (LETT |y|
                                   (SPADCALL |y|
                                    (|getShellEntry| $ 13))
                                   |STAGG-;fill!;ASA;10|))))))
           (EXIT |x|))))) 

(DEFUN |STAGG-;setelt;AI2S;11| (|x| |i| |s| $)
  (SEQ (LETT |i| (- |i| (SPADCALL |x| (|getShellEntry| $ 21)))
             |STAGG-;setelt;AI2S;11|)
       (COND
         ((OR (< |i| 0)
              (SPADCALL
                  (LETT |x|
                        (SPADCALL |x|
                            (|check-subtype| (>= |i| 0)
                                '(|NonNegativeInteger|) |i|)
                            (|getShellEntry| $ 25))
                        |STAGG-;setelt;AI2S;11|)
                  (|getShellEntry| $ 18)))
          (EXIT (|error| "index out of range"))))
       (EXIT (SPADCALL |x| |s| (|getShellEntry| $ 46))))) 

(DEFUN |STAGG-;setelt;AUs2S;12| (|x| |i| |s| $)
  (PROG (|l| |h| |z| |y|)
    (RETURN
      (SEQ (LETT |l|
                 (- (SPADCALL |i| (|getShellEntry| $ 28))
                    (SPADCALL |x| (|getShellEntry| $ 21)))
                 |STAGG-;setelt;AUs2S;12|)
           (EXIT (COND
                   ((< |l| 0) (|error| "index out of range"))
                   ('T
                    (SEQ (LETT |h|
                               (COND
                                 ((SPADCALL |i| (|getShellEntry| $ 29))
                                  (- (SPADCALL |i|
                                      (|getShellEntry| $ 31))
                                     (SPADCALL |x|
                                      (|getShellEntry| $ 21))))
                                 ('T
                                  (SPADCALL |x| (|getShellEntry| $ 51))))
                               |STAGG-;setelt;AUs2S;12|)
                         (EXIT (COND
                                 ((< |h| |l|) |s|)
                                 ('T
                                  (SEQ (LETT |y|
                                        (SPADCALL |x|
                                         (|check-subtype| (>= |l| 0)
                                          '(|NonNegativeInteger|) |l|)
                                         (|getShellEntry| $ 25))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LETT |z|
                                        (SPADCALL |y|
                                         (LET
                                          ((#0=#:G1443
                                            (+ (- |h| |l|) 1)))
                                           (|check-subtype| (>= #0# 0)
                                            '(|NonNegativeInteger|)
                                            #0#))
                                         (|getShellEntry| $ 25))
                                        |STAGG-;setelt;AUs2S;12|)
                                       (LOOP
                                         (COND
                                           ((NOT
                                             (NOT
                                              (SPADCALL |y| |z|
                                               (|getShellEntry| $ 52))))
                                            (RETURN NIL))
                                           (T
                                            (SEQ
                                             (SPADCALL |y| |s|
                                              (|getShellEntry| $ 46))
                                             (EXIT
                                              (LETT |y|
                                               (SPADCALL |y|
                                                (|getShellEntry| $ 13))
                                               |STAGG-;setelt;AUs2S;12|))))))
                                       (EXIT |s|))))))))))))) 

(DEFUN |STAGG-;concat!;3A;13| (|x| |y| $)
  (SEQ (COND
         ((SPADCALL |x| (|getShellEntry| $ 18)) |y|)
         ('T
          (SEQ (SPADCALL (SPADCALL |x| (|getShellEntry| $ 54)) |y|
                   (|getShellEntry| $ 55))
               (EXIT |x|)))))) 

(DEFUN |StreamAggregate&| (|#1| |#2|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$2| (|devaluate| |#2|))
         (|dv$| (LIST '|StreamAggregate&| |dv$1| |dv$2|))
         ($ (|newShell| 61)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (|setShellEntry| $ 7 |#2|)
    (COND
      ((|HasAttribute| |#1| '|shallowlyMutable|)
       (PROGN
         (|setShellEntry| $ 38
             (CONS (|dispatchFunction| |STAGG-;concat;3A;7|) $))
         (|setShellEntry| $ 45
             (CONS (|dispatchFunction| |STAGG-;concat;LA;8|) $))
         (|setShellEntry| $ 48
             (CONS (|dispatchFunction| |STAGG-;map!;M2A;9|) $))
         (|setShellEntry| $ 49
             (CONS (|dispatchFunction| |STAGG-;fill!;ASA;10|) $))
         (|setShellEntry| $ 50
             (CONS (|dispatchFunction| |STAGG-;setelt;AI2S;11|) $))
         (|setShellEntry| $ 53
             (CONS (|dispatchFunction| |STAGG-;setelt;AUs2S;12|) $))
         (|setShellEntry| $ 56
             (CONS (|dispatchFunction| |STAGG-;concat!;3A;13|) $)))))
    $)) 

(MAKEPROP '|StreamAggregate&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (|local| |#2|)
             (|Boolean|) (0 . |cyclic?|) (5 . |not|)
             |STAGG-;explicitlyFinite?;AB;1|
             |STAGG-;possiblyInfinite?;AB;2| (10 . |rest|) (|List| 7)
             (15 . |construct|) (|NonNegativeInteger|)
             |STAGG-;first;ANniA;3| (20 . |empty?|) (25 . |first|)
             (|Integer|) (30 . |minIndex|) (35 . -) (41 . |Zero|)
             (45 . <) (51 . |rest|) |STAGG-;elt;AIS;5|
             (|UniversalSegment| 20) (57 . |lo|) (62 . |hasHi|)
             (67 . |copy|) (72 . |hi|) (77 . |empty|) (81 . |One|)
             (85 . +) (91 . |first|) |STAGG-;elt;AUsA;6|
             (97 . |concat!|) (103 . |concat|) (|List| 6)
             (109 . |empty?|) (114 . |first|) (119 . |rest|) (|List| $)
             (124 . |concat|) (129 . |concat|) (134 . |setfirst!|)
             (|Mapping| 7 7) (140 . |map!|) (146 . |fill!|)
             (152 . |setelt|) (159 . |maxIndex|) (164 . |eq?|)
             (170 . |setelt|) (177 . |tail|) (182 . |setrest!|)
             (188 . |concat!|) '"rest" '"last" '"first" '"value")
          '#(|setelt| 194 |possiblyInfinite?| 208 |map!| 213 |first|
             219 |fill!| 225 |explicitlyFinite?| 231 |elt| 236
             |concat!| 248 |concat| 254)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 56
                                '(1 6 8 0 9 1 8 0 0 10 1 6 0 0 13 1 6 0
                                  14 15 1 6 8 0 18 1 6 7 0 19 1 6 20 0
                                  21 2 20 0 0 0 22 0 20 0 23 2 20 8 0 0
                                  24 2 6 0 0 16 25 1 27 20 0 28 1 27 8
                                  0 29 1 6 0 0 30 1 27 20 0 31 0 6 0 32
                                  0 16 0 33 2 20 0 0 0 34 2 6 0 0 16 35
                                  2 6 0 0 0 37 2 0 0 0 0 38 1 39 8 0 40
                                  1 39 6 0 41 1 39 0 0 42 1 6 0 43 44 1
                                  0 0 43 45 2 6 7 0 7 46 2 0 0 47 0 48
                                  2 0 0 0 7 49 3 0 7 0 20 7 50 1 6 20 0
                                  51 2 6 8 0 0 52 3 0 7 0 27 7 53 1 6 0
                                  0 54 2 6 0 0 0 55 2 0 0 0 0 56 3 0 7
                                  0 20 7 50 3 0 7 0 27 7 53 1 0 8 0 12
                                  2 0 0 47 0 48 2 0 0 0 16 17 2 0 0 0 7
                                  49 1 0 8 0 11 2 0 7 0 20 26 2 0 0 0
                                  27 36 2 0 0 0 0 56 1 0 0 43 45 2 0 0
                                  0 0 38)))))
          '|lookupComplete|)) 
