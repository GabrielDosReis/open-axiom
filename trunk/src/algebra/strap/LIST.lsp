
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%List|) |LIST;nil;$;1|)) 

(PUT '|LIST;nil;$;1| '|SPADreplace| '(XLAM NIL NIL)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Boolean|)
                |LIST;null;$B;2|)) 

(PUT '|LIST;null;$B;2| '|SPADreplace| 'NULL) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%List|)
                |LIST;cons;S2$;3|)) 

(PUT '|LIST;cons;S2$;3| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%List|)
                |LIST;append;3$;4|)) 

(PUT '|LIST;append;3$;4| '|SPADreplace| 'APPEND) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Void|)
                |LIST;writeOMList|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%String|)
                |LIST;OMwrite;$S;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Boolean| |%Shell|) |%String|)
                |LIST;OMwrite;$BS;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Void|)
                |LIST;OMwrite;Omd$V;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Boolean| |%Shell|)
                    |%Void|)
                |LIST;OMwrite;Omd$BV;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%List|)
                |LIST;setUnion;3$;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%List|)
                |LIST;setIntersection;3$;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%List| |%Shell|) |%List|)
                |LIST;setDifference;3$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |LIST;convert;$If;13|)) 

(DEFUN |LIST;nil;$;1| ($) (DECLARE (IGNORE $)) NIL) 

(DEFUN |LIST;null;$B;2| (|l| $) (DECLARE (IGNORE $)) (NULL |l|)) 

(DEFUN |LIST;cons;S2$;3| (|s| |l| $)
  (DECLARE (IGNORE $))
  (CONS |s| |l|)) 

(DEFUN |LIST;append;3$;4| (|l| |t| $)
  (DECLARE (IGNORE $))
  (APPEND |l| |t|)) 

(DEFUN |LIST;writeOMList| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 16))
       (SPADCALL |dev| "list1" "list" (|getShellEntry| $ 18))
       (LOOP
         (COND
           ((NOT (NOT (NULL |x|))) (RETURN NIL))
           (T (SEQ (SPADCALL |dev|
                       (SPADCALL |x| (|getShellEntry| $ 20)) NIL
                       (|getShellEntry| $ 22))
                   (EXIT (SETQ |x|
                               (SPADCALL |x| (|getShellEntry| $ 23))))))))
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 24))))) 

(DEFUN |LIST;OMwrite;$S;6| (|x| $)
  (PROG (|s| |sp| |dev|)
    (RETURN
      (SEQ (LETT |s| "" |LIST;OMwrite;$S;6|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |LIST;OMwrite;$S;6|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 26))
                     (|getShellEntry| $ 27))
                 |LIST;OMwrite;$S;6|)
           (SPADCALL |dev| (|getShellEntry| $ 28))
           (|LIST;writeOMList| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 29))
           (SPADCALL |dev| (|getShellEntry| $ 30))
           (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|))))) 

(DEFUN |LIST;OMwrite;$BS;7| (|x| |wholeObj| $)
  (PROG (|s| |sp| |dev|)
    (RETURN
      (SEQ (LETT |s| "" |LIST;OMwrite;$BS;7|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |LIST;OMwrite;$BS;7|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 26))
                     (|getShellEntry| $ 27))
                 |LIST;OMwrite;$BS;7|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 28))))
           (|LIST;writeOMList| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 29))))
           (SPADCALL |dev| (|getShellEntry| $ 30))
           (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|))))) 

(DEFUN |LIST;OMwrite;Omd$V;8| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 28))
       (|LIST;writeOMList| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 29))))) 

(DEFUN |LIST;OMwrite;Omd$BV;9| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 28))))
       (|LIST;writeOMList| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 29))))))) 

(DEFUN |LIST;setUnion;3$;10| (|l1| |l2| $)
  (SPADCALL (SPADCALL |l1| |l2| (|getShellEntry| $ 35))
            (|getShellEntry| $ 36))) 

(DEFUN |LIST;setIntersection;3$;11| (|l1| |l2| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| (SPADCALL (|getShellEntry| $ 38))
                 |LIST;setIntersection;3$;11|)
           (SETQ |l1| (SPADCALL |l1| (|getShellEntry| $ 36)))
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |l1| (|getShellEntry| $ 39))))
                (RETURN NIL))
               (T (SEQ (COND
                         ((SPADCALL
                              (SPADCALL |l1| (|getShellEntry| $ 20))
                              |l2| (|getShellEntry| $ 40))
                          (SETQ |u|
                                (CONS (SPADCALL |l1|
                                       (|getShellEntry| $ 20))
                                      |u|))))
                       (EXIT (SETQ |l1|
                                   (SPADCALL |l1|
                                    (|getShellEntry| $ 23))))))))
           (EXIT |u|))))) 

(DEFUN |LIST;setDifference;3$;12| (|l1| |l2| $)
  (PROG (|lu| |l11|)
    (RETURN
      (SEQ (SETQ |l1| (SPADCALL |l1| (|getShellEntry| $ 36)))
           (LETT |lu| (SPADCALL (|getShellEntry| $ 38))
                 |LIST;setDifference;3$;12|)
           (LOOP
             (COND
               ((NOT (NOT (SPADCALL |l1| (|getShellEntry| $ 39))))
                (RETURN NIL))
               (T (SEQ (LETT |l11|
                             (SPADCALL |l1| 1 (|getShellEntry| $ 42))
                             |LIST;setDifference;3$;12|)
                       (COND
                         ((NOT (SPADCALL |l11| |l2|
                                   (|getShellEntry| $ 40)))
                          (SETQ |lu|
                                (SPADCALL |l11| |lu|
                                    (|getShellEntry| $ 43)))))
                       (EXIT (SETQ |l1|
                                   (SPADCALL |l1|
                                    (|getShellEntry| $ 23))))))))
           (EXIT |lu|))))) 

(DEFUN |LIST;convert;$If;13| (|x| $)
  (SPADCALL
      (CONS (SPADCALL '|construct| (|getShellEntry| $ 47))
            (LET ((#0=#:G1444 |x|) (#1=#:G1443 NIL))
              (LOOP
                (COND
                  ((ATOM #0#) (RETURN (NREVERSE #1#)))
                  (T (LET ((|a| (CAR #0#)))
                       (SETQ #1#
                             (CONS (SPADCALL |a|
                                    (|getShellEntry| $ 48))
                                   #1#)))))
                (SETQ #0# (CDR #0#)))))
      (|getShellEntry| $ 52))) 

(DEFUN |List| (#0=#:G1445)
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#1=#:G1446)
    (RETURN
      (COND
        ((SETQ #1#
               (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                   (HGET |$ConstructorCache| '|List|)
                   '|domainEqualList|))
         (|CDRwithIncrement| #1#))
        ('T
         (UNWIND-PROTECT
           (PROG1 (|List;| #0#) (SETQ #1# T))
           (COND ((NOT #1#) (HREM |$ConstructorCache| '|List|))))))))) 

(DEFUN |List;| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|)) (|dv$| (LIST '|List| |dv$1|))
         ($ (|newShell| 70))
         (|pv$| (|buildPredVector| 0 0
                    (LIST (OR (AND (|HasCategory| |#1| '(|OrderedSet|))
                                   (|HasCategory| |#1|
                                    (LIST '|Evalable|
                                     (|devaluate| |#1|))))
                              (AND (|HasCategory| |#1|
                                    '(|SetCategory|))
                                   (|HasCategory| |#1|
                                    (LIST '|Evalable|
                                     (|devaluate| |#1|)))))
                          (OR (AND (|HasCategory| |#1|
                                    '(|SetCategory|))
                                   (|HasCategory| |#1|
                                    (LIST '|Evalable|
                                     (|devaluate| |#1|))))
                              (|HasCategory| |#1|
                                  (LIST '|CoercibleTo| '(|OutputForm|))))
                          (|HasCategory| |#1|
                              (LIST '|ConvertibleTo| '(|InputForm|)))
                          (OR (|HasCategory| |#1| '(|OrderedSet|))
                              (|HasCategory| |#1| '(|SetCategory|)))
                          (|HasCategory| |#1| '(|OrderedSet|))
                          (|HasCategory| |#1| '(|OpenMath|))
                          (|HasCategory| (|Integer|) '(|OrderedSet|))
                          (|HasCategory| |#1| '(|SetCategory|))
                          (|HasCategory| |#1|
                              (LIST '|CoercibleTo| '(|OutputForm|)))
                          (AND (|HasCategory| |#1| '(|SetCategory|))
                               (|HasCategory| |#1|
                                   (LIST '|Evalable|
                                    (|devaluate| |#1|))))))))
    (DECLARE (SPECIAL |$ConstructorCache|))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|List| (LIST |dv$1|) (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 |#1|)
    (COND
      ((|testBitVector| |pv$| 6)
       (PROGN
         (|setShellEntry| $ 31
             (CONS (|dispatchFunction| |LIST;OMwrite;$S;6|) $))
         (|setShellEntry| $ 32
             (CONS (|dispatchFunction| |LIST;OMwrite;$BS;7|) $))
         (|setShellEntry| $ 33
             (CONS (|dispatchFunction| |LIST;OMwrite;Omd$V;8|) $))
         (|setShellEntry| $ 34
             (CONS (|dispatchFunction| |LIST;OMwrite;Omd$BV;9|) $)))))
    (COND
      ((|testBitVector| |pv$| 8)
       (PROGN
         (|setShellEntry| $ 37
             (CONS (|dispatchFunction| |LIST;setUnion;3$;10|) $))
         (|setShellEntry| $ 41
             (CONS (|dispatchFunction| |LIST;setIntersection;3$;11|) $))
         (|setShellEntry| $ 44
             (CONS (|dispatchFunction| |LIST;setDifference;3$;12|) $)))))
    (COND
      ((|testBitVector| |pv$| 3)
       (|setShellEntry| $ 53
           (CONS (|dispatchFunction| |LIST;convert;$If;13|) $))))
    $)) 

(MAKEPROP '|List| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|IndexedList| 6 (NRTEVAL 1))
             (|local| |#1|) (|Integer|) (0 . |One|)
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |LIST;nil;$;1|) $))
             (|Boolean|) |LIST;null;$B;2| |LIST;cons;S2$;3|
             |LIST;append;3$;4| (|Void|) (|OpenMathDevice|)
             (4 . |OMputApp|) (|String|) (9 . |OMputSymbol|)
             (16 . |not|) (21 . |first|) (26 . |false|)
             (30 . |OMwrite|) (37 . |rest|) (42 . |OMputEndApp|)
             (|OpenMathEncoding|) (47 . |OMencodingXML|)
             (51 . |OMopenString|) (57 . |OMputObject|)
             (62 . |OMputEndObject|) (67 . |OMclose|) (72 . |OMwrite|)
             (77 . |OMwrite|) (83 . |OMwrite|) (89 . |OMwrite|)
             (96 . |concat|) (102 . |removeDuplicates|)
             (107 . |setUnion|) (113 . |empty|) (117 . |empty?|)
             (122 . |member?|) (128 . |setIntersection|) (134 . |elt|)
             (140 . |concat|) (146 . |setDifference|) (|OutputForm|)
             (|InputForm|) (152 . |convert|) (157 . |convert|)
             (|List| 46) (162 . |concat|) (|List| $) (168 . |convert|)
             (173 . |convert|) (|Mapping| 6 6 6) (|NonNegativeInteger|)
             (|List| 6) (|Equation| 6) (|List| 57) (|Mapping| 10 6)
             (|Mapping| 10 6 6) (|UniversalSegment| 7) '"last" '"rest"
             '"first" '"value" (|Mapping| 6 6) (|SingleInteger|)
             (|List| 7) (|Union| 6 '"failed"))
          '#(|setUnion| 178 |setIntersection| 184 |setDifference| 190
             |rest| 196 |removeDuplicates| 201 |null| 206 |nil| 211
             |member?| 215 |first| 221 |empty?| 226 |empty| 231 |elt|
             235 |convert| 241 |cons| 246 |concat| 252 |append| 264
             |OMwrite| 270)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 6
                    '(0 0 0 0 0 0 0 0 0 0 0 5 0 0 0 1 4 0 1 2 3 4 6))
                (CONS '#(|ListAggregate&| |StreamAggregate&|
                         |ExtensibleLinearAggregate&|
                         |FiniteLinearAggregate&|
                         |UnaryRecursiveAggregate&| |LinearAggregate&|
                         |RecursiveAggregate&| |IndexedAggregate&|
                         |Collection&| |HomogeneousAggregate&|
                         |EltableAggregate&| |OrderedSet&| NIL
                         |Aggregate&| NIL |Evalable&| |SetCategory&|
                         NIL |InnerEvalable&| NIL NIL |BasicType&| NIL)
                      (CONS '#((|ListAggregate| 6)
                               (|StreamAggregate| 6)
                               (|ExtensibleLinearAggregate| 6)
                               (|FiniteLinearAggregate| 6)
                               (|UnaryRecursiveAggregate| 6)
                               (|LinearAggregate| 6)
                               (|RecursiveAggregate| 6)
                               (|IndexedAggregate| 7 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|EltableAggregate| 7 6) (|OrderedSet|)
                               (|Eltable| 61 $$) (|Aggregate|)
                               (|Eltable| 7 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|)
                               (|InnerEvalable| 6 6) (|CoercibleTo| 45)
                               (|ConvertibleTo| 46) (|BasicType|)
                               (|OpenMath|))
                            (|makeByteWordVec2| 53
                                '(0 7 0 8 1 15 14 0 16 3 15 14 0 17 17
                                  18 1 10 0 0 19 1 0 6 0 20 0 10 0 21 3
                                  6 14 15 0 10 22 1 0 0 0 23 1 15 14 0
                                  24 0 25 0 26 2 15 0 17 25 27 1 15 14
                                  0 28 1 15 14 0 29 1 15 14 0 30 1 0 17
                                  0 31 2 0 17 0 10 32 2 0 14 15 0 33 3
                                  0 14 15 0 10 34 2 0 0 0 0 35 1 0 0 0
                                  36 2 0 0 0 0 37 0 0 0 38 1 0 10 0 39
                                  2 0 10 6 0 40 2 0 0 0 0 41 2 0 6 0 7
                                  42 2 0 0 6 0 43 2 0 0 0 0 44 1 46 0
                                  45 47 1 6 46 0 48 2 49 0 46 0 50 1 46
                                  0 51 52 1 0 46 0 53 2 8 0 0 0 37 2 8
                                  0 0 0 41 2 8 0 0 0 44 1 0 0 0 23 1 8
                                  0 0 36 1 0 10 0 11 0 0 0 9 2 8 10 6 0
                                  40 1 0 6 0 20 1 0 10 0 39 0 0 0 38 2
                                  0 6 0 7 42 1 3 46 0 53 2 0 0 6 0 12 2
                                  0 0 0 0 35 2 0 0 6 0 43 2 0 0 0 0 13
                                  3 6 14 15 0 10 34 2 6 17 0 10 32 2 6
                                  14 15 0 33 1 6 17 0 31)))))
          '|lookupIncomplete|)) 
