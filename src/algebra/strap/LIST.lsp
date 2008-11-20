
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
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 14))
       (SPADCALL |dev| "list1" "list" (|getShellEntry| $ 16))
       (SEQ G190
            (COND
              ((NULL (SPADCALL (NULL |x|) (|getShellEntry| $ 17)))
               (GO G191)))
            (SEQ (SPADCALL |dev| (|SPADfirst| |x|) 'NIL
                     (|getShellEntry| $ 18))
                 (EXIT (LETT |x| (CDR |x|) |LIST;writeOMList|)))
            NIL (GO G190) G191 (EXIT NIL))
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 19))))) 

(DEFUN |LIST;OMwrite;$S;6| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |LIST;OMwrite;$S;6|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |LIST;OMwrite;$S;6|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                     (|getShellEntry| $ 22))
                 |LIST;OMwrite;$S;6|)
           (SPADCALL |dev| (|getShellEntry| $ 23))
           (|LIST;writeOMList| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 24))
           (SPADCALL |dev| (|getShellEntry| $ 25))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |LIST;OMwrite;$S;6|)
           (EXIT |s|))))) 

(DEFUN |LIST;OMwrite;$BS;7| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |LIST;OMwrite;$BS;7|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |LIST;OMwrite;$BS;7|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                     (|getShellEntry| $ 22))
                 |LIST;OMwrite;$BS;7|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
           (|LIST;writeOMList| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))
           (SPADCALL |dev| (|getShellEntry| $ 25))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |LIST;OMwrite;$BS;7|)
           (EXIT |s|))))) 

(DEFUN |LIST;OMwrite;Omd$V;8| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 23))
       (|LIST;writeOMList| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 24))))) 

(DEFUN |LIST;OMwrite;Omd$BV;9| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
       (|LIST;writeOMList| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))))) 

(DEFUN |LIST;setUnion;3$;10| (|l1| |l2| $)
  (SPADCALL (SPADCALL |l1| |l2| (|getShellEntry| $ 30))
            (|getShellEntry| $ 31))) 

(DEFUN |LIST;setIntersection;3$;11| (|l1| |l2| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| NIL |LIST;setIntersection;3$;11|)
           (LETT |l1| (SPADCALL |l1| (|getShellEntry| $ 31))
                 |LIST;setIntersection;3$;11|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |l1|) (|getShellEntry| $ 17)))
                   (GO G191)))
                (SEQ (COND
                       ((SPADCALL (|SPADfirst| |l1|) |l2|
                            (|getShellEntry| $ 33))
                        (LETT |u| (CONS (|SPADfirst| |l1|) |u|)
                              |LIST;setIntersection;3$;11|)))
                     (EXIT (LETT |l1| (CDR |l1|)
                                 |LIST;setIntersection;3$;11|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |u|))))) 

(DEFUN |LIST;setDifference;3$;12| (|l1| |l2| $)
  (PROG (|l11| |lu|)
    (RETURN
      (SEQ (LETT |l1| (SPADCALL |l1| (|getShellEntry| $ 31))
                 |LIST;setDifference;3$;12|)
           (LETT |lu| NIL |LIST;setDifference;3$;12|)
           (SEQ G190
                (COND
                  ((NULL (SPADCALL (NULL |l1|) (|getShellEntry| $ 17)))
                   (GO G191)))
                (SEQ (LETT |l11|
                           (SPADCALL |l1| 1 (|getShellEntry| $ 36))
                           |LIST;setDifference;3$;12|)
                     (COND
                       ((NULL (SPADCALL |l11| |l2|
                                  (|getShellEntry| $ 33)))
                        (LETT |lu| (CONS |l11| |lu|)
                              |LIST;setDifference;3$;12|)))
                     (EXIT (LETT |l1| (CDR |l1|)
                                 |LIST;setDifference;3$;12|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |lu|))))) 

(DEFUN |LIST;convert;$If;13| (|x| $)
  (PROG (#0=#:G1447 |a| #1=#:G1448)
    (RETURN
      (SEQ (SPADCALL
               (CONS (SPADCALL
                         (SPADCALL "construct" (|getShellEntry| $ 39))
                         (|getShellEntry| $ 41))
                     (PROGN
                       (LETT #0# NIL |LIST;convert;$If;13|)
                       (SEQ (LETT |a| NIL |LIST;convert;$If;13|)
                            (LETT #1# |x| |LIST;convert;$If;13|) G190
                            (COND
                              ((OR (ATOM #1#)
                                   (PROGN
                                     (LETT |a| (CAR #1#)
                                      |LIST;convert;$If;13|)
                                     NIL))
                               (GO G191)))
                            (SEQ (EXIT (LETT #0#
                                        (CONS
                                         (SPADCALL |a|
                                          (|getShellEntry| $ 42))
                                         #0#)
                                        |LIST;convert;$If;13|)))
                            (LETT #1# (CDR #1#) |LIST;convert;$If;13|)
                            (GO G190) G191 (EXIT (NREVERSE0 #0#)))))
               (|getShellEntry| $ 44)))))) 

(DEFUN |List| (#0=#:G1449)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1450)
        (RETURN
          (COND
            ((LETT #1#
                   (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                       (HGET |$ConstructorCache| '|List|)
                       '|domainEqualList|)
                   |List|)
             (|CDRwithIncrement| #1#))
            ('T
             (UNWIND-PROTECT
               (PROG1 (|List;| #0#) (LETT #1# T |List|))
               (COND ((NOT #1#) (HREM |$ConstructorCache| '|List|))))))))))) 

(DEFUN |List;| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|List|))
        (LETT |dv$| (LIST '|List| |dv$1|) . #0#)
        (LETT $ (|newShell| 63) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (OR (AND (|HasCategory| |#1|
                                      '(|OrderedSet|))
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
                                    '(|CoercibleTo| (|OutputForm|))))
                            (|HasCategory| |#1|
                                '(|ConvertibleTo| (|InputForm|)))
                            (OR (|HasCategory| |#1| '(|OrderedSet|))
                                (|HasCategory| |#1| '(|SetCategory|)))
                            (|HasCategory| |#1| '(|OrderedSet|))
                            (|HasCategory| |#1| '(|OpenMath|))
                            (|HasCategory| (|Integer|) '(|OrderedSet|))
                            (|HasCategory| |#1| '(|SetCategory|))
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|))))
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|))))) . #0#))
        (|haddProp| |$ConstructorCache| '|List| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (COND
          ((|testBitVector| |pv$| 6)
           (PROGN
             (|setShellEntry| $ 26
                 (CONS (|dispatchFunction| |LIST;OMwrite;$S;6|) $))
             (|setShellEntry| $ 27
                 (CONS (|dispatchFunction| |LIST;OMwrite;$BS;7|) $))
             (|setShellEntry| $ 28
                 (CONS (|dispatchFunction| |LIST;OMwrite;Omd$V;8|) $))
             (|setShellEntry| $ 29
                 (CONS (|dispatchFunction| |LIST;OMwrite;Omd$BV;9|) $)))))
        (COND
          ((|testBitVector| |pv$| 8)
           (PROGN
             (|setShellEntry| $ 32
                 (CONS (|dispatchFunction| |LIST;setUnion;3$;10|) $))
             (|setShellEntry| $ 34
                 (CONS (|dispatchFunction|
                           |LIST;setIntersection;3$;11|)
                       $))
             (|setShellEntry| $ 37
                 (CONS (|dispatchFunction| |LIST;setDifference;3$;12|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (|setShellEntry| $ 45
               (CONS (|dispatchFunction| |LIST;convert;$If;13|) $))))
        $)))) 

(MAKEPROP '|List| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|IndexedList| 6 (NRTEVAL 1))
             (|local| |#1|) |LIST;nil;$;1| (|Boolean|) |LIST;null;$B;2|
             |LIST;cons;S2$;3| |LIST;append;3$;4| (|Void|)
             (|OpenMathDevice|) (0 . |OMputApp|) (|String|)
             (5 . |OMputSymbol|) (12 . |not|) (17 . |OMwrite|)
             (24 . |OMputEndApp|) (|OpenMathEncoding|)
             (29 . |OMencodingXML|) (33 . |OMopenString|)
             (39 . |OMputObject|) (44 . |OMputEndObject|)
             (49 . |OMclose|) (54 . |OMwrite|) (59 . |OMwrite|)
             (65 . |OMwrite|) (71 . |OMwrite|) (78 . |concat|)
             (84 . |removeDuplicates|) (89 . |setUnion|)
             (95 . |member?|) (101 . |setIntersection|) (|Integer|)
             (107 . |elt|) (113 . |setDifference|) (|Symbol|)
             (119 . |coerce|) (|InputForm|) (124 . |convert|)
             (129 . |convert|) (|List| $) (134 . |convert|)
             (139 . |convert|) (|Mapping| 6 6 6) (|NonNegativeInteger|)
             (|List| 6) (|Equation| 6) (|List| 49) (|Mapping| 8 6)
             (|Mapping| 8 6 6) (|UniversalSegment| 35) '"last" '"rest"
             '"first" '"value" (|Mapping| 6 6) (|OutputForm|)
             (|SingleInteger|) (|List| 35) (|Union| 6 '"failed"))
          '#(|setUnion| 144 |setIntersection| 150 |setDifference| 156
             |removeDuplicates| 162 |null| 167 |nil| 172 |member?| 176
             |elt| 182 |convert| 188 |cons| 193 |concat| 199 |append|
             205 |OMwrite| 211)
          '((|shallowlyMutable| . 0) (|finiteAggregate| . 0))
          (CONS (|makeByteWordVec2| 6
                    '(0 0 0 0 0 0 0 0 0 0 5 0 0 1 4 0 0 1 2 3 4 6))
                (CONS '#(|ListAggregate&| |StreamAggregate&|
                         |ExtensibleLinearAggregate&|
                         |FiniteLinearAggregate&|
                         |UnaryRecursiveAggregate&| |LinearAggregate&|
                         |RecursiveAggregate&| |IndexedAggregate&|
                         |Collection&| |HomogeneousAggregate&|
                         |OrderedSet&| |Aggregate&| |EltableAggregate&|
                         |Evalable&| |SetCategory&| NIL NIL
                         |InnerEvalable&| NIL NIL |BasicType&| NIL)
                      (CONS '#((|ListAggregate| 6)
                               (|StreamAggregate| 6)
                               (|ExtensibleLinearAggregate| 6)
                               (|FiniteLinearAggregate| 6)
                               (|UnaryRecursiveAggregate| 6)
                               (|LinearAggregate| 6)
                               (|RecursiveAggregate| 6)
                               (|IndexedAggregate| 35 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 35 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|)
                               (|Eltable| 35 6) (|InnerEvalable| 6 6)
                               (|CoercibleTo| 59) (|ConvertibleTo| 40)
                               (|BasicType|) (|OpenMath|))
                            (|makeByteWordVec2| 45
                                '(1 13 12 0 14 3 13 12 0 15 15 16 1 8 0
                                  0 17 3 6 12 13 0 8 18 1 13 12 0 19 0
                                  20 0 21 2 13 0 15 20 22 1 13 12 0 23
                                  1 13 12 0 24 1 13 12 0 25 1 0 15 0 26
                                  2 0 15 0 8 27 2 0 12 13 0 28 3 0 12
                                  13 0 8 29 2 0 0 0 0 30 1 0 0 0 31 2 0
                                  0 0 0 32 2 0 8 6 0 33 2 0 0 0 0 34 2
                                  0 6 0 35 36 2 0 0 0 0 37 1 38 0 15 39
                                  1 40 0 38 41 1 6 40 0 42 1 40 0 43 44
                                  1 0 40 0 45 2 8 0 0 0 32 2 8 0 0 0 34
                                  2 8 0 0 0 37 1 8 0 0 31 1 0 8 0 9 0 0
                                  0 7 2 8 8 6 0 33 2 0 6 0 35 36 1 3 40
                                  0 45 2 0 0 6 0 10 2 0 0 0 0 30 2 0 0
                                  0 0 11 3 6 12 13 0 8 29 2 6 12 13 0
                                  28 1 6 15 0 26 2 6 15 0 8 27)))))
          '|lookupIncomplete|)) 
