
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
       (SEQ G190 (COND ((NULL (NOT (NULL |x|))) (GO G191)))
            (SEQ (SPADCALL |dev| (|SPADfirst| |x|) 'NIL
                     (|getShellEntry| $ 21))
                 (EXIT (LETT |x| (CDR |x|) |LIST;writeOMList|)))
            NIL (GO G190) G191 (EXIT NIL))
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 23))))) 

(DEFUN |LIST;OMwrite;$S;6| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |LIST;OMwrite;$S;6|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |LIST;OMwrite;$S;6|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 25))
                     (|getShellEntry| $ 26))
                 |LIST;OMwrite;$S;6|)
           (SPADCALL |dev| (|getShellEntry| $ 27))
           (|LIST;writeOMList| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 28))
           (SPADCALL |dev| (|getShellEntry| $ 29))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |LIST;OMwrite;$S;6|)
           (EXIT |s|))))) 

(DEFUN |LIST;OMwrite;$BS;7| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |LIST;OMwrite;$BS;7|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |LIST;OMwrite;$BS;7|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 25))
                     (|getShellEntry| $ 26))
                 |LIST;OMwrite;$BS;7|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 27))))
           (|LIST;writeOMList| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 28))))
           (SPADCALL |dev| (|getShellEntry| $ 29))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |LIST;OMwrite;$BS;7|)
           (EXIT |s|))))) 

(DEFUN |LIST;OMwrite;Omd$V;8| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 27))
       (|LIST;writeOMList| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 28))))) 

(DEFUN |LIST;OMwrite;Omd$BV;9| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 27))))
       (|LIST;writeOMList| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 28))))))) 

(DEFUN |LIST;setUnion;3$;10| (|l1| |l2| $)
  (SPADCALL (SPADCALL |l1| |l2| (|getShellEntry| $ 34))
            (|getShellEntry| $ 35))) 

(DEFUN |LIST;setIntersection;3$;11| (|l1| |l2| $)
  (PROG (|u|)
    (RETURN
      (SEQ (LETT |u| NIL |LIST;setIntersection;3$;11|)
           (LETT |l1| (SPADCALL |l1| (|getShellEntry| $ 35))
                 |LIST;setIntersection;3$;11|)
           (SEQ G190 (COND ((NULL (NOT (NULL |l1|))) (GO G191)))
                (SEQ (COND
                       ((SPADCALL (|SPADfirst| |l1|) |l2|
                            (|getShellEntry| $ 39))
                        (LETT |u| (CONS (|SPADfirst| |l1|) |u|)
                              |LIST;setIntersection;3$;11|)))
                     (EXIT (LETT |l1| (CDR |l1|)
                                 |LIST;setIntersection;3$;11|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |u|))))) 

(DEFUN |LIST;setDifference;3$;12| (|l1| |l2| $)
  (PROG (|l11| |lu|)
    (RETURN
      (SEQ (LETT |l1| (SPADCALL |l1| (|getShellEntry| $ 35))
                 |LIST;setDifference;3$;12|)
           (LETT |lu| NIL |LIST;setDifference;3$;12|)
           (SEQ G190 (COND ((NULL (NOT (NULL |l1|))) (GO G191)))
                (SEQ (LETT |l11|
                           (SPADCALL |l1| 1 (|getShellEntry| $ 41))
                           |LIST;setDifference;3$;12|)
                     (COND
                       ((NOT (SPADCALL |l11| |l2|
                                 (|getShellEntry| $ 39)))
                        (LETT |lu| (CONS |l11| |lu|)
                              |LIST;setDifference;3$;12|)))
                     (EXIT (LETT |l1| (CDR |l1|)
                                 |LIST;setDifference;3$;12|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT |lu|))))) 

(DEFUN |LIST;convert;$If;13| (|x| $)
  (PROG (#0=#:G1444 |a| #1=#:G1445)
    (RETURN
      (SEQ (SPADCALL
               (CONS (SPADCALL '|construct| (|getShellEntry| $ 46))
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
                                          (|getShellEntry| $ 47))
                                         #0#)
                                        |LIST;convert;$If;13|)))
                            (LETT #1# (CDR #1#) |LIST;convert;$If;13|)
                            (GO G190) G191 (EXIT (NREVERSE0 #0#)))))
               (|getShellEntry| $ 51)))))) 

(DEFUN |List| (#0=#:G1446)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1447)
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
        (LETT $ (|newShell| 69) . #0#)
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
                            (|HasCategory| |#1|
                                '(|CoercibleTo| (|OutputForm|)))
                            (AND (|HasCategory| |#1| '(|SetCategory|))
                                 (|HasCategory| |#1|
                                     (LIST '|Evalable|
                                      (|devaluate| |#1|)))))) . #0#))
        (|haddProp| |$ConstructorCache| '|List| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (COND
          ((|testBitVector| |pv$| 6)
           (PROGN
             (|setShellEntry| $ 30
                 (CONS (|dispatchFunction| |LIST;OMwrite;$S;6|) $))
             (|setShellEntry| $ 31
                 (CONS (|dispatchFunction| |LIST;OMwrite;$BS;7|) $))
             (|setShellEntry| $ 32
                 (CONS (|dispatchFunction| |LIST;OMwrite;Omd$V;8|) $))
             (|setShellEntry| $ 33
                 (CONS (|dispatchFunction| |LIST;OMwrite;Omd$BV;9|) $)))))
        (COND
          ((|testBitVector| |pv$| 8)
           (PROGN
             (|setShellEntry| $ 36
                 (CONS (|dispatchFunction| |LIST;setUnion;3$;10|) $))
             (|setShellEntry| $ 40
                 (CONS (|dispatchFunction|
                           |LIST;setIntersection;3$;11|)
                       $))
             (|setShellEntry| $ 43
                 (CONS (|dispatchFunction| |LIST;setDifference;3$;12|)
                       $)))))
        (COND
          ((|testBitVector| |pv$| 3)
           (|setShellEntry| $ 52
               (CONS (|dispatchFunction| |LIST;convert;$If;13|) $))))
        $)))) 

(MAKEPROP '|List| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|IndexedList| 6 (NRTEVAL 1))
             (|local| |#1|) (|Integer|) (0 . |One|) |LIST;nil;$;1|
             (|Boolean|) |LIST;null;$B;2| |LIST;cons;S2$;3|
             |LIST;append;3$;4| (|Void|) (|OpenMathDevice|)
             (4 . |OMputApp|) (|String|) (9 . |OMputSymbol|)
             (16 . |first|) (21 . |false|) (25 . |OMwrite|)
             (32 . |rest|) (37 . |OMputEndApp|) (|OpenMathEncoding|)
             (42 . |OMencodingXML|) (46 . |OMopenString|)
             (52 . |OMputObject|) (57 . |OMputEndObject|)
             (62 . |OMclose|) (67 . |OMwrite|) (72 . |OMwrite|)
             (78 . |OMwrite|) (84 . |OMwrite|) (91 . |concat|)
             (97 . |removeDuplicates|) (102 . |setUnion|)
             (108 . |empty|) (112 . |empty?|) (117 . |member?|)
             (123 . |setIntersection|) (129 . |elt|) (135 . |concat|)
             (141 . |setDifference|) (|OutputForm|) (|InputForm|)
             (147 . |convert|) (152 . |convert|) (|List| 45)
             (157 . |concat|) (|List| $) (163 . |convert|)
             (168 . |convert|) (|Mapping| 6 6 6) (|NonNegativeInteger|)
             (|List| 6) (|Equation| 6) (|List| 56) (|Mapping| 10 6)
             (|Mapping| 10 6 6) (|UniversalSegment| 7) '"last" '"rest"
             '"first" '"value" (|Mapping| 6 6) (|SingleInteger|)
             (|List| 7) (|Union| 6 '"failed"))
          '#(|setUnion| 173 |setIntersection| 179 |setDifference| 185
             |rest| 191 |removeDuplicates| 196 |null| 201 |nil| 206
             |member?| 210 |first| 216 |empty?| 221 |empty| 226 |elt|
             230 |convert| 236 |cons| 241 |concat| 247 |append| 259
             |OMwrite| 265)
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
                               (|IndexedAggregate| 7 6)
                               (|Collection| 6)
                               (|HomogeneousAggregate| 6)
                               (|OrderedSet|) (|Aggregate|)
                               (|EltableAggregate| 7 6) (|Evalable| 6)
                               (|SetCategory|) (|Type|) (|Eltable| 7 6)
                               (|InnerEvalable| 6 6) (|CoercibleTo| 44)
                               (|ConvertibleTo| 45) (|BasicType|)
                               (|OpenMath|))
                            (|makeByteWordVec2| 52
                                '(0 7 0 8 1 15 14 0 16 3 15 14 0 17 17
                                  18 1 0 6 0 19 0 10 0 20 3 6 14 15 0
                                  10 21 1 0 0 0 22 1 15 14 0 23 0 24 0
                                  25 2 15 0 17 24 26 1 15 14 0 27 1 15
                                  14 0 28 1 15 14 0 29 1 0 17 0 30 2 0
                                  17 0 10 31 2 0 14 15 0 32 3 0 14 15 0
                                  10 33 2 0 0 0 0 34 1 0 0 0 35 2 0 0 0
                                  0 36 0 0 0 37 1 0 10 0 38 2 0 10 6 0
                                  39 2 0 0 0 0 40 2 0 6 0 7 41 2 0 0 6
                                  0 42 2 0 0 0 0 43 1 45 0 44 46 1 6 45
                                  0 47 2 48 0 45 0 49 1 45 0 50 51 1 0
                                  45 0 52 2 8 0 0 0 36 2 8 0 0 0 40 2 8
                                  0 0 0 43 1 0 0 0 22 1 8 0 0 35 1 0 10
                                  0 11 0 0 0 9 2 8 10 6 0 39 1 0 6 0 19
                                  1 0 10 0 38 0 0 0 37 2 0 6 0 7 41 1 3
                                  45 0 52 2 0 0 6 0 12 2 0 0 6 0 42 2 0
                                  0 0 0 34 2 0 0 0 0 13 3 6 14 15 0 10
                                  33 2 6 14 15 0 32 1 6 17 0 30 2 6 17
                                  0 10 31)))))
          '|lookupIncomplete|)) 
