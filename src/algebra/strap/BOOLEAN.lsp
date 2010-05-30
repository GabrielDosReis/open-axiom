
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;test;2$;1|)) 

(PUT '|BOOLEAN;test;2$;1| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Boolean|) |BOOLEAN;true;$;2|)) 

(PUT '|BOOLEAN;true;$;2| '|SPADreplace| '(XLAM NIL |%true|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Boolean|) |BOOLEAN;false;$;3|)) 

(PUT '|BOOLEAN;false;$;3| '|SPADreplace| '(XLAM NIL |%false|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;not;2$;4|)) 

(PUT '|BOOLEAN;not;2$;4| '|SPADreplace| '|%not|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;~;2$;5|)) 

(PUT '|BOOLEAN;~;2$;5| '|SPADreplace| '|%not|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;and;3$;6|)) 

(PUT '|BOOLEAN;and;3$;6| '|SPADreplace| '|%and|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;/\\;3$;7|)) 

(PUT '|BOOLEAN;/\\;3$;7| '|SPADreplace| '|%and|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;or;3$;8|)) 

(PUT '|BOOLEAN;or;3$;8| '|SPADreplace| '|%or|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;\\/;3$;9|)) 

(PUT '|BOOLEAN;\\/;3$;9| '|SPADreplace| '|%or|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;xor;3$;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;nor;3$;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;nand;3$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;=;3$;13|)) 

(PUT '|BOOLEAN;=;3$;13| '|SPADreplace| '|%eq|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;implies;3$;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;equiv;3$;15|)) 

(PUT '|BOOLEAN;equiv;3$;15| '|SPADreplace| '|%eq|) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;<;3$;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |BOOLEAN;size;Nni;17|)) 

(PUT '|BOOLEAN;size;Nni;17| '|SPADreplace| '(XLAM NIL 2)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Boolean|)
                |BOOLEAN;index;Pi$;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) (|%IntegerSection| 1))
                |BOOLEAN;lookup;$Pi;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Boolean|) |BOOLEAN;random;$;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Thing|)
                |BOOLEAN;convert;$If;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Thing|)
                |BOOLEAN;coerce;$Of;22|)) 

(DEFUN |BOOLEAN;test;2$;1| (|a| $) (DECLARE (IGNORE $)) |a|) 

(DEFUN |BOOLEAN;true;$;2| ($) (DECLARE (IGNORE $)) T) 

(DEFUN |BOOLEAN;false;$;3| ($) (DECLARE (IGNORE $)) NIL) 

(DEFUN |BOOLEAN;not;2$;4| (|b| $) (DECLARE (IGNORE $)) (NOT |b|)) 

(DEFUN |BOOLEAN;~;2$;5| (|b| $) (DECLARE (IGNORE $)) (NOT |b|)) 

(DEFUN |BOOLEAN;and;3$;6| (|a| |b| $)
  (DECLARE (IGNORE $))
  (AND |a| |b|)) 

(DEFUN |BOOLEAN;/\\;3$;7| (|a| |b| $)
  (DECLARE (IGNORE $))
  (AND |a| |b|)) 

(DEFUN |BOOLEAN;or;3$;8| (|a| |b| $)
  (DECLARE (IGNORE $))
  (OR |a| |b|)) 

(DEFUN |BOOLEAN;\\/;3$;9| (|a| |b| $)
  (DECLARE (IGNORE $))
  (OR |a| |b|)) 

(DEFUN |BOOLEAN;xor;3$;10| (|a| |b| $)
  (COND (|a| (NOT |b|)) ('T |b|))) 

(DEFUN |BOOLEAN;nor;3$;11| (|a| |b| $)
  (COND (|a| NIL) ('T (NOT |b|)))) 

(DEFUN |BOOLEAN;nand;3$;12| (|a| |b| $) (COND (|a| (NOT |b|)) ('T T))) 

(DEFUN |BOOLEAN;=;3$;13| (|a| |b| $)
  (DECLARE (IGNORE $))
  (EQ |a| |b|)) 

(DEFUN |BOOLEAN;implies;3$;14| (|a| |b| $) (COND (|a| |b|) ('T T))) 

(DEFUN |BOOLEAN;equiv;3$;15| (|a| |b| $)
  (DECLARE (IGNORE $))
  (EQ |a| |b|)) 

(DEFUN |BOOLEAN;<;3$;16| (|a| |b| $) (COND (|b| (NOT |a|)) ('T NIL))) 

(DEFUN |BOOLEAN;size;Nni;17| ($) (DECLARE (IGNORE $)) 2) 

(DEFUN |BOOLEAN;index;Pi$;18| (|i| $)
  (COND ((SPADCALL |i| (|getShellEntry| $ 26)) NIL) ('T T))) 

(DEFUN |BOOLEAN;lookup;$Pi;19| (|a| $) (COND (|a| 1) ('T 2))) 

(DEFUN |BOOLEAN;random;$;20| ($)
  (COND ((SPADCALL (|random|) (|getShellEntry| $ 26)) NIL) ('T T))) 

(DEFUN |BOOLEAN;convert;$If;21| (|x| $)
  (COND (|x| '|true|) ('T '|false|))) 

(DEFUN |BOOLEAN;coerce;$Of;22| (|x| $)
  (COND (|x| '|true|) ('T '|false|))) 

(DEFUN |Boolean| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1424)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|Boolean|) |Boolean|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Boolean|
                                   (LIST
                                    (CONS NIL (CONS 1 (|Boolean;|))))))
                 (LETT #0# T |Boolean|))
               (COND
                 ((NOT #0#) (HREM |$ConstructorCache| '|Boolean|))))))))))) 

(DEFUN |Boolean;| ()
  (LET ((|dv$| (LIST '|Boolean|)) ($ (|newShell| 39))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|Boolean| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    $)) 

(MAKEPROP '|Boolean| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL |BOOLEAN;test;2$;1|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |BOOLEAN;true;$;2|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |BOOLEAN;false;$;3|) $))
             |BOOLEAN;not;2$;4| |BOOLEAN;~;2$;5| |BOOLEAN;and;3$;6|
             |BOOLEAN;/\\;3$;7| |BOOLEAN;or;3$;8| |BOOLEAN;\\/;3$;9|
             |BOOLEAN;xor;3$;10| |BOOLEAN;nor;3$;11|
             |BOOLEAN;nand;3$;12| (|Boolean|) |BOOLEAN;=;3$;13|
             |BOOLEAN;implies;3$;14| |BOOLEAN;equiv;3$;15|
             |BOOLEAN;<;3$;16| (|NonNegativeInteger|)
             |BOOLEAN;size;Nni;17| (|Integer|) (0 . |even?|)
             (|PositiveInteger|) |BOOLEAN;index;Pi$;18| (5 . |One|)
             |BOOLEAN;lookup;$Pi;19| (9 . |random|)
             |BOOLEAN;random;$;20| (|InputForm|)
             |BOOLEAN;convert;$If;21| (|OutputForm|)
             |BOOLEAN;coerce;$Of;22| (|SingleInteger|) (|String|))
          '#(~= 13 ~ 19 |xor| 24 |true| 30 |test| 34 |size| 39 |random|
             43 |or| 47 |not| 53 |nor| 58 |nand| 64 |min| 70 |max| 80
             |lookup| 90 |latex| 95 |index| 100 |implies| 105 |hash|
             111 |false| 116 |equiv| 120 |convert| 126 |coerce| 131
             |before?| 136 |and| 142 |\\/| 148 >= 154 > 160 = 166 <=
             172 < 178 |/\\| 184)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(NIL |OrderedSet&| NIL NIL NIL |Logic&|
                         |SetCategory&| NIL NIL |BasicType&| NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|PropositionalLogic|) (|Finite|)
                               (|BooleanLogic|) (|Logic|)
                               (|SetCategory|) (|ConvertibleTo| 33)
                               (|Type|) (|BasicType|)
                               (|CoercibleTo| 35))
                            (|makeByteWordVec2| 38
                                '(1 25 18 0 26 0 27 0 29 0 25 0 31 2 0
                                  18 0 0 1 1 0 0 0 10 2 0 0 0 0 15 0 0
                                  0 7 1 0 0 0 6 0 0 23 24 0 0 0 32 2 0
                                  0 0 0 13 1 0 0 0 9 2 0 0 0 0 16 2 0 0
                                  0 0 17 0 0 0 1 2 0 0 0 0 1 0 0 0 1 2
                                  0 0 0 0 1 1 0 27 0 30 1 0 38 0 1 1 0
                                  0 27 28 2 0 0 0 0 20 1 0 37 0 1 0 0 0
                                  8 2 0 0 0 0 21 1 0 33 0 34 1 0 35 0
                                  36 2 0 18 0 0 1 2 0 0 0 0 11 2 0 0 0
                                  0 14 2 0 18 0 0 1 2 0 18 0 0 1 2 0 18
                                  0 0 19 2 0 18 0 0 1 2 0 18 0 0 22 2 0
                                  0 0 0 12)))))
          '|lookupComplete|)) 

(MAKEPROP '|Boolean| 'NILADIC T) 
