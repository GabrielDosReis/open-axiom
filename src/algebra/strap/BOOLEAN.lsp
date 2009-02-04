
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;test;2$;1|)) 

(PUT '|BOOLEAN;test;2$;1| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;nt|)) 

(PUT '|BOOLEAN;nt| '|SPADreplace| 'NOT) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Boolean|) |BOOLEAN;true;$;3|)) 

(PUT '|BOOLEAN;true;$;3| '|SPADreplace| '(XLAM NIL 'T)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Boolean|) |BOOLEAN;false;$;4|)) 

(PUT '|BOOLEAN;false;$;4| '|SPADreplace| '(XLAM NIL NIL)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;not;2$;5|)) 

(PUT '|BOOLEAN;not;2$;5| '|SPADreplace| 'NOT) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;~;2$;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;and;3$;7|)) 

(PUT '|BOOLEAN;and;3$;7| '|SPADreplace| 'AND) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;/\\;3$;8|)) 

(PUT '|BOOLEAN;/\\;3$;8| '|SPADreplace| 'AND) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;or;3$;9|)) 

(PUT '|BOOLEAN;or;3$;9| '|SPADreplace| 'OR) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;\\/;3$;10|)) 

(PUT '|BOOLEAN;\\/;3$;10| '|SPADreplace| 'OR) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;xor;3$;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;nor;3$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;nand;3$;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;=;3$;14|)) 

(PUT '|BOOLEAN;=;3$;14| '|SPADreplace| 'EQ) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;implies;3$;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;equiv;3$;16|)) 

(PUT '|BOOLEAN;equiv;3$;16| '|SPADreplace| 'EQ) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Boolean| |%Shell|) |%Boolean|)
                |BOOLEAN;<;3$;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |BOOLEAN;size;Nni;18|)) 

(PUT '|BOOLEAN;size;Nni;18| '|SPADreplace| '(XLAM NIL 2)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Boolean|)
                |BOOLEAN;index;Pi$;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) (|%IntegerSection| 1))
                |BOOLEAN;lookup;$Pi;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Boolean|) |BOOLEAN;random;$;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Thing|)
                |BOOLEAN;convert;$If;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Boolean| |%Shell|) |%Thing|)
                |BOOLEAN;coerce;$Of;23|)) 

(DEFUN |BOOLEAN;test;2$;1| (|a| $) (DECLARE (IGNORE $)) |a|) 

(DEFUN |BOOLEAN;nt| (|a| $) (DECLARE (IGNORE $)) (NOT |a|)) 

(DEFUN |BOOLEAN;true;$;3| ($) (DECLARE (IGNORE $)) 'T) 

(DEFUN |BOOLEAN;false;$;4| ($) (DECLARE (IGNORE $)) NIL) 

(DEFUN |BOOLEAN;not;2$;5| (|b| $) (DECLARE (IGNORE $)) (NOT |b|)) 

(DEFUN |BOOLEAN;~;2$;6| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;and;3$;7| (|a| |b| $)
  (DECLARE (IGNORE $))
  (AND |a| |b|)) 

(DEFUN |BOOLEAN;/\\;3$;8| (|a| |b| $)
  (DECLARE (IGNORE $))
  (AND |a| |b|)) 

(DEFUN |BOOLEAN;or;3$;9| (|a| |b| $)
  (DECLARE (IGNORE $))
  (OR |a| |b|)) 

(DEFUN |BOOLEAN;\\/;3$;10| (|a| |b| $)
  (DECLARE (IGNORE $))
  (OR |a| |b|)) 

(DEFUN |BOOLEAN;xor;3$;11| (|a| |b| $)
  (COND (|a| (NOT |b|)) ('T |b|))) 

(DEFUN |BOOLEAN;nor;3$;12| (|a| |b| $)
  (COND (|a| 'NIL) ('T (NOT |b|)))) 

(DEFUN |BOOLEAN;nand;3$;13| (|a| |b| $)
  (COND (|a| (NOT |b|)) ('T 'T))) 

(DEFUN |BOOLEAN;=;3$;14| (|a| |b| $)
  (DECLARE (IGNORE $))
  (EQ |a| |b|)) 

(DEFUN |BOOLEAN;implies;3$;15| (|a| |b| $) (COND (|a| |b|) ('T 'T))) 

(DEFUN |BOOLEAN;equiv;3$;16| (|a| |b| $)
  (DECLARE (IGNORE $))
  (EQ |a| |b|)) 

(DEFUN |BOOLEAN;<;3$;17| (|a| |b| $) (COND (|b| (NOT |a|)) ('T 'NIL))) 

(DEFUN |BOOLEAN;size;Nni;18| ($) (DECLARE (IGNORE $)) 2) 

(DEFUN |BOOLEAN;index;Pi$;19| (|i| $)
  (COND ((SPADCALL |i| (|getShellEntry| $ 26)) 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;lookup;$Pi;20| (|a| $) (COND (|a| 1) ('T 2))) 

(DEFUN |BOOLEAN;random;$;21| ($)
  (COND ((SPADCALL (|random|) (|getShellEntry| $ 26)) 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;convert;$If;22| (|x| $)
  (SPADCALL (COND (|x| '|true|) ('T '|false|)) (|getShellEntry| $ 33))) 

(DEFUN |BOOLEAN;coerce;$Of;23| (|x| $)
  (SPADCALL (COND (|x| '|true|) ('T '|false|)) (|getShellEntry| $ 36))) 

(DEFUN |Boolean| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1426)
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
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|Boolean|) . #0=(|Boolean|))
        (LETT $ (|newShell| 40) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Boolean| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|Boolean| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL |BOOLEAN;test;2$;1|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |BOOLEAN;true;$;3|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |BOOLEAN;false;$;4|) $))
             |BOOLEAN;not;2$;5| |BOOLEAN;~;2$;6| |BOOLEAN;and;3$;7|
             |BOOLEAN;/\\;3$;8| |BOOLEAN;or;3$;9| |BOOLEAN;\\/;3$;10|
             |BOOLEAN;xor;3$;11| |BOOLEAN;nor;3$;12|
             |BOOLEAN;nand;3$;13| (|Boolean|) |BOOLEAN;=;3$;14|
             |BOOLEAN;implies;3$;15| |BOOLEAN;equiv;3$;16|
             |BOOLEAN;<;3$;17| (|NonNegativeInteger|)
             |BOOLEAN;size;Nni;18| (|Integer|) (0 . |even?|)
             (|PositiveInteger|) |BOOLEAN;index;Pi$;19|
             |BOOLEAN;lookup;$Pi;20| |BOOLEAN;random;$;21| (|Symbol|)
             (|InputForm|) (5 . |convert|) |BOOLEAN;convert;$If;22|
             (|OutputForm|) (10 . |outputForm|) |BOOLEAN;coerce;$Of;23|
             (|String|) (|SingleInteger|))
          '#(~= 15 ~ 21 |xor| 26 |true| 32 |test| 36 |size| 41 |random|
             45 |or| 49 |not| 55 |nor| 60 |nand| 66 |min| 72 |max| 82
             |lookup| 92 |latex| 97 |index| 102 |implies| 107 |hash|
             113 |false| 118 |equiv| 122 |convert| 128 |coerce| 133
             |and| 138 |\\/| 144 >= 150 > 156 = 162 <= 168 < 174 |/\\|
             180)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0))
                (CONS '#(NIL |OrderedSet&| NIL NIL |Logic&|
                         |SetCategory&| NIL |BasicType&| NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|PropositionalLogic|) (|Finite|)
                               (|Logic|) (|SetCategory|)
                               (|ConvertibleTo| 32) (|BasicType|)
                               (|CoercibleTo| 35))
                            (|makeByteWordVec2| 39
                                '(1 25 18 0 26 1 32 0 31 33 1 35 0 31
                                  36 2 0 18 0 0 1 1 0 0 0 10 2 0 0 0 0
                                  15 0 0 0 7 1 0 0 0 6 0 0 23 24 0 0 0
                                  30 2 0 0 0 0 13 1 0 0 0 9 2 0 0 0 0
                                  16 2 0 0 0 0 17 0 0 0 1 2 0 0 0 0 1 0
                                  0 0 1 2 0 0 0 0 1 1 0 27 0 29 1 0 38
                                  0 1 1 0 0 27 28 2 0 0 0 0 20 1 0 39 0
                                  1 0 0 0 8 2 0 0 0 0 21 1 0 32 0 34 1
                                  0 35 0 37 2 0 0 0 0 11 2 0 0 0 0 14 2
                                  0 18 0 0 1 2 0 18 0 0 1 2 0 18 0 0 19
                                  2 0 18 0 0 1 2 0 18 0 0 22 2 0 0 0 0
                                  12)))))
          '|lookupComplete|)) 

(MAKEPROP '|Boolean| 'NILADIC T) 
