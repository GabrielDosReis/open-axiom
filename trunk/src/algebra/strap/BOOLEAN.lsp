
(/VERSIONCHECK 2) 

(PUT '|BOOLEAN;test;2$;1| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DEFUN |BOOLEAN;test;2$;1| (|a| $) |a|) 

(DEFUN |BOOLEAN;nt| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(PUT '|BOOLEAN;true;$;3| '|SPADreplace| '(XLAM NIL 'T)) 

(DEFUN |BOOLEAN;true;$;3| ($) 'T) 

(PUT '|BOOLEAN;false;$;4| '|SPADreplace| '(XLAM NIL NIL)) 

(DEFUN |BOOLEAN;false;$;4| ($) NIL) 

(DEFUN |BOOLEAN;not;2$;5| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;~;2$;6| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;and;3$;7| (|a| |b| $) (COND (|a| |b|) ('T 'NIL))) 

(DEFUN |BOOLEAN;/\\;3$;8| (|a| |b| $) (COND (|a| |b|) ('T 'NIL))) 

(DEFUN |BOOLEAN;or;3$;9| (|a| |b| $) (COND (|a| 'T) ('T |b|))) 

(DEFUN |BOOLEAN;\\/;3$;10| (|a| |b| $) (COND (|a| 'T) ('T |b|))) 

(DEFUN |BOOLEAN;xor;3$;11| (|a| |b| $)
  (COND (|a| (|BOOLEAN;nt| |b| $)) ('T |b|))) 

(DEFUN |BOOLEAN;nor;3$;12| (|a| |b| $)
  (COND (|a| 'NIL) ('T (|BOOLEAN;nt| |b| $)))) 

(DEFUN |BOOLEAN;nand;3$;13| (|a| |b| $)
  (COND (|a| (|BOOLEAN;nt| |b| $)) ('T 'T))) 

(PUT '|BOOLEAN;=;3$;14| '|SPADreplace| 'EQ) 

(DEFUN |BOOLEAN;=;3$;14| (|a| |b| $) (EQ |a| |b|)) 

(DEFUN |BOOLEAN;implies;3$;15| (|a| |b| $) (COND (|a| |b|) ('T 'T))) 

(PUT '|BOOLEAN;equiv;3$;16| '|SPADreplace| 'EQ) 

(DEFUN |BOOLEAN;equiv;3$;16| (|a| |b| $) (EQ |a| |b|)) 

(DEFUN |BOOLEAN;<;3$;17| (|a| |b| $)
  (COND (|b| (|BOOLEAN;nt| |a| $)) ('T 'NIL))) 

(PUT '|BOOLEAN;size;Nni;18| '|SPADreplace| '(XLAM NIL 2)) 

(DEFUN |BOOLEAN;size;Nni;18| ($) 2) 

(DEFUN |BOOLEAN;index;Pi$;19| (|i| $)
  (COND ((SPADCALL |i| (|getShellEntry| $ 26)) 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;lookup;$Pi;20| (|a| $) (COND (|a| 1) ('T 2))) 

(DEFUN |BOOLEAN;random;$;21| ($)
  (COND ((SPADCALL (|random|) (|getShellEntry| $ 26)) 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;convert;$If;22| (|x| $)
  (COND
    (|x| (SPADCALL (SPADCALL "true" (|getShellEntry| $ 33))
             (|getShellEntry| $ 35)))
    ('T
     (SPADCALL (SPADCALL "false" (|getShellEntry| $ 33))
         (|getShellEntry| $ 35))))) 

(DEFUN |BOOLEAN;coerce;$Of;23| (|x| $)
  (COND
    (|x| (SPADCALL "true" (|getShellEntry| $ 38)))
    ('T (SPADCALL "false" (|getShellEntry| $ 38))))) 

(DEFUN |Boolean| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1422)
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
        (LETT $ (|newShell| 41) . #0#)
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
             |BOOLEAN;lookup;$Pi;20| |BOOLEAN;random;$;21| (|String|)
             (|Symbol|) (5 . |coerce|) (|InputForm|) (10 . |convert|)
             |BOOLEAN;convert;$If;22| (|OutputForm|) (15 . |message|)
             |BOOLEAN;coerce;$Of;23| (|SingleInteger|))
          '#(~= 20 ~ 26 |xor| 31 |true| 37 |test| 41 |size| 46 |random|
             50 |or| 54 |not| 60 |nor| 65 |nand| 71 |min| 77 |max| 83
             |lookup| 89 |latex| 94 |index| 99 |implies| 104 |hash| 110
             |false| 115 |equiv| 119 |convert| 125 |coerce| 130 |and|
             135 |\\/| 141 >= 147 > 153 = 159 <= 165 < 171 |/\\| 177)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0))
                (CONS '#(NIL |OrderedSet&| NIL NIL |Logic&|
                         |SetCategory&| NIL |BasicType&| NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|PropositionalLogic|) (|Finite|)
                               (|Logic|) (|SetCategory|)
                               (|ConvertibleTo| 34) (|BasicType|)
                               (|CoercibleTo| 37))
                            (|makeByteWordVec2| 40
                                '(1 25 18 0 26 1 32 0 31 33 1 34 0 32
                                  35 1 37 0 31 38 2 0 18 0 0 1 1 0 0 0
                                  10 2 0 0 0 0 15 0 0 0 7 1 0 0 0 6 0 0
                                  23 24 0 0 0 30 2 0 0 0 0 13 1 0 0 0 9
                                  2 0 0 0 0 16 2 0 0 0 0 17 2 0 0 0 0 1
                                  2 0 0 0 0 1 1 0 27 0 29 1 0 31 0 1 1
                                  0 0 27 28 2 0 0 0 0 20 1 0 40 0 1 0 0
                                  0 8 2 0 0 0 0 21 1 0 34 0 36 1 0 37 0
                                  39 2 0 0 0 0 11 2 0 0 0 0 14 2 0 18 0
                                  0 1 2 0 18 0 0 1 2 0 18 0 0 19 2 0 18
                                  0 0 1 2 0 18 0 0 22 2 0 0 0 0 12)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Boolean| '|isFunctor|
             '(((|test| ($ $)) T (ELT $ 6))
               ((|nor| ($ $ $)) T (ELT $ 16))
               ((|nand| ($ $ $)) T (ELT $ 17))
               ((|xor| ($ $ $)) T (ELT $ 15))
               ((|false| ($)) T (CONST $ 8))
               ((|true| ($)) T (CONST $ 7))
               ((|convert| ((|InputForm|) $)) T (ELT $ 36))
               ((|not| ($ $)) T (ELT $ 9))
               ((|and| ($ $ $)) T (ELT $ 11))
               ((|or| ($ $ $)) T (ELT $ 13))
               ((|implies| ($ $ $)) T (ELT $ 20))
               ((|equiv| ($ $ $)) T (ELT $ 21))
               ((~ ($ $)) T (ELT $ 10)) ((|/\\| ($ $ $)) T (ELT $ 12))
               ((|\\/| ($ $ $)) T (ELT $ 14))
               ((|size| ((|NonNegativeInteger|))) T (ELT $ 24))
               ((|index| ($ (|PositiveInteger|))) T (ELT $ 28))
               ((|lookup| ((|PositiveInteger|) $)) T (ELT $ 29))
               ((|random| ($)) T (ELT $ 30))
               ((|min| ($ $ $)) T (ELT $ NIL))
               ((|max| ($ $ $)) T (ELT $ NIL))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ 22))
               ((|latex| ((|String|) $)) T (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 39))
               ((= ((|Boolean|) $ $)) T (ELT $ 19))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|Boolean| '(|Boolean|)
                 '((|Join| (|OrderedFinite|) (|Logic|)
                           (|PropositionalLogic|)
                           (|ConvertibleTo| (|InputForm|))
                           (CATEGORY |domain|
                               (SIGNATURE |true| ($) |constant|)
                               (SIGNATURE |false| ($) |constant|)
                               (SIGNATURE |xor| ($ $ $))
                               (SIGNATURE |nand| ($ $ $))
                               (SIGNATURE |nor| ($ $ $))
                               (SIGNATURE |test| ($ $)))))
                 T '|Boolean|
                 (|put| '|Boolean| '|mode|
                        '(|Mapping|
                             (|Join| (|OrderedFinite|) (|Logic|)
                                     (|PropositionalLogic|)
                                     (|ConvertibleTo| (|InputForm|))
                                     (CATEGORY |domain|
                                      (SIGNATURE |true| ($) |constant|)
                                      (SIGNATURE |false| ($)
                                       |constant|)
                                      (SIGNATURE |xor| ($ $ $))
                                      (SIGNATURE |nand| ($ $ $))
                                      (SIGNATURE |nor| ($ $ $))
                                      (SIGNATURE |test| ($ $)))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|Boolean| 'NILADIC T) 
