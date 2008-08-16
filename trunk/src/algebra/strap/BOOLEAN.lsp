
(/VERSIONCHECK 2) 

(PUT '|BOOLEAN;test;2$;1| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DEFUN |BOOLEAN;test;2$;1| (|a| $) |a|) 

(DEFUN |BOOLEAN;nt| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(PUT '|BOOLEAN;true;$;3| '|SPADreplace| '(XLAM NIL 'T)) 

(DEFUN |BOOLEAN;true;$;3| ($) 'T) 

(PUT '|BOOLEAN;false;$;4| '|SPADreplace| '(XLAM NIL NIL)) 

(DEFUN |BOOLEAN;false;$;4| ($) NIL) 

(DEFUN |BOOLEAN;not;2$;5| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;^;2$;6| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;~;2$;7| (|b| $) (COND (|b| 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;and;3$;8| (|a| |b| $) (COND (|a| |b|) ('T 'NIL))) 

(DEFUN |BOOLEAN;/\\;3$;9| (|a| |b| $) (COND (|a| |b|) ('T 'NIL))) 

(DEFUN |BOOLEAN;or;3$;10| (|a| |b| $) (COND (|a| 'T) ('T |b|))) 

(DEFUN |BOOLEAN;\\/;3$;11| (|a| |b| $) (COND (|a| 'T) ('T |b|))) 

(DEFUN |BOOLEAN;xor;3$;12| (|a| |b| $)
  (COND (|a| (|BOOLEAN;nt| |b| $)) ('T |b|))) 

(DEFUN |BOOLEAN;nor;3$;13| (|a| |b| $)
  (COND (|a| 'NIL) ('T (|BOOLEAN;nt| |b| $)))) 

(DEFUN |BOOLEAN;nand;3$;14| (|a| |b| $)
  (COND (|a| (|BOOLEAN;nt| |b| $)) ('T 'T))) 

(PUT '|BOOLEAN;=;2$B;15| '|SPADreplace| 'EQ) 

(DEFUN |BOOLEAN;=;2$B;15| (|a| |b| $) (EQ |a| |b|)) 

(DEFUN |BOOLEAN;implies;3$;16| (|a| |b| $) (COND (|a| |b|) ('T 'T))) 

(PUT '|BOOLEAN;equiv;3$;17| '|SPADreplace| 'EQ) 

(DEFUN |BOOLEAN;equiv;3$;17| (|a| |b| $) (EQ |a| |b|)) 

(DEFUN |BOOLEAN;<;2$B;18| (|a| |b| $)
  (COND (|b| (|BOOLEAN;nt| |a| $)) ('T 'NIL))) 

(PUT '|BOOLEAN;size;Nni;19| '|SPADreplace| '(XLAM NIL 2)) 

(DEFUN |BOOLEAN;size;Nni;19| ($) 2) 

(DEFUN |BOOLEAN;index;Pi$;20| (|i| $)
  (COND ((SPADCALL |i| (|getShellEntry| $ 27)) 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;lookup;$Pi;21| (|a| $) (COND (|a| 1) ('T 2))) 

(DEFUN |BOOLEAN;random;$;22| ($)
  (COND ((SPADCALL (|random|) (|getShellEntry| $ 27)) 'NIL) ('T 'T))) 

(DEFUN |BOOLEAN;convert;$If;23| (|x| $)
  (COND
    (|x| (SPADCALL (SPADCALL "true" (|getShellEntry| $ 34))
             (|getShellEntry| $ 36)))
    ('T
     (SPADCALL (SPADCALL "false" (|getShellEntry| $ 34))
         (|getShellEntry| $ 36))))) 

(DEFUN |BOOLEAN;coerce;$Of;24| (|x| $)
  (COND
    (|x| (SPADCALL "true" (|getShellEntry| $ 39)))
    ('T (SPADCALL "false" (|getShellEntry| $ 39))))) 

(DEFUN |Boolean| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1421)
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
        (LETT $ (|newShell| 42) . #0#)
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
             |BOOLEAN;not;2$;5| |BOOLEAN;^;2$;6| |BOOLEAN;~;2$;7|
             |BOOLEAN;and;3$;8| |BOOLEAN;/\\;3$;9| |BOOLEAN;or;3$;10|
             |BOOLEAN;\\/;3$;11| |BOOLEAN;xor;3$;12|
             |BOOLEAN;nor;3$;13| |BOOLEAN;nand;3$;14| (|Boolean|)
             |BOOLEAN;=;2$B;15| |BOOLEAN;implies;3$;16|
             |BOOLEAN;equiv;3$;17| |BOOLEAN;<;2$B;18|
             (|NonNegativeInteger|) |BOOLEAN;size;Nni;19| (|Integer|)
             (0 . |even?|) (|PositiveInteger|) |BOOLEAN;index;Pi$;20|
             |BOOLEAN;lookup;$Pi;21| |BOOLEAN;random;$;22| (|String|)
             (|Symbol|) (5 . |coerce|) (|InputForm|) (10 . |convert|)
             |BOOLEAN;convert;$If;23| (|OutputForm|) (15 . |message|)
             |BOOLEAN;coerce;$Of;24| (|SingleInteger|))
          '#(~= 20 ~ 26 |xor| 31 |true| 37 |test| 41 |size| 46 |random|
             50 |or| 54 |not| 60 |nor| 65 |nand| 71 |min| 77 |max| 83
             |lookup| 89 |latex| 94 |index| 99 |implies| 104 |hash| 110
             |false| 115 |equiv| 119 |convert| 125 |coerce| 130 |and|
             135 ^ 141 |\\/| 146 >= 152 > 158 = 164 <= 170 < 176 |/\\|
             182)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0))
                (CONS '#(|OrderedSet&| NIL |Logic&| |SetCategory&| NIL
                         NIL |BasicType&| NIL)
                      (CONS '#((|OrderedSet|) (|Finite|) (|Logic|)
                               (|SetCategory|) (|ConvertibleTo| 35)
                               (|PropositionalLogic|) (|BasicType|)
                               (|CoercibleTo| 38))
                            (|makeByteWordVec2| 41
                                '(1 26 19 0 27 1 33 0 32 34 1 35 0 33
                                  36 1 38 0 32 39 2 0 19 0 0 1 1 0 0 0
                                  11 2 0 0 0 0 16 0 0 0 7 1 0 0 0 6 0 0
                                  24 25 0 0 0 31 2 0 0 0 0 14 1 0 0 0 9
                                  2 0 0 0 0 17 2 0 0 0 0 18 2 0 0 0 0 1
                                  2 0 0 0 0 1 1 0 28 0 30 1 0 32 0 1 1
                                  0 0 28 29 2 0 0 0 0 21 1 0 41 0 1 0 0
                                  0 8 2 0 0 0 0 22 1 0 35 0 37 1 0 38 0
                                  40 2 0 0 0 0 12 1 0 0 0 10 2 0 0 0 0
                                  15 2 0 19 0 0 1 2 0 19 0 0 1 2 0 19 0
                                  0 20 2 0 19 0 0 1 2 0 19 0 0 23 2 0 0
                                  0 0 13)))))
          '|lookupComplete|)) 

(MAKEPROP '|Boolean| 'NILADIC T) 
