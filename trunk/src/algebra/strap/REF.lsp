
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Pair| |%Shell|) |%Boolean|)
                |REF;=;2$B;1|)) 

(PUT '|REF;=;2$B;1| '|SPADreplace| 'EQ) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|) |REF;ref;S$;2|)) 

(PUT '|REF;ref;S$;2| '|SPADreplace| 'LIST) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Shell|) |%Thing|) |REF;elt;$S;3|)) 

(PUT '|REF;elt;$S;3| '|SPADreplace| 'QCAR) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Thing| |%Shell|) |%Thing|)
                |REF;setelt;$2S;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Shell|) |%Thing|)
                |REF;deref;$S;5|)) 

(PUT '|REF;deref;$S;5| '|SPADreplace| 'QCAR) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Thing| |%Shell|) |%Thing|)
                |REF;setref;$2S;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Pair| |%Shell|) |%Thing|)
                |REF;coerce;$Of;7|)) 

(DEFUN |REF;=;2$B;1| (|p| |q| $) (DECLARE (IGNORE $)) (EQ |p| |q|)) 

(DEFUN |REF;ref;S$;2| (|v| $) (DECLARE (IGNORE $)) (LIST |v|)) 

(DEFUN |REF;elt;$S;3| (|p| $) (DECLARE (IGNORE $)) (QCAR |p|)) 

(DEFUN |REF;setelt;$2S;4| (|p| |v| $)
  (PROGN (RPLACA |p| |v|) (QCAR |p|))) 

(DEFUN |REF;deref;$S;5| (|p| $) (DECLARE (IGNORE $)) (QCAR |p|)) 

(DEFUN |REF;setref;$2S;6| (|p| |v| $)
  (PROGN (RPLACA |p| |v|) (QCAR |p|))) 

(DEFUN |REF;coerce;$Of;7| (|p| $)
  (SPADCALL (SPADCALL "ref" (|getShellEntry| $ 17))
      (LIST (SPADCALL (QCAR |p|) (|getShellEntry| $ 18)))
      (|getShellEntry| $ 20))) 

(DEFUN |Reference| (#0=#:G1406)
  (PROG ()
    (RETURN
      (PROG (#1=#:G1407)
        (RETURN
          (COND
            ((LETT #1#
                   (|lassocShiftWithFunction| (LIST (|devaluate| #0#))
                       (HGET |$ConstructorCache| '|Reference|)
                       '|domainEqualList|)
                   |Reference|)
             (|CDRwithIncrement| #1#))
            ('T
             (UNWIND-PROTECT
               (PROG1 (|Reference;| #0#) (LETT #1# T |Reference|))
               (COND
                 ((NOT #1#) (HREM |$ConstructorCache| '|Reference|))))))))))) 

(DEFUN |Reference;| (|#1|)
  (PROG (|dv$1| |dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$1| (|devaluate| |#1|) . #0=(|Reference|))
        (LETT |dv$| (LIST '|Reference| |dv$1|) . #0#)
        (LETT $ (|newShell| 23) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$|
                  (|buildPredVector| 0 0
                      (LIST (|HasCategory| |#1| '(|SetCategory|)))) . #0#))
        (|haddProp| |$ConstructorCache| '|Reference| (LIST |dv$1|)
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 |#1|)
        (|setShellEntry| $ 7 (|Record| (|:| |value| |#1|)))
        (COND
          ((|testBitVector| |pv$| 1)
           (|setShellEntry| $ 21
               (CONS (|dispatchFunction| |REF;coerce;$Of;7|) $))))
        $)))) 

(MAKEPROP '|Reference| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) '|Rep| (|Boolean|)
             |REF;=;2$B;1| |REF;ref;S$;2| |REF;elt;$S;3|
             |REF;setelt;$2S;4| |REF;deref;$S;5| |REF;setref;$2S;6|
             (|String|) (|OutputForm|) (0 . |message|) (5 . |coerce|)
             (|List| $) (10 . |prefix|) (16 . |coerce|)
             (|SingleInteger|))
          '#(~= 21 |setref| 27 |setelt| 33 |ref| 39 |latex| 44 |hash|
             49 |elt| 54 |deref| 59 |coerce| 64 = 69)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(1 0 1 1))
                (CONS '#(|SetCategory&| NIL |BasicType&| NIL)
                      (CONS '#((|SetCategory|) (|Type|) (|BasicType|)
                               (|CoercibleTo| 16))
                            (|makeByteWordVec2| 22
                                '(1 16 0 15 17 1 6 16 0 18 2 16 0 0 19
                                  20 1 0 16 0 21 2 1 8 0 0 1 2 0 6 0 6
                                  14 2 0 6 0 6 12 1 0 0 6 10 1 1 15 0 1
                                  1 1 22 0 1 1 0 6 0 11 1 0 6 0 13 1 1
                                  16 0 21 2 0 8 0 0 9)))))
          '|lookupComplete|)) 
