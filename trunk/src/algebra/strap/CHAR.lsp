
(/VERSIONCHECK 2) 

(PUT '|CHAR;=;2$B;1| '|SPADreplace| 'CHAR=) 

(DEFUN |CHAR;=;2$B;1| (|a| |b| $) (CHAR= |a| |b|)) 

(PUT '|CHAR;<;2$B;2| '|SPADreplace| 'CHAR<) 

(DEFUN |CHAR;<;2$B;2| (|a| |b| $) (CHAR< |a| |b|)) 

(PUT '|CHAR;size;Nni;3| '|SPADreplace| '(XLAM NIL 256)) 

(DEFUN |CHAR;size;Nni;3| ($) 256) 

(DEFUN |CHAR;index;Pi$;4| (|n| $)
  (PROG (#0=#:G1389)
    (RETURN
      (SPADCALL
          (PROG1 (LETT #0# (- |n| 1) |CHAR;index;Pi$;4|)
            (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#))
          (QREFELT $ 11))))) 

(DEFUN |CHAR;lookup;$Pi;5| (|c| $)
  (PROG (#0=#:G1391)
    (RETURN
      (PROG1 (LETT #0# (+ 1 (SPADCALL |c| (QREFELT $ 14)))
                   |CHAR;lookup;$Pi;5|)
        (|check-subtype| (> #0# 0) '(|PositiveInteger|) #0#))))) 

(PUT '|CHAR;char;Nni$;6| '|SPADreplace| 'CODE-CHAR) 

(DEFUN |CHAR;char;Nni$;6| (|n| $) (CODE-CHAR |n|)) 

(PUT '|CHAR;ord;$Nni;7| '|SPADreplace| 'CHAR-CODE) 

(DEFUN |CHAR;ord;$Nni;7| (|c| $) (CHAR-CODE |c|)) 

(DEFUN |CHAR;random;$;8| ($)
  (SPADCALL (RANDOM (SPADCALL (QREFELT $ 10))) (QREFELT $ 11))) 

(PUT '|CHAR;space;$;9| '|SPADreplace| '(XLAM NIL (CHAR "   " 0))) 

(DEFUN |CHAR;space;$;9| ($) (CHAR "   " 0)) 

(PUT '|CHAR;quote;$;10| '|SPADreplace| '(XLAM NIL (CHAR "\" " 0))) 

(DEFUN |CHAR;quote;$;10| ($) (CHAR "\" " 0)) 

(PUT '|CHAR;escape;$;11| '|SPADreplace| '(XLAM NIL (CHAR "_ " 0))) 

(DEFUN |CHAR;escape;$;11| ($) (CHAR "_ " 0)) 

(PUT '|CHAR;coerce;$Of;12| '|SPADreplace| '(XLAM (|c|) |c|)) 

(DEFUN |CHAR;coerce;$Of;12| (|c| $) |c|) 

(DEFUN |CHAR;digit?;$B;13| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 23) (QREFELT $ 25))) 

(DEFUN |CHAR;hexDigit?;$B;14| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 27) (QREFELT $ 25))) 

(DEFUN |CHAR;upperCase?;$B;15| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 29) (QREFELT $ 25))) 

(DEFUN |CHAR;lowerCase?;$B;16| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 31) (QREFELT $ 25))) 

(DEFUN |CHAR;alphabetic?;$B;17| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 33) (QREFELT $ 25))) 

(DEFUN |CHAR;alphanumeric?;$B;18| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 35) (QREFELT $ 25))) 

(DEFUN |CHAR;latex;$S;19| (|c| $)
  (STRCONC "\\mbox{`" (STRCONC (MAKE-FULL-CVEC 1 |c|) "'}"))) 

(DEFUN |CHAR;char;S$;20| (|s| $)
  (COND
    ((EQL (QCSIZE |s|) 1)
     (SPADCALL |s| (SPADCALL |s| (QREFELT $ 40)) (QREFELT $ 41)))
    ('T (|userError| "String is not a single character")))) 

(PUT '|CHAR;upperCase;2$;21| '|SPADreplace| 'CHAR-UPCASE) 

(DEFUN |CHAR;upperCase;2$;21| (|c| $) (CHAR-UPCASE |c|)) 

(PUT '|CHAR;lowerCase;2$;22| '|SPADreplace| 'CHAR-DOWNCASE) 

(DEFUN |CHAR;lowerCase;2$;22| (|c| $) (CHAR-DOWNCASE |c|)) 

(DEFUN |Character| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1412)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|Character|)
                   |Character|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Character|
                                   (LIST
                                    (CONS NIL (CONS 1 (|Character;|))))))
                 (LETT #0# T |Character|))
               (COND
                 ((NOT #0#) (HREM |$ConstructorCache| '|Character|))))))))))) 

(DEFUN |Character;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|Character|) . #0=(|Character|))
        (LETT $ (|newShell| 46) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Character| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|Character| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Boolean|) |CHAR;=;2$B;1|
             |CHAR;<;2$B;2| (|NonNegativeInteger|) |CHAR;size;Nni;3|
             |CHAR;char;Nni$;6| (|PositiveInteger|) |CHAR;index;Pi$;4|
             |CHAR;ord;$Nni;7| |CHAR;lookup;$Pi;5| |CHAR;random;$;8|
             |CHAR;space;$;9| |CHAR;quote;$;10| |CHAR;escape;$;11|
             (|OutputForm|) |CHAR;coerce;$Of;12| (|CharacterClass|)
             (0 . |digit|) (|Character|) (4 . |member?|)
             |CHAR;digit?;$B;13| (10 . |hexDigit|)
             |CHAR;hexDigit?;$B;14| (14 . |upperCase|)
             |CHAR;upperCase?;$B;15| (18 . |lowerCase|)
             |CHAR;lowerCase?;$B;16| (22 . |alphabetic|)
             |CHAR;alphabetic?;$B;17| (26 . |alphanumeric|)
             |CHAR;alphanumeric?;$B;18| (|String|) |CHAR;latex;$S;19|
             (|Integer|) (30 . |minIndex|) (35 . |elt|)
             |CHAR;char;S$;20| |CHAR;upperCase;2$;21|
             |CHAR;lowerCase;2$;22| (|SingleInteger|))
          '#(~= 41 |upperCase?| 47 |upperCase| 52 |space| 57 |size| 61
             |random| 65 |quote| 69 |ord| 73 |min| 78 |max| 84
             |lowerCase?| 90 |lowerCase| 95 |lookup| 100 |latex| 105
             |index| 110 |hexDigit?| 115 |hash| 120 |escape| 125
             |digit?| 129 |coerce| 134 |char| 139 |alphanumeric?| 149
             |alphabetic?| 154 >= 159 > 165 = 171 <= 177 < 183)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0))
                (CONS '#(NIL |OrderedSet&| NIL |SetCategory&|
                         |BasicType&| NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|Finite|) (|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 20))
                            (|makeByteWordVec2| 45
                                '(0 22 0 23 2 22 6 24 0 25 0 22 0 27 0
                                  22 0 29 0 22 0 31 0 22 0 33 0 22 0 35
                                  1 37 39 0 40 2 37 24 0 39 41 2 0 6 0
                                  0 1 1 0 6 0 30 1 0 0 0 43 0 0 0 17 0
                                  0 9 10 0 0 0 16 0 0 0 18 1 0 9 0 14 2
                                  0 0 0 0 1 2 0 0 0 0 1 1 0 6 0 32 1 0
                                  0 0 44 1 0 12 0 15 1 0 37 0 38 1 0 0
                                  12 13 1 0 6 0 28 1 0 45 0 1 0 0 0 19
                                  1 0 6 0 26 1 0 20 0 21 1 0 0 37 42 1
                                  0 0 9 11 1 0 6 0 36 1 0 6 0 34 2 0 6
                                  0 0 1 2 0 6 0 0 1 2 0 6 0 0 7 2 0 6 0
                                  0 1 2 0 6 0 0 8)))))
          '|lookupComplete|)) 

(MAKEPROP '|Character| 'NILADIC T) 
