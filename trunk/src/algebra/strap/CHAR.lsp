
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;=;2$B;1|)) 

(PUT '|CHAR;=;2$B;1| '|SPADreplace| 'CHAR=) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;<;2$B;2|)) 

(PUT '|CHAR;<;2$B;2| '|SPADreplace| 'CHAR<) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |CHAR;size;Nni;3|)) 

(PUT '|CHAR;size;Nni;3| '|SPADreplace| '(XLAM NIL 256)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Char|)
                |CHAR;index;Pi$;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) (|%IntegerSection| 1))
                |CHAR;lookup;$Pi;5|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Shell|) |%Char|)
                |CHAR;char;Nni$;6|)) 

(PUT '|CHAR;char;Nni$;6| '|SPADreplace| 'CODE-CHAR) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) (|%IntegerSection| 0))
                |CHAR;ord;$Nni;7|)) 

(PUT '|CHAR;ord;$Nni;7| '|SPADreplace| 'CHAR-CODE) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;random;$;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;space;$;9|)) 

(PUT '|CHAR;space;$;9| '|SPADreplace| '(XLAM NIL (CHAR "   " 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;quote;$;10|)) 

(PUT '|CHAR;quote;$;10| '|SPADreplace| '(XLAM NIL (CHAR "\" " 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;escape;$;11|)) 

(PUT '|CHAR;escape;$;11| '|SPADreplace| '(XLAM NIL (CHAR "_ " 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Thing|)
                |CHAR;coerce;$Of;12|)) 

(PUT '|CHAR;coerce;$Of;12| '|SPADreplace| '(XLAM (|c|) |c|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;digit?;$B;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;hexDigit?;$B;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;upperCase?;$B;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;lowerCase?;$B;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;alphabetic?;$B;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;alphanumeric?;$B;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%String|)
                |CHAR;latex;$S;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Char|)
                |CHAR;char;S$;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Char|)
                |CHAR;upperCase;2$;21|)) 

(PUT '|CHAR;upperCase;2$;21| '|SPADreplace| 'CHAR-UPCASE) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Char|)
                |CHAR;lowerCase;2$;22|)) 

(PUT '|CHAR;lowerCase;2$;22| '|SPADreplace| 'CHAR-DOWNCASE) 

(DEFUN |CHAR;=;2$B;1| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR= |a| |b|)) 

(DEFUN |CHAR;<;2$B;2| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR< |a| |b|)) 

(DEFUN |CHAR;size;Nni;3| ($) (DECLARE (IGNORE $)) 256) 

(DEFUN |CHAR;index;Pi$;4| (|n| $)
  (PROG (#0=#:G1402)
    (RETURN
      (CODE-CHAR
          (PROG1 (LETT #0# (- |n| 1) |CHAR;index;Pi$;4|)
            (|check-subtype| (COND ((< #0# 0) 'NIL) ('T 'T))
                '(|NonNegativeInteger|) #0#)))))) 

(DEFUN |CHAR;lookup;$Pi;5| (|c| $)
  (PROG (#0=#:G1404)
    (RETURN
      (PROG1 (LETT #0# (+ 1 (CHAR-CODE |c|)) |CHAR;lookup;$Pi;5|)
        (|check-subtype| (< 0 #0#) '(|PositiveInteger|) #0#))))) 

(DEFUN |CHAR;char;Nni$;6| (|n| $)
  (DECLARE (IGNORE $))
  (CODE-CHAR |n|)) 

(DEFUN |CHAR;ord;$Nni;7| (|c| $) (DECLARE (IGNORE $)) (CHAR-CODE |c|)) 

(DEFUN |CHAR;random;$;8| ($) (CODE-CHAR (RANDOM 256))) 

(DEFUN |CHAR;space;$;9| ($) (DECLARE (IGNORE $)) (CHAR "   " 0)) 

(DEFUN |CHAR;quote;$;10| ($) (DECLARE (IGNORE $)) (CHAR "\" " 0)) 

(DEFUN |CHAR;escape;$;11| ($) (DECLARE (IGNORE $)) (CHAR "_ " 0)) 

(DEFUN |CHAR;coerce;$Of;12| (|c| $) (DECLARE (IGNORE $)) |c|) 

(DEFUN |CHAR;digit?;$B;13| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 29) (|getShellEntry| $ 31))) 

(DEFUN |CHAR;hexDigit?;$B;14| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 33) (|getShellEntry| $ 31))) 

(DEFUN |CHAR;upperCase?;$B;15| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 35) (|getShellEntry| $ 31))) 

(DEFUN |CHAR;lowerCase?;$B;16| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 37) (|getShellEntry| $ 31))) 

(DEFUN |CHAR;alphabetic?;$B;17| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 39) (|getShellEntry| $ 31))) 

(DEFUN |CHAR;alphanumeric?;$B;18| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 41) (|getShellEntry| $ 31))) 

(DEFUN |CHAR;latex;$S;19| (|c| $)
  (STRCONC "\\mbox{`" (STRCONC (MAKE-FULL-CVEC 1 |c|) "'}"))) 

(DEFUN |CHAR;char;S$;20| (|s| $)
  (COND
    ((EQL (QCSIZE |s|) 1)
     (SPADCALL |s| (SPADCALL |s| (|getShellEntry| $ 49))
         (|getShellEntry| $ 50)))
    ('T (|userError| "String is not a single character")))) 

(DEFUN |CHAR;upperCase;2$;21| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-UPCASE |c|)) 

(DEFUN |CHAR;lowerCase;2$;22| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-DOWNCASE |c|)) 

(DEFUN |Character| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1425)
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
        (LETT $ (|newShell| 55) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Character| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|Character| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Boolean|) |CHAR;=;2$B;1|
             |CHAR;<;2$B;2| (|NonNegativeInteger|) |CHAR;size;Nni;3|
             (|PositiveInteger|) (0 . |One|) (4 . |One|) (|Integer|)
             (8 . -) |CHAR;char;Nni$;6| |CHAR;index;Pi$;4|
             |CHAR;ord;$Nni;7| (14 . +) |CHAR;lookup;$Pi;5|
             (20 . |random|) |CHAR;random;$;8| |CHAR;space;$;9|
             |CHAR;quote;$;10| |CHAR;escape;$;11| (|OutputForm|)
             |CHAR;coerce;$Of;12| (|CharacterClass|) (25 . |digit|)
             (|Character|) (29 . |member?|) |CHAR;digit?;$B;13|
             (35 . |hexDigit|) |CHAR;hexDigit?;$B;14|
             (39 . |upperCase|) |CHAR;upperCase?;$B;15|
             (43 . |lowerCase|) |CHAR;lowerCase?;$B;16|
             (47 . |alphabetic|) |CHAR;alphabetic?;$B;17|
             (51 . |alphanumeric|) |CHAR;alphanumeric?;$B;18|
             (|String|) (55 . |new|) (61 . |concat|) |CHAR;latex;$S;19|
             (67 . |#|) (72 . |one?|) (77 . |minIndex|) (82 . |elt|)
             |CHAR;char;S$;20| |CHAR;upperCase;2$;21|
             |CHAR;lowerCase;2$;22| (|SingleInteger|))
          '#(~= 88 |upperCase?| 94 |upperCase| 99 |space| 104 |size|
             108 |random| 112 |quote| 116 |ord| 120 |min| 125 |max| 135
             |lowerCase?| 145 |lowerCase| 150 |lookup| 155 |latex| 160
             |index| 165 |hexDigit?| 170 |hash| 175 |escape| 180
             |digit?| 184 |coerce| 189 |char| 194 |alphanumeric?| 204
             |alphabetic?| 209 >= 214 > 220 = 226 <= 232 < 238)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0))
                (CONS '#(NIL |OrderedSet&| NIL |SetCategory&|
                         |BasicType&| NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|Finite|) (|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 26))
                            (|makeByteWordVec2| 54
                                '(0 11 0 12 0 9 0 13 2 14 0 0 0 15 2 9
                                  0 0 0 19 1 9 0 0 21 0 28 0 29 2 28 6
                                  30 0 31 0 28 0 33 0 28 0 35 0 28 0 37
                                  0 28 0 39 0 28 0 41 2 43 0 9 30 44 2
                                  43 0 0 0 45 1 43 9 0 47 1 9 6 0 48 1
                                  43 14 0 49 2 43 30 0 14 50 2 0 6 0 0
                                  1 1 0 6 0 36 1 0 0 0 52 0 0 0 23 0 0
                                  9 10 0 0 0 22 0 0 0 24 1 0 9 0 18 0 0
                                  0 1 2 0 0 0 0 1 0 0 0 1 2 0 0 0 0 1 1
                                  0 6 0 38 1 0 0 0 53 1 0 11 0 20 1 0
                                  43 0 46 1 0 0 11 17 1 0 6 0 34 1 0 54
                                  0 1 0 0 0 25 1 0 6 0 32 1 0 26 0 27 1
                                  0 0 9 16 1 0 0 43 51 1 0 6 0 42 1 0 6
                                  0 40 2 0 6 0 0 1 2 0 6 0 0 1 2 0 6 0
                                  0 7 2 0 6 0 0 1 2 0 6 0 0 8)))))
          '|lookupComplete|)) 

(MAKEPROP '|Character| 'NILADIC T) 
