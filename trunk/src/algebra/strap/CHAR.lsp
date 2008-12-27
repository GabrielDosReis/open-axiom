
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
  (PROG (#0=#:G1401)
    (RETURN
      (CODE-CHAR
          (PROG1 (LETT #0# (- |n| 1) |CHAR;index;Pi$;4|)
            (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#)))))) 

(DEFUN |CHAR;lookup;$Pi;5| (|c| $)
  (PROG (#0=#:G1403)
    (RETURN
      (PROG1 (LETT #0# (+ 1 (CHAR-CODE |c|)) |CHAR;lookup;$Pi;5|)
        (|check-subtype| (> #0# 0) '(|PositiveInteger|) #0#))))) 

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
  (SPADCALL |c| (|spadConstant| $ 23) (|getShellEntry| $ 25))) 

(DEFUN |CHAR;hexDigit?;$B;14| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 27) (|getShellEntry| $ 25))) 

(DEFUN |CHAR;upperCase?;$B;15| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 29) (|getShellEntry| $ 25))) 

(DEFUN |CHAR;lowerCase?;$B;16| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 31) (|getShellEntry| $ 25))) 

(DEFUN |CHAR;alphabetic?;$B;17| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 33) (|getShellEntry| $ 25))) 

(DEFUN |CHAR;alphanumeric?;$B;18| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 35) (|getShellEntry| $ 25))) 

(DEFUN |CHAR;latex;$S;19| (|c| $)
  (STRCONC "\\mbox{`" (STRCONC (MAKE-FULL-CVEC 1 |c|) "'}"))) 

(DEFUN |CHAR;char;S$;20| (|s| $)
  (COND
    ((EQL (QCSIZE |s|) 1)
     (SPADCALL |s| (SPADCALL |s| (|getShellEntry| $ 40))
         (|getShellEntry| $ 41)))
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
      (PROG (#0=#:G1424)
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
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
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
             |random| 65 |quote| 69 |ord| 73 |min| 78 |max| 88
             |lowerCase?| 98 |lowerCase| 103 |lookup| 108 |latex| 113
             |index| 118 |hexDigit?| 123 |hash| 128 |escape| 133
             |digit?| 137 |coerce| 142 |char| 147 |alphanumeric?| 157
             |alphabetic?| 162 >= 167 > 173 = 179 <= 185 < 191)
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
                                  0 9 10 0 0 0 16 0 0 0 18 1 0 9 0 14 0
                                  0 0 1 2 0 0 0 0 1 0 0 0 1 2 0 0 0 0 1
                                  1 0 6 0 32 1 0 0 0 44 1 0 12 0 15 1 0
                                  37 0 38 1 0 0 12 13 1 0 6 0 28 1 0 45
                                  0 1 0 0 0 19 1 0 6 0 26 1 0 20 0 21 1
                                  0 0 9 11 1 0 0 37 42 1 0 6 0 36 1 0 6
                                  0 34 2 0 6 0 0 1 2 0 6 0 0 1 2 0 6 0
                                  0 7 2 0 6 0 0 1 2 0 6 0 0 8)))))
          '|lookupComplete|)) 

(MAKEPROP '|Character| 'NILADIC T) 
