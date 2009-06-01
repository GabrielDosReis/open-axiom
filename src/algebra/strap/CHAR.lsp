
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;=;2$B;1|)) 

(PUT '|CHAR;=;2$B;1| '|SPADreplace| 'CHAR=) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;<;2$B;2|)) 

(PUT '|CHAR;<;2$B;2| '|SPADreplace| 'CHAR<) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;>;2$B;3|)) 

(PUT '|CHAR;>;2$B;3| '|SPADreplace| 'CHAR>) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;<=;2$B;4|)) 

(PUT '|CHAR;<=;2$B;4| '|SPADreplace| 'CHAR<=) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;>=;2$B;5|)) 

(PUT '|CHAR;>=;2$B;5| '|SPADreplace| 'CHAR>=) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |CHAR;size;Nni;6|)) 

(PUT '|CHAR;size;Nni;6| '|SPADreplace| '(XLAM NIL 256)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Char|)
                |CHAR;index;Pi$;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) (|%IntegerSection| 1))
                |CHAR;lookup;$Pi;8|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Shell|) |%Char|)
                |CHAR;char;Nni$;9|)) 

(PUT '|CHAR;char;Nni$;9| '|SPADreplace| 'CODE-CHAR) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) (|%IntegerSection| 0))
                |CHAR;ord;$Nni;10|)) 

(PUT '|CHAR;ord;$Nni;10| '|SPADreplace| 'CHAR-CODE) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;random;$;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;space;$;12|)) 

(PUT '|CHAR;space;$;12| '|SPADreplace| '(XLAM NIL (CHAR "   " 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;quote;$;13|)) 

(PUT '|CHAR;quote;$;13| '|SPADreplace| '(XLAM NIL (CHAR "\" " 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;escape;$;14|)) 

(PUT '|CHAR;escape;$;14| '|SPADreplace| '(XLAM NIL (CHAR "_ " 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Thing|)
                |CHAR;coerce;$Of;15|)) 

(PUT '|CHAR;coerce;$Of;15| '|SPADreplace| '(XLAM (|c|) |c|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;digit?;$B;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;hexDigit?;$B;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;upperCase?;$B;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;lowerCase?;$B;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;alphabetic?;$B;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;alphanumeric?;$B;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%String|)
                |CHAR;latex;$S;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Char|)
                |CHAR;char;S$;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Char|)
                |CHAR;upperCase;2$;24|)) 

(PUT '|CHAR;upperCase;2$;24| '|SPADreplace| 'CHAR-UPCASE) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Char|)
                |CHAR;lowerCase;2$;25|)) 

(PUT '|CHAR;lowerCase;2$;25| '|SPADreplace| 'CHAR-DOWNCASE) 

(DEFUN |CHAR;=;2$B;1| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR= |a| |b|)) 

(DEFUN |CHAR;<;2$B;2| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR< |a| |b|)) 

(DEFUN |CHAR;>;2$B;3| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR> |a| |b|)) 

(DEFUN |CHAR;<=;2$B;4| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR<= |a| |b|)) 

(DEFUN |CHAR;>=;2$B;5| (|a| |b| $)
  (DECLARE (IGNORE $))
  (CHAR>= |a| |b|)) 

(DEFUN |CHAR;size;Nni;6| ($) (DECLARE (IGNORE $)) 256) 

(DEFUN |CHAR;index;Pi$;7| (|n| $)
  (PROG (#0=#:G1405)
    (RETURN
      (CODE-CHAR
          (PROG1 (LETT #0# (- |n| 1) |CHAR;index;Pi$;7|)
            (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#)))))) 

(DEFUN |CHAR;lookup;$Pi;8| (|c| $)
  (PROG (#0=#:G1407)
    (RETURN
      (PROG1 (LETT #0# (+ 1 (CHAR-CODE |c|)) |CHAR;lookup;$Pi;8|)
        (|check-subtype| (< 0 #0#) '(|PositiveInteger|) #0#))))) 

(DEFUN |CHAR;char;Nni$;9| (|n| $)
  (DECLARE (IGNORE $))
  (CODE-CHAR |n|)) 

(DEFUN |CHAR;ord;$Nni;10| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-CODE |c|)) 

(DEFUN |CHAR;random;$;11| ($) (CODE-CHAR (RANDOM 256))) 

(DEFUN |CHAR;space;$;12| ($) (DECLARE (IGNORE $)) (CHAR "   " 0)) 

(DEFUN |CHAR;quote;$;13| ($) (DECLARE (IGNORE $)) (CHAR "\" " 0)) 

(DEFUN |CHAR;escape;$;14| ($) (DECLARE (IGNORE $)) (CHAR "_ " 0)) 

(DEFUN |CHAR;coerce;$Of;15| (|c| $) (DECLARE (IGNORE $)) |c|) 

(DEFUN |CHAR;digit?;$B;16| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 32) (|getShellEntry| $ 34))) 

(DEFUN |CHAR;hexDigit?;$B;17| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 36) (|getShellEntry| $ 34))) 

(DEFUN |CHAR;upperCase?;$B;18| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 38) (|getShellEntry| $ 34))) 

(DEFUN |CHAR;lowerCase?;$B;19| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 40) (|getShellEntry| $ 34))) 

(DEFUN |CHAR;alphabetic?;$B;20| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 42) (|getShellEntry| $ 34))) 

(DEFUN |CHAR;alphanumeric?;$B;21| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 44) (|getShellEntry| $ 34))) 

(DEFUN |CHAR;latex;$S;22| (|c| $)
  (STRCONC "\\mbox{`" (STRCONC (MAKE-FULL-CVEC 1 |c|) "'}"))) 

(DEFUN |CHAR;char;S$;23| (|s| $)
  (COND
    ((EQL (QCSIZE |s|) 1)
     (SPADCALL |s| (SPADCALL |s| (|getShellEntry| $ 52))
         (|getShellEntry| $ 53)))
    ('T (|userError| "String is not a single character")))) 

(DEFUN |CHAR;upperCase;2$;24| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-UPCASE |c|)) 

(DEFUN |CHAR;lowerCase;2$;25| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-DOWNCASE |c|)) 

(DEFUN |Character| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1428)
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
        (LETT $ (|newShell| 58) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Character| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|Character| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Boolean|) |CHAR;=;2$B;1|
             |CHAR;<;2$B;2| |CHAR;>;2$B;3| |CHAR;<=;2$B;4|
             |CHAR;>=;2$B;5| (|NonNegativeInteger|) |CHAR;size;Nni;6|
             (|PositiveInteger|) (0 . |One|) (4 . |One|) (|Integer|)
             (8 . -) |CHAR;char;Nni$;9| |CHAR;index;Pi$;7|
             |CHAR;ord;$Nni;10| (14 . +) |CHAR;lookup;$Pi;8|
             (20 . |random|) |CHAR;random;$;11| |CHAR;space;$;12|
             |CHAR;quote;$;13| |CHAR;escape;$;14| (|OutputForm|)
             |CHAR;coerce;$Of;15| (|CharacterClass|) (25 . |digit|)
             (|Character|) (29 . |member?|) |CHAR;digit?;$B;16|
             (35 . |hexDigit|) |CHAR;hexDigit?;$B;17|
             (39 . |upperCase|) |CHAR;upperCase?;$B;18|
             (43 . |lowerCase|) |CHAR;lowerCase?;$B;19|
             (47 . |alphabetic|) |CHAR;alphabetic?;$B;20|
             (51 . |alphanumeric|) |CHAR;alphanumeric?;$B;21|
             (|String|) (55 . |new|) (61 . |concat|) |CHAR;latex;$S;22|
             (67 . |#|) (72 . |one?|) (77 . |minIndex|) (82 . |elt|)
             |CHAR;char;S$;23| |CHAR;upperCase;2$;24|
             |CHAR;lowerCase;2$;25| (|SingleInteger|))
          '#(~= 88 |upperCase?| 94 |upperCase| 99 |space| 104 |size|
             108 |random| 112 |quote| 116 |ord| 120 |min| 125 |max| 135
             |lowerCase?| 145 |lowerCase| 150 |lookup| 155 |latex| 160
             |index| 165 |hexDigit?| 170 |hash| 175 |escape| 180
             |digit?| 184 |coerce| 189 |char| 194 |before?| 204
             |alphanumeric?| 210 |alphabetic?| 215 >= 220 > 226 = 232
             <= 238 < 244)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0))
                (CONS '#(NIL |OrderedSet&| NIL |SetCategory&|
                         |BasicType&| NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|Finite|) (|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 29))
                            (|makeByteWordVec2| 57
                                '(0 14 0 15 0 12 0 16 2 17 0 0 0 18 2
                                  12 0 0 0 22 1 12 0 0 24 0 31 0 32 2
                                  31 6 33 0 34 0 31 0 36 0 31 0 38 0 31
                                  0 40 0 31 0 42 0 31 0 44 2 46 0 12 33
                                  47 2 46 0 0 0 48 1 46 12 0 50 1 12 6
                                  0 51 1 46 17 0 52 2 46 33 0 17 53 2 0
                                  6 0 0 1 1 0 6 0 39 1 0 0 0 55 0 0 0
                                  26 0 0 12 13 0 0 0 25 0 0 0 27 1 0 12
                                  0 21 0 0 0 1 2 0 0 0 0 1 0 0 0 1 2 0
                                  0 0 0 1 1 0 6 0 41 1 0 0 0 56 1 0 14
                                  0 23 1 0 46 0 49 1 0 0 14 20 1 0 6 0
                                  37 1 0 57 0 1 0 0 0 28 1 0 6 0 35 1 0
                                  29 0 30 1 0 0 46 54 1 0 0 12 19 2 0 6
                                  0 0 1 1 0 6 0 45 1 0 6 0 43 2 0 6 0 0
                                  11 2 0 6 0 0 9 2 0 6 0 0 7 2 0 6 0 0
                                  10 2 0 6 0 0 8)))))
          '|lookupComplete|)) 

(MAKEPROP '|Character| 'NILADIC T) 
