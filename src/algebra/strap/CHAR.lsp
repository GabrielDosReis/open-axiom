
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;=;2$B;1|)) 

(PUT '|CHAR;=;2$B;1| '|SPADreplace| '|%ceq|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;<;2$B;2|)) 

(PUT '|CHAR;<;2$B;2| '|SPADreplace| '|%clt|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;>;2$B;3|)) 

(PUT '|CHAR;>;2$B;3| '|SPADreplace| '|%cgt|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;<=;2$B;4|)) 

(PUT '|CHAR;<=;2$B;4| '|SPADreplace| '|%cle|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Char| |%Shell|) |%Boolean|)
                |CHAR;>=;2$B;5|)) 

(PUT '|CHAR;>=;2$B;5| '|SPADreplace| '|%cge|) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |CHAR;size;Nni;6|)) 

(PUT '|CHAR;size;Nni;6| '|SPADreplace| '(XLAM NIL 256)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Char|)
                |CHAR;index;Pi$;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) (|%IntegerSection| 1))
                |CHAR;lookup;$Pi;8|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 0) |%Shell|) |%Char|)
                |CHAR;char;Nni$;9|)) 

(PUT '|CHAR;char;Nni$;9| '|SPADreplace| '|%i2c|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) (|%IntegerSection| 0))
                |CHAR;ord;$Nni;10|)) 

(PUT '|CHAR;ord;$Nni;10| '|SPADreplace| '|%c2i|) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;random;$;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;space;$;12|)) 

(PUT '|CHAR;space;$;12| '|SPADreplace| '(XLAM NIL (|%ccst| " "))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;quote;$;13|)) 

(PUT '|CHAR;quote;$;13| '|SPADreplace| '(XLAM NIL (|%ccst| "\""))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;underscore;$;14|)) 

(PUT '|CHAR;underscore;$;14| '|SPADreplace| '(XLAM NIL (|%ccst| "_"))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;newline;$;15|)) 

(PUT '|CHAR;newline;$;15| '|SPADreplace| '(XLAM NIL (|%ccst| "\\n"))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|)
                |CHAR;carriageReturn;$;16|)) 

(PUT '|CHAR;carriageReturn;$;16| '|SPADreplace|
     '(XLAM NIL (|%i2c| 13))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;linefeed;$;17|)) 

(PUT '|CHAR;linefeed;$;17| '|SPADreplace| '(XLAM NIL (|%i2c| 10))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;formfeed;$;18|)) 

(PUT '|CHAR;formfeed;$;18| '|SPADreplace| '(XLAM NIL (|%i2c| 12))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;backspace;$;19|)) 

(PUT '|CHAR;backspace;$;19| '|SPADreplace| '(XLAM NIL (|%i2c| 8))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|)
                |CHAR;horizontalTab;$;20|)) 

(PUT '|CHAR;horizontalTab;$;20| '|SPADreplace| '(XLAM NIL (|%i2c| 9))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;verticalTab;$;21|)) 

(PUT '|CHAR;verticalTab;$;21| '|SPADreplace| '(XLAM NIL (|%i2c| 11))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Char|) |CHAR;escape;$;22|)) 

(PUT '|CHAR;escape;$;22| '|SPADreplace| '(XLAM NIL (|%i2c| 27))) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Thing|)
                |CHAR;coerce;$Of;23|)) 

(PUT '|CHAR;coerce;$Of;23| '|SPADreplace| '(XLAM (|c|) |c|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;digit?;$B;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;hexDigit?;$B;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;upperCase?;$B;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;lowerCase?;$B;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;alphabetic?;$B;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Boolean|)
                |CHAR;alphanumeric?;$B;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%String|)
                |CHAR;latex;$S;30|)) 

(PUT '|CHAR;latex;$S;30| '|SPADreplace|
     '(XLAM (|c|)
            (|%strconc| "\\mbox{`"
                (|%strconc| (MAKE-FULL-CVEC 1 |c|) "'}")))) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Char|)
                |CHAR;char;S$;31|)) 

(PUT '|CHAR;char;S$;31| '|SPADreplace| '|%s2c|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Char|)
                |CHAR;upperCase;2$;32|)) 

(PUT '|CHAR;upperCase;2$;32| '|SPADreplace| '|%cup|) 

(DECLAIM (FTYPE (FUNCTION (|%Char| |%Shell|) |%Char|)
                |CHAR;lowerCase;2$;33|)) 

(PUT '|CHAR;lowerCase;2$;33| '|SPADreplace| '|%cdown|) 

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
  (CODE-CHAR (LET ((#0=#:G1379 (- |n| 1)))
               (|check-subtype| (NOT (MINUSP #0#))
                   '(|NonNegativeInteger|) #0#)))) 

(DEFUN |CHAR;lookup;$Pi;8| (|c| $)
  (LET ((#0=#:G1381 (+ 1 (CHAR-CODE |c|))))
    (|check-subtype| (PLUSP #0#) '(|PositiveInteger|) #0#))) 

(DEFUN |CHAR;char;Nni$;9| (|n| $)
  (DECLARE (IGNORE $))
  (CODE-CHAR |n|)) 

(DEFUN |CHAR;ord;$Nni;10| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-CODE |c|)) 

(DEFUN |CHAR;random;$;11| ($) (CODE-CHAR (RANDOM 256))) 

(DEFUN |CHAR;space;$;12| ($) (DECLARE (IGNORE $)) #\Space) 

(DEFUN |CHAR;quote;$;13| ($) (DECLARE (IGNORE $)) #\") 

(DEFUN |CHAR;underscore;$;14| ($) (DECLARE (IGNORE $)) #\_) 

(DEFUN |CHAR;newline;$;15| ($) (DECLARE (IGNORE $)) #\Newline) 

(DEFUN |CHAR;carriageReturn;$;16| ($)
  (DECLARE (IGNORE $))
  (CODE-CHAR 13)) 

(DEFUN |CHAR;linefeed;$;17| ($) (DECLARE (IGNORE $)) (CODE-CHAR 10)) 

(DEFUN |CHAR;formfeed;$;18| ($) (DECLARE (IGNORE $)) (CODE-CHAR 12)) 

(DEFUN |CHAR;backspace;$;19| ($) (DECLARE (IGNORE $)) (CODE-CHAR 8)) 

(DEFUN |CHAR;horizontalTab;$;20| ($)
  (DECLARE (IGNORE $))
  (CODE-CHAR 9)) 

(DEFUN |CHAR;verticalTab;$;21| ($)
  (DECLARE (IGNORE $))
  (CODE-CHAR 11)) 

(DEFUN |CHAR;escape;$;22| ($) (DECLARE (IGNORE $)) (CODE-CHAR 27)) 

(DEFUN |CHAR;coerce;$Of;23| (|c| $) (DECLARE (IGNORE $)) |c|) 

(DEFUN |CHAR;digit?;$B;24| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 40) (|shellEntry| $ 42))) 

(DEFUN |CHAR;hexDigit?;$B;25| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 44) (|shellEntry| $ 42))) 

(DEFUN |CHAR;upperCase?;$B;26| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 46) (|shellEntry| $ 42))) 

(DEFUN |CHAR;lowerCase?;$B;27| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 48) (|shellEntry| $ 42))) 

(DEFUN |CHAR;alphabetic?;$B;28| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 50) (|shellEntry| $ 42))) 

(DEFUN |CHAR;alphanumeric?;$B;29| (|c| $)
  (SPADCALL |c| (|spadConstant| $ 52) (|shellEntry| $ 42))) 

(DEFUN |CHAR;latex;$S;30| (|c| $)
  (DECLARE (IGNORE $))
  (STRCONC "\\mbox{`" (STRCONC (MAKE-FULL-CVEC 1 |c|) "'}"))) 

(DEFUN |CHAR;char;S$;31| (|s| $)
  (DECLARE (IGNORE $))
  (|stringToChar| |s|)) 

(DEFUN |CHAR;upperCase;2$;32| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-UPCASE |c|)) 

(DEFUN |CHAR;lowerCase;2$;33| (|c| $)
  (DECLARE (IGNORE $))
  (CHAR-DOWNCASE |c|)) 

(DEFUN |Character| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((#0=#:G1408 (HGET |$ConstructorCache| '|Character|)))
    (COND
      (#0# (|CDRwithIncrement| (CDAR #0#)))
      (T (UNWIND-PROTECT
           (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Character|
                               (LIST (CONS NIL (CONS 1 (|Character;|))))))
             (SETQ #0# T))
           (COND ((NOT #0#) (HREM |$ConstructorCache| '|Character|)))))))) 

(DEFUN |Character;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| '(|Character|)) ($ (|newShell| 62))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|haddProp| |$ConstructorCache| '|Character| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    $)) 

(MAKEPROP '|Character| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Boolean|) |CHAR;=;2$B;1|
             |CHAR;<;2$B;2| |CHAR;>;2$B;3| |CHAR;<=;2$B;4|
             |CHAR;>=;2$B;5| (|NonNegativeInteger|) |CHAR;size;Nni;6|
             (|PositiveInteger|) (0 . |One|) (4 . |One|) (|Integer|)
             (8 . -) |CHAR;char;Nni$;9| |CHAR;index;Pi$;7|
             |CHAR;ord;$Nni;10| (14 . +) |CHAR;lookup;$Pi;8|
             (20 . |random|) |CHAR;random;$;11|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;space;$;12|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;quote;$;13|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;underscore;$;14|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;newline;$;15|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction|
                                |CHAR;carriageReturn;$;16|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;linefeed;$;17|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;formfeed;$;18|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;backspace;$;19|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction|
                                |CHAR;horizontalTab;$;20|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction|
                                |CHAR;verticalTab;$;21|)
                            $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |CHAR;escape;$;22|) $))
             (|OutputForm|) |CHAR;coerce;$Of;23| (|CharacterClass|)
             (25 . |digit|) (|Character|) (29 . |member?|)
             |CHAR;digit?;$B;24| (35 . |hexDigit|)
             |CHAR;hexDigit?;$B;25| (39 . |upperCase|)
             |CHAR;upperCase?;$B;26| (43 . |lowerCase|)
             |CHAR;lowerCase?;$B;27| (47 . |alphabetic|)
             |CHAR;alphabetic?;$B;28| (51 . |alphanumeric|)
             |CHAR;alphanumeric?;$B;29| (|String|) (55 . |new|)
             (61 . |concat|) |CHAR;latex;$S;30| |CHAR;char;S$;31|
             |CHAR;upperCase;2$;32| |CHAR;lowerCase;2$;33|
             (|SingleInteger|))
          '#(~= 67 |verticalTab| 73 |upperCase?| 77 |upperCase| 82
             |underscore| 87 |space| 91 |size| 95 |random| 99 |quote|
             103 |ord| 107 |newline| 112 |min| 116 |max| 126
             |lowerCase?| 136 |lowerCase| 141 |lookup| 146 |linefeed|
             151 |latex| 155 |index| 160 |horizontalTab| 165
             |hexDigit?| 169 |hash| 174 |formfeed| 179 |escape| 183
             |digit?| 187 |coerce| 192 |char| 197 |carriageReturn| 207
             |before?| 211 |backspace| 217 |alphanumeric?| 221
             |alphabetic?| 226 >= 231 > 237 = 243 <= 249 < 255)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0))
                (CONS '#(NIL NIL NIL |SetCategory&| |OrderedType&|
                         |BasicType&| NIL NIL)
                      (CONS '#((|OrderedFinite|) (|OrderedSet|)
                               (|Finite|) (|SetCategory|)
                               (|OrderedType|) (|BasicType|) (|Type|)
                               (|CoercibleTo| 37))
                            (|makeByteWordVec2| 61
                                '(0 14 0 15 0 12 0 16 2 17 0 0 0 18 2
                                  12 0 0 0 22 1 12 0 0 24 0 39 0 40 2
                                  39 6 41 0 42 0 39 0 44 0 39 0 46 0 39
                                  0 48 0 39 0 50 0 39 0 52 2 54 0 12 41
                                  55 2 54 0 0 0 56 2 0 6 0 0 1 0 0 0 35
                                  1 0 6 0 47 1 0 0 0 59 0 0 0 28 0 0 0
                                  26 0 0 12 13 0 0 0 25 0 0 0 27 1 0 12
                                  0 21 0 0 0 29 0 0 0 1 2 0 0 0 0 1 0 0
                                  0 1 2 0 0 0 0 1 1 0 6 0 49 1 0 0 0 60
                                  1 0 14 0 23 0 0 0 31 1 0 54 0 57 1 0
                                  0 14 20 0 0 0 34 1 0 6 0 45 1 0 61 0
                                  1 0 0 0 32 0 0 0 36 1 0 6 0 43 1 0 37
                                  0 38 1 0 0 54 58 1 0 0 12 19 0 0 0 30
                                  2 0 6 0 0 1 0 0 0 33 1 0 6 0 53 1 0 6
                                  0 51 2 0 6 0 0 11 2 0 6 0 0 9 2 0 6 0
                                  0 7 2 0 6 0 0 10 2 0 6 0 0 8)))))
          '|lookupComplete|)) 

(MAKEPROP '|Character| 'NILADIC T) 
