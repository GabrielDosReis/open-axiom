
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Short| |%Shell|) |%Void|)
                |SINT;writeOMSingleInt|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%String|)
                |SINT;OMwrite;$S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Boolean| |%Shell|) |%String|)
                |SINT;OMwrite;$BS;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Short| |%Shell|) |%Void|)
                |SINT;OMwrite;Omd$V;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Short| |%Boolean| |%Shell|)
                    |%Void|)
                |SINT;OMwrite;Omd$BV;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SINT;reducedSystem;MM;6|)) 

(PUT '|SINT;reducedSystem;MM;6| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Thing|)
                |SINT;coerce;$Of;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Integer|)
                |SINT;convert;$I;8|)) 

(PUT '|SINT;convert;$I;8| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Short| |%Shell|) |%Short|)
                |SINT;*;I2$;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;Zero;$;10|)) 

(PUT '|SINT;Zero;$;10| '|SPADreplace| '(XLAM NIL 0)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;One;$;11|)) 

(PUT '|SINT;One;$;11| '|SPADreplace| '(XLAM NIL 1)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;base;$;12|)) 

(PUT '|SINT;base;$;12| '|SPADreplace| '(XLAM NIL 2)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;max;$;13|)) 

(PUT '|SINT;max;$;13| '|SPADreplace| '(XLAM NIL |$ShortMaximum|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;min;$;14|)) 

(PUT '|SINT;min;$;14| '|SPADreplace| '(XLAM NIL |$ShortMinimum|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;=;2$B;15|)) 

(PUT '|SINT;=;2$B;15| '|SPADreplace| 'EQL) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|) |SINT;~;2$;16|)) 

(PUT '|SINT;~;2$;16| '|SPADreplace| 'LOGNOT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;not;2$;17|)) 

(PUT '|SINT;not;2$;17| '|SPADreplace| 'LOGNOT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;/\\;3$;18|)) 

(PUT '|SINT;/\\;3$;18| '|SPADreplace| 'LOGAND) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;\\/;3$;19|)) 

(PUT '|SINT;\\/;3$;19| '|SPADreplace| 'LOGIOR) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;Not;2$;20|)) 

(PUT '|SINT;Not;2$;20| '|SPADreplace| 'LOGNOT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;And;3$;21|)) 

(PUT '|SINT;And;3$;21| '|SPADreplace| 'LOGAND) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;Or;3$;22|)) 

(PUT '|SINT;Or;3$;22| '|SPADreplace| 'LOGIOR) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;xor;3$;23|)) 

(PUT '|SINT;xor;3$;23| '|SPADreplace| 'LOGXOR) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;<;2$B;24|)) 

(PUT '|SINT;<;2$B;24| '|SPADreplace| 'QSLESSP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;inc;2$;25|)) 

(PUT '|SINT;inc;2$;25| '|SPADreplace| 'QSADD1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;dec;2$;26|)) 

(PUT '|SINT;dec;2$;26| '|SPADreplace| 'QSSUB1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|) |SINT;-;2$;27|)) 

(PUT '|SINT;-;2$;27| '|SPADreplace| 'QSMINUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;+;3$;28|)) 

(PUT '|SINT;+;3$;28| '|SPADreplace| 'QSPLUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;-;3$;29|)) 

(PUT '|SINT;-;3$;29| '|SPADreplace| 'QSDIFFERENCE) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;*;3$;30|)) 

(PUT '|SINT;*;3$;30| '|SPADreplace| 'QSTIMES) 

(DECLAIM (FTYPE (FUNCTION (|%Short| (|%IntegerSection| 0) |%Shell|)
                    |%Short|)
                |SINT;**;$Nni$;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;quo;3$;32|)) 

(PUT '|SINT;quo;3$;32| '|SPADreplace| 'QSQUOTIENT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;rem;3$;33|)) 

(PUT '|SINT;rem;3$;33| '|SPADreplace| 'QSREMAINDER) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Pair|)
                |SINT;divide;2$R;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;gcd;3$;35|)) 

(PUT '|SINT;gcd;3$;35| '|SPADreplace| 'GCD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;abs;2$;36|)) 

(PUT '|SINT;abs;2$;36| '|SPADreplace| 'QSABSVAL) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;odd?;$B;37|)) 

(PUT '|SINT;odd?;$B;37| '|SPADreplace| 'QSODDP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;zero?;$B;38|)) 

(PUT '|SINT;zero?;$B;38| '|SPADreplace| 'QSZEROP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;one?;$B;39|)) 

(PUT '|SINT;one?;$B;39| '|SPADreplace| '(XLAM (|x|) (EQL |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;max;3$;40|)) 

(PUT '|SINT;max;3$;40| '|SPADreplace| 'QSMAX) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;min;3$;41|)) 

(PUT '|SINT;min;3$;41| '|SPADreplace| 'QSMIN) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;hash;2$;42|)) 

(PUT '|SINT;hash;2$;42| '|SPADreplace| 'HASHEQ) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;length;2$;43|)) 

(PUT '|SINT;length;2$;43| '|SPADreplace| 'INTEGER-LENGTH) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;shift;3$;44|)) 

(PUT '|SINT;shift;3$;44| '|SPADreplace| 'QSLEFTSHIFT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;mulmod;4$;45|)) 

(PUT '|SINT;mulmod;4$;45| '|SPADreplace| 'QSMULTMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;addmod;4$;46|)) 

(PUT '|SINT;addmod;4$;46| '|SPADreplace| 'QSADDMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;submod;4$;47|)) 

(PUT '|SINT;submod;4$;47| '|SPADreplace| 'QSDIFMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;negative?;$B;48|)) 

(PUT '|SINT;negative?;$B;48| '|SPADreplace| 'QSMINUSP) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |SINT;reducedSystem;MVR;49|)) 

(PUT '|SINT;reducedSystem;MVR;49| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;positiveRemainder;3$;50|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |SINT;coerce;I$;51|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;random;$;52|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;random;2$;53|)) 

(PUT '|SINT;random;2$;53| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Shell|)
                |SINT;unitNormal;$R;54|)) 

(DEFUN |SINT;writeOMSingleInt| (|dev| |x| $)
  (SEQ (COND
         ((QSLESSP |x| 0)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 9))
               (SPADCALL |dev| "arith1" "unaryminus"
                   (|getShellEntry| $ 11))
               (SPADCALL |dev| (QSMINUS |x|) (|getShellEntry| $ 13))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 14)))))
         ('T (SPADCALL |dev| |x| (|getShellEntry| $ 13)))))) 

(DEFUN |SINT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 16))
                     (|getShellEntry| $ 17))
                 |SINT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 18))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 19))
           (SPADCALL |dev| (|getShellEntry| $ 20))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 16))
                     (|getShellEntry| $ 17))
                 |SINT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 19))))
           (SPADCALL |dev| (|getShellEntry| $ 20))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 18))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 19))))) 

(DEFUN |SINT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 19))))))) 

(DEFUN |SINT;reducedSystem;MM;6| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |SINT;coerce;$Of;7| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 30))) 

(DEFUN |SINT;convert;$I;8| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |SINT;*;I2$;9| (|i| |y| $)
  (QSTIMES (SPADCALL |i| (|getShellEntry| $ 33)) |y|)) 

(DEFUN |SINT;Zero;$;10| ($) (DECLARE (IGNORE $)) 0) 

(DEFUN |SINT;One;$;11| ($) (DECLARE (IGNORE $)) 1) 

(DEFUN |SINT;base;$;12| ($) (DECLARE (IGNORE $)) 2) 

(DEFUN |SINT;max;$;13| ($) (DECLARE (IGNORE $)) |$ShortMaximum|) 

(DEFUN |SINT;min;$;14| ($) (DECLARE (IGNORE $)) |$ShortMinimum|) 

(DEFUN |SINT;=;2$B;15| (|x| |y| $) (DECLARE (IGNORE $)) (EQL |x| |y|)) 

(DEFUN |SINT;~;2$;16| (|x| $) (DECLARE (IGNORE $)) (LOGNOT |x|)) 

(DEFUN |SINT;not;2$;17| (|x| $) (DECLARE (IGNORE $)) (LOGNOT |x|)) 

(DEFUN |SINT;/\\;3$;18| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGAND |x| |y|)) 

(DEFUN |SINT;\\/;3$;19| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGIOR |x| |y|)) 

(DEFUN |SINT;Not;2$;20| (|x| $) (DECLARE (IGNORE $)) (LOGNOT |x|)) 

(DEFUN |SINT;And;3$;21| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGAND |x| |y|)) 

(DEFUN |SINT;Or;3$;22| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGIOR |x| |y|)) 

(DEFUN |SINT;xor;3$;23| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGXOR |x| |y|)) 

(DEFUN |SINT;<;2$B;24| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSLESSP |x| |y|)) 

(DEFUN |SINT;inc;2$;25| (|x| $) (DECLARE (IGNORE $)) (QSADD1 |x|)) 

(DEFUN |SINT;dec;2$;26| (|x| $) (DECLARE (IGNORE $)) (QSSUB1 |x|)) 

(DEFUN |SINT;-;2$;27| (|x| $) (DECLARE (IGNORE $)) (QSMINUS |x|)) 

(DEFUN |SINT;+;3$;28| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSPLUS |x| |y|)) 

(DEFUN |SINT;-;3$;29| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSDIFFERENCE |x| |y|)) 

(DEFUN |SINT;*;3$;30| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSTIMES |x| |y|)) 

(DEFUN |SINT;**;$Nni$;31| (|x| |n| $)
  (SPADCALL (EXPT |x| |n|) (|getShellEntry| $ 33))) 

(DEFUN |SINT;quo;3$;32| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSQUOTIENT |x| |y|)) 

(DEFUN |SINT;rem;3$;33| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSREMAINDER |x| |y|)) 

(DEFUN |SINT;divide;2$R;34| (|x| |y| $)
  (CONS (QSQUOTIENT |x| |y|) (QSREMAINDER |x| |y|))) 

(DEFUN |SINT;gcd;3$;35| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GCD |x| |y|)) 

(DEFUN |SINT;abs;2$;36| (|x| $) (DECLARE (IGNORE $)) (QSABSVAL |x|)) 

(DEFUN |SINT;odd?;$B;37| (|x| $) (DECLARE (IGNORE $)) (QSODDP |x|)) 

(DEFUN |SINT;zero?;$B;38| (|x| $) (DECLARE (IGNORE $)) (QSZEROP |x|)) 

(DEFUN |SINT;one?;$B;39| (|x| $) (DECLARE (IGNORE $)) (EQL |x| 1)) 

(DEFUN |SINT;max;3$;40| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMAX |x| |y|)) 

(DEFUN |SINT;min;3$;41| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMIN |x| |y|)) 

(DEFUN |SINT;hash;2$;42| (|x| $) (DECLARE (IGNORE $)) (HASHEQ |x|)) 

(DEFUN |SINT;length;2$;43| (|x| $)
  (DECLARE (IGNORE $))
  (INTEGER-LENGTH |x|)) 

(DEFUN |SINT;shift;3$;44| (|x| |n| $)
  (DECLARE (IGNORE $))
  (QSLEFTSHIFT |x| |n|)) 

(DEFUN |SINT;mulmod;4$;45| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSMULTMOD |a| |b| |p|)) 

(DEFUN |SINT;addmod;4$;46| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSADDMOD |a| |b| |p|)) 

(DEFUN |SINT;submod;4$;47| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSDIFMOD |a| |b| |p|)) 

(DEFUN |SINT;negative?;$B;48| (|x| $)
  (DECLARE (IGNORE $))
  (QSMINUSP |x|)) 

(DEFUN |SINT;reducedSystem;MVR;49| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;50| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (QSREMAINDER |x| |n|)
                 |SINT;positiveRemainder;3$;50|)
           (EXIT (COND
                   ((QSMINUSP |r|)
                    (COND
                      ((QSMINUSP |n|) (QSDIFFERENCE |x| |n|))
                      ('T (QSPLUS |r| |n|))))
                   ('T |r|))))))) 

(DEFUN |SINT;coerce;I$;51| (|x| $)
  (SEQ (COND
         ((NULL (< |$ShortMaximum| |x|))
          (COND ((NULL (< |x| |$ShortMinimum|)) (EXIT |x|)))))
       (EXIT (|error| "integer too large to represent in a machine word")))) 

(DEFUN |SINT;random;$;52| ($)
  (SEQ (|setShellEntry| $ 6
           (REMAINDER (TIMES 314159269 (|getShellEntry| $ 6))
               2147483647))
       (EXIT (REMAINDER (|getShellEntry| $ 6) 67108864)))) 

(DEFUN |SINT;random;2$;53| (|n| $) (DECLARE (IGNORE $)) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;54| (|x| $)
  (COND
    ((QSLESSP |x| 0) (VECTOR -1 (QSMINUS |x|) -1))
    ('T (VECTOR 1 |x| 1)))) 

(DEFUN |SingleInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1491)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|SingleInteger|)
                   |SingleInteger|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|SingleInteger|
                                   (LIST
                                    (CONS NIL
                                     (CONS 1 (|SingleInteger;|))))))
                 (LETT #0# T |SingleInteger|))
               (COND
                 ((NOT #0#)
                  (HREM |$ConstructorCache| '|SingleInteger|))))))))))) 

(DEFUN |SingleInteger;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|SingleInteger|) . #0=(|SingleInteger|))
        (LETT $ (|newShell| 105) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|SingleInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 1)
        $)))) 

(MAKEPROP '|SingleInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL '|seed| (|Void|)
             (|OpenMathDevice|) (0 . |OMputApp|) (|String|)
             (5 . |OMputSymbol|) (|Integer|) (12 . |OMputInteger|)
             (18 . |OMputEndApp|) (|OpenMathEncoding|)
             (23 . |OMencodingXML|) (27 . |OMopenString|)
             (33 . |OMputObject|) (38 . |OMputEndObject|)
             (43 . |OMclose|) |SINT;OMwrite;$S;2| (|Boolean|)
             |SINT;OMwrite;$BS;3| |SINT;OMwrite;Omd$V;4|
             |SINT;OMwrite;Omd$BV;5| (|Matrix| 12) (|Matrix| $)
             |SINT;reducedSystem;MM;6| (|OutputForm|) (48 . |coerce|)
             |SINT;coerce;$Of;7| |SINT;convert;$I;8| (53 . |coerce|)
             |SINT;*;I2$;9|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;Zero;$;10|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;One;$;11|) $))
             |SINT;base;$;12| |SINT;max;$;13| |SINT;min;$;14|
             |SINT;=;2$B;15| |SINT;~;2$;16| |SINT;not;2$;17|
             |SINT;/\\;3$;18| |SINT;\\/;3$;19| |SINT;Not;2$;20|
             |SINT;And;3$;21| |SINT;Or;3$;22| |SINT;xor;3$;23|
             |SINT;<;2$B;24| |SINT;inc;2$;25| |SINT;dec;2$;26|
             |SINT;-;2$;27| |SINT;+;3$;28| |SINT;-;3$;29|
             |SINT;*;3$;30| (|NonNegativeInteger|) |SINT;**;$Nni$;31|
             |SINT;quo;3$;32| |SINT;rem;3$;33|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;34| |SINT;gcd;3$;35| |SINT;abs;2$;36|
             |SINT;odd?;$B;37| |SINT;zero?;$B;38| |SINT;one?;$B;39|
             |SINT;max;3$;40| |SINT;min;3$;41| (|SingleInteger|)
             |SINT;hash;2$;42| |SINT;length;2$;43| |SINT;shift;3$;44|
             |SINT;mulmod;4$;45| |SINT;addmod;4$;46|
             |SINT;submod;4$;47| |SINT;negative?;$B;48| (|Vector| 12)
             (|Record| (|:| |mat| 26) (|:| |vec| 77)) (|Vector| $)
             |SINT;reducedSystem;MVR;49| |SINT;positiveRemainder;3$;50|
             |SINT;coerce;I$;51| |SINT;random;$;52| |SINT;random;2$;53|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;54| (|Fraction| 12)
             (|Union| 87 '"failed") (|Union| $ '"failed") (|Float|)
             (|DoubleFloat|) (|Pattern| 12) (|PatternMatchResult| 12 $)
             (|InputForm|) (|Union| 12 '"failed") (|List| $)
             (|Record| (|:| |coef| 96) (|:| |generator| $))
             (|Union| 96 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 100 '"failed") (|Factored| $)
             (|SparseUnivariatePolynomial| $) (|PositiveInteger|))
          '#(~= 58 ~ 64 |zero?| 69 |xor| 74 |unitNormal| 80
             |unitCanonical| 85 |unit?| 90 |symmetricRemainder| 95
             |subtractIfCan| 101 |submod| 107 |squareFreePart| 114
             |squareFree| 119 |sizeLess?| 124 |sign| 130 |shift| 135
             |sample| 141 |retractIfCan| 145 |retract| 150 |rem| 155
             |reducedSystem| 161 |recip| 172 |rationalIfCan| 177
             |rational?| 182 |rational| 187 |random| 192 |quo| 201
             |principalIdeal| 207 |prime?| 212 |powmod| 217
             |positiveRemainder| 224 |positive?| 230 |permutation| 235
             |patternMatch| 241 |one?| 248 |odd?| 253 |not| 258
             |nextItem| 263 |negative?| 268 |multiEuclidean| 273
             |mulmod| 279 |min| 286 |max| 296 |mask| 306 |length| 311
             |lcm| 316 |latex| 327 |invmod| 332 |init| 338 |inc| 342
             |hash| 347 |gcdPolynomial| 352 |gcd| 358 |factorial| 369
             |factor| 374 |extendedEuclidean| 379 |exquo| 392
             |expressIdealMember| 398 |even?| 404 |euclideanSize| 409
             |divide| 414 |differentiate| 420 |dec| 431 |copy| 436
             |convert| 441 |coerce| 466 |characteristic| 486 |bit?| 490
             |binomial| 496 |base| 502 |associates?| 506 |addmod| 512
             |abs| 519 |\\/| 524 |Zero| 530 |Or| 534 |One| 540
             |OMwrite| 544 |Not| 568 D 573 |And| 584 >= 590 > 596 = 602
             <= 608 < 614 |/\\| 620 - 626 + 637 ** 643 * 655)
          '((|noetherian| . 0) (|canonicalsClosed| . 0)
            (|canonical| . 0) (|canonicalUnitNormal| . 0)
            (|multiplicativeValuation| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |DifferentialRing&| |OrderedRing&| NIL NIL
                         |Module&| NIL |Ring&| NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL |AbelianMonoid&|
                         |Monoid&| NIL NIL |OrderedSet&|
                         |AbelianSemiGroup&| |SemiGroup&| |Logic&| NIL
                         |RetractableTo&| |SetCategory&| NIL NIL NIL
                         NIL NIL NIL NIL NIL NIL |BasicType&| NIL)
                      (CONS '#((|IntegerNumberSystem|)
                               (|EuclideanDomain|)
                               (|UniqueFactorizationDomain|)
                               (|PrincipalIdealDomain|)
                               (|OrderedIntegralDomain|) (|GcdDomain|)
                               (|IntegralDomain|) (|Algebra| $$)
                               (|CharacteristicZero|)
                               (|LinearlyExplicitRingOver| 12)
                               (|DifferentialRing|) (|OrderedRing|)
                               (|CommutativeRing|) (|EntireRing|)
                               (|Module| $$) (|BiModule| $$ $$)
                               (|Ring|) (|OrderedAbelianGroup|)
                               (|LeftModule| $$) (|Rng|)
                               (|RightModule| $$)
                               (|OrderedCancellationAbelianMonoid|)
                               (|AbelianGroup|)
                               (|OrderedAbelianMonoid|)
                               (|CancellationAbelianMonoid|)
                               (|OrderedAbelianSemiGroup|)
                               (|AbelianMonoid|) (|Monoid|)
                               (|StepThrough|) (|PatternMatchable| 12)
                               (|OrderedSet|) (|AbelianSemiGroup|)
                               (|SemiGroup|) (|Logic|) (|RealConstant|)
                               (|RetractableTo| 12) (|SetCategory|)
                               (|OpenMath|) (|ConvertibleTo| 90)
                               (|ConvertibleTo| 91)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 92)
                               (|ConvertibleTo| 94)
                               (|ConvertibleTo| 12)
                               (|CoercibleFrom| $$)
                               (|CoercibleFrom| 12) (|BasicType|)
                               (|CoercibleTo| 29))
                            (|makeByteWordVec2| 104
                                '(1 8 7 0 9 3 8 7 0 10 10 11 2 8 7 0 12
                                  13 1 8 7 0 14 0 15 0 16 2 8 0 10 15
                                  17 1 8 7 0 18 1 8 7 0 19 1 8 7 0 20 1
                                  12 29 0 30 1 0 0 12 33 2 0 22 0 0 1 1
                                  0 0 0 41 1 0 22 0 65 2 0 0 0 0 48 1 0
                                  85 0 86 1 0 0 0 1 1 0 22 0 1 2 0 0 0
                                  0 1 2 0 89 0 0 1 3 0 0 0 0 0 75 1 0 0
                                  0 1 1 0 102 0 1 2 0 22 0 0 1 1 0 12 0
                                  1 2 0 0 0 0 72 0 0 0 1 1 0 95 0 1 1 0
                                  12 0 1 2 0 0 0 0 59 1 0 26 27 28 2 0
                                  78 27 79 80 1 0 89 0 1 1 0 88 0 1 1 0
                                  22 0 1 1 0 87 0 1 1 0 0 0 84 0 0 0 83
                                  2 0 0 0 0 58 1 0 97 96 1 1 0 22 0 1 3
                                  0 0 0 0 0 1 2 0 0 0 0 81 1 0 22 0 1 2
                                  0 0 0 0 1 3 0 93 0 92 93 1 1 0 22 0
                                  66 1 0 22 0 64 1 0 0 0 42 1 0 89 0 1
                                  1 0 22 0 76 2 0 98 96 0 1 3 0 0 0 0 0
                                  73 0 0 0 39 2 0 0 0 0 68 0 0 0 38 2 0
                                  0 0 0 67 1 0 0 0 1 1 0 0 0 71 2 0 0 0
                                  0 1 1 0 0 96 1 1 0 10 0 1 2 0 0 0 0 1
                                  0 0 0 1 1 0 0 0 50 1 0 69 0 70 2 0
                                  103 103 103 1 2 0 0 0 0 62 1 0 0 96 1
                                  1 0 0 0 1 1 0 102 0 1 2 0 99 0 0 1 3
                                  0 101 0 0 0 1 2 0 89 0 0 1 2 0 98 96
                                  0 1 1 0 22 0 1 1 0 56 0 1 2 0 60 0 0
                                  61 1 0 0 0 1 2 0 0 0 56 1 1 0 0 0 51
                                  1 0 0 0 1 1 0 90 0 1 1 0 91 0 1 1 0
                                  92 0 1 1 0 94 0 1 1 0 12 0 32 1 0 0
                                  12 82 1 0 0 0 1 1 0 0 12 82 1 0 29 0
                                  31 0 0 56 1 2 0 22 0 0 1 2 0 0 0 0 1
                                  0 0 0 37 2 0 22 0 0 1 3 0 0 0 0 0 74
                                  1 0 0 0 63 2 0 0 0 0 44 0 0 0 35 2 0
                                  0 0 0 47 0 0 0 36 3 0 7 8 0 22 25 2 0
                                  10 0 22 23 2 0 7 8 0 24 1 0 10 0 21 1
                                  0 0 0 45 1 0 0 0 1 2 0 0 0 56 1 2 0 0
                                  0 0 46 2 0 22 0 0 1 2 0 22 0 0 1 2 0
                                  22 0 0 40 2 0 22 0 0 1 2 0 22 0 0 49
                                  2 0 0 0 0 43 1 0 0 0 52 2 0 0 0 0 54
                                  2 0 0 0 0 53 2 0 0 0 56 57 2 0 0 0
                                  104 1 2 0 0 0 0 55 2 0 0 12 0 34 2 0
                                  0 56 0 1 2 0 0 104 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
