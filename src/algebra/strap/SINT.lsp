
(/VERSIONCHECK 2) 

(|noteSubDomainInfo| '|SingleInteger| '(|Integer|) '(SMINTP |#1|)) 

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

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |SINT;size;Nni;49|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Short|)
                |SINT;index;Pi$;50|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) (|%IntegerSection| 1))
                |SINT;lookup;$Pi;51|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |SINT;reducedSystem;MVR;52|)) 

(PUT '|SINT;reducedSystem;MVR;52| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;positiveRemainder;3$;53|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |SINT;coerce;I$;54|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;random;$;55|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;random;2$;56|)) 

(PUT '|SINT;random;2$;56| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Shell|)
                |SINT;unitNormal;$R;57|)) 

(DEFUN |SINT;writeOMSingleInt| (|dev| |x| $)
  (SEQ (COND
         ((QSLESSP |x| 0)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 13))
               (SPADCALL |dev| "arith1" "unaryminus"
                   (|getShellEntry| $ 15))
               (SPADCALL |dev| (QSMINUS |x|) (|getShellEntry| $ 18))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 19)))))
         ('T (SPADCALL |dev| |x| (|getShellEntry| $ 18)))))) 

(DEFUN |SINT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                     (|getShellEntry| $ 22))
                 |SINT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 23))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 24))
           (SPADCALL |dev| (|getShellEntry| $ 25))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                     (|getShellEntry| $ 22))
                 |SINT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))
           (SPADCALL |dev| (|getShellEntry| $ 25))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 23))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 24))))) 

(DEFUN |SINT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))))) 

(DEFUN |SINT;reducedSystem;MM;6| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |SINT;coerce;$Of;7| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 34))) 

(DEFUN |SINT;convert;$I;8| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |SINT;*;I2$;9| (|i| |y| $)
  (QSTIMES (SPADCALL |i| (|getShellEntry| $ 36)) |y|)) 

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
  (SPADCALL (EXPT |x| |n|) (|getShellEntry| $ 36))) 

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

(DEFUN |SINT;size;Nni;49| ($)
  (+ (- |$ShortMaximum| |$ShortMinimum|) 1)) 

(DEFUN |SINT;index;Pi$;50| (|i| $)
  (PROG (#0=#:G1456)
    (RETURN
      (PROG1 (LETT #0# (- (+ |i| |$ShortMinimum|) 1)
                   |SINT;index;Pi$;50|)
        (|check-subtype| (SMINTP #0#) '(|SingleInteger|) #0#))))) 

(DEFUN |SINT;lookup;$Pi;51| (|x| $) (+ (- |x| |$ShortMinimum|) 1)) 

(DEFUN |SINT;reducedSystem;MVR;52| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;53| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (QSREMAINDER |x| |n|)
                 |SINT;positiveRemainder;3$;53|)
           (EXIT (COND
                   ((QSMINUSP |r|)
                    (COND
                      ((QSMINUSP |n|) (QSDIFFERENCE |x| |n|))
                      ('T (QSPLUS |r| |n|))))
                   ('T |r|))))))) 

(DEFUN |SINT;coerce;I$;54| (|x| $)
  (PROG1 |x| (|check-subtype| (SMINTP |x|) '(|SingleInteger|) |x|))) 

(DEFUN |SINT;random;$;55| ($)
  (SEQ (|setShellEntry| $ 6
           (REMAINDER (TIMES 314159269 (|getShellEntry| $ 6))
               2147483647))
       (EXIT (REMAINDER (|getShellEntry| $ 6) 67108864)))) 

(DEFUN |SINT;random;2$;56| (|n| $) (DECLARE (IGNORE $)) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;57| (|x| $)
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
        (LETT $ (|newShell| 111) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|SingleInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 1)
        $)))) 

(MAKEPROP '|SingleInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) '|seed|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;Zero;$;10|) $))
             (0 . |Zero|) (|Boolean|) |SINT;<;2$B;24| (|Void|)
             (|OpenMathDevice|) (4 . |OMputApp|) (|String|)
             (9 . |OMputSymbol|) |SINT;-;2$;27| |SINT;convert;$I;8|
             (16 . |OMputInteger|) (22 . |OMputEndApp|)
             (|OpenMathEncoding|) (27 . |OMencodingXML|)
             (31 . |OMopenString|) (37 . |OMputObject|)
             (42 . |OMputEndObject|) (47 . |OMclose|)
             |SINT;OMwrite;$S;2| |SINT;OMwrite;$BS;3|
             |SINT;OMwrite;Omd$V;4| |SINT;OMwrite;Omd$BV;5|
             (|Matrix| 5) (|Matrix| $) |SINT;reducedSystem;MM;6|
             (|OutputForm|) (52 . |coerce|) |SINT;coerce;$Of;7|
             (57 . |coerce|) |SINT;*;3$;30| |SINT;*;I2$;9|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;One;$;11|) $))
             |SINT;base;$;12|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;max;$;13|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;min;$;14|) $))
             |SINT;=;2$B;15| |SINT;~;2$;16| |SINT;not;2$;17|
             |SINT;/\\;3$;18| |SINT;\\/;3$;19| |SINT;Not;2$;20|
             |SINT;And;3$;21| |SINT;Or;3$;22| |SINT;xor;3$;23|
             |SINT;inc;2$;25| |SINT;dec;2$;26| |SINT;+;3$;28|
             |SINT;-;3$;29| (|NonNegativeInteger|) |SINT;**;$Nni$;31|
             |SINT;quo;3$;32| |SINT;rem;3$;33|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;34| |SINT;gcd;3$;35| |SINT;abs;2$;36|
             |SINT;odd?;$B;37| |SINT;zero?;$B;38| (62 . |One|)
             |SINT;one?;$B;39| |SINT;max;3$;40| |SINT;min;3$;41|
             (|SingleInteger|) |SINT;hash;2$;42| |SINT;length;2$;43|
             |SINT;shift;3$;44| |SINT;mulmod;4$;45| |SINT;addmod;4$;46|
             |SINT;submod;4$;47| |SINT;negative?;$B;48|
             |SINT;size;Nni;49| (|PositiveInteger|) (66 . +) (72 . -)
             |SINT;index;Pi$;50| |SINT;lookup;$Pi;51| (|Vector| 5)
             (|Record| (|:| |mat| 30) (|:| |vec| 84)) (|Vector| $)
             |SINT;reducedSystem;MVR;52| |SINT;positiveRemainder;3$;53|
             |SINT;coerce;I$;54| |SINT;random;$;55| |SINT;random;2$;56|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;57| (|Fraction| 5)
             (|Union| 94 '"failed") (|DoubleFloat|)
             (|Union| $ '"failed") (|Float|) (|PatternMatchResult| 5 $)
             (|Pattern| 5) (|InputForm|) (|Union| 5 '"failed")
             (|List| $) (|Union| 103 '"failed")
             (|Record| (|:| |coef| 103) (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 106 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Factored| $) (|SparseUnivariatePolynomial| $))
          '#(~= 78 ~ 84 |zero?| 89 |xor| 94 |unitNormal| 100
             |unitCanonical| 105 |unit?| 110 |symmetricRemainder| 115
             |subtractIfCan| 121 |submod| 127 |squareFreePart| 134
             |squareFree| 139 |sizeLess?| 144 |size| 150 |sign| 154
             |shift| 159 |sample| 165 |retractIfCan| 169 |retract| 174
             |rem| 179 |reducedSystem| 185 |recip| 196 |rationalIfCan|
             201 |rational?| 206 |rational| 211 |random| 216 |quo| 225
             |principalIdeal| 231 |prime?| 236 |powmod| 241
             |positiveRemainder| 248 |positive?| 254 |permutation| 259
             |patternMatch| 265 |one?| 272 |odd?| 277 |not| 282
             |nextItem| 287 |negative?| 292 |multiEuclidean| 297
             |mulmod| 303 |min| 310 |max| 320 |mask| 330 |lookup| 335
             |length| 340 |lcm| 345 |latex| 356 |invmod| 361 |init| 367
             |index| 371 |inc| 376 |hash| 381 |gcdPolynomial| 386 |gcd|
             392 |factorial| 403 |factor| 408 |extendedEuclidean| 413
             |exquo| 426 |expressIdealMember| 432 |even?| 438
             |euclideanSize| 443 |divide| 448 |differentiate| 454 |dec|
             465 |copy| 470 |convert| 475 |coerce| 500 |characteristic|
             520 |bit?| 524 |binomial| 530 |before?| 536 |base| 542
             |associates?| 546 |addmod| 552 |abs| 559 |\\/| 564 |Zero|
             570 |Or| 574 |One| 580 |OMwrite| 584 |Not| 608 D 613 |And|
             624 >= 630 > 636 = 642 <= 648 < 654 |/\\| 660 - 666 + 677
             ** 683 * 695)
          '((|noetherian| . 0) (|canonicalsClosed| . 0)
            (|canonical| . 0) (|canonicalUnitNormal| . 0)
            (|multiplicativeValuation| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |DifferentialRing&| |OrderedRing&| NIL NIL
                         |Module&| NIL |Ring&| NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL NIL NIL
                         |AbelianMonoid&| |Monoid&| NIL NIL NIL
                         |OrderedSet&| NIL NIL |AbelianSemiGroup&|
                         |SemiGroup&| NIL |Logic&| NIL |RetractableTo&|
                         |SetCategory&| NIL NIL NIL NIL NIL NIL NIL NIL
                         NIL |BasicType&| NIL)
                      (CONS '#((|IntegerNumberSystem|)
                               (|EuclideanDomain|)
                               (|UniqueFactorizationDomain|)
                               (|PrincipalIdealDomain|)
                               (|OrderedIntegralDomain|) (|GcdDomain|)
                               (|IntegralDomain|) (|Algebra| $$)
                               (|CharacteristicZero|)
                               (|LinearlyExplicitRingOver| 5)
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
                               (|OrderedFinite|)
                               (|OrderedAbelianSemiGroup|)
                               (|LinearSet| $$) (|AbelianMonoid|)
                               (|Monoid|) (|Finite|) (|StepThrough|)
                               (|PatternMatchable| 5) (|OrderedSet|)
                               (|LeftLinearSet| $$)
                               (|RightLinearSet| $$)
                               (|AbelianSemiGroup|) (|SemiGroup|)
                               (|LeftLinearSet| 5) (|Logic|)
                               (|RealConstant|) (|RetractableTo| 5)
                               (|SetCategory|) (|OpenMath|)
                               (|ConvertibleTo| 98)
                               (|ConvertibleTo| 96)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 100)
                               (|ConvertibleTo| 101)
                               (|ConvertibleTo| 5) (|CoercibleFrom| $$)
                               (|CoercibleFrom| 5) (|BasicType|)
                               (|CoercibleTo| 33))
                            (|makeByteWordVec2| 110
                                '(0 5 0 8 1 12 11 0 13 3 12 11 0 14 14
                                  15 2 12 11 0 5 18 1 12 11 0 19 0 20 0
                                  21 2 12 0 14 20 22 1 12 11 0 23 1 12
                                  11 0 24 1 12 11 0 25 1 5 33 0 34 1 0
                                  0 5 36 0 5 0 66 2 79 0 0 0 80 2 5 0 0
                                  0 81 2 0 9 0 0 1 1 0 0 0 44 1 0 9 0
                                  65 2 0 0 0 0 51 1 0 92 0 93 1 0 0 0 1
                                  1 0 9 0 1 2 0 0 0 0 1 2 0 97 0 0 1 3
                                  0 0 0 0 0 76 1 0 0 0 1 1 0 109 0 1 2
                                  0 9 0 0 1 0 0 56 78 1 0 5 0 1 2 0 0 0
                                  0 73 0 0 0 1 1 0 102 0 1 1 0 5 0 1 2
                                  0 0 0 0 59 2 0 85 31 86 87 1 0 30 31
                                  32 1 0 97 0 1 1 0 95 0 1 1 0 9 0 1 1
                                  0 94 0 1 1 0 0 0 91 0 0 0 90 2 0 0 0
                                  0 58 1 0 105 103 1 1 0 9 0 1 3 0 0 0
                                  0 0 1 2 0 0 0 0 88 1 0 9 0 1 2 0 0 0
                                  0 1 3 0 99 0 100 99 1 1 0 9 0 67 1 0
                                  9 0 64 1 0 0 0 45 1 0 97 0 1 1 0 9 0
                                  77 2 0 104 103 0 1 3 0 0 0 0 0 74 0 0
                                  0 42 2 0 0 0 0 69 0 0 0 41 2 0 0 0 0
                                  68 1 0 0 0 1 1 0 79 0 83 1 0 0 0 72 2
                                  0 0 0 0 1 1 0 0 103 1 1 0 14 0 1 2 0
                                  0 0 0 1 0 0 0 1 1 0 0 79 82 1 0 0 0
                                  52 1 0 70 0 71 2 0 110 110 110 1 2 0
                                  0 0 0 62 1 0 0 103 1 1 0 0 0 1 1 0
                                  109 0 1 3 0 107 0 0 0 1 2 0 108 0 0 1
                                  2 0 97 0 0 1 2 0 104 103 0 1 1 0 9 0
                                  1 1 0 56 0 1 2 0 60 0 0 61 1 0 0 0 1
                                  2 0 0 0 56 1 1 0 0 0 53 1 0 0 0 1 1 0
                                  96 0 1 1 0 98 0 1 1 0 101 0 1 1 0 100
                                  0 1 1 0 5 0 17 1 0 0 5 89 1 0 0 0 1 1
                                  0 0 5 89 1 0 33 0 35 0 0 56 1 2 0 9 0
                                  0 1 2 0 0 0 0 1 2 0 9 0 0 1 0 0 0 40
                                  2 0 9 0 0 1 3 0 0 0 0 0 75 1 0 0 0 63
                                  2 0 0 0 0 47 0 0 0 7 2 0 0 0 0 50 0 0
                                  0 39 2 0 11 12 0 28 3 0 11 12 0 9 29
                                  2 0 14 0 9 27 1 0 14 0 26 1 0 0 0 48
                                  1 0 0 0 1 2 0 0 0 56 1 2 0 0 0 0 49 2
                                  0 9 0 0 1 2 0 9 0 0 1 2 0 9 0 0 43 2
                                  0 9 0 0 1 2 0 9 0 0 10 2 0 0 0 0 46 2
                                  0 0 0 0 55 1 0 0 0 16 2 0 0 0 0 54 2
                                  0 0 0 56 57 2 0 0 0 79 1 2 0 0 0 0 37
                                  2 0 0 5 0 38 2 0 0 56 0 1 2 0 0 79 0
                                  1)))))
          '|lookupComplete|)) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
