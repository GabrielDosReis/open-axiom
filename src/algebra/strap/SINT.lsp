
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

(PUT '|SINT;=;2$B;15| '|SPADreplace| '|%ieq|) 

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
                |SINT;and;3$;22|)) 

(PUT '|SINT;and;3$;22| '|SPADreplace| 'LOGAND) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;Or;3$;23|)) 

(PUT '|SINT;Or;3$;23| '|SPADreplace| 'LOGIOR) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;or;3$;24|)) 

(PUT '|SINT;or;3$;24| '|SPADreplace| 'LOGIOR) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;xor;3$;25|)) 

(PUT '|SINT;xor;3$;25| '|SPADreplace| 'LOGXOR) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;<;2$B;26|)) 

(PUT '|SINT;<;2$B;26| '|SPADreplace| 'QSLESSP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;>;2$B;27|)) 

(PUT '|SINT;>;2$B;27| '|SPADreplace| 'QSGREATERP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;<=;2$B;28|)) 

(PUT '|SINT;<=;2$B;28| '|SPADreplace| '|%ile|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;>=;2$B;29|)) 

(PUT '|SINT;>=;2$B;29| '|SPADreplace| '|%ige|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;inc;2$;30|)) 

(PUT '|SINT;inc;2$;30| '|SPADreplace| 'QSADD1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;dec;2$;31|)) 

(PUT '|SINT;dec;2$;31| '|SPADreplace| 'QSSUB1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|) |SINT;-;2$;32|)) 

(PUT '|SINT;-;2$;32| '|SPADreplace| 'QSMINUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;+;3$;33|)) 

(PUT '|SINT;+;3$;33| '|SPADreplace| 'QSPLUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;-;3$;34|)) 

(PUT '|SINT;-;3$;34| '|SPADreplace| 'QSDIFFERENCE) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;*;3$;35|)) 

(PUT '|SINT;*;3$;35| '|SPADreplace| 'QSTIMES) 

(DECLAIM (FTYPE (FUNCTION (|%Short| (|%IntegerSection| 0) |%Shell|)
                    |%Short|)
                |SINT;**;$Nni$;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;quo;3$;37|)) 

(PUT '|SINT;quo;3$;37| '|SPADreplace| 'QSQUOTIENT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;rem;3$;38|)) 

(PUT '|SINT;rem;3$;38| '|SPADreplace| 'QSREMAINDER) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Pair|)
                |SINT;divide;2$R;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;gcd;3$;40|)) 

(PUT '|SINT;gcd;3$;40| '|SPADreplace| '|%igcd|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;abs;2$;41|)) 

(PUT '|SINT;abs;2$;41| '|SPADreplace| 'QSABSVAL) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;odd?;$B;42|)) 

(PUT '|SINT;odd?;$B;42| '|SPADreplace| 'QSODDP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;zero?;$B;43|)) 

(PUT '|SINT;zero?;$B;43| '|SPADreplace| 'QSZEROP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;one?;$B;44|)) 

(PUT '|SINT;one?;$B;44| '|SPADreplace| '(XLAM (|x|) (|%ieq| |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;max;3$;45|)) 

(PUT '|SINT;max;3$;45| '|SPADreplace| 'QSMAX) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;min;3$;46|)) 

(PUT '|SINT;min;3$;46| '|SPADreplace| 'QSMIN) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;hash;2$;47|)) 

(PUT '|SINT;hash;2$;47| '|SPADreplace| 'HASHEQ) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;length;2$;48|)) 

(PUT '|SINT;length;2$;48| '|SPADreplace| 'INTEGER-LENGTH) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;shift;3$;49|)) 

(PUT '|SINT;shift;3$;49| '|SPADreplace| 'QSLEFTSHIFT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;mulmod;4$;50|)) 

(PUT '|SINT;mulmod;4$;50| '|SPADreplace| 'QSMULTMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;addmod;4$;51|)) 

(PUT '|SINT;addmod;4$;51| '|SPADreplace| 'QSADDMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;submod;4$;52|)) 

(PUT '|SINT;submod;4$;52| '|SPADreplace| 'QSDIFMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;negative?;$B;53|)) 

(PUT '|SINT;negative?;$B;53| '|SPADreplace| 'QSMINUSP) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |SINT;size;Nni;54|)) 

(PUT '|SINT;size;Nni;54| '|SPADreplace|
     '(XLAM NIL (+ (- |$ShortMaximum| |$ShortMinimum|) 1))) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Short|)
                |SINT;index;Pi$;55|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) (|%IntegerSection| 1))
                |SINT;lookup;$Pi;56|)) 

(PUT '|SINT;lookup;$Pi;56| '|SPADreplace|
     '(XLAM (|x|) (+ (- |x| |$ShortMinimum|) 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |SINT;reducedSystem;MVR;57|)) 

(PUT '|SINT;reducedSystem;MVR;57| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;positiveRemainder;3$;58|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |SINT;coerce;I$;59|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;random;$;60|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;random;2$;61|)) 

(PUT '|SINT;random;2$;61| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Shell|)
                |SINT;unitNormal;$R;62|)) 

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
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                    (|getShellEntry| $ 22))))
    (SEQ (SPADCALL |dev| (|getShellEntry| $ 23))
         (|SINT;writeOMSingleInt| |dev| |x| $)
         (SPADCALL |dev| (|getShellEntry| $ 24))
         (SPADCALL |dev| (|getShellEntry| $ 25))
         (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|)))) 

(DEFUN |SINT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                    (|getShellEntry| $ 22))))
    (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
         (|SINT;writeOMSingleInt| |dev| |x| $)
         (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))
         (SPADCALL |dev| (|getShellEntry| $ 25))
         (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|)))) 

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

(DEFUN |SINT;and;3$;22| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGAND |x| |y|)) 

(DEFUN |SINT;Or;3$;23| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGIOR |x| |y|)) 

(DEFUN |SINT;or;3$;24| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGIOR |x| |y|)) 

(DEFUN |SINT;xor;3$;25| (|x| |y| $)
  (DECLARE (IGNORE $))
  (LOGXOR |x| |y|)) 

(DEFUN |SINT;<;2$B;26| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSLESSP |x| |y|)) 

(DEFUN |SINT;>;2$B;27| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSGREATERP |x| |y|)) 

(DEFUN |SINT;<=;2$B;28| (|x| |y| $) (DECLARE (IGNORE $)) (<= |x| |y|)) 

(DEFUN |SINT;>=;2$B;29| (|x| |y| $) (DECLARE (IGNORE $)) (>= |x| |y|)) 

(DEFUN |SINT;inc;2$;30| (|x| $) (DECLARE (IGNORE $)) (QSADD1 |x|)) 

(DEFUN |SINT;dec;2$;31| (|x| $) (DECLARE (IGNORE $)) (QSSUB1 |x|)) 

(DEFUN |SINT;-;2$;32| (|x| $) (DECLARE (IGNORE $)) (QSMINUS |x|)) 

(DEFUN |SINT;+;3$;33| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSPLUS |x| |y|)) 

(DEFUN |SINT;-;3$;34| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSDIFFERENCE |x| |y|)) 

(DEFUN |SINT;*;3$;35| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSTIMES |x| |y|)) 

(DEFUN |SINT;**;$Nni$;36| (|x| |n| $)
  (SPADCALL (EXPT |x| |n|) (|getShellEntry| $ 36))) 

(DEFUN |SINT;quo;3$;37| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSQUOTIENT |x| |y|)) 

(DEFUN |SINT;rem;3$;38| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSREMAINDER |x| |y|)) 

(DEFUN |SINT;divide;2$R;39| (|x| |y| $)
  (CONS (QSQUOTIENT |x| |y|) (QSREMAINDER |x| |y|))) 

(DEFUN |SINT;gcd;3$;40| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GCD |x| |y|)) 

(DEFUN |SINT;abs;2$;41| (|x| $) (DECLARE (IGNORE $)) (QSABSVAL |x|)) 

(DEFUN |SINT;odd?;$B;42| (|x| $) (DECLARE (IGNORE $)) (QSODDP |x|)) 

(DEFUN |SINT;zero?;$B;43| (|x| $) (DECLARE (IGNORE $)) (QSZEROP |x|)) 

(DEFUN |SINT;one?;$B;44| (|x| $) (DECLARE (IGNORE $)) (EQL |x| 1)) 

(DEFUN |SINT;max;3$;45| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMAX |x| |y|)) 

(DEFUN |SINT;min;3$;46| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMIN |x| |y|)) 

(DEFUN |SINT;hash;2$;47| (|x| $) (DECLARE (IGNORE $)) (HASHEQ |x|)) 

(DEFUN |SINT;length;2$;48| (|x| $)
  (DECLARE (IGNORE $))
  (INTEGER-LENGTH |x|)) 

(DEFUN |SINT;shift;3$;49| (|x| |n| $)
  (DECLARE (IGNORE $))
  (QSLEFTSHIFT |x| |n|)) 

(DEFUN |SINT;mulmod;4$;50| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSMULTMOD |a| |b| |p|)) 

(DEFUN |SINT;addmod;4$;51| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSADDMOD |a| |b| |p|)) 

(DEFUN |SINT;submod;4$;52| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSDIFMOD |a| |b| |p|)) 

(DEFUN |SINT;negative?;$B;53| (|x| $)
  (DECLARE (IGNORE $))
  (QSMINUSP |x|)) 

(DEFUN |SINT;size;Nni;54| ($)
  (DECLARE (IGNORE $))
  (+ (- |$ShortMaximum| |$ShortMinimum|) 1)) 

(DEFUN |SINT;index;Pi$;55| (|i| $)
  (LET ((#0=#:G1461 (- (+ |i| |$ShortMinimum|) 1)))
    (|check-subtype| (SMINTP #0#) '(|SingleInteger|) #0#))) 

(DEFUN |SINT;lookup;$Pi;56| (|x| $)
  (DECLARE (IGNORE $))
  (+ (- |x| |$ShortMinimum|) 1)) 

(DEFUN |SINT;reducedSystem;MVR;57| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;58| (|x| |n| $)
  (LET ((|r| (QSREMAINDER |x| |n|)))
    (COND
      ((QSMINUSP |r|)
       (COND
         ((QSMINUSP |n|) (QSDIFFERENCE |x| |n|))
         ('T (QSPLUS |r| |n|))))
      ('T |r|)))) 

(DEFUN |SINT;coerce;I$;59| (|x| $)
  (|check-subtype| (SMINTP |x|) '(|SingleInteger|) |x|)) 

(DEFUN |SINT;random;$;60| ($)
  (SEQ (|setShellEntry| $ 6
           (REMAINDER (TIMES 314159269 (|getShellEntry| $ 6))
               2147483647))
       (EXIT (REMAINDER (|getShellEntry| $ 6) 67108864)))) 

(DEFUN |SINT;random;2$;61| (|n| $) (DECLARE (IGNORE $)) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;62| (|x| $)
  (COND
    ((QSLESSP |x| 0) (VECTOR -1 (QSMINUS |x|) -1))
    ('T (VECTOR 1 |x| 1)))) 

(DEFUN |SingleInteger| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#0=#:G1495)
    (RETURN
      (COND
        ((SETQ #0# (HGET |$ConstructorCache| '|SingleInteger|))
         (|CDRwithIncrement| (CDAR #0#)))
        ('T
         (UNWIND-PROTECT
           (PROG1 (CDDAR (HPUT |$ConstructorCache| '|SingleInteger|
                               (LIST (CONS NIL
                                      (CONS 1 (|SingleInteger;|))))))
             (SETQ #0# T))
           (COND
             ((NOT #0#) (HREM |$ConstructorCache| '|SingleInteger|))))))))) 

(DEFUN |SingleInteger;| ()
  (LET ((|dv$| (LIST '|SingleInteger|)) ($ (|newShell| 116))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (DECLARE (SPECIAL |$ConstructorCache|))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|SingleInteger| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 1)
    $)) 

(MAKEPROP '|SingleInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) '|seed|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;Zero;$;10|) $))
             (0 . |Zero|) (|Boolean|) |SINT;<;2$B;26| (|Void|)
             (|OpenMathDevice|) (4 . |OMputApp|) (|String|)
             (9 . |OMputSymbol|) |SINT;-;2$;32| |SINT;convert;$I;8|
             (16 . |OMputInteger|) (22 . |OMputEndApp|)
             (|OpenMathEncoding|) (27 . |OMencodingXML|)
             (31 . |OMopenString|) (37 . |OMputObject|)
             (42 . |OMputEndObject|) (47 . |OMclose|)
             |SINT;OMwrite;$S;2| |SINT;OMwrite;$BS;3|
             |SINT;OMwrite;Omd$V;4| |SINT;OMwrite;Omd$BV;5|
             (|Matrix| 5) (|Matrix| $) |SINT;reducedSystem;MM;6|
             (|OutputForm|) (52 . |coerce|) |SINT;coerce;$Of;7|
             (57 . |coerce|) |SINT;*;3$;35| |SINT;*;I2$;9|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;One;$;11|) $))
             |SINT;base;$;12|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;max;$;13|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;min;$;14|) $))
             |SINT;=;2$B;15| |SINT;~;2$;16| |SINT;not;2$;17|
             |SINT;/\\;3$;18| |SINT;\\/;3$;19| |SINT;Not;2$;20|
             |SINT;And;3$;21| |SINT;and;3$;22| |SINT;Or;3$;23|
             |SINT;or;3$;24| |SINT;xor;3$;25| |SINT;>;2$B;27|
             |SINT;<=;2$B;28| |SINT;>=;2$B;29| |SINT;inc;2$;30|
             |SINT;dec;2$;31| |SINT;+;3$;33| |SINT;-;3$;34|
             (|NonNegativeInteger|) |SINT;**;$Nni$;36| |SINT;quo;3$;37|
             |SINT;rem;3$;38|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;39| |SINT;gcd;3$;40| |SINT;abs;2$;41|
             |SINT;odd?;$B;42| |SINT;zero?;$B;43| (62 . |One|)
             |SINT;one?;$B;44| |SINT;max;3$;45| |SINT;min;3$;46|
             (|SingleInteger|) |SINT;hash;2$;47| |SINT;length;2$;48|
             |SINT;shift;3$;49| |SINT;mulmod;4$;50| |SINT;addmod;4$;51|
             |SINT;submod;4$;52| |SINT;negative?;$B;53|
             |SINT;size;Nni;54| (|PositiveInteger|) (66 . +) (72 . -)
             |SINT;index;Pi$;55| |SINT;lookup;$Pi;56| (|Vector| 5)
             (|Record| (|:| |mat| 30) (|:| |vec| 89)) (|Vector| $)
             |SINT;reducedSystem;MVR;57| |SINT;positiveRemainder;3$;58|
             |SINT;coerce;I$;59| |SINT;random;$;60| |SINT;random;2$;61|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;62| (|Fraction| 5)
             (|Union| 99 '"failed") (|Union| $ '"failed") (|Float|)
             (|DoubleFloat|) (|PatternMatchResult| 5 $) (|Pattern| 5)
             (|InputForm|) (|Union| 5 '"failed") (|List| $)
             (|Union| 108 '"failed")
             (|Record| (|:| |coef| 108) (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 112 '"failed") (|Factored| $)
             (|SparseUnivariatePolynomial| $))
          '#(~= 78 ~ 84 |zero?| 89 |xor| 94 |unitNormal| 100
             |unitCanonical| 105 |unit?| 110 |symmetricRemainder| 115
             |subtractIfCan| 121 |submod| 127 |squareFreePart| 134
             |squareFree| 139 |sizeLess?| 144 |size| 150 |sign| 154
             |shift| 159 |sample| 165 |retractIfCan| 169 |retract| 174
             |rem| 179 |reducedSystem| 185 |recip| 207 |rationalIfCan|
             212 |rational?| 217 |rational| 222 |random| 227 |quo| 236
             |principalIdeal| 242 |prime?| 247 |powmod| 252
             |positiveRemainder| 259 |positive?| 265 |permutation| 270
             |patternMatch| 276 |or| 283 |one?| 289 |odd?| 294 |not|
             299 |nextItem| 304 |negative?| 309 |multiEuclidean| 314
             |mulmod| 320 |min| 327 |max| 337 |mask| 347 |lookup| 352
             |length| 357 |lcm| 362 |latex| 373 |invmod| 378 |init| 384
             |index| 388 |inc| 393 |hash| 398 |gcdPolynomial| 403 |gcd|
             409 |factorial| 420 |factor| 425 |extendedEuclidean| 430
             |exquo| 443 |expressIdealMember| 449 |even?| 455
             |euclideanSize| 460 |divide| 465 |differentiate| 471 |dec|
             482 |copy| 487 |convert| 492 |coerce| 517 |characteristic|
             537 |bit?| 541 |binomial| 547 |before?| 553 |base| 559
             |associates?| 563 |and| 569 |addmod| 575 |abs| 582 |\\/|
             587 |Zero| 593 |Or| 597 |One| 603 |OMwrite| 607 |Not| 631
             D 636 |And| 647 >= 653 > 659 = 665 <= 671 < 677 |/\\| 683
             - 689 + 700 ** 706 * 718)
          '((|noetherian| . 0) (|canonicalsClosed| . 0)
            (|canonical| . 0) (|canonicalUnitNormal| . 0)
            (|multiplicativeValuation| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |OrderedRing&| NIL NIL |Module&| NIL NIL
                         |Ring&| NIL NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL NIL NIL
                         |AbelianMonoid&| |Monoid&| NIL NIL NIL
                         |DifferentialSpace&| |OrderedSet&| NIL NIL
                         |AbelianSemiGroup&| |SemiGroup&| NIL |Logic&|
                         NIL NIL |RetractableTo&| |DifferentialDomain&|
                         |SetCategory&| NIL NIL NIL NIL NIL NIL NIL NIL
                         NIL NIL |BasicType&| NIL)
                      (CONS '#((|IntegerNumberSystem|)
                               (|EuclideanDomain|)
                               (|UniqueFactorizationDomain|)
                               (|PrincipalIdealDomain|)
                               (|OrderedIntegralDomain|) (|GcdDomain|)
                               (|IntegralDomain|) (|Algebra| $$)
                               (|CharacteristicZero|)
                               (|DifferentialRing|) (|OrderedRing|)
                               (|CommutativeRing|) (|EntireRing|)
                               (|Module| $$)
                               (|LinearlyExplicitRingOver| 5)
                               (|BiModule| $$ $$) (|Ring|)
                               (|LeftModule| 5) (|OrderedAbelianGroup|)
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
                               (|PatternMatchable| 5)
                               (|DifferentialSpace|) (|OrderedSet|)
                               (|LeftLinearSet| $$)
                               (|RightLinearSet| $$)
                               (|AbelianSemiGroup|) (|SemiGroup|)
                               (|LeftLinearSet| 5) (|Logic|)
                               (|BooleanLogic|) (|RealConstant|)
                               (|RetractableTo| 5)
                               (|DifferentialDomain| $$)
                               (|SetCategory|) (|OpenMath|)
                               (|ConvertibleTo| 102)
                               (|ConvertibleTo| 103)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 105)
                               (|ConvertibleTo| 106)
                               (|ConvertibleTo| 5) (|Type|)
                               (|CoercibleFrom| $$) (|CoercibleFrom| 5)
                               (|BasicType|) (|CoercibleTo| 33))
                            (|makeByteWordVec2| 115
                                '(0 5 0 8 1 12 11 0 13 3 12 11 0 14 14
                                  15 2 12 11 0 5 18 1 12 11 0 19 0 20 0
                                  21 2 12 0 14 20 22 1 12 11 0 23 1 12
                                  11 0 24 1 12 11 0 25 1 5 33 0 34 1 0
                                  0 5 36 0 5 0 71 2 84 0 0 0 85 2 5 0 0
                                  0 86 2 0 9 0 0 1 1 0 0 0 44 1 0 9 0
                                  70 2 0 0 0 0 53 1 0 97 0 98 1 0 0 0 1
                                  1 0 9 0 1 2 0 0 0 0 1 2 0 101 0 0 1 3
                                  0 0 0 0 0 81 1 0 0 0 1 1 0 114 0 1 2
                                  0 9 0 0 1 0 0 61 83 1 0 5 0 1 2 0 0 0
                                  0 78 0 0 0 1 1 0 107 0 1 1 0 5 0 1 2
                                  0 0 0 0 64 1 0 30 91 1 1 0 30 31 32 2
                                  0 90 91 0 1 2 0 90 31 91 92 1 0 101 0
                                  1 1 0 100 0 1 1 0 9 0 1 1 0 99 0 1 0
                                  0 0 95 1 0 0 0 96 2 0 0 0 0 63 1 0
                                  110 108 1 1 0 9 0 1 3 0 0 0 0 0 1 2 0
                                  0 0 0 93 1 0 9 0 1 2 0 0 0 0 1 3 0
                                  104 0 105 104 1 2 0 0 0 0 52 1 0 9 0
                                  72 1 0 9 0 69 1 0 0 0 45 1 0 101 0 1
                                  1 0 9 0 82 2 0 109 108 0 1 3 0 0 0 0
                                  0 79 0 0 0 42 2 0 0 0 0 74 0 0 0 41 2
                                  0 0 0 0 73 1 0 0 0 1 1 0 84 0 88 1 0
                                  0 0 77 1 0 0 108 1 2 0 0 0 0 1 1 0 14
                                  0 1 2 0 0 0 0 1 0 0 0 1 1 0 0 84 87 1
                                  0 0 0 57 1 0 75 0 76 2 0 115 115 115
                                  1 1 0 0 108 1 2 0 0 0 0 67 1 0 0 0 1
                                  1 0 114 0 1 2 0 111 0 0 1 3 0 113 0 0
                                  0 1 2 0 101 0 0 1 2 0 109 108 0 1 1 0
                                  9 0 1 1 0 61 0 1 2 0 65 0 0 66 2 0 0
                                  0 61 1 1 0 0 0 1 1 0 0 0 58 1 0 0 0 1
                                  1 0 102 0 1 1 0 103 0 1 1 0 106 0 1 1
                                  0 105 0 1 1 0 5 0 17 1 0 0 5 94 1 0 0
                                  0 1 1 0 0 5 94 1 0 33 0 35 0 0 61 1 2
                                  0 9 0 0 1 2 0 0 0 0 1 2 0 9 0 0 1 0 0
                                  0 40 2 0 9 0 0 1 2 0 0 0 0 50 3 0 0 0
                                  0 0 80 1 0 0 0 68 2 0 0 0 0 47 0 0 0
                                  7 2 0 0 0 0 51 0 0 0 39 3 0 11 12 0 9
                                  29 2 0 14 0 9 27 2 0 11 12 0 28 1 0
                                  14 0 26 1 0 0 0 48 2 0 0 0 61 1 1 0 0
                                  0 1 2 0 0 0 0 49 2 0 9 0 0 56 2 0 9 0
                                  0 54 2 0 9 0 0 43 2 0 9 0 0 55 2 0 9
                                  0 0 10 2 0 0 0 0 46 1 0 0 0 16 2 0 0
                                  0 0 60 2 0 0 0 0 59 2 0 0 0 61 62 2 0
                                  0 0 84 1 2 0 0 5 0 38 2 0 0 0 0 37 2
                                  0 0 5 0 38 2 0 0 61 0 1 2 0 0 84 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
