
(/VERSIONCHECK 2) 

(|noteSubDomainInfo| '|SingleInteger| '(|Integer|) '(|%ismall?| |#1|)) 

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

(PUT '|SINT;~;2$;16| '|SPADreplace| '|%bitnot|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;not;2$;17|)) 

(PUT '|SINT;not;2$;17| '|SPADreplace| '|%bitnot|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;/\\;3$;18|)) 

(PUT '|SINT;/\\;3$;18| '|SPADreplace| '|%bitand|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;\\/;3$;19|)) 

(PUT '|SINT;\\/;3$;19| '|SPADreplace| '|%bitior|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;Not;2$;20|)) 

(PUT '|SINT;Not;2$;20| '|SPADreplace| '|%bitnot|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;And;3$;21|)) 

(PUT '|SINT;And;3$;21| '|SPADreplace| '|%bitand|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;and;3$;22|)) 

(PUT '|SINT;and;3$;22| '|SPADreplace| '|%bitand|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;Or;3$;23|)) 

(PUT '|SINT;Or;3$;23| '|SPADreplace| '|%bitior|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;or;3$;24|)) 

(PUT '|SINT;or;3$;24| '|SPADreplace| '|%bitior|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;xor;3$;25|)) 

(PUT '|SINT;xor;3$;25| '|SPADreplace| '|%bitxor|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;<;2$B;26|)) 

(PUT '|SINT;<;2$B;26| '|SPADreplace| '|%ilt|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;>;2$B;27|)) 

(PUT '|SINT;>;2$B;27| '|SPADreplace|
     '(XLAM (|x| |y|) (|%ilt| |y| |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;<=;2$B;28|)) 

(PUT '|SINT;<=;2$B;28| '|SPADreplace|
     '(XLAM (|x| |y|) (|%not| (|%ilt| |y| |x|)))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;>=;2$B;29|)) 

(PUT '|SINT;>=;2$B;29| '|SPADreplace|
     '(XLAM (|x| |y|) (|%not| (|%ilt| |x| |y|)))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;inc;2$;30|)) 

(PUT '|SINT;inc;2$;30| '|SPADreplace| '|%iinc|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;dec;2$;31|)) 

(PUT '|SINT;dec;2$;31| '|SPADreplace| '|%idec|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|) |SINT;-;2$;32|)) 

(PUT '|SINT;-;2$;32| '|SPADreplace| '|%ineg|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;+;3$;33|)) 

(PUT '|SINT;+;3$;33| '|SPADreplace| '|%iadd|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;-;3$;34|)) 

(PUT '|SINT;-;3$;34| '|SPADreplace| '|%isub|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;*;3$;35|)) 

(PUT '|SINT;*;3$;35| '|SPADreplace| '|%imul|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| (|%IntegerSection| 0) |%Shell|)
                    |%Short|)
                |SINT;**;$Nni$;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;quo;3$;37|)) 

(PUT '|SINT;quo;3$;37| '|SPADreplace| '|%iquo|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;rem;3$;38|)) 

(PUT '|SINT;rem;3$;38| '|SPADreplace| '|%irem|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Pair|)
                |SINT;divide;2$R;39|)) 

(PUT '|SINT;divide;2$R;39| '|SPADreplace| '|%idivide|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;gcd;3$;40|)) 

(PUT '|SINT;gcd;3$;40| '|SPADreplace| '|%igcd|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;abs;2$;41|)) 

(PUT '|SINT;abs;2$;41| '|SPADreplace| '|%iabs|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;odd?;$B;42|)) 

(PUT '|SINT;odd?;$B;42| '|SPADreplace| '|%iodd?|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;zero?;$B;43|)) 

(PUT '|SINT;zero?;$B;43| '|SPADreplace| '(XLAM (|x|) (|%ieq| |x| 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;one?;$B;44|)) 

(PUT '|SINT;one?;$B;44| '|SPADreplace| '(XLAM (|x|) (|%ieq| |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;max;3$;45|)) 

(PUT '|SINT;max;3$;45| '|SPADreplace| '|%imax|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;min;3$;46|)) 

(PUT '|SINT;min;3$;46| '|SPADreplace| '|%imin|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;hash;2$;47|)) 

(PUT '|SINT;hash;2$;47| '|SPADreplace| '|%hash|) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;length;2$;48|)) 

(PUT '|SINT;length;2$;48| '|SPADreplace| '|%ilength|) 

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

(PUT '|SINT;negative?;$B;53| '|SPADreplace|
     '(XLAM (|x|) (|%ilt| |x| 0))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |SINT;size;Nni;54|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Short|)
                |SINT;index;Pi$;55|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) (|%IntegerSection| 1))
                |SINT;lookup;$Pi;56|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |SINT;reducedSystem;MVR;57|)) 

(PUT '|SINT;reducedSystem;MVR;57| '|SPADreplace| '|%pair|) 

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

(PUT '|SINT;size;Nni;54| '|SPADreplace|
     '(XLAM NIL (|%iadd| (|%isub| |$ShortMaximum| |$ShortMinimum|) 1))) 

(PUT '|SINT;lookup;$Pi;56| '|SPADreplace|
     '(XLAM (|x|) (|%iadd| (|%isub| |x| |$ShortMinimum|) 1))) 

(DEFUN |SINT;writeOMSingleInt| (|dev| |x| $)
  (COND
    ((MINUSP |x|)
     (SEQ (SPADCALL |dev| (|shellEntry| $ 11))
          (SPADCALL |dev| "arith1" "unaryminus" (|shellEntry| $ 13))
          (SPADCALL |dev| (- |x|) (|shellEntry| $ 16))
          (EXIT (SPADCALL |dev| (|shellEntry| $ 17)))))
    (T (SPADCALL |dev| |x| (|shellEntry| $ 16))))) 

(DEFUN |SINT;OMwrite;$S;2| (|x| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|shellEntry| $ 19))
                    (|shellEntry| $ 20))))
    (SEQ (SPADCALL |dev| (|shellEntry| $ 21))
         (|SINT;writeOMSingleInt| |dev| |x| $)
         (SPADCALL |dev| (|shellEntry| $ 22))
         (SPADCALL |dev| (|shellEntry| $ 23))
         (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|)))) 

(DEFUN |SINT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|shellEntry| $ 19))
                    (|shellEntry| $ 20))))
    (SEQ (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 21))))
         (|SINT;writeOMSingleInt| |dev| |x| $)
         (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 22))))
         (SPADCALL |dev| (|shellEntry| $ 23))
         (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|)))) 

(DEFUN |SINT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|shellEntry| $ 21))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|shellEntry| $ 22))))) 

(DEFUN |SINT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 21))))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 22))))))) 

(DEFUN |SINT;reducedSystem;MM;6| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |SINT;coerce;$Of;7| (|x| $) (SPADCALL |x| (|shellEntry| $ 32))) 

(DEFUN |SINT;convert;$I;8| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |SINT;*;I2$;9| (|i| |y| $)
  (* (SPADCALL |i| (|shellEntry| $ 34)) |y|)) 

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

(DEFUN |SINT;<;2$B;26| (|x| |y| $) (DECLARE (IGNORE $)) (< |x| |y|)) 

(DEFUN |SINT;>;2$B;27| (|x| |y| $) (DECLARE (IGNORE $)) (< |y| |x|)) 

(DEFUN |SINT;<=;2$B;28| (|x| |y| $)
  (DECLARE (IGNORE $))
  (NOT (< |y| |x|))) 

(DEFUN |SINT;>=;2$B;29| (|x| |y| $)
  (DECLARE (IGNORE $))
  (NOT (< |x| |y|))) 

(DEFUN |SINT;inc;2$;30| (|x| $) (DECLARE (IGNORE $)) (1+ |x|)) 

(DEFUN |SINT;dec;2$;31| (|x| $) (DECLARE (IGNORE $)) (1- |x|)) 

(DEFUN |SINT;-;2$;32| (|x| $) (DECLARE (IGNORE $)) (- |x|)) 

(DEFUN |SINT;+;3$;33| (|x| |y| $) (DECLARE (IGNORE $)) (+ |x| |y|)) 

(DEFUN |SINT;-;3$;34| (|x| |y| $) (DECLARE (IGNORE $)) (- |x| |y|)) 

(DEFUN |SINT;*;3$;35| (|x| |y| $) (DECLARE (IGNORE $)) (* |x| |y|)) 

(DEFUN |SINT;**;$Nni$;36| (|x| |n| $)
  (SPADCALL (EXPT |x| |n|) (|shellEntry| $ 34))) 

(DEFUN |SINT;quo;3$;37| (|x| |y| $)
  (DECLARE (IGNORE $))
  (TRUNCATE |x| |y|)) 

(DEFUN |SINT;rem;3$;38| (|x| |y| $)
  (DECLARE (IGNORE $))
  (REM |x| |y|)) 

(DEFUN |SINT;divide;2$R;39| (|x| |y| $)
  (DECLARE (IGNORE $))
  (MULTIPLE-VALUE-CALL #'CONS (TRUNCATE |x| |y|))) 

(DEFUN |SINT;gcd;3$;40| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GCD |x| |y|)) 

(DEFUN |SINT;abs;2$;41| (|x| $) (DECLARE (IGNORE $)) (ABS |x|)) 

(DEFUN |SINT;odd?;$B;42| (|x| $) (DECLARE (IGNORE $)) (ODDP |x|)) 

(DEFUN |SINT;zero?;$B;43| (|x| $) (DECLARE (IGNORE $)) (ZEROP |x|)) 

(DEFUN |SINT;one?;$B;44| (|x| $) (DECLARE (IGNORE $)) (EQL |x| 1)) 

(DEFUN |SINT;max;3$;45| (|x| |y| $)
  (DECLARE (IGNORE $))
  (MAX |x| |y|)) 

(DEFUN |SINT;min;3$;46| (|x| |y| $)
  (DECLARE (IGNORE $))
  (MIN |x| |y|)) 

(DEFUN |SINT;hash;2$;47| (|x| $) (DECLARE (IGNORE $)) (SXHASH |x|)) 

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
  (MINUSP |x|)) 

(DEFUN |SINT;size;Nni;54| ($)
  (DECLARE (IGNORE $))
  (+ (- |$ShortMaximum| |$ShortMinimum|) 1)) 

(DEFUN |SINT;index;Pi$;55| (|i| $)
  (LET ((#0=#:G1439 (- (+ |i| |$ShortMinimum|) 1)))
    (|check-subtype| (SMINTP #0#) '(|SingleInteger|) #0#))) 

(DEFUN |SINT;lookup;$Pi;56| (|x| $)
  (DECLARE (IGNORE $))
  (+ (- |x| |$ShortMinimum|) 1)) 

(DEFUN |SINT;reducedSystem;MVR;57| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;58| (|x| |n| $)
  (LET ((|r| (REM |x| |n|)))
    (COND
      ((MINUSP |r|) (COND ((MINUSP |n|) (- |x| |n|)) (T (+ |r| |n|))))
      (T |r|)))) 

(DEFUN |SINT;coerce;I$;59| (|x| $)
  (|check-subtype| (SMINTP |x|) '(|SingleInteger|) |x|)) 

(DEFUN |SINT;random;$;60| ($)
  (SEQ (SETF (|shellEntry| $ 6)
             (REM (TIMES 314159269 (SVREF $ 6)) 2147483647))
       (EXIT (REM (SVREF $ 6) 67108864)))) 

(DEFUN |SINT;random;2$;61| (|n| $) (DECLARE (IGNORE $)) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;62| (|x| $)
  (COND ((MINUSP |x|) (VECTOR -1 (- |x|) -1)) (T (VECTOR 1 |x| 1)))) 

(DEFUN |SingleInteger| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#0=#:G1478)
    (RETURN
      (COND
        ((SETQ #0# (HGET |$ConstructorCache| '|SingleInteger|))
         (|CDRwithIncrement| (CDAR #0#)))
        (T (UNWIND-PROTECT
             (PROG1 (CDDAR (HPUT |$ConstructorCache| '|SingleInteger|
                                 (LIST (CONS NIL
                                        (CONS 1 (|SingleInteger;|))))))
               (SETQ #0# T))
             (COND
               ((NOT #0#) (HREM |$ConstructorCache| '|SingleInteger|))))))))) 

(DEFUN |SingleInteger;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| '(|SingleInteger|)) ($ (|newShell| 115))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|haddProp| |$ConstructorCache| '|SingleInteger| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) 1)
    $)) 

(MAKEPROP '|SingleInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) '|seed| (|Boolean|)
             |SINT;negative?;$B;53| (|Void|) (|OpenMathDevice|)
             (0 . |OMputApp|) (|String|) (5 . |OMputSymbol|)
             |SINT;-;2$;32| |SINT;convert;$I;8| (12 . |OMputInteger|)
             (18 . |OMputEndApp|) (|OpenMathEncoding|)
             (23 . |OMencodingXML|) (27 . |OMopenString|)
             (33 . |OMputObject|) (38 . |OMputEndObject|)
             (43 . |OMclose|) |SINT;OMwrite;$S;2| |SINT;OMwrite;$BS;3|
             |SINT;OMwrite;Omd$V;4| |SINT;OMwrite;Omd$BV;5|
             (|Matrix| 5) (|Matrix| $) |SINT;reducedSystem;MM;6|
             (|OutputForm|) (48 . |coerce|) |SINT;coerce;$Of;7|
             (53 . |coerce|) |SINT;*;I2$;9|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;Zero;$;10|) $))
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
             |SINT;or;3$;24| |SINT;xor;3$;25| |SINT;<;2$B;26|
             |SINT;>;2$B;27| |SINT;<=;2$B;28| |SINT;>=;2$B;29|
             |SINT;inc;2$;30| |SINT;dec;2$;31| |SINT;+;3$;33|
             |SINT;-;3$;34| |SINT;*;3$;35| (|NonNegativeInteger|)
             |SINT;**;$Nni$;36| |SINT;quo;3$;37| |SINT;rem;3$;38|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;39| |SINT;gcd;3$;40| |SINT;abs;2$;41|
             |SINT;odd?;$B;42| |SINT;zero?;$B;43| |SINT;one?;$B;44|
             |SINT;max;3$;45| |SINT;min;3$;46| (|SingleInteger|)
             |SINT;hash;2$;47| |SINT;length;2$;48| |SINT;shift;3$;49|
             |SINT;mulmod;4$;50| |SINT;addmod;4$;51|
             |SINT;submod;4$;52| |SINT;size;Nni;54| (|PositiveInteger|)
             (58 . +) (64 . |One|) (68 . -) |SINT;index;Pi$;55|
             |SINT;lookup;$Pi;56| (|Vector| 5)
             (|Record| (|:| |mat| 28) (|:| |vec| 88)) (|Vector| $)
             |SINT;reducedSystem;MVR;57| |SINT;positiveRemainder;3$;58|
             |SINT;coerce;I$;59| |SINT;random;$;60| |SINT;random;2$;61|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;62| (|Fraction| 5)
             (|Union| 98 '"failed") (|Union| $ '"failed") (|Float|)
             (|DoubleFloat|) (|PatternMatchResult| 5 $) (|Pattern| 5)
             (|InputForm|) (|Union| 5 '"failed") (|List| $)
             (|Union| 107 '"failed")
             (|Record| (|:| |coef| 107) (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 111 '"failed") (|Factored| $)
             (|SparseUnivariatePolynomial| $))
          '#(~= 74 ~ 80 |zero?| 85 |xor| 90 |unitNormal| 96
             |unitCanonical| 101 |unit?| 106 |symmetricRemainder| 111
             |subtractIfCan| 117 |submod| 123 |squareFreePart| 130
             |squareFree| 135 |sizeLess?| 140 |size| 146 |sign| 150
             |shift| 155 |sample| 161 |retractIfCan| 165 |retract| 170
             |rem| 175 |reducedSystem| 181 |recip| 192 |rationalIfCan|
             197 |rational?| 202 |rational| 207 |random| 212 |quo| 221
             |principalIdeal| 227 |prime?| 232 |powmod| 237
             |positiveRemainder| 244 |positive?| 250 |permutation| 255
             |patternMatch| 261 |or| 268 |one?| 274 |odd?| 279 |not|
             284 |nextItem| 289 |negative?| 294 |multiEuclidean| 299
             |mulmod| 305 |min| 312 |max| 322 |mask| 332 |lookup| 337
             |length| 342 |leftReducedSystem| 347 |lcm| 358 |latex| 369
             |invmod| 374 |init| 380 |index| 384 |inc| 389 |hash| 394
             |gcdPolynomial| 399 |gcd| 405 |factorial| 416 |factor| 421
             |extendedEuclidean| 426 |exquo| 439 |expressIdealMember|
             445 |even?| 451 |euclideanSize| 456 |divide| 461
             |differentiate| 467 |dec| 478 |copy| 483 |convert| 488
             |coerce| 513 |characteristic| 533 |bit?| 537 |binomial|
             543 |before?| 549 |base| 555 |associates?| 559 |and| 565
             |addmod| 571 |abs| 578 |\\/| 583 |Zero| 589 |Or| 593 |One|
             599 |OMwrite| 603 |Not| 627 D 632 |And| 643 >= 649 > 655 =
             661 <= 667 < 673 |/\\| 679 - 685 + 696 ** 702 * 714)
          '((|noetherian| . 0) (|canonicalsClosed| . 0)
            (|canonical| . 0) (|canonicalUnitNormal| . 0)
            (|multiplicativeValuation| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |OrderedRing&| NIL NIL |Module&| NIL NIL
                         |Ring&| NIL NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL NIL NIL
                         |AbelianMonoid&| |Monoid&| NIL NIL NIL NIL NIL
                         NIL |AbelianSemiGroup&| |SemiGroup&| NIL
                         |BooleanLogic&| |DifferentialSpace&|
                         |OrderedType&| |SetCategory&| |Logic&| NIL
                         |RetractableTo&| |DifferentialDomain&|
                         |BasicType&| NIL NIL NIL NIL NIL NIL NIL NIL
                         NIL NIL NIL)
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
                               (|PatternMatchable| 5) (|OrderedSet|)
                               (|LeftLinearSet| $$)
                               (|RightLinearSet| $$)
                               (|AbelianSemiGroup|) (|SemiGroup|)
                               (|LeftLinearSet| 5) (|BooleanLogic|)
                               (|DifferentialSpace|) (|OrderedType|)
                               (|SetCategory|) (|Logic|)
                               (|RealConstant|) (|RetractableTo| 5)
                               (|DifferentialDomain| $$) (|BasicType|)
                               (|OpenMath|) (|ConvertibleTo| 101)
                               (|ConvertibleTo| 102)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 104)
                               (|ConvertibleTo| 105)
                               (|ConvertibleTo| 5) (|CoercibleFrom| $$)
                               (|CoercibleFrom| 5) (|Type|)
                               (|CoercibleTo| 31))
                            (|makeByteWordVec2| 114
                                '(1 10 9 0 11 3 10 9 0 12 12 13 2 10 9
                                  0 5 16 1 10 9 0 17 0 18 0 19 2 10 0
                                  12 18 20 1 10 9 0 21 1 10 9 0 22 1 10
                                  9 0 23 1 5 31 0 32 1 0 0 5 34 2 82 0
                                  0 0 83 0 82 0 84 2 5 0 0 0 85 2 0 7 0
                                  0 1 1 0 0 0 42 1 0 7 0 70 2 0 0 0 0
                                  51 1 0 96 0 97 1 0 0 0 1 1 0 7 0 1 2
                                  0 0 0 0 1 2 0 100 0 0 1 3 0 0 0 0 0
                                  80 1 0 0 0 1 1 0 113 0 1 2 0 7 0 0 1
                                  0 0 61 81 1 0 5 0 1 2 0 0 0 0 77 0 0
                                  0 1 1 0 106 0 1 1 0 5 0 1 2 0 0 0 0
                                  64 1 0 28 29 30 2 0 89 29 90 91 1 0
                                  100 0 1 1 0 99 0 1 1 0 7 0 1 1 0 98 0
                                  1 0 0 0 94 1 0 0 0 95 2 0 0 0 0 63 1
                                  0 109 107 1 1 0 7 0 1 3 0 0 0 0 0 1 2
                                  0 0 0 0 92 1 0 7 0 1 2 0 0 0 0 1 3 0
                                  103 0 104 103 1 2 0 0 0 0 50 1 0 7 0
                                  71 1 0 7 0 69 1 0 0 0 43 1 0 100 0 1
                                  1 0 7 0 8 2 0 108 107 0 1 3 0 0 0 0 0
                                  78 0 0 0 40 2 0 0 0 0 73 0 0 0 39 2 0
                                  0 0 0 72 1 0 0 0 1 1 0 82 0 87 1 0 0
                                  0 76 1 0 28 90 1 2 0 89 90 0 1 1 0 0
                                  107 1 2 0 0 0 0 1 1 0 12 0 1 2 0 0 0
                                  0 1 0 0 0 1 1 0 0 82 86 1 0 0 0 56 1
                                  0 74 0 75 2 0 114 114 114 1 1 0 0 107
                                  1 2 0 0 0 0 67 1 0 0 0 1 1 0 113 0 1
                                  2 0 110 0 0 1 3 0 112 0 0 0 1 2 0 100
                                  0 0 1 2 0 108 107 0 1 1 0 7 0 1 1 0
                                  61 0 1 2 0 65 0 0 66 2 0 0 0 61 1 1 0
                                  0 0 1 1 0 0 0 57 1 0 0 0 1 1 0 101 0
                                  1 1 0 102 0 1 1 0 105 0 1 1 0 104 0 1
                                  1 0 5 0 15 1 0 0 5 93 1 0 0 0 1 1 0 0
                                  5 93 1 0 31 0 33 0 0 61 1 2 0 7 0 0 1
                                  2 0 0 0 0 1 2 0 7 0 0 1 0 0 0 38 2 0
                                  7 0 0 1 2 0 0 0 0 48 3 0 0 0 0 0 79 1
                                  0 0 0 68 2 0 0 0 0 45 0 0 0 36 2 0 0
                                  0 0 49 0 0 0 37 3 0 9 10 0 7 27 2 0
                                  12 0 7 25 2 0 9 10 0 26 1 0 12 0 24 1
                                  0 0 0 46 2 0 0 0 61 1 1 0 0 0 1 2 0 0
                                  0 0 47 2 0 7 0 0 55 2 0 7 0 0 53 2 0
                                  7 0 0 41 2 0 7 0 0 54 2 0 7 0 0 52 2
                                  0 0 0 0 44 1 0 0 0 14 2 0 0 0 0 59 2
                                  0 0 0 0 58 2 0 0 0 61 62 2 0 0 0 82 1
                                  2 0 0 5 0 35 2 0 0 0 0 60 2 0 0 5 0
                                  35 2 0 0 61 0 1 2 0 0 82 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
