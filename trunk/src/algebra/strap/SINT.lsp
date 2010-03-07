
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

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;>;2$B;25|)) 

(PUT '|SINT;>;2$B;25| '|SPADreplace| 'QSGREATERP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;<=;2$B;26|)) 

(PUT '|SINT;<=;2$B;26| '|SPADreplace|
     '(XLAM (|x| |y|) (NOT (> |x| |y|)))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Boolean|)
                |SINT;>=;2$B;27|)) 

(PUT '|SINT;>=;2$B;27| '|SPADreplace| '>=) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;inc;2$;28|)) 

(PUT '|SINT;inc;2$;28| '|SPADreplace| 'QSADD1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;dec;2$;29|)) 

(PUT '|SINT;dec;2$;29| '|SPADreplace| 'QSSUB1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|) |SINT;-;2$;30|)) 

(PUT '|SINT;-;2$;30| '|SPADreplace| 'QSMINUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;+;3$;31|)) 

(PUT '|SINT;+;3$;31| '|SPADreplace| 'QSPLUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;-;3$;32|)) 

(PUT '|SINT;-;3$;32| '|SPADreplace| 'QSDIFFERENCE) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;*;3$;33|)) 

(PUT '|SINT;*;3$;33| '|SPADreplace| 'QSTIMES) 

(DECLAIM (FTYPE (FUNCTION (|%Short| (|%IntegerSection| 0) |%Shell|)
                    |%Short|)
                |SINT;**;$Nni$;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;quo;3$;35|)) 

(PUT '|SINT;quo;3$;35| '|SPADreplace| 'QSQUOTIENT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;rem;3$;36|)) 

(PUT '|SINT;rem;3$;36| '|SPADreplace| 'QSREMAINDER) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Pair|)
                |SINT;divide;2$R;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;gcd;3$;38|)) 

(PUT '|SINT;gcd;3$;38| '|SPADreplace| 'GCD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;abs;2$;39|)) 

(PUT '|SINT;abs;2$;39| '|SPADreplace| 'QSABSVAL) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;odd?;$B;40|)) 

(PUT '|SINT;odd?;$B;40| '|SPADreplace| 'QSODDP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;zero?;$B;41|)) 

(PUT '|SINT;zero?;$B;41| '|SPADreplace| 'QSZEROP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;one?;$B;42|)) 

(PUT '|SINT;one?;$B;42| '|SPADreplace| '(XLAM (|x|) (EQL |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;max;3$;43|)) 

(PUT '|SINT;max;3$;43| '|SPADreplace| 'QSMAX) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;min;3$;44|)) 

(PUT '|SINT;min;3$;44| '|SPADreplace| 'QSMIN) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;hash;2$;45|)) 

(PUT '|SINT;hash;2$;45| '|SPADreplace| 'HASHEQ) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;length;2$;46|)) 

(PUT '|SINT;length;2$;46| '|SPADreplace| 'INTEGER-LENGTH) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;shift;3$;47|)) 

(PUT '|SINT;shift;3$;47| '|SPADreplace| 'QSLEFTSHIFT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;mulmod;4$;48|)) 

(PUT '|SINT;mulmod;4$;48| '|SPADreplace| 'QSMULTMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;addmod;4$;49|)) 

(PUT '|SINT;addmod;4$;49| '|SPADreplace| 'QSADDMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;submod;4$;50|)) 

(PUT '|SINT;submod;4$;50| '|SPADreplace| 'QSDIFMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;negative?;$B;51|)) 

(PUT '|SINT;negative?;$B;51| '|SPADreplace| 'QSMINUSP) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |SINT;size;Nni;52|)) 

(PUT '|SINT;size;Nni;52| '|SPADreplace|
     '(XLAM NIL (+ (- |$ShortMaximum| |$ShortMinimum|) 1))) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Short|)
                |SINT;index;Pi$;53|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) (|%IntegerSection| 1))
                |SINT;lookup;$Pi;54|)) 

(PUT '|SINT;lookup;$Pi;54| '|SPADreplace|
     '(XLAM (|x|) (+ (- |x| |$ShortMinimum|) 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |SINT;reducedSystem;MVR;55|)) 

(PUT '|SINT;reducedSystem;MVR;55| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;positiveRemainder;3$;56|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |SINT;coerce;I$;57|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;random;$;58|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;random;2$;59|)) 

(PUT '|SINT;random;2$;59| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Shell|)
                |SINT;unitNormal;$R;60|)) 

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

(DEFUN |SINT;>;2$B;25| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSGREATERP |x| |y|)) 

(DEFUN |SINT;<=;2$B;26| (|x| |y| $)
  (DECLARE (IGNORE $))
  (NOT (> |x| |y|))) 

(DEFUN |SINT;>=;2$B;27| (|x| |y| $) (DECLARE (IGNORE $)) (>= |x| |y|)) 

(DEFUN |SINT;inc;2$;28| (|x| $) (DECLARE (IGNORE $)) (QSADD1 |x|)) 

(DEFUN |SINT;dec;2$;29| (|x| $) (DECLARE (IGNORE $)) (QSSUB1 |x|)) 

(DEFUN |SINT;-;2$;30| (|x| $) (DECLARE (IGNORE $)) (QSMINUS |x|)) 

(DEFUN |SINT;+;3$;31| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSPLUS |x| |y|)) 

(DEFUN |SINT;-;3$;32| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSDIFFERENCE |x| |y|)) 

(DEFUN |SINT;*;3$;33| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSTIMES |x| |y|)) 

(DEFUN |SINT;**;$Nni$;34| (|x| |n| $)
  (SPADCALL (EXPT |x| |n|) (|getShellEntry| $ 36))) 

(DEFUN |SINT;quo;3$;35| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSQUOTIENT |x| |y|)) 

(DEFUN |SINT;rem;3$;36| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSREMAINDER |x| |y|)) 

(DEFUN |SINT;divide;2$R;37| (|x| |y| $)
  (CONS (QSQUOTIENT |x| |y|) (QSREMAINDER |x| |y|))) 

(DEFUN |SINT;gcd;3$;38| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GCD |x| |y|)) 

(DEFUN |SINT;abs;2$;39| (|x| $) (DECLARE (IGNORE $)) (QSABSVAL |x|)) 

(DEFUN |SINT;odd?;$B;40| (|x| $) (DECLARE (IGNORE $)) (QSODDP |x|)) 

(DEFUN |SINT;zero?;$B;41| (|x| $) (DECLARE (IGNORE $)) (QSZEROP |x|)) 

(DEFUN |SINT;one?;$B;42| (|x| $) (DECLARE (IGNORE $)) (EQL |x| 1)) 

(DEFUN |SINT;max;3$;43| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMAX |x| |y|)) 

(DEFUN |SINT;min;3$;44| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMIN |x| |y|)) 

(DEFUN |SINT;hash;2$;45| (|x| $) (DECLARE (IGNORE $)) (HASHEQ |x|)) 

(DEFUN |SINT;length;2$;46| (|x| $)
  (DECLARE (IGNORE $))
  (INTEGER-LENGTH |x|)) 

(DEFUN |SINT;shift;3$;47| (|x| |n| $)
  (DECLARE (IGNORE $))
  (QSLEFTSHIFT |x| |n|)) 

(DEFUN |SINT;mulmod;4$;48| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSMULTMOD |a| |b| |p|)) 

(DEFUN |SINT;addmod;4$;49| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSADDMOD |a| |b| |p|)) 

(DEFUN |SINT;submod;4$;50| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSDIFMOD |a| |b| |p|)) 

(DEFUN |SINT;negative?;$B;51| (|x| $)
  (DECLARE (IGNORE $))
  (QSMINUSP |x|)) 

(DEFUN |SINT;size;Nni;52| ($)
  (DECLARE (IGNORE $))
  (+ (- |$ShortMaximum| |$ShortMinimum|) 1)) 

(DEFUN |SINT;index;Pi$;53| (|i| $)
  (PROG (#0=#:G1457)
    (RETURN
      (PROG1 (LETT #0# (- (+ |i| |$ShortMinimum|) 1)
                   |SINT;index;Pi$;53|)
        (|check-subtype| (SMINTP #0#) '(|SingleInteger|) #0#))))) 

(DEFUN |SINT;lookup;$Pi;54| (|x| $)
  (DECLARE (IGNORE $))
  (+ (- |x| |$ShortMinimum|) 1)) 

(DEFUN |SINT;reducedSystem;MVR;55| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;56| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (QSREMAINDER |x| |n|)
                 |SINT;positiveRemainder;3$;56|)
           (EXIT (COND
                   ((QSMINUSP |r|)
                    (COND
                      ((QSMINUSP |n|) (QSDIFFERENCE |x| |n|))
                      ('T (QSPLUS |r| |n|))))
                   ('T |r|))))))) 

(DEFUN |SINT;coerce;I$;57| (|x| $)
  (PROG1 |x| (|check-subtype| (SMINTP |x|) '(|SingleInteger|) |x|))) 

(DEFUN |SINT;random;$;58| ($)
  (SEQ (|setShellEntry| $ 6
           (REMAINDER (TIMES 314159269 (|getShellEntry| $ 6))
               2147483647))
       (EXIT (REMAINDER (|getShellEntry| $ 6) 67108864)))) 

(DEFUN |SINT;random;2$;59| (|n| $) (DECLARE (IGNORE $)) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;60| (|x| $)
  (COND
    ((QSLESSP |x| 0) (VECTOR -1 (QSMINUS |x|) -1))
    ('T (VECTOR 1 |x| 1)))) 

(DEFUN |SingleInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1492)
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
        (LETT $ (|newShell| 115) . #0#)
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
             (9 . |OMputSymbol|) |SINT;-;2$;30| |SINT;convert;$I;8|
             (16 . |OMputInteger|) (22 . |OMputEndApp|)
             (|OpenMathEncoding|) (27 . |OMencodingXML|)
             (31 . |OMopenString|) (37 . |OMputObject|)
             (42 . |OMputEndObject|) (47 . |OMclose|)
             |SINT;OMwrite;$S;2| |SINT;OMwrite;$BS;3|
             |SINT;OMwrite;Omd$V;4| |SINT;OMwrite;Omd$BV;5|
             (|Matrix| 5) (|Matrix| $) |SINT;reducedSystem;MM;6|
             (|OutputForm|) (52 . |coerce|) |SINT;coerce;$Of;7|
             (57 . |coerce|) |SINT;*;3$;33| |SINT;*;I2$;9|
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
             |SINT;>;2$B;25| (62 . |not|) |SINT;<=;2$B;26|
             |SINT;>=;2$B;27| |SINT;inc;2$;28| |SINT;dec;2$;29|
             |SINT;+;3$;31| |SINT;-;3$;32| (|NonNegativeInteger|)
             |SINT;**;$Nni$;34| |SINT;quo;3$;35| |SINT;rem;3$;36|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;37| |SINT;gcd;3$;38| |SINT;abs;2$;39|
             |SINT;odd?;$B;40| |SINT;zero?;$B;41| (67 . |One|)
             |SINT;one?;$B;42| |SINT;max;3$;43| |SINT;min;3$;44|
             (|SingleInteger|) |SINT;hash;2$;45| |SINT;length;2$;46|
             |SINT;shift;3$;47| |SINT;mulmod;4$;48| |SINT;addmod;4$;49|
             |SINT;submod;4$;50| |SINT;negative?;$B;51|
             |SINT;size;Nni;52| (|PositiveInteger|) (71 . +) (77 . -)
             |SINT;index;Pi$;53| |SINT;lookup;$Pi;54| (|Vector| 5)
             (|Record| (|:| |mat| 30) (|:| |vec| 88)) (|Vector| $)
             |SINT;reducedSystem;MVR;55| |SINT;positiveRemainder;3$;56|
             |SINT;coerce;I$;57| |SINT;random;$;58| |SINT;random;2$;59|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;60| (|Fraction| 5)
             (|Union| 98 '"failed") (|DoubleFloat|)
             (|Union| $ '"failed") (|Float|) (|PatternMatchResult| 5 $)
             (|Pattern| 5) (|InputForm|) (|Union| 5 '"failed")
             (|List| $) (|Union| 107 '"failed")
             (|Record| (|:| |coef| 107) (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 110 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Factored| $) (|SparseUnivariatePolynomial| $))
          '#(~= 83 ~ 89 |zero?| 94 |xor| 99 |unitNormal| 105
             |unitCanonical| 110 |unit?| 115 |symmetricRemainder| 120
             |subtractIfCan| 126 |submod| 132 |squareFreePart| 139
             |squareFree| 144 |sizeLess?| 149 |size| 155 |sign| 159
             |shift| 164 |sample| 170 |retractIfCan| 174 |retract| 179
             |rem| 184 |reducedSystem| 190 |recip| 201 |rationalIfCan|
             206 |rational?| 211 |rational| 216 |random| 221 |quo| 230
             |principalIdeal| 236 |prime?| 241 |powmod| 246
             |positiveRemainder| 253 |positive?| 259 |permutation| 264
             |patternMatch| 270 |one?| 277 |odd?| 282 |not| 287
             |nextItem| 292 |negative?| 297 |multiEuclidean| 302
             |mulmod| 308 |min| 315 |max| 325 |mask| 335 |lookup| 340
             |length| 345 |lcm| 350 |latex| 361 |invmod| 366 |init| 372
             |index| 376 |inc| 381 |hash| 386 |gcdPolynomial| 391 |gcd|
             397 |factorial| 408 |factor| 413 |extendedEuclidean| 418
             |exquo| 431 |expressIdealMember| 437 |even?| 443
             |euclideanSize| 448 |divide| 453 |differentiate| 459 |dec|
             470 |copy| 475 |convert| 480 |coerce| 505 |characteristic|
             525 |bit?| 529 |binomial| 535 |before?| 541 |base| 547
             |associates?| 551 |addmod| 557 |abs| 564 |\\/| 569 |Zero|
             575 |Or| 579 |One| 585 |OMwrite| 589 |Not| 613 D 618 |And|
             629 >= 635 > 641 = 647 <= 653 < 659 |/\\| 665 - 671 + 682
             ** 688 * 700)
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
                               (|ConvertibleTo| 102)
                               (|ConvertibleTo| 100)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 104)
                               (|ConvertibleTo| 105)
                               (|ConvertibleTo| 5) (|CoercibleFrom| $$)
                               (|CoercibleFrom| 5) (|BasicType|)
                               (|CoercibleTo| 33))
                            (|makeByteWordVec2| 114
                                '(0 5 0 8 1 12 11 0 13 3 12 11 0 14 14
                                  15 2 12 11 0 5 18 1 12 11 0 19 0 20 0
                                  21 2 12 0 14 20 22 1 12 11 0 23 1 12
                                  11 0 24 1 12 11 0 25 1 5 33 0 34 1 0
                                  0 5 36 1 9 0 0 53 0 5 0 70 2 83 0 0 0
                                  84 2 5 0 0 0 85 2 0 9 0 0 1 1 0 0 0
                                  44 1 0 9 0 69 2 0 0 0 0 51 1 0 96 0
                                  97 1 0 0 0 1 1 0 9 0 1 2 0 0 0 0 1 2
                                  0 101 0 0 1 3 0 0 0 0 0 80 1 0 0 0 1
                                  1 0 113 0 1 2 0 9 0 0 1 0 0 60 82 1 0
                                  5 0 1 2 0 0 0 0 77 0 0 0 1 1 0 106 0
                                  1 1 0 5 0 1 2 0 0 0 0 63 2 0 89 31 90
                                  91 1 0 30 31 32 1 0 101 0 1 1 0 99 0
                                  1 1 0 9 0 1 1 0 98 0 1 1 0 0 0 95 0 0
                                  0 94 2 0 0 0 0 62 1 0 109 107 1 1 0 9
                                  0 1 3 0 0 0 0 0 1 2 0 0 0 0 92 1 0 9
                                  0 1 2 0 0 0 0 1 3 0 103 0 104 103 1 1
                                  0 9 0 71 1 0 9 0 68 1 0 0 0 45 1 0
                                  101 0 1 1 0 9 0 81 2 0 108 107 0 1 3
                                  0 0 0 0 0 78 0 0 0 42 2 0 0 0 0 73 0
                                  0 0 41 2 0 0 0 0 72 1 0 0 0 1 1 0 83
                                  0 87 1 0 0 0 76 2 0 0 0 0 1 1 0 0 107
                                  1 1 0 14 0 1 2 0 0 0 0 1 0 0 0 1 1 0
                                  0 83 86 1 0 0 0 56 1 0 74 0 75 2 0
                                  114 114 114 1 2 0 0 0 0 66 1 0 0 107
                                  1 1 0 0 0 1 1 0 113 0 1 3 0 111 0 0 0
                                  1 2 0 112 0 0 1 2 0 101 0 0 1 2 0 108
                                  107 0 1 1 0 9 0 1 1 0 60 0 1 2 0 64 0
                                  0 65 1 0 0 0 1 2 0 0 0 60 1 1 0 0 0
                                  57 1 0 0 0 1 1 0 100 0 1 1 0 102 0 1
                                  1 0 105 0 1 1 0 104 0 1 1 0 5 0 17 1
                                  0 0 5 93 1 0 0 0 1 1 0 0 5 93 1 0 33
                                  0 35 0 0 60 1 2 0 9 0 0 1 2 0 0 0 0 1
                                  2 0 9 0 0 1 0 0 0 40 2 0 9 0 0 1 3 0
                                  0 0 0 0 79 1 0 0 0 67 2 0 0 0 0 47 0
                                  0 0 7 2 0 0 0 0 50 0 0 0 39 2 0 11 12
                                  0 28 3 0 11 12 0 9 29 2 0 14 0 9 27 1
                                  0 14 0 26 1 0 0 0 48 1 0 0 0 1 2 0 0
                                  0 60 1 2 0 0 0 0 49 2 0 9 0 0 55 2 0
                                  9 0 0 52 2 0 9 0 0 43 2 0 9 0 0 54 2
                                  0 9 0 0 10 2 0 0 0 0 46 2 0 0 0 0 59
                                  1 0 0 0 16 2 0 0 0 0 58 2 0 0 0 60 61
                                  2 0 0 0 83 1 2 0 0 0 0 37 2 0 0 5 0
                                  38 2 0 0 60 0 1 2 0 0 83 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
