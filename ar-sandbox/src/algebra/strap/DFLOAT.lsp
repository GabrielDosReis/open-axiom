
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%String|)
                |DFLOAT;OMwrite;$S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Boolean| |%Shell|)
                    |%String|)
                |DFLOAT;OMwrite;$BS;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%DoubleFloat| |%Shell|) |%Void|)
                |DFLOAT;OMwrite;Omd$V;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%DoubleFloat| |%Boolean| |%Shell|)
                    |%Void|)
                |DFLOAT;OMwrite;Omd$BV;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;checkComplex|)) 

(PUT '|DFLOAT;checkComplex| '|SPADreplace| 'C-TO-R) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 1))
                |DFLOAT;base;Pi;6|)) 

(PUT '|DFLOAT;base;Pi;6| '|SPADreplace| '(XLAM NIL (FLOAT-RADIX 0.0))) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Integer|)
                |DFLOAT;mantissa;$I;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Integer|)
                |DFLOAT;exponent;$I;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 1))
                |DFLOAT;precision;Pi;9|)) 

(PUT '|DFLOAT;precision;Pi;9| '|SPADreplace|
     '(XLAM NIL (FLOAT-DIGITS 0.0))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 1))
                |DFLOAT;bits;Pi;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%DoubleFloat|) |DFLOAT;max;$;11|)) 

(PUT '|DFLOAT;max;$;11| '|SPADreplace|
     '(XLAM NIL |$DoubleFloatMaximum|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%DoubleFloat|) |DFLOAT;min;$;12|)) 

(PUT '|DFLOAT;min;$;12| '|SPADreplace|
     '(XLAM NIL |$DoubleFloatMinimum|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Integer|)
                |DFLOAT;order;$I;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%DoubleFloat|)
                |DFLOAT;Zero;$;14|)) 

(PUT '|DFLOAT;Zero;$;14| '|SPADreplace|
     '(XLAM NIL (FLOAT 0 |$DoubleFloatMaximum|))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%DoubleFloat|) |DFLOAT;One;$;15|)) 

(PUT '|DFLOAT;One;$;15| '|SPADreplace|
     '(XLAM NIL (FLOAT 1 |$DoubleFloatMaximum|))) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%DoubleFloat|)
                |DFLOAT;exp1;$;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%DoubleFloat|) |DFLOAT;pi;$;17|)) 

(PUT '|DFLOAT;pi;$;17| '|SPADreplace| '(XLAM NIL PI)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Thing|)
                |DFLOAT;coerce;$Of;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Thing|)
                |DFLOAT;convert;$If;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%Boolean|)
                |DFLOAT;<;2$B;20|)) 

(PUT '|DFLOAT;<;2$B;20| '|SPADreplace| '<) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%Boolean|)
                |DFLOAT;>;2$B;21|)) 

(PUT '|DFLOAT;>;2$B;21| '|SPADreplace| '>) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%Boolean|)
                |DFLOAT;<=;2$B;22|)) 

(PUT '|DFLOAT;<=;2$B;22| '|SPADreplace|
     '(XLAM (|x| |y|) (NOT (> |x| |y|)))) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%Boolean|)
                |DFLOAT;>=;2$B;23|)) 

(PUT '|DFLOAT;>=;2$B;23| '|SPADreplace| '>=) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;-;2$;24|)) 

(PUT '|DFLOAT;-;2$;24| '|SPADreplace| '-) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;+;3$;25|)) 

(PUT '|DFLOAT;+;3$;25| '|SPADreplace| '+) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;-;3$;26|)) 

(PUT '|DFLOAT;-;3$;26| '|SPADreplace| '-) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;*;3$;27|)) 

(PUT '|DFLOAT;*;3$;27| '|SPADreplace| '*) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;*;I2$;28|)) 

(PUT '|DFLOAT;*;I2$;28| '|SPADreplace| '*) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;max;3$;29|)) 

(PUT '|DFLOAT;max;3$;29| '|SPADreplace| 'MAX) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;min;3$;30|)) 

(PUT '|DFLOAT;min;3$;30| '|SPADreplace| 'MIN) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%Boolean|)
                |DFLOAT;=;2$B;31|)) 

(PUT '|DFLOAT;=;2$B;31| '|SPADreplace| '=) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Integer| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;/;$I$;32|)) 

(PUT '|DFLOAT;/;$I$;32| '|SPADreplace| '/) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;sqrt;2$;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;log10;2$;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Integer| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;**;$I$;35|)) 

(PUT '|DFLOAT;**;$I$;35| '|SPADreplace| 'EXPT) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;**;3$;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%DoubleFloat|)
                |DFLOAT;coerce;I$;37|)) 

(PUT '|DFLOAT;coerce;I$;37| '|SPADreplace|
     '(XLAM (|i|) (FLOAT |i| |$DoubleFloatMaximum|))) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;exp;2$;38|)) 

(PUT '|DFLOAT;exp;2$;38| '|SPADreplace| 'EXP) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;log;2$;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;log2;2$;40|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;sin;2$;41|)) 

(PUT '|DFLOAT;sin;2$;41| '|SPADreplace| 'SIN) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;cos;2$;42|)) 

(PUT '|DFLOAT;cos;2$;42| '|SPADreplace| 'COS) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;tan;2$;43|)) 

(PUT '|DFLOAT;tan;2$;43| '|SPADreplace| 'TAN) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;cot;2$;44|)) 

(PUT '|DFLOAT;cot;2$;44| '|SPADreplace| 'COT) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;sec;2$;45|)) 

(PUT '|DFLOAT;sec;2$;45| '|SPADreplace| 'SEC) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;csc;2$;46|)) 

(PUT '|DFLOAT;csc;2$;46| '|SPADreplace| 'CSC) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;asin;2$;47|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;acos;2$;48|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;atan;2$;49|)) 

(PUT '|DFLOAT;atan;2$;49| '|SPADreplace| 'ATAN) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;acsc;2$;50|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;acot;2$;51|)) 

(PUT '|DFLOAT;acot;2$;51| '|SPADreplace| 'ACOT) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;asec;2$;52|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;sinh;2$;53|)) 

(PUT '|DFLOAT;sinh;2$;53| '|SPADreplace| 'SINH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;cosh;2$;54|)) 

(PUT '|DFLOAT;cosh;2$;54| '|SPADreplace| 'COSH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;tanh;2$;55|)) 

(PUT '|DFLOAT;tanh;2$;55| '|SPADreplace| 'TANH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;csch;2$;56|)) 

(PUT '|DFLOAT;csch;2$;56| '|SPADreplace| 'CSCH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;coth;2$;57|)) 

(PUT '|DFLOAT;coth;2$;57| '|SPADreplace| 'COTH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;sech;2$;58|)) 

(PUT '|DFLOAT;sech;2$;58| '|SPADreplace| 'SECH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;asinh;2$;59|)) 

(PUT '|DFLOAT;asinh;2$;59| '|SPADreplace| 'ASINH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;acosh;2$;60|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;atanh;2$;61|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;acsch;2$;62|)) 

(PUT '|DFLOAT;acsch;2$;62| '|SPADreplace| 'ACSCH) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;acoth;2$;63|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;asech;2$;64|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;/;3$;65|)) 

(PUT '|DFLOAT;/;3$;65| '|SPADreplace| '/) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Boolean|)
                |DFLOAT;negative?;$B;66|)) 

(PUT '|DFLOAT;negative?;$B;66| '|SPADreplace| 'MINUSP) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Boolean|)
                |DFLOAT;zero?;$B;67|)) 

(PUT '|DFLOAT;zero?;$B;67| '|SPADreplace| 'ZEROP) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Boolean|)
                |DFLOAT;one?;$B;68|)) 

(PUT '|DFLOAT;one?;$B;68| '|SPADreplace| '(XLAM (|x|) (= |x| 1.0))) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Short|)
                |DFLOAT;hash;$Si;69|)) 

(PUT '|DFLOAT;hash;$Si;69| '|SPADreplace| 'HASHEQ) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Pair|)
                |DFLOAT;recip;$U;70|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;differentiate;2$;71|)) 

(PUT '|DFLOAT;differentiate;2$;71| '|SPADreplace| '(XLAM (|x|) 0.0)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;Gamma;2$;72|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;Beta;3$;73|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Integer|)
                |DFLOAT;wholePart;$I;74|)) 

(PUT '|DFLOAT;wholePart;$I;74| '|SPADreplace| 'FIX) 

(DECLAIM (FTYPE (FUNCTION
                    (|%Integer| |%Integer| (|%IntegerSection| 1)
                        |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;float;2IPi$;75|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;convert;2$;76|)) 

(PUT '|DFLOAT;convert;2$;76| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Thing|)
                |DFLOAT;convert;$F;77|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%DoubleFloat| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |DFLOAT;rationalApproximation;$NniF;78|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%DoubleFloat| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;atan;3$;79|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Thing|)
                |DFLOAT;retract;$F;80|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Pair|)
                |DFLOAT;retractIfCan;$U;81|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Integer|)
                |DFLOAT;retract;$I;82|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Pair|)
                |DFLOAT;retractIfCan;$U;83|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Integer|)
                |DFLOAT;sign;$I;84|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%DoubleFloat|)
                |DFLOAT;abs;2$;85|)) 

(PUT '|DFLOAT;abs;2$;85| '|SPADreplace|
     '(XLAM (|x|) (FLOAT-SIGN 1.0 |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Pair|)
                |DFLOAT;manexp|)) 

(DECLAIM (FTYPE (FUNCTION
                    (|%DoubleFloat| (|%IntegerSection| 0)
                        (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |DFLOAT;rationalApproximation;$2NniF;87|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Thing| |%Shell|)
                    |%DoubleFloat|)
                |DFLOAT;**;$F$;88|)) 

(DEFUN |DFLOAT;OMwrite;$S;1| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |DFLOAT;OMwrite;$S;1|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |DFLOAT;OMwrite;$S;1|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 7))
                     (|getShellEntry| $ 10))
                 |DFLOAT;OMwrite;$S;1|)
           (SPADCALL |dev| (|getShellEntry| $ 12))
           (SPADCALL |dev| |x| (|getShellEntry| $ 15))
           (SPADCALL |dev| (|getShellEntry| $ 16))
           (SPADCALL |dev| (|getShellEntry| $ 17))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |DFLOAT;OMwrite;$S;1|)
           (EXIT |s|))))) 

(DEFUN |DFLOAT;OMwrite;$BS;2| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |DFLOAT;OMwrite;$BS;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|)
                 |DFLOAT;OMwrite;$BS;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 7))
                     (|getShellEntry| $ 10))
                 |DFLOAT;OMwrite;$BS;2|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 12))))
           (SPADCALL |dev| |x| (|getShellEntry| $ 15))
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 16))))
           (SPADCALL |dev| (|getShellEntry| $ 17))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|)
                 |DFLOAT;OMwrite;$BS;2|)
           (EXIT |s|))))) 

(DEFUN |DFLOAT;OMwrite;Omd$V;3| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 12))
       (SPADCALL |dev| |x| (|getShellEntry| $ 15))
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 16))))) 

(DEFUN |DFLOAT;OMwrite;Omd$BV;4| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 12))))
       (SPADCALL |dev| |x| (|getShellEntry| $ 15))
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 16))))))) 

(DEFUN |DFLOAT;checkComplex| (|x| $)
  (DECLARE (IGNORE $))
  (C-TO-R |x|)) 

(DEFUN |DFLOAT;base;Pi;6| ($) (DECLARE (IGNORE $)) (FLOAT-RADIX 0.0)) 

(DEFUN |DFLOAT;mantissa;$I;7| (|x| $) (QCAR (|DFLOAT;manexp| |x| $))) 

(DEFUN |DFLOAT;exponent;$I;8| (|x| $) (QCDR (|DFLOAT;manexp| |x| $))) 

(DEFUN |DFLOAT;precision;Pi;9| ($)
  (DECLARE (IGNORE $))
  (FLOAT-DIGITS 0.0)) 

(DEFUN |DFLOAT;bits;Pi;10| ($)
  (COND
    ((EQL (FLOAT-RADIX 0.0) 2) (FLOAT-DIGITS 0.0))
    ((EQL (FLOAT-RADIX 0.0) 16) (* 4 (FLOAT-DIGITS 0.0)))
    ('T
     (LET ((#0=#:G1424
               (FIX (SPADCALL (FLOAT-DIGITS 0.0)
                        (|DFLOAT;log2;2$;40|
                            (FLOAT (FLOAT-RADIX 0.0)
                                   |$DoubleFloatMaximum|)
                            $)
                        (|getShellEntry| $ 34)))))
       (|check-subtype| (AND (>= #0# 0) (> #0# 0)) '(|PositiveInteger|)
           #0#))))) 

(DEFUN |DFLOAT;max;$;11| ($)
  (DECLARE (IGNORE $))
  |$DoubleFloatMaximum|) 

(DEFUN |DFLOAT;min;$;12| ($)
  (DECLARE (IGNORE $))
  |$DoubleFloatMinimum|) 

(DEFUN |DFLOAT;order;$I;13| (|a| $)
  (- (+ (FLOAT-DIGITS 0.0) (|DFLOAT;exponent;$I;8| |a| $)) 1)) 

(DEFUN |DFLOAT;Zero;$;14| ($)
  (DECLARE (IGNORE $))
  (FLOAT 0 |$DoubleFloatMaximum|)) 

(DEFUN |DFLOAT;One;$;15| ($)
  (DECLARE (IGNORE $))
  (FLOAT 1 |$DoubleFloatMaximum|)) 

(DEFUN |DFLOAT;exp1;$;16| ($)
  (/ (FLOAT 534625820200 |$DoubleFloatMaximum|)
     (FLOAT 196677847971 |$DoubleFloatMaximum|))) 

(DEFUN |DFLOAT;pi;$;17| ($) (DECLARE (IGNORE $)) PI) 

(DEFUN |DFLOAT;coerce;$Of;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 48))) 

(DEFUN |DFLOAT;convert;$If;19| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 51))) 

(DEFUN |DFLOAT;<;2$B;20| (|x| |y| $) (DECLARE (IGNORE $)) (< |x| |y|)) 

(DEFUN |DFLOAT;>;2$B;21| (|x| |y| $) (DECLARE (IGNORE $)) (> |x| |y|)) 

(DEFUN |DFLOAT;<=;2$B;22| (|x| |y| $)
  (DECLARE (IGNORE $))
  (NOT (> |x| |y|))) 

(DEFUN |DFLOAT;>=;2$B;23| (|x| |y| $)
  (DECLARE (IGNORE $))
  (>= |x| |y|)) 

(DEFUN |DFLOAT;-;2$;24| (|x| $) (DECLARE (IGNORE $)) (- |x|)) 

(DEFUN |DFLOAT;+;3$;25| (|x| |y| $) (DECLARE (IGNORE $)) (+ |x| |y|)) 

(DEFUN |DFLOAT;-;3$;26| (|x| |y| $) (DECLARE (IGNORE $)) (- |x| |y|)) 

(DEFUN |DFLOAT;*;3$;27| (|x| |y| $) (DECLARE (IGNORE $)) (* |x| |y|)) 

(DEFUN |DFLOAT;*;I2$;28| (|i| |x| $) (DECLARE (IGNORE $)) (* |i| |x|)) 

(DEFUN |DFLOAT;max;3$;29| (|x| |y| $)
  (DECLARE (IGNORE $))
  (MAX |x| |y|)) 

(DEFUN |DFLOAT;min;3$;30| (|x| |y| $)
  (DECLARE (IGNORE $))
  (MIN |x| |y|)) 

(DEFUN |DFLOAT;=;2$B;31| (|x| |y| $) (DECLARE (IGNORE $)) (= |x| |y|)) 

(DEFUN |DFLOAT;/;$I$;32| (|x| |i| $) (DECLARE (IGNORE $)) (/ |x| |i|)) 

(DEFUN |DFLOAT;sqrt;2$;33| (|x| $) (C-TO-R (SQRT |x|))) 

(DEFUN |DFLOAT;log10;2$;34| (|x| $) (C-TO-R (|log| |x|))) 

(DEFUN |DFLOAT;**;$I$;35| (|x| |i| $)
  (DECLARE (IGNORE $))
  (EXPT |x| |i|)) 

(DEFUN |DFLOAT;**;3$;36| (|x| |y| $) (C-TO-R (EXPT |x| |y|))) 

(DEFUN |DFLOAT;coerce;I$;37| (|i| $)
  (DECLARE (IGNORE $))
  (FLOAT |i| |$DoubleFloatMaximum|)) 

(DEFUN |DFLOAT;exp;2$;38| (|x| $) (DECLARE (IGNORE $)) (EXP |x|)) 

(DEFUN |DFLOAT;log;2$;39| (|x| $) (C-TO-R (LN |x|))) 

(DEFUN |DFLOAT;log2;2$;40| (|x| $) (C-TO-R (LOG2 |x|))) 

(DEFUN |DFLOAT;sin;2$;41| (|x| $) (DECLARE (IGNORE $)) (SIN |x|)) 

(DEFUN |DFLOAT;cos;2$;42| (|x| $) (DECLARE (IGNORE $)) (COS |x|)) 

(DEFUN |DFLOAT;tan;2$;43| (|x| $) (DECLARE (IGNORE $)) (TAN |x|)) 

(DEFUN |DFLOAT;cot;2$;44| (|x| $) (DECLARE (IGNORE $)) (COT |x|)) 

(DEFUN |DFLOAT;sec;2$;45| (|x| $) (DECLARE (IGNORE $)) (SEC |x|)) 

(DEFUN |DFLOAT;csc;2$;46| (|x| $) (DECLARE (IGNORE $)) (CSC |x|)) 

(DEFUN |DFLOAT;asin;2$;47| (|x| $) (C-TO-R (ASIN |x|))) 

(DEFUN |DFLOAT;acos;2$;48| (|x| $) (C-TO-R (ACOS |x|))) 

(DEFUN |DFLOAT;atan;2$;49| (|x| $) (DECLARE (IGNORE $)) (ATAN |x|)) 

(DEFUN |DFLOAT;acsc;2$;50| (|x| $) (C-TO-R (ACSC |x|))) 

(DEFUN |DFLOAT;acot;2$;51| (|x| $) (DECLARE (IGNORE $)) (ACOT |x|)) 

(DEFUN |DFLOAT;asec;2$;52| (|x| $) (C-TO-R (ASEC |x|))) 

(DEFUN |DFLOAT;sinh;2$;53| (|x| $) (DECLARE (IGNORE $)) (SINH |x|)) 

(DEFUN |DFLOAT;cosh;2$;54| (|x| $) (DECLARE (IGNORE $)) (COSH |x|)) 

(DEFUN |DFLOAT;tanh;2$;55| (|x| $) (DECLARE (IGNORE $)) (TANH |x|)) 

(DEFUN |DFLOAT;csch;2$;56| (|x| $) (DECLARE (IGNORE $)) (CSCH |x|)) 

(DEFUN |DFLOAT;coth;2$;57| (|x| $) (DECLARE (IGNORE $)) (COTH |x|)) 

(DEFUN |DFLOAT;sech;2$;58| (|x| $) (DECLARE (IGNORE $)) (SECH |x|)) 

(DEFUN |DFLOAT;asinh;2$;59| (|x| $) (DECLARE (IGNORE $)) (ASINH |x|)) 

(DEFUN |DFLOAT;acosh;2$;60| (|x| $) (C-TO-R (ACOSH |x|))) 

(DEFUN |DFLOAT;atanh;2$;61| (|x| $) (C-TO-R (ATANH |x|))) 

(DEFUN |DFLOAT;acsch;2$;62| (|x| $) (DECLARE (IGNORE $)) (ACSCH |x|)) 

(DEFUN |DFLOAT;acoth;2$;63| (|x| $) (C-TO-R (ACOTH |x|))) 

(DEFUN |DFLOAT;asech;2$;64| (|x| $) (C-TO-R (ASECH |x|))) 

(DEFUN |DFLOAT;/;3$;65| (|x| |y| $) (DECLARE (IGNORE $)) (/ |x| |y|)) 

(DEFUN |DFLOAT;negative?;$B;66| (|x| $)
  (DECLARE (IGNORE $))
  (MINUSP |x|)) 

(DEFUN |DFLOAT;zero?;$B;67| (|x| $) (DECLARE (IGNORE $)) (ZEROP |x|)) 

(DEFUN |DFLOAT;one?;$B;68| (|x| $) (DECLARE (IGNORE $)) (= |x| 1.0)) 

(DEFUN |DFLOAT;hash;$Si;69| (|x| $) (DECLARE (IGNORE $)) (HASHEQ |x|)) 

(DEFUN |DFLOAT;recip;$U;70| (|x| $)
  (COND ((ZEROP |x|) (CONS 1 "failed")) ('T (CONS 0 (/ 1.0 |x|))))) 

(DEFUN |DFLOAT;differentiate;2$;71| (|x| $) (DECLARE (IGNORE $)) 0.0) 

(DEFUN |DFLOAT;Gamma;2$;72| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 107))) 

(DEFUN |DFLOAT;Beta;3$;73| (|x| |y| $)
  (SPADCALL |x| |y| (|getShellEntry| $ 109))) 

(DEFUN |DFLOAT;wholePart;$I;74| (|x| $)
  (DECLARE (IGNORE $))
  (FIX |x|)) 

(DEFUN |DFLOAT;float;2IPi$;75| (|ma| |ex| |b| $)
  (* |ma| (EXPT (FLOAT |b| |$DoubleFloatMaximum|) |ex|))) 

(DEFUN |DFLOAT;convert;2$;76| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |DFLOAT;convert;$F;77| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 113))) 

(DEFUN |DFLOAT;rationalApproximation;$NniF;78| (|x| |d| $)
  (|DFLOAT;rationalApproximation;$2NniF;87| |x| |d| 10 $)) 

(DEFUN |DFLOAT;atan;3$;79| (|x| |y| $)
  (PROG (|theta|)
    (RETURN
      (SEQ (COND
             ((= |x| 0.0)
              (COND
                ((> |y| 0.0) (/ PI 2))
                ((< |y| 0.0) (- (/ PI 2)))
                ('T 0.0)))
             ('T
              (SEQ (LETT |theta| (ATAN (FLOAT-SIGN 1.0 (/ |y| |x|)))
                         |DFLOAT;atan;3$;79|)
                   (COND
                     ((< |x| 0.0)
                      (LETT |theta| (- PI |theta|) |DFLOAT;atan;3$;79|)))
                   (COND
                     ((< |y| 0.0)
                      (LETT |theta| (- |theta|) |DFLOAT;atan;3$;79|)))
                   (EXIT |theta|)))))))) 

(DEFUN |DFLOAT;retract;$F;80| (|x| $)
  (|DFLOAT;rationalApproximation;$2NniF;87| |x|
      (LET ((#0=#:G1503 (- (FLOAT-DIGITS 0.0) 1)))
        (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#))
      (FLOAT-RADIX 0.0) $)) 

(DEFUN |DFLOAT;retractIfCan;$U;81| (|x| $)
  (CONS 0
        (|DFLOAT;rationalApproximation;$2NniF;87| |x|
            (LET ((#0=#:G1511 (- (FLOAT-DIGITS 0.0) 1)))
              (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#))
            (FLOAT-RADIX 0.0) $))) 

(DEFUN |DFLOAT;retract;$I;82| (|x| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n| (FIX |x|) |DFLOAT;retract;$I;82|)
           (EXIT (COND
                   ((= |x| (FLOAT |n| |$DoubleFloatMaximum|)) |n|)
                   ('T (|error| "Not an integer")))))))) 

(DEFUN |DFLOAT;retractIfCan;$U;83| (|x| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n| (FIX |x|) |DFLOAT;retractIfCan;$U;83|)
           (EXIT (COND
                   ((= |x| (FLOAT |n| |$DoubleFloatMaximum|))
                    (CONS 0 |n|))
                   ('T (CONS 1 "failed")))))))) 

(DEFUN |DFLOAT;sign;$I;84| (|x| $)
  (|DFLOAT;retract;$I;82| (FLOAT-SIGN |x| 1.0) $)) 

(DEFUN |DFLOAT;abs;2$;85| (|x| $)
  (DECLARE (IGNORE $))
  (FLOAT-SIGN 1.0 |x|)) 

(DEFUN |DFLOAT;manexp| (|x| $)
  (PROG (|s| #0=#:G1528 |me| |two53|)
    (RETURN
      (SEQ (EXIT (COND
                   ((ZEROP |x|) (CONS 0 0))
                   ('T
                    (SEQ (LETT |s| (|DFLOAT;sign;$I;84| |x| $)
                               |DFLOAT;manexp|)
                         (LETT |x| (FLOAT-SIGN 1.0 |x|)
                               |DFLOAT;manexp|)
                         (COND
                           ((> |x| |$DoubleFloatMaximum|)
                            (PROGN
                              (LETT #0#
                                    (CONS
                                     (+
                                      (* |s|
                                       (|DFLOAT;mantissa;$I;7|
                                        |$DoubleFloatMaximum| $))
                                      1)
                                     (|DFLOAT;exponent;$I;8|
                                      |$DoubleFloatMaximum| $))
                                    |DFLOAT;manexp|)
                              (GO #0#))))
                         (LETT |me| (MANEXP |x|) |DFLOAT;manexp|)
                         (LETT |two53|
                               (EXPT (FLOAT-RADIX 0.0)
                                     (FLOAT-DIGITS 0.0))
                               |DFLOAT;manexp|)
                         (EXIT (CONS (* |s|
                                      (FIX (* |two53| (QCAR |me|))))
                                     (- (QCDR |me|) (FLOAT-DIGITS 0.0))))))))
           #0# (EXIT #0#))))) 

(DEFUN |DFLOAT;rationalApproximation;$2NniF;87| (|f| |d| |b| $)
  (PROG (|#G109| |nu| |ex| BASE |de| |tol| |#G110| |q| |r| |p2| |q2|
                 #0=#:G1538 |#G111| |#G112| |p0| |p1| |#G113| |#G114|
                 |q0| |q1| |#G115| |#G116| |s| |t|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |#G109| (|DFLOAT;manexp| |f| $)
                            |DFLOAT;rationalApproximation;$2NniF;87|)
                      (LETT |nu| (QCAR |#G109|)
                            |DFLOAT;rationalApproximation;$2NniF;87|)
                      (LETT |ex| (QCDR |#G109|)
                            |DFLOAT;rationalApproximation;$2NniF;87|)
                      |#G109|
                      (LETT BASE (FLOAT-RADIX 0.0)
                            |DFLOAT;rationalApproximation;$2NniF;87|)
                      (EXIT (COND
                              ((>= |ex| 0)
                               (SPADCALL
                                   (* |nu|
                                    (EXPT BASE
                                     (|check-subtype| (>= |ex| 0)
                                      '(|NonNegativeInteger|) |ex|)))
                                   (|getShellEntry| $ 136)))
                              ('T
                               (SEQ (LETT |de|
                                     (EXPT BASE
                                      (LET ((#1=#:G1539 (- |ex|)))
                                        (|check-subtype| (>= #1# 0)
                                         '(|NonNegativeInteger|) #1#)))
                                     |DFLOAT;rationalApproximation;$2NniF;87|)
                                    (EXIT
                                     (COND
                                       ((< |b| 2)
                                        (|error| "base must be > 1"))
                                       ('T
                                        (SEQ
                                         (LETT |tol| (EXPT |b| |d|)
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (LETT |s| |nu|
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (LETT |t| |de|
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (LETT |p0| 0
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (LETT |p1| 1
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (LETT |q0| 1
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (LETT |q1| 0
                                          |DFLOAT;rationalApproximation;$2NniF;87|)
                                         (EXIT
                                          (SEQ G190 NIL
                                           (SEQ
                                            (LETT |#G110|
                                             (DIVIDE2 |s| |t|)
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |q| (QCAR |#G110|)
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |r| (QCDR |#G110|)
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            |#G110|
                                            (LETT |p2|
                                             (+ (* |q| |p1|) |p0|)
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |q2|
                                             (+ (* |q| |q1|) |q0|)
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (COND
                                              ((OR (EQL |r| 0)
                                                (<
                                                 (SPADCALL |tol|
                                                  (ABS
                                                   (- (* |nu| |q2|)
                                                    (* |de| |p2|)))
                                                  (|getShellEntry| $
                                                   145))
                                                 (* |de| (ABS |p2|))))
                                               (EXIT
                                                (PROGN
                                                  (LETT #0#
                                                   (SPADCALL |p2| |q2|
                                                    (|getShellEntry| $
                                                     143))
                                                   |DFLOAT;rationalApproximation;$2NniF;87|)
                                                  (GO #0#)))))
                                            (LETT |#G111| |p1|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |#G112| |p2|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |p0| |#G111|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |p1| |#G112|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |#G113| |q1|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |#G114| |q2|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |q0| |#G113|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (LETT |q1| |#G114|
                                             |DFLOAT;rationalApproximation;$2NniF;87|)
                                            (EXIT
                                             (PROGN
                                               (LETT |#G115| |t|
                                                |DFLOAT;rationalApproximation;$2NniF;87|)
                                               (LETT |#G116| |r|
                                                |DFLOAT;rationalApproximation;$2NniF;87|)
                                               (LETT |s| |#G115|
                                                |DFLOAT;rationalApproximation;$2NniF;87|)
                                               (LETT |t| |#G116|
                                                |DFLOAT;rationalApproximation;$2NniF;87|))))
                                           NIL (GO G190) G191
                                           (EXIT NIL)))))))))))))
           #0# (EXIT #0#))))) 

(DEFUN |DFLOAT;**;$F$;88| (|x| |r| $)
  (PROG (|n| |d| #0=#:G1549)
    (RETURN
      (SEQ (EXIT (COND
                   ((ZEROP |x|)
                    (COND
                      ((SPADCALL |r| (|getShellEntry| $ 147))
                       (|error| "0**0 is undefined"))
                      ((SPADCALL |r| (|getShellEntry| $ 148))
                       (|error| "division by 0"))
                      ('T 0.0)))
                   ((OR (SPADCALL |r| (|getShellEntry| $ 147))
                        (= |x| 1.0))
                    1.0)
                   ('T
                    (COND
                      ((SPADCALL |r| (|getShellEntry| $ 149)) |x|)
                      ('T
                       (SEQ (LETT |n|
                                  (SPADCALL |r|
                                      (|getShellEntry| $ 150))
                                  |DFLOAT;**;$F$;88|)
                            (LETT |d|
                                  (SPADCALL |r|
                                      (|getShellEntry| $ 151))
                                  |DFLOAT;**;$F$;88|)
                            (EXIT (COND
                                    ((MINUSP |x|)
                                     (COND
                                       ((ODDP |d|)
                                        (COND
                                          ((ODDP |n|)
                                           (PROGN
                                             (LETT #0#
                                              (-
                                               (|DFLOAT;**;$F$;88|
                                                (- |x|) |r| $))
                                              |DFLOAT;**;$F$;88|)
                                             (GO #0#)))
                                          ('T
                                           (PROGN
                                             (LETT #0#
                                              (|DFLOAT;**;$F$;88|
                                               (- |x|) |r| $)
                                              |DFLOAT;**;$F$;88|)
                                             (GO #0#)))))
                                       ('T (|error| "negative root"))))
                                    ((EQL |d| 2)
                                     (EXPT (|DFLOAT;sqrt;2$;33| |x| $)
                                      |n|))
                                    ('T
                                     (|DFLOAT;**;3$;36| |x|
                                      (/
                                       (FLOAT |n|
                                        |$DoubleFloatMaximum|)
                                       (FLOAT |d|
                                        |$DoubleFloatMaximum|))
                                      $))))))))))
           #0# (EXIT #0#))))) 

(DEFUN |DoubleFloat| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1561)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|DoubleFloat|)
                   |DoubleFloat|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|DoubleFloat|
                                   (LIST
                                    (CONS NIL
                                     (CONS 1 (|DoubleFloat;|))))))
                 (LETT #0# T |DoubleFloat|))
               (COND
                 ((NOT #0#) (HREM |$ConstructorCache| '|DoubleFloat|))))))))))) 

(DEFUN |DoubleFloat;| ()
  (LET ((|dv$| (LIST '|DoubleFloat|)) ($ (|newShell| 165))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|DoubleFloat| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    $)) 

(MAKEPROP '|DoubleFloat| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|OpenMathEncoding|)
             (0 . |OMencodingXML|) (|String|) (|OpenMathDevice|)
             (4 . |OMopenString|) (|Void|) (10 . |OMputObject|)
             (|DoubleFloat|) |DFLOAT;convert;2$;76| (15 . |OMputFloat|)
             (21 . |OMputEndObject|) (26 . |OMclose|)
             |DFLOAT;OMwrite;$S;1| (|Boolean|) |DFLOAT;OMwrite;$BS;2|
             |DFLOAT;OMwrite;Omd$V;3| |DFLOAT;OMwrite;Omd$BV;4|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |DFLOAT;Zero;$;14|) $))
             (|PositiveInteger|) |DFLOAT;base;Pi;6| (|Integer|)
             |DFLOAT;mantissa;$I;7| |DFLOAT;exponent;$I;8|
             |DFLOAT;precision;Pi;9| (31 . =) (37 . *) (43 . |coerce|)
             |DFLOAT;log2;2$;40| (48 . *) |DFLOAT;wholePart;$I;74|
             |DFLOAT;bits;Pi;10| |DFLOAT;max;$;11| |DFLOAT;min;$;12|
             (54 . +) (60 . |One|) (64 . -) |DFLOAT;order;$I;13|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |DFLOAT;One;$;15|) $))
             |DFLOAT;/;3$;65| |DFLOAT;exp1;$;16| |DFLOAT;pi;$;17|
             (|OutputForm|) (70 . |outputForm|) |DFLOAT;coerce;$Of;18|
             (|InputForm|) (75 . |convert|) |DFLOAT;convert;$If;19|
             |DFLOAT;<;2$B;20| |DFLOAT;>;2$B;21| (80 . |not|)
             |DFLOAT;<=;2$B;22| |DFLOAT;>=;2$B;23| |DFLOAT;-;2$;24|
             |DFLOAT;+;3$;25| |DFLOAT;-;3$;26| |DFLOAT;*;3$;27|
             |DFLOAT;*;I2$;28| |DFLOAT;max;3$;29| |DFLOAT;min;3$;30|
             |DFLOAT;=;2$B;31| |DFLOAT;/;$I$;32| |DFLOAT;sqrt;2$;33|
             |DFLOAT;log10;2$;34| |DFLOAT;**;$I$;35| |DFLOAT;**;3$;36|
             |DFLOAT;coerce;I$;37| |DFLOAT;exp;2$;38|
             |DFLOAT;log;2$;39| |DFLOAT;sin;2$;41| |DFLOAT;cos;2$;42|
             |DFLOAT;tan;2$;43| |DFLOAT;cot;2$;44| |DFLOAT;sec;2$;45|
             |DFLOAT;csc;2$;46| |DFLOAT;asin;2$;47| |DFLOAT;acos;2$;48|
             |DFLOAT;atan;2$;49| |DFLOAT;acsc;2$;50|
             |DFLOAT;acot;2$;51| |DFLOAT;asec;2$;52|
             |DFLOAT;sinh;2$;53| |DFLOAT;cosh;2$;54|
             |DFLOAT;tanh;2$;55| |DFLOAT;csch;2$;56|
             |DFLOAT;coth;2$;57| |DFLOAT;sech;2$;58|
             |DFLOAT;asinh;2$;59| |DFLOAT;acosh;2$;60|
             |DFLOAT;atanh;2$;61| |DFLOAT;acsch;2$;62|
             |DFLOAT;acoth;2$;63| |DFLOAT;asech;2$;64|
             |DFLOAT;negative?;$B;66| |DFLOAT;zero?;$B;67|
             |DFLOAT;one?;$B;68| (|SingleInteger|) |DFLOAT;hash;$Si;69|
             (|Union| $ '"failed") |DFLOAT;recip;$U;70|
             |DFLOAT;differentiate;2$;71|
             (|DoubleFloatSpecialFunctions|) (85 . |Gamma|)
             |DFLOAT;Gamma;2$;72| (90 . |Beta|) |DFLOAT;Beta;3$;73|
             |DFLOAT;float;2IPi$;75| (|Float|) (96 . |convert|)
             |DFLOAT;convert;$F;77| (|Fraction| 26)
             (|NonNegativeInteger|)
             |DFLOAT;rationalApproximation;$2NniF;87|
             |DFLOAT;rationalApproximation;$NniF;78| (101 . |Zero|)
             |DFLOAT;abs;2$;85| |DFLOAT;atan;3$;79| (105 . |One|)
             |DFLOAT;retract;$F;80| (|Union| 115 '"failed")
             |DFLOAT;retractIfCan;$U;81| |DFLOAT;retract;$I;82|
             (|Union| 26 '"failed") |DFLOAT;retractIfCan;$U;83|
             |DFLOAT;sign;$I;84| (109 . *) (115 . **) (121 . |Zero|)
             (125 . |Zero|) (129 . >=) (135 . **) (141 . |coerce|)
             (146 . -) (151 . <) (157 . **)
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (163 . |divide|) (169 . =) (175 . /) (181 . |abs|)
             (186 . *) (192 . <) (198 . |zero?|) (203 . |negative?|)
             (208 . |one?|) (213 . |numer|) (218 . |denom|)
             (223 . |odd?|) |DFLOAT;**;$F$;88|
             (|PatternMatchResult| 112 $) (|Pattern| 112)
             (|Factored| $) (|List| $) (|Union| 157 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 160 '"failed")
             (|Record| (|:| |coef| 157) (|:| |generator| $))
             (|SparseUnivariatePolynomial| $)
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $)))
          '#(~= 228 |zero?| 234 |wholePart| 239 |unitNormal| 244
             |unitCanonical| 249 |unit?| 254 |truncate| 259 |tanh| 264
             |tan| 269 |subtractIfCan| 274 |squareFreePart| 280
             |squareFree| 285 |sqrt| 290 |sizeLess?| 295 |sinh| 301
             |sin| 306 |sign| 311 |sech| 316 |sec| 321 |sample| 326
             |round| 330 |retractIfCan| 335 |retract| 345 |rem| 355
             |recip| 361 |rationalApproximation| 366 |quo| 379
             |principalIdeal| 385 |prime?| 390 |precision| 395
             |positive?| 399 |pi| 404 |patternMatch| 408 |order| 415
             |one?| 420 |nthRoot| 425 |norm| 431 |negative?| 436
             |multiEuclidean| 441 |min| 447 |max| 457 |mantissa| 467
             |log2| 472 |log10| 477 |log| 482 |lcm| 487 |latex| 498
             |inv| 503 |hash| 508 |gcdPolynomial| 513 |gcd| 519
             |fractionPart| 530 |floor| 535 |float| 540 |factor| 553
             |extendedEuclidean| 558 |exquo| 571 |expressIdealMember|
             577 |exponent| 583 |exp1| 588 |exp| 592 |euclideanSize|
             597 |divide| 602 |digits| 608 |differentiate| 612 |csch|
             623 |csc| 628 |coth| 633 |cot| 638 |cosh| 643 |cos| 648
             |convert| 653 |coerce| 673 |characteristic| 703 |ceiling|
             707 |bits| 712 |before?| 716 |base| 722 |atanh| 726 |atan|
             731 |associates?| 742 |asinh| 748 |asin| 753 |asech| 758
             |asec| 763 |acsch| 768 |acsc| 773 |acoth| 778 |acot| 783
             |acosh| 788 |acos| 793 |abs| 798 |Zero| 803 |One| 807
             |OMwrite| 811 |Gamma| 835 D 840 |Beta| 851 >= 857 > 863 =
             869 <= 875 < 881 / 887 - 899 + 910 ** 916 * 946)
          '((|approximate| . 0) (|canonicalsClosed| . 0)
            (|canonicalUnitNormal| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(|FloatingPointSystem&| |RealNumberSystem&|
                         |Field&| |EuclideanDomain&| NIL
                         |UniqueFactorizationDomain&| |GcdDomain&|
                         |DivisionRing&| |IntegralDomain&| |Algebra&|
                         |Algebra&| |DifferentialRing&| NIL
                         |OrderedRing&| |Module&| NIL NIL |Module&| NIL
                         NIL |Ring&| NIL NIL NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL NIL NIL
                         |AbelianMonoid&| |Monoid&| NIL |OrderedSet&|
                         NIL NIL NIL NIL |AbelianSemiGroup&|
                         |SemiGroup&| NIL
                         |TranscendentalFunctionCategory&|
                         |RetractableTo&| |RetractableTo&| NIL
                         |SetCategory&| NIL
                         |ElementaryFunctionCategory&| NIL
                         |HyperbolicFunctionCategory&|
                         |ArcTrigonometricFunctionCategory&|
                         |TrigonometricFunctionCategory&| NIL NIL
                         |RadicalCategory&| NIL NIL NIL NIL NIL
                         |BasicType&| NIL)
                      (CONS '#((|FloatingPointSystem|)
                               (|RealNumberSystem|) (|Field|)
                               (|EuclideanDomain|)
                               (|PrincipalIdealDomain|)
                               (|UniqueFactorizationDomain|)
                               (|GcdDomain|) (|DivisionRing|)
                               (|IntegralDomain|) (|Algebra| 115)
                               (|Algebra| $$) (|DifferentialRing|)
                               (|CharacteristicZero|) (|OrderedRing|)
                               (|Module| 115) (|EntireRing|)
                               (|CommutativeRing|) (|Module| $$)
                               (|BiModule| 115 115) (|BiModule| $$ $$)
                               (|Ring|) (|OrderedAbelianGroup|)
                               (|RightModule| 115) (|LeftModule| 115)
                               (|LeftModule| $$) (|Rng|)
                               (|RightModule| $$)
                               (|OrderedCancellationAbelianMonoid|)
                               (|AbelianGroup|)
                               (|OrderedAbelianMonoid|)
                               (|CancellationAbelianMonoid|)
                               (|OrderedAbelianSemiGroup|)
                               (|LinearSet| 115) (|LinearSet| $$)
                               (|AbelianMonoid|) (|Monoid|)
                               (|PatternMatchable| 112) (|OrderedSet|)
                               (|LeftLinearSet| 115)
                               (|RightLinearSet| 115)
                               (|LeftLinearSet| $$)
                               (|RightLinearSet| $$)
                               (|AbelianSemiGroup|) (|SemiGroup|)
                               (|LeftLinearSet| 26)
                               (|TranscendentalFunctionCategory|)
                               (|RetractableTo| 115)
                               (|RetractableTo| 26) (|RealConstant|)
                               (|SetCategory|) (|ConvertibleTo| 50)
                               (|ElementaryFunctionCategory|)
                               (|ArcHyperbolicFunctionCategory|)
                               (|HyperbolicFunctionCategory|)
                               (|ArcTrigonometricFunctionCategory|)
                               (|TrigonometricFunctionCategory|)
                               (|OpenMath|) (|ConvertibleTo| 155)
                               (|RadicalCategory|)
                               (|ConvertibleTo| 112)
                               (|ConvertibleTo| 13)
                               (|CoercibleFrom| 115)
                               (|CoercibleFrom| $$)
                               (|CoercibleFrom| 26) (|BasicType|)
                               (|CoercibleTo| 47))
                            (|makeByteWordVec2| 164
                                '(0 6 0 7 2 9 0 8 6 10 1 9 11 0 12 2 9
                                  11 0 13 15 1 9 11 0 16 1 9 11 0 17 2
                                  24 19 0 0 30 2 24 0 24 0 31 1 0 0 26
                                  32 2 0 0 24 0 34 2 26 0 0 0 39 0 26 0
                                  40 2 26 0 0 0 41 1 47 0 13 48 1 50 0
                                  13 51 1 19 0 0 55 1 106 13 13 107 2
                                  106 13 13 13 109 1 112 0 13 113 0 26
                                  0 119 0 24 0 122 2 26 0 26 0 130 2 26
                                  0 0 116 131 0 115 0 132 0 116 0 133 2
                                  26 19 0 0 134 2 24 0 0 116 135 1 115
                                  0 26 136 1 26 0 0 137 2 116 19 0 0
                                  138 2 116 0 0 116 139 2 26 140 0 0
                                  141 2 26 19 0 0 142 2 115 0 26 26 143
                                  1 26 0 0 144 2 26 0 116 0 145 2 26 19
                                  0 0 146 1 115 19 0 147 1 115 19 0 148
                                  1 115 19 0 149 1 115 26 0 150 1 115
                                  26 0 151 1 26 19 0 152 2 0 19 0 0 1 1
                                  0 19 0 99 1 0 26 0 35 1 0 164 0 1 1 0
                                  0 0 1 1 0 19 0 1 1 0 0 0 1 1 0 0 0 88
                                  1 0 0 0 76 2 0 103 0 0 1 1 0 0 0 1 1
                                  0 156 0 1 1 0 0 0 67 2 0 19 0 0 1 1 0
                                  0 0 86 1 0 0 0 74 1 0 26 0 129 1 0 0
                                  0 91 1 0 0 0 78 0 0 0 1 1 0 0 0 1 1 0
                                  124 0 125 1 0 127 0 128 1 0 115 0 123
                                  1 0 26 0 126 2 0 0 0 0 1 1 0 103 0
                                  104 2 0 115 0 116 118 3 0 115 0 116
                                  116 117 2 0 0 0 0 1 1 0 162 157 1 1 0
                                  19 0 1 0 0 24 29 1 0 19 0 1 0 0 0 46
                                  3 0 154 0 155 154 1 1 0 26 0 42 1 0
                                  19 0 100 2 0 0 0 26 1 1 0 0 0 1 1 0
                                  19 0 98 2 0 158 157 0 1 0 0 0 38 2 0
                                  0 0 0 64 0 0 0 37 2 0 0 0 0 63 1 0 26
                                  0 27 1 0 0 0 33 1 0 0 0 68 1 0 0 0 73
                                  1 0 0 157 1 2 0 0 0 0 1 1 0 8 0 1 1 0
                                  0 0 1 1 0 101 0 102 2 0 163 163 163 1
                                  1 0 0 157 1 2 0 0 0 0 1 1 0 0 0 1 1 0
                                  0 0 1 3 0 0 26 26 24 111 2 0 0 26 26
                                  1 1 0 156 0 1 2 0 159 0 0 1 3 0 161 0
                                  0 0 1 2 0 103 0 0 1 2 0 158 157 0 1 1
                                  0 26 0 28 0 0 0 45 1 0 0 0 72 1 0 116
                                  0 1 2 0 140 0 0 1 0 0 24 1 1 0 0 0
                                  105 2 0 0 0 116 1 1 0 0 0 89 1 0 0 0
                                  79 1 0 0 0 90 1 0 0 0 77 1 0 0 0 87 1
                                  0 0 0 75 1 0 50 0 52 1 0 155 0 1 1 0
                                  112 0 114 1 0 13 0 14 1 0 0 115 1 1 0
                                  0 26 71 1 0 0 115 1 1 0 0 0 1 1 0 0
                                  26 71 1 0 47 0 49 0 0 116 1 1 0 0 0 1
                                  0 0 24 36 2 0 19 0 0 1 0 0 24 25 1 0
                                  0 0 94 2 0 0 0 0 121 1 0 0 0 82 2 0
                                  19 0 0 1 1 0 0 0 92 1 0 0 0 80 1 0 0
                                  0 97 1 0 0 0 85 1 0 0 0 95 1 0 0 0 83
                                  1 0 0 0 96 1 0 0 0 84 1 0 0 0 93 1 0
                                  0 0 81 1 0 0 0 120 0 0 0 23 0 0 0 43
                                  2 0 11 9 0 21 3 0 11 9 0 19 22 1 0 8
                                  0 18 2 0 8 0 19 20 1 0 0 0 108 1 0 0
                                  0 1 2 0 0 0 116 1 2 0 0 0 0 110 2 0
                                  19 0 0 57 2 0 19 0 0 54 2 0 19 0 0 65
                                  2 0 19 0 0 56 2 0 19 0 0 53 2 0 0 0
                                  26 66 2 0 0 0 0 44 2 0 0 0 0 60 1 0 0
                                  0 58 2 0 0 0 0 59 2 0 0 0 0 70 2 0 0
                                  0 115 153 2 0 0 0 26 69 2 0 0 0 116 1
                                  2 0 0 0 24 1 2 0 0 115 0 1 2 0 0 0
                                  115 1 2 0 0 0 0 61 2 0 0 26 0 62 2 0
                                  0 116 0 1 2 0 0 24 0 34)))))
          '|lookupComplete|)) 

(MAKEPROP '|DoubleFloat| 'NILADIC T) 