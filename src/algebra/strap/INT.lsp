
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Void|)
                |INT;writeOMInt|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%String|)
                |INT;OMwrite;$S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Boolean| |%Shell|) |%String|)
                |INT;OMwrite;$BS;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Void|)
                |INT;OMwrite;Omd$V;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Boolean| |%Shell|)
                    |%Void|)
                |INT;OMwrite;Omd$BV;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;zero?;$B;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;one?;$B;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |INT;Zero;$;8|)) 

(PUT '|INT;Zero;$;8| '|SPADreplace| '(XLAM NIL 0)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |INT;One;$;9|)) 

(PUT '|INT;One;$;9| '|SPADreplace| '(XLAM NIL 1)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |INT;base;$;10|)) 

(PUT '|INT;base;$;10| '|SPADreplace| '(XLAM NIL 2)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;copy;2$;11|)) 

(PUT '|INT;copy;2$;11| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;inc;2$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;dec;2$;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |INT;hash;$Si;14|)) 

(PUT '|INT;hash;$Si;14| '|SPADreplace| '|%hash|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;negative?;$B;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |INT;coerce;$Of;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;coerce;2$;17|)) 

(PUT '|INT;coerce;2$;17| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;convert;2$;18|)) 

(PUT '|INT;convert;2$;18| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;length;2$;19|)) 

(PUT '|INT;length;2$;19| '|SPADreplace| '|%ilength|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Integer| |%Shell|)
                    |%Integer|)
                |INT;addmod;4$;20|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Integer| |%Shell|)
                    |%Integer|)
                |INT;submod;4$;21|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Integer| |%Shell|)
                    |%Integer|)
                |INT;mulmod;4$;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |INT;convert;$F;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%DoubleFloat|)
                |INT;convert;$Df;24|)) 

(PUT '|INT;convert;$Df;24| '|SPADreplace| '|%i2f|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |INT;convert;$If;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%String|)
                |INT;convert;$S;26|)) 

(PUT '|INT;convert;$S;26| '|SPADreplace| '|%i2s|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%String|)
                |INT;latex;$S;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;positiveRemainder;3$;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;reducedSystem;2M;29|)) 

(PUT '|INT;reducedSystem;2M;29| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |INT;reducedSystem;MVR;30|)) 

(PUT '|INT;reducedSystem;MVR;30| '|SPADreplace|
     '(XLAM (|m| |v|) (|%pair| |m| '|vec|))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;abs;2$;31|)) 

(PUT '|INT;abs;2$;31| '|SPADreplace| '|%iabs|) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |INT;random;$;32|)) 

(PUT '|INT;random;$;32| '|SPADreplace| '|random|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;random;2$;33|)) 

(PUT '|INT;random;2$;33| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;=;2$B;34|)) 

(PUT '|INT;=;2$B;34| '|SPADreplace| '|%ieq|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;<;2$B;35|)) 

(PUT '|INT;<;2$B;35| '|SPADreplace| '|%ilt|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;>;2$B;36|)) 

(PUT '|INT;>;2$B;36| '|SPADreplace| '(XLAM (|x| |y|) (|%ilt| |y| |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;<=;2$B;37|)) 

(PUT '|INT;<=;2$B;37| '|SPADreplace|
     '(XLAM (|x| |y|) (|%not| (|%ilt| |y| |x|)))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;>=;2$B;38|)) 

(PUT '|INT;>=;2$B;38| '|SPADreplace|
     '(XLAM (|x| |y|) (|%not| (|%ilt| |x| |y|)))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;-;2$;39|)) 

(PUT '|INT;-;2$;39| '|SPADreplace| '|%ineg|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;+;3$;40|)) 

(PUT '|INT;+;3$;40| '|SPADreplace| '|%iadd|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;-;3$;41|)) 

(PUT '|INT;-;3$;41| '|SPADreplace| '|%isub|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;*;3$;42|)) 

(PUT '|INT;*;3$;42| '|SPADreplace| '|%imul|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;*;3$;43|)) 

(PUT '|INT;*;3$;43| '|SPADreplace| '|%imul|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| (|%IntegerSection| 0) |%Shell|)
                    |%Integer|)
                |INT;**;$Nni$;44|)) 

(PUT '|INT;**;$Nni$;44| '|SPADreplace| '|%ipow|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;odd?;$B;45|)) 

(PUT '|INT;odd?;$B;45| '|SPADreplace| '|%iodd?|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;max;3$;46|)) 

(PUT '|INT;max;3$;46| '|SPADreplace| '|%imax|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;min;3$;47|)) 

(PUT '|INT;min;3$;47| '|SPADreplace| '|%imin|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Pair|)
                |INT;divide;2$R;48|)) 

(PUT '|INT;divide;2$R;48| '|SPADreplace| '|%idivide|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;quo;3$;49|)) 

(PUT '|INT;quo;3$;49| '|SPADreplace| '|%iquo|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;rem;3$;50|)) 

(PUT '|INT;rem;3$;50| '|SPADreplace| '|%irem|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;shift;3$;51|)) 

(PUT '|INT;shift;3$;51| '|SPADreplace| 'ASH) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Pair|)
                |INT;recip;$U;52|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;gcd;3$;53|)) 

(PUT '|INT;gcd;3$;53| '|SPADreplace| '|%igcd|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Shell|)
                |INT;unitNormal;$R;54|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;unitCanonical;2$;55|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |INT;solveLinearPolynomialEquation|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;squareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;factorPolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;factorSquareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |INT;gcdPolynomial;3Sup;60|)) 

(PUT '|INT;zero?;$B;6| '|SPADreplace| '(XLAM (|x|) (|%ieq| |x| 0))) 

(PUT '|INT;one?;$B;7| '|SPADreplace| '(XLAM (|x|) (|%ieq| |x| 1))) 

(PUT '|INT;inc;2$;12| '|SPADreplace| '(XLAM (|x|) (|%iadd| |x| 1))) 

(PUT '|INT;dec;2$;13| '|SPADreplace| '(XLAM (|x|) (|%isub| |x| 1))) 

(PUT '|INT;negative?;$B;15| '|SPADreplace|
     '(XLAM (|x|) (|%ilt| |x| 0))) 

(PUT '|INT;mulmod;4$;22| '|SPADreplace|
     '(XLAM (|a| |b| |p|) (|%irem| (|%imul| |a| |b|) |p|))) 

(PUT '|INT;unitCanonical;2$;55| '|SPADreplace| '|%iabs|) 

(DEFUN |INT;writeOMInt| (|dev| |x| $)
  (COND
    ((MINUSP |x|)
     (SEQ (SPADCALL |dev| (|shellEntry| $ 10))
          (SPADCALL |dev| "arith1" "unary_minus" (|shellEntry| $ 12))
          (SPADCALL |dev| (- |x|) (|shellEntry| $ 15))
          (EXIT (SPADCALL |dev| (|shellEntry| $ 16)))))
    (T (SPADCALL |dev| |x| (|shellEntry| $ 15))))) 

(DEFUN |INT;OMwrite;$S;2| (|x| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|shellEntry| $ 18))
                    (|shellEntry| $ 19))))
    (SEQ (SPADCALL |dev| (|shellEntry| $ 20))
         (|INT;writeOMInt| |dev| |x| $)
         (SPADCALL |dev| (|shellEntry| $ 21))
         (SPADCALL |dev| (|shellEntry| $ 22))
         (EXIT (OM-STRINGPTRTOSTRING |sp|))))) 

(DEFUN |INT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|shellEntry| $ 18))
                    (|shellEntry| $ 19))))
    (SEQ (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 20))))
         (|INT;writeOMInt| |dev| |x| $)
         (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 21))))
         (SPADCALL |dev| (|shellEntry| $ 22))
         (EXIT (OM-STRINGPTRTOSTRING |sp|))))) 

(DEFUN |INT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|shellEntry| $ 20))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|shellEntry| $ 21))))) 

(DEFUN |INT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 20))))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (COND (|wholeObj| (SPADCALL |dev| (|shellEntry| $ 21))))))) 

(DEFUN |INT;zero?;$B;6| (|x| $) (DECLARE (IGNORE $)) (ZEROP |x|)) 

(DEFUN |INT;one?;$B;7| (|x| $) (DECLARE (IGNORE $)) (EQL |x| 1)) 

(DEFUN |INT;Zero;$;8| ($) (DECLARE (IGNORE $)) 0) 

(DEFUN |INT;One;$;9| ($) (DECLARE (IGNORE $)) 1) 

(DEFUN |INT;base;$;10| ($) (DECLARE (IGNORE $)) 2) 

(DEFUN |INT;copy;2$;11| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INT;inc;2$;12| (|x| $) (DECLARE (IGNORE $)) (+ |x| 1)) 

(DEFUN |INT;dec;2$;13| (|x| $) (DECLARE (IGNORE $)) (- |x| 1)) 

(DEFUN |INT;hash;$Si;14| (|x| $) (DECLARE (IGNORE $)) (SXHASH |x|)) 

(DEFUN |INT;negative?;$B;15| (|x| $)
  (DECLARE (IGNORE $))
  (MINUSP |x|)) 

(DEFUN |INT;coerce;$Of;16| (|x| $) (SPADCALL |x| (|shellEntry| $ 42))) 

(DEFUN |INT;coerce;2$;17| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |INT;convert;2$;18| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INT;length;2$;19| (|a| $)
  (DECLARE (IGNORE $))
  (INTEGER-LENGTH |a|)) 

(DEFUN |INT;addmod;4$;20| (|a| |b| |p| $)
  (LET ((|c| (+ |a| |b|)))
    (COND ((NOT (< |c| |p|)) (- |c| |p|)) (T |c|)))) 

(DEFUN |INT;submod;4$;21| (|a| |b| |p| $)
  (LET ((|c| (- |a| |b|))) (COND ((MINUSP |c|) (+ |c| |p|)) (T |c|)))) 

(DEFUN |INT;mulmod;4$;22| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (REM (* |a| |b|) |p|)) 

(DEFUN |INT;convert;$F;23| (|x| $) (SPADCALL |x| (|shellEntry| $ 53))) 

(DEFUN |INT;convert;$Df;24| (|x| $)
  (DECLARE (IGNORE $))
  (FLOAT |x| |$DoubleFloatMaximum|)) 

(DEFUN |INT;convert;$If;25| (|x| $)
  (SPADCALL |x| (|shellEntry| $ 59))) 

(DEFUN |INT;convert;$S;26| (|x| $)
  (DECLARE (IGNORE $))
  (WRITE-TO-STRING |x|)) 

(DEFUN |INT;latex;$S;27| (|x| $)
  (LET ((|s| (WRITE-TO-STRING |x|)))
    (SEQ (COND ((AND (< -1 |x|) (< |x| 10)) (EXIT |s|)))
         (EXIT (STRCONC "{" (STRCONC |s| "}")))))) 

(DEFUN |INT;positiveRemainder;3$;28| (|a| |b| $)
  (PROG (|r|)
    (RETURN
      (COND
        ((|INT;negative?;$B;15|
             (LETT |r| (REM |a| |b|) |INT;positiveRemainder;3$;28|) $)
         (COND ((MINUSP |b|) (- |r| |b|)) (T (+ |r| |b|))))
        (T |r|))))) 

(DEFUN |INT;reducedSystem;2M;29| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |INT;reducedSystem;MVR;30| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| '|vec|)) 

(DEFUN |INT;abs;2$;31| (|x| $) (DECLARE (IGNORE $)) (ABS |x|)) 

(DEFUN |INT;random;$;32| ($) (DECLARE (IGNORE $)) (|random|)) 

(DEFUN |INT;random;2$;33| (|x| $) (DECLARE (IGNORE $)) (RANDOM |x|)) 

(DEFUN |INT;=;2$B;34| (|x| |y| $) (DECLARE (IGNORE $)) (EQL |x| |y|)) 

(DEFUN |INT;<;2$B;35| (|x| |y| $) (DECLARE (IGNORE $)) (< |x| |y|)) 

(DEFUN |INT;>;2$B;36| (|x| |y| $) (DECLARE (IGNORE $)) (< |y| |x|)) 

(DEFUN |INT;<=;2$B;37| (|x| |y| $)
  (DECLARE (IGNORE $))
  (NOT (< |y| |x|))) 

(DEFUN |INT;>=;2$B;38| (|x| |y| $)
  (DECLARE (IGNORE $))
  (NOT (< |x| |y|))) 

(DEFUN |INT;-;2$;39| (|x| $) (DECLARE (IGNORE $)) (- |x|)) 

(DEFUN |INT;+;3$;40| (|x| |y| $) (DECLARE (IGNORE $)) (+ |x| |y|)) 

(DEFUN |INT;-;3$;41| (|x| |y| $) (DECLARE (IGNORE $)) (- |x| |y|)) 

(DEFUN |INT;*;3$;42| (|x| |y| $) (DECLARE (IGNORE $)) (* |x| |y|)) 

(DEFUN |INT;*;3$;43| (|m| |y| $) (DECLARE (IGNORE $)) (* |m| |y|)) 

(DEFUN |INT;**;$Nni$;44| (|x| |n| $)
  (DECLARE (IGNORE $))
  (EXPT |x| |n|)) 

(DEFUN |INT;odd?;$B;45| (|x| $) (DECLARE (IGNORE $)) (ODDP |x|)) 

(DEFUN |INT;max;3$;46| (|x| |y| $) (DECLARE (IGNORE $)) (MAX |x| |y|)) 

(DEFUN |INT;min;3$;47| (|x| |y| $) (DECLARE (IGNORE $)) (MIN |x| |y|)) 

(DEFUN |INT;divide;2$R;48| (|x| |y| $)
  (DECLARE (IGNORE $))
  (MULTIPLE-VALUE-CALL #'CONS (TRUNCATE |x| |y|))) 

(DEFUN |INT;quo;3$;49| (|x| |y| $)
  (DECLARE (IGNORE $))
  (TRUNCATE |x| |y|)) 

(DEFUN |INT;rem;3$;50| (|x| |y| $) (DECLARE (IGNORE $)) (REM |x| |y|)) 

(DEFUN |INT;shift;3$;51| (|x| |y| $)
  (DECLARE (IGNORE $))
  (ASH |x| |y|)) 

(DEFUN |INT;recip;$U;52| (|x| $)
  (COND
    ((OR (EQL |x| 1) (EQL |x| -1)) (CONS 0 |x|))
    (T (CONS 1 "failed")))) 

(DEFUN |INT;gcd;3$;53| (|x| |y| $) (DECLARE (IGNORE $)) (GCD |x| |y|)) 

(DEFUN |INT;unitNormal;$R;54| (|x| $)
  (COND ((MINUSP |x|) (VECTOR -1 (- |x|) -1)) (T (VECTOR 1 |x| 1)))) 

(DEFUN |INT;unitCanonical;2$;55| (|x| $)
  (DECLARE (IGNORE $))
  (ABS |x|)) 

(DEFUN |INT;solveLinearPolynomialEquation| (|lp| |p| $)
  (SPADCALL |lp| |p| (|shellEntry| $ 100))) 

(DEFUN |INT;squareFreePolynomial| (|p| $)
  (SPADCALL |p| (|shellEntry| $ 104))) 

(DEFUN |INT;factorPolynomial| (|p| $)
  (LET ((|pp| (SPADCALL |p| (|shellEntry| $ 105))))
    (COND
      ((EQL (SPADCALL |pp| (|shellEntry| $ 106))
            (SPADCALL |p| (|shellEntry| $ 106)))
       (SPADCALL |p| (|shellEntry| $ 108)))
      (T (SPADCALL (SPADCALL |pp| (|shellEntry| $ 108))
             (SPADCALL (CONS #'|INT;factorPolynomial!0| $)
                 (SPADCALL
                     (LET ((#0=#:G1477
                               (SPADCALL
                                   (SPADCALL |p| (|shellEntry| $ 106))
                                   (SPADCALL |pp| (|shellEntry| $ 106))
                                   (|shellEntry| $ 110))))
                       (|check-union| (ZEROP (CAR #0#)) $ #0#)
                       (CDR #0#))
                     (|shellEntry| $ 112))
                 (|shellEntry| $ 116))
             (|shellEntry| $ 118)))))) 

(DEFUN |INT;factorPolynomial!0| (|#1| $)
  (SPADCALL |#1| (|shellEntry| $ 109))) 

(DEFUN |INT;factorSquareFreePolynomial| (|p| $)
  (SPADCALL |p| (|shellEntry| $ 119))) 

(DEFUN |INT;gcdPolynomial;3Sup;60| (|p| |q| $)
  (COND
    ((SPADCALL |p| (|shellEntry| $ 120))
     (SPADCALL |q| (|shellEntry| $ 121)))
    ((SPADCALL |q| (|shellEntry| $ 120))
     (SPADCALL |p| (|shellEntry| $ 121)))
    (T (SPADCALL (LIST |p| |q|) (|shellEntry| $ 124))))) 

(DEFUN |Integer| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((#0=#:G1509 (HGET |$ConstructorCache| '|Integer|)))
    (COND
      (#0# (|CDRwithIncrement| (CDAR #0#)))
      (T (UNWIND-PROTECT
           (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Integer|
                               (LIST (CONS NIL (CONS 1 (|Integer;|))))))
             (SETQ #0# T))
           (COND ((NOT #0#) (HREM |$ConstructorCache| '|Integer|)))))))) 

(DEFUN |Integer;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| '(|Integer|)) ($ (|newShell| 139))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|haddProp| |$ConstructorCache| '|Integer| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 81)
          (SETF (|shellEntry| $ 80)
                (CONS (|dispatchFunction| |INT;*;3$;43|) $)))
    $)) 

(MAKEPROP '|Integer| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Boolean|) |INT;negative?;$B;15|
             (|Void|) (|OpenMathDevice|) (0 . |OMputApp|) (|String|)
             (5 . |OMputSymbol|) |INT;-;2$;39| (|Integer|)
             (12 . |OMputInteger|) (18 . |OMputEndApp|)
             (|OpenMathEncoding|) (23 . |OMencodingXML|)
             (27 . |OMopenString|) (33 . |OMputObject|)
             (38 . |OMputEndObject|) (43 . |OMclose|)
             |INT;OMwrite;$S;2| |INT;OMwrite;$BS;3|
             |INT;OMwrite;Omd$V;4| |INT;OMwrite;Omd$BV;5|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |INT;Zero;$;8|) $))
             |INT;=;2$B;34| |INT;zero?;$B;6|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |INT;One;$;9|) $))
             |INT;one?;$B;7| |INT;base;$;10| |INT;copy;2$;11|
             |INT;+;3$;40| |INT;inc;2$;12| |INT;-;3$;41|
             |INT;dec;2$;13| (|SingleInteger|) |INT;hash;$Si;14|
             |INT;<;2$B;35| (|OutputForm|) (48 . |outputForm|)
             |INT;coerce;$Of;16| |INT;coerce;2$;17| |INT;convert;2$;18|
             |INT;length;2$;19| |INT;>=;2$B;38| |INT;addmod;4$;20|
             |INT;submod;4$;21| |INT;rem;3$;50| |INT;mulmod;4$;22|
             (|Float|) (53 . |coerce|) |INT;convert;$F;23|
             (|DoubleFloat|) (58 . |coerce|) |INT;convert;$Df;24|
             (|InputForm|) (63 . |convert|) |INT;convert;$If;25|
             |INT;convert;$S;26| (|NonNegativeInteger|) (68 . |One|)
             (72 . <) (78 . |concat|) |INT;latex;$S;27|
             |INT;positiveRemainder;3$;28| (|Matrix| 14) (|Matrix| $)
             |INT;reducedSystem;2M;29| (|Vector| 14)
             (|Record| (|:| |mat| 68) (|:| |vec| 71)) (|Vector| $)
             |INT;reducedSystem;MVR;30| |INT;abs;2$;31|
             |INT;random;$;32| |INT;random;2$;33| |INT;>;2$B;36|
             |INT;<=;2$B;37| NIL NIL |INT;**;$Nni$;44| |INT;odd?;$B;45|
             |INT;max;3$;46| |INT;min;3$;47|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |INT;divide;2$R;48| |INT;quo;3$;49| |INT;shift;3$;51|
             (|Union| $ '"failed") |INT;recip;$U;52| |INT;gcd;3$;53|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                       (|:| |associate| $))
             |INT;unitNormal;$R;54| |INT;unitCanonical;2$;55|
             (|SparseUnivariatePolynomial| 14) (|List| 96)
             (|Union| 97 '"failed")
             (|IntegerSolveLinearPolynomialEquation|)
             (84 . |solveLinearPolynomialEquation|)
             (|SparseUnivariatePolynomial| $$) (|Factored| 101)
             (|UnivariatePolynomialSquareFree| $$ 101)
             (90 . |squareFree|) (95 . |primitivePart|)
             (100 . |leadingCoefficient|) (|GaloisGroupFactorizer| 101)
             (105 . |factor|) (110 . |coerce|) (115 . |exquo|)
             (|Factored| $) (121 . |factor|) (|Mapping| 101 $$)
             (|Factored| $$) (|FactoredFunctions2| $$ 101)
             (126 . |map|) (|FactoredFunctionUtilities| 101)
             (132 . |mergeFactors|) (138 . |factorSquareFree|)
             (143 . |zero?|) (148 . |unitCanonical|) (|List| 101)
             (|HeuGcd| 101) (153 . |gcd|)
             (|SparseUnivariatePolynomial| $)
             |INT;gcdPolynomial;3Sup;60| (|Fraction| 14)
             (|Union| 127 '"failed") (|Pattern| 14)
             (|PatternMatchResult| 14 $) (|Union| 14 '"failed")
             (|List| $) (|Record| (|:| |coef| 132) (|:| |generator| $))
             (|Union| 132 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 136 '"failed") (|PositiveInteger|))
          '#(~= 158 |zero?| 164 |unitNormal| 169 |unitCanonical| 174
             |unit?| 179 |symmetricRemainder| 184 |subtractIfCan| 190
             |submod| 196 |squareFreePart| 203 |squareFree| 208
             |sizeLess?| 213 |sign| 219 |shift| 224 |sample| 230
             |retractIfCan| 234 |retract| 239 |rem| 244 |reducedSystem|
             250 |recip| 261 |rationalIfCan| 266 |rational?| 271
             |rational| 276 |random| 281 |quo| 290 |principalIdeal| 296
             |prime?| 301 |powmod| 306 |positiveRemainder| 313
             |positive?| 319 |permutation| 324 |patternMatch| 330
             |one?| 337 |odd?| 342 |nextItem| 347 |negative?| 352
             |multiEuclidean| 357 |mulmod| 363 |min| 370 |max| 376
             |mask| 382 |length| 387 |leftReducedSystem| 392 |lcm| 403
             |latex| 414 |invmod| 419 |init| 425 |inc| 429 |hash| 434
             |gcdPolynomial| 439 |gcd| 445 |factorial| 456 |factor| 461
             |extendedEuclidean| 466 |exquo| 479 |expressIdealMember|
             485 |even?| 491 |euclideanSize| 496 |divide| 501
             |differentiate| 507 |dec| 518 |copy| 523 |convert| 528
             |coerce| 558 |characteristic| 578 |bit?| 582 |binomial|
             588 |before?| 594 |base| 600 |associates?| 604 |addmod|
             610 |abs| 617 |Zero| 622 |One| 626 |OMwrite| 630 D 654 >=
             665 > 671 = 677 <= 683 < 689 - 695 + 706 ** 712 * 724)
          '((|infinite| . 0) (|noetherian| . 0)
            (|canonicalsClosed| . 0) (|canonical| . 0)
            (|canonicalUnitNormal| . 0) (|multiplicativeValuation| . 0)
            (|noZeroDivisors| . 0) ((|commutative| "*") . 0)
            (|rightUnitary| . 0) (|leftUnitary| . 0)
            (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |OrderedRing&| NIL NIL |Module&| NIL NIL
                         |Ring&| NIL NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL NIL
                         |AbelianMonoid&| |Monoid&| NIL NIL NIL NIL NIL
                         |AbelianSemiGroup&| |SemiGroup&| NIL
                         |DifferentialSpace&| |OrderedType&|
                         |SetCategory&| NIL |RetractableTo&|
                         |DifferentialDomain&| |BasicType&| NIL NIL NIL
                         NIL NIL NIL NIL NIL NIL NIL NIL NIL)
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
                               (|LinearlyExplicitRingOver| 14)
                               (|BiModule| $$ $$) (|Ring|)
                               (|LeftModule| 14)
                               (|OrderedAbelianGroup|)
                               (|LeftModule| $$) (|Rng|)
                               (|RightModule| $$)
                               (|OrderedCancellationAbelianMonoid|)
                               (|AbelianGroup|)
                               (|OrderedAbelianMonoid|)
                               (|CancellationAbelianMonoid|)
                               (|OrderedAbelianSemiGroup|)
                               (|LinearSet| $$) (|AbelianMonoid|)
                               (|Monoid|) (|StepThrough|)
                               (|PatternMatchable| 14) (|OrderedSet|)
                               (|LeftLinearSet| $$)
                               (|RightLinearSet| $$)
                               (|AbelianSemiGroup|) (|SemiGroup|)
                               (|LeftLinearSet| 14)
                               (|DifferentialSpace|) (|OrderedType|)
                               (|SetCategory|) (|RealConstant|)
                               (|RetractableTo| 14)
                               (|DifferentialDomain| $$) (|BasicType|)
                               (|OpenMath|) (|ConvertibleTo| 11)
                               (|ConvertibleTo| 52)
                               (|ConvertibleTo| 55)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 129)
                               (|ConvertibleTo| 58)
                               (|ConvertibleTo| 14)
                               (|CoercibleFrom| $$)
                               (|CoercibleFrom| 14) (|Type|)
                               (|CoercibleTo| 41))
                            (|makeByteWordVec2| 138
                                '(1 9 8 0 10 3 9 8 0 11 11 12 2 9 8 0
                                  14 15 1 9 8 0 16 0 17 0 18 2 9 0 11
                                  17 19 1 9 8 0 20 1 9 8 0 21 1 9 8 0
                                  22 1 41 0 14 42 1 52 0 14 53 1 55 0
                                  14 56 1 58 0 14 59 0 62 0 63 2 14 6 0
                                  0 64 2 11 0 0 0 65 2 99 98 97 96 100
                                  1 103 102 101 104 1 101 0 0 105 1 101
                                  2 0 106 1 107 102 101 108 1 101 0 2
                                  109 2 0 90 0 0 110 1 0 111 0 112 2
                                  115 102 113 114 116 2 117 102 102 102
                                  118 1 107 102 101 119 1 101 6 0 120 1
                                  101 0 0 121 1 123 101 122 124 2 0 6 0
                                  0 1 1 0 6 0 29 1 0 93 0 94 1 0 0 0 95
                                  1 0 6 0 1 2 0 0 0 0 1 2 0 90 0 0 1 3
                                  0 0 0 0 0 49 1 0 0 0 1 1 0 111 0 1 2
                                  0 6 0 0 1 1 0 14 0 1 2 0 0 0 0 89 0 0
                                  0 1 1 0 131 0 1 1 0 14 0 1 2 0 0 0 0
                                  50 1 0 68 69 70 2 0 72 69 73 74 1 0
                                  90 0 91 1 0 128 0 1 1 0 6 0 1 1 0 127
                                  0 1 0 0 0 76 1 0 0 0 77 2 0 0 0 0 88
                                  1 0 133 132 1 1 0 6 0 1 3 0 0 0 0 0 1
                                  2 0 0 0 0 67 1 0 6 0 1 2 0 0 0 0 1 3
                                  0 130 0 129 130 1 1 0 6 0 31 1 0 6 0
                                  83 1 0 90 0 1 1 0 6 0 7 2 0 134 132 0
                                  1 3 0 0 0 0 0 51 2 0 0 0 0 85 2 0 0 0
                                  0 84 1 0 0 0 1 1 0 0 0 46 2 0 72 73 0
                                  1 1 0 68 73 1 2 0 0 0 0 1 1 0 0 132 1
                                  1 0 11 0 66 2 0 0 0 0 1 0 0 0 1 1 0 0
                                  0 35 1 0 38 0 39 2 0 125 125 125 126
                                  2 0 0 0 0 92 1 0 0 132 1 1 0 0 0 1 1
                                  0 111 0 112 2 0 135 0 0 1 3 0 137 0 0
                                  0 1 2 0 90 0 0 110 2 0 134 132 0 1 1
                                  0 6 0 1 1 0 62 0 1 2 0 86 0 0 87 2 0
                                  0 0 62 1 1 0 0 0 1 1 0 0 0 37 1 0 0 0
                                  33 1 0 11 0 61 1 0 55 0 57 1 0 52 0
                                  54 1 0 129 0 1 1 0 58 0 60 1 0 14 0
                                  45 1 0 0 14 44 1 0 0 0 1 1 0 0 14 44
                                  1 0 41 0 43 0 0 62 1 2 0 6 0 0 1 2 0
                                  0 0 0 1 2 0 6 0 0 1 0 0 0 32 2 0 6 0
                                  0 1 3 0 0 0 0 0 48 1 0 0 0 75 0 0 0
                                  27 0 0 0 30 3 0 8 9 0 6 26 2 0 11 0 6
                                  24 2 0 8 9 0 25 1 0 11 0 23 2 0 0 0
                                  62 1 1 0 0 0 1 2 0 6 0 0 47 2 0 6 0 0
                                  78 2 0 6 0 0 28 2 0 6 0 0 79 2 0 6 0
                                  0 40 1 0 0 0 13 2 0 0 0 0 36 2 0 0 0
                                  0 34 2 0 0 0 62 82 2 0 0 0 138 1 2 0
                                  0 14 0 81 2 0 0 0 0 80 2 0 0 14 0 81
                                  2 0 0 62 0 1 2 0 0 138 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|Integer| 'NILADIC T) 
