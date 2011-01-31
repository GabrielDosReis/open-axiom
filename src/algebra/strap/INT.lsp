
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
     '(XLAM (|m| |v|) (|%makepair| |m| '|vec|))) 

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
  (SEQ (COND
         ((MINUSP |x|)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 10))
               (SPADCALL |dev| "arith1" "unary_minus"
                   (|getShellEntry| $ 12))
               (SPADCALL |dev| (- |x|) (|getShellEntry| $ 15))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 16)))))
         (T (SPADCALL |dev| |x| (|getShellEntry| $ 15)))))) 

(DEFUN |INT;OMwrite;$S;2| (|x| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 18))
                    (|getShellEntry| $ 19))))
    (SEQ (SPADCALL |dev| (|getShellEntry| $ 20))
         (|INT;writeOMInt| |dev| |x| $)
         (SPADCALL |dev| (|getShellEntry| $ 21))
         (SPADCALL |dev| (|getShellEntry| $ 22))
         (EXIT (OM-STRINGPTRTOSTRING |sp|))))) 

(DEFUN |INT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 18))
                    (|getShellEntry| $ 19))))
    (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 20))))
         (|INT;writeOMInt| |dev| |x| $)
         (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 21))))
         (SPADCALL |dev| (|getShellEntry| $ 22))
         (EXIT (OM-STRINGPTRTOSTRING |sp|))))) 

(DEFUN |INT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 20))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 21))))) 

(DEFUN |INT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 20))))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 21))))))) 

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

(DEFUN |INT;coerce;$Of;16| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 42))) 

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

(DEFUN |INT;convert;$F;23| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 53))) 

(DEFUN |INT;convert;$Df;24| (|x| $)
  (DECLARE (IGNORE $))
  (FLOAT |x| |$DoubleFloatMaximum|)) 

(DEFUN |INT;convert;$If;25| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 59))) 

(DEFUN |INT;convert;$S;26| (|x| $)
  (DECLARE (IGNORE $))
  (WRITE-TO-STRING |x|)) 

(DEFUN |INT;latex;$S;27| (|x| $)
  (LET ((|s| (WRITE-TO-STRING |x|)))
    (SEQ (COND ((< -1 |x|) (COND ((< |x| 10) (EXIT |s|)))))
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
  (SPADCALL |lp| |p| (|getShellEntry| $ 101))) 

(DEFUN |INT;squareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 105))) 

(DEFUN |INT;factorPolynomial| (|p| $)
  (LET ((|pp| (SPADCALL |p| (|getShellEntry| $ 106))))
    (COND
      ((EQL (SPADCALL |pp| (|getShellEntry| $ 107))
            (SPADCALL |p| (|getShellEntry| $ 107)))
       (SPADCALL |p| (|getShellEntry| $ 109)))
      (T (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 109))
             (SPADCALL (CONS #'|INT;factorPolynomial!0| $)
                 (SPADCALL
                     (LET ((#0=#:G1479
                               (SPADCALL
                                   (SPADCALL |p|
                                    (|getShellEntry| $ 107))
                                   (SPADCALL |pp|
                                    (|getShellEntry| $ 107))
                                   (|getShellEntry| $ 111))))
                       (|check-union| (ZEROP (CAR #0#)) $ #0#)
                       (CDR #0#))
                     (|getShellEntry| $ 113))
                 (|getShellEntry| $ 117))
             (|getShellEntry| $ 119)))))) 

(DEFUN |INT;factorPolynomial!0| (|#1| $)
  (SPADCALL |#1| (|getShellEntry| $ 110))) 

(DEFUN |INT;factorSquareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 120))) 

(DEFUN |INT;gcdPolynomial;3Sup;60| (|p| |q| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 121))
     (SPADCALL |q| (|getShellEntry| $ 122)))
    ((SPADCALL |q| (|getShellEntry| $ 121))
     (SPADCALL |p| (|getShellEntry| $ 122)))
    (T (SPADCALL (LIST |p| |q|) (|getShellEntry| $ 125))))) 

(DEFUN |Integer| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#0=#:G1511)
    (RETURN
      (COND
        ((SETQ #0# (HGET |$ConstructorCache| '|Integer|))
         (|CDRwithIncrement| (CDAR #0#)))
        (T (UNWIND-PROTECT
             (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Integer|
                                 (LIST (CONS NIL (CONS 1 (|Integer;|))))))
               (SETQ #0# T))
             (COND ((NOT #0#) (HREM |$ConstructorCache| '|Integer|))))))))) 

(DEFUN |Integer;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| '(|Integer|)) ($ (|newShell| 140))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|Integer| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 82
        (|setShellEntry| $ 81
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
             (68 . |string|) |INT;convert;$S;26| (|NonNegativeInteger|)
             (73 . |One|) (77 . <) (83 . |concat|) |INT;latex;$S;27|
             |INT;positiveRemainder;3$;28| (|Matrix| 14) (|Matrix| $)
             |INT;reducedSystem;2M;29| (|Vector| 14)
             (|Record| (|:| |mat| 69) (|:| |vec| 72)) (|Vector| $)
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
             (|SparseUnivariatePolynomial| 14) (|List| 97)
             (|Union| 98 '"failed")
             (|IntegerSolveLinearPolynomialEquation|)
             (89 . |solveLinearPolynomialEquation|)
             (|SparseUnivariatePolynomial| $$) (|Factored| 102)
             (|UnivariatePolynomialSquareFree| $$ 102)
             (95 . |squareFree|) (100 . |primitivePart|)
             (105 . |leadingCoefficient|) (|GaloisGroupFactorizer| 102)
             (110 . |factor|) (115 . |coerce|) (120 . |exquo|)
             (|Factored| $) (126 . |factor|) (|Mapping| 102 $$)
             (|Factored| $$) (|FactoredFunctions2| $$ 102)
             (131 . |map|) (|FactoredFunctionUtilities| 102)
             (137 . |mergeFactors|) (143 . |factorSquareFree|)
             (148 . |zero?|) (153 . |unitCanonical|) (|List| 102)
             (|HeuGcd| 102) (158 . |gcd|)
             (|SparseUnivariatePolynomial| $)
             |INT;gcdPolynomial;3Sup;60| (|Fraction| 14)
             (|Union| 128 '"failed") (|Pattern| 14)
             (|PatternMatchResult| 14 $) (|Union| 14 '"failed")
             (|List| $) (|Record| (|:| |coef| 133) (|:| |generator| $))
             (|Union| 133 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 137 '"failed") (|PositiveInteger|))
          '#(~= 163 |zero?| 169 |unitNormal| 174 |unitCanonical| 179
             |unit?| 184 |symmetricRemainder| 189 |subtractIfCan| 195
             |submod| 201 |squareFreePart| 208 |squareFree| 213
             |sizeLess?| 218 |sign| 224 |shift| 229 |sample| 235
             |retractIfCan| 239 |retract| 244 |rem| 249 |reducedSystem|
             255 |recip| 266 |rationalIfCan| 271 |rational?| 276
             |rational| 281 |random| 286 |quo| 295 |principalIdeal| 301
             |prime?| 306 |powmod| 311 |positiveRemainder| 318
             |positive?| 324 |permutation| 329 |patternMatch| 335
             |one?| 342 |odd?| 347 |nextItem| 352 |negative?| 357
             |multiEuclidean| 362 |mulmod| 368 |min| 375 |max| 381
             |mask| 387 |length| 392 |leftReducedSystem| 397 |lcm| 408
             |latex| 419 |invmod| 424 |init| 430 |inc| 434 |hash| 439
             |gcdPolynomial| 444 |gcd| 450 |factorial| 461 |factor| 466
             |extendedEuclidean| 471 |exquo| 484 |expressIdealMember|
             490 |even?| 496 |euclideanSize| 501 |divide| 506
             |differentiate| 512 |dec| 523 |copy| 528 |convert| 533
             |coerce| 563 |characteristic| 583 |bit?| 587 |binomial|
             593 |before?| 599 |base| 605 |associates?| 609 |addmod|
             615 |abs| 622 |Zero| 627 |One| 631 |OMwrite| 635 D 659 >=
             670 > 676 = 682 <= 688 < 694 - 700 + 711 ** 717 * 729)
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
                               (|ConvertibleTo| 130)
                               (|ConvertibleTo| 58)
                               (|ConvertibleTo| 14)
                               (|CoercibleFrom| $$)
                               (|CoercibleFrom| 14) (|Type|)
                               (|CoercibleTo| 41))
                            (|makeByteWordVec2| 139
                                '(1 9 8 0 10 3 9 8 0 11 11 12 2 9 8 0
                                  14 15 1 9 8 0 16 0 17 0 18 2 9 0 11
                                  17 19 1 9 8 0 20 1 9 8 0 21 1 9 8 0
                                  22 1 41 0 14 42 1 52 0 14 53 1 55 0
                                  14 56 1 58 0 14 59 1 11 0 14 61 0 63
                                  0 64 2 14 6 0 0 65 2 11 0 0 0 66 2
                                  100 99 98 97 101 1 104 103 102 105 1
                                  102 0 0 106 1 102 2 0 107 1 108 103
                                  102 109 1 102 0 2 110 2 0 91 0 0 111
                                  1 0 112 0 113 2 116 103 114 115 117 2
                                  118 103 103 103 119 1 108 103 102 120
                                  1 102 6 0 121 1 102 0 0 122 1 124 102
                                  123 125 2 0 6 0 0 1 1 0 6 0 29 1 0 94
                                  0 95 1 0 0 0 96 1 0 6 0 1 2 0 0 0 0 1
                                  2 0 91 0 0 1 3 0 0 0 0 0 49 1 0 0 0 1
                                  1 0 112 0 1 2 0 6 0 0 1 1 0 14 0 1 2
                                  0 0 0 0 90 0 0 0 1 1 0 132 0 1 1 0 14
                                  0 1 2 0 0 0 0 50 1 0 69 70 71 2 0 73
                                  70 74 75 1 0 91 0 92 1 0 129 0 1 1 0
                                  6 0 1 1 0 128 0 1 0 0 0 77 1 0 0 0 78
                                  2 0 0 0 0 89 1 0 134 133 1 1 0 6 0 1
                                  3 0 0 0 0 0 1 2 0 0 0 0 68 1 0 6 0 1
                                  2 0 0 0 0 1 3 0 131 0 130 131 1 1 0 6
                                  0 31 1 0 6 0 84 1 0 91 0 1 1 0 6 0 7
                                  2 0 135 133 0 1 3 0 0 0 0 0 51 2 0 0
                                  0 0 86 2 0 0 0 0 85 1 0 0 0 1 1 0 0 0
                                  46 2 0 73 74 0 1 1 0 69 74 1 2 0 0 0
                                  0 1 1 0 0 133 1 1 0 11 0 67 2 0 0 0 0
                                  1 0 0 0 1 1 0 0 0 35 1 0 38 0 39 2 0
                                  126 126 126 127 2 0 0 0 0 93 1 0 0
                                  133 1 1 0 0 0 1 1 0 112 0 113 2 0 136
                                  0 0 1 3 0 138 0 0 0 1 2 0 91 0 0 111
                                  2 0 135 133 0 1 1 0 6 0 1 1 0 63 0 1
                                  2 0 87 0 0 88 2 0 0 0 63 1 1 0 0 0 1
                                  1 0 0 0 37 1 0 0 0 33 1 0 11 0 62 1 0
                                  55 0 57 1 0 52 0 54 1 0 130 0 1 1 0
                                  58 0 60 1 0 14 0 45 1 0 0 14 44 1 0 0
                                  0 1 1 0 0 14 44 1 0 41 0 43 0 0 63 1
                                  2 0 6 0 0 1 2 0 0 0 0 1 2 0 6 0 0 1 0
                                  0 0 32 2 0 6 0 0 1 3 0 0 0 0 0 48 1 0
                                  0 0 76 0 0 0 27 0 0 0 30 3 0 8 9 0 6
                                  26 2 0 11 0 6 24 2 0 8 9 0 25 1 0 11
                                  0 23 2 0 0 0 63 1 1 0 0 0 1 2 0 6 0 0
                                  47 2 0 6 0 0 79 2 0 6 0 0 28 2 0 6 0
                                  0 80 2 0 6 0 0 40 1 0 0 0 13 2 0 0 0
                                  0 36 2 0 0 0 0 34 2 0 0 0 63 83 2 0 0
                                  0 139 1 2 0 0 14 0 82 2 0 0 0 0 81 2
                                  0 0 14 0 82 2 0 0 63 0 1 2 0 0 139 0
                                  1)))))
          '|lookupComplete|)) 

(MAKEPROP '|Integer| 'NILADIC T) 
