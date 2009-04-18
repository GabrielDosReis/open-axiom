
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

(PUT '|INT;zero?;$B;6| '|SPADreplace| 'ZEROP) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;one?;$B;7|)) 

(PUT '|INT;one?;$B;7| '|SPADreplace| '(XLAM (|x|) (EQL |x| 1))) 

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

(PUT '|INT;inc;2$;12| '|SPADreplace| '(XLAM (|x|) (+ |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;dec;2$;13|)) 

(PUT '|INT;dec;2$;13| '|SPADreplace| '(XLAM (|x|) (- |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |INT;hash;$Si;14|)) 

(PUT '|INT;hash;$Si;14| '|SPADreplace| 'SXHASH) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;negative?;$B;15|)) 

(PUT '|INT;negative?;$B;15| '|SPADreplace| 'MINUSP) 

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

(PUT '|INT;length;2$;19| '|SPADreplace| 'INTEGER-LENGTH) 

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

(PUT '|INT;convert;$Df;24| '|SPADreplace|
     '(XLAM (|x|) (FLOAT |x| |$DoubleFloatMaximum|))) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |INT;convert;$If;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%String|)
                |INT;convert;$S;26|)) 

(PUT '|INT;convert;$S;26| '|SPADreplace| 'STRINGIMAGE) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%String|)
                |INT;latex;$S;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;positiveRemainder;3$;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;reducedSystem;2M;29|)) 

(PUT '|INT;reducedSystem;2M;29| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |INT;reducedSystem;MVR;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;abs;2$;31|)) 

(PUT '|INT;abs;2$;31| '|SPADreplace| 'ABS) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |INT;random;$;32|)) 

(PUT '|INT;random;$;32| '|SPADreplace| '|random|) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;random;2$;33|)) 

(PUT '|INT;random;2$;33| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;=;2$B;34|)) 

(PUT '|INT;=;2$B;34| '|SPADreplace| 'EQL) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Boolean|)
                |INT;<;2$B;35|)) 

(PUT '|INT;<;2$B;35| '|SPADreplace| '<) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;-;2$;36|)) 

(PUT '|INT;-;2$;36| '|SPADreplace| '-) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;+;3$;37|)) 

(PUT '|INT;+;3$;37| '|SPADreplace| '+) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;-;3$;38|)) 

(PUT '|INT;-;3$;38| '|SPADreplace| '-) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;*;3$;39|)) 

(PUT '|INT;*;3$;39| '|SPADreplace| '*) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;*;3$;40|)) 

(PUT '|INT;*;3$;40| '|SPADreplace| '*) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| (|%IntegerSection| 0) |%Shell|)
                    |%Integer|)
                |INT;**;$Nni$;41|)) 

(PUT '|INT;**;$Nni$;41| '|SPADreplace| 'EXPT) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Boolean|)
                |INT;odd?;$B;42|)) 

(PUT '|INT;odd?;$B;42| '|SPADreplace| 'ODDP) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;max;3$;43|)) 

(PUT '|INT;max;3$;43| '|SPADreplace| 'MAX) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;min;3$;44|)) 

(PUT '|INT;min;3$;44| '|SPADreplace| 'MIN) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Pair|)
                |INT;divide;2$R;45|)) 

(PUT '|INT;divide;2$R;45| '|SPADreplace| 'DIVIDE2) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;quo;3$;46|)) 

(PUT '|INT;quo;3$;46| '|SPADreplace| 'QUOTIENT2) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;rem;3$;47|)) 

(PUT '|INT;rem;3$;47| '|SPADreplace| 'REMAINDER2) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;shift;3$;48|)) 

(PUT '|INT;shift;3$;48| '|SPADreplace| 'ASH) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Pair|)
                |INT;recip;$U;49|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Integer|)
                |INT;gcd;3$;50|)) 

(PUT '|INT;gcd;3$;50| '|SPADreplace| 'GCD) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Shell|)
                |INT;unitNormal;$R;51|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Integer|)
                |INT;unitCanonical;2$;52|)) 

(PUT '|INT;unitCanonical;2$;52| '|SPADreplace| 'ABS) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Thing| |%Shell|) |%Pair|)
                |INT;solveLinearPolynomialEquation|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;squareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;factorPolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |INT;factorSquareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |INT;gcdPolynomial;3Sup;57|)) 

(DEFUN |INT;writeOMInt| (|dev| |x| $)
  (SEQ (COND
         ((< |x| 0)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 13))
               (SPADCALL |dev| "arith1" "unary_minus"
                   (|getShellEntry| $ 15))
               (SPADCALL |dev| (- |x|) (|getShellEntry| $ 18))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 19)))))
         ('T (SPADCALL |dev| |x| (|getShellEntry| $ 18)))))) 

(DEFUN |INT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |INT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |INT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                     (|getShellEntry| $ 22))
                 |INT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 23))
           (|INT;writeOMInt| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 24))
           (SPADCALL |dev| (|getShellEntry| $ 25))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |INT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |INT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |INT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |INT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 21))
                     (|getShellEntry| $ 22))
                 |INT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
           (|INT;writeOMInt| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))
           (SPADCALL |dev| (|getShellEntry| $ 25))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |INT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |INT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 23))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 24))))) 

(DEFUN |INT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 23))))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 24))))))) 

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
  (SPADCALL |x| (|getShellEntry| $ 45))) 

(DEFUN |INT;coerce;2$;17| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |INT;convert;2$;18| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |INT;length;2$;19| (|a| $)
  (DECLARE (IGNORE $))
  (INTEGER-LENGTH |a|)) 

(DEFUN |INT;addmod;4$;20| (|a| |b| |p| $)
  (PROG (|c| #0=#:G1432)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |c| (+ |a| |b|) |INT;addmod;4$;20|)
                           (EXIT (COND
                                   ((NOT (< |c| |p|))
                                    (PROGN
                                      (LETT #0# (- |c| |p|)
                                       |INT;addmod;4$;20|)
                                      (GO #0#))))))
                      (EXIT |c|)))
           #0# (EXIT #0#))))) 

(DEFUN |INT;submod;4$;21| (|a| |b| |p| $)
  (PROG (|c|)
    (RETURN
      (SEQ (LETT |c| (- |a| |b|) |INT;submod;4$;21|)
           (EXIT (COND ((< |c| 0) (+ |c| |p|)) ('T |c|))))))) 

(DEFUN |INT;mulmod;4$;22| (|a| |b| |p| $)
  (REMAINDER2 (* |a| |b|) |p|)) 

(DEFUN |INT;convert;$F;23| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 56))) 

(DEFUN |INT;convert;$Df;24| (|x| $)
  (DECLARE (IGNORE $))
  (FLOAT |x| |$DoubleFloatMaximum|)) 

(DEFUN |INT;convert;$If;25| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 62))) 

(DEFUN |INT;convert;$S;26| (|x| $)
  (DECLARE (IGNORE $))
  (STRINGIMAGE |x|)) 

(DEFUN |INT;latex;$S;27| (|x| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s| (STRINGIMAGE |x|) |INT;latex;$S;27|)
           (COND ((< -1 |x|) (COND ((< |x| 10) (EXIT |s|)))))
           (EXIT (STRCONC "{" (STRCONC |s| "}"))))))) 

(DEFUN |INT;positiveRemainder;3$;28| (|a| |b| $)
  (PROG (|r|)
    (RETURN
      (COND
        ((MINUSP (LETT |r| (REMAINDER2 |a| |b|)
                       |INT;positiveRemainder;3$;28|))
         (COND ((MINUSP |b|) (- |r| |b|)) ('T (+ |r| |b|))))
        ('T |r|))))) 

(DEFUN |INT;reducedSystem;2M;29| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |INT;reducedSystem;MVR;30| (|m| |v| $) (CONS |m| '|vec|)) 

(DEFUN |INT;abs;2$;31| (|x| $) (DECLARE (IGNORE $)) (ABS |x|)) 

(DEFUN |INT;random;$;32| ($) (DECLARE (IGNORE $)) (|random|)) 

(DEFUN |INT;random;2$;33| (|x| $) (DECLARE (IGNORE $)) (RANDOM |x|)) 

(DEFUN |INT;=;2$B;34| (|x| |y| $) (DECLARE (IGNORE $)) (EQL |x| |y|)) 

(DEFUN |INT;<;2$B;35| (|x| |y| $) (DECLARE (IGNORE $)) (< |x| |y|)) 

(DEFUN |INT;-;2$;36| (|x| $) (DECLARE (IGNORE $)) (- |x|)) 

(DEFUN |INT;+;3$;37| (|x| |y| $) (DECLARE (IGNORE $)) (+ |x| |y|)) 

(DEFUN |INT;-;3$;38| (|x| |y| $) (DECLARE (IGNORE $)) (- |x| |y|)) 

(DEFUN |INT;*;3$;39| (|x| |y| $) (DECLARE (IGNORE $)) (* |x| |y|)) 

(DEFUN |INT;*;3$;40| (|m| |y| $) (DECLARE (IGNORE $)) (* |m| |y|)) 

(DEFUN |INT;**;$Nni$;41| (|x| |n| $)
  (DECLARE (IGNORE $))
  (EXPT |x| |n|)) 

(DEFUN |INT;odd?;$B;42| (|x| $) (DECLARE (IGNORE $)) (ODDP |x|)) 

(DEFUN |INT;max;3$;43| (|x| |y| $) (DECLARE (IGNORE $)) (MAX |x| |y|)) 

(DEFUN |INT;min;3$;44| (|x| |y| $) (DECLARE (IGNORE $)) (MIN |x| |y|)) 

(DEFUN |INT;divide;2$R;45| (|x| |y| $)
  (DECLARE (IGNORE $))
  (DIVIDE2 |x| |y|)) 

(DEFUN |INT;quo;3$;46| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QUOTIENT2 |x| |y|)) 

(DEFUN |INT;rem;3$;47| (|x| |y| $)
  (DECLARE (IGNORE $))
  (REMAINDER2 |x| |y|)) 

(DEFUN |INT;shift;3$;48| (|x| |y| $)
  (DECLARE (IGNORE $))
  (ASH |x| |y|)) 

(DEFUN |INT;recip;$U;49| (|x| $)
  (COND
    ((OR (EQL |x| 1) (EQL |x| -1)) (CONS 0 |x|))
    ('T (CONS 1 "failed")))) 

(DEFUN |INT;gcd;3$;50| (|x| |y| $) (DECLARE (IGNORE $)) (GCD |x| |y|)) 

(DEFUN |INT;unitNormal;$R;51| (|x| $)
  (COND ((< |x| 0) (VECTOR -1 (- |x|) -1)) ('T (VECTOR 1 |x| 1)))) 

(DEFUN |INT;unitCanonical;2$;52| (|x| $)
  (DECLARE (IGNORE $))
  (ABS |x|)) 

(DEFUN |INT;solveLinearPolynomialEquation| (|lp| |p| $)
  (SPADCALL |lp| |p| (|getShellEntry| $ 99))) 

(DEFUN |INT;squareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 103))) 

(DEFUN |INT;factorPolynomial| (|p| $)
  (PROG (|pp| #0=#:G1498)
    (RETURN
      (SEQ (LETT |pp| (SPADCALL |p| (|getShellEntry| $ 104))
                 |INT;factorPolynomial|)
           (EXIT (COND
                   ((EQL (SPADCALL |pp| (|getShellEntry| $ 105))
                         (SPADCALL |p| (|getShellEntry| $ 105)))
                    (SPADCALL |p| (|getShellEntry| $ 107)))
                   ('T
                    (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 107))
                        (SPADCALL (CONS #'|INT;factorPolynomial!0| $)
                            (SPADCALL
                                (PROG2 (LETT #0#
                                        (SPADCALL
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 105))
                                         (SPADCALL |pp|
                                          (|getShellEntry| $ 105))
                                         (|getShellEntry| $ 109))
                                        |INT;factorPolynomial|)
                                       (QCDR #0#)
                                  (|check-union| (QEQCAR #0# 0) $ #0#))
                                (|getShellEntry| $ 111))
                            (|getShellEntry| $ 115))
                        (|getShellEntry| $ 117))))))))) 

(DEFUN |INT;factorPolynomial!0| (|#1| $)
  (SPADCALL |#1| (|getShellEntry| $ 108))) 

(DEFUN |INT;factorSquareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 118))) 

(DEFUN |INT;gcdPolynomial;3Sup;57| (|p| |q| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 119))
     (SPADCALL |q| (|getShellEntry| $ 120)))
    ((SPADCALL |q| (|getShellEntry| $ 119))
     (SPADCALL |p| (|getShellEntry| $ 120)))
    ('T (SPADCALL (LIST |p| |q|) (|getShellEntry| $ 123))))) 

(DEFUN |Integer| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1523)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|Integer|) |Integer|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Integer|
                                   (LIST
                                    (CONS NIL (CONS 1 (|Integer;|))))))
                 (LETT #0# T |Integer|))
               (COND
                 ((NOT #0#) (HREM |$ConstructorCache| '|Integer|))))))))))) 

(DEFUN |Integer;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|Integer|) . #0=(|Integer|))
        (LETT $ (|newShell| 138) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Integer| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 80
            (|setShellEntry| $ 52
                (CONS (|dispatchFunction| |INT;*;3$;40|) $)))
        $)))) 

(MAKEPROP '|Integer| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |INT;Zero;$;8|) $))
             (|NonNegativeInteger|) (0 . |Zero|) (|Boolean|)
             |INT;<;2$B;35| (|Void|) (|OpenMathDevice|)
             (4 . |OMputApp|) (|String|) (9 . |OMputSymbol|)
             |INT;-;2$;36| (|Integer|) (16 . |OMputInteger|)
             (22 . |OMputEndApp|) (|OpenMathEncoding|)
             (27 . |OMencodingXML|) (31 . |OMopenString|)
             (37 . |OMputObject|) (42 . |OMputEndObject|)
             (47 . |OMclose|) |INT;OMwrite;$S;2| |INT;OMwrite;$BS;3|
             |INT;OMwrite;Omd$V;4| |INT;OMwrite;Omd$BV;5|
             |INT;zero?;$B;6|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |INT;One;$;9|) $))
             (52 . |One|) |INT;=;2$B;34| |INT;one?;$B;7|
             |INT;base;$;10| |INT;copy;2$;11| |INT;+;3$;37|
             |INT;inc;2$;12| |INT;-;3$;38| |INT;dec;2$;13|
             (|SingleInteger|) |INT;hash;$Si;14| |INT;negative?;$B;15|
             (|OutputForm|) (56 . |outputForm|) |INT;coerce;$Of;16|
             |INT;coerce;2$;17| |INT;convert;2$;18| |INT;length;2$;19|
             |INT;addmod;4$;20| |INT;submod;4$;21| NIL |INT;rem;3$;47|
             |INT;mulmod;4$;22| (|Float|) (61 . |coerce|)
             |INT;convert;$F;23| (|DoubleFloat|) (66 . |coerce|)
             |INT;convert;$Df;24| (|InputForm|) (71 . |convert|)
             |INT;convert;$If;25| (76 . |string|) |INT;convert;$S;26|
             (81 . <) (87 . |concat|) |INT;latex;$S;27|
             |INT;positiveRemainder;3$;28| (|Matrix| 17) (|Matrix| $)
             |INT;reducedSystem;2M;29| (|Vector| 17)
             (|Record| (|:| |mat| 70) (|:| |vec| 73)) (|Vector| $)
             |INT;reducedSystem;MVR;30| |INT;abs;2$;31|
             |INT;random;$;32| |INT;random;2$;33| NIL |INT;**;$Nni$;41|
             |INT;odd?;$B;42| |INT;max;3$;43| |INT;min;3$;44|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |INT;divide;2$R;45| |INT;quo;3$;46| |INT;shift;3$;48|
             (|Union| $ '"failed") |INT;recip;$U;49| |INT;gcd;3$;50|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |INT;unitNormal;$R;51| |INT;unitCanonical;2$;52|
             (|SparseUnivariatePolynomial| 17) (|List| 95)
             (|Union| 96 '"failed")
             (|IntegerSolveLinearPolynomialEquation|)
             (93 . |solveLinearPolynomialEquation|)
             (|SparseUnivariatePolynomial| $$) (|Factored| 100)
             (|UnivariatePolynomialSquareFree| $$ 100)
             (99 . |squareFree|) (104 . |primitivePart|)
             (109 . |leadingCoefficient|) (|GaloisGroupFactorizer| 100)
             (114 . |factor|) (119 . |coerce|) (124 . |exquo|)
             (|Factored| $) (130 . |factor|) (|Mapping| 100 $$)
             (|Factored| $$) (|FactoredFunctions2| $$ 100)
             (135 . |map|) (|FactoredFunctionUtilities| 100)
             (141 . |mergeFactors|) (147 . |factorSquareFree|)
             (152 . |zero?|) (157 . |unitCanonical|) (|List| 100)
             (|HeuGcd| 100) (162 . |gcd|)
             (|SparseUnivariatePolynomial| $)
             |INT;gcdPolynomial;3Sup;57| (|Fraction| 17)
             (|Union| 126 '"failed") (|PatternMatchResult| 17 $)
             (|Pattern| 17) (|Union| 17 '"failed") (|List| $)
             (|Record| (|:| |coef| 131) (|:| |generator| $))
             (|Union| 131 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 135 '"failed") (|PositiveInteger|))
          '#(~= 167 |zero?| 173 |unitNormal| 178 |unitCanonical| 183
             |unit?| 188 |symmetricRemainder| 193 |subtractIfCan| 199
             |submod| 205 |squareFreePart| 212 |squareFree| 217
             |sizeLess?| 222 |sign| 228 |shift| 233 |sample| 239
             |retractIfCan| 243 |retract| 248 |rem| 253 |reducedSystem|
             259 |recip| 270 |rationalIfCan| 275 |rational?| 280
             |rational| 285 |random| 290 |quo| 299 |principalIdeal| 305
             |prime?| 310 |powmod| 315 |positiveRemainder| 322
             |positive?| 328 |permutation| 333 |patternMatch| 339
             |one?| 346 |odd?| 351 |nextItem| 356 |negative?| 361
             |multiEuclidean| 366 |mulmod| 372 |min| 379 |max| 385
             |mask| 391 |length| 396 |lcm| 401 |latex| 412 |invmod| 417
             |init| 423 |inc| 427 |hash| 432 |gcdPolynomial| 437 |gcd|
             443 |factorial| 454 |factor| 459 |extendedEuclidean| 464
             |exquo| 477 |expressIdealMember| 483 |even?| 489
             |euclideanSize| 494 |divide| 499 |differentiate| 505 |dec|
             516 |copy| 521 |convert| 526 |coerce| 556 |characteristic|
             576 |bit?| 580 |binomial| 586 |base| 592 |associates?| 596
             |addmod| 602 |abs| 609 |Zero| 614 |One| 618 |OMwrite| 622
             D 646 >= 657 > 663 = 669 <= 675 < 681 - 687 + 698 ** 704 *
             716)
          '((|infinite| . 0) (|noetherian| . 0)
            (|canonicalsClosed| . 0) (|canonical| . 0)
            (|canonicalUnitNormal| . 0) (|multiplicativeValuation| . 0)
            (|noZeroDivisors| . 0) ((|commutative| "*") . 0)
            (|rightUnitary| . 0) (|leftUnitary| . 0)
            (|unitsKnown| . 0))
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
                         |AbelianSemiGroup&| |SemiGroup&| NIL
                         |RetractableTo&| |SetCategory&| NIL NIL NIL
                         NIL NIL NIL NIL NIL NIL NIL |BasicType&| NIL)
                      (CONS '#((|IntegerNumberSystem|)
                               (|EuclideanDomain|)
                               (|UniqueFactorizationDomain|)
                               (|PrincipalIdealDomain|)
                               (|OrderedIntegralDomain|) (|GcdDomain|)
                               (|IntegralDomain|) (|Algebra| $$)
                               (|CharacteristicZero|)
                               (|LinearlyExplicitRingOver| 17)
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
                               (|StepThrough|) (|PatternMatchable| 17)
                               (|OrderedSet|) (|AbelianSemiGroup|)
                               (|SemiGroup|) (|RealConstant|)
                               (|RetractableTo| 17) (|SetCategory|)
                               (|OpenMath|) (|ConvertibleTo| 14)
                               (|ConvertibleTo| 55)
                               (|ConvertibleTo| 58)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 129)
                               (|ConvertibleTo| 61)
                               (|ConvertibleTo| 17)
                               (|CoercibleFrom| $$)
                               (|CoercibleFrom| 17) (|BasicType|)
                               (|CoercibleTo| 44))
                            (|makeByteWordVec2| 137
                                '(0 7 0 8 1 12 11 0 13 3 12 11 0 14 14
                                  15 2 12 11 0 17 18 1 12 11 0 19 0 20
                                  0 21 2 12 0 14 20 22 1 12 11 0 23 1
                                  12 11 0 24 1 12 11 0 25 0 7 0 32 1 44
                                  0 17 45 1 55 0 17 56 1 58 0 17 59 1
                                  61 0 17 62 1 14 0 17 64 2 17 9 0 0 66
                                  2 14 0 0 0 67 2 98 97 96 95 99 1 102
                                  101 100 103 1 100 0 0 104 1 100 2 0
                                  105 1 106 101 100 107 1 100 0 2 108 2
                                  0 89 0 0 109 1 0 110 0 111 2 114 101
                                  112 113 115 2 116 101 101 101 117 1
                                  106 101 100 118 1 100 9 0 119 1 100 0
                                  0 120 1 122 100 121 123 2 0 9 0 0 1 1
                                  0 9 0 30 1 0 92 0 93 1 0 0 0 94 1 0 9
                                  0 1 2 0 0 0 0 1 2 0 89 0 0 1 3 0 0 0
                                  0 0 51 1 0 0 0 1 1 0 110 0 1 2 0 9 0
                                  0 1 1 0 17 0 1 2 0 0 0 0 88 0 0 0 1 1
                                  0 130 0 1 1 0 17 0 1 2 0 0 0 0 53 2 0
                                  74 71 75 76 1 0 70 71 72 1 0 89 0 90
                                  1 0 127 0 1 1 0 9 0 1 1 0 126 0 1 0 0
                                  0 78 1 0 0 0 79 2 0 0 0 0 87 1 0 132
                                  131 1 1 0 9 0 1 3 0 0 0 0 0 1 2 0 0 0
                                  0 69 1 0 9 0 1 2 0 0 0 0 1 3 0 128 0
                                  129 128 1 1 0 9 0 34 1 0 9 0 82 1 0
                                  89 0 1 1 0 9 0 43 2 0 133 131 0 1 3 0
                                  0 0 0 0 54 2 0 0 0 0 84 2 0 0 0 0 83
                                  1 0 0 0 1 1 0 0 0 49 2 0 0 0 0 1 1 0
                                  0 131 1 1 0 14 0 68 2 0 0 0 0 1 0 0 0
                                  1 1 0 0 0 38 1 0 41 0 42 2 0 124 124
                                  124 125 2 0 0 0 0 91 1 0 0 131 1 1 0
                                  0 0 1 1 0 110 0 111 2 0 134 0 0 1 3 0
                                  136 0 0 0 1 2 0 89 0 0 109 2 0 133
                                  131 0 1 1 0 9 0 1 1 0 7 0 1 2 0 85 0
                                  0 86 1 0 0 0 1 2 0 0 0 7 1 1 0 0 0 40
                                  1 0 0 0 36 1 0 14 0 65 1 0 58 0 60 1
                                  0 55 0 57 1 0 61 0 63 1 0 129 0 1 1 0
                                  17 0 48 1 0 0 17 47 1 0 0 0 1 1 0 0
                                  17 47 1 0 44 0 46 0 0 7 1 2 0 9 0 0 1
                                  2 0 0 0 0 1 0 0 0 35 2 0 9 0 0 1 3 0
                                  0 0 0 0 50 1 0 0 0 77 0 0 0 6 0 0 0
                                  31 3 0 11 12 0 9 29 2 0 14 0 9 27 2 0
                                  11 12 0 28 1 0 14 0 26 1 0 0 0 1 2 0
                                  0 0 7 1 2 0 9 0 0 1 2 0 9 0 0 1 2 0 9
                                  0 0 33 2 0 9 0 0 1 2 0 9 0 0 10 2 0 0
                                  0 0 39 1 0 0 0 16 2 0 0 0 0 37 2 0 0
                                  0 7 81 2 0 0 0 137 1 2 0 0 0 0 52 2 0
                                  0 17 0 80 2 0 0 7 0 1 2 0 0 137 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|Integer| 'NILADIC T) 
