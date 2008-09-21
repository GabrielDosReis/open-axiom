
(/VERSIONCHECK 2) 

(DEFUN |DFLOAT;doubleFloatFormat;2S;1| (|s| $)
  (PROG (|ss|)
    (RETURN
      (SEQ (LETT |ss| (|getShellEntry| $ 6)
                 |DFLOAT;doubleFloatFormat;2S;1|)
           (SETELT $ 6 |s|) (EXIT |ss|))))) 

(DEFUN |DFLOAT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |DFLOAT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |DFLOAT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 10))
                     (|getShellEntry| $ 12))
                 |DFLOAT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 14))
           (SPADCALL |dev| |x| (|getShellEntry| $ 16))
           (SPADCALL |dev| (|getShellEntry| $ 17))
           (SPADCALL |dev| (|getShellEntry| $ 18))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |DFLOAT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |DFLOAT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |DFLOAT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|)
                 |DFLOAT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 10))
                     (|getShellEntry| $ 12))
                 |DFLOAT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 14))))
           (SPADCALL |dev| |x| (|getShellEntry| $ 16))
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 17))))
           (SPADCALL |dev| (|getShellEntry| $ 18))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|)
                 |DFLOAT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |DFLOAT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 14))
       (SPADCALL |dev| |x| (|getShellEntry| $ 16))
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 17))))) 

(DEFUN |DFLOAT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 14))))
       (SPADCALL |dev| |x| (|getShellEntry| $ 16))
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 17))))))) 

(PUT '|DFLOAT;checkComplex| '|SPADreplace| 'C-TO-R) 

(DEFUN |DFLOAT;checkComplex| (|x| $) (C-TO-R |x|)) 

(PUT '|DFLOAT;base;Pi;7| '|SPADreplace| '(XLAM NIL (FLOAT-RADIX 0.0))) 

(DEFUN |DFLOAT;base;Pi;7| ($) (FLOAT-RADIX 0.0)) 

(DEFUN |DFLOAT;mantissa;$I;8| (|x| $) (QCAR (|DFLOAT;manexp| |x| $))) 

(DEFUN |DFLOAT;exponent;$I;9| (|x| $) (QCDR (|DFLOAT;manexp| |x| $))) 

(PUT '|DFLOAT;precision;Pi;10| '|SPADreplace|
     '(XLAM NIL (FLOAT-DIGITS 0.0))) 

(DEFUN |DFLOAT;precision;Pi;10| ($) (FLOAT-DIGITS 0.0)) 

(DEFUN |DFLOAT;bits;Pi;11| ($)
  (PROG (#0=#:G1421)
    (RETURN
      (COND
        ((EQL (FLOAT-RADIX 0.0) 2) (FLOAT-DIGITS 0.0))
        ((EQL (FLOAT-RADIX 0.0) 16) (* 4 (FLOAT-DIGITS 0.0)))
        ('T
         (PROG1 (LETT #0#
                      (FIX (SPADCALL (FLOAT-DIGITS 0.0)
                               (SPADCALL
                                   (FLOAT (FLOAT-RADIX 0.0)
                                    |$DoubleFloatMaximum|)
                                   (|getShellEntry| $ 30))
                               (|getShellEntry| $ 31)))
                      |DFLOAT;bits;Pi;11|)
           (|check-subtype| (> #0# 0) '(|PositiveInteger|) #0#))))))) 

(PUT '|DFLOAT;max;$;12| '|SPADreplace|
     '(XLAM NIL |$DoubleFloatMaximum|)) 

(DEFUN |DFLOAT;max;$;12| ($) |$DoubleFloatMaximum|) 

(PUT '|DFLOAT;min;$;13| '|SPADreplace|
     '(XLAM NIL |$DoubleFloatMinimum|)) 

(DEFUN |DFLOAT;min;$;13| ($) |$DoubleFloatMinimum|) 

(DEFUN |DFLOAT;order;$I;14| (|a| $)
  (- (+ (FLOAT-DIGITS 0.0) (SPADCALL |a| (|getShellEntry| $ 28))) 1)) 

(PUT '|DFLOAT;Zero;$;15| '|SPADreplace|
     '(XLAM NIL (FLOAT 0 |$DoubleFloatMaximum|))) 

(DEFUN |DFLOAT;Zero;$;15| ($) (FLOAT 0 |$DoubleFloatMaximum|)) 

(PUT '|DFLOAT;One;$;16| '|SPADreplace|
     '(XLAM NIL (FLOAT 1 |$DoubleFloatMaximum|))) 

(DEFUN |DFLOAT;One;$;16| ($) (FLOAT 1 |$DoubleFloatMaximum|)) 

(DEFUN |DFLOAT;exp1;$;17| ($)
  (/ (FLOAT 534625820200 |$DoubleFloatMaximum|)
     (FLOAT 196677847971 |$DoubleFloatMaximum|))) 

(PUT '|DFLOAT;pi;$;18| '|SPADreplace| '(XLAM NIL PI)) 

(DEFUN |DFLOAT;pi;$;18| ($) PI) 

(DEFUN |DFLOAT;coerce;$Of;19| (|x| $)
  (SPADCALL (FORMAT NIL (|getShellEntry| $ 6) |x|)
      (|getShellEntry| $ 41))) 

(DEFUN |DFLOAT;convert;$If;20| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 44))) 

(PUT '|DFLOAT;<;2$B;21| '|SPADreplace| '<) 

(DEFUN |DFLOAT;<;2$B;21| (|x| |y| $) (< |x| |y|)) 

(PUT '|DFLOAT;-;2$;22| '|SPADreplace| '-) 

(DEFUN |DFLOAT;-;2$;22| (|x| $) (- |x|)) 

(PUT '|DFLOAT;+;3$;23| '|SPADreplace| '+) 

(DEFUN |DFLOAT;+;3$;23| (|x| |y| $) (+ |x| |y|)) 

(PUT '|DFLOAT;-;3$;24| '|SPADreplace| '-) 

(DEFUN |DFLOAT;-;3$;24| (|x| |y| $) (- |x| |y|)) 

(PUT '|DFLOAT;*;3$;25| '|SPADreplace| '*) 

(DEFUN |DFLOAT;*;3$;25| (|x| |y| $) (* |x| |y|)) 

(PUT '|DFLOAT;*;I2$;26| '|SPADreplace| '*) 

(DEFUN |DFLOAT;*;I2$;26| (|i| |x| $) (* |i| |x|)) 

(PUT '|DFLOAT;max;3$;27| '|SPADreplace| 'MAX) 

(DEFUN |DFLOAT;max;3$;27| (|x| |y| $) (MAX |x| |y|)) 

(PUT '|DFLOAT;min;3$;28| '|SPADreplace| 'MIN) 

(DEFUN |DFLOAT;min;3$;28| (|x| |y| $) (MIN |x| |y|)) 

(PUT '|DFLOAT;=;2$B;29| '|SPADreplace| '=) 

(DEFUN |DFLOAT;=;2$B;29| (|x| |y| $) (= |x| |y|)) 

(PUT '|DFLOAT;/;$I$;30| '|SPADreplace| '/) 

(DEFUN |DFLOAT;/;$I$;30| (|x| |i| $) (/ |x| |i|)) 

(DEFUN |DFLOAT;sqrt;2$;31| (|x| $)
  (|DFLOAT;checkComplex| (SQRT |x|) $)) 

(DEFUN |DFLOAT;log10;2$;32| (|x| $)
  (|DFLOAT;checkComplex| (|log| |x|) $)) 

(PUT '|DFLOAT;**;$I$;33| '|SPADreplace| 'EXPT) 

(DEFUN |DFLOAT;**;$I$;33| (|x| |i| $) (EXPT |x| |i|)) 

(DEFUN |DFLOAT;**;3$;34| (|x| |y| $)
  (|DFLOAT;checkComplex| (EXPT |x| |y|) $)) 

(PUT '|DFLOAT;coerce;I$;35| '|SPADreplace|
     '(XLAM (|i|) (FLOAT |i| |$DoubleFloatMaximum|))) 

(DEFUN |DFLOAT;coerce;I$;35| (|i| $)
  (FLOAT |i| |$DoubleFloatMaximum|)) 

(PUT '|DFLOAT;exp;2$;36| '|SPADreplace| 'EXP) 

(DEFUN |DFLOAT;exp;2$;36| (|x| $) (EXP |x|)) 

(DEFUN |DFLOAT;log;2$;37| (|x| $) (|DFLOAT;checkComplex| (LN |x|) $)) 

(DEFUN |DFLOAT;log2;2$;38| (|x| $)
  (|DFLOAT;checkComplex| (LOG2 |x|) $)) 

(PUT '|DFLOAT;sin;2$;39| '|SPADreplace| 'SIN) 

(DEFUN |DFLOAT;sin;2$;39| (|x| $) (SIN |x|)) 

(PUT '|DFLOAT;cos;2$;40| '|SPADreplace| 'COS) 

(DEFUN |DFLOAT;cos;2$;40| (|x| $) (COS |x|)) 

(PUT '|DFLOAT;tan;2$;41| '|SPADreplace| 'TAN) 

(DEFUN |DFLOAT;tan;2$;41| (|x| $) (TAN |x|)) 

(PUT '|DFLOAT;cot;2$;42| '|SPADreplace| 'COT) 

(DEFUN |DFLOAT;cot;2$;42| (|x| $) (COT |x|)) 

(PUT '|DFLOAT;sec;2$;43| '|SPADreplace| 'SEC) 

(DEFUN |DFLOAT;sec;2$;43| (|x| $) (SEC |x|)) 

(PUT '|DFLOAT;csc;2$;44| '|SPADreplace| 'CSC) 

(DEFUN |DFLOAT;csc;2$;44| (|x| $) (CSC |x|)) 

(DEFUN |DFLOAT;asin;2$;45| (|x| $)
  (|DFLOAT;checkComplex| (ASIN |x|) $)) 

(DEFUN |DFLOAT;acos;2$;46| (|x| $)
  (|DFLOAT;checkComplex| (ACOS |x|) $)) 

(PUT '|DFLOAT;atan;2$;47| '|SPADreplace| 'ATAN) 

(DEFUN |DFLOAT;atan;2$;47| (|x| $) (ATAN |x|)) 

(DEFUN |DFLOAT;acsc;2$;48| (|x| $)
  (|DFLOAT;checkComplex| (ACSC |x|) $)) 

(PUT '|DFLOAT;acot;2$;49| '|SPADreplace| 'ACOT) 

(DEFUN |DFLOAT;acot;2$;49| (|x| $) (ACOT |x|)) 

(DEFUN |DFLOAT;asec;2$;50| (|x| $)
  (|DFLOAT;checkComplex| (ASEC |x|) $)) 

(PUT '|DFLOAT;sinh;2$;51| '|SPADreplace| 'SINH) 

(DEFUN |DFLOAT;sinh;2$;51| (|x| $) (SINH |x|)) 

(PUT '|DFLOAT;cosh;2$;52| '|SPADreplace| 'COSH) 

(DEFUN |DFLOAT;cosh;2$;52| (|x| $) (COSH |x|)) 

(PUT '|DFLOAT;tanh;2$;53| '|SPADreplace| 'TANH) 

(DEFUN |DFLOAT;tanh;2$;53| (|x| $) (TANH |x|)) 

(PUT '|DFLOAT;csch;2$;54| '|SPADreplace| 'CSCH) 

(DEFUN |DFLOAT;csch;2$;54| (|x| $) (CSCH |x|)) 

(PUT '|DFLOAT;coth;2$;55| '|SPADreplace| 'COTH) 

(DEFUN |DFLOAT;coth;2$;55| (|x| $) (COTH |x|)) 

(PUT '|DFLOAT;sech;2$;56| '|SPADreplace| 'SECH) 

(DEFUN |DFLOAT;sech;2$;56| (|x| $) (SECH |x|)) 

(PUT '|DFLOAT;asinh;2$;57| '|SPADreplace| 'ASINH) 

(DEFUN |DFLOAT;asinh;2$;57| (|x| $) (ASINH |x|)) 

(DEFUN |DFLOAT;acosh;2$;58| (|x| $)
  (|DFLOAT;checkComplex| (ACOSH |x|) $)) 

(DEFUN |DFLOAT;atanh;2$;59| (|x| $)
  (|DFLOAT;checkComplex| (ATANH |x|) $)) 

(PUT '|DFLOAT;acsch;2$;60| '|SPADreplace| 'ACSCH) 

(DEFUN |DFLOAT;acsch;2$;60| (|x| $) (ACSCH |x|)) 

(DEFUN |DFLOAT;acoth;2$;61| (|x| $)
  (|DFLOAT;checkComplex| (ACOTH |x|) $)) 

(DEFUN |DFLOAT;asech;2$;62| (|x| $)
  (|DFLOAT;checkComplex| (ASECH |x|) $)) 

(PUT '|DFLOAT;/;3$;63| '|SPADreplace| '/) 

(DEFUN |DFLOAT;/;3$;63| (|x| |y| $) (/ |x| |y|)) 

(PUT '|DFLOAT;negative?;$B;64| '|SPADreplace| 'MINUSP) 

(DEFUN |DFLOAT;negative?;$B;64| (|x| $) (MINUSP |x|)) 

(PUT '|DFLOAT;zero?;$B;65| '|SPADreplace| 'ZEROP) 

(DEFUN |DFLOAT;zero?;$B;65| (|x| $) (ZEROP |x|)) 

(PUT '|DFLOAT;hash;$Si;66| '|SPADreplace| 'HASHEQ) 

(DEFUN |DFLOAT;hash;$Si;66| (|x| $) (HASHEQ |x|)) 

(DEFUN |DFLOAT;recip;$U;67| (|x| $)
  (COND ((ZEROP |x|) (CONS 1 "failed")) ('T (CONS 0 (/ 1.0 |x|))))) 

(PUT '|DFLOAT;differentiate;2$;68| '|SPADreplace| '(XLAM (|x|) 0.0)) 

(DEFUN |DFLOAT;differentiate;2$;68| (|x| $) 0.0) 

(DEFUN |DFLOAT;Gamma;2$;69| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 96))) 

(DEFUN |DFLOAT;Beta;3$;70| (|x| |y| $)
  (SPADCALL |x| |y| (|getShellEntry| $ 98))) 

(PUT '|DFLOAT;wholePart;$I;71| '|SPADreplace| 'FIX) 

(DEFUN |DFLOAT;wholePart;$I;71| (|x| $) (FIX |x|)) 

(DEFUN |DFLOAT;float;2IPi$;72| (|ma| |ex| |b| $)
  (* |ma| (EXPT (FLOAT |b| |$DoubleFloatMaximum|) |ex|))) 

(PUT '|DFLOAT;convert;2$;73| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |DFLOAT;convert;2$;73| (|x| $) |x|) 

(DEFUN |DFLOAT;convert;$F;74| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 104))) 

(DEFUN |DFLOAT;rationalApproximation;$NniF;75| (|x| |d| $)
  (SPADCALL |x| |d| 10 (|getShellEntry| $ 108))) 

(DEFUN |DFLOAT;atan;3$;76| (|x| |y| $)
  (PROG (|theta|)
    (RETURN
      (SEQ (COND
             ((= |x| 0.0)
              (COND
                ((< 0.0 |y|) (/ PI 2))
                ((< |y| 0.0) (- (/ PI 2)))
                ('T 0.0)))
             ('T
              (SEQ (LETT |theta| (ATAN (FLOAT-SIGN 1.0 (/ |y| |x|)))
                         |DFLOAT;atan;3$;76|)
                   (COND
                     ((< |x| 0.0)
                      (LETT |theta| (- PI |theta|) |DFLOAT;atan;3$;76|)))
                   (COND
                     ((< |y| 0.0)
                      (LETT |theta| (- |theta|) |DFLOAT;atan;3$;76|)))
                   (EXIT |theta|)))))))) 

(DEFUN |DFLOAT;retract;$F;77| (|x| $)
  (PROG (#0=#:G1496)
    (RETURN
      (SPADCALL |x|
          (PROG1 (LETT #0# (- (FLOAT-DIGITS 0.0) 1)
                       |DFLOAT;retract;$F;77|)
            (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|) #0#))
          (FLOAT-RADIX 0.0) (|getShellEntry| $ 108))))) 

(DEFUN |DFLOAT;retractIfCan;$U;78| (|x| $)
  (PROG (#0=#:G1501)
    (RETURN
      (CONS 0
            (SPADCALL |x|
                (PROG1 (LETT #0# (- (FLOAT-DIGITS 0.0) 1)
                             |DFLOAT;retractIfCan;$U;78|)
                  (|check-subtype| (>= #0# 0) '(|NonNegativeInteger|)
                      #0#))
                (FLOAT-RADIX 0.0) (|getShellEntry| $ 108)))))) 

(DEFUN |DFLOAT;retract;$I;79| (|x| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n| (FIX |x|) |DFLOAT;retract;$I;79|)
           (EXIT (COND
                   ((= |x| (FLOAT |n| |$DoubleFloatMaximum|)) |n|)
                   ('T (|error| "Not an integer")))))))) 

(DEFUN |DFLOAT;retractIfCan;$U;80| (|x| $)
  (PROG (|n|)
    (RETURN
      (SEQ (LETT |n| (FIX |x|) |DFLOAT;retractIfCan;$U;80|)
           (EXIT (COND
                   ((= |x| (FLOAT |n| |$DoubleFloatMaximum|))
                    (CONS 0 |n|))
                   ('T (CONS 1 "failed")))))))) 

(DEFUN |DFLOAT;sign;$I;81| (|x| $)
  (SPADCALL (FLOAT-SIGN |x| 1.0) (|getShellEntry| $ 114))) 

(PUT '|DFLOAT;abs;2$;82| '|SPADreplace|
     '(XLAM (|x|) (FLOAT-SIGN 1.0 |x|))) 

(DEFUN |DFLOAT;abs;2$;82| (|x| $) (FLOAT-SIGN 1.0 |x|)) 

(DEFUN |DFLOAT;manexp| (|x| $)
  (PROG (|s| #0=#:G1522 |me| |two53|)
    (RETURN
      (SEQ (EXIT (COND
                   ((ZEROP |x|) (CONS 0 0))
                   ('T
                    (SEQ (LETT |s|
                               (SPADCALL |x| (|getShellEntry| $ 117))
                               |DFLOAT;manexp|)
                         (LETT |x| (FLOAT-SIGN 1.0 |x|)
                               |DFLOAT;manexp|)
                         (COND
                           ((< |$DoubleFloatMaximum| |x|)
                            (PROGN
                              (LETT #0#
                                    (CONS
                                     (+
                                      (* |s|
                                       (SPADCALL |$DoubleFloatMaximum|
                                        (|getShellEntry| $ 27)))
                                      1)
                                     (SPADCALL |$DoubleFloatMaximum|
                                      (|getShellEntry| $ 28)))
                                    |DFLOAT;manexp|)
                              (GO #0#))))
                         (LETT |me| (MANEXP |x|) |DFLOAT;manexp|)
                         (LETT |two53|
                               (SPADCALL (FLOAT-RADIX 0.0)
                                   (FLOAT-DIGITS 0.0)
                                   (|getShellEntry| $ 119))
                               |DFLOAT;manexp|)
                         (EXIT (CONS (* |s|
                                      (FIX (* |two53| (QCAR |me|))))
                                     (- (QCDR |me|) (FLOAT-DIGITS 0.0))))))))
           #0# (EXIT #0#))))) 

(DEFUN |DFLOAT;rationalApproximation;$2NniF;84| (|f| |d| |b| $)
  (PROG (|#G103| |nu| |ex| BASE #0=#:G1524 |de| |tol| |#G104| |q| |r|
                 |p2| |q2| #1=#:G1540 |#G105| |#G106| |p0| |p1| |#G107|
                 |#G108| |q0| |q1| |#G109| |#G110| |s| |t| #2=#:G1538)
    (RETURN
      (SEQ (EXIT (SEQ (PROGN
                        (LETT |#G103| (|DFLOAT;manexp| |f| $)
                              |DFLOAT;rationalApproximation;$2NniF;84|)
                        (LETT |nu| (QCAR |#G103|)
                              |DFLOAT;rationalApproximation;$2NniF;84|)
                        (LETT |ex| (QCDR |#G103|)
                              |DFLOAT;rationalApproximation;$2NniF;84|)
                        |#G103|)
                      (LETT BASE (FLOAT-RADIX 0.0)
                            |DFLOAT;rationalApproximation;$2NniF;84|)
                      (EXIT (COND
                              ((< |ex| 0)
                               (SEQ (LETT |de|
                                     (EXPT BASE
                                      (PROG1
                                       (LETT #0# (- |ex|)
                                        |DFLOAT;rationalApproximation;$2NniF;84|)
                                        (|check-subtype| (>= #0# 0)
                                         '(|NonNegativeInteger|) #0#)))
                                     |DFLOAT;rationalApproximation;$2NniF;84|)
                                    (EXIT
                                     (COND
                                       ((< |b| 2)
                                        (|error| "base must be > 1"))
                                       ('T
                                        (SEQ
                                         (LETT |tol| (EXPT |b| |d|)
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (LETT |s| |nu|
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (LETT |t| |de|
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (LETT |p0| 0
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (LETT |p1| 1
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (LETT |q0| 1
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (LETT |q1| 0
                                          |DFLOAT;rationalApproximation;$2NniF;84|)
                                         (EXIT
                                          (SEQ G190 NIL
                                           (SEQ
                                            (PROGN
                                              (LETT |#G104|
                                               (DIVIDE2 |s| |t|)
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |q| (QCAR |#G104|)
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |r| (QCDR |#G104|)
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              |#G104|)
                                            (LETT |p2|
                                             (+ (* |q| |p1|) |p0|)
                                             |DFLOAT;rationalApproximation;$2NniF;84|)
                                            (LETT |q2|
                                             (+ (* |q| |q1|) |q0|)
                                             |DFLOAT;rationalApproximation;$2NniF;84|)
                                            (COND
                                              ((OR (EQL |r| 0)
                                                (<
                                                 (SPADCALL |tol|
                                                  (ABS
                                                   (- (* |nu| |q2|)
                                                    (* |de| |p2|)))
                                                  (|getShellEntry| $
                                                   122))
                                                 (* |de| (ABS |p2|))))
                                               (EXIT
                                                (PROGN
                                                  (LETT #1#
                                                   (SPADCALL |p2| |q2|
                                                    (|getShellEntry| $
                                                     121))
                                                   |DFLOAT;rationalApproximation;$2NniF;84|)
                                                  (GO #1#)))))
                                            (PROGN
                                              (LETT |#G105| |p1|
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |#G106| |p2|
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |p0| |#G105|
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |p1| |#G106|
                                               |DFLOAT;rationalApproximation;$2NniF;84|))
                                            (PROGN
                                              (LETT |#G107| |q1|
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |#G108| |q2|
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |q0| |#G107|
                                               |DFLOAT;rationalApproximation;$2NniF;84|)
                                              (LETT |q1| |#G108|
                                               |DFLOAT;rationalApproximation;$2NniF;84|))
                                            (EXIT
                                             (PROGN
                                               (LETT |#G109| |t|
                                                |DFLOAT;rationalApproximation;$2NniF;84|)
                                               (LETT |#G110| |r|
                                                |DFLOAT;rationalApproximation;$2NniF;84|)
                                               (LETT |s| |#G109|
                                                |DFLOAT;rationalApproximation;$2NniF;84|)
                                               (LETT |t| |#G110|
                                                |DFLOAT;rationalApproximation;$2NniF;84|))))
                                           NIL (GO G190) G191
                                           (EXIT NIL)))))))))
                              ('T
                               (SPADCALL
                                   (* |nu|
                                    (EXPT BASE
                                     (PROG1
                                      (LETT #2# |ex|
                                       |DFLOAT;rationalApproximation;$2NniF;84|)
                                       (|check-subtype| (>= #2# 0)
                                        '(|NonNegativeInteger|) #2#))))
                                   (|getShellEntry| $ 123)))))))
           #1# (EXIT #1#))))) 

(DEFUN |DFLOAT;**;$F$;85| (|x| |r| $)
  (PROG (|n| |d| #0=#:G1549)
    (RETURN
      (SEQ (EXIT (COND
                   ((ZEROP |x|)
                    (COND
                      ((SPADCALL |r| (|getShellEntry| $ 124))
                       (|error| "0**0 is undefined"))
                      ((SPADCALL |r| (|getShellEntry| $ 125))
                       (|error| "division by 0"))
                      ('T 0.0)))
                   ((OR (SPADCALL |r| (|getShellEntry| $ 124))
                        (= |x| 1.0))
                    1.0)
                   ('T
                    (COND
                      ((SPADCALL |r| (|spadConstant| $ 126)
                           (|getShellEntry| $ 127))
                       |x|)
                      ('T
                       (SEQ (LETT |n|
                                  (SPADCALL |r|
                                      (|getShellEntry| $ 128))
                                  |DFLOAT;**;$F$;85|)
                            (LETT |d|
                                  (SPADCALL |r|
                                      (|getShellEntry| $ 129))
                                  |DFLOAT;**;$F$;85|)
                            (EXIT (COND
                                    ((MINUSP |x|)
                                     (COND
                                       ((ODDP |d|)
                                        (COND
                                          ((ODDP |n|)
                                           (PROGN
                                             (LETT #0#
                                              (-
                                               (SPADCALL (- |x|) |r|
                                                (|getShellEntry| $ 130)))
                                              |DFLOAT;**;$F$;85|)
                                             (GO #0#)))
                                          ('T
                                           (PROGN
                                             (LETT #0#
                                              (SPADCALL (- |x|) |r|
                                               (|getShellEntry| $ 130))
                                              |DFLOAT;**;$F$;85|)
                                             (GO #0#)))))
                                       ('T (|error| "negative root"))))
                                    ((EQL |d| 2)
                                     (EXPT
                                      (SPADCALL |x|
                                       (|getShellEntry| $ 56))
                                      |n|))
                                    ('T
                                     (SPADCALL |x|
                                      (/
                                       (FLOAT |n|
                                        |$DoubleFloatMaximum|)
                                       (FLOAT |d|
                                        |$DoubleFloatMaximum|))
                                      (|getShellEntry| $ 59)))))))))))
           #0# (EXIT #0#))))) 

(DEFUN |DoubleFloat| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1562)
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
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|DoubleFloat|) . #0=(|DoubleFloat|))
        (LETT $ (|newShell| 143) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|DoubleFloat| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 "~G")
        $)))) 

(MAKEPROP '|DoubleFloat| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL '|format| (|String|)
             |DFLOAT;doubleFloatFormat;2S;1| (|OpenMathEncoding|)
             (0 . |OMencodingXML|) (|OpenMathDevice|)
             (4 . |OMopenString|) (|Void|) (10 . |OMputObject|)
             (|DoubleFloat|) (15 . |OMputFloat|)
             (21 . |OMputEndObject|) (26 . |OMclose|)
             |DFLOAT;OMwrite;$S;2| (|Boolean|) |DFLOAT;OMwrite;$BS;3|
             |DFLOAT;OMwrite;Omd$V;4| |DFLOAT;OMwrite;Omd$BV;5|
             (|PositiveInteger|) |DFLOAT;base;Pi;7| (|Integer|)
             |DFLOAT;mantissa;$I;8| |DFLOAT;exponent;$I;9|
             |DFLOAT;precision;Pi;10| |DFLOAT;log2;2$;38| (31 . *)
             |DFLOAT;bits;Pi;11| |DFLOAT;max;$;12| |DFLOAT;min;$;13|
             |DFLOAT;order;$I;14|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |DFLOAT;Zero;$;15|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |DFLOAT;One;$;16|) $))
             |DFLOAT;exp1;$;17| |DFLOAT;pi;$;18| (|OutputForm|)
             (37 . |outputForm|) |DFLOAT;coerce;$Of;19| (|InputForm|)
             (42 . |convert|) |DFLOAT;convert;$If;20| |DFLOAT;<;2$B;21|
             |DFLOAT;-;2$;22| |DFLOAT;+;3$;23| |DFLOAT;-;3$;24|
             |DFLOAT;*;3$;25| |DFLOAT;*;I2$;26| |DFLOAT;max;3$;27|
             |DFLOAT;min;3$;28| |DFLOAT;=;2$B;29| |DFLOAT;/;$I$;30|
             |DFLOAT;sqrt;2$;31| |DFLOAT;log10;2$;32|
             |DFLOAT;**;$I$;33| |DFLOAT;**;3$;34| |DFLOAT;coerce;I$;35|
             |DFLOAT;exp;2$;36| |DFLOAT;log;2$;37| |DFLOAT;sin;2$;39|
             |DFLOAT;cos;2$;40| |DFLOAT;tan;2$;41| |DFLOAT;cot;2$;42|
             |DFLOAT;sec;2$;43| |DFLOAT;csc;2$;44| |DFLOAT;asin;2$;45|
             |DFLOAT;acos;2$;46| |DFLOAT;atan;2$;47|
             |DFLOAT;acsc;2$;48| |DFLOAT;acot;2$;49|
             |DFLOAT;asec;2$;50| |DFLOAT;sinh;2$;51|
             |DFLOAT;cosh;2$;52| |DFLOAT;tanh;2$;53|
             |DFLOAT;csch;2$;54| |DFLOAT;coth;2$;55|
             |DFLOAT;sech;2$;56| |DFLOAT;asinh;2$;57|
             |DFLOAT;acosh;2$;58| |DFLOAT;atanh;2$;59|
             |DFLOAT;acsch;2$;60| |DFLOAT;acoth;2$;61|
             |DFLOAT;asech;2$;62| |DFLOAT;/;3$;63|
             |DFLOAT;negative?;$B;64| |DFLOAT;zero?;$B;65|
             (|SingleInteger|) |DFLOAT;hash;$Si;66|
             (|Union| $ '"failed") |DFLOAT;recip;$U;67|
             |DFLOAT;differentiate;2$;68|
             (|DoubleFloatSpecialFunctions|) (47 . |Gamma|)
             |DFLOAT;Gamma;2$;69| (52 . |Beta|) |DFLOAT;Beta;3$;70|
             |DFLOAT;wholePart;$I;71| |DFLOAT;float;2IPi$;72|
             |DFLOAT;convert;2$;73| (|Float|) (58 . |convert|)
             |DFLOAT;convert;$F;74| (|Fraction| 26)
             (|NonNegativeInteger|)
             |DFLOAT;rationalApproximation;$2NniF;84|
             |DFLOAT;rationalApproximation;$NniF;75|
             |DFLOAT;atan;3$;76| |DFLOAT;retract;$F;77|
             (|Union| 106 '"failed") |DFLOAT;retractIfCan;$U;78|
             |DFLOAT;retract;$I;79| (|Union| 26 '"failed")
             |DFLOAT;retractIfCan;$U;80| |DFLOAT;sign;$I;81|
             |DFLOAT;abs;2$;82| (63 . **) (69 . |Zero|) (73 . /)
             (79 . *) (85 . |coerce|) (90 . |zero?|) (95 . |negative?|)
             (100 . |One|) (104 . =) (110 . |numer|) (115 . |denom|)
             |DFLOAT;**;$F$;85| (|PatternMatchResult| 103 $)
             (|Pattern| 103) (|Factored| $)
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 134 '"failed") (|List| $) (|Union| 136 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (|SparseUnivariatePolynomial| $)
             (|Record| (|:| |coef| 136) (|:| |generator| $))
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $)))
          '#(~= 120 |zero?| 126 |wholePart| 131 |unitNormal| 136
             |unitCanonical| 141 |unit?| 146 |truncate| 151 |tanh| 156
             |tan| 161 |subtractIfCan| 166 |squareFreePart| 172
             |squareFree| 177 |sqrt| 182 |sizeLess?| 187 |sinh| 193
             |sin| 198 |sign| 203 |sech| 208 |sec| 213 |sample| 218
             |round| 222 |retractIfCan| 227 |retract| 237 |rem| 247
             |recip| 253 |rationalApproximation| 258 |quo| 271
             |principalIdeal| 277 |prime?| 282 |precision| 287
             |positive?| 291 |pi| 296 |patternMatch| 300 |order| 307
             |one?| 312 |nthRoot| 317 |norm| 323 |negative?| 328
             |multiEuclidean| 333 |min| 339 |max| 349 |mantissa| 359
             |log2| 364 |log10| 369 |log| 374 |lcm| 379 |latex| 390
             |inv| 395 |hash| 400 |gcdPolynomial| 405 |gcd| 411
             |fractionPart| 422 |floor| 427 |float| 432 |factor| 445
             |extendedEuclidean| 450 |exquo| 463 |expressIdealMember|
             469 |exponent| 475 |exp1| 480 |exp| 484 |euclideanSize|
             489 |doubleFloatFormat| 494 |divide| 499 |digits| 505
             |differentiate| 509 |csch| 520 |csc| 525 |coth| 530 |cot|
             535 |cosh| 540 |cos| 545 |convert| 550 |coerce| 570
             |characteristic| 600 |ceiling| 604 |bits| 609 |base| 613
             |atanh| 617 |atan| 622 |associates?| 633 |asinh| 639
             |asin| 644 |asech| 649 |asec| 654 |acsch| 659 |acsc| 664
             |acoth| 669 |acot| 674 |acosh| 679 |acos| 684 |abs| 689
             |Zero| 694 |One| 698 |OMwrite| 702 |Gamma| 726 D 731
             |Beta| 742 >= 748 > 754 = 760 <= 766 < 772 / 778 - 790 +
             801 ** 807 * 837)
          '((|approximate| . 0) (|canonicalsClosed| . 0)
            (|canonicalUnitNormal| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0))
                (CONS '#(|FloatingPointSystem&| |RealNumberSystem&|
                         |Field&| |EuclideanDomain&| NIL
                         |UniqueFactorizationDomain&| |GcdDomain&|
                         |DivisionRing&| |IntegralDomain&| |Algebra&|
                         |Algebra&| |DifferentialRing&| NIL
                         |OrderedRing&| |Module&| NIL NIL |Module&| NIL
                         NIL |Ring&| NIL NIL NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL |AbelianMonoid&|
                         |Monoid&| NIL |OrderedSet&|
                         |AbelianSemiGroup&| |SemiGroup&|
                         |TranscendentalFunctionCategory&| NIL
                         |SetCategory&| NIL
                         |ElementaryFunctionCategory&| NIL
                         |HyperbolicFunctionCategory&|
                         |ArcTrigonometricFunctionCategory&|
                         |TrigonometricFunctionCategory&| NIL NIL
                         |RadicalCategory&| |RetractableTo&|
                         |RetractableTo&| NIL NIL |BasicType&| NIL)
                      (CONS '#((|FloatingPointSystem|)
                               (|RealNumberSystem|) (|Field|)
                               (|EuclideanDomain|)
                               (|PrincipalIdealDomain|)
                               (|UniqueFactorizationDomain|)
                               (|GcdDomain|) (|DivisionRing|)
                               (|IntegralDomain|) (|Algebra| 106)
                               (|Algebra| $$) (|DifferentialRing|)
                               (|CharacteristicZero|) (|OrderedRing|)
                               (|Module| 106) (|EntireRing|)
                               (|CommutativeRing|) (|Module| $$)
                               (|BiModule| 106 106) (|BiModule| $$ $$)
                               (|Ring|) (|OrderedAbelianGroup|)
                               (|RightModule| 106) (|LeftModule| 106)
                               (|LeftModule| $$) (|Rng|)
                               (|RightModule| $$)
                               (|OrderedCancellationAbelianMonoid|)
                               (|AbelianGroup|)
                               (|OrderedAbelianMonoid|)
                               (|CancellationAbelianMonoid|)
                               (|OrderedAbelianSemiGroup|)
                               (|AbelianMonoid|) (|Monoid|)
                               (|PatternMatchable| 103) (|OrderedSet|)
                               (|AbelianSemiGroup|) (|SemiGroup|)
                               (|TranscendentalFunctionCategory|)
                               (|RealConstant|) (|SetCategory|)
                               (|ConvertibleTo| 43)
                               (|ElementaryFunctionCategory|)
                               (|ArcHyperbolicFunctionCategory|)
                               (|HyperbolicFunctionCategory|)
                               (|ArcTrigonometricFunctionCategory|)
                               (|TrigonometricFunctionCategory|)
                               (|OpenMath|) (|ConvertibleTo| 132)
                               (|RadicalCategory|)
                               (|RetractableTo| 106)
                               (|RetractableTo| 26)
                               (|ConvertibleTo| 103)
                               (|ConvertibleTo| 15) (|BasicType|)
                               (|CoercibleTo| 40))
                            (|makeByteWordVec2| 142
                                '(0 9 0 10 2 11 0 7 9 12 1 11 13 0 14 2
                                  11 13 0 15 16 1 11 13 0 17 1 11 13 0
                                  18 2 0 0 24 0 31 1 40 0 15 41 1 43 0
                                  15 44 1 95 15 15 96 2 95 15 15 15 98
                                  1 103 0 15 104 2 26 0 0 24 119 0 106
                                  0 120 2 106 0 26 26 121 2 26 0 107 0
                                  122 1 106 0 26 123 1 106 20 0 124 1
                                  106 20 0 125 0 106 0 126 2 106 20 0 0
                                  127 1 106 26 0 128 1 106 26 0 129 2 0
                                  20 0 0 1 1 0 20 0 89 1 0 26 0 100 1 0
                                  142 0 1 1 0 0 0 1 1 0 20 0 1 1 0 0 0
                                  1 1 0 0 0 77 1 0 0 0 65 2 0 92 0 0 1
                                  1 0 0 0 1 1 0 133 0 1 1 0 0 0 56 2 0
                                  20 0 0 1 1 0 0 0 75 1 0 0 0 63 1 0 26
                                  0 117 1 0 0 0 80 1 0 0 0 67 0 0 0 1 1
                                  0 0 0 1 1 0 112 0 113 1 0 115 0 116 1
                                  0 106 0 111 1 0 26 0 114 2 0 0 0 0 1
                                  1 0 92 0 93 2 0 106 0 107 109 3 0 106
                                  0 107 107 108 2 0 0 0 0 1 1 0 141 136
                                  1 1 0 20 0 1 0 0 24 29 1 0 20 0 1 0 0
                                  0 39 3 0 131 0 132 131 1 1 0 26 0 35
                                  1 0 20 0 1 2 0 0 0 26 1 1 0 0 0 1 1 0
                                  20 0 88 2 0 137 136 0 1 0 0 0 34 2 0
                                  0 0 0 53 0 0 0 33 2 0 0 0 0 52 1 0 26
                                  0 27 1 0 0 0 30 1 0 0 0 57 1 0 0 0 62
                                  2 0 0 0 0 1 1 0 0 136 1 1 0 7 0 1 1 0
                                  0 0 1 1 0 90 0 91 2 0 140 140 140 1 1
                                  0 0 136 1 2 0 0 0 0 1 1 0 0 0 1 1 0 0
                                  0 1 2 0 0 26 26 1 3 0 0 26 26 24 101
                                  1 0 133 0 1 3 0 135 0 0 0 1 2 0 138 0
                                  0 1 2 0 92 0 0 1 2 0 137 136 0 1 1 0
                                  26 0 28 0 0 0 38 1 0 0 0 61 1 0 107 0
                                  1 1 0 7 7 8 2 0 139 0 0 1 0 0 24 1 1
                                  0 0 0 94 2 0 0 0 107 1 1 0 0 0 78 1 0
                                  0 0 68 1 0 0 0 79 1 0 0 0 66 1 0 0 0
                                  76 1 0 0 0 64 1 0 43 0 45 1 0 132 0 1
                                  1 0 15 0 102 1 0 103 0 105 1 0 0 106
                                  1 1 0 0 26 60 1 0 0 106 1 1 0 0 26 60
                                  1 0 0 0 1 1 0 40 0 42 0 0 107 1 1 0 0
                                  0 1 0 0 24 32 0 0 24 25 1 0 0 0 83 2
                                  0 0 0 0 110 1 0 0 0 71 2 0 20 0 0 1 1
                                  0 0 0 81 1 0 0 0 69 1 0 0 0 86 1 0 0
                                  0 74 1 0 0 0 84 1 0 0 0 72 1 0 0 0 85
                                  1 0 0 0 73 1 0 0 0 82 1 0 0 0 70 1 0
                                  0 0 118 0 0 0 36 0 0 0 37 3 0 13 11 0
                                  20 23 2 0 7 0 20 21 2 0 13 11 0 22 1
                                  0 7 0 19 1 0 0 0 97 1 0 0 0 1 2 0 0 0
                                  107 1 2 0 0 0 0 99 2 0 20 0 0 1 2 0
                                  20 0 0 1 2 0 20 0 0 54 2 0 20 0 0 1 2
                                  0 20 0 0 46 2 0 0 0 26 55 2 0 0 0 0
                                  87 2 0 0 0 0 49 1 0 0 0 47 2 0 0 0 0
                                  48 2 0 0 0 0 59 2 0 0 0 106 130 2 0 0
                                  0 26 58 2 0 0 0 107 1 2 0 0 0 24 1 2
                                  0 0 0 106 1 2 0 0 106 0 1 2 0 0 0 0
                                  50 2 0 0 26 0 51 2 0 0 107 0 1 2 0 0
                                  24 0 31)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|DoubleFloat| '|isFunctor|
             '(((|rationalApproximation|
                    ((|Fraction| (|Integer|)) $ (|NonNegativeInteger|)
                     (|NonNegativeInteger|)))
                T (ELT $ 108))
               ((|rationalApproximation|
                    ((|Fraction| (|Integer|)) $ (|NonNegativeInteger|)))
                T (ELT $ 109))
               ((|doubleFloatFormat| ((|String|) (|String|))) T
                (ELT $ 8))
               ((|Beta| ($ $ $)) T (ELT $ 99))
               ((|Gamma| ($ $)) T (ELT $ 97))
               ((|atan| ($ $ $)) T (ELT $ 110))
               ((|log10| ($ $)) T (ELT $ 57))
               ((|log2| ($ $)) T (ELT $ 30))
               ((|exp1| ($)) T (ELT $ 38))
               ((/ ($ $ (|Integer|))) T (ELT $ 55))
               ((|convert| ((|InputForm|) $)) T (ELT $ 45))
               ((|tan| ($ $)) T (ELT $ 65))
               ((|sin| ($ $)) T (ELT $ 63))
               ((|sec| ($ $)) T (ELT $ 67))
               ((|csc| ($ $)) T (ELT $ 68))
               ((|cot| ($ $)) T (ELT $ 66))
               ((|cos| ($ $)) T (ELT $ 64))
               ((|acos| ($ $)) T (ELT $ 70))
               ((|acot| ($ $)) T (ELT $ 73))
               ((|acsc| ($ $)) T (ELT $ 72))
               ((|asec| ($ $)) T (ELT $ 74))
               ((|asin| ($ $)) T (ELT $ 69))
               ((|atan| ($ $)) T (ELT $ 71))
               ((|cosh| ($ $)) T (ELT $ 76))
               ((|coth| ($ $)) T (ELT $ 79))
               ((|csch| ($ $)) T (ELT $ 78))
               ((|sech| ($ $)) T (ELT $ 80))
               ((|sinh| ($ $)) T (ELT $ 75))
               ((|tanh| ($ $)) T (ELT $ 77))
               ((|acosh| ($ $)) T (ELT $ 82))
               ((|acoth| ($ $)) T (ELT $ 85))
               ((|acsch| ($ $)) T (ELT $ 84))
               ((|asech| ($ $)) T (ELT $ 86))
               ((|asinh| ($ $)) T (ELT $ 81))
               ((|atanh| ($ $)) T (ELT $ 83))
               ((|log| ($ $)) T (ELT $ 62))
               ((|exp| ($ $)) T (ELT $ 61)) ((** ($ $ $)) T (ELT $ 59))
               ((|pi| ($)) T (ELT $ 39))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $ (|Boolean|)))
                T (ELT $ 23))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $)) T
                (ELT $ 22))
               ((|OMwrite| ((|String|) $ (|Boolean|))) T (ELT $ 21))
               ((|OMwrite| ((|String|) $)) T (ELT $ 19))
               ((|differentiate| ($ $)) T (ELT $ 94))
               ((D ($ $)) T (ELT $ NIL))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((D ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|max| ($))
                (AND (|not| (|has| $ (ATTRIBUTE |arbitraryExponent|)))
                     (|not| (|has| $ (ATTRIBUTE |arbitraryPrecision|))))
                (ELT $ 33))
               ((|min| ($))
                (AND (|not| (|has| $ (ATTRIBUTE |arbitraryExponent|)))
                     (|not| (|has| $ (ATTRIBUTE |arbitraryPrecision|))))
                (ELT $ 34))
               ((|decreasePrecision| ((|PositiveInteger|) (|Integer|)))
                (|has| $ (ATTRIBUTE |arbitraryPrecision|)) (ELT $ NIL))
               ((|increasePrecision| ((|PositiveInteger|) (|Integer|)))
                (|has| $ (ATTRIBUTE |arbitraryPrecision|)) (ELT $ NIL))
               ((|precision| ((|PositiveInteger|) (|PositiveInteger|)))
                (|has| $ (ATTRIBUTE |arbitraryPrecision|)) (ELT $ NIL))
               ((|digits| ((|PositiveInteger|) (|PositiveInteger|)))
                (|has| $ (ATTRIBUTE |arbitraryPrecision|)) (ELT $ NIL))
               ((|bits| ((|PositiveInteger|) (|PositiveInteger|)))
                (|has| $ (ATTRIBUTE |arbitraryPrecision|)) (ELT $ NIL))
               ((|precision| ((|PositiveInteger|))) T (ELT $ 29))
               ((|digits| ((|PositiveInteger|))) T (ELT $ NIL))
               ((|bits| ((|PositiveInteger|))) T (ELT $ 32))
               ((|mantissa| ((|Integer|) $)) T (ELT $ 27))
               ((|exponent| ((|Integer|) $)) T (ELT $ 28))
               ((|base| ((|PositiveInteger|))) T (ELT $ 25))
               ((|order| ((|Integer|) $)) T (ELT $ 35))
               ((|float| ($ (|Integer|) (|Integer|)
                            (|PositiveInteger|)))
                T (ELT $ 101))
               ((|float| ($ (|Integer|) (|Integer|))) T (ELT $ NIL))
               ((|round| ($ $)) T (ELT $ NIL))
               ((|truncate| ($ $)) T (ELT $ NIL))
               ((|fractionPart| ($ $)) T (ELT $ NIL))
               ((|wholePart| ((|Integer|) $)) T (ELT $ 100))
               ((|floor| ($ $)) T (ELT $ NIL))
               ((|ceiling| ($ $)) T (ELT $ NIL))
               ((|norm| ($ $)) T (ELT $ NIL))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Float|) $) $
                     (|Pattern| (|Float|))
                     (|PatternMatchResult| (|Float|) $)))
                T (ELT $ NIL))
               ((|convert| ((|Pattern| (|Float|)) $)) T (ELT $ NIL))
               ((** ($ $ (|Fraction| (|Integer|)))) T (ELT $ 130))
               ((|nthRoot| ($ $ (|Integer|))) T (ELT $ NIL))
               ((|sqrt| ($ $)) T (ELT $ 56))
               ((|retract| ((|Fraction| (|Integer|)) $)) T (ELT $ 111))
               ((|retractIfCan|
                    ((|Union| (|Fraction| (|Integer|)) "failed") $))
                T (ELT $ 113))
               ((|coerce| ($ (|Fraction| (|Integer|)))) T (ELT $ NIL))
               ((|retract| ((|Integer|) $)) T (ELT $ 114))
               ((|retractIfCan| ((|Union| (|Integer|) "failed") $)) T
                (ELT $ 116))
               ((|coerce| ($ (|Integer|))) T (ELT $ 60))
               ((|convert| ((|DoubleFloat|) $)) T (ELT $ 102))
               ((|convert| ((|Float|) $)) T (ELT $ 105))
               ((< ((|Boolean|) $ $)) T (ELT $ 46))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|max| ($ $ $)) T (ELT $ 52))
               ((|min| ($ $ $)) T (ELT $ 53))
               ((|positive?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|negative?| ((|Boolean|) $)) T (ELT $ 88))
               ((|sign| ((|Integer|) $)) T (ELT $ 117))
               ((|abs| ($ $)) T (ELT $ 118)) ((/ ($ $ $)) T (ELT $ 87))
               ((|coerce| ($ (|Fraction| (|Integer|)))) T (ELT $ NIL))
               ((* ($ (|Fraction| (|Integer|)) $)) T (ELT $ NIL))
               ((* ($ $ (|Fraction| (|Integer|)))) T (ELT $ NIL))
               ((** ($ $ (|Integer|))) T (ELT $ 58))
               ((|inv| ($ $)) T (ELT $ NIL))
               ((|prime?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|squareFree| ((|Factored| $) $)) T (ELT $ NIL))
               ((|squareFreePart| ($ $)) T (ELT $ NIL))
               ((|factor| ((|Factored| $) $)) T (ELT $ NIL))
               ((|multiEuclidean|
                    ((|Union| (|List| $) "failed") (|List| $) $))
                T (ELT $ NIL))
               ((|extendedEuclidean|
                    ((|Union| (|Record| (|:| |coef1| $)
                                  (|:| |coef2| $))
                              "failed")
                     $ $ $))
                T (ELT $ NIL))
               ((|extendedEuclidean|
                    ((|Record| (|:| |coef1| $) (|:| |coef2| $)
                         (|:| |generator| $))
                     $ $))
                T (ELT $ NIL))
               ((|rem| ($ $ $)) T (ELT $ NIL))
               ((|quo| ($ $ $)) T (ELT $ NIL))
               ((|divide|
                    ((|Record| (|:| |quotient| $) (|:| |remainder| $))
                     $ $))
                T (ELT $ NIL))
               ((|euclideanSize| ((|NonNegativeInteger|) $)) T
                (ELT $ NIL))
               ((|sizeLess?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|expressIdealMember|
                    ((|Union| (|List| $) "failed") (|List| $) $))
                T (ELT $ NIL))
               ((|principalIdeal|
                    ((|Record| (|:| |coef| (|List| $))
                         (|:| |generator| $))
                     (|List| $)))
                T (ELT $ NIL))
               ((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ NIL))
               ((|lcm| ($ (|List| $))) T (ELT $ NIL))
               ((|lcm| ($ $ $)) T (ELT $ NIL))
               ((|gcd| ($ (|List| $))) T (ELT $ NIL))
               ((|gcd| ($ $ $)) T (ELT $ NIL))
               ((|unit?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|associates?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|unitCanonical| ($ $)) T (ELT $ NIL))
               ((|unitNormal|
                    ((|Record| (|:| |unit| $) (|:| |canonical| $)
                         (|:| |associate| $))
                     $))
                T (ELT $ NIL))
               ((|exquo| ((|Union| $ "failed") $ $)) T (ELT $ NIL))
               ((|coerce| ($ $)) T (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 60))
               ((|characteristic| ((|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|One| ($)) T (CONST $ 37))
               ((|one?| ((|Boolean|) $)) T (ELT $ NIL))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|recip| ((|Union| $ "failed") $)) T (ELT $ 93))
               ((* ($ $ $)) T (ELT $ 50))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((* ($ (|Integer|) $)) T (ELT $ 51))
               ((- ($ $ $)) T (ELT $ 49)) ((- ($ $)) T (ELT $ 47))
               ((|subtractIfCan| ((|Union| $ "failed") $ $)) T
                (ELT $ NIL))
               ((* ($ (|NonNegativeInteger|) $)) T (ELT $ NIL))
               ((|zero?| ((|Boolean|) $)) T (ELT $ 89))
               ((|sample| ($)) T (CONST $ NIL))
               ((|Zero| ($)) T (CONST $ 36))
               ((* ($ (|PositiveInteger|) $)) T (ELT $ 31))
               ((+ ($ $ $)) T (ELT $ 48))
               ((|latex| ((|String|) $)) T (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ 91))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 42))
               ((= ((|Boolean|) $ $)) T (ELT $ 54))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|DoubleFloat| '(|DoubleFloat|)
                 '((|Join| (|FloatingPointSystem|) (|DifferentialRing|)
                           (|OpenMath|)
                           (|TranscendentalFunctionCategory|)
                           (|ConvertibleTo| (|InputForm|))
                           (CATEGORY |domain|
                               (SIGNATURE / ($ $ (|Integer|)))
                               (SIGNATURE ** ($ $ $))
                               (SIGNATURE |exp1| ($))
                               (SIGNATURE |log2| ($ $))
                               (SIGNATURE |log10| ($ $))
                               (SIGNATURE |atan| ($ $ $))
                               (SIGNATURE |Gamma| ($ $))
                               (SIGNATURE |Beta| ($ $ $))
                               (SIGNATURE |doubleFloatFormat|
                                   ((|String|) (|String|)))
                               (SIGNATURE |rationalApproximation|
                                   ((|Fraction| (|Integer|)) $
                                    (|NonNegativeInteger|)))
                               (SIGNATURE |rationalApproximation|
                                   ((|Fraction| (|Integer|)) $
                                    (|NonNegativeInteger|)
                                    (|NonNegativeInteger|))))))
                 T '|DoubleFloat|
                 (|put| '|DoubleFloat| '|mode|
                        '(|Mapping|
                             (|Join| (|FloatingPointSystem|)
                                     (|DifferentialRing|) (|OpenMath|)
                                     (|TranscendentalFunctionCategory|)
                                     (|ConvertibleTo| (|InputForm|))
                                     (CATEGORY |domain|
                                      (SIGNATURE / ($ $ (|Integer|)))
                                      (SIGNATURE ** ($ $ $))
                                      (SIGNATURE |exp1| ($))
                                      (SIGNATURE |log2| ($ $))
                                      (SIGNATURE |log10| ($ $))
                                      (SIGNATURE |atan| ($ $ $))
                                      (SIGNATURE |Gamma| ($ $))
                                      (SIGNATURE |Beta| ($ $ $))
                                      (SIGNATURE |doubleFloatFormat|
                                       ((|String|) (|String|)))
                                      (SIGNATURE
                                       |rationalApproximation|
                                       ((|Fraction| (|Integer|)) $
                                        (|NonNegativeInteger|)))
                                      (SIGNATURE
                                       |rationalApproximation|
                                       ((|Fraction| (|Integer|)) $
                                        (|NonNegativeInteger|)
                                        (|NonNegativeInteger|))))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|DoubleFloat| 'NILADIC T) 
