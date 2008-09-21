
(/VERSIONCHECK 2) 

(DEFUN |INT;writeOMInt| (|dev| |x| $)
  (SEQ (COND
         ((< |x| 0)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 8))
               (SPADCALL |dev| "arith1" "unary_minus"
                   (|getShellEntry| $ 10))
               (SPADCALL |dev| (- |x|) (|getShellEntry| $ 12))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 13)))))
         ('T (SPADCALL |dev| |x| (|getShellEntry| $ 12)))))) 

(DEFUN |INT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |INT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |INT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 15))
                     (|getShellEntry| $ 16))
                 |INT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 17))
           (|INT;writeOMInt| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 18))
           (SPADCALL |dev| (|getShellEntry| $ 19))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |INT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |INT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |INT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |INT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 15))
                     (|getShellEntry| $ 16))
                 |INT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 17))))
           (|INT;writeOMInt| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))
           (SPADCALL |dev| (|getShellEntry| $ 19))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |INT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |INT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 17))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 18))))) 

(DEFUN |INT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 17))))
       (|INT;writeOMInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))))) 

(PUT '|INT;zero?;$B;6| '|SPADreplace| 'ZEROP) 

(DEFUN |INT;zero?;$B;6| (|x| $) (ZEROP |x|)) 

(PUT '|INT;one?;$B;7| '|SPADreplace| '(XLAM (|x|) (EQL |x| 1))) 

(DEFUN |INT;one?;$B;7| (|x| $) (EQL |x| 1)) 

(PUT '|INT;Zero;$;8| '|SPADreplace| '(XLAM NIL 0)) 

(DEFUN |INT;Zero;$;8| ($) 0) 

(PUT '|INT;One;$;9| '|SPADreplace| '(XLAM NIL 1)) 

(DEFUN |INT;One;$;9| ($) 1) 

(PUT '|INT;base;$;10| '|SPADreplace| '(XLAM NIL 2)) 

(DEFUN |INT;base;$;10| ($) 2) 

(PUT '|INT;copy;2$;11| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |INT;copy;2$;11| (|x| $) |x|) 

(PUT '|INT;inc;2$;12| '|SPADreplace| '(XLAM (|x|) (+ |x| 1))) 

(DEFUN |INT;inc;2$;12| (|x| $) (+ |x| 1)) 

(PUT '|INT;dec;2$;13| '|SPADreplace| '(XLAM (|x|) (- |x| 1))) 

(DEFUN |INT;dec;2$;13| (|x| $) (- |x| 1)) 

(PUT '|INT;hash;$Si;14| '|SPADreplace| 'SXHASH) 

(DEFUN |INT;hash;$Si;14| (|x| $) (SXHASH |x|)) 

(PUT '|INT;negative?;$B;15| '|SPADreplace| 'MINUSP) 

(DEFUN |INT;negative?;$B;15| (|x| $) (MINUSP |x|)) 

(DEFUN |INT;coerce;$Of;16| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 37))) 

(PUT '|INT;coerce;2$;17| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DEFUN |INT;coerce;2$;17| (|m| $) |m|) 

(PUT '|INT;convert;2$;18| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |INT;convert;2$;18| (|x| $) |x|) 

(PUT '|INT;length;2$;19| '|SPADreplace| 'INTEGER-LENGTH) 

(DEFUN |INT;length;2$;19| (|a| $) (INTEGER-LENGTH |a|)) 

(DEFUN |INT;addmod;4$;20| (|a| |b| |p| $)
  (PROG (|c| #0=#:G1429)
    (RETURN
      (SEQ (EXIT (SEQ (SEQ (LETT |c| (+ |a| |b|) |INT;addmod;4$;20|)
                           (EXIT (COND
                                   ((NULL (< |c| |p|))
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
  (SPADCALL |x| (|getShellEntry| $ 46))) 

(PUT '|INT;convert;$Df;24| '|SPADreplace|
     '(XLAM (|x|) (FLOAT |x| |$DoubleFloatMaximum|))) 

(DEFUN |INT;convert;$Df;24| (|x| $) (FLOAT |x| |$DoubleFloatMaximum|)) 

(DEFUN |INT;convert;$If;25| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 51))) 

(PUT '|INT;convert;$S;26| '|SPADreplace| 'STRINGIMAGE) 

(DEFUN |INT;convert;$S;26| (|x| $) (STRINGIMAGE |x|)) 

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

(PUT '|INT;reducedSystem;2M;29| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DEFUN |INT;reducedSystem;2M;29| (|m| $) |m|) 

(DEFUN |INT;reducedSystem;MVR;30| (|m| |v| $) (CONS |m| '|vec|)) 

(PUT '|INT;abs;2$;31| '|SPADreplace| 'ABS) 

(DEFUN |INT;abs;2$;31| (|x| $) (ABS |x|)) 

(PUT '|INT;random;$;32| '|SPADreplace| '|random|) 

(DEFUN |INT;random;$;32| ($) (|random|)) 

(PUT '|INT;random;2$;33| '|SPADreplace| 'RANDOM) 

(DEFUN |INT;random;2$;33| (|x| $) (RANDOM |x|)) 

(PUT '|INT;=;2$B;34| '|SPADreplace| 'EQL) 

(DEFUN |INT;=;2$B;34| (|x| |y| $) (EQL |x| |y|)) 

(PUT '|INT;<;2$B;35| '|SPADreplace| '<) 

(DEFUN |INT;<;2$B;35| (|x| |y| $) (< |x| |y|)) 

(PUT '|INT;-;2$;36| '|SPADreplace| '-) 

(DEFUN |INT;-;2$;36| (|x| $) (- |x|)) 

(PUT '|INT;+;3$;37| '|SPADreplace| '+) 

(DEFUN |INT;+;3$;37| (|x| |y| $) (+ |x| |y|)) 

(PUT '|INT;-;3$;38| '|SPADreplace| '-) 

(DEFUN |INT;-;3$;38| (|x| |y| $) (- |x| |y|)) 

(PUT '|INT;*;3$;39| '|SPADreplace| '*) 

(DEFUN |INT;*;3$;39| (|x| |y| $) (* |x| |y|)) 

(PUT '|INT;*;3$;40| '|SPADreplace| '*) 

(DEFUN |INT;*;3$;40| (|m| |y| $) (* |m| |y|)) 

(PUT '|INT;**;$Nni$;41| '|SPADreplace| 'EXPT) 

(DEFUN |INT;**;$Nni$;41| (|x| |n| $) (EXPT |x| |n|)) 

(PUT '|INT;odd?;$B;42| '|SPADreplace| 'ODDP) 

(DEFUN |INT;odd?;$B;42| (|x| $) (ODDP |x|)) 

(PUT '|INT;max;3$;43| '|SPADreplace| 'MAX) 

(DEFUN |INT;max;3$;43| (|x| |y| $) (MAX |x| |y|)) 

(PUT '|INT;min;3$;44| '|SPADreplace| 'MIN) 

(DEFUN |INT;min;3$;44| (|x| |y| $) (MIN |x| |y|)) 

(PUT '|INT;divide;2$R;45| '|SPADreplace| 'DIVIDE2) 

(DEFUN |INT;divide;2$R;45| (|x| |y| $) (DIVIDE2 |x| |y|)) 

(PUT '|INT;quo;3$;46| '|SPADreplace| 'QUOTIENT2) 

(DEFUN |INT;quo;3$;46| (|x| |y| $) (QUOTIENT2 |x| |y|)) 

(PUT '|INT;rem;3$;47| '|SPADreplace| 'REMAINDER2) 

(DEFUN |INT;rem;3$;47| (|x| |y| $) (REMAINDER2 |x| |y|)) 

(PUT '|INT;shift;3$;48| '|SPADreplace| 'ASH) 

(DEFUN |INT;shift;3$;48| (|x| |y| $) (ASH |x| |y|)) 

(DEFUN |INT;exquo;2$U;49| (|x| |y| $)
  (COND
    ((OR (ZEROP |y|) (NULL (ZEROP (REMAINDER2 |x| |y|))))
     (CONS 1 "failed"))
    ('T (CONS 0 (QUOTIENT2 |x| |y|))))) 

(DEFUN |INT;recip;$U;50| (|x| $)
  (COND
    ((OR (EQL |x| 1) (EQL |x| -1)) (CONS 0 |x|))
    ('T (CONS 1 "failed")))) 

(PUT '|INT;gcd;3$;51| '|SPADreplace| 'GCD) 

(DEFUN |INT;gcd;3$;51| (|x| |y| $) (GCD |x| |y|)) 

(DEFUN |INT;unitNormal;$R;52| (|x| $)
  (COND ((< |x| 0) (VECTOR -1 (- |x|) -1)) ('T (VECTOR 1 |x| 1)))) 

(PUT '|INT;unitCanonical;2$;53| '|SPADreplace| 'ABS) 

(DEFUN |INT;unitCanonical;2$;53| (|x| $) (ABS |x|)) 

(DEFUN |INT;solveLinearPolynomialEquation| (|lp| |p| $)
  (SPADCALL |lp| |p| (|getShellEntry| $ 94))) 

(DEFUN |INT;squareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 98))) 

(DEFUN |INT;factorPolynomial| (|p| $)
  (PROG (|pp| #0=#:G1500)
    (RETURN
      (SEQ (LETT |pp| (SPADCALL |p| (|getShellEntry| $ 99))
                 |INT;factorPolynomial|)
           (EXIT (COND
                   ((EQL (SPADCALL |pp| (|getShellEntry| $ 100))
                         (SPADCALL |p| (|getShellEntry| $ 100)))
                    (SPADCALL |p| (|getShellEntry| $ 102)))
                   ('T
                    (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 102))
                        (SPADCALL (CONS #'|INT;factorPolynomial!0| $)
                            (SPADCALL
                                (PROG2 (LETT #0#
                                        (SPADCALL
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 100))
                                         (SPADCALL |pp|
                                          (|getShellEntry| $ 100))
                                         (|getShellEntry| $ 84))
                                        |INT;factorPolynomial|)
                                       (QCDR #0#)
                                  (|check-union| (QEQCAR #0# 0) $ #0#))
                                (|getShellEntry| $ 105))
                            (|getShellEntry| $ 109))
                        (|getShellEntry| $ 111))))))))) 

(DEFUN |INT;factorPolynomial!0| (|#1| $)
  (SPADCALL |#1| (|getShellEntry| $ 103))) 

(DEFUN |INT;factorSquareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 112))) 

(DEFUN |INT;gcdPolynomial;3Sup;58| (|p| |q| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 113))
     (SPADCALL |q| (|getShellEntry| $ 114)))
    ((SPADCALL |q| (|getShellEntry| $ 113))
     (SPADCALL |p| (|getShellEntry| $ 114)))
    ('T (SPADCALL (LIST |p| |q|) (|getShellEntry| $ 117))))) 

(DEFUN |Integer| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1525)
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
        (LETT $ (|newShell| 132) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Integer| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 72
            (|setShellEntry| $ 71
                (CONS (|dispatchFunction| |INT;*;3$;40|) $)))
        $)))) 

(MAKEPROP '|Integer| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Void|) (|OpenMathDevice|)
             (0 . |OMputApp|) (|String|) (5 . |OMputSymbol|)
             (|Integer|) (12 . |OMputInteger|) (18 . |OMputEndApp|)
             (|OpenMathEncoding|) (23 . |OMencodingXML|)
             (27 . |OMopenString|) (33 . |OMputObject|)
             (38 . |OMputEndObject|) (43 . |OMclose|)
             |INT;OMwrite;$S;2| (|Boolean|) |INT;OMwrite;$BS;3|
             |INT;OMwrite;Omd$V;4| |INT;OMwrite;Omd$BV;5|
             |INT;zero?;$B;6| |INT;one?;$B;7|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |INT;Zero;$;8|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |INT;One;$;9|) $))
             |INT;base;$;10| |INT;copy;2$;11| |INT;inc;2$;12|
             |INT;dec;2$;13| (|SingleInteger|) |INT;hash;$Si;14|
             |INT;negative?;$B;15| (|OutputForm|) (48 . |outputForm|)
             |INT;coerce;$Of;16| |INT;coerce;2$;17| |INT;convert;2$;18|
             |INT;length;2$;19| |INT;addmod;4$;20| |INT;submod;4$;21|
             |INT;mulmod;4$;22| (|Float|) (53 . |coerce|)
             |INT;convert;$F;23| (|DoubleFloat|) |INT;convert;$Df;24|
             (|InputForm|) (58 . |convert|) |INT;convert;$If;25|
             |INT;convert;$S;26| |INT;latex;$S;27|
             |INT;positiveRemainder;3$;28| (|Matrix| 11) (|Matrix| $)
             |INT;reducedSystem;2M;29| (|Vector| 11)
             (|Record| (|:| |mat| 56) (|:| |vec| 59)) (|Vector| $)
             |INT;reducedSystem;MVR;30| |INT;abs;2$;31|
             |INT;random;$;32| |INT;random;2$;33| |INT;=;2$B;34|
             |INT;<;2$B;35| |INT;-;2$;36| |INT;+;3$;37| |INT;-;3$;38|
             NIL NIL (|NonNegativeInteger|) |INT;**;$Nni$;41|
             |INT;odd?;$B;42| |INT;max;3$;43| |INT;min;3$;44|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |INT;divide;2$R;45| |INT;quo;3$;46| |INT;rem;3$;47|
             |INT;shift;3$;48| (|Union| $ '"failed") |INT;exquo;2$U;49|
             |INT;recip;$U;50| |INT;gcd;3$;51|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |INT;unitNormal;$R;52| |INT;unitCanonical;2$;53|
             (|SparseUnivariatePolynomial| 11) (|List| 90)
             (|Union| 91 '"failed")
             (|IntegerSolveLinearPolynomialEquation|)
             (63 . |solveLinearPolynomialEquation|)
             (|SparseUnivariatePolynomial| $$) (|Factored| 95)
             (|UnivariatePolynomialSquareFree| $$ 95)
             (69 . |squareFree|) (74 . |primitivePart|)
             (79 . |leadingCoefficient|) (|GaloisGroupFactorizer| 95)
             (84 . |factor|) (89 . |coerce|) (|Factored| $)
             (94 . |factor|) (|Mapping| 95 $$) (|Factored| $$)
             (|FactoredFunctions2| $$ 95) (99 . |map|)
             (|FactoredFunctionUtilities| 95) (105 . |mergeFactors|)
             (111 . |factorSquareFree|) (116 . |zero?|)
             (121 . |unitCanonical|) (|List| 95) (|HeuGcd| 95)
             (126 . |gcd|) (|SparseUnivariatePolynomial| $)
             |INT;gcdPolynomial;3Sup;58| (|Fraction| 11)
             (|Union| 120 '"failed") (|PatternMatchResult| 11 $)
             (|Pattern| 11) (|Union| 11 '"failed") (|List| $)
             (|Record| (|:| |coef| 125) (|:| |generator| $))
             (|Union| 125 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 129 '"failed") (|PositiveInteger|))
          '#(~= 131 |zero?| 137 |unitNormal| 142 |unitCanonical| 147
             |unit?| 152 |symmetricRemainder| 157 |subtractIfCan| 163
             |submod| 169 |squareFreePart| 176 |squareFree| 181
             |sizeLess?| 186 |sign| 192 |shift| 197 |sample| 203
             |retractIfCan| 207 |retract| 212 |rem| 217 |reducedSystem|
             223 |recip| 234 |rationalIfCan| 239 |rational?| 244
             |rational| 249 |random| 254 |quo| 263 |principalIdeal| 269
             |prime?| 274 |powmod| 279 |positiveRemainder| 286
             |positive?| 292 |permutation| 297 |patternMatch| 303
             |one?| 310 |odd?| 315 |nextItem| 320 |negative?| 325
             |multiEuclidean| 330 |mulmod| 336 |min| 343 |max| 349
             |mask| 355 |length| 360 |lcm| 365 |latex| 376 |invmod| 381
             |init| 387 |inc| 391 |hash| 396 |gcdPolynomial| 401 |gcd|
             407 |factorial| 418 |factor| 423 |extendedEuclidean| 428
             |exquo| 441 |expressIdealMember| 447 |even?| 453
             |euclideanSize| 458 |divide| 463 |differentiate| 469 |dec|
             480 |copy| 485 |convert| 490 |coerce| 520 |characteristic|
             540 |bit?| 544 |binomial| 550 |base| 556 |associates?| 560
             |addmod| 566 |abs| 573 |Zero| 578 |One| 582 |OMwrite| 586
             D 610 >= 621 > 627 = 633 <= 639 < 645 - 651 + 662 ** 668 *
             680)
          '((|infinite| . 0) (|noetherian| . 0)
            (|canonicalsClosed| . 0) (|canonical| . 0)
            (|canonicalUnitNormal| . 0) (|multiplicativeValuation| . 0)
            (|noZeroDivisors| . 0) ((|commutative| "*") . 0)
            (|rightUnitary| . 0) (|leftUnitary| . 0)
            (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |DifferentialRing&| |OrderedRing&| NIL NIL
                         |Module&| NIL |Ring&| NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL |AbelianMonoid&|
                         |Monoid&| NIL NIL |OrderedSet&|
                         |AbelianSemiGroup&| |SemiGroup&| NIL
                         |SetCategory&| NIL NIL NIL NIL NIL NIL NIL
                         |RetractableTo&| NIL |BasicType&| NIL)
                      (CONS '#((|IntegerNumberSystem|)
                               (|EuclideanDomain|)
                               (|UniqueFactorizationDomain|)
                               (|PrincipalIdealDomain|)
                               (|OrderedIntegralDomain|) (|GcdDomain|)
                               (|IntegralDomain|) (|Algebra| $$)
                               (|CharacteristicZero|)
                               (|LinearlyExplicitRingOver| 11)
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
                               (|StepThrough|) (|PatternMatchable| 11)
                               (|OrderedSet|) (|AbelianSemiGroup|)
                               (|SemiGroup|) (|RealConstant|)
                               (|SetCategory|) (|OpenMath|)
                               (|ConvertibleTo| 9) (|ConvertibleTo| 45)
                               (|ConvertibleTo| 48)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 123)
                               (|ConvertibleTo| 50)
                               (|RetractableTo| 11)
                               (|ConvertibleTo| 11) (|BasicType|)
                               (|CoercibleTo| 36))
                            (|makeByteWordVec2| 131
                                '(1 7 6 0 8 3 7 6 0 9 9 10 2 7 6 0 11
                                  12 1 7 6 0 13 0 14 0 15 2 7 0 9 14 16
                                  1 7 6 0 17 1 7 6 0 18 1 7 6 0 19 1 36
                                  0 11 37 1 45 0 11 46 1 50 0 11 51 2
                                  93 92 91 90 94 1 97 96 95 98 1 95 0 0
                                  99 1 95 2 0 100 1 101 96 95 102 1 95
                                  0 2 103 1 0 104 0 105 2 108 96 106
                                  107 109 2 110 96 96 96 111 1 101 96
                                  95 112 1 95 21 0 113 1 95 0 0 114 1
                                  116 95 115 117 2 0 21 0 0 1 1 0 21 0
                                  25 1 0 87 0 88 1 0 0 0 89 1 0 21 0 1
                                  2 0 0 0 0 1 2 0 83 0 0 1 3 0 0 0 0 0
                                  43 1 0 0 0 1 1 0 104 0 1 2 0 21 0 0 1
                                  1 0 11 0 1 2 0 0 0 0 82 0 0 0 1 1 0
                                  124 0 1 1 0 11 0 1 2 0 0 0 0 81 2 0
                                  60 57 61 62 1 0 56 57 58 1 0 83 0 85
                                  1 0 121 0 1 1 0 21 0 1 1 0 120 0 1 0
                                  0 0 64 1 0 0 0 65 2 0 0 0 0 80 1 0
                                  126 125 1 1 0 21 0 1 3 0 0 0 0 0 1 2
                                  0 0 0 0 55 1 0 21 0 1 2 0 0 0 0 1 3 0
                                  122 0 123 122 1 1 0 21 0 26 1 0 21 0
                                  75 1 0 83 0 1 1 0 21 0 35 2 0 127 125
                                  0 1 3 0 0 0 0 0 44 2 0 0 0 0 77 2 0 0
                                  0 0 76 1 0 0 0 1 1 0 0 0 41 2 0 0 0 0
                                  1 1 0 0 125 1 1 0 9 0 54 2 0 0 0 0 1
                                  0 0 0 1 1 0 0 0 31 1 0 33 0 34 2 0
                                  118 118 118 119 2 0 0 0 0 86 1 0 0
                                  125 1 1 0 0 0 1 1 0 104 0 105 2 0 128
                                  0 0 1 3 0 130 0 0 0 1 2 0 83 0 0 84 2
                                  0 127 125 0 1 1 0 21 0 1 1 0 73 0 1 2
                                  0 78 0 0 79 1 0 0 0 1 2 0 0 0 73 1 1
                                  0 0 0 32 1 0 0 0 30 1 0 9 0 53 1 0 48
                                  0 49 1 0 45 0 47 1 0 50 0 52 1 0 123
                                  0 1 1 0 11 0 40 1 0 0 11 39 1 0 0 0 1
                                  1 0 0 11 39 1 0 36 0 38 0 0 73 1 2 0
                                  21 0 0 1 2 0 0 0 0 1 0 0 0 29 2 0 21
                                  0 0 1 3 0 0 0 0 0 42 1 0 0 0 63 0 0 0
                                  27 0 0 0 28 3 0 6 7 0 21 24 2 0 9 0
                                  21 22 2 0 6 7 0 23 1 0 9 0 20 1 0 0 0
                                  1 2 0 0 0 73 1 2 0 21 0 0 1 2 0 21 0
                                  0 1 2 0 21 0 0 66 2 0 21 0 0 1 2 0 21
                                  0 0 67 2 0 0 0 0 70 1 0 0 0 68 2 0 0
                                  0 0 69 2 0 0 0 73 74 2 0 0 0 131 1 2
                                  0 0 0 0 71 2 0 0 11 0 72 2 0 0 73 0 1
                                  2 0 0 131 0 1)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Integer| '|isFunctor|
             '(((|OMwrite| ((|Void|) (|OpenMathDevice|) $ (|Boolean|)))
                T (ELT $ 24))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $)) T
                (ELT $ 23))
               ((|OMwrite| ((|String|) $ (|Boolean|))) T (ELT $ 22))
               ((|OMwrite| ((|String|) $)) T (ELT $ 20))
               ((|convert| ((|String|) $)) T (ELT $ 53))
               ((|invmod| ($ $ $)) T (ELT $ NIL))
               ((|powmod| ($ $ $ $)) T (ELT $ NIL))
               ((|mulmod| ($ $ $ $)) T (ELT $ 44))
               ((|submod| ($ $ $ $)) T (ELT $ 43))
               ((|addmod| ($ $ $ $)) T (ELT $ 42))
               ((|mask| ($ $)) T (ELT $ NIL))
               ((|dec| ($ $)) T (ELT $ 32))
               ((|inc| ($ $)) T (ELT $ 31))
               ((|copy| ($ $)) T (ELT $ 30))
               ((|random| ($ $)) T (ELT $ 65))
               ((|random| ($)) T (ELT $ 64))
               ((|rationalIfCan|
                    ((|Union| (|Fraction| (|Integer|)) "failed") $))
                T (ELT $ NIL))
               ((|rational| ((|Fraction| (|Integer|)) $)) T
                (ELT $ NIL))
               ((|rational?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|symmetricRemainder| ($ $ $)) T (ELT $ NIL))
               ((|positiveRemainder| ($ $ $)) T (ELT $ 55))
               ((|bit?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|shift| ($ $ $)) T (ELT $ 82))
               ((|length| ($ $)) T (ELT $ 41))
               ((|base| ($)) T (ELT $ 29))
               ((|even?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|odd?| ((|Boolean|) $)) T (ELT $ 75))
               ((|init| ($)) T (CONST $ NIL))
               ((|nextItem| ((|Union| $ "failed") $)) T (ELT $ NIL))
               ((|convert| ((|DoubleFloat|) $)) T (ELT $ 49))
               ((|convert| ((|Float|) $)) T (ELT $ 47))
               ((|permutation| ($ $ $)) T (ELT $ NIL))
               ((|factorial| ($ $)) T (ELT $ NIL))
               ((|binomial| ($ $ $)) T (ELT $ NIL))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Integer|) $) $
                     (|Pattern| (|Integer|))
                     (|PatternMatchResult| (|Integer|) $)))
                T (ELT $ NIL))
               ((|convert| ((|Pattern| (|Integer|)) $)) T (ELT $ NIL))
               ((|convert| ((|InputForm|) $)) T (ELT $ 52))
               ((|reducedSystem| ((|Matrix| (|Integer|)) (|Matrix| $)))
                T (ELT $ 58))
               ((|reducedSystem|
                    ((|Record| (|:| |mat| (|Matrix| (|Integer|)))
                         (|:| |vec| (|Vector| (|Integer|))))
                     (|Matrix| $) (|Vector| $)))
                T (ELT $ 62))
               ((|retract| ((|Integer|) $)) T (ELT $ NIL))
               ((|retractIfCan| ((|Union| (|Integer|) "failed") $)) T
                (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 39))
               ((|convert| ((|Integer|) $)) T (ELT $ 40))
               ((|differentiate| ($ $)) T (ELT $ NIL))
               ((D ($ $)) T (ELT $ NIL))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((D ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|abs| ($ $)) T (ELT $ 63))
               ((|sign| ((|Integer|) $)) T (ELT $ NIL))
               ((|negative?| ((|Boolean|) $)) T (ELT $ 35))
               ((|positive?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|min| ($ $ $)) T (ELT $ 77))
               ((|max| ($ $ $)) T (ELT $ 76))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ 67))
               ((|principalIdeal|
                    ((|Record| (|:| |coef| (|List| $))
                         (|:| |generator| $))
                     (|List| $)))
                T (ELT $ NIL))
               ((|expressIdealMember|
                    ((|Union| (|List| $) "failed") (|List| $) $))
                T (ELT $ NIL))
               ((|sizeLess?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|euclideanSize| ((|NonNegativeInteger|) $)) T
                (ELT $ NIL))
               ((|divide|
                    ((|Record| (|:| |quotient| $) (|:| |remainder| $))
                     $ $))
                T (ELT $ 79))
               ((|quo| ($ $ $)) T (ELT $ 80))
               ((|rem| ($ $ $)) T (ELT $ 81))
               ((|extendedEuclidean|
                    ((|Record| (|:| |coef1| $) (|:| |coef2| $)
                         (|:| |generator| $))
                     $ $))
                T (ELT $ NIL))
               ((|extendedEuclidean|
                    ((|Union| (|Record| (|:| |coef1| $)
                                  (|:| |coef2| $))
                              "failed")
                     $ $ $))
                T (ELT $ NIL))
               ((|multiEuclidean|
                    ((|Union| (|List| $) "failed") (|List| $) $))
                T (ELT $ NIL))
               ((|factor| ((|Factored| $) $)) T (ELT $ 105))
               ((|squareFreePart| ($ $)) T (ELT $ NIL))
               ((|squareFree| ((|Factored| $) $)) T (ELT $ NIL))
               ((|prime?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ 119))
               ((|lcm| ($ (|List| $))) T (ELT $ NIL))
               ((|lcm| ($ $ $)) T (ELT $ NIL))
               ((|gcd| ($ (|List| $))) T (ELT $ NIL))
               ((|gcd| ($ $ $)) T (ELT $ 86))
               ((|unit?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|associates?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|unitCanonical| ($ $)) T (ELT $ 89))
               ((|unitNormal|
                    ((|Record| (|:| |unit| $) (|:| |canonical| $)
                         (|:| |associate| $))
                     $))
                T (ELT $ 88))
               ((|exquo| ((|Union| $ "failed") $ $)) T (ELT $ 84))
               ((|coerce| ($ $)) T (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 39))
               ((|characteristic| ((|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|One| ($)) T (CONST $ 28))
               ((|one?| ((|Boolean|) $)) T (ELT $ 26))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ 74))
               ((|recip| ((|Union| $ "failed") $)) T (ELT $ 85))
               ((* ($ $ $)) T (ELT $ 71))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((* ($ (|Integer|) $)) T (ELT $ 72))
               ((- ($ $ $)) T (ELT $ 70)) ((- ($ $)) T (ELT $ 68))
               ((|subtractIfCan| ((|Union| $ "failed") $ $)) T
                (ELT $ NIL))
               ((* ($ (|NonNegativeInteger|) $)) T (ELT $ NIL))
               ((|zero?| ((|Boolean|) $)) T (ELT $ 25))
               ((|sample| ($)) T (CONST $ NIL))
               ((|Zero| ($)) T (CONST $ 27))
               ((* ($ (|PositiveInteger|) $)) T (ELT $ NIL))
               ((+ ($ $ $)) T (ELT $ 69))
               ((|latex| ((|String|) $)) T (ELT $ 54))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ 34))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 38))
               ((= ((|Boolean|) $ $)) T (ELT $ 66))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|Integer| '(|Integer|)
                 '((|Join| (|IntegerNumberSystem|)
                           (|ConvertibleTo| (|String|)) (|OpenMath|)
                           (CATEGORY |domain|
                               (SIGNATURE |random| ($ $))
                               (ATTRIBUTE |canonical|)
                               (ATTRIBUTE |canonicalsClosed|)
                               (ATTRIBUTE |noetherian|)
                               (ATTRIBUTE |infinite|))))
                 T '|Integer|
                 (|put| '|Integer| '|mode|
                        '(|Mapping|
                             (|Join| (|IntegerNumberSystem|)
                                     (|ConvertibleTo| (|String|))
                                     (|OpenMath|)
                                     (CATEGORY |domain|
                                      (SIGNATURE |random| ($ $))
                                      (ATTRIBUTE |canonical|)
                                      (ATTRIBUTE |canonicalsClosed|)
                                      (ATTRIBUTE |noetherian|)
                                      (ATTRIBUTE |infinite|))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|Integer| 'NILADIC T) 
