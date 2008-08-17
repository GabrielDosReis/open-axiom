
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

(PUT '|INT;hash;2$;14| '|SPADreplace| 'SXHASH) 

(DEFUN |INT;hash;2$;14| (|x| $) (SXHASH |x|)) 

(PUT '|INT;negative?;$B;15| '|SPADreplace| 'MINUSP) 

(DEFUN |INT;negative?;$B;15| (|x| $) (MINUSP |x|)) 

(DEFUN |INT;coerce;$Of;16| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 36))) 

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
  (SPADCALL |x| (|getShellEntry| $ 45))) 

(PUT '|INT;convert;$Df;24| '|SPADreplace|
     '(XLAM (|x|) (FLOAT |x| MOST-POSITIVE-LONG-FLOAT))) 

(DEFUN |INT;convert;$Df;24| (|x| $)
  (FLOAT |x| MOST-POSITIVE-LONG-FLOAT)) 

(DEFUN |INT;convert;$If;25| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 50))) 

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
  (SPADCALL |lp| |p| (|getShellEntry| $ 93))) 

(DEFUN |INT;squareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 97))) 

(DEFUN |INT;factorPolynomial| (|p| $)
  (PROG (|pp| #0=#:G1500)
    (RETURN
      (SEQ (LETT |pp| (SPADCALL |p| (|getShellEntry| $ 98))
                 |INT;factorPolynomial|)
           (EXIT (COND
                   ((EQL (SPADCALL |pp| (|getShellEntry| $ 99))
                         (SPADCALL |p| (|getShellEntry| $ 99)))
                    (SPADCALL |p| (|getShellEntry| $ 101)))
                   ('T
                    (SPADCALL (SPADCALL |pp| (|getShellEntry| $ 101))
                        (SPADCALL (CONS #'|INT;factorPolynomial!0| $)
                            (SPADCALL
                                (PROG2 (LETT #0#
                                        (SPADCALL
                                         (SPADCALL |p|
                                          (|getShellEntry| $ 99))
                                         (SPADCALL |pp|
                                          (|getShellEntry| $ 99))
                                         (|getShellEntry| $ 83))
                                        |INT;factorPolynomial|)
                                       (QCDR #0#)
                                  (|check-union| (QEQCAR #0# 0) $ #0#))
                                (|getShellEntry| $ 104))
                            (|getShellEntry| $ 108))
                        (|getShellEntry| $ 110))))))))) 

(DEFUN |INT;factorPolynomial!0| (|#1| $)
  (SPADCALL |#1| (|getShellEntry| $ 102))) 

(DEFUN |INT;factorSquareFreePolynomial| (|p| $)
  (SPADCALL |p| (|getShellEntry| $ 111))) 

(DEFUN |INT;gcdPolynomial;3Sup;58| (|p| |q| $)
  (COND
    ((SPADCALL |p| (|getShellEntry| $ 112))
     (SPADCALL |q| (|getShellEntry| $ 113)))
    ((SPADCALL |q| (|getShellEntry| $ 112))
     (SPADCALL |p| (|getShellEntry| $ 113)))
    ('T (SPADCALL (LIST |p| |q|) (|getShellEntry| $ 116))))) 

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
        (|setShellEntry| $ 71
            (|setShellEntry| $ 70
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
             |INT;dec;2$;13| |INT;hash;2$;14| |INT;negative?;$B;15|
             (|OutputForm|) (48 . |outputForm|) |INT;coerce;$Of;16|
             |INT;coerce;2$;17| |INT;convert;2$;18| |INT;length;2$;19|
             |INT;addmod;4$;20| |INT;submod;4$;21| |INT;mulmod;4$;22|
             (|Float|) (53 . |coerce|) |INT;convert;$F;23|
             (|DoubleFloat|) |INT;convert;$Df;24| (|InputForm|)
             (58 . |convert|) |INT;convert;$If;25| |INT;convert;$S;26|
             |INT;latex;$S;27| |INT;positiveRemainder;3$;28|
             (|Matrix| 11) (|Matrix| $) |INT;reducedSystem;2M;29|
             (|Vector| 11) (|Record| (|:| |mat| 55) (|:| |vec| 58))
             (|Vector| $) |INT;reducedSystem;MVR;30| |INT;abs;2$;31|
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
             (|SparseUnivariatePolynomial| 11) (|List| 89)
             (|Union| 90 '"failed")
             (|IntegerSolveLinearPolynomialEquation|)
             (63 . |solveLinearPolynomialEquation|)
             (|SparseUnivariatePolynomial| $$) (|Factored| 94)
             (|UnivariatePolynomialSquareFree| $$ 94)
             (69 . |squareFree|) (74 . |primitivePart|)
             (79 . |leadingCoefficient|) (|GaloisGroupFactorizer| 94)
             (84 . |factor|) (89 . |coerce|) (|Factored| $)
             (94 . |factor|) (|Mapping| 94 $$) (|Factored| $$)
             (|FactoredFunctions2| $$ 94) (99 . |map|)
             (|FactoredFunctionUtilities| 94) (105 . |mergeFactors|)
             (111 . |factorSquareFree|) (116 . |zero?|)
             (121 . |unitCanonical|) (|List| 94) (|HeuGcd| 94)
             (126 . |gcd|) (|SparseUnivariatePolynomial| $)
             |INT;gcdPolynomial;3Sup;58| (|Fraction| 11)
             (|Union| 119 '"failed") (|PatternMatchResult| 11 $)
             (|Pattern| 11) (|Union| 11 '"failed") (|List| $)
             (|Union| 124 '"failed")
             (|Record| (|:| |coef| 124) (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 127 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|PositiveInteger|) (|SingleInteger|))
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
             |init| 387 |inc| 391 |hash| 396 |gcdPolynomial| 406 |gcd|
             412 |factorial| 423 |factor| 428 |extendedEuclidean| 433
             |exquo| 446 |expressIdealMember| 452 |even?| 458
             |euclideanSize| 463 |divide| 468 |differentiate| 474 |dec|
             485 |copy| 490 |convert| 495 |coerce| 525 |characteristic|
             545 |bit?| 549 |binomial| 555 |base| 561 |associates?| 565
             |addmod| 571 |abs| 578 ^ 583 |Zero| 595 |One| 599
             |OMwrite| 603 D 627 >= 638 > 644 = 650 <= 656 < 662 - 668
             + 679 ** 685 * 697)
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
                               (|ConvertibleTo| 9) (|ConvertibleTo| 44)
                               (|ConvertibleTo| 47)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 122)
                               (|ConvertibleTo| 49)
                               (|RetractableTo| 11)
                               (|ConvertibleTo| 11) (|BasicType|)
                               (|CoercibleTo| 35))
                            (|makeByteWordVec2| 131
                                '(1 7 6 0 8 3 7 6 0 9 9 10 2 7 6 0 11
                                  12 1 7 6 0 13 0 14 0 15 2 7 0 9 14 16
                                  1 7 6 0 17 1 7 6 0 18 1 7 6 0 19 1 35
                                  0 11 36 1 44 0 11 45 1 49 0 11 50 2
                                  92 91 90 89 93 1 96 95 94 97 1 94 0 0
                                  98 1 94 2 0 99 1 100 95 94 101 1 94 0
                                  2 102 1 0 103 0 104 2 107 95 105 106
                                  108 2 109 95 95 95 110 1 100 95 94
                                  111 1 94 21 0 112 1 94 0 0 113 1 115
                                  94 114 116 2 0 21 0 0 1 1 0 21 0 25 1
                                  0 86 0 87 1 0 0 0 88 1 0 21 0 1 2 0 0
                                  0 0 1 2 0 82 0 0 1 3 0 0 0 0 0 42 1 0
                                  0 0 1 1 0 103 0 1 2 0 21 0 0 1 1 0 11
                                  0 1 2 0 0 0 0 81 0 0 0 1 1 0 123 0 1
                                  1 0 11 0 1 2 0 0 0 0 80 2 0 59 56 60
                                  61 1 0 55 56 57 1 0 82 0 84 1 0 120 0
                                  1 1 0 21 0 1 1 0 119 0 1 1 0 0 0 64 0
                                  0 0 63 2 0 0 0 0 79 1 0 126 124 1 1 0
                                  21 0 1 3 0 0 0 0 0 1 2 0 0 0 0 54 1 0
                                  21 0 1 2 0 0 0 0 1 3 0 121 0 122 121
                                  1 1 0 21 0 26 1 0 21 0 74 1 0 82 0 1
                                  1 0 21 0 34 2 0 125 124 0 1 3 0 0 0 0
                                  0 43 2 0 0 0 0 76 2 0 0 0 0 75 1 0 0
                                  0 1 1 0 0 0 40 1 0 0 124 1 2 0 0 0 0
                                  1 1 0 9 0 53 2 0 0 0 0 1 0 0 0 1 1 0
                                  0 0 31 1 0 0 0 33 1 0 131 0 1 2 0 117
                                  117 117 118 2 0 0 0 0 85 1 0 0 124 1
                                  1 0 0 0 1 1 0 103 0 104 3 0 128 0 0 0
                                  1 2 0 129 0 0 1 2 0 82 0 0 83 2 0 125
                                  124 0 1 1 0 21 0 1 1 0 72 0 1 2 0 77
                                  0 0 78 1 0 0 0 1 2 0 0 0 72 1 1 0 0 0
                                  32 1 0 0 0 30 1 0 9 0 52 1 0 47 0 48
                                  1 0 44 0 46 1 0 49 0 51 1 0 122 0 1 1
                                  0 11 0 39 1 0 0 11 38 1 0 0 11 38 1 0
                                  0 0 1 1 0 35 0 37 0 0 72 1 2 0 21 0 0
                                  1 2 0 0 0 0 1 0 0 0 29 2 0 21 0 0 1 3
                                  0 0 0 0 0 41 1 0 0 0 62 2 0 0 0 72 1
                                  2 0 0 0 130 1 0 0 0 27 0 0 0 28 3 0 6
                                  7 0 21 24 2 0 9 0 21 22 2 0 6 7 0 23
                                  1 0 9 0 20 1 0 0 0 1 2 0 0 0 72 1 2 0
                                  21 0 0 1 2 0 21 0 0 1 2 0 21 0 0 65 2
                                  0 21 0 0 1 2 0 21 0 0 66 2 0 0 0 0 69
                                  1 0 0 0 67 2 0 0 0 0 68 2 0 0 0 72 73
                                  2 0 0 0 130 1 2 0 0 0 0 70 2 0 0 11 0
                                  71 2 0 0 72 0 1 2 0 0 130 0 1)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Integer| '|isFunctor|
             '(((|OMwrite| ((|Void|) (|OpenMathDevice|) $ (|Boolean|)))
                T (ELT $ 24))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $)) T
                (ELT $ 23))
               ((|OMwrite| ((|String|) $ (|Boolean|))) T (ELT $ 22))
               ((|OMwrite| ((|String|) $)) T (ELT $ 20))
               ((|convert| ((|String|) $)) T (ELT $ 52))
               ((|invmod| ($ $ $)) T (ELT $ NIL))
               ((|powmod| ($ $ $ $)) T (ELT $ NIL))
               ((|mulmod| ($ $ $ $)) T (ELT $ 43))
               ((|submod| ($ $ $ $)) T (ELT $ 42))
               ((|addmod| ($ $ $ $)) T (ELT $ 41))
               ((|mask| ($ $)) T (ELT $ NIL))
               ((|dec| ($ $)) T (ELT $ 32))
               ((|inc| ($ $)) T (ELT $ 31))
               ((|copy| ($ $)) T (ELT $ 30))
               ((|hash| ($ $)) T (ELT $ 33))
               ((|random| ($ $)) T (ELT $ 64))
               ((|random| ($)) T (ELT $ 63))
               ((|rationalIfCan|
                    ((|Union| (|Fraction| (|Integer|)) "failed") $))
                T (ELT $ NIL))
               ((|rational| ((|Fraction| (|Integer|)) $)) T
                (ELT $ NIL))
               ((|rational?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|symmetricRemainder| ($ $ $)) T (ELT $ NIL))
               ((|positiveRemainder| ($ $ $)) T (ELT $ 54))
               ((|bit?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|shift| ($ $ $)) T (ELT $ 81))
               ((|length| ($ $)) T (ELT $ 40))
               ((|base| ($)) T (ELT $ 29))
               ((|even?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|odd?| ((|Boolean|) $)) T (ELT $ 74))
               ((|init| ($)) T (CONST $ NIL))
               ((|nextItem| ((|Union| $ "failed") $)) T (ELT $ NIL))
               ((|convert| ((|DoubleFloat|) $)) T (ELT $ 48))
               ((|convert| ((|Float|) $)) T (ELT $ 46))
               ((|permutation| ($ $ $)) T (ELT $ NIL))
               ((|factorial| ($ $)) T (ELT $ NIL))
               ((|binomial| ($ $ $)) T (ELT $ NIL))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Integer|) $) $
                     (|Pattern| (|Integer|))
                     (|PatternMatchResult| (|Integer|) $)))
                T (ELT $ NIL))
               ((|convert| ((|Pattern| (|Integer|)) $)) T (ELT $ NIL))
               ((|convert| ((|InputForm|) $)) T (ELT $ 51))
               ((|reducedSystem| ((|Matrix| (|Integer|)) (|Matrix| $)))
                T (ELT $ 57))
               ((|reducedSystem|
                    ((|Record| (|:| |mat| (|Matrix| (|Integer|)))
                         (|:| |vec| (|Vector| (|Integer|))))
                     (|Matrix| $) (|Vector| $)))
                T (ELT $ 61))
               ((|retract| ((|Integer|) $)) T (ELT $ NIL))
               ((|retractIfCan| ((|Union| (|Integer|) "failed") $)) T
                (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 38))
               ((|convert| ((|Integer|) $)) T (ELT $ 39))
               ((|differentiate| ($ $)) T (ELT $ NIL))
               ((D ($ $)) T (ELT $ NIL))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((D ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|abs| ($ $)) T (ELT $ 62))
               ((|sign| ((|Integer|) $)) T (ELT $ NIL))
               ((|negative?| ((|Boolean|) $)) T (ELT $ 34))
               ((|positive?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|min| ($ $ $)) T (ELT $ 76))
               ((|max| ($ $ $)) T (ELT $ 75))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ 66))
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
                T (ELT $ 78))
               ((|quo| ($ $ $)) T (ELT $ 79))
               ((|rem| ($ $ $)) T (ELT $ 80))
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
               ((|factor| ((|Factored| $) $)) T (ELT $ 104))
               ((|squareFreePart| ($ $)) T (ELT $ NIL))
               ((|squareFree| ((|Factored| $) $)) T (ELT $ NIL))
               ((|prime?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ 118))
               ((|lcm| ($ (|List| $))) T (ELT $ NIL))
               ((|lcm| ($ $ $)) T (ELT $ NIL))
               ((|gcd| ($ (|List| $))) T (ELT $ NIL))
               ((|gcd| ($ $ $)) T (ELT $ 85))
               ((|unit?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|associates?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|unitCanonical| ($ $)) T (ELT $ 88))
               ((|unitNormal|
                    ((|Record| (|:| |unit| $) (|:| |canonical| $)
                         (|:| |associate| $))
                     $))
                T (ELT $ 87))
               ((|exquo| ((|Union| $ "failed") $ $)) T (ELT $ 83))
               ((|coerce| ($ $)) T (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 38))
               ((|characteristic| ((|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|One| ($)) T (CONST $ 28))
               ((|one?| ((|Boolean|) $)) T (ELT $ 26))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ 73))
               ((^ ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|recip| ((|Union| $ "failed") $)) T (ELT $ 84))
               ((* ($ $ $)) T (ELT $ 70))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((^ ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((* ($ (|Integer|) $)) T (ELT $ 71))
               ((- ($ $ $)) T (ELT $ 69)) ((- ($ $)) T (ELT $ 67))
               ((|subtractIfCan| ((|Union| $ "failed") $ $)) T
                (ELT $ NIL))
               ((* ($ (|NonNegativeInteger|) $)) T (ELT $ NIL))
               ((|zero?| ((|Boolean|) $)) T (ELT $ 25))
               ((|sample| ($)) T (CONST $ NIL))
               ((|Zero| ($)) T (CONST $ 27))
               ((* ($ (|PositiveInteger|) $)) T (ELT $ NIL))
               ((+ ($ $ $)) T (ELT $ 68))
               ((|latex| ((|String|) $)) T (ELT $ 53))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 37))
               ((= ((|Boolean|) $ $)) T (ELT $ 65))
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
