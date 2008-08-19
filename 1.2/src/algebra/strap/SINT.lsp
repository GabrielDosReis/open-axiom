
(/VERSIONCHECK 2) 

(DEFUN |SINT;writeOMSingleInt| (|dev| |x| $)
  (SEQ (COND
         ((QSLESSP |x| 0)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 9))
               (SPADCALL |dev| "arith1" "unaryminus"
                   (|getShellEntry| $ 11))
               (SPADCALL |dev| (QSMINUS |x|) (|getShellEntry| $ 13))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 14)))))
         ('T (SPADCALL |dev| |x| (|getShellEntry| $ 13)))))) 

(DEFUN |SINT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 16))
                     (|getShellEntry| $ 17))
                 |SINT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 18))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 19))
           (SPADCALL |dev| (|getShellEntry| $ 20))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 16))
                     (|getShellEntry| $ 17))
                 |SINT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 19))))
           (SPADCALL |dev| (|getShellEntry| $ 20))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 18))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 19))))) 

(DEFUN |SINT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 19))))))) 

(PUT '|SINT;reducedSystem;MM;6| '|SPADreplace| '(XLAM (|m|) |m|)) 

(DEFUN |SINT;reducedSystem;MM;6| (|m| $) |m|) 

(DEFUN |SINT;coerce;$Of;7| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 30))) 

(PUT '|SINT;convert;$I;8| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |SINT;convert;$I;8| (|x| $) |x|) 

(DEFUN |SINT;*;I2$;9| (|i| |y| $)
  (QSTIMES (SPADCALL |i| (|getShellEntry| $ 33)) |y|)) 

(PUT '|SINT;Zero;$;10| '|SPADreplace| '(XLAM NIL 0)) 

(DEFUN |SINT;Zero;$;10| ($) 0) 

(PUT '|SINT;One;$;11| '|SPADreplace| '(XLAM NIL 1)) 

(DEFUN |SINT;One;$;11| ($) 1) 

(PUT '|SINT;base;$;12| '|SPADreplace| '(XLAM NIL 2)) 

(DEFUN |SINT;base;$;12| ($) 2) 

(PUT '|SINT;max;$;13| '|SPADreplace| '(XLAM NIL MOST-POSITIVE-FIXNUM)) 

(DEFUN |SINT;max;$;13| ($) MOST-POSITIVE-FIXNUM) 

(PUT '|SINT;min;$;14| '|SPADreplace| '(XLAM NIL MOST-NEGATIVE-FIXNUM)) 

(DEFUN |SINT;min;$;14| ($) MOST-NEGATIVE-FIXNUM) 

(PUT '|SINT;=;2$B;15| '|SPADreplace| 'EQL) 

(DEFUN |SINT;=;2$B;15| (|x| |y| $) (EQL |x| |y|)) 

(PUT '|SINT;~;2$;16| '|SPADreplace| 'LOGNOT) 

(DEFUN |SINT;~;2$;16| (|x| $) (LOGNOT |x|)) 

(PUT '|SINT;not;2$;17| '|SPADreplace| 'LOGNOT) 

(DEFUN |SINT;not;2$;17| (|x| $) (LOGNOT |x|)) 

(PUT '|SINT;/\\;3$;18| '|SPADreplace| 'LOGAND) 

(DEFUN |SINT;/\\;3$;18| (|x| |y| $) (LOGAND |x| |y|)) 

(PUT '|SINT;\\/;3$;19| '|SPADreplace| 'LOGIOR) 

(DEFUN |SINT;\\/;3$;19| (|x| |y| $) (LOGIOR |x| |y|)) 

(PUT '|SINT;Not;2$;20| '|SPADreplace| 'LOGNOT) 

(DEFUN |SINT;Not;2$;20| (|x| $) (LOGNOT |x|)) 

(PUT '|SINT;And;3$;21| '|SPADreplace| 'LOGAND) 

(DEFUN |SINT;And;3$;21| (|x| |y| $) (LOGAND |x| |y|)) 

(PUT '|SINT;Or;3$;22| '|SPADreplace| 'LOGIOR) 

(DEFUN |SINT;Or;3$;22| (|x| |y| $) (LOGIOR |x| |y|)) 

(PUT '|SINT;xor;3$;23| '|SPADreplace| 'LOGXOR) 

(DEFUN |SINT;xor;3$;23| (|x| |y| $) (LOGXOR |x| |y|)) 

(PUT '|SINT;<;2$B;24| '|SPADreplace| 'QSLESSP) 

(DEFUN |SINT;<;2$B;24| (|x| |y| $) (QSLESSP |x| |y|)) 

(PUT '|SINT;inc;2$;25| '|SPADreplace| 'QSADD1) 

(DEFUN |SINT;inc;2$;25| (|x| $) (QSADD1 |x|)) 

(PUT '|SINT;dec;2$;26| '|SPADreplace| 'QSSUB1) 

(DEFUN |SINT;dec;2$;26| (|x| $) (QSSUB1 |x|)) 

(PUT '|SINT;-;2$;27| '|SPADreplace| 'QSMINUS) 

(DEFUN |SINT;-;2$;27| (|x| $) (QSMINUS |x|)) 

(PUT '|SINT;+;3$;28| '|SPADreplace| 'QSPLUS) 

(DEFUN |SINT;+;3$;28| (|x| |y| $) (QSPLUS |x| |y|)) 

(PUT '|SINT;-;3$;29| '|SPADreplace| 'QSDIFFERENCE) 

(DEFUN |SINT;-;3$;29| (|x| |y| $) (QSDIFFERENCE |x| |y|)) 

(PUT '|SINT;*;3$;30| '|SPADreplace| 'QSTIMES) 

(DEFUN |SINT;*;3$;30| (|x| |y| $) (QSTIMES |x| |y|)) 

(DEFUN |SINT;**;$Nni$;31| (|x| |n| $)
  (SPADCALL (EXPT |x| |n|) (|getShellEntry| $ 33))) 

(PUT '|SINT;quo;3$;32| '|SPADreplace| 'QSQUOTIENT) 

(DEFUN |SINT;quo;3$;32| (|x| |y| $) (QSQUOTIENT |x| |y|)) 

(PUT '|SINT;rem;3$;33| '|SPADreplace| 'QSREMAINDER) 

(DEFUN |SINT;rem;3$;33| (|x| |y| $) (QSREMAINDER |x| |y|)) 

(DEFUN |SINT;divide;2$R;34| (|x| |y| $)
  (CONS (QSQUOTIENT |x| |y|) (QSREMAINDER |x| |y|))) 

(PUT '|SINT;gcd;3$;35| '|SPADreplace| 'GCD) 

(DEFUN |SINT;gcd;3$;35| (|x| |y| $) (GCD |x| |y|)) 

(PUT '|SINT;abs;2$;36| '|SPADreplace| 'QSABSVAL) 

(DEFUN |SINT;abs;2$;36| (|x| $) (QSABSVAL |x|)) 

(PUT '|SINT;odd?;$B;37| '|SPADreplace| 'QSODDP) 

(DEFUN |SINT;odd?;$B;37| (|x| $) (QSODDP |x|)) 

(PUT '|SINT;zero?;$B;38| '|SPADreplace| 'QSZEROP) 

(DEFUN |SINT;zero?;$B;38| (|x| $) (QSZEROP |x|)) 

(PUT '|SINT;one?;$B;39| '|SPADreplace| '(XLAM (|x|) (EQL |x| 1))) 

(DEFUN |SINT;one?;$B;39| (|x| $) (EQL |x| 1)) 

(PUT '|SINT;max;3$;40| '|SPADreplace| 'QSMAX) 

(DEFUN |SINT;max;3$;40| (|x| |y| $) (QSMAX |x| |y|)) 

(PUT '|SINT;min;3$;41| '|SPADreplace| 'QSMIN) 

(DEFUN |SINT;min;3$;41| (|x| |y| $) (QSMIN |x| |y|)) 

(PUT '|SINT;hash;2$;42| '|SPADreplace| 'HASHEQ) 

(DEFUN |SINT;hash;2$;42| (|x| $) (HASHEQ |x|)) 

(PUT '|SINT;length;2$;43| '|SPADreplace| 'INTEGER-LENGTH) 

(DEFUN |SINT;length;2$;43| (|x| $) (INTEGER-LENGTH |x|)) 

(PUT '|SINT;shift;3$;44| '|SPADreplace| 'QSLEFTSHIFT) 

(DEFUN |SINT;shift;3$;44| (|x| |n| $) (QSLEFTSHIFT |x| |n|)) 

(PUT '|SINT;mulmod;4$;45| '|SPADreplace| 'QSMULTMOD) 

(DEFUN |SINT;mulmod;4$;45| (|a| |b| |p| $) (QSMULTMOD |a| |b| |p|)) 

(PUT '|SINT;addmod;4$;46| '|SPADreplace| 'QSADDMOD) 

(DEFUN |SINT;addmod;4$;46| (|a| |b| |p| $) (QSADDMOD |a| |b| |p|)) 

(PUT '|SINT;submod;4$;47| '|SPADreplace| 'QSDIFMOD) 

(DEFUN |SINT;submod;4$;47| (|a| |b| |p| $) (QSDIFMOD |a| |b| |p|)) 

(PUT '|SINT;negative?;$B;48| '|SPADreplace| 'QSMINUSP) 

(DEFUN |SINT;negative?;$B;48| (|x| $) (QSMINUSP |x|)) 

(PUT '|SINT;reducedSystem;MVR;49| '|SPADreplace| 'CONS) 

(DEFUN |SINT;reducedSystem;MVR;49| (|m| |v| $) (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;50| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (QSREMAINDER |x| |n|)
                 |SINT;positiveRemainder;3$;50|)
           (EXIT (COND
                   ((QSMINUSP |r|)
                    (COND
                      ((QSMINUSP |n|) (QSDIFFERENCE |x| |n|))
                      ('T (QSPLUS |r| |n|))))
                   ('T |r|))))))) 

(DEFUN |SINT;coerce;I$;51| (|x| $)
  (SEQ (COND
         ((NULL (< MOST-POSITIVE-FIXNUM |x|))
          (COND ((NULL (< |x| MOST-NEGATIVE-FIXNUM)) (EXIT |x|)))))
       (EXIT (|error| "integer too large to represent in a machine word")))) 

(DEFUN |SINT;random;$;52| ($)
  (SEQ (SETELT $ 6
               (REMAINDER (TIMES 314159269 (|getShellEntry| $ 6))
                   2147483647))
       (EXIT (REMAINDER (|getShellEntry| $ 6) 67108864)))) 

(PUT '|SINT;random;2$;53| '|SPADreplace| 'RANDOM) 

(DEFUN |SINT;random;2$;53| (|n| $) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;54| (|x| $)
  (COND
    ((QSLESSP |x| 0) (VECTOR -1 (QSMINUS |x|) -1))
    ('T (VECTOR 1 |x| 1)))) 

(DEFUN |SingleInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1488)
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
        (LETT $ (|newShell| 105) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|SingleInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 1)
        $)))) 

(MAKEPROP '|SingleInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL '|seed| (|Void|)
             (|OpenMathDevice|) (0 . |OMputApp|) (|String|)
             (5 . |OMputSymbol|) (|Integer|) (12 . |OMputInteger|)
             (18 . |OMputEndApp|) (|OpenMathEncoding|)
             (23 . |OMencodingXML|) (27 . |OMopenString|)
             (33 . |OMputObject|) (38 . |OMputEndObject|)
             (43 . |OMclose|) |SINT;OMwrite;$S;2| (|Boolean|)
             |SINT;OMwrite;$BS;3| |SINT;OMwrite;Omd$V;4|
             |SINT;OMwrite;Omd$BV;5| (|Matrix| 12) (|Matrix| $)
             |SINT;reducedSystem;MM;6| (|OutputForm|) (48 . |coerce|)
             |SINT;coerce;$Of;7| |SINT;convert;$I;8| (53 . |coerce|)
             |SINT;*;I2$;9|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;Zero;$;10|) $))
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;One;$;11|) $))
             |SINT;base;$;12| |SINT;max;$;13| |SINT;min;$;14|
             |SINT;=;2$B;15| |SINT;~;2$;16| |SINT;not;2$;17|
             |SINT;/\\;3$;18| |SINT;\\/;3$;19| |SINT;Not;2$;20|
             |SINT;And;3$;21| |SINT;Or;3$;22| |SINT;xor;3$;23|
             |SINT;<;2$B;24| |SINT;inc;2$;25| |SINT;dec;2$;26|
             |SINT;-;2$;27| |SINT;+;3$;28| |SINT;-;3$;29|
             |SINT;*;3$;30| (|NonNegativeInteger|) |SINT;**;$Nni$;31|
             |SINT;quo;3$;32| |SINT;rem;3$;33|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;34| |SINT;gcd;3$;35| |SINT;abs;2$;36|
             |SINT;odd?;$B;37| |SINT;zero?;$B;38| |SINT;one?;$B;39|
             |SINT;max;3$;40| |SINT;min;3$;41| |SINT;hash;2$;42|
             |SINT;length;2$;43| |SINT;shift;3$;44| |SINT;mulmod;4$;45|
             |SINT;addmod;4$;46| |SINT;submod;4$;47|
             |SINT;negative?;$B;48| (|Vector| 12)
             (|Record| (|:| |mat| 26) (|:| |vec| 76)) (|Vector| $)
             |SINT;reducedSystem;MVR;49| |SINT;positiveRemainder;3$;50|
             |SINT;coerce;I$;51| |SINT;random;$;52| |SINT;random;2$;53|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;54| (|Fraction| 12)
             (|Union| 86 '"failed") (|Union| $ '"failed") (|Float|)
             (|DoubleFloat|) (|Pattern| 12) (|PatternMatchResult| 12 $)
             (|InputForm|) (|Union| 12 '"failed") (|List| $)
             (|Record| (|:| |coef| 95) (|:| |generator| $))
             (|Union| 95 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 99 '"failed") (|Factored| $)
             (|SparseUnivariatePolynomial| $) (|PositiveInteger|)
             (|SingleInteger|))
          '#(~= 58 ~ 64 |zero?| 69 |xor| 74 |unitNormal| 80
             |unitCanonical| 85 |unit?| 90 |symmetricRemainder| 95
             |subtractIfCan| 101 |submod| 107 |squareFreePart| 114
             |squareFree| 119 |sizeLess?| 124 |sign| 130 |shift| 135
             |sample| 141 |retractIfCan| 145 |retract| 150 |rem| 155
             |reducedSystem| 161 |recip| 172 |rationalIfCan| 177
             |rational?| 182 |rational| 187 |random| 192 |quo| 201
             |principalIdeal| 207 |prime?| 212 |powmod| 217
             |positiveRemainder| 224 |positive?| 230 |permutation| 235
             |patternMatch| 241 |one?| 248 |odd?| 253 |not| 258
             |nextItem| 263 |negative?| 268 |multiEuclidean| 273
             |mulmod| 279 |min| 286 |max| 296 |mask| 306 |length| 311
             |lcm| 316 |latex| 327 |invmod| 332 |init| 338 |inc| 342
             |hash| 347 |gcdPolynomial| 357 |gcd| 363 |factorial| 374
             |factor| 379 |extendedEuclidean| 384 |exquo| 397
             |expressIdealMember| 403 |even?| 409 |euclideanSize| 414
             |divide| 419 |differentiate| 425 |dec| 436 |copy| 441
             |convert| 446 |coerce| 471 |characteristic| 491 |bit?| 495
             |binomial| 501 |base| 507 |associates?| 511 |addmod| 517
             |abs| 524 ^ 529 |\\/| 541 |Zero| 547 |Or| 551 |One| 557
             |OMwrite| 561 |Not| 585 D 590 |And| 601 >= 607 > 613 = 619
             <= 625 < 631 |/\\| 637 - 643 + 654 ** 660 * 672)
          '((|noetherian| . 0) (|canonicalsClosed| . 0)
            (|canonical| . 0) (|canonicalUnitNormal| . 0)
            (|multiplicativeValuation| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
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
                         |AbelianSemiGroup&| |SemiGroup&| |Logic&| NIL
                         |SetCategory&| NIL NIL NIL NIL NIL NIL
                         |RetractableTo&| NIL |BasicType&| NIL)
                      (CONS '#((|IntegerNumberSystem|)
                               (|EuclideanDomain|)
                               (|UniqueFactorizationDomain|)
                               (|PrincipalIdealDomain|)
                               (|OrderedIntegralDomain|) (|GcdDomain|)
                               (|IntegralDomain|) (|Algebra| $$)
                               (|CharacteristicZero|)
                               (|LinearlyExplicitRingOver| 12)
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
                               (|StepThrough|) (|PatternMatchable| 12)
                               (|OrderedSet|) (|AbelianSemiGroup|)
                               (|SemiGroup|) (|Logic|) (|RealConstant|)
                               (|SetCategory|) (|OpenMath|)
                               (|ConvertibleTo| 89)
                               (|ConvertibleTo| 90)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 91)
                               (|ConvertibleTo| 93)
                               (|RetractableTo| 12)
                               (|ConvertibleTo| 12) (|BasicType|)
                               (|CoercibleTo| 29))
                            (|makeByteWordVec2| 104
                                '(1 8 7 0 9 3 8 7 0 10 10 11 2 8 7 0 12
                                  13 1 8 7 0 14 0 15 0 16 2 8 0 10 15
                                  17 1 8 7 0 18 1 8 7 0 19 1 8 7 0 20 1
                                  12 29 0 30 1 0 0 12 33 2 0 22 0 0 1 1
                                  0 0 0 41 1 0 22 0 65 2 0 0 0 0 48 1 0
                                  84 0 85 1 0 0 0 1 1 0 22 0 1 2 0 0 0
                                  0 1 2 0 88 0 0 1 3 0 0 0 0 0 74 1 0 0
                                  0 1 1 0 101 0 1 2 0 22 0 0 1 1 0 12 0
                                  1 2 0 0 0 0 71 0 0 0 1 1 0 94 0 1 1 0
                                  12 0 1 2 0 0 0 0 59 1 0 26 27 28 2 0
                                  77 27 78 79 1 0 88 0 1 1 0 87 0 1 1 0
                                  22 0 1 1 0 86 0 1 1 0 0 0 83 0 0 0 82
                                  2 0 0 0 0 58 1 0 96 95 1 1 0 22 0 1 3
                                  0 0 0 0 0 1 2 0 0 0 0 80 1 0 22 0 1 2
                                  0 0 0 0 1 3 0 92 0 91 92 1 1 0 22 0
                                  66 1 0 22 0 64 1 0 0 0 42 1 0 88 0 1
                                  1 0 22 0 75 2 0 97 95 0 1 3 0 0 0 0 0
                                  72 0 0 0 39 2 0 0 0 0 68 0 0 0 38 2 0
                                  0 0 0 67 1 0 0 0 1 1 0 0 0 70 1 0 0
                                  95 1 2 0 0 0 0 1 1 0 10 0 1 2 0 0 0 0
                                  1 0 0 0 1 1 0 0 0 50 1 0 0 0 69 1 0
                                  104 0 1 2 0 102 102 102 1 1 0 0 95 1
                                  2 0 0 0 0 62 1 0 0 0 1 1 0 101 0 1 2
                                  0 98 0 0 1 3 0 100 0 0 0 1 2 0 88 0 0
                                  1 2 0 97 95 0 1 1 0 22 0 1 1 0 56 0 1
                                  2 0 60 0 0 61 1 0 0 0 1 2 0 0 0 56 1
                                  1 0 0 0 51 1 0 0 0 1 1 0 89 0 1 1 0
                                  90 0 1 1 0 91 0 1 1 0 93 0 1 1 0 12 0
                                  32 1 0 0 12 81 1 0 0 0 1 1 0 0 12 81
                                  1 0 29 0 31 0 0 56 1 2 0 22 0 0 1 2 0
                                  0 0 0 1 0 0 0 37 2 0 22 0 0 1 3 0 0 0
                                  0 0 73 1 0 0 0 63 2 0 0 0 56 1 2 0 0
                                  0 103 1 2 0 0 0 0 44 0 0 0 35 2 0 0 0
                                  0 47 0 0 0 36 3 0 7 8 0 22 25 2 0 10
                                  0 22 23 2 0 7 8 0 24 1 0 10 0 21 1 0
                                  0 0 45 1 0 0 0 1 2 0 0 0 56 1 2 0 0 0
                                  0 46 2 0 22 0 0 1 2 0 22 0 0 1 2 0 22
                                  0 0 40 2 0 22 0 0 1 2 0 22 0 0 49 2 0
                                  0 0 0 43 1 0 0 0 52 2 0 0 0 0 54 2 0
                                  0 0 0 53 2 0 0 0 56 57 2 0 0 0 103 1
                                  2 0 0 0 0 55 2 0 0 12 0 34 2 0 0 56 0
                                  1 2 0 0 103 0 1)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|SingleInteger| '|isFunctor|
             '(((|Or| ($ $ $)) T (ELT $ 47))
               ((|And| ($ $ $)) T (ELT $ 46))
               ((|Not| ($ $)) T (ELT $ 45))
               ((|xor| ($ $ $)) T (ELT $ 48))
               ((|not| ($ $)) T (ELT $ 42)) ((|min| ($)) T (ELT $ 39))
               ((|max| ($)) T (ELT $ 38))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $ (|Boolean|)))
                T (ELT $ 25))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $)) T
                (ELT $ 24))
               ((|OMwrite| ((|String|) $ (|Boolean|))) T (ELT $ 23))
               ((|OMwrite| ((|String|) $)) T (ELT $ 21))
               ((~ ($ $)) T (ELT $ 41)) ((|/\\| ($ $ $)) T (ELT $ 43))
               ((|\\/| ($ $ $)) T (ELT $ 44))
               ((|invmod| ($ $ $)) T (ELT $ NIL))
               ((|powmod| ($ $ $ $)) T (ELT $ NIL))
               ((|mulmod| ($ $ $ $)) T (ELT $ 72))
               ((|submod| ($ $ $ $)) T (ELT $ 74))
               ((|addmod| ($ $ $ $)) T (ELT $ 73))
               ((|mask| ($ $)) T (ELT $ NIL))
               ((|dec| ($ $)) T (ELT $ 51))
               ((|inc| ($ $)) T (ELT $ 50))
               ((|copy| ($ $)) T (ELT $ NIL))
               ((|hash| ($ $)) T (ELT $ 69))
               ((|random| ($ $)) T (ELT $ 83))
               ((|random| ($)) T (ELT $ 82))
               ((|rationalIfCan|
                    ((|Union| (|Fraction| (|Integer|)) "failed") $))
                T (ELT $ NIL))
               ((|rational| ((|Fraction| (|Integer|)) $)) T
                (ELT $ NIL))
               ((|rational?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|symmetricRemainder| ($ $ $)) T (ELT $ NIL))
               ((|positiveRemainder| ($ $ $)) T (ELT $ 80))
               ((|bit?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|shift| ($ $ $)) T (ELT $ 71))
               ((|length| ($ $)) T (ELT $ 70))
               ((|base| ($)) T (ELT $ 37))
               ((|even?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|odd?| ((|Boolean|) $)) T (ELT $ 64))
               ((|init| ($)) T (CONST $ NIL))
               ((|nextItem| ((|Union| $ "failed") $)) T (ELT $ NIL))
               ((|convert| ((|DoubleFloat|) $)) T (ELT $ NIL))
               ((|convert| ((|Float|) $)) T (ELT $ NIL))
               ((|permutation| ($ $ $)) T (ELT $ NIL))
               ((|factorial| ($ $)) T (ELT $ NIL))
               ((|binomial| ($ $ $)) T (ELT $ NIL))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Integer|) $) $
                     (|Pattern| (|Integer|))
                     (|PatternMatchResult| (|Integer|) $)))
                T (ELT $ NIL))
               ((|convert| ((|Pattern| (|Integer|)) $)) T (ELT $ NIL))
               ((|convert| ((|InputForm|) $)) T (ELT $ NIL))
               ((|reducedSystem| ((|Matrix| (|Integer|)) (|Matrix| $)))
                T (ELT $ 28))
               ((|reducedSystem|
                    ((|Record| (|:| |mat| (|Matrix| (|Integer|)))
                         (|:| |vec| (|Vector| (|Integer|))))
                     (|Matrix| $) (|Vector| $)))
                T (ELT $ 79))
               ((|retract| ((|Integer|) $)) T (ELT $ NIL))
               ((|retractIfCan| ((|Union| (|Integer|) "failed") $)) T
                (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 81))
               ((|convert| ((|Integer|) $)) T (ELT $ 32))
               ((|differentiate| ($ $)) T (ELT $ NIL))
               ((D ($ $)) T (ELT $ NIL))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((D ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|abs| ($ $)) T (ELT $ 63))
               ((|sign| ((|Integer|) $)) T (ELT $ NIL))
               ((|negative?| ((|Boolean|) $)) T (ELT $ 75))
               ((|positive?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|min| ($ $ $)) T (ELT $ 68))
               ((|max| ($ $ $)) T (ELT $ 67))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ 49))
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
                T (ELT $ 61))
               ((|quo| ($ $ $)) T (ELT $ 58))
               ((|rem| ($ $ $)) T (ELT $ 59))
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
               ((|factor| ((|Factored| $) $)) T (ELT $ NIL))
               ((|squareFreePart| ($ $)) T (ELT $ NIL))
               ((|squareFree| ((|Factored| $) $)) T (ELT $ NIL))
               ((|prime?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|gcdPolynomial|
                    ((|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)
                     (|SparseUnivariatePolynomial| $)))
                T (ELT $ NIL))
               ((|lcm| ($ (|List| $))) T (ELT $ NIL))
               ((|lcm| ($ $ $)) T (ELT $ NIL))
               ((|gcd| ($ (|List| $))) T (ELT $ NIL))
               ((|gcd| ($ $ $)) T (ELT $ 62))
               ((|unit?| ((|Boolean|) $)) T (ELT $ NIL))
               ((|associates?| ((|Boolean|) $ $)) T (ELT $ NIL))
               ((|unitCanonical| ($ $)) T (ELT $ NIL))
               ((|unitNormal|
                    ((|Record| (|:| |unit| $) (|:| |canonical| $)
                         (|:| |associate| $))
                     $))
                T (ELT $ 85))
               ((|exquo| ((|Union| $ "failed") $ $)) T (ELT $ NIL))
               ((|coerce| ($ $)) T (ELT $ NIL))
               ((|coerce| ($ (|Integer|))) T (ELT $ 81))
               ((|characteristic| ((|NonNegativeInteger|))) T
                (ELT $ NIL))
               ((|One| ($)) T (CONST $ 36))
               ((|one?| ((|Boolean|) $)) T (ELT $ 66))
               ((** ($ $ (|NonNegativeInteger|))) T (ELT $ 57))
               ((^ ($ $ (|NonNegativeInteger|))) T (ELT $ NIL))
               ((|recip| ((|Union| $ "failed") $)) T (ELT $ NIL))
               ((* ($ $ $)) T (ELT $ 55))
               ((** ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((^ ($ $ (|PositiveInteger|))) T (ELT $ NIL))
               ((* ($ (|Integer|) $)) T (ELT $ 34))
               ((- ($ $ $)) T (ELT $ 54)) ((- ($ $)) T (ELT $ 52))
               ((|subtractIfCan| ((|Union| $ "failed") $ $)) T
                (ELT $ NIL))
               ((* ($ (|NonNegativeInteger|) $)) T (ELT $ NIL))
               ((|zero?| ((|Boolean|) $)) T (ELT $ 65))
               ((|sample| ($)) T (CONST $ NIL))
               ((|Zero| ($)) T (CONST $ 35))
               ((* ($ (|PositiveInteger|) $)) T (ELT $ NIL))
               ((+ ($ $ $)) T (ELT $ 53))
               ((|latex| ((|String|) $)) T (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 31))
               ((= ((|Boolean|) $ $)) T (ELT $ 40))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|SingleInteger| '(|SingleInteger|)
                 '((|Join| (|IntegerNumberSystem|) (|Logic|)
                           (|OpenMath|)
                           (CATEGORY |domain| (ATTRIBUTE |canonical|)
                               (ATTRIBUTE |canonicalsClosed|)
                               (ATTRIBUTE |noetherian|)
                               (SIGNATURE |max| ($))
                               (SIGNATURE |min| ($))
                               (SIGNATURE |not| ($ $))
                               (SIGNATURE ~ ($ $))
                               (SIGNATURE |/\\| ($ $ $))
                               (SIGNATURE |\\/| ($ $ $))
                               (SIGNATURE |xor| ($ $ $))
                               (SIGNATURE |Not| ($ $))
                               (SIGNATURE |And| ($ $ $))
                               (SIGNATURE |Or| ($ $ $)))))
                 T '|SingleInteger|
                 (|put| '|SingleInteger| '|mode|
                        '(|Mapping|
                             (|Join| (|IntegerNumberSystem|) (|Logic|)
                                     (|OpenMath|)
                                     (CATEGORY |domain|
                                      (ATTRIBUTE |canonical|)
                                      (ATTRIBUTE |canonicalsClosed|)
                                      (ATTRIBUTE |noetherian|)
                                      (SIGNATURE |max| ($))
                                      (SIGNATURE |min| ($))
                                      (SIGNATURE |not| ($ $))
                                      (SIGNATURE ~ ($ $))
                                      (SIGNATURE |/\\| ($ $ $))
                                      (SIGNATURE |\\/| ($ $ $))
                                      (SIGNATURE |xor| ($ $ $))
                                      (SIGNATURE |Not| ($ $))
                                      (SIGNATURE |And| ($ $ $))
                                      (SIGNATURE |Or| ($ $ $)))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
