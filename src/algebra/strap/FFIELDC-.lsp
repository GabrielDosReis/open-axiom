
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;differentiate;2S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |FFIELDC-;init;S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;nextItem;SU;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;order;SOpc;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;conditionP;MU;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;charthRoot;2S;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;charthRoot;SU;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|)
                |FFIELDC-;createPrimitiveElement;S;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |FFIELDC-;primitive?;SB;9|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 1))
                |FFIELDC-;order;SPi;10|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) (|%IntegerSection| 0))
                |FFIELDC-;discreteLog;SNni;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Pair|)
                |FFIELDC-;discreteLog;2SU;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;squareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;factorPolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;factorSquareFreePolynomial|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |FFIELDC-;gcdPolynomial;3Sup;16|)) 

(DEFUN |FFIELDC-;differentiate;2S;1| (|x| $) (|spadConstant| $ 7)) 

(DEFUN |FFIELDC-;init;S;2| ($) (|spadConstant| $ 7)) 

(DEFUN |FFIELDC-;nextItem;SU;3| (|a| $)
  (COND
    ((SPADCALL
         (SETQ |a|
               (SPADCALL (+ (SPADCALL |a| (|shellEntry| $ 11)) 1)
                   (|shellEntry| $ 14)))
         (|shellEntry| $ 16))
     (CONS 1 "failed"))
    (T (CONS 0 |a|)))) 

(DEFUN |FFIELDC-;order;SOpc;4| (|e| $)
  (SPADCALL (SPADCALL |e| (|shellEntry| $ 19)) (|shellEntry| $ 22))) 

(DEFUN |FFIELDC-;conditionP;MU;5| (|mat| $)
  (LET ((|l| (SPADCALL |mat| (|shellEntry| $ 27))))
    (SEQ (COND
           ((OR (NULL |l|)
                (SPADCALL (ELT $ 16) (|SPADfirst| |l|)
                    (|shellEntry| $ 31)))
            (EXIT (CONS 1 "failed"))))
         (EXIT (CONS 0
                     (SPADCALL (ELT $ 32) (|SPADfirst| |l|)
                         (|shellEntry| $ 34))))))) 

(DEFUN |FFIELDC-;charthRoot;2S;6| (|x| $)
  (SPADCALL |x|
      (TRUNCATE (SPADCALL (|shellEntry| $ 40)) (|spadConstant| $ 41))
      (|shellEntry| $ 43))) 

(DEFUN |FFIELDC-;charthRoot;SU;7| (|x| $)
  (CONS 0 (SPADCALL |x| (|shellEntry| $ 32)))) 

(DEFUN |FFIELDC-;createPrimitiveElement;S;8| ($)
  (PROG (|e|)
    (RETURN
      (LET ((|sm1| (- (SPADCALL (|shellEntry| $ 40)) 1))
            (|start| (COND
                       ((SPADCALL (SPADCALL (|shellEntry| $ 48))
                            (CONS 1 "polynomial") (|shellEntry| $ 49))
                        (|spadConstant| $ 41))
                       (T 1)))
            (|found| NIL))
        (SEQ (LET ((|i| |start|))
               (LOOP
                 (COND
                   ((NOT (NOT |found|)) (RETURN NIL))
                   (T (SEQ (LETT |e|
                                 (SPADCALL
                                     (|check-subtype|
                                      (AND (NOT (MINUSP |i|))
                                       (PLUSP |i|))
                                      '(|PositiveInteger|) |i|)
                                     (|shellEntry| $ 14))
                                 |FFIELDC-;createPrimitiveElement;S;8|)
                           (EXIT (SETQ |found|
                                       (EQL
                                        (SPADCALL |e|
                                         (|shellEntry| $ 19))
                                        |sm1|))))))
                 (SETQ |i| (+ |i| 1))))
             (EXIT |e|)))))) 

(DEFUN |FFIELDC-;primitive?;SB;9| (|a| $)
  (COND
    ((SPADCALL |a| (|shellEntry| $ 16)) NIL)
    (T (LET ((|explist| (SPADCALL (|shellEntry| $ 56)))
             (|q| (- (SPADCALL (|shellEntry| $ 40)) 1))
             (|equalone| NIL))
         (SEQ (LET ((#0=#:G1488 |explist|) (|exp| NIL))
                (LOOP
                  (COND
                    ((OR (ATOM #0#) (PROGN (SETQ |exp| (CAR #0#)) NIL)
                         (NOT (NOT |equalone|)))
                     (RETURN NIL))
                    (T (SETQ |equalone|
                             (SPADCALL
                                 (SPADCALL |a|
                                     (TRUNCATE |q| (CAR |exp|))
                                     (|shellEntry| $ 58))
                                 (|shellEntry| $ 59)))))
                  (SETQ #0# (CDR #0#))))
              (EXIT (NOT |equalone|))))))) 

(DEFUN |FFIELDC-;order;SPi;10| (|e| $)
  (PROG (|primeDivisor| |a| |goon|)
    (RETURN
      (COND
        ((SPADCALL |e| (|spadConstant| $ 7) (|shellEntry| $ 63))
         (|error| "order(0) is not defined "))
        (T (LET ((|ord| (- (SPADCALL (|shellEntry| $ 40)) 1))
                 (|lof| (SPADCALL (|shellEntry| $ 56))))
             (SEQ (LET ((#0=#:G1489 |lof|))
                    (LOOP
                      (COND
                        ((ATOM #0#) (RETURN NIL))
                        (T (LET ((|rec| (CAR #0#)))
                             (SEQ (LETT |a|
                                        (TRUNCATE |ord|
                                         (LETT |primeDivisor|
                                          (CAR |rec|)
                                          |FFIELDC-;order;SPi;10|))
                                        |FFIELDC-;order;SPi;10|)
                                  (LETT |goon|
                                        (SPADCALL
                                         (SPADCALL |e| |a|
                                          (|shellEntry| $ 58))
                                         (|shellEntry| $ 59))
                                        |FFIELDC-;order;SPi;10|)
                                  (LET ((|j| 0)
                                        (#1=#:G1490 (- (CDR |rec|) 2)))
                                    (LOOP
                                      (COND
                                        ((OR (> |j| #1#) (NOT |goon|))
                                         (RETURN NIL))
                                        (T
                                         (SEQ (SETQ |ord| |a|)
                                          (SETQ |a|
                                           (TRUNCATE |ord|
                                            |primeDivisor|))
                                          (EXIT
                                           (SETQ |goon|
                                            (SPADCALL
                                             (SPADCALL |e| |a|
                                              (|shellEntry| $ 58))
                                             (|shellEntry| $ 59)))))))
                                      (SETQ |j| (+ |j| 1))))
                                  (EXIT (COND
                                          (|goon| (SETQ |ord| |a|))))))))
                      (SETQ #0# (CDR #0#))))
                  (EXIT |ord|)))))))) 

(DEFUN |FFIELDC-;discreteLog;SNni;11| (|b| $)
  (PROG (|rho| |exptable| |n| |c| |end| |found| |disc1| |fac| |disclog|
               |mult| |groupord| |exp|)
    (RETURN
      (COND
        ((SPADCALL |b| (|shellEntry| $ 16))
         (|error| "discreteLog: logarithm of zero"))
        (T (LET ((|faclist| (SPADCALL (|shellEntry| $ 56))) (|a| |b|)
                 (|gen| (SPADCALL (|shellEntry| $ 65))))
             (COND
               ((SPADCALL |b| |gen| (|shellEntry| $ 63)) 1)
               (T (SEQ (LETT |disclog| 0
                             |FFIELDC-;discreteLog;SNni;11|)
                       (LETT |mult| 1 |FFIELDC-;discreteLog;SNni;11|)
                       (LETT |groupord|
                             (- (SPADCALL (|shellEntry| $ 40)) 1)
                             |FFIELDC-;discreteLog;SNni;11|)
                       (LETT |exp| |groupord|
                             |FFIELDC-;discreteLog;SNni;11|)
                       (LET ((#0=#:G1491 |faclist|))
                         (LOOP
                           (COND
                             ((ATOM #0#) (RETURN NIL))
                             (T (LET ((|f| (CAR #0#)))
                                  (SEQ (LETT |fac| (CAR |f|)
                                        |FFIELDC-;discreteLog;SNni;11|)
                                       (EXIT
                                        (LET
                                         ((|t| 0)
                                          (#1=#:G1492 (- (CDR |f|) 1)))
                                          (LOOP
                                            (COND
                                              ((> |t| #1#)
                                               (RETURN NIL))
                                              (T
                                               (SEQ
                                                (SETQ |exp|
                                                 (TRUNCATE |exp| |fac|))
                                                (LETT |exptable|
                                                 (SPADCALL |fac|
                                                  (|shellEntry| $ 67))
                                                 |FFIELDC-;discreteLog;SNni;11|)
                                                (LETT |n|
                                                 (SPADCALL |exptable|
                                                  (|shellEntry| $ 68))
                                                 |FFIELDC-;discreteLog;SNni;11|)
                                                (LETT |c|
                                                 (SPADCALL |a| |exp|
                                                  (|shellEntry| $ 58))
                                                 |FFIELDC-;discreteLog;SNni;11|)
                                                (LETT |end|
                                                 (TRUNCATE (- |fac| 1)
                                                  |n|)
                                                 |FFIELDC-;discreteLog;SNni;11|)
                                                (LETT |found| NIL
                                                 |FFIELDC-;discreteLog;SNni;11|)
                                                (LETT |disc1| 0
                                                 |FFIELDC-;discreteLog;SNni;11|)
                                                (LET ((|i| 0))
                                                  (LOOP
                                                    (COND
                                                      ((OR
                                                        (> |i| |end|)
                                                        (NOT
                                                         (NOT |found|)))
                                                       (RETURN NIL))
                                                      (T
                                                       (SEQ
                                                        (LETT |rho|
                                                         (SPADCALL
                                                          (SPADCALL |c|
                                                           (|shellEntry|
                                                            $ 11))
                                                          |exptable|
                                                          (|shellEntry|
                                                           $ 71))
                                                         |FFIELDC-;discreteLog;SNni;11|)
                                                        (EXIT
                                                         (COND
                                                           ((ZEROP
                                                             (CAR
                                                              |rho|))
                                                            (SEQ
                                                             (SETQ
                                                              |found|
                                                              T)
                                                             (EXIT
                                                              (SETQ
                                                               |disc1|
                                                               (*
                                                                (+
                                                                 (* |n|
                                                                  |i|)
                                                                 (CDR
                                                                  |rho|))
                                                                |mult|)))))
                                                           (T
                                                            (SETQ |c|
                                                             (SPADCALL
                                                              |c|
                                                              (SPADCALL
                                                               |gen|
                                                               (*
                                                                (TRUNCATE
                                                                 |groupord|
                                                                 |fac|)
                                                                (- |n|))
                                                               (|shellEntry|
                                                                $ 58))
                                                              (|shellEntry|
                                                               $ 77)))))))))
                                                    (SETQ |i|
                                                     (+ |i| 1))))
                                                (EXIT
                                                 (COND
                                                   (|found|
                                                    (SEQ
                                                     (SETQ |mult|
                                                      (* |mult| |fac|))
                                                     (SETQ |disclog|
                                                      (+ |disclog|
                                                       |disc1|))
                                                     (EXIT
                                                      (SETQ |a|
                                                       (SPADCALL |a|
                                                        (SPADCALL |gen|
                                                         (- |disc1|)
                                                         (|shellEntry|
                                                          $ 58))
                                                        (|shellEntry| $
                                                         77))))))
                                                   (T
                                                    (|error|
                                                     "discreteLog: ?? discrete logarithm")))))))
                                            (SETQ |t| (+ |t| 1)))))))))
                           (SETQ #0# (CDR #0#))))
                       (EXIT |disclog|)))))))))) 

(DEFUN |FFIELDC-;discreteLog;2SU;12| (|logbase| |b| $)
  (PROG (|rhoHelp| |rho| |fac| |primroot| |groupord| |faclist| |a|
            |disclog| |mult| |exp|)
    (RETURN
      (COND
        ((SPADCALL |b| (|shellEntry| $ 16))
         (SEQ (SPADCALL "discreteLog: logarithm of zero"
                  (|shellEntry| $ 83))
              (EXIT (CONS 1 "failed"))))
        ((SPADCALL |logbase| (|shellEntry| $ 16))
         (SEQ (SPADCALL "discreteLog: logarithm to base zero"
                  (|shellEntry| $ 83))
              (EXIT (CONS 1 "failed"))))
        ((SPADCALL |b| |logbase| (|shellEntry| $ 63)) (CONS 0 1))
        (T (COND
             ((NOT (ZEROP (REM (LETT |groupord|
                                     (SPADCALL |logbase|
                                      (|shellEntry| $ 19))
                                     |FFIELDC-;discreteLog;2SU;12|)
                               (SPADCALL |b| (|shellEntry| $ 19)))))
              (SEQ (SPADCALL
                       "discreteLog: second argument not in cyclic group generated by first argument"
                       (|shellEntry| $ 83))
                   (EXIT (CONS 1 "failed"))))
             (T (SEQ (LETT |faclist|
                           (SPADCALL
                               (SPADCALL |groupord|
                                   (|shellEntry| $ 87))
                               (|shellEntry| $ 89))
                           |FFIELDC-;discreteLog;2SU;12|)
                     (LETT |a| |b| |FFIELDC-;discreteLog;2SU;12|)
                     (LETT |disclog| 0 |FFIELDC-;discreteLog;2SU;12|)
                     (LETT |mult| 1 |FFIELDC-;discreteLog;2SU;12|)
                     (LETT |exp| |groupord|
                           |FFIELDC-;discreteLog;2SU;12|)
                     (LET ((#0=#:G1493 |faclist|))
                       (LOOP
                         (COND
                           ((ATOM #0#) (RETURN NIL))
                           (T (LET ((|f| (CAR #0#)))
                                (SEQ (LETT |fac| (CAR |f|)
                                      |FFIELDC-;discreteLog;2SU;12|)
                                     (LETT |primroot|
                                      (SPADCALL |logbase|
                                       (TRUNCATE |groupord| |fac|)
                                       (|shellEntry| $ 58))
                                      |FFIELDC-;discreteLog;2SU;12|)
                                     (EXIT
                                      (LET
                                       ((|t| 0)
                                        (#1=#:G1494 (- (CDR |f|) 1)))
                                        (LOOP
                                          (COND
                                            ((> |t| #1#) (RETURN NIL))
                                            (T
                                             (SEQ
                                              (SETQ |exp|
                                               (TRUNCATE |exp| |fac|))
                                              (LETT |rhoHelp|
                                               (SPADCALL |primroot|
                                                (SPADCALL |a| |exp|
                                                 (|shellEntry| $ 58))
                                                |fac|
                                                (|shellEntry| $ 91))
                                               |FFIELDC-;discreteLog;2SU;12|)
                                              (EXIT
                                               (COND
                                                 ((EQL (CAR |rhoHelp|)
                                                   1)
                                                  (RETURN-FROM
                                                   |FFIELDC-;discreteLog;2SU;12|
                                                    (CONS 1 "failed")))
                                                 (T
                                                  (SEQ
                                                   (LETT |rho|
                                                    (SPADCALL
                                                     (CDR |rhoHelp|)
                                                     |mult|
                                                     (|shellEntry| $
                                                      92))
                                                    |FFIELDC-;discreteLog;2SU;12|)
                                                   (SETQ |disclog|
                                                    (+ |disclog| |rho|))
                                                   (SETQ |mult|
                                                    (* |mult| |fac|))
                                                   (EXIT
                                                    (SETQ |a|
                                                     (SPADCALL |a|
                                                      (SPADCALL
                                                       |logbase|
                                                       (- |rho|)
                                                       (|shellEntry| $
                                                        58))
                                                      (|shellEntry| $
                                                       77)))))))))))
                                          (SETQ |t| (+ |t| 1)))))))))
                         (SETQ #0# (CDR #0#))))
                     (EXIT (CONS 0 |disclog|)))))))))) 

(DEFUN |FFIELDC-;squareFreePolynomial| (|f| $)
  (SPADCALL |f| (|shellEntry| $ 97))) 

(DEFUN |FFIELDC-;factorPolynomial| (|f| $)
  (SPADCALL |f| (|shellEntry| $ 99))) 

(DEFUN |FFIELDC-;factorSquareFreePolynomial| (|f| $)
  (COND
    ((SPADCALL |f| (|spadConstant| $ 100) (|shellEntry| $ 101))
     (|spadConstant| $ 102))
    (T (LET ((|flist| (SPADCALL |f| T (|shellEntry| $ 106))))
         (SPADCALL (SPADCALL (CAR |flist|) (|shellEntry| $ 107))
             (LET ((#0=#:G1483 NIL) (#1=#:G1484 T)
                   (#2=#:G1495 (CDR |flist|)))
               (LOOP
                 (COND
                   ((ATOM #2#)
                    (RETURN
                      (COND (#1# (|spadConstant| $ 110)) (T #0#))))
                   (T (LET ((|u| (CAR #2#)))
                        (LET ((#3=#:G1482
                                  (SPADCALL (CAR |u|) (CDR |u|)
                                      (|shellEntry| $ 108))))
                          (COND
                            (#1# (SETQ #0# #3#))
                            (T (SETQ #0#
                                     (SPADCALL #0# #3#
                                      (|shellEntry| $ 109)))))
                          (SETQ #1# NIL)))))
                 (SETQ #2# (CDR #2#))))
             (|shellEntry| $ 111)))))) 

(DEFUN |FFIELDC-;gcdPolynomial;3Sup;16| (|f| |g| $)
  (SPADCALL |f| |g| (|shellEntry| $ 113))) 

(DEFUN |FiniteFieldCategory&| (|#1|)
  (LET* ((|dv$1| (|devaluate| |#1|))
         (|dv$| (LIST '|FiniteFieldCategory&| |dv$1|))
         ($ (|newShell| 116)) (|pv$| (|buildPredVector| 0 0 NIL)))
    (SETF (|shellEntry| $ 0) |dv$|)
    (SETF (|shellEntry| $ 3) |pv$|)
    (|stuffDomainSlots| $)
    (SETF (|shellEntry| $ 6) |#1|)
    $)) 

(MAKEPROP '|FiniteFieldCategory&| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|local| |#1|) (0 . |Zero|)
             |FFIELDC-;differentiate;2S;1| |FFIELDC-;init;S;2|
             (|PositiveInteger|) (4 . |lookup|) (9 . |One|) (13 . +)
             (19 . |index|) (|Boolean|) (24 . |zero?|)
             (|Union| $ '"failed") |FFIELDC-;nextItem;SU;3|
             (29 . |order|) (|Integer|) (|OnePointCompletion| 10)
             (34 . |coerce|) |FFIELDC-;order;SOpc;4| (|Vector| 6)
             (|List| 24) (|Matrix| 6) (39 . |nullSpace|)
             (44 . |empty?|) (49 . |first|) (|Mapping| 15 6)
             (54 . |every?|) (60 . |charthRoot|) (|Mapping| 6 6)
             (65 . |map|) (|Vector| $) (|Union| 35 '"failed")
             (|Matrix| $) |FFIELDC-;conditionP;MU;5|
             (|NonNegativeInteger|) (71 . |size|)
             (75 . |characteristic|) (79 . |quo|) (85 . **)
             |FFIELDC-;charthRoot;2S;6| |FFIELDC-;charthRoot;SU;7|
             (91 . -)
             (|Union| '"prime" '"polynomial" '"normal" '"cyclic")
             (97 . |representationType|) (101 . =) (107 . |false|)
             (111 . |not|) (116 . =)
             |FFIELDC-;createPrimitiveElement;S;8|
             (|Record| (|:| |factor| 20) (|:| |exponent| 20))
             (|List| 54) (122 . |factorsOfCyclicGroupSize|)
             (126 . |quo|) (132 . **) (138 . |one?|)
             |FFIELDC-;primitive?;SB;9| (143 . |Zero|) (147 . |Zero|)
             (151 . =) |FFIELDC-;order;SPi;10|
             (157 . |primitiveElement|) (|Table| 10 39)
             (161 . |tableForDiscreteLogarithm|) (166 . |#|)
             (171 . |One|) (|Union| 39 '"failed") (175 . |search|)
             (181 . |true|) (185 . *) (191 . +) (197 . *) (203 . -)
             (208 . *) (214 . +) |FFIELDC-;discreteLog;SNni;11|
             (|Void|) (|String|) (|OutputForm|) (220 . |messagePrint|)
             (225 . |rem|) (231 . |zero?|) (|Factored| $)
             (236 . |factor|) (|Factored| 20) (241 . |factors|)
             (|DiscreteLogarithmPackage| 6)
             (246 . |shanksDiscLogAlgorithm|) (253 . *)
             |FFIELDC-;discreteLog;2SU;12|
             (|SparseUnivariatePolynomial| 6) (|Factored| 94)
             (|UnivariatePolynomialSquareFree| 6 94)
             (259 . |squareFree|) (|DistinctDegreeFactorize| 6 94)
             (264 . |factor|) (269 . |Zero|) (273 . =) (279 . |Zero|)
             (|Record| (|:| |irr| 94) (|:| |pow| 20)) (|List| 103)
             (|Record| (|:| |cont| 6) (|:| |factors| 104))
             (283 . |distdfact|) (289 . |coerce|) (294 . |primeFactor|)
             (300 . *) (306 . |One|) (310 . *) (|EuclideanDomain&| 94)
             (316 . |gcd|) (|SparseUnivariatePolynomial| $)
             |FFIELDC-;gcdPolynomial;3Sup;16|)
          '#(|primitive?| 322 |order| 327 |nextItem| 337 |init| 342
             |gcdPolynomial| 346 |discreteLog| 352 |differentiate| 363
             |createPrimitiveElement| 368 |conditionP| 372 |charthRoot|
             377)
          'NIL
          (CONS (|makeByteWordVec2| 1 'NIL)
                (CONS '#()
                      (CONS '#()
                            (|makeByteWordVec2| 115
                                '(0 6 0 7 1 6 10 0 11 0 10 0 12 2 10 0
                                  0 0 13 1 6 0 10 14 1 6 15 0 16 1 6 10
                                  0 19 1 21 0 20 22 1 26 25 0 27 1 25
                                  15 0 28 1 25 24 0 29 2 24 15 30 0 31
                                  1 6 0 0 32 2 24 0 33 0 34 0 6 39 40 0
                                  6 39 41 2 39 0 0 0 42 2 6 0 0 39 43 2
                                  20 0 0 0 46 0 6 47 48 2 47 15 0 0 49
                                  0 15 0 50 1 15 0 0 51 2 10 15 0 0 52
                                  0 6 55 56 2 20 0 0 0 57 2 6 0 0 20 58
                                  1 6 15 0 59 0 39 0 61 0 20 0 62 2 6
                                  15 0 0 63 0 6 0 65 1 6 66 20 67 1 66
                                  39 0 68 0 39 0 69 2 66 70 10 0 71 0
                                  15 0 72 2 39 0 39 0 73 2 39 0 0 0 74
                                  2 20 0 20 0 75 1 20 0 0 76 2 6 0 0 0
                                  77 2 20 0 0 0 78 1 82 80 81 83 2 39 0
                                  0 0 84 1 39 15 0 85 1 20 86 0 87 1 88
                                  55 0 89 3 90 70 6 6 39 91 2 20 0 39 0
                                  92 1 96 95 94 97 1 98 95 94 99 0 94 0
                                  100 2 94 15 0 0 101 0 95 0 102 2 98
                                  105 94 15 106 1 94 0 6 107 2 95 0 94
                                  20 108 2 95 0 0 0 109 0 95 0 110 2 95
                                  0 94 0 111 2 112 0 0 0 113 1 0 15 0
                                  60 1 0 10 0 64 1 0 21 0 23 1 0 17 0
                                  18 0 0 0 9 2 0 114 114 114 115 1 0 39
                                  0 79 2 0 70 0 0 93 1 0 0 0 8 0 0 0 53
                                  1 0 36 37 38 1 0 0 0 44 1 0 17 0 45)))))
          '|lookupComplete|)) 
