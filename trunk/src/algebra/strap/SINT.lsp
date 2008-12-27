
(/VERSIONCHECK 2) 

(SETQ |$CategoryFrame|
      (|put| #0='|SingleInteger| '|SuperDomain| #1='(|Integer|)
             (|put| #1# '|SubDomain|
                    (CONS '(|SingleInteger| SMINTP |#1|)
                          (DELASC #0#
                                  (|get| #1# '|SubDomain|
                                         |$CategoryFrame|)))
                    |$CategoryFrame|))) 

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

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;inc;2$;25|)) 

(PUT '|SINT;inc;2$;25| '|SPADreplace| 'QSADD1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;dec;2$;26|)) 

(PUT '|SINT;dec;2$;26| '|SPADreplace| 'QSSUB1) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|) |SINT;-;2$;27|)) 

(PUT '|SINT;-;2$;27| '|SPADreplace| 'QSMINUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;+;3$;28|)) 

(PUT '|SINT;+;3$;28| '|SPADreplace| 'QSPLUS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;-;3$;29|)) 

(PUT '|SINT;-;3$;29| '|SPADreplace| 'QSDIFFERENCE) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;*;3$;30|)) 

(PUT '|SINT;*;3$;30| '|SPADreplace| 'QSTIMES) 

(DECLAIM (FTYPE (FUNCTION (|%Short| (|%IntegerSection| 0) |%Shell|)
                    |%Short|)
                |SINT;**;$Nni$;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;quo;3$;32|)) 

(PUT '|SINT;quo;3$;32| '|SPADreplace| 'QSQUOTIENT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;rem;3$;33|)) 

(PUT '|SINT;rem;3$;33| '|SPADreplace| 'QSREMAINDER) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Pair|)
                |SINT;divide;2$R;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;gcd;3$;35|)) 

(PUT '|SINT;gcd;3$;35| '|SPADreplace| 'GCD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;abs;2$;36|)) 

(PUT '|SINT;abs;2$;36| '|SPADreplace| 'QSABSVAL) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;odd?;$B;37|)) 

(PUT '|SINT;odd?;$B;37| '|SPADreplace| 'QSODDP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;zero?;$B;38|)) 

(PUT '|SINT;zero?;$B;38| '|SPADreplace| 'QSZEROP) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;one?;$B;39|)) 

(PUT '|SINT;one?;$B;39| '|SPADreplace| '(XLAM (|x|) (EQL |x| 1))) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;max;3$;40|)) 

(PUT '|SINT;max;3$;40| '|SPADreplace| 'QSMAX) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;min;3$;41|)) 

(PUT '|SINT;min;3$;41| '|SPADreplace| 'QSMIN) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;hash;2$;42|)) 

(PUT '|SINT;hash;2$;42| '|SPADreplace| 'HASHEQ) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;length;2$;43|)) 

(PUT '|SINT;length;2$;43| '|SPADreplace| 'INTEGER-LENGTH) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;shift;3$;44|)) 

(PUT '|SINT;shift;3$;44| '|SPADreplace| 'QSLEFTSHIFT) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;mulmod;4$;45|)) 

(PUT '|SINT;mulmod;4$;45| '|SPADreplace| 'QSMULTMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;addmod;4$;46|)) 

(PUT '|SINT;addmod;4$;46| '|SPADreplace| 'QSADDMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Short| |%Shell|)
                    |%Short|)
                |SINT;submod;4$;47|)) 

(PUT '|SINT;submod;4$;47| '|SPADreplace| 'QSDIFMOD) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Boolean|)
                |SINT;negative?;$B;48|)) 

(PUT '|SINT;negative?;$B;48| '|SPADreplace| 'QSMINUSP) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) (|%IntegerSection| 0))
                |SINT;size;Nni;49|)) 

(DECLAIM (FTYPE (FUNCTION ((|%IntegerSection| 1) |%Shell|) |%Short|)
                |SINT;index;Pi$;50|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) (|%IntegerSection| 1))
                |SINT;lookup;$Pi;51|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%Vector| *) |%Shell|) |%Pair|)
                |SINT;reducedSystem;MVR;52|)) 

(PUT '|SINT;reducedSystem;MVR;52| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Short| |%Shell|) |%Short|)
                |SINT;positiveRemainder;3$;53|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Short|)
                |SINT;coerce;I$;54|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Short|) |SINT;random;$;55|)) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Short|)
                |SINT;random;2$;56|)) 

(PUT '|SINT;random;2$;56| '|SPADreplace| 'RANDOM) 

(DECLAIM (FTYPE (FUNCTION (|%Short| |%Shell|) |%Shell|)
                |SINT;unitNormal;$R;57|)) 

(DEFUN |SINT;writeOMSingleInt| (|dev| |x| $)
  (SEQ (COND
         ((QSLESSP |x| 0)
          (SEQ (SPADCALL |dev| (|getShellEntry| $ 9))
               (SPADCALL |dev| "arith1" "unaryminus"
                   (|getShellEntry| $ 11))
               (SPADCALL |dev| (QSMINUS |x|) (|getShellEntry| $ 12))
               (EXIT (SPADCALL |dev| (|getShellEntry| $ 13)))))
         ('T (SPADCALL |dev| |x| (|getShellEntry| $ 12)))))) 

(DEFUN |SINT;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 15))
                     (|getShellEntry| $ 16))
                 |SINT;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 17))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 18))
           (SPADCALL |dev| (|getShellEntry| $ 19))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SINT;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SINT;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 15))
                     (|getShellEntry| $ 16))
                 |SINT;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 17))))
           (|SINT;writeOMSingleInt| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))
           (SPADCALL |dev| (|getShellEntry| $ 19))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SINT;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |SINT;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 17))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 18))))) 

(DEFUN |SINT;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 17))))
       (|SINT;writeOMSingleInt| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 18))))))) 

(DEFUN |SINT;reducedSystem;MM;6| (|m| $) (DECLARE (IGNORE $)) |m|) 

(DEFUN |SINT;coerce;$Of;7| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 29))) 

(DEFUN |SINT;convert;$I;8| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |SINT;*;I2$;9| (|i| |y| $)
  (QSTIMES (SPADCALL |i| (|getShellEntry| $ 32)) |y|)) 

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

(DEFUN |SINT;inc;2$;25| (|x| $) (DECLARE (IGNORE $)) (QSADD1 |x|)) 

(DEFUN |SINT;dec;2$;26| (|x| $) (DECLARE (IGNORE $)) (QSSUB1 |x|)) 

(DEFUN |SINT;-;2$;27| (|x| $) (DECLARE (IGNORE $)) (QSMINUS |x|)) 

(DEFUN |SINT;+;3$;28| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSPLUS |x| |y|)) 

(DEFUN |SINT;-;3$;29| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSDIFFERENCE |x| |y|)) 

(DEFUN |SINT;*;3$;30| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSTIMES |x| |y|)) 

(DEFUN |SINT;**;$Nni$;31| (|x| |n| $)
  (SPADCALL (EXPT |x| |n|) (|getShellEntry| $ 32))) 

(DEFUN |SINT;quo;3$;32| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSQUOTIENT |x| |y|)) 

(DEFUN |SINT;rem;3$;33| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSREMAINDER |x| |y|)) 

(DEFUN |SINT;divide;2$R;34| (|x| |y| $)
  (CONS (QSQUOTIENT |x| |y|) (QSREMAINDER |x| |y|))) 

(DEFUN |SINT;gcd;3$;35| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GCD |x| |y|)) 

(DEFUN |SINT;abs;2$;36| (|x| $) (DECLARE (IGNORE $)) (QSABSVAL |x|)) 

(DEFUN |SINT;odd?;$B;37| (|x| $) (DECLARE (IGNORE $)) (QSODDP |x|)) 

(DEFUN |SINT;zero?;$B;38| (|x| $) (DECLARE (IGNORE $)) (QSZEROP |x|)) 

(DEFUN |SINT;one?;$B;39| (|x| $) (DECLARE (IGNORE $)) (EQL |x| 1)) 

(DEFUN |SINT;max;3$;40| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMAX |x| |y|)) 

(DEFUN |SINT;min;3$;41| (|x| |y| $)
  (DECLARE (IGNORE $))
  (QSMIN |x| |y|)) 

(DEFUN |SINT;hash;2$;42| (|x| $) (DECLARE (IGNORE $)) (HASHEQ |x|)) 

(DEFUN |SINT;length;2$;43| (|x| $)
  (DECLARE (IGNORE $))
  (INTEGER-LENGTH |x|)) 

(DEFUN |SINT;shift;3$;44| (|x| |n| $)
  (DECLARE (IGNORE $))
  (QSLEFTSHIFT |x| |n|)) 

(DEFUN |SINT;mulmod;4$;45| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSMULTMOD |a| |b| |p|)) 

(DEFUN |SINT;addmod;4$;46| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSADDMOD |a| |b| |p|)) 

(DEFUN |SINT;submod;4$;47| (|a| |b| |p| $)
  (DECLARE (IGNORE $))
  (QSDIFMOD |a| |b| |p|)) 

(DEFUN |SINT;negative?;$B;48| (|x| $)
  (DECLARE (IGNORE $))
  (QSMINUSP |x|)) 

(DEFUN |SINT;size;Nni;49| ($)
  (QSPLUS (QSDIFFERENCE |$ShortMaximum| |$ShortMinimum|) 1)) 

(DEFUN |SINT;index;Pi$;50| (|i| $)
  (COND
    ((< (|SINT;size;Nni;49| $) |i|)
     (|error| (LIST "index %1b out of range"
                    (SPADCALL |i| (|getShellEntry| $ 78)))))
    ('T (- (+ |i| |$ShortMinimum|) 1)))) 

(DEFUN |SINT;lookup;$Pi;51| (|x| $)
  (QSPLUS (QSDIFFERENCE |x| |$ShortMinimum|) 1)) 

(DEFUN |SINT;reducedSystem;MVR;52| (|m| |v| $)
  (DECLARE (IGNORE $))
  (CONS |m| |v|)) 

(DEFUN |SINT;positiveRemainder;3$;53| (|x| |n| $)
  (PROG (|r|)
    (RETURN
      (SEQ (LETT |r| (QSREMAINDER |x| |n|)
                 |SINT;positiveRemainder;3$;53|)
           (EXIT (COND
                   ((QSMINUSP |r|)
                    (COND
                      ((QSMINUSP |n|) (QSDIFFERENCE |x| |n|))
                      ('T (QSPLUS |r| |n|))))
                   ('T |r|))))))) 

(DEFUN |SINT;coerce;I$;54| (|x| $)
  (SEQ (COND
         ((NULL (< 2147483647 |x|))
          (COND ((NULL (< |x| -2147483648)) (EXIT |x|)))))
       (EXIT (|error| "integer too large to represent in a machine word")))) 

(DEFUN |SINT;random;$;55| ($)
  (SEQ (|setShellEntry| $ 6
           (REMAINDER (TIMES 314159269 (|getShellEntry| $ 6))
               2147483647))
       (EXIT (REMAINDER (|getShellEntry| $ 6) 67108864)))) 

(DEFUN |SINT;random;2$;56| (|n| $) (DECLARE (IGNORE $)) (RANDOM |n|)) 

(DEFUN |SINT;unitNormal;$R;57| (|x| $)
  (COND
    ((QSLESSP |x| 0) (VECTOR -1 (QSMINUS |x|) -1))
    ('T (VECTOR 1 |x| 1)))) 

(DEFUN |SingleInteger| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1495)
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
        (LETT $ (|newShell| 108) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|SingleInteger| NIL
            (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 1)
        $)))) 

(MAKEPROP '|SingleInteger| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL (|Integer|) '|seed| (|Void|)
             (|OpenMathDevice|) (0 . |OMputApp|) (|String|)
             (5 . |OMputSymbol|) (12 . |OMputInteger|)
             (18 . |OMputEndApp|) (|OpenMathEncoding|)
             (23 . |OMencodingXML|) (27 . |OMopenString|)
             (33 . |OMputObject|) (38 . |OMputEndObject|)
             (43 . |OMclose|) |SINT;OMwrite;$S;2| (|Boolean|)
             |SINT;OMwrite;$BS;3| |SINT;OMwrite;Omd$V;4|
             |SINT;OMwrite;Omd$BV;5| (|Matrix| 5) (|Matrix| $)
             |SINT;reducedSystem;MM;6| (|OutputForm|) (48 . |coerce|)
             |SINT;coerce;$Of;7| |SINT;convert;$I;8| (53 . |coerce|)
             |SINT;*;I2$;9|
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SINT;Zero;$;10|) $))
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
             |SINT;<;2$B;24| |SINT;inc;2$;25| |SINT;dec;2$;26|
             |SINT;-;2$;27| |SINT;+;3$;28| |SINT;-;3$;29|
             |SINT;*;3$;30| (|NonNegativeInteger|) |SINT;**;$Nni$;31|
             |SINT;quo;3$;32| |SINT;rem;3$;33|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             |SINT;divide;2$R;34| |SINT;gcd;3$;35| |SINT;abs;2$;36|
             |SINT;odd?;$B;37| |SINT;zero?;$B;38| |SINT;one?;$B;39|
             |SINT;max;3$;40| |SINT;min;3$;41| (|SingleInteger|)
             |SINT;hash;2$;42| |SINT;length;2$;43| |SINT;shift;3$;44|
             |SINT;mulmod;4$;45| |SINT;addmod;4$;46|
             |SINT;submod;4$;47| |SINT;negative?;$B;48|
             |SINT;size;Nni;49| (|PositiveInteger|) (58 . |coerce|)
             |SINT;index;Pi$;50| |SINT;lookup;$Pi;51| (|Vector| 5)
             (|Record| (|:| |mat| 25) (|:| |vec| 81)) (|Vector| $)
             |SINT;reducedSystem;MVR;52| |SINT;positiveRemainder;3$;53|
             |SINT;coerce;I$;54| |SINT;random;$;55| |SINT;random;2$;56|
             (|Record| (|:| |unit| $) (|:| |canonical| $)
                 (|:| |associate| $))
             |SINT;unitNormal;$R;57| (|Fraction| 5)
             (|Union| 91 '"failed") (|Union| $ '"failed") (|Float|)
             (|DoubleFloat|) (|Pattern| 5) (|PatternMatchResult| 5 $)
             (|InputForm|) (|Union| 5 '"failed") (|List| $)
             (|Record| (|:| |coef| 100) (|:| |generator| $))
             (|Union| 100 '"failed")
             (|Record| (|:| |coef1| $) (|:| |coef2| $)
                 (|:| |generator| $))
             (|Record| (|:| |coef1| $) (|:| |coef2| $))
             (|Union| 104 '"failed") (|Factored| $)
             (|SparseUnivariatePolynomial| $))
          '#(~= 63 ~ 69 |zero?| 74 |xor| 79 |unitNormal| 85
             |unitCanonical| 90 |unit?| 95 |symmetricRemainder| 100
             |subtractIfCan| 106 |submod| 112 |squareFreePart| 119
             |squareFree| 124 |sizeLess?| 129 |size| 135 |sign| 139
             |shift| 144 |sample| 150 |retractIfCan| 154 |retract| 159
             |rem| 164 |reducedSystem| 170 |recip| 181 |rationalIfCan|
             186 |rational?| 191 |rational| 196 |random| 201 |quo| 210
             |principalIdeal| 216 |prime?| 221 |powmod| 226
             |positiveRemainder| 233 |positive?| 239 |permutation| 244
             |patternMatch| 250 |one?| 257 |odd?| 262 |not| 267
             |nextItem| 272 |negative?| 277 |multiEuclidean| 282
             |mulmod| 288 |min| 295 |max| 305 |mask| 315 |lookup| 320
             |length| 325 |lcm| 330 |latex| 341 |invmod| 346 |init| 352
             |index| 356 |inc| 361 |hash| 366 |gcdPolynomial| 371 |gcd|
             377 |factorial| 388 |factor| 393 |extendedEuclidean| 398
             |exquo| 411 |expressIdealMember| 417 |even?| 423
             |euclideanSize| 428 |divide| 433 |differentiate| 439 |dec|
             450 |copy| 455 |convert| 460 |coerce| 485 |characteristic|
             505 |bit?| 509 |binomial| 515 |base| 521 |associates?| 525
             |addmod| 531 |abs| 538 |\\/| 543 |Zero| 549 |Or| 553 |One|
             559 |OMwrite| 563 |Not| 587 D 592 |And| 603 >= 609 > 615 =
             621 <= 627 < 633 |/\\| 639 - 645 + 656 ** 662 * 674)
          '((|noetherian| . 0) (|canonicalsClosed| . 0)
            (|canonical| . 0) (|canonicalUnitNormal| . 0)
            (|multiplicativeValuation| . 0) (|noZeroDivisors| . 0)
            ((|commutative| "*") . 0) (|rightUnitary| . 0)
            (|leftUnitary| . 0) (|unitsKnown| . 0))
          (CONS (|makeByteWordVec2| 1
                    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                      0))
                (CONS '#(|IntegerNumberSystem&| |EuclideanDomain&|
                         |UniqueFactorizationDomain&| NIL NIL
                         |GcdDomain&| |IntegralDomain&| |Algebra&| NIL
                         NIL |DifferentialRing&| |OrderedRing&| NIL NIL
                         |Module&| NIL |Ring&| NIL NIL NIL NIL NIL
                         |AbelianGroup&| NIL NIL NIL NIL
                         |AbelianMonoid&| |Monoid&| NIL NIL NIL
                         |OrderedSet&| |AbelianSemiGroup&| |SemiGroup&|
                         |Logic&| NIL |RetractableTo&| |SetCategory&|
                         NIL NIL NIL NIL NIL NIL NIL NIL NIL
                         |BasicType&| NIL)
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
                               (|AbelianMonoid|) (|Monoid|) (|Finite|)
                               (|StepThrough|) (|PatternMatchable| 5)
                               (|OrderedSet|) (|AbelianSemiGroup|)
                               (|SemiGroup|) (|Logic|) (|RealConstant|)
                               (|RetractableTo| 5) (|SetCategory|)
                               (|OpenMath|) (|ConvertibleTo| 94)
                               (|ConvertibleTo| 95)
                               (|CombinatorialFunctionCategory|)
                               (|ConvertibleTo| 96)
                               (|ConvertibleTo| 98) (|ConvertibleTo| 5)
                               (|CoercibleFrom| $$) (|CoercibleFrom| 5)
                               (|BasicType|) (|CoercibleTo| 28))
                            (|makeByteWordVec2| 107
                                '(1 8 7 0 9 3 8 7 0 10 10 11 2 8 7 0 5
                                  12 1 8 7 0 13 0 14 0 15 2 8 0 10 14
                                  16 1 8 7 0 17 1 8 7 0 18 1 8 7 0 19 1
                                  5 28 0 29 1 0 0 5 32 1 77 28 0 78 2 0
                                  21 0 0 1 1 0 0 0 40 1 0 21 0 64 2 0 0
                                  0 0 47 1 0 89 0 90 1 0 0 0 1 1 0 21 0
                                  1 2 0 0 0 0 1 2 0 93 0 0 1 3 0 0 0 0
                                  0 74 1 0 0 0 1 1 0 106 0 1 2 0 21 0 0
                                  1 0 0 55 76 1 0 5 0 1 2 0 0 0 0 71 0
                                  0 0 1 1 0 99 0 1 1 0 5 0 1 2 0 0 0 0
                                  58 1 0 25 26 27 2 0 82 26 83 84 1 0
                                  93 0 1 1 0 92 0 1 1 0 21 0 1 1 0 91 0
                                  1 1 0 0 0 88 0 0 0 87 2 0 0 0 0 57 1
                                  0 101 100 1 1 0 21 0 1 3 0 0 0 0 0 1
                                  2 0 0 0 0 85 1 0 21 0 1 2 0 0 0 0 1 3
                                  0 97 0 96 97 1 1 0 21 0 65 1 0 21 0
                                  63 1 0 0 0 41 1 0 93 0 1 1 0 21 0 75
                                  2 0 102 100 0 1 3 0 0 0 0 0 72 0 0 0
                                  38 2 0 0 0 0 67 0 0 0 37 2 0 0 0 0 66
                                  1 0 0 0 1 1 0 77 0 80 1 0 0 0 70 1 0
                                  0 100 1 2 0 0 0 0 1 1 0 10 0 1 2 0 0
                                  0 0 1 0 0 0 1 1 0 0 77 79 1 0 0 0 49
                                  1 0 68 0 69 2 0 107 107 107 1 1 0 0
                                  100 1 2 0 0 0 0 61 1 0 0 0 1 1 0 106
                                  0 1 2 0 103 0 0 1 3 0 105 0 0 0 1 2 0
                                  93 0 0 1 2 0 102 100 0 1 1 0 21 0 1 1
                                  0 55 0 1 2 0 59 0 0 60 1 0 0 0 1 2 0
                                  0 0 55 1 1 0 0 0 50 1 0 0 0 1 1 0 94
                                  0 1 1 0 95 0 1 1 0 96 0 1 1 0 98 0 1
                                  1 0 5 0 31 1 0 0 5 86 1 0 0 0 1 1 0 0
                                  5 86 1 0 28 0 30 0 0 55 1 2 0 21 0 0
                                  1 2 0 0 0 0 1 0 0 0 36 2 0 21 0 0 1 3
                                  0 0 0 0 0 73 1 0 0 0 62 2 0 0 0 0 43
                                  0 0 0 34 2 0 0 0 0 46 0 0 0 35 2 0 7
                                  8 0 23 3 0 7 8 0 21 24 2 0 10 0 21 22
                                  1 0 10 0 20 1 0 0 0 44 1 0 0 0 1 2 0
                                  0 0 55 1 2 0 0 0 0 45 2 0 21 0 0 1 2
                                  0 21 0 0 1 2 0 21 0 0 39 2 0 21 0 0 1
                                  2 0 21 0 0 48 2 0 0 0 0 42 1 0 0 0 51
                                  2 0 0 0 0 53 2 0 0 0 0 52 2 0 0 0 55
                                  56 2 0 0 0 77 1 2 0 0 0 0 54 2 0 0 5
                                  0 33 2 0 0 55 0 1 2 0 0 77 0 1)))))
          '|lookupComplete|)) 

(MAKEPROP '|SingleInteger| 'NILADIC T) 
