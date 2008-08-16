
(/VERSIONCHECK 2) 

(PUT '|OUTFORM;print;$V;1| '|SPADreplace| '|mathprint|) 

(DEFUN |OUTFORM;print;$V;1| (|x| $) (|mathprint| |x|)) 

(DEFUN |OUTFORM;message;S$;2| (|s| $)
  (COND
    ((SPADCALL |s| (QREFELT $ 11)) (SPADCALL (QREFELT $ 12)))
    ('T |s|))) 

(DEFUN |OUTFORM;messagePrint;SV;3| (|s| $)
  (SPADCALL (SPADCALL |s| (QREFELT $ 13)) (QREFELT $ 8))) 

(PUT '|OUTFORM;=;2$B;4| '|SPADreplace| 'EQUAL) 

(DEFUN |OUTFORM;=;2$B;4| (|a| |b| $) (EQUAL |a| |b|)) 

(DEFUN |OUTFORM;=;3$;5| (|a| |b| $)
  (LIST (|OUTFORM;sform| "=" $) |a| |b|)) 

(PUT '|OUTFORM;coerce;$Of;6| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DEFUN |OUTFORM;coerce;$Of;6| (|a| $) |a|) 

(PUT '|OUTFORM;outputForm;I$;7| '|SPADreplace| '(XLAM (|n|) |n|)) 

(DEFUN |OUTFORM;outputForm;I$;7| (|n| $) |n|) 

(PUT '|OUTFORM;outputForm;S$;8| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DEFUN |OUTFORM;outputForm;S$;8| (|e| $) |e|) 

(PUT '|OUTFORM;outputForm;Df$;9| '|SPADreplace| '(XLAM (|f|) |f|)) 

(DEFUN |OUTFORM;outputForm;Df$;9| (|f| $) |f|) 

(PUT '|OUTFORM;sform| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DEFUN |OUTFORM;sform| (|s| $) |s|) 

(PUT '|OUTFORM;eform| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DEFUN |OUTFORM;eform| (|e| $) |e|) 

(PUT '|OUTFORM;iform| '|SPADreplace| '(XLAM (|n|) |n|)) 

(DEFUN |OUTFORM;iform| (|n| $) |n|) 

(DEFUN |OUTFORM;outputForm;S$;13| (|s| $)
  (|OUTFORM;sform|
      (SPADCALL (SPADCALL (QREFELT $ 26))
          (SPADCALL |s| (SPADCALL (QREFELT $ 26)) (QREFELT $ 27))
          (QREFELT $ 28))
      $)) 

(PUT '|OUTFORM;width;$I;14| '|SPADreplace| '|outformWidth|) 

(DEFUN |OUTFORM;width;$I;14| (|a| $) (|outformWidth| |a|)) 

(PUT '|OUTFORM;height;$I;15| '|SPADreplace| '|height|) 

(DEFUN |OUTFORM;height;$I;15| (|a| $) (|height| |a|)) 

(PUT '|OUTFORM;subHeight;$I;16| '|SPADreplace| '|subspan|) 

(DEFUN |OUTFORM;subHeight;$I;16| (|a| $) (|subspan| |a|)) 

(PUT '|OUTFORM;superHeight;$I;17| '|SPADreplace| '|superspan|) 

(DEFUN |OUTFORM;superHeight;$I;17| (|a| $) (|superspan| |a|)) 

(PUT '|OUTFORM;height;I;18| '|SPADreplace| '(XLAM NIL 20)) 

(DEFUN |OUTFORM;height;I;18| ($) 20) 

(PUT '|OUTFORM;width;I;19| '|SPADreplace| '(XLAM NIL 66)) 

(DEFUN |OUTFORM;width;I;19| ($) 66) 

(DEFUN |OUTFORM;center;$I$;20| (|a| |w| $)
  (SPADCALL
      (SPADCALL (QUOTIENT2 (- |w| (SPADCALL |a| (QREFELT $ 30))) 2)
          (QREFELT $ 36))
      |a| (QREFELT $ 37))) 

(DEFUN |OUTFORM;left;$I$;21| (|a| |w| $)
  (SPADCALL |a|
      (SPADCALL (- |w| (SPADCALL |a| (QREFELT $ 30))) (QREFELT $ 36))
      (QREFELT $ 37))) 

(DEFUN |OUTFORM;right;$I$;22| (|a| |w| $)
  (SPADCALL
      (SPADCALL (- |w| (SPADCALL |a| (QREFELT $ 30))) (QREFELT $ 36))
      |a| (QREFELT $ 37))) 

(DEFUN |OUTFORM;center;2$;23| (|a| $)
  (SPADCALL |a| (SPADCALL (QREFELT $ 35)) (QREFELT $ 38))) 

(DEFUN |OUTFORM;left;2$;24| (|a| $)
  (SPADCALL |a| (SPADCALL (QREFELT $ 35)) (QREFELT $ 39))) 

(DEFUN |OUTFORM;right;2$;25| (|a| $)
  (SPADCALL |a| (SPADCALL (QREFELT $ 35)) (QREFELT $ 40))) 

(DEFUN |OUTFORM;vspace;I$;26| (|n| $)
  (COND
    ((EQL |n| 0) (SPADCALL (QREFELT $ 12)))
    ('T
     (SPADCALL (|OUTFORM;sform| " " $)
         (SPADCALL (- |n| 1) (QREFELT $ 44)) (QREFELT $ 45))))) 

(DEFUN |OUTFORM;hspace;I$;27| (|n| $)
  (COND
    ((EQL |n| 0) (SPADCALL (QREFELT $ 12)))
    ('T (|OUTFORM;sform| (|fillerSpaces| |n|) $)))) 

(DEFUN |OUTFORM;rspace;2I$;28| (|n| |m| $)
  (COND
    ((OR (EQL |n| 0) (EQL |m| 0)) (SPADCALL (QREFELT $ 12)))
    ('T
     (SPADCALL (SPADCALL |n| (QREFELT $ 36))
         (SPADCALL |n| (- |m| 1) (QREFELT $ 46)) (QREFELT $ 45))))) 

(DEFUN |OUTFORM;matrix;L$;29| (|ll| $)
  (PROG (#0=#:G1437 |l| #1=#:G1438 |lv|)
    (RETURN
      (SEQ (LETT |lv|
                 (PROGN
                   (LETT #0# NIL |OUTFORM;matrix;L$;29|)
                   (SEQ (LETT |l| NIL |OUTFORM;matrix;L$;29|)
                        (LETT #1# |ll| |OUTFORM;matrix;L$;29|) G190
                        (COND
                          ((OR (ATOM #1#)
                               (PROGN
                                 (LETT |l| (CAR #1#)
                                       |OUTFORM;matrix;L$;29|)
                                 NIL))
                           (GO G191)))
                        (SEQ (EXIT (LETT #0# (CONS (LIST2VEC |l|) #0#)
                                    |OUTFORM;matrix;L$;29|)))
                        (LETT #1# (CDR #1#) |OUTFORM;matrix;L$;29|)
                        (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                 |OUTFORM;matrix;L$;29|)
           (EXIT (CONS (|OUTFORM;eform| 'MATRIX $) (LIST2VEC |lv|))))))) 

(DEFUN |OUTFORM;pile;L$;30| (|l| $)
  (CONS (|OUTFORM;eform| 'SC $) |l|)) 

(DEFUN |OUTFORM;commaSeparate;L$;31| (|l| $)
  (CONS (|OUTFORM;eform| 'AGGLST $) |l|)) 

(DEFUN |OUTFORM;semicolonSeparate;L$;32| (|l| $)
  (CONS (|OUTFORM;eform| 'AGGSET $) |l|)) 

(DEFUN |OUTFORM;blankSeparate;L$;33| (|l| $)
  (PROG (|c| |u| #0=#:G1446 |l1|)
    (RETURN
      (SEQ (LETT |c| (|OUTFORM;eform| 'CONCATB $)
                 |OUTFORM;blankSeparate;L$;33|)
           (LETT |l1| NIL |OUTFORM;blankSeparate;L$;33|)
           (SEQ (LETT |u| NIL |OUTFORM;blankSeparate;L$;33|)
                (LETT #0# (SPADCALL |l| (QREFELT $ 53))
                      |OUTFORM;blankSeparate;L$;33|)
                G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |u| (CAR #0#)
                               |OUTFORM;blankSeparate;L$;33|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (COND
                             ((EQCAR |u| |c|)
                              (LETT |l1|
                                    (SPADCALL (CDR |u|) |l1|
                                     (QREFELT $ 54))
                                    |OUTFORM;blankSeparate;L$;33|))
                             ('T
                              (LETT |l1| (CONS |u| |l1|)
                                    |OUTFORM;blankSeparate;L$;33|)))))
                (LETT #0# (CDR #0#) |OUTFORM;blankSeparate;L$;33|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (CONS |c| |l1|)))))) 

(DEFUN |OUTFORM;brace;2$;34| (|a| $)
  (LIST (|OUTFORM;eform| 'BRACE $) |a|)) 

(DEFUN |OUTFORM;brace;L$;35| (|l| $)
  (SPADCALL (SPADCALL |l| (QREFELT $ 51)) (QREFELT $ 56))) 

(DEFUN |OUTFORM;bracket;2$;36| (|a| $)
  (LIST (|OUTFORM;eform| 'BRACKET $) |a|)) 

(DEFUN |OUTFORM;bracket;L$;37| (|l| $)
  (SPADCALL (SPADCALL |l| (QREFELT $ 51)) (QREFELT $ 58))) 

(DEFUN |OUTFORM;paren;2$;38| (|a| $)
  (LIST (|OUTFORM;eform| 'PAREN $) |a|)) 

(DEFUN |OUTFORM;paren;L$;39| (|l| $)
  (SPADCALL (SPADCALL |l| (QREFELT $ 51)) (QREFELT $ 60))) 

(DEFUN |OUTFORM;sub;3$;40| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SUB $) |a| |b|)) 

(DEFUN |OUTFORM;super;3$;41| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $) |b|)) 

(DEFUN |OUTFORM;presub;3$;42| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
        (|OUTFORM;sform| " " $) (|OUTFORM;sform| " " $) |b|)) 

(DEFUN |OUTFORM;presuper;3$;43| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
        (|OUTFORM;sform| " " $) |b|)) 

(DEFUN |OUTFORM;scripts;$L$;44| (|a| |l| $)
  (COND
    ((SPADCALL |l| (QREFELT $ 66)) |a|)
    ((SPADCALL (SPADCALL |l| (QREFELT $ 67)) (QREFELT $ 66))
     (SPADCALL |a| (SPADCALL |l| (QREFELT $ 68)) (QREFELT $ 62)))
    ('T (CONS (|OUTFORM;eform| 'SUPERSUB $) (CONS |a| |l|))))) 

(DEFUN |OUTFORM;supersub;$L$;45| (|a| |l| $)
  (SEQ (COND
         ((ODDP (SPADCALL |l| (QREFELT $ 71)))
          (LETT |l|
                (SPADCALL |l| (LIST (SPADCALL (QREFELT $ 12)))
                    (QREFELT $ 73))
                |OUTFORM;supersub;$L$;45|)))
       (EXIT (CONS (|OUTFORM;eform| 'ALTSUPERSUB $) (CONS |a| |l|))))) 

(DEFUN |OUTFORM;hconcat;3$;46| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'CONCAT $) |a| |b|)) 

(DEFUN |OUTFORM;hconcat;L$;47| (|l| $)
  (CONS (|OUTFORM;eform| 'CONCAT $) |l|)) 

(DEFUN |OUTFORM;vconcat;3$;48| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'VCONCAT $) |a| |b|)) 

(DEFUN |OUTFORM;vconcat;L$;49| (|l| $)
  (CONS (|OUTFORM;eform| 'VCONCAT $) |l|)) 

(DEFUN |OUTFORM;~=;3$;50| (|a| |b| $)
  (LIST (|OUTFORM;sform| "~=" $) |a| |b|)) 

(DEFUN |OUTFORM;<;3$;51| (|a| |b| $)
  (LIST (|OUTFORM;sform| "<" $) |a| |b|)) 

(DEFUN |OUTFORM;>;3$;52| (|a| |b| $)
  (LIST (|OUTFORM;sform| ">" $) |a| |b|)) 

(DEFUN |OUTFORM;<=;3$;53| (|a| |b| $)
  (LIST (|OUTFORM;sform| "<=" $) |a| |b|)) 

(DEFUN |OUTFORM;>=;3$;54| (|a| |b| $)
  (LIST (|OUTFORM;sform| ">=" $) |a| |b|)) 

(DEFUN |OUTFORM;+;3$;55| (|a| |b| $)
  (LIST (|OUTFORM;sform| "+" $) |a| |b|)) 

(DEFUN |OUTFORM;-;3$;56| (|a| |b| $)
  (LIST (|OUTFORM;sform| "-" $) |a| |b|)) 

(DEFUN |OUTFORM;-;2$;57| (|a| $) (LIST (|OUTFORM;sform| "-" $) |a|)) 

(DEFUN |OUTFORM;*;3$;58| (|a| |b| $)
  (LIST (|OUTFORM;sform| "*" $) |a| |b|)) 

(DEFUN |OUTFORM;/;3$;59| (|a| |b| $)
  (LIST (|OUTFORM;sform| "/" $) |a| |b|)) 

(DEFUN |OUTFORM;**;3$;60| (|a| |b| $)
  (LIST (|OUTFORM;sform| "**" $) |a| |b|)) 

(DEFUN |OUTFORM;div;3$;61| (|a| |b| $)
  (LIST (|OUTFORM;sform| "div" $) |a| |b|)) 

(DEFUN |OUTFORM;rem;3$;62| (|a| |b| $)
  (LIST (|OUTFORM;sform| "rem" $) |a| |b|)) 

(DEFUN |OUTFORM;quo;3$;63| (|a| |b| $)
  (LIST (|OUTFORM;sform| "quo" $) |a| |b|)) 

(DEFUN |OUTFORM;exquo;3$;64| (|a| |b| $)
  (LIST (|OUTFORM;sform| "exquo" $) |a| |b|)) 

(DEFUN |OUTFORM;and;3$;65| (|a| |b| $)
  (LIST (|OUTFORM;sform| "and" $) |a| |b|)) 

(DEFUN |OUTFORM;or;3$;66| (|a| |b| $)
  (LIST (|OUTFORM;sform| "or" $) |a| |b|)) 

(DEFUN |OUTFORM;not;2$;67| (|a| $)
  (LIST (|OUTFORM;sform| "not" $) |a|)) 

(DEFUN |OUTFORM;SEGMENT;3$;68| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SEGMENT $) |a| |b|)) 

(DEFUN |OUTFORM;SEGMENT;2$;69| (|a| $)
  (LIST (|OUTFORM;eform| 'SEGMENT $) |a|)) 

(DEFUN |OUTFORM;binomial;3$;70| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'BINOMIAL $) |a| |b|)) 

(DEFUN |OUTFORM;empty;$;71| ($) (LIST (|OUTFORM;eform| 'NOTHING $))) 

(DEFUN |OUTFORM;infix?;$B;72| (|a| $)
  (PROG (#0=#:G1491 |e|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |e|
                            (COND
                              ((IDENTP |a|) |a|)
                              ((STRINGP |a|) (INTERN |a|))
                              ('T
                               (PROGN
                                 (LETT #0# 'NIL |OUTFORM;infix?;$B;72|)
                                 (GO #0#))))
                            |OUTFORM;infix?;$B;72|)
                      (EXIT (COND ((GET |e| 'INFIXOP) 'T) ('T 'NIL)))))
           #0# (EXIT #0#))))) 

(PUT '|OUTFORM;elt;$L$;73| '|SPADreplace| 'CONS) 

(DEFUN |OUTFORM;elt;$L$;73| (|a| |l| $) (CONS |a| |l|)) 

(DEFUN |OUTFORM;prefix;$L$;74| (|a| |l| $)
  (COND
    ((NULL (SPADCALL |a| (QREFELT $ 98))) (CONS |a| |l|))
    ('T
     (SPADCALL |a|
         (SPADCALL (SPADCALL |l| (QREFELT $ 51)) (QREFELT $ 60))
         (QREFELT $ 37))))) 

(DEFUN |OUTFORM;infix;$L$;75| (|a| |l| $)
  (COND
    ((SPADCALL |l| (QREFELT $ 66)) (SPADCALL (QREFELT $ 12)))
    ((SPADCALL (SPADCALL |l| (QREFELT $ 67)) (QREFELT $ 66))
     (SPADCALL |l| (QREFELT $ 68)))
    ((SPADCALL |a| (QREFELT $ 98)) (CONS |a| |l|))
    ('T
     (SPADCALL
         (LIST (SPADCALL |l| (QREFELT $ 68)) |a|
               (SPADCALL |a| (SPADCALL |l| (QREFELT $ 101))
                   (QREFELT $ 102)))
         (QREFELT $ 75))))) 

(DEFUN |OUTFORM;infix;4$;76| (|a| |b| |c| $)
  (COND
    ((SPADCALL |a| (QREFELT $ 98)) (LIST |a| |b| |c|))
    ('T (SPADCALL (LIST |b| |a| |c|) (QREFELT $ 75))))) 

(DEFUN |OUTFORM;postfix;3$;77| (|a| |b| $)
  (SPADCALL |b| |a| (QREFELT $ 37))) 

(DEFUN |OUTFORM;string;2$;78| (|a| $)
  (LIST (|OUTFORM;eform| 'STRING $) |a|)) 

(DEFUN |OUTFORM;quote;2$;79| (|a| $)
  (LIST (|OUTFORM;eform| 'QUOTE $) |a|)) 

(DEFUN |OUTFORM;overbar;2$;80| (|a| $)
  (LIST (|OUTFORM;eform| 'OVERBAR $) |a|)) 

(DEFUN |OUTFORM;dot;2$;81| (|a| $)
  (SPADCALL |a| (|OUTFORM;sform| "." $) (QREFELT $ 63))) 

(DEFUN |OUTFORM;prime;2$;82| (|a| $)
  (SPADCALL |a| (|OUTFORM;sform| "," $) (QREFELT $ 63))) 

(DEFUN |OUTFORM;dot;$Nni$;83| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn| (SPADCALL "." (QREFELT $ 110)))
                 |OUTFORM;dot;$Nni$;83|)
           (EXIT (SPADCALL |a| (|OUTFORM;sform| |s| $) (QREFELT $ 63))))))) 

(DEFUN |OUTFORM;prime;$Nni$;84| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn| (SPADCALL "," (QREFELT $ 110)))
                 |OUTFORM;prime;$Nni$;84|)
           (EXIT (SPADCALL |a| (|OUTFORM;sform| |s| $) (QREFELT $ 63))))))) 

(DEFUN |OUTFORM;overlabel;3$;85| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'OVERLABEL $) |a| |b|)) 

(DEFUN |OUTFORM;box;2$;86| (|a| $)
  (LIST (|OUTFORM;eform| 'BOX $) |a|)) 

(DEFUN |OUTFORM;zag;3$;87| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'ZAG $) |a| |b|)) 

(DEFUN |OUTFORM;root;2$;88| (|a| $)
  (LIST (|OUTFORM;eform| 'ROOT $) |a|)) 

(DEFUN |OUTFORM;root;3$;89| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'ROOT $) |a| |b|)) 

(DEFUN |OUTFORM;over;3$;90| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'OVER $) |a| |b|)) 

(DEFUN |OUTFORM;slash;3$;91| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SLASH $) |a| |b|)) 

(DEFUN |OUTFORM;assign;3$;92| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'LET $) |a| |b|)) 

(DEFUN |OUTFORM;label;3$;93| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'EQUATNUM $) |a| |b|)) 

(DEFUN |OUTFORM;rarrow;3$;94| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'TAG $) |a| |b|)) 

(DEFUN |OUTFORM;differentiate;$Nni$;95| (|a| |nn| $)
  (PROG (#0=#:G1521 |r| |s|)
    (RETURN
      (SEQ (COND
             ((ZEROP |nn|) |a|)
             ((< |nn| 4) (SPADCALL |a| |nn| (QREFELT $ 112)))
             ('T
              (SEQ (LETT |r|
                         (SPADCALL
                             (PROG1 (LETT #0# |nn|
                                     |OUTFORM;differentiate;$Nni$;95|)
                               (|check-subtype| (> #0# 0)
                                   '(|PositiveInteger|) #0#))
                             (QREFELT $ 125))
                         |OUTFORM;differentiate;$Nni$;95|)
                   (LETT |s| (SPADCALL |r| (QREFELT $ 126))
                         |OUTFORM;differentiate;$Nni$;95|)
                   (EXIT (SPADCALL |a|
                             (SPADCALL (|OUTFORM;sform| |s| $)
                                 (QREFELT $ 60))
                             (QREFELT $ 63)))))))))) 

(DEFUN |OUTFORM;sum;2$;96| (|a| $)
  (LIST (|OUTFORM;eform| 'SIGMA $) (SPADCALL (QREFELT $ 12)) |a|)) 

(DEFUN |OUTFORM;sum;3$;97| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'SIGMA $) |b| |a|)) 

(DEFUN |OUTFORM;sum;4$;98| (|a| |b| |c| $)
  (LIST (|OUTFORM;eform| 'SIGMA2 $) |b| |c| |a|)) 

(DEFUN |OUTFORM;prod;2$;99| (|a| $)
  (LIST (|OUTFORM;eform| 'PI $) (SPADCALL (QREFELT $ 12)) |a|)) 

(DEFUN |OUTFORM;prod;3$;100| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'PI $) |b| |a|)) 

(DEFUN |OUTFORM;prod;4$;101| (|a| |b| |c| $)
  (LIST (|OUTFORM;eform| 'PI2 $) |b| |c| |a|)) 

(DEFUN |OUTFORM;int;2$;102| (|a| $)
  (LIST (|OUTFORM;eform| 'INTSIGN $) (SPADCALL (QREFELT $ 12))
        (SPADCALL (QREFELT $ 12)) |a|)) 

(DEFUN |OUTFORM;int;3$;103| (|a| |b| $)
  (LIST (|OUTFORM;eform| 'INTSIGN $) |b| (SPADCALL (QREFELT $ 12)) |a|)) 

(DEFUN |OUTFORM;int;4$;104| (|a| |b| |c| $)
  (LIST (|OUTFORM;eform| 'INTSIGN $) |b| |c| |a|)) 

(DEFUN |OutputForm| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1535)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|OutputForm|)
                   |OutputForm|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|OutputForm|
                                   (LIST
                                    (CONS NIL (CONS 1 (|OutputForm;|))))))
                 (LETT #0# T |OutputForm|))
               (COND
                 ((NOT #0#) (HREM |$ConstructorCache| '|OutputForm|))))))))))) 

(DEFUN |OutputForm;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|OutputForm|) . #0=(|OutputForm|))
        (LETT $ (|newShell| 138) . #0#)
        (QSETREFV $ 0 |dv$|)
        (QSETREFV $ 3 (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|OutputForm| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (QSETREFV $ 6 (|List| $))
        $)))) 

(MAKEPROP '|OutputForm| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL '|Rep| (|Void|)
             |OUTFORM;print;$V;1| (|Boolean|) (|String|) (0 . |empty?|)
             |OUTFORM;empty;$;71| |OUTFORM;message;S$;2|
             |OUTFORM;messagePrint;SV;3| |OUTFORM;=;2$B;4|
             |OUTFORM;=;3$;5| (|OutputForm|) |OUTFORM;coerce;$Of;6|
             (|Integer|) |OUTFORM;outputForm;I$;7| (|Symbol|)
             |OUTFORM;outputForm;S$;8| (|DoubleFloat|)
             |OUTFORM;outputForm;Df$;9| (|Character|) (5 . |quote|)
             (9 . |concat|) (15 . |concat|) |OUTFORM;outputForm;S$;13|
             |OUTFORM;width;$I;14| |OUTFORM;height;$I;15|
             |OUTFORM;subHeight;$I;16| |OUTFORM;superHeight;$I;17|
             |OUTFORM;height;I;18| |OUTFORM;width;I;19|
             |OUTFORM;hspace;I$;27| |OUTFORM;hconcat;3$;46|
             |OUTFORM;center;$I$;20| |OUTFORM;left;$I$;21|
             |OUTFORM;right;$I$;22| |OUTFORM;center;2$;23|
             |OUTFORM;left;2$;24| |OUTFORM;right;2$;25|
             |OUTFORM;vspace;I$;26| |OUTFORM;vconcat;3$;48|
             |OUTFORM;rspace;2I$;28| (|List| 49) |OUTFORM;matrix;L$;29|
             (|List| $) |OUTFORM;pile;L$;30|
             |OUTFORM;commaSeparate;L$;31|
             |OUTFORM;semicolonSeparate;L$;32| (21 . |reverse|)
             (26 . |append|) |OUTFORM;blankSeparate;L$;33|
             |OUTFORM;brace;2$;34| |OUTFORM;brace;L$;35|
             |OUTFORM;bracket;2$;36| |OUTFORM;bracket;L$;37|
             |OUTFORM;paren;2$;38| |OUTFORM;paren;L$;39|
             |OUTFORM;sub;3$;40| |OUTFORM;super;3$;41|
             |OUTFORM;presub;3$;42| |OUTFORM;presuper;3$;43|
             (32 . |null|) (37 . |rest|) (42 . |first|)
             |OUTFORM;scripts;$L$;44| (|NonNegativeInteger|) (47 . |#|)
             (|List| $$) (52 . |append|) |OUTFORM;supersub;$L$;45|
             |OUTFORM;hconcat;L$;47| |OUTFORM;vconcat;L$;49|
             |OUTFORM;~=;3$;50| |OUTFORM;<;3$;51| |OUTFORM;>;3$;52|
             |OUTFORM;<=;3$;53| |OUTFORM;>=;3$;54| |OUTFORM;+;3$;55|
             |OUTFORM;-;3$;56| |OUTFORM;-;2$;57| |OUTFORM;*;3$;58|
             |OUTFORM;/;3$;59| |OUTFORM;**;3$;60| |OUTFORM;div;3$;61|
             |OUTFORM;rem;3$;62| |OUTFORM;quo;3$;63|
             |OUTFORM;exquo;3$;64| |OUTFORM;and;3$;65|
             |OUTFORM;or;3$;66| |OUTFORM;not;2$;67|
             |OUTFORM;SEGMENT;3$;68| |OUTFORM;SEGMENT;2$;69|
             |OUTFORM;binomial;3$;70| |OUTFORM;infix?;$B;72|
             |OUTFORM;elt;$L$;73| |OUTFORM;prefix;$L$;74| (58 . |rest|)
             |OUTFORM;infix;$L$;75| |OUTFORM;infix;4$;76|
             |OUTFORM;postfix;3$;77| |OUTFORM;string;2$;78|
             |OUTFORM;quote;2$;79| |OUTFORM;overbar;2$;80|
             |OUTFORM;dot;2$;81| |OUTFORM;prime;2$;82| (63 . |char|)
             |OUTFORM;dot;$Nni$;83| |OUTFORM;prime;$Nni$;84|
             |OUTFORM;overlabel;3$;85| |OUTFORM;box;2$;86|
             |OUTFORM;zag;3$;87| |OUTFORM;root;2$;88|
             |OUTFORM;root;3$;89| |OUTFORM;over;3$;90|
             |OUTFORM;slash;3$;91| |OUTFORM;assign;3$;92|
             |OUTFORM;label;3$;93| |OUTFORM;rarrow;3$;94|
             (|PositiveInteger|) (|NumberFormats|) (68 . |FormatRoman|)
             (73 . |lowerCase|) |OUTFORM;differentiate;$Nni$;95|
             |OUTFORM;sum;2$;96| |OUTFORM;sum;3$;97|
             |OUTFORM;sum;4$;98| |OUTFORM;prod;2$;99|
             |OUTFORM;prod;3$;100| |OUTFORM;prod;4$;101|
             |OUTFORM;int;2$;102| |OUTFORM;int;3$;103|
             |OUTFORM;int;4$;104| (|SingleInteger|))
          '#(~= 78 |zag| 90 |width| 96 |vspace| 105 |vconcat| 110
             |supersub| 121 |superHeight| 127 |super| 132 |sum| 138
             |subHeight| 156 |sub| 161 |string| 167 |slash| 172
             |semicolonSeparate| 178 |scripts| 183 |rspace| 189 |root|
             195 |right| 206 |rem| 217 |rarrow| 223 |quote| 229 |quo|
             234 |prod| 240 |print| 258 |prime| 263 |presuper| 274
             |presub| 280 |prefix| 286 |postfix| 292 |pile| 298 |paren|
             303 |overlabel| 313 |overbar| 319 |over| 324 |outputForm|
             330 |or| 350 |not| 356 |messagePrint| 361 |message| 366
             |matrix| 371 |left| 376 |latex| 387 |label| 392 |int| 398
             |infix?| 416 |infix| 421 |hspace| 434 |height| 439
             |hconcat| 448 |hash| 459 |exquo| 464 |empty| 470 |elt| 474
             |dot| 480 |div| 491 |differentiate| 497 |commaSeparate|
             503 |coerce| 508 |center| 513 |bracket| 524 |brace| 534
             |box| 544 |blankSeparate| 549 |binomial| 554 |assign| 560
             |and| 566 SEGMENT 572 >= 583 > 589 = 595 <= 607 < 613 /
             619 - 625 + 636 ** 642 * 648)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0))
                (CONS '#(|SetCategory&| |BasicType&| NIL)
                      (CONS '#((|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 17))
                            (|makeByteWordVec2| 137
                                '(1 10 9 0 11 0 25 0 26 2 10 0 0 25 27
                                  2 10 0 25 0 28 1 6 0 0 53 2 6 0 0 0
                                  54 1 6 9 0 66 1 6 0 0 67 1 6 2 0 68 1
                                  6 70 0 71 2 72 0 0 0 73 1 72 0 0 101
                                  1 25 0 10 110 1 124 10 123 125 1 10 0
                                  0 126 2 0 0 0 0 77 2 0 9 0 0 1 2 0 0
                                  0 0 115 0 0 19 35 1 0 19 0 30 1 0 0
                                  19 44 1 0 0 49 76 2 0 0 0 0 45 2 0 0
                                  0 49 74 1 0 19 0 33 2 0 0 0 0 63 2 0
                                  0 0 0 129 3 0 0 0 0 0 130 1 0 0 0 128
                                  1 0 19 0 32 2 0 0 0 0 62 1 0 0 0 105
                                  2 0 0 0 0 119 1 0 0 49 52 2 0 0 0 49
                                  69 2 0 0 19 19 46 1 0 0 0 116 2 0 0 0
                                  0 117 1 0 0 0 43 2 0 0 0 19 40 2 0 0
                                  0 0 89 2 0 0 0 0 122 1 0 0 0 106 2 0
                                  0 0 0 90 3 0 0 0 0 0 133 1 0 0 0 131
                                  2 0 0 0 0 132 1 0 7 0 8 2 0 0 0 70
                                  112 1 0 0 0 109 2 0 0 0 0 65 2 0 0 0
                                  0 64 2 0 0 0 49 100 2 0 0 0 0 104 1 0
                                  0 49 50 1 0 0 49 61 1 0 0 0 60 2 0 0
                                  0 0 113 1 0 0 0 107 2 0 0 0 0 118 1 0
                                  0 10 29 1 0 0 23 24 1 0 0 21 22 1 0 0
                                  19 20 2 0 0 0 0 93 1 0 0 0 94 1 0 7
                                  10 14 1 0 0 10 13 1 0 0 47 48 1 0 0 0
                                  42 2 0 0 0 19 39 1 0 10 0 1 2 0 0 0 0
                                  121 3 0 0 0 0 0 136 2 0 0 0 0 135 1 0
                                  0 0 134 1 0 9 0 98 2 0 0 0 49 102 3 0
                                  0 0 0 0 103 1 0 0 19 36 0 0 19 34 1 0
                                  19 0 31 1 0 0 49 75 2 0 0 0 0 37 1 0
                                  137 0 1 2 0 0 0 0 91 0 0 0 12 2 0 0 0
                                  49 99 2 0 0 0 70 111 1 0 0 0 108 2 0
                                  0 0 0 88 2 0 0 0 70 127 1 0 0 49 51 1
                                  0 17 0 18 1 0 0 0 41 2 0 0 0 19 38 1
                                  0 0 0 58 1 0 0 49 59 1 0 0 49 57 1 0
                                  0 0 56 1 0 0 0 114 1 0 0 49 55 2 0 0
                                  0 0 97 2 0 0 0 0 120 2 0 0 0 0 92 1 0
                                  0 0 96 2 0 0 0 0 95 2 0 0 0 0 81 2 0
                                  0 0 0 79 2 0 0 0 0 16 2 0 9 0 0 15 2
                                  0 0 0 0 80 2 0 0 0 0 78 2 0 0 0 0 86
                                  1 0 0 0 84 2 0 0 0 0 83 2 0 0 0 0 82
                                  2 0 0 0 0 87 2 0 0 0 0 85)))))
          '|lookupComplete|)) 

(MAKEPROP '|OutputForm| 'NILADIC T) 
