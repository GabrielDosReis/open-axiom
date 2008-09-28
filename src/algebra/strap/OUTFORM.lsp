
(/VERSIONCHECK 2) 

(PUT '|OUTFORM;sform| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DEFUN |OUTFORM;sform| (|s| $) |s|) 

(PUT '|OUTFORM;eform| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DEFUN |OUTFORM;eform| (|e| $) |e|) 

(PUT '|OUTFORM;iform| '|SPADreplace| '(XLAM (|i|) |i|)) 

(DEFUN |OUTFORM;iform| (|i| $) |i|) 

(PUT '|OUTFORM;bless| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |OUTFORM;bless| (|x| $) |x|) 

(PUT '|OUTFORM;print;$V;5| '|SPADreplace| '|mathprint|) 

(DEFUN |OUTFORM;print;$V;5| (|x| $) (|mathprint| |x|)) 

(DEFUN |OUTFORM;message;S$;6| (|s| $)
  (COND
    ((SPADCALL |s| (|getShellEntry| $ 10))
     (SPADCALL (|getShellEntry| $ 11)))
    ('T |s|))) 

(DEFUN |OUTFORM;messagePrint;SV;7| (|s| $)
  (SPADCALL (SPADCALL |s| (|getShellEntry| $ 12))
            (|getShellEntry| $ 7))) 

(PUT '|OUTFORM;=;2$B;8| '|SPADreplace| 'EQUAL) 

(DEFUN |OUTFORM;=;2$B;8| (|a| |b| $) (EQUAL |a| |b|)) 

(DEFUN |OUTFORM;=;3$;9| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "=" $) |a| |b|) $)) 

(PUT '|OUTFORM;coerce;2$;10| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DEFUN |OUTFORM;coerce;2$;10| (|a| $) |a|) 

(PUT '|OUTFORM;outputForm;I$;11| '|SPADreplace| '(XLAM (|n|) |n|)) 

(DEFUN |OUTFORM;outputForm;I$;11| (|n| $) |n|) 

(PUT '|OUTFORM;outputForm;S$;12| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DEFUN |OUTFORM;outputForm;S$;12| (|e| $) |e|) 

(PUT '|OUTFORM;outputForm;Df$;13| '|SPADreplace| '(XLAM (|f|) |f|)) 

(DEFUN |OUTFORM;outputForm;Df$;13| (|f| $) |f|) 

(DEFUN |OUTFORM;outputForm;S$;14| (|s| $)
  (|OUTFORM;sform|
      (SPADCALL (SPADCALL (|getShellEntry| $ 25))
          (SPADCALL |s| (SPADCALL (|getShellEntry| $ 25))
              (|getShellEntry| $ 26))
          (|getShellEntry| $ 27))
      $)) 

(PUT '|OUTFORM;width;$I;15| '|SPADreplace| '|outformWidth|) 

(DEFUN |OUTFORM;width;$I;15| (|a| $) (|outformWidth| |a|)) 

(PUT '|OUTFORM;height;$I;16| '|SPADreplace| '|height|) 

(DEFUN |OUTFORM;height;$I;16| (|a| $) (|height| |a|)) 

(PUT '|OUTFORM;subHeight;$I;17| '|SPADreplace| '|subspan|) 

(DEFUN |OUTFORM;subHeight;$I;17| (|a| $) (|subspan| |a|)) 

(PUT '|OUTFORM;superHeight;$I;18| '|SPADreplace| '|superspan|) 

(DEFUN |OUTFORM;superHeight;$I;18| (|a| $) (|superspan| |a|)) 

(PUT '|OUTFORM;height;I;19| '|SPADreplace| '(XLAM NIL 20)) 

(DEFUN |OUTFORM;height;I;19| ($) 20) 

(PUT '|OUTFORM;width;I;20| '|SPADreplace| '(XLAM NIL 66)) 

(DEFUN |OUTFORM;width;I;20| ($) 66) 

(DEFUN |OUTFORM;center;$I$;21| (|a| |w| $)
  (SPADCALL
      (SPADCALL
          (QUOTIENT2 (- |w| (SPADCALL |a| (|getShellEntry| $ 29))) 2)
          (|getShellEntry| $ 35))
      |a| (|getShellEntry| $ 36))) 

(DEFUN |OUTFORM;left;$I$;22| (|a| |w| $)
  (SPADCALL |a|
            (SPADCALL (- |w| (SPADCALL |a| (|getShellEntry| $ 29)))
                (|getShellEntry| $ 35))
            (|getShellEntry| $ 36))) 

(DEFUN |OUTFORM;right;$I$;23| (|a| |w| $)
  (SPADCALL
      (SPADCALL (- |w| (SPADCALL |a| (|getShellEntry| $ 29)))
          (|getShellEntry| $ 35))
      |a| (|getShellEntry| $ 36))) 

(DEFUN |OUTFORM;center;2$;24| (|a| $)
  (SPADCALL |a| (SPADCALL (|getShellEntry| $ 34))
      (|getShellEntry| $ 37))) 

(DEFUN |OUTFORM;left;2$;25| (|a| $)
  (SPADCALL |a| (SPADCALL (|getShellEntry| $ 34))
      (|getShellEntry| $ 38))) 

(DEFUN |OUTFORM;right;2$;26| (|a| $)
  (SPADCALL |a| (SPADCALL (|getShellEntry| $ 34))
      (|getShellEntry| $ 39))) 

(DEFUN |OUTFORM;vspace;I$;27| (|n| $)
  (COND
    ((EQL |n| 0) (SPADCALL (|getShellEntry| $ 11)))
    ('T
     (SPADCALL (|OUTFORM;sform| " " $)
         (SPADCALL (- |n| 1) (|getShellEntry| $ 43))
         (|getShellEntry| $ 44))))) 

(DEFUN |OUTFORM;hspace;I$;28| (|n| $)
  (COND
    ((EQL |n| 0) (SPADCALL (|getShellEntry| $ 11)))
    ('T (|OUTFORM;sform| (|fillerSpaces| |n|) $)))) 

(DEFUN |OUTFORM;rspace;2I$;29| (|n| |m| $)
  (COND
    ((OR (EQL |n| 0) (EQL |m| 0)) (SPADCALL (|getShellEntry| $ 11)))
    ('T
     (SPADCALL (SPADCALL |n| (|getShellEntry| $ 35))
         (SPADCALL |n| (- |m| 1) (|getShellEntry| $ 45))
         (|getShellEntry| $ 44))))) 

(DEFUN |OUTFORM;matrix;L$;30| (|ll| $)
  (PROG (#0=#:G1440 |l| #1=#:G1441 |lv|)
    (RETURN
      (SEQ (LETT |lv|
                 (|OUTFORM;bless|
                     (PROGN
                       (LETT #0# NIL |OUTFORM;matrix;L$;30|)
                       (SEQ (LETT |l| NIL |OUTFORM;matrix;L$;30|)
                            (LETT #1# |ll| |OUTFORM;matrix;L$;30|) G190
                            (COND
                              ((OR (ATOM #1#)
                                   (PROGN
                                     (LETT |l| (CAR #1#)
                                      |OUTFORM;matrix;L$;30|)
                                     NIL))
                               (GO G191)))
                            (SEQ (EXIT (LETT #0#
                                        (CONS (LIST2VEC |l|) #0#)
                                        |OUTFORM;matrix;L$;30|)))
                            (LETT #1# (CDR #1#) |OUTFORM;matrix;L$;30|)
                            (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                     $)
                 |OUTFORM;matrix;L$;30|)
           (EXIT (CONS (|OUTFORM;eform| 'MATRIX $) (LIST2VEC |lv|))))))) 

(DEFUN |OUTFORM;pile;L$;31| (|l| $)
  (CONS (|OUTFORM;eform| 'SC $) |l|)) 

(DEFUN |OUTFORM;commaSeparate;L$;32| (|l| $)
  (CONS (|OUTFORM;eform| 'AGGLST $) |l|)) 

(DEFUN |OUTFORM;semicolonSeparate;L$;33| (|l| $)
  (CONS (|OUTFORM;eform| 'AGGSET $) |l|)) 

(DEFUN |OUTFORM;blankSeparate;L$;34| (|l| $)
  (PROG (|c| |u| #0=#:G1449 |l1|)
    (RETURN
      (SEQ (LETT |c| (|OUTFORM;eform| 'CONCATB $)
                 |OUTFORM;blankSeparate;L$;34|)
           (LETT |l1| NIL |OUTFORM;blankSeparate;L$;34|)
           (SEQ (LETT |u| NIL |OUTFORM;blankSeparate;L$;34|)
                (LETT #0# (SPADCALL |l| (|getShellEntry| $ 53))
                      |OUTFORM;blankSeparate;L$;34|)
                G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |u| (CAR #0#)
                               |OUTFORM;blankSeparate;L$;34|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (COND
                             ((EQCAR |u| |c|)
                              (LETT |l1|
                                    (SPADCALL (CDR |u|) |l1|
                                     (|getShellEntry| $ 54))
                                    |OUTFORM;blankSeparate;L$;34|))
                             ('T
                              (LETT |l1| (CONS |u| |l1|)
                                    |OUTFORM;blankSeparate;L$;34|)))))
                (LETT #0# (CDR #0#) |OUTFORM;blankSeparate;L$;34|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (CONS |c| |l1|)))))) 

(DEFUN |OUTFORM;brace;2$;35| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BRACE $) |a|) $)) 

(DEFUN |OUTFORM;brace;L$;36| (|l| $)
  (SPADCALL (SPADCALL |l| (|getShellEntry| $ 50))
      (|getShellEntry| $ 56))) 

(DEFUN |OUTFORM;bracket;2$;37| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BRACKET $) |a|) $)) 

(DEFUN |OUTFORM;bracket;L$;38| (|l| $)
  (SPADCALL (SPADCALL |l| (|getShellEntry| $ 50))
      (|getShellEntry| $ 58))) 

(DEFUN |OUTFORM;paren;2$;39| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'PAREN $) |a|) $)) 

(DEFUN |OUTFORM;paren;L$;40| (|l| $)
  (SPADCALL (SPADCALL |l| (|getShellEntry| $ 50))
      (|getShellEntry| $ 60))) 

(DEFUN |OUTFORM;sub;3$;41| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SUB $) |a| |b|) $)) 

(DEFUN |OUTFORM;super;3$;42| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
            |b|)
      $)) 

(DEFUN |OUTFORM;presub;3$;43| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
            (|OUTFORM;sform| " " $) (|OUTFORM;sform| " " $) |b|)
      $)) 

(DEFUN |OUTFORM;presuper;3$;44| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
            (|OUTFORM;sform| " " $) |b|)
      $)) 

(DEFUN |OUTFORM;scripts;$L$;45| (|a| |l| $)
  (COND
    ((SPADCALL |l| (|getShellEntry| $ 66)) |a|)
    ((SPADCALL (SPADCALL |l| (|getShellEntry| $ 67))
         (|getShellEntry| $ 66))
     (SPADCALL |a| (SPADCALL |l| (|getShellEntry| $ 68))
         (|getShellEntry| $ 62)))
    ('T (CONS (|OUTFORM;eform| 'SUPERSUB $) (CONS |a| |l|))))) 

(DEFUN |OUTFORM;supersub;$L$;46| (|a| |l| $)
  (SEQ (COND
         ((ODDP (SPADCALL |l| (|getShellEntry| $ 71)))
          (LETT |l|
                (SPADCALL |l| (LIST (SPADCALL (|getShellEntry| $ 11)))
                    (|getShellEntry| $ 54))
                |OUTFORM;supersub;$L$;46|)))
       (EXIT (CONS (|OUTFORM;eform| 'ALTSUPERSUB $) (CONS |a| |l|))))) 

(DEFUN |OUTFORM;hconcat;3$;47| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'CONCAT $) |a| |b|) $)) 

(DEFUN |OUTFORM;hconcat;L$;48| (|l| $)
  (CONS (|OUTFORM;eform| 'CONCAT $) |l|)) 

(DEFUN |OUTFORM;vconcat;3$;49| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'VCONCAT $) |a| |b|) $)) 

(DEFUN |OUTFORM;vconcat;L$;50| (|l| $)
  (CONS (|OUTFORM;eform| 'VCONCAT $) |l|)) 

(DEFUN |OUTFORM;~=;3$;51| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "~=" $) |a| |b|) $)) 

(DEFUN |OUTFORM;<;3$;52| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "<" $) |a| |b|) $)) 

(DEFUN |OUTFORM;>;3$;53| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| ">" $) |a| |b|) $)) 

(DEFUN |OUTFORM;<=;3$;54| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "<=" $) |a| |b|) $)) 

(DEFUN |OUTFORM;>=;3$;55| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| ">=" $) |a| |b|) $)) 

(DEFUN |OUTFORM;+;3$;56| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "+" $) |a| |b|) $)) 

(DEFUN |OUTFORM;-;3$;57| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "-" $) |a| |b|) $)) 

(DEFUN |OUTFORM;-;2$;58| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "-" $) |a|) $)) 

(DEFUN |OUTFORM;*;3$;59| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "*" $) |a| |b|) $)) 

(DEFUN |OUTFORM;/;3$;60| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "/" $) |a| |b|) $)) 

(DEFUN |OUTFORM;**;3$;61| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "**" $) |a| |b|) $)) 

(DEFUN |OUTFORM;div;3$;62| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "div" $) |a| |b|) $)) 

(DEFUN |OUTFORM;rem;3$;63| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "rem" $) |a| |b|) $)) 

(DEFUN |OUTFORM;quo;3$;64| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "quo" $) |a| |b|) $)) 

(DEFUN |OUTFORM;exquo;3$;65| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "exquo" $) |a| |b|) $)) 

(DEFUN |OUTFORM;and;3$;66| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "and" $) |a| |b|) $)) 

(DEFUN |OUTFORM;or;3$;67| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "or" $) |a| |b|) $)) 

(DEFUN |OUTFORM;not;2$;68| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "not" $) |a|) $)) 

(DEFUN |OUTFORM;SEGMENT;3$;69| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SEGMENT $) |a| |b|) $)) 

(DEFUN |OUTFORM;SEGMENT;2$;70| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SEGMENT $) |a|) $)) 

(DEFUN |OUTFORM;binomial;3$;71| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BINOMIAL $) |a| |b|) $)) 

(DEFUN |OUTFORM;empty;$;72| ($)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'NOTHING $)) $)) 

(DEFUN |OUTFORM;infix?;$B;73| (|a| $)
  (PROG (#0=#:G1494 |e|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |e|
                            (COND
                              ((IDENTP |a|) |a|)
                              ((STRINGP |a|) (INTERN |a|))
                              ('T
                               (PROGN
                                 (LETT #0# 'NIL |OUTFORM;infix?;$B;73|)
                                 (GO #0#))))
                            |OUTFORM;infix?;$B;73|)
                      (EXIT (COND ((GET |e| 'INFIXOP) 'T) ('T 'NIL)))))
           #0# (EXIT #0#))))) 

(PUT '|OUTFORM;elt;$L$;74| '|SPADreplace| 'CONS) 

(DEFUN |OUTFORM;elt;$L$;74| (|a| |l| $) (CONS |a| |l|)) 

(DEFUN |OUTFORM;prefix;$L$;75| (|a| |l| $)
  (COND
    ((NULL (SPADCALL |a| (|getShellEntry| $ 96))) (CONS |a| |l|))
    ('T
     (SPADCALL |a|
         (SPADCALL (SPADCALL |l| (|getShellEntry| $ 50))
             (|getShellEntry| $ 60))
         (|getShellEntry| $ 36))))) 

(DEFUN |OUTFORM;infix;$L$;76| (|a| |l| $)
  (COND
    ((SPADCALL |l| (|getShellEntry| $ 66))
     (SPADCALL (|getShellEntry| $ 11)))
    ((SPADCALL (SPADCALL |l| (|getShellEntry| $ 67))
         (|getShellEntry| $ 66))
     (SPADCALL |l| (|getShellEntry| $ 68)))
    ((SPADCALL |a| (|getShellEntry| $ 96)) (CONS |a| |l|))
    ('T
     (SPADCALL
         (LIST (SPADCALL |l| (|getShellEntry| $ 68)) |a|
               (SPADCALL |a| (SPADCALL |l| (|getShellEntry| $ 67))
                   (|getShellEntry| $ 99)))
         (|getShellEntry| $ 73))))) 

(DEFUN |OUTFORM;infix;4$;77| (|a| |b| |c| $)
  (COND
    ((SPADCALL |a| (|getShellEntry| $ 96))
     (|OUTFORM;bless| (LIST |a| |b| |c|) $))
    ('T (SPADCALL (LIST |b| |a| |c|) (|getShellEntry| $ 73))))) 

(DEFUN |OUTFORM;postfix;3$;78| (|a| |b| $)
  (SPADCALL |b| |a| (|getShellEntry| $ 36))) 

(DEFUN |OUTFORM;string;2$;79| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'STRING $) |a|) $)) 

(DEFUN |OUTFORM;quote;2$;80| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'QUOTE $) |a|) $)) 

(DEFUN |OUTFORM;overbar;2$;81| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'OVERBAR $) |a|) $)) 

(DEFUN |OUTFORM;dot;2$;82| (|a| $)
  (SPADCALL |a| (|OUTFORM;sform| "." $) (|getShellEntry| $ 63))) 

(DEFUN |OUTFORM;prime;2$;83| (|a| $)
  (SPADCALL |a| (|OUTFORM;sform| "," $) (|getShellEntry| $ 63))) 

(DEFUN |OUTFORM;dot;$Nni$;84| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn|
                     (SPADCALL "." (|getShellEntry| $ 107)))
                 |OUTFORM;dot;$Nni$;84|)
           (EXIT (SPADCALL |a| (|OUTFORM;sform| |s| $)
                     (|getShellEntry| $ 63))))))) 

(DEFUN |OUTFORM;prime;$Nni$;85| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn|
                     (SPADCALL "," (|getShellEntry| $ 107)))
                 |OUTFORM;prime;$Nni$;85|)
           (EXIT (SPADCALL |a| (|OUTFORM;sform| |s| $)
                     (|getShellEntry| $ 63))))))) 

(DEFUN |OUTFORM;overlabel;3$;86| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'OVERLABEL $) |a| |b|) $)) 

(DEFUN |OUTFORM;box;2$;87| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BOX $) |a|) $)) 

(DEFUN |OUTFORM;zag;3$;88| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'ZAG $) |a| |b|) $)) 

(DEFUN |OUTFORM;root;2$;89| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'ROOT $) |a|) $)) 

(DEFUN |OUTFORM;root;3$;90| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'ROOT $) |a| |b|) $)) 

(DEFUN |OUTFORM;over;3$;91| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'OVER $) |a| |b|) $)) 

(DEFUN |OUTFORM;slash;3$;92| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SLASH $) |a| |b|) $)) 

(DEFUN |OUTFORM;assign;3$;93| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'LET $) |a| |b|) $)) 

(DEFUN |OUTFORM;label;3$;94| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'EQUATNUM $) |a| |b|) $)) 

(DEFUN |OUTFORM;rarrow;3$;95| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'TAG $) |a| |b|) $)) 

(DEFUN |OUTFORM;differentiate;$Nni$;96| (|a| |nn| $)
  (PROG (#0=#:G1524 |r| |s|)
    (RETURN
      (SEQ (COND
             ((ZEROP |nn|) |a|)
             ((< |nn| 4) (SPADCALL |a| |nn| (|getShellEntry| $ 109)))
             ('T
              (SEQ (LETT |r|
                         (SPADCALL
                             (PROG1 (LETT #0# |nn|
                                     |OUTFORM;differentiate;$Nni$;96|)
                               (|check-subtype| (> #0# 0)
                                   '(|PositiveInteger|) #0#))
                             (|getShellEntry| $ 122))
                         |OUTFORM;differentiate;$Nni$;96|)
                   (LETT |s| (SPADCALL |r| (|getShellEntry| $ 123))
                         |OUTFORM;differentiate;$Nni$;96|)
                   (EXIT (SPADCALL |a|
                             (SPADCALL (|OUTFORM;sform| |s| $)
                                 (|getShellEntry| $ 60))
                             (|getShellEntry| $ 63)))))))))) 

(DEFUN |OUTFORM;sum;2$;97| (|a| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SIGMA $)
            (SPADCALL (|getShellEntry| $ 11)) |a|)
      $)) 

(DEFUN |OUTFORM;sum;3$;98| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SIGMA $) |b| |a|) $)) 

(DEFUN |OUTFORM;sum;4$;99| (|a| |b| |c| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SIGMA2 $) |b| |c| |a|) $)) 

(DEFUN |OUTFORM;prod;2$;100| (|a| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'PI $) (SPADCALL (|getShellEntry| $ 11))
            |a|)
      $)) 

(DEFUN |OUTFORM;prod;3$;101| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'PI $) |b| |a|) $)) 

(DEFUN |OUTFORM;prod;4$;102| (|a| |b| |c| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'PI2 $) |b| |c| |a|) $)) 

(DEFUN |OUTFORM;int;2$;103| (|a| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'INTSIGN $)
            (SPADCALL (|getShellEntry| $ 11))
            (SPADCALL (|getShellEntry| $ 11)) |a|)
      $)) 

(DEFUN |OUTFORM;int;3$;104| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'INTSIGN $) |b|
            (SPADCALL (|getShellEntry| $ 11)) |a|)
      $)) 

(DEFUN |OUTFORM;int;4$;105| (|a| |b| |c| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'INTSIGN $) |b| |c| |a|) $)) 

(DEFUN |OutputForm| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1538)
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
        (LETT $ (|newShell| 135) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|OutputForm| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        $)))) 

(MAKEPROP '|OutputForm| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Void|) |OUTFORM;print;$V;5|
             (|Boolean|) (|String|) (0 . |empty?|) |OUTFORM;empty;$;72|
             |OUTFORM;message;S$;6| |OUTFORM;messagePrint;SV;7|
             |OUTFORM;=;2$B;8| |OUTFORM;=;3$;9| (|OutputForm|)
             |OUTFORM;coerce;2$;10| (|Integer|)
             |OUTFORM;outputForm;I$;11| (|Symbol|)
             |OUTFORM;outputForm;S$;12| (|DoubleFloat|)
             |OUTFORM;outputForm;Df$;13| (|Character|) (5 . |quote|)
             (9 . |concat|) (15 . |concat|) |OUTFORM;outputForm;S$;14|
             |OUTFORM;width;$I;15| |OUTFORM;height;$I;16|
             |OUTFORM;subHeight;$I;17| |OUTFORM;superHeight;$I;18|
             |OUTFORM;height;I;19| |OUTFORM;width;I;20|
             |OUTFORM;hspace;I$;28| |OUTFORM;hconcat;3$;47|
             |OUTFORM;center;$I$;21| |OUTFORM;left;$I$;22|
             |OUTFORM;right;$I$;23| |OUTFORM;center;2$;24|
             |OUTFORM;left;2$;25| |OUTFORM;right;2$;26|
             |OUTFORM;vspace;I$;27| |OUTFORM;vconcat;3$;49|
             |OUTFORM;rspace;2I$;29| (|List| $) (|List| 46)
             |OUTFORM;matrix;L$;30| |OUTFORM;pile;L$;31|
             |OUTFORM;commaSeparate;L$;32|
             |OUTFORM;semicolonSeparate;L$;33| (|List| $$)
             (21 . |reverse|) (26 . |append|)
             |OUTFORM;blankSeparate;L$;34| |OUTFORM;brace;2$;35|
             |OUTFORM;brace;L$;36| |OUTFORM;bracket;2$;37|
             |OUTFORM;bracket;L$;38| |OUTFORM;paren;2$;39|
             |OUTFORM;paren;L$;40| |OUTFORM;sub;3$;41|
             |OUTFORM;super;3$;42| |OUTFORM;presub;3$;43|
             |OUTFORM;presuper;3$;44| (32 . |null|) (37 . |rest|)
             (42 . |first|) |OUTFORM;scripts;$L$;45|
             (|NonNegativeInteger|) (47 . |#|)
             |OUTFORM;supersub;$L$;46| |OUTFORM;hconcat;L$;48|
             |OUTFORM;vconcat;L$;50| |OUTFORM;~=;3$;51|
             |OUTFORM;<;3$;52| |OUTFORM;>;3$;53| |OUTFORM;<=;3$;54|
             |OUTFORM;>=;3$;55| |OUTFORM;+;3$;56| |OUTFORM;-;3$;57|
             |OUTFORM;-;2$;58| |OUTFORM;*;3$;59| |OUTFORM;/;3$;60|
             |OUTFORM;**;3$;61| |OUTFORM;div;3$;62| |OUTFORM;rem;3$;63|
             |OUTFORM;quo;3$;64| |OUTFORM;exquo;3$;65|
             |OUTFORM;and;3$;66| |OUTFORM;or;3$;67| |OUTFORM;not;2$;68|
             |OUTFORM;SEGMENT;3$;69| |OUTFORM;SEGMENT;2$;70|
             |OUTFORM;binomial;3$;71| |OUTFORM;infix?;$B;73|
             |OUTFORM;elt;$L$;74| |OUTFORM;prefix;$L$;75|
             |OUTFORM;infix;$L$;76| |OUTFORM;infix;4$;77|
             |OUTFORM;postfix;3$;78| |OUTFORM;string;2$;79|
             |OUTFORM;quote;2$;80| |OUTFORM;overbar;2$;81|
             |OUTFORM;dot;2$;82| |OUTFORM;prime;2$;83| (52 . |char|)
             |OUTFORM;dot;$Nni$;84| |OUTFORM;prime;$Nni$;85|
             |OUTFORM;overlabel;3$;86| |OUTFORM;box;2$;87|
             |OUTFORM;zag;3$;88| |OUTFORM;root;2$;89|
             |OUTFORM;root;3$;90| |OUTFORM;over;3$;91|
             |OUTFORM;slash;3$;92| |OUTFORM;assign;3$;93|
             |OUTFORM;label;3$;94| |OUTFORM;rarrow;3$;95|
             (|PositiveInteger|) (|NumberFormats|) (57 . |FormatRoman|)
             (62 . |lowerCase|) |OUTFORM;differentiate;$Nni$;96|
             |OUTFORM;sum;2$;97| |OUTFORM;sum;3$;98|
             |OUTFORM;sum;4$;99| |OUTFORM;prod;2$;100|
             |OUTFORM;prod;3$;101| |OUTFORM;prod;4$;102|
             |OUTFORM;int;2$;103| |OUTFORM;int;3$;104|
             |OUTFORM;int;4$;105| (|SingleInteger|))
          '#(~= 67 |zag| 79 |width| 85 |vspace| 94 |vconcat| 99
             |supersub| 110 |superHeight| 116 |super| 121 |sum| 127
             |subHeight| 145 |sub| 150 |string| 156 |slash| 161
             |semicolonSeparate| 167 |scripts| 172 |rspace| 178 |root|
             184 |right| 195 |rem| 206 |rarrow| 212 |quote| 218 |quo|
             223 |prod| 229 |print| 247 |prime| 252 |presuper| 263
             |presub| 269 |prefix| 275 |postfix| 281 |pile| 287 |paren|
             292 |overlabel| 302 |overbar| 308 |over| 313 |outputForm|
             319 |or| 339 |not| 345 |messagePrint| 350 |message| 355
             |matrix| 360 |left| 365 |latex| 376 |label| 381 |int| 387
             |infix?| 405 |infix| 410 |hspace| 423 |height| 428
             |hconcat| 437 |hash| 448 |exquo| 453 |empty| 459 |elt| 463
             |dot| 469 |div| 480 |differentiate| 486 |commaSeparate|
             492 |coerce| 497 |center| 502 |bracket| 513 |brace| 523
             |box| 533 |blankSeparate| 538 |binomial| 543 |assign| 549
             |and| 555 SEGMENT 561 >= 572 > 578 = 584 <= 596 < 602 /
             608 - 614 + 625 ** 631 * 637)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0))
                (CONS '#(|SetCategory&| |BasicType&| NIL)
                      (CONS '#((|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 16))
                            (|makeByteWordVec2| 134
                                '(1 9 8 0 10 0 24 0 25 2 9 0 0 24 26 2
                                  9 0 24 0 27 1 52 0 0 53 2 52 0 0 0 54
                                  1 52 8 0 66 1 52 0 0 67 1 52 2 0 68 1
                                  52 70 0 71 1 24 0 9 107 1 121 9 120
                                  122 1 9 0 0 123 2 0 0 0 0 75 2 0 8 0
                                  0 1 2 0 0 0 0 112 0 0 18 34 1 0 18 0
                                  29 1 0 0 18 43 1 0 0 46 74 2 0 0 0 0
                                  44 2 0 0 0 46 72 1 0 18 0 32 2 0 0 0
                                  0 63 2 0 0 0 0 126 3 0 0 0 0 0 127 1
                                  0 0 0 125 1 0 18 0 31 2 0 0 0 0 62 1
                                  0 0 0 102 2 0 0 0 0 116 1 0 0 46 51 2
                                  0 0 0 46 69 2 0 0 18 18 45 1 0 0 0
                                  113 2 0 0 0 0 114 1 0 0 0 42 2 0 0 0
                                  18 39 2 0 0 0 0 87 2 0 0 0 0 119 1 0
                                  0 0 103 2 0 0 0 0 88 3 0 0 0 0 0 130
                                  1 0 0 0 128 2 0 0 0 0 129 1 0 6 0 7 2
                                  0 0 0 70 109 1 0 0 0 106 2 0 0 0 0 65
                                  2 0 0 0 0 64 2 0 0 0 46 98 2 0 0 0 0
                                  101 1 0 0 46 49 1 0 0 46 61 1 0 0 0
                                  60 2 0 0 0 0 110 1 0 0 0 104 2 0 0 0
                                  0 115 1 0 0 9 28 1 0 0 22 23 1 0 0 20
                                  21 1 0 0 18 19 2 0 0 0 0 91 1 0 0 0
                                  92 1 0 6 9 13 1 0 0 9 12 1 0 0 47 48
                                  1 0 0 0 41 2 0 0 0 18 38 1 0 9 0 1 2
                                  0 0 0 0 118 3 0 0 0 0 0 133 2 0 0 0 0
                                  132 1 0 0 0 131 1 0 8 0 96 2 0 0 0 46
                                  99 3 0 0 0 0 0 100 1 0 0 18 35 0 0 18
                                  33 1 0 18 0 30 1 0 0 46 73 2 0 0 0 0
                                  36 1 0 134 0 1 2 0 0 0 0 89 0 0 0 11
                                  2 0 0 0 46 97 2 0 0 0 70 108 1 0 0 0
                                  105 2 0 0 0 0 86 2 0 0 0 70 124 1 0 0
                                  46 50 1 0 16 0 17 1 0 0 0 40 2 0 0 0
                                  18 37 1 0 0 0 58 1 0 0 46 59 1 0 0 46
                                  57 1 0 0 0 56 1 0 0 0 111 1 0 0 46 55
                                  2 0 0 0 0 95 2 0 0 0 0 117 2 0 0 0 0
                                  90 1 0 0 0 94 2 0 0 0 0 93 2 0 0 0 0
                                  79 2 0 0 0 0 77 2 0 0 0 0 15 2 0 8 0
                                  0 14 2 0 0 0 0 78 2 0 0 0 0 76 2 0 0
                                  0 0 84 1 0 0 0 82 2 0 0 0 0 81 2 0 0
                                  0 0 80 2 0 0 0 0 85 2 0 0 0 0 83)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|OutputForm| '|isFunctor|
             '(((SEGMENT ($ $)) T (ELT $ 94))
               ((SEGMENT ($ $ $)) T (ELT $ 93))
               ((|not| ($ $)) T (ELT $ 92))
               ((|or| ($ $ $)) T (ELT $ 91))
               ((|and| ($ $ $)) T (ELT $ 90))
               ((|exquo| ($ $ $)) T (ELT $ 89))
               ((|quo| ($ $ $)) T (ELT $ 88))
               ((|rem| ($ $ $)) T (ELT $ 87))
               ((|div| ($ $ $)) T (ELT $ 86))
               ((** ($ $ $)) T (ELT $ 85)) ((/ ($ $ $)) T (ELT $ 84))
               ((* ($ $ $)) T (ELT $ 83)) ((- ($ $)) T (ELT $ 82))
               ((- ($ $ $)) T (ELT $ 81)) ((+ ($ $ $)) T (ELT $ 80))
               ((>= ($ $ $)) T (ELT $ 79)) ((<= ($ $ $)) T (ELT $ 78))
               ((> ($ $ $)) T (ELT $ 77)) ((< ($ $ $)) T (ELT $ 76))
               ((~= ($ $ $)) T (ELT $ 75)) ((= ($ $ $)) T (ELT $ 15))
               ((|blankSeparate| ($ (|List| $))) T (ELT $ 55))
               ((|semicolonSeparate| ($ (|List| $))) T (ELT $ 51))
               ((|commaSeparate| ($ (|List| $))) T (ELT $ 50))
               ((|pile| ($ (|List| $))) T (ELT $ 49))
               ((|paren| ($ (|List| $))) T (ELT $ 61))
               ((|paren| ($ $)) T (ELT $ 60))
               ((|bracket| ($ (|List| $))) T (ELT $ 59))
               ((|bracket| ($ $)) T (ELT $ 58))
               ((|brace| ($ (|List| $))) T (ELT $ 57))
               ((|brace| ($ $)) T (ELT $ 56))
               ((|int| ($ $ $ $)) T (ELT $ 133))
               ((|int| ($ $ $)) T (ELT $ 132))
               ((|int| ($ $)) T (ELT $ 131))
               ((|prod| ($ $ $ $)) T (ELT $ 130))
               ((|prod| ($ $ $)) T (ELT $ 129))
               ((|prod| ($ $)) T (ELT $ 128))
               ((|sum| ($ $ $ $)) T (ELT $ 127))
               ((|sum| ($ $ $)) T (ELT $ 126))
               ((|sum| ($ $)) T (ELT $ 125))
               ((|overlabel| ($ $ $)) T (ELT $ 110))
               ((|overbar| ($ $)) T (ELT $ 104))
               ((|prime| ($ $ (|NonNegativeInteger|))) T (ELT $ 109))
               ((|prime| ($ $)) T (ELT $ 106))
               ((|dot| ($ $ (|NonNegativeInteger|))) T (ELT $ 108))
               ((|dot| ($ $)) T (ELT $ 105))
               ((|quote| ($ $)) T (ELT $ 103))
               ((|supersub| ($ $ (|List| $))) T (ELT $ 72))
               ((|scripts| ($ $ (|List| $))) T (ELT $ 69))
               ((|presuper| ($ $ $)) T (ELT $ 65))
               ((|presub| ($ $ $)) T (ELT $ 64))
               ((|super| ($ $ $)) T (ELT $ 63))
               ((|sub| ($ $ $)) T (ELT $ 62))
               ((|binomial| ($ $ $)) T (ELT $ 95))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ 124))
               ((|rarrow| ($ $ $)) T (ELT $ 119))
               ((|assign| ($ $ $)) T (ELT $ 117))
               ((|slash| ($ $ $)) T (ELT $ 116))
               ((|over| ($ $ $)) T (ELT $ 115))
               ((|root| ($ $ $)) T (ELT $ 114))
               ((|root| ($ $)) T (ELT $ 113))
               ((|zag| ($ $ $)) T (ELT $ 112))
               ((|matrix| ($ (|List| (|List| $)))) T (ELT $ 48))
               ((|box| ($ $)) T (ELT $ 111))
               ((|label| ($ $ $)) T (ELT $ 118))
               ((|string| ($ $)) T (ELT $ 102))
               ((|elt| ($ $ (|List| $))) T (ELT $ 97))
               ((|infix?| ((|Boolean|) $)) T (ELT $ 96))
               ((|postfix| ($ $ $)) T (ELT $ 101))
               ((|infix| ($ $ $ $)) T (ELT $ 100))
               ((|infix| ($ $ (|List| $))) T (ELT $ 99))
               ((|prefix| ($ $ (|List| $))) T (ELT $ 98))
               ((|vconcat| ($ (|List| $))) T (ELT $ 74))
               ((|hconcat| ($ (|List| $))) T (ELT $ 73))
               ((|vconcat| ($ $ $)) T (ELT $ 44))
               ((|hconcat| ($ $ $)) T (ELT $ 36))
               ((|center| ($ $)) T (ELT $ 40))
               ((|right| ($ $)) T (ELT $ 42))
               ((|left| ($ $)) T (ELT $ 41))
               ((|center| ($ $ (|Integer|))) T (ELT $ 37))
               ((|right| ($ $ (|Integer|))) T (ELT $ 39))
               ((|left| ($ $ (|Integer|))) T (ELT $ 38))
               ((|rspace| ($ (|Integer|) (|Integer|))) T (ELT $ 45))
               ((|vspace| ($ (|Integer|))) T (ELT $ 43))
               ((|hspace| ($ (|Integer|))) T (ELT $ 35))
               ((|superHeight| ((|Integer|) $)) T (ELT $ 32))
               ((|subHeight| ((|Integer|) $)) T (ELT $ 31))
               ((|height| ((|Integer|))) T (ELT $ 33))
               ((|width| ((|Integer|))) T (ELT $ 34))
               ((|height| ((|Integer|) $)) T (ELT $ 30))
               ((|width| ((|Integer|) $)) T (ELT $ 29))
               ((|empty| ($)) T (ELT $ 11))
               ((|outputForm| ($ (|DoubleFloat|))) T (ELT $ 23))
               ((|outputForm| ($ (|String|))) T (ELT $ 28))
               ((|outputForm| ($ (|Symbol|))) T (ELT $ 21))
               ((|outputForm| ($ (|Integer|))) T (ELT $ 19))
               ((|messagePrint| ((|Void|) (|String|))) T (ELT $ 13))
               ((|message| ($ (|String|))) T (ELT $ 12))
               ((|print| ((|Void|) $)) T (ELT $ 7))
               ((|latex| ((|String|) $)) T (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 17))
               ((= ((|Boolean|) $ $)) T (ELT $ 14))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|OutputForm| '(|OutputForm|)
                 '((|Join| (|SetCategory|)
                           (CATEGORY |domain|
                               (SIGNATURE |print| ((|Void|) $))
                               (SIGNATURE |message| ($ (|String|)))
                               (SIGNATURE |messagePrint|
                                   ((|Void|) (|String|)))
                               (SIGNATURE |outputForm| ($ (|Integer|)))
                               (SIGNATURE |outputForm| ($ (|Symbol|)))
                               (SIGNATURE |outputForm| ($ (|String|)))
                               (SIGNATURE |outputForm|
                                   ($ (|DoubleFloat|)))
                               (SIGNATURE |empty| ($))
                               (SIGNATURE |width| ((|Integer|) $))
                               (SIGNATURE |height| ((|Integer|) $))
                               (SIGNATURE |width| ((|Integer|)))
                               (SIGNATURE |height| ((|Integer|)))
                               (SIGNATURE |subHeight| ((|Integer|) $))
                               (SIGNATURE |superHeight|
                                   ((|Integer|) $))
                               (SIGNATURE |hspace| ($ (|Integer|)))
                               (SIGNATURE |vspace| ($ (|Integer|)))
                               (SIGNATURE |rspace|
                                   ($ (|Integer|) (|Integer|)))
                               (SIGNATURE |left| ($ $ (|Integer|)))
                               (SIGNATURE |right| ($ $ (|Integer|)))
                               (SIGNATURE |center| ($ $ (|Integer|)))
                               (SIGNATURE |left| ($ $))
                               (SIGNATURE |right| ($ $))
                               (SIGNATURE |center| ($ $))
                               (SIGNATURE |hconcat| ($ $ $))
                               (SIGNATURE |vconcat| ($ $ $))
                               (SIGNATURE |hconcat| ($ (|List| $)))
                               (SIGNATURE |vconcat| ($ (|List| $)))
                               (SIGNATURE |prefix| ($ $ (|List| $)))
                               (SIGNATURE |infix| ($ $ (|List| $)))
                               (SIGNATURE |infix| ($ $ $ $))
                               (SIGNATURE |postfix| ($ $ $))
                               (SIGNATURE |infix?| ((|Boolean|) $))
                               (SIGNATURE |elt| ($ $ (|List| $)))
                               (SIGNATURE |string| ($ $))
                               (SIGNATURE |label| ($ $ $))
                               (SIGNATURE |box| ($ $))
                               (SIGNATURE |matrix|
                                   ($ (|List| (|List| $))))
                               (SIGNATURE |zag| ($ $ $))
                               (SIGNATURE |root| ($ $))
                               (SIGNATURE |root| ($ $ $))
                               (SIGNATURE |over| ($ $ $))
                               (SIGNATURE |slash| ($ $ $))
                               (SIGNATURE |assign| ($ $ $))
                               (SIGNATURE |rarrow| ($ $ $))
                               (SIGNATURE |differentiate|
                                   ($ $ (|NonNegativeInteger|)))
                               (SIGNATURE |binomial| ($ $ $))
                               (SIGNATURE |sub| ($ $ $))
                               (SIGNATURE |super| ($ $ $))
                               (SIGNATURE |presub| ($ $ $))
                               (SIGNATURE |presuper| ($ $ $))
                               (SIGNATURE |scripts| ($ $ (|List| $)))
                               (SIGNATURE |supersub| ($ $ (|List| $)))
                               (SIGNATURE |quote| ($ $))
                               (SIGNATURE |dot| ($ $))
                               (SIGNATURE |dot|
                                   ($ $ (|NonNegativeInteger|)))
                               (SIGNATURE |prime| ($ $))
                               (SIGNATURE |prime|
                                   ($ $ (|NonNegativeInteger|)))
                               (SIGNATURE |overbar| ($ $))
                               (SIGNATURE |overlabel| ($ $ $))
                               (SIGNATURE |sum| ($ $))
                               (SIGNATURE |sum| ($ $ $))
                               (SIGNATURE |sum| ($ $ $ $))
                               (SIGNATURE |prod| ($ $))
                               (SIGNATURE |prod| ($ $ $))
                               (SIGNATURE |prod| ($ $ $ $))
                               (SIGNATURE |int| ($ $))
                               (SIGNATURE |int| ($ $ $))
                               (SIGNATURE |int| ($ $ $ $))
                               (SIGNATURE |brace| ($ $))
                               (SIGNATURE |brace| ($ (|List| $)))
                               (SIGNATURE |bracket| ($ $))
                               (SIGNATURE |bracket| ($ (|List| $)))
                               (SIGNATURE |paren| ($ $))
                               (SIGNATURE |paren| ($ (|List| $)))
                               (SIGNATURE |pile| ($ (|List| $)))
                               (SIGNATURE |commaSeparate|
                                   ($ (|List| $)))
                               (SIGNATURE |semicolonSeparate|
                                   ($ (|List| $)))
                               (SIGNATURE |blankSeparate|
                                   ($ (|List| $)))
                               (SIGNATURE = ($ $ $))
                               (SIGNATURE ~= ($ $ $))
                               (SIGNATURE < ($ $ $))
                               (SIGNATURE > ($ $ $))
                               (SIGNATURE <= ($ $ $))
                               (SIGNATURE >= ($ $ $))
                               (SIGNATURE + ($ $ $))
                               (SIGNATURE - ($ $ $))
                               (SIGNATURE - ($ $))
                               (SIGNATURE * ($ $ $))
                               (SIGNATURE / ($ $ $))
                               (SIGNATURE ** ($ $ $))
                               (SIGNATURE |div| ($ $ $))
                               (SIGNATURE |rem| ($ $ $))
                               (SIGNATURE |quo| ($ $ $))
                               (SIGNATURE |exquo| ($ $ $))
                               (SIGNATURE |and| ($ $ $))
                               (SIGNATURE |or| ($ $ $))
                               (SIGNATURE |not| ($ $))
                               (SIGNATURE SEGMENT ($ $ $))
                               (SIGNATURE SEGMENT ($ $)))))
                 T '|OutputForm|
                 (|put| '|OutputForm| '|mode|
                        '(|Mapping|
                             (|Join| (|SetCategory|)
                                     (CATEGORY |domain|
                                      (SIGNATURE |print| ((|Void|) $))
                                      (SIGNATURE |message|
                                       ($ (|String|)))
                                      (SIGNATURE |messagePrint|
                                       ((|Void|) (|String|)))
                                      (SIGNATURE |outputForm|
                                       ($ (|Integer|)))
                                      (SIGNATURE |outputForm|
                                       ($ (|Symbol|)))
                                      (SIGNATURE |outputForm|
                                       ($ (|String|)))
                                      (SIGNATURE |outputForm|
                                       ($ (|DoubleFloat|)))
                                      (SIGNATURE |empty| ($))
                                      (SIGNATURE |width|
                                       ((|Integer|) $))
                                      (SIGNATURE |height|
                                       ((|Integer|) $))
                                      (SIGNATURE |width| ((|Integer|)))
                                      (SIGNATURE |height|
                                       ((|Integer|)))
                                      (SIGNATURE |subHeight|
                                       ((|Integer|) $))
                                      (SIGNATURE |superHeight|
                                       ((|Integer|) $))
                                      (SIGNATURE |hspace|
                                       ($ (|Integer|)))
                                      (SIGNATURE |vspace|
                                       ($ (|Integer|)))
                                      (SIGNATURE |rspace|
                                       ($ (|Integer|) (|Integer|)))
                                      (SIGNATURE |left|
                                       ($ $ (|Integer|)))
                                      (SIGNATURE |right|
                                       ($ $ (|Integer|)))
                                      (SIGNATURE |center|
                                       ($ $ (|Integer|)))
                                      (SIGNATURE |left| ($ $))
                                      (SIGNATURE |right| ($ $))
                                      (SIGNATURE |center| ($ $))
                                      (SIGNATURE |hconcat| ($ $ $))
                                      (SIGNATURE |vconcat| ($ $ $))
                                      (SIGNATURE |hconcat|
                                       ($ (|List| $)))
                                      (SIGNATURE |vconcat|
                                       ($ (|List| $)))
                                      (SIGNATURE |prefix|
                                       ($ $ (|List| $)))
                                      (SIGNATURE |infix|
                                       ($ $ (|List| $)))
                                      (SIGNATURE |infix| ($ $ $ $))
                                      (SIGNATURE |postfix| ($ $ $))
                                      (SIGNATURE |infix?|
                                       ((|Boolean|) $))
                                      (SIGNATURE |elt|
                                       ($ $ (|List| $)))
                                      (SIGNATURE |string| ($ $))
                                      (SIGNATURE |label| ($ $ $))
                                      (SIGNATURE |box| ($ $))
                                      (SIGNATURE |matrix|
                                       ($ (|List| (|List| $))))
                                      (SIGNATURE |zag| ($ $ $))
                                      (SIGNATURE |root| ($ $))
                                      (SIGNATURE |root| ($ $ $))
                                      (SIGNATURE |over| ($ $ $))
                                      (SIGNATURE |slash| ($ $ $))
                                      (SIGNATURE |assign| ($ $ $))
                                      (SIGNATURE |rarrow| ($ $ $))
                                      (SIGNATURE |differentiate|
                                       ($ $ (|NonNegativeInteger|)))
                                      (SIGNATURE |binomial| ($ $ $))
                                      (SIGNATURE |sub| ($ $ $))
                                      (SIGNATURE |super| ($ $ $))
                                      (SIGNATURE |presub| ($ $ $))
                                      (SIGNATURE |presuper| ($ $ $))
                                      (SIGNATURE |scripts|
                                       ($ $ (|List| $)))
                                      (SIGNATURE |supersub|
                                       ($ $ (|List| $)))
                                      (SIGNATURE |quote| ($ $))
                                      (SIGNATURE |dot| ($ $))
                                      (SIGNATURE |dot|
                                       ($ $ (|NonNegativeInteger|)))
                                      (SIGNATURE |prime| ($ $))
                                      (SIGNATURE |prime|
                                       ($ $ (|NonNegativeInteger|)))
                                      (SIGNATURE |overbar| ($ $))
                                      (SIGNATURE |overlabel| ($ $ $))
                                      (SIGNATURE |sum| ($ $))
                                      (SIGNATURE |sum| ($ $ $))
                                      (SIGNATURE |sum| ($ $ $ $))
                                      (SIGNATURE |prod| ($ $))
                                      (SIGNATURE |prod| ($ $ $))
                                      (SIGNATURE |prod| ($ $ $ $))
                                      (SIGNATURE |int| ($ $))
                                      (SIGNATURE |int| ($ $ $))
                                      (SIGNATURE |int| ($ $ $ $))
                                      (SIGNATURE |brace| ($ $))
                                      (SIGNATURE |brace|
                                       ($ (|List| $)))
                                      (SIGNATURE |bracket| ($ $))
                                      (SIGNATURE |bracket|
                                       ($ (|List| $)))
                                      (SIGNATURE |paren| ($ $))
                                      (SIGNATURE |paren|
                                       ($ (|List| $)))
                                      (SIGNATURE |pile| ($ (|List| $)))
                                      (SIGNATURE |commaSeparate|
                                       ($ (|List| $)))
                                      (SIGNATURE |semicolonSeparate|
                                       ($ (|List| $)))
                                      (SIGNATURE |blankSeparate|
                                       ($ (|List| $)))
                                      (SIGNATURE = ($ $ $))
                                      (SIGNATURE ~= ($ $ $))
                                      (SIGNATURE < ($ $ $))
                                      (SIGNATURE > ($ $ $))
                                      (SIGNATURE <= ($ $ $))
                                      (SIGNATURE >= ($ $ $))
                                      (SIGNATURE + ($ $ $))
                                      (SIGNATURE - ($ $ $))
                                      (SIGNATURE - ($ $))
                                      (SIGNATURE * ($ $ $))
                                      (SIGNATURE / ($ $ $))
                                      (SIGNATURE ** ($ $ $))
                                      (SIGNATURE |div| ($ $ $))
                                      (SIGNATURE |rem| ($ $ $))
                                      (SIGNATURE |quo| ($ $ $))
                                      (SIGNATURE |exquo| ($ $ $))
                                      (SIGNATURE |and| ($ $ $))
                                      (SIGNATURE |or| ($ $ $))
                                      (SIGNATURE |not| ($ $))
                                      (SIGNATURE SEGMENT ($ $ $))
                                      (SIGNATURE SEGMENT ($ $)))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|OutputForm| 'NILADIC T) 
