
(/VERSIONCHECK 2) 

(DEFUN |OUTFORM;doubleFloatFormat;2S;1| (|s| $)
  (PROG (|ss|)
    (RETURN
      (SEQ (LETT |ss| (|getShellEntry| $ 6)
                 |OUTFORM;doubleFloatFormat;2S;1|)
           (SETELT $ 6 |s|) (EXIT |ss|))))) 

(PUT '|OUTFORM;sform| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DEFUN |OUTFORM;sform| (|s| $) |s|) 

(PUT '|OUTFORM;eform| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DEFUN |OUTFORM;eform| (|e| $) |e|) 

(PUT '|OUTFORM;iform| '|SPADreplace| '(XLAM (|i|) |i|)) 

(DEFUN |OUTFORM;iform| (|i| $) |i|) 

(PUT '|OUTFORM;bless| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DEFUN |OUTFORM;bless| (|x| $) |x|) 

(PUT '|OUTFORM;print;$V;6| '|SPADreplace| '|mathprint|) 

(DEFUN |OUTFORM;print;$V;6| (|x| $) (|mathprint| |x|)) 

(DEFUN |OUTFORM;message;S$;7| (|s| $)
  (COND
    ((SPADCALL |s| (|getShellEntry| $ 12))
     (SPADCALL (|getShellEntry| $ 13)))
    ('T |s|))) 

(DEFUN |OUTFORM;messagePrint;SV;8| (|s| $)
  (SPADCALL (SPADCALL |s| (|getShellEntry| $ 14))
            (|getShellEntry| $ 10))) 

(PUT '|OUTFORM;=;2$B;9| '|SPADreplace| 'EQUAL) 

(DEFUN |OUTFORM;=;2$B;9| (|a| |b| $) (EQUAL |a| |b|)) 

(DEFUN |OUTFORM;=;3$;10| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "=" $) |a| |b|) $)) 

(PUT '|OUTFORM;coerce;2$;11| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DEFUN |OUTFORM;coerce;2$;11| (|a| $) |a|) 

(PUT '|OUTFORM;outputForm;I$;12| '|SPADreplace| '(XLAM (|n|) |n|)) 

(DEFUN |OUTFORM;outputForm;I$;12| (|n| $) |n|) 

(PUT '|OUTFORM;outputForm;S$;13| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DEFUN |OUTFORM;outputForm;S$;13| (|e| $) |e|) 

(DEFUN |OUTFORM;outputForm;Df$;14| (|f| $)
  (FORMAT NIL (|getShellEntry| $ 6) |f|)) 

(DEFUN |OUTFORM;outputForm;S$;15| (|s| $)
  (|OUTFORM;sform|
      (SPADCALL (SPADCALL (|getShellEntry| $ 27))
          (SPADCALL |s| (SPADCALL (|getShellEntry| $ 27))
              (|getShellEntry| $ 28))
          (|getShellEntry| $ 29))
      $)) 

(PUT '|OUTFORM;width;$I;16| '|SPADreplace| '|outformWidth|) 

(DEFUN |OUTFORM;width;$I;16| (|a| $) (|outformWidth| |a|)) 

(PUT '|OUTFORM;height;$I;17| '|SPADreplace| '|height|) 

(DEFUN |OUTFORM;height;$I;17| (|a| $) (|height| |a|)) 

(PUT '|OUTFORM;subHeight;$I;18| '|SPADreplace| '|subspan|) 

(DEFUN |OUTFORM;subHeight;$I;18| (|a| $) (|subspan| |a|)) 

(PUT '|OUTFORM;superHeight;$I;19| '|SPADreplace| '|superspan|) 

(DEFUN |OUTFORM;superHeight;$I;19| (|a| $) (|superspan| |a|)) 

(PUT '|OUTFORM;height;I;20| '|SPADreplace| '(XLAM NIL 20)) 

(DEFUN |OUTFORM;height;I;20| ($) 20) 

(PUT '|OUTFORM;width;I;21| '|SPADreplace| '(XLAM NIL 66)) 

(DEFUN |OUTFORM;width;I;21| ($) 66) 

(DEFUN |OUTFORM;center;$I$;22| (|a| |w| $)
  (SPADCALL
      (SPADCALL
          (QUOTIENT2 (- |w| (SPADCALL |a| (|getShellEntry| $ 31))) 2)
          (|getShellEntry| $ 37))
      |a| (|getShellEntry| $ 38))) 

(DEFUN |OUTFORM;left;$I$;23| (|a| |w| $)
  (SPADCALL |a|
            (SPADCALL (- |w| (SPADCALL |a| (|getShellEntry| $ 31)))
                (|getShellEntry| $ 37))
            (|getShellEntry| $ 38))) 

(DEFUN |OUTFORM;right;$I$;24| (|a| |w| $)
  (SPADCALL
      (SPADCALL (- |w| (SPADCALL |a| (|getShellEntry| $ 31)))
          (|getShellEntry| $ 37))
      |a| (|getShellEntry| $ 38))) 

(DEFUN |OUTFORM;center;2$;25| (|a| $)
  (SPADCALL |a| (SPADCALL (|getShellEntry| $ 36))
      (|getShellEntry| $ 39))) 

(DEFUN |OUTFORM;left;2$;26| (|a| $)
  (SPADCALL |a| (SPADCALL (|getShellEntry| $ 36))
      (|getShellEntry| $ 40))) 

(DEFUN |OUTFORM;right;2$;27| (|a| $)
  (SPADCALL |a| (SPADCALL (|getShellEntry| $ 36))
      (|getShellEntry| $ 41))) 

(DEFUN |OUTFORM;vspace;I$;28| (|n| $)
  (COND
    ((EQL |n| 0) (SPADCALL (|getShellEntry| $ 13)))
    ('T
     (SPADCALL (|OUTFORM;sform| " " $)
         (SPADCALL (- |n| 1) (|getShellEntry| $ 45))
         (|getShellEntry| $ 46))))) 

(DEFUN |OUTFORM;hspace;I$;29| (|n| $)
  (COND
    ((EQL |n| 0) (SPADCALL (|getShellEntry| $ 13)))
    ('T (|OUTFORM;sform| (|fillerSpaces| |n|) $)))) 

(DEFUN |OUTFORM;rspace;2I$;30| (|n| |m| $)
  (COND
    ((OR (EQL |n| 0) (EQL |m| 0)) (SPADCALL (|getShellEntry| $ 13)))
    ('T
     (SPADCALL (SPADCALL |n| (|getShellEntry| $ 37))
         (SPADCALL |n| (- |m| 1) (|getShellEntry| $ 47))
         (|getShellEntry| $ 46))))) 

(DEFUN |OUTFORM;matrix;L$;31| (|ll| $)
  (PROG (#0=#:G1445 |l| #1=#:G1446 |lv|)
    (RETURN
      (SEQ (LETT |lv|
                 (|OUTFORM;bless|
                     (PROGN
                       (LETT #0# NIL |OUTFORM;matrix;L$;31|)
                       (SEQ (LETT |l| NIL |OUTFORM;matrix;L$;31|)
                            (LETT #1# |ll| |OUTFORM;matrix;L$;31|) G190
                            (COND
                              ((OR (ATOM #1#)
                                   (PROGN
                                     (LETT |l| (CAR #1#)
                                      |OUTFORM;matrix;L$;31|)
                                     NIL))
                               (GO G191)))
                            (SEQ (EXIT (LETT #0#
                                        (CONS (LIST2VEC |l|) #0#)
                                        |OUTFORM;matrix;L$;31|)))
                            (LETT #1# (CDR #1#) |OUTFORM;matrix;L$;31|)
                            (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                     $)
                 |OUTFORM;matrix;L$;31|)
           (EXIT (CONS (|OUTFORM;eform| 'MATRIX $) (LIST2VEC |lv|))))))) 

(DEFUN |OUTFORM;pile;L$;32| (|l| $)
  (CONS (|OUTFORM;eform| 'SC $) |l|)) 

(DEFUN |OUTFORM;commaSeparate;L$;33| (|l| $)
  (CONS (|OUTFORM;eform| 'AGGLST $) |l|)) 

(DEFUN |OUTFORM;semicolonSeparate;L$;34| (|l| $)
  (CONS (|OUTFORM;eform| 'AGGSET $) |l|)) 

(DEFUN |OUTFORM;blankSeparate;L$;35| (|l| $)
  (PROG (|c| |u| #0=#:G1454 |l1|)
    (RETURN
      (SEQ (LETT |c| (|OUTFORM;eform| 'CONCATB $)
                 |OUTFORM;blankSeparate;L$;35|)
           (LETT |l1| NIL |OUTFORM;blankSeparate;L$;35|)
           (SEQ (LETT |u| NIL |OUTFORM;blankSeparate;L$;35|)
                (LETT #0# (SPADCALL |l| (|getShellEntry| $ 55))
                      |OUTFORM;blankSeparate;L$;35|)
                G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |u| (CAR #0#)
                               |OUTFORM;blankSeparate;L$;35|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (COND
                             ((EQCAR |u| |c|)
                              (LETT |l1|
                                    (SPADCALL (CDR |u|) |l1|
                                     (|getShellEntry| $ 56))
                                    |OUTFORM;blankSeparate;L$;35|))
                             ('T
                              (LETT |l1| (CONS |u| |l1|)
                                    |OUTFORM;blankSeparate;L$;35|)))))
                (LETT #0# (CDR #0#) |OUTFORM;blankSeparate;L$;35|)
                (GO G190) G191 (EXIT NIL))
           (EXIT (CONS |c| |l1|)))))) 

(DEFUN |OUTFORM;brace;2$;36| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BRACE $) |a|) $)) 

(DEFUN |OUTFORM;brace;L$;37| (|l| $)
  (SPADCALL (SPADCALL |l| (|getShellEntry| $ 52))
      (|getShellEntry| $ 58))) 

(DEFUN |OUTFORM;bracket;2$;38| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BRACKET $) |a|) $)) 

(DEFUN |OUTFORM;bracket;L$;39| (|l| $)
  (SPADCALL (SPADCALL |l| (|getShellEntry| $ 52))
      (|getShellEntry| $ 60))) 

(DEFUN |OUTFORM;paren;2$;40| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'PAREN $) |a|) $)) 

(DEFUN |OUTFORM;paren;L$;41| (|l| $)
  (SPADCALL (SPADCALL |l| (|getShellEntry| $ 52))
      (|getShellEntry| $ 62))) 

(DEFUN |OUTFORM;sub;3$;42| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SUB $) |a| |b|) $)) 

(DEFUN |OUTFORM;super;3$;43| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
            |b|)
      $)) 

(DEFUN |OUTFORM;presub;3$;44| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
            (|OUTFORM;sform| " " $) (|OUTFORM;sform| " " $) |b|)
      $)) 

(DEFUN |OUTFORM;presuper;3$;45| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SUPERSUB $) |a| (|OUTFORM;sform| " " $)
            (|OUTFORM;sform| " " $) |b|)
      $)) 

(DEFUN |OUTFORM;scripts;$L$;46| (|a| |l| $)
  (COND
    ((SPADCALL |l| (|getShellEntry| $ 68)) |a|)
    ((SPADCALL (SPADCALL |l| (|getShellEntry| $ 69))
         (|getShellEntry| $ 68))
     (SPADCALL |a| (SPADCALL |l| (|getShellEntry| $ 70))
         (|getShellEntry| $ 64)))
    ('T (CONS (|OUTFORM;eform| 'SUPERSUB $) (CONS |a| |l|))))) 

(DEFUN |OUTFORM;supersub;$L$;47| (|a| |l| $)
  (SEQ (COND
         ((ODDP (SPADCALL |l| (|getShellEntry| $ 73)))
          (LETT |l|
                (SPADCALL |l| (LIST (SPADCALL (|getShellEntry| $ 13)))
                    (|getShellEntry| $ 56))
                |OUTFORM;supersub;$L$;47|)))
       (EXIT (CONS (|OUTFORM;eform| 'ALTSUPERSUB $) (CONS |a| |l|))))) 

(DEFUN |OUTFORM;hconcat;3$;48| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'CONCAT $) |a| |b|) $)) 

(DEFUN |OUTFORM;hconcat;L$;49| (|l| $)
  (CONS (|OUTFORM;eform| 'CONCAT $) |l|)) 

(DEFUN |OUTFORM;vconcat;3$;50| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'VCONCAT $) |a| |b|) $)) 

(DEFUN |OUTFORM;vconcat;L$;51| (|l| $)
  (CONS (|OUTFORM;eform| 'VCONCAT $) |l|)) 

(DEFUN |OUTFORM;~=;3$;52| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "~=" $) |a| |b|) $)) 

(DEFUN |OUTFORM;<;3$;53| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "<" $) |a| |b|) $)) 

(DEFUN |OUTFORM;>;3$;54| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| ">" $) |a| |b|) $)) 

(DEFUN |OUTFORM;<=;3$;55| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "<=" $) |a| |b|) $)) 

(DEFUN |OUTFORM;>=;3$;56| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| ">=" $) |a| |b|) $)) 

(DEFUN |OUTFORM;+;3$;57| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "+" $) |a| |b|) $)) 

(DEFUN |OUTFORM;-;3$;58| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "-" $) |a| |b|) $)) 

(DEFUN |OUTFORM;-;2$;59| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "-" $) |a|) $)) 

(DEFUN |OUTFORM;*;3$;60| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "*" $) |a| |b|) $)) 

(DEFUN |OUTFORM;/;3$;61| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "/" $) |a| |b|) $)) 

(DEFUN |OUTFORM;**;3$;62| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "**" $) |a| |b|) $)) 

(DEFUN |OUTFORM;div;3$;63| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "div" $) |a| |b|) $)) 

(DEFUN |OUTFORM;rem;3$;64| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "rem" $) |a| |b|) $)) 

(DEFUN |OUTFORM;quo;3$;65| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "quo" $) |a| |b|) $)) 

(DEFUN |OUTFORM;exquo;3$;66| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "exquo" $) |a| |b|) $)) 

(DEFUN |OUTFORM;and;3$;67| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "and" $) |a| |b|) $)) 

(DEFUN |OUTFORM;or;3$;68| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "or" $) |a| |b|) $)) 

(DEFUN |OUTFORM;not;2$;69| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;sform| "not" $) |a|) $)) 

(DEFUN |OUTFORM;SEGMENT;3$;70| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SEGMENT $) |a| |b|) $)) 

(DEFUN |OUTFORM;SEGMENT;2$;71| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SEGMENT $) |a|) $)) 

(DEFUN |OUTFORM;binomial;3$;72| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BINOMIAL $) |a| |b|) $)) 

(DEFUN |OUTFORM;empty;$;73| ($)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'NOTHING $)) $)) 

(DEFUN |OUTFORM;infix?;$B;74| (|a| $)
  (PROG (#0=#:G1499 |e|)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |e|
                            (COND
                              ((IDENTP |a|) |a|)
                              ((STRINGP |a|) (INTERN |a|))
                              ('T
                               (PROGN
                                 (LETT #0# 'NIL |OUTFORM;infix?;$B;74|)
                                 (GO #0#))))
                            |OUTFORM;infix?;$B;74|)
                      (EXIT (COND ((GET |e| 'INFIXOP) 'T) ('T 'NIL)))))
           #0# (EXIT #0#))))) 

(PUT '|OUTFORM;elt;$L$;75| '|SPADreplace| 'CONS) 

(DEFUN |OUTFORM;elt;$L$;75| (|a| |l| $) (CONS |a| |l|)) 

(DEFUN |OUTFORM;prefix;$L$;76| (|a| |l| $)
  (COND
    ((NULL (SPADCALL |a| (|getShellEntry| $ 98))) (CONS |a| |l|))
    ('T
     (SPADCALL |a|
         (SPADCALL (SPADCALL |l| (|getShellEntry| $ 52))
             (|getShellEntry| $ 62))
         (|getShellEntry| $ 38))))) 

(DEFUN |OUTFORM;infix;$L$;77| (|a| |l| $)
  (COND
    ((SPADCALL |l| (|getShellEntry| $ 68))
     (SPADCALL (|getShellEntry| $ 13)))
    ((SPADCALL (SPADCALL |l| (|getShellEntry| $ 69))
         (|getShellEntry| $ 68))
     (SPADCALL |l| (|getShellEntry| $ 70)))
    ((SPADCALL |a| (|getShellEntry| $ 98)) (CONS |a| |l|))
    ('T
     (SPADCALL
         (LIST (SPADCALL |l| (|getShellEntry| $ 70)) |a|
               (SPADCALL |a| (SPADCALL |l| (|getShellEntry| $ 69))
                   (|getShellEntry| $ 101)))
         (|getShellEntry| $ 75))))) 

(DEFUN |OUTFORM;infix;4$;78| (|a| |b| |c| $)
  (COND
    ((SPADCALL |a| (|getShellEntry| $ 98))
     (|OUTFORM;bless| (LIST |a| |b| |c|) $))
    ('T (SPADCALL (LIST |b| |a| |c|) (|getShellEntry| $ 75))))) 

(DEFUN |OUTFORM;postfix;3$;79| (|a| |b| $)
  (SPADCALL |b| |a| (|getShellEntry| $ 38))) 

(DEFUN |OUTFORM;string;2$;80| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'STRING $) |a|) $)) 

(DEFUN |OUTFORM;quote;2$;81| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'QUOTE $) |a|) $)) 

(DEFUN |OUTFORM;overbar;2$;82| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'OVERBAR $) |a|) $)) 

(DEFUN |OUTFORM;dot;2$;83| (|a| $)
  (SPADCALL |a| (|OUTFORM;sform| "." $) (|getShellEntry| $ 65))) 

(DEFUN |OUTFORM;prime;2$;84| (|a| $)
  (SPADCALL |a| (|OUTFORM;sform| "," $) (|getShellEntry| $ 65))) 

(DEFUN |OUTFORM;dot;$Nni$;85| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn|
                     (SPADCALL "." (|getShellEntry| $ 109)))
                 |OUTFORM;dot;$Nni$;85|)
           (EXIT (SPADCALL |a| (|OUTFORM;sform| |s| $)
                     (|getShellEntry| $ 65))))))) 

(DEFUN |OUTFORM;prime;$Nni$;86| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn|
                     (SPADCALL "," (|getShellEntry| $ 109)))
                 |OUTFORM;prime;$Nni$;86|)
           (EXIT (SPADCALL |a| (|OUTFORM;sform| |s| $)
                     (|getShellEntry| $ 65))))))) 

(DEFUN |OUTFORM;overlabel;3$;87| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'OVERLABEL $) |a| |b|) $)) 

(DEFUN |OUTFORM;box;2$;88| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'BOX $) |a|) $)) 

(DEFUN |OUTFORM;zag;3$;89| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'ZAG $) |a| |b|) $)) 

(DEFUN |OUTFORM;root;2$;90| (|a| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'ROOT $) |a|) $)) 

(DEFUN |OUTFORM;root;3$;91| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'ROOT $) |a| |b|) $)) 

(DEFUN |OUTFORM;over;3$;92| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'OVER $) |a| |b|) $)) 

(DEFUN |OUTFORM;slash;3$;93| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SLASH $) |a| |b|) $)) 

(DEFUN |OUTFORM;assign;3$;94| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'LET $) |a| |b|) $)) 

(DEFUN |OUTFORM;label;3$;95| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'EQUATNUM $) |a| |b|) $)) 

(DEFUN |OUTFORM;rarrow;3$;96| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'TAG $) |a| |b|) $)) 

(DEFUN |OUTFORM;differentiate;$Nni$;97| (|a| |nn| $)
  (PROG (#0=#:G1529 |r| |s|)
    (RETURN
      (SEQ (COND
             ((ZEROP |nn|) |a|)
             ((< |nn| 4) (SPADCALL |a| |nn| (|getShellEntry| $ 111)))
             ('T
              (SEQ (LETT |r|
                         (SPADCALL
                             (PROG1 (LETT #0# |nn|
                                     |OUTFORM;differentiate;$Nni$;97|)
                               (|check-subtype| (> #0# 0)
                                   '(|PositiveInteger|) #0#))
                             (|getShellEntry| $ 124))
                         |OUTFORM;differentiate;$Nni$;97|)
                   (LETT |s| (SPADCALL |r| (|getShellEntry| $ 125))
                         |OUTFORM;differentiate;$Nni$;97|)
                   (EXIT (SPADCALL |a|
                             (SPADCALL (|OUTFORM;sform| |s| $)
                                 (|getShellEntry| $ 62))
                             (|getShellEntry| $ 65)))))))))) 

(DEFUN |OUTFORM;sum;2$;98| (|a| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'SIGMA $)
            (SPADCALL (|getShellEntry| $ 13)) |a|)
      $)) 

(DEFUN |OUTFORM;sum;3$;99| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SIGMA $) |b| |a|) $)) 

(DEFUN |OUTFORM;sum;4$;100| (|a| |b| |c| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'SIGMA2 $) |b| |c| |a|) $)) 

(DEFUN |OUTFORM;prod;2$;101| (|a| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'PI $) (SPADCALL (|getShellEntry| $ 13))
            |a|)
      $)) 

(DEFUN |OUTFORM;prod;3$;102| (|a| |b| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'PI $) |b| |a|) $)) 

(DEFUN |OUTFORM;prod;4$;103| (|a| |b| |c| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'PI2 $) |b| |c| |a|) $)) 

(DEFUN |OUTFORM;int;2$;104| (|a| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'INTSIGN $)
            (SPADCALL (|getShellEntry| $ 13))
            (SPADCALL (|getShellEntry| $ 13)) |a|)
      $)) 

(DEFUN |OUTFORM;int;3$;105| (|a| |b| $)
  (|OUTFORM;bless|
      (LIST (|OUTFORM;eform| 'INTSIGN $) |b|
            (SPADCALL (|getShellEntry| $ 13)) |a|)
      $)) 

(DEFUN |OUTFORM;int;4$;106| (|a| |b| |c| $)
  (|OUTFORM;bless| (LIST (|OUTFORM;eform| 'INTSIGN $) |b| |c| |a|) $)) 

(DEFUN |OutputForm| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1543)
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
        (LETT $ (|newShell| 137) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|OutputForm| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 6 "~G")
        $)))) 

(MAKEPROP '|OutputForm| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL '|format| (|String|)
             |OUTFORM;doubleFloatFormat;2S;1| (|Void|)
             |OUTFORM;print;$V;6| (|Boolean|) (0 . |empty?|)
             |OUTFORM;empty;$;73| |OUTFORM;message;S$;7|
             |OUTFORM;messagePrint;SV;8| |OUTFORM;=;2$B;9|
             |OUTFORM;=;3$;10| (|OutputForm|) |OUTFORM;coerce;2$;11|
             (|Integer|) |OUTFORM;outputForm;I$;12| (|Symbol|)
             |OUTFORM;outputForm;S$;13| (|DoubleFloat|)
             |OUTFORM;outputForm;Df$;14| (|Character|) (5 . |quote|)
             (9 . |concat|) (15 . |concat|) |OUTFORM;outputForm;S$;15|
             |OUTFORM;width;$I;16| |OUTFORM;height;$I;17|
             |OUTFORM;subHeight;$I;18| |OUTFORM;superHeight;$I;19|
             |OUTFORM;height;I;20| |OUTFORM;width;I;21|
             |OUTFORM;hspace;I$;29| |OUTFORM;hconcat;3$;48|
             |OUTFORM;center;$I$;22| |OUTFORM;left;$I$;23|
             |OUTFORM;right;$I$;24| |OUTFORM;center;2$;25|
             |OUTFORM;left;2$;26| |OUTFORM;right;2$;27|
             |OUTFORM;vspace;I$;28| |OUTFORM;vconcat;3$;50|
             |OUTFORM;rspace;2I$;30| (|List| $) (|List| 48)
             |OUTFORM;matrix;L$;31| |OUTFORM;pile;L$;32|
             |OUTFORM;commaSeparate;L$;33|
             |OUTFORM;semicolonSeparate;L$;34| (|List| $$)
             (21 . |reverse|) (26 . |append|)
             |OUTFORM;blankSeparate;L$;35| |OUTFORM;brace;2$;36|
             |OUTFORM;brace;L$;37| |OUTFORM;bracket;2$;38|
             |OUTFORM;bracket;L$;39| |OUTFORM;paren;2$;40|
             |OUTFORM;paren;L$;41| |OUTFORM;sub;3$;42|
             |OUTFORM;super;3$;43| |OUTFORM;presub;3$;44|
             |OUTFORM;presuper;3$;45| (32 . |null|) (37 . |rest|)
             (42 . |first|) |OUTFORM;scripts;$L$;46|
             (|NonNegativeInteger|) (47 . |#|)
             |OUTFORM;supersub;$L$;47| |OUTFORM;hconcat;L$;49|
             |OUTFORM;vconcat;L$;51| |OUTFORM;~=;3$;52|
             |OUTFORM;<;3$;53| |OUTFORM;>;3$;54| |OUTFORM;<=;3$;55|
             |OUTFORM;>=;3$;56| |OUTFORM;+;3$;57| |OUTFORM;-;3$;58|
             |OUTFORM;-;2$;59| |OUTFORM;*;3$;60| |OUTFORM;/;3$;61|
             |OUTFORM;**;3$;62| |OUTFORM;div;3$;63| |OUTFORM;rem;3$;64|
             |OUTFORM;quo;3$;65| |OUTFORM;exquo;3$;66|
             |OUTFORM;and;3$;67| |OUTFORM;or;3$;68| |OUTFORM;not;2$;69|
             |OUTFORM;SEGMENT;3$;70| |OUTFORM;SEGMENT;2$;71|
             |OUTFORM;binomial;3$;72| |OUTFORM;infix?;$B;74|
             |OUTFORM;elt;$L$;75| |OUTFORM;prefix;$L$;76|
             |OUTFORM;infix;$L$;77| |OUTFORM;infix;4$;78|
             |OUTFORM;postfix;3$;79| |OUTFORM;string;2$;80|
             |OUTFORM;quote;2$;81| |OUTFORM;overbar;2$;82|
             |OUTFORM;dot;2$;83| |OUTFORM;prime;2$;84| (52 . |char|)
             |OUTFORM;dot;$Nni$;85| |OUTFORM;prime;$Nni$;86|
             |OUTFORM;overlabel;3$;87| |OUTFORM;box;2$;88|
             |OUTFORM;zag;3$;89| |OUTFORM;root;2$;90|
             |OUTFORM;root;3$;91| |OUTFORM;over;3$;92|
             |OUTFORM;slash;3$;93| |OUTFORM;assign;3$;94|
             |OUTFORM;label;3$;95| |OUTFORM;rarrow;3$;96|
             (|PositiveInteger|) (|NumberFormats|) (57 . |FormatRoman|)
             (62 . |lowerCase|) |OUTFORM;differentiate;$Nni$;97|
             |OUTFORM;sum;2$;98| |OUTFORM;sum;3$;99|
             |OUTFORM;sum;4$;100| |OUTFORM;prod;2$;101|
             |OUTFORM;prod;3$;102| |OUTFORM;prod;4$;103|
             |OUTFORM;int;2$;104| |OUTFORM;int;3$;105|
             |OUTFORM;int;4$;106| (|SingleInteger|))
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
             |doubleFloatFormat| 469 |dot| 474 |div| 485
             |differentiate| 491 |commaSeparate| 497 |coerce| 502
             |center| 507 |bracket| 518 |brace| 528 |box| 538
             |blankSeparate| 543 |binomial| 548 |assign| 554 |and| 560
             SEGMENT 566 >= 577 > 583 = 589 <= 601 < 607 / 613 - 619 +
             630 ** 636 * 642)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0))
                (CONS '#(|SetCategory&| |BasicType&| NIL)
                      (CONS '#((|SetCategory|) (|BasicType|)
                               (|CoercibleTo| 18))
                            (|makeByteWordVec2| 136
                                '(1 7 11 0 12 0 26 0 27 2 7 0 0 26 28 2
                                  7 0 26 0 29 1 54 0 0 55 2 54 0 0 0 56
                                  1 54 11 0 68 1 54 0 0 69 1 54 2 0 70
                                  1 54 72 0 73 1 26 0 7 109 1 123 7 122
                                  124 1 7 0 0 125 2 0 0 0 0 77 2 0 11 0
                                  0 1 2 0 0 0 0 114 0 0 20 36 1 0 20 0
                                  31 1 0 0 20 45 1 0 0 48 76 2 0 0 0 0
                                  46 2 0 0 0 48 74 1 0 20 0 34 2 0 0 0
                                  0 65 2 0 0 0 0 128 3 0 0 0 0 0 129 1
                                  0 0 0 127 1 0 20 0 33 2 0 0 0 0 64 1
                                  0 0 0 104 2 0 0 0 0 118 1 0 0 48 53 2
                                  0 0 0 48 71 2 0 0 20 20 47 1 0 0 0
                                  115 2 0 0 0 0 116 1 0 0 0 44 2 0 0 0
                                  20 41 2 0 0 0 0 89 2 0 0 0 0 121 1 0
                                  0 0 105 2 0 0 0 0 90 3 0 0 0 0 0 132
                                  1 0 0 0 130 2 0 0 0 0 131 1 0 9 0 10
                                  2 0 0 0 72 111 1 0 0 0 108 2 0 0 0 0
                                  67 2 0 0 0 0 66 2 0 0 0 48 100 2 0 0
                                  0 0 103 1 0 0 48 51 1 0 0 48 63 1 0 0
                                  0 62 2 0 0 0 0 112 1 0 0 0 106 2 0 0
                                  0 0 117 1 0 0 24 25 1 0 0 22 23 1 0 0
                                  7 30 1 0 0 20 21 2 0 0 0 0 93 1 0 0 0
                                  94 1 0 9 7 15 1 0 0 7 14 1 0 0 49 50
                                  1 0 0 0 43 2 0 0 0 20 40 1 0 7 0 1 2
                                  0 0 0 0 120 3 0 0 0 0 0 135 2 0 0 0 0
                                  134 1 0 0 0 133 1 0 11 0 98 2 0 0 0
                                  48 101 3 0 0 0 0 0 102 1 0 0 20 37 0
                                  0 20 35 1 0 20 0 32 1 0 0 48 75 2 0 0
                                  0 0 38 1 0 136 0 1 2 0 0 0 0 91 0 0 0
                                  13 2 0 0 0 48 99 1 0 7 7 8 2 0 0 0 72
                                  110 1 0 0 0 107 2 0 0 0 0 88 2 0 0 0
                                  72 126 1 0 0 48 52 1 0 18 0 19 1 0 0
                                  0 42 2 0 0 0 20 39 1 0 0 0 60 1 0 0
                                  48 61 1 0 0 48 59 1 0 0 0 58 1 0 0 0
                                  113 1 0 0 48 57 2 0 0 0 0 97 2 0 0 0
                                  0 119 2 0 0 0 0 92 1 0 0 0 96 2 0 0 0
                                  0 95 2 0 0 0 0 81 2 0 0 0 0 79 2 0 0
                                  0 0 17 2 0 11 0 0 16 2 0 0 0 0 80 2 0
                                  0 0 0 78 2 0 0 0 0 86 1 0 0 0 84 2 0
                                  0 0 0 83 2 0 0 0 0 82 2 0 0 0 0 87 2
                                  0 0 0 0 85)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|OutputForm| '|isFunctor|
             '(((SEGMENT ($ $)) T (ELT $ 96))
               ((SEGMENT ($ $ $)) T (ELT $ 95))
               ((|not| ($ $)) T (ELT $ 94))
               ((|or| ($ $ $)) T (ELT $ 93))
               ((|and| ($ $ $)) T (ELT $ 92))
               ((|exquo| ($ $ $)) T (ELT $ 91))
               ((|quo| ($ $ $)) T (ELT $ 90))
               ((|rem| ($ $ $)) T (ELT $ 89))
               ((|div| ($ $ $)) T (ELT $ 88))
               ((** ($ $ $)) T (ELT $ 87)) ((/ ($ $ $)) T (ELT $ 86))
               ((* ($ $ $)) T (ELT $ 85)) ((- ($ $)) T (ELT $ 84))
               ((- ($ $ $)) T (ELT $ 83)) ((+ ($ $ $)) T (ELT $ 82))
               ((>= ($ $ $)) T (ELT $ 81)) ((<= ($ $ $)) T (ELT $ 80))
               ((> ($ $ $)) T (ELT $ 79)) ((< ($ $ $)) T (ELT $ 78))
               ((~= ($ $ $)) T (ELT $ 77)) ((= ($ $ $)) T (ELT $ 17))
               ((|blankSeparate| ($ (|List| $))) T (ELT $ 57))
               ((|semicolonSeparate| ($ (|List| $))) T (ELT $ 53))
               ((|commaSeparate| ($ (|List| $))) T (ELT $ 52))
               ((|pile| ($ (|List| $))) T (ELT $ 51))
               ((|paren| ($ (|List| $))) T (ELT $ 63))
               ((|paren| ($ $)) T (ELT $ 62))
               ((|bracket| ($ (|List| $))) T (ELT $ 61))
               ((|bracket| ($ $)) T (ELT $ 60))
               ((|brace| ($ (|List| $))) T (ELT $ 59))
               ((|brace| ($ $)) T (ELT $ 58))
               ((|int| ($ $ $ $)) T (ELT $ 135))
               ((|int| ($ $ $)) T (ELT $ 134))
               ((|int| ($ $)) T (ELT $ 133))
               ((|prod| ($ $ $ $)) T (ELT $ 132))
               ((|prod| ($ $ $)) T (ELT $ 131))
               ((|prod| ($ $)) T (ELT $ 130))
               ((|sum| ($ $ $ $)) T (ELT $ 129))
               ((|sum| ($ $ $)) T (ELT $ 128))
               ((|sum| ($ $)) T (ELT $ 127))
               ((|overlabel| ($ $ $)) T (ELT $ 112))
               ((|overbar| ($ $)) T (ELT $ 106))
               ((|prime| ($ $ (|NonNegativeInteger|))) T (ELT $ 111))
               ((|prime| ($ $)) T (ELT $ 108))
               ((|dot| ($ $ (|NonNegativeInteger|))) T (ELT $ 110))
               ((|dot| ($ $)) T (ELT $ 107))
               ((|quote| ($ $)) T (ELT $ 105))
               ((|supersub| ($ $ (|List| $))) T (ELT $ 74))
               ((|scripts| ($ $ (|List| $))) T (ELT $ 71))
               ((|presuper| ($ $ $)) T (ELT $ 67))
               ((|presub| ($ $ $)) T (ELT $ 66))
               ((|super| ($ $ $)) T (ELT $ 65))
               ((|sub| ($ $ $)) T (ELT $ 64))
               ((|binomial| ($ $ $)) T (ELT $ 97))
               ((|differentiate| ($ $ (|NonNegativeInteger|))) T
                (ELT $ 126))
               ((|rarrow| ($ $ $)) T (ELT $ 121))
               ((|assign| ($ $ $)) T (ELT $ 119))
               ((|slash| ($ $ $)) T (ELT $ 118))
               ((|over| ($ $ $)) T (ELT $ 117))
               ((|root| ($ $ $)) T (ELT $ 116))
               ((|root| ($ $)) T (ELT $ 115))
               ((|zag| ($ $ $)) T (ELT $ 114))
               ((|matrix| ($ (|List| (|List| $)))) T (ELT $ 50))
               ((|box| ($ $)) T (ELT $ 113))
               ((|label| ($ $ $)) T (ELT $ 120))
               ((|string| ($ $)) T (ELT $ 104))
               ((|elt| ($ $ (|List| $))) T (ELT $ 99))
               ((|infix?| ((|Boolean|) $)) T (ELT $ 98))
               ((|postfix| ($ $ $)) T (ELT $ 103))
               ((|infix| ($ $ $ $)) T (ELT $ 102))
               ((|infix| ($ $ (|List| $))) T (ELT $ 101))
               ((|prefix| ($ $ (|List| $))) T (ELT $ 100))
               ((|vconcat| ($ (|List| $))) T (ELT $ 76))
               ((|hconcat| ($ (|List| $))) T (ELT $ 75))
               ((|vconcat| ($ $ $)) T (ELT $ 46))
               ((|hconcat| ($ $ $)) T (ELT $ 38))
               ((|center| ($ $)) T (ELT $ 42))
               ((|right| ($ $)) T (ELT $ 44))
               ((|left| ($ $)) T (ELT $ 43))
               ((|center| ($ $ (|Integer|))) T (ELT $ 39))
               ((|right| ($ $ (|Integer|))) T (ELT $ 41))
               ((|left| ($ $ (|Integer|))) T (ELT $ 40))
               ((|rspace| ($ (|Integer|) (|Integer|))) T (ELT $ 47))
               ((|vspace| ($ (|Integer|))) T (ELT $ 45))
               ((|hspace| ($ (|Integer|))) T (ELT $ 37))
               ((|superHeight| ((|Integer|) $)) T (ELT $ 34))
               ((|subHeight| ((|Integer|) $)) T (ELT $ 33))
               ((|height| ((|Integer|))) T (ELT $ 35))
               ((|width| ((|Integer|))) T (ELT $ 36))
               ((|height| ((|Integer|) $)) T (ELT $ 32))
               ((|width| ((|Integer|) $)) T (ELT $ 31))
               ((|doubleFloatFormat| ((|String|) (|String|))) T
                (ELT $ 8))
               ((|empty| ($)) T (ELT $ 13))
               ((|outputForm| ($ (|DoubleFloat|))) T (ELT $ 25))
               ((|outputForm| ($ (|String|))) T (ELT $ 30))
               ((|outputForm| ($ (|Symbol|))) T (ELT $ 23))
               ((|outputForm| ($ (|Integer|))) T (ELT $ 21))
               ((|messagePrint| ((|Void|) (|String|))) T (ELT $ 15))
               ((|message| ($ (|String|))) T (ELT $ 14))
               ((|print| ((|Void|) $)) T (ELT $ 10))
               ((|latex| ((|String|) $)) T (ELT $ NIL))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 19))
               ((= ((|Boolean|) $ $)) T (ELT $ 16))
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
                               (SIGNATURE |doubleFloatFormat|
                                   ((|String|) (|String|)))
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
                                      (SIGNATURE |doubleFloatFormat|
                                       ((|String|) (|String|)))
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
