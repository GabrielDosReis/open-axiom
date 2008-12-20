
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%String|)
                |OUTFORM;doubleFloatFormat;2S;1|)) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Thing|)
                |OUTFORM;sform|)) 

(PUT '|OUTFORM;sform| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;eform|)) 

(PUT '|OUTFORM;eform| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |OUTFORM;iform|)) 

(PUT '|OUTFORM;iform| '|SPADreplace| '(XLAM (|i|) |i|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|) |OUTFORM;bless|)) 

(PUT '|OUTFORM;bless| '|SPADreplace| '(XLAM (|x|) |x|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Void|)
                |OUTFORM;print;$V;6|)) 

(PUT '|OUTFORM;print;$V;6| '|SPADreplace| '|mathprint|) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Thing|)
                |OUTFORM;message;S$;7|)) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Void|)
                |OUTFORM;messagePrint;SV;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |OUTFORM;=;2$B;9|)) 

(PUT '|OUTFORM;=;2$B;9| '|SPADreplace| 'EQUAL) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;=;3$;10|)) 

(PUT '|OUTFORM;=;3$;10| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "=" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;coerce;2$;11|)) 

(PUT '|OUTFORM;coerce;2$;11| '|SPADreplace| '(XLAM (|a|) |a|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |OUTFORM;outputForm;I$;12|)) 

(PUT '|OUTFORM;outputForm;I$;12| '|SPADreplace| '(XLAM (|n|) |n|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;outputForm;S$;13|)) 

(PUT '|OUTFORM;outputForm;S$;13| '|SPADreplace| '(XLAM (|e|) |e|)) 

(DECLAIM (FTYPE (FUNCTION (|%DoubleFloat| |%Shell|) |%Thing|)
                |OUTFORM;outputForm;Df$;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Thing|)
                |OUTFORM;outputForm;S$;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |OUTFORM;width;$I;16|)) 

(PUT '|OUTFORM;width;$I;16| '|SPADreplace| '|outformWidth|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |OUTFORM;height;$I;17|)) 

(PUT '|OUTFORM;height;$I;17| '|SPADreplace| '|height|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |OUTFORM;subHeight;$I;18|)) 

(PUT '|OUTFORM;subHeight;$I;18| '|SPADreplace| '|subspan|) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Integer|)
                |OUTFORM;superHeight;$I;19|)) 

(PUT '|OUTFORM;superHeight;$I;19| '|SPADreplace| '|superspan|) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |OUTFORM;height;I;20|)) 

(PUT '|OUTFORM;height;I;20| '|SPADreplace| '(XLAM NIL 20)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Integer|) |OUTFORM;width;I;21|)) 

(PUT '|OUTFORM;width;I;21| '|SPADreplace| '(XLAM NIL 66)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |OUTFORM;center;$I$;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |OUTFORM;left;$I$;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Integer| |%Shell|) |%Thing|)
                |OUTFORM;right;$I$;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;center;2$;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;left;2$;26|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;right;2$;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |OUTFORM;vspace;I$;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%Thing|)
                |OUTFORM;hspace;I$;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Integer| |%Shell|) |%Thing|)
                |OUTFORM;rspace;2I$;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;matrix;L$;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;pile;L$;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;commaSeparate;L$;33|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;semicolonSeparate;L$;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;blankSeparate;L$;35|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;brace;2$;36|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;brace;L$;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;bracket;2$;38|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;bracket;L$;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;paren;2$;40|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;paren;L$;41|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;sub;3$;42|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;super;3$;43|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;presub;3$;44|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;presuper;3$;45|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;scripts;$L$;46|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;supersub;$L$;47|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;hconcat;3$;48|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;hconcat;L$;49|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;vconcat;3$;50|)) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;vconcat;L$;51|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;~=;3$;52|)) 

(PUT '|OUTFORM;~=;3$;52| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "~=" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;<;3$;53|)) 

(PUT '|OUTFORM;<;3$;53| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "<" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;>;3$;54|)) 

(PUT '|OUTFORM;>;3$;54| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST ">" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;<=;3$;55|)) 

(PUT '|OUTFORM;<=;3$;55| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "<=" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;>=;3$;56|)) 

(PUT '|OUTFORM;>=;3$;56| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST ">=" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;+;3$;57|)) 

(PUT '|OUTFORM;+;3$;57| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "+" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;-;3$;58|)) 

(PUT '|OUTFORM;-;3$;58| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "-" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;-;2$;59|)) 

(PUT '|OUTFORM;-;2$;59| '|SPADreplace| '(XLAM (|a|) (LIST "-" |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;*;3$;60|)) 

(PUT '|OUTFORM;*;3$;60| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "*" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;/;3$;61|)) 

(PUT '|OUTFORM;/;3$;61| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "/" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;**;3$;62|)) 

(PUT '|OUTFORM;**;3$;62| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "**" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;div;3$;63|)) 

(PUT '|OUTFORM;div;3$;63| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "div" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;rem;3$;64|)) 

(PUT '|OUTFORM;rem;3$;64| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "rem" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;quo;3$;65|)) 

(PUT '|OUTFORM;quo;3$;65| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "quo" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;exquo;3$;66|)) 

(PUT '|OUTFORM;exquo;3$;66| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "exquo" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;and;3$;67|)) 

(PUT '|OUTFORM;and;3$;67| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "and" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;or;3$;68|)) 

(PUT '|OUTFORM;or;3$;68| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST "or" |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;not;2$;69|)) 

(PUT '|OUTFORM;not;2$;69| '|SPADreplace|
     '(XLAM (|a|) (LIST "not" |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;SEGMENT;3$;70|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;SEGMENT;2$;71|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;binomial;3$;72|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |OUTFORM;empty;$;73|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |OUTFORM;infix?;$B;74|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;elt;$L$;75|)) 

(PUT '|OUTFORM;elt;$L$;75| '|SPADreplace| 'CONS) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;prefix;$L$;76|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;infix;$L$;77|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;infix;4$;78|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;postfix;3$;79|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;string;2$;80|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;quote;2$;81|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;overbar;2$;82|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;dot;2$;83|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;prime;2$;84|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |OUTFORM;dot;$Nni$;85|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |OUTFORM;prime;$Nni$;86|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;overlabel;3$;87|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;box;2$;88|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;zag;3$;89|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;root;2$;90|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;root;3$;91|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;over;3$;92|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;slash;3$;93|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;assign;3$;94|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;label;3$;95|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;rarrow;3$;96|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |OUTFORM;differentiate;$Nni$;97|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;sum;2$;98|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;sum;3$;99|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;sum;4$;100|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;prod;2$;101|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;prod;3$;102|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;prod;4$;103|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;int;2$;104|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;int;3$;105|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;int;4$;106|)) 

(DEFUN |OUTFORM;doubleFloatFormat;2S;1| (|s| $)
  (PROG (|ss|)
    (RETURN
      (SEQ (LETT |ss| (|getShellEntry| $ 6)
                 |OUTFORM;doubleFloatFormat;2S;1|)
           (|setShellEntry| $ 6 |s|) (EXIT |ss|))))) 

(DEFUN |OUTFORM;sform| (|s| $) (DECLARE (IGNORE $)) |s|) 

(DEFUN |OUTFORM;eform| (|e| $) (DECLARE (IGNORE $)) |e|) 

(DEFUN |OUTFORM;iform| (|i| $) (DECLARE (IGNORE $)) |i|) 

(DEFUN |OUTFORM;bless| (|x| $) (DECLARE (IGNORE $)) |x|) 

(DEFUN |OUTFORM;print;$V;6| (|x| $)
  (DECLARE (IGNORE $))
  (|mathprint| |x|)) 

(DEFUN |OUTFORM;message;S$;7| (|s| $)
  (COND
    ((SPADCALL |s| (|getShellEntry| $ 12)) (|OUTFORM;empty;$;73| $))
    ('T |s|))) 

(DEFUN |OUTFORM;messagePrint;SV;8| (|s| $)
  (|mathprint| (|OUTFORM;message;S$;7| |s| $))) 

(DEFUN |OUTFORM;=;2$B;9| (|a| |b| $)
  (DECLARE (IGNORE $))
  (EQUAL |a| |b|)) 

(DEFUN |OUTFORM;=;3$;10| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "=" |a| |b|)) 

(DEFUN |OUTFORM;coerce;2$;11| (|a| $) (DECLARE (IGNORE $)) |a|) 

(DEFUN |OUTFORM;outputForm;I$;12| (|n| $) (DECLARE (IGNORE $)) |n|) 

(DEFUN |OUTFORM;outputForm;S$;13| (|e| $) (DECLARE (IGNORE $)) |e|) 

(DEFUN |OUTFORM;outputForm;Df$;14| (|f| $)
  (FORMAT NIL (|getShellEntry| $ 6) |f|)) 

(DEFUN |OUTFORM;outputForm;S$;15| (|s| $)
  (SPADCALL (SPADCALL (|getShellEntry| $ 27))
      (SPADCALL |s| (SPADCALL (|getShellEntry| $ 27))
                (|getShellEntry| $ 28))
      (|getShellEntry| $ 29))) 

(DEFUN |OUTFORM;width;$I;16| (|a| $)
  (DECLARE (IGNORE $))
  (|outformWidth| |a|)) 

(DEFUN |OUTFORM;height;$I;17| (|a| $)
  (DECLARE (IGNORE $))
  (|height| |a|)) 

(DEFUN |OUTFORM;subHeight;$I;18| (|a| $)
  (DECLARE (IGNORE $))
  (|subspan| |a|)) 

(DEFUN |OUTFORM;superHeight;$I;19| (|a| $)
  (DECLARE (IGNORE $))
  (|superspan| |a|)) 

(DEFUN |OUTFORM;height;I;20| ($) (DECLARE (IGNORE $)) 20) 

(DEFUN |OUTFORM;width;I;21| ($) (DECLARE (IGNORE $)) 66) 

(DEFUN |OUTFORM;center;$I$;22| (|a| |w| $)
  (|OUTFORM;hconcat;3$;48|
      (|OUTFORM;hspace;I$;29|
          (QUOTIENT2 (- |w| (|outformWidth| |a|)) 2) $)
      |a| $)) 

(DEFUN |OUTFORM;left;$I$;23| (|a| |w| $)
  (|OUTFORM;hconcat;3$;48| |a|
      (|OUTFORM;hspace;I$;29| (- |w| (|outformWidth| |a|)) $) $)) 

(DEFUN |OUTFORM;right;$I$;24| (|a| |w| $)
  (|OUTFORM;hconcat;3$;48|
      (|OUTFORM;hspace;I$;29| (- |w| (|outformWidth| |a|)) $) |a| $)) 

(DEFUN |OUTFORM;center;2$;25| (|a| $)
  (|OUTFORM;center;$I$;22| |a| 66 $)) 

(DEFUN |OUTFORM;left;2$;26| (|a| $) (|OUTFORM;left;$I$;23| |a| 66 $)) 

(DEFUN |OUTFORM;right;2$;27| (|a| $)
  (|OUTFORM;right;$I$;24| |a| 66 $)) 

(DEFUN |OUTFORM;vspace;I$;28| (|n| $)
  (COND
    ((EQL |n| 0) (|OUTFORM;empty;$;73| $))
    ('T
     (|OUTFORM;vconcat;3$;50| " " (|OUTFORM;vspace;I$;28| (- |n| 1) $)
         $)))) 

(DEFUN |OUTFORM;hspace;I$;29| (|n| $)
  (COND
    ((EQL |n| 0) (|OUTFORM;empty;$;73| $))
    ('T (|fillerSpaces| |n|)))) 

(DEFUN |OUTFORM;rspace;2I$;30| (|n| |m| $)
  (COND
    ((OR (EQL |n| 0) (EQL |m| 0)) (|OUTFORM;empty;$;73| $))
    ('T
     (|OUTFORM;vconcat;3$;50| (|OUTFORM;hspace;I$;29| |n| $)
         (|OUTFORM;rspace;2I$;30| |n| (- |m| 1) $) $)))) 

(DEFUN |OUTFORM;matrix;L$;31| (|ll| $)
  (PROG (#0=#:G1539 |l| #1=#:G1540 |lv|)
    (RETURN
      (SEQ (LETT |lv|
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
                        (SEQ (EXIT (LETT #0# (CONS (LIST2VEC |l|) #0#)
                                    |OUTFORM;matrix;L$;31|)))
                        (LETT #1# (CDR #1#) |OUTFORM;matrix;L$;31|)
                        (GO G190) G191 (EXIT (NREVERSE0 #0#))))
                 |OUTFORM;matrix;L$;31|)
           (EXIT (CONS 'MATRIX (LIST2VEC |lv|))))))) 

(DEFUN |OUTFORM;pile;L$;32| (|l| $) (CONS 'SC |l|)) 

(DEFUN |OUTFORM;commaSeparate;L$;33| (|l| $) (CONS 'AGGLST |l|)) 

(DEFUN |OUTFORM;semicolonSeparate;L$;34| (|l| $) (CONS 'AGGSET |l|)) 

(DEFUN |OUTFORM;blankSeparate;L$;35| (|l| $)
  (PROG (|c| |u| #0=#:G1541 |l1|)
    (RETURN
      (SEQ (LETT |c| 'CONCATB |OUTFORM;blankSeparate;L$;35|)
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

(DEFUN |OUTFORM;brace;2$;36| (|a| $) (LIST 'BRACE |a|)) 

(DEFUN |OUTFORM;brace;L$;37| (|l| $)
  (|OUTFORM;brace;2$;36| (|OUTFORM;commaSeparate;L$;33| |l| $) $)) 

(DEFUN |OUTFORM;bracket;2$;38| (|a| $) (LIST 'BRACKET |a|)) 

(DEFUN |OUTFORM;bracket;L$;39| (|l| $)
  (|OUTFORM;bracket;2$;38| (|OUTFORM;commaSeparate;L$;33| |l| $) $)) 

(DEFUN |OUTFORM;paren;2$;40| (|a| $) (LIST 'PAREN |a|)) 

(DEFUN |OUTFORM;paren;L$;41| (|l| $)
  (|OUTFORM;paren;2$;40| (|OUTFORM;commaSeparate;L$;33| |l| $) $)) 

(DEFUN |OUTFORM;sub;3$;42| (|a| |b| $) (LIST 'SUB |a| |b|)) 

(DEFUN |OUTFORM;super;3$;43| (|a| |b| $) (LIST 'SUPERSUB |a| " " |b|)) 

(DEFUN |OUTFORM;presub;3$;44| (|a| |b| $)
  (LIST 'SUPERSUB |a| " " " " " " |b|)) 

(DEFUN |OUTFORM;presuper;3$;45| (|a| |b| $)
  (LIST 'SUPERSUB |a| " " " " |b|)) 

(DEFUN |OUTFORM;scripts;$L$;46| (|a| |l| $)
  (COND
    ((SPADCALL |l| (|getShellEntry| $ 68)) |a|)
    ((SPADCALL (SPADCALL |l| (|getShellEntry| $ 69))
         (|getShellEntry| $ 68))
     (|OUTFORM;sub;3$;42| |a| (SPADCALL |l| (|getShellEntry| $ 70)) $))
    ('T (CONS 'SUPERSUB (CONS |a| |l|))))) 

(DEFUN |OUTFORM;supersub;$L$;47| (|a| |l| $)
  (SEQ (COND
         ((ODDP (SPADCALL |l| (|getShellEntry| $ 73)))
          (LETT |l|
                (SPADCALL |l| (LIST (|OUTFORM;empty;$;73| $))
                    (|getShellEntry| $ 56))
                |OUTFORM;supersub;$L$;47|)))
       (EXIT (CONS 'ALTSUPERSUB (CONS |a| |l|))))) 

(DEFUN |OUTFORM;hconcat;3$;48| (|a| |b| $) (LIST 'CONCAT |a| |b|)) 

(DEFUN |OUTFORM;hconcat;L$;49| (|l| $) (CONS 'CONCAT |l|)) 

(DEFUN |OUTFORM;vconcat;3$;50| (|a| |b| $) (LIST 'VCONCAT |a| |b|)) 

(DEFUN |OUTFORM;vconcat;L$;51| (|l| $) (CONS 'VCONCAT |l|)) 

(DEFUN |OUTFORM;~=;3$;52| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "~=" |a| |b|)) 

(DEFUN |OUTFORM;<;3$;53| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "<" |a| |b|)) 

(DEFUN |OUTFORM;>;3$;54| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST ">" |a| |b|)) 

(DEFUN |OUTFORM;<=;3$;55| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "<=" |a| |b|)) 

(DEFUN |OUTFORM;>=;3$;56| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST ">=" |a| |b|)) 

(DEFUN |OUTFORM;+;3$;57| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "+" |a| |b|)) 

(DEFUN |OUTFORM;-;3$;58| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "-" |a| |b|)) 

(DEFUN |OUTFORM;-;2$;59| (|a| $) (DECLARE (IGNORE $)) (LIST "-" |a|)) 

(DEFUN |OUTFORM;*;3$;60| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "*" |a| |b|)) 

(DEFUN |OUTFORM;/;3$;61| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "/" |a| |b|)) 

(DEFUN |OUTFORM;**;3$;62| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "**" |a| |b|)) 

(DEFUN |OUTFORM;div;3$;63| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "div" |a| |b|)) 

(DEFUN |OUTFORM;rem;3$;64| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "rem" |a| |b|)) 

(DEFUN |OUTFORM;quo;3$;65| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "quo" |a| |b|)) 

(DEFUN |OUTFORM;exquo;3$;66| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "exquo" |a| |b|)) 

(DEFUN |OUTFORM;and;3$;67| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "and" |a| |b|)) 

(DEFUN |OUTFORM;or;3$;68| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST "or" |a| |b|)) 

(DEFUN |OUTFORM;not;2$;69| (|a| $)
  (DECLARE (IGNORE $))
  (LIST "not" |a|)) 

(DEFUN |OUTFORM;SEGMENT;3$;70| (|a| |b| $) (LIST 'SEGMENT |a| |b|)) 

(DEFUN |OUTFORM;SEGMENT;2$;71| (|a| $) (LIST 'SEGMENT |a|)) 

(DEFUN |OUTFORM;binomial;3$;72| (|a| |b| $) (LIST 'BINOMIAL |a| |b|)) 

(DEFUN |OUTFORM;empty;$;73| ($) (LIST 'NOTHING)) 

(DEFUN |OUTFORM;infix?;$B;74| (|a| $)
  (PROG (#0=#:G1496 |e|)
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

(DEFUN |OUTFORM;elt;$L$;75| (|a| |l| $)
  (DECLARE (IGNORE $))
  (CONS |a| |l|)) 

(DEFUN |OUTFORM;prefix;$L$;76| (|a| |l| $)
  (COND
    ((NULL (|OUTFORM;infix?;$B;74| |a| $)) (CONS |a| |l|))
    ('T
     (|OUTFORM;hconcat;3$;48| |a|
         (|OUTFORM;paren;2$;40| (|OUTFORM;commaSeparate;L$;33| |l| $)
             $)
         $)))) 

(DEFUN |OUTFORM;infix;$L$;77| (|a| |l| $)
  (COND
    ((SPADCALL |l| (|getShellEntry| $ 68)) (|OUTFORM;empty;$;73| $))
    ((SPADCALL (SPADCALL |l| (|getShellEntry| $ 69))
         (|getShellEntry| $ 68))
     (SPADCALL |l| (|getShellEntry| $ 70)))
    ((|OUTFORM;infix?;$B;74| |a| $) (CONS |a| |l|))
    ('T
     (|OUTFORM;hconcat;L$;49|
         (LIST (SPADCALL |l| (|getShellEntry| $ 70)) |a|
               (|OUTFORM;infix;$L$;77| |a|
                   (SPADCALL |l| (|getShellEntry| $ 69)) $))
         $)))) 

(DEFUN |OUTFORM;infix;4$;78| (|a| |b| |c| $)
  (COND
    ((|OUTFORM;infix?;$B;74| |a| $) (LIST |a| |b| |c|))
    ('T (|OUTFORM;hconcat;L$;49| (LIST |b| |a| |c|) $)))) 

(DEFUN |OUTFORM;postfix;3$;79| (|a| |b| $)
  (|OUTFORM;hconcat;3$;48| |b| |a| $)) 

(DEFUN |OUTFORM;string;2$;80| (|a| $) (LIST 'STRING |a|)) 

(DEFUN |OUTFORM;quote;2$;81| (|a| $) (LIST 'QUOTE |a|)) 

(DEFUN |OUTFORM;overbar;2$;82| (|a| $) (LIST 'OVERBAR |a|)) 

(DEFUN |OUTFORM;dot;2$;83| (|a| $) (|OUTFORM;super;3$;43| |a| "." $)) 

(DEFUN |OUTFORM;prime;2$;84| (|a| $)
  (|OUTFORM;super;3$;43| |a| "," $)) 

(DEFUN |OUTFORM;dot;$Nni$;85| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn|
                     (SPADCALL "." (|getShellEntry| $ 109)))
                 |OUTFORM;dot;$Nni$;85|)
           (EXIT (|OUTFORM;super;3$;43| |a| |s| $)))))) 

(DEFUN |OUTFORM;prime;$Nni$;86| (|a| |nn| $)
  (PROG (|s|)
    (RETURN
      (SEQ (LETT |s|
                 (MAKE-FULL-CVEC |nn|
                     (SPADCALL "," (|getShellEntry| $ 109)))
                 |OUTFORM;prime;$Nni$;86|)
           (EXIT (|OUTFORM;super;3$;43| |a| |s| $)))))) 

(DEFUN |OUTFORM;overlabel;3$;87| (|a| |b| $)
  (LIST 'OVERLABEL |a| |b|)) 

(DEFUN |OUTFORM;box;2$;88| (|a| $) (LIST 'BOX |a|)) 

(DEFUN |OUTFORM;zag;3$;89| (|a| |b| $) (LIST 'ZAG |a| |b|)) 

(DEFUN |OUTFORM;root;2$;90| (|a| $) (LIST 'ROOT |a|)) 

(DEFUN |OUTFORM;root;3$;91| (|a| |b| $) (LIST 'ROOT |a| |b|)) 

(DEFUN |OUTFORM;over;3$;92| (|a| |b| $) (LIST 'OVER |a| |b|)) 

(DEFUN |OUTFORM;slash;3$;93| (|a| |b| $) (LIST 'SLASH |a| |b|)) 

(DEFUN |OUTFORM;assign;3$;94| (|a| |b| $) (LIST 'LET |a| |b|)) 

(DEFUN |OUTFORM;label;3$;95| (|a| |b| $) (LIST 'EQUATNUM |a| |b|)) 

(DEFUN |OUTFORM;rarrow;3$;96| (|a| |b| $) (LIST 'TAG |a| |b|)) 

(DEFUN |OUTFORM;differentiate;$Nni$;97| (|a| |nn| $)
  (PROG (#0=#:G1526 |r| |s|)
    (RETURN
      (SEQ (COND
             ((ZEROP |nn|) |a|)
             ((< |nn| 4) (|OUTFORM;prime;$Nni$;86| |a| |nn| $))
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
                   (EXIT (|OUTFORM;super;3$;43| |a|
                             (|OUTFORM;paren;2$;40| |s| $) $))))))))) 

(DEFUN |OUTFORM;sum;2$;98| (|a| $)
  (LIST 'SIGMA (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;sum;3$;99| (|a| |b| $) (LIST 'SIGMA |b| |a|)) 

(DEFUN |OUTFORM;sum;4$;100| (|a| |b| |c| $)
  (LIST 'SIGMA2 |b| |c| |a|)) 

(DEFUN |OUTFORM;prod;2$;101| (|a| $)
  (LIST 'PI (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;prod;3$;102| (|a| |b| $) (LIST 'PI |b| |a|)) 

(DEFUN |OUTFORM;prod;4$;103| (|a| |b| |c| $) (LIST 'PI2 |b| |c| |a|)) 

(DEFUN |OUTFORM;int;2$;104| (|a| $)
  (LIST 'INTSIGN (|OUTFORM;empty;$;73| $) (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;int;3$;105| (|a| |b| $)
  (LIST 'INTSIGN |b| (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;int;4$;106| (|a| |b| |c| $)
  (LIST 'INTSIGN |b| |c| |a|)) 

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

(MAKEPROP '|OutputForm| 'NILADIC T) 
