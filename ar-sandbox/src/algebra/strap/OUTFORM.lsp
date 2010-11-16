
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
     '(XLAM (|a| |b|) (LIST '= |a| |b|))) 

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

(PUT '|OUTFORM;outputForm;Df$;14| '|SPADreplace|
     'DFLOAT-FORMAT-GENERAL) 

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

(PUT '|OUTFORM;pile;L$;32| '|SPADreplace| '(XLAM (|l|) (CONS 'SC |l|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;commaSeparate;L$;33|)) 

(PUT '|OUTFORM;commaSeparate;L$;33| '|SPADreplace|
     '(XLAM (|l|) (CONS 'AGGLST |l|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;semicolonSeparate;L$;34|)) 

(PUT '|OUTFORM;semicolonSeparate;L$;34| '|SPADreplace|
     '(XLAM (|l|) (CONS 'AGGSET |l|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;blankSeparate;L$;35|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;brace;2$;36|)) 

(PUT '|OUTFORM;brace;2$;36| '|SPADreplace|
     '(XLAM (|a|) (LIST 'BRACE |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;brace;L$;37|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;bracket;2$;38|)) 

(PUT '|OUTFORM;bracket;2$;38| '|SPADreplace|
     '(XLAM (|a|) (LIST 'BRACKET |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;bracket;L$;39|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;paren;2$;40|)) 

(PUT '|OUTFORM;paren;2$;40| '|SPADreplace|
     '(XLAM (|a|) (LIST 'PAREN |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;paren;L$;41|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;sub;3$;42|)) 

(PUT '|OUTFORM;sub;3$;42| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SUB |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;super;3$;43|)) 

(PUT '|OUTFORM;super;3$;43| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SUPERSUB |a| " " |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;presub;3$;44|)) 

(PUT '|OUTFORM;presub;3$;44| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SUPERSUB |a| " " " " " " |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;presuper;3$;45|)) 

(PUT '|OUTFORM;presuper;3$;45| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SUPERSUB |a| " " " " |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;scripts;$L$;46|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |OUTFORM;supersub;$L$;47|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;hconcat;3$;48|)) 

(PUT '|OUTFORM;hconcat;3$;48| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'CONCAT |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;hconcat;L$;49|)) 

(PUT '|OUTFORM;hconcat;L$;49| '|SPADreplace|
     '(XLAM (|l|) (CONS 'CONCAT |l|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;vconcat;3$;50|)) 

(PUT '|OUTFORM;vconcat;3$;50| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'VCONCAT |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%List| |%Shell|) |%Thing|)
                |OUTFORM;vconcat;L$;51|)) 

(PUT '|OUTFORM;vconcat;L$;51| '|SPADreplace|
     '(XLAM (|l|) (CONS 'VCONCAT |l|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;~=;3$;52|)) 

(PUT '|OUTFORM;~=;3$;52| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '~= |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;<;3$;53|)) 

(PUT '|OUTFORM;<;3$;53| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '< |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;>;3$;54|)) 

(PUT '|OUTFORM;>;3$;54| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '> |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;<=;3$;55|)) 

(PUT '|OUTFORM;<=;3$;55| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '<= |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;>=;3$;56|)) 

(PUT '|OUTFORM;>=;3$;56| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '>= |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;+;3$;57|)) 

(PUT '|OUTFORM;+;3$;57| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '+ |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;-;3$;58|)) 

(PUT '|OUTFORM;-;3$;58| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '- |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;-;2$;59|)) 

(PUT '|OUTFORM;-;2$;59| '|SPADreplace| '(XLAM (|a|) (LIST '- |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;*;3$;60|)) 

(PUT '|OUTFORM;*;3$;60| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '* |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;/;3$;61|)) 

(PUT '|OUTFORM;/;3$;61| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '/ |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;**;3$;62|)) 

(PUT '|OUTFORM;**;3$;62| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '** |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;div;3$;63|)) 

(PUT '|OUTFORM;div;3$;63| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '|div| |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;rem;3$;64|)) 

(PUT '|OUTFORM;rem;3$;64| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '|rem| |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;quo;3$;65|)) 

(PUT '|OUTFORM;quo;3$;65| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '|quo| |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;exquo;3$;66|)) 

(PUT '|OUTFORM;exquo;3$;66| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '|exquo| |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;and;3$;67|)) 

(PUT '|OUTFORM;and;3$;67| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '|and| |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;or;3$;68|)) 

(PUT '|OUTFORM;or;3$;68| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '|or| |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;not;2$;69|)) 

(PUT '|OUTFORM;not;2$;69| '|SPADreplace|
     '(XLAM (|a|) (LIST '|not| |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;SEGMENT;3$;70|)) 

(PUT '|OUTFORM;SEGMENT;3$;70| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SEGMENT |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;SEGMENT;2$;71|)) 

(PUT '|OUTFORM;SEGMENT;2$;71| '|SPADreplace|
     '(XLAM (|a|) (LIST 'SEGMENT |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;binomial;3$;72|)) 

(PUT '|OUTFORM;binomial;3$;72| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'BINOMIAL |a| |b|))) 

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

(PUT '|OUTFORM;string;2$;80| '|SPADreplace|
     '(XLAM (|a|) (LIST 'STRING |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;quote;2$;81|)) 

(PUT '|OUTFORM;quote;2$;81| '|SPADreplace|
     '(XLAM (|a|) (LIST 'QUOTE |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;overbar;2$;82|)) 

(PUT '|OUTFORM;overbar;2$;82| '|SPADreplace|
     '(XLAM (|a|) (LIST 'OVERBAR |a|))) 

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

(PUT '|OUTFORM;overlabel;3$;87| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'OVERLABEL |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;box;2$;88|)) 

(PUT '|OUTFORM;box;2$;88| '|SPADreplace| '(XLAM (|a|) (LIST 'BOX |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;zag;3$;89|)) 

(PUT '|OUTFORM;zag;3$;89| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'ZAG |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;root;2$;90|)) 

(PUT '|OUTFORM;root;2$;90| '|SPADreplace|
     '(XLAM (|a|) (LIST 'ROOT |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;root;3$;91|)) 

(PUT '|OUTFORM;root;3$;91| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'ROOT |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;over;3$;92|)) 

(PUT '|OUTFORM;over;3$;92| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'OVER |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;slash;3$;93|)) 

(PUT '|OUTFORM;slash;3$;93| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SLASH |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;assign;3$;94|)) 

(PUT '|OUTFORM;assign;3$;94| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST '%LET |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;label;3$;95|)) 

(PUT '|OUTFORM;label;3$;95| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'EQUATNUM |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;rarrow;3$;96|)) 

(PUT '|OUTFORM;rarrow;3$;96| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'RARROW |a| |b|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| (|%IntegerSection| 0) |%Shell|)
                    |%Thing|)
                |OUTFORM;differentiate;$Nni$;97|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;sum;2$;98|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;sum;3$;99|)) 

(PUT '|OUTFORM;sum;3$;99| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'SIGMA |b| |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;sum;4$;100|)) 

(PUT '|OUTFORM;sum;4$;100| '|SPADreplace|
     '(XLAM (|a| |b| |c|) (LIST 'SIGMA2 |b| |c| |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;prod;2$;101|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;prod;3$;102|)) 

(PUT '|OUTFORM;prod;3$;102| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'PI |b| |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;prod;4$;103|)) 

(PUT '|OUTFORM;prod;4$;103| '|SPADreplace|
     '(XLAM (|a| |b| |c|) (LIST 'PI2 |b| |c| |a|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |OUTFORM;int;2$;104|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Thing|)
                |OUTFORM;int;3$;105|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |OUTFORM;int;4$;106|)) 

(PUT '|OUTFORM;int;4$;106| '|SPADreplace|
     '(XLAM (|a| |b| |c|) (LIST 'INTSIGN |b| |c| |a|))) 

(PUT '|OUTFORM;postfix;3$;79| '|SPADreplace|
     '(XLAM (|a| |b|) (LIST 'CONCAT |b| |a|))) 

(PUT '|OUTFORM;dot;2$;83| '|SPADreplace|
     '(XLAM (|a|) (LIST 'SUPERSUB |a| " " '|.|))) 

(PUT '|OUTFORM;prime;2$;84| '|SPADreplace|
     '(XLAM (|a|) (LIST 'SUPERSUB |a| " " '|,|))) 

(DEFUN |OUTFORM;doubleFloatFormat;2S;1| (|s| $)
  (LET ((|ss| (|getShellEntry| $ 6)))
    (SEQ (|setShellEntry| $ 6 |s|) (EXIT |ss|)))) 

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
    (T |s|))) 

(DEFUN |OUTFORM;messagePrint;SV;8| (|s| $)
  (|mathprint| (|OUTFORM;message;S$;7| |s| $))) 

(DEFUN |OUTFORM;=;2$B;9| (|a| |b| $)
  (DECLARE (IGNORE $))
  (EQUAL |a| |b|)) 

(DEFUN |OUTFORM;=;3$;10| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '= |a| |b|)) 

(DEFUN |OUTFORM;coerce;2$;11| (|a| $) (DECLARE (IGNORE $)) |a|) 

(DEFUN |OUTFORM;outputForm;I$;12| (|n| $) (DECLARE (IGNORE $)) |n|) 

(DEFUN |OUTFORM;outputForm;S$;13| (|e| $) (DECLARE (IGNORE $)) |e|) 

(DEFUN |OUTFORM;outputForm;Df$;14| (|f| $)
  (DECLARE (IGNORE $))
  (DFLOAT-FORMAT-GENERAL |f|)) 

(DEFUN |OUTFORM;outputForm;S$;15| (|s| $)
  (SPADCALL (|spadConstant| $ 27)
            (SPADCALL |s| (|spadConstant| $ 27) (|getShellEntry| $ 28))
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
      (|OUTFORM;hspace;I$;29| (TRUNCATE (- |w| (|outformWidth| |a|)) 2)
          $)
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
    ((PLUSP |n|)
     (|OUTFORM;vconcat;3$;50| " " (|OUTFORM;vspace;I$;28| (- |n| 1) $)
         $))
    (T (|OUTFORM;empty;$;73| $)))) 

(DEFUN |OUTFORM;hspace;I$;29| (|n| $)
  (COND
    ((PLUSP |n|) (|fillerSpaces| |n|))
    (T (|OUTFORM;empty;$;73| $)))) 

(DEFUN |OUTFORM;rspace;2I$;30| (|n| |m| $)
  (SEQ (COND
         ((PLUSP |n|)
          (COND ((NOT (PLUSP |m|)) (EXIT (|OUTFORM;empty;$;73| $)))))
         (T (EXIT (|OUTFORM;empty;$;73| $))))
       (EXIT (|OUTFORM;vconcat;3$;50| (|OUTFORM;hspace;I$;29| |n| $)
                 (|OUTFORM;rspace;2I$;30| |n| (- |m| 1) $) $)))) 

(DEFUN |OUTFORM;matrix;L$;31| (|ll| $)
  (LET ((|lv| (LET ((#0=#:G1529 |ll|) (#1=#:G1528 NIL))
                (LOOP
                  (COND
                    ((ATOM #0#) (RETURN (NREVERSE #1#)))
                    (T (LET ((|l| (CAR #0#)))
                         (SETQ #1# (CONS (LIST2VEC |l|) #1#)))))
                  (SETQ #0# (CDR #0#))))))
    (CONS 'MATRIX (LIST2VEC |lv|)))) 

(DEFUN |OUTFORM;pile;L$;32| (|l| $)
  (DECLARE (IGNORE $))
  (CONS 'SC |l|)) 

(DEFUN |OUTFORM;commaSeparate;L$;33| (|l| $)
  (DECLARE (IGNORE $))
  (CONS 'AGGLST |l|)) 

(DEFUN |OUTFORM;semicolonSeparate;L$;34| (|l| $)
  (DECLARE (IGNORE $))
  (CONS 'AGGSET |l|)) 

(DEFUN |OUTFORM;blankSeparate;L$;35| (|l| $)
  (LET ((|c| 'CONCATB) (|l1| NIL))
    (SEQ (LET ((#0=#:G1530 (REVERSE |l|)))
           (LOOP
             (COND
               ((ATOM #0#) (RETURN NIL))
               (T (LET ((|u| (CAR #0#)))
                    (COND
                      ((EQCAR |u| |c|)
                       (SETQ |l1| (APPEND (CDR |u|) |l1|)))
                      (T (SETQ |l1| (CONS |u| |l1|)))))))
             (SETQ #0# (CDR #0#))))
         (EXIT (CONS |c| |l1|))))) 

(DEFUN |OUTFORM;brace;2$;36| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'BRACE |a|)) 

(DEFUN |OUTFORM;brace;L$;37| (|l| $)
  (|OUTFORM;brace;2$;36| (CONS 'AGGLST |l|) $)) 

(DEFUN |OUTFORM;bracket;2$;38| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'BRACKET |a|)) 

(DEFUN |OUTFORM;bracket;L$;39| (|l| $)
  (|OUTFORM;bracket;2$;38| (CONS 'AGGLST |l|) $)) 

(DEFUN |OUTFORM;paren;2$;40| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'PAREN |a|)) 

(DEFUN |OUTFORM;paren;L$;41| (|l| $)
  (|OUTFORM;paren;2$;40| (CONS 'AGGLST |l|) $)) 

(DEFUN |OUTFORM;sub;3$;42| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SUB |a| |b|)) 

(DEFUN |OUTFORM;super;3$;43| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SUPERSUB |a| " " |b|)) 

(DEFUN |OUTFORM;presub;3$;44| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SUPERSUB |a| " " " " " " |b|)) 

(DEFUN |OUTFORM;presuper;3$;45| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SUPERSUB |a| " " " " |b|)) 

(DEFUN |OUTFORM;scripts;$L$;46| (|a| |l| $)
  (COND
    ((NULL |l|) |a|)
    ((NULL (CDR |l|))
     (|OUTFORM;sub;3$;42| |a| (SPADCALL |l| (|getShellEntry| $ 78)) $))
    (T (CONS 'SUPERSUB (CONS |a| |l|))))) 

(DEFUN |OUTFORM;supersub;$L$;47| (|a| |l| $)
  (SEQ (COND
         ((ODDP (LENGTH |l|))
          (SETQ |l| (APPEND |l| (LIST (|OUTFORM;empty;$;73| $))))))
       (EXIT (CONS 'ALTSUPERSUB (CONS |a| |l|))))) 

(DEFUN |OUTFORM;hconcat;3$;48| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'CONCAT |a| |b|)) 

(DEFUN |OUTFORM;hconcat;L$;49| (|l| $)
  (DECLARE (IGNORE $))
  (CONS 'CONCAT |l|)) 

(DEFUN |OUTFORM;vconcat;3$;50| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'VCONCAT |a| |b|)) 

(DEFUN |OUTFORM;vconcat;L$;51| (|l| $)
  (DECLARE (IGNORE $))
  (CONS 'VCONCAT |l|)) 

(DEFUN |OUTFORM;~=;3$;52| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '~= |a| |b|)) 

(DEFUN |OUTFORM;<;3$;53| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '< |a| |b|)) 

(DEFUN |OUTFORM;>;3$;54| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '> |a| |b|)) 

(DEFUN |OUTFORM;<=;3$;55| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '<= |a| |b|)) 

(DEFUN |OUTFORM;>=;3$;56| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '>= |a| |b|)) 

(DEFUN |OUTFORM;+;3$;57| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '+ |a| |b|)) 

(DEFUN |OUTFORM;-;3$;58| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '- |a| |b|)) 

(DEFUN |OUTFORM;-;2$;59| (|a| $) (DECLARE (IGNORE $)) (LIST '- |a|)) 

(DEFUN |OUTFORM;*;3$;60| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '* |a| |b|)) 

(DEFUN |OUTFORM;/;3$;61| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '/ |a| |b|)) 

(DEFUN |OUTFORM;**;3$;62| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '** |a| |b|)) 

(DEFUN |OUTFORM;div;3$;63| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '|div| |a| |b|)) 

(DEFUN |OUTFORM;rem;3$;64| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '|rem| |a| |b|)) 

(DEFUN |OUTFORM;quo;3$;65| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '|quo| |a| |b|)) 

(DEFUN |OUTFORM;exquo;3$;66| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '|exquo| |a| |b|)) 

(DEFUN |OUTFORM;and;3$;67| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '|and| |a| |b|)) 

(DEFUN |OUTFORM;or;3$;68| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '|or| |a| |b|)) 

(DEFUN |OUTFORM;not;2$;69| (|a| $)
  (DECLARE (IGNORE $))
  (LIST '|not| |a|)) 

(DEFUN |OUTFORM;SEGMENT;3$;70| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SEGMENT |a| |b|)) 

(DEFUN |OUTFORM;SEGMENT;2$;71| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'SEGMENT |a|)) 

(DEFUN |OUTFORM;binomial;3$;72| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'BINOMIAL |a| |b|)) 

(DEFUN |OUTFORM;empty;$;73| ($) (LIST 'NOTHING)) 

(DEFUN |OUTFORM;infix?;$B;74| (|a| $)
  (LET ((|e| (COND
               ((IDENTP |a|) |a|)
               ((STRINGP |a|) (INTERN |a|))
               (T (RETURN-FROM |OUTFORM;infix?;$B;74| NIL)))))
    (COND ((GET |e| 'INFIXOP) T) (T NIL)))) 

(DEFUN |OUTFORM;elt;$L$;75| (|a| |l| $)
  (DECLARE (IGNORE $))
  (CONS |a| |l|)) 

(DEFUN |OUTFORM;prefix;$L$;76| (|a| |l| $)
  (COND
    ((NOT (|OUTFORM;infix?;$B;74| |a| $)) (CONS |a| |l|))
    (T (|OUTFORM;hconcat;3$;48| |a|
           (|OUTFORM;paren;2$;40| (CONS 'AGGLST |l|) $) $)))) 

(DEFUN |OUTFORM;infix;$L$;77| (|a| |l| $)
  (COND
    ((NULL |l|) (|OUTFORM;empty;$;73| $))
    ((NULL (CDR |l|)) (SPADCALL |l| (|getShellEntry| $ 78)))
    ((|OUTFORM;infix?;$B;74| |a| $) (CONS |a| |l|))
    (T (|OUTFORM;hconcat;L$;49|
           (LIST (SPADCALL |l| (|getShellEntry| $ 78)) |a|
                 (|OUTFORM;infix;$L$;77| |a| (CDR |l|) $))
           $)))) 

(DEFUN |OUTFORM;infix;4$;78| (|a| |b| |c| $)
  (COND
    ((|OUTFORM;infix?;$B;74| |a| $) (LIST |a| |b| |c|))
    (T (|OUTFORM;hconcat;L$;49| (LIST |b| |a| |c|) $)))) 

(DEFUN |OUTFORM;postfix;3$;79| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'CONCAT |b| |a|)) 

(DEFUN |OUTFORM;string;2$;80| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'STRING |a|)) 

(DEFUN |OUTFORM;quote;2$;81| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'QUOTE |a|)) 

(DEFUN |OUTFORM;overbar;2$;82| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'OVERBAR |a|)) 

(DEFUN |OUTFORM;dot;2$;83| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'SUPERSUB |a| " " '|.|)) 

(DEFUN |OUTFORM;prime;2$;84| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'SUPERSUB |a| " " '|,|)) 

(DEFUN |OUTFORM;dot;$Nni$;85| (|a| |nn| $)
  (LET ((|s| (MAKE-FULL-CVEC |nn|
                 (SPADCALL "." (|getShellEntry| $ 119)))))
    (LIST 'SUPERSUB |a| " " |s|))) 

(DEFUN |OUTFORM;prime;$Nni$;86| (|a| |nn| $)
  (LET ((|s| (MAKE-FULL-CVEC |nn|
                 (SPADCALL "," (|getShellEntry| $ 119)))))
    (LIST 'SUPERSUB |a| " " |s|))) 

(DEFUN |OUTFORM;overlabel;3$;87| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'OVERLABEL |a| |b|)) 

(DEFUN |OUTFORM;box;2$;88| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'BOX |a|)) 

(DEFUN |OUTFORM;zag;3$;89| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'ZAG |a| |b|)) 

(DEFUN |OUTFORM;root;2$;90| (|a| $)
  (DECLARE (IGNORE $))
  (LIST 'ROOT |a|)) 

(DEFUN |OUTFORM;root;3$;91| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'ROOT |a| |b|)) 

(DEFUN |OUTFORM;over;3$;92| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'OVER |a| |b|)) 

(DEFUN |OUTFORM;slash;3$;93| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SLASH |a| |b|)) 

(DEFUN |OUTFORM;assign;3$;94| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST '%LET |a| |b|)) 

(DEFUN |OUTFORM;label;3$;95| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'EQUATNUM |a| |b|)) 

(DEFUN |OUTFORM;rarrow;3$;96| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'RARROW |a| |b|)) 

(DEFUN |OUTFORM;differentiate;$Nni$;97| (|a| |nn| $)
  (PROG (|r| |s|)
    (RETURN
      (SEQ (COND
             ((ZEROP |nn|) |a|)
             ((< |nn| 4) (|OUTFORM;prime;$Nni$;86| |a| |nn| $))
             (T (SEQ (LETT |r|
                           (SPADCALL
                               (|check-subtype| (PLUSP |nn|)
                                   '(|PositiveInteger|) |nn|)
                               (|getShellEntry| $ 137))
                           |OUTFORM;differentiate;$Nni$;97|)
                     (LETT |s| (SPADCALL |r| (|getShellEntry| $ 138))
                           |OUTFORM;differentiate;$Nni$;97|)
                     (EXIT (|OUTFORM;super;3$;43| |a| (LIST 'PAREN |s|)
                               $))))))))) 

(DEFUN |OUTFORM;sum;2$;98| (|a| $)
  (LIST 'SIGMA (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;sum;3$;99| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'SIGMA |b| |a|)) 

(DEFUN |OUTFORM;sum;4$;100| (|a| |b| |c| $)
  (DECLARE (IGNORE $))
  (LIST 'SIGMA2 |b| |c| |a|)) 

(DEFUN |OUTFORM;prod;2$;101| (|a| $)
  (LIST 'PI (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;prod;3$;102| (|a| |b| $)
  (DECLARE (IGNORE $))
  (LIST 'PI |b| |a|)) 

(DEFUN |OUTFORM;prod;4$;103| (|a| |b| |c| $)
  (DECLARE (IGNORE $))
  (LIST 'PI2 |b| |c| |a|)) 

(DEFUN |OUTFORM;int;2$;104| (|a| $)
  (LIST 'INTSIGN (|OUTFORM;empty;$;73| $) (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;int;3$;105| (|a| |b| $)
  (LIST 'INTSIGN |b| (|OUTFORM;empty;$;73| $) |a|)) 

(DEFUN |OUTFORM;int;4$;106| (|a| |b| |c| $)
  (DECLARE (IGNORE $))
  (LIST 'INTSIGN |b| |c| |a|)) 

(DEFUN |OutputForm| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#0=#:G1532)
    (RETURN
      (COND
        ((SETQ #0# (HGET |$ConstructorCache| '|OutputForm|))
         (|CDRwithIncrement| (CDAR #0#)))
        (T (UNWIND-PROTECT
             (PROG1 (CDDAR (HPUT |$ConstructorCache| '|OutputForm|
                                 (LIST (CONS NIL
                                        (CONS 1 (|OutputForm;|))))))
               (SETQ #0# T))
             (COND
               ((NOT #0#) (HREM |$ConstructorCache| '|OutputForm|))))))))) 

(DEFUN |OutputForm;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| (LIST '|OutputForm|)) ($ (|newShell| 150))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
    (|haddProp| |$ConstructorCache| '|OutputForm| NIL (CONS 1 $))
    (|stuffDomainSlots| $)
    (|setShellEntry| $ 6 "~G")
    $)) 

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
             |OUTFORM;height;I;20| |OUTFORM;width;I;21| (21 . -)
             (27 . |quo|) |OUTFORM;hspace;I$;29|
             |OUTFORM;hconcat;3$;48| |OUTFORM;center;$I$;22|
             |OUTFORM;left;$I$;23| |OUTFORM;right;$I$;24|
             |OUTFORM;center;2$;25| |OUTFORM;left;2$;26|
             |OUTFORM;right;2$;27| (|NonNegativeInteger|) (33 . |Zero|)
             (37 . |Zero|) (41 . >) (47 . |One|) (51 . |One|)
             |OUTFORM;vspace;I$;28| |OUTFORM;vconcat;3$;50|
             |OUTFORM;rspace;2I$;30| (|List| $) (|List| 56)
             |OUTFORM;matrix;L$;31| |OUTFORM;pile;L$;32|
             |OUTFORM;commaSeparate;L$;33|
             |OUTFORM;semicolonSeparate;L$;34| (|List| $$)
             (55 . |reverse|) (60 . |append|)
             |OUTFORM;blankSeparate;L$;35| |OUTFORM;brace;2$;36|
             |OUTFORM;brace;L$;37| |OUTFORM;bracket;2$;38|
             |OUTFORM;bracket;L$;39| |OUTFORM;paren;2$;40|
             |OUTFORM;paren;L$;41| |OUTFORM;sub;3$;42|
             |OUTFORM;super;3$;43| |OUTFORM;presub;3$;44|
             |OUTFORM;presuper;3$;45| (66 . |null|) (71 . |rest|)
             (76 . |first|) |OUTFORM;scripts;$L$;46| (81 . |#|)
             (86 . |odd?|) |OUTFORM;supersub;$L$;47|
             |OUTFORM;hconcat;L$;49| |OUTFORM;vconcat;L$;51|
             |OUTFORM;~=;3$;52| |OUTFORM;<;3$;53| |OUTFORM;>;3$;54|
             |OUTFORM;<=;3$;55| |OUTFORM;>=;3$;56| |OUTFORM;+;3$;57|
             |OUTFORM;-;3$;58| |OUTFORM;-;2$;59| |OUTFORM;*;3$;60|
             |OUTFORM;/;3$;61| |OUTFORM;**;3$;62| |OUTFORM;div;3$;63|
             |OUTFORM;rem;3$;64| |OUTFORM;quo;3$;65|
             |OUTFORM;exquo;3$;66| |OUTFORM;and;3$;67|
             |OUTFORM;or;3$;68| |OUTFORM;not;2$;69|
             |OUTFORM;SEGMENT;3$;70| |OUTFORM;SEGMENT;2$;71|
             |OUTFORM;binomial;3$;72| (91 . |false|) (95 . |true|)
             |OUTFORM;infix?;$B;74| |OUTFORM;elt;$L$;75|
             |OUTFORM;prefix;$L$;76| |OUTFORM;infix;$L$;77|
             |OUTFORM;infix;4$;78| |OUTFORM;postfix;3$;79|
             |OUTFORM;string;2$;80| |OUTFORM;quote;2$;81|
             |OUTFORM;overbar;2$;82| |OUTFORM;dot;2$;83|
             |OUTFORM;prime;2$;84| (99 . |char|) (104 . |new|)
             |OUTFORM;dot;$Nni$;85| |OUTFORM;prime;$Nni$;86|
             |OUTFORM;overlabel;3$;87| |OUTFORM;box;2$;88|
             |OUTFORM;zag;3$;89| |OUTFORM;root;2$;90|
             |OUTFORM;root;3$;91| |OUTFORM;over;3$;92|
             |OUTFORM;slash;3$;93| |OUTFORM;assign;3$;94|
             |OUTFORM;label;3$;95| |OUTFORM;rarrow;3$;96|
             (110 . |zero?|) (115 . <) (|PositiveInteger|)
             (|NumberFormats|) (121 . |FormatRoman|)
             (126 . |lowerCase|) |OUTFORM;differentiate;$Nni$;97|
             |OUTFORM;sum;2$;98| |OUTFORM;sum;3$;99|
             |OUTFORM;sum;4$;100| |OUTFORM;prod;2$;101|
             |OUTFORM;prod;3$;102| |OUTFORM;prod;4$;103|
             |OUTFORM;int;2$;104| |OUTFORM;int;3$;105|
             |OUTFORM;int;4$;106| (|SingleInteger|))
          '#(~= 131 |zag| 143 |width| 149 |vspace| 158 |vconcat| 163
             |supersub| 174 |superHeight| 180 |super| 185 |sum| 191
             |subHeight| 209 |sub| 214 |string| 220 |slash| 225
             |semicolonSeparate| 231 |scripts| 236 |rspace| 242 |root|
             248 |right| 259 |rem| 270 |rarrow| 276 |quote| 282 |quo|
             287 |prod| 293 |print| 311 |prime| 316 |presuper| 327
             |presub| 333 |prefix| 339 |postfix| 345 |pile| 351 |paren|
             356 |overlabel| 366 |overbar| 372 |over| 377 |outputForm|
             383 |or| 403 |not| 409 |messagePrint| 414 |message| 419
             |matrix| 424 |left| 429 |latex| 440 |label| 445 |int| 451
             |infix?| 469 |infix| 474 |hspace| 487 |height| 492
             |hconcat| 501 |hash| 512 |exquo| 517 |empty| 523 |elt| 527
             |doubleFloatFormat| 533 |dot| 538 |div| 549
             |differentiate| 555 |commaSeparate| 561 |coerce| 566
             |center| 571 |bracket| 582 |brace| 592 |box| 602
             |blankSeparate| 607 |binomial| 612 |before?| 618 |assign|
             624 |and| 630 SEGMENT 636 >= 647 > 653 = 659 <= 671 < 677
             / 683 - 689 + 700 ** 706 * 712)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0))
                (CONS '#(|SetCategory&| |BasicType&| NIL NIL)
                      (CONS '#((|SetCategory|) (|BasicType|) (|Type|)
                               (|CoercibleTo| 18))
                            (|makeByteWordVec2| 149
                                '(1 7 11 0 12 0 26 0 27 2 7 0 0 26 28 2
                                  7 0 26 0 29 2 20 0 0 0 37 2 20 0 0 0
                                  38 0 47 0 48 0 20 0 49 2 20 11 0 0 50
                                  0 47 0 51 0 20 0 52 1 62 0 0 63 2 62
                                  0 0 0 64 1 62 11 0 76 1 62 0 0 77 1
                                  62 2 0 78 1 62 47 0 80 1 20 11 0 81 0
                                  11 0 106 0 11 0 107 1 26 0 7 119 2 7
                                  0 47 26 120 1 47 11 0 133 2 47 11 0 0
                                  134 1 136 7 135 137 1 7 0 0 138 2 0 0
                                  0 0 85 2 0 11 0 0 1 2 0 0 0 0 125 0 0
                                  20 36 1 0 20 0 31 1 0 0 20 53 1 0 0
                                  56 84 2 0 0 0 0 54 2 0 0 0 56 82 1 0
                                  20 0 34 2 0 0 0 0 73 2 0 0 0 0 141 3
                                  0 0 0 0 0 142 1 0 0 0 140 1 0 20 0 33
                                  2 0 0 0 0 72 1 0 0 0 114 2 0 0 0 0
                                  129 1 0 0 56 61 2 0 0 0 56 79 2 0 0
                                  20 20 55 2 0 0 0 0 127 1 0 0 0 126 1
                                  0 0 0 46 2 0 0 0 20 43 2 0 0 0 0 97 2
                                  0 0 0 0 132 1 0 0 0 115 2 0 0 0 0 98
                                  3 0 0 0 0 0 145 1 0 0 0 143 2 0 0 0 0
                                  144 1 0 9 0 10 1 0 0 0 118 2 0 0 0 47
                                  122 2 0 0 0 0 75 2 0 0 0 0 74 2 0 0 0
                                  56 110 2 0 0 0 0 113 1 0 0 56 59 1 0
                                  0 56 71 1 0 0 0 70 2 0 0 0 0 123 1 0
                                  0 0 116 2 0 0 0 0 128 1 0 0 7 30 1 0
                                  0 24 25 1 0 0 20 21 1 0 0 22 23 2 0 0
                                  0 0 101 1 0 0 0 102 1 0 9 7 15 1 0 0
                                  7 14 1 0 0 57 58 1 0 0 0 45 2 0 0 0
                                  20 42 1 0 7 0 1 2 0 0 0 0 131 3 0 0 0
                                  0 0 148 2 0 0 0 0 147 1 0 0 0 146 1 0
                                  11 0 108 3 0 0 0 0 0 112 2 0 0 0 56
                                  111 1 0 0 20 39 0 0 20 35 1 0 20 0 32
                                  1 0 0 56 83 2 0 0 0 0 40 1 0 149 0 1
                                  2 0 0 0 0 99 0 0 0 13 2 0 0 0 56 109
                                  1 0 7 7 8 1 0 0 0 117 2 0 0 0 47 121
                                  2 0 0 0 0 96 2 0 0 0 47 139 1 0 0 56
                                  60 1 0 18 0 19 1 0 0 0 44 2 0 0 0 20
                                  41 1 0 0 0 68 1 0 0 56 69 1 0 0 56 67
                                  1 0 0 0 66 1 0 0 0 124 1 0 0 56 65 2
                                  0 0 0 0 105 2 0 11 0 0 1 2 0 0 0 0
                                  130 2 0 0 0 0 100 1 0 0 0 104 2 0 0 0
                                  0 103 2 0 0 0 0 89 2 0 0 0 0 87 2 0 0
                                  0 0 17 2 0 11 0 0 16 2 0 0 0 0 88 2 0
                                  0 0 0 86 2 0 0 0 0 94 1 0 0 0 92 2 0
                                  0 0 0 91 2 0 0 0 0 90 2 0 0 0 0 95 2
                                  0 0 0 0 93)))))
          '|lookupComplete|)) 

(MAKEPROP '|OutputForm| 'NILADIC T) 
