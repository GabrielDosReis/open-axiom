
(/VERSIONCHECK 2) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Void|)
                |SYMBOL;writeOMSym|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%String|)
                |SYMBOL;OMwrite;$S;2|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Boolean| |%Shell|) |%String|)
                |SYMBOL;OMwrite;$BS;3|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Void|)
                |SYMBOL;OMwrite;Omd$V;4|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Boolean| |%Shell|)
                    |%Void|)
                |SYMBOL;OMwrite;Omd$BV;5|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;convert;$If;6|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;convert;2$;7|)) 

(PUT '|SYMBOL;convert;2$;7| '|SPADreplace| '(XLAM (|s|) |s|)) 

(DECLAIM (FTYPE (FUNCTION (|%String| |%Shell|) |%Thing|)
                |SYMBOL;coerce;S$;8|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |SYMBOL;=;2$B;9|)) 

(PUT '|SYMBOL;=;2$B;9| '|SPADreplace| 'EQUAL) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Shell|) |%Boolean|)
                |SYMBOL;<;2$B;10|)) 

(PUT '|SYMBOL;<;2$B;10| '|SPADreplace|
     '(XLAM (|x| |y|) (GGREATERP |y| |x|))) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;coerce;$Of;11|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |SYMBOL;subscript;$L$;12|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |SYMBOL;elt;$L$;13|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |SYMBOL;superscript;$L$;14|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |SYMBOL;argscript;$L$;15|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |SYMBOL;patternMatch;$P2Pmr;16|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Thing| |%Thing| |%Shell|)
                    |%Thing|)
                |SYMBOL;patternMatch;$P2Pmr;17|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;convert;$P;18|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;convert;$P;19|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell| |%Shell|) |%String|)
                |SYMBOL;syprefix|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell| |%Shell|) |%List|)
                |SYMBOL;syscripts|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%List| |%Shell|) |%Thing|)
                |SYMBOL;script;$L$;22|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell| |%Shell|) |%Thing|)
                |SYMBOL;script;$R$;23|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%String|)
                |SYMBOL;string;$S;24|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%String|)
                |SYMBOL;latex;$S;25|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%String| |%Shell|) |%String|)
                |SYMBOL;anyRadix|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |SYMBOL;new;$;27|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;new;2$;28|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Void|) |SYMBOL;resetNew;V;29|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Boolean|)
                |SYMBOL;scripted?;$B;30|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Thing|)
                |SYMBOL;name;2$;31|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%Shell|)
                |SYMBOL;scripts;$R;32|)) 

(DECLAIM (FTYPE (FUNCTION (|%Integer| |%Shell|) |%String|)
                |SYMBOL;istring|)) 

(DECLAIM (FTYPE (FUNCTION (|%Thing| |%Shell|) |%List|)
                |SYMBOL;list;$L;34|)) 

(DECLAIM (FTYPE (FUNCTION (|%Shell|) |%Thing|) |SYMBOL;sample;$;35|)) 

(PUT '|SYMBOL;sample;$;35| '|SPADreplace| '(XLAM NIL '|aSymbol|)) 

(DEFUN |SYMBOL;writeOMSym| (|dev| |x| $)
  (COND
    ((|SYMBOL;scripted?;$B;30| |x| $)
     (|error| "Cannot convert a scripted symbol to OpenMath"))
    ('T (SPADCALL |dev| |x| (|getShellEntry| $ 27))))) 

(DEFUN |SYMBOL;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SYMBOL;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SYMBOL;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 29))
                     (|getShellEntry| $ 30))
                 |SYMBOL;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 31))
           (|SYMBOL;writeOMSym| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 32))
           (SPADCALL |dev| (|getShellEntry| $ 33))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SYMBOL;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |SYMBOL;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SYMBOL;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|)
                 |SYMBOL;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 29))
                     (|getShellEntry| $ 30))
                 |SYMBOL;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 31))))
           (|SYMBOL;writeOMSym| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 32))))
           (SPADCALL |dev| (|getShellEntry| $ 33))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|)
                 |SYMBOL;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |SYMBOL;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 31))
       (|SYMBOL;writeOMSym| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 32))))) 

(DEFUN |SYMBOL;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 31))))
       (|SYMBOL;writeOMSym| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 32))))))) 

(DEFUN |SYMBOL;convert;$If;6| (|s| $)
  (SPADCALL |s| (|getShellEntry| $ 47))) 

(DEFUN |SYMBOL;convert;2$;7| (|s| $) (DECLARE (IGNORE $)) |s|) 

(DEFUN |SYMBOL;coerce;S$;8| (|s| $) (VALUES (INTERN |s|))) 

(DEFUN |SYMBOL;=;2$B;9| (|x| |y| $)
  (DECLARE (IGNORE $))
  (EQUAL |x| |y|)) 

(DEFUN |SYMBOL;<;2$B;10| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GGREATERP |y| |x|)) 

(DEFUN |SYMBOL;coerce;$Of;11| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 54))) 

(DEFUN |SYMBOL;subscript;$L$;12| (|sy| |lx| $)
  (|SYMBOL;script;$L$;22| |sy| (LIST |lx| NIL NIL NIL NIL) $)) 

(DEFUN |SYMBOL;elt;$L$;13| (|sy| |lx| $)
  (|SYMBOL;subscript;$L$;12| |sy| |lx| $)) 

(DEFUN |SYMBOL;superscript;$L$;14| (|sy| |lx| $)
  (|SYMBOL;script;$L$;22| |sy| (LIST NIL |lx| NIL NIL NIL) $)) 

(DEFUN |SYMBOL;argscript;$L$;15| (|sy| |lx| $)
  (|SYMBOL;script;$L$;22| |sy| (LIST NIL NIL NIL NIL |lx|) $)) 

(DEFUN |SYMBOL;patternMatch;$P2Pmr;16| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 69))) 

(DEFUN |SYMBOL;patternMatch;$P2Pmr;17| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 76))) 

(DEFUN |SYMBOL;convert;$P;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 79))) 

(DEFUN |SYMBOL;convert;$P;19| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 81))) 

(DEFUN |SYMBOL;syprefix| (|sc| $)
  (PROG (|ns| #0=#:G1549 |n| #1=#:G1550)
    (RETURN
      (SEQ (LETT |ns|
                 (LIST (LENGTH (QVELT |sc| 3)) (LENGTH (QVELT |sc| 2))
                       (LENGTH (QVELT |sc| 1)) (LENGTH (QVELT |sc| 0)))
                 |SYMBOL;syprefix|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((>= (LENGTH |ns|) 2)
                            (ZEROP (|SPADfirst| |ns|)))
                           ('T NIL)))
                   (GO G191)))
                (SEQ (EXIT (LETT |ns| (CDR |ns|) |SYMBOL;syprefix|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL
                     (CONS (STRCONC (|getShellEntry| $ 38)
                                    (|SYMBOL;istring|
                                     (LENGTH (QVELT |sc| 4)) $))
                           (PROGN
                             (LETT #0# NIL |SYMBOL;syprefix|)
                             (SEQ (LETT |n| NIL |SYMBOL;syprefix|)
                                  (LETT #1# (NREVERSE |ns|)
                                        |SYMBOL;syprefix|)
                                  G190
                                  (COND
                                    ((OR (ATOM #1#)
                                      (PROGN
                                        (LETT |n| (CAR #1#)
                                         |SYMBOL;syprefix|)
                                        NIL))
                                     (GO G191)))
                                  (SEQ (EXIT
                                        (LETT #0#
                                         (CONS (|SYMBOL;istring| |n| $)
                                          #0#)
                                         |SYMBOL;syprefix|)))
                                  (LETT #1# (CDR #1#)
                                        |SYMBOL;syprefix|)
                                  (GO G190) G191
                                  (EXIT (NREVERSE0 #0#)))))
                     (|getShellEntry| $ 95))))))) 

(DEFUN |SYMBOL;syscripts| (|sc| $)
  (PROG (|all|)
    (RETURN
      (SEQ (LETT |all| (QVELT |sc| 3) |SYMBOL;syscripts|)
           (LETT |all|
                 (SPADCALL (QVELT |sc| 2) |all| (|getShellEntry| $ 96))
                 |SYMBOL;syscripts|)
           (LETT |all|
                 (SPADCALL (QVELT |sc| 1) |all| (|getShellEntry| $ 96))
                 |SYMBOL;syscripts|)
           (LETT |all|
                 (SPADCALL (QVELT |sc| 0) |all| (|getShellEntry| $ 96))
                 |SYMBOL;syscripts|)
           (EXIT (SPADCALL |all| (QVELT |sc| 4) (|getShellEntry| $ 96))))))) 

(DEFUN |SYMBOL;script;$L$;22| (|sy| |ls| $)
  (PROG (|sc|)
    (RETURN
      (SEQ (LETT |sc| (VECTOR NIL NIL NIL NIL NIL)
                 |SYMBOL;script;$L$;22|)
           (COND
             ((NOT (NULL |ls|))
              (SEQ (QSETVELT |sc| 0 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NOT (NULL |ls|))
              (SEQ (QSETVELT |sc| 1 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NOT (NULL |ls|))
              (SEQ (QSETVELT |sc| 2 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NOT (NULL |ls|))
              (SEQ (QSETVELT |sc| 3 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NOT (NULL |ls|))
              (SEQ (QSETVELT |sc| 4 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (EXIT (|SYMBOL;script;$R$;23| |sy| |sc| $)))))) 

(DEFUN |SYMBOL;script;$R$;23| (|sy| |sc| $)
  (COND
    ((|SYMBOL;scripted?;$B;30| |sy| $)
     (|error| "Cannot add scripts to a scripted symbol"))
    ('T
     (CONS (|SYMBOL;coerce;$Of;11|
               (|SYMBOL;coerce;S$;8|
                   (STRCONC (|SYMBOL;syprefix| |sc| $)
                            (|SYMBOL;string;$S;24|
                                (|SYMBOL;name;2$;31| |sy| $) $))
                   $)
               $)
           (|SYMBOL;syscripts| |sc| $))))) 

(DEFUN |SYMBOL;string;$S;24| (|e| $)
  (COND
    ((NOT (|SYMBOL;scripted?;$B;30| |e| $)) (PNAME |e|))
    ('T (|error| "Cannot form string from non-atomic symbols.")))) 

(DEFUN |SYMBOL;latex;$S;25| (|e| $)
  (PROG (|ss| |lo| |sc| |s|)
    (RETURN
      (SEQ (LETT |s| (PNAME (|SYMBOL;name;2$;31| |e| $))
                 |SYMBOL;latex;$S;25|)
           (COND
             ((> (QCSIZE |s|) 1)
              (COND
                ((SPADCALL (SPADCALL |s| 1 (|getShellEntry| $ 108))
                     (SPADCALL "\\" (|getShellEntry| $ 43))
                     (|getShellEntry| $ 109))
                 (LETT |s| (STRCONC "\\mbox{\\it " (STRCONC |s| "}"))
                       |SYMBOL;latex;$S;25|)))))
           (COND ((NOT (|SYMBOL;scripted?;$B;30| |e| $)) (EXIT |s|)))
           (LETT |ss| (|SYMBOL;scripts;$R;32| |e| $)
                 |SYMBOL;latex;$S;25|)
           (LETT |lo| (QVELT |ss| 0) |SYMBOL;latex;$S;25|)
           (COND
             ((NOT (NULL |lo|))
              (SEQ (LETT |sc| "_{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND ((NULL (NOT (NULL |lo|))) (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 114)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NOT (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |s| |sc|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 1) |SYMBOL;latex;$S;25|)
           (COND
             ((NOT (NULL |lo|))
              (SEQ (LETT |sc| "^{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND ((NULL (NOT (NULL |lo|))) (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 114)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NOT (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |s| |sc|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 2) |SYMBOL;latex;$S;25|)
           (COND
             ((NOT (NULL |lo|))
              (SEQ (LETT |sc| "{}^{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND ((NULL (NOT (NULL |lo|))) (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 114)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NOT (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |sc| |s|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 3) |SYMBOL;latex;$S;25|)
           (COND
             ((NOT (NULL |lo|))
              (SEQ (LETT |sc| "{}_{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND ((NULL (NOT (NULL |lo|))) (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 114)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NOT (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |sc| |s|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 4) |SYMBOL;latex;$S;25|)
           (COND
             ((NOT (NULL |lo|))
              (SEQ (LETT |sc| "\\left( {" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND ((NULL (NOT (NULL |lo|))) (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 114)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NOT (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "} \\right)")
                         |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |s| |sc|)
                               |SYMBOL;latex;$S;25|)))))
           (EXIT |s|))))) 

(DEFUN |SYMBOL;anyRadix| (|n| |s| $)
  (PROG (|qr| |ns| #0=#:G1505)
    (RETURN
      (SEQ (EXIT (SEQ (LETT |ns| "" |SYMBOL;anyRadix|)
                      (EXIT (SEQ G190 NIL
                                 (SEQ (LETT |qr|
                                       (DIVIDE2 |n| (QCSIZE |s|))
                                       |SYMBOL;anyRadix|)
                                      (LETT |n| (QCAR |qr|)
                                       |SYMBOL;anyRadix|)
                                      (LETT |ns|
                                       (SPADCALL
                                        (SPADCALL |s|
                                         (+ (QCDR |qr|)
                                          (SPADCALL |s|
                                           (|getShellEntry| $ 119)))
                                         (|getShellEntry| $ 108))
                                        |ns| (|getShellEntry| $ 121))
                                       |SYMBOL;anyRadix|)
                                      (EXIT
                                       (COND
                                         ((ZEROP |n|)
                                          (PROGN
                                            (LETT #0# |ns|
                                             |SYMBOL;anyRadix|)
                                            (GO #0#))))))
                                 NIL (GO G190) G191 (EXIT NIL)))))
           #0# (EXIT #0#))))) 

(DEFUN |SYMBOL;new;$;27| ($)
  (PROG (|sym|)
    (RETURN
      (SEQ (LETT |sym|
                 (|SYMBOL;anyRadix|
                     (SPADCALL (|getShellEntry| $ 10)
                         (|getShellEntry| $ 122))
                     (|getShellEntry| $ 20) $)
                 |SYMBOL;new;$;27|)
           (SPADCALL (|getShellEntry| $ 10)
               (+ (SPADCALL (|getShellEntry| $ 10)
                      (|getShellEntry| $ 122))
                  1)
               (|getShellEntry| $ 123))
           (EXIT (|SYMBOL;coerce;S$;8| (STRCONC "%" |sym|) $)))))) 

(DEFUN |SYMBOL;new;2$;28| (|x| $)
  (PROG (|u| |n| |xx|)
    (RETURN
      (SEQ (LETT |n|
                 (SEQ (LETT |u|
                            (SPADCALL |x| (|getShellEntry| $ 13)
                                (|getShellEntry| $ 126))
                            |SYMBOL;new;2$;28|)
                      (EXIT (COND
                              ((QEQCAR |u| 1) 0)
                              ('T (+ (QCDR |u|) 1)))))
                 |SYMBOL;new;2$;28|)
           (SPADCALL (|getShellEntry| $ 13) |x| |n|
               (|getShellEntry| $ 129))
           (LETT |xx|
                 (COND
                   ((NOT (|SYMBOL;scripted?;$B;30| |x| $))
                    (|SYMBOL;string;$S;24| |x| $))
                   ('T
                    (|SYMBOL;string;$S;24| (|SYMBOL;name;2$;31| |x| $)
                        $)))
                 |SYMBOL;new;2$;28|)
           (LETT |xx| (STRCONC "%" |xx|) |SYMBOL;new;2$;28|)
           (LETT |xx|
                 (COND
                   ((>= (SPADCALL
                            (SPADCALL |xx|
                                (SPADCALL |xx| (|getShellEntry| $ 130))
                                (|getShellEntry| $ 108))
                            (|getShellEntry| $ 19)
                            (|getShellEntry| $ 131))
                        (SPADCALL (|getShellEntry| $ 19)
                            (|getShellEntry| $ 119)))
                    (STRCONC |xx|
                             (|SYMBOL;anyRadix| |n|
                                 (|getShellEntry| $ 21) $)))
                   ('T
                    (STRCONC |xx|
                             (|SYMBOL;anyRadix| |n|
                                 (|getShellEntry| $ 19) $))))
                 |SYMBOL;new;2$;28|)
           (COND
             ((NOT (|SYMBOL;scripted?;$B;30| |x| $))
              (EXIT (|SYMBOL;coerce;S$;8| |xx| $))))
           (EXIT (|SYMBOL;script;$R$;23| (|SYMBOL;coerce;S$;8| |xx| $)
                     (|SYMBOL;scripts;$R;32| |x| $) $)))))) 

(DEFUN |SYMBOL;resetNew;V;29| ($)
  (PROG (|k| #0=#:G1551)
    (RETURN
      (SEQ (SPADCALL (|getShellEntry| $ 10) 0 (|getShellEntry| $ 123))
           (SEQ (LETT |k| NIL |SYMBOL;resetNew;V;29|)
                (LETT #0#
                      (SPADCALL (|getShellEntry| $ 13)
                          (|getShellEntry| $ 135))
                      |SYMBOL;resetNew;V;29|)
                G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |k| (CAR #0#) |SYMBOL;resetNew;V;29|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (SPADCALL |k| (|getShellEntry| $ 13)
                               (|getShellEntry| $ 136))))
                (LETT #0# (CDR #0#) |SYMBOL;resetNew;V;29|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (SPADCALL (|getShellEntry| $ 137))))))) 

(DEFUN |SYMBOL;scripted?;$B;30| (|sy| $) (NOT (ATOM |sy|))) 

(DEFUN |SYMBOL;name;2$;31| (|sy| $)
  (PROG (|str| |i| #0=#:G1552 #1=#:G1532 #2=#:G1530)
    (RETURN
      (SEQ (EXIT (COND
                   ((NOT (|SYMBOL;scripted?;$B;30| |sy| $)) |sy|)
                   ('T
                    (SEQ (LETT |str|
                               (|SYMBOL;string;$S;24|
                                   (SPADCALL
                                    (|SYMBOL;list;$L;34| |sy| $)
                                    (|getShellEntry| $ 140))
                                   $)
                               |SYMBOL;name;2$;31|)
                         (SEQ (EXIT (SEQ
                                     (LETT |i|
                                      (+ (|getShellEntry| $ 41) 1)
                                      |SYMBOL;name;2$;31|)
                                     (LETT #0# (QCSIZE |str|)
                                      |SYMBOL;name;2$;31|)
                                     G190
                                     (COND ((> |i| #0#) (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (COND
                                         ((NOT
                                           (SPADCALL
                                            (SPADCALL |str| |i|
                                             (|getShellEntry| $ 108))
                                            (|getShellEntry| $ 142)))
                                          (PROGN
                                            (LETT #2#
                                             (PROGN
                                               (LETT #1#
                                                (|SYMBOL;coerce;S$;8|
                                                 (SPADCALL |str|
                                                  (SPADCALL |i|
                                                   (QCSIZE |str|)
                                                   (|getShellEntry| $
                                                    144))
                                                  (|getShellEntry| $
                                                   145))
                                                 $)
                                                |SYMBOL;name;2$;31|)
                                               (GO #1#))
                                             |SYMBOL;name;2$;31|)
                                            (GO #2#))))))
                                     (LETT |i| (+ |i| 1)
                                      |SYMBOL;name;2$;31|)
                                     (GO G190) G191 (EXIT NIL)))
                              #2# (EXIT #2#))
                         (EXIT (|error| "Improper scripted symbol"))))))
           #1# (EXIT #1#))))) 

(DEFUN |SYMBOL;scripts;$R;32| (|sy| $)
  (PROG (|lscripts| |str| |nstr| |j| #0=#:G1535 |nscripts| |m| |n|
            #1=#:G1553 |i| #2=#:G1554 |a| #3=#:G1555 |allscripts|)
    (RETURN
      (SEQ (COND
             ((NOT (|SYMBOL;scripted?;$B;30| |sy| $))
              (VECTOR NIL NIL NIL NIL NIL))
             ('T
              (SEQ (LETT |nscripts| (LIST 0 0 0 0 0)
                         |SYMBOL;scripts;$R;32|)
                   (LETT |lscripts| (LIST NIL NIL NIL NIL NIL)
                         |SYMBOL;scripts;$R;32|)
                   (LETT |str|
                         (|SYMBOL;string;$S;24|
                             (SPADCALL (|SYMBOL;list;$L;34| |sy| $)
                                 (|getShellEntry| $ 140))
                             $)
                         |SYMBOL;scripts;$R;32|)
                   (LETT |nstr| (QCSIZE |str|) |SYMBOL;scripts;$R;32|)
                   (LETT |m|
                         (SPADCALL |nscripts| (|getShellEntry| $ 147))
                         |SYMBOL;scripts;$R;32|)
                   (SEQ (LETT |j| (+ (|getShellEntry| $ 41) 1)
                              |SYMBOL;scripts;$R;32|)
                        (LETT |i| |m| |SYMBOL;scripts;$R;32|) G190
                        (COND
                          ((OR (> |j| |nstr|)
                               (NULL (SPADCALL
                                      (SPADCALL |str| |j|
                                       (|getShellEntry| $ 108))
                                      (|getShellEntry| $ 142))))
                           (GO G191)))
                        (SPADCALL |nscripts| |i|
                            (PROG1 (LETT #0#
                                    (-
                                     (SPADCALL
                                      (SPADCALL |str| |j|
                                       (|getShellEntry| $ 108))
                                      (|getShellEntry| $ 44))
                                     (|getShellEntry| $ 45))
                                    |SYMBOL;scripts;$R;32|)
                              (|check-subtype| (>= #0# 0)
                                  '(|NonNegativeInteger|) #0#))
                            (|getShellEntry| $ 151))
                        (LETT |i|
                              (PROG1 (+ |i| 1)
                                (LETT |j| (+ |j| 1)
                                      |SYMBOL;scripts;$R;32|))
                              |SYMBOL;scripts;$R;32|)
                        (GO G190) G191 (EXIT NIL))
                   (LETT |nscripts|
                         (SPADCALL (CDR |nscripts|)
                             (|SPADfirst| |nscripts|)
                             (|getShellEntry| $ 154))
                         |SYMBOL;scripts;$R;32|)
                   (LETT |allscripts|
                         (SPADCALL (|SYMBOL;list;$L;34| |sy| $)
                             (|getShellEntry| $ 155))
                         |SYMBOL;scripts;$R;32|)
                   (LETT |m|
                         (SPADCALL |lscripts| (|getShellEntry| $ 156))
                         |SYMBOL;scripts;$R;32|)
                   (SEQ (LETT |n| NIL |SYMBOL;scripts;$R;32|)
                        (LETT #1# |nscripts| |SYMBOL;scripts;$R;32|)
                        (LETT |i| |m| |SYMBOL;scripts;$R;32|) G190
                        (COND
                          ((OR (ATOM #1#)
                               (PROGN
                                 (LETT |n| (CAR #1#)
                                       |SYMBOL;scripts;$R;32|)
                                 NIL))
                           (GO G191)))
                        (COND
                          ((< (SPADCALL |allscripts|
                                  (|getShellEntry| $ 157))
                              |n|)
                           (|error| "Improper script count in symbol"))
                          ('T
                           (SEQ (SPADCALL |lscripts| |i|
                                    (PROGN
                                      (LETT #2# NIL
                                       |SYMBOL;scripts;$R;32|)
                                      (SEQ
                                       (LETT |a| NIL
                                        |SYMBOL;scripts;$R;32|)
                                       (LETT #3#
                                        (SPADCALL |allscripts| |n|
                                         (|getShellEntry| $ 159))
                                        |SYMBOL;scripts;$R;32|)
                                       G190
                                       (COND
                                         ((OR (ATOM #3#)
                                           (PROGN
                                             (LETT |a| (CAR #3#)
                                              |SYMBOL;scripts;$R;32|)
                                             NIL))
                                          (GO G191)))
                                       (LETT #2#
                                        (CONS
                                         (|SYMBOL;coerce;$Of;11| |a| $)
                                         #2#)
                                        |SYMBOL;scripts;$R;32|)
                                       (LETT #3# (CDR #3#)
                                        |SYMBOL;scripts;$R;32|)
                                       (GO G190) G191
                                       (EXIT (NREVERSE0 #2#))))
                                    (|getShellEntry| $ 160))
                                (EXIT (LETT |allscripts|
                                       (SPADCALL |allscripts| |n|
                                        (|getShellEntry| $ 161))
                                       |SYMBOL;scripts;$R;32|)))))
                        (LETT |i|
                              (PROG1 (+ |i| 1)
                                (LETT #1# (CDR #1#)
                                      |SYMBOL;scripts;$R;32|))
                              |SYMBOL;scripts;$R;32|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT (VECTOR (SPADCALL |lscripts| |m|
                                     (|getShellEntry| $ 162))
                                 (SPADCALL |lscripts| (+ |m| 1)
                                     (|getShellEntry| $ 162))
                                 (SPADCALL |lscripts| (+ |m| 2)
                                     (|getShellEntry| $ 162))
                                 (SPADCALL |lscripts| (+ |m| 3)
                                     (|getShellEntry| $ 162))
                                 (SPADCALL |lscripts| (+ |m| 4)
                                     (|getShellEntry| $ 162))))))))))) 

(DEFUN |SYMBOL;istring| (|n| $)
  (COND
    ((> |n| 9) (|error| "Can have at most 9 scripts of each kind"))
    ('T (|getSimpleArrayEntry| (|getShellEntry| $ 18) (+ |n| 0))))) 

(DEFUN |SYMBOL;list;$L;34| (|sy| $)
  (COND
    ((NOT (|SYMBOL;scripted?;$B;30| |sy| $))
     (|error| "Cannot convert a symbol to a list if it is not subscripted"))
    ('T |sy|))) 

(DEFUN |SYMBOL;sample;$;35| ($) (DECLARE (IGNORE $)) '|aSymbol|) 

(DEFUN |Symbol| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1557)
        (RETURN
          (COND
            ((LETT #0# (HGET |$ConstructorCache| '|Symbol|) |Symbol|)
             (|CDRwithIncrement| (CDAR #0#)))
            ('T
             (UNWIND-PROTECT
               (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Symbol|
                                   (LIST
                                    (CONS NIL (CONS 1 (|Symbol;|))))))
                 (LETT #0# T |Symbol|))
               (COND ((NOT #0#) (HREM |$ConstructorCache| '|Symbol|))))))))))) 

(DEFUN |Symbol;| ()
  (PROG (|dv$| $ |pv$|)
    (RETURN
      (PROGN
        (LETT |dv$| '(|Symbol|) . #0=(|Symbol|))
        (LETT $ (|newShell| 168) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Symbol| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 10 (SPADCALL 0 (|getShellEntry| $ 9)))
        (|setShellEntry| $ 13 (SPADCALL (|getShellEntry| $ 12)))
        (|setShellEntry| $ 18
            (SPADCALL (LIST "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
                (|getShellEntry| $ 17)))
        (|setShellEntry| $ 19 "0123456789")
        (|setShellEntry| $ 20 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (|setShellEntry| $ 21 "abcdefghijklmnopqrstuvwxyz")
        (|setShellEntry| $ 38 "*")
        (|setShellEntry| $ 41 (QCSIZE (|getShellEntry| $ 38)))
        (|setShellEntry| $ 45
            (SPADCALL (SPADCALL "0" (|getShellEntry| $ 43))
                (|getShellEntry| $ 44)))
        $)))) 

(MAKEPROP '|Symbol| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Integer|) (0 . |Zero|)
             (|Reference| 6) (4 . |ref|) '|count|
             (|AssociationList| $$ 6) (9 . |empty|) '|xcount|
             (|String|) (|List| 14) (|PrimitiveArray| 14)
             (13 . |construct|) '|istrings| '|nums| 'ALPHAS '|alphas|
             (|Boolean|) |SYMBOL;scripted?;$B;30| (|Void|) (|Symbol|)
             (|OpenMathDevice|) (18 . |OMputVariable|)
             (|OpenMathEncoding|) (24 . |OMencodingXML|)
             (28 . |OMopenString|) (34 . |OMputObject|)
             (39 . |OMputEndObject|) (44 . |OMclose|)
             |SYMBOL;OMwrite;$S;2| |SYMBOL;OMwrite;$BS;3|
             |SYMBOL;OMwrite;Omd$V;4| |SYMBOL;OMwrite;Omd$BV;5| '|hd|
             (|NonNegativeInteger|) (49 . |#|) '|lhd| (|Character|)
             (54 . |char|) (59 . |ord|) '|ord0| (|InputForm|)
             (64 . |convert|) |SYMBOL;convert;$If;6|
             |SYMBOL;convert;2$;7| |SYMBOL;coerce;S$;8|
             |SYMBOL;=;2$B;9| |SYMBOL;<;2$B;10| (|OutputForm|)
             (69 . |outputForm|) |SYMBOL;coerce;$Of;11| (74 . |nil|)
             (|List| 53) (78 . |nil|) (|List| 57) (82 . |nil|)
             |SYMBOL;script;$L$;22| |SYMBOL;subscript;$L$;12|
             |SYMBOL;elt;$L$;13| |SYMBOL;superscript;$L$;14|
             |SYMBOL;argscript;$L$;15| (|PatternMatchResult| 6 25)
             (|Pattern| 6) (|PatternMatchSymbol| 6)
             (86 . |patternMatch|) (|PatternMatchResult| 6 $)
             |SYMBOL;patternMatch;$P2Pmr;16| (|Float|)
             (|PatternMatchResult| 72 25) (|Pattern| 72)
             (|PatternMatchSymbol| 72) (93 . |patternMatch|)
             (|PatternMatchResult| 72 $)
             |SYMBOL;patternMatch;$P2Pmr;17| (100 . |coerce|)
             |SYMBOL;convert;$P;18| (105 . |coerce|)
             |SYMBOL;convert;$P;19| (110 . |#|) (|List| 6) (115 . |#|)
             (120 . >=) (126 . |first|) (131 . |zero?|) (136 . |false|)
             (140 . |rest|) (145 . |concat|) (151 . |reverse!|)
             (156 . |concat|) (|List| $) (162 . |concat|)
             (167 . |concat|) (173 . |null|) (178 . |first|)
             (183 . |rest|)
             (|Record| (|:| |sub| 57) (|:| |sup| 57) (|:| |presup| 57)
                 (|:| |presub| 57) (|:| |args| 57))
             |SYMBOL;script;$R$;23| |SYMBOL;name;2$;31|
             |SYMBOL;string;$S;24| (188 . |concat|) (194 . |One|)
             (198 . >) (204 . |One|) (208 . |elt|) (214 . ~=)
             |SYMBOL;scripts;$R;32| (220 . |empty?|) (225 . |not|)
             (230 . |first|) (235 . |latex|) (240 . |rest|)
             |SYMBOL;latex;$S;25|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (245 . |divide|) (251 . |minIndex|) (256 . +)
             (262 . |concat|) (268 . |elt|) (273 . |setelt|)
             |SYMBOL;new;$;27| (|Union| 6 '"failed") (279 . |search|)
             (285 . |Zero|) (289 . |inc|) (294 . |setelt|)
             (301 . |maxIndex|) (306 . |position|) (312 . >=)
             |SYMBOL;new;2$;28| (|List| $$) (318 . |keys|)
             (323 . |remove!|) (329 . |void|) |SYMBOL;resetNew;V;29|
             |SYMBOL;list;$L;34| (333 . |first|) (338 . +)
             (344 . |digit?|) (|UniversalSegment| 6) (349 . SEGMENT)
             (355 . |elt|) (|List| 39) (361 . |minIndex|)
             (|PositiveInteger|) (366 . |One|) (370 . -)
             (376 . |setelt|) (383 . |rest|) (388 . |first|)
             (393 . |concat|) (399 . |rest|) (404 . |minIndex|)
             (409 . |#|) (414 . <) (420 . |first|) (426 . |setelt|)
             (433 . |rest|) (439 . |elt|) (445 . >) (451 . |minIndex|)
             (456 . |elt|)
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SYMBOL;sample;$;35|)
                            $))
             (|SingleInteger|))
          '#(~= 462 |superscript| 468 |subscript| 474 |string| 480
             |scripts| 485 |scripted?| 490 |script| 495 |sample| 507
             |resetNew| 511 |patternMatch| 515 |new| 529 |name| 538
             |min| 543 |max| 549 |list| 555 |latex| 560 |hash| 565
             |elt| 570 |convert| 576 |coerce| 596 |before?| 606
             |argscript| 612 |OMwrite| 618 >= 642 > 648 = 654 <= 660 <
             666)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(|OrderedSet&| NIL NIL |SetCategory&|
                         |BasicType&| NIL NIL NIL NIL NIL NIL NIL)
                      (CONS '#((|OrderedSet|) (|PatternMatchable| 72)
                               (|PatternMatchable| 6) (|SetCategory|)
                               (|BasicType|) (|ConvertibleTo| 74)
                               (|ConvertibleTo| 67)
                               (|CoercibleFrom| 14)
                               (|ConvertibleTo| 25) (|OpenMath|)
                               (|ConvertibleTo| 46) (|CoercibleTo| 53))
                            (|makeByteWordVec2| 167
                                '(0 6 0 7 1 8 0 6 9 0 11 0 12 1 16 0 15
                                  17 2 26 24 0 25 27 0 28 0 29 2 26 0
                                  14 28 30 1 26 24 0 31 1 26 24 0 32 1
                                  26 24 0 33 1 14 39 0 40 1 42 0 14 43
                                  1 42 39 0 44 1 46 0 25 47 1 53 0 25
                                  54 0 15 0 56 0 57 0 58 0 59 0 60 3 68
                                  66 25 67 66 69 3 75 73 25 74 73 76 1
                                  74 0 25 79 1 67 0 25 81 1 57 39 0 83
                                  1 84 39 0 85 2 39 22 0 0 86 1 84 6 0
                                  87 1 6 22 0 88 0 22 0 89 1 84 0 0 90
                                  2 14 0 0 0 91 1 84 0 0 92 2 15 0 14 0
                                  93 1 14 0 94 95 2 57 0 0 0 96 1 59 22
                                  0 97 1 59 57 0 98 1 59 0 0 99 2 57 0
                                  53 0 104 0 39 0 105 2 39 22 0 0 106 0
                                  6 0 107 2 14 42 0 6 108 2 42 22 0 0
                                  109 1 57 22 0 111 1 22 0 0 112 1 57
                                  53 0 113 1 53 14 0 114 1 57 0 0 115 2
                                  6 117 0 0 118 1 14 6 0 119 2 6 0 0 0
                                  120 2 14 0 42 0 121 1 8 6 0 122 2 8 6
                                  0 6 123 2 11 125 2 0 126 0 39 0 127 1
                                  6 0 0 128 3 11 6 0 2 6 129 1 14 6 0
                                  130 2 14 6 42 0 131 2 6 22 0 0 132 1
                                  11 134 0 135 2 11 125 2 0 136 0 24 0
                                  137 1 134 2 0 140 2 39 0 0 0 141 1 42
                                  22 0 142 2 143 0 6 6 144 2 14 0 0 143
                                  145 1 146 6 0 147 0 148 0 149 2 6 0 0
                                  0 150 3 146 39 0 6 39 151 1 146 0 0
                                  152 1 146 39 0 153 2 146 0 0 39 154 1
                                  134 0 0 155 1 59 6 0 156 1 134 39 0
                                  157 2 39 22 0 0 158 2 134 0 0 39 159
                                  3 59 57 0 6 57 160 2 134 0 0 39 161 2
                                  59 57 0 6 162 2 6 22 0 0 163 1 16 6 0
                                  164 2 16 14 0 6 165 2 0 22 0 0 1 2 0
                                  0 0 57 64 2 0 0 0 57 62 1 0 14 0 103
                                  1 0 100 0 110 1 0 22 0 23 2 0 0 0 59
                                  61 2 0 0 0 100 101 0 0 0 166 0 0 24
                                  138 3 0 77 0 74 77 78 3 0 70 0 67 70
                                  71 1 0 0 0 133 0 0 0 124 1 0 0 0 102
                                  2 0 0 0 0 1 2 0 0 0 0 1 1 0 94 0 139
                                  1 0 14 0 116 1 0 167 0 1 2 0 0 0 57
                                  63 1 0 67 0 82 1 0 74 0 80 1 0 25 0
                                  49 1 0 46 0 48 1 0 0 14 50 1 0 53 0
                                  55 2 0 22 0 0 1 2 0 0 0 57 65 2 0 24
                                  26 0 36 3 0 24 26 0 22 37 1 0 14 0 34
                                  2 0 14 0 22 35 2 0 22 0 0 1 2 0 22 0
                                  0 1 2 0 22 0 0 51 2 0 22 0 0 1 2 0 22
                                  0 0 52)))))
          '|lookupComplete|)) 

(MAKEPROP '|Symbol| 'NILADIC T) 