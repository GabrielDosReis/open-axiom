
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

(DEFUN |SYMBOL;writeOMSym| (|dev| |x| $)
  (COND
    ((SPADCALL |x| (|getShellEntry| $ 22))
     (|error| "Cannot convert a scripted symbol to OpenMath"))
    ('T (SPADCALL |dev| |x| (|getShellEntry| $ 26))))) 

(DEFUN |SYMBOL;OMwrite;$S;2| (|x| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SYMBOL;OMwrite;$S;2|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|) |SYMBOL;OMwrite;$S;2|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 28))
                     (|getShellEntry| $ 29))
                 |SYMBOL;OMwrite;$S;2|)
           (SPADCALL |dev| (|getShellEntry| $ 30))
           (|SYMBOL;writeOMSym| |dev| |x| $)
           (SPADCALL |dev| (|getShellEntry| $ 31))
           (SPADCALL |dev| (|getShellEntry| $ 32))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|) |SYMBOL;OMwrite;$S;2|)
           (EXIT |s|))))) 

(DEFUN |SYMBOL;OMwrite;$BS;3| (|x| |wholeObj| $)
  (PROG (|sp| |dev| |s|)
    (RETURN
      (SEQ (LETT |s| "" |SYMBOL;OMwrite;$BS;3|)
           (LETT |sp| (OM-STRINGTOSTRINGPTR |s|)
                 |SYMBOL;OMwrite;$BS;3|)
           (LETT |dev|
                 (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 28))
                     (|getShellEntry| $ 29))
                 |SYMBOL;OMwrite;$BS;3|)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 30))))
           (|SYMBOL;writeOMSym| |dev| |x| $)
           (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 31))))
           (SPADCALL |dev| (|getShellEntry| $ 32))
           (LETT |s| (OM-STRINGPTRTOSTRING |sp|)
                 |SYMBOL;OMwrite;$BS;3|)
           (EXIT |s|))))) 

(DEFUN |SYMBOL;OMwrite;Omd$V;4| (|dev| |x| $)
  (SEQ (SPADCALL |dev| (|getShellEntry| $ 30))
       (|SYMBOL;writeOMSym| |dev| |x| $)
       (EXIT (SPADCALL |dev| (|getShellEntry| $ 31))))) 

(DEFUN |SYMBOL;OMwrite;Omd$BV;5| (|dev| |x| |wholeObj| $)
  (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 30))))
       (|SYMBOL;writeOMSym| |dev| |x| $)
       (EXIT (COND
               (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 31))))))) 

(DEFUN |SYMBOL;convert;$If;6| (|s| $)
  (SPADCALL |s| (|getShellEntry| $ 45))) 

(DEFUN |SYMBOL;convert;2$;7| (|s| $) (DECLARE (IGNORE $)) |s|) 

(DEFUN |SYMBOL;coerce;S$;8| (|s| $) (VALUES (INTERN |s|))) 

(DEFUN |SYMBOL;=;2$B;9| (|x| |y| $)
  (DECLARE (IGNORE $))
  (EQUAL |x| |y|)) 

(DEFUN |SYMBOL;<;2$B;10| (|x| |y| $)
  (DECLARE (IGNORE $))
  (GGREATERP |y| |x|)) 

(DEFUN |SYMBOL;coerce;$Of;11| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 52))) 

(DEFUN |SYMBOL;subscript;$L$;12| (|sy| |lx| $)
  (|SYMBOL;script;$L$;22| |sy| (LIST |lx| NIL NIL NIL NIL) $)) 

(DEFUN |SYMBOL;elt;$L$;13| (|sy| |lx| $)
  (|SYMBOL;subscript;$L$;12| |sy| |lx| $)) 

(DEFUN |SYMBOL;superscript;$L$;14| (|sy| |lx| $)
  (|SYMBOL;script;$L$;22| |sy| (LIST NIL |lx| NIL NIL NIL) $)) 

(DEFUN |SYMBOL;argscript;$L$;15| (|sy| |lx| $)
  (|SYMBOL;script;$L$;22| |sy| (LIST NIL NIL NIL NIL |lx|) $)) 

(DEFUN |SYMBOL;patternMatch;$P2Pmr;16| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 64))) 

(DEFUN |SYMBOL;patternMatch;$P2Pmr;17| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 71))) 

(DEFUN |SYMBOL;convert;$P;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 74))) 

(DEFUN |SYMBOL;convert;$P;19| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 76))) 

(DEFUN |SYMBOL;syprefix| (|sc| $)
  (PROG (|ns| #0=#:G1548 |n| #1=#:G1549)
    (RETURN
      (SEQ (LETT |ns|
                 (LIST (LENGTH (QVELT |sc| 3)) (LENGTH (QVELT |sc| 2))
                       (LENGTH (QVELT |sc| 1)) (LENGTH (QVELT |sc| 0)))
                 |SYMBOL;syprefix|)
           (SEQ G190
                (COND
                  ((NULL (COND
                           ((< (LENGTH |ns|) 2) 'NIL)
                           ('T (ZEROP (|SPADfirst| |ns|)))))
                   (GO G191)))
                (SEQ (EXIT (LETT |ns| (CDR |ns|) |SYMBOL;syprefix|)))
                NIL (GO G190) G191 (EXIT NIL))
           (EXIT (SPADCALL
                     (CONS (STRCONC (|getShellEntry| $ 37)
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
                     (|getShellEntry| $ 79))))))) 

(DEFUN |SYMBOL;syscripts| (|sc| $)
  (PROG (|all|)
    (RETURN
      (SEQ (LETT |all| (QVELT |sc| 3) |SYMBOL;syscripts|)
           (LETT |all|
                 (SPADCALL (QVELT |sc| 2) |all| (|getShellEntry| $ 80))
                 |SYMBOL;syscripts|)
           (LETT |all|
                 (SPADCALL (QVELT |sc| 1) |all| (|getShellEntry| $ 80))
                 |SYMBOL;syscripts|)
           (LETT |all|
                 (SPADCALL (QVELT |sc| 0) |all| (|getShellEntry| $ 80))
                 |SYMBOL;syscripts|)
           (EXIT (SPADCALL |all| (QVELT |sc| 4) (|getShellEntry| $ 80))))))) 

(DEFUN |SYMBOL;script;$L$;22| (|sy| |ls| $)
  (PROG (|sc|)
    (RETURN
      (SEQ (LETT |sc| (VECTOR NIL NIL NIL NIL NIL)
                 |SYMBOL;script;$L$;22|)
           (COND
             ((NULL (NULL |ls|))
              (SEQ (QSETVELT |sc| 0 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NULL (NULL |ls|))
              (SEQ (QSETVELT |sc| 1 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NULL (NULL |ls|))
              (SEQ (QSETVELT |sc| 2 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NULL (NULL |ls|))
              (SEQ (QSETVELT |sc| 3 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (COND
             ((NULL (NULL |ls|))
              (SEQ (QSETVELT |sc| 4 (|SPADfirst| |ls|))
                   (EXIT (LETT |ls| (CDR |ls|) |SYMBOL;script;$L$;22|)))))
           (EXIT (|SYMBOL;script;$R$;23| |sy| |sc| $)))))) 

(DEFUN |SYMBOL;script;$R$;23| (|sy| |sc| $)
  (COND
    ((SPADCALL |sy| (|getShellEntry| $ 22))
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
    ((NULL (SPADCALL |e| (|getShellEntry| $ 22))) (PNAME |e|))
    ('T (|error| "Cannot form string from non-atomic symbols.")))) 

(DEFUN |SYMBOL;latex;$S;25| (|e| $)
  (PROG (|ss| |lo| |sc| |s|)
    (RETURN
      (SEQ (LETT |s| (PNAME (|SYMBOL;name;2$;31| |e| $))
                 |SYMBOL;latex;$S;25|)
           (COND
             ((< 1 (QCSIZE |s|))
              (COND
                ((SPADCALL (SPADCALL |s| 1 (|getShellEntry| $ 85))
                     (SPADCALL "\\" (|getShellEntry| $ 40))
                     (|getShellEntry| $ 86))
                 (LETT |s| (STRCONC "\\mbox{\\it " (STRCONC |s| "}"))
                       |SYMBOL;latex;$S;25|)))))
           (COND
             ((NULL (SPADCALL |e| (|getShellEntry| $ 22))) (EXIT |s|)))
           (LETT |ss| (|SYMBOL;scripts;$R;32| |e| $)
                 |SYMBOL;latex;$S;25|)
           (LETT |lo| (QVELT |ss| 0) |SYMBOL;latex;$S;25|)
           (COND
             ((NULL (NULL |lo|))
              (SEQ (LETT |sc| "_{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL |lo|)
                                     (|getShellEntry| $ 88)))
                           (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 89)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NULL (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |s| |sc|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 1) |SYMBOL;latex;$S;25|)
           (COND
             ((NULL (NULL |lo|))
              (SEQ (LETT |sc| "^{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL |lo|)
                                     (|getShellEntry| $ 88)))
                           (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 89)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NULL (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |s| |sc|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 2) |SYMBOL;latex;$S;25|)
           (COND
             ((NULL (NULL |lo|))
              (SEQ (LETT |sc| "{}^{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL |lo|)
                                     (|getShellEntry| $ 88)))
                           (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 89)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NULL (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |sc| |s|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 3) |SYMBOL;latex;$S;25|)
           (COND
             ((NULL (NULL |lo|))
              (SEQ (LETT |sc| "{}_{" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL |lo|)
                                     (|getShellEntry| $ 88)))
                           (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 89)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NULL (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "}") |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |sc| |s|)
                               |SYMBOL;latex;$S;25|)))))
           (LETT |lo| (QVELT |ss| 4) |SYMBOL;latex;$S;25|)
           (COND
             ((NULL (NULL |lo|))
              (SEQ (LETT |sc| "\\left( {" |SYMBOL;latex;$S;25|)
                   (SEQ G190
                        (COND
                          ((NULL (SPADCALL (NULL |lo|)
                                     (|getShellEntry| $ 88)))
                           (GO G191)))
                        (SEQ (LETT |sc|
                                   (STRCONC |sc|
                                    (SPADCALL (|SPADfirst| |lo|)
                                     (|getShellEntry| $ 89)))
                                   |SYMBOL;latex;$S;25|)
                             (LETT |lo| (CDR |lo|)
                                   |SYMBOL;latex;$S;25|)
                             (EXIT (COND
                                     ((NULL (NULL |lo|))
                                      (LETT |sc| (STRCONC |sc| ", ")
                                       |SYMBOL;latex;$S;25|)))))
                        NIL (GO G190) G191 (EXIT NIL))
                   (LETT |sc| (STRCONC |sc| "} \\right)")
                         |SYMBOL;latex;$S;25|)
                   (EXIT (LETT |s| (STRCONC |s| |sc|)
                               |SYMBOL;latex;$S;25|)))))
           (EXIT |s|))))) 

(DEFUN |SYMBOL;anyRadix| (|n| |s| $)
  (PROG (|qr| |ns| #0=#:G1503)
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
                                           (|getShellEntry| $ 91)))
                                         (|getShellEntry| $ 85))
                                        |ns| (|getShellEntry| $ 92))
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
                     (SPADCALL (|getShellEntry| $ 9)
                         (|getShellEntry| $ 93))
                     (|getShellEntry| $ 19) $)
                 |SYMBOL;new;$;27|)
           (SPADCALL (|getShellEntry| $ 9)
               (+ (SPADCALL (|getShellEntry| $ 9)
                      (|getShellEntry| $ 93))
                  1)
               (|getShellEntry| $ 94))
           (EXIT (|SYMBOL;coerce;S$;8| (STRCONC "%" |sym|) $)))))) 

(DEFUN |SYMBOL;new;2$;28| (|x| $)
  (PROG (|u| |n| |xx|)
    (RETURN
      (SEQ (LETT |n|
                 (SEQ (LETT |u|
                            (SPADCALL |x| (|getShellEntry| $ 12)
                                (|getShellEntry| $ 97))
                            |SYMBOL;new;2$;28|)
                      (EXIT (COND
                              ((QEQCAR |u| 1) 0)
                              ('T (+ (QCDR |u|) 1)))))
                 |SYMBOL;new;2$;28|)
           (SPADCALL (|getShellEntry| $ 12) |x| |n|
               (|getShellEntry| $ 98))
           (LETT |xx|
                 (COND
                   ((NULL (SPADCALL |x| (|getShellEntry| $ 22)))
                    (|SYMBOL;string;$S;24| |x| $))
                   ('T
                    (|SYMBOL;string;$S;24| (|SYMBOL;name;2$;31| |x| $)
                        $)))
                 |SYMBOL;new;2$;28|)
           (LETT |xx| (STRCONC "%" |xx|) |SYMBOL;new;2$;28|)
           (LETT |xx|
                 (COND
                   ((NULL (< (SPADCALL
                                 (SPADCALL |xx|
                                     (SPADCALL |xx|
                                      (|getShellEntry| $ 99))
                                     (|getShellEntry| $ 85))
                                 (|getShellEntry| $ 18)
                                 (|getShellEntry| $ 100))
                             (SPADCALL (|getShellEntry| $ 18)
                                 (|getShellEntry| $ 91))))
                    (STRCONC |xx|
                             (|SYMBOL;anyRadix| |n|
                                 (|getShellEntry| $ 20) $)))
                   ('T
                    (STRCONC |xx|
                             (|SYMBOL;anyRadix| |n|
                                 (|getShellEntry| $ 18) $))))
                 |SYMBOL;new;2$;28|)
           (COND
             ((NULL (SPADCALL |x| (|getShellEntry| $ 22)))
              (EXIT (|SYMBOL;coerce;S$;8| |xx| $))))
           (EXIT (|SYMBOL;script;$R$;23| (|SYMBOL;coerce;S$;8| |xx| $)
                     (|SYMBOL;scripts;$R;32| |x| $) $)))))) 

(DEFUN |SYMBOL;resetNew;V;29| ($)
  (PROG (|k| #0=#:G1550)
    (RETURN
      (SEQ (SPADCALL (|getShellEntry| $ 9) 0 (|getShellEntry| $ 94))
           (SEQ (LETT |k| NIL |SYMBOL;resetNew;V;29|)
                (LETT #0#
                      (SPADCALL (|getShellEntry| $ 12)
                          (|getShellEntry| $ 103))
                      |SYMBOL;resetNew;V;29|)
                G190
                (COND
                  ((OR (ATOM #0#)
                       (PROGN
                         (LETT |k| (CAR #0#) |SYMBOL;resetNew;V;29|)
                         NIL))
                   (GO G191)))
                (SEQ (EXIT (SPADCALL |k| (|getShellEntry| $ 12)
                               (|getShellEntry| $ 104))))
                (LETT #0# (CDR #0#) |SYMBOL;resetNew;V;29|) (GO G190)
                G191 (EXIT NIL))
           (EXIT (SPADCALL (|getShellEntry| $ 105))))))) 

(DEFUN |SYMBOL;scripted?;$B;30| (|sy| $)
  (SPADCALL (ATOM |sy|) (|getShellEntry| $ 88))) 

(DEFUN |SYMBOL;name;2$;31| (|sy| $)
  (PROG (|str| |i| #0=#:G1551 #1=#:G1531 #2=#:G1529)
    (RETURN
      (SEQ (EXIT (COND
                   ((NULL (SPADCALL |sy| (|getShellEntry| $ 22))) |sy|)
                   ('T
                    (SEQ (LETT |str|
                               (|SYMBOL;string;$S;24|
                                   (SPADCALL
                                    (|SYMBOL;list;$L;34| |sy| $)
                                    (|getShellEntry| $ 108))
                                   $)
                               |SYMBOL;name;2$;31|)
                         (SEQ (EXIT (SEQ
                                     (LETT |i|
                                      (+ (|getShellEntry| $ 38) 1)
                                      |SYMBOL;name;2$;31|)
                                     (LETT #0# (QCSIZE |str|)
                                      |SYMBOL;name;2$;31|)
                                     G190
                                     (COND ((> |i| #0#) (GO G191)))
                                     (SEQ
                                      (EXIT
                                       (COND
                                         ((NULL
                                           (SPADCALL
                                            (SPADCALL |str| |i|
                                             (|getShellEntry| $ 85))
                                            (|getShellEntry| $ 109)))
                                          (PROGN
                                            (LETT #2#
                                             (PROGN
                                               (LETT #1#
                                                (|SYMBOL;coerce;S$;8|
                                                 (SPADCALL |str|
                                                  (SPADCALL |i|
                                                   (QCSIZE |str|)
                                                   (|getShellEntry| $
                                                    111))
                                                  (|getShellEntry| $
                                                   112))
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
  (PROG (|lscripts| |str| |nstr| |j| #0=#:G1534 |nscripts| |m| |n|
            #1=#:G1552 |i| #2=#:G1553 |a| #3=#:G1554 |allscripts|)
    (RETURN
      (SEQ (COND
             ((NULL (SPADCALL |sy| (|getShellEntry| $ 22)))
              (VECTOR NIL NIL NIL NIL NIL))
             ('T
              (SEQ (LETT |nscripts| (LIST 0 0 0 0 0)
                         |SYMBOL;scripts;$R;32|)
                   (LETT |lscripts| (LIST NIL NIL NIL NIL NIL)
                         |SYMBOL;scripts;$R;32|)
                   (LETT |str|
                         (|SYMBOL;string;$S;24|
                             (SPADCALL (|SYMBOL;list;$L;34| |sy| $)
                                 (|getShellEntry| $ 108))
                             $)
                         |SYMBOL;scripts;$R;32|)
                   (LETT |nstr| (QCSIZE |str|) |SYMBOL;scripts;$R;32|)
                   (LETT |m|
                         (SPADCALL |nscripts| (|getShellEntry| $ 114))
                         |SYMBOL;scripts;$R;32|)
                   (SEQ (LETT |j| (+ (|getShellEntry| $ 38) 1)
                              |SYMBOL;scripts;$R;32|)
                        (LETT |i| |m| |SYMBOL;scripts;$R;32|) G190
                        (COND
                          ((OR (> |j| |nstr|)
                               (NULL (SPADCALL
                                      (SPADCALL |str| |j|
                                       (|getShellEntry| $ 85))
                                      (|getShellEntry| $ 109))))
                           (GO G191)))
                        (SEQ (EXIT (SPADCALL |nscripts| |i|
                                    (PROG1
                                     (LETT #0#
                                      (-
                                       (SPADCALL
                                        (SPADCALL |str| |j|
                                         (|getShellEntry| $ 85))
                                        (|getShellEntry| $ 42))
                                       (|getShellEntry| $ 43))
                                      |SYMBOL;scripts;$R;32|)
                                      (|check-subtype| (>= #0# 0)
                                       '(|NonNegativeInteger|) #0#))
                                    (|getShellEntry| $ 115))))
                        (LETT |i|
                              (PROG1 (+ |i| 1)
                                (LETT |j| (+ |j| 1)
                                      |SYMBOL;scripts;$R;32|))
                              |SYMBOL;scripts;$R;32|)
                        (GO G190) G191 (EXIT NIL))
                   (LETT |nscripts|
                         (SPADCALL (CDR |nscripts|)
                             (|SPADfirst| |nscripts|)
                             (|getShellEntry| $ 116))
                         |SYMBOL;scripts;$R;32|)
                   (LETT |allscripts|
                         (SPADCALL (|SYMBOL;list;$L;34| |sy| $)
                             (|getShellEntry| $ 117))
                         |SYMBOL;scripts;$R;32|)
                   (LETT |m|
                         (SPADCALL |lscripts| (|getShellEntry| $ 118))
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
                        (SEQ (EXIT (COND
                                     ((<
                                       (SPADCALL |allscripts|
                                        (|getShellEntry| $ 119))
                                       |n|)
                                      (|error|
                                       "Improper script count in symbol"))
                                     ('T
                                      (SEQ
                                       (SPADCALL |lscripts| |i|
                                        (PROGN
                                          (LETT #2# NIL
                                           |SYMBOL;scripts;$R;32|)
                                          (SEQ
                                           (LETT |a| NIL
                                            |SYMBOL;scripts;$R;32|)
                                           (LETT #3#
                                            (SPADCALL |allscripts| |n|
                                             (|getShellEntry| $ 120))
                                            |SYMBOL;scripts;$R;32|)
                                           G190
                                           (COND
                                             ((OR (ATOM #3#)
                                               (PROGN
                                                 (LETT |a| (CAR #3#)
                                                  |SYMBOL;scripts;$R;32|)
                                                 NIL))
                                              (GO G191)))
                                           (SEQ
                                            (EXIT
                                             (LETT #2#
                                              (CONS
                                               (|SYMBOL;coerce;$Of;11|
                                                |a| $)
                                               #2#)
                                              |SYMBOL;scripts;$R;32|)))
                                           (LETT #3# (CDR #3#)
                                            |SYMBOL;scripts;$R;32|)
                                           (GO G190) G191
                                           (EXIT (NREVERSE0 #2#))))
                                        (|getShellEntry| $ 121))
                                       (EXIT
                                        (LETT |allscripts|
                                         (SPADCALL |allscripts| |n|
                                          (|getShellEntry| $ 122))
                                         |SYMBOL;scripts;$R;32|)))))))
                        (LETT |i|
                              (PROG1 (+ |i| 1)
                                (LETT #1# (CDR #1#)
                                      |SYMBOL;scripts;$R;32|))
                              |SYMBOL;scripts;$R;32|)
                        (GO G190) G191 (EXIT NIL))
                   (EXIT (VECTOR (SPADCALL |lscripts| |m|
                                     (|getShellEntry| $ 123))
                                 (SPADCALL |lscripts| (+ |m| 1)
                                     (|getShellEntry| $ 123))
                                 (SPADCALL |lscripts| (+ |m| 2)
                                     (|getShellEntry| $ 123))
                                 (SPADCALL |lscripts| (+ |m| 3)
                                     (|getShellEntry| $ 123))
                                 (SPADCALL |lscripts| (+ |m| 4)
                                     (|getShellEntry| $ 123))))))))))) 

(DEFUN |SYMBOL;istring| (|n| $)
  (COND
    ((< 9 |n|) (|error| "Can have at most 9 scripts of each kind"))
    ('T (|getSimpleArrayEntry| (|getShellEntry| $ 17) (+ |n| 0))))) 

(DEFUN |SYMBOL;list;$L;34| (|sy| $)
  (COND
    ((NULL (SPADCALL |sy| (|getShellEntry| $ 22)))
     (|error| "Cannot convert a symbol to a list if it is not subscripted"))
    ('T |sy|))) 

(DEFUN |SYMBOL;sample;$;35| ($) (|SYMBOL;coerce;S$;8| "aSymbol" $)) 

(DEFUN |Symbol| ()
  (PROG ()
    (RETURN
      (PROG (#0=#:G1556)
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
        (LETT $ (|newShell| 126) . #0#)
        (|setShellEntry| $ 0 |dv$|)
        (|setShellEntry| $ 3
            (LETT |pv$| (|buildPredVector| 0 0 NIL) . #0#))
        (|haddProp| |$ConstructorCache| '|Symbol| NIL (CONS 1 $))
        (|stuffDomainSlots| $)
        (|setShellEntry| $ 9 (SPADCALL 0 (|getShellEntry| $ 8)))
        (|setShellEntry| $ 12 (SPADCALL (|getShellEntry| $ 11)))
        (|setShellEntry| $ 17
            (SPADCALL (LIST "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
                (|getShellEntry| $ 16)))
        (|setShellEntry| $ 18 "0123456789")
        (|setShellEntry| $ 19 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (|setShellEntry| $ 20 "abcdefghijklmnopqrstuvwxyz")
        (|setShellEntry| $ 37 "*")
        (|setShellEntry| $ 38 (QCSIZE (|getShellEntry| $ 37)))
        (|setShellEntry| $ 43
            (SPADCALL (SPADCALL "0" (|getShellEntry| $ 40))
                (|getShellEntry| $ 42)))
        $)))) 

(MAKEPROP '|Symbol| '|infovec|
    (LIST '#(NIL NIL NIL NIL NIL NIL (|Integer|) (|Reference| 6)
             (0 . |ref|) '|count| (|AssociationList| $$ 6)
             (5 . |empty|) '|xcount| (|String|) (|List| 13)
             (|PrimitiveArray| 13) (9 . |construct|) '|istrings|
             '|nums| 'ALPHAS '|alphas| (|Boolean|)
             |SYMBOL;scripted?;$B;30| (|Void|) (|Symbol|)
             (|OpenMathDevice|) (14 . |OMputVariable|)
             (|OpenMathEncoding|) (20 . |OMencodingXML|)
             (24 . |OMopenString|) (30 . |OMputObject|)
             (35 . |OMputEndObject|) (40 . |OMclose|)
             |SYMBOL;OMwrite;$S;2| |SYMBOL;OMwrite;$BS;3|
             |SYMBOL;OMwrite;Omd$V;4| |SYMBOL;OMwrite;Omd$BV;5| '|hd|
             '|lhd| (|Character|) (45 . |char|) (|NonNegativeInteger|)
             (50 . |ord|) '|ord0| (|InputForm|) (55 . |convert|)
             |SYMBOL;convert;$If;6| |SYMBOL;convert;2$;7|
             |SYMBOL;coerce;S$;8| |SYMBOL;=;2$B;9| |SYMBOL;<;2$B;10|
             (|OutputForm|) (60 . |outputForm|) |SYMBOL;coerce;$Of;11|
             (|List| 51) (|List| 54) |SYMBOL;script;$L$;22|
             |SYMBOL;subscript;$L$;12| |SYMBOL;elt;$L$;13|
             |SYMBOL;superscript;$L$;14| |SYMBOL;argscript;$L$;15|
             (|PatternMatchResult| 6 24) (|Pattern| 6)
             (|PatternMatchSymbol| 6) (65 . |patternMatch|)
             (|PatternMatchResult| 6 $) |SYMBOL;patternMatch;$P2Pmr;16|
             (|Float|) (|PatternMatchResult| 67 24) (|Pattern| 67)
             (|PatternMatchSymbol| 67) (72 . |patternMatch|)
             (|PatternMatchResult| 67 $)
             |SYMBOL;patternMatch;$P2Pmr;17| (79 . |coerce|)
             |SYMBOL;convert;$P;18| (84 . |coerce|)
             |SYMBOL;convert;$P;19| (|List| $) (89 . |concat|)
             (94 . |concat|)
             (|Record| (|:| |sub| 54) (|:| |sup| 54) (|:| |presup| 54)
                 (|:| |presub| 54) (|:| |args| 54))
             |SYMBOL;script;$R$;23| |SYMBOL;name;2$;31|
             |SYMBOL;string;$S;24| (100 . |elt|) (106 . ~=)
             |SYMBOL;scripts;$R;32| (112 . |not|) (117 . |latex|)
             |SYMBOL;latex;$S;25| (122 . |minIndex|) (127 . |concat|)
             (133 . |elt|) (138 . |setelt|) |SYMBOL;new;$;27|
             (|Union| 6 '"failed") (144 . |search|) (150 . |setelt|)
             (157 . |maxIndex|) (162 . |position|) |SYMBOL;new;2$;28|
             (|List| $$) (168 . |keys|) (173 . |remove!|)
             (179 . |void|) |SYMBOL;resetNew;V;29| |SYMBOL;list;$L;34|
             (183 . |first|) (188 . |digit?|) (|UniversalSegment| 6)
             (193 . SEGMENT) (199 . |elt|) (|List| 41)
             (205 . |minIndex|) (210 . |setelt|) (217 . |concat|)
             (223 . |rest|) (228 . |minIndex|) (233 . |#|)
             (238 . |first|) (244 . |setelt|) (251 . |rest|)
             (257 . |elt|)
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SYMBOL;sample;$;35|)
                            $))
             (|SingleInteger|))
          '#(~= 263 |superscript| 269 |subscript| 275 |string| 281
             |scripts| 286 |scripted?| 291 |script| 296 |sample| 308
             |resetNew| 312 |patternMatch| 316 |new| 330 |name| 339
             |min| 344 |max| 350 |list| 356 |latex| 361 |hash| 366
             |elt| 371 |convert| 377 |coerce| 397 |argscript| 407
             |OMwrite| 413 >= 437 > 443 = 449 <= 455 < 461)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(|OrderedSet&| NIL NIL |SetCategory&|
                         |BasicType&| NIL NIL NIL NIL NIL NIL)
                      (CONS '#((|OrderedSet|) (|PatternMatchable| 67)
                               (|PatternMatchable| 6) (|SetCategory|)
                               (|BasicType|) (|ConvertibleTo| 69)
                               (|ConvertibleTo| 62)
                               (|ConvertibleTo| 24) (|OpenMath|)
                               (|ConvertibleTo| 44) (|CoercibleTo| 51))
                            (|makeByteWordVec2| 125
                                '(1 7 0 6 8 0 10 0 11 1 15 0 14 16 2 25
                                  23 0 24 26 0 27 0 28 2 25 0 13 27 29
                                  1 25 23 0 30 1 25 23 0 31 1 25 23 0
                                  32 1 39 0 13 40 1 39 41 0 42 1 44 0
                                  24 45 1 51 0 24 52 3 63 61 24 62 61
                                  64 3 70 68 24 69 68 71 1 69 0 24 74 1
                                  62 0 24 76 1 13 0 78 79 2 54 0 0 0 80
                                  2 13 39 0 6 85 2 39 21 0 0 86 1 21 0
                                  0 88 1 51 13 0 89 1 13 6 0 91 2 13 0
                                  39 0 92 1 7 6 0 93 2 7 6 0 6 94 2 10
                                  96 2 0 97 3 10 6 0 2 6 98 1 13 6 0 99
                                  2 13 6 39 0 100 1 10 102 0 103 2 10
                                  96 2 0 104 0 23 0 105 1 102 2 0 108 1
                                  39 21 0 109 2 110 0 6 6 111 2 13 0 0
                                  110 112 1 113 6 0 114 3 113 41 0 6 41
                                  115 2 113 0 0 41 116 1 102 0 0 117 1
                                  55 6 0 118 1 102 41 0 119 2 102 0 0
                                  41 120 3 55 54 0 6 54 121 2 102 0 0
                                  41 122 2 55 54 0 6 123 2 0 21 0 0 1 2
                                  0 0 0 54 59 2 0 0 0 54 57 1 0 13 0 84
                                  1 0 81 0 87 1 0 21 0 22 2 0 0 0 55 56
                                  2 0 0 0 81 82 0 0 0 124 0 0 23 106 3
                                  0 65 0 62 65 66 3 0 72 0 69 72 73 1 0
                                  0 0 101 0 0 0 95 1 0 0 0 83 2 0 0 0 0
                                  1 2 0 0 0 0 1 1 0 78 0 107 1 0 13 0
                                  90 1 0 125 0 1 2 0 0 0 54 58 1 0 62 0
                                  77 1 0 69 0 75 1 0 24 0 47 1 0 44 0
                                  46 1 0 0 13 48 1 0 51 0 53 2 0 0 0 54
                                  60 3 0 23 25 0 21 36 2 0 13 0 21 34 2
                                  0 23 25 0 35 1 0 13 0 33 2 0 21 0 0 1
                                  2 0 21 0 0 1 2 0 21 0 0 49 2 0 21 0 0
                                  1 2 0 21 0 0 50)))))
          '|lookupComplete|)) 

(SETQ |$CategoryFrame|
      (|put| '|Symbol| '|isFunctor|
             '(((|sample| ($)) T (CONST $ 124))
               ((|list| ((|List| $) $)) T (ELT $ 107))
               ((|string| ((|String|) $)) T (ELT $ 84))
               ((|elt| ($ $ (|List| (|OutputForm|)))) T (ELT $ 58))
               ((|argscript| ($ $ (|List| (|OutputForm|)))) T
                (ELT $ 60))
               ((|superscript| ($ $ (|List| (|OutputForm|)))) T
                (ELT $ 59))
               ((|subscript| ($ $ (|List| (|OutputForm|)))) T
                (ELT $ 57))
               ((|script|
                    ($ $
                       (|Record| (|:| |sub| (|List| (|OutputForm|)))
                           (|:| |sup| (|List| (|OutputForm|)))
                           (|:| |presup| (|List| (|OutputForm|)))
                           (|:| |presub| (|List| (|OutputForm|)))
                           (|:| |args| (|List| (|OutputForm|))))))
                T (ELT $ 82))
               ((|script| ($ $ (|List| (|List| (|OutputForm|))))) T
                (ELT $ 56))
               ((|scripts|
                    ((|Record| (|:| |sub| (|List| (|OutputForm|)))
                         (|:| |sup| (|List| (|OutputForm|)))
                         (|:| |presup| (|List| (|OutputForm|)))
                         (|:| |presub| (|List| (|OutputForm|)))
                         (|:| |args| (|List| (|OutputForm|))))
                     $))
                T (ELT $ 87))
               ((|scripted?| ((|Boolean|) $)) T (ELT $ 22))
               ((|name| ($ $)) T (ELT $ 83))
               ((|coerce| ($ (|String|))) T (ELT $ 48))
               ((|resetNew| ((|Void|))) T (ELT $ 106))
               ((|new| ($ $)) T (ELT $ 101)) ((|new| ($)) T (ELT $ 95))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Float|) $) $
                     (|Pattern| (|Float|))
                     (|PatternMatchResult| (|Float|) $)))
                T (ELT $ 73))
               ((|patternMatch|
                    ((|PatternMatchResult| (|Integer|) $) $
                     (|Pattern| (|Integer|))
                     (|PatternMatchResult| (|Integer|) $)))
                T (ELT $ 66))
               ((|convert| ((|Pattern| (|Float|)) $)) T (ELT $ 75))
               ((|convert| ((|Pattern| (|Integer|)) $)) T (ELT $ 77))
               ((|convert| ((|Symbol|) $)) T (ELT $ 47))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $ (|Boolean|)))
                T (ELT $ 36))
               ((|OMwrite| ((|Void|) (|OpenMathDevice|) $)) T
                (ELT $ 35))
               ((|OMwrite| ((|String|) $ (|Boolean|))) T (ELT $ 34))
               ((|OMwrite| ((|String|) $)) T (ELT $ 33))
               ((|convert| ((|InputForm|) $)) T (ELT $ 46))
               ((|min| ($ $ $)) T (ELT $ NIL))
               ((|max| ($ $ $)) T (ELT $ NIL))
               ((<= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((>= ((|Boolean|) $ $)) T (ELT $ NIL))
               ((> ((|Boolean|) $ $)) T (ELT $ NIL))
               ((< ((|Boolean|) $ $)) T (ELT $ 50))
               ((|latex| ((|String|) $)) T (ELT $ 90))
               ((|hash| ((|SingleInteger|) $)) T (ELT $ NIL))
               ((|coerce| ((|OutputForm|) $)) T (ELT $ 53))
               ((= ((|Boolean|) $ $)) T (ELT $ 49))
               ((~= ((|Boolean|) $ $)) T (ELT $ NIL)))
             (|addModemap| '|Symbol| '(|Symbol|)
                 '((|Join| (|OrderedSet|)
                           (|ConvertibleTo| (|InputForm|)) (|OpenMath|)
                           (|ConvertibleTo| (|Symbol|))
                           (|ConvertibleTo| (|Pattern| (|Integer|)))
                           (|ConvertibleTo| (|Pattern| (|Float|)))
                           (|PatternMatchable| (|Integer|))
                           (|PatternMatchable| (|Float|))
                           (CATEGORY |domain| (SIGNATURE |new| ($))
                               (SIGNATURE |new| ($ $))
                               (SIGNATURE |resetNew| ((|Void|)))
                               (SIGNATURE |coerce| ($ (|String|)))
                               (SIGNATURE |name| ($ $))
                               (SIGNATURE |scripted?| ((|Boolean|) $))
                               (SIGNATURE |scripts|
                                   ((|Record|
                                     (|:| |sub|
                                      (|List| (|OutputForm|)))
                                     (|:| |sup|
                                      (|List| (|OutputForm|)))
                                     (|:| |presup|
                                      (|List| (|OutputForm|)))
                                     (|:| |presub|
                                      (|List| (|OutputForm|)))
                                     (|:| |args|
                                      (|List| (|OutputForm|))))
                                    $))
                               (SIGNATURE |script|
                                   ($ $
                                    (|List| (|List| (|OutputForm|)))))
                               (SIGNATURE |script|
                                   ($ $
                                    (|Record|
                                     (|:| |sub|
                                      (|List| (|OutputForm|)))
                                     (|:| |sup|
                                      (|List| (|OutputForm|)))
                                     (|:| |presup|
                                      (|List| (|OutputForm|)))
                                     (|:| |presub|
                                      (|List| (|OutputForm|)))
                                     (|:| |args|
                                      (|List| (|OutputForm|))))))
                               (SIGNATURE |subscript|
                                   ($ $ (|List| (|OutputForm|))))
                               (SIGNATURE |superscript|
                                   ($ $ (|List| (|OutputForm|))))
                               (SIGNATURE |argscript|
                                   ($ $ (|List| (|OutputForm|))))
                               (SIGNATURE |elt|
                                   ($ $ (|List| (|OutputForm|))))
                               (SIGNATURE |string| ((|String|) $))
                               (SIGNATURE |list| ((|List| $) $))
                               (SIGNATURE |sample| ($) |constant|))))
                 T '|Symbol|
                 (|put| '|Symbol| '|mode|
                        '(|Mapping|
                             (|Join| (|OrderedSet|)
                                     (|ConvertibleTo| (|InputForm|))
                                     (|OpenMath|)
                                     (|ConvertibleTo| (|Symbol|))
                                     (|ConvertibleTo|
                                      (|Pattern| (|Integer|)))
                                     (|ConvertibleTo|
                                      (|Pattern| (|Float|)))
                                     (|PatternMatchable| (|Integer|))
                                     (|PatternMatchable| (|Float|))
                                     (CATEGORY |domain|
                                      (SIGNATURE |new| ($))
                                      (SIGNATURE |new| ($ $))
                                      (SIGNATURE |resetNew| ((|Void|)))
                                      (SIGNATURE |coerce|
                                       ($ (|String|)))
                                      (SIGNATURE |name| ($ $))
                                      (SIGNATURE |scripted?|
                                       ((|Boolean|) $))
                                      (SIGNATURE |scripts|
                                       ((|Record|
                                         (|:| |sub|
                                          (|List| (|OutputForm|)))
                                         (|:| |sup|
                                          (|List| (|OutputForm|)))
                                         (|:| |presup|
                                          (|List| (|OutputForm|)))
                                         (|:| |presub|
                                          (|List| (|OutputForm|)))
                                         (|:| |args|
                                          (|List| (|OutputForm|))))
                                        $))
                                      (SIGNATURE |script|
                                       ($ $
                                        (|List|
                                         (|List| (|OutputForm|)))))
                                      (SIGNATURE |script|
                                       ($ $
                                        (|Record|
                                         (|:| |sub|
                                          (|List| (|OutputForm|)))
                                         (|:| |sup|
                                          (|List| (|OutputForm|)))
                                         (|:| |presup|
                                          (|List| (|OutputForm|)))
                                         (|:| |presub|
                                          (|List| (|OutputForm|)))
                                         (|:| |args|
                                          (|List| (|OutputForm|))))))
                                      (SIGNATURE |subscript|
                                       ($ $ (|List| (|OutputForm|))))
                                      (SIGNATURE |superscript|
                                       ($ $ (|List| (|OutputForm|))))
                                      (SIGNATURE |argscript|
                                       ($ $ (|List| (|OutputForm|))))
                                      (SIGNATURE |elt|
                                       ($ $ (|List| (|OutputForm|))))
                                      (SIGNATURE |string|
                                       ((|String|) $))
                                      (SIGNATURE |list| ((|List| $) $))
                                      (SIGNATURE |sample| ($)
                                       |constant|))))
                        |$CategoryFrame|)))) 

(MAKEPROP '|Symbol| 'NILADIC T) 
