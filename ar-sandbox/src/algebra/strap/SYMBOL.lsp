
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
    (T (SPADCALL |dev| |x| (|getShellEntry| $ 27))))) 

(DEFUN |SYMBOL;OMwrite;$S;2| (|x| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 29))
                    (|getShellEntry| $ 30))))
    (SEQ (SPADCALL |dev| (|getShellEntry| $ 31))
         (|SYMBOL;writeOMSym| |dev| |x| $)
         (SPADCALL |dev| (|getShellEntry| $ 32))
         (SPADCALL |dev| (|getShellEntry| $ 33))
         (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|)))) 

(DEFUN |SYMBOL;OMwrite;$BS;3| (|x| |wholeObj| $)
  (LET* ((|s| "") (|sp| (OM-STRINGTOSTRINGPTR |s|))
         (|dev| (SPADCALL |sp| (SPADCALL (|getShellEntry| $ 29))
                    (|getShellEntry| $ 30))))
    (SEQ (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 31))))
         (|SYMBOL;writeOMSym| |dev| |x| $)
         (COND (|wholeObj| (SPADCALL |dev| (|getShellEntry| $ 32))))
         (SPADCALL |dev| (|getShellEntry| $ 33))
         (SETQ |s| (OM-STRINGPTRTOSTRING |sp|)) (EXIT |s|)))) 

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
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 67))) 

(DEFUN |SYMBOL;patternMatch;$P2Pmr;17| (|x| |p| |l| $)
  (SPADCALL |x| |p| |l| (|getShellEntry| $ 74))) 

(DEFUN |SYMBOL;convert;$P;18| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 77))) 

(DEFUN |SYMBOL;convert;$P;19| (|x| $)
  (SPADCALL |x| (|getShellEntry| $ 79))) 

(DEFUN |SYMBOL;syprefix| (|sc| $)
  (LET ((|ns| (LIST (LENGTH (QVELT |sc| 3)) (LENGTH (QVELT |sc| 2))
                    (LENGTH (QVELT |sc| 1)) (LENGTH (QVELT |sc| 0)))))
    (SEQ (LOOP
           (COND
             ((NOT (COND
                     ((NOT (< (LENGTH |ns|) 2))
                      (ZEROP (|SPADfirst| |ns|)))
                     (T NIL)))
              (RETURN NIL))
             (T (SETQ |ns| (CDR |ns|)))))
         (EXIT (SPADCALL
                   (CONS (STRCONC (|getShellEntry| $ 38)
                                  (|SYMBOL;istring|
                                      (LENGTH (QVELT |sc| 4)) $))
                         (LET ((#0=#:G1524 (NREVERSE |ns|))
                               (#1=#:G1523 NIL))
                           (LOOP
                             (COND
                               ((ATOM #0#) (RETURN (NREVERSE #1#)))
                               (T (LET ((|n| (CAR #0#)))
                                    (SETQ #1#
                                     (CONS (|SYMBOL;istring| |n| $)
                                      #1#)))))
                             (SETQ #0# (CDR #0#)))))
                   (|getShellEntry| $ 93)))))) 

(DEFUN |SYMBOL;syscripts| (|sc| $)
  (LET ((|all| (QVELT |sc| 3)))
    (SEQ (SETQ |all|
               (SPADCALL (QVELT |sc| 2) |all| (|getShellEntry| $ 94)))
         (SETQ |all|
               (SPADCALL (QVELT |sc| 1) |all| (|getShellEntry| $ 94)))
         (SETQ |all|
               (SPADCALL (QVELT |sc| 0) |all| (|getShellEntry| $ 94)))
         (EXIT (SPADCALL |all| (QVELT |sc| 4) (|getShellEntry| $ 94)))))) 

(DEFUN |SYMBOL;script;$L$;22| (|sy| |ls| $)
  (LET ((|sc| (VECTOR NIL NIL NIL NIL NIL)))
    (SEQ (COND
           ((NOT (NULL |ls|))
            (SEQ (QSETVELT |sc| 0 (|SPADfirst| |ls|))
                 (EXIT (SETQ |ls| (CDR |ls|))))))
         (COND
           ((NOT (NULL |ls|))
            (SEQ (QSETVELT |sc| 1 (|SPADfirst| |ls|))
                 (EXIT (SETQ |ls| (CDR |ls|))))))
         (COND
           ((NOT (NULL |ls|))
            (SEQ (QSETVELT |sc| 2 (|SPADfirst| |ls|))
                 (EXIT (SETQ |ls| (CDR |ls|))))))
         (COND
           ((NOT (NULL |ls|))
            (SEQ (QSETVELT |sc| 3 (|SPADfirst| |ls|))
                 (EXIT (SETQ |ls| (CDR |ls|))))))
         (COND
           ((NOT (NULL |ls|))
            (SEQ (QSETVELT |sc| 4 (|SPADfirst| |ls|))
                 (EXIT (SETQ |ls| (CDR |ls|))))))
         (EXIT (|SYMBOL;script;$R$;23| |sy| |sc| $))))) 

(DEFUN |SYMBOL;script;$R$;23| (|sy| |sc| $)
  (COND
    ((|SYMBOL;scripted?;$B;30| |sy| $)
     (|error| "Cannot add scripts to a scripted symbol"))
    (T (CONS (|SYMBOL;coerce;$Of;11|
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
    (T (|error| "Cannot form string from non-atomic symbols.")))) 

(DEFUN |SYMBOL;latex;$S;25| (|e| $)
  (PROG (|ss| |lo| |sc|)
    (RETURN
      (LET ((|s| (PNAME (SPADCALL |e| (|getShellEntry| $ 100)))))
        (SEQ (COND
               ((< 1 (QCSIZE |s|))
                (COND
                  ((SPADCALL (SPADCALL |s| 1 (|getShellEntry| $ 106))
                       (SPADCALL "\\" (|getShellEntry| $ 43))
                       (|getShellEntry| $ 107))
                   (SETQ |s|
                         (STRCONC "\\mbox{\\it " (STRCONC |s| "}")))))))
             (COND
               ((NOT (|SYMBOL;scripted?;$B;30| |e| $)) (EXIT |s|)))
             (LETT |ss| (|SYMBOL;scripts;$R;32| |e| $)
                   |SYMBOL;latex;$S;25|)
             (LETT |lo| (QVELT |ss| 0) |SYMBOL;latex;$S;25|)
             (COND
               ((NOT (NULL |lo|))
                (SEQ (LETT |sc| "_{" |SYMBOL;latex;$S;25|)
                     (LOOP
                       (COND
                         ((NOT (NOT (NULL |lo|))) (RETURN NIL))
                         (T (SEQ (SETQ |sc|
                                       (STRCONC |sc|
                                        (SPADCALL (|SPADfirst| |lo|)
                                         (|getShellEntry| $ 112))))
                                 (SETQ |lo| (CDR |lo|))
                                 (EXIT (COND
                                         ((NOT (NULL |lo|))
                                          (SETQ |sc|
                                           (STRCONC |sc| ", ")))))))))
                     (SETQ |sc| (STRCONC |sc| "}"))
                     (EXIT (SETQ |s| (STRCONC |s| |sc|))))))
             (SETQ |lo| (QVELT |ss| 1))
             (COND
               ((NOT (NULL |lo|))
                (SEQ (LETT |sc| "^{" |SYMBOL;latex;$S;25|)
                     (LOOP
                       (COND
                         ((NOT (NOT (NULL |lo|))) (RETURN NIL))
                         (T (SEQ (SETQ |sc|
                                       (STRCONC |sc|
                                        (SPADCALL (|SPADfirst| |lo|)
                                         (|getShellEntry| $ 112))))
                                 (SETQ |lo| (CDR |lo|))
                                 (EXIT (COND
                                         ((NOT (NULL |lo|))
                                          (SETQ |sc|
                                           (STRCONC |sc| ", ")))))))))
                     (SETQ |sc| (STRCONC |sc| "}"))
                     (EXIT (SETQ |s| (STRCONC |s| |sc|))))))
             (SETQ |lo| (QVELT |ss| 2))
             (COND
               ((NOT (NULL |lo|))
                (SEQ (LETT |sc| "{}^{" |SYMBOL;latex;$S;25|)
                     (LOOP
                       (COND
                         ((NOT (NOT (NULL |lo|))) (RETURN NIL))
                         (T (SEQ (SETQ |sc|
                                       (STRCONC |sc|
                                        (SPADCALL (|SPADfirst| |lo|)
                                         (|getShellEntry| $ 112))))
                                 (SETQ |lo| (CDR |lo|))
                                 (EXIT (COND
                                         ((NOT (NULL |lo|))
                                          (SETQ |sc|
                                           (STRCONC |sc| ", ")))))))))
                     (SETQ |sc| (STRCONC |sc| "}"))
                     (EXIT (SETQ |s| (STRCONC |sc| |s|))))))
             (SETQ |lo| (QVELT |ss| 3))
             (COND
               ((NOT (NULL |lo|))
                (SEQ (LETT |sc| "{}_{" |SYMBOL;latex;$S;25|)
                     (LOOP
                       (COND
                         ((NOT (NOT (NULL |lo|))) (RETURN NIL))
                         (T (SEQ (SETQ |sc|
                                       (STRCONC |sc|
                                        (SPADCALL (|SPADfirst| |lo|)
                                         (|getShellEntry| $ 112))))
                                 (SETQ |lo| (CDR |lo|))
                                 (EXIT (COND
                                         ((NOT (NULL |lo|))
                                          (SETQ |sc|
                                           (STRCONC |sc| ", ")))))))))
                     (SETQ |sc| (STRCONC |sc| "}"))
                     (EXIT (SETQ |s| (STRCONC |sc| |s|))))))
             (SETQ |lo| (QVELT |ss| 4))
             (COND
               ((NOT (NULL |lo|))
                (SEQ (LETT |sc| "\\left( {" |SYMBOL;latex;$S;25|)
                     (LOOP
                       (COND
                         ((NOT (NOT (NULL |lo|))) (RETURN NIL))
                         (T (SEQ (SETQ |sc|
                                       (STRCONC |sc|
                                        (SPADCALL (|SPADfirst| |lo|)
                                         (|getShellEntry| $ 112))))
                                 (SETQ |lo| (CDR |lo|))
                                 (EXIT (COND
                                         ((NOT (NULL |lo|))
                                          (SETQ |sc|
                                           (STRCONC |sc| ", ")))))))))
                     (SETQ |sc| (STRCONC |sc| "} \\right)"))
                     (EXIT (SETQ |s| (STRCONC |s| |sc|))))))
             (EXIT |s|)))))) 

(DEFUN |SYMBOL;anyRadix| (|n| |s| $)
  (PROG (|qr|)
    (RETURN
      (LET ((|ns| ""))
        (LOOP
          (COND
            (NIL (RETURN NIL))
            (T (SEQ (LETT |qr|
                          (MULTIPLE-VALUE-CALL #'CONS
                              (TRUNCATE |n| (QCSIZE |s|)))
                          |SYMBOL;anyRadix|)
                    (SETQ |n| (CAR |qr|))
                    (SETQ |ns|
                          (SPADCALL
                              (SPADCALL |s|
                                  (+ (CDR |qr|)
                                     (SPADCALL |s|
                                      (|getShellEntry| $ 117)))
                                  (|getShellEntry| $ 106))
                              |ns| (|getShellEntry| $ 119)))
                    (EXIT (COND
                            ((ZEROP |n|)
                             (RETURN-FROM |SYMBOL;anyRadix| |ns|)))))))))))) 

(DEFUN |SYMBOL;new;$;27| ($)
  (LET ((|sym| (|SYMBOL;anyRadix|
                   (SPADCALL (|getShellEntry| $ 10)
                       (|getShellEntry| $ 120))
                   (|getShellEntry| $ 20) $)))
    (SEQ (SPADCALL (|getShellEntry| $ 10)
             (+ (SPADCALL (|getShellEntry| $ 10)
                    (|getShellEntry| $ 120))
                1)
             (|getShellEntry| $ 121))
         (EXIT (|SYMBOL;coerce;S$;8| (STRCONC "%" |sym|) $))))) 

(DEFUN |SYMBOL;new;2$;28| (|x| $)
  (PROG (|u| |n| |xx|)
    (RETURN
      (SEQ (LETT |n|
                 (SEQ (LETT |u|
                            (SPADCALL |x| (|getShellEntry| $ 13)
                                (|getShellEntry| $ 124))
                            |SYMBOL;new;2$;28|)
                      (EXIT (COND
                              ((EQL (CAR |u|) 1) 0)
                              (T (+ (CDR |u|) 1)))))
                 |SYMBOL;new;2$;28|)
           (SPADCALL (|getShellEntry| $ 13) |x| |n|
               (|getShellEntry| $ 127))
           (LETT |xx|
                 (COND
                   ((NOT (|SYMBOL;scripted?;$B;30| |x| $))
                    (|SYMBOL;string;$S;24| |x| $))
                   (T (|SYMBOL;string;$S;24|
                          (|SYMBOL;name;2$;31| |x| $) $)))
                 |SYMBOL;new;2$;28|)
           (SETQ |xx| (STRCONC "%" |xx|))
           (SETQ |xx|
                 (COND
                   ((NOT (< (SPADCALL
                                (SPADCALL |xx|
                                    (SPADCALL |xx|
                                     (|getShellEntry| $ 128))
                                    (|getShellEntry| $ 106))
                                (|getShellEntry| $ 19)
                                (|getShellEntry| $ 129))
                            (SPADCALL (|getShellEntry| $ 19)
                                (|getShellEntry| $ 117))))
                    (STRCONC |xx|
                             (|SYMBOL;anyRadix| |n|
                                 (|getShellEntry| $ 21) $)))
                   (T (STRCONC |xx|
                               (|SYMBOL;anyRadix| |n|
                                   (|getShellEntry| $ 19) $)))))
           (COND
             ((NOT (|SYMBOL;scripted?;$B;30| |x| $))
              (EXIT (|SYMBOL;coerce;S$;8| |xx| $))))
           (EXIT (|SYMBOL;script;$R$;23| (|SYMBOL;coerce;S$;8| |xx| $)
                     (|SYMBOL;scripts;$R;32| |x| $) $)))))) 

(DEFUN |SYMBOL;resetNew;V;29| ($)
  (SEQ (SPADCALL (|getShellEntry| $ 10) 0 (|getShellEntry| $ 121))
       (EXIT (LET ((#0=#:G1525
                       (SPADCALL (|getShellEntry| $ 13)
                           (|getShellEntry| $ 133))))
               (LOOP
                 (COND
                   ((ATOM #0#) (RETURN NIL))
                   (T (LET ((|k| (CAR #0#)))
                        (SPADCALL |k| (|getShellEntry| $ 13)
                            (|getShellEntry| $ 134)))))
                 (SETQ #0# (CDR #0#))))))) 

(DEFUN |SYMBOL;scripted?;$B;30| (|sy| $) (NOT (ATOM |sy|))) 

(DEFUN |SYMBOL;name;2$;31| (|sy| $)
  (PROG (|str|)
    (RETURN
      (SEQ (COND
             ((NOT (|SYMBOL;scripted?;$B;30| |sy| $)) |sy|)
             (T (SEQ (LETT |str|
                           (|SYMBOL;string;$S;24|
                               (SPADCALL (|SYMBOL;list;$L;34| |sy| $)
                                   (|getShellEntry| $ 137))
                               $)
                           |SYMBOL;name;2$;31|)
                     (LET ((|i| (+ (|getShellEntry| $ 41) 1))
                           (#0=#:G1526 (QCSIZE |str|)))
                       (LOOP
                         (COND
                           ((> |i| #0#) (RETURN NIL))
                           (T (COND
                                ((NOT (SPADCALL
                                       (SPADCALL |str| |i|
                                        (|getShellEntry| $ 106))
                                       (|getShellEntry| $ 139)))
                                 (RETURN-FROM |SYMBOL;name;2$;31|
                                   (|SYMBOL;coerce;S$;8|
                                    (SPADCALL |str|
                                     (SPADCALL |i| (QCSIZE |str|)
                                      (|getShellEntry| $ 141))
                                     (|getShellEntry| $ 142))
                                    $))))))
                         (SETQ |i| (+ |i| 1))))
                     (EXIT (|error| "Improper scripted symbol"))))))))) 

(DEFUN |SYMBOL;scripts;$R;32| (|sy| $)
  (PROG (|nscripts| |lscripts| |str| |nstr| |m| |allscripts|)
    (RETURN
      (SEQ (COND
             ((NOT (|SYMBOL;scripted?;$B;30| |sy| $))
              (VECTOR NIL NIL NIL NIL NIL))
             (T (SEQ (LETT |nscripts| (LIST 0 0 0 0 0)
                           |SYMBOL;scripts;$R;32|)
                     (LETT |lscripts| (LIST NIL NIL NIL NIL NIL)
                           |SYMBOL;scripts;$R;32|)
                     (LETT |str|
                           (|SYMBOL;string;$S;24|
                               (SPADCALL (|SYMBOL;list;$L;34| |sy| $)
                                   (|getShellEntry| $ 137))
                               $)
                           |SYMBOL;scripts;$R;32|)
                     (LETT |nstr| (QCSIZE |str|)
                           |SYMBOL;scripts;$R;32|)
                     (LETT |m|
                           (SPADCALL |nscripts|
                               (|getShellEntry| $ 144))
                           |SYMBOL;scripts;$R;32|)
                     (LET ((|i| |m|)
                           (|j| (+ (|getShellEntry| $ 41) 1)))
                       (LOOP
                         (COND
                           ((OR (> |j| |nstr|)
                                (NOT (SPADCALL
                                      (SPADCALL |str| |j|
                                       (|getShellEntry| $ 106))
                                      (|getShellEntry| $ 139))))
                            (RETURN NIL))
                           (T (SPADCALL |nscripts| |i|
                                  (LET ((#0=#:G1517
                                         (-
                                          (SPADCALL
                                           (SPADCALL |str| |j|
                                            (|getShellEntry| $ 106))
                                           (|getShellEntry| $ 44))
                                          (|getShellEntry| $ 45))))
                                    (|check-subtype| (NOT (MINUSP #0#))
                                     '(|NonNegativeInteger|) #0#))
                                  (|getShellEntry| $ 148))))
                         (SETQ |i| (+ |i| 1))
                         (SETQ |j| (+ |j| 1))))
                     (SETQ |nscripts|
                           (SPADCALL (CDR |nscripts|)
                               (|SPADfirst| |nscripts|)
                               (|getShellEntry| $ 151)))
                     (LETT |allscripts|
                           (CDR (|SYMBOL;list;$L;34| |sy| $))
                           |SYMBOL;scripts;$R;32|)
                     (SETQ |m|
                           (SPADCALL |lscripts|
                               (|getShellEntry| $ 153)))
                     (LET ((|i| |m|) (#1=#:G1527 |nscripts|))
                       (LOOP
                         (COND
                           ((ATOM #1#) (RETURN NIL))
                           (T (LET ((|n| (CAR #1#)))
                                (COND
                                  ((< (LENGTH |allscripts|) |n|)
                                   (|error|
                                    "Improper script count in symbol"))
                                  (T (SEQ
                                      (SPADCALL |lscripts| |i|
                                       (LET
                                        ((#2=#:G1529
                                          (SPADCALL |allscripts| |n|
                                           (|getShellEntry| $ 156)))
                                         (#3=#:G1528 NIL))
                                         (LOOP
                                           (COND
                                             ((ATOM #2#)
                                              (RETURN (NREVERSE #3#)))
                                             (T
                                              (LET ((|a| (CAR #2#)))
                                                (SETQ #3#
                                                 (CONS
                                                  (|SYMBOL;coerce;$Of;11|
                                                   |a| $)
                                                  #3#)))))
                                           (SETQ #2# (CDR #2#))))
                                       (|getShellEntry| $ 157))
                                      (EXIT
                                       (SETQ |allscripts|
                                        (SPADCALL |allscripts| |n|
                                         (|getShellEntry| $ 158))))))))))
                         (SETQ |i| (+ |i| 1))
                         (SETQ #1# (CDR #1#))))
                     (EXIT (VECTOR (SPADCALL |lscripts| |m|
                                    (|getShellEntry| $ 159))
                                   (SPADCALL |lscripts| (+ |m| 1)
                                    (|getShellEntry| $ 159))
                                   (SPADCALL |lscripts| (+ |m| 2)
                                    (|getShellEntry| $ 159))
                                   (SPADCALL |lscripts| (+ |m| 3)
                                    (|getShellEntry| $ 159))
                                   (SPADCALL |lscripts| (+ |m| 4)
                                    (|getShellEntry| $ 159))))))))))) 

(DEFUN |SYMBOL;istring| (|n| $)
  (COND
    ((< 9 |n|) (|error| "Can have at most 9 scripts of each kind"))
    (T (|getSimpleArrayEntry| (|getShellEntry| $ 18) |n|)))) 

(DEFUN |SYMBOL;list;$L;34| (|sy| $)
  (COND
    ((NOT (|SYMBOL;scripted?;$B;30| |sy| $))
     (|error| "Cannot convert a symbol to a list if it is not subscripted"))
    (T |sy|))) 

(DEFUN |SYMBOL;sample;$;35| ($) (DECLARE (IGNORE $)) '|aSymbol|) 

(DEFUN |Symbol| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (PROG (#0=#:G1531)
    (RETURN
      (COND
        ((SETQ #0# (HGET |$ConstructorCache| '|Symbol|))
         (|CDRwithIncrement| (CDAR #0#)))
        (T (UNWIND-PROTECT
             (PROG1 (CDDAR (HPUT |$ConstructorCache| '|Symbol|
                                 (LIST (CONS NIL (CONS 1 (|Symbol;|))))))
               (SETQ #0# T))
             (COND ((NOT #0#) (HREM |$ConstructorCache| '|Symbol|))))))))) 

(DEFUN |Symbol;| ()
  (DECLARE (SPECIAL |$ConstructorCache|))
  (LET ((|dv$| (LIST '|Symbol|)) ($ (|newShell| 165))
        (|pv$| (|buildPredVector| 0 0 NIL)))
    (|setShellEntry| $ 0 |dv$|)
    (|setShellEntry| $ 3 |pv$|)
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
    $)) 

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
             (69 . |outputForm|) |SYMBOL;coerce;$Of;11| (|List| 53)
             (74 . |nil|) (|List| 56) |SYMBOL;script;$L$;22|
             |SYMBOL;subscript;$L$;12| |SYMBOL;elt;$L$;13|
             |SYMBOL;superscript;$L$;14| |SYMBOL;argscript;$L$;15|
             (|PatternMatchResult| 6 25) (|Pattern| 6)
             (|PatternMatchSymbol| 6) (78 . |patternMatch|)
             (|PatternMatchResult| 6 $) |SYMBOL;patternMatch;$P2Pmr;16|
             (|Float|) (|PatternMatchResult| 70 25) (|Pattern| 70)
             (|PatternMatchSymbol| 70) (85 . |patternMatch|)
             (|PatternMatchResult| 70 $)
             |SYMBOL;patternMatch;$P2Pmr;17| (92 . |coerce|)
             |SYMBOL;convert;$P;18| (97 . |coerce|)
             |SYMBOL;convert;$P;19| (102 . |#|) (|List| 6) (107 . |#|)
             (112 . >=) (118 . |first|) (123 . |zero?|) (128 . |false|)
             (132 . |rest|) (137 . |concat|) (143 . |reverse!|)
             (148 . |concat|) (|List| $) (154 . |concat|)
             (159 . |concat|) (165 . |null|) (170 . |first|)
             (175 . |rest|)
             (|Record| (|:| |sub| 56) (|:| |sup| 56) (|:| |presup| 56)
                 (|:| |presub| 56) (|:| |args| 56))
             |SYMBOL;script;$R$;23| |SYMBOL;name;2$;31|
             |SYMBOL;string;$S;24| (180 . |concat|) (186 . |One|)
             (190 . >) (196 . |One|) (200 . |elt|) (206 . ~=)
             |SYMBOL;scripts;$R;32| (212 . |empty?|) (217 . |not|)
             (222 . |first|) (227 . |latex|) (232 . |rest|)
             |SYMBOL;latex;$S;25|
             (|Record| (|:| |quotient| $) (|:| |remainder| $))
             (237 . |divide|) (243 . |minIndex|) (248 . +)
             (254 . |concat|) (260 . |elt|) (265 . |setelt|)
             |SYMBOL;new;$;27| (|Union| 6 '"failed") (271 . |search|)
             (277 . |Zero|) (281 . |inc|) (286 . |setelt|)
             (293 . |maxIndex|) (298 . |position|) (304 . >=)
             |SYMBOL;new;2$;28| (|List| $$) (310 . |keys|)
             (315 . |remove!|) |SYMBOL;resetNew;V;29|
             |SYMBOL;list;$L;34| (321 . |first|) (326 . +)
             (332 . |digit?|) (|UniversalSegment| 6) (337 . SEGMENT)
             (343 . |elt|) (|List| 39) (349 . |minIndex|)
             (|PositiveInteger|) (354 . |One|) (358 . -)
             (364 . |setelt|) (371 . |rest|) (376 . |first|)
             (381 . |concat|) (387 . |rest|) (392 . |minIndex|)
             (397 . |#|) (402 . <) (408 . |first|) (414 . |setelt|)
             (421 . |rest|) (427 . |elt|) (433 . >) (439 . |minIndex|)
             (444 . |elt|)
             (CONS IDENTITY
                   (FUNCALL (|dispatchFunction| |SYMBOL;sample;$;35|)
                            $))
             (|SingleInteger|))
          '#(~= 450 |superscript| 456 |subscript| 462 |string| 468
             |scripts| 473 |scripted?| 478 |script| 483 |sample| 495
             |resetNew| 499 |patternMatch| 503 |new| 517 |name| 526
             |min| 531 |max| 537 |list| 543 |latex| 548 |hash| 553
             |elt| 558 |convert| 564 |coerce| 584 |before?| 594
             |argscript| 600 |OMwrite| 606 >= 630 > 636 = 642 <= 648 <
             654)
          'NIL
          (CONS (|makeByteWordVec2| 1 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                (CONS '#(NIL NIL NIL |SetCategory&| |OrderedType&|
                         |BasicType&| NIL NIL NIL NIL NIL NIL NIL NIL)
                      (CONS '#((|OrderedSet|) (|PatternMatchable| 70)
                               (|PatternMatchable| 6) (|SetCategory|)
                               (|OrderedType|) (|BasicType|)
                               (|ConvertibleTo| 72)
                               (|ConvertibleTo| 65)
                               (|CoercibleFrom| 14)
                               (|ConvertibleTo| 25) (|OpenMath|)
                               (|ConvertibleTo| 46) (|Type|)
                               (|CoercibleTo| 53))
                            (|makeByteWordVec2| 164
                                '(0 6 0 7 1 8 0 6 9 0 11 0 12 1 16 0 15
                                  17 2 26 24 0 25 27 0 28 0 29 2 26 0
                                  14 28 30 1 26 24 0 31 1 26 24 0 32 1
                                  26 24 0 33 1 14 39 0 40 1 42 0 14 43
                                  1 42 39 0 44 1 46 0 25 47 1 53 0 25
                                  54 0 56 0 57 3 66 64 25 65 64 67 3 73
                                  71 25 72 71 74 1 72 0 25 77 1 65 0 25
                                  79 1 56 39 0 81 1 82 39 0 83 2 39 22
                                  0 0 84 1 82 6 0 85 1 6 22 0 86 0 22 0
                                  87 1 82 0 0 88 2 14 0 0 0 89 1 82 0 0
                                  90 2 15 0 14 0 91 1 14 0 92 93 2 56 0
                                  0 0 94 1 58 22 0 95 1 58 56 0 96 1 58
                                  0 0 97 2 56 0 53 0 102 0 39 0 103 2
                                  39 22 0 0 104 0 6 0 105 2 14 42 0 6
                                  106 2 42 22 0 0 107 1 56 22 0 109 1
                                  22 0 0 110 1 56 53 0 111 1 53 14 0
                                  112 1 56 0 0 113 2 6 115 0 0 116 1 14
                                  6 0 117 2 6 0 0 0 118 2 14 0 42 0 119
                                  1 8 6 0 120 2 8 6 0 6 121 2 11 123 2
                                  0 124 0 39 0 125 1 6 0 0 126 3 11 6 0
                                  2 6 127 1 14 6 0 128 2 14 6 42 0 129
                                  2 6 22 0 0 130 1 11 132 0 133 2 11
                                  123 2 0 134 1 132 2 0 137 2 39 0 0 0
                                  138 1 42 22 0 139 2 140 0 6 6 141 2
                                  14 0 0 140 142 1 143 6 0 144 0 145 0
                                  146 2 6 0 0 0 147 3 143 39 0 6 39 148
                                  1 143 0 0 149 1 143 39 0 150 2 143 0
                                  0 39 151 1 132 0 0 152 1 58 6 0 153 1
                                  132 39 0 154 2 39 22 0 0 155 2 132 0
                                  0 39 156 3 58 56 0 6 56 157 2 132 0 0
                                  39 158 2 58 56 0 6 159 2 6 22 0 0 160
                                  1 16 6 0 161 2 16 14 0 6 162 2 0 22 0
                                  0 1 2 0 0 0 56 62 2 0 0 0 56 60 1 0
                                  14 0 101 1 0 98 0 108 1 0 22 0 23 2 0
                                  0 0 58 59 2 0 0 0 98 99 0 0 0 163 0 0
                                  24 135 3 0 75 0 72 75 76 3 0 68 0 65
                                  68 69 1 0 0 0 131 0 0 0 122 1 0 0 0
                                  100 2 0 0 0 0 1 2 0 0 0 0 1 1 0 92 0
                                  136 1 0 14 0 114 1 0 164 0 1 2 0 0 0
                                  56 61 1 0 65 0 80 1 0 72 0 78 1 0 25
                                  0 49 1 0 46 0 48 1 0 0 14 50 1 0 53 0
                                  55 2 0 22 0 0 1 2 0 0 0 56 63 2 0 24
                                  26 0 36 3 0 24 26 0 22 37 1 0 14 0 34
                                  2 0 14 0 22 35 2 0 22 0 0 1 2 0 22 0
                                  0 1 2 0 22 0 0 51 2 0 22 0 0 1 2 0 22
                                  0 0 52)))))
          '|lookupComplete|)) 

(MAKEPROP '|Symbol| 'NILADIC T) 
