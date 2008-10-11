
(/VERSIONCHECK 2) 

(DEFPARAMETER |UniqueFactorizationDomain;AL| 'NIL) 

(DEFUN |UniqueFactorizationDomain;| ()
  (PROG (#0=#:G1397)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|GcdDomain|)
                           (|mkCategory| '|domain|
                               '(((|prime?| ((|Boolean|) $)) T)
                                 ((|squareFree| ((|Factored| $) $)) T)
                                 ((|squareFreePart| ($ $)) T)
                                 ((|factor| ((|Factored| $) $)) T))
                               NIL '((|Factored| $) (|Boolean|)) NIL))
                   |UniqueFactorizationDomain|)
        (SETELT #0# 0 '(|UniqueFactorizationDomain|)))))) 

(DEFUN |UniqueFactorizationDomain| ()
  (LET ()
    (COND
      (|UniqueFactorizationDomain;AL|)
      (T (SETQ |UniqueFactorizationDomain;AL|
               (|UniqueFactorizationDomain;|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|UniqueFactorizationDomain| '|isCategory| T
             (|addModemap| '|UniqueFactorizationDomain|
                 '(|UniqueFactorizationDomain|) '((|Category|)) T
                 '|UniqueFactorizationDomain| |$CategoryFrame|))) 

(MAKEPROP '|UniqueFactorizationDomain| 'NILADIC T) 
