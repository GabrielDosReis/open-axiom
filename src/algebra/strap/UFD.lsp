
(/VERSIONCHECK 2) 

(DEFPARAMETER |UniqueFactorizationDomain;AL| 'NIL) 

(DEFUN |UniqueFactorizationDomain| ()
  (LET (#:G1387)
    (COND
      (|UniqueFactorizationDomain;AL|)
      (T (SETQ |UniqueFactorizationDomain;AL|
               (|UniqueFactorizationDomain;|)))))) 

(DEFUN |UniqueFactorizationDomain;| ()
  (PROG (#0=#:G1385)
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

(MAKEPROP '|UniqueFactorizationDomain| 'NILADIC T) 
