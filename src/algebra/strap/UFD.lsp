
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
        (|setShellEntry| #0# 0 '(|UniqueFactorizationDomain|)))))) 

(DEFUN |UniqueFactorizationDomain| ()
  (LET ()
    (COND
      (|UniqueFactorizationDomain;AL|)
      (T (SETQ |UniqueFactorizationDomain;AL|
               (|UniqueFactorizationDomain;|)))))) 

(MAKEPROP '|UniqueFactorizationDomain| 'NILADIC T) 
