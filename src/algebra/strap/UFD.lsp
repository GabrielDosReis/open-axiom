
(/VERSIONCHECK 2) 

(DEFPARAMETER |UniqueFactorizationDomain;AL| 'NIL) 

(DEFUN |UniqueFactorizationDomain;| ()
  (LET ((#0=#:G1372
            (|Join| (|GcdDomain|)
                    (|mkCategory| '|domain|
                        '(((|prime?| ((|Boolean|) $)) T)
                          ((|squareFree| ((|Factored| $) $)) T)
                          ((|squareFreePart| ($ $)) T)
                          ((|factor| ((|Factored| $) $)) T))
                        NIL '((|Factored| $) (|Boolean|)) NIL))))
    (SETF (|shellEntry| #0# 0) '(|UniqueFactorizationDomain|))
    #0#)) 

(DEFUN |UniqueFactorizationDomain| ()
  (COND
    (|UniqueFactorizationDomain;AL|)
    (T (SETQ |UniqueFactorizationDomain;AL|
             (|UniqueFactorizationDomain;|))))) 

(MAKEPROP '|UniqueFactorizationDomain| 'NILADIC T) 
