
(/VERSIONCHECK 2) 

(DEFPARAMETER |DifferentialRing;AL| 'NIL) 

(DEFUN |DifferentialRing;| ()
  (LET ((#0=#:G1396
            (|Join| (|Ring|)
                    (|mkCategory| '|domain|
                        '(((|differentiate| ($ $)) T) ((D ($ $)) T)
                          ((|differentiate|
                               ($ $ (|NonNegativeInteger|)))
                           T)
                          ((D ($ $ (|NonNegativeInteger|))) T))
                        NIL '((|NonNegativeInteger|)) NIL))))
    (|setShellEntry| #0# 0 '(|DifferentialRing|))
    #0#)) 

(DEFUN |DifferentialRing| ()
  (COND
    (|DifferentialRing;AL|)
    (T (SETQ |DifferentialRing;AL| (|DifferentialRing;|))))) 

(MAKEPROP '|DifferentialRing| 'NILADIC T) 
