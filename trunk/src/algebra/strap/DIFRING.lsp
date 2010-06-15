
(/VERSIONCHECK 2) 

(DEFPARAMETER |DifferentialRing;AL| 'NIL) 

(DEFUN |DifferentialRing;| ()
  (LET ((#0=#:G1397
            (|Join| (|Ring|) (|DifferentialDomain| '$)
                    (|mkCategory| '|domain|
                        '(((|differentiate|
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
