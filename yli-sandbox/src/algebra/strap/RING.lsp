
(/VERSIONCHECK 2) 

(DEFPARAMETER |Ring;AL| 'NIL) 

(DEFUN |Ring;| ()
  (LET ((#0=#:G1398
            (|sublisV| (PAIR '(#1=#:G1397) (LIST '(|Integer|)))
                (|Join| (|Rng|) (|Monoid|) (|LeftModule| '$)
                        (|CoercibleFrom| '#1#)
                        (|mkCategory| '|package|
                            '(((|characteristic|
                                   ((|NonNegativeInteger|)) |constant|)
                               T))
                            '((|unitsKnown| T))
                            '((|NonNegativeInteger|)) NIL)))))
    (|setShellEntry| #0# 0 '(|Ring|))
    #0#)) 

(DEFUN |Ring| () (COND (|Ring;AL|) (T (SETQ |Ring;AL| (|Ring;|))))) 

(MAKEPROP '|Ring| 'NILADIC T) 
