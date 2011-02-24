
(/VERSIONCHECK 2) 

(DEFPARAMETER |Ring;AL| 'NIL) 

(DEFUN |Ring;| ()
  (LET ((#0=#:G1373
            (|sublisV| (PAIR '(#1=#:G1372) '((|Integer|)))
                (|Join| (|Rng|) (|Monoid|) (|LeftModule| '$)
                        (|CoercibleFrom| '#1#)
                        (|mkCategory| '|package|
                            '(((|characteristic|
                                   ((|NonNegativeInteger|)) |constant|)
                               T))
                            '((|unitsKnown| T))
                            '((|NonNegativeInteger|)) NIL)))))
    (SETF (|shellEntry| #0# 0) '(|Ring|))
    #0#)) 

(DEFUN |Ring| () (COND (|Ring;AL|) (T (SETQ |Ring;AL| (|Ring;|))))) 

(MAKEPROP '|Ring| 'NILADIC T) 
