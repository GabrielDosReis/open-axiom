
(/VERSIONCHECK 2) 

(DEFPARAMETER |Ring;AL| 'NIL) 

(DEFUN |Ring;| ()
  (PROG (#0=#:G1399)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV| (PAIR '(#1=#:G1398) (LIST '(|Integer|)))
                       (|Join| (|Rng|) (|Monoid|) (|LeftModule| '$)
                               (|CoercibleFrom| '#1#)
                               (|mkCategory| '|package|
                                   '(((|characteristic|
                                       ((|NonNegativeInteger|))
                                       |constant|)
                                      T))
                                   '((|unitsKnown| T))
                                   '((|NonNegativeInteger|)) NIL)))
                   |Ring|)
        (|setShellEntry| #0# 0 '(|Ring|)))))) 

(DEFUN |Ring| ()
  (LET () (COND (|Ring;AL|) (T (SETQ |Ring;AL| (|Ring;|)))))) 

(MAKEPROP '|Ring| 'NILADIC T) 
