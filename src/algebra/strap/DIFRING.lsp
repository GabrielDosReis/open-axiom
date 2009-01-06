
(/VERSIONCHECK 2) 

(DEFPARAMETER |DifferentialRing;AL| 'NIL) 

(DEFUN |DifferentialRing;| ()
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|Ring|)
                           (|mkCategory| '|domain|
                               '(((|differentiate| ($ $)) T)
                                 ((D ($ $)) T)
                                 ((|differentiate|
                                      ($ $ (|NonNegativeInteger|)))
                                  T)
                                 ((D ($ $ (|NonNegativeInteger|))) T))
                               NIL '((|NonNegativeInteger|)) NIL))
                   |DifferentialRing|)
        (|setShellEntry| #0# 0 '(|DifferentialRing|)))))) 

(DEFUN |DifferentialRing| ()
  (LET ()
    (COND
      (|DifferentialRing;AL|)
      (T (SETQ |DifferentialRing;AL| (|DifferentialRing;|)))))) 

(MAKEPROP '|DifferentialRing| 'NILADIC T) 
