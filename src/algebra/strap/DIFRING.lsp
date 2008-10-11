
(/VERSIONCHECK 2) 

(DEFPARAMETER |DifferentialRing;AL| 'NIL) 

(DEFUN |DifferentialRing;| ()
  (PROG (#0=#:G1397)
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
        (SETELT #0# 0 '(|DifferentialRing|)))))) 

(DEFUN |DifferentialRing| ()
  (LET ()
    (COND
      (|DifferentialRing;AL|)
      (T (SETQ |DifferentialRing;AL| (|DifferentialRing;|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|DifferentialRing| '|isCategory| T
             (|addModemap| '|DifferentialRing| '(|DifferentialRing|)
                 '((|Category|)) T '|DifferentialRing|
                 |$CategoryFrame|))) 

(MAKEPROP '|DifferentialRing| 'NILADIC T) 
