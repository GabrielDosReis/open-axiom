
(/VERSIONCHECK 2) 

(DEFPARAMETER |DivisionRing;AL| 'NIL) 

(DEFUN |DivisionRing;| ()
  (LET ((#0=#:G1400
            (|sublisV|
                (PAIR '(#1=#:G1399) (LIST '(|Fraction| (|Integer|))))
                (|Join| (|EntireRing|) (|Algebra| '#1#)
                        (|mkCategory| '|domain|
                            '(((** ($ $ (|Integer|))) T)
                              ((|inv| ($ $)) T))
                            NIL '((|Integer|)) NIL)))))
    (|setShellEntry| #0# 0 '(|DivisionRing|))
    #0#)) 

(DEFUN |DivisionRing| ()
  (COND
    (|DivisionRing;AL|)
    (T (SETQ |DivisionRing;AL| (|DivisionRing;|))))) 

(MAKEPROP '|DivisionRing| 'NILADIC T) 
