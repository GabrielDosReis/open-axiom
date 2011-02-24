
(/VERSIONCHECK 2) 

(DEFPARAMETER |DivisionRing;AL| 'NIL) 

(DEFUN |DivisionRing;| ()
  (LET ((#0=#:G1375
            (|sublisV| (PAIR '(#1=#:G1374) '((|Fraction| (|Integer|))))
                       (|Join| (|EntireRing|) (|Algebra| '#1#)
                               (|mkCategory| '|domain|
                                   '(((** ($ $ (|Integer|))) T)
                                     ((|inv| ($ $)) T))
                                   NIL '((|Integer|)) NIL)))))
    (SETF (|shellEntry| #0# 0) '(|DivisionRing|))
    #0#)) 

(DEFUN |DivisionRing| ()
  (COND
    (|DivisionRing;AL|)
    (T (SETQ |DivisionRing;AL| (|DivisionRing;|))))) 

(MAKEPROP '|DivisionRing| 'NILADIC T) 
