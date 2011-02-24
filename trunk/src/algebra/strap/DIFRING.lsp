
(/VERSIONCHECK 2) 

(DEFPARAMETER |DifferentialRing;AL| 'NIL) 

(DEFUN |DifferentialRing;| ()
  (LET ((#0=#:G1372 (|Join| (|Ring|) (|DifferentialSpace|))))
    (SETF (|shellEntry| #0# 0) '(|DifferentialRing|))
    #0#)) 

(DEFUN |DifferentialRing| ()
  (COND
    (|DifferentialRing;AL|)
    (T (SETQ |DifferentialRing;AL| (|DifferentialRing;|))))) 

(MAKEPROP '|DifferentialRing| 'NILADIC T) 
