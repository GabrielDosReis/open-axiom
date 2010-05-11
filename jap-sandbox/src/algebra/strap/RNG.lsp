
(/VERSIONCHECK 2) 

(DEFPARAMETER |Rng;AL| 'NIL) 

(DEFUN |Rng;| ()
  (LET ((#0=#:G1396 (|Join| (|AbelianGroup|) (|SemiGroup|))))
    (|setShellEntry| #0# 0 '(|Rng|))
    #0#)) 

(DEFUN |Rng| () (COND (|Rng;AL|) (T (SETQ |Rng;AL| (|Rng;|))))) 

(MAKEPROP '|Rng| 'NILADIC T) 
