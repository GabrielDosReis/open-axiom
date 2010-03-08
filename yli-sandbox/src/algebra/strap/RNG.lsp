
(/VERSIONCHECK 2) 

(DEFPARAMETER |Rng;AL| 'NIL) 

(DEFUN |Rng;| ()
  (PROG (#0=#:G1396)
    (RETURN
      (PROG1 (LETT #0# (|Join| (|AbelianGroup|) (|SemiGroup|)) |Rng|)
        (|setShellEntry| #0# 0 '(|Rng|)))))) 

(DEFUN |Rng| ()
  (LET () (COND (|Rng;AL|) (T (SETQ |Rng;AL| (|Rng;|)))))) 

(MAKEPROP '|Rng| 'NILADIC T) 
