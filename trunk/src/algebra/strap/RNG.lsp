
(/VERSIONCHECK 2) 

(DEFPARAMETER |Rng;AL| 'NIL) 

(DEFUN |Rng| ()
  (LET (#:G1387) (COND (|Rng;AL|) (T (SETQ |Rng;AL| (|Rng;|)))))) 

(DEFUN |Rng;| ()
  (PROG (#0=#:G1385)
    (RETURN
      (PROG1 (LETT #0# (|Join| (|AbelianGroup|) (|SemiGroup|)) |Rng|)
        (SETELT #0# 0 '(|Rng|)))))) 

(MAKEPROP '|Rng| 'NILADIC T) 
