
(/VERSIONCHECK 2) 

(DEFPARAMETER |Rng;AL| 'NIL) 

(DEFUN |Rng;| ()
  (PROG (#0=#:G1397)
    (RETURN
      (PROG1 (LETT #0# (|Join| (|AbelianGroup|) (|SemiGroup|)) |Rng|)
        (SETELT #0# 0 '(|Rng|)))))) 

(DEFUN |Rng| ()
  (LET () (COND (|Rng;AL|) (T (SETQ |Rng;AL| (|Rng;|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|Rng| '|isCategory| T
             (|addModemap| '|Rng| '(|Rng|) '((|Category|)) T '|Rng|
                 |$CategoryFrame|))) 

(MAKEPROP '|Rng| 'NILADIC T) 
