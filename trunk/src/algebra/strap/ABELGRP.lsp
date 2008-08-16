
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianGroup;AL| 'NIL) 

(DEFUN |AbelianGroup| ()
  (LET (#:G1388)
    (COND
      (|AbelianGroup;AL|)
      (T (SETQ |AbelianGroup;AL| (|AbelianGroup;|)))))) 

(DEFUN |AbelianGroup;| ()
  (PROG (#0=#:G1386)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|CancellationAbelianMonoid|)
                           (|mkCategory| '|domain|
                               '(((- ($ $)) T) ((- ($ $ $)) T)
                                 ((* ($ (|Integer|) $)) T))
                               NIL '((|Integer|)) NIL))
                   |AbelianGroup|)
        (SETELT #0# 0 '(|AbelianGroup|)))))) 

(MAKEPROP '|AbelianGroup| 'NILADIC T) 
