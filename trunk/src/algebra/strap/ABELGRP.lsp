
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianGroup;AL| 'NIL) 

(DEFUN |AbelianGroup;| ()
  (PROG (#0=#:G1399)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|CancellationAbelianMonoid|)
                           (|mkCategory| '|domain|
                               '(((- ($ $)) T) ((- ($ $ $)) T)
                                 ((* ($ (|Integer|) $)) T))
                               NIL '((|Integer|)) NIL))
                   |AbelianGroup|)
        (|setShellEntry| #0# 0 '(|AbelianGroup|)))))) 

(DEFUN |AbelianGroup| ()
  (LET ()
    (COND
      (|AbelianGroup;AL|)
      (T (SETQ |AbelianGroup;AL| (|AbelianGroup;|)))))) 

(MAKEPROP '|AbelianGroup| 'NILADIC T) 
