
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianSemiGroup;AL| 'NIL) 

(DEFUN |AbelianSemiGroup;| ()
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|SetCategory|)
                           (|mkCategory| '|domain|
                               '(((+ ($ $ $)) T)
                                 ((* ($ (|PositiveInteger|) $)) T))
                               NIL '((|PositiveInteger|)) NIL))
                   |AbelianSemiGroup|)
        (|setShellEntry| #0# 0 '(|AbelianSemiGroup|)))))) 

(DEFUN |AbelianSemiGroup| ()
  (LET ()
    (COND
      (|AbelianSemiGroup;AL|)
      (T (SETQ |AbelianSemiGroup;AL| (|AbelianSemiGroup;|)))))) 

(MAKEPROP '|AbelianSemiGroup| 'NILADIC T) 
