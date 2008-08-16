
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianSemiGroup;AL| 'NIL) 

(DEFUN |AbelianSemiGroup| ()
  (LET (#:G1387)
    (COND
      (|AbelianSemiGroup;AL|)
      (T (SETQ |AbelianSemiGroup;AL| (|AbelianSemiGroup;|)))))) 

(DEFUN |AbelianSemiGroup;| ()
  (PROG (#0=#:G1385)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|SetCategory|)
                           (|mkCategory| '|domain|
                               '(((+ ($ $ $)) T)
                                 ((* ($ (|PositiveInteger|) $)) T))
                               NIL '((|PositiveInteger|)) NIL))
                   |AbelianSemiGroup|)
        (SETELT #0# 0 '(|AbelianSemiGroup|)))))) 

(MAKEPROP '|AbelianSemiGroup| 'NILADIC T) 
