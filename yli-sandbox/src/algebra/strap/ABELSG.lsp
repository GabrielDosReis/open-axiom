
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianSemiGroup;AL| 'NIL) 

(DEFUN |AbelianSemiGroup;| ()
  (LET ((#0=#:G1397
            (|Join| (|SetCategory|)
                    (|mkCategory| '|domain|
                        '(((+ ($ $ $)) T)
                          ((* ($ (|PositiveInteger|) $)) T))
                        NIL '((|PositiveInteger|)) NIL))))
    (|setShellEntry| #0# 0 '(|AbelianSemiGroup|))
    #0#)) 

(DEFUN |AbelianSemiGroup| ()
  (COND
    (|AbelianSemiGroup;AL|)
    (T (SETQ |AbelianSemiGroup;AL| (|AbelianSemiGroup;|))))) 

(MAKEPROP '|AbelianSemiGroup| 'NILADIC T) 
