
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianMonoid;AL| 'NIL) 

(DEFUN |AbelianMonoid;| ()
  (LET ((#0=#:G1397
            (|Join| (|AbelianSemiGroup|)
                    (|mkCategory| '|domain|
                        '(((|Zero| ($) |constant|) T)
                          ((|sample| ($) |constant|) T)
                          ((|zero?| ((|Boolean|) $)) T)
                          ((* ($ (|NonNegativeInteger|) $)) T))
                        NIL '((|NonNegativeInteger|) (|Boolean|)) NIL))))
    (|setShellEntry| #0# 0 '(|AbelianMonoid|))
    #0#)) 

(DEFUN |AbelianMonoid| ()
  (COND
    (|AbelianMonoid;AL|)
    (T (SETQ |AbelianMonoid;AL| (|AbelianMonoid;|))))) 

(MAKEPROP '|AbelianMonoid| 'NILADIC T) 
