
(/VERSIONCHECK 2) 

(DEFPARAMETER |Monoid;AL| 'NIL) 

(DEFUN |Monoid;| ()
  (LET ((#0=#:G1399
            (|Join| (|SemiGroup|)
                    (|mkCategory| '|domain|
                        '(((|One| ($) |constant|) T)
                          ((|sample| ($) |constant|) T)
                          ((|one?| ((|Boolean|) $)) T)
                          ((** ($ $ (|NonNegativeInteger|))) T)
                          ((|recip| ((|Union| $ "failed") $)) T))
                        NIL '((|NonNegativeInteger|) (|Boolean|)) NIL))))
    (|setShellEntry| #0# 0 '(|Monoid|))
    #0#)) 

(DEFUN |Monoid| ()
  (COND (|Monoid;AL|) (T (SETQ |Monoid;AL| (|Monoid;|))))) 

(MAKEPROP '|Monoid| 'NILADIC T) 
