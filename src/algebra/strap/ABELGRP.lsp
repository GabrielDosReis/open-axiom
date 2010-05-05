
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianGroup;AL| 'NIL) 

(DEFUN |AbelianGroup;| ()
  (LET ((#0=#:G1398
            (|sublisV| (PAIR '(#1=#:G1397) (LIST '(|Integer|)))
                       (|Join| (|CancellationAbelianMonoid|)
                               (|LeftLinearSet| '#1#)
                               (|mkCategory| '|domain|
                                   '(((- ($ $)) T) ((- ($ $ $)) T)) NIL
                                   'NIL NIL)))))
    (|setShellEntry| #0# 0 '(|AbelianGroup|))
    #0#)) 

(DEFUN |AbelianGroup| ()
  (COND
    (|AbelianGroup;AL|)
    (T (SETQ |AbelianGroup;AL| (|AbelianGroup;|))))) 

(MAKEPROP '|AbelianGroup| 'NILADIC T) 
