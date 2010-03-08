
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianGroup;AL| 'NIL) 

(DEFUN |AbelianGroup;| ()
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV| (PAIR '(#1=#:G1397) (LIST '(|Integer|)))
                       (|Join| (|CancellationAbelianMonoid|)
                               (|LeftLinearSet| '#1#)
                               (|mkCategory| '|domain|
                                   '(((- ($ $)) T) ((- ($ $ $)) T)) NIL
                                   'NIL NIL)))
                   |AbelianGroup|)
        (|setShellEntry| #0# 0 '(|AbelianGroup|)))))) 

(DEFUN |AbelianGroup| ()
  (LET ()
    (COND
      (|AbelianGroup;AL|)
      (T (SETQ |AbelianGroup;AL| (|AbelianGroup;|)))))) 

(MAKEPROP '|AbelianGroup| 'NILADIC T) 
