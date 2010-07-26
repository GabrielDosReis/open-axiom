
(/VERSIONCHECK 2) 

(DEFPARAMETER |CancellationAbelianMonoid;AL| 'NIL) 

(DEFUN |CancellationAbelianMonoid;| ()
  (LET ((#0=#:G1397
            (|Join| (|AbelianMonoid|)
                    (|mkCategory| '|domain|
                        '(((|subtractIfCan| ((|Union| $ "failed") $ $))
                           T))
                        NIL 'NIL NIL))))
    (|setShellEntry| #0# 0 '(|CancellationAbelianMonoid|))
    #0#)) 

(DEFUN |CancellationAbelianMonoid| ()
  (COND
    (|CancellationAbelianMonoid;AL|)
    (T (SETQ |CancellationAbelianMonoid;AL|
             (|CancellationAbelianMonoid;|))))) 

(MAKEPROP '|CancellationAbelianMonoid| 'NILADIC T) 
