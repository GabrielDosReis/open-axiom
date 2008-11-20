
(/VERSIONCHECK 2) 

(DEFPARAMETER |CancellationAbelianMonoid;AL| 'NIL) 

(DEFUN |CancellationAbelianMonoid;| ()
  (PROG (#0=#:G1397)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|AbelianMonoid|)
                           (|mkCategory| '|domain|
                               '(((|subtractIfCan|
                                      ((|Union| $ "failed") $ $))
                                  T))
                               NIL 'NIL NIL))
                   |CancellationAbelianMonoid|)
        (|setShellEntry| #0# 0 '(|CancellationAbelianMonoid|)))))) 

(DEFUN |CancellationAbelianMonoid| ()
  (LET ()
    (COND
      (|CancellationAbelianMonoid;AL|)
      (T (SETQ |CancellationAbelianMonoid;AL|
               (|CancellationAbelianMonoid;|)))))) 

(MAKEPROP '|CancellationAbelianMonoid| 'NILADIC T) 
