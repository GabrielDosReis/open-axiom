
(/VERSIONCHECK 2) 

(DEFPARAMETER |CancellationAbelianMonoid;AL| 'NIL) 

(DEFUN |CancellationAbelianMonoid| ()
  (LET (#:G1387)
    (COND
      (|CancellationAbelianMonoid;AL|)
      (T (SETQ |CancellationAbelianMonoid;AL|
               (|CancellationAbelianMonoid;|)))))) 

(DEFUN |CancellationAbelianMonoid;| ()
  (PROG (#0=#:G1385)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|AbelianMonoid|)
                           (|mkCategory| '|domain|
                               '(((|subtractIfCan|
                                      ((|Union| $ "failed") $ $))
                                  T))
                               NIL 'NIL NIL))
                   |CancellationAbelianMonoid|)
        (SETELT #0# 0 '(|CancellationAbelianMonoid|)))))) 

(MAKEPROP '|CancellationAbelianMonoid| 'NILADIC T) 
