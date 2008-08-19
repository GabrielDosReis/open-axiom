
(/VERSIONCHECK 2) 

(DEFPARAMETER |CancellationAbelianMonoid;AL| 'NIL) 

(DEFUN |CancellationAbelianMonoid| ()
  (LET (#:G1396)
    (COND
      (|CancellationAbelianMonoid;AL|)
      (T (SETQ |CancellationAbelianMonoid;AL|
               (|CancellationAbelianMonoid;|)))))) 

(DEFUN |CancellationAbelianMonoid;| ()
  (PROG (#0=#:G1394)
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

(SETQ |$CategoryFrame|
      (|put| '|CancellationAbelianMonoid| '|isCategory| T
             (|addModemap| '|CancellationAbelianMonoid|
                 '(|CancellationAbelianMonoid|) '((|Category|)) T
                 '|CancellationAbelianMonoid| |$CategoryFrame|))) 

(MAKEPROP '|CancellationAbelianMonoid| 'NILADIC T) 
