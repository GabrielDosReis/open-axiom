
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianMonoid;AL| 'NIL) 

(DEFUN |AbelianMonoid;| ()
  (PROG (#0=#:G1397)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|AbelianSemiGroup|)
                           (|mkCategory| '|domain|
                               '(((|Zero| ($) |constant|) T)
                                 ((|sample| ($) |constant|) T)
                                 ((|zero?| ((|Boolean|) $)) T)
                                 ((* ($ (|NonNegativeInteger|) $)) T))
                               NIL
                               '((|NonNegativeInteger|) (|Boolean|))
                               NIL))
                   |AbelianMonoid|)
        (|setShellEntry| #0# 0 '(|AbelianMonoid|)))))) 

(DEFUN |AbelianMonoid| ()
  (LET ()
    (COND
      (|AbelianMonoid;AL|)
      (T (SETQ |AbelianMonoid;AL| (|AbelianMonoid;|)))))) 

(MAKEPROP '|AbelianMonoid| 'NILADIC T) 
