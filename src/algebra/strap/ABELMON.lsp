
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianMonoid;AL| 'NIL) 

(DEFUN |AbelianMonoid| ()
  (LET (#:G1388)
    (COND
      (|AbelianMonoid;AL|)
      (T (SETQ |AbelianMonoid;AL| (|AbelianMonoid;|)))))) 

(DEFUN |AbelianMonoid;| ()
  (PROG (#0=#:G1386)
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
        (SETELT #0# 0 '(|AbelianMonoid|)))))) 

(MAKEPROP '|AbelianMonoid| 'NILADIC T) 
