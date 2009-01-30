
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianMonoid;AL| 'NIL) 

(DEFUN |AbelianMonoid| ()
  (LET (#:G1397)
    (COND
      (|AbelianMonoid;AL|)
      (T (SETQ |AbelianMonoid;AL| (|AbelianMonoid;|)))))) 

(DEFUN |AbelianMonoid;| ()
  (PROG (#0=#:G1395)
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

(SETQ |$CategoryFrame|
      (|put| '|AbelianMonoid| '|isCategory| T
             (|addModemap| '|AbelianMonoid| '(|AbelianMonoid|)
                 '((|Category|)) T '|AbelianMonoid| |$CategoryFrame|))) 

(MAKEPROP '|AbelianMonoid| 'NILADIC T) 
