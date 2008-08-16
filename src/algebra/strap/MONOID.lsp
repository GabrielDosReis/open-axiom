
(/VERSIONCHECK 2) 

(DEFPARAMETER |Monoid;AL| 'NIL) 

(DEFUN |Monoid| ()
  (LET (#:G1388)
    (COND (|Monoid;AL|) (T (SETQ |Monoid;AL| (|Monoid;|)))))) 

(DEFUN |Monoid;| ()
  (PROG (#0=#:G1386)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|SemiGroup|)
                           (|mkCategory| '|domain|
                               '(((|One| ($) |constant|) T)
                                 ((|sample| ($) |constant|) T)
                                 ((|one?| ((|Boolean|) $)) T)
                                 ((** ($ $ (|NonNegativeInteger|))) T)
                                 ((^ ($ $ (|NonNegativeInteger|))) T)
                                 ((|recip| ((|Union| $ "failed") $)) T))
                               NIL
                               '((|NonNegativeInteger|) (|Boolean|))
                               NIL))
                   |Monoid|)
        (SETELT #0# 0 '(|Monoid|)))))) 

(MAKEPROP '|Monoid| 'NILADIC T) 
