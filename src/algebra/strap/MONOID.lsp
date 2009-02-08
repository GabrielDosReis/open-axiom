
(/VERSIONCHECK 2) 

(DEFPARAMETER |Monoid;AL| 'NIL) 

(DEFUN |Monoid;| ()
  (PROG (#0=#:G1400)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|SemiGroup|)
                           (|mkCategory| '|domain|
                               '(((|One| ($) |constant|) T)
                                 ((|sample| ($) |constant|) T)
                                 ((|one?| ((|Boolean|) $)) T)
                                 ((** ($ $ (|NonNegativeInteger|))) T)
                                 ((|recip| ((|Union| $ "failed") $)) T))
                               NIL
                               '((|NonNegativeInteger|) (|Boolean|))
                               NIL))
                   |Monoid|)
        (|setShellEntry| #0# 0 '(|Monoid|)))))) 

(DEFUN |Monoid| ()
  (LET () (COND (|Monoid;AL|) (T (SETQ |Monoid;AL| (|Monoid;|)))))) 

(MAKEPROP '|Monoid| 'NILADIC T) 
