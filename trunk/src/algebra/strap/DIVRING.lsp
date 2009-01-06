
(/VERSIONCHECK 2) 

(DEFPARAMETER |DivisionRing;AL| 'NIL) 

(DEFUN |DivisionRing;| ()
  (PROG (#0=#:G1401)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1400)
                             (LIST '(|Fraction| (|Integer|))))
                       (|Join| (|EntireRing|) (|Algebra| '#1#)
                               (|mkCategory| '|domain|
                                   '(((** ($ $ (|Integer|))) T)
                                     ((|inv| ($ $)) T))
                                   NIL '((|Integer|)) NIL)))
                   |DivisionRing|)
        (|setShellEntry| #0# 0 '(|DivisionRing|)))))) 

(DEFUN |DivisionRing| ()
  (LET ()
    (COND
      (|DivisionRing;AL|)
      (T (SETQ |DivisionRing;AL| (|DivisionRing;|)))))) 

(MAKEPROP '|DivisionRing| 'NILADIC T) 
