
(/VERSIONCHECK 2) 

(DEFPARAMETER |DivisionRing;AL| 'NIL) 

(DEFUN |DivisionRing;| ()
  (PROG (#0=#:G1400)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1399)
                             (LIST '(|Fraction| (|Integer|))))
                       (|Join| (|EntireRing|) (|Algebra| '#1#)
                               (|mkCategory| '|domain|
                                   '(((** ($ $ (|Integer|))) T)
                                     ((|inv| ($ $)) T))
                                   NIL '((|Integer|)) NIL)))
                   |DivisionRing|)
        (SETELT #0# 0 '(|DivisionRing|)))))) 

(DEFUN |DivisionRing| ()
  (LET ()
    (COND
      (|DivisionRing;AL|)
      (T (SETQ |DivisionRing;AL| (|DivisionRing;|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|DivisionRing| '|isCategory| T
             (|addModemap| '|DivisionRing| '(|DivisionRing|)
                 '((|Category|)) T '|DivisionRing| |$CategoryFrame|))) 

(MAKEPROP '|DivisionRing| 'NILADIC T) 
