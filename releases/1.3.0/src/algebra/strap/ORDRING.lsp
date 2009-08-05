
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedRing;AL| 'NIL) 

(DEFUN |OrderedRing;| ()
  (PROG (#0=#:G1404)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|OrderedAbelianGroup|) (|Ring|) (|Monoid|)
                           (|mkCategory| '|domain|
                               '(((|positive?| ((|Boolean|) $)) T)
                                 ((|negative?| ((|Boolean|) $)) T)
                                 ((|sign| ((|Integer|) $)) T)
                                 ((|abs| ($ $)) T))
                               NIL '((|Integer|) (|Boolean|)) NIL))
                   |OrderedRing|)
        (|setShellEntry| #0# 0 '(|OrderedRing|)))))) 

(DEFUN |OrderedRing| ()
  (LET ()
    (COND
      (|OrderedRing;AL|)
      (T (SETQ |OrderedRing;AL| (|OrderedRing;|)))))) 

(MAKEPROP '|OrderedRing| 'NILADIC T) 
