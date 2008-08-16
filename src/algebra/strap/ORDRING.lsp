
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedRing;AL| 'NIL) 

(DEFUN |OrderedRing| ()
  (LET (#:G1393)
    (COND
      (|OrderedRing;AL|)
      (T (SETQ |OrderedRing;AL| (|OrderedRing;|)))))) 

(DEFUN |OrderedRing;| ()
  (PROG (#0=#:G1391)
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
        (SETELT #0# 0 '(|OrderedRing|)))))) 

(MAKEPROP '|OrderedRing| 'NILADIC T) 
