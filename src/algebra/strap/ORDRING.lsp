
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedRing;AL| 'NIL) 

(DEFUN |OrderedRing;| ()
  (PROG (#0=#:G1403)
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

(DEFUN |OrderedRing| ()
  (LET ()
    (COND
      (|OrderedRing;AL|)
      (T (SETQ |OrderedRing;AL| (|OrderedRing;|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|OrderedRing| '|isCategory| T
             (|addModemap| '|OrderedRing| '(|OrderedRing|)
                 '((|Category|)) T '|OrderedRing| |$CategoryFrame|))) 

(MAKEPROP '|OrderedRing| 'NILADIC T) 
