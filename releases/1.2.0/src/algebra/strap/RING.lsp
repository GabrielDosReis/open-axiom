
(/VERSIONCHECK 2) 

(DEFPARAMETER |Ring;AL| 'NIL) 

(DEFUN |Ring| ()
  (LET (#:G1396) (COND (|Ring;AL|) (T (SETQ |Ring;AL| (|Ring;|)))))) 

(DEFUN |Ring;| ()
  (PROG (#0=#:G1394)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|Rng|) (|Monoid|) (|LeftModule| '$)
                           (|mkCategory| '|domain|
                               '(((|characteristic|
                                      ((|NonNegativeInteger|)))
                                  T)
                                 ((|coerce| ($ (|Integer|))) T))
                               '((|unitsKnown| T))
                               '((|Integer|) (|NonNegativeInteger|))
                               NIL))
                   |Ring|)
        (SETELT #0# 0 '(|Ring|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|Ring| '|isCategory| T
             (|addModemap| '|Ring| '(|Ring|) '((|Category|)) T '|Ring|
                 |$CategoryFrame|))) 

(MAKEPROP '|Ring| 'NILADIC T) 
