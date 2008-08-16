
(/VERSIONCHECK 2) 

(DEFPARAMETER |SetCategory;AL| 'NIL) 

(DEFUN |SetCategory| ()
  (LET (#:G1388)
    (COND
      (|SetCategory;AL|)
      (T (SETQ |SetCategory;AL| (|SetCategory;|)))))) 

(DEFUN |SetCategory;| ()
  (PROG (#0=#:G1386)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1385) (LIST '(|OutputForm|)))
                       (|Join| (|BasicType|) (|CoercibleTo| '#1#)
                               (|mkCategory| '|domain|
                                   '(((|hash| ((|SingleInteger|) $)) T)
                                     ((|latex| ((|String|) $)) T))
                                   NIL '((|String|) (|SingleInteger|))
                                   NIL)))
                   |SetCategory|)
        (SETELT #0# 0 '(|SetCategory|)))))) 

(MAKEPROP '|SetCategory| 'NILADIC T) 
