
(/VERSIONCHECK 2) 

(DEFPARAMETER |SetCategory;AL| 'NIL) 

(DEFUN |SetCategory;| ()
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1397) (LIST '(|OutputForm|)))
                       (|Join| (|BasicType|) (|CoercibleTo| '#1#)
                               (|mkCategory| '|domain|
                                   '(((|hash| ((|SingleInteger|) $)) T)
                                     ((|latex| ((|String|) $)) T))
                                   NIL '((|String|) (|SingleInteger|))
                                   NIL)))
                   |SetCategory|)
        (SETELT #0# 0 '(|SetCategory|)))))) 

(DEFUN |SetCategory| ()
  (LET ()
    (COND
      (|SetCategory;AL|)
      (T (SETQ |SetCategory;AL| (|SetCategory;|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|SetCategory| '|isCategory| T
             (|addModemap| '|SetCategory| '(|SetCategory|)
                 '((|Category|)) T '|SetCategory| |$CategoryFrame|))) 

(MAKEPROP '|SetCategory| 'NILADIC T) 
