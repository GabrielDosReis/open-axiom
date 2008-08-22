
(/VERSIONCHECK 2) 

(DEFPARAMETER |AbelianSemiGroup;AL| 'NIL) 

(DEFUN |AbelianSemiGroup| ()
  (LET (#:G1396)
    (COND
      (|AbelianSemiGroup;AL|)
      (T (SETQ |AbelianSemiGroup;AL| (|AbelianSemiGroup;|)))))) 

(DEFUN |AbelianSemiGroup;| ()
  (PROG (#0=#:G1394)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|SetCategory|)
                           (|mkCategory| '|domain|
                               '(((+ ($ $ $)) T)
                                 ((* ($ (|PositiveInteger|) $)) T))
                               NIL '((|PositiveInteger|)) NIL))
                   |AbelianSemiGroup|)
        (SETELT #0# 0 '(|AbelianSemiGroup|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|AbelianSemiGroup| '|isCategory| T
             (|addModemap| '|AbelianSemiGroup| '(|AbelianSemiGroup|)
                 '((|Category|)) T '|AbelianSemiGroup|
                 |$CategoryFrame|))) 

(MAKEPROP '|AbelianSemiGroup| 'NILADIC T) 
