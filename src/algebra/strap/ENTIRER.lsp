
(/VERSIONCHECK 2) 

(DEFPARAMETER |EntireRing;AL| 'NIL) 

(DEFUN |EntireRing| ()
  (LET (#:G1387)
    (COND
      (|EntireRing;AL|)
      (T (SETQ |EntireRing;AL| (|EntireRing;|)))))) 

(DEFUN |EntireRing;| ()
  (PROG (#0=#:G1385)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|Ring|) (|BiModule| '$ '$)
                           (|mkCategory| '|package| NIL
                               '((|noZeroDivisors| T)) 'NIL NIL))
                   |EntireRing|)
        (SETELT #0# 0 '(|EntireRing|)))))) 

(MAKEPROP '|EntireRing| 'NILADIC T) 
