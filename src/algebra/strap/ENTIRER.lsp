
(/VERSIONCHECK 2) 

(DEFPARAMETER |EntireRing;AL| 'NIL) 

(DEFUN |EntireRing;| ()
  (PROG (#0=#:G1397)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|Ring|) (|BiModule| '$ '$)
                           (|mkCategory| '|package| NIL
                               '((|noZeroDivisors| T)) 'NIL NIL))
                   |EntireRing|)
        (|setShellEntry| #0# 0 '(|EntireRing|)))))) 

(DEFUN |EntireRing| ()
  (LET ()
    (COND
      (|EntireRing;AL|)
      (T (SETQ |EntireRing;AL| (|EntireRing;|)))))) 

(MAKEPROP '|EntireRing| 'NILADIC T) 
