
(/VERSIONCHECK 2) 

(DEFPARAMETER |EntireRing;AL| 'NIL) 

(DEFUN |EntireRing;| ()
  (LET ((#0=#:G1372
            (|Join| (|Ring|) (|BiModule| '$ '$)
                    (|mkCategory| '|package| NIL
                        '((|noZeroDivisors| T)) 'NIL NIL))))
    (|setShellEntry| #0# 0 '(|EntireRing|))
    #0#)) 

(DEFUN |EntireRing| ()
  (COND (|EntireRing;AL|) (T (SETQ |EntireRing;AL| (|EntireRing;|))))) 

(MAKEPROP '|EntireRing| 'NILADIC T) 
