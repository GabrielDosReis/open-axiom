
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedIntegralDomain;AL| 'NIL) 

(DEFUN |OrderedIntegralDomain| ()
  (LET (#:G1387)
    (COND
      (|OrderedIntegralDomain;AL|)
      (T (SETQ |OrderedIntegralDomain;AL| (|OrderedIntegralDomain;|)))))) 

(DEFUN |OrderedIntegralDomain;| ()
  (PROG (#0=#:G1385)
    (RETURN
      (PROG1 (LETT #0# (|Join| (|IntegralDomain|) (|OrderedRing|))
                   |OrderedIntegralDomain|)
        (SETELT #0# 0 '(|OrderedIntegralDomain|)))))) 

(MAKEPROP '|OrderedIntegralDomain| 'NILADIC T) 
